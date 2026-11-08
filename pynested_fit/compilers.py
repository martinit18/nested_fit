# nested_fit compiles the model at run time, so gfortran and g++ must be on the PATH of the machine
# running it, with the pip wheel as with a source build. Checked before the binary starts; the binary
# repeats the check at the compile itself, for those who run it without Python. f2c is optional: it
# takes a Fortran model to the GPU, and its absence is only said, before the run goes on the CPU.
import shutil
import sys

MESSAGE = ("`{}` was not found. nested_fit compiles your model at run time and needs gfortran and g++ installed.\n"
           "Ubuntu/Debian: sudo apt install gfortran g++ ; Fedora/RHEL: sudo dnf install gcc-gfortran gcc-c++ ; macOS: brew install gcc\n"
           'See "What must be installed on the machine" in README.md.')
# command lines that name their own tools, or compile nothing
SKIP = {'--compile-cpp', '-cc', '--compile-f90', '-cf', '--cache-link', '-cl',
        '--cache-location', '--function-list', '-fl', '--help', '-h'}


F2C_MESSAGE = ("f2c was not found: a Fortran model (function.source: {}) runs on the CPU only. For the GPU, nested_fit "
               "translates it to C with f2c: Ubuntu/Debian: sudo apt install f2c ; Fedora/RHEL: sudo dnf install f2c")


def fortran_source(input_file):
    # the Fortran file named by function.source in the input, if any (a C file or an expression needs no f2c)
    try:
        with open(input_file) as f:
            for line in f:
                key, _, value = line.strip().partition(':')
                if key == 'source' and value.strip().rsplit('.', 1)[-1] in ('f', 'F', 'for', 'f90', 'F90'):
                    return value.strip()
    except OSError:
        pass
    return None


def require_compilers(argv=(), input_file='nf_input.yaml'):
    if SKIP & set(argv):
        return
    argv = list(argv)
    for flag in ('-i', '--input-file'):
        if flag in argv and argv.index(flag) + 1 < len(argv):
            input_file = argv[argv.index(flag) + 1]
    for tool in ('gfortran', 'g++'):
        if shutil.which(tool) is None:
            print(MESSAGE.format(tool), file=sys.stderr)
            sys.exit(2)
    source = fortran_source(input_file)
    if source and shutil.which('f2c') is None:
        print(F2C_MESSAGE.format(source), file=sys.stderr)   # a notice, not a stop: the CPU run is correct
