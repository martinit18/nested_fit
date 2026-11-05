# nested_fit compiles the model at run time, so gfortran and g++ must be on the PATH of the machine
# running it, with the pip wheel as with a source build. Checked before the binary starts; the binary
# repeats the check at the compile itself, for those who run it without Python.
import shutil
import sys

MESSAGE = ("`{}` was not found. nested_fit compiles your model at run time and needs gfortran and g++ installed.\n"
           "Ubuntu/Debian: sudo apt install gfortran g++ ; Fedora/RHEL: sudo dnf install gcc-gfortran gcc-c++ ; macOS: brew install gcc\n"
           'See "What must be installed on the machine" in README.md.')
# command lines that name their own tools, or compile nothing
SKIP = {'--compile-cpp', '-cc', '--compile-f90', '-cf', '--cache-link', '-cl',
        '--cache-location', '--function-list', '-fl', '--help', '-h'}


def require_compilers(argv=()):
    if SKIP & set(argv):
        return
    for tool in ('gfortran', 'g++'):
        if shutil.which(tool) is None:
            print(MESSAGE.format(tool), file=sys.stderr)
            sys.exit(2)
