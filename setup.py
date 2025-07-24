from setuptools import setup, Extension, find_packages
from setuptools.command.build_ext import build_ext
from setuptools.command.install_lib import install_lib
from setuptools.command.install_scripts import install_scripts
from distutils.command.install_data import install_data
import pathlib
import os
import subprocess
import shutil
import platform


class CMakeExt(Extension):
    def __init__(self, name, sourcedir=''):
        # Invoke the extension without any sources
        super().__init__(name, sources=[])

        # Get the source dir
        self.src_dir = os.fspath(pathlib.Path(sourcedir).resolve())
        print(f'[CMakeExt] Source dir: {self.src_dir}')

        # Set cmake default options for the python release
        self.cmake_options = [
            '-DCMAKE_BUILD_TYPE=Release',
            '-DOPENMP=ON',
            '-DLTRACE=OFF',
            '-DPPROF=OFF'
        ]

        # Add osx sysroot if we are on macOS
        if platform.system() == 'Darwin':
            try:
                sdk_path = subprocess.check_output(
                    ['xcrun', '--sdk', 'macosx', '--show-sdk-path'],
                    universal_newlines=True
                ).strip()
                
                # Append to cmake options
                self.cmake_options.append(f'-DCMAKE_OSX_SYSROOT={sdk_path}')
            except Exception as e:
                print('DBG | Failed to determine macOS SDK path:', e)


class NFInstallData(install_data):
    def run(self):
        print('DBG | InstallData')
        self.outfiles = self.distribution.data_files


class NFInstallExt(install_lib):
    def run(self):
        print('DBG | InstallLib')
        self.skip_build = True

        bin_dir = self.distribution.bin_dir # type: ignore

        additional_files = [os.path.join(bin_dir, file) for file in os.listdir(bin_dir) if
                            os.path.isfile(os.path.join(bin_dir, file)) and file.startswith('nested_fit')
                           ]

        # HACK: (César) The package name is hardcoded...
        pathlib.Path(os.path.join(self.build_dir, 'pynested_fit/')).mkdir(exist_ok=True)
        for file in additional_files:
            shutil.copy(file, os.path.join(self.build_dir, 'pynested_fit/' + os.path.basename(file)))
        
        self.distribution.data_files = [os.path.join(self.build_dir, 'pynested_fit/' + os.path.basename(file)) for file in additional_files]

        print('Dist files: ', self.distribution.data_files)
        print('Dist files: ', additional_files)

        self.distribution.run_command('install_data')

        super().run()


class NFInstallScripts(install_scripts):
    def run(self):
        print('DBG | InstallScripts')
        self.skip_build = True

        bin_dir = self.distribution.bin_dir # type: ignore

        script_dirs = [os.path.join(bin_dir, dir) for dir in os.listdir(bin_dir) if os.path.isdir(os.path.join(bin_dir, dir))]
        print('Script Dirs:', script_dirs)

        for sd in script_dirs:
            shutil.copy(sd, os.path.join(self.build_dir, os.path.basename(sd)))

        self.distribution.scripts = script_dirs

        super().run()
        # TODO: (César) We need to copy the installed cache. See how to do this


class NFBuildExt(build_ext):
    def __init__(self, *args, **kwargs):

        # Invoke build ext init
        super().__init__(*args, **kwargs)

    def run(self):
        # Support for editable mode
        if self.editable_mode:
            print('WARNING: Editable mode will not install nested_fit!')
            return

        for ext in self.extensions:
            self.build_nf(ext)

        # NOTE: (César) Do not run the base method, we don't want a .so file out of this
        # super().run() 

    def build_nf(self, ext: CMakeExt):
        print('DBG | BuildNF')
        cwd = pathlib.Path().absolute()

        build_temp = pathlib.Path(self.build_temp)
        build_temp.mkdir(parents=True, exist_ok=True)

        os.chdir(str(build_temp))
        self.spawn(['cmake', str(ext.src_dir)] + ext.cmake_options)
        if not self.dry_run: # type: ignore
            # subprocess.call(['cmake', '--build', '.', '--target', 'install'])
            subprocess.call(['cmake', '--build', '.'])
        os.chdir(str(cwd))

        bin_dir = os.path.join(build_temp, '../../bin')
        self.distribution.bin_dir = bin_dir # type: ignore
        # TODO: (César) We need to install at least two executable files for now (currently only using list's [0])
        bin_files = [os.path.join(bin_dir, file) for file in os.listdir(bin_dir) if
                     os.path.isfile(os.path.join(bin_dir, file)) and file.startswith('nested_fit')
                    ]
        print('Bin file(s) found:', bin_files)


setup(
    cmdclass={
        'build_ext': NFBuildExt,
        'install_data': NFInstallData,
        'install_lib': NFInstallExt,
        'install_scripts': NFInstallScripts
    },
    ext_modules=[CMakeExt('nested_fit')],
    packages=find_packages(exclude=['src/']),
)
