from setuptools import setup, Extension, find_packages
from setuptools.command.build_ext import build_ext
import pathlib
import os
import subprocess
# import platform


class CMakeExt(Extension):
    def __init__(self, name, sourcedir=''):
        # Invoke the extension without any sources
        super().__init__(name, sources=[])

        # Get the source dir
        self.src_dir = os.fspath(pathlib.Path(sourcedir).resolve())
        print(f'[CMakeExt] Source dir: {self.src_dir}')

        # Set cmake default options for the python release
        self.cmake_options = [
            '-DOPENMP=ON',
            '-DLTRACE=OFF',
            '-DPPROF=OFF'
        ]

        # TODO: (César): Build options depending on os


class NFBuildExt(build_ext):
    def run(self):
        for ext in self.extensions:
            self.build_nf(ext)
        super().run()

    def build_nf(self, ext: CMakeExt):
        cwd = pathlib.Path().absolute()

        build_temp = pathlib.Path(self.build_temp)
        build_temp.mkdir(parents=True, exist_ok=True)

        os.chdir(str(build_temp))
        self.spawn(['cmake', str(ext.src_dir)] + ext.cmake_options)
        if not self.dry_run:
            subprocess.call(['cmake', '--build', '.', '--target', 'install'])
        os.chdir(str(cwd))


setup(
    cmdclass={'build_ext': NFBuildExt},
    ext_modules=[CMakeExt('nested_fit')],
    packages=find_packages(exclude=['src/']),
    package_data={'': ['nested_py/*.py']}
)
