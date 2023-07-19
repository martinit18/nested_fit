"""
This script configures all of the test cases. Is called on the CMakeLists.txt file configure step.
"""

import json
import sys
import pathlib
import shutil
import glob

if __name__ == '__main__':
    nf_test_version = sys.argv[1]
    ntries = sys.argv[2]
    cmake_build_dir = sys.argv[3]
    test_dir = '/'.join(sys.argv[4:])
    test_name = sys.argv[5]

def file_replace(func, in_dir, out_dir, variant=None):
    with open(out_dir, 'w') as input_file:
        with open(in_dir, 'r') as template_file:
            input_file.write(func(template_file.read(), variant))

def replace_single(input_data, variant):
    input_data = input_data.replace('@nf_test_version_input@', nf_test_version)
    input_data = input_data.replace('@nf_test_current_ntries@', ntries)
    return input_data

def replace_modes(input_data, variant):
    input_data = input_data.replace('@nf_test_version_input@', nf_test_version)
    input_data = input_data.replace('@nf_test_current_ntries@', ntries)
    for entry_k, entry_v in variant.items():
        if isinstance(entry_v, list):
            for i, v in enumerate(entry_v):
                input_data = input_data.replace(f'@{entry_k}_{i}@', f'{v}')
        else:
            input_data = input_data.replace(f'@{entry_k}@', f'{entry_v}')
    return input_data

def main():
    modes = {}

    try:
        with open(f'{test_dir}/modes.json', 'r') as in_file:
            modes = json.load(in_file)
    except FileNotFoundError:
        pass

    # Generate the output folder(s)
    if not modes:
        print(f'-- Configuring Test -> {test_name} (Single Variant)')
        test_build_dir = f'{cmake_build_dir}/tests/{test_name}'
        pathlib.Path(f'{test_build_dir}/').mkdir(exist_ok=True, parents=True)
        file_replace(replace_single,
                     f'{test_dir}/nf_input.dat.in',
                     f'{test_build_dir}/nf_input.dat',
                     variant=None)
        for f in glob.glob(f'{test_dir}/*.dat'): shutil.copy2(f, f'{test_build_dir}/')
        shutil.copy2(f'{test_dir}/nf_expect.py', f'{test_build_dir}/nf_expect.py')
    else:
        for i, variant in enumerate(modes['variants']):
            print(f'-- Configuring Test -> {test_name} (Variant {i})')
            test_build_dir = f'{cmake_build_dir}/tests/{test_name}_{i}'
            pathlib.Path(f'{test_build_dir}/').mkdir(exist_ok=True, parents=True)
            file_replace(replace_modes,
                         f'{test_dir}/nf_input.dat.in',
                         f'{test_build_dir}/nf_input.dat',
                         variant=variant)
            for f in glob.glob(f'{test_dir}/*.dat'): shutil.copy2(f, f'{test_build_dir}/')
            shutil.copy2(f'{test_dir}/nf_expect.py', f'{test_build_dir}/nf_expect.py')

if __name__ == '__main__':
    main()
