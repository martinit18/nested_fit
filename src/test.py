import sys
import os
import subprocess
import numpy as np

"""
This file contains the code that discovers and runs the directories with the tests
containing the nested_fit inputs and then tests its files.
"""

def run_nf(folder: str, nf_exec: str, cmake_build_dir: str, cmake_source_dir: str, target_precision: float, total_tests: int, i: int):
    extension = '.exe' if os.name == 'nt' else ''
    executable = f'{cmake_source_dir}/bin/{nf_exec}' + extension
    p = subprocess.Popen([executable], cwd=f'{cmake_build_dir}/tests/{folder}', stdout=subprocess.PIPE, bufsize=0)
    print('Please wait...')

    initial_val = None
    while True:
        line = p.stdout.readline()
        if not line and p.poll() is not None:
            break
        try:
            nf_curr_precision = float(line.decode("utf-8").split('|')[-3].split(':')[1])
            if not initial_val:
                initial_val = nf_curr_precision
            
            # TODO(César): This is not ideal, but it works for now
            pct = (nf_curr_precision - target_precision) / (target_precision - initial_val) + 1.0
            # k = 200
            # mod_pct = (np.exp(pct*k)-1) / (np.exp(k) - 1)

            print(f'Running nested_fit for \'{folder}\' | {pct*100.0:3.4f}% | ({i:2d} /{total_tests:2d} )\r', end='', flush=True)
        except:
            pass # Just ignore if we can't get the precision
    print('')

def run_test(folder: str, cmake_build_dir: str):
    p = subprocess.Popen([python_exec, f'{cmake_build_dir}/tests/{folder}/nf_expect.py'], cwd=f'{cmake_build_dir}/tests/{folder}', stdout=subprocess.PIPE)
    out, _ = p.communicate()
    print(''.join(['\t' + l + '\n' for l in out.decode("utf-8").split('\n')]))

if __name__ == '__main__':
    python_exec = '/opt/homebrew/Frameworks/Python.framework/Versions/3.11/bin/python3.11'
    nf_exec_list = 'nested_fit4.5.2;nested_fit4.5.2;nested_fit4.5.2;nested_fit_func4.5.2;nested_fit4.5.2;nested_fit_func4.5.2;nested_fit4.5.2;nested_fit_func4.5.2'.split(';')
    cmake_source_dir = '/Users/martino/Owncloud/CNRS/work/programs/nested_fit/actual_version_with_git'
    cmake_build_dir = '/Users/martino/Owncloud/CNRS/work/programs/nested_fit/actual_version_with_git/src'
    test_folders = '4gauss_bg_0;4gauss_bg_1;4gauss_bg_2;4gauss_bg_3;CMakeFiles;gauss_bg;4gauss_bg_0;4gauss_bg_1;4gauss_bg_2;4gauss_bg_3;CMakeFiles;ENERGY_HARM_3D;TEST_EGGBOX_0;TEST_EGGBOX_1;TEST_EGGBOX_2;TEST_EGGBOX_3;TEST_GAUSS;gauss_bg'.split(';')

    for i, (folder, nf_exec) in enumerate(zip(test_folders, nf_exec_list)):
        run_nf(folder, nf_exec, cmake_build_dir, cmake_source_dir, 1E-5, len(test_folders), i+1)
        run_test(folder, cmake_build_dir)
