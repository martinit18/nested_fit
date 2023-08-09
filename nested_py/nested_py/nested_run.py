#!/usr/bin/env python
import sys
import subprocess
from tqdm import notebook as tqdm_notebook
from tqdm import tqdm as tqdm_terminal
import numpy as np

class Executor():
    def __init__(self, input_file='nf_input.dat', **kwargs):
        self.input_file = input_file
        self.run_default = True
        self.nested_process = None
        self.max_n = 0
        self.acc_lim = 0.0

        try:
            with open(self.input_file, 'r') as f:
                lines = f.readlines()
                self.max_n = int(lines[11].split('#')[0].split()[-1])
                self.acc_lim = float(lines[7].split('#')[0].split()[0])
                if(len(kwargs) == 0): pass
                else:
                    self.run_default = False
        except FileNotFoundError:
            print(f'Input file {input_file} not found.')
            sys.exit()

    def run(self, nested_fit_version, nested_func=False, use_mpi=False):
        if self.run_default:
            self.nested_process = subprocess.Popen(
                ["nested_fit" + ("_func" if nested_func else "") + nested_fit_version],
                stdout=subprocess.PIPE
            )
            pbar = None
            conv_bar = None
            firstLine = True
            acc_mult = 0.0
            max_acc = 0
            if is_env_notebook():
                pbar = tqdm_notebook.tqdm_notebook(total=self.max_n, bar_format='Iteration: {n}/{total} {bar}')
                conv_bar = tqdm_notebook.tqdm_notebook(bar_format='Accuracy: {postfix[0][value]:.2e} {bar}', postfix=[{"value": 0}])
            else:
                pbar = tqdm_terminal(total=self.max_n, bar_format='Iteration: {n}/{total} {bar}')
                conv_bar = tqdm_terminal(bar_format='Accuracy: {postfix[0][value]:.2e} {bar}', postfix=[{"value": 0}])
            pbar.set_description('Iteration')
            conv_bar.set_description('Normalized Accuracy')

            while True:
                line = self.nested_process.stdout.readline().decode("utf-8")
                if not line: break
                if not use_mpi:
                    if line[0] != '|': continue
                    pbar.update(100)

                    acc = float(line.split('|')[-3].split(':')[-1].strip())
                    if firstLine:
                        firstLine = False
                        acc_n = np.log10(self.acc_lim)
                        acc_mult = (10 ** np.abs(acc_n))
                        max_acc = int(acc * acc_mult)
                        conv_bar.reset(total=max_acc)

                    conv_bar.postfix[0]['value'] = acc
                    acc = acc * acc_mult
                    conv_bar.n = int(max_acc - acc)
                    conv_bar.refresh()
                    # print(acc, flush=True)
                else:
                    # TODO(César)
                    pass

            if self.nested_process.wait() == 0:
                pbar.colour = 'green'
                conv_bar.colour = 'green'
                pbar.close()
                conv_bar.close()
                print('Success running nested_fit.')
            else:
                pbar.colour = 'red'
                conv_bar.colour = 'red'
                pbar.close()
                conv_bar.close()
                print('Error running nested_fit.')
    
#------------- Additional tools
def is_env_notebook() -> bool:
    try:
        shell = get_ipython().__class__.__name__
        if shell == 'ZMQInteractiveShell':
            return True   # Jupyter notebook or qtconsole
        elif shell == 'TerminalInteractiveShell':
            return False  # Terminal running IPython
        else:
            return False  # Other type (?)
    except NameError:
        return False      # Probably standard Python interpreter
