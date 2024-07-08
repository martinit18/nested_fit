#!/usr/bin/env python

# Tqdm for progress bars
from tqdm.autonotebook import tqdm

# Other imports
import pandas as pd
from importlib.metadata import version as imp_version
import subprocess
import pathlib
import logging
import yaml
import json


class Configurator():
    '''Writes the nf_input.yaml file for automatic runs and creates a python settings interface.
    '''

    def __init__(self,
                 datafiles=[],
                 specstr='x,c',
                 likelihood='GAUSSIAN',
                 expressions=[],
                 params={},

                 livepoints=200,
                 search_method='SLICE_SAMPLING',
                 search_params=(0.5, 3),
                 search_maxtries=1000,
                 search_multries=100,
                 search_maxsteps=100000,

                 conv_method='LIKE_ACC',
                 conv_accuracy=1.E-05,
                 conv_parameter=0.01,

                 cluster_enable=False,
                 cluster_method='f',
                 cluster_distance=0.5,
                 cluster_bandwidth=0.2,

                 keep_yaml=True
                 ):

        self.logger = logging.getLogger(__name__)

        if not datafiles:
            self.logger.error('Configurator needs at least one datafile.')
            return

        # Set some defaults
        self._config = {}

        # major.minor only
        self._config['version'] = float('.'.join(imp_version('nested_fit').split('.')[:2]))

        # datafiles
        self._config['datafiles'] = datafiles

        self._config['search'] = {}
        self._config['search']['livepoints'] = livepoints
        self._config['search']['method'] = search_method
        self._config['search']['param1'] = search_params[0]
        self._config['search']['param2'] = search_params[1]
        self._config['search']['max_tries'] = search_maxtries
        self._config['search']['tries_mult'] = search_multries
        self._config['search']['num_tries'] = 1
        self._config['search']['max_steps'] = search_maxsteps

        self._config['convergence'] = {}
        self._config['convergence']['method'] = conv_method
        self._config['convergence']['accuracy'] = conv_accuracy
        self._config['convergence']['parameter'] = conv_parameter

        self._config['clustering'] = {}
        self._config['clustering']['enabled'] = cluster_enable
        self._config['clustering']['method'] = cluster_method
        self._config['clustering']['distance'] = cluster_distance
        self._config['clustering']['bandwidth'] = cluster_bandwidth

        # input file layout
        self._config['specstr'] = specstr

        self._config['likelihood'] = 'GAUSSIAN'

        self._config['function'] = {}

        self.multiexp = len(expressions) > 1
        if expressions:
            if self.multiexp:
                for i, expr in enumerate(expressions):
                    self._config['function'][f'expression_{i + 1}'] = expr
            else:
                self._config['function']['expression'] = expressions[0]

        self._config['function']['params'] = params

        # Get all data by default
        self.manual_extents = False
        if self.multiexp:
            for i, exp in enumerate(expressions):
                self._config[f'data_{i + 1}'] = {'xmin': 0, 'xmax': 0, 'ymin': 0, 'ymax': 0}
        else:
            self._config['data'] = {'xmin': 0, 'xmax': 0, 'ymin': 0, 'ymax': 0}
            ext = pathlib.Path(self._config['datafiles'][0]).suffixes[-1]
            if ext == '.csv':
                delimiter = ','
            elif ext == '.tsv':
                delimiter = '\t'
            else:
                self.logger.error('Input file invalid format/extension.')
                self.logger.error('Valid formats: `.csv` and `.tsv`.')
            self._df = pd.read_csv(self._config['datafiles'][0], delimiter=delimiter, header=None)

        self._reconfigure_data_extents()

        self._keep_yaml = keep_yaml

    def set_expression(self, expr, slot=0):
        if slot == 0:
            self._config['function']['expression'] = expr
        else:
            self._config['function'][f'expression_{slot}'] = expr

    def set_extents(self, xmin, xmax, slot=0):
        if xmin == 0 and xmax == 0:
            self.manual_extents = False
            self._reconfigure_data_extents()
        else:
            self.manual_extents = True
            if slot == 0:
                self._config['data']['xmin'] = xmin
                self._config['data']['xmax'] = xmax
            else:
                self._config[f'data_{slot}']['xmin'] = xmin
                self._config[f'data_{slot}']['xmax'] = xmax

    def set_params(self, **params):
        self._config['function']['params'] = params

    def sample(self, path='.', disablebar=False, disable_output=False):
        version = imp_version('nested_fit')

        self._write_yaml_file(path)

        self._nf_process = subprocess.Popen(
            [f'nested_fit{version}', '-lo', '-v', 'error'],
            stdout=subprocess.PIPE,
            cwd=pathlib.Path(path).resolve()
        )

        pbar = tqdm(total=self._config['search']['max_steps'], desc='Running nested_fit', disable=disablebar)

        # TODO: (César): Make this a thread and send data via socket
        while self._nf_process.poll() is None:
            line = self._nf_process.stdout.readline().decode("utf-8").split('|')

            # Print errors
            if '<ERROR>' in line[0]:
                print(line[0], end='')  # BUG: (César): This assumes the error logs don't have any '|' char
                # end_color = 'red'

            if 'LO' not in line[0]:
                continue

            pbar.update(100)  # TODO: (César) : This assumes the output comes in mod 100

        if not self._keep_yaml:
            pathlib.Path.unlink(f'{path}/nf_input.yaml', missing_ok=True)

        pbar.close()

        if not disable_output:
            with open(f'{path}/output_res.json', 'r') as f:
                return json.load(f)
        return None

    def dashboard(self):
        pass

    def _write_yaml_file(self, path):
        # We want the datafiles as a string
        datafiles = self._config['datafiles']
        self._config['datafiles'] = ', '.join(datafiles)

        with open(f'{path}/nf_input.yaml', 'w') as f:
            data = yaml.dump(self._config, width=10000)
            f.write(data)

        self._config['datafiles'] = datafiles

    def _calculate_data_extents(self, file):
        ext = pathlib.Path(self._config['datafiles'][0]).suffixes[-1]
        if ext == '.csv':
            delimiter = ','
        elif ext == '.tsv':
            delimiter = '\t'
        else:
            self.logger.error('Input file invalid format/extension.')
            self.logger.error('Valid formats: `.csv` and `.tsv`.')

        df = pd.read_csv(file, delimiter=delimiter, header=None)

        # Get where the x's are column-wise
        x_col = self._config['specstr'].split(',').index('x')

        return (df[x_col].min().item(), df[x_col].max().item())

    def _get_data(self):
        if self.multiexp:
            self.logger.error('_get_data does not support multiple datafiles.')
            return None

        x_col = self._config['specstr'].split(',').index('x')
        y_col = self._config['specstr'].split(',').index('c')
        # e_col = self._config['specstr'].split(',').index('ce')

        return (self._df[x_col].tolist(), self._df[y_col].tolist())

    def _reconfigure_data_extents(self):
        if self.manual_extents or not self._config['datafiles']:
            return

        # Read the datafiles and set the extents
        if not self.multiexp:
            for file in self._config['datafiles']:
                xmin, xmax = self._calculate_data_extents(file)
                self._config['data']['xmin'] = xmin
                self._config['data']['xmax'] = xmax
        else:
            for i, file in enumerate(self._config['datafiles']):
                xmin, xmax = self._calculate_data_extents(file)
                self._config[f'data_{i + 1}']['xmin'] = xmin
                self._config[f'data_{i + 1}']['xmax'] = xmax


# ------------- Additional tools
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
