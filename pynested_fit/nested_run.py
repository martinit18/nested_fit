#!/usr/bin/env python

# The new version uses rich to print out
from rich.live import Live as RLive
from rich.layout import Layout as RLayout
from rich.panel import Panel as RPanel
from rich.table import Table as RTable
# from rich.text import Text as RText
# from rich.progress import Progress as RProgress
# from rich.progress import BarColumn as RBarColumn
# from rich.progress import TextColumn as RTextColumn
from rich.columns import Columns as RColumns

# Custom rich widgets import
from .widgets import bar as cbar
from .widgets import timer as ctimer
from .widgets import var as cvar
from .widgets import hfinder as chfinder

# Metadata
from .metadata import __features__

# Rich debugging
# from rich import print as rprint

# Other imports
import pandas as pd
from importlib.metadata import version as imp_version
import subprocess
import pathlib
import logging
import yaml
import json
import time
import psutil
from typing import List


class NFDashboardHeader():
    def __init__(self):
        # Timer starts on header __init__
        # This is not of much importance. We only want a course way
        # to time the dashboard running time
        self._timer = ctimer.NFDashboardTimer()

        self._layout = RLayout()
        self._layout.split_row(RLayout(name='left'), RLayout(name='right'))

        # Make the Top left
        self._cpu_load_disp = cbar.HRollingBarDisplay(15, callback=psutil.cpu_percent)
        self._mem_load_disp = cbar.HRollingBarDisplay(15, callback=lambda: psutil.virtual_memory()[2])
        top_left = RLayout()
        top_left.split_row(RLayout(name='left'), RLayout(name='right'))

        version_time_grid = RTable.grid(expand=False)
        version_time_grid.add_column(justify='right')
        version_time_grid.add_column(justify='left')
        version_time_grid.add_row('[b]Version[/b]', ' ' + imp_version('nested_fit'))
        version_time_grid.add_row('[b]Elapsed[/b]', self._timer)
        top_left['left'].update(version_time_grid)

        cpu_mem_grid = RTable.grid(expand=False)
        cpu_mem_grid.add_column(justify='right')
        cpu_mem_grid.add_column(justify='right')
        cpu_mem_grid.add_row('[b]CPU[/b] ', self._cpu_load_disp)
        cpu_mem_grid.add_row('[b]MEM[/b] ', self._mem_load_disp)
        top_left['right'].update(cpu_mem_grid)

        self._layout['left'].update(top_left)

        # Make the top right
        top_right = RLayout()
        top_right.split_row(RLayout(name='left'), RLayout(name='center'), RLayout(name='right'))
        switches = RTable.grid(expand=False)
        switches.add_column(justify='right')
        switches.add_column(justify='left')
        switches.add_row(
            '[b]OpenMP[/b]',
            ' [green]YES[/green]' if __features__['OpenMP'] == 'ON' else ' [red]NO[/red]' # type: ignore
        )
        switches.add_row(
            '[b]OpenMPI[/b]',
            ' [green]YES[/green]' if __features__['OpenMPI'] == 'ON' else ' [red]NO[/red]' # type: ignore
        )

        top_right['left'].update(switches)

        debug_grid = RTable.grid(expand=False)
        debug_grid.add_column(justify='right')
        debug_grid.add_column(justify='left')

        if __features__['LTRACE'] == 'ON': # type: ignore
            debug_grid.add_row('', ' [b][yellow]:warning: Tracing on![/yellow][/b]')
        if __features__['BUILDTYPE'] == 'Debug': # type: ignore
            debug_grid.add_row('', ' [b][yellow]:warning: Debug build![/yellow][/b]')

        top_right['center'].update(debug_grid)

        debug_grid2 = RTable.grid(expand=False)
        debug_grid2.add_column(justify='right')
        debug_grid2.add_column(justify='left')

        if __features__['PPROF'] == 'ON': # type: ignore
            debug_grid2.add_row('[b][yellow]:warning: Profiling on![/yellow][/b]', '')
        else:
            debug_grid2.add_row('', '')
        debug_grid2.add_row('[green]✓ ALL OK[/green]', '')

        top_right['right'].update(debug_grid2)

        self._layout['right'].update(top_right)

    def __rich__(self):
        return self._layout

    def update(self):
        self._timer.update()
        self._cpu_load_disp.update()
        self._mem_load_disp.update()


class NFDashboardInput():
    def __init__(self, config):
        self._layout = RLayout()
        self._layout.split_column(RLayout(name='top'), RLayout(name='mid'), RLayout(name='bot'))

        top_grid, top_sets = self._generate_set_grid(config)
        self._layout['mid'].size = None
        self._layout['mid'].ratio = 100
        self._layout['mid'].update(RTable.grid())
        self._layout['top'].minimum_size = 5 * ((len(top_sets) + 1) // 2)
        self._layout['top'].update(top_grid)

        # TODO: (César) Add a dynamic way to detect ncols based on screen size
        panel, psize = self._generate_misc_params_in(config, 2)
        self._layout['bot'].minimum_size = psize
        self._layout['bot'].update(panel)

    def _generate_misc_params_in(self, config, ncols):
        columns = RLayout()
        columns.split_row(*[RLayout(name=str(i)) for i in range(ncols)])
        grid = RTable.grid(expand=False)
        grid.add_column(justify='right')
        grid.add_column(justify='left')

        fetch_dict = {
            'N. Livepoints': str(config._config['search']['livepoints']),
            'Search Method': (config._config['search']['method']),
            'Search Params': '(' + ', '.join(map(str, (config._config['search']['param1'], config._config['search']['param1']))) + ')',
            'Convergence Method': str(config._config['convergence']['method']),
            'Convergence Acc.': str(config._config['convergence']['accuracy']),
            'Convergence Param': str(config._config['convergence']['parameter'])
        }

        if config._config['clustering']['enabled']:
            fetch_dict['Clustering Method'] = str(config._config['clustering']['method'])
            fetch_dict['Clustering Params'] = '(' + ', '.join(map(str, (config._config['clustering']['parameter1'], config._config['clustering']['parameter2']))) + ')'

        fetch_dict['N. Params'] = str(len(
            [x for x, y in config._config['function']['params'].items()
                if 'fixed' not in y or not y['fixed']
            ])) + ' (' + str(len(config._config['function']['params'].keys())) + ')'

        # NOTE: (César) For now add rows at will
        #               This could probably be changed for a better layout disposition
        for k, v in fetch_dict.items():
            grid.add_row(f'[b]{k}:[/b]', f' {v}')

        return RPanel(grid, title='Input File'), 2 + len(fetch_dict.keys())

    def _generate_set_grid(self, config):
        top_sets = []
        for i, df in enumerate(config._config['datafiles']):
            in_large = RTable.grid(expand=False)
            in_large.add_column(justify='right')
            in_large.add_column(justify='left')
            datafiles_grid = RTable.grid(expand=False)
            datafiles_grid.add_column(justify='left', no_wrap=True)
            datafiles_grid.add_row(f' {df}')
            in_large.add_row('[b]Datafile:[/b]', datafiles_grid)
            ext_str = ','.join([str(ext) for _, ext in config.get_extents()[i].items()])
            in_large.add_row('[b]Extents:[/b]', f' {ext_str}')

            functions_grid = RTable.grid(expand=False)
            functions_grid.add_column(justify='left', no_wrap=True)
            functions_grid.add_row(f' {config.get_functions_expr()[i].split("=")[-1].strip()}')
            in_large.add_row('[b]Function:[/b]', functions_grid)

            frame = RPanel(in_large, title=f'Set #{i}')
            top_sets.append(frame)

        # TODO: (César) Reordering / resizing items could be more sensible to screen width
        if len(top_sets) > 1:
            top_grid = RTable.grid(expand=False)
            top_grid.add_column()
            top_grid.add_column()
        else:
            top_grid = RTable.grid(expand=True)
            top_grid.add_column()

        if len(top_sets) == 1:
            top_grid.add_row(top_sets[0])
        elif len(top_sets) % 2 == 0:
            i = iter(top_sets)
            for s in zip(i, i):
                top_grid.add_row(s[0], s[1])
        else:
            i = iter(top_sets[:-1])
            for s in zip(i, i):
                top_grid.add_row(s[0], s[1])
            top_grid.add_row(top_sets[-1], '')
        return top_grid, top_sets

    def __rich__(self):
        return self._layout

    def update(self):
        pass


class NFDashboardOutput():
    def __init__(self):
        self._layout = RLayout()
        self._vars: List[cvar.NFDashboardVariable] = []
        self._grid = None
        self._finder = None

    def __rich__(self):
        return self._layout

    def _load_live_data(self, live_data : str):
        # This is the output from nested_fit after the pipe char (i.e. LO | <-)
        NPAR_IDX = 7 # This is where the number of pars starts
        # fortran output style: npar, names, values
        loutput = live_data.split()

        # We know the output from here on needs to be even
        # So no need to read npar
        live_vars = loutput[NPAR_IDX + 1:]
        live_szh = len(live_vars)//2

        # Returns (names, values)
        return live_vars[:live_szh], live_vars[live_szh:]

    def _chunkerize(self, lst: List, chunk_sz: int):
        for i in range(0, len(lst), chunk_sz):
            yield lst[i:i + chunk_sz]

    def _construct_grid(self):
        if not self._finder:
            self._finder = chfinder.HFinder()
        self._layout.update(self._finder)
        dims = self._finder.get_available_dimensions()

        if not dims:
            return False

        CHUNK = 20
        self._chunk_sz = dims[0] // CHUNK
        for _ in range(self._chunk_sz):
            self._grid.add_column(width=CHUNK) # type: ignore

        return True

    def update(self, live_data : str):
        live_names, live_values = self._load_live_data(live_data)
        if not self._vars:
            # Construct the layout
            self._grid = RTable.grid(expand=False)
            if not self._construct_grid():
                return # Early out and wait for the size calculation

            for name, value in zip(live_names, live_values):
                self._vars.append(cvar.NFDashboardVariable(name, value))

            for vars in self._chunkerize(self._vars, self._chunk_sz):
                if len(vars) < self._chunk_sz:
                    diff = self._chunk_sz - len(vars)
                    nv = vars + [''] * diff
                    self._grid.add_row(*nv)
                else:
                    self._grid.add_row(*vars)
            self._layout.update(self._grid)
        else:
            for i, value in enumerate(live_values):
                self._vars[i].update(value)


class Configurator():
    '''Writes the nf_input.yaml file for automatic runs and creates a python settings interface.
    '''

    def __init__(self,
                 datafiles=[],
                 specstr='x,c',
                 filefmt='auto',
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
                 cluster_parameter1=0.5,
                 cluster_parameter2=0.2,

                 keep_yaml=True,
                 **kwargs
                 ):

        self.logger = logging.getLogger("rich")

        if not datafiles:
            self.logger.error('Configurator needs at least one datafile.')
            return None

        # Create the config file that will serve to write
        # the nf_input.yaml file
        self._config = {}

        # major.minor only
        self._config['version'] = float('.'.join(imp_version('nested_fit').split('.')[:2]))

        # datafiles
        self._config['datafiles'] = datafiles

        # search
        self._config['search'] = {}
        self._config['search']['livepoints'] = livepoints
        self._config['search']['method'] = search_method
        self._config['search']['param1'] = search_params[0]
        self._config['search']['param2'] = search_params[1]
        self._config['search']['max_tries'] = search_maxtries
        self._config['search']['tries_mult'] = search_multries
        self._config['search']['num_tries'] = 1
        self._config['search']['max_steps'] = search_maxsteps

        # convergence
        self._config['convergence'] = {}
        self._config['convergence']['method'] = conv_method
        self._config['convergence']['accuracy'] = conv_accuracy
        self._config['convergence']['parameter'] = conv_parameter

        # clustering
        self._config['clustering'] = {}
        self._config['clustering']['enabled'] = cluster_enable
        self._config['clustering']['method'] = cluster_method
        self._config['clustering']['parameter1'] = cluster_parameter1
        self._config['clustering']['parameter2'] = cluster_parameter2

        # input file
        self._config['specstr'] = specstr
        self._config['filefmt'] = self._compute_filefmt(filefmt) # handle filefmt = 'auto' (python only)

        # likelihood
        self._config['likelihood'] = likelihood

        # expressions and params
        self._config['function'] = {}

        self._df = []

        self.expcount = len(expressions)
        self.multiexp = self.expcount > 1
        if expressions:
            if self.multiexp:
                for i, expr in enumerate(expressions):
                    self.set_expression(expr, slot=(i + 1))
            else:
                self.set_expression(expressions[0])

        self._config['function']['params'] = params

        # Check for kwargs
        if not self._check_kwargs(kwargs):
            return None
        self._assign_kwargs(kwargs)

        # TODO: (César) This workload of finding out xrange should be offloaded to
        #               the nested_fit executable
        input_delimiter = self._get_data_file_delimiter()
        if self.multiexp:
            for i, _ in enumerate(expressions):
                self._df.append(pd.read_csv(self._config['datafiles'][i], delimiter=input_delimiter, header=None))
                if self._find_kwarg(f'data_{i + 1}') is None:
                    self._config[f'data_{i + 1}'] = {'xmin': 0, 'xmax': 0, 'ymin': 0, 'ymax': 0}
                    self._reconfigure_data_extents(slot=(i + 1))
        else:
            self._df.append(pd.read_csv(self._config['datafiles'][0], delimiter=input_delimiter, header=None))
            if self._find_kwarg('data') is None:
                self._config['data'] = {'xmin': 0, 'xmax': 0, 'ymin': 0, 'ymax': 0}
                self._reconfigure_data_extents(slot=0)

        self._keep_yaml = keep_yaml

    def get_functions_expr(self):
        return [f for _, f in self._config['function'].items() if f]

    def get_extents(self):
        if self.multiexp:
            return [self._config[f'data_{i + 1}'] for i in range(self.expcount)]
        return [self._config['data']]

    def set_expression(self, expr, slot=0):
        if slot == 0:
            self._config['function']['expression'] = expr
        else:
            self._config['function'][f'expression_{slot}'] = expr

    def set_extents(self, xmin, xmax, ymin=0, ymax=0, slot=0):
        if xmin == 0 and xmax == 0 and ymin == 0 and ymax == 0:
            self._reconfigure_data_extents(slot=slot)
        else:
            if slot == 0 and not self.multiexp:
                self._config['data']['xmin'] = xmin
                self._config['data']['xmax'] = xmax
                self._config['data']['ymin'] = ymin
                self._config['data']['ymax'] = ymax
            elif slot == 0 and self.multiexp:
                for i in range(self.expcount):
                    self._config[f'data_{i + 1}']['xmin'] = xmin
                    self._config[f'data_{i + 1}']['xmax'] = xmax
                    self._config[f'data_{i + 1}']['ymin'] = ymin
                    self._config[f'data_{i + 1}']['ymax'] = ymax
            else:
                self._config[f'data_{slot}']['xmin'] = xmin
                self._config[f'data_{slot}']['xmax'] = xmax
                self._config[f'data_{slot}']['ymin'] = ymin
                self._config[f'data_{slot}']['ymax'] = ymax

    def set_params(self, **params):
        self._config['function']['params'] = params

    def sample(self, path='.', silent_output=False):
        version = imp_version('nested_fit')

        self._write_yaml_file(path)

        self._nf_process = subprocess.Popen(
            [f'nested_fit{version}', '-lo', '-v', 'error'],
            stdout=subprocess.PIPE,
            cwd=pathlib.Path(path).resolve()
        )

        if not silent_output:
            self._generate_live_dashboard()

            with RLive(self._live_dash, refresh_per_second=2):
                while self._nf_process.poll() is None:
                    live_data = self._parse_nf_stdout()
                    self._draw_live_table(live_data)
        else:
            while self._nf_process.poll() is None:
                # NOTE: (César) This `silent_output` hides errors for now (the user will need to check the log)
                time.sleep(0.1) # Wait until nf stops regardless of result

        if not self._keep_yaml:
            pathlib.Path(f'{path}/nf_input.yaml').unlink(missing_ok=True)

        try:
            with open(f'{path}/nf_output_res.json', 'r') as f:
                return json.load(f)
        except IOError as e:
            self.logger.error('Could not load nested_fit\'s output result.')
            self.logger.error(f'I/O exception {e}')
            return None

    def _assign_kwargs(self, kwargs):
        for kw, vw in kwargs.items():
            self._config[kw] = vw
        self._init_kwargs = kwargs

    def _find_kwarg(self, name):
        if name in self._init_kwargs.keys():
            return self._init_kwargs[name]
        return None

    def _check_kwargs(self, kwargs):
        self._valid_kwargs_pat = []

        # NOTE: (César) This is prob faster than regex
        if self.multiexp:
            for i in range(self.expcount):
                self._valid_kwargs_pat.append(f'data_{i+1}')
        else:
            self._valid_kwargs_pat.append('data')
        
        # Some kwargs are invalid, figure out which ones
        # e.g. data_0 fails, random_input fails, ...
        #      data is ok, data_1 is ok, ...
        in_pattern = [k in self._valid_kwargs_pat for k in kwargs]
        if not all(in_pattern):
            for i, kw in enumerate(kwargs):
                if not in_pattern[i]:
                    self.logger.error(f'The kwarg "{kw}" is not valid on the current context.')
            return False
        return True

    def _auto_detect_data_file_delimiter(self, slot=0):
        return pathlib.Path(self._config['datafiles'][slot]).suffixes[-1]

    def _get_data_file_delimiter(self):
        ext = self._config['filefmt']
        if ext == '.csv':
            return ','
        elif ext == '.tsv':
            return r'\s+' # Allow for spaces in the 'tsv' format not only tabs
        else:
            self.logger.error('Input file invalid format/extension.')
            self.logger.error('Valid formats: `.csv` and `.tsv`.')
            return None

    def _compute_filefmt(self, fmt):
        if fmt == 'auto':
            return self._auto_detect_data_file_delimiter(slot=0)
        else:
            return fmt

    def _parse_stdout_error(self, line):
        # Handle errors
        if '<ERROR>' in line[0]:
            print(''.join(line), end='')
            return True
        if 'LO' not in line[0]:
            return True
        return False

    def _parse_nf_stdout(self):
        line = self._nf_process.stdout
        if line:
            line = line.readline().decode("utf-8").split('|')
        else:
            return None

        if self._parse_stdout_error(line):
            return None

        return line[-1].strip()

    def _generate_live_dashboard(self):
        # Display some input options so we know
        self._live_dash = RLayout()

        self._live_dash.split_column(
            RLayout(name='header', size=4),
            RLayout(name='body')
        )

        self._live_dash['body'].split_row(
            RLayout(name='input_info'),
            RLayout(name='output_info')
        )

        self._header = NFDashboardHeader()
        header_panel = RPanel(self._header, title='Nested Fit Dashboard')
        self._input_info = NFDashboardInput(self)
        input_panel = RPanel(self._input_info, title='Input Info')
        self._output_info = NFDashboardOutput()
        output_panel = RPanel(self._output_info, title='Output Info')
        self._live_dash['header'].update(header_panel)
        self._live_dash['body']['input_info'].update(input_panel)
        self._live_dash['body']['output_info'].update(output_panel)

    def _draw_live_table(self, data):
        self._header.update()
        self._input_info.update()
        if data:
            self._output_info.update(data)

    def _write_yaml_file(self, path):
        # We want the datafiles as a string
        datafiles = self._config['datafiles']
        self._config['datafiles'] = ', '.join(datafiles)

        with open(f'{path}/nf_input.yaml', 'w') as f:
            data = yaml.dump(self._config, width=10000)
            f.write(data)

        self._config['datafiles'] = datafiles

    def _calculate_data_extents(self, slot=0):
        if self.multiexp:
            slot -= 1
        # Get where the x's are column-wise
        x_col = self._config['specstr'].split(',').index('x')
        return (self._df[slot][x_col].min().item(), self._df[slot][x_col].max().item())

    def _get_data(self):
        if self.multiexp:
            self.logger.error('_get_data does not support multiple datafiles.')
            return None

        x_col = self._config['specstr'].split(',').index('x')
        y_col = self._config['specstr'].split(',').index('c')
        # e_col = self._config['specstr'].split(',').index('ce')

        return (self._df[x_col].tolist(), self._df[y_col].tolist())

    def _reconfigure_data_extents(self, slot=0):
        # Read the datafiles and set the extents
        if not self.multiexp:
            xmin, xmax = self._calculate_data_extents(slot=0)
            self._config['data']['xmin'] = xmin
            self._config['data']['xmax'] = xmax
        else:
            xmin, xmax = self._calculate_data_extents(slot=slot)
            self._config[f'data_{slot}']['xmin'] = xmin
            self._config[f'data_{slot}']['xmax'] = xmax
