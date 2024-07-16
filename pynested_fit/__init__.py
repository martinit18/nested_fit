from .nested_res import *
from .nested_run import *
from .utils import get_env_type
import logging
from rich.logging import RichHandler
from rich import print as rprint
from rich.panel import Panel as RPanel
from rich.table import Table as RTable

# Setup the version
from .metadata import __version__

# Setup the available features
from .metadata import __features__

# Setup the current cache folder
from .metadata import __cache__

# Finally, install rich traceback handler
from rich.traceback import install as rich_traceback
rich_traceback(show_locals=False)

# Setup the logger
logging.basicConfig(
    # level="NOTSET",
    format="%(message)s",
    datefmt="[%X]",
    handlers=[RichHandler(rich_tracebacks=True)]
)

# IPython interactive string print
ipython_header_grid = RTable.grid(expand=False)
ipython_header_grid.add_column(justify='right')
ipython_header_grid.add_column(justify='left')
ipython_header_grid.add_row('[b]Version[/b]', ' ' + __version__)
if __features__ != 'Unknown':
    ipython_header_grid.add_row(
        '[b]OpenMP Support[/b]',
        '[green] YES[/green]' if __features__['OpenMP'] == 'ON' else '[red] NO[/red]'
    )
    ipython_header_grid.add_row(
        '[b]OpenMPI Support[/b]',
        '[green] YES[/green]' if __features__['OpenMPI'] == 'ON' else '[red] NO[/red]'
    )
    ipython_header_grid.add_row('[b]Cache Location[/b]', ' ' + __cache__)

    warn_grid = RTable.grid(expand=False)
    warn_grid.add_column()
    warns = False
    if __features__['BUILDTYPE'] == 'Debug':
        warn_grid.add_row(' [yellow]:warning: Debug build[/yellow]')
        warns = True

    if __features__['PPROF'] == 'ON':
        warn_grid.add_row(' [yellow]:warning: Profiling build[/yellow]')
        warns = True

    if __features__['LTRACE'] == 'ON':
        warn_grid.add_row(' [yellow]:warning: Trace logging ON[/yellow]')
        warns = True

    if warns:
        ipython_header_grid.add_row('[b]Warnings[/b]', warn_grid)
else:
    ipython_header_grid.add_row('[b]Warnings[/b]', ' [red]:exclamation: Could not find feature list[/red]')
    ipython_header_grid.add_row('', ' [red]:exclamation: Instalation might be corrupted[/red]')

ipython_header = r'''
[b]Help[/b]
Examples on how to use pynested_fit
are available are available under the
examples folder at [link=https://github.com/martinit18/nested_fit/][blue]the repository[/blue][/link].

[b]Authors[/b]
Martino Trassinelli
Lune Maillard
César Godinho'''

if get_env_type() == 'IPython':
    vgrid = RTable.grid(expand=False)
    vgrid.add_column()
    vgrid.add_row(ipython_header_grid)
    vgrid.add_row(ipython_header)
    rprint(RPanel(vgrid, title=f'Welcome to {__package__}', expand=False))

# Clean the namespace
del get_env_type
del logging
del RichHandler
del rich_traceback
del ipython_header
del rprint
