# Brief  : Displays realtime plots using plotext.
#          This is an attempt to integrate plotext in rich.
#          As suggested in: plotext/issues/26
# Author : César Godinho
# Date   : 09/08/2024

from rich.jupyter import JupyterMixin
from rich.ansi import AnsiDecoder # Undocumented
from rich.console import Console, ConsoleOptions, Group
import plotext as plt
import numpy as np


class NFPlot(JupyterMixin):
    def __init__(self, type: str='bar'):
        self._decoder = AnsiDecoder()
        self._type = type
        self._x: np.ndarray = np.array([])
        self._y: np.ndarray = np.array([])

    def __rich_console__(self, console: Console, options: ConsoleOptions):
        self._width = options.max_width or console.width
        self._height = options.height or console.height
        self._canvas = Group(*self._decoder.decode(self._display_plot(self._width, self._height)))
        yield self._canvas

    def _display_plot(self, *size):
        plt.clf()

        if self._type == 'bar':
            plt.bar(self._x, self._y)

        plt.plotsize(*size)
        return plt.build()

    def update(self, X: np.ndarray, Y: np.ndarray):
        self._x = X
        self._y = Y
