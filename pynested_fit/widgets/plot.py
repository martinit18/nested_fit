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
from typing import List


class NFPlot(JupyterMixin):
    def __init__(self):
        self._decoder = AnsiDecoder()
        self._type: List[str] = []
        self._label: List[str] = []
        self._color: List[str] = []
        self._x: List[np.ndarray] = [] # np.array([])
        self._y: List[np.ndarray] = [] # np.array([])
        self._xmin: float = +1e300
        self._xmax: float = -1e300
        self._ymin: float = +1e300
        self._ymax: float = -1e300

    def __rich_console__(self, console: Console, options: ConsoleOptions):
        self._width = options.max_width or console.width
        self._height = options.height or console.height
        self._canvas = Group(*self._decoder.decode(self._display_plot(self._width, self._height)))
        yield self._canvas

    def draw(self, X: np.ndarray, Y: np.ndarray, type: str='bar', label: str='', color: str=''):
        self._update(X, Y, type, label, color)
        self._update_min_max(X, Y)

    def _display_plot(self, *size):
        plt.clf()
        plt.theme('pro')

        for t, x, y, label, color in zip(self._type, self._x, self._y, self._label, self._color):
            getattr(plt, t)(x, y, label=label, color=color)
            # plt.bar(x, y)

        self._update_ticks()
        plt.plotsize(*size)
        return plt.build()


    def _update_min_max(self, X: np.ndarray, Y: np.ndarray):
        xmin = min(X)
        if xmin < self._xmin:
            self._xmin = xmin

        xmax = max(X)
        if xmax > self._xmax:
            self._xmax = xmax

        ymin = min(Y)
        if ymin < self._ymin:
            self._ymin = ymin

        ymax = max(Y)
        if ymax > self._ymax:
            self._ymax = ymax

    def _update_ticks(self):
        xticks  = [x for x in np.linspace(self._xmin, self._xmax, 7)]
        xlabels = [f'{x:.2f}' for x in xticks]
        plt.xticks(xticks, xlabels)

        yticks  = [y for y in np.linspace(self._ymin, self._ymax, 15)]
        ylabels = [f'{y:.1f}' for y in yticks]
        plt.yticks(yticks, ylabels)

    def _update(self, X: np.ndarray, Y: np.ndarray, type: str, label: str, color: str):
        self._x.append(X)
        self._y.append(Y)
        self._type.append(type)
        self._label.append(label)
        self._color.append(color)

    def clear(self):
        self._x.clear()
        self._y.clear()
        self._type.clear()
        self._label.clear()
        self._color.clear()
