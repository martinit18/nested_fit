from rich.console import Console, ConsoleOptions


class HFinder:
    def __init__(self):
        self._available_dims = None

    def __rich_console__(self, console: Console, options: ConsoleOptions):
        self._available_dims = (options.max_width, options.max_height)
        yield f'Probing dimensions... {self._available_dims}'

    def get_available_dimensions(self):
        return self._available_dims
