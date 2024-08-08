from rich.panel import Panel as RPanel


class NFDashboardVariable():
    '''This class holds and manages printing variables out of nested_fit.
       For now it solely displays the parameter maximum likelihood value. Eq. to "max"
       in the ouput file.'''
    def __init__(self, name, value):
        self._name = name
        self._value = value

    def __rich__(self):
        out = f'Max: {float(self._value):.5e}'
        return RPanel(out, title=self._name, title_align='left', highlight=True)

    # def get_panel_size(self):
    #     return self._outsize

    def update(self, value):
        self._value = value
