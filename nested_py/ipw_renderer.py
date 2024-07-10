import ipywidgets as ipw

# Workaround for LaTeX not displaying on ipywidgets==8.1.1
from IPython.display import display, Math
import multiprocessing as pmp
import psutil
import streamlit as st
import platform


class IPWRenderer():
    '''Renders a small dashboard for the user to see when using nested_py as an
       executor.

       Also presents the possibility to render some nice outputs for the final
       results.
    '''

    def __init__(self):
        self._full_layout = ipw.VBox()
        self._add_attr('title',
                       ipw.HTML(r'<b><font size=5em>Nested_fit Dashboard</b>'))

        # self._version = imp_version('nested_fit')
        # TODO: (César) : Remove this dummy version for testing
        self._version = '5.1.1'
        self._local_core_count = pmp.cpu_count()
        desc_box = ipw.VBox([ipw.HTML('<b><font size=4em>Info</b>'), ipw.HBox([
            ipw.HTML(f'''
            <ul class="alignMe">
                <li><pre><font size=2em><b>Version</b>                  : {self._version}</pre></li>
                <li><pre><b>Available Cores (OpenMP)</b> : {self._local_core_count}</pre></li>
                <li><pre><b>CPU</b>                      : {psutil.cpu_percent()}%</pre></li>
                <li><pre><b>MEM</b>                      : {psutil.virtual_memory().percent}%</pre></li>
            </ul>''', layout=ipw.Layout(padding='0em 10em 0em 0em')),
            ipw.HTML(f'''
            <ul>
                <li><pre>System  : {platform.uname()[0]}</pre></li>
                <li><pre>Release : {platform.uname()[2]}</pre></li>
            </ul>
                     ''', layout=ipw.Layout(padding='0em 10em 0em 0em')),
            ipw.HTML('''
            <ul>
                <li><pre>Modules : (TODO ModuleList)</pre></li>
            </ul>
                     ''')
            ], layout=ipw.Layout(padding='0em', border='0px', margin='0em 0em 0em 0em'))])

        desc_box.add_class('''  .alignMe b {
                                    display: inline-block;
                                    width: 50%;
                                    position: relative;
                                    padding-right: 10px; /* Ensures colon does not overlay the text */
                                }

                                .alignMe b::after {
                                    content: "-";
                                    position: absolute;
                                    right: 10px;
                                }''')

        self._add_attr('title_desc', desc_box)

        live_prev = ipw.VBox([ipw.HTML('<b><font size=4em>Live Preview</b>'), ipw.HBox([ipw.HTML('TODO')])])

        self._add_attr('live_preview', live_prev)

        # pct_meter = ''

    def _add_attr(self, name, value):
        setattr(self, f'_dispattr_{name}', value)

        if not hasattr(self, '_full_layout'):
            self._full_layout = ipw.VBox()

        self._full_layout.children = tuple(list(self._full_layout.children) + [value])

    # TODO: (César) : Check if we are in a Jupyter/IPython environment or not
    def render_dashboard(self):
        display(self._full_layout)
