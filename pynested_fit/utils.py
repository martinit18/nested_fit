import re

ANSI_ESCAPE_RE = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')

def get_env_type() -> str:
    try:
        shell = get_ipython().__class__.__name__
        if shell == 'ZMQInteractiveShell':
            return 'Jupyter'   # Jupyter notebook or qtconsole
        elif shell == 'TerminalInteractiveShell':
            return 'IPython'   # Terminal running IPython
        else:
            return 'Unknown'   # Other type (?)
    except NameError:
        return 'Interpreter'   # Probably standard Python interpreter

def strip_ansi_codes(string: str) -> str:
    return ANSI_ESCAPE_RE.sub('', string) 
