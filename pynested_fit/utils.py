def get_env_type():
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
