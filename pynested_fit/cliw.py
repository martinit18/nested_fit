# Brief  : CLI wrapper for nested_fit executable (when installed via pip)
# Author : César Godinho
# Date   : 16/07/2025

import os
import sys
import subprocess
from .metadata import __version__

def main():
    exe = os.path.join(os.path.dirname(__file__), f'nested_fit{__version__}')
    sys.exit(subprocess.call([exe] + sys.argv[1:]))
