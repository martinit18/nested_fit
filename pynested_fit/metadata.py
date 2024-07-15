from importlib.metadata import version as imp_version
from configparser import ConfigParser
from subprocess import check_output
import sys

__version__ = imp_version('nested_fit')


# Nested fit needs to be installed and on path for this to work
# Other wise default to unknown features status
def parse_features():
    try:
        nf_cache_folder = check_output([f'nested_fit{__version__}', '--cache-location'])
    except Exception:
        return ('Unknown', 'Unknown')
    nf_cache_folder = nf_cache_folder.decode(sys.stdout.encoding).strip()
    config = ConfigParser()
    config.read(f'{nf_cache_folder}/manifest.txt')

    return (config['Features'], nf_cache_folder)


__features__, __cache__ = parse_features()

# Cleanup namespace
del imp_version
del parse_features
del ConfigParser
del check_output
del sys
