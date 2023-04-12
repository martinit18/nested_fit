"""
This script generates a .cmake file to set all of the keys on the modes.json file
"""

import json
import sys

def main():
    cmake_build_dir = sys.argv[1]
    cmake_test_name = sys.argv[2]

    with open('modes.json', 'r') as in_file:
        modes = json.load(in_file)
        # Load the variants
        variants = [(x, len(modes['variants'][i][x]) if isinstance(modes['variants'][i][x], list) else 1, isinstance(modes['variants'][i][x], list)) for i, x in enumerate(list(set([k for v in modes['variants'] for k in v])))]
        
        # Load the constants
        # TODO

        # Generate the .cmake file
        with open(f'{cmake_build_dir}/configure_json_keys_{cmake_test_name}.cmake', 'w') as out_file:
            out =  'include(TryJsonField)\n'
            out += 'macro(load_json_variants json_data_str i)\n'
            for v in variants:
                out += '\n'.join([f'\ttry_implement_json_field(JSON_DATA ${{json_data_str}} KEYS \"variants\" ${{i}} \"{v[0]}\"' + (f' {i} ' if v[2] else ' ') + f'PARAMETER_OUT {v[0]}' + (f'_{i}' if v[2] else '') + ')' for i in range(v[1])])
                out += '\n'
            out += 'endmacro()\n'
            out_file.write(out)

if __name__ == '__main__':
    main()
