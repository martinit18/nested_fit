import sys
import os
import re
from typing import Tuple

def test_pass(message: str=''):
    print(f'Test OK -> {message}')
    
def test_fail(message: str=''):
    print(f'Test FAIL -> {message}')
    
def try_cast(type, value):
    try:
        return type(value)
    except ValueError:
        raise RuntimeError(f'Expected a {type} type.')
    

def try_match(command_list: list, conversion: str, content: str) -> bool:
    command = ''.join(command_list)
    if conversion == 'string':
        if re.search(command[1:-1], content):
            return True, command[1:-1]
        return False, command[1:-1]
    elif conversion == 'float':
        if re.search('range', command):
            srange = re.search('\((.*)\)', command)
            if srange:
                srange = srange.group(0)[1:-1]
                srange = srange.split(',')
                
                if len(srange) > 2:
                    raise RuntimeError(f'range only has 2 arguments, but {len(srange)} were supplied.')
                
                minmax = [float(e.lstrip().rstrip()) for e in srange]
                
                if minmax[0] >= minmax[1]:
                    raise RuntimeError(f'range\'s first argument must be smaller than the second.')
                
                return float(content) >= minmax[0] and float(content) <= minmax[1], ','.join(str(i) for i in minmax)
        return try_cast(float, content) == try_cast(float, command), try_cast(float, command)
                    
    elif conversion == 'int':
        if re.search('range', command):
            srange = re.search('\((.*)\)', command)
            if srange:
                srange = srange.group(0)[1:-1]
                srange = srange.split(',')
                
                if len(srange) > 2:
                    raise RuntimeError(f'range only has 2 arguments, but {len(srange)} were supplied.')
                
                minmax = [int(e.lstrip().rstrip()) for e in srange]
                
                if minmax[0] >= minmax[1]:
                    raise RuntimeError(f'range\'s first argument must be smaller than the second.')
                
                return int(content) >= minmax[0] and int(content) <= minmax[1], ','.join(str(i) for i in minmax)
        return try_cast(int, content) == try_cast(int, command), try_cast(int, command)

# A very simple regex parser for the .expect files that executes tests
# based on the file description and returns accordingly
def parse_expect_exec(filename: str, lines: list) -> list:
    if len(lines) < 1:
        raise RuntimeError(f'{filename} is empty.')
    
    expect_file = None
    filename_scope = None
    last_line_type = None
    inside_block   = False
    global_split_char = None
    
    expected_value = None
    all_tests = []
    for i, line in enumerate(lines):
        using_splt = False
        # print(line)
        
        # Remove trailing \n
        line = line.strip()
        if not line:
            continue
        
        # Remove comments
        line = re.compile('(#.*)(?=(?:[^"]|"[^"]*")*$)').sub('', line)
        if not line:
            continue
        
        # Is this a file start ?
        reg = re.search('(?<=from)(.*)(?=expect)', line)
        if reg:
            filename_scope = reg.group(0).lstrip().rstrip()
            last_line_type = 'fromdir'
            
            if inside_block:
                raise RuntimeError(f'at {filename}:{i+1} : Nested blocks are not allowed.')
            
            continue
        elif filename_scope == None:
            raise RuntimeError(f'at {filename}:{i+1} : No mandatory from ... expect directive found.')
        
        reg = (line == '{')
        if reg:
            if last_line_type != 'fromdir':
                raise RuntimeError(f'at {filename}:{i+1} : Can only open bracket block after from ... expect directive.')
            else:
                last_line_type = None
                if inside_block:
                    raise RuntimeError(f'at {filename}:{i+1} : Nested blocks are not allowed.')
                inside_block = True
                
                # Open the file
                try:
                    file_handler = open(filename_scope, 'r')
                    expect_file = file_handler.readlines()
                    print(f'Testing file: {filename_scope}\n')
                except OSError:
                    raise RuntimeError(f'Could not open file : {filename_scope}')
                
                continue
        
        if line == '}':
            if not inside_block:
                raise RuntimeError(f'at {filename}:{i+1} : There was no corresponding block to close.')
            else:
                inside_block = False
                filename_scope = None
                expect_file = None
                print('\n')
                continue
        
        # At this point we should always be inside a block if an exception wasn't thrown
        if line.startswith('line'):
            line_directive = line.split()
            conv_type = 'string' # The default conversion type
            result = False
            
            lineno = line_directive[1]
            if not lineno.isnumeric():
                raise RuntimeError(f'at {filename}:{i+1} : line directive requires a line number.')
            
            lineno = int(lineno) - 1
            
            if lineno > len(expect_file):
                raise RuntimeError(f'at {filename}:{i+1} : line number is to large for file {filename_scope}')
            
            content = expect_file[lineno]
            
            op = line_directive[2]
            if op == 'as':
                conv_type = line_directive[3]
                if conv_type not in ['string', 'float', 'int']:
                    raise RuntimeError(f'at {filename}:{i+1} : as requires a type but {conv_type} given.')
                
                if line_directive[4] == 'match':
                    try:
                        result, expected_value = try_match(line_directive[5:], conv_type, content)
                    except RuntimeError:
                        raise RuntimeError(f'at {filename}:{i+1} : try_match error.')
                else:
                    raise RuntimeError(f'at {filename}:{i+1} : as needs to be followed by a match keyword.')
            
            elif op == 'match':
                try:
                    result, expected_value  = try_match(line_directive[3:], conv_type, content)
                except RuntimeError:
                    raise RuntimeError(f'at {filename}:{i+1} : try_match error.')
            elif re.search('split', op):
                using_splt = True
                split_loc = re.search('\[(.*)\]', op)
                if split_loc:
                    split_idx = split_loc.group(0)[1:-1]
                    if not split_idx.isnumeric():
                        raise RuntimeError(f'at {filename}:{i+1} : split directive requires a numeric index.')
                    
                    rev_content = content.split(global_split_char)
                    
                    if int(split_idx) > len(rev_content) or int(split_idx) < -len(rev_content):
                        raise RuntimeError(f'at {filename}:{i+1} : split directive is referencing an invalid numeric index.')
                    
                    
                    rev_content = [i.rstrip().lstrip() for i in rev_content if i][int(split_idx)]
                    
                else:
                    raise RuntimeError(f'at {filename}:{i+1} : split directive expected an index.')
                
                if line_directive[3] == 'as':
                    conv_type = line_directive[4]
                    if conv_type not in ['string', 'float', 'int']:
                        raise RuntimeError(f'at {filename}:{i+1} : as requires a type but {conv_type} given.')
                    
                    if line_directive[5] == 'match':
                        try:
                            result, expected_value  = try_match(line_directive[6:], conv_type, rev_content)
                        except RuntimeError:
                            raise RuntimeError(f'at {filename}:{i+1} : try_match error.')
                    else:
                        raise RuntimeError(f'at {filename}:{i+1} : as needs to be followed by a match keyword.')
                elif line_directive[3] == 'match':
                    try:
                        result, expected_value  = try_match(line_directive[4:], conv_type, rev_content)
                    except RuntimeError:
                        raise RuntimeError(f'at {filename}:{i+1} : try_match error.')
                else:
                    raise RuntimeError(f'at {filename}:{i+1} : split needs to be followed by a match or as keyword.')
                
            if result:
                print(f'Test for line {lineno+1} passed.')
                all_tests.append(True)
            else:
                print(f'Test for line {lineno+1} failed.')
                all_tests.append(False)
            
            if not using_splt:
                print(f'Value    = {content.rstrip()}')
            else:
                print(f'Value    = {rev_content}')
            print(f'Expected = {expected_value}\n')
                
        if line.startswith('set'):
            line_directive = line.split()
            if line_directive[1] not in ['split']:
                raise RuntimeError(f'at {filename}:{i+1} : {line_directive[1]} not recognized.')
            
            if line_directive[1] == 'split':
                char = re.search('\'.*\'|\".*\"', line)
                if char:
                    c = char.group(0)[1:-1]
                    if len(c) == 0:
                        global_split_char = None
                    else:
                        global_split_char = c
                else:
                    raise RuntimeError(f'at {filename}:{i+1} : split requires a valid string or char after it in this case.')
                
    return all_tests
                
def evaluate_test() -> Tuple[bool, int, int]:
    with open('nf.expect', 'r') as f:
        # Read the file
        expected_file_lines = f.readlines()
        
        # Parse the file and get all the test results
        parse_result = parse_expect_exec('nf.expect', expected_file_lines)
        return all(parse_result), sum(parse_result), len(parse_result)
    
    
def run_test_folder(folder: str, nf_exec: str):
    # Run default nested fit on the cwd
    os.system(f'../../bin/{nf_exec} >/dev/null 2>&1')
    
    passed, num_passed, num_test = evaluate_test()
    
    if passed:
        test_pass(f'{num_passed}/{num_test} passed.')
    else:
        test_fail(f'{num_passed}/{num_test} passed.')
    
    # Cleanup nf_output_*.dat files
    for file in next(os.walk(os.getcwd()), (None, None, []))[2]:
        if file.startswith('nf_output'):
            os.unlink(file)
    
        

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print('This script requires two arguments.')
        exit()
    folder = sys.argv[1]
    nf_exec = sys.argv[2]
    
    run_test_folder(folder, nf_exec)
