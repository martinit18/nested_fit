from __future__ import annotations
import sys
import re

"""
This file contains the testing suit library to
check files against some conditions.
"""


# https://svn.blender.org/svnroot/bf-blender/trunk/blender/build_files/scons/tools/bcolors.py
class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


class CheckIfFile():
    def __init__(self, filename: str, verbose_on_passed: bool = False) -> None:
        self.filename = filename
        self.split = None
        self.current_line = 0
        self.file_contents = []
        self.current_token = None
        self.current_index = 0
        self.match_test_index = 0
        self.passed_tests = 0
        self.failed_tests = 0
        self.token_is_index = False
        self.verbose_on_passed = verbose_on_passed
        try:
            self.file = open(filename, 'r')
            self.file_contents = [x.rstrip().lstrip()
                                  for x in self.file.readlines()]
            self.file.close()
        except IOError as e:
            print('IO error: ', e)
            sys.exit(1)
        except Exception as e:
            print('Unexpected error: ', e)
            sys.exit(1)

        print(f'Testing file {bcolors.HEADER}{self.filename}{bcolors.ENDC}')

    def __del__(self) -> None:
        print(f'Test summary: {self.passed_tests}/{self.match_test_index}'
              'tests passed.', end='\n\n')

    def _match_function(func):
        def wrapper(self, *args, **kwargs):
            self.match_test_index += 1
            return func(self, *args, **kwargs)
        return wrapper

    def check_all_passed(self) -> bool:
        return self.failed_tests == 0

    def with_split(self, token: str) -> CheckIfFile:
        self.split = token
        return self

    def line(self, line: int) -> CheckIfFile:
        self.current_line = line - 1
        if self.current_line >= len(self.file_contents) or self.current_line < 0:
            raise IndexError(f'Invalid line number index {self.current_line}/{len(self.file_contents)}.')
        self.current_token = self.file_contents[self.current_line]
        self.token_is_index = False
        return self

    def at(self, index: int) -> CheckIfFile:
        if not self.split:
            raise RuntimeError('at function called, but no split token specified.')
        self.tokens = [i.rstrip().lstrip() for i in self.file_contents[self.current_line].split(self.split) if i]
        if index >= len(self.tokens) or index < 0:
            raise IndexError(f'Invalid line token index {index}/{len(self.tokens)}.')
        self.current_token = self.tokens[index]
        self.token_is_index = True
        self.current_index = index
        return self

    def _match_passed(self) -> None:
        self.passed_tests += 1
        print(f'Match {self.match_test_index} - {bcolors.OKGREEN}Passed{bcolors.ENDC}')
        if self.verbose_on_passed:
            if not self.token_is_index:
                cline = self.file_contents[self.current_line]
                print(f'{self.current_line + 1:04d} | {cline}')
                print(' ' * 5 + '| ', end='')
                print(bcolors.OKGREEN + '~' * (len(cline) // 2) + '^' + '~' * (len(cline) // 2) + bcolors.ENDC)
            else:
                print(f'{self.current_line + 1:04d} | ' + ' '.join(self.tokens))
                print(' ' * 5 + '| ', end='')
                print(' ' * sum((len(tok) + 1 for tok in self.tokens[:self.current_index])), end='')
                print(f'{bcolors.OKGREEN}^{bcolors.ENDC}')

    def _match_failed(self, reason: str = '') -> None:
        self.failed_tests += 1
        print(f'Match {self.match_test_index} - {bcolors.FAIL}Failed{bcolors.ENDC}')
        if reason != '':
            print(f'{self.filename}:{self.current_line+1} - {bcolors.WARNING}{reason}{bcolors.ENDC}')

        if not self.token_is_index:
            cline = self.file_contents[self.current_line]
            print(f'{self.current_line + 1:04d} | {cline}')
            print(' ' * 5 + '| ', end='')
            print(bcolors.FAIL + ('~' * (len(cline) // 2) + '^' + '~' * (len(cline) // 2))[:len(cline)] + bcolors.ENDC)
        else:
            print(f'{self.current_line + 1:04d} | ' + ' '.join(self.tokens))
            print(' ' * 5 + '| ', end='')
            print(' ' * sum((len(tok) + 1 for tok in self.tokens[:self.current_index])), end='')
            print(f'{bcolors.FAIL}^{bcolors.ENDC}')

        print('')

    @_match_function
    def match(self, value: any, verbatim: bool = True) -> CheckIfFile:
        if value is type(self.current_token):
            raise TypeError(f'match called with value as type {type(value)} but {type(self.current_token)} was casted.')
        if not self.current_token:
            raise RuntimeError('match called but no token specified.')

        success = False
        if verbatim:
            success = (self.current_token == value)
        else:
            if not isinstance(self.current_token, str):
                raise RuntimeError(f'match in non verbatim mode only works on strings, but casted value with type {type(value)} is being used.')
            if not isinstance(value, str):
                raise RuntimeError(f'match in non verbatim mode only works on strings, but pattern value with type {type(value)} is being used.')
            if re.search(value, self.current_token):
                success = True

        if success:
            self._match_passed()
        else:
            if not verbatim:
                self._match_failed(f'Expected a match with regex expression {value}')
            else:
                self._match_failed(f'Expected value {value} but file has value {self.current_token}')

        return self

    @_match_function
    def match_range(self, lobound: float, hibound: float, inclusive: bool = True) -> CheckIfFile:
        if not isinstance(self.current_token, float) and not isinstance(self.current_token, int):
            raise TypeError(f'match_range can only be called on {float} or {int} types but current token as {type(self.current_token)} type.')

        if inclusive:
            if self.current_token >= lobound and self.current_token <= hibound:
                self._match_passed()
            else:
                self._match_failed(f'Expected value between [{lobound}, {hibound}] but got {self.current_token}.')
        else:
            if self.current_token > lobound and self.current_token < hibound:
                self._match_passed()
            else:
                self._match_failed(f'Expected value between ]{lobound}, {hibound}[ but got {self.current_token}.')

        return self

    def cast(self, t: type) -> CheckIfFile:
        if not self.current_token:
            raise RuntimeError('cast called but no token specified.')
        self.current_token = t(self.current_token)
        return self
