# NESTED FIT TESTING SUITE

### Creating new tests

##### Setup the directory
1. Create a new folder under `data` or `func` folder according to the new test characteristics.

2. Create a `nf_input.dat.in` file with contents just like a regular `nf_input.dat`.

3. (`data` tests only) Copy/create a data file.

4. Create a `nf_expect.py` file.

5. (Optional) Create a `modes.json` file.

You should now have something looking like this:

```
tests/
└── (data|func)/
    └── MyNewTest/
        ├── datafile.dat
        ├── nf_input.dat.in
        ├── modes.json
        └── nf_expect.py
```

> :warning: The filenames must be exact. Except for the data file.

##### Editing the files
- The `nf_input.dat.in`:
This file will be copied to the build directory and works just like a `nf_input.dat` file, except you can add variables that are configured before the tests are setup.
For example, instead of using a fixed program version, you will pass `@nf_test_version_input@` to the file's first line and the current version will always be copied into that location.
Another already available variable is the `@nf_test_current_ntries@`, which for now is modified automatically, and should be placed on the "Number of tries and maximum of steps" line of the file like so:
    ```
    n k 0.5 0.2                     # cluster analysis setup
    @nf_test_current_ntries@ 100000 # Number of tries and maximum of steps
    GAUSS_BG 	  		# Name of the function
    ```
- The `nf_expect.py` file:
This file is the script that actually performs the assertion in the end of the tests runs and checks for outputs. Let's see an example of the file and go through it:
    ```py
    0 import sys
    1 sys.path.append('..') # This is required for the import to work for now
    2 from test_suite import CheckIfFile # Import file checking from the test suite
    3
    4 if __name__ == '__main__':
    5    (
    6        CheckIfFile('nf_output_res.dat', verbose_on_passed=True).with_split(' ')
    7            .line(2).at(1).cast(int).match(1)
    8            .line(11).at(1).cast(float).match_range(0.10, 0.12)
    9            .line(12).at(1).cast(float).match_range(453, 456)
    10           .line(13).at(1).cast(float).match_range(284, 316)
    11           .line(14).at(1).cast(float).match_range(24, 28)
    12   )
    13   (
    14       CheckIfFile('nf_output_points.paramnames', verbose_on_passed=True)
    15           .line(1).match('bg', verbatim=False)
    16           .line(2).match('x0', verbatim=False)
    17           .line(3).match('amp', verbatim=False)
    18           .line(4).match('sigma', verbatim=False)
    19   )
    ```
    The syntax is pretty self explanatory. For example in line 6, we just check if the file `nf_output_res.dat` with lines splitted with spaces (`with_split(' ')`) at file line 12, check if the second part of the split line string matches a range as a float.
    It is possible to chain calls of the `CheckIfFile` class.
    In line 15 we can see just a raw line string match using regex (`verbatim=False`) or just a verbatim match (i.e. char-by-char) (`verbatim=True`). Most of the time you might just want to use regex mode.
    It is also possible to pass the `verbose_on_passed=True` flag to the `CheckIfFile` constructor to display test verbose results to the stdout even if they are passing.
    A test folder will only pass if **all** of the conditions under this file are met.

- The `modes.json` file:
This is where things get interesting. This file allows to declare your own variables to use in the `nf_input.dat.in` file much like the `@nf_test_version_input@`. Let's see an example of a `modes.json` file go through it:
    ```json
    {
        "variants": [
            {
                "fit_func": "GAUSS_BG",
                "clustering_func": "k"
            },
            {
                "fit_func": "GAUSS_BG",
                "clustering_func": "g"
            },
            {
                "fit_func": "GAUSS_EXP_BG",
                "clustering_func": "k"
            },
            {
                "fit_func": "GAUSS_EXP_BG",
                "clustering_func": "g"
            }
        ]
    }
    ```
    Lets say we wanted to make tests for multiple clustering modes. The above json file has 4 variants, this means it will generate 4 different test folders, in each one it will replace the `@my_var_name@` inside the `nf_input.dat.in` file with the correspondig value in the `modes.json` file.
    For example, for this `modes.json` file we could alter the above example input file like so:
    ```
    y @clustering_func@ 0.5 0.2     # cluster analysis setup
    @nf_test_current_ntries@ 100000 # Number of tries and maximum of steps
    @fit_func@ 	  		# Name of the function
    ```
    The `modes.json` file also supports arrays:
    ```json
    {
        "variants": [
            {
                "fit_func": "GAUSS_BG",
                "clustering_func": "k",
                "cluster_params": [0.5, 0.2]
            },
            ...
        ]
    }
    ```
    accessible via (in this case) `@cluster_params_0@` and `@cluster_params_1@` (i.e. `@my_var_<index>@`).

    ```
    y @clustering_func@ @cluster_params_0@ @cluster_params_1@  # cluster analysis setup
    @nf_test_current_ntries@ 100000                            # Number of tries and maximum of steps
    @fit_func@ 	  		                           # Name of the function
    ```

    Cmake will then generate the tests for each test 'variant' defined and replace the `@vars` with the value in the json file.

    > :warning: For now there is no default values so, if the `modes.json` does have a variant without values the `nf_input.dat` variable location will be empty.
### Running tests

To run tests simply call `make test-all` on your cmake build directory.
> :warning: Testing requires python3 (tested with python3.8).
