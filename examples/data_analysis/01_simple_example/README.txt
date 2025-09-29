Simple example with channel and counts in a CSV input file

- internal_func_input: example using an internal function located in `internal_func.f90`
- latex_func_input: example with a function written in latex style in the input file
- legacy_func_input: example with a function from the program fortran library

All above input files uses SLICE_SAMPLING as search engine. 
The following folders use different search method (and the internal function). Namely,

- RANDOM_WALK: example with a simple random walk search
- RANDOM_WALK_SYN: simple random walk plus synthetic live point
- RANDOM_WALK_NO_DB: simple random walk plus synthetic live point and re-entering method that breaks the detailed balance


For space separated or tab separated files, put `filefmt: .tsv` in the input file.



Reference name of the example: gauss_bg