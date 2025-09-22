# NESTED FIT

Nested_fit is a data analysis program based on the Bayesian statistics for the computation of, for a given set of data and chosen model, the complete probability distribution for each parameter and the Bayesian evidence.
The evidence calculation is based on the nested algorithm from Skilling 2004 [1,2] with the implementation of specific traversall for the search of new live points.
It is written in Fortran/C++/Python and includes complementary routines for visualizing the output results and for doing automatic analysis of series of data.
More information on the program can be found in Refs. [A,B,C] here above.

- :hammer: Jump to [installation](#Installation-instructions).
- :page_with_curl: Jump to [usage](#Usage).

### License

Users are required to accept the license agreement given in LICENSE file. Nested Fit is free for academic usage.

Users are also required to cite the Nested Fit papers here below in their publications (at least (A) and (B)) and their authors.

**Reference articles of nested_fit**:
- [A] M. Trassinelli, *Bayesian data analysis tools for atomic physics*, Nucl. Instrum. Methods B **408**, 301-312 (2017),
[doi:10.1016/j.nimb.2017.05.030](http://dx.doi.org/10.1016/j.nimb.2017.05.030),  [	arXiv:1611.10189](https://arxiv.org/abs/1611.10189)\
- [B] M. Trassinelli, *The Nested_fit Data Analysis Program*, Proceedings **33**, 14 (2019), [doi:10.3390/proceedings2019033014](https://doi.org/10.3390/proceedings2019033014)\
- [C] M. Trassinelli, P. Ciccodicola *Mean Shift Cluster Recognition Method Implementation in the Nested Sampling Algorithm*, Entropy **22**, 185 (2020), [doi:10.3390/e22020185](https://doi.org/10.3390/e22020185)
\
- [D] L. Maillard, F. Finocchi, M. Trassinelli * *Assessing Search and Unsupervised Clustering Algorithms in Nested Sampling*, Entropy **25**, 347 (2023), [doi:10.3390/e25020347](https://doi.org/10.3390/e25020347)

### Authors
Dr. Martino Trassinelli\
CNRS, Institute of NanoSciences of Paris\
email: martino.trassinelli AT cnrs.fr\
email: m.trassinelli AT gmail.com

Lune Maillard\
Institute of NanoSciences of Paris, Sorbonne Université, CNRS\
email: lune.maillard AT insp.upmc.fr

César Godinho\
LIBPhys / NOVA University of Lisbon\
email: c.godinho AT campus.fct.unl.pt

## Quick start examples

**Quick start with google colab:** 
[Download](https://github.com/martinit18/nested_fit/blob/dev/examples/jupyter_notebooks/quick_start_with_google_colab.ipynb)
[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/martinit18/nested_fit/blob/dev/examples/jupyter_notebooks/quick_start_with_google_colab.ipynb)

Other jupyter notebook examples can be found in `examples/jupyter_notebooks`.

## Installation instructions
> :warning: Windows support is deprecated. Compile at own risk.

### Using PIP
For most users if you are running on Linux or MacOS (x86_64 only!), chances are you can just install directly from pip.
```sh
pipx install nested_fit
```

### Installing from source (automatic)
If you are not running intel x86_64 you can install via the following script:
```bash
curl -sSL https://raw.githubusercontent.com/martinit18/nested_fit/refs/heads/master/install.sh | bash
```

Or if you wish to install the dependencies manually beforehand:
```bash
curl -sSL https://raw.githubusercontent.com/martinit18/nested_fit/refs/heads/master/install.sh | bash -s -- --no-deps
```

Or if you want to install a nightly version within a virtual environment:
```bash
curl -sSL https://raw.githubusercontent.com/martinit18/nested_fit/refs/heads/master/install.sh | bash -s -- --use-venv --nightly
```

### Installing from source (manual)

To manually install from source the following dependencies are required:
- cmake ( >= 3.10 )
- make
- gcc
- g++
- gfortran
- python3 ( >= 3.8 )
- pipx

If you wish to also build the python package for bindings and analysis support:
```bash
pipx install git+https://github.com/martinit18/nested_fit.git
# or
git clone https://github.com/martinit18/nested_fit.git
pipx install ./nested_fit -v
```

If all else fails you will need to manually compile nested_fit from source and generate the python library in pure mode.
This 'hack' is done via the editable installation mode:
```bash
# Clone the repo
git clone https://github.com/martinit18/nested_fit.git

# Make build directory
mkdir -p nested_fit/build

# Configure (and specify install prefix if required)
cmake -S nested_fit -B nested_fit/build -DOPENMP=ON -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=<install_path>

# Compile and install
cmake --build nested_fit/build --config Release

# Now you will have the nested_fit binary available at nested_fit/bin/nested_fit_xxx
export PATH=$PATH:<your_clone_path>/nested_fit/bin

# Or alternatively install from within your build tool
cmake --build nested_fit/build --target install

# Now install the python library in editable mode skipping compilation
pip install -e ./nested_fit -v
```

:warning: If you have further issues please refer to the file *STEPBYSTEP_INSTALL.md*.

:warning: **This installation may not work with ANACONDA.**

**CMake options**

| Option             | Description                                                          | Default |
|:-------------------|:---------------------------------------------------------------------|:-------:|
|NORNG               | Set the nested_fit to use a set seed. Internal test use mainly.      | OFF     |
|OPENMP              | Enable/Disable OpenMP support.                                       | OFF     |
|LAPACK              | Use LAPACK library functions instead of the internal ones.           | OFF     |
|LTRACE              | Enable trace logging output. Will hinder performance.                | OFF     |
|PPROF               | Enable performance counter profilling.                               | OFF     |

## General comments
- After a default installation via pipx (directly via PyPI or git clone) or script `nested_fit_xxx` will be aliased as `nested_fit`.
- Only if you installed via the manual and editable mode will you be required to use `nested_fit_xxx`.

## Comments for macOS users
- If you are running `gfortran` installed with homebrew, you should use `gcc` and `g++` from homebrew as well and not the macOS preinstalled one.
For this make sure that homebrew/bin has a priority on the other bin directories
(e.g. something like `export PATH=/opt/homebrew/bin:<other stuff of you>:$PATH` in your .bashrc)
making sure your `g++` is pointing the homebrew `g++-XX`.
Eventually creating the `ln -s /opt/homebrew/bin/g++-XX g++` link if required.
- The use of the following cmake option can help too:
``-DCMAKE_Fortran_COMPILER=`which gfortran` -DCMAKE_C_COMPILER=`which gcc` -DCMAKE_CXX_COMPILER=`which g++``

## File descriptions

**Input files**:
- `nf_input.yaml`: Main input file of the program with all user parameter definitions and run configurations.
More info is available [here](#Usage).
- `datafile` to analyze indicated in `nf_input.yaml`.

**Main output files**:
- `nf_output_res.dat`: main output with the results.
It contains the details of the computation (n. of live points trials, n. of total iteration), the final evidence value and its uncertainty, the parameter values corresponding to the maximum of the likelihood function, the mean, the median, the standard deviation and the confidence intervals (credible intervals) (68%, 95% and 99%) of the posterior probability distribution of each parameter.
The corresponding information gain and the Bayesian complexity are also provided.
- `nf_output_res.json`: same as above but in JSON format.
- `nf_output_data_*.dat`: original input data together with the model function values corresponding to the parameters with the highest likelihood function value ('max') to the mean value ('mean') or median ('median'), the residuals and the uncertainty associated to the data.
- `nf_output_fit_*.dat`: Model function values with higher density sampling that the data (for plot purpose). In addition, different components of the model are given.
- `nf_output_tries.dat`: For each live points trial, it contains the final evidence, the number of iterations and the maximum value of the likelihood function.
- `nf_output_points.txt`: contains all discarded and final live points values, their associated likelihood values and posterior probabilities. From them, the different parameter probability distributions can be built.
- `nf_output_diag.dat`: contains likelihood values corresponding to the birth of the discarded and final live points, and the rank of the new live point once inserted in the enemble. These values are used for statistics of the evidence (with the help of Anesthetic library) and for diagnostics.
For this purpose, the python module `nested_py` can be used also for compressed `nf_output_points.txt` files (using gzip with the name `nf_output_points.txt.gz`).
Together with this file, also the files `nf_output_points.paramnames` and `nf_output_points.ranges` are created for the use of GetDist and Anesthetic python libraries.
- `nf_output_cluster_mean_std.dat`: contains the number of clusters, number of point per clusters, and mean and standard deviations for each cluster (if the clustering option is activated).
- `nf_output_cluster_max.dat`: contains the number of clusters, the maximum value of the likelihood and the corresponding parameter values (if the clustering option is activated).

**Details of the input file line by line**

A complete selection of input files example is given in the folder `examples` where use cases of the python library are given.

It follows a complete description of `nf_input.yaml` file.

```yaml
version: 5.5                             # Program version
calculation_mode: DATA                   # Type of calculation
```
The type of calculation is spefified by `calculation_mode` variable. 
Available options are:
- `DATA`: for data analysis. A likelihood function is explored the Bayesian evidence is evaluated. It  requires a data file to read and thus the inputs `datafiles, specstr, likelihood`.
- `INTEGRAL`: for the calculation of the integral of a given function.
- `POTENTIAL`: for exploration of a potential energy and for building the partition function. 

```yaml
datafiles: file1.csv [, file2.csv, ...]  # Name of the data file(s)
```
If you have space- or tab-separated files, select the `.tsv` format adding the line
```yaml
filefmt: .tsv
specstr: x,c,ce                          # Datafile layout 
```
A typical example could be `ce, i, i, x, c`, where the first column indicate the error bars, the second and the third are ignored, the fourt indicates the x-coordinate and the last the values.

The `specstr` field tells nested_fit what data the datafile columns have.

`specsct` specification:
- `x`: This is the x data column (required).
- `c`: This is the counts data column (required).
- `ce`: This is the error data column (optional).
- `i`: This column will be ignored by nested_fit (optional).

```yaml
likelihood: GAUSSIAN                     # The likelihood function
```

Likelihood functions available (for data with error bars, Poisson statistics is assumed for counts):
- `GAUSSIAN`: Default normal distribution assuming data likelihood.
- `MOD_JEFFREYS`: Robust likelihood that does not assume a majorated error distribution (see Ref. [2]).

```yaml
function:                                               # Choose among the following expressions
    expression: f(x, a, b) = ax + b                     # Use LaTeX
    # expression: f(x, a, b) = \texttt{linear}(x, a, b) # Or use C++/Fortran
    # expression: GAUSS_BG                              # Or use a nested_fit legacy function (deprecated)

    # Or for multiple files use the following nomenclature (one expressian for each file)
    # expression_1: ...
    # expression_2: ...
    # expression_<n>: ...

    params:                                             # Parameters boundaries and co.
      a:  { value: 0.11,    step: -1, min: 0,      max: 0.5}
      b:  { value: 450,     step: -1, min: 400,    max: 600}
```
More details on the function definitions are presented below [here](#Function-Definition), and in particular
- LaTeX specification [here](#LaTeX-Specification).
- C++/Fortran API specification [here](#C++/Fortran-Specification).

```yaml
search:
    livepoints: 200     # Number of live points
    method: RANDOM_WALK # Search method
    param1: 0.2         # Param 1 of chosen method (see below)
    param2: 20          # Param 2 of chosen method (see below)
    max_tries: 1000     # Maximum tries before stop (max_tries * tries_mult)
    tries_mult: 100     # Max tries multiplier
    num_tries: 1        # Number of runs
    max_steps: 100000   # Max number of steps before stop
```

For the moment there are, 
- four random walks: `RANDOM_WALK`, `RANDOM_WALK_SYN`, `RANDOM_WALK_RECENT`,`RANDOM_WALK_NO_DB`. 
For `RANDOM_WALK_NO_DB` (where two alternative methods are implemented, see Ref. [C]) or just one:  `RANDOM_WALK_SYN` (where only the synthetic live point creation is implemented) and `RANDOM_WALK_RECENT` where we recenter with respect to the mean value of the live point. 
Except for the simplest researche, the detailed balance is maybe not respected but it can be more efficient for finding the minima for certain cases.
- A uniform search around each live point `UNIFORM`, 
- Three versions of slice sampling: `SLICE_SAMPLING`, `SLICE_SAMPLING_TRANSF`,`SLICE_SAMPLING_ADAPT`.  The first two correspond, respectively, to the search being done in two different spaces (transformed and real) with the first one faster than the second one. `SLICE_SAMPLING_ADAPT` an adaptable step but the detailed balance is maybe not respected. 

The first two parameters of the above line are specific to the search algorithm:
- `RANDOM_WALK`,  `RANDOM_WALK_NO_DB` par. 1: fraction of standard deviation for each jump, par. 2: number of jumps. Suggested values: 0.1-0.2, 10-40. 
-  `SLICE_SAMPLING`, `SLICE_SAMPLING_TRANSF`, and `SLICE_SAMPLING_ADAPT` par. 1: fraction of standard deviation for segment exploration, par. 2: number of jumps. Suggested values: ~1, 3-5. 
- `UNIFORM` par. 1: fraction of standard deviation for the box size, par. 2: number of jumps. Suggested values: 0.1-1, 1.

```yaml
convergence:
    method:    LIKE_ACC  # Method used for convergence 
    accuracy:  1.E-05    # Evidence final accuracy (in this case)
    parameter: 0.01      # Additional convergence parameter
```

For the moment, there are three convergence methods:
- `LIKE_ACC`: the algorithm stops when the difference between the calculated evidence and the estimated total evidence is below a certain value (first parameter on the above line). Typical value for the parameter : 1.E-05. In this case, the function that is maximised is the log-likelihood and it is associated to data.
- `ENERGY_ACC`: the algorithm stops when the difference between the calculated partition function and the estimated total partition function is below a certain value (first parameter on the above line). The second parameter corresponds to the temperature at which the partition function is calculated. Typical value for the first parameter : 1.E-05. In this case, the function that is maximised is the opposite of the energy function.
- `ENERGY_MAX`: the algorithm stops when the difference between the current contribution to the partion function and its maximal previous contribution is below a certain value (first parameter on the above line). The second parameter corresponds to the temperature at which the partition function is calculated. Typical value for the first parameter : -10.  In this case, the function that is maximised is the opposite of the energy function.

After convergence is reached, all remaining live points are assigned: 
- the logarithm of the likelihoods averaged over the live points (`LIKE_ACC` case),
- the opposite of the energies averaged over the live points (`ENERGY_ACC` and `ENERGY_MAX` cases).



```yaml
clustering:
    enabled:     true # False by default (if ommited)
    method:      f    # Clustering method (see below)
    parameter1:  0.5  # Clustering parameter 1
    parameter2:  0.2  # Clustering parameter 2
```

For the moment four clustering algorithms are implemented. The two parameters are specific to the method
For the second option:
- `f`: mean-shift with flat kernel (par. 1: distance (relative to the maximum distance))
- `g`: mean-shift with gaussian kernel (par. 1: distance, par. 2: bandwidth)
- `d`: dbscan (par. 1: distance, par. 2 : minimum number of neighbours)
- `s`: agglomerative clustering with single linkage (par. 1: distance limit)
- `k`: k nearest neighbours (no parameters)


## Function definition

```yaml
function:
    expression:  f(x,a,b) = a + b * x # function expression in latex form
    params:
    # Compact 
    a: { value: 0, step: -1, min: 0, max: 10 } # `fixed` is false by default
    b: { value: 0, step: -1, min: 0, max: 10 } # `fixed` is false by default
    
    # Extended
    e_a:
      value: 0
      step: -1
      min: 0
      max: 10
      fixed: false

data: { xmin: 1, xmax: 100, ymin: 0, ymax: 0 }
```
These entries of the file define how the function given parameters should vary. Their `min`/`max` span,
their `MCMC` step and either if they are variable or fixed. Both an extended and a compact version of declaring the parameters are available.

The `data` field allows to control the input file's X and Y span of which the `nested_fit` will run from. Allowing to crop data if necessary.
As of version 5.2, `ymin` and `ymax` can be both zero, meaning automatica span. However, `xmin`/`xmax` should always contain a valid range.

### LaTeX Specification
LaTeX is the preferred mode to write input functions whenever possible. It is also the main mode of calling native functions from the input file.
LaTeX functions need to follow the syntax:
```
<my_func_name>(x, a_0, a_1, ..., a_n) = <definition>
```
where the user defined arguments `a_i` must be of one of the following forms: `<letter>_<char>` or `<letter>`, where `<char>` is any alphanumeric character and `<letter>` any lower or uppercase letter of the english alphabet.
Examples: `I_0`, `A`, `x_2`. Any other form is invalid. For 1D functions (currently the only one supported with this mode), `x` must be strictly named, and the first parameter.

Now for the function `<definition>`. Here one can:

- Use basic math. Examples `ax + b` (with `a` and `b` arguments), `a*x + b` ('programing' form also allowed). [Available operators: `*`, `+`, `-`, `/`]
- Use math embedded basic functions. Examples `a\sqrt{x} + b` or `a\exp{x-b}`. [Available functions: `sqrt`, `exp`, `log`, `sin`, `cos`, `tan`, `arcsin`, `arccos`, `arctan`, `Gamma`, `abs`, `erf`, `sign`]
- Use some LaTeX constructs. Examples `\frac{x}{a + b}` or `a\exp{\frac{x}{b}}`. [Available constructs: `frac`]
- Use mathematical constants: `a\pi + bx`. [Available constants: `pi`]
- Call other user defined functions via syntax: `\texttt{<other_func_name>}(x, a_0, a_1, ..., a_n)`, where the passed parameters should be some currently available arguments. Alternatively one can also use `mathrm` isntead of `texttt`.
- Call system defined functions via the same syntax as above and listed here below.
Additional functions can be added by the user in the function `internal_func.f90`.

|Function|Declaration|Description|
|:-:|:-:|:-:|
| `GAUSS_IF` | `GAUSS_IF(x, x_0, A, s)` | Gaussian with integral `A`, mean `x_0` and standard deviation (sigma) `s` |
| `LORE_IF` | `LORE_IF(x, x_0, A, g)` | Lorentzian (Cauchy) profile with integral `A`, mean `x_0` and width (gamma)  `g` |
| `VOIGT_IF` | `VOIGT_IF(x, x_0, A, s, g)` | Voigt profile (convolution between a Gaussian and a Lorentzian, with integral `A`, mean `x_0`, sigma `s` and gamma  `g` |
|`WofzRe`| `WofzRe(zr, zi)` | `zr`/`zi`: the real and imaginary part of the<br/> given input, respectively.
|`Interpolate`| `Interpolate(filename, x, smooth)` | `filename`: the name of the file where the xy data<br/> is availabe (`.csv` format).<br/><br/>`x`: where to evaluate the spline.<br/><br/>`smooth`: The spline smoothing factor. Around the same order of magnitude as the number of points m (more precisely, between m - sqrt( 2 * m ) and m + sqrt( 2 * m ), see `curfit.f`for more details).

One can use this declaration mode directly on the input file:
```yaml
function:
    expression: test_func(x, a, b) = \frac{x}{\exp{2b\pi}} + \sin{b}
```
Or it is also possible to add the function via the command line:
```sh
nested_fitx.x.x -fa 'test_func(x, a, b) = \frac{x}{\exp{2b\pi}} + \sin{b}'
```

This function would then be available and could be used in the following fashion (which is functionally equivalent to using the function directly):
```yaml
function:
    expression: f(x, a, b) = \texttt{test_func}(x, a, b)
```

> :warning: All functions ran in nested_fit either via normal execution on using the `-fa` command. Get stored under a cache and are reusable whenever necessary. If a function name is repeated, the old declaration is **overwritten without warning**. This is also valid for C++/Fortran native user functions.

### C++/Fortran Specification

This is the second mode to declare functions. Althouth giving a bit more work, it allows for a much finer control of what is going on.

There are two ways to do it:
1. by compyling your `.cpp` or `.f90` file with the command  
```sh
nested_fitx.x.x -fa example.f90
```
N.B. the function file has to be in the analysis folder where you execute the program.

2. by writing the function in the file `internal_func.f90` and recompiling the ensemble of the program.

### Legacy function

Legacy functions are listed in USERFCN*.f files. They need the additional configuration parameter `npar`. 
Examples of use of a legacy function can be found in `examples/data_analysis/aaa_simple_example/legacy_func_input` directory.

*Additional information can be found in the reference articles.*

## Present version and history of the past versions

The present version is 5.5.2\
New features:
- Add PyPI binary distribution for x86_64 macOS
- Add PyPI binary distribution for multiple linux systems
- Add PyPI source distribution as a default for other systems
- Rename CLI command to `nested_fit` to use the latest installed version via pip


Previous versions are:
 - 5.4 Merge of executable for data analysis and function exploration via the new calculation mode variable \
Debug of not-yet  working feature of the version 5 compared to the version 4 \
New outputs with maxima of each cluster \
New RANDOM_WALK function with detailed balance respected
 - 5.3 New jupyter notebooks running in Google Colab \
New innterpolation functions in python library \
Live display when sampling from python. Works in console and jupyter notebooks \
Live display featured maximum likelihood prediction plot \
Add input info on JSON output file for parsing.
 - 5.2 Add JSON output for easier manipulation of results. \
New simple python interface to embed nested_fit on source code.
 - 5.1 Add feature for older systems not easily supporting cmake to configure via GNU autotools. \
 Add performance profiling tool boilerplate code enabling a detailed analysis without hindering performance.
 - 5.0 New modified Jeffreys likelihood for data \
 Update README.md \
 Add CI support via github actions. Only available for linux and macOS. \
 Add support to fully install via pip. \
 Add python package install support (not published). \
 Support custom data file column ordering/separation. Support .csv, .tsv \
 New native function parser that reads users Fortran or C/C++ files with functions. \
 New LaTeX function parser that reads user inline functions. \
 Complete overhaul of input file and function user function definition.
 - 4.6.1 New search method \
Covariance matrix and its Cholesky decomposition calculated when 5% of the points have changed \
Number of calls recorded in two variables \
 - 4.5 New functions \
 New exercices \
 New modified Jeffreys likelihood for data \
 Number of calls recorded in the main output file \
 No limitation in number of steps \
 Record of birth likelihood values and rank for diagnostics in `nf_output_diag.dat` file \
 Implementation of Anesthetic package for evaluation of evidence uncertainty with one run in python library
 - 4.4 New "write_input" function in python library \
 New fit functions \
 External LAPACK library link option \ 
 OpenMP for parallel search of new points \
 OpenMPI support (only available for number of tries) \
 OpenMP parallelisation for independent live point search \ 
 New user function calling method \
 Add Windows support \
 New build system generator (CMake) \
 Improved performance
 - 4.3 New (test) function : harmonic potential in 3D and loggamma \
 Choice between different convergence methods : evidence or partition function  
 - 4.2 Additional search methods : Uniform search around each live point and Slice Sampling
 - 4.1 New cluster recognition methods added
 - 4.0  2D data analysis for count-type XY \
 Computation acceleration for the 1D case introducing a mask instead of IF condition in the likelihood \
 New ERFPEAK function \
 New example files for python analysis with pandas
 - 3.5 Modularization of the search and cluster recognition methods in preparation of implementation of new algorithms \
 New interpolation options for 1D and 2D histograms using GetDist Python package \
 Correction of some bugs in the python library \
 Additional folder with exercises is now available \
 Installation instructions now available \
 Compatible now with intel fortran (options to change in Makefile)
 - 3.4 Introduction of benchmark tests with synthetic likelihood function via the module Mod_likelihood_tests,f90 (instead of Mod_likelihood.f90).
 Available tests: TEST_GAUSS (multidimensional Gaussian), TEST_GAUSSIAN_SHELLS (multidimensional Gaussian shells, worse case with available search and clustering methods),
 TEST_EGGBOX (eggbox style profile to test clustering), TEST_ROSENBROCK (Rosenbock function test for 2-n dimension).\
 Change of the outputs: `nf_output_points.dat` -> `nf_output_points.txt`, plus files `nf_output_points.paramnames`,  and `nf_output_points.ranges` to be compatible with GetDist Python package. New 'triangle plot' available now.
 - 3.3 Modular version of likelihood function in preparation for handling more complex data (2D data, ...).
 - 3.2  This is the first version with free sharing code only.
    Pion mass function and laser interpolation taken out to avoid Numerical Recipes.\
    Indexing for sorting data from SLATEC routine now.\
    Log(factorial) and gamma function from intrinsic function DLGAMMA now (and via a new routine for the factorial).\
    Test for integer input for Poisson likelihood.\
    Fitpack for splines in Shirley profile too.
 - 3.1  Optimization of parallel computing.\
      Corrections to Shirley functions.\
      Add of latest Shirley functions.\
      Fix a bug in the fixed parameters.\
      Add of the time stamp to the cluster analysis files
 - 3.0  Cluster analysis for live points to improve (very much!!!) the search efficiency.
 - 2.3  Parallelism optimization. Usercondition routine removed\
      Correction for gaussian priors (before was done at each jump, now only at the end).
 - 2.2  Add of "Rocking curve" profile that uses external simulated data for set of files
      (parallel and antiparallel, still to test).
      Solved problem with probabilities > 1 for gaussian data. Each data point is considered distributed
      with a Gaussian. But in the normalization factor, sigmas have not to appear. Otherwise probabilities
      can have a dimension.The variables are naturally transformed in dimensionless unit in the
      exponential part.\
      Extraction of live points values in case of non-convergence
 - 2.1  Add of "Rocking curve" profile that uses external simulated data and FITPACK routines for the use of smoothed B-splines.
 - 2.0  Treatment of data with error bars becomes possible.\
      No error bars: Likelihood with Poisson distribution.\
      Error bars   : Likelihood with Gaussian distribution.
 - 1.0  Add of Shirley background for photoemission spectra\
      Add of data output for mean and median parameter values (in addition to max likelihood values).
      Parallelization is back but only for the likelihood calculation otherwise it sucks (most of the time).
 - 0.9  Implementation of complex error function from TOMS algorithm n. 680 (included in Fortran 2008) WOFZ.f.
 - 0.8: Save in a file the different fit components (to implement in part of the functions).
 - 0.7: Add of pion mass function for sets.
 - 0.6: Add capabilities to analyze sets of spectra.
 - 0.5: Add of the pion mass function.
 - 0.4: Improved search algorithm for presence of local maxima
      Add of the information, minimum required iteration and complexity calculation
      D.S. Sivia, "Data Analysis, a Bayesian tutorial" (Ref. [2]),
      R. Trotta, Contemporary Physics 49, 71 (2008),
      J. Veitch and A. Vecchio, Phys. Rev. D 81, 062003 (2010).
 - 0.3: Optimization with memory allocation and more variables accessible from the input file.
 - 0.2: Version with parallel seek of groups of live points inspired by (not working anymore since 2016)
      J. Veitch and A. Vecchio, Phys. Rev. D 81, 062003 (2010) and
      N. Chopin and C.P. Robert, Biometrika 97, 741-755 (2010).
 - 0.1: Program developed from D.S. Sivia, "Data Analysis, a Bayesian tutorial" (2006) and L. Simons' original program.

## External codes and other contributors

### Additional included sources

In addition to the original files from the main authors, nesed_fit includes:
- *sterf* package, to profile the code (MIT license). Written by C. A. Godinho.
- *FITPACK (DIERCKX)* package, to fit and interpolating data with splines (no license)
  - Ref: Paul Dierckx, Curve and Surface Fitting with Splines, Oxford University Press, 1993
 Developer,
  - Address: Paul Dierckx, Department of Computer Science, K.U. Leuven, Celestijnenlaan 200 A, B-3001, Heverlee, Belgium, Paul.Dierckx@cs.kuleuven.ac.be
  - [http://nalag.cs.kuleuven.be/research/topics/fitpack.shtml](http://nalag.cs.kuleuven.be/research/topics/fitpack.shtml)
- `dpsort.f` + dependencies from SLATEC library (no license) for sorting arrays with respect another array
- `WOFZ.f`' for the complex error function from TOMS algorithm n. 680 (included in Fortran 2008)
- `rinteg.f` to calculate the integral of a function using a nq points quadrature ( nq is any integer between 1 and 14 ). Written by C. C. J. Roothaan (no license)

### Other contributors to the code
(chronological order)

- Anna Lévy (photoemission spectra models)
- Nancy Paul (rocking curves and Bragg spectrometer lines)

### Thanks for bugs corrections
(alphabetical order)

- Takuma Okumura

## Other resources

**Articles about nested sampling method**:\   
[1] J. Skilling, *Nested sampling for general Bayesian computation*, Bayesian Anal. **1**, 833-859 (2006)\
[2] D.S. Sivia and J. Skilling, *Data analysis: a Bayesian tutorial*. Second ed. 2006: Oxford University Press
