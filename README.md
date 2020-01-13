# NESTED FIT

Nested_fit is a data program analysis based on the Bayesian statistics for the computation of, for a given set of data and chosen model, the complete probability distribution for each parameter and the Bayesian evidence.
The evidence calculation is based on the nested algorithm from Skilling 2004 [1,2] with the implementation of specific random walk for the search of new live points.
It is written in Fortran with some Python complementary routines for visualizing the output results and for doing automatic analyses of series of data.
More information on the program can be found in Refs. [3,4].


**Articles about nested sampling method**:\
[1] J. Skilling, *Nested sampling for general Bayesian computation*, Bayesian Anal. **1**, 833-859 (2006)\
[2] D.S. Sivia and J. Skilling, *Data analysis: a Bayesian tutorial*. Second ed. 2006: Oxford University Press\

**Reference articles of nested_fit**:\
[3] M. Trassinelli, *The Nested_fit Data Analysis Program*, Proceedings **33**, 14 (2019), [https://doi.org/10.3390/proceedings2019033014](https://doi.org/10.3390/proceedings2019033014)\
[4] M. Trassinelli, *Bayesian data analysis tools for atomic physics*, Nucl. Instrum. Methods B **408**, 301-312 (2017),
[http://dx.doi.org/10.1016/j.nimb.2017.05.030](http://dx.doi.org/10.1016/j.nimb.2017.05.030),  [	arXiv:1611.10189](https://arxiv.org/abs/1611.10189)

## Basic instructions

**Input files**:
- '*nf_input.dat*': Main input file of the program with all user parameter definition.
About the model parameter, use the same order than that one you can find in 'USERFCN.f' or 'USERFCN_SET.f' files.
- *datafile* to analyze indicated in 'nf_input.dat'.

If set of data files are analyzed, also
- '*nf_input_set.dat*': additional input relative to the other datafiles (name and fit limits).
- additional *datafiles* to analyze.

**Main output files**:
- '*nf_output_res.dat*': main output with the results.
It contains the details of the computation (n. of live points trials, n. of total iteration), the final evidence value and its uncertainty, the parameter values corresponding to the maximum of the likelihood function, the mean, the median, the standard deviation and the confidence intervals (credible intervals) (68%, 95% and 99%) of the posterior probability distribution of each parameter.
The corresponding information gain and the Bayesian complexity are also provided.
- '*nf_output_data_ .dat*': original input data together with the model function values corresponding to the parameters with the highest likelihood function value ('max') to the mean value ('mean') or median ('median'), the residuals and the uncertainty associated to the data.
- '*nf_output_fit_ .dat*': Model function values with higher density sampling that the data (for plot purpose). In addition, different components of the model are given
- '*nf_output_tries.dat*': For each live points trial, it contains the final evidence, the number of iterations and the maximum value of the likelihood function.
- '*nf_output_points.dat*': contains all discarded and final live points values, their associated likelihood values and posterior probabilities. From them, the different parameter probability distributions can be built.
For this purpose, the python function in '*nested_res_ .py*' can be used.


Additional information can be found in the reference articles.

## History of the past versions

The present version is 3.2.

Previous versions were including libraries without free licenses
(Numerical Recipes).
This is the first version with free sharing code only.

- Pion mass function and laser interpolation taken out to avoid Numerical Recipes
- Indexing for sorting data from SLATEC routine now
- Log(factorial) and gamma function from intrinsic function DLGAMMA now (and via a new routine for the factorial)
- Test for integer input for Poisson likelihood
- Fitpack for splines in Shirley profile too


Previous versions are:

 - 3.1  Optimization of parallel computing
      Corrections to Shirley functions
      Add of latest Shirley functions
      Fix a bug in the fixed parameters
      Add of the time stamp to the cluster analysis files
 - 3.0  Cluster analysis for live points to improve (very much!!!) the search efficiency
 - 2.3  Parallelism optimization. Usercondition routine removed
      Correction for gaussian priors (before was done at each jump, now only at the end)
 - 2.2  Add of "Rocking curve" profile that uses external simulated data for set of files
      (parallel and antiparallel, still to test)
      Solved problem with probabilities > 1 for gaussian data. Each data point is considered distributed
      with a Gaussian. But in the normalization factor, sigmas have not to appear. Otherwise probabilities
      can have a dimension.The variables are naturally transformed in dimensionless unit in the
      exponential part.
      Extraction of live points values in case of non-convergence
 - 2.1  Add of "Rocking curve" profile that uses external simulated data and
      FITPACK routines for the use of smothed B-splines
 - 2.0  Treatment of data with error bars becomes possible
      No errorbars: Likelihood with Poisson distribution
      Errorbars   : Likelihood with Gaussian distribution
 - 1.0  Add of Shirley background for photoemission spectra
      Add of data output for mean and median parameter values (in addition to max likelihood values)
      Parallelization is back but only for the likelihood calculation otherwise it sucks (most of the time)
 - 0.9  Implementation of complex error function from TOMS algorithm n. 680 (included in Fortran 2008) WOFZ.f
 - 0.8: Save in a file the different fit components (to implement in part of the functions)
 - 0.7: Add of pion mass function for sets
 - 0.6: Add capabilities to analyze sets of spectra
 - 0.5: Add of the pion mass function
 - 0.4: Improved search algorithm for presence of local maxima
      Add of the information, minimum required iteration and complexity calculation
      D.S. Sivia, "Data Analysis, a Bayesian tutorial" (2006)
      R. Trotta, Contemporary Physics 49, 71 (2008).
      J. Veitch and A. Vecchio, Phys. Rev. D 81, 062003 (2010)
 - 0.3: Optimization with mermory allocation and more variables accessible from the input file
 - 0.2: Version with parallel seek of groups of live points inspired by (not working anymore since 2016)
      J. Veitch and A. Vecchio, Phys. Rev. D 81, 062003 (2010) and
      N. Chopin and C.P. Robert, Biometrika 97, 741-755 (2010)
 - 0.1: Program developed from D.S. Sivia, "Data Analysis, a Bayesian tutorial" (2006) and L. Simons' original program

 ## Additional included sources

 In addition to the original files from the main author (M. Trassinelli), nesed_fit includes
 - *FITPACK (DIERCKX)* package, to fit and interpolating data with splines (no license)
   - Ref: Paul Dierckx, Curve and Surface Fitting with Splines, Oxford University Press, 1993
 Developer, 
   - Address: Paul Dierckx, Department of Computer Science, K.U. Leuven, Celestijnenlaan 200 A, B-3001, Heverlee, Belgium, Paul.Dierckx@cs.kuleuven.ac.be
   - [http://nalag.cs.kuleuven.be/research/topics/fitpack.shtml](http://nalag.cs.kuleuven.be/research/topics/fitpack.shtml)
 - '*dpsort.f*' + dependencies from SLATEC library (GPLv2 Licence) for sorting arrays with respect another array
 - '*WOFZ.f*'' for the complex error function from TOMS algorithm n. 680 (included in Fortran 2008)
 - '*rinteg.f*' to calculate the integral of a function using a nq points quadrature ( nq is any integer between 1 and 14 ). Written by C. C. J. Roothaan (no license)
