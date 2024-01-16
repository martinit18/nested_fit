PROGRAM NESTED_FIT
  ! Time-stamp: <Last changed by martino on Monday 07 June 2021 at CEST 10:29:22>
  !
  ! Please read README and LICENSE files for more information
  ! 5.1  Add feature for older systems not easily supporting cmake to configure via GNU autotools.
  !      Add performance profiling tool boilerplate code enabling a detailed analysis without hindering performance.
  ! 5.0  Update README.md
  !      Add CI support via github actions. Only available for linux and macOS.
  !      Add support to fully install via pip.
  !      Add python package install support (not published).
  !      Support custom data file column ordering/separation. Support .csv, .tsv
  !      New native function parser that reads users Fortran or C/C++ files with functions.
  !      New LaTeX function parser that reads user inline functions.
  !      Complete overhaul of input file and function user function definition.
  ! 4.5  New modified Jeffreys likelihood for data
  ! 4.4  New "write_input" function in python library
  !      New fit functions
  !      External LAPACK library link option
  !      OpenMP for parallel search of new points
  !      OpenMPI support (only available for number of tries)
  !      New user function calling method
  !      Add Windows support
  !      New build system generator (CMake)
  !      Improved performance
  ! 4.3  New (test) functions : harmonic potential in 3D and loggamma
  !      Choice between different convergence methods : evidence or partition function
  ! 4.2  New search method added: uniform (around each live point) and slice sampling
  !      New decay functions added
  !      Faster user function comparison; Default mode reverted to single threaded (Disabled OpenMP)
  ! 4.1  Several cluster recognition methods added.
  ! 4.0  2D data analysis available, new input and output files for future developments
  !      1D parallelization acceleration with masks instead of IF conditions in the likelihood
  ! 3.5  Modularization of the search algorithm (in preparation of new algorithms implementation)
  !      New interpolation options for 1D and 2D histograms using GetDist Python package
  !      Correction of some bugs in the python library
  !      Additional folder with exercises is now available
  !      Installation instructions now available
  !      Compatible now with intel fortran (options to change in Makefile)
  ! 3.4  Introduction of benchmark tests with synthetic likelihood functions
  !      via the module Mod_likelihood_tests,f90 (instead of Mod_likkelihood.f90)
  !      Available tests: TEST_GAUSS (multidimensional Gaussian)
  !                       TEST_GAUSSIAN_SHELLS (multidimensional Gaussian shells, worse case with available search and clustering methods)
  !                       TEST_EGGBOX (eggbox style profile to test clustering)
  !                       TEST_ROSENBROCK (Rosenbrock function to arbitrary dimension)
  !      Change of the outputs: nf_output_points.dat -> nf_output_points.dat, plus files nf_output_points.paramnames,
  !      and nf_output_points.ranges to be compatible with GetDist Python package and Polychord
  ! 3.3  Modular version of likelihood function in preparation for handling more complex data (2D data, ...)
  ! 3.2  Pion mass function and laser interpolation taken out to avoid Numerical Recipes
  !      Indexing for sorting data from SLATEC routine now
  !      Log(factorial) and gamma function from intrinsic function DLGAMMA now (and via a new routine for the factorial)
  !      Test for integer input for Poisson likelihood
  !      Fitpack for splines in Shirley profile too
  ! 3.1  Optimization of parallel computing
  !      Corrections to Shirley functions
  !      Add of latest Shirley functions
  !      Fix a bug in the fixed parameters
  !      Add of the time stamp to the cluster analysis files
  ! 3.0  Cluster analysis for live points to improve (very much!!!) the search efficiency
  ! 2.3  Parallelism optimization. Usercondition routine removed
  !      Correction for gaussian priors (before was done at each jump, now only at the end)
  ! 2.2  Add of "Rocking curve" profile that uses external simulated data for set of files
  !      (parallel and antiparallel, still to test)
  !      Solved problem with probabilities > 1 for gaussian data. Each data point is considered distributed
  !      with a Gaussian. But in the normalization factor, sigmas have not to appear. Otherwise probabilities
  !      can have a dimension.The variables are naturally transformed in dimensionless unit in the
  !      exponential part.
  !      Extraction of live points values in case of non-convergence
  ! 2.1  Add of "Rocking curve" profile that uses external simulated data and
  !      FITPACK routines for the use of smothed B-splines
  ! 2.0  Treatment of data with error bars becomes possible
  !      No errorbars: Likelihood with Poisson distribution
  !      Errorbars   : Likelihood with Gaussian distribution
  ! 1.0  Add of Shirley background for photoemission spectra
  !      Add of data output for mean and median parameter values (in addition to max likelihood values)
  !      Parallelization is back but only for the likelihood calculation otherwise it sucks (most of the time)
  ! 0.9  Implementation of complex error function from TOMS algorithm n. 680 (included in Fortran 2008) WOFZ.f
  ! 0.8: Save in a file the different fit components (to implement in part of the functions)
  ! 0.7: Add of pion mass function for sets
  ! 0.6: Add capabilities to analyze sets of spectra
  ! 0.5: Add of the pion mass function
  ! 0.4: Improved search algorithm for presence of local maxima
  !      Add of the information, minimum required iteration and complexity calculation
  !      D.S. Sivia, "Data Analysis, a Bayesian tutorial" (2006)
  !      R. Trotta, Contemporary Physics 49, 71 (2008).
  !      J. Veitch and A. Vecchio, Phys. Rev. D 81, 062003 (2010)
  ! 0.3: Optimization with mermory allocation and more variables accessible from the input file
  ! 0.2: Version with parallel seek of groups of live points inspired by (not working anymore since 2016)
  !      J. Veitch and A. Vecchio, Phys. Rev. D 81, 062003 (2010) and
  !      N. Chopin and C.P. Robert, Biometrika 97, 741-755 (2010)
  ! 0.1: Program developed from D.S. Sivia, "Data Analysis, a Bayesian tutorial" (2006) and Leo's program

   ! Module with math parameter + some parsing stuff
   USE MOD_MATH
   ! Module for automatic function parsing/compilation
   USE MOD_AUTOFUNC
   ! Module with user functions routines and legacy calls
   USE MOD_USERFCN
   ! Module for input parsing (legacy and YAML)
   USE MOD_INPUTPARSE
   ! Module for CLI lib
   USE MOD_ARGPARSE
   ! Module for optional variables
   USE MOD_OPTIONS
   ! Module for the input parameter definition
   USE MOD_PARAMETERS
   ! Module for likelihood for data analysis
   USE MOD_LIKELIHOOD
   ! Module for metadata
   USE MOD_METADATA
   ! Module for logging and displaying messages
   USE MOD_LOGGER
   ! Module for performance profiling
   USE MOD_PERFPROF
   ! Parallelization library !!!CAREFULL to the table dimension in this case!!
   USE OMP_LIB

#ifdef OPENMPI_ON
   USE MPI
#endif
  !USE RNG
  !
  IMPLICIT NONE
  ! Data
  INTEGER(4) :: i=0, j=0, k=0

  ! Parameters values and co.
  CHARACTER :: string*2048
  CHARACTER :: spec_str*1024 = 'x,c'
  CHARACTER :: version_file*20
  LOGICAL   :: file_exists

  ! Results from Nested sampling
  INTEGER(4)                           :: nall=0
  REAL(8)                              :: evsum_final=0., live_like_max=0.
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_final
  REAL(8), ALLOCATABLE, DIMENSION(:)   :: weight, live_like_final, live_max

  ! Final results
  REAL(8), ALLOCATABLE, DIMENSION(:)   :: par_mean, par_sd
  REAL(8), ALLOCATABLE, DIMENSION(:)   :: par_median_w, par_m68_w, par_p68_w, par_m95_w
  REAL(8), ALLOCATABLE, DIMENSION(:)   :: par_p95_w, par_m99_w, par_p99_w
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: weight_par
  REAL(8)                              :: mean_tmp=0., mean2_tmp=0., weight_tot=0., weight_int=0., weight_int_next=0.
  REAL(8)                              :: evsum_err=0.
  REAL(8)                              :: evsum_err_est=0.
  REAL(8)                              :: live_like_mean=0., info=0., comp=0.
  INTEGER(8)                           :: nexp=0

  ! Parallelization variables for master mpi node
  INTEGER(4)                             :: itry=1
  INTEGER(4), DIMENSION(1)               :: itrymax
  INTEGER(4), ALLOCATABLE, DIMENSION(:)  :: nall_try
  REAL(8), ALLOCATABLE, DIMENSION(:)     :: evsum_final_try, live_like_max_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: live_final_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:)   :: live_like_final_try, weight_try, live_max_try

  ! Parallelization variables for each mpi instance
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_final_try_instance
  REAL(8), ALLOCATABLE, DIMENSION(:)   :: live_like_final_try_instance, weight_try_instance, live_max_try_instance

  ! OpenMPI stuff
  INTEGER(4) :: mpi_rank !, mpi_cluster_size, mpi_ierror

  ! Time measurement variables
  REAL(8) :: seconds, seconds_omp, startt, stopt, startt_omp, stopt_omp

  ! Random number variables
  INTEGER(4) :: seed_array(33) = 1

  ! Function definitions
  EXTERNAL             :: NESTED_SAMPLING
#ifndef FUNC_TARGET
  INTEGER(4), EXTERNAL :: SELECT_USERFCN
#endif
  
  ! Map for the config reading
  TYPE(InputDataMap_t) :: input_config

  INTERFACE
   SUBROUTINE DISABLE_STDOUT() BIND(c, name='DisableStdout')
      USE, INTRINSIC :: iso_c_binding
      IMPLICIT NONE
   END SUBROUTINE
  END INTERFACE

#ifdef PPROF
  ! Start the top tree profiling (keep this as the first subroutine call)
  PROFILED(Nested_fit)
#endif

  ! Init the logger file
  CALL START_LOG()

  ! Init the autofunc module
  CALL INIT_AUTOFUNC()

  ! Add arguments to the executable (possibly prefer adding the flags for them into mod options)
  CALL ADD_ARGUMENT(argdef_t("compact-output", "c", .FALSE.,&
    'Sets nested fit console output to be more compact. &
     Ideal for systems with lower resolution or smaller dpi screens.',&
    B_COMPACT&
  ))

  CALL ADD_ARGUMENT(argdef_t("input-file", "i", .TRUE.,&
    "Overwrites the input filename. The input filename defaults to 'nf_input.dat'.",&
    B_INPUTFILE&
  ))

  CALL ADD_ARGUMENT(argdef_t("compile-cpp", "cc", .TRUE.,&
    "Changes the compilation command for c++ when adding a raw function via the `function-add` command &
     or the input file. This defaults to the g++ compiler: 'g++ -c -shared -O3 -w -fPIC'. &
     LaTeX functions always compile to Fortran internally. Refer to the `compile-f90` command to change their &
     compilation command.",&
    B_CPPCMPCMD&
  ))

  CALL ADD_ARGUMENT(argdef_t("compile-f90", "cf", .TRUE.,&
    "Changes the compilation command for Fortran when adding a raw function via the `function-add` command &
     or the input file. This defaults to the gfortran compiler: 'gfortran -cpp -c -shared -O3 -w -fPIC'. &
     LaTeX functions always compile to Fortran internally. This command also executes on their evaluation.",&
    B_F90CMPCMD&
  ))

  CALL ADD_ARGUMENT(argdef_t("cache-link", "cl", .TRUE.,&
    "Changes the link command for c++/Fortran when adding a raw function via the `function-add` command or the input file. &
     Also gets triggered on `cache-recompile` commands. Defaults to the gcc compiler: 'gcc -shared -fPIC'.",&
    B_LNKCMD&
  ))

  CALL ADD_ARGUMENT(argdef_t("cache-delete", "cd", .FALSE.,&
    "Deletes the user defined functions in the cache folder.",&
    B_DELCACHE&
  ))

  CALL ADD_ARGUMENT(argdef_t("cache-recompile", "cr", .FALSE.,&
    "Recompiles all of the functions available in the cache.",&
    B_RECUSRFUNC&
  ))

  CALL ADD_ARGUMENT(argdef_t("function-add", "fa", .TRUE.,&
    "Adds a new function with name <name>(x, ...)=<expression> to the cache. &
     For example: -fa 'gauss1D(x, u, s) = \frac{1}{s\sqrt{2\pi}}\exp(-\frac{(x-u)^2}{2s^2})'. &
     If the function <name> already exists, an overwrite will take place.",&
    B_ADDUSRFUNC&
  ))

  CALL ADD_ARGUMENT(argdef_t("function-run", "fr", .TRUE.,&
    "Runs function with name <name>(x, ...) from the cache. &
     For example: -fr 'gauss1D(2, 2, [2, 4])' would output the gaussian function value at 2 with u=2 and sigma=4. &
     If the function <name> does not exist, nothing happens.",&
    B_RUNUSRFUNC&
  ))

  CALL ADD_ARGUMENT(argdef_t("function-list", "fl", .FALSE.,&
    "Lists all of the functions available in the cache.",&
    B_LSTUSRFUNC&
  ))

  CALL ADD_ARGUMENT(argdef_t("verbosity", "v", .TRUE.,&
    "Change the verbosity level. &
     Available levels = ['none', 'error', 'warning', 'message', 'trace'].",&
    B_SETVERBOSITY&
  ))

  CALL ADD_ARGUMENT(argdef_t("suppress-output", "so", .FALSE.,&
    "Suppresses all output. Useful for automation / when console ouput is not required. &
     This is the linux equivalent of doing './nested_fitx.x.x > /dev/null'.",&
    B_SUPPRESSOUTPUT&
  ))
  
  ! Parse executable arguments !!! THIS NEEDS TO COME BEFORE THE MPI_INIT() SUBROUTINE !!!
  CALL PARSE_ARGUMENTS()

#ifdef OPENMPI_ON
    CALL MPI_INIT(mpi_ierror)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_cluster_size, mpi_ierror)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, mpi_rank, mpi_ierror)
#else
    mpi_rank = 0
#endif

  !!!!!!!! Initiate random generator with the same seed each time !!!!!!!!!!!
#ifdef NORNG_ON  
  IF(mpi_rank.EQ.0) THEN
      CALL LOG_WARNING_HEADER()
      CALL LOG_WARNING('Nested_fit is running with a set seed! This is intended for testing only!')
      CALL LOG_WARNING('If you are using this as a production setting change the cmake NORNG option to OFF.')
      CALL LOG_WARNING_HEADER()
      CALL sleep(1)
  ENDIF

  CALL RANDOM_SEED(PUT=seed_array)
#endif

#ifdef OPENMPI_ON
   CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_ierror)
#endif

  ! Calculate time elapsed !!!!!!!!!!!!!!!!!!!!
  ! Parallel real time (and number of threads)  
  !$ seconds = omp_get_wtime( )
  !$ nth = omp_get_max_threads()

  ! Absolute time
  CALL CPU_TIME(startt)
  !$ startt_omp = omp_get_wtime( )

  IF(mpi_rank.EQ.0) THEN
      ! Initialize values --------------------------------------------------------------------------------------------------------------
      filename = ' '
      funcname = ' '
      likelihood_funcname = ' '

      ! Read parameter file ------------------------------------------------------------------------------------------------------------
      INQUIRE(FILE=TRIM(opt_input_file), EXIST=file_exists)
      IF(.NOT.file_exists) THEN
         CALL LOG_ERROR_HEADER()
         CALL LOG_ERROR('Input file ('//TRIM(opt_input_file)//') was not found.')
         CALL LOG_ERROR('Aborting Execution...')
         CALL LOG_ERROR_HEADER()
         CALL HALT_EXECUTION()
      ENDIF

      CALL PARSE_INPUT(TRIM(opt_input_file), input_config)

      ! Check the version
      CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'version', version_file, MANDATORY=.TRUE.)
      IF(version.NE.version_file) THEN
         CALL LOG_WARNING_HEADER()
         CALL LOG_WARNING('Specified program version does not match the current one.')
         CALL LOG_WARNING('Specified = '//TRIM(version_file))
         CALL LOG_WARNING('Current   = '//TRIM(version))
         CALL LOG_WARNING('Some features could be deprecated, or not used.')
         CALL LOG_WARNING('Please upgrade the input file version.')
         CALL LOG_WARNING('Continuing with possible future errors...')
         CALL LOG_WARNING_HEADER()
      END IF

#ifndef FUNC_TARGET
      ! Prepare the likelihood module to receive values from the input file
      CALL PREPARE_LIKELIHOOD()
#endif

      ! General configuration
      CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'datafiles' , string             , MANDATORY=.TRUE. )
      CALL POPULATE_INPUTFILES       (string)
      CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'specstr'   , spec_str           , MANDATORY=.FALSE.) !      x,c by default
      CALL POPULATE_DATATYPE         (spec_str, data_type)
      CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'filefmt'   , fileformat         , MANDATORY=.FALSE.) !     .csv by default
      CALL POPULATE_FILEFMT          (fileformat)
      CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'likelihood', likelihood_funcname, MANDATORY=.FALSE.) ! GAUSSIAN by default
      CALL FIELD_FROM_INPUT_LOGICAL  (input_config, 'fileheader', opt_file_has_header, MANDATORY=.FALSE.) !    False by default
      
      ! Search configuration
      CALL FIELD_FROM_INPUT_INTEGER  (input_config, 'search.livepoints', nlive        , MANDATORY=.TRUE. )
      CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'search.method'    , search_method, MANDATORY=.TRUE. )
      CALL FIELD_FROM_INPUT_REAL     (input_config, 'search.param1'    , search_par1  , MANDATORY=.TRUE. )
      CALL FIELD_FROM_INPUT_REAL     (input_config, 'search.param2'    , search_par2  , MANDATORY=.TRUE. )
      CALL FIELD_FROM_INPUT_INTEGER  (input_config, 'search.max_tries' , maxtries     , MANDATORY=.TRUE. )
      CALL FIELD_FROM_INPUT_INTEGER  (input_config, 'search.tries_mult', maxntries    , MANDATORY=.TRUE. )
      CALL FIELD_FROM_INPUT_INTEGER  (input_config, 'search.num_tries' , ntry         , MANDATORY=.FALSE.) !      1 by default
      CALL FIELD_FROM_INPUT_INTEGER  (input_config, 'search.max_steps' , maxstep_try  , MANDATORY=.FALSE.) ! 100000 by default
      
      ! Convergence configuration
      CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'convergence.method'   , conv_method, MANDATORY=.TRUE.)
      CALL FIELD_FROM_INPUT_REAL     (input_config, 'convergence.accuracy' , evaccuracy , MANDATORY=.TRUE.)
      CALL FIELD_FROM_INPUT_REAL     (input_config, 'convergence.parameter', conv_par   , MANDATORY=.TRUE.)

      ! Clustering configuration
      CALL FIELD_FROM_INPUT_LOGICAL  (input_config, 'clustering.enabled'  , make_cluster  , MANDATORY=.FALSE.) ! False by default
      CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'clustering.method'   , cluster_method, MANDATORY=.FALSE.) !     f by default
      CALL FIELD_FROM_INPUT_REAL     (input_config, 'clustering.distance' , cluster_par1  , MANDATORY=.FALSE.) !   0.4 by default
      CALL FIELD_FROM_INPUT_REAL     (input_config, 'clustering.bandwidth', cluster_par2  , MANDATORY=.FALSE.) !   0.1 by default

      ! Data configuration
      ! TODO(César): Make this not mandatory and get bounds from data file
      IF(is_set) THEN
         DO i = 1, nset
            CALL FIELD_FROM_INPUT_REAL     (input_config, 'data_'//TRIM(ADJUSTL(INT_TO_STR_INLINE(i)))//'.xmin', xmin(1), MANDATORY=.TRUE. )
            CALL FIELD_FROM_INPUT_REAL     (input_config, 'data_'//TRIM(ADJUSTL(INT_TO_STR_INLINE(i)))//'.xmax', xmax(1), MANDATORY=.TRUE. )
            CALL FIELD_FROM_INPUT_REAL     (input_config, 'data_'//TRIM(ADJUSTL(INT_TO_STR_INLINE(i)))//'.ymin', ymin(1), MANDATORY=.FALSE.) ! 0 by default (i.e. whole data)
            CALL FIELD_FROM_INPUT_REAL     (input_config, 'data_'//TRIM(ADJUSTL(INT_TO_STR_INLINE(i)))//'.ymax', ymax(1), MANDATORY=.FALSE.) ! 0 by default (i.e. whole data)
         END DO
      ELSE
         CALL FIELD_FROM_INPUT_REAL     (input_config, 'data.xmin', xmin(1), MANDATORY=.TRUE. )
         CALL FIELD_FROM_INPUT_REAL     (input_config, 'data.xmax', xmax(1), MANDATORY=.TRUE. )
         CALL FIELD_FROM_INPUT_REAL     (input_config, 'data.ymin', ymin(1), MANDATORY=.FALSE.) ! 0 by default (i.e. whole data)
         CALL FIELD_FROM_INPUT_REAL     (input_config, 'data.ymax', ymax(1), MANDATORY=.FALSE.) ! 0 by default (i.e. whole data)
      ENDIF

      ! Legacy stuff required
      ! TODO(César): Deprecate this (whenever we implement the convolution function)
      CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'lr'    , lr    , MANDATORY=.FALSE.) ! r by default
      CALL FIELD_FROM_INPUT_INTEGER  (input_config, 'npoint', npoint, MANDATORY=.FALSE.) ! 0 by default
      CALL FIELD_FROM_INPUT_INTEGER  (input_config, 'nwidth', nwidth, MANDATORY=.FALSE.) ! 0 by default
      
      ! Function configuration
      IF(is_set) THEN
         DO i = 1, nset
            CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'function.expression_'//TRIM(ADJUSTL(INT_TO_STR_INLINE(i))), funcname(i), MANDATORY=.TRUE.) ! LaTeX Expression or Legacy name
         END DO
      ELSE
         CALL FIELD_FROM_INPUT_CHARACTER(input_config, 'function.expression', funcname(1), MANDATORY=.TRUE.) ! LaTeX Expression or Legacy name
      ENDIF
      CALL CONFIGURE_USERFUNCTION()

      ! Read set of spectra file parameter
      ! IF (is_set) THEN
      !    OPEN (UNIT=88, FILE='nf_input_set.dat', STATUS='old')
      !    ! READ(88,*,ERR=11,END=11) nset
      !    DO k = 2, nset
      !       READ(88,*,ERR=11,END=11) xmin(k), xmax(k), string ! TODO
      !    END DO
      !    ! DO k = 2, nset
      !    !    READ(88,*,ERR=11,END=11) filename(k)
      !    ! END DO
      ! 11   CONTINUE
      !    CLOSE(88)
      ! ENDIF
  ENDIF

  ! Receive data from the mpi root node
  ! All the code should be refactored really but for now
  ! TODO(César): Refactor this to a function
#ifdef OPENMPI_ON
      CALL MPI_Bcast(filename(1), 64, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(is_set, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(data_type, 3, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(likelihood_funcname, 64, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(nlive, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(conv_method, 64, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(evaccuracy, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(conv_par, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(search_method, 64, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(search_par1, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(search_par2, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(maxtries, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(maxntries, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(make_cluster, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(cluster_method, 1, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(cluster_par1, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(cluster_par2, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(ntry, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(maxstep_try, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(funcname(1), 64, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(lr, 1, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(npoint, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(nwidth, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(xmin(1), 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(xmax(1), 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(ymin(1), 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(ymax(1), 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(npar, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)

   IF(mpi_rank.NE.0) THEN
      !
      ! Allocate space for parameters and initialize
      ALLOCATE(live_max(npar),par_num(npar),par_name(npar),par_in(npar),par_step(npar), &
         par_bnd1(npar),par_bnd2(npar),par_fix(npar), &
         par_mean(npar),par_sd(npar), &
         par_median_w(npar), &
         par_m68_w(npar),par_p68_w(npar),par_m95_w(npar),par_p95_w(npar),par_m99_w(npar),par_p99_w(npar))
         live_max = 0.
         par_num = 0
         par_name = ' '
         par_in = 0.
         par_step = 0.
         par_bnd1 = 0.
         par_bnd2 = 0.
         par_fix = 0
         par_mean = 0.
         par_sd = 0.
         par_median_w = 0.
         par_m68_w = 0.
         par_p68_w = 0.
         par_m95_w = 0.
         par_p95_w = 0.
         par_m99_w = 0.
         par_p99_w = 0.
   ENDIF
#endif

 !----------------------------------------------------------------------------------------------------------------------------------

  ! Some tests and messages
  IF(mpi_rank.EQ.0) THEN
      ! Not implemented combination of inputs
      IF (data_type(1:1).EQ.'2'.AND.is_set) THEN
         CALL LOG_ERROR_HEADER()
         CALL LOG_ERROR('Set of 2D files not yet implemented. Change your input file.')
         CALL LOG_ERROR_HEADER()
         CALL HALT_EXECUTION()
      END IF
  END IF

  ! Initialize likelihood function
  CALL INIT_LIKELIHOOD()

#ifdef OPENMPI_ON
   CALL MPI_Bcast(par_num, npar, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
   CALL MPI_Bcast(par_fix, npar, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
   CALL MPI_Bcast(par_name,npar*10, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
   CALL MPI_Bcast(par_step, npar, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
   CALL MPI_Bcast(par_in, npar, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
   CALL MPI_Bcast(par_bnd1, npar, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
   CALL MPI_Bcast(par_bnd2, npar, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)

   CALL MPI_Bcast(funcid, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)

   CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_ierror)
#endif


  IF(mpi_rank.EQ.0) THEN
     ! Allocate parallel stuff ---------------------------------------------------------------------------------------------------------
     ALLOCATE(live_like_final_try(maxstep_try,ntry),weight_try(maxstep_try,ntry),&
          live_final_try(maxstep_try,npar,ntry),live_max_try(npar,ntry),&
          nall_try(ntry),evsum_final_try(ntry),live_like_max_try(ntry))
     live_like_final_try = 0.
     weight_try = 0.
     live_final_try = 0.
     live_max_try = 0.
     nall_try = 0
     evsum_final_try = 0.
     live_like_max_try = -100000.
#ifdef OPENMPI_ON
     ALLOCATE(live_final_try_instance(maxstep_try,npar))
     ALLOCATE(live_like_final_try_instance(maxstep_try), weight_try_instance(maxstep_try), live_max_try_instance(npar))
#endif
     !
  ELSE
     ALLOCATE(live_final_try_instance(maxstep_try,npar))
     ALLOCATE(live_like_final_try_instance(maxstep_try), weight_try_instance(maxstep_try), live_max_try_instance(npar))
  ENDIF



  ! If all parameters are fixed, skip Nested sampling ------------------------------------------------------------------------------
  IF (ALL(par_fix.EQ.1)) THEN
     live_max = par_in
     par_mean = par_in
     par_median_w = par_in
     ! To check the likelihood function
     evsum_final = LOGLIKELIHOOD(par_in)
     IF(mpi_rank.EQ.0) THEN
         CALL LOG_WARNING_HEADER()
         CALL LOG_WARNING('All parameters are fixed.')
         CALL LOG_WARNING_HEADER()
     ENDIF
     GOTO 501
  END IF

#ifdef OPENMPI_ON
  IF(mpi_rank.EQ.0) THEN
      IF(mpi_cluster_size.GT.ntry) THEN
         CALL LOG_WARNING_HEADER()
         CALL LOG_WARNING('Specified MPI cluster size is bigger than the number of tries in the input file.')
         CALL LOG_WARNING('This feature is not supported at the moment.')
         CALL LOG_WARNING('Idling the unused processes.')
         CALL LOG_WARNING_HEADER()
      ENDIF
  ENDIF
#endif

  !
  ! Run the Nested sampling
#ifndef OPENMPI_ON
   DO itry=1,ntry
      CALL NESTED_SAMPLING(itry,maxstep_try,nall_try(itry),evsum_final_try(itry), &
            live_like_final_try(:,itry),weight_try(:,itry),&
            live_final_try(:,:,itry),live_like_max_try(itry),live_max_try(:,itry), 0, 0)
   END DO
#else
   IF(mpi_rank.LT.ntry) THEN
      CALL NESTED_SAMPLING(mpi_rank,maxstep_try,nall_try_instance,evsum_final_try_instance, &
         live_like_final_try_instance,weight_try_instance,&
         live_final_try_instance,live_like_max_try_instance,live_max_try_instance, mpi_rank, mpi_cluster_size)
      
      ! Wait for all the calculations to finish
      CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_ierror)

      ! All the code should be refactored really but for now
      ! TODO(César): Refactor this to a function
      IF(mpi_rank.NE.0) THEN
         CALL MPI_SEND(nall_try_instance, 1, MPI_INT, 0, mpi_rank, MPI_COMM_WORLD, mpi_ierror)
         CALL MPI_SEND(evsum_final_try_instance, 1, MPI_DOUBLE, 0, mpi_rank, MPI_COMM_WORLD, mpi_ierror)
         CALL MPI_SEND(live_like_final_try_instance, maxstep_try, MPI_DOUBLE, 0, mpi_rank, MPI_COMM_WORLD, mpi_ierror)
         CALL MPI_SEND(weight_try_instance, maxstep_try, MPI_DOUBLE, 0, mpi_rank, MPI_COMM_WORLD, mpi_ierror)
         CALL MPI_SEND(live_final_try_instance, maxstep_try*npar, MPI_DOUBLE, 0, mpi_rank, MPI_COMM_WORLD, mpi_ierror)
         CALL MPI_SEND(live_like_max_try_instance, 1, MPI_DOUBLE, 0, mpi_rank, MPI_COMM_WORLD, mpi_ierror)
         CALL MPI_SEND(live_max_try_instance, npar, MPI_DOUBLE, 0, mpi_rank, MPI_COMM_WORLD, mpi_ierror)
      ENDIF
   ENDIF
#endif

#ifdef OPENMPI_ON
   ! Gather all of the runs in the root node back again
   IF(mpi_rank.EQ.0) THEN
         nall_try(1) = nall_try_instance
         evsum_final_try(1) = evsum_final_try_instance
         live_like_final_try(:,1) = live_like_final_try_instance
         weight_try(:,1) = weight_try_instance
         live_final_try(:,:,1) = live_final_try_instance
         live_like_max_try(1) = live_like_max_try_instance
         live_max_try(:,1) = live_max_try_instance
         DO itry=1,ntry-1
            CALL MPI_RECV(nall_try(itry+1), 1, MPI_INT, itry, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_ierror)
            CALL MPI_RECV(evsum_final_try(itry+1), 1, MPI_DOUBLE, itry, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_ierror)
            CALL MPI_RECV(live_like_final_try(:,itry+1), maxstep_try, MPI_DOUBLE, itry, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_ierror)
            CALL MPI_RECV(weight_try(:,itry+1), maxstep_try, MPI_DOUBLE, itry, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_ierror)
            CALL MPI_RECV(live_final_try(:,:,itry+1), maxstep_try*npar, MPI_DOUBLE, itry, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_ierror)
            CALL MPI_RECV(live_like_max_try(itry+1), 1, MPI_DOUBLE, itry, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_ierror)
            CALL MPI_RECV(live_max_try(:,itry+1), npar, MPI_DOUBLE, itry, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_ierror)
         END DO
   ENDIF
#endif

  IF(mpi_rank.EQ.0) THEN
      ! Re-assemble the points ---------------------------------------------------------------
      ! Final number of points
      nall = SUM(nall_try)
      ALLOCATE(weight(nall),live_like_final(nall),live_final(nall,npar),weight_par(nall,2))
      weight = 0.
      live_final = 0.
      live_like_final = 0.
      live_final = 0.
      weight_par = 0.

      ! Final evidence and dispersion
      !evsum_final = SUM(evsum_final_try)/ntry
      IF(ntry.GT.1) THEN
         CALL MEANVAR(evsum_final_try(:ntry),ntry,evsum_final,evsum_err)
         evsum_err = SQRT(evsum_err)

         WRITE(*,*) ' '
         WRITE(*,*) 'Results of the singles try ----------------------------------------------------'
         DO itry=1,ntry
            WRITE(*,*) 'N. try', itry , 'n. steps', nall_try(itry), 'Final evidence', evsum_final_try(itry), &
                  'Max loglikelihood', live_like_max_try(itry)
         END DO
         WRITE(*,*) 'Evidence average:', evsum_final
         WRITE(*,*) 'Evidence standard deviation:', evsum_err
         WRITE(*,*) '-----------------------------------------------------------------------------------'


         OPEN(23,FILE='nf_output_tries.dat',STATUS= 'UNKNOWN')
         WRITE(23,*) 'Number_of_tries ', ntry
         WRITE(23,*) 'Evidence_average:', evsum_final
         WRITE(23,*) 'Evidence_standard_deviation:', evsum_err
         WRITE(23,*) '# N. try   n. steps    Final evidence   Max loglikelihood'
         DO itry=1,ntry
            WRITE(23,*)  itry , nall_try(itry), evsum_final_try(itry), live_like_max_try(itry)
         END DO
         CLOSE(23)

         ! Assemble all points and weights
         DO itry=1,ntry
            !WRITE(*,*) 'Sum from', SUM(nall_try(:itry-1))+1, 'to', SUM(nall_try(:itry))
            live_like_final(SUM(nall_try(:itry-1))+1:SUM(nall_try(:itry))) = live_like_final_try(:nall_try(itry),itry)
            weight(SUM(nall_try(:itry-1))+1:SUM(nall_try(:itry))) = weight_try(:nall_try(itry),itry)*nall_try(itry)/nall
            live_final(SUM(nall_try(:itry-1))+1:SUM(nall_try(:itry)),:) = live_final_try(:nall_try(itry),:,itry)
         END DO
         ! Reorder them (not necessary maybe, but finally yes for plotting)
         !CALL SORTN2(nall,npar,1,live_like_final(:nall),live_final(:nall,:),weight(:nall))


         !Take the maximum of the maximum
         itrymax = MAXLOC(live_like_max_try)
         live_like_max = live_like_max_try(itrymax(1))
         live_max = live_max_try(:,itrymax(1))

      ELSE
         ! Just one try, much more simple!
         evsum_final = evsum_final_try(1)
         evsum_err = 0.

         live_like_final = live_like_final_try(:nall,1)
         weight          = weight_try(:nall,1)
         live_final      = live_final_try(:nall,:,1)
         live_like_max   = live_like_max_try(1)
         live_max        = live_max_try(:,1)
      END IF


      ! ------------Calculate the final parameters, errors and data  --------------------------

      ! Calculate the uncertanity of the evidence calculation
      ! (See J. Veitch and A. Vecchio, Phys. Rev. D 81, 062003 (2010))
      ! TODO(César): There is a better way to do this
      evsum_err_est = DSQRT(DBLE(nall))/(nlive*ntry)

      ! Calculate the mean and the standard deviation for each parameter

      ! Normalize the weights
      weight_tot = 0.
      DO i=1,nall
         weight_tot = weight_tot + weight(i)
      END DO
      weight = weight/weight_tot

      DO j=1,npar
         IF (par_fix(j).NE.1) THEN
            mean_tmp = 0.
            mean2_tmp = 0.

            ! Mean calculation
            DO i=1,nall
               mean_tmp = mean_tmp + live_final(i,j)*weight(i)
            END DO
            par_mean(j) = mean_tmp

            !! Standard deviation calculation
            DO i=1,nall
               mean2_tmp = mean2_tmp + (live_final(i,j)-par_mean(j))**2*weight(i)
            END DO
            par_sd(j) = DSQRT(mean2_tmp)


            ! Median and confidence levels
            ! Order a defined parameter with his weight
            weight_par(:,1) = weight
            weight_par(:,2) = live_final(:,j)
            CALL SORTN(nall,2,weight_par(:,2),weight_par)
            ! Look for confidential levels and median
            weight_int = 0.
            DO i=1,nall-1
               weight_int = weight_int + weight_par(i,1)
               weight_int_next = weight_int + weight_par(i+1,1)
               !write(*,*) j, i, nall, weight_int, weight_int_next, weight_par(i,2)
               ! Low limit 99%
               IF(weight_int.LT.0.005d0.AND.weight_int_next.GT.0.005) THEN
                  par_m99_w(j) = (weight_par(i,2) + weight_par(i+1,2))/2
                  !write(*,*) 'par_m99_w found', j, i, par_m99_w(j)
                  ! Low limit 95%
               ELSE IF(weight_int.LT.0.025d0.AND.weight_int_next.GT.0.025) THEN
                  par_m95_w(j) = (weight_par(i,2) + weight_par(i+1,2))/2
                  !write(*,*) 'par_m95_w found', j, i, par_m95_w(j)
                  ! Low limit 68%
               ELSE IF(weight_int.LT.0.16d0.AND.weight_int_next.GT.0.16) THEN
                  par_m68_w(j) = (weight_par(i,2) + weight_par(i+1,2))/2
                  !write(*,*) 'par_m68_w found', j, i, par_m68_w(j)
                  ! Median
               ELSE IF(weight_int.LT.0.5d0.AND.weight_int_next.GT.0.5) THEN
                  par_median_w(j) = (weight_par(i,2) + weight_par(i+1,2))/2
                  !write(*,*) 'median found', j, i, par_median_w(j)
                  ! High limit 68%
               ELSE IF(weight_int.LT.0.84d0.AND.weight_int_next.GT.0.84) THEN
                  par_p68_w(j) = (weight_par(i,2) + weight_par(i+1,2))/2
                  !write(*,*) 'par_p68_w found', j, i, par_p68_w(j)
                  ! High limit 95%
               ELSE IF(weight_int.LT.0.975d0.AND.weight_int_next.GT.0.975) THEN
                  par_p95_w(j) = (weight_par(i,2) + weight_par(i+1,2))/2
                  !write(*,*) 'par_p95_w found', j, i, par_p95_w(j)
                  ! High limit 99%
               ELSE IF(weight_int.LT.0.995d0.AND.weight_int_next.GT.0.995) THEN
                  par_p99_w(j) = (weight_par(i,2) + weight_par(i+1,2))/2
                  !write(*,*) 'par_p99_w found', j, i, par_p99_w(j)
               END IF
            END DO
         ELSE
            par_mean(j) =  par_in(j)
            par_median_w(j) = par_in(j)
         END IF
      END DO
  ENDIF

   501 CONTINUE

   IF(mpi_rank.EQ.0) THEN
      ! Final actions for the likelihood function
      CALL FINAL_LIKELIHOOD(live_max,par_mean,par_median_w)

      !-----------------Calculate information and bayesian complexity --------------------------
      ! Calculation of the mean loglikelihood
      live_like_mean = 0.
      weight_tot = 0.
      DO i=1,nall
         live_like_mean = live_like_mean + weight(i)*live_like_final(i)
      END DO
      live_like_mean = live_like_mean


      ! Calculation of the information (see Sivia page 186)
      info = live_like_mean - evsum_final
      ! Calculation of the complexity (Trotta 06 + Sivia)
      comp = -2*(live_like_mean - live_like_max)
      ! Calculation of the minimal number of iteration (Veitch and Vecchio 2010)
      ! (Skilling in Sivia suggest EXP(nlive*info) per process)
      ! Test overflow
      IF(DABS(info).LT.700) THEN
         nexp = FLOOR(DEXP(info)*ntry)
      ELSE
         nexp= 0
      END IF

      ! ---------------- Write results on screen and files -------------------------------------
      ! Write files in the format for GetDist
      ! Data
      OPEN(23,FILE='nf_output_points.txt',STATUS= 'UNKNOWN')
      !WRITE(23,*) '# weight   lnlikelihood      parameters'
      DO j=1,nall
         WRITE(23,*) weight(j), live_like_final(j), live_final(j,:)
      END DO
      CLOSE(23)
      ! Names of parameters
      OPEN(23,FILE='nf_output_points.paramnames',STATUS= 'UNKNOWN')
      DO i=1,npar
         WRITE(23,*) par_name(i)
      END DO
      CLOSE(23)
      ! Range of parameters
      OPEN(23,FILE='nf_output_points.ranges',STATUS= 'UNKNOWN')
      DO i=1,npar
         WRITE(23,*) par_name(i), par_bnd1(i), par_bnd2(i)
      END DO
      CLOSE(23)
   ENDIF

   ! Calculate end time
   ! Normal time
   CALL CPU_TIME(stopt)
   seconds  = stopt - startt
   ! Parallel time
   seconds_omp = seconds
   !$ stopt_omp = omp_get_wtime( )
   !$ seconds_omp =  stopt_omp - startt_omp
   
   IF(mpi_rank.EQ.0) THEN
      CALL PRINT_OUTPUT_RESULT()
      DEALLOCATE(weight,live_like_final,live_final,weight_par)
   ENDIF

  ! Deallocate stuff
  DEALLOCATE(par_num,par_name,par_in,par_step,par_bnd1,par_bnd2,par_fix, &
       live_max,par_mean,par_sd, &
       par_median_w, &
       par_m68_w,par_p68_w,par_m95_w,par_p95_w,par_m99_w,par_p99_w)
  ! Dellocate parallel stuff
  IF(mpi_rank.EQ.0) THEN
      DEALLOCATE(live_like_final_try,weight_try,live_final_try,live_max_try, &
            nall_try,evsum_final_try,live_like_max_try)
  ENDIF

#ifdef OPENMPI_ON
  DEALLOCATE(live_final_try_instance)
  DEALLOCATE(live_like_final_try_instance, weight_try_instance, live_max_try_instance)
  CALL MPI_FINALIZE(mpi_ierror)
#endif

#ifdef NORNG_ON
  IF(mpi_rank.EQ.0) THEN
     CALL LOG_WARNING_HEADER()
     CALL LOG_WARNING('This nested_fit output was ran with a set seed! This is intended for testing only!')
     CALL LOG_WARNING('If you are using this as a production setting change the cmake NORNG option to OFF.')
     CALL LOG_WARNING_HEADER()
  ENDIF
#endif

  ! Clean the autofunc module
  CALL CLEAN_AUTOFUNC()

  ! Close the logger file
  CALL CLOSE_LOG()

  ! Shutdown the performance profiling
  CALL SHUTDOWN_PERF_PROF()

  CONTAINS

  SUBROUTINE PRINT_OUTPUT_RESULT()
   PROFILED(PRINT_OUTPUT_RESULT)
   OPEN(22,FILE='nf_output_res.dat',STATUS= 'UNKNOWN')
   WRITE(22,*) '#############_FINAL_RESULTS_#####################################################################################'
   WRITE(22,*) 'N._of_trials:                          ', ntry
   WRITE(22,*) 'N._of_total_iteration:                 ', nall
   WRITE(22,*) 'N._of_used_livepoints:                 ', nlive
   WRITE(22,*) 'Final_evidence_(log):                  ', evsum_final
   WRITE(22,*) 'Evidence_estimated_uncertainty_(log):  ', evsum_err_est
   WRITE(22,*) 'Evidence_standard_deviation_(log):     ', evsum_err
   WRITE(22,*) '------------------------------------------------------------------------------------------------------------------'
   WRITE(22,*) 'Max_likelihood_(log):', live_like_max
   WRITE(22,*) 'Max_parameter_set: '
   DO i=1,npar
      WRITE(22,*) par_name(i), live_max(i)
   END DO
   WRITE(22,*) '-------------------------------------------------------------------------------------------------------------------'
   WRITE(22,*) 'Mean_value_and_standard_deviation_of_the_parameters'
   DO i=1,npar
      WRITE(22,*) par_name(i), par_mean(i), '+/-', par_sd(i)
   END DO
   WRITE(22,*) '-------------------------------------------------------------------------------------------------------------------'
   WRITE(22,*) 'Median_and_confidence_levels: low_99%,     low_95%,     low_68%,     median,     high_68%,    high_95%,   high_99%'
   DO i=1,npar
      WRITE(22,*) par_name(i), par_m99_w(i),par_m95_w(i),par_m68_w(i),par_median_w(i),&
            par_p68_w(i),par_p95_w(i),par_p99_w(i)
   END DO
   WRITE(22,*) '-------------------------------------------------------------------------------------------------------------------'
   WRITE(22,*) 'Additional_information'
   WRITE(22,*) 'Information:                                ', info
   WRITE(22,*) 'Minimal_required_iteration_(ntry*exp[info]):', nexp
   WRITE(22,*) 'Bayesian_complexity:                        ', comp
   WRITE(22,*) '###################################################################################################################'
   WRITE(22,*) 'Number_of_used_cores:                       ', nth
   WRITE(22,*) 'Time_elapsed_(tot_and_real_in_s):           ', seconds, seconds_omp
   CLOSE(22)

   IF(opt_suppress_output) RETURN

   WRITE(*,*) ' '
   WRITE(*,*) ' '
   WRITE(*,*) '############## FINAL RESULTS #####################################################################################'
   WRITE(*,*) 'N. of trials:                         ', ntry
   WRITE(*,*) 'N. of total iteration:                ', nall
   WRITE(*,*) 'N._of_used_livepoints:               ', nlive
   WRITE(*,*) 'Final evidence (log):                 ', evsum_final
   WRITE(*,*) 'Evidence estimated uncertainty (log): ', evsum_err_est
   WRITE(*,*) 'Evidence standard deviation (log):    ', evsum_err

   WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
   WRITE(*,*) 'Max likelihood (log):', live_like_max
   WRITE(*,*) 'Max parameter set : '
   DO i=1,npar
      WRITE(*,*) par_name(i), ':', live_max(i)
   END DO
   WRITE(*,*) '-------------------------------------------------------------------------------------------------------------------'
   WRITE(*,*) 'Mean value and standard deviation of the parameters'
   DO i=1,npar
      WRITE(*,*) par_name(i), ':', par_mean(i), '+/-', par_sd(i)
   END DO
   WRITE(*,*) '--------------------------------------------------------------------------------------------------------------------'
   WRITE(*,*) 'Median and confidence levels: low 99%,     low 95%,     low68%,     median,     high 68%,     high 95%,     high 99%'
   DO i=1,npar
      WRITE(*,*) par_name(i), ':', par_m99_w(i),par_m95_w(i),par_m68_w(i),par_median_w(i),&
            par_p68_w(i),par_p95_w(i),par_p99_w(i)
   END DO
   WRITE(*,*) '-------------------------------------------------------------------------------------------------------------------'
   WRITE(*,*) 'Additional information'
   WRITE(*,*) 'Information:                                ', info
   WRITE(*,*) 'Minimal required iteration (ntry*exp[info]):', nexp
   WRITE(*,*) 'Bayesian complexity:                        ', comp
   WRITE(*,*) '####################################################################################################################'
   WRITE(*,*) 'Number of used cores:                       ', nth
   WRITE(*,*) 'Time elapsed (tot and real in s):           ', seconds, seconds_omp
  END SUBROUTINE

  SUBROUTINE FIND_TOTAL_PARAMS_USERFUNCTION(parse_results, parameters, vsize)
    TYPE(ParseLatex_t), INTENT(IN), DIMENSION(:) :: parse_results
    CHARACTER(64), INTENT(OUT), DIMENSION(:)     :: parameters
    INTEGER, INTENT(OUT)                         :: vsize
    INTEGER                                      :: i, j, k

    PROFILED(FIND_TOTAL_PARAMS_USERFUNCTION)

    IF(SIZE(parse_results).EQ.1) THEN
      ! Avoid calling expensive STR_ARRAY_UNIQUE
      DO i = 1, SIZE(parse_results(1)%parameter_names)
        parameters(i) = TRIM(parse_results(1)%parameter_names(i))
      END DO
      vsize = SIZE(parse_results(1)%parameter_names)
    ELSE
      k = 0
      DO i = 1, SIZE(parse_results)
        DO j = 1, SIZE(parse_results(i)%parameter_names)
          k = k + 1
          parameters(k) = TRIM(parse_results(i)%parameter_names(j))
        END DO
      END DO
    
      CALL STR_ARRAY_UNIQUE(parameters, k)
      vsize = k

      CALL LOG_TRACE('Running parameter detection for `set` mode...')
      DO i = 1, vsize
        CALL LOG_TRACE('Userfunction found param: '//TRIM(parameters(i)))
      END DO

    ENDIF
  END SUBROUTINE

  SUBROUTINE CONFIGURE_USERFUNCTION()

   CHARACTER(512)                 :: definition, key
   TYPE(ParseLatex_t)             :: parse_result(nsetmax)
   CHARACTER(64)                  :: parameters(64*nsetmax) ! 64 params per set function
   INTEGER                        :: numparams
   INTEGER                        :: i
   LOGICAL                        :: fix_logical = .FALSE.
   CHARACTER(128)                 :: legacy_param_keys(64)
   CHARACTER(128)                 :: legacy_param_names(64)
   INTEGER                        :: legacy_param_count
   CHARACTER(128)                 :: splitarr(16)
   INTEGER                        :: splitarr_count

   PROFILED(CONFIGURE_USERFUNCTION)

   ! Check if the function is legacy or not
   ! If the first function is legacy, we don't need to worry about this
   IF(.NOT.IS_LEGACY_USERFCN(TRIM(funcname(1)))) THEN
      ! Try to extract an expression
      DO i = 1, nset
        parse_result(i) = PARSE_LATEX(TRIM(funcname(i)))
        
        ! Stop execution if parsing failed
        IF(parse_result(i)%error.NE.0) CALL HALT_EXECUTION()

        definition = TRIM(funcname(i)(INDEX(funcname(i), '=')+1:LEN_TRIM(funcname(i))))
        
        ! All fine, so just compile the function
        CALL COMPILE_CACHE_FUNC(parse_result(i), definition)
      END DO
      
      ! Now figure out the required parameters (relatively complex for a set)
      CALL FIND_TOTAL_PARAMS_USERFUNCTION(parse_result(1:nset), parameters, numparams)
      npar = numparams

      ! Deallocate all of the parsed data
      DO i = 1, nset
        CALL PARSE_LATEX_DEALLOC(parse_result(i))
      END DO

      ! Allocate space for parameters and initialize
      ALLOCATE(live_max(npar),par_num(npar),par_name(npar),par_in(npar),par_step(npar), &
        par_bnd1(npar),par_bnd2(npar),par_fix(npar), &
        par_mean(npar),par_sd(npar), &
        par_median_w(npar), &
        par_m68_w(npar),par_p68_w(npar),par_m95_w(npar),par_p95_w(npar),par_m99_w(npar),par_p99_w(npar))
      live_max = 0.
      par_num = 0
      par_name = ' '
      par_in = 0.
      par_step = 0.
      par_bnd1 = 0.
      par_bnd2 = 0.
      par_fix = 0
      par_mean = 0.
      par_sd = 0.
      par_median_w = 0.
      par_m68_w = 0.
      par_p68_w = 0.
      par_m95_w = 0.
      par_p95_w = 0.
      par_m99_w = 0.
      par_p99_w = 0.

      ! Handle the arguments from the input file
      DO i=1, npar
        key         = 'function.params.'//TRIM(parameters(i))//'.'
        par_num(i)  = i
        par_name(i) = TRIM(parameters(i))
        CALL LOG_TRACE('Reading key: '//key)
        CALL FIELD_FROM_INPUT_REAL   (input_config, TRIM(key)//'value', par_in(i)  , MANDATORY=.TRUE. )
        CALL FIELD_FROM_INPUT_REAL   (input_config, TRIM(key)//'step' , par_step(i), MANDATORY=.FALSE.) !    -1 by default
        CALL FIELD_FROM_INPUT_REAL   (input_config, TRIM(key)//'min'  , par_bnd1(i), MANDATORY=.TRUE. )
        CALL FIELD_FROM_INPUT_REAL   (input_config, TRIM(key)//'max'  , par_bnd2(i), MANDATORY=.TRUE. )
        CALL FIELD_FROM_INPUT_LOGICAL(input_config, TRIM(key)//'fixed', fix_logical, MANDATORY=.FALSE.) ! False by default
        par_fix(i) = fix_logical ! Implicit conversion

        IF (par_bnd1(i).GE.par_bnd2(i)) THEN
          CALL LOG_ERROR_HEADER()
          CALL LOG_ERROR('Bad limits for parameter `'//TRIM(parameters(i))//'`.')
          CALL LOG_ERROR('min >= max.')
          CALL LOG_ERROR('Aborting Execution...')
          CALL LOG_ERROR_HEADER()
          CALL HALT_EXECUTION()
        END IF
      END DO
   ELSE
      ! Use the function as is (legacy mode)
      ! Nothing to be done here (for now)
      ! Get legacy required parameters
      CALL input_config%subkeys_of('function.params.', legacy_param_keys, legacy_param_count)
      DO i = 1, legacy_param_count
         legacy_param_keys(i) = legacy_param_keys(i)(1:INDEX(legacy_param_keys(i), '.', back=.TRUE.)-1)
      END DO
      CALL STR_ARRAY_UNIQUE(legacy_param_keys, legacy_param_count)
      
      IF(legacy_param_count.EQ.0) THEN
         CALL LOG_ERROR_HEADER()
         CALL LOG_ERROR('Function legacy mode detected.')
         CALL LOG_ERROR('But no parameters found.')
         CALL LOG_ERROR('Aborting Execution...')
         CALL LOG_ERROR_HEADER()
         CALL HALT_EXECUTION()
      ENDIF

      npar = legacy_param_count

      CALL LOG_TRACE('Legacy parameters found:')
      DO i = 1, npar
         CALL LOG_TRACE(TRIM(legacy_param_keys(i)))
      END DO

      ! Allocate space for parameters and initialize
      ALLOCATE(live_max(npar),par_num(npar),par_name(npar),par_in(npar),par_step(npar), &
         par_bnd1(npar),par_bnd2(npar),par_fix(npar), &
         par_mean(npar),par_sd(npar), &
         par_median_w(npar), &
         par_m68_w(npar),par_p68_w(npar),par_m95_w(npar),par_p95_w(npar),par_m99_w(npar),par_p99_w(npar))
      live_max = 0.
      par_num = 0
      par_name = ' '
      par_in = 0.
      par_step = 0.
      par_bnd1 = 0.
      par_bnd2 = 0.
      par_fix = 0
      par_mean = 0.
      par_sd = 0.
      par_median_w = 0.
      par_m68_w = 0.
      par_p68_w = 0.
      par_m95_w = 0.
      par_p95_w = 0.
      par_m99_w = 0.
      par_p99_w = 0.

      ! Handle the arguments from the input file
      DO i=1, npar
         CALL SPLIT_INPUT_ON('.', legacy_param_keys(i), splitarr, splitarr_count, 16)
         key         = TRIM(legacy_param_keys(i))//'.'
         par_name(i) = TRIM(splitarr(splitarr_count))
         CALL LOG_TRACE('Reading key: '//key)
         CALL FIELD_FROM_INPUT_INTEGER(input_config, TRIM(key)//'npar' , par_num(i)          , MANDATORY=.TRUE. )
         CALL FIELD_FROM_INPUT_REAL   (input_config, TRIM(key)//'value', par_in(par_num(i))  , MANDATORY=.TRUE. )
         CALL FIELD_FROM_INPUT_REAL   (input_config, TRIM(key)//'step' , par_step(par_num(i)), MANDATORY=.FALSE.) !    -1 by default
         CALL FIELD_FROM_INPUT_REAL   (input_config, TRIM(key)//'min'  , par_bnd1(par_num(i)), MANDATORY=.TRUE. )
         CALL FIELD_FROM_INPUT_REAL   (input_config, TRIM(key)//'max'  , par_bnd2(par_num(i)), MANDATORY=.TRUE. )
         CALL FIELD_FROM_INPUT_LOGICAL(input_config, TRIM(key)//'fixed', fix_logical         , MANDATORY=.FALSE.) ! False by default
         par_fix(par_num(i)) = fix_logical ! Implicit conversion

         IF (par_bnd1(par_num(i)).GE.par_bnd2(par_num(i))) THEN
            CALL LOG_ERROR_HEADER()
            CALL LOG_ERROR('Bad limits for parameter `'//TRIM(splitarr(splitarr_count))//'`.')
            CALL LOG_ERROR('min >= max.')
            CALL LOG_ERROR('Aborting Execution...')
            CALL LOG_ERROR_HEADER()
            CALL HALT_EXECUTION()
         END IF
      END DO

   ENDIF

  END SUBROUTINE

  SUBROUTINE POPULATE_FILEFMT(format)
   IMPLICIT NONE
   CHARACTER(LEN=16), INTENT(INOUT) :: format

   CALL STR_TO_LOWER(format)
   IF((TRIM(format).NE.'.csv').AND.(TRIM(format).NE.'.tsv')) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Unrecognized filefmt: `'//TRIM(format)//'`.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF

  END SUBROUTINE

  SUBROUTINE POPULATE_INPUTFILES(filestr)
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: filestr
   INTEGER                      :: count

   CALL SPLIT_INPUT_ON(',', filestr, filename, count, nsetmax)

   IF(count.GT.1) THEN
      ! We have a set of files
      CALL LOG_TRACE('Running for a set of files.')

      is_set = .TRUE.
      nset = count
      DO i = 1, count
         filename(i) = ADJUSTL(filename(i)) ! Skip possible left spaces from comma separation
      END DO
   ELSE
      nset = 1
      is_set = .FALSE.
   ENDIF

  END SUBROUTINE


  SUBROUTINE POPULATE_DATATYPE(specstr, output)
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN)  :: specstr
   CHARACTER(3)    , INTENT(OUT) :: output
   CHARACTER(64)                 :: specifiers(specstrmaxcol)
   CHARACTER                     :: dimensions
   INTEGER                       :: count
   INTEGER                       :: i, j
   INTEGER                       :: order_int
   LOGICAL                       :: found_spec, spec_err

! Ignore populating the data for the nested_fit_func target
#ifndef FUNC_TARGET
   ! Declare the valid spec string fields
   CHARACTER(32), PARAMETER :: spec_fields(7) = [CHARACTER(LEN=32) ::&
         'x',&
         'y',&
         ! 'xe',&
         ! 'ye',&
         'c',&
         'ce',&
         't',&
         'i',&
         'img'&
      ]

   CALL SPLIT_INPUT_ON(',', specstr, specifiers, count, specstrmaxcol)
   
   IF(count.LT.1) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Specified `specstr` is not valid.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF

   CALL specstr_ordermap%insert('ncols', count)

   ! NOTE(César): Special case for image data (legacy 2D)
   IF(TRIM(spec_str).EQ.'img') THEN
      CALL specstr_ordermap%insert('img_v', 1)
      output = '2c'
      RETURN
   ELSE IF(INDEX(spec_str, 'img').GT.0) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Specified `specstr` is not valid.')
      CALL LOG_ERROR('Spec name `img` is an exclusive spec. But other specs were found.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF
   
   dimensions = ' '
   found_spec = .FALSE.
   DO i = 1, count
      ! Try and find a spec
      DO j = 1, 7
         IF(TRIM(specifiers(i)).EQ.TRIM(spec_fields(j))) THEN
            CALL LOG_TRACE('specstr: Found valid specifier `'//TRIM(spec_fields(j))//'`.')
            ! HACK(César): This is required for the hash to work with some keys (why ???)
            CALL specstr_ordermap%insert(TRIM(spec_fields(j))//'_v', i)
            found_spec = .TRUE.
            GOTO 919
         ENDIF
      END DO

      919   CONTINUE
      IF(.NOT.found_spec) THEN
         CALL LOG_ERROR_HEADER()
         CALL LOG_ERROR('Specified `specstr` is not valid.')
         CALL LOG_ERROR('Spec name `'//TRIM(specifiers(i))//'` is not a valid name.')
         CALL LOG_ERROR('Aborting Execution...')
         CALL LOG_ERROR_HEADER()
         CALL HALT_EXECUTION()
      ENDIF
      found_spec = .FALSE.
   END DO

   CALL specstr_ordermap%find('x_v', order_int, spec_err)
   IF(spec_err) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Specified `specstr` is valid.')
      CALL LOG_ERROR('But spec `x` is required and was not found.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF
   dimensions = '1'

   CALL specstr_ordermap%find('y_v', order_int, spec_err)
   IF(.NOT.spec_err) THEN
      dimensions = '2'
   ENDIF

   CALL specstr_ordermap%find('c_v', order_int, spec_err)
   IF(spec_err) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Specified `specstr` is valid.')
      CALL LOG_ERROR('But spec `c` is required and was not found.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF

   output(1:1) = dimensions(1:1)

   CALL specstr_ordermap%find('ce_v', order_int, spec_err)
   IF(.NOT.spec_err) THEN
      output(2:2) = 'e'
   ELSE
      output(2:2) = 'c'
   ENDIF
#endif
  END SUBROUTINE

  SUBROUTINE FIELD_FROM_INPUT_REAL(config, field_name, field_value, mandatory)
   TYPE(InputDataMap_t), INTENT(INOUT) :: config
   CHARACTER(*)        , INTENT(IN)    :: field_name
   REAL(8)             , INTENT(OUT)   :: field_value
   LOGICAL             , INTENT(IN)    :: mandatory
   
   TYPE(InputDataGenericValue_t) :: generic_val
   LOGICAL                       :: error

   CALL config%find(field_name, generic_val, error)
   IF(error.AND.mandatory) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Input config for parameter `'//TRIM(field_name)//'` was not found and is mandatory.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      STOP
   ELSEIF(error) THEN
      RETURN ! Just ignore the value without warning or errors (for now)
   ENDIF
   field_value = generic_val%toReal()
  END SUBROUTINE

  SUBROUTINE FIELD_FROM_INPUT_INTEGER(config, field_name, field_value, mandatory)
   TYPE(InputDataMap_t), INTENT(INOUT) :: config
   CHARACTER(*)        , INTENT(IN)    :: field_name
   INTEGER             , INTENT(OUT)   :: field_value
   LOGICAL             , INTENT(IN)    :: mandatory
   
   TYPE(InputDataGenericValue_t) :: generic_val
   LOGICAL                       :: error

   CALL config%find(field_name, generic_val, error)
   IF(error.AND.mandatory) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Input config for parameter `'//TRIM(field_name)//'` was not found and is mandatory.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      STOP
   ELSEIF(error) THEN
      RETURN ! Just ignore the value without warning or errors (for now)
   ENDIF
   field_value = generic_val%toInteger()
  END SUBROUTINE

  SUBROUTINE FIELD_FROM_INPUT_CHARACTER(config, field_name, field_value, mandatory)
   TYPE(InputDataMap_t), INTENT(INOUT) :: config
   CHARACTER(*)        , INTENT(IN)    :: field_name
   CHARACTER(*)        , INTENT(OUT)   :: field_value
   LOGICAL             , INTENT(IN)    :: mandatory
   
   TYPE(InputDataGenericValue_t) :: generic_val
   LOGICAL                       :: error

   CALL config%find(field_name, generic_val, error)
   IF(error.AND.mandatory) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Input config for parameter `'//TRIM(field_name)//'` was not found and is mandatory.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      STOP
   ELSEIF(error) THEN
      RETURN ! Just ignore the value without warning or errors (for now)
   ENDIF
   field_value = generic_val%toCharacter()
  END SUBROUTINE

  SUBROUTINE FIELD_FROM_INPUT_LOGICAL(config, field_name, field_value, mandatory)
   TYPE(InputDataMap_t), INTENT(INOUT) :: config
   CHARACTER(*)        , INTENT(IN)    :: field_name
   LOGICAL             , INTENT(OUT)   :: field_value
   LOGICAL             , INTENT(IN)    :: mandatory
   
   TYPE(InputDataGenericValue_t) :: generic_val
   LOGICAL                       :: error

   CALL config%find(field_name, generic_val, error)
   IF(error.AND.mandatory) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Input config for parameter `'//TRIM(field_name)//'` was not found and is mandatory.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      STOP
   ELSEIF(error) THEN
      RETURN ! Just ignore the value without warning or errors (for now)
   ENDIF
   field_value = generic_val%toLogical()
  END SUBROUTINE

  SUBROUTINE B_COMPACT(this, invalue)
   CLASS(argdef_t), INTENT(IN) :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue
   opt_compact_output = .TRUE.
  END SUBROUTINE

  SUBROUTINE B_INPUTFILE(this, invalue)
   CLASS(argdef_t), INTENT(IN) :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue
   opt_input_file = TRIM(invalue)
  END SUBROUTINE

  SUBROUTINE DEL_FOLDER_RECURSIVE(where)
   CHARACTER(LEN=*), INTENT(IN) :: where

#ifdef _WIN32
   CALL EXECUTE_COMMAND_LINE('del /s /q '//TRIM(where)//'*') ! TODO(César): Check for potential errors, and test it on windows
#else
   CALL EXECUTE_COMMAND_LINE('rm -rf '//TRIM(where)//'*')    ! TODO(César): Check for potential errors
#endif
  END SUBROUTINE

  SUBROUTINE B_DELCACHE(this, invalue)
   CLASS(argdef_t), INTENT(IN) :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue
   CHARACTER(LEN=6) :: delete_ok

   PROFILED(B_DELCACHE)

   CALL LOG_MESSAGE_HEADER()
   CALL LOG_MESSAGE('This action is irreversible. All of the user functions will be lost.')
   CALL LOG_MESSAGE('Type `Delete` to delete the cache?')
   CALL LOG_MESSAGE_HEADER()
   WRITE(*, '(a2)', advance='no') '> '
   READ(*, '(a6)') delete_ok

   IF(delete_ok.EQ.'Delete') THEN
      CALL LOG_MESSAGE_HEADER()
      CALL LOG_MESSAGE('Cache deleted.')
      CALL LOG_MESSAGE_HEADER()
      CALL DEL_FOLDER_RECURSIVE(nf_cache_folder)
   ELSE
      CALL LOG_MESSAGE_HEADER()
      CALL LOG_MESSAGE('Operation aborted by the user.')
      CALL LOG_MESSAGE_HEADER()
   ENDIF
   STOP
  END SUBROUTINE

  
  SUBROUTINE B_ADDUSRFUNC(this, invalue)
   CLASS(argdef_t), INTENT(IN)    :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue
   CHARACTER(LEN=512)             :: definition
   TYPE(ParseLatex_t)             :: parse_result
   INTEGER                        :: last_char
   
   PROFILED(B_ADDUSRFUNC)

   last_char = LEN_TRIM(invalue)
   IF(last_char.LT.5) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Failed to add user function.')
      CALL LOG_ERROR('Must provide a LaTeX expression or a f90/c++ file.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF

   ! NOTE: (César): Checking for an extension could work if the string ends in .ccc
   !                Where this 'ccc' could only be chars or it could be confused
   !                with a number end in latex (e.g. x + 1.342)
   !                So checking for an extension is not that trivial
   !                If the extension does not match a cpp or f90 one just assume we have latex code
   
   ! Detect if the input is latex or a file
   IF(HAS_VALID_CPP_EXT(TRIM(invalue)).OR.HAS_VALID_F90_EXT(TRIM(invalue))) THEN
      CALL COMPILE_CACHE_FUNC_NATIVE(TRIM(invalue))
      CALL HALT_EXECUTION()
   ELSE
      parse_result = PARSE_LATEX(TRIM(invalue))

      IF(parse_result%error.EQ.0) THEN
         definition = TRIM(invalue(INDEX(invalue, '=')+1:LEN_TRIM(invalue)))
         CALL COMPILE_CACHE_FUNC(parse_result, definition)
      ENDIF

      CALL PARSE_LATEX_DEALLOC(parse_result)
      CALL HALT_EXECUTION()
   ENDIF
  END SUBROUTINE

  SUBROUTINE B_RUNUSRFUNC(this, invalue)
   CLASS(argdef_t), INTENT(IN)    :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue
   CHARACTER(128)                 :: function_name
   CHARACTER(128)                 :: parameters
   CHARACTER(128)                 :: tmp, tmp2
   PROCEDURE(proc_ptr_t), POINTER :: fptr
   LOGICAL                        :: loaded_ok


   REAL(8)              :: x, y
   INTEGER              :: nargs
   REAL(8), ALLOCATABLE :: args(:)
   INTEGER              :: i, i0, i1
    
   PROFILED(B_RUNUSRFUNC)

   ! Find the function name
   function_name = TRIM(ADJUSTL(invalue(1:INDEX(invalue, '(')-1)))
   
   ! Find the function parameters
   parameters    = TRIM(ADJUSTL(invalue(INDEX(invalue, '(')+1:LEN_TRIM(invalue)-1)))

   ! Find x
   i0 = INDEX(parameters, ',')
   tmp           = TRIM(ADJUSTL(parameters(1:i0)))
   READ(tmp, *) x

   ! Find nargs
   i1 = INDEX(parameters(i0+1:LEN_TRIM(parameters)), ',') + i0
   tmp           = TRIM(ADJUSTL(parameters(i0+1:i1-1)))
   READ(tmp, *) nargs

   IF(nargs.GT.0) THEN
      ALLOCATE(args(nargs))
      ! Find the remaining arguments
      tmp = parameters(INDEX(parameters, '[')+1:LEN_TRIM(parameters)-1)
      DO i = 1, nargs-1
         tmp2 = tmp(1:INDEX(tmp, ',')-1)
         READ(tmp2, *) args(i)
         tmp = tmp(INDEX(tmp, ',')+1:LEN_TRIM(tmp))
      END DO
   
      READ(tmp, *) args(nargs)
   ENDIF


   CALL GET_USER_FUNC_PROCPTR(function_name, fptr, loaded_ok)

   IF(.NOT.loaded_ok) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Failed to load proc address.')
      CALL LOG_ERROR('Maybe the specified function name is incorrect/not in the cache.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF

   y = fptr(x, nargs, args)

   WRITE(*,*) TRIM(ADJUSTL(function_name)), '(', TRIM(parameters), ') =', y

   IF(nargs.GT.0) THEN
      DEALLOCATE(args)
   ENDIF
   CALL HALT_EXECUTION()
  END SUBROUTINE

  SUBROUTINE WRITE_REPEAT_CHAR(val, n)
   CHARACTER(LEN=1), INTENT(IN) :: val
   INTEGER, INTENT(IN)          :: n
   INTEGER                      :: i

   DO i = 1, n
      WRITE(*, '(a)', advance='no') val
   END DO
  END SUBROUTINE

  SUBROUTINE B_LSTUSRFUNC(this, invalue)
   CLASS(argdef_t), INTENT(IN)    :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue
   INTEGER                        :: i, n, max_dec_s, s
   TYPE(cache_entry_t)            :: cache_entry
   CHARACTER(LEN=16)              :: tmp

   PROFILED(B_LSTUSRFUNC)

   CALL GET_CACHE_SIZE(n)

   IF(n.EQ.0) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('No cache functions found.')
      CALL LOG_ERROR('Use the `-fa` option to add a function to the cache.')
      CALL LOG_ERROR_HEADER()
      STOP
   ENDIF

   ! Find out what is the largest declaration so we adjust the table accordingly
   ! TODO(César) : Linebreak in case of huge declarations
   max_dec_s = 21 ! This is the size of `Function Declaration` header
   DO i = 1, n
      cache_entry = GET_CACHE(i)
      max_dec_s   = MAX(LEN_TRIM(cache_entry%dec), max_dec_s)
   END DO
   WRITE(tmp,'(I0)') max_dec_s

   ! How many more table steps do we need
   s = MAX(max_dec_s - 21, 0)

   ! Basically, just display the cache now
   WRITE(*,'(a)', advance='no')      ' --------------------------------------------------------------------'
   CALL WRITE_REPEAT_CHAR('-', s)
   WRITE(*, '(a)', advance='no') NEW_LINE('A')
   WRITE(*,'(a)', advance='no')     '| Name           | Argc | Last Modified       | Function Declaration '
   CALL WRITE_REPEAT_CHAR(' ', s)
   WRITE(*, '(a)') '|'
   WRITE(*,'(a)', advance='no')      ' ---------------- ------ --------------------- ----------------------'
   CALL WRITE_REPEAT_CHAR('-', s)
   WRITE(*, '(a)', advance='no') NEW_LINE('A')
   DO i = 1, n
      cache_entry = GET_CACHE(i)
      WRITE(*,'(a2, a15, a1, I2, a6, a20, a2, a'//TRIM(tmp)//', a1)') '| ', &
      cache_entry%name, '|', &
      cache_entry%argc, '    | ', &
      ADJUSTL(cache_entry%date_modified), '| ', &
      ADJUSTL(cache_entry%dec), '|'
   END DO
   WRITE(*,'(a)', advance='no')      ' --------------------------------------------------------------------'
   CALL WRITE_REPEAT_CHAR('-', s)
   WRITE(*, '(a)', advance='no') NEW_LINE('A')


   STOP
  END SUBROUTINE

  SUBROUTINE B_RECUSRFUNC(this, invalue)
   CLASS(argdef_t), INTENT(IN)    :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue

   PROFILED(B_RECUSRFUNC)

   CALL LOG_MESSAGE_HEADER()
   CALL LOG_MESSAGE('Recompiling cache...')
   CALL LOG_MESSAGE_HEADER()

   CALL RECOMPILE_CACHE()

   STOP
  END SUBROUTINE

  SUBROUTINE B_SETVERBOSITY(this, invalue)
   CLASS(argdef_t), INTENT(IN)    :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue

   CALL LOG_VERBOSITY(invalue)
  END SUBROUTINE

  SUBROUTINE B_SUPPRESSOUTPUT(this, invalue)
   CLASS(argdef_t), INTENT(IN)    :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue

   CALL LOG_VERBOSITY('none')
   opt_suppress_output = .TRUE.
   CALL DISABLE_STDOUT()
  END SUBROUTINE

  SUBROUTINE B_CPPCMPCMD(this, invalue)
   CLASS(argdef_t), INTENT(IN)    :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue
   
   opt_cpp_comp_cmd = TRIM(invalue)
   
  END SUBROUTINE

  SUBROUTINE B_F90CMPCMD(this, invalue)
   CLASS(argdef_t), INTENT(IN)    :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue
   
   opt_f90_comp_cmd = TRIM(invalue)
   
  END SUBROUTINE

  SUBROUTINE B_LNKCMD(this, invalue)
   CLASS(argdef_t), INTENT(IN)    :: this
   CHARACTER(LEN=512), INTENT(IN) :: invalue
   
   opt_lnk_cmd = TRIM(invalue)
   
  END SUBROUTINE

END PROGRAM NESTED_FIT
