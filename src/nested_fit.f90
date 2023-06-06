PROGRAM NESTED_FIT
  ! Time-stamp: <Last changed by martino on Monday 07 June 2021 at CEST 10:29:22>
  !
  ! Please read README and LICENSE files for more inforamtion
  ! 4.4  OpenMPI support (only available for number of tries)
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


   ! Module for CLI lib
   USE argparse
   ! Module for optional variables
   USE MOD_OPTIONS
   ! Module for the input parameter definition
   USE MOD_PARAMETERS
   ! Module for likelihood for data analysis
   USE MOD_LIKELIHOOD
   ! Module for metadata
   USE MOD_METADATA
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
  CHARACTER :: string*128
  CHARACTER :: version_file*20

  ! Results from Nested sampling
  INTEGER(4) :: nall=0
  REAL(8) :: evsum_final=0., live_like_max=0.
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_final
  REAL(8), ALLOCATABLE, DIMENSION(:) :: weight, live_like_final, live_max

  ! Final results
  REAL(8), ALLOCATABLE, DIMENSION(:) :: par_mean, par_sd
  REAL(8), ALLOCATABLE, DIMENSION(:) :: par_median_w, par_m68_w, par_p68_w, par_m95_w
  REAL(8), ALLOCATABLE, DIMENSION(:) :: par_p95_w, par_m99_w, par_p99_w
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: weight_par
  REAL(8) :: mean_tmp=0., mean2_tmp=0., weight_tot=0., weight_int=0., weight_int_next=0.
  REAL(8) :: evsum_err=0.
  REAL(8) :: evsum_err_est=0.
  REAL(8) :: live_like_mean=0., info=0., comp=0.
  INTEGER(8) :: nexp=0

  ! Parallelization variables for master mpi node
  INTEGER(4) :: itry=1
  INTEGER(4), DIMENSION(1) :: itrymax
  INTEGER(4), ALLOCATABLE, DIMENSION(:) :: nall_try
  REAL(8), ALLOCATABLE, DIMENSION(:) :: evsum_final_try, live_like_max_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: live_final_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_like_final_try, weight_try, live_max_try

  ! Parallelization variables for each mpi instance
  INTEGER(4) :: nall_try_instance
  REAL(8) :: evsum_final_try_instance, live_like_max_try_instance
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_final_try_instance
  REAL(8), ALLOCATABLE, DIMENSION(:) :: live_like_final_try_instance, weight_try_instance, live_max_try_instance

  ! OpenMPI stuff
  INTEGER(4) :: mpi_rank, mpi_cluster_size, mpi_ierror

  ! Time measurement variables
  REAL(8) :: seconds, seconds_omp, startt, stopt, startt_omp, stopt_omp

  ! Random number variables
  INTEGER(4) :: seed_array(33) = 1

  ! Function definitions
  EXTERNAL :: NESTED_SAMPLING, SORTN, MEANVAR
  
  TYPE(argdef_t) :: argdefs(2)
  TYPE(argval_t) :: argval
  argdefs(1) = argdef_t("help", "h", .FALSE.)
  argdefs(2) = argdef_t("compact-output", "c", .FALSE.)
  
  DO
      argval = get_next_arg(argdefs)
      IF(argval%valid) THEN
         SELECT CASE (argval%arg%short_name)
            CASE("c")
               opt_compact_output = .TRUE.
            CASE("h")
               ! TODO(César): Print a help screen for optional arguments
               WRITE(*,*) '-----------------------------------------------------------------------'
               WRITE(*,*) '                                                                       '
               WRITE(*,*) '                                                                       '
               WRITE(*,*) '                                                                       '
               WRITE(*,*) '                                                                       '
               WRITE(*,*) '                                                                       '
               WRITE(*,*) '                                                                       '
               WRITE(*,*) '-----------------------------------------------------------------------'
               ! Note(Cesar): This is before initializing mpi (if we have it on) so we should be good
               STOP
         END SELECT
      ELSE
         IF(argval%value.NE.CHAR(255)) THEN
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*) '       ERROR:           Nested_fit argument parsing failed!'
            WRITE(*,*) '       ERROR:           Aborting execution.'
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            STOP ! Error parsing cli inputs
         ENDIF
         EXIT
      ENDIF
  END DO

#ifdef OPENMPI_ON
    CALL MPI_INIT(mpi_ierror)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_cluster_size, mpi_ierror)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, mpi_rank, mpi_ierror)
#else
    mpi_rank = 0
#endif

  !!!!!!!! Initiate random generator with the same seed each time !!!!!!!!!!!
  IF(static_seed.AND.mpi_rank.EQ.0) THEN
      WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
      WRITE(*,*) '       ATTENTION:           Nested_fit is running with a set seed! This is intended for testing only!'
      WRITE(*,*) '       ATTENTION:           If you are using this as a production setting change the cmake NORNG option to OFF.'
      WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
      CALL sleep(1)
  ENDIF

  IF(static_seed) THEN
      CALL RANDOM_SEED(PUT=seed_array)
  ENDIF

#ifdef OPENMPI_ON
   CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_ierror)
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Calculate time elapsed !!!!!!!!!!!!!!!!!!!!
  ! Parallel real time (and number of threads)  
  !$ seconds = omp_get_wtime( )
  !$ nth = omp_get_max_threads()

  ! Absolute time
  CALL CPU_TIME(startt)
  !$ startt_omp = omp_get_wtime( )

  ! Print program version
  IF(mpi_rank.EQ.0) THEN
      WRITE(*,*) 'Current program version = ', version

      ! Initialize values --------------------------------------------------------------------------------------------------------------
      filename = ' '
      funcname = ' '
      likelihood_funcname = ' '

      ! Read parameter file ------------------------------------------------------------------------------------------------------------
      OPEN (UNIT=77, FILE='nf_input.dat', STATUS='old')
      READ(77,*) version_file, string
      IF(version.NE.version_file) THEN
         WRITE(*,*) 'Program version not corresponding to the input type'
         WRITE(*,*) 'Please change your input.dat'
#ifdef OPENMPI_ON
         CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_ierror)
#endif
         STOP
      END IF
      READ(77,*) filename(1), string
      READ(77,*) set_yn, string
      READ(77,*) data_type, string
      READ(77,*) likelihood_funcname, string
      READ(77,*) nlive, string
      READ(77,*) conv_method, string
      READ(77,*) evaccuracy, conv_par, string
      READ(77,*) search_method, string
      READ(77,*) search_par1, search_par2, maxtries, maxntries, string
      READ(77,*) cluster_yn, cluster_method, cluster_par1, cluster_par2, string
      READ(77,*) ntry, maxstep_try, string
      READ(77,*) funcname, string
      READ(77,*) lr, string
      READ(77,*) npoint, nwidth, string
      READ(77,*) xmin(1), xmax(1), ymin(1), ymax(1), string
      READ(77,*) npar, string
      READ(77,*) string

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

      DO i = 1, npar
         READ(77,*,ERR=10,END=10) par_num(i),par_name(i),par_in(i),par_step(i),par_bnd1(i),par_bnd2(i),par_fix(i)
         IF (par_bnd1(i).GE.par_bnd2(i)) THEN
            WRITE(*,*) 'Bad limits in parameter n.', i, ' (low bound1 >= high bound!!!) Change it and restart'
            WRITE(*,*) 'Low bound:',par_bnd1(i), 'High bound:', par_bnd2(i)
#ifdef OPENMPI_ON
            CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_ierror)
#endif
            STOP
         END IF
      END DO
      10 CONTINUE
      CLOSE(77)

      ! Read set of spectra file parameter
      IF (set_yn.EQ.'y'.OR.set_yn.EQ.'Y') THEN
         OPEN (UNIT=88, FILE='nf_input_set.dat', STATUS='old')
         READ(88,*,ERR=11,END=11) nset
         DO k = 2, nset
            READ(88,*,ERR=11,END=11) xmin(k), xmax(k), string
         END DO
         DO k = 2, nset
            READ(88,*,ERR=11,END=11) filename(k)
         END DO
      11   CONTINUE
         CLOSE(88)
      ENDIF
  ENDIF

  ! Receive data from the mpi root node
  ! All the code should be refactored really but for now
  ! TODO(César): Refactor this to a function
#ifdef OPENMPI_ON
      CALL MPI_Bcast(filename(1), 64, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(set_yn, 1, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
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
      CALL MPI_Bcast(cluster_yn, 1, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(cluster_method, 1, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(cluster_par1, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(cluster_par2, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(ntry, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(maxstep_try, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierror)
      CALL MPI_Bcast(funcname, 64, MPI_CHARACTER, 0, MPI_COMM_WORLD, mpi_ierror)
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
      WRITE(*,*) 'Parameter file read'
      WRITE(*,*) 'Filenames = ', filename(1:nset)
      WRITE(*,*) 'Funcname = ', funcname

      ! Not implemented combination of inputs
      IF (data_type(1:1).EQ.'2'.AND.set_yn.EQ.'y') THEN
         WRITE(*,*)
         WRITE(*,*) 'ERROR'
         WRITE(*,*) 'Set of 2D files not yet implemented. Change your input file.'
#ifdef OPENMPI_ON
         CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_ierror)
#endif
         STOP
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
         WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
         WRITE(*,*) '       ATTENTION:           All parameters are fixed'
         WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
     ENDIF
     GOTO 501
  END IF

#ifdef OPENMPI_ON
  IF(mpi_rank.EQ.0) THEN
      IF(mpi_cluster_size.GT.ntry) THEN
         WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
         WRITE(*,*) '       ATTENTION:           Specified MPI cluster size is bigger than the number of tries in the input file'
         WRITE(*,*) '       ATTENTION:           This feature is not supported at the moment'
         WRITE(*,*) '       ATTENTION:           Idling the unused processes'
         WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
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


         !IF(arg.EQ. ' ') THEN
         OPEN(23,FILE='nf_output_tries.dat',STATUS= 'UNKNOWN')
         WRITE(23,*) 'Number_of_tries ', ntry
         WRITE(23,*) 'Evidence_average:', evsum_final
         WRITE(23,*) 'Evidence_standard_deviation:', evsum_err
         WRITE(23,*) '# N. try   n. steps    Final evidence   Max loglikelihood'
         DO itry=1,ntry
            WRITE(23,*)  itry , nall_try(itry), evsum_final_try(itry), live_like_max_try(itry)
         END DO
         CLOSE(23)
         !END IF

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
      evsum_err_est = DSQRT(DBLE(nall))/(nlive*ntry)

      ! Calculate the mean and the standard deviation for each parameter

      ! print arrays sizes
      !write(*,*) 'nall', nall
      !write(*,*) 'weight_par dimensions', shape(weight_par)
      !write(*,*) 'weight dimensions', shape(weight)

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


      !END IF
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
      !write(*,*) info, DEXP(info), ntry, FLOOR(DEXP(info)*ntry), nexp, live_like_mean,  evsum_final
      !pause

      ! ---------------- Write results on screen and files -------------------------------------
      !OPEN(23,FILE='nf_output_points.dat',STATUS= 'UNKNOWN')
      !WRITE(23,*) '# n     lnlikelihood     weight      parameters'
      !DO j=1,nall
      !   WRITE(23,*) j, live_like_final(j), weight(j), live_final(j,:)
      !END DO
      !CLOSE(23)

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
         WRITE(23,*) par_name(i), par_bnd1(i),par_bnd2(i)
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
      !IF(arg.EQ. ' ') THEN
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

  IF(static_seed.AND.mpi_rank.EQ.0) THEN
     WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
     WRITE(*,*) '       ATTENTION:           This nested_fit output was ran with a set seed! This is intended for testing only!'
     WRITE(*,*) '       ATTENTION:           If you are using this as a production setting change the cmake NORNG option to OFF.'
     WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
  ENDIF
  !IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
  !   DEALLOCATE(x,nc,enc)
  !ELSE
     !DEALLOCATE(x_set,nc_set)
  !END IF


!100 FORMAT (A,' ',E10.4e2,' ',E10.4e2,' ',E10.4e2,' ',E10.4e2)


END PROGRAM NESTED_FIT
