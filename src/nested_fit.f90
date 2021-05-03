PROGRAM NESTED_FIT
  ! Time-stamp: <Last changed by martino on Monday 03 May 2021 at CEST 12:14:24>
  !
  ! Please read README and LICENSE files for more inforamtion
  !
  ! 4.0  2D data analysis available, new input and output files for future developments
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

  ! Parallelization library !!!CAREFULL to the table dimension in this case!!
  USE OMP_LIB
  ! Module for the input parameter definition
  USE MOD_PARAMETERS
  ! Module for likelihood for data analysis
  USE MOD_LIKELIHOOD
  !USE RNG
  !
  IMPLICIT NONE
  ! Data
  INTEGER(4) :: i=0, j=0, k=0
  ! Parameters values and co.
  CHARACTER :: string*128
  REAL(4) :: version_file
  REAL(4), PARAMETER :: version = 4.0
  REAL(8) :: search_par1 = 0.0
  REAL(8) :: search_par2 = 0.0
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
  ! Parallelization variables
  INTEGER(4) :: itry=1, nth=1
  INTEGER(4), DIMENSION(1) :: itrymax
  INTEGER(4), ALLOCATABLE, DIMENSION(:) :: nall_try
  REAL(8), ALLOCATABLE, DIMENSION(:) :: evsum_final_try, live_like_max_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: live_final_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_like_final_try, weight_try, live_max_try
  ! Time measurement variables
  REAL(8) :: seconds, seconds_omp, startt, stopt

  ! Random number variables
  !INTEGER(4) :: seed_array(33) = 1


  ! PARALLEL VARIABLE: PUT ".TRUE." IF YOU WANT TO RUN IN PARALLEL
  ! CHANGE ALSO THE FFLAGS IN THE MAKEFILE
  LOGICAL, PARAMETER :: parallel_on = .TRUE.

  EXTERNAL :: NESTED_SAMPLING, SORTN, MEANVAR



<<<<<<< HEAD
  !!!!!!!! Initiate random generator the same  seed each time !!!!!!!!!!!!!!!!!!
  CALL RANDOM_SEED(PUT=seed_array)
=======
  !!!!!!!! Initiate random generator with the same seed each time !!!!!!!!!!!
  !CALL RANDOM_SEED(PUT=seed_array)
>>>>>>> v4
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Other variants
  !CALL RANDOM_SEED()
  !CALL INIT_RANDOM_SEED()

  ! Other tries
  !CALL RANDOM_NUMBER(rng)
  !DO itry=1,ntry
  !   CALL RNG_SEED(rng(itry), 932117 + 10*itry)
  !   write(*,*) rng(1:5,itry)
  !END DO



  ! Calculate time elapsed !!!!!!!!!!!!!!!!!!!!
  ! Parallel real time (and number of threads)
  IF (parallel_on) THEN
    seconds = omp_get_wtime( )
    nth = omp_get_max_threads()
  END IF
  ! Absolute time
  CALL CPU_TIME(startt)

  ! Print program version
  WRITE(*,*) 'Current program version = ', version

  ! Initialize values --------------------------------------------------------------------------------------------------------------
  filename = ' '
  funcname = ' '



  ! Read parameter file ------------------------------------------------------------------------------------------------------------
  OPEN (UNIT=77, FILE='nf_input.dat', STATUS='old')
  READ(77,*) version_file, string
  IF(version.NE.version_file) THEN
     WRITE(*,*) 'Program version not corresponding to the input type'
     WRITE(*,*) 'Please change your input.dat'
     STOP
  END IF
  READ(77,*) filename(1), string
  READ(77,*) set_yn, string
  READ(77,*) data_type, string
  READ(77,*) nlive, string
  READ(77,*) evaccuracy, string
  READ(77,*) search_method, string
  READ(77,*) search_par1, search_par2, maxtries, maxntries, string
  READ(77,*) cluster_yn, cluster_method, distance_limit, bandwidth, string
  READ(77,*) ntry, maxstep_try, string
  READ(77,*) funcname, string
  READ(77,*) lr, string
  READ(77,*) npoint, nwidth, string
  READ(77,*) xmin(1), xmax(1), ymin(1), ymax(1), string
  READ(77,*) npar, string
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
  !
  !
  READ(77,*) string
  DO i = 1, npar
     READ(77,*,ERR=10,END=10) par_num(i),par_name(i),par_in(i),par_step(i),par_bnd1(i),par_bnd2(i),par_fix(i)
     IF (par_bnd1(i).GE.par_bnd2(i)) THEN
        WRITE(*,*) 'Bad limits in parameter n.', i, ' (low bound1 >= high bound!!!) Change it and restart'
        WRITE(*,*) 'Low bound:',par_bnd1(i), 'High bound:', par_bnd2(i)
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

  ! Adapt serach parameters to the search algorithm
  IF (search_method.EQ.'RANDOM_WALK') THEN
     sdfraction = search_par1
     njump      = INT(search_par2)
  END IF


  ! ----------------------------------------------------------------------------------------------------------------------------------


  ! Some tests and messages
  WRITE(*,*) 'Parameter file read'
  WRITE(*,*) 'Filenames = ', filename(1:nset)
  WRITE(*,*) 'Funcname = ', funcname

  ! Not implemented combination of inputs
  IF (data_type(1:1).EQ.'2'.AND.set_yn.EQ.'y') THEN
     WRITE(*,*)
     WRITE(*,*) 'ERROR'
     WRITE(*,*) 'Set of 2D files not yet implemented. Change your input file.'
     STOP
  END IF



  ! Initialize likelihood function
  CALL INIT_LIKELIHOOD()


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
  !




  ! If all parameters are fixed, skip Nested sampling ------------------------------------------------------------------------------
  IF (ALL(par_fix.EQ.1)) THEN
     live_max = par_in
     par_mean = par_in
     par_median_w = par_in
     ! To check the likelihood function
     evsum_final = LOGLIKELIHOOD(par_in)
     WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
     WRITE(*,*) '       ATTENTION:           All parameters are fixed'
     WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
     GOTO 501
  END IF


  ! Parallel Nested sampling.
  ! Compute nth ensemble of live point in parallel and recombine them at the end
  !
  !!!$OMP PARALLEL  SHARED(ndata,x,nc,funcname,npar,fix,step,val,bnd1,bnd2,nlive,evaccuracy,sdfraction)
  !!!$OMP SHARED(nall_try,evsum_final_try,live_like_final_try,weight_try, &
  !!!$OMP live_final_try,live_like_max_try,live_max_try)
  !nth = omp_get_num_threads()
  !IF(nth.GT.nthmax) THEN
  !   WRITE(*,*) 'Number of threads largr of number max of threads', nthmax, 'Change something'
  !   STOP
  !END IF

  !
  ! Run the Nested sampling


  !!!$OMP PARALLEL DO
  ! IT IS NOT WORKING
  DO itry=1,ntry
     !CALL RANDOM_SEED()
     !CALL SRAND(932117 +10*itry)
     !CALL RANDOM_NUMBER(rn)

     CALL NESTED_SAMPLING(itry,maxstep_try,nall_try(itry),evsum_final_try(itry), &
          live_like_final_try(:,itry),weight_try(:,itry),&
          live_final_try(:,:,itry),live_like_max_try(itry),live_max_try(:,itry))

  END DO
!!!$OMP END PARALLEL DO

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


501 CONTINUE

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


  ! Calculate end time
  ! Parallel time
  IF (parallel_on) seconds_omp = omp_get_wtime( ) - seconds
  ! Normal time
  CALL CPU_TIME(stopt)
  seconds  = stopt - startt

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


  ! Deallocate stuff
  DEALLOCATE(par_num,par_name,par_in,par_step,par_bnd1,par_bnd2,par_fix, &
       live_max,par_mean,par_sd, &
       par_median_w, &
       par_m68_w,par_p68_w,par_m95_w,par_p95_w,par_m99_w,par_p99_w)
  DEALLOCATE(weight,live_like_final,live_final,weight_par)
  ! Dellocate parallel stuff
  DEALLOCATE(live_like_final_try,weight_try,live_final_try,live_max_try, &
       nall_try,evsum_final_try,live_like_max_try)


  !IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
  !   DEALLOCATE(x,nc,enc)
  !ELSE
     !DEALLOCATE(x_set,nc_set)
  !END IF


!100 FORMAT (A,' ',E10.4e2,' ',E10.4e2,' ',E10.4e2,' ',E10.4e2)


END PROGRAM NESTED_FIT
