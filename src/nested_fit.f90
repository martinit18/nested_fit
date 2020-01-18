PROGRAM NESTED_FIT
  ! Time-stamp: <Last changed by martino on Thursday 09 January 2020 at CET 09:55:28>
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
  ! Module for cluster analysis
  USE MOD_MEAN_SHIFT_CLUSTER_ANALYSIS
  !USE RNG
  !
  IMPLICIT NONE
  ! Data
  INTEGER(4) :: i=0, j=0, k=0, ndata=0, nd=0
  INTEGER(4) :: npoint=0, nwidth=0
  INTEGER(4) :: npar=0
  INTEGER(4), PARAMETER :: maxdata=10000
  ! Likelihood stuff
  REAL(8) :: DLOG_FAC
  REAL(8), PARAMETER :: pi=3.141592653589793d0
  ! Parameters values and co.
  INTEGER(4), ALLOCATABLE,  DIMENSION(:) :: par_num, fix
  CHARACTER,  ALLOCATABLE, DIMENSION(:) :: par_name*10
  REAL(8), ALLOCATABLE, DIMENSION(:) :: step, val, bnd1, bnd2
  !
  INTEGER(4) :: nlive=0
  REAL(8) :: evaccuracy=0., sdfraction=0., const_ll=0.
  REAL(8) :: LOGLIKELIHOOD_PAR,  LOGLIKELIHOOD_PAR_SET
  CHARACTER :: string*128, filename*64, funcname*64, out_filename*64, tmp_filename*64
  REAL(8) :: xmin=0, xmax=0
  REAL(4) :: version_file
  REAL(4), PARAMETER :: version = 3.2
  CHARACTER :: lr*1= 'r'
  CHARACTER :: set_yn*1= 'n',errorbars_yn*1= 'n',cluster_yn*1= 'n'
  ! Stuff to save separate components
  LOGICAL :: plot = .false.
  REAL(8) :: yfit
  REAL(8), DIMENSION(maxdata) :: x_raw=0, nc_raw=0, nc_err_raw=0, x_tmp=0, nc_tmp=0, nc_err_tmp=0
  !REAL(8), ALLOCATABLE, DIMENSION(:) :: x, nc, enc
  REAL(8), DIMENSION(maxdata) :: x, nc, nc_err, enc
  ! Parameters for set of spectra
  INTEGER(4), PARAMETER :: nsetmax=10
  INTEGER(4) :: nset = 1
  INTEGER(4), DIMENSION(nsetmax) :: ndata_set=0
  REAL(8), DIMENSION(nsetmax) :: xmin_set, xmax_set
  REAL(8), DIMENSION(maxdata,nsetmax) :: x_tmp_set=0, nc_tmp_set=0, nc_err_tmp_set=0
  CHARACTER, DIMENSION(nsetmax) :: filename_set*64
  !REAL(8), ALLOCATABLE, DIMENSION(:,:) :: x_set, nc_set
  REAL(8), DIMENSION(maxdata,nsetmax) :: x_set=0, nc_set=0, nc_err_set=0
  ! Results from Nested sampling
  INTEGER(4) :: njump=20, maxstep_try = 10000, maxtries=1000, maxntries=10
  INTEGER(4) :: nall=0
  REAL(8) :: evsum_final=0., live_like_max=0.
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_final
  REAL(8), ALLOCATABLE, DIMENSION(:) :: weight, live_like_final, live_max
  ! Final results
  REAL(8), ALLOCATABLE, DIMENSION(:) :: par_mean, par_sd
  REAL(8), ALLOCATABLE, DIMENSION(:) :: par_median_w, par_m68_w, par_p68_w, par_m95_w
  REAL(8), ALLOCATABLE, DIMENSION(:) :: par_p95_w, par_m99_w, par_p99_w
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: weight_par
  REAL(8) :: USERFCN, USERFCN_SET
  REAL(8) :: mean_tmp=0., mean2_tmp=0., weight_tot=0., weight_int=0., weight_int_next=0.
  REAL(8) :: evsum_err=0., evsum_err_est=0.
  INTEGER(8), PARAMETER :: maxfit = 10000
  REAL(8) :: minx=0., maxx=0., xfit=0., dx=0.
  REAL(8) :: live_like_mean=0., info=0., comp=0.
  INTEGER(8) :: nexp=0
  ! Parallelization variables
  INTEGER(4) :: itry=1, ntry=1, nth
  INTEGER(4), DIMENSION(1) :: itrymax
  INTEGER(4), ALLOCATABLE, DIMENSION(:) :: nall_try
  REAL(8), ALLOCATABLE, DIMENSION(:) :: evsum_final_try, live_like_max_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: live_final_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_like_final_try, weight_try, live_max_try
  ! Time measurement variables
  REAL(8) :: seconds, seconds_omp, startt, stopt
  ! Argument staff
  !CHARACTER(len=32) :: arg
  INTEGER(4) :: iproc
  CHARACTER(len=2) :: proc
  ! Random number variables
  !INTEGER, PARAMETER :: ntrymax = 10, irnmax = 100000
  !REAL(8), DIMENSION(irnmax,ntrymax) :: rng
  !INTEGER :: irn
  !REAL(8) ::rn
  INTEGER(4) :: seed_array(33) = 1

  COMMON /loglikelihood/ const_ll
  COMMON /func_exp/ lr
  COMMON /func_conv/ npoint, nwidth
  COMMON /func_plot/ plot

  ! Get argument
  !CALL GETARG(1, arg)
  !IF(ARG.NE. ' ') THEN
  !   !READ(arg,'(I10)') iproc
  !   WRITE(tmp_filename, 1002) arg, '_nf_output_res.dat'
  !   WRITE(*,*) tmp_filename
     !WRITE(*,*) arg, iproc
  !END IF

  ! Calculate time elapsed
  ! Parallel real time
  seconds = omp_get_wtime( )
  ! Absolute time
  CALL CPU_TIME(startt)

  ! Print program version
  WRITE(*,*) 'Current program version = ', version

  ! Read parameter file
  OPEN (UNIT=77, FILE='nf_input.dat', STATUS='old')
  READ(77,*) version_file, string
  IF(version.NE.version_file) THEN
     WRITE(*,*) 'Program version not corresponding to the input type'
     WRITE(*,*) 'Please change your input.dat'
     STOP
  END IF
  READ(77,*) filename, string
  READ(77,*) set_yn, string
  READ(77,*) errorbars_yn, string
  READ(77,*) nlive, string
  READ(77,*) evaccuracy, string
  READ(77,*) sdfraction, njump, maxtries, maxntries, string
  READ(77,*) cluster_yn, cluster_method, distance_limit, bandwidth, string
  READ(77,*) ntry, maxstep_try, string
  READ(77,*) funcname, string
  READ(77,*) lr, string
  READ(77,*) npoint, nwidth, string
  READ(77,*) xmin, xmax, string
  READ(77,*) npar, string
  !
  !
  ! Allocate space for parameters and initialize
  ALLOCATE(live_max(npar),par_num(npar),par_name(npar),val(npar),step(npar),bnd1(npar),bnd2(npar),fix(npar), &
       par_mean(npar),par_sd(npar), &
       par_median_w(npar), &
       par_m68_w(npar),par_p68_w(npar),par_m95_w(npar),par_p95_w(npar),par_m99_w(npar),par_p99_w(npar))
  live_max = 0.
  par_num = 0
  par_name = ' '
  val = 0.
  step = 0.
  bnd1 = 0.
  bnd2 = 0.
  fix = 0
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
     READ(77,*,ERR=10,END=10) par_num(i),par_name(i),val(i),step(i),bnd1(i),bnd2(i),fix(i)
     IF (bnd1(i).GE.bnd2(i)) THEN
        WRITE(*,*) 'Bad limits in parameter n.', i, ' (low bound1 >= high bound!!!) Change it and restart'
        WRITE(*,*) 'Low bound:', bnd1(i), 'High bound:', bnd2(i)
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
        READ(88,*,ERR=11,END=11) xmin_set(k), xmax_set(k), string
     END DO
     xmin_set(1) = xmin
     xmax_set(1) = xmax
     DO k = 2, nset
        READ(88,*,ERR=11,END=11) filename_set(k)
     END DO
     filename_set(1) = filename
11   CONTINUE
     CLOSE(88)
  ENDIF


  ! Some tests and messages
  WRITE(*,*) 'Parameter file read'
  IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
     WRITE(*,*) 'Filename = ', filename
  ELSE
     WRITE(*,*) 'Filenames = ', filename_set(1:nset)
  END IF
  WRITE(*,*) 'Funcname = ', funcname

  !write(*,*) const_ll

  ! READ DATA, calculate the constants for the likelihood function
  IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
     OPEN(10,file=filename,status='old')
     ! Case without errorbars
     IF (errorbars_yn.EQ.'n'.OR.errorbars_yn.EQ.'N') THEN
        DO i=1, maxdata
           READ(10,*,END=20) x_raw(i), nc_raw(i)
           ! Make test for integer numbers
           IF (ABS(nc_raw(i)-INT(nc_raw(i))).GT.1E-5) THEN
              WRITE(*,*) 'Attention, input numbers are not counts and you are using Poisson statistic (no error bar)'
              WRITE(*,*) 'n. counts = ', nc_raw(i)
              WRITE(*,*) 'Change something!'
              STOP
           END IF
           ! Select the data
           IF(x_raw(i).GE.xmin.AND.x_raw(i).LE.xmax) THEN
              nd = nd + 1
              x_tmp(nd) = x_raw(i)
              nc_tmp(nd) = nc_raw(i)
              ! Calculation of the constant part of the likelihood with Poisson distribution
              !IF (nc_tmp(nd).GT.0.) const_ll = const_ll - DFAC_LN(INT(nc_tmp(nd)))
              ! Uses of the gamma function gamma(n) = (n-1)!
              IF (nc_tmp(nd).GT.0.) const_ll = const_ll - DLOG_FAC(INT(nc_tmp(nd)))
              !write(*,*) nc_tmp(nd), const_ll
           END IF
        ENDDO
     ELSE
        ! Case with errorbars
        DO i=1, maxdata
           READ(10,*,END=20) x_raw(i), nc_raw(i), nc_err_raw(i)
           ! Select the data
           IF(x_raw(i).GE.xmin.AND.x_raw(i).LE.xmax) THEN
              nd = nd + 1
              x_tmp(nd) = x_raw(i)
              nc_tmp(nd) = nc_raw(i)
              nc_err_tmp(nd) = nc_err_raw(i)
              IF (nc_err_tmp(nd).LE.0.) THEN
                 WRITE(*,*) 'Errorbars with a value equal to 0 or negative'
                 WRITE(*,*) 'Check our data or do not use errorbars'
                 STOP
              END IF
              ! Calculation of the constant part of the likelihood with Gaussian distribution
              const_ll = -nd*DLOG(2*pi)
           END IF
        ENDDO
     END IF
20   CONTINUE
     CLOSE(10)
     ndata = nd

     WRITE(*,*) 'Data file read'
     WRITE(*,*) 'ndata = ', ndata
     WRITE(*,*) 'constant in evidence calc. = ', const_ll
     !pause

     ! Allocate set of data
     !ALLOCATE(x(ndata),nc(ndata),enc(ndata))
     x = 0.
     nc = 0.
     nc_err = 1.
     x(1:ndata)  = x_tmp(1:ndata)
     nc(1:ndata) = nc_tmp(1:ndata)
     nc_err(1:ndata) = nc_err_tmp(1:ndata)
     enc = 0.

  ELSE IF (set_yn.EQ.'y'.OR.set_yn.EQ.'Y') THEN
     ! Case with set of files
     ! READ set of data
     DO k=1,nset
        ! Case without errorbars
        IF (errorbars_yn.EQ.'n'.OR.errorbars_yn.EQ.'N') THEN
           OPEN(10,file=filename_set(k),status='old')
           nd = 0
           DO i=1, maxdata
              READ(10,*,END=30) x_raw(i), nc_raw(i)
              ! Make test for integer numbers
              IF (ABS(nc_raw(i)-INT(nc_raw(i))).GT.1E-5) THEN
                 WRITE(*,*) 'Attention, input numbers are not counts and you are using Poisson statistic (no error bar)'
                 WRITE(*,*) 'n. counts = ', nc_raw(i)
                 WRITE(*,*) 'Change something!'
                 STOP
              END IF
              ! Select the data
              IF(x_raw(i).GE.xmin_set(k).AND.x_raw(i).LE.xmax_set(k)) THEN
                 nd =nd + 1
                 x_tmp_set(nd,k) = x_raw(i)
                 nc_tmp_set(nd,k) = nc_raw(i)
                 ! Calculation of the constant part of the likelihood with Poisson distribution
                 !IF (nc_tmp_set(nd,k).GT.0.) const_ll = const_ll - DFAC_LN(INT(nc_tmp_set(nd,k))) DLOG(DGAMMA((nc_tmp(nd))+1.))
                 IF (nc_tmp_set(nd,k).GT.0.) const_ll = const_ll - DLOG_FAC(INT(nc_tmp_set(nd,k)))
              END IF
           ENDDO
30         CONTINUE
           CLOSE(10)
           ndata_set(k) = nd
        ELSE
           OPEN(10,file=filename_set(k),status='old')
           nd = 0
           DO i=1, maxdata
              READ(10,*,END=40) x_raw(i), nc_raw(i), nc_err_raw(i)
              ! Select the data
              IF(x_raw(i).GE.xmin_set(k).AND.x_raw(i).LE.xmax_set(k)) THEN
                 nd =nd + 1
                 x_tmp_set(nd,k) = x_raw(i)
                 nc_tmp_set(nd,k) = nc_raw(i)
                 nc_err_tmp_set(nd,k) = nc_err_raw(i)
                 IF (nc_err_tmp_set(nd,k).LE.0.) THEN
                    WRITE(*,*) 'Errorbars with a value equal to 0 or negative'
                    WRITE(*,*) 'Check our data or do not use errorbars'
                    STOP
                 END IF
                 ! Calculation of the constant part of the likelihood with Gaussian distribution
                 const_ll = const_ll - DLOG(DSQRT(2*pi)*nc_err_tmp_set(nd,k))
              END IF
           ENDDO
40         CONTINUE
           CLOSE(10)
           ndata_set(k) = nd
        END IF
        WRITE(*,*) 'Data file read. N.', k, ' of ', nset, ' files'
        WRITE(*,*) 'ndata = ', ndata_set(k)

     END DO



     ! Allocate set of data
     ndata = MAXVAL(ndata_set)
     !ALLOCATE(x_set(ndata,nset),nc_set(ndata,nset))
     x_set = 0.
     nc_set = 0.
     DO k=1,nset
        x_set(1:ndata_set(k),k)  = x_tmp_set(1:ndata_set(k),k)
        nc_set(1:ndata_set(k),k) = nc_tmp_set(1:ndata_set(k),k)
        nc_err_set(1:ndata_set(k),k) = nc_err_tmp_set(1:ndata_set(k),k)
     END DO
  ELSE
     WRITE(*,*) 'Check your input file. Correct number of parameters?'
  END IF




  ! Initialise functions if needed
  IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
     IF(funcname.EQ.'ROCKING_CURVE') THEN
        ! Passing as argument the smoothing factors to be adjusted case by case
        ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
        ! with m the number of points
        CALL INIT_ROCKING(val(6),val(7))
     ELSE IF(funcname.EQ.'TWO_INTERP_VOIGT_POLY') THEN
        ! Passing as argument the smoothing factors to be adjusted case by case
        ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
        ! with m the number of points
        CALL INIT_TWO_INTERP(val(14),val(15))
     ELSE IF(funcname.EQ.'SIX_GAUSS_SHIRLEYBG'&
          .OR.funcname.EQ.'SIX_VOIGT_SHIRLEYBG'&
          .OR.funcname.EQ.'SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES'&
          .OR.funcname.EQ.'SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES') THEN
        CALL INIT_SHIRLEY(ndata,x(1:ndata),nc(1:ndata))
     END IF
  ELSE
     IF(funcname.EQ.'ROCKING_CURVE_SET') THEN
        ! Passing as argument the smoothing factors to be adjusted case by case
        ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
        ! with m the number of points
        CALL INIT_ROCKING_SET(val(9),val(10),val(11),val(12))
     END IF
  END IF



  ! Allocate parallel stuff
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




  ! If all parameters are fixed, skip Nested sampling
  IF (ALL(fix.EQ.1)) THEN
     live_max = val
     par_mean = val
     par_median_w = val
     ! To check the likelihood function
     IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
        evsum_final = LOGLIKELIHOOD_PAR(funcname,ndata,x(1:ndata),nc(1:ndata),nc_err(1:ndata),errorbars_yn,npar,val)
     ELSE
        evsum_final = LOGLIKELIHOOD_PAR_SET(funcname,ndata,nset,ndata_set(1:nset),x_set(1:ndata,1:nset),&
             nc_set(1:ndata,1:nset),nc_err_set(1:ndata,1:nset),errorbars_yn,npar,val)
     END IF
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

  ! Initiate random generator with a different seed each time
  !CALL INIT_RANDOM_SEED()
  !CALL RANDOM_SEED(PUT=seed_array)
  !CALL RANDOM_SEED()
  ! To start always with the same random seeds for tests propose
  !CALL RANDOM_SEED(PUT=seed_array)
  ! Other tries
  !CALL RANDOM_NUMBER(rng)
  !DO itry=1,ntry
  !   CALL RNG_SEED(rng(itry), 932117 + 10*itry)
  !   write(*,*) rng(1:5,itry)
  !END DO


  !!!$OMP PARALLEL DO
  ! IT IS NOT WORKING
  DO itry=1,ntry
     !CALL RANDOM_SEED()
     !CALL SRAND(932117 +10*itry)
     !CALL RANDOM_NUMBER(rn)
     !WRITE(*,*) 'Number of try:', itry, rn, rand(0), rng_uniform(rng(itry))

     !write(*,*) itry,ndata,nset,ndata_set(1:nset),funcname,&
     !     npar,fix,step,val,bnd1,bnd2,nlive,evaccuracy,sdfraction
     !PAUSE
     !write(*,*) x_set(1:ndata,1:nset),nc_set(1:ndata,1:nset)
     !PAUSE

     IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
        CALL NESTED_SAMPLING(itry,ndata,x(1:ndata),nc(1:ndata),nc_err(1:ndata),errorbars_yn,&
             funcname,npar,fix,step,val,bnd1,bnd2,nlive,evaccuracy,sdfraction,njump,maxtries,maxntries,&
             cluster_yn,maxstep_try,nall_try(itry),evsum_final_try(itry),live_like_final_try(:,itry),weight_try(:,itry),&
             live_final_try(:,:,itry),live_like_max_try(itry),live_max_try(:,itry))
     ELSE
        CALL NESTED_SAMPLING_SET(itry,ndata,nset,ndata_set(1:nset),x_set(1:ndata,1:nset),nc_set(1:ndata,1:nset), &
             nc_err_set(1:ndata,1:nset),errorbars_yn,funcname,&
             npar,fix,step,val,bnd1,bnd2,nlive,evaccuracy,sdfraction,&
             njump,cluster_yn,maxstep_try,nall_try(itry),evsum_final_try(itry),live_like_final_try(:,itry),weight_try(:,itry),&
             live_final_try(:,:,itry),live_like_max_try(itry),live_max_try(:,itry))
     END IF
  END DO
  !!!$OMP END PARALLEL DO

  ! Re-assemble the points ---------------------------------------------------------------
  ! Final number of points
  nall = SUM(nall_try)
  ALLOCATE(weight(nall),live_like_final(nall),live_final(nall,npar),weight_par(nall,npar))
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
     OPEN(22,FILE='nf_output_tries.dat',STATUS= 'UNKNOWN')
     WRITE(22,*) 'Number_of_tries ', ntry
     WRITE(22,*) 'Evidence_average:', evsum_final
     WRITE(22,*) 'Evidence_standard_deviation:', evsum_err
     WRITE(22,*) '# N. try   n. steps    Final evidence   Max loglikelihood'
     DO itry=1,ntry
        WRITE(22,*)  itry , nall_try(itry), evsum_final_try(itry), live_like_max_try(itry)
     END DO
     CLOSE(22)
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
  !evsum_err_est = DSQRT(DBLE(nall))/(nlive*ntry)

  ! Calculate the mean and the standard deviation for each parameter

  ! print arrays sizes
  !write(*,*) 'nall', nall
  !write(*,*) 'weight_par dimensions', shape(weight_par)
  !write(*,*) 'weight dimensions', shape(weight)

  DO j=1,npar
     IF (fix(j).NE.1) THEN
        mean_tmp = 0.
        mean2_tmp = 0.
        weight_tot = 0.
        DO i=1,nall
           weight_tot = weight_tot + weight(i)
           mean_tmp = mean_tmp + live_final(i,j)*weight(i)
           mean2_tmp = mean2_tmp + live_final(i,j)**2*weight(i)
        END DO
        ! Mean and standard deviation
        par_mean(j) = mean_tmp/weight_tot
        par_sd(j) = DSQRT(mean2_tmp/weight_tot - par_mean(j)**2)

        ! Median and confidence levels
        ! Order a defined parameter with his weight


        weight_par(:,1) = weight/weight_tot
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
        par_mean(j) =  val(j)
        par_median_w(j) = val(j)
     END IF
  END DO

501 CONTINUE

   !-----------------------Calculate the expected function values -------------------------

  ! Calculate the expected function value and residual for max likelihood
  ! New from version 1.0: save three pairs of files with values from max likelihood, mean and median of the parameters
  !IF(arg.EQ. ' ') THEN
  IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
     ! max likelihood values
     DO i=1, ndata
        enc(i) = USERFCN(x(i),npar,live_max,funcname)
     ENDDO
     OPEN (UNIT=20, FILE='nf_output_data_max.dat', STATUS='unknown')
     WRITE(20,*)'# x    y data    y theory      y diff    y err'
     DO i=1, ndata
        IF (errorbars_yn.EQ.'y'.OR.errorbars_yn.EQ.'Y') THEN
           WRITE(20,*) x(i), ' ',nc(i), ' ',enc(i), ' ',nc(i)-enc(i), ' ', nc_err(i)
        ELSE
           WRITE(20,*) x(i), ' ',nc(i), ' ',enc(i), ' ',nc(i)-enc(i), ' ', sqrt(nc(i))
        END IF
     END DO
     CLOSE(20)
     ! mean values
     DO i=1, ndata
        enc(i) = USERFCN(x(i),npar,par_mean,funcname)
     ENDDO
     OPEN (UNIT=20, FILE='nf_output_data_mean.dat', STATUS='unknown')
     WRITE(20,*)'# x    y data    y theory      y diff    y err'
     DO i=1, ndata
        IF (errorbars_yn.EQ.'y'.OR.errorbars_yn.EQ.'Y') THEN
           WRITE(20,*) x(i), ' ',nc(i), ' ',enc(i), ' ',nc(i)-enc(i), ' ', nc_err(i)
        ELSE
           WRITE(20,*) x(i), ' ',nc(i), ' ',enc(i), ' ',nc(i)-enc(i), ' ', sqrt(nc(i))
        END IF
     END DO
     CLOSE(20)
     ! median values
     DO i=1, ndata
        enc(i) = USERFCN(x(i),npar,par_median_w,funcname)
     ENDDO
     OPEN (UNIT=20, FILE='nf_output_data_median.dat', STATUS='unknown')
     WRITE(20,*)'# x    y data    y theory      y diff    y err'
     DO i=1, ndata
        IF (errorbars_yn.EQ.'y'.OR.errorbars_yn.EQ.'Y') THEN
           WRITE(20,*) x(i), ' ',nc(i), ' ',enc(i), ' ',nc(i)-enc(i), ' ', nc_err(i)
        ELSE
           WRITE(20,*) x(i), ' ',nc(i), ' ',enc(i), ' ',nc(i)-enc(i), ' ', sqrt(nc(i))
        END IF
     END DO
     CLOSE(20)
  ELSE
     DO k=1, nset
        enc = 0.
        ! max likelihood values
        DO i=1, ndata_set(k)
           enc(i) = USERFCN_SET(x_set(i,k),npar,live_max,funcname,k)
        ENDDO
        WRITE(out_filename,1000) 'nf_output_data_max_',k,'.dat'
        OPEN (UNIT=30, FILE=out_filename, STATUS='unknown')
        WRITE(30,*)'# x    y data    y theory      y diff    y err'
        DO i=1, ndata_set(k)
           IF (errorbars_yn.EQ.'y'.OR.errorbars_yn.EQ.'Y') THEN
              WRITE(30,*) x_set(i,k), ' ',nc_set(i,k), ' ',enc(i), ' ',nc_set(i,k)-enc(i), ' ',  nc_err_set(i,k)
           ELSE
              WRITE(30,*) x_set(i,k), ' ',nc_set(i,k), ' ',enc(i), ' ',nc_set(i,k)-enc(i), ' ', sqrt(nc_set(i,k))
           END IF
        END DO
        CLOSE(30)
        ! mean values
        DO i=1, ndata_set(k)
           enc(i) = USERFCN_SET(x_set(i,k),npar,par_mean,funcname,k)
        ENDDO
        WRITE(out_filename,2000) 'nf_output_data_mean_',k,'.dat'
        OPEN (UNIT=30, FILE=out_filename, STATUS='unknown')
        WRITE(30,*)'# x    y data    y theory      y diff    y err'
        DO i=1, ndata_set(k)
           IF (errorbars_yn.EQ.'y'.OR.errorbars_yn.EQ.'Y') THEN
              WRITE(30,*) x_set(i,k), ' ',nc_set(i,k), ' ',enc(i), ' ',nc_set(i,k)-enc(i), ' ',  nc_err_set(i,k)
           ELSE
              WRITE(30,*) x_set(i,k), ' ',nc_set(i,k), ' ',enc(i), ' ',nc_set(i,k)-enc(i), ' ', sqrt(nc_set(i,k))
           END IF
        END DO
        CLOSE(30)
        ! median values
        DO i=1, ndata_set(k)
           enc(i) = USERFCN_SET(x_set(i,k),npar,par_median_w,funcname,k)
        ENDDO
        WRITE(out_filename,3000) 'nf_output_data_median_',k,'.dat'
        OPEN (UNIT=30, FILE=out_filename, STATUS='unknown')
        WRITE(30,*)'# x    y data    y theory      y diff    y err'
        DO i=1, ndata_set(k)
           IF (errorbars_yn.EQ.'y'.OR.errorbars_yn.EQ.'Y') THEN
              WRITE(30,*) x_set(i,k), ' ',nc_set(i,k), ' ',enc(i), ' ',nc_set(i,k)-enc(i), ' ',  nc_err_set(i,k)
           ELSE
              WRITE(30,*) x_set(i,k), ' ',nc_set(i,k), ' ',enc(i), ' ',nc_set(i,k)-enc(i), ' ', sqrt(nc_set(i,k))
           END IF
        END DO
        CLOSE(30)
     END DO
  END IF

  ! Save in a file the different fit components !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! New from version 1.0: save three pairs of files with values from max likelihood, mean and median of the parameters
  IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
     maxx = xmax
     minx = xmin
     plot = .true.
     xfit = 0.
     dx=(maxx-minx)/(maxfit-1)

     ! max likelihood values
     OPEN (UNIT=40, FILE='nf_output_fit_max.dat', STATUS='unknown')
     WRITE(40,*)'# x    y fit'
     DO i=1, maxfit
        xfit = minx + (i-1)*dx
        yfit = USERFCN(xfit,npar,live_max,funcname)
     ENDDO
     CLOSE(40)

     ! mean values
     OPEN (UNIT=40, FILE='nf_output_fit_mean.dat', STATUS='unknown')
     WRITE(40,*)'# x    y fit'
     DO i=1, maxfit
        xfit = minx + (i-1)*dx
        yfit = USERFCN(xfit,npar,par_mean,funcname)
     ENDDO
     CLOSE(40)

     ! mean values
     OPEN (UNIT=40, FILE='nf_output_fit_median.dat', STATUS='unknown')
     WRITE(40,*)'# x    y fit'
     DO i=1, maxfit
        xfit = minx + (i-1)*dx
        yfit = USERFCN(xfit,npar,par_median_w,funcname)
     ENDDO
     CLOSE(40)
  ELSE
     DO k=1, nset
        maxx = xmax_set(k)
        minx = xmin_set(k)
        plot = .true.
        xfit = 0.
        dx=(maxx-minx)/(maxfit-1)

        ! max likelihood values
        WRITE(out_filename,1001) 'nf_output_fit_max_',k,'.dat'
        OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
        WRITE(40,*)'# x    y fit'
        DO i=1, maxfit
           xfit = minx + (i-1)*dx
           !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
           yfit = USERFCN_SET(xfit,npar,live_max,funcname,k)
        ENDDO
        CLOSE(40)

        WRITE(out_filename,2001) 'nf_output_fit_mean_',k,'.dat'
        OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
        WRITE(40,*)'# x    y fit'
        DO i=1, maxfit
           xfit = minx + (i-1)*dx
           !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
           yfit = USERFCN_SET(xfit,npar,par_mean,funcname,k)
        ENDDO
        CLOSE(40)

        WRITE(out_filename,3001) 'nf_output_fit_median_',k,'.dat'
        WRITE(40,*)'# x    y fit'
        OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
        DO i=1, maxfit
           xfit = minx + (i-1)*dx
           !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
           yfit = USERFCN_SET(xfit,npar,par_median_w,funcname,k)
        ENDDO
        CLOSE(40)
     END DO
  END IF

  !END IF
  !-----------------Calculate information and bayesian complexity --------------------------
  ! Calculation of the mean loglikelihood
  live_like_mean = 0.
  weight_tot = 0.
  DO i=1,nall
     weight_tot = weight_tot + weight(i)
     live_like_mean = live_like_mean + weight(i)*live_like_final(i)
  END DO
  live_like_mean = live_like_mean/weight_tot


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
  !IF(arg.EQ. ' ') THEN
  OPEN(23,FILE='nf_output_points.dat',STATUS= 'UNKNOWN')
  WRITE(23,*) '# n     lnlikelihood     weight      parameters'
  DO j=1,nall
     WRITE(23,*) j, live_like_final(j), weight(j), live_final(j,:)
  END DO
  CLOSE(23)
  !END IF

  ! Calculate end time
  seconds_omp = omp_get_wtime( ) - seconds
  CALL CPU_TIME(stopt)
  seconds  = stopt - startt
  nth = omp_get_max_threads()

  !IF(arg.EQ. ' ') THEN
  OPEN(22,FILE='nf_output_res.dat',STATUS= 'UNKNOWN')
  !ELSE
  !   WRITE(tmp_filename, 1002) arg, '_nf_output_res.dat'
  !   WRITE(*,*) tmp_filename
  !   !pause
  !   OPEN(22,FILE=tmp_filename,STATUS= 'UNKNOWN')
  !END IF
  WRITE(22,*) '#############_FINAL_RESULTS_#####################################################################################'
  WRITE(22,*) 'N._of_trials:            ', ntry
  WRITE(22,*) 'N._of_total_iteration:   ', nall
  WRITE(22,*) 'Final_evidence_(log):    ', evsum_final
  WRITE(22,*) 'Evidence_dispersion(log):', evsum_err
  !WRITE(22,*) 'Estimate_evidence_error(log):', evsum_err_est
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
  WRITE(*,*) 'N. of trials:             ', ntry
  WRITE(*,*) 'N. of total iteration:    ', nall
  WRITE(*,*) 'Final evidence (log):     ', evsum_final
  WRITE(*,*) 'Evidence dispersion (log):', evsum_err
  !WRITE(*,*) 'Estimate evidence_error (log):', evsum_err_est

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
  DEALLOCATE(par_num,par_name,val,step,bnd1,bnd2,fix, &
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
1000 FORMAT (A19,I1,A4)
1001 FORMAT (A18,I1,A4)
2000 FORMAT (A20,I1,A4)
2001 FORMAT (A19,I1,A4)
3000 FORMAT (A22,I1,A4)
3001 FORMAT (A21,I1,A4)

END PROGRAM NESTED_FIT
