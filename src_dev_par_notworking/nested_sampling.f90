SUBROUTINE NESTED_SAMPLING(itry,maxstep,nall,evsum_final,live_like_final,live_birth_final,live_rank_final,&
   weight,live_final,live_like_max,live_max,mpi_rank,mpi_cluster_size)
  ! Time-stamp: <Last changed by martino on Saturday 09 August 2025 at CEST 15:25:49>

#ifdef OPENMPI_ON
  USE MPI
  USE MOD_MPI
#endif

#ifdef OPENMP_ON
  USE OMP_LIB
#endif

  ! Additional math module
  USE MOD_MATH

  ! Parameter module
  USE MOD_PARAMETERS, ONLY:  npar, nlive, conv_method, evaccuracy, conv_par, &
        search_par2, par_name, par_in, par_step, par_bnd1, par_bnd2, par_fix, &
        make_cluster, nth, maxtries, maxntries, searchid
  ! Module for likelihood
  USE MOD_LIKELIHOOD_GEN
  ! Module for searching new live points
  USE MOD_SEARCH_NEW_POINT, ONLY: SEARCH_NEW_POINT, MAKE_LIVE_MEAN_SD, REMAKE_LIVE_MEAN_SD, DEALLOCATE_SEARCH_NEW_POINTS
  ! Module for cluster analysis
  USE MOD_CLUSTER_ANALYSIS, ONLY: cluster_on, DEALLOCATE_CLUSTER, MAKE_CLUSTER_ANALYSIS, REMAKE_CLUSTER_STD, GET_CLUSTER_MEAN_SD
  ! Module for covariance matrix
  USE MOD_COVARIANCE_MATRIX
  ! Module for optionals
  USE MOD_OPTIONS
  ! Module for logging
  USE MOD_LOGGER
  ! Module for proffiling
  USE MOD_PERFPROF

  !
  IMPLICIT NONE
  INTEGER(4), INTENT(IN) :: itry, maxstep
  INTEGER(4), INTENT(OUT) :: nall
  REAL(8), INTENT(OUT) :: evsum_final, live_like_max
  REAL(8), INTENT(OUT), DIMENSION(npar) :: live_max
  REAL(8), INTENT(OUT), DIMENSION(maxstep) :: weight
  REAL(8), INTENT(OUT), DIMENSION(maxstep) :: live_like_final
  REAL(8), INTENT(OUT), DIMENSION(maxstep,npar) :: live_final
  REAL(8), INTENT(OUT), DIMENSION(maxstep) :: live_birth_final
  INTEGER(4), INTENT(OUT), DIMENSION(maxstep) :: live_rank_final
  INTEGER(4), INTENT(IN) :: mpi_rank, mpi_cluster_size
  ! Random number variables
  !INTEGER, INTENT(IN) :: irnmax
  !REAL(8), INTENT(IN), DIMENSION(irnmax) :: rng
  ! Prior variables
  REAL(8), DIMENSION(npar) :: par_prior
  ! Loop variables
  INTEGER(4) :: nstep = 2
  REAL(8) :: lntstep=1., lntrest=0., lntmass =0., constm = 0., constp = 0.
  REAL(8), DIMENSION(maxstep) :: evstep
  REAL(8), DIMENSION(maxstep) :: live_like_old        ! End values
  REAL(8), DIMENSION(maxstep) :: live_birth_old       ! Birth values
  INTEGER(4), DIMENSION(maxstep) :: live_rank_old     ! Likelihood sort rank at the birth
  REAL(8), DIMENSION(maxstep,npar) :: live_old
  REAL(8) :: evsum = 0., evrestest = 0., evtotest = 0.
  ! Search variable to monitor efficiency
  INTEGER(4), ALLOCATABLE, DIMENSION(:) :: ntries
  ! Live points variables parallel
  REAL(8), ALLOCATABLE, DIMENSION(:) :: live_like_new
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_new
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: too_many_tries
  INTEGER(4), ALLOCATABLE, DIMENSION(:) :: icluster
  LOGICAL, ALLOCATABLE :: live_searching(:), live_ready(:)
  LOGICAL :: early_exit = .false.
  LOGICAL :: normal_exit = .false.
  ! Live points variables
  REAL(8) :: min_live_like = 0.
  REAL(8), DIMENSION(nlive,npar) :: live
  REAL(8), DIMENSION(nlive) :: live_like              
  REAL(8), DIMENSION(nlive) :: live_birth             ! Likelihood step value (birth value)
  INTEGER(4), DIMENSION(nlive) :: live_rank           ! Likelihood sort rank
  INTEGER(4) :: icluster_old=0
  ! Final calculations
  INTEGER(4) :: nstep_final
  REAL(8) :: last_likes, live_like_last, evrest_last, evlast              
  REAL(8), DIMENSION(nlive) :: live_birth_last               
  INTEGER(4), DIMENSION(nlive) :: live_rank_last  
  ! Rest
  INTEGER(4) :: j, l, n, jlim, it
  REAL(8) :: ADDLOG, rn, gval
  !REAL(4) :: t1, t2

  ! OPENMP stuff
#ifdef OPENMP_ON
  INTEGER(4) :: nthreads=1
#endif
  
  ! MPI Stuff
  REAL(8) :: moving_eff_avg = 0.
  !INTEGER(4) :: processor_name_size
  !INTEGER(4) :: mpi_ierror
  !INTEGER(4) :: mpi_child_spawn_error(1)
#ifdef OPENMPI_ON
  character(LEN=MPI_MAX_PROCESSOR_NAME) :: processor_name
#endif
  CHARACTER :: info_string*4096
  CHARACTER :: fparname_fmt*32
  CHARACTER :: fparval_fmt*32

  LOGICAL :: make_cluster_internal, need_cluster
  INTEGER(4) :: n_call_cluster, n_call_cluster_it, n_mat_cov
  INTEGER(4), PARAMETER :: n_call_cluster_it_max=3, n_call_cluster_max=10

  PROFILED(NESTED_SAMPLING)

  ! Initialize variables (with different seeds for different processors)
  gval = 0.
  par_prior = 0.
  live = 0.
  live_like = 0.
  live_birth = 0.
  live_rank = 0
  live_old = 0.
  live_like_old = 0.
  live_birth_old = 0.
  live_rank_old = 0
  live_final = 0.
  live_like_final = 0.
  live_birth_final = 0.
  live_rank_final = 0
  evstep = 0.
  nstep = maxstep - nlive + 1
  n_call_cluster=0
  n_call_cluster_it=0
  n_mat_cov=0
  !maxtries = 50*njump ! Accept an efficiency of more than 2% for the exploration, otherwise change something

  ! OPENMP stuff
#ifdef OPENMP_ON
  nthreads = omp_get_max_threads()
#endif

  ALLOCATE(ntries(nth),live_like_new(nth),live_new(nth,npar),too_many_tries(nth),icluster(nth),live_searching(nth),live_ready(nth))
  ntries = 0
  live_new = 0.
  live_like_new = 0.
  icluster = 0
  too_many_tries = .false.
  make_cluster_internal=.false.
  need_cluster=.false.
  live_searching = .false.
  live_ready = .false.

  ! If using lo output set the fmt for params and param names
  IF(opt_lib_output) THEN
    WRITE(fparval_fmt, '(I8, "(F23.8)")') npar
    WRITE(fparname_fmt, '(I8, "(A16)")') npar
  ENDIF

  ! ---------- Inintial live points sorting ------------------------------------------------
  CALL LOG_TRACE('Sorting live points. N. of points = '//TRIM(ADJUSTL(INT_TO_STR_INLINE(nlive))))
  DO j=1, nlive
     ! Sort point considering priors
     ! Generate random prior if not fixe
     DO l=1,npar
        IF(par_fix(l).NE.1) THEN
           ! Uniform prior inside the limits
           IF (par_step(l).LE.0.) THEN
801           CALL RANDOM_NUMBER(rn)
              par_prior(l) = par_bnd1(l) + (par_bnd2(l)-par_bnd1(l))*rn
              IF(par_prior(l).LT.par_bnd1(l).OR.par_prior(l).GT.par_bnd2(l)) GOTO 801
           ELSE
              ! Gaussian distribution
700           CALL RANDOM_NUMBER(rn)
              par_prior(l) = par_bnd1(l) + (par_bnd2(l)-par_bnd1(l))*rn
              gval = dexp(-(par_prior(l)-par_in(l))**2/(2*par_step(l)**2))
              CALL RANDOM_NUMBER(rn)
              IF(rn.GT.gval) GOTO 700
           END IF
        ELSE
           ! If fixed, take the value indicated in the file
           par_prior(l) = par_in(l)
        END IF
     END DO
     ! If it is good, take it
     live(j,:) = par_prior(:)
     live_like(j) = LOGLIKELIHOOD_WITH_TEST(npar, par_prior)
     live_rank(j) = j
  END DO
  live_birth = MINVAL(live_like) - 10.d0

  ! Calculate average and standard deviation of the parameters
  CALL MAKE_LIVE_MEAN_SD(live)

  ! Order livepoints
  CALL SORTN(nlive,npar,live_like,live)
  ! Store in a file
  OPEN(11,FILE='nf_output_initial_live_points.dat',STATUS= 'UNKNOWN')
  WRITE(11,*) '# n     lnlikelihood     parameters    birth_lnlike   rank'
  DO j=1,nlive
     WRITE(11,*) j, live_like(j), live(j,:), live_birth(j), live_rank(j)
  END DO
  CLOSE(11)


  ! --------- Set-up stuff before main loop ------------------------------------------------
  ! Calculate the intervarls (logarithmic) for the integration
  ! This is equivalent to the "crude" approximation from Skilling

  ! Calculate the first differential prior mass and the remaining prior mass
  !tlnmass(1) = DLOG(-(tstep(1)-1))
  !tlnrest(1) = DLOG(tstep(1))

  ! Alternative calculations
  lntstep = -1.d0/nlive
  lntrest = lntstep
  lntmass = DLOG(1 - DEXP(lntstep))
  ! Check
  !write(*,*) dlog(tstep(1)), lntstep, dlog(tstep(1))-lntstep
  !write(*,*) tlnrest(1), lntrest, tlnrest(1) - lntrest
  !write(*,*) tlnmass(1), lntmass, tlnmass(1) - lntmass
  ! Calculations of the constant to be used later
  constm = DLOG(1 - DEXP(-2.d0/nlive)) - DLOG(2.d0)
  constp = DLOG(1 + DEXP(-2.d0/nlive)) - DLOG(2.d0)
  !write(*,*) constm, constp

  ! Actual minimum loglikelihood, fictif birth likelihood, 
  ! rank and correspondent live point
  live_like_old(1) = live_like(1)
  live_old(1,:) = live(1,:)
  live_birth_old(1) = live_birth(1)
  live_rank_old(1) = live_rank(1)

  ! First evidence (sum because we are working with logarithms)
  IF(conv_method .EQ. 'LIKE_ACC') THEN
    evstep(1) = live_like_old(1) + lntmass
    evsum = evstep(1)
  ELSE IF(conv_method .EQ. 'ENERGY_ACC') THEN
    evstep(1) = 1./conv_par*live_like_old(1) + lntmass
    evsum = evstep(1)
  ELSE IF(conv_method .EQ. 'ENERGY_MAX') THEN
    evstep(1) = 1./conv_par*live_like_old(1) + lntmass
    evsum = evstep(1)
  ELSE
    CALL LOG_ERROR_HEADER()
    CALL LOG_ERROR('Invalid convergence method.')
    CALL LOG_ERROR('Available options: [LIKE_ACC, ENERGY_ACC, ENERGY_MAX].')
    CALL LOG_ERROR_HEADER()
  END IF


  ! If MPI is active, spawn the writter process
#ifdef OPENMPI_ON
   CALL MPI_Comm_spawn(nf_child_proc_name, MPI_ARGV_NULL, 1, MPI_INFO_NULL, 0, MPI_COMM_WORLD, mpi_child_writter_comm, mpi_child_spawn_error, mpi_ierror)
   IF(mpi_rank.EQ.0) THEN
         CALL MPI_SEND(mpi_cluster_size, 1, MPI_INT, 0, 0, mpi_child_writter_comm, mpi_ierror)
         CALL MPI_SEND(opt_compact_output, 1, MPI_LOGICAL, 0, 0, mpi_child_writter_comm, mpi_ierror)
         CALL MPI_SEND(opt_suppress_output, 1, MPI_LOGICAL, 0, 0, mpi_child_writter_comm, mpi_ierror)
   ENDIF
   CALL MPI_Get_processor_name(processor_name, processor_name_size, mpi_ierror)
   processor_name = TRIM(ADJUSTL(processor_name))
   CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_ierror)
#endif

  ! Present minimal value of the likelihood
  min_live_like = live_like(1)


  ! Preliminary tasks before the main loop
  n = 1
  
  ! Create covariance matrix and Cholesky decomposition
  CALL ALLOCATE_PAR_VAR()
  CALL CREATE_MAT_COV(live(:,par_var))

  ! Search for new live points a first time before running the parallel region
  it = 1
  !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(p_cluster) &
  !$OMP& SHARED(n,itry,ntries,min_live_like,live_like,live,nth,live_like_new,live_new,icluster,too_many_tries,live_searching,live_ready)  
  DO it=1,nth
      live_searching(it) = .true. 
      CALL SEARCH_NEW_POINT(n,itry,min_live_like,live_like,live, &
         live_like_new(it),live_new(it,:),icluster(it),ntries(it),too_many_tries(it))
      live_searching(it) = .false. 
      live_ready(it) = .true.
  END DO
  !$OMP END PARALLEL DO

  ! Degugging ???
  print *, "Max threads:", omp_get_max_threads()
  print *, "Num threads in parallel region:"
  !$omp parallel
  print *, "Thread", omp_get_thread_num(), "active in parallel region"
  !$omp end parallel

  ! Main parallel region
  !#########################################################################################################################################################  
  !$OMP PARALLEL  num_threads(nthreads)
  !$OMP SINGLE
  !---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  ! ---------------------------------------------------------------------------------------!
  !                                                                                        !
  !                                 START THE MAIN LOOP                                    !
  !                                                                                        !
  !----------------------------------------------------------------------------------------!

  
  main_loop: DO WHILE ((.NOT. normal_exit) .AND. (.NOT. early_exit))
     
   !-----------------------------------------------------------------------------------------------------------------

     ! If the number of steps is reached, we stop the loop
     IF (n.GT.nstep) THEN
        normal_exit = .true.
        nstep_final = n - 1
        !EXIT main_loop
     END IF

     ! If the number of tries is reached, we stop the loop
     IF (MAXVAL(ntries).GT.maxtries) THEN
        CALL LOG_ERROR_HEADER()
        CALL LOG_ERROR('Too many tries to find new live points for try n.: '//TRIM(ADJUSTL(INT_TO_STR_INLINE(itry))))
        CALL LOG_ERROR('More than '//TRIM(ADJUSTL(INT_TO_STR_INLINE(maxtries))))
        CALL LOG_ERROR('We take the data as they are :-~')
        CALL LOG_ERROR_HEADER()
        early_exit = .true.
        nstep_final = n - 1
        !EXIT main_loop
     END IF
     
     IF (normal_exit .OR. early_exit) EXIT main_loop
     
901  IF(make_cluster_internal) THEN
        CALL LOG_TRACE('Performing cluster analysis. Number of step = '//TRIM(ADJUSTL(INT_TO_STR_INLINE(n))))
        CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
        cluster_on = .true.
        make_cluster_internal=.false.
        IF(need_cluster) THEN
           n_call_cluster_it=n_call_cluster_it+1
           n_call_cluster=n_call_cluster+1
           need_cluster=.false.
        END IF
        CALL REALLOCATE_MAT_COV() ! Adapt the covariance matrix and Cholesky decomposition to the number of clusters
        CALL FILL_MAT_COV(live(:,par_var)) ! Fill the covariance matrix and Cholesky decomposition
        n_mat_cov=0
     END IF
     
     IF(n_mat_cov .GT. 0.05*nlive) THEN ! If more than 5% of the points have changed, calculate the covariance matrix and Cholesky decomposition again
        CALL FILL_MAT_COV(live(:,par_var))
        n_mat_cov=0
     END IF
     
     IF(ALL(too_many_tries)) THEN
        !If all searches failed to find a new point, a cluster analysis will be performed immediately if clustering is used. Otherwise, the run will end.
#ifdef OPENMPI_ON
        ! Signal final data MAXED_OUT
        CALL MPI_Send(info_string, 256, MPI_CHARACTER, 0, MPI_TAG_SEARCH_DONE_MANY_TRIES, mpi_child_writter_comm, mpi_ierror)
#endif
        IF (make_cluster) THEN
           IF(n_call_cluster_it>=n_call_cluster_it_max) THEN
              CALL LOG_ERROR_HEADER()
              CALL LOG_ERROR('Too many cluster analysis for an iteration.')
              CALL LOG_ERROR('Change cluster recognition parameters.')
              CALL LOG_ERROR_HEADER()
              CALL HALT_EXECUTION()
           END IF
           IF(n_call_cluster>=n_call_cluster_max) THEN
              CALL LOG_ERROR_HEADER()
              CALL LOG_ERROR('Too many cluster analysis.')
              CALL LOG_ERROR('Change cluster recognition parameters.')
              CALL LOG_ERROR_HEADER()
              CALL HALT_EXECUTION()
           END IF
           make_cluster_internal=.true.
           need_cluster=.true.
           GOTO 901
        ELSE
           CALL LOG_WARNING_HEADER()
           CALL LOG_WARNING('Too many tries to find new live points for try n.: '//TRIM(ADJUSTL(INT_TO_STR_INLINE(itry))))
           CALL LOG_WARNING('More than '//TRIM(ADJUSTL(INT_TO_STR_INLINE(maxtries))))
           CALL LOG_WARNING('We take the data as they are :-~')
           CALL LOG_WARNING_HEADER()
           early_exit = .true.
           nstep_final = n - 1
        END IF
     ELSE IF(ANY(too_many_tries)) THEN ! TODO Redo it to work with MPI
        !If at least one search failed to find a new point, a cluster analysis will be performed after adding the points from the successful searches if clustering is used. Otherwise, the run will end.
#ifdef OPENMPI_ON
        ! Signal final data MAXED_OUT
        CALL MPI_Send(info_string, 256, MPI_CHARACTER, 0, MPI_TAG_SEARCH_DONE_MANY_TRIES, mpi_child_writter_comm, mpi_ierror)
#endif
        IF (make_cluster) THEN
           IF(n_call_cluster_it>=n_call_cluster_it_max) THEN
              CALL LOG_ERROR_HEADER()
              CALL LOG_ERROR('Too many cluster analysis for an iteration.')
              CALL LOG_ERROR('Change cluster recognition parameters.')
              CALL LOG_ERROR_HEADER()
              CALL HALT_EXECUTION()
           END IF
           IF(n_call_cluster>=n_call_cluster_max) THEN
              CALL LOG_ERROR_HEADER()
              CALL LOG_ERROR('Too many cluster analysis.')
              CALL LOG_ERROR('Change cluster recognition parameters.')
              CALL LOG_ERROR_HEADER()
              CALL HALT_EXECUTION()
           END IF
           make_cluster_internal=.true.
           need_cluster=.true.
        ELSE
           CALL LOG_WARNING_HEADER()
           CALL LOG_WARNING('Too many tries to find new live points for try n.: '//TRIM(ADJUSTL(INT_TO_STR_INLINE(itry))))
           CALL LOG_WARNING('More than '//TRIM(ADJUSTL(INT_TO_STR_INLINE(maxtries*maxntries))))
           CALL LOG_WARNING('We take the data as they are :-~')
           CALL LOG_WARNING_HEADER()
           early_exit = .true.
           nstep_final = n - 1
        END IF
     END IF
     
     ! -------------------------------------!
     !     Subloop for threads starts       !
     ! -------------------------------------!  

     
     subloop: DO it = 1, nth

        IF (live_ready(it)) THEN

           ! If the parallely computed live point is still good, take it for loop calculation.
           ! Otherwise skip it
           IF ((live_like_new(it).GT.min_live_like)) THEN
              n = n + 1
              n_call_cluster_it=0
              n_mat_cov=n_mat_cov+1
              IF (n.GT.nstep) THEN
                 ! If the number of steps is reached, we stop the loop
                 normal_exit = .true.
                 nstep_final = n - 1
              END IF
           ELSE
              CYCLE subloop
           ENDIF
           
           
           ! Calculate steps, mass and rest each time
           ! trapezoidal rule applied here (Skilling Entropy 2006)
           !tlnmass(n) = DLOG(-(tstep(n+1)-tstep(n-1))/2.d0)
           !tlnrest(n) = DLOG((tstep(n+1)+tstep(n-1))/2.d0)
           ! Alternative calculation
           lntmass = - (n-1.d0)/nlive + constm
           lntrest = - (n-1.d0)/nlive + constp
           ! Check
           !write(*,*) n, tlnrest(n), lntrest, tlnrest(n) - lntrest
           !write(*,*) n, tlnmass(n), lntmass, tlnmass(n) - lntmass
           !pause
           
           ! Reorder found point (no parallel here) and make the required calculation for the evidence
           ! Reorder point
           ! Order and exclude last point
           jlim=0
           IF (live_like_new(it).GT.live_like(nlive)) THEN
              jlim = nlive
           ELSE
              DO j=1,nlive-1
                 IF (live_like_new(it).GT.live_like(j).AND.live_like_new(it).LE.live_like(j+1)) THEN
                    jlim = j
                    EXIT
                 END IF
              END DO
           END IF
           
           ! Store old values
           live_like_old(n) = live_like(1)
           live_old(n,:) = live(1,:)
           live_birth_old(n) = live_birth(1)
           live_rank_old(n) = live_rank(1)
           
           ! Insert the new one
           IF (jlim.LT.1.OR.jlim.GT.nlive) THEN
              CALL LOG_ERROR_HEADER()
              CALL LOG_ERROR('Problem in the search method, or in the calculations...')
              CALL LOG_ERROR('No improvement in the likelihood value after finding the new point...')
              CALL LOG_ERROR('j = '//TRIM(ADJUSTL(INT_TO_STR_INLINE(jlim)))//'old min like = '//TRIM(ADJUSTL(REAL_TO_STR_INLINE(min_live_like)))//'new min like = '//TRIM(ADJUSTL(REAL_TO_STR_INLINE(live_like_new(it)))))
              CALL LOG_ERROR_HEADER()
              CALL HALT_EXECUTION()
           ELSE IF (jlim.EQ.1) THEN
              live_like(1) = live_like_new(it)
              live(1,:) = live_new(it,:)
              live_birth(1) = live_like_old(n)
              live_rank(1) = 1
           ELSE
              ! Shift values
              live_like(1:jlim-1) =  live_like(2:jlim)
              live(1:jlim-1,:) =  live(2:jlim,:)
              live_birth(1:jlim-1) =  live_birth(2:jlim)
              live_rank(1:jlim-1) =  live_rank(2:jlim)
              ! Insert new value
              live_like(jlim) =  live_like_new(it)
              live(jlim,:) =  live_new(it,:)
              live_birth(jlim) = live_like_old(n)
              live_rank(jlim) = jlim
              ! The rest stay as it is
           END IF
           
           ! Present minimal value of the likelihood
           min_live_like = live_like(1)
           
           
           SELECT CASE (searchid)
           CASE (2,3,4)
              IF(make_cluster) THEN
                 IF(MOD(n,10*nlive).EQ.0 .AND. n .NE. 0) THEN
                    make_cluster_internal=.true.
                 END IF
              END IF
           END SELECT
           
           ! Assign to the new point, the same cluster number of the start point
           IF (cluster_on) THEN
              ! Instert new point
              p_cluster(1:jlim-1) =  p_cluster(2:jlim)
              p_cluster(jlim) = icluster(it)
              cluster_np(icluster(it)) = cluster_np(icluster(it)) + 1
              ! Take out old point
              icluster_old = p_cluster(1)
              cluster_np(icluster_old) = cluster_np(icluster_old) - 1
              ! Call cluster module to recalculate the std of the considered cluster and the cluster of the discarted point
              CALL REMAKE_CLUSTER_STD(live,icluster(it),icluster_old)
           END IF
           
           IF(conv_method .EQ. 'LIKE_ACC') THEN
              ! Calculate the evidence for this step
              evstep(n) = live_like_old(n) + lntmass
              
              ! Sum the evidences
              evsum = ADDLOG(evsum,evstep(n))
              
              ! Check if the estimate accuracy is reached
              evrestest = live_like(nlive) + lntrest
              evtotest = ADDLOG(evsum,evrestest)
           ELSE IF(conv_method .EQ. 'ENERGY_ACC') THEN
              ! Calculate the contribution to the partition function at that temperature for this step
              evstep(n) = 1./conv_par*live_like_old(n) + lntmass
              
              ! Sum the contribution with the previous contributions
              evsum = ADDLOG(evsum,evstep(n))
              
              ! Check if the estimate accuracy is reached
              evrestest = 1./conv_par*live_like(nlive) + lntrest
              evtotest = ADDLOG(evsum,evrestest)
           ELSE IF(conv_method .EQ. 'ENERGY_MAX') THEN
              ! Calculate the contribution to the partition function at that temperature for this step
              evstep(n) = 1./conv_par*live_like_old(n) + lntmass
              
              ! Max between the present contribution and previous ones
              evsum = MAX(evsum,evstep(n))
              
              ! Check if the estimate accuracy is reached
              evtotest = evstep(n)
           ELSE
              CALL LOG_ERROR_HEADER()
              CALL LOG_ERROR('Invalid convergence method.')
              CALL LOG_ERROR('Available options: [LIKE_ACC, ENERGY_ACC, ENERGY_MAX].')
              CALL LOG_ERROR_HEADER()
           END IF
           
           
           ! Check if the estimate accuracy is reached
           IF (evtotest-evsum.LT.evaccuracy) THEN
              ! If the estimate accuracy is reached, we stop the loop
              normal_exit = .true.
              nstep_final = n
           END IF

         ! DEGUGGING printint ???????????????????????????????????????????????????????????????????????????????????????
         ! Write status
         !  write(*,*) 'N step: ', n, 'iteration: ', it, '/', nth, 'Ev. : ', evsum ,  'Acc: ', evtotest-evsum, normal_exit! ???? Debugging
         !  write(*,*) live_like_old(n), live_birth_old(n), live_rank_old(n), min_live_like, evsum, evstep(n), evtotest-evsum!????

         ! Store actual live points for debugging ????????????????????????????????????
         !   OPEN(22,FILE='nf_output_debug_live_points',STATUS= 'UNKNOWN')
         !   WRITE(22,*) '# n     lnlikelihood     parameters'
         !   DO l=1,nlive
         !      WRITE(22,*) l, live_like(l), live(l,:)
         !   END DO
         !   CLOSE(22)
         !   pause

         !   WRITE(*,*) '# n     lnlikelihood     parameters'
         !   DO l=1,nlive
         !      WRITE(*,*) l, live_like(l), live(l,:)
         !   END DO
           
         ! Write temporal results in a file  for debugging ????????????????????????????????
         !   OPEN(33,FILE='nf_output_debug_status.dat',STATUS= 'UNKNOWN')
         !   WRITE(33,*) 'New last live point : ', live_like(1), live(1,:)
         !   WRITE(33,*) 'New first live point : ', live_like(nlive), live(nlive,:)
         !   WRITE(33,*) 'N step: ', n, 'Ev. at present: ',evsum, &
         !        'Ev. of the step: ', evstep(n), 'Diff. with the estimate total ev.: ', evtotest-evsum
         !   CLOSE(33)

         !   WRITE(*,*) 'New last live point : ', live_like(1), live(1,:)
         !   WRITE(*,*) 'N step: ', n, 'Ev. at present: ',evsum, &
         !        'Ev. of the step: ', evstep(n), 'Diff. with the estimate total ev.: ', evtotest-evsum

           IF(live_like(nlive).EQ.0) THEN 
              WRITE(*,*) 'Problem, something is wrong!!! live_like(nlive): ', live_like(nlive) !???? debugging
              STOP
           END IF
           ! ????????????????????????????????????????????????????????????????????????????????????????????????????????


           ! Write the information to the file
           IF (opt_suppress_output) CYCLE
           IF (opt_lib_output) THEN
              ! Calculate the average efficiency of the search
              moving_eff_avg = REAL(search_par2/ntries(it),8)
              WRITE(*,*) 'LO | ', itry+1, n, min_live_like, evsum, evstep(n), evtotest-evsum, moving_eff_avg, npar, par_name, live(nlive, :)
           ENDIF
           
           ! Write the information to the file or send it to the MPI writter process
         IF (MOD(n,100).EQ.0) THEN
              ! Calculate the average efficiency of the search
              moving_eff_avg = REAL(search_par2/ntries(it),8)
#ifdef OPENMPI_ON
              IF(opt_compact_output) THEN
                 WRITE(info_string,20) itry+1, n, min_live_like, evsum, evstep(n), evtotest-evsum, &
                      moving_eff_avg
              ELSE
                 WRITE(info_string,21) processor_name, itry+1, n, min_live_like, evsum, evstep(n), evtotest-evsum, &
                      moving_eff_avg
              ENDIF
20            FORMAT('| N: ', I2, ' | S: ', I8, ' | MLL: ', F20.12, ' | E: ', F20.12, &
                   ' | Es: ', F20.12, ' | Ea: ', ES14.7, ' | Ae: ', F6.4, ' |')
21            FORMAT('| Machine: ', A10, ' | N. try: ', I2, ' | N. step: ', I10, ' | Min. loglike: ', F23.15, ' | Evidence: ', F23.15, &
                   ' | Ev. step: ', F23.15, ' | Ev. pres. acc.: ', ES14.7, ' | Avg eff.: ', F6.4, ' |')
              CALL MPI_Send(info_string, 256, MPI_CHARACTER, 0, MPI_TAG_SEARCH_STATUS, mpi_child_writter_comm, mpi_ierror)
#else
              IF(opt_suppress_output) CYCLE
              IF(opt_compact_output) THEN
                 WRITE(info_string,22) itry, n, min_live_like, evsum, evstep(n), evtotest-evsum, moving_eff_avg
22               FORMAT('| N: ', I2, ' | S: ', I8, ' | MLL: ', F20.12, ' | E: ', F20.12, &
                      ' | Es: ', F20.12, ' | Ea: ', ES14.7, ' | Te: ', F6.4, ' |')
                 WRITE(*,24) info_string
24               FORMAT(A150)
              ELSE IF(opt_lib_output) THEN
                 WRITE(*,*) 'LO | ', itry, n, min_live_like, evsum, evstep(n), evtotest-evsum, moving_eff_avg, npar, par_name, live(nlive, :)
              ELSE
                 WRITE(info_string,23) itry, n, min_live_like, evsum, evstep(n), evtotest-evsum, moving_eff_avg
23               FORMAT('| N. try: ', I2, ' | N. step: ', I10, ' | Min. loglike: ', F23.15, ' | Evidence: ', F23.15, &
                      ' | Ev. step: ', F23.15, ' | Ev. pres. acc.: ', ES14.7, ' | Typical eff.: ', F6.4, ' |')
                 WRITE(*,25) info_string
25               FORMAT(A220)
              ENDIF
#endif
           ENDIF

           ! Reset the number of tries for this iteration
           ntries = 0
           
           ! Reset the cluster analysis if needed
           make_cluster_internal=.false.
           need_cluster=.false.
           
           ! Reset the number of tries for each thread
           too_many_tries = .false.
           
           
           live_like_new(it) =  min_live_like ! Reset the new live point with the minimum likelihood value
           live_ready(it) = .false.
        ENDIF
     END DO subloop
     
     ! -------------------------------------!
     !     Subloop for threads stop         !
     ! -------------------------------------!   
     
     ! Remake calculation of mean and standard deviation live points
     IF (.NOT.cluster_on) CALL REMAKE_LIVE_MEAN_SD(live) 

     
     ! Worker threads: search for new points ------------------------------------------------------------------------------------------------------------------
     DO it=1,nth
        IF(.NOT.live_ready(it).AND..NOT.live_searching(it)) THEN
           live_searching(it) = .true.
           !$OMP TASK  DEFAULT(NONE) FIRSTPRIVATE(it) PRIVATE(p_cluster) &
           !$OMP& SHARED(n,itry,ntries,min_live_like,live_like,live,nth,live_like_new,live_new,icluster,too_many_tries,live_searching,live_ready)
           CALL SEARCH_NEW_POINT(n,itry,min_live_like,live_like,live, &
                live_like_new(it),live_new(it,:),icluster(it),ntries(it),too_many_tries(it))
           ! Debugging stuff ????
           write(*,*) 'IN NS thread ', omp_get_thread_num(), 'it: ', it, 'new live_like ', live_like_new(it), min_live_like, too_many_tries(it)
           live_ready(it) = .true.
           live_searching(it) = .false.
           !$OMP FLUSH(live_ready, live_searching, live_like_new,live_new)
           !$OMP END TASK
        END IF
     END DO

     ! -----------------------------------------------------------------------------------------------------------------
     
     
     ! ---------------------------------------------------------------------------------------!
     !                                                                                        !
     !                                 STOP THE MAIN LOOP                                     !
     !                                                                                        !
     !----------------------------------------------------------------------------------------!
  END DO main_loop
  
  !$OMP END SINGLE
  !$OMP END PARALLEL
  !#########################################################################################################################################################

  
#ifdef OPENMPI_ON
  IF((evtotest-evsum).GE.evaccuracy) THEN
     ! Signal final data MAXED_OUT
     CALL MPI_Send(info_string, 256, MPI_CHARACTER, 0, MPI_TAG_SEARCH_ERROR_MAXED_OUT, mpi_child_writter_comm, mpi_ierror)
  ELSE
     ! Signal final data OK
     CALL MPI_Send(info_string, 256, MPI_CHARACTER, 0, MPI_TAG_SEARCH_DONE_OK, mpi_child_writter_comm, mpi_ierror)
  END IF
#endif


  !------------ Calculate the total evidence with the last live points ---------------------
#ifdef OPENMPI_ON
  CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_ierror)
#endif

  IF(((evtotest-evsum).GE.evaccuracy).AND.(n.GE.nstep)) THEN
   CALL HALT_EXECUTION()
  ENDIF

  ! Store the last live points
  OPEN(99,FILE='nf_output_last_live_points.dat',STATUS= 'UNKNOWN')
  WRITE(99,*) '# n step =',  n-1
  WRITE(99,*) '# Evidence of the step =', evstep(n-1)
  IF(conv_method .EQ. 'LIKE_ACC') THEN
    WRITE(99,*) '# Evidence accuracy =',  ADDLOG(evsum,live_like(nlive) + lntrest) - evsum
  ELSE IF(conv_method .EQ. 'ENERGY_ACC') THEN
    WRITE(99,*) '# Evidence accuracy =',  ADDLOG(evsum,1./conv_par*live_like(nlive) + lntrest) - evsum
  ELSE IF(conv_method .EQ. 'ENERGY_MAX') THEN
    WRITE(99,*) '# Evidence accuracy =',  1./conv_par*live_like(nlive) + lntmass - evsum
  ELSE
   CALL LOG_ERROR_HEADER()
   CALL LOG_ERROR('Invalid convergence method.')
   CALL LOG_ERROR('Available options: [LIKE_ACC, ENERGY_ACC, ENERGY_MAX].')
   CALL LOG_ERROR_HEADER()
  END IF
  WRITE(99,*) '# n     lnlikelihood     parameters    birth_lnlike   rank'
  DO j=1,nlive
     WRITE(99,*) j, live_like(j), live(j,:), live_birth(j), live_rank(j)
  END DO
  CLOSE(99)

  ! Calculate the last evidence
  IF(conv_method .EQ. 'LIKE_ACC') THEN
    ! Sum the last loglikelihood  (considering that we are dealing with logs)
    last_likes = live_like(1)
    DO j=2,nlive
       last_likes = ADDLOG(last_likes,live_like(j))
    END DO
    ! Arithmetic average value of the loglikelihood
    live_like_last = last_likes - DLOG(DFLOAT(nlive))

    ! Evidence of each last points assuming equal volume spacing (EXP(tlnrest)/nlive)
    evlast = live_like_last + lntrest - DLOG(DFLOAT(nlive))

    ! The final evidence !!!
    evrest_last = live_like_last + lntrest
    evsum_final = ADDLOG(evsum,evrest_last)
  ELSE IF(conv_method .EQ. 'ENERGY_ACC') THEN
    ! Sum the last energies  (considering that we are dealing with logs)
    last_likes = live_like(1)
    DO j=2,nlive
       last_likes = last_likes+live_like(j)
    END DO
    ! Average value of the energies
    live_like_last = last_likes/DFLOAT(nlive)

    ! Contribution of each last points assuming equal volume spacing (EXP(tlnrest)/nlive)
    evlast = 1./conv_par*live_like_last + lntrest - DLOG(DFLOAT(nlive))

    ! The final partion function !!!
    evrest_last = 1./conv_par*live_like_last + lntrest
    evsum_final = ADDLOG(evsum,evrest_last)
  ELSE IF(conv_method .EQ. 'ENERGY_MAX') THEN
    ! Sum the last energies  (considering that we are dealing with logs)
    last_likes = live_like(1)
    DO j=2,nlive
       last_likes = last_likes+live_like(j)
    END DO
    ! Average value of the energies
    live_like_last = last_likes/DFLOAT(nlive)

    ! Contribution of each last points assuming equal volume spacing (EXP(tlnrest)/nlive)
    evlast = 1./conv_par*live_like_last + lntrest - DLOG(DFLOAT(nlive))

    ! The maximal contribution !!!
    evrest_last = 1./conv_par*live_like_last + lntrest
    evsum_final = MAX(evsum,evrest_last)
  ELSE
   CALL LOG_ERROR_HEADER()
   CALL LOG_ERROR('Invalid convergence method.')
   CALL LOG_ERROR('Available options: [LIKE_ACC, ENERGY_ACC, ENERGY_MAX].')
   CALL LOG_ERROR_HEADER()
  END IF
  
  ! Insert the last live points in the ensemble
  live_old(nstep_final+1:nstep_final+nlive,:) = live(:,:)
  live_like_old(nstep_final+1:nstep_final+nlive) = live_like_last
  live_birth_old(nstep_final+1:nstep_final+nlive) = live_birth(:)
  live_rank_old(nstep_final+1:nstep_final+nlive) = live_rank(:)
  evstep(nstep_final+1:nstep_final+nlive) = evlast

  ! Total output of the routine (plis evsum_final)
  live_final = live_old
  live_like_final = live_like_old
  live_birth_final = live_birth_old
  live_rank_final = live_rank_old

  nall = nstep_final + nlive

  ! Deallocate variables for cluster analysis
  IF (cluster_on) THEN
     ! Reset cluster analysis
     cluster_on = .FALSE.
     CALL DEALLOCATE_CLUSTER()
  END IF

  ! Deallocate parallel stuff
  DEALLOCATE(live_like_new,live_new,too_many_tries,icluster)
  ! Deallocate variables for search new live points
  CALL DEALLOCATE_SEARCH_NEW_POINTS()


  !------------ Calculate weights and parameter best values and distributions ----------------

  ! Save the point with the highest likelihood
  live_max = live(nlive,:)
  live_like_max = live_like(nlive)

  ! Calculate the weights with test of overflow
  weight = 0.d0
  DO n=1,nstep_final
     IF(DABS(evstep(n) - evsum_final).LT.700) weight(n) = DEXP(evstep(n) - evsum_final)
  END DO

!1000 FORMAT (A12,I1,A4)
!2000 FORMAT (A7,I1,A4)
!3000 FORMAT (A8,I1,A4)
!4000 FORMAT (A,I3,A,I3,A,ES12.6,A,ES12.6,A,ES12.6,A,ES12.6,A,ES10.2)

  RETURN

END SUBROUTINE NESTED_SAMPLING

!#############################################################################################


! ___________________________________________________________________________________________

FUNCTION ADDLOG(log1,log2)
  IMPLICIT NONE
  REAL(8) :: log1, log2, ADDLOG

  ADDLOG = 0.
  ! Sum of two numbers in logarithmic notation with check of over and underflow

  IF (log1.GT.log2) THEN
     IF((log1-log2).LT.700) THEN
        ADDLOG = log1 + DLOG(1.d0 + DEXP(log2-log1))
     ELSE
        ADDLOG = log1
     END IF
  ELSE
     IF((log2-log1).LT.700) THEN
        ADDLOG = log2 + DLOG(1.d0 + DEXP(log1-log2))
     ELSE
        ADDLOG = log2
     END IF
  END IF

END FUNCTION ADDLOG

