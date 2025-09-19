SUBROUTINE NESTED_SAMPLING(itry,maxstep,nall,evsum_final,live_like_final,live_birth_final,live_rank_final,&
   weight,live_final,live_like_max,live_max,mpi_rank,mpi_cluster_size)
  ! Time-stamp: <Last changed by martino on Thursday 18 September 2025 at CEST 10:03:48>

  ! Additional math module
  USE MOD_MATH

  ! Parameter module
  USE MOD_PARAMETERS, ONLY:  npar, nlive, conv_method, evaccuracy, conv_par, &
        search_par2, par_in, par_step, par_bnd1, par_bnd2, par_fix, par_name, &
        make_cluster, nth, maxtries, maxntries, searchid
  ! Module for likelihood
  USE MOD_LIKELIHOOD_GEN
  ! Module for searching new live points
  USE MOD_SEARCH_NEW_POINT, ONLY: SEARCH_NEW_POINT, MAKE_PRELIM_CALC, REMAKE_CALC, DEALLOCATE_SEARCH_NEW_POINTS
  ! Module for cluster analysis
  USE MOD_CLUSTER_ANALYSIS, ONLY: cluster_on, make_cluster_internal, p_cluster, cluster_np, DEALLOCATE_CLUSTER, MAKE_CLUSTER_ANALYSIS, GET_CLUSTER_MEAN_SD, WRITE_CLUSTER_DATA
  ! Module for optionals
  USE MOD_OPTIONS
  ! Module for logging
  USE MOD_LOGGER
  ! Module for proffiling
  USE MOD_PERFPROF

!#ifdef OPENMP_ON
  USE OMP_LIB
!#endif

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
  REAL(8) :: moving_eff_avg = 0.
  CHARACTER :: info_string*4096
  CHARACTER :: fparname_fmt*32
  CHARACTER :: fparval_fmt*32
  INTEGER(4) :: n_call_cluster, n_call_cluster_it, n_calc
  INTEGER(4), PARAMETER :: n_call_cluster_it_max=10, n_call_cluster_max=100

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
  n_calc=0
  !maxtries = 50*njump ! Accept an efficiency of more than 2% for the exploration, otherwise change something

  ALLOCATE(ntries(nth),live_like_new(nth),live_new(nth,npar),too_many_tries(nth),icluster(nth),live_searching(nth),live_ready(nth))
  ntries = 0
  live_new = 0.
  live_like_new = 0.
  icluster = 0
  too_many_tries = .false.
  live_searching = .false.
  live_ready = .false.
  normal_exit = .false. 
  early_exit = .false.

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

  ! Calculate average and standard deviation, covariance, etc. of the parameters
  CALL MAKE_PRELIM_CALC(live)

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


  ! Present minimal value of the likelihood
  min_live_like = live_like(1)

  ! Degugging ???
  !!$ print *, "Max threads:", nth 
  !!$ print *, "Num threads in parallel region:"
  !!$omp parallel
  !!$ print *, "Thread", omp_get_thread_num(), "active in parallel region"
  !!$omp end parallel

  ! Main parallel region
  !#########################################################################################################################################################  
  !$OMP PARALLEL  num_threads(nth)
  !$OMP SINGLE
  

  ! ---------------------------------------------------------------------------------------!
  !                                                                                        !
  !                                 START THE MAIN LOOP                                    !
  !                                                                                        !
  !----------------------------------------------------------------------------------------!
  n = 1

  main_loop: DO WHILE ((.NOT. normal_exit) .AND. (.NOT. early_exit))


      ! Start live points search in parallel -------------------------------------------------------------------------------------------------
      it = 1
      !!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(it) &
      !!$OMP SHARED(live_searching,live_ready,n,itry,ntries,min_live_like,live_like,live,nth,live_like_new,live_new,icluster,too_many_tries)  
      DO it=1,nth
         IF(.NOT.live_ready(it).AND..NOT.live_searching(it)) THEN
            live_searching(it) = .true.
            !$OMP TASK DEFAULT(NONE) FIRSTPRIVATE(it) &
            !$OMP SHARED(live_searching,live_ready,n,itry,ntries,min_live_like,live_like,live,nth,live_like_new,live_new,icluster,too_many_tries) 
            !write(*,*) 'Thread: ', omp_get_thread_num(), 'it: ', it,  'live_ready(it): ', live_ready(it), 'in search loop'  ! Debugging ???
            CALL SEARCH_NEW_POINT(n,itry,min_live_like,live_like,live, &
            live_like_new(it),live_new(it,:),icluster(it),ntries(it),too_many_tries(it))
            live_searching(it) = .false.
            live_ready(it) = .true.
            !$OMP FLUSH(live_ready, live_searching, live_like_new,live_new, icluster, too_many_tries)
            !$OMP END TASK
            END IF
      END DO
      !$OMP TASKWAIT

      !!$OMP END PARALLEL DO
      ! --------------------------------------------------------------------------------------------------------------------------------------

      !$OMP TASK PRIVATE(it) &
      !$OMP SHARED(live_searching,live_ready,n,itry,ntries,min_live_like,live_like,live,nth,live_like_new,live_new,icluster,too_many_tries) 
      


     !write(*,*) live_ready, 'at the start of the main loop'  ! Debugging ???
     
     ! Initial checks and operations 

     ! Force clustering if slide sampling is selected
     IF (make_cluster) THEN
        SELECT CASE (searchid)
        CASE (2,3,4)
           IF(MOD(n,10*nlive).EQ.0 .AND. n .NE. 0) THEN
              make_cluster_internal=.true.
           END IF
        END SELECT
     END IF
     
     ! Check if too many tries to find a new point  
     IF(ANY(too_many_tries)) THEN
        !If all searches failed to find a new point, a cluster analysis will be performed. Otherwise, the run will end.
        IF (make_cluster) THEN
           make_cluster_internal=.true.
           ! Check if too many cluster analysis for an iteration or in total
           IF(n_call_cluster_it>=n_call_cluster_it_max) THEN
              CALL LOG_ERROR_HEADER()
              CALL LOG_ERROR('Too many cluster analysis for an iteration.')
              CALL LOG_ERROR('Change cluster recognition parameters.')
              CALL LOG_ERROR_HEADER()
              CALL HALT_EXECUTION()
           END IF
           ! .... or in total
           IF(n_call_cluster>=n_call_cluster_max) THEN
              CALL LOG_ERROR_HEADER()
              CALL LOG_ERROR('Too many cluster analysis.')
              CALL LOG_ERROR('Change cluster recognition parameters.')
              CALL LOG_ERROR_HEADER()
              CALL HALT_EXECUTION()
           END IF
        ELSE
           CALL LOG_WARNING_HEADER()
           CALL LOG_WARNING('Too many tries to find new live points for try n.: '//TRIM(ADJUSTL(INT_TO_STR_INLINE(itry))))
           CALL LOG_WARNING('More than '//TRIM(ADJUSTL(INT_TO_STR_INLINE(maxtries))))
           CALL LOG_WARNING('We take the data as they are :-~')
           CALL LOG_WARNING_HEADER()
           early_exit = .true.
           nstep_final = n - 1
        END IF
     END IF
     
     ! Exit already now from the main loop if needed
     !IF (normal_exit .OR. early_exit) EXIT main_loop

      
      IF(make_cluster_internal) THEN
         CALL LOG_MESSAGE('Performing cluster analysis. Number of analyses = '//TRIM(ADJUSTL(INT_TO_STR_INLINE(n_call_cluster+1))))
         CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
         cluster_on = .true.
         CALL REMAKE_CALC(live)
         make_cluster_internal = .false.
         n_call_cluster_it = n_call_cluster_it+1
         n_call_cluster = n_call_cluster+1
         n_calc=0
      END IF
      
      ! If more than 5% of the points have changed, calculate 
      ! the standard deviations, the covariance matrix and Cholesky decomposition again
      IF(n_calc .GT. 0.05*nlive) THEN 
         CALL REMAKE_CALC(live)
         n_calc=0
      END IF
      ! --------------------------------------------------------------------------------------------------------------------------------------


        
      ! -------------------------------------!
      !     Subloop for threads starts       !
      ! -------------------------------------!      
      
      
      subloop: DO it = 1, nth ! Start of the main nested sampling loop -----------------------------------------------------------------------

          ! Check first if the thread has finished and if it is good candidate
         IF (live_ready(it)) THEN
            
            ! If the parallely computed live point is still good, take it for loop calculation.
            ! Otherwise skip it
            IF ((live_like_new(it).GT.min_live_like)) THEN
               n = n + 1
               n_call_cluster_it=0
               n_calc=n_calc+1
            ELSE
               GOTO 800
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
               search_loop: DO j=1,nlive-1
                  IF (live_like_new(it).GT.live_like(j).AND.live_like_new(it).LE.live_like(j+1)) THEN
                     jlim = j
                     EXIT search_loop
                  END IF
               END DO search_loop
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
            
            ! Assign to the new point, the same cluster number of the start point
            IF (cluster_on) THEN
               ! Instert new point
               p_cluster(1:jlim-1) =  p_cluster(2:jlim)
               p_cluster(jlim) = icluster(it)
               cluster_np(icluster(it)) = cluster_np(icluster(it)) + 1
               ! Take out old point
               icluster_old = p_cluster(1)
               cluster_np(icluster_old) = cluster_np(icluster_old) - 1
               !   ! If the old cluster is now empty, we need to do a cluster analysis NOT WORKING!!
               !   IF (cluster_np(icluster_old).EQ.0) THEN
               !      CALL LOG_MESSAGE('Performing cluster analysis. Number of analyses = '//TRIM(ADJUSTL(INT_TO_STR_INLINE(n_call_cluster+1))))
               !      CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
               !      CALL REMAKE_CALC(live)
               !      make_cluster_internal = .false.
               !      n_call_cluster_it = n_call_cluster_it+1
               !      n_call_cluster = n_call_cluster+1
               !      n_calc=0
               !   END IF
               !   ! Call cluster module to recalculate the std of the considered cluster and the cluster of the discarted point
               !   CALL REMAKE_CLUSTER_STD(live,icluster(it),icluster_old)
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
            
            ! Write status
            !write(*,*) 'N step : ', n, 'Evidence at present : ', evsum ! ???? Debugging
            !write(*,*) n, live_like_old(n), live_birth_old(n), live_rank_old(n) !????
            
            ! Check if the estimate accuracy is reached
            IF (evtotest-evsum.LT.evaccuracy) normal_exit = .true.
            
            !   moving_eff_avg = MOVING_AVG(search_par2/ntries(it))
            IF (MOD(n,100).EQ.0) THEN
               IF(opt_suppress_output) CYCLE
               moving_eff_avg = REAL(search_par2/ntries(it),8)
               IF(opt_compact_output) THEN
                  WRITE(info_string,22) itry, n, min_live_like, evsum, evstep(n), evtotest-evsum, moving_eff_avg
22                FORMAT('| N: ', I2, ' | S: ', I8, ' | MLL: ', F20.12, ' | E: ', F20.12, &
                       ' | Es: ', F20.12, ' | Ea: ', ES14.7, ' | Te: ', F6.4, ' |')
                  WRITE(*,24) info_string
24                FORMAT(A150)
               ELSE IF(opt_lib_output) THEN
                  WRITE(*,*) 'LO | ', itry, n, min_live_like, evsum, evstep(n), evtotest-evsum, moving_eff_avg, npar, par_name, live(nlive, :)
               ELSE
                  WRITE(info_string,23) itry, n, min_live_like, evsum, evstep(n), evtotest-evsum, moving_eff_avg
23                FORMAT('| N. try: ', I2, ' | N. step: ', I10, ' | Min. loglike: ', F23.15, ' | Evidence: ', F23.15, &
                       ' | Ev. step: ', F23.15, ' | Ev. pres. acc.: ', ES14.7, ' | Typical eff.: ', F6.4, ' |')
                  WRITE(*,25) info_string
25                FORMAT(A220)
               ENDIF
            ENDIF
            
            ! If the number of steps is reached, we stop the loop
            IF (n.GE.nstep) THEN
               CALL LOG_MESSAGE('Maximum number of iteraction reached  = '//TRIM(ADJUSTL(INT_TO_STR_INLINE(nstep))))
               CALL LOG_MESSAGE('Exiting from the main nested sampling loop  = '//TRIM(ADJUSTL(INT_TO_STR_INLINE(nstep))))
               normal_exit = .true.
               nstep_final = n
            END IF
            
         END IF 

         ! Make the live point ready for a new search
   800   live_ready(it) = .false.
         !write(*,*) 'it: ', it,  'live_ready(it): ', live_ready(it), 'end of subloop'  ! Debugging ???

      END DO subloop ! End of the main nested sampling loop
      
      ! Final operations
      
      ! Remake calculation of mean and standard deviation live points
      CALL REMAKE_CALC(live)
      
      
      ! Reset the number of tries for this iteration
      ntries = 0

      !$OMP END TASK ! ------------------------------------------------------------------------------------------------------------------------------------

      
  
   END DO main_loop ! End of the main loop
  ! ---------------------------------------------------------------------------------------!
  !                                                                                        !
  !                                 STOP THE MAIN LOOP                                     !
  !                                                                                        !
  !----------------------------------------------------------------------------------------!

  !$OMP END SINGLE
  !$OMP END PARALLEL
  !#########################################################################################################################################################

301 CONTINUE

  ! Store the number of steps
  nstep_final = n

  !------------ Calculate the total evidence with the last live points ---------------------
601 CONTINUE

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

  ! Make last calculations and deallocate variables for cluster analysis
  IF (cluster_on) THEN
     ! Calculate the final clusters
     CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
     ! Write cluster max information
     CALL WRITE_CLUSTER_DATA(live,live_like)
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

! ! ___________________________________________________________________________________________

! FUNCTION MOVING_AVG(val)
!    USE MOD_MATH
!    IMPLICIT NONE
!    REAL(8) :: val, MOVING_AVG

!    MOVING_AVG = 0.

!    window_array = CSHIFT(window_array, 1)
!    window_array(moving_avg_window) = val

!    MOVING_AVG = SUM(window_array) / moving_avg_window
! END FUNCTION MOVING_AVG
 
!  ! ___________________________________________________________________________________________
