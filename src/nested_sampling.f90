SUBROUTINE NESTED_SAMPLING(itry,maxstep,nall,evsum_final,live_like_final,weight,&
     live_final,live_like_max,live_max)
  ! Time-stamp: <Last changed by martino on Saturday 23 July 2022 at CEST 15:10:10>
  ! For parallel tests only
  !SUBROUTINE NESTED_SAMPLING(irnmax,rng,itry,ndata,x,nc,funcname,&
  !   npar,par_fix,par_step,par_in,par_bnd1,par_bnd2,nlive,evaccuracy,sdfraction,&
  !   nall,evsum_final,live_like_final,weight,live_final,live_like_max,live_max)
  USE OMP_LIB
  !USE RNG

  ! Parameter module
  USE MOD_PARAMETERS, ONLY:  nlive, evaccuracy, search_par2, par_in, par_step, par_bnd1, par_bnd2, par_fix, nth
  ! Module for likelihood
  USE MOD_LIKELIHOOD
  ! Module for searching new live points
  USE MOD_SEARCH_NEW_POINT
  ! Module for cluster analysis
  USE MOD_CLUSTER_ANALYSIS, ONLY: cluster_on

  !
  IMPLICIT NONE
  INTEGER(4), INTENT(IN) :: itry, maxstep
  INTEGER(4), INTENT(OUT) :: nall
  REAL(8), INTENT(OUT) :: evsum_final, live_like_max
  REAL(8), INTENT(OUT), DIMENSION(npar) :: live_max
  REAL(8), INTENT(OUT), DIMENSION(maxstep) :: weight
  REAL(8), INTENT(OUT), DIMENSION(maxstep) :: live_like_final
  REAL(8), INTENT(OUT), DIMENSION(maxstep,npar) :: live_final
  ! Random number variables
  !INTEGER, INTENT(IN) :: irnmax
  !REAL(8), INTENT(IN), DIMENSION(irnmax) :: rng
  ! Prior variables
  REAL(8), DIMENSION(npar) :: par_prior
  ! Loop variables
  INTEGER(4) :: nstep = 2
  REAL(8), DIMENSION(maxstep) :: tstep, tlnmass, tlnrest, evstep
  REAL(8), DIMENSION(maxstep) :: live_like_old
  REAL(8), DIMENSION(maxstep,npar) :: live_old
  REAL(8) :: evsum = 0., evrestest = 0., evtotest = 0.
  ! Search variable to monitor efficiency
  INTEGER(4) :: ntries=0
  ! Live points variables parallel
  REAL(8), ALLOCATABLE, DIMENSION(:) :: live_like_new
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_new
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: too_many_tries
  INTEGER(4), ALLOCATABLE, DIMENSION(:) :: icluster
  REAL(8) :: min_live_like = 0.
  REAL(8), DIMENSION(nlive) :: live_like
  REAL(8), DIMENSION(nlive,npar) :: live
  INTEGER(4) :: icluster_old=0
  ! Final calculations
  INTEGER(4) :: nstep_final
  REAL(8) :: last_likes, live_like_last, evrest_last, evlast
  ! Rest
  INTEGER(4) :: j, l, n, jlim, it
  REAL(8) :: ADDLOG, RANDN, rn

  EXTERNAL :: SORTN

  ! This is very important
  !!$OMP THREADPRIVATE(evsum)
  ! With these nothing change
  !!$OMP THREADPRIVATE(evrestest,evtotest)
  ! The other variables gave an error:
  ! Error: DUMMY attribute conflicts with THREADPRIVATE attribute in 'live_max' at (1)

  !!th_num = omp_get_thread_num()

  ! Initialize variables (with different seeds for different processors)
 !irn = 1
  par_prior = 0.
  live = 0.
  live_like = 0.
  live_old = 0.
  live_like_old = 0.
  live_final = 0.
  live_like_final = 0.
  tstep = 0.
  tlnmass = 0.
  tlnrest = 0.
  evstep = 0.
  nstep = maxstep - nlive + 1
  !maxtries = 50*njump ! Accept an efficiency of more than 2% for the exploration, otherwise change something

  ALLOCATE(live_like_new(nth),live_new(nth,npar),too_many_tries(nth),icluster(nth))
  live_new = 0.
  live_like_new = 0.
  icluster = 0
  too_many_tries = .false.

  ! ---------- Inintial live points sorting ------------------------------------------------
  WRITE(*,*) 'Sorting live points. N. of points = ', nlive
  DO j=1, nlive


     ! Sort point considering priors
     ! Generate random prior if not fixe
     DO l=1,npar
        IF(par_fix(l).NE.1) THEN
           ! Uniform prior inside the limits
           IF (par_step(l).LE.0.) THEN
801           CALL RANDOM_NUMBER(rn)
              par_prior(l) = par_bnd1(l) + (par_bnd2(l)-par_bnd1(l))*rn
              !par_prior(l) = par_bnd1(l) + (par_bnd2(l)-par_bnd1(l))*RAND(0)
              !par_prior(l) = par_bnd1(l) + (par_bnd2(l)-par_bnd1(l))*rng(irn)
              !irn = irn + 1
              !write(*,*) par_bnd1(l), par_bnd2(l), par_prior(l), rn
              IF(par_prior(l).LT.par_bnd1(l).OR.par_prior(l).GT.par_bnd2(l)) GOTO 801
           ELSE
              ! Gaussian distrigution
700           par_prior(l) = par_in(l) + par_step(l)*RANDN
              IF(par_prior(l).LT.par_bnd1(l).OR.par_prior(l).GT.par_bnd2(l)) GOTO 700
           END IF
        ELSE
           ! If fixed, take the value indicated in the file
           par_prior(l) = par_in(l)
        END IF
     END DO
     ! If it is good, take it
     live(j,:) = par_prior(:)
     live_like(j) = LOGLIKELIHOOD_WITH_TEST(par_prior)
     !IF (live_like(j).GT.0) THEN
     !   WRITE(*,*) 'Attention!! Log(likelihood) strangely large (',live_like(j),'). Change the parameters bouduaries'
     !WRITE(*,*) 'Live point number:',j, 'Log(likelihood):', live_like(j), 'Parameters:', live(j,:)
     !   STOP
     !END IF
  END DO



  ! Calculate average and standard deviation of the parameters

  ! Order livepoints
  CALL SORTN(nlive,npar,live_like,live)
  ! Store in a file
  OPEN(11,FILE='nf_output_initial_live_points.dat',STATUS= 'UNKNOWN')
  WRITE(11,*) '# n     lnlikelihood     parameters'
  DO j=1,nlive
     WRITE(11,*) j, live_like(j), live(j,:)
  END DO
  CLOSE(11)


  ! --------- Set-up stuff before main loop ------------------------------------------------
  ! Calculate the intervarls (lograithmic) for the integration
  ! This is equivalent to the "crude" approximation from Skilling
  IF (maxstep/nlive.GT.700) THEN
     WRITE(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
     WRITE(*,*) '!!! Attention, too few nlive points for too many maxstep !!!'
     WRITE(*,*) '!!! Step volumes cannot be calculated correctly          !!!'
     WRITE(*,*) '!!! Change your input file                               !!!'
     WRITE(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
     STOP
  END IF
  DO l=1, maxstep
     tstep(l) = DEXP(-DFLOAT(l)/DFLOAT(nlive))
  ENDDO


  ! Alternatively, nlive number should be sorted and the maximum value should be taken as shrinking factor
  !tstep(0) = 1.d0
  !!CALL RANDOM_NUMBER(rng)
  !!tstep(1) = MAXVAL(rng)
  !
  !tstep(1) = MAXVAL(rng(irn:irn+nlive))
  !irn = irn + nlive

  !!CALL RANDOM_NUMBER(rng)
  !!tstep(2) = MAXVAL(rng)*tstep(1)
  !
  !tstep(2) = MAXVAL(rng(irn:irn+nlive))*tstep(1)
  !irn = irn + nlive


  ! Calculate the first differential prior mass and the remaining prior mass
  tlnmass(1) = DLOG(-(tstep(1)-1))
  tlnrest(1) = DLOG(tstep(1))

  ! Actual minimum loglikelihood and correspondent live point
  live_like_old(1) = live_like(1)
  live_old(1,:) = live(1,:)
  ! First evidence (sum because we are working with logarithms)
  evstep(1) = live_like_old(1) + tlnmass(1)
  evsum = evstep(1)

  ! Present minimal value of the likelihood
  min_live_like = live_like(1)

  ! ---------------------------------------------------------------------------------------!
  !                                                                                        !
  !                                 START THE MAIN LOOP                                    !
  !                                                                                        !
  !----------------------------------------------------------------------------------------!
  n = 1
  DO WHILE (n.LE.nstep)

     ! ##########################################################################
     ! Find a new live point
     
     ! Parallisation is on progress, not working yet
     !$ print *,'Starting parallel computation with ', nth, ' threads' 
     !$OMP PARALLEL PRIVATE(it,ntries) &
     !$OMP SHARED(live_like_new,live_new,icluster,too_many_tries)
     it  = OMP_GET_THREAD_NUM() + 1
     write(*,*) nth,it,n,itry,min_live_like,live_like(1)
     CALL SEARCH_NEW_POINT(n,itry,min_live_like,live_like,live, &
          live_like_new(it),live_new(it,:),icluster(it),ntries,too_many_tries(it))
     write(*,*) OMP_GET_THREAD_NUM(), it, min_live_like, live_like_new(it)
     !$OMP END PARALLEL

     IF (ANY(too_many_tries)) THEN
        nstep_final = n - 1
        GOTO 601
     END IF
     ! ##########################################################################

     DO it = 1, nth

        ! If the parallely computed live point is not good anymore, skip it.
        ! Otherwise take it for loop calculation
        IF ((it.GT.1).AND.(live_like_new(it).LE.min_live_like)) THEN
           CYCLE
        ELSE
           n = n +1
        ENDIF

        ! Calculate steps, mass and rest each time
        ! trapezoidal rule applied here (Skilling Entropy 2006)
        tlnmass(n) = DLOG(-(tstep(n+1)-tstep(n-1))/2.d0)
        tlnrest(n) = DLOG((tstep(n+1)+tstep(n-1))/2.d0)

        ! Reorder found point (no parallel here) and make the required calculation for the evidence
        ! Reorder point
        ! Order and exclude last point
        DO j=1,nlive-1
           IF (live_like_new(it).GT.live_like(j).AND.live_like_new(it).LT.live_like(j+1)) THEN
              jlim = j
           ELSE IF (live_like_new(it).GT.live_like(nlive)) THEN
              jlim = nlive
           END IF
        END DO

        ! Store old values
        live_like_old(n) = live_like(1)
        live_old(n,:) = live(1,:)

        ! Insert the new one
        IF (jlim.LT.1.OR.jlim.GT.nlive) THEN
           WRITE(*,*) 'Problem in the search method, or in the calculations'
           WRITE(*,*) 'No improvement in the likelihood value after finding the new point'
           WRITE(*,*) 'j = ', jlim, 'old min like = ', min_live_like, 'new min like = ', live_like_new
           STOP
        ELSE IF (jlim.EQ.1) THEN
           live_like(1) = live_like_new(it)
           live(1,:) = live_new(it,:)
        ELSE
           ! Shift values
           live_like(1:jlim-1) =  live_like(2:jlim)
           live(1:jlim-1,:) =  live(2:jlim,:)
           ! Insert new value
           live_like(jlim) =  live_like_new(it)
           live(jlim,:) =  live_new(it,:)
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
           ! Call cluster module to recalculate the std of the considered cluster and the cluster of the discarted point
           CALL REMAKE_CLUSTER_STD(live,icluster(it),icluster_old)
        END IF

        ! Calculate the evidence for this step
        evstep(n) = live_like_old(n) + tlnmass(n)

        ! Sum the evidences
        evsum = ADDLOG(evsum,evstep(n))

        ! Check if the estimate accuracy is reached
        evrestest = live_like(nlive) + tlnrest(n)
        evtotest = ADDLOG(evsum,evrestest)

        IF (evtotest-evsum.LT.evaccuracy) GOTO 301

        IF (MOD(n,50).EQ.0) THEN
           !   ! Write status
           WRITE(*,*) 'N. try:', itry, 'N step:', n, &
                'Min. loglike', min_live_like,'Evidence: ',evsum, &
                'Ev. step:', evstep(n),'Ev. pres. acc.:', evtotest-evsum, &
                'Typical eff.:', search_par2/ntries
           !
           !   ! Store actual live points
           !   WRITE(out_filename,1000) 'live_points_',itry,'.dat'
           !   OPEN(22,FILE=out_filename,STATUS= 'UNKNOWN')
           !   WRITE(22,*) '# n     lnlikelihood     parameters'
           !   DO l=1,nlive
           !      WRITE(22,*) l, live_like(l), live(l,:)
           !   END DO
           !   CLOSE(22)
           !   pause
           !
           !   ! Write temporal results in a file
           !   WRITE(out_filename,2000) 'status_',itry,'.dat'
           !   OPEN(33,FILE='status.dat',STATUS= 'UNKNOWN')
           !   WRITE(33,*) 'New last live point : ', live_like(1), live(1,:)
           !   WRITE(33,*) 'New first live point : ', live_like(nlive), live(nlive,:)
           !   WRITE(33,*) 'N step: ', n, 'Ev. at present: ',evsum, &
           !        'Ev. of the step: ', evstep(n), 'Diff. with the estimate total ev.: ', evtotest-evsum
           !   CLOSE(33)
           !
        END IF
     END DO
  END DO

  ! ---------------------------------------------------------------------------------------!
  !                                                                                        !
  !                                 STOP THE MAIN LOOP                                     !
  !                                                                                        !
  !----------------------------------------------------------------------------------------!
301 CONTINUE


  IF((evtotest-evsum).GE.evaccuracy) THEN
     WRITE(*,*) 'Final accuracy not reached in try n.', itry
     WRITE(*,*) 'Number of steps = ', n, 'Present accuracy = ', evtotest-evsum
     WRITE(*,*) 'Change your parameters (maxstep, nlive, accuracy,...)'
     IF (n.GE.nstep) STOP
  END IF

  ! Store the number of steps
  nstep_final = n

  ! Store steps in a file
  !OPEN(11,FILE='tsteps.dat',STATUS= 'UNKNOWN')
  !WRITE(11,*) '# tstep  tlnmass    tlnrest'
  !DO j=1, nstep_final
  !   WRITE(11,*) j, tstep(j), tlnmass(j), tlnrest(j)
  !END DO
  !CLOSE(11)

  ! Write status
  !WRITE(*,*) 'N step : ', n, 'Evidence at present : ', evsum

  ! Store actual live points
  !WRITE(out_filename,1000) 'live_points_',itry,'.dat'
  !OPEN(22,FILE=out_filename,STATUS= 'UNKNOWN')
  !WRITE(22,*) '# n     lnlikelihood     parameters'
  !DO l=1,nlive
  !   WRITE(22,*) l, live_like(l), live(l,:)
  !END DO
  !CLOSE(22)
  !
  !   ! Write temporal results in a file
  !WRITE(out_filename,2000) 'status_',itry,'.dat'
  !OPEN(33,FILE=out_filename,STATUS= 'UNKNOWN')
  !WRITE(33,*) 'New last live point : ', live_like(1), live(1,:)
  !WRITE(33,*) 'New first live point : ', live_like(nlive), live(nlive,:)
  !WRITE(33,*) 'N step : ', n, 'Evidence at present : ',evsum, &
  !     'Ev. of the step : ', evstep(n), 'Diff. with the estimate total evidence : ', evtotest-evsum
  !CLOSE(33)
  !

  ! Check the calculation of the evsteps
  !WRITE(out_filename,3000) 'evsteps_',th_num,'.dat'
  !OPEN(44,FILE=out_filename,STATUS= 'UNKNOWN')
  !WRITE(44,*) '# n   lnlikelihood     parameters'
  !DO l=1,nstep_final
  !   WRITE(44,*) l, evstep(l)
  !END DO
  !CLOSE(44)

  !------------ Calculate the total evidence with the last live points ---------------------
601 CONTINUE

  ! Store the last live points
  OPEN(99,FILE='nf_output_last_live_points.dat',STATUS= 'UNKNOWN')
  WRITE(99,*) '# n step =',  n-1
  WRITE(99,*) '# Evidence of the step =', evstep(n-1)
  WRITE(99,*) '# Evidence accuracy =',  ADDLOG(evsum,live_like(nlive) + tlnrest(n-1)) - evsum
  WRITE(99,*) '# n     lnlikelihood     parameters'
  DO j=1,nlive
     WRITE(99,*) j, live_like(j), live(j,:)
  END DO
  CLOSE(99)

  ! Calculate the last evidence
  ! Sum the last loglikelihood  (considering that we are dealing with logs)
  last_likes = live_like(1)
  DO j=2,nlive
     last_likes = ADDLOG(last_likes,live_like(j))
  END DO
  ! Average value of the loglikelihood
  live_like_last = last_likes - DLOG(DFLOAT(nlive))

  ! Evidence of each last points assuming equal volume spacing (EXP(tlnrest)/nlive)
  evlast = live_like_last + tlnrest(nstep_final) - DLOG(DFLOAT(nlive))

  ! The final evidence !!!
  evrest_last = live_like_last + tlnrest(nstep_final)
  evsum_final = ADDLOG(evsum,evrest_last)

  ! Insert the last live points in the ensemble
  live_old(nstep_final+1:nstep_final+nlive,:) = live(:,:)
  live_like_old(nstep_final+1:nstep_final+nlive) = live_like_last
  evstep(nstep_final+1:nstep_final+nlive) = evlast

  ! Total output of the routine (plis evsum_final)
  live_final = live_old
  live_like_final = live_like_old

  nall = nstep_final + nlive


  ! Deallocate variables for cluster analysis
  IF (cluster_on) THEN
     ! Reset cluster analysis
     cluster_on = .FALSE.
     CALL DEALLOCATE_CLUSTER()
  END IF

  ! Deallocate parallel stuff
  DEALLOCATE(live_like_new,live_new,too_many_tries,icluster)


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

!#############################################################################################
