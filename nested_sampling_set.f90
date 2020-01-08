SUBROUTINE NESTED_SAMPLING_SET(itry,ndata,nset,ndata_set,x_set,nc_set, &
     nc_err_set,errorbars_yn,funcname,&
     npar,par_fix,par_step,par_in,par_bnd1,par_bnd2,nlive,evaccuracy,sdfraction,&
     njump,cluster_yn,maxstep,nall,evsum_final,live_like_final,weight,live_final,live_like_max,live_max)
  ! Time-stamp: <Last changed by martino on Wednesday 01 January 2020 at CET 22:28:11>
  !!USE OMP_LIB
  !USE RNG
  !
  IMPLICIT NONE
  ! Random number variables
  !INTEGER, INTENT(IN) :: irnmax
  !REAL(8), INTENT(IN), DIMENSION(irnmax) :: rng
  !INTEGER(4) :: irn=1
  REAL(8), DIMENSION(nlive) :: rng
  REAL(8) :: rn
  ! Data
  INTEGER(4), INTENT(IN) :: itry, ndata, njump, maxstep, nset
  INTEGER(4), INTENT(IN), DIMENSION(nset) :: ndata_set
  REAL(8), INTENT(IN), DIMENSION(ndata,nset) :: x_set, nc_set, nc_err_set
  CHARACTER, INTENT(IN) :: errorbars_yn*1, cluster_yn*1, funcname*64
  ! Model varialbles and co.
  INTEGER(4), INTENT(IN) :: npar
  INTEGER(4), INTENT(IN), DIMENSION(npar) :: par_fix
  REAL(8), INTENT(IN), DIMENSION(npar) :: par_step, par_in, par_bnd1, par_bnd2
  INTEGER(4), INTENT(IN) :: nlive
  REAL(8), INTENT(IN) :: evaccuracy, sdfraction
  ! Prior variables
  REAL(8), DIMENSION(npar) :: par_prior
  ! Live points variables
  REAL(8), DIMENSION(nlive) :: live_like
  REAL(8), DIMENSION(nlive,npar) :: live
  ! Loop variables
  INTEGER(4) :: nstep = 8000
  REAL(8), DIMENSION(maxstep) :: tstep, tlnmass, tlnrest, evstep
  REAL(8) :: live_like_new
  REAL(8), DIMENSION(npar) :: live_new
  REAL(8), DIMENSION(maxstep) :: live_like_old
  REAL(8), DIMENSION(maxstep,npar) :: live_old
  REAL(8) :: evsum = 0., evrestest = 0., evtotest = 0.
  ! MCMC new point search varialbes
  INTEGER(4), PARAMETER :: maxtries = 1000, maxntries = 1000
  REAL(8) :: min_live_like, gval
  REAL(8), DIMENSION(npar) :: live_ave, live_var, live_sd, start_jump, new_jump
  INTEGER(4) :: istart, ntries, n_ntries
  LOGICAL :: outlimits
  ! Final calculations
  INTEGER(4) :: nstep_final
  REAL(8) :: last_likes, live_like_last, evrest_last, evlast
  INTEGER(4), INTENT(OUT) :: nall
  REAL(8), INTENT(OUT) :: evsum_final, live_like_max
  REAL(8), INTENT(OUT), DIMENSION(npar) :: live_max
  REAL(8), INTENT(OUT), DIMENSION(maxstep) :: weight
  REAL(8), INTENT(OUT), DIMENSION(maxstep) :: live_like_final
  REAL(8), INTENT(OUT), DIMENSION(maxstep,npar) :: live_final
  ! Rest
  INTEGER(4) :: i,j, l, n, jlim
  REAL(8) :: ADDLOG, LOGLIKELIHOOD_PAR_SET, RANDN
  CHARACTER :: out_filename*64
  ! Parallel stuff
  INTEGER(4) :: th_num

  ! This is very important
  !!$OMP THREADPRIVATE(evsum)
  ! With these nothing change
  !!$OMP THREADPRIVATE(evrestest,evtotest)
  ! The other variables gave an error:
  ! Error: DUMMY attribute conflicts with THREADPRIVATE attribute in 'live_max' at (1)

  !!th_num = omp_get_thread_num()

  !write(*,*) itry,ndata,nset,ndata_set,funcname,&
  !     npar,par_fix,par_step,par_in,par_bnd1,par_bnd2,nlive,evaccuracy,sdfraction
  !PAUSE
  !write(*,*) x_set,nc_set
  !PAUSE

  ! Initialize variables (with different seeds for different processors)
 !irn = 1
  par_prior = 0.
  live = 0.
  live_like = 0.
  live_new = 0.
  live_like_new = 0.
  live_old = 0.
  live_like_old = 0.
  live_final = 0.
  live_like_final = 0.
  tstep = 0.
  tlnmass = 0.
  tlnrest = 0.
  evstep = 0.
  nstep = maxstep - nlive + 1
  ntries = 0
  n_ntries = 0

  ! ---------- Inintial live points sorting ------------------------------------------------
  WRITE(*,*) 'Sorting live points'
  DO j=1, nlive
     ! Sort point considering priors
     ! Generate random prior if not fixed
800  DO l=1,npar
        IF(par_fix(l).NE.1) THEN
           ! Uniform prior inside the limits
           IF (par_step(l).LE.0.) THEN
801           CALL RANDOM_NUMBER(rn)
              par_prior(l) = par_bnd1(l) + (par_bnd2(l)-par_bnd1(l))*rn
              !par_prior(l) = par_bnd1(l) + (par_bnd2(l)-par_bnd1(l))*RAND(0)
              !par_prior(l) = par_bnd1(l) + (par_bnd2(l)-par_bnd1(l))*rng(irn)
              !irn = irn + 1
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
     live_like(j) = LOGLIKELIHOOD_PAR_SET(funcname,ndata,nset,ndata_set,x_set,nc_set,nc_err_set,errorbars_yn,npar,par_prior)
     IF (live_like(j).GT.-10.) THEN
        WRITE(*,*) 'Attention!! Log(likelihood) strangely large (',live_like(j),'). Change the parameters bouduaries'
        WRITE(*,*) 'Live point number:',j, 'Log(likelihood):', live_like(j), 'Parameters:', live(j,:)
        STOP
     END IF
  END DO


  ! Calculate average and standard deviation of the parameters

  ! Order livepoints
  CALL SORTN(nlive,npar,live_like,live)
  ! Store in a file
  !OPEN(11,FILE='initial_live_points.dat',STATUS= 'UNKNOWN')
  !WRITE(11,*) '# n     lnlikelihood     parameters'
  !DO j=1,nlive
  !   WRITE(11,*) j, live_like(j), live(j,:)
  !END DO
  !CLOSE(11)

  ! --------- Set-up stuff before main loop ------------------------------------------------
  ! Calculate the intervarls (lograithmic) for the integration
  ! This is equivalent to the "crude" approximation from Skilling
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


  ! ---------------------------------------------------------------------------------------!
  !                                                                                        !
  !                                 START THE MAIN LOOP                                    !
  !                                                                                        !
  !----------------------------------------------------------------------------------------!
  WRITE(*,*) 'Starting main loop for nested sampling'

  DO n=2, nstep
     ! Calculate steps, mass and rest each time without crude approximation
     !!CALL RANDOM_NUMBER(rng)
     !!tstep(n+1) = MAXVAL(rng)*tstep(n)

     !tstep(n+1) = MAXVAL(rng(irn:irn+nlive))*tstep(n)
     !irn = irn + nlive
     tlnmass(n) = DLOG(-(tstep(n+1)-tstep(n-1))/2.d0)
     tlnrest(n) = DLOG((tstep(n+1)+tstep(n-1))/2.d0)

     ! Find new live points
     ! ----------------------------------FIND_POINT_MCMC------------------------------------
     live_new = 0.
     live_like_new = 0.
     live_ave = 0.
     live_var = 0.
     live_sd  = 0.
     ntries = 0
     n_ntries = 0
     min_live_like = live_like(1)

     ! Calculate momenta of the live points
     DO i=1,npar
        CALL MEANVAR(live(:,i),nlive,live_ave(i),live_var(i))
     END DO
     live_sd = DSQRT(live_var)

     ! Select a live point as starting point
     CALL RANDOM_NUMBER(rn)
     istart= FLOOR((nlive-1)*rn+1)
     !istart= FLOOR((nlive-1)*RAND(0)+1)
     !istart= FLOOR((nlive-1)*rng(irn)+1)
     !irn = irn + 1
     start_jump = live(istart,:)

     !write(*,*) istart,  live(istart,1)
     !pause


     ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
500  DO i=1,njump
        ntries = ntries + 1
501     CONTINUE
        !$OMP PARALLEL DO
        DO l=1,npar
           IF (par_fix(l).NE.1) THEN
502           CALL RANDOM_NUMBER(rn)
              new_jump(l) = start_jump(l) + live_sd(l)*sdfraction*(2.*rn-1.)
              !new_jump(l) = start_jump(l) + live_sd(l)*sdfraction*(2.*RAND(0)-1.)
              !new_jump(l) = start_jump(l) + live_sd(l)*sdfraction*(2.*rng(irn)-1.)
              !irn = irn + 1
              IF (new_jump(l).LT.par_bnd1(l).OR.new_jump(l).GT.par_bnd2(l)) GOTO 502
           ELSE
              new_jump(l) = start_jump(l)
           END IF
        END DO
        !$OMP END PARALLEL DO
        !write(*,*) 'Jump n. ', ntries, ' : ', new_jump(1), start_jump(1), live_sd(1)

        ! Check if the new point is inside the parameter volume defined by the minimum likelihood of the live points
         IF (LOGLIKELIHOOD_PAR_SET(funcname,ndata,nset,ndata_set,x_set,nc_set,nc_err_set,errorbars_yn,npar,new_jump)&
              .GT.min_live_like) THEN
           start_jump = new_jump

        ELSE
           ! Check failures
           ! message of too many failures
           ! Choose another point determinated with a specific method (at present mine)
           !
           ! But before, if you already tried many times, do something different or eventually gave up


           IF (ntries.GT.maxtries) THEN
              n_ntries = n_ntries + 1
              ! If you already did too much tries, gave up!
              IF (n_ntries.GT.maxntries) THEN
                 WRITE(*,*) 'Too many tries to find new live points for try n.', itry, '!!!! More than ', maxtries*maxntries
                 WRITE(*,*) 'We take the data as they are :-~'
                 nstep_final = n - 1
                 GOTO 601
              END IF
              !
              WRITE(*,*) 'Too many tries to find new live points for try n.',itry,'!!!! More than',maxtries,'n_ntries =',n_ntries
              ! Some test for desesperate seeking (for presence of several maxima)
              ! If nothing is found, restart from a livepoint
              ntries = 0
              ! Stanrdard technique, start from a different life point
!!$              CALL RANDOM_NUMBER(rn)
!!$              istart= FLOOR((nlive)*rn+1)
!!$              start_jump = live(istart,:)
              !
              ! My new technique: mix parameter values from different sets to hope to find a good new value
              !$OMP PARALLEL DO
              DO l=1,npar
                 IF (par_step(l).LE.0.) THEN
                    CALL RANDOM_NUMBER(rn)
                    istart= FLOOR((nlive-1)*rn+1)
                    start_jump(l) = live(istart,l)
                    ! Leo's technique
                    !start_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rn
                    !!start_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*RAND(0)
                    !!start_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rng(irn)
                    !!irn = irn + 1
                 END IF
              END DO
              !$OMP END PARALLEL DO
              ! Store the present live points as they are
              OPEN(99,FILE='nf_intermediate_live_points.dat',STATUS= 'UNKNOWN')
              WRITE(99,*) '# n step =',  n-1
              WRITE(99,*) '# n tries =', n_ntries
              WRITE(99,*) '# Evidence of the step =', evstep(n-1)
              WRITE(99,*) '# Evidence accuracy =',  ADDLOG(evsum,live_like(nlive) + tlnrest(n-1)) - evsum
              WRITE(99,*) '# n     lnlikelihood     parameters'
              DO j=1,nlive
                 WRITE(99,*) j, live_like(j), live(j,:)
              END DO
              CLOSE(99)
              !
              GOTO 500
           END IF
           !
           ! Stanrdard technique
           CALL RANDOM_NUMBER(rn)
           istart= FLOOR((nlive)*rn+1)
           start_jump = live(istart,:)
           !
           ! My new technique: mix parameter values from different sets to hope to find a good new value
!!$           !$OMP PARALLEL DO
!!$           DO l=1,npar
!!$              IF (par_step(l).LE.0.) THEN
!!$                 CALL RANDOM_NUMBER(rn)
!!$                 istart= FLOOR((nlive-1)*rn+1)
!!$                 start_jump(l) = live(istart,l)
!!$                 ! Leo's technique
!!$                 !start_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rn
!!$                 !!start_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*RAND(0)
!!$                 !!start_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rng(irn)
!!$                 !!irn = irn + 1
!!$              END IF
!!$           END DO
!!$           !$OMP END PARALLEL DO
           GOTO 500
        END IF
     END DO
     ! Final check of the last point
     ! Final check of the last point for gaussian priors
     !CALL USERCONDITIONS(funcname,npar,par_fix,new_jump,par_in,par_step,par_bnd1,par_bnd2, &
     !     live_sd,start_jump,sdfraction,outlimits)
     DO l=1,npar
        IF(par_fix(l).NE.1) THEN
           IF(par_step(l).GT.0) THEN
              ! maximum of the distribution is 1 (and is not normalized to 1 as the previous line)
              gval = dexp(-(new_jump(l)-par_in(l))**2/(2*par_step(l)**2))
              CALL RANDOM_NUMBER(rn)
              IF (rn.GT.gval) GOTO 500
           END IF
        END IF
     END DO

     ! Last(maybe useless) check
      IF (LOGLIKELIHOOD_PAR_SET(funcname,ndata,nset,ndata_set,x_set,nc_set,nc_err_set,errorbars_yn,npar,new_jump)&
           .LT.min_live_like.OR.outlimits) GOTO 500


     ! Take the last point after jumps as new livepoint
     live_new = new_jump
     live_like_new = LOGLIKELIHOOD_PAR_SET(funcname,ndata,nset,ndata_set,x_set,nc_set,nc_err_set,errorbars_yn,npar,new_jump)

     ! ------------------------------------------------------------------------------------

     ! Reorder found point (no parallel here) and make the required calculation for the evidence
     ! Reorder point
     ! Order and exclude last point
     DO j=1,nlive-1
        IF (live_like_new.GT.live_like(j).AND.live_like_new.LT.live_like(j+1)) THEN
           jlim = j
        END IF
     END DO
     IF (jlim.LE.1) GOTO 500
     IF (live_like_new.GT.live_like(nlive)) jlim = nlive

     ! Store old values
     live_like_old(n) = live_like(1)
     live_old(n,:) = live(1,:)
     ! Shift values
     live_like(1:jlim-1) =  live_like(2:jlim)
     live(1:jlim-1,:) =  live(2:jlim,:)
     ! Insert new value
     live_like(jlim) =  live_like_new
     live(jlim,:) =  live_new
     ! The rest stay as it is

     ! Calculate the evidence for this step
     evstep(n) = live_like_old(n) + tlnmass(n)

     ! Sum the evidences
     evsum = ADDLOG(evsum,evstep(n))

     ! Check if the estimate accuracy is reached
     evrestest = live_like(nlive) + tlnrest(n)
     evtotest = ADDLOG(evsum,evrestest)

     IF (evtotest-evsum.LT.evaccuracy.OR.n_ntries.GT.maxntries) GOTO 301

     IF (MOD(n,50).EQ.0) THEN
     !   ! Write status
        WRITE(*,*) 'N. try: ', itry, 'N step: ', n, 'Ev. at present: ',evsum, &
             'Ev. of the step: ', evstep(n),'Diff. with the estimate total ev.: ', evtotest-evsum
     !
     !   ! Store actual live points
     !   WRITE(out_filename,1000) 'live_points_',itry,'.dat'
     !   OPEN(22,FILE=out_filename,STATUS= 'UNKNOWN')
     !   WRITE(22,*) '# n     lnlikelihood     parameters'
     !   DO l=1,nlive
     !      WRITE(22,*) l, live_like(l), live(l,:)
     !   END DO
     !   CLOSE(22)
     !
     !   ! Write temporal results in a file
     !   WRITE(out_filename,2000) 'status_',itry,'.dat'
     !   OPEN(33,FILE=out_filename,STATUS= 'UNKNOWN')
     !   WRITE(33,*) 'New last live point : ', live_like(1), live(1,:)
     !   WRITE(33,*) 'New first live point : ', live_like(nlive), live(nlive,:)
     !   WRITE(33,*) 'N step: ', n, 'Ev. at present: ',evsum, &
     !        'Ev. of the step: ', evstep(n), 'Diff. with the estimate total ev.: ', evtotest-evsum
     !   CLOSE(33)
     !
     END IF
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

  ! Calculate the last evidence
  ! Sum the last loglikelihood  (considering that we are dealing with logs)
  last_likes = live_like(1)
  DO j=2,nlive
     last_likes = ADDLOG(last_likes,live_like(j))
  END DO
  ! Average value
  live_like_last = last_likes - DLOG(DFLOAT(nlive))

  ! Evidence of each last points
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


  !------------ Calculate weights and parameter best values and distributions ----------------

  ! Save the point with the highest likelihood
  live_max = live(nlive,:)
  live_like_max = live_like(nlive)

  ! Calculate the weights with test of overflow
  weight = 0.d0
  DO n=1,nstep_final
     IF(DABS(evstep(n) - evsum_final).LT.700) weight(n) = DEXP(evstep(n) - evsum_final)
  END DO

1000 FORMAT (A12,I1,A4)
!2000 FORMAT (A7,I1,A4)
!3000 FORMAT (A8,I1,A4)

  RETURN

END SUBROUTINE NESTED_SAMPLING_SET

!#############################################################################################


! __________________________________________________________________________________________
FUNCTION LOGLIKELIHOOD_PAR_SET(funcname,ndata,nset,ndata_set,x_set,nc_set,nc_err_set,errorbars_yn,npar,par)
  IMPLICIT NONE
  INTEGER(4) :: ndata, npar, nset
  INTEGER(4), DIMENSION(nset) :: ndata_set
  REAL(8), DIMENSION(ndata,nset) :: x_set, nc_set, nc_err_set
  REAL(8), DIMENSION(ndata) :: ll_tmp
  CHARACTER :: errorbars_yn*1, funcname*64
  REAL(8) :: USERFCN_SET, LOGLIKELIHOOD_PAR_SET, enc, const_ll
  REAL(8), DIMENSION(npar) :: par
  INTEGER(4) :: i, k
  COMMON /loglikelihood/ const_ll

  ! Calculate LIKELIHOOD
  LOGLIKELIHOOD_PAR_SET = 0.
  ll_tmp = 0.

  IF (errorbars_yn.EQ.'n'.OR.errorbars_yn.EQ.'N') THEN
     DO k =1, nset
        !$OMP PARALLEL DO
        DO i=1, ndata_set(k)
           enc = USERFCN_SET(x_set(i,k),npar,par,funcname,k)
           IF (nc_set(i,k).EQ.0..AND.enc.GT.0.) THEN
              ll_tmp(i) = nc_set(i,k)*DLOG(enc) - enc
           ELSE IF(nc_set(i,k).GT.0..AND.enc.GT.0.) THEN
              ll_tmp(i) = nc_set(i,k)*DLOG(enc) - enc
           ELSE IF(nc_set(i,k).GT.0..AND.enc.LE.0.) THEN
              WRITE(*,*) 'LIKELIHOOD ERROR: put a background in your function'
              WRITE(*,*) 'number of counts different from 0, model prediction equal 0 or less'
              STOP
           END IF
        END DO
        !$OMP END PARALLEL DO
        !pause
        !
        ! Sum all together
        LOGLIKELIHOOD_PAR_SET =  LOGLIKELIHOOD_PAR_SET + SUM(ll_tmp) + const_ll
     END DO
  ELSE
     DO k =1, nset
        !$OMP PARALLEL DO
        DO i=1, ndata_set(k)
           enc = USERFCN_SET(x_set(i,k),npar,par,funcname,k)
           ll_tmp(i) = - (nc_set(i,k) - enc)**2/(2*nc_err_set(i,k)**2)
           !write(*,*) x(i),nc(i), enc,nc_err(i), ll_tmp(i), const_ll !!??
        ENDDO
        !$OMP END PARALLEL DO
        !pause
        !
        ! Sum all together
        LOGLIKELIHOOD_PAR_SET=  LOGLIKELIHOOD_PAR_SET + SUM(ll_tmp) + const_ll
     END DO
  END IF

END FUNCTION LOGLIKELIHOOD_PAR_SET


! ___________________________________________________________________________________________


!#############################################################################################
