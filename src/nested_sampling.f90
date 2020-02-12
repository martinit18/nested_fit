SUBROUTINE NESTED_SAMPLING(itry,ndata,x,nc,nc_err,errorbars_yn,funcname,&
     npar,par_fix,par_step,par_in,par_bnd1,par_bnd2,nlive,evaccuracy,sdfraction,&
     njump,maxtries,maxntries,cluster_yn,maxstep,nall,evsum_final,live_like_final,weight,&
     live_final,live_like_max,live_max)
  ! Time-stamp: <Last changed by martino on Wednesday 01 January 2020 at CET 22:27:59>
  ! For parallel tests only
  !SUBROUTINE NESTED_SAMPLING(irnmax,rng,itry,ndata,x,nc,funcname,&
  !   npar,par_fix,par_step,par_in,par_bnd1,par_bnd2,nlive,evaccuracy,sdfraction,&
  !   nall,evsum_final,live_like_final,weight,live_final,live_like_max,live_max)
  !!USE OMP_LIB
  !USE RNG
  !
  ! Module for cluster analysis
  USE MOD_MEAN_SHIFT_CLUSTER_ANALYSIS
  IMPLICIT NONE
  ! Random number variables
  !INTEGER, INTENT(IN) :: irnmax
  !REAL(8), INTENT(IN), DIMENSION(irnmax) :: rng
  INTEGER(4) :: irn
  REAL(8), DIMENSION(nlive) :: rng
  REAL(8) :: rn
  ! Data
  INTEGER(4), INTENT(IN) :: itry, ndata, njump, maxstep
  REAL(8), INTENT(IN), DIMENSION(ndata) :: x, nc,nc_err
  CHARACTER, INTENT(IN) :: errorbars_yn*1, funcname*64
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
  INTEGER(4) :: maxtries, maxntries
  REAL(8) :: min_live_like, gval
  REAL(8), DIMENSION(npar) :: live_ave, live_var, live_sd, start_jump, new_jump
  INTEGER(4) :: istart, ntries, n_ntries
  ! Cluster analysis variables
  CHARACTER, INTENT(IN) :: cluster_yn*1
  REAL(8), DIMENSION(nlive,npar) :: live_selected
  INTEGER(4) :: icluster, icluster_old
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
  REAL(8) :: ADDLOG, LOGLIKELIHOOD_PAR, RANDN
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
  !maxtries = 50*njump ! Accept an efficiency of more than 2% for the exploration, otherwise change something


  ! ---------- Inintial live points sorting ------------------------------------------------
  WRITE(*,*) 'Sorting live points. N. of points = ', nlive
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
     live_like(j) = LOGLIKELIHOOD_PAR(funcname,ndata,x,nc,nc_err,errorbars_yn,npar,par_prior)

     IF (live_like(j).GT.-10) THEN
        WRITE(*,*) 'Attention!! Log(likelihood) strangely large (',live_like(j),'). Change the parameters bouduaries'
        WRITE(*,*) 'Live point number:',j, 'Log(likelihood):', live_like(j), 'Parameters:', live(j,:)
        STOP
     END IF
  END DO


  ! Calculate average and standard deviation of the parameters

  ! Order livepoints
  CALL SORTN(nlive,npar,live_like,live)
  ! Store in a file
  OPEN(11,FILE='nf_initial_live_points.dat',STATUS= 'UNKNOWN')
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


  ! ---------------------------------------------------------------------------------------!
  !                                                                                        !
  !                                 START THE MAIN LOOP                                    !
  !                                                                                        !
  !----------------------------------------------------------------------------------------!

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
     new_jump = par_in
     live_new = 0.
     live_like_new = 0.
     live_ave = 0.
     live_var = 0.
     live_sd  = 0.
     n_ntries = 0
     min_live_like = live_like(1)


     ! Select a live point as starting point
     ntries = 0
     CALL RANDOM_NUMBER(rn)
     istart= FLOOR((nlive-1)*rn+1)
     !istart= FLOOR((nlive-1)*RAND(0)+1)
     !istart= FLOOR((nlive-1)*rng(irn)+1)
     !irn = irn + 1
     start_jump = live(istart,:)


     ! Calculate momenta of the live points
400  IF (cluster_on) THEN
        ! Identify cluster appartenance
        icluster = p_cluster(istart)
        ! Get for the specific cluster if the cluster analusis is on
        live_sd(:) = cluster_std(icluster,:)
        IF(cluster_std(icluster,1).EQ.0.) THEN
           ! If the cluster is formed only from one point, take the standard standard deviation
           !$OMP PARALLEL DO
           DO i=1,npar
              IF(par_fix(l).NE.1) THEN
                 CALL MEANVAR(live(:,i),nlive,live_ave(i),live_var(i))
              ELSE
                 live_ave(i) = par_in(i)
              END IF
           END DO
           !$OMP END PARALLEL DO
        live_sd = DSQRT(live_var)
        END IF
     ELSE
        !$OMP PARALLEL DO
        DO i=1,npar
           IF(par_fix(i).NE.1) THEN
              CALL MEANVAR(live(:,i),nlive,live_ave(i),live_var(i))
           ELSE
              live_ave(i) = par_in(i)
           END IF
        END DO
        !$OMP END PARALLEL DO
        live_sd = DSQRT(live_var)
     END IF



     ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
500  CONTINUE
     DO i=1,njump
501     CONTINUE
        ntries = ntries + 1
        !$OMP PARALLEL DO
        DO l=1,npar
           IF (par_fix(l).NE.1) THEN
502           CALL RANDOM_NUMBER(rn)
              new_jump(l) = start_jump(l) + live_sd(l)*sdfraction*(2.*rn-1.)
              !new_jump(l) = start_jump(l) + live_sd(l)*sdfraction*(2.*RAND(0)-1.)
              !new_jump(l) = start_jump(l) + live_sd(l)*sdfraction*(2.*rng(irn)-1.)
              !irn = irn + 1
              IF (new_jump(l).LT.par_bnd1(l).OR.new_jump(l).GT.par_bnd2(l)) THEN
                 ntries = ntries + 1
                 GOTO 502
              END IF
           END IF
        END DO
        !$OMP END PARALLEL DO

        ! Check if the new point is inside the parameter volume defined by the minimum likelihood of the live points
        IF (LOGLIKELIHOOD_PAR(funcname,ndata,x,nc,nc_err,errorbars_yn,npar,new_jump).GT.min_live_like) THEN
           start_jump = new_jump
        ELSE
           ! Check failures
           ! message of too many failures
           ! Choose another point determinated with a specific method (at present mine)

           !
           ! But before, if you already tried many times, do something different or eventually gave up
           IF (ntries.GT.maxtries) THEN   ! Loop for ntries > maxtries
              n_ntries = n_ntries + 1
              ! If nothing is found, restart from a livepoint
              ntries = 0

              ! If you already did too much tries, gave up or start a cluster analysis
              IF (n_ntries.GE.maxntries) THEN
                 IF (cluster_yn.EQ.'y'.OR.cluster_yn.EQ.'Y') THEN
                    WRITE(*,*) 'Performing cluster analysis. Number of step = ', n
                    CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
                    ! outputs: p_cluster ! flag of number of appartenance cluster for each live point
                    cluster_on = .true.
                    n_ntries = 0

                    ! Choose a new random live point and restart all
                    CALL RANDOM_NUMBER(rn)
                    istart= FLOOR((nlive-1)*rn+1)
                    start_jump = live(istart,:)

                    GOTO 400

                 ELSE
                    WRITE(*,*) 'Too many tries to find new live points for try n.', itry, '!!!! More than ', maxtries*maxntries
                    WRITE(*,*) 'We take the data as they are :-~'
                    nstep_final = n - 1

                    GOTO 601

                 END IF
              END IF
              !
              WRITE(*,*) 'Too many tries to find new live points for try n.',itry,'!!!! More than',maxtries,'n_ntries =',n_ntries, &
                   'n. step =', n
              ! Some test for desesperate seeking (for presence of several maxima)


              ! DO CLUSTER ANALYSIS if selected, or use other randomizations
              ! For all live points, assign a cluster number, which is used to calculate specific standard deviation
              ! for the search of the new point

              IF(.not.cluster_on) THEN
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

                 ! Alternate the three techniques to find a new life point
                 CALL RANDOM_NUMBER(rn)
                 irn = FLOOR(2*rn+1)
                 IF(MOD(ntries,2).EQ.1) THEN
                    ! My new technique: mix parameter values from different sets to hope to find a good new value
                    !$OMP PARALLEL DO
                    DO l=1,npar
                       IF (par_fix(l).NE.1) THEN
                          CALL RANDOM_NUMBER(rn)
                          istart= FLOOR((nlive-1)*rn+1)
                          new_jump(l) = live(istart,l)
                       END IF
                    END DO
                    !$OMP END PARALLEL DO
                 ELSE
                    ! Leo's technique, go between the average and this point
                    !$OMP PARALLEL DO
                    DO l=1,npar
                       IF (par_fix(l).NE.1) THEN
                          CALL RANDOM_NUMBER(rn)
                          new_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rn
                       END IF
                    END DO
                    !$OMP END PARALLEL DO
                 END IF
                 !

                 IF (LOGLIKELIHOOD_PAR(funcname,ndata,x,nc,nc_err,errorbars_yn,npar,start_jump).GT.min_live_like) THEN
                    start_jump = new_jump
                 ELSE
                    ! Choose a new random live point and restart all
                    CALL RANDOM_NUMBER(rn)
                    istart= FLOOR((nlive-1)*rn+1)
                    start_jump = live(istart,:)
                 END IF
                 GOTO 500
              ELSE
                 ! If cluster analysis, do not mix the points!!
                 ! Choose a new random live point and restart all
                 CALL RANDOM_NUMBER(rn)
                 istart= FLOOR((nlive-1)*rn+1)
                 start_jump = live(istart,:)
                 GOTO 400
              END IF

           END IF    ! End loop for ntries > maxtries

           GOTO 501  ! Restart looking for new points without changing the starting point

        END IF  ! End of loop with failure for likelihood value
     END DO


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
     IF (LOGLIKELIHOOD_PAR(funcname,ndata,x,nc,nc_err,errorbars_yn,npar,new_jump).LT.min_live_like) GOTO 500

     ! Take the last point after jumps as new livepoint
     live_new = new_jump
     live_like_new = LOGLIKELIHOOD_PAR(funcname,ndata,x,nc,nc_err,errorbars_yn,npar,new_jump)

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


     ! Assign to the new point, the same cluster number of the start point
     IF (cluster_on) THEN
        ! Instert new point
        p_cluster(1:jlim-1) =  p_cluster(2:jlim)
        p_cluster(jlim) = icluster
        cluster_np(icluster) = cluster_np(icluster) + 1
        ! Take out old point
        icluster_old = p_cluster(1)
        cluster_np(icluster_old) = cluster_np(icluster_old) - 1
        ! Call cluster module to recalculate the std of the considered cluster and the cluster of the discarted point
         CALL REMAKE_CLUSTER_STD(live,icluster,icluster_old)
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
             'Typical eff.:', REAL(njump)/ntries
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
  WRITE(99,*) '# n tries =', n_ntries
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
!4000 FORMAT (A,I3,A,I3,A,ES12.6,A,ES12.6,A,ES12.6,A,ES12.6,A,ES10.2)

  RETURN

END SUBROUTINE NESTED_SAMPLING

!#############################################################################################




!!$!OLD VERSION WORKING
!!$
! __________________________________________________________________________________________
FUNCTION LOGLIKELIHOOD_PAR(funcname,ndata,x,nc,nc_err,errorbars_yn,npar,par)
  ! Parallelizable version of the loglikelihood function
  IMPLICIT NONE
  INTEGER(4) :: ndata, npar
  REAL(8), DIMENSION(ndata) :: x, nc, nc_err, ll_tmp
  CHARACTER :: errorbars_yn*1, funcname*64
  REAL(8) :: USERFCN, LOGLIKELIHOOD_PAR, enc, const_ll
  REAL(8), DIMENSION(npar) :: par
  INTEGER(4) :: i
  COMMON /loglikelihood/ const_ll

  ! Calculate LIKELIHOOD
  LOGLIKELIHOOD_PAR = 0.
  ll_tmp = 0.


  IF (errorbars_yn.EQ.'n'.OR.errorbars_yn.EQ.'N') THEN
     !$OMP PARALLEL DO
     DO i=1, ndata
        enc = USERFCN(x(i),npar,par,funcname)
        IF (nc(i).EQ.0..AND.enc.GT.0.) THEN
           ll_tmp(i) = - enc
        ELSE IF(nc(i).GT.0..AND.enc.GT.0.) THEN
           !ll_tmp(i) = nc(i)*DLOG(enc) - enc - DFACTLN(INT(nc(i)))
           ll_tmp(i) = nc(i)*DLOG(enc) - enc
        ELSE IF(nc(i).GT.0..AND.enc.LE.0.) THEN
           WRITE(*,*) 'LIKELIHOOD ERROR: put a background in your function'
           WRITE(*,*) 'nuber of counts different from 0, model prediction equal 0 or less'
           STOP
        END IF
        !write(*,*) x(i),nc(i), enc, ll_tmp(i), sum(ll_tmp), sum(ll_tmp) + const_ll
     ENDDO
     !$OMP END PARALLEL DO
     !pause
     !
     ! Sum all together
     LOGLIKELIHOOD_PAR = SUM(ll_tmp) + const_ll
     !LOGLIKELIHOOD_PAR = SUM(ll_tmp)
  ELSE
     !$OMP PARALLEL DO
     DO i=1, ndata
        enc = USERFCN(x(i),npar,par,funcname)
        ll_tmp(i) = - (nc(i) - enc)**2/(2*nc_err(i)**2)
        !write(*,*) x(i),nc(i), enc,nc_err(i), ll_tmp(i), const_ll !!??
     ENDDO
     !$OMP END PARALLEL DO
     !
     ! Sum all together
     LOGLIKELIHOOD_PAR = SUM(ll_tmp) + const_ll
     !LOGLIKELIHOOD_PAR = SUM(ll_tmp)
  END IF

END FUNCTION LOGLIKELIHOOD_PAR


!!$! Working too but slower too!! 105 sec instead ~90
!!$! __________________________________________________________________________________________
!!$FUNCTION LOGLIKELIHOOD_PAR(funcname,ndata,x,nc,nc_err,errorbars_yn,npar,par)
!!$  ! Parallelizable version of the loglikelihood function MODIFIED!!!!!!
!!$  IMPLICIT NONE
!!$  INTEGER(4) :: ndata, npar
!!$  REAL(8), DIMENSION(ndata) :: x, nc, nc_err
!!$  REAL(8) :: ll_tmp
!!$  CHARACTER :: errorbars_yn*1, funcname*64
!!$  REAL(8) :: USERFCN, LOGLIKELIHOOD_PAR, enc, const_ll
!!$  REAL(8), DIMENSION(npar) :: par
!!$  INTEGER(4) :: i
!!$  REAL(8), PARAMETER :: pi=3.141592653589793d0
!!$  COMMON /loglikelihood/ const_ll
!!$
!!$  ! Calculate LIKELIHOOD
!!$  !$OMP PARALLEL PRIVATE(ll_tmp,enc) SHARED(LOGLIKELIHOOD_PAR,x,nc,npar,funcname)
!!$  LOGLIKELIHOOD_PAR = 0.
!!$  ll_tmp = 0.
!!$
!!$
!!$  IF (errorbars_yn.EQ.'n'.OR.errorbars_yn.EQ.'N') THEN
!!$     !$OMP DO
!!$     DO i=1, ndata
!!$        enc = USERFCN(x(i),npar,par,funcname)
!!$        IF (nc(i).EQ.0..AND.enc.GT.0.) THEN
!!$           ll_tmp = ll_tmp - enc
!!$        ELSE IF(nc(i).GT.0..AND.enc.GT.0.) THEN
!!$           !ll_tmp(i) = nc(i)*DLOG(enc) - enc - DFACTLN(INT(nc(i)))
!!$           ll_tmp = ll_tmp + nc(i)*DLOG(enc) - enc
!!$        ELSE IF(nc(i).GT.0..AND.enc.EQ.0.) THEN
!!$           WRITE(*,*) 'LIKELIHOOD ERROR: put a background in your function'
!!$           WRITE(*,*) 'nuber of counts different from 0, model prediction equal 0'
!!$           STOP
!!$        END IF
!!$        !write(*,*) x(i),nc(i), enc, ll_tmp(i), sum(ll_tmp), sum(ll_tmp) + const_ll
!!$     ENDDO
!!$     !$OMP END  DO
!!$     !pause
!!$     !
!!$     ! Sum all together
!!$     !$OMP CRITICAL
!!$     LOGLIKELIHOOD_PAR =  LOGLIKELIHOOD_PAR + ll_tmp
!!$     !$OMP END CRITICAL
!!$  ELSE
!!$     ! ???????? TO CHANGE IF WORKING
!!$     DO i=1, ndata
!!$        enc = USERFCN(x(i),npar,par,funcname)
!!$        ll_tmp = ll_tmp - (nc(i) - enc)**2/(2*nc_err(i)**2)
!!$        !write(*,*) x(i),nc(i), enc,nc_err(i), ll_tmp(i), const_ll !!??
!!$     ENDDO
!!$     !
!!$     ! Sum all together
!!$     LOGLIKELIHOOD_PAR = LOGLIKELIHOOD_PAR + const_ll
!!$  END IF
!!$  !$OMP END PARALLEL
!!$  LOGLIKELIHOOD_PAR = LOGLIKELIHOOD_PAR + const_ll
!!$
!!$END FUNCTION LOGLIKELIHOOD_PAR
!!$



!!$! WORKING?? SLOWER !!!!  120 sec instead ~90
!!$! __________________________________________________________________________________________
!!$FUNCTION LOGLIKELIHOOD_PAR(funcname,ndata,x,nc,nc_err,errorbars_yn,npar,par)
!!$  ! Parallelizable version of the loglikelihood function
!!$  IMPLICIT NONE
!!$  INTEGER(4) :: ndata, npar
!!$  REAL(8), DIMENSION(ndata) :: x, nc, nc_err, ll_tmp
!!$  CHARACTER :: errorbars_yn*1, funcname*64
!!$  REAL(8) :: USERFCN, LOGLIKELIHOOD_PAR, enc, const_ll
!!$  REAL(8), DIMENSION(npar) :: par
!!$  INTEGER(4) :: i
!!$  REAL(8), PARAMETER :: pi=3.141592653589793d0
!!$  COMMON /loglikelihood/ const_ll
!!$
!!$  ! Calculate LIKELIHOOD
!!$  LOGLIKELIHOOD_PAR = 0.
!!$  ll_tmp = 0.
!!$
!!$
!!$  IF (errorbars_yn.EQ.'n'.OR.errorbars_yn.EQ.'N') THEN
!!$     !$OMP PARALLEL DO
!!$     DO i=1, ndata
!!$        enc = USERFCN(x(i),npar,par,funcname)
!!$        IF (nc(i).EQ.0..AND.enc.GT.0.) THEN
!!$           ll_tmp(i) = - enc
!!$        ELSE IF(nc(i).GT.0..AND.enc.GT.0.) THEN
!!$           !ll_tmp(i) = nc(i)*DLOG(enc) - enc - DFACTLN(INT(nc(i)))
!!$           ll_tmp(i) = nc(i)*DLOG(enc) - enc
!!$        ELSE IF(nc(i).GT.0..AND.enc.EQ.0.) THEN
!!$           WRITE(*,*) 'LIKELIHOOD ERROR: put a background in your function'
!!$           WRITE(*,*) 'nuber of counts different from 0, model prediction equal 0'
!!$           STOP
!!$        END IF
!!$        !write(*,*) x(i),nc(i), enc, ll_tmp(i), sum(ll_tmp), sum(ll_tmp) + const_ll
!!$     ENDDO
!!$     !$OMP END PARALLEL DO
!!$     !pause
!!$     !
!!$     ! Sum all together
!!$     !$OMP PARALLEL DO REDUCTION(+:LOGLIKELIHOOD_PAR)
!!$     DO i=1, ndata
!!$        LOGLIKELIHOOD_PAR = LOGLIKELIHOOD_PAR+ ll_tmp(i)
!!$     END DO
!!$     LOGLIKELIHOOD_PAR =  LOGLIKELIHOOD_PAR + const_ll
!!$  ELSE
!!$     !$OMP PARALLEL DO
!!$     DO i=1, ndata
!!$        enc = USERFCN(x(i),npar,par,funcname)
!!$        ll_tmp(i) = - (nc(i) - enc)**2/(2*nc_err(i)**2)
!!$        !write(*,*) x(i),nc(i), enc,nc_err(i), ll_tmp(i), const_ll !!??
!!$     ENDDO
!!$     !$OMP END PARALLEL DO
!!$     !
!!$     ! Sum all together
!!$     LOGLIKELIHOOD_PAR = SUM(ll_tmp) + const_ll
!!$  END IF

!!$  END FUNCTION LOGLIKELIHOOD_PAR

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
