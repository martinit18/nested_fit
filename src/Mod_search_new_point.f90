MODULE MOD_SEARCH_NEW_POINT
  ! Automatic Time-stamp: <Last changed by martino on Monday 03 May 2021 at CEST 12:14:44>
  ! Module for search of new points

  ! Module for the input parameter definition
  !USE MOD_PARAMETERS, ONLY: search_type ???? TO IMPLEMENT
  USE MOD_PARAMETERS, ONLY:  npar, par_step, par_bnd1, par_bnd2, par_fix
  ! Module for likelihood
  USE MOD_LIKELIHOOD
  ! Module for cluster analysis
  USE MOD_CLUSTER_ANALYSIS

  IMPLICIT NONE

CONTAINS
  
  
!!$  ! Select option of several search modes
!!$  !#####################################################################################################################
!!$  
!!$  SUBROUTINE SEARCH_NEW_POINT(min_ll,nlive,live_like,live,new_live_like,new_live)
!!$    ! Main search function
!!$    USE MOD_PARAMETERS, ONLY: search_method
!!$
!!$    REAL(8), INTENT(IN) :: min_ll
!!$    INTEGER(4), INTENT(IN) :: nlive
!!$    REAL(8), DIMENSION(nlive), INTENT(IN) :: live_like
!!$    REAL(8), DIMENSION(nlive,npar), INTENT(IN) :: live
!!$    REAL(8), INTENT(OUT) :: new_live_like
!!$    REAL(8), DIMENSION(npar), INTENT(OUT) :: new_live
!!$
!!$    ! Select the search method
!!$    IF (search_method.eq.'RANDOM_WALK') THEN
!!$    CALL LAWN_MOWER_ROBOT(min_ll,nlive,live_like,live)
!!$    ELSE
!!$       WRITE(*,*) 'Error of the search type name in Mod_search_new_point module'
!!$       WRITE(*,*) 'Check the manual and the input file'
!!$       STOP
!!$    END IF
!!$
!!$
!!$  END SUBROUTINE SEARCH_NEW_POINT

  !#####################################################################################################################

  SUBROUTINE RANDOM_WALK_SEARCH(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)
  ! SUBROUTINE LAWN_MOWER_ROBOT(min_ll,nlive,live_like,live,new_live_like,new_live)

    USE MOD_PARAMETERS, ONLY: nlive, sdfraction, njump, maxtries, maxntries, &
         cluster_yn, cluster_method, distance_limit, bandwidth, par_in

    ! MCMC search function from Leo's ideas and mine
    INTEGER(4), INTENT(IN) :: n, itry
    REAL(8), INTENT(IN) :: min_live_like
    REAL(8), INTENT(IN), DIMENSION(nlive) :: live_like
    REAL(8), INTENT(IN), DIMENSION(nlive,npar) :: live
    REAL(8), INTENT(OUT) :: live_like_new
    REAL(8), DIMENSION(npar), INTENT(OUT) :: live_new
    INTEGER(4), INTENT(OUT) :: icluster, ntries
    LOGICAL, INTENT(OUT) :: too_many_tries
    ! MCMC new point search variables
    REAL(8) :: gval=0.
    REAL(8), DIMENSION(npar) :: live_ave, live_var, live_sd, start_jump, new_jump
    INTEGER(4) :: istart=0, n_ntries=0
    REAL(8), DIMENSION(nlive,npar) :: live_selected
    ! Other variables
    INTEGER(4) :: i=0, l=0, irn=0
    REAL(8) :: rn
    INTEGER(4) :: n_call_cluster_it, test
    INTEGER(4), INTENT(INOUT) :: n_call_cluster

    ! Find new live points
    ! ----------------------------------FIND_POINT_MCMC------------------------------------
    new_jump = par_in
    live_new = 0.
    live_like_new = 0.
    live_ave = 0.
    live_var = 0.
    live_sd  = 0.
    ntries   = 0
    n_ntries = 0
    too_many_tries = .false.
    
    n_call_cluster_it=0

    ! Select a live point as starting point
    ntries = 0
    CALL RANDOM_NUMBER(rn)
    istart= FLOOR((nlive-1)*rn+1)
    !istart= FLOOR((nlive-1)*RAND(0)+1)
    !istart= FLOOR((nlive-1)*rng(irn)+1)
    !irn = irn + 1
    start_jump = live(istart,:)


    ! Calculate momenta of the live points
400 IF (cluster_on) THEN
       ! Identify cluster appartenance
       icluster = p_cluster(istart)
       ! Get for the specific cluster if the cluster analysis is on
       ! Standard deviation
       live_sd(:) = cluster_std(icluster,:)
       IF(cluster_std(icluster,1).EQ.0.) THEN
          ! If the cluster is formed only from one point, take the standard standard deviation
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
       ! and mean
       live_ave(:) = cluster_mean(icluster,:)
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
500 CONTINUE
    DO i=1,njump
501    CONTINUE
       ntries = ntries + 1
       !$OMP PARALLEL DO
       DO l=1,npar
          IF (par_fix(l).NE.1) THEN
502          CALL RANDOM_NUMBER(rn)
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
       IF (LOGLIKELIHOOD(new_jump).GT.min_live_like) THEN
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

             !
             WRITE(*,*) 'Too many tries to find new live points for try n.', &
             itry,'!!!! More than',maxtries,&
             'n_ntries =',n_ntries,' over ', maxntries, 'n. step =', n

             ! If you already did too much tries, gave up or start a cluster analysis
             IF (n_ntries.GE.maxntries) THEN

                   
                IF (cluster_yn.EQ.'y'.OR.cluster_yn.EQ.'Y') THEN
                   
                   IF(n_call_cluster_it>=3) THEN
                     WRITE(*,*) 'Too many cluster analysis for an iteration'
                     WRITE(*,*) 'Change cluster recognition parameters'
                     STOP
                   END IF
                   IF(n_call_cluster>=10) THEN
                     WRITE(*,*) 'Too many cluster analysis'
                     WRITE(*,*) 'Change cluster recognition parameters'
                     STOP
                   END IF
                   
                   WRITE(*,*) 'Performing cluster analysis. Number of step = ', n
                   !
                   !write(*,*) nlive, npar, cluster_yn, cluster_method, bandwidth, distance_limit
                   !pause
                   CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
                   ! outputs: p_cluster ! flag of number of appartenance cluster for each live point
                   cluster_on = .true.
                   n_ntries = 0
                   n_call_cluster_it=n_call_cluster_it+1
                   n_call_cluster=n_call_cluster+1

                   ! Choose a new random live point and restart all
                   CALL RANDOM_NUMBER(rn)
                   istart= FLOOR((nlive-1)*rn+1)
                   start_jump = live(istart,:)

                   GOTO 400

                ELSE
                   WRITE(*,*) 'Too many tries to find new live points for try n.', itry, '!!!! More than ', maxtries*maxntries
                   WRITE(*,*) 'We take the data as they are :-~'
                   too_many_tries = .true.
                   RETURN
                END IF
             END IF
             ! Some test for desesperate seeking (for presence of several maxima)


             ! DO CLUSTER ANALYSIS if selected, or use other randomizations
             ! For all live points, assign a cluster number, which is used to calculate specific standard deviation
             ! for the search of the new point

             IF(.not.cluster_on) THEN
                ! Store the present live points as they are
                !OPEN(99,FILE='nf_intermediate_live_points.dat',STATUS= 'UNKNOWN')
                !WRITE(99,*) '# n step =',  n-1
                !WRITE(99,*) '# n tries =', n_ntries
                !WRITE(99,*) '# Evidence of the step =', evstep(n-1)
                !WRITE(99,*) '# Evidence accuracy =',  ADDLOG(evsum,live_like(nlive) + tlnrest(n-1)) - evsum
                !WRITE(99,*) '# n     lnlikelihood     parameters'
                !DO j=1,nlive
                !   WRITE(99,*) j, live_like(j), live(j,:)
                !END DO
                !CLOSE(99)

                ! Alternate the two techniques to find a new life point
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

                IF (LOGLIKELIHOOD(new_jump).GT.min_live_like) THEN
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
                ! Leo's technique only, go between the average and this point
                !$OMP PARALLEL DO
                DO l=1,npar
                   IF (par_fix(l).NE.1) THEN
                      CALL RANDOM_NUMBER(rn)
                      new_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rn
                   END IF
                END DO
                !$OMP END PARALLEL DO
                IF (LOGLIKELIHOOD(new_jump).GT.min_live_like) THEN
                   start_jump = new_jump
                ELSE
                   ! Choose a new random live point and restart all
                   CALL RANDOM_NUMBER(rn)
                   istart= FLOOR((nlive-1)*rn+1)
                   start_jump = live(istart,:)
                END IF
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
    IF (LOGLIKELIHOOD(new_jump).LT.min_live_like) GOTO 500

    ! Take the last point after jumps as new livepoint
    live_new = new_jump
    live_like_new = LOGLIKELIHOOD(new_jump)

    RETURN  
    ! ------------------------------------------------------------------------------------


  END SUBROUTINE SEARCH_NEW_POINT


  !#####################################################################################################################


END MODULE MOD_SEARCH_NEW_POINT
