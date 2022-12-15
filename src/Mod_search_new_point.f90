MODULE MOD_SEARCH_NEW_POINT
  ! Automatic Time-stamp: <Last changed by martino on Monday 03 May 2021 at CEST 12:14:44>
  ! Module for search of new points

  ! Module for the input parameter definition
  USE MOD_PARAMETERS, ONLY:  npar, par_step, par_bnd1, par_bnd2, par_fix
  ! Module for likelihood
  USE MOD_LIKELIHOOD
  ! Module for cluster analysis
  USE MOD_CLUSTER_ANALYSIS

  IMPLICIT NONE

CONTAINS

  ! TODO(César): Remap all of these writes to the mpi_status_process

  SUBROUTINE SEARCH_NEW_POINT(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)
    ! Main search function
    USE MOD_PARAMETERS, ONLY: search_method, nlive

    INTEGER(4), INTENT(IN) :: n, itry
    REAL(8), INTENT(IN) :: min_live_like
    REAL(8), INTENT(IN), DIMENSION(nlive) :: live_like
    REAL(8), INTENT(IN), DIMENSION(nlive,npar) :: live
    REAL(8), INTENT(OUT) :: live_like_new
    REAL(8), DIMENSION(npar), INTENT(OUT) :: live_new
    INTEGER(4), INTENT(OUT) :: icluster, ntries
    LOGICAL, INTENT(OUT) :: too_many_tries
    INTEGER(4), INTENT(INOUT) :: n_call_cluster

    ! Select the search method
    IF (search_method.eq.'RANDOM_WALK') THEN
        CALL RANDOM_WALK(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)
    ELSE IF(search_method .EQ. 'UNIFORM') THEN
        CALL UNIFORM(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)
    ELSE IF(search_method .EQ. 'SLICE_SAMPLING') THEN
        CALL SLICE_SAMPLING(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)
    ELSE IF(search_method .EQ. 'SLICE_SAMPLING_ADAPT') THEN
        CALL SLICE_SAMPLING_ADAPT(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)
    ELSE
        WRITE(*,*) 'Error of the search type name in Mod_search_new_point module'
        WRITE(*,*) 'Check the manual and the input file'
        STOP
    END IF


  END SUBROUTINE SEARCH_NEW_POINT

  !#####################################################################################################################

  SUBROUTINE RANDOM_WALK(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)
  ! SUBROUTINE LAWN_MOWER_ROBOT(min_ll,nlive,live_like,live,new_live_like,new_live)


    USE MOD_PARAMETERS, ONLY: nlive, search_par1, search_par2, maxtries, maxntries, &
         cluster_yn, cluster_method, par_in

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
    REAL(8) :: sdfraction
    INTEGER(4) :: njump
    REAL(8) :: loglike

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
    sdfraction=search_par1
    njump=INT(search_par2)

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
            !  WRITE(*,*) 'Too many tries to find new live points for try n.', &
            !  itry,'!!!! More than',maxtries,&
            !  'n_ntries =',n_ntries,' over ', maxntries, 'n. step =', n

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

                  !  WRITE(*,*) 'Performing cluster analysis. Number of step = ', n
                   !
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
                  !  WRITE(*,*) 'Too many tries to find new live points for try n.', itry, '!!!! More than ', maxtries*maxntries
                  !  WRITE(*,*) 'We take the data as they are :-~'
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
    loglike = LOGLIKELIHOOD(new_jump)
    IF (loglike.LT.min_live_like) GOTO 500

    ! Take the last point after jumps as new livepoint
    live_new = new_jump
    live_like_new = loglike

    RETURN
    ! ------------------------------------------------------------------------------------


  END SUBROUTINE RANDOM_WALK

  SUBROUTINE UNIFORM(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)

    USE MOD_PARAMETERS, ONLY: nlive, search_par1, search_par2, maxtries, maxntries, &
         cluster_yn, cluster_method, par_in

    ! uniform search function
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
    INTEGER(4) :: i=0, l=0, irn=0, j
    REAL(8) :: rn, sd_mean, frac
    INTEGER(4) :: n_call_cluster_it, test
    INTEGER(4), INTENT(INOUT) :: n_call_cluster
    INTEGER(4) :: nb_cube, njump
    REAL(8) :: loglike
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
    frac=search_par1
    n_call_cluster_it=0
    nb_cube=0
    njump=INT(search_par2)

    ! Select a live point as starting point
    ntries = 0
    CALL RANDOM_NUMBER(rn)
    istart= FLOOR((nlive-1)*rn+1)
    start_jump = live(istart,:)


    ! Calculate momenta of the live points
600 IF (cluster_on) THEN
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
    sd_mean=SUM(live_sd)*1.0/npar


    ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
700 CONTINUE
    DO i=1,njump
701    CONTINUE
       ntries = ntries + 1
       !$OMP PARALLEL DO
       DO l=1,npar
          IF (par_fix(l).NE.1) THEN
702          CALL RANDOM_NUMBER(rn)
             rn=2*frac*live_sd(l)*rn-frac*live_sd(l)
             new_jump(l) = start_jump(l) + rn !select randomly a point in the box around the starting point
             IF (new_jump(l).LT.par_bnd1(l).OR.new_jump(l).GT.par_bnd2(l)) THEN
                ntries = ntries + 1
                GOTO 702
             END IF
          END IF
       END DO
        !$OMP END PARALLEL DO


       ! Check if the new point is inside the parameter volume defined by the minimum likelihood of the live points
       IF (LOGLIKELIHOOD(new_jump).GT.min_live_like) THEN
           !$OMP PARALLEL DO REDUCTION(+:nb_cube)
           DO j=1, nlive
              IF(ALL(ABS(new_jump-live(j,:)) .LT. frac*live_sd)) nb_cube=nb_cube+1
           END DO
          !$OMP END PARALLEL DO
          CALL RANDOM_NUMBER(rn)
          IF(rn.GT.1./nb_cube) THEN
            nb_cube=0
            ntries=ntries+1
            GOTO 700
          END IF
          start_jump = new_jump
          frac=search_par1
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

                   GOTO 600

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

             frac=frac/2.0

             IF(.not.cluster_on) THEN
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
                GOTO 700
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
                GOTO 600
             END IF
          END IF    ! End loop for ntries > maxtries

          GOTO 701  ! Restart looking for new points without changing the starting point

       END IF  ! End of loop with failure for likelihood value
    END DO

    ! Final check of the last point for gaussian priors
    DO l=1,npar
       IF(par_fix(l).NE.1) THEN
          IF(par_step(l).GT.0) THEN
             ! maximum of the distribution is 1 (and is not normalized to 1 as the previous line)
             gval = dexp(-(new_jump(l)-par_in(l))**2/(2*par_step(l)**2))
             CALL RANDOM_NUMBER(rn)
             IF (rn.GT.gval) GOTO 700
          END IF
       END IF
    END DO


    ! Last(maybe useless) check
    loglike = LOGLIKELIHOOD(new_jump)
    IF(loglike.LT.min_live_like) GOTO 700

    ! Take the last point after jumps as new livepoint
    live_new = new_jump
    live_like_new = loglike

    RETURN
    ! ------------------------------------------------------------------------------------


  END SUBROUTINE UNIFORM

  !----------------------------------------------------------------------------------------------------------------------------------------

SUBROUTINE SLICE_SAMPLING(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)

    !inspired from polychord code

    USE MOD_PARAMETERS, ONLY: nlive, search_par1, search_par2, maxtries, maxntries, &
         cluster_yn, cluster_method, par_in
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
    REAL(8), DIMENSION(npar) :: live_ave, live_var, live_sd, start_jump_comp, new_jump_comp
    INTEGER(4) :: istart=0, n_ntries=0
    REAL(8), DIMENSION(nlive,npar) :: live_selected
    ! Other variables
    INTEGER(4) :: i=0, l=0, irn=0,j=0, kr=0, kl=0
    REAL(8) :: rn, sd_mean
    INTEGER(4) :: n_call_cluster_it, test
    INTEGER(4), INTENT(INOUT) :: n_call_cluster
    INTEGER(4) :: dim_eff
    REAL(8), DIMENSION(:,:),ALLOCATABLE :: basis
    !REAL(8), DIMENSION(:,:), ALLOCATABLE :: basis
    REAL(8), DIMENSION(:), ALLOCATABLE :: left, right, left_prov,right_prov
    REAL(8), DIMENSION(:,:), ALLOCATABLE  :: live_cov, live_chol, inv_chol
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: live_nf
    REAL(8), DIMENSION(:), ALLOCATABLE :: start_jump, new_jump, start_jump_t, new_jump_t
    INTEGER(4), DIMENSION(:), ALLOCATABLE :: par_var
    REAL(8) :: part_like, size_jump, size_jump_save, loglike
    LOGICAL :: test_bnd
    INTEGER(4) :: init_fail, test2, njump
    ! Find new live points
    ! ----------------------------------FIND_POINT_MCMC------------------------------------
    live_new = 0.
    live_like_new = 0.
    live_ave = 0.
    live_var = 0.
    live_sd  = 0.
    ntries   = 0
    n_ntries = 0
    too_many_tries = .false.
    n_call_cluster_it=0
    dim_eff=npar-SUM(par_fix) !number of parameters not fixed
    size_jump=search_par1
    init_fail=0
    size_jump_save=size_jump
    njump=INT(search_par2) !number of bases used
    IF(.NOT. ALLOCATED(basis)) ALLOCATE(basis(dim_eff,dim_eff), &
        live_cov(dim_eff,dim_eff),live_chol(dim_eff,dim_eff),inv_chol(dim_eff,dim_eff))
    IF(.NOT. ALLOCATED(live_nf)) ALLOCATE(live_nf(nlive,dim_eff))
    IF(.NOT. ALLOCATED(start_jump)) ALLOCATE(start_jump(dim_eff),new_jump(dim_eff), &
        start_jump_t(dim_eff),new_jump_t(dim_eff))
    IF(.NOT. ALLOCATED(par_var)) ALLOCATE(par_var(dim_eff))
    IF(.NOT. ALLOCATED(left)) ALLOCATE(left(dim_eff),right(dim_eff), &
        left_prov(dim_eff),right_prov(dim_eff))


    ! Select a live point as starting point
    ntries = 0


    IF(cluster_yn.EQ.'y'.OR.cluster_yn.EQ.'Y') THEN
       IF(MOD(n,10*nlive).EQ.0 .AND. n .NE. 0) THEN
             WRITE(*,*) 'Performing cluster analysis. Number of step = ', n
             CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
            cluster_on = .true.
             n_ntries = 0
       END IF
    END IF


    j=1
    DO i=1,npar
      IF(par_fix(i).NE.1) THEN
        par_var(j)=i
        j=j+1
      END IF
    END DO


500 CALL RANDOM_NUMBER(rn)
    istart= FLOOR((nlive-1)*rn+1)
    start_jump_comp = live(istart,:)

    !Select only the variables that are not fixed
    start_jump=start_jump_comp(par_var)
    live_nf=live(:,par_var)

    ! Calculate momenta of the live points
600 IF (cluster_on) THEN
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
    sd_mean=SUM(live_sd)*1.0/npar

    CALL MAT_COV(live_nf,nlive,dim_eff,istart,live_cov)
    CALL CHOLESKY(dim_eff,live_cov,live_chol)
    CALL TRIANG_INV(dim_eff,live_chol,inv_chol)
    start_jump_t=matmul(inv_chol,start_jump) !start jump in the new space

    ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
700 CONTINUE
    DO i=1,njump
       !ntries=0
       !Generate a random orthonormal basis
       CALL BASE_O_N(dim_eff,basis)
       DO l=1,dim_eff
         size_jump=search_par1
701      CONTINUE
         !Place the first interval around the start_jump
702      CALL RANDOM_NUMBER(rn)
         left=start_jump_t-rn*basis(:,l)*size_jump
         right=left+basis(:,l)*size_jump
         CALL RANDOM_NUMBER(rn)
         kl=FLOOR(100*rn)
         kr=100-kl
         CALL TEST_BND_SUB(dim_eff,left,par_var,live_chol,test_bnd)
         IF(.NOT.test_bnd) GOTO 202
         !Extend the interval left then right
         !size_jump_save=size_jump
         j=1
         CALL PART_LIKE_SUB(dim_eff,left,live_chol,part_like)
         DO WHILE(part_like.GT.min_live_like .AND. j<=kl) !check if the left boundary verifies the condition
           left=left-basis(:,l)*size_jump
           CALL TEST_BND_SUB(dim_eff,left,par_var,live_chol,test_bnd)
           IF(.NOT. test_bnd) THEN
             GOTO 202
           END IF
           CALL PART_LIKE_SUB(dim_eff,left,live_chol,part_like)
           j=j+1
         END DO
202      CALL TEST_BND_SUB(dim_eff,right,par_var,live_chol,test_bnd)
         IF(.NOT.test_bnd) GOTO 203
         j=1
         CALL PART_LIKE_SUB(dim_eff,right,live_chol,part_like)
         DO WHILE(part_like.GT.min_live_like .AND. j<=kr) !check if the right boundary verifies the condition
           right=right+basis(:,l)*size_jump
           CALL TEST_BND_SUB(dim_eff,right,par_var,live_chol,test_bnd)
           IF(.NOT. test_bnd) THEN
             GOTO 203
           END IF
           CALL PART_LIKE_SUB(dim_eff,right,live_chol,part_like)
           j=j+1
         END DO
         !Select new point
203      CALL RANDOM_NUMBER(rn)
         new_jump_t=left+rn*(right-left)
         ntries=1+ntries
         CALL TEST_BND_SUB(dim_eff,new_jump_t,par_var,live_chol,test_bnd) !check if the new point is inside the sampled space
         IF(test_bnd) THEN
            CALL PART_LIKE_SUB(dim_eff,new_jump_t,live_chol,part_like) !check if the new point verifies the condition
         ELSE
            part_like=min_live_like-1
         END IF
         DO WHILE(part_like.LT.min_live_like)
204        ntries=ntries+1
           IF(ntries .GT. maxtries) THEN
             n_ntries=n_ntries+1
             ntries=0

             WRITE(*,*) 'Too many tries to find new live points for try n.', &
             itry,'!!!! More than',maxtries,&
             'n_ntries =',n_ntries,' over ', maxntries, 'n. step =', n

             IF(n_ntries .GE. maxntries) THEN
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
                 CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
                 cluster_on = .true.
                 n_ntries = 0
                 n_call_cluster_it=n_call_cluster_it+1
                 n_call_cluster=n_call_cluster+1
                 GOTO 500
               ELSE
                 WRITE(*,*) 'Too many tries to find new live points for try n.', itry, '!!!! More than ', maxtries
                 WRITE(*,*) 'We take the data as they are :-~'
                 too_many_tries = .true.
                 RETURN
               END IF
             END IF
             GOTO 500
           END IF
           IF(DOT_PRODUCT(start_jump_t-new_jump_t,start_jump_t-left)>0) THEN !change the left or right bondary accordingly
             left=new_jump_t
           ELSE
             right=new_jump_t
           END IF
           CALL RANDOM_NUMBER(rn)
           new_jump_t=left+rn*(right-left)
           CALL TEST_BND_SUB(dim_eff,new_jump_t,par_var,live_chol,test_bnd)
           IF(.NOT. test_bnd) GOTO 204
           CALL PART_LIKE_SUB(dim_eff,new_jump_t,live_chol,part_like)
         END DO
         start_jump_t=new_jump_t
       END DO
     END DO

    !Find the new point in the original space
    new_jump=matmul(live_chol,new_jump_t)
    j=1
    DO l=1,npar
      IF(par_fix(l).NE.1) THEN
        new_jump_comp(l)=new_jump(j)
        j=j+1
      ELSE
        new_jump_comp(l)=par_in(l)
      END IF
    END DO

    ! Final check of the last point for gaussian priors
    DO l=1,npar
       IF(par_fix(l).NE.1) THEN
          IF(par_step(l).GT.0) THEN
             ! maximum of the distribution is 1 (and is not normalized to 1 as the previous line)
             gval = dexp(-(new_jump_comp(l)-par_in(l))**2/(2*par_step(l)**2))
             CALL RANDOM_NUMBER(rn)
             IF (rn.GT.gval) GOTO 700
          END IF
       END IF
    END DO

    ! Last(maybe useless) check
    loglike = LOGLIKELIHOOD(new_jump_comp)
    IF(loglike.LT.min_live_like) GOTO 700

!    DO l=1,npar
!      IF (new_jump_comp(l).LT.par_bnd1(l).OR.new_jump_comp(l).GT.par_bnd2(l)) THEN
!        WRITE(*,*) l, .FALSE.
!      END IF
!    END DO

    ! Take the last point after jumps as new livepoint
    live_new = new_jump_comp
    live_like_new = loglike
    ntries=ntries/dim_eff

END SUBROUTINE SLICE_SAMPLING

!----------------------------------------------------------------------------------------------

SUBROUTINE SLICE_SAMPLING_ADAPT(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)

    !inspired from polychord code

    USE MOD_PARAMETERS, ONLY: nlive, search_par1, search_par2, maxtries, maxntries, &
         cluster_yn, cluster_method, par_in
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
    REAL(8), DIMENSION(npar) :: live_ave, live_var, live_sd, start_jump_comp, new_jump_comp
    INTEGER(4) :: istart=0, n_ntries=0
    REAL(8), DIMENSION(nlive,npar) :: live_selected
    ! Other variables
    INTEGER(4) :: i=0, l=0, irn=0,j=0, k=0
    REAL(8) :: rn, sd_mean
    INTEGER(4) :: n_call_cluster_it, test
    INTEGER(4), INTENT(INOUT) :: n_call_cluster
    INTEGER(4) :: dim_eff
    REAL(8), DIMENSION(:,:),ALLOCATABLE :: basis
    !REAL(8), DIMENSION(:,:), ALLOCATABLE :: basis
    REAL(8), DIMENSION(:), ALLOCATABLE :: left, right, left_prov,right_prov
    REAL(8), DIMENSION(:,:), ALLOCATABLE  :: live_cov, live_chol, inv_chol
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: live_nf
    REAL(8), DIMENSION(:), ALLOCATABLE :: start_jump, new_jump, start_jump_t, new_jump_t
    INTEGER(4), DIMENSION(:), ALLOCATABLE :: par_var
    REAL(8) :: part_like, size_jump, size_jump_save, loglike
    LOGICAL :: test_bnd
    INTEGER(4) :: init_fail, test2, njump
    ! Find new live points
    ! ----------------------------------FIND_POINT_MCMC------------------------------------
    live_new = 0.
    live_like_new = 0.
    live_ave = 0.
    live_var = 0.
    live_sd  = 0.
    ntries   = 0
    n_ntries = 0
    too_many_tries = .false.
    n_call_cluster_it=0
    dim_eff=npar-SUM(par_fix) !number of parameters not fixed
    size_jump=search_par1
    init_fail=0
    size_jump_save=size_jump
    njump=INT(search_par2) !number of bases used
    IF(.NOT. ALLOCATED(basis)) ALLOCATE(basis(dim_eff,dim_eff), &
        live_cov(dim_eff,dim_eff),live_chol(dim_eff,dim_eff),inv_chol(dim_eff,dim_eff))
    IF(.NOT. ALLOCATED(live_nf)) ALLOCATE(live_nf(nlive,dim_eff))
    IF(.NOT. ALLOCATED(start_jump)) ALLOCATE(start_jump(dim_eff),new_jump(dim_eff), &
        start_jump_t(dim_eff),new_jump_t(dim_eff))
    IF(.NOT. ALLOCATED(par_var)) ALLOCATE(par_var(dim_eff))
    IF(.NOT. ALLOCATED(left)) ALLOCATE(left(dim_eff),right(dim_eff), &
        left_prov(dim_eff),right_prov(dim_eff))


    ! Select a live point as starting point
    ntries = 0


    IF(cluster_yn.EQ.'y'.OR.cluster_yn.EQ.'Y') THEN
       IF(MOD(n,10*nlive).EQ.0 .AND. n .NE. 0) THEN
             WRITE(*,*) 'Performing cluster analysis. Number of step = ', n
             CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
            cluster_on = .true.
             n_ntries = 0
       END IF
    END IF


    j=1
    DO i=1,npar
      IF(par_fix(i).NE.1) THEN
        par_var(j)=i
        j=j+1
      END IF
    END DO


500 CALL RANDOM_NUMBER(rn)
    istart= FLOOR((nlive-1)*rn+1)
    start_jump_comp = live(istart,:)

    !Select only the variables that are not fixed
    start_jump=start_jump_comp(par_var)
    live_nf=live(:,par_var)

    ! Calculate momenta of the live points
600 IF (cluster_on) THEN
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
    sd_mean=SUM(live_sd)*1.0/npar

    CALL MAT_COV(live_nf,nlive,dim_eff,istart,live_cov)
    CALL CHOLESKY(dim_eff,live_cov,live_chol)
    CALL TRIANG_INV(dim_eff,live_chol,inv_chol)
    start_jump_t=matmul(inv_chol,start_jump) !start jump in the new space

    ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
700 CONTINUE
    DO i=1,njump
       !ntries=0
       !Generate a random orthonormal basis
       CALL BASE_O_N(dim_eff,basis)
       DO l=1,dim_eff
         size_jump=search_par1
701      CONTINUE
         !Place the first interval around the start_jump
702      CALL RANDOM_NUMBER(rn)
         left=start_jump_t-rn*basis(:,l)*size_jump
         right=left+basis(:,l)*size_jump
         CALL TEST_BND_SUB(dim_eff,left,par_var,live_chol,test_bnd)
         IF(.NOT. test_bnd) THEN
           init_fail=init_fail+1
           IF(init_fail>5) THEN
             size_jump=size_jump/2.
             init_fail=0
           END IF
           GOTO 702
         END IF
         CALL TEST_BND_SUB(dim_eff,right,par_var,live_chol,test_bnd)
         IF(.NOT. test_bnd) THEN
           init_fail=init_fail+1
           IF(init_fail>5) THEN
             size_jump=size_jump/2.
             init_fail=0
           END IF
           GOTO 702
         END IF
         !Extend the interval left then right
         size_jump_save=size_jump
         j=1
         k=1
         CALL PART_LIKE_SUB(dim_eff,left,live_chol,part_like)
         DO WHILE(part_like.GT.min_live_like .AND. j<=10) !check if the left boundary verifies the condition
200        left_prov=left-basis(:,l)*size_jump
           CALL TEST_BND_SUB(dim_eff,left_prov,par_var,live_chol,test_bnd)
           IF(.NOT. test_bnd) THEN
             IF(k>3) EXIT
             size_jump=size_jump/2
             GOTO 200
             k=k+1
           !  EXIT
           END IF
           left=left_prov
           CALL PART_LIKE_SUB(dim_eff,left,live_chol,part_like)
           j=j+1
         END DO
         size_jump=size_jump_save
         j=1
         k=1
         CALL PART_LIKE_SUB(dim_eff,right,live_chol,part_like)
         DO WHILE(part_like.GT.min_live_like .AND. j<=10) !check if the right boundary verifies the condition
201           right_prov=right+basis(:,l)*size_jump
           CALL TEST_BND_SUB(dim_eff,right_prov,par_var,live_chol,test_bnd)
           IF(.NOT. test_bnd) THEN
             IF(k>3) EXIT
             size_jump=size_jump/2
             GOTO 201
           !  EXIT
           END IF
           right=right_prov
           CALL PART_LIKE_SUB(dim_eff,right,live_chol,part_like)
           j=j+1
         END DO
         size_jump=size_jump_save
         !Select new point
         CALL RANDOM_NUMBER(rn)
         new_jump_t=left+rn*(right-left)
         ntries=1+ntries
         CALL PART_LIKE_SUB(dim_eff,new_jump_t,live_chol,part_like) !check if the new point verifies the condition
         CALL TEST_BND_SUB(dim_eff,new_jump_t,par_var,live_chol,test_bnd) !check if the new point is inside the sampled space
         DO WHILE(part_like.LT.min_live_like .OR. .NOT. test_bnd)
           ntries=ntries+1
           IF(ntries .GT. maxtries) THEN
             n_ntries=n_ntries+1
             ntries=0

             WRITE(*,*) 'Too many tries to find new live points for try n.', &
             itry,'!!!! More than',maxtries,&
             'n_ntries =',n_ntries,' over ', maxntries, 'n. step =', n

             IF(n_ntries .GE. maxntries) THEN
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
                 CALL MAKE_CLUSTER_ANALYSIS(nlive,npar,live)
                 cluster_on = .true.
                 n_ntries = 0
                 n_call_cluster_it=n_call_cluster_it+1
                 n_call_cluster=n_call_cluster+1
                 GOTO 500
               ELSE
                 WRITE(*,*) 'Too many tries to find new live points for try n.', itry, '!!!! More than ', maxtries
                 WRITE(*,*) 'We take the data as they are :-~'
                 too_many_tries = .true.
                 RETURN
               END IF
             END IF
             GOTO 500
           END IF
           IF(DOT_PRODUCT(start_jump_t-new_jump_t,start_jump_t-left)>0) THEN !change the left or right bondary accordingly
             left=new_jump_t
           ELSE
             right=new_jump_t
           END IF
           CALL RANDOM_NUMBER(rn)
           new_jump_t=left+rn*(right-left)
           CALL PART_LIKE_SUB(dim_eff,new_jump_t,live_chol,part_like)
           CALL TEST_BND_SUB(dim_eff,new_jump_t,par_var,live_chol,test_bnd)
         END DO
         start_jump_t=new_jump_t
       END DO
     END DO

    !Find the new point in the original space
    new_jump=matmul(live_chol,new_jump_t)
    j=1
    DO l=1,npar
      IF(par_fix(l).NE.1) THEN
        new_jump_comp(l)=new_jump(j)
        j=j+1
      ELSE
        new_jump_comp(l)=par_in(l)
      END IF
    END DO

    ! Final check of the last point for gaussian priors
    DO l=1,npar
       IF(par_fix(l).NE.1) THEN
          IF(par_step(l).GT.0) THEN
             ! maximum of the distribution is 1 (and is not normalized to 1 as the previous line)
             gval = dexp(-(new_jump_comp(l)-par_in(l))**2/(2*par_step(l)**2))
             CALL RANDOM_NUMBER(rn)
             IF (rn.GT.gval) GOTO 700
          END IF
       END IF
    END DO

    ! Last(maybe useless) check
    loglike = LOGLIKELIHOOD(new_jump)
    IF(loglike.LT.min_live_like) GOTO 700

!    DO l=1,npar
!      IF (new_jump_comp(l).LT.par_bnd1(l).OR.new_jump_comp(l).GT.par_bnd2(l)) THEN
!        WRITE(*,*) l, .FALSE.
!      END IF
!    END DO

    ! Take the last point after jumps as new livepoint
    live_new = new_jump_comp
    live_like_new = loglike
    ntries=ntries/dim_eff

END SUBROUTINE SLICE_SAMPLING_ADAPT

SUBROUTINE BASE_O_N(D,base) !generates an orthonormal basis
  INTEGER(4), INTENT(IN) :: D
  REAL(8), DIMENSION(D,D), INTENT(INOUT) :: base
  INTEGER(4) :: i,j

  DO i=1,D
    DO j=1,D
      CALL random_number(base(i,j))
    END DO
  END DO
  base(:,1)=base(:,1)/NORM2(base(:,1))
  DO i=2,D
    DO j=1,i-1
      base(:,i)=base(:,i)-DOT_PRODUCT(base(:,i),base(:,j))*base(:,j)
    END DO
    base(:,j)=base(:,j)/NORM2(base(:,j))
  END DO
END SUBROUTINE BASE_O_N

SUBROUTINE MAT_COV(pts,np,D,istart,cov) !calculates the covariance matrix
  INTEGER(4), INTENT(IN) :: np, D, istart
  REAL(8), DIMENSION(np,D), INTENT(IN) :: pts
  REAL(8), DIMENSION(D,D), INTENT(OUT) :: cov
  REAL(8), DIMENSION(D) :: mean, mean_prov
  INTEGER(4) :: i,j, icluster

  IF (cluster_on) THEN
    ! Identify cluster appartenance
    icluster = p_cluster(istart)
    ! Get for the specific cluster if the cluster analysis is on
    ! Standard deviation
    IF(cluster_np(icluster).LT.2*D) THEN
       mean=pts(istart,:)
       !$OMP PARALLEL DO
       DO i=1,D
         mean_prov(i)=SUM(pts(:,i))/np
       END DO
       !$OMP END PARALLEL DO
       !$OMP PARALLEL DO
       DO i=1,D
         !$OMP PARALLEL DO
         DO j=i,D
           cov(i,j)=SUM((pts(:,i)-mean_prov(i))*(pts(:,j)-mean_prov(j)))/(np-1)
           cov(j,i)=cov(i,j)
         END DO
         !$OMP END PARALLEL DO
       END DO
       !$OMP END PARALLEL DO
    ELSE IF(icluster==0) THEN
       !$OMP PARALLEL DO
       DO i=1,D
         mean(i)=SUM(pts(:,i))/np
       END DO
       !$OMP END PARALLEL DO
       !$OMP PARALLEL DO
       DO i=1,D
         !$OMP PARALLEL DO
         DO j=i,D
           cov(i,j)=SUM((pts(:,i)-mean(i))*(pts(:,j)-mean(j)))/(np-1)
           cov(j,i)=cov(i,j)
         END DO
         !$OMP END PARALLEL DO
       END DO
       !$OMP END PARALLEL DO
    ELSE
       !$OMP PARALLEL DO
       DO i=1,D
         mean(i)=SUM(pts(:,i),MASK=(p_cluster==icluster))/cluster_np(icluster)
       END DO
       !$OMP END PARALLEL DO
       !$OMP PARALLEL DO
       DO i=1,D
         !$OMP PARALLEL DO
         DO j=i,D
           cov(i,j)=SUM((pts(:,i)-mean(i))*(pts(:,j)-mean(j)),MASK=(p_cluster==icluster))/(cluster_np(icluster)-1)
           cov(j,i)=cov(i,j)
         END DO
         !$OMP END PARALLEL DO
       END DO
       !$OMP END PARALLEL DO
    END IF
  ELSE
    !$OMP PARALLEL DO
    DO i=1,D
      mean(i)=SUM(pts(:,i))/np
    END DO
    !$OMP END PARALLEL DO
    !$OMP PARALLEL DO
     DO i=1,D
       !$OMP PARALLEL DO
      DO j=i,D
        cov(i,j)=SUM((pts(:,i)-mean(i))*(pts(:,j)-mean(j)))/(np-1)
        cov(j,i)=cov(i,j)
      END DO
      !$OMP END PARALLEL DO
    END DO
    !$OMP END PARALLEL DO
  END IF
END SUBROUTINE MAT_COV

SUBROUTINE CHOLESKY(D,cov,chol) !calculates the cholesky decomposition of the covariance matrix
   INTEGER(4), INTENT(IN) :: D
   REAL(8), DIMENSION(D,D), INTENT(IN) :: cov
   REAL(8), DIMENSION(D,D), INTENT(OUT) :: chol
   INTEGER(4) :: i,j,k
   chol=0
   chol(1,1)=SQRT(cov(1,1))
   DO i=2,D
     chol(i,1)=cov(i,1)/chol(1,1)
   END DO
   DO i=2,D
     chol(i,i)=cov(i,i)
     DO k=1,i-1
       chol(i,i)=chol(i,i)-chol(i,k)**2
     END DO
     chol(i,i)=SQRT(chol(i,i))
     DO j=i+1,D
       chol(j,i)=cov(i,j)
       DO k=1,i-1
         chol(j,i)=chol(j,i)-chol(i,k)*chol(j,k)
       END DO
       chol(j,i)=chol(j,i)/chol(i,i)
     END DO
   END DO
END SUBROUTINE CHOLESKY

SUBROUTINE TRIANG_INV(D,mat,mat_inv) !calculates the inverse of the cholesky decomposition
   INTEGER(4), INTENT(IN) :: D
   REAL(8), DIMENSION(D,D), INTENT(IN) :: mat
   REAL(8), DIMENSION(D,D), INTENT(OUT) :: mat_inv
   INTEGER(4) :: i,j,k
   mat_inv=0
   DO i=1,D
     mat_inv(i,i)=1./mat(i,i)
   END DO

   DO j=1,D-1
     DO i=1,D-j
       DO k=1,j
          mat_inv(i+j,i)=mat_inv(i+j,i)-mat(i+k,i)*mat_inv(i+j,i+k)
       END DO
       mat_inv(i+j,i)=mat_inv(i+j,i)/mat(i,i)
     END DO
   END DO
END SUBROUTINE TRIANG_INV

SUBROUTINE PART_LIKE_SUB(D,pt,chol,part_like) !calculates the likelihood for a point in the new space
  USE MOD_PARAMETERS, ONLY: par_in
  INTEGER(4), INTENT(IN) :: D
  REAL(8), DIMENSION(D), INTENT(IN) :: pt
  REAL(8), DIMENSION(D,D), INTENT(IN) :: chol
  REAL(8), INTENT(OUT) :: part_like
  REAL(8), DIMENSION(D) :: pt_t
  REAL(8), DIMENSION(npar) :: pt_comp
  INTEGER(4) :: l, j
  pt_t=matmul(chol,pt)
  j=1
  DO l=1,npar
    IF(par_fix(l).NE.1) THEN
      pt_comp(l)=pt_t(j)
      j=j+1
    ELSE
      pt_comp(l)=par_in(l)
    END IF
  END DO
  !write(*,*) pt_comp ! ?????
  part_like=LOGLIKELIHOOD(pt_comp)
END SUBROUTINE PART_LIKE_SUB


SUBROUTINE TEST_BND_SUB(D,pt,par_var,chol,test_bnd) !checks if a point in the new space is in the sampled space
  INTEGER(4), INTENT(IN) :: D
  REAL(8), DIMENSION(D), INTENT(IN) :: pt
  INTEGER(4), DIMENSION(D), INTENT(IN) :: par_var
  REAL(8), DIMENSION(D,D), INTENT(IN) :: chol
  LOGICAL, INTENT(OUT) :: test_bnd
  REAL(8), DIMENSION(D) :: pt_t
  REAL(8), DIMENSION(D) :: bnd1, bnd2
  INTEGER(4) :: l
  bnd1=par_bnd1(par_var)
  bnd2=par_bnd2(par_var)
  pt_t=matmul(chol,pt)
  test_bnd=.TRUE.
  DO l=1,D
    IF (pt_t(l).LT.bnd1(l).OR.pt_t(l).GT.bnd2(l)) THEN
      test_bnd=.FALSE.
      EXIT
    END IF
  END DO
END SUBROUTINE TEST_BND_SUB

 !#####################################################################################################################


END MODULE MOD_SEARCH_NEW_POINT
