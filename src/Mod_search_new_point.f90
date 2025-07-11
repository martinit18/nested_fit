MODULE MOD_SEARCH_NEW_POINT
  ! Automatic Time-stamp: <Last changed by martino on Friday 15 November 2024 at CET 22:36:28>
  ! Module for search of new points

  ! Module for the input parameter definition
  USE MOD_PARAMETERS !, ONLY:  npar, par_step, par_bnd1, par_bnd2, par_fix, searchid
  ! Module for likelihood
  USE MOD_LIKELIHOOD_GEN, ONLY: LOGLIKELIHOOD
  ! Module for cluster analysis
  USE MOD_CLUSTER_ANALYSIS
  ! Module for math
  USE MOD_MATH
  ! Module for perfprof
  USE MOD_PERFPROF
  ! Module for covariance matrix
  USE MOD_COVARIANCE_MATRIX
  
  !$ USE OMP_LIB

  IMPLICIT NONE

  REAL(8), DIMENSION(:), ALLOCATABLE :: live_ave, live_sd
!#ifdef LAPACK_ON
!  EXTERNAL :: dpotrf, dtrtri, dtrmv
!#endif

CONTAINS
  
  ! TODO(César): Remap all of these writes to the mpi_status_process
  
  SUBROUTINE INIT_SEARCH_METHOD()
#ifdef OPENMPI_ON
    INTEGER(4) :: mpi_ierror
#endif
    IF (search_method.eq.'RANDOM_WALK') THEN
       searchid = 0
    ELSE IF(search_method.EQ.'UNIFORM') THEN
       searchid = 1
    ELSE IF(search_method.EQ.'SLICE_SAMPLING_TRANSF') THEN
       searchid = 2
    ELSE IF(search_method.EQ.'SLICE_SAMPLING') THEN
       searchid = 3
    ELSE IF(search_method.EQ.'SLICE_SAMPLING_ADAPT') THEN
       searchid = 4
    ELSE
       CALL LOG_ERROR_HEADER()
       CALL LOG_ERROR('Error of the search type name in Mod_search_new_point module.')
       CALL LOG_ERROR('Check the manual and the input file.')
       CALL LOG_ERROR_HEADER()
       CALL HALT_EXECUTION()
    END IF
  END SUBROUTINE INIT_SEARCH_METHOD
  
   

  SUBROUTINE MAKE_LIVE_MEAN_SD(live)  
   
  USE MOD_PARAMETERS, ONLY: nlive

  REAL(8), INTENT(IN), DIMENSION(nlive,npar) :: live
  INTEGER(4) :: i
  REAL(8), DIMENSION(npar) :: live_var

  ALLOCATE(live_ave(npar),live_sd(npar))

  live_ave = 0.
  live_var = 0.
  live_sd  = 0.
  
!!$OMP PARALLEL DO
  DO i=1,npar
     IF(par_fix(i).NE.1) THEN
        CALL MEANVAR(live(:,i),nlive,live_ave(i),live_var(i))
     ELSE
        live_ave(i) = par_in(i)
     END IF
  END DO
!!$OMP END PARALLEL DO
  live_sd = DSQRT(live_var)
   
  END SUBROUTINE MAKE_LIVE_MEAN_SD

  !#####################################################################################################################

  SUBROUTINE REMAKE_LIVE_MEAN_SD(live)  
   
   USE MOD_PARAMETERS, ONLY: nlive
 
   REAL(8), INTENT(IN), DIMENSION(nlive,npar) :: live
   INTEGER(4) :: i
   REAL(8), DIMENSION(npar) :: live_var
 
   live_ave = 0.
   live_var = 0.
   live_sd  = 0.
   
 !!$OMP PARALLEL DO
   DO i=1,npar
      IF(par_fix(i).NE.1) THEN
         CALL MEANVAR(live(:,i),nlive,live_ave(i),live_var(i))
         live_sd(i) = DSQRT(live_var(i))
      END IF
   END DO
 !!$OMP END PARALLEL DO
    
   END SUBROUTINE REMAKE_LIVE_MEAN_SD

   !#####################################################################################################################

   SUBROUTINE DEALLOCATE_SEARCH_NEW_POINTS 
      ! Close allocated memory
    
      DEALLOCATE(live_ave,live_sd)
   
   END SUBROUTINE DEALLOCATE_SEARCH_NEW_POINTS
  
  !#####################################################################################################################

  SUBROUTINE SEARCH_NEW_POINT(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries)
    ! Main search function
    USE MOD_PARAMETERS, ONLY: nlive


    INTEGER(4), INTENT(IN) :: n, itry
    REAL(8), INTENT(IN) :: min_live_like
    REAL(8), INTENT(IN), DIMENSION(nlive) :: live_like
    REAL(8), INTENT(IN), DIMENSION(nlive,npar) :: live
    REAL(8), INTENT(OUT) :: live_like_new
    REAL(8), DIMENSION(npar), INTENT(OUT) :: live_new
    INTEGER(4), INTENT(OUT) :: icluster, ntries
    LOGICAL, INTENT(OUT) :: too_many_tries

    PROFILED(SEARCH_NEW_POINT)

    ! Select the search method
    SELECT CASE (searchid)
      CASE (0)
         CALL RANDOM_WALK(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries)
      CASE (1)
         CALL UNIFORM(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries)
      CASE (2)
         CALL SLICE_SAMPLING_TRANSF(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries)
      CASE (3)
         CALL SLICE_SAMPLING(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries)
      CASE (4)
         CALL SLICE_SAMPLING_ADAPT(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries)
    END SELECT

  END SUBROUTINE SEARCH_NEW_POINT

  !#####################################################################################################################

  SUBROUTINE RANDOM_WALK(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries)
  ! SUBROUTINE LAWN_MOWER_ROBOT(min_ll,nlive,live_like,live,new_live_like,new_live)


    USE MOD_PARAMETERS, ONLY: nlive, search_par1, search_par2, maxtries, maxntries, par_in

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
    REAL(8) :: gval
    REAL(8), DIMENSION(npar) :: start_jump, new_jump
    INTEGER(4) :: istart, n_ntries
    ! Other variables
    INTEGER(4) :: i, l, irn
    REAL(8) :: rn
    REAL(8) :: sdfraction
    INTEGER(4) :: njump
    REAL(8) :: loglike


    PROFILED(RANDOM_WALK)

    ! Find new live points
    ! ----------------------------------FIND_POINT_MCMC------------------------------------
    new_jump = par_in
    live_new = 0.
    live_like_new = 0.
    ntries   = 1
    istart   = 0
    n_ntries = 0
    too_many_tries = .false.
    gval = 0.

    sdfraction=search_par1
    njump=INT(search_par2)

    ! Select a live point as starting point
    CALL RANDOM_NUMBER(rn)
    istart= FLOOR((nlive-1)*rn+1)
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
          CALL REMAKE_LIVE_MEAN_SD(live)
       END IF
       ! and mean
       live_ave(:) = cluster_mean(icluster,:)
    END IF



    ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
500 CONTINUE
    ntries = 0
    DO i=1,njump
501    CONTINUE
       ntries = ntries + 1
       DO l=1,npar
          IF (par_fix(l).NE.1) THEN
502          CALL RANDOM_NUMBER(rn)
             new_jump(l) = start_jump(l) + live_sd(l)*sdfraction*(2.*rn-1.)
             IF (new_jump(l).LT.par_bnd1(l).OR.new_jump(l).GT.par_bnd2(l)) THEN
                ntries = ntries + 1
                GOTO 502
             END IF
          END IF
       END DO
       !!$OMP END PARALLEL DO

       ! Check if the new point is inside the parameter volume defined by the minimum likelihood of the live points
       IF (LOGLIKELIHOOD(npar, new_jump).GT.min_live_like) THEN
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
             ntries = 1

             !
            !  WRITE(*,*) 'Too many tries to find new live points for try n.', &
            !  itry,'!!!! More than',maxtries,&
            !  'n_ntries =',n_ntries,' over ', maxntries, 'n. step =', n

             ! If you already did too much tries, gave up or start a cluster analysis
             IF (n_ntries.GE.maxntries) THEN
                too_many_tries=.true.
                live_like_new=min_live_like
                live_new = live(1,:)
                GOTO 600
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
                   !!$OMP PARALLEL DO
                   DO l=1,npar
                      IF (par_fix(l).NE.1) THEN
                         CALL RANDOM_NUMBER(rn)
                         istart= FLOOR((nlive-1)*rn+1)
                         new_jump(l) = live(istart,l)
                      END IF
                   END DO
                   !!$OMP END PARALLEL DO
                ELSE
                   ! Leo's technique, go between the average and this point
                   !!$OMP PARALLEL DO
                   DO l=1,npar
                      IF (par_fix(l).NE.1) THEN
                         CALL RANDOM_NUMBER(rn)
                         new_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rn
                      END IF
                   END DO
                   !!$OMP END PARALLEL DO
                END IF
                !

                IF (LOGLIKELIHOOD(npar, new_jump).GT.min_live_like) THEN
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
                !!$OMP PARALLEL DO
                DO l=1,npar
                   IF (par_fix(l).NE.1) THEN
                      CALL RANDOM_NUMBER(rn)
                      new_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rn
                   END IF
                END DO
                !!$OMP END PARALLEL DO
                IF (LOGLIKELIHOOD(npar, new_jump).GT.min_live_like) THEN
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
    loglike = LOGLIKELIHOOD(npar, new_jump)
    IF (loglike.LT.min_live_like) GOTO 500

    ! Take the last point after jumps as new livepoint
    live_new = new_jump
    live_like_new = loglike

600 CONTINUE

    RETURN
    ! ------------------------------------------------------------------------------------


  END SUBROUTINE RANDOM_WALK

  SUBROUTINE UNIFORM(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries)

    USE MOD_PARAMETERS, ONLY: nlive, search_par1, search_par2, maxtries, maxntries, par_in

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
    REAL(8) :: gval
    REAL(8), DIMENSION(npar) :: start_jump, new_jump
    INTEGER(4) :: istart, n_ntries
    ! Other variables
    INTEGER(4) :: i, l, irn, j
    REAL(8) :: rn, frac
    INTEGER(4) :: nb_cube, njump
    REAL(8) :: loglike

    PROFILED(UNIFORM)

    ! Find new live points
    ! ----------------------------------FIND_POINT_MCMC------------------------------------
    new_jump = par_in
    live_new = 0.
    live_like_new = 0.
    ntries   = 0
    istart   = 0
    n_ntries = 0
    too_many_tries = .false.
    gval = 0.
    frac=search_par1
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
          CALL REMAKE_LIVE_MEAN_SD(live)
       END IF
       ! and mean
       live_ave(:) = cluster_mean(icluster,:)
    END IF


    ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
700 CONTINUE
    DO i=1,njump
701    CONTINUE
       ntries = ntries + 1
       !!$OMP PARALLEL DO
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
        !!$OMP END PARALLEL DO


       ! Check if the new point is inside the parameter volume defined by the minimum likelihood of the live points
       IF (LOGLIKELIHOOD(npar, new_jump).GT.min_live_like) THEN
           !$OMP SIMD REDUCTION(+:nb_cube)
           DO j=1, nlive
              IF(ALL(ABS(new_jump-live(j,:)) .LT. frac*live_sd)) nb_cube=nb_cube+1
           END DO
          !$OMP END SIMD
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
                too_many_tries=.true.
                live_like_new=min_live_like
                live_new = live(1,:)
                GOTO 400
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
                   !!$OMP PARALLEL DO
                   DO l=1,npar
                      IF (par_fix(l).NE.1) THEN
                         CALL RANDOM_NUMBER(rn)
                         istart= FLOOR((nlive-1)*rn+1)
                         new_jump(l) = live(istart,l)
                      END IF
                   END DO
                   !!$OMP END PARALLEL DO
                ELSE
                   ! Leo's technique, go between the average and this point
                   !!$OMP PARALLEL DO
                   DO l=1,npar
                      IF (par_fix(l).NE.1) THEN
                         CALL RANDOM_NUMBER(rn)
                         new_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rn
                      END IF
                   END DO
                   !!$OMP END PARALLEL DO
                END IF
                !
                 IF (LOGLIKELIHOOD(npar, new_jump).GT.min_live_like) THEN
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
                !!$OMP PARALLEL DO
                DO l=1,npar
                   IF (par_fix(l).NE.1) THEN
                      CALL RANDOM_NUMBER(rn)
                      new_jump(l) = live_ave(l) + (new_jump(l) - live_ave(l))*rn
                   END IF
                END DO
                !!$OMP END PARALLEL DO
                IF (LOGLIKELIHOOD(npar, new_jump).GT.min_live_like) THEN
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
    loglike = LOGLIKELIHOOD(npar, new_jump)
    IF(loglike.LT.min_live_like) GOTO 700

    ! Take the last point after jumps as new livepoint
    live_new = new_jump
    live_like_new = loglike

400 CONTINUE

    RETURN
    ! ------------------------------------------------------------------------------------


  END SUBROUTINE UNIFORM

  !----------------------------------------------------------------------------------------------------------------------------------------

SUBROUTINE SLICE_SAMPLING_TRANSF(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries)

    !inspired from polychord code
    ! This is slice sampling performed in the transformed space

    USE MOD_PARAMETERS, ONLY: nlive, search_par1, search_par2, maxtries, maxntries, par_in
    INTEGER(4), INTENT(IN) :: n, itry
    REAL(8), INTENT(IN) :: min_live_like
    REAL(8), INTENT(IN), DIMENSION(nlive) :: live_like
    REAL(8), INTENT(IN), DIMENSION(nlive,npar) :: live
    REAL(8), INTENT(OUT) :: live_like_new
    REAL(8), DIMENSION(npar), INTENT(OUT) :: live_new
    INTEGER(4), INTENT(OUT) :: icluster, ntries
    LOGICAL, INTENT(OUT) :: too_many_tries
    ! MCMC new point search variables
    REAL(8) :: gval
    REAL(8), DIMENSION(npar) :: start_jump_comp, new_jump_comp
    INTEGER(4) :: istart, n_ntries
    ! Other variables
    INTEGER(4) :: i, l, j, kr, kl
    REAL(8) :: rn
    !INTEGER(4) :: dim_eff
    REAL(8), DIMENSION(:,:),ALLOCATABLE :: basis
    REAL(8), DIMENSION(:), ALLOCATABLE :: left, right, left_prov,right_prov
    REAL(8), DIMENSION(:,:), ALLOCATABLE  :: live_cov, live_chol, inv_chol
    REAL(8), DIMENSION(:), ALLOCATABLE :: start_jump, new_jump, start_jump_t, new_jump_t
    REAL(8) :: part_like, size_jump, size_jump_save, loglike
    LOGICAL :: test_bnd
    INTEGER(4) :: init_fail, njump

    PROFILED(SLICE_SAMPLING)
        
    ! Find new live points
    ! ----------------------------------FIND_POINT_MCMC------------------------------------
    live_new = 0.
    live_like_new = 0.
    ntries   = 0
    istart   = 0
    n_ntries = 0
    too_many_tries = .false.
    gval = 0.
    size_jump=search_par1
    init_fail=0
    size_jump_save=size_jump
    njump=INT(search_par2) !number of bases used
    IF(.NOT. ALLOCATED(basis)) ALLOCATE(basis(dim_eff,dim_eff), &
        live_cov(dim_eff,dim_eff),live_chol(dim_eff,dim_eff),inv_chol(dim_eff,dim_eff))
    !IF(.NOT. ALLOCATED(live_nf)) ALLOCATE(live_nf(nlive,dim_eff))
    IF(.NOT. ALLOCATED(start_jump)) ALLOCATE(start_jump(dim_eff),new_jump(dim_eff), &
        start_jump_t(dim_eff),new_jump_t(dim_eff))
    !IF(.NOT. ALLOCATED(par_var)) ALLOCATE(par_var(dim_eff))
    IF(.NOT. ALLOCATED(left)) ALLOCATE(left(dim_eff),right(dim_eff), &
        left_prov(dim_eff),right_prov(dim_eff))


    ! Select a live point as starting point
    ntries = 0


500 CALL RANDOM_NUMBER(rn)
    istart= FLOOR((nlive-1)*rn+1)
    start_jump_comp = live(istart,:)

    !Select only the variables that are not fixed
    start_jump=start_jump_comp(par_var)

    ! Calculate momenta of the live points
    IF (cluster_on) THEN
       ! Identify cluster appartenance
       icluster = p_cluster(istart)
       ! Get for the specific cluster if the cluster analysis is on
       ! Standard deviation
       live_sd(:) = cluster_std(icluster,:)
       IF(cluster_std(icluster,1).EQ.0.) THEN
          ! If the cluster is formed only from one point, take the standard standard deviation
          CALL REMAKE_LIVE_MEAN_SD(live)
       END IF
       ! and mean
       live_ave(:) = cluster_mean(icluster,:)
    END IF

    IF(cluster_on) THEN ! Get the covariance matrix and Cholesky decomposition for the correct cluster
       live_cov=mat_cov(:,:,icluster)
       live_chol=mat_chol(:,:,icluster)
    ELSE
       live_cov=mat_cov(:,:,1)
       live_chol=mat_chol(:,:,1)
    END IF
    ! Calculate the inverse of the Cholesky matrix
#ifdef LAPACK_ON
    inv_chol=live_chol
    CALL dtrtri('L','N',dim_eff,inv_chol,dim_eff,i)
    start_jump_t=start_jump
    CALL dtrmv('L','N','N',dim_eff,inv_chol,dim_eff,start_jump_t,1)
#else
    CALL TRIANG_INV(dim_eff,live_chol,inv_chol)
    start_jump_t=matmul(inv_chol,start_jump) !start jump in the new space
#endif


    ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
700 CONTINUE
    DO i=1,njump
       !ntries=0
       !Generate a random orthonormal basis
       CALL BASE_O_N(dim_eff,basis)
       DO l=1,dim_eff
         size_jump=search_par1
         CONTINUE
         !Place the first interval around the start_jump
         CALL RANDOM_NUMBER(rn)
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
         DO WHILE((.NOT.(part_like.GT.min_live_like)) .OR. (.NOT. test_bnd))
204        ntries=ntries+1
           IF(ntries .GT. maxtries) THEN
             n_ntries=n_ntries+1
             ntries=0

             WRITE(*,*) 'Too many tries to find new live points for try n.', &
             itry,'!!!! More than',maxtries,&
             'n_ntries =',n_ntries,' over ', maxntries, 'n. step =', n

             IF(n_ntries .GE. maxntries) THEN
               too_many_tries=.true.
               live_like_new=min_live_like
               live_new = live(1,:)
               GOTO 400
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
#ifdef LAPACK_ON
     new_jump=new_jump_t
     CALL dtrmv('L','N','N',dim_eff,live_chol,dim_eff,new_jump,1)
#else
     new_jump=matmul(live_chol,new_jump_t)
#endif
     ! Complete the new point with the fixed variables that were previously discarded
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
    loglike = LOGLIKELIHOOD(npar, new_jump_comp)
    IF(loglike.LT.min_live_like) GOTO 700


    ! Take the last point after jumps as new livepoint
    live_new = new_jump_comp
    live_like_new = loglike
400 ntries=ntries/dim_eff

END SUBROUTINE SLICE_SAMPLING_TRANSF

!----------------------------------------------------------------------------------------------

SUBROUTINE SLICE_SAMPLING(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries)

    !inspired from polychord code
    ! This is slice sampling performed in the initial space

    USE MOD_PARAMETERS, ONLY: nlive, search_par1, search_par2, maxtries, maxntries, par_in
    INTEGER(4), INTENT(IN) :: n, itry
    REAL(8), INTENT(IN) :: min_live_like
    REAL(8), INTENT(IN), DIMENSION(nlive) :: live_like
    REAL(8), INTENT(IN), DIMENSION(nlive,npar) :: live
    REAL(8), INTENT(OUT) :: live_like_new
    REAL(8), DIMENSION(npar), INTENT(OUT) :: live_new
    INTEGER(4), INTENT(OUT) :: icluster, ntries
    LOGICAL, INTENT(OUT) :: too_many_tries
    ! MCMC new point search variables
    REAL(8) :: gval
    REAL(8), DIMENSION(npar) :: start_jump, new_jump
    INTEGER(4) :: istart, n_ntries
    ! Other variables
    INTEGER(4) :: i, l, j, kr, kl, m
    REAL(8) :: rn
    REAL(8), DIMENSION(:,:),ALLOCATABLE :: basis_eff, basis
    REAL(8), DIMENSION(npar) :: left, right
    REAL(8), DIMENSION(:,:), ALLOCATABLE  :: live_cov, live_chol
    REAL(8) :: part_like, size_jump, size_jump_save, loglike
    LOGICAL :: test_bnd
    INTEGER(4) :: init_fail, njump
    ! Find new live points
    ! ----------------------------------FIND_POINT_MCMC------------------------------------
    live_new = 0.
    live_like_new = 0.
    ntries   = 0
    istart   = 0
    n_ntries = 0
    too_many_tries = .false.
    gval = 0.
    !dim_eff=npar-SUM(par_fix) !number of parameters not fixed
    size_jump=search_par1
    init_fail=0
    size_jump_save=size_jump
    njump=INT(search_par2) !number of bases used
    IF(.NOT. ALLOCATED(basis_eff)) ALLOCATE(basis_eff(dim_eff,dim_eff), basis(npar,dim_eff), &
        live_cov(dim_eff,dim_eff),live_chol(dim_eff,dim_eff))!, live_nf(nlive,dim_eff))


    ! Select a live point as starting point
    ntries = 0

500 CALL RANDOM_NUMBER(rn)
    istart= FLOOR((nlive-1)*rn+1)
    start_jump = live(istart,:)
    
    ! Calculate momenta of the live points
    IF (cluster_on) THEN
       ! Identify cluster appartenance
       icluster = p_cluster(istart)
       ! Get for the specific cluster if the cluster analysis is on
       ! Standard deviation
       live_sd(:) = cluster_std(icluster,:)
       IF(cluster_std(icluster,1).EQ.0.) THEN
          ! If the cluster is formed only from one point, take the standard standard deviation
          CALL REMAKE_LIVE_MEAN_SD(live)
       END IF
       ! and mean
       live_ave(:) = cluster_mean(icluster,:)
    END IF
    

    IF(cluster_on) THEN ! Get the covariance matrix and Cholesky decomposition for the correct cluster
       live_cov=mat_cov(:,:,icluster)
       live_chol=mat_chol(:,:,icluster)
    ELSE
       live_cov=mat_cov(:,:,1)
       live_chol=mat_chol(:,:,1)
    END IF

    ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
700 CONTINUE
    DO i=1,njump
       !ntries=0
       !Generate a random orthonormal basis
       CALL BASE_O_N(dim_eff,basis_eff) !basis in transformed space
       basis_eff=matmul(live_chol,basis_eff) !basis in real space
       basis=0
       basis(par_var,:)=basis_eff
       DO l=1,dim_eff
         size_jump=search_par1
         CONTINUE
         !Place the first interval around the start_jump
         CALL RANDOM_NUMBER(rn)
         left=start_jump-rn*basis(:,l)*size_jump
         right=left+basis(:,l)*size_jump
         CALL RANDOM_NUMBER(rn)
         kl=FLOOR(100*rn)
         kr=100-kl
         test_bnd=.TRUE.
         DO m=1,npar
            IF(left(m).LT.par_bnd1(m) .OR. left(m).GT.par_bnd2(m)) THEN
               test_bnd=.FALSE.
               EXIT
            ENDIF
         END DO
         IF(.NOT.test_bnd) GOTO 202
         !Extend the interval left then right
         !size_jump_save=size_jump
         j=1
         part_like=LOGLIKELIHOOD(npar, left)
         DO WHILE(part_like.GT.min_live_like .AND. j<=kl) !check if the left boundary verifies the condition
           left=left-basis(:,l)*size_jump
           test_bnd=.TRUE.
           DO m=1,npar
              IF(left(m).LT.par_bnd1(m) .OR. left(m).GT.par_bnd2(m)) THEN
                 test_bnd=.FALSE.
                 EXIT
              ENDIF
           END DO
           IF(.NOT. test_bnd) THEN
             GOTO 202
           END IF
           part_like=LOGLIKELIHOOD(npar, left)
           j=j+1
         END DO
202      test_bnd=.TRUE.
         DO m=1,npar
            IF(right(m).LT.par_bnd1(m) .OR. right(m).GT.par_bnd2(m)) THEN
               test_bnd=.FALSE.
               EXIT
            ENDIF
         END DO
         IF(.NOT.test_bnd) GOTO 203
         j=1
         part_like=LOGLIKELIHOOD(npar, right)
         !WRITE(*,*) 'Right', part_like, min_live_like
         DO WHILE(part_like.GT.min_live_like .AND. j<=kr) !check if the right boundary verifies the condition
           right=right+basis(:,l)*size_jump
           test_bnd=.TRUE.
           DO m=1,npar
              IF(right(m).LT.par_bnd1(m) .OR. right(m).GT.par_bnd2(m)) THEN
                 test_bnd=.FALSE.
                 EXIT
              ENDIF
           END DO
           IF(.NOT. test_bnd) THEN
             GOTO 203
           END IF
           part_like=LOGLIKELIHOOD(npar, right)
           j=j+1
         END DO
         !Select new point
203      CALL RANDOM_NUMBER(rn)
         new_jump=left+rn*(right-left)
         ntries=1+ntries
         test_bnd=.TRUE.
         DO m=1,npar
            IF(new_jump(m).LT.par_bnd1(m) .OR. new_jump(m).GT.par_bnd2(m) .AND. par_fix(m).NE.1) THEN
               test_bnd=.FALSE.
               EXIT
            ENDIF
         END DO !check if the new point is inside the sampled space
         IF(test_bnd) THEN
            part_like=LOGLIKELIHOOD(npar, new_jump) !check if the new point verifies the condition
         ELSE
            part_like=min_live_like-1
         END IF
         !WRITE(*,*) 'New jump', new_jump, part_like, min_live_like
         DO WHILE((.NOT.(part_like.GT.min_live_like)) .OR. (.NOT. test_bnd))
204        ntries=ntries+1
           IF(ntries .GT. maxtries) THEN
             n_ntries=n_ntries+1
             ntries=0

             WRITE(*,*) 'Too many tries to find new live points for try n.', &
             itry,'!!!! More than',maxtries,&
             'n_ntries =',n_ntries,' over ', maxntries, 'n. step =', n

             IF(n_ntries .GE. maxntries) THEN
               too_many_tries=.true.
               live_like_new=min_live_like
               live_new = live(1,:)
               GOTO 400
             END IF
             GOTO 500
           END IF
           IF(DOT_PRODUCT(start_jump-new_jump,start_jump-left)>0) THEN !change the left or right bondary accordingly
             left=new_jump
           ELSE
             right=new_jump
           END IF
           CALL RANDOM_NUMBER(rn)
           new_jump=left+rn*(right-left)
           test_bnd=.TRUE.
           DO m=1,npar
              IF(new_jump(m).LT.par_bnd1(m) .OR. new_jump(m).GT.par_bnd2(m)) THEN
                 test_bnd=.FALSE.
                 EXIT
              ENDIF
           END DO 
           IF(.NOT. test_bnd) GOTO 204
           part_like=LOGLIKELIHOOD(npar, new_jump)
         END DO
         start_jump=new_jump
       END DO
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
    loglike = LOGLIKELIHOOD(npar, new_jump)
    IF(loglike.LT.min_live_like) GOTO 700

    ! Take the last point after jumps as new livepoint
    live_new = new_jump
    live_like_new = loglike
400 ntries=ntries/dim_eff

END SUBROUTINE SLICE_SAMPLING

!----------------------------------------------------------------------------------------------

SUBROUTINE SLICE_SAMPLING_ADAPT(n,itry,min_live_like,live_like,live, &
       live_like_new,live_new,icluster,ntries,too_many_tries)

    !inspired from polychord code
    ! This is slice sampling with an adaptable size for the step

    USE MOD_PARAMETERS, ONLY: nlive, search_par1, search_par2, maxtries, maxntries, par_in
    INTEGER(4), INTENT(IN) :: n, itry
    REAL(8), INTENT(IN) :: min_live_like
    REAL(8), INTENT(IN), DIMENSION(nlive) :: live_like
    REAL(8), INTENT(IN), DIMENSION(nlive,npar) :: live
    REAL(8), INTENT(OUT) :: live_like_new
    REAL(8), DIMENSION(npar), INTENT(OUT) :: live_new
    INTEGER(4), INTENT(OUT) :: icluster, ntries
    LOGICAL, INTENT(OUT) :: too_many_tries
    ! MCMC new point search variables
    REAL(8) :: gval
    REAL(8), DIMENSION(npar) :: start_jump_comp, new_jump_comp
    INTEGER(4) :: istart, n_ntries
    ! Other variables
    INTEGER(4) :: i, l, j, k
    REAL(8) :: rn
    REAL(8), DIMENSION(:,:),ALLOCATABLE :: basis
    REAL(8), DIMENSION(:), ALLOCATABLE :: left, right, left_prov,right_prov
    REAL(8), DIMENSION(:,:), ALLOCATABLE  :: live_cov, live_chol, inv_chol
    REAL(8), DIMENSION(:), ALLOCATABLE :: start_jump, new_jump, start_jump_t, new_jump_t
    REAL(8) :: part_like, size_jump, size_jump_save, loglike
    LOGICAL :: test_bnd
    INTEGER(4) :: init_fail, njump

    PROFILED(SLICE_SAMPLING_ADAPT)

    ! Find new live points
    ! ----------------------------------FIND_POINT_MCMC------------------------------------
    live_new = 0.
    live_like_new = 0.
    ntries   = 0
    istart   = 0
    n_ntries = 0
    too_many_tries = .false.
    gval = 0.
    size_jump=search_par1
    init_fail=0
    size_jump_save=size_jump
    njump=INT(search_par2) !number of bases used
    IF(.NOT. ALLOCATED(basis)) ALLOCATE(basis(dim_eff,dim_eff), &
        live_cov(dim_eff,dim_eff),live_chol(dim_eff,dim_eff),inv_chol(dim_eff,dim_eff))
    IF(.NOT. ALLOCATED(start_jump)) ALLOCATE(start_jump(dim_eff),new_jump(dim_eff), &
        start_jump_t(dim_eff),new_jump_t(dim_eff))
    IF(.NOT. ALLOCATED(left)) ALLOCATE(left(dim_eff),right(dim_eff), &
        left_prov(dim_eff),right_prov(dim_eff))


    ! Select a live point as starting point
    ntries = 0

500 CALL RANDOM_NUMBER(rn)
    istart= FLOOR((nlive-1)*rn+1)
    start_jump_comp = live(istart,:)

    !Select only the variables that are not fixed
    start_jump=start_jump_comp(par_var)
    !live_nf=live(:,par_var)

    ! Calculate momenta of the live points
    IF (cluster_on) THEN
       ! Identify cluster appartenance
       icluster = p_cluster(istart)
       ! Get for the specific cluster if the cluster analysis is on
       ! Standard deviation
       live_sd(:) = cluster_std(icluster,:)
       IF(cluster_std(icluster,1).EQ.0.) THEN
          ! If the cluster is formed only from one point, take the standard standard deviation
          CALL REMAKE_LIVE_MEAN_SD(live)
       END IF
       ! and mean
       live_ave(:) = cluster_mean(icluster,:)
    END IF

    IF(cluster_on) THEN ! Get the covariance matrix and Cholesky decomposition for the correct cluster
       live_cov=mat_cov(:,:,icluster)
       live_chol=mat_chol(:,:,icluster)
    ELSE
       live_cov=mat_cov(:,:,1)
       live_chol=mat_chol(:,:,1)
    END IF
    CALL TRIANG_INV(dim_eff,live_chol,inv_chol) ! Calculate the inverse of the Cholesky matrix
    start_jump_t=matmul(inv_chol,start_jump) ! start jump in the new space

    ! Make several consecutive casual jumps in the region with loglikelyhood > minlogll
700 CONTINUE
    DO i=1,njump
       !ntries=0
       !Generate a random orthonormal basis
       CALL BASE_O_N(dim_eff,basis)
       DO l=1,dim_eff
         size_jump=search_par1
         CONTINUE
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
             IF(k>10) EXIT
             size_jump=size_jump/2.
             k=k+1
             GOTO 200
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
             IF(k>10) EXIT
             size_jump=size_jump/2.
             k=k+1
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
         DO WHILE((.NOT.(part_like.GT.min_live_like)) .OR. .NOT. test_bnd)
           ntries=ntries+1
           IF(ntries .GT. maxtries) THEN
             n_ntries=n_ntries+1
             ntries=0

             WRITE(*,*) 'Too many tries to find new live points for try n.', &
             itry,'!!!! More than',maxtries,&
             'n_ntries =',n_ntries,' over ', maxntries, 'n. step =', n

             IF(n_ntries .GE. maxntries) THEN
               too_many_tries=.true.
               live_like_new=min_live_like
               live_new = live(1,:)
               GOTO 400
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
    ! Complete the new point with the fixed variables that were previously discarded
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
    loglike = LOGLIKELIHOOD(npar, new_jump_comp)
    IF(loglike.LT.min_live_like) GOTO 700

    ! Take the last point after jumps as new livepoint
    live_new = new_jump_comp
    live_like_new = loglike
400 ntries=ntries/dim_eff
END SUBROUTINE SLICE_SAMPLING_ADAPT

SUBROUTINE BASE_O_N(D,base) !generates an orthonormal basis
  INTEGER(4), INTENT(IN) :: D
  REAL(8), DIMENSION(D,D), INTENT(INOUT) :: base
  INTEGER(4) :: i,j

  PROFILED(BASE_O_N)

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

SUBROUTINE TRIANG_INV(D,mat,mat_inv) !calculates the inverse of the cholesky decomposition
   INTEGER(4), INTENT(IN) :: D
   REAL(8), DIMENSION(D,D), INTENT(IN) :: mat
   REAL(8), DIMENSION(D,D), INTENT(OUT) :: mat_inv
   INTEGER(4) :: i,j,k

   PROFILED(TRIANG_INV)

   mat_inv=0
   !!$OMP SIMD
   DO i=1,D
     mat_inv(i,i)=1./mat(i,i)
   END DO

   !!$OMP SIMD
   DO j=1,D-1
     !!$OMP SIMD
     DO i=1,D-j
       !!$OMP SIMD
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

  PROFILED(PART_LIKE_SUB)

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
  part_like=LOGLIKELIHOOD(npar, pt_comp)
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

  PROFILED(TEST_BND_SUB)

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
