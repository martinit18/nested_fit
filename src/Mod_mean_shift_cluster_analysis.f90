MODULE MOD_MEAN_SHIFT_CLUSTER_ANALYSIS
  ! Automatic Time-stamp: <Last changed by martino on Friday 06 March 2020 at CET 14:11:01>
  ! Module for cluster analysis for point in n dimensions
 
  ! Module for the input parameter definition
  USE MOD_PARAMETERS
  
  IMPLICIT NONE
 
  LOGICAL :: cluster_on = .false.
  INTEGER(4) :: np=0, ndim=0, ncluster=0
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: cluster_std, cluster_mean
  INTEGER(4), ALLOCATABLE, DIMENSION(:) :: p_cluster, cluster_np



CONTAINS

  !--------------------------------------------------------------------------------------------------------------



  SUBROUTINE MAKE_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)
    ! From a group of np points p of dimension ndim, determine the clusters
    ! using distance and bandwidth as parameters
    ! INPUTS
    USE Mod_timestamp, only: timestamp
    INTEGER(4), INTENT(IN) :: np_in, ndim_in
    REAL(8), INTENT(IN), DIMENSION(np_in,ndim_in) :: p_in
    ! Other variables
    REAL(8), DIMENSION(np_in,ndim_in) :: p, p_mean_shift, p_mean_old
    REAL(8), PARAMETER :: accuracy = 1e-7, accuracy_cluster = 1e-2
    REAL(8) :: dist, weight, val_max, val_min, actual_accuracy, max_accuracy=0.
    REAL(8), DIMENSION(ndim_in) :: num, dem
    INTEGER(4), PARAMETER :: iter_max=30, ncluster_max=500
    INTEGER(4) :: i, j, k, l, nn, min_nn
    REAL(8), DIMENSION(ncluster_max,ndim_in) :: mean_cluster
    LOGICAL :: accuracy_reached

    np = np_in
    ndim = ndim_in
    min_nn = np

    p = 0.
    p_mean_shift = 0.
    p_mean_old = 0.

    ! Allocate the variables if needed
    IF(.NOT.cluster_on) ALLOCATE(p_cluster(np))
    p_cluster = 0


    ! Calculate min and max and renormalize the point dimensions
    DO l=1,ndim
       val_max = maxval(p_in(:,l))
       val_min = minval(p_in(:,l))
       p(:,l) = (p_in(:,l)-val_min)/(val_max-val_min)
    END DO


    ! Before the main algorithm
    p_mean_shift = p
    accuracy_reached = .false.
    actual_accuracy = 0.

!!$    OPEN (UNIT=10, FILE='nf_meanshift_initial.dat', STATUS='unknown')
!!$    DO l=1,np
!!$       WRITE(10,*) p_mean_shift(l,:)
!!$    END DO
!!$    CLOSE(10)

    ! Run the algorithm -----------------------------------------------------------------------------------
    ! Stop when the mean values do not move anymore
    WRITE(*,*) 'Starting cluster analysis'
    DO i=1,iter_max
       p_mean_old  = p_mean_shift

       ! Calculate the neighbor point mean value for the selected point
       max_accuracy = 0.
       !!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(p_mean_old,p_mean_shift,max_accuracy,min_nn)
       DO j=1,np
          ! Scan all other points for calculating the mean....
          num = 0.
          dem = 0.
          nn = 0
          DO k=1,np
             IF (j.NE.k) THEN
                dist = NORM2(p_mean_old(j,:)-p_mean_old(k,:))
                ! ... that are close enough
                IF (dist.LE.distance_limit) THEN
                   nn = nn + 1
                   IF(cluster_method.EQ.'g'.OR.cluster_method.EQ.'G') THEN
                      ! Calculate the baricenter for any dimension with gaussian kernel
                      weight = GAUSSIAN_KERNEL(dist,bandwidth)
                      DO l=1,ndim
                         num(l) = num(l) + p(k,l)*weight
                         dem(l) = dem(l) + weight
                         !write(*,*) i,j,k,l, nn, num(l), dem(l)
                      END DO
                      ! Caluclate baricenter with flat kernel
                   ELSEIF(cluster_method.EQ.'f'.OR.cluster_method.EQ.'F') THEN
                      DO l=1,ndim
                         num(l) = num(l) + p(k,l)
                         dem(l) = dem(l) + 1
                      END DO
                   END IF
                END IF
             END IF
          END DO
          ! Calculate the new mean
          IF(nn.EQ.0) THEN
!             WRITE(*,*) 'Too few neighbouring points. Try to change the limit distance'
             p_mean_shift(j,:) =  p_mean_old(j,:)
          ELSE
             p_mean_shift(j,:) = num(:)/dem(:)
             actual_accuracy = NORM2(p_mean_shift(j,:)-p_mean_old(j,:))
             max_accuracy = max(actual_accuracy,max_accuracy)
             !write(*,*) max_accuracy
          END IF
          ! Calculate the minimal number of neighbours
          min_nn = min(min_nn,nn)
          !write(*,*) i,j,nn, actual_accuracy, max_accuracy
          !pause
       END DO
       !!$OMP END PARALLEL DO

       WRITE(*,*) 'n_iteration = ', i, 'present accuracy = ', max_accuracy

       OPEN (UNIT=10, FILE='nf_meanshift_check.dat', STATUS='unknown')
       DO l=1,np
          WRITE(10,*) p_mean_shift(l,:)
       END DO
       CLOSE(10)
       !write(*,*) bandwidth, distance_limit, cluster_method
       !pause

       ! Accuracy reached. Exit the loop
       IF(max_accuracy.LE.accuracy) EXIT

    END DO


!    WRITE(*,*) 'n_iteration = ', i, 'minimal n. of neighbours = ', min_nn, 'present accuracy = ', max_accuracy

    IF(i.EQ.iter_max) THEN
       WRITE(*,*) 'Maximal number of iteration is reached. Change something'
    END IF


    ! Determine the number of clusters ----------------------------------------------------------------
    ncluster = 1
    ! First point
    mean_cluster(1,:) = p_mean_shift(1,:)
    p_cluster(1) = 1
    ! Check other points
    DO j=2,np
       ! Check if it is equal to other mean values
       DO k=1,ncluster
          IF (NORM2(p_mean_shift(j,:)-mean_cluster(k,:)).LE.(accuracy_cluster)) THEN
             p_cluster(j) = k
             !write(*,*) j, k
             GOTO 100
          END IF
       END DO
       IF(p_cluster(j).EQ.0) THEN
          ncluster = ncluster + 1
          mean_cluster(ncluster,:) = p_mean_shift(j,:)
          p_cluster(j) = ncluster
          !write(*,*) j, k, ncluster, NORM2(p_mean_shift(j,:)-mean_cluster(k,:)), 'new cluster'
       END IF
100    CONTINUE
    END DO
    WRITE(*,*) 'Number of cluster found = ', ncluster, 'Minimal n. of neighbours = ', min_nn

    IF(ncluster.EQ.ncluster_max) THEN
       WRITE(*,*) 'Maximal number of clusters. Change something'
       STOP
    END IF

    ! Write file for further analysis and check of cluster recognition
    OPEN (UNIT=10, FILE='nf_meanshift_final_'//timestamp()//'.dat', STATUS='unknown')
    DO l=1,np
       WRITE(10,*) p_cluster(l), p_in(l,:)
    END DO
    CLOSE(10)


    ! Allocate and calculate the standard deviations of the clusters -------------------------------------
    IF(ALLOCATED(cluster_std)) DEALLOCATE(cluster_std,cluster_mean,cluster_np)
    ALLOCATE(cluster_std(ncluster,ndim),cluster_mean(ncluster,ndim),cluster_np(ncluster))
    cluster_std = 0.
    cluster_mean = 0.
    cluster_np = 0

    ! Calculate standard deviation and mean of the clusters
    !!$OMP PARALLEL DO
    DO k=1,ncluster
!       WRITE(*,*) 'Computing mean and std. dev. of cluster ', k
       CALL MAKE_CLUSTER_STD(p_in,k)
    END DO
    !!$OMP END PARALLEL DO

    OPEN (UNIT=10, FILE='nf_meanshift_mean_std.dat', STATUS='unknown')
    DO k=1,ncluster
       DO l=1,ndim
          WRITE(10,*) k, cluster_np(k), l, cluster_mean(k,l), cluster_std(k,l)
       END DO
    END DO
    CLOSE(10)


  END SUBROUTINE MAKE_CLUSTER_ANALYSIS


  !--------------------------------------------------------------------------------------------------------------



  SUBROUTINE DEALLOCATE_CLUSTER()
    ! Deallocate variables

    DEALLOCATE(p_cluster,cluster_std,cluster_mean,cluster_np)

  END SUBROUTINE DEALLOCATE_CLUSTER


  !--------------------------------------------------------------------------------------------------------------



  SUBROUTINE MAKE_CLUSTER_STD(p,icl)
    ! Make analysis of the selected cluster
    REAL(8), DIMENSION(np,ndim), INTENT(IN) :: p
    INTEGER(4), INTENT(IN) :: icl

    INTEGER(4) :: np_cl, j, l
    REAL(8), DIMENSION(np,ndim) :: p_cl


    ! Recognize cluster elements------------------------------
    np_cl = 0
    p_cl = 0.
    DO j=1,np
       IF(p_cluster(j).EQ.icl) THEN
          np_cl = np_cl + 1
          p_cl(np_cl,:) = p(j,:)
       END IF
    END DO
    cluster_np(icl) = np_cl

    ! Compute average and std---------------------------------
    IF(cluster_np(icl).EQ.1) THEN
       cluster_mean(icl,:) = p_cl(1,:)
       cluster_std(icl,:) = 0.
    ELSE
       DO l=1,ndim
          cluster_mean(icl,l) = 0.
          cluster_std(icl,l) = 0.
          cluster_mean(icl,l) = SUM(p_cl(1:cluster_np(icl),l))/cluster_np(icl)
          cluster_std(icl,l) =  &
               DSQRT(SUM((p_cl(1:cluster_np(icl),l)-cluster_mean(icl,l))**2)/ &
               (cluster_np(icl)-1))
       END DO
    END IF

  END SUBROUTINE MAKE_CLUSTER_STD

  !--------------------------------------------------------------------------------------------------------------

  SUBROUTINE REMAKE_CLUSTER_STD(p,icl_new,icl_old)
    ! Remake analysis of the selected cluster
    REAL(8), DIMENSION(np,ndim), INTENT(IN) :: p
    INTEGER(4), INTENT(IN) :: icl_new,icl_old

    ! Remake the calculation of the cluster with the new element ...
    CALL MAKE_CLUSTER_STD(p,icl_new)
    ! ... and the old if there are not the same
    IF(icl_new.NE.icl_old) THEN
       ! if it is not empty
       IF(icl_old.GE.1) CALL MAKE_CLUSTER_STD(p,icl_old)
    END IF

  END SUBROUTINE REMAKE_CLUSTER_STD


  !--------------------------------------------------------------------------------------------------------------


  FUNCTION GAUSSIAN_KERNEL(x,sigma)
    ! Gaussian kernel
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8) :: x, sigma, GAUSSIAN_KERNEL

    GAUSSIAN_KERNEL = 1/(2*pi*sigma)*DEXP(-(x**2)/(2*sigma**2))

  END FUNCTION GAUSSIAN_KERNEL



!!$  ! ####################### OTHER USEFULL FUNCTIONS ##########################################################
!!$
!!$
!!$  FUNCTION GET_CLUSTER_STD(icluster)
!!$    ! Get the standar deviation of the selected cluster
!!$
!!$  END FUNCTION GET_CLUSTER_STD
!!$
!!$
!!$
!!$
!--------------------------------------------------------------------------------------------------------------




!##################################################################################################################

END MODULE MOD_MEAN_SHIFT_CLUSTER_ANALYSIS
