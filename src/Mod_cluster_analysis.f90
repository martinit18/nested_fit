MODULE MOD_CLUSTER_ANALYSIS
  ! Automatic Time-stamp: <Last changed by martino on Monday 03 May 2021 at CEST 12:08:59>
  ! Module for cluster analysis for point in n dimensions
  !
  ! To eventually change to select and add other cluster analyses (eventually to be selected in the input file)


  IMPLICIT NONE

  LOGICAL :: cluster_on = .false.
  INTEGER(4) :: np=0, ndim=0, ncluster=0
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: cluster_std, cluster_mean
  INTEGER(4), ALLOCATABLE, DIMENSION(:) :: p_cluster, cluster_np



CONTAINS


  ! Select option of several cluster analyses
  !####################################################################################################################

  SUBROUTINE MAKE_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)

    USE MOD_PARAMETERS, ONLY: cluster_method

    INTEGER(4), INTENT(IN) :: np_in, ndim_in
    REAL(8), INTENT(IN), DIMENSION(np_in,ndim_in) :: p_in

    ! Select the search method
    IF (cluster_method.EQ.'f'.OR.cluster_method.EQ.'g') THEN
       CALL MEANSHIFT_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)
    ELSE IF (cluster_method .EQ. 'd') THEN
      CALL DBSCAN_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)
    ELSE IF (cluster_method .EQ. 's') THEN
      CALL AGGLOMERATIVE_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)
    ELSE IF (cluster_method .EQ. 'k') THEN
      CALL KNN_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)
    ELSE
       WRITE(*,*) 'Error of the cluster analysis method name in Mod_cluster_analaysis module'
       WRITE(*,*) 'Check the manual and the input file'
       STOP
    END IF


  END SUBROUTINE MAKE_CLUSTER_ANALYSIS



  !####################################################################################################################


  SUBROUTINE MEANSHIFT_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)
    ! From a group of np points p of dimension ndim, determine the clusters
    ! using the mean shift algorithm
    ! using distance and bandwidth as parameters
    ! INPUTS
    USE MOD_TIMESTAMP, ONLY: timestamp
    ! Module for the input parameter definition
    USE MOD_PARAMETERS, ONLY: cluster_method, cluster_par1, cluster_par2
    INTEGER(4), INTENT(IN) :: np_in, ndim_in
    REAL(8), INTENT(IN), DIMENSION(np_in,ndim_in) :: p_in
    ! Other variables
    REAL(8), DIMENSION(np_in,ndim_in) :: p, p_mean_shift, p_mean_old
    REAL(8), PARAMETER :: accuracy = 1e-7, accuracy_cluster = 1e-2
    REAL(8) :: dist, weight, val_max, val_min, actual_accuracy, max_accuracy=0.
    REAL(8) :: distance_limit=0., bandwidth=0.
    REAL(8), DIMENSION(ndim_in) :: num, dem
    INTEGER(4), PARAMETER :: iter_max=30, ncluster_max=500
    INTEGER(4) :: i, j, k, l, nn, min_nn
    REAL(8), DIMENSION(ncluster_max,ndim_in) :: mean_cluster
    LOGICAL :: accuracy_reached
    LOGICAL, DIMENSION(ndim_in) :: p_fix

    distance_limit = cluster_par1
    bandwidth = cluster_par2

    np = np_in
    ndim = ndim_in
    min_nn = np

    p = 0.
    p_mean_shift = 0.
    p_mean_old = 0.
    p_fix = .FALSE.

    ! Allocate the variables if needed
    IF(.NOT.cluster_on) ALLOCATE(p_cluster(np))
    p_cluster = 0


    ! Calculate min and max and renormalize the point dimensions
    DO l=1,ndim
       val_max = maxval(p_in(:,l))
       val_min = minval(p_in(:,l))
       IF (val_max.NE.val_min) THEN
         p(:,l) = (p_in(:,l)-val_min)/(val_max-val_min)
       ELSE !If it is a fixed parameter, just ignore it
         p(:,l) = 0.
       ENDIF
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
    WRITE(*,*) 'Starting mean-shift cluster analysis'
    DO i=1,iter_max
       p_mean_old  = p_mean_shift

       ! Calculate the neighbor point mean value for the selected point
       max_accuracy = 0.
       !$OMP PARALLEL DO &
       !$OMP PRIVATE(num,dem,nn,k,dist,weight,l,actual_accuracy) &
       !$OMP REDUCTION(min:min_nn) &
       !$OMP REDUCTION(max:max_accuracy)
       DO j=1,np
          ! Scan all other points for calculating the mean....
          num = 0.
          dem = 1.
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
                        IF (val_max.NE.val_min) THEN
                          num(l) = num(l) + p(k,l)*weight
                          dem(l) = dem(l) + weight
                          !write(*,*) i,j,k,l, nn, num(l), dem(l)
                        ENDIF
                      END DO
                      ! Caluclate baricenter with flat kernel
                   ELSEIF(cluster_method.EQ.'f'.OR.cluster_method.EQ.'F') THEN
                      DO l=1,ndim
                        IF (val_max.NE.val_min) THEN
                          num(l) = num(l) + p(k,l)
                          dem(l) = dem(l) + 1
                        ENDIF
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
       !$OMP END PARALLEL DO

       WRITE(*,*) 'n_iteration = ', i, 'present accuracy = ', max_accuracy

       OPEN (UNIT=10, FILE='nf_output_meanshift_check.dat', STATUS='unknown')
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
          IF (ncluster.GT.ncluster_max) THEN
             WRITE(*,*) '  '
             WRITE(*,*) 'ERROR!!! Too many clusters more than ', ncluster_max
             WRITE(*,*) 'Change cluster recognition parameters in mean-shift'
             STOP
          END IF
          mean_cluster(ncluster,:) = p_mean_shift(j,:)
          p_cluster(j) = ncluster
          !write(*,*) j, k, ncluster, NORM2(p_mean_shift(j,:)-mean_cluster(k,:)), 'new cluster'
       END IF
100    CONTINUE
    END DO
    WRITE(*,*) 'Number of cluster found = ', ncluster, 'Minimal n. of neighbours = ', min_nn


    ! Write file for further analysis and check of cluster recognition
    OPEN (UNIT=10, FILE='nf_output_cluster_final_'//timestamp()//'.dat', STATUS='unknown')
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
    DO k=1,ncluster
!       WRITE(*,*) 'Computing mean and std. dev. of cluster ', k
       CALL MAKE_CLUSTER_STD(p_in,k)
    END DO

    OPEN (UNIT=10, FILE='nf_output_cluster_mean_std.dat', STATUS='unknown')
    DO k=1,ncluster
       DO l=1,ndim
          WRITE(10,*) k, cluster_np(k), l, cluster_mean(k,l), cluster_std(k,l)
       END DO
    END DO
    CLOSE(10)


  END SUBROUTINE MEANSHIFT_CLUSTER_ANALYSIS


  !--------------------------------------------------------------------------------------------------------------

SUBROUTINE DBSCAN_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)
    !see https://scikit-learn.org/stable/modules/clustering.html
    ! From a group of np points p of dimension ndim, determine the clusters
    ! using the DBSCAN algorithm
    ! using distance_limit and min_neighb (minimal number of neigbours) as parameters
    ! INPUTS
    USE MOD_TIMESTAMP, ONLY: timestamp
    ! Module for the input parameter definition
    USE MOD_PARAMETERS, ONLY: cluster_par1, cluster_par2
    INTEGER(4), INTENT(IN) :: np_in, ndim_in
    REAL(8), INTENT(IN), DIMENSION(np_in,ndim_in) :: p_in
    REAL(8) :: distance_limit=0.
    INTEGER(4), PARAMETER :: ncluster_max=500
    INTEGER(4) :: i, j, k, nn, min_nn, min_neighb
    INTEGER(4), DIMENSION(np_in) :: neighb, selected, not_cluster
    REAL(8), DIMENSION(np_in,ndim_in) :: p
    REAL(8) :: val_max, val_min

    distance_limit = cluster_par1
    min_neighb=nint(cluster_par2)


    np = np_in
    ndim = ndim_in
    min_nn = np
    nn=0
    neighb=0
    selected=0
    not_cluster=1
    ncluster=0
    ! Allocate the variables if needed
    IF(.NOT.cluster_on) ALLOCATE(p_cluster(np))
    p_cluster = 0


    WRITE(*,*) 'Starting dbscan cluster analysis'
    DO i=1,ndim
       val_max = maxval(p_in(:,i))
       val_min = minval(p_in(:,i))
       IF (val_max.NE.val_min) THEN
         p(:,i) = (p_in(:,i)-val_min)/(val_max-val_min)
       ELSE !If it is a fixed parameter, just ignore it
         p(:,i) = 0.
       ENDIF
    END DO

    DO i=1,np
      IF(not_cluster(i)==1) THEN    ! if the point is not in a cluster yet
        !$OMP PARALLEL DO &
        !$OMP REDUCTION(+:nn)
        DO j=1,np
          IF(NORM2(p(i,:)-p(j,:))<=distance_limit) THEN
            neighb(j)=1
            nn=nn+1
          ENDIF
        END DO
        !$OMP END PARALLEL DO
        min_nn=min(min_nn,nn)
        ! a point is in its own neighbourood
        IF(nn>=min_neighb) THEN
          nn=0
          ncluster=ncluster+1
          IF (ncluster.GT.ncluster_max) THEN
             WRITE(*,*) '  '
             WRITE(*,*) 'ERROR!!! Too many clusters more than ', ncluster_max
             WRITE(*,*) 'Change cluster recognition parameters in DBSCAN'
             STOP
          END IF
          not_cluster(i)=0
          p_cluster(i)=ncluster
          !$OMP PARALLEL DO
          DO j=1,np
            IF(not_cluster(j)==1 .AND. neighb(j)==1) THEN
              selected(j)=1 ! select the point if it is not in a cluster and is a neighbor of i
            END IF
          END DO
          !$OMP END PARALLEL DO
          neighb=0
          DO
            IF(SUM(selected(:))==0) EXIT ! no more point to put in the cluster
            DO j=1,np
              IF(selected(j)==1) THEN
                not_cluster(j)=0
                selected(j)=0
                p_cluster(j)=ncluster
                !$OMP PARALLEL DO &
                !$OMP REDUCTION(+:nn)
                DO k=1,np
                  IF(NORM2(p(j,:)-p(k,:))<=distance_limit) THEN
                    neighb(k)=1
                    nn=nn+1
                  END IF
                END DO
                !$OMP END PARALLEL DO
                min_nn=min(min_nn,nn)
                IF(nn>=min_neighb) THEN
                  !$OMP PARALLEL DO
                  DO k=1,np
                    IF(not_cluster(k)==1 .AND. neighb(k)==1) THEN
                      selected(k)=1 ! select the point if it is not in a cluster and is a neighbor of j
                    END IF
                  END DO
                  !$OMP END PARALLEL DO
                END IF
                nn=0
                neighb=0
              END IF
            END DO
          END DO
        END IF
      END IF
      nn=0
      neighb=0
    END DO
    WRITE(*,*) 'Number of cluster found = ', ncluster, 'Minimal n. of neighbours = ', min_nn
    WRITE(*,*) 'Number of outliers = ', COUNT(p_cluster==0)
    IF(COUNT(p_cluster==0)>=np/5.0) THEN
      WRITE(*,*) 'Too many outliers '
      WRITE(*,*) 'Change cluster recognition parameters in DBSCAN'
      STOP
    END IF

    OPEN (UNIT=10, FILE='nf_output_cluster_final_'//timestamp()//'.dat', STATUS='unknown')
    DO i=1,np
       WRITE(10,*) p_cluster(i), p_in(i,:)
    END DO
    CLOSE(10)

    IF(ALLOCATED(cluster_std)) THEN
      DEALLOCATE(cluster_std)
      DEALLOCATE(cluster_mean)
      DEALLOCATE(cluster_np)
    END IF

    IF(COUNT(p_cluster==0)==0) THEN
       ALLOCATE(cluster_std(ncluster,ndim),cluster_mean(ncluster,ndim),cluster_np(ncluster))
    ELSE
       ALLOCATE(cluster_std(0:ncluster,ndim),cluster_mean(0:ncluster,ndim),cluster_np(0:ncluster))
    END IF
    cluster_std = 0.
    cluster_mean = 0.
    cluster_np = 0

    ! Calculate standard deviation and mean of the clusters
    IF(COUNT(p_cluster==0)==0) THEN
      DO j=1,ncluster
        ! WRITE(*,*) 'Computing mean and std. dev. of cluster ', j
        CALL MAKE_CLUSTER_STD(p_in,j)
      END DO
    ELSE
      DO j=0,ncluster
        ! WRITE(*,*) 'Computing mean and std. dev. of cluster ', j
        CALL MAKE_CLUSTER_STD(p_in,j)
      END DO
      DO j=1,np
        IF(p_cluster(j).EQ.0) THEN
          cluster_np(0)=cluster_np(0) + 1
        END IF
      END DO
    END IF

    OPEN (UNIT=10, FILE='nf_output_cluster_mean_std.dat', STATUS='unknown')
    IF(COUNT(p_cluster==0)==0) THEN
      DO j=1,ncluster
         DO k=1,ndim
            WRITE(10,*) j, cluster_np(j), k, cluster_mean(j,k), cluster_std(j,k)
         END DO
      END DO
    ELSE
      DO j=0,ncluster
         DO k=1,ndim
            WRITE(10,*) j, cluster_np(j), k, cluster_mean(j,k), cluster_std(j,k)
         END DO
      END DO  
    END IF
    CLOSE(10)
  END SUBROUTINE DBSCAN_CLUSTER_ANALYSIS

  !--------------------------------------------------------------------------------------------------------------

  SUBROUTINE AGGLOMERATIVE_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)
    !see https://scikit-learn.org/stable/modules/clustering.html
    ! From a group of np points p of dimension ndim, determine the clusters
    ! using the agglomerative algorithm
    ! using distance_limit as parameter
    ! INPUTS
    USE MOD_TIMESTAMP, ONLY: timestamp
    ! Module for the input parameter definition
    USE MOD_PARAMETERS, ONLY: cluster_method, cluster_par1
    ! distance_limit corresponds to maximum distance percentage
    INTEGER(4), INTENT(IN) :: np_in, ndim_in
    REAL(8), INTENT(IN), DIMENSION(np_in,ndim_in) :: p_in
    REAL(8) :: distance_limit=0.
    INTEGER(4), PARAMETER :: ncluster_max=500
    INTEGER(4) :: i, j, k, l, clust_min, clust_max
    REAL(8), DIMENSION(np_in,ndim_in) :: p
    REAL(8) :: val_max, val_min, dist_min, dist, max_dist
    REAL(8), DIMENSION(np_in,np_in) :: dist_pt
    INTEGER(4), DIMENSION(2) :: clusters_to_concat

    distance_limit = cluster_par1

    np = np_in
    ndim = ndim_in
    ncluster=np_in

    ! Allocate the variables if needed
    IF(.NOT.cluster_on) ALLOCATE(p_cluster(np))
    DO i=1,np
      p_cluster(i)= i
    END DO

    WRITE(*,*) 'Starting agglomerative cluster analysis'

    !$OMP PARALLEL DO PRIVATE(j)
    DO i=1,ndim
      val_max = maxval(p(:,i))
      val_min = minval(p(:,i))
      IF (val_max.NE.val_min) THEN
        p(:,i) = (p(:,i)-val_min)/(val_max-val_min)
      ELSE !If it is a fixed parameter, just ignore it
        p(:,i) = 0.
      ENDIF
    END DO

    !$OMP PARALLEL DO PRIVATE(j)
    DO i=1,np ! calculate the distance matrix
    dist_pt(i,i)=-1
      DO j=i+1,np
        dist_pt(i,j)=NORM2(p(i,:)-p(j,:))
        dist_pt(j,i)=-1
      END DO
    END DO
    !$OMP END PARALLEL DO

    max_dist=distance_limit*maxval(dist_pt) !the maximum distance to join two clusters

    DO
      dist_min=minval(dist_pt,dist_pt>=0)
      clusters_to_concat=minloc(dist_pt,dist_pt>=0)
      clust_min=min(p_cluster(minval(clusters_to_concat)),p_cluster(maxval(clusters_to_concat)))
      clust_max=max(p_cluster(minval(clusters_to_concat)),p_cluster(maxval(clusters_to_concat))) !finding the two clusters that are the closest
      IF(dist_min>=max_dist .OR. ncluster==1) EXIT !checking if the maximum distance has been reached or if there is only one cluster
      !$OMP PARALLEL DO PRIVATE(j)
      DO i=1,np
        DO j=1,np
          IF(p_cluster(i)==clust_min .AND. p_cluster(j)==clust_max) THEN !set the distance of two points in the same clusters at -1
            dist_pt(i,j)=-1
            dist_pt(j,i)=-1
          END IF
        END DO
      END DO
      !$OMP END PARALLEL DO
      !$OMP PARALLEL DO
      DO i=1,np
        IF(p_cluster(i)==clust_max) THEN ! put the two clusters together
          p_cluster(i)=clust_min
        END IF
      END DO
      !$OMP END PARALLEL DO

      DO i=clust_max+1,ncluster ! change the labels of the other clusters
        !$OMP PARALLEL DO
        DO j=1,np
          IF(p_cluster(j)==i) THEN
            p_cluster(j)=i-1
          END IF
        END DO
        !$OMP END PARALLEL DO
      END DO
      ncluster=ncluster-1
    END DO

    IF (ncluster.GT.ncluster_max) THEN
      WRITE(*,*) '  '
      WRITE(*,*) 'ERROR!!! Too many clusters more than ', ncluster_max
      WRITE(*,*) 'Change cluster recognition parameters in agglomerative'
      STOP
    END IF

    WRITE(*,*) 'Number of cluster found = ', ncluster

    OPEN (UNIT=10, FILE='nf_output_cluster_final_'//timestamp()//'.dat', STATUS='unknown')
    DO i=1,np
       WRITE(10,*) p_cluster(i), p_in(i,:)
    END DO
    CLOSE(10)

    IF(ALLOCATED(cluster_std)) DEALLOCATE(cluster_std,cluster_mean,cluster_np)
    ALLOCATE(cluster_std(ncluster,ndim),cluster_mean(ncluster,ndim),cluster_np(ncluster))
    cluster_std = 0.
    cluster_mean = 0.
    cluster_np = 0

    ! Calculate standard deviation and mean of the clusters
    DO j=1,ncluster
      ! WRITE(*,*) 'Computing mean and std. dev. of cluster ', j
      CALL MAKE_CLUSTER_STD(p_in,j)
    END DO

    OPEN (UNIT=10, FILE='nf_output_clustermean_std.dat', STATUS='unknown')
    DO j=1,ncluster
       DO k=1,ndim
         WRITE(10,*) j, cluster_np(j), k, cluster_mean(j,k), cluster_std(j,k)
       END DO
    END DO
    CLOSE(10)

  END SUBROUTINE AGGLOMERATIVE_CLUSTER_ANALYSIS

  !--------------------------------------------------------------------------------------------------------------

  SUBROUTINE KNN_CLUSTER_ANALYSIS(np_in,ndim_in,p_in)
    !inspired from polychord code
    ! From a group of np points p of dimension ndim, determine the clusters
    ! using the KNN algorithm
    ! no parameters
    ! INPUTS
    USE MOD_TIMESTAMP, ONLY: timestamp
    INTEGER(4), INTENT(IN) :: np_in, ndim_in
    REAL(8), INTENT(IN), DIMENSION(np_in,ndim_in) :: p_in
    INTEGER(4), PARAMETER :: ncluster_max=500
    INTEGER(4) :: i, j, k, l, clust_min, clust_max, icluster
    REAL(8), DIMENSION(np_in,ndim_in) :: p
    INTEGER(4), DIMENSION(np_in) :: p_cluster_new, p_cluster_old
    INTEGER(4) :: ncluster_new, ncluster_old, ncluster_temp
    REAL(8), DIMENSION(np_in,np_in) :: dist_pt
    INTEGER(4), DIMENSION(np_in,np_in) :: knn_mat
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: dist_pt_temp
    INTEGER(4) :: np_temp
    INTEGER(4), DIMENSION(:), ALLOCATABLE :: in_cluster, p_cluster_temp

    p=p_in
    np = np_in
    ndim = ndim_in
    np_temp=0


    WRITE(*,*) 'Starting KNN cluster analysis'

    !$OMP PARALLEL DO
    DO i=1,np
      p_cluster_new(i) = i
      p_cluster_old(i) = i
    END DO
    !$OMP END PARALLEL DO

    !$OMP PARALLEL DO PRIVATE(j)
    DO i=1,np  ! distance matrix
    dist_pt(i,i)=0
      DO j=i+1,np
        dist_pt(i,j)=NORM2(p(i,:)-p(j,:))
        dist_pt(j,i)=dist_pt(i,j)
      END DO
    END DO
    !$OMP END PARALLEL DO


    DO k=2,np
      CALL FIND_KNN(dist_pt,knn_mat(:,:k),k,np)
      DO i=1,np
        DO j=i+1,np
          IF(ANY(knn_mat(i,:k)==j) .AND. ANY(knn_mat(j,:k)==i)) THEN ! if i and j are among each other's k nearest neighbours, their clusters are joined
            clust_min=min(p_cluster_new(i),p_cluster_new(j))
            clust_max=max(p_cluster_new(i),p_cluster_new(j))
            IF(clust_min/=clust_max) THEN
              WHERE(p_cluster_new==clust_max)
                p_cluster_new=clust_min
              ELSEWHERE(p_cluster_new>clust_max) !new label for the other clusters
                p_cluster_new=p_cluster_new-1
              END WHERE
            END IF
          END IF
        END DO
      END DO
      ncluster_new=int(maxval(p_cluster_new))
      IF(ALL(p_cluster_new==p_cluster_old)) THEN !check if the clustering is the same for two consecutive value of k
        EXIT
      ELSE IF (ncluster_new==1) THEN !check if there only one cluster
        EXIT
      ELSE
        ncluster_old=ncluster_new !update the information
        p_cluster_old=p_cluster_new
        DO l=1,np
          p_cluster_new(l) = l
        END DO
      END IF
    END DO

    IF(ncluster_new>1) THEN !if more than one cluster, do the analysis for each cluster found
      icluster=1
      DO WHILE(icluster<=ncluster_new)
400     CONTINUE
        !$OMP PARALLEL DO REDUCTION(+:np_temp)
        DO i=1,np
          IF(p_cluster_new(i)==icluster) np_temp=np_temp+1
        END DO
        !$OMP END PARALLEL DO
        IF(np_temp>=3) THEN
          IF(ALLOCATED(dist_pt_temp)) DEALLOCATE(dist_pt_temp)
          IF(ALLOCATED(p_cluster_temp)) DEALLOCATE(p_cluster_temp)
          IF(ALLOCATED(in_cluster)) DEALLOCATE(in_cluster)
          ALLOCATE(dist_pt_temp(np_temp,np_temp))
          ALLOCATE(p_cluster_temp(np_temp))
          ALLOCATE(in_cluster(np_temp))
          j=1
          DO l=1,np
            IF(p_cluster_new(l)==icluster) THEN
              in_cluster(j)=l
              j=j+1
            END IF
          END DO
          dist_pt_temp=dist_pt(in_cluster,in_cluster)
          CALL MAKE_SUB_CLUSTERS(dist_pt_temp,np_temp,ncluster_temp,p_cluster_temp)
          IF(ncluster_temp==1) THEN
            icluster=icluster+1
            np_temp=0
          ELSE
            DO j=1,np_temp
              l=in_cluster(j)
              IF(p_cluster_temp(j)/=1) p_cluster_new(l)=ncluster_new+p_cluster_temp(j)-1
            END DO
            ncluster_new=ncluster_new+ncluster_temp-1
            IF (ncluster_new .GT.ncluster_max) THEN
              WRITE(*,*) '  '
              WRITE(*,*) 'ERROR!!! Too many clusters'
              STOP
            END IF
            np_temp=0
            GOTO 400
          END IF
        ELSE
          icluster=icluster+1
          np_temp=0
        END IF
      END DO
    END IF

    ncluster=ncluster_new

    IF (ncluster.GT.ncluster_max) THEN
      WRITE(*,*) '  '
      WRITE(*,*) 'ERROR!!! Too many clusters'
      STOP
    END IF

    WRITE(*,*) 'Number of cluster found = ', ncluster


    IF(.NOT.cluster_on) ALLOCATE(p_cluster(np))
    p_cluster=p_cluster_new

    OPEN (UNIT=10, FILE='nf_output_cluster_final_'//timestamp()//'.dat', STATUS='unknown')
    DO i=1,np
       WRITE(10,*) p_cluster(i), p_in(i,:)
    END DO
    CLOSE(10)

    IF(ALLOCATED(cluster_std)) DEALLOCATE(cluster_std,cluster_mean,cluster_np)
    ALLOCATE(cluster_std(ncluster,ndim),cluster_mean(ncluster,ndim),cluster_np(ncluster))
    cluster_std = 0.
    cluster_mean = 0.
    cluster_np = 0

    ! Calculate standard deviation and mean of the clusters
    DO j=1,ncluster
      ! WRITE(*,*) 'Computing mean and std. dev. of cluster ', j
      CALL MAKE_CLUSTER_STD(p_in,j)
    END DO

    OPEN (UNIT=10, FILE='nf_output_cluster_mean_std.dat', STATUS='unknown')
    DO j=1,ncluster
       DO k=1,ndim
         WRITE(10,*) j, cluster_np(j), k, cluster_mean(j,k), cluster_std(j,k)
       END DO
    END DO
    CLOSE(10)


  END SUBROUTINE KNN_CLUSTER_ANALYSIS

  !--------------------------------------------------------------------------------------------------------------

  SUBROUTINE FIND_KNN(dist,knn,k, nb_pt)
    INTEGER(4), INTENT(IN) :: nb_pt, k
    REAL(8), DIMENSION(nb_pt,nb_pt), INTENT(IN):: dist
    INTEGER(4), DIMENSION(nb_pt,k), INTENT(OUT) :: knn
    INTEGER(4) :: i, j

    knn(:,1)=MINLOC(dist, DIM=1)
    DO i=2,k
      !$OMP PARALLEL DO
      DO j=1,nb_pt
        knn(j,i)=MINLOC(dist(:,j), MASK=(dist(:,j)>dist(knn(j,i-1),j)),DIM=1)
      END DO
      !$OMP END PARALLEL DO
    END DO


  END SUBROUTINE FIND_KNN

  !--------------------------------------------------------------------------------------------------------------

  SUBROUTINE MAKE_SUB_CLUSTERS(dist_pt,np,ncluster2,p_cluster2)
    INTEGER(4), INTENT(IN) :: np
    INTEGER(4), INTENT(OUT) :: ncluster2
    INTEGER(4), DIMENSION(np), INTENT(OUT) :: p_cluster2
    REAL(8), DIMENSION(np,np), INTENT(IN) :: dist_pt
    INTEGER(4), DIMENSION(np,np) :: knn_mat
    INTEGER(4) :: i, j, k, l, clust_min, clust_max
    INTEGER(4), DIMENSION(np) :: p_cluster_new, p_cluster_old
    INTEGER(4) :: ncluster_new, ncluster_old


    DO i=1,np
      p_cluster_new(i) = i
      p_cluster_old(i) = i
    END DO

    DO k=2,np
      CALL FIND_KNN(dist_pt,knn_mat(:,:k),k,np)
      DO i=1,np
        DO j=i+1,np
          IF(ANY(knn_mat(i,:k)==j) .AND. ANY(knn_mat(j,:k)==i)) THEN
            clust_min=min(p_cluster_new(i),p_cluster_new(j))
            clust_max=max(p_cluster_new(i),p_cluster_new(j))
            IF(clust_min/=clust_max) THEN
              WHERE(p_cluster_new==clust_max)
                p_cluster_new=clust_min
              ELSEWHERE(p_cluster_new>clust_max)
                p_cluster_new=p_cluster_new-1
              END WHERE
            END IF
          END IF
        END DO
      END DO
      ncluster_new=int(maxval(p_cluster_new))
      IF(ALL(p_cluster_new==p_cluster_old)) THEN
        EXIT
      ELSE IF (ncluster_new==1) THEN
        EXIT
      ELSE
        ncluster_old=ncluster_new
        p_cluster_old=p_cluster_new
        DO l=1,np
          p_cluster_new(l) = l
        END DO
      END IF
    END DO
    p_cluster2=p_cluster_new
    ncluster2=ncluster_new
  END SUBROUTINE MAKE_SUB_CLUSTERS

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


    IF(icl==0) THEN
      DO l=1,ndim
        cluster_mean(icl,l) = 0.
        cluster_std(icl,l) = 0.
        cluster_mean(icl,l) = SUM(p(1:np,l))/np
        cluster_std(icl,l) =  &
            DSQRT(SUM((p(1:np,l)-cluster_mean(icl,l))**2)/ &
                  (np-1))
      END DO
    ELSE
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

END MODULE MOD_CLUSTER_ANALYSIS
