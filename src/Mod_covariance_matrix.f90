MODULE MOD_COVARIANCE_MATRIX
  ! Automatic Time-stamp: <Last changed by martino on Thursday 01 June 2023 at CEST 15:23:06>
  ! Module for calculating the covariance matrix

  ! Module for the input parameter definition
  USE MOD_PARAMETERS, ONLY:  npar, par_fix, cluster_method, nlive
  ! Module for cluster analysis
  USE MOD_CLUSTER_ANALYSIS, ONLY: cluster_on, p_cluster, ncluster, cluster_np
  
  !$ USE OMP_LIB

  IMPLICIT NONE

  REAL(8), DIMENSION(:,:,:), ALLOCATABLE :: mat_cov, mat_chol
  INTEGER(4) :: dim_eff
  INTEGER(4), DIMENSION(:), ALLOCATABLE :: par_var
#ifdef LAPACK_ON
  EXTERNAL :: dpotrf, dtrtri, dtrmv
#endif


CONTAINS

SUBROUTINE ALLOCATE_PAR_VAR()
  INTEGER(4) :: i, j
  dim_eff=npar-SUM(par_fix) !number of parameters not fixed
  IF(.NOT. ALLOCATED(par_var)) ALLOCATE(par_var(dim_eff))
  j=1
  ! Select the parameters that are not fixed
  DO i=1,npar
    IF(par_fix(i).NE.1) THEN
      par_var(j)=i
      j=j+1
    END IF
  END DO

END SUBROUTINE ALLOCATE_PAR_VAR

SUBROUTINE CREATE_MAT_COV(pts)
  REAL(8), DIMENSION(nlive,dim_eff), INTENT(IN) :: pts
  INTEGER(4) :: j, k
  
  IF(ALLOCATED(mat_cov)) DEALLOCATE(mat_cov)
  IF(ALLOCATED(mat_chol)) DEALLOCATE(mat_chol)
  ! Allocate the covariance matrix and Cholesky decomposition and fill them
  ALLOCATE(mat_cov(dim_eff,dim_eff,1),mat_chol(dim_eff,dim_eff,1))
  CALL CALC_MAT_COV(pts,nlive,dim_eff,1,mat_cov(:,:,1))
#ifdef LAPACK_ON
    mat_chol=mat_cov
    CALL dpotrf('L',dim_eff,mat_chol(:,:,1),dim_eff,k)
    DO j=1,dim_eff
       mat_chol(1:(j-1),j,1)=0
    END DO
#else
    CALL CHOLESKY(dim_eff,mat_cov(:,:,1),mat_chol(:,:,1))
#endif

END SUBROUTINE CREATE_MAT_COV

SUBROUTINE REALLOCATE_MAT_COV()

  ! Reallocate the covariance matrix and Cholesky decomposition
  IF(ALLOCATED(mat_cov)) DEALLOCATE(mat_cov)
  IF(ALLOCATED(mat_chol)) DEALLOCATE(mat_chol)
  IF(COUNT(p_cluster==0)==0) THEN
     ALLOCATE(mat_cov(dim_eff,dim_eff,ncluster),mat_chol(dim_eff,dim_eff,ncluster))
  ELSE
     ALLOCATE(mat_cov(dim_eff,dim_eff,0:ncluster),mat_chol(dim_eff,dim_eff,0:ncluster))
  END IF
  mat_cov=0
  mat_chol=0

END SUBROUTINE REALLOCATE_MAT_COV

SUBROUTINE FILL_MAT_COV(pts)
  REAL(8), DIMENSION(nlive,dim_eff), INTENT(IN) :: pts
  INTEGER(4) :: i, j, k
  
  ! Fill the covariance matrix and Cholesky decomposition
  IF(cluster_on) THEN
     IF(COUNT(p_cluster==0)==0) THEN
        DO i=1,ncluster
           CALL CALC_MAT_COV(pts,nlive,dim_eff,i,mat_cov(:,:,i))
#ifdef LAPACK_ON
           mat_chol(:,:,i)=mat_cov(:,:,i)
           CALL dpotrf('L',dim_eff,mat_chol(:,:,i),dim_eff,k)
           DO j=1,dim_eff
              mat_chol(1:(j-1),j,i)=0
           END DO
#else
           CALL CHOLESKY(dim_eff,mat_cov(:,:,i),mat_chol(:,:,i))
#endif
        END DO
     ELSE
        DO i=0,ncluster
           CALL CALC_MAT_COV(pts,nlive,dim_eff,i,mat_cov(:,:,i))
#ifdef LAPACK_ON
           mat_chol(:,:,i)=mat_cov(:,:,i)
           CALL dpotrf('L',dim_eff,mat_chol(:,:,i),dim_eff,k)
           DO j=1,dim_eff
              mat_chol(1:(j-1),j,i)=0
           END DO
#else
           CALL CHOLESKY(dim_eff,mat_cov(:,:,i),mat_chol(:,:,i))
#endif
        END DO
     END IF
  ELSE
     CALL CALC_MAT_COV(pts,nlive,dim_eff,1,mat_cov(:,:,1))
#ifdef LAPACK_ON
           mat_chol(:,:,1)=mat_cov(:,:,1)
           CALL dpotrf('L',dim_eff,mat_chol(:,:,1),dim_eff,k)
           DO j=1,dim_eff
              mat_chol(1:(j-1),j,1)=0
           END DO
#else
           CALL CHOLESKY(dim_eff,mat_cov(:,:,1),mat_chol(:,:,1))
#endif
  END IF


END SUBROUTINE FILL_MAT_COV

SUBROUTINE CALC_MAT_COV(pts,np,D,icluster,cov) !calculates the covariance matrix
  INTEGER(4), INTENT(IN) :: np, D, icluster
  REAL(8), DIMENSION(np,D), INTENT(IN) :: pts
  REAL(8), DIMENSION(D,D), INTENT(OUT) :: cov
  REAL(8), DIMENSION(D) :: mean, mean_prov
  INTEGER(4) :: i, j

  IF (cluster_on) THEN
    ! Get for the specific cluster if the cluster analysis is on
    ! Standard deviation
    IF(cluster_np(icluster).LT.2*D) THEN
       !$OMP SIMD
       DO i=1,D
         mean_prov(i)=SUM(pts(:,i))/np
       END DO
       !$OMP END SIMD
       !!$OMP SIMD
       DO j=1,D
         !$OMP SIMD
         DO i=j,D
           cov(i,j)=SUM((pts(:,i)-mean_prov(i))*(pts(:,j)-mean_prov(j)))/(np-1)
           cov(j,i)=cov(i,j)
         END DO
         !$OMP END SIMD
       END DO
       !!$OMP END SIMD
    ELSE IF(icluster==0) THEN
       !$OMP SIMD
       DO i=1,D
         mean(i)=SUM(pts(:,i))/np
       END DO
       !$OMP END SIMD
       !!$OMP SIMD
       DO j=1,D
         !$OMP SIMD
         DO i=j,D
           cov(i,j)=SUM((pts(:,i)-mean(i))*(pts(:,j)-mean(j)))/(np-1)
           cov(j,i)=cov(i,j)
         END DO
         !$OMP END SIMD
       END DO
       !!$OMP END SIMD
    ELSE
       !$OMP SIMD
       DO i=1,D
         mean(i)=SUM(pts(:,i),MASK=(p_cluster==icluster))/cluster_np(icluster)
       END DO
       !$OMP END SIMD
       !!$OMP SIMD
       DO j=1,D
         !$OMP SIMD
         DO i=j,D
           cov(i,j)=SUM((pts(:,i)-mean(i))*(pts(:,j)-mean(j)),MASK=(p_cluster==icluster))/(cluster_np(icluster)-1)
           cov(j,i)=cov(i,j)
         END DO
         !$OMP END SIMD
       END DO
       !!$OMP END SIMD
    END IF
  ELSE
    !$OMP SIMD
    DO i=1,D
      mean(i)=SUM(pts(:,i))/np
    END DO
    !$OMP END SIMD
    !!$OMP SIMD
     DO j=1,D
       !$OMP SIMD
      DO i=j,D
        cov(i,j)=SUM((pts(:,i)-mean(i))*(pts(:,j)-mean(j)))/(np-1)
        cov(j,i)=cov(i,j)
      END DO
      !$OMP END SIMD
    END DO
    !!$OMP END SIMD
  END IF
END SUBROUTINE CALC_MAT_COV

SUBROUTINE CHOLESKY(D,cov,chol) !calculates the cholesky decomposition of the covariance matrix
   INTEGER(4), INTENT(IN) :: D
   REAL(8), DIMENSION(D,D), INTENT(IN) :: cov
   REAL(8), DIMENSION(D,D), INTENT(OUT) :: chol
   INTEGER(4) :: i,j,k
   chol=0
   chol(1,1)=SQRT(cov(1,1))
   !!$OMP SIMD
   DO i=2,D
     chol(i,1)=cov(i,1)/chol(1,1)
   END DO
   !!$OMP SIMD
   DO i=2,D
     chol(i,i)=cov(i,i)
     !!$OMP SIMD
     DO k=1,i-1
       chol(i,i)=chol(i,i)-chol(i,k)**2
     END DO
     chol(i,i)=SQRT(chol(i,i))
     !!$OMP SIMD
     DO j=i+1,D
       chol(j,i)=cov(i,j)
       !!$OMP SIMD
       DO k=1,i-1
         chol(j,i)=chol(j,i)-chol(i,k)*chol(j,k)
       END DO
       chol(j,i)=chol(j,i)/chol(i,i)
     END DO
   END DO
END SUBROUTINE CHOLESKY


END MODULE MOD_COVARIANCE_MATRIX
