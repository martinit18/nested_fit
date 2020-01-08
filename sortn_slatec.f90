SUBROUTINE SORTN(n,ndim,asort,array)
  ! Subroutine to sort an array of dimension (n,ndim) ordering
  ! with respect to another array (asort) of dimension n
  IMPLICIT NONE
  INTEGER(4), INTENT(IN) :: n, ndim
  REAL(8), INTENT(INOUT), DIMENSION(n) :: asort
  REAL(8), INTENT(INOUT), DIMENSION(n,ndim) :: array
  INTEGER(4), DIMENSION(n) :: iwksp
  REAL(8), DIMENSION(n) :: wkspsort
  REAL(8), DIMENSION(n,ndim) :: wksp
  INTEGER(4) :: i, ier

  wkspsort = asort
  wksp = array
  CALL DPSORT(asort,n,iwksp,1,ier)
  IF(ier.GT.0) THEN
    WRITE(*,*) 'Error in SORTN. IER = ', ier
    STOP
  ENDIF
  DO i=1,n
     asort(i) = wkspsort(iwksp(i))
     array(i,:) = wksp(iwksp(i),:)
  ENDDO

END SUBROUTINE SORTN

!____________________________________________________________________________________

SUBROUTINE SORTN2(n,ndim1,ndim2,asort,array1,array2)
  ! Subroutine to sort two arrays of dimension (n,ndim1) (n,ndim2) ordering
  ! with respect to another array (asort) of dimension n
  IMPLICIT NONE
  INTEGER(4), INTENT(IN) :: n, ndim1, ndim2
  REAL(8), INTENT(INOUT), DIMENSION(n) :: asort
  REAL(8), INTENT(INOUT), DIMENSION(n,ndim1) :: array1
  REAL(8), INTENT(INOUT), DIMENSION(n,ndim2) :: array2
  INTEGER(4), DIMENSION(n) :: iwksp
  REAL(8), DIMENSION(n) :: wkspsort
  REAL(8), DIMENSION(n,ndim1) :: wksp1
  REAL(8), DIMENSION(n,ndim2) :: wksp2
  INTEGER(4) :: i, ier

  wkspsort = asort
  wksp1 = array1
  wksp2 = array2
  CALL DPSORT(asort,n,iwksp,1,ier)
  IF(ier.GT.0) THEN
    WRITE(*,*) 'Error in SORTN2. IER = ', ier
    STOP
  ENDIF
  DO i=1,n
     asort(i) = wkspsort(iwksp(i))
     array1(i,:) = wksp1(iwksp(i),:)
     array2(i,:) = wksp2(iwksp(i),:)
  ENDDO

END SUBROUTINE SORTN2
