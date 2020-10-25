FUNCTION DLOG_FAC(n)
  ! Time-stamp: <Last changed by martino on Wednesday 08 January 2020 at CET 17:33:48>
  ! Simple logarithm of the factorial based on the intrinsic function DLGAMMA
  IMPLICIT NONE
  INTEGER(4), INTENT(IN) :: n
  REAL(8) DLOG_FAC

  IF(n.LT.0) THEN
     WRITE(*,*) 'Negative number in DLOG_FAC. Change something'
     WRITE(*,*) 'n = ', n
     STOP
  ELSE IF(n.EQ.0) THEN
     ! 0!=1 so log(0!) = 0
     DLOG_FAC = 0.
  ELSE IF(n.GT.0) THEN
     DLOG_FAC = LOG_GAMMA(n+1.D0)
  END IF

  RETURN

END FUNCTION DLOG_FAC
