FUNCTION RANDN()
  ! Time-stamp: <Last changed by martino on Wednesday 01 January 2020 at CET 22:24:41>
  !Box-muller transfor used for normal distrubion

  IMPLICIT NONE
  REAL(8) :: RANDN, rn1, rn2
  REAL(8), PARAMETER :: pi=3.141592653589793d0

10 CALL RANDOM_NUMBER(rn1)
  CALL RANDOM_NUMBER(rn2)  
  IF (rn1.LE.0.OR.rn2.LE.0) GOTO 10
  RANDN = (-2 * log(rn1)) ** 0.5 * cos(2*pi * rn2)
  ! For different mean and standard deviation
  !RANDN = (-2 * log(rn1)) ** 0.5 * cos(2*pi * rn2)  * std + mean

END FUNCTION RANDN
