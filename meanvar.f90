
! Time-stamp: <Last changed by martino on Tuesday 31 December 2019 at CET 17:49:34>

SUBROUTINE MEANVAR(data,n,mean,var)
! Simple subroutine for calculation of mean and variance
  IMPLICIT NONE
  INTEGER(4), INTENT(IN) :: n
  REAL(8), INTENT(IN), DIMENSION(n) :: data
  REAL(8), INTENT(OUT) :: mean, var

  mean = 0.
  var = 0.

  mean = SUM(data)/n
  var = SUM((data-mean)**2)/(n-1)
  


END SUBROUTINE MEANVAR
