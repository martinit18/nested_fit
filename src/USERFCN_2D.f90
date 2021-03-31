! Time-stamp: <Last changed by martino on Tuesday 30 March 2021 at CEST 22:35:00>

REAL(8) FUNCTION USERFCN_2D(x,y,npar,val,funcname)
  ! Library of 2D functions 
  
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  CHARACTER, INTENT(IN) :: funcname*64
  !
  REAL(8) :: GAUSS_SIMPLE_2D, GAUSS_BG_2D

  ! Choose your model (see below for definition)
  IF(funcname.EQ.'GAUSS_SIMPLE_2D') THEN
     USERFCN_2D = GAUSS_SIMPLE_2D(x,y,npar,val)
  ELSE IF(funcname.EQ.'GAUSS_BG_2D') THEN
     USERFCN_2D = GAUSS_BG_2D(x,y,npar,val)
  ELSE
     WRITE(*,*) 'Error in the function name def. in USERFCN_2D'
     WRITE(*,*) 'Check in the manual and in the nf_input.dat file'
     STOP
  END IF
  

  RETURN

END FUNCTION USERFCN_2D


!################################### SHAPES DEFINITIONS #####################################


REAL(8) FUNCTION GAUSS_SIMPLE_2D(x,y,npar,val)
  !     Normalized Gaussian distribution in 2 dimension, witouut correlation between x and y
  !     The value of 'amp' is the value of the volume below the curve
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), PARAMETER :: pi=3.141592653589793d0
  REAL(8) :: amp, x0, y0, sigmax, sigmay

  amp    = val(1)
  x0     = val(2)
  y0     = val(3)
  sigmax = val(4)
  sigmay = val(5)
 

  !     Test of under of underflow first
  IF(DABS((x-x0)**2/(2*sigmax**2)).LT.700.OR.DABS((y-y0)**2/(2*sigmay**2)).LT.700) THEN
     GAUSS_SIMPLE_2D = amp/(2*pi*sigmax*sigmay)* &
          dexp(-(x-x0)**2/(2*sigmax**2))* &
          dexp(-(y-y0)**2/(2*sigmay**2))
  ELSE
     GAUSS_SIMPLE_2D = 0.d0
  END IF

  RETURN

END FUNCTION GAUSS_SIMPLE_2D

!________________________________________________________________________________________

REAL(8) FUNCTION GAUSS_BG_2D(x,y,npar,val)
  !     Normalized Gaussian distribution in 2 dimension, witouut correlation between x and y
  !     The value of 'amp' is the value of the volume below the curve
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(5) :: valg
  REAL(8), PARAMETER :: pi=3.141592653589793d0
  REAL(8) :: amp, x0, y0, sigmax, sigmay, bg
  REAL(8) GAUSS_SIMPLE_2D

  amp    = val(1)
  x0     = val(2)
  y0     = val(3)
  sigmax = val(4)
  sigmay = val(5)
  bg     = val(6)

  ! For the pure gaussian peak
  valg(1) = amp
  valg(2) = x0
  valg(3) = y0
  valg(4) = sigmax
  valg(5) = sigmay
 
  GAUSS_BG_2D = GAUSS_SIMPLE_2D(x,y,5,valg) + bg

  RETURN

END FUNCTION GAUSS_BG_2D
