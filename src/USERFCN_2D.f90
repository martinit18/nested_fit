! Time-stamp: <Last changed by martino on Friday 02 April 2021 at CEST 18:38:45>

REAL(8) FUNCTION USERFCN_2D(x,y,npar,val,funcname)
  ! Library of 2D functions 
  
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  CHARACTER, INTENT(IN) :: funcname*64
  !
  REAL(8) :: GAUSS_SIMPLE_2D, GAUSS_BG_2D
  REAL(8) :: GAUSS_LINE_BG_2D, TWO_LORE_LINE_BG_2D

  ! Choose your model (see below for definition)
  IF(funcname.EQ.'GAUSS_SIMPLE_2D') THEN
     USERFCN_2D = GAUSS_SIMPLE_2D(x,y,npar,val)
  ELSE IF(funcname.EQ.'GAUSS_BG_2D') THEN
     USERFCN_2D = GAUSS_BG_2D(x,y,npar,val)
  ELSE IF(funcname.EQ.'GAUSS_LINE_BG_2D') THEN
     USERFCN_2D = GAUSS_LINE_BG_2D(x,y,npar,val)
  ELSE IF(funcname.EQ.'TWO_LORE_LINE_BG_2D') THEN
     USERFCN_2D = TWO_LORE_LINE_BG_2D(x,y,npar,val)
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





!################################## LINES PROFILES ############################################

! The first three parameters will be systematically be related to the line slope
! x = a + b*(y-y0) + c*(y-y0)**2
! y0: middle of the image
!
! 
!
!##############################################################################################


REAL(8) FUNCTION GAUSS_LINE_BG_2D(x,y,npar,val)
  ! Normalized Gaussian line profile.
  ! x: dispersion axis --> Gaussian profile
  ! y: parallel axis --> slope of the line
  ! The value of 'amp' is the value of the volume below the curve

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(3) :: valg
  REAL(8) :: GAUSS
  REAL(8) :: a, b, c, amp, sigma, y0, bg, x0

  a     = val(1)
  b     = val(2)
  c     = val(3)
  y0    = val(4)
  amp   = val(5)
  sigma = val(6)
  bg    = val(7)

  x0 = a + b*(y-y0) + c*(y-y0)**2

  valg(1) = x0
  valg(2) = amp
  valg(3) = sigma
  

  GAUSS_LINE_BG_2D = GAUSS(x,3,valg) + bg

  RETURN
  
  

END FUNCTION GAUSS_LINE_BG_2D

!________________________________________________________________________________________

REAL(8) FUNCTION TWO_LORE_LINE_BG_2D(x,y,npar,val)
  ! Normalized Gaussian line profile.
  ! x: dispersion axis --> Gaussian profile
  ! y: parallel axis --> slope of the line
  ! The value of 'amp' is the value of the volume below the curve
  ! Dy and y0 are not variables but information for the heigth of the detector and
  ! the middle plan, respectively

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(3) :: vall1, vall2
  REAL(8) :: LORE
  REAL(8) :: a, b, c, y0, Dy, dx, amp1, gamma, damp, amp2, bg, x01, x02

  a     = val(1)
  b     = val(2)
  c     = val(3)
  y0    = val(4)
  Dy    = val(5)
  dx    = val(6)
  amp1  = val(7)
  damp  = val(8)
  gamma = val(9)
  bg    = val(10)

  x01 = a + b*(y-y0) + c*(y-y0)**2
  x02 = x01 + dx
  amp1 = amp1/Dy    ! For the normalization along the plan perpendicular to the dispersion
  amp2 = amp1*damp

  vall1(1) = x01
  vall1(2) = amp1
  vall1(3) = gamma
  
  vall2(1) = x02
  vall2(2) = amp2
  vall2(3) = gamma
  

  TWO_LORE_LINE_BG_2D = LORE(x,3,vall1) + LORE(x,3,vall2) + bg

  RETURN
  
  

END FUNCTION TWO_LORE_LINE_BG_2D

!________________________________________________________________________________________
