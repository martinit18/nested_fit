! Time-stamp: <Last changed by martino on Monday 21 June 2021 at CEST 17:56:10>

FUNCTION SELECT_USERFCN_2D(funcname)
   IMPLICIT NONE
   CHARACTER*64 funcname
   INTEGER*4 SELECT_USERFCN_2D

   ! Choose your model (see below for definition)
   IF(funcname.EQ.'GAUSS_SIMPLE_2D') THEN
      SELECT_USERFCN_2D = 0
   ELSE IF(funcname.EQ.'GAUSS_BG_2D') THEN
      SELECT_USERFCN_2D = 1
   ELSE IF(funcname.EQ.'GAUSS_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 2
   ELSE IF(funcname.EQ.'LORE_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 3
   ELSE IF(funcname.EQ.'VOIGT_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 4
   ELSE IF(funcname.EQ.'SUPERGAUSS_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 5
   ELSE IF(funcname.EQ.'ERFPEAK_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 6
   ELSE IF(funcname.EQ.'TWO_ERFPEAK_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 7
   ELSE IF(funcname.EQ.'TWO_LORE_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 8
   ELSE IF(funcname.EQ.'TWO_LORE_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 9
   ELSE IF(funcname.EQ.'TWO_LORE_WF_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 10
   ELSE IF(funcname.EQ.'FOUR_LORE_WF_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 11
   ELSE IF(funcname.EQ.'TWO_VOIGT_LINE_BG_2D') THEN
      SELECT_USERFCN_2D = 12
   ELSE
      WRITE(*,*) 'Selected function:', funcname
      WRITE(*,*) 'Error in the function name def. in SELECT_USERFCN_2D'
      WRITE(*,*) 'Check in the manual and in the input.dat file'
      STOP
   END IF
   RETURN
END

REAL(8) FUNCTION USERFCN_2D(x,y,npar,val,funcid)
  ! Library of 2D functions

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  INTEGER(4), INTENT(IN) :: funcid
  !
  REAL(8) :: GAUSS_SIMPLE_2D, GAUSS_BG_2D
  REAL(8) :: GAUSS_LINE_BG_2D, SUPERGAUSS_LINE_BG_2D
  REAL(8) :: LORE_LINE_BG_2D, ERFPEAK_LINE_BG_2D, TWO_ERFPEAK_LINE_BG_2D
  REAL(8) :: VOIGT_LINE_BG_2D
  REAL(8) :: TWO_LORE_LINE_BG_2D, TWO_LORE_WF_LINE_BG_2D, TWO_VOIGT_LINE_BG_2D
  REAL(8) :: FOUR_LORE_WF_LINE_BG_2D

  ! Choose your model (see below for definition)
  SELECT CASE (funcid)
  CASE (0)
     USERFCN_2D = GAUSS_SIMPLE_2D(x,y,npar,val)
  CASE(1)
     USERFCN_2D = GAUSS_BG_2D(x,y,npar,val)
  CASE(2)
     USERFCN_2D = GAUSS_LINE_BG_2D(x,y,npar,val)
  CASE(3)
     USERFCN_2D = LORE_LINE_BG_2D(x,y,npar,val)
  CASE(4)
     USERFCN_2D = VOIGT_LINE_BG_2D(x,y,npar,val)
  CASE(5)
     USERFCN_2D = SUPERGAUSS_LINE_BG_2D(x,y,npar,val)
  CASE(6)
     USERFCN_2D = ERFPEAK_LINE_BG_2D(x,y,npar,val)
  CASE(7)
     USERFCN_2D = TWO_ERFPEAK_LINE_BG_2D(x,y,npar,val)
  CASE(8)
     USERFCN_2D = TWO_LORE_LINE_BG_2D(x,y,npar,val)
  CASE(9)
     USERFCN_2D = TWO_LORE_LINE_BG_2D(x,y,npar,val)
  CASE(10)
     USERFCN_2D = TWO_LORE_WF_LINE_BG_2D(x,y,npar,val)
   CASE(11)
      USERFCN_2D = FOUR_LORE_WF_LINE_BG_2D(x,y,npar,val)
   CASE(12)
      USERFCN_2D = TWO_VOIGT_LINE_BG_2D(x,y,npar,val)
  END SELECT

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
  REAL(8) :: a, b, c, amp, sigma, y0, bg, x0, Dy

  a     = val(1)
  b     = val(2)
  c     = val(3)
  y0    = val(4)
  Dy    = val(5)
  amp   = val(6)
  sigma = val(7)
  bg    = val(8)

  x0 = a + b*(y-y0) + c*(y-y0)**2
  amp = amp/Dy    ! For the normalization along the plan perpendicular to the dispersion

  valg(1) = x0
  valg(2) = amp
  valg(3) = sigma


  GAUSS_LINE_BG_2D = GAUSS(x,3,valg) + bg

  RETURN



END FUNCTION GAUSS_LINE_BG_2D

!________________________________________________________________________________________

REAL(8) FUNCTION LORE_LINE_BG_2D(x,y,npar,val)
  ! Normalized Lorentzian line profile.
  ! x: dispersion axis --> Gaussian profile
  ! y: parallel axis --> slope of the line
  ! The value of 'amp' is the value of the volume below the curve

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(3) :: vall
  REAL(8) :: LORE
  REAL(8) :: a, b, c, amp, gamma, y0, bg, x0, Dy

  a     = val(1)
  b     = val(2)
  c     = val(3)
  y0    = val(4)
  Dy    = val(5)
  amp   = val(6)
  gamma = val(7)
  bg    = val(8)

  x0 = a + b*(y-y0) + c*(y-y0)**2
  amp = amp/Dy    ! For the normalization along the plan perpendicular to the dispersion

  vall(1) = x0
  vall(2) = amp
  vall(3) = gamma


  LORE_LINE_BG_2D = LORE(x,3,vall) + bg

  RETURN



END FUNCTION LORE_LINE_BG_2D

!________________________________________________________________________________________



REAL(8) FUNCTION VOIGT_LINE_BG_2D(x,y,npar,val)
  ! Normalized Voigt line profile.
  ! x: dispersion axis --> Voigt profile
  ! y: parallel axis --> slope of the line
  ! The value of 'amp' is the value of the volume below the curve

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(4) :: valv
  REAL(8) :: VOIGT
  REAL(8) :: a, b, c, amp, sigma, gamma, y0, bg, x0, Dy

  a     = val(1)
  b     = val(2)
  c     = val(3)
  y0    = val(4)
  Dy    = val(5)
  amp   = val(6)
  sigma = val(7)
  gamma = val(8)
  bg    = val(9)

  x0 = a + b*(y-y0) + c*(y-y0)**2
  amp = amp/Dy    ! For the normalization along the plan perpendicular to the dispersion

  valv(1) = x0
  valv(2) = amp
  valv(3) = sigma
  valv(4) = gamma


  VOIGT_LINE_BG_2D = VOIGT(x,4,valv) + bg

  RETURN



END FUNCTION VOIGT_LINE_BG_2D


!________________________________________________________________________________________


REAL(8) FUNCTION SUPERGAUSS_LINE_BG_2D(x,y,npar,val)
  ! Normalized Supergaussian line profile.
  ! x: dispersion axis --> Supergaussian profile
  ! y: parallel axis --> slope of the line
  ! The value of 'amp' is the value of the volume below the curve
  ! Dy and y0 are not variables but information for the heigth of the detector and
  ! the middle plan, respectively

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(3) :: valg
  REAL(8) :: SUPERGAUSS
  REAL(8) :: a, b, c, y0, Dy, amp, sigma, bg, x0

  a     = val(1)
  b     = val(2)
  c     = val(3)
  y0    = val(4)
  Dy    = val(5)
  amp   = val(6)
  sigma = val(7)
  bg    = val(8)

  x0 = a + b*(y-y0) + c*(y-y0)**2
  amp = amp/Dy    ! For the normalization along the plan perpendicular to the dispersion

  valg(1) = x0
  valg(2) = amp
  valg(3) = sigma

  SUPERGAUSS_LINE_BG_2D = SUPERGAUSS(x,3,valg) + bg

  RETURN



END FUNCTION SUPERGAUSS_LINE_BG_2D

!________________________________________________________________________________________


REAL(8) FUNCTION ERFPEAK_LINE_BG_2D(x,y,npar,val)
  ! Normalized errorfunction peak line profile.
  ! x: dispersion axis --> Gaussian-flat convolution profile
  ! y: parallel axis --> slope of the line
  ! The value of 'amp' is the value of the volume below the curve
  ! Dy and y0 are not variables but information for the heigth of the detector and
  ! the middle plan, respectively

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(4) :: vale
  REAL(8) :: ERFPEAK
  REAL(8) :: a, b, c, y0, Dy, amp, sigma, w, x0, bg

  a     = val(1)
  b     = val(2)
  c     = val(3)
  y0    = val(4)
  Dy    = val(5)
  amp   = val(6)
  sigma = val(7)
  w     = val(8)
  bg    = val(9)

  x0 = a + b*(y-y0) + c*(y-y0)**2
  amp = amp/Dy    ! For the normalization along the plan perpendicular to the dispersion

  vale(1) = x0
  vale(2) = amp
  vale(3) = sigma
  vale(4) = w

  ERFPEAK_LINE_BG_2D = ERFPEAK(x,4,vale) + bg

  RETURN



END FUNCTION ERFPEAK_LINE_BG_2D

!________________________________________________________________________________________


REAL(8) FUNCTION TWO_ERFPEAK_LINE_BG_2D(x,y,npar,val)
  ! Normalized two Lorentzian line profiles.
  ! x: dispersion axis --> Gaussian-flat convolution profile
  ! y: parallel axis --> slope of the line
  ! The value of 'amp' is the value of the volume below the curve
  ! Dy and y0 are not variables but information for the heigth of the detector and
  ! the middle plan, respectively

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(4) :: vale1, vale2
  REAL(8) :: ERFPEAK
  REAL(8) :: a, b, c, y0, Dy, dx, amp1, sigma, w, damp, amp2, bg, x01, x02
  ! To plot the different components
  LOGICAL :: plot
  COMMON /func_plot/ plot

  a     = val(1)
  b     = val(2)
  c     = val(3)
  y0    = val(4)
  Dy    = val(5)
  dx    = val(6)
  amp1  = val(7)
  damp  = val(8)
  sigma = val(9)
  w     = val(10)
  bg    = val(11)

  x01 = a + b*(y-y0) + c*(y-y0)**2
  x02 = x01 + dx
  amp1 = amp1/Dy    ! For the normalization along the plan perpendicular to the dispersion
  amp2 = amp1*damp

  vale1(1) = x01
  vale1(2) = amp1
  vale1(3) = sigma
  vale1(4) = w

  vale2(1) = x02
  vale2(2) = amp2
  vale2(3) = sigma
  vale2(4) = w


  TWO_ERFPEAK_LINE_BG_2D = ERFPEAK(x,4,vale1) + ERFPEAK(x,4,vale2) + bg

  RETURN



END FUNCTION TWO_ERFPEAK_LINE_BG_2D

!________________________________________________________________________________________


REAL(8) FUNCTION TWO_LORE_LINE_BG_2D(x,y,npar,val)
  ! Normalized two Lorentzian line profiles.
  ! x: dispersion axis --> Lorentzian profile
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
  ! To plot the different components
  LOGICAL :: plot
  COMMON /func_plot/ plot

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


REAL(8) FUNCTION TWO_LORE_WF_LINE_BG_2D(x,y,npar,val)
  ! Normalized two Lorentzian line profiles.
  ! x: dispersion axis --> Lorentzian profile
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
  REAL(8) :: a, b, c, y0, Dy, dx, amp1, gamma1, gamma2, damp, amp2, bg, x01, x02
  ! To plot the different components
  LOGICAL :: plot
  COMMON /func_plot/ plot

  a      = val(1)
  b      = val(2)
  c      = val(3)
  y0     = val(4)
  Dy     = val(5)
  dx     = val(6)
  amp1   = val(7)
  damp   = val(8)
  gamma1 = val(9)
  gamma2 = val(10)
  bg     = val(11)

  x01 = a + b*(y-y0) + c*(y-y0)**2
  x02 = x01 + dx
  amp1 = amp1/Dy    ! For the normalization along the plan perpendicular to the dispersion
  amp2 = amp1*damp

  vall1(1) = x01
  vall1(2) = amp1
  vall1(3) = gamma1

  vall2(1) = x02
  vall2(2) = amp2
  vall2(3) = gamma2


  TWO_LORE_WF_LINE_BG_2D = LORE(x,3,vall1) + LORE(x,3,vall2) + bg

  RETURN



END FUNCTION TWO_LORE_WF_LINE_BG_2D


!________________________________________________________________________________________

!________________________________________________________________________________________


REAL(8) FUNCTION FOUR_LORE_WF_LINE_BG_2D(x,y,npar,val)
  ! Normalized four Lorentzian line profiles.
  ! x: dispersion axis --> Lorentzian profile
  ! y: parallel axis --> slope of the line
  ! The value of 'amp' is the value of the volume below the curve
  ! Dy and y0 are not variables but information for the heigth of the detector and
  ! the middle plan, respectively

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(3) :: vall1, vall2, vall3, vall4
  REAL(8) :: LORE
  REAL(8) :: a, b, c, y0, Dy
  REAL(8) :: amp1, gamma1, dx2, damp2, gamma2
  REAL(8) :: dx3, damp3, gamma3, dx4, damp4, gamma4
  REAL(8) :: x01, x02, x03, x04, amp2, amp3, amp4, bg
  ! To plot the different components
  LOGICAL :: plot
  COMMON /func_plot/ plot

  a      = val(1)
  b      = val(2)
  c      = val(3)
  y0     = val(4)
  Dy     = val(5)
  dx2    = val(6)
  dx3    = val(7)
  dx4    = val(8)
  amp1   = val(9)
  damp2  = val(10)
  damp3  = val(11)
  damp4  = val(12)
  gamma1 = val(13)
  gamma2 = val(14)
  gamma3 = val(15)
  gamma4 = val(16)
  bg     = val(17)

  x01 = a + b*(y-y0) + c*(y-y0)**2
  x02 = x01 + dx2
  x03 = x01 + dx3
  x04 = x02 + dx4
  amp1 = amp1/Dy    ! For the normalization along the plan perpendicular to the dispersion
  amp2 = amp1*damp2
  amp3 = amp1*damp3
  amp4 = amp1*damp4

  vall1(1) = x01
  vall1(2) = amp1
  vall1(3) = gamma1

  vall2(1) = x02
  vall2(2) = amp2
  vall2(3) = gamma2

  vall3(1) = x03
  vall3(2) = amp3
  vall3(3) = gamma3

  vall4(1) = x04
  vall4(2) = amp4
  vall4(3) = gamma4


  FOUR_LORE_WF_LINE_BG_2D = LORE(x,3,vall1) + LORE(x,3,vall2) + LORE(x,3,vall3) + LORE(x,3,vall4)+ bg

  RETURN



END FUNCTION FOUR_LORE_WF_LINE_BG_2D


!________________________________________________________________________________________



REAL(8) FUNCTION TWO_VOIGT_LINE_BG_2D(x,y,npar,val)
  ! Normalized two Voigt line profiles.
  ! x: dispersion axis --> Lorentzian profile
  ! y: parallel axis --> slope of the line
  ! The value of 'amp' is the value of the volume below the curve
  ! Dy and y0 are not variables but information for the heigth of the detector and
  ! the middle plan, respectively

  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x, y
  INTEGER(4), INTENT(IN) :: npar
  REAL(8), DIMENSION(npar), INTENT(IN) :: val
  !
  REAL(8), DIMENSION(4) :: valv1, valv2
  REAL(8) :: VOIGT
  REAL(8) :: a, b, c, y0, Dy, dx, amp1, sigma, gamma, damp, amp2, bg, x01, x02

  a     = val(1)
  b     = val(2)
  c     = val(3)
  y0    = val(4)
  Dy    = val(5)
  dx    = val(6)
  amp1  = val(7)
  damp  = val(8)
  sigma = val(9)
  gamma = val(10)
  bg    = val(11)

  x01 = a + b*(y-y0) + c*(y-y0)**2
  x02 = x01 + dx
  amp1 = amp1/Dy    ! For the normalization along the plan perpendicular to the dispersion
  amp2 = amp1*damp

  valv1(1) = x01
  valv1(2) = amp1
  valv1(3) = sigma
  valv1(4) = gamma

  valv2(1) = x02
  valv2(2) = amp2
  valv2(3) = sigma
  valv2(4) = gamma


  TWO_VOIGT_LINE_BG_2D = VOIGT(x,4,valv1) + VOIGT(x,4,valv2) + bg

  RETURN



END FUNCTION TWO_VOIGT_LINE_BG_2D


!________________________________________________________________________________________
