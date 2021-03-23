c     Automatic Time-stamp: <Last changed by martino on Tuesday 07 April 2020 at CEST 18:34:52>
c################################### USERFCN DEFINITION #####################################



      FUNCTION USERFCN(x,npar,val,funcname)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 USERFCN, GAUSS, GAUSS_BG, DOUBLE_GAUSS_BG, DOUBLET_GAUSS_BG
      REAL*8 TRIPLE_GAUSS_BG,QUAD_GAUSS_BG,QUINT_GAUSS_BG,SIX_GAUSS_BG
      REAL*8 SIX_GAUSS_EXPBG_WF, SIX_GAUSS_DBEXPBG_WF
      REAL*8 LORE, LORENORM, LORE_BG, CONS
      REAL*8 SIX_VOIGT_BG, SIX_VOIGT_XRD, SIX_VOIGT_EXP_BG
      REAL*8 EIGHT_GAUSS_POLYBG_WF, EIGHT_VOIGT_POLYBG_WF
      REAL*8 EXPFCN, ERFFCN,BS_EM, BS_EM2, BS_EM_NM
      REAL*8 POWER, ND_M_PLEIADES
      REAL*8 POLY, SIX_VOIGT_POLYBG,SIX_VOIGT_EXP_POLYBG
      REAL*8 SIX_VOIGT_POLYBG_WF
      REAL*8 MB_BG, GAUSS_EXP_BG, VOIGT, DOUBLE_VOIGT_BG
      REAL*8 GAUSS_EXP_BG_CONV,GAUSS_GAUSS_EXP_BG
      REAL*8 VOIGT_BG,VOIGT_EXP, VOIGT_EXP_BG, DOUBLE_VOIGT_EXP_BG
      REAL*8 SIX_VOIGT_EXPBG_WF, WEIBULL, WEIBULL_BG, WEIBULL_ERFBG
      REAL*8 LASER, THRESHOLD
      REAL*8 GAUSS_ERF, VOIGT_ERF, TWO_GAUSS_ERF_EXPBG
      REAL*8 TWO_DOUBL_GAUSS_ERF_POLY,TWO_DOUBL_GAUSS_ERF_FREESIG_POLY
      REAL*8 TWO_DOUBL_VOIGT_ERF_POLY
      REAL*8 EXPSIN, EXPCOS, EXPSIMP, TWO_EXPSIN
      REAL*8 FOUR_VOIGT_BG, SIX_VOIGT_FREEGAMMA_BG
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY, SIX_GAUSS_ERF_FREESIG_POLY2
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN
      REAL*8 GAUSS_ERF_CST, GAUSSERF_CST, FOUR_VOIGT_BG_PLEIADES
      REAL*8 FOUR_GAUSS_BG_PLEIADES, FOUR_PSEUDOVOIGT_BG_PLEIADES
      REAL*8 FOUR_GAUSS_PARAMETER_BG_PLEIADES
      REAL*8 FOUR_VOIGT_PARAMETER_BG_PLEIADES
      REAL*8 SIX_VOIGT_PARA_POLY_SIG_PLEIADES
      REAL*8 SIX_VOIGT_PARAMETER_BG_PLEIADES
      REAL*8 SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES
      REAL*8 SIX_GAUSS_PARAMETER_BG_PLEIADES
      REAL*8 SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES
      REAL*8 SIX_GAUSS_SHIRLEYBG
      REAL*8 SIX_VOIGT_SHIRLEYBG
      REAL*8 SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES
      REAL*8 ROCKING_CURVE,TWO_INTERP_VOIGT_POLY,THREE_INTERP_VOIGT_POLY
      REAL*8 TWO_INTERP_VOIGT_POLY_X0
      REAL*8 GAUSS_3D
      REAL*8, ALLOCATABLE, DIMENSION(:) ::  x
      CHARACTER*64 funcname

c     Choose your model (see below for definition)
      IF(funcname.EQ.'GAUSS') THEN
         USERFCN = GAUSS(x,npar,val)
      ELSE IF(funcname.EQ.'GAUSS_BG') THEN
         USERFCN = GAUSS_BG(x,npar,val)
      ELSE IF(funcname.EQ.'LORE') THEN
         USERFCN = LORE(x,npar,val)
      ELSE IF(funcname.EQ.'LORENORM') THEN
         USERFCN = LORENORM(x,npar,val)
      ELSE IF(funcname.EQ.'LORE_BG') THEN
         USERFCN = LORE_BG(x,npar,val)
      ELSE IF(funcname.EQ.'CONS') THEN
         USERFCN = CONS(x,npar,val)
      ELSE IF(funcname.EQ.'DOUBLE_GAUSS_BG') THEN
         USERFCN = DOUBLE_GAUSS_BG(x,npar,val)
      ELSE IF(funcname.EQ.'DOUBLET_GAUSS_BG') THEN
         USERFCN = DOUBLET_GAUSS_BG(x,npar,val)
      ELSE IF(funcname.EQ.'TRIPLE_GAUSS_BG') THEN
         USERFCN = TRIPLE_GAUSS_BG(x,npar,val)
      ELSE IF(funcname.EQ.'QUAD_GAUSS_BG') THEN
         USERFCN = QUAD_GAUSS_BG(x,npar,val)
      ELSE IF(funcname.EQ.'QUINT_GAUSS_BG') THEN
         USERFCN = QUINT_GAUSS_BG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_GAUSS_BG') THEN
         USERFCN = SIX_GAUSS_BG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_GAUSS_EXPBG_WF') THEN
         USERFCN = SIX_GAUSS_EXPBG_WF(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_GAUSS_DBEXPBG_WF') THEN
         USERFCN = SIX_GAUSS_DBEXPBG_WF(x,npar,val)
      ELSE IF(funcname.EQ.'EIGHT_GAUSS_POLYBG_WF') THEN
         USERFCN = EIGHT_GAUSS_POLYBG_WF(x,npar,val)
      ELSE IF(funcname.EQ.'EIGHT_VOIGT_POLYBG_WF') THEN
         USERFCN = EIGHT_VOIGT_POLYBG_WF(x,npar,val)
      ELSE IF(funcname.EQ.'EXPFCN') THEN
         USERFCN = EXPFCN(x,npar,val)
      ELSE IF(funcname.EQ.'ERFFCN') THEN
         USERFCN = ERFFCN(x,npar,val)
      ELSE IF(funcname.EQ.'MB_BG') THEN
         USERFCN = MB_BG(x,npar,val)
      ELSE IF(funcname.EQ.'GAUSS_EXP_BG') THEN
         USERFCN = GAUSS_EXP_BG(x,npar,val)
      ELSE IF(funcname.EQ.'GAUSS_GAUSS_EXP_BG') THEN
         USERFCN = GAUSS_GAUSS_EXP_BG(x,npar,val)
      ELSE IF(funcname.EQ.'GAUSS_EXP_BG_CONV') THEN
         USERFCN = GAUSS_EXP_BG_CONV(x,npar,val)
      ELSE IF(funcname.EQ.'VOIGT') THEN
         USERFCN = VOIGT(x,npar,val)
      ELSE IF(funcname.EQ.'VOIGT_BG') THEN
         USERFCN = VOIGT_BG(x,npar,val)
      ELSE IF(funcname.EQ.'VOIGT_EXP') THEN
         USERFCN = VOIGT_EXP(x,npar,val)
      ELSE IF(funcname.EQ.'VOIGT_ERF') THEN
         USERFCN = VOIGT_ERF(x,npar,val)
      ELSE IF(funcname.EQ.'VOIGT_EXP_BG') THEN
         USERFCN = VOIGT_EXP_BG(x,npar,val)
      ELSE IF(funcname.EQ.'DOUBLE_VOIGT_BG') THEN
         USERFCN = DOUBLE_VOIGT_BG(x,npar,val)
      ELSE IF(funcname.EQ.'DOUBLE_VOIGT_EXP_BG') THEN
         USERFCN = DOUBLE_VOIGT_EXP_BG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_BG') THEN
         USERFCN = SIX_VOIGT_BG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_POLYBG') THEN
         USERFCN = SIX_VOIGT_POLYBG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_XRD') THEN
         USERFCN = SIX_VOIGT_XRD(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_POLYBG_WF') THEN
         USERFCN = SIX_VOIGT_POLYBG_WF(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_EXP_BG') THEN
         USERFCN = SIX_VOIGT_EXP_BG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_FREEGAMMA_BG') THEN
         USERFCN = SIX_VOIGT_FREEGAMMA_BG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY') THEN
         USERFCN = SIX_GAUSS_ERF_FREESIG_POLY(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY2') THEN
         USERFCN = SIX_GAUSS_ERF_FREESIG_POLY2(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_EXP_POLYBG') THEN
         USERFCN = SIX_VOIGT_EXP_POLYBG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_EXPBG_WF') THEN
         USERFCN =SIX_VOIGT_EXPBG_WF(x,npar,val)
      ELSE IF(funcname.EQ.'TWO_INTERP_VOIGT_POLY') THEN
         USERFCN =TWO_INTERP_VOIGT_POLY(x,npar,val)
      ELSE IF(funcname.EQ.'TWO_INTERP_VOIGT_POLY_X0') THEN
         USERFCN =TWO_INTERP_VOIGT_POLY_X0(x,npar,val)
      ELSE IF(funcname.EQ.'THREE_INTERP_VOIGT_POLY') THEN
         USERFCN =THREE_INTERP_VOIGT_POLY(x,npar,val)
      ELSE IF(funcname.EQ.'WEIBULL') THEN
         USERFCN = WEIBULL(x,npar,val)
      ELSE IF(funcname.EQ.'WEIBULL_BG') THEN
         USERFCN = WEIBULL_BG(x,npar,val)
      ELSE IF(funcname.EQ.'WEIBULL_ERFBG') THEN
         USERFCN = WEIBULL_ERFBG(x,npar,val)
      ELSE IF(funcname.EQ.'LASER') THEN
         USERFCN = LASER(x,npar,val)
      ELSE IF(funcname.EQ.'THRESHOLD') THEN
         USERFCN = THRESHOLD(x,npar,val)
      ELSE IF(funcname.EQ.'GAUSS_ERF') THEN
         USERFCN = GAUSS_ERF(x,npar,val)
      ELSE IF(funcname.EQ.'TWO_GAUSS_ERF_EXPBG') THEN
         USERFCN = TWO_GAUSS_ERF_EXPBG(x,npar,val)
      ELSE IF(funcname.EQ.'TWO_DOUBL_GAUSS_ERF_POLY') THEN
         USERFCN = TWO_DOUBL_GAUSS_ERF_POLY(x,npar,val)
      ELSE IF(funcname.EQ.'TWO_DOUBL_GAUSS_ERF_FREESIG_POLY') THEN
         USERFCN = TWO_DOUBL_GAUSS_ERF_FREESIG_POLY(x,npar,val)
      ELSE IF(funcname.EQ.'TWO_DOUBL_VOIGT_ERF_POLY') THEN
         USERFCN = TWO_DOUBL_VOIGT_ERF_POLY(x,npar,val)
      ELSE IF(funcname.EQ.'POLY') THEN
         USERFCN = POLY(x,npar,val)
      ELSE IF(funcname.EQ.'POWER') THEN
         USERFCN = POWER(x,npar,val)
      ELSE IF(funcname.EQ.'ND_M_PLEIADES') THEN
         USERFCN = ND_M_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'BS_EM') THEN
         USERFCN = BS_EM(x,npar,val)
      ELSE IF(funcname.EQ.'BS_EM2') THEN
         USERFCN = BS_EM2(x,npar,val)
      ELSE IF(funcname.EQ.'BS_EM_NM') THEN
         USERFCN = BS_EM_NM(x,npar,val)
      ELSE IF(funcname.EQ.'EXPCOS') THEN
         USERFCN = EXPCOS(x,npar,val)
      ELSE IF(funcname.EQ.'EXPSIN') THEN
         USERFCN = EXPSIN(x,npar,val)
      ELSE IF(funcname.EQ.'EXPSIMP') THEN
         USERFCN = EXPSIMP(x,npar,val)
      ELSE IF(funcname.EQ.'TWO_EXPSIN') THEN
         USERFCN = TWO_EXPSIN(x,npar,val)
      ELSE IF(funcname.EQ.'FOUR_VOIGT_BG') THEN
         USERFCN = FOUR_VOIGT_BG(x,npar,val)
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS') THEN
         USERFCN = FOUR_GAUSS_ERF_TWO_GAUSS(x,npar,val)
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN') THEN
         USERFCN = FOUR_GAUSS_ERF_TWO_GAUSS_STAN(x,npar,val)
      ELSE IF(funcname.EQ.'GAUSS_ERF_CST') THEN
         USERFCN = GAUSS_ERF_CST(x,npar,val)
      ELSE IF(funcname.EQ.'GAUSSERF_CST') THEN
         USERFCN = GAUSSERF_CST(x,npar,val)
      ELSE IF(funcname.EQ.'FOUR_VOIGT_BG_PLEIADES') THEN
         USERFCN = FOUR_VOIGT_BG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'FOUR_GAUSS_BG_PLEIADES') THEN
         USERFCN = FOUR_GAUSS_BG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'FOUR_PSEUDOVOIGT_BG_PLEIADES') THEN
         USERFCN = FOUR_PSEUDOVOIGT_BG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'FOUR_GAUSS_PARAMETER_BG_PLEIADES') THEN
         USERFCN = FOUR_GAUSS_PARAMETER_BG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'FOUR_VOIGT_PARAMETER_BG_PLEIADES') THEN
         USERFCN = FOUR_VOIGT_PARAMETER_BG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_PARAMETER_BG_PLEIADES') THEN
         USERFCN = SIX_VOIGT_PARAMETER_BG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_GAUSS_PARAMETER_BG_PLEIADES') THEN
         USERFCN = SIX_GAUSS_PARAMETER_BG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES') THEN
         USERFCN = SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_GAUSS_SHIRLEYBG') THEN
         USERFCN = SIX_GAUSS_SHIRLEYBG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_SHIRLEYBG') THEN
         USERFCN = SIX_VOIGT_SHIRLEYBG(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES') THEN
         USERFCN = SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES') THEN
         USERFCN = SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'ROCKING_CURVE') THEN
         USERFCN = ROCKING_CURVE(x,npar,val)
      ELSE IF(funcname.EQ.'SIX_VOIGT_PARA_POLY_SIG_PLEIADES') THEN
         USERFCN = SIX_VOIGT_PARA_POLY_SIG_PLEIADES(x,npar,val)
      ELSE IF(funcname.EQ.'GAUSS_3D') THEN
         USERFCN = GAUSS_3D(x,npar,val)
      ELSE
         WRITE(*,*) 'Error in the function name def. in USERFCN'
         WRITE(*,*) 'Check in the manual and in the input.dat file'
         STOP
      END IF

      RETURN
      END


c################################### LINESHAPE DEFINITIONS #####################################

      FUNCTION GAUSS(X,npar,val)
c     Normalized Gaussian distribution
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma

      x0    = val(1)
      amp   = val(2)
      sigma = val(3)

c     Test of under of underflow first
      IF(DABS(-(x-x0)**2/(2*sigma**2)).LT.700) THEN
         GAUSS = amp/(dsqrt(2*pi)*sigma)*
     +        dexp(-(x-x0)**2/(2*sigma**2))
      ELSE
         GAUSS = 0.d0
      END IF

      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION GAUSS_BG(X,npar,val)
c     Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3)
      REAL*8 GAUSS, GAUSS_BG, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma, bg
      LOGICAL plot
      COMMON /func_plot/ plot

      bg    = val(1)
      x0    = val(2)
      amp   = val(3)
      sigma = val(4)

c     for the pure gauss peak
      val1(1) = x0
      val1(2) = amp
      val1(3) = sigma

      GAUSS_BG = GAUSS(x,3,val1) + bg

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, GAUSS_BG, GAUSS(x,3,val1), bg
      END IF

      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION LORE(X,npar,val)
c     Normalized Lorentzian distribution
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 LORE, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, gamma

      x0    = val(1)
      amp   = val(2)
      gamma = val(3)

      LORE = amp*gamma/(pi*((x-x0)**2+gamma**2))

      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION LORENORM(X,npar,val)
c     Normalized Lorentzian distribution
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 LORENORM, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, gamma

      x0    = val(1)
      amp   = val(2)
      gamma = val(3)

      LORENORM = ((amp*gamma)/(2*pi))*(1/((x-x0)**2+gamma**2/4))

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION LORE_BG(X,npar,val)
c     Normalized Lorentzian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3)
      REAL*8 LORE, LORE_BG, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, gamma, bg

      bg    = val(1)
      x0    = val(2)
      amp   = val(3)
      gamma = val(4)

c     for the pure gauss peak
      val1(1) = x0
      val1(2) = amp
      val1(3) = gamma

      LORE_BG = LORE(x,3,val1) + bg

      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION CONS(X,npar,val)
c     Conservative formulation long a parabola for 2D fits following Sivia Ch. 8.3.1
c     See functions.nb for more explainations
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 CONS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma

      x0    = val(1)
      amp   = val(2)
      sigma = val(3)

c     Test of under of overflow first
      IF(DABS(-(x-x0)**2/(2*sigma**2)).LT.700) THEN
         IF(x.EQ.x0) THEN
            CONS = amp/(2*dsqrt(2*pi)*sigma)
         ELSE
            CONS = amp/(dsqrt(2*pi)*sigma)*
     +           (1-dexp(-(x-x0)**2/(2*sigma**2)))/
     +           ((x-x0)/sigma)**2
         END IF
      ELSE
         CONS = 0.d0
      END IF

c      write(*,*) x, -(x-x0)**2/(2*sigma**2), CONS

      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION DOUBLE_GAUSS_BG(X,npar,val)
c     2 Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3), val2(3)
      REAL*8 DOUBLE_GAUSS_BG, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, amp1, amp2, sigma, bg

      bg    = val(1)
      x01   = val(2)
      x02   = val(3)
      amp1  = val(4)
      amp2  = val(5)
      sigma = val(6)

c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma


c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma

      DOUBLE_GAUSS_BG = GAUSS(x,3,val1) + GAUSS(x,3,val2) + bg

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION DOUBLET_GAUSS_BG(X,npar,val)
c     2 Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3), val2(3)
      REAL*8 DOUBLET_GAUSS_BG, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx, amp1, damp, sigma, bg

      bg    = val(1)
      x01   = val(2)
      dx    = val(3)
      amp1  = val(4)
      damp  = val(5)
      sigma = val(6)

c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma


c     second gauss peak
      val2(1) = x01 + dx
      val2(2) = amp1*damp
      val2(3) = sigma


      DOUBLET_GAUSS_BG = GAUSS(x,3,val1) + GAUSS(x,3,val2) + bg

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION TRIPLE_GAUSS_BG(X,npar,val)
c     2 Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3), val2(3), val3(3)
      REAL*8 TRIPLE_GAUSS_BG, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, amp1, amp2, amp3, sigma, bg

      bg    = val(1)
      x01   = val(2)
      x02   = val(3)
      x03   = val(4)
      amp1  = val(5)
      amp2  = val(6)
      amp3  = val(7)
      sigma = val(8)
c      sigma = 12.d0

c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma


      TRIPLE_GAUSS_BG = GAUSS(x,3,val1) + GAUSS(x,3,val2) +
     +     GAUSS(x,3,val3) + bg

      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION QUAD_GAUSS_BG(X,npar,val)
c     2 Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3), val2(3), val3(3), val4(3)
      REAL*8 QUAD_GAUSS_BG, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, amp1, amp2, amp3, amp4, sigma, bg

      bg    = val(1)
      x01   = val(2)
      x02   = val(3)
      x03   = val(4)
      x04   = val(5)
      amp1  = val(6)
      amp2  = val(7)
      amp3  = val(8)
      amp4  = val(9)
c      sigma = val(10)
      sigma = 12.d0

c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma


      QUAD_GAUSS_BG = GAUSS(x,3,val1) + GAUSS(x,3,val2) +
     +     GAUSS(x,3,val3) +  GAUSS(x,3,val4) + bg

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION QUINT_GAUSS_BG(X,npar,val)
c     2 Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3), val2(3), val3(3), val4(3), val5(3)
      REAL*8 QUINT_GAUSS_BG, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05
      REAL*8 amp1, amp2, amp3, amp4, amp5, sigma, bg

      bg    = val(1)
      x01   = val(2)
      x02   = val(3)
      x03   = val(4)
      x04   = val(5)
      x05   = val(6)
      amp1  = val(7)
      amp2  = val(8)
      amp3  = val(9)
      amp4  = val(10)
      amp5  = val(11)
c      sigma = val(12)
      sigma = 12.d0

c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma


      QUINT_GAUSS_BG = GAUSS(x,3,val1) + GAUSS(x,3,val2) +
     +     GAUSS(x,3,val3) +  GAUSS(x,3,val4) + GAUSS(x,3,val5) + bg

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION SIX_GAUSS_BG(X,npar,val)
c     2 Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar),val1(3),val2(3),val3(3),val4(3),val5(3),val6(3)
      REAL*8 SIX_GAUSS_BG, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6, sigma, bg

      bg    = val(1)
      x01   = val(2)
      x02   = val(3)
      x03   = val(4)
      x04   = val(5)
      x05   = val(6)
      x06   = val(7)
      amp1  = val(8)
      amp2  = val(9)
      amp3  = val(10)
      amp4  = val(11)
      amp5  = val(12)
      amp6  = val(13)
      sigma = val(14)
c      sigma = 12.d0

c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma


      SIX_GAUSS_BG = GAUSS(x,3,val1) + GAUSS(x,3,val2) +
     + GAUSS(x,3,val3) +  GAUSS(x,3,val4) + GAUSS(x,3,val5)
     + + GAUSS(x,3,val6) + bg

      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION SIX_GAUSS_EXPBG_WF(X,npar,val)
c     6 Normalized Gaussian distribution plus exponential background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar),val1(3),val2(3),val3(3),val4(3),val5(3),val6(3)
      REAL*8 SIX_GAUSS_EXPBG_WF, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma1, sigma2, sigma3, sigma4, sigma5, sigma6
      REAL*8 bg, bg_amp, bg_tau
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma1 = val(13)
      sigma2 = val(14)
      sigma3 = val(15)
      sigma4 = val(16)
      sigma5 = val(17)
      sigma6 = val(18)
      bg_amp = val(19)
      bg_tau = val(20)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma2

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma3

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma4

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma5

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma6

c     exponential background
      IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
         bg = bg_amp*dexp(-x/bg_tau)
      ELSE IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
         bg = bg_amp*dexp(+x/bg_tau)
      END IF

      SIX_GAUSS_EXPBG_WF = GAUSS(x,3,val1) + GAUSS(x,3,val2) +
     + GAUSS(x,3,val3) +  GAUSS(x,3,val4) + GAUSS(x,3,val5)
     + + GAUSS(x,3,val6) + bg

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION SIX_GAUSS_DBEXPBG_WF(X,npar,val)
c     6 Normalized Gaussian distribution plus two exponential background:
c     one for the high energy side (bremmshtrahlung, efficiency,...)
c     and one for the low energy side (windows absorption)
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar),val1(3),val2(3),val3(3),val4(3),val5(3),val6(3)
      REAL*8 SIX_GAUSS_DBEXPBG_WF, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma1, sigma2, sigma3, sigma4, sigma5, sigma6
      REAL*8 bg, bg_amp, bg_tau0, bg_tau1, bg_tau2, bg_x0
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01     = val(1)
      x02     = val(2)
      x03     = val(3)
      x04     = val(4)
      x05     = val(5)
      x06     = val(6)
      amp1    = val(7)
      amp2    = val(8)
      amp3    = val(9)
      amp4    = val(10)
      amp5    = val(11)
      amp6    = val(12)
      sigma1  = val(13)
      sigma2  = val(14)
      sigma3  = val(15)
      sigma4  = val(16)
      sigma5  = val(17)
      sigma6  = val(18)
      bg_amp  = val(19)
      bg_tau0 = val(20)
      bg_tau1 = val(21)
      bg_tau2 = val(22)
      bg_x0 = val(23)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma2

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma3

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma4

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma5

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma6

c     Quadruple exponential background

      bg = bg_amp*(1-dexp(-(x-bg_x0)/bg_tau1))
     +     *dexp(-(x-bg_x0)/bg_tau0)

      SIX_GAUSS_DBEXPBG_WF = GAUSS(x,3,val1) + GAUSS(x,3,val2) +
     + GAUSS(x,3,val3) +  GAUSS(x,3,val4) + GAUSS(x,3,val5)
     + + GAUSS(x,3,val6) + bg

      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION EIGHT_GAUSS_POLYBG_WF(X,npar,val)
c     6 Normalized Gaussian distribution plus exponential background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar),val1(3),val2(3),val3(3),val4(3),val5(3),val6(3)
      REAL*8 val7(3), val8(3), valp(8)
      REAL*8 EIGHT_GAUSS_POLYBG_WF, POLY, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06, x07, x08
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6, amp7, amp8
      REAL*8 sigma1,sigma2,sigma3,sigma4,sigma5,sigma6,sigma7,sigma8
      REAL*8 a,b,c,d,e,f,g

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      x07    = val(7)
      x08    = val(8)
      amp1   = val(9)
      amp2   = val(10)
      amp3   = val(11)
      amp4   = val(12)
      amp5   = val(13)
      amp6   = val(14)
      amp7   = val(15)
      amp8   = val(16)
      sigma1 = val(17)
      sigma2 = val(18)
      sigma3 = val(19)
      sigma4 = val(20)
      sigma5 = val(21)
      sigma6 = val(22)
      sigma7 = val(23)
      sigma8 = val(24)
      a      = val(25)
      b      = val(26)
      c      = val(27)
      d      = val(28)
      e      = val(29)
      f      = val(30)
      g      = val(31)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma2

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma3

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma4

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma5

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma6

c     seventh gauss peak
      val7(1) = x07
      val7(2) = amp7
      val7(3) = sigma7

c     eighth gauss peak
      val8(1) = x08
      val8(2) = amp8
      val8(3) = sigma8

c     polynomial background
      valp(1) = 0.
      valp(2) = a
      valp(3) = b
      valp(4) = c
      valp(5) = d
      valp(6) = e
      valp(7) = f
      valp(8) = g

      EIGHT_GAUSS_POLYBG_WF = GAUSS(x,3,val1) + GAUSS(x,3,val2) +
     + GAUSS(x,3,val3) +  GAUSS(x,3,val4) + GAUSS(x,3,val5)
     + + GAUSS(x,3,val6) + GAUSS(x,3,val7)+ GAUSS(x,3,val8) +
     + POLY(x,8,valp)

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION SIX_VOIGT_EXPBG_WF(X,npar,val)
c     6 Normalized Gaussian distribution plus exponential background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar),val1(4),val2(4),val3(4),val4(4),val5(4),val6(4)
      REAL*8 SIX_VOIGT_EXPBG_WF, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma1, sigma2, sigma3, sigma4, sigma5, sigma6
      REAL*8 gamma1, gamma2, gamma3, gamma4, gamma5, gamma6
      REAL*8 bg, bg_amp, bg_tau
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma1 = val(13)
      sigma2 = val(14)
      sigma3 = val(15)
      sigma4 = val(16)
      sigma5 = val(17)
      sigma6 = val(18)
      gamma1 = val(19)
      gamma2 = val(20)
      gamma3 = val(21)
      gamma4 = val(22)
      gamma5 = val(23)
      gamma6 = val(24)
      bg_amp = val(25)
      bg_tau = val(26)


c     first voigt peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1
      val1(4) = gamma1

c     second voigt peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma2
      val2(4) = gamma2

c     third voigt peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma3
      val3(4) = gamma3

c     fourth voigt peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma4
      val4(4) = gamma4

c     fifth voigt peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma5
      val5(4) = gamma5

c     sixth voigt peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma6
      val6(4) = gamma6

c     exponential background
      IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
         bg = bg_amp*dexp(-x/bg_tau)
      ELSE IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
         bg = bg_amp*dexp(+x/bg_tau)
      END IF

      SIX_VOIGT_EXPBG_WF = VOIGT(x,4,val1) + VOIGT(x,4,val2) +
     + VOIGT(x,4,val3) +  VOIGT(x,4,val4) + VOIGT(x,4,val5)
     + + VOIGT(x,4,val6) + bg

      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION EIGHT_VOIGT_POLYBG_WF(X,npar,val)
c     6 Normalized Gaussian distribution plus exponential background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar),val1(4),val2(4),val3(4),val4(4),val5(4),val6(4)
      REAL*8 val7(4), val8(4), valp(8)
      REAL*8 EIGHT_VOIGT_POLYBG_WF, VOIGT, POLY, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06, x07, x08
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6, amp7, amp8
      REAL*8 sigma1,sigma2,sigma3,sigma4,sigma5,sigma6,sigma7,sigma8
      REAL*8 gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,gamma8
      REAL*8 a, b, c, d, e, f, g
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      x07    = val(7)
      x08    = val(8)
      amp1   = val(9)
      amp2   = val(10)
      amp3   = val(11)
      amp4   = val(12)
      amp5   = val(13)
      amp6   = val(14)
      amp7   = val(15)
      amp8   = val(16)
      sigma1 = val(17)
      sigma2 = val(18)
      sigma3 = val(19)
      sigma4 = val(20)
      sigma5 = val(21)
      sigma6 = val(22)
      sigma7 = val(23)
      sigma8 = val(24)
      gamma1 = val(25)
      gamma2 = val(26)
      gamma3 = val(27)
      gamma4 = val(28)
      gamma5 = val(29)
      gamma6 = val(30)
      gamma7 = val(31)
      a      = val(32)
      b      = val(33)
      c      = val(34)
      d      = val(35)
      e      = val(36)
      f      = val(37)
      g      = val(38)


c     first voigt peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1
      val1(4) = gamma1

c     second voigt peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma2
      val2(4) = gamma2

c     third voigt peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma3
      val3(4) = gamma3

c     fourth voigt peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma4
      val4(4) = gamma4

c     fifth voigt peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma5
      val5(4) = gamma5

c     sixth voigt peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma6
      val6(4) = gamma6

c     seventh voigt peak
      val7(1) = x07
      val7(2) = amp7
      val7(3) = sigma7
      val7(4) = gamma7

c     sixth voigt peak
      val8(1) = x08
      val8(2) = amp8
      val8(3) = sigma8
      val8(4) = gamma8

      EIGHT_VOIGT_POLYBG_WF = VOIGT(x,4,val1) + VOIGT(x,4,val2) +
     + VOIGT(x,4,val3) +  VOIGT(x,4,val4) + VOIGT(x,4,val5)
     + + VOIGT(x,4,val6) + VOIGT(x,4,val7)+ VOIGT(x,4,val8)
     + + POLY(x,8,valp)

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION ERFFCN(X,npar,val)
c     Error function from 0 to 1 when lr = 'R'
c     and from 1 to 0 when lr = 'L'
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 ERFFCN, x
      REAL*8 pi
      REAL*8 x0
      REAL*8 amp
      REAL*8 sigma
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x0     = val(1)
      amp    = val(2)
      sigma  = val(3)


c     exponential background
      IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
         ERFFCN = amp*(DERF(+(x-x0)/(dsqrt(2.D0)*sigma))+1)/2
      ELSE IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
         ERFFCN = amp*(DERF(-(x-x0)/(dsqrt(2.D0)*sigma))+1)/2
      END IF

      RETURN
      END

c _______________________________________________________________________________________________


c _______________________________________________________________________________________________

      FUNCTION EXPFCN(X,npar,val)
c     Exponential
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 EXPFCN, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0
      REAL*8 amp
      REAL*8 tau
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x0     = val(1)
      amp    = val(2)
      tau    = val(3)


c     exponential background
      IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
         EXPFCN = amp*dexp(-(x-x0)/tau)
      ELSE IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
         EXPFCN = amp*dexp(+(x-x0)/tau)
      END IF

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION EXPSIMP(X,npar,val)
c     Exponential
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 EXPSIMP, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 amp
      REAL*8 tau
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      amp    = val(1)
      tau    = val(2)


c     exponential background
      IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
         EXPSIMP = amp*dexp(-x/tau)
      ELSE IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
         EXPSIMP = amp*dexp(+x/tau)
      END IF

      RETURN
      END

c _______________________________________________________________________________________________



      FUNCTION MB_BG(X,npar,val)
c     Normalized Maxwell-Boltzmann distribution plus background
c     The value of 'amp' is the value of the surface below the curve
c     Change the definition of x0 and the discrimination IF-ELSE to have
c     the asymmetry in the other side
c     see functions.nb
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 MB_BG, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr


      bg    = val(1)
      IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
c    Asymmetry in the left side
         x0    = val(2) + DSQRT(2.d0)*val(4)
      ELSE IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
c    Asymmetry in the right side
         x0    = val(2) - DSQRT(2.d0)*val(4)
      END IF
      amp   = val(3)
      sigma = val(4)

      IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
c    Asymmetry in the left side
         IF ((x-x0).LT.0) THEN
            MB_BG = amp*2*(x-x0)**2/(sqrt(2*pi)*
     +           sigma**3)*
     +           dexp(-(x-x0)**2/(2*sigma**2)) + bg
         ELSE
            MB_BG = bg
         END IF
      ELSE IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
c    Asymmetry in the right side
         IF ((x-x0).GT.0) THEN
            MB_BG = amp*2*(x-x0)**2/(sqrt(2*pi)
     +           *sigma**3)*
     +           dexp(-(x-x0)**2/(2*sigma**2)) + bg
         ELSE
            MB_BG = bg
         END IF
      END IF

      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION GAUSS_EXP_BG(X,npar,val)
c     Convolution between an exponential and a gaussian function
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 GAUSS_EXP_BG, x, derf
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 bg, x0, amp, sigma, tau
      CHARACTER*1 lr
      COMMON /func_exp/ lr


      bg    = val(1)
      x0    = val(2)
      amp   = val(3)
      sigma = val(4)
      tau   = val(5)

c     Exponential part on the left-------------------------------
      IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
         GAUSS_EXP_BG = amp*(dexp((sigma**2 + 2*(x - x0)*tau)/
     +        (2.*tau**2))*(1 + derf((sigma*(-((x - x0)/sigma**2) -
     +        1/tau))/dsqrt(2.d0))))/(2.*tau) + bg
      ELSE IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
c     Exponential part on the right-----------------------------
         GAUSS_EXP_BG = amp*(dexp((sigma**2 - 2*(x - x0)*tau)/
     +        (2.*tau**2))*(1 + derf((sigma*(((x - x0)/sigma**2) -
     +        1/tau))/dsqrt(2.d0))))/(2.*tau)+ bg
c     ----------------------------------------------------------
      END IF

      RETURN
      END


c _______________________________________________________________________________________________


      FUNCTION GAUSS_GAUSS_EXP_BG(X,npar,val)
c     Convolution between an exponential and a gaussian function
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valg(3), valge(5)
      REAL*8 GAUSS_GAUSS_EXP_BG,GAUSS,GAUSS_EXP_BG, x
      REAL*8 bg, x01, x02, amp1, amp2, sigma1, sigma2, tau

      CHARACTER*1 lr
      COMMON /func_exp/ lr


      bg     = val(1)
      x01    = val(2)
      x02    = val(3)
      amp1   = val(4)
      amp2   = val(5)
      sigma1 = val(6)
      sigma2 = val(7)
      tau    = val(8)

      valge(1) = bg
      valge(2) = x01
      valge(3) = amp1
      valge(4) = sigma1
      valge(5) = tau

      valg(1) = x02
      valg(2) = amp2
      valg(3) = sigma2

      GAUSS_GAUSS_EXP_BG = GAUSS_EXP_BG(x,5,valge) + GAUSS(x,3,valg)


      RETURN
      END


c _______________________________________________________________________________________________


      FUNCTION GAUSS_EXP_BG_CONV(X,npar,val)
c     Convolution between an exponential and a gaussian function
c     The value of 'amp' is the value of the surface below the curve
c     TEST FOR NUMERICAL CONVOLUTION (to compare with the previous method)
c
c     Suggested parameters (see Fit_results_Gauss-x-Exp.txt)
c     npoint = 250
c     n of width = 10
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valg(3)
      REAL*8 GAUSS_EXP_BG_CONV, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 bg, x0, amp, sigma, tau
c     Variables for convolution
      INTEGER*4 npoint, nwidth, i
      REAL*8 xc, maxwidth, width, step, rinteg
      REAL*8 f0(npoint)
      CHARACTER*1 lr
      EXTERNAL RINTEG
      COMMON /func_exp/ lr
      COMMON /func_conv/ npoint, nwidth


      bg    = val(1)
      x0    = val(2)
      amp   = val(3)
      sigma = val(4)
      tau   = val(5)

      valg(1) = x0
      valg(2) = amp
      valg(3) = sigma

c     Calculate parameters for convolution,
c     considering that the convolution with the exponential is
c     between 0 and "+ infinity"
      width = dsqrt(sigma**2 + tau**2)
      maxwidth = nwidth*width
      step = maxwidth/(npoint-1)
c     Make the convolution
      IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
c     ASYMMETRY IN THE RIGHT SIDE ----------------------------------
         DO i=1, npoint
            xc =  (i-1)*step
            f0(i) = dexp(-(xc)/tau)/tau*GAUSS(x-xc,3,valg)
         END DO
      ELSE IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
c     ASYMMETRY IN THE LEFT SIDE ---------------------------------
         DO i=1, npoint
            xc =  (i-1)*step - maxwidth
            f0(i) = dexp(xc/tau)/tau*GAUSS(x-xc,3,valg)
         END DO
c     -------------------------------------------------------------
      END IF

      GAUSS_EXP_BG_CONV = RINTEG(f0,1,npoint,14,step) + bg

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION VOIGT(X,npar,val)
c     Voigt profile using the complex error function
c     (from the official one for fortran 2008) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 VOIGT, x
      REAL*8 pi, zr, zi, wr, wi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma, gamma
c     EXTERNAL CW
      LOGICAL flag
      EXTERNAL WOFZ


      x0    = val(1)
      amp   = val(2)
      sigma = val(3)
      gamma = val(4)

      zr = (x-x0)/(sigma*dsqrt(2.d0))
      zi = gamma/(sigma*dsqrt(2.d0))

c      CALL CW(zr,zi,wr,wi)
c      WRITE(*,*) zr,zi
c      PAUSE
       CALL WOFZ(zr,zi,wr,wi,flag)

c      IF(flag) THEN
C         WRITE(*,*) 'Problem with the comp. erf in VOIGT function'
c      END IF


      VOIGT =  amp*wr/(sigma*dsqrt(2.d0*pi))


      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION VOIGT_BG(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valv(4)
      REAL*8 VOIGT_BG, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 bg, x0, amp, sigma, gamma

      bg    = val(1)
      x0    = val(2)
      amp   = val(3)
      sigma = val(4)
      gamma = val(5)

c     Parameter for Voigt profile
      valv(1) = x0
      valv(2) = amp
      valv(3) = sigma
      valv(4) = gamma

      VOIGT_BG = VOIGT(x,4,valv) + bg

      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION VOIGT_EXP(X,npar,val)
c     Voigt profile convoluted with an exponential
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valv(4)
      REAL*8 VOIGT, VOIGT_EXP, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma, gamma, tau
c     Variables for convolution
      INTEGER*4 npoint, nwidth, i
      REAL*8 xc, maxwidth, width, step, rinteg
      REAL*8 f0(npoint)
      CHARACTER*1 lr
      EXTERNAL RINTEG
      COMMON /func_exp/ lr
      COMMON /func_conv/ npoint, nwidth

      x0    = val(1)
      amp   = val(2)
      sigma = val(3)
      gamma = val(4)
      tau   = val(5)

      valv(1) = x0
      valv(2) = amp
      valv(3) = sigma
      valv(4) = gamma


c     Calculate parameters for convolution,
c     considering that the convolution with the exponential is
c     between 0 and "+ infinity"
      width = dsqrt(sigma**2 + tau**2)
      maxwidth = nwidth*width
      step = maxwidth/(npoint-1)
c     Make the convolution
      IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
c     ASYMMETRY IN THE RIGHT SIDE ----------------------------------
         DO i=1, npoint
            xc =  (i-1)*step
            f0(i) = dexp(-(xc)/tau)/tau*VOIGT(x-xc,4,valv)
         END DO
      ELSE IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
c     ASYMMETRY IN THE LEFT SIDE ---------------------------------
         DO i=1, npoint
            xc =  (i-1)*step - maxwidth
            f0(i) = dexp(xc/tau)/tau*VOIGT(x-xc,4,valv)
         END DO
c     -------------------------------------------------------------
      END IF

      VOIGT_EXP = RINTEG(f0,1,npoint,14,step)

      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION VOIGT_EXP_BG(X,npar,val)
c     Voigt profile convoluted with an exponential
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valt(5)
      REAL*8 VOIGT_EXP, VOIGT_EXP_BG, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 bg, x0, amp, sigma, gamma, tau

      bg    = val(1)
      x0    = val(2)
      amp   = val(3)
      sigma = val(4)
      gamma = val(5)
      tau   = val(6)

      valt(1) = x0
      valt(2) = amp
      valt(3) = sigma
      valt(4) = gamma
      valt(5) = tau

      VOIGT_EXP_BG = VOIGT_EXP(x,5,valt) + bg

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION DOUBLE_VOIGT_BG(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4), val2(4)
      REAL*8 DOUBLE_VOIGT_BG, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 bg, x01, x02, amp1, amp2, sigma, gamma

      bg    = val(1)
      x01   = val(2)
      x02   = val(3)
      amp1  = val(4)
      amp2  = val(5)
      sigma = val(6)
      gamma = val(7)

c     fist peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma

c     second peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma

      DOUBLE_VOIGT_BG =  VOIGT(x,4,val1) + VOIGT(x,4,val2) + bg


      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION DOUBLE_VOIGT_EXP_BG(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(5), val2(5)
      REAL*8 DOUBLE_VOIGT_EXP_BG, VOIGT_EXP, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 bg, x01, x02, amp1, amp2, sigma, gamma, tau

      bg    = val(1)
      x01   = val(2)
      x02   = val(3)
      amp1  = val(4)
      amp2  = val(5)
      sigma = val(6)
      gamma = val(7)
      tau   = val(8)

c     fist peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma
      val1(5) = tau

c     second peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma
      val2(5) = tau

      DOUBLE_VOIGT_EXP_BG =  VOIGT_EXP(x,5,val1) +
     +     VOIGT_EXP(x,5,val2) + bg


      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION SIX_VOIGT_BG(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4),val4(4),val5(4),val6(4)
      REAL*8 SIX_VOIGT_BG, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma, gamma, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma  = val(13)
      gamma  = val(14)
      bg     = val(15)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma
      val3(4) = gamma

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma
      val4(4) = gamma

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma
      val5(4) = gamma

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma
      val6(4) = gamma

      SIX_VOIGT_BG = VOIGT(x,4,val1) + VOIGT(x,4,val2) +VOIGT(x,4,val3)
     +     + VOIGT(x,4,val4) +VOIGT(x,4,val5) + VOIGT(x,4,val6) + bg
      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION SIX_GAUSS_SHIRLEYBG(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3),val2(3),val3(3),val4(3),val5(3),val6(3)
      REAL*8 SIX_GAUSS_SHIRLEYBG, GAUSS, SHIRLEY, x
      EXTERNAL SHIRLEY
      REAL*8 pi, dx_sh, x0_sh
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma, bg, bg_sh
      COMMON /aux_shirely/dx_sh, x0_sh

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma  = val(13)
      bg     = val(14)
      bg_sh  = val(15)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma

      SIX_GAUSS_SHIRLEYBG = GAUSS(x,3,val1) + GAUSS(x,3,val2)
     +     + GAUSS(x,3,val3) + GAUSS(x,3,val4)
     +     + GAUSS(x,3,val5) + GAUSS(x,3,val6)
     +     + bg_sh*(SHIRLEY(x)) + bg
!     note:  - bg*x is to take out the integration of a constant bakground

!      write(*,*) x, bg,SHIRLEY(x),bg*((x-x0_sh)/dx_sh+1)
!      pause

      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION SIX_VOIGT_SHIRLEYBG(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4),val4(4),val5(4),val6(4)
      REAL*8 SIX_VOIGT_SHIRLEYBG, VOIGT, SHIRLEY, x
      EXTERNAL SHIRLEY
      REAL*8 pi, dx_sh, x0_sh
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma, gamma, bg, bg_sh
      COMMON /aux_shirely/dx_sh, x0_sh
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma  = val(13)
      gamma  = val(14)
      bg     = val(15)
      bg_sh  = val(16)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma
      val3(4) = gamma

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma
      val4(4) = gamma

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma
      val5(4) = gamma

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma
      val6(4) = gamma

      SIX_VOIGT_SHIRLEYBG = VOIGT(x,4,val1) + VOIGT(x,4,val2)
     +     + VOIGT(x,4,val3) + VOIGT(x,4,val4)
     +     + VOIGT(x,4,val5) + VOIGT(x,4,val6)
     +     + bg_sh*(SHIRLEY(x)) + bg
c     note:  - bg*x is to take out the integration of a constant bakground


c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, SIX_VOIGT_SHIRLEYBG,
     +        VOIGT(x,4,val1), VOIGT(x,4,val2),
     +        VOIGT(x,4,val3),VOIGT(x,4,val4),
     +        VOIGT(x,4,val5), VOIGT(x,4,val6),
     +        bg, bg_sh*(SHIRLEY(x))
      ENDIF

      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION SIX_VOIGT_POLYBG(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4),val4(4),val5(4),val6(4)
      REAL*8 valp(8)
      REAL*8 SIX_VOIGT_POLYBG, VOIGT, POLY,x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma, gamma
      REAL*8 bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma  = val(13)
      gamma  = val(14)
      bg     = val(15)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma
      val3(4) = gamma

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma
      val4(4) = gamma

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma
      val5(4) = gamma

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma
      val6(4) = gamma

c     polynomial background from alternative fit
c     present values from r0125s0023.1D.dat
      valp(1) = 0.
      valp(2) = 41.400364203825816
      valp(3) = -3.92691778285953197E-002
      valp(4) = 1.29548754501671265E-004
      valp(5) = -1.24480338048226108E-007
      valp(6) = 0.
      valp(7) = 0.
      valp(8) = 0.

      SIX_VOIGT_POLYBG = VOIGT(x,4,val1) + VOIGT(x,4,val2)
     +     + VOIGT(x,4,val3)
     +     + VOIGT(x,4,val4) +VOIGT(x,4,val5) + VOIGT(x,4,val6)
     +     + bg*POLY(x,8,valp)
      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION SIX_VOIGT_XRD(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     adapted for XRD data, characterized by doublet (1 and 2 of CuKa)
c     that should have the same intensity ratio and distance
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4),val4(4),val5(4),val6(4)
      REAL*8 valp(8)
      REAL*8 SIX_VOIGT_XRD, VOIGT,x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x03, x05
      REAL*8 amp1, amp3, amp5
      REAL*8 dx0, damp, sigma, gamma
      REAL*8 bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x03    = val(2)
      x05    = val(3)
      amp1   = val(4)
      amp3   = val(5)
      amp5   = val(6)
      dx0    = val(7)
      damp   = val(8)
      sigma  = val(9)
      gamma  = val(10)
      bg     = val(11)


c     first voigt peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma

c     second voigt peak (twin of peak 1)
      val2(1) = x01 + dx0
      val2(2) = amp1*damp
      val2(3) = sigma
      val2(4) = gamma

c     third voigt peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma
      val3(4) = gamma

c     fourth voigt peak( twin of peak 3)
      val4(1) = x03 + dx0
      val4(2) = amp3*damp
      val4(3) = sigma
      val4(4) = gamma

c     fifth voigt peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma
      val5(4) = gamma

c     sixth voigt peak ( twin of peak 5)
      val6(1) = x05 + dx0
      val6(2) = amp5*damp
      val6(3) = sigma
      val6(4) = gamma

      SIX_VOIGT_XRD = VOIGT(x,4,val1) + VOIGT(x,4,val2)
     +     + VOIGT(x,4,val3)
     +     + VOIGT(x,4,val4) +VOIGT(x,4,val5) + VOIGT(x,4,val6)
     +     + bg
      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION SIX_VOIGT_POLYBG_WF(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4),val4(4),val5(4),val6(4)
      REAL*8 valp(8)
      REAL*8 SIX_VOIGT_POLYBG_WF, VOIGT, POLY,x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma1, sigma2, sigma3, sigma4, sigma5, sigma6, gamma
      REAL*8 bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma1 = val(13)
      sigma2 = val(14)
      sigma3 = val(15)
      sigma4 = val(16)
      sigma5 = val(17)
      sigma6 = val(18)
      gamma  = val(19)
      bg     = val(20)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1
      val1(4) = gamma

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma2
      val2(4) = gamma

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma3
      val3(4) = gamma

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma4
      val4(4) = gamma

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma5
      val5(4) = gamma

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma6
      val6(4) = gamma

c     polynomial background from alternative fit
c     present values from r0125s0023.1D.dat
      valp(1) = 0.
      valp(2) = 41.400364203825816
      valp(3) = -3.92691778285953197E-002
      valp(4) = 1.29548754501671265E-004
      valp(5) = -1.24480338048226108E-007
      valp(6) = 0.
      valp(7) = 0.
      valp(8) = 0.

      SIX_VOIGT_POLYBG_WF = VOIGT(x,4,val1) + VOIGT(x,4,val2)
     +     + VOIGT(x,4,val3)
     +     + VOIGT(x,4,val4) +VOIGT(x,4,val5) + VOIGT(x,4,val6)
     +     + bg*POLY(x,8,valp)
      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION SIX_VOIGT_FREEGAMMA_BG(X,npar,val)
c     Voigt profile with 6 different gamma using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
c     Added by Anna Levy 2017
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4),val4(4),val5(4),val6(4)
      REAL*8 SIX_VOIGT_FREEGAMMA_BG, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma, gamma1,gamma2, gamma3, gamma4, gamma5, gamma6, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma  = val(13)
      gamma1  = val(14)
      gamma2  = val(15)
      gamma3  = val(16)
      gamma4  = val(17)
      gamma5  = val(18)
      gamma6  = val(19)
      bg     = val(20)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma1

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma2

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma
      val3(4) = gamma3

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma
      val4(4) = gamma4

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma
      val5(4) = gamma5

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma
      val6(4) = gamma6

      SIX_VOIGT_FREEGAMMA_BG = VOIGT(x,4,val1) + VOIGT(x,4,val2)
     +     +VOIGT(x,4,val3)+ VOIGT(x,4,val4)
     +     + VOIGT(x,4,val5) + VOIGT(x,4,val6) + bg
      RETURN
      END

! ------------------------------------------------------------------------
!
      FUNCTION FOUR_VOIGT_BG(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
c     Added by Anna Levy 2017
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4),val4(4)
      REAL*8 FOUR_VOIGT_BG, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04
      REAL*8 amp1, amp2, amp3, amp4
      REAL*8 sigma, gamma1, gamma2, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      amp1   = val(5)
      amp2   = val(6)
      amp3   = val(7)
      amp4   = val(8)
      sigma  = val(9)
      gamma1 = val(10)
      gamma2 = val(11)
      bg     = val(12)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma1

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma1

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma
      val3(4) = gamma2

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma
      val4(4) = gamma2

      FOUR_VOIGT_BG = VOIGT(x,4,val1) + VOIGT(x,4,val2) +VOIGT(x,4,val3)
     +     + VOIGT(x,4,val4) + bg
      RETURN
      END

c _________________________________________________________________________________________________

      FUNCTION SIX_VOIGT_EXP_BG(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(5),val2(5),val3(5),val4(5),val5(5),val6(5)
      REAL*8 SIX_VOIGT_EXP_BG, VOIGT_EXP, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma, gamma, bg, tau
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma  = val(13)
      gamma  = val(14)
      tau    = val(15)
      bg     = val(16)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma
      val1(5) = tau

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma
      val2(5) = tau

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma
      val3(4) = gamma
      val3(5) = tau

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma
      val4(4) = gamma
      val4(5) = tau

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma
      val5(4) = gamma
      val5(5) = tau

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma
      val6(4) = gamma
      val6(5) = tau

      SIX_VOIGT_EXP_BG = VOIGT_EXP(x,5,val1) + VOIGT_EXP(x,5,val2)
     +     +VOIGT_EXP(x,5,val3)+ VOIGT_EXP(x,5,val4)
     +     + VOIGT_EXP(x,5,val5) + VOIGT_EXP(x,5,val6) + bg
      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION SIX_VOIGT_EXP_POLYBG(X,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(5),val2(5),val3(5),val4(5),val5(5),val6(5)
      REAL*8 valp(8)
      REAL*8 SIX_VOIGT_EXP_POLYBG, VOIGT_EXP, POLY, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, x02, x03, x04, x05, x06
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6
      REAL*8 sigma, gamma, tau
      REAL*8 bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01    = val(1)
      x02    = val(2)
      x03    = val(3)
      x04    = val(4)
      x05    = val(5)
      x06    = val(6)
      amp1   = val(7)
      amp2   = val(8)
      amp3   = val(9)
      amp4   = val(10)
      amp5   = val(11)
      amp6   = val(12)
      sigma  = val(13)
      gamma  = val(14)
      tau    = val(15)
      bg     = val(16)


c     "neutral"  voigt peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma
      val1(5) = tau

c     second gauss peak
      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma
      val2(5) = tau

c     third gauss peak
      val3(1) = x03
      val3(2) = amp3
      val3(3) = sigma
      val3(4) = gamma
      val3(5) = tau

c     fourth gauss peak
      val4(1) = x04
      val4(2) = amp4
      val4(3) = sigma
      val4(4) = gamma
      val4(5) = tau

c     fifth gauss peak
      val5(1) = x05
      val5(2) = amp5
      val5(3) = sigma
      val5(4) = gamma
      val5(5) = tau

c     sixth gauss peak
      val6(1) = x06
      val6(2) = amp6
      val6(3) = sigma
      val6(4) = gamma
      val6(5) = tau

c     polynomial background from alternative fit
c     present values from r0125s0023.1D.dat
      valp(1) = 0.
      valp(2) = 41.400364203825816
      valp(3) = -3.92691778285953197E-002
      valp(4) = 1.29548754501671265E-004
      valp(5) = -1.24480338048226108E-007
      valp(6) = 0.
      valp(7) = 0.
      valp(8) = 0.

      SIX_VOIGT_EXP_POLYBG = VOIGT_EXP(x,5,val1) + VOIGT_EXP(x,5,val2)
     +     +VOIGT_EXP(x,5,val3)+ VOIGT_EXP(x,5,val4)
     +     + VOIGT_EXP(x,5,val5) + VOIGT_EXP(x,5,val6)
     +     + bg*POLY(x,8,valp)
      RETURN
      END
c _______________________________________________________________________________________________


      FUNCTION WEIBULL(X,npar,val)
c     Normalized Weibull function
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 WEIBULL, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, lambda, kappa
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x0      = val(1)
      amp     = val(2)
      lambda  = val(3)
      kappa   = val(4)
      IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
c    Asymmetry in the left side
         x0    = val(1)
      ELSE IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
c    Asymmetry in the right side
         x0    = val(1)
      END IF
c     Comment: max at lambda*((kappa-1)/kappa)**(1/kappa)
       IF ((x-x0).LT.0) THEN
            WEIBULL =amp*kappa/lambda*((x-x0)/lambda)**(kappa-1)*
     +           dexp(-((x-x0)/lambda)**kappa)
         ELSE
            WEIBULL = 0.d0
         END IF
      IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
c    Asymmetry in the left side

      ELSE IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
c    Asymmetry in the right side
         IF ((x-x0).GT.0) THEN
            WEIBULL =amp*kappa/lambda*((x-x0)/lambda)**(kappa-1)*
     +           dexp(-((x-x0)/lambda)**kappa)
         ELSE
            WEIBULL = 0.d0
         END IF
      END IF

      RETURN
      END


c     _______________________________________________________________________________________________

      FUNCTION WEIBULL_BG(X,npar,val)
c     Normalized Weibull function with flat background
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valw(4)
      REAL*8 WEIBULL,WEIBULL_BG, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, lambda, kappa,bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x0      = val(1)
      amp     = val(2)
      lambda  = val(3)
      kappa   = val(4)
      bg      = val(5)

c     Parameters for the Weibull distribution
      valw(1) = x0
      valw(2) = amp
      valw(3) = lambda
      valw(4) = kappa

      WEIBULL_BG = WEIBULL(X,4,valw) + bg

      RETURN
      END
c     _______________________________________________________________________________________________

      FUNCTION WEIBULL_ERFBG(X,npar,val)
c     Normalized Weibull function with error function background
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valw(8)
      REAL*8 WEIBULL,WEIBULL_ERFBG, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, lambda, kappa, dx0_erf, amp_erf, dsigma_erf, bg
      REAL*8 mean_wb, sigma_wb, x0_erf, sigma_erf
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x0         = val(1)
      amp        = val(2)
      lambda     = val(3)
      kappa      = val(4)
      dx0_erf    = val(5)
      amp_erf    = val(6)
      dsigma_erf = val(7)
      bg         = val(8)

c     Parameters for the Weibull distribution
      valw(1) = x0
      valw(2) = amp
      valw(3) = lambda
      valw(4) = kappa
c     Mean and standard deviation of Weibull distribution (Wikipedia or other books)
c     Gamma function is written as function of its logarithm
      mean_wb = lambda*(GAMMA(1+1/kappa))
      !sigma_wb = dsqrt(lambda**2*(dexp(dgamln(1+2/kappa)))-mean_wb**2)
      sigma_wb = dsqrt(lambda**2*(GAMMA(1+2/kappa))-mean_wb**2) ! Martino's change 07/01/2020

c     Calculation of the "erf" background as cumulative function of Gaussian with
      x0_erf = mean_wb + dx0_erf
      sigma_erf = dsigma_erf*sigma_wb

      WEIBULL_ERFBG = WEIBULL(X,4,valw) +
     +     amp_erf*1/2*(1+derf((x-x0_erf)/(sigma_erf*dsqrt(2.d0))))+ bg

      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION LASER(X,npar,val)
c     X-ray emission function funtion for laser-cluster interaction studies
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 LASER, x
      REAL*8 I0, amp
      REAL*8 beta, LASER1, LASER2, LASER3

      amp     = val(1)
      I0      = val(2)


      IF(x.GT.I0) THEN
         beta = dsqrt(x/I0-1)
!LASER = amp*(4/3*beta-4/3*datan(beta)+2/9*beta**3)
         LASER1 = amp*4/3*beta
         LASER2 = -amp*4/3*datan(beta)
         LASER3 = amp*2/9*beta**3
         LASER = LASER1 + LASER2 + LASER3
      ELSE IF(x.LT.I0) THEN
         LASER = 0.0
      END IF


      RETURN
      END
c _______________________________________________________________________________________________


      FUNCTION ROCKING_CURVE(X,npar,val)
c     Rocking curve with s and p polarization extracted from simulations
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 ROCKING_CURVE, x, y_s, y_p
      REAL*8 amp,damp_sp, x0_s, dx0_sp, bg
c     Interpolation variables
      INTEGER*4 k, nn_s, nn_p, ier_s,ier_p, nest
      PARAMETER (nest=1000)
      REAL*8 t_s(nest), c_s(nest), t_p(nest), c_p(nest)
      COMMON /rocking/ t_s, t_p, c_s, c_p, k, nn_s, nn_p
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot


      amp     = val(1)
      damp_sp = val(2)
      x0_s    = val(3)
      dx0_sp   = val(4)
      bg      = val(5)
c
c     s polarization contribution
      CALL SPLEV(t_s,nn_s,c_s,k,x-x0_s,y_s,1,1,ier_s)


c     p polarization contribution
      CALL SPLEV(t_p,nn_p,c_p,k,x-(x0_s+dx0_sp),y_p,1,1,ier_p)

      ROCKING_CURVE = amp*((1-damp_sp)*y_s+damp_sp*y_p) + bg
c

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, ROCKING_CURVE, amp*(1-damp_sp)*y_s,
     +        amp*damp_sp*y_p, bg
      ENDIF


      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION TWO_INTERP_VOIGT_POLY(X,npar,val)
c     Rocking curve with s and p polarization extracted from simulations
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valv(4), valp(8)
      REAL*8 TWO_INTERP_VOIGT_POLY, VOIGT, POLY, x, y_1, y_2
      REAL*8 amp1, amp2, amp3, x01, x02, x03
      REAL*8 sigma, gamma, a, b, c, d, e
c     Interpolation variables
      INTEGER*4 k, nn_1, nn_2, ier_1,ier_2, nest
      PARAMETER (nest=1000)
      REAL*8 t_1(nest), c_1(nest), t_2(nest), c_2(nest)
      COMMON /two_interp/ t_1, t_2, c_1, c_2, k, nn_1, nn_2
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      amp1  = val(1)
      amp2  = val(2)
      amp3  = val(3)
      x01   = val(4)
      x02   = val(5)
      x03   = val(6)
      sigma = val(7)
      gamma = val(8)
      a     = val(9)
      b     = val(10)
      c     = val(11)
      d     = val(12)
      e     = val(13)


c     Voigt peak
      valv(1) = x03
      valv(2) = amp3
      valv(3) = sigma
      valv(4) = gamma

c     Polynomial background
      valp(1) = 0.
      valp(2) = a
      valp(3) = b
      valp(4) = c
      valp(5) = d
      valp(6) = e
      valp(7) = 0.
      valp(8) = 0.


c     First peak
      CALL SPLEV(t_1,nn_1,c_1,k,x-x01,y_1,1,1,ier_1)

c     Second peak
      CALL SPLEV(t_2,nn_2,c_2,k,x-x02,y_2,1,1,ier_2)

      TWO_INTERP_VOIGT_POLY = amp1*y_1 + amp2*y_2 + VOIGT(x,4,valv) +
     +     POLY(x,8,valp)


c     Save different components
      IF(plot) THEN
         WRITE(40,*) x, TWO_INTERP_VOIGT_POLY, amp1*y_1, amp2*y_2,
     +        VOIGT(x,4,valv), POLY(x,8,valp)

      ENDIF

      RETURN
      END
c _______________________________________________________________________________________________


      FUNCTION TWO_INTERP_VOIGT_POLY_X0(X,npar,val)
c     Rocking curve with s and p polarization extracted from simulations
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valv(4), valp(8)
      REAL*8 TWO_INTERP_VOIGT_POLY_X0, VOIGT, POLY, x, y_1, y_2
      REAL*8 amp1, amp2, amp3, x01, x02, x03
      REAL*8 sigma, gamma, a, b, c, d, e, x0
c     Interpolation variables
      INTEGER*4 k, nn_1, nn_2, ier_1,ier_2, nest
      PARAMETER (nest=1000)
      REAL*8 t_1(nest), c_1(nest), t_2(nest), c_2(nest)
      COMMON /two_interp/ t_1, t_2, c_1, c_2, k, nn_1, nn_2
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      amp1  = val(1)
      amp2  = val(2)
      amp3  = val(3)
      x01   = val(4)
      x02   = val(5)
      x03   = val(6)
      sigma = val(7)
      gamma = val(8)
      gamma = val(8)
      x0    = val(9)
      a     = val(10)
      b     = val(11)
      c     = val(12)
      d     = val(13)
      e     = val(14)


c     Voigt peak
      valv(1) = x03
      valv(2) = amp3
      valv(3) = sigma
      valv(4) = gamma

c     Polynomial background
      valp(1) = x0
      valp(2) = a
      valp(3) = b
      valp(4) = c
      valp(5) = d
      valp(6) = e
      valp(7) = 0.
      valp(8) = 0.


c     First peak
      CALL SPLEV(t_1,nn_1,c_1,k,x-x01,y_1,1,1,ier_1)

c     Second peak
      CALL SPLEV(t_2,nn_2,c_2,k,x-x02,y_2,1,1,ier_2)

      TWO_INTERP_VOIGT_POLY_X0 = amp1*y_1 + amp2*y_2 + VOIGT(x,4,valv) +
     +     POLY(x,8,valp)


c     Save different components
      IF(plot) THEN
         WRITE(40,*) x, TWO_INTERP_VOIGT_POLY_X0, amp1*y_1, amp2*y_2,
     +        VOIGT(x,4,valv), POLY(x,8,valp)

      ENDIF

      RETURN
      END


c _______________________________________________________________________________________________


            FUNCTION THREE_INTERP_VOIGT_POLY(X,npar,val)
c     Rocking curve with s and p polarization extracted from simulations
            IMPLICIT NONE
            INTEGER*4 npar
            REAL*8 val(npar), valp(8)
            REAL*8 THREE_INTERP_VOIGT_POLY, VOIGT, POLY,x,y_1,y_2,y_3
            REAL*8 amp1, amp2, amp3, x01, x02, x03
            REAL*8 a, b, c, d, e
c     Interpolation variables
            INTEGER*4 k,nn_1,nn_2,nn_3,ier_1,ier_2,ier_3,nest
            PARAMETER (nest=1000)
            REAL*8 t_1(nest),c_1(nest),t_2(nest),c_2(nest)
            REAL*8 t_3(nest),c_3(nest)
            COMMON /three_interp/
     +           t_1,t_2,t_3,c_1,c_2,c_3,k,nn_1,nn_2,nn_3
c           To plot the different components
            LOGICAL plot
            COMMON /func_plot/ plot

            amp1  = val(1)
            amp2  = val(2)
            amp3  = val(3)
            x01   = val(4)
            x02   = val(5)
            x03   = val(6)
            a     = val(7)
            b     = val(8)
            c     = val(9)
            d     = val(10)
            e     = val(11)


c     Polynomial background
            valp(1) = 0.
            valp(2) = a
            valp(3) = b
            valp(4) = c
            valp(5) = d
            valp(6) = e
            valp(7) = 0.
            valp(8) = 0.


c     First peak
            CALL SPLEV(t_1,nn_1,c_1,k,x-x01,y_1,1,1,ier_1)

c     Second peak
            CALL SPLEV(t_2,nn_2,c_2,k,x-x02,y_2,1,1,ier_2)

c     Third peak
            CALL SPLEV(t_3,nn_3,c_3,k,x-x03,y_3,1,1,ier_3)

            THREE_INTERP_VOIGT_POLY = amp1*y_1 + amp2*y_2 + amp3*y_3 +
     +           POLY(x,8,valp)


c     Save different components
            IF(plot) THEN
               WRITE(40,*) x, THREE_INTERP_VOIGT_POLY,
     +              amp1*y_1, amp2*y_2, amp3*y_3, POLY(x,8,valp)

            ENDIF

            RETURN
            END



c _______________________________________________________________________________________________


      FUNCTION THRESHOLD(X,npar,val)
c     Threshold funtion for laser-cluster interaction studies
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 THRESHOLD, x
      REAL*8 tau, alpha, I0

      tau     = val(1)
      alpha   = val(2)
      I0      = val(3)


      IF(x.GT.tau) THEN
         THRESHOLD = I0
      ELSE IF(x.LT.tau) THEN
         THRESHOLD = I0*(x/tau)**(-alpha)
      END IF

      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION GAUSS_ERF(X,npar,val)
c     Normalized Gaussian distribution + "Shirley" background (i.e. error function in this case)
c     proportional to the integral. The direction of the background is selected by the input file
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valg(3), vale(3)
      REAL*8 GAUSS_ERF, GAUSS, ERFFCN, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma, damp

      x0    = val(1)
      amp   = val(2)
      sigma = val(3)
      damp  = val(4)

      valg(1) = x0
      valg(2) = amp
      valg(3) = sigma

      vale(1) = x0
      vale(2) = amp*damp
      vale(3) = sigma


      GAUSS_ERF = GAUSS(x,3,valg) + ERFFCN(x,3,vale)


      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION VOIGT_ERF(X,npar,val)
c     Normalized voigt distribution + error function as "Shirley" background
c     with the characteristic width approximated to the half of the FWHM of the Voigt
c     The value of 'amp' is the value of the surface below the voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valv(4), vale(3)
      REAL*8 VOIGT_ERF, VOIGT, ERFFCN, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma, gamma, damp, fwhmg, fwhml

      x0    = val(1)
      amp   = val(2)
      sigma = val(3)
      gamma = val(4)
      damp  = val(5)

      valv(1) = x0
      valv(2) = amp
      valv(3) = sigma
      valv(4) = gamma

      fwhmg = 2*sigma*dsqrt(2*dlog(2.d0))
      fwhml = 2*gamma

      vale(1) = x0
      vale(2) = amp*damp
      vale(3) = (0.5346*fwhml+dsqrt(0.2166*fwhml**2+fwhmg**2))/2


      VOIGT_ERF = VOIGT(x,4,valv) + ERFFCN(x,3,vale)


      RETURN
      END
c _______________________________________________________________________________________________


      FUNCTION TWO_GAUSS_ERF_EXPBG(X,npar,val)
c     Normalized Gaussian distribution
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4), val2(4)
      REAL*8 TWO_GAUSS_ERF_EXPBG, GAUSS_ERF, x
      REAL*8 x01, x02, dx02, amp1, amp2, damp2, sigma, damperf
      REAL*8 bg_amp, bg_tau, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x01      = val(1)
      dx02     = val(2)
      amp1     = val(3)
      damp2    = val(4)
      sigma    = val(5)
      damperf  = val(6)
      bg_amp   = val(7)
      bg_tau   = val(8)


      x02 = x01 + dx02
      amp2 = amp1*damp2

      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = damperf


      val2(1) = x02
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = damperf

c     exponential background
      IF(lr.EQ.'r'.OR.lr.EQ.'R') THEN
         bg = bg_amp*dexp(-x/bg_tau)
      ELSE IF(lr.EQ.'l'.OR.lr.EQ.'L') THEN
         bg = bg_amp*dexp(+x/bg_tau)
      END IF

      TWO_GAUSS_ERF_EXPBG = GAUSS_ERF(x,3,val1) +
     +     GAUSS_ERF(x,3,val2) + bg
c+ bg


      RETURN
      END


c     _______________________________________________________________________________________________



      FUNCTION TWO_DOUBL_GAUSS_ERF_POLY(X,npar,val)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val11(4), val12(4), val21(4), val22(4)
      REAL*8 valp(8)
      REAL*8 TWO_DOUBL_GAUSS_ERF_POLY, GAUSS_ERF, POLY, x
      REAL*8 x11, dx10, dx01
      REAL*8 amp11, damp01, damp10, sigma, damperf
      REAL*8 a, b, c, d, e
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x11     = val(1)
      dx01    = val(2)
      dx10    = val(3)
      amp11   = val(4)
      damp01  = val(5)
      damp10  = val(6)
      sigma   = val(7)
      damperf = val(8)
      a       = val(9)
      b       = val(10)
      c       = val(11)
      d       = val(12)
      e       = val(13)



c     First peak of first doublet
      val11(1) = x11
      val11(2) = amp11
      val11(3) = sigma
      val11(4) = damperf

c     Second peak of first doublet
      val12(1) = x11 + dx01
      val12(2) = amp11*damp01
      val12(3) = sigma
      val12(4) = damperf

c     First peak of second doublet
      val21(1) = x11 + dx10
      val21(2) = amp11*damp10
      val21(3) = sigma
      val21(4) = damperf

c     Second peak of second doublet
      val22(1) = x11 + dx10 + dx01
      val22(2) = amp11*damp10*damp01
      val22(3) = sigma
      val22(4) = damperf

c     Polynomial background
      valp(1) = 0.
      valp(2) = a
      valp(3) = b
      valp(4) = c
      valp(5) = d
      valp(6) = e
      valp(7) = 0.
      valp(8) = 0.

      TWO_DOUBL_GAUSS_ERF_POLY = GAUSS_ERF(x,4,val11) +
     +     GAUSS_ERF(x,4,val12) + GAUSS_ERF(x,4,val21) +
     +     GAUSS_ERF(x,4,val22) + POLY(x,8,valp)


      RETURN
      END

c     _______________________________________________________________________________________________

      FUNCTION TWO_DOUBL_GAUSS_ERF_FREESIG_POLY(X,npar,val)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val11(4), val12(4), val21(4), val22(4)
      REAL*8 valp(8)
      REAL*8 TWO_DOUBL_GAUSS_ERF_FREESIG_POLY, GAUSS_ERF, POLY, x
      REAL*8 x11, dx10, dx01
      REAL*8 amp11, damp01, damp10, sigma1, sigma2, damperf
      REAL*8 a, b, c, d, e
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x11     = val(1)
      dx01    = val(2)
      dx10    = val(3)
      amp11   = val(4)
      damp01  = val(5)
      damp10  = val(6)
      sigma1  = val(7)
      sigma2  = val(8)
      damperf = val(9)
      a       = val(10)
      b       = val(11)
      c       = val(12)
      d       = val(13)
      e       = val(14)



c     First peak of first doublet
      val11(1) = x11
      val11(2) = amp11
      val11(3) = sigma1
      val11(4) = damperf

c     Second peak of first doublet
      val12(1) = x11 + dx01
      val12(2) = amp11*damp01
      val12(3) = sigma1
      val12(4) = damperf

c     First peak of second doublet
      val21(1) = x11 + dx10
      val21(2) = amp11*damp10
      val21(3) = sigma2
      val21(4) = damperf

c     Second peak of second doublet
      val22(1) = x11 + dx10 + dx01
      val22(2) = amp11*damp10*damp01
      val22(3) = sigma2
      val22(4) = damperf

c     Polynomial background
      valp(1) = 0.
      valp(2) = a
      valp(3) = b
      valp(4) = c
      valp(5) = d
      valp(6) = e
      valp(7) = 0.
      valp(8) = 0.

      TWO_DOUBL_GAUSS_ERF_FREESIG_POLY = GAUSS_ERF(x,4,val11) +
     +     GAUSS_ERF(x,4,val12) + GAUSS_ERF(x,4,val21) +
     +     GAUSS_ERF(x,4,val22) + POLY(x,8,valp)


      RETURN
      END

c     _______________________________________________________________________________________________

      FUNCTION SIX_GAUSS_ERF_FREESIG_POLY(X,npar,val)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar),val1(4),val2(4),val3(4),val4(4),val5(4),val6(4)
      REAL*8 valp(8)
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY, GAUSS_ERF, POLY, x
      REAL*8 x1, dx2, dx3, dx4, dx5, dx6
      REAL*8 amp1, amp2, amp3, amp4, amp5, amp6, damperf
      REAL*8 sigma1, dsigma2, dsigma3, dsigma4, dsigma5, dsigma6
      REAL*8 a, b, c, d, e
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot


      x1       = val(1)
      dx2      = val(2)
      dx3      = val(3)
      dx4      = val(4)
      dx5      = val(5)
      dx6      = val(6)
      amp1     = val(7)
      amp2     = val(8)
      amp3     = val(9)
      amp4     = val(10)
      amp5     = val(11)
      amp6     = val(12)
      sigma1   = val(13)
      dsigma2  = val(14)
      dsigma3  = val(15)
      dsigma4  = val(16)
      dsigma5  = val(17)
      dsigma6  = val(18)
      damperf  = val(19)
      a        = val(20)
      b        = val(21)
      c        = val(22)
      d        = val(23)
      e        = val(24)



c     First (main) peak
      val1(1) = x1
      val1(2) = amp1
      val1(3) = sigma1
      val1(4) = damperf

c     Second peak
      val2(1) = x1 + dx2
      val2(2) = amp2
      val2(3) = sigma1*dsigma2
      val2(4) = damperf

c     Third peak
      val3(1) = x1 + dx3
      val3(2) = amp3
      val3(3) = sigma1*dsigma3
      val3(4) = damperf

c     Fourth peak
      val4(1) = x1 + dx4
      val4(2) = amp4
      val4(3) = sigma1*dsigma4
      val4(4) = damperf

c     Fifth peak
      val5(1) = x1 + dx5
      val5(2) = amp5
      val5(3) = sigma1*dsigma5
      val5(4) = damperf

c     Sixth peak
      val6(1) = x1 + dx6
      val6(2) = amp6
      val6(3) = sigma1*dsigma6
      val6(4) = damperf

c     Polynomial background
      valp(1) = 0.
      valp(2) = a
      valp(3) = b
      valp(4) = c
      valp(5) = d
      valp(6) = e
      valp(7) = 0.
      valp(8) = 0.

      SIX_GAUSS_ERF_FREESIG_POLY = GAUSS_ERF(x,4,val1) +
     +     GAUSS_ERF(x,4,val2) + GAUSS_ERF(x,4,val3) +
     +     GAUSS_ERF(x,4,val4) + GAUSS_ERF(x,4,val5) +
     +     GAUSS_ERF(x,4,val6) + POLY(x,8,valp)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, GAUSS_ERF(x,4,val1),
     +     GAUSS_ERF(x,4,val2), GAUSS_ERF(x,4,val3),
     +     GAUSS_ERF(x,4,val4), GAUSS_ERF(x,4,val5),
     +     GAUSS_ERF(x,4,val6), POLY(x,8,valp)
      ENDIF

      RETURN
      END

c     _______________________________________________________________________________________________


      FUNCTION SIX_GAUSS_ERF_FREESIG_POLY2(x,npar,val)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY2, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(4),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 x1_1, dx2, dx3, dx4, dx5, dx6
      REAL*8 amp1_1, amp2_1, amp3_1, amp4_1, damp45, amp6_1, damperf_1
      REAL*8 sigma1_1
      REAL*8 sigma4_1
      REAL*8 dsigma21, dsigma31, dsigma54, dsigma61
      REAL*8 a_1, b_1, c_1
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1_1       = val(1)
      dx2        = val(2)
      dx3        = val(3)
      dx4        = val(4)
      dx5        = val(5)
      dx6        = val(6)
      amp1_1     = val(7)
      amp2_1     = val(8)
      amp3_1     = val(9)
      amp4_1     = val(10)
      damp45     = val(11)
      amp6_1     = val(12)
      sigma1_1   = val(13)
      dsigma21   = val(14)
      dsigma31   = val(15)
      sigma4_1   = val(16)
      dsigma54   = val(17)
      dsigma61   = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      b_1        = val(21)
      c_1        = val(22)

c     FIRST SPECTRUM

c     First (main) peak
      val1_1(1) = x1_1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1_1
      val1_1(4) = damperf_1

c     Second peak
      val2_1(1) = x1_1 + dx2
      val2_1(2) = amp2_1
      val2_1(3) = sigma1_1*dsigma21
      val2_1(4) = damperf_1

c     Third peak
      val3_1(1) = x1_1 + dx3
      val3_1(2) = amp3_1
      val3_1(3) = sigma1_1*dsigma31
      val3_1(4) = damperf_1

c     Fourth peak
      val4_1(1) = x1_1 + dx4
      val4_1(2) = amp4_1
      val4_1(3) = sigma4_1
      val4_1(4) = damperf_1

c     Fifth peak
      val5_1(1) = x1_1 + dx5
      val5_1(2) = amp4_1*damp45
      val5_1(3) = sigma4_1*dsigma54
      val5_1(4) = damperf_1

c     Sixth peak
      val6_1(1) = x1_1 + dx6
      val6_1(2) = amp6_1
      val6_1(3) = sigma1_1*dsigma61
      val6_1(4) = damperf_1

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

c     FIRST SPECTRUM

c     First (main) peak
      val1_1g(1) = x1_1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1_1
      val1_1e(1) = x1_1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1_1

c     Second peak
      val2_1g(1) = x1_1 + dx2
      val2_1g(2) = amp2_1
      val2_1g(3) = sigma1_1*dsigma21
      val2_1e(1) = x1_1 + dx2
      val2_1e(2) = amp2_1*damperf_1
      val2_1e(3) = sigma1_1*dsigma21

c     Third peak
      val3_1g(1) = x1_1 + dx3
      val3_1g(2) = amp3_1
      val3_1g(3) = sigma1_1*dsigma31
      val3_1e(1) = x1_1 + dx3
      val3_1e(2) = amp3_1*damperf_1
      val3_1e(3) = sigma1_1*dsigma31

c     Fourth peak
      val4_1g(1) = x1_1 + dx4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma4_1
      val4_1e(1) = x1_1 + dx4
      val4_1e(2) = amp4_1*damperf_1
      val4_1e(3) = sigma4_1

c     Fifth peak
      val5_1g(1) = x1_1 + dx5
      val5_1g(2) = amp4_1*damp45
      val5_1g(3) = sigma4_1*dsigma54
      val5_1e(1) = x1_1 + dx5
      val5_1e(2) = amp4_1*damp45*damperf_1
      val5_1e(3) = sigma4_1*dsigma54

c     Sixth peak
      val6_1g(1) = x1_1 + dx6
      val6_1g(2) = amp6_1
      val6_1g(3) = sigma1_1*dsigma61
      val6_1e(1) = x1_1 + dx6
      val6_1e(2) = amp6_1*damperf_1
      val6_1e(3) = sigma1_1*dsigma61

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = b_1
      valp_1(4) = c_1
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.


	  SIX_GAUSS_ERF_FREESIG_POLY2 = GAUSS_ERF(x,4,val1_1) +
     +        GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +        GAUSS_ERF(x,4,val4_1) + GAUSS_ERF(x,4,val5_1) +
     +        GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)

c     Save the different components
      IF(plot) THEN
      WRITE(40,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val4_1e),
     +			 ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
      ENDIF

	  RETURN
	  END




c     _______________________________________________________________________________________________



      FUNCTION TWO_DOUBL_VOIGT_ERF_POLY(X,npar,val)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val11(5), val12(5), val21(5), val22(5)
      REAL*8 valp(8)
      REAL*8 TWO_DOUBL_VOIGT_ERF_POLY, VOIGT_ERF, POLY, x
      REAL*8 x11, dx10, dx01
      REAL*8 amp11, damp01, damp10, sigma
      REAL*8 gamma1, gamma2, damperf
      REAL*8 a, b, c, d, e
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x11     = val(1)
      dx01    = val(2)
      dx10    = val(3)
      amp11   = val(4)
      damp01  = val(5)
      damp10  = val(6)
      sigma   = val(7)
      gamma1  = val(8)
      gamma2  = val(9)
      damperf = val(10)
      a       = val(11)
      b       = val(12)
      c       = val(13)
      d       = val(14)
      e       = val(15)



c     First peak of first doublet
      val11(1) = x11
      val11(2) = amp11
      val11(3) = sigma
      val11(4) = gamma1
      val11(5) = damperf

c     Second peak of first doublet
      val12(1) = x11 + dx01
      val12(2) = amp11*damp01
      val12(3) = sigma
      val12(4) = gamma1
      val12(5) = damperf

c     First peak of second doublet
      val21(1) = x11 + dx10
      val21(2) = amp11*damp10
      val21(3) = sigma
      val21(4) = gamma2
      val21(5) = damperf

c     Second peak of second doublet
      val22(1) = x11 + dx10 + dx01
      val22(2) = amp11*damp10*damp01
      val22(3) = sigma
      val22(4) = gamma2
      val22(4) = damperf

c     Polynomial background
      valp(1) = 0.
      valp(2) = a
      valp(3) = b
      valp(4) = c
      valp(5) = d
      valp(6) = e
      valp(7) = 0.
      valp(8) = 0.

      TWO_DOUBL_VOIGT_ERF_POLY = VOIGT_ERF(x,5,val11) +
     +     VOIGT_ERF(x,5,val12) + VOIGT_ERF(x,5,val21) +
     +     VOIGT_ERF(x,5,val22) + POLY(x,8,valp)


      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION POLY(X,npar,val)
c     Simple polynom of degree 6
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 POLY, x
      REAL*8 x0,a,b,c,d,e,f,g
      LOGICAL plot
      COMMON /func_plot/ plot



      x0 = val(1)
      a  = val(2)
      b  = val(3)
      c  = val(4)
      d  = val(5)
      e  = val(6)
      f  = val(7)
      g  = val(8)

      POLY = a + b*(x-x0) + c*(x-x0)**2 +  d*(x-x0)**3
     +     + e*(x-x0)**4 + f*(x-x0)**5 + g*(x-x0)**6


      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION POWER(X,npar,val)
c     Simple power function
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 POWER, x
      REAL*8 x0,a,n
      LOGICAL plot
      COMMON /func_plot/ plot



      x0 = val(1)
      a  = val(2)
      n  = val(3)

      POWER = a*(x-x0)**n

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, POWER
      END IF

      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION BS_EM(X,npar,val)
c     Bremmstralung emittance with Kramer's cross sections
      IMPLICIT NONE
      INTEGER*4 npar, Z
      REAL*8 val(npar), vale(3)
      REAL*8 BS_EM, BS_EM_NR, BS_EM_R, EXPFCN, x
      REAL*8 pi, alpha, hbar, c, me
      PARAMETER(pi=3.141592653589793d0, alpha=7.2973525376d-3)
      PARAMETER(hbar=6.58211899d-16, c=299792458d0, me = 510998.910d0 )
      REAL*8 x0, NiNe, kT

c     Constant comment
c     hbar in eV sec, c in m/sec, me in eV/c^2
c

c     "adjustable" constant: Z of the injected atom
      Z = 18

      x0    = val(1)
      NiNe  = val(2)
      kT    = val(3)

      vale(1) = x0
      vale(2) = 1.d0
      vale(3) = kT


c     Non-relativistic Bremmstralung emittance
      BS_EM_NR = NiNe*(Z*hbar)**2*(4*alpha*c/dsqrt(6*me))**3
     +     *dsqrt(pi/kT)*EXPFCN(x,3,vale)

c     Bremmstalung emittance with relativistic correction
c     (up to 1/(mec^2)^2 order))
      BS_EM_R = BS_EM_NR*(1.d0 + (16*x + kT)/(8*me)
     +     + (128*x**2 - 224*x*kT + 121*kT**2)/(128*me**2))

c     Choose the non-relativistic or relativistic function
c      BS_EM = BS_EM_NR
      BS_EM = BS_EM_R

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION BS_EM2(X,npar,val)
c     Bremmstralung emittance with Kramer's cross sections
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 BS_EM2, BS_EM_NR, BS_EM_R, x
      REAL*8 pi, alpha, hbar, c, me
      PARAMETER(pi=3.141592653589793d0, alpha=7.2973525376d-3)
c     Parameters in SI and eV
      PARAMETER(hbar=6.58211899d-16,c=299792458d0,me=510998.910d0 )
c     Parameters in pure SI
c      PARAMETER(hbar=1.054571726d-34,c=299792458d0,me=9.10938291d-31)
c      PARAMETER(e=1.602176565d-19)
c      REAL*8 xsi,kTsi
      REAL*8 NiNe, kT, Z

c     Constant comment
c     hbar in eV sec, c in m/sec, me in eV/c^2
c

c     "adjustable" constant: Z of the injected atom
c      Z = 18

      NiNe  = val(1)
      kT    = val(2)
      Z     = val(3)

c     Transform the energy "x" and "kT" from eV to SI
c      xsi = x*e
c      kTsi = kT*e

c     Non-relativistic Bremmstralung emittance
      BS_EM_NR = NiNe*(Z*hbar)**2*(4*alpha*c/dsqrt(6*me))**3
     +     *dsqrt(pi/kT)*dexp(-x/kT)

c     Bremmstalung emittance with relativistic correction
c     (up to 1/(mec^2)^2 order))
      BS_EM_R = BS_EM_NR*(1.d0 + (16*x + kT)/(8*me)
     +     + (128*x**2 - 224*x*kT + 121*kT**2)/(128*me**2))

c     Choose the non-relativistic or relativistic function
c      BS_EM2 = BS_EM_NR
      BS_EM2 = BS_EM_R

      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION BS_EM_NM(X,npar,val)
c     Bremmstralung emittance with Kramer's cross sections
c     with non Maxwelian distribution adapted by J.-P. Santos from Barue JAP1994
c     and present in Santos PRA2010
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 BS_EM_NM, x
      REAL*8 pi, alpha, hbar, c, me, e
      PARAMETER(pi=3.141592653589793d0, alpha=7.2973525376d-3)
c     Parameters in SI and eV
c      PARAMETER(hbar=6.58211899d-16,c=299792458d0,me=510998.910d0 )
c     Parameters in pure SI
      PARAMETER(hbar=1.054571726d-34,c=299792458d0,me=9.10938291d-31)
      PARAMETER(e=1.602176565d-19)
      REAL*8 xsi, kTsi
      REAL*8 NiNe, kT, Z

c     Constant comment
c     hbar in eV sec, c in m/sec, me in eV/c^2
c
c     "adjustable" constant: Z of the injected atom
c      Z = 18

      NiNe  = val(1)
      kT    = val(2)
      Z     = val(3)

c     Transform the energy "x" from eV to SI
      xsi = x*e
      kTsi = kT*e

      BS_EM_NM = NiNe*
c     First part
     -     (8*dsqrt(2.d0)*alpha**6*dexp(-xsi/kTsi)*hbar**4*Pi**2*Z**2)/
     -     (9.*c**2*kTsi**3*(1.d0 + (3*kTsi**2)/(c**4*me**2) +
     -     (3*kTsi)/(c**2*me))*
     -     me**2*(3*kTsi**2 + 3*c**2*kTsi*me + c**4*me**2))
c     Second part
     -     *(dsqrt(3.d0)*kTsi**2*(3*kTsi**2 + 3*c**2*kTsi*me
     -     + c**4*me**2))/
     -     (2048.*alpha**3*c**8*hbar**2*me**4.5*Pi)
c     Third part
     -     *(2*dsqrt(xsi)*(239085*kTsi**5 +
     -     630*kTsi**4*(135*c**2*me + 253*xsi)+
     -     32*(c**2*me + xsi)**3*(32*c**4*me**2 -
     -     24*c**2*me*xsi + 23*xsi**2) +
     -     84*kTsi**3*(145*c**4*me**2 + 675*c**2*me*xsi + 759*xsi**2) +
     -     8*kTsi**2*(705*c**6*me**3 + 1015*c**4*me**2*xsi +
     -     2835*c**2*me*xsi**2 +
     -     2277*xsi**3) + 16*kTsi*(216*c**8*me**4 + 235*c**6*me**3*xsi +
     -     203*c**4*me**2*xsi**2 + 405*c**2*me*xsi**3 + 253*xsi**4)) +
     -     dexp(xsi/kTsi)*dsqrt(kTsi)*(239085*kTsi**5 +
     -     85050*c**2*kTsi**4*me +
     -     12180*c**4*kTsi**3*me**2 + 5640*c**6*kTsi**2*me**3 +
     -     3456*c**8*kTsi*me**4 +
     -     1024*c**10*me**5)*dsqrt(Pi)*derfc(Sqrt(xsi/kTsi)))


      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION EXPSIN(X,npar,val)
c     Exponential function with sinusoidal modulation
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), vale(2)
      REAL*8 EXPSIN, EXPSIMP, x, N0, tau, amp, omega, phi
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)

      N0    = val(1)
      tau   = val(2)
      amp   = val(3)
      omega = val(4)
      phi   = val(5)

      vale(1) = N0
      vale(2) = tau

      EXPSIN = EXPSIMP(x,2,vale)*(1 + amp*dsin(omega*x+phi))


      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION EXPCOS(X,npar,val)
c     Exponential function with sinusoidal modulation
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), vale(2)
      REAL*8 EXPCOS, EXPSIMP, x, N0, tau, amp, omega, phi
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)

      N0    = val(1)
      tau   = val(2)
      amp   = val(3)
      omega = val(4)
      phi   = val(5)

      vale(1) = N0
      vale(2) = tau

      EXPCOS = EXPSIMP(x,2,vale)*(1 + amp*dcos(omega*x+phi))


      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION TWO_EXPSIN(x,npar,val)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), vale(2)
      REAL*8 EXPSIMP, TWO_EXPSIN, x
      REAL*8 N0, tau, amp1, amp2, omega1, omega2, phi1,phi2,dphi2


      N0      = val(1)
      tau     = val(2)
      amp1    = val(3)
      amp2    = val(4)
      omega1  = val(5)
      omega2  = val(6)
      phi1    = val(7)
      dphi2   = val(8)

      vale(1) = N0
      vale(2) = tau

      phi2 = phi1 + dphi2

      TWO_EXPSIN = EXPSIMP(x,2,vale)*(1 + amp1*dsin(omega2*x+phi1)
     +     + amp1*dsin(omega2*x+phi2))

      RETURN
      END




c     _______________________________________________________________________________________________


      FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS(x,npar,val)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(3),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 x1_1, dx2, dx3, dx4, dx5, dx6
      REAL*8 amp1_1, amp2_1, amp3_1, amp4_1, damp45, amp6_1, damperf_1
      REAL*8 sigma1_1
      REAL*8 sigma4_1
      REAL*8 dsigma21, dsigma31, dsigma54, dsigma61
      REAL*8 a_1, b_1, c_1
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1_1       = val(1)
      dx2        = val(2)
      dx3        = val(3)
      dx4        = val(4)
      dx5        = val(5)
      dx6        = val(6)
      amp1_1     = val(7)
      amp2_1     = val(8)
      amp3_1     = val(9)
      amp4_1     = val(10)
      damp45     = val(11)
      amp6_1     = val(12)
      sigma1_1   = val(13)
      dsigma21   = val(14)
      dsigma31   = val(15)
      sigma4_1   = val(16)
      dsigma54   = val(17)
      dsigma61   = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      b_1        = val(21)
      c_1        = val(22)

c     FIRST SPECTRUM

c     First (main) peak
      val1_1(1) = x1_1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1_1
      val1_1(4) = damperf_1

c     Second peak
      val2_1(1) = x1_1 + dx2
      val2_1(2) = amp2_1
      val2_1(3) = sigma1_1*dsigma21
      val2_1(4) = damperf_1

c     Third peak
      val3_1(1) = x1_1 + dx3
      val3_1(2) = amp3_1
      val3_1(3) = sigma1_1*dsigma31
      val3_1(4) = damperf_1

c     Fourth peak
      val4_1(1) = x1_1 + dx4
      val4_1(2) = amp4_1
      val4_1(3) = sigma4_1

c     Fifth peak
      val5_1(1) = x1_1 + dx5
      val5_1(2) = amp4_1*damp45
      val5_1(3) = sigma4_1*dsigma54

c     Sixth peak
      val6_1(1) = x1_1 + dx6
      val6_1(2) = amp6_1
      val6_1(3) = sigma1_1*dsigma61
      val6_1(4) = damperf_1

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

c     FIRST SPECTRUM

c     First (main) peak
      val1_1g(1) = x1_1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1_1
      val1_1e(1) = x1_1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1_1

c     Second peak
      val2_1g(1) = x1_1 + dx2
      val2_1g(2) = amp2_1
      val2_1g(3) = sigma1_1*dsigma21
      val2_1e(1) = x1_1 + dx2
      val2_1e(2) = amp2_1*damperf_1
      val2_1e(3) = sigma1_1*dsigma21

c     Third peak
      val3_1g(1) = x1_1 + dx3
      val3_1g(2) = amp3_1
      val3_1g(3) = sigma1_1*dsigma31
      val3_1e(1) = x1_1 + dx3
      val3_1e(2) = amp3_1*damperf_1
      val3_1e(3) = sigma1_1*dsigma31

c     Fourth peak
      val4_1g(1) = x1_1 + dx4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma4_1

c     Fifth peak
      val5_1g(1) = x1_1 + dx5
      val5_1g(2) = amp4_1*damp45
      val5_1g(3) = sigma4_1*dsigma54

c     Sixth peak
      val6_1g(1) = x1_1 + dx6
      val6_1g(2) = amp6_1
      val6_1g(3) = sigma1_1*dsigma61
      val6_1e(1) = x1_1 + dx6
      val6_1e(2) = amp6_1*damperf_1
      val6_1e(3) = sigma1_1*dsigma61

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = b_1
      valp_1(4) = c_1
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.


	  FOUR_GAUSS_ERF_TWO_GAUSS = GAUSS_ERF(x,4,val1_1) +
     +        GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +        GAUSS(x,3,val4_1) + GAUSS(x,3,val5_1) +
     +        GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)

c     Save the different components
      IF(plot) THEN
      WRITE(40,*) x, FOUR_GAUSS_ERF_TWO_GAUSS, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val4_1e),
     +			 ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
      ENDIF

	  RETURN
	  END

c _______________________________________________________________________________________________

	  FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS_STAN(x,npar,val)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 x1, dx21, dx31, x4, dx51, dx61
      REAL*8 amp1_1, damp21, damp31, amp4_1, damp51, damp61, damperf_1
      REAL*8 sigma1
      REAL*8 sigma4
      REAL*8 dsigma21, dsigma31, dsigma51, dsigma61
      REAL*8 a_1
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1         = val(1)
      dx21       = val(2)
      dx31       = val(3)
      x4         = val(4)
      dx51       = val(5)
      dx61       = val(6)
      amp1_1     = val(7)
      damp21     = val(8)
      damp31     = val(9)
      amp4_1     = val(10)
      damp51     = val(11)
      damp61     = val(12)
      sigma1     = val(13)
      dsigma21   = val(14)
      dsigma31   = val(15)
      sigma4     = val(16)
      dsigma51   = val(17)
      dsigma61   = val(18)
      damperf_1  = val(19)
      a_1        = val(20)

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1(1) = x1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1
      val1_1(4) = damperf_1

c     Second peak solid
      val2_1(1) = x1 + dx21
      val2_1(2) = amp1_1*damp21
      val2_1(3) = sigma1*dsigma21
      val2_1(4) = damperf_1

c     Third peak solid
      val3_1(1) = x1 + dx31
      val3_1(2) = amp1_1*damp31
      val3_1(3) = sigma1*dsigma31
      val3_1(4) = damperf_1

c     Fourth peak gas
      val4_1(1) = x4
      val4_1(2) = amp4_1
      val4_1(3) = sigma4

c     Fifth peak gas
      val5_1(1) = x1 + dx51
      val5_1(2) = amp1_1*damp51
      val5_1(3) = sigma1*dsigma51
      val5_1(4) = damperf_1

c     Sixth peak solid
      val6_1(1) = x1 + dx61
      val6_1(2) = amp1_1*damp61
      val6_1(3) = sigma1*dsigma61
      val6_1(4) = damperf_1

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1g(1) = x1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1
      val1_1e(1) = x1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1

c     Second peak solid
      val2_1g(1) = x1 + dx21
      val2_1g(2) = amp1_1*damp21
      val2_1g(3) = sigma1*dsigma21
      val2_1e(1) = x1 + dx21
      val2_1e(2) = amp1_1*damp21*damperf_1
      val2_1e(3) = sigma1*dsigma21

c     Third peak solid
      val3_1g(1) = x1 + dx31
      val3_1g(2) = amp1_1*damp31
      val3_1g(3) = sigma1*dsigma31
      val3_1e(1) = x1 + dx31
      val3_1e(2) = amp1_1*damp31*damperf_1
      val3_1e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_1g(1) = x4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma4

c     Fifth peak gas
      val5_1g(1) = x1 + dx51
      val5_1g(2) = amp1_1*damp51
      val5_1g(3) = sigma1*dsigma51
      val5_1e(1) = x1 + dx51
      val5_1e(2) = amp1_1*damp51*damperf_1
      val5_1e(3) = sigma1*dsigma51

c     Sixth peak solid
      val6_1g(1) = x1 + dx61
      val6_1g(2) = amp1_1*damp61
      val6_1g(3) = sigma1*dsigma61
      val6_1e(1) = x1 + dx61
      val6_1e(2) = amp1_1*damp61*damperf_1
      val6_1e(3) = sigma1*dsigma61

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = 0.
      valp_1(4) = 0.
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.


      FOUR_GAUSS_ERF_TWO_GAUSS_STAN = GAUSS_ERF(x,4,val1_1) +
     +     GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +     GAUSS(x,3,val4_1) + GAUSS_ERF(x,3,val5_1) +
     +     GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, FOUR_GAUSS_ERF_TWO_GAUSS_STAN,
     +        GAUSS(x,4,val1_1g),
     +        GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +        GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +        GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +        ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +        ERFFCN(x,3,val3_1e), ERFFCN(x,3,val5_1e),
     +        ERFFCN(x,3,val6_1e)
      ENDIF

      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION GAUSS_ERF_CST(X,npar,val)
c     Normalized Gaussian distribution + "Shirley" background (i.e. error function in this case)
c     proportional to the integral. The direction of the background is selected by the input file
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valg(3), vale(3), valc(1)
      REAL*8 GAUSS_ERF_CST, GAUSS, ERFFCN, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma_gauss, damp, sigma_erf, cst
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x0   	  = val(1)
      amp  	  = val(2)
      sigma_gauss = val(3)
      damp	  = val(4)
      sigma_erf	  = val(5)
      cst         = val(6)

      valg(1) = x0
      valg(2) = amp
      valg(3) = sigma_gauss

      vale(1) = x0
      vale(2) = amp*damp
      vale(3) = sigma_erf

	  valc(1) = cst

      GAUSS_ERF_CST = GAUSS(x,3,valg) + ERFFCN(x,3,vale) +  valc(1)

c     Save the different components
      IF(plot) THEN
		WRITE(40,*) x, GAUSS_ERF_CST, GAUSS(x,3,valg),
     +           ERFFCN(x,3,vale),  valc(1)
      ENDIF

      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION GAUSSERF_CST(X,npar,val)
c     Normalized Gaussian distribution + "Shirley" background (i.e. error function in this case)
c     proportional to the integral. The direction of the background is selected by the input file
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), valg(4), vale_plot(3), valc(1), valg_plot(3)
      REAL*8 GAUSS_ERF, GAUSS, ERFFCN, x, GAUSSERF_CST
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, amp, sigma, damp, cst
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x0   			 = val(1)
      amp  			 = val(2)
      sigma		     = val(3)
      damp			 = val(4)
	  cst 		     = val(5)

      valg(1) = x0
      valg(2) = amp
      valg(3) = sigma
      valg(4) = damp

	  valc(1) = cst

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

      valg_plot(1) = x0
      valg_plot(2) = amp
      valg_plot(3) = sigma

      vale_plot(1) = x0
      vale_plot(2) = amp*damp
      vale_plot(3) = sigma

      GAUSSERF_CST = GAUSS_ERF(x,4,valg) +  valc(1)

c     Save the different components
      IF(plot) THEN
		WRITE(40,*) x, GAUSSERF_CST, GAUSS(x,3,valg_plot),
     +           ERFFCN(x,3,vale_plot),  valc(1)
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________


      FUNCTION FOUR_VOIGT_BG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4),val4(4), valc(1)
      REAL*8 FOUR_VOIGT_BG_PLEIADES, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx43
      REAL*8 amp1, amp2, amp3, amp4
      REAL*8 sigma, gamma1, gamma2,gamma3, gamma4, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01    = val(1)
      dx21   = val(2)
      dx31   = val(3)
      dx43   = val(4)
      amp1   = val(5)
      amp2   = val(6)
      amp3   = val(7)
      amp4   = val(8)
      sigma  = val(9)
      gamma1 = val(10)
      gamma2 = val(11)
      gamma3 = val(12)
      gamma4 = val(13)
      bg     = val(14)


c     first voigt peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma1

c     second voigt peak
      val2(1) = x01 + dx21
      val2(2) = amp2
      val2(3) = sigma
      val2(4) = gamma2

c     third voigt peak
      val3(1) = x01 + dx31
      val3(2) = amp3
      val3(3) = sigma
      val3(4) = gamma3

c     fourth voigt peak
      val4(1) = x01 + dx31 + dx43
      val4(2) = amp4
      val4(3) = sigma
      val4(4) = gamma4

	  valc(1) = bg

      FOUR_VOIGT_BG_PLEIADES = VOIGT(x,4,val1) + VOIGT(x,4,val2)
     +     + VOIGT(x,4,val3) + VOIGT(x,4,val4) + valc(1)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, FOUR_VOIGT_BG_PLEIADES,
     +        VOIGT(x,4,val1), VOIGT(x,4,val2),
     +        VOIGT(x,4,val3),  VOIGT(x,4,val4), valc(1)
      ENDIF

      RETURN
      END
c _________________________________________________________________________________________________


      FUNCTION FOUR_GAUSS_BG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3),val2(3),val3(3),val4(3), valc(1)
      REAL*8 FOUR_GAUSS_BG_PLEIADES, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx43
      REAL*8 amp1, amp2, amp3, amp4
      REAL*8 sigma1, sigma2, sigma3, sigma4, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01    = val(1)
      dx21   = val(2)
      dx31   = val(3)
      dx43   = val(4)
      amp1   = val(5)
      amp2   = val(6)
      amp3   = val(7)
      amp4   = val(8)
      sigma1 = val(9)
      sigma2 = val(10)
      sigma3 = val(11)
      sigma4 = val(12)
      bg     = val(13)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1

c     second gauss peak
      val2(1) = x01 + dx21
      val2(2) = amp2
      val2(3) = sigma2

c     third gauss peak
      val3(1) = x01 + dx31
      val3(2) = amp3
      val3(3) = sigma3

c     fourth gauss peak
      val4(1) = x01 + dx31 + dx43
      val4(2) = amp4
      val4(3) = sigma4

	  valc(1) = bg

      FOUR_GAUSS_BG_PLEIADES = GAUSS(x,3,val1) + GAUSS(x,3,val2)
     +     + GAUSS(x,3,val3) + GAUSS(x,3,val4) + valc(1)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, FOUR_GAUSS_BG_PLEIADES,
     +        GAUSS(x,3,val1), GAUSS(x,3,val2),
     +        GAUSS(x,3,val3),  GAUSS(x,3,val4), valc(1)
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________


      FUNCTION FOUR_PSEUDOVOIGT_BG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1l(3), val1g(3), val2l(3), val2g(3)
      REAL*8 val3l(3), val3g(3),val4l(3), val4g(3)
      REAL*8 valc(1), valp(1)
      REAL*8 FOUR_PSEUDOVOIGT_BG_PLEIADES, GAUSS, LORE, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx43
      REAL*8 amp1, amp2, amp3, amp4
      REAL*8 gamma1, gamma2, gamma3, gamma4, sigma, p, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01    = val(1)
      dx21   = val(2)
      dx31   = val(3)
      dx43   = val(4)
      amp1   = val(5)
      amp2   = val(6)
      amp3   = val(7)
      amp4   = val(8)
      sigma  = val(9)
      gamma1 = val(10)
      gamma2 = val(11)
      gamma3 = val(12)
      gamma4 = val(13)
      bg     = val(14)
      p	     = val(15)


c     first pseudovoigt peak
      val1l(1) = x01
      val1l(2) = amp1
      val1l(3) = gamma1
      val1g(1) = x01
      val1g(2) = amp1
      val1g(3) = sigma

c     second pseudovoigt peak
      val2l(1) = x01 + dx21
      val2l(2) = amp2
      val2l(3) = gamma2
      val2g(1) = x01 + dx21
      val2g(2) = amp2
      val2g(3) = sigma

c     third pseudovoigt peak
      val3l(1) = x01 + dx31
      val3l(2) = amp3
      val3l(3) = gamma3
      val3g(1) = x01 + dx31
      val3g(2) = amp3
      val3g(3) = sigma

c     fourth pseudovoigt peak
      val4l(1) = x01 + dx31 + dx43
      val4l(2) = amp4
      val4l(3) = gamma4
      val4g(1) = x01 + dx31 + dx43
      val4g(2) = amp4
      val4g(3) = sigma

	  valc(1) = bg
	  valp(1) = p

      FOUR_PSEUDOVOIGT_BG_PLEIADES =
     +         valp(1)*LORE(x,3,val1l) + (1-valp(1))*GAUSS(x,3,val1g)
     +         + valp(1)*LORE(x,3,val2l) + (1-valp(1))*GAUSS(x,3,val2g)
     +         + valp(1)*LORE(x,3,val3l) + (1-valp(1))*GAUSS(x,3,val3g)
     +         + valp(1)*LORE(x,3,val4l) + (1-valp(1))*GAUSS(x,3,val4g)
     +         + valc(1)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, FOUR_PSEUDOVOIGT_BG_PLEIADES,
     +        valp(1)*LORE(x,3,val1l) + (1-valp(1))*GAUSS(x,3,val1g),
     +        valp(1)*LORE(x,3,val2l) + (1-valp(1))*GAUSS(x,3,val2g),
     +        valp(1)*LORE(x,3,val3l) + (1-valp(1))*GAUSS(x,3,val3g),
     +        valp(1)*LORE(x,3,val4l) + (1-valp(1))*GAUSS(x,3,val4g),
     +        valc(1)
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________


      FUNCTION FOUR_GAUSS_PARAMETER_BG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3),val2(3),val3(3),val4(3), valc(1)
      REAL*8 FOUR_GAUSS_PARAMETER_BG_PLEIADES, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx42
      REAL*8 amp1, damp21, damp31, damp42
      REAL*8 sigma1, dsigma21, dsigma31, dsigma42, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01   	 = val(1)
      dx21  	 = val(2)
      dx31  	 = val(3)
      dx42	 = val(4)
      amp1  	 = val(5)
      damp21	 = val(6)
      damp31 	 = val(7)
      damp42 	 = val(8)
      sigma1	 = val(9)
      dsigma21	 = val(10)
      dsigma31	 = val(11)
      dsigma42	 = val(12)
      bg    	 = val(13)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1

c     second gauss peak
      val2(1) = x01 + dx21
      val2(2) = amp1 * damp21
      val2(3) = sigma1 * dsigma21

c     third gauss peak
      val3(1) = x01 + dx31
      val3(2) = amp1 * damp31
      val3(3) = sigma1 * dsigma31

c     fourth gauss peak
      val4(1) = x01 + dx21 + dx31 + dx42
      val4(2) = amp1 * damp21 * damp31 * damp42
      val4(3) = sigma1 * dsigma21 * dsigma31 * dsigma42

	  valc(1) = bg

      FOUR_GAUSS_PARAMETER_BG_PLEIADES = GAUSS(x,3,val1)
     +     + GAUSS(x,3,val2)
     +     + GAUSS(x,3,val3) + GAUSS(x,3,val4) + valc(1)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, FOUR_GAUSS_PARAMETER_BG_PLEIADES,
     +        GAUSS(x,3,val1), GAUSS(x,3,val2),
     +        GAUSS(x,3,val3),  GAUSS(x,3,val4), valc(1)
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________


      FUNCTION FOUR_VOIGT_PARAMETER_BG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4),val4(4), valc(1)
      REAL*8 FOUR_VOIGT_PARAMETER_BG_PLEIADES, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx42
      REAL*8 amp1, damp21, damp31, damp42
      REAL*8 sigma, gamma1, dgamma21, dgamma31, dgamma42, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01   	 = val(1)
      dx21  	 = val(2)
      dx31  	 = val(3)
      dx42	  	 = val(4)
      amp1  	 = val(5)
      damp21	 = val(6)
      damp31 	 = val(7)
      damp42 	 = val(8)
      sigma 	 = val(9)
      gamma1	 = val(10)
      dgamma21	 = val(11)
      dgamma31	 = val(12)
      dgamma42	 = val(13)
      bg    	 = val(14)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma1

c     second gauss peak
      val2(1) = x01 + dx21
      val2(2) = amp1 * damp21
      val2(3) = sigma
      val2(4) = gamma1 * dgamma21

c     third gauss peak
      val3(1) = x01 + dx31
      val3(2) = amp1 * damp31
      val3(3) = sigma
      val3(4) = gamma1 * dgamma31

c     fourth gauss peak
      val4(1) = x01 + dx21 + dx31 + dx42
      val4(2) = amp1 * damp21 * damp31 * damp42
      val4(3) = sigma
      val4(4) = gamma1 * dgamma21 * dgamma31 * dgamma42

	  valc(1) = bg

      FOUR_VOIGT_PARAMETER_BG_PLEIADES = VOIGT(x,4,val1)
     +     + VOIGT(x,4,val2)
     +     + VOIGT(x,4,val3) + VOIGT(x,3,val4) + valc(1)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, FOUR_VOIGT_PARAMETER_BG_PLEIADES,
     +        VOIGT(x,4,val1), VOIGT(x,4,val2),
     +        VOIGT(x,4,val3),  VOIGT(x,4,val4), valc(1)
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________


      FUNCTION SIX_VOIGT_PARAMETER_BG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4)
      REAL*8 val4(4), val5(4), val6(4)
      REAL*8 valc(1)
      REAL*8 SIX_VOIGT_PARAMETER_BG_PLEIADES, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx42, dx51, dx62
      REAL*8 amp1, damp21, damp31, damp42, damp51, damp62
      REAL*8 sigma, gamma1, dgamma21, dgamma31
      REAL*8 dgamma42, dgamma51, dgamma62, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01   	 = val(1)
      dx21  	 = val(2)
      dx31  	 = val(3)
      dx42	 = val(4)
      amp1  	 = val(5)
      damp21	 = val(6)
      damp31 	 = val(7)
      damp42 	 = val(8)
      sigma 	 = val(9)
      gamma1	 = val(10)
      dgamma21	 = val(11)
      dgamma31	 = val(12)
      dgamma42	 = val(13)
      bg    	 = val(14)
      dx51  	 = val(15)
      dx62  	 = val(16)
      damp51	 = val(17)
      damp62	 = val(18)
      dgamma51	 = val(19)
      dgamma62	 = val(20)

c     first voigt peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma1

c     second voigt peak
      val2(1) = x01 + dx21
      val2(2) = amp1 * damp21
      val2(3) = sigma
      val2(4) = gamma1 * dgamma21

c     third voigt peak
      val3(1) = x01 + dx31
      val3(2) = amp1 * damp31
      val3(3) = sigma
      val3(4) = gamma1 * dgamma31

c     fourth voigt peak
      val4(1) = x01 + dx21 + dx31 + dx42
      val4(2) = amp1 * damp21 * damp31 * damp42
      val4(3) = sigma
      val4(4) = gamma1 * dgamma21 * dgamma31 * dgamma42

c     fifth voigt peak
      val5(1) = x01 + dx51
      val5(2) = amp1 * damp51
      val5(3) = sigma
      val5(4) = gamma1 * dgamma51

c     sixth voigt peak
      val6(1) = x01 + dx21 + dx51 + dx62
      val6(2) = amp1 * damp21 * damp51 * damp62
      val6(3) = sigma
      val6(4) = gamma1 * dgamma21 * dgamma51 * dgamma62

      valc(1) = bg

      SIX_VOIGT_PARAMETER_BG_PLEIADES = VOIGT(x,4,val1)
     +     + VOIGT(x,4,val2) + VOIGT(x,4,val3)
     +     + VOIGT(x,3,val4) + VOIGT(x,4,val5)
     +     + VOIGT(x,4,val6) + valc(1)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, SIX_VOIGT_PARAMETER_BG_PLEIADES,
     +        VOIGT(x,4,val1), VOIGT(x,4,val2),
     +        VOIGT(x,4,val3), VOIGT(x,4,val4),
     +        VOIGT(x,4,val5), VOIGT(x,4,val6),
     +        valc(1)
      ENDIF

      RETURN
      END
c _________________________________________________________________________________________________


      FUNCTION SIX_GAUSS_PARAMETER_BG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3),val2(3),val3(3),val4(3)
      REAL*8 val5(3), val6(3), valc(1)
      REAL*8 SIX_GAUSS_PARAMETER_BG_PLEIADES, GAUSS, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx42,dx51, dx62
      REAL*8 amp1, damp21, damp31, damp42, damp51, damp62
      REAL*8 sigma1, dsigma21, dsigma31, dsigma42, dsigma51
      REAL*8 dsigma62, bg
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01   	 = val(1)
      dx21  	 = val(2)
      dx31  	 = val(3)
      dx42	 = val(4)
      amp1  	 = val(5)
      damp21	 = val(6)
      damp31 	 = val(7)
      damp42 	 = val(8)
      sigma1	 = val(9)
      dsigma21	 = val(10)
      dsigma31	 = val(11)
      dsigma42	 = val(12)
      bg    	 = val(13)
      dx51  	 = val(14)
      dx62  	 = val(15)
      damp51	 = val(16)
      damp62	 = val(17)
      dsigma51	 = val(18)
      dsigma62	 = val(19)


c     first gauss peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1

c     second gauss peak
      val2(1) = x01 + dx21
      val2(2) = amp1 * damp21
      val2(3) = sigma1 * dsigma21

c     third gauss peak
      val3(1) = x01 + dx31
      val3(2) = amp1 * damp31
      val3(3) = sigma1 * dsigma31

c     fourth gauss peak
      val4(1) = x01 + dx21 + dx31 + dx42
      val4(2) = amp1 * damp21 * damp31 * damp42
      val4(3) = sigma1 * dsigma21 * dsigma31 * dsigma42

c     fifth gauss peak
      val5(1) = x01 + dx51
      val5(2) = amp1 * damp51
      val5(3) = sigma1 * dsigma51

c     sixth gauss peak
      val6(1) = x01 + dx21 + dx51 + dx62
      val6(2) = amp1 * damp21 * damp51 * damp62
      val6(3) = sigma1 * dsigma21 * dsigma51 * dsigma62

	  valc(1) = bg

      SIX_GAUSS_PARAMETER_BG_PLEIADES = GAUSS(x,3,val1)
     +         + GAUSS(x,3,val2) + GAUSS(x,3,val3) + GAUSS(x,3,val4)
     +         + GAUSS(x,3,val5) + GAUSS(x,3,val6) + valc(1)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, SIX_GAUSS_PARAMETER_BG_PLEIADES,
     +        GAUSS(x,3,val1), GAUSS(x,3,val2),
     +        GAUSS(x,3,val3),  GAUSS(x,3,val4),
     +        GAUSS(x,3,val5), GAUSS(x,3,val6), valc(1)
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________


      FUNCTION SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1l(3), val1g(3), val2l(3), val2g(3)
      REAL*8 val3l(3), val3g(3),val4l(3), val4g(3)
      REAL*8 val5l(3), val5g(3),val6l(3), val6g(3)
      REAL*8 valc(1), valp(1)
      REAL*8 SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES, GAUSS, LORENORM, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx42
      REAL*8 amp1, damp21, damp31, damp42
      REAL*8 gamma1, dgamma21, dgamma31, dgamma42, sigma, p, bg
      REAL*8 dx51, dx62, damp51, damp62, dgamma51, dgamma62
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01   	 = val(1)
      dx21  	 = val(2)
      dx31  	 = val(3)
      dx42  	 = val(4)
      amp1  	 = val(5)
      damp21	 = val(6)
      damp31 	 = val(7)
      damp42 	 = val(8)
      sigma 	 = val(9)
      gamma1 	 = val(10)
      dgamma21 	 = val(11)
      dgamma31	 = val(12)
      dgamma42 	 = val(13)
      bg         = val(14)
      p	         = val(15)
      dx51  	 = val(16)
      dx62  	 = val(17)
      damp51	 = val(18)
      damp62	 = val(19)
      dgamma51	 = val(20)
      dgamma62	 = val(21)

c     first pseudovoigt peak
      val1l(1) = x01
      val1l(2) = amp1
      val1l(3) = gamma1
      val1g(1) = x01
      val1g(2) = amp1
      val1g(3) = sigma

c     second pseudovoigt peak
      val2l(1) = x01 + dx21
      val2l(2) = amp1 * damp21
      val2l(3) = gamma1 * dgamma21
      val2g(1) = x01 + dx21
      val2g(2) = amp1 * damp21
      val2g(3) = sigma

c     third pseudovoigt peak
      val3l(1) = x01 + dx31
      val3l(2) = amp1 * damp31
      val3l(3) = gamma1 * dgamma31
      val3g(1) = x01 + dx31
      val3g(2) = amp1 * damp31
      val3g(3) = sigma

c     fourth pseudovoigt peak
      val4l(1) = x01 + dx21 + dx31 + dx42
      val4l(2) = amp1 * damp21 * damp31 * damp42
      val4l(3) = gamma1 * dgamma21 * dgamma31 * dgamma42
      val4g(1) = x01 + dx21 + dx31 + dx42
      val4g(2) = amp1 * damp21 * damp31 * damp42
      val4g(3) = sigma

c     fifth pseudovoigt peak
      val5l(1) = x01 + dx51
      val5l(2) = amp1 * damp51
      val5l(3) = gamma1 * dgamma51
      val5g(1) = x01 + dx51
      val5g(2) = amp1 * damp51
      val5g(3) = sigma

c     sixth pseudovoigt peak
      val6l(1) = x01 + dx21 + dx51 + dx62
      val6l(2) = amp1 * damp21 * damp51 * damp62
      val6l(3) = gamma1 * dgamma21 * dgamma51 * dgamma62
      val6g(1) = x01 + dx21 + dx51 + dx62
      val6g(2) = amp1 * damp21 * damp51 * damp62
      val6g(3) = sigma

      valc(1) = bg
      valp(1) = p

      SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES =
     +     valp(1)*LORENORM(x,3,val1l) + (1-valp(1))*GAUSS(x,3,val1g)
     +     + valp(1)*LORENORM(x,3,val2l) + (1-valp(1))*GAUSS(x,3,val2g)
     +	   + valp(1)*LORENORM(x,3,val3l) + (1-valp(1))*GAUSS(x,3,val3g)
     + 	   + valp(1)*LORENORM(x,3,val4l) + (1-valp(1))*GAUSS(x,3,val4g)
     + 	   + valp(1)*LORENORM(x,3,val5l) + (1-valp(1))*GAUSS(x,3,val5g)
     + 	   + valp(1)*LORENORM(x,3,val6l) + (1-valp(1))*GAUSS(x,3,val6g)
     +	   + valc(1)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES,
     +        valp(1)*LORENORM(x,3,val1l)+(1-valp(1))*GAUSS(x,3,val1g),
     +        valp(1)*LORENORM(x,3,val2l)
     +        + (1-valp(1))*GAUSS(x,3,val2g),
     +        valp(1)*LORENORM(x,3,val3l)
     +        + (1-valp(1))*GAUSS(x,3,val3g),
     +        valp(1)*LORENORM(x,3,val4l)
     +        + (1-valp(1))*GAUSS(x,3,val4g),
     +        valp(1)*LORENORM(x,3,val5l)
     +        + (1-valp(1))*GAUSS(x,3,val5g),
     +        valp(1)*LORENORM(x,3,val6l)
     +        + (1-valp(1))*GAUSS(x,3,val6g),
     +        valc(1)
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________

      FUNCTION SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4)
      REAL*8 val4(4), val5(4), val6(4)
      REAL*8 valc(1)
      REAL*8 SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES, VOIGT, SHIRLEY, x
      EXTERNAL SHIRLEY
      REAL*8 pi, dx_sh, x0_sh
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx42, dx51, dx62
      REAL*8 amp1, damp21, damp31, damp42, damp51, damp62
      REAL*8 sigma1, dsigma31, dsigma43, gamma1, dgamma21, dgamma31
      REAL*8 dgamma42, dgamma51, dgamma62, bg, bg_sh
      CHARACTER*1 lr
      COMMON /aux_shirely/dx_sh, x0_sh
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01   	 = val(1)
      dx21  	 = val(2)
      dx31  	 = val(3)
      dx42	 = val(4)
      amp1  	 = val(5)
      damp21	 = val(6)
      damp31 	 = val(7)
      damp42 	 = val(8)
      sigma1 	 = val(9)
      gamma1	 = val(10)
      dgamma21	 = val(11)
      dgamma31	 = val(12)
      dgamma42	 = val(13)
      bg    	 = val(14)
      dx51  	 = val(15)
      dx62  	 = val(16)
      damp51	 = val(17)
      damp62	 = val(18)
      dgamma51	 = val(19)
      dgamma62	 = val(20)
      bg_sh  = val(21)
      dsigma31  = val(22)
      dsigma43  = val(23)

c     first voigt peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1
      val1(4) = gamma1

c     second voigt peak
      val2(1) = x01 + dx21
      val2(2) = amp1 * damp21
      val2(3) = sigma1
      val2(4) = gamma1 * dgamma21

c     third voigt peak
      val3(1) = x01 + dx31
      val3(2) = amp1 * damp31
      val3(3) = sigma1 * dsigma31
      val3(4) = gamma1 * dgamma31

c     fourth voigt peak
      val4(1) = x01 + dx21 + dx31 + dx42
      val4(2) = amp1 * damp21 * damp31 * damp42
      val4(3) = sigma1 * dsigma31 * dsigma43
      val4(4) = gamma1 * dgamma21 * dgamma31 * dgamma42

c     fifth voigt peak
      val5(1) = x01 + dx51
      val5(2) = amp1 * damp51
      val5(3) = sigma1
      val5(4) = gamma1 * dgamma51

c     sixth voigt peak
      val6(1) = x01 + dx21 + dx51 + dx62
      val6(2) = amp1 * damp21 * damp51 * damp62
      val6(3) = sigma1
      val6(4) = gamma1 * dgamma21 * dgamma51 * dgamma62

      valc(1) = bg

      SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES = VOIGT(x,4,val1)
     +     + VOIGT(x,4,val2) + VOIGT(x,4,val3)
     +     + VOIGT(x,4,val4) + VOIGT(x,4,val5)
     +     + VOIGT(x,4,val6) + valc(1)
     +     + bg_sh*(SHIRLEY(x))

c     Save the different components
      IF(plot) THEN
		WRITE(40,*) x, VOIGT(x,4,val1), VOIGT(x,4,val2),
     +           VOIGT(x,4,val3), VOIGT(x,4,val4),
     +           VOIGT(x,4,val5), VOIGT(x,4,val6),
     +           valc(1),
     +           bg_sh*(SHIRLEY(x))
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________

      FUNCTION SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4)
      REAL*8 val4(4), val5(4), val6(4)
      REAL*8 valc(1)
      REAL*8 SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES, VOIGT, SHIRLEY, x
      EXTERNAL SHIRLEY
      REAL*8 pi, dx_sh, x0_sh
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx42, dx51, dx62
      REAL*8 amp1, damp21, damp31, damp42, damp51, damp62
      REAL*8 sigma, gamma1, dgamma21, dgamma31
      REAL*8 dgamma42, dgamma51, dgamma62, bg, bg_sh
      CHARACTER*1 lr
      COMMON /aux_shirely/dx_sh, x0_sh
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01   	 = val(1)
      dx21  	 = val(2)
      dx31  	 = val(3)
      dx42	 = val(4)
      amp1  	 = val(5)
      damp21	 = val(6)
      damp31 	 = val(7)
      damp42 	 = val(8)
      sigma 	 = val(9)
      gamma1	 = val(10)
      dgamma21	 = val(11)
      dgamma31	 = val(12)
      dgamma42	 = val(13)
      bg    	 = val(14)
      dx51  	 = val(15)
      dx62  	 = val(16)
      damp51	 = val(17)
      damp62	 = val(18)
      dgamma51	 = val(19)
      dgamma62	 = val(20)
      bg_sh      = val(21)

c     first voigt peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma1

c     second voigt peak
      val2(1) = x01 + dx21
      val2(2) = amp1 * damp21
      val2(3) = sigma
      val2(4) = gamma1 * dgamma21

c     third voigt peak
      val3(1) = x01 + dx31
      val3(2) = amp1 * damp31
      val3(3) = sigma
      val3(4) = gamma1 * dgamma31

c     fourth voigt peak
      val4(1) = x01 + dx21 + dx31 + dx42
      val4(2) = amp1 * damp21 * damp31 * damp42
      val4(3) = sigma
      val4(4) = gamma1 * dgamma21 * dgamma31 * dgamma42

c     fifth voigt peak
      val5(1) = x01 + dx51
      val5(2) = amp1 * damp51
      val5(3) = sigma
      val5(4) = gamma1 * dgamma51

c     sixth voigt peak
      val6(1) = x01 + dx21 + dx51 + dx62
      val6(2) = amp1 * damp21 * damp51 * damp62
      val6(3) = sigma
      val6(4) = gamma1 * dgamma21 * dgamma51 * dgamma62

      valc(1) = bg

      SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES = VOIGT(x,4,val1)
     +     + VOIGT(x,4,val2) + VOIGT(x,4,val3)
     +     + VOIGT(x,3,val4) + VOIGT(x,4,val5)
     +     + VOIGT(x,4,val6) + valc(1)
     +     + bg_sh*(SHIRLEY(x))

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES,
     +        VOIGT(x,4,val1), VOIGT(x,4,val2),
     +        VOIGT(x,4,val3), VOIGT(x,4,val4),
     +        VOIGT(x,4,val5), VOIGT(x,4,val6),
     +        valc(1),
     +        bg_sh*(SHIRLEY(x))
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________


      FUNCTION SIX_VOIGT_PARA_POLY_SIG_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4)
      REAL*8 val4(4), val5(4), val6(4)
      REAL*8 valc(8)
      REAL*8 SIX_VOIGT_PARA_POLY_SIG_PLEIADES, VOIGT, POLY, x
      REAL*8 pi, dx_sh, x0_sh
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x01, dx21, dx31, dx42, dx51, dx62
      REAL*8 amp1, damp21, damp31, damp42, damp51, damp62
      REAL*8 sigma1, dsigma31, dsigma43, gamma1, dgamma21, dgamma31
      REAL*8 dgamma42, dgamma51, dgamma62, bg, bg_sh, slope
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x01   	 = val(1)
      dx21  	 = val(2)
      dx31  	 = val(3)
      dx42	 	 = val(4)
      amp1  	 = val(5)
      damp21	 = val(6)
      damp31 	 = val(7)
      damp42 	 = val(8)
      sigma1 	 = val(9)
      gamma1	 = val(10)
      dgamma21	 = val(11)
      dgamma31	 = val(12)
      dgamma42	 = val(13)
      bg    	 = val(14)
      dx51  	 = val(15)
      dx62  	 = val(16)
      damp51	 = val(17)
      damp62	 = val(18)
      dgamma51	 = val(19)
      dgamma62	 = val(20)
      slope 	 = val(21)
      dsigma31   = val(22)
      dsigma43   = val(23)

c     first voigt peak
      val1(1) = x01
      val1(2) = amp1
      val1(3) = sigma1
      val1(4) = gamma1

c     second voigt peak
      val2(1) = x01 + dx21
      val2(2) = amp1 * damp21
      val2(3) = sigma1
      val2(4) = gamma1 * dgamma21

c     third voigt peak
      val3(1) = x01 + dx31
      val3(2) = amp1 * damp31
      val3(3) = sigma1 * dsigma31
      val3(4) = gamma1 * dgamma31

c     fourth voigt peak
      val4(1) = x01 + dx21 + dx31 + dx42
      val4(2) = amp1 * damp21 * damp31 * damp42
      val4(3) = sigma1 * dsigma31 * dsigma43
      val4(4) = gamma1 * dgamma21 * dgamma31 * dgamma42

c     fifth voigt peak
      val5(1) = x01 + dx51
      val5(2) = amp1 * damp51
      val5(3) = sigma1
      val5(4) = gamma1 * dgamma51

c     sixth voigt peak
      val6(1) = x01 + dx21 + dx51 + dx62
      val6(2) = amp1 * damp21 * damp51 * damp62
      val6(3) = sigma1
      val6(4) = gamma1 * dgamma21 * dgamma51 * dgamma62

      valc(1) = 0.
      valc(2) = bg
      valc(3) = slope
      valc(4) = 0.
      valc(5) = 0.
      valc(6) = 0.
      valc(7) = 0.
      valc(8) = 0.

      SIX_VOIGT_PARA_POLY_SIG_PLEIADES = VOIGT(x,4,val1)
     +     + VOIGT(x,4,val2) + VOIGT(x,4,val3)
     +     + VOIGT(x,3,val4) + VOIGT(x,4,val5)
     +     + VOIGT(x,4,val6) + valc(1)
     +     + valc(2)+x*valc(3)

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, VOIGT(x,4,val1), VOIGT(x,4,val2),
     +        VOIGT(x,4,val3), VOIGT(x,4,val4),
     +        VOIGT(x,4,val5), VOIGT(x,4,val6),
     +        valc(2)+x*valc(3)
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________

      FUNCTION ND_M_PLEIADES(x,npar,val)
c     Voigt profile using the complex error function (from Paul Indelicato) and following
c     the wikipedia text to obtain a normalized Voigt profile
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(4),val2(4),val3(4)
      REAL*8 val4(3), val5(3), val6(4)
      REAL*8 valc(1)
      REAL*8 ND_M_PLEIADES, VOIGT, SHIRLEY, x, GAUSS
      EXTERNAL SHIRLEY
      REAL*8 pi, dx_sh, x0_sh
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x1, dx21, dx31, x4, dx54, x6
      REAL*8 amp1, damp21, damp31, amp4, damp54, amp6
      REAL*8 sigma, sigma4, dsigma54
      REAL*8 gamma1, dgamma21, dgamma31, gamma6, bg, bg_sh
      CHARACTER*1 lr
      COMMON /aux_shirely/dx_sh, x0_sh
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1   		 = val(1)
      dx21  	 = val(2)
      dx31  	 = val(3)
      x4		 = val(4)
      dx54		 = val(5)
      x6		 = val(6)
      amp1  	 = val(7)
      damp21	 = val(8)
      damp31 	 = val(9)
      amp4	 	 = val(10)
      damp54	 = val(11)
      amp6	 	 = val(12)
      sigma 	 = val(13)
      gamma1	 = val(14)
      dgamma21	 = val(15)
      dgamma31	 = val(16)
      sigma4	 = val(17)
      dsigma54	 = val(18)
      gamma6	 = val(19)
      bg    	 = val(20)
      bg_sh  	 = val(21)

c     first voigt peak
      val1(1) = x1
      val1(2) = amp1
      val1(3) = sigma
      val1(4) = gamma1

c     second voigt peak
      val2(1) = x1 + dx21
      val2(2) = amp1 * damp21
      val2(3) = sigma
      val2(4) = gamma1 * dgamma21

c     third voigt peak
      val3(1) = x1 + dx31
      val3(2) = amp1 * damp31
      val3(3) = sigma
      val3(4) = gamma1 * dgamma31

c     fourth gauss peak
      val4(1) = x4
      val4(2) = amp4
      val4(3) = sigma4

c     fifth gauss peak
      val5(1) = x4 + dx54
      val5(2) = amp4 * damp54
      val5(3) = sigma4 * dsigma54

c     sixth voigt peak
      val6(1) = x6
      val6(2) = amp6
      val6(3) = sigma
      val6(4) = gamma6

      valc(1) = bg

      ND_M_PLEIADES = VOIGT(x,4,val1)
     +     + VOIGT(x,4,val2) + VOIGT(x,4,val3)
     +     + GAUSS(x,3,val4) + GAUSS(x,3,val5)
     +     + VOIGT(x,4,val6) + valc(1)
     +     + bg_sh*(SHIRLEY(x))

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, VOIGT(x,4,val1), VOIGT(x,4,val2),
     +        VOIGT(x,4,val3), GAUSS(x,3,val4),
     +        GAUSS(x,3,val5), VOIGT(x,4,val6),
     +        valc(1),
     +        bg_sh*(SHIRLEY(x))
      ENDIF

      RETURN
      END

c _________________________________________________________________________________________________


      FUNCTION GAUSS_3D(X,npar,val)
c     Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(3)
      REAL*8 x(3)
      REAL*8 GAUSS_3D 
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, y0, z0, amp, sigmax, sigmay, sigmaz, bg
      REAL*8  x_, y_, z_, expx, expy, expz
      LOGICAL plot
      COMMON /func_plot/ plot

      x(1) = x_
      x(2) = y_
      x(3) = z_

      bg    = val(1)
      x0    = val(2)
      y0    = val(3)
      z0    = val(4)
      amp   = val(5)
      sigmax= val(6)
      sigmay= val(7)
      sigmaz= val(8)

c     Test of under of underflow first
      expx = -(x_-x0)**2/(2*sigmax**2)
      expy = -(y_-y0)**2/(2*sigmay**2)
      expz = -(z_-z0)**2/(2*sigmaz**2)
      IF(DABS(expx + expy + expz).LT.700) THEN
         GAUSS_3D = amp/(dsqrt((2*pi)**3)*sigmax*sigmay*sigmaz)*
     +        dexp(expx + expy + expz)
      ELSE
         GAUSS_3D = 0.d0
      END IF      


      RETURN
      END





c ##############################################################################################
