c     Automatic Time-stamp: <Last changed by martino on Tuesday 02 April 2019 at CEST 17:19:46>
c################################### USERFCN DEFINITION #####################################

      FUNCTION USERFCN_SET(x,npar,val,funcname,j)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar)
      REAL*8 x, USERFCN_SET, WEIBULL_EL_LASER, GAUSS_BG_SET
      REAL*8 DOUBLE_EXPSIN, DOUBLE_EXPSIN_BIS, DOUBLE_EXPSIMP
      REAL*8 DOUBLE_TWO_EXPSIN
      REAL*8 TRIPLE_EXPSIN,TRIPLE_EXPSIN_BIS,TRIPLE_EXPSIN_TRIS
      REAL*8 TRIPLE_EXPSIMP
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET2
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET3
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_SET
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET
      REAL*8 ROCKING_CURVE_SET
      CHARACTER*64 funcname

 
c     Choose your model (see below for definition)
      IF(funcname.EQ.'GAUSS_BG_SET') THEN
         USERFCN_SET = GAUSS_BG_SET(x,npar,val,j)
      ELSE IF(funcname.EQ.'DOUBLE_EXPSIMP') THEN
         USERFCN_SET = DOUBLE_EXPSIMP(x,npar,val,j)
      ELSE IF(funcname.EQ.'DOUBLE_EXPSIN') THEN
         USERFCN_SET = DOUBLE_EXPSIN(x,npar,val,j)
      ELSE IF(funcname.EQ.'DOUBLE_EXPSIN_BIS') THEN
         USERFCN_SET = DOUBLE_EXPSIN_BIS(x,npar,val,j)
      ELSE IF(funcname.EQ.'DOUBLE_TWO_EXPSIN') THEN
         USERFCN_SET = DOUBLE_TWO_EXPSIN(x,npar,val,j)
      ELSE IF(funcname.EQ.'TRIPLE_EXPSIMP') THEN
         USERFCN_SET = TRIPLE_EXPSIMP(x,npar,val,j)
      ELSE IF(funcname.EQ.'TRIPLE_EXPSIN') THEN
         USERFCN_SET = TRIPLE_EXPSIN(x,npar,val,j)
      ELSE IF(funcname.EQ.'TRIPLE_EXPSIN_BIS') THEN
         USERFCN_SET = TRIPLE_EXPSIN_BIS(x,npar,val,j)
      ELSE IF(funcname.EQ.'TRIPLE_EXPSIN_TRIS') THEN
         USERFCN_SET = TRIPLE_EXPSIN_TRIS(x,npar,val,j)
      ELSE IF(funcname.EQ.'WEIBULL_EL_LASER') THEN
         USERFCN_SET = WEIBULL_EL_LASER(x,npar,val,j)
      ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET') THEN
         USERFCN_SET = SIX_GAUSS_ERF_FREESIG_POLY_SET(x,npar,val,j)
      ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET2') THEN
         USERFCN_SET = SIX_GAUSS_ERF_FREESIG_POLY_SET2(x,npar,val,j)
      ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET3') THEN
         USERFCN_SET = SIX_GAUSS_ERF_FREESIG_POLY_SET3(x,npar,val,j)
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_SET') THEN
         USERFCN_SET = FOUR_GAUSS_ERF_TWO_GAUSS_SET(x,npar,val,j)
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET') THEN
         USERFCN_SET = FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET(x,npar,val,j)
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET') THEN
         USERFCN_SET = FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET(x,npar,val,j)
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET') THEN
         USERFCN_SET = FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET(x,npar,val,j)
      ELSE IF(funcname.EQ.'ROCKING_CURVE_SET') THEN
         USERFCN_SET = ROCKING_CURVE_SET(x,npar,val,j)
      ELSE
         WRITE(*,*) 'Selected function:', funcname
         WRITE(*,*) 'Error in the function name def. in USERFCN_SET'
         WRITE(*,*) 'Check in the manual and in the input.dat file'
         STOP
      END IF

      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION GAUSS_BG_SET(x,npar,val,j)
c     Double gaussian with background with common sigma
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(7), val2(4)
      REAL*8 GAUSS_BG_SET, GAUSS_BG, x
      REAL*8 x01, delta, amp1, amp2, sigma, bg1, bg2

      bg1   = val(1)
      x01   = val(2)
      amp1  = val(3)
      sigma = val(4)
      bg2   = val(5)
      delta = val(6)
      amp2  = val(7)

c     first gauss peak
      val1(1) = bg1
      val1(2) = x01
      val1(3) = amp1
      val1(4) = sigma


c     second gauss peak
      val2(1) = bg2
      val2(2) = x01 + delta
      val2(3) = amp2
      val2(4) = sigma

      IF (j.EQ.1) THEN
         GAUSS_BG_SET = GAUSS_BG(x,4,val1)
      ELSEIF (j.EQ.2) THEN
         GAUSS_BG_SET = GAUSS_BG(x,4,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION DOUBLE_EXPSIMP(x,npar,val,j)
c     Double exponential decay with common decay constant
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(2), val2(2)
      REAL*8 DOUBLE_EXPSIMP, EXPSIMP, x
      REAL*8 N01, N02, tau


      N01   = val(1)
      N02   = val(2)
      tau   = val(3)

c     first exponential decay
      val1(1) = N01
      val1(2) = tau


c     second exponential decay
      val2(1) = N02
      val2(2) = tau

      IF (j.EQ.1) THEN
         DOUBLE_EXPSIMP = EXPSIMP(x,2,val1)
      ELSEIF (j.EQ.2) THEN
         DOUBLE_EXPSIMP = EXPSIMP(x,2,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION DOUBLE_EXPSIN(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5)
      REAL*8 DOUBLE_EXPSIN, EXPSIN, x
      REAL*8 N01, N02, tau, amp1, amp2, omega, domega, phi1, phi2


      N01    = val(1)
      N02    = val(2)
      tau    = val(3)
      amp1   = val(4)
      amp2   = val(5)
      omega  = val(6)
      domega = val(7)
      phi1   = val(8)
      phi2   = val(9)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp2
      val2(4) = omega + domega
      val2(5) = phi2

      IF (j.EQ.1) THEN
         DOUBLE_EXPSIN = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         DOUBLE_EXPSIN = EXPSIN(x,5,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END
c______________________________________________________________________________________________

      FUNCTION DOUBLE_EXPSIN_BIS(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5)
      REAL*8 DOUBLE_EXPSIN_BIS, EXPSIN, x
      REAL*8 N01, N02, tau, amp1, damp2, omega, domega, phi1, dphi2


      N01    = val(1)
      N02    = val(2)
      tau    = val(3)
      amp1   = val(4)
      damp2   = val(5)
      omega  = val(6)
      domega = val(7)
      phi1   = val(8)
      dphi2   = val(9)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp1*damp2
      val2(4) = omega + domega
      val2(5) = phi1 + dphi2

      IF (j.EQ.1) THEN
         DOUBLE_EXPSIN_BIS = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         DOUBLE_EXPSIN_BIS = EXPSIN(x,5,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION DOUBLE_TWO_EXPSIN(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(8), val2(8)
      REAL*8 DOUBLE_TWO_EXPSIN, TWO_EXPSIN, x
      REAL*8 N01, N02, tau, amp11, amp12, damp2
      REAL*8 omega1, omega2, phi1, dphi2


      N01    = val(1)
      N02    = val(2)
      tau    = val(3)
      amp11  = val(4)
      amp12  = val(5)
      damp2  = val(6)
      omega1 = val(7)
      omega2 = val(8)
      phi1   = val(9)
      dphi2  = val(10)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp11
      val1(4) = amp12
      val1(5) = omega1
      val1(6) = omega2
      val1(7) = phi1
      val1(8) = dphi2


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp11*damp2
      val2(4) = amp12*damp2
      val2(5) = omega1
      val2(6) = omega2
      val2(7) = phi1
      val2(8) = dphi2

      IF (j.EQ.1) THEN
         DOUBLE_TWO_EXPSIN = TWO_EXPSIN(x,8,val1)
      ELSEIF (j.EQ.2) THEN
         DOUBLE_TWO_EXPSIN = TWO_EXPSIN(x,8,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END


c______________________________________________________________________________________________

      FUNCTION TRIPLE_EXPSIMP(x,npar,val,j)
c     Triple exponential decay with common decay constant
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(2), val2(2), val3(2)
      REAL*8 TRIPLE_EXPSIMP, EXPSIMP, x
      REAL*8 N01, N02, N03,tau


      N01   = val(1)
      N02   = val(2)
      N03   = val(3)
      tau   = val(4)

c     first exponential decay
      val1(1) = N01
      val1(2) = tau


c     second exponential decay
      val2(1) = N02
      val2(2) = tau


c     third exponential decay
      val3(1) = N03
      val3(2) = tau

      IF (j.EQ.1) THEN
         TRIPLE_EXPSIMP = EXPSIMP(x,2,val1)
      ELSEIF (j.EQ.2) THEN
         TRIPLE_EXPSIMP = EXPSIMP(x,2,val2)
      ELSEIF (j.EQ.3) THEN
         TRIPLE_EXPSIMP = EXPSIMP(x,2,val3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION TRIPLE_EXPSIN(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5), val3(5)
      REAL*8 TRIPLE_EXPSIN, EXPSIN, x
      REAL*8 N01, N02,  N03, tau, amp1, amp2, amp3
      REAL*8  omega, domega1, domega2, phi1, phi2, phi3


      N01    = val(1)
      N02    = val(2)
      N03    = val(3)
      tau    = val(4)
      amp1   = val(5)
      amp2   = val(6)
      amp3   = val(7)
      omega  = val(8)
      domega1= val(9)
      domega2= val(10)
      phi1   = val(11)
      phi2   = val(12)
      phi3   = val(13)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp2
      val2(4) = omega + domega1
      val2(5) = phi2


c     third exponential decay
      val3(1) = N03
      val3(2) = tau
      val3(3) = amp3
      val3(4) = omega + domega2
      val3(5) = phi3

      IF (j.EQ.1) THEN
         TRIPLE_EXPSIN = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         TRIPLE_EXPSIN = EXPSIN(x,5,val2)
      ELSEIF (j.EQ.3) THEN
         TRIPLE_EXPSIN = EXPSIN(x,5,val3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END
c______________________________________________________________________________________________

      FUNCTION TRIPLE_EXPSIN_BIS(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5), val3(5)
      REAL*8 TRIPLE_EXPSIN_BIS, EXPSIN, x
      REAL*8 N01, N02,  N03, tau, amp1, damp2, damp3
      REAL*8  omega, domega1, domega2, phi1, dphi2, dphi3


      N01    = val(1)
      N02    = val(2)
      N03    = val(3)
      tau    = val(4)
      amp1   = val(5)
      damp2   = val(6)
      damp3   = val(7)
      omega  = val(8)
      domega1= val(9)
      domega2= val(10)
      phi1   = val(11)
      dphi2   = val(12)
      dphi3   = val(13)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp1*damp2
      val2(4) = omega + domega1
      val2(5) = phi1+dphi2


c     third exponential decay
      val3(1) = N03
      val3(2) = tau
      val3(3) = amp1*damp2*damp3
      val3(4) = omega + domega2
      val3(5) = phi1+dphi2+dphi3

      IF (j.EQ.1) THEN
         TRIPLE_EXPSIN_BIS = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         TRIPLE_EXPSIN_BIS = EXPSIN(x,5,val2)
      ELSEIF (j.EQ.3) THEN
         TRIPLE_EXPSIN_BIS = EXPSIN(x,5,val3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END
c______________________________________________________________________________________________

      FUNCTION TRIPLE_EXPSIN_TRIS(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5), val3(5)
      REAL*8 TRIPLE_EXPSIN_TRIS, EXPSIN, x
      REAL*8 N01, N02,  N03, tau, amp1, amp2, damp3
      REAL*8  omega, domega1, domega2, phi1, phi2, dphi3


      N01    = val(1)
      N02    = val(2)
      N03    = val(3)
      tau    = val(4)
      amp1   = val(5)
      amp2   = val(6)
      damp3   = val(7)
      omega  = val(8)
      domega1= val(9)
      domega2= val(10)
      phi1   = val(11)
      phi2   = val(12)
      dphi3   = val(13)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp2
      val2(4) = omega + domega1
      val2(5) = phi2


c     third exponential decay
      val3(1) = N03
      val3(2) = tau
      val3(3) = amp2*damp3
      val3(4) = omega + domega2
      val3(5) = phi2+dphi3

      IF (j.EQ.1) THEN
         TRIPLE_EXPSIN_TRIS = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         TRIPLE_EXPSIN_TRIS = EXPSIN(x,5,val2)
      ELSEIF (j.EQ.3) THEN
         TRIPLE_EXPSIN_TRIS = EXPSIN(x,5,val3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END
c______________________________________________________________________________________________

      FUNCTION WEIBULL_EL_LASER(X,npar,val,j)
c     Double fit with normalized Weibull function with "erf" background
c     for the first spectrum (cluster-electron type) and
c     normalized Weibull for the second spectrum (cluster-laser type)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val_el(8), val_laser(4)
      REAL*8 WEIBULL,WEIBULL_EL_LASER, WEIBULL_ERFBG, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0_el, amp_el, lambda, kappa
      REAL*8 dx0_erf, amp_erf,dsigma_erf, bg_el
      REAL*8 dx0_laser, amp_laser
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x0_el      = val(1)
      amp_el     = val(2)
      lambda     = val(3)
      kappa      = val(4)
      dx0_erf    = val(5)
      amp_erf    = val(6)
      dsigma_erf = val(7)
      bg_el      = val(8)
      dx0_laser  = val(9)
      amp_laser  = val(10)

c     Parameters for the Weibull distribution with erf background for cluster-electron interaction
      val_el(1) = x0_el
      val_el(2) = amp_el
      val_el(3) = lambda
      val_el(4) = kappa
      val_el(5) = dx0_erf
      val_el(6) = amp_erf
      val_el(7) = dsigma_erf
      val_el(8) = bg_el
c     Parameters for the Weibull distribution for cluster-electron interaction
      val_laser(1) = x0_el + dx0_laser
      val_laser(2) = amp_laser
      val_laser(3) = lambda
      val_laser(4) = kappa


      IF(j.EQ.1) THEN
         WEIBULL_EL_LASER = WEIBULL_ERFBG(X,8,val_el)
      ELSE IF(j.EQ.2) THEN
         WEIBULL_EL_LASER = WEIBULL(X,4,val_laser)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF


      RETURN
      END

c     _______________________________________________________________________________________________

      FUNCTION SIX_GAUSS_ERF_FREESIG_POLY_SET(X,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      INTEGER*4 j
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(4),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(4),val5_2(4),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
      REAL*8 valp_3(8), val1_3g(3),val2_3g(3),val3_3g(3)
      REAL*8 val4_3g(3),val5_3g(3),val6_3g(3)
      REAL*8 val1_3(4),val2_3(4),val3_3(4)
      REAL*8 val4_3(4),val5_3(4),val6_3(4)
      REAL*8 val1_3e(3),val2_3e(3),val3_3e(3)
      REAL*8 val4_3e(3),val5_3e(3),val6_3e(3)
      REAL*8 x1_1, x1_2, x1_3, dx2, dx3, dx4, dx5, dx6
      REAL*8 amp1_1, amp2_1, amp3_1, amp4_1, amp5_1, amp6_1, damperf_1
      REAL*8 amp1_2, amp2_2, amp3_2, amp4_2, amp5_2, amp6_2, damperf_2
      REAL*8 amp1_3, amp2_3, amp3_3, amp4_3, amp5_3, amp6_3, damperf_3
      REAL*8 sigma1_1, sigma1_2, sigma1_3
      REAL*8 dsigma2, dsigma3, dsigma4, dsigma5, dsigma6
      REAL*8 a_1, b_1, c_1, a_2, b_2, c_2, a_3, b_3, c_3
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
      amp5_1     = val(11)
      amp6_1     = val(12)
      sigma1_1   = val(13)
      dsigma2    = val(14)
      dsigma3    = val(15)
      dsigma4    = val(16)
      dsigma5    = val(17)
      dsigma6    = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      b_1        = val(21)
      c_1        = val(22)
      x1_2       = val(23)
      amp1_2     = val(24)
      amp2_2     = val(25)
      amp3_2     = val(26)
      amp4_2     = val(27)
      amp5_2     = val(28)
      amp6_2     = val(29)
      sigma1_2   = val(30)
      damperf_2  = val(31)
      a_2        = val(32)
      b_2        = val(33)
      c_2        = val(34)
      x1_3       = val(35)
      amp1_3     = val(36)
      amp2_3     = val(37)
      amp3_3     = val(38)
      amp4_3     = val(39)
      amp5_3     = val(40)
      amp6_3     = val(41)
      sigma1_3   = val(42)
      damperf_3  = val(43)
      a_3        = val(44)
      b_3        = val(45)
      c_3        = val(46)


c     FIRST SPECTRUM

c     First (main) peak
      val1_1(1) = x1_1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1_1
      val1_1(4) = damperf_1

c     Second peak
      val2_1(1) = x1_1 + dx2
      val2_1(2) = amp2_1
      val2_1(3) = sigma1_1*dsigma2
      val2_1(4) = damperf_1

c     Third peak
      val3_1(1) = x1_1 + dx3
      val3_1(2) = amp3_1
      val3_1(3) = sigma1_1*dsigma3
      val3_1(4) = damperf_1

c     Fourth peak
      val4_1(1) = x1_1 + dx4
      val4_1(2) = amp4_1
      val4_1(3) = sigma1_1*dsigma4
      val4_1(4) = damperf_1

c     Fifth peak
      val5_1(1) = x1_1 + dx5
      val5_1(2) = amp5_1
      val5_1(3) = sigma1_1*dsigma5
      val5_1(4) = damperf_1

c     Sixth peak
      val6_1(1) = x1_1 + dx6
      val6_1(2) = amp6_1
      val6_1(3) = sigma1_1*dsigma6
      val6_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak
      val1_2(1) = x1_2
      val1_2(2) = amp1_2
      val1_2(3) = sigma1_2
      val1_2(4) = damperf_2

c     Second peak
      val2_2(1) = x1_2 + dx2
      val2_2(2) = amp2_2
      val2_2(3) = sigma1_2*dsigma2
      val2_2(4) = damperf_2

c     Third peak
      val3_2(1) = x1_2 + dx3
      val3_2(2) = amp3_2
      val3_2(3) = sigma1_2*dsigma3
      val3_2(4) = damperf_2

c     Fourth peak
      val4_2(1) = x1_2 + dx4
      val4_2(2) = amp4_2
      val4_2(3) = sigma1_2*dsigma4
      val4_2(4) = damperf_2

c     Fifth peak
      val5_2(1) = x1_2 + dx5
      val5_2(2) = amp5_2
      val5_2(3) = sigma1_2*dsigma5
      val5_2(4) = damperf_2

c     Sixth peak
      val6_2(1) = x1_2 + dx6
      val6_2(2) = amp6_2
      val6_2(3) = sigma1_2*dsigma6
      val6_2(4) = damperf_2

c     THIRD SPECTRUM

c     First (main) peak
      val1_3(1) = x1_3
      val1_3(2) = amp1_3
      val1_3(3) = sigma1_3
      val1_3(4) = damperf_3

c     Second peak
      val2_3(1) = x1_3 + dx2
      val2_3(2) = amp2_3
      val2_3(3) = sigma1_3*dsigma2
      val2_3(4) = damperf_3

c     Third peak
      val3_3(1) = x1_3 + dx3
      val3_3(2) = amp3_3
      val3_3(3) = sigma1_3*dsigma3
      val3_3(4) = damperf_3

c     Fourth peak
      val4_3(1) = x1_3 + dx4
      val4_3(2) = amp4_3
      val4_3(3) = sigma1_3*dsigma4
      val4_3(4) = damperf_3

c     Fifth peak
      val5_3(1) = x1_3 + dx5
      val5_3(2) = amp5_3
      val5_3(3) = sigma1_3*dsigma5
      val5_3(4) = damperf_3

c     Sixth peak
      val6_3(1) = x1_3 + dx6
      val6_3(2) = amp6_3
      val6_3(3) = sigma1_3*dsigma6
      val6_3(4) = damperf_3

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
      val2_1g(3) = sigma1_1*dsigma2
      val2_1e(1) = x1_1 + dx2
      val2_1e(2) = amp2_1*damperf_1
      val2_1e(3) = sigma1_1*dsigma2

c     Third peak
      val3_1g(1) = x1_1 + dx3
      val3_1g(2) = amp3_1
      val3_1g(3) = sigma1_1*dsigma3
      val3_1e(1) = x1_1 + dx3
      val3_1e(2) = amp3_1*damperf_1
      val3_1e(3) = sigma1_1*dsigma3

c     Fourth peak
      val4_1g(1) = x1_1 + dx4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma1_1*dsigma4
      val4_1e(1) = x1_1 + dx4
      val4_1e(2) = amp4_1*damperf_1
      val4_1e(3) = sigma1_1*dsigma4

c     Fifth peak
      val5_1g(1) = x1_1 + dx5
      val5_1g(2) = amp5_1
      val5_1g(3) = sigma1_1*dsigma5
      val5_1e(1) = x1_1 + dx5
      val5_1e(2) = amp5_1*damperf_1
      val5_1e(3) = sigma1_1*dsigma5

c     Sixth peak
      val6_1g(1) = x1_1 + dx6
      val6_1g(2) = amp6_1
      val6_1g(3) = sigma1_1*dsigma6
      val6_1e(1) = x1_1 + dx6
      val6_1e(2) = amp6_1*damperf_1
      val6_1e(3) = sigma1_1*dsigma6

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = b_1
      valp_1(4) = c_1
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak
      val1_2g(1) = x1_2
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1_2
      val1_2e(1) = x1_2
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1_2

c     Second peak
      val2_2g(1) = x1_2 + dx2
      val2_2g(2) = amp2_2
      val2_2g(3) = sigma1_2*dsigma2
      val2_2e(1) = x1_2 + dx2
      val2_2e(2) = amp2_2*damperf_2
      val2_2e(3) = sigma1_2*dsigma2

c     Third peak
      val3_2g(1) = x1_2 + dx3
      val3_2g(2) = amp3_2
      val3_2g(3) = sigma1_2*dsigma3
      val3_2e(1) = x1_2 + dx3
      val3_2e(2) = amp3_2*damperf_2
      val3_2e(3) = sigma1_2*dsigma3

c     Fourth peak
      val4_2g(1) = x1_2 + dx4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma1_2*dsigma4
      val4_2e(1) = x1_2 + dx4
      val4_2e(2) = amp4_2*damperf_2
      val4_2e(3) = sigma1_2*dsigma4

c     Fifth peak
      val5_2g(1) = x1_2 + dx5
      val5_2g(2) = amp5_2
      val5_2g(3) = sigma1_2*dsigma5
      val5_2e(1) = x1_2 + dx5
      val5_2e(2) = amp5_2*damperf_2
      val5_2e(3) = sigma1_2*dsigma5

c     Sixth peak
      val6_2g(1) = x1_2 + dx6
      val6_2g(2) = amp6_2
      val6_2g(3) = sigma1_2*dsigma6
      val6_2e(1) = x1_2 + dx6
      val6_2e(2) = amp6_2*damperf_2
      val6_2e(3) = sigma1_2*dsigma6

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = b_2
      valp_2(4) = c_2
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

c     THIRD SPECTRUM

c     First (main) peak
      val1_3g(1) = x1_3
      val1_3g(2) = amp1_3
      val1_3g(3) = sigma1_3
      val1_3e(1) = x1_3
      val1_3e(2) = amp1_3*damperf_3
      val1_3e(3) = sigma1_3

c     Second peak
      val2_3g(1) = x1_3 + dx2
      val2_3g(2) = amp2_3
      val2_3g(3) = sigma1_3*dsigma2
      val2_3e(1) = x1_3 + dx2
      val2_3e(2) = amp2_3*damperf_3
      val2_3e(3) = sigma1_3*dsigma2

c     Third peak
      val3_3g(1) = x1_3 + dx3
      val3_3g(2) = amp3_3
      val3_3g(3) = sigma1_3*dsigma3
      val3_3e(1) = x1_3 + dx3
      val3_3e(2) = amp3_3*damperf_3
      val3_3e(3) = sigma1_3*dsigma3

c     Fourth peak
      val4_3g(1) = x1_3 + dx4
      val4_3g(2) = amp4_3
      val4_3g(3) = sigma1_3*dsigma4
      val4_3e(1) = x1_3 + dx4
      val4_3e(2) = amp4_3*damperf_3
      val4_3e(3) = sigma1_3*dsigma4

c     Fifth peak
      val5_3g(1) = x1_3 + dx5
      val5_3g(2) = amp5_3
      val5_3g(3) = sigma1_3*dsigma5
      val5_3e(1) = x1_3 + dx5
      val5_3e(2) = amp5_3*damperf_3
      val5_3e(3) = sigma1_3*dsigma5

c     Sixth peak
      val6_3g(1) = x1_3 + dx6
      val6_3g(2) = amp6_3
      val6_3g(3) = sigma1_3*dsigma6
      val6_3e(1) = x1_3 + dx6
      val6_3e(2) = amp6_3*damperf_3
      val6_3e(3) = sigma1_3*dsigma6

c     Polynomial background
      valp_3(1) = 0.
      valp_3(2) = a_3
      valp_3(3) = b_3
      valp_3(4) = c_3
      valp_3(5) = 0.
      valp_3(6) = 0.
      valp_3(7) = 0.
      valp_3(8) = 0.

      IF (j.EQ.1) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET = GAUSS_ERF(x,4,val1_1) +
     +        GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +        GAUSS_ERF(x,4,val4_1) + GAUSS_ERF(x,4,val5_1) +
     +        GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET = GAUSS_ERF(x,4,val1_2) +
     +        GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +        GAUSS_ERF(x,4,val4_2) + GAUSS_ERF(x,4,val5_2) +
     +        GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)
      ELSEIF (j.EQ.3) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET = GAUSS_ERF(x,4,val1_3) +
     +        GAUSS_ERF(x,4,val2_3) + GAUSS_ERF(x,4,val3_3) +
     +        GAUSS_ERF(x,4,val4_3) + GAUSS_ERF(x,4,val5_3) +
     +        GAUSS_ERF(x,4,val6_3) + POLY(x,8,valp_3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
           WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val4_1e),
     +			 ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +		 	 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val4_2e),
     +			 ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e)
         ELSEIF (j.EQ.3) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_3g),
     +           GAUSS(x,3,val2_3g),  GAUSS(x,3,val3_3g),
     +           GAUSS(x,3,val4_3g),  GAUSS(x,3,val5_3g),
     +           GAUSS(x,3,val6_3g),  POLY(x,8,valp_3),
     +			 ERFFCN(x,3,val1_3e), ERFFCN(x,3,val2_3e),
     +			 ERFFCN(x,3,val3_3e), ERFFCN(x,3,val4_3e),
     +			 ERFFCN(x,3,val5_3e), ERFFCN(x,3,val6_3e)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF
      ENDIF


      RETURN
      END

c     _______________________________________________________________________________________________


      FUNCTION SIX_GAUSS_ERF_FREESIG_POLY_SET2(X,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      INTEGER*4 j
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET2, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(4),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(4),val5_2(4),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
      REAL*8 valp_3(8), val1_3g(3),val2_3g(3),val3_3g(3)
      REAL*8 val4_3g(3),val5_3g(3),val6_3g(3)
      REAL*8 val1_3(4),val2_3(4),val3_3(4)
      REAL*8 val4_3(4),val5_3(4),val6_3(4)
      REAL*8 val1_3e(3),val2_3e(3),val3_3e(3)
      REAL*8 val4_3e(3),val5_3e(3),val6_3e(3)
      REAL*8 x1_1, x1_2, x1_3, dx2, dx3, dx4, dx5, dx6
      REAL*8 amp1_1, amp2_1, amp3_1, amp4_1, damp45, amp6_1, damperf_1
      REAL*8 amp1_2, amp2_2, amp3_2, amp4_2, amp6_2, damperf_2
      REAL*8 amp1_3, amp2_3, amp3_3, amp4_3, amp6_3, damperf_3
      REAL*8 sigma1_1, sigma1_2, sigma1_3
      REAL*8 sigma4_1, sigma4_2, sigma4_3
      REAL*8 dsigma21, dsigma31, dsigma54, dsigma61
      REAL*8 a_1, b_1, c_1, a_2, b_2, c_2, a_3, b_3, c_3
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
      x1_2       = val(23)
      amp1_2     = val(24)
      amp2_2     = val(25)
      amp3_2     = val(26)
      amp4_2     = val(27)
      amp6_2     = val(28)
      sigma1_2   = val(29)
      sigma4_2   = val(30)
      damperf_2  = val(31)
      a_2        = val(32)
      b_2        = val(33)
      c_2        = val(34)
      x1_3       = val(35)
      amp1_3     = val(36)
      amp2_3     = val(37)
      amp3_3     = val(38)
      amp4_3     = val(39)
      amp6_3     = val(40)
      sigma1_3   = val(41)
      sigma4_3   = val(42)
      damperf_3  = val(43)
      a_3        = val(44)
      b_3        = val(45)
      c_3        = val(46)


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

c     SECOND SPECTRUM

c     First (main) peak
      val1_2(1) = x1_2
      val1_2(2) = amp1_2
      val1_2(3) = sigma1_2
      val1_2(4) = damperf_2

c     Second peak
      val2_2(1) = x1_2 + dx2
      val2_2(2) = amp2_2
      val2_2(3) = sigma1_2*dsigma21
      val2_2(4) = damperf_2

c     Third peak
      val3_2(1) = x1_2 + dx3
      val3_2(2) = amp3_2
      val3_2(3) = sigma1_2*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak
      val4_2(1) = x1_2 + dx4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4_2
      val4_2(4) = damperf_2

c     Fifth peak
      val5_2(1) = x1_2 + dx5
      val5_2(2) = amp4_2*damp45
      val5_2(3) = sigma4_2*dsigma54
      val5_2(4) = damperf_2

c     Sixth peak
      val6_2(1) = x1_2 + dx6
      val6_2(2) = amp6_2
      val6_2(3) = sigma1_2*dsigma61
      val6_2(4) = damperf_2

c     THIRD SPECTRUM

c     First (main) peak
      val1_3(1) = x1_3
      val1_3(2) = amp1_3
      val1_3(3) = sigma1_3
      val1_3(4) = damperf_3

c     Second peak
      val2_3(1) = x1_3 + dx2
      val2_3(2) = amp2_3
      val2_3(3) = sigma1_3*dsigma21
      val2_3(4) = damperf_3

c     Third peak
      val3_3(1) = x1_3 + dx3
      val3_3(2) = amp3_3
      val3_3(3) = sigma1_3*dsigma31
      val3_3(4) = damperf_3

c     Fourth peak
      val4_3(1) = x1_3 + dx4
      val4_3(2) = amp4_3
      val4_3(3) = sigma4_3
      val4_3(4) = damperf_3

c     Fifth peak
      val5_3(1) = x1_3 + dx5
      val5_3(2) = amp4_3*damp45
      val5_3(3) = sigma4_3*dsigma54
      val5_3(4) = damperf_3

c     Sixth peak
      val6_3(1) = x1_3 + dx6
      val6_3(2) = amp6_3
      val6_3(3) = sigma1_3*dsigma61
      val6_3(4) = damperf_3

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

c     SECOND SPECTRUM

c     First (main) peak
      val1_2g(1) = x1_2
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1_2
      val1_2e(1) = x1_2
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1_2

c     Second peak
      val2_2g(1) = x1_2 + dx2
      val2_2g(2) = amp2_2
      val2_2g(3) = sigma1_2*dsigma21
      val2_2e(1) = x1_2 + dx2
      val2_2e(2) = amp2_2*damperf_2
      val2_2e(3) = sigma1_2*dsigma21

c     Third peak
      val3_2g(1) = x1_2 + dx3
      val3_2g(2) = amp3_2
      val3_2g(3) = sigma1_2*dsigma31
      val3_2e(1) = x1_2 + dx3
      val3_2e(2) = amp3_2*damperf_2
      val3_2e(3) = sigma1_2*dsigma31

c     Fourth peak
      val4_2g(1) = x1_2 + dx4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4_2
      val4_2e(1) = x1_2 + dx4
      val4_2e(2) = amp4_2*damperf_2
      val4_2e(3) = sigma4_2

c     Fifth peak
      val5_2g(1) = x1_2 + dx5
      val5_2g(2) = amp4_2*damp45
      val5_2g(3) = sigma4_2*dsigma54
      val5_2e(1) = x1_2 + dx5
      val5_2e(2) = amp4_2*damp45*damperf_2
      val5_2e(3) = sigma4_2*dsigma54

c     Sixth peak
      val6_2g(1) = x1_2 + dx6
      val6_2g(2) = amp6_2
      val6_2g(3) = sigma1_2*dsigma61
      val6_2e(1) = x1_2 + dx6
      val6_2e(2) = amp6_2*damperf_2
      val6_2e(3) = sigma1_2*dsigma61

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = b_2
      valp_2(4) = c_2
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

c     THIRD SPECTRUM

c     First (main) peak
      val1_3g(1) = x1_3
      val1_3g(2) = amp1_3
      val1_3g(3) = sigma1_3
      val1_3e(1) = x1_3
      val1_3e(2) = amp1_3*damperf_3
      val1_3e(3) = sigma1_3

c     Second peak
      val2_3g(1) = x1_3 + dx2
      val2_3g(2) = amp2_3
      val2_3g(3) = sigma1_3*dsigma21
      val2_3e(1) = x1_3 + dx2
      val2_3e(2) = amp2_3*damperf_3
      val2_3e(3) = sigma1_3*dsigma21

c     Third peak
      val3_3g(1) = x1_3 + dx3
      val3_3g(2) = amp3_3
      val3_3g(3) = sigma1_3*dsigma31
      val3_3e(1) = x1_3 + dx3
      val3_3e(2) = amp3_3*damperf_3
      val3_3e(3) = sigma1_3*dsigma31

c     Fourth peak
      val4_3g(1) = x1_3 + dx4
      val4_3g(2) = amp4_3
      val4_3g(3) = sigma4_3
      val4_3e(1) = x1_3 + dx4
      val4_3e(2) = amp4_3*damperf_3
      val4_3e(3) = sigma4_3

c     Fifth peak
      val5_3g(1) = x1_3 + dx5
      val5_3g(2) = amp4_3*damp45
      val5_3g(3) = sigma4_3*dsigma54
      val5_3e(1) = x1_3 + dx5
      val5_3e(2) = amp4_3*damp45*damperf_3
      val5_3e(3) = sigma4_3*dsigma54

c     Sixth peak
      val6_3g(1) = x1_3 + dx6
      val6_3g(2) = amp6_3
      val6_3g(3) = sigma1_3*dsigma61
      val6_3e(1) = x1_3 + dx6
      val6_3e(2) = amp6_3*damperf_3
      val6_3e(3) = sigma1_3*dsigma61

c     Polynomial background
      valp_3(1) = 0.
      valp_3(2) = a_3
      valp_3(3) = b_3
      valp_3(4) = c_3
      valp_3(5) = 0.
      valp_3(6) = 0.
      valp_3(7) = 0.
      valp_3(8) = 0.

      IF (j.EQ.1) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET2 = GAUSS_ERF(x,4,val1_1) +
     +        GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +        GAUSS_ERF(x,4,val4_1) + GAUSS_ERF(x,4,val5_1) +
     +        GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET2 = GAUSS_ERF(x,4,val1_2) +
     +        GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +        GAUSS_ERF(x,4,val4_2) + GAUSS_ERF(x,4,val5_2) +
     +        GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)
      ELSEIF (j.EQ.3) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET2 = GAUSS_ERF(x,4,val1_3) +
     +        GAUSS_ERF(x,4,val2_3) + GAUSS_ERF(x,4,val3_3) +
     +        GAUSS_ERF(x,4,val4_3) + GAUSS_ERF(x,4,val5_3) +
     +        GAUSS_ERF(x,4,val6_3) + POLY(x,8,valp_3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
           WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val4_1e),
     +			 ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +		 	 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val4_2e),
     +			 ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e)
         ELSEIF (j.EQ.3) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_3g),
     +           GAUSS(x,3,val2_3g),  GAUSS(x,3,val3_3g),
     +           GAUSS(x,3,val4_3g),  GAUSS(x,3,val5_3g),
     +           GAUSS(x,3,val6_3g),  POLY(x,8,valp_3),
     +			 ERFFCN(x,3,val1_3e), ERFFCN(x,3,val2_3e),
     +			 ERFFCN(x,3,val3_3e), ERFFCN(x,3,val4_3e),
     +			 ERFFCN(x,3,val5_3e), ERFFCN(x,3,val6_3e)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF
      ENDIF

	  RETURN
	  END


      FUNCTION SIX_GAUSS_ERF_FREESIG_POLY_SET3(X,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      INTEGER*4 j
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET3, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(4),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(4),val5_2(4),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
      REAL*8 valp_3(8), val1_3g(3),val2_3g(3),val3_3g(3)
      REAL*8 val4_3g(3),val5_3g(3),val6_3g(3)
      REAL*8 val1_3(4),val2_3(4),val3_3(4)
      REAL*8 val4_3(4),val5_3(4),val6_3(4)
      REAL*8 val1_3e(3),val2_3e(3),val3_3e(3)
      REAL*8 val4_3e(3),val5_3e(3),val6_3e(3)
      REAL*8 x1_1, x1_2, x1_3, dx2, dx3, dx4, dx5, dx6
      REAL*8 amp1_1, amp2_1, amp3_1, amp4_1, damp45, amp6_1, damperf_1
      REAL*8 amp1_2, amp2_2, amp3_2, amp4_2, amp6_2, damperf_2
      REAL*8 amp1_3, amp2_3, amp3_3, amp4_3, amp6_3, damperf_3
      REAL*8 sigma1_1, sigma1_2, sigma1_3
      REAL*8 sigma3_1, sigma3_2, sigma3_3
      REAL*8 sigma4_1, sigma4_2, sigma4_3
      REAL*8 dsigma21, dsigma54, dsigma61
      REAL*8 a_1, b_1, c_1, a_2, b_2, c_2, a_3, b_3, c_3
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
      sigma3_1   = val(15)
      sigma4_1   = val(16)
      dsigma54   = val(17)
      dsigma61   = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      b_1        = val(21)
      c_1        = val(22)
      x1_2       = val(23)
      amp1_2     = val(24)
      amp2_2     = val(25)
      amp3_2     = val(26)
      amp4_2     = val(27)
      amp6_2     = val(28)
      sigma1_2   = val(29)
      sigma3_2   = val(30)
      sigma4_2   = val(31)
      damperf_2  = val(32)
      a_2        = val(33)
      b_2        = val(34)
      c_2        = val(35)
      x1_3       = val(36)
      amp1_3     = val(37)
      amp2_3     = val(38)
      amp3_3     = val(39)
      amp4_3     = val(40)
      amp6_3     = val(41)
      sigma1_3   = val(42)
      sigma3_3   = val(43)
      sigma4_3   = val(44)
      damperf_3  = val(45)
      a_3        = val(46)
      b_3        = val(47)
      c_3        = val(48)


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
      val3_1(3) = sigma3_1
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

c     SECOND SPECTRUM

c     First (main) peak
      val1_2(1) = x1_2
      val1_2(2) = amp1_2
      val1_2(3) = sigma1_2
      val1_2(4) = damperf_2

c     Second peak
      val2_2(1) = x1_2 + dx2
      val2_2(2) = amp2_2
      val2_2(3) = sigma1_2*dsigma21
      val2_2(4) = damperf_2

c     Third peak
      val3_2(1) = x1_2 + dx3
      val3_2(2) = amp3_2
      val3_2(3) = sigma3_2
      val3_2(4) = damperf_2

c     Fourth peak
      val4_2(1) = x1_2 + dx4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4_2
      val4_2(4) = damperf_2

c     Fifth peak
      val5_2(1) = x1_2 + dx5
      val5_2(2) = amp4_2*damp45
      val5_2(3) = sigma4_2*dsigma54
      val5_2(4) = damperf_2

c     Sixth peak
      val6_2(1) = x1_2 + dx6
      val6_2(2) = amp6_2
      val6_2(3) = sigma1_2*dsigma61
      val6_2(4) = damperf_2

c     THIRD SPECTRUM

c     First (main) peak
      val1_3(1) = x1_3
      val1_3(2) = amp1_3
      val1_3(3) = sigma1_3
      val1_3(4) = damperf_3

c     Second peak
      val2_3(1) = x1_3 + dx2
      val2_3(2) = amp2_3
      val2_3(3) = sigma1_3*dsigma21
      val2_3(4) = damperf_3

c     Third peak
      val3_3(1) = x1_3 + dx3
      val3_3(2) = amp3_3
      val3_3(3) = sigma3_3
      val3_3(4) = damperf_3

c     Fourth peak
      val4_3(1) = x1_3 + dx4
      val4_3(2) = amp4_3
      val4_3(3) = sigma4_3
      val4_3(4) = damperf_3

c     Fifth peak
      val5_3(1) = x1_3 + dx5
      val5_3(2) = amp4_3*damp45
      val5_3(3) = sigma4_3*dsigma54
      val5_3(4) = damperf_3

c     Sixth peak
      val6_3(1) = x1_3 + dx6
      val6_3(2) = amp6_3
      val6_3(3) = sigma1_3*dsigma61
      val6_3(4) = damperf_3

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
      val3_1g(3) = sigma3_1
      val3_1e(1) = x1_1 + dx3
      val3_1e(2) = amp3_1*damperf_1
      val3_1e(3) = sigma3_1

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

c     SECOND SPECTRUM

c     First (main) peak
      val1_2g(1) = x1_2
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1_2
      val1_2e(1) = x1_2
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1_2

c     Second peak
      val2_2g(1) = x1_2 + dx2
      val2_2g(2) = amp2_2
      val2_2g(3) = sigma1_2*dsigma21
      val2_2e(1) = x1_2 + dx2
      val2_2e(2) = amp2_2*damperf_2
      val2_2e(3) = sigma1_2*dsigma21

c     Third peak
      val3_2g(1) = x1_2 + dx3
      val3_2g(2) = amp3_2
      val3_2g(3) = sigma3_2
      val3_2e(1) = x1_2 + dx3
      val3_2e(2) = amp3_2*damperf_2
      val3_2e(3) = sigma3_2

c     Fourth peak
      val4_2g(1) = x1_2 + dx4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4_2
      val4_2e(1) = x1_2 + dx4
      val4_2e(2) = amp4_2*damperf_2
      val4_2e(3) = sigma4_2

c     Fifth peak
      val5_2g(1) = x1_2 + dx5
      val5_2g(2) = amp4_2*damp45
      val5_2g(3) = sigma4_2*dsigma54
      val5_2e(1) = x1_2 + dx5
      val5_2e(2) = amp4_2*damp45*damperf_2
      val5_2e(3) = sigma4_2*dsigma54

c     Sixth peak
      val6_2g(1) = x1_2 + dx6
      val6_2g(2) = amp6_2
      val6_2g(3) = sigma1_2*dsigma61
      val6_2e(1) = x1_2 + dx6
      val6_2e(2) = amp6_2*damperf_2
      val6_2e(3) = sigma1_2*dsigma61

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = b_2
      valp_2(4) = c_2
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

c     THIRD SPECTRUM

c     First (main) peak
      val1_3g(1) = x1_3
      val1_3g(2) = amp1_3
      val1_3g(3) = sigma1_3
      val1_3e(1) = x1_3
      val1_3e(2) = amp1_3*damperf_3
      val1_3e(3) = sigma1_3

c     Second peak
      val2_3g(1) = x1_3 + dx2
      val2_3g(2) = amp2_3
      val2_3g(3) = sigma1_3*dsigma21
      val2_3e(1) = x1_3 + dx2
      val2_3e(2) = amp2_3*damperf_3
      val2_3e(3) = sigma1_3*dsigma21

c     Third peak
      val3_3g(1) = x1_3 + dx3
      val3_3g(2) = amp3_3
      val3_3g(3) = sigma3_3
      val3_3e(1) = x1_3 + dx3
      val3_3e(2) = amp3_3*damperf_3
      val3_3e(3) = sigma3_3

c     Fourth peak
      val4_3g(1) = x1_3 + dx4
      val4_3g(2) = amp4_3
      val4_3g(3) = sigma4_3
      val4_3e(1) = x1_3 + dx4
      val4_3e(2) = amp4_3*damperf_3
      val4_3e(3) = sigma4_3

c     Fifth peak
      val5_3g(1) = x1_3 + dx5
      val5_3g(2) = amp4_3*damp45
      val5_3g(3) = sigma4_3*dsigma54
      val5_3e(1) = x1_3 + dx5
      val5_3e(2) = amp4_3*damp45*damperf_3
      val5_3e(3) = sigma4_3*dsigma54

c     Sixth peak
      val6_3g(1) = x1_3 + dx6
      val6_3g(2) = amp6_3
      val6_3g(3) = sigma1_3*dsigma61
      val6_3e(1) = x1_3 + dx6
      val6_3e(2) = amp6_3*damperf_3
      val6_3e(3) = sigma1_3*dsigma61

c     Polynomial background
      valp_3(1) = 0.
      valp_3(2) = a_3
      valp_3(3) = b_3
      valp_3(4) = c_3
      valp_3(5) = 0.
      valp_3(6) = 0.
      valp_3(7) = 0.
      valp_3(8) = 0.

      IF (j.EQ.1) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET3 = GAUSS_ERF(x,4,val1_1) +
     +        GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +        GAUSS_ERF(x,4,val4_1) + GAUSS_ERF(x,4,val5_1) +
     +        GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET3 = GAUSS_ERF(x,4,val1_2) +
     +        GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +        GAUSS_ERF(x,4,val4_2) + GAUSS_ERF(x,4,val5_2) +
     +        GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)
      ELSEIF (j.EQ.3) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET3 = GAUSS_ERF(x,4,val1_3) +
     +        GAUSS_ERF(x,4,val2_3) + GAUSS_ERF(x,4,val3_3) +
     +        GAUSS_ERF(x,4,val4_3) + GAUSS_ERF(x,4,val5_3) +
     +        GAUSS_ERF(x,4,val6_3) + POLY(x,8,valp_3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
           WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val4_1e),
     +			 ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +		 	 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val4_2e),
     +			 ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e)
         ELSEIF (j.EQ.3) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_3g),
     +           GAUSS(x,3,val2_3g),  GAUSS(x,3,val3_3g),
     +           GAUSS(x,3,val4_3g),  GAUSS(x,3,val5_3g),
     +           GAUSS(x,3,val6_3g),  POLY(x,8,valp_3),
     +			 ERFFCN(x,3,val1_3e), ERFFCN(x,3,val2_3e),
     +			 ERFFCN(x,3,val3_3e), ERFFCN(x,3,val4_3e),
     +			 ERFFCN(x,3,val5_3e), ERFFCN(x,3,val6_3e)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF
      ENDIF

      RETURN
      END

	  FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS_SET(x,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
	  INTEGER*4 j
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(3),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(3),val5_2(3),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
	  REAL*8 x1, dx21, dx31, x4, dx54, dx61
      REAL*8 amp1_1, damp21, damp31, amp4_1, damp45, damp61, damperf_1
      REAL*8 amp1_2, amp4_2, damperf_2
      REAL*8 sigma1
      REAL*8 sigma4
      REAL*8 dsigma21, dsigma31, dsigma54, dsigma61
      REAL*8 a_1
      REAL*8 a_2
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1         = val(1)
      dx21       = val(2)
      dx31       = val(3)
      x4         = val(4)
      dx54       = val(5)
      dx61       = val(6)
      amp1_1     = val(7)
      damp21     = val(8)
      damp31     = val(9)
      amp4_1     = val(10)
      damp45     = val(11)
      damp61     = val(12)
      sigma1     = val(13)
      dsigma21   = val(14)
      dsigma31   = val(15)
      sigma4     = val(16)
      dsigma54   = val(17)
      dsigma61   = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      amp1_2     = val(21)
      amp4_2     = val(22)
      damperf_2  = val(23)
      a_2        = val(24)

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
      val5_1(1) = x4 + dx54
      val5_1(2) = amp4_1*damp45
      val5_1(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_1(1) = x1 + dx61
      val6_1(2) = amp1_1*damp61
      val6_1(3) = sigma1*dsigma61
      val6_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2(1) = x1
      val1_2(2) = amp1_2
      val1_2(3) = sigma1
      val1_2(4) = damperf_2

c     Second peak solid
      val2_2(1) = x1 + dx21
      val2_2(2) = amp1_2*damp21
      val2_2(3) = sigma1*dsigma21
      val2_2(4) = damperf_2

c     Third peak solid
      val3_2(1) = x1 + dx31
      val3_2(2) = amp1_2*damp31
      val3_2(3) = sigma1*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak gas
      val4_2(1) = x4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4

c     Fifth peak gas
      val5_2(1) = x4 + dx54
      val5_2(2) = amp4_2*damp45
      val5_2(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_2(1) = x1 + dx61
      val6_2(2) = amp1_2*damp61
      val6_2(3) = sigma1*dsigma61
      val6_2(4) = damperf_2

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
      val5_1g(1) = x4 + dx54
      val5_1g(2) = amp4_1*damp45
      val5_1g(3) = sigma4*dsigma54

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

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2g(1) = x1
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1
      val1_2e(1) = x1
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1

c     Second peak solid
      val2_2g(1) = x1 + dx21
      val2_2g(2) = amp1_2*damp21
      val2_2g(3) = sigma1*dsigma21
      val2_2e(1) = x1 + dx21
      val2_2e(2) = amp1_2*damp21*damperf_2
      val2_2e(3) = sigma1*dsigma21

c     Third peak solid
      val3_2g(1) = x1 + dx31
      val3_2g(2) = amp1_2*damp31
      val3_2g(3) = sigma1*dsigma31
      val3_2e(1) = x1 + dx31
      val3_2e(2) = amp1_2*damp31*damperf_2
      val3_2e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_2g(1) = x4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4

c     Fifth peak gas
      val5_2g(1) = x4 + dx54
      val5_2g(2) = amp4_2*damp45
      val5_2g(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_2g(1) = x1 + dx61
      val6_2g(2) = amp1_2*damp61
      val6_2g(3) = sigma1*dsigma61
      val6_2e(1) = x1 + dx61
      val6_2e(2) = amp1_2*damp61*damperf_2
      val6_2e(3) = sigma1*dsigma61

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = 0.
      valp_2(4) = 0.
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

      IF (j.EQ.1) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_SET = GAUSS_ERF(x,4,val1_1) +
     +      GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +      GAUSS(x,3,val4_1) + GAUSS(x,3,val5_1) +
     +      GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_SET = GAUSS_ERF(x,4,val1_2) +
     +      GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +      GAUSS(x,3,val4_2) + GAUSS(x,3,val5_2) +
     +      GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)

	  ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files'
         WRITE(*,*) j
      END IF

c     Save the different components
      IF(plot) THEN
           IF (j.EQ.1) THEN
			WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val6_1e)
	       ELSEIF (j.EQ.2) THEN
			WRITE(30,*) x, GAUSS(x,4,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +			 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val6_2e)
		   ELSE
			WRITE(*,*) 'Too many spectra! Check your input files'
		   END IF
      ENDIF

	  RETURN
	  END


  	  FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET(x,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
	  INTEGER*4 j
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(3),val5_2(4),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
	  REAL*8 x1, dx21, dx31, x4, dx51, dx61
      REAL*8 amp1_1, damp21, damp31, amp4_1, damp51, damp61, damperf_1
      REAL*8 amp1_2, amp4_2, damperf_2
      REAL*8 sigma1
      REAL*8 sigma4
      REAL*8 dsigma21, dsigma31, dsigma51, dsigma61
      REAL*8 a_1
      REAL*8 a_2
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
      amp1_2     = val(21)
      amp4_2     = val(22)
      damperf_2  = val(23)
      a_2        = val(24)

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

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2(1) = x1
      val1_2(2) = amp1_2
      val1_2(3) = sigma1
      val1_2(4) = damperf_2

c     Second peak solid
      val2_2(1) = x1 + dx21
      val2_2(2) = amp1_2*damp21
      val2_2(3) = sigma1*dsigma21
      val2_2(4) = damperf_2

c     Third peak solid
      val3_2(1) = x1 + dx31
      val3_2(2) = amp1_2*damp31
      val3_2(3) = sigma1*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak gas
      val4_2(1) = x4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4

c     Fifth peak gas
      val5_2(1) = x1 + dx51
      val5_2(2) = amp1_2*damp51
      val5_2(3) = sigma1*dsigma51
      val5_2(4) = damperf_2

c     Sixth peak solid
      val6_2(1) = x1 + dx61
      val6_2(2) = amp1_2*damp61
      val6_2(3) = sigma1*dsigma61
      val6_2(4) = damperf_2

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

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2g(1) = x1
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1
      val1_2e(1) = x1
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1

c     Second peak solid
      val2_2g(1) = x1 + dx21
      val2_2g(2) = amp1_2*damp21
      val2_2g(3) = sigma1*dsigma21
      val2_2e(1) = x1 + dx21
      val2_2e(2) = amp1_2*damp21*damperf_2
      val2_2e(3) = sigma1*dsigma21

c     Third peak solid
      val3_2g(1) = x1 + dx31
      val3_2g(2) = amp1_2*damp31
      val3_2g(3) = sigma1*dsigma31
      val3_2e(1) = x1 + dx31
      val3_2e(2) = amp1_2*damp31*damperf_2
      val3_2e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_2g(1) = x4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4

c     Fifth peak gas
      val5_2g(1) = x1 + dx51
      val5_2g(2) = amp1_2*damp51
      val5_2g(3) = sigma1*dsigma51
      val5_2e(1) = x1 + dx51
      val5_2e(2) = amp1_2*damp51*damperf_2
      val5_2e(3) = sigma1*dsigma51

c     Sixth peak solid
      val6_2g(1) = x1 + dx61
      val6_2g(2) = amp1_2*damp61
      val6_2g(3) = sigma1*dsigma61
      val6_2e(1) = x1 + dx61
      val6_2e(2) = amp1_2*damp61*damperf_2
      val6_2e(3) = sigma1*dsigma61

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = 0.
      valp_2(4) = 0.
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

      IF (j.EQ.1) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET = GAUSS_ERF(x,4,val1_1) +
     +      GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +      GAUSS(x,3,val4_1) + GAUSS_ERF(x,3,val5_1) +
     +      GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET = GAUSS_ERF(x,4,val1_2) +
     +      GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +      GAUSS(x,3,val4_2) + GAUSS_ERF(x,3,val5_2) +
     +      GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)

	  ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files'
         WRITE(*,*) j
      END IF

c     Save the different components
      IF(plot) THEN
           IF (j.EQ.1) THEN
			WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
	       ELSEIF (j.EQ.2) THEN
			WRITE(30,*) x, GAUSS(x,4,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +			 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e)
		   ELSE
			WRITE(*,*) 'Too many spectra! Check your input files'
		   END IF
      ENDIF

	  RETURN
	  END


      FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET(x,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
	  INTEGER*4 j
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3),val7_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(4),val6_1(4), val7_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3),val7_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3),val7_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(3),val5_2(4),val6_2(4),val7_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3),val7_2e(3)
	  REAL*8 x1, dx21, dx31, x4, dx51, dx61, dx71
      REAL*8 amp1_1, damp21, damp31, amp4_1, damp51, damp61, damp71
	  REAL*8 damperf_1
      REAL*8 amp1_2, amp4_2, damperf_2
      REAL*8 sigma1
      REAL*8 sigma4, sigma7
      REAL*8 dsigma21, dsigma31, dsigma51, dsigma61
      REAL*8 a_1
      REAL*8 a_2
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
      dx71       = val(7)
      amp1_1     = val(8)
      damp21     = val(9)
      damp31     = val(10)
      amp4_1     = val(11)
      damp51     = val(12)
      damp61     = val(13)
      damp71     = val(14)
      sigma1     = val(15)
      dsigma21   = val(16)
      dsigma31   = val(17)
      sigma4     = val(18)
      dsigma51   = val(19)
      dsigma61   = val(20)
      sigma7     = val(21)
      damperf_1  = val(22)
      a_1        = val(23)
      amp1_2     = val(24)
      amp4_2     = val(25)
      damperf_2  = val(26)
      a_2        = val(27)

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

c     Seventh peak solid
      val7_1(1) = x1 + dx71
      val7_1(2) = amp1_1*damp71
      val7_1(3) = sigma7
      val7_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2(1) = x1
      val1_2(2) = amp1_2
      val1_2(3) = sigma1
      val1_2(4) = damperf_2

c     Second peak solid
      val2_2(1) = x1 + dx21
      val2_2(2) = amp1_2*damp21
      val2_2(3) = sigma1*dsigma21
      val2_2(4) = damperf_2

c     Third peak solid
      val3_2(1) = x1 + dx31
      val3_2(2) = amp1_2*damp31
      val3_2(3) = sigma1*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak gas
      val4_2(1) = x4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4

c     Fifth peak gas
      val5_2(1) = x1 + dx51
      val5_2(2) = amp1_2*damp51
      val5_2(3) = sigma1*dsigma51
      val5_2(4) = damperf_2

c     Sixth peak solid
      val6_2(1) = x1 + dx61
      val6_2(2) = amp1_2*damp61
      val6_2(3) = sigma1*dsigma61
      val6_2(4) = damperf_2

c     Seventh peak solid
      val7_2(1) = x1 + dx71
      val7_2(2) = amp1_2*damp71
      val7_2(3) = sigma7
      val7_2(4) = damperf_2

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

c     Seventh peak solid
      val7_1g(1) = x1 + dx71
      val7_1g(2) = amp1_1*damp71
      val7_1g(3) = sigma7
      val7_1e(1) = x1 + dx71
      val7_1e(2) = amp1_1*damp71*damperf_1
      val7_1e(3) = sigma7

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = 0.
      valp_1(4) = 0.
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2g(1) = x1
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1
      val1_2e(1) = x1
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1

c     Second peak solid
      val2_2g(1) = x1 + dx21
      val2_2g(2) = amp1_2*damp21
      val2_2g(3) = sigma1*dsigma21
      val2_2e(1) = x1 + dx21
      val2_2e(2) = amp1_2*damp21*damperf_2
      val2_2e(3) = sigma1*dsigma21

c     Third peak solid
      val3_2g(1) = x1 + dx31
      val3_2g(2) = amp1_2*damp31
      val3_2g(3) = sigma1*dsigma31
      val3_2e(1) = x1 + dx31
      val3_2e(2) = amp1_2*damp31*damperf_2
      val3_2e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_2g(1) = x4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4

c     Fifth peak gas
      val5_2g(1) = x1 + dx51
      val5_2g(2) = amp1_2*damp51
      val5_2g(3) = sigma1*dsigma51
      val5_2e(1) = x1 + dx51
      val5_2e(2) = amp1_2*damp51*damperf_2
      val5_2e(3) = sigma1*dsigma51

c     Sixth peak solid
      val6_2g(1) = x1 + dx61
      val6_2g(2) = amp1_2*damp61
      val6_2g(3) = sigma1*dsigma61
      val6_2e(1) = x1 + dx61
      val6_2e(2) = amp1_2*damp61*damperf_2
      val6_2e(3) = sigma1*dsigma61

c     Seventh peak solid
      val7_2g(1) = x1 + dx71
      val7_2g(2) = amp1_2*damp71
      val7_2g(3) = sigma7
      val7_2e(1) = x1 + dx71
      val7_2e(2) = amp1_2*damp71*damperf_2
      val7_2e(3) = sigma7

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = 0.
      valp_2(4) = 0.
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

      IF (j.EQ.1) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET = GAUSS_ERF(x,4,val1_1) +
     +      GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +      GAUSS(x,3,val4_1) + GAUSS_ERF(x,4,val5_1) +
     +      GAUSS_ERF(x,4,val6_1) + GAUSS_ERF(x,4,val7_1) +
     +      POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET = GAUSS_ERF(x,4,val1_2) +
     +      GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +      GAUSS(x,3,val4_2) + GAUSS_ERF(x,4,val5_2) +
     +      GAUSS_ERF(x,4,val6_2) + GAUSS_ERF(x,4,val7_2) +
     +      POLY(x,8,valp_2)
      ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files'
         WRITE(*,*) j
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
            WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),GAUSS(x,3,val7_1g),
     +           POLY(x,8,valp_1), ERFFCN(x,3,val1_1e),
     +           ERFFCN(x,3,val2_1e), ERFFCN(x,3,val3_1e),
     +           ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e),
     +           ERFFCN(x,3,val7_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,4,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  GAUSS(x,3,val7_2g),
     +           POLY(x,8,valp_2), ERFFCN(x,3,val1_2e),
     +           ERFFCN(x,3,val2_2e), ERFFCN(x,3,val3_2e),
     +           ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e),
     +           ERFFCN(x,3,val7_2e)
         ELSE
            WRITE(*,*) 'Too many spectra! Check your input files'
         END IF
      ENDIF

      RETURN
      END
      FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET(x,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
	  INTEGER*4 j
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3),val7_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(3),val6_1(4), val7_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val6_1e(3),val7_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3),val7_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(3),val5_2(3),val6_2(4),val7_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val6_2e(3),val7_2e(3)
	  REAL*8 x1, dx21, dx31, x4, dx54, dx61, dx71
      REAL*8 amp1_1, damp21, damp31, amp4_1, damp54, damp61, damp71
	  REAL*8 damperf_1
      REAL*8 amp1_2, amp4_2, damperf_2
      REAL*8 sigma1
      REAL*8 sigma4, sigma7
      REAL*8 dsigma21, dsigma31, dsigma54, dsigma61
      REAL*8 a_1
      REAL*8 a_2
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1         = val(1)
      dx21       = val(2)
      dx31       = val(3)
      x4         = val(4)
      dx54       = val(5)
      dx61       = val(6)
      dx71       = val(7)
      amp1_1     = val(8)
      damp21     = val(9)
      damp31     = val(10)
      amp4_1     = val(11)
      damp54    = val(12)
      damp61     = val(13)
      damp71     = val(14)
      sigma1     = val(15)
      dsigma21   = val(16)
      dsigma31   = val(17)
      sigma4     = val(18)
      dsigma54   = val(19)
      dsigma61   = val(20)
      sigma7     = val(21)
      damperf_1  = val(22)
      a_1        = val(23)
      amp1_2     = val(24)
      amp4_2     = val(25)
      damperf_2  = val(26)
      a_2        = val(27)

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
      val5_1(1) = x4 + dx54
      val5_1(2) = amp4_1*damp54
      val5_1(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_1(1) = x1 + dx61
      val6_1(2) = amp1_1*damp61
      val6_1(3) = sigma1*dsigma61
      val6_1(4) = damperf_1

c     Seventh peak solid
      val7_1(1) = x1 + dx71
      val7_1(2) = amp1_1*damp71
      val7_1(3) = sigma7
      val7_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2(1) = x1
      val1_2(2) = amp1_2
      val1_2(3) = sigma1
      val1_2(4) = damperf_2

c     Second peak solid
      val2_2(1) = x1 + dx21
      val2_2(2) = amp1_2*damp21
      val2_2(3) = sigma1*dsigma21
      val2_2(4) = damperf_2

c     Third peak solid
      val3_2(1) = x1 + dx31
      val3_2(2) = amp1_2*damp31
      val3_2(3) = sigma1*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak gas
      val4_2(1) = x4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4

c     Fifth peak gas
      val5_2(1) = x4 + dx54
      val5_2(2) = amp4_2*damp54
      val5_2(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_2(1) = x1 + dx61
      val6_2(2) = amp1_2*damp61
      val6_2(3) = sigma1*dsigma61
      val6_2(4) = damperf_2

c     Seventh peak solid
      val7_2(1) = x1 + dx71
      val7_2(2) = amp1_2*damp71
      val7_2(3) = sigma7
      val7_2(4) = damperf_2

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
      val5_1g(1) = x4 + dx54
      val5_1g(2) = amp4_1*damp54
      val5_1g(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_1g(1) = x1 + dx61
      val6_1g(2) = amp1_1*damp61
      val6_1g(3) = sigma1*dsigma61
      val6_1e(1) = x1 + dx61
      val6_1e(2) = amp1_1*damp61*damperf_1
      val6_1e(3) = sigma1*dsigma61

c     Seventh peak solid
      val7_1g(1) = x1 + dx71
      val7_1g(2) = amp1_1*damp71
      val7_1g(3) = sigma7
      val7_1e(1) = x1 + dx71
      val7_1e(2) = amp1_1*damp71*damperf_1
      val7_1e(3) = sigma7

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = 0.
      valp_1(4) = 0.
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2g(1) = x1
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1
      val1_2e(1) = x1
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1

c     Second peak solid
      val2_2g(1) = x1 + dx21
      val2_2g(2) = amp1_2*damp21
      val2_2g(3) = sigma1*dsigma21
      val2_2e(1) = x1 + dx21
      val2_2e(2) = amp1_2*damp21*damperf_2
      val2_2e(3) = sigma1*dsigma21

c     Third peak solid
      val3_2g(1) = x1 + dx31
      val3_2g(2) = amp1_2*damp31
      val3_2g(3) = sigma1*dsigma31
      val3_2e(1) = x1 + dx31
      val3_2e(2) = amp1_2*damp31*damperf_2
      val3_2e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_2g(1) = x4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4

c     Fifth peak gas
      val5_2g(1) = x4 + dx54
      val5_2g(2) = amp4_2*damp54
      val5_2g(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_2g(1) = x1 + dx61
      val6_2g(2) = amp1_2*damp61
      val6_2g(3) = sigma1*dsigma61
      val6_2e(1) = x1 + dx61
      val6_2e(2) = amp1_2*damp61*damperf_2
      val6_2e(3) = sigma1*dsigma61

c     Seventh peak solid
      val7_2g(1) = x1 + dx71
      val7_2g(2) = amp1_2*damp71
      val7_2g(3) = sigma7
      val7_2e(1) = x1 + dx71
      val7_2e(2) = amp1_2*damp71*damperf_2
      val7_2e(3) = sigma7

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = 0.
      valp_2(4) = 0.
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

      IF (j.EQ.1) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET = GAUSS_ERF(x,4,val1_1) +
     +      GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +      GAUSS(x,3,val4_1) + GAUSS(x,3,val5_1) +
     +      GAUSS_ERF(x,4,val6_1) + GAUSS_ERF(x,4,val7_1) +
     +      POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET = GAUSS_ERF(x,4,val1_2) +
     +      GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +      GAUSS(x,3,val4_2) + GAUSS(x,3,val5_2) +
     +      GAUSS_ERF(x,4,val6_2) + GAUSS_ERF(x,4,val7_2) +
     +      POLY(x,8,valp_2)
      ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files'
         WRITE(*,*) j
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
            WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),GAUSS(x,3,val7_1g),
     +           POLY(x,8,valp_1), ERFFCN(x,3,val1_1e),
     +           ERFFCN(x,3,val2_1e), ERFFCN(x,3,val3_1e),
     +           ERFFCN(x,3,val6_1e), ERFFCN(x,3,val7_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,4,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  GAUSS(x,3,val7_2g),
     +           POLY(x,8,valp_2), ERFFCN(x,3,val1_2e),
     +           ERFFCN(x,3,val2_2e), ERFFCN(x,3,val3_2e),
     +           ERFFCN(x,3,val6_2e), ERFFCN(x,3,val7_2e)
         ELSE
            WRITE(*,*) 'Too many spectra! Check your input files'
         END IF
      ENDIF

      RETURN
      END


c _______________________________________________________________________________________________


      FUNCTION ROCKING_CURVE_SET(X,npar,val,j)
c     Rocking curve with s and p polarization extracted from simulations
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar)
      REAL*8 ROCKING_CURVE_SET, x, y_s_par, y_p_par,  y_s_apar, y_p_apar
      REAL*8 amp_par, amp_apar, damp_sp, x0_s_par, dx0_p_par, x0_s_apar
      REAL*8 dx0_sp, bg_par, bg_apar
c     Interpolation variables
      INTEGER*4 k, nn_s_par, nn_p_par, nn_s_apar, nn_p_apar
      INTEGER*4 ier_s,ier_p, nest
      PARAMETER (nest=1000)
      REAL*8 t_s_par(nest),c_s_par(nest),t_p_par(nest),c_p_par(nest)
      REAL*8 t_s_apar(nest),c_s_apar(nest),t_p_apar(nest),c_p_apar(nest)
      COMMON /rocking_set/ t_s_par, t_p_par, t_s_apar, t_p_apar,
     +     c_s_par, c_p_par, c_s_apar, c_p_apar,k,
     +     nn_s_par, nn_p_par, nn_s_apar, nn_p_apar
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot


c     Parallel spectrum
      amp_par  = val(1)
      damp_sp  = val(3)
      x0_s_par = val(4)
      dx0_sp   = val(6)
      bg_par   = val(7)

c     Antiparallel spectrum
      amp_apar  = val(2)
      damp_sp   = val(3)
      x0_s_apar = val(5)
      dx0_sp    = val(6)
      bg_apar   = val(8)

      IF (j.EQ.1) THEN
c     Parallel component
c     s polarization contribution
         CALL SPLEV(t_s_par,nn_s_par,c_s_par,k,
     +        x-x0_s_par,y_s_par,1,1,ier_s)

c     p polarization contribution
         CALL SPLEV(t_p_par,nn_p_par,c_p_par,k,
     +        x-(x0_s_par+dx0_sp),y_p_par,1,1,ier_p)

         ROCKING_CURVE_SET =
     +        amp_par*((1-damp_sp)*y_s_par+damp_sp*y_p_par) + bg_par

      ELSEIF (j.EQ.2) THEN
c     Antiparallel component
c     s polarization contribution
         CALL SPLEV(t_s_apar,nn_s_apar,c_s_apar,k,
     +        x-x0_s_apar,y_s_apar,1,1,ier_s)

c     p polarization contribution
         CALL SPLEV(t_p_apar,nn_p_apar,c_p_apar,k,
     +        x-(x0_s_apar+dx0_sp),y_p_apar,1,1,ier_p)

         ROCKING_CURVE_SET =
     +        amp_apar*((1-damp_sp)*y_s_apar+damp_sp*y_p_apar) + bg_apar
      ENDIF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
            ROCKING_CURVE_SET =
     +           amp_par*((1-damp_sp)*y_s_par+damp_sp*y_p_par) + bg_par
            WRITE(30,*) x, ROCKING_CURVE_SET,
     +           amp_par*((1-damp_sp)*y_s_par),
     +           amp_par*(damp_sp*y_p_par), bg_par

         ELSEIF (j.EQ.2) THEN
            ROCKING_CURVE_SET =
     +        amp_apar*((1-damp_sp)*y_s_apar+damp_sp*y_p_apar) + bg_apar
            WRITE(30,*) x, ROCKING_CURVE_SET,
     +           amp_apar*((1-damp_sp)*y_s_apar),
     +           amp_apar*(damp_sp*y_p_apar), bg_apar
         END IF
      ENDIF

      RETURN
      END

c ##############################################################################################
