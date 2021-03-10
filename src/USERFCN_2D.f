c     Automatic Time-stamp: <Last changed by martino on Tuesday 07 April 2020 at CEST 18:34:52>
c################################### USERFCN_2D DEFINITION #####################################



      FUNCTION USERFCN_2D(x,y,npar,val,funcname)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 USERFCN_2D, GAUSS, GAUSS_BG, DOUBLE_GAUSS_BG, DOUBLET_GAUSS_BG
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
      REAL*8 SOMBRERO, SOMBRERO_BG, LANDAU, POLY_EVENX, FABIAN
      REAL*8 x, y
      CHARACTER*64 funcname

c     Choose your model (see below for definition)
      IF(funcname.EQ.'GAUSS') THEN
         USERFCN_2D = GAUSS(x,y,npar,val)
      ELSE IF(funcname.EQ.'GAUSS_BG') THEN
         USERFCN_2D = GAUSS_BG(x,y,npar,val)
      ELSE IF(funcname.EQ.'SOMBRERO') THEN
         USERFCN_2D = SOMBRERO(x,y,npar,val)
      ELSE IF(funcname.EQ.'SOMBRERO_BG') THEN
         USERFCN_2D = SOMBRERO_BG(x,y,npar,val)
      ELSE IF(funcname.EQ.'LANDAU') THEN
         USERFCN_2D = LANDAU(x,y,npar,val)
      ELSE IF(funcname.EQ.'POLY_EVENX') THEN
         USERFCN_2D = POLY_EVENX(x,y,npar,val)
      ELSE IF(funcname.EQ.'FABIAN') THEN
         USERFCN_2D = FABIAN(x,y,npar,val)
      ELSE
         WRITE(*,*) 'Error in the function name def. in USERFCN_2D'
         WRITE(*,*) 'Check in the manual and in the input.dat file'
         STOP
      END IF

      RETURN
      END


c################################### LINESHAPE DEFINITIONS #####################################

      FUNCTION GAUSS(X,Y,npar,val)
c     Normalized Gaussian distribution
c     The value of 'amp' is the value of the surface-->volume below the curve-->surface

      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 GAUSS, x, y ! 2D: added y
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, y0, amp, sigmax, sigmay, aux1, aux2, norm  ! 2D: added y and sigma

      x0    = val(1)
      y0    = val(2)
      amp   = val(3)                 ! 2D: added variables. ASK 
      sigmax= val(4)
      sigmay= val(5)
      aux1  = -(x-x0)**2/(2*sigmax**2)
      aux2  = -(y-y0)**2/(2*sigmay**2)
      norm  = amp/(2*pi*sigmax*sigmay)
c     Test of under of underflow first
      IF(ABS(aux1+aux2).LT.700) THEN   ! 2D: added. 
         GAUSS = norm*dexp(aux1+aux2)  ! Corrected normalisation. amp is now a volume 
      ELSE
         GAUSS = 0.d0
      END IF

      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION GAUSS_BG(X,Y,npar,val)
c     Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(5)        ! 2D: Changed 3->5
      REAL*8 GAUSS, GAUSS_BG, x, y  	! 2D: Added y
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, y0, amp, sigmax, sigmay, bg  ! 2D: Added y
      LOGICAL plot
      COMMON /func_plot/ plot

      bg    = val(1)
      x0    = val(2)
      y0    = val(3)
      amp   = val(4)                 ! 2D: added variables. ASK 
      sigmax= val(5)
      sigmay= val(6)

c     for the pure gauss peak
      val1(1) = x0
      val1(2) = y0
      val1(3) = amp                 ! 2D: added variables
      val1(4) = sigmax
      val1(5) = sigmay
      
      GAUSS_BG = GAUSS(x,y,5,val1) + bg   ! 2D: Changed 3->5

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, GAUSS_BG, GAUSS(x,y,5,val1), bg ! 2D: added y Changed 3->5
      END IF

      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION LANDAU(X,Y,npar,val)
c     Landau-like potential curve with phase transition at y=y0.
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)    
      REAL*8 LANDAU, x, y
      REAL*8 a, b, c, d, y0
      LOGICAL plot
      COMMON /func_plot/ plot


      a    = val(1)
      b    = val(2)
      c    = val(3)
      d    = val(4)
      y0   = val(5)                
      
      LANDAU = a*x**6+b*x**4+c*(y-y0)*x**2+d


      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION SOMBRERO(X,Y,npar,val)
c     Higgs-like mexican hat (sombrero) potential.
c     Spontaneous symetry breaking happens at y=y0
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)    
      REAL*8 SOMBRERO, x, y
      REAL*8 x0, y0, amp, sx, sy
      LOGICAL plot
      COMMON /func_plot/ plot


      x0    = val(1)
      y0    = val(2)
      amp   = val(3)                
      sx    = val(4)
      sy    = val(5)
      

      SOMBRERO = amp*((sx*(x-x0))**2-sy*(y-y0))**2


      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION SOMBRERO_BG(X,Y,npar,val)
c     Higgs-like mexican hat (sombrero) potential plus background.
c     Spontaneous symetry breaking happens at y=y0
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(5)      
      REAL*8 SOMBRERO, SOMBRERO_BG,x, y
      REAL*8 x0, y0, amp, sx, sy, bg
      LOGICAL plot
      COMMON /func_plot/ plot


      bg    = val(1)
      x0    = val(2)
      y0    = val(3)
      amp   = val(4)                
      sx    = val(5)
      sy    = val(6)

c     for the pure sombrero
      val1(1) = x0
      val1(2) = y0
      val1(3) = amp
      val1(4) = sx
      val1(5) = sy

      
      SOMBRERO_BG = SOMBRERO(x,y,5,val1)+bg


      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION POLY_EVENX(X,Y,npar,val)
c     Polynomial on x^n*y^m with n even and n,m<=4
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)      
      REAL*8 POLY_EVENX, x, y
      REAL*8 y_0, y_2, y_4, y_6
      REAL*8 y0, yc, c44, c43, c42, c41, c40
      REAL*8 c24, c23, c22, c21, c20
      REAL*8 c04, c03, c02, c01, c00
      REAL*8 c64, c63, c62, c61, c60
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      c00   = val(2)
      c01   = val(3)
      c02   = val(4)
      c03   = val(5)
      c04   = val(6)      
      c20   = val(7)
      c21   = val(8)
      c22   = val(9)
      c23   = val(10)
      c24   = val(11)
      c40   = val(12)
      c41   = val(13)
      c42   = val(14)
      c43   = val(15)
      c44   = val(16)
      c60   = val(17)
      c61   = val(18)
      c62   = val(19)
      c63   = val(20)
      c64   = val(21)


      yc=y-y0
      y_6=c64*yc**4+c63*yc**3+c62*yc**2+c41*yc+c60
      y_4=c44*yc**4+c43*yc**3+c42*yc**2+c41*yc+c40
      y_2=c24*yc**4+c23*yc**3+c22*yc**2+c21*yc+c20
      y_0=c04*yc**4+c03*yc**3+c02*yc**2+c01*yc+c00
      
      POLY_EVENX =x**6*y_6+x**4*y_4+x**2*y_2+y_0

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, POLY_EVENX 
      END IF



      RETURN
      END




c _______________________________________________________________________________________________

      FUNCTION FABIAN(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr 
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)      
      REAL*8 FABIAN, x, y
      REAL*8 amp1,amp2, x0, yc
      REAL*8 y0, q, a
      REAL*8 r, N, s, bg, N0
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      q     = val(2)
      a     = val(3)
      r     = val(4)
      N     = val(5)
      s     = val(6)
      bg    = val(7)
      N0    = val(8)
      
      yc     = y-y0
      amp1   = N
      amp2   = N
      x0     = a*SQRT(-yc**r)	

      IF(yc.LE.0) THEN
         FABIAN=amp1*((x-x0)*(x+x0))**(2*q)+bg
      ELSE
         FABIAN=amp2*(x**2)**(2*q)+bg
      END IF
         

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, FABIAN 
      END IF



      RETURN
      END





c ##############################################################################################
