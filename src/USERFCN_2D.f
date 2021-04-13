c     Automatic Time-stamp: <Last changed by martino on Tuesday 07 April 2020 at CEST 18:34:52>
c################################### USERFCN_2D DEFINITION #####################################



      FUNCTION USERFCN_2D(x,y,npar,val,funcname)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 USERFCN_2D, GAUSS_2D, GAUSS_BG_2D
      REAL*8 SOMBRERO_2D, SOMBRERO_BG_2D, LANDAU_2D, POLY_EVENX_2D
      REAL*8 FABIAN_2D, MOD_FABIAN_2D, POLY_2D, POLY_MORE_2D
      REAL*8 HAMILTONIAN_XY_2D, HAMILTONIAN_XQ_2D, R_POT_2D
      REAL*8 R_LJ_2D, POWER_FRAC_2D, DOUBLE_MORSE_2D, QT_2D
      REAL*8 REAL_ENERGY_XY_2D
      REAL*8 x, y
      CHARACTER*64 funcname

c     Choose your model (see below for definition)
      IF(funcname.EQ.'GAUSS_2D') THEN
         USERFCN_2D = GAUSS_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'GAUSS_BG_2D') THEN
         USERFCN_2D = GAUSS_BG_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'SOMBRERO_2D') THEN
         USERFCN_2D = SOMBRERO_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'SOMBRERO_BG_2D') THEN
         USERFCN_2D = SOMBRERO_BG_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'LANDAU_2D') THEN
         USERFCN_2D = LANDAU_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'POLY_EVENX_2D') THEN
         USERFCN_2D = POLY_EVENX_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'FABIAN_2D') THEN
         USERFCN_2D = FABIAN_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'MOD_FABIAN_2D') THEN
         USERFCN_2D = MOD_FABIAN_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'POLY_2D') THEN
         USERFCN_2D = POLY_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'POLY_MORE_2D') THEN
         USERFCN_2D = POLY_MORE_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'HAMILTONIAN_XY_2D') THEN
         USERFCN_2D = HAMILTONIAN_XY_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'HAMILTONIAN_XQ_2D') THEN
         USERFCN_2D = HAMILTONIAN_XQ_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'R_POT_2D') THEN
         USERFCN_2D = R_POT_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'R_LJ_2D') THEN
         USERFCN_2D = R_LJ_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'POWER_FRAC_2D') THEN
         USERFCN_2D = POWER_FRAC_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'DOUBLE_MORSE_2D') THEN
         USERFCN_2D = DOUBLE_MORSE_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'QT_2D') THEN
         USERFCN_2D = QT_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'REAL_ENERGY_XY_2D') THEN
         USERFCN_2D = REAL_ENERGY_XY_2D(x,y,npar,val)





      ELSE
         WRITE(*,*) 'Error in the function name def. in USERFCN_2D'
         WRITE(*,*) 'Check in the manual and in the input.dat file'
         STOP
      END IF

      RETURN
      END


c################################### LINESHAPE DEFINITIONS #####################################

      FUNCTION GAUSS_2D(X,Y,npar,val)
c     Normalized Gaussian distribution
c     The value of 'amp' is the value of the surface-->volume below the curve-->surface

      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 GAUSS_2D, x, y ! 2D: added y
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
         GAUSS_2D = norm*dexp(aux1+aux2)  ! Corrected normalisation. amp is now a volume
      ELSE
         GAUSS_2D = 0.d0
      END IF

      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION GAUSS_BG_2D(X,Y,npar,val)
c     Normalized Gaussian distribution plus background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(5)        ! 2D: Changed 3->5
      REAL*8 GAUSS_2D, GAUSS_BG_2D, x, y  	! 2D: Added y
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

      GAUSS_BG_2D = GAUSS_2D(x,y,5,val1) + bg   ! 2D: Changed 3->5

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, GAUSS_BG_2D, GAUSS_2D(x,y,5,val1), bg ! 2D: added y Changed 3->5
      END IF

      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION LANDAU_2D(X,Y,npar,val)
c     Landau-like potential curve with phase transition at y=y0.
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 LANDAU_2D, x, y
      REAL*8 a, b, c, d, y0
      LOGICAL plot
      COMMON /func_plot/ plot


      a    = val(1)
      b    = val(2)
      c    = val(3)
      d    = val(4)
      y0   = val(5)

      LANDAU_2D = a*x**6+b*x**4+c*(y-y0)*x**2+d


      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION SOMBRERO_2D(X,Y,npar,val)
c     Higgs-like mexican hat (sombrero) potential.
c     Spontaneous symetry breaking happens at y=y0
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 SOMBRERO_2D, x, y
      REAL*8 x0, y0, amp, sx, sy
      LOGICAL plot
      COMMON /func_plot/ plot


      x0    = val(1)
      y0    = val(2)
      amp   = val(3)
      sx    = val(4)
      sy    = val(5)


      SOMBRERO_2D = amp*((sx*(x-x0))**2-sy*(y-y0))**2


      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION SOMBRERO_BG_2D(X,Y,npar,val)
c     Higgs-like mexican hat (sombrero) potential plus background.
c     Spontaneous symetry breaking happens at y=y0
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar), val1(5)
      REAL*8 SOMBRERO_2D, SOMBRERO_BG_2D,x, y
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


      SOMBRERO_BG_2D = SOMBRERO_2D(x,y,5,val1)+bg


      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION POLY_2D(X,Y,npar,val)
c     
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 POLY_2D, x, y
      REAL*8 x0, y0
      REAL*8 xc, yc, t0, t1, t2
      REAL*8 c20, c11, c10, c02, c01, c00
      LOGICAL plot
      COMMON /func_plot/ plot


      x0    = val(1)
      y0    = val(2)
      c00   = val(3)
      c10   = val(4)
      c01   = val(5)
      c20   = val(6)
      c11   = val(7)
      c02   = val(8)

      yc = y-y0
      xc = x-x0
      t0 = c00
      t1 = c10*xc+c01*yc
      t2 = c20*xc**2+c11*xc*yc+c02*yc**2


      POLY_2D =t0 + t1+ t2

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, POLY_2D
      END IF



      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION POLY_MORE_2D(X,Y,npar,val)
c     
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 POLY_MORE_2D, x, y
      REAL*8 x0, y0
      REAL*8 xc, yc, t0, t1, t2, t3, t4, t5
      REAL*8 c20, c11, c10, c02, c01, c00, c41
      REAL*8 c22, c12, c21, c40, c04, c31, c32, c23

      LOGICAL plot
      COMMON /func_plot/ plot


      x0    = val(1)
      y0    = val(2)
      c00   = val(3)
      c10   = val(4)
      c01   = val(5)
      c20   = val(6)
      c11   = val(7)
      c02   = val(8)
      c12   = val(9)
      c21   = val(10)
      c22   = val(11)
      c40   = val(12)
      c04   = val(13)
      c31   = val(14)
      c41   = val(15)
      c32   = val(16)
      c23   = val(17)

      yc = y-y0
      xc = x-x0
      t0 = c00
      t1 = c10*xc+c01*yc
      t2 = c20*xc**2+c11*xc*yc+c02*yc**2
      t3 = c12*xc*yc**2 + c21*xc**2*yc
      t4 = c22*xc**2*yc**2 + c40*xc**4 + c04*yc**4 + c31*xc**3*yc
      t5 = c41*xc**4*yc + c32*xc**3*yc**2 + c23*xc**2*yc**3



      POLY_MORE_2D =t0 + t1 + t2 + t4 +t5

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, POLY_MORE_2D
      END IF



      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION HAMILTONIAN_XY_2D(X,Y,npar,val)
c     
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 HAMILTONIAN_XY_2D, x, y
      REAL*8 x0, y0, om, bg, alp, bet, q
      REAL*8 xc, yc, xy_int, alpha, beta, x2, x4
      LOGICAL plot
      COMMON /func_plot/ plot


      x0    = val(1)
      y0    = val(2)
      om    = val(3)
      alp   = val(4)
      bet   = val(5)


      yc = y-y0
      xc = x-x0
      
      alpha = alp
      beta  = bet
      
      xy_int = om**2/2*(xc+yc)**2
      x2     = alpha*x**2
      x4     = beta*x**4


      HAMILTONIAN_XY_2D = xy_int + x2 + x4 + bg

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, HAMILTONIAN_XY_2D
      END IF



      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION HAMILTONIAN_XQ_2D(X,Y,npar,val)
c     
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 HAMILTONIAN_XQ_2D, x, y

      REAL*8 x0, y0, om1, om2, bg, alp, bet, q2, amp
      REAL*8 xc, yc, xy_int, alpha, beta
      REAL*8 q0,q0amp, q0exp, qharm, dq, x2, x4
      LOGICAL plot
      COMMON /func_plot/ plot


      x0    = val(1)
      y0    = val(2)
      om1   = val(3)
      q2    = val(4)
      om2   = val(5)
      alp   = val(6)
      bet   = val(7)
      q0amp = val(8)
      q0exp = val(9)

      yc = y-y0
      xc = x-x0
      
      IF(yc.LE.0) THEN
         q0 = q0amp*(ABS(yc))**(q0exp/2)
      ELSE
         q0  = 0
      END IF

      
      dq    = yc-q0
      alpha = alp
      beta  = bet
      
      xy_int = om1**2/2*(xc+q2)**2
      qharm  = om2**2/2*yc**2
      x2     = alpha*xc**2*dq
      x4     = beta*xc**4*dq**2


      HAMILTONIAN_XQ_2D = xy_int + qharm + x2 + x4 + bg

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, HAMILTONIAN_XQ_2D
      END IF



      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION POLY_EVENX_2D(X,Y,npar,val)
c     Polynomial on x^n*y^m with n even and n,m<=4
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 POLY_EVENX_2D, x, y
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

      POLY_EVENX_2D =x**6*y_6+x**4*y_4+x**2*y_2+y_0

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, POLY_EVENX_2D
      END IF



      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FABIAN_2D, x, y
      REAL*8 amp1,amp2, x0, yc,morse, q2
      REAL*8 y0, q, a
      REAL*8 r, N, s, bg, N0,D,m0,qamp,lda
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
      D     = val(9)
      m0    = val(10)
      qamp  = val(11)
      lda   = val(12)

      yc     = y-y0
      amp1   = N
      amp2   = N+N0*yc
      morse  = D*(1-EXP(-(y-m0)/lda))**2


      IF(yc.LE.0) THEN
        x0 = a*(-yc)**(r/2)
        FABIAN_2D=amp1*((((x-x0)*(x+x0))**2)**q)+bg + morse
      ELSE
        q2=qamp*yc+q
        FABIAN_2D=amp2*(((x**2)**2)**q2)+bg + morse
      END IF


      !write(*,*) q, x0, (x-x0)*(x+x0), ((((x-x0)*(x+x0))**2)**q)
      !pause


c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, FABIAN_2D
      END IF

      
      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION MOD_FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 MOD_FABIAN_2D, x, y
      REAL*8 amp1,amp2, x0, yc,morse, q2,x2,x4, hx, hy
      REAL*8 y0, q, a, stuf, exp1, exp2, exp10, exp20
      REAL*8 r, alp, bet, bg, s,D,m0,qamp,lda,omx, omy
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      q     = val(2)
      a     = val(3)
      r     = val(4)
      alp   = val(5)
      bet   = val(6)
      bg    = val(7)
      s     = val(8)
      D     = val(9)
      m0    = val(10)
      qamp  = val(11)
      lda   = val(12)
      omx   = val(13)
      omy   = val(14)
      exp1  = val(15)
      exp2  = val(16)
      exp10 = val(17)
      exp20 = val(18)
      
      
      yc     = y-y0
      amp2   = bet*ABS(yc)**exp2
      morse  = D*(1-EXP(-(y-m0)/lda))**2
      hx     = 1/2*omx**2*x**2
      hy     = 1/2*omy**2*yc**2
      


      IF(yc.LE.0) THEN
        amp1 = -alp*(ABS(yc))**exp1
        x0 = a*(-yc)**(r/2)      
        x2 = amp1*ABS((ABS(x)-x0))**exp10
        x4 = amp2*ABS((ABS(x)-x0))**exp20
        MOD_FABIAN_2D = x2 + x4 + hx+ hy + bg + morse
      ELSE
        amp1   = alp*(ABS(yc))**exp1
        stuf = hx+hy+bg+morse
        MOD_FABIAN_2D=amp1*((ABS(x))**exp10)+amp2*((ABS(x))**exp20)+stuf
      END IF


      !write(*,*) q, x0, (x-x0)*(x+x0), ((((x-x0)*(x+x0))**2)**q)
      !pause


c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, MOD_FABIAN_2D
      END IF

      
      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION R_POT_2D(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 R_POT_2D, x, y
      REAL*8 potential, r, D, lda, m0, bg, q1
      REAL*8 harm1, harm2,harmr1 ,om1, om2, q2, cul
      REAL*8 ct, r0, qr1, q3, om3, k, omr1, harm3, harm
      REAL*8 harmr2, omr2, qr2
      LOGICAL plot
      COMMON /func_plot/ plot
      
      D   = val(1)
      lda = val(2)
      m0  = val(3)
      bg  = val(4)
      om1 = val(5)
      q1  = val(6) 
      om2 = val(7)
      q2  = val(8)
      om3 = val(9)
      q3  = val(10)
      r0  = val(11)
      omr1= val(12)
      qr1 = val(13)
      omr2= val(14)
      qr2 = val(15)
      k   = val(16)

      
      r  = SQRT(x**2+y**2)
      potential= D*(1-EXP(-(r-m0)/lda))**2
      harm1 = 0.5*om1*(ABS(y)**q1)
      harm2 = 0.5*om2*(ABS(y)**q2)
      harm3 = 0.5*om3*(ABS(y)**q3)
      harmr1= 0.5*omr1*(ABS(r)**qr1)
      harmr2= 0.5*omr2*(ABS(r)**qr2)


      harm = harm1 + harm2 + harm3 + harmr1 + harmr2
      
      cul   = -k/r
      
      IF(r.LE.r0) THEN
         R_POT_2D = potential + bg + harm
      ELSE
         R_POT_2D = cul + bg + harm
      END IF

      
      
      
      IF(plot) THEN
         WRITE(40,*) x, y, R_POT_2D
      END IF      

      
      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION R_LJ_2D(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 R_LJ_2D, x, y
      REAL*8 lj, r, eps, sigma, bg, coef
      LOGICAL plot
      COMMON /func_plot/ plot
      
      eps   = val(1)
      sigma = val(2)
      bg    = val(3) 
      
      r     = SQRT(x**2+y**2)
      coef  = sigma/r

      lj= 4*eps*(coef**12-coef**6)
      !harm = 0.5*om*(r**2)
      
      
      R_LJ_2D = lj + bg 
      
      
      
      IF(plot) THEN
         WRITE(40,*) x, y, R_LJ_2D
      END IF      
      
      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION POWER_FRAC_2D(X,Y,npar,val)
c     
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 POWER_FRAC_2D, x, y
      REAL*8 x0, y0
      REAL*8 xc, yc, t0, t1, t2, t3, t4, t5
      REAL*8 c20, c11, c10, c02, c01, c00, c41
      REAL*8 c22, c12, c21, c40, c04, c31, c32, c23
      REAL*8 td0, td1, td2, td3, td4, td5
      REAL*8 d20, d11, d10, d02, d01, d00, d41
      REAL*8 d22, d12, d21, d40, d04, d31, d32, d23
      REAL*8 expn, expd



      LOGICAL plot
      COMMON /func_plot/ plot


      x0    = val(1)
      y0    = val(2)
      c00   = val(3)
      c10   = val(4)
      c01   = val(5)
      c20   = val(6)
      c11   = val(7)
      c02   = val(8)
      c12   = val(9)
      c21   = val(10)
      c22   = val(11)
      c40   = val(12)
      c04   = val(13)
      c31   = val(14)
      c41   = val(15)
      c32   = val(16)
      c23   = val(17)
      d00   = val(18)
      d10   = val(19)
      d01   = val(20)
      d20   = val(21)
      d11   = val(22)
      d02   = val(23)
      d12   = val(24)
      d21   = val(25)
      d22   = val(26)
      d40   = val(27)
      d04   = val(28)
      d31   = val(29)
      d41   = val(30)
      d32   = val(31)
      d23   = val(32)
      expn  = val(33)
      expd  = val(34)



      yc = y-y0
      xc = x-x0
      t0 = c00
      t1 = c10*xc+c01*yc
      t2 = c20*xc**2+c11*xc*yc+c02*yc**2
      t3 = c12*xc*yc**2 + c21*xc**2*yc
      t4 = c22*xc**2*yc**2 + c40*xc**4 + c04*yc**4 + c31*xc**3*yc
      t5 = c41*xc**4*yc + c32*xc**3*yc**2 + c23*xc**2*yc**3
      td0= d00
      td1= d10*xc+d01*yc
      td2= d20*xc**2+d11*xc*yc+d02*yc**2
      td3= d12*xc*yc**2 + d21*xc**2*yc
      td4= d22*xc**2*yc**2 + d40*xc**4 + d04*yc**4 + d31*xc**3*yc
      td5= d41*xc**4*yc + d32*xc**3*yc**2 + d23*xc**2*yc**3




      POWER_FRAC_2D =(t0+t1+t2+t4+t5)**expn/(td0+td1+td2+td4+td5)**expd

c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, POWER_FRAC_2D
      END IF



      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION DOUBLE_MORSE_2D(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DOUBLE_MORSE_2D, x, y
      REAL*8 r1, D1, lda1, m01, x01, y01
      REAL*8 r2, D2, lda2, m02, x02, y02
      REAL*8 morse1, morse2, bg
      REAL*8 harm, om, q
      LOGICAL plot
      COMMON /func_plot/ plot
      
      x01  = val(1)
      y01  = val(2)
      D1   = val(3)
      lda1 = val(4)
      m01  = val(5)
      x02  = val(6)
      y02  = val(7)
      D2   = val(8)
      lda2 = val(9)
      m02  = val(10)            
      bg   = val(11)
      om   = val(12)
      q    = val(13)


      
      r1  = SQRT((x-x01)**2+(y-y01)**2)
      r2  = SQRT((x-x02)**2+(y-y02)**2)
      harm = 0.5*om*y**q
      
      morse1 = D1*(1-EXP(-(r1-m01)/lda1))**2
      morse2 = D2*(1-EXP(-(r2-m02)/lda2))**2


      DOUBLE_MORSE_2D = morse1 + morse2 + bg + harm

      
      
      
      IF(plot) THEN
         WRITE(40,*) x, y, DOUBLE_MORSE_2D
      END IF      

      
      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION QT_2D(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 QT_2D, x, y
      REAL*8 x0, c, y0, bg,om1, om2, om3
      REAL*8 exp1, exp2, exp3, var, t1, t2, t3
      REAL*8 amp, k, texp
      LOGICAL plot
      COMMON /func_plot/ plot
      
      x0  = val(1)
      y0  = val(2)
      c   = val(3)
      om1 = val(4)
      exp1= val(5)
      om2 = val(6)
      exp2= val(7)
      om3 = val(8)
      exp3= val(9)
      amp = val(10)
      k   = val(11)
      bg  = val(12)

      
      var = x-x0-c*(y-y0)
      
      t1   = 0.5*om1*var**exp1
      t2   = 0.5*om2*var**exp2
      t3   = 0.5*om3*var**exp3
      texp = amp*EXP(k*var)


      QT_2D = t1 + t2 + t3 + texp + bg

      
      
      
      IF(plot) THEN
         WRITE(40,*) x, y, QT_2D
      END IF      

      
      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION REAL_ENERGY_XY_2D(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 REAL_ENERGY_XY_2D, x, y
      REAL(8) :: kappa_b, y0   ! the proton, along y (bending)
      REAL(8) :: kappa_o, q0, alpha   ! O-O potential 
      REAL(8) :: q, morse, qmod                               ! the proton
      REAL(8) :: r,en0,en1
      REAL(8) :: dd, r0, lam
      REAL(8) :: arg1, arg2, f1, f2 
      REAL(8) :: temp  ! temperature      
      LOGICAL plot
      COMMON /func_plot/ plot
      
      q         = val(1)
      temp      = val(2)
      kappa_b   = val(3)
      kappa_o   = val(4)
      y0        = val(5)
      q0        = val(6)
      alpha     = val(7)
      dd        = val(8)
      r0        = val(9)
      lam       = val(10)

     
      !Defining distance
      r = SQRT(x*x+y*y)
      
      !double morse part
      arg1 = (r-r0)/lam
      f1   = (1-exp(-arg1))**2

      arg2 = (q-(r-r0))/lam
      f2   = (1-exp(-arg2))**2

      morse= dd*(f1 + f2)

      !q (an)harmonic part

      qmod = q + alpha*temp
      en0 = kappa_o*(qmod-q0)**2 + kappa_o*(qmod-q0)**4/q0**2 
      en1 = kappa_b*(y-y0)**2

      REAL_ENERGY_XY_2D = en0 + en1 + morse 
      
      
      
      IF(plot) THEN
         WRITE(40,*) x, y, REAL_ENERGY_XY_2D
      END IF      

      
      RETURN
      END




c ##############################################################################################
