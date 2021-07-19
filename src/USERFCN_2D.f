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
      REAL*8 REAL_ENERGY_XY_2D, REAL_ENERGY_QT_2D, REAL_FABIAN_2D
      REAL*8 MOD_REAL_FABIAN_2D, ERF_FABIAN_2D, POWER_REAL_FABIAN_2D
      REAL*8 DIS_REAL_FABIAN_2D, ONE_DIS_FABIAN_2D, ONE_CONT_FABIAN_2D
      REAL*8 DOUBLE_FABIAN, SYM_DOUBLE_FABIAN, QUAD_PARABOLA, DD_MORSE
      REAL*8 INT_DOUBLE_FABIAN, INT_TH_DOUBLE_FABIAN, DINT_DOUBLE_FABIAN
      REAL*8 CENT_SYM_DOUBLE_FABIAN, DINT_TH_DOUBLE_FABIAN
      REAL*8 DINT_EXP_DOUBLE_FABIAN, FAKE_ENERGY_XY
      REAL*8 DINT_LINT_DOUBLE_FABIAN, DINT_LINT_SAME_DOUBLE_FABIAN
      REAL*8 DINT_EXP_DOUBLE_FABIAN
      REAL*8 DINT_EXP_DOUBLE_FABIAN,DINT_EXP_EXTRA_FABIAN 
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
      ELSE IF(funcname.EQ.'REAL_FABIAN_2D') THEN
         USERFCN_2D = REAL_FABIAN_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'POWER_REAL_FABIAN_2D') THEN
         USERFCN_2D = POWER_REAL_FABIAN_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'ERF_FABIAN_2D') THEN
         USERFCN_2D = ERF_FABIAN_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'MOD_REAL_FABIAN_2D') THEN
         USERFCN_2D = MOD_REAL_FABIAN_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'DIS_REAL_FABIAN_2D') THEN
         USERFCN_2D = DIS_REAL_FABIAN_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'ONE_DIS_FABIAN_2D') THEN
         USERFCN_2D = ONE_DIS_FABIAN_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'ONE_CONT_FABIAN_2D') THEN
         USERFCN_2D = ONE_CONT_FABIAN_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'MOD_REAL_FABIAN_2D') THEN
         USERFCN_2D = MOD_REAL_FABIAN_2D(x,y,npar,val)
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
      ELSE IF(funcname.EQ.'REAL_ENERGY_QT_2D') THEN
         USERFCN_2D = REAL_ENERGY_QT_2D(x,y,npar,val)
      ELSE IF(funcname.EQ.'DOUBLE_FABIAN') THEN
         USERFCN_2D = DOUBLE_FABIAN(x,y,npar,val)
      ELSE IF(funcname.EQ.'SYM_DOUBLE_FABIAN') THEN
         USERFCN_2D = SYM_DOUBLE_FABIAN(x,y,npar,val)
      ELSE IF(funcname.EQ.'CENT_SYM_DOUBLE_FABIAN') THEN
         USERFCN_2D = CENT_SYM_DOUBLE_FABIAN(x,y,npar,val)
      ELSE IF(funcname.EQ.'QUAD_PARABOLA') THEN
         USERFCN_2D = QUAD_PARABOLA(x,y,npar,val)
      ELSE IF(funcname.EQ.'DD_MORSE') THEN
         USERFCN_2D = DD_MORSE(x,y,npar,val)
      ELSE IF(funcname.EQ.'INT_DOUBLE_FABIAN') THEN
         USERFCN_2D = INT_DOUBLE_FABIAN(x,y,npar,val)
      ELSE IF(funcname.EQ.'DINT_DOUBLE_FABIAN') THEN
         USERFCN_2D = DINT_DOUBLE_FABIAN(x,y,npar,val)
      ELSE IF(funcname.EQ.'INT_TH_DOUBLE_FABIAN') THEN
         USERFCN_2D = INT_TH_DOUBLE_FABIAN(x,y,npar,val)
      ELSE IF(funcname.EQ.'DINT_TH_DOUBLE_FABIAN') THEN
         USERFCN_2D = DINT_TH_DOUBLE_FABIAN(x,y,npar,val)
      ELSE IF(funcname.EQ.'DINT_EXP_DOUBLE_FABIAN') THEN
         USERFCN_2D = DINT_EXP_DOUBLE_FABIAN(x,y,npar,val)
      ELSE IF(funcname.EQ.'FAKE_ENERGY_XY') THEN
         USERFCN_2D = FAKE_ENERGY_XY(x,y,npar,val)
      ELSE IF(funcname.EQ.'DINT_LINT_DOUBLE_FABIAN') THEN
         USERFCN_2D = DINT_LINT_DOUBLE_FABIAN(x,y,npar,val)
      ELSE IF(funcname.EQ.'DINT_LINT_SAME_DOUBLE_FABIAN') THEN
         USERFCN_2D = DINT_LINT_SAME_DOUBLE_FABIAN(x,y,npar,val)
=======
      ELSE IF(funcname.EQ.'DINT_EXP_EXTRA_FABIAN') THEN
         USERFCN_2D = DINT_EXP_EXTRA_FABIAN(x,y,npar,val)










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
c     The value of 'amp' is the value of the volume below the surface

      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 GAUSS_2D, x, y ! 2D: added y
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, y0, amp, sigmax, sigmay, aux1, aux2, norm  

      x0    = val(1)
      y0    = val(2)
      amp   = val(3)                 
      sigmax= val(4)
      sigmay= val(5)
      aux1  = -(x-x0)**2/(2*sigmax**2)
      aux2  = -(y-y0)**2/(2*sigmay**2)
      norm  = amp/(2*pi*sigmax*sigmay)
c     Test of under of underflow first
      IF(ABS(aux1+aux2).LT.700) THEN   
         GAUSS_2D = norm*dexp(aux1+aux2)  
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
      REAL*8 amp1,amp2, x0, yc,potential, q2, q2_, B_y, A_y
      REAL*8 y0, q1,qb1,qb2, a, Nb1, Nb2, bmp1, bmp2
      REAL*8 r, N1, s, bg, N2,D,m0,qamp,lda,b
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      q1     = val(2)
      a     = val(3)
      r     = val(4)
      N1     = val(5)
      s     = val(6)
      bg    = val(7)
      N2    = val(8)
      D     = val(9)
      m0    = val(10)
      q2    = val(11)
      lda   = val(12)
      Nb1   = val(13)
      qb1   = val(14)
      b     = val(15)
      qb2   = val(16)
      Nb2   = val(17)

      yc     = y-y0
      amp1   = N1
      amp2   = N1 
      bmp1   = Nb1
      bmp2   = Nb2


      potential= D*(1-EXP(-(b*y-m0)/lda))**2        !morse
      !potential = -D*(1+lda*(y-m0))*exp(-lda*(y-m0))!Rydberg
      !potential = D*(1-m0/y)*exp(-lda*(y*y-m0*m0))  !Varshni
      !potential = 4*D*((lda/(y-m0))**12-(lda/(y-m0))**6)      !Leonard-Jones


      IF(yc.LE.0) THEN
        x0   = (a*ABS(yc))**(r/2)
        A_y  = amp1*((((x-x0)*(x+x0))**2)**q1)
        B_y  = bmp1*((((x-x0)*(x+x0))**2)**qb1)
        FABIAN_2D= A_y + B_y + bg + potential
      ELSE
        q2   = q1
        !qb2  = qb1
        !q2  = q1/(1+b*yc) + s
        A_y = amp2*(((x**2)**2)**q2)
        B_y = bmp2*(((x**2)**2)**qb2)
        FABIAN_2D= A_y + B_y + bg + potential
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

      FUNCTION MOD_REAL_FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 MOD_REAL_FABIAN_2D, x, y
      REAL*8 y0, a, r, q, N, p, y2, D, m0, lda
      REAL*8  xmin, ycut, ysmear,b, s, bg, s2, b2
      REAL*8 amp, x0, yc,potential, A_y, arg, anharm, k
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      a     = val(2)
      r     = val(3)
      q     = val(4)
      N     = val(5)
      p     = val(6)
      y2    = val(7)
      D     = val(8)
      m0    = val(9)
      lda   = val(10)
      xmin  = val(11)
      ycut  = val(12)
      ysmear= val(13)
      b     = val(14)
      s     = val(15)
      bg    = val(16)
      s2    = val(17)
      b2    = val(18)
      
      

      potential= D*(1-EXP(-(y-m0)*lda))**2        !morse

      !potential = -D*(1+lda*(y-m0))*exp(-lda*(y-m0))!Rydberg
      !potential = D*(1-m0/y)*exp(-lda*(y*y-m0*m0))  !Varshni
      !potential = 4*D*((lda/(y-m0))**12-(lda/(y-m0))**6)      !Lennard-Jones



      
      !DOUBLE WELL
      yc     = y-y0
      IF(yc.LE.0) THEN
        x0   = a*(ABS(yc))**(r/2)
        A_y  = (((x-x0)*(x+x0)/xmin**2)**2)**q
      ELSE
        A_y = ((x**2/xmin**2)**2)**q
      END IF

      amp = N*A_y*EXP(-((y/y2)**2)**p)  
            

      !Anharmonic term
      
      arg = -y+ycut
      anharm = (b*(x/xmin)**s + b2*(x/xmin)**s2)*(1-TANH(arg))/2



      MOD_REAL_FABIAN_2D = amp + anharm + potential + bg




c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, MOD_REAL_FABIAN_2D
      END IF

      
      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION POWER_REAL_FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 POWER_REAL_FABIAN_2D, x, y
      REAL*8 y0, a, r, q, N, p, y2, D, m0, lda
      REAL*8  xmin, ycut, ysmear,b, s, bg
      REAL*8 amp, x0, yc,potential, A_y, arg, anharm, k
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      a     = val(2)
      r     = val(3)
      q     = val(4)
      N     = val(5)
      p     = val(6)
      y2    = val(7)
      D     = val(8)
      m0    = val(9)
      lda   = val(10)
      xmin  = val(11)
      ycut  = val(12)
      ysmear= val(13)
      b     = val(14)
      s     = val(15)
      bg    = val(16)
      
      

      potential= D*(1-EXP(-(y-m0)*lda))**2        !morse

      !potential = -D*(1+lda*(y-m0))*exp(-lda*(y-m0))!Rydberg
      !potential = D*(1-m0/y)*exp(-lda*(y*y-m0*m0))  !Varshni
      !potential = 4*D*((lda/(y-m0))**12-(lda/(y-m0))**6)      !Lennard-Jones



      
      !DOUBLE WELL
      yc     = y-y0
      IF(yc.LE.0) THEN
        x0   = a*(ABS(yc))**(r/2)
        A_y  = (((x-x0)*(x+x0)/xmin**2)**2)**q
      ELSE
        A_y = ((x**2/xmin**2)**2)**q
      END IF

      amp = N*A_y*((y**p)**2)  
            

      !Anharmonic term
      
      arg = -(y-ycut)/ysmear
      anharm = b*(x/xmin)**s*(1-TANH(arg))/2



      POWER_REAL_FABIAN_2D = amp + anharm + potential + bg




c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, POWER_REAL_FABIAN_2D
      END IF

      
      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION ONE_DIS_FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 ONE_DIS_FABIAN_2D, x, y
      REAL*8 y0, a, r, q, N, p, y2, D, m0, lda
      REAL*8  xmin, ycut, ysmear,b, s, bg
      REAL*8 amp, x0, yc,potential, A_y, arg, anharm, k
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      a     = val(2)
      r     = val(3)
      q     = val(4)
      N     = val(5)
      p     = val(6)
      y2    = val(7)
      D     = val(8)
      m0    = val(9)
      lda   = val(10)
      xmin  = val(11)
      ycut  = val(12)
      ysmear= val(13)
      b     = val(14)
      s     = val(15)
      bg    = val(16)
      
      

      potential= D*(1-EXP(-(y-m0)*lda))**2        !morse

      !potential = -D*(1+lda*(y-m0))*exp(-lda*(y-m0))!Rydberg
      !potential = D*(1-m0/y)*exp(-lda*(y*y-m0*m0))  !Varshni
      !potential = 4*D*((lda/(y-m0))**12-(lda/(y-m0))**6)      !Lennard-Jones



      
      !DOUBLE WELL
      yc     = y-y0
      IF(yc.LE.0) THEN
        x0   = a*(ABS(yc))**(r/2)
        A_y  = (((x-x0)*(x+x0)/xmin**2)**2)**q
        anharm = 0
      ELSE
        A_y = ((x**2/xmin**2)**2)**q
        anharm =  b*(x/xmin)**s
      END IF

      amp = N*A_y*(EXP(-(y/y2)**2))**p  
            

      !Anharmonic term
      
      arg = -(y-ycut)/ysmear



      ONE_DIS_FABIAN_2D = amp + anharm + potential + bg




c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, ONE_DIS_FABIAN_2D
      END IF

      
      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION ONE_CONT_FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 ONE_CONT_FABIAN_2D, x, y
      REAL*8 y0, a, r, q, N, p, y2, D, m0, lda
      REAL*8  xmin, ycut, ysmear,b, s, bg
      REAL*8 amp, x0, yc,potential, A_y, arg, anharm, k
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      a     = val(2)
      r     = val(3)
      q     = val(4)
      N     = val(5)
      p     = val(6)
      y2    = val(7)
      D     = val(8)
      m0    = val(9)
      lda   = val(10)
      xmin  = val(11)
      ycut  = val(12)
      ysmear= val(13)
      b     = val(14)
      s     = val(15)
      bg    = val(16)
      
      

      potential= D*(1-EXP(-(y-m0)*lda))**2        !morse

      !potential = -D*(1+lda*(y-m0))*exp(-lda*(y-m0))!Rydberg
      !potential = D*(1-m0/y)*exp(-lda*(y*y-m0*m0))  !Varshni
      !potential = 4*D*((lda/(y-m0))**12-(lda/(y-m0))**6)      !Lennard-Jones



      
      !DOUBLE WELL
      yc     = y-y0
      IF(yc.LE.0) THEN
        x0   = a*(ABS(yc))**(r/2)
        A_y  = (((x-x0)*(x+x0)/xmin**2)**2)**q
        anharm = 0
      ELSE
        A_y = ((x**2/xmin**2)**2)**q
        anharm =  (yc**(p))**2*b*(x/xmin)**s
      END IF

      amp = N*A_y*(EXP(-(y/y2)**2))  
            

      !Anharmonic term
      
      arg = -(y-ycut)/ysmear



      ONE_CONT_FABIAN_2D = amp + anharm + potential + bg




c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, ONE_CONT_FABIAN_2D
      END IF

      
      RETURN
      END




c _______________________________________________________________________________________________


      FUNCTION DIS_REAL_FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DIS_REAL_FABIAN_2D, x, y
      REAL*8 y0, a, r, q, N, p, y2, D, m0, lda
      REAL*8  xmin, ycut, ysmear,b, s, bg
      REAL*8 amp, x0, yc,potential, A_y, arg, anharm, k
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      a     = val(2)
      r     = val(3)
      q     = val(4)
      N     = val(5)
      p     = val(6)
      y2    = val(7)
      D     = val(8)
      m0    = val(9)
      lda   = val(10)
      xmin  = val(11)
      ycut  = val(12)
      ysmear= val(13)
      b     = val(14)
      s     = val(15)
      bg    = val(16)
      
      

      potential= D*(1-EXP(-(y-m0)*lda))**2        !morse

      !potential = -D*(1+lda*(y-m0))*exp(-lda*(y-m0))!Rydberg
      !potential = D*(1-m0/y)*exp(-lda*(y*y-m0*m0))  !Varshni
      !potential = 4*D*((lda/(y-m0))**12-(lda/(y-m0))**6)      !Lennard-Jones



      
      !DOUBLE WELL
      yc     = y-y0
      IF(yc.LE.0) THEN
        x0   = a*(ABS(yc))**(r/2)
        A_y  = (((x-x0)*(x+x0)/xmin**2)**2)**q
      ELSE
        A_y = ((x**2/xmin**2)**2)**q
      END IF

      amp = N*A_y*(EXP(-(y/y2)**2))**p  
            

      !Anharmonic term
      
      arg = -(y-ycut)/ysmear
      anharm = b*((x/xmin)**s)**2



      DIS_REAL_FABIAN_2D = amp + anharm + potential + bg




c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, DIS_REAL_FABIAN_2D
      END IF

      
      RETURN
      END



c _______________________________________________________________________________________________


      FUNCTION Y0_REAL_FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 Y0_REAL_FABIAN_2D, x, y
      REAL*8 y0, a, r, q, N, p, y2, D, m0, lda
      REAL*8  xmin, ycut, ysmear,b, s, bg
      REAL*8 amp, x0, yc,potential, A_y, arg, anharm, k
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      a     = val(2)
      r     = val(3)
      q     = val(4)
      N     = val(5)
      p     = val(6)
      y2    = val(7)
      D     = val(8)
      m0    = val(9)
      lda   = val(10)
      xmin  = val(11)
      ycut  = val(12)
      ysmear= val(13)
      b     = val(14)
      s     = val(15)
      bg    = val(16)
      
      

      potential= D*(1-EXP(-(y-m0)*lda))**2        !morse

      !potential = -D*(1+lda*(y-m0))*exp(-lda*(y-m0))!Rydberg
      !potential = D*(1-m0/y)*exp(-lda*(y*y-m0*m0))  !Varshni
      !potential = 4*D*((lda/(y-m0))**12-(lda/(y-m0))**6)      !Lennard-Jones



      
      !DOUBLE WELL
      yc     = y-y0
      IF(yc.LE.0) THEN
        x0   = a*(ABS(yc))**(r/2)
        A_y  = (((x-x0)*(x+x0)/xmin**2)**2)**q
      ELSE
        A_y = ((x**2/xmin**2)**2)**q
      END IF

      amp = N*A_y*(EXP(-(y/y2)**2))**p  
            

      !Anharmonic term
      
      arg = -(y-y0)/ysmear
      anharm = b*(x/xmin)**s*(1-TANH(arg))/2



      Y0_REAL_FABIAN_2D = amp + anharm + potential + bg


c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, Y0_REAL_FABIAN_2D
      END IF

      
      RETURN
      END

c ______________________________________________________________________________________________




      FUNCTION REAL_FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 REAL_FABIAN_2D, x, y
      REAL*8 y0, a, r, q, N, p, y2, D, m0, lda
      REAL*8  xmin, ycut, ysmear,b, s, bg
      REAL*8 amp, x0, yc,potential, A_y, arg, anharm, k
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      a     = val(2)
      r     = val(3)
      q     = val(4)
      N     = val(5)
      p     = val(6)
      y2    = val(7)
      D     = val(8)
      m0    = val(9)
      lda   = val(10)
      xmin  = val(11)
      ycut  = val(12)
      ysmear= val(13)
      b     = val(14)
      s     = val(15)
      bg    = val(16)
      
      

      potential= D*(1-EXP(-(y-m0)*lda))**2        !morse

      !potential = -D*(1+lda*(y-m0))*exp(-lda*(y-m0))!Rydberg
      !potential = D*(1-m0/y)*exp(-lda*(y*y-m0*m0))  !Varshni
      !potential = 4*D*((lda/(y-m0))**12-(lda/(y-m0))**6)      !Lennard-Jones



      
      !DOUBLE WELL
      yc     = y-y0
      IF(yc.LE.0) THEN
        x0   = a*(ABS(yc))**(r/2)
        A_y  = (((x-x0)*(x+x0)/xmin**2)**2)**q
      ELSE
        A_y = ((x**2/xmin**2)**2)**q
      END IF

      amp = N*A_y*(EXP(-(y/y2)**2))**p  
            

      !Anharmonic term
      
      arg = -(y-ycut)/ysmear
      anharm = b*(x/xmin)**s*(1-TANH(arg))/2



      REAL_FABIAN_2D = amp + anharm + potential + bg




c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, REAL_FABIAN_2D
      END IF

      
      RETURN
      END

c _______________________________________________________________________________________________


      FUNCTION ERF_FABIAN_2D(X,Y,npar,val)
c     One minimum for yc>thr and 2 (or more if p>1) for yc<thr
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 ERF_FABIAN_2D, x, y
      REAL*8 y0, a, r, q, N, p, y2, D, m0, lda
      REAL*8  xmin, ycut, ysmear,b, s, bg
      REAL*8 amp, x0, yc,potential, A_y, arg, anharm, k
      LOGICAL plot
      COMMON /func_plot/ plot


      y0    = val(1)
      a     = val(2)
      r     = val(3)
      q     = val(4)
      N     = val(5)
      p     = val(6)
      y2    = val(7)
      D     = val(8)
      m0    = val(9)
      lda   = val(10)
      xmin  = val(11)
      ycut  = val(12)
      ysmear= val(13)
      b     = val(14)
      s     = val(15)
      bg    = val(16)
      
      

      potential= D*(1-EXP(-(y-m0)*lda))**2        !morse

      !potential = -D*(1+lda*(y-m0))*exp(-lda*(y-m0))!Rydberg
      !potential = D*(1-m0/y)*exp(-lda*(y*y-m0*m0))  !Varshni
      !potential = 4*D*((lda/(y-m0))**12-(lda/(y-m0))**6)      !Lennard-Jones



      
      !DOUBLE WELL
      yc     = y-y0
      IF(yc.LE.0) THEN
        x0   = a*(ABS(yc))**(r/2)
        A_y  = (((x-x0)*(x+x0)/xmin**2)**2)**q
      ELSE
        A_y = ((x**2/xmin**2)**2)**q
      END IF

      amp = N*A_y*(EXP(-(y/y2)**2))**p  
            

      !Anharmonic term
      
      arg = -(y-ycut)/ysmear
      anharm = b*(x/xmin)**s*(1-ERF(arg))/2



      ERF_FABIAN_2D = amp + anharm + potential + bg




c     Save the different components
      IF(plot) THEN
         WRITE(40,*) x, y, ERF_FABIAN_2D
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
      REAL(8) :: dd, r0, lam, bg
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
      bg        = val(11)

     
      !Defining distance
      r = SQRT(x*x+y*y)
      
      !double morse part
      arg1 = (r-r0)/lam
      f1   = (1-exp(-arg1))**2

      arg2 = (q-(r-r0))/lam
      f2   = (1-exp(-arg2))**2

      morse= dd*(f1 + f2)

      !q (an)harmonic part

      qmod = q0 + alpha*temp

      en0 = kappa_o*(q-qmod)**2 + kappa_o*(q-qmod)**4/qmod**2 
      en1 = kappa_b*(ABS(y-y0))**2

      REAL_ENERGY_XY_2D = en0 + en1 + morse + bg
      
      
      
      IF(plot) THEN
         WRITE(40,*) x, y, REAL_ENERGY_XY_2D
      END IF      

      
      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION FAKE_ENERGY_XY(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FAKE_ENERGY_XY, x, y
      real(8) :: kappa_b    ! the proton, along y (bending)
      real(8) :: alpha   ! O-O potential 
      real(8) :: kappa_b2 
      real(8) :: dd, r0, y0, lam=0.35
      REAL(8) :: ENERGY
      REAL(8) :: morse                   ! the proton
      REAL(8) :: r, en1
      REAL(8) :: arg1, f1    
      LOGICAL plot
      COMMON /func_plot/ plot
      
      
      kappa_b   = val(1)
      kappa_b2   = val(2)
      y0        = val(3)
      dd        = val(4)
      r0        = val(5)
      lam       = val(6)

     
      !Defining distance
      r = SQRT(x*x+y*y)
      
      !double morse part
      arg1 = (r-r0)/lam
      f1   = (1-exp(-arg1))**2


      morse= dd*(f1) !+ f2)

      
      en1 = kappa_b*(ABS(y-y0))**2 + kappa_b2*(ABS(y-y0))**4

      FAKE_ENERGY_XY = en1 + morse 
      
      
      IF(plot) THEN
         WRITE(40,*) x, y, FAKE_ENERGY_XY
      END IF      

      
      RETURN
      END






c _______________________________________________________________________________________________

      FUNCTION REAL_ENERGY_QT_2D(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 REAL_ENERGY_QT_2D, x, y
      REAL(8) :: kappa_b, y0   ! the proton, along y (bending)
      REAL(8) :: kappa_o, q0, alpha   ! O-O potential 
      REAL(8) :: q, morse, qmod                               ! the proton
      REAL(8) :: r,en0,en1
      REAL(8) :: dd, r0, lam, bg, x_, y_
      REAL(8) :: arg1, arg2, f1, f2 
      REAL(8) :: temp  ! temperature      
      LOGICAL plot
      COMMON /func_plot/ plot
      
      x_        = val(1)
      y_        = val(2)
      kappa_b   = val(3)
      kappa_o   = val(4)
      y0        = val(5)
      q0        = val(6)
      alpha     = val(7)
      dd        = val(8)
      r0        = val(9)
      lam       = val(10)
      bg        = val(11)

     
      !Redefining variables

      q    = x
      temp = y


      !Defining distance
      r = SQRT(x_*x_+y_*y_)
      
      !double morse part
      arg1 = (r-r0)/lam
      f1   = (1-exp(-arg1))**2

      arg2 = (q-(r-r0))/lam
      f2   = (1-exp(-arg2))**2

      morse= dd*(f1 + f2)

      !q (an)harmonic part

      qmod = q0 + alpha*temp

      en0 = kappa_o*(q-qmod)**2 + kappa_o*(q-qmod)**4/qmod**2 
      en1 = kappa_b*(ABS(y-y0))**2

      REAL_ENERGY_QT_2D = en0 + en1 + morse + bg
      
      
      
      IF(plot) THEN
         WRITE(40,*) x, y, REAL_ENERGY_QT_2D
      END IF      

      
      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DOUBLE_FABIAN, x, y
      REAL*8 s1,s2,s01,s02,N,bg
      REAL*8 p1, p2
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      s01 = val(2) 
      s02 = val(3) 
      bg = val(4) 

      s1 = (x+y)/2
      s2 = (x-y)/2
      


      p1 = (s1+s01)*(s1-s01)
      p2 = (s2+s02)*(s2-s02)

      DOUBLE_FABIAN = N*p1**2*p2**2 + bg







      IF(plot) THEN
         WRITE(40,*) x, y, DOUBLE_FABIAN
      END IF      



      RETURN
      END




c _______________________________________________________________________________________________

      FUNCTION SYM_DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 SYM_DOUBLE_FABIAN, x, y
      REAL*8 s0,N,bg,q,b
      REAL*8 p2, p1,anharm
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      s0 = val(2)
      bg = val(3)
      q  = val(4)
      b  = val(5)



      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*(x**6 + y**6)

      SYM_DOUBLE_FABIAN = N*(p1**2)**q+ N*(p2**2)**q + anharm + bg







      IF(plot) THEN
         WRITE(40,*) x, y, SYM_DOUBLE_FABIAN
      END IF      



      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION CENT_SYM_DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 CENT_SYM_DOUBLE_FABIAN, x, y
      REAL*8 s0,N,bg,q,b
      REAL*8 p2, p1,anharm
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      s0 = val(2)
      bg = val(3)
      q  = val(4)
      b  = val(5)



      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*(p1**2 + p2**2)**3

      CENT_SYM_DOUBLE_FABIAN = N*(p1**2)**q+ N*(p2**2)**q + anharm + bg







      IF(plot) THEN
         WRITE(40,*) x, y, CENT_SYM_DOUBLE_FABIAN
      END IF      



      RETURN
      END
c _______________________________________________________________________________________________

      FUNCTION INT_DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 INT_DOUBLE_FABIAN, prev, x, y
      REAL*8 s0,N,bg,q,b,i1,i3
      REAL*8 p2, p1,anharm, inter, pol_inter
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      s0 = val(2)
      bg = val(3)
      q  = val(4)
      b  = val(5)
      i1 = val(6)
      i3 = val(7)



      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      

      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*(x**6 + y**6)

      
      inter = x*y
      pol_inter = i1*inter + i3*inter**3


      prev = N*(p1**2)**q+ N*(p2**2)**q + anharm + bg

      INT_DOUBLE_FABIAN =prev + pol_inter







      IF(plot) THEN
         WRITE(40,*) x, y, INT_DOUBLE_FABIAN
      END IF      



      RETURN
      END
      
c _______________________________________________________________________________________________

      FUNCTION DINT_DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DINT_DOUBLE_FABIAN, prev, x, y
      REAL*8 s0,N,bg,q,b,i1,i2
      REAL*8 p2, p1,anharm, inter, pol_inter
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      s0 = val(2)
      bg = val(3)
      q  = val(4)
      b  = val(5)
      i1 = val(6)
      i2 = val(7)



      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      

      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*(x**6 + y**6)

      
      inter = abs(x-y)
      pol_inter = i1*inter + i2*inter**2


      prev = N*(p1**2)**q+ N*(p2**2)**q + anharm + bg

      DINT_DOUBLE_FABIAN =prev + pol_inter







      IF(plot) THEN
         WRITE(40,*) x, y, DINT_DOUBLE_FABIAN
      END IF      



      RETURN
      END



c _______________________________________________________________________________________________

      FUNCTION INT_TH_DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 INT_TH_DOUBLE_FABIAN, prev, x, y
      REAL*8 s0,N,bg,q,b,amp, a, y2_0
      REAL*8 p2, p1,anharm, inter, tan_inter
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      s0 = val(2)
      bg = val(3)
      q  = val(4)
      b  = val(5)
      amp= val(6)
      a  = val(7)
      y2_0= val(8)



      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      

      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*(x**6 + y**6)

      
      inter = x*y
      tan_inter = amp*tanh(inter/a-y2_0)


      prev = N*(p1**2)**q+ N*(p2**2)**q + anharm + bg

      INT_TH_DOUBLE_FABIAN =prev + tan_inter







      IF(plot) THEN
         WRITE(40,*) x, y, INT_TH_DOUBLE_FABIAN
      END IF      



      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION DINT_TH_DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DINT_TH_DOUBLE_FABIAN, prev, x, y
      REAL*8 s0,N,bg,q,b,amp, a, y2_0
      REAL*8 p2, p1,anharm, inter, tan_inter
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      s0 = val(2)
      bg = val(3)
      q  = val(4)
      b  = val(5)
      amp= val(6)
      a  = val(7)
      y2_0= val(8)



      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      

      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*(x**6 + y**6)

      
      inter = -abs(x-y)
      tan_inter = amp*tanh(inter/a-y2_0)


      prev = N*(p1**2)**q+ N*(p2**2)**q + anharm + bg

      DINT_TH_DOUBLE_FABIAN =prev + tan_inter







      IF(plot) THEN
         WRITE(40,*) x, y, DINT_TH_DOUBLE_FABIAN
      END IF      



      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION DINT_EXP_DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DINT_EXP_DOUBLE_FABIAN, prev, x, y
      REAL*8 s0,N,bg,q,b,amp, a, y2_0, p
      REAL*8 p2, p1,anharm, inter, exp_inter
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      s0 = val(2)
      bg = val(3)
      q  = val(4)
      b  = val(5)
      amp= val(6)
      a  = val(7)
      y2_0= val(8)
      p  = val(9)



      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      

      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*((x**2)**p + (y**2)**p)

      
      inter = -abs(x-y)
      exp_inter = amp*exp(inter/a-y2_0)


      prev = N*(p1**2)**q+ N*(p2**2)**q + anharm + bg

      DINT_EXP_DOUBLE_FABIAN =prev + exp_inter







      IF(plot) THEN
         WRITE(40,*) x, y, DINT_EXP_DOUBLE_FABIAN
      END IF      



      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION DINT_EXP_EXTRA_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DINT_EXP_EXTRA_FABIAN, prev, x, y
      REAL*8 s0,N,bg,q,b,amp, a, y2_0, p
      REAL*8 p2, p1,anharm, inter, exp_inter
      REAL*8 anharm_2, p_2, q_2, N_2, b_2, prev_2
      LOGICAL plot
      COMMON /func_plot/ plot


      N    = val(1)
      s0   = val(2)
      bg   = val(3)
      q    = val(4)
      b    = val(5)
      amp  = val(6)
      a    = val(7)
      y2_0 = val(8)
      p    = val(9)
      N_2  = val(10)
      q_2  = val(11)
      b_2  = val(12)
      p_2  = val(13)


      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      

      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*((x**2)**p + (y**2)**p)
      anharm_2 = b_2*((x**2)**p_2 + (y**2)**p_2)

      
      inter = -abs(x-y)
      exp_inter = amp*exp(inter/a-y2_0)


      prev = N*(p1**2)**q+ N*(p2**2)**q  + anharm + anharm_2 + bg
      prev_2 = N_2*(p1**2)**q_2+ N_2*(p2**2)**q_2

      DINT_EXP_EXTRA_FABIAN =prev + prev_2 + exp_inter







      IF(plot) THEN
         WRITE(40,*) x, y, DINT_EXP_EXTRA_FABIAN
      END IF      



      RETURN
      END




c _______________________________________________________________________________________________

      FUNCTION QUAD_PARABOLA(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 QUAD_PARABOLA, x, y
      REAL*8 r0, bg, N,q
      REAL*8 par
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      r0 = val(2)
      bg = val(3)
      q  = val(4)



      IF ((x>0).AND.(y>0)) THEN
         par = (x-r0)**2+(y-r0)**2
      ELSE IF ((x>0).AND.(y<0)) THEN
         par = (x-r0)**2+(y+r0)**2
      ELSE IF ((x<0).AND.(y<0)) THEN
         par = (x+r0)**2+(y+r0)**2
      ELSE
         par = (x+r0)**2+(y-r0)**2
      END IF

      


      
      QUAD_PARABOLA = N*par**q + bg







      IF(plot) THEN
         WRITE(40,*) x, y, QUAD_PARABOLA
      END IF      



      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION DD_MORSE(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DD_MORSE, x, y
      REAL*8 r0, bg, N,q
      REAL*8 dm1, dm2
      LOGICAL plot
      COMMON /func_plot/ plot


      N  = val(1)
      r0 = val(2)
      bg = val(3)
      q  = val(4)



      dm1 = (1-exp(q*(x-r0)))**2+(1-exp(-q*(x+r0)))**2
      dm2 = (1-exp(q*(y-r0)))**2+(1-exp(-q*(y+r0)))**2
      


      
      DD_MORSE = N*(dm1+dm2) + bg







      IF(plot) THEN
         WRITE(40,*) x, y, DD_MORSE
      END IF      



      RETURN
      END


c _______________________________________________________________________________________________

      FUNCTION DINT_LINT_DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DINT_LINT_DOUBLE_FABIAN, prev, x, y
      REAL*8 s0,N,bg,q,b,amp, a1, a2, p
      REAL*8 p2, p1,anharm, inter, lin_inter, inter_pos
      LOGICAL plot
      COMMON /func_plot/ plot


      N   = val(1)
      s0  = val(2)
      bg  = val(3)
      q   = val(4)
      b   = val(5)
      amp = val(6)
      a1  = val(7)
      a2  = val(8)
      p   = val(9)



      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      

      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*((x**2)**p + (y**2)**p)

      
      inter = abs(x-y)
      inter_pos = abs(x+y)
      
      lin_inter = a1*inter+a2*inter_pos


      prev = N*(p1**2)**q+ N*(p2**2)**q + anharm + bg

      DINT_LINT_DOUBLE_FABIAN =prev + lin_inter







      IF(plot) THEN
         WRITE(40,*) x, y, DINT_LINT_DOUBLE_FABIAN
      END IF      



      RETURN
      END

c _______________________________________________________________________________________________

      FUNCTION DINT_LINT_SAME_DOUBLE_FABIAN(X,Y,npar,val)
      
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 DINT_LINT_SAME_DOUBLE_FABIAN, prev, x, y
      REAL*8 s0,N,bg,q,b,amp, a, p
      REAL*8 p2, p1,anharm, inter, lin_inter, inter_pos
      LOGICAL plot
      COMMON /func_plot/ plot


      N   = val(1)
      s0  = val(2)
      bg  = val(3)
      q   = val(4)
      b   = val(5)
      amp = val(6)
      a  = val(7)
      p   = val(8)



      !s1 = (x+y)/2
      !s2 = (x-y)/2
      


      

      p1 = (x+s0)*(x-s0)
      p2 = (y+s0)*(y-s0)

      anharm = b*((x**2)**p + (y**2)**p)

      
      inter = abs(x-y)
      inter_pos = abs(x+y)
      
      lin_inter = a*inter+a*inter_pos


      prev = N*(p1**2)**q+ N*(p2**2)**q + anharm + bg

      DINT_LINT_SAME_DOUBLE_FABIAN =prev + lin_inter







      IF(plot) THEN
         WRITE(40,*) x, y, DINT_LINT_SAME_DOUBLE_FABIAN
      END IF      



      RETURN
      END



c ##############################################################################################
