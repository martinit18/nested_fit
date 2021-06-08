!     Automatic Time-stamp: <Last changed by martino on Tuesday 07 April 2020 at CEST 18:34:52>
!################################### USERFCN DEFINITION #####################################



      FUNCTION USERFCN_nD(x,npar,val,funcname)
      USE MOD_NSM
      IMPLICIT NONE
      
      
      INTEGER(4) :: npar, maxdim=40
      REAL(8), DIMENSION(npar) :: val
      REAL(8), DIMENSION(:) :: x(maxdim)
      REAL(8) ::  USERFCN_nD, GAUSS_3D,REAL_ENERGY_ND,NSM_F72,NSM_BE_F72
      CHARACTER(64) :: funcname
      
      

!     Choose your model (see below for definition)
      IF(funcname.EQ.'GAUSS_3D') THEN
         USERFCN_nD = GAUSS_3D(x,npar,val)
      ELSE IF(funcname.EQ.'REAL_ENERGY_ND') THEN
         USERFCN_nD = REAL_ENERGY_ND(x,npar,val)
      ELSE IF(funcname.EQ.'NSM_F72') THEN
         USERFCN_nD = NSM_F72(x,npar,val)
      ELSE IF(funcname.EQ.'NSM_BE_F72') THEN
         USERFCN_nD = NSM_BE_F72(x,npar,val)
      ELSE IF(funcname.EQ.'NSM_SN') THEN
         USERFCN_nD = NSM_SN(x,npar,val)
      ELSE IF(funcname.EQ.'NSM_SN_BE') THEN
         USERFCN_nD = NSM_SN_BE(x,npar,val)
      



      ELSE
         WRITE(*,*) 'Error in the function name def. in USERFCN'
         WRITE(*,*) 'Check in the manual and in the input.dat file'
         STOP
      END IF

      RETURN
      END


!################################### LINESHAPE DEFINITIONS #####################################


      FUNCTION GAUSS_3D(X,npar,val)
!     Normalized Gaussian distribution plus background
!     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 x(3)
      REAL*8 GAUSS_3D 
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0, y0, z0, amp, sigmax, sigmay, sigmaz, bg
      REAL*8  x_, y_, z_, expx, expy, expz,pref
      LOGICAL plot
      COMMON /func_plot/ plot

      !x(1) = x_
      !x(2) = y_
      !x(3) = z_

      bg    = val(1)
      x0    = val(2)
      y0    = val(3)
      z0    = val(4)
      amp   = val(5)
      sigmax= val(6)
      sigmay= val(7)
      sigmaz= val(8)

!     Test of under of underflow first
      expx = -(x(1)-x0)**2/(2*sigmax**2)
      expy = -(x(2)-y0)**2/(2*sigmay**2)
      expz = -(x(3)-z0)**2/(2*sigmaz**2)
      IF(ABS(expx + expy + expz).LT.700) THEN
         pref = amp/(dsqrt((2*pi)**3)*sigmax*sigmay*sigmaz)
         GAUSS_3D = pref*exp(expx+expy+expz)
      ELSE
         GAUSS_3D = 0.d0
      END IF      
      
      IF(plot) THEN
         WRITE(40,*) x(:) , GAUSS_3D
      END IF      
 
      RETURN
      END



!______________________________________________________________________________________________

      FUNCTION REAL_ENERGY_ND(X, npar,val)
      
      IMPLICIT NONE
      INTEGER(4) :: npar
      REAL(8) :: val(npar)
      REAL(8) :: REAL_ENERGY_ND, x(4),x_, y
      REAL(8) :: kappa_b, y0   ! the proton, along y (bending)
      REAL(8) :: kappa_o, q0, alpha   ! O-O potential
      REAL(8) :: q, q_mod , morse                   ! the proton
      REAL(8) :: r,en0,en1
      REAL(8) :: dd, r0, lam
      REAL(8) :: arg1, arg2, f1, f2, bg, qmod 
      REAL(8) :: temp  ! temperature      
      LOGICAL plot
      COMMON /func_plot/ plot
      

      kappa_b   = val(1)
      kappa_o   = val(2)
      y0        = val(3)
      q0        = val(4)
      alpha     = val(5)
      dd        = val(6)
      r0        = val(7)
      lam       = val(8)


      x(1) = x_
      x(2) = y
      x(3) = q
      x(4) = temp


     
      !Defining distance
      r = SQRT(x_*x_+y*y)
      
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

      REAL_ENERGY_ND = en0 + en1 + morse + bg

      
      
      
      !IF(plot) THEN
      !   WRITE(40,*) x, y, REAL_ENERGY_XY_2D
      !END IF      

      
      RETURN
      END


! ##############################################################################################

      FUNCTION NSM_BE_F72(X,npar,val)

      IMPLICIT NONE
      INTEGER(4) :: npar
      REAL(8) :: val(npar)
      REAL(8) :: Jinp, ltemp_gs
      REAL(8) :: NSM_BE_F72, X(3)
      INTEGER(4) :: N, k, l, i,loc, cnt, col, el, ier
      INTEGER(4) :: loc_gs, lgth_gs, siz_gs, lgth
      INTEGER(4) :: nentries=1000, n2b, n3b, junk_size=5, MATRIX_SIZE, siz
      CHARACTER  :: namefile*64, np_yn*1, string*64
      REAL(8), DIMENSION(1000) :: nuc=-1, J=-1, length=-1, element=-1, sp=-1
      REAL(8), ALLOCATABLE, DIMENSION(:) ::  lin_m, eig, iperm
      REAL(8), ALLOCATABLE, DIMENSION(:) ::  iperm_gs, lin_m_gs, eig_gs
      CHARACTER(64), ALLOCATABLE, DIMENSION(:) :: junk_var, names_2b
      CHARACTER(64), ALLOCATABLE, DIMENSION(:) ::  names_3b
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: me_2b, me_3b, matrix
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: matrix_gs
      REAL(8) :: ex, me_stmp, me_tmp, val_tmp, ltemp, rem, J_st, e_sp
      REAL(8), DIMENSION(2) :: sort_ex
      INTEGER(2) :: compar


      !INPUTING THE NUCLEUS AND STATE
      N    = X(1) 
      Jinp = X(2)
      el   = X(3)
      !Reading the file_selection 
      
      namefile = 'f72.dat'
      J_st = 3.5
      n2b  = 4
      n3b  = 6
      !OPEN(49, file = 'file_selection.txt',status='old') 
      !READ(49,*) namefile, string
      !READ(49,*) np_yn, string
      !READ(49,*) J_st, string
      !READ(49,*) n2b, string
      !READ(49,*) n3b,string
      !CLOSE(49)
      !write(*,*) namefile, J_st, n3b
      ALLOCATE(names_2b(n2b),names_3b(n3b),junk_var(junk_size))
      !write(*,*) 'Hola 1'

      
      ! Reading the number of entries (rows-head)

      nentries = 1000 
      
      !OPEN(1, file = namefile,status='old') 
      !DO 
      !   READ (1,*, END=10) 
      !   nentries = nentries + 1 
      !END DO v
      !10 CLOSE (1) 

      !ALLOCATE(nuc(nentries), J(nentries), length(nentries), element(nentries), sp(nentries))
      ALLOCATE(me_2b(n2b,nentries), me_3b(n3b,nentries))
      !write(*,*) 'Hola 2'


      
      ! Assigning values to the variables
      
      !write(*,*) 'Before reading'
      OPEN(2, file = namefile) 
      REWIND 2
      READ(2,*) junk_var(:), names_3b(:), names_2b(:)
      DO i=1, 33
         READ(2,*,ERR=45, END=45) nuc(i), J(i), length(i), element(i), sp(i), me_3b(:,i), me_2b(:,i)
      END DO 
      45 CLOSE (2)
      !write(*,*) 'After reading'


      

      !Checking positions:

      !of the state
      DO i=1, nentries
         IF (nuc(i).EQ.N) THEN
            IF (J(i).EQ.Jinp) THEN
               loc = i 
               lgth  = length(i)
               ltemp = real(lgth)
               siz = int((sqrt(1.+8*ltemp)-1)/2)
               !write(*,*) N, J(i),lgth_gs, ltemp_gs, siz_gs
               EXIT 
            END IF
         END IF 
      END DO

!write(*,*) N, Jinp, lgth, ltemp, siz


      !ALLOCATE(val(n2b+n3b))



      !Assigning values to val


      ! UNCOMMENT FOR TRIAL
      !DO i=1, 2
      !   val(i) = 1.
      !END DO

      !DO i=3, 7
      !   val(i) = 3.
      !END DO
      !DO i=8, n2b + n3b
      !   val(i) = 4.
      !END DO


      ! UNCOMMENT FOR FUNCTION. f7/2 Shell 

      !val(1) = v0
      !val(2) = v2
      !val(3) = v4
      !val(4) = v6
      !val(5) = v3
      !val(6) = v5
      !val(7) = v7
      !val(8) = v9
      !val(9) = v11
      !val(10)= v15




      ALLOCATE(matrix(siz,siz), lin_m(lgth),eig(siz))


     ! Locating excited  state
      DO l=1, lgth
         me_tmp = 0
         DO k=1, n2b
            val_tmp = val(k)
            me_stmp= me_2b(k,loc + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
         END DO
         DO k=1, n3b
            val_tmp = val(n2b+k)
            me_stmp= me_3b(k,loc + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
         END DO 
         lin_m(l)  = me_tmp
      END DO


      cnt = 1
      col = 1
      DO i = 1, siz
         DO l = col, siz
             matrix(i,l) = lin_m(cnt)
             matrix(l,i) = lin_m(cnt)
             cnt = cnt +1
         END DO
         col = col + 1
      END DO



     !Symmetrizing matrix
     !write(*,*) siz
     call diasym(matrix,eig,siz)
     


     !Ordering energies

     ALLOCATE(iperm(siz))


     CALL DPSORT(eig,siz,iperm,2,ier)

     !Adding single particle energies

      e_sp = val(n2b+n3b+1)


     !Calculating requested energy
      NSM_BE_F72 = eig(el)+ N*e_sp


      RETURN
      END 

! ##############################################################################################

      FUNCTION NSM_F72(X,npar,val)

      IMPLICIT NONE
      INTEGER(4) :: npar
      REAL(8) :: val(npar)
      REAL(8) :: Jinp, ltemp_gs
      REAL(8) :: NSM_F72, X(3)
      INTEGER(4) :: N, k, l, i,loc, cnt, col, el, ier
      INTEGER(4) :: loc_gs, lgth_gs, siz_gs, lgth
      INTEGER(4) :: nentries=1000, n2b, n3b, junk_size=5, MATRIX_SIZE, siz
      CHARACTER  :: namefile*64, np_yn*1, string*64
      REAL(8), DIMENSION(1000) :: nuc=-1, J=-1, length=-1, element=-1, sp=-1
      REAL(8), ALLOCATABLE, DIMENSION(:) ::  lin_m, eig, iperm
      REAL(8), ALLOCATABLE, DIMENSION(:) ::  iperm_gs, lin_m_gs, eig_gs
      CHARACTER(64), ALLOCATABLE, DIMENSION(:) :: junk_var, names_2b
      CHARACTER(64), ALLOCATABLE, DIMENSION(:) ::  names_3b
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: me_2b, me_3b, matrix
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: matrix_gs
      REAL(8) :: ex, me_stmp, me_tmp, val_tmp, ltemp, rem, J_st
      REAL(8), DIMENSION(2) :: sort_ex
      INTEGER(2) :: compar


      !INPUTING THE NUCLEUS AND STATE
      N    = X(1) 
      Jinp = X(2)
      el   = X(3)
      !Reading the file_selection 
      
      namefile = 'f72.dat'
      J_st = 3.5
      n2b  = 4
      n3b  = 6
      !OPEN(49, file = 'file_selection.txt',status='old') 
      !READ(49,*) namefile, string
      !READ(49,*) np_yn, string
      !READ(49,*) J_st, string
      !READ(49,*) n2b, string
      !READ(49,*) n3b,string
      !CLOSE(49)
      !write(*,*) namefile, J_st, n3b
      ALLOCATE(names_2b(n2b),names_3b(n3b),junk_var(junk_size))
      !write(*,*) 'Hola 1'

      
      ! Reading the number of entries (rows-head)

      nentries = 1000 
      
      !OPEN(1, file = namefile,status='old') 
      !DO 
      !   READ (1,*, END=10) 
      !   nentries = nentries + 1 
      !END DO v
      !10 CLOSE (1) 

      !ALLOCATE(nuc(nentries), J(nentries), length(nentries), element(nentries), sp(nentries))
      ALLOCATE(me_2b(n2b,nentries), me_3b(n3b,nentries))
      !write(*,*) 'Hola 2'


      
      ! Assigning values to the variables
      
      !write(*,*) 'Before reading'
      OPEN(2, file = namefile) 
      REWIND 2
      READ(2,*) junk_var(:), names_3b(:), names_2b(:)
      DO i=1, 33
         READ(2,*,ERR=45, END=45) nuc(i), J(i), length(i), element(i), sp(i), me_3b(:,i), me_2b(:,i)
      END DO 
      45 CLOSE (2)
      !write(*,*) 'After reading'


      

      !Checking positions:

      !of the GS
      DO i=1, nentries
         IF (nuc(i).EQ.N) THEN
            !write(*,*) MOD(N,2)
            IF (MOD(N,2)==0.) THEN
               IF (J(i)==0) THEN
                  loc_gs = i
                  lgth_gs = length(i)
                  ltemp_gs = real(lgth_gs)
                  siz_gs = int((sqrt(1.+8*ltemp_gs)-1)/2)
                  !write(*,*) N, J(i),lgth_gs, ltemp_gs, siz_gs
                  EXIT
               END IF
            ELSE
               IF (J(i)==J_st) THEN
                  loc_gs = i
                  lgth_gs = length(i)
                  ltemp_gs = real(lgth_gs)
                  siz_gs = int((sqrt(1.+8*ltemp_gs)-1)/2)
                  !write(*,*) N, J(i),lgth_gs, ltemp_gs, siz_gs
                  EXIT
               END IF
            END IF
         END IF 
      END DO
      
!write(*,*) N, Jinp, lgth_gs, ltemp_gs, siz_gs


      !of the state
      DO i=1, nentries
         IF (nuc(i).EQ.N) THEN
            IF (J(i).EQ.Jinp) THEN
               loc = i 
               lgth  = length(i)
               ltemp = real(lgth)
               siz = int((sqrt(1.+8*ltemp)-1)/2)
               !write(*,*) N, J(i),lgth_gs, ltemp_gs, siz_gs
               EXIT 
            END IF
         END IF 
      END DO

!write(*,*) N, Jinp, lgth, ltemp, siz


      !ALLOCATE(val(n2b+n3b))



      !Assigning values to val


      ! UNCOMMENT FOR TRIAL
      !DO i=1, 2
      !   val(i) = 1.
      !END DO

      !DO i=3, 7
      !   val(i) = 3.
      !END DO
      !DO i=8, n2b + n3b
      !   val(i) = 4.
      !END DO


      ! UNCOMMENT FOR FUNCTION. f7/2 Shell 

      !val(1) = v0
      !val(2) = v2
      !val(3) = v4
      !val(4) = v6
      !val(5) = v3
      !val(6) = v5
      !val(7) = v7
      !val(8) = v9
      !val(9) = v11
      !val(10)= v15




      ALLOCATE(matrix(siz,siz), lin_m(lgth),eig(siz))
      ALLOCATE(matrix_gs(siz_gs,siz_gs), lin_m_gs(lgth_gs),eig_gs(siz_gs))


      !GS_matrix

      DO l=1, lgth_gs
         me_tmp = 0
         DO k=1, n2b
            val_tmp = val(k)
            me_stmp= me_2b(k,loc_gs + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
            !write(*,*) 'gf =', me_stmp, k
         END DO
         DO k=1, n3b
            val_tmp = val(n2b+k)
            me_stmp= me_3b(k,loc_gs + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
            !write(*,*) 'gf=', me_stmp, k 
         END DO 
         lin_m_gs(l)  = me_tmp
      END DO


      cnt = 1
      col = 1
      DO i = 1, siz_gs
         DO l = col, siz_gs
            matrix_gs(i,l) = lin_m_gs(cnt)
            matrix_gs(l,i) = lin_m_gs(cnt)
            cnt = cnt +1
         END DO
         col = col + 1
      END DO


     ! Locating excited  state
      DO l=1, lgth
         me_tmp = 0
         DO k=1, n2b
            val_tmp = val(k)
            me_stmp= me_2b(k,loc + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
         END DO
         DO k=1, n3b
            val_tmp = val(n2b+k)
            me_stmp= me_3b(k,loc + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
         END DO 
         lin_m(l)  = me_tmp
      END DO


      cnt = 1
      col = 1
      DO i = 1, siz
         DO l = col, siz
             matrix(i,l) = lin_m(cnt)
             matrix(l,i) = lin_m(cnt)
             cnt = cnt +1
         END DO
         col = col + 1
      END DO



     !Symmetrizing matrix
     !write(*,*) siz
     write(*,*) siz
     call diasym(matrix,eig,siz)

     !write(*,*) matrix, siz
     write(*,*) siz_gs
     call diasym(matrix_gs,eig_gs,siz_gs)

     


     !Ordering energies

     ALLOCATE(iperm(siz),iperm_gs(siz_gs))


     CALL DPSORT(eig,siz,iperm,2,ier)

     CALL DPSORT(eig_gs,siz_gs,iperm_gs,2,ier)



     !Calculating requested energy
      NSM_F72 = eig(el)-eig_gs(1) 


      RETURN
      END 


      subroutine diasym(a,eig,n)
      implicit none

      integer n,l,inf
      real*8  a(n,n),eig(n),work(n*(3+n/2))

      l=n*(3+n/2)
      call dsyev('V','U',n,a,n,eig,work,l,inf)
      end subroutine diasym

