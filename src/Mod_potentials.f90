MODULE MOD_POTENTIALS
  ! Module of energy potential landscapes, no real data are involved here

  ! Module for the input parameter definition
  USE MOD_PARAMETERS !, ONLY: npar, funcname, funcid, searchid

  ! Module for logging
  USE MOD_LOGGER

  IMPLICIT NONE
  PUBLIC :: SELECT_LIKELIHOODFCN

  REAL(8)    :: a_norm=0., e_const = 0.

CONTAINS

  SUBROUTINE INIT_LIKELIHOOD_POT()
    ! Initialize the normal likelihood with data files and special function
    
    CALL LOG_TRACE('Initialization of func likelihood.')

    IF (funcname(1).eq.'ENERGY_LJ_3D_PBC_NORM'.or.funcname(1).eq.'ENERGY_LJ_2D_PBC_NORM') THEN
      CALL INIT_ENERGY_LJ_PBC_NORM()
    END IF
    
    funcid = SELECT_LIKELIHOODFCN(funcname(1))

  END SUBROUTINE INIT_LIKELIHOOD_POT

  !#####################################################################################################################


  REAL(8) FUNCTION LOGLIKELIHOOD_WITH_TEST_POT(npar, par)
    ! Make some tests first if required

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(npar), INTENT(IN) :: par


    LOGLIKELIHOOD_WITH_TEST_POT = LOGLIKELIHOOD_POT(npar,par)

  END FUNCTION LOGLIKELIHOOD_WITH_TEST_POT

  !------------------------------------------------------------------------------------------------------------------------
  ! TODO(César): In reality this should be in another file, but I am following the previous "rules"...
  FUNCTION SELECT_LIKELIHOODFCN(funcname)
    IMPLICIT NONE
    INTEGER*4 SELECT_LIKELIHOODFCN
    CHARACTER*64 funcname

    IF(calc_mode.EQ.'POTENTIAL') THEN
      IF(funcname.eq.'ENERGY_HARM_3D') THEN
        SELECT_LIKELIHOODFCN = 0
      ELSE IF(funcname.eq.'ENERGY_LJ_3D_PBC') THEN
        SELECT_LIKELIHOODFCN = 1
      ELSE IF(funcname.eq.'ENERGY_LJ_3D_PBC_NORM') THEN
        SELECT_LIKELIHOODFCN = 2
      ELSE IF(funcname.eq.'ENERGY_LJ_2D_PBC_NORM') THEN
        SELECT_LIKELIHOODFCN = 3
      ELSE
        ! SELECT_LIKELIHOODFCN = -1
        CALL LOG_ERROR_HEADER()
        CALL LOG_ERROR('You selected the calculation mode`'//TRIM(calc_mode)//'`.')
        CALL LOG_ERROR('Select a valid potential to be explored.')
        CALL LOG_ERROR('Aborting Execution...')
        CALL LOG_ERROR_HEADER()
      END IF
    ELSE IF(calc_mode.EQ.'Q_POTENTIAL') THEN
      IF(funcname.eq.'Q_ENERGY_HARM_3D') THEN
        SELECT_LIKELIHOODFCN = 4
      ELSE IF(funcname.eq.'Q_ENERGY_LJ_3D_PBC') THEN
        SELECT_LIKELIHOODFCN = 5
      ELSE IF(funcname.eq.'Q_ENERGY_LJ_3D_LP') THEN
        SELECT_LIKELIHOODFCN = 6
      ELSE IF(funcname.eq.'Q_ENERGY_LJ_2D_PBC') THEN
        SELECT_LIKELIHOODFCN = 7
      ELSE IF(funcname.eq.'Q_ENERGY_LJ_2D_LP') THEN
        SELECT_LIKELIHOODFCN = 8
      ELSE IF(funcname.eq.'Q_ENERGY_LJ_2D_LP_PRIOR') THEN
        SELECT_LIKELIHOODFCN = 9
      ELSE
        ! SELECT_LIKELIHOODFCN = -1
        CALL LOG_ERROR_HEADER()
        CALL LOG_ERROR('You selected the calculation mode`'//TRIM(calc_mode)//'`.')
        CALL LOG_ERROR('Select a valid potential to be explored.')
        CALL LOG_ERROR('Aborting Execution...')
        CALL LOG_ERROR_HEADER()
      END IF
    END IF

    RETURN
  END

  REAL(8) FUNCTION LOGLIKELIHOOD_POT(npar, par)
    ! Main likelihood function

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    REAL(8), DIMENSION(4) :: en_decomp ! To use with quantum potential that output the decomposition of the energy into averaged potential and replica interaction as well as the temperature

    !$OMP CRITICAL
    ncall = ncall + 1
    IF(ncall == 1.E+9) THEN
       ncall9=ncall9+1
       ncall=0
    END IF
    !$OMP END CRITICAL
    
    ! Select the test function
    SELECT CASE (funcid)
    CASE (0)
       LOGLIKELIHOOD_POT = ENERGY_HARM_3D(npar, par)
    CASE (1)
       LOGLIKELIHOOD_POT = ENERGY_LJ_3D_PBC(npar, par)
    CASE (2)
       LOGLIKELIHOOD_POT = ENERGY_LJ_3D_PBC_NORM(npar, par)
    CASE (3)
       LOGLIKELIHOOD_POT = ENERGY_LJ_2D_PBC_NORM(npar, par)
    CASE (4)
       en_decomp = Q_ENERGY_HARM_3D(npar, par)
       LOGLIKELIHOOD_POT = -en_decomp(1)
    CASE (5)
       en_decomp = Q_ENERGY_LJ_3D_PBC(npar, par)
       LOGLIKELIHOOD_POT = -en_decomp(1)
    CASE (6)
       en_decomp = Q_ENERGY_LJ_3D_LP(npar, par)
       LOGLIKELIHOOD_POT = -en_decomp(1)
    CASE (7)
       en_decomp = Q_ENERGY_LJ_2D_PBC(npar, par)
       LOGLIKELIHOOD_POT = -en_decomp(1)
    CASE (8)
       en_decomp = Q_ENERGY_LJ_2D_LP(npar, par)
       LOGLIKELIHOOD_POT = -en_decomp(1)
    CASE (9)
       en_decomp = Q_ENERGY_LJ_2D_LP_PRIOR(npar, par)
       LOGLIKELIHOOD_POT = -en_decomp(1)
    END SELECT


  END FUNCTION LOGLIKELIHOOD_POT


  FUNCTION LOGLIKELIHOOD_POT_WRITE(npar, par)
    ! Likelihood function for writing

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    REAL(8), DIMENSION(4) :: LOGLIKELIHOOD_POT_WRITE ! When not quantum, vector completed with 0 and sign need to be changed

    ! Select the test function
    SELECT CASE (funcid)
    CASE (0)
       LOGLIKELIHOOD_POT_WRITE = (/-ENERGY_HARM_3D(npar, par), 0.d0, 0.d0, 0.d0/)
    CASE (1)
       LOGLIKELIHOOD_POT_WRITE = (/-ENERGY_LJ_3D_PBC(npar, par), 0.d0, 0.d0, 0.d0/)
    CASE (2)
       LOGLIKELIHOOD_POT_WRITE = (/-ENERGY_LJ_3D_PBC_NORM(npar, par), 0.d0, 0.d0, 0.d0/)
    CASE (3)
       LOGLIKELIHOOD_POT_WRITE = (/-ENERGY_LJ_2D_PBC_NORM(npar, par), 0.d0, 0.d0, 0.d0/)
    CASE (4)
       LOGLIKELIHOOD_POT_WRITE = Q_ENERGY_HARM_3D(npar, par)
    CASE (5)
       LOGLIKELIHOOD_POT_WRITE = Q_ENERGY_LJ_3D_PBC(npar, par)
    CASE (6)
       LOGLIKELIHOOD_POT_WRITE = Q_ENERGY_LJ_3D_LP(npar, par)
    CASE (7)
       LOGLIKELIHOOD_POT_WRITE = Q_ENERGY_LJ_2D_PBC(npar, par)
    CASE (8)
       LOGLIKELIHOOD_POT_WRITE = Q_ENERGY_LJ_2D_LP(npar, par)
    CASE (9)
       LOGLIKELIHOOD_POT_WRITE = Q_ENERGY_LJ_2D_LP_PRIOR(npar, par)
    END SELECT


  END FUNCTION LOGLIKELIHOOD_POT_WRITE


  !#####################################################################################################################

  SUBROUTINE FINAL_LIKELIHOOD_POT(live_max,par_mean,par_median_w)
    ! Final action for the likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w

    CALL LOG_MESSAGE_HEADER()
    CALL LOG_MESSAGE('End of potential exploration.')
    CALL LOG_MESSAGE('Number of calls : '//TRIM(ADJUSTL(INT8_TO_STR_INLINE(ncall))))
    CALL LOG_MESSAGE_HEADER()
    ! OPEN(11,FILE='nf_output_n_likelihood_calls.txt',STATUS= 'UNKNOWN')
    ! WRITE(11,*) ncall
    ! CLOSE(11)

  END SUBROUTINE FINAL_LIKELIHOOD_POT


  !#####################################################################################################################
  !############################################## AVAILABLE FUNCTIONS ##################################################
  !#####################################################################################################################


  REAL(8) FUNCTION ENERGY_HARM_3D(npar, par)
    !> The parameters are the positions of the points (...,x_i,y_i,z_i,....)
    !> Potential of the form eps*SUM(x-i**2+y_i**2+z_i**2)

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), PARAMETER ::  eps=1.
    REAL(8), DIMENSION(npar) :: x     
    INTEGER(4) :: N, i
    REAL(8) :: rij, ener

    x = par
    N=INT(npar/3)
    
    ener=0.
    DO i=1,N
      rij=(x(i*3-2))**2+(x(i*3-1))**2+(x(i*3))**2
      ener=ener+eps*(rij)   
    END DO
    
    ENERGY_HARM_3D=-ener
  END FUNCTION ENERGY_HARM_3D


!#####################################################################################################################   

  REAL(8) FUNCTION ENERGY_LJ_3D_PBC(npar, par)
    !> The parameters are the positions of the points (...,x_i,y_i,z_i,....)
    !> Potential of the form 4*eps*((r0/rij)**12-(r0/rij)**6) with rij=sqrt((x_i-x_j)**2+(y_i-y_j)**2+(z_i-z_j)**2) with periodic boundary conditions

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), PARAMETER ::  eps=1.
    REAL(8), DIMENSION(npar-1) :: x     
    INTEGER(4) :: N, i, j
    REAL(8) :: rij, ener, r0, dx, dy, dz, box_x, box_y, box_z
    
    r0=par(1)
    x = par(2:)
    N=INT(npar/3)
    box_x=par_bnd2(2)-par_bnd1(2)
    box_y=par_bnd2(3)-par_bnd1(3)
    box_z=par_bnd2(4)-par_bnd1(4)
    
    ener=0.
    DO i=1,N
      DO j=i+1,N
        dx=x(i*3-2)-x(j*3-2)
        dy=x(i*3-1)-x(j*3-1)
        dz=x(i*3)-x(j*3)
        dx=dx-box_x*NINT(dx/box_x)
        dy=dy-box_y*NINT(dy/box_y)
        dz=dz-box_z*NINT(dz/box_z)
        rij=SQRT(dx**2+dy**2+dz**2)
        IF(rij<=3*r0) ener=ener+4*eps*((r0/rij)**12-(r0/rij)**6-(1./3.)**12+(1./3.)**6) 
      END DO    
    END DO
    
    !ener=ener+0.5*k*(SUM(x(1:npar:2))**2+SUM(x(2:npar:2))**2)!/npar

    ENERGY_LJ_3D_PBC=-ener!*1./N
  END FUNCTION ENERGY_LJ_3D_PBC

  !#####################################################################################################################   
  SUBROUTINE INIT_ENERGY_LJ_PBC_NORM()
    ! Initialize the normalization constant for the LJ potential, which is the value of the potential at the minimum, i.e. -eps

    REAL(8) :: r0, eps
    REAL(8), PARAMETER :: r_max=3.
    e_const = (1/r_max)**12-(1/r_max)**6
    

  END SUBROUTINE INIT_ENERGY_LJ_PBC_NORM
  
  !--------------------------------------------------------------------------------------------------------------
  
  REAL(8) FUNCTION ENERGY_LJ_3D_PBC_NORM(npar, par)
    !> The parameters are the positions of the points (...,x_i,y_i,z_i,....)
    !> Potential of the form 4*eps*((r0/rij)**12-(r0/rij)**6) with rij=sqrt((x_i-x_j)**2+(y_i-y_j)**2+(z_i-z_j)**2) with periodic boundary conditions
    !> now in a normalized box

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    ! REAL(8), PARAMETER ::  eps=1.,r_max=3.
    REAL(8), DIMENSION(npar-1) :: x     
    INTEGER(4) :: N, i, j
    REAL(8) :: rij, ener, r0, dx, dy, dz
    
    r0=par(1)
    x = par(2:)
    N=INT(npar/3)
    
    ener=0.
    DO i=1,N
      DO j=i+1,N
        dx=x(i*3-2)-x(j*3-2)
        dy=x(i*3-1)-x(j*3-1)
        dz=x(i*3)-x(j*3)
        dx=dx-NINT(dx)
        dy=dy-NINT(dy)
        dz=dz-NINT(dz)
        rij=SQRT(dx**2+dy**2+dz**2)
        IF(rij<=3*r0) ener=ener+4*((r0/rij)**12-(r0/rij)**6-e_const)
      END DO    
    END DO
    
    !ener=ener+0.5*k*(SUM(x(1:npar:2))**2+SUM(x(2:npar:2))**2)!/npar

    ENERGY_LJ_3D_PBC_NORM=-ener!*1./N
  END FUNCTION ENERGY_LJ_3D_PBC_NORM
  
  !--------------------------------------------------------------------------------------------------------------
  
  REAL(8) FUNCTION ENERGY_LJ_2D_PBC_NORM(npar, par)
    !> The parameters are the positions of the points (...,x_i,y_i,....)
    !> Potential of the form 4*eps*((r0/rij)**12-(r0/rij)**6) with rij=sqrt((x_i-x_j)**2+(y_i-y_j)**2) with periodic boundary conditions
    !> now in a normalized box

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    ! REAL(8), PARAMETER ::  eps=1.,r_max=3.
    REAL(8), DIMENSION(npar-1) :: x     
    INTEGER(4) :: N, i, j
    REAL(8) :: rij, ener, r0, dx, dy
    
    r0=par(1)
    x = par(2:)
    N=INT(npar/2)
    
    ener=0.
    DO i=1,N
      DO j=i+1,N
        dx=x(i*2-1)-x(j*2-1)
        dy=x(i*2-0)-x(j*2-0)
        dx=dx-NINT(dx)
        dy=dy-NINT(dy)
        rij=SQRT(dx**2+dy**2)
        IF(rij<=3*r0) ener=ener+4*((r0/rij)**12-(r0/rij)**6-e_const)
      END DO    
    END DO
    
    !ener=ener+0.5*k*(SUM(x(1:npar:2))**2+SUM(x(2:npar:2))**2)!/npar

    ENERGY_LJ_2D_PBC_NORM=-ener!*1./N
  END FUNCTION ENERGY_LJ_2D_PBC_NORM

!##################################################################################################################### 
  FUNCTION Q_ENERGY_HARM_3D(npar, par)
    !> First parameters N (number of atoms), P (number of replicas), tau (temperature), m (mass of atoms), omega (harmonic potential parameters) 
    !> Parameters are in the order (x_11,y_11,z_11,...,x_1N,y_1N,z_1N,...,x_ij,y_ij,z_ij,...,x_P1,y_P1,z_P1,...,x_PN,y_PN,z_PN)
    !> with 1<=i<=P, 1<=j<=N
    !> hbar=1, k_b=1

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), DIMENSION(4) :: Q_ENERGY_HARM_3D
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8) ::  eps
    REAL(8), DIMENSION(SIZE(par)-5+3*INT(par(1))) :: x     
    INTEGER(4) :: N, i, j, P, k, ind_xi, ind_xj
    REAL(8) :: rij, ener, omega, tau, m, rharm, harm, V

    N=INT(par(1))
    P=INT(par(2))
    tau=par(3)
    m=par(4)
    omega=par(5)
    eps=0.5*m*omega**2
    x(:3*N*P) = par(6:)
    x((3*N*P+1):) = par(6:(5+3*N)) !Replica P+1 is replica 1
    ener=0.
    harm=0.
    V=0.
    DO k=1,P
      DO i=1,N
        ind_xi=(k-1)*3*N+(i-1)*3 !starting index for i
        rharm=(x(ind_xi+1)-x(ind_xi+3*N+1))**2+(x(ind_xi+2)-x(ind_xi+3*N+2))**2+(x(ind_xi+3)-x(ind_xi+3*N+3))**2 !harmonic interaction between neighbouring replicas
        harm=harm+0.5*P*tau**2*m*rharm
        rij=(x(ind_xi+1))**2+(x(ind_xi+2))**2+(x(ind_xi+3))**2
        V=V+eps*(rij)/P
      END DO
    END DO
    ener = V + harm
    Q_ENERGY_HARM_3D=(/ener,V,harm,tau/)
  END FUNCTION Q_ENERGY_HARM_3D

  !#####################################################################################################################
  
  FUNCTION Q_ENERGY_LJ_3D_PBC(npar, par)
    !> First parameters N (number of atoms), P (number of replicas), tau (temperature), m (mass of atoms), r0 (harmonic potential parameter) 
    !> Parameters are in the order (x_11,y_11,z_11,...,x_1N,y_1N,z_1N,...,x_ij,y_ij,z_ij,...,x_P1,y_P1,z_P1,...,x_PN,y_PN,z_PN)
    !> with 1<=i<=P, 1<=j<=N
    !> hbar=1, k_b=1
    
    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), DIMENSION(4) :: Q_ENERGY_LJ_3D_PBC
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8) ::  eps=1
    REAL(8), DIMENSION(SIZE(par)-5+3*INT(par(1))) :: x     
    INTEGER(4) :: N, i, j, P, k, ind_xi, ind_xj
    REAL(8) :: rij, ener, r0, tau, m, rharm, dx, dy, dz, box_x, box_y, box_z, harm, V
    
    N=INT(par(1))
    P=INT(par(2))
    tau=par(3)
    m=par(4)
    r0=par(5)
    x(:3*N*P) = par(6:)
    x((3*N*P+1):)=par(6:(5+3*N))
    box_x=par_bnd2(6)-par_bnd1(6)
    box_y=par_bnd2(7)-par_bnd1(7)
    box_z=par_bnd2(8)-par_bnd1(8)
    
    ener=0.
    harm=0.
    V=0.
    DO k=1,P
       DO i=1,N
          ind_xi=(k-1)*3*N+(i-1)*3 !starting index for i
          dx=x(ind_xi+1)-x(ind_xi+3*N+1)
          dy=x(ind_xi+2)-x(ind_xi+3*N+2)
          dz=x(ind_xi+3)-x(ind_xi+3*N+3)
          rharm=dx**2+dy**2+dz**2 !harmonic interaction between neighbouring replicas
          harm=harm+0.5*P*tau**2*m*rharm !0.5*P*tau**2*m*rharm
          DO j=i+1,N
             ind_xj=(k-1)*3*N+(j-1)*3 !starting index for j
             dx=x(ind_xi+1)-x(ind_xj+1)
             dy=x(ind_xi+2)-x(ind_xj+2)
             dz=x(ind_xi+3)-x(ind_xj+3)
             dx=dx-box_x*NINT(dx/box_x)
             dy=dy-box_y*NINT(dy/box_y)
             dz=dz-box_z*NINT(dz/box_z)
             rij=SQRT(dx**2+dy**2+dz**2)
             IF(rij<=3*r0) V=V+4./P*eps*((r0/rij)**12-(r0/rij)**6-(1./3.)**12+(1./3.)**6)     
          END DO
       END DO
    END DO
    ener = V + harm
    
    Q_ENERGY_LJ_3D_PBC=(/ener,V,harm,tau/)
  END FUNCTION Q_ENERGY_LJ_3D_PBC
  
  FUNCTION Q_ENERGY_LJ_3D_LP(npar, par)
    !> First parameters N (number of atoms), P (number of replicas), tau (temperature), m (mass of atoms), r0 (harmonic potential parameter) 
    !> Parameters are in the order (x_11,y_11,z_11,...,x_1N,y_1N,z_1N,...,x_ij,y_ij,z_ij,...,x_P1,y_P1,z_P1,...,x_PN,y_PN,z_PN)
    !> with 1<=i<=P, 1<=j<=N
    !> hbar=1, k_b=1
    
    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), DIMENSION(4) :: Q_ENERGY_LJ_3D_LP
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8) ::  eps=1.
    REAL(8), DIMENSION(SIZE(par)-5) :: x
    REAL(8), DIMENSION(SIZE(par)-5+3*INT(par(1))) :: y
    !REAL(8), DIMENSION(3*INT(par(1))) :: x_c     
    INTEGER(4) :: N, i, j, P, k, ind_xi, ind_xj
    REAL(8) :: rij, ener, r0, tau, m, rharm, lambda_P2, dx, dy, dz, box_x, box_y, box_z, V, harm
    
    N = INT(par(1))
    P = INT(par(2))
    tau = 1/par(3) ! Attention: parameter used here is beta = 1/tau. Change if other parameter in input file
    m = par(4)
    r0 = par(5)
    lambda_P2 = 1./(m*P*tau**2)
    y(:(3*N*(P-1))) = par((6+3*N):) ! First P-1 beads
    DO i=1,3*N
       y(3*N*(P-1)+i) = -SUM(y(i:(3*N*(P-1)):(3*N))) ! Last bead
    END DO
    y((3*N*P+1):) = par((6+3*N):(5+2*3*N)) ! Add first bead at the end
    DO i=1,P-1
       x((3*N*(i-1)+1):(3*N*i)) = par(6:(5+3*N))+SQRT(lambda_P2)*par((6+3*N*i):(5+3*N*(i+1))) ! First P-1 beads
    END DO
    DO i=1,3*N
       x(3*N*(P-1)+i) = par(5+i)-SQRT(lambda_P2)*SUM(par((5+3*N+i)::(3*N))) ! Last bead
    END DO
    box_x=par_bnd2(6)-par_bnd1(6)
    box_y=par_bnd2(7)-par_bnd1(7)
    box_z=par_bnd2(8)-par_bnd1(8)
    
    
    ener=0.
    harm=0.
    V=0.
    DO k=1,P
       DO i=1,N
          ind_xi=(k-1)*3*N+(i-1)*3 !starting index for i
          dx=y(ind_xi+1)-y(ind_xi+3*N+1)
          dy=y(ind_xi+2)-y(ind_xi+3*N+2)
          dz=y(ind_xi+3)-y(ind_xi+3*N+3)
          rharm=dx**2+dy**2+dz**2 !harmonic interaction between neighbouring replicas
          harm=harm+0.5*rharm !0.5*P*tau**2*m*rharm
          DO j=i+1,N
             ind_xj=(k-1)*3*N+(j-1)*3 !starting index for j
             dx=x(ind_xi+1)-x(ind_xj+1)
             dy=x(ind_xi+2)-x(ind_xj+2)
             dz=x(ind_xi+3)-x(ind_xj+3)
             dx=dx-box_x*NINT(dx/box_x)
             dy=dy-box_y*NINT(dy/box_y)
             dz=dz-box_z*NINT(dz/box_z)
             rij=SQRT(dx**2+dy**2+dz**2)
             IF(rij<=3*r0) V=V+4./P*eps*((r0/rij)**12-(r0/rij)**6-(1./3.)**12+(1./3.)**6)
             
          END DO
       END DO
    END DO
    ener = V + harm
    
    Q_ENERGY_LJ_3D_LP=(/ener,V,harm,tau/)
  END FUNCTION Q_ENERGY_LJ_3D_LP

  FUNCTION Q_ENERGY_LJ_2D_PBC(npar, par)
    !> First parameters N (number of atoms), P (number of replicas), tau (temperature), m (mass of atoms), r0 (harmonic potential parameter) 
    !> Parameters are in the order (x_11,y_11,...,x_1N,y_1N,...,x_ij,y_ij,...,x_P1,y_P1,...,x_PN,y_PN)
    !> with 1<=i<=P, 1<=j<=N
    !> hbar=1, k_b=1
    
    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), DIMENSION(4) :: Q_ENERGY_LJ_2D_PBC
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8) ::  eps=1
    REAL(8), DIMENSION(SIZE(par)-5+2*INT(par(1))) :: x     
    INTEGER(4) :: N, i, j, P, k, ind_xi, ind_xj
    REAL(8) :: rij, ener, r0, tau, m, rharm, dx, dy, box_x, box_y, harm, V
    
    N=INT(par(1))
    P=INT(par(2))
    tau=par(3)
    m=par(4)
    r0=par(5)
    x(:2*N*P) = par(6:)
    x((2*N*P+1):)=par(6:(5+2*N))
    box_x=par_bnd2(6)-par_bnd1(6)
    box_y=par_bnd2(7)-par_bnd1(7)
    
    ener=0.
    harm=0.
    V=0.
    DO k=1,P
       DO i=1,N
          ind_xi=(k-1)*2*N+(i-1)*2 !starting index for i
          dx=x(ind_xi+1)-x(ind_xi+2*N+1)
          dy=x(ind_xi+2)-x(ind_xi+2*N+2)
          rharm=dx**2+dy**2 !harmonic interaction between neighbouring replicas
          harm=harm+0.5*P*tau**2*m*rharm !0.5*P*tau**2*m*rharm
          DO j=i+1,N
             ind_xj=(k-1)*2*N+(j-1)*2 !starting index for j
             dx=x(ind_xi+1)-x(ind_xj+1)
             dy=x(ind_xi+2)-x(ind_xj+2)
             dx=dx-box_x*NINT(dx/box_x)
             dy=dy-box_y*NINT(dy/box_y)
             rij=SQRT(dx**2+dy**2)
             IF(rij<=3*r0) V=V+4./P*eps*((r0/rij)**12-(r0/rij)**6-(1./3.)**12+(1./3.)**6)     
          END DO
       END DO
    END DO
    ener = V + harm
    
    Q_ENERGY_LJ_2D_PBC=(/ener,V,harm,tau/)
  END FUNCTION Q_ENERGY_LJ_2D_PBC

  FUNCTION Q_ENERGY_LJ_2D_LP(npar, par)
    !> First parameters N (number of atoms), P (number of replicas), tau (temperature), m (mass of atoms), r0 (harmonic potential parameter) 
    !> Parameters are in the order (x_11,y_11,...,x_1N,y_1N,...,x_ij,y_ij,...,x_P1,y_P1,...,x_PN,y_PN)
    !> with 1<=i<=P, 1<=j<=N
    !> hbar=1, k_b=1
    
    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), DIMENSION(4) :: Q_ENERGY_LJ_2D_LP
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8) ::  eps=1.
    REAL(8), DIMENSION(SIZE(par)-5) :: x
    REAL(8), DIMENSION(SIZE(par)-5+2*INT(par(1))) :: y
    !REAL(8), DIMENSION(3*INT(par(1))) :: x_c     
    INTEGER(4) :: N, i, j, P, k, ind_xi, ind_xj
    REAL(8) :: rij, ener, r0, tau, m, rharm, lambda_P2, dx, dy, box_x, box_y, V, harm
    
    N = INT(par(1))
    P = INT(par(2))
    tau = 1/par(3) ! Attention: parameter used here is beta = 1/tau. Change if other parameter in input file
    m = par(4)
    r0 = par(5)
    lambda_P2 = 1./(m*P*tau**2)
    y(:(2*N*(P-1))) = par((6+2*N):) ! First P-1 beads
    DO i=1,2*N
       y(2*N*(P-1)+i) = -SUM(y(i:(2*N*(P-1)):(2*N))) ! Last bead
    END DO
    y((2*N*P+1):) = par((6+2*N):(5+2*2*N)) ! Add first bead at the end
    DO i=1,P-1
       x((2*N*(i-1)+1):(2*N*i)) = par(6:(5+2*N))+SQRT(lambda_P2)*par((6+2*N*i):(5+2*N*(i+1))) ! First P-1 beads
    END DO
    DO i=1,2*N
       x(2*N*(P-1)+i) = par(5+i)-SQRT(lambda_P2)*SUM(par((5+2*N+i)::(2*N))) ! Last bead
    END DO
    box_x=par_bnd2(6)-par_bnd1(6)
    box_y=par_bnd2(7)-par_bnd1(7)
    
    
    ener=0.
    harm=0.
    V=0.
    DO k=1,P
       DO i=1,N
          ind_xi=(k-1)*2*N+(i-1)*2 !starting index for i
          dx=y(ind_xi+1)-y(ind_xi+2*N+1)
          dy=y(ind_xi+2)-y(ind_xi+2*N+2)
          rharm=dx**2+dy**2 !harmonic interaction between neighbouring replicas
          harm=harm+0.5*rharm !0.5*P*tau**2*m*rharm
          DO j=i+1,N
             ind_xj=(k-1)*2*N+(j-1)*2 !starting index for j
             dx=x(ind_xi+1)-x(ind_xj+1)
             dy=x(ind_xi+2)-x(ind_xj+2)
             dx=dx-box_x*NINT(dx/box_x)
             dy=dy-box_y*NINT(dy/box_y)
             rij=SQRT(dx**2+dy**2)
             IF(rij<=3*r0) V=V+4./P*eps*((r0/rij)**12-(r0/rij)**6-(1./3.)**12+(1./3.)**6)
             
          END DO
       END DO
    END DO
    ener = V + harm
    
    Q_ENERGY_LJ_2D_LP=(/ener,V,harm,tau/)
  END FUNCTION Q_ENERGY_LJ_2D_LP

  FUNCTION Q_ENERGY_LJ_2D_LP_PRIOR(npar, par)
    !> First parameters N (number of atoms), P (number of replicas), tau (temperature), m (mass of atoms), r0 (harmonic potential parameter) 
    !> Parameters are in the order (x_11,y_11,...,x_1N,y_1N,...,x_ij,y_ij,...,x_P1,y_P1,...,x_PN,y_PN)
    !> with 1<=i<=P, 1<=j<=N
    !> hbar=1, k_b=1
    
    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), DIMENSION(4) :: Q_ENERGY_LJ_2D_LP_PRIOR
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    INTEGER(4), PARAMETER :: first_coord = 7
    REAL(8) ::  eps=1.
    REAL(8), DIMENSION(SIZE(par)-(first_coord-1)) :: x
    REAL(8), DIMENSION(SIZE(par)-(first_coord-1)+2*INT(par(1))) :: y
    !REAL(8), DIMENSION(3*INT(par(1))) :: x_c     
    INTEGER(4) :: N, i, j, P, k, ind_xi, ind_xj
    REAL(8) :: rij, ener, r0, tau, nu, m, rharm, lambda_P2, dx, dy, box_x, box_y, V, harm
    
    N = INT(par(1))
    P = INT(par(2))
    ! FOR UNIFORM SAMPLING IN beta^nu: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! par(3) = beta^nu
    ! beta = par(3)^(1/nu)
    ! tau = 1/beta = 1/par(3)^(1/nu) = par(3)^(-1/nu)
    nu = par(6) ! for prior Uniform(beta^nu)
    tau = par(3)**(-1/nu) ! Attention: parameter used here is beta^nu = tau^(-nu). Change if other parameter in input file
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    m = par(4)
    r0 = par(5)
    ! first_coord = 7 ! npar of 1st coordinate
    lambda_P2 = 1./(m*P*tau**2)
    y(:(2*N*(P-1))) = par((first_coord+2*N):) ! First P-1 beads
    DO i=1,2*N
       y(2*N*(P-1)+i) = -SUM(y(i:(2*N*(P-1)):(2*N))) ! Last bead
    END DO
    y((2*N*P+1):) = par((first_coord+2*N):(first_coord-1+2*2*N)) ! Add first bead at the end
    DO i=1,P-1
       x((2*N*(i-1)+1):(2*N*i)) = par(first_coord:(first_coord-1+2*N))+SQRT(lambda_P2)*par((first_coord+2*N*i):(first_coord-1+2*N*(i+1))) ! First P-1 beads
    END DO
    DO i=1,2*N
       x(2*N*(P-1)+i) = par(first_coord-1+i)-SQRT(lambda_P2)*SUM(par((first_coord-1+2*N+i)::(2*N))) ! Last bead
    END DO
    box_x=par_bnd2(first_coord)-par_bnd1(first_coord)
    box_y=par_bnd2(first_coord+1)-par_bnd1(first_coord+1)
    
    
    ener=0.
    harm=0.
    V=0.
    DO k=1,P
       DO i=1,N
          ind_xi=(k-1)*2*N+(i-1)*2 !starting index for i
          dx=y(ind_xi+1)-y(ind_xi+2*N+1)
          dy=y(ind_xi+2)-y(ind_xi+2*N+2)
          rharm=dx**2+dy**2 !harmonic interaction between neighbouring replicas
          harm=harm+0.5*rharm !0.5*P*tau**2*m*rharm
          DO j=i+1,N
             ind_xj=(k-1)*2*N+(j-1)*2 !starting index for j
             dx=x(ind_xi+1)-x(ind_xj+1)
             dy=x(ind_xi+2)-x(ind_xj+2)
             dx=dx-box_x*NINT(dx/box_x)
             dy=dy-box_y*NINT(dy/box_y)
             rij=SQRT(dx**2+dy**2)
             IF(rij<=3*r0) V=V+4./P*eps*((r0/rij)**12-(r0/rij)**6-(1./3.)**12+(1./3.)**6)
             
          END DO
       END DO
    END DO
    ener = V + harm
    
    Q_ENERGY_LJ_2D_LP_PRIOR=(/ener,V,harm,tau/)
  END FUNCTION Q_ENERGY_LJ_2D_LP_PRIOR
  
  


END MODULE MOD_POTENTIALS