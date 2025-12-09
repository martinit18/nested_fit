MODULE MOD_POTENTIALS
  ! Module of energy potential landscapes, no real data are involved here

  ! Module for the input parameter definition
  USE MOD_PARAMETERS !, ONLY: npar, funcname, funcid, searchid

  ! Module for logging
  USE MOD_LOGGER

  IMPLICIT NONE
  PUBLIC :: SELECT_LIKELIHOODFCN

  REAL(8)    :: a_norm=0.

CONTAINS


  SUBROUTINE INIT_LIKELIHOOD_POT()
    ! Initialize the normal likelihood with data files and special function
    
    CALL LOG_TRACE('Initialization of func likelihood.')

    !IF (funcname.eq.'TEST_ROSENBROCK') THEN
    !   CALL INIT_ROSENBROCK()
    !END IF
    
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
        SELECT_LIKELIHOODFCN = 2
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
       en_decomp = Q_ENERGY_HARM_3D(npar, par)
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
       LOGLIKELIHOOD_POT_WRITE = Q_ENERGY_HARM_3D(npar, par)
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
    !> Potential of the form 4*eps*((rij/r0)**12-(rij/r0)**6) with rij=sqrt((x_i-x_j)**2+(y_i-y_j)**2+(z_i-z_j)**2) with periodic boundary conditions

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


END MODULE MOD_POTENTIALS
