MODULE MOD_POTENTIALS
  ! Module of energy potential landscapes, no real data are involved here

  ! Module for the input parameter definition
  USE MOD_PARAMETERS !, ONLY: npar, funcname, funcid, searchid
#ifdef OPENMPI_ON
  USE MPI
#endif

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

    ! All potentials are legacy function (for the moment)
    LEGACY_USERFCN = .TRUE.
    
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

    RETURN
  END

  REAL(8) FUNCTION LOGLIKELIHOOD_POT(npar, par)
    ! Main likelihood function

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(npar), INTENT(IN) :: par

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
    END SELECT


  END FUNCTION LOGLIKELIHOOD_POT


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


END MODULE MOD_POTENTIALS
