MODULE MOD_LIKELIHOOD
  ! Automatic Time-stamp: <Last changed by martino on Wednesday 18 March 2020 at CET 12:33:51>
  ! Module of likelihood test function, no real data are involved here

    
  
  !#####################################################################################################################
  
  ! IMPORTANT: to switch between likelihood types for test and others,
  ! change the name of the file to compile in the Makefile,
  ! Mod_likeihood.f90 or Mod_likeihood_tests.f90
  
  !#####################################################################################################################
  
  

  ! Module for the input parameter definition
  USE MOD_PARAMETERS

  IMPLICIT NONE


CONTAINS


  SUBROUTINE INIT_LIKELIHOOD()
    ! Initialize the normal likelihood with data files and special function

    WRITE(*,*) 'Initialization of test likelihood function'

  END SUBROUTINE INIT_LIKELIHOOD


 
  REAL(8) FUNCTION LOGLIKELIHOOD(par)
    ! Main likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: par

    ! Select the test function
    IF (funcname.eq.'TEST_GAUSS') THEN
       LOGLIKELIHOOD = TEST_GAUSS(par)
    ELSE
       WRITE(*,*) 'Error of the function name in Mod_likelihood_test module'
       WRITE(*,*) 'Check the manual and the input file'
       STOP
    END IF


  END FUNCTION LOGLIKELIHOOD

  !#####################################################################################################################

  SUBROUTINE FINAL_LIKELIHOOD(live_max,par_mean,par_median_w)
    ! Final action for the likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w

    WRITE(*,*) 'End of likelihood test'

  END SUBROUTINE FINAL_LIKELIHOOD


  !#####################################################################################################################
  !############################################## AVAILABLE FUNCTIONS ################################################## 
  !#####################################################################################################################

  REAL(8) FUNCTION TEST_GAUSS(par)
    
    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), PARAMETER :: x0=0., sigma=1., amp=1.
    REAL(8) :: x

    x = par(1)
    
    IF(DABS(-(x-x0)**2/(2*sigma**2)).LT.700) THEN
       TEST_GAUSS = amp/(dsqrt(2*pi)*sigma)*dexp(-(x-x0)**2/(2*sigma**2))
    ELSE
       TEST_GAUSS = 0.d0
    END IF
    

  END FUNCTION TEST_GAUSS
 
  !#####################################################################################################################   

END MODULE MOD_LIKELIHOOD
