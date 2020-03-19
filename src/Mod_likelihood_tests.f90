MODULE MOD_LIKELIHOOD
  ! Automatic Time-stamp: <Last changed by martino on Thursday 19 March 2020 at CET 15:06:43>
  ! Module of likelihood test function, no real data are involved here

    
  
  !#####################################################################################################################
  
  ! IMPORTANT: to switch between likelihood types for test and others,
  ! change the name of the file to compile in the Makefile,
  ! Mod_likeihood.f90 or Mod_likeihood_tests.f90
  
  !#####################################################################################################################
  
  

  ! Module for the input parameter definition
  USE MOD_PARAMETERS

  IMPLICIT NONE
  INTEGER(8) :: ncall=0


CONTAINS


  SUBROUTINE INIT_LIKELIHOOD()
    ! Initialize the normal likelihood with data files and special function

    WRITE(*,*) 'Initialization of test likelihood function'

  END SUBROUTINE INIT_LIKELIHOOD


 
  REAL(8) FUNCTION LOGLIKELIHOOD(par)
    ! Main likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: par

    ncall = ncall + 1

    ! Select the test function
    IF (funcname.eq.'TEST_GAUSS') THEN
       LOGLIKELIHOOD = TEST_GAUSS(par)
    ELSE IF (funcname.eq.'TEST_EGGBOX') THEN
       LOGLIKELIHOOD = TEST_EGGBOX(par)
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

    WRITE(*,*) ' '
    WRITE(*,*) 'End of likelihood test'
    WRITE(*,*) 'Number of calls : ', ncall

  END SUBROUTINE FINAL_LIKELIHOOD


  !#####################################################################################################################
  !############################################## AVAILABLE FUNCTIONS ################################################## 
  !#####################################################################################################################

  REAL(8) FUNCTION TEST_GAUSS(par)
    !> Basic multidimensional Gaussian likelihood with mean mu(:) and an uncorrelated covariance sigma(:).
    !! Inspired from polychord code
    !! 
    !! It is normalised so that it should output an evidence of 1.0 for
    !! effectively infinite priors.
    !!
    !! The mean is set at 0.5 by default, and all sigmas at 0.01
    
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), DIMENSION(SIZE(par)) :: sigma ! Standard deviation (uncorrelated) 
    REAL(8), DIMENSION(SIZE(par)) :: mu    ! Mean 
    REAL(8), DIMENSION(SIZE(par)) :: x     ! Variable to explore

    x = par

    ! Initialise the mean and standard deviation
    mu    = 5d-1   ! mean in the center
    sigma = 1d-1  ! all sigma set relatively small

    ! Gaussian normalisation
    TEST_GAUSS = - SUM( LOG( sigma ) + LOG(2*pi)/2d0 ) 

    ! theta dependence
    TEST_GAUSS = TEST_GAUSS - SUM( ( ( x - mu ) / sigma ) ** 2d0 ) / 2d0
    

  END FUNCTION TEST_GAUSS

    REAL(8) FUNCTION TEST_EGGBOX(par)
    !> Eggbox likelihood for tests
    
    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), DIMENSION(SIZE(par)) :: theta ! Variable to explore

    theta = par

    TEST_EGGBOX = - (2 + PRODUCT(COS(THETA/2d0)))**5
    

  END FUNCTION TEST_EGGBOX
 
  !#####################################################################################################################   

END MODULE MOD_LIKELIHOOD
