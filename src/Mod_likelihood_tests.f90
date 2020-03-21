MODULE MOD_LIKELIHOOD
  ! Automatic Time-stamp: <Last changed by martino on Saturday 21 March 2020 at CET 17:11:12>
  ! Module of likelihood test function, no real data are involved here



  !#####################################################################################################################

  ! IMPORTANT: to switch between likelihood types for test and others,
  ! change the name of the file to compile in the Makefile,
  ! Mod_likeihood.f90 or Mod_likeihood_tests.f90

  !#####################################################################################################################

  !#####################################################################################################################

  ! OTHER IMPORTANT NOTE: because of the intrinsic construction of nested samplig, change of range can change the result
  ! It is the case also in Polychord. In polychord you change the sigma, it is not changing the result. In Nested Fit,
  ! the search algorithm has to be optimized. FA CACARE!!!! NON FUNZIONA !!!!?????

  !#####################################################################################################################


  ! Module for the input parameter definition
  USE MOD_PARAMETERS, ONLY: npar, funcname

  IMPLICIT NONE
  INTEGER(8) :: ncall=0
  REAL(8) :: a_norm=0.

CONTAINS


  SUBROUTINE INIT_LIKELIHOOD()
    ! Initialize the normal likelihood with data files and special function

    WRITE(*,*) 'Initialization of test likelihood function'

    !IF (funcname.eq.'TEST_ROSENBROCK') THEN
    !   CALL INIT_ROSENBROCK()
    !END IF

  END SUBROUTINE INIT_LIKELIHOOD

  !#####################################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD(par)
    ! Main likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: par

    ncall = ncall + 1

    ! Select the test function
    IF (funcname.eq.'TEST_SIMPLE_GAUSS') THEN
       LOGLIKELIHOOD = TEST_SIMPLE_GAUSS(par)
    ELSE IF (funcname.eq.'TEST_GAUSS') THEN
       LOGLIKELIHOOD = TEST_GAUSS(par)
    ELSE IF (funcname.eq.'TEST_EGGBOX') THEN
       LOGLIKELIHOOD = TEST_EGGBOX(par)
    ELSE IF (funcname.eq.'TEST_ROSENBROCK') THEN
       LOGLIKELIHOOD = TEST_ROSENBROCK(par)
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

  
  !#####################################################################################################################
  REAL(8) FUNCTION TEST_SIMPLE_GAUSS(par)
    
    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), PARAMETER :: x0=0., sigma=1., amp=1.
    REAL(8) :: x

    x = par(1)
    
    IF(DABS(-(x-x0)**2/(2*sigma**2)).LT.700) THEN
       TEST_SIMPLE_GAUSS = amp/(dsqrt(2*pi)*sigma)*dexp(-(x-x0)**2/(2*sigma**2))
    ELSE
       TEST_SIMPLE_GAUSS = 0.d0
    END IF
    

  END FUNCTION TEST_SIMPLE_GAUSS
  
  REAL(8) FUNCTION TEST_GAUSS(par)
    !> Basic multidimensional Gaussian likelihood with mean mu(:) and an uncorrelated covariance sigma(:).
    !! Inspired from polychord code
    !! 
    !! It is normalised so that it should output an evidence of 1.0 for
    !! effectively infinite priors.
    !!
    !! The mean is set at 0.0 by default, and all sigmas at 0.01

    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), DIMENSION(SIZE(par)) :: sigma ! Standard deviation (uncorrelated) 
    REAL(8), DIMENSION(SIZE(par)) :: mu    ! Mean 
    REAL(8), DIMENSION(SIZE(par)) :: x     ! Variable to explore

    x = par

    ! Initialise the mean and standard deviation
    mu    = 5d-1  ! mean 
    sigma = 1d-2  ! all sigma set relatively small

    ! Gaussian normalisation
    TEST_GAUSS = - SUM( LOG( sigma ) + LOG(2*pi)/2d0 ) 

    ! theta dependence
    TEST_GAUSS = TEST_GAUSS - SUM( ( ( x - mu ) / sigma ) ** 2d0 ) / 2d0


  END FUNCTION TEST_GAUSS

  !#####################################################################################################################

  REAL(8) FUNCTION TEST_EGGBOX(par)
    !> Eggbox likelihood for tests

    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), DIMENSION(SIZE(par)) :: theta ! Variable to explore

    theta = par

    TEST_EGGBOX = - (2 + PRODUCT(COS(THETA/2d0)))**5


  END FUNCTION TEST_EGGBOX

  !#####################################################################################################################
  SUBROUTINE INIT_ROSENBROCK()
    ! Initialization of Rosenbrock function
    ! Normalization for N = 2 following  Handley MNRAS (2015)
    REAL(8), PARAMETER :: pi=3.141592653589793d0

    IF (npar.EQ.2) THEN
       a_norm =  -0.5d0 * DLOG( pi**npar / (2d0*100) )
    ELSE
       a_norm = 0.d0
    END IF

  END SUBROUTINE INIT_ROSENBROCK
  
  REAL(8) FUNCTION TEST_ROSENBROCK(par)
    ! Upside down Rosenbock function
    ! http://en.wikipedia.org/wiki/Rosenbrock_function
    ! 
    ! \log\mathcal{L}(x) = - \sum_{i=1}^{N-1}  (1 -x_i)^2 + 100(x_{i+1} -\x_i^2 )^2 
    ! it has exactly one minimum for N=2 and N=3 (at (1,1) and (1, 1, 1)) and exactly two minima for 4<= N<= 7.
    ! The global minimum of all ones and a local minimum near x_{N})= (x_{1},x_{2},\dots ,x_{N})=(-1,1,\dots ,1).
    ! For N = 2, it is normalized to 1.
    ! For N = 5, the integral is equal to -15.1091 following Handley MNRAS (2015)

    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), DIMENSION(SIZE(par)) :: x ! Variable to explore
    INTEGER(4) :: i=0

    x = par
    
    TEST_ROSENBROCK =  - SUM( (1-x(1:npar-1))**2 + 100d0*( x(2:npar) - x(1:npar-1)**2 )**2 )
    
  END FUNCTION TEST_ROSENBROCK

  !#####################################################################################################################   

END MODULE MOD_LIKELIHOOD
