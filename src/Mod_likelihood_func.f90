MODULE MOD_LIKELIHOOD
  ! Automatic Time-stamp: <Last changed by martino on Monday 03 May 2021 at CEST 13:23:14>
  ! Module of likelihood test function, no real data are involved here

  !#####################################################################################################################

  ! IMPORTANT NOTE: because of the intrinsic construction of nested samplig, change of range can change the result
  ! It is the case also in Polychord. In polychord you change the sigma, it is not changing the result. In Nested Fit,
  ! the search algorithm has to be optimized. FA CACARE!!!! NON FUNZIONA !!!!?????

  !#####################################################################################################################


  ! Module for the input parameter definition
  USE MOD_PARAMETERS !, ONLY: npar, funcname, funcid, searchid

#ifdef OPENMPI_ON
  USE MPI
#endif

  IMPLICIT NONE
  INTEGER(8) :: ncall=0
  REAL(8) :: a_norm=0.

CONTAINS


  SUBROUTINE INIT_LIKELIHOOD()
    ! Initialize the normal likelihood with data files and special function

    ! Initialize the search method params
    CALL INIT_SEARCH_METHOD()
    
    WRITE(*,*) 'Initialization of func likelihood'

    !IF (funcname.eq.'TEST_ROSENBROCK') THEN
    !   CALL INIT_ROSENBROCK()
    !END IF
    
    funcid = SELECT_LIKELIHOODFCN(funcname)

  END SUBROUTINE INIT_LIKELIHOOD

  SUBROUTINE INIT_SEARCH_METHOD()
#ifdef OPENMPI_ON
      INTEGER(4) :: mpi_ierror
#endif

      IF (search_method.eq.'RANDOM_WALK') THEN
            searchid = 0
      ELSE IF(search_method.EQ.'UNIFORM') THEN
            searchid = 1
      ELSE IF(search_method.EQ.'SLICE_SAMPLING') THEN
            searchid = 2
      ELSE IF(search_method.EQ.'SLICE_SAMPLING_ADAPT') THEN
            searchid = 3
      ELSE
            WRITE(*,*) 'Error of the search type name in Mod_search_new_point module'
            WRITE(*,*) 'Check the manual and the input file'
#ifdef OPENMPI_ON
            CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_ierror)
#endif
            STOP
      END IF
  END SUBROUTINE INIT_SEARCH_METHOD

  !#####################################################################################################################


  REAL(8) FUNCTION LOGLIKELIHOOD_WITH_TEST(par)
    ! Make some tests first if required

    REAL(8), DIMENSION(npar), INTENT(IN) :: par


    LOGLIKELIHOOD_WITH_TEST = LOGLIKELIHOOD(par)

  END FUNCTION LOGLIKELIHOOD_WITH_TEST

  !------------------------------------------------------------------------------------------------------------------------
  ! TODO(Cesar): In reality this should be in another file, but I am following the previous "rules"...
  FUNCTION SELECT_LIKELIHOODFCN(funcname)
    IMPLICIT NONE
    INTEGER*4 SELECT_LIKELIHOODFCN
    CHARACTER*64 funcname

    IF (funcname.eq.'TEST_SIMPLE_GAUSS') THEN
      SELECT_LIKELIHOODFCN = 0
    ELSE IF (funcname.eq.'TEST_GAUSS') THEN
      SELECT_LIKELIHOODFCN = 1
    ELSE IF (funcname.eq.'TEST_GAUSSIAN_SHELLS') THEN
      SELECT_LIKELIHOODFCN = 2
    ELSE IF (funcname.eq.'TEST_EGGBOX') THEN
      SELECT_LIKELIHOODFCN = 3
    ELSE IF (funcname.eq.'TEST_ROSENBROCK') THEN
      SELECT_LIKELIHOODFCN = 4
    ELSE IF (funcname.eq.'TEST_GAUSS_WITH_CORRELATION') THEN
      SELECT_LIKELIHOODFCN = 5
    ELSE IF(funcname.eq.'ENERGY_HARM_3D') THEN
      SELECT_LIKELIHOODFCN = 6
    ELSE IF(funcname.eq.'TEST_LOGGAMMA') THEN
      SELECT_LIKELIHOODFCN = 7
    ELSE
       WRITE(*,*) 'Error of the function name in Mod_likelihood_test module'
       WRITE(*,*) 'Check the manual and the input file'
       STOP
    END IF

    RETURN
  END

  REAL(8) FUNCTION LOGLIKELIHOOD(par)
    ! Main likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: par

    ncall = ncall + 1

    ! Select the test function
    SELECT CASE (funcid)
    CASE (0)
       LOGLIKELIHOOD = TEST_SIMPLE_GAUSS(par)
    CASE (1)
       LOGLIKELIHOOD = TEST_GAUSS(par)
    CASE (2)
       LOGLIKELIHOOD = TEST_GAUSSIAN_SHELLS(par)
    CASE (3)
       LOGLIKELIHOOD = TEST_EGGBOX(par)
    CASE (4)
       LOGLIKELIHOOD = TEST_ROSENBROCK(par)
    CASE (5)
       LOGLIKELIHOOD = TEST_GAUSS_WITH_CORRELATION(par)
    CASE (6)
       LOGLIKELIHOOD = ENERGY_HARM_3D(par)
    CASE (7)
       LOGLIKELIHOOD = TEST_LOGGAMMA(par)
    END SELECT


  END FUNCTION LOGLIKELIHOOD


  !#####################################################################################################################

  SUBROUTINE FINAL_LIKELIHOOD(live_max,par_mean,par_median_w)
    ! Final action for the likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w

    WRITE(*,*) ' '
    WRITE(*,*) 'End of likelihood test'
    WRITE(*,*) 'Number of calls : ', ncall
    OPEN(11,FILE='nf_output_n_likelihood_calls.txt',STATUS= 'UNKNOWN')
    WRITE(11,*) ncall
    CLOSE(11)

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

  !#####################################################################################################################

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

    ! x dependence
    TEST_GAUSS = TEST_GAUSS - SUM( ( ( x - mu ) / sigma ) ** 2d0 ) / 2d0


  END FUNCTION TEST_GAUSS

  !#####################################################################################################################

  REAL(8) FUNCTION TEST_GAUSSIAN_SHELLS(par)
    !> Basic multidimensional Gaussian shells likelihood with mean mu(:) and an uncorrelated covariance sigma(:).
    !! Inspired from polychord and multinest codes

    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8) :: ADDLOG, radius, sigma, A_norm, value_shell_1, value_shell_2
    REAL(8), DIMENSION(SIZE(par)) :: mu    ! Mean
    REAL(8), DIMENSION(SIZE(par)) :: x     ! Variable to explore

    x = par

    ! Initialise the mean and standard deviation
    mu    = 0.d0  ! mean (to be changed after for the first dimension)
    radius = 2d0  ! radius
    sigma = 1d-2  ! all sigma set relatively small

    ! Gaussian normalisation like Feroz 2009
    A_norm = - ( LOG( sigma ) + LOG(2*pi)/2d0 )

    ! First shell
    ! Parameters
    mu(1)  = -3.5
    ! x dependence first shell
    value_shell_1 = A_norm - ( (DSQRT( SUM( (mu-x)**2 ) ) - radius)**2 /(2d0*sigma**2) )

    ! Second shell
    ! Parameters
    mu(1)  = +3.5
    ! x dependence first shell
    value_shell_2 = A_norm - ( (DSQRT( SUM( (mu-x)**2 ) ) - radius)**2 /(2d0*sigma**2) )

    TEST_GAUSSIAN_SHELLS = ADDLOG(value_shell_1,value_shell_2)


  END FUNCTION TEST_GAUSSIAN_SHELLS

  !#####################################################################################################################

  REAL(8) FUNCTION TEST_EGGBOX(par)
    !> Eggbox likelihood for tests

    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), DIMENSION(SIZE(par)) :: theta ! Variable to explore

    theta = par

    TEST_EGGBOX = (2 + PRODUCT(COS(THETA/2d0)))**5


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

  REAL(8) FUNCTION TEST_GAUSS_WITH_CORRELATION(par)
    !> Multidimensional Gaussian likelihood with mean mu(:) and a correlated covariance Sigma(:). (Here 2D)
    !! Inspired by the TEST_GAUSS function (https://en.wikipedia.org/wiki/Multivariate_normal_distribution)
    !!
    !! It is normalised so that it should output an evidence of 1.0 for
    !! effectively infinite priors.
    !!
    !! The mean is set at 0.0 by default, the diagonal terms of the covariance matrix at 0.01, and the non diagonal terms at 0.009

    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8) :: sigma_d, sigma_c ! diagonal and non diagonal terms of the covariance (correlated)
    REAL(8), DIMENSION(SIZE(par)) :: mu    ! Mean
    REAL(8), DIMENSION(SIZE(par)) :: x     ! Variable to explore

    x = par

    ! Initialise the mean and standard deviation
    mu    = 0  ! mean
    sigma_d = 1d-2  ! all sigma set relatively small, diagonal elements
    sigma_c = 9d-3  ! non-diagonal elements

    ! Gaussian normalisation
    TEST_GAUSS_WITH_CORRELATION = - LOG( sigma_d**2-sigma_c**2 )/2.d0 - LOG(2*pi)

    ! x dependence
    TEST_GAUSS_WITH_CORRELATION = TEST_GAUSS_WITH_CORRELATION &
            - (sigma_d*(x(1)-mu(1))**2+sigma_d*(x(2)-mu(2))**2-2*sigma_c*(x(1)-mu(1))*(x(2)-mu(2)))/ (2.d0*(sigma_d**2-sigma_c**2))


  END FUNCTION TEST_GAUSS_WITH_CORRELATION
  !#####################################################################################################################


  REAL(8) FUNCTION ENERGY_HARM_3D(par)
    !> The parameters are the positions of the points (...,x_i,y_i,z_i,....)
    !> Potential of the form eps*SUM(x-i**2+y_i**2+z_i**2)

    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8), PARAMETER ::  eps=1.
    REAL(8), DIMENSION(SIZE(par)) :: x     
    INTEGER(4) :: N, i, j
    REAL(8) :: rij, ener

    x = par
    N=INT(SIZE(x)/3)
    
    ener=0.
    DO i=1,N
      rij=(x(i*3-2))**2+(x(i*3-1))**2+(x(i*3))**2
      ener=ener+eps*(rij)   
    END DO
    
    ENERGY_HARM_3D=-ener
  END FUNCTION ENERGY_HARM_3D

!#####################################################################################################################   
  
  REAL(8) FUNCTION TEST_LOGGAMMA(par)
    !> Basic multidimensional Gaussian likelihood with mean mu(:) and an uncorrelated covariance sigma(:).
    !! Inspired from multinest and ultranest code
    !!
    !! It is normalised so that it should output an evidence of 1.0 for
    !! effectively infinite priors.
    !!
    !! The mean is set at 0.0 by default, and all sigmas at 0.01

    REAL(8), DIMENSION(:), INTENT(IN) :: par
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    REAL(8) :: sigma ! Standard deviation (uncorrelated)
    REAL(8) :: mu1, mu2    ! Mean
    REAL(8) :: c    ! Shape
    REAL(8), DIMENSION(SIZE(par)) :: x     ! Variable to explore
    REAL(8) :: L1, L2
    INTEGER(8) :: half, i

    x = par

    ! Initialise the mean and standard deviation
    mu1    = 1d0/3.
    mu2    = 2d0/3.  
    sigma = 1d0/30.  ! all sigma set relatively small
    c=1.
    half=INT((SIZE(x)+2d0)/2d0)
    !WRITE(*,*) mu1, mu2, sigma, c, half,size(x)

    L1=0.5*((EXP(c*((x(1)-mu1)/sigma)-EXP((x(1)-mu1)/sigma))/(GAMMA(c)*sigma)) &
            +(EXP(c*((x(1)-mu2)/sigma)-EXP((x(1)-mu2)/sigma))/(GAMMA(c)*sigma)))
    L2=0.5*1/(SQRT(2*pi)*sigma)*(EXP(-((x(2)-mu1)/sigma)**2/2d0)+EXP(-((x(2)-mu2)/sigma)**2/2d0))
    TEST_LOGGAMMA=LOG(L1)+LOG(L2)
    
    DO i=3,half
       TEST_LOGGAMMA=TEST_LOGGAMMA+c*((x(i)-mu2)/sigma)-EXP((x(i)-mu2)/sigma)-LOG(GAMMA(c))-LOG(sigma)
    END DO
    
    DO i=half+1,SIZE(x)
        TEST_LOGGAMMA=TEST_LOGGAMMA-LOG(sigma)-LOG(2*pi)/2d0-(((x(i)-mu2)/sigma)**2d0/2d0)
    END DO

  END FUNCTION TEST_LOGGAMMA
  
!#####################################################################################################################   


END MODULE MOD_LIKELIHOOD
