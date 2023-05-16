MODULE MOD_LIKELIHOOD
  ! Automatic Time-stamp: <Last changed by martino on Monday 15 May 2023 at CEST 18:37:52>
  ! Module of the likelihood function for data analysis

  ! Module for the input parameter definition
  USE MOD_PARAMETERS

  IMPLICIT NONE

  ! Data variables
  INTEGER(4) :: ndata, ncall=0
  INTEGER(4), DIMENSION(nsetmax) :: ndata_set=0
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: x, nc, nc_err
  ! Data variable for 2D images
  INTEGER(4) :: nx=0, ny=0
  INTEGER(4), ALLOCATABLE, DIMENSION(:,:) :: adata
  INTEGER(1), ALLOCATABLE, DIMENSION(:,:) :: adata_mask
  ! Likelihood variables
  REAL(8) :: const_ll = 0.

CONTAINS

  SUBROUTINE INIT_LIKELIHOOD()
    ! Initialize the normal likelihood with data files and special function

    ! Initialize the search method params
    CALL INIT_SEARCH_METHOD()

    ! Read data ------------------------------------------------------------------------------------------------------------------------
    CALL READ_DATA()

    ! Initialize functions
    CALL INIT_FUNCTIONS()

  END SUBROUTINE INIT_LIKELIHOOD

  !#####################################################################################################################

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

  SUBROUTINE READ_DATA()
    ! Subroutine to read data files
    INTEGER(4) :: k=0
    REAL(8), DIMENSION(maxdata,nsetmax) :: x_tmp=0, nc_tmp=0, nc_err_tmp=0

    ! READ DATA, calculate the constants for the likelihood function
    ! Initialize
    ndata = 0
    ndata_set = 0
    const_ll = 0.

    IF (data_type.EQ.'1c') THEN
       ! Case 1D with counts ----------------------------------------------------------------
       DO k=1,nset
          CALL READ_FILE_COUNTS(filename(k),xmin(k),xmax(k),ndata_set(k), &
               x_tmp(:,k),nc_tmp(:,k))
          WRITE(*,*) 'Number of file = ', k, ' of ', nset
          WRITE(*,*) 'Read data file ', TRIM(filename(k))
          WRITE(*,*) 'ndata = ', ndata_set(k)
          WRITE(*,*) 'constant in evidence calc. = ', const_ll
       END DO
       ! Allocate set of data
       ndata = MAXVAL(ndata_set)
       ALLOCATE(x(ndata,nset),nc(ndata,nset))
       x = 0.
       nc = 0.
       DO k=1,nset
          x(1:ndata_set(k),k)  = x_tmp(1:ndata_set(k),k)
          nc(1:ndata_set(k),k) = nc_tmp(1:ndata_set(k),k)
       END DO
    ELSE IF (data_type.EQ.'1e') THEN
       ! Case 1D with errorbars -------------------------------------------------------------------
       DO k=1,nset
          CALL READ_FILE_ERRORBARS(filename(k),xmin(k),xmax(k),ndata_set(k), &
               x_tmp(:,k),nc_tmp(:,k),nc_err_tmp(:,k))
          WRITE(*,*) 'Number of file = ', k, ' of ', nset
          WRITE(*,*) 'Data file ', filename(k), ' read'
          WRITE(*,*) 'ndata = ', ndata
          WRITE(*,*) 'constant in evidence calc. = ', const_ll
       END DO
       ! Allocate set of data
       ndata = MAXVAL(ndata_set)
       ALLOCATE(x(ndata,nset),nc(ndata,nset),nc_err(ndata,nset))
       x = 0.
       nc = 0.
       nc_err = 0.
       DO k=1,nset
          x(1:ndata_set(k),k)  = x_tmp(1:ndata_set(k),k)
          nc(1:ndata_set(k),k) = nc_tmp(1:ndata_set(k),k)
          nc_err(1:ndata_set(k),k) = nc_err_tmp(1:ndata_set(k),k)
       END DO
    ELSE IF (data_type.EQ.'2c') THEN
       ! Case 2D with counts in a matrix -------------------------------------------------------------------
       DO k=1,nset
          CALL READ_FILE_COUNTS_2D(filename(k),xmin(k),xmax(k),ymin(k),ymax(k),ndata_set(k))
          WRITE(*,*) 'Number of file = ', k, ' of ', nset
          WRITE(*,*) 'Read data file ', TRIM(filename(k))
          WRITE(*,*) 'ndata = ', ndata_set(k)
          WRITE(*,*) 'constant in evidence calc. = ', const_ll

          IF(ndata.EQ.0) WRITE(*,*) 'ndata = 0, no data in the selected range. Please check your min max and data'
       END DO
    ELSE
       WRITE(*,*) 'Data type ', data_type, ' not available. Please change your input file'
       STOP
    END IF

    WRITE(*,*) 'Final constant in evidence calc. = ', const_ll

  END SUBROUTINE READ_DATA

  !#####################################################################################################################

  SUBROUTINE READ_FILE_COUNTS(namefile,minx,maxx,datan,x_tmp,nc_tmp)
    ! Read one file of data

    USE, INTRINSIC :: IEEE_ARITHMETIC

    CHARACTER, INTENT(IN) :: namefile*64
    REAL(8), INTENT(IN) :: minx, maxx
    INTEGER(4), INTENT(OUT) :: datan
    REAL(8), DIMENSION(maxdata), INTENT(OUT) :: x_tmp, nc_tmp
    !
    INTEGER(4) :: i=0, nd=0
    REAL(8), DIMENSION(maxdata) :: x_raw=0, nc_raw=0
    REAL(8) :: DLOG_FAC

    ! Initialize
    x_tmp = 0.
    nc_tmp = 0.
    datan = 0
    nd=0

    ! Open file and read
    OPEN(10,file=namefile,status='old')
    DO i=1, maxdata
       READ(10,*,END=20) x_raw(i), nc_raw(i)
       ! Make test for integer numbers, NaN and infinites
       IF (ABS(nc_raw(i)-INT(nc_raw(i))).GT.1E-5) THEN
          WRITE(*,*) 'Attention, input numbers are not counts and you are using Poisson statistic (no error bar)'
          WRITE(*,*) 'n. counts = ', nc_raw(i)
          WRITE(*,*) 'Change something!'
          STOP
       ELSE IF (nc_raw(i).LT.0.AND.IEEE_IS_FINITE(nc_raw(i))) THEN
          ! Check if counts are negative
          WRITE(*,*) 'Negative counts are not accepted. Change input file'
          STOP
       ELSE IF (.NOT.IEEE_IS_FINITE(nc_raw(i)).OR.IEEE_IS_NAN(nc_raw(i))) THEN
          ! Check infinites and nan
          WRITE(*,*) 'Infinite or "NaN" counts are not accepted. Change input file'
          STOP
       END IF

       ! Select the data
       IF(x_raw(i).GE.minx.AND.x_raw(i).LE.maxx) THEN
          nd = nd + 1
          x_tmp(nd) = x_raw(i)
          nc_tmp(nd) = nc_raw(i)
          ! Calculation of the constant part of the likelihood with Poisson distribution
          !IF (nc_tmp(nd).GT.0.) const_ll = const_ll - DFAC_LN(INT(nc_tmp(nd)))
          ! Uses of the gamma function gamma(n) = (n-1)!
          IF (nc_tmp(nd).GT.0.) const_ll = const_ll - DLOG_FAC(INT(nc_tmp(nd)))
       END IF
    ENDDO

20  CONTINUE
    CLOSE(10)
    datan = nd

  END SUBROUTINE READ_FILE_COUNTS

  !#####################################################################################################################

  SUBROUTINE READ_FILE_ERRORBARS(namefile,minx,maxx,datan,x_tmp,nc_tmp,nc_err_tmp)
    ! Read one file of data
    CHARACTER, INTENT(IN) :: namefile*64
    REAL(8), INTENT(IN) :: minx, maxx
    INTEGER(4), INTENT(OUT) :: datan
    REAL(8), DIMENSION(maxdata), INTENT(OUT) :: x_tmp, nc_tmp, nc_err_tmp
    !
    INTEGER(4) :: i=0, nd = 0
    REAL(8), DIMENSION(maxdata) :: x_raw=0, nc_raw=0, nc_err_raw=0
    REAL(8), PARAMETER :: pi=3.141592653589793d0


    ! Initialize
    x_tmp = 0.
    nc_tmp = 0.
    nc_err_tmp = 0.
    nd = 0

    ! Open file and read
    OPEN(10,file=namefile,status='old')
    DO i=1, maxdata
       READ(10,*,END=20) x_raw(i), nc_raw(i), nc_err_raw(i)
       ! Select the data
       IF(x_raw(i).GE.minx.AND.x_raw(i).LE.maxx) THEN
          nd = nd + 1
          x_tmp(nd) = x_raw(i)
          nc_tmp(nd) = nc_raw(i)
          nc_err_tmp(nd) = nc_err_raw(i)
          IF (nc_err_tmp(nd).LE.0.) THEN
             WRITE(*,*) 'Errorbars with a value equal to 0 or negative'
             WRITE(*,*) 'Check our data or do not use errorbars'
             STOP
          END IF
          ! Calculation of the constant part of the likelihood with Gaussian distribution
          const_ll = -DLOG(nc_err_raw(i))
       END IF
    ENDDO

20  CONTINUE
    CLOSE(10)
    ! Calculation of the constant part of the likelihood with Gaussian distribution
    const_ll = const_ll - nd*DLOG(2*pi)/2
    datan = nd

  END SUBROUTINE READ_FILE_ERRORBARS

    !#####################################################################################################################

  SUBROUTINE READ_FILE_COUNTS_2D(namefile,minx,maxx,miny,maxy,datan)
    ! Read one file of data

    USE, INTRINSIC :: IEEE_ARITHMETIC

    CHARACTER, INTENT(IN) :: namefile*64
    REAL(8), INTENT(IN) :: minx, maxx, miny, maxy
    INTEGER(4), INTENT(OUT) :: datan
    !
    REAL(8) :: DLOG_FAC
    INTEGER(4) :: i, j, sx, sy, imin, imax, jmin, jmax
    REAL(8), ALLOCATABLE, DIMENSION(:,:) :: adata_tmp
    CHARACTER :: string*128

    ! Initialize
    datan = 0

    ! Open file and read headers and the 2D matrix data in one shot
    OPEN(10,file=namefile,status='old')
    READ(10,*) string
    READ(10,*) sx, sy
    ALLOCATE(adata_tmp(sy,sx))
    READ(10,*) adata_tmp
    CLOSE(10)

    ! Check if counts are negative
    IF (ANY(adata_tmp.LT.0.AND.IEEE_IS_FINITE(adata_tmp))) THEN
       WRITE(*,*) 'Negative counts are not accepted. Change input file'
       STOP
    END IF

    ! Substitute infinites and nan with negative counts
    WHERE (.NOT.IEEE_IS_FINITE(adata_tmp))
       adata_tmp = -1
    END WHERE
    WHERE (IEEE_IS_NAN(adata_tmp))
       adata_tmp = -1
    END WHERE

    !write(*,*) adata_tmp(:,1)
    !write(*,*) adata_tmp(:,2)
    !write(*,*) adata_tmp(:,3)
    !pause

    imin = INT(minx+1)
    imax = INT(maxx)
    jmin = INT(miny+1)
    jmax = INT(maxy)
    nx = imax - imin + 1
    ny = jmax - jmin + 1

    !write(*,*) imin, imax, jmin, jmax


    ALLOCATE(adata(nx,ny),adata_mask(nx,ny))
    adata = INT(TRANSPOSE(adata_tmp(jmin:jmax,imin:imax)))

    DEALLOCATE(adata_tmp)


    !write(*,*) adata(1,:)
    !write(*,*) adata(2,:)
    !write(*,*) adata(3,:)
    !pause

    ! Count the available data
    !datan = COUNT(ieee_is_finite(adata))

    ! Calculation of the constant part of the likelihood with Poisson distribution
    adata_mask = 0
    DO i=1,nx
       DO j=1,ny
          IF (adata(i,j).GT.0) THEN
             adata_mask(i,j) = 1
             datan = datan + 1
             const_ll = const_ll - DLOG_FAC(adata(i,j))
          ELSE IF(adata(i,j).EQ.0) THEN
             adata_mask(i,j) = 1
             datan = datan + 1
          END IF
          ! And the rest are bad pixels
       END DO
    END DO

  END SUBROUTINE READ_FILE_COUNTS_2D

  !#####################################################################################################################

#define DATA_IS_C   B'00000001'
#define DATA_IS_E   B'00000010'
#define DATA_IS_1D  B'00010000'
#define DATA_IS_2D  B'00100000'
#define DATA_IS_SET B'10000000'
#define BIT_CHECK_IF(what) (IAND(dataid, what).GT.0)
  
  SUBROUTINE INIT_FUNCTIONS()
    ! Subroutine to initialize the user functions, functions id and data ids

    INTEGER(4) :: SELECT_USERFCN, SELECT_USERFCN_SET, SELECT_USERFCN_2D

    ! Init the dataid for data types !
    ! ------------------------------ !
    ! This integer works like this:  !
    !  yn ud 2  1     ud ud e  c     !
    !  b7 b6 b5 b4    b3 b2 b1 b0    !
    !  ~~ ~~~~~~~~    ~~~~~~~~~~~    !
    !  ^      ^            ^         !
    ! set  Data dim    Data type     !
    ! ------------------------------ !
    ! ud = undefined (space for a 3d analysis [b6] and other distributions [b2, b3])
    ! yn = 0/1 -> n/y
    ! TODO(Cesar): The e/c 1/2 could live in the same bit, making comparisons even faster
    ! TODO(Cesar): But there wouldn't be much space for working with other values in the future
    
    ! Is the data 1 or 2-D ?
    IF(data_type(1:1).EQ.'1') THEN
       dataid = IOR(dataid, DATA_IS_1D)
    ELSE IF(data_type(1:1).EQ.'2') THEN
       dataid = IOR(dataid, DATA_IS_2D)
    END IF

    ! Should we use a poisson distribution or do we have the error bars ?
    IF(data_type(2:2).EQ.'c') THEN
       dataid = IOR(dataid, DATA_IS_C)
    ELSE IF(data_type(2:2).EQ.'e') THEN
       dataid = IOR(dataid, DATA_IS_E)
    END IF

    ! Is this a set ?
    IF(set_yn.EQ.'y'.OR.set_yn.EQ.'Y') THEN
       dataid = IOR(dataid, DATA_IS_SET)
    END IF

    ! Init the funcid for function names
    IF(.NOT.BIT_CHECK_IF(DATA_IS_SET)) THEN
       IF(BIT_CHECK_IF(DATA_IS_1D)) THEN
          funcid = SELECT_USERFCN(funcname)
       ELSE
          funcid = SELECT_USERFCN_2D(funcname)
       END IF
    ELSE
       IF(BIT_CHECK_IF(DATA_IS_2D)) THEN
          WRITE(*,*) 'Sets for 2D functions are not yet implemented.'
          STOP
       END IF
       funcid = SELECT_USERFCN_SET(funcname)
    END IF

    ! Initialise functions if needed
    IF (.NOT.BIT_CHECK_IF(DATA_IS_SET)) THEN
       IF(funcname.EQ.'ROCKING_CURVE') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_ROCKING(par_in(6),par_in(7))
       ELSE IF(funcname.EQ.'TWO_INTERP_VOIGT_POLY'&
            .OR.funcname.EQ.'TWO_INTERP_VOIGT_POLY_X0') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_TWO_INTERP(par_in(14),par_in(15))
       ELSE IF(funcname.EQ.'THREE_INTERP_VOIGT_POLY') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_THREE_INTERP(par_in(12),par_in(13),par_in(14))
       ELSE IF(funcname.EQ.'SIX_GAUSS_SHIRLEYBG'&
            .OR.funcname.EQ.'SIX_VOIGT_SHIRLEYBG'&
            .OR.funcname.EQ.'SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES'&
            .OR.funcname.EQ.'SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES') THEN
          CALL INIT_SHIRLEY(ndata,x(:,1),nc(:,1))
       END IF
    ELSE
       IF(funcname.EQ.'ROCKING_CURVE_SET') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_ROCKING_SET(par_in(9),par_in(10),par_in(11),par_in(12))
       END IF
    END IF

  END SUBROUTINE INIT_FUNCTIONS

!#####################################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD(par)
    ! Main likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: par

    ncall=ncall+1
    IF (BIT_CHECK_IF(DATA_IS_1D)) THEN
       LOGLIKELIHOOD = LOGLIKELIHOOD_1D(par)
    ELSE IF (BIT_CHECK_IF(DATA_IS_2D)) THEN
       LOGLIKELIHOOD = LOGLIKELIHOOD_2D(par)
    END IF

  END FUNCTION LOGLIKELIHOOD

  !#####################################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD_WITH_TEST(par)
    ! Main likelihood function with a preliminary test for Poisson
    ! This allows for avoid this test in the main loop calculation to speed the parallel computation

    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    !
    REAL(8) :: enc
    INTEGER(4) :: i, j, k
    REAL(8) :: USERFCN, USERFCN_SET, USERFCN_2D, xx, yy

    ncall=ncall+1
    IF (BIT_CHECK_IF(DATA_IS_C).AND.BIT_CHECK_IF(DATA_IS_1D)) THEN
       ! Check if the choosen function assumes zero or negative values
       DO k=1,nset
          DO i=1, ndata_set(k)
             ! Poisson distribution calculation --------------------------------------------------
             IF (.NOT.BIT_CHECK_IF(DATA_IS_SET)) THEN
                enc = USERFCN(x(i,k),npar,par,funcid)
             ELSE
                enc = USERFCN_SET(x(i,k),npar,par,funcid,k)
             END IF
             IF (enc.LE.0) THEN
                WRITE(*,*) 'LIKELIHOOD ERROR: put a background in your function'
                WRITE(*,*) 'number of counts different from 0, model prediction equal 0 or less'
                WRITE(*,*) 'Function value = ', enc, ' n. counts = ', nc(i,k)
                STOP ! TODO(Cesar): Handle all of these errors for the case of MPI
             END IF
          END DO
       END DO
       LOGLIKELIHOOD_WITH_TEST = LOGLIKELIHOOD_1D(par)
    ELSE IF (BIT_CHECK_IF(DATA_IS_E).AND.BIT_CHECK_IF(DATA_IS_1D)) THEN
       LOGLIKELIHOOD_WITH_TEST = LOGLIKELIHOOD_1D(par)
    ELSE IF (BIT_CHECK_IF(DATA_IS_C).AND.BIT_CHECK_IF(DATA_IS_2D)) THEN
       ! Check if the choosen function assumes zero or negative values
       DO i=1, nx
          DO j=1, ny
             xx = i - 0.5 + xmin(k) ! Real coordinates are given by the bins, the center of the bin.
             yy = j - 0.5 + ymin(k) ! additional -1 to take well into account xmin,ymin
             enc = USERFCN_2D(xx,yy,npar,par,funcid)
             IF (enc.LE.0) THEN
                WRITE(*,*) 'LIKELIHOOD ERROR: put a background in your function'
                WRITE(*,*) 'number of counts different from 0, model prediction equal 0 or less'
                WRITE(*,*) 'Function value = ', enc, ' n. counts = ', adata(i,j)
                STOP
             END IF
          END DO
       END DO
       LOGLIKELIHOOD_WITH_TEST = LOGLIKELIHOOD_2D(par)
    ELSE IF (BIT_CHECK_IF(DATA_IS_E).AND.BIT_CHECK_IF(DATA_IS_2D)) THEN
       LOGLIKELIHOOD_WITH_TEST = LOGLIKELIHOOD_2D(par)

    END IF

  END FUNCTION LOGLIKELIHOOD_WITH_TEST

  !#####################################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD_1D(par)

    ! Main likelihood function
    ! Type: Poisson , Gaussian , .... soon 2D I hope

    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    !
    REAL(8) :: USERFCN, USERFCN_SET
    REAL(8) :: ll_tmp
    REAL(8), DIMENSION(ndata, nset) :: enc
    INTEGER(4) :: i, k

    ! Calculate LIKELIHOOD
    ll_tmp = 0.
    enc = 0.

    IF (.NOT.BIT_CHECK_IF(DATA_IS_SET)) THEN
       ! No set --------------------------------------------------------------------------------------------------------
       !TODO(César): This is unnecessary and somewhat verbose
       k=1
       IF (BIT_CHECK_IF(DATA_IS_C)) THEN
          ! Poisson distribution calculation --------------------------------------------------
          ! Calculate the function (not SIMD optimisable)
          DO i=1, ndata_set(k)
             enc(i, k) = USERFCN(x(i, k),npar,par,funcid)
          END DO
          ! Calculate the likelihood
          !$OMP SIMD REDUCTION(+:ll_tmp)
          DO i=1, ndata_set(k)
             ll_tmp = ll_tmp + nc(i, k)*DLOG(enc(i,k)) - enc(i,k)
          END DO
          !$OMP END SIMD
       ELSE !IF (BIT_CHECK_IF(DATA_IS_E)) THEN
          ! Normal (Gaussian) distribution calculation --------------------------------------
          ! Calculate the function (not SIMD optimisable)
          DO i=1, ndata_set(k)
             enc(i,k) = USERFCN(x(i,k),npar,par,funcid)
          END DO
          ! Calculate the likelihood
          !$OMP SIMD REDUCTION(+:ll_tmp)
          DO i=1, ndata_set(k)
             ll_tmp = ll_tmp - (nc(i,k) - enc(i,k))**2/(2*nc_err(i,k)**2)
          END DO
          !$OMP END SIMD
       END IF
    ELSE
       ! Set ----------------------------------------------------------------------------------------------------------
       DO k=1,nset
          IF (BIT_CHECK_IF(DATA_IS_C)) THEN
             ! Poisson distribution calculation --------------------------------------------------
             ! Calculate the function (not SIMD optimisable)
             DO i=1, ndata_set(k)
                enc(i,k) = USERFCN_SET(x(i,k),npar,par,funcid,k)
             END DO
             ! Calculate the likelihood
             !$OMP SIMD REDUCTION(+:ll_tmp)
             DO i=1, ndata_set(k)
                ll_tmp = ll_tmp + nc(i,k)*DLOG(enc(i,k)) - enc(i,k)
             END DO
             !$OMP END SIMD
          ELSE
             ! Normal (Gaussian) distribution calculation --------------------------------------
             ! Calculate the function (not SIMD optimisable)
             DO i=1, ndata_set(k)
                enc(i,k) = USERFCN_SET(x(i,k),npar,par,funcid,k)
             END DO
             ! Calculate the likelihood
             !$OMP SIMD REDUCTION(+:ll_tmp)
             DO i = 1, ndata_set(k)
                ll_tmp = ll_tmp - (nc(i, k) - enc(i, k))**2/(2*nc_err(i, k)**2)
             END DO
             !$OMP END SIMD
          END IF
       END DO
    END IF
    !
    ! Sum all together
    LOGLIKELIHOOD_1D = ll_tmp + const_ll

  END FUNCTION LOGLIKELIHOOD_1D

  !###################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD_2D(par)

    ! Main likelihood function for 2D data
    ! Type: Poisson only for the moment

    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    !
    REAL(8) :: USERFCN_2D
    REAL(8) :: ll_tmp
    REAL(8), DIMENSION(nx, ny) :: enc
    INTEGER(4) :: i, j, k

    ! Calculate LIKELIHOOD
    ll_tmp = 0.
    enc = 0.
    k = 1

    DO j = 1, ny ! inversion of i,j for increasing memory administration efficency (but not differences noticed for te moment)
       DO i = 1, nx
          ! Calculation of the function values  --------------------------------------------------
          !xx = i - 0.5 + xmin(k) ! Real coordinates are given by the bins, the center of the bin.
          !yy = j - 0.5 + ymin(k) ! additional -1 to take well into account xmin,ymin
          enc(i, j) = USERFCN_2D(i - 0.5 + xmin(k), j - 0.5 + ymin(k),npar,par,funcid)
       END DO
    END DO
    !$OMP SIMD COLLAPSE(2) REDUCTION(+:ll_tmp)
    DO j = 1, ny ! inversion of i,j for increasing memory administration efficency (but not differences noticed for te moment)
       DO i = 1, nx
          ll_tmp = ll_tmp + adata_mask(i,j)*(adata(i,j)*DLOG(enc(i,j)) - enc(i,j))
       END DO
    END DO
    !$OMP END SIMD

    ! Sum all together
    LOGLIKELIHOOD_2D = ll_tmp + const_ll

  END FUNCTION LOGLIKELIHOOD_2D

  !#####################################################################################################################

  SUBROUTINE FINAL_LIKELIHOOD(live_max,par_mean,par_median_w)
    ! Final action for the likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w

    ! Write auxiliar files for plots
    IF (data_type(1:1).EQ.'1') THEN
       CALL WRITE_EXPECTED_VALUES(live_max,par_mean,par_median_w)
    ELSE IF (data_type(1:1).EQ.'2') THEN
       CALL WRITE_EXPECTED_VALUES_2D(live_max,par_mean,par_median_w)
    END IF

    WRITE(*,*) ' '
    WRITE(*,*) 'End of likelihood test'
    WRITE(*,*) 'Number of calls : ', ncall
    OPEN(11,FILE='nf_output_n_likelihood_calls.txt',STATUS= 'UNKNOWN')
    WRITE(11,*) ncall
    CLOSE(11)
    
    ! Deallocate variables
    CALL DEALLOCATE_DATA()

  END SUBROUTINE FINAL_LIKELIHOOD

  !#####################################################################################################################
  SUBROUTINE WRITE_EXPECTED_VALUES(live_max,par_mean,par_median_w)
    ! Write all auxiliar files for plots and co.

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w
    !
    INTEGER(8), PARAMETER :: maxfit = 10000
    REAL(8) :: USERFCN, USERFCN_SET
    REAL(8) :: minx=0., maxx=0., xfit=0., dx=0., enc = 0.
    INTEGER(4) :: i=0, k=0
    ! Stuff to save separate components
    LOGICAL :: plot = .FALSE.
    REAL(8) :: yfit
    CHARACTER :: out_filename*64

    COMMON /func_plot/ plot

    !-----------------------Calculate the expected function values -------------------------

    ! Calculate the expected function value and residual for max likelihood
    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       k=1
       enc = 0.
       ! max likelihood values -------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_max.dat', STATUS='unknown')
       WRITE(20,*)'# x    y data    y theory      y diff    y err'
       DO i=1, ndata
          enc = USERFCN(x(i,k),npar,live_max,funcid)
          IF (data_type.EQ.'1e') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', nc_err(i,k)
          ELSE IF (data_type.EQ.'1c') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', SQRT(nc(i,k))
          END IF
       END DO
       CLOSE(20)
       ! mean values ------------------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_mean.dat', STATUS='unknown')
       WRITE(20,*)'# x    y data    y theory      y diff    y err'
       DO i=1, ndata
          enc = USERFCN(x(i,k),npar,par_mean,funcid)
          IF (data_type.EQ.'1e') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', nc_err(i,k)
          ELSE IF (data_type.EQ.'1c') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', SQRT(nc(i,k))
          END IF
       END DO
       CLOSE(20)
       ! median values ----------------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_median.dat', STATUS='unknown')
       WRITE(20,*)'# x    y data    y theory      y diff    y err'
       DO i=1, ndata
          enc = USERFCN(x(i,k),npar,par_median_w,funcid)
          IF (data_type.EQ.'1e') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', nc_err(i,k)
          ELSE IF (data_type.EQ.'1c') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', SQRT(nc(i,k))
          END IF
       END DO
       CLOSE(20)
       ! -------------------------------------------------------------------------------------
    ELSE
       DO k=1, nset
          enc = 0.
          ! max likelihood values ------------------------------------------------------------
          WRITE(out_filename,1000) 'nf_output_data_max_',k,'.dat'
          OPEN (UNIT=30, FILE=out_filename, STATUS='unknown')
          WRITE(30,*)'# x    y data    y theory      y diff    y err'
          DO i=1, ndata_set(k)
             enc = USERFCN_SET(x(i,k),npar,live_max,funcid,k)
             IF (data_type.EQ.'1e') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ',  nc_err(i,k)
             ELSE IF (data_type.EQ.'1c') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', SQRT(nc(i,k))
             END IF
          END DO
          CLOSE(30)
          ! mean values ----------------------------------------------------------------------
          WRITE(out_filename,2000) 'nf_output_data_mean_',k,'.dat'
          OPEN (UNIT=30, FILE=out_filename, STATUS='unknown')
          WRITE(30,*)'# x    y data    y theory      y diff    y err'
          DO i=1, ndata_set(k)
             enc = USERFCN_SET(x(i,k),npar,live_max,funcid,k)
             IF (data_type.EQ.'1e') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ',  nc_err(i,k)
             ELSE IF (data_type.EQ.'1c') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', SQRT(nc(i,k))
             END IF
          END DO
          CLOSE(30)
          ! median values ---------------------------------------------------------------------
          WRITE(out_filename,3000) 'nf_output_data_median_',k,'.dat'
          OPEN (UNIT=30, FILE=out_filename, STATUS='unknown')
          WRITE(30,*)'# x    y data    y theory      y diff    y err'
          DO i=1, ndata_set(k)
             enc = USERFCN_SET(x(i,k),npar,live_max,funcid,k)
             IF (data_type.EQ.'1e') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ',  nc_err(i,k)
             ELSE IF (data_type.EQ.'1c') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', SQRT(nc(i,k))
             END IF
          END DO
          CLOSE(30)
          ! ----------------------------------------------------------------------------------
       END DO
    END IF

    ! Save in a file the different fit components !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! New from version 1.0: save three pairs of files with values from max likelihood, mean and median of the parameters
    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       maxx = xmax(1)
       minx = xmin(1)
       plot = .TRUE.
       xfit = 0.
       dx=(maxx-minx)/(maxfit-1)

       ! max likelihood values
       OPEN (UNIT=40, FILE='nf_output_fit_max.dat', STATUS='unknown')
       WRITE(40,*)'# x    y fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          yfit = USERFCN(xfit,npar,live_max,funcid)
       ENDDO
       CLOSE(40)

       ! mean values
       OPEN (UNIT=40, FILE='nf_output_fit_mean.dat', STATUS='unknown')
       WRITE(40,*)'# x    y fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          yfit = USERFCN(xfit,npar,par_mean,funcid)
       ENDDO
       CLOSE(40)

       ! mean values
       OPEN (UNIT=40, FILE='nf_output_fit_median.dat', STATUS='unknown')
       WRITE(40,*)'# x    y fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          yfit = USERFCN(xfit,npar,par_median_w,funcid)
       ENDDO
       CLOSE(40)
    ELSE
       DO k=1, nset
          maxx = xmax(k)
          minx = xmin(k)
          plot = .TRUE.
          xfit = 0.
          dx=(maxx-minx)/(maxfit-1)

          ! max likelihood values
          WRITE(out_filename,1001) 'nf_output_fit_max_',k,'.dat'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          WRITE(40,*)'# x    y fit'
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,live_max,funcid,k)
          ENDDO
          CLOSE(40)

          WRITE(out_filename,2001) 'nf_output_fit_mean_',k,'.dat'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          WRITE(40,*)'# x    y fit'
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,par_mean,funcid,k)
          ENDDO
          CLOSE(40)

          WRITE(out_filename,3001) 'nf_output_fit_median_',k,'.dat'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          WRITE(40,*)'# x    y fit'
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,par_median_w,funcid,k)
          ENDDO
          CLOSE(40)
       END DO
    END IF


1000 FORMAT (A19,I1,A4)
1001 FORMAT (A18,I1,A4)
2000 FORMAT (A20,I1,A4)
2001 FORMAT (A19,I1,A4)
3000 FORMAT (A22,I1,A4)
3001 FORMAT (A21,I1,A4)

  END SUBROUTINE WRITE_EXPECTED_VALUES

    !#####################################################################################################################
  SUBROUTINE WRITE_EXPECTED_VALUES_2D(live_max,par_mean,par_median_w)
    ! Write all auxiliar files for plots and co. in 2D

    USE, INTRINSIC :: IEEE_ARITHMETIC

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w
    !
    INTEGER(8), PARAMETER :: maxfit = 10000
    REAL(8) :: USERFCN_2D
    REAL(8) :: minx=0., maxx=0., dx, xfit, yfit
    INTEGER(4) :: i=0, j=0, k=0, ix=0
    ! Stuff to save separate components
    LOGICAL :: plot = .FALSE.
    REAL(8), DIMENSION(nx,ny) :: aenc, ares
    REAL(8), DIMENSION(nx) :: hx, hnc, henc, hres
    REAL(8) :: xx, yy, nan, a, b, c, y0, Dy

    COMMON /func_plot/ plot

    nan = IEEE_VALUE(nan, IEEE_QUIET_NAN)
    aenc = 0.
    ares = 0.

    ! If "LINE" profile, prepare for projection
    IF (INDEX(funcname,"_LINE").NE.0) THEN
       hx   = 0.
       hnc  = 0.
       henc = 0.
       hres = 0.
       a  = live_max(1)
       b  = live_max(2)
       c  = live_max(3)
       y0 = live_max(4)
       Dy = live_max(5)
    END IF

    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       k=1
       maxx = xmax(1)
       minx = xmin(1)
       plot = .TRUE.
       xfit = 0.
       dx=(maxx-minx)/(maxfit-1)
       IF (data_type.EQ.'2c') THEN
          ! Rewrite data and write function and residuals and corresponding projection if "LINE" profile ---
          OPEN (UNIT=20, FILE='nf_output_data_2D.dat', STATUS='unknown')
          WRITE(20,*)'# matrix dimensions: ', nx, ny
          DO i=1,nx
             WRITE(20,*) adata(i,:)
             xx = i - 0.5 + xmin(k) ! Real coordinates are given by the bins, the center of the bin.
             ! If "LINE" profile, built the projection
             IF (INDEX(funcname,"_LINE").NE.0) THEN
                hx(i) = xx
             END IF
             DO j=1, ny
                IF (adata(i,j).GE.0) THEN
                   ! Poisson distribution calculation --------------------------------------------------
                   yy = j - 0.5 + ymin(k) ! additional -1 to take well into account xmin,ymin
                   aenc(i,j) = USERFCN_2D(xx,yy,npar,live_max,funcid)
                   ares(i,j) = adata(i,j) - aenc(i,j)
                   IF (INDEX(funcname,"_LINE").NE.0) THEN
                      ix = CEILING(xx - b*(yy-y0) - c*(yy-y0)**2 - xmin(k))
                      IF (ix.GE.1.AND.ix.LE.nx) THEN
                         hnc(ix)  = hnc(ix) + adata(i,j)
                         henc(ix) = henc(ix) + aenc(i,j)
                         hres(ix) = hres(ix) + ares(i,j)
                         !write(*,*) ix, xx, yy, y0 !hx(i), hnc(ix), henc(ix), hres(ix)
                         !pause
                      END IF
                   END IF
                ELSE
                   aenc(i,j) = nan
                   ares(i,j) = nan
                END IF
             END DO
          END DO
          CLOSE(20)

          ! Write projection
          IF (INDEX(funcname,"_LINE").NE.0) THEN
             ! max likelihood values -------------------------------------------------------------
             OPEN (UNIT=20, FILE='nf_output_data_max.dat', STATUS='unknown')
             WRITE(20,*)'# x    y data    y theory      y diff    y err'
             DO i=1, nx
                WRITE(20,*) hx(i), ' ',hnc(i), ' ',henc(i), ' ',hres(i), ' ', SQRT(hnc(i))
             END DO
             CLOSE(20)
             ! Fit result with high resolution
             OPEN (UNIT=40, FILE='nf_output_fit_max.dat', STATUS='unknown')
             WRITE(40,*)'# x    y fit'
             DO i=1, maxfit
                xfit = minx + (i-1)*dx
                yfit = Dy*USERFCN_2D(xfit,y0,npar,live_max,funcid)
                WRITE(40,*) xfit, yfit
             ENDDO
             CLOSE(40)
          END IF

          OPEN (UNIT=20, FILE='nf_output_fit_max_2D.dat', STATUS='unknown')
          OPEN (UNIT=30, FILE='nf_output_fitres_max_2D.dat', STATUS='unknown')
          WRITE(20,*)'# matrix dimensions: ', nx, ny
          WRITE(30,*)'# matrix dimensions: ', nx, ny
          DO i=1,nx
             WRITE(20,*) aenc(i,:)
             WRITE(30,*) ares(i,:)
          END DO
          CLOSE(20)
          CLOSE(30)

       END IF
    END IF

  END SUBROUTINE WRITE_EXPECTED_VALUES_2D

  !#####################################################################################################################

  SUBROUTINE DEALLOCATE_DATA()

    IF (data_type.EQ.'1c') THEN
       DEALLOCATE(x,nc)
    ELSE IF (data_type.EQ.'1e') THEN
       DEALLOCATE(x,nc,nc_err)
    ELSE IF (data_type.EQ.'2c') THEN
       DEALLOCATE(adata)
    END IF

  END SUBROUTINE DEALLOCATE_DATA

  !#####################################################################################################################

END MODULE MOD_LIKELIHOOD
