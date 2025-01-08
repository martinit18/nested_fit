MODULE MOD_LIKELIHOOD
  ! Automatic Time-stamp: <Last changed by martino on Wednesday 11 October 2023 at CEST 09:39:50>
  ! Module of the likelihood function for data analysis

  ! Module for the input parameter definition
  USE MOD_PARAMETERS

  ! Module for the user functions
  USE MOD_USERFCN

  ! Module for logging
  USE MOD_LOGGER

  ! Math module
  USE MOD_MATH

  ! Options
  USE MOD_OPTIONS
  
  ! String manipulation
  USE MOD_STRUTIL

#ifdef OPENMPI_ON
   USE MPI
#endif

   !$ USE OMP_LIB

  IMPLICIT NONE

  ! Data variables
  INTEGER(4) :: ndata
  INTEGER(4), DIMENSION(nsetmax) :: ndata_set=0
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: x, nc, nc_err
  ! Data variable for 2D images
  INTEGER(4) :: nx=0, ny=0
  INTEGER(4), ALLOCATABLE, DIMENSION(:,:) :: adata
  INTEGER(1), ALLOCATABLE, DIMENSION(:,:) :: adata_mask
  ! Likelihood variables
  REAL(8) :: const_ll = 0.

  ! A generic string map for the specstr input parsing when loading the datafile
  TYPE :: SpecMapPair_t
   CHARACTER(128)               :: key   = ''
   INTEGER                      :: value
   TYPE(SpecMapPair_t), POINTER :: next  => null()
   LOGICAL                      :: valid = .FALSE.
  END TYPE SpecMapPair_t  
  TYPE :: SpecMap_t
   ! PRIVATE
   TYPE(SpecMapPair_t), ALLOCATABLE :: pairs(:)
   INTEGER                          :: capacity = 0
   INTEGER                          :: length   = 0  
   CONTAINS
   PROCEDURE :: init   => SPEC_MAP_INIT
   PROCEDURE :: free   => SPEC_MAP_FREE
   PROCEDURE :: find   => SPEC_MAP_FIND
   PROCEDURE :: insert => SPEC_MAP_INSERT
  END TYPE SpecMap_t

  TYPE(SpecMap_t) :: specstr_ordermap

CONTAINS


   SUBROUTINE SPEC_MAP_INIT(map, cap)
      CLASS(SpecMap_t), INTENT(OUT) :: map
      INTEGER         , INTENT(IN)  :: cap

      IF(map%capacity.NE.0) RETURN

      map%capacity = cap
      map%length = 0
      ALLOCATE(map%pairs(cap))
   END SUBROUTINE

   RECURSIVE SUBROUTINE CLEAR_LL(parent)
      TYPE(SpecMapPair_t), POINTER, INTENT(INOUT) :: parent

      IF(ASSOCIATED(parent%next)) THEN
         CALL CLEAR_LL(parent%next)
      ENDIF
      DEALLOCATE(parent)
   END SUBROUTINE

   SUBROUTINE SPEC_MAP_FREE(map)
      CLASS(SpecMap_t), INTENT(INOUT) :: map
      INTEGER                         :: i

      IF(map%capacity.NE.0) THEN

         DO i = 1, map%capacity
            IF(ASSOCIATED(map%pairs(i)%next)) THEN
                  CALL CLEAR_LL(map%pairs(i)%next)
            ENDIF
         END DO

         DEALLOCATE(map%pairs)
      ENDIF
   END SUBROUTINE

   SUBROUTINE SPEC_MAP_FIND(map, key, output, error)
      USE iso_fortran_env
      CLASS(SpecMap_t), INTENT(IN), TARGET :: map
      CHARACTER(*)    , INTENT(IN)         :: key
      integer         , INTENT(OUT)        :: output
      LOGICAL         , INTENT(OUT)        :: error
      TYPE(SpecMapPair_t), POINTER         :: pair
      INTEGER                              :: hash
      INTEGER                              :: index

      CALL MURMURHASH2_32(key, LEN_TRIM(key), hash)

      index = MODULO(hash, map%capacity)
      pair  => map%pairs(index)

      IF(.NOT.pair%valid) THEN
         error = .TRUE.
         RETURN
      ENDIF

      DO
         IF(TRIM(pair%key).EQ.TRIM(key)) THEN
            error  = .FALSE.
            output = pair%value
            RETURN
         ENDIF

         IF(.NOT.ASSOCIATED(pair%next)) THEN
            error = .TRUE.
            RETURN
         ENDIF
         pair = pair%next
      END DO
   END SUBROUTINE

   SUBROUTINE SPEC_MAP_INSERT(map, key, input)
      CLASS(SpecMap_t)   , INTENT(INOUT), TARGET :: map
      CHARACTER(*)       , INTENT(IN)            :: key
      INTEGER            , INTENT(IN)            :: input
      TYPE(SpECMapPair_t), POINTER               :: pair
      INTEGER                                    :: hash
      INTEGER                                    :: index

      CALL MURMURHASH2_32(key, LEN_TRIM(key), hash)

      index = MODULO(hash, map%capacity)
      pair  => map%pairs(index)

      ! Insert new (without collision)
      IF(.NOT.pair%valid) THEN
         map%pairs(index) = SpecMapPair_t(key, input, null(), .TRUE.)
         RETURN
      ENDIF
      
      DO
         ! Update
         IF(TRIM(pair%key).EQ.TRIM(key)) THEN
            pair%value = input
            RETURN
         ENDIF

         ! Insert new (with collision)
         IF(.NOT.ASSOCIATED(pair%next)) THEN
            ALLOCATE(pair%next)
            pair%next = SpecMapPair_t(key, input, null(), .TRUE.)
            RETURN
         ENDIF

         pair = pair%next
      END DO
   END SUBROUTINE

#define DATA_IS_C   B'00000001'
#define DATA_IS_E   B'00000010'
#define DATA_IS_1D  B'00010000'
#define DATA_IS_2D  B'00100000'
#define DATA_IS_SET B'10000000'
#define BIT_CHECK_IF(what) (IAND(dataid, what).GT.0)

  SUBROUTINE PREINIT_LIKELIHOOD_DATA()
   CALL specstr_ordermap%init(64)   ! Init the specstr map for data file load ordering
  END SUBROUTINE 

  SUBROUTINE POPULATE_INPUTFILES(filestr)
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: filestr
   INTEGER                      :: count, i

   CALL SPLIT_INPUT_ON(',', filestr, filename, count, nsetmax)

   IF(count.GT.1) THEN
      ! We have a set of files
      CALL LOG_TRACE('Running for a set of files.')

      is_set = .TRUE.
      nset = count
      DO i = 1, count
         filename(i) = ADJUSTL(filename(i)) ! Skip possible left spaces from comma separation
      END DO
   ELSE
      nset = 1
      is_set = .FALSE.
   ENDIF

  END SUBROUTINE

  SUBROUTINE POPULATE_DATATYPE(specstr, output)
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN)  :: specstr
   CHARACTER(3)    , INTENT(OUT) :: output
   CHARACTER(64)                 :: specifiers(specstrmaxcol)
   CHARACTER                     :: dimensions
   INTEGER                       :: count
   INTEGER                       :: i, j
   INTEGER                       :: order_int
   LOGICAL                       :: found_spec, spec_err

! Ignore populating the data for the nested_fit_func target
#ifndef FUNC_TARGET
   ! Declare the valid spec string fields
   CHARACTER(32), PARAMETER :: spec_fields(7) = [CHARACTER(LEN=32) ::&
         'x',&
         'y',&
         ! 'xe',&
         ! 'ye',&
         'c',&
         'ce',&
         't',&
         'i',&
         'img'&
      ]

   CALL SPLIT_INPUT_ON(',', specstr, specifiers, count, specstrmaxcol)
   
   IF(count.LT.1) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Specified `specstr` is not valid.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF

   CALL specstr_ordermap%insert('ncols', count)

   ! NOTE(César): Special case for image data (legacy 2D)
   IF(TRIM(spec_str).EQ.'img') THEN
      CALL specstr_ordermap%insert('img_v', 1)
      output = '2c'
      RETURN
   ELSE IF(INDEX(spec_str, 'img').GT.0) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Specified `specstr` is not valid.')
      CALL LOG_ERROR('Spec name `img` is an exclusive spec. But other specs were found.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF
   
   dimensions = ' '
   found_spec = .FALSE.
   DO i = 1, count
      ! Try and find a spec
      DO j = 1, 7
         IF(TRIM(specifiers(i)).EQ.TRIM(spec_fields(j))) THEN
            CALL LOG_TRACE('specstr: Found valid specifier `'//TRIM(spec_fields(j))//'`.')
            ! HACK(César): This is required for the hash to work with some keys (why ???)
            CALL specstr_ordermap%insert(TRIM(spec_fields(j))//'_v', i)
            found_spec = .TRUE.
            GOTO 919
         ENDIF
      END DO

      919   CONTINUE
      IF(.NOT.found_spec) THEN
         CALL LOG_ERROR_HEADER()
         CALL LOG_ERROR('Specified `specstr` is not valid.')
         CALL LOG_ERROR('Spec name `'//TRIM(specifiers(i))//'` is not a valid name.')
         CALL LOG_ERROR('Aborting Execution...')
         CALL LOG_ERROR_HEADER()
         CALL HALT_EXECUTION()
      ENDIF
      found_spec = .FALSE.
   END DO

   CALL specstr_ordermap%find('x_v', order_int, spec_err)
   IF(spec_err) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Specified `specstr` is valid.')
      CALL LOG_ERROR('But spec `x` is required and was not found.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF
   dimensions = '1'

   CALL specstr_ordermap%find('y_v', order_int, spec_err)
   IF(.NOT.spec_err) THEN
      dimensions = '2'
   ENDIF

   CALL specstr_ordermap%find('c_v', order_int, spec_err)
   IF(spec_err) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Specified `specstr` is valid.')
      CALL LOG_ERROR('But spec `c` is required and was not found.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF

   output(1:1) = dimensions(1:1)

   CALL specstr_ordermap%find('ce_v', order_int, spec_err)
   IF(.NOT.spec_err) THEN
      output(2:2) = 'e'
   ELSE
      output(2:2) = 'c'
   ENDIF
#endif
  END SUBROUTINE

  SUBROUTINE POPULATE_FILEFMT(format)
   IMPLICIT NONE
   CHARACTER(LEN=16), INTENT(INOUT) :: format

   CALL STR_TO_LOWER(format)
   IF((TRIM(format).NE.'.csv').AND.(TRIM(format).NE.'.tsv')) THEN
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Unrecognized filefmt: `'//TRIM(format)//'`.')
      CALL LOG_ERROR('Aborting Execution...')
      CALL LOG_ERROR_HEADER()
      CALL HALT_EXECUTION()
   ENDIF

  END SUBROUTINE

  SUBROUTINE INIT_LIKELIHOOD_DATA()
    ! Initialize the normal likelihood with data files and special function

    ! Separate the name of the different files
    CALL POPULATE_INPUTFILES(filenames)

    ! Populate data formats
    CALL POPULATE_DATATYPE(spec_str, data_type)
    CALL POPULATE_FILEFMT(fileformat)

    ! Read data ------------------------------------------------------------------------------------------------------------------------
    CALL READ_DATA()

    ! Is it a legacy function?
    LEGACY_USERFCN = IS_LEGACY_USERFCN(funcname(1))

    ! Initialize functions
    CALL INIT_FUNCTIONS()

    ! Initialize likelihood function
    CALL INIT_LIKELIHOOD_FUNC()

    ! Free the spec string map
    CALL specstr_ordermap%free() ! NOTE(César): This is safe for a non-inited map

  END SUBROUTINE INIT_LIKELIHOOD_DATA

  !#####################################################################################################################

!   SUBROUTINE INIT_SEARCH_METHOD()
! #ifdef OPENMPI_ON
!    INTEGER(4) :: mpi_ierror
! #endif

!     IF (search_method.eq.'RANDOM_WALK') THEN
!       searchid = 0
!     ELSE IF(search_method.EQ.'UNIFORM') THEN
!        searchid = 1
!     ELSE IF(search_method.EQ.'SLICE_SAMPLING_TRANSF') THEN
!        searchid = 2
!     ELSE IF(search_method.EQ.'SLICE_SAMPLING') THEN
!        searchid = 3
!     ELSE IF(search_method.EQ.'SLICE_SAMPLING_ADAPT') THEN
!        searchid = 4
!     ELSE
!       CALL LOG_ERROR_HEADER()
!       CALL LOG_ERROR('Error of the search type name in Mod_likelihood module.')
!       CALL LOG_ERROR('Check the manual and the input file.')
!       CALL LOG_ERROR('Available options: [RANDOM_WALK, UNIFORM, SLICE_SAMPLING, SLICE_SAMPLING_ADAPT]')
!       CALL LOG_ERROR_HEADER()
!       CALL HALT_EXECUTION()
!     END IF
!   END SUBROUTINE INIT_SEARCH_METHOD

  SUBROUTINE INIT_LIKELIHOOD_FUNC()
#ifdef OPENMPI_ON
      INTEGER(4) :: mpi_ierror
#endif

      IF(TRIM(likelihood_funcname).eq.'GAUSSIAN') THEN
         loglikefuncid = 0
      ELSE IF(TRIM(likelihood_funcname).eq.'MOD_JEFFREYS') THEN
         loglikefuncid = 1
      ELSE
         CALL LOG_ERROR_HEADER()
         CALL LOG_ERROR('Error of the likelihood function type name in Mod_likelihood module.')
         CALL LOG_ERROR('Check the manual and the input file.')
         CALL LOG_ERROR('Available options: [GAUSSIAN, MOD_JEFFREYS]')
         CALL LOG_ERROR_HEADER()
         CALL HALT_EXECUTION()
      END IF

      ! Note(César): This only works because INIT_FUNCTIONS is called earlier
      IF (BIT_CHECK_IF(DATA_IS_C).AND.loglikefuncid.GT.0) THEN
         CALL LOG_ERROR_HEADER()
         CALL LOG_ERROR('Count based data does not support non Gaussian likelihood functions.')
         CALL LOG_ERROR('Check the manual and the input file.')
         CALL LOG_ERROR_HEADER()
         CALL HALT_EXECUTION()
      END IF

  END SUBROUTINE INIT_LIKELIHOOD_FUNC

  SUBROUTINE READ_DATA()
    ! Subroutine to read data files
    INTEGER(4) :: k=0
    !  REAL(8), DIMENSION(maxdata,nsetmax) :: x_tmp=0, nc_tmp=0, nc_err_tmp=0
    REAL(8), DIMENSION(maxdata,nsetmax) :: x_tmp=0, y_tmp=0, xe_tmp=0, ye_tmp=0, c_tmp=0, ce_tmp=0
    

    ! READ DATA, calculate the constants for the likelihood function
    ! Initialize
    ndata = 0
    ndata_set = 0
    const_ll = 0.

    DO k=1, nset
      CALL LOG_TRACE('Reading file: '//TRIM(filename(k)))
      CALL READ_FILE_GENERIC(filename(k), xmin(k), xmax(k), ymin(k), ymax(k), ndata_set(k), x_tmp(:,k),&
         y_tmp(:,k), xe_tmp(:,k), ye_tmp(:,k), c_tmp(:,k), ce_tmp(:,k))
    END DO

    ndata = MAXVAL(ndata_set)
    ALLOCATE(x(ndata,nset),nc(ndata,nset),nc_err(ndata,nset)) ! Even if we are not using ce_tmp
    x = 0.
    nc = 0.
    nc_err = 0.
    DO k=1,nset
      x(1:ndata_set(k),k)      = x_tmp(1:ndata_set(k),k)
      nc(1:ndata_set(k),k)     = c_tmp(1:ndata_set(k),k)
      nc_err(1:ndata_set(k),k) = ce_tmp(1:ndata_set(k),k)
    END DO
  END SUBROUTINE READ_DATA

  !#####################################################################################################################

  SUBROUTINE TRY_GET_COLVAL_REAL(specname, varout, arrin, success)
   CHARACTER(*) , INTENT(IN)  :: specname
   REAL(8)      , INTENT(OUT) :: varout
   CHARACTER(64), INTENT(IN)  :: arrin(specstrmaxcol)
   LOGICAL      , INTENT(OUT) :: success
   INTEGER                    :: specorder
   LOGICAL                    :: error

   CALL specstr_ordermap%find(TRIM(specname), specorder, error)
   
   ! There is no such spec, early out
   IF(error) THEN
      varout = 0.
      success = .FALSE.
      RETURN
   ENDIF

   success = .TRUE.
   READ(arrin(specorder),*) varout
  END SUBROUTINE

  SUBROUTINE TRY_GET_COLVAL_CHAR(specname, varout, arrin, success)
   CHARACTER(*) , INTENT(IN)  :: specname
   CHARACTER(64), INTENT(OUT) :: varout
   CHARACTER(64), INTENT(IN)  :: arrin(specstrmaxcol)
   LOGICAL      , INTENT(OUT) :: success
   INTEGER                    :: specorder
   LOGICAL                    :: error

   CALL specstr_ordermap%find(TRIM(specname), specorder, error)
   
   ! There is no such spec, early out
   IF(error) THEN
      varout = ' '
      success = .FALSE.
      RETURN
   ENDIF

   success = .TRUE.
   varout = arrin(specorder)
  END SUBROUTINE

  FUNCTION IS_POISSON_COUNT(val)
   USE, INTRINSIC :: IEEE_ARITHMETIC
   REAL(8), INTENT(IN) :: val
   LOGICAL             :: IS_POISSON_COUNT

   IF (ABS(val-INT(val)).GT.1E-5) THEN ! Make test for integer numbers, NaN and infinites
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Input number is not integer and you are using Poisson statistic (no error bar).')
      CALL LOG_ERROR_HEADER()
      IS_POISSON_COUNT = .FALSE.
      RETURN
   ELSE IF (val.LT.0.AND.IEEE_IS_FINITE(val)) THEN ! Check if counts are negative
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Input number is negative and you are using Poisson statistic (no error bar).')
      CALL LOG_ERROR_HEADER()
      IS_POISSON_COUNT = .FALSE.
      RETURN
   ELSE IF (.NOT.IEEE_IS_FINITE(val).OR.IEEE_IS_NAN(val)) THEN ! Check infinites and nan
      CALL LOG_ERROR_HEADER()
      CALL LOG_ERROR('Infinite or "NaN" counts are not accepted.')
      CALL LOG_ERROR_HEADER()
      IS_POISSON_COUNT = .FALSE.
      RETURN
   END IF

   IS_POISSON_COUNT = .TRUE.
  END FUNCTION

  ! NOTE(César): Experimental substitute for the READ_FILE_* subroutines using the `specstr` from the input file (or the default one)
  SUBROUTINE READ_FILE_GENERIC(namefile, minx, maxx, miny, maxy, datan, x_tmp, y_tmp, xe_tmp, ye_tmp, c_tmp, ce_tmp)
   USE, INTRINSIC :: IEEE_ARITHMETIC
   CHARACTER                  , INTENT(IN)  :: namefile*64
   REAL(8)                    , INTENT(IN)  :: minx, maxx, miny, maxy
   INTEGER(4)                 , INTENT(OUT) :: datan
   REAL(8), DIMENSION(maxdata), INTENT(OUT) :: x_tmp, y_tmp, xe_tmp, ye_tmp, c_tmp, ce_tmp

   CHARACTER(64)                            :: cvars(specstrmaxcol)
   INTEGER                                  :: ncols, expected_ncols, i, j, nd
   LOGICAL                                  :: spec_err, is_not_real
   CHARACTER(1024)                          :: line
   CHARACTER(64)                            :: dummy
   REAL(8)                                  :: dummy_real

   REAL(8), PARAMETER                       :: pi = 3.141592653589793d0

   ! Which data types are present in the input file
   LOGICAL :: img_present = .FALSE.
   LOGICAL :: x_present = .FALSE.
   LOGICAL :: y_present = .FALSE.
   LOGICAL :: c_present = .FALSE.
   LOGICAL :: ce_present = .FALSE.
   LOGICAL :: t_present = .FALSE.
   LOGICAL :: i_present = .FALSE.

   ! Initialize
   x_tmp  = 0.
   y_tmp  = 0.
   xe_tmp = 0. ! NOTE(César): Reserved for future use.
   ye_tmp = 0. ! NOTE(César): Reserved for future use.
   c_tmp  = 0.
   ce_tmp = 0.
   cvars  = ' '
   nd     = 0

   ! Get the number of columns declared on the specstr
   CALL specstr_ordermap%find('ncols', expected_ncols, spec_err) ! spec_err should always be false here
   
   ! Is the input a 2D image ?
   CALL specstr_ordermap%find('img_v', i, spec_err) ! i is a dummy here

   IF(.NOT.spec_err) THEN
      ! Using legacy function for now
      CALL READ_FILE_COUNTS_2D(namefile, minx, maxx, miny, maxy, datan)

      IF(datan.EQ.0) THEN
         CALL LOG_ERROR_HEADER()
         CALL LOG_ERROR('Data count: 0, no data in the selected range. Please check your min max and data.')
         CALL LOG_ERROR_HEADER()
      ENDIF
      RETURN
   ENDIF

   ! Open file and read for non image data format
   OPEN(10,file=namefile,status='old')
   DO i=1, maxdata
      READ(10,'(a)',END=20) line

      ! Format the line according to the specifiers (CSV)
      IF(TRIM(fileformat).EQ.'.csv') THEN
         CALL SPLIT_INPUT_ON(',', TRIM(ADJUSTL(line)), cvars, ncols, specstrmaxcol)
         ! write(*,*) TRIM(line), cvars, ncols
      ELSE IF(TRIM(fileformat).EQ.'.tsv') THEN
         CALL SPLIT_INPUT_ON_V(CHAR(9)//' ', TRIM(ADJUSTL(line)), cvars, ncols, specstrmaxcol, repeat=.TRUE.)
      ENDIF

      IF(opt_file_has_header.AND.i.EQ.1) THEN
         DO j=1, ncols
            CALL TRY_PARSE_REAL(cvars(j), dummy_real, is_not_real)
            IF(.NOT.is_not_real) THEN
               CALL LOG_WARNING_HEADER()
               CALL LOG_WARNING('Input file specified a header on the data file.')
               CALL LOG_WARNING('But the header appears to have real formatted data.')
               CALL LOG_WARNING('Values could be being unintentionally ignored...')
               CALL LOG_WARNING('At column: '//TRIM(ADJUSTL(INT_TO_STR_INLINE(j))))
               CALL LOG_WARNING_HEADER()
            ENDIF
         END DO
         CYCLE
      ENDIF

      ! File does not contain the expected amount of columns
      IF(ncols.NE.expected_ncols) THEN
         CALL LOG_ERROR_HEADER()
         CALL LOG_ERROR('Input file does not have the layout specified in the `specstr`.')
         CALL LOG_ERROR('At line: '//TRIM(ADJUSTL(INT_TO_STR_INLINE(i)))//'.')
         CALL LOG_ERROR('Aborting Execution...')
         CALL LOG_ERROR_HEADER()
         CALL HALT_EXECUTION()
      ENDIF

      ! Parse each cell
      ! TODO(César): This should really run before the loop once to figure out the indices
      CALL TRY_GET_COLVAL_REAL('x_v'  , x_tmp(i) , cvars, x_present  ) ! Try for x
      CALL TRY_GET_COLVAL_REAL('c_v'  , c_tmp(i) , cvars, c_present  ) ! Try for c
      CALL TRY_GET_COLVAL_REAL('ce_v' , ce_tmp(i), cvars, ce_present ) ! Try for ce
      CALL TRY_GET_COLVAL_CHAR('i_v'  , dummy    , cvars, i_present  ) ! Try for i

      ! CALL TRY_GET_COLVAL_REAL('y_v'  , y_tmp(i) , cvars, y_present) ! Try for y ! TODO(César): Reserved for future use.
      ! CALL TRY_GET_COLVAL_REAL('t_v'  , t_tmp(i) , cvars, t_present) ! Try for t ! TODO(César): Reserved for future use.

      IF(c_present.AND.(.NOT.ce_present).AND.x_present.AND.(.NOT.y_present)) THEN
         ! 1D without errorbars (poisson)
         IF(.NOT.IS_POISSON_COUNT(c_tmp(i))) THEN
            CALL HALT_EXECUTION()
         ENDIF

         IF(x_tmp(i).GE.minx.AND.x_tmp(i).LE.maxx) THEN
            nd = nd + 1
            x_tmp(nd) = x_tmp(i)
            c_tmp(nd) = c_tmp(i)
            ! Calculation of the constant part of the likelihood with Poisson distribution
            ! Uses of the gamma function gamma(n) = (n-1)!
            IF (c_tmp(nd).GT.0.) const_ll = const_ll - DLOG_FAC(INT(c_tmp(nd)))
         END IF
      ELSE IF(c_present.AND.ce_present.AND.x_present.AND.(.NOT.y_present)) THEN
         ! 1D with errorbars (gaussian)
         IF(x_tmp(i).GE.minx.AND.x_tmp(i).LE.maxx) THEN
            nd = nd + 1
            x_tmp(nd) = x_tmp(i)
            c_tmp(nd) = c_tmp(i)
            ce_tmp(nd) = ce_tmp(i)
            IF (ce_tmp(nd).LE.0.) THEN
               CALL LOG_ERROR_HEADER()
               CALL LOG_ERROR('Errorbar with a value equal to 0 or negative.')
               CALL LOG_ERROR('At value: '//TRIM(ADJUSTL(REAL_TO_STR_INLINE(ce_tmp(i)))))
               CALL LOG_ERROR_HEADER()
               CALL HALT_EXECUTION()
            END IF
            ! Calculation of the constant part of the likelihood with Gaussian distribution
            const_ll = const_ll - DLOG(ce_tmp(i))
         END IF
      ENDIF
   ENDDO

   20 CONTINUE
   CLOSE(10)

   IF(c_present.AND.ce_present.AND.x_present.AND.(.NOT.y_present)) THEN
      const_ll = const_ll - nd*DLOG(2*pi)/2
   ENDIF
   datan = nd
   CALL LOG_TRACE('File has '//TRIM(ADJUSTL(INT_TO_STR_INLINE(nd)))//' points.')
  END SUBROUTINE

  SUBROUTINE READ_FILE_COUNTS_2D(namefile,minx,maxx,miny,maxy,datan)
    ! Read one file of data

    USE, INTRINSIC :: IEEE_ARITHMETIC

    CHARACTER, INTENT(IN) :: namefile*64
    REAL(8), INTENT(IN) :: minx, maxx, miny, maxy
    INTEGER(4), INTENT(OUT) :: datan
    !
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
       CALL LOG_ERROR_HEADER()
       CALL LOG_ERROR('Negative counts are not accepted.')
       CALL LOG_ERROR_HEADER()
       CALL HALT_EXECUTION()
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
    ! TODO(César): The e/c 1/2 could live in the same bit, making comparisons even faster
    ! TODO(César): But there wouldn't be much space for working with other values in the future
    
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
    IF(is_set) THEN
       dataid = IOR(dataid, DATA_IS_SET)
    END IF

    ! Init the funcid for function names
    IF(.NOT.BIT_CHECK_IF(DATA_IS_SET)) THEN
       IF(BIT_CHECK_IF(DATA_IS_1D)) THEN
          CALL SET_USERFUNC_PROCPTR(funcname(1))
       ELSE
          funcid = SELECT_USERFCN_2D(funcname(1))
       END IF
    ELSE
       IF(BIT_CHECK_IF(DATA_IS_2D)) THEN
          CALL LOG_ERROR_HEADER()
          CALL LOG_ERROR('Sets for 2D functions are not yet implemented.')
          CALL LOG_ERROR_HEADER()
          CALL HALT_EXECUTION()
       END IF
       CALL SET_USERFUNC_SET_PROCPTR(funcname(1))
    END IF

    ! Initialise functions if needed
    IF (.NOT.BIT_CHECK_IF(DATA_IS_SET)) THEN
       IF(funcname(1).EQ.'ROCKING_CURVE') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_ROCKING(par_in(6),par_in(7))
       ELSE IF(funcname(1).EQ.'TWO_INTERP_VOIGT_POLY'&
            .OR.funcname(1).EQ.'TWO_INTERP_VOIGT_POLY_X0') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_TWO_INTERP(par_in(14),par_in(15))
       ELSE IF(funcname(1).EQ.'THREE_INTERP_VOIGT_POLY') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_THREE_INTERP(par_in(12),par_in(13),par_in(14))
       ELSE IF(funcname(1).EQ.'SIX_GAUSS_SHIRLEYBG'&
            .OR.funcname(1).EQ.'SIX_VOIGT_SHIRLEYBG'&
            .OR.funcname(1).EQ.'SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES'&
            .OR.funcname(1).EQ.'SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES') THEN
          CALL INIT_SHIRLEY(ndata,x(:,1),nc(:,1))
       END IF
    ELSE
       IF(funcname(1).EQ.'ROCKING_CURVE_SET') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_ROCKING_SET(par_in(9),par_in(10),par_in(11),par_in(12))
       END IF
    END IF

  END SUBROUTINE INIT_FUNCTIONS

!#####################################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD_DATA(npar, par)
    ! Main likelihood function

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(npar), INTENT(IN) :: par

    !$OMP CRITICAL
    ncall=ncall+1
    IF(ncall == 1.E+9) THEN
       ncall9=ncall9+1
       ncall=0
    END IF
    !$OMP END CRITICAL
    
    IF (BIT_CHECK_IF(DATA_IS_1D)) THEN
       LOGLIKELIHOOD_DATA= LOGLIKELIHOOD_1D(npar, par)
    ELSE IF (BIT_CHECK_IF(DATA_IS_2D)) THEN
       LOGLIKELIHOOD_DATA= LOGLIKELIHOOD_2D(npar, par)
    END IF

  END FUNCTION LOGLIKELIHOOD_DATA

  !#####################################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD_WITH_TEST_DATA(npar, par)
    ! Main likelihood function with a preliminary test for Poisson
    ! This allows for avoid this test in the main loop calculation to speed the parallel computation

    
    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    !
    REAL(8) :: enc
    INTEGER(4) :: i, j, k, np
    REAL(8) :: USERFCN_2D, xx, yy

    !$OMP CRITICAL
    ncall=ncall+1
    IF(ncall == 1.E+9) THEN
       ncall9=ncall9+1
       ncall=0
    END IF
    !$OMP END CRITICAL
    
    IF (BIT_CHECK_IF(DATA_IS_C).AND.BIT_CHECK_IF(DATA_IS_1D)) THEN
       ! Check if the choosen function assumes zero or negative values
       DO k=1,nset
          DO i=1, ndata_set(k)
             ! Poisson distribution calculation --------------------------------------------------
             IF (.NOT.BIT_CHECK_IF(DATA_IS_SET)) THEN
                enc = USERFCN(x(i,k),npar,par)
             ELSE
                enc = USERFCN_SET(x(i,k),npar,par,k)
             END IF
             IF (enc.LE.0) THEN
                CALL LOG_ERROR_HEADER()
                CALL LOG_ERROR('LIKELIHOOD ERROR.')
                CALL LOG_ERROR('The user function needs to be strictly positive for all the analysis domain.')
                CALL LOG_ERROR('Function value = '//TRIM(ADJUSTL(REAL_TO_STR_INLINE(enc)))//' at:')
                CALL LOG_ERROR('x = '//TRIM(ADJUSTL(REAL_TO_STR_INLINE(x(i,k)))))
                DO np=1, npar
                  CALL LOG_ERROR(TRIM(par_name(np))//' = '//TRIM(ADJUSTL(REAL_TO_STR_INLINE(par(np)))))
                END DO
                CALL LOG_ERROR_HEADER()
                CALL HALT_EXECUTION()
             END IF
          END DO
       END DO
       LOGLIKELIHOOD_WITH_TEST_DATA = LOGLIKELIHOOD_1D(npar, par)
    ELSE IF (BIT_CHECK_IF(DATA_IS_E).AND.BIT_CHECK_IF(DATA_IS_1D)) THEN
       LOGLIKELIHOOD_WITH_TEST_DATA = LOGLIKELIHOOD_1D(npar, par)
    ELSE IF (BIT_CHECK_IF(DATA_IS_C).AND.BIT_CHECK_IF(DATA_IS_2D)) THEN
       ! Check if the choosen function assumes zero or negative values
       DO i=1, nx
          DO j=1, ny
             xx = i - 0.5 + xmin(k) ! Real coordinates are given by the bins, the center of the bin.
             yy = j - 0.5 + ymin(k) ! additional -1 to take well into account xmin,ymin
             enc = USERFCN_2D(xx,yy,npar,par,funcid)
             IF (enc.LE.0) THEN
                CALL LOG_ERROR_HEADER()
                CALL LOG_ERROR('LIKELIHOOD ERROR.')
                CALL LOG_ERROR('The user function needs to be strictly positive for all the analysis domain.')
                CALL LOG_ERROR('Function value = '//TRIM(ADJUSTL(REAL_TO_STR_INLINE(enc)))//' at:')
                CALL LOG_ERROR('x = '//TRIM(ADJUSTL(INT_TO_STR_INLINE(adata(i,k)))))
                DO np=1, npar
                  CALL LOG_ERROR(TRIM(par_name(np))//' = '//TRIM(ADJUSTL(REAL_TO_STR_INLINE(par(np)))))
                END DO
                CALL LOG_ERROR_HEADER()
                CALL HALT_EXECUTION()
             END IF
          END DO
       END DO
       LOGLIKELIHOOD_WITH_TEST_DATA = LOGLIKELIHOOD_2D(npar, par)
    ELSE IF (BIT_CHECK_IF(DATA_IS_E).AND.BIT_CHECK_IF(DATA_IS_2D)) THEN
       LOGLIKELIHOOD_WITH_TEST_DATA = LOGLIKELIHOOD_2D(npar, par)

    END IF

  END FUNCTION LOGLIKELIHOOD_WITH_TEST_DATA

  !#####################################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD_1D(npar, par)
    
    ! Main likelihood function
    ! Type: Poisson , Gaussian , .... soon 2D I hope
    
    
    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    !
    REAL(8) :: ll_tmp, rk, rk2
    REAL(8), DIMENSION(ndata, nset) :: enc
    INTEGER(4) :: i, k
    
    ! Calculate LIKELIHOOD
    ll_tmp = 0.
    
    IF (.NOT.BIT_CHECK_IF(DATA_IS_SET)) THEN
       ! No set --------------------------------------------------------------------------------------------------------
       !TODO(César): This is unnecessary and somewhat verbose
       k=1
       IF (BIT_CHECK_IF(DATA_IS_C)) THEN
          ! Poisson distribution calculation --------------------------------------------------
          ! Calculate the function (not SIMD optimisable)
          DO i=1, ndata_set(k)
             enc(i, k) = USERFCN(x(i, k),npar,par)
             ! CALL LOG_TRACE('enc('//TRIM(REAL_TO_STR_INLINE(x(i, k)))//') = '//TRIM(REAL_TO_STR_INLINE(enc(i, k))))
             ! CALL LOG_TRACE('A = '//TRIM(REAL_TO_STR_INLINE(par(1))))
          END DO
          ! Calculate the likelihood
          !$OMP SIMD REDUCTION(+:ll_tmp)
          DO i=1, ndata_set(k)
             ll_tmp = ll_tmp + nc(i, k)*DLOG(enc(i,k)) - enc(i,k)
          END DO
       ELSE !IF (BIT_CHECK_IF(DATA_IS_E)) THEN
          SELECT CASE (loglikefuncid)
          CASE (0) !-- Gaussian likelihood
             ! Normal (Gaussian) distribution calculation --------------------------------------
             ! Calculate the function (not SIMD optimisable)
             DO i=1, ndata_set(k)
                enc(i,k) = USERFCN(x(i,k),npar,par)
               !  CALL LOG_TRACE('x = '//TRIM(REAL_TO_STR_INLINE(x(i,k)))//', y = '//TRIM(REAL_TO_STR_INLINE(enc(i, k)))//', par(1)= '//TRIM(REAL_TO_STR_INLINE(par(1)))//', par(2) ='//TRIM(REAL_TO_STR_INLINE(par(2))))
             END DO
             ! Calculate the likelihood
             !$OMP SIMD REDUCTION(+:ll_tmp)
             DO i=1, ndata_set(k)
                ll_tmp = ll_tmp - (nc(i,k) - enc(i,k))**2/(2*nc_err(i,k)**2)
             END DO
             !$OMP END SIMD
          CASE (1) !-- Modified Jeffreys likelihood 
             DO i=1, ndata_set(k)
                enc(i,k) = USERFCN(x(i,k),npar,par)
             END DO
             ! Calculate the likelihood
             !$OMP SIMD REDUCTION(+:ll_tmp)
             DO i=1, ndata_set(k)
                rk = (nc(i, k) - enc(i,k)) / nc_err(i,k)
                rk2 = rk ** 2
                ll_tmp = ll_tmp + DLOG((1 - EXP(-rk2/2))/rk2)
             ENDDO
             !$OMP END SIMD
          END SELECT
       END IF
    ELSE
       ! Set ----------------------------------------------------------------------------------------------------------
       DO k=1,nset
          IF (BIT_CHECK_IF(DATA_IS_C)) THEN
             ! Poisson distribution calculation --------------------------------------------------
             ! Calculate the function (not SIMD optimisable)
             DO i=1, ndata_set(k)
                enc(i,k) = USERFCN_SET(x(i,k),npar,par,k)
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
                enc(i,k) = USERFCN_SET(x(i,k),npar,par,k)
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

  REAL(8) FUNCTION LOGLIKELIHOOD_2D(npar, par)

    ! Main likelihood function for 2D data
    ! Type: Poisson only for the moment

    INTEGER, INTENT(IN) :: npar
    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    !
    REAL(8) :: USERFCN_2D
    REAL(8) :: ll_tmp
    REAL(8), DIMENSION(nx, ny) :: enc
    INTEGER(4) :: i, j, k

    ! Calculate LIKELIHOOD
    ll_tmp = 0.
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

  SUBROUTINE FINAL_LIKELIHOOD_DATA(live_max,par_mean,par_median_w)
    ! Final action for the likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w

    ! Write auxiliar files for plots
    IF (data_type(1:1).EQ.'1') THEN
       CALL WRITE_EXPECTED_VALUES(live_max,par_mean,par_median_w)
    ELSE IF (data_type(1:1).EQ.'2') THEN
       CALL WRITE_EXPECTED_VALUES_2D(live_max,par_mean,par_median_w)
    END IF

    CALL LOG_MESSAGE_HEADER()
    CALL LOG_MESSAGE('End of likelihood test.')
    CALL LOG_MESSAGE('Number of calls : '//TRIM(ADJUSTL(INT8_TO_STR_INLINE(ncall))))
    CALL LOG_MESSAGE_HEADER()
    OPEN(11,FILE='nf_output_n_likelihood_calls.txt',STATUS= 'UNKNOWN')
    WRITE(11,*) ncall
    CLOSE(11)
    
    ! Deallocate variables
    CALL DEALLOCATE_DATA()

  END SUBROUTINE FINAL_LIKELIHOOD_DATA

  !#####################################################################################################################
  SUBROUTINE WRITE_EXPECTED_VALUES(live_max,par_mean,par_median_w)
    ! Write all auxiliar files for plots and co.

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w
    !
    INTEGER(8), PARAMETER :: maxfit = 10000
    REAL(8) :: minx=0., maxx=0., xfit=0., dx=0., enc = 0.
    INTEGER(4) :: i=0, k=0
    ! Stuff to save separate components
    LOGICAL :: plot = .FALSE.
    REAL(8) :: yfit
    CHARACTER :: out_filename*64

    COMMON /func_plot/ plot

    !-----------------------Calculate the expected function values -------------------------

    ! Calculate the expected function value and residual for max likelihood
    IF (.NOT.is_set) THEN
       k=1
       enc = 0.
       ! max likelihood values -------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_max.dat', STATUS='unknown')
       WRITE(20,*)'# x    y data    y theory      y diff    y err'
       DO i=1, ndata
          enc = USERFCN(x(i,k),npar,live_max)
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
          enc = USERFCN(x(i,k),npar,par_mean)
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
          enc = USERFCN(x(i,k),npar,par_median_w)
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
             enc = USERFCN_SET(x(i,k),npar,live_max,k)
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
             enc = USERFCN_SET(x(i,k),npar,live_max,k)
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
             enc = USERFCN_SET(x(i,k),npar,live_max,k)
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
    IF (.NOT.is_set) THEN
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
          yfit = USERFCN(xfit,npar,live_max)
       ENDDO
       CLOSE(40)

       ! mean values
       OPEN (UNIT=40, FILE='nf_output_fit_mean.dat', STATUS='unknown')
       WRITE(40,*)'# x    y fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          yfit = USERFCN(xfit,npar,par_mean)
       ENDDO
       CLOSE(40)

       ! mean values
       OPEN (UNIT=40, FILE='nf_output_fit_median.dat', STATUS='unknown')
       WRITE(40,*)'# x    y fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          yfit = USERFCN(xfit,npar,par_median_w)
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
             yfit = USERFCN_SET(xfit,npar,live_max,k)
          ENDDO
          CLOSE(40)

          WRITE(out_filename,2001) 'nf_output_fit_mean_',k,'.dat'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          WRITE(40,*)'# x    y fit'
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,par_mean,k)
          ENDDO
          CLOSE(40)

          WRITE(out_filename,3001) 'nf_output_fit_median_',k,'.dat'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          WRITE(40,*)'# x    y fit'
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,par_median_w,k)
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
    IF (INDEX(funcname(1),"_LINE").NE.0) THEN
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

    IF (.NOT.is_set) THEN
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
             IF (INDEX(funcname(1),"_LINE").NE.0) THEN
                hx(i) = xx
             END IF
             DO j=1, ny
                IF (adata(i,j).GE.0) THEN
                   ! Poisson distribution calculation --------------------------------------------------
                   yy = j - 0.5 + ymin(k) ! additional -1 to take well into account xmin,ymin
                   aenc(i,j) = USERFCN_2D(xx,yy,npar,live_max,funcid)
                   ares(i,j) = adata(i,j) - aenc(i,j)
                   IF (INDEX(funcname(1),"_LINE").NE.0) THEN
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
          IF (INDEX(funcname(1),"_LINE").NE.0) THEN
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

    IF ((data_type.EQ.'1c').OR.(data_type.EQ.'1e')) THEN
       DEALLOCATE(x,nc,nc_err)
    ELSE IF (data_type.EQ.'2c') THEN
       DEALLOCATE(adata)
    END IF

  END SUBROUTINE DEALLOCATE_DATA

  !#####################################################################################################################

END MODULE MOD_LIKELIHOOD
