! Brief  : A Module that contains multiple utility functions/types
! Author : César Godinho
! Date   : 13/08/2023


MODULE MOD_INTERPOLATE
    IMPLICIT NONE

    PUBLIC :: SplineData_t, GlobalSplineMap, INTERPOLATE_FROM_FILE, EVALUATE_SPLINE_DATA
    PRIVATE
    
    TYPE :: SplineData_t
        REAL(8), DIMENSION(:), POINTER     :: t ! Knots x val
        REAL(8), DIMENSION(:), POINTER     :: c ! Coeficients
        INTEGER                            :: n ! Dim of c and t
        INTEGER                            :: k ! Spline dimension
        INTEGER                            :: e ! Out of bounds evaluation strategy
    END TYPE SplineData_t

    TYPE :: SplineMapPair_t
        CHARACTER(128)                 :: key
        TYPE(SplineData_t)             :: value
        TYPE(SplineMapPair_t), POINTER :: next => null()
        LOGICAL                        :: valid = .FALSE.
    END TYPE SplineMapPair_t

    TYPE :: SplineMap_t
        PRIVATE
        TYPE(SplineMapPair_t), ALLOCATABLE :: pairs(:)
        INTEGER                            :: capacity = 0
        INTEGER                            :: length   = 0

        CONTAINS
        PROCEDURE, PUBLIC :: init   => SPLINE_MAP_INIT
        PROCEDURE, PUBLIC :: free   => SPLINE_MAP_FREE
        PROCEDURE, PUBLIC :: find   => SPLINE_MAP_FIND
        PROCEDURE, PUBLIC :: insert => SPLINE_MAP_INSERT
    END TYPE SplineMap_t

    TYPE(SplineMap_t) :: GlobalSplineMap

    CONTAINS

    SUBROUTINE EVALUATE_SPLINE_DATA(spline_data, x, y)
        TYPE(SplineData_t), INTENT(IN)  :: spline_data
        REAL(8)           , INTENT(IN)  :: x
        REAL(8)           , INTENT(OUT) :: y
        INTEGER                         :: ierr

        CALL SPLEV(spline_data%t, spline_data%n, spline_data%c, spline_data%k, x, y, 1, spline_data%e, ierr)
        IF(ierr.GT.0) THEN
            ! TODO(César) : Error handling
        ENDIF
    END SUBROUTINE

    SUBROUTINE INTERPOLATE_FROM_FILE(interpolator_file, output, s)
        CHARACTER(128)    , INTENT(IN)  :: interpolator_file
        TYPE(SplineData_t), INTENT(OUT) :: output
        REAL(8)           , INTENT(IN)  :: s

        INTEGER, PARAMETER :: interpmax = 10000000 ! Absolute maximum number of points for interpolation
        INTEGER, PARAMETER :: iopt      = 0        ! Using a smooth spline (automatically calculated)
        INTEGER, PARAMETER :: lwrk      = 2000000  ! Size of the working space array
        INTEGER, PARAMETER :: k         = 3        ! B-spline type (cubic)
        INTEGER            :: nest      = 0        ! Max storage for knots (theoretically should be much higher)
        REAL(8)            :: sum       = 0.       ! Total sum of the points (y-wise)
        LOGICAL            :: file_ok              ! Interpolator function file exists

        ! Input/Output variables
        REAL(8)   , ALLOCATABLE         :: x(:), y(:), w(:)
        REAL(8)   , ALLOCATABLE, TARGET :: t(:), c(:)
        REAL(8)   , ALLOCATABLE         :: wrk(:)
        INTEGER(4), ALLOCATABLE         :: iwrk(:)
        INTEGER(4)                      :: m, i, n, r
        INTEGER(4)                      :: ierr

        WRITE(*,*) ' '
        WRITE(*,*) '##### Initialization interpolation ######'
        WRITE(*,*) TRIM(interpolator_file)
        
        INQUIRE(FILE=TRIM(interpolator_file), EXIST=file_ok)
        IF(.NOT.file_ok) THEN
            ! TODO(César) : Error handling
        ENDIF

        ! Figure out the file size (i.e. the number of points)
        ! NOTE(César) : We can avoid a 2nd pass on the file if we use a vector like array aproach similar to the one on argparse.f90
        m = 0
        OPEN(1, FILE=TRIM(interpolator_file), STATUS='old')
        DO
            READ(1,*,END=10)
            m = m + 1
        END DO
        10 CLOSE(1)

        IF(m.LT.3) THEN
            ! TODO(César) : Error handling
        ENDIF

        nest = m+k+1
        ALLOCATE(wrk((m*(k+1)+nest*(7+3*k))*2), iwrk(nest))
        ALLOCATE(x(m), y(m), w(m))
        ALLOCATE(t(nest), c(nest))

        OPEN(1, file=TRIM(interpolator_file), status='old')
        DO i=1, m
            READ(1,*) x(i), y(i)
            sum = sum + y(i)
        END DO
        CLOSE(1)

        ! Normalize function and calculate the weights
        DO i=1, m
            IF (y(i).GT.0) THEN
                w(i) = 1/(DSQRT(y(i)))
            ELSE
                ! TODO(César) : Error handling
                WRITE(*,*) 'Change your simulation file. All data must be positive.'
            ENDIF
        ENDDO
        
        CALL CURFIT(iopt, m, x, y, w, x(1), x(m), k, s, nest, n, t, c, r, wrk, lwrk, iwrk, ierr)

        IF(ierr.GT.0) THEN
            ! TODO(César) : Error handling
        ENDIF

        output = SplineData_t(t, c, n, k, 1)

        DEALLOCATE(wrk, iwrk)
        DEALLOCATE(x, y, w)
    END SUBROUTINE

    SUBROUTINE SPLINE_MAP_INIT(map, cap)
        CLASS(SplineMap_t), INTENT(OUT) :: map
        INTEGER,            INTENT(IN)  :: cap

        map%capacity = cap
        map%length = 0
        ALLOCATE(map%pairs(cap))
    END SUBROUTINE

    RECURSIVE SUBROUTINE CLEAR_LL(parent)
        TYPE(SplineMapPair_t), POINTER, INTENT(INOUT) :: parent

        IF(ASSOCIATED(parent%next)) THEN
            CALL CLEAR_LL(parent%next)
        ENDIF
        DEALLOCATE(parent)
    END SUBROUTINE

    SUBROUTINE SPLINE_MAP_FREE(map)
        CLASS(SplineMap_t), INTENT(INOUT) :: map
        INTEGER                           :: i

        IF(map%capacity.NE.0) THEN

            DO i = 1, map%capacity
                IF(ASSOCIATED(map%pairs(i)%next)) THEN
                    CALL CLEAR_LL(map%pairs(i)%next)
                ENDIF
            END DO

            DEALLOCATE(map%pairs)
        ENDIF
    END SUBROUTINE

    FUNCTION ASCII_SUM(input, len)
        INTEGER  , INTENT(IN)  :: len
        CHARACTER, INTENT(IN)  :: input(len)
        INTEGER                :: ASCII_SUM, i

        ASCII_SUM = 0
        DO i = 1, len
            ASCII_SUM = ASCII_SUM + ICHAR(input(i))
        END DO

        RETURN
    END FUNCTION

    SUBROUTINE MURMURHASH2_32(input, len, output)
        INTEGER   , INTENT(IN)  :: len
        CHARACTER , INTENT(IN)  :: input(len)
        INTEGER(4), INTENT(OUT) :: output
        
        INTEGER(4), PARAMETER :: m = '5bd1e995'X
        INTEGER(4), PARAMETER :: r = 24
        INTEGER(4)            :: h, k, l, i


        ! Use ascii char sum as a seed
        h = XOR(ASCII_SUM(input, len), len)

        l = len
        i = 1
        
        DO WHILE(l.GE.4)
            k = TRANSFER(input(i:i+3), i)

            k = k * m
            k = XOR(k, SHIFTR(k, r))
            k = k * m

            h = h * m
            h = XOR(h, k)

            i = i + 4
            l = l - 4
        END DO

        ! Handle the rest
        IF(l.EQ.3) THEN
            h = XOR(h, SHIFTL(ICHAR(input(3)), 16))
        ENDIF
        
        IF(l.GE.2) THEN
            h = XOR(h, SHIFTL(ICHAR(input(2)), 8))
        ENDIF
        
        IF(l.GE.1) THEN
            h = XOR(h, ICHAR(input(1)))
            h = h * m
        ENDIF

        h = XOR(h, SHIFTR(h, 13))
        h = h * m
        h = XOR(h, SHIFTR(h, 15))

        output = h
    END SUBROUTINE

    SUBROUTINE SPLINE_MAP_FIND(map, key, output, error)
        CLASS(SplineMap_t), INTENT(IN)  :: map
        CHARACTER(128),     INTENT(IN)  :: key
        TYPE(SplineData_t), INTENT(OUT) :: output
        LOGICAL, INTENT(OUT)            :: error
        TYPE(SplineMapPair_t)           :: pair
        INTEGER                         :: hash
        INTEGER                         :: index

        CALL MURMURHASH2_32(key, LEN_TRIM(key), hash)

        index = MODULO(hash, map%capacity)
        pair  = map%pairs(index)

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

    SUBROUTINE SPLINE_MAP_INSERT(map, key, input)
        CLASS(SplineMap_t), INTENT(INOUT) :: map
        CHARACTER(128),     INTENT(IN)    :: key
        TYPE(SplineData_t), INTENT(IN)    :: input
        TYPE(SplineMapPair_t), POINTER    :: pair
        INTEGER                           :: hash
        INTEGER                           :: index

        CALL MURMURHASH2_32(key, LEN_TRIM(key), hash)

        index = MODULO(hash, map%capacity)
        pair  = map%pairs(index)

        ! Insert new (without collision)
        IF(.NOT.pair%valid) THEN
            map%pairs(index) = SplineMapPair_t(key, input, null(), .TRUE.)
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
                pair%next = SplineMapPair_t(key, input, null(), .TRUE.)
                RETURN
            ENDIF

            pair = pair%next
        END DO
    END SUBROUTINE

END MODULE MOD_INTERPOLATE
