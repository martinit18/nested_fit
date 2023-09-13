MODULE MOD_MATH
    ! Module for additional math functions

    ! Module for logging
    USE MOD_LOGGER
  
    IMPLICIT NONE
    INTEGER(4), PARAMETER :: moving_avg_window = 50 ! In samples
    REAL(8), DIMENSION(moving_avg_window) :: window_array = 0.

    CONTAINS

    ! Parse string to int
    SUBROUTINE TRY_PARSE_INT(input_str, output, error)
        IMPLICIT NONE
        CHARACTER(LEN=*)               :: input_str
        INTEGER, INTENT(OUT)           :: output
        LOGICAL, INTENT(OUT)           :: error
        INTEGER                        :: idx
     
        error = .FALSE.
        DO idx = 1, LEN_TRIM(input_str)
           IF((input_str(idx:idx).LT.'0').OR.(input_str(idx:idx).GT.'9')) THEN
              error = .TRUE.
           ENDIF
        END DO
     
        IF(.NOT.error) THEN
           READ(input_str,'(I10)') output
        ENDIF
    END SUBROUTINE

    ! Parse string to real
    SUBROUTINE TRY_PARSE_REAL(input_str, output, error)
        IMPLICIT NONE
        CHARACTER(LEN=*)               :: input_str
        REAL(8), INTENT(OUT)           :: output
        LOGICAL, INTENT(OUT)           :: error
        INTEGER                        :: idx
     
        error = .FALSE.
        DO idx = 1, LEN_TRIM(input_str)
           IF(((input_str(idx:idx).LT.'0').OR.(input_str(idx:idx).GT.'9')).AND.&
              ((input_str(idx:idx).NE.'-').AND.(input_str(idx:idx).NE.'E').AND.&
               (input_str(idx:idx).NE.'.').AND.(input_str(idx:idx).NE.'e'))) THEN
              error = .TRUE.
           ENDIF
        END DO
     
        IF(.NOT.error) THEN
           READ(input_str,*) output
        ENDIF
    END SUBROUTINE

    ! Box-muller transform used for normal distrubion
    FUNCTION RANDN()
        IMPLICIT NONE
        REAL(8), PARAMETER :: pi=3.141592653589793d0
        REAL(8)            :: RANDN, rn1, rn2

        DO
            CALL RANDOM_NUMBER(rn1)
            CALL RANDOM_NUMBER(rn2)  
            IF (rn1.GT.0.AND.rn2.GT.0) THEN
                ! For std=1, mean=0
                RANDN = (-2 * log(rn1)) ** 0.5 * cos(2*pi * rn2)

                ! ! For different mean and standard deviation
                ! RANDN = (-2 * log(rn1)) ** 0.5 * cos(2*pi * rn2)  * std + mean
                RETURN
            ENDIF
        END DO
    END FUNCTION RANDN

    ! Simple subroutine for calculation of mean and variance
    SUBROUTINE MEANVAR(data,n,mean,var)
        IMPLICIT NONE
        INTEGER(4), INTENT(IN)               :: n
        REAL(8)   , INTENT(IN), DIMENSION(n) :: data
        REAL(8)   , INTENT(OUT)              :: mean, var

        mean = 0.
        var  = 0.

        mean = SUM(data)/n
        var  = SUM((data-mean)**2)/(n-1)
    END SUBROUTINE MEANVAR

    ! Simple logarithm of the factorial based on the intrinsic function DLGAMMA
    FUNCTION DLOG_FAC(n)
        IMPLICIT NONE
        INTEGER(4), INTENT(IN) :: n
        REAL(8)                :: DLOG_FAC
        
        IF(n.LT.0) THEN
            ! Note(César): Thechnically I don't think this will ever print
            ! Since it is used for the constant part of the likelihood for poisson dist
            ! And the input file is already checked for negative values...
            CALL LOG_ERROR_HEADER()
            CALL LOG_ERROR('Error while calling DLOG_FAC with argument: '//TRIM(ADJUSTL(INT_TO_STR_INLINE(n))))
            CALL LOG_ERROR('DLOG_FAC domain is valid only for n >= 0.')
            CALL LOG_ERROR('Aborting Execution...')
            CALL LOG_ERROR_HEADER()
            CALL HALT_EXECUTION()
        ELSE IF(n.EQ.0) THEN
            ! 0!=1 so log(0!) = 0
            DLOG_FAC = 0.
        ELSE ! IF(n.GT.0)
            DLOG_FAC = LOG_GAMMA(n+1.D0)
        END IF
    END FUNCTION DLOG_FAC

    ! Subroutine to sort an array of dimension (n,ndim) ordering
    ! with respect to another array (asort) of dimension n
    SUBROUTINE SORTN(n,ndim,asort,array)
        IMPLICIT NONE
        INTEGER(4), INTENT(IN) :: n, ndim
        REAL(8)   , INTENT(INOUT), DIMENSION(n)      :: asort
        REAL(8)   , INTENT(INOUT), DIMENSION(n,ndim) :: array
        INTEGER(4)               , DIMENSION(n)      :: iwksp
        REAL(8)                  , DIMENSION(n)      :: wkspsort
        REAL(8)                  , DIMENSION(n,ndim) :: wksp
        INTEGER(4)                                   :: i, ier
        
        wkspsort = asort
        wksp     = array

        CALL DPSORT(asort,n,iwksp,1,ier)
        IF(ier.GT.0) THEN
            CALL LOG_ERROR_HEADER()
            CALL LOG_ERROR('In subroutine `SORTN`.')
            CALL LOG_ERROR('DPSORT returned the following error: '//TRIM(ADJUSTL(INT_TO_STR_INLINE(ier))))
            CALL LOG_ERROR('Aborting Execution...')
            CALL LOG_ERROR_HEADER()
            CALL HALT_EXECUTION()
        ENDIF

        DO i=1,n
            asort(i)   = wkspsort(iwksp(i))
            array(i,:) = wksp(iwksp(i),:)
        ENDDO
    END SUBROUTINE SORTN
      
    ! Subroutine to sort two arrays of dimension (n,ndim1) (n,ndim2) ordering
    ! with respect to another array (asort) of dimension n
    SUBROUTINE SORTN2(n,ndim1,ndim2,asort,array1,array2)
        IMPLICIT NONE
        INTEGER(4), INTENT(IN) :: n, ndim1, ndim2
        REAL(8)   , INTENT(INOUT), DIMENSION(n)       :: asort
        REAL(8)   , INTENT(INOUT), DIMENSION(n,ndim1) :: array1
        REAL(8)   , INTENT(INOUT), DIMENSION(n,ndim2) :: array2
        INTEGER(4)               , DIMENSION(n)       :: iwksp
        REAL(8)                  , DIMENSION(n)       :: wkspsort
        REAL(8)                  , DIMENSION(n,ndim1) :: wksp1
        REAL(8)                  , DIMENSION(n,ndim2) :: wksp2
        INTEGER(4)                                    :: i, ier
        
        wkspsort = asort
        wksp1    = array1
        wksp2    = array2

        CALL DPSORT(asort,n,iwksp,1,ier)
        IF(ier.GT.0) THEN
            CALL LOG_ERROR_HEADER()
            CALL LOG_ERROR('In subroutine `SORTN2`.')
            CALL LOG_ERROR('DPSORT returned the following error: '//TRIM(ADJUSTL(INT_TO_STR_INLINE(ier))))
            CALL LOG_ERROR('Aborting Execution...')
            CALL LOG_ERROR_HEADER()
            CALL HALT_EXECUTION()
        ENDIF

        DO i=1,n
            asort(i)    = wkspsort(iwksp(i))
            array1(i,:) = wksp1(iwksp(i),:)
            array2(i,:) = wksp2(iwksp(i),:)
        ENDDO
    END SUBROUTINE SORTN2
    
END MODULE MOD_MATH