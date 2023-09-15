MODULE MOD_LOGGER
    USE MOD_TIMESTAMP
    IMPLICIT NONE

    PUBLIC :: START_LOG, CLOSE_LOG, LOG_ERROR, LOG_WARNING, LOG_MESSAGE, LOG_TRACE,&
              LOG_ERROR_HEADER, LOG_WARNING_HEADER, LOG_MESSAGE_HEADER, LOG_VERBOSITY,&
              REAL_TO_STR_INLINE, INT_TO_STR_INLINE, INT8_TO_STR_INLINE
    PRIVATE

    CHARACTER(64), PARAMETER :: log_file_name = 'nf_loglast.log'
    INTEGER      , PARAMETER :: log_file_unit = 999

    CHARACTER(16), PARAMETER :: verbosity_levels(5) = [CHARACTER(LEN=16) :: 'none', 'error', 'warning', 'message', 'trace']
    !                                                     verbosity levels  =  0       1         2          3         4
    INTEGER                  :: logger_verbosity = 4 ! Default verbosity max (trace can revert to message case not defined)

    CONTAINS

    SUBROUTINE START_LOG()
        OPEN(log_file_unit, FILE=log_file_name, STATUS='unknown')
    END SUBROUTINE

    SUBROUTINE CLOSE_LOG()
        CLOSE(log_file_unit)
    END SUBROUTINE

    SUBROUTINE LOG_VERBOSITY(verbosity)
        CHARACTER(*), INTENT(IN) :: verbosity
        INTEGER :: i

        DO i = 1, 5
            IF(TRIM(verbosity).EQ.verbosity_levels(i)) THEN
                logger_verbosity = i - 1
                CALL LOG_TRACE('Changing verbosity level to: '//TRIM(verbosity)//'.')
                RETURN
            ENDIF
        END DO
        
        CALL LOG_ERROR_HEADER()
        CALL LOG_ERROR('Selected verbosity level ('//TRIM(verbosity)//') does not exist.')
        CALL LOG_ERROR('Available options = [''none'', ''error'', ''warning'', ''message'', ''trace'']') ! Hardcoded for simplicity
        CALL LOG_ERROR_HEADER()
    END SUBROUTINE

    ! NOTE(César): This is quite cumbersome
    SUBROUTINE LOG_HEADER()
        WRITE(*,*)     '------------------------------------------------------------------------------------------------------------------'
    END SUBROUTINE

    SUBROUTINE LOG_ERROR_HEADER()
        IF(logger_verbosity.LT.1) RETURN
        WRITE(*,'(a)', advance='no') ' '//CHAR(27)//'[31m'
        CALL LOG_HEADER()
        WRITE(*, '(a)', advance='no') char(27)//'[0m'
    END SUBROUTINE

    SUBROUTINE LOG_WARNING_HEADER()
        IF(logger_verbosity.LT.2) RETURN
        WRITE(*,'(a)', advance='no') ' '//CHAR(27)//'[33m'
        CALL LOG_HEADER()
        WRITE(*, '(a)', advance='no') char(27)//'[0m'
    END SUBROUTINE

    SUBROUTINE LOG_MESSAGE_HEADER()
        IF(logger_verbosity.LT.3) RETURN
        WRITE(*,'(a)', advance='no') ' '//CHAR(27)//'[39m'
        CALL LOG_HEADER()
        WRITE(*, '(a)', advance='no') char(27)//'[0m'
    END SUBROUTINE

    SUBROUTINE LOG_GENERIC(level, msg, colorid)
        CHARACTER(*) , INTENT(IN)               :: level
        CHARACTER(*) , INTENT(IN)               :: msg
        CHARACTER(*) , INTENT(IN)               :: colorid
        CHARACTER(15)                           :: ts

        ts = timestamp()

        ! Print to terminal (assumes ANSI escape codes)
        WRITE(*,'(a, a11, a)', advance='no') ' '//colorid, TRIM(level)
        WRITE(*, *) ': ', TRIM(msg), char(27)//'[0m'

        ! Print to file
        WRITE(log_file_unit,*) TRIM(level),' : ', TRIM(msg)
    END SUBROUTINE

    SUBROUTINE LOG_ERROR(msg)
        CHARACTER(*), INTENT(IN) :: msg
        IF(logger_verbosity.LT.1) RETURN
        CALL LOG_GENERIC('<ERROR>', msg, CHAR(27)//'[31m')
    END SUBROUTINE

    SUBROUTINE LOG_WARNING(msg)
        CHARACTER(*), INTENT(IN) :: msg
        IF(logger_verbosity.LT.2) RETURN
        CALL LOG_GENERIC('<WARNING>', msg, CHAR(27)//'[33m')
    END SUBROUTINE

    SUBROUTINE LOG_MESSAGE(msg)
        CHARACTER(*), INTENT(IN) :: msg
        IF(logger_verbosity.LT.3) RETURN
        CALL LOG_GENERIC('<ATTENTION>', msg, CHAR(27)//'[39m')
    END SUBROUTINE

    SUBROUTINE LOG_TRACE(msg)
        CHARACTER(*), INTENT(IN) :: msg
#ifndef LTRACE
        RETURN ! NOTE(César): LOG_TRACE should be optimized out with just a return statement ??
#else
        IF(logger_verbosity.LT.4) RETURN
        CALL LOG_GENERIC('<TRACE>', msg, CHAR(27)//'[39m')
#endif
    END SUBROUTINE

    ! TODO(César): Even tho this is used mainly for logging, it should be moved out of here
    FUNCTION REAL_TO_STR_INLINE(val)
        REAL(8), INTENT(IN) :: val
        CHARACTER(32)       :: REAL_TO_STR_INLINE
        WRITE(REAL_TO_STR_INLINE, *) val
    END FUNCTION

    FUNCTION INT_TO_STR_INLINE(val)
        INTEGER, INTENT(IN) :: val
        CHARACTER(32)       :: INT_TO_STR_INLINE
        WRITE(INT_TO_STR_INLINE, *) val
    END FUNCTION

    FUNCTION INT8_TO_STR_INLINE(val)
        INTEGER(8), INTENT(IN) :: val
        CHARACTER(32)          :: INT8_TO_STR_INLINE
        WRITE(INT8_TO_STR_INLINE, *) val
    END FUNCTION

END MODULE MOD_LOGGER