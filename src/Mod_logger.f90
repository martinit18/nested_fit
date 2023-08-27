MODULE MOD_LOGGER
    USE MOD_TIMESTAMP
    IMPLICIT NONE

    PUBLIC :: START_LOG, CLOSE_LOG, LOG_ERROR, LOG_WARNING, LOG_MESSAGE, LOG_TRACE, LOG_HEADER,&
              REAL_TO_STR_INLINE, INT_TO_STR_INLINE, INT8_TO_STR_INLINE
    PRIVATE

    CHARACTER(64), PARAMETER :: log_file_name = 'nf_loglast.log'
    INTEGER      , PARAMETER :: log_file_unit = 999

    CONTAINS

    SUBROUTINE START_LOG()
        OPEN(log_file_unit, FILE=log_file_name, STATUS='unknown')
    END SUBROUTINE

    SUBROUTINE CLOSE_LOG()
        CLOSE(log_file_unit)
    END SUBROUTINE

    SUBROUTINE LOG_HEADER()
        WRITE(*,*)     '------------------------------------------------------------------------------------------------------------------'
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
        CALL LOG_GENERIC('<ERROR>', msg, CHAR(27)//'[31m')
    END SUBROUTINE

    SUBROUTINE LOG_WARNING(msg)
        CHARACTER(*), INTENT(IN) :: msg
        CALL LOG_GENERIC('<WARNING>', msg, CHAR(27)//'[33m')
    END SUBROUTINE

    SUBROUTINE LOG_MESSAGE(msg)
        CHARACTER(*), INTENT(IN) :: msg
        CALL LOG_GENERIC('<ATTENTION>', msg, CHAR(27)//'[39m')
    END SUBROUTINE

    SUBROUTINE LOG_TRACE(msg)
        CHARACTER(*), INTENT(IN) :: msg
#ifndef LTRACE
        RETURN ! NOTE(César): LOG_TRACE should be optimized out with just a return statement ??
#endif
        CALL LOG_GENERIC('<TRACE>', msg, '39')
    END SUBROUTINE

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