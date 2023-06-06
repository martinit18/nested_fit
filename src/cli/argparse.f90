! Brief  : Argument parsing module
! Author : César Godinho
! Date   : 06/06/2023

module argparse

    IMPLICIT NONE
    PUBLIC :: argdef_t, argval_t, get_next_arg
    PRIVATE

    INTEGER :: nextargidx=1

    TYPE argdef_t
        CHARACTER(LEN=64) :: long_name
        CHARACTER         :: short_name
        LOGICAL           :: supports_attr
    END TYPE argdef_t

    TYPE argval_t
        TYPE(argdef_t) :: arg
        CHARACTER(LEN=128) :: value
        LOGICAL :: valid
    END TYPE argval_t

    CONTAINS

    TYPE(argval_t) FUNCTION get_next_arg(argdefs)
        TYPE(argdef_t), INTENT(IN) :: argdefs(:)
        CHARACTER(LEN=256) :: raw_arg
        CHARACTER(LEN=128) :: raw_value

        INTEGER :: i
        LOGICAL :: valid_value

        IF(nextargidx > COMMAND_ARGUMENT_COUNT()) THEN
            get_next_arg = argval_t(argdefs(nextargidx), CHAR(255), .FALSE.)
            RETURN
        ENDIF

        CALL GET_COMMAND_ARGUMENT(nextargidx, raw_arg)
        nextargidx = nextargidx + 1

        DO i = 1, size(argdefs)
            IF(raw_arg(1:2) == '--') THEN ! Using long_name
                IF(TRIM(raw_arg(3:LEN_TRIM(raw_arg))) == argdefs(i)%long_name) THEN
                    valid_value = TRY_PEAK_NEXT(nextargidx, raw_value)
                    IF(valid_value.AND.(.NOT.argdefs(i)%supports_attr)) THEN
                        get_next_arg = argval_t(argdefs(i), CHAR(0), .FALSE.)
                        WRITE(*, *) "Argument '--", TRIM(argdefs(i)%long_name), "' does not support value assignment."
                        RETURN
                    ELSEIF(valid_value) THEN
                        get_next_arg = argval_t(argdefs(i), TRIM(raw_value), .TRUE.)
                        RETURN
                    ELSE
                        get_next_arg = argval_t(argdefs(i), CHAR(0), .TRUE.)
                        RETURN
                    ENDIF
                END IF
            ELSEIF(raw_arg(1:1) == '-') THEN ! Using short_name
                IF(TRIM(raw_arg(2:2)) == argdefs(i)%short_name) THEN
                    IF(LEN(TRIM(raw_arg(2:LEN_TRIM(raw_arg)))) > 1) THEN
                        get_next_arg = argval_t(argdefs(i), CHAR(0), .FALSE.)
                        WRITE(*, *) "Short argument notation '-", raw_arg(2:LEN_TRIM(raw_arg)),"' needs to have only one character."
                        RETURN
                    END IF
                    valid_value = TRY_PEAK_NEXT(nextargidx, raw_value)
                    IF(valid_value.AND.(.NOT.argdefs(i)%supports_attr)) THEN
                        get_next_arg = argval_t(argdefs(i), CHAR(0), .FALSE.)
                        WRITE(*, *) "Argument '-", TRIM(argdefs(i)%short_name), "' does not support value assignment."
                        RETURN
                    ELSEIF(valid_value) THEN
                        get_next_arg = argval_t(argdefs(i), TRIM(raw_value), .TRUE.)
                        RETURN
                    ELSE
                        get_next_arg = argval_t(argdefs(i), CHAR(0), .TRUE.)
                        RETURN
                    ENDIF
                END IF
            ELSE ! Error
                get_next_arg = argval_t(argdefs(i), CHAR(0), .FALSE.)
                WRITE(*, *) "Argument with value '", TRIM(raw_arg), "' needs an argument name (in place arguments not allowed)."
                RETURN
            ENDIF
        END DO

        ! Not found
        get_next_arg = argval_t(argdefs(1), CHAR(0), .FALSE.)
        WRITE(*, *) "Argument '", TRIM(raw_arg), "' not found."

    END FUNCTION get_next_arg

    LOGICAL FUNCTION try_peak_next(idx, val)
        INTEGER, INTENT(IN) :: idx
        CHARACTER(LEN=128), INTENT(OUT) :: val

        IF(idx > COMMAND_ARGUMENT_COUNT()) THEN
            try_peak_next = .FALSE.
            RETURN
        ENDIF

        CALL GET_COMMAND_ARGUMENT(idx, val)

        IF((val(1:2) == '--').OR.(val(1:1) == '-')) THEN
            try_peak_next = .FALSE.
            RETURN
        ENDIF
        IF(LEN_TRIM(val).EQ.0) THEN
            try_peak_next = .FALSE.
            RETURN
        ENDIF

        try_peak_next = .TRUE.

    END FUNCTION try_peak_next

end module argparse
