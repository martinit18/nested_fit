! Brief  : Argument parsing module
! Author : César Godinho
! Date   : 06/06/2023

module MOD_ARGPARSE

    ! Module for logging
    USE MOD_LOGGER

    IMPLICIT NONE
    PUBLIC :: parse_arguments, add_argument, argdef_t
    PRIVATE
    
    TYPE argdef_t
        CHARACTER(LEN=64)        :: long_name
        CHARACTER(LEN=2)         :: short_name
        LOGICAL                  :: supports_attr
        CHARACTER(LEN=512)       :: description
        PROCEDURE(func), POINTER :: exec => null()
    END TYPE argdef_t

    TYPE argval_t
        TYPE(argdef_t) :: arg
        CHARACTER(LEN=512) :: value
        LOGICAL :: valid
    END TYPE argval_t

    ABSTRACT INTERFACE
        SUBROUTINE func(self, value)
            IMPORT :: argdef_t, argval_t
            CLASS(argdef_t), INTENT(IN) :: self
            CHARACTER(LEN=512), INTENT(IN) :: value
        END SUBROUTINE
    END INTERFACE

    INTEGER                     :: nextargidx=1
    TYPE(argdef_t), ALLOCATABLE :: arguments(:)
    INTEGER                     :: nargs=0

    CONTAINS

    TYPE(argval_t) FUNCTION get_next_arg(argdefs)
        TYPE(argdef_t), INTENT(IN) :: argdefs(:)
        CHARACTER(LEN=512) :: raw_arg
        CHARACTER(LEN=512) :: raw_value

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
                        nextargidx = nextargidx + 1
                        RETURN
                    ELSE
                        get_next_arg = argval_t(argdefs(i), CHAR(0), .TRUE.)
                        RETURN
                    ENDIF
                END IF
            ELSEIF(raw_arg(1:1) == '-') THEN ! Using short_name
                IF(TRIM(raw_arg(2:LEN_TRIM(raw_arg))) == argdefs(i)%short_name) THEN
                    IF(LEN(TRIM(raw_arg(2:LEN_TRIM(raw_arg)))) > 2) THEN
                        get_next_arg = argval_t(argdefs(i), CHAR(0), .FALSE.)
                        WRITE(*, *) "Short argument notation '-", raw_arg(2:LEN_TRIM(raw_arg)),"' needs to have only one or two character(s)."
                        RETURN
                    END IF
                    valid_value = TRY_PEAK_NEXT(nextargidx, raw_value)
                    IF(valid_value.AND.(.NOT.argdefs(i)%supports_attr)) THEN
                        get_next_arg = argval_t(argdefs(i), CHAR(0), .FALSE.)
                        WRITE(*, *) "Argument '-", TRIM(argdefs(i)%short_name), "' does not support value assignment."
                        RETURN
                    ELSEIF(valid_value) THEN
                        get_next_arg = argval_t(argdefs(i), TRIM(raw_value), .TRUE.)
                        nextargidx = nextargidx + 1
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
        CHARACTER(LEN=512), INTENT(OUT) :: val

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

    SUBROUTINE print_description(argdef, header_sz)
        TYPE(argdef_t), INTENT(IN) :: argdef
        INTEGER, INTENT(IN)        :: header_sz
        CHARACTER(LEN=512)         :: write_tmp(2)
        CHARACTER                  :: wspace(128) = ' ' ! FIXME(César): Fixed size whitespace array
        INTEGER                    :: desc_blocks, dblock
        
        ! Write option naming
        WRITE(write_tmp(1),*) '--', argdef%long_name

        IF(argdef%short_name == "") THEN
            WRITE(*,*) '|', TRIM(write_tmp(1)), wspace(1:(header_sz - LEN_TRIM(write_tmp(1)))), '|'
        ELSE
            WRITE(write_tmp(2),*) '-', argdef%short_name
            WRITE(*,*) '|', TRIM(write_tmp(1)), ', ', TRIM(write_tmp(2)), wspace(1:(header_sz - LEN_TRIM(write_tmp(1)) - LEN_TRIM(write_tmp(2)) - 2)), '|'
        ENDIF
        
        ! Write description
        ! How many lines do we need ?
        desc_blocks = LEN_TRIM(argdef%description) / (header_sz - 2)

        IF(desc_blocks.LE.0) THEN
            WRITE(write_tmp(1), *) TRIM(argdef%description)
            WRITE(*,*) '|', TRIM(write_tmp(1)), wspace(1:(header_sz - LEN_TRIM(write_tmp(1)))), '|'
        ELSE
            DO dblock = 1, desc_blocks
                WRITE(write_tmp(1), *) TRIM(argdef%description((1 + (dblock-1)*(header_sz - 2)):(dblock*(header_sz - 2))))
                WRITE(*,*) '|', TRIM(write_tmp(1)), wspace(1:(header_sz - LEN_TRIM(write_tmp(1)))), '|'
            END DO
            ! Don't run over the last block
            WRITE(write_tmp(1), *) TRIM(argdef%description(1 + desc_blocks*(header_sz - 2):))
            WRITE(*,*) '|', TRIM(write_tmp(1)), wspace(1:(header_sz - LEN_TRIM(write_tmp(1)))), '|'
        ENDIF
    END SUBROUTINE

    SUBROUTINE add_argument(arg)
        TYPE(argdef_t), INTENT(IN) :: arg
        TYPE(argdef_t), ALLOCATABLE :: tmp(:)        

        IF(nargs.EQ.0) THEN
            ALLOCATE(arguments(2))
        ENDIF

        IF(SIZE(arguments).EQ.nargs) THEN
            CALL MOVE_ALLOC(arguments, tmp)
            ALLOCATE(arguments(nargs*2))
            arguments(1:nargs) = tmp
        ENDIF

        nargs = nargs + 1
        arguments(nargs) = arg
    END SUBROUTINE

    SUBROUTINE parse_arguments()
        USE MOD_OPTIONS
        USE MOD_METADATA

        IMPLICIT NONE
        TYPE(argval_t)    :: argval
        CHARACTER(LEN=64) :: nf_write_tmp
        CHARACTER(LEN=64) :: nf_exec_name
        CHARACTER         :: wspace(128) = ' ' ! FIXME(César): Fixed size whitespace array
        INTEGER           :: nf_header_sz = LEN('|---------------------------------------------------------------------|') - 2
        INTEGER           :: i

        ! The help command is always present
        CALL ADD_ARGUMENT(argdef_t("help", "h", .FALSE.,&
            'Prints this help screen.'&
        ))
        
        DO
            argval = get_next_arg(arguments)
            IF(argval%valid) THEN
                SELECT CASE (argval%arg%short_name)
                    CASE("h")
                        WRITE(*,*) '|---------------------------------------------------------------------|'
                        WRITE(nf_write_tmp, *) 'Nested_fit (v', version_full,')'
                        WRITE(*,*) '|', nf_write_tmp, wspace(1:(nf_header_sz - LEN(nf_write_tmp))), '|'
                        WRITE(*,*) '|=====================================================================|'
                        WRITE(nf_write_tmp, *) 'Usage: ', exec_target_name, ' [OPTION]...'
                        WRITE(*,*) '|', nf_write_tmp, wspace(1:(nf_header_sz - LEN(nf_write_tmp))), '|'
                        WRITE(*,*) '|                                                                     |'
                        WRITE(nf_write_tmp, *) 'Options:'
                        WRITE(*,*) '|', nf_write_tmp, wspace(1:(nf_header_sz - LEN(nf_write_tmp))), '|'
                        WRITE(*,*) '|                                                                     |'
                        DO i=1, nargs
                            CALL PRINT_DESCRIPTION(arguments(i), nf_header_sz)
                            WRITE(*,*) '|                                                                     |'
                        END DO
                        WRITE(*,*) '|---------------------------------------------------------------------|'
                        STOP ! Note(César): This is before initializing mpi (if we have it on) so we should be good
                    CASE DEFAULT
                        CALL argval%arg%exec(argval%value)
                END SELECT
            ELSE
                IF(argval%value.NE.CHAR(255)) THEN
                    WRITE(*,*) 'Use -h for help.'
                    CALL LOG_HEADER()
                    CALL LOG_ERROR('Nested_fit argument parsing failed!')
                    CALL LOG_ERROR('Aborting Execution...')
                    CALL LOG_HEADER()
                    STOP ! Error parsing cli inputs
                ENDIF
                EXIT
            ENDIF
        END DO
    END SUBROUTINE

end module MOD_ARGPARSE
