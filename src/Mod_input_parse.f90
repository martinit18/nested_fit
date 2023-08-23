! Brief : This module contains the necessary routines to parse the input from the (very simple) yaml file
! Author : César Godinho
! Date   : 19/08/2023

MODULE MOD_INPUTPARSE
    ! Math module
    USE MOD_MATH
    IMPLICIT NONE
    PUBLIC :: InputDataMap_t, InputDataGenericValue_t, PARSE_INPUT
    PRIVATE

    TYPE :: InputDataGenericValue_t
        CHARACTER(512) :: data
        
        CONTAINS
        PROCEDURE, PUBLIC :: toLogical   => INPUTDATA_CONVERT_VAL_LOGICAL
        PROCEDURE, PUBLIC :: toCharacter => INPUTDATA_CONVERT_VAL_CHARACTER
        PROCEDURE, PUBLIC :: toInteger   => INPUTDATA_CONVERT_VAL_INTEGER
        PROCEDURE, PUBLIC :: toReal      => INPUTDATA_CONVERT_VAL_REAL
    END TYPE InputDataGenericValue_t

    TYPE :: InputDataMapPair_t
        CHARACTER(128)                    :: key   = ''
        TYPE(InputDataGenericValue_t)     :: value
        TYPE(InputDataMapPair_t), POINTER :: next  => null()
        LOGICAL                           :: valid = .FALSE.
    END TYPE InputDataMapPair_t

    TYPE :: InputDataMap_t
        ! PRIVATE
        TYPE(InputDataMapPair_t), ALLOCATABLE :: pairs(:)
        INTEGER                               :: capacity = 0
        INTEGER                               :: length   = 0

        CONTAINS
        PROCEDURE, PUBLIC :: init   => INPUTDATA_MAP_INIT
        PROCEDURE, PUBLIC :: free   => INPUTDATA_MAP_FREE
        PROCEDURE, PUBLIC :: find   => INPUTDATA_MAP_FIND
        PROCEDURE, PUBLIC :: insert => INPUTDATA_MAP_INSERT
    END TYPE InputDataMap_t

    TYPE :: IntegerStack_t
        INTEGER, DIMENSION(32)        :: values
        INTEGER                       :: index = 0

        CONTAINS
        PROCEDURE, PUBLIC :: push  => STACK_PUSH
        PROCEDURE, PUBLIC :: pop   => STACK_POP
        PROCEDURE, PUBLIC :: top   => STACK_TOP
        PROCEDURE, PUBLIC :: empty => STACK_EMPTY
        PROCEDURE, PUBLIC :: count => STACK_COUNT
    END TYPE IntegerStack_t

    CONTAINS

    SUBROUTINE STACK_PUSH(stack, input)
        CLASS(IntegerStack_t), INTENT(INOUT) :: stack
        INTEGER              , INTENT(IN)    :: input

        IF(stack%index.LE.32) THEN
            stack%index = stack%index + 1
            stack%values(stack%index) = input
        ENDIF
    END SUBROUTINE

    SUBROUTINE STACK_POP(stack)
        CLASS(IntegerStack_t), INTENT(INOUT) :: stack

        IF(stack%index.GT.0) THEN
            stack%index = stack%index - 1
        ENDIF
    END SUBROUTINE

    SUBROUTINE STACK_TOP(stack, output)
        CLASS(IntegerStack_t), INTENT(INOUT) :: stack
        INTEGER              , INTENT(OUT)   :: output

        IF(stack%index.GT.0) THEN
            output = stack%values(stack%index)
        ENDIF
    END SUBROUTINE

    SUBROUTINE STACK_EMPTY(stack, output)
        CLASS(IntegerStack_t), INTENT(INOUT) :: stack
        LOGICAL              , INTENT(OUT)   :: output

        IF(stack%index.GT.0) THEN
            output = .FALSE.
        ELSE
            output = .TRUE.
        ENDIF
    END SUBROUTINE

    SUBROUTINE STACK_COUNT(stack, output)
        CLASS(IntegerStack_t), INTENT(INOUT) :: stack
        INTEGER              , INTENT(OUT)   :: output

        output = stack%index
    END SUBROUTINE


    SUBROUTINE COUNT_LBLANKS(input, count)
        CHARACTER(512), INTENT(IN)  :: input
        INTEGER       , INTENT(OUT) :: count
        INTEGER                     :: i

        count = 0

        DO i = 1, LEN_TRIM(input)
            IF(input(i:i).NE.' ') THEN
                RETURN
            ENDIF
            count = count + 1
        END DO
    END SUBROUTINE

    SUBROUTINE MAKE_SCOPE_STRING(scope_stack, stack_size, output)
        CHARACTER(128), DIMENSION(32), INTENT(IN)  :: scope_stack
        INTEGER                      , INTENT(IN)  :: stack_size
        CHARACTER(512)               , INTENT(OUT) :: output

        INTEGER :: i

        output = TRIM(scope_stack(1))

        DO i = 2, stack_size
            output = TRIM(output)//'.'//TRIM(scope_stack(i))
        END DO
    END SUBROUTINE

    SUBROUTINE FIRST_NON_BLANK(string, idx)
        CHARACTER(*)        , INTENT(IN)  :: string
        INTEGER             , INTENT(OUT) :: idx

        INTEGER :: i

        idx = 0

        DO i = 1, LEN_TRIM(string)
            IF(string(i:i).NE.' ') THEN
                idx = idx + 1
                RETURN
            ENDIF
            idx = idx + 1
        END DO

        idx = 0
    END SUBROUTINE

    SUBROUTINE PARSE_INPUT(filename, config)
        CHARACTER(*)        , INTENT(IN)  :: filename
        TYPE(InputDataMap_t), INTENT(OUT) :: config

        INTEGER                       :: i, lblanks, top, count, nbindex, lastidx
        CHARACTER(512)                :: line, key
        CHARACTER(128)                :: name
        TYPE(IntegerStack_t)          :: parent_stack
        CHARACTER(128), DIMENSION(32) :: scope = ''
        LOGICAL                       :: empty_stack
        
        CALL config%init(512) ! Make a big map

        ! Read the whole file and parse the lines
        OPEN(1, FILE=TRIM(filename), STATUS='old')
        DO
            READ(1,'(a)',END=10) line

            ! Ignore blank lines and line comments
            CALL FIRST_NON_BLANK(line, nbindex)
            IF((nbindex.EQ.0).OR.line(nbindex:nbindex).EQ.'#') THEN
                CYCLE
            ENDIF

            ! Ignore inline comments
            lastidx = INDEX(line, '#')
            IF(lastidx.EQ.0) THEN
                lastidx = LEN_TRIM(line)
            ELSE
                lastidx = LEN_TRIM(line(1:(lastidx - 1)))
            ENDIF

            ! Process line and get scope
            i = INDEX(line, ':')

            name = TRIM(line(1:i-1))

            ! Count left blanks on line
            CALL COUNT_LBLANKS(line, lblanks)

            ! Unwind stack until lblanks meet last known parent
            CALL parent_stack%empty(empty_stack)
            IF(.NOT.empty_stack) THEN
                CALL parent_stack%top(top)
                DO WHILE((top.GE.lblanks).AND.(.NOT.empty_stack))
                    CALL parent_stack%pop()
                    CALL parent_stack%empty(empty_stack)
                    CALL parent_stack%top(top) ! top is safe against empty
                END DO
            ENDIF

            ! Set this as the new parent
            CALL parent_stack%push(lblanks)
            CALL parent_stack%count(count)
            scope(count) = TRIM(ADJUSTL(name))

            IF(lastidx.NE.i) THEN
                ! Make the key based on current scope
                CALL MAKE_SCOPE_STRING(scope, count, key)

                WRITE(*,*) TRIM(key), ' = ', TRIM(ADJUSTL(line(i+1:lastidx)))

                ! There is content in the line => map it
                CALL config%insert(key, InputDataGenericValue_t(TRIM(ADJUSTL(line(i+1:lastidx)))))
            ENDIF
        END DO
10      CLOSE(1)
    END SUBROUTINE

    FUNCTION INPUTDATA_CONVERT_VAL_LOGICAL(this)
        CLASS(InputDataGenericValue_t), INTENT(IN) :: this
        LOGICAL                                    :: INPUTDATA_CONVERT_VAL_LOGICAL

        IF(&
            (TRIM(this%data).EQ.'true').OR.&
            (TRIM(this%data).EQ.'True').OR.&
            (TRIM(this%data).EQ.'TRUE')&
        ) THEN
            
            INPUTDATA_CONVERT_VAL_LOGICAL = .TRUE.    
        ELSE IF(&
            (TRIM(this%data).EQ.'false').OR.&
            (TRIM(this%data).EQ.'False').OR.&
            (TRIM(this%data).EQ.'FALSE')&
        ) THEN
            INPUTDATA_CONVERT_VAL_LOGICAL = .FALSE.
        ELSE
            WRITE(*,*)           '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*)           '       ERROR:           Failed to convert value `', TRIM(this%data), '` to logical.'
            WRITE(*,'(a,I2,a)') '        ERROR:           Expected `true/false` or `True/False` or `TRUE/FALSE`.'
            WRITE(*,*)           '       ERROR:           Aborting Execution...'
            WRITE(*,*)           '------------------------------------------------------------------------------------------------------------------'
            STOP
            ! NOTE(César) : This works before OpenMPI init!
        ENDIF
        RETURN

    END FUNCTION

    FUNCTION INPUTDATA_CONVERT_VAL_CHARACTER(this)
        CLASS(InputDataGenericValue_t), INTENT(IN) :: this
        CHARACTER(512)                             :: INPUTDATA_CONVERT_VAL_CHARACTER

        INPUTDATA_CONVERT_VAL_CHARACTER = this%data
        RETURN
    END FUNCTION

    FUNCTION INPUTDATA_CONVERT_VAL_INTEGER(this)
        CLASS(InputDataGenericValue_t), INTENT(IN) :: this
        INTEGER                                    :: INPUTDATA_CONVERT_VAL_INTEGER
        LOGICAL                                    :: error
        
        CALL TRY_PARSE_INT(this%data, INPUTDATA_CONVERT_VAL_INTEGER, error)

        IF(error) THEN
            WRITE(*,*)           '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*)           '       ERROR:           Failed to convert value `', TRIM(this%data), '` to integer.'
            WRITE(*,*)           '       ERROR:           Aborting Execution...'
            WRITE(*,*)           '------------------------------------------------------------------------------------------------------------------'
            STOP
            ! NOTE(César) : This works before OpenMPI init!
        ENDIF

        RETURN
    END FUNCTION

    FUNCTION INPUTDATA_CONVERT_VAL_REAL(this)
        CLASS(InputDataGenericValue_t), INTENT(IN) :: this
        REAL(8)                                    :: INPUTDATA_CONVERT_VAL_REAL
        LOGICAL                                    :: error
        
        CALL TRY_PARSE_REAL(this%data, INPUTDATA_CONVERT_VAL_REAL, error)

        IF(error) THEN
            WRITE(*,*)           '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*)           '       ERROR:           Failed to convert value `', TRIM(this%data), '` to real.'
            WRITE(*,*)           '       ERROR:           Aborting Execution...'
            WRITE(*,*)           '------------------------------------------------------------------------------------------------------------------'
            STOP
            ! NOTE(César) : This works before OpenMPI init!
        ENDIF

        RETURN
    END FUNCTION

    

    SUBROUTINE INPUTDATA_MAP_INIT(map, cap)
        CLASS(InputDataMap_t), INTENT(OUT) :: map
        INTEGER,               INTENT(IN)  :: cap
        
        IF(map%capacity.NE.0) RETURN
        
        map%capacity = cap
        map%length = 0
        ALLOCATE(map%pairs(cap))
    END SUBROUTINE

    RECURSIVE SUBROUTINE CLEAR_LL(parent)
        TYPE(InputDataMapPair_t), POINTER, INTENT(INOUT) :: parent

        IF(ASSOCIATED(parent%next)) THEN
            CALL CLEAR_LL(parent%next)
        ENDIF
        DEALLOCATE(parent)
    END SUBROUTINE

    SUBROUTINE INPUTDATA_MAP_FREE(map)
        CLASS(InputDataMap_t), INTENT(INOUT) :: map
        INTEGER                              :: i

        IF(map%capacity.NE.0) THEN

            DO i = 1, map%capacity
                IF(ASSOCIATED(map%pairs(i)%next)) THEN
                    CALL CLEAR_LL(map%pairs(i)%next)
                ENDIF
            END DO

            DEALLOCATE(map%pairs)
        ENDIF
    END SUBROUTINE

    SUBROUTINE INPUTDATA_MAP_FIND(map, key, output, error)
        USE iso_fortran_env
        CLASS(InputDataMap_t)        , INTENT(IN), TARGET  :: map
        CHARACTER(*)                 , INTENT(IN)          :: key
        TYPE(InputDataGenericValue_t), INTENT(OUT)         :: output
        LOGICAL, INTENT(OUT)                               :: error
        TYPE(InputDataMapPair_t), POINTER                  :: pair
        INTEGER                                            :: hash
        INTEGER                                            :: index

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

    SUBROUTINE INPUTDATA_MAP_INSERT(map, key, input)
        CLASS(InputDataMap_t)        , INTENT(INOUT), TARGET :: map
        CHARACTER(*)                 , INTENT(IN)            :: key
        TYPE(InputDataGenericValue_t), INTENT(IN)            :: input
        TYPE(InputDataMapPair_t), POINTER                    :: pair
        INTEGER                                              :: hash
        INTEGER                                              :: index

        CALL MURMURHASH2_32(key, LEN_TRIM(key), hash)

        index = MODULO(hash, map%capacity)
        pair  => map%pairs(index)

        ! Insert new (without collision)
        IF(.NOT.pair%valid) THEN
            map%pairs(index) = InputDataMapPair_t(key, input, null(), .TRUE.)
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
                pair%next = InputDataMapPair_t(key, input, null(), .TRUE.)
                RETURN
            ENDIF

            pair = pair%next
        END DO
    END SUBROUTINE

END MODULE MOD_INPUTPARSE
