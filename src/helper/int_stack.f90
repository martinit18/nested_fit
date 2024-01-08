MODULE MOD_INTSTACK
    USE, INTRINSIC :: iso_c_binding
    IMPLICIT NONE

    PUBLIC :: IntegerStack_t
    PRIVATE
    
    ! Normal integer
    TYPE :: IntegerStack_t
        INTEGER, DIMENSION(1024)      :: values
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

        IF(stack%index.LE.1024) THEN
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
END MODULE MOD_INTSTACK
