! Brief : This module contains the necessary routines to write to json format
!       : It is not intended as a json compatibility module, simply a way to write to the format
! Author : César Godinho
! Date   : 04/04/2024

MODULE MOD_JSONIO
    ! Logging module
    USE MOD_LOGGER
    
    IMPLICIT NONE
    ! PUBLIC :: JsonObj_t
    ! PRIVATE
    PUBLIC
    
    TYPE :: JsonKey_t
        CHARACTER(1024)          :: key
        CHARACTER(1024)          :: value ! The Data buffer containing whatever we want to write
    END TYPE JsonKey_t

    TYPE :: JsonEntries_t
        ! PRIVATE
        TYPE(JsonKey_t), ALLOCATABLE :: buffer(:)
        INTEGER                      :: length   = 0

        CONTAINS
        PROCEDURE, PRIVATE :: JSON_ENTRY_PUSH_STR, JSON_ENTRY_PUSH_INT, JSON_ENTRY_PUSH_REAL
        PROCEDURE, PUBLIC  :: free => JSON_ENTRY_FREE
        PROCEDURE, PUBLIC  :: sort => JSON_ENTRY_SORT
        GENERIC  , PUBLIC  :: push => JSON_ENTRY_PUSH_STR, JSON_ENTRY_PUSH_INT, JSON_ENTRY_PUSH_REAL
    END TYPE JsonEntries_t

    CONTAINS

    SUBROUTINE JSON_ENTRY_FREE(entries)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        IF(entries%length.GT.0) THEN
            DEALLOCATE(entries%buffer)
        ENDIF
    END SUBROUTINE

    RECURSIVE SUBROUTINE JSON_QUICKSORT_ALPHANUM(entries, left, right)
        IMPLICIT NONE
        TYPE(JsonKey_t), INTENT(INOUT) :: entries(:)
        INTEGER        , INTENT(IN)    :: left
        INTEGER        , INTENT(IN)    :: right
        INTEGER         :: i, j
        TYPE(JsonKey_t) :: pivot, tmp

        IF(left.LT.right) THEN
            pivot = entries((left + right) / 2)
            i = left
            j = right
            DO WHILE(i.LE.j)
                DO WHILE(TRIM(entries(i)%key).LT.TRIM(pivot%key))
                    i = i + 1
                END DO
                DO WHILE(TRIM(entries(j)%key).GT.TRIM(pivot%key))
                    j = j - 1
                END DO

                IF(i.LE.j) THEN
                    tmp = entries(i)
                    entries(i) = entries(j)
                    entries(j) = tmp
                    i = i + 1
                    j = j - 1
                ENDIF
            END DO
            CALL JSON_QUICKSORT_ALPHANUM(entries, left, j)
            CALL JSON_QUICKSORT_ALPHANUM(entries, i, right)
        ENDIF
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_SORT(entries)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CALL LOG_TRACE('Sorting json keys...')
        CALL JSON_QUICKSORT_ALPHANUM(entries%buffer, 1, SIZE(entries%buffer))
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_STR(entries, key, value)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        CHARACTER(*)        , INTENT(IN)    :: value
        TYPE(JsonKey_t), ALLOCATABLE        :: tmp(:)

        IF(entries%length.EQ.0) THEN
            ALLOCATE(entries%buffer(2))
        ENDIF

        IF(SIZE(entries%buffer).EQ.entries%length) THEN
            CALL MOVE_ALLOC(entries%buffer, tmp)
            ALLOCATE(entries%buffer(entries%length*2))
            entries%buffer(1:entries%length) = tmp
        ENDIF

        entries%length = entries%length + 1
        entries%buffer(entries%length) = JsonKey_t(TRIM(key), TRIM(value))
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_INT(entries, key, value)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        INTEGER             , INTENT(IN)    :: value
        CHARACTER(128) :: str
        
        WRITE(str, '(I128)') value
        CALL JSON_ENTRY_PUSH_STR(entries, key, str)
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_REAL(entries, key, value)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        REAL                , INTENT(IN)    :: value
        CHARACTER(128) :: str
        
        WRITE(str, *) value
        CALL JSON_ENTRY_PUSH_STR(entries, key, str)
    END SUBROUTINE
END MODULE MOD_JSONIO
