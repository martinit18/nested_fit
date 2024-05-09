! Brief : This module contains the necessary routines to write to json format
!       : It is not intended as a json compatibility module, simply a way to write to the format
! Author : César Godinho
! Date   : 04/04/2024

MODULE MOD_JSONIO
    ! Logging module
    USE MOD_LOGGER

    ! Perfporf module
    USE MOD_PERFPROF

    ! String manipulation module
    USE MOD_STRUTIL
    
    IMPLICIT NONE
    PUBLIC :: JsonEntries_t
    PRIVATE
    
    TYPE :: JsonKey_t
        CHARACTER(1024) :: key
        CHARACTER(1024) :: value
    END TYPE JsonKey_t

    TYPE :: JsonEntries_t
        PRIVATE
        TYPE(JsonKey_t), ALLOCATABLE :: buffer(:)
        INTEGER                      :: length = 0
        LOGICAL                      :: sorted = .FALSE.

        CONTAINS
        PROCEDURE, PRIVATE :: JSON_ENTRY_PUSH_STR_INTERNAL, JSON_ENTRY_PUSH_STR, JSON_ENTRY_PUSH_INT, JSON_ENTRY_PUSH_REAL,&
                              JSON_ENTRY_PUSH_REAL_ARR, JSON_ENTRY_PUSH_LOGICAL, JSON_ENTRY_PUSH_REAL8, JSON_ENTRY_PUSH_INT8
        PROCEDURE, PUBLIC  :: free  => JSON_ENTRY_FREE
        PROCEDURE, PUBLIC  :: sort  => JSON_ENTRY_SORT
        PROCEDURE, PUBLIC  :: write => JSON_ENTRY_WRITE
        GENERIC  , PUBLIC  :: push  => JSON_ENTRY_PUSH_STR, JSON_ENTRY_PUSH_INT, JSON_ENTRY_PUSH_REAL, JSON_ENTRY_PUSH_REAL_ARR,&
                                       JSON_ENTRY_PUSH_LOGICAL, JSON_ENTRY_PUSH_REAL8, JSON_ENTRY_PUSH_INT8
    END TYPE JsonEntries_t

    CONTAINS

    SUBROUTINE JSON_ENTRY_FREE(entries)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        IF(entries%length.GT.0) THEN
            DEALLOCATE(entries%buffer)
        ENDIF
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_SWAP(i, j)
        IMPLICIT NONE
        TYPE(JsonKey_t), INTENT(INOUT) :: i, j
        TYPE(JsonKey_t) :: tmp

        tmp = i
        i = j
        j = tmp
    END SUBROUTINE

    RECURSIVE SUBROUTINE JSON_QUICKSORT_ALPHANUM(entries, left, right)
        IMPLICIT NONE
        TYPE(JsonKey_t), INTENT(INOUT) :: entries(:)
        INTEGER        , INTENT(IN)    :: left
        INTEGER        , INTENT(IN)    :: right
        INTEGER         :: i, j
        TYPE(JsonKey_t) :: pivot, tmp

        IF(left.LE.right) THEN
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
                    CALL JSON_ENTRY_SWAP(entries(i), entries(j))
                    i = i + 1
                    j = j - 1
                ENDIF
            END DO
            IF(j.GT.left)  CALL JSON_QUICKSORT_ALPHANUM(entries, left, j)
            IF(i.LT.right) CALL JSON_QUICKSORT_ALPHANUM(entries, i, right)
        ENDIF
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_SORT(entries)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries

        PROFILED(JSON_ENTRY_SORT)

        CALL LOG_TRACE('jsonio: Sorting '//TRIM(ADJUSTL(INT_TO_STR_INLINE(entries%length)))//' json keys...')
        CALL JSON_QUICKSORT_ALPHANUM(entries%buffer, 1, entries%length)
        entries%sorted = .TRUE.
    END SUBROUTINE

    FUNCTION GET_ENTRY_DUMP_STR(entry)
        TYPE(JsonKey_t), INTENT(IN) :: entry
        CHARACTER(2048) :: GET_ENTRY_DUMP_STR

        IF(JSON_KEY_IS_SIMPLE(entry)) THEN
            GET_ENTRY_DUMP_STR = '"'//TRIM(ADJUSTL(entry%key))//'": '//TRIM(ADJUSTL(entry%value))
        ELSE
            GET_ENTRY_DUMP_STR = '"'//TRIM(ADJUSTL(entry%key(INDEX(entry%key, '.', BACK=.TRUE.)+1:)))//'": '//TRIM(ADJUSTL(entry%value))
        ENDIF
    END FUNCTION

    FUNCTION JSON_KEY_IS_SIMPLE(key)
        TYPE(JsonKey_t), INTENT(IN) :: key
        LOGICAL :: JSON_KEY_IS_SIMPLE

        JSON_KEY_IS_SIMPLE = (INDEX(TRIM(key%key), '.').EQ.0)
    END FUNCTION

    SUBROUTINE LIST_KEY_IDX_DIFF(a, b, count, i)
        CHARACTER(1024), INTENT(IN)  :: a(16)
        CHARACTER(1024), INTENT(IN)  :: b(16)
        INTEGER        , INTENT(IN)  :: count
        INTEGER        , INTENT(OUT) :: i

        i = 1
        DO WHILE(TRIM(a(i)).EQ.TRIM(b(i)))
            i = i + 1
            IF(i.GT.count) EXIT
        END DO
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_WRITE(entries, filename)
        IMPLICIT NONE
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: filename
        INTEGER         :: i, j, k, l, depth, last_depth, close_count
        CHARACTER(2)    :: close_str
        TYPE(JsonKey_t) :: key
        CHARACTER(1024) :: current_root_key_list(16), last_root_key_list(16)
        
        PROFILED(JSON_ENTRY_WRITE)

        current_root_key_list = ''
        last_root_key_list = ''
        depth = 0

        CALL LOG_TRACE('jsonio: Writing data to json format...')

        IF(.NOT.entries%sorted) CALL entries%sort()

        OPEN(74,FILE=TRIM(filename), STATUS='UNKNOWN')
        WRITE(74, '(a)', ADVANCE='no') '{'
        DO i = 1, entries%length
            key = entries%buffer(i)
            last_depth = depth
            
            ! Split into subkeys
            CALL SPLIT_INPUT_ON('.', key%key, current_root_key_list, depth, 16)

            ! Find out which key differs (what depth should we brace close on)
            CALL LIST_KEY_IDX_DIFF(current_root_key_list, last_root_key_list, depth, k)

            close_count = last_depth - k
            
            ! Are there more entries or just skip (newline)?
            IF(close_count.EQ.0) THEN
                WRITE(74, '(a)') ','
            ELSE
                WRITE(74, '(a)') ''
            ENDIF

            ! Close entries
            close_str = '}'
            DO j = 1, close_count
                IF(j.EQ.close_count) close_str = '},'
                WRITE(74, '(a)') TRIM(REPEAT(CHAR(9), last_depth - j)//close_str)
            END DO

            ! Write new header
            DO j = k, depth - 1
                WRITE(74, '(a)') TRIM(REPEAT(CHAR(9), j)//'"'//TRIM(current_root_key_list(j))//'": {') 
            END DO
            
            ! Finally, write the entry
            WRITE(74, '(a)', ADVANCE='no') TRIM(REPEAT(CHAR(9), depth)//GET_ENTRY_DUMP_STR(key))

            last_root_key_list = current_root_key_list
        END DO

        ! Close last entry
        WRITE(74, '(a)') ''
        DO i = 1, depth
            WRITE(74, '(a)') TRIM(REPEAT(CHAR(9), depth - i)//'}')
        END DO
        CLOSE(74)
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_STR(entries, key, value)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        CHARACTER(*)        , INTENT(IN)    :: value

        CALL JSON_ENTRY_PUSH_STR_INTERNAL(entries, key, '"'//value//'"')
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_STR_INTERNAL(entries, key, value)
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
        entries%sorted = .FALSE.
        CALL LOG_TRACE('jsonio: [key: '//TRIM(key)//'][value: '//TRIM(ADJUSTl(value))//']')
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_INT(entries, key, value)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        INTEGER             , INTENT(IN)    :: value
        CHARACTER(128) :: str

        WRITE(str, *) value
        CALL JSON_ENTRY_PUSH_STR_INTERNAL(entries, key, str)
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_REAL(entries, key, value)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        REAL                , INTENT(IN)    :: value
        CHARACTER(128) :: str
        
        WRITE(str, *) value
        CALL JSON_ENTRY_PUSH_STR_INTERNAL(entries, key, str)
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_REAL_ARR(entries, key, arr)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        REAL                , INTENT(IN)    :: arr(:)
        INTEGER         :: i
        CHARACTER(2048) :: tmp, str
        
        str = '['
        DO i = 1, SIZE(arr)
            WRITE(tmp, *) arr(i)
            IF(i.NE.1) str = TRIM(str)//','
            ! NOTE: (César): Using null char as an empty string here on merge...
            !                Idk if this is well defined on fortran specification
            str = TRIM(str)//MERGE(' ', CHAR(0), i.NE.1)//TRIM(ADJUSTL(tmp))
        END DO
        str = TRIM(str)//']'
        CALL JSON_ENTRY_PUSH_STR_INTERNAL(entries, key, str)
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_LOGICAL(entries, key, value)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        LOGICAL             , INTENT(IN)    :: value
        CHARACTER(128) :: str

        str = MERGE('true ', 'false', value)
        CALL JSON_ENTRY_PUSH_STR_INTERNAL(entries, key, str)
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_REAL8(entries, key, value)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        REAL(8)             , INTENT(IN)    :: value
        CHARACTER(128) :: str

        WRITE(str, *) value
        CALL JSON_ENTRY_PUSH_STR_INTERNAL(entries, key, str)
    END SUBROUTINE

    SUBROUTINE JSON_ENTRY_PUSH_INT8(entries, key, value)
        CLASS(JsonEntries_t), INTENT(INOUT) :: entries
        CHARACTER(*)        , INTENT(IN)    :: key
        INTEGER(8)          , INTENT(IN)    :: value
        CHARACTER(128) :: str

        WRITE(str, *) value
        CALL JSON_ENTRY_PUSH_STR_INTERNAL(entries, key, str)
    END SUBROUTINE
END MODULE MOD_JSONIO
