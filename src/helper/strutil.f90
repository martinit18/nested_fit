! Brief : This module contains string manipulation functions
! Author : César Godinho
! Date   : 09/05/2024

MODULE MOD_STRUTIL
CONTAINS
    SUBROUTINE SPLIT_INPUT_ON(char, input, output, count, arrsize, repeat)
       IMPLICIT NONE
       CHARACTER(LEN=1) , INTENT(IN)    :: char
       CHARACTER(LEN=*) , INTENT(IN)    :: input
       INTEGER          , INTENT(IN)    :: arrsize
       LOGICAL, OPTIONAL                :: repeat
       CHARACTER(LEN=*) , INTENT(OUT)   :: output(arrsize)
       INTEGER          , INTENT(OUT)   :: count
       INTEGER                          :: i
       CHARACTER(LEN=2048)              :: ninput
       LOGICAL                          :: repeat_i

       i = INDEX(input, char)
       count = 1
       repeat_i = .FALSE.

       IF(i.EQ.0) THEN
          ! There is no splitting to be made
          ! Return the original
          output(1) = input
          RETURN
       ENDIF

       IF(PRESENT(repeat)) repeat_i = repeat ! Do not repeat the separator eval by default

       ninput = input
       DO WHILE(i.GT.0.AND.count.LE.arrsize)
          output(count) = TRIM(ninput(1:i-1))
          ninput = ninput(i+1:)
          ! write(*,*) count, TRIM(output(count)), ' -> ', TRIM(ninput), arrsize, i
          i = INDEX(TRIM(ninput), char)

          ! Skip the seperator until a non-separator charater is detected
          IF(repeat_i) THEN
             DO WHILE(i.EQ.1)
                ninput = ninput(i+1:)
                i = INDEX(TRIM(ninput), char)
             END DO
          ENDIF

          count = count + 1
       END DO

       ! Don't forget the last value
       output(count) = TRIM(input(INDEX(input, char, back=.TRUE.)+1:))
    END SUBROUTINE

    SUBROUTINE SPLIT_FIND_MIN(chars, input, min)
       IMPLICIT NONE
       CHARACTER(LEN=*), INTENT(IN)  :: chars
       CHARACTER(LEN=*), INTENT(IN)  :: input
       INTEGER         , INTENT(OUT) :: min
       INTEGER                       :: i, j, k

       i = LEN_TRIM(input)
       DO j = 1, LEN(chars)
          k = INDEX(input, chars(j:j))
          IF(k.LT.i.AND.k.GT.0) i = k
       END DO

       IF(i.EQ.LEN_TRIM(input)) THEN
          min = 0
       ELSE
          min = i
       ENDIF
    END SUBROUTINE

    SUBROUTINE SPLIT_INPUT_ON_V(chars, input, output, count, arrsize, repeat)
       IMPLICIT NONE
       CHARACTER(LEN=*) , INTENT(IN)    :: chars
       CHARACTER(LEN=*) , INTENT(IN)    :: input
       INTEGER          , INTENT(IN)    :: arrsize
       LOGICAL, OPTIONAL                :: repeat
       CHARACTER(LEN=*) , INTENT(OUT)   :: output(arrsize)
       INTEGER          , INTENT(OUT)   :: count
       INTEGER                          :: i
       CHARACTER(LEN=2048)              :: ninput
       LOGICAL                          :: repeat_i
       
       ! Find the min
       CALL SPLIT_FIND_MIN(chars, input, i)
       count = 1
       repeat_i = .FALSE.

       IF(i.EQ.0) THEN
          ! There is no splitting to be made
          ! Return the original
          output(1) = input
          RETURN
       ENDIF

       IF(PRESENT(repeat)) repeat_i = repeat ! Do not repeat the separator eval by default

       ninput = input
       DO WHILE(i.GT.0.AND.count.LE.arrsize)
          output(count) = TRIM(ninput(1:i-1))
          ninput = ninput(i+1:)
          ! write(*,*) count, TRIM(output(count)), ' -> ', TRIM(ninput), arrsize, i
          CALL SPLIT_FIND_MIN(chars, ninput, i)

          ! Skip the seperator until a non-separator charater is detected
          IF(repeat_i) THEN
             DO WHILE(i.EQ.1)
                ninput = ninput(i+1:)
                CALL SPLIT_FIND_MIN(chars, ninput, i)
             END DO
          ENDIF

          count = count + 1
       END DO

       ! Don't forget the last value
       CALL SPLIT_FIND_MIN(chars, ninput, i)
       output(count) = TRIM(ninput(i+1:))
    END SUBROUTINE

    SUBROUTINE STR_TO_LOWER(input)
       IMPLICIT NONE
       CHARACTER(LEN=*) , INTENT(INOUT) :: input
       INTEGER :: i, code

       DO i = 1, LEN_TRIM(input)
          code = ICHAR(input(i:i))
          IF(code.GE.65.AND.code.LE.90) THEN
             input(i:i) = CHAR(code + 32) ! NOTE(César): 'a' - 'A'
          ENDIF
       END DO
    END SUBROUTINE

    ! TODO: (César): Use a tree for very large arrays to be faster to compute unique
    SUBROUTINE STR_ARRAY_UNIQUE(array, countout)
       IMPLICIT NONE
       CHARACTER(*), INTENT(INOUT)           :: array(countout)
       CHARACTER(LEN(array(1)))              :: arrcpy(countout)
       INTEGER     , INTENT(INOUT)           :: countout
       INTEGER                               :: i, j

       countout = 1
       DO i = 1, SIZE(array)
          DO j = 1, countout
             IF(TRIM(array(i)).EQ.TRIM(arrcpy(j))) GOTO 173
          END DO
          arrcpy(countout) = array(i)
          countout = countout + 1
          173 CONTINUE
       END DO

       countout = countout - 1
       array = arrcpy
    END SUBROUTINE

    SUBROUTINE FILENAME_FIND_EXT(filename, ext)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN)  :: filename
        CHARACTER(*), INTENT(OUT) :: ext

        CHARACTER(128) :: ext_tmp(16) ! NOTE(César): Filenames with 16+ `.` chars will fail
        INTEGER        :: total_sz

        CALL SPLIT_INPUT_ON('.', filename, ext_tmp, total_sz, 16, repeat = .FALSE.)

        IF(total_sz.NE.1) THEN
            ext = TRIM(ext_tmp(total_sz))
        ELSE
            ext = ''
        ENDIF
    END SUBROUTINE

    SUBROUTINE ARRAY_JOIN(array, ch, output)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: array(:)
        CHARACTER(1), INTENT(IN) :: ch
        CHARACTER(LEN=LEN(array(1))*SIZE(array)), INTENT(OUT) :: output

        INTEGER :: i
        
        output = TRIM(array(1))
        
        DO i = 2, SIZE(array)
            output = output//ch//TRIM(array(i))
        END DO
    END SUBROUTINE

    SUBROUTINE STR_REPLACE_ALL(str, ch, with, output)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN)             :: str
        CHARACTER(1), INTENT(IN)             :: ch
        CHARACTER(1), INTENT(IN)             :: with
        CHARACTER(LEN=LEN(str)), INTENT(OUT) :: output

        INTEGER :: i

        output = TRIM(str)
        i = INDEX(output, ch)
        DO WHILE(i.GT.0)
            output(i:i) = with
            i = INDEX(output, ch)
        END DO
    END SUBROUTINE

    ! NOTE: (César) We cannot use `STR_REPLACE_ALL` for this since it is recursive on the replacement
    !               and this might add the same character to escape (e.g. '\' -> '\\')
    SUBROUTINE STR_ESCAPE_CHARS(str, ch, output)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN)             :: str
        CHARACTER(1), INTENT(IN)             :: ch
        CHARACTER(LEN=LEN(str)), INTENT(OUT) :: output

        INTEGER :: i, j, k
        k = 0
        j = 0

        i = INDEX(str, ch)
        j = i
        DO WHILE(i.GT.0)
            output = TRIM(output)//str(k:j-1)
            output = TRIM(output)//'\'//ch
            k = i + 1
            j = i
            i = INDEX(str(j+1:), ch)
            j = j + i
        END DO

        IF(k.LE.LEN_TRIM(str)) THEN
            output = TRIM(output)//str(k:)
        END IF
    END SUBROUTINE

    ! BUG: (César) Not sure why the ALLOCATE causes issues with this function when used from another filename
    !              Maybe scope issues?? Or pass by value? Or by ref?? This is weird...

    ! SUBROUTINE F_C_STRING_ALLOC(f_string, c_string)
    !     USE iso_c_binding
    !     CHARACTER(c_char), DIMENSION(:), POINTER, INTENT(OUT) :: c_string
    !     CHARACTER(LEN=*), INTENT(IN)                          :: f_string
    !
    !     INTEGER :: len
    !
    !     len = LEN_TRIM(f_string)
    !     ALLOCATE(c_string(len + 1))
    !     c_string = TRANSFER(TRIM(f_string), c_string)
    !     c_string(len + 1) = c_null_char
    ! END SUBROUTINE
    !
    ! SUBROUTINE F_C_STRING_DEALLOC(c_string)
    !     USE iso_c_binding
    !     CHARACTER(c_char), DIMENSION(:), POINTER, INTENT(INOUT) :: c_string
    !
    !     DEALLOCATE(c_string)
    ! END SUBROUTINE

    SUBROUTINE C_STRING_SIZE(c_string, len)
        USE iso_c_binding
        TYPE(c_ptr)      , INTENT(IN)  :: c_string
        INTEGER(c_size_t), INTENT(OUT) :: len

        INTERFACE
            FUNCTION c_strlen(c_stringl) RESULT(lenl) BIND(c, name='strlen')
                USE, INTRINSIC :: iso_c_binding
                TYPE(c_ptr), VALUE :: c_stringl
                INTEGER(c_size_t)  :: lenl
            END FUNCTION
        END INTERFACE

        len = c_strlen(c_string)
    END SUBROUTINE

    SUBROUTINE C_F_STRING(c_string, f_string)
        USE iso_c_binding
        TYPE(c_ptr), INTENT(IN)                    :: c_string
        CHARACTER(LEN=*), INTENT(OUT)              :: f_string
        CHARACTER(LEN=:), POINTER                  :: f_ptr
        INTEGER(c_size_t)                          :: c_size

        CALL C_STRING_SIZE(c_string, c_size)
        CALL C_F_POINTER(c_string, f_ptr)
        f_string = f_ptr(1:c_size)
    END SUBROUTINE

    ! SUBROUTINE C_F_INTEGER_ARRAY(c_intarray, f_intarray, f_size)
    !     USE iso_c_binding
    !     TYPE(c_ptr), INTENT(IN)                     :: c_intarray
    !     INTEGER, DIMENSION(:), POINTER, INTENT(OUT) :: f_intarray
    !     INTEGER, INTENT(IN)                         :: f_size
    !
    !     CALL C_F_POINTER(c_intarray, f_intarray, [f_size])
    ! END SUBROUTINE
END MODULE
