SUBROUTINE SPLIT_INPUT_ON(char, input, output, count, arrsize)
   IMPLICIT NONE
   CHARACTER(LEN=1) , INTENT(IN)  :: char
   CHARACTER(LEN=*) , INTENT(IN)  :: input
   INTEGER          , INTENT(IN)  :: arrsize
   CHARACTER(LEN=*) , INTENT(OUT) :: output(arrsize)
   INTEGER          , INTENT(OUT) :: count
   INTEGER                        :: i
   CHARACTER(LEN=2048)            :: ninput

   i = INDEX(input, char)
   count = 1

   IF(i.EQ.0) THEN
      ! There is no splitting to be made
      ! Return the original
      output(1) = input
      RETURN
   ENDIF

   ninput = input
   DO WHILE(i.GT.0.AND.count.LE.arrsize)
      output(count) = TRIM(ninput(1:i-1))
      ! write(*,*) count, output(count), arrsize, i
      ninput = ninput(i+1:)
      i = INDEX(ninput, char)
      count = count + 1
   END DO

   ! Don't forget the last value
   output(count) = TRIM(input(INDEX(input, char, back=.TRUE.)+1:))
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

! TODO(César): Use a tree for very large arrays to be faster to compute unique
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

    CALL SPLIT_INPUT_ON('.', filename, ext_tmp, total_sz, 16)

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
