SUBROUTINE SPLIT_INPUT_ON(char, input, output, count, arrsize)
   IMPLICIT NONE
   CHARACTER(LEN=1) , INTENT(IN)  :: char
   CHARACTER(LEN=*) , INTENT(IN)  :: input
   INTEGER          , INTENT(IN)  :: arrsize
   CHARACTER(LEN=64), INTENT(OUT) :: output(arrsize)
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
   CHARACTER(LEN=*) , INTENT(INOUT) :: input
   INTEGER :: i, code

   DO i = 1, LEN_TRIM(input)
      code = ICHAR(input(i:i))
      IF(code.GE.65.AND.code.LE.90) THEN
         input(i:i) = CHAR(code + 32) ! NOTE(César): 'a' - 'A'
      ENDIF
   END DO
END SUBROUTINE
