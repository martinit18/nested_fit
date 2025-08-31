FUNCTION ASCII_SUM(input, len)
    IMPLICIT NONE

    INTEGER  , INTENT(IN)  :: len
    CHARACTER, INTENT(IN)  :: input(len)
    INTEGER(4)             :: ASCII_SUM, i

    ASCII_SUM = 0
    DO i = 1, len
        ASCII_SUM = ASCII_SUM + ICHAR(input(i))
    END DO

    RETURN
END FUNCTION

! Murmurhash2 algorithm port from C implementation
SUBROUTINE MURMURHASH2_32(input, len, output)
    IMPLICIT NONE

    INTEGER   , INTENT(IN)  :: len
    CHARACTER , INTENT(IN)  :: input(len)
    INTEGER(4), INTENT(OUT) :: output
    
    INTEGER(4), PARAMETER :: m = 1540483477
    INTEGER(4), PARAMETER :: r = 24
    INTEGER(4)            :: h, k, l, i

    INTEGER(4) :: ASCII_SUM


    ! Use ascii char sum as a seed
    h = XOR(ASCII_SUM(input, len), len)

    l = len
    i = 1
    
    DO WHILE(l.GE.4)
        k = TRANSFER(input(i:i+3), k)

        k = k * m
        k = XOR(k, SHIFTR(k, r))
        k = k * m

        h = h * m
        h = XOR(h, k)

        i = i + 4
        l = l - 4
    END DO

    ! Handle the rest
    IF(l.EQ.3) THEN
        h = XOR(h, SHIFTL(ICHAR(input(3)), 16))
    ENDIF
    
    IF(l.GE.2) THEN
        h = XOR(h, SHIFTL(ICHAR(input(2)), 8))
    ENDIF
    
    IF(l.GE.1) THEN
        h = XOR(h, ICHAR(input(1)))
        h = h * m
    ENDIF

    h = XOR(h, SHIFTR(h, 13))
    h = h * m
    h = XOR(h, SHIFTR(h, 15))

    output = h
END SUBROUTINE
