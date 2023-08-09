! Brief : This module contains the internal functions that get compiled to aid in the user-defined functions
!         This gets compiled inside the shared library with the user functions
!         There is no need to link against this with the main nested_fit executable
! Author : César Godinho
! Date   : 09/08/2023

! Compute the real value out of the faddeeva function w(z)
FUNCTION WofzRe(zr, zi)
    REAL(8), INTENT(IN) :: zr, zi
    REAL(8) :: wr, wi
    LOGICAL :: flag
    REAL(8) :: WofzRe
    EXTERNAL WOFZ

    CALL WOFZ(zr, zi, wr, wi, flag)
    WofzRe = wr
    RETURN
END FUNCTION

FUNCTION WofzRe2(zr, zi)
    REAL(8), INTENT(IN) :: zr, zi
    REAL(8) :: wr, wi
    LOGICAL :: flag
    REAL(8) :: WofzRe
    EXTERNAL WOFZ

    CALL WOFZ(zr, zi, wr, wi, flag)
    WofzRe = wr
    RETURN
END FUNCTION
