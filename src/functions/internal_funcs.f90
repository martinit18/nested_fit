! Brief : This file contains the internal functions that get compiled to aid in the user-defined functions
!         This gets compiled inside the shared library with the user functions
!         There is no need to link against this with the main nested_fit executable
! Author : César Godinho
! Date   : 09/08/2023

REAL(8) FUNCTION GAUSS_IF(x, x0, amp, sigma)
    ! Normalized Gaussian distribution
    ! The value of 'amp' is the value of the surface below the curve
    IMPLICIT NONE
    REAL(8) :: x, x0, amp, sigma
    REAL(8), PARAMETER :: pi=3.141592653589793d0

    ! Test of under of underflow first
    IF(DABS((x-x0)**2/(2*sigma**2)).LT.700) THEN:
        GAUSS_IF = amp/(DSQRT(2*pi)*sigma)*DEXP(-(x-x0)**2/(2*sigma**2))
    ELSE
        GAUSS_IF = 0.d0
    END IF

END FUNCTION GAUSS_IF


! Compute the real value out of the faddeeva function w(z)
FUNCTION WofzRe(zr, zi)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: zr, zi
    REAL(8) :: wr, wi
    LOGICAL :: flag
    REAL(8) :: WofzRe
    EXTERNAL WOFZ

    CALL WOFZ(zr, zi, wr, wi, flag)
    WofzRe = wr
    RETURN
END FUNCTION

FUNCTION Interpolate(interpolator_file, x, s)
    USE MOD_INTERPOLATE
    USE MOD_LOGGER
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: interpolator_file
    REAL(8),      INTENT(IN) :: x
    REAL(4),      INTENT(IN) :: s
    REAL(8)                  :: Interpolate
    TYPE(SplineData_t)       :: spline_data
    LOGICAL                  :: error

    CALL GlobalSplineMap%find(interpolator_file, spline_data, error)
    IF(.NOT.error) THEN
        ! Use spline data
        CALL EVALUATE_SPLINE_DATA(spline_data, x, Interpolate)
        RETURN
    ENDIF
    
    ! Interpolate the new spline data
    CALL INTERPOLATE_FROM_FILE(interpolator_file, spline_data, REAL(s, kind=8))
    CALL GlobalSplineMap%insert(interpolator_file, spline_data)

    CALL GlobalSplineMap%find(interpolator_file, spline_data, error)
    IF(.NOT.error) THEN
        CALL EVALUATE_SPLINE_DATA(spline_data, x, Interpolate)
        RETURN
    ELSE
        ! Fatal error
        CALL LOG_ERROR_HEADER()
        CALL LOG_ERROR('Failed to find the interpolation function in hashtable.')
        CALL LOG_ERROR('This is possibly a bug in the source code.')
        CALL LOG_ERROR('Please open an issue, reporting all your steps, so we are able to reproduce.')
        CALL LOG_ERROR('Aborting Execution...')
        CALL LOG_ERROR_HEADER()

        !CALL HALT_EXECUTION() ! Not working on Mac in this function only
        STOP
    ENDIF
END FUNCTION
