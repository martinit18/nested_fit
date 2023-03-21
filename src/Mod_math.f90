MODULE MOD_MATH
    ! Module for additional math functions
  
    IMPLICIT NONE
    INTEGER(4), PARAMETER :: moving_avg_window = 50 ! In samples
    REAL(8), DIMENSION(moving_avg_window) :: window_array = 0.
    
END MODULE MOD_MATH