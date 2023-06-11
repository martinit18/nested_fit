MODULE MOD_OPTIONS
    ! Module for cli options
  
    IMPLICIT NONE
    LOGICAL :: opt_compact_output = .FALSE.
    CHARACTER(LEN=128) :: opt_input_file = 'nf_input.dat'
    INTEGER :: opt_ntry = 0
    
END MODULE MOD_OPTIONS