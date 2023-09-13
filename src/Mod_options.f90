MODULE MOD_OPTIONS
    ! Module for cli options
  
    IMPLICIT NONE
    LOGICAL :: opt_compact_output = .FALSE.
    CHARACTER(LEN=128) :: opt_input_file = 'nf_input.yaml'
    LOGICAL :: opt_suppress_output = .FALSE.
    
END MODULE MOD_OPTIONS