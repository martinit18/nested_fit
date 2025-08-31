MODULE MOD_OPTIONS
    ! Module for cli options
  
    IMPLICIT NONE
    LOGICAL :: opt_compact_output = .FALSE.
    LOGICAL :: opt_lib_output = .FALSE.
    CHARACTER(LEN=128) :: opt_input_file = 'nf_input.yaml'
    LOGICAL :: opt_suppress_output = .FALSE.
    LOGICAL :: opt_file_has_header = .FALSE.
    
    ! TODO(César): This should be in a config file.
    ! TODO(César): Also add this to the nf cache to load on next runs.
    CHARACTER(LEN=512) :: opt_cpp_comp_cmd = 'g++ -c -shared -O3 -w -fPIC'
    CHARACTER(LEN=512) :: opt_f90_comp_cmd = 'gfortran -cpp -c -shared -O3 -w -fPIC -ffree-line-length-0'
    CHARACTER(LEN=512) :: opt_lnk_cmd      = 'gcc -shared -fPIC -lgfortran'
    
END MODULE MOD_OPTIONS
