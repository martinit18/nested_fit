! Brief  : Manages a small cache file that handles option saving
! Author : César Godinho
! Date   : Modified - 21/07/2026

MODULE MOD_OPTIONS
    ! Module for cli options

    ! Module for metadata
    USE MOD_METADATA
  
    IMPLICIT NONE
    LOGICAL :: opt_compact_output = .FALSE.
    LOGICAL :: opt_lib_output = .FALSE.
    CHARACTER(LEN=128) :: opt_input_file = 'nf_input.yaml'
    LOGICAL :: opt_suppress_output = .FALSE.
    LOGICAL :: opt_file_has_header = .FALSE.
    
    CHARACTER(LEN=512) :: opt_cpp_comp_cmd = 'g++ -c -shared -O3 -w -fPIC'
    CHARACTER(LEN=512) :: opt_f90_comp_cmd = 'gfortran -cpp -c -shared -O3 -w -fPIC -ffree-line-length-0'
    CHARACTER(LEN=512) :: opt_lnk_cmd      = 'gcc -shared -fPIC -lgfortran'

    CONTAINS
    SUBROUTINE OPT_LOAD_CACHE()
        IMPLICIT NONE
        INTEGER :: ios

        OPEN(747, FILE=TRIM(nf_cache_folder)//'cache.opt', ACTION='read', STATUS='old', IOSTAT=ios)
        IF(ios.EQ.0) THEN
            READ(747, '(A)') opt_cpp_comp_cmd
            READ(747, '(A)') opt_f90_comp_cmd
            READ(747, '(A)') opt_lnk_cmd
        ENDIF
        CLOSE(747)
    END SUBROUTINE

    SUBROUTINE OPT_SAVE_CACHE()
        IMPLICIT NONE

        OPEN(747, FILE=TRIM(nf_cache_folder)//'cache.opt', ACTION='write', STATUS='replace')
        WRITE(747, '(A)') TRIM(opt_cpp_comp_cmd)
        WRITE(747, '(A)') TRIM(opt_f90_comp_cmd)
        WRITE(747, '(A)') TRIM(opt_lnk_cmd)
        CLOSE(747)
    END SUBROUTINE
END MODULE MOD_OPTIONS
