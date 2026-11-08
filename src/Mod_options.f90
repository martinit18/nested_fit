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
    ! GPU backend, from the input file (`search.device`, `search.gpu_min_points`); the
    ! environment (NESTED_FIT_DEVICE, NESTED_FIT_GPU_MIN_POINTS) overrides both.
    CHARACTER(LEN=128) :: opt_device = 'auto'
    INTEGER :: opt_gpu_min_points = 0
    ! `function.gpu_source`: a C file defining nf_user(x, params) for a legacy (Fortran) user
    ! function, compiled for the device at run time. params[i] is the parameter with npar = i+1.
    CHARACTER(LEN=512) :: opt_gpu_source = ''
    ! `function.source`: a Fortran file beside the input, compiled at run time into its own
    ! library, defining the function named by `function.expression` in the legacy convention
    ! FUNCTION NAME(x, npar, val). No copy into src/, no rebuild of nested_fit.
    CHARACTER(LEN=512) :: opt_function_source = ''
    ! `search.gpu_tolerance`: the error allowed on log L by the mixed precision of the GPU, in
    ! log L units. The startup self-test measures the error at a ladder of thresholds and the
    ! run uses the loosest one that meets it; while the live points are far apart the sampler
    ! allows more, down to this at the end. 0.01 is far below the evidence uncertainty of any run.
    REAL(8) :: opt_gpu_tolerance = 1d-2
    ! `search.gpu_threads`: how many of the OpenMP threads evaluate on the device; the others
    ! use the CPU, so both are busy. -1 (default): decided by the startup self-test, which keeps
    ! at most 8 threads, all on the GPU, when it uses the GPU (GPU_SELF_TEST).
    INTEGER :: opt_gpu_threads = -1
    ! `search.gpu_precision`: 'auto' (default: 'mixed' when the model marks its terms with
    ! NF_TERM, 'double' otherwise), 'double' (the CPU's closed forms, the CPU's bits) or 'mixed'
    ! (per-term: NF_TERM terms in float, the large ones in double; 7-10x the double rate on
    ! consumer and workstation GPUs). Env NESTED_FIT_GPU_PRECISION overrides.
    CHARACTER(LEN=16) :: opt_gpu_precision = 'auto'
    
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
