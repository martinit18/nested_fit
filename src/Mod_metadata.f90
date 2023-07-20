MODULE MOD_METADATA
  ! Module for program runtime metadata

  IMPLICIT NONE
  CHARACTER(LEN=5), PARAMETER :: version_full = '4.5.2'
  CHARACTER(LEN=3), PARAMETER :: version = '4.5'
  CHARACTER(LEN=18), PARAMETER :: nf_child_proc_name = 'nf_child_proc4.5.2'

  LOGICAL, PARAMETER :: parallel_on = .FALSE.
  LOGICAL, PARAMETER :: parallel_mpi_on = .FALSE.
  LOGICAL, PARAMETER :: static_seed = .FALSE.

#ifdef FUNC_TARGET
  CHARACTER(LEN=20), PARAMETER :: exec_target_name = 'nested_fit_func4.5.2'
#else
  CHARACTER(LEN=15), PARAMETER :: exec_target_name = 'nested_fit4.5.2'
#endif


END MODULE MOD_METADATA
