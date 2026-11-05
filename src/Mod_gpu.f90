! GPU likelihood backend: Fortran side. Depends only on iso_c_binding, so Mod_likelihood
! can USE it without a cycle. The data and the user function are handed in explicitly.
MODULE MOD_GPU
  USE, INTRINSIC :: iso_c_binding
  IMPLICIT NONE
  LOGICAL :: gpu_on = .FALSE.
  CHARACTER(LEN=16) :: gpu_pref = 'auto'   ! the device preference in force: NESTED_FIT_DEVICE, else search.device

  INTERFACE
    SUBROUTINE nf_gpu_set_preference(device) BIND(c, name='nf_gpu_set_preference')
      IMPORT :: c_char
      CHARACTER(KIND=c_char), DIMENSION(*), INTENT(IN) :: device
    END SUBROUTINE
    FUNCTION nf_gpu_available() BIND(c, name='nf_gpu_available')
      IMPORT :: c_int
      INTEGER(c_int) :: nf_gpu_available
    END FUNCTION
    FUNCTION nf_gpu_device_name() BIND(c, name='nf_gpu_device_name')
      IMPORT :: c_ptr
      TYPE(c_ptr) :: nf_gpu_device_name
    END FUNCTION
    FUNCTION nf_gpu_last_error() BIND(c, name='nf_gpu_last_error')
      IMPORT :: c_ptr
      TYPE(c_ptr) :: nf_gpu_last_error
    END FUNCTION
    FUNCTION nf_gpu_init(x, nc, nc_err, ndata, like_kind, nthreads) BIND(c, name='nf_gpu_init')
      IMPORT :: c_int, c_double
      REAL(c_double), DIMENSION(*), INTENT(IN) :: x, nc, nc_err
      INTEGER(c_int), VALUE :: ndata, like_kind, nthreads
      INTEGER(c_int) :: nf_gpu_init
    END FUNCTION
    FUNCTION nf_gpu_set_function(c_body, param_ids, npar) BIND(c, name='nf_gpu_set_function')
      IMPORT :: c_int, c_char
      CHARACTER(KIND=c_char), DIMENSION(*), INTENT(IN) :: c_body, param_ids
      INTEGER(c_int), VALUE :: npar
      INTEGER(c_int) :: nf_gpu_set_function
    END FUNCTION
    FUNCTION nf_gpu_loglike(par, npar, thread_id) BIND(c, name='nf_gpu_loglike')
      IMPORT :: c_int, c_double
      REAL(c_double), DIMENSION(*), INTENT(IN) :: par
      INTEGER(c_int), VALUE :: npar, thread_id
      REAL(c_double) :: nf_gpu_loglike
    END FUNCTION
    SUBROUTINE nf_gpu_free() BIND(c, name='nf_gpu_free')
    END SUBROUTINE
  END INTERFACE

CONTAINS

  FUNCTION C_STR(p) RESULT(s)
    TYPE(c_ptr), INTENT(IN) :: p
    CHARACTER(LEN=:), ALLOCATABLE :: s
    CHARACTER(KIND=c_char), POINTER :: a(:)
    INTEGER :: n, i
    IF (.NOT.C_ASSOCIATED(p)) THEN
      s = ''
      RETURN
    END IF
    CALL C_F_POINTER(p, a, [4096])
    n = 0
    DO WHILE (n < 4096)
      IF (a(n+1) == c_null_char) EXIT
      n = n + 1
    END DO
    ALLOCATE(CHARACTER(LEN=n) :: s)
    DO i = 1, n
      s(i:i) = a(i)
    END DO
  END FUNCTION

  ! Decide, upload, compile. Returns a one-line reason in `why` when the GPU is not used.
  SUBROUTINE GPU_SETUP(x, nc, nc_err, ndata, like_kind, nthreads, c_body, param_ids, npar, why, device_pref, min_points_pref)
    REAL(8), DIMENSION(*), INTENT(IN) :: x, nc, nc_err
    INTEGER, INTENT(IN) :: ndata, like_kind, nthreads, npar, min_points_pref
    CHARACTER(LEN=*), INTENT(IN) :: c_body, param_ids, device_pref
    CHARACTER(LEN=*), INTENT(OUT) :: why
    CHARACTER(LEN=32) :: env
    INTEGER :: minpts, st
    gpu_on = .FALSE.
    why = ''
    CALL GET_ENVIRONMENT_VARIABLE('NESTED_FIT_DEVICE', env, STATUS=st)
    IF (st == 0 .AND. TRIM(env) == 'cpu') THEN
      why = 'NESTED_FIT_DEVICE=cpu'
      RETURN
    END IF
    IF (st /= 0 .AND. TRIM(device_pref) == 'cpu') THEN
      why = 'search.device: cpu'
      RETURN
    END IF
    gpu_pref = device_pref
    IF (st == 0 .AND. LEN_TRIM(env) > 0) gpu_pref = env
    CALL nf_gpu_set_preference(TRIM(device_pref)//c_null_char)
    IF (nf_gpu_available() == 0) THEN
      why = C_STR(nf_gpu_last_error())
      RETURN
    END IF
    IF (LEN_TRIM(c_body) == 0) THEN
      why = 'user function not expressible on the device (VOIGT/Interpolate/nested user functions)'
      RETURN
    END IF
    minpts = min_points_pref
    CALL GET_ENVIRONMENT_VARIABLE('NESTED_FIT_GPU_MIN_POINTS', env, STATUS=st)
    IF (st == 0) READ(env, *, IOSTAT=st) minpts
    IF (ndata < minpts) THEN
      WRITE(why, '(a,i0,a,i0,a)') 'ndata=', ndata, ' below NESTED_FIT_GPU_MIN_POINTS=', minpts, ''
      RETURN
    END IF
    IF (nf_gpu_init(x, nc, nc_err, ndata, like_kind, nthreads) /= 0) THEN
      why = C_STR(nf_gpu_last_error())
      RETURN
    END IF
    IF (nf_gpu_set_function(TRIM(c_body)//c_null_char, TRIM(param_ids)//c_null_char, npar) /= 0) THEN
      why = C_STR(nf_gpu_last_error())
      CALL nf_gpu_free()
      RETURN
    END IF
    gpu_on = .TRUE.
  END SUBROUTINE

  SUBROUTINE READ_TEXT_FILE(path, text, err)
    ! Whole file into a string (a device source given by `function.gpu_source`).
    CHARACTER(LEN=*), INTENT(IN) :: path
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: text
    CHARACTER(LEN=*), INTENT(OUT) :: err
    INTEGER :: u, sz, ios
    err = ''
    OPEN(NEWUNIT=u, FILE=path, ACCESS='stream', FORM='unformatted', STATUS='old', ACTION='read', IOSTAT=ios)
    IF (ios /= 0) THEN
      err = 'cannot open gpu_source '//path
      RETURN
    END IF
    INQUIRE(UNIT=u, SIZE=sz)
    ALLOCATE(CHARACTER(LEN=sz) :: text)
    READ(u, IOSTAT=ios) text
    CLOSE(u)
    IF (ios /= 0) THEN
      err = 'cannot read gpu_source '//path
      DEALLOCATE(text)
    END IF
  END SUBROUTINE

END MODULE MOD_GPU
