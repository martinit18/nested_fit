! GPU likelihood backend: Fortran side. Depends only on iso_c_binding, so Mod_likelihood
! can USE it without a cycle. The data and the user function are handed in explicitly.
MODULE MOD_GPU
  USE, INTRINSIC :: iso_c_binding
  USE MOD_LOGGER, ONLY: LOG_MESSAGE
  IMPLICIT NONE
  LOGICAL :: gpu_on = .FALSE.
  INTEGER :: gpu_threads = -1   ! threads 0..gpu_threads-1 evaluate on the device, the rest on the CPU; -1 = all
  LOGICAL :: gpu_mixed = .FALSE.    ! per-term precision: NF_TERM terms in single, the large ones in double
  LOGICAL :: gpu_precision_auto = .FALSE.   ! the precision was chosen from the model (search.gpu_precision: auto)
  LOGICAL :: gpu_threads_auto = .FALSE.     ! the thread split is left to the self-test (search.gpu_threads: -1)
  CHARACTER(LEN=16) :: gpu_pref = 'auto'   ! the device preference in force: NESTED_FIT_DEVICE, else search.device
  LOGICAL :: gpu_per_point = .FALSE.   ! mixed precision without NF_TERM markers: the rule applies to the whole model at each point
  INTEGER :: gpu_nth_before = 1        ! the thread count before the self-test lowered it, for CPU work after the run
  ! Mixed precision as a tolerance. The kernel's rule has one threshold, in units of the noise
  ! of the data point: a term (or the model, per point) above it is recomputed in double. The
  ! self-test measures the error in log L at the thresholds of this ladder, all single to all
  ! double, and GPU_THRESHOLD_FOR picks the loosest that meets an allowed error. The run asks
  ! for the user's tolerance at the end and more while the live points are far apart.
  REAL(8) :: gpu_tolerance = 1d-2            ! search.gpu_tolerance: the error allowed on log L at the end of the run
  REAL(8), PARAMETER :: gpu_schedule_fraction = 1d-2   ! allowed at a step: this fraction of the mean log L gap between live points
  INTEGER, PARAMETER :: gpu_nladder = 9
  REAL(8), DIMENSION(gpu_nladder) :: gpu_ladder = [1d30, 1d4, 1d3, 1d2, 1d1, 1d0, 1d-1, 1d-2, 0d0]
  REAL(8), DIMENSION(gpu_nladder) :: gpu_ladder_err = HUGE(1d0)   ! filled by the self-test
  REAL(8) :: gpu_threshold_now = 0.1d0

  INTERFACE
    SUBROUTINE nf_gpu_set_preference(device) BIND(c, name='nf_gpu_set_preference')
      IMPORT :: c_char
      CHARACTER(KIND=c_char), DIMENSION(*), INTENT(IN) :: device
    END SUBROUTINE
    SUBROUTINE nf_gpu_set_precision(precision) BIND(c, name='nf_gpu_set_precision')
      IMPORT :: c_char
      CHARACTER(KIND=c_char), DIMENSION(*), INTENT(IN) :: precision
    END SUBROUTINE
    SUBROUTINE nf_gpu_set_tolerance(threshold) BIND(c, name='nf_gpu_set_tolerance')
      IMPORT :: c_double
      REAL(c_double), VALUE :: threshold
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
    FUNCTION nf_gpu_loglike_at(par, npar, thread_id, threshold) BIND(c, name='nf_gpu_loglike_at')
      IMPORT :: c_int, c_double
      REAL(c_double), DIMENSION(*), INTENT(IN) :: par
      INTEGER(c_int), VALUE :: npar, thread_id
      REAL(c_double), VALUE :: threshold
      REAL(c_double) :: nf_gpu_loglike_at
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

  ! Is there a device to prepare a source for? The checks GPU_SETUP makes first, so that nothing is translated or
  ! read for a machine without a GPU or a run that asked for the CPU.
  FUNCTION GPU_DEVICE_PRESENT(device_pref, why) RESULT(present)
    CHARACTER(LEN=*), INTENT(IN) :: device_pref
    CHARACTER(LEN=*), INTENT(OUT) :: why
    LOGICAL :: present
    CHARACTER(LEN=32) :: env
    INTEGER :: st
    present = .FALSE.
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
    CALL nf_gpu_set_preference(TRIM(device_pref)//c_null_char)
    IF (nf_gpu_available() == 0) THEN
      why = C_STR(nf_gpu_last_error())
      RETURN
    END IF
    present = .TRUE.
  END FUNCTION

  ! Decide, upload, compile. Returns a one-line reason in `why` when the GPU is not used.
  SUBROUTINE GPU_SETUP(x, nc, nc_err, ndata, like_kind, nthreads, c_body, param_ids, npar, why, device_pref, min_points_pref, threads_pref, precision_pref, tolerance_pref)
    REAL(8), DIMENSION(*), INTENT(IN) :: x, nc, nc_err
    INTEGER, INTENT(IN) :: ndata, like_kind, nthreads, npar, min_points_pref, threads_pref
    REAL(8), INTENT(IN) :: tolerance_pref
    CHARACTER(LEN=*), INTENT(IN) :: c_body, param_ids, device_pref, precision_pref
    CHARACTER(LEN=16) :: prec
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
    prec = precision_pref
    CALL GET_ENVIRONMENT_VARIABLE('NESTED_FIT_GPU_PRECISION', env, STATUS=st)
    IF (st == 0 .AND. LEN_TRIM(env) > 0) prec = env
    gpu_precision_auto = (prec(1:1) == 'a' .OR. prec(1:1) == 'A')
    ! auto: mixed, per term when the model marks its additive terms with NF_TERM, per point otherwise
    IF (gpu_precision_auto) prec = 'mixed'
    gpu_mixed = (prec(1:1) == 'm' .OR. prec(1:1) == 'M')
    gpu_per_point = gpu_mixed .AND. INDEX(c_body, 'NF_TERM') == 0
    CALL nf_gpu_set_precision(TRIM(prec)//c_null_char)
    gpu_tolerance = tolerance_pref
    CALL GET_ENVIRONMENT_VARIABLE('NESTED_FIT_GPU_TOLERANCE', env, STATUS=st)
    IF (st == 0 .AND. LEN_TRIM(env) > 0) READ(env, *, IOSTAT=st) gpu_tolerance
    gpu_threshold_now = 0.1d0
    CALL nf_gpu_set_tolerance(gpu_threshold_now)
    IF (nf_gpu_set_function(TRIM(c_body)//c_null_char, TRIM(param_ids)//c_null_char, npar) /= 0) THEN
      why = C_STR(nf_gpu_last_error())
      CALL nf_gpu_free()
      RETURN
    END IF
    gpu_threads = threads_pref
    CALL GET_ENVIRONMENT_VARIABLE('NESTED_FIT_GPU_THREADS', env, STATUS=st)
    IF (st == 0) READ(env, *, IOSTAT=st) gpu_threads
    gpu_threads_auto = (gpu_threads < 0)
    IF (gpu_threads == 0) THEN
      why = 'gpu_threads = 0'
      CALL nf_gpu_free()
      RETURN
    END IF
    gpu_on = .TRUE.
  END SUBROUTINE

  ! The loosest threshold of the ladder whose measured error meets eps; 0 (all double) when none does
  FUNCTION GPU_THRESHOLD_FOR(eps) RESULT(thr)
    REAL(8), INTENT(IN) :: eps
    REAL(8) :: thr
    INTEGER :: j
    thr = 0d0
    DO j = 1, gpu_nladder
       IF (gpu_ladder_err(j) <= eps) THEN
          thr = gpu_ladder(j)
          RETURN
       END IF
    END DO
  END FUNCTION

  ! The error ladder at one point of the parameter space, on the device: the value at each
  ! threshold against the all-double one (the CPU's to 1e-9). The error of single precision
  ! scales with the residuals, so it is far larger at the prior-box points of the self-test
  ! than where the live points are: measured again at the best live point as the run goes.
  SUBROUTINE GPU_MEASURE_LADDER(p, np)
    !$ USE omp_lib
    INTEGER, INTENT(IN) :: np
    REAL(8), DIMENSION(np), INTENT(IN) :: p
    REAL(8) :: ref
    INTEGER :: j, tid
    tid = 0
    !$ tid = omp_get_thread_num()
    ref = nf_gpu_loglike_at(p, np, tid, 0d0)
    DO j = 1, gpu_nladder - 1
       gpu_ladder_err(j) = ABS(nf_gpu_loglike_at(p, np, tid, gpu_ladder(j)) - ref)
    END DO
    gpu_ladder_err(gpu_nladder) = 0d0
    DO j = gpu_nladder - 1, 1, -1   ! monotone in the threshold
       gpu_ladder_err(j) = MAX(gpu_ladder_err(j), gpu_ladder_err(j+1))
    END DO
  END SUBROUTINE

  ! Called by the sampler when it recomputes its search data, with the best live point and
  ! gap = the mean distance in log L between neighbouring live points. Early in a run the gap
  ! is large and the GPU runs in single precision; it shrinks as the points converge, down to
  ! the user's tolerance. Ten launches at one point, a few milliseconds.
  SUBROUTINE GPU_SCHEDULE(gap, p, np)
    REAL(8), INTENT(IN) :: gap
    INTEGER, INTENT(IN) :: np
    REAL(8), DIMENSION(np), INTENT(IN) :: p
    REAL(8) :: thr, eps
    INTEGER, SAVE :: nlogged = 0
    CHARACTER(256) :: msg
    IF (.NOT.gpu_on .OR. .NOT.gpu_mixed) RETURN
    CALL GPU_MEASURE_LADDER(p, np)
    eps = MAX(gpu_tolerance, gpu_schedule_fraction*gap)
    thr = GPU_THRESHOLD_FOR(eps)
    IF (thr /= gpu_threshold_now) THEN
       gpu_threshold_now = thr
       CALL nf_gpu_set_tolerance(thr)
       IF (nlogged < 30) THEN   ! the threshold moves a few times per run; never flood the log
          nlogged = nlogged + 1
          WRITE(msg, '(a,es8.1,a,es8.1,a,es8.1,a,es8.1,a)') 'GPU: precision threshold ', thr, ' (allowed error ', eps, &
               ' in log L for a live-point gap of ', gap, '; all single would differ by ', gpu_ladder_err(1), ' at the best live point)'
          CALL LOG_MESSAGE(TRIM(msg))
       END IF
    END IF
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
