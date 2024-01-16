! Brief  : This module contains the performance profiler module. It checks for timings wherever needed.
! Author : César Godinho
! Date   : 08/01/2024

MODULE MOD_PERFPROF
    USE MOD_LOGGER
    USE MOD_AUTOFUNC ! TODO: (César) Using this for the F -> C subroutines but they should really move out of there
    USE, INTRINSIC :: iso_c_binding
    IMPLICIT NONE

    PUBLIC :: SHUTDOWN_PERF_PROF, ScopedPerfTimer
    PRIVATE

    ! NOTE: Using same naming scheme as in stperf (see https://github.com/lPrimemaster/stperf/blob/master/stperf.h)
    !       Without the underscore, since fortran does not allow it (at least gfortran does not)
    TYPE, BIND(c) :: stperf_PerfNodeThreadList_t
        TYPE(c_ptr)        :: elements
        INTEGER(c_int64_t) :: size
    END TYPE stperf_PerfNodeThreadList_t

    TYPE, BIND(c) :: stperf_PerfNodeList_t
        TYPE(c_ptr)        :: elements
        INTEGER(c_int64_t) :: size
        INTEGER(c_int64_t) :: thread_id
    END TYPE stperf_PerfNodeList_t

    TYPE, BIND(c) :: stperf_PerfNode_t
        INTEGER(c_int)                    :: granularity
        REAL(c_float)                     :: value
        REAL(c_float)                     :: pct
        INTEGER(c_int64_t)                :: nanos
        CHARACTER(c_char), DIMENSION(128) :: name
        INTEGER(c_int)                    :: indent
        INTEGER(c_int64_t)                :: hits
        TYPE(stperf_PerfNodeList_t)       :: children
    END TYPE stperf_PerfNode_t

#ifdef PPROF
    INTERFACE
        FUNCTION stperf_StartProf(name, line, suffix) RESULT(handle) BIND(c, name='stperf_StartProf')
            USE, INTRINSIC :: iso_c_binding
            IMPLICIT NONE
            CHARACTER(c_char), INTENT(IN), DIMENSION(*) :: name
            INTEGER(c_int), INTENT(IN), VALUE           :: line
            CHARACTER(c_char), INTENT(IN), DIMENSION(*) :: suffix
            INTEGER(c_int64_t)                          :: handle
        END FUNCTION

        SUBROUTINE stperf_StopProf(handle) BIND(c, name='stperf_StopProf')
            USE, INTRINSIC :: iso_c_binding
            IMPLICIT NONE
            INTEGER(c_int64_t), INTENT(IN), VALUE :: handle
        END SUBROUTINE

        SUBROUTINE stperf_StopCounters() BIND(c, name='stperf_StopCounters')
            USE, INTRINSIC :: iso_c_binding
            IMPLICIT NONE
        END SUBROUTINE
        
        FUNCTION stperf_GetCallTree() RESULT(output) BIND(c, name='stperf_GetCallTree')
            USE, INTRINSIC :: iso_c_binding
            IMPORT stperf_PerfNodeThreadList_t
            IMPLICIT NONE
            TYPE(stperf_PerfNodeThreadList_t) :: output
        END FUNCTION

        FUNCTION stperf_GetCallTreeDot() RESULT(output) BIND(c, name='stperf_GetCallTreeDot')
            USE, INTRINSIC :: iso_c_binding
            IMPLICIT NONE
            TYPE(c_ptr) :: output
        END FUNCTION

        FUNCTION stperf_GetThreadRoot(tree, tid) RESULT(output) BIND(c, name='stperf_GetThreadRoot')
            USE, INTRINSIC :: iso_c_binding
            IMPORT stperf_PerfNodeThreadList_t
            IMPLICIT NONE
            TYPE(stperf_PerfNodeThreadList_t), INTENT(IN) :: tree ! NOTE: (César) Fortran pass by ref - so this works
            INTEGER(c_int64_t), INTENT(IN), VALUE         :: tid
            TYPE(c_ptr)                                   :: output
        END FUNCTION

        FUNCTION stperf_GetCallTreeString(tree) RESULT(output) BIND(c, name='stperf_GetCallTreeString')
            USE, INTRINSIC :: iso_c_binding
            IMPORT stperf_PerfNodeThreadList_t
            IMPLICIT NONE
            TYPE(stperf_PerfNodeThreadList_t), INTENT(IN), VALUE :: tree
            TYPE(c_ptr)                                          :: output
        END FUNCTION

        SUBROUTINE stperf_FreeCallTreeString(string) BIND(c, name='stperf_FreeCallTreeString')
            USE, INTRINSIC :: iso_c_binding
            IMPLICIT NONE
            TYPE(c_ptr), INTENT(IN), VALUE :: string
        END SUBROUTINE

        SUBROUTINE stperf_FreeCallTree(tree) BIND(c, name='stperf_FreeCallTree')
            USE, INTRINSIC :: iso_c_binding
            IMPORT stperf_PerfNodeThreadList_t
            IMPLICIT NONE
            TYPE(stperf_PerfNodeThreadList_t), INTENT(IN), VALUE :: tree
        END SUBROUTINE

        SUBROUTINE stperf_ResetCounters() BIND(c, name='stperf_ResetCounters')
            USE, INTRINSIC :: iso_c_binding
            IMPLICIT NONE
        END SUBROUTINE

        FUNCTION stperf_GetCurrentThreadId() RESULT(output) BIND(c, name='stperf_GetCurrentThreadId')
            USE, INTRINSIC :: iso_c_binding
            IMPLICIT NONE
            INTEGER(c_int64_t) :: output
        END FUNCTION
    END INTERFACE
#endif

    TYPE :: ScopedPerfTimer
        PRIVATE
        INTEGER(c_int64_t) :: handle
        
        CONTAINS
        PROCEDURE, PUBLIC :: init => constructor
        FINAL :: destroy
    END TYPE
            
    CONTAINS

    SUBROUTINE F_C_STRING_ALLOC(f_string, c_string)
        USE iso_c_binding
        CHARACTER(c_char), DIMENSION(:), POINTER, INTENT(OUT) :: c_string
        CHARACTER(LEN=*), INTENT(IN)                          :: f_string

        INTEGER :: len

        len = LEN_TRIM(f_string)
        ALLOCATE(c_string(len + 1))
        c_string = TRANSFER(TRIM(f_string), c_string)
        c_string(len + 1) = c_null_char
    END SUBROUTINE

    SUBROUTINE F_C_STRING_DEALLOC(c_string)
        USE iso_c_binding
        CHARACTER(c_char), DIMENSION(:), POINTER, INTENT(INOUT) :: c_string

        DEALLOCATE(c_string)
    END SUBROUTINE

    SUBROUTINE constructor(this, name, line)
        USE, INTRINSIC :: iso_c_binding
        CHARACTER(LEN=*), INTENT(IN) :: name
        INTEGER, INTENT(IN)          :: line
        CLASS(ScopedPerfTimer)       :: this

        CHARACTER(c_char), DIMENSION(:), POINTER :: c_name
        INTEGER(c_int64_t)                       :: handle
#ifdef PPROF 
        ! NOTE: (César) Not very efficient memory-wise
        CALL F_C_STRING_ALLOC(name, c_name)
        this%handle = stperf_StartProf(c_name(1), line, c_null_char)
        CALL F_C_STRING_DEALLOC(c_name)
#endif
    END SUBROUTINE

    SUBROUTINE destroy(this)
        TYPE(ScopedPerfTimer), INTENT(INOUT) :: this
#ifdef PPROF
        CALL stperf_StopProf(this%handle)
#endif
    END SUBROUTINE

    SUBROUTINE SHUTDOWN_PERF_PROF()
        TYPE(stperf_PerfNodeThreadList_t) :: nodes
        TYPE(c_ptr) :: c_report, c_dotfile
        INTEGER(c_size_t) :: c_report_size, c_dotfile_size
        CHARACTER(LEN=:), ALLOCATABLE :: f_report, f_dotfile

#ifdef PPROF
        CALL LOG_TRACE('Finalizing performance profiling.')
        CALL stperf_StopCounters()
        nodes = stperf_GetCallTree()
        c_report = stperf_GetCallTreeString(nodes)
        c_dotfile = stperf_GetCallTreeDot()

        CALL C_STRING_SIZE(c_report, c_report_size)
        CALL C_STRING_SIZE(c_dotfile, c_dotfile_size)

        ALLOCATE(CHARACTER(LEN=c_report_size)  :: f_report)
        ALLOCATE(CHARACTER(LEN=c_dotfile_size) :: f_dotfile)
        
        CALL C_F_STRING(c_report, f_report)
        CALL C_F_STRING(c_dotfile, f_dotfile)

        CALL LOG_TRACE('PPROF dump:'//NEW_LINE('N')//TRIM(f_report))

        CALL LOG_MESSAGE('Writing nf_pprof.dot...')
        OPEN(7, FILE='nf_pprof.dot', STATUS='unknown')
        WRITE(7,'(a)') f_dotfile
        CLOSE(7)

        DEALLOCATE(f_report, f_dotfile)

        CALL stperf_FreeCallTreeString(c_report)
        CALL stperf_FreeCallTree(nodes)
#endif
    END SUBROUTINE
END MODULE MOD_PERFPROF
