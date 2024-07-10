! Brief  : This module contains the performance profiler module. It checks for timings wherever needed.
! Author : César Godinho
! Date   : 09/07/2024

MODULE MOD_PERFPROF
    USE MOD_LOGGER
    USE MOD_AUTOFUNC ! TODO: (César) Using this for the F -> C subroutines but they should really move out of there
    USE MOD_STRUTIL
    USE, INTRINSIC :: iso_c_binding
    IMPLICIT NONE

    PUBLIC :: ScopedPerfTimer
    PRIVATE

    TYPE, BIND(C) :: TracyCtxF ! Equivalent to `TracyCZoneCtx`
        INTEGER(c_int64_t) :: id
        INTEGER(c_int)     :: active
    END TYPE
    
    TYPE :: ScopedPerfTimer
        PRIVATE
        TYPE(TracyCtxF) :: handle
#ifdef PPROF
        CONTAINS
        PROCEDURE, PUBLIC :: init => constructor
        FINAL :: destroy
#endif
    END TYPE

#ifdef PPROF
    INTERFACE
        FUNCTION TRACY_ALLOC_SRCLOC(line, source, sourceSz, function, functionSz) BIND(C, NAME='___tracy_alloc_srcloc')
            IMPORT :: c_int32_t, c_int64_t, c_ptr, c_long, c_char
            INTEGER(c_int32_t), VALUE :: line
            CHARACTER(c_char), INTENT(IN), DIMENSION(*) :: source
            CHARACTER(c_char), INTENT(IN), DIMENSION(*) :: function
            INTEGER(c_long)   , VALUE :: sourceSz
            INTEGER(c_long)   , VALUE :: functionSz
            INTEGER(c_int64_t)        :: TRACEZONE_SRCLOC
        END FUNCTION

        FUNCTION TRACY_EMIT_ZONE_BEGIN_ALLOC(srcloc, active) BIND(C, NAME='___tracy_emit_zone_begin_alloc')
            IMPORT :: c_int64_t, c_int, TracyCtxF
            INTEGER(c_int64_t), VALUE :: srcloc
            INTEGER(c_int)    , VALUE :: active
            TYPE(TracyCtxF)           :: TRACY_EMIT_ZONE_BEGIN_ALLOC
        END FUNCTION

        SUBROUTINE TRACY_EMIT_ZONE_END(ctx) bind(C, NAME='___tracy_emit_zone_end')
            IMPORT :: TracyCtxF
            TYPE(TracyCtxF), VALUE :: ctx
        END SUBROUTINE
    END INTERFACE

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

    SUBROUTINE constructor(this, name, line, file)
        USE, INTRINSIC :: iso_c_binding
        CHARACTER(LEN=*), INTENT(IN) :: name
        CHARACTER(LEN=*), INTENT(IN) :: file
        INTEGER(4)      , INTENT(IN) :: line
        CLASS(ScopedPerfTimer)       :: this

        ! CHARACTER(c_char), DIMENSION(:), POINTER :: c_name

        TYPE(TracyCtxF)    :: ctx
        INTEGER(c_int64_t) :: loc
        INTEGER(c_long)    :: funcsz
        INTEGER(c_long)    :: sourcesz

        funcsz = LEN(name)
        sourcesz = LEN(file)

        loc = TRACY_ALLOC_SRCLOC(line, file//CHAR(0), sourcesz, name//CHAR(0), funcsz)
        this%handle = TRACY_EMIT_ZONE_BEGIN_ALLOC(loc, 1)
    END SUBROUTINE

    SUBROUTINE destroy(this)
        TYPE(ScopedPerfTimer), INTENT(INOUT) :: this

        CALL TRACY_EMIT_ZONE_END(this%handle)
    END SUBROUTINE
#endif
END MODULE MOD_PERFPROF
