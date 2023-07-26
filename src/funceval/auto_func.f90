! Brief  : Module for automatically managing/loading RT compiled user functions.
! Author : César Godinho
! Date   : 23/07/2023

! Linearly sorting the cache in this file should be ok since we are
! using a relatively low ammount of comparisons at the start to check
! for the call of user functions.
MODULE autofunc
    USE MOD_METADATA
    IMPLICIT NONE
    
    PUBLIC :: COMPILE_CACHE_FUNC, LOAD_DLL_PROC, FREE_DLL, INIT_AUTOFUNC
    PRIVATE

    TYPE cache_entry_t
        CHARACTER(LEN=64) :: date_modified
        CHARACTER(LEN=64) :: name
    END TYPE cache_entry_t

    CHARACTER(LEN=*), PARAMETER      :: dll_name    = TRIM(nf_cache_folder)//'dynamic_calls.so'
    CHARACTER(LEN=*), PARAMETER      :: fname_cache = TRIM(nf_cache_folder)//'func_names.dat'
    TYPE(cache_entry_t), ALLOCATABLE :: entries(:)
    INTEGER                          :: nentries=0

    INTERFACE
        FUNCTION dlopen(filename, mode) BIND(c, name='dlopen')
            USE iso_c_binding
            IMPLICIT NONE
            TYPE(c_ptr) :: dlopen
            CHARACTER(c_char), INTENT(IN) :: filename(*)
            INTEGER(c_int), VALUE :: mode
        END FUNCTION

        FUNCTION dlsym(handle,name) BIND(c,name="dlsym")
            USE iso_c_binding
            IMPLICIT NONE
            TYPE(c_funptr) :: dlsym
            TYPE(c_ptr), VALUE :: handle
            CHARACTER(c_char), INTENT(IN) :: name(*)
         END FUNCTION
   
         FUNCTION dlclose(handle) BIND(c,name="dlclose")
            USE iso_c_binding
            IMPLICIT NONE
            INTEGER(c_int) :: dlclose
            TYPE(c_ptr), VALUE :: handle
         END FUNCTION
    END INTERFACE

    CONTAINS

    SUBROUTINE ADD_ENTRY(arg)
        TYPE(cache_entry_t), INTENT(IN) :: arg
        TYPE(cache_entry_t), ALLOCATABLE :: tmp(:)        

        IF(nentries.EQ.0) THEN
            ALLOCATE(entries(2))
        ENDIF

        IF(SIZE(entries).EQ.nentries) THEN
            CALL MOVE_ALLOC(entries, tmp)
            ALLOCATE(entries(nentries*2))
            entries(1:nentries) = tmp
        ENDIF

        nentries = nentries + 1
        entries(nentries) = arg
    END SUBROUTINE
    
    SUBROUTINE READ_CACHE()
        CHARACTER(LEN=512) :: line
        CHARACTER(LEN=64) :: date, fname
        LOGICAL :: exist
        INTEGER :: i
        
        INQUIRE(FILE=TRIM(fname_cache), EXIST=exist)
        IF (exist) THEN
            OPEN(77, FILE=TRIM(fname_cache), STATUS='old', ACTION='read')
            DO
                READ(77,('(A)'), END=10) line

                i = INDEX(line, '-')
                fname = TRIM(line(1:i-1))
                date  = TRIM(line(i+1:LEN_TRIM(line)))

                CALL ADD_ENTRY(cache_entry_t(date, TRIM(fname)))
            END DO
10          CLOSE(77)
        ENDIF
    END SUBROUTINE

    SUBROUTINE UPDATE_CACHE(fname)
        CHARACTER(LEN=*), INTENT(IN) :: fname
        CHARACTER(LEN=64)            :: date
        INTEGER                      :: i

        CALL FDATE(date)
        DO i = 1, nentries
            IF(fname.EQ.TRIM(entries(i)%name)) THEN
                entries(i)%date_modified = date
                RETURN
            ENDIF
        END DO

        CALL ADD_ENTRY(cache_entry_t(date, fname))
    END SUBROUTINE

    SUBROUTINE WRITE_CACHE()
        INTEGER :: i
        
        OPEN(UNIT=77, FILE=TRIM(fname_cache), STATUS='UNKNOWN')
            DO i = 1, nentries
                WRITE(77,'(a, a, a)') TRIM(entries(i)%name), ' - ', entries(i)%date_modified
            END DO
        CLOSE(77)

        DEALLOCATE(entries)

    END SUBROUTINE

    FUNCTION CHECK_FUNC_CACHE(fname)
        CHARACTER(LEN=64), INTENT(IN) :: fname
        LOGICAL                       :: check_func_cache
        INTEGER                       :: i

        DO i = 1, nentries
            IF(fname.EQ.entries(i)%name) THEN
                check_func_cache = .TRUE.
                RETURN
            ENDIF
        END DO

        check_func_cache = .FALSE.
        RETURN
    END FUNCTION

    SUBROUTINE COMPILE_CACHE_FUNC(funcname, expression)
        CHARACTER(LEN=*), INTENT(IN)  :: funcname
        CHARACTER(LEN=*), INTENT(IN)  :: expression ! Must be an F90 compliant expression
        CHARACTER(LEN=128)            :: filename
        INTEGER                       :: status
        
        WRITE(filename, '(a,a)') TRIM(nf_cache_folder), 'last_compile.f90'
        OPEN(UNIT=77, FILE=TRIM(filename), STATUS='UNKNOWN')
            WRITE(77,'(a)') 'function '//TRIM(funcname)//"(x, npar, params) bind(c, name='"//TRIM(funcname)//"_')"
            WRITE(77,'(a)') char(9)//'use, intrinsic :: iso_c_binding'
            WRITE(77,'(a)') char(9)//'implicit none'
            WRITE(77,'(a)') char(9)//'real(c_double), intent(in) :: x'
            WRITE(77,'(a)') char(9)//'integer(c_int), intent(in) :: npar'
            WRITE(77,'(a)') char(9)//'real(c_double), intent(in) :: params(npar)'
            WRITE(77,'(a)') char(9)//'real(c_double)             :: '//TRIM(funcname)
            WRITE(77,'(a)') char(9)
            ! WRITE(77,'(a)') char(9)//'real(c_double), external :: test_func'
            WRITE(77,'(a)') char(9)
            WRITE(77,'(a)') char(9)//TRIM(funcname)//' = '//expression
            WRITE(77,'(a)') 'end function '//TRIM(funcname)
        CLOSE(77)

        CALL EXECUTE_COMMAND_LINE('gfortran -c -shared -fPIC '//TRIM(filename)//' -o '//TRIM(nf_cache_folder)//TRIM(funcname)//'.o', EXITSTAT=status)
        IF(status.NE.0) THEN
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*) '       ERROR:           Failed to compile the function provided.'
            WRITE(*,*) '       ERROR:           Aborting Execution...'
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            STOP ! NOTE(César) : This works, before MPI init!!
        ENDIF
        CALL EXECUTE_COMMAND_LINE('gcc -shared -fPIC '//TRIM(nf_cache_folder)//'*.o -o '//TRIM(dll_name), EXITSTAT=status)
        IF(status.NE.0) THEN
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*) '       ERROR:           Failed to link the function provided.'
            WRITE(*,*) '       ERROR:           Aborting Execution...'
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            STOP ! NOTE(César) : This works, before MPI init!!
        ENDIF
        CALL UPDATE_CACHE(TRIM(funcname))
    END SUBROUTINE

    SUBROUTINE INIT_AUTOFUNC()
        CALL READ_CACHE()
    END SUBROUTINE

    SUBROUTINE LOAD_DLL_PROC(procname, procaddr, fileaddr)
        USE iso_c_binding
        CHARACTER(LEN=*), INTENT(IN)  :: procname
        TYPE(c_funptr),   INTENT(OUT) :: procaddr
        TYPE(c_ptr),      INTENT(OUT) :: fileaddr

        fileaddr = dlopen(TRIM(dll_name)//c_null_char, 1)
        IF(.NOT.c_associated(fileaddr)) THEN
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*) '       ERROR:           Could not load dynamic function dll.'
            WRITE(*,*) '       ERROR:           Aborting Execution...'
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            RETURN
        ENDIF

        procaddr = dlsym(fileaddr, TRIM(procname)//'_'//c_null_char)
        IF(.NOT.c_associated(procaddr)) THEN
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*) '       ERROR:           Could not load dynamic function ('//TRIM(procname)//')'
            WRITE(*,*) '       ERROR:           From dll ('//TRIM(dll_name)//').'
            WRITE(*,*) '       ERROR:           Aborting Execution...'
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            RETURN
        ENDIF

        RETURN
    END SUBROUTINE

    SUBROUTINE FREE_DLL(fileaddr)
        USE iso_c_binding
        TYPE(c_ptr), INTENT(IN) :: fileaddr
        INTEGER(c_int) :: status

        CALL WRITE_CACHE()
        
        status = dlclose(fileaddr)
        IF(status.NE.0) THEN
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*) '       ERROR:           Could not free dll.'
            WRITE(*,*) '       ERROR:           Aborting Execution...'
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            RETURN
        ENDIF

        RETURN
    END SUBROUTINE
END MODULE autofunc
