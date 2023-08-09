! Brief  : Module for automatically managing/loading RT compiled user functions.
!          Basically we accept multiple input types now:
!              - Old style (aka. functions inside the USERFCN*.f files, this should be deprecated)
!              - New style with LaTeX directly inside the function line in the <input>.dat file
!              - New style with f90 files with with a correct signature function inside the function line in the <input>.dat file
!
! Author : César Godinho
! Date   : 23/07/2023

! Linearly sorting the cache in this file should be ok since we are
! using a relatively low ammount of comparisons at the start to check
! for the call of user functions.
MODULE MOD_AUTOFUNC
    USE MOD_METADATA
    USE iso_c_binding
    IMPLICIT NONE
    
    PUBLIC :: COMPILE_CACHE_FUNC, &
        LOAD_DLL_PROC, &
        FREE_DLL, &
        INIT_AUTOFUNC, &
        CLEAN_AUTOFUNC, &
        ParseLatex_t, &
        proc_ptr_t, &
        PARSE_LATEX, &
        PARSE_LATEX_DEALLOC, &
        GET_USER_FUNC_PROCPTR, &
        GET_CACHE_SIZE, &
        GET_CACHE, &
        cache_entry_t
    PRIVATE

    TYPE, BIND(c) :: ParseOutput_t
        TYPE(c_ptr)    :: function_name
        TYPE(c_ptr)    :: parameter_names
        TYPE(c_ptr)    :: parameter_identifiers
        INTEGER(c_int) :: num_params

        TYPE(c_ptr)    :: functions
        INTEGER(c_int) :: num_funcs
        TYPE(c_ptr)    :: func_argc

        TYPE(c_ptr)    :: infixcode_f90
        INTEGER(c_int) :: error
    END TYPE ParseOutput_t

    TYPE :: ParseLatex_t
        CHARACTER(128)                           :: function_name
        CHARACTER(64), DIMENSION(:), ALLOCATABLE :: parameter_names
        CHARACTER(64), DIMENSION(:), ALLOCATABLE :: parameter_identifiers
        INTEGER                                  :: num_params
        
        CHARACTER(64), DIMENSION(:), ALLOCATABLE :: functions
        INTEGER                                  :: num_funcs
        INTEGER, POINTER, DIMENSION(:)           :: func_argc

        CHARACTER(512)                           :: infixcode_f90
        INTEGER                                  :: error
    END TYPE ParseLatex_t

    ! latex_parser.cpp interface
    INTERFACE
        FUNCTION ParseLatexToF90(expression) RESULT(output) BIND(c, name='ParseLatexToF90')
            USE, INTRINSIC :: iso_c_binding
            IMPORT :: ParseOutput_t
            IMPLICIT NONE
            CHARACTER(c_char), INTENT(IN), DIMENSION(*) :: expression
            TYPE(ParseOutput_t)                         :: output
        END FUNCTION

        SUBROUTINE FreeParseOutput(parsedata) BIND(c, name='FreeParseOutput')
            USE, INTRINSIC :: iso_c_binding
            IMPORT :: ParseOutput_t
            IMPLICIT NONE
            TYPE(ParseOutput_t), INTENT(IN) :: parsedata
        END SUBROUTINE

        SUBROUTINE GetErrorMsg(parsedata, message) BIND(c, name='GetErrorMsg')
            USE, INTRINSIC :: iso_c_binding
            IMPORT :: ParseOutput_t
            IMPLICIT NONE
            TYPE(ParseOutput_t), INTENT(IN)              :: parsedata
            CHARACTER(c_char), INTENT(OUT), DIMENSION(*) :: message
        END SUBROUTINE

        SUBROUTINE CheckParseValidity(parsedata, cache_path) BIND(c, name='CheckParseValidity')
            USE, INTRINSIC :: iso_c_binding
            IMPORT :: ParseOutput_t
            IMPLICIT NONE
            TYPE(ParseOutput_t), INTENT(INOUT)          :: parsedata
            CHARACTER(c_char), INTENT(IN), DIMENSION(*) :: cache_path
        END SUBROUTINE
    END INTERFACE

    ! Cache stuff
    TYPE cache_entry_t
        CHARACTER(LEN=64)  :: date_modified
        CHARACTER(LEN=64)  :: name
        CHARACTER(LEN=512) :: dec
        INTEGER            :: argc
    END TYPE cache_entry_t

    CHARACTER(LEN=*), PARAMETER      :: dll_name    = TRIM(nf_cache_folder)//'dynamic_calls.so'
    CHARACTER(LEN=*), PARAMETER      :: fname_cache = TRIM(nf_cache_folder)//'func_names.dat'
    TYPE(cache_entry_t), ALLOCATABLE :: entries(:)
    INTEGER                          :: nentries=0
    TYPE(c_ptr), ALLOCATABLE         :: loaded_addr(:)
    INTEGER                          :: nloaded_addr=0

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

    ABSTRACT INTERFACE
        FUNCTION proc_ptr_t(x, npar, params)
            USE, INTRINSIC :: iso_c_binding
            IMPLICIT NONE
            REAL(8), INTENT(IN) :: x
            INTEGER, INTENT(IN) :: npar
            REAL(8), INTENT(IN) :: params(npar)
            REAL(c_double)      :: proc_ptr_t
        END FUNCTION proc_ptr_t
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

    SUBROUTINE C_F_STRING(c_string, f_string)
        USE iso_c_binding
        TYPE(c_ptr), INTENT(IN)                              :: c_string
        CHARACTER(LEN=*), INTENT(OUT)                        :: f_string
        CHARACTER(256), POINTER                              :: f_ptr

        CALL C_F_POINTER(c_string, f_ptr)
        f_string = f_ptr(1:index(f_ptr, c_null_char)-1)
    END SUBROUTINE

    SUBROUTINE C_F_INTEGER_ARRAY(c_intarray, f_intarray, f_size)
        USE iso_c_binding
        TYPE(c_ptr), INTENT(IN)                     :: c_intarray
        INTEGER, DIMENSION(:), POINTER, INTENT(OUT) :: f_intarray
        INTEGER, INTENT(IN)                         :: f_size

        CALL C_F_POINTER(c_intarray, f_intarray, [f_size])
    END SUBROUTINE

    SUBROUTINE STRING_SPLIT(input, output, size)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN)  :: input
        CHARACTER(LEN=*), INTENT(OUT) :: output(size)
        INTEGER, INTENT(IN)           :: size
        INTEGER                       :: i

        READ(input,*) output(1:size)
    END SUBROUTINE   

    SUBROUTINE C_F_PARSESTRUCT(c_type, f_type)
        USE iso_c_binding
        TYPE(ParseOutput_t), INTENT(IN) :: c_type
        TYPE(ParseLatex_t), INTENT(OUT) :: f_type
        CHARACTER(4096)                 :: tmp_string
        INTEGER                         :: error

        ! Easy copying the 'simpler' datatypes
        f_type%num_params = c_type%num_params
        f_type%num_funcs  = c_type%num_funcs
        f_type%error      = c_type%error

        ! Function name
        CALL C_F_STRING(c_type%function_name, f_type%function_name)

        ! Infix code string
        CALL C_F_STRING(c_type%infixcode_f90, f_type%infixcode_f90)

        ! Number of function arguments
        CALL C_F_INTEGER_ARRAY(c_type%func_argc, f_type%func_argc, f_type%num_funcs)

        ! Parameter names
        CALL C_F_STRING(c_type%parameter_names, tmp_string)
        ALLOCATE(f_type%parameter_names(f_type%num_params))
        CALL STRING_SPLIT(tmp_string, f_type%parameter_names, f_type%num_params)

        ! Parameter identifiers
        CALL C_F_STRING(c_type%parameter_identifiers, tmp_string)
        ALLOCATE(f_type%parameter_identifiers(f_type%num_params))
        CALL STRING_SPLIT(tmp_string, f_type%parameter_identifiers, f_type%num_params)

        ! Function names
        CALL C_F_STRING(c_type%functions, tmp_string)
        ALLOCATE(f_type%functions(f_type%num_funcs))
        CALL STRING_SPLIT(tmp_string, f_type%functions, f_type%num_funcs)
    END SUBROUTINE

    FUNCTION PARSE_LATEX(expression) RESULT(parsed_data_f)
        CHARACTER(LEN=*), INTENT(IN)             :: expression
        TYPE(ParseOutput_t)                      :: parsed_data
        TYPE(ParseLatex_t)                       :: parsed_data_f
        CHARACTER(c_char), DIMENSION(:), POINTER :: c_expression
        CHARACTER(128)                           :: error_msg

        ! Parse the expression
        CALL F_C_STRING_ALLOC(expression, c_expression)
        parsed_data = ParseLatexToF90(c_expression(1)) ! Do the heavy lifting
        CALL F_C_STRING_DEALLOC(c_expression)

        IF(parsed_data%error.EQ.0) THEN
            CALL CheckParseValidity(parsed_data, TRIM(fname_cache))
        ENDIF

        IF(parsed_data%error.NE.0) THEN
            CALL GetErrorMsg(parsed_data, error_msg)
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*) '       ERROR:           Failed to parse the LaTeX code provided.'
            WRITE(*,*) '       ERROR:           Error message = ', TRIM(error_msg), '.'
            WRITE(*,*) '       ERROR:           Aborting Execution...'
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            CALL FreeParseOutput(parsed_data)
            STOP
        ENDIF
        
        ! Initialize Fortran side struct with well defined types
        CALL C_F_PARSESTRUCT(parsed_data, parsed_data_f)

        ! Clean up C side
        CALL FreeParseOutput(parsed_data)
    END FUNCTION

    SUBROUTINE PARSE_LATEX_DEALLOC(parsed_data)
        TYPE(ParseLatex_t), INTENT(INOUT) :: parsed_data

        DEALLOCATE(parsed_data%parameter_names)
        DEALLOCATE(parsed_data%parameter_identifiers)
        DEALLOCATE(parsed_data%functions)
    END SUBROUTINE

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

    SUBROUTINE CLEAN_ENTRIES()
        IMPLICIT NONE

        IF(nentries.GT.0) THEN
            DEALLOCATE(entries)
        ENDIF
    END SUBROUTINE

    SUBROUTINE ADD_ADDR(arg)
        TYPE(c_ptr), INTENT(IN) :: arg
        TYPE(c_ptr), ALLOCATABLE :: tmp(:)        

        IF(nloaded_addr.EQ.0) THEN
            ALLOCATE(loaded_addr(2))
        ENDIF

        IF(SIZE(loaded_addr).EQ.nloaded_addr) THEN
            CALL MOVE_ALLOC(loaded_addr, tmp)
            ALLOCATE(loaded_addr(nloaded_addr*2))
            loaded_addr(1:nloaded_addr) = tmp
        ENDIF

        nloaded_addr = nloaded_addr + 1
        loaded_addr(nloaded_addr) = arg
    END SUBROUTINE

    SUBROUTINE CLEAN_ADDRS()
        IMPLICIT NONE
        INTEGER :: i

        IF(nloaded_addr.GT.0) THEN
            DO i = 1, nloaded_addr
                CALL FREE_DLL(loaded_addr(i))
            END DO
            DEALLOCATE(loaded_addr)
        ENDIF
    END SUBROUTINE
    
    SUBROUTINE READ_CACHE()
        CHARACTER(LEN=512) :: line
        CHARACTER(LEN=64)  :: date, argc, fname
        CHARACTER(LEN=512) :: fdec
        INTEGER            :: argc_i
        LOGICAL            :: exist
        INTEGER            :: i0, i1, i2
        
        INQUIRE(FILE=TRIM(fname_cache), EXIST=exist)
        IF (exist) THEN
            OPEN(77, FILE=TRIM(fname_cache), STATUS='old', ACTION='read')
            DO
                READ(77,('(A)'), END=10) line

                i0 = INDEX(line, '-')
                i1 = INDEX(line(i0+1:), '-') + i0
                i2 = INDEX(line(i1+1:), '-') + i1
                fname = TRIM(line(1:i0-1))
                argc  = TRIM(line(i0+1:i1-1))
                date  = TRIM(line(i1+1:i2-1))
                fdec  = TRIM(line(i2+1:LEN_TRIM(line)))
                
                READ(argc,*) argc_i
                CALL ADD_ENTRY(cache_entry_t(date, TRIM(fname), TRIM(fdec), argc_i))
            END DO
10          CLOSE(77)
        ENDIF
    END SUBROUTINE

    SUBROUTINE UPDATE_CACHE(fname, argc, fdec)
        CHARACTER(LEN=*), INTENT(IN)   :: fname
        INTEGER, INTENT(IN)            :: argc
        CHARACTER(LEN=512), INTENT(IN) :: fdec
        CHARACTER(LEN=64)              :: date
        INTEGER                        :: i

        CALL FDATE(date)
        DO i = 1, nentries
            IF(fname.EQ.TRIM(entries(i)%name)) THEN
                entries(i)%date_modified = date
                entries(i)%dec = TRIM(fdec)
                entries(i)%argc = argc
                RETURN
            ENDIF
        END DO

        CALL ADD_ENTRY(cache_entry_t(date, fname, TRIM(fdec), argc))
    END SUBROUTINE

    SUBROUTINE GET_CACHE_SIZE(n)
        INTEGER, INTENT(OUT) :: n
        n = nentries
    END SUBROUTINE

    FUNCTION GET_CACHE(i)
        INTEGER, INTENT(IN) :: i
        TYPE(cache_entry_t) :: GET_CACHE

        IF(i.GT.nentries) THEN
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*) '       ERROR:           Trying to get cache with invalid index.'
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            RETURN
        ENDIF

        GET_CACHE = entries(i)
    END FUNCTION

    SUBROUTINE WRITE_CACHE()
        INTEGER :: i
        
        OPEN(UNIT=77, FILE=TRIM(fname_cache), STATUS='UNKNOWN')
            DO i = 1, nentries
                WRITE(77,'(a, a, I2, a, a, a, a)')         &
                    TRIM(entries(i)%name), ' - ',          &
                    entries(i)%argc, ' - ',                &
                    TRIM(entries(i)%date_modified), ' - ', &
                    TRIM(entries(i)%dec)
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

    SUBROUTINE COMPILE_CACHE_FUNC(parse_data, original_data)
        TYPE(ParseLatex_t), INTENT(IN) :: parse_data
        CHARACTER(LEN=512), INTENT(IN) :: original_data
        CHARACTER(LEN=128)             :: filename
        INTEGER                        :: status
        CHARACTER(128)                 :: funcname
        INTEGER                        :: argc
        CHARACTER(512)                 :: expression
        INTEGER                        :: i

        funcname   = parse_data%function_name
        argc       = parse_data%num_params + 1
        expression = parse_data%infixcode_f90
        
        WRITE(filename, '(a,a)') TRIM(nf_cache_folder), 'last_compile.f90'
        OPEN(UNIT=77, FILE=TRIM(filename), STATUS='UNKNOWN')
            WRITE(77,'(a)') 'function '//TRIM(funcname)//"(x, npar, params) bind(c, name='"//TRIM(funcname)//"_')"
            WRITE(77,'(a)') char(9)//'use, intrinsic :: iso_c_binding'
            WRITE(77,'(a)') char(9)//'implicit none'
            WRITE(77,'(a)') char(9)//'real(8), intent(in) :: x'
            WRITE(77,'(a)') char(9)//'integer, intent(in) :: npar'
            WRITE(77,'(a)') char(9)//'real(8), intent(in) :: params(npar)'
            WRITE(77,'(a)') char(9)//'real(c_double)      :: '//TRIM(funcname)
            WRITE(77,'(a)') char(9)
            WRITE(77,'(a)') char(9)//'real(8), parameter  :: pi = 3.141592653589793d0'
            WRITE(77,'(a)') char(9)
            DO i = 1, parse_data%num_funcs
                WRITE(77,'(a)') char(9)//'real(c_double), external :: '//TRIM(parse_data%functions(i))
            END DO
            WRITE(77,'(a)') char(9)
            DO i = 1, parse_data%num_params
                WRITE(77,'(a)') char(9)//'real(8) :: '//TRIM(parse_data%parameter_identifiers(i))
            END DO
            WRITE(77,'(a)') char(9)
            DO i = 1, parse_data%num_params
                IF(LEN_TRIM(parse_data%parameter_identifiers(i)).EQ.2) THEN
                    WRITE(77,'(a, I0, a)') char(9)//TRIM(parse_data%parameter_identifiers(i))//' = params(',i,')'
                ELSE
                    WRITE(77,'(a, I0, a)') char(9)//TRIM(parse_data%parameter_identifiers(i))//'  = params(',i,')'
                ENDIF
            END DO
            WRITE(77,'(a)') char(9)
            WRITE(77,'(a)') char(9)//TRIM(funcname)//' = '//TRIM(expression)
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
        CALL UPDATE_CACHE(TRIM(funcname), argc, original_data)
        CALL WRITE_CACHE()
    END SUBROUTINE

    SUBROUTINE INIT_AUTOFUNC()
        CALL READ_CACHE()
    END SUBROUTINE

    SUBROUTINE CLEAN_AUTOFUNC()
        CALL WRITE_CACHE()
        CALL CLEAN_ADDRS()
        CALL CLEAN_ENTRIES()
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

    SUBROUTINE GET_USER_FUNC_PROCPTR(name, proc, loaded)
        USE, INTRINSIC :: iso_c_binding
        CHARACTER(128), INTENT(IN)                 :: name
        PROCEDURE(proc_ptr_t), POINTER, INTENT(IN) :: proc
        LOGICAL, INTENT(OUT)                       :: loaded

        TYPE(c_funptr) :: procaddr
        TYPE(c_ptr)    :: fileaddr

        CALL LOAD_DLL_PROC(name, procaddr, fileaddr)
        CALL C_F_PROCPOINTER(procaddr, proc)

        IF(.NOT.associated(proc)) THEN
            loaded = .FALSE.
        ELSE
            loaded = .TRUE.
        ENDIF

    END SUBROUTINE

END MODULE MOD_AUTOFUNC
