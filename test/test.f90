! Brief : Module for unit tests.
! Author: César Godinho
! Date  : 13/08/2024

MODULE MOD_UTEST
    IMPLICIT NONE
    PRIVATE
    CHARACTER(512) :: i_name, i_lerrormsg
    LOGICAL        :: i_errorflag
    INTEGER(8)     :: i_wallrate, i_walltime(2)
    REAL           :: i_cputime(2), i_cputimetotal, i_walltimetotal

    PUBLIC :: BEGIN_TEST, END_TEST, ASSERT_REQUIRED

    CONTAINS
    SUBROUTINE BEGIN_TEST(name)
        CHARACTER(*), INTENT(IN) :: name
        i_name = name
        i_errorflag = .FALSE.
        WRITE(*,*) '===== Test Start ====='
        WRITE(*,*) 'Name: '//TRIM(i_name)
        WRITE(*,*) '======================'

        CALL CPU_TIME(i_cputime(1))
        CALL SYSTEM_CLOCK(COUNT_RATE=i_wallrate)
        CALL SYSTEM_CLOCK(COUNT=i_walltime(1))
    END SUBROUTINE

    SUBROUTINE END_TEST()
        CALL CPU_TIME(i_cputime(2))
        CALL SYSTEM_CLOCK(COUNT=i_walltime(2))
        i_cputimetotal = i_cputime(2) - i_cputime(1)
        i_walltimetotal = (i_walltime(2) - i_walltime(1))/DBLE(i_wallrate)

        WRITE(*,*) '===== Test End ====='
        WRITE(*,*) 'Name     : '//TRIM(i_name)
        WRITE(*,*) 'CPU Time :', i_cputimetotal 
        WRITE(*,*) 'Wall Time:', i_walltimetotal
        IF(i_errorflag) THEN
            WRITE(*,*) 'Status   : FAILED'
        ELSE
            WRITE(*,*) 'Status   : PASSED'
        END IF
        WRITE(*,*) '===================='
    END SUBROUTINE

    SUBROUTINE TEST_ERROR()
        WRITE(*,*) '===== ERROR ====='
        WRITE(*,*) 'Name   : '//TRIM(i_name)
        WRITE(*,*) 'Message: '//TRIM(i_lerrormsg)
        WRITE(*,*) '================='
        i_errorflag = .TRUE.
    END SUBROUTINE

    ! This flags that x is required for the test to pass
    SUBROUTINE ASSERT_REQUIRED(x, errormsg)
        LOGICAL     , INTENT(IN) :: x
        CHARACTER(*), INTENT(IN) :: errormsg

        IF(.NOT.x) THEN
            i_lerrormsg = errormsg
            CALL TEST_ERROR()
        END IF
    END SUBROUTINE
END MODULE MOD_UTEST
