! Brief  : This is intended to be a replacement for USERFCN.f, USERFCN_SET.f and USERFCN_2D.f90
! Author : César Godinho
! Date   : 07/08/2023

MODULE MOD_USERFCN
    USE MOD_AUTOFUNC
    IMPLICIT NONE

    PUBLIC :: USERFCN
    PRIVATE
    
    PROCEDURE(proc_ptr_t), POINTER :: USERFCN => null()

    CONTAINS

    SUBROUTINE SELECT_USERFCN(name)
        CHARACTER(LEN=128), INTENT(IN) :: name
        
        
    END SUBROUTINE

END MODULE MOD_USERFCN
