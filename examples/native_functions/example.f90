! Brief : Example fortran function to RT into nested_fit
! Author: César Godinho
! Date  : 15/11/23

! These defines are required and must be empty, the argument names can be arbitrary
#define DCL_VAR(name, index)
#define NF_FUNC_EXPORT

! You can use any fortran compliant code
MODULE Example
    TYPE, PUBLIC :: LinearFunc_t
        REAL(8) :: a, b
        
        CONTAINS
        PROCEDURE :: at => LINEAR_AT
    END TYPE LinearFunc_t

    CONTAINS
    FUNCTION LINEAR_AT(this, x) RESULT(y)
        IMPLICIT NONE
        CLASS(LinearFunc_t), INTENT(IN) :: this
        REAL(8), INTENT(IN)             :: x
        REAL(8)                         :: y
        
        y = this%a * x + this%b
    END FUNCTION
END MODULE

! Now declare the export function via the NF_FUNC_EXPORT directive
! BINC(c, name='<lowercase_name>') is also required
! The Function signature needs to be exactly like this (except parameter names)
NF_FUNC_EXPORT FUNCTION EXPORT_FUNC(x, npar, params) bind(c, name='export_func')
    ! This is required, since the return type is c compliant
    USE, INTRINSIC :: iso_c_binding
    USE Example

    REAL(8), INTENT(IN) :: x
    INTEGER, INTENT(IN) :: npar
    REAL(8), INTENT(IN) :: params(npar)
    REAL(c_double)      :: EXPORT_FUNC
    TYPE(LinearFunc_t)  :: f
    ! This can be anywhere inside the function
    ! This assigns the parameter names to the params
    ! Usually you'll want to name as many as you use
    ! Since this will be used to calculate the npar
    ! These are the names that must be present in the input file
    DCL_VAR("x_0", 1)
    DCL_VAR("x_1", 2)

    f = LinearFunc_t(params(1), params(2)) ! f(x) = x * x_0 + x_1
    

    ! This is possible but not recomended since, this function will be called
    ! many, many times from within nested_fit
    ! Here it is just illustratory
    WRITE(*,*) "Hello from fortran_func"

    ! return -> f(x)
    EXPORT_FUNC = f%at(x) 
END FUNCTION

! Now just test it:
! nested_fitx.x.x -fa example.f90
! nested_fitx.x.x -fr 'export_func(2, 2, [-4, -3])'

