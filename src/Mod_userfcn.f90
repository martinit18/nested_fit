! Brief  : This is intended to be a replacement for USERFCN.f, USERFCN_SET.f and USERFCN_2D.f90
! Author : César Godinho
! Date   : 22/08/2023

MODULE MOD_USERFCN
    USE MOD_LOGGER
    USE MOD_AUTOFUNC
    USE MOD_PARAMETERS
    IMPLICIT NONE

    PUBLIC :: USERFCN, USERFCN_SET, IS_LEGACY_USERFCN, SET_USERFUNC_PROCPTR, SET_USERFUNC_SET_PROCPTR
    PRIVATE
    
    PROCEDURE(proc_ptr_t)    , POINTER :: USERFCN     => null()
    PROCEDURE(proc_ptr_set_t), POINTER :: USERFCN_SET => null()

    TYPE arr_proc_ptr
        PROCEDURE(proc_ptr_t), POINTER, NOPASS :: ptr => null()
    END TYPE arr_proc_ptr
    
    TYPE(arr_proc_ptr), DIMENSION(32) :: SET_PTR_ARR

    INTEGER :: SET_PTR_COUNT = 0

    CONTAINS

    FUNCTION IS_LEGACY_USERFCN(funcname)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: funcname
        LOGICAL                  :: IS_LEGACY_USERFCN
        
        ! Legacy stuff
        IF(funcname.EQ.'GAUSS') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SUPERGAUSS') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'ERFPEAK') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'GAUSS_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'LORE') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'LORENORM') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'LORE_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLE_LORE_WF_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_LORE_WF_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_LORE_WF_REL_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'CONS') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLE_GAUSS_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLET_GAUSS_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TRIPLE_GAUSS_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'QUAD_GAUSS_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'QUINT_GAUSS_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_EXPBG_WF') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_DBEXPBG_WF') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'EIGHT_GAUSS_POLYBG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'EIGHT_VOIGT_POLYBG_WF') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'EXPFCN') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'ERFFCN') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'MB_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'GAUSS_EXP_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'GAUSS_GAUSS_EXP_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'GAUSS_EXP_BG_CONV') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'VOIGT') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'VOIGT_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'VOIGT_EXP') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'VOIGT_ERF') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'VOIGT_EXP_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLE_VOIGT_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLE_VOIGT_EXP_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_POLYBG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_XRD') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_POLYBG_WF') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_EXP_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_FREEGAMMA_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY2') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_EXP_POLYBG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_EXPBG_WF') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TWO_INTERP_VOIGT_POLY') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TWO_INTERP_VOIGT_POLY_X2') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'THREE_INTERP_VOIGT_POLY') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'WEIBULL') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'WEIBULL_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'WEIBULL_ERFBG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'LASER') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'THRESHOLD') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'GAUSS_ERF') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TWO_GAUSS_ERF_EXPBG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TWO_DOUBL_GAUSS_ERF_POLY') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TWO_DOUBL_GAUSS_ERF_FREESIG_POLY') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TWO_DOUBL_VOIGT_ERF_POLY') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'POLY') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'POWER') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'ND_M_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'BS_EM') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'BS_EM2') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'BS_EM_NM') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'EXPCOS') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'EXPSIN') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'EXPSIMP') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TWO_EXPSIN') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_VOIGT_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'GAUSS_ERF_CST') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'GAUSSERF_CST') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_VOIGT_BG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_GAUSS_BG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_PSEUDOVOIGT_BG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_GAUSS_PARAMETER_BG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_VOIGT_PARAMETER_BG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_PARAMETER_BG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_PARAMETER_BG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_SHIRLEYBG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_SHIRLEYBG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'ROCKING_CURVE') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_VOIGT_PARA_POLY_SIG_PLEIADES') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'POWER_CONST') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'ELEVEN_GAUSS_WF_REL_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLE_GAUSS_BG_Si') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'EIGHT_GAUSS_WF_REL_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'ELEVEN_GAUSS_WF_CORREL_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'ELEVEN_GAUSS_WF_POISSON_BG') THEN
            IS_LEGACY_USERFCN = .TRUE.

        ! For Set functions
        ELSE IF(funcname.EQ.'GAUSS_BG_SET') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLE_EXPSIMP') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLE_EXPSIN') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLE_EXPSIN_BIS') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'DOUBLE_TWO_EXPSIN') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TRIPLE_EXPSIMP') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TRIPLE_EXPSIN') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TRIPLE_EXPSIN_BIS') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'TRIPLE_EXPSIN_TRIS') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'WEIBULL_EL_LASER') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET2') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET3') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_SET') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE IF(funcname.EQ.'ROCKING_CURVE_SET') THEN
            IS_LEGACY_USERFCN = .TRUE.
        ELSE
            IS_LEGACY_USERFCN = .FALSE.
        END IF
    END FUNCTION

    SUBROUTINE SET_USERFUNC_PROCPTR(funcname)
        CHARACTER(512), INTENT(IN) :: funcname
        CHARACTER(128)             :: func_header
        LOGICAL                    :: loaded_ok

        IF(.NOT.IS_LEGACY_USERFCN(funcname)) THEN
            func_header = TRIM(funcname(1:INDEX(funcname, '(')-1))
            CALL LOG_TRACE('Setting user function pointer named => '//TRIM(func_header))

            CALL GET_USER_FUNC_PROCPTR(func_header, USERFCN, loaded_ok)

            IF(.NOT.loaded_ok) THEN
                CALL LOG_ERROR_HEADER()
                CALL LOG_ERROR('Failed to load proc address for function => '//TRIM(func_header))
                CALL LOG_ERROR('Maybe the specified function name is incorrect/not in the cache.')
                CALL LOG_ERROR('Aborting Execution...')
                CALL LOG_ERROR_HEADER()
                STOP
            ENDIF
        ELSE
            ! Set the legacy function pointer
            CALL LOG_TRACE('Setting user function pointer named => '//TRIM(funcname))
            CALL SET_LEGACY_FUNC(funcname)
        ENDIF
    END SUBROUTINE

    SUBROUTINE SET_USERFUNC_SET_PROCPTR(funcname)
        CHARACTER(512), INTENT(IN) :: funcname(nsetmax)
        CHARACTER(128)             :: func_header
        LOGICAL                    :: loaded_ok
        INTEGER                    :: i

        IF(.NOT.IS_LEGACY_USERFCN(funcname(1))) THEN
            ! Setup the user function for each available set
            DO i = 1, nset
                func_header = TRIM(funcname(i)(1:INDEX(funcname(i), '(')-1))
                CALL LOG_TRACE('Setting user function pointer set named => '//TRIM(func_header))
                CALL GET_USER_FUNC_PROCPTR(func_header, SET_PTR_ARR(i)%ptr, loaded_ok)
            
                IF(.NOT.loaded_ok) THEN
                    CALL LOG_ERROR_HEADER()
                    CALL LOG_ERROR('Failed to load proc address for function => '//TRIM(func_header))
                    CALL LOG_ERROR('Maybe the specified function name is incorrect/not in the cache.')
                    CALL LOG_ERROR('Aborting Execution...')
                    CALL LOG_ERROR_HEADER()
                    CALL HALT_EXECUTION()
                ENDIF
            END DO
            USERFCN_SET => USER_FUNCTION_SET_AUTO_PTR
        ELSE
            ! Set the legacy function pointer
            CALL LOG_TRACE('Setting user function set pointer named => '//TRIM(funcname(1)))
            CALL SET_LEGACY_FUNC(funcname(1))
        ENDIF
    END SUBROUTINE
    
    ! This get's pointed by the USERFCN_SET pointer when we are using parsed functions
    FUNCTION USER_FUNCTION_SET_AUTO_PTR(x, npar, params, sid)
        USE, INTRINSIC :: iso_c_binding
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: npar
        REAL(8), INTENT(IN) :: params(npar)
        INTEGER, INTENT(IN) :: sid
        REAL(c_double)      :: USER_FUNCTION_SET_AUTO_PTR

        USER_FUNCTION_SET_AUTO_PTR = SET_PTR_ARR(sid)%ptr(x, npar, params)
    END FUNCTION

    SUBROUTINE SET_LEGACY_FUNC(funcname)
        CHARACTER(128), INTENT(IN) :: funcname

        ! Legacy stuff for data
#ifndef FUNC_TARGET
        REAL*8, EXTERNAL :: GAUSS, SUPERGAUSS, ERFPEAK, GAUSS_BG
        REAL*8, EXTERNAL :: DOUBLE_GAUSS_BG, DOUBLET_GAUSS_BG
        REAL*8, EXTERNAL :: TRIPLE_GAUSS_BG,QUAD_GAUSS_BG,QUINT_GAUSS_BG,SIX_GAUSS_BG
        REAL*8, EXTERNAL :: SIX_GAUSS_EXPBG_WF, SIX_GAUSS_DBEXPBG_WF
        REAL*8, EXTERNAL :: LORE, LORENORM, LORE_BG, DOUBLE_LORE_WF_BG
        REAL*8, EXTERNAL :: SIX_LORE_WF_BG, SIX_LORE_WF_REL_BG, CONS
        REAL*8, EXTERNAL :: SIX_VOIGT_BG, SIX_VOIGT_XRD, SIX_VOIGT_EXP_BG
        REAL*8, EXTERNAL :: EIGHT_GAUSS_POLYBG, EIGHT_VOIGT_POLYBG_WF
        REAL*8, EXTERNAL :: EXPFCN, ERFFCN,BS_EM, BS_EM2, BS_EM_NM
        REAL*8, EXTERNAL :: POWER, POWER_CONST, ND_M_PLEIADES
        REAL*8, EXTERNAL :: POLY, SIX_VOIGT_POLYBG,SIX_VOIGT_EXP_POLYBG
        REAL*8, EXTERNAL :: SIX_VOIGT_POLYBG_WF
        REAL*8, EXTERNAL :: MB_BG, GAUSS_EXP_BG, VOIGT, DOUBLE_VOIGT_BG
        REAL*8, EXTERNAL :: GAUSS_EXP_BG_CONV,GAUSS_GAUSS_EXP_BG
        REAL*8, EXTERNAL :: VOIGT_BG,VOIGT_EXP, VOIGT_EXP_BG, DOUBLE_VOIGT_EXP_BG
        REAL*8, EXTERNAL :: SIX_VOIGT_EXPBG_WF, WEIBULL, WEIBULL_BG, WEIBULL_ERFBG
        REAL*8, EXTERNAL :: LASER, THRESHOLD
        REAL*8, EXTERNAL :: GAUSS_ERF, VOIGT_ERF, TWO_GAUSS_ERF_EXPBG
        REAL*8, EXTERNAL :: TWO_DOUBL_GAUSS_ERF_POLY,TWO_DOUBL_GAUSS_ERF_FREESIG_POLY
        REAL*8, EXTERNAL :: TWO_DOUBL_VOIGT_ERF_POLY
        REAL*8, EXTERNAL :: EXPSIN, EXPCOS, EXPSIMP, TWO_EXPSIN
        REAL*8, EXTERNAL :: FOUR_VOIGT_BG, SIX_VOIGT_FREEGAMMA_BG
        REAL*8, EXTERNAL :: SIX_GAUSS_ERF_FREESIG_POLY, SIX_GAUSS_ERF_FREESIG_POLY2
        REAL*8, EXTERNAL :: FOUR_GAUSS_ERF_TWO_GAUSS
        REAL*8, EXTERNAL :: FOUR_GAUSS_ERF_TWO_GAUSS_STAN
        REAL*8, EXTERNAL :: GAUSS_ERF_CST, GAUSSERF_CST, FOUR_VOIGT_BG_PLEIADES
        REAL*8, EXTERNAL :: FOUR_GAUSS_BG_PLEIADES, FOUR_PSEUDOVOIGT_BG_PLEIADES
        REAL*8, EXTERNAL :: FOUR_GAUSS_PARAMETER_BG_PLEIADES
        REAL*8, EXTERNAL :: FOUR_VOIGT_PARAMETER_BG_PLEIADES
        REAL*8, EXTERNAL :: SIX_VOIGT_PARA_POLY_SIG_PLEIADES
        REAL*8, EXTERNAL :: SIX_VOIGT_PARAMETER_BG_PLEIADES
        REAL*8, EXTERNAL :: SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES
        REAL*8, EXTERNAL :: SIX_GAUSS_PARAMETER_BG_PLEIADES
        REAL*8, EXTERNAL :: SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES
        REAL*8, EXTERNAL :: SIX_GAUSS_SHIRLEYBG
        REAL*8, EXTERNAL :: SIX_VOIGT_SHIRLEYBG
        REAL*8, EXTERNAL :: SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES
        REAL*8, EXTERNAL :: ROCKING_CURVE,TWO_INTERP_VOIGT_POLY,THREE_INTERP_VOIGT_POLY
        REAL*8, EXTERNAL :: TWO_INTERP_VOIGT_POLY_X0
        REAL*8, EXTERNAL :: DECAY, DECAY_SIMP
        REAL*8, EXTERNAL :: ELEVEN_GAUSS_WF_REL_BG, ELEVEN_GAUSS_WF_CORREL_BG
        REAL*8, EXTERNAL :: DOUBLE_GAUSS_BG_Si
        REAL*8, EXTERNAL :: EIGHT_GAUSS_WF_REL_BG
        REAL*8, EXTERNAL :: ELEVEN_GAUSS_WF_POISSON_BG

        ! For set
        REAL*8, EXTERNAL ::  WEIBULL_EL_LASER, GAUSS_BG_SET
        REAL*8, EXTERNAL ::  DOUBLE_EXPSIN, DOUBLE_EXPSIN_BIS, DOUBLE_EXPSIMP
        REAL*8, EXTERNAL ::  DOUBLE_TWO_EXPSIN
        REAL*8, EXTERNAL ::  TRIPLE_EXPSIN,TRIPLE_EXPSIN_BIS,TRIPLE_EXPSIN_TRIS
        REAL*8, EXTERNAL ::  TRIPLE_EXPSIMP
        REAL*8, EXTERNAL ::  SIX_GAUSS_ERF_FREESIG_POLY_SET
        REAL*8, EXTERNAL ::  SIX_GAUSS_ERF_FREESIG_POLY_SET2
        REAL*8, EXTERNAL ::  SIX_GAUSS_ERF_FREESIG_POLY_SET3
        REAL*8, EXTERNAL ::  FOUR_GAUSS_ERF_TWO_GAUSS_SET
        REAL*8, EXTERNAL ::  FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET
        REAL*8, EXTERNAL ::  FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET
        REAL*8, EXTERNAL ::  FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET
        REAL*8, EXTERNAL ::  ROCKING_CURVE_SET

        IF(funcname.EQ.'GAUSS') THEN
            USERFCN => GAUSS
        ELSE IF(funcname.EQ.'SUPERGAUSS') THEN
            USERFCN => SUPERGAUSS
        ELSE IF(funcname.EQ.'ERFPEAK') THEN
            USERFCN => ERFPEAK
        ELSE IF(funcname.EQ.'GAUSS_BG') THEN
            USERFCN => GAUSS_BG
        ELSE IF(funcname.EQ.'LORE') THEN
            USERFCN => LORE
        ELSE IF(funcname.EQ.'LORENORM') THEN
            USERFCN => LORENORM
        ELSE IF(funcname.EQ.'LORE_BG') THEN
            USERFCN => LORE_BG
        ELSE IF(funcname.EQ.'DOUBLE_LORE_WF_BG') THEN
            USERFCN => DOUBLE_LORE_WF_BG
        ELSE IF(funcname.EQ.'SIX_LORE_WF_BG') THEN
            USERFCN => SIX_LORE_WF_BG
        ELSE IF(funcname.EQ.'SIX_LORE_WF_REL_BG') THEN
            USERFCN => SIX_LORE_WF_REL_BG
        ELSE IF(funcname.EQ.'CONS') THEN
            USERFCN => CONS
        ELSE IF(funcname.EQ.'DOUBLE_GAUSS_BG') THEN
            USERFCN => DOUBLE_GAUSS_BG
        ELSE IF(funcname.EQ.'DOUBLET_GAUSS_BG') THEN
            USERFCN => DOUBLET_GAUSS_BG
        ELSE IF(funcname.EQ.'TRIPLE_GAUSS_BG') THEN
            USERFCN => TRIPLE_GAUSS_BG
        ELSE IF(funcname.EQ.'QUAD_GAUSS_BG') THEN
            USERFCN => QUAD_GAUSS_BG
        ELSE IF(funcname.EQ.'QUINT_GAUSS_BG') THEN
            USERFCN => QUINT_GAUSS_BG
        ELSE IF(funcname.EQ.'SIX_GAUSS_BG') THEN
            USERFCN => SIX_GAUSS_BG
        ELSE IF(funcname.EQ.'SIX_GAUSS_EXPBG_WF') THEN
            USERFCN => SIX_GAUSS_EXPBG_WF
        ELSE IF(funcname.EQ.'SIX_GAUSS_DBEXPBG_WF') THEN
            USERFCN => SIX_GAUSS_DBEXPBG_WF
        ELSE IF(funcname.EQ.'EIGHT_GAUSS_POLYBG') THEN
            USERFCN => EIGHT_GAUSS_POLYBG
        ELSE IF(funcname.EQ.'EIGHT_VOIGT_POLYBG_WF') THEN
            USERFCN => EIGHT_VOIGT_POLYBG_WF
        ELSE IF(funcname.EQ.'EXPFCN') THEN
            USERFCN => EXPFCN
        ELSE IF(funcname.EQ.'ERFFCN') THEN
            USERFCN => ERFFCN
        ELSE IF(funcname.EQ.'MB_BG') THEN
            USERFCN => MB_BG
        ELSE IF(funcname.EQ.'GAUSS_EXP_BG') THEN
            USERFCN => GAUSS_EXP_BG
        ELSE IF(funcname.EQ.'GAUSS_GAUSS_EXP_BG') THEN
            USERFCN => GAUSS_GAUSS_EXP_BG
        ELSE IF(funcname.EQ.'GAUSS_EXP_BG_CONV') THEN
            USERFCN => GAUSS_EXP_BG_CONV
        ELSE IF(funcname.EQ.'VOIGT') THEN
            USERFCN => VOIGT
        ELSE IF(funcname.EQ.'VOIGT_BG') THEN
            USERFCN => VOIGT_BG
        ELSE IF(funcname.EQ.'VOIGT_EXP') THEN
            USERFCN => VOIGT_EXP
        ELSE IF(funcname.EQ.'VOIGT_ERF') THEN
            USERFCN => VOIGT_ERF
        ELSE IF(funcname.EQ.'VOIGT_EXP_BG') THEN
            USERFCN => VOIGT_EXP_BG
        ELSE IF(funcname.EQ.'DOUBLE_VOIGT_BG') THEN
            USERFCN => DOUBLE_VOIGT_BG
        ELSE IF(funcname.EQ.'DOUBLE_VOIGT_EXP_BG') THEN
            USERFCN => DOUBLE_VOIGT_EXP_BG
        ELSE IF(funcname.EQ.'SIX_VOIGT_BG') THEN
            USERFCN => SIX_VOIGT_BG
        ELSE IF(funcname.EQ.'SIX_VOIGT_POLYBG') THEN
            USERFCN => SIX_VOIGT_POLYBG
        ELSE IF(funcname.EQ.'SIX_VOIGT_XRD') THEN
            USERFCN => SIX_VOIGT_XRD
        ELSE IF(funcname.EQ.'SIX_VOIGT_POLYBG_WF') THEN
            USERFCN => SIX_VOIGT_POLYBG_WF
        ELSE IF(funcname.EQ.'SIX_VOIGT_EXP_BG') THEN
            USERFCN => SIX_VOIGT_EXP_BG
        ELSE IF(funcname.EQ.'SIX_VOIGT_FREEGAMMA_BG') THEN
            USERFCN => SIX_VOIGT_FREEGAMMA_BG
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY') THEN
            USERFCN => SIX_GAUSS_ERF_FREESIG_POLY
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY2') THEN
            USERFCN => SIX_GAUSS_ERF_FREESIG_POLY2
        ELSE IF(funcname.EQ.'SIX_VOIGT_EXP_POLYBG') THEN
            USERFCN => SIX_VOIGT_EXP_POLYBG
        ELSE IF(funcname.EQ.'SIX_VOIGT_EXPBG_WF') THEN
            USERFCN => SIX_VOIGT_EXPBG_WF
        ELSE IF(funcname.EQ.'TWO_INTERP_VOIGT_POLY') THEN
            USERFCN => TWO_INTERP_VOIGT_POLY
        ELSE IF(funcname.EQ.'TWO_INTERP_VOIGT_POLY_X0') THEN
            USERFCN => TWO_INTERP_VOIGT_POLY_X0
        ELSE IF(funcname.EQ.'THREE_INTERP_VOIGT_POLY') THEN
            USERFCN => THREE_INTERP_VOIGT_POLY
        ELSE IF(funcname.EQ.'WEIBULL') THEN
            USERFCN => WEIBULL
        ELSE IF(funcname.EQ.'WEIBULL_BG') THEN
            USERFCN => WEIBULL_BG
        ELSE IF(funcname.EQ.'WEIBULL_ERFBG') THEN
            USERFCN => WEIBULL_ERFBG
        ELSE IF(funcname.EQ.'LASER') THEN
            USERFCN => LASER
        ELSE IF(funcname.EQ.'THRESHOLD') THEN
            USERFCN => THRESHOLD
        ELSE IF(funcname.EQ.'GAUSS_ERF') THEN
            USERFCN => GAUSS_ERF
        ELSE IF(funcname.EQ.'TWO_GAUSS_ERF_EXPBG') THEN
            USERFCN => TWO_GAUSS_ERF_EXPBG
        ELSE IF(funcname.EQ.'TWO_DOUBL_GAUSS_ERF_POLY') THEN
            USERFCN => TWO_DOUBL_GAUSS_ERF_POLY
        ELSE IF(funcname.EQ.'TWO_DOUBL_GAUSS_ERF_FREESIG_POLY') THEN
            USERFCN => TWO_DOUBL_GAUSS_ERF_FREESIG_POLY
        ELSE IF(funcname.EQ.'TWO_DOUBL_VOIGT_ERF_POLY') THEN
            USERFCN => TWO_DOUBL_VOIGT_ERF_POLY
        ELSE IF(funcname.EQ.'POLY') THEN
            USERFCN => POLY
        ELSE IF(funcname.EQ.'POWER') THEN
            USERFCN => POWER
        ELSE IF(funcname.EQ.'ND_M_PLEIADES') THEN
            USERFCN => ND_M_PLEIADES
        ELSE IF(funcname.EQ.'BS_EM') THEN
            USERFCN => BS_EM
        ELSE IF(funcname.EQ.'BS_EM2') THEN
            USERFCN => BS_EM2
        ELSE IF(funcname.EQ.'BS_EM_NM') THEN
            USERFCN => BS_EM_NM
        ELSE IF(funcname.EQ.'EXPCOS') THEN
            USERFCN => EXPCOS
        ELSE IF(funcname.EQ.'EXPSIN') THEN
            USERFCN => EXPSIN
        ELSE IF(funcname.EQ.'EXPSIMP') THEN
            USERFCN => EXPSIMP
        ELSE IF(funcname.EQ.'TWO_EXPSIN') THEN
            USERFCN => TWO_EXPSIN
        ELSE IF(funcname.EQ.'FOUR_VOIGT_BG') THEN
            USERFCN => FOUR_VOIGT_BG
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS') THEN
            USERFCN => FOUR_GAUSS_ERF_TWO_GAUSS
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN') THEN
            USERFCN => FOUR_GAUSS_ERF_TWO_GAUSS_STAN
        ELSE IF(funcname.EQ.'GAUSS_ERF_CST') THEN
            USERFCN => GAUSS_ERF_CST
        ELSE IF(funcname.EQ.'GAUSSERF_CST') THEN
            USERFCN => GAUSSERF_CST
        ELSE IF(funcname.EQ.'FOUR_VOIGT_BG_PLEIADES') THEN
            USERFCN => FOUR_VOIGT_BG_PLEIADES
        ELSE IF(funcname.EQ.'FOUR_GAUSS_BG_PLEIADES') THEN
            USERFCN => FOUR_GAUSS_BG_PLEIADES
        ELSE IF(funcname.EQ.'FOUR_PSEUDOVOIGT_BG_PLEIADES') THEN
            USERFCN => FOUR_PSEUDOVOIGT_BG_PLEIADES
        ELSE IF(funcname.EQ.'FOUR_GAUSS_PARAMETER_BG_PLEIADES') THEN
            USERFCN => FOUR_GAUSS_PARAMETER_BG_PLEIADES
        ELSE IF(funcname.EQ.'FOUR_VOIGT_PARAMETER_BG_PLEIADES') THEN
            USERFCN => FOUR_VOIGT_PARAMETER_BG_PLEIADES
        ELSE IF(funcname.EQ.'SIX_VOIGT_PARAMETER_BG_PLEIADES') THEN
            USERFCN => SIX_VOIGT_PARAMETER_BG_PLEIADES
        ELSE IF(funcname.EQ.'SIX_GAUSS_PARAMETER_BG_PLEIADES') THEN
            USERFCN => SIX_GAUSS_PARAMETER_BG_PLEIADES
        ELSE IF(funcname.EQ.'SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES') THEN
            USERFCN => SIX_PSEUDOVOIGT_PARAMETER_BG_PLEIADES
        ELSE IF(funcname.EQ.'SIX_GAUSS_SHIRLEYBG') THEN
            USERFCN => SIX_GAUSS_SHIRLEYBG
        ELSE IF(funcname.EQ.'SIX_VOIGT_SHIRLEYBG') THEN
            USERFCN => SIX_VOIGT_SHIRLEYBG
        ELSE IF(funcname.EQ.'SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES') THEN
            USERFCN => SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES
        ELSE IF(funcname.EQ.'SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES') THEN
            USERFCN => SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES
        ELSE IF(funcname.EQ.'ROCKING_CURVE') THEN
            USERFCN => ROCKING_CURVE
        ELSE IF(funcname.EQ.'SIX_VOIGT_PARA_POLY_SIG_PLEIADES') THEN
            USERFCN => SIX_VOIGT_PARA_POLY_SIG_PLEIADES
        ELSE IF(funcname.EQ.'POWER_CONST') THEN
            USERFCN => POWER_CONST
        ELSE IF(funcname.EQ.'ELEVEN_GAUSS_WF_REL_BG') THEN
            USERFCN => ELEVEN_GAUSS_WF_REL_BG
        ELSE IF(funcname.EQ.'DOUBLE_GAUSS_BG_Si') THEN
            USERFCN => DOUBLE_GAUSS_BG_Si
        ELSE IF(funcname.EQ.'EIGHT_GAUSS_WF_REL_BG') THEN
            USERFCN => EIGHT_GAUSS_WF_REL_BG
        ELSE IF(funcname.EQ.'ELEVEN_GAUSS_WF_CORREL_BG') THEN
            USERFCN => ELEVEN_GAUSS_WF_CORREL_BG
        ELSE IF(funcname.EQ.'ELEVEN_GAUSS_WF_POISSON_BG') THEN
            USERFCN => ELEVEN_GAUSS_WF_POISSON_BG

        ! Set start
        ELSE IF(funcname.EQ.'GAUSS_BG_SET') THEN
            USERFCN_SET => GAUSS_BG_SET
        ELSE IF(funcname.EQ.'DOUBLE_EXPSIMP') THEN
            USERFCN_SET => DOUBLE_EXPSIMP
        ELSE IF(funcname.EQ.'DOUBLE_EXPSIN') THEN
            USERFCN_SET => DOUBLE_EXPSIN
        ELSE IF(funcname.EQ.'DOUBLE_EXPSIN_BIS') THEN
            USERFCN_SET => DOUBLE_EXPSIN_BIS
        ELSE IF(funcname.EQ.'DOUBLE_TWO_EXPSIN') THEN
            USERFCN_SET => DOUBLE_TWO_EXPSIN
        ELSE IF(funcname.EQ.'TRIPLE_EXPSIMP') THEN
            USERFCN_SET => TRIPLE_EXPSIMP
        ELSE IF(funcname.EQ.'TRIPLE_EXPSIN') THEN
            USERFCN_SET => TRIPLE_EXPSIN
        ELSE IF(funcname.EQ.'TRIPLE_EXPSIN_BIS') THEN
            USERFCN_SET => TRIPLE_EXPSIN_BIS
        ELSE IF(funcname.EQ.'TRIPLE_EXPSIN_TRIS') THEN
            USERFCN_SET => TRIPLE_EXPSIN_TRIS
        ELSE IF(funcname.EQ.'WEIBULL_EL_LASER') THEN
            USERFCN_SET => WEIBULL_EL_LASER
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET') THEN
            USERFCN_SET => SIX_GAUSS_ERF_FREESIG_POLY_SET
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET2') THEN
            USERFCN_SET => SIX_GAUSS_ERF_FREESIG_POLY_SET2
        ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET3') THEN
            USERFCN_SET => SIX_GAUSS_ERF_FREESIG_POLY_SET3
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_SET') THEN
            USERFCN_SET => FOUR_GAUSS_ERF_TWO_GAUSS_SET
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET') THEN
            USERFCN_SET => FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET') THEN
            USERFCN_SET => FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET
        ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET') THEN
            USERFCN_SET => FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET
        ELSE IF(funcname.EQ.'ROCKING_CURVE_SET') THEN
            USERFCN_SET => ROCKING_CURVE_SET
        ENDIF
#else
    ! There is no USERFCN for functions
    USERFCN => null()

    ! There is no USERFCN_SET for functions
    USERFCN_SET => null()
#endif
    END SUBROUTINE

END MODULE MOD_USERFCN
