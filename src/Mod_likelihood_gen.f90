! Brief  : This is the likelihood pointer to distringuish between data likelihood, function integrals and partition functions
! Date   : 14/11/2024

MODULE MOD_LIKELIHOOD_GEN
    USE MOD_LIKELIHOOD
    USE MOD_POTENTIALS
    USE MOD_INTEGRATED_FUNC
    USE MOD_LOGGER
    USE MOD_AUTOFUNC
    USE MOD_PARAMETERS
    USE MOD_SEARCH_NEW_POINT, ONLY: INIT_SEARCH_METHOD

    IMPLICIT NONE

    PUBLIC :: LOGLIKELIHOOD, LOGLIKELIHOOD_WITH_TEST, INIT_LIKELIHOOD, FINAL_LIKELIHOOD
    
    PRIVATE
    
    PROCEDURE(proc_like_t), POINTER :: LOGLIKELIHOOD => null()
    PROCEDURE(proc_like_t), POINTER :: LOGLIKELIHOOD_WITH_TEST => null()

    ABSTRACT INTERFACE
       REAL*8 FUNCTION proc_like_t(npar,par)
         USE, INTRINSIC :: iso_c_binding
         IMPLICIT NONE
         REAL(c_double)      :: proc_time_t
         INTEGER, INTENT(IN) :: npar
         REAL(8), DIMENSION(npar), INTENT(IN) :: par
       END FUNCTION proc_like_t
    END INTERFACE
    
    CONTAINS
    
    SUBROUTINE INIT_LIKELIHOOD
        ! Select the likelihood type and the init and final associated calculations
        
        REAL*8, EXTERNAL :: LIKELIHOOD_DATA, LIKELIHOOD_POT, LIKELIHOOD_INTEG

        ! Initialize the search method params
        CALL INIT_SEARCH_METHOD()
        
        IF (calc_mode.EQ.'DATA') THEN !----------------------------------
            CALL INIT_LIKELIHOOD_DATA()
            LOGLIKELIHOOD_WITH_TEST => LOGLIKELIHOOD_WITH_TEST_DATA
            LOGLIKELIHOOD => LOGLIKELIHOOD_DATA
        ELSE IF (calc_mode.EQ.'POTENTIAL') THEN !------------------------
            CALL INIT_LIKELIHOOD_POT()
            LOGLIKELIHOOD_WITH_TEST => LOGLIKELIHOOD_WITH_TEST_POT
            LOGLIKELIHOOD => LOGLIKELIHOOD_POT
        ELSE IF (calc_mode.EQ.'INTEGRAL') THEN ! ------------------------
            CALL INIT_LIKELIHOOD_INTEG()
            LOGLIKELIHOOD_WITH_TEST => LOGLIKELIHOOD_WITH_TEST_INTEG
            LOGLIKELIHOOD => LOGLIKELIHOOD_INTEG
        ELSE ! -------------------------------------------------------------
            CALL LOG_ERROR_HEADER()
            CALL LOG_ERROR('Select a valid calculation mode.')
            CALL LOG_ERROR('Aborting Execution...')
            CALL LOG_ERROR_HEADER() 
        END IF ! -----------------------------------------------------------
    END SUBROUTINE INIT_LIKELIHOOD

    SUBROUTINE FINAL_LIKELIHOOD(live_max,par_mean,par_median_w)
        REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w
        IF (calc_mode.EQ.'DATA') THEN !----------------------------------
            CALL FINAL_LIKELIHOOD_DATA(live_max,par_mean,par_median_w)
        ELSE IF (calc_mode.EQ.'POTENTIAL') THEN !------------------------
            CALL FINAL_LIKELIHOOD_POT(live_max,par_mean,par_median_w)
        ELSE IF (calc_mode.EQ.'INTEGRAL') THEN ! ------------------------
            CALL FINAL_LIKELIHOOD_INTEG(live_max,par_mean,par_median_w)
        END IF ! -----------------------------------------------------------
    END SUBROUTINE FINAL_LIKELIHOOD

END MODULE MOD_LIKELIHOOD_GEN
