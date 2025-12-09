MODULE MOD_ARRAY_TRIES
  ! Module for input parameters definitions

  IMPLICIT NONE

  ! Arrays for all points for each try
  REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: live_final_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_like_final_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: weight_try
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: live_birth_final_try
  INTEGER(4), ALLOCATABLE, DIMENSION(:,:) :: live_rank_final_try

END MODULE MOD_ARRAY_TRIES
