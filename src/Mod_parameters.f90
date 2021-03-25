MODULE MOD_PARAMETERS
  ! Automatic Time-stamp: <Last changed by martino on Wednesday 26 February 2020 at CET 15:07:03>
  ! Module for input parameters definitions

  IMPLICIT NONE
  
  ! General parameters
  INTEGER(4), PARAMETER :: maxdata=10000, nsetmax=10, maxdim=20
  ! Input variables
  CHARACTER, DIMENSION(nsetmax) :: filename*64
  CHARACTER :: set_yn*1= 'n',errorbars_yn*1= 'n'
  INTEGER(4) :: nset=1
  INTEGER(4) :: nlive=0
  REAL(8) :: evaccuracy=0., sdfraction=0.
  INTEGER(4) :: njump=20, maxtries=1000, maxntries=10
  CHARACTER :: cluster_yn*1= 'n'
  CHARACTER :: cluster_method*1= 'f'
  REAL(8) :: bandwidth=0.12, distance_limit=0.4
  INTEGER(4) :: ntry=1, maxstep_try = 10000
  CHARACTER :: funcname*64
  CHARACTER :: lr*1= 'r'
  INTEGER(4) :: npoint=0, nwidth=0
  REAL(8), DIMENSION(nsetmax) :: xmin=0., xmax=0, ymin=0., ymax=0.
  INTEGER(4) :: npar=0
  INTEGER(4), ALLOCATABLE,  DIMENSION(:) :: par_num, par_fix
  CHARACTER,  ALLOCATABLE, DIMENSION(:) :: par_name*10
  REAL(8), ALLOCATABLE, DIMENSION(:) :: par_step, par_in, par_bnd1, par_bnd2

  
  COMMON /func_exp/ lr
  COMMON /func_conv/ npoint, nwidth
  !
  


END MODULE MOD_PARAMETERS
