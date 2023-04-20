MODULE MOD_PARAMETERS
  ! Automatic Time-stamp: <Last changed by martino on Monday 29 March 2021 at CEST 16:55:25>
  ! Module for input parameters definitions

  IMPLICIT NONE

  ! General parameters
  INTEGER(4), PARAMETER :: maxdata=10000, nsetmax=10
  ! Input variables
  CHARACTER, DIMENSION(nsetmax) :: filename*64
  CHARACTER :: set_yn*1= 'n', data_type*3= '1c'
  CHARACTER :: likelihood_funcname*64
  INTEGER(4) :: nset=1
  INTEGER(4) :: nlive=0
  REAL(8) :: evaccuracy=0., conv_par, search_par1=0., search_par2=0.
  CHARACTER :: search_method*64
  CHARACTER :: conv_method*64
  INTEGER(4) :: maxtries=1000, maxntries=10
  CHARACTER :: cluster_yn*1= 'n'
  CHARACTER :: cluster_method*1= 'f'
  REAL(8) :: cluster_par1=0.4, cluster_par2=0.1
  INTEGER(4) :: ntry=1, maxstep_try = 10000
  CHARACTER :: funcname*64
  CHARACTER :: lr*1= 'r'
  INTEGER(4) :: npoint=0, nwidth=0
  REAL(8), DIMENSION(nsetmax) :: xmin=0., xmax=0., ymin=0., ymax=0.
  INTEGER(4) :: npar=0
  INTEGER(4), ALLOCATABLE,  DIMENSION(:) :: par_num, par_fix
  CHARACTER,  ALLOCATABLE, DIMENSION(:) :: par_name*10
  REAL(8), ALLOCATABLE, DIMENSION(:) :: par_step, par_in, par_bnd1, par_bnd2
  ! parallel variables
  INTEGER(4) :: nth=1


  ! Calculated variables
  INTEGER(4) :: loglikefuncid=0
  INTEGER(4) :: funcid=0
  INTEGER(4) :: dataid=0
  INTEGER(4) :: searchid=0
  

  COMMON /func_exp/ lr
  COMMON /func_conv/ npoint, nwidth
  !



END MODULE MOD_PARAMETERS
