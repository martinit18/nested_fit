MODULE MOD_PARAMETERS
  ! Module for input parameters definitions

  IMPLICIT NONE

  ! General parameters
  INTEGER(4), PARAMETER :: maxdata=10000, nsetmax=10
  INTEGER(4), PARAMETER :: specstrmaxcol = 32
  CHARACTER(16)         :: fileformat = '.csv'

  ! Input variables
  CHARACTER(128), DIMENSION(nsetmax) :: filename
  LOGICAL                            :: is_set=.FALSE.
  CHARACTER(3)                       :: data_type='1c'
  CHARACTER(64)                      :: likelihood_funcname = 'GAUSSIAN'
  INTEGER(4)                         :: nset=1
  INTEGER(4)                         :: nlive=0

  ! Search params
  REAL(8)    :: evaccuracy=0., conv_par, search_par1=0., search_par2=0.
  CHARACTER  :: search_method*64
  CHARACTER  :: conv_method*64
  INTEGER(4) :: maxtries=1000, maxntries=10
  INTEGER(4) :: ntry=1, maxstep_try = 100000

  ! Clustering
  LOGICAL    :: make_cluster=.FALSE.
  CHARACTER  :: cluster_method='f'
  REAL(8)    :: cluster_par1=0.4, cluster_par2=0.1

  ! Function
  CHARACTER(512), DIMENSION(nsetmax)     :: funcname
  CHARACTER                              :: lr= 'r'
  INTEGER(4)                             :: npoint=0, nwidth=0
  REAL(8), DIMENSION(nsetmax)            :: xmin=0., xmax=0., ymin=0., ymax=0.
  INTEGER(4)                             :: npar=0
  INTEGER(4), ALLOCATABLE,  DIMENSION(:) :: par_num, par_fix
  CHARACTER,  ALLOCATABLE, DIMENSION(:)  :: par_name*10
  REAL(8), ALLOCATABLE, DIMENSION(:)     :: par_step, par_in, par_bnd1, par_bnd2

  ! parallel variables
  INTEGER(4) :: nth=1

  ! Calculated variables
  INTEGER(4) :: loglikefuncid=0
  INTEGER(4) :: funcid=0
  INTEGER(4) :: dataid=0
  INTEGER(4) :: searchid=0

  COMMON /func_exp/ lr
  COMMON /func_conv/ npoint, nwidth

END MODULE MOD_PARAMETERS
