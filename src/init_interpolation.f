      SUBROUTINE INTERPOLATE(fileinterp_name,
     +     interpolated_name,s,t,c,k,nn)
c     Subroutine for the preliminary interpolation of the simulated spectra.
c     Because of the statistical noise, the spectra are fitted with as weight, the expected statistical fluctiation
c     To note, the simulated spectra must have positive count number for all channels
      USE MOD_LOGGER
      IMPLICIT NONE
c     Simulation variables
      CHARACTER*64 fileinterp_name, interpolated_name
      INTEGER*4 i, interpmax, n
      PARAMETER (interpmax = 100000)
      REAL*8 x(interpmax), y(interpmax), w(interpmax)
      REAL*8 sum
c     Interpolation subroutine variables
      INTEGER*4 iopt, k, nest, nn, lwrk, ier
      REAL*8 fp, s
      PARAMETER (iopt=0,nest=1000,lwrk=20000)
c     s_s=90,s_p=60
      REAL*8 t(nest), c(nest)
      REAL*8 wrk(lwrk)
      INTEGER*4 iwrk(nest)
      REAL*8 xp, yp


c     Initialize the variables
      n = 0
      k = 3
      DO i=1,interpmax
         x(i) = 0.
         y(i) = 0.
         w(i) = 0.
      ENDDO


      WRITE(*,*) ' '
      WRITE(*,*) '##### Initialization interpolation ######'
      WRITE(*,*) fileinterp_name

      sum = 0.
      OPEN(18,file=fileinterp_name, status='old')
      DO i=1, interpmax
         READ(18,*,END=180) x(i), y(i)
         sum = sum + y(i)
      ENDDO
 180  CONTINUE
      n = i-1
      CLOSE(18)
c     Normalize function and calculate the weights
      DO i=1, n
         IF (y(i).GT.0) THEN
            w(i) = 1/(DSQRT(y(i)))
         ELSE
            WRITE(*,*) 'Change your simulation file.
     +   All data must be positive'
         ENDIF
      ENDDO
      WRITE(*,*) 'File to interpolate read'
      WRITE(*,*) 'Simulation count sum = ', sum


c     Make the interpolation
c      write(*,*) fileinterp_name, sum, n, x(1),y(1),w(1)
c      pause

      CALL CURFIT(iopt,n,x,y,w,x(1),x(n),k,s,nest,nn,
     +     t,c,fp,wrk,lwrk,iwrk,ier)

      WRITE(*,*) 'Interpolation error code: ', ier
      WRITE(*,*) 'Number of knots = ', nn


c     For checking, write the interpolated data
      OPEN(20,file=interpolated_name,status='unknown')
      DO i = 1, 1000
         xp = x(1) + (x(n)-x(1))/1000*(i-1)
         CALL SPLEV(t,nn,c,k,xp,yp,1,1,ier)
         WRITE(20,*) xp, yp
      ENDDO
      CLOSE(20)

      WRITE(*,*) '##### End interpolation file ####'
      WRITE(*,*) fileinterp_name

c     ####################################################################

c     Spline normalization
      c = c/sum

      END


c     ##################################################################################################################################################


      SUBROUTINE INIT_ROCKING(s_s,s_p)
c     Subroutine for the preliminary interpolation of the simulated spectra.
c     Because of the statistical noise, the spectra are fitted with as weight, the expected statistical fluctiation
c     To note, the simulated spectra must have positive count number for all channels
      IMPLICIT NONE
c     Simulation variables
      CHARACTER*64 fileinterpname_s, fileinterpname_p
      CHARACTER*64 interpolated_s, interpolated_p
c     Interpolation subroutine variables
      INTEGER*4 k, nest, nn_s, nn_p
      REAL*8  s_s, s_p
      PARAMETER (nest=1000)
c     s_s=90,s_p=60
      REAL*8 t_s(nest), c_s(nest), t_p(nest), c_p(nest)
      COMMON /rocking/ t_s, t_p, c_s, c_p, k, nn_s, nn_p


c     Initialize the variables
      k = 3
      fileinterpname_s = 'rocking_curve_s.dat'
      fileinterpname_p = 'rocking_curve_p.dat'
      interpolated_s   = 'interpolated_s.dat'
      interpolated_p   = 'interpolated_p.dat'


      WRITE(*,*) ' '
      WRITE(*,*) '##### Initialization rocking curve profile ######'

c     ############################################ S POLARIZATION #################################################
c     Read s polarized rocking curve

      CALL  INTERPOLATE(fileinterpname_s,
     +     interpolated_s,s_s,t_s,c_s,k,nn_s)

c     ############################################ P POLARIZATION #################################################
c     Read p polarized rocking curve

      CALL  INTERPOLATE(fileinterpname_p,
     +     interpolated_p,s_p,t_p,c_p,k,nn_p)


      END

c     ####################################################################################################################################



c     ####################################################################################################################################
      SUBROUTINE INIT_ROCKING_SET(s_s_par,s_p_par,s_s_apar,s_p_apar)
c     Subroutine for the preliminary interpolation of the simulated spectra for the analaysis of
c     parallel and antiparallel configurations at the same time
c     Because of the statistical noise, the spectra are fitted with as weight, the expected statistical fluctiation
c     To note, the simulated spectra must have positive count number for all channels
      IMPLICIT NONE
c     Simulation variables
      CHARACTER*64 fileinterpname_s_par, fileinterpname_p_par
      CHARACTER*64 fileinterpname_s_apar, fileinterpname_p_apar
      CHARACTER*64 interpolated_s_par, interpolated_p_par
      CHARACTER*64 interpolated_s_apar, interpolated_p_apar
      !INTEGER*4 i, interpmax, n_s_par, n_p_par, n_s_apar, n_p_apar
c     Interpolation subroutine var
c     Interpolation subroutine variables
      INTEGER*4 k, nest
      INTEGER*4 nn_s_par, nn_p_par, nn_s_apar, nn_p_apar
      REAL*8 s_s_par, s_p_par, s_s_apar, s_p_apar
      PARAMETER (nest=1000)
c     s_s=90,s_p=60
      REAL*8 t_s_par(nest), c_s_par(nest)
      REAL*8 t_p_par(nest), c_p_par(nest)
      REAL*8 t_s_apar(nest), c_s_apar(nest)
      REAL*8 t_p_apar(nest), c_p_apar(nest)
      COMMON /rocking_set/ t_s_par, t_p_par, t_s_apar, t_p_apar,
     +     c_s_par, c_p_par, c_s_apar, c_p_apar,k,
     +     nn_s_par, nn_p_par, nn_s_apar, nn_p_apar


c     Initialize the variables
      k = 3
      fileinterpname_s_par  = 'rocking_curve_s_par.dat'
      fileinterpname_p_par  = 'rocking_curve_p_par.dat'
      fileinterpname_s_apar = 'rocking_curve_s_apar.dat'
      fileinterpname_p_apar = 'rocking_curve_p_apar.dat'
      interpolated_s_par    = 'interpolated_s_par.dat'
      interpolated_p_par    = 'interpolated_p_par.dat'
      interpolated_s_apar   = 'interpolated_s_apar.dat'
      interpolated_p_apar   = 'interpolated_p_apar.dat'


      WRITE(*,*) ' '
      WRITE(*,*) '##### Initialization rocking curve profile ######'

c     ############################################ S POLARIZATION  PARALLEL CONFIGURATION #################################################
c     Read s polarized rocking curve parallel configuration

      CALL  INTERPOLATE(fileinterpname_s_par,
     +     interpolated_s_par,s_s_par,t_s_par,c_s_par,k,nn_s_par)


c     ############################################ P POLARIZATION  PARALLEL CONFIGURATION #################################################
c     Read p polarized rocking curve parallel configuration

      CALL  INTERPOLATE(fileinterpname_p_par,
     +     interpolated_p_par,s_p_par,t_p_par,c_p_par,k,nn_p_par)

c     ############################################ S POLARIZATION  ANTIPARALLEL CONFIGURATION #################################################
c     Read s polarized rocking curve parallel configuration

      CALL  INTERPOLATE(fileinterpname_s_apar,
     +     interpolated_s_apar,s_s_apar,t_s_apar,c_s_apar,k,nn_s_apar)

c     ############################################ P POLARIZATION  ANTIPARALLEL CONFIGURATION #################################################
c     Read p polarized rocking curve parallel configuration

      CALL  INTERPOLATE(fileinterpname_p_apar,
     +     interpolated_p_apar,s_p_apar,t_p_apar,c_p_apar,k,nn_p_apar)

c      ########################################################################################################################################
      WRITE(*,*) 'End of interpolations'

      END





c     ####################################################################################################################################
      SUBROUTINE INIT_TWO_INTERP(s_1,s_2)
c     Subroutine for the preliminary interpolation of the simulated spectra with two different profiles
c     Because of the statistical noise, the spectra are fitted with as weight, the expected statistical fluctiation
c     To note, the simulated spectra must have positive count number for all channels
      IMPLICIT NONE
c     Simulation variables
      CHARACTER*64 fileinterpname_1, fileinterpname_2
      CHARACTER*64 interpolated_1, interpolated_2
c     Interpolation subroutine variables
      INTEGER*4 k, nest, nn_1, nn_2
      REAL*8  s_1, s_2
      PARAMETER (nest=1000)
c     s_s=90,s_p=60
      REAL*8 t_1(nest), c_1(nest), t_2(nest), c_2(nest)
      COMMON /two_interp/ t_1, t_2, c_1, c_2, k, nn_1, nn_2


c     Initialize the variables
      k = 3
      fileinterpname_1 = 'to_interpolate_1.dat'
      fileinterpname_2 = 'to_interpolate_2.dat'
      interpolated_1   = 'interpolated_1.dat'
      interpolated_2   = 'interpolated_2.dat'


      WRITE(*,*) ' '
      WRITE(*,*) '##### Initialization of the two interpolation ######'

c     ############################################ 1st peak #################################################

      CALL  INTERPOLATE(fileinterpname_1,
     +     interpolated_1,s_1,t_1,c_1,k,nn_1)

c     ############################################ 2nd peak #################################################

      CALL  INTERPOLATE(fileinterpname_2,
     +     interpolated_2,s_2,t_2,c_2,k,nn_2)

c      ########################################################################################################################################
      WRITE(*,*) '########### End of interpolations ###########'


      END

c     ####################################################################################################################################


c     ####################################################################################################################################
      SUBROUTINE INIT_THREE_INTERP(s_1,s_2,s_3)
c     Subroutine for the preliminary interpolation of the simulated spectra with two different profiles
c     Because of the statistical noise, the spectra are fitted with as weight, the expected statistical fluctiation
c     To note, the simulated spectra must have positive count number for all channels
      IMPLICIT NONE
c     Simulation variables
      CHARACTER*64 fileinterpname_1, fileinterpname_2, fileinterpname_3
      CHARACTER*64 interpolated_1, interpolated_2, interpolated_3
c     Interpolation subroutine variables
      INTEGER*4 k, nest, nn_1, nn_2, nn_3
      REAL*8  s_1, s_2, s_3
      PARAMETER (nest=1000)
c     s_s=90,s_p=60
      REAL*8 t_1(nest),c_1(nest),t_2(nest),c_2(nest),t_3(nest),c_3(nest)
      COMMON /three_interp/ t_1,t_2,t_3,c_1,c_2,c_3,k,nn_1,nn_2,nn_3


c     Initialize the variables
      k = 3
      fileinterpname_1 = 'to_interpolate_1.dat'
      fileinterpname_2 = 'to_interpolate_2.dat'
      fileinterpname_3 = 'to_interpolate_3.dat'
      interpolated_1   = 'interpolated_1.dat'
      interpolated_2   = 'interpolated_2.dat'
      interpolated_3   = 'interpolated_3.dat'


      WRITE(*,*) ' '
      WRITE(*,*) '##### Initialization of the two interpolation ######'

c     ############################################ 1st peak #################################################

      CALL  INTERPOLATE(fileinterpname_1,
     +     interpolated_1,s_1,t_1,c_1,k,nn_1)

c     ############################################ 2nd peak #################################################

      CALL  INTERPOLATE(fileinterpname_2,
     +     interpolated_2,s_2,t_2,c_2,k,nn_2)

c     ############################################ 3th peak #################################################

      CALL  INTERPOLATE(fileinterpname_3,
     +     interpolated_3,s_3,t_3,c_3,k,nn_3)

c      ########################################################################################################################################
      WRITE(*,*) '########### End of interpolations ###########'


      END

c     ####################################################################################################################################
