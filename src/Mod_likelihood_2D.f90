MODULE MOD_LIKELIHOOD
  ! Automatic Time-stamp: <Last changed by martino on Tuesday 07 April 2020 at CEST 18:34:18>
  ! Module of the likelihood function for data analysis


  !#####################################################################################################################

  ! IMPORTANT: to switch between likelihood types for test and others,
  ! change the name of the file to compile in the Makefile,
  ! Mod_likeihood.f90 or Mod_likeihood_tests.f90


  !#####################################################################################################################


  ! Module for the input parameter definition
  USE MOD_PARAMETERS
  !ONLY: maxdata, nsetmax, filename, errorbars_yn, set_yn, nset, funcname, &
  !     xmin, xmax, npar, par_in

  IMPLICIT NONE

  ! Data variables
  INTEGER(4) :: ndata
  INTEGER(4), DIMENSION(nsetmax) :: ndata_set=0
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: x, y, nc, nc_err ! 2D: added y
  ! Likelihood variables
  REAL(8) :: const_ll = 0.

CONTAINS
        
  SUBROUTINE INIT_LIKELIHOOD()
    ! Initialize the normal likelihood with data files and special function

    ! Read data
    ! ------------------------------------------------------------------------------------------------------------------------
    CALL READ_DATA()

    ! Initialize functions
    !CALL INIT_FUNCTIONS()

  END SUBROUTINE INIT_LIKELIHOOD 

  !#####################################################################################################################

  SUBROUTINE READ_DATA()
    ! Subroutine to read data files
    INTEGER(4) :: k=0
    REAL(8), DIMENSION(maxdata,nsetmax) :: x_tmp=0, y_tmp=0, nc_tmp=0, nc_err_tmp=0 ! 2D:added y_tmp=0

    ! READ DATA, calculate the constants for the likelihood function
    ! Initialize
    ndata = 0
    ndata_set = 0
    const_ll = 0.

    IF (errorbars_yn.EQ.'n'.OR.errorbars_yn.EQ.'N') THEN
       ! Case with counts ----------------------------------------------------------------
       DO k=1,nset
          CALL READ_FILE_COUNTS(filename(k),xmin(k),xmax(k),ymin(k),ymax(k),ndata_set(k), &
               x_tmp(:,k),y_tmp(:,k),nc_tmp(:,k))

          WRITE(*,*) 'Number of file = ', k, ' of ', nset
          WRITE(*,*) 'Read data file ', TRIM(filename(k))
          WRITE(*,*) 'ndata = ', ndata_set(k)
          WRITE(*,*) 'constant in evidence calc. = ', const_ll
       END DO
       ! Allocate set of data
       ndata = MAXVAL(ndata_set)
       ALLOCATE(x(ndata,nset),y(ndata,nset),nc(ndata,nset))
       x = 0.
       y = 0.
       nc = 0.
       DO k=1,nset
          x(1:ndata_set(k),k)  = x_tmp(1:ndata_set(k),k)
          y(1:ndata_set(k),k)  = y_tmp(1:ndata_set(k),k)
          nc(1:ndata_set(k),k) = nc_tmp(1:ndata_set(k),k)
       END DO
    ELSE
       ! Case with errorbars -------------------------------------------------------------------
       DO k=1,nset
          CALL READ_FILE_ERRORBARS(filename(k),xmin(k),xmax(k),ymin(k),ymax(k),ndata_set(k), &
               x_tmp(:,k),y_tmp(:,k),nc_tmp(:,k),nc_err_tmp(:,k))  ! 2D: added y
          WRITE(*,*) 'Number of file = ', k, ' of ', nset
          WRITE(*,*) 'Data file', filename(k), ' read'
          WRITE(*,*) 'ndata = ', ndata_set(k)
          WRITE(*,*) 'constant in evidence calc. = ', const_ll
       END DO
       ! Allocate set of data
       ndata = MAXVAL(ndata_set)
       ALLOCATE(x(ndata,nset),y(ndata,nset),nc(ndata,nset),nc_err(ndata,nset))  ! 2D: added y
       x = 0.
       y = 0. ! 2D: added y
       nc = 0.
       nc_err = 0.
       DO k=1,nset
          x(1:ndata_set(k),k)  = x_tmp(1:ndata_set(k),k)
          y(1:ndata_set(k),k)  = y_tmp(1:ndata_set(k),k)    ! 2D: added y
          nc(1:ndata_set(k),k) = nc_tmp(1:ndata_set(k),k)
          nc_err(1:ndata_set(k),k) = nc_err_tmp(1:ndata_set(k),k)
       END DO
    END IF


  END SUBROUTINE READ_DATA

  !#####################################################################################################################

  SUBROUTINE READ_FILE_COUNTS(namefile,minx,maxx,miny,maxy,datan,x_tmp,y_tmp,nc_tmp)
    ! Read one file of data
    CHARACTER, INTENT(IN) :: namefile*64
    REAL(8), INTENT(IN) :: minx, maxx, miny, maxy
    INTEGER(4), INTENT(OUT) :: datan
    REAL(8), DIMENSION(maxdata), INTENT(OUT) :: x_tmp, y_tmp, nc_tmp
    !
    INTEGER(4) :: i=0, nd=0
    REAL(8), DIMENSION(maxdata) :: x_raw=0, y_raw, nc_raw=0
    REAL(8) :: DLOG_FAC

    ! Initialize
    x_tmp = 0.
    y_tmp = 0.
    nc_tmp = 0.
    datan = 0
    nd=0

    ! Open file and read
    OPEN(10,file=namefile,status='old')
    DO i=1, maxdata
       READ(10,*,END=20) x_raw(i), y_raw(i), nc_raw(i)
       ! Make test for integer numbers
       IF (ABS(nc_raw(i)-INT(nc_raw(i))).GT.1E-5) THEN
          WRITE(*,*) 'Attention, input numbers are not counts and you are using Poisson statistic (no error bar)'
          WRITE(*,*) 'n. counts = ', nc_raw(i)
          WRITE(*,*) 'Change something!'
          STOP
       END IF
       ! Select the data
       IF(x_raw(i).GE.minx.AND.x_raw(i).LE.maxx.AND.y_raw(i).GE.miny.AND.y_raw(i).LE.maxy) THEN
          nd = nd + 1
          x_tmp(nd) = x_raw(i)
          y_tmp(nd) = y_raw(i)
          nc_tmp(nd) = nc_raw(i)
          ! Calculation of the constant part of the likelihood with Poisson distribution
          !IF (nc_tmp(nd).GT.0.) const_ll = const_ll - DFAC_LN(INT(nc_tmp(nd)))
          ! Uses of the gamma function gamma(n) = (n-1)!
          IF (nc_tmp(nd).GT.0.) const_ll = const_ll - DLOG_FAC(INT(nc_tmp(nd)))
          !write(*,*) nc_tmp(nd), const_ll
       END IF
    ENDDO

20  CONTINUE
    CLOSE(10)
    datan = nd

  END SUBROUTINE READ_FILE_COUNTS


  !#####################################################################################################################

  SUBROUTINE READ_FILE_ERRORBARS(namefile,minx,maxx,miny,maxy,datan,x_tmp,y_tmp,nc_tmp,nc_err_tmp)   ! 2D: added y
    ! Read one file of data
    CHARACTER, INTENT(IN) :: namefile*64
    REAL(8), INTENT(IN) :: minx, maxx, miny, maxy ! 2D: Added y
    INTEGER(4), INTENT(OUT) :: datan
    REAL(8), DIMENSION(maxdata), INTENT(OUT) :: x_tmp, y_tmp, nc_tmp, nc_err_tmp  ! 2D: added y
    !
    INTEGER(4) :: i=0, nd =0
    REAL(8), DIMENSION(maxdata) :: x_raw=0, y_raw=0, nc_raw=0, nc_err_raw=0  ! 2D: added y
    REAL(8), PARAMETER :: pi=3.141592653589793d0
    

    ! Initialize
    x_tmp = 0.
    y_tmp = 0.  ! 2D: added y
    nc_tmp = 0.
    nc_err_tmp = 0.
    nd = 0

    ! Open file and read
    OPEN(10,file=namefile,status='old')
    DO i=1, maxdata
       READ(10,*,END=20) x_raw(i), y_raw(i), nc_raw(i), nc_err_raw(i)  ! 2D: added y
       ! Select the data
       IF(x_raw(i).GE.minx.AND.x_raw(i).LE.maxx.AND.y_raw(i).GE.miny.AND.y_raw(i).LE.maxy) THEN       ! 2D: added y
          nd = nd + 1
          x_tmp(nd) = x_raw(i)
          y_tmp(nd) = y_raw(i)    ! 2D added y
          nc_tmp(nd) = nc_raw(i)
          nc_err_tmp(nd) = nc_err_raw(i)
          IF (nc_err_tmp(nd).LE.0.) THEN
             WRITE(*,*) 'Errorbars with a value equal to 0 or negative'
             WRITE(*,*) 'Check our data or do not use errorbars'
             STOP
          END IF
       END IF
    ENDDO


20  CONTINUE
    CLOSE(10)
    ! Calculation of the constant part of the likelihood with Gaussian distribution
    const_ll = -nd*DLOG(2*pi)/2
    datan = nd 


  END SUBROUTINE READ_FILE_ERRORBARS

  !#####################################################################################################################




  FUNCTION LOGLIKELIHOOD(par)

    ! Main likelihood function
    ! Type: Poisson , Gaussian , .... soon 2D I hope. It's happening!!!!

    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    !
    REAL(8) :: LOGLIKELIHOOD, USERFCN_2D, USERFCN_SET  !2D: USERFCN--> 2D
    REAL(8) :: ll_tmp, enc
    INTEGER(4) :: i=0, k=0


    ! Calculate LIKELIHOOD
    ll_tmp = 0.

    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       ! No set --------------------------------------------------------------------------------------------------------
       k=1
       IF (errorbars_yn.EQ.'n'.OR.errorbars_yn.EQ.'N') THEN
          
       ELSE
          !$OMP PARALLEL DO PRIVATE(i,enc) REDUCTION(+:ll_tmp)
          DO i=1, ndata_set(k)
             ! Normal (Gaussian) distribution calculation --------------------------------------
             enc = USERFCN_2D(x(i,k),y(i,k),npar,par,funcname)      !
             ll_tmp = ll_tmp - (nc(i,k) - enc)**2/(2*nc_err(i,k)**2)
          ENDDO
          !$OMP END PARALLEL DO
       END IF
    ELSE
       
    END IF
    !
    ! Sum all together
    LOGLIKELIHOOD = ll_tmp + const_ll


  END FUNCTION LOGLIKELIHOOD

  !#####################################################################################################################

  SUBROUTINE FINAL_LIKELIHOOD(live_max,par_mean,par_median_w)
    ! Final action for the likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w

    ! Write auxiliar files for plots
    CALL WRITE_EXPECTED_VALUES(live_max,par_mean,par_median_w)

    ! Deallocate variables
    CALL DEALLOCATE_DATA()

  END SUBROUTINE FINAL_LIKELIHOOD


  !#####################################################################################################################
  SUBROUTINE WRITE_EXPECTED_VALUES(live_max,par_mean,par_median_w)
    ! Write all auxiliar files for plots and co.

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w
    !
    INTEGER(8), PARAMETER :: maxfit = 100
    REAL(8) :: USERFCN_2D, USERFCN_SET
    REAL(8) :: minx=0., maxx=0., xfit=0., dx=0.,miny=0., maxy=0., yfit=0., dy=0.,  enc = 0. !2Dadded y
    INTEGER(4) :: i=0, j=0, k=0
    ! Stuff to save separate components
    LOGICAL :: plot = .false.
    REAL(8) :: zfit
    CHARACTER :: out_filename*64

    COMMON /func_plot/ plot

    !-----------------------Calculate the expected function values -------------------------

    ! Calculate the expected function value and residual for max likelihood
    ! New from version 1.0: save three pairs of files with values from max likelihood, mean and median of the parameters
    !IF(arg.EQ. ' ') THEN
    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       k=1
       enc = 0.
       ! max likelihood values -------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_max.dat', STATUS='unknown')
       WRITE(20,*)'# x    y    z data    z theory      z diff    z err'
       DO i=1, ndata
          enc = USERFCN_2D(x(i,k),y(i,k),npar,live_max,funcname)
          IF (errorbars_yn.EQ.'y'.OR.errorbars_yn.EQ.'Y') THEN
             WRITE(20,*) x(i,k), ' ',y(i,k),' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', nc_err(i,k)
          ELSE
          END IF
       END DO
       CLOSE(20)
       ! mean values ------------------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_mean.dat', STATUS='unknown')
       WRITE(20,*)'# x    y    z data    z theory      z diff    z err'
       DO i=1, ndata
          enc = USERFCN_2D(x(i,k),y(i,k),npar,par_mean,funcname)       !2D added y
          IF (errorbars_yn.EQ.'y'.OR.errorbars_yn.EQ.'Y') THEN
             WRITE(20,*) x(i,k), ' ',y(i,k),' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', nc_err(i,k)  !2D added y
          ELSE
          END IF
       END DO
       CLOSE(20)
       ! median values ----------------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_median.dat', STATUS='unknown')
       WRITE(20,*)'# x    y    z data    z theory      z diff    z err'
       DO i=1, ndata
          enc = USERFCN_2D(x(i,k),y(i,k),npar,par_median_w,funcname)  !2D added y
          IF (errorbars_yn.EQ.'y'.OR.errorbars_yn.EQ.'Y') THEN
             WRITE(20,*) x(i,k), ' ',y(i,k),' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', nc_err(i,k)   !2D added y
          ELSE
          END IF
       END DO
       CLOSE(20)
       ! -------------------------------------------------------------------------------------
    ELSE
       
    END IF

    ! Save in a file the different fit components !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! New from version 1.0: save three pairs of files with values from max likelihood, mean and median of the parameters
    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       maxx = xmax(1)
       minx = xmin(1)
       maxy = ymax(1)
       miny = ymin(1)   ! 2D added y
       plot = .true.
       xfit = 0.
       yfit = 0.
       dx=(maxx-minx)/(maxfit-1)
       dy=(maxy-miny)/(maxfit-1) 

       ! max likelihood values
       OPEN (UNIT=40, FILE='nf_output_fit_max.dat', STATUS='unknown')
       WRITE(40,*)'# x    y    z fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          DO j=1, maxfit
             yfit = miny + (j-1)*dy
             zfit = USERFCN_2D(xfit,yfit,npar,live_max,funcname)    !2D added z
          ENDDO
       ENDDO
       CLOSE(40)

       ! mean values
       OPEN (UNIT=40, FILE='nf_output_fit_mean.dat', STATUS='unknown')
       WRITE(40,*)'# x    y    z fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          DO j=1, maxfit
             yfit = miny + (j-1)*dy
             zfit = USERFCN_2D(xfit,yfit,npar,par_mean,funcname)
          ENDDO
       ENDDO
       CLOSE(40)

       ! mean values
       OPEN (UNIT=40, FILE='nf_output_fit_median.dat', STATUS='unknown')
       WRITE(40,*)'# x    y    z fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          DO j=1, maxfit
             yfit = miny + (j-1)*dy
             zfit = USERFCN_2D(xfit,yfit,npar,par_median_w,funcname)
          ENDDO
       ENDDO
       CLOSE(40)
    ELSE
       DO k=1, nset
          maxx = xmax(k)
          maxy = ymax(k)
          minx = xmin(k)
          miny = ymin(k)
          plot = .true.
          xfit = 0.
          yfit = 0.
          dx=(maxx-minx)/(maxfit-1)
          dy=(maxy-miny)/(maxfit-1)

          !YET TO FINISH! DO NOT USE SET

          ! max likelihood values
          WRITE(out_filename,1001) 'nf_output_fit_max_',k,'.dat'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          WRITE(40,*)'# x    y fit'
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             DO j=1, maxfit
                yfit = miny + (j-1)*dx
                zfit = USERFCN_SET(xfit,npar,live_max,funcname,k)
             ENDDO    ! 2D NOT COMPLETELY IMPLEMENTED
          ENDDO
          CLOSE(40)

          WRITE(out_filename,2001) 'nf_output_fit_mean_',k,'.dat'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          WRITE(40,*)'# x    y fit'
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,par_mean,funcname,k)
          ENDDO
          CLOSE(40)

          WRITE(out_filename,3001) 'nf_output_fit_median_',k,'.dat'
          WRITE(40,*)'# x    y fit'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,par_median_w,funcname,k)
          ENDDO
          CLOSE(40)
       END DO
    END IF


1000 FORMAT (A19,I1,A4)
1001 FORMAT (A18,I1,A4)
2000 FORMAT (A20,I1,A4)
2001 FORMAT (A19,I1,A4)
3000 FORMAT (A22,I1,A4)
3001 FORMAT (A21,I1,A4)

  END SUBROUTINE WRITE_EXPECTED_VALUES


  !#####################################################################################################################

  SUBROUTINE DEALLOCATE_DATA()

    IF (errorbars_yn.EQ.'n'.OR.errorbars_yn.EQ.'N') THEN
       DEALLOCATE(x,y,nc)
    ELSE
       DEALLOCATE(x,y,nc,nc_err)    ! 2D: Added y
    END IF

  END SUBROUTINE DEALLOCATE_DATA

  !#####################################################################################################################



END MODULE MOD_LIKELIHOOD
