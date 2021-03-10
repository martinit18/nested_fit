c     Automatic Time-stamp: <Last changed by martino on Tuesday 02 April 2019 at CEST 17:19:46>
c################################### USERFCN DEFINITION #####################################

      FUNCTION USERFCN_SET(x,npar,val,funcname,j)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar)
      REAL*8 x, USERFCN_SET
      CHARACTER*64 funcname

 
c     Choose your model (see below for definition)
      IF(funcname.EQ.'GAUSS_BG_SET') THEN
         
      ELSE
         WRITE(*,*) 'Selected function:', funcname
         WRITE(*,*) 'Error in the function name def. in USERFCN_SET'
         WRITE(*,*) 'Check in the manual and in the input.dat file'
         STOP
      END IF

      RETURN
      END

c______________________________________________________________________________________________

 
c ##############################################################################################
