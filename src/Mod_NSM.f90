MODULE MOD_NSM

! INCLUDES FUNCTIONS FOR NUCPHYS. 

IMPLICIT NONE

!CHARACTER  :: namefile*64 ='f72.dat', np_yn*1, string*64
!REAL(8) :: J_st = 3.5

INTEGER(4) :: nentries=1000,  n2b, n3b, junk_size=5
REAL(8) :: J_st
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: me_2b, me_3b
REAL(8), DIMENSION(1000) :: nuc=-1, Jays=-1, length=-1, element=-1, sp=-1
CHARACTER  :: namefile*64




CONTAINS

	
   SUBROUTINE INIT_NSM()
   !nuc,J,length,element,sp,me_3b,me_2b,J_st,n2b,n3b
   INTEGER(4) :: junk_size=5, i
   CHARACTER ::  string*64
   
   
   ! Reads the data from the matrix elements file
   
      !namefile = 'f72.dat'
      !J_st = 3.5
      !n2b  = 4
      !n3b  = 6
      
      
      OPEN(49, file = 'file_selection.txt',status='old') 
      READ(49,*) namefile, string
      READ(49,*) J_st, string
      READ(49,*) n2b, string
      READ(49,*) n3b,string
      CLOSE(49)
      
      
      
      WRITE(*,*) 'File used ', namefile
      WRITE (*,*) 'J of the ground state ',J_st
      WRITE (*,*) '# of 2/3 body m.e. :',n2b, n3b
   
      ALLOCATE(me_2b(n2b,nentries), me_3b(n3b,nentries))
      
      OPEN(2, file = namefile) 
      READ(2,*) string
      
      DO i=1, nentries
      !nuc(i), Jays(i), length(i), element(i), sp(i),
         READ(2,*, END=45)  nuc(i), Jays(i),length(i),element(i), sp(i),me_3b(:,i), me_2b(:,i)
      END DO 
      45 CLOSE (2)
      write(*,*) 'After reading'
                 

   END SUBROUTINE INIT_NSM
   
   !_______________________________________________________________________
   
   REAL(8) FUNCTION NSM_SN(X,npar,val)

      IMPLICIT NONE
      INTEGER(4) :: npar
      REAL(8) :: val(npar)
      REAL(8) ::  X(3)
      REAL(8) :: Jinp, ltemp_gs
      INTEGER(4) :: N, k, l, i,loc , cnt, col, el, ier
      INTEGER(4) :: loc_gs, lgth_gs, siz_gs, lgth, compar
      INTEGER(4) :: nentries=1000, n2b, n3b, junk_size=5, siz
      REAL(8), ALLOCATABLE, DIMENSION(:) ::  lin_m, eig, iperm
      REAL(8), ALLOCATABLE, DIMENSION(:) ::  iperm_gs, lin_m_gs, eig_gs
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: matrix, matrix_gs
      REAL(8) :: ex, me_stmp, me_tmp, val_tmp, ltemp, rem, J_st
      REAL(8), DIMENSION(2) :: sort_ex
      
      N    = X(1) 
      Jinp = X(2)
      el   = X(3)
      
      
      !Checking positions:

      !of the GS
      DO i=1, nentries
         IF (nuc(i).EQ.N) THEN
            !write(*,*) MOD(N,2)
            IF (MOD(N,2)==0.) THEN
               IF (Jays(i)==0) THEN
                  loc_gs = i
                  lgth_gs = length(i)
                  ltemp_gs = real(lgth_gs)
                  siz_gs = int((sqrt(1.+8*ltemp_gs)-1)/2)
                  !write(*,*) N, J(i),lgth_gs, ltemp_gs, siz_gs
                  EXIT
               END IF
            ELSE
               IF (Jays(i)==J_st) THEN
                  loc_gs = i
                  lgth_gs = length(i)
                  ltemp_gs = real(lgth_gs)
                  siz_gs = int((sqrt(1.+8*ltemp_gs)-1)/2)
                  !write(*,*) N, J(i),lgth_gs, ltemp_gs, siz_gs
                  EXIT
               END IF
            END IF
         END IF 
      END DO
      
!write(*,*) N, Jinp, lgth_gs, ltemp_gs, siz_gs


      !of the state
      DO i=1, nentries
         IF (nuc(i).EQ.N) THEN
            IF (Jays(i).EQ.Jinp) THEN
               loc = i 
               lgth  = length(i)
               ltemp = real(lgth)
               siz = int((sqrt(1.+8*ltemp)-1)/2)
               !write(*,*) N, J(i),lgth_gs, ltemp_gs, siz_gs
               EXIT 
            END IF
         END IF 
      END DO
      
      
      
      ALLOCATE(matrix(siz,siz), lin_m(lgth),eig(siz))
      ALLOCATE(matrix_gs(siz_gs,siz_gs), lin_m_gs(lgth_gs),eig_gs(siz_gs))
      
      
      DO l=1, lgth_gs
         me_tmp = 0
         DO k=1, n2b
            val_tmp = val(k)
            me_stmp= me_2b(k,loc_gs + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
            !write(*,*) 'gf =', me_stmp, k
         END DO
         DO k=1, n3b
            val_tmp = val(n2b+k)
            me_stmp= me_3b(k,loc_gs + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
            !write(*,*) 'gf=', me_stmp, k 
         END DO 
         lin_m_gs(l)  = me_tmp
      END DO


      cnt = 1
      col = 1
      DO i = 1, siz_gs
         DO l = col, siz_gs
            matrix_gs(i,l) = lin_m_gs(cnt)
            matrix_gs(l,i) = lin_m_gs(cnt)
            cnt = cnt +1
         END DO
         col = col + 1
      END DO


     ! Locating excited  state
      DO l=1, lgth
         me_tmp = 0
         DO k=1, n2b
            val_tmp = val(k)
            me_stmp= me_2b(k,loc + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
         END DO
         DO k=1, n3b
            val_tmp = val(n2b+k)
            me_stmp= me_3b(k,loc + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
         END DO 
         lin_m(l)  = me_tmp
      END DO


      cnt = 1
      col = 1
      DO i = 1, siz
         DO l = col, siz
             matrix(i,l) = lin_m(cnt)
             matrix(l,i) = lin_m(cnt)
             cnt = cnt +1
         END DO
         col = col + 1
      END DO



     !Symmetrizing matrix
     !write(*,*) siz
     call diasym(matrix,eig,siz)

     !write(*,*) matrix, siz
     call diasym(matrix_gs,eig_gs,siz_gs)

     


     !Ordering energies

     ALLOCATE(iperm(siz),iperm_gs(siz_gs))


     CALL DPSORT(eig,siz,iperm,2,ier)

     CALL DPSORT(eig_gs,siz_gs,iperm_gs,2,ier)



     !Calculating requested energy
      NSM_SN = eig(el)-eig_gs(1) 


      RETURN
      END 
      
      
      !_______________________________________________________________
      
      REAL(8) FUNCTION NSM_SN_BE(X,npar,val)

      IMPLICIT NONE
      INTEGER(4) :: npar
      REAL(8) :: val(npar)
      REAL(8) :: X(3)
      REAL(8) :: Jinp, ltemp_gs, e_sp
      INTEGER(4) :: N, k, l, i,loc , cnt, col, el, ier
      INTEGER(4) :: loc_gs, lgth_gs, siz_gs, lgth, compar
      INTEGER(4) :: nentries=1000, junk_size=5, siz
      REAL(8), ALLOCATABLE, DIMENSION(:) ::  lin_m, eig, iperm
      REAL(8), ALLOCATABLE, DIMENSION(:) ::  iperm_gs, lin_m_gs, eig_gs
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: matrix, matrix_gs
      REAL(8) :: ex, me_stmp, me_tmp, val_tmp, ltemp, rem, J_st
      REAL(8), DIMENSION(2) :: sort_ex


      !INPUTING THE NUCLEUS AND STATE
      N    = X(1) 
      Jinp = X(2)
      el   = X(3)
      

      

      !Checking positions:

      !of the state
      DO i=1, nentries
         IF (nuc(i).EQ.N) THEN
            IF (Jays(i).EQ.Jinp) THEN
               loc = i 
               lgth  = length(i)
               ltemp = real(lgth)
               siz = int((sqrt(1.+8*ltemp)-1)/2)
               !write(*,*) N, J(i),lgth_gs, ltemp_gs, siz_gs
               EXIT 
            END IF
         END IF 
      END DO

!write(*,*) N, Jinp, lgth, ltemp, siz


      !ALLOCATE(val(n2b+n3b))



      !Assigning values to val


      ! UNCOMMENT FOR TRIAL
      !DO i=1, 2
      !   val(i) = 1.
      !END DO

      !DO i=3, 7
      !   val(i) = 3.
      !END DO
      !DO i=8, n2b + n3b
      !   val(i) = 4.
      !END DO


      ! UNCOMMENT FOR FUNCTION. f7/2 Shell 

      !val(1) = v0
      !val(2) = v2
      !val(3) = v4
      !val(4) = v6
      !val(5) = v3
      !val(6) = v5
      !val(7) = v7
      !val(8) = v9
      !val(9) = v11
      !val(10)= v15




      ALLOCATE(matrix(siz,siz), lin_m(lgth),eig(siz))


     ! Locating excited  state
      DO l=1, lgth
         me_tmp = 0
         DO k=1, n2b
            val_tmp = val(k)
            me_stmp= me_2b(k,loc + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
         END DO
         DO k=1, n3b
            val_tmp = val(n2b+k)
            me_stmp= me_3b(k,loc + l -1)
            me_tmp = me_tmp + val_tmp*me_stmp
         END DO 
         lin_m(l)  = me_tmp
      END DO


      cnt = 1
      col = 1
      DO i = 1, siz
         DO l = col, siz
             matrix(i,l) = lin_m(cnt)
             matrix(l,i) = lin_m(cnt)
             cnt = cnt +1
         END DO
         col = col + 1
      END DO



     !Symmetrizing matrix
     !write(*,*) siz
     call diasym(matrix,eig,siz)
     


     !Ordering energies

     ALLOCATE(iperm(siz))


     CALL DPSORT(eig,siz,iperm,2,ier)

     !Adding single particle energies
      e_sp = val(n2b+n3b+1)
      


     !Calculating requested energy
      NSM_SN_BE = eig(el) + N*e_sp


      RETURN
      END 
   
   
   



! ######################################################################
END MODULE MOD_NSM

