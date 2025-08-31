      function rinteg (f,na,nb,nq,h)
c
c  this program calculates the integral of the function f from point na
c  to point nb using a nq points quadrature ( nq is any integer between
c  1 and 14 ).  h is the grid size.
c     written by c. c. j. roothaan

c     Received from Paul Indelicato on 26/11/2017 --------------------------------------
c     "Ce code a été écrit par Roothan, un des historiques des calculs Hartree-Fock.
c     Il l’a donné à Yong-Ki Kim, qui a fait sa thèse avec lui à Chicago...
c     il n’y a probablement aucun droit d’aucune sorte...
c     Yong-Ki était au NIST et à été renversé par un chauffard en Alaska il y a 7-8 ans"
c     ----------------------------------------------------------------------------------
c
      implicit doubleprecision(a-h,o-z)
      dimension c(105),c1(25),c2(80),d(14),f(nb)
      equivalence (c1(1),c(1)),(c2(1),c(26))
      data c1/1.d0,2.d0,1.d0,23.d0,28.d0,9.d0,25.d0,20.d0,31.d0,8.d0,
     &1413.d0,1586.d0,1104.d0,1902.d0,475.d0,1456.d0,1333.d0,1746.d0,
     &944.d0,1982.d0,459.d0,119585.d0,130936.d0,89437.d0,177984.d0/
      data c2/54851.d0,176648.d0,36799.d0,122175.d0,111080.d0,156451.d0,
     &46912.d0,220509.d0,29336.d0,185153.d0,35584.d0,7200319.d0,
     &7783754.d0,5095890.d0,12489922.d0,-1020160.d0,16263486.d0,
     &261166.d0,11532470.d0,2082753.d0,7305728.d0,6767167.d0,9516362.d0,
     &1053138.d0,18554050.d0,-7084288.d0,20306238.d0,-1471442.d0,
     &11965622.d0,2034625.d0,952327935.d0,1021256716.d0,636547389.d0,
     &1942518504.d0,-1065220914.d0,3897945600.d0,-2145575886.d0,
     &3373884696.d0,-454944189.d0,1637546484.d0,262747265.d0,
     &963053825.d0,896771060.d0,1299041091.d0,-196805736.d0,
     &3609224754.d0,-3398609664.d0,6231334350.d0,-3812282136.d0,
     &4207237821.d0,-732728564.d0,1693103359.d0,257696640.d0,
     &5206230892907.d0,5551687979302.d0,3283609164916.d0,
     &12465244770050.d0,-13155015007785.d0,39022895874876.d0,
     &-41078125154304.d0,53315213499588.d0,-32865015189975.d0,
     &28323664941310.d0,-5605325192308.d0,9535909891802.d0,
     &1382741929621.d0,5252701747968.d0,4920175305323.d0,
     &7268021504806.d0,-3009613761932.d0,28198302087170.d0,
     &-41474518178601.d0,76782233435964.d0,-78837462715392.d0,
     &81634716670404.d0,-48598072507095.d0,34616887868158.d0,
     &-7321658717812.d0,9821965479386.d0,1360737653653.d0/
      data d/2.d0,2.d0,24.d0,24.d0,1440.d0,1440.d0,120960.d0,120960.d0,
     &7257600.d0,7257600.d0,958003200.d0,958003200.d0,5230697472000.d0,
     &5230697472000.d0/
      a=0.d0
c
c      write(*,*) a
c      pause
c
      l=na
      m=nb
      i=nq*(nq+1)/2
c
c      write(*,*) l,m,i
c      pause
c
      do 1 j=1,nq
      a=a+c(i)*(f(l)+f(m))
c
c      write(*,*) nq,a,c(i), f(l), f(m)
c      pause
c
      l=l+1
      m=m-1
    1 i=i-1
      a=a/d(nq)
c
c      write(*,*) a,d(nq),nq
c      pause
c
      do 2 n=l,m
    2 a=a+f(n)
      rinteg=a*h
c
c      write(*,*) a,h
c      pause
c
      return
      end
