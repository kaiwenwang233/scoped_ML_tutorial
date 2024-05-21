      program OTcorr 
c2012/08/30 npha nline format problem fixed. caused problem in 2008 cc file.
c determine and write origin time correction to cross correlation
c differential time file.
      implicit none
      integer         narguments
      integer         iargc, ifindi, k,k1,k2
      logical ex
      integer i,j,trimlen,maxeve,npha
      character line*150, fn0*80 , fn1*80, fn2*80, fn3*80
      logical fn0ex,fn2ex,fn1ex
      parameter(maxeve=1000000)
      character str5*5,str1*1
      integer acusp_mem(maxeve),acusp(maxeve)
      integer cusp_mem(maxeve),yr_mem(maxeve),mo_mem(maxeve),
     & dy_mem(maxeve),hr_mem(maxeve),mn_mem(maxeve),
     & cusp(maxeve),yr(maxeve),mo(maxeve),dy(maxeve),hr(maxeve), 
     & mn(maxeve)
      real sc(maxeve),sc_mem(maxeve),dot(maxeve)
      integer nev, nev_mem, c1,c2
      integer juliam,take, ii(maxeve)
      real dot1,dot2,maxdot,fdum,ddot,otc


c--- get input parameter file name:
      narguments = iargc()
      if(narguments.lt.4) goto 2
      call getarg(1,fn0)
      inquire(FILE= fn0,exist=ex)
      if(.not. ex) stop' >>> ERROR OPENING INPUT FILE 1.'
      call getarg(2,fn1)
      inquire(FILE= fn1,exist=ex)
      if(.not. ex) stop' >>> ERROR OPENING INPUT FILE 2.'
      call getarg(3,fn2)
      inquire(FILE= fn2,exist=ex)
      if(.not. ex) stop' >>> ERROR OPENING INPUT FILE 3.'
      call getarg(4,fn3)
      goto 3
2     stop
     &'>>phase (.pha) OT (memOT.dat) dt in (dt.cc) dt out (dt.cc.x)'
3     continue

cc--- Get phase file name:
c      write(6,'(a)') 'PHASE FILE [<ret> data.pha]: '
c      read(5,'(a)') fn0
c      if(trimlen(fn0).le.1) then
c         fn0= 'data.pha'            !default input file name
c      else
c         fn0= fn0(1:trimlen(fn0))
c      endif
c      inquire(FILE= fn0,exist=fn0ex)
c      if(.not. fn0ex) then
c          stop' --> ERROR OPENING PHASE FILE.'//
c     &           '  CHECK FILENAME!!'
c      endif
c
cc--- Get mem OT input file name:
c      write(6,'(a)') 'MEM OT INPUT FILE [<ret> memOT.dat]: '
c      read(5,'(a)') fn1
c      if(trimlen(fn1).le.1) then
c         fn1= 'memOT.dat'            !default input file name
c      else
c         fn1= fn1(1:trimlen(fn1))
c      endif
c      inquire(FILE= fn1,exist=fn1ex)
c      if(.not. fn1ex) then
c          stop' --> ERROR OPENING MEM ORIGIN TIME FILE.'//
c     &           '  CHECK FILENAME!!'
c      endif
c
cc--- Get dtime file name:
c      write(6,'(a)') 'DTIME INPUT FILE [<ret> dtime.dat]: '
c      read(5,'(a)') fn2
c      if(trimlen(fn2).le.1) then
c         fn2= 'dtime.dat'            !default input file name
c      else
c         fn2= fn2(1:trimlen(fn2))
c      endif
c      inquire(FILE= fn2,exist=fn2ex)
c      if(.not. fn2ex) then
c          stop' --> ERROR OPENING OUTPUT FILE.'//
c     &           '  CHECK FILENAME!!'
c      endif
c
cc--- Get  corr dtime output file name:
c      write(6,'(a)') 'DTIME OUTPUT FILE [<ret> dtime.dat.corr]: '
c      read(5,'(a)') fn3
c      if(trimlen(fn3).le.1) then
c         fn3= 'dtime.dat.corr'            !default input file name
c      else
c         fn3= fn3(1:trimlen(fn3))
c      endif

c--- open files:
      open(8,file=fn2, status='unknown') 
      open(3,file=fn3, status='unknown') 
      open(5,file='NoPhaEv.dat',status='unknown') 

c--- read event info from phase file: 
      open(2,file=fn0, status='unknown') 
      i= 1
10    read(2,'(a)',end=15)line
      if (line(1:1).eq.'#')  then
         read (line,*,err=1290) str5,yr(i),mo(i),dy(i),hr(i),
     & mn(i),sc(i),
     &   fdum,fdum,fdum,fdum,fdum,fdum,fdum,cusp(i)
         i= i+1
      endif
      goto 10 
15    nev= i-1
      write(*,*)'events = ',nev
      write(66,*)'events = ',nev
      close(2)

c--- read MEM OT file: 
      open(1,file=fn1, status='unknown') 
      i= 1
20    read(1,*,end=25)cusp_mem(i),yr_mem(i),mo_mem(i),
     & dy_mem(i),hr_mem(i), mn_mem(i),sc_mem(i)
c      write(*,*)cusp_mem(i)
      i= i+1 
      goto 20
25    nev_mem= i-1
      write(*,*)'MEM events = ',nev_mem
      write(66,*)'MEM events = ',nev_mem
      close(1)

c--- sort cusp_mem array:
      call indexxi (nev_mem, cusp_mem,ii)
      do i=1,nev_mem
         acusp_mem(i) = cusp_mem(ii(i))
      enddo

c--- write correction array:
      maxdot= 0
      do i=1,nev
         dot(i)= -999	! for all ev not in mem file
         take= 0	! check for multiple listings

         k= ifindi(nev_mem,acusp_mem,cusp(i))
         if(k.eq.0) then
c            write(*,*)'Event not in MEM catalog: ',cusp(i)
            dot(i)= -999
         else

c         do j=1,ev_mem
c            if(cusp_mem(j).eq.cusp(i)) then

               dot(i)= (JULIAM(yr(i),mo(i),dy(i),hr(i),mn(i))
     &  -JULIAM(yr_mem(ii(k)),mo_mem(ii(k)),dy_mem(ii(k)),
     &  hr_mem(ii(k)),mn_mem(ii(k)))) * 60 + (sc(i) - sc_mem(ii(k)))
               if(abs(dot(i)).gt.20) then
c		  write(*,*)'OTC > 20: set to -999:'
c                  write(*,*)cusp(i),yr(i),mo(i),dy(i),hr(i),mn(i),sc(i)
c                  write(*,*)cusp_mem(ii(k)),yr_mem(ii(k)),
c     & mo_mem(ii(k)),dy_mem(ii(k)),
c     &              hr_mem(ii(k)),mn_mem(ii(k)),sc_mem(ii(k))
c                  write(*,*)'OTC = ',dot(i),' -> -999'
c		  write(66,*)'OTC > 10: set to -999:'
c                  write(66,*)cusp(i),yr(i),mo(i),dy(i),hr(i),mn(i),sc(i)
c                  write(66,*)cusp_mem(ii(k)),yr_mem(ii(k)),mo_mem(ii(k)),
c     &              dy_mem(ii(k)),
c     &              hr_mem(ii(k)),mn_mem(ii(k)),sc_mem(ii(k))
c                  write(66,*)'OTC = ',dot(i),' -> -999'
                  dot(i)= -999 
               endif
         endif
c               if(abs(dot(i)).gt.abs(maxdot) .and. dot(i).ne.-999) 
c     &   maxdot= dot(i)
c               take= take+1
c            endif
c         enddo
c         if(take.gt.1) then
c            write(*,*)'NFG: Multiple event in MEM file:',cusp(i)
c            write(66,*)'NFG: Multiple event in MEM file:',cusp(i)
c            stop 
c         endif
c         if(take.eq.0) then 
cc            write(*,*)'Event not in MEM catalog: ',cusp(i)
c            dot(i)= -999
c         endif
      enddo
c      write(*,*)'max OT difference = ',maxdot
c      write(66,*)'max OT difference = ',maxdot

c--- read dtime file:
c--- sort cusp array:
      call indexxi (nev, cusp,ii)
      do i=1,nev
         acusp(i) = cusp(ii(i))
      enddo
200   read(8,'(a)',end=300)line
      if(line(1:1).ne.'#') then
         write(3,'(a)')line(1:trimlen(line))
      else
         read(line,*)str1,c1,c2,otc !,npha
         k1= ifindi(nev,acusp,c1)
         k2= ifindi(nev,acusp,c2)
         if(k1.eq.0) then
c            write(5,*)'Event not in PHA catalog: ',c1
            write(5,*)c1
            ddot= -999
            goto 250 
         endif
         if(k2.eq.0) then
c            write(5,*)'Event not in PHA catalog: ',c2
            write(5,*)c2
            ddot= -999
            goto 250 
         endif
         dot1= dot(ii(k1)) 
         dot2= dot(ii(k2)) 
 	 ddot= dot1-dot2
         if(abs(ddot).gt.999) ddot= -999 
250      continue
c120830         write(3,'(a1,1x,i11,1x,i11,
c120830     &   1x,f10.4,2i9)')'#',c1,c2,ddot,npha,nline
         write(3,'(a1,1x,i11,1x,i11,
     &   1x,f10.4)')'#',c1,c2,ddot !,npha
c     &   1x,f10.4,i9)')'#',c1,c2,ddot !,npha
      endif
      goto 200

c         do i=1,nev
c            if(c1.eq.cusp(i) .and. dot(i).ne.-999) then
c                 dot1= dot(i) 
c                 take= take+1
c            endif
c            if(c2.eq.cusp(i) .and. dot(i).ne.-999) then
c                 dot2= dot(i) 
c                 take= take+1
c            endif
c            if(take.eq.2) goto 270
c         enddo
c         write(666,*)'NFG: no OTC found (event not in MEM/pha list): '
c         if(dot1.eq.-999) write(666,*)c1,take
c         if(dot2.eq.-999) write(666,*)c2,take
c         write(3,'(a2,1x,i11,1x,i11,
c     &    1x,a5)')'# ',c1,c2,' -999'


c      endif
c      goto 200

c270   continue
c      write(3,'(a2,1x,i11,1x,i11,
c     & 1x,f10.4)')'# ',c1,c2,dot1-dot2

c      goto 200

1290  stop'Format error in phase file.'

300   close(8)
      close(3)

      end

      integer function TRIMLEN(t)
c------------------------------------------------------------------
c     Author:  Urs Kradolfer, June 1986
c     Call:    nc=TRIMLEN(char)
c
c          --> nc says, how many characters the input-string has
c              (ignoring trailing blanks!).
c
      implicit none
c
      character t*(*)
      do 1 trimlen=LEN(t),1,-1
    1    if(t(trimlen:trimlen).ne.' ')RETURN
      trimlen=1
      end ! of integer function trimlen
c


      INTEGER FUNCTION JULIAM(IYR,IMO,IDY,IHR,IMN)
C UMRECHNEN VON JAHR-MONAT-TAG-STUNDEN-MINUTEN IN MINUTEN:
C   (WENN IMN 4-BYTE INTEGER, DANN JAHR < 4000)
      implicit none
      integer iyr,imo,idy,ihr,imn
      integer KMO(12)
      integer leap,ky,km,kd,ky4,ky1,ky0,kl,l
      DATA KMO/0,31,59,90,120,151,181,212,243,273,304,334/
      DATA LEAP/1/
      KY= IYR
      KM= IMO
      KD= IDY
      IF(KM.LE.0) KM= 1
10    JULIAM= 365*KY
      KD= KMO(KM)+KD
      KY4= KY/4
      KY1= KY/100
      KY0= KY/1000
      KL= LEAP*(KY4-KY1+KY0)
      L=0
      IF(KY4*4.EQ.KY.AND.(KY1*100.NE.KY.OR.KY0*1000.EQ.KY))L= LEAP
      IF(L.NE.0.AND.KM.LT.3) KL= KL-LEAP
      JULIAM= JULIAM+KD+KL
      JULIAM= JULIAM*24+IHR
      JULIAM= JULIAM*60+IMN
      return
      END ! of integer function juliam
c
      subroutine DATUM(ITF,IYR,IMO,IDY,IHR,IMN)
C UMRECHNEN DES DATUMS IN MINUTEN (CF. JULIAM) IN YR-MO-DY-HR-MI
C   (MIT IMN<2**31, JAHR < 4000
c
      implicit none
      integer iyr,imo,idy,ihr,imn
c      
      integer id,l,iyr4,iyrh,iyrt,ld,i
      integer KMO(12)
      INTEGER ITF,K,KH
      DATA KMO/31,28,31,30,31,30,31,31,30,31,30,31/
      K= ITF/60
      IMN= ITF-K*60
      KH= K/24
      IHR= K-KH*24
      IYR= KH/365
5     ID= KH-IYR*365
      L= 0
      IYR4= IYR/4
      IYRH= IYR/100
      IYRT= IYR/1000
      LD= IYR4-IYRH+IYRT
      IF(IYR4*4.EQ.IYR.AND.(IYRH*100.NE.IYR.OR.IYRT*1000.EQ.IYR)) L= 1
      ID= ID-LD+L
      IF(ID.GT.0) GOTO 10
      if(id.eq.0.and.ihr.eq.0.and.imn.eq.0) then
          idy= 0
          imo= 0
          return
      endif
      IYR= IYR-1
      GOTO 5
10    KMO(2)= 28+L
      DO 20 I=1,12
      ID= ID- KMO(I)
      IF(ID.LE.0) GOTO 30
20    CONTINUE
      I=12
30    IDY= ID+KMO(I)
      IMO= I
      RETURN
      end ! of subr. datum



	subroutine indexxi(n, iarrin, indx)

	implicit none

	integer	n
	integer	iarrin(n)
	integer	indx(n)

	integer	i
	integer	indxt
	integer	ir
	integer	j
	integer	l
	integer	q

      if (n.lt.1) then
         return
      else if (n.eq.1) then
         indx(1) = 1
         return
      endif

      do 11 j=1,n
         indx(j) = j
11    continue
      l = n/2+1
      ir = n
10    continue
         if (l.gt.1) then
            l = l-1
            indxt = indx(l)
            q = iarrin(indxt)
         else
            indxt = indx(ir)
            q = iarrin(indxt)
            indx(ir) = indx(1)
            ir = ir-1
            if (ir.eq.1) then
               indx(1) = indxt
               return
            endif
         endif
         i = l
         j = l+l
20       if (j.le.ir) then
            if (j.lt.ir) then
               if (iarrin(indx(j)).lt.iarrin(indx(j+1))) j = j+1
            endif
            if (q.lt.iarrin(indx(j))) then
               indx(i) = indx(j)
               i = j
               j = j+j
            else
               j = ir+1
            endif
         go to 20
         endif
         indx(i) = indxt
      go to 10
      end


c Find specified value in ordered integer vector

	integer function ifindi(n, ia, iv)

	implicit none

c	Parameters:
	integer		n
	integer		ia(n)	! [1..n] Vector to search
	integer		iv	! Value to find

c	Local variables:
	integer		i
	integer		k	! 2^(no. of chops)

      if (n.le.0) then
         ifindi = 0
         return
      endif

      if (iv.lt.ia(1) .or. iv.gt.ia(n)) then
c        Outside range of vector
         ifindi=0
         return
      endif

      k = 2
      i = nint(real(n)/k)
10    if (k.gt.2*n) then
c        Value not in vector
         ifindi = 0
         return
      endif
      k = k*2
      if (iv.lt.ia(i)) then
c        Value smaller:  Search below
         i = i-nint(real(n)/k)
         goto 10
      endif
      if (iv.gt.ia(i)) then
c        Value larger:  Search above
         i = i+nint(real(n)/k)
         goto 10
      endif

c     Value found: iv == ia[i]
      ifindi = i
      return
      end
