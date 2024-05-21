       program sel_sta 

	implicit none
        integer 	ntot, nmatch
        integer		MEV
        integer  	ierr1	
        integer  	ierr2	
        integer        	trimlen 
        integer         n
        integer         n1	
        integer         n2	
        integer         id1,id2
        parameter(mev=8000000)
        integer         nev
        integer         itake
	real	 	r,otc
	real		az
	real		tcorr	
	integer		nev1
	integer		nev2
	real		del
	real		dist
	integer		i
	integer		j
	integer		k
	integer		l
	integer		iargc
	integer		ii
	integer		narguments
	integer		nsta
	character	fn1*80
	character	fn2*80
	character	fn3*80
	character	line*180
	character	hline*180
	character	pline(10000)*180
	character	p_sta*7
	character	str1*1,str,pha*1
	logical 	ex
	integer		npha,nphaP,nphaS
        integer		yr,mo,dy,hr,mn,adate(MEV),aid(MEV)
   	real 		time(MEV),a
        real		sc,lat(mev),lon(mev),la,lo,de,ma,r1,r2,r3


c--- get input parameter file name:
      narguments = iargc()
      if(narguments.lt.2) goto 2
      call getarg(1,fn1)
      inquire(FILE= fn1,exist=ex)
      if(.not. ex) stop' >>> ERROR OPENING INPUT FILE 1.'
      call getarg(2,fn2)
      goto 3
2     stop'>>add_npha *.pha output(.pha.npha)'
3     continue


c--- read absolute network travel times:
      nev= 0
      npha= 0
      nphaP= 0
      nphaS= 0
      open (1,file=fn1, status='unknown')
      open (2,file=fn2, status='unknown')

430   read (1,'(a)',end=460) line  		! read header line
      if (line(1:1).eq.'#') then
         if(nev.gt.0) then
            read(hline,*)str1,id1,id2,otc
            write(2,'(a1,i9,i9,f10.4,3i6)') str1,id1,id2,otc,
     &      npha,nphaP,nphaS 
c            write(2,'(a,i6)') hline(1:trimlen(hline)),npha 
            do i=1,npha
               write(2,'(a)') pline(i)(1:trimlen(pline(i)))
            enddo
         endif
         hline= line
         npha= 0
         nphaP= 0
         nphaS= 0
         nev= nev+1 
      else
c         write(*,*)line 
         read(line,*)str1,a,a,pha
	 if(pha.eq.'P') nphaP= nphaP+1
	 if(pha.eq.'S') nphaS= nphaS+1
         npha= npha+1
         pline(npha)= line 
      endif
      goto 430
c460   write(2,'(a,i6)') hline(1:trimlen(hline)),npha 
460   read(hline,*)str1,id1,id2,otc
      write(2,'(a1,i9,i9,f10.4,3i6)') str1,id1,id2,otc,
     & npha,nphaP,nphaS
      do i=1,npha
         write(2,'(a)') pline(i)(1:trimlen(pline(i)))
      enddo
      close(1)
      write(*,*)' Total events in catalog = ',nev
      end


c Length of character string, excluding trailing blanks

	integer function trimlen(t)

	implicit none

c	Parameter:
	character t*(*)

      do 1 trimlen=LEN(t),1,-1
    1    if(t(trimlen:trimlen).ne.' ')RETURN
      trimlen=1
      end ! of integer function trimlen
