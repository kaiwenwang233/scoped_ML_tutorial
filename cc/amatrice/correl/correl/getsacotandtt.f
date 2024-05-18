c --- otttout in directory, form ot and tt table later, with staindx from dir
c since whole file is 4*byte records can use same byte swap as conversac.c

c f77 -o getsacotandtt getsacotandtt.f freeunit.f parsedot.f swap.f trimlen.f

        implicit none

        include 'correl.inc'    

	integer ids(MAXFILES)
        character*100 files(MAXFILES), fil	
        integer	iunit,i,ichr,nfiles,lunit

	integer yr,jday,hr,min,sec,ms,wt
	real ttp

	integer system
	integer trimlen

	character*4 realtmp
	character*4 inttmp
	character*4 str
	real rtmp
	integer itmp
	equivalence(rtmp,realtmp)
	equivalence(itmp,inttmp)

	logical iswap

	iswap=.false.

	i=system('ls | grep VHZ  > tmp.files')

      	call freeunit(iunit)
      	open(iunit,file='tmp.files',status='unknown')

	i=1
10	read(iunit,'(a)',end=20,err=70) files(i)
	i=i+1
	goto 10
20	nfiles=i-1
	close(iunit)
	i=system('rm tmp.files')

c --- Get integer ids
	do i=1,nfiles
	  call parsedot(files(i),30,ichr)
	  read(files(i)(1:ichr-1),'(i)') ids(i)
	enddo

c --- Open output file
        call freeunit(lunit)
      	open(lunit,file='otttout',form='unformatted',
     &		access='direct',recl=4,err=75)


c --- Loop over files
	do i=1,nfiles

c --- Get needed header information
        call freeunit(iunit)
	fil=files(i)
      	open(iunit,file=fil(1:trimlen(fil)),form='unformatted',
     &		access='direct',recl=4,err=50)

	read(iunit,rec=71,err=50) inttmp
	 if(iswap) call swap(inttmp,4)
	 yr=itmp
	read(iunit,rec=72,err=50) inttmp
	 if(iswap) call swap(inttmp,4)
	 jday=itmp
	read(iunit,rec=73,err=50) inttmp
	 if(iswap) call swap(inttmp,4)
	 hr=itmp
	read(iunit,rec=74,err=50) inttmp
	 if(iswap) call swap(inttmp,4)
	 min=itmp
	read(iunit,rec=75,err=50) inttmp
	 if(iswap) call swap(inttmp,4)
	 sec=itmp
	read(iunit,rec=76,err=50) inttmp
	 if(iswap) call swap(inttmp,4)
	 ms=itmp

	read(iunit,rec=9,err=50) realtmp
	 if(iswap) call swap(realtmp,4)
	 ttp=rtmp
	read(iunit,rec=41,err=50) realtmp
	 if(iswap) call swap(realtmp,4)
	 wt=rtmp

	if(wt.ne.999.) then
	   read(iunit,rec=122,err=50) str
	   read(str(1:1),'(i)') wt
	endif


c	write(*,'(i8,1x,i4,1x,i3,1x,i2,1x,i2,1x,i2,1x,i3,
c	print *, ids(i),yr,jday,hr,min,sec,ms,ttp,wt

	write(lunit,rec=9*i-8) ids(i)
	write(lunit,rec=9*i-7) yr
	write(lunit,rec=9*i-6) jday
	write(lunit,rec=9*i-5) hr
	write(lunit,rec=9*i-4) min
	write(lunit,rec=9*i-3) sec
	write(lunit,rec=9*i-2) ms
	write(lunit,rec=9*i-1) ttp
	write(lunit,rec=9*i)   wt


	goto 60

50	write(*,*) 'Error reading sac header: ', fil(1:trimlen(fil))


60	enddo	! do j=1,n	loop over files 

	goto 80

70	stop 'Error reading sac file list'
75	stop 'Error opening output file'

80	end
