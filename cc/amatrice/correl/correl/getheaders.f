	program getheaders

c  f77 -o getheaders getheaders.f parsedot.f freeunit.f trimlen.f swap.f

	integer MAXEVE
	parameter(MAXEVE=130000)
        character*100 files(MAXEVE)		
	character*20 pattern
	integer i, n, iunit
	logical iswap

	pattern='VHZ'
	iswap=.false.

	i=system('ls  | grep ' //pattern// ' > tmp.files')
      	call freeunit(iunit)
      	open(iunit,file='tmp.files',status='unknown')

	i=1
10	read(iunit,'(a)',end=20) files(i)
	i=i+1
	goto 10
20	n=i-1
	close(iunit)
	i=system('rm tmp.files')

	call getheaders(n,files,iswap)

	end

	subroutine getheaders(n,files,iswap)

	implicit none

	integer MAXEVE
	parameter(MAXEVE=130000)

c	Input:
	integer n		! number of files
        character*100 files(MAXEVE)
        character*100 fil		
	logical iswap

c	Local:
	integer f1,f2,f3
	integer iunit, j
	integer trimlen
	real del,ptime,rot,theor
	integer yr,jday,hr,min,sec,ms,id,ichr
	real lat,lon,dep,mag
	character*8 desc

	character*4 strtmp
	character*4 inttmp
	real tmp
	integer itmp
	equivalence(tmp,strtmp)
	equivalence(itmp,inttmp)


c --- Open for ASCII
c        call freeunit(f1)
c      	open(f1,file='origin',status='unknown')
c        call freeunit(f2)
c      	open(f2,file='tt',status='unknown')


c --- Open for Binary
        call freeunit(f1)
      	open(f1,file='origin',form='unformatted',access='direct',
     &		recl=4)
        call freeunit(f2)
      	open(f2,file='tt',form='unformatted',access='direct',
     &		recl=4)
        call freeunit(f3)
      	open(f3,file='del',form='unformatted',access='direct',
     &		recl=4)



	do j=1,n

	  call parsedot(files(j),30,ichr)
	  read(files(j)(1:ichr-1),'(i)') id


c Get needed header information
        call freeunit(iunit)
c	fil=dir(1:trimlen(dir))// '/' //files(j)
	fil=files(j)

      	open(iunit,file=fil,form='unformatted',access='direct',
     &		recl=4,err=50)


c --- Get origin time and event location
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
	read(iunit,rec=36,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 lat=tmp
	read(iunit,rec=37,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 lon=tmp
	read(iunit,rec=39,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 dep=tmp
	read(iunit,rec=40,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 mag=tmp

c --- Get other phase info
	read(iunit,rec=1,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 del=tmp
	read(iunit,rec=8,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 rot=tmp
	read(iunit,rec=9,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 ptime=tmp
	read(iunit,rec=41,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 theor=tmp

	read(iunit,rec=121,err=50) desc(1:4)
	read(iunit,rec=122,err=50) desc(5:8)

	close(iunit)

c --- Output in ASCII
c	write(f1,'(i4,2x,i3,2x,i2,2x,i2,2x,i2,2x,i3,2x,f3.1,2x,i10)')
c     &            yr,jday,hr,min,sec,ms,rot,id
c
c	if (theor.eq.999) then
c	  write(f2,'(i10,2x,f8.4,2x,"  999")') id, ptime
c	else
c	  write(f2,'(i10,2x,f8.4,2x,a8)') id, ptime, desc
c	endif

c --- Output in Binary
	write(f1,rec=12*j-4) lat
	write(f1,rec=12*j-11) yr
	write(f1,rec=12*j-10) jday
	write(f1,rec=12*j-9) hr
	write(f1,rec=12*j-8) min
	write(f1,rec=12*j-7) sec
	write(f1,rec=12*j-6) ms
	write(f1,rec=12*j-5) rot
	write(f1,rec=12*j-4) lat
	write(f1,rec=12*j-3) lon
	write(f1,rec=12*j-2) dep
	write(f1,rec=12*j-1) mag
	write(f1,rec=12*j) id

	if (theor.eq.999) desc="999     "
	write(f2,rec=4*j-3) id
	write(f2,rec=4*j-2) ptime
	write(f2,rec=4*j-1) desc(1:4)
	write(f2,rec=4*j) desc(5:8)

	write(f3,rec=2*j-1) id
	write(f3,rec=2*j) del

	print *, del

	goto 60

50	write(*,*) 'Error reading sac header: ', 
     &                  fil(1:trimlen(fil))

60	enddo	! do j=1,n	loop over files 

	close(f1)
	close(f2)
	close(f3)

	return
	end
