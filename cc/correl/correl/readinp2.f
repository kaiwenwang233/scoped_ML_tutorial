	subroutine readinp2(fn_pair,fn_sta,ids,i1,i2,sta,nev,nsta,npair)

      implicit none
      
      include 'correl.inc'
      
      character*15  sta(MAXSTA)			! Master station list
      character*20 fn_pair, fn_sta		! filenames

      integer	ids(MAXEVE)			! Master event id list
      integer	i1(MAXPAIR), i2(MAXPAIR)	! Master pair indices
      integer	nev				! number of master ids
      integer	nsta				! number of master stations
      integer	npair				! number of master pairs

	integer i,j,ct,iunit

c --- Binary file variables
c	character*576320008 str		! must be >= (2*MAXPAIR+MAXEVE+2)*4
	character*854098452 str		! must be >= (2*MAXPAIR+MAXEVE+2)*4


	character*4 pairstr
	integer itmp
	equivalence(itmp,pairstr)

	
	integer size
	integer system
	integer iend
	integer iodd
	integer ieven
	integer nbytes
	

c --- Read in station file
        call freeunit(iunit)
      	open(iunit,file=fn_sta,status='unknown')
	i=1
30	read(iunit,'(a)',err=998,end=40) sta(i)
	i=i+1
	goto 30
40	nsta=i-1
	close(iunit)

c --- First try reading pair file as an ASCII file, if error then try binary
        call freeunit(iunit)
      	open(iunit,file=fn_pair,status='unknown')

	read(iunit,'(i)',err=100) nev
	do i=1,nev
	   read(iunit,'(i)',err=997) ids(i)
	enddo
	read(iunit,'(i)',err=997) npair

	if (npair.eq.-12345) then	! do all possible pairs
	   ct=0
	   do i=1,nev-1
	      do j=i+1,nev
	         ct=ct+1
	         i1(ct)=i
	         i2(ct)=j
	      enddo
	   enddo
	   npair=ct
	else
	   do i=1,npair
	      read(iunit,*,err=997) i1(i), i2(i)
	   enddo
	endif
	close(iunit)
	
	
	goto 999

100	close(iunit)

c --- ASCII read failed on pair file, try reading as binary

	write(*,*) "Opening as a binary file"

        call freeunit(iunit)
      	open(iunit,file=fn_pair,form='unformatted',access='direct',
     &		recl=4,err=800)

	read(iunit,rec=1,err=897) nev
	do i=1,nev
	   read(iunit,rec=i+1,err=897) ids(i)
	enddo
	read(iunit,rec=nev+2,err=897) npair
	close(iunit)

	if (npair.eq.-12345) then	! do all possible pairs
	   ct=0
	   do i=1,nev-1
	      do j=i+1,nev
	         ct=ct+1
	         i1(ct)=i
	         i2(ct)=j
	      enddo
	   enddo
	   npair=ct
	else
c --- get size of file in bytes
	   i=system('ls -l ' // fn_pair // ' | nawk \'{print $5}\' > tmp.inp')
           call freeunit(iunit)
      	   open(iunit,file='tmp.inp',status='unknown')
	   read(iunit,*) size
	   close(iunit)
	   i=system('rm tmp.inp')

c  check that character array is big enough
c	   if(size.gt.len(str)) stop

c --- read in entire file into str
      	   open(10,file=fn_pair,form='unformatted',access='direct',
     &		recl=size,err=800)
	   read(iunit,rec=1,err=898) str(1:size)
	   close(iunit)
c --- parse string data to equivalence string
	   iend=4*(nev+2)
	   do i=1,npair			! fill up i1 array
	      iodd=2*i-1
	      pairstr(1:4)=str(4*iodd+iend-3:4*iodd+iend)
	      i1(i)=itmp
	   enddo
	   do i=1,npair			! fill up i2 array
	      ieven=2*i
	      pairstr(1:4)=str(4*ieven+iend-3:4*ieven+iend)
	      i2(i)=itmp
c easy to swap if need be here.
	   enddo

	print *, "Binary read successful."
	endif

	goto 999

c --- end of binary pair file read

800	write(*,*) 'Can not open pair file ', fn_pair
	stop

897     write(*,*) 'Error reading pair file ',fn_pair,' at line: ',i
	stop

898     write(*,*) 'Error reading pair data, check dimensions.'
	stop

997	write(*,*) 'Error reading pair file ',fn_pair,' at count: ',i
	stop
998	write(*,*) 'Error reading station file ',fn_sta,' at line: ',i
	stop

999	return
	end
