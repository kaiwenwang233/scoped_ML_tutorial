	subroutine readinp(fn_pair,fn_sta,ids,i1,i2,sta,nev,nsta,npair)

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

c --- First try reading as an ASCII file, if error then read as binary
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
	
	
        call freeunit(iunit)
      	open(iunit,file=fn_sta,status='unknown')
	i=1
30	read(iunit,'(a)',err=998,end=40) sta(i)
	i=i+1
	goto 30
40	nsta=i-1
	close(iunit)

	goto 999

100	continue

c --- ASCII read failed, try reading as binary

997	write(*,*) 'Error reading pair file ',fn_pair,' at count: ',i
	stop
998	write(*,*) 'Error reading station file ',fn_sta,' at line: ',i
	stop
999	return
	end
