	program test3

	call freeunit(iunit)
	open(iunit,file='test.hdr',status='unknown')
	read(iunit,*,end=49,err=50) iyr,imo

	print *, iyr, imo

	goto 100

49	write(*,*) 'empty header'
	goto 100
50	write(*,*) 'error header'

100	end
