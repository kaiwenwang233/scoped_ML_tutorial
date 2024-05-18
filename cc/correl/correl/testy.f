	program testy

	integer nev,npair
	parameter(nev=8000000,npair=9)
	integer i,size

	character*8000000 j
	
	integer ids(1000000)
	real cc(1000000)
	equivalence(ids,j)
	equivalence(cc,j)
	integer system
	character file*8

c 1 mil obs, takes 16-17 seconds binary to write out, 5 sec in ascii
c 1 mil obs, takes 50 seconds binary to read in, 4 sec in ascii

	file='out'
	i=system('ls -l ' // file // ' | nawk \'{print $5}\'> tmp.inp')
      	   open(11,file='tmp.inp',status='unknown')
	   read(11,*) size
	   close(11)
      	open(10,file='out',form='unformatted',access='direct',
     &		recl=size)

	read(10,rec=1) j(1:4000000)
c	write(10,rec=1) j


	print *, j(4000000:4000000)
	print *, ids(1000000)
	print *, cc(1000000)

c      	open(10,file='out',status='unknown')
c	do i=1,1000000
c	  write(10,rec=i) i
c	  write(10,'(i8)') i
c	  read(10,'(i8)') j
c	enddo
c	write(10,rec=4) 9028
	close(10)


c      	open(11,file='out',form='unformatted',access='direct',
c     &		recl=4)
c	do i=1,100000
c	  read(11,rec=i) j
c	enddo
c	close(11)

	end
