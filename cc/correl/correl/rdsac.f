	program rdsac

c	modeled after sac2correl.m

	integer*4 j
	real*4 r
	character*4 str


      	open(10,file='test.sac',form='unformatted',access='direct',
     &		recl=4)

	print *, "real header"

	do i=1,70
	   read(10,rec=i) r
	   print *, r
	enddo

	print *, "integer header"

	do i=1,40
	   read(10,rec=i+70) j
	   print *, j
	enddo

	print *, "string header"

	do i=1,48
	   read(10,rec=i+110) str
	   print *, str
	enddo

	print *, "sac data"

	do i=1,40
	   read(10,rec=i+158) r
	   print *, r
	enddo

	close(10)

	end
