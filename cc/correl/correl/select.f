	program select

c  f77 -o select select.f
c  awk '{print $1}' *.sift | sort > sift.sta

	integer MAXEVE
	parameter(MAXEVE=10000000)
	integer   ids(MAXEVE), ct

        integer system

        open(11,file='sift.sta',status='unknown')
        open(12,file='sel.out',status='unknown')

	i=system('date')
	print *, "Reading in sift.sta..."
	i=1
10	read(11,*,end=20) ids(i)
	i=i+1
	goto 10
20	nev=i-1
	print *, "Competed reading in sift.sta."
	i=system('date')

c --- count the number of stations observing each event
	ct=1
	do i=2,nev
	   if(ids(i).eq.ids(i-1)) then
	      ct=ct+1
	   else
	      write(12,'(i8,2x,i4)') ids(i-1), ct
	      ct=1
	   endif	
	enddo
	

	close(11)
	close(12)

	end

