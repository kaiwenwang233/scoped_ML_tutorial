	program readsac

c	Input:  filename, t1, t2, icut, swap
c	Output: seis, tref

c  how to make sure that the length of the windows is the same nint?
c  need character equivalence to do byte swap
c  test with readsac and sac2mat in matlab.
c  perhaps two file opens is slower than Doug's approach

	real dt,t0,ptime,stime,rot
	integer*4 npts,n1,n2
	integer ifile,r1,r2,s1,s2,wlen,blen,npts1,npts2,shift
	character*400000 str
	real seis(100000), y(100000)
	equivalence(seis,str)


	real t1,t2
	real tcut,tstart

	t1=3.5
	t2=7
	icut='P'


c Get needed header information
        call freeunit(ifile)
      	open(ifile,file='test.sac',form='unformatted',access='direct',
     &		recl=4)

	read(10,rec=1) dt
	read(10,rec=6) t0
	read(10,rec=9) ptime
	read(10,rec=11) stime
	read(10,rec=80) npts
	read(10,rec=8) rot

	print *, dt,t0,ptime,stime,npts,rot

	close(ifile)

c Compute time window
	if(icut.eq.'P') tcut=ptime
	if(icut.eq.'S') tcut=stime
	if(tcut.eq.-12345.) stop' Error: can not cut, header time not set.'

	tstart=tcut-t1
	n1=nint((tstart-t0)/dt)+1
	tdiff=t2+t1
	n2=n1+nint(tdiff/dt)

	print *, tcut,tstart,n1,tdiff,n2

	if(n1.lt.1)  stop' Error: cut window starts before first sample.'
	if(n1.gt.npts)  stop' Error: cut window starts after last sample.'
	if(n2.lt.1)  stop' Error: cut window ends before first sample.'
	if(n2.gt.npts)  stop' Error: cut window ends after last sample.'

c Get Data

	call getSACdata('test.sac',npts,n1,n2,y,ierr)

	wlen=n2-n1+1

        call freeunit(ii)
      	open(ii,file='y2',status='unknown')
	do i=1,wlen
	  write(ii,*) y(i)
	enddo
	close(ifile)


	end
