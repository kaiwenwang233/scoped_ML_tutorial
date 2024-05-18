	subroutine readsac(n,files,dir,t1,t2,dt,icut,iswap,lunit,
     &		           data,nev,nsamp,tref,ind)

	include 'correl.inc'

c	Input:
	integer n	! number of files
        character*100 files(MAXFILES)		! Master file list
	character*50 dir
	real t1	
	real t2
	real dt
	character icut
	logical iswap

c	Output:
	real data(MAXSAMP,MAXFILES) ! (1...nsamp,1...nev) seismogram matrix 
	integer nev
	integer nsamp
	real tref(MAXFILES)
	integer ind(MAXFILES)	! (1...n) with 1s and 0s

c  f77 readsac.f freeunit.f sac.f swap.f resample.f
c  tested with readp in matlab produces exactly the same data,nev,nsamp,tref
c  errors seem to work, could write all of the errors to a log file
c  could record summary of bad file reads in standard output

c	Local:
	real t0,ptime,stime,rot,dt1
	integer*4 npts,n1,n2
	integer iunit,wlen,wlen2,lunit
	real  y(MAXSAMP), y2(MAXSAMP)
	character*4 strtmp
	character*4 inttmp
	real tmp
	equivalence(tmp,strtmp)
	equivalence(npts,inttmp)
	real tcut,tstart
	character*100 fil
	integer trimlen
	real mean
	integer ct, ct2



	if(n.gt.MAXFILES) goto 70		! Number of events out of bounds

c Initialize index vector, nev, nsamp, and wlen2
	do i=1,n
	   ind(i)=1
	enddo

	nev=0
	tdiff=t2+t1
	nsamp=nint(tdiff/dt)+1
	wlen2=nsamp

	if(nsamp.gt.MAXSAMP) goto 80	! Number of samples out of bounds

c Loop over files
	ct2=0
	do j=1,n

c Get needed header information
        call freeunit(iunit)
	fil=dir(1:trimlen(dir))// '/' //files(j)


      	open(iunit,file=fil,form='unformatted',access='direct',
     &		recl=4,err=50)


	read(iunit,rec=1,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 dt1=tmp
	read(iunit,rec=6,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 t0=tmp
	read(iunit,rec=9,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 ptime=tmp
	read(iunit,rec=11,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 stime=tmp
	read(iunit,rec=8,err=50) strtmp
	 if(iswap) call swap(strtmp,4)
	 rot=tmp
	read(iunit,rec=80,err=50) inttmp
	 if(iswap) call swap(inttmp,4)

c	print *, fil(1:trimlen(fil))

c	print *, dt1,t0,ptime,stime,npts,rot, iunit

	close(iunit)

c --- Compute time window
	if(icut.eq.'P') tcut=ptime
	if(icut.eq.'S') tcut=stime
	if(tcut.eq.-12345.) goto 51

	tstart=tcut-t1
	n1=nint((tstart-t0)/dt1)+1
	tdiff=t2+t1
	n2=n1+nint(tdiff/dt1)

c	print *, t1, t2, dt1, n2-n1

	if(n1.lt.1)     goto 52
	if(n1.gt.npts)  goto 53
	if(n2.lt.1)     goto 54
	if(n2.gt.npts)  goto 55
	if(n2-n1.gt.MAXSAMP) goto 59

c --- Compute reference time of the first sample of cut window

	tref(j)=t0+(n1-1)*dt1

c	print *, tcut,tstart,n1,tdiff,n2,tref(j)

c --- Get Data
	ierr=0
c	print *, npts,n1,n2
c	print *, ierr
c        print *, iswap

	call getSACdata(fil,npts,n1,n2,iswap,y,ierr)

c	print *, npts,n1,n2
c	print *, ierr

c replacement for getSACdata

c	k=0
c	do i=158+n1,158+n1+3
c	print *, i, iunit
c          read(iunit,rec=159,err=56) strtmp
c	  if(iswap) call swap(strtmp,4)
c          k=k+1
c	  y(k)=tmp
c	enddo

c	print *, y(1), y(2), y(3)

c	close(iunit)

c replacement for getSACdata

	if(ierr.ne.0) goto 56

	wlen=n2-n1+1

c	print *,  ierr, wlen, nsamp, dt1, dt

c --- Remove mean from data vectors 
	   mean=0.
	   do i=1,wlen
	      mean=mean+y(i)
	   enddo
	   mean=mean/wlen
	   do i=1,wlen
	      y(i)=y(i)-mean
	   enddo

c --- Find zero traces
	   ct=0
	   do i=1,wlen
	      if(y(i).eq.0) ct=ct+1
	   enddo
	   if(ct.eq.wlen) goto 57


c	if(dt1.lt.dt-.001) goto 58    ! perhaps this is not necessary

	ct2=ct2+1
	if(dt1.eq.dt) then
	 do i=1,wlen
	   data(i,ct2)=y(i)
	 enddo
	else
	 write(lunit,*) 'Resampling: ', fil(1:trimlen(fil))
	 call resample(dt1,y,wlen,dt,y2,wlen2)

	nsamp=min(nsamp,wlen2)	! resample may cause slightly shorter windows

	 do i=1,wlen2
	   data(i,ct2)=y2(i)
	if (data(i,ct2).le.1e10) then
c	 print *, "data is real"
	else
c	 print *, i, ct2, data(i,ct2), "data is nan"
	 ind(j)=0		! mark event as problematic
	 ct2=ct2-1
	 write(lunit,*) 'Waveform is Nan after resample. ', 
     &                  fil(1:trimlen(fil))
	goto 60

	endif
	 enddo
	endif
	nev=nev+1


	goto 60

50	ind(j)=0		! mark event as problematic
	write(lunit,*) 'Error reading sac header: ', 
     &                  fil(1:trimlen(fil))
	goto 60
51	ind(j)=0
	write(lunit,*) 'Header cut point travel time not set (-12345): ', 
     &                 fil(1:trimlen(fil))
	goto 60
52	ind(j)=0
	write(lunit,*) 'Cut window starts before first sample: ', 
     &                  fil(1:trimlen(fil))
	goto 60
53	ind(j)=0
	write(lunit,*) 'Cut window starts after last sample: ', 
     &                  fil(1:trimlen(fil))
	goto 60
54	ind(j)=0
	write(lunit,*) 'Cut window ends before first sample: ', 
     &                  fil(1:trimlen(fil))
	goto 60
55	ind(j)=0
	write(lunit,*) 'Cut window ends after last sample: ',
     &                 fil(1:trimlen(fil))
	goto 60
56	ind(j)=0
	write(lunit,*) 'Error reading sac data: ', fil(1:trimlen(fil)), ierr
	goto 60
57	ind(j)=0
	write(lunit,*) 'Trace is all zeros: ', fil(1:trimlen(fil))
	goto 60
58	ind(j)=0
	write(lunit,*) 'Sample interval is less than requested: ', 
     &                 fil(1:trimlen(fil))
	goto 60
59	ind(j)=0
	write(lunit,*) 'Too many samples', fil(1:trimlen(fil)), 
     &                  n2-n1, MAXSAMP, dt1, dt
	goto 60
60	continue	
	enddo	! do j=1,n	loop over files 

c --- Remove problematic records from tref
	ct2=0
	do i=1,n
	  if(ind(i).eq.1) then
	    ct2=ct2+1
	    tref(ct2)=tref(i)
	  endif
	enddo


	return

70	write(*,*) 'Number of events too large, increase MAXFILES'
	write(*,*) n, MAXFILES
	stop

80	write(*,*) 'Number of samples requested too large'
	write(*,*) 'increase MAXSAMP'
	write(*,*) fil
	stop


1000	end


c	program runsac
c
c f77 readsac3.f freeunit.f sac.f swap.f resample.f 
c
c       parameter(MAXFILES=10000,MAXFILES=100000,MAXSAMP=5000)
c	Input:
c	integer n	! number of files
c        character*30 files(MAXFILES)		! Master file list
c	real t1	
c	real t2
c	real dt
c	real icut
c	logical iswap
c
cc	Output:
c	real data(MAXSAMP,MAXFILES)		! seismogram matrix
c	integer nev
c	integer nsamp
c	real tref(MAXFILES)
c	integer ind(MAXFILES)
c
c	t1=40
c	t2=3
c	dt=0.01
c	icut='P'
c	iswap=.false.
c
c        call freeunit(iunit)
c      	open(iunit,file='files',status='unknown')
c
c	i=1
c10	read(iunit,'(a)',end=20) files(i)
c	i=i+1
c	goto 10
c20	n=i-1
c
c
c	call readsac(n,files,t1,t2,dt,icut,iswap,data,nev,nsamp,tref,ind)
c
c	write(*,*) (ind(l), l=1,n)
c
c	do i=1,nsamp
cc	write(*,'(f)') (data(i,l), l=1,nev)
c	enddo
c
c	end

