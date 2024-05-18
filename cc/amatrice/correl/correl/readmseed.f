	subroutine readmseed(n,files,dir,t1,t2,dt,icut,iswap,
     &		           lunit,data,nev,nsamp,tref,ind)

	include 'correl.inc'

c     -- include common block of header variables (hdr_...) and constants (c_...)
      	include 'codeco_common.f'

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
	integer	iyr, imo, idy, ihr, imn	
	real sec
	real t0,ptime,stime,dt1
	integer*4 npts,n1,n2
	integer iunit,wlen,wlen2,lunit,junit
	real  y(MAXSAMP), y2(MAXSAMP)
	real tcut,tstart
	character*100 fil
	character*100 hdrfile
	integer trimlen
	real mean
	integer ct, ct2
      	logical ihead	  
      	integer*4 iy(c_sigsize)
      	integer*4 ierr
      	character infile*80

	ihead=.false.

	if(n.gt.MAXFILES) goto 70		! Number of events out of bounds

c	fil=dir(1:trimlen(dir))// '.scratch'
c	call freeunit(junit)
c	open(junit,file=fil,status='unknown')

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


c --- Get needed header information

	fil=dir(1:trimlen(dir))// '/' //files(j)
	hdrfile=fil(1:trimlen(fil))// '.hdr'
c	print *, hdrfile
c	write(junit,*) hdrfile
	call freeunit(iunit)
	open(iunit,file=hdrfile,status='unknown',err=50)
	read(iunit,*,end=49,err=50) iyr,imo,idy,ihr,imn,sec,t0,ptime,
     &   stime
	close(iunit)

c --- Get Data

	fil=dir(1:trimlen(dir))// '/' //files(j)
	infile=fil(1:trimlen(fil))
c	print *, infile
	call mseedin( infile, iy, ierr, iswap, ihead)

	if(ierr.ne.0) goto 56

	dt1=1/hdr_smprate
	npts=hdr_nsamp

c	print *, npts, dt1,  infile
c	print *, iy(1:5)
c	write(junit,*) npts, dt1,  infile
c	write(junit,*) iy(1:5)

c --- Compute time window
	if(icut.eq.'P') tcut=ptime
	if(icut.eq.'S') tcut=stime
	if(tcut.eq.-12345.) goto 51
c	print *, 'one'
c	write(junit,*) 'one'
	tstart=tcut-t1
	n1=nint((tstart-t0)/dt1)+1
	tdiff=t2+t1
	n2=n1+nint(tdiff/dt1)
c	print *, 'two'
c	write(junit,*) 'two'

	if(n1.lt.1)     goto 52
	if(n1.gt.npts)  goto 53
	if(n2.lt.1)     goto 54
	if(n2.gt.npts)  goto 55
	if(n2-n1.gt.MAXSAMP) goto 59
c	print *, 'three'
	ct=1
	do i=n1,n2
 	  y(ct)=iy(i)
	  ct=ct+1
	enddo

c	print *, y(1:5)
c	write(junit,*) y(1:5)

	wlen=n2-n1+1

c --- Compute reference time of the first sample of cut window
c	print *, '3a'
c	print *, t0+(n1-1)*dt1, j
c	print *, j
c	write(junit,*) j
	tref(j)=t0+(n1-1)*dt1
c	print *, 'four'

c --- Remove mean from data vectors 
	   mean=0.
	   do i=1,wlen
	      mean=mean+y(i)
	   enddo
	   mean=mean/wlen
	   do i=1,wlen
	      y(i)=y(i)-mean
	   enddo
c	print *, 'five'
c --- Find zero traces
	   ct=0
	   do i=1,wlen
	      if(y(i).eq.0) ct=ct+1
	   enddo
	   if(ct.eq.wlen) goto 57

c	print *, 'six'
c	if(dt1.lt.dt-.001) goto 58    ! perhaps this is not necessary

	ct2=ct2+1
	if(dt1.eq.dt) then
	 do i=1,wlen
	   data(i,ct2)=y(i)
	 enddo
c	print *, '7'
	else
	 write(lunit,*) 'Resampling: ', dt1, dt , fil(1:trimlen(fil))
	 call resample(dt1,y,wlen,dt,y2,wlen2)
	 do i=1,wlen2
	   data(i,ct2)=y2(i)
	 enddo
c	print *, '8'
	endif
	nev=nev+1
	nsamp=min(nsamp,wlen2)	! resample may cause slightly shorter windows

	goto 60

49	ind(j)=0		! mark event as problematic
	write(lunit,*) 'miniseed header empty: ', 
     &                  hdrfile(1:trimlen(hdrfile))
	close(iunit)

	goto 60
50	ind(j)=0		! mark event as problematic
	write(lunit,*) 'Error reading miniseed header: ', 
     &                  hdrfile(1:trimlen(hdrfile))
	close(iunit)

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
	write(lunit,*) 'Error reading miniseed data: ', 
     &                 fil(1:trimlen(fil)), ierr
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

c	close(junit)
c	fil='rm '//dir(1:trimlen(dir))// '.scratch'
c	i=system(fil)

	return

70	write(*,*) 'Number of events too large, increase MAXFILES'
	write(*,*) n,MAXFILES
	stop

80	write(*,*) 'Number of samples requested too large'
	write(*,*) 'increase MAXSAMP'
	write(*,*) fil
	stop


1000	end


