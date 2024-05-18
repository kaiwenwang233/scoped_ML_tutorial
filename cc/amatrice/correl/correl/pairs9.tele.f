      program pairs9

c --- this program is a stripped down version of correl9.f.  it only computes
c --- the pairs at each station that will have correlations, but doesn't
c --- read in the waveforms or do the correlations.  used with dividesta.m
c --- to parallelize the stations across the CPUs.

         
      implicit none

      include 'correl.inc'     
      
      character*15  sta(MAXSTA)			! Master station list
      character*100 files(MAXEVE)		! Master file list
      character*100 fn_inp, fn_pair, fn_sta, fn_cc! filenames
      character*100 fn_log 
      character*100 fn_ids, fn_nids, fn_cat
      character*30 prodir

      integer	ids(MAXEVE)			! Master event id list
      integer	nids(MAXEVE)			! New id list
      integer	i1, i2				! Master pair indices
      integer	idssub(MAXEVE)			! Subset event id list
      integer	isub, jsub			! Subset pair indices
      integer	ind(MAXEVE)			! index map from master to subset
      integer	ind1(MAXEVE)			! flag if new (1) or old (0)
      integer	nev				! number of master ids
      integer	nsta				! number of master stations
      integer	npair				! number of master pairs
      integer	nevsub				! number of subset ids
      
      real	data(MAXSAMP,MAXFILES)		! seismogram matrix 
      integer	nsamp				! number of samples at station
      real	tref(MAXFILES)			! reference travel times of first sample

	integer yr,mo,day,hr,min,nst,gap,clo
	real sec,mag,rms
	real lat(MAXEVE),lon(MAXEVE),dep(MAXEVE)
	integer cids(MAXEVE)

      
c --- local variables
      integer 	i3				! loop index over nsta
      integer	k				! loop index over npair
      real	y1(MAXSAMP),y2(MAXSAMP)		! seismograms to correlate
      real	cc,dt,cc2,dt2,dtsamp2
      integer	iunit
      integer	ct, ct2,missed
      integer   trimlen
	real t1	
	real t2, w1
	real delta
	character icut
	real lag
	integer nlag
	logical iswap
	character*20 pattern
	integer l,n,i,nev2,nev3,j,nw1,nw2
	character*50 dir
	integer system
	real tdiff
	real dtsamp
	integer lunit
	real lowcorner, highcorner
	real ccthr
	real tmp
	logical ifilter

	real		dist
	real		maxdist, stadist
	real dlat,dlon
	real slat, slon,del,az
	real		PI
        real            KMPERDEG
	integer inew, iold

	parameter	(PI=3.141593)
        parameter       (KMPERDEG=111.1949266)

      
      
	print *, 'seg fault check parameter'
	print *, 'decide on character lengths and make consistent'

c      call getinp(fn_inp,fn_pair,fn_sta,fn_cc,pattern,t1,t2,delta,icut,iswap)

c --- Output:

c	print *, "enter pair file:"
c	read(*,*) fn_pair
	print *, "enter catalog file:"
	read(*,*) fn_cat
c	print *, "enter new catalog file:"
c	read(*,*) fn_catnew
	print *, "enter station file:"
	read(*,*) fn_sta
	print *, "enter output file:"
	read(*,*) fn_cc
	print *, "enter log file:"
	read(*,*) fn_log

c --- Input parameters

	pattern='VHZ'		! inactive
	t1=0.			! seconds before pick to read in
	t2=2.			! seconds after pick to read in, window is t1+t2
	w1=1.			! window length of short window (seconds)
	delta=0.01		! sample interval, resamples if necessary
	icut='P'		! phase to read in
c	icut='S'
	lag=1.			! lags to search over (seconds)
c	iswap=.false.		! swap bytes flag
	iswap=.true.
	ifilter=.true.		! filter flag	
	lowcorner=1.5		! low pass corner (Hz)
	highcorner=15.		! high pass corner (Hz)
	maxdist=100.		! maximum distance to correlate (km)
	ccthr=0.6		! CC threshold to save data

      	prodir='./' 		! project directory with NET.STA subdirectories

c --- Add lag padding to correlation window to cut data window
	
	nlag=nint(lag/delta)	! get integer sample lag
	nw1=nint(w1/delta)	! samples of short window
	nw2=nint((t2+t1)/delta)	! samples of long window

	t1=t1+lag
	t2=t2+lag
	tdiff=t2+t1
	nsamp=nint(tdiff/delta)+1
	if(nsamp.gt.MAXSAMP) goto 900	! Number of samples out of bounds

 
c	write(*,*) 'Getting pairs...'
c	i=system('date')
c        call readinp2(fn_pair,fn_sta,ids,i1,i2,sta,nev,nsta,npair)
c	write(*,*) 'Done getting pairs and events.', npair, nev
c	i=system('date')

c --- Read input old catalog
	write(*,*) "Reading in old catalog file..."
c	i=system('date')
        call freeunit(iunit)
      	open(iunit,file=fn_cat,status='unknown')

	i=1
c10	read(iunit,*,end=20) yr,mo,day,hr,min,sec,lat(i),lon(i),dep(i),
c     &                mag,nst,gap,clo,rms,ids(i)
c10	read(iunit,*,end=20) yr,mo,day,hr,min,sec,lat(i),lon(i),dep(i),
c     &                mag,tmp,tmp,tmp,ids(i)
10	read(iunit,*,end=20) yr,mo,day,hr,min,sec,lat(i),lon(i),dep(i),
     &                mag,ids(i)
	ind1(i)=0
	if(i.lt.5) print *, yr,mo,day,hr,min,ids(i)
	i=i+1
	goto 10
20	nev=i-1
	close(iunit)
	write(*,*) 'Number of events: ', nev

	iold=i

c --- Read input new catalog
c	write(*,*) "Reading in new catalog file..."
c	i=system('date')
c        call freeunit(iunit)
c      	open(iunit,file=fn_catnew,status='unknown')
c
c15	read(iunit,*,end=25) yr,mo,day,hr,min,sec,lat(i),lon(i),dep(i),
c     &                mag,ids(i)
c	ind1(i)=1
c	if(i.lt.iold+5) print *, yr,mo,day,hr,min,ids(i)
c	i=i+1
c	goto 15
c25	nev=i-1
c	close(iunit)
c	write(*,*) 'Number of events: ', nev

c --- Select event subset
	print *, "enter id file:"
	read(*,*) fn_ids(1:100)
	print *, fn_ids(1:trimlen(fn_ids))
	call freeunit(iunit)
	open(iunit,file=fn_ids(1:trimlen(fn_ids)),status='unknown')
	i=1
30	read(iunit,*,end=40) cids(i)
	i=i+1
	goto 30
40	nev2=i-1
	close(iunit)

	print *, 'subset ', nev2
        call indexmap(nev,ids,nev2,cids,ind)

	ct=0
	do i=1,nev
           if(ind(i).ne.0) then
	      ct=ct+1
	      lat(ct)=lat(i)
	      lon(ct)=lon(i)
	      dep(ct)=dep(i)
	      ids(ct)=ids(i)
   	      ind1(ct)=ind1(i)
	   endif
	enddo
	nev=ct

	write(*,*) 'Number of events after event select: ', nev
c	i=system('date')

c	not used any more in findsac2.f
c	call freeunit(iunit)
c	open(iunit,file='cids',status='unknown')
c	do i=1,nev
c          write(iunit,*) ids(i)
c	enddo
c	close(iunit)

	if(nev.gt.MAXEVE) stop'increase maxeve'

c --- Select new ids
	print *, "enter new ids file:"
	read(*,*) fn_nids(1:100)
	print *, fn_nids(1:trimlen(fn_nids))
	call freeunit(iunit)
	open(iunit,file=fn_nids(1:trimlen(fn_nids)),status='unknown')
	i=1
35	read(iunit,*,end=45) nids(i)
	i=i+1
	goto 35
45	nev3=i-1
	close(iunit)

        call indexmap(nev,ids,nev3,nids,ind1)

	do i=1,nev
           if(ind1(i).ne.0) ind1(i)=1
	enddo


c        call freeunit(lunit)
c	open(lunit,file=fn_log,status='unknown')

c --- Read in station file
        call freeunit(iunit)
      	open(iunit,file=fn_sta,status='unknown')
	i=1
50	read(iunit,'(a)',err=998,end=60) sta(i)
	i=i+1
	goto 50
60	nsta=i-1
	close(iunit)

      
      ct=0
      do i3=1,nsta
c --- !! station loop doesn't work, need to open and close new files and staids
      
	dir=prodir(1:trimlen(prodir))// '/' //sta(i3)
	write(*,*) 'Matching files at ', sta(i3)(1:trimlen(sta(i3)))
c	i=system('date')
	call findsac2(ids,nev,dir,idssub,nevsub,files)
	write(*,*) 'Found this many files: ', nevsub

	npair=0


	if (nevsub.lt.2) then
	   write(*,*) "Less than two events at: ", 
     &                 sta(i3)(1:trimlen(sta(i3)))
	   goto 100
	endif


         call indexmap(nev,ids,nevsub,idssub,ind)


c --- Compute pairs (upper triangular)
	write(*,*) 'Computing correlations...'
	do i1=1,nev-1
	   do i2=i1+1,nev

	   inew = ind1(i1)+ind1(i2)

	   if (inew.ge.1) then

            isub=ind(i1)
            jsub=ind(i2)

           if (isub.ne.0.and.jsub.ne.0) then

c --- Limit by inter-event separation distance
c --- faster to execute above "if" statement than to compute distance
              dlat= lat(i1) - lat(i2)
              dlon= lon(i1) - lon(i2)
              dist= sqrt( (dlat*KMPERDEG)**2 +
     &                     (dlon*(cos(lat(i1)*PI/180)*KMPERDEG))**2 +
     &                     (dep(i1)-dep(i2))**2 )
              if(dist.le.maxdist) then
                  npair=npair+1  

	    endif	! if(dist.le.maxdist) then
            endif	! if (isub.and.jsub) then
            endif	! if (inew.ge.1) then
      
         enddo	! do i2=i1+1,nev
         enddo	! do i1=1,nev-1
      
100     continue
	enddo	! do i3=1,nsta

	write(*,*) 'pairs ', sta(i3-1)(1:trimlen(sta(i3-1))), npair
	i=system('date')


      
        close(iunit)
	close(lunit)
	
	goto 1000
c --- Error handling

900	write(*,*) 'Number of samples requested too large'
        write(*,*) 'increase MAXSAMP'
	stop

998	write(*,*) 'Error reading station file ',fn_sta,' at line: ',i
	stop

      
1000      end
      
      
      
