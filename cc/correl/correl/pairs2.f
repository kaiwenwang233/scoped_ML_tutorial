	program pairs2
c 1191881994 pairs with dist=6, 14:25:05, 16:03:50
c maybe better to divide by stations, and use stadist. a billion pairs
c is 8 GB of disk space and RAM that is needed, Long Valley may need
c to divide pair files, 1) compute all then divide into four or whatever
c
c  f77 -O -o pairs2 pairs2.f indexmap.f ifindi.f indexxi.f freeunit.f

	implicit none

	integer MAXEVE, MAXPAIR
	parameter(MAXEVE=300000,MAXPAIR=100000000)
	character*100 line 
	integer yr,mo,day,hr,min,nst,gap,clo
	real sec,mag,rms
	real lat(MAXEVE),lon(MAXEVE),dep(MAXEVE)
	integer ids(MAXEVE), cids(MAXEVE), ind(MAXEVE)
	integer iunit,i,j,nev,npair,nev2
c	real hdist,vdist
	real		dist
	real		maxdist, stadist
        real            KMPERDEG
	real dlat,dlon
	real		PI
	real slat, slon,del,az
	integer ct, size
	integer np2, np3, np4, np5, np6

	parameter	(PI=3.141593)
        parameter       (KMPERDEG=111.1949266)


c	integer data(MAXEVE+2+2*MAXPAIR)
c	character str*801200008
c	equivalence(data,str)

c --- Get distance thresholds
c	write(*,*) "Enter horizontal and vertical distance thresholds (km):"
c	read(*,*) hdist, vdist
c	write(*,*) 'Enter max separation distance (km):'
c	read(*,*) maxdist


c --- Read input catalog
	write(*,*) "Reading in catalog file..."
        call freeunit(iunit)
      	open(iunit,file='ncsn.cat',status='unknown')

c	read(iunit,'(a)') line		
c	print *, line
	i=1
10	read(iunit,*,end=20) yr,mo,day,hr,min,sec,lat(i),lon(i),dep(i),
     &                mag,nst,gap,clo,rms,ids(i)
	if(i.lt.5) print *, yr,mo,day,hr,min,ids(i)
	i=i+1
	goto 10
20	nev=i-1
	close(iunit)
	write(*,*) 'Number of events: ', nev

c --- Select event subset
	call freeunit(iunit)
	open(iunit,file='/data/play9/ncsn/NC.MDR/ids',status='unknown')
	i=1
30	read(iunit,*,end=40) cids(i)
	i=i+1
	goto 30
40	nev2=i-1
	close(iunit)

        call indexmap(nev,ids,nev2,cids,ind)

	ct=0
	do i=1,nev
           if(ind(i).ne.0) then
	      ct=ct+1
	      lat(ct)=lat(i)
	      lon(ct)=lon(i)
	      dep(ct)=dep(i)
	      ids(ct)=ids(i)
c	      data(ct+1)=ids(i)
	   endif
	enddo
	nev=ct

	write(*,*) 'Number of events after event select: ', nev
c	data(1)=nev


c --- Select events within 150 km of station
c	ct=0
c	slat=37.206833   
c	slon=-121.797333
c	stadist=150.
c	maxdist=6.
c	data(1)=nev
c	do i=1,nev
c	   call delaz(lat(i),lon(i),slat,slon,del,dist,az)
cc	   print *, lat(i), lon(i), slat, slon, del, dist, az, i
c           if(dist.le.stadist) then
c	      ct=ct+1
c	      lat(ct)=lat(i)
c	      lon(ct)=lon(i)
c	      dep(ct)=dep(i)
c	      ids(ct)=ids(i)
c	      data(ct+1)=ids(i)
c	   endif
c	enddo
c	nev=ct
c
c	write(*,*) 'Number of events after station distance: ', nev
c	data(1)=nev

c --- Compute pairs 
	write(*,*) 'Computing pairs...'
	npair=0
	np2=0
	np3=0
	np4=0
	np5=0
	np6=0
        do i= 1,nev-1 
	   if(mod(i,5000).eq.0) write(*,*) 'On event: ', i, 
     &                          ' with npairs: ', npair
           do j=i+1,nev
              dlat= lat(i) - lat(j)
              dlon= lon(i) - lon(j)
              dist= sqrt( (dlat*KMPERDEG)**2 +
     &                     (dlon*(cos(lat(i)*PI/180)*KMPERDEG))**2 +
     &                     (dep(i)-dep(j))**2 )

		if(dist.le.2) np2=np2+1
		if(dist.le.3) np3=np3+1
		if(dist.le.4) np4=np4+1
		if(dist.le.4.5) np5=np5+1
		if(dist.le.6) np6=np6+1

			npair=np3

c              if(dist.le.maxdist) then
c                  npair=npair+1  
c	          data(2*npair+nev+1)=i
c	   	  data(2*npair+nev+2)=j
c              endif
           enddo
        enddo
	write(*,*) 'Number of pairs: ', npair

	print *, np2,np3,np4,np5,np6
c	data(nev+2)=npair

c --- Output to binary file
c	size=4*(nev+2+2*npair)
c	write(*,*) 'Writing out binary file (bytes): ', size
c	call freeunit(iunit)
c        open(iunit,file='pair.out',form='unformatted',access='direct',
c     &          recl=size)
c	write(iunit,rec=1) str(1:size)
c	close(iunit)


	end


c ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c Compute distance and azimuth on a sphere

	subroutine delaz(alat, alon, blat, blon, del, dist, az)

c	computes distance and azimuth from a to b
c	a and b are in decimal degrees and n-e coordinates
c	del -- delta in degrees
c	dist -- distance in km
c	az -- azimuth from a to b clockwise from north in degrees

c	Original author:  Bruce Julian
c	new values for radius used (from Edi Kissling)

	implicit none

c	Parameters:
	real	alat, alon	! Coordinates of first point
	real	blat, blon	! Coordinates of second point
	real	del		! Central angle (degrees)
	real	dist		! Distance (km)
	real	az		! Azimuth from a to b (degrees)

	real	xtop
	real	xden

c	Local variables:
	doubleprecision	acol, bcol
	doubleprecision	azr
	doubleprecision	blatr, blonr
	doubleprecision	colat
	doubleprecision	cosdel
	doubleprecision	delr
	doubleprecision	flat
	doubleprecision	geoa
	doubleprecision	geob
	doubleprecision	rad
	doubleprecision	radius
	doubleprecision alatr, alonr
	doubleprecision diflon
	doubleprecision pi2
	doubleprecision tana, tanb

c	Built-in functions:  Declarations not needed
	doubleprecision dtan
	doubleprecision	datan
	doubleprecision	dsin
	doubleprecision	dcos
	doubleprecision	dacos

c	doubleprecision top,den

	data pi2/1.570796d0/
	data rad/1.745329d-02/
	data flat/.993231d0/

c-----convert to radians
	alatr=alat*rad
	alonr=alon*rad
	blatr=blat*rad
	blonr=blon*rad
c-----convert latitudes to geocentric colatitudes
	tana=flat*dtan(alatr)
	geoa=datan(tana)
	acol=pi2-geoa
	tanb=flat*dtan(blatr)
	geob=datan(tanb)
	bcol=pi2-geob
c-----calculate delta
	diflon=blonr-alonr
	cosdel=dsin(acol)*dsin(bcol)*dcos(diflon)+dcos(acol)*
     &	dcos(bcol)
	delr=dacos(cosdel)
c-----calculate azimuth from a to b

c*****	Note the use of single precision xtop and xden instead
c	of the double precision top and den in the original
c	program.
c*****	Note also the call to atan2 instead of datan2.
c	Both of these changes were made so that dyn.load
c	would work in Splus.  For some reason, the ld command
c	ld -r -d didn't find _d_atan2
c						WLE 10/16/91

	xtop = dsin(diflon)
	xden=(dsin(acol)/dtan(bcol))-dcos(diflon)*dcos(acol)
	azr=atan2(xtop,xden)
c----- convert to degrees
	del=delr/rad
	az=azr/rad
	if(az.lt.0.0) az=360.+az
c-----compute distance in kilometers
	colat=pi2-(alatr+blatr)/2.d0
	radius=6378.163d0*
     &	(1.d0+3.35278d-3*((1.d0/3.d0)-(dcos(colat)**2)))
	dist=delr*radius
	return
c  ***** end of subroutine delaz *****
	end


c Length of character string, excluding trailing blanks

	integer function trimlen(t)

	implicit none

c	Parameter:
	character t*(*)

      do 1 trimlen=LEN(t),1,-1
    1    if(t(trimlen:trimlen).ne.' ')RETURN
      trimlen=1
      end ! of integer function trimlen
