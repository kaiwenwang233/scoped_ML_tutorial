      program getpairs

c --- f77 -o getpairs getpairs.f freeunit.f

c     input catalog file contains columns of ids, lat, lon, depth
c     outputs I and J indices along with distance for pairs with 
c     separations less than maxdist
         
      implicit none

      integer*4 MAXEVE
      parameter(MAXEVE=10000)

      character*100 fn_cat, fn_out
      integer	i1, i2, i, iunit		! Master pair indices
      integer	nev, npair			! number of master ids
      integer   ids(MAXEVE)
	real lat(MAXEVE),lon(MAXEVE), dep(MAXEVE)
      	real maxdist, dlat,dlon, dist
	real		PI
        real            KMPERDEG

	parameter	(PI=3.141593)
        parameter       (KMPERDEG=111.1949266)

      
	maxdist=6

	print *, "enter catalog file:"
	read(*,*) fn_cat

	print *, "enter output file:"
	read(*,*) fn_out

c --- Read input catalog
	write(*,*) "Reading in catalog file..."
        call freeunit(iunit)
      	open(iunit,file=fn_cat,status='unknown')

	i=1
10	read(iunit,*,end=20) ids(i),lat(i),lon(i),dep(i)
	i=i+1
	goto 10
20	nev=i-1
	close(iunit)
	write(*,*) 'Number of events: ', nev

c --- Compute pairs (upper triangular)
	write(*,*) 'Computing distances...'
        call freeunit(iunit)
      	open(iunit,file=fn_out,status='unknown')
	npair=0
	do i1=1,nev-1
	   do i2=i1+1,nev

              dlat= lat(i1) - lat(i2)
              dlon= lon(i1) - lon(i2)
              dist= sqrt( (dlat*KMPERDEG)**2 +
     &                     (dlon*(cos(lat(i1)*PI/180)*KMPERDEG))**2 +
     &                     (dep(i1)-dep(i2))**2 )
              if(dist.le.maxdist) then
                  npair=npair+1  
		  write(iunit,'(i6,2x,i6,2x,f6.1)') i1, i2, dist
	      endif

	          if(mod(npair,100000).eq.0) then
	   write(*,'("Done with ",i3," 100K pairs")') 
     &               npair/100000
	          endif

	   enddo
	enddo            

	write(*,*) 'total number of CC measurements: ', npair

	close(iunit)

1000      end
      
      
      
