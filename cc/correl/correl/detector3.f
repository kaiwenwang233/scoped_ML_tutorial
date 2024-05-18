	program detector3

	real CXX,CYY,CXY,denom, CCtmp, CC
	integer Npts,wlen,nlag,ioff,joff,i,j,k,ix,iy, time
	integer system
	parameter(wlen=1000,time=100000)
	real X(wlen)
	real Y(time)



	CC=0.0
	do i=1,wlen
	 X(i)=i
	enddo

	do i=1,time
	 Y(i)=i
	enddo
	

	nlag=time-wlen
	ioff=1

	i=system('date')
	do joff=1,nlag
	  CXX=0.0
	  CYY=0.0
          CXY=0.0
	  do k=1,wlen
            i=ioff+k-1
	    j=joff+k-1
	    CXX=CXX+X(i)*X(i)	! could place this outside of two loops
	    CYY=CYY+Y(j)*Y(j)
	    CXY=CXY+X(i)*Y(j)
	  enddo
	  denom=sqrt(CXX*CYY)
	  CCtmp=CXY/denom
	  if(CC.lt.CCtmp) then
	    CC=CCtmp
	    ix=ioff
	    iy=joff
	  endif
	enddo


	i=system('date')
	end
