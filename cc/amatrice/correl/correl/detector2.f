
	subroutine detector2(X,Y,Npts,wlen,CC,ix,iy)
	real X(*),Y(*),CC
	real CXX,CYY,CXY,denom, CCtmp
	integer Npts,wlen,nlag,ioff,joff,i,j,k,ix,iy


	CC=0.0

	nlag=Npts-wlen

c	do ioff=1,nlag,nstep

	do ioff=1,nlag
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
	enddo

	return
	end
