     	subroutine winalign(y1,y2,wlen,a,b,dtsamp,cc)

c  input y1, y2, wlen
c  output a, b, dtsamp, cc
c  plotwin.m used to test this function

	integer wlen, dtsamp, ishift
	real y1(*), y2(*), yy1(256), yy2(256)
	real a(wlen), b(wlen), dt, cc
	
c perhaps hardwire for 256? seems to get more observations
c having 100 hardwire doesn't give same results as winalign2 which is at 1
c make sure input y1 & y2 have enough points in window=1-120:256+100;
	do i=1,256
	   yy1(i)=y1(i+100+20)
	   yy2(i)=y2(i+100+20)
	end do

	call xcorrf(yy1,yy2,256,0,cc,dt)
c	call xcorrf(yy1,yy2,wlen,0,cc,dt)
	dtsamp=int(dt)

c  Restrict maximum amount of delay adjustment to maxalign = 100
	if(abs(dtsamp).ge.100)then
	   dtsamp=isign(1,dtsamp)*100
	endif

	
c  Shift both traces based on delay so that onset is at 20 position in window
	ishift = 100 - abs(dtsamp)

	if(dtsamp.ge.0)then

	   do i=1,wlen
		a(i)=y1(i+ishift)
		b(i)=y2(i+dtsamp+ishift)
	   end do

	else
	
	   do i=1,wlen
		a(i)=y1(i-dtsamp+ishift)
		b(i)=y2(i+ishift)
	   end do

	endif
		


	return
	end
