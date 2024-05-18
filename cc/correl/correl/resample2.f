	subroutine resample(dt1,y1,n1,dt2,y2,n2)

c Input:
c	dt1 -- current sample interval
c	y1  -- data vector
c	n1  -- number of points in y1
c	dt2 -- desired sample interval
c
c Output:
c	y2  -- interpolated vector
c	n2  -- number of points in y2
c
c Uses cubic spline interpolation from Numerical Recipes to resample.
c Sample intervals, dt1 & dt2, can be any arbitrary real values.  The
c reference time of the first point is the same for both.  Time vectors
c can be created as follows:
c
c	t1=(0:dt1:(n1-1)*dt1)+tref
c	t2=(0:dt2:(n2-1)*dt2)+tref

c  David Schaff 22-Jul-03, schaff@stanfordalumni.org


        integer n1,n2
	real y1(n1),y2(*),ys(n1),dt1,dt2,t1(n1),t2
	data yp1,ypn/2.0E+30,2.0E+30/


	do i=1,n1
	   t1(i)=(i-1)*dt1
	enddo

	call spline(t1,y1,n1,yp1,ypn,ys)

	n2=int((n1-1)*dt1/dt2)+1
        ilo=1
        ihi=1
	do while(t1(ihi) .lt. dt2)
	   ihi=ihi+1
	enddo
	do j=1,n2
	   t2=(j-1)*dt2
	   call splint(t1,y1,ys,n1,ilo,ihi,t2,y2(j))
	enddo

	return
	end


      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
      POINTER(P1,U)
      REAL U(*)
      DIMENSION X(N),Y(N),Y2(N)

      P1=MALLOC(4*N)
      IF (YP1.GT..99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      CALL FREE(P1)
      RETURN
      END


	subroutine splint(x,y,y2,NPTS,ilo,ihi,t,ynew)
	real x(*),y(*),y2(*)

c first update ilo and ihi if necessary
	do while(x(ilo) .le. t)
	   ilo=ilo+1
	end do
	if(x(ilo) .gt. t)ilo=ilo-1
	if(ilo .lt. 1)ilo=1

	do while(x(ihi) .lt. t)
	   ihi=ihi+1
	   if(ihi .eq. NPTS)goto 10
	end do
10	if(x(ihi).lt.t)then
	   ynew=y(ihi)
	   return
	endif

	H=x(ihi)-x(ilo)
	if(H .eq. 0.0)then
	   ynew=y(ihi)
	   return
	endif

	A=(x(ihi)-t)/H
	B=(t-x(ilo))/H
	ynew=A*y(ilo)+B*y(ihi)+((A**3-A)*y2(ilo)+
     +       (B**3-B)*y2(ihi))*(H**2)/6.0
	return
	end


