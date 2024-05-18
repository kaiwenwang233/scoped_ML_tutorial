	subroutine crosscorr2(X,Y,Npts,Npadin,CC,RIoff)
	real x(*),Y(*),XY,CC,RIoff, CCold
	real CXX,CYY,XYmax,denom 
	real xmean, ymean
        logical INTERP
        real Amtrx(5,3),yt(5),Atrans(3,5),ATA(3,3),ATAI(3,3),ATB(3)
        integer INDX(3)
	integer Npts, Npadin
	
c Remove mean from data
c	xmean=0.
c	ymean=0.
c	do i=1,Npts
c	   xmean=xmean+x(i)
c	   ymean=ymean+y(i)
c	end do
c	xmean=xmean/Npts
c	ymean=ymean/Npts
c	print *, xmean, ymean

c	do i=1,Npts
c	   x(i)=x(i)-xmean
c	   y(i)=y(i)-ymean
c	end do


        INTERP=.true.
	nstep=1
c	nstep=NstepIn
        if(Nstep .gt. NpadIn/2)Nstep=NpadIn/2
	Npad=(Npadin/nstep)*nstep
c First calculate the crosscorrelation using decimated traces
	ist=Npad+1
	iend=Npts-Npad

	XYmax=0.0
	do j=1,2*Npad,nstep		!only calculate crosscorrelation at every nstep pnts
	   XY=0.0
           iCurroff=j-Npad-1
	   do k=ist,iend,nstep		!only use every nstep point in the two traces
	      XY=XY+X(k)*Y(k+iCurroff)
	   end do
	   if(XYmax .lt. XY)then
	      XYmax=XY
	      ioff=iCurroff
	   endif
	end do


c Now calculate the crosscorrelation at the max-lag using all points
        CXY=0.0
	do j=ist,iend
           k=ioff+j
	   CXY=CXY+X(j)*Y(k)
	end do
	XYmax=CXY


c Now calculate the cross correlation at one lag before and one lag after
c the current max. if both results are less than current max, we are done.
c Otherwise, go in the direction of the greater of the two results until
c the cross correlation starts decreasing. Then we are done.
	XYminus=0.0
	ioffminus=ioff-1
	do k=ist,iend
	   XYminus=XYminus+X(k)*Y(k+ioffminus)
	end do

	XYplus=0.0
	ioffplus=ioff+1
	do k=ist,iend
	   XYplus=XYplus+X(k)*Y(k+ioffplus)
	end do

	if(XYmax .gt. XYplus .and. XYmax .gt. XYminus)goto 100

	if(XYminus .gt. XYplus)then
           dowhile(XYminus .gt. XYmax)
	      XYmax=XYminus
	      XYminus=0.0
	      ioffminus=ioffminus-1
	      do k=ist,iend
	         XYminus=XYminus+X(k)*Y(k+ioffminus)
	      end do
	   end do
	   ioff=ioffminus+1
	   goto 100
	else
           dowhile(XYplus .gt. XYmax)
	      XYmax=XYplus
	      XYplus=0.0
	      ioffplus=ioffplus+1
	      do k=ist,iend
	         XYplus=XYplus+X(k)*Y(k+ioffplus)
	      end do
	   end do
	   ioff=ioffplus-1
	   goto 100
	endif


	CXX=0.0
	CYY=0.0
	do j=ist,iend
           k=ioff+j
	   CXX=CXX+X(j)*X(j)
	   CYY=CYY+Y(k)*Y(k)
	end do
	denom=sqrt(CXX*CYY)
	CC=XYmax/denom
	RIoff=ioff
	if(.not. INTERP)then
	   write(*,*)'returning early'
           return
	endif

c If the first estimate of the maximum cross correlation was the best
c then we skip to here.

c  calculate the two autocorrelation maxima
100	CXX=0.0
	CYY=0.0
        CXY=0.0
	do j=ist,iend
           k=ioff+j
	   CXX=CXX+X(j)*X(j)
	   CYY=CYY+Y(k)*Y(k)
	   CXY=CXY+X(j)*Y(k)
	end do
	denom=sqrt(CXX*CYY)
	CC=CXY/denom
	RIoff=ioff
	CCold=CC

c	return

c Here interpolate the maximum of the cross correlation function by fitting
c a quadratic to 5 points around the current max by least squares, and then
c using the analytic expression for the maximum of the quadratic (xmax=-b/2a)
c 
c Modified to only return interpolated shift for subsample precision because
c for high CCs sometimes interpolated CC will be greater than one and for
c low CCs, it will be less than zero.

        if(INTERP)then
           do m=1,5
              joff=ioff-3+m
 	      CXX=0.0
	      CYY=0.0
              CXY=0.0
	      do j=ist,iend
                 k=joff+j
	         CXX=CXX+X(j)*X(j)
	         CYY=CYY+Y(k)*Y(k)
	         CXY=CXY+X(j)*Y(k)
	      end do
	      denom=sqrt(CXX*CYY)
	      yt(m)=CXY/denom
              Amtrx(m,1)=joff*joff
	      Amtrx(m,2)=joff
              Amtrx(m,3)=1.0
              Atrans(1,m)=Amtrx(m,1)
              Atrans(2,m)=Amtrx(m,2)
              Atrans(3,m)=Amtrx(m,3)
           end do
c          calculate Atranspose * A
           do m=1,3
              do k=1,3
                 ATA(m,k)=0.0
                 do l=1,5
                    ATA(m,k)=ATA(m,k)+Atrans(m,l)*Amtrx(l,k)
                 end do
              end do
           end do
c          Now get the inverse of ATA
           do i=1,3
              do j=1,3
                 ATAI(i,j)=0.0
              end do
              ATAI(i,i)=1.0
           end do
           call ludcmp(ATA,3,3,INDX,D)
           do j=1,3
              call lubksb(ATA,3,3,INDX,ATAI(1,j)) 
           end do  
      

c       Now calculate Atranspose*yt
           do j=1,3
              ATB(j)=0.0
              do m=1,5
                 ATB(j)=ATB(j)+Atrans(j,m)*yt(m)
              end do
           end do

c       finally calculate  ATAI * ATB 
           aa=0.0
           bb=0.0
           ccc=0.0
           do j=1,3
              aa=aa+ATAI(1,j)*ATB(j)
              bb=bb+ATAI(2,j)*ATB(j)
              ccc=ccc+ATAI(3,j)*ATB(j)
           end do
c       And finally the solution...
           xmax=-bb/2/aa
           ymax=aa*xmax*xmax+bb*xmax+ccc
c       If the subsample interpolation is less than 1 sample different keep.
c       Otherwise parabola fit could be widely in error, due to low CC.
	   if(abs(xmax-RIoff).lt.1)  RIoff=xmax 
        endif

	return
	end


      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
      PARAMETER (NMAX=100,TINY=1.0E-20)
      DIMENSION A(NP,NP),INDX(N),VV(NMAX)
      D=1.
      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
        IF (AAMAX.EQ.0.) PAUSE 'Singular matrix.'
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0.)A(J,J)=TINY
          DUM=1./A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      IF(A(N,N).EQ.0.)A(N,N)=TINY
      RETURN
      END


      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      DIMENSION A(NP,NP),INDX(N),B(N)
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END
