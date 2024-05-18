      subroutine xcorrf(data1,data2,n,INTERP,cc,dt)
C
C this subroutine determines the crosscorrelation maximum CC
C and the lag DT from input data DATA1 and DATA2 with
C both have length N, which must be a power of two.
C
C computes up to -N,+N-1 lags
C
C G.Bokelmann, 1999
C

C do real*8?  use originals to test keep only cc and dt here
C sign only can do 128 or 256 because of gateway?

      parameter(nmax=8192)
      dimension data1(n),data2(n)
      real cc, dt
      real cross(2*nmax)
      integer lags(2*nmax)
      real ans(4*nmax),d1(2*nmax),d2(2*nmax)
      real a,b,c
	real yt(5)
C      complex ans(n)
      integer noff, noff1, INTERP
C zero-padding
      n1=n/2
      n2=2*n
      n3=n+n/2
      do 10 i=1,n/2
      d1(i)=0.
      d1(i+n3)=0.
      d2(i)=0.
      d2(i+n3)=0.
10    continue
      do 15 i=1,n
      d1(i+n1)=data1(i)
      d2(i+n1)=data2(i)
15    continue
      call correl(d2,d1,n2,ans)
C
      noff=n2/2
      noff1=noff+1
      cc=0.0  
      do 11 i=1,noff
      cross(i)=ans(i+noff)
      lags(i)=i-noff1
11    continue
      noff=n2/2
      do 12 i=1,n2/2
      cross(i+noff)=ans(i)
      lags(i+noff)=i-1
12    continue
C

c  Find peak in correlation function, absolute value maximum
	do  i=1,2*n
	   if(abs(cc) .lt. abs(cross(i)))then
		cc=cross(i)
		dt=float(lags(i))
	   endif
	end do

	if(INTERP.eq.1) then

c  Fit a parabola to 5 points around peak
        do m=1,5
              joff=int(dt)-3+m
	      yt(m)=cross(joff+noff1)
	end do

	call parabfit(yt,int(dt),a,b,c)

c       And finally the solution...
           xmax=-b/(2*a)
           ymax=a*xmax*xmax+b*xmax+c

	dt=xmax
c	cc=ymax

	endif

	return
	end

c Here interpolate the maximum of the cross correlation function by fitting
c a quadratic to 5 points around the current max by least squares, and then
c using the analytic expression for the maximum of the quadratic (xmax=-b/2a)
c 
c Modified to only return interpolated shift for subsample precision because
c for high CCs sometimes interpolated CC will be greater than one and for
c low CCs, it will be less than zero.

	subroutine parabfit(yt,ioff,aa,bb,ccc)
        real Amtrx(5,3),yt(5),Atrans(3,5),ATA(3,3),ATAI(3,3),ATB(3)
	real aa, bb, ccc
	integer ioff

           do m=1,5
              joff=ioff-3+m
c	      yt(m)=CXY/denom
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

c why does commenting these out cause errors?
           xmax=-bb/2/aa
           ymax=aa*xmax*xmax+bb*xmax+ccc


	return
	end


C-----------------------------------------
      subroutine correl(data1,data2,n,ans)
C crosscorrelation
C normalization: divide by sqrt of spectral power
      parameter(nmax=8192)
      dimension data1(n),data2(n)
      complex fft(nmax),ans(n)
      real a,b,c
      call twofft(data1,data2,fft,ans,n)
      a=0.
      b=0.
      do 10 i=1,n
      a=a+fft(i)*conjg(fft(i))
      b=b+ans(i)*conjg(ans(i))
10    continue
      c=sqrt(a)*sqrt(b)*0.5
      do 11 i=1,n/2+1
        ans(i)=fft(i)*conjg(ans(i))/c
11    continue
      ans(1)=cmplx(real(ans(1)),real(ans(n/2+1)))
      call realft(ans,n/2,-1)
      return
      end
C-----------
      SUBROUTINE TWOFFT(DATA1,DATA2,FFT1,FFT2,N)
      DIMENSION DATA1(N),DATA2(N)
      COMPLEX FFT1(N),FFT2(N),H1,H2,C1,C2
      C1=CMPLX(0.5,0.0)
      C2=CMPLX(0.0,-0.5)
      DO 11 J=1,N
        FFT1(J)=CMPLX(DATA1(J),DATA2(J))
11    CONTINUE
      CALL FOUR1(FFT1,N,1)
      FFT2(1)=CMPLX(AIMAG(FFT1(1)),0.0)
      FFT1(1)=CMPLX(REAL(FFT1(1)),0.0)
      N2=N+2
      DO 12 J=2,N/2+1
        H1=C1*(FFT1(J)+CONJG(FFT1(N2-J)))
        H2=C2*(FFT1(J)-CONJG(FFT1(N2-J)))
        FFT1(J)=H1
        FFT1(N2-J)=CONJG(H1)
        FFT2(J)=H2
        FFT2(N2-J)=CONJG(H2)
12    CONTINUE
      RETURN
      END
C-----------
      SUBROUTINE FOUR1(DATA,NN,ISIGN)
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      N=2*NN
      J=1
      DO 11 I=1,N,2
        IF(J.GT.I)THEN
          TEMPR=DATA(J)
          TEMPI=DATA(J+1)
          DATA(J)=DATA(I)
          DATA(J+1)=DATA(I+1)
          DATA(I)=TEMPR
          DATA(I+1)=TEMPI
        ENDIF
        M=N/2
1       IF ((M.GE.2).AND.(J.GT.M)) THEN
          J=J-M
          M=M/2
        GO TO 1
        ENDIF
        J=J+M
11    CONTINUE
      MMAX=2
2     IF (N.GT.MMAX) THEN
        ISTEP=2*MMAX
        THETA=6.28318530717959D0/(ISIGN*MMAX)
        WPR=-2.D0*DSIN(0.5D0*THETA)**2
        WPI=DSIN(THETA)
        WR=1.D0
        WI=0.D0
        DO 13 M=1,MMAX,2
          DO 12 I=M,N,ISTEP
            J=I+MMAX
            TEMPR=SNGL(WR)*DATA(J)-SNGL(WI)*DATA(J+1)
            TEMPI=SNGL(WR)*DATA(J+1)+SNGL(WI)*DATA(J)
            DATA(J)=DATA(I)-TEMPR
            DATA(J+1)=DATA(I+1)-TEMPI
            DATA(I)=DATA(I)+TEMPR
            DATA(I+1)=DATA(I+1)+TEMPI
12        CONTINUE
          WTEMP=WR
          WR=WR*WPR-WI*WPI+WR
          WI=WI*WPR+WTEMP*WPI+WI
13      CONTINUE
        MMAX=ISTEP
      GO TO 2
      ENDIF
      RETURN
      END
C-----------
      SUBROUTINE REALFT(DATA,N,ISIGN)
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      THETA=6.28318530717959D0/2.0D0/DBLE(N)
      C1=0.5
      IF (ISIGN.EQ.1) THEN
        C2=-0.5
        CALL FOUR1(DATA,N,+1)
      ELSE
        C2=0.5
        THETA=-THETA
      ENDIF
      WPR=-2.0D0*DSIN(0.5D0*THETA)**2
      WPI=DSIN(THETA)
      WR=1.0D0+WPR
      WI=WPI
      N2P3=2*N+3
      DO 11 I=2,N/2+1
        I1=2*I-1
        I2=I1+1
        I3=N2P3-I2
        I4=I3+1
        WRS=SNGL(WR)
        WIS=SNGL(WI)
        H1R=C1*(DATA(I1)+DATA(I3))
        H1I=C1*(DATA(I2)-DATA(I4))
        H2R=-C2*(DATA(I2)+DATA(I4))
        H2I=C2*(DATA(I1)-DATA(I3))
        DATA(I1)=H1R+WRS*H2R-WIS*H2I
        DATA(I2)=H1I+WRS*H2I+WIS*H2R
        DATA(I3)=H1R-WRS*H2R+WIS*H2I
        DATA(I4)=-H1I+WRS*H2I+WIS*H2R
        WTEMP=WR
        WR=WR*WPR-WI*WPI+WR
        WI=WI*WPR+WTEMP*WPI+WI
11    CONTINUE
      IF (ISIGN.EQ.1) THEN
        H1R=DATA(1)
        DATA(1)=H1R+DATA(2)
        DATA(2)=H1R-DATA(2)
      ELSE
        H1R=DATA(1)
        DATA(1)=C1*(H1R+DATA(2))
        DATA(2)=C1*(H1R-DATA(2))
        CALL FOUR1(DATA,N,-1)
      ENDIF
      RETURN
      END
