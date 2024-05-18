	subroutine updatemseedheader(fil,iyr,imo,idy,ihr,imn,
     &				sec,ttp,iswap,lunit)

      implicit none

c     -- include common block of header variables (hdr_...) and constants (c_...)
      include 'codeco_common.f'

	character*100 fil
	character*100 hdrfile
	logical iswap
	integer	iyr, imo, idy, ihr, imn	, lunit
	real sec, ttp, tts

c	Local:
	real sec2, seconds, ksec2, kms2 
	integer iunit
	integer isec, ims, ijulm, idoy, ijulm2
	integer	iyr2, imo2, idy2, ihr2, imn2, isec2, ims2, idoy2
	integer trimlen
	integer juliam
      	logical ihead	     ! return only header if true
      	integer*4 iy(c_sigsize)
      	integer*4 ierr
      	character infile*80

	ihead=.true.

	infile=fil(1:trimlen(fil))
	call mseedin( infile, iy, ierr, iswap, ihead)

	if (ierr.ne.0) goto 50

	iyr2=hdr_year
	imo2=hdr_month
	idy2=hdr_day
	ihr2=hdr_hour
	imn2=hdr_min
	isec2=hdr_sec
	ims2=hdr_msec

	ijulm=juliam(iyr, imo, idy, ihr, imn)
	ijulm2=juliam(iyr2, imo2, idy2, ihr2, imn2)
	ksec2=isec2
	kms2=ims2
	sec2=ksec2+kms2/1000
	seconds=(ijulm2-ijulm)*60 + sec2-sec
	tts=ttp*1.732

	hdrfile=fil(1:trimlen(fil))// '.hdr'
        call freeunit(iunit)
	open(iunit,file=hdrfile,status='unknown')

	write(iunit,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3,2x,f11.4,
     &  1x,f11.4,1x,f11.4)') iyr,imo,idy,ihr,imn,sec,seconds,ttp,tts


c	print *, iyr,imo,idy,ihr,imn,sec,seconds,ttp,tts

	close(iunit)

	goto 60

50 	write(lunit,*) 'Error reading miniseed header: ', 
     &                  fil(1:trimlen(fil))
	goto 60


60	return


1000	end


