c     file mseed_io.f
c          ==========
c
c     substituted 'or' by 'ior' because of HP-UX f77 compiler/loader. 
c     4May2000 uk
c
c     Version from Karl Koch, 25Nov99
c
c     version 2, 20-Aug-98
c
c     Mini-SEED output, uses external program 'write_steim1' of K. Stammler.
c     19-Aug-98, K. Koch
c     modified, 20-Aug-98, K. Stammler


c ==============================================================================



      subroutine mseedout( iy )

c     Writes Mini-SEED files.  Uses external program 'write_steim1'.  Writes
c     separate file for each channel.  Output filenames are created
c     automatically.

      implicit none

c     -- get header variables (hdr_...) and constants (c_...)
      include 'codeco_common.f'

c     -- parameters
      integer          iy(c_sigsize) ! sample array

c     -- local variables
      integer          y2            ! 2-digit year
      character*132    outfile       ! name of output file
      integer*4        i             ! counter

c     -- functions
      integer   trimlen

c     -- executable code

c     -- Create two ASCII files for 'write_steim1'

c     -- write out header provisionally
      open(12,file='GSEDAT2SEED.HDR',status='unknown',err=992)
      write(12,'(a,i6)') 'sequence number: ', 1
      write(12,'(2a)') 'station: ', hdr_station
      write(12,'(2a)') 'channel: ', hdr_chan
      write( 12, '(a,i4,5(a,i2.2),a,i3.3)' )  'start time: ',
     &   hdr_year, '/', hdr_month, '/', hdr_day, '_', hdr_hour,
     &   ':', hdr_min, ':', hdr_sec, '.', hdr_msec
      write( 12, '(a,i4)' )  'sample rate factor: ',
     &   ifix(hdr_smprate+0.5)
      write(12,'(a)') 'sample rate multiplier: 1'
      close( 12 )

c     -- write out ASCII-data provisionally
      open(11,file='GSEDAT2SEED.ASC',status='unknown',err=991)
      do i=1,hdr_nsamp
         write(11,'(i9)') iy(i)
      enddo
      close( 11 )

c     -- make outputfile name
      y2 = hdr_year - 1900
      if  (y2 .ge. 100)  y2 = y2 - 100
      i = trimlen( hdr_station )
      write( outfile, '(2a,3i2.2,a,3i2.2,2a)' )  hdr_station(1:i),
     &   '_', y2, hdr_month, hdr_day, '_', hdr_hour, hdr_min,
     &   hdr_sec, '.', hdr_chan(1:3)
      call casefold( outfile )

      if (hdr_debug .gt. 0)  write(*,*)
     &     '-- Processing GSEDAT2SEED.ASC and GSEDAT2SEED.HDR'
      if (hdr_debug .gt. 0)  write(*,*)
     &     '-- Creating mSEED file ',outfile

c     -- convert ASCII files to Mini-SEED by external program
      call system ('$SEED_PROG/write_steim1 '//
     &   '-- GSEDAT2SEED.HDR GSEDAT2SEED.ASC '//outfile)

c     -- clean up
      if (hdr_debug .gt. 0) write(*,*)
     &     '-- Cleaning up GSEDAT2SEED.ASC and GSEDAT2SEED.HDR'
      if  (hdr_debug .gt. 2)  call system( 'cat GSEDAT2SEED.HDR' )
      call system( '\rm GSEDAT2SEED.HDR GSEDAT2SEED.ASC' )

      return

 991  write (*,*) '** mseedout ---- cannot create outfile'
      return
 992  write (*,*) '** mseedout ---- cannot create headerfile'
      return

      end


c     file mseed_io.f
c          ========
c
c     version 1, 25-Nov-99
c
c     MiniSEED I/O routines for codeco.
c     Original routines by J.M. Steim
c     Modified code from LARS OTTEMOELLER , MARCH 1997 (MSEEDASC.FOR)
c     Adapted for CODECO by K.Koch


c==============================================================================



      subroutine mseedin( infile, iy, ierror, iswap, ihead)
c
c     Basic input routine for MiniSEED data
c
      implicit none
c     -- get header variables (hdr_...) and constants (c_...)
      include 'codeco_common.f'

c     -- parameters
      character*80   infile          ! name of input file (input)
      integer        iy(c_sigsize)   ! sample array (output)
      integer        ierror          ! return status (output)
      logical 	     iswap	     ! swap bytes if true
      logical 	     ihead	     ! return only header if true


c
c     decode.f - a simplified decompressor for Sun FORTRAN77
c
c     ---------------------------------------------------------
c     read, decompress seed data records from vbb seed files
c     ---------------------------------------------------------
c     this is a minimal example program showing how to decompress
c      and  write to an output file the seed data records as generated
c      by quanterra data loggers such as installed at iris sites
c      there is very little error checking in this
c       program, so use it only to get started.
c       consult the SEED Reference Manual V2.3 that describes the
c       'fixed data-record headers' and the 'steim compression algorithm'
c       for a detailed description of the format we are trying to read
c       here.
c      
c                             jms 90/07/10
c
c  modified  by  reason
c  -------- ---  ------------------------------------------------------------
c  91/07/31 jms  4096-byte seed records. this is hardcoded for 4096-byte only.
c                 this will also decode "seed_activity_flag_begin_event" bit
c                 so that expansion error messages are not printed
c                 unnecessarily.
c  93/03/10      will now decode compression level 2.
c  93/03/20 jms  will now decode only recognized SEED channel names.
c  94/01/01 jms  will now decode any SEED channel names, with previously
c                known names retaining their "channel numbers". Will now
c                also append the SEED channel name to the ascii output header.
c                will now open a file only when there are data to write
c                into it, so that many 0-length files are not created.
c                will now look for time tears greater than 1/2 sample period.
c                will now read standard "mini-seed" records with blockette 1000.
c                the input file is expected to contain records from only a
c                single (the first one encountered) station. records 
c                containing other stations will be ignored.
c                a status indicator will now also appear on each header line
c                as up to 8 characters: "XNMTGE--"
c                where: X = decompression (expansion) error
c                       N = number of decompressed samples disagrees with header
c                       M = data missing before decompression (fault)
c                       T = timing is inaccurate (reception loss)
c                       G = time tear relative to previous record
c                       E = beginning of event (not an error)
c                error and status messages will be issued to stdout before
c                the header line to which they pertain.
c  94/03/27 jms  1. will now decode the "first data byte" allowing for
c		 embedded blockettes
c		 2. time gap checking shortened to 0.05 sample time from 0.5
c		 3. status bits ar now (correctly) logicallly accumulated,
c		  not arithmetically.
c  95/04/19 jms  if the header decoding finds neither quanterra-specific
c                 info embedded nor a blockette 1000, will assume
c                 compression level 1, because the file was probably
c                 generated by some non FDSN-SEED compliant SEED writer.
c                   
c
c  The following is the SEED data record header layout. Some items following "word_order"
c  in the data-only blockette 1000 section are unique to Quanterra systems, and are subject
c  to change. this program does not use these values. Most Quanterra systems running SHEAR
c  released before 1994 also encode the compression level in an undocumented area of the
c  header, because at the time of that SHEAR release, blockette 1000 had not yet been
c  defined. This program will look for a blockette 1000, and if present, will use it.
c  Otherwise it will scan the undocumented portion of the header. Therefore, this program
c  cannot be used to read other than Quanterra SEED records if blockette 1000 is not present.
c
c
c   SEED_fixed_data_record_header = record
c 1,2 sequence                      : array[1..6]/char;{ record number }
c  2  seed_record_type              : char ;           { ascii 'D' for data record }
c  2  continuation_record           : char ;
c 3,4 station_ID_call_letters       : array[1..5]/char;
c  4  location_id                   : array[1..2]/char;{ non aligned! }
c 4,5 channel_ID                    : array[1..3]/char;{ non aligned! }
c  5  seednet                       : array[1..2]/char;{ seed network ID }
c  6  yr     =ibits(header(6),16,16): integer ;        { year }
c  6  jday   =ibits(header(6),0,16) : integer ;        { day of year }
c  7  hr     =ibits(header(7),24,8) : byte ;           { hour }
c  7  minute =ibits(header(7),16,8) : byte ;
c  7  seconds=ibits(header(7),8,8)  : byte ;
c  7  unused =ibits(header(7),0,8)  : byte ;
c  8  tenth_millisec                : integer ;
c  8  samples_in_record             : integer ;
c  9  sample_rate_factor            : integer ;
c  9  sample_rate_multiplier        : integer ;        { always 1 }
c 10  activity_flags                : byte;            { ?I?LEBTC }
c 10  IO_flags                      : byte ;
c 10  data_quality_flags            : byte ;           { ???G???? }
c 10  number_of_following_blockettes: byte ;           { normally 0 }
c 11  tenth_msec_correction         : longint ;        { temporarily 0 }
c 12  first_data_byte               : integer ;        { 0 or 48 or 64 - data starts in frame 1 }
c 12  first_blockette_byte          : integer ;        { 0 for SHEAR, or 48 for ULTRA-SHEAR }
c 13  blockette_type          16,16 : integer ;        {if present, 1000 always, in ULTRA-SHEAR}
c 13  next_offset                   : integer ;        {offset to next blockette}
c 14  encoding_format         24,8  : byte ;           {10 = Steim1, 11 = Steim2}
c 14  word_order                    : byte ;           {1 always}
c 14  rec_length                    : byte ;           {8=256, 9=512, 12=4096} { this and following are }
c 14  dob_reserved                  : byte ;           {0 always}              { unique to quanterra }
c 15  comp                          : byte ;           { orig component, Z=0 }
c 15  strm                          : byte ;           { orig stream, VBB, VSP, LG, MP, LP, VLP, ULP ..}
c 15  compname                      : array[1..2]/char;{ orig component name }
c 16  qual                          : byte ;           {-1 to 5 quality indicator}
c 16  usec99                        : byte ;           {0 to 99 microseconds}
c 16  state_of_health               : byte ;           {copy of soh}
c 16  frame_count                   : byte ;           {number of 64 byte data frames}
c   end ;
c

c     -- local variables
c      include 'decode.inc'
      integer blksize,steim1,steim2
      integer iblk, lastvalue,initial 
      integer ifileopen, icurrlevel, iprevlevel, iseedname
      integer maxseedchannels, iexpansion, isamples, imissing
      integer inaccurate, igap, ibeginevent

      parameter (blksize = 4096)
      parameter (iblk = blksize / 4)
      parameter (lastvalue = 1)
      parameter (initial = 2 )
      parameter (ifileopen = 3)
      parameter (icurrlevel = 4)
      parameter (iprevlevel = 5)
      parameter (iseedname = 6)
      parameter (maxseedchannels = 100)
      parameter (steim1 = 10)
      parameter (steim2 = 11)
      parameter (iexpansion = 1)
      parameter (isamples = 2)
      parameter (imissing = 4)
      parameter (inaccurate = 8)
      parameter (igap = 16)
      parameter (ibeginevent = 32)

      integer*4   record(iblk),data(blksize*2)
      integer*4   sname,nsamp,samp1,samplast,rate
      integer*4   yy,dd,hh,mi,sec,msec,ins,nd
      integer*4   chanbuf(6,maxseedchannels),highestchan
      real*8      endtime(maxseedchannels)
      character*8 errstring,chout*4
		integer     irecord, ich, currstation
      logical     event

      integer*4 i

      	character*4 inttmp
        integer*4 itmp
        equivalence(itmp,inttmp)


c------------------------------

c     -- executable code

      if  (hdr_debug .gt. 2)  write(*,*)  '-- entering mseedin'

ckkc     -- test if this is a 512/4096 blocked file ????
ckk
ckk      if  (hdr_debug .gt. 1)  
ckk     &    write(*,"(' now reading ----- ',a40)") infile
ckk
ckk      open(1,file=infile,access='direct',form='unformatted',
ckk     &     err=290,recl=blksize)
ckk
ckkc  ---- Check for 4096-blocking
ckk      read(1,rec=1,err=290,end=300) record
ckk      goto 291
ckk290   write(*,*) ' Record length not 4096'
ckk      ierror=999
ckk      return
ckk291   close(1)

300   continue
      
      if  (hdr_debug .gt. 1)  
     &    write(*,"(' now reading ----- ',a40)") infile
      open(1,file=infile,access='direct',form='unformatted',
     &     status='old',err=2000,recl=blksize)
      irecord=1
      call initialize (chanbuf, highestchan,endtime)
      currstation = 0
      hdr_nsamp=0

      do while (.true.)
      read(1,rec=irecord,err=2000)record

	if(iswap) then
	do i=1,iblk
	 itmp=record(i)
	 call swap(inttmp,4)
	 record(i)=itmp
	enddo
	endif
c
c     --- this loop to be repeated for all the records ---
c

         irecord=irecord+1
         call head (record,sname,msec,
     &             samp1,samplast,rate,yy,dd,hh,mi,sec,ins,nsamp,ich,
     &             chanbuf,highestchan,event,ierror,endtime)
c
c     --- this will enforce a single station per file
c
         if((rate.ne.0).and.(ich.ne.0)) then
            if(currstation.eq.0) currstation = sname
         endif
         if((rate.ne.0).and.(ich.ne.0).and.(currstation.eq.sname)) then
            call dedata (record,samp1,samplast,ich,nsamp,data,nd,ierror,
     &                   chanbuf,event)
            
            if(chanbuf(icurrlevel,ich).ne.chanbuf(iprevlevel,ich)) then
              if  (hdr_debug .gt. 4)  
     &        write(*,"('compression level ',i1)") 
     &        chanbuf(icurrlevel,ich)-steim1+1
              chanbuf(iprevlevel,ich)=chanbuf(icurrlevel,ich)
            endif
            if (nsamp.gt.0) then


c write channel name
                
              if (hdr_nsamp.le.0) then
                  hdr_year=yy
                  hdr_jday=dd
                  hdr_hour=hh
                  hdr_min =mi
                  hdr_sec =sec
                  hdr_msec=msec
                  hdr_smprate=rate
c                  hdr_calfac=samplast
c                  hdr_calper=samp1
                  write(hdr_station,'(a4)') sname
                  write(chout,'(a4)') chanbuf(iseedname,ich)
                  hdr_chan=chout(2:4)
c                  call julien(hdr_day,hdr_month,hdr_year,hdr_jday,0)
		  call datumd(hdr_jday,hdr_year,hdr_month,hdr_day)
		  if (ihead) return
              endif

              if  (hdr_debug .gt. 2)  
     &            write(*,5)irecord-1,ich,sname,samp1,rate,
     &                yy,dd,hh,mi,sec, msec,nsamp,
     &                chanbuf(iseedname,ich),errstring(ierror)
   5           format(i5,1x,i5,1x,a4,i10,8i5,a4,1x,a8)
              if (hdr_nsamp+nsamp .gt. c_sigsize) then
                  write(*,"('Aborting input - DIMENSION ',i10, 
     &            ' exceeded (',i10,')')") c_sigsize, hdr_nsamp
                  return
              endif
              do i=1,nsamp
                  iy(hdr_nsamp+i)=data(i)
              enddo
              hdr_nsamp=hdr_nsamp+nsamp
              if  (hdr_debug .gt. 4)  
     &           write(*,*)  'CUR= ',hdr_nsamp
            endif
          endif
      enddo
 2000 continue
      close(1)

      return
      end




      subroutine initialize (chanbuf, high,endtime)
c      include 'decode.inc'
      integer blksize,steim1,steim2
      parameter (blksize = 4096)
      parameter (iblk = blksize / 4)
      parameter (lastvalue = 1)
      parameter (initial = 2 )
      parameter (ifileopen = 3)
      parameter (icurrlevel = 4)
      parameter (iprevlevel = 5)
      parameter (iseedname = 6)
      parameter (maxseedchannels = 100)
      parameter (steim1 = 10)
      parameter (steim2 = 11)
      parameter (iexpansion = 1)
      parameter (isamples = 2)
      parameter (imissing = 4)
      parameter (inaccurate = 8)
      parameter (igap = 16)
      parameter (ibeginevent = 32)

      integer*4 chanbuf(6,maxseedchannels),high
      real*8 endtime(maxseedchannels)
      integer*4 strnam(33)
      character*4 strnamx(33)
      equivalence (strnam,strnamx)
      data strnamx /' BHZ',' BHN',' BHE',
     &             ' HHZ',' HHN',' HHE',
     &             ' HLZ',' HLN',' HLE',
     &             ' LHZ',' LHN',' LHE',
     &             ' VHZ',' VHN',' VHE',
     &             ' UHZ',' UHN',' UHE',
     &             ' VMZ',' NMZ',' EMZ',
     &             ' EHZ',' EHN',' EHE',
     &             ' ELZ',' ELN',' ELE',
     &             ' BCI',' ECI',' HCI',
     &             ' LCI',' VCI',' UCI'/
      do 10 k=1,maxseedchannels
         chanbuf(lastvalue,k)=0
         chanbuf(initial,k)=1
         chanbuf(ifileopen,k)=0
         chanbuf(icurrlevel,k)=0
         chanbuf(iprevlevel,k)=0
         endtime(k)=0.0d0
         chanbuf(iseedname,k)=0
   10 continue
      do 20 k=1,33
         chanbuf(iseedname,k)=strnam(k)
   20 continue
      high=33
      return
      end



      subroutine cdump (chanbuf,high)
c      include 'decode.inc'
      integer blksize,steim1,steim2
      parameter (blksize = 4096)
      parameter (iblk = blksize / 4)
      parameter (lastvalue = 1)
      parameter (initial = 2 )
      parameter (ifileopen = 3)
      parameter (icurrlevel = 4)
      parameter (iprevlevel = 5)
      parameter (iseedname = 6)
      parameter (maxseedchannels = 100)
      parameter (steim1 = 10)
      parameter (steim2 = 11)
      parameter (iexpansion = 1)
      parameter (isamples = 2)
      parameter (imissing = 4)
      parameter (inaccurate = 8)
      parameter (igap = 16)
      parameter (ibeginevent = 32)

      integer*4 chanbuf(6,maxseedchannels),high
      do 10 k=1,high
         write(*,5) k,chanbuf(lastvalue,k),
     &              chanbuf(initial,k), chanbuf(ifileopen,k),
     &              chanbuf(icurrlevel,k),chanbuf(iprevlevel,k),
     &              chanbuf(iseedname,k),
     &              chanbuf(iseedname,k)
    5       format(6(i2,1h ),1h",a4,1h",1h ,z8) 
   10 continue
      return
      end


      character*8 function errstring (ierr)
c      include 'decode.inc'
      integer blksize,steim1,steim2
      parameter (blksize = 4096)
      parameter (iblk = blksize / 4)
      parameter (lastvalue = 1)
      parameter (initial = 2 )
      parameter (ifileopen = 3)
      parameter (icurrlevel = 4)
      parameter (iprevlevel = 5)
      parameter (iseedname = 6)
      parameter (maxseedchannels = 100)
      parameter (steim1 = 10)
      parameter (steim2 = 11)
      parameter (iexpansion = 1)
      parameter (isamples = 2)
      parameter (imissing = 4)
      parameter (inaccurate = 8)
      parameter (igap = 16)
      parameter (ibeginevent = 32)

      integer*4 ierr
      character*8 temp
      temp = '--------'
      if (iand(ierr,iexpansion).ne.0) 	temp(1:1) = 'X'
      if (iand(ierr,isamples).ne.0) 	temp(2:2) = 'N'
      if (iand(ierr,imissing).ne.0) 	temp(3:3) = 'M'
      if (iand(ierr,inaccurate).ne.0) 	temp(4:4) = 'T'
      if (iand(ierr,igap).ne.0) 	temp(5:5) = 'G'
      if (iand(ierr,ibeginevent).ne.0) 	temp(6:6) = 'E'
      errstring = temp
      return
      end


      character*4 function int2ch (intparam)
      character*4 intparam
      int2ch=intparam
      return
      end


      function nbcha(c1,nc1)
c   returns # of leading non-blank characters
c   nc1=# of characters in c1
      character   c1*80 
      do  k=1, nc1
          l=nc1-k+1
          if(c1(l:l).ne.' ') then
               lnblank=k-1
               nbcha=nc1-lnblank
               return
          end if
      end do
      end

      integer*4 function j84time(yy,dd,hh,mi,sec)
      integer*4 yy,dd,hh,mi,sec,temp
      integer*4 julcal(65)
      data julcal    /0, 31622400,  63158400,  94694400, 126230400,
     &                  157852800, 189388800, 220924800, 252460800,
     &                  284083200, 315619200, 347155200, 378691200,
     &                  410313600, 441849600, 473385600, 504921600,
     &                  536544000, 568080000, 599616000, 631152000,
     &                  662774400, 694310400, 725846400, 757382400,
     &                  789004800, 820540800, 852076800, 883612800,
     &                  915235200, 946771200, 978307200,1009843200,
     &                 1041465600,1073001600,1104537600,1136073600,
     &                 1167696000,1199232000,1230768000,1262304000,
     &                 1293926400,1325462400,1356998400,1388534400,
     &                 1420156800,1451692800,1483228800,1514764800,
     &                 1546387200,1577923200,1609459200,1640995200,
     &                 1672617600,1704153600,1735689600,1767225600,
     &                 1798848000,1830384000,1861920000,1893456000,
     &                 1925078400,1956614400,1988150400,2019686400/
      if((yy.lt.1984).or.(yy.gt.2048)) then
        j84time=-1
        return
      endif
      temp=julcal(yy-1983)+((dd-1)*86400)+(hh*3600)+(mi*60)+sec
      j84time=temp
      return
      end


      subroutine head (header,sname,msec,
     &             samp1,samplast,rate,yy,dd,hh,mi,sec,ins,nsamp,ich,
     &             chanbuf,high,event,ierr,endtime)
c
c      include 'decode.inc'
      integer blksize,steim1,steim2
      parameter (blksize = 4096)
      parameter (iblk = blksize / 4)
      parameter (lastvalue = 1)
      parameter (initial = 2 )
      parameter (ifileopen = 3)
      parameter (icurrlevel = 4)
      parameter (iprevlevel = 5)
      parameter (iseedname = 6)
      parameter (maxseedchannels = 100)
      parameter (steim1 = 10)
      parameter (steim2 = 11)
      parameter (iexpansion = 1)
      parameter (isamples = 2)
      parameter (imissing = 4)
      parameter (inaccurate = 8)
      parameter (igap = 16)
      parameter (ibeginevent = 32)

      integer*4   header(iblk),sname,samp1,samplast
      integer*4   rate,yy,dd,hh,sec
      integer*4   chname
      integer*4   chanbuf(6,maxseedchannels),high
      logical     event
      integer*4   seed_qual_missing_data
      integer*4   seed_qual_questionable_timetag
      integer*4   seed_act_begin_event
      integer*4   quality
      real*8      temp,endtime(maxseedchannels),secsam
      data        seed_qual_missing_data /z'00000010'/
      data        seed_qual_questionable_timetag /z'00000080'/
      data        seed_act_begin_event /z'00000004'/
c
c NOTE: some error checking could be done here to see that the
c       data about to be processed are syntactically-correct seed
c       data records, otherwise some long string of drivel may result.
c
      ierr   = 0
      chname = ibits(header(4),0,16)*65536+ibits(header(5),16,16)
      ich    = ichnnl(chname,chanbuf,high)
      sname  = header(3)
      msec   = ic2(ibits(header(8),16,16))/10
      if((msec.lt.0).or.(msec.gt.999)) then
               write(*,
     &           "('warning: out-of-range msec correction: ',i10)")  msec
      endif
c        "first_data_byte" contains the starting data byte, which we
c	  convert to longword index. in a record with no blockettes,
c	  the first data byte is 64.
      ifirstblockindex=ibits(header(12),16,16)/4
      samp1  = header(ifirstblockindex+2)
      samplast  = header(ifirstblockindex+3)
      rate   = ic2(ibits(header(9),16,16))
      yy = ibits(header(6),16,16)
      dd = ibits(header(6),0,16)
      hh = ibits(header(7),24,8)
      mi = ibits(header(7),16,8)
      sec= ibits(header(7),8,8)
      nsamp=ibits(header(8),0,16)
c
c     'rate' is samples/sec (if .gt.0) or sec/sample (if .lt.0)
c
      if(rate.gt.0) secsam = 1./float(rate)
      if(rate.lt.0) secsam = float(-rate)
c
c extract the beginning-of-event flag
c
      event=(iand(ibits(header(10),24,8),seed_act_begin_event).ne.0)
      if (event) ierr = ior(ierr,ibeginevent)
c
c check for
c time gaps between records that should be temporally contiguous.
c
      if(ich.ne.0) then
        if((.not. event).and.(chanbuf(initial,ich).eq.0)) then
          if(endtime(ich).ne.0.0d0) then
            temp = j84time(yy,dd,hh,mi,sec) + (msec+500.0d0)/1000.0d0
     &              - endtime(ich) - secsam
            if(temp.gt.0.05d0*secsam) then
               write(*,
     &           "('warning: time gap (sec): ',f15.3)") sngl(temp)
               ierr = ior(ierr,igap)
            endif
          endif
        endif
      endif
c
c calculate this record ending time to the nearest second, for next time.
c
      if((ich.ne.0).and.(rate.ne.0)) then
          endtime(ich)=
     &      j84time(yy,dd,hh,mi,sec)+(msec+500.0d0)/1000.0+nsamp*secsam
        else
          endtime(ich)=0.0
      endif
c
c see if a "data-only" blockette (1000) is present. if so, read compression level from there.
c if not, read compression level from the quanterra-specific information in the unused portion
c of the seed data record header. this info is available through SHEAR version 34/02-1231,
c and contains 1 for level 1, and 2 for level 2.
c if neither of these pieces of info are available, assume level 1.
c
      if(ich.ne.0) then
        if(ibits(header(13),16,16).eq.1000) then
            chanbuf(icurrlevel,ich)=ibits(header(14),24,8)
          else
            iframe=ibits(header(14),0,8)
            if(iframe.eq.0) chanbuf(icurrlevel,ich)=steim1
            if(iframe.eq.1) chanbuf(icurrlevel,ich)=steim1
            if(iframe.eq.2) chanbuf(icurrlevel,ich)=steim2
        endif
      endif
c
c now look for any critical error flags
c
      quality=ibits(header(10),8,8)
      if(iand(quality,seed_qual_missing_data).ne.0) then
          write(*,*) 'warning: data missing.'
          ierr = ior(ierr,imissing)
      endif
      if(iand(quality,seed_qual_questionable_timetag).ne.0) then
          write(*,*) 'warning: inaccurate time tag.'
          ierr = ior(ierr,inaccurate)
      endif
      return
      end


      integer function ichnnl(chname,chanbuf,high)
c      include 'decode.inc'
      integer blksize,steim1,steim2
      parameter (blksize = 4096)
      parameter (iblk = blksize / 4)
      parameter (lastvalue = 1)
      parameter (initial = 2 )
      parameter (ifileopen = 3)
      parameter (icurrlevel = 4)
      parameter (iprevlevel = 5)
      parameter (iseedname = 6)
      parameter (maxseedchannels = 100)
      parameter (steim1 = 10)
      parameter (steim2 = 11)
      parameter (iexpansion = 1)
      parameter (isamples = 2)
      parameter (imissing = 4)
      parameter (inaccurate = 8)
      parameter (igap = 16)
      parameter (ibeginevent = 32)

      integer*4 chanbuf(6,maxseedchannels),high,chname
      character*4 chnamx
c
c     returns a channel number for a given name
c     the first 33 channel names are fixed by default in the "initialize" routine.
c     other seed channel names will be dynamically added as encountered.
c     if the maximum number "maxseedchannels" s exceeded, 0 will be returned.
c
c in case a blank seed name is found, give it one. this can happen if the
c SHEAR system is incorrectly configured.
c
      if(chnamx.eq.'    ') chnamx = ' XXX'
      ichnnl=0
      do i=1,high
        if  (chanbuf(iseedname,i).eq.chname) then
          ichnnl = i
          return
        endif
      enddo
      if(high.lt.maxseedchannels) then
        high=high+1
        chanbuf(iseedname,high)=chname
        ichnnl = high
      endif
      return
      end



      subroutine dedata (record,samp1,samplast,ich,nsamp,data,nd,ierr,
     &                   chanbuf,event)
c      include 'decode.inc'
      integer blksize,steim1,steim2
      parameter (blksize = 4096)
      parameter (iblk = blksize / 4)
      parameter (lastvalue = 1)
      parameter (initial = 2 )
      parameter (ifileopen = 3)
      parameter (icurrlevel = 4)
      parameter (iprevlevel = 5)
      parameter (iseedname = 6)
      parameter (maxseedchannels = 100)
      parameter (steim1 = 10)
      parameter (steim2 = 11)
      parameter (iexpansion = 1)
      parameter (isamples = 2)
      parameter (imissing = 4)
      parameter (inaccurate = 8)
      parameter (igap = 16)
      parameter (ibeginevent = 32)

      integer*4 record(iblk),data(blksize*2),map(15),samp1,samplast
      integer*4 chanbuf(6,maxseedchannels)
      logical     event
      integer*4 iwunpk(8),idecomp(5,8),level
      data idecomp/0,     0,          0,         0,          0,
     &             1,     0, 1073741823, 536870912, 1073741824,
     &             2,    15,      32767,     16384,      32768,
     &             3,    10,       1023,       512,       1024,
     &             5,     6,         63,        32,         64,
     &             6,     5,         31,        16,         32,
     &             7,     4,         15,         8,         16,
     &             0,     0,          0,         0,          0/

      data   mask1/z'ffffff00'/,   mask2/z'ffff0000'/
      nd=0
c        "first_data_byte" contains the starting data byte, which we
c	  convert to longword index. in a record with no blockettes,
c	  the first data byte is 64.
      iaddr=ibits(record(12),16,16)/4
      if(iaddr.ne.16) then
          write(*,
     &      "('embedded blockette. data begins at longword:',i3)") iaddr
      endif
      ifirstblock=iaddr/16
      level=chanbuf(icurrlevel,ich)
      do 300 i=ifirstblock,(blksize/64)-1
c        now decode i-th frame
c        first two words of frame are map for following data
         iaddr=iaddr+1
         k=30
         do 100 j=1,15
            k=k-2
            map(j)=ibits(record(iaddr),k,2)
  100    continue
         do 200 j=1,15
           iaddr=iaddr+1
           if(level.eq.steim1) then
            if(map(j).ne.3) go to 110
               nd=nd+1
               data(nd)=record(iaddr)
               go to 200
  110       continue
            if(map(j).ne.2) go to 120
               nd=nd+1
               data(nd)=ic2(ibits(record(iaddr),16,16))
               nd=nd+1
               data(nd)=ic2(ibits(record(iaddr),0,16))
               go to 200
  120       continue
           endif

          if(map(j).ne.1) go to 130
             nd=nd+1
             data(nd)=ic1(ibits(record(iaddr),24,8))
             nd=nd+1
             data(nd)=ic1(ibits(record(iaddr),16,8))
             nd=nd+1
             data(nd)=ic1(ibits(record(iaddr),8,8))
             nd=nd+1
             data(nd)=ic1(ibits(record(iaddr),0,8))
             go to 200
  130     continue

          if(level.eq.steim2) then
            if(map(j).ne.0) then
       		isubcode=(map(j)-2)*4+ibits(record(iaddr),30,2)+1
       		isamps=idecomp(1,isubcode)
       		if(isamps.gt.0) then
         		iaccum=record(iaddr)
         		do k=isamps,1,-1
              			iwork=iand(iaccum,idecomp(3,isubcode))
              			ihibit=idecomp(4,isubcode)
              			if(iand(iwork,ihibit).ne.0) then
                  			iwork=iwork-idecomp(5,isubcode)
              			endif
              			iwunpk(k)=iwork
                     		iaccum=ishft(iaccum,-idecomp(2,isubcode))
         		enddo
         		do k=1,isamps
                     		nd=nd+1
              			data(nd)=iwunpk(k)
         		enddo
       		endif
     	    endif
          endif

  200   continue
  300 continue
      if(nsamp.gt.1) then
        ifirst=chanbuf(lastvalue,ich)+data(1)
        if (event) then
          write(*,
     &      "('beginning of event')")
          elseif ((ifirst.ne.samp1).and.(chanbuf(initial,ich).eq.0))then
          write(*,
     &      "('expansion error at integration constant check')")
            ierr = ior(ierr,iexpansion)
        endif
        chanbuf(initial,ich) = 0
        data(1)=samp1
        do i=2,nd
           data(i)=data(i-1)+data(i)
        enddo
        chanbuf(lastvalue,ich)=data(nd)
        if (samplast.ne.data(nd)) then
          write(*,
     &      "('error: expansion disagrees at end of record')")
          ierr = ior(ierr,iexpansion)
        endif
      endif
      if (nd.ne.nsamp) then
        write(*,
     &    "('number of decompressed samples ',i5,
     &    ' does not agree with header ',i5)") nd,nsamp
        ierr = ior(ierr,isamples)
        endif
      return
      end


      integer function ic1(iarg)
c
c     pads with 1's word "ic1" if its rightmost byte
c     has a 1 as most significant bit (this because
c     byte 0 has to be interpreted as a signed number)
c
      data mask1/z'ffffff00'/
      ic1=iarg
cuk      if(btest(iarg,7))ic1=or(iarg,mask1)
      if(btest(iarg,7))ic1=ior(iarg,mask1)
      return
      end





      integer function ic2(iarg)
c
c     pads with 1's word "ic2" if its rightmost 2-byte half
c     has a 1 as most significant bit (this because
c     bytes 0,1 have to be interpreted as a signed number)
c
      data mask2/z'ffff0000'/
      ic2=iarg
cuk      if(btest(iarg,15))ic2=or(iarg,mask2)
      if(btest(iarg,15))ic2=ior(iarg,mask2)
      return
      end
