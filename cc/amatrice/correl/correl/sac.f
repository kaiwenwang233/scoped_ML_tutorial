
	subroutine getSACdata(fname,NPTS,beginIndex,EndIndex,iswap,
     &	data,ierr)

	include 'correl.inc'
	character*100 fname
	real data(MAXSAMP)

c	character*(*) fname
c	real data(*)
	integer beginIndex,EndIndex	!must be from 1 to NPTS
	logical iswap			!flag to swap bytes


	integer BLOCKSIZE		!equals header length
	logical StartInBlock,EndInBlock
	integer StartBlock,SBindex,endBlock,EBindex
	character*632 cdata
	real dblock(158)
	equivalence (dblock,cdata)
	data BLOCKSIZE/632/


c STRATEGY-----The SAC file has a header of length BLOCKSIZE (in bytes)
c              To read the file quickly we want the record size to be
c              large. This allows us to read in a large block of data
c	       (into a string of length BLOCKSIZE) with one read. The
c	       string is equivalenced to a real array, and after each
c	       read, we copy the contents of that array to the appropriate
c	       place in the output array. The only hangup is that the
c	       file is not likely to be an integer multiple of BLOCKSIZE,
c	       and the user may request arbitrary start and stop indices.
c	       Thus part of the data read will be from incomplete blocks.
c	       There are 3 cases to handle.


        call freeunit(iunit)
c      	open(iunit,file=fname,form='unformatted',access='direct',
c     &		recl=632,err=10,iostat=iernum)


	open(iunit,file=fname,access='direct',recl=BLOCKSIZE,form=
     +       'unformatted',err=10,iostat=iernum)

	ierr=0

	NblocksInFile=NPTS*4/BLOCKSIZE

c If InBlock is false then the block index is actually an index
c into the remaining bytes after the last complete block.
	call getBlockAndIndex(NblocksInFile,beginIndex,BLOCKSIZE,
     +                         StartBlock,SBindex,StartInBlock)
	call getBlockAndIndex(NblocksInFile,endIndex,BLOCKSIZE,
     +                         endBlock,EBindex,EndInBlock)


c case 1 ... both indices are before the end of complete blocks
	if(StartInBlock .and. EndInBlock)then
	   k=0

	   read(iunit,rec=1+StartBlock,err=20)cdata
	   if (iswap) call swap(cdata,632)
	   do l=(SBindex+3)/4,BLOCKSIZE/4
		k=k+1
		data(k)=dblock(l)
	   end do
	   do j=StartBlock+1,endBlock-1
	      read(iunit,rec=j+1,err=20)cdata
	      if (iswap) call swap(cdata,632)
	      do l=1,BLOCKSIZE/4
	         k=k+1
	         data(k)=dblock(l)
	      end do
	   end do
	   read(iunit,rec=1+endBlock,err=20)cdata
	   if (iswap) call swap(cdata,632)
	   do l=1,(EBindex+3)/4
	      k=k+1
	      data(k)=dblock(l)
	   end do
	   close(iunit)
	   return
	endif

c case 2 starting index is in a block but end index is not.
	if(StartInBlock .and. (.not. EndInBlock))then
	   k=0
	   read(iunit,rec=1+StartBlock,err=20)cdata
	   if (iswap) call swap(cdata,632)
	   do l=(SBindex+3)/4,BLOCKSIZE/4
		k=k+1
		data(k)=dblock(l)
	   end do
	   do j=StartBlock+1,NblocksInFile
	      read(iunit,rec=j+1,err=20)cdata
	      if (iswap) call swap(cdata,632)
	      do l=1,BLOCKSIZE/4
	         k=k+1
	         data(k)=dblock(l)
	      end do
	   end do
	   read(iunit,rec=NblocksInFile+2,err=20)cdata(1:EBindex)
	   if (iswap) call swap(cdata,632)
	   do l=1,(EBindex+3)/4 
	      k=k+1
	      data(k)=dblock(l)
	   end do
	   close(iunit)
	   return
	endif


c case 3 both indices are past end block
	if(.not. (StartInBlock .or. EndInBlock))then
	   k=0
	   read(iunit,rec=NblocksInFile+2,err=20)cdata(1:EBindex)
	   if (iswap) call swap(cdata,632)
	   do l=(SBindex+3)/4,(EBindex+3)/4
	      k=k+1
	      data(k)=dblock(l)
	   end do
	   close(iunit)
	   return
	endif
	
10	ierr=10
	return

20	ierr=20
	close(iunit)
	return
	end


	subroutine getBlockAndIndex(NblocksInFile,Index,BLOCKSIZE,
     +                        Block,Bindex,InBlock)
	integer Index,BLOCKSIZE,BLOCK,Bindex
	logical InBlock,exit

c Index is the index into the real*4 data array
c Block is the block number where that point is located
c Bindex is either the number of bytes into the block or the number
c of bytes past the last block to the data point. 

	InBlock=.false.
	if(NblocksInFile*BLOCKSIZE .lt. Index*4)then
	   Bindex=Index*4-NblocksInFile*BLOCKSIZE-3
	   return
	else
	   BLOCK=0
	   exit=.false.
	   do while(.not.exit)
	      if(BLOCK*BLOCKSIZE .lt. Index*4)then
		  Bindex=(Index-1)*4-BLOCK*BLOCKSIZE+1
		  BLOCK=BLOCK+1
	      else
		  exit=.true.
	      endif
	   end do
	   InBlock=.true.
	   return
	endif
	end	      
	




