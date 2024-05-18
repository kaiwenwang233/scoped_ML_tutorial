c	program finds

c        parameter(MAXEVE=100000)
c        character*30	files(MAXEVE)		! Master file list
c	integer  ids(MAXEVE), idssub(MAXEVE)
c	character*20 pattern

c	pattern='VHZ'

c      	call freeunit(iunit)
c      	open(iunit,file='cids',status='unknown')
c
c	i=1
c10	read(iunit,'(i)',end=20) ids(i)
c	i=i+1
c	goto 10
c20	nev=i-1
c
c	
c	call findsac(ids,nev,pattern,idssub,nevsub,files)

c999   i=ieee_flags('clear','exception','all',out)


c	end

	subroutine findsac(ids,nev,pattern,dir,idssub,nevsub,files)

        include 'correl.inc'  
   
        character*100 files(MAXEVE), files2(MAXFILES)	! Master file list
	integer  ids(MAXEVE),idssub(MAXEVE),idssub2(MAXFILES)
	integer  idx(MAXEVE)
	character*20 pattern
	character*50 dir
	character*60 tmpfile
        integer   trimlen



c	i=system('ls ' //dir// ' | grep ' //pattern// ' > tmp.files')
	tmpfile=dir(1:trimlen(dir)) // '/tmp.files'
	i=system('ls ' //dir// ' | grep ' //pattern// ' > ' // tmpfile )

      	call freeunit(iunit)
c      	open(iunit,file='tmp.files',status='unknown')
      	open(iunit,file=tmpfile,status='unknown')

	i=1
10	read(iunit,'(a)',end=20,err=30) files2(i)
	i=i+1
	goto 10
20	nfiles=i-1
	close(iunit)
c	i=system('rm tmp.files')
	i=system('rm ' // tmpfile)


	do i=1,nfiles
	  call parsedot(files2(i),30,ichr)
	  read(files2(i)(1:ichr-1),'(i)') idssub2(i)
c	print *, ichr
c	print *, idssub2(i)
	enddo

c	print *, idssub2(nfiles)


	call indexmap(nev,ids,nfiles,idssub2,idx)

c Get matching files
	nevsub=0
	do i=1,nev
c	print *, idx(i)
	  if(idx(i).ne.0) then
	    nevsub=nevsub+1
	    idssub(nevsub)=idssub2(idx(i))
	    files(nevsub)=files2(idx(i))
c	print *, files(nevsub)
	  endif
	enddo

	return

30	stop 'Error reading sac file list'
	end



