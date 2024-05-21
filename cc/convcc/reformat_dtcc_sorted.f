       program cc_summar
       implicit none
      integer         narguments
      integer         iargc,j
      integer n,np,ns,id1,id2,id1old,id2old
      integer itake
      real cc,ccp,ccs,dt,otc
      character line*60,str1*1, pha*7,sta*8,
     c  fn1*50,fn2*50
      real ootc 
      logical ex


c--- get input parameter file name:
      narguments = iargc()
      if(narguments.lt.2) goto 2
      call getarg(1,fn1)
      inquire(FILE= fn1,exist=ex)
      if(.not. ex) stop' >>> ERROR OPENING INPUT FILE 1.'
      call getarg(2,fn2)
      goto 3
2     stop'>>input (dt.cc.0), output (dt.cc.1)'
3     continue
      open(1,file=fn1,status='unknown')
      open(2,file=fn2,status='unknown')

      write(*,*)'Input file = ',fn1
      write(*,*)'Output file = ',fn2
      id1old=0
      id2old=0
      otc=0
10    read(1,'(a)',end=100) line
      read(line,*,end=100)sta,id1,id2,dt,cc,pha
      if(id1.eq.id1old.and.id2.eq.id2old) then
         write(2,'(a8,f9.3,f6.2,a1,a5)')sta,dt,cc," ",pha
      else 
         write(2,'(a2,2i10,f9.3)')"# ",id1,id2,otc
         write(2,'(a8,f9.3,f6.2,a1,a5)')sta,dt,cc," ",pha
         id1old= id1
         id2old= id2
      endif
      goto 10
100   write(*,*)'Done.'
      close(1)
      close(2)
      end
