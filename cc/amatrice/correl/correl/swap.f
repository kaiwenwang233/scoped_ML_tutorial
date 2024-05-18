	subroutine swap(cdata,n)

	character*(*) cdata
	character*4 str
	integer m

	m=n/4


	do i=1,m
	 str(1:1)=cdata(4*i:4*i)
	 str(2:2)=cdata(4*i-1:4*i-1)
	 str(3:3)=cdata(4*i-2:4*i-2)
	 str(4:4)=cdata(4*i-3:4*i-3)
         cdata(4*i-3:4*i)=str
	enddo
	
	return
	end

	
