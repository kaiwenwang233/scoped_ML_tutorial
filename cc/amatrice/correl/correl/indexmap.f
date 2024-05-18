	subroutine indexmap(n1,i1,n2,i2,ind)

c  indexmap(n1,i1,n2,i2,ind) -- Returns an indexmap, ind, from integer vector
c  i1 to i2, such that i1 -> i2(ind).  If an element of i1 is not found in i2
c  the corresponding ind element has a zero place holder.  ind is the same 
c  length as i1.

c  David Schaff 15-Aug-03

	implicit none

c	Inputs:
	integer  n1	  ! number of points in first vector
	integer  i1(n1)   ! first vector
	integer  n2	  ! number of points in second vector
	integer  i2(n2)   ! second vector

c	Output:
	integer  ind(n1)  ! index vector

c	Local variables:
	integer i, l
	integer i2tmp(n2) ! sorted second vector
	integer idx(n2)   ! sorted index vector i2tmp=i2(idx)
	integer ifindi	  ! integer search function


c	Sort the second vector and its index vector (to speed searching below)
	call indexxi(n2,i2,idx)
	do i=1,n2
	  i2tmp(i)=i2(idx(i))
	enddo

c	Find the values of i1 that are in i2 and get the index in i2
	do i=1,n1
	  l=ifindi(n2,i2tmp,i1(i))
	  if(l.eq.0) then
	     ind(i)=0
	  else
	     ind(i)=idx(l)	
	  endif
	enddo

	return
	end
