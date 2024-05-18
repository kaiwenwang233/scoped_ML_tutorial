      program testnan2

c gfortran testnan2.f

	implicit none
	real x
	x=-1.0
	x=sqrt(x)
	x=1e9
c	if (isnan(x)) then
	if (x.le.1e10) then
	 print *, "x is real"
	else
	 print *, "x is Nan"
	endif

	end program testnan2

