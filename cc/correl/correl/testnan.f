      program testnan

	implicit none
	real x
	x=-1.0
	x=sqrt(x)
	if (isnan(x)) then
	 print *, "x is Nan"
	endif

	end program testnan

