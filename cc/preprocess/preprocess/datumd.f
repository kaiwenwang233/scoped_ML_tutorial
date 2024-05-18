c compute calendar date from yr and julian day of year.

	subroutine datumd(idoy, iyr, imo, idy)

	implicit none

c	Parameters:
	integer	idoy
	integer	iyr
	integer	imo
	integer	idy


c	Local variables:
	integer	i
	integer kmo(12)
	integer id
	integer	iyr4

	data kmo/31,28,31,30,31,30,31,31,30,31,30,31/

      iyr4 = iyr/4

c for some reason kmo(2) doesn't get initialized after multiple runs.
c this corrects bug

	kmo(2) = 28	


c10    if(mod(iyr,4).eq.0) kmo(2) = 29	! this may be fine.
10    if(iyr4*4.eq.iyr) kmo(2) = 29

      id=idoy
      do i=1,12
         id = id- kmo(i)
         if (id.le.0) goto 30
      enddo
      i =12
30    idy = id+kmo(i)
      imo = i
      return
      end ! of subr. datumd
