	subroutine parsedot(string,maxchr,ichr)

        character string

        do ichr=1,maxchr
           if(string(ichr:ichr).eq.'.') return
	enddo
        return
        end
