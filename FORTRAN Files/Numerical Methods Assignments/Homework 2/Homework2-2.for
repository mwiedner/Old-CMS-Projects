	Program Homework2
	
	Real amt = 0.0
	
	write(*,*) 'Enter the amount of Carbon-14'
	read(*,*) amt
	
	IF (amt.le.0.0) THEN
            write(*,*) 'Carbon-14 levels must be greater than zero.'
	ELSE
            write(*,*) 'The age of the material is the following:'
            write(*,*) (-1.0 * ((log10(amt)) / 0.0001216))
	END IF
	
	STOP
	END