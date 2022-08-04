	Program Homework1

	Real a
	Real b
	Real c
	Real ans1
	Real ans2
	
	write(*,*) 'Enter the values of a, b, then c'
	
	read(*,*) a, b, c
	
	ans1=(((-1.0 * b) + sqrt((b*b) - (4.0 * a * c))) / (2.0 * a))
	ans2=(((-1.0 * b) - sqrt((b*b) - (4.0 * a * c))) / (2.0 * a))
	
        write(*,*) 'Your two values of x are: '
        IF (ans1.GE.0.0 .OR. ans1.LE.0.0) THEN
            write(*,*) ans1
        ELSE
            write(*,*) 'Imaginary so have fun with that'
        END IF
        
        write(*,*) 'and'
        
        IF (ans2.GE.0.0 .OR. ans2.LE.0.0) THEN
            write(*,*) ans2
        ELSE
            write(*,*) 'imaginary so have fun with that.'
        END IF
	
	STOP
	END