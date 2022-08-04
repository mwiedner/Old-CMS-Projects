	Program Homework2
	
	real, dimension(8) :: values
	
	real sum = 0.0
	real avg = 0.0
	real sd = 0.0
	
	! Placeholding real variable for calculating the numerator in the standard deviation formula
	real sdnum = 0.0
	
	
	! Open the values file for reading
	open (42, file = 'values.txt')
	
	do i = 1, 8
		read(42, *) values(i)
	end do
	
	close (42)

	! Impressive arithmetic
	do i = 1, 8
		sum = sum + values(i)
	end do
	
	avg = sum / 8.0
	
	do i = 1, 8
		sdnum = sdnum + ((values(i) - avg)**2)
	end do
	
	sd = (sdnum/(8.0))**(1.0/2.0)
	
	! Open the results file for writing
	open (14, file = 'results.txt')
	
	write(14,*) 'Sum:', sum
	write(14,*) 'Average:', avg
	write(14,*) 'Standard Deviation:', sd
	
	close (14)
	
	
	STOP
	END