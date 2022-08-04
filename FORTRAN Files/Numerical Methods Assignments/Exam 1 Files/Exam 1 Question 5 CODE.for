      ! Michael Wiedner
	  ! Dr. Fuse
	  ! CMS 495 - Numerical Methods
	  ! 3/5/2021
	  ! Exam 1 Question 5 Code
	  
	  PROGRAM Problem 5
    
      integer :: count,i
      real :: x, hsumx, hsumy, tsumx, tsumy, havgx, havgy, tavgx, tavgy
      
      real, dimension(200) :: hundrd ! Array for 2 sets of 100 random values
      real, dimension(20000) :: tentho ! Array for 2 sets of 10,000 random values
      
      ! Set sums initially equal to zero
      hsumx = 0
      hsumy = 0
      tsumx = 0
      tsumy = 0
      
      
      ! RNG
      call SYSTEM_CLOCK(count)
      call SRAND(count) 
      do i = 1,20200
      x = (rand() * (12.7 - 1.5)) + 0.75 ! Minimum value of 0.75 and maximum value of 12.7 - 1.5 + 0.75 = 11.95
      if (i.LE.200) then ! Apply first 200 values to the first array
          hundrd(i) = x
      else ! Apply the remaining 20,000 values to the second array
          tentho(i - 200) = x
      end if
      end do
    
      do i = 1, 100 ! Find the sums and then the averages
          hsumx = hsumx + hundrd(i)
          hsumy = hsumy + hundrd(i + 100)
      end do
      havgx = hsumx / 100.0
      havgy = hsumy / 100.0
      
      do i = 1, 10000 ! Find the sums and then the averages
          tsumx = tsumx + tentho(i)
          tsumy = tsumy + tentho(i + 10000)
      end do
      tavgx = tsumx / 10000.0
      tavgy = tsumy / 10000.0
      
      
      write(*,*) "Average of 100 simulations:", havgx, havgy
      write(*,*) "Average of 10,000 simulations:" , tavgx, tavgy
      STOP
      END