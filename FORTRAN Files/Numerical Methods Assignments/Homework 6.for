      ! Michael Wiedner
	  ! Dr Fuse
	  ! CMS 495 - Numerical Methods
	  ! 3/15/2021
	  ! Homework 6
	  
      PROGRAM Homework 6
    
      integer :: i
      real :: h, yn1, yn, fun, tol, abs
      real, dimension(5) :: c ! array of coefficients
      
      yn = 1
      n = 0
      abs = yn
      
      write(*,*) "The function we are computing is the following:"
      write(*,*) "y' + 2y = x^3 * e^{-2x}"
      write(*,*) ! Blank space for formatting
      write(*,*) "Please enter a value for h (The step-size):"
      read(*,*) h
      if (h.LE.0.0) then ! Step size needs to be greater than zero
          write(*,*) "h must be greater than zero. Abort!"
          stop
      end if
      write(*,*) "Please enter a value for tolerance:" ! The tolerance is the acceptable y-value of the estimated root
      read(*,*) tol
      if (tol.LE.0.0) then
          write(*,*) "Tolerance must be greater than zero. Abort!"
          stop
      end if
      
      do while (abs.gt.tol) ! Main do loop
          fun = fval(h, yn, n)
          yn1 = fun
          
          n = n + 1
          yn = yn1
          
          ! This isn't necessary since we know the function is positive in the y-direction, but it is relevant for a general case
          if (yn.gt.0.0) then ! Absolute value of y sub n
              abs = yn
          else
              abs = yn * (-1.0)
          end if
      end do ! End main do loop
      
      write(*,*)
      write(*,*)
      write(*,*) "The zero is at x = ", h * n
      write(*,*)
      write(*,*) "This was found after", n, "iterations."
      
      
      STOP
      END ! End the program
      
      FUNCTION fval (h, yn, n)
          real :: fval
          
          real :: h, yn, t, j
          integer :: n
          
          j = real(n)
          
          t = h * j
          
          fval = yn + h * (((t**3)*(exp(-2*t)))-(2*(yn)))
      end function