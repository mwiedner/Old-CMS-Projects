      ! Michael Wiedner
      ! Dr. Fuse
      ! Numerical Methods CMS 495
      ! 4/12/2021
      ! Exam 2 Question 2

      PROGRAM Exam2Q2
      integer :: i, count, j
      real :: xn, xn1, xnm, tol, lbound, rbound, xx
      real, dimension(6) :: zeroes
      
      xx = 0
      lbound = -2
      rbound = 2
      
      
      write(*,*) "Newton's Method"
      write(*,*) "----------------------------------------------------"
      
      do i = 1,6 ! Since the degree of the polynomial is 6, there are at most 6 zeroes so we loop 6 times.
      ! set the initial value for x_0 as the midpoint between the two bounds. "The guess"
      xn = (lbound + rbound)/2
      xn1 = rbound
      count = 0
      tol = xn
      
      
      do while (abs(abs(xn1) - abs(xn)).gt.(10**(-6)))
          xn = tol
          xn1 = xn - ((fun(xn)) / (der(xn))) ! Find the next x
          
          tol = xn1 ! Set xn equal to xn1 to essentially increase the subscript of x by 1
          
          count = count + 1
          
      end do
      
      if ((ce(xn, zeroes(1)).eq.0.0).and.(ce(xn, zeroes(2)).eq.0.0).and. ! Very messy if statement that checks if a previously found x is the same as the new one
     &(ce(xn, zeroes(3)).eq.0.0).and.(ce(xn, zeroes(4)).eq.0.0).and. ! If the same, then ignore printing.
     &(ce(xn, zeroes(5)).eq.0.0).and.(ce(xn, zeroes(6)).eq.0.0)) then
      write(*,*) "A zero has been found to be", xn, "."
      write(*,*) "This was found after", count, "iterations."
      write(*,*) " " ! blank space for formatting
      
      zeroes(i) = xn ! Add the zero to the array
      
      ! After finding the first x value, adjust the bounds to the midpoint between the first x-value and the further original bound while leaving the other bound fixed
      if (abs(lbound - xn1).lt.abs(rbound - xn1)) then
          lbound = (xn1 + rbound) / 2
      else
          rbound = (xn1 + lbound) / 2
      end if
      
      end if
      
      end do
      
      write(*,*) " "
      
      write(*,*) "Secant Method"
      write(*,*) "----------------------------------------------------"
      
      ! Begin secant method
      
      ! Reset bounds and previous zeroes
      lbound = -2
      rbound = 2
      do i = 1,6
          zeroes(i) = null
      end do
      
      do i = 1, 6
          
      ! Reset the counter
      count = 0
      
      ! Declare initial guesses for x_1 and x_2
      xnm = lbound
      xn1 = rbound
      xn = (lbound + rbound) / 2
      tol = xn
      
      do while (abs(xn1 - xn).gt.(10**(-6)))
          xn = tol
          xn1 = xn - ((fun(xn)) / ((fun(xn) - fun(xnm)) / (xn - xnm)))
          
          xnm = xn
          tol = xn1
          
          count = count + 1
      end do
      
      if ((ce(xn1, zeroes(1)).eq.0.0).and.(ce(xn1, zeroes(2)).eq.0.0) ! Very messy if statement that checks if a previously found x is the same as the new one
     &.and.(ce(xn1, zeroes(3)).eq.0.0).and.(ce(xn1, zeroes(4)).eq.0.0)
     &.and.(ce(xn1, zeroes(5)).eq.0.0).and.(ce(xn1, zeroes(6)).eq.0.0))
     &then
      write(*,*) "A zero has been found to be", xn1, "."
      write(*,*) "This was found after", count, "iterations."
      write(*,*) " " ! blank space for formatting
      
      zeroes(i) = xn ! Add the zero to the array
      
      ! After finding the first x value, adjust the bounds to the midpoint between the first x-value and the further original bound while leaving the other bound fixed
      if (abs(lbound - xn1).lt.abs(rbound - xn1)) then
          lbound = (xn1 + rbound) / 2
      else
          rbound = (xn1 + lbound) / 2
      end if
      
      end if
      
      end do
      
      STOP
      END PROGRAM
      
      FUNCTION fun (x) ! Function function
      real :: x, fun
      fun = ((x**6) - x - 1.0)
      END FUNCTION
      
      FUNCTION der (x) ! Derivative function
      real :: x, der
      der = ((6 * (x**5)) - 1)
      END FUNCTION
      
      FUNCTION abs (x) ! Absolute value function
      real :: x, abs
      if (x.lt.0.0) then
          abs = x * (-1.0)
      else
          abs = x
      end if
      END FUNCTION
      
      FUNCTION CE (x, y) ! Function for determining if two values are within 10^-6 of each other
      real:: x, y, CE, ax, ay
      ax = abs(x)
      ay = abs(y)
      
      if ((abs(ax-ay).le.(10**(-6)))) then 
      CE = 1.0  
      else if (abs(ay-ax).le.(10**(-6))) then
          CE = 1.0
      else
          ce = 0.0
      end if
      END FUNCTION