      ! Michael Wiedner
      PROGRAM EuclideanAlgorithm
      integer :: a, b, n, r, temp, perma, permb, gcd, i, t
      
      ! Create arrays for storing all the values we find throughout calculation. 
      ! Array size is 10000 because that should always be big enough.
      integer, dimension(10000) :: aa
      integer, dimension(10000) :: bb
      integer, dimension(10000) :: nn
      integer, dimension(10000) :: rr
      integer, dimension(10000) :: ac
      integer, dimension(10000) :: bc
      
      write(*,*) "Let's do the Euclidean Algorithm! Enter the 2 values"
      write(*,*) ! print blank line
      read(*,*) a
      read(*,*) b
      
      if (b.gt.a) then ! Make sure the a value is larger than the b value. If not, fix it.
      temp = a
      a = b
      b = temp
      end if
      
      ! Establish permanent values for a and b that won't change after the algorithm is complete
      perma = a
      permb = b
      
      write(*,*) "We are using the following numbers: ", a, b
      r = 1 ! Default r to equal 1 so the while loop below will actually run. This is always overwritten in the loop.
	  
      i = 0 ! Value for keeping track of loop iterations
      
      DO WHILE (r.gt.0.0) ! Loop until R is 0
	  
      i = i + 1 ! Count which iteration it is
	  
      write(*,*) ! Print blank line
      n = INT(floor(REAL(a) / REAL(b)))
      r = a - (n * b)
      write(*,*) a, "= ", n, " * ", b, " + ", r ! Print the equation
	  
      ! Store all of the values into their respective arrays
      aa(i) = a
      bb(i) = b
      nn(i) = n
      rr(i) = r
      
      ! Adjust values for the next iteration
      a = b
      b = r
      END DO
	  
      gcd = a
      
      write(*,*) "GCD is ", gcd
      write(*,*) ! Blank space
      write(*,*) "The linear-equation equation is now: "
      write(*,*) perma, "x + ", permb, "y = ", a
      
      ! ac and bc represent the coefficients in front of a and b for each remainder in the Euclidean Algorithm.
      ! The first two pairs of ac and bc are found outside of the loop 
      ! since they do not yet share the pattern of the subsequent pairs.
      ac(1) = 1
      bc(1) = (-1) * nn(1)
      write(*,*) rr(1), " = ", ac(1), "a + (", bc(1), "b)"
      
      ac(2) = -1 * ac(1) * nn(2)
      bc(2) = -1 * bc(1) * nn(2) + 1
      write(*,*) rr(2), " = ", ac(2), "a + (", bc(2), "b)"
      
      ! Update the values of a and b so that, once the loop is finished, we can check our result.
      ! The values of these variables are not related to what they were in the previous loop.
      ! They are just no longer in use, so why not recycle them.
      a = ac(2)
      b = bc(2)

      ! When there are more than 3 remainders, the loop is necessary. It runs one time less than the previous loop. 
      DO t = 3, i-1
      
      ac(t) = ac(t-2) + (-1 * nn(t)) * ac(t-1)
      bc(t) = bc(t-2) + (-1 * nn(t)) * bc(t-1)
      write(*,*) rr(t), " = ", ac(t), "a + (", bc(t), "b)"
      
      ! Update the values of a and b
      a = ac(t)
      b = bc(t)
      
      END DO
      
      ! Math check
      if (((perma * a) + (permb * b)).eq.gcd) then
          write(*,*) "We are correct!"
      else
          write(*,*) "Uh oh! Check the computation."
      end if
      
      ! Possible solutions
      write(*,*)
      write(*,*) "(", a, "+ k * (", permb / gcd, "), "
     &, b, "- k * (", perma / gcd, "))"
      
      STOP
      END PROGRAM