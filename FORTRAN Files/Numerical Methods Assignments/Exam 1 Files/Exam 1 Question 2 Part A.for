      ! Michael Wiedner
	  ! Dr Fuse
	  ! CMS 495 - Numerical Methods
	  ! 3/5/2021
	  ! Exam 1 Question 2 Code - Bisection Search
	  
      PROGRAM Problem 2 Bisection
    
      integer :: i, degree, iterat
      real :: a, b, tol, cnstnt, m, ya, yb, ym, atemp, btemp, tollo
      
      real, dimension(5) :: c ! array of coefficients
      
      iterat = 0
      do i = 1, 5
          c(i) = 0
      end do
      
      write(*,*) "Input the following information in order:"
      write(*,*) "Min of range, max of range, tolerance, degree of pol."
      write(*,*) "The first 3 values must be doubles and the last int."
      read(*,*) a
      read(*,*) b
      if (a.ge.b) then 
          write(*,*) "Minimum is larger than maximum. Abort!"
          stop
      end if
      read(*,*) tol
      if (tol.LE.0.0) then
          write(*,*) "Tolerance must be greater than zero. Abort!"
          stop
      end if
      read(*,*) degree
      if (degree.gt.5.or.degree.lt.1) then
          write(*,*) "The degree must be 1, 2, 3, 4, or 5. Abort!"
          stop
      end if
      
      write(*,*) "Enter the coefficients of your polynomial from 
     &right to left (the coefficient in front of the x^1 first):"
      do i = 1, degree
          read(*,*) c(i)
      end do
      
      write(*,*) "Enter the constant of your polynomial:"
      read(*,*) cnstnt
      
      write(*,*)
      write(*,*) "Your polynomial is being processed as the following:"
      do i = 1, degree
          write(*,*) c(degree - i + 1), "x to the ", (degree - i + 1)
          write(*,*) "plus"
      end do
      
      write(*,*) cnstnt
      write(*,*) ! Empty line for neat printing
      
      ya = fvalue(a, c(1), c(2), c(3), c(4), c(5), cnstnt) ! y value at left bound
      ya = fvalue(b, c(1), c(2), c(3), c(4), c(5), cnstnt) ! y value at right bound
      
      if (ya.eq.0.0) then ! Test if the boundaries are zeroes
          write(*,*) "There exists a zero at the boundary", a
          write(*,*) "This solution was found with no iterations."
      else if (yb.eq.0.0) then 
          write(*,*) "There exists a zero at the boundary", b
          write(*,*) "This solution was found with no iterations."
      end if
      
      if ((a1*b1).gt.0.0) then
          write(*,*) "The zero does not exist. Goodbye"
          stop
      end if
      
      tollo = tmptol(a,b,c(1),c(2),c(3),c(4),c(5),cnstnt)
      
      
      do while(tollo.GE.tol) ! Main while loop
          tollo = tmptol(a,b,c(1),c(2),c(3),c(4),c(5),cnstnt)  
          m = (a+b)/2.0 ! define the midpoint as the sum of the bounds divided by 2
          
          atemp = a ! Placeholding variables for the values of a and b. No idea why but this fixed an error
          btemp = b
          
          ya = fvalue(a, c(1), c(2), c(3), c(4), c(5), cnstnt) ! y value at left bound
          yb = fvalue(b, c(1), c(2), c(3), c(4), c(5), cnstnt) ! y value at right bound
          ym = fvalue(m, c(1), c(2), c(3), c(4), c(5), cnstnt) ! y value at midpoint
          
          if((ya*ym).lt.0.0) then ! if f of left bound times f of midpoint is negative, then the 0 is in it
             ! Then 0 is in first half
             ! Left bound stays the same
            b=m ! Right bound becomes the m
            a = atemp
          end if
          if((yb*ym).lt.0.0) then ! Else 0 is in the second half
            a=m ! Left bound becomes the m
             ! Right bound stays the same
            b = btemp
          end if
       
       !tollo = tmptol(a,b,c(1),c(2),c(3),c(4),c(5),cnstnt)   
       iterat = iterat + 1
        
      end do ! End main while loop
      
      
      write(*,*) "The zero has been found after", iterat, "iterations."
      
      if (ya.lt.0.0) then
          ya = ya * (-1.0)
      end if
      if (yb.lt.0.0) then
          yb = yb * (-1.0)
      end if
      
      if (yb.GT.ya) then
              write(*,*) "The zero is", a
          else
              write(*,*) "The zero is", b
          end if
      
      
      STOP
      END
      
      
      FUNCTION fvalue (x, c1, c2, c3, c4, c5, con)
          real :: fvalue
          
          real :: x, c1, c2, c3, c4, c5, con
          
      fvalue = c5*(x**5) + c4*(x**4) + c3*(x**3) + c2*(x**2)
     &+ c1*(x) + con
      END FUNCTION
      
      FUNCTION tmptol (p1, p2, c1, c2, c3, c4, c5, con)
          real :: tmptol
          
      real :: p1,p2,temp,c1,c2,c3,c4,c5,con,yp1,yp2,bst,oth,ty1,ty2
          
          yp1 = fvalue(p1, c1, c2, c3, c4, c5, con) ! y value of point one
          yp2 = fvalue(p2, c1, c2, c3, c4, c5, con) ! y value of point two
          
          ! Find which of the two points is the best
          ! Start by getting the absolute value of the y values of the points to see which is furthest from zero
          if(yp1.lt.0.0) then
              ty1 = yp1 * (-1.0)
          else
              ty1 = yp1
          end if
          
          if (yp2.lt.0.0) then
              ty2 = yp2 * (-1.0)
          else
              ty2 = yp2
          end if
          
          
          if (ty1.GT.ty2) then
              bst = p2
              oth = p1
          else
              bst = p1
              oth = p2
          end if
          
          temp = ((bst - oth) / bst)
          if (temp.LT.0.0) then
              temp = temp * (-1.0)
          end if
          tmptol = temp
      END FUNCTION