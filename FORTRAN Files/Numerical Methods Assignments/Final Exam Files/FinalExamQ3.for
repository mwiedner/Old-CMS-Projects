      ! Michael Wiedner
      ! Dr. Fuse
      ! Numerical Methods CMS 495
      ! 5/3/2021
      ! Final Exam Question 3

      PROGRAM FinalQ3
      integer :: i, n
      real :: h, k1, k2, k3, k4, yn, yn1, tn
      
      write(*,*) "Evaluating the equation y' + y = sin(4(pi)t) with
     &y(0) = 0.5 and grid size 0.25 using fourth-order Runga-Kutta:"
      write(*,*) 
      write(*,*) "   T sub n            Y sub n"
      
      yn = 0.5 ! y(0) is given
      h = 0.25 ! First grid size
      
      
      do i = 1, 10 ! Runga-Kutta Loop
          tn = h * (i-1)
          k1 = h * fun(tn,yn)
          k2 = h * fun((tn + (0.5 * h)), (yn + (0.5 * k1)))
          k3 = h * fun((tn + (0.5 * h)), (yn + (0.5 * k2)))
          k4 = h * fun((tn + h), (yn + k3))
          yn1 = yn + ((1.0/6.0) * (k1 + (2 * k2) + (2 * k3) + k4))
          yn = yn1
          write(*,*) tn, ", ", yn
      end do
      write(*,*) ! Print blank line
      
      ! Reset variables for the new grid size
      yn = 0.5
      h = 0.125
      
      write(*,*)
      write(*,*) "----------------------------------------------------"
      write(*,*)
      
      write(*,*) "Now using grid size 0.125:"
      write(*,*) 
      write(*,*) "   T sub n            Y sub n"
      
      do i = 1, 10 ! Runga-Kutta Loop
          tn = h * (i-1)
          k1 = h * fun(tn,yn)
          k2 = h * fun((tn + (0.5 * h)), (yn + (0.5 * k1)))
          k3 = h * fun((tn + (0.5 * h)), (yn + (0.5 * k2)))
          k4 = h * fun((tn + h), (yn + k3))
          yn1 = yn + ((1.0/6.0) * (k1 + (2 * k2) + (2 * k3) + k4))
          yn = yn1
          write(*,*) tn, ", ", yn
      end do
      
      STOP
      END PROGRAM
      
      FUNCTION fun(t, y)
      real :: y, t, pi
          
      pi = 4.D0*ATAN(1.D0)
      
      fun = (SIN((4*pi)*t)) - y
      END FUNCTION