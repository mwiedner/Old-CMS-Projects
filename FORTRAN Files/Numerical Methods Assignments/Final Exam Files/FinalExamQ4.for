      ! Michael Wiedner
      ! Dr. Fuse
      ! Numerical Methods CMS 495
      ! 5/3/2021
      ! Final Exam Question 4

      PROGRAM FinalQ4
      
      integer :: i
      real :: h, tn, lbound, rbound, yh, yn, yn1, tnm

      ! Declare bounds
      lbound = 1.0 
      rbound = 5.0
      
      i = 0 ! This variable will keep track of the number of iterations
      
      yn = 2 ! y(1) is given
      h = 0.125 ! I chose the grid size to be 1/8 because why not
      tnm = lbound ! t_0 is always going to be the left bound
      
      write(*,*) "Solving the equation y' = t + y over the range t = 
     &[", int(lbound), ", ", int(rbound), "] and step-size ", h, 
     &"using the predictor-corrector method."
      write(*,*)
      
      write(*,*) "Iteration:     "
     &,"t-value:     "
     &,"y-hat value:     "
     &,"y sub n+1 value:     "
      write(*,*) "----------------------------------------------------"
      do while (tn.le.rbound) ! Loop while tn is less than or equal to the right bound
          i = i + 1
          tn = tnm + (h) ! tn is equal to the previous t plus the step size
          yh = yn +( h * (fun(tnm, yn))) ! solve for y hat sub {n+1}
          yn1 = yn + ((0.5 * h) * (fun(tnm, yn) + fun(tn, yh))) ! solve for y sub {n+1}
          
          yn = yn1 ! Set yn equal to yn1 in order to proceed with the next iteration of the method
          tnm = tn ! Set tnm equal to tn in order to proceed with the next iteration of the method
          write(*,*) i, tn, yh, yn1
      end do
      write(*,*)
      write(*,*) "The solution has been found to be", yn1, " after "
     &, i, "iterations of the method."
      
      STOP
      END PROGRAM 
      
      FUNCTION fun(t, y) ! Simple function that returns the value of the function given a t and y value
          
      real :: y, t, fun
          
      fun = t + y
      END FUNCTION