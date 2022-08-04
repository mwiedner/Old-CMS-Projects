      ! Michael Wiedner
      ! Dr. Fuse
      ! Numerical Methods CMS 495
      ! 5/3/2021
      ! Final Exam Question 2
      
      PROGRAM FinalQ2
      integer :: i, count, j
      real :: xiyi,xi,yi,xi2,xip2
      real, dimension(8) :: period
      real, dimension(8) :: reldis
      real, dimension(6) :: mval
      real, dimension(6) :: bval
      real, dimension(6) :: te
      
      ! Fill the arrays with the respective values for the period (in years) and relative distance (in AU)
      period = (/0.242,0.616,1.00,1.881,11.86,29.33,84.32,164.79/)
      reldis = (/0.388,0.724,1.00,1.524,5.20,9.51,19.23,30.10/)
      
      count = size(period) ! The count variable will be the number of pairs of data. In this case, it is 8.
      
      ! Declare variables used for calculating slope and intercept of linear regression
      xiyi = 0
      xi = 0
      yi = 0
      xi2 = 0
      xip2 = 0
      
      write(*,*) "This program uses the linear least-squares fit method 
     &to find a linear regression line for planet data. First using 
     &all given planet data, then partitioning the data and using 
     &several different linear fits, then comparing the results to find
     &which is more accurate using an absolute error analysis."
      write(*,*)
      write(*,*) "----------------------------------------------------"
      write(*,*)
      
      ! Begin linear least squares fit for ALL planets.
      
      ! Calculate the summations needed to find m and b
      do i = 1, count
          xiyi = xiyi + (period(i) * reldis(i))
          xi = xi + (period(i))
          yi = yi + (reldis(i))
          xi2 = xi2 + (period(i)**2)
      end do
      xip2 = xi**2
      
      mval(1) = ((count * xiyi) - ((xi) * (yi))) / 
     &((count * (xi2)) - (xip2))
      
      bval(1) = ((xi2 * yi) - (xi * xiyi)) / ((count * xi2) - xip2)
      
      write(*,*) "The equation for the linear regression line found
     & using least squares fit on ALL planet data is the following:"
      write(*,*) "y = (",mval(1),")x + (",bval(1),")"
      write(*,*) "----------------------------------------------------"
      write(*,*)
      
      ! Begin linear least squares fit for T1 to T4
      
      count = (size(period))/2 ! The count variable will be the number of pairs of data. In this case, it is 4.
      
      ! Declare variables used for calculating slope and intercept of linear regression
      xiyi = 0
      xi = 0
      yi = 0
      xi2 = 0
      xip2 = 0
      
      ! Begin linear least squares fit.
      
      ! Calculate the summations needed to find m and b
      do i = 1, count
          xiyi = xiyi + (period(i) * reldis(i))
          xi = xi + (period(i))
          yi = yi + (reldis(i))
          xi2 = xi2 + (period(i)**2)
      end do
      xip2 = xi**2
      
      mval(2) = ((count * xiyi) - ((xi) * (yi))) / 
     &((count * (xi2)) - (xip2))
      
      bval(2) = ((xi2 * yi) - (xi * xiyi)) / ((count * xi2) - xip2)
      
      write(*,*) "The equation for the linear regression line found
     & using least squares fit on T-planet (four innermost planets)
     &data is the following:"
      write(*,*) "y = (",mval(2),")x + (",bval(2),")"
      write(*,*) "----------------------------------------------------"
      
      ! Begin linear least squares fit for T-planets plus j G-planets
      
      do j = 1, ((size(period)) / 2) - 1 ! Loop 3 times
      
      count = ((size(period)) / 2) + j! The count variable will be the number of pairs of data. In this case, it is 4 + j.
      
      ! Declare variables used for calculating slope and intercept of linear regression
      xiyi = 0
      xi = 0
      yi = 0
      xi2 = 0
      xip2 = 0
      
      ! Begin linear least squares fit calculation.
      
      ! Calculate the summations needed to find m and b
      do i = 1, count
          xiyi = xiyi + (period(i) * reldis(i))
          xi = xi + (period(i))
          yi = yi + (reldis(i))
          xi2 = xi2 + (period(i)**2)
      end do
      xip2 = xi**2
      
      mval(j+2) = ((count * xiyi) - ((xi) * (yi))) / 
     &((count * (xi2)) - (xip2))
      
      bval(j+2) = ((xi2 * yi) - (xi * xiyi)) / ((count * xi2) - xip2)
      
      write(*,*) "The equation for the linear regression line found
     & using least squares fit on T-planet (four outermost planets) 
     &data plus", j, "G-planet(s) is the following:"
      write(*,*) "y = (",mval(j+2),")x + (",bval(j+2),")"
      write(*,*) "----------------------------------------------------"
      write(*,*)
      end do
      
      ! Begin linear least squares fit for ONLY G-planets
      
      count = (size(period)) / 2 ! The count variable will be the number of pairs of data. In this case, it is 4.
      
      ! Declare variables used for calculating slope and intercept of linear regression
      xiyi = 0
      xi = 0
      yi = 0
      xi2 = 0
      xip2 = 0
      
      ! Begin linear least squares fit calculation.
      
      ! Calculate the summations needed to find m and b
      do i = 1, count
          xiyi = xiyi + (period(i+4) * reldis(i+4))
          xi = xi + (period(i+4))
          yi = yi + (reldis(i+4))
          xi2 = xi2 + (period(i+4)**2)
      end do
      xip2 = xi**2
      
      mval(6) = ((count * xiyi) - ((xi) * (yi))) / 
     &((count * (xi2)) - (xip2))
      
      bval(6) = ((xi2 * yi) - (xi * xiyi)) / ((count * xi2) - xip2)
      
      write(*,*) "The equation for the linear regression line found
     & using least squares fit on G-planet (four outermost planets) 
     &data is the following:"
      write(*,*) "y = (",mval(6),")x + (",bval(6),")"
      write(*,*) "----------------------------------------------------"
      write(*,*)
      
      
      write(*,*) "To evaluate how to optimize our linear regression, 
     &we must calculate error. For each equation we found, we will
     &find the total absolute error, which we will define as the sum of
     &the differences between the actual value and estimated value
     &for all pairs of data. If an equation has a lower total absolute
     &error, then it is more accurate."
      
      ! Begin error calculations
      
      count = size(period)
      do i = 1, 6
          te(i) = 0
      end do
      
      do i = 1, count
      te(1) = te(1) + abs(period(i) - fun(period(i),mval(1),bval(1)))
      end do
      
      count = (size(period))/2.0 ! Change the value of count since the relevant data is the half of all of the data, so 4
      
      do i = 1, count ! First four data pairs for four innermost planets
      te(2) = te(2) + abs(period(i) - fun(period(i),mval(2),bval(2)))
      end do
      
      do i = 1, count ! Last four data pairs for four outermost planets
      te(6) = te(6) + abs(period(i+4) - 
     &fun(period(i+4),mval(6),bval(6)))
      end do
      
      do j = 1, count - 1 ! Tracks which equation
      do i = 1, count + j ! Tracks which variable
      te(j+2) = te(j+2) + abs(period(i) - 
     &fun(period(i),mval(j+2),bval(j+2)))
      end do
      end do
      
      write(*,*)
      write(*,*) "The total absolute errors for the equations and 
     &their data are the following:"
     &, te(1), te(2), te(3), te(4), te(5), te(6)
      
      write(*,*)
      write(*,*) "The outermost planets increase the magnitude of 
     &absolute error by", te(1)-te(2), ", which is a "
     &, (te(1) / te(2)) * 100,
     &"percent increase and quite significant."
      write(*,*)
      write(*,*) "Since the total absolute error is lower for 
     &the equations in which the data is partitioned, we can conclude
     &that our estimations are better fit with two unique linear fits." 
      
      STOP
      END PROGRAM
      
      FUNCTION fun(x,m,b) ! Function for finding the y-value given an x, slope, and y-intercept
          real :: x, m, b, fun
          
          fun = (m * x) + b
      END FUNCTION