      ! Michael Wiedner
      ! Dr. Fuse
      ! Numerical Methods CMS 495
      ! 4/12/2021
      ! Exam 2 Question 1

      
      ! forcedata.txt contains the first 13 pairs of data from the given set. This turns out to be a sufficient sampling of 
      ! the given data so that it includes all relevant data for the problem, but this will be explained later in the code. 
      ! The first half of the lines of numbers are the x-values for the pairs and the second half are the y-values. 
      ! This way xval(i) and yval(i) return the correct data for pair #i.
      ! The data was manually reformatted as the standard, decimal representation instead of being expressed in scientific notation.
      
      
      PROGRAM Exam2Q1
      integer :: i, count
      real :: xiyi, xi, yi, xi2, xip2, m, b
      real, dimension(13) :: xval
      real, dimension(13) :: yval
      
      open (19, file = 'forcedata.txt') ! Open forcedata.txt
      
      count = 0 ! Count represents the number of pairs of relevant data
      
      ! Declare variables used for calculating slope and intercept of linear regression
      xiyi = 0
      xi = 0
      yi = 0
      xi2 = 0
      xip2 = 0
      
      do i = 1, 26 ! Read through the txt file and place the data into arrays. One array for the x values (displacement) and another for the y values (force)
          if (i.le.13) then
          read(19,*) xval(i)
          else
          read(19,*) yval(i - 13)
          end if
      end do
      
      ! Calculate where the data is qualified for the linear fitting. The condition for a given piece of data to qualify 
      ! for the linear fitting is for the difference between the force data of one piece of data and the following 
      ! must be greater than 0.01E-09
      do i = 1, 12 ! Loops only 12 times because there are 13 pairs in our data set, and since we're finding yval(i + 1), it compensates for all 13 pairs
          
          temp = (yval(i) - yval(i + 1)) ! Store the difference as a temporary variable for simplicity
          
          if (temp.lt.0.0) then ! Absolute value-ify
              temp = temp * (-1.0)
          end if
          
          if (temp.gt.(0.01 * (10**(-9)))) then ! Check the condition
              ! The data is deemed relevant so we increase the count number for later use as the index of the arrays
          count = count + 1
          end if
          
      end do
      write(*,*) "Number of relevant pairs of data: ", count ! We are left with 11 out of 13 relevant data pairs. 
      write(*,*) " " ! Blank space for formatting
      
      ! The sample of the first 13 pairs of data from the given set is sufficient for finding all relevant data from the total set since
      ! all of the data after the 11th pair is deemed irrelevant by our condition.
      
      ! The linear data is officially declared to begin with the first pair in the data set and end with the 11th pair, since every pair
      ! following the 11th no longer satisfies the condition of having a difference in force value of greater than 0.01E-09.
      
      ! Begin linear least squares fit.
      
      ! Calculate the summations needed to find m and b
      do i = 1, count
          xiyi = xiyi + (xval(i) * yval(i))
          xi = xi + (xval(i))
          yi = yi + (yval(i))
          xi2 = xi2 + (xval(i)**2)
      end do
      xip2 = xi**2
      
      m = ((count * xiyi) - ((xi) * (yi))) / ((count * (xi2)) - (xip2))
      
      b = ((xi2 * yi) - (xi * xiyi)) / ((count * xi2) - xip2)
      
      write(*,*) "The equation for the linear regression line found
     & using least squares fit is the following:"
      write(*,*) "y = (",m,")x + (",b,")"
      
      STOP
      END PROGRAM