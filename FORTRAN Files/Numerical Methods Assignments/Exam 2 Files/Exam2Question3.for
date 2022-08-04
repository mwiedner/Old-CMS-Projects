      ! Michael Wiedner
      ! Dr. Fuse
      ! Numerical Methods CMS 495
      ! 4/12/2021
      ! Exam 2 Question 3

      PROGRAM Exam1Q3
      implicit none
      integer :: i, j
      real :: x, y
      real, dimension(3) :: orbper
      real, dimension(3) :: radius
      real, dimension(3) :: smajax
      real, dimension(3) :: sminax
      real, dimension(3) :: eccent
      character(len = 7), dimension(3) :: names
      
      ! Open the coords.txt file for writing
      open (19, file = 'coords.txt')
      
      ! Declare the names of the planet for clean printing
      names(1) = "Earth"
      names(2) = "Jupiter"
      names(3) = "Neptune"
      
      ! Declare the orbital periods of each planet in Earth days
      orbper(1) = 365.26
      orbper(2) = 11.862 * 365.0
      orbper(3) = 164.81 * 365.0
      
      ! Declare the average distances from the sun for each planet in AU (The radius of its orbit)
      radius(1) = 1.0
      radius(2) = 5.203
      radius(3) = 30.06
      
      ! Declare the length of the semi-major axis of the planet's elliptical orbit in AU
      smajax(1) = 0.999987495143
      smajax(2) = 5.20441899578
      smajax(3) = 30.04762018982
      
      ! Declare the orbital eccentricity of the planets' orbits
      eccent(1) = 0.0167
      eccent(2) = 0.0489
      eccent(3) = 0.0113
      
      ! The semi-minor axis is equal to the semi-major axis times the square root of (1 - (eccentricity squared))
      do i = 1, 3
          sminax(i) = smajax(i) * ((1 - (eccent(i)**2))**(0.5))
      end do
      
      ! Begin orbit simulation. Each planet will orbit 5 times and be plotted 4 times per orbit.
      do i = 1, 3 ! Loop for each of the three planets
          
          write(19,*) trim(names(i)) ! Print the name of the planet after using the trim function to remove white space from the end of the string.
          
          do j = 1, 20 ! Loop 20 times per planet for 20 different coordinates
              
              ! Planets' coordinates are plotted at each of the orbits' vertices as the four points per orbit.
              if (mod(j,4).eq.0) then
                  x = smajax(i)
                  y = 0
              else if (mod(j,4).eq.1) then
                  x = 0
                  y = sminax(i)
              else if (mod(j,4).eq.2) then
                  x = smajax(i) * (-1.0)
                  y = 0
              else if (mod(j,4).eq.3) then
                  x = 0
                  y = sminax(i) * (-1.0)
              end if
              
              write(19,*) "X = ", x, ", Y = ", y
          end do
          
      end do
      
      STOP
      END PROGRAM