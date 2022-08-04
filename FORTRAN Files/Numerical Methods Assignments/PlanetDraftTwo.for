      ! Michael Wiedner, Lauren Gietzen and Nikola Vuckevic
      ! Dr. Fuse
      ! Numerical Methods CMS 495
      ! 4/2/2021
      ! Homework 7 Question 1

      PROGRAM Homework7Q1
      implicit none
      integer :: i, j
      real :: t, rad, theta
      real, dimension(7) :: orbper
      real, dimension(7) :: radius
      character(len = 7), dimension(7) :: names
      
      ! Open the coords.txt file for writing
      open (19, file = 'coords.txt')
      
      ! Declare the names of the planet for clean printing
      names(1) = "Venus"
      names(2) = "Earth"
      names(3) = "Mars"
      names(4) = "Jupiter"
      names(5) = "Saturn"
      names(6) = "Uranus"
      names(7) = "Neptune"
      
      ! Declare the orbital periods of each planet in Earth days
      orbper(1) = 224.68
      orbper(2) = 365.26
      orbper(3) = 686.98
      orbper(4) = 11.862 * 365.0
      orbper(5) = 29.456 * 365.0
      orbper(6) = 84.07 * 365.0
      orbper(7) = 164.81 * 365.0
      
      ! Declare the average distances from the sun for each planet in AU (The radius of its orbit)
      radius(1) = 0.723
      radius(2) = 1.0
      radius(3) = 1.524
      radius(4) = 5.203
      radius(5) = 9.539
      radius(6) = 19.18
      radius(7) = 30.06
      
      ! Begin orbit simulation. Coordinates will be written as polar coordinates instead of cartesian coordinates because it's a lot easier.
      do i = 1, 7 ! Loop for each of the seven planets
          
          write(19,*) trim(names(i)) ! Print the name of the planet after using the trim function to remove white space from the end of the string.
          
          do j = 1, 20 ! Loop 20 times per planet for 20 different coordinates
              
              rad = radius(i) ! The radius is always simply the average distance from the sun for all values of theta.
              
              t = ((orbper(i) * 3) / 20) * j ! Each planet is to be plotted 20 times. This means that each time value must equal
              ! the previous time plus a twentieth of three orbital periods. This number is then multiplied by the number of coordinates plotted plus one to 
              ! get an increment of the previous figure as the step-size of t.
              
              theta = MOD(((t * 360.0) / orbper(i)),360.0) ! Theta is then equal to the time elapsed in days (which was calculated above) times 360 degrees, then 
              ! divided by the orbital period of the planet, mod 360.
              
              write(19,*) "(",rad,"AU ,",theta,"DEG)" ! Write the polar coordinate to the coords.txt file.
              
          end do
          
      end do
      
      STOP
      END PROGRAM