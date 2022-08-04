      ! Michael Wiedner
      ! Dr. Fuse
      ! Numerical Methods CMS 495
      ! 5/3/2021
      ! Final Exam Question 1

      PROGRAM FinalQ1
      
      integer :: i, rando2, count
      real :: rando1,srad,sysdis,orbrad,depth,bright,tbr,rvar,rando3,ra
      real, dimension(10) :: prad
      real, dimension(120) :: randy
      
      ! We will be using Wikipedia data on the star K2-18. 
      srad = 0.469 * 0.00465047 ! Star's radius was given in solar radii so I converted to AU
      sysdis = 1070 * 63241 ! Distance from observer to the system, converted to AU (This distance is the distance given by the problem, not the distance between Earth and K2-18)
      orbrad = 1 ! Orbital radius is 1 AU
      bright = 0.035 ! Measured in Solar Luminosities
      bright = bright * 4.83 ! Convert figure to Absolute Magnitude
      
      ! RNG -----------------------------------------------------------
      call SYSTEM_CLOCK(count)
      call SRAND(count)
      do i = 1, 120 ! Loop is used because we need to store 120 random variables for future use in our simulation.
      rando1 = (rand() * 0.1) ! First random variable is the percentage change along the range [-10%, 10%]
      rando2 = 1 + FLOOR(rand() * 2) ! Second variable is to randomize if negative or positive
      
      if (rando2.eq.1) then ! If second variable is 1, then make the first variable negative
          rando1 = rando1 * (-1)
      end if
      randy(i) = rando1 ! Store the random number
      end do
      
      do i = 1, 10 ! Third random variable determines random planet radii. 
          rando3 = 0.01 + (rand() * 0.05) ! Generates a random value for radius in solar radii for consistency, with a minimum radius of 0.01 solar radii.
          prad(i) = rando3 * 0.00465047 ! Radius is immediately converted to AU and assigned to a value in the prad array
      end do
      ! ---------------------------------------------------------------
      
      
      write(*,*) "This program models the change in stellar brightness 
     &over time
     &of an M2 star 1070 light years from Earth with one planet in 
     &transit orbiting circularly with a radius of 1 AU, for 10 
     &different, random planet radii."
      write(*,*) "----------------------------------------------------"
      write(*,*)
      
      ! Minimum planet radius calculation
      depth = 0.0
      tbr = 0.0
      ! This do loop tests if the change in stellar brightness is perceived by increasing the planet's radius by small increments until a change is detectable.
      do while ((bright - tbr).gt.0.000000001) ! The program prints up to 9 decimal places, so if a change in stellar brightness is less than that figure, then the program cannot detect it and thus it is the minimum planet radius.
          ra = (ra + 0.000000001) * 0.00465047 ! Increase the increment of the radius by the figure mentioned above, but convert the radius to AU.
      depth = ((ra / srad)**2) ! Calculate depth
      tbr = (bright * (1 - depth)) ! Adjust the true brightness based on the depth.
      end do
      write(*,*) "The minimum planet radius that can cause the 
     &simulation to perceive a change in stellar brightness 
     &is ", ra, "AU, or ", ra* (1.496*(10**8)), " km."
      write(*,*) "----------------------------------------------------"
      write(*,*) "Each planet will have an orbital period of one Earth
     &year and the data will be plotted as (years, absolute magnitude)."
      write(*,*)
      
      
      do i = 1, 10 ! Main loop for all 10 planets
      write(*,*) "Simulated brightness over time, measured in years
     &and Absolute 
     &Magnitude, for planet #", i," with planet radius", prad(i),
     &" AU, or ", prad(i) * (1.496*(10**8)), "kilometers:" ! Display planet number and its radius in both AU and kilometers
      write(*,*)
      write(*,*) "    Years          Absolute Magnitude"
          do j = 1, 12 ! Loop for each individual planet. Done 12 times, so plotted four times per orbit, once where the planet is in front of the star, and once where it is not.
              rvar = randy(((i-1) * 12) + j) ! Define the random variable as one of the values from the randy array. This variable will be a random number along the range [-0.1, 0.1]
              if (mod(j,4).ne.0) then ! Planet is not in front of the star
                  depth = 0.0 ! Depth is the percentage change of the brightness based on interferences, namely planet transit and random noise. Here, depth does not change because there is no planet in transit in front of the star, except for random noise. The random variable will add a +- 10% change to the brightness.
              else ! Planet is in front of the star
                  depth = ((prad(i) / srad)**2) ! Adjust depth based on the planet radius
              end if
              tbr = (bright * (1 - depth)) * (1 - rvar) ! Total brightness is then the brightness multiplied by 1 - depth, so the percentage change based on the transit of the planet. But then also multiplied by 1 - rvar to add random noise.
              write(*,*) j*0.25, tbr
          end do
          write(*,*)
      end do
      
      STOP
      END PROGRAM 