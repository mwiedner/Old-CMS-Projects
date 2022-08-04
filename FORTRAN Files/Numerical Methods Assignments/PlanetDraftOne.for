      ! Michael Wiedner

      PROGRAM Homework7Q1
      implicit none
      integer :: p
      real :: t, rad, theta
      real, dimension(7) :: orbper
      real, dimension(7) :: radius
      
      ! Declare the orbital periods of each planet in Earth days
      orbper(1) = 224.68
      orbper(2) = 365.26
      orbper(3) = 686.98
      orbper(4) = 11.862 * 365.0
      orbper(5) = 29.456 * 365.0
      orbper(6) = 84.07 * 365.0
      orbper(7) = 164.81 * 365.0
      
      ! Declare the average distances from the sun for each planet in AU
      radius(1) = 0.723
      radius(2) = 1.0
      radius(3) = 1.524
      radius(4) = 5.203
      radius(5) = 9.539
      radius(6) = 19.18
      radius(7) = 30.06
      
      p = 0
      t = 0
      
      write(*,*) "Which planet would you like to simulate?"
      write(*,*) "1. Venus 2. Earth 3. Mars 4. Jupiter 5. Saturn
     &6. Uranus 7. Neptune."
      write(*,*) "Enter the corresponding number."
      read(*,*) p
      if (p.LE.0.OR.p.GT.7) then
          write(*,*) "Error: Invalid User Input."
          stop
      end if
      
      write(*,*) "How much time (in Earth days) shall elapse? 
     &Be sure that three orbits occur."
      read(*,*) t
      if (t.lt.0.0) then 
          write(*,*) "Error: Time must be positive."
          stop
      end if
      
      if ((t/orbper(p)).LT.3.0) then
          write(*,*) "Error: Time is too brief. Three orbits are not
     &completed in that time frame."
      stop
      end if
      
      
      ! Begin planet simulations
      rad = radius(p)
      theta = MOD(((t * 360.0) / orbper(p)),360.0)
      
      write(*,*) "The planet is now at the following polar coordinates:"
      write(*,*) "(",rad,"AU ,",theta,"DEG)"
      
      STOP
      END PROGRAM