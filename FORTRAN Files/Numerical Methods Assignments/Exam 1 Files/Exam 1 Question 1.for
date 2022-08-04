      ! Michael Wiedner
	  ! Dr Fuse
	  ! CMS 495 - Numerical Methods
	  ! 3/5/2021
	  ! Exam 1 Question 1
	  
      PROGRAM Problem 1
    
      integer :: i, j
      real :: z, k
      
      
      ! Index 1 is for beam #1, index 2 is for beam #2, etc.
      real, dimension(5) :: b1
      real, dimension(5) :: b2
      real, dimension(5) :: b3
      real, dimension(5) :: b4
      real, dimension(5) :: b5
      
      
      b1 = (/10.0,3.0,2.0,1.7,5.0/)
      b2 = (/4.0,1.5,6.0,0.5,4.5/)
      b3 = (/15.0,2.0,2.0,1.0,1.0/)
      b4 = (/8.0,0.75,8.0,2.0,1.9/)
      b5 = (/20.0,2.0,4.0,1.4,3.0/)
      
      
      
      do i = 1, 5
          
          write(*,*) !Blank space for formatting
          write(*,*) !Blank space for formatting
          write(*,*) "Beam No.",i, "Total Length =", b1(i), "ft"
          write(*,*) !Blank space for formatting
          write(*,*) "Distance of Load From Fixed End", "             "
     &,"Deflection"
          do j = 1, 10
              k = real(j)
              if (j.le.b1(i)) then
                  res = de(b5(i),k,b4(i),b1(i),b2(i),b3(i))
                  write(*,*) k, "                      ",res
              end if
              if (((b1(i)-k).LT.1.0).AND.((b1(i)-k).GT.0.0)) then
                  res = de(b5(i),b1(i),b4(i),b1(i),b2(i),b3(i))
                  write(*,*) b1(i), "                          ",res
                  exit
              end if
          end do
          
      end do 
      ! a b h e L
      STOP
      END ! End the program
      
      FUNCTION de(l,a,e,le,b,h)
      real :: de
          
      real :: l,a,e,le,b,h
      
      de = ((l*(a**2.0))/((2.0*e*(b*(h**3.0)))/12.0))*(le-(a/3.0))
      
      
      END FUNCTION