      PROGRAM midnight
          
      ! My compiler really hates using variables as array indexes.
      ! I am unsure if this is a common issue because looking it up
      ! told me nothing.
          
      LOGICAL play
      play = .TRUE.
      
      
      do while(play)
          
      integer :: count,i,input  ! count and i are for RNG, input is 
      ! for user input
      
      real x,paone,pafour,pbone,pbfour    ! x is for user input, the
      ! rest are boolean-esque integers for testing if 1 or 4 is in
      ! the user bank.
      
      real, dimension(42) :: rolls ! Array for die rolls. 42 because
      ! that is the maximum number of die that will need to be 
      ! rolled in a game. 2(6+5+4+3+2+1) = 42
      real, dimension(6) :: pabank ! Player A bank
      real, dimension(6) :: pbbank ! Player B bank
      
      ! Booleans for reading console, Players' turns
      LOGICAL reads, pat, pbt
      reads = .TRUE.
      pat = .TRUE.
      pbt = .TRUE.
      
      paone = 0.0
      pafour = 0.0
      pbone = 0.0
      pbfour = 0.0
      
      ! Make banks initially empty
      pabank = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)
      pbbank = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)
      
      ! RNG
      call SYSTEM_CLOCK(count)
      call SRAND(count) 
      do i = 1,42
      x = 1 + FLOOR(rand() * 6)
      rolls(i) = x
      end do
          
          
          write(*,*) 'Welcome to Midnight.'
          write(*,*) 'Use the number keys to control the game.'
          
          INTEGER track = 0
          INTEGER round = 1
          do while (pat)
          INTEGER banked = 0
          write(*,*) 'PLAYER 1, here is your set of die rolls:'
          do i = 1, (6 - track)
          write(*,"(I1,A2,F3.1)") i, ': ', rolls(((round - 1) * 6) + i)
          end do
          write(*,*) 'Enter which die to bank and 0 when done.'
          
          do while (reads)
              read(*,*) input
              
              if ((input.GE.1).AND.(input.LE.(6 - track))) THEN
                  pabank(track + 1) = rolls((round - 1) * 6 + input)
                  track = track + 1
                  banked = banked + 1
              else if (banked.EQ.0) THEN
                  write(*,*) 'You must bank one or more die.'
              else
                  reads = .FALSE.
              end if
          end do
          
          write(*,*) 'Here is your current bank:'
          do i = 1, 6
              write(*,*) pabank(i)
          end do
          
          if (track.EQ.6) THEN
              pat = .FALSE.
          end if 
          
          round = round + 1
          end do ! Player A turn loop
          
          
          
          
          
          
          INTEGER track = 0
          INTEGER round = 1
          do while (pat)
          INTEGER banked = 0
          write(*,*) 'PLAYER 2, here is your set of die rolls:'
          do i = 1, (6 - track)
          write(*,"(I1,A2,F3.1)") i, ': ', rolls(((round - 1) * 6) + 21 + i)
          end do
          write(*,*) 'Enter which die to bank and 0 when done.'
          
          do while (reads)
              read(*,*) input
              
              if ((input.GE.1).AND.(input.LE.(6 - track))) THEN
                  pbbank(track + 1) = rolls((round - 1) * 6 + input)
                  track = track + 1
                  banked = banked + 1
              else if (banked.EQ.0) THEN
                  write(*,*) 'You must bank one or more die.'
              else
                  reads = .FALSE.
              end if
          end do
          
          write(*,*) 'Here is your current bank:'
          do i = 1, 6
              write(*,*) pbbank(i)
          end do
          
          if (track.EQ.6) THEN
              pbt = .FALSE.
          end if 
          
          round = round + 1
          end do ! Player B turn loop
          
          
          ! Score tally
          REAL pascor = 0
          REAL pbscor = 0
          
          do i = 1, 6
              if (pabank(i).EQ.(1.0)) THEN
                  paone = 1.0
              end if
              if (pabank(i).EQ.(4.0)) THEN 
                  pafour = 1.0
              end if
              pascor = pascor + pabank(i)
              
              if (pbbank(i).EQ.(1.0)) THEN
                  pbone = 1.0
              end if
              if (pbbank(i).EQ.(4.0)) THEN 
                  pbfour = 1.0
              end if
              pbscor = pbscor + pbbank(i)
          end do
          
          pascor = pascor * paone * pafour
          pbscor = pbscor * pbone * pbfour
          
          write(*,*) 'Player 1 score:'
          write(*,*) pascor
          write(*,*) 'Player 2 score:'
          write(*,*) pbscor
          
          IF (pascor.GT.pbscor) THEN
              write(*,*) 'Player 1 has won.'
              play = .FALSE.
          ELSE IF (pbscor.GT.pascor) THEN
              write(*,*) 'Player 2 has won.'
              play = .FALSE.
          ELSE
              write(*,*) 'Tie! Rematch!'
          END IF
      end do ! End main game loop
      
     
 
     

      
     
      STOP
      END program midnight