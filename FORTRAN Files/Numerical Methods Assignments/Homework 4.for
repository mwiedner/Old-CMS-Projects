      PROGRAM BLACKJACK
    
      integer :: count,i, k, drawn, drawn2, drawn3, hndwon, cpuwin, hand, cpuhnd, input  ! count and i are for RNG
      real, dimension(11) :: tempar ! temporary array for calling the
      ! card value function properly. Fortran is so weird man
      real :: x
      LOGICAL drawin, cardck
      
      real, dimension(10000) :: cbank ! Array for random cards. It
      ! appears that the RNG method I am using doesn't create a new
      ! set of random numbers when invoked a second time later
      ! in the code (I'm assuming because it gets the system
      ! clock when the code starts and reuses that value on
      ! subsequent iterations. To help in fixing this, I am
      ! creating an array of 10,000 random numbers 1-52 to improve
      ! the randomness of card drawing.
      real, dimension(52) :: burned ! Array for previously drawn cards
      
      drawn = 0
      hndwon = 0
      cpuwin = 0
      drawn3 = 0
      
      ! RNG
      call SYSTEM_CLOCK(count)
      call SRAND(count) 
      do i = 1,10000
      x = FLOOR(rand() * 51)
      cbank(i) = x
      end do
    
      !write(*,*) cbank(1), cbank(2)
      !x = cvalue(cbank(1))
      !x = cvalue(cbank(2))
      !write(*,*) x
      
      
      
      write(*,*) 'Welcome to Blackjack.'
      do while (drawn.LT.47) 
          cardck = .TRUE.
          write(*,*) 'Here is your initial hand: '
          drawn = drawn + 2
          drawn2 = drawn
          drawn3 = drawn3 + drawn
          
          tempar(drawn2 - 1) = cvalue(cbank(drawn2 - 1))
          do while(cardck)
              do k = 1, 52
                  if (tempar(drawn2 - 1).EQ.burned(k)) then
                      tempar(drawn2 - 1) = cvalue(cbank(drawn3))
                      drawn2 = drawn2 + 1
                      drawn3 = drawn3 + 1
                  end if
              end do 
              cardck = .FALSE.
          end do
          burned(drawn2 - 1) = tempar(drawn2 - 1) ! Burn the card
          
          drawn2 = drawn
          drawn3 = drawn3 + drawn
          cardck = .TRUE.
          tempar(drawn) = cvalue(cbank(drawn))
          do while(cardck)
              do k = 1, 52
                  if (tempar(drawn2).EQ.burned(k)) then
                      tempar(drawn2) = cvalue(cbank(drawn3))
                      drawn2 = drawn2 + 1
                      drawn3 = drawn3 + 1
                  end if
              end do 
              cardck = .FALSE.
          end do
          burned(drawn) = tempar(drawn) ! Burn the drawn card
          
          hand = int(tempar(drawn - 1) + tempar(drawn))
          write(*,*) 'Your current hand is worth', hand
          write(*,*) 'Would you like to 1. stand or 2. hit?'
          read(*,*) input
          if (input.eq.1) then
              write(*,*) 'You stand with a hand of value', hand
          else if (input.eq.2) then 
              write(*,*) 'You take another card.'
              drawin = .TRUE.
              do while (drawin) 
                  drawn = drawn + 1
                  tempar(drawn) = cvalue(drawn)
                  
                  cardck = .TRUE.
                  drawn2 = drawn
                  drawn3 = drawn3 + drawn
                  do while(cardck)
                    do k = 1, 52
                        if (tempar(drawn2).EQ.burned(k)) then
                            tempar(drawn2 - 1) = cvalue(cbank(drawn3))
                            drawn2 = drawn2 + 1
                            drawn3 = drawn3 + 1
                        end if
                    end do 
                    cardck = .FALSE.
                  end do
                  
                  hand = hand + int(tempar(drawn))
                  if ((cvalue(drawn).EQ.(11.0)).AND.(hand.GT.21)) then
                      hand = hand - 10
                  end if
                  write(*,*) 'Your current hand is worth', hand
                  write(*,*) 'Would you like to 1. stand or 2. hit?'
                  read(*,*) input
                  if (input.eq.1) then
                      write(*,*) 'You stand with a hand of value', hand
                      drawin = .FALSE.
                  else if (input.eq.2) then 
                      write(*,*) 'You take another card.'
                  else
                      write(*,*) 'Critical error: invalid input.'
                  end if
                  if (hand.GT.21) then
                      write(*,*) 'You have busted and lost the hand.'
                      hand = 0
                      drawin = .FALSE.
                  end if
              end do
          else
              write(*,*) 'Critical error: invalid input.'
          end if
          ! End player turn
          
          
          
          
          
          
          drawn = drawn + 2
          tempar(drawn - 1) = cvalue(cbank(drawn - 1))
          
          cardck = .TRUE.
                  drawn2 = drawn
                  drawn3 = drawn3 + drawn
                  do while(cardck)
                    do k = 1, 52
                        if (tempar(drawn2).EQ.burned(k)) then
                            tempar(drawn2 - 1) = cvalue(cbank(drawn3))
                            drawn2 = drawn2 + 1
                            drawn3 = drawn3 + 1
                        end if
                    end do 
                    cardck = .FALSE.
                  end do
          
          tempar(drawn) = cvalue(cbank(drawn))
          cardck = .TRUE.
                  drawn2 = drawn
                  drawn3 = drawn3 + drawn
                  do while(cardck)
                    do k = 1, 52
                        if (tempar(drawn2).EQ.burned(k)) then
                            tempar(drawn2 - 1) = cvalue(cbank(drawn3))
                            drawn2 = drawn2 + 1
                            drawn3 = drawn3 + 1
                        end if
                    end do 
                    cardck = .FALSE.
                  end do
          
          cpuhnd = int(tempar(drawn - 1) + tempar(drawn))
          
          if (cpuhnd.GE.16) then
              write(*,*) 'The dealer stands with a hand of', cpuhnd
          else 
              write(*,*) 'The dealer hits.'
              drawin = .TRUE.
              do while (drawin) 
                  drawn = drawn + 1
                  tempar(drawn) = cvalue(drawn)
                  
                  cardck = .TRUE.
                  drawn2 = drawn
                  drawn3 = drawn3 + drawn
                  do while(cardck)
                    do k = 1, 52
                        if (tempar(drawn2).EQ.burned(k)) then
                            tempar(drawn2 - 1) = cvalue(cbank(drawn3))
                            drawn2 = drawn2 + 1
                            drawn3 = drawn3 + 1
                        end if
                    end do 
                    cardck = .FALSE.
                  end do
                  
                  cpuhnd = cpuhnd + int(tempar(drawn))
                  if ((cvalue(drawn).EQ.(11.0)).AND.(cpuhnd.GT.21)) then
                      cpuhnd = cpuhnd - 10
                  end if
                  if (cpuhnd.GE.16) then
                      write(*,*) 'The dealer stands with', cpuhnd
                      drawin = .FALSE.
                  else
                      write(*,*) 'The dealer hits.'
                  end if
                  if (cpuhnd.GT.21) then
                      write(*,*) 'The dealer busted.'
                      cpuhnd = 0
                      drawin = .FALSE.
                  end if
              end do
          end if
          ! End dealer turn
          
          if (hand.GT.cpuhnd) then
              write(*,*) 'You won the hand!'
              hndwon = hndwon + 1
          else if (cpuhnd.GT.hand) then
              write(*,*) 'You lost the hand.'
              cpuwin = cpuwin + 1
          else
              write(*,*) 'Push. No change in score.'
          end if
          
          write(*,*) 'Your current score is', hndwon, 'to', cpuwin
          
          if (drawn.LT.47) then
              write(*,*) 'On to the next round!'
          else
              write(*,*) 'The deck is now empty, Thanks for playing!'
          end if
          
      end do
      
      
      
      
      STOP
      END PROGRAM
      
      function cvalue(i)
          implicit none
          integer :: k
          
          real :: cvalue, i, mod, divi
          !integer :: suit
          character(len = 17) :: flcard
          character(len = 5) :: numbr
          character(len = 8) :: suit
          
          mod = MODULO(i,13.0)
          divi = (i / 13.0)
          
          if (divi.LT.1.0) THEN
              suit = 'Clubs'
          else if (divi.LT.2.0) THEN
              suit = 'Diamonds'
          else if (divi.LT.3.0) THEN
              suit = 'Hearts'
          else if (divi.LT.4.0) THEN
              suit = 'Spades'
          end if
          
          if (mod.EQ.0.0) THEN
              numbr = 'Ace'
              cvalue = 11
          else if (mod.LE.9.0) THEN
              do k = 1, 9
                  if (int(mod).EQ.k) THEN
                  numbr = char(k+1)
                  end if
              end do
              cvalue = mod + 1
          else if (mod.EQ.10.0) THEN
              numbr = 'Jack'
              cvalue = 10
          else if (mod.EQ.11.0) THEN
              numbr = 'Queen'
              cvalue = 10
          else if (mod.EQ.12.0) THEN
              numbr = 'King'
              cvalue = 10
          end if
          
          flcard = numbr//" of "//suit
          write(*,*) flcard
          ! write(*,*) numbr
          ! drawn = drawn + 1 Seems as though functions can't access data members of the original program. Have fun!
          ! burned(drawn) = i
      end function cvalue