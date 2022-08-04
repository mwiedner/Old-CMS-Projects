        Program Homework2

        Integer resp

        write(*,*) 'For each prompt enter 1 or 2 for the 
     &respective choice.'
        write(*,*) ''
        write(*,*) 'You find yourself in a room with a closed door and a
     & slightly ajar window. Do you climb through the window 
     &or open the door?'

        read(*,*) resp
        IF (resp.EQ.1) THEN
            write(*,*) 'You open the window and climb through, but quick
     &ly realize that you are not on the ground floor - or even close. 
     &You died.'
        ELSE IF (resp.EQ.2) THEN
            write(*,*) 'You open the door and it leads to another room. 
     &Inside, there is a dim lamp illuminating a palletable-looking 
     &hot dog. Do you eat the hot dog or continue searching?'
            read(*,*) resp
            IF (resp.EQ.1) THEN
                write(*,*) 'You take a bite of the hot dog and it is 
     &surprisingly yet pleasantly tangy. Upon closer 
     &inspection, there is a notable amount of purple and 
     &green mold growing on the sausage. Do you panic 
     &or just relax and vibe?'
                read(*,*) resp
                IF (resp.EQ.1) THEN
                    write(*,*) 'You begin to panic. Why would you eat 
     &that hot dog. Are you a fool? This is not good. 
     &Not at all. What can you do? There is nothing you 
     &can do. You are a dead man walking. 
     &Five minutes later, you die.'
                ELSE IF (resp.EQ.2) THEN
                    write(*,*) 'You have nothing to worry about man. 
     &Just relax bro. You are chilling. Dont you feel nice? 
     &Ooh look over there! What is that?! Is that Larry? 
     &Larry the unicorn from your dreams as a toddler? 
     &You should go ride Larry. Do you want to ride Larry 
     &or go back through the door you came?'
                    read(*,*) resp
                    IF (resp.EQ.1) THEN
                        write(*,*) 'Larry is so soft. You have missed 
     &Larry so much. He is such a good friend. You 
     &climb onto Larry. You ride off through the 
     &rolling hills and see a rainbow! Do you want to 
     &ride to the rainbow or travel the plains with 
     &Larry?'
                        read(*,*) resp
                        IF (resp.EQ.1) THEN
                            write(*,*) 'You tell Larry to ride to the 
     &rainbow, but he does not want to. Do you get 
     &off Larry or do you listen to him?'
                            read(*,*) resp
                            IF (resp.EQ.1) THEN
                                write(*,*) 'You climb off of Larry and 
     &walk to the rainbow. When you get there, 
     &there is a leprechaun! You say hello, 
     &but he grows to 20 feet tall and stomps 
     &on you. You should have listened to 
     &Larry. You died.'
                            ELSE IF (resp.EQ.2) THEN
                            write(*,*) 'You trust Larry. Larry is always 
     &right. Never doubt Larry. Ever. Larry is 
     &your friend and you love Larry. Do not 
     &ever think of doubting Larry again. 
     &Larry senses your mutual trust and bond 
     &growing, and he flaps his wings and 
     &flies towards the sunset. When did Larry 
     &get wings? Where am I? Larry turns his head 
     &180 degrees to look you in the eyes and 
     &says: I thought you learned to never 
     &question me. Larry turns his head back 
     &and continues to fly away with you on his 
     &back. The two of you live happily ever 
     &after. Well, as long as you do not 
     &question Larry ever again.'
                            ELSE
                                write(*,*) 'You have entered an 
     &unacceptable 
     &response and broke Zork. 
     &Shame on you.'
                            END IF
                        ELSE IF (resp.EQ.2) THEN
                            write(*,*) 'You listen to Larry and stay 
     &away from the rainbows. Good choice. 
     &Very good choice. You and Larry ride 
     &towards the sunset, living happily 
     &ever after.'
                        ELSE
                            write(*,*) 'You have entered an unacceptable 
     &response and broke Zork. Shame on you.'
                        END IF
                    ELSE IF (resp.EQ.2) THEN
                        write(*,*) 'You have neglected Larry. That was 
     &a mistake. You should have learned by now. 
     &The moment you turn your back to Larry you feel 
     &a piercing pain in your heart. You look down, 
     &and there is a unicorn horn in your chest. 
     &You died.'
                    ELSE
                        write(*,*) 'You have entered an unacceptable 
     &response and broke Zork. Shame on you.'
                    END IF
                ELSE
                    write(*,*) 'You have entered an unacceptable 
     &response and broke Zork. Shame on you.'
                    END IF
            ELSE IF (resp.EQ.2) THEN
                write(*,*) 'You choose to ignore the hot dog and search 
     &for other ways of escaping. There is nothing. The hot 
     &dog was your only hope. But the hot dog 
     &has sensed your disrespect and vanished. You have no 
     &choice but to accept your fate. These rooms are your 
     &final resting place. You died.'
        ELSE
        write(*,*) 'You have entered an unacceptable
     &response and broke Zork. Shame on you.'
        END IF

        STOP
        END IF
        END