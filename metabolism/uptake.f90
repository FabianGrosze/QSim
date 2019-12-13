  subroutine uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)                                    
                                                                       
                                                                       

!     Ein Programm zur Berechnung der N�hrstoffaufnahme durch Algen, sowie des Zellinternen N�hrstoffgehalts

                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            


                                                                       
!     STAND:27.11.2015                                                


!     Funktion: dQ/dt = upX - �*Q

                                                                       
      j_intern = 1   ! Wahl der Funktion f�r die Abh�ngigkeit der Aufnahmerate von der zellinternen Konzentration
      ms = 10

      up_N2i = 0.0 
      up_Ci = 0.0

      sumup_C = 0.0
      sumup_N2 = 0.0

      deltat = tflie/ms

      xk = max(0.0,xk*deltat) ! Wachstumsrate �
                                                                       

      do m = 1, ms
        if(j_intern==0)then
          up_Ci = ((Qmxi-yk)/(Qmxi-Qmni))*upmxi*(CNaehr/(CNaehr+Halbi))                                     
          if(jcyano.eq.1.and.ifix.eq.1)up_N2i = ((Qmxi-yk)/(Qmxi-Qmni))*upmxi*(Halbi/(CNaehr+Halbi))                                      
            else if(j_intern==1)then
              up_Ci = upmxi*((1.-yk/Qmxi)/(1.-yk/Qmxi+0.01))*(CNaehr/(CNaehr+Halbi))
              if(jcyano.eq.1.and.ifix.eq.1)up_N2i = upmxi*((1.-yk/Qmxi)/(1.-yk/Qmxi+0.01))*(Halbi/(CNaehr+Halbi))
        endif

        up_Ci = up_Ci*deltat 
        up_N2i = up_N2i*deltat

        sup_C = up_Ci + up_N2i

        ykaus = yk
        yk = yk + (sup_C - xk*yk)

        if(yk>Qmxi)then
          dyk = yk - Qmxi
          sup_C = sup_C - dyk
          if((up_Ci + up_N2i)==0.0)then
            up_Ci = 0.0
            up_N2i = 0.0
              else
               up_Ci = sup_C * (up_Ci/(up_Ci + up_N2i))
               up_N2i = sup_C * (up_N2i/(up_Ci + up_N2i))
          endif 
          yk = Qmxi
        endif

        sumup_C = sumup_C + up_Ci
        sumup_N2 = sumup_N2 + up_N2i
      enddo  


        up_Ci = sumup_C
        up_N2i = sumup_N2 


  END subroutine uptake                                      

