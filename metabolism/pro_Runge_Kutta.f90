  subroutine Pro_Runge_Kutta(xk,yk,Qquell,Qsenk)
                                                            
  
!     Ein Programm zur numerischen Lösung gewoehnlicher Differenzial-Gleichungen
!     mit dem Runge-Kutta-Verfahren                                                       
                                                            
!     AUTOR:VOLKER KIRCHESCH                                 
                                                            
!     STAND:09.11.2009                                       
                                                            
                                                            
     double precision   :: k1,k2,k3,k4,Qsenk,Qquell,Q
      
!    yk Stoffgroesse (Nährstoffe, Rotatorien etc.)
!    xk Prozessraten (µ, resp, mor)

     n = 4
     h = 0.25

     Q = (Qquell-Qsenk)

     do j = 1,n
       k1 = yk*xk+Q
       k2 = (yk+0.5*h*k1)*xk+Q 
       k3 = (yk+0.5*h*k2)*xk+Q
       k4 = (yk+h*k3)*xk+Q
       yk = yk+((h/6.)*(k1+2.*k2+2.*k3+k4))
     enddo

      end subroutine Pro_Runge_Kutta                                                
