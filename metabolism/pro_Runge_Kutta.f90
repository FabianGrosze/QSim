!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualit�t
!
!   Copyright (C) 2020 Bundesanstalt f�r Gew�sserkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie k�nnen es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation ver�ffentlicht, weitergeben und/oder modifizieren. 
!   Die Ver�ffentlichung dieses Programms erfolgt in der Hoffnung, da� es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT F�R EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

  subroutine Pro_Runge_Kutta(xk,yk,Qquell,Qsenk)
                                                            
  
!     Ein Programm zur numerischen L�sung gewoehnlicher Differenzial-Gleichungen
!     mit dem Runge-Kutta-Verfahren                                                       
                                                            
!     AUTOR:VOLKER KIRCHESCH                                 
                                                            
!     STAND:09.11.2009                                       
                                                            
                                                            
     double precision   :: k1,k2,k3,k4,Qsenk,Qquell,Q
      
!    yk Stoffgroesse (N�hrstoffe, Rotatorien etc.)
!    xk Prozessraten (�, resp, mor)

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
