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

  subroutine runge_kutta(yk,xk,h,n,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix)                                   
!                                                                       
!                                                                       
!     Ein Programm zur numerischen L�sung gewoehnlicher Differenzial-Gle
!     mit dem Runge-Kutta-Verfahren                                     
!                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
!                                                                       
!     STAND:12.08.2007                                                  
!                                                                       
!                                                                       
      real k1,k2,k3,k4 
!                                                                       
!                                                                       
      j_intern = 1

      up_N2i = 0.0 
      up_Ci = 0.0

      xk = xk*tflie 

        if(j_intern==0)then
          up_Ci = ((Qmxi-yk)/(Qmxi-Qmni))*upmxi*(CNaehr/(CNaehr+Halbi))                                     
          if(jcyano.eq.1.and.ifix.eq.1)up_N2i = ((Qmxi-yk)/(Qmxi-Qmni))*upmxi*(Halbi/(CNaehr+Halbi))                                      
            else if(j_intern==1)then
              up_Ci = upmxi*((1.-yk/Qmxi)/(1.-yk/Qmxi+0.01))*(CNaehr/(CNaehr+Halbi))                                     
              if(jcyano.eq.1.and.ifix.eq.1)up_N2i = upmxi*((1.-yk/Qmxi)/(1.-yk/Qmxi+0.01))*(Halbi/(CNaehr+Halbi))
        endif
         
        up_Ci = up_Ci*tflie 
        up_N2i = up_N2i*tflie 
                                                                       
        yk0 = yk*exp(-xk)
        yk = yk0 + up_Ci + up_N2i

        if(yk>Qmxi)then
          up_Ci = (Qmxi - yk0)*(up_Ci/(up_Ci + up_N2i))
          up_N2i = (Qmxi - yk0)*(up_N2i/(up_Ci + up_N2i)) 
          yk = Qmxi
        endif

                                                                       

  END subroutine runge_kutta                                          
