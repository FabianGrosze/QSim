! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !
subroutine zooplankton_wrapper_3d(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   
   integer, intent(in) :: i
   
   
   iglob = i + meinrang * part
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   kontroll = iglob == kontrollknoten
   np = (i-1) * number_plankt_vari
   nt = (i-1) * number_trans_quant
   
   
   if (kontroll) then
      print*,"before zooplankton:"
      print*, "   zooind = ", planktonic_variable_p(50 + np)
      print*, ""
   endif 
   
   call zooplankton(
            planktonic_variable_p(50 + np), ! zooind
            planktonic_variable_p( 8 + np), ! aki
            planktonic_variable_p( 9 + np), ! agr
            planktonic_variable_p(10 + np), ! abl
            planktonic_variable_p( 1 + np), ! tempw
            planktonic_variable_p( 2 + np), ! vo2
            tflie,                          ! tflie
            transfer_quantity_p( 6 + nt),   ! abszo
            transfer_quantity_p(42 + nt),   ! ir
            transfer_quantity_p(16 + nt),   ! zexki
            transfer_quantity_p(17 + nt),   ! zexgr
            transfer_quantity_p(18 + nt),   ! zexbl
            transfer_quantity_p(27 + nt),   ! dzres1
            transfer_quantity_p(28 + nt),   ! dzres2
            transfer_quantity_p(73 + nt),   ! algzob
            transfer_quantity_p(72 + nt),   ! algzog
            transfer_quantity_p(53 + nt),   ! algzok
            transfer_quantity_p(76 + nt),   ! rmuas
            transfer_quantity_p(77 + nt),   ! rakr
            transfer_quantity_p(78 + nt),   ! rbar
            transfer_quantity_p(79 + nt),   ! iras
            kontroll, iglob)
         
   if (kontroll) then
      print*, "after zooplankton:"
      print*, "   zooind = ", planktonic_variable_p(50 + np)
      print*, "   abszo  = ", transfer_quantity_p( 6 + nt)
      print*, "   ir     = ", transfer_quantity_p(42 + nt)
      print*, "   zexki  = ", transfer_quantity_p(16 + nt)
      print*, "   zexgr  = ", transfer_quantity_p(17 + nt)
      print*, "   zexbl  = ", transfer_quantity_p(18 + nt)
      print*, "   dzres1 = ", transfer_quantity_p(27 + nt)
      print*, "   dzres2 = ", transfer_quantity_p(28 + nt)
      print*, "   algzob = ", transfer_quantity_p(73 + nt)
      print*, "   algzog = ", transfer_quantity_p(72 + nt)
      print*, "   abszo  = ", transfer_quantity_p(53 + nt)
      print*, "   algzok = ", transfer_quantity_p(76 + nt)
      print*, "   rmuas  = ", transfer_quantity_p(77 + nt)
      print*, "   rakr   = ", transfer_quantity_p(78 + nt)
      print*, "   iras   = ", transfer_quantity_p(79 + nt)
      print*, ""
   endif
   
   return
end subroutine zooplankton_wrapper_3d

