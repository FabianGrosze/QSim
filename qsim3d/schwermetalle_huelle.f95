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
!> Subroutine schwermetalle_huelle() simulates heavy metals
!! \n\n
!! Quelle: schwermetalle_huelle.f95 ; zurück: \ref lnk_schwermetalle
subroutine schwermetalle_huelle(i)
   use modell
   use QSimDatenfelder
   use module_aparam
   implicit none
   integer :: i,npla,ntra,nben
   iglob = i + meinrang * part
   control = iglob == kontrollknoten  ! i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   npla = (i-1)*number_plankt_vari ! Ort im Feld der transportierten planktischen Variablen
   ntra = (i-1)*number_trans_quant
   nben = (i-1)*number_benth_distr
   
   call schwermetalle_kern(                                  &
                            planktonic_variable_p( 52+npla)  &
                           ,planktonic_variable_p( 52+npla)  &
                           ,planktonic_variable_p( 66+npla)  &
                           ,planktonic_variable_p( 66+npla)  &
                           ,transfer_quantity_p(12+ntra)     &
                           ,iformVert                        &
                           ,anzZeit                          &
                           ,transfer_quantity_p( 5+ntra)     &
                           ,benthic_distribution_p(26+nben)  &
                           ,benthic_distribution_p(27+nben)  &
                           ,benthic_distribution_p(28+nben)  &
                           ,planktonic_variable_p( 80+npla)  &
                           ,planktonic_variable_p( 81+npla)  &
                           ,planktonic_variable_p( 82+npla)  &
                           ,planktonic_variable_p( 83+npla)  &
                           ,planktonic_variable_p( 84+npla)  &
                           ,planktonic_variable_p( 85+npla)  &
                           ,planktonic_variable_p( 86+npla)  &
                           ,planktonic_variable_p( 87+npla)  &
                           ,planktonic_variable_p( 88+npla)  &
                           ,planktonic_variable_p( 89+npla)  &
                           ,planktonic_variable_p( 90+npla)  &
                           ,planktonic_variable_p( 91+npla)  &
                           ,planktonic_variable_p( 92+npla)  &
                           ,planktonic_variable_p( 93+npla)  &
                           ,planktonic_variable_p( 94+npla)  &
                           ,planktonic_variable_p( 95+npla)  &
                           ,planktonic_variable_p( 96+npla)  &
                           ,planktonic_variable_p( 97+npla)  &
                           ,planktonic_variable_p( 98+npla)  &
                           ,planktonic_variable_p( 99+npla)  &
                           ,planktonic_variable_p(100+npla)  &
                           ,planktonic_variable_p(101+npla)  &
                           ,control,iglob)
   return
end subroutine schwermetalle_huelle
