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
!> Subroutine erosion_huelle() simulaties erosion of suspended Matters
!! \n\n
!! Quelle: erosion_huelle.f95
subroutine erosion_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,npla,ntra
   real tiefes,flaes,raus
   real dsedHs !! Sohlhöhenänderung im aktuellen Zeitschritt
   iglob = (i+meinrang*part) ! i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   kontroll = iglob == kontrollknoten
   npla = (i-1)*number_plankt_vari ! Ort im Feld der transportierten planktischen Variablen
   ntra = (i-1)*number_trans_quant
   tflie = real(deltat)/86400
   tiefes = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   if (tiefes <= min_tief)tiefes = min_tief ! minimale Wassertiefe erhalten
   raus = strickler( zone(point_zone(iglob))%reib , tiefes )
   !      SUBROUTINE erosion_kern(tflie,TIEFEs,RAUs,VMITTs        &
   !                           ,SSeross,sss,ssalgs,dsedHs       &
   !                       ,tauscs,M_eross,n_eross,sedrohs  &
   !                           ,kontroll,jjj)
   call erosion_kern( &
                     tflie                    &
                     ,TIEFEs                  &
                     ,RAUs                    &
                     ,rb_hydraul_p(1+(i-1)*number_rb_hydraul)                  &
                     ,transfer_quantity_p(12+ntra)                 &
                     ,planktonic_variable_p(53+npla)                     &
                     ,planktonic_variable_p(52+npla)                  &
                     ,transfer_quantity_p(68+ntra)         &
                     ,zone(point_zone(iglob))%erosi%tau_krit                   &
                     ,zone(point_zone(iglob))%erosi%M_eros                 &
                     ,zone(point_zone(iglob))%erosi%n_eros                 &
                     ,zone(point_zone(iglob))%erosi%sed_roh                 &
                     ,kontroll                &
                     ,iglob )
   return
end subroutine erosion_huelle
