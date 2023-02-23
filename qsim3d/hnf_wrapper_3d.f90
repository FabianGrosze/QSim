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
!> Wrapper to call metabolism function hnf
!!
!! This module is currently turned off
subroutine hnf_wrapper_3d(i)
   use modell
   use QSimDatenfelder
   ! use module_metabolism, only: hnf
   implicit none
   
   integer, intent(in) :: i
   iglob = i + meinrang * part
   
   ! This module is currently turned off
   
   ! convert timestep from seconds (QSim3D) into days (QSim)
   ! tflie = real(deltat)/86400. 
   ! 
   ! if (kontroll) then
   !    print*, "before hnf:"
   !    print*, "   CHNF = ", planktonic_variable_p(48+(i-1)*number_plankt_vari)
   !    print*, ""
   ! endif
   ! 
   ! 
   ! call hnf(
   !    planktonic_variable_p(48+(i-1)*number_plankt_vari), ! chnf
   !    planktonic_variable_p(42+(i-1)*number_plankt_vari), ! bac
   !    planktonic_variable_p( 2+(i-1)*number_plankt_vari), ! vo2
   !    planktonic_variable_p( 1+(i-1)*number_plankt_vari), ! tempw
   !    transfer_quantity_p(96+(i-1)*number_trans_quant),   ! drhnf
   !    tflie,                                              ! tflie
   !    transfer_quantity_p(11+(i-1)*number_trans_quant),   ! hnfbac
   !    hnfupa(1),                                          ! hnfupa (from module qsimdatenfelder)
   !    hnfrea(1),                                          ! hnfrea (from module qsimdatenfelder)
   !    hnfexa(1),                                          ! hnfexa (from module qsimdatenfelder)
   !    hnfmoa(1),                                          ! hnfmoa (from module qsimdatenfelder)
   !    hnfmua(1),                                          ! hnfmua (from module qsimdatenfelder)
   !    transfer_quantity_p(44+(i-1)*number_trans_quant),   ! ro2hnf
   !    transfer_quantity_p(10+(i-1)*number_trans_quant),   ! bsbhnf
   !    kontroll, iglob)         
   !               
   ! if (kontroll) then
   !    print*, "after hnf:"
   !    print*, "   CHNF = ", planktonic_variable_p(48+(i-1)*number_plankt_vari)
   !    print*, "   HNFmua = ", HNFmua(1)
   !    print*, "   HNFrea = ", HNFrea(1)
   !    print*, "   HNFupa = ", HNFupa(1)
   !    print*, "   HNFmoa = ", HNFmoa(1)
   !    print*, "   HNFexa = ", HNFexa(1)
   !    print*, ""
   !    
   ! endif
   
   return
end subroutine hnf_wrapper_3d
