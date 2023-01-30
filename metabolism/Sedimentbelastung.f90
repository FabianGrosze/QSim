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

!> Berechnung der Sedimentbelastung
!!
!! Sedimentbelastung wird aus der Belastung des suspendierten Sediments berrechnet.
!! Dazu wird über die erosionsfreien Zeitschritte gemittelt.
!! Nach dem Ende der Erosion wird die Mittelung fortgesetzt.
!!
!! file: sedimentbelastung.f90 zurück: \ref lnk_schwermetalle
subroutine sedimentbelastung(SSalgs,                     &
                             hgsZns, hglZns, ZnSeds,     &
                             hgsCads, hglCads, CadSeds,  &
                             hgsCus, hglCus, CuSeds,     &
                             hgsNis, hglNis, NiSeds,     &
                             hgsAss, hglAss, AsSeds,     &
                             hgsPbs, hglPbs, PbSeds,     &
                             hgsCrs, hglCrs, CrSeds,     &
                             hgsFes, hglFes, FeSeds,     &
                             hgsHgs, hglHgs, HgSeds,     &
                             hgsMns, hglMns, MnSeds,     &
                             hgsUs, hglUs, USeds,        &
                             anzZeits, SSeross,          &
                             kontroll,  jjj)
   implicit none
   
   integer                  :: anzZeits
   real                     :: SSalgs,SSeross
   real                     :: hgsZns,hglZns,ZnSeds
   real                     :: hgsCads,hglCads,CadSeds
   real                     :: hgsCus,hglCus,CuSeds
   real                     :: hgsNis,hglNis,NiSeds
   real                     :: hgsAss,hglAss,AsSeds
   real                     :: hgsPbs,hglPbs,PbSeds
   real                     :: hgsCrs,hglCrs,CrSeds
   real                     :: hgsFes,hglFes,FeSeds
   real                     :: hgsHgs,hglHgs,HgSeds
   real                     :: hgsMns,hglMns,MnSeds
   real                     :: hgsUs,hglUs,USeds
   logical, intent(in)      :: kontroll  !< debugging
   integer, intent(in)      :: jjj       !< debugging
   
   ! counting timesteps without erosion
   if (SSeross <= 0.0)anzZeits = anzZeits+1
   if (anzZeits <= 0) then ! no deposition yet, sediment equals suspension
      !print*,'Sedimentbelastung timecounter anzZeits .le. zero'
      ZnSeds = 1000 * (hgsZns-hglZns)/SSalgs
      CadSeds = 1000*(hgsCads-hglCads)/SSalgs
      CuSeds = 1000 * (hgsCus-hglCus)/SSalgs
      NiSeds = 1000 * (hgsNis-hglNis)/SSalgs
      AsSeds = 1000 * (hgsAss-hglAss)/SSalgs
      PbSeds = 1000 * (hgsPbs-hglPbs)/SSalgs
      CrSeds = 1000 * (hgsCrs-hglCrs)/SSalgs
      FeSeds = 1000 * (hgsFes-hglFes)/SSalgs
      HgSeds = 1000 * (hgsHgs-hglHgs)/SSalgs
      MnSeds = 1000 * (hgsMns-hglMns)/SSalgs
      USeds = 1000 * (hgsUs-hglUs)  /SSalgs
   
   else
      ! compute sedimentcontent from median of water content in erosionfree timesteps
      ZnSeds = ( ZnSeds*(anzZeits-1)+(1000 * (hgsZns-hglZns)/SSalgs) )/real(anzZeits)
      CadSeds = (CadSeds*(anzZeits-1)+(1000*(hgsCads-hglCads)/SSalgs) )/real(anzZeits)
      CuSeds = ( CuSeds*(anzZeits-1)+(1000 * (hgsCus-hglCus)/SSalgs) )/real(anzZeits)
      NiSeds = ( NiSeds*(anzZeits-1)+(1000 * (hgsNis-hglNis)/SSalgs) )/real(anzZeits)
      AsSeds = ( AsSeds*(anzZeits-1)+(1000 * (hgsAss-hglAss)/SSalgs) )/real(anzZeits)
      PbSeds = ( PbSeds*(anzZeits-1)+(1000 * (hgsPbs-hglPbs)/SSalgs) )/real(anzZeits)
      CrSeds = ( CrSeds*(anzZeits-1)+(1000 * (hgsCrs-hglCrs)/SSalgs) )/real(anzZeits)
      FeSeds = ( FeSeds*(anzZeits-1)+(1000 * (hgsFes-hglFes)/SSalgs) )/real(anzZeits)
      HgSeds = ( HgSeds*(anzZeits-1)+(1000 * (hgsHgs-hglHgs)/SSalgs) )/real(anzZeits)
      MnSeds = ( MnSeds*(anzZeits-1)+(1000 * (hgsMns-hglMns)/SSalgs) )/real(anzZeits)
      USeds = (  USeds*(anzZeits-1)+(1000 * (hgsUs-hglUs)  /SSalgs) )/real(anzZeits)
   endif
   return
end subroutine sedimentbelastung
