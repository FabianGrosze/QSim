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
   real                     :: SSalgs,SSeross, schweba
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
   
   schweba=SSalgs*1000.0 ! Schwebstoffgehalt in mueg/l
   
   ! counting timesteps without erosion
   if (SSeross <= 0.0)anzZeits = anzZeits+1
   if (anzZeits <= 0) then ! no deposition yet, sediment equals suspension
      !print*,'Sedimentbelastung timecounter anzZeits .le. zero'
      ZnSeds = (hgsZns-hglZns)/schweba
      CadSeds =(hgsCads-hglCads)/schweba
      CuSeds = (hgsCus-hglCus)/schweba
      NiSeds = (hgsNis-hglNis)/schweba
      AsSeds = (hgsAss-hglAss)/schweba
      PbSeds = (hgsPbs-hglPbs)/schweba
      CrSeds = (hgsCrs-hglCrs)/schweba
      FeSeds = (hgsFes-hglFes)/schweba
      HgSeds = (hgsHgs-hglHgs)/schweba
      MnSeds = (hgsMns-hglMns)/schweba
      USeds = (hgsUs-hglUs)  /schweba
      !stop
   else
      ! compute sedimentcontent from median of water content in erosionfree timesteps
      ZnSeds = ( ZnSeds*(anzZeits-1)+((hgsZns-hglZns)/schweba) )/real(anzZeits)
      CadSeds = (CadSeds*(anzZeits-1)+((hgsCads-hglCads)/schweba) )/real(anzZeits)
      CuSeds = ( CuSeds*(anzZeits-1)+((hgsCus-hglCus)/schweba) )/real(anzZeits)
      NiSeds = ( NiSeds*(anzZeits-1)+((hgsNis-hglNis)/schweba) )/real(anzZeits)
      AsSeds = ( AsSeds*(anzZeits-1)+((hgsAss-hglAss)/schweba) )/real(anzZeits)
      PbSeds = ( PbSeds*(anzZeits-1)+((hgsPbs-hglPbs)/schweba) )/real(anzZeits)
      CrSeds = ( CrSeds*(anzZeits-1)+((hgsCrs-hglCrs)/schweba) )/real(anzZeits)
      FeSeds = ( FeSeds*(anzZeits-1)+((hgsFes-hglFes)/schweba) )/real(anzZeits)
      HgSeds = ( HgSeds*(anzZeits-1)+((hgsHgs-hglHgs)/schweba) )/real(anzZeits)
      MnSeds = ( MnSeds*(anzZeits-1)+((hgsMns-hglMns)/schweba) )/real(anzZeits)
      USeds = (  USeds*(anzZeits-1)+((hgsUs-hglUs)  /schweba) )/real(anzZeits)
   endif
   return
end subroutine sedimentbelastung
