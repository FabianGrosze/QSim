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
subroutine sedimentbelastung(ssalgs,                     &
                             hgszns, hglzns, znseds,     &
                             hgscads, hglcads, cadseds,  &
                             hgscus, hglcus, cuseds,     &
                             hgsnis, hglnis, niseds,     &
                             hgsass, hglass, asseds,     &
                             hgspbs, hglpbs, pbseds,     &
                             hgscrs, hglcrs, crseds,     &
                             hgsfes, hglfes, feseds,     &
                             hgshgs, hglhgs, hgseds,     &
                             hgsmns, hglmns, mnseds,     &
                             hgsus, hglus, useds,        &
                             anzzeits, sseross,          &
                             control,  jjj)
   implicit none
   
   integer                  :: anzzeits
   real                     :: ssalgs,sseross
   real                     :: hgszns,hglzns,znseds
   real                     :: hgscads,hglcads,cadseds
   real                     :: hgscus,hglcus,cuseds
   real                     :: hgsnis,hglnis,niseds
   real                     :: hgsass,hglass,asseds
   real                     :: hgspbs,hglpbs,pbseds
   real                     :: hgscrs,hglcrs,crseds
   real                     :: hgsfes,hglfes,feseds
   real                     :: hgshgs,hglhgs,hgseds
   real                     :: hgsmns,hglmns,mnseds
   real                     :: hgsus,hglus,useds
   logical, intent(in)      :: control  !< debugging
   integer, intent(in)      :: jjj       !< debugging
   
   ! counting timesteps without erosion
   if (sseross <= 0.0)anzzeits = anzzeits+1
   
   if (anzzeits <= 0) then ! no deposition yet, sediment equals suspension
      znseds = 1000 * (hgszns-hglzns)/ssalgs
      cadseds = 1000*(hgscads-hglcads)/ssalgs
      cuseds = 1000 * (hgscus-hglcus)/ssalgs
      niseds = 1000 * (hgsnis-hglnis)/ssalgs
      asseds = 1000 * (hgsass-hglass)/ssalgs
      pbseds = 1000 * (hgspbs-hglpbs)/ssalgs
      crseds = 1000 * (hgscrs-hglcrs)/ssalgs
      feseds = 1000 * (hgsfes-hglfes)/ssalgs
      hgseds = 1000 * (hgshgs-hglhgs)/ssalgs
      mnseds = 1000 * (hgsmns-hglmns)/ssalgs
      useds = 1000 * (hgsus-hglus)  /ssalgs
   
   else
      ! compute sedimentcontent from median of water content in erosionfree timesteps
      znseds = ( znseds*(anzzeits-1)+(1000 * (hgszns-hglzns)/ssalgs) )/real(anzzeits)
      cadseds = (cadseds*(anzzeits-1)+(1000*(hgscads-hglcads)/ssalgs) )/real(anzzeits)
      cuseds = ( cuseds*(anzzeits-1)+(1000 * (hgscus-hglcus)/ssalgs) )/real(anzzeits)
      niseds = ( niseds*(anzzeits-1)+(1000 * (hgsnis-hglnis)/ssalgs) )/real(anzzeits)
      asseds = ( asseds*(anzzeits-1)+(1000 * (hgsass-hglass)/ssalgs) )/real(anzzeits)
      pbseds = ( pbseds*(anzzeits-1)+(1000 * (hgspbs-hglpbs)/ssalgs) )/real(anzzeits)
      crseds = ( crseds*(anzzeits-1)+(1000 * (hgscrs-hglcrs)/ssalgs) )/real(anzzeits)
      feseds = ( feseds*(anzzeits-1)+(1000 * (hgsfes-hglfes)/ssalgs) )/real(anzzeits)
      hgseds = ( hgseds*(anzzeits-1)+(1000 * (hgshgs-hglhgs)/ssalgs) )/real(anzzeits)
      mnseds = ( mnseds*(anzzeits-1)+(1000 * (hgsmns-hglmns)/ssalgs) )/real(anzzeits)
      useds = (  useds*(anzzeits-1)+(1000 * (hgsus-hglus)  /ssalgs) )/real(anzzeits)
   endif
   return
end subroutine sedimentbelastung
