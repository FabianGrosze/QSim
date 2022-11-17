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

subroutine mphyt_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,nk
   !real :: pflmin,pflmax
   !real :: sa, su
   
   
   ! macrophytes are currently turned off.
   ! Only the return values of `subroutine mphyt` will be given values here
   ! to avoid potential conflicts with other modules that may use these variables
   benthic_distribution_p(3+(i-1)*number_benth_distr) = 0.0 ! pfl
   zone(point_zone(iglob))%macrodicht%pflmin          = 0.0 ! pflmin
   zone(point_zone(iglob))%macrodicht%pflmax          = 0.0 ! pflmax
   transfer_quantity_p(30+(i-1)*number_trans_quant)   = 0.0 ! po2p
   transfer_quantity_p(31+(i-1)*number_trans_quant)   = 0.0 ! po2r
   
   
   
   ! iglob = (i+meinrang*part)
   ! nk = (i-1)*number_plankt_vari
   ! kontroll = iglob == kontrollknoten
   ! tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe
   ! tempw(1:2) = planktonic_variable_p( 1+nk)  ! Wassertemperatur
   ! anze = 1            ! Anzahl der Profile im aktuellen Strang
   ! po2p(1:2) = transfer_quantity_p(30+(i-1)*number_trans_quant) ! Sauerstoffproduktion durch Makrophyten in mgO2/l je Zeitschritt
   ! po2r(1:2) = transfer_quantity_p(31+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Makrophyten in mgO2/l je Zeitschritt
   ! pfldalg(1:2) = 0.0 ! unbenutzte Variable
   ! tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim-3D) in real Tage (QSim-1D)
   ! itags = tag             ! Tag im Monat  module::modell zeitsekunde()
   ! monats = monat          ! Monat im Jahr module::modell zeitsekunde()
   ! itstart = zone(point_zone(iglob))%macrophyt%starttag
   ! mstart = zone(point_zone(iglob))%macrophyt%startmonat
   ! itmax = zone(point_zone(iglob))%macrophyt%maxtag
   ! mmax = zone(point_zone(iglob))%macrophyt%maxmonat
   ! itend = zone(point_zone(iglob))%macrophyt%endtag
   ! mend = zone(point_zone(iglob))%macrophyt%endmonat
   ! schwi(1:2) = transfer_quantity_p(64+(i-1)*number_trans_quant)      ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
   ! pflmin = zone(point_zone(iglob))%macrodicht%pflmin ! Minimale Dichte der Makrophyten im Winter
   ! pflmax = zone(point_zone(iglob))%macrodicht%pflmax ! Maximale Dichte der Makrophyten im Sommer
   ! pfl(1:2) = benthic_distribution_p(3+(i-1)*number_benth_distr) ! Trockengewicht Wasserpflanzen in g/m²
   ! sa = sonnenaufgang
   ! su = sonnenuntergang
   ! ! module_QSimDatenfelder.f95:    integer , parameter :: ilang=1
   ! extk(1:2) = transfer_quantity_p(54+(i-1)*number_trans_quant) ! mittlerer Extinktionskoeffizient
   ! mstr = 1        ! Strangzähler | nur ein Profil in einem Strang
   ! ifehl = 0  ! if ISNAN(tempmt)(zwischenwert Wassertemperatur) > ifehl=24
   ! ifhStr = 0 ! Strangnummer in dem der Fehler auftrat
   ! if (kontroll)print*,'vor mphyt() pfl,po2p,po2r = ',pfl(1),po2p(1),po2r(1)
   ! !----------------------------------------------------------------------------------
   ! call mphyt(tiefe,tempw,anze,po2p,po2r,pfldalg,tflie               &
              ! ,itags,monats,itstart,mstart,itmax,mmax,itend,mend,schwi          &
              ! ,pflmin,pflmax,pfl,sa,su,ilang,extk,mstr,ifehl,ifhstr             &
              ! ,kontroll ,iglob ) !!wy
   ! !----------------------------------------------------------------------------------
   ! if (kontroll)print*,'nach mphyt() pfl,po2p,po2r = ',pfl(1),po2p(1),po2r(1)
   ! benthic_distribution_p(3+(i-1)*number_benth_distr) = pfl(1) ! Trockengewicht Wasserpflanzen in g/m²
   ! transfer_quantity_p(30+(i-1)*number_trans_quant) = po2p(1)  ! Sauerstoffproduktion durch Makrophyten in mgO2/l je Zeitschritt
   ! transfer_quantity_p(31+(i-1)*number_trans_quant) = po2r(1)  ! Sauerstoffverbrauch durch Makrophyten in mgO2/l je Zeitschritt
   return
end subroutine mphyt_huelle
