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

subroutine albenth_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   
   integer :: i,nk
   
   ! benthic algae are currently turned off.
   ! Only the return values of `subroutine albenth` will be given values here
   ! to avoid potential conflicts with other modules that may use these variables
   benthic_distribution_p(13+(i-1)*number_benth_distr) = 0.0 ! albewg
   benthic_distribution_p(14+(i-1)*number_benth_distr) = 0.0 ! albewk
   benthic_distribution_p(11+(i-1)*number_benth_distr) = 0.0 ! alberg
   benthic_distribution_p(12+(i-1)*number_benth_distr) = 0.0 ! alberk
   benthic_distribution_p(10+(i-1)*number_benth_distr) = 0.0 ! cmatgr
   benthic_distribution_p( 9+(i-1)*number_benth_distr) = 0.0 ! cmatki
   
   
   
   ! iglob = (i+meinrang*part)
   ! nk = (i-1)*number_plankt_vari
   ! kontroll = iglob == kontrollknoten
   
   ! if (kontroll)print*,'albenth_huelle  iglob,i,nk,meinrang = ',iglob,i,nk,meinrang
   
   ! schwi(1:2) = schwi_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer)    ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
   ! tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim3D) in real Tage (QSim)
   ! tempw(1:2) = planktonic_variable_p(1+nk)    ! Wasser-Temperatur
   ! tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
   ! vmitt(1:2) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag; randbedingungen.h
   ! vno3(1:2) = planktonic_variable_p( 5+nk)  ! nitrat (unbenutzt)
   ! vnh4(1:2) = planktonic_variable_p( 3+nk)  ! ammonium (unbenutzt)
   ! gelp(1:2) = planktonic_variable_p( 6+nk)  ! gelöster ortho-Phosphat-Phosphor tiefengemittelt
   ! albewg(1:2) = benthic_distribution_p(13+(i-1)*number_benth_distr) ! Wachstum benthischer gruen-Algen
   ! alberg(1:2) = benthic_distribution_p(11+(i-1)*number_benth_distr) ! Respiration benthischer gruen-Algen
   ! elen(1:2) = 1    ! Elementlänge (nicht verwendet)
   ! flae(1:2) = tiefe(1)*500.0 !! breite konstant 500 m ; wird nur für linienquelle verwendet, die in 3d nicht existiert. wie oxygen_huelle.f95
   ! ior = 1             ! Laufindex
   ! anze = 1               ! Anzahl der Profile im aktuellen Strang
   ! ! aggmax ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
   ! ! agksn ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
   ! ! agksp ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
   ! si(1) = planktonic_variable_p( 7+nk)  ! silikat-Silizium-Konzentration (tiefengemittelt)
   ! ! akksn ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
   ! ! akksp ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
   ! ! akkssi ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
   ! ! akgmax ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
   ! albewk(1:2) = benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
   ! alberk(1:2) = benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
   ! abegm2(1:2) = benthic_distribution_p(72+(i-1)*number_benth_distr)
   ! abekm2(1:2) = benthic_distribution_p(73+(i-1)*number_benth_distr)
   ! vabfl(1:2) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   ! cmatgr(1:2) = benthic_distribution_p(10+(i-1)*number_benth_distr) ! Abspülung benthischer gruen-Algen
   ! cmatki(1:2) = benthic_distribution_p(9+(i-1)*number_benth_distr) ! Abspülung benthischer kiesel-Algen
   ! ! akchl ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
   ! ! agchl ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
   ! extk(1:2) = transfer_quantity_p(54+(i-1)*number_trans_quant) ! mittlerer Extinktionskoeffizient
   ! ! module_QSimDatenfelder.f95:    integer , parameter :: ilang=1
   ! mstr = 1        ! Strangzähler | nur ein Profil in einem Strang
   ! !----------------------------------------------------------------------------------
   ! call albenth(schwi,tflie,tempw,tiefe,vmitt,vno3,vnh4,gelp,albewg,alberg,elen,flae,ior,anze         &
                ! ,aggmax,agksn,agksp,si,akksn,akksp,akkssi,akgmax,albewk,alberk,abegm2,abekm2           &
                ! ,vabfl,cmatgr,cmatki,akchl,agchl,extk,ilang,mstr                                       &
                ! ,kontroll ,iglob )
   ! !----------------------------------------------------------------------------------
   ! ! albenth():               benthische Verteilungen
   ! !      cmatki(ior) = albewk(ior)      9   Abspülung benthischer kiesel-Algen ??
   ! benthic_distribution_p(9+(i-1)*number_benth_distr) = cmatki(1)
   ! !      cmatgr(ior) = albewg(ior)      10   Abspülung benthischer gruen-Algen ??
   ! benthic_distribution_p(10+(i-1)*number_benth_distr) = cmatgr(1)
   ! !      alberg(ior) = alberg(ior)/tiefe(ior)   11    Respiration benthischer gruen-Algen    mgBio/l je Zeitschritt
   ! benthic_distribution_p(11+(i-1)*number_benth_distr) = alberg(1)
   ! !      alberk(ior) = alberk(ior)/tiefe(ior)    12   Respiration benthischer kiesel-Algen    mgBio/l je Zeitschritt
   ! benthic_distribution_p(12+(i-1)*number_benth_distr) = alberk(1)
   ! !      albewg(ior) = albewg(ior)/tiefe(ior)   13     Wachstum benthischer gruen-Algen    mgBio/l je Zeitschritt
   ! benthic_distribution_p(13+(i-1)*number_benth_distr) = albewg(1)
   ! !      albewk(ior) = albewk(ior)/tiefe(ior)   14    Wachstum benthischer kiesel-Algen    mgBio/l je Zeitschritt
   ! benthic_distribution_p(14+(i-1)*number_benth_distr) = albewk(1)
   ! !      abegm2(ior) = abegrt-alberg(ior)-(cmatgr(ior)*tiefe(ior))
   ! benthic_distribution_p(72+(i-1)*number_benth_distr) = abegm2(1) ! Biomasse benthischer Grünalgen
   ! !      abekm2(ior) = abekit-alberk(ior)-(cmatki(ior)*tiefe(ior))
   ! benthic_distribution_p(73+(i-1)*number_benth_distr) = abekm2(1) ! Biomasse benthischer Kieselalgen
   ! !nicht hier ...
   ! ! 20    abeowg    Sauerstoffproduktion (Wachstum) benthischer Grünalgen    mgO/l je Zeitschritt
   ! ! wird in oxygen.f90 berechnet:      abeowg(ior) = albewg(ior)*falgog
   ! ! 21    abeorg    Sauerstoffverbrauch (Respiration) benthischer Grünalgen    mgO/l je Zeitschritt
   ! ! wird in oxygen.f90 berechnet:      abeorg(ior) = alberg(ior)*opgrmix
   ! ! 22    abeowk    Sauerstoffproduktion (Wachstum) benthischer Kieselalgen    mgO/l je Zeitschritt
   ! ! wird in oxygen.f90 berechnet:      abeowk(ior) = albewk(ior)*falgok
   ! ! 23    abeork    Sauerstoffverbrauch (Respiration) benthischer Kieselalgen    mgO/l je Zeitschritt
   ! ! wird in oxygen.f90 berechnet:      abeork(ior) = alberk(ior)*opkimix
   return
end subroutine albenth_huelle
