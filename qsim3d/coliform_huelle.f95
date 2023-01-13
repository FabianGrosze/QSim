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

subroutine coliform_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,nk
   real, dimension(100) :: ecoli, coliL
   if (RateCd <= 0.0) then
      if (kontroll)print*,'coliform_huelle no simulation without parameters'
      return
   end if
   iglob = (i+meinrang*part)
   nk = (i-1)*number_plankt_vari
   kontroll = iglob == kontrollknoten
   if (kontroll)print*,'coliform_huelle... start: iglob,i,nk,meinrang = ',iglob,i,nk,meinrang
   tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
   rau(1:2) = strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
   vmitt(1:2) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag; randbedingungen.h
   vabfl(1:2) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   elen(1:2) = 1    ! Elementlänge (nicht verwendet)
   FLAE(1:2) = tiefe(1)*500.0 !! Breite konstant 500 m ; wird nur für Linienquelle verwendet, die in 3D nicht existiert. wie oxygen_huelle.f95
   flag(1:2) = 0    ! keine Einleitungen
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim3D) in real Tage (QSim)
   schwi(1:2) = schwi_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer)    ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
   ss(1:2) = planktonic_variable_p(53+nk) ! ORG. UND ANORG. SCHWEBSTOFFE(OHNE ALGEN UND ZOOPLANKTER) schweb()
   zooind(1:2) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien in Ind/l
   !GRote jetzt direkt aus QSimDatenfelder GRote=transfer_parameter_p(67) ! Gewicht einer Rotatorie µg  | Aparam.txt
   CHLA(1:2) = planktonic_variable_p(11+nk)  ! Chlorophyl-A
   tempw(1:2) = planktonic_variable_p(1+nk)    ! Wasser-Temperatur
   jiein(1:2) = 0       ! keine Punkt-Einleitungen
   ecoli(1:2) = 0.0     ! keine Einleitung
   qeinl(1:2) = 0.0      ! kein Einleitung (Abfluss)
   coliL(1:2) = 0.0      ! kein Linienquelle
   qeinlL(1:2) = 0.0      ! für Linienquelle; nicht verwendet
   anze = 1               ! Anzahl der Profile im aktuellen Strang
   iorLa(1:2) = 0         ! zur Berücksichtigung der Linienquelle; nicht verwendet
   iorLe(1:2) = 0         ! zur Berücksichtigung der Linienquelle; nicht verwendet
   ieinLs(1:2) = 0        ! keine Linienquellen
   ilbuhn = 0           ! keine Buhnen
   coli(1:2) = planktonic_variable_p(61+(i-1)*number_plankt_vari) ! Fäkalcoliforme Bakterien
   DOSCF(1:2) = planktonic_variable_p(70+(i-1)*number_plankt_vari) !
   EXTKS(1,1:2) = zone(point_zone(iglob))%seditemp%extiks ! Extinktionskoeffizient für PARS ((nicht mehr)nur bei Temperaturmodellierung erforderlich!)       mstr
   ! RateCd ! aus APARAM.txt
   ! etaCd  ! aus APARAM.txt
   ! RateCI ! aus APARAM.txt
   ! xnueC  ! aus APARAM.txt
   ! RateCG ! aus APARAM.txt
   ! RateCS ! aus APARAM.txt
   !----------------------------------------------------------------------------------
   
   call COLIFORM(tiefe,rau,vmitt,vabfl,elen,flae,flag,tflie,schwi,ss,zooind,GROT,Chla,tempw,jiein,ecoli  &
                 ,qeinl,coliL,qeinlL,anze,iorLa,iorLe,ieinLs,ilbuhn,coli,DOSCF,extkS,mstr                &
                 ,ratecd,etacd,rateci,xnuec,ratecg,ratecs                                                &
                 ,kontroll, iglob)
   !----------------------------------------------------------------------------------
   
   
   planktonic_variable_p(61+(i-1)*number_plankt_vari) = coli(1)  ! Fäkalcoliforme Bakterien
   planktonic_variable_p(70+(i-1)*number_plankt_vari) = DOSCF(1) !
   if (kontroll)print*,'coliform_huelle ende: coli = ',coli(1)
   return
end subroutine coliform_huelle
