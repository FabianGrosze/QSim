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

subroutine oxygen_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,j,nk,i2,k
   !     i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   iglob = (i+meinrang*part) ! globale Knotennummer
   i2 = zone(point_zone(iglob))%wettstat%wetterstations_nummer !! ist parallel !!!
   nk = (i-1)*number_plankt_vari ! Ort im Feld der transporterten, planktischen Variablen
   kontroll = (iglob == kontrollknoten)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Daten-übergabe:
   vo2(1) = planktonic_variable_p( 2+nk) ! Sauerstoffgehalt tiefengemittelt
   vo2(2) = vo2(1)
   tempw(1) = planktonic_variable_p( 1+nk)  ! Wassertemperatur
   tempw(2) = tempw(1)
   vmitt(1) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag
   vmitt(2) = vmitt(1)
   tiefe(1) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe
   tiefe(2) = tiefe(1)
   rhyd(1) = tiefe(1) ! hydraulischer Radius | sinnvollste Annahme im mehrdimensionalen
   rhyd(2) = rhyd(1)
   rau(1:2) = strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
   if (kontroll)print*,'oxygen_huelle: kst, ust = ', rau(1)  &
       , abs(VMITT(1)) *  (9.81**0.5) / ( (tiefe(1)**0.166667)*rau(1) )
   !FN = 1./RAU(ior)
   !G = 9.81
   !UST = ((FN*G**0.5)/tiefe(ior)**0.166667)*abs(VMITT(ior))
   FLAE(1) = tiefe(1)*500.0 !! Breite konstant 500 m ; wird in der Belüftungsformel verwendet,
   ! hat aber keine Entsprechung im Mehrdimensionalen, daher sinnvoller Wert fürs Ästuar
   FLAE(2) = FLAE(1)
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim-3D) in real Tage (QSim-1D)
   go2n(1) = transfer_quantity_p(32+(i-1)*number_trans_quant) ! FUER DIE STICKSTOFFOXYDATION VERBRAUCHTE SAUERSTOFFMENGE
   go2n(2) = go2n(1)
   dalgki(1) = transfer_quantity_p(20+(i-1)*number_trans_quant) ! Zuwachs Kiesel-Algen
   dalgki(2) = dalgki(1)
   dalggr(1) = transfer_quantity_p(21+(i-1)*number_trans_quant) ! Zuwachs Grün-Algen
   dalggr(2) = dalggr(1)
   dalgak(1) = transfer_quantity_p(23+(i-1)*number_trans_quant) ! Respiration Kiesel-Algen
   dalgak(2) = dalgak(1)
   dalgag(1) = transfer_quantity_p(24+(i-1)*number_trans_quant) ! Respiration Grün-Algen
   dalgag(2) = dalgag(1)
   akinh4(1) = transfer_quantity_p(33+(i-1)*number_trans_quant) ! Ammoniumaufnahme der kiesel-Algen
   akinh4(2) = akinh4(1)
   agrnh4(1) = transfer_quantity_p(34+(i-1)*number_trans_quant) ! Ammoniumaufnahme der gruen-Algen
   agrnh4(2) = agrnh4(1)
   akino3(1) = transfer_quantity_p(36+(i-1)*number_trans_quant) ! Nitrataufnahme der kiesel-Algen
   akino3(2) = akino3(1)
   agrno3(1) = transfer_quantity_p(37+(i-1)*number_trans_quant) ! Nitrataufnahme der gruen-Algen
   agrno3(2) = agrno3(1)
   bsbt(1) = transfer_quantity_p(1+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Kohlenstoffabbau
   bsbt(2) = bsbt(1)
   hJO2(1,1) = benthic_distribution_p(8+(i-1)*number_benth_distr) ! Sauerstoffzehrung des Sediments gO2/m² und Zeitschritt
   hJO2(1,2) = hJO2(1,1)
   flag(1) = 0         ! keine Einleitungen
   flag(2) = flag(1)
   elen(1) = 1         ! Elementlänge (nicht verwendet)
   elen(2) = elen(1)
   ior = 1             ! Laufindex im Strang (T-QSim verwendet nur erstes Profil(Punkt) im Strang)
   anze = 1            ! Anzahl der Profile im aktuellen Strang
   dzres1(1) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
   dzres1(2) = dzres1(1)
   dzres2(1) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten
   dzres2(2) = dzres2(1)
   hSchlr(1,1) = benthic_distribution_p(16+(i-1)*number_benth_distr) ! Sauerstoffzehrung durch das Sediments, Ausgabe in mgO2/(l*h)
   hSchlr(1,2) = hSchlr(1,1)
   eo2(1) = 0.0         ! Einleite-Konzentration (keine Einleitungen - nicht verwendet)
   qeinl(1) = 0.0       ! kein Abfluss Einleitung
   vabfl(1) = 2.5 !! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   vabfl(2) = vabfl(1)
   PO2P(1) = transfer_quantity_p(30+(i-1)*number_trans_quant) ! Sauerstoffproduktion durch Makrophyten in mgO2/l je Zeitschritt
   PO2P(2) = PO2P(1)
   PO2R(1) = transfer_quantity_p(31+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Makrophyten in mgO2/l je Zeitschritt
   PO2R(2) = PO2R(1)
   so2ein(1) = benthic_distribution_p(17+(i-1)*number_benth_distr) ! Sauerstoffeintrag aus der Luft ? , Ausgabe in mgO2/(l*h)
   so2ein(2) = so2ein(1)
   dO2o2D(1) = benthic_distribution_p(18+(i-1)*number_benth_distr) ! Beiwert Oberflächenbelüftung ?  ersetzt bbei2D
   dO2o2D(2) = dO2o2D(1)
   salgo(1) = transfer_quantity_p(39+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Makrophyten in mgO2/l je Zeitschritt
   salgo(2) = salgo(1)
   dalgo(1) = transfer_quantity_p(40+(i-1)*number_trans_quant) ! Sauerstoffproduktion der Gruen-, Kiesel-, und Blaualgen
   dalgo(2) = dalgo(1)
   dalgao(1) = transfer_quantity_p(41+(i-1)*number_trans_quant)! Respiration (Sauerstoffverbrauch) der Gruen-, Kiesel-, und Blaualgen
   dalgao(2) = dalgao(1)
   o2ein1(1) = benthic_distribution_p(19+(i-1)*number_benth_distr) ! Sauerstoffeintrag aus der Luft ? , Ausgabe in mgO2/(l*h)
   o2ein1(2) = o2ein1(1)
   jiein(1) = 0        ! keine Punkt-Einleitungen
   ! \ref globaleParameter direkt aus QSimDatenfelder opgrmi = transfer_parameter_p(16) ! Min. O2-Prod. Grünalgen
   ! \ref globaleParameter direkt aus QSimDatenfelder opgrma = transfer_parameter_p(17) ! Max. O2-Prod. Grünalgen
   ! \ref globaleParameter direkt aus QSimDatenfelder opkimi = transfer_parameter_p(40) ! Min. O2-Prod. Kieselalgen
   ! \ref globaleParameter direkt aus QSimDatenfelder opkima = transfer_parameter_p(41) ! Max. O2-Prod. Kieselalgen
   albewg(1) = benthic_distribution_p(13+(i-1)*number_benth_distr) ! Wachstum benthischer gruen-Algen
   albewg(2) = albewg(1)
   alberg(1) = benthic_distribution_p(11+(i-1)*number_benth_distr) ! Respiration benthischer gruen-Algen
   alberg(2) = alberg(1)
   abeowg(1) = benthic_distribution_p(20+(i-1)*number_benth_distr) ! Sauerstoffproduktion benthischer Grünalge
   abeowg(2) = abeowg(1)
   abeorg(1) = benthic_distribution_p(21+(i-1)*number_benth_distr) ! Sauerstoffverbrauch benthischer Grünalgen
   abeorg(2) = abeorg(1)
   ! \ref globaleParameter direkt aus QSimDatenfelder opblmi = transfer_parameter_p(60) ! Min. O2-Prod. Blaualgen
   ! \ref globaleParameter direkt aus QSimDatenfelder opblma = transfer_parameter_p(61) ! Max. O2-Prod. Blaualgen
   ablnh4(1) = transfer_quantity_p(35+(i-1)*number_trans_quant) ! Ammoniumaufnahme der blau-Algen, tiefengem.
   ablnh4(2) = ablnh4(1)
   ablno3(1) = transfer_quantity_p(38+(i-1)*number_trans_quant) ! Nitrataufnahme der blau-Algen, tiefengem.
   ablno3(2) = ablno3(1)
   dalgbl(1) = transfer_quantity_p(22+(i-1)*number_trans_quant) ! Zuwachs Blau-Algen
   dalgbl(2) = dalgbl(1)
   dalgab(1) = transfer_quantity_p(25+(i-1)*number_trans_quant) ! Respiration Blau-Algen
   dalgab(2) = dalgab(1)
   albewk(1) = benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
   albewk(2) = albewk(1)
   alberk(1) = benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
   alberk(2) = alberk(1)
   abeowk(1) = benthic_distribution_p(22+(i-1)*number_benth_distr) ! Sauerstoffproduktion benthischer Kieselalgen
   abeowk(2) = abeowk(1)
   abeork(1) = benthic_distribution_p(23+(i-1)*number_benth_distr) ! Sauerstoffverbrauch benthischer Kieselalgen
   abeork(2) = abeork(1)
   ro2dr(1) = benthic_distribution_p(24+(i-1)*number_benth_distr) ! Respiration Dreissena-Muscheln pro Zeitschritt in mgO2/l je Zeitschritt
   ro2dr(2) = ro2dr(1)
   wge(1) = wge_T(i2)        ! Windgeschwindigkeit  aus Wetterstationsdaten
   wge(2) = wge(1)
   idwe(1,1) = 1 ! Vorgaben so als ob nur eine Wetterstation an dem knoten existiert (i2 ist falsch)  ! Wetterstation Nr.
   idwe(1,2) = idwe(1,1)
   fkm (1) = 0.0  ! Flusskilometer (unbenutzt)
   uhrz = uhrzeit_stunde ! Uhrzeit module ::modell zeitsekunde()
   si(1) = planktonic_variable_p(7+nk) ! geloestes Silikat-Silizium, tiefengemittelt (unbenutzt)
   si(2) = si(1)
   gelp(1) = planktonic_variable_p(6+nk) ! geloester ortho-Phosphat-P, tiefengemittelt (unbenutzt)
   gelp(2) = gelp(1)
   ssalg(1) = planktonic_variable_p(52+(i-1)*number_plankt_vari) ! ??? (unbenutzt)
   ssalg(2) = ssalg(1)
   ir(1) = transfer_quantity_p(42+(i-1)*number_trans_quant) ! Ingestionsrate der Rotatorien in mg/(l*h) | konsum() (unbenutzt)
   ir(2) = ir(1)
   vNH4(1) = planktonic_variable_p( 3+nk)  ! ammonium (unbenutzt)
   vNH4(2) = vNH4(1)
   vno3(1) = planktonic_variable_p( 5+nk)  ! nitrat (unbenutzt)
   vno3(2) = vno3(1)
   bsbbet(1) = benthic_distribution_p(7+(i-1)*number_benth_distr) ! Sauerstoffverbrauch durch Organismen auf Makrophyten, Ausgabewert bsbtb ??? (unbenutzt)
   bsbbet(2) = bsbbet(1)
   zooro2(1) = transfer_quantity_p(43+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Zooplanktonrespiration (Rückgabewert)
   zooro2(2) = zooro2(1)
   rO2HNF(1) = transfer_quantity_p(44+(i-1)*number_trans_quant) ! Respiration HNF ???
   rO2HNF(2) = rO2HNF(1)
   ilbuhn = 0          ! keine Buhnen
   iwied = 0      ! unbenutzte Variable
   do j = 1,num_lev ! Sauerstoffgehalt tiefenaufgelöst
      vo2z(j,1) = plankt_vari_vert_p(j+(2-1)*num_lev+(i-1)*number_plankt_vari*num_lev)
      vo2z(j,2) = vo2z(j,1)
   end do ! alle j tiefenlevels
   susO2N(1) = 0.0     ! unbenutzte Variable
   susO2N(2) = susO2N(1)
   nkzs(1) = 1          ! nur eine Tiefenschicht
   nkzs(2) = nkzs(1)
   dH2D = 0.25 ! Dicke Tiefenschicht ??? Übergabe an Einleiter_Misch() | qsim.f90: dH2D = 0.25
   o2L(1) = 0.0     ! keine linienquelle
   qeinlL(1) = 0.0 ! keine linienquelle
   iorLa(1) = 0 ! keine linienquelle
   iorLe(1) = 0 ! keine linienquelle
   ieinLs(1) = 0 ! keine linienquelle
   !!<tr><td>     agnh4z,aknh4z,abnh4z   </td><td>  Ammoniumaufnahme der Algen gruen, kiesel und blau, tiefenaufgel.   </td><td> OXYGEN nur Rückgabewert   </td><td>   </td></tr>
   !!<tr><td>     agrnh4,akinh4,ablnh4   </td><td>  Ammoniumaufnahme der Algen gruen, kiesel und blau, tiefengem.   </td><td> OXYGEN nur Rückgabewert   </td><td>   </td></tr>
   !!<tr><td>     agno3z,akno3z,abno3z   </td><td>  Nitrataufnahme der Algen gruen, kiesel und blau, tiefenaufgel.   </td><td> OXYGEN nur Rückgabewert   </td><td>   </td></tr>
   do j = 1,num_lev_trans
      agnh4z(j,1) = trans_quant_vert_p(j+(10-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      agnh4z(j,2) = agnh4z(j,1) ! Ammoniumaufnahme Grün-Algen
      aknh4z(j,1) = trans_quant_vert_p(j+(9-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      aknh4z(j,2) = aknh4z(j,1) ! Ammoniumaufnahme Kiesel-Algen
      abnh4z(j,1) = trans_quant_vert_p(j+(11-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      abnh4z(j,2) = abnh4z(j,1) ! Ammoniumaufnahme Blau-Algen
      dalgkz(j,1) = trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      dalgkz(j,2) = dalgkz(j,1)
      dalgbz(j,1) = trans_quant_vert_p(j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      dalgbz(j,2) = dalgbz(j,1)
      dalggz(j,1) = trans_quant_vert_p(j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      dalggz(j,2) = dalggz(j,1)
      agno3z(j,1) = trans_quant_vert_p(j+(16-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      agno3z(j,2) = agno3z(j,1) ! Nitrataufnahme Grün-Algen
      akno3z(j,1) = trans_quant_vert_p(j+(15-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      akno3z(j,2) = akno3z(j,1) ! Nitrataufnahme Kiesel-Algen
      abno3z(j,1) = trans_quant_vert_p(j+(17-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      abno3z(j,2) = abno3z(j,1) ! Nitrataufnahme Blau-Algen
   end do ! alle j tiefenlevels
   do j = 1,num_lev_trans
      algakz(j,1) = trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      algakz(j,2) = algakz(j,1)
      algagz(j,1) = trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      algagz(j,2) = algagz(j,1)
      algabz(j,1) = trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      algabz(j,2) = algabz(j,1)
      vz1(j,1) = trans_quant_vert_p(j+(21-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! lokale Sauerstoffzehrung/produktion tiefenaufgelöst  !
      vz1(j,2) = vz1(j,1)
      tempwz(j,1) = plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari*num_lev) ! wassertemperatur tiefenaufgelöst
      tempwz(j,2) = tempwz(j,1)
   end do ! alle j tiefenlevels
   saett(1) = transfer_quantity_p(45+(i-1)*number_trans_quant) ! Sauerstoff Sättigungs-Konzentration in mgO2/l
   saett(2) = saett(1)
   mstr = 1            ! Strangzähler ein Strang mit nur einem Profil in Hüllroutine
   write(cpfad,'(A)')'cpfad unbenutzt in oxygen, funk.dat ausser Betrieb'
   ! Belüftungsparameter werden nicht mehr von funk.dat gelesen, sondern im Aufruf übergeben.
   ij = 0         ! zeitschritt im Tag
   ! itags=tag           ! direkt aus QSimDatenfelder
   ! monats=monat          ! direkt aus QSimDatenfelder
   dC_DenW(1:2) = transfer_quantity_p(90 +(i-1)*number_trans_quant)! dC_DenW(ior) = dNO3Den/0.93 ! C-Abbau durch Denitrifikation in der Wassersäule
   TOC_CSB = 3.1  !....Berechnung der "BSB-Komponenten" am oberen Rand   !     (auch bei Stundenwert-Generierung)!!!!!
   Wlage(1,1:2) = zone(point_zone(iglob))%wettstat%wetterstations_lage ! Höhenlage der zuständigen Wetterstation mü.NHN
   hWS(1,1:2) = rb_hydraul_p(3+(i-1)*number_rb_hydraul) ! Wasserspiegellage, von holen_trans() gesetzt
   etemp(1) = 0.0         ! Einleite-Temperatur (da keine Einleitungen - nicht verwendet)
   dH2De = 0.0   ! unbenutzt                                                  &
   ! Ansteuerung der Belüftungsformel:
   !! Erübrigt das Lesen von funk.dat
   !iphyw = 0
   !iphy = 0
   !zwgmes = 0.0
   ifehl = 0  ! if ISNAN(tempmt)(zwischenwert Wassertemperatur) > ifehl=24
   ifhStr = 0 ! Strangnummer in dem der Fehler auftrat
   ! azStrs - direkt aus QSimDatenfelder
   zooind(1) = planktonic_variable_p(50+nk)  ! Anzahl der Rotatorien
   zooind(2) = zooind(1)
   !     iphy = 1   ! neue Formel von mir mit Wind
   !     iphy = 2   ! neue Formel von mir ohne Wind
   !     iphy = 3   ! Formel von Wolf
   !     iphy = 4   ! Formel von Melching
   ! direkt aus EREIGG.txt gelesen
   if ((iphy < 1) .or. (iphy > 4)) then
      call qerror('iphy out of range 1...4 , aeration method, set in EREIGG.txt') ! Reaeration rate
   endif
   if (kontroll) print*,'oxygen (davor): vo2(1),vo2(2),i,FLAE(1),TEMPWz(1,1) = ',vo2(1),vo2(2),i, FLAE(1),TEMPWz(1,1)
   do k = 1,number_benth_distr
      if (isnan(benthic_distribution_p(k+(i-1)*number_benth_distr))) then
         print*,'vor oxygen: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
         if (meinrang == 0)print*,'benth_distr_name:',benth_distr_name(k)
      endif
   end do
   !qsim13.40 15okt18
   call oxygen(VO2,TEMPW,RAU,VMITT,TIEFE,rhyd,FLAE,TFLIE,go2n,dalgki,dalggr,dalgak,dalgag,akinh4   &
               ,agrnh4,akino3,agrno3,bsbt,hJO2,flag,elen,ior,anze,dzres1,dzres2,hschlr             &
               ,eo2,qeinl,vabfl,po2p,po2r,so2ein,dO2o2D,salgo,dalgo,dalgao,o2ein1,jiein            &
               ,opgrmi,opgrma,opkimi,opkima,albewg,alberg,abeowg,abeorg,opblmi,opblma,ablnh4       &
               ,ablno3,dalgbl,dalgab,albewk,alberk,abeowk,abeork,ro2dr,wge,IDWe,fkm,uhrz           &
               ,zooro2,rO2HNF,ilbuhn,iwied,vo2z,susO2N,nkzs,dH2D,o2L,qeinlL                        &
               ,iorLa,iorLe,ieinLs,agnh4z,aknh4z,abnh4z,dalgkz,dalgbz,dalggz,agno3z,akno3z         &
               ,abno3z,algakz,algagz,algabz,vz1,tempwz,saett,mstr,cpfad,ij,itags,monats            &
               ,dC_DenW,TOC_CSB,WLage,hWS,etemp,dH2De,ifehl,ifhStr,azStrs,zooind,GROT,iphy         &
               ,kontroll ,iglob )
   
   ! qsim13.301_28mae18
   !         call oxygen(VO2,TEMPW,RAU,VMITT,TIEFE,rhyd,FLAE,TFLIE,go2n,dalgki,dalggr,dalgak,dalgag,akinh4   &
   !                    ,agrnh4,akino3,agrno3,bsbt,hJO2,flag,elen,ior,anze,dzres1,dzres2,hschlr              &
   !                    ,eo2,qeinl,vabfl,po2p,po2r,so2ein,dO2o2D,salgo,dalgo,dalgao,o2ein1,jiein             &
   !                    ,opgrmi,opgrma,opkimi,opkima,albewg,alberg,abeowg,abeorg,opblmi,opblma,ablnh4        &
   !                    ,ablno3,dalgbl,dalgab,albewk,alberk,abeowk,abeork,ro2dr,wge,IDWe,fkm,uhrz,vnh4       &
   !                    ,vno3,bsbbet,zooro2,rO2HNF,ilbuhn,iwied,vo2z,susO2N,nkzs,dH2D,o2L,qeinlL             &
   !                    ,iorLa,iorLe,ieinLs,agnh4z,aknh4z,abnh4z,dalgkz,dalgbz,dalggz,agno3z,akno3z          &
   !                    ,abno3z,algakz,algagz,algabz,vz1,tempwz,saett,mstr,cpfad,ij,itags,monats             &
   !                    ,dC_DenW,TOC_CSB,WLage,hWS,etemp,dH2De,ifehl,ifhStr ,azStrs                          &
   !                    ,zooind,GROTe,iphy      ,kontroll ,iglob )
   !version 13_30
   !       call oxygen(VO2,TEMPW,RAU,VMITT,TIEFE,FLAE,TFLIE,go2n,dalgki,dalggr,dalgak,dalgag,akinh4    &
   !                    ,agrnh4,akino3,agrno3,bsbt,hJO2,flag,elen,ior,anze,dzres1,dzres2,hschlr         &
   !                    ,eo2,qeinl,vabfl,po2p,po2r,so2ein,bbei2D,salgo,dalgo,dalgao,o2ein1,jiein        &
   !                    ,opgrmi,opgrma,opkimi,opkima,albewg,alberg,abeowg,abeorg,opblmi,opblma,ablnh4   &
   !                    ,ablno3,dalgbl,dalgab,albewk,alberk,abeowk,abeork,ro2dr,wge,IDWe,fkm,uhrz  &
   !                    ,vnh4,vno3,bsbbet,zooro2,rO2HNF,ilbuhn,iwied,vo2z,susO2N,nkzs,dH2D  &
   !,o2L,qeinlL ,iorLa,iorLe,ieinLs       &
   !                    ,agnh4z,aknh4z,abnh4z,dalgkz,dalgbz,dalggz,agno3z,akno3z     &
   !                    ,abno3z,algakz,algagz,algabz,vz1,tempwz,saett,mstr,cpfad,ij,itags,monats        &
   !,dC_DenW,TOC_CSB,WLage,hWS        &
   !                    ,etemp,dH2De        &
   !,ifehl,ifhStr, 1, kontroll)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Daten-rückgabe:
   planktonic_variable_p(2+nk) = vo2(1) ! Sauerstoffgehalt tiefengemittelt
   do j = 1,num_lev ! Sauerstoffgehalt tiefenaufgelöst
      plankt_vari_vert_p(j+(2-1)*num_lev+(i-1)*number_plankt_vari*num_lev) = vo2z(j,1)
   end do ! alle j tiefenlevels
   benthic_distribution_p(16+(i-1)*number_benth_distr) = hSchlr(1,1) ! Sauerstoffzehrung durch das Sediments, Ausgabe in mgO2/(l*h)
   benthic_distribution_p(17+(i-1)*number_benth_distr) = so2ein(1) ! max. Sauerstoffeintrag aus der Luft ? , Ausgabe in mgO2/(l*h)
   benthic_distribution_p(18+(i-1)*number_benth_distr) = dO2o2D(1) ! Beiwert Oberflächenbelüftung ? Ausgabewert ersetzt bbei2D
   transfer_quantity_p(39+(i-1)*number_trans_quant) = salgo(1) ! Sauerstoffverbrauch durch Makrophyten in mgO2/l je Zeitschritt
   transfer_quantity_p(40+(i-1)*number_trans_quant) = dalgo(1) ! Sauerstoffproduktion der Gruen-, Kiesel-, und Blaualgen
   transfer_quantity_p(41+(i-1)*number_trans_quant) = dalgao(1)! Respiration (Sauerstoffverbrauch) der Gruen-, Kiesel-, und Blaualgen
   benthic_distribution_p(19+(i-1)*number_benth_distr) = o2ein1(1) ! Defizit-Sauerstoffeintrag aus der Luft ? , Ausgabe in mgO2/(l*h)
   benthic_distribution_p(20+(i-1)*number_benth_distr) = abeowg(1) ! Sauerstoffproduktion benthischer Grünalge
   benthic_distribution_p(21+(i-1)*number_benth_distr) = abeorg(1) ! Sauerstoffverbrauch benthischer Grünalgen
   benthic_distribution_p(22+(i-1)*number_benth_distr) = abeowk(1) ! Sauerstoffproduktion benthischer Kieselalgen
   benthic_distribution_p(23+(i-1)*number_benth_distr) = abeork(1) ! Sauerstoffverbrauch benthischer Kieselalgen
   transfer_quantity_p(43+(i-1)*number_trans_quant) = zooro2(1) ! Sauerstoffverbrauch durch Zooplanktonrespiration (Rückgabewert)
   do j = 1,num_lev_trans
      trans_quant_vert_p(j+(21-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = vz1(j,1) ! lokale Sauerstoffzehrung/produktion tiefenaufgelöst  !
   end do ! alle j tiefenlevels
   transfer_quantity_p(45+(i-1)*number_trans_quant) = saett(1) ! Sauerstoff Sättigungs-Konzentration in mgO2/l
   ! checks
   if (kontroll) print*,'oxygen (danach): plankt2,vo2(1),vo2(2),i = ',planktonic_variable_p(2+nk),vo2(1),vo2(2),i
   do k = 1,number_plankt_vari
      if (isnan(planktonic_variable_p(k+nk))) then
         print*,'nach oxygen: isnan(planktonic_variable_p  node#',iglob,' variable# ',k
         if (meinrang == 0)print*,'planktonic_variable_name:',planktonic_variable_name(k)
      endif
   end do
   do k = 1,number_trans_quant
      if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
         print*,'nach oxygen: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
         if (meinrang == 0)print*,'trans_quant_name:',trans_quant_name(k)
      endif
   end do
   do k = 1,number_benth_distr
      if (isnan(benthic_distribution_p(k+(i-1)*number_benth_distr))) then
         print*,'nach oxygen: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
         if (meinrang == 0)print*,'benth_distr_name:',benth_distr_name(k)
      endif
   end do
   return
end subroutine oxygen_huelle
!----+-----+----
!> die Subroutine ini_oxygen() steht in oxygen_huelle.f95\n
!! schreibt zunächst testwerte in die Sauerstoff-variablen. \n
!! prozess 0 alleine
!! ### ausgeschaltet in initialisieren() ### Vorbelegung durch randwerte
subroutine ini_oxygen()
   use modell
   implicit none
   integer i,j,nk
   real :: sauerstoffsaettigung
   !! 100% Sauerstoffsättigung:
   do i = 1,number_plankt_point
      nk = (i-1)*number_plankt_vari
      planktonic_variable(2+nk) = sauerstoffsaettigung(planktonic_variable(1+nk)) !! vO2_saett(T)
      do j = 1,num_lev
         plankt_vari_vert(j+(2-1)*num_lev+(i-1)*number_plankt_vari*num_lev) = planktonic_variable(2+nk) ! Sauerstoffgehalt tiefenaufgelöst
      end do ! alle j tiefenlevels
   end do
   print*,'Anfangsbedingung: 100% Sauerstoffsättigung vO2(1) = ',planktonic_variable(2+(1-1)*number_plankt_vari)
   return
end subroutine ini_oxygen

!----+-----+----
real function sauerstoffsaettigung(T)
   implicit none
   real :: T
   sauerstoffsaettigung = &
                          14.603-T*0.40215+(T**2) *0.007687-(T**3)*0.0000693
end function sauerstoffsaettigung

!----+-----+----
subroutine aufteilung_oxygen()
   return
end subroutine aufteilung_oxygen

