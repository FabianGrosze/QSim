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

subroutine ncyc_huelle(i)
   use modell
   use isotope
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,j,nk
   real :: f
   iglob = (i+meinrang*part)
   kontroll = iglob == kontrollknoten
   nk = (i-1)*number_plankt_vari
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe:
   tempw(1) = planktonic_variable_p( 1+nk)  ! Wassertemperatur
   tempw(2) = tempw(1)
   vx0(1) = planktonic_variable_p(15+nk)  ! nitrosomonas
   vx0(2) = vx0(1)
   vNH4(1) = planktonic_variable_p(3+nk)  ! ammonium
   vNH4(2) = vNH4(1)
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe
   if (tiefe(1) <= min_tief)print*,'ncyc_huelle: tiefe(1) <= min_tief,iglob,meinrang',tiefe(1),iglob,meinrang
   rau(1:2) = strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
   vmitt(1) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag
   vmitt(2) = vmitt(1)
   ! für die ABHAENGIGKEIT DER NITRIFIKATIONSLEISTUNG SESSILER NITRIFIKANTEN VON DER FLIESSGESCHWINDIGKEIT:
   rhyd(1) = tiefe(1) ! hydraulischer Radius | sinnvollste Annahme im mehrdimensionalen
   ! if (rhyd(1).le. 0.1)rhyd(1) = 0.1 !! clipping notwendig ?  ###
   rhyd(2) = rhyd(1)
   vo2(1) = planktonic_variable_p( 2+nk) ! Sauerstoffgehalt tiefengemittelt
   vo2(2) = vo2(1)
   go2n(1) = transfer_quantity_p(32+(i-1)*number_trans_quant) ! FUER DIE STICKSTOFFOXYDATION VERBRAUCHTE SAUERSTOFFMENGE
   go2n(2) = go2n(1)
   vno3(1) = planktonic_variable_p(5+nk)  ! nitrat
   vno3(2) = vno3(1)
   dC_DenW(1) = transfer_quantity_p(90 +(i-1)*number_trans_quant)! dC_DenW(ior) = dNO3Den/0.93 ! C-Abbau durch Denitrifikation in der Wassersäule
   dC_DenW(2) = dC_DenW(1)
   ! bsbt(1) = transfer_quantity_p(1+(i-1)*number_trans_quant) ! Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt (nur Rückgabe)
   ! bsbt(2) = bsbt(1)
   flag(1) = 0         ! keine Einleitungen
   flag(2) = flag(1)   ! ncyc so benutzen , dass nur der 1. Strang mit nur einem Knoten/Profil berechnet wird
   elen(1) = 1         ! Elementlänge (nicht verwendet)
   elen(2) = elen(1)
   ior = 1             ! Laufindex
   anze = 1            ! Anzahl der Profile im aktuellen Strang
   
   enh4(1) = 0.0        ! keine Einleitung
   eno3(1) = 0.0        ! keine Einleitung
   ex0(1) = 0.0        ! keine Einleitung
   qeinl(1) = 0.0      ! kein Abfluss Einleitung
   vabfl(1) = 2.5 !! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   vabfl(2) = vabfl(1)
   ! biofilme auf Wasserpflanzen macrophyten benthic_distribution(2,k)
   pfl(1) = benthic_distribution_p(3+(i-1)*number_benth_distr) ! Trockengewicht Wasserpflanzen in g/m²
   pfl(2) = pfl(1)
   sgo2n(1) = transfer_quantity_p(51+(i-1)*number_trans_quant) ! Aufsummierter Nitrifikationssauerstoffverbrauch, nur Ausgabe
   sgo2n(2) = sgo2n(1)
   sedx0(1) = benthic_distribution_p(33+(i-1)*number_benth_distr) ! sedimentierte Nitrosomonasbiomasse in µg/l, nur Ausgabewert
   sedx0(2) = sedx0(1)
   doN(1) = transfer_quantity_p(3+(i-1)*number_trans_quant) ! mineralisierter N-Gehalt in der Wassersäule ; Ammoniumfreisetzung beim Abbau org. Kohlenstoffverbidungen
   doN(2) = doN(1)
   susn(1) = transfer_quantity_p(29+(i-1)*number_trans_quant) ! Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE
   susn(2) = susn(1)
   bettn(1) = benthic_distribution_p(34+(i-1)*number_benth_distr) ! OXYDIERTE STICKSTOFFMENGE AM GEWAESSERBETT
   bettn(2) = bettn(1)
   susno(1) = transfer_quantity_p(52+(i-1)*number_trans_quant) ! ?
   susno(2) = susno(1)
   agrnh4(1) = transfer_quantity_p(34+(i-1)*number_trans_quant) ! Ammoniumaufnahme der gruen-Algen (unwirksam)
   agrnh4(2) = agrnh4(1)
   akinh4(1) = transfer_quantity_p(33+(i-1)*number_trans_quant) ! Ammoniumaufnahme der kiesel-Algen
   akinh4(2) = akinh4(1)
   dzres1(1) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
   dzres1(2) = dzres1(1)
   dzres2(1) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten
   dzres2(2) = dzres2(1)
   agrno3(1) = transfer_quantity_p(37+(i-1)*number_trans_quant) ! Nitrataufnahme der gruen-Algen
   agrno3(2) = agrno3(1)
   akino3(1) = transfer_quantity_p(36+(i-1)*number_trans_quant) ! Nitrataufnahme der kiesel-Algen
   akino3(2) = akino3(1)
   jiein(1) = 0        ! null Punkt-Einleitungen
   ischif(1:2) = zone(point_zone(iglob))%schiff%schifffahrts_zone ! hier unbenutzt
   ! direkt aus QSimDatenfelder ynmx1e   ! max. Wachstumsr. Nitrosomonas
   ! direkt aus QSimDatenfelder stks1e   ! Halbsättigungskonzen. Nitrosomonas
   ! direkt aus QSimDatenfelder anitrie  ! Absterberate für Nitrosomonas
   ! direkt aus QSimDatenfelder anitri2e ! Absterberate für Nitrobacter
   ! direkt aus QSimDatenfelder bnmx1e   ! Max. Umsatzrate Ammonium sessile Organismen
   ! direkt aus QSimDatenfelder bnks1e   ! Michaelis-Menten Konstante für den sessilen Umsatz von Ammonium
   vph(1) = planktonic_variable_p(66+nk) ! PH-Wert
   vph(2) = vph(1)
   vno2(1) = planktonic_variable_p(4+nk)  ! nitrit
   vno2(2) = vno2(1)
   ij = 0         ! unbenutzte Variable
   albewg(1) = benthic_distribution_p(13+(i-1)*number_benth_distr) ! Wachstum benthischer gruen-Algen
   albewg(2) = albewg(1)
   alberg(1) = benthic_distribution_p(11+(i-1)*number_benth_distr) ! Respiration benthischer gruen-Algen
   alberg(2) = alberg(1)
   albewk(1) = benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
   albewk(2) = albewk(1)
   alberk(1) = benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
   alberk(2) = alberk(1)
   resdr(1) = benthic_distribution_p(15+(i-1)*number_benth_distr) ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
   resdr(2) = resdr(1)
   aki(1) = planktonic_variable_p(8+nk) ! Anteil kiesel-Algen
   aki(2) = aki(1)
   agr(1) = planktonic_variable_p(9+nk) ! Anteil gruen-Algen
   agr(2) = agr(1)
   exdrvk(1) = benthic_distribution_p(29+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Kiesel-Algen
   exdrvk(2) = exdrvk(1)
   exdrvg(1) = benthic_distribution_p(30+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Grün-Algen
   exdrvg(2) = exdrvg(1)
   vx02(1) = planktonic_variable_p(16+nk)  ! nitrobacter
   vx02(2) = vx02(1)
   ex02(1) = 0.0        ! keine Einleitung
   eno2(1) = 0.0        ! keine Einleitung
   ! direkt aus QSimDatenfelder ynmx2e=transfer_parameter_p(77) ! max. Wachstumsr. Nitrobacter
   ! direkt aus QSimDatenfelder stks2e=transfer_parameter_p(78) ! Halbsättigungskonzen. Nitrobacter
   
   abl(1) = planktonic_variable_p(10+nk) ! Anteil ? Blau-Algen
   abl(2) = abl(1)
   ablnh4(1) = transfer_quantity_p(35+(i-1)*number_trans_quant) ! Ammoniumaufnahme der blau-Algen, tiefengem.
   ablnh4(2) = ablnh4(1)
   ablno3(1) = transfer_quantity_p(38+(i-1)*number_trans_quant) ! Nitrataufnahme der blau-Algen, tiefengem.
   ablno3(2) = ablno3(1)
   exdrvb(1) = benthic_distribution_p(31+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen
   exdrvb(2) = exdrvb(1)
   ! direkt aus QSimDatenfelder bnmx2e=transfer_parameter_p(80) ! Maximale Umsatzrate von Nitrit durch sessile Organismen in g/(m²*d) #### geht nicht ??????
   ! direkt aus QSimDatenfelder bnks2e=transfer_parameter_p(81) ! Michaelis-Menten Konst. sessil Umsatz Nitrit  in mg/l ?
   nl0(1) = planktonic_variable_p(57+nk) ! Verhältnis von Stickstoff zu Kohlenstoff in organischem Material
   nl0(2) = nl0(1)
   zooind(1) = planktonic_variable_p(50+nk) ! Anzahl der Rotatorien
   zooind(2) = zooind(1)
   ! direkt aus QSimDatenfelder GRote=transfer_parameter_p(67) ! Gewicht einer Rotatorie µg
   !nZoo  JETZT direkt aus QSimDatenfelder = transfer_value_p(2) ! Stickstoffanteil in der Rotatorienbiomasse mgN/mgBiom.
   gesN(1) = planktonic_variable_p(67+nk) ! gesamtstickstoff
   gesN(2) = gesN(1)
   orgCsd(1,1) = benthic_distribution_p(6+(i-1)*number_benth_distr) ! Sedimentierte Menge an organischem Kohlenstoff (ohne lebende Algen und Rotatorien)
   orgCsd(1,2) = orgCsd(1,1)
   egesN(1) = 0.0        ! hier keine Einleitung
   sedalk(1) = benthic_distribution_p(26+(i-1)*number_benth_distr) ! Sedimentierte Menge an Kiesel-Algen
   sedalk(2) = sedalk(1)
   sedalb(1) = benthic_distribution_p(28+(i-1)*number_benth_distr) ! Sedimentierte Menge an Blau-Algen
   sedalb(2) = sedalb(1)
   sedalg(1) = benthic_distribution_p(27+(i-1)*number_benth_distr) ! Sedimentierte Menge an Grün-Algen
   sedalg(2) = sedalg(1)
   ilbuhn = 0          ! keine Buhnen
   iwied = 0           ! unbenutzte Variable
   fkm (1) = 0.0       ! Flusskilometer (unbenutzt)
   CD(1,1) = planktonic_variable_p(37+nk) ! leicht abbaubare gelöste organische C-Verbindungen
   CD(1,2) = CD(1,1)
   CD(2,1) = planktonic_variable_p(38+nk) ! schwer abbaubare gelöste organische C-Verbindungen
   CD(2,2) = CD(2,1)
   CP(1,1) = planktonic_variable_p(39+nk) ! leicht abbaubare partikuläre organische C-Verbindungen
   CP(1,2) = CP(1,1)
   CP(2,1) = planktonic_variable_p(40+nk) ! schwer abbaubare partikuläre organische C-Verbindungen
   CP(2,2) = CP(2,1)
   CM(1) = planktonic_variable_p(41+nk) ! monomolekularen organischen C-Verbindungen
   CM(2) = CM(1)
   BAC(1) = planktonic_variable_p(42+nk) ! Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
   BAC(2) = BAC(1)
   bsbct(1) = transfer_quantity_p(47+(i-1)*number_trans_quant) ! mineralisierter Kohlenstoffgehalt in der Wassersäule | unbenutzt
   bsbct(2) = bsbct(1)
   nkzs(1) = 1         ! bisher nur eine Tiefenschicht
   nkzs(2) = nkzs(1)
   do j = 1,num_lev
      vNH4z(j,1) = plankt_vari_vert_p(j+(3-1)*num_lev+(i-1)*number_plankt_vari*num_lev)
      vNH4z(j,2) = vNH4z(j,1) ! Ammonium-Stickstoffkonzentration g/m³ tiefenaufgelöst
      vno2z(j,1) = plankt_vari_vert_p(j+(4-1)*num_lev+(i-1)*number_plankt_vari*num_lev)
      vno2z(j,2) = vno2z(j,1) ! Nitrit-Stickstoffkonzentration g/m³ tiefenaufgelöst
      vno3z(j,1) = plankt_vari_vert_p(j+(5-1)*num_lev+(i-1)*number_plankt_vari*num_lev)
      vno3z(j,2) = vno3z(j,1) ! Nitrat-Stickstoffkonzentration g/m³ tiefenaufgelöst
   end do ! alle tiefenlevels
   dH2D = 0.25 ! Dicke Tiefenschicht ??? Übergabe an Einleiter_Misch() | qsim.f90: dH2D = 0.25
   
   hJNO3(1,1) = benthic_distribution_p(35+(i-1)*number_benth_distr) ! Nitrat-Freisetzung aus dem Sediment
   hJNO3(1,2) = hJNO3(1,1)
   !### veri13.3 ### benthic_distribution_p(36+(i-1)*number_benth_distr)=0.1 !### veri13.3 ###
   hJNH4(1,1) = benthic_distribution_p(36+(i-1)*number_benth_distr) ! Ammonium-Freisetzung aus dem Sediment
   hJNH4(1,2) = hJNH4(1,1)
   hJN2(1,1) = benthic_distribution_p(47+(i-1)*number_benth_distr) ! N2 Freisetzung aus dem Sediment
   hJN2(1,2) = hJN2(1,1)
   susO2N(1) = 0.0     ! unbenutzte Variable
   susO2N(2) = susO2N(1)
   hFluN3(1,1) = benthic_distribution_p(37+(i-1)*number_benth_distr) ! Ausgabe NitratFlux Wasser/Sediment in mgN/(l*h)
   hFluN3(1,2) = hFluN3(1,1)
   ! direkt aus QSimDatenfelder akksn=transfer_parameter_p(24) ! N-Halbsättigung Kieselalgen
   ! direkt aus QSimDatenfelder agksn=transfer_parameter_p( 4) ! N-Halbsättigung Grünalgen
   ! direkt aus QSimDatenfelder Qmx_NK=transfer_parameter_p(31) ! max. Stickstoffanteil Algenbiomasse kiesel
   Q_NK(1) = planktonic_variable_p(30+nk) ! Stickstoffanteil der Algenbiomasse kiesel
   Q_NK(2) = Q_NK(1)
   do j = 1,num_lev_trans ! N-Aufnahmerate der Algengruppe kiesel
      up_NKz(j,1) = trans_quant_vert_p(j+(1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
      up_NKz(j,2) = up_NKz(j,1)
   end do ! alle j tiefenlevels
   ! direkt aus QSimDatenfelder Qmx_NG=transfer_parameter_p(10) ! max. N-Gehalt der Grünalgen
   Q_NG(1) = planktonic_variable_p(33+nk) ! Stickstoffanteil der Algenbiomasse gruen
   Q_NG(2) = Q_NG(1)
   do j = 1,num_lev_trans ! N-Aufnahmerate der Algengruppe gruen
      up_NGz(j,1) = trans_quant_vert_p(j+(2-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
      up_NGz(j,2) = up_NGz(j,1)
   end do ! alle j tiefenlevels
   ! direkt aus QSimDatenfelder Qmx_NB=transfer_parameter_p(54) ! max. Stickstoffanteil Algenbiomasse blau
   Q_NB(1) = planktonic_variable_p(35+nk) ! Stickstoffanteil der Algenbiomasse blau
   Q_NB(2) = Q_NB(1)
   do j = 1,num_lev_trans ! N-Aufnahmerate der Algengruppe blau
      up_NBz(j,1) = trans_quant_vert_p(j+(3-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
      up_NBz(j,2) = up_NBz(j,1)
   end do
   do j = 1,num_lev_trans ! ??? unbenutzt
      dalgkz(j,1) = trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      dalgkz(j,2) = dalgkz(j,1)
      dalgbz(j,1) = trans_quant_vert_p(j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      dalgbz(j,2) = dalgbz(j,1)
      dalggz(j,1) = trans_quant_vert_p(j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      dalggz(j,2) = dalggz(j,1)
   end do
   do j = 1,num_lev_trans
      
      !### veri13.3 ###trans_quant_vert_p(j+(10-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = 0.01 !### veri13.3 ###
      agnh4z(j,1) = trans_quant_vert_p(j+(10-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      agnh4z(j,2) = agnh4z(j,1) ! Ammoniumaufnahme Grün-Algen
      aknh4z(j,1) = trans_quant_vert_p(j+(9-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      aknh4z(j,2) = aknh4z(j,1) ! Ammoniumaufnahme Kiesel-Algen
      abnh4z(j,1) = trans_quant_vert_p(j+(11-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      abnh4z(j,2) = abnh4z(j,1) ! Ammoniumaufnahme Blau-Algen
      agno3z(j,1) = trans_quant_vert_p(j+(16-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      agno3z(j,2) = agno3z(j,1) ! Nitrataufnahme Grün-Algen
      akno3z(j,1) = trans_quant_vert_p(j+(15-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      akno3z(j,2) = akno3z(j,1) ! Nitrataufnahme Kiesel-Algen
      abno3z(j,1) = trans_quant_vert_p(j+(17-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      abno3z(j,2) = abno3z(j,1) ! Nitrataufnahme Blau-Algen
   end do
   do j = 1,num_lev ! Sauerstoffgehalt tiefenaufgelöst
      vo2z(j,1) = plankt_vari_vert_p(j+(2-1)*num_lev+(i-1)*number_plankt_vari*num_lev)
      vo2z(j,2) = vo2z(j,1)
   end do
   abltbr(1) = transfer_quantity_p(50+(i-1)*number_trans_quant) ! ??? Blaualgen ???
   abltbr(2) = abltbr(1)
   akitbr(1) = transfer_quantity_p(48+(i-1)*number_trans_quant) ! ? hier unbenutzt
   akitbr(2) = akitbr(1)
   agrtbr(1) = transfer_quantity_p(49+(i-1)*number_trans_quant) ! ? hier unbenutzt
   agrtbr(2) = agrtbr(1)
   do j = 1,num_lev_trans
      agrbrz(j,1) = trans_quant_vert_p(j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      agrbrz(j,2) = agrbrz(j,1) ! Wachstum ? Grün-Algen-Biomasse
      akibrz(j,1) = trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      akibrz(j,2) = akibrz(j,1) !  Wachstum ? Kiesel-Algen-Biomasse
      ablbrz(j,1) = trans_quant_vert_p(j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      ablbrz(j,2) = ablbrz(j,1) !  Wachstum ? Blau-Algen-Biomasse
   end do
   mstr = 1            ! Strangzähler
   uhrz = uhrzeit_stunde ! Uhrzeit module::modell zeitsekunde()
   itags = tag           ! Tag im Monat module::modell zeitsekunde()
   monats = monat        ! Monat im Jahr module::modell zeitsekunde()
   enl0(1) = 0.0       ! hier keine Einleitung
   do j = 1,num_lev_trans
      algakz(j,1) = trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      algakz(j,2) = algakz(j,1) ! Respiration Kiesel-Algen
      algagz(j,1) = trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      algagz(j,2) = algagz(j,1) ! Respiration Grün-Algen
      algabz(j,1) = trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      algabz(j,2) = algabz(j,1)
   end do
   do j = 1,num_lev_trans
      up_N2z(j,1) = trans_quant_vert_p(j+(8-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
      up_N2z(j,2) = up_N2z(j,1) ! Aufnahmerate von Luftstickstoff durch Blaualgen
   end do ! alle j tiefenlevels
   iorLa(1) = 0              ! AnfangsKnoten der Linienquelle; nicht verwendet
   iorLe(1) = 0              ! EndKnoten der Linienquelle; nicht verwendetiorLa,
   ieinLs(1) = 0       ! null Linien-Einleitungen
   ieinLs(2) = ieinLs(1)
   flae(1) = 1000.0 !! unbenutzt da keine Einleitung
   flae(2) = flae(1)
   qeinlL(1) = 0.0        ! Zufluss Linienquelle; nicht verwendet
   eNH4L(1) = 0.0       ! hier keine Linienquelle
   eNO2L(1) = 0.0       ! hier keine Linienquelle
   eNO3L(1) = 0.0       ! hier keine Linienquelle
   gesNL(1) = 0.0       ! hier keine Linienquelle
   do j = 1,num_lev_trans
      hgesNz(1,j,1) = plankt_vari_vert_p(j+(67-1)*num_lev+(i-1)*number_plankt_vari*num_lev) ! wie gesN
      hgesNz(1,j,2) = hgesNz(1,j,1)
   end do ! alle j tiefenlevels
   !if(kontroll)then
   !   print*,' ncyc  vorher:'
   !   print*,' Nitrit, vx0(1) nitrosomonas=',planktonic_variable_p( 4+nk), vx0(1), planktonic_variable_p(15+nk)
   !   print*,' rau(1),tiefe(1),vmitt(1),rhyd(1)=',rau(1),tiefe(1),vmitt(1),rhyd(1)
   !endif
   if (kontroll)print*,'ncyc vorher: Ammonium,Nitrit,Nitrat,nitrosomonas,nitrobacter = '  &
       ,vNH4(1),vno2(1),vno3(1),vx0(1),vx02(1)
   if (kontroll)print*,'ncyc vorher: hJNO3,hJNH4,hJN2 = ',hJNO3(1,1),hJNH4(1,1),hJN2(1,1)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   call ncyc(tempw,vx0,vnh4,tflie,rau,tiefe,vmitt,rhyd,vo2,go2n,vno3,dC_DenW,flag,elen,ior,anze              &
             ,enh4,eno3,ex0,qeinl,vabfl,pfl,sgo2n,sedx0,don,susn,bettn,susno,agrnh4,akinh4,dzres1,dzres2     &
             ,agrno3,akino3,jiein,ischif,YNMAX1,STKS1,ANITR1,BNMX1,BNKS1,vph,vno2,ij                         &
             ,albewg,alberg,albewk,alberk,resdr,aki,agr,exdrvk,exdrvg,vx02,ex02,eno2,YNMAX2,STKS2,ANITR2     &
             ,abl,ablnh4,ablno3,exdrvb,BNMX2,BNKS2,nl0,zooind,GROT,nzoo,gesN,orgCsd                          &
             ,egesN,sedalk,sedalb,sedalg,ilbuhn,iwied,fkm,CD,CP,CM,BAC,bsbct,nkzs,vnh4z,vno2z,vno3z,dH2D     &
             ,hJNO3,hJNH4,hJN2,susO2N,hFluN3,akksN,agksN,abksN,Qmx_NK,Q_NK,up_NKz,Qmx_NG,Q_NG,up_NGz,Qmx_NB  &
             ,Q_NB,up_NBz,dalgkz,dalgbz,dalggz,agnh4z,aknh4z,abnh4z,agno3z,akno3z,abno3z,vo2z,abltbr         &
             ,akitbr,agrtbr,agrbrz,akibrz,ablbrz,mstr,uhrz,itags,monats,enl0,algakz,algagz,algabz            &
             ,up_N2z,iorLa,iorLe,ieinLs,flae,qeinlL,eNH4L,eNO2L,eNO3L,gesNL,hgesNz,algdrk,algdrg,algdrb      &
             ,ifehl,ifhstr, azStrs,    kontroll ,iglob )
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe:
   if ( isnan(vno2z(1,1)) )print*,'ncyc nachher: isnan(vno2z)  node#',iglob
   if (kontroll)print*,'ncyc nachher: gesN,Ammonium,vNH4z,Nitrit,vno2z,Nitrat, nitrosomona,nitrobacter,nl0s = ',  &
       gesN(1),vNH4(1),vNH4z(1,1),vno2(1),vno2z(1,1),vno3(1),vx0(1),vx02(1),nl0(1)
   planktonic_variable_p( 3+nk) = vNH4(1)  ! ammonium
   planktonic_variable_p( 4+nk) = vno2(1)  ! nitrit
   planktonic_variable_p( 5+nk) = vno3(1)  ! nitrat
   planktonic_variable_p(15+nk) = vx0(1)   ! nitrosomonas
   planktonic_variable_p(16+nk) = vx02(1)  ! nitrobacter
   ! keine Rückgabe planktonic_variable_p(57+nk) = nl0(1)   ! Verhältnis von Stickstoff zu Kohlenstoff in organischem Material
   planktonic_variable_p(67+nk) = gesN(1)  ! gesamtstickstoff
   transfer_quantity_p(32+(i-1)*number_trans_quant) = go2n(1) ! FUER DIE STICKSTOFFOXYDATION VERBRAUCHTE SAUERSTOFFMENGE
   transfer_quantity_p(90 +(i-1)*number_trans_quant) = dC_DenW(1)! dC_DenW(ior) = dNO3Den/0.93 ! C-Abbau durch Denitrifikation in der Wassersäule
   transfer_quantity_p(51+(i-1)*number_trans_quant) = sgo2n(1) ! Aufsummierter Nitrifikationssauerstoffverbrauch, nur Ausgabe
   transfer_quantity_p(29+(i-1)*number_trans_quant) = susn(1) ! Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE
   benthic_distribution_p(34+(i-1)*number_benth_distr) = bettn(1) ! OXYDIERTE STICKSTOFFMENGE AM GEWAESSERBETT, Ausgabewert
   transfer_quantity_p(52+(i-1)*number_trans_quant) = susno(1) ! ?, Ausgabewert
   transfer_quantity_p(34+(i-1)*number_trans_quant) = agrnh4(1) ! Ammoniumaufnahme der gruen-Algen
   transfer_quantity_p(33+(i-1)*number_trans_quant) = akinh4(1) ! Ammoniumaufnahme der kiesel-Algen
   transfer_quantity_p(37+(i-1)*number_trans_quant) = agrno3(1) ! Nitrataufnahme der gruen-Algen
   transfer_quantity_p(36+(i-1)*number_trans_quant) = akino3(1) ! Nitrataufnahme der kiesel-Algen
   do j = 1,num_lev
      plankt_vari_vert_p(j+(3-1)*num_lev+(i-1)*number_plankt_vari*num_lev) = vNH4z(j,1)
      plankt_vari_vert_p(j+(4-1)*num_lev+(i-1)*number_plankt_vari*num_lev) = vno2z(j,1)
      plankt_vari_vert_p(j+(5-1)*num_lev+(i-1)*number_plankt_vari*num_lev) = vno3z(j,1)
      plankt_vari_vert_p(j+(67-1)*num_lev+(i-1)*number_plankt_vari*num_lev) = hgesNz(1,j,1) ! wie gesN
   end do ! alle j tiefenlevels
   benthic_distribution_p(37+(i-1)*number_benth_distr) = hFluN3(1,1) ! Ausgabe NitratFlux Wasser/Sediment in mgN/(l*h)
   !!    Isotope:
   !      if(nkzs(ior)>1)      algN3m = agrno3(ior)+akino3(ior)+ablno3(ior)
   !      else                 algN3m = agno3z(1,ior)+akno3z(1,ior)+abno3z(1,ior)
   !      if(vno3(ior).gt. 0.0)then
   !         f=algN3m/vno3(ior) !! fraction consumed in this timestep
   !         call isotop_no3konsum(f)
   !      call isotop_no3konsum(f,kontroll)
   return
end subroutine ncyc_huelle
