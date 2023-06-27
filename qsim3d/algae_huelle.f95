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

!> SUBROUTINE algae_huelle() \n
!! zurück: \ref index oder \ref lnk_phytoplankton ; 
!! Code: algae_huelle.f95
subroutine algae_huelle(i)
   use modell
   use QSimDatenfelder
   use module_aparam
   implicit none
   
   integer :: i,j,k,nk,i2,string_write_error
   integer :: ieros_flag
   logical :: error
     
   !if(i==1)print*,'algae_huelle läuft an'
   iglob = i + meinrang * part
   do k = 1,number_trans_quant
      if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
         print*,'vorher: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
         if (meinrang == 0)print*,'trans_quant_name:',trans_quant_name(k)
      endif
   enddo
   
   do j = 1,num_lev_trans
      do k = 1,number_trans_quant_vert
         if (isnan(trans_quant_vert_p(j+(k-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert))) then
            print*,'algaes** vorher: isnan(trans_quant_vert_p  node#',iglob,' level#', j,' variable# ',k
            if (meinrang == 0)print*,'trans_quant_vert_name:',trans_quant_vert_name(k)
         endif
      enddo
   enddo
   nk = (i-1)*number_plankt_vari
   i2 = zone(point_zone(iglob))%wettstat%wetterstations_nummer !! ist parallel !!!
   control = iglob == kontrollknoten
   !if(i.eq.1)print*,'algae_huelle: lesen von e_extnct.dat (algaeski) bei jedem Knoten ist noch viiieel zu umständlich ###'
   if (control)print*,'algae_huelle: nk,i,iglob = ',nk,i,iglob
   
   ! ==========================================================================
   ! Datenübergabe
   schwi(1) = schwi_T(i2)    ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
   schwi(2) = schwi(1)
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   tempw(1) = planktonic_variable_p(1+nk)    ! Wasser-Temperatur
   tempw(2) = tempw(1)
   do j = 1,num_lev
      tempwz(j,1) = plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Wassertemperatur tiefenaufgelöst
      tempwz(j,2) = tempwz(j,1)
   enddo
   tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
   rau(1:2) = strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
   vmitt(1) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag; randbedingungen.h
   vmitt(2) = vmitt(1)
   vno3(1) = planktonic_variable_p(5+nk)  ! nitrat
   vno3(2) = vno3(1)
   vNH4(1) = planktonic_variable_p(3+nk)  ! ammonium
   vNH4(2) = vNH4(1)
   gelp(1) = planktonic_variable_p( 6+nk)  ! gelöster ortho-Phosphat-Phosphor tiefengemittelt
   gelp(2) = gelp(1)
   svhemk(1) = planktonic_variable_p(21+nk)  ! Mittelwertbildung der Licht-Hemmung ?
   svhemk(2) = svhemk(1)
   svhemg(1) = planktonic_variable_p(22+nk)  ! Mittelwertbildung der Licht-Hemmung ?
   svhemg(2) = svhemg(1)
   svhemb(1) = planktonic_variable_p(23+nk)  ! Mittelwertbildung der Licht-Hemmung ?
   svhemb(2) = svhemb(1)
   CHLA(1) = planktonic_variable_p(11+nk)  ! Chlorophyl-A
   CHLA(2) = CHLA(1)
   ssalg(1:2) = planktonic_variable_p(52+nk) ! GESAMTSCHWEBSTOFFE incl. lebender Organismen, messbar, Randwert
   ir(1) = transfer_quantity_p(42+(i-1)*number_trans_quant) ! Ingestionsrate der Rotatorien in mg/(l*h) | konsum() (unbenutzt)
   ir(2) = ir(1)
   si(1) = planktonic_variable_p( 7+nk)  ! silikat-Silizium-Konzentration (tiefengemittelt)
   si(2) = si(1)
   dalgki(1) = transfer_quantity_p(20+(i-1)*number_trans_quant) ! Zuwachs Kiesel-Algen
   dalgki(2) = dalgki(1)
   dalggr(1) = transfer_quantity_p(21+(i-1)*number_trans_quant) ! Zuwachs grün-Algen
   dalggr(2) = dalggr(1)
   dalgbl(1) = transfer_quantity_p(22+(i-1)*number_trans_quant) ! Zuwachs blau-Algen
   dalgbl(2) = dalgbl(1)
   dalgak(1) = transfer_quantity_p(23+(i-1)*number_trans_quant) ! Respiration Kiesel-Algen
   dalgak(2) = dalgak(1)
   dalgag(1) = transfer_quantity_p(24+(i-1)*number_trans_quant) ! Respiration grün-Algen
   dalgag(2) = dalgag(1)
   dalgab(1) = transfer_quantity_p(25+(i-1)*number_trans_quant) ! Respiration blau-Algen
   dalgab(2) = dalgab(1)
   flag(1) = 0         ! keine Einleitungen
   flag(2) = flag(1)
   elen(1) = 1         ! Elementlänge (nicht verwendet)
   elen(2) = elen(1)
   ior = 1             ! Laufindex
   anze = 1            ! Anzahl der Profile im aktuellen Strang
   sedalk(1) = benthic_distribution_p(26+(i-1)*number_benth_distr) ! Sedimentierte Menge an Kiesel-Algen  benthische_verteilungen.f95
   sedalk(2) = sedalk(1)
   sedalg(1) = benthic_distribution_p(27+(i-1)*number_benth_distr) ! Sedimentierte Menge an Grün-Algen
   sedalg(2) = sedalg(1)
   sedalb(1) = benthic_distribution_p(28+(i-1)*number_benth_distr) ! Sedimentierte Menge an Blau-Algen
   sedalb(2) = sedalb(1)
   algzok(1) = transfer_quantity_p(53+(i-1)*number_trans_quant) ! kiesel-Algen-Konsum Zoo-Plankton in mg/l
   algzok(2) = algzok(1)
   algzog(1) = transfer_quantity_p(72+(i-1)*number_trans_quant) ! grün-Algen-Konsum Zoo-Plankton in mg/l
   algzog(2) = algzog(1)
   algzob(1) = transfer_quantity_p(73+(i-1)*number_trans_quant) ! blau-Algen-Konsum Zoo-Plankton in mg/l
   algzob(2) = algzob(1)
   echla(1) = 0.0     ! keine Einleitung
   qeinl(1) = 0.0      ! kein Abfluss Einleitung
   vabfl(1) = 0.0     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   vabfl(2) = vabfl(1)
   dkimor(1) = transfer_quantity_p(7+(i-1)*number_trans_quant) ! Absterberate Kieselalgen
   dkimor(2) = dkimor(1)
   dgrmor(1) = transfer_quantity_p(8+(i-1)*number_trans_quant) ! Absterberate Grünalgen
   dgrmor(2) = dgrmor(1)
   dblmor(1) = transfer_quantity_p(9+(i-1)*number_trans_quant) ! Absterberate Blaualgen
   dblmor(2) = dblmor(1)
   fkm (1) = 0.0      ! Flusskilometer (unbenutzt)
   jiein(1) = 0       ! keine Punkt-Einleitungen
   evkigr(1) = 0.0  ! Einleitungswert (keine Einleitungen in T-QSim)
   vkigr(1) = planktonic_variable_p(19+(i-1)*number_plankt_vari) ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ??
   vkigr(2) = vkigr(1)
   antbl(1) = planktonic_variable_p(20+(i-1)*number_plankt_vari) ! Anteil der Blaualgen am Gesamt-Chlorophyll-a
   antbl(2) = antbl(1)
   eantbl(1) = 0.0  ! Einleitungswert (keine Einleitungen in T-QSim)

   saettk = transfer_value_p(6)    ! Rückgabewert ???
   saettg = transfer_value_p(8)    ! ???
   saettb = transfer_value_p(9)    ! ???
   akrema = 0.0 ! unbenutzt
   !if(control)print*,'algae_huelle: PCmax =',(akgmax+akremi)/(1.-frmuke) ! max C-spezifische Photosyntheserate bei optimal Temperatur
   sbioki = 0.0 ! unbenutzt
   vco2(1) = transfer_quantity_p(26+(i-1)*number_trans_quant) ! Kohlendioxyd ! unbenutzt
   vco2(2) = vco2(1)
   ! iph ! mit/ohne ph-Wertberechnung  kommt aus modul modell
   akbcm(1) = planktonic_variable_p(24+nk) ! Verhältnis Chlorophyll-a zu Kohlenstoff Kieselalgen
   akbcm(2) = akbcm(1)
   agbcm(1) = planktonic_variable_p(25+nk) ! Verhältnis Chlorophyll-a zu Kohlenstoff Gruenalgen
   agbcm(2) = agbcm(1) ! nur Einleitung
   abbcm(1) = planktonic_variable_p(26+nk) ! Verhältnis Chlorophyll-a zu Kohlenstoff der blau-Algen
   abbcm(2) = abbcm(1) ! nur Einleitung
   aki(1) = planktonic_variable_p(8+nk) ! Biomasse an Kiesel-Algen
   aki(2) = aki(1)
   agr(1) = planktonic_variable_p(9+nk) ! Biomasse an gruen-Algen
   agr(2) = agr(1)
   abl(1) = planktonic_variable_p(10+nk) ! Biomasse an Blau-Algen
   abl(2) = abl(1)
   extk(1) = transfer_quantity_p(54+(i-1)*number_trans_quant) ! mittlerer Extinktionskoeffizient
   extk(2) = extk(1)
   ilamda = rb_extnct_ilamda
   do j = 1,ilamda
      eta(j) = rb_extnct_p(1 + (j-1)*anz_extnct_koeff)
      !if(control)print*,'eta(',j,')=',eta(j)
      aw(j) = rb_extnct_p(2 + (j-1)*anz_extnct_koeff)
      ack(j) = rb_extnct_p(3 + (j-1)*anz_extnct_koeff)
      acg(j) = rb_extnct_p(4 + (j-1)*anz_extnct_koeff)
      acb(j) = rb_extnct_p(5 + (j-1)*anz_extnct_koeff)
      ah(j) = rb_extnct_p(6 + (j-1)*anz_extnct_koeff)
      as(j) = rb_extnct_p(7 + (j-1)*anz_extnct_koeff)
      al(j) = rb_extnct_p(8 + (j-1)*anz_extnct_koeff)
      ! extk_lamda wird nur von algaeski an algaesbl und algaesgr übergeben. an jedem Knoten hier in algae_huelle
      !extk_lamda(j,1) = rb_extnct_p(9 + (j-1)*anz_extnct_koeff) ! eigentlich nur Rückgabewert
      extk_lamda(j,1) = 0.0 ! initialize
      !if(control)print*,'extk_lamda(',j,',1)=',extk_lamda(j,1)
   enddo
   uhrz = uhrzeit_stunde     ! Uhrzeit module::modell zeitsekunde()
   sised(1) = benthic_distribution_p(2+(i-1)*number_benth_distr) ! Siliziumgehalt im Sediment
   sised(2) = sised(1)
   tpki(1) = transfer_quantity_p(55+(i-1)*number_trans_quant) ! Ausgabeparameter ?? Kieselalgen Phosphor ??
   tpki(2) = tpki(1)
   tpgr(1) = transfer_quantity_p(80+(i-1)*number_trans_quant) ! Ausgabeparameter ?? Grünalgen Phosphor ??
   tpgr(2) = tpgr(1)
   tpbl(1) = transfer_quantity_p(81+(i-1)*number_trans_quant) ! Ausgabeparameter ?? Blaualgen Phosphorhemmung ??
   tpbl(2) = tpbl(1)
   iwied = 0      ! unbenutzte Variable
   akmuea(1) = transfer_quantity_p(56+(i-1)*number_trans_quant) ! Wachstumsrate Ausgabeparameter algaeski()
   akmuea(2) = akmuea(1)
   agmuea(1) = transfer_quantity_p(84+(i-1)*number_trans_quant) ! Wachstumsrate Ausgabeparameter algaesgr()
   agmuea(2) = agmuea(1)
   abmuea(1) = transfer_quantity_p(85+(i-1)*number_trans_quant) ! Wachstumsrate Ausgabeparameter algaesbl()
   abmuea(2) = abmuea(1)
   ftaaus(1) = transfer_quantity_p(57+(i-1)*number_trans_quant) ! Ausgabeparameter algaeski() fta
   ftaaus(2) = ftaaus(1)
   fiaus(1) = transfer_quantity_p(58+(i-1)*number_trans_quant) ! Ausgabeparameter algaeski() Pmit/(Pmax*3.6)
   fiaus(2) = fiaus(1)
   figaus(1) = transfer_quantity_p(82+(i-1)*number_trans_quant) ! Ausgabeparameter algaesgr() Pmit/(Pmax*3.6)
   figaus(2) = figaus(1)
   fibaus(1) = transfer_quantity_p(83+(i-1)*number_trans_quant) ! Ausgabeparameter algaesbl() Pmit/(Pmax*3.6)
   fibaus(2) = fibaus(1)
   fheaus(1) = transfer_quantity_p(59+(i-1)*number_trans_quant) ! Ausgabeparameter algaeski() svhemk
   fheaus(2) = fheaus(1)
   fhegas(1) = transfer_quantity_p(86+(i-1)*number_trans_quant) ! Ausgabeparameter algaesi() svhemg
   fhegas(2) = fhegas(1)
   fhebas(1) = transfer_quantity_p(87+(i-1)*number_trans_quant) ! Ausgabeparameter algaes() svhemb
   fhebas(2) = fhebas(1)
   akraus(1) = transfer_quantity_p(60+(i-1)*number_trans_quant) ! Ausgabe akbcm algaeski()
   akraus(2) = akraus(1)
   agreau(1) = transfer_quantity_p(88+(i-1)*number_trans_quant) ! Ausgabe agbcm algaesgr()
   agreau(2) = agreau(1)
   abreau(1) = transfer_quantity_p(89+(i-1)*number_trans_quant) ! Ausgabe von abbcm (Chlorophyll-a zu Kohlenstoff der blau-Algen) algaesbl()
   abreau(2) = abreau(1)
   tauscs = transfer_value_p(7)    ! Schiffseinfluss     qsim.f90: tauscs = 1.25
   ischif = zone(point_zone(iglob))%schiff%schifffahrts_zone ! schifffahrt in dieser module::zonen ; 1->Schiffsverkehr  , 0-> kein Schiffsverkehr; MODELLG.txt "F"
   ilbuhn = 0          ! keine Buhnen
   ieros_flag = ieros ! Erosionsflag unbenutzt
   cmatki(1) = benthic_distribution_p(9+(i-1)*number_benth_distr) ! Abspülung benthischer kiesel-Algen
   cmatki(2) = cmatki(1)
   cmatgr(1) = benthic_distribution_p(10+(i-1)*number_benth_distr) ! Abspülung benthischer grün-Algen
   cmatgr(2) = cmatgr(1)
   algdrk(1) = benthic_distribution_p(38+(i-1)*number_benth_distr) ! \ref Algen-Konsum-bentisch (Muscheln) in mg/l
   algdrk(2) = algdrk(1)
   algdrg(1) = benthic_distribution_p(40+(i-1)*number_benth_distr) ! grün-Algen-Konsum-bentisch (Muscheln) in mg/l
   algdrg(2) = algdrg(1)
   algdrb(1) = benthic_distribution_p(41+(i-1)*number_benth_distr) ! blau-Algen-Konsum-bentisch (Muscheln) in mg/l
   algdrb(2) = algdrb(1)
   !
   algcok(1) = benthic_distribution_p(39+(i-1)*number_benth_distr) ! Kiesel-Algen Konsum durch Corophium ?
   algcok(2) = algcok(1)
   algcog(1) = benthic_distribution_p(42+(i-1)*number_benth_distr) ! grün-Algen Konsum durch Corophium ?
   algcog(2) = algcog(1)
   algcob(1) = benthic_distribution_p(43+(i-1)*number_benth_distr) ! blau-Algen Konsum durch Corophium ?
   algcob(2) = algcob(1)
   ess = 0.0 ! unbenutzt
   ! pbiogr, nbiogr, nbiobl, pbiobl ! unbenutzt
   zooind(1) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien in Ind/l
   zooind(2) = zooind(1)
   !jetzt direkt aus QSimDatenfelder GROT=transfer_parameter_p(67) ! Gewicht einer Rotatorie µg  | Aparam.txt
   ss(1) = planktonic_variable_p(53+nk) ! ORG. UND ANORG. SCHWEBSTOFFE (OHNE ALGEN UND ZOOPLANKTER)
   ss(2) = ss(1)
   Q_PK(1) = planktonic_variable_p(31+nk) ! Phosphoranteil der Kiesel-Algenbiomasse
   Q_PK(2) = Q_PK(1)
   Q_NK(1) = planktonic_variable_p(30+nk) ! Stickstoffanteil der Algenbiomasse kiesel
   Q_NK(2) = Q_NK(1)
   Q_SK(1) = planktonic_variable_p(32+nk)  ! Siliziumgehalt Kieselalgen
   Q_SK(2) = Q_SK(1)
   Q_NG(1) = planktonic_variable_p(33+nk) ! Stickstoffanteil der Algenbiomasse grün
   Q_NG(2) = Q_NG(1)
   Q_PG(1) = planktonic_variable_p(34+nk) ! Phosphoranteil der grün-Algenbiomasse
   Q_PG(2) = Q_PG(1)
   Q_NB(1) = planktonic_variable_p(35+nk) ! Stickstoffanteil der Algenbiomasse blau
   Q_NB(2) = Q_NB(1)
   Q_PB(1) = planktonic_variable_p(36+nk) ! Phosphoranteil der blau-Algenbiomasse
   Q_PB(2) = Q_PB(1)
   do j = 1,num_lev
      vNH4z(j,1) = plankt_vari_vert_p(j+(3-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Ammonium-Stickstoffkonzentration g/m³ tiefenaufgelöst
      vNH4z(j,2) = vNH4z(j,1)
      vno3z(j,1) = plankt_vari_vert_p(j+(5-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Nitrat-Stickstoffkonzentration g/m³ tiefenaufgelöst
      vno3z(j,2) = vno3z(j,1)
      gelpz(j,1) = plankt_vari_vert_p(j+(6-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! gelöster Phosphor tiefenaufgelöst
      gelpz(j,2) = gelpz(j,1)
      siz(j,1) = plankt_vari_vert_p(j+(7-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Silizium-Konzentration (tiefenaufgelöst)
      siz(j,2) = siz(j,1)
      dalgkz(j,1) = trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zuwachs Kieselalgen tiefenaufgelöst
      dalgkz(j,2) = dalgkz(j,1)
      dalggz(j,1) = trans_quant_vert_p(j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zuwachs Kieselalgen tiefenaufgelöst
      dalggz(j,2) = dalggz(j,1)
      dalgbz(j,1) = trans_quant_vert_p(j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zuwachs Kieselalgen tiefenaufgelöst
      dalgbz(j,2) = dalgbz(j,1)
   enddo
   nkzs(1) = 1         ! nur eine Tiefenschicht
   nkzs(2) = nkzs(1)
   dH2D = 0.25 ! Dicke Tiefenschicht ???
   dH2De = 0.25 ! unklar
   write(cpfad,'(A)', iostat = string_write_error )adjustl(trim(modellverzeichnis))! =  ! modellverzeichnis zum einlesen vone_extnct.dat
   if (string_write_error /= 0) then
      write(fehler,*)'241: Übergabe modellverzeichnis- > cpfad in algae_huelle() fehlgeschalgen'
      call qerror(fehler)
   endif
   do j = 1,num_lev
      up_NKz(j,1) = trans_quant_vert_p(j+(1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! N (Stickstoff) Aufnahmerate der Kiesel-Algen
      up_NKz(j,2) = up_NKz(j,1)
      up_NGz(j,1) = trans_quant_vert_p(j+(2-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! N (Stickstoff) Aufnahmerate der grün-Algen
      up_NGz(j,2) = up_NGz(j,1)
      up_NBz(j,1) = trans_quant_vert_p(j+(3-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! N (Stickstoff) Aufnahmerate der blau-Algen
      up_NBz(j,2) = up_NBz(j,1)
      up_Siz(j,1) = trans_quant_vert_p(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Si-Aufnahmerate der Kiesel-Algen
      up_Siz(j,2) = up_Siz(j,1)
      up_PKz(j,1) = trans_quant_vert_p(j+(5-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! P (Phosphor) Aufnahmerate der Kiesel-Algen
      up_PKz(j,2) = up_PKz(j,1)
      up_PGz(j,1) = trans_quant_vert_p(j+(6-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! P (Phosphor) Aufnahmerate der grün-Algen
      up_PGz(j,2) = up_PGz(j,1)
      up_PBz(j,1) = trans_quant_vert_p(j+(7-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! P (Phosphor) Aufnahmerate der blau-Algen
      up_PBz(j,2) = up_PBz(j,1)
   enddo

   Skmor(1) = planktonic_variable_p(69+nk)  ! Silizium in schwebenden, abgestorbenen Kieselalgen
   Skmor(2) = Skmor(1)
   akitbr(1) = transfer_quantity_p(48+(i-1)*number_trans_quant) ! Kieselalgen ??
   akitbr(2) = akitbr(1)
   !if(iglob.eq.kontrollknoten)print*,'vor algaeski; akitbr=',akitbr(1)
   agrtbr(1) = transfer_quantity_p(49+(i-1)*number_trans_quant) !
   agrtbr(2) = agrtbr(1)
   abltbr(1) = transfer_quantity_p(50+(i-1)*number_trans_quant) ! Zwischengröße Algenbiomasse ???
   abltbr(2) = abltbr(1)
   do j = 1,num_lev
      chlaz(j,1) = plankt_vari_vert_p(j+(11-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Chlorophyl-A tiefenaufgelöst
      chlaz(j,2) = chlaz(j,1)
   enddo
   do j = 1,num_lev_trans
      akibrz(j,1) = trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zwischengröße Kiesel-Algen-Biomasse ?
      akibrz(j,2) = akibrz(j,1)
      agrbrz(j,1) = trans_quant_vert_p(j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zwischengröße grün-Algen-Biomasse ?
      agrbrz(j,2) = agrbrz(j,1)
      ablbrz(j,1) = trans_quant_vert_p(j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zwischengröße blau-Algen-Biomasse ?
      ablbrz(j,2) = ablbrz(j,1)
      up_N2z(j,1) = trans_quant_vert_p(j+(8-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !  Aufnahmerate von Luftstickstoff durch Blaualgen
      up_N2z(j,2) = up_N2z(j,1)
   enddo
   do j = 1,num_lev
      akiz(j,1) = plankt_vari_vert_p(j+( 8-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse kiesel-Algen tiefenaufgelöst
      akiz(j,2) = akiz(j,1)
      agrz(j,1) = plankt_vari_vert_p(j+( 9-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse gruen-Algen tiefenaufgelöst
      agrz(j,2) = agrz(j,1)
      ablz(j,1) = plankt_vari_vert_p(j+(10-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse blau-Algen tiefenaufgelöst
      ablz(j,2) = ablz(j,1)
   enddo
   chlaL(1) = 0.0            ! für Linienquelle; nicht verwendet
   qeinlL(1) = 0.0           ! für Linienquelle; nicht verwendet
   iorLa(1) = 0              ! zur Berücksichtigung der Linienquelle; nicht verwendet
   iorLe(1) = 0              ! zur Berücksichtigung der Linienquelle; nicht verwendet
   ieinLs(1) = 0             ! keine Linienquellen
   do j = 1,num_lev_trans
      algakz(j,1) = trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !  Respirierte Kiesel-Algenbiomasse,
      algakz(j,2) = algakz(j,1)
      algagz(j,1) = trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !  Respirierte grün-Algenbiomasse,
      algagz(j,2) = algagz(j,1)
      algabz(j,1) = trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !  Respirierte blau-Algenbiomasse
      algabz(j,2) = algabz(j,1)
      algzkz(j,1) = trans_quant_vert_p(j+(26-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l
      algzkz(j,2) = algzkz(j,1)
      algzgz(j,1) = trans_quant_vert_p(j+(27-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! grün-Algen-Konsum durch Zoo-Plankton in mg/l
      algzgz(j,2) = algzgz(j,1)
      algzbz(j,1) = trans_quant_vert_p(j+(28-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! blau-Algen-Konsum durch Zoo-Plankton in mg/l
      algzbz(j,2) = algzbz(j,1)
   enddo
   Chlaki(1) = planktonic_variable_p(12+nk)  ! Chlorophyl in Kieselalgen muegchla/l
   Chlaki(2) = Chlaki(1)
   chlagr(1) = planktonic_variable_p(13+nk)  ! Chlorophyl in gruenalgen muegchla/l
   chlagr(2) = chlagr(1)
   chlabl(1) = planktonic_variable_p(14+nk)  ! Chlorophyl in Blaualgen muegchla/l
   chlabl(2) = chlabl(1)
   do j = 1,num_lev ! wohl nur Rückgabeparameter ???
      hchlkz(1,j,1) = plankt_vari_vert_p(j+(12-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Chlorophylgehalt der Kieselalgen
      hchlkz(1,j,2) = hchlkz(1,j,1)
      hchlgz(1,j,1) = plankt_vari_vert_p(j+(13-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Chlorophylgehalt der Gruenalgen
      hchlgz(1,j,2) = hchlgz(1,j,1)
      hchlbz(1,j,1) = plankt_vari_vert_p(j+(14-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Chlorophylgehalt der Blaualgen
      hchlbz(1,j,2) = hchlbz(1,j,1)
      hCChlkz(1,j,1) = plankt_vari_vert_p(j+(20-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! c-chla Verhältnis Kiesel
      hCChlkz(1,j,2) = hCChlkz(1,j,1)
      hCChlgz(1,j,1) = plankt_vari_vert_p(j+(21-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! c-chla Verhältnis grün
      hCChlgz(1,j,2) = hCChlgz(1,j,1)
      hCChlbz(1,j,1) = plankt_vari_vert_p(j+(22-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! c-chla Verhältnis blau
      hCChlbz(1,j,2) = hCChlbz(1,j,1)
   enddo
   ! in tiefengemittelten Berechnungen eigentlich inaktiv
   Dz2D(1:2) = transfer_quantity_p(61+(i-1)*number_trans_quant) ! vertikalen Dispersionskoeffizient aus k_eps()
   ! vorsichtshalber:
   Dz2D(1:2) = 0.0
   sedAlg_MQ(1,1) = benthic_distribution_p(52+(i-1)*number_benth_distr) ! ?? wird aus sedflux kommen
   sedAlg_MQ(1,2) = sedAlg_MQ(1,1)
   if (control) print*,'vor algaeski: sedAlg_MQ = ', sedAlg_MQ(1,1)
   do j = 1,num_lev !
      hQ_NKz(1,j,1) = plankt_vari_vert_p(j+(17-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Stickstoffanteil der Algenbiomasse kiesel
      hQ_NKz(1,j,2) = hQ_NKz(1,j,1)
      hQ_NGz(1,j,1) = plankt_vari_vert_p(j+(18-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! grün
      hQ_NGz(1,j,2) = hQ_NGz(1,j,1)
      hQ_NBz(1,j,1) = plankt_vari_vert_p(j+(19-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! blau
      hQ_NBz(1,j,2) = hQ_NBz(1,j,1)
   enddo
   ! Für die Berechnung des Chlorophyll-a/Kohlenstoff-Verhaeltnisses
   ! Angabe in mgChla/mgC  ; in ini_algae() gesetzt in . Variablendefinition in module QSimDatenfelder
   
   EXTKS(1,1:2) = zone(point_zone(iglob))%seditemp%extiks ! Extinktionskoeffizient für PARS ((nicht mehr)nur bei Temperaturmodellierung erforderlich!)
   akmor_1(1,1:2) = planktonic_variable_p(77+nk) ! ?? Kiesel-Algen
   agmor_1(1,1:2) = planktonic_variable_p(78+nk) ! ?? Gruen-Algen
   abmor_1(1,1:2) = planktonic_variable_p(79+nk) ! ?? Blau-Algen
   mstr = 1        ! Strangzähler | nur ein Profil in einem Strang
   it_h(1,1) = 1   ! ???
   it_h(1,2) = it_h(1,1)
   itags = tag     ! Tag im Monat module::modell zeitsekunde()
   monats = monat  ! Monat im Jahr module::modell zeitsekunde()
   isim_end = 0
   if (control) print*,'vor algaes**: up_Siz = ',up_Siz(1,1)
   if (control) print*,'vor algaes**: chlaki,chlagr,chlabl = ',chlaki(1),chlagr(1),chlabl(1)
   if (control) print*,'vor algaes**: aki,agr,abl = ',aki(1),agr(1),abl(1)
   if (control) print*,'vor algaeski: extk,EXTKS = ',extk(1:2),EXTKS(1,1:2)
   if (control) print*,'vor algaes**: schwi(1),CHLA,up_NKz = ',schwi(1),CHLA(1),up_NKz(1,1)
   if (control) print*,'vor algaes**: Q_NK,hQ_NKz,akbcm,hCChlgz = ',Q_NK(1),hQ_NKz(1,1,1),akbcm(1),hCChlgz(1,1,1)
   if (control) print*,'vor algaes**: algzok,zooind,GROT = ',algzok(1),zooind(1),GROT
   if (control) print*,'vor algaeski: svhemk = ',svhemk(1),extk(1)
   if (control) print*,'vor algaes**: akmor_1,agmor_1,abmor_1 = ',akmor_1(1,1),agmor_1(1,1),abmor_1(1,1)
   do k = 1,number_benth_distr
      if (isnan(benthic_distribution_p(k+(i-1)*number_benth_distr))) then
         print*,'vor algaes**: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
         if (meinrang == 0)print*,'benth_distr_name:',benth_distr_name(k)
      endif
   enddo
   
   ! ==========================================================================
   ! metabolism
   ! ==========================================================================
   ! Algenaufruf in genau der Reihenfolge Kiesel, Blau, Grün
   ! Grünalgen zuletzt, weil dort zusammengefasst wird   sonst nachfolgende zusammenfassung:
   
   call algaeski(schwi, tflie, tempw, tempwz, rau, tiefe, vmitt, vno3, vnh4,   &
                 gelp, svhemk, svhemb, svhemg, chla, si, dalgki, dalgak, flag, &
                 elen, anze, sedalk, algzok, echla, qeinl, vabfl, dkimor,      &
                 jiein, evkigr, vkigr, antbl, eantbl, saettk, akbcm, abbcm,    &
                 agbcm, aki, abl, agr, extk, extk_lamda, ilamda, eta, aw, ack, &
                 acg, acb, ah, as, al, sised, tpki, iwied, akmuea, ftaaus,     &
                 fiaus, fheaus, akraus, tauscs, ischif, ilbuhn, cmatki, algdrk,&
                 algcok, zooind, grot, ss, q_pk, q_nk, q_sk, vnh4z, vno3z,     &
                 gelpz, siz, dalgkz, nkzs, dh2d, up_pkz, up_nkz, up_siz, skmor,&
                 akitbr, chlaz, akibrz, akiz, algakz, algzkz, ablz, agrz,      &
                 chlaki, hchlkz, hchlgz, hchlbz, hcchlkz, hcchlbz, hcchlgz,    &
                 dz2d, chlabl, chlagr, a1ki, sedalg_mq, sedalk0, hq_nkz,       &
                 hq_ngz, hq_nbz, q_pg, q_ng, q_pb, q_nb, mstr, it_h, isim_end, &
                 extks, akmor_1, agmor_1, abmor_1,                             &
                 control, iglob)
   
   call algaesbl(schwi, tflie, tempw, rau, tiefe, vmitt, vno3, vnh4, gelp,     &
                 svhemb, chla, dalgbl, dalgab, anze, sedalb, algzob,           & 
                 dblmor, saettb,                                               &
                 vkigr, abbcm, abl, tpbl, fibaus, abmuea, fhebas, abreau,      &
                 tauscs, ischif, ilbuhn, ieros, algdrb, algcob,                &
                 antbl, extk, extk_lamda, ilamda, ack, acg, acb, al,           &
                 vnh4z, vno3z, gelpz, dalgbz, nkzs, dh2d, tempwz, up_pbz,      &
                 up_nbz,                                                       &
                 q_nb, q_pb,  abltbr, ablbrz, up_n2z, ablz,                    &
                 chlabl, a1bl, hchlbz, hcchlbz, algabz,                        &
                 algzbz, dz2d,  sedalg_mq,                                     &
                 sedalb0, hq_nbz, mstr, isim_end, abmor_1,                     &
                 control, iglob)
   
   call algaesgr(schwi, tflie, tempw, rau, tiefe, vmitt, vno3, vnh4, gelp,     &
                 svhemg, chla, dalggr, dalgag, anze, sedalg, algzog, dgrmor,   &
                 vkigr, chlaki, chlagr, algdrg, agbcm, agr, cmatgr, antbl,     &
                 chlabl, extk, extk_lamda, ilamda, eta, aw, ack, acg, acb, ah, &
                 as, al, tpgr, algcog, figaus, agmuea, fhegas, agreau, tauscs, &
                 ischif, ilbuhn, q_pg, q_ng, vnh4z, vno3z, gelpz, dalggz, nkzs,&
                 dh2d, tempwz, mstr, up_pgz, up_ngz, agrtbr, agrbrz, agrz,     &
                 chlaz, hchlkz, hchlgz, hchlbz, hcchlgz, algagz, algzgz, dz2d, &
                 sedalg_mq, sedalg0, hq_ngz, a1gr,  isim_end, agmor_1,         &
                 control, iglob)
   
   ! vkigr(ior) = chlaki(ior)/(chlagrt+chlaki(ior)+chlabl(ior))
   ! antbl(ior) = chlabl(ior)/(chlagrt+chlaki(ior)+chlabl(ior))
   ! if(control) print*,'nach vorläufiger Ersatz: chla,chlaki,chlabl,chlagr= ',chla(1),chlaki(1),chlabl(1),chlagr(1)
   ! chla(1) = chlaki(1) + chlabl(1) + chlagr(1) !!!### = chlaki(ior) + chlabl(ior) +chlagrt
   ! if(chla(1).le. 0.0) then
   !    write(fehler,*)'algae_huelle chla .le. 0.0 am Knoten #',iglob
   !    call qerror(fehler)
   ! endif
   ! vkigr(1) = chlaki(1)/chla(1)
   ! antbl(1) = chlabl(1)/chla(1)
   if (control) print*,'nach algaes**: up_Siz = ',up_Siz(1,1)
   if (control) print*,'nach algaes**: chlaki,chlagr,chlabl = ',chlaki(1),chlagr(1),chlabl(1)
   if (control) print*,'nach algaes**: aki,agr,abl = ',aki(1),agr(1),abl(1)
   if (control) print*,'nach algaesgr: vkigr(1),chlaki(1),chla(1) = ',vkigr(1),chlaki(1),chla(1)
   if (control) print*,'nach algaesgr: svhemk,svhemg,svhemb = ',svhemk(1),svhemg(1),svhemb(1)
   if (control) print*,'nach algaesgr: extk = ',extk(1)
   if (control) print*,'nach algaesgr: agbcm, hchlgz, hCChlgz = ', agbcm(1), hchlgz(1,1,1), hCChlgz(1,1,1)
   if (control) print*,'nach algaes**: akmor_1,agmor_1,abmor_1 = ',akmor_1(1,1),agmor_1(1,1),abmor_1(1,1)
   if (control) print*,'nach algaes**: up_NKz = ',up_NKz(1,1)
   if (control) print*,'nach algaes**:hchlkz hchlgz hchlbz hCChlgz hCChlkz hCChlbz = ' &
       ,hchlkz(1,1,1), hchlgz(1,1,1), hchlbz(1,1,1), hCChlgz(1,1,1), hCChlkz(1,1,1), hCChlbz(1,1,1)
   778 continue
   
   ! ==========================================================================
   ! Datenrückgabe
   ! ==========================================================================
   planktonic_variable_p(21+nk) = svhemk(1)
   planktonic_variable_p(22+nk) = svhemg(1)
   planktonic_variable_p(23+nk) = svhemb(1)
   planktonic_variable_p(11+nk) = CHLA(1)    ! Chlorophyl-A tiefengemittelt
   planktonic_variable_p(12+nk) = Chlaki(1)  ! Chlorophyl in Kiesel-Algen muegchla/l
   planktonic_variable_p(13+nk) = chlagr(1)  ! Chlorophyl in Gruenalgen muegchla/l
   planktonic_variable_p(14+nk) = chlabl(1)  ! Chlorophyl in Blaualgen muegchla/l
   transfer_quantity_p(20+(i-1)*number_trans_quant) = dalgki(1) ! Zuwachs Kiesel-Algen
   transfer_quantity_p(21+(i-1)*number_trans_quant) = dalggr(1) ! Zuwachs grün-Algen
   transfer_quantity_p(22+(i-1)*number_trans_quant) = dalgbl(1) ! Zuwachs blau-Algen
   transfer_quantity_p(23+(i-1)*number_trans_quant) = dalgak(1) ! Respiration Kiesel-Algen
   transfer_quantity_p(24+(i-1)*number_trans_quant) = dalgag(1) ! Respiration grün-Algen
   transfer_quantity_p(25+(i-1)*number_trans_quant) = dalgab(1) ! Respiration blau-Algen
   benthic_distribution_p(26+(i-1)*number_benth_distr) = sedalk(1) ! Sedimentierte Menge an Kiesel-Algen
   benthic_distribution_p(27+(i-1)*number_benth_distr) = sedalg(1) ! Sedimentierte Menge an Grün-Algen
   benthic_distribution_p(28+(i-1)*number_benth_distr) = sedalb(1) ! Sedimentierte Menge an Blau-Algen
   transfer_quantity_p(7+(i-1)*number_trans_quant) = dkimor(1) ! Absterberate Kieselalgen
   transfer_quantity_p(8+(i-1)*number_trans_quant) = dgrmor(1) ! Absterberate Grünalgen
   transfer_quantity_p(9+(i-1)*number_trans_quant) = dblmor(1) ! Absterberate Blaualgen
   planktonic_variable_p(19+nk) = vkigr(1) ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ??
   planktonic_variable_p(20+nk) = antbl(1) ! Anteil der Blaualgen am Gesamt-Chlorophyll-a
   transfer_value_p(6) = saettk    ! Rückgabewert ???
   transfer_value_p(8) = saettg    ! ???
   transfer_value_p(9) = saettb    ! ???
   planktonic_variable_p(24+nk) = akbcm(1) ! Verhältnis Chlorophyll-a zu Kohlenstoff Kieselalgen
   planktonic_variable_p(25+nk) = agbcm(1) ! Verhältnis Chlorophyll-a zu Kohlenstoff Gruenalgen
   planktonic_variable_p(26+nk) = abbcm(1) ! Verhältnis Chlorophyll-a zu Kohlenstoff der blau-Algen
   planktonic_variable_p( 8+nk) = aki(1) ! Biomasse Kiesel-Algen
   planktonic_variable_p( 9+nk) = agr(1) ! Trockenmasse Gruen-Algen
   planktonic_variable_p(10+nk) = abl(1) ! Trockenmasse Blau-Algen
   transfer_quantity_p(54+(i-1)*number_trans_quant) = extk(1) ! mittlerer Extinktionskoeffizient
   if (extk(1) <= 0.0)   print*,'algae_huelle extk(1+2) <= 0.0 ,extkS, iglob',extk(1),extk(2),extkS(1,1),iglob
   ! extk_lamda wird nur von algaeski an algaesbl und algaesgr übergeben. an jedem Knoten hier in algae_huelle
   !do j=1,ilamda
   !   rb_extnct_p(9 + (j-1)*anz_extnct_koeff)= extk_lamda(j,1) ! ??? an allen Knoten auf allen Prozessen immer derselbe Wert??
   !enddo
   benthic_distribution_p(2+(i-1)*number_benth_distr) = sised(1) ! Siliziumgehalt im Sediment (wird von algaeski verändert)
   transfer_quantity_p(55+(i-1)*number_trans_quant) = tpki(1) ! Ausgabeparameter ?? Kieselalgen Phosphor ??
   transfer_quantity_p(80+(i-1)*number_trans_quant) = tpgr(1) ! Ausgabeparameter ?? Grünalgen Phosphor ??
   transfer_quantity_p(81+(i-1)*number_trans_quant) = tpbl(1) ! Ausgabeparameter ?? Blaualgen Phosphorhemmung ??
   transfer_quantity_p(56+(i-1)*number_trans_quant) = akmuea(1) ! Ausgabeparameter algaeski()
   transfer_quantity_p(84+(i-1)*number_trans_quant) = agmuea(1) ! Wachstumsrate Ausgabeparameter algaesgr()
   transfer_quantity_p(85+(i-1)*number_trans_quant) = abmuea(1) ! Wachstumsrate Ausgabeparameter algaesbl()
   transfer_quantity_p(57+(i-1)*number_trans_quant) = ftaaus(1) ! Ausgabeparameter algaeski() fta
   transfer_quantity_p(58+(i-1)*number_trans_quant) = fiaus(1)  ! Ausgabeparameter algaeski() Pmit/(Pmax*3.6)
   transfer_quantity_p(82+(i-1)*number_trans_quant) = figaus(1) ! Ausgabeparameter algaesgr() Pmit/(Pmax*3.6)
   transfer_quantity_p(83+(i-1)*number_trans_quant) = fibaus(1) ! Ausgabeparameter algaesbl() Pmit/(Pmax*3.6)
   transfer_quantity_p(59+(i-1)*number_trans_quant) = fheaus(1) ! Ausgabeparameter algaeski() svhemk
   transfer_quantity_p(86+(i-1)*number_trans_quant) = fhegas(1) ! Ausgabeparameter algaesi() svhemg
   transfer_quantity_p(87+(i-1)*number_trans_quant) = fhebas(1) ! Ausgabeparameter algaesbl() svhemb
   transfer_quantity_p(60+(i-1)*number_trans_quant) = akraus(1) ! Ausgabeparameter algaeski() F53
   transfer_quantity_p(88+(i-1)*number_trans_quant) = agreau(1) ! Ausgabe agbcm algaesgr()
   transfer_quantity_p(89+(i-1)*number_trans_quant) = abreau(1) ! Ausgabe von abbcm (Chlorophyll-a zu Kohlenstoff der blau-Algen) algaesbl()
   planktonic_variable_p(30+nk) = Q_NK(1) ! Stickstoffanteil der Algenbiomasse kiesel
   planktonic_variable_p(31+nk) = Q_PK(1) ! Phosphoranteil der Kiesel-Algenbiomasse
   planktonic_variable_p(32+nk) = Q_SK(1) ! Siliziumgehalt Kieselalgen
   planktonic_variable_p(33+nk) = Q_NG(1) ! Stickstoffanteil der Algenbiomasse grün
   planktonic_variable_p(34+nk) = Q_PG(1) ! Phosphoranteil der grün-Algenbiomasse
   planktonic_variable_p(35+nk) = Q_NB(1) ! Stickstoffanteil der Algenbiomasse blau
   planktonic_variable_p(36+nk) = Q_PB(1) ! Phosphoranteil der blau-Algenbiomasse
   planktonic_variable_p(77+nk) = akmor_1(1,1) ! ?? Kiesel-Algen
   planktonic_variable_p(78+nk) = agmor_1(1,1) ! ?? Gruen-Algen
   planktonic_variable_p(79+nk) = abmor_1(1,1) ! ?? Blau-Algen
   do j = 1,num_lev
      trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = dalgkz(j,1) ! Zuwachs Kieselalgen tiefenaufgelöst
      trans_quant_vert_p(j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = dalggz(j,1) ! Zuwachs Kieselalgen tiefenaufgelöst
      trans_quant_vert_p(j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = dalgbz(j,1) ! Zuwachs Kieselalgen tiefenaufgelöst
   enddo
   do j = 1,num_lev
      trans_quant_vert_p(j+(1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_NKz(j,1) ! N (Stickstoff) Aufnahmerate der Kiesel-Algen
      trans_quant_vert_p(j+(2-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_NGz(j,1) ! N (Stickstoff) Aufnahmerate der grün-Algen
      trans_quant_vert_p(j+(3-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_NBz(j,1) ! N (Stickstoff) Aufnahmerate der blau-Algen
      trans_quant_vert_p(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_Siz(j,1) ! Si-Aufnahmerate der Kiesel-Algen
      trans_quant_vert_p(j+(5-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_PKz(j,1) ! P (Phosphor) Aufnahmerate der Kiesel-Algen
      trans_quant_vert_p(j+(6-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_PGz(j,1) ! P (Phosphor) Aufnahmerate der grün-Algen
      trans_quant_vert_p(j+(7-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_PBz(j,1) ! P (Phosphor) Aufnahmerate der blau-Algen
   enddo
   planktonic_variable_p(69+nk) = Skmor(1) ! Silizium in schwebenden, abgestorbenen Kieselalgen
   transfer_quantity_p(48+(i-1)*number_trans_quant) = akitbr(1) ! Kieselalgen ??
   transfer_quantity_p(49+(i-1)*number_trans_quant) = agrtbr(1) !
   transfer_quantity_p(50+(i-1)*number_trans_quant) = abltbr(1) ! Zwischengröße Algenbiomasse ???
   do j = 1,num_lev
      plankt_vari_vert_p(j+(11-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = chlaz(j,1) ! Chlorophyl-A tiefenaufgelöst
   enddo
   do j = 1,num_lev_trans
      trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = akibrz(j,1) ! Kiesel-Algen-Biomasse? Wachstum? tiefenaufgelöst
      trans_quant_vert_p(j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = agrbrz(j,1) ! Zwischengröße grün-Algen-Biomasse ?
      trans_quant_vert_p(j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = ablbrz(j,1) ! Zwischengröße blau-Algen-Biomasse ?
      trans_quant_vert_p(j+(8-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_N2z(j,1) !  Aufnahmerate von Luftstickstoff durch Blaualgen
   enddo
   do j = 1,num_lev
      plankt_vari_vert_p(j+( 8-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = akiz(j,1) ! Biomasse kiesel-Algen tiefenaufgelöst
      plankt_vari_vert_p(j+( 9-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = agrz(j,1) ! Biomasse gruen-Algen tiefenaufgelöst
      plankt_vari_vert_p(j+(10-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = ablz(j,1) ! Biomasse blau-Algen tiefenaufgelöst
   enddo
   do j = 1,num_lev_trans
      trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algakz(j,1) !  Respirierte Kiesel-Algenbiomasse, tiefenaufgelöst
      trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algagz(j,1) !  Respirierte grün-Algenbiomasse,
      trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algabz(j,1) !  Respirierte blau-Algenbiomasse
      trans_quant_vert_p(j+(26-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzkz(j,1) ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l
      trans_quant_vert_p(j+(27-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzgz(j,1) ! grün-Algen-Konsum durch Zoo-Plankton in mg/l
      trans_quant_vert_p(j+(28-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzbz(j,1) ! blau-Algen-Konsum durch Zoo-Plankton in mg/l
   enddo
   do j = 1,num_lev
      plankt_vari_vert_p(j+(12-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hchlkz(1,j,1) ! Chlorophylgehalt der Kieselalgen
      plankt_vari_vert_p(j+(13-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hchlgz(1,j,1) ! Chlorophylgehalt der Gruenalgen
      plankt_vari_vert_p(j+(14-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hchlbz(1,j,1) ! Chlorophylgehalt der Blaualgen
      plankt_vari_vert_p(j+(20-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hCChlkz(1,j,1) ! c-chla Verhältnis Kiesel
      plankt_vari_vert_p(j+(21-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hCChlgz(1,j,1) ! c-chla Verhältnis grün
      plankt_vari_vert_p(j+(22-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hCChlbz(1,j,1) ! c-chla Verhältnis blau
   enddo
   benthic_distribution_p(52+(i-1)*number_benth_distr) = sedAlg_MQ(1,1) ! ??
   do j = 1,num_lev
      plankt_vari_vert_p(j+(17-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hQ_NKz(1,j,1) ! Stickstoffanteil der Algenbiomasse kiesel
      plankt_vari_vert_p(j+(18-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hQ_NGz(1,j,1) ! grün
      plankt_vari_vert_p(j+(19-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hQ_NBz(1,j,1) ! blau
   enddo
   benthic_distribution_p(53+(i-1)*number_benth_distr) = sedAlk0(1) ! !sedAlk0 wird nur an k_eps() übergeben.
   
   
   ! ==========================================================================
   ! Kontrolle und Prüfung auf NaN
   ! ==========================================================================
   if (num_lev_trans > nkzs(1)) call qerror('algae_huelle Tiefenschichtung trans_quant_vert passt nicht')

   do k = 1,number_plankt_vari
      if (isnan(planktonic_variable_p(k+nk))) then
         print*,'nach algaes**: isnan(planktonic_variable_p  node#',iglob,' variable# ',k
         if (meinrang == 0)print*,'planktonic_variable_name:',planktonic_variable_name(k)
      endif
   enddo
   do j = 1,num_lev
      do k = 1,number_plankt_vari_vert
         if (isnan(plankt_vari_vert_p(j+(k-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev))) then
            print*,'nach algaes**: isnan(plankt_vari_vert_p  node#',iglob,' level#', j,' variable# ',k
            if (meinrang == 0)print*,'plankt_vari_vert_name:',plankt_vari_vert_name(k)
         endif
      enddo
   enddo
   do k = 1,number_trans_quant
      if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
         print*,'nach algaes**: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
         if (meinrang == 0)print*,'trans_quant_name:',trans_quant_name(k)
      endif
   enddo
   do j = 1,num_lev_trans
      do k = 1,number_trans_quant_vert
         if (isnan(trans_quant_vert_p(j+(k-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert))) then
            print*,'nach algaes**: isnan(trans_quant_vert_p  node#',iglob,' level#', j,' variable# ',k
            if (meinrang == 0)print*,'trans_quant_vert_name:',trans_quant_vert_name(k)
         endif
      enddo
   enddo
   do k = 1,number_benth_distr
      if (isnan(benthic_distribution_p(k+(i-1)*number_benth_distr))) then
         print*,'nach algaes**: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
         if (meinrang == 0)print*,'benth_distr_name:',benth_distr_name(k)
      endif
   enddo

   return
end subroutine algae_huelle



!> SUBROUTINE algae_aufteilung()
!!\n zurück: \ref lnk_algenaufteilung_zufluss ; Code: algae_huelle.f95
subroutine algae_aufteilung(i)!! ### ERSETZT
   use modell
   use QSimDatenfelder
   use module_aparam
   implicit none
   integer i,nk,l
   real T, Te0, Sum_N, f_NK
   real CChl0k,CChl0g,CChl0b! Version 13.30.01
   if (meinrang /= 0)call qerror("algae_aufteilung darf nur auf Prozessor 0 laufen")
   nk = (i-1)*number_plankt_vari
   !!    Ausgangspunkt sind die Angaben:
   !planktonic_variable(11+nk) = rabe(zaehl)%wert_jetzt(13)  !  CHLA   Chorophyl-a
   !planktonic_variable(19+nk) = rabe(zaehl)%wert_jetzt(14)  !  VKIGR  Anteil Kieselalgen (falls unbekannt 0)
   !planktonic_variable(20+nk) = rabe(zaehl)%wert_jetzt(15)  !  ANTBL  Anteil Blaualgen (falls unbekannt 0)
   !....ag(k,b)chl gilt für 20°C  in mgC/mgChla
   Te0 = 20.
   T = planktonic_variable(1+nk) ! Wassertemperatur
   CChl0k = akchl * exp(-a1Ki * Te0)              ! C:Chla bei 0°C für Kieselalgen mgC/mgChla
   CChl0g = agchl * exp(-a1Gr * Te0)              ! Grünalgen
   CChl0b = abchl * exp(-a1Bl * Te0)              ! Blaualgen
   !...temperaturabhängiges des C:Chla-Verhältnisses
   planktonic_variable(24+nk) = CChl0k * exp(a1Ki * T) ! akbcm
   planktonic_variable(25+nk) = CChl0g * exp(a1Gr * T) ! agbcm
   planktonic_variable(26+nk) = CChl0b * exp(a1Bl * T) ! abbcm
   ! mg Algenbiomasse, Chla in µg/l
   planktonic_variable(12+nk) = &   ! chlaki = chla * vkigr
                                planktonic_variable(11+nk)*planktonic_variable(19+nk)
   planktonic_variable(13+nk) = &   ! chlagr = chla * (1-vkigr-antbl)
                                planktonic_variable(11+nk)*(1.-planktonic_variable(19+nk)-planktonic_variable(20+nk))
   planktonic_variable(14+nk) = &   ! chlabl = chla * antbl
                                planktonic_variable(11+nk)*planktonic_variable(20+nk)
   !      akis(mstr,mRB) = (chlas(mstr,mRB)*vkigrs(mstr,mRB)/1000.)*(akbcms(mstr,mRB)/Caki)
   planktonic_variable( 8+nk) = ( planktonic_variable(12+nk)*planktonic_variable(24+nk) ) / (1000.0*Caki)
   !     &   planktonic_variable(11+nk)*planktonic_variable(19+nk)     & ! aki = (chla*VKIGR)/(1000*akbcm*Caki)
   !     &   /(1000.*planktonic_variable(24+nk)*Caki)
   do l = 1,num_lev ! akiz konstante Verteilung in der Vertikalen
      plankt_vari_vert(l+(8-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(8+nk)
   enddo ! alle l levels
   !      agrs(mstr,mRB) = (chlas(mstr,mRB)*(1.-vkigrs(mstr,mRB)-antbls(mstr,mRB))/1000.)*(agbcms(mstr,mRB)/Cagr)
   planktonic_variable(9+nk) = ( planktonic_variable(13+nk)*planktonic_variable(25+nk) ) / (1000.0*Cagr)
   !     &   planktonic_variable(11+nk)*(1.-planktonic_variable(19+nk)-planktonic_variable(20+nk)) &
   !     &   /(1000.*planktonic_variable(25+nk)*Cagr) ! agr = (chla*(1-VKIGR-ANTBL))/(1000*agbcm*Cagr)
   do l = 1,num_lev ! ablz konstante Verteilung in der Vertikalen
      plankt_vari_vert(l+(9-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(9+nk)
   enddo ! alle l levels
   
   !      abls(mstr,mRB) = (chlas(mstr,mRB)*antbls(mstr,mRB)/1000.)*(abbcms(mstr,mRB)/Cabl)
   planktonic_variable(10+nk) = ( planktonic_variable(14+nk)*planktonic_variable(26+nk) ) / (1000.0*Cabl)
   !     &   planktonic_variable(11+nk)*planktonic_variable(20+nk) &
   !     &   /(1000.*planktonic_variable(26+nk)*Cabl)  ! abl = (chla*ANTBL)/(1000*abbcm*Cabl)
   do l = 1,num_lev ! agrz konstante Verteilung in der Vertikalen
      plankt_vari_vert(l+(10-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(10+nk)
   enddo ! alle l levels
   !....zelluläre Nährstoffgehalte in den Algen
   !      Sum_N = vnh4s(mstr,mRB)+vNO3s(mstr,mRB)
   !      f_NK = 0.582*(Sum_N/(Sum_N+0.011))
   !      f_NK = 1.
   !      Q_NKs(mstr,mRB) = Qmx_NK-(1.-f_NK)*(Qmx_NK-Qmn_NK)
   planktonic_variable(30+nk) = Qmx_NK ! Stickstoffanteil der Algenbiomasse kiesel
   !      Q_PKs(mstr,mRB) = Qmn_PK+(Qmx_PK-Qmn_PK)/1.
   planktonic_variable(31+nk) = Qmx_PK ! Phosphoranteil der Kiesel-Algenbiomasse
   !      Q_SKs(mstr,mRB) = Qmx_SK
   planktonic_variable(32+nk) = Qmx_SK  ! Siliziumgehalt Kieselalgen
   !      Q_NGs(mstr,mRB) = Qmn_NG+(Qmx_NG-Qmn_NG)/1.
   planktonic_variable(33+nk) = Qmx_NG ! Stickstoffanteil der Algenbiomasse grün
   !      Q_PGs(mstr,mRB) = Qmn_PG+(Qmx_PG-Qmn_PG)/1.
   planktonic_variable(34+nk) = Qmx_PG ! Phosphoranteil der grün-Algenbiomasse
   !      Q_NBs(mstr,mRB) = Qmn_NB+(Qmx_NB-Qmn_NB)/1.
   planktonic_variable(35+nk) = Qmx_NB ! Stickstoffanteil der Algenbiomasse blau
   !      Q_PBs(mstr,mRB) = Qmn_PB+(Qmx_PB-Qmn_PB)/1.
   planktonic_variable(36+nk) = Qmx_PB ! Phosphoranteil der blau-Algenbiomasse
   
   planktonic_variable(21+nk) = 0.01 ! svhemk ### unklar ###
   planktonic_variable(22+nk) = 0.01 ! svhemg ### unklar ###
   planktonic_variable(23+nk) = 0.01 ! svhemb ### unklar ###
   planktonic_variable(27+nk) = 0.01 ! akiiv ### unklar ###
   planktonic_variable(28+nk) = 0.01 ! agriv ### unklar ###
   planktonic_variable(29+nk) = 0.01 ! abliv ### unklar ###
   return
end subroutine algae_aufteilung
