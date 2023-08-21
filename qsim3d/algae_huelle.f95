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

!> SUBROUTINE algae_huelle()
!!
!! zurück: \ref index oder \ref lnk_phytoplankton
!! Code: algae_huelle.f95
subroutine algae_huelle(i)
   use modell
   use QSimDatenfelder
   use module_aparam
   implicit none
   
   integer, intent(in) :: i
   integer             :: j, k, nk, nt, nb, nl, nlt
     
   
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
   
   nk = (i-1) * number_plankt_vari
   nt = (i-1) * number_trans_quant
   nb = (i-1) * number_benth_distr
   
   control = iglob == kontrollknoten
   
   if (control) print '(a,*(i0))', 'algae_huelle: nk,i,iglob = ', nk, i, iglob
   
   ! ==========================================================================
   ! Datenübergabe
   ! ==========================================================================
   tflie = real(deltat)/86400. 
   
   tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul)           ! Wassertiefe aus randbedingungen.h
   vmitt(1:2) = rb_hydraul_p(1+(i-1)*number_rb_hydraul)           ! Geschwindigkeitsbetrag; randbedingungen.h
   
   rau(1:2)     = strickler(zone(point_zone(iglob))%reib, tiefe(1))               ! Strickler Reibungsbeiwert
   schwi(1:2)   = schwi_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer) ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
   ischif       = zone(point_zone(iglob))%schiff%schifffahrts_zone                ! schifffahrt 
   extks(1,1:2) = zone(point_zone(iglob))%seditemp%extiks ! Extinktionskoeffizient für PARS
   
   tempw(1:2)     = planktonic_variable_p(1+nk)  ! Wassertemperatur                  
   vnh4(1:2)      = planktonic_variable_p( 3+nk) ! Ammonium
   vno3(1:2)      = planktonic_variable_p( 5+nk) ! Nitrat
   gelp(1:2)      = planktonic_variable_p( 6+nk) ! gelöster ortho-Phosphat-Phosphor tiefengemittelt
   si(1:2)        = planktonic_variable_p( 7+nk) ! Silikat-Silizium-Konzentration (tiefengemittelt)
   aki(1:2)       = planktonic_variable_p( 8+nk) ! Biomasse an Kiesel-Algen
   agr(1:2)       = planktonic_variable_p( 9+nk) ! Biomasse an gruen-Algen
   abl(1:2)       = planktonic_variable_p(10+nk) ! Biomasse an Blau-Algen
   chla(1:2)      = planktonic_variable_p(11+nk) ! Chlorophyl-A
   chlaki(1:2)    = planktonic_variable_p(12+nk) ! Chlorophyl in Kieselalgen muegchla/l
   chlagr(1:2)    = planktonic_variable_p(13+nk) ! Chlorophyl in gruenalgen muegchla/l
   chlabl(1:2)    = planktonic_variable_p(14+nk) ! Chlorophyl in Blaualgen muegchla/l
   vkigr(1:2)     = planktonic_variable_p(19+nk) ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ?
   antbl(1:2)     = planktonic_variable_p(20+nk) ! Anteil der Blaualgen am Gesamt-Chlorophyll-a
   svhemk(1:2)    = planktonic_variable_p(21+nk) ! Mittelwertbildung der Lichthemmung ?
   svhemg(1:2)    = planktonic_variable_p(22+nk) ! Mittelwertbildung der Lichthemmung ?
   svhemb(1:2)    = planktonic_variable_p(23+nk) ! Mittelwertbildung der Lichthemmung ?
   akbcm(1:2)     = planktonic_variable_p(24+nk) ! Verhältnis Chlorophyll-a zu Kohlenstoff Kieselalgen
   agbcm(1:2)     = planktonic_variable_p(25+nk) ! Verhältnis Chlorophyll-a zu Kohlenstoff Gruenalgen
   abbcm(1:2)     = planktonic_variable_p(26+nk) ! Verhältnis Chlorophyll-a zu Kohlenstoff der blau-Algen
   q_pk(1:2)      = planktonic_variable_p(31+nk) ! Phosphoranteil der Kiesel-Algenbiomasse
   q_nk(1:2)      = planktonic_variable_p(30+nk) ! Stickstoffanteil der Algenbiomasse kiesel
   q_sk(1:2)      = planktonic_variable_p(32+nk) ! Siliziumgehalt Kieselalgen
   q_ng(1:2)      = planktonic_variable_p(33+nk) ! Stickstoffanteil der Algenbiomasse grün
   q_pg(1:2)      = planktonic_variable_p(34+nk) ! Phosphoranteil der grün-Algenbiomasse
   q_nb(1:2)      = planktonic_variable_p(35+nk) ! Stickstoffanteil der Algenbiomasse blau
   q_pb(1:2)      = planktonic_variable_p(36+nk) ! Phosphoranteil der blau-Algenbiomasse
   zooind(1:2)    = planktonic_variable_p(50+nk) ! Anzahl der Rotatorien in Ind/l
   ssalg(1:2)     = planktonic_variable_p(52+nk) ! Gesamtschwebstoffe incl. lebender Organismen, messbar, Randwert
   ss(1:2)        = planktonic_variable_p(53+nk) ! ORG. UND ANORG. SCHWEBSTOFFE (OHNE ALGEN UND ZOOPLANKTER)
   skmor(1:2)     = planktonic_variable_p(69+nk) ! Silizium in schwebenden, abgestorbenen Kieselalgen
   akmor_1(1,1:2) = planktonic_variable_p(77+nk) ! Kiesel-Algen?
   agmor_1(1,1:2) = planktonic_variable_p(78+nk) ! Gruen-Algen?
   abmor_1(1,1:2) = planktonic_variable_p(79+nk) ! Blau-Algen?
   
   dkimor(1:2) = transfer_quantity_p( 7+nt) ! Absterberate Kieselalgen
   dgrmor(1:2) = transfer_quantity_p( 8+nt) ! Absterberate Grünalgen
   dblmor(1:2) = transfer_quantity_p( 9+nt) ! Absterberate Blaualgen
   dalgki(1:2) = transfer_quantity_p(20+nt) ! Zuwachs Kieselalgen
   dalggr(1:2) = transfer_quantity_p(21+nt) ! Zuwachs Grünalgen
   dalgbl(1:2) = transfer_quantity_p(22+nt) ! Zuwachs Blaualgen
   dalgak(1:2) = transfer_quantity_p(23+nt) ! Respiration Kieselalgen
   dalgag(1:2) = transfer_quantity_p(24+nt) ! Respiration Grünalgen
   dalgab(1:2) = transfer_quantity_p(25+nt) ! Respiration Blaualgen
   akitbr(1:2) = transfer_quantity_p(48+nt) ! Kieselalgen?
   agrtbr(1:2) = transfer_quantity_p(49+nt) !
   abltbr(1:2) = transfer_quantity_p(50+nt) ! Zwischengröße Algenbiomasse?
   algzok(1:2) = transfer_quantity_p(53+nt) ! diatoms consumed by zooplankton [mg/l]
   extk(1:2)   = transfer_quantity_p(54+nt) ! mittlerer Extinktionskoeffizient
   tpki(1:2)   = transfer_quantity_p(55+nt) ! Ausgabeparameter ? Kieselalgen Phosphor?
   akmuea(1:2) = transfer_quantity_p(56+nt) ! Wachstumsrate Ausgabeparameter algaeski()
   ftaaus(1:2) = transfer_quantity_p(57+nt) ! Ausgabeparameter algaeski() fta
   fiaus(1:2)  = transfer_quantity_p(58+nt) ! Ausgabeparameter algaeski() Pmit/(Pmax*3.6)
   fheaus(1:2) = transfer_quantity_p(59+nt) ! Ausgabeparameter algaeski() svhemk
   akraus(1:2) = transfer_quantity_p(60+nt) ! Ausgabe akbcm algaeski()
   algzog(1:2) = transfer_quantity_p(72+nt) ! green algae consumed by zooplankton [mg/l]
   algzob(1:2) = transfer_quantity_p(73+nt) ! cyanobacteria consumed by zooplankton [mg/l]
   tpgr(1:2)   = transfer_quantity_p(80+nt) ! Ausgabeparameter ? Grünalgen Phosphor?
   tpbl(1:2)   = transfer_quantity_p(81+nt) ! Ausgabeparameter ? Blaualgen Phosphorhemmung?
   figaus(1:2) = transfer_quantity_p(82+nt) ! Ausgabeparameter algaesgr() Pmit/(Pmax*3.6)
   fibaus(1:2) = transfer_quantity_p(83+nt) ! Ausgabeparameter algaesbl() Pmit/(Pmax*3.6)
   agmuea(1:2) = transfer_quantity_p(84+nt) ! Wachstumsrate Ausgabeparameter algaesgr()
   abmuea(1:2) = transfer_quantity_p(85+nt) ! Wachstumsrate Ausgabeparameter algaesbl()
   fhegas(1:2) = transfer_quantity_p(86+nt) ! Ausgabeparameter algaesi() svhemg
   fhebas(1:2) = transfer_quantity_p(87+nt) ! Ausgabeparameter algaes() svhemb
   agreau(1:2) = transfer_quantity_p(88+nt) ! Ausgabe agbcm algaesgr()
   abreau(1:2) = transfer_quantity_p(89+nt) ! Ausgabe von abbcm (Chlorophyll-a zu Kohlenstoff der blau-Algen) algaesbl()
   
   flag(1:2)   = 0    ! keine Einleitungen
   nkzs(1:2)   = 1    ! nur eine Tiefenschicht
   anze        = 1    ! Anzahl der Profile im aktuellen Strang
   mstr        = 1    ! Strangzähler
   jiein(1)    = 0    ! keine Punkt-Einleitungen
   ilbuhn      = 0    ! keine Buhnen
   iorLa(1)    = 0    ! zur Berücksichtigung der Linienquelle; nicht verwendet
   iorLe(1)    = 0    ! zur Berücksichtigung der Linienquelle; nicht verwendet
   ieinLs(1)   = 0    ! keine Linienquellen
   it_h(1,1:2) = 1
   isim_end    = 0
   iwied       = 0           
   dz2d(1:2)   = 0.0
   vabfl(1:2)  = 0.0  ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   echla(1)    = 0.0  ! keine Einleitung
   qeinl(1)    = 0.0  ! kein Abfluss Einleitung
   evkigr(1)   = 0.0  ! Einleitungswert (keine Einleitungen)
   eantbl(1)   = 0.0  ! Einleitungswert (keine Einleitungen)
   dh2d        = 0.25 ! Dicke Tiefenschicht
   ilamda      = rb_extnct_ilamda
   
   saettk = transfer_value_p(6)
   tauscs = transfer_value_p(7) ! Schiffseinfluss 
   saettg = transfer_value_p(8)
   saettb = transfer_value_p(9)
   
   sised(1:2)       = benthic_distribution_p( 2+nb) ! Siliziumgehalt im Sediment
   cmatki(1:2)      = benthic_distribution_p( 9+nb) ! Abspülung benthischer kiesel-Algen
   cmatgr(1:2)      = benthic_distribution_p(10+nb) ! Abspülung benthischer grün-Algen
   sedalk(1:2)      = benthic_distribution_p(26+nb) ! Sedimentierte Menge an Kiesel-Algen benthische_verteilungen.f95
   sedalg(1:2)      = benthic_distribution_p(27+nb) ! Sedimentierte Menge an Grün-Algen
   sedalb(1:2)      = benthic_distribution_p(28+nb) ! Sedimentierte Menge an Blau-Algen
   algdrk(1:2)      = benthic_distribution_p(38+nb) ! \ref Algen-Konsum-bentisch (Muscheln) in mg/l
   algcok(1:2)      = benthic_distribution_p(39+nb) ! Kiesel-Algen Konsum durch Corophium ?
   algdrg(1:2)      = benthic_distribution_p(40+nb) ! grün-Algen-Konsum-bentisch (Muscheln) in mg/l
   algdrb(1:2)      = benthic_distribution_p(41+nb) ! blau-Algen-Konsum-bentisch (Muscheln) in mg/l
   algcog(1:2)      = benthic_distribution_p(42+nb) ! grün-Algen Konsum durch Corophium ?
   algcob(1:2)      = benthic_distribution_p(43+nb) ! blau-Algen Konsum durch Corophium ?
   sedalg_mq(1,1:2) = benthic_distribution_p(52+nb) ! wird aus sedflux kommen

   do j = 1,ilamda
      eta(j) = rb_extnct_p(1 + (j-1)*anz_extnct_koeff)
      aw(j)  = rb_extnct_p(2 + (j-1)*anz_extnct_koeff)
      ack(j) = rb_extnct_p(3 + (j-1)*anz_extnct_koeff)
      acg(j) = rb_extnct_p(4 + (j-1)*anz_extnct_koeff)
      acb(j) = rb_extnct_p(5 + (j-1)*anz_extnct_koeff)
      ah(j)  = rb_extnct_p(6 + (j-1)*anz_extnct_koeff)
      as(j)  = rb_extnct_p(7 + (j-1)*anz_extnct_koeff)
      al(j)  = rb_extnct_p(8 + (j-1)*anz_extnct_koeff)
      ! extk_lamda is a return value of `algaeski` and is given to 
      ! `algaesbl` and `algesgr`
      ! extk_lamda(j,1) = rb_extnct_p(9 + (j-1)*anz_extnct_koeff)
      extk_lamda(j,1) = 0.0
   enddo
   
   nl  = (i-1) * number_plankt_vari_vert * num_lev
   nlt = (i-1) * num_lev_trans * number_trans_quant_vert
   
   do j = 1,num_lev
      tempwz(j,1:2)    = plankt_vari_vert_p(j+( 1-1)*num_lev+nl) ! Wassertemperatur tiefenaufgelöst
      vnh4z(j,1:2)     = plankt_vari_vert_p(j+( 3-1)*num_lev+nl) ! Ammonium-Stickstoffkonzentration g/m³ tiefenaufgelöst
      vno3z(j,1:2)     = plankt_vari_vert_p(j+( 5-1)*num_lev+nl) ! Nitrat-Stickstoffkonzentration g/m³ tiefenaufgelöst
      gelpz(j,1:2)     = plankt_vari_vert_p(j+( 6-1)*num_lev+nl) ! gelöster Phosphor tiefenaufgelöst
      siz(j,1:2)       = plankt_vari_vert_p(j+( 7-1)*num_lev+nl) ! Silizium-Konzentration (tiefenaufgelöst)
      akiz(j,1:2)      = plankt_vari_vert_p(j+( 8-1)*num_lev+nl) ! Biomasse kiesel-Algen tiefenaufgelöst
      agrz(j,1:2)      = plankt_vari_vert_p(j+( 9-1)*num_lev+nl) ! Biomasse gruen-Algen tiefenaufgelöst
      ablz(j,1:2)      = plankt_vari_vert_p(j+(10-1)*num_lev+nl) ! Biomasse blau-Algen tiefenaufgelöst
      chlaz(j,1:2)     = plankt_vari_vert_p(j+(11-1)*num_lev+nl) ! Chlorophyl-A tiefenaufgelöst
      hchlkz(1,j,1:2)  = plankt_vari_vert_p(j+(12-1)*num_lev+nl) ! Chlorophylgehalt der Kieselalgen
      hchlgz(1,j,1:2)  = plankt_vari_vert_p(j+(13-1)*num_lev+nl) ! Chlorophylgehalt der Gruenalgen
      hchlbz(1,j,1:2)  = plankt_vari_vert_p(j+(14-1)*num_lev+nl) ! Chlorophylgehalt der Blaualgen
      hq_nkz(1,j,1:2)  = plankt_vari_vert_p(j+(17-1)*num_lev+nl) ! Stickstoffanteil der Algenbiomasse kiesel
      hq_ngz(1,j,1:2)  = plankt_vari_vert_p(j+(18-1)*num_lev+nl) ! grün
      hq_nbz(1,j,1:2)  = plankt_vari_vert_p(j+(19-1)*num_lev+nl) ! blau
      hcchlkz(1,j,1:2) = plankt_vari_vert_p(j+(20-1)*num_lev+nl) ! c-chla Verhältnis Kiesel
      hcchlgz(1,j,1:2) = plankt_vari_vert_p(j+(21-1)*num_lev+nl) ! c-chla Verhältnis grün
      hcchlbz(1,j,1:2) = plankt_vari_vert_p(j+(22-1)*num_lev+nl) ! c-chla Verhältnis blau
      
      up_nkz(j,1:2) = trans_quant_vert_p(j+( 1-1)*num_lev_trans+nlt) ! N (Stickstoff) Aufnahmerate der Kiesel-Algen
      up_ngz(j,1:2) = trans_quant_vert_p(j+( 2-1)*num_lev_trans+nlt) ! N (Stickstoff) Aufnahmerate der grün-Algen
      up_nbz(j,1:2) = trans_quant_vert_p(j+( 3-1)*num_lev_trans+nlt) ! N (Stickstoff) Aufnahmerate der blau-Algen
      up_siz(j,1:2) = trans_quant_vert_p(j+( 4-1)*num_lev_trans+nlt) ! Si-Aufnahmerate der Kiesel-Algen
      up_pkz(j,1:2) = trans_quant_vert_p(j+( 5-1)*num_lev_trans+nlt) ! P (Phosphor) Aufnahmerate der Kiesel-Algen
      up_pgz(j,1:2) = trans_quant_vert_p(j+( 6-1)*num_lev_trans+nlt) ! P (Phosphor) Aufnahmerate der grün-Algen
      up_pbz(j,1:2) = trans_quant_vert_p(j+( 7-1)*num_lev_trans+nlt) ! P (Phosphor) Aufnahmerate der blau-Algen
      dalgkz(j,1:2) = trans_quant_vert_p(j+(12-1)*num_lev_trans+nlt) ! Zuwachs Kieselalgen tiefenaufgelöst
      dalggz(j,1:2) = trans_quant_vert_p(j+(13-1)*num_lev_trans+nlt) ! Zuwachs Kieselalgen tiefenaufgelöst
      dalgbz(j,1:2) = trans_quant_vert_p(j+(14-1)*num_lev_trans+nlt) ! Zuwachs Kieselalgen tiefenaufgelöst
   enddo

   do j = 1,num_lev_trans
      up_n2z(j,1:2) = trans_quant_vert_p(j+( 8-1)*num_lev_trans+nlt) ! Aufnahmerate von Luftstickstoff durch Blaualgen
      algakz(j,1:2) = trans_quant_vert_p(j+(18-1)*num_lev_trans+nlt) ! Respirierte Kiesel-Algenbiomasse,
      algagz(j,1:2) = trans_quant_vert_p(j+(19-1)*num_lev_trans+nlt) ! Respirierte grün-Algenbiomasse,
      algabz(j,1:2) = trans_quant_vert_p(j+(20-1)*num_lev_trans+nlt) ! Respirierte blau-Algenbiomasse
      akibrz(j,1:2) = trans_quant_vert_p(j+(23-1)*num_lev_trans+nlt) ! Zwischengröße Kiesel-Algen-Biomasse ?
      agrbrz(j,1:2) = trans_quant_vert_p(j+(24-1)*num_lev_trans+nlt) ! Zwischengröße grün-Algen-Biomasse ?
      ablbrz(j,1:2) = trans_quant_vert_p(j+(25-1)*num_lev_trans+nlt) ! Zwischengröße blau-Algen-Biomasse ?
      algzkz(j,1:2) = trans_quant_vert_p(j+(26-1)*num_lev_trans+nlt) ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l
      algzgz(j,1:2) = trans_quant_vert_p(j+(27-1)*num_lev_trans+nlt) ! grün-Algen-Konsum durch Zoo-Plankton in mg/l
      algzbz(j,1:2) = trans_quant_vert_p(j+(28-1)*num_lev_trans+nlt) ! blau-Algen-Konsum durch Zoo-Plankton in mg/l
   enddo
   
   
   if (control) then
      print*, 'before algae metabolism:'
      print*, '   up_siz = ',up_siz(1,1)
      print*, '   chlaki,chlagr,chlabl = ',chlaki(1),chlagr(1),chlabl(1)
      print*, '   aki,agr,abl = ',aki(1),agr(1),abl(1)
      print*, '   schwi(1),chla,up_nkz = ',schwi(1),chla(1),up_nkz(1,1)
      print*, '   q_nk,hq_nkz,akbcm,hcchlgz = ',q_nk(1),hq_nkz(1,1,1),akbcm(1),hcchlgz(1,1,1)
      print*, '   algzok,zooind,grot = ',algzok(1),zooind(1),grot
      print*, '   akmor_1,agmor_1,abmor_1 = ',akmor_1(1,1),agmor_1(1,1),abmor_1(1,1)
      print*, '   svhemk = ',svhemk(1),extk(1)
      print*, '   sedalg_mq = ', sedalg_mq(1,1)
      print*, '   extk,extks = ',extk(1:2),extks(1,1:2)
      print*, ''
   endif
   
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
   ! Grünalgen zuletzt, weil dort zusammengefasst wird sonst nachfolgende zusammenfassung:
   
   call algaeski(schwi, tflie, tempw, tempwz, rau, tiefe, vmitt, vno3, vnh4,   &
                 gelp, svhemk, svhemb, svhemg, chla, si, dalgki, dalgak, flag, &
                 anze, sedalk, algzok, echla, qeinl, vabfl, dkimor,            &
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
   
   if (control) then
      print*, 'after algae metabolism:'
      print*, '   up_siz = ',up_siz(1,1)
      print*, '   chlaki,chlagr,chlabl = ',chlaki(1),chlagr(1),chlabl(1)
      print*, '   aki,agr,abl = ',aki(1),agr(1),abl(1)
      print*, '   vkigr(1),chlaki(1),chla(1) = ',vkigr(1),chlaki(1),chla(1)
      print*, '   svhemk,svhemg,svhemb = ',svhemk(1),svhemg(1),svhemb(1)
      print*, '   extk = ',extk(1)
      print*, '   agbcm, hchlgz, hcchlgz = ', agbcm(1), hchlgz(1,1,1), hcchlgz(1,1,1)
      print*, '   akmor_1,agmor_1,abmor_1 = ',akmor_1(1,1),agmor_1(1,1),abmor_1(1,1)
      print*, '   up_nkz = ',up_nkz(1,1)
      print*, '   hchlkz hchlgz hchlbz hcchlgz hcchlkz hcchlbz = ' &
            ,hchlkz(1,1,1), hchlgz(1,1,1), hchlbz(1,1,1), hcchlgz(1,1,1), hcchlkz(1,1,1), hcchlbz(1,1,1)
   endif
   
   
   ! ==========================================================================
   ! Datenrückgabe
   ! ==========================================================================
   planktonic_variable_p( 8+nk) = aki(1)       ! Biomasse Kiesel-Algen
   planktonic_variable_p( 9+nk) = agr(1)       ! Trockenmasse Gruen-Algen
   planktonic_variable_p(10+nk) = abl(1)       ! Trockenmasse Blau-Algen
   planktonic_variable_p(11+nk) = chla(1)      ! Chlorophyl-A tiefengemittelt
   planktonic_variable_p(12+nk) = Chlaki(1)    ! Chlorophyl in Kiesel-Algen muegchla/l
   planktonic_variable_p(13+nk) = chlagr(1)    ! Chlorophyl in Gruenalgen muegchla/l
   planktonic_variable_p(14+nk) = chlabl(1)    ! Chlorophyl in Blaualgen muegchla/l
   planktonic_variable_p(19+nk) = vkigr(1)     ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ?
   planktonic_variable_p(20+nk) = antbl(1)     ! Anteil der Blaualgen am Gesamt-Chlorophyll-a
   planktonic_variable_p(21+nk) = svhemk(1)    !
   planktonic_variable_p(22+nk) = svhemg(1)    !
   planktonic_variable_p(23+nk) = svhemb(1)    !
   planktonic_variable_p(24+nk) = akbcm(1)     ! Verhältnis Chlorophyll-a zu Kohlenstoff Kieselalgen
   planktonic_variable_p(25+nk) = agbcm(1)     ! Verhältnis Chlorophyll-a zu Kohlenstoff Gruenalgen
   planktonic_variable_p(26+nk) = abbcm(1)     ! Verhältnis Chlorophyll-a zu Kohlenstoff der blau-Algen
   planktonic_variable_p(30+nk) = q_nk(1)      ! Stickstoffanteil der Algenbiomasse kiesel
   planktonic_variable_p(31+nk) = q_pk(1)      ! Phosphoranteil der Kiesel-Algenbiomasse
   planktonic_variable_p(32+nk) = q_sk(1)      ! Siliziumgehalt Kieselalgen
   planktonic_variable_p(33+nk) = q_ng(1)      ! Stickstoffanteil der Algenbiomasse grün
   planktonic_variable_p(34+nk) = q_pg(1)      ! Phosphoranteil der grün-Algenbiomasse
   planktonic_variable_p(35+nk) = q_nb(1)      ! Stickstoffanteil der Algenbiomasse blau
   planktonic_variable_p(36+nk) = q_pb(1)      ! Phosphoranteil der blau-Algenbiomasse
   planktonic_variable_p(69+nk) = skmor(1)     ! Silizium in schwebenden, abgestorbenen Kieselalgen
   planktonic_variable_p(77+nk) = akmor_1(1,1) ! ? Kiesel-Algen
   planktonic_variable_p(78+nk) = agmor_1(1,1) ! ? Gruen-Algen
   planktonic_variable_p(79+nk) = abmor_1(1,1) ! ? Blau-Algen
   
   transfer_quantity_p( 7+nt) = dkimor(1) ! Absterberate Kieselalgen
   transfer_quantity_p( 8+nt) = dgrmor(1) ! Absterberate Grünalgen
   transfer_quantity_p( 9+nt) = dblmor(1) ! Absterberate Blaualgen
   transfer_quantity_p(20+nt) = dalgki(1) ! Zuwachs Kiesel-Algen
   transfer_quantity_p(21+nt) = dalggr(1) ! Zuwachs grün-Algen
   transfer_quantity_p(22+nt) = dalgbl(1) ! Zuwachs blau-Algen
   transfer_quantity_p(23+nt) = dalgak(1) ! Respiration Kiesel-Algen
   transfer_quantity_p(24+nt) = dalgag(1) ! Respiration grün-Algen
   transfer_quantity_p(25+nt) = dalgab(1) ! Respiration blau-Algen
   transfer_quantity_p(48+nt) = akitbr(1) ! Kieselalgen ?
   transfer_quantity_p(49+nt) = agrtbr(1) !
   transfer_quantity_p(50+nt) = abltbr(1) ! Zwischengröße Algenbiomasse?
   transfer_quantity_p(54+nt) = extk(1)   ! mittlerer Extinktionskoeffizien
   transfer_quantity_p(55+nt) = tpki(1)   ! Ausgabeparameter ? Kieselalgen Phosphor ?
   transfer_quantity_p(56+nt) = akmuea(1) ! Ausgabeparameter algaeski()
   transfer_quantity_p(57+nt) = ftaaus(1) ! Ausgabeparameter algaeski() fta
   transfer_quantity_p(58+nt) = fiaus(1)  ! Ausgabeparameter algaeski() Pmit/(Pmax*3.6)
   transfer_quantity_p(59+nt) = fheaus(1) ! Ausgabeparameter algaeski() svhemk
   transfer_quantity_p(60+nt) = akraus(1) ! Ausgabeparameter algaeski() F53
   transfer_quantity_p(80+nt) = tpgr(1)   ! Ausgabeparameter ? Grünalgen Phosphor ?
   transfer_quantity_p(81+nt) = tpbl(1)   ! Ausgabeparameter ? Blaualgen Phosphorhemmung ?
   transfer_quantity_p(82+nt) = figaus(1) ! Ausgabeparameter algaesgr() Pmit/(Pmax*3.6)
   transfer_quantity_p(83+nt) = fibaus(1) ! Ausgabeparameter algaesbl() Pmit/(Pmax*3.6)
   transfer_quantity_p(84+nt) = agmuea(1) ! Wachstumsrate Ausgabeparameter algaesgr()
   transfer_quantity_p(85+nt) = abmuea(1) ! Wachstumsrate Ausgabeparameter algaesbl()
   transfer_quantity_p(86+nt) = fhegas(1) ! Ausgabeparameter algaesi() svhemg
   transfer_quantity_p(87+nt) = fhebas(1) ! Ausgabeparameter algaesbl() svhemb
   transfer_quantity_p(88+nt) = agreau(1) ! Ausgabe agbcm algaesgr()
   transfer_quantity_p(89+nt) = abreau(1) ! Ausgabe von abbcm (Chlorophyll-a zu Kohlenstoff der blau-Algen) algaesbl()
   
   benthic_distribution_p( 2+nb) = sised(1)       ! Siliziumgehalt im Sediment (wird von algaeski verändert)
   benthic_distribution_p(26+nb) = sedalk(1)      ! Sedimentierte Menge an Kiesel-Algen
   benthic_distribution_p(27+nb) = sedalg(1)      ! Sedimentierte Menge an Grün-Algen
   benthic_distribution_p(28+nb) = sedalb(1)      ! Sedimentierte Menge an Blau-Algen
   benthic_distribution_p(52+nb) = sedalg_mq(1,1) ! 
   benthic_distribution_p(53+nb) = sedalk0(1)     ! sedAlk0 wird nur an k_eps() übergeben.
   
   transfer_value_p(6) = saettk
   transfer_value_p(8) = saettg
   transfer_value_p(9) = saettb
   
   
   do j = 1,num_lev
      plankt_vari_vert_p(j+( 8-1)*num_lev+nl) = akiz(j,1)      ! Biomasse kiesel-Algen tiefenaufgelöst
      plankt_vari_vert_p(j+( 9-1)*num_lev+nl) = agrz(j,1)      ! Biomasse gruen-Algen tiefenaufgelöst
      plankt_vari_vert_p(j+(10-1)*num_lev+nl) = ablz(j,1)      ! Biomasse blau-Algen tiefenaufgelöst
      plankt_vari_vert_p(j+(11-1)*num_lev+nl) = chlaz(j,1)     ! Chlorophyl-A tiefenaufgelöst
      plankt_vari_vert_p(j+(12-1)*num_lev+nl) = hchlkz(1,j,1)  ! Chlorophylgehalt der Kieselalgen
      plankt_vari_vert_p(j+(13-1)*num_lev+nl) = hchlgz(1,j,1)  ! Chlorophylgehalt der Gruenalgen
      plankt_vari_vert_p(j+(14-1)*num_lev+nl) = hchlbz(1,j,1)  ! Chlorophylgehalt der Blaualgen
      plankt_vari_vert_p(j+(17-1)*num_lev+nl) = hq_nkz(1,j,1)  ! Stickstoffanteil der Algenbiomasse kiesel
      plankt_vari_vert_p(j+(18-1)*num_lev+nl) = hq_ngz(1,j,1)  ! grün
      plankt_vari_vert_p(j+(19-1)*num_lev+nl) = hq_nbz(1,j,1)  ! blau
      plankt_vari_vert_p(j+(20-1)*num_lev+nl) = hcchlkz(1,j,1) ! c-chla Verhältnis Kiesel
      plankt_vari_vert_p(j+(21-1)*num_lev+nl) = hcchlgz(1,j,1) ! c-chla Verhältnis grün
      plankt_vari_vert_p(j+(22-1)*num_lev+nl) = hcchlbz(1,j,1) ! c-chla Verhältnis blau

      trans_quant_vert_p(j+( 1-1)*num_lev_trans+nlt) = up_nkz(j,1) ! N (Stickstoff) Aufnahmerate der Kiesel-Algen
      trans_quant_vert_p(j+( 2-1)*num_lev_trans+nlt) = up_ngz(j,1) ! N (Stickstoff) Aufnahmerate der grün-Algen
      trans_quant_vert_p(j+( 3-1)*num_lev_trans+nlt) = up_nbz(j,1) ! N (Stickstoff) Aufnahmerate der blau-Algen
      trans_quant_vert_p(j+( 4-1)*num_lev_trans+nlt) = up_siz(j,1) ! Si-Aufnahmerate der Kiesel-Algen
      trans_quant_vert_p(j+( 5-1)*num_lev_trans+nlt) = up_pkz(j,1) ! P (Phosphor) Aufnahmerate der Kiesel-Algen
      trans_quant_vert_p(j+( 6-1)*num_lev_trans+nlt) = up_pgz(j,1) ! P (Phosphor) Aufnahmerate der grün-Algen
      trans_quant_vert_p(j+( 7-1)*num_lev_trans+nlt) = up_pbz(j,1) ! P (Phosphor) Aufnahmerate der blau-Algen
      trans_quant_vert_p(j+(12-1)*num_lev_trans+nlt) = dalgkz(j,1) ! Zuwachs Kieselalgen tiefenaufgelöst
      trans_quant_vert_p(j+(13-1)*num_lev_trans+nlt) = dalggz(j,1) ! Zuwachs Kieselalgen tiefenaufgelöst
      trans_quant_vert_p(j+(14-1)*num_lev_trans+nlt) = dalgbz(j,1) ! Zuwachs Kieselalgen tiefenaufgelöst

   enddo
   
   do j = 1,num_lev_trans
      trans_quant_vert_p(j+( 8-1)*num_lev_trans+nlt) = up_n2z(j,1) ! Aufnahmerate von Luftstickstoff durch Blaualgen
      trans_quant_vert_p(j+(18-1)*num_lev_trans+nlt) = algakz(j,1) ! Respirierte Kiesel-Algenbiomasse, tiefenaufgelöst
      trans_quant_vert_p(j+(19-1)*num_lev_trans+nlt) = algagz(j,1) ! Respirierte grün-Algenbiomasse,
      trans_quant_vert_p(j+(20-1)*num_lev_trans+nlt) = algabz(j,1) ! Respirierte blau-Algenbiomasse
      trans_quant_vert_p(j+(23-1)*num_lev_trans+nlt) = akibrz(j,1) ! Kiesel-Algen-Biomasse? Wachstum? tiefenaufgelöst
      trans_quant_vert_p(j+(24-1)*num_lev_trans+nlt) = agrbrz(j,1) ! Zwischengröße grün-Algen-Biomasse ?
      trans_quant_vert_p(j+(25-1)*num_lev_trans+nlt) = ablbrz(j,1) ! Zwischengröße blau-Algen-Biomasse ?
      trans_quant_vert_p(j+(26-1)*num_lev_trans+nlt) = algzkz(j,1) ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l
      trans_quant_vert_p(j+(27-1)*num_lev_trans+nlt) = algzgz(j,1) ! grün-Algen-Konsum durch Zoo-Plankton in mg/l
      trans_quant_vert_p(j+(28-1)*num_lev_trans+nlt) = algzbz(j,1) ! blau-Algen-Konsum durch Zoo-Plankton in mg/l
   enddo
   
   if (extk(1) <= 0.0) print*,'algae_huelle extk(1+2) <= 0.0 , extks, iglob', extk(1), extk(2), extks(1,1), iglob
   
   ! ==========================================================================
   ! Kontrolle und Prüfung auf NaN
   ! ==========================================================================
   if (num_lev_trans > nkzs(1)) call qerror('Wrong number of depth resolved layers in algae_huelle.')

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

