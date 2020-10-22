!> \page rotatorien Rotatorien
!!
!! Algen können in Gewässern in nennenswertem Umfang von Zooplankton konsumiert werden. 
!! QSim modelliert daher stellvertretend für das Zooplankton die Rotatorien (Rädertierchen). 
!! Diese stellen die 1. Ebene des Konsums (erste Trophie-Ebene) der Algen (Primärproduzenten) dar. 
!! Rotatorien werden im Modell als passiv im Wasser schwebend betrachtet.\n
!! Bei der Beprobung werden diese Organismen mit Netzen/Filtern von 50 µm Maschenweite gefangen 
!! und danach bestimmt und gezählt. Entsprechend werden sie im Modell als Individuendichte pro Volumen
!! modelliert, aus der mit einem mittleren Individuengewicht 
!! die Biomasse-Konzentration berechnet werden kann.
!! 
!! <h2>Herkunft</h2>
!!   UNTERPROGRAMM ZUR BERECHNUNG DES ZOOPLANKTONEINFLUSSES\n            
!!   AUF DEN STOFF-UND SAUERSTOFFHAUSHALT EINES FLIEGEWAESSERS \n        
!!   AUTOR :      VOLKER KIRCHESCH    \n                                 
!!   entnommen aus Version qsim13.301_28mae18\n 
!!                                           
!! <h2>Teilprozesse</h2>
!!   Wachstum verbunden mit Fraß \n
!!   Respiration \n
!!   Mortalität \n
!!                 
!! <h2>Schnittstellenbeschreibung</h2>
!!  subroutine konsum() \ref vkigr, \ref tempw, \ref vo2, \ref tflie                                  &\n
!!  &, \ref ezind, \ref zooind, \ref abszo, \ref ir, \ref flag,elen,ior,anze,qeinl,vabfl             &\n
!!  &,jiein, \ref fopire, \ref grote, \ref dzres1, \ref dzres2, \ref zresge                          &\n
!!  &, \ref irmaxe, \ref zexki, \ref zexgr, \ref zexbl                                         &\n
!!  &, \ref aki, \ref agr, \ref abl, \ref iwied, \ref rmuas, \ref iras, \ref tgzoo, \ref bac, \ref zbac    &\n
!!  &, \ref rakr, \ref rbar, \ref chnf, \ref zhnf, \ref ilbuhn, \ref zakie
!!  , \ref zagre, \ref zable, \ref hnfza, \ref algzok        &\n
!!  &, \ref algzog, \ref algzob, \ref akiz, \ref agrz, \ref ablz
!!  , \ref algzkz, \ref algzgz, \ref algzbz, \ref nkzs, \ref monats    &\n
!!  &, \ref itags, \ref uhrz, \ref mstr, \ref azstrs, \ref kontroll, \ref iglob)
!!                            
!! <h2>Dokumentation und Veröffentlichungen</h2>
!!
!! Bisher existiert eine Dokumentation des Konsum/Rotatorien-Moduls als Kapitel 13 der
!! <a href="./pdf/QSimDoku_ncycWy.pdf" target="_blank">Kurzdoku</a> (Version vom 22. Nov. 2017)
!! \n\n
!! <a href="./pdf/Schoel_et_al_2002rhein.pdf" target="_blank"> 
!! Modelling the Chlorophyll a Content of the River Rhine - 
!! Interrelation between Riverine Algal Production and Population
!! Biomass of Grazers, Rotifers and the Zebra Mussel,
!! Dreissena polymorpha</a>\n
!! Schöl et al. 2002 </a>
!! \n\n
!! <a href="./pdf/Schoel_et_al_1999mosel-saar.pdf" target="_blank"> 
!! Model-based analysis of oxygen budget and biological processes in the
!! regulated rivers Moselle and Saar: modelling the influence of benthic
!! filter feeders on phytoplankton</a>\n
!! Schöl et al. 1999
!!
!! <h2>IT-Realisierung</h2>
!! Die QSim Subroutine konsum() wird in QSim-3D von der Hüllroutine konsum_huelle() aufgerufen (siehe dazu: \ref hüllen). 
!!
!! <h2>Aufruf-Parameter:(Stand QSim 13.30)</h2>
!!<table >
!!<tr><th> Variablen-Name \n QSim-1D </th><th> Daten-Feld \n QSim-3D </th><th> Beschreibung </th><th> Einheit </th></tr>
!!<tr><td> vkigr	</td><td> \ref tiefengemittelte_planktische_variable 19 </td><td> Anteil Kieselalgen </td><td> - </td></tr>
!!<tr><td> TEMPW	</td><td> \ref tiefengemittelte_planktische_variable 1 </td><td> Wasser-Temperatur </td><td> °C </td></tr>
!!<tr><td> VO2		</td><td> \ref tiefengemittelte_planktische_variable 2 </td><td> Sauerstoff </td><td> mg/l </td></tr>
!!<tr><td> TFLIE	</td><td>  real(deltatt)/86400  </td><td> Zeitschritt </td><td> TFLIE in d; deltat in s  </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td> ezind	</td><td> - </td><td> keine Einleitungen in QSim-3D </td><td> - </td></tr>
!!<tr><td> ZOOIND	</td><td> \ref tiefengemittelte_planktische_variable 50 </td><td> Individuendichte </td><td> 1/l </td></tr>
!!<tr><td> abszo	</td><td> \ref tiefengemittelte_übergabe_variable 6 </td><td> Absterberate </td><td> 1/d </td></tr>
!!<tr><td> ir		</td><td> \ref tiefengemittelte_übergabe_variable 42 </td><td> Ingestionsrate </td><td> mg/(l*h) </td></tr>
!!<tr><td> flag		</td><td> 0 </td><td> keine Einleitungen </td><td> - </td></tr>
!!<tr><td> elen		</td><td> 1 </td><td> Elementlänge (nicht verwendet) </td><td> - </td></tr>
!!<tr><td> ior		</td><td> 1 </td><td> Laufindex </td><td> - </td></tr>
!!<tr><td> anze		</td><td> 1 </td><td> Anzahl der Profile im aktuellen Strang </td><td> - </td></tr>
!!<tr><td> qeinl	</td><td> 0 </td><td> keine Einleitung </td><td> - </td></tr>
!!<tr><td> vabfl	</td><td> 0 </td><td> keine Einleitung </td><td> - </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td> jiein	</td><td> 0 </td><td> keine Punkt-Einleitungen </td><td> - </td></tr>
!!<tr><td> FopIRe	</td><td> \ref globaleParameter </td><td> Halbsättigungskonstante für Futteraufnahme </td><td> mg/l </td></tr>
!!<tr><td> GRote	</td><td> \ref globaleParameter </td><td> Gewicht einer Rotatorie </td><td> µg </td></tr>
!!<tr><td> dzres1	</td><td> \ref tiefengemittelte_übergabe_variable 27 </td><td> Grund-Respiration Konsumenten </td><td>  </td></tr>
!!<tr><td> dzres2	</td><td> \ref tiefengemittelte_übergabe_variable 28 </td><td> Fraßabhängige Respirationsrate </td><td>  </td></tr>
!!<tr><td> zresge	</td><td> \ref globaleParameter </td><td> Grundrespiration Rotatorien </td><td> 1/d </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td> irmaxe	</td><td> \ref globaleParameter </td><td> max. Gewichtsspez. Algenaufnahmerate </td><td>  </td></tr>
!!<tr><td> zexki	</td><td> \ref tiefengemittelte_übergabe_variable 16 </td><td> Ausscheidungen infolge Konsums von Kieselalgen </td><td>  </td></tr>
!!<tr><td> zexgr	</td><td> \ref tiefengemittelte_übergabe_variable 17 </td><td> Ausscheidungen infolge Konsums von Grünalgen </td><td>  </td></tr>
!!<tr><td> zexbl	</td><td> \ref tiefengemittelte_übergabe_variable 18 </td><td> Ausscheidungen infolge Konsums von Blaualgen  </td><td>  </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td> aki		</td><td> \ref tiefengemittelte_planktische_variable 8 </td><td> Biomasse der Kieselalgen </td><td> mg/l </td></tr>
!!<tr><td> agr		</td><td> \ref tiefengemittelte_planktische_variable 9 </td><td> Biomasse der Grünalgen </td><td> mg/l </td></tr>
!!<tr><td> abl		</td><td> \ref tiefengemittelte_planktische_variable 10 </td><td> Biomasse der Blaualgen </td><td> mg/l </td></tr>
!!<tr><td> iwied	</td><td> 0 </td><td> unbenutzte Variable </td><td> - </td></tr>
!!<tr><td> rmuas	</td><td> \ref tiefengemittelte_übergabe_variable 76 </td><td> Ausgabe: Nettowachstumsrate </td><td> 1/d </td></tr>
!!<tr><td> iras		</td><td> \ref tiefengemittelte_übergabe_variable 79 </td><td> Ausgabe: Ingestionsrate </td><td> 1/d </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td> rakr		</td><td> \ref tiefengemittelte_übergabe_variable 77 </td><td> Ausgabe: fraßabhängige Respiration </td><td> 1/d </td></tr>
!!<tr><td> rbar		</td><td> \ref tiefengemittelte_übergabe_variable 78 </td><td> Ausgabe: GRund?-Respiration? </td><td> 1/d </td></tr>
!!<tr><td> CHNF		</td><td> \ref tiefengemittelte_planktische_variable 48 </td><td> C-Masse der heterotrophen Nanoflagelaten </td><td> mg C / l </td></tr>
!!<tr><td> zHNF		</td><td> \ref tiefengemittelte_übergabe_variable 74 </td><td> Aufnahmerate der HNF </td><td> 1/d </td></tr>
!!<tr><td> ilbuhn	</td><td> 0 </td><td> keine Buhnen </td><td> - </td></tr>
!!<tr><td> zakie	</td><td> \ref globaleParameter </td><td> Filtrierbarkeit Kieselalgen </td><td> - </td></tr>
!!<tr><td> zagre	</td><td> \ref globaleParameter </td><td> Filtrierbarkeit Grünalgen </td><td> - </td></tr>
!!<tr><td> zable	</td><td> \ref globaleParameter </td><td> Filtrierbarkeit Blaualgen </td><td> - </td></tr>
!!<tr><td> HNFza	</td><td> \ref tiefengemittelte_übergabe_variable 75 </td><td> Ausgabe: HNFza(ior) = (zHNF(ior)/CHNF(ior))*24. </td><td>  </td></tr>
!!<tr><td> algzok	</td><td> \ref tiefengemittelte_übergabe_variable 53 </td><td> kiesel-Algen-Konsum </td><td> mg/l </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td> algzog	</td><td> \ref tiefengemittelte_übergabe_variable 72 </td><td> grün-Algen-Konsum </td><td> mg/l </td></tr>
!!<tr><td> algzob	</td><td> \ref tiefengemittelte_übergabe_variable 73 </td><td> blau-Algen-Konsum </td><td> mg/l </td></tr>
!!<tr><td> akiz		</td><td> \ref tiefenaufgelöste_planktische_variable 8 </td><td> Biomasse kiesel-Algen </td><td> mg/l </td></tr>
!!<tr><td> agrz		</td><td> \ref tiefenaufgelöste_planktische_variable 9 </td><td> Biomasse grün-Algen </td><td> mg/l </td></tr>
!!<tr><td> ablz		</td><td> \ref tiefenaufgelöste_planktische_variable 10 </td><td> Biomasse blau-Algen </td><td> mg/l </td></tr>
!!<tr><td> algzkz	</td><td> \ref tiefenaufgelöste_übergabe_variable 26 </td><td> kiesel-Algen-Konsum </td><td> mg/l </td></tr>
!!<tr><td> algzgz	</td><td> \ref tiefenaufgelöste_übergabe_variable 27 </td><td> grün-Algen-Konsum </td><td> mg/l </td></tr>
!!<tr><td> algzbz	</td><td> \ref tiefenaufgelöste_übergabe_variable 28 </td><td> blau-Algen-Konsum </td><td> mg/l </td></tr>
!!<tr><td> nkzs		</td><td> noch 1 </td><td> Anzahl Tiefenschichten </td><td>  </td></tr>
!!<tr><td> monats	</td><td> module ::modell monat </td><td> monat </td><td>  </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td> itags	</td><td> module ::modell tag </td><td> tag </td><td>  </td></tr>
!!<tr><td> mstr		</td><td> 1 </td><td> Strangzähler </td><td>  </td></tr>
!!</table>
!! \n\n
!! Quelle konsum_huelle.f95; zurück zu: \ref Stoffumsatz
!

!!<table border="0" ><tr><td  width="50%" ><center> 
!! \image html rotatorien.jpg "Wandtafel zu Rotatorien aus der Sammlung der Humbold-Universität Berlin"
!! \image latex rotatorien.jpg "Wandtafel zu Rotatorien aus der Sammlung der Humbold-Universität Berlin" width=0.95\textwidth
!! © Humboldt-Universität zu Berlin, Lebenswissenschaftliche Fakultät, Institut für Biologie/Vergleichende Zoologie
!! http://www.sammlungen.hu-berlin.de/dokumente/10219/
!! </center></td><td  width="50%" align="left" valign="top">
!!</td></tr></table>

!> SUBROUTINE konsum_huelle() wird beschrieben in: \ref rotatorien \n
!! Quelle konsum.huelle.f95
      SUBROUTINE konsum_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none

!    integer                        :: anze
!    integer, Dimension(1000)       :: jiein, nkzs, flag
!    real                           :: irmaxK, irmaxG, irmaxB, irmaxn, irmax, mueRot, mormax, mormin, morRot 
!    real, Dimension(50)            :: irz
!    real, Dimension(100)           :: ezind, qeinl 
!    real, Dimension(1000)          :: tempw, vo2, zooind, abszo, zexki, zexgr, zexbl, aki, agr, abl, vkigr 
!    real, Dimension(1000)          :: elen, ir, dzres1, dzres2, vabfl, rmuas, iras, rakr, rbar 
!    real, Dimension(1000)          :: CHNF, zHNF, HNFza, algzok, algzog, algzob 
!    real, Dimension(50,1000)       :: akiz, agrz, ablz, algzkz, algzgz, algzbz 
!    double precision               :: Qquell,QSenk 

      integer :: i,j

      iglob=(i+meinrang*part)

!! aus module::aparam
!! , irmaxe, GRote

      !if(i.eq.1)print*,'Rotatorien konsumieren drauflos'

! vkigr,TEMPW,VO2,TFLIE  
      vkigr(1) = planktonic_variable_p(19+(i-1)*number_plankt_vari) ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ??
      vkigr(2) = vkigr(1)
      tempw(1) = planktonic_variable_p( 1+(i-1)*number_plankt_vari) ! Wasser-Temperatur
      tempw(2) = tempw(1)
      vo2(1) = planktonic_variable_p( 2+(i-1)*number_plankt_vari) ! Sauerstoffgehalt tiefengemittelt
      vo2(2) = vo2(1)
      tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)

 ! ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl
      ezind = 0.0  ! Einleitungswert (keine Einleitungen in QSim-3D)
      zooind(1) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien
      zooind(2) = zooind(1)
      !abszo(1) = transfer_quantity_p(6+(i-1)*number_trans_quant) ! Absterberate Zooplankton
      !abszo(2) = abszo(1)
      !ir(1) = transfer_quantity_p(42+(i-1)*number_trans_quant) ! Ingestionsrate der Rotatorien in mg/(l*h) | konsum()
      !ir(2) = ir(1)
      flag(1)=0         ! keine Einleitungen
      flag(2)=flag(1)
      elen(1)=1         ! Elementlänge (nicht verwendet)
      elen(2)=elen(1)
      ior=1             ! Laufindex
      anze=1            ! Anzahl der Profile im aktuellen Strang
      qeinl(1)= 0.0      ! kein Abfluss Einleitung
      qeinl(2)= qeinl(1)
      vabfl(1) = 0.0     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
      vabfl(2) = vabfl(1)

! jiein,FopIRe,GRote,dzres1,dzres2,zresge
      jiein(1)=0       ! keine Punkt-Einleitungen
      ! FopIRe ! Halbsättigungskonstante für Futteraufnahme d. Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
      ! GRote ! Gewicht Rotatorie ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
      !dzres1(1) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
      !dzres1(2) = dzres1(1)
      !dzres2(1) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten
      !dzres2(2) = dzres2(1)
      ! zresge ! Grundrespiration Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder

! irmaxe,zexki,zexgr,zexbl                                         &
      ! IRMAXe ! max. Gewichtsspez. Algenaufnahmerate d. Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
      !zexki(1) = transfer_quantity_p(16+(i-1)*number_trans_quant) 
      ! Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen
      !zexki(2) = zexki(1)
      !zexgr(1) = transfer_quantity_p(17+(i-1)*number_trans_quant) ! Grünalgen
      !zexgr(2) = zexgr(1)
      !zexbl(1) = transfer_quantity_p(18+(i-1)*number_trans_quant) ! Blaualgen
      !zexbl(2) = zexbl(1)

! aki,agr,abl,iwied,rmuas,iras                                     &
      aki(1) = planktonic_variable_p(8+(i-1)*number_plankt_vari) ! Biomasse  Kiesel-Algen ! keine Rückgabevariable
      aki(2) = aki(1)
      agr(1) = planktonic_variable_p(9+(i-1)*number_plankt_vari) !  Biomasse Grün-Algen
      agr(2) = agr(1) 
      abl(1) = planktonic_variable_p(10+(i-1)*number_plankt_vari) !  Biomasse Blau-Algen
      abl(2) = abl(1)
      ! ### iwied = 0 : allererster Zeitschritt, danach iwied = 1 ###                  
      iwied=1     
      ! rmuas ! Ausgabeparameter
      ! iras ??? Ausgabeparameter

! rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
      ! rakr ! Ausgabeparameter
      ! rbar ! Ausgabeparameter
      CHNF(1:2) = planktonic_variable_p(48+(i-1)*number_plankt_vari) ! C-Masse der heterotrophen Nanoflagelaten
      if(CHNF(1).le. 0.0) CHNF(1:2) = 0.0 ! CHNF=-1 meint keine HNF

      !zHNF(1) = transfer_quantity_p(74+(i-1)*number_trans_quant) ! Aufnahmerate der HNF   
      !zHNF(2) =   zHNF(1)  
!   ir - Ingestionsrate in mg/(l*h)                                  
!   zHNF - Aufnahmerate der HNF                                     
!   ir/A - Filtriertes Wasservolumen l/h                     
!   zHNF(ior) = ir(ior)*CHNF(ior)/(agr(ior)+aki(ior)+abl(ior)) 
      ilbuhn = 0          ! keine Buhnen
      ! ZAKIe ! Filtrierbarkeit Kieselalgen ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
      ! zagre!  Filtrierbarkeit Grünalgen ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
      ! zable! Filtrierbarkeit der Blaualgen durch Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
      !HNFza(1) = transfer_quantity_p(75+(i-1)*number_trans_quant) !   Ausgabe: HNFza(ior) = (zHNF(ior)/CHNF(ior))*24. 
      !HNFza(2) = HNFza(1)
      !algzok(1) = transfer_quantity_p(53+(i-1)*number_trans_quant) ! kiesel-Algen-Konsum Zoo-Plankton in mg/l
      !algzok(2) = algzok(1)

! algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
      !algzog(1) = transfer_quantity_p(72+(i-1)*number_trans_quant) ! gruen-Algen-Konsum Zoo-Plankton in mg/l
      !algzog(2) = algzog(1)
      !algzob(1) = transfer_quantity_p(73+(i-1)*number_trans_quant) ! blau-Algen-Konsum Zoo-Plankton in mg/l
      !algzob(2) = algzob(1)
      do j=1,num_lev 
         akiz(j,1) = plankt_vari_vert_p(j+( 8-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse kiesel-Algen tiefenaufgelöst
         akiz(j,2) = akiz(j,1)
         agrz(j,1) = plankt_vari_vert_p(j+( 9-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse gruen-Algen tiefenaufgelöst
         agrz(j,2) = agrz(j,1)
         ablz(j,1) = plankt_vari_vert_p(j+(10-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse blau-Algen tiefenaufgelöst
         ablz(j,2) = ablz(j,1)
         !algzkz(j,1) = trans_quant_vert_p(j+(26-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l 
         !algzkz(j,2) = algzkz(j,1)
         !algzgz(j,1) = trans_quant_vert_p(j+(27-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Grün-Algen-Konsum durch Zoo-Plankton in mg/l 
         !algzgz(j,2) = algzgz(j,1)
         !algzbz(j,1) = trans_quant_vert_p(j+(28-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Blau-Algen-Konsum durch Zoo-Plankton in mg/l 
         !algzbz(j,2) = algzbz(j,1)
      end do
      nkzs(1)=1         ! nur eine Tiefenschicht
      nkzs(2)=nkzs(1)
      monats = monat  ! Monat im Jahr module::modell zeitsekunde()

! itags,mstr)                                               
      itags = tag     ! Tag im Monat module::modell zeitsekunde()
      uhrz=uhrzeit_stunde ! Uhrzeit 
      mstr = 1        ! Strangzähler | nur ein Profil in einem Strang
    ! azStrs=1 parameter in 3D

      TGZoo(1,1) = planktonic_variable_p(76+(i-1)*number_plankt_vari) ! Zooplanktongewicht
      TGZoo(1,2) = TGZoo(1,1)

      BAC(1) = planktonic_variable_p(42+(i-1)*number_plankt_vari) ! Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
      BAC(2) = BAC(1)

      kontroll=(iglob.eq.kontrollknoten) 
      if(kontroll)then
         print*,'konsum vorher: tflie=',tflie,' uhrz=',uhrz
         print*,'GROTe',GROTe
         print*,'zooind,abszo,ir',zooind(1),abszo(1),ir(1)
         print*,'dzres1,dzres2',dzres1(1),dzres2(1)
         print*,'zexki,zexgr,zexbl',zexki(1),zexgr(1),zexbl(1)
         print*,'rmuas,iras',rmuas(1),iras(1)
         print*,'rakr,rbar,zHNF,HNFza,algzok',rakr(1),rbar(1),zHNF(1),HNFza(1),algzok(1)
         print*,'algzog,algzob,algzkz,algzgz,algzbz',algzog(1),algzob(1),algzkz(1,1),algzgz(1,1),algzbz(1,1)
      end if  ! kontrolle

!version  Stoffumsatzroutinen aus der QSim1D Version 13_40 vom 15. Oktober 2018 in QSim3D
  call konsum(vkigr,TEMPW,VO2,TFLIE                                  &
  &,ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl             &
  &,jiein,FopIRe,GRote,dzres1,dzres2,zresge                          &
  &,irmaxe,zexki,zexgr,zexbl                                         &
  &,aki,agr,abl,iwied,rmuas,iras,TGZoo,BAC,zBAC                      &
  &,rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
  &,algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
  &,itags,uhrz,mstr,azStrs                                           &                                                   
  &,kontroll ,iglob ) !!wy  

!version qsim13.301_28mae18wy_3D!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!  call konsum(vkigr,TEMPW,VO2,TFLIE                                  &
!  &,ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl             &
!  &,jiein,FopIRe,GRote,dzres1,dzres2,zresge                          &
!  &,irmaxe,zexki,zexgr,zexbl                                         &
!  &,aki,agr,abl,iwied,rmuas,iras,TGZoo,BAC,zBAC                      &
!  &,rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
!  &,algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
!  &,itags,uhrz,mstr,azStrs,kontroll,iglob)
!Version 13.30 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!  call konsum(vkigr,TEMPW,VO2,TFLIE                            &
!  &,ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl             &
!  &,jiein,FopIRe,GRote,dzres1,dzres2,zresge                          &
!  &,irmaxe,zexki,zexgr,zexbl                                         &
!  &,aki,agr,abl,iwied,rmuas,iras                                     &
!  &,rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
!  &,algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
!  &,itags,uhrz,mstr,kontroll,iglob)                                               
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

! vkigr,TEMPW,VO2,TFLIE  
! ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl
      planktonic_variable_p(50+(i-1)*number_plankt_vari) = zooind(1) ! Anzahl der Rotatorien
      planktonic_variable_p(76+(i-1)*number_plankt_vari) = TGZoo(1,1)  ! Zooplanktongewicht
      transfer_quantity_p(6+(i-1)*number_trans_quant) = abszo(1) ! Absterberate Zooplankton
      transfer_quantity_p(42+(i-1)*number_trans_quant) = ir(1) ! Ingestionsrate der Rotatorien in mg/(l*h) | konsum()

! jiein,FopIRe,GRote,dzres1,dzres2,zresge
      transfer_quantity_p(27+(i-1)*number_trans_quant) = dzres1(1) ! Grund-Respiration Konsumenten
      transfer_quantity_p(28+(i-1)*number_trans_quant) = dzres2(1) ! Fraßabhängige Respirationsrate Konsumenten

! irmaxe,zexki,zexgr,zexbl                                         &
      ! IRMAXe ! max. Gewichtsspez. Algenaufnahmerate d. Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
      transfer_quantity_p(16+(i-1)*number_trans_quant) = zexki(1) 
      ! Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen
      transfer_quantity_p(17+(i-1)*number_trans_quant) = zexgr(1) ! Grünalgen
      transfer_quantity_p(18+(i-1)*number_trans_quant) = zexbl(1) ! Blaualgen

! aki,agr,abl,iwied,rmuas,iras                                     &
      transfer_quantity_p(76+(i-1)*number_trans_quant) = rmuas(1) !  = mueRot-respRg ! Nettowachstumsrate Rotatorien ? Ausgabeparameter
      transfer_quantity_p(79+(i-1)*number_trans_quant) = iras(1)  !  Ausgabe Ingestionsrate

! rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
      transfer_quantity_p(77+(i-1)*number_trans_quant) = rakr(1)  !  = iras(ior)*respaR ! Fraßabhängige Respiration ? Ausgabeparameter
      transfer_quantity_p(78+(i-1)*number_trans_quant) = rbar(1)  !  = respRg ! GRund?-Respiration ? Ausgabeparameter
      transfer_quantity_p(74+(i-1)*number_trans_quant) = zHNF(1) ! Aufnahmerate der HNF   
      transfer_quantity_p(91+(i-1)*number_trans_quant) = zBAC(1) ! Aufnahmerate der BAC   
      transfer_quantity_p(75+(i-1)*number_trans_quant) = HNFza(1) !   Ausgabe: HNFza(ior) = (zHNF(ior)/CHNF(ior))*24. 
      transfer_quantity_p(53+(i-1)*number_trans_quant) = algzok(1) ! kiesel-Algen-Konsum Zoo-Plankton in mg/l

! algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
      transfer_quantity_p(72+(i-1)*number_trans_quant) = algzog(1) ! gruen-Algen-Konsum Zoo-Plankton in mg/l
      transfer_quantity_p(73+(i-1)*number_trans_quant) = algzob(1) ! blau-Algen-Konsum Zoo-Plankton in mg/l
      do j=1,num_lev_trans
         trans_quant_vert_p(j+(26-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzkz(j,1)  ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l 
         trans_quant_vert_p(j+(27-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzgz(j,1) ! Grün-Algen-Konsum durch Zoo-Plankton in mg/l 
         trans_quant_vert_p(j+(28-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzbz(j,1) ! Blau-Algen-Konsum durch Zoo-Plankton in mg/l 
      end do
! itags,mstr)  
                                             
      if(kontroll)then
         print*,'konsum nachher: tflie=',tflie,' uhrz=',uhrz
         print*,'GROTe',GROTe
         print*,'zooind,abszo,ir',zooind(1),abszo(1),ir(1)
         print*,'dzres1,dzres2',dzres1(1),dzres2(1)
         print*,'zexki,zexgr,zexbl',zexki(1),zexgr(1),zexbl(1)
         print*,'rmuas,iras',rmuas(1),iras(1)
         print*,'rakr,rbar,zHNF,HNFza,algzok',rakr(1),rbar(1),zHNF(1),HNFza(1),algzok(1)
         print*,'algzog,algzob,algzkz,algzgz(,algzbz',algzog(1),algzob(1),algzkz(1,1),algzgz(1,1),algzbz(1,1)
      end if  ! kontrolle

      RETURN 
      END subroutine konsum_huelle
                                          
!----+-----+----
!      end module konsum_module


