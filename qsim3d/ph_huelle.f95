!> \page PH-Wert pH-Wert
!!
!! <h2>Herkunft</h2>
!!     ph() \n 
!!     EIN PROGRAMM ZUR BERECHNUNG DES PH-WERTES EINES GEWAESSERS \n       
!!     aus dem m-Wert und der Kohlensaeuresumme   \n                       
!!     AUTOR:VOLKER KIRCHESCH   \n 
!!     entnommen aus Version qsim13.301_28mae18\n 
!!  
!! <h2>Teilprozesse</h2>
!! 
!! Die folgenden Quellen und Senken für CO2 werden in diesem Baustein berücksichtigt:
!! <ul>
!!    <li>Abbau organischer Kohlenstoffverbindungen siehe: \ref BSB</li>
!!    <li>Atmung (Konsumenten aber auch Algen)</li>
!!    <li>Pflanzenwachstum (CO2-Verbrauch)</li>
!!    <li>Gasaustausch über die Gewässeroberfläche</li>
!!    <li>Nitrifikation </li>
!! </ul>
!! Im 3D-Modell sind noch keine Wehre implementiert, daher kommt es auch zu keinem Co2-Austrag an Wehren wie im 1D.   \n                           
!!
!! <h2>Schnittstellenbeschreibung</h2>
!!
!! call ph()\n
!! ( \ref mw, \ref pw, \ref ca, \ref lf, \ref tempw, \ref tflie, \ref susn, \ref bsbt, \ref dalgki        &\n
!! &, \ref dalggr, \ref dalgak, \ref dalgag, \ref po2p, \ref po2r, \ref rau, \ref vmitt, \ref tiefe         &\n
!! &, \ref flae, \ref vabfl                                             &\n
!! &, \ref flag, \ref elen, \ref ior, \ref anze, \ref vph                                 &\n
!! &, \ref elfl, \ref cal, \ref qeinll, \ref iorla, \ref iorle, \ref ieinls                     &\n
!! &, \ref ssalg, \ref stind, \ref albewg                                     &\n
!! &, \ref alberg, \ref albewk, \ref alberk, \ref wge                               &\n
!! &, \ref abl, \ref dalgbl, \ref dalgab, \ref idwe, \ref iwied, \ref fkm, \ref ij, \ref resdr              &\n
!! &, \ref dzres1, \ref dzres2, \ref aki, \ref agr                                  &\n
!! &, \ref ilbuhn, \ref eph, \ref emw, \ref elf, \ref eca, \ref vco2, \ref qeinl, \ref jiein                &\n
!! &, \ref mstr, \ref cpfad, \ref rhyd, \ref wlage, \ref hws, \ref itags, \ref monats, \ref uhrz                           &\n
!! &, \ref azstrs, \ref iphy , \ref kontroll , \ref iglob )\n
!! \n
!! Die QSim3D Subroutine ph_huelle() dient dem Aufruf der QSimD-subroutine ph(). 
!! (Zum Hüllroutinen-Konzept siehe: \ref hüllen )
!!
!! <h2>Rand und Anfangsbedingungen</h2>
!! Ergänzung des P-Wertes im Zufluss mit pwert(); 
!! Siehe dazu auch \ref randbedingungen_ergaenzen .
!! 
!! <h2>Dokumentation</h2>
!! Bisher ist das pH-Wert-Moduls in der
!! <a href="./pdf/QSimDoku_ncycWy.pdf" target="_blank">Kurzdoku</a> (Version vom 22. Nov. 2017) noch nicht beschrieben.
!! \n
!! Es existiert aber ein älterer
!! <a href="./pdf/PH_Verena.pdf" target="_blank">Dokumentationsentwurf</a> 
!! zum pH-Wert Modul von Verena Michalski.
!! \n\n 
!! zurück: \ref Stoffumsatz; Quelle: ph_huelle.f95


!! \n
!!<table >
!!<tr><th>Variablen-Name QSim </th><th> Beschreibung			</th><th> T-QSim Daten-Feld 		</th></tr>
!!<tr><td> mw	</td><td> m-Wert (Carbonathärte,Säurekapazität?)</td><td> planktische_variablen::planktonic_variable_p 62	</td></tr>
!!<tr><td> pw	</td><td> p-Wert (Basenkapazität ?)		</td><td> planktische_variablen::planktonic_variable_p 63	</td></tr>
!!<tr><td> ca	</td><td> Calcium				</td><td> planktische_variablen::planktonic_variable_p 64	</td></tr>
!!<tr><td> lf	</td><td> Leitfähigkeit				</td><td> planktische_variablen::planktonic_variable_p 65	</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> tempw</td><td> Wassertemperatur			</td><td> planktische_variablen::planktonic_variable_p 1	</td></tr>
!!<tr><td> tflie</td><td> Zeitschritts (d)			</td><td>				</td></tr>
!!<tr><td> susn	</td><td> SUSP.NITRIF. OXIDIERTE AMMONIUMMENGE	</td><td> uebergabe_werte::transfer_quantity_p 29	</td></tr>
!!<tr><td> bsbt	</td><td> orgc Sauerstoffverbrauch		</td><td> uebergabe_werte::transfer_quantity_p 1		</td></tr>
!!<tr><td> dalgki</td><td> Zuwachs Kiesel-Algen			</td><td> uebergabe_werte::transfer_quantity_p 20	</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> dalggr</td><td> Zuwachs Grün-Algen			</td><td> uebergabe_werte::transfer_quantity_p 21	</td></tr>
!!<tr><td> dalgak</td><td> Respiration Kiesel-Algen		</td><td> uebergabe_werte::transfer_quantity_p 23	</td></tr>
!!<tr><td> dalgag</td><td> Respiration Grün-Algen		</td><td> uebergabe_werte::transfer_quantity_p 24	</td></tr>
!!<tr><td> PO2P	</td><td> Sauerstoffproduktion durch Makrophyten</td><td> uebergabe_werte::transfer_quantity_p 30	</td></tr>
!!<tr><td> PO2R	</td><td> Sauerstoffverbrauch durch Makrophyten	</td><td> uebergabe_werte::transfer_quantity_p 31	</td></tr> 
!!<tr><td> rau	</td><td> Strickler Reibungsbeiwert		</td><td> benthische_verteilungen::benthic_distribution_p 5	</td></tr>
!!<tr><td> vmitt</td><td> Geschwindigkeit			</td><td> randbedingungen rb_hydraul_p (1		</td></tr>
!!<tr><td> tiefe</td><td> Tiefe					</td><td> randbedingungen rb_hydraul_p (2		</td></tr>
!!<tr><td> flae	</td><td> unbenutzt da keine Einleitung		</td><td>				</td></tr>
!!<tr><td> vabfl</td><td> wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.</td><td>	</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> flag </td><td> keine Einleitungen			</td><td>				</td></tr>
!!<tr><td> elen	</td><td> Elementlänge (nicht verwendet)	</td><td>				</td></tr>
!!<tr><td> anze	</td><td> Anzahl der Profile im aktuellen Strang</td><td>				</td></tr>
!!<tr><td> ior	</td><td> Laufindex				</td><td>				</td></tr>
!!<tr><td> vph	</td><td> der zu berechnende PH-Wert		</td><td> planktische_variablen::planktonic_variable_p 66	</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> ssalg</td><td> ???					</td><td> planktische_variablen::planktonic_variable_p 52	</td></tr>
!!<tr><td> stind</td><td> Zeitsumme, Funktion unklar ###	</td><td> planktische_variablen::planktonic_variable_p 59	</td></tr>
!!<tr><td> albewg</td><td> Wachstum benthischer gruen-Algen	</td><td> benthische_verteilungen::benthic_distribution_p 13	</td></tr>
!!<tr><td> alberg</td><td> Respiration benthischer gruen-Algen	</td><td> benthische_verteilungen::benthic_distribution_p 11	</td></tr>
!!<tr><td> albewk</td><td> Wachstum benthischer kiesel-Algen	</td><td> benthische_verteilungen::benthic_distribution_p 14	</td></tr>
!!<tr><td> alberk</td><td> Respiration benthischer kiesel-Algen	</td><td> benthische_verteilungen::benthic_distribution_p 12	</td></tr>
!!<tr><td> wge	</td><td> Windgeschwindigkeit aus Wetterstationsdaten</td><td> wetter::wge_T		</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> abl	</td><td> Anteil blau-Algen			</td><td> planktische_variablen::planktonic_variable_p 10	</td></tr>
!!<tr><td> dalgbl</td><td> Zuwachs Blau-Algen			</td><td> uebergabe_werte::transfer_quantity_p 22	</td></tr>
!!<tr><td> dalgab</td><td> Respiration Blau-Algen		</td><td> uebergabe_werte::transfer_quantity_p 25	</td></tr>
!!<tr><td> idwe	</td><td> sieht nur eine Wetterstation Nr.1	</td><td> #### Vorsicht ####		</td></tr>
!!<tr><td> iwied</td><td> unbenutzte Variable			</td><td>				</td></tr>
!!<tr><td> fkm	</td><td> Flusskilometer unbenutzt		</td><td>				</td></tr>
!!<tr><td> ij	</td><td> unbenutzte Variable			</td><td>				</td></tr>
!!<tr><td> resdr</td><td> Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)</td><td> benthische_verteilungen::benthic_distribution_p 15</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> dzres1</td><td> Grund-Respiration Konsumenten	</td><td> uebergabe_werte::transfer_quantity_p 27	</td></tr>
!!<tr><td> dzres2</td><td> Fraßabhängige Respirationsrate Konsumenten</td><td> uebergabe_werte::transfer_quantity_p 28	</td></tr>
!!<tr><td> agr	</td><td> Anteil gruen-Algen			</td><td> planktische_variablen::planktonic_variable_p  9	</td></tr>
!!<tr><td> aki	</td><td> Anteil kiesel-Algen			</td><td> planktische_variablen::planktonic_variable_p  8	</td></tr>
!!<tr><td> ilbuhn</td><td> keine Buhnen				</td><td>				</td></tr>
!!<tr><td> eph	</td><td> keine Einleitung			</td><td>				</td></tr>
!!<tr><td> emw	</td><td> keine Einleitung			</td><td>				</td></tr>
!!<tr><td> elf	</td><td> keine Einleitung			</td><td>				</td></tr>
!!<tr><td> eca	</td><td> keine Einleitung			</td><td>				</td></tr>
!!<tr><td> vco2	</td><td> Kohlendioxyd				</td><td> uebergabe_werte::transfer_quantity_p 26		</td></tr>
!!<tr><td> qeinl</td><td> kein Abfluss Einleitung		</td><td>				</td></tr>
!!<tr><td> jiein</td><td> null Punkt-Einleitungen		</td><td>				</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> mstr	</td><td> Strangzähler				</td><td>				</td></tr>
!!<tr><td> cpfad</td><td> (unbenutzt)				</td><td>				</td></tr>
!!<tr><td> itags</td><td> Tag im Monat 	(unbenutzt)		</td><td>				</td></tr>
!!<tr><td> monats</td><td> Monat im Jahr(unbenutzt)		</td><td>				</td></tr>
!!<tr><td> uhrz	</td><td> Uhrzeit  	(unbenutzt)		</td><td>				</td></tr>
!!<tr><td>	</td><td>					</td><td>				</td></tr>
!!</table>
!! \n\n 
!! zurück: \ref Stoffumsatz; Quelle: ph_huelle.f95

!--------------------------------------------------------------------------------------------------------------- orgc

!http://de.wikipedia.org/wiki/Wasseranalyse

!https://www.dew21.de/default.aspx?g=621&l=1031&r=-1&t=101638&lz=M&on=479592&a=11&z=m-Wert&id=479592&z2=
!m-Wert
!Der m-Wert ist eine wasserchemische Rechengröße, die aus den Konzentrationen der Hydrogencarbonat- und Carbonationen sowie den bei der Dissoziation des Wassers entstehenden H+- undOH--Ionen ermittelt wird. Er ist hilfreich bei der Berechnung von Mischwasseranalysen und bei der Vorausberechnung von Änderungen der Wasserzusammensetzung infolge von Aufbereitungsmaßnahmen.
!Das m erinnert daran, dass man früher Methylorange als Farbindikator für eine Titration mit Salzsäure verwendet hat, die etwa gleichbedeutend ist mit der heutigen Bestimmung der Säurekapazität bis pH 4,3. Letztere ist annähernd gleich dem m-Wert.

!> Das module ph_module widmet sich dem \ref PH-Wert (sauer <-> basisch)
!! \n\n 
!! zurück: \ref Stoffumsatz; Quelle: ph_huelle.f95
!      module ph_module
!      implicit none
!      save
!      PUBLIC :: ph_huelle , ini_ph
!      CONTAINS

!> Die Subroutine ph_huelle() dient dem Aufruf der QSim-subroutine ph(). \n\n
!! Diese dient der Berechnung des \ref PH-Wert
!! \n\n 
!! Quelle: ph_huelle.f95
      SUBROUTINE ph_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none
      integer :: i, i2, iii

!> i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
      iglob=(i+meinrang*part)
      if (iglob.gt. knotenanzahl2D) return ! überstehende Nummern nicht bearbeiten.
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe: 

      ! planktische variablen
      mw(1) = planktonic_variable_p(62+(i-1)*number_plankt_vari) ! m-Wert
      mw(2) = mw(1)
      pw(1) = planktonic_variable_p(63+(i-1)*number_plankt_vari) ! p-Wert
      pw(2) = pw(1)
      ca(1) = planktonic_variable_p(64+(i-1)*number_plankt_vari) ! Calium ?
      ca(2) = ca(1)
      lf(1) = planktonic_variable_p(65+(i-1)*number_plankt_vari) ! Leitfähigkeit
      lf(2) = lf(1)

      ! wasser-milieu
      tempw(1) = planktonic_variable_p( 1+(i-1)*number_plankt_vari)  ! Wassertemperatur
      !tflie = 1      ! Zeitschrittdauer in d

      tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)

      susn(1) = transfer_quantity_p(29+(i-1)*number_trans_quant) ! Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE
      susn(2) = susn(1)
      bsbt(1) = transfer_quantity_p(1+(i-1)*number_trans_quant) !orgc Sauerstoffverbrauch
      bsbt(2) = bsbt(1)
      dalgki(1) = transfer_quantity_p(20+(i-1)*number_trans_quant) ! Zuwachs Kiesel-Algen
      dalgki(2) = dalgki(1)
      dalggr(1) = transfer_quantity_p(21+(i-1)*number_trans_quant) ! Zuwachs Grün-Algen
      dalggr(2) = dalggr(1) 
      dalgak(1) = transfer_quantity_p(23+(i-1)*number_trans_quant) ! Respiration Kiesel-Algen
      dalgak(2) = dalgak(1)
      dalgag(1) = transfer_quantity_p(24+(i-1)*number_trans_quant) ! Respiration Grün-Algen
      dalgag(2) = dalgag(1)
      PO2P(1) = transfer_quantity_p(30+(i-1)*number_trans_quant) ! Sauerstoffproduktion durch Makrophyten
      PO2P(2) = PO2P(1)
      PO2R(1) = transfer_quantity_p(31+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Makrophyten
      PO2R(2) = PO2R(1)
      ! Hydraulik
      tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! water_depth(i) 
      rau(1:2)= strickler( zone(point_zone(iglob))%reib , tiefe(1) )
      vmitt(1:2) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! vel_mag(i) 
      flae(1:2) = 1000.0 !! unbenutzt da keine Einleitung
      vabfl(1:2) = 2.5 !! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.

      ! ph so benutzen , dass nur der 1. Strang mit nur einem Knoten/Profil berechnet wird
      flag(1)=0         ! keine Einleitungen
      flag(2)=flag(1)
      elen(1)=1         ! Elementlänge (nicht verwendet)
      elen(2)=elen(1)
      anze=1            ! Anzahl der Profile im aktuellen Strang
      ior=1             ! Laufindex

      vph(1) = planktonic_variable_p(66+(i-1)*number_plankt_vari) ! der zu berechnende PH-Wert
      vph(2) = vph(1)

      qeinlL(1)=0.0           ! Zufluss Linienquelle; nicht verwendet
      iorLa(1)=0              ! AnfangsKnoten der Linienquelle; nicht verwendet
      iorLe(1)=0              ! EndKnoten der Linienquelle; nicht verwendet
      ieinLs(1)=0             ! keine Linienquellen
      ieinLs(2)=ieinLs(1)
      elfL(1)=0.0             ! keine Linienquellen
      CaL(1)=0.0              ! keine Linienquellen

      ssalg(1) = planktonic_variable_p(52+(i-1)*number_plankt_vari) ! 
      ssalg(2) = ssalg(1)
      stind(1) = planktonic_variable_p(59+(i-1)*number_plankt_vari) ! 
      stind(2) = stind(1)
      albewg(1)=benthic_distribution_p(13+(i-1)*number_benth_distr) ! Wachstum benthischer gruen-Algen
      albewg(2) = albewg(1)
      alberg(1)=benthic_distribution_p(11+(i-1)*number_benth_distr) ! Respiration benthischer gruen-Algen
      alberg(2) = alberg(1)
      albewk(1)=benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
      albewk(2) = albewk(1)
      alberk(1)=benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
      alberk(2) = alberk(1)
      i2=zone(point_zone(iglob))%wettstat%wetterstations_nummer !! ist parallel !!!
      wge(1:2)=wge_T(i2)        ! Windgeschwindigkeit  aus Wetterstationsdaten

      abl(1) = planktonic_variable_p(10+(i-1)*number_plankt_vari) ! Anteil blau-Algen
      abl(2) = abl(1)
      dalgbl(1) = transfer_quantity_p(22+(i-1)*number_trans_quant) ! Zuwachs Blau-Algen
      dalgbl(2) = dalgbl(1)
      dalgab(1) = transfer_quantity_p(25+(i-1)*number_trans_quant) ! Respiration Blau-Algen
      dalgab(2) = dalgab(1)
      idwe(1,1)= 1            ! Eigentlich Wetterstationsnummer ,muss aber 1 sein, 
      ! weil in typw(1) wge(1) ro(1) die Daten der aktuellen Wetterstation in 1 haben
      idwe(1,2)=idwe(1,1)
      iwied=0      ! unbenutzte Variable
      fkm (1)=0.0  ! Flusskilometer unbenutzt
      ij=0         ! unbenutzte Variable
      resdr(1)=benthic_distribution_p(15+(i-1)*number_benth_distr) ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
      resdr(2) = resdr(1)
                                  
      dzres1(1) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
      dzres1(2) = dzres1(1)
      dzres2(1) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten
      dzres2(2) = dzres2(1)
      agr(1) = planktonic_variable_p( 9+(i-1)*number_plankt_vari) ! 
      agr(2) = agr(1)
      aki(1) = planktonic_variable_p( 8+(i-1)*number_plankt_vari) ! 
      aki(2) = aki(1)

      ilbuhn=0          ! keine Buhnen
      eph(1) = 0.0      ! keine Einleitung
      emw(1) = 0.0      ! keine Einleitung
      elf(1) = 0.0      ! keine Einleitung
      eca(1) = 0.0      ! keine Einleitung
      vco2(1) = transfer_quantity_p(26+(i-1)*number_trans_quant) ! Kohlendioxyd
      vco2(2) = vco2(1)
      qeinl(1)=0.0      ! kein Abfluss Einleitung
      jiein(1)=0        ! null Punkt-Einleitungen

      mstr=1            ! Strangzähler
      !cpfad            ! (unbenutzt)
      rhyd(1:2) = tiefe(1) ! hydraulischer Radius | sinnvollste Annahme im mehrdimensionalen
      Wlage(1,1:2)=zone(point_zone(iglob))%wettstat%wetterstations_lage ! Höhenlage der zuständigen Wetterstation mü.NHN 
      hWS(1,1:2)= rb_hydraul_p(3+(i-1)*number_rb_hydraul) ! Wasserspiegellage, von holen_trans() gesetzt
      itags=tag           ! Tag im Monat module::modell zeitsekunde() 	(unbenutzt)
      monats=monat          ! Monat im Jahr module::modell zeitsekunde() (unbenutzt)
      uhrz=uhrzeit_stunde ! Uhrzeit module::modell zeitsekunde() (unbenutzt)
      ! azStrs=1 - aus QSimDatenfelder
      ! iphy direkt aus module_modell
      kontroll=iglob.eq.kontrollknoten
	  iii=20; if(kontroll) iii=0
      do 
	  	 mw(1:2)=planktonic_variable_p(62+(i-1)*number_plankt_vari)
		 pw(1:2 )=planktonic_variable_p(63+(i-1)*number_plankt_vari)
         ca(1:2) =(real(iii)/20.0)*planktonic_variable_p(64+(i-1)*number_plankt_vari)
         lf(1:2) =planktonic_variable_p(65+(i-1)*number_plankt_vari)
         vph(1:2)=planktonic_variable_p(66+(i-1)*number_plankt_vari)
         if(kontroll) print*,'ph  vorher: vph,mw,pw,ca,lf,vco2',vph(1),mw(1),pw(1),ca(1),lf(1),vco2(1)
	  
	  ! vph,mw,(pw),ca,vco2
	  ! lf,tempw
	  ! rau,vmitt,tiefe,flae,vabfl,wge # Belüftung
	  ! po2p,po2r # produktion und respiration der Macrophyten
	  ! bsbt # bakterieller Abbau org. Kohlenstoffe
	  ! dalgki,dalggr,dalgbl | dalgak,dalgag,dalgab
	  ! albewg,alberg,albewk,alberk
	  ! dzres1,dzres2,resdr
	  ! susn

!qsim13.40 15okt18 ###############################################################################################
      call ph(mw,pw,ca,lf,tempw,tflie,susn,bsbt,dalgki        &
     &,dalggr,dalgak,dalgag,po2p,po2r,rau,vmitt,tiefe         &
     &,flae,vabfl                                             &
     &,flag,elen,ior,anze,vph                                 &
     &,elfL,caL,qeinlL,iorLa,iorLe,ieinLs                     &
     &,ssalg,stind,albewg                                     &
     &,alberg,albewk,alberk,wge                               &
     &,abl,dalgbl,dalgab,IDWe,iwied,fkm,ij,resdr              &
     &,dzres1,dzres2,aki,agr                                  &
     &,ilbuhn,eph,emw,elf,eca,vco2,qeinl,jiein                &
     &,mstr,cpfad,rhyd,WLage,hWS,itags,monats,uhrz            &
     &,azStrs,iphy   ,kontroll ,iglob )
             
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Daten-rückgabe: 
	     if(kontroll) print*,'ph nachher: vph,mw,pw,ca,lf,vco2',vph(1),mw(1),pw(1),ca(1),lf(1),vco2(1)
		 iii=iii+1
		 if (iii.gt.20)exit
      enddo !permutation am Kontrollknoten

      ! Transportkonzentrationen zurückschreiben
      planktonic_variable_p(62+(i-1)*number_plankt_vari) = mw(1) !
      planktonic_variable_p(63+(i-1)*number_plankt_vari) = pw(1) ! 
      planktonic_variable_p(64+(i-1)*number_plankt_vari) = ca(1) ! 
      planktonic_variable_p(65+(i-1)*number_plankt_vari) = lf(1) !  ### wie geht das mit dem Salzgehalt zusammen????
      planktonic_variable_p(66+(i-1)*number_plankt_vari) = vph(1) ! 
      planktonic_variable_p(59+(i-1)*number_plankt_vari) = stind(1) ! ??? Minutenzähler Bedeutung sehr unklar; Versuch einer Altersvariablen?
      ! Übergabekonzentrationen Rückgabewerte
      transfer_quantity_p(26+(i-1)*number_trans_quant) = vco2(1) ! Kohlendioxyd

      RETURN 
      END subroutine ph_huelle
!----+-----+----
!> die Subroutine ini_ph() steht in der datei ph_huelle.f95 \n
!! sie schreibt zunächst Nullen (ph=7) in die ph-variablen (mw,pw,ca,lf). \n
!! 
!! ### ausgeschaltet in initialisieren() ### Vorbelegung durch randwerte
      SUBROUTINE ini_ph()
      use modell                                                 
      implicit none
      integer i

            do i=1,number_plankt_point
               planktonic_variable(62+(i-1)*number_plankt_vari) = 0.0 ! 1.5 ! mw 2.5
               planktonic_variable(63+(i-1)*number_plankt_vari) = 0.0 ! pw
               planktonic_variable(64+(i-1)*number_plankt_vari) = 0.0 ! 41.0 ! ca  41.0
               planktonic_variable(65+(i-1)*number_plankt_vari) = 0.0 ! 402.0 ! lf #### momentan von ini_salz gesetzt ???402.0
               planktonic_variable(66+(i-1)*number_plankt_vari) = 7.0 ! vph 
            end do

      RETURN 
      END subroutine ini_ph
                                          
!----+-----+----
!>  \page ph_aufteilung Ergänzung P-Wert
!! siehe: ph_aufteilung_einleitung(), von wo QSim Subroutine pweinl() aufgerufen wird \n
!! Kontext: \ref PH-Wert

!> die Subroutine ph_aufteilung_einleitung() sthet in: ph_huelle.f95\n
!! Sie dient dazu am Rand den fehlenden p-Wert durch Zuhilfenahme der QSim Subroutine pweinl() zu berechnen. \n
!! läuft nur auf prozessor 0 \n
!! 
      SUBROUTINE ph_aufteilung_einleitung(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none
      integer :: i
      real, Dimension(azStrs,100)    :: mws, vphs, lfs, tempws, pws 

      tempws(1,1) = planktonic_variable( 1+(i-1)*number_plankt_vari)  ! Wassertemperatur
      mws(1,1) = planktonic_variable(62+(i-1)*number_plankt_vari) ! m-Wert
      vphs(1,1) = planktonic_variable(66+(i-1)*number_plankt_vari) ! PH-Wert
      lfs(1,1) = planktonic_variable(65+(i-1)*number_plankt_vari) ! Leitfähigkeit
      mstr = 1        ! Strangzähler

!     Berechnung des p-Wertes am Start (ohne Algen)                     
!                                                                       
      call pwert(mws,vphs,lfs,tempws,pws,  1,  mstr,  azStrs) ! mRB=1
!                                                                       
      planktonic_variable(63+(i-1)*number_plankt_vari) = pws(1,1) ! p-Wert Rückgabe

      RETURN 
      END subroutine ph_aufteilung_einleitung
      !call pweinl(mw,vph,lf,tempw,pw,ior) 
                                          
!----+-----+----
!      end module ph_module

!! <h2>Modellierte Teilprozesse:</h2>
!! <ul>
!!   <li>   Berechnung der Kohlensaeuresumme in mol/l  \n  
!!   \f$ moca = ca/(1000.*40.08) \f$\n
!!   \f$ c = mw*1.e-3 - pw*1.e-3 \f$\n
!!   </li>                   
!!   <li>  Berechnung der absoluten Temperatur \n  
!!   \f$ abst = tempw(ior)+273.16 \f$\n
!!   </li>                   
!!   <li>   Berechnung der negativen Logarithmen der Dissoziationskonstantenl 
!!     bei einer Ionenstaerke von 0 mol/l                                
!!     in Abhaengigkeit von der abs. Temperatur  \n                        
!!   \f$ pk1 = (17052./abst)+215.21*log10(abst)-0.12675*abst-545.56  \f$\n
!!   \f$ pk2 = (2902.39/abst)+0.02379*abst-6.498  \f$\n
!!   \f$ pkw = (4471.33/abst)+0.017053*abst-6.085 \f$\n
!!   \f$ kca = 9.41e-9-2.52e-10*tempw(ior)+2.76e-12*tempw(ior)**2 -1.14e-14*tempw(ior)**3   \f$\n                                        
!!   \f$ pkca = (alog10(kca))*(-1.)  = f(T)\f$\n
!!   </li>                   
!!   <li>   Einfluss der Ionenstärke                                         
!!   \f$ mue = 1.7e-5*lf(ior) \f$\n
!!   \f$ lgk1 = sqrt(mue)/(1.+1.4*sqrt(mue)) \f$\n
!!   \f$ lgk2 = (2.*sqrt(mue))/(1.+1.4*sqrt(mue)) \f$\n
!!   \f$ lgkca = (4.*sqrt(mue))/(1.+3.9*sqrt(mue)) \f$\n
!!   \f$ hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) \f$\n
!!   </li>                   
!! </ul>
!! \n\n 

