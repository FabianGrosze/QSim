!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------
! !> \page PH-Wert pH-Wert
! !!
! !! Nachfolge: Markdown Doku \subpage ph_doku
! !!  
! !! <h2>Teilprozesse</h2>
! !! 
! !! Die folgenden Quellen und Senken für CO2 werden in diesem Baustein berücksichtigt:
! !! <ul>
! !!    <li>Abbau organischer Kohlenstoffverbindungen siehe: \ref BSB</li>
! !!    <li>Atmung (Konsumenten aber auch Algen)</li>
! !!    <li>Pflanzenwachstum (CO2-Verbrauch)</li>
! !!    <li>Gasaustausch über die Gewässeroberfläche</li>
! !!    <li>Nitrifikation </li>
! !! </ul>
! !! Im 3D-Modell sind noch keine Wehre implementiert, daher kommt es auch zu keinem Co2-Austrag an Wehren wie im 1D. \n
! !!
! !!
! !! 
! !! \n
! !!<table >
! !!<tr><th>Variablen-Name QSim </th><th> Beschreibung			</th><th> T-QSim Daten-Feld 		</th></tr>
! !!<tr><td> mw	</td><td> m-Wert (Carbonathärte,Säurekapazität?)</td><td> planktische_variablen::planktonic_variable_p 62	</td></tr>
! !!<tr><td> pw	</td><td> p-Wert (Basenkapazität ?)		</td><td> planktische_variablen::planktonic_variable_p 63	</td></tr>
! !!<tr><td> ca	</td><td> Calcium				</td><td> planktische_variablen::planktonic_variable_p 64	</td></tr>
! !!<tr><td> lf	</td><td> Leitfähigkeit				</td><td> planktische_variablen::planktonic_variable_p 65	</td></tr>
! !!<tr><td> </td><td> </td><td> </td></tr>
! !!<tr><td> tempw</td><td> Wassertemperatur			</td><td> planktische_variablen::planktonic_variable_p 1	</td></tr>
! !!<tr><td> tflie</td><td> Zeitschritts (d)			</td><td>				</td></tr>
! !!<tr><td> susn	</td><td> SUSP.NITRIF. OXIDIERTE AMMONIUMMENGE	</td><td> uebergabe_werte::transfer_quantity_p 29	</td></tr>
! !!<tr><td> bsbt	</td><td> orgc Sauerstoffverbrauch		</td><td> uebergabe_werte::transfer_quantity_p 1		</td></tr>
! !!<tr><td> dalgki</td><td> Zuwachs Kiesel-Algen			</td><td> uebergabe_werte::transfer_quantity_p 20	</td></tr>
! !!<tr><td> </td><td> </td><td> </td></tr>
! !!<tr><td> dalggr</td><td> Zuwachs Grün-Algen			</td><td> uebergabe_werte::transfer_quantity_p 21	</td></tr>
! !!<tr><td> dalgak</td><td> Respiration Kiesel-Algen		</td><td> uebergabe_werte::transfer_quantity_p 23	</td></tr>
! !!<tr><td> dalgag</td><td> Respiration Grün-Algen		</td><td> uebergabe_werte::transfer_quantity_p 24	</td></tr>
! !!<tr><td> PO2P	</td><td> Sauerstoffproduktion durch Makrophyten</td><td> uebergabe_werte::transfer_quantity_p 30	</td></tr>
! !!<tr><td> PO2R	</td><td> Sauerstoffverbrauch durch Makrophyten	</td><td> uebergabe_werte::transfer_quantity_p 31	</td></tr> 
! !!<tr><td> rau	</td><td> Strickler Reibungsbeiwert		</td><td> benthische_verteilungen::benthic_distribution_p 5	</td></tr>
! !!<tr><td> vmitt</td><td> Geschwindigkeit			</td><td> randbedingungen rb_hydraul_p (1		</td></tr>
! !!<tr><td> tiefe</td><td> Tiefe					</td><td> randbedingungen rb_hydraul_p (2		</td></tr>
! !!<tr><td> flae	</td><td> unbenutzt da keine Einleitung		</td><td>				</td></tr>
! !!<tr><td> vabfl</td><td> wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.</td><td>	</td></tr>
! !!<tr><td> </td><td> </td><td> </td></tr>
! !!<tr><td> flag </td><td> keine Einleitungen			</td><td>				</td></tr>
! !!<tr><td> elen	</td><td> Elementlänge (nicht verwendet)	</td><td>				</td></tr>
! !!<tr><td> anze	</td><td> Anzahl der Profile im aktuellen Strang</td><td>				</td></tr>
! !!<tr><td> ior	</td><td> Laufindex				</td><td>				</td></tr>
! !!<tr><td> vph	</td><td> der zu berechnende PH-Wert		</td><td> planktische_variablen::planktonic_variable_p 66	</td></tr>
! !!<tr><td> </td><td> </td><td> </td></tr>
! !!<tr><td> ssalg</td><td> ???					</td><td> planktische_variablen::planktonic_variable_p 52	</td></tr>
! !!<tr><td> stind</td><td> Zeitsumme, Funktion unklar ###	</td><td> planktische_variablen::planktonic_variable_p 59	</td></tr>
! !!<tr><td> albewg</td><td> Wachstum benthischer gruen-Algen	</td><td> benthische_verteilungen::benthic_distribution_p 13	</td></tr>
! !!<tr><td> alberg</td><td> Respiration benthischer gruen-Algen	</td><td> benthische_verteilungen::benthic_distribution_p 11	</td></tr>
! !!<tr><td> albewk</td><td> Wachstum benthischer kiesel-Algen	</td><td> benthische_verteilungen::benthic_distribution_p 14	</td></tr>
! !!<tr><td> alberk</td><td> Respiration benthischer kiesel-Algen	</td><td> benthische_verteilungen::benthic_distribution_p 12	</td></tr>
! !!<tr><td> wge	</td><td> Windgeschwindigkeit aus Wetterstationsdaten</td><td> wetter::wge_T		</td></tr>
! !!<tr><td> </td><td> </td><td> </td></tr>
! !!<tr><td> abl	</td><td> Anteil blau-Algen			</td><td> planktische_variablen::planktonic_variable_p 10	</td></tr>
! !!<tr><td> dalgbl</td><td> Zuwachs Blau-Algen			</td><td> uebergabe_werte::transfer_quantity_p 22	</td></tr>
! !!<tr><td> dalgab</td><td> Respiration Blau-Algen		</td><td> uebergabe_werte::transfer_quantity_p 25	</td></tr>
! !!<tr><td> idwe	</td><td> sieht nur eine Wetterstation Nr.1	</td><td> #### Vorsicht ####		</td></tr>
! !!<tr><td> iwied</td><td> unbenutzte Variable			</td><td>				</td></tr>
! !!<tr><td> fkm	</td><td> Flusskilometer unbenutzt		</td><td>				</td></tr>
! !!<tr><td> ij	</td><td> unbenutzte Variable			</td><td>				</td></tr>
! !!<tr><td> resdr</td><td> Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)</td><td> benthische_verteilungen::benthic_distribution_p 15</td></tr>
! !!<tr><td> </td><td> </td><td> </td></tr>
! !!<tr><td> dzres1</td><td> Grund-Respiration Konsumenten	</td><td> uebergabe_werte::transfer_quantity_p 27	</td></tr>
! !!<tr><td> dzres2</td><td> Fraßabhängige Respirationsrate Konsumenten</td><td> uebergabe_werte::transfer_quantity_p 28	</td></tr>
! !!<tr><td> agr	</td><td> Anteil gruen-Algen			</td><td> planktische_variablen::planktonic_variable_p  9	</td></tr>
! !!<tr><td> aki	</td><td> Anteil kiesel-Algen			</td><td> planktische_variablen::planktonic_variable_p  8	</td></tr>
! !!<tr><td> ilbuhn</td><td> keine Buhnen				</td><td>				</td></tr>
! !!<tr><td> eph	</td><td> keine Einleitung			</td><td>				</td></tr>
! !!<tr><td> emw	</td><td> keine Einleitung			</td><td>				</td></tr>
! !!<tr><td> elf	</td><td> keine Einleitung			</td><td>				</td></tr>
! !!<tr><td> eca	</td><td> keine Einleitung			</td><td>				</td></tr>
! !!<tr><td> vco2	</td><td> Kohlendioxyd				</td><td> uebergabe_werte::transfer_quantity_p 26		</td></tr>
! !!<tr><td> qeinl</td><td> kein Abfluss Einleitung		</td><td>				</td></tr>
! !!<tr><td> jiein</td><td> null Punkt-Einleitungen		</td><td>				</td></tr>
! !!<tr><td> </td><td> </td><td> </td></tr>
! !!<tr><td> mstr	</td><td> Strangzähler				</! td><td>				</td></tr>
! !!<tr><td> cpfad</td><td> (unbenutzt)				</td><td>				</td></tr>
! !!<tr><td> itags</td><td> Tag im Monat 	(unbenutzt)		</td><td>				</td></tr>
! !!<tr><td> monats</td><td> Monat im Jahr(unbenutzt)		</td><td>				</td></tr>
! !!<tr><td> uhrz	</td><td> Uhrzeit  	(unbenutzt)		</td><td>				</td></tr>
! !!<tr><td>	</td><td>					</td><td>				</td></tr>
! !!</table>
! !! \n\n 
! !! zurück: \ref lnk_ueberblick; Quelle: ph_huelle.f95

!--------------------------------------------------------------------------------------------------------------- orgc

!http://de.wikipedia.org/wiki/Wasseranalyse

!https://www.dew21.de/default.aspx?g=621&l=1031&r=-1&t=101638&lz=M&on=479592&a=11&z=m-Wert&id=479592&z2=
!m-Wert
!Der m-Wert ist eine wasserchemische Rechengröße, die aus den Konzentrationen der Hydrogencarbonat- und Carbonationen sowie den bei der Dissoziation des Wassers entstehenden H+- undOH--Ionen ermittelt wird. Er ist hilfreich bei der Berechnung von Mischwasseranalysen und bei der Vorausberechnung von Änderungen der Wasserzusammensetzung infolge von Aufbereitungsmaßnahmen.
!Das m erinnert daran, dass man früher Methylorange als Farbindikator für eine Titration mit Salzsäure verwendet hat, die etwa gleichbedeutend ist mit der heutigen Bestimmung der Säurekapazität bis pH 4,3. Letztere ist annähernd gleich dem m-Wert.

!> Das module ph_module widmet sich dem \ref lnk_ph (sauer <-> basisch)
!! \n\n 
!! zurück: \ref lnk_ueberblick; Quelle: ph_huelle.f95
!      module ph_module
!      implicit none
!      save
!      PUBLIC :: ph_huelle , ini_ph
!      CONTAINS

!> Die Subroutine ph_huelle() dient dem Aufruf der QSim-subroutine ph(). \n\n
!! Diese dient der Berechnung des \ref lnk_ph
!! \n\n 
!! Quelle: ph_huelle.f95
      SUBROUTINE ph_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none
      integer :: i
      real tiefes,raus,flaes

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe: 

      ! planktische variablen
      !mw(1:2) = planktonic_variable_p(62+(i-1)*number_plankt_vari) ! m-Wert
      !pw(1:2) = planktonic_variable_p(63+(i-1)*number_plankt_vari) ! p-Wert
      !ca(1:2) = planktonic_variable_p(64+(i-1)*number_plankt_vari) ! Calium ?
      !lf(1:2) = planktonic_variable_p(65+(i-1)*number_plankt_vari) ! Leitfähigkeit
      !vph(1:2) = planktonic_variable_p(66+(i-1)*number_plankt_vari) ! der zu berechnende PH-Wert
      !vco2(1:2) = transfer_quantity_p(26+(i-1)*number_trans_quant) ! Kohlendioxyd
      !tempw(1:2) = planktonic_variable_p( 1+(i-1)*number_plankt_vari)  ! Wassertemperatur

      !tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
      ! Hydraulik
      !rau(1:2)= strickler( zone(point_zone(iglob))%reib , tiefe(1) )
      !vmitt(1:2) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! vel_mag(i) 
      !flae(1:2) = 1000.0 !! unbenutzt da keine Einleitung
      !rhyd(1:2) = tiefe(1) ! hydraulischer Radius | sinnvollste Annahme im mehrdimensionalen
	  !tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! water_depth(i) 
      !Wlage(1,1:2)=zone(point_zone(iglob))%wettstat%wetterstations_lage ! Höhenlage der zuständigen Wetterstation mü.NHN 
      !hWS(1,1:2)= rb_hydraul_p(3+(i-1)*number_rb_hydraul) ! Wasserspiegellage, von holen_trans() gesetzt
      !wge(1:2)=wge_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer)        ! Windgeschwindigkeit  aus Wetterstationsdaten

      !susn(1:2) = transfer_quantity_p(29+(i-1)*number_trans_quant) ! Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE
      !!#bsbt(1:2) = transfer_quantity_p(1+(i-1)*number_trans_quant) !orgc Sauerstoffverbrauch
	  !bsbct(1:2) = transfer_quantity_p(47+(i-1)*number_trans_quant)
      !dalgki(1:2) = transfer_quantity_p(20+(i-1)*number_trans_quant) ! Zuwachs Kiesel-Algen
      !dalggr(1:2) = transfer_quantity_p(21+(i-1)*number_trans_quant) ! Zuwachs Grün-Algen
      !dalgbl(1:2) = transfer_quantity_p(22+(i-1)*number_trans_quant) ! Zuwachs Blau-Algen
      !dalgak(1:2) = transfer_quantity_p(23+(i-1)*number_trans_quant) ! Respiration Kiesel-Algen
      !dalgag(1:2) = transfer_quantity_p(24+(i-1)*number_trans_quant) ! Respiration Grün-Algen
      !dalgab(1:2) = transfer_quantity_p(25+(i-1)*number_trans_quant) ! Respiration Blau-Algen
      !PO2P(1:2) = transfer_quantity_p(30+(i-1)*number_trans_quant) ! Sauerstoffproduktion durch Makrophyten
      !PO2R(1:2) = transfer_quantity_p(31+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Makrophyten

      !ssalg(1:2) = planktonic_variable_p(52+(i-1)*number_plankt_vari) ! 
      !stind(1:2) = planktonic_variable_p(59+(i-1)*number_plankt_vari) ! 
      !albewg(1:2)=benthic_distribution_p(13+(i-1)*number_benth_distr) ! Wachstum benthischer gruen-Algen
      !alberg(1:2)=benthic_distribution_p(11+(i-1)*number_benth_distr) ! Respiration benthischer gruen-Algen
      !albewk(1:2)=benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
      !alberk(1:2)=benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
	  	  
      !resdr(1:2)=benthic_distribution_p(15+(i-1)*number_benth_distr) ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
      !dzres1(1:2) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
      !dzres2(1:2) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten

      if((iphy<1).or.(iphy>4))then
         write(fehler,*)'ph_huelle: aeration flag iphy',iphy,' out of bounds i,meinrang=',i,meinrang
         call qerror(fehler)
      endif

      !! ---
      iglob=(i+meinrang*part) ! i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
      if (iglob.gt. knotenanzahl2D) return ! überstehende Nummern nicht bearbeiten.
      tflie=real(deltat)/86400
      tiefes=rb_hydraul_p(2+(i-1)*number_rb_hydraul)
      raus=strickler( zone(point_zone(iglob))%reib , tiefes )
      flaes=1000.0
	  ! Caki,Cagr,Cabl  ! set by   ini_algae() delivered by module_QSimDatenfelder.f95
      kontroll=iglob.eq.kontrollknoten
	  
      if(kontroll)print*,iglob,meinrang,i,part,"  vor ph_kern lf,ph=",  &
      planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)

!  subroutine ph_kern(mws,pws,cas,lfs,tempws,vphs,vco2s                 &
!                    ,tflie,raus,vmitts,tiefes,rhyds,flaes              &
!                    ,wges,WLages,hWSs,iphy                             &
!                    ,bsbcts,resdrs,dzres1s,dzres2s                     &
!				 	 ,dalgkis,dalggrs,dalgbls,dalgaks,dalgags,dalgabs   &
!					 ,Caki,Cagr,Cabl                                    &
!					 ,albergs,alberks,albewgs,albewks                   &
!					 ,susns,po2ps,po2rs,ssalgs,stinds                   &

            call ph_kern(planktonic_variable_p(62+(i-1)*number_plankt_vari) &
	 &                  ,planktonic_variable_p(63+(i-1)*number_plankt_vari) &
     &                  ,planktonic_variable_p(64+(i-1)*number_plankt_vari) &
	 &                  ,planktonic_variable_p(65+(i-1)*number_plankt_vari) &
     &                  ,planktonic_variable_p( 1+(i-1)*number_plankt_vari) &
	 &                  ,planktonic_variable_p(66+(i-1)*number_plankt_vari) &
     &                  ,transfer_quantity_p(26+(i-1)*number_trans_quant)   &
     &                  ,tflie                                   &
     &                  ,raus                                    &
     &                  ,rb_hydraul_p(1+(i-1)*number_rb_hydraul) &
     &                  ,rb_hydraul_p(2+(i-1)*number_rb_hydraul) &
     &                  ,rb_hydraul_p(2+(i-1)*number_rb_hydraul) &
     &                  ,flaes                                   &
     &                  ,wge_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer)  &
     &                  ,zone(point_zone(iglob))%wettstat%wetterstations_lage           &
     &                  ,rb_hydraul_p(3+(i-1)*number_rb_hydraul)                        &
     &                  ,iphy                                                           &
     &                  ,transfer_quantity_p(47+(i-1)*number_trans_quant)    &
     &                  ,benthic_distribution_p(15+(i-1)*number_benth_distr) &
     &                  ,transfer_quantity_p(27+(i-1)*number_trans_quant)    &
     &                  ,transfer_quantity_p(28+(i-1)*number_trans_quant)    &
     &                  ,transfer_quantity_p(20+(i-1)*number_trans_quant) &
     &                  ,transfer_quantity_p(21+(i-1)*number_trans_quant) &
     &                  ,transfer_quantity_p(22+(i-1)*number_trans_quant) &
     &                  ,transfer_quantity_p(23+(i-1)*number_trans_quant) &
     &                  ,transfer_quantity_p(24+(i-1)*number_trans_quant) &
     &                  ,transfer_quantity_p(25+(i-1)*number_trans_quant) &
     &                  ,Caki,Cagr,Cabl   &
     &                  ,benthic_distribution_p(11+(i-1)*number_benth_distr) &
     &                  ,benthic_distribution_p(12+(i-1)*number_benth_distr) &
     &                  ,benthic_distribution_p(13+(i-1)*number_benth_distr) &
     &                  ,benthic_distribution_p(14+(i-1)*number_benth_distr) &
     &                  ,transfer_quantity_p(29+(i-1)*number_trans_quant) &
     &                  ,transfer_quantity_p(30+(i-1)*number_trans_quant) &
     &                  ,transfer_quantity_p(31+(i-1)*number_trans_quant) &
     &                  ,planktonic_variable_p(52+(i-1)*number_plankt_vari) &
     &                  ,planktonic_variable_p(59+(i-1)*number_plankt_vari) &
     &                  ,kontroll ,iglob )  

      if(kontroll)print*,iglob,meinrang,i,part," nach ph_kern lf,ph=",  &
      planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)

      if((iphy<1).or.(iphy>4))then
         write(fehler,*)'ph_huelle nachher: aeration flag iphy',iphy,' out of bounds i,meinrang=',i,meinrang
         call qerror(fehler)
      endif
             
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Daten-rückgabe: 

      ! Transportkonzentrationen zurückschreiben
      !planktonic_variable_p(62+(i-1)*number_plankt_vari) = mw(1) !
      !planktonic_variable_p(63+(i-1)*number_plankt_vari) = pw(1) ! 
      !planktonic_variable_p(64+(i-1)*number_plankt_vari) = ca(1) ! 
      !planktonic_variable_p(65+(i-1)*number_plankt_vari) = lf(1) !  ### wie geht das mit dem Salzgehalt zusammen????
      !planktonic_variable_p(66+(i-1)*number_plankt_vari) = vph(1) ! 
      !planktonic_variable_p(59+(i-1)*number_plankt_vari) = stind(1) ! ??? Minutenzähler Bedeutung sehr unklar; Versuch einer Altersvariablen?
      ! Übergabekonzentrationen Rückgabewerte
      !transfer_quantity_p(26+(i-1)*number_trans_quant) = vco2(1) ! Kohlendioxyd

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

