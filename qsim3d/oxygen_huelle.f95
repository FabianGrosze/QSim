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

!> \page Sauerstoffgehalt Sauerstoffgehalt
!!
!! Der Sauerstoffgehalt steht im Zentrum der Gewässergüte-Untersuchung, da ein Mangel an Sauerstoff
!! für viele im Wasser lebende Tiere tödlich ist.\n\n
!! Der Baustein oxygen dient dazu, den Sauerstoffgehalt im Wasser zu bilanzieren.\n
!! 
!! <h2>Herkunft</h2>
!!     oxygen()\n
!!     EIN PROGRAMM ZUR BERECHNUNG DES SAUERSTOFFGEHALTS IN FLIEßGEWÄSSERN \n
!!     AUTOR : VOLKER KIRCHESCH          \n                                
!!     entnommen aus Version qsim13.301_28mae18\n 
!! 
!! <h2>Teilprozesse</h2>
!! 
!! <h4> zweistufiges Berechnungsverfahren</h4>
!! In jedem Zeitschritt wird in einer ersten Stufe ein Zwischenwert des Sauerstoffgehalts
!! infolge der untenstehend aufgeführten biochemischen Zehrungs- und Produktionsprozesse berechnet.
!! In einer zweiten Stufe wird dann ausgehend von dem Zwischenwert 
!! die Belüftung über die Gewässeroberfläche ermittelt.
!!
!! <h4> Lokale Zehrungs- und Produktionsprozesse </h4>
!!    <ol>
!!       <li>\subpage prodO2alg</li>
!!       <li>\subpage Respirationalgen </li>
!!       <li>Sauerstoff-Verbrauch durch Nitrifikation. siehe dazu Baustein: \ref Stickstoff</li>
!!       <li>Zehrung infolge der Umsetzung von org. C-Verbindungen. siehe dazu den Abschnitt: \ref o2zehr aus dem Baustein: \ref BSB</li>
!!       <li>\subpage SedFluxO2 </li>
!!       <li>Produktion und Respiration durch Makrophyten mphyt()</li>
!!       <li>Produktion und Respiration benthischer Blau- und Kiesel-Algen albenth()</li>
!!       <li>\subpage Zooplanktonrespiration , konsum()</li>
!!       <li>Respiration Dreissena-Muscheln dreissen() </li>
!!       <li>Respiration Heterotropher Nanoflagelaten hnf() </li>
!!    </ol>
!! 
!! <h4>Zwischenbilanz</h4>
!! Die Zwischenbilanz ergibt sich dann aus der \subpage ZehrProdO2
!!
!! <h4>Austausch über die Gewässeroberfläche / Belüftung</h4>
!! Gewässer können an der Gewässeroberfläche Sauerstoff an die Luft abgeben, oder aus ihr aufnehmen.
!! Wie dieser Prozess in QSim abgebildet wird, lesen Sie im Unterabschnitt: \subpage lueftO2
!! 
!! <h2>Schnittstellenbeschreibung</h2>
!!  SUBROUTINE oxygen()\n
!! ( \ref vo2, \ref tempw, \ref rau, \ref vmitt, \ref tiefe, \ref rhyd, \ref flae, \ref tflie, \ref go2n
!! , \ref dalgki, \ref dalggr, \ref dalgak, \ref dalgag, \ref akinh4   &\n
!! , \ref agrnh4, \ref akino3, \ref agrno3, \ref bsbt, \ref hjo2, \ref flag, \ref elen, \ref ior
!! , \ref anze, \ref dzres1, \ref dzres2, \ref hschlr              &\n
!! , *eo2*, \ref qeinl, \ref vabfl, \ref po2p, \ref po2r, \ref so2ein, \ref do2o2d, \ref salgo
!! , \ref dalgo, \ref dalgao, \ref o2ein1, \ref jiein             &\n
!! , \ref opgrmi, \ref opgrma, \ref opkimi, \ref opkima, \ref albewg, \ref alberg, \ref abeowg, \ref abeorg
!! , \ref opblmi, \ref opblma, \ref ablnh4        &\n
!! , \ref ablno3, \ref dalgbl, \ref dalgab, \ref albewk, \ref alberk, \ref abeowk, \ref abeork, \ref ro2dr
!! , \ref wge, *idwe*, \ref fkm, \ref uhrz, \ref vnh4       &\n
!! , \ref vno3, \ref bsbbet, \ref zooro2, \ref ro2hnf, \ref ilbuhn, \ref iwied, \ref vo2z, *suso2n*
!! , \ref nkzs, \ref dh2d, *o2l*, *qeinll*             &\n
!! , \ref iorla, \ref iorle, \ref ieinls, \ref agnh4z, \ref aknh4z, \ref abnh4z, \ref dalgkz, \ref dalgbz
!! , \ref dalggz, \ref agno3z, \ref akno3z          &\n
!! , \ref abno3z, \ref algakz, \ref algagz, \ref algabz, \ref vz1, \ref tempwz, \ref saett, \ref mstr
!! , \ref cpfad, \ref ij, \ref itags, \ref monats             &\n
!! , \ref dc_denw, \ref toc_csb, \ref wlage, \ref hws, *etemp*, \ref dh2de, \ref ifehl, \ref ifhstr
!! , \ref azstrs                          &\n 
!! , \ref zooind, \ref grote, \ref iphy, \ref kontroll, \ref iglob)\n                          
!!
!! Die QSim3D Subroutine oxygen_huelle() ruft die QSim-Subroutine oxygen() auf, 
!! \n(Zum Hüllroutinen-Konzept siehe: \ref hüllen )
!!
!! <h2>Dokumentation und Veröffentlichungen</h2>
!! <a href="./pdf/Becker_et_al_RRA_2009.pdf" target="_blank">
!! MODELLING THE EFFECTS OF THERMAL STRATIFICATION ON THE OXYGEN BUDGET OF AN IMPOUNDED RIVER </a>\n 
!! Becker et al. 2009 
!! \n\n
!! <a href="./pdf/Schoel_et_al_1999mosel-saar.pdf" target="_blank"> 
!! Model-based analysis of oxygen budget and biological processes in the
!! regulated rivers Moselle and Saar: modelling the influence of benthic
!! filter feeders on phytoplankton</a>\n 
!! Schöl et al. 1999 \n
!! \n\n
!! Desweiteren  existiert eine Dokumentation des Sauerstoff-Moduls als Kapitel 15 der
!! <a href="./pdf/QSimDoku_ncycWy.pdf" target="_blank">Kurzdoku</a> (Version vom 22. Nov. 2017)
!! \n\n
!! zurück zu: \ref lnk_ueberblick ; Quelle: oxygen_huelle.f95

!! Das nebenstehende Bild vom Fischsterben im Main, April 1971, ist der Webseite der 
!! <a href="http://www.frankfurter-fischerzunft.de/Galarie/1971_fischsterben.htm"  target="_blank">
!! Frankfurter Fischerzunft</a> entnommen.\n\n

!!<table border="0" ><tr><td  width="50%" >
!! \image html 1971_fischsterbenMainFischerzunft945.jpg ""  
!! \image latex 1971_fischsterbenMainFischerzunft945.jpg ""  
!! </td><td  width="50%" align="left" valign="top">
!!</td></tr></table>


!! \n
!! \n
!! <table >
!! <tr><th>     Variablen-Name QSim	 </th><th> Daten-Feld T-QSim	</th><th> Beschreibung </th></tr>
!! <tr><td> \ref vo2 	</td><td> planktische_variablen::planktonic_variable_p ( 2+nk) </td><td> Sauerstoffgehalt tiefengemittelt </td></tr>
!! <tr><td> \ref tempw  </td><td>  planktische_variablen::planktonic_variable_p ( 1+nk)  </td><td> Wassertemperatur </td></tr>
!! <tr><td>  rau(1) </td><td>  benthische_verteilungen::benthic_distribution_p (5+(i-1)*number_benth_distr) </td><td> Strickler Reibungsbeiwert </td></tr>
!! <tr><td>  vmitt(1)  </td><td>  randbedingungen rb_hydraul_p (1+(i-1)*number_rb_hydraul) </td><td> Geschwindigkeitsbetrag </td></tr>
!! <tr><td>  tiefe(1)  </td><td>  randbedingungen rb_hydraul_p (2+(i-1)*number_rb_hydraul) </td><td> Wassertiefe </td></tr>
!! <tr><td>  FLAE(1) </td><td> tiefe(1)*500.0 </td><td>  Breite konstant 500 m ; wird in der Belüftungsformel verwendet,  </td></tr>
!! <tr><td>  tflie  </td><td>  real(deltat)/86400 </td><td> Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim) </td></tr>
!! <tr><td>  go2n(1)  </td><td>  uebergabe_werte::transfer_quantity_p (32+(i-1)*number_trans_quant) </td><td> FUER DIE STICKSTOFFOXYDATION VERBRAUCHTE SAUERSTOFFMENGE </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> dalgki(1)  </td><td>  uebergabe_werte::transfer_quantity_p (20+(i-1)*number_trans_quant) </td><td> Zuwachs Kiesel-Algen </td></tr>
!! <tr><td>  dalggr(1)  </td><td>  uebergabe_werte::transfer_quantity_p (21+(i-1)*number_trans_quant) </td><td> Zuwachs Grün-Algen </td></tr>
!! <tr><td>  dalgak(1)  </td><td>  uebergabe_werte::transfer_quantity_p (23+(i-1)*number_trans_quant) </td><td> Respiration Kiesel-Algen </td></tr>
!! <tr><td>  dalgag(1)  </td><td>  uebergabe_werte::transfer_quantity_p (24+(i-1)*number_trans_quant) </td><td> Respiration Grün-Algen </td></tr>
!! <tr><td>  akinh4(1)  </td><td>  uebergabe_werte::transfer_quantity_p (33+(i-1)*number_trans_quant) </td><td> Ammoniumaufnahme der kiesel-Algen </td></tr>
!! <tr><td>  agrnh4(1)  </td><td>  uebergabe_werte::transfer_quantity_p (34+(i-1)*number_trans_quant) </td><td> Ammoniumaufnahme der gruen-Algen </td></tr>
!! <tr><td>  akino3(1)  </td><td>  uebergabe_werte::transfer_quantity_p (36+(i-1)*number_trans_quant) </td><td> Nitrataufnahme der kiesel-Algen </td></tr>
!! <tr><td>  agrno3(1)  </td><td>  uebergabe_werte::transfer_quantity_p (37+(i-1)*number_trans_quant) </td><td> Nitrataufnahme der gruen-Algen </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> bsbt(1)  </td><td>  uebergabe_werte::transfer_quantity_p (1+(i-1)*number_trans_quant) </td><td> Sauerstoffverbrauch durch Kohlenstoffabbau </td></tr>
!! <tr><td>  hJO2(1,1)  </td><td>  benthische_verteilungen::benthic_distribution_p (8+(i-1)*number_benth_distr) </td><td> Sauerstoffzehrung des Sediments gO2/m² und Zeitschritt </td></tr>
!! <tr><td>  flag(1) </td><td> 0         </td><td> keine Einleitungen </td></tr>
!! <tr><td>  elen(1) </td><td> 1         </td><td> Elementlänge (nicht verwendet) </td></tr>
!! <tr><td>  ior </td><td> 1             </td><td> Laufindex im Strang (T-QSim verwendet nur erstes Profil(Punkt) im Strang) </td></tr>
!! <tr><td>  anze </td><td> 1            </td><td> Anzahl der Profile im aktuellen Strang </td></tr>
!! <tr><td>  dzres1(1)  </td><td>  uebergabe_werte::transfer_quantity_p (27+(i-1)*number_trans_quant) </td><td> Grund-Respiration Konsumenten </td></tr>
!! <tr><td>  dzres2(1)  </td><td>  uebergabe_werte::transfer_quantity_p (28+(i-1)*number_trans_quant) </td><td> Fraßabhängige Respirationsrate Konsumenten </td></tr>
!! <tr><td>  hSchlr(1,1)  </td><td>  benthische_verteilungen::benthic_distribution_p (16+(i-1)*number_benth_distr) </td><td> Sauerstoffzehrung durch das Sediments, Ausgabe in mgO2/(l*h)  </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> eo2(1) </td><td> 0.0         </td><td> Einleite-Konzentration (keine Einleitungen - nicht verwendet) </td></tr>
!! <tr><td>  qeinl(1) </td><td> 0.0       </td><td> kein Abfluss Einleitung </td></tr>
!! <tr><td>  vabfl(1)  </td><td>  2.5 !! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet. </td></tr>
!! <tr><td>  PO2P(1)  </td><td>  uebergabe_werte::transfer_quantity_p (30+(i-1)*number_trans_quant) </td><td> Sauerstoffproduktion durch Makrophyten in mgO2/l je Zeitschritt </td></tr>
!! <tr><td>  PO2R(1)  </td><td>  uebergabe_werte::transfer_quantity_p (31+(i-1)*number_trans_quant) </td><td> Sauerstoffverbrauch durch Makrophyten in mgO2/l je Zeitschritt </td></tr>
!! <tr><td>  so2ein(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (17+(i-1)*number_benth_distr) </td><td> Sauerstoffeintrag aus der Luft ? , Ausgabe in mgO2/(l*h)  </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr> </td></tr>
!! <tr><td>  bbei2D(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (18+(i-1)*number_benth_distr) </td><td> Beiwert Oberflächenbelüftung ?  </td></tr>
!! <tr><td>  salgo(1)  </td><td>  uebergabe_werte::transfer_quantity_p (39+(i-1)*number_trans_quant) </td><td> Sauerstoffverbrauch durch Makrophyten in mgO2/l je Zeitschritt </td></tr>
!! <tr><td>  dalgo(1)  </td><td>  uebergabe_werte::transfer_quantity_p (40+(i-1)*number_trans_quant) </td><td> Sauerstoffproduktion der Gruen-, Kiesel-, und Blaualgen </td></tr>
!! <tr><td>  dalgao(1)  </td><td>  uebergabe_werte::transfer_quantity_p (41+(i-1)*number_trans_quant)! Respiration (Sauerstoffverbrauch) der Gruen-, Kiesel-, und Blaualgen </td></tr>
!! <tr><td>  o2ein1(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (19+(i-1)*number_benth_distr) </td><td> Sauerstoffeintrag aus der Luft ? , Ausgabe in mgO2/(l*h)  </td></tr>
!! <tr><td>   jiein(1) </td><td> 0        </td><td> keine Punkt-Einleitungen </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> opgrmi  </td><td> ! \ref globaleParameter direkt aus QSimDatenfelder </td><td> Min. O2-Prod. Grünalgen </td></tr>
!! <tr><td>  opgrma  </td><td> ! \ref globaleParameter direkt aus QSimDatenfelder  </td><td> Max. O2-Prod. Grünalgen </td></tr>
!! <tr><td>  opkimi  </td><td> ! \ref globaleParameter direkt aus QSimDatenfelder  </td><td> Min. O2-Prod. Kieselalgen </td></tr>
!! <tr><td>  opkima  </td><td> ! \ref globaleParameter direkt aus QSimDatenfelder  </td><td> Max. O2-Prod. Kieselalgen </td></tr>
!! <tr><td>  albewg(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (13+(i-1)*number_benth_distr) </td><td> Wachstum benthischer gruen-Algen </td></tr>
!! <tr><td>  alberg(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (11+(i-1)*number_benth_distr) </td><td> Respiration benthischer gruen-Algen </td></tr>
!! <tr><td>  abeowg(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (20+(i-1)*number_benth_distr) </td><td> Sauerstoffproduktion benthischer Grünalge </td></tr>
!! <tr><td>  abeorg(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (21+(i-1)*number_benth_distr) </td><td> Sauerstoffverbrauch benthischer Grünalgen </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> opblmi  </td><td> ! \ref globaleParameter direkt aus QSimDatenfelder  </td><td> Min. O2-Prod. Blaualgen </td></tr>
!! <tr><td>  opblma  </td><td> ! \ref globaleParameter direkt aus QSimDatenfelder  </td><td> Max. O2-Prod. Blaualgen </td></tr>
!! <tr><td>  ablnh4(1)  </td><td>  uebergabe_werte::transfer_quantity_p (35+(i-1)*number_trans_quant) </td><td> Ammoniumaufnahme der blau-Algen, tiefengem. </td></tr>
!! <tr><td>  ablno3(1)  </td><td>  uebergabe_werte::transfer_quantity_p (38+(i-1)*number_trans_quant) </td><td> Nitrataufnahme der blau-Algen, tiefengem. </td></tr>
!! <tr><td>  dalgbl(1)  </td><td>  uebergabe_werte::transfer_quantity_p (22+(i-1)*number_trans_quant) </td><td> Zuwachs Blau-Algen </td></tr>
!! <tr><td>  dalgab(1)  </td><td>  uebergabe_werte::transfer_quantity_p (25+(i-1)*number_trans_quant) </td><td> Respiration Blau-Algen </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> albewk(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (14+(i-1)*number_benth_distr) </td><td> Wachstum benthischer kiesel-Algen </td></tr>
!! <tr><td>  alberk(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (12+(i-1)*number_benth_distr) </td><td> Respiration benthischer kiesel-Algen </td></tr>
!! <tr><td>  abeowk(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (22+(i-1)*number_benth_distr) </td><td> Sauerstoffproduktion benthischer Kieselalgen </td></tr>
!! <tr><td>  abeork(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (23+(i-1)*number_benth_distr) </td><td> Sauerstoffverbrauch benthischer Kieselalgen </td></tr>
!! <tr><td>  ro2dr(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (24+(i-1)*number_benth_distr) </td><td> Respiration Dreissena-Muscheln pro Zeitschritt in mgO2/l je Zeitschritt </td></tr>
!! <tr><td>  wge(1) </td><td> wetter::wge_T (i2)   </td><td> Windgeschwindigkeit  aus Wetterstationsdaten </td></tr>
!! <tr><td>  idwe(1,1) </td><td>  i2  </td><td> Wetterstation Nr. </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> fkm (1) </td><td> 0.0  </td><td> Flusskilometer (unbenutzt) </td></tr>
!! <tr><td>  uhrz </td><td> uhrzeit_stunde </td><td> Uhrzeit module ::modell zeitsekunde()  </td></tr>
!! <tr><td>  si(1)  </td><td>  planktische_variablen::planktonic_variable_p (7+nk) </td><td> geloestes Silikat-Silizium, tiefengemittelt (unbenutzt)  </td></tr>
!! <tr><td>  gelp(1)  </td><td>  planktische_variablen::planktonic_variable_p (6+nk) </td><td> geloester ortho-Phosphat-P, tiefengemittelt (unbenutzt) </td></tr>
!! <tr><td>  ssalg(1)  </td><td>  planktische_variablen::planktonic_variable_p (52+(i-1)*number_plankt_vari) </td><td> ??? (unbenutzt) </td></tr>
!! <tr><td>  ir(1)  </td><td>  uebergabe_werte::transfer_quantity_p (42+(i-1)*number_trans_quant) </td><td> Ingestionsrate der Rotatorien in mg/(l*h) | konsum() (unbenutzt) </td></tr>
!! <tr><td>  vNH4(1)  </td><td>  planktische_variablen::planktonic_variable_p ( 3+nk)  </td><td> ammonium (unbenutzt) </td></tr>
!! <tr><td>  vno3(1)  </td><td>  planktische_variablen::planktonic_variable_p ( 5+nk)  </td><td> nitrat (unbenutzt) </td></tr>
!! <tr><td>  bsbbet(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (7+(i-1)*number_benth_distr) </td><td> Sauerstoffverbrauch durch Organismen auf Makrophyten, Ausgabewert bsbtb ??? (unbenutzt) </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> zooro2(1)  </td><td>  uebergabe_werte::transfer_quantity_p (43+(i-1)*number_trans_quant) </td><td> Sauerstoffverbrauch durch Zooplanktonrespiration (Rückgabewert) </td></tr>
!! <tr><td>  rO2HNF(1)  </td><td>  uebergabe_werte::transfer_quantity_p (44+(i-1)*number_trans_quant) </td><td> Respiration HNF ??? </td></tr>
!! <tr><td>  ilbuhn </td><td> 0    </td><td> keine Buhnen </td></tr>
!! <tr><td>  iwied </td><td> 0 </td><td> unbenutzte Variable </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td>    vo2z(j,1) </td><td> planktische_variablen::plankt_vari_vert_p (j+(2-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) </td></tr>
!! <tr><td>  susO2N(1) </td><td>  0.0     </td><td> unbenutzte Variable </td></tr>
!! <tr><td>  nkzs(1) </td><td> 1 </td><td> nur eine Tiefenschicht </td></tr>
!! <tr><td>  dH2D  </td><td>  0.25 </td><td> Dicke Tiefenschicht ??? Übergabe an Einleiter_Misch() | qsim.f90: dH2D = 0.25 </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td>    agnh4z(j,1)  </td><td>  trans_quant_vert_p(j+(10-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Ammoniumaufnahme Grün-Algen </td></tr>
!! <tr><td>     aknh4z(j,1)  </td><td>  trans_quant_vert_p(j+(9-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Ammoniumaufnahme Kiesel-Algen </td></tr>
!! <tr><td>     abnh4z(j,1)  </td><td>  trans_quant_vert_p(j+(11-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Ammoniumaufnahme Blau-Algen </td></tr>
!! <tr><td>     dalgkz(j,1)  </td><td>  trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td>  </td></tr>
!! <tr><td>     dalgbz(j,1)  </td><td>  trans_quant_vert_p(j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td>  </td></tr>
!! <tr><td>     dalggz(j,1)  </td><td>  trans_quant_vert_p(j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td>  </td></tr>
!! <tr><td>     agno3z(j,1)  </td><td>  trans_quant_vert_p(j+(16-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Nitrataufnahme Grün-Algen </td></tr>
!! <tr><td>     akno3z(j,1)  </td><td>  trans_quant_vert_p(j+(15-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Nitrataufnahme Kiesel-Algen </td></tr>
!! <tr><td>     abno3z(j,1)  </td><td>  trans_quant_vert_p(j+(17-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Nitrataufnahme Blau-Algen </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td>    algakz(j,1)  </td><td>  trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td>  </td></tr>
!! <tr><td>     algagz(j,1)  </td><td>  trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td>  </td></tr>
!! <tr><td>     algabz(j,1)  </td><td>  trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td>  </td></tr>
!! <tr><td>     vz1(j,1)  </td><td>  trans_quant_vert_p(j+(21-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> lokale Sauerstoffzehrung/produktion tiefenaufgelöst </td></tr>
!! <tr><td>     tempwz(j,1) </td><td>  planktische_variablen::plankt_vari_vert_p (j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) </td><td> wassertemperatur tiefenaufgelöst </td></tr>
!! <tr><td>  saett(1)  </td><td>  uebergabe_werte::transfer_quantity_p (45+(i-1)*number_trans_quant) </td><td> Sauerstoff Sättigungs-Konzentration in mgO2/l  </td></tr>
!! <tr><td>  mstr </td><td> 1 </td><td> Strangzähler ein Strang mit nur einem Profil in Hüllroutine </td></tr>
!! <tr><td>  ij </td><td> 0 </td><td> unbenutzte Variable </td></tr>
!! <tr><td>  itags </td><td> tag </td><td> Tag im Monat module ::modell zeitsekunde() </td></tr>
!! <tr><td>  monats </td><td> monat </td><td> Monat im Jahr module ::modell zeitsekunde() (unbenutzt) </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> etemp(1) </td><td> 0.0  </td><td> Einleite-Temperatur (da keine Einleitungen - nicht verwendet) </td></tr>
!! <tr><td>  dH2De  </td><td> 0.0   </td><td> unbenutzt </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> iphyw  </td><td>  0 </td><td> Ansteuerung der Belüftungsformel: ??? </td></tr>
!! <tr><td>  iphy  </td><td>  0  </td><td> Ansteuerung der Belüftungsformel: </td></tr>
!! <tr><td>  zwgmes  </td><td>  0.0 </td><td> Ansteuerung der Belüftungsformel:  </td></tr>
!! <tr><td>  kontroll </td><td> (i.eq.kontrollknoten) </td><td>  </td></tr>
!! </table>
!! \n
!! \n\n
!! zurück zu: \ref lnk_ueberblick ; Quelle: oxygen_huelle.f95
!                                                                       !                                                                       
!> \page ZehrProdO2 Summe der Änderung des Sauerstoffgehalts infolge lokaler Produktion und Zehrung
!! hier wird nun die Aufsummation der Änderung der Sauerstoffkonzentration infolge von lokalen Zehrungs- und
!! Produktionsprozessen zum Zwischenwert \f$ \widetilde{\Delta O_2} \f$ formelmäßig dargestellt:\n
!! \f{eqnarray*}{
!!    \widetilde{\Delta O_2} &=& \Delta {O_2}_{alg,prod} - \Delta {O_2}_{alg,resp} \\
!!    &-& \Delta {O_2}_{N} - \Delta {O_2}_{orgC} - \Delta {O_2}_{Sed} \\
!!    &+& \Delta {O_2}_{mph,prod} - \Delta {O_2}_{mph,resp} \\
!!    &+& \Delta {O_2}_{bent,gr,prod} - \Delta {O_2}_{bent,gr,resp} + \Delta {O_2}_{bent,ki,prod} - \Delta {O_2}_{bent,ki,resp} \\
!!    &-& \Delta {O_2}_{rot} - \Delta {O_2}_{drs} - \Delta {O_2}_{HNF} \\
!! \f}
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th><th> Herkunft </th></tr>
!!<tr><td> \f$ \widetilde{\Delta O_2} \f$  </td><td> v </td><td width="50%" > Änderung der Sauerstoffkonzentration infolge Zehrung + Produktion </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> - </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{alg,prod} \f$  </td><td> dalgo </td><td> Sauerstoff-Produktion der Algen je Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td>  </td><td> \ref prodO2alg </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{alg,resp} \f$  </td><td> dalgao </td><td> Sauerstoff-Verbrauch (Respiration) der Algen je Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td>  </td><td> \ref Respirationalgen </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{N} \f$  </td><td> go2n </td><td> Verbrauch durch Nitrifikation pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> ncyc() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{orgC} \f$  </td><td> bsbt </td><td> Zehrung infolge Umsetzung von org. C-Verbindungen pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> \ref o2zehr  orgc </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{Sed} \f$  </td><td> hSchlr </td><td> Sauerstoffzehrung des Sediments je Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> sedflux() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{mph,prod} \f$  </td><td> po2p </td><td> Produktion durch Makrophyten pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> mphyt() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{mph,resp} \f$  </td><td> po2r </td><td> Respiration durch Makrophyten pro Zeitschritt </td><td> mgO2/l je Zeitschritt  </td><td> ? </td><td> mphyt() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{bent,gr,prod} \f$  </td><td> abeowg </td><td> Produktion durch benthische Grünalgen pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> albenth() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{bent,gr,resp} \f$  </td><td> abeorg </td><td> Respiration durch benthische Grünalgen pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> albenth() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{bent,ki,prod} \f$  </td><td> abeowk </td><td> Produktion durch benthische Kieselalgen pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> albenth() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{bent,ki,resp} \f$  </td><td> abeork </td><td> Respiration durch benthische Kieselalgen pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> albenth() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{rot} \f$  </td><td> vo2leb </td><td> Sauerstoffverbrauch Zooplankton-Respiration pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> konsum() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{drs} \f$  </td><td> ro2dr </td><td> Respiration Dreissena-Muscheln pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> dreissen() </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{HNF} \f$  </td><td> rO2HNF </td><td> Respiration Heterotropher Nanoflagelaten pro Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> hnf() </td></tr>
!!</table>\n
!! 2D \n
!!      if(nkzs(ior).eq.1)goto 190 \n
!!      do 189 nkz = 1,nkzs(ior) \n
!!      vz1(nkz,ior) = -go2n(ior)-bsbt(ior)+dalgoz(nkz,ior)               &\n
!!     &-algaoz(nkz,ior)-vo2leb+po2p(ior)-po2r(ior)+abeowg(ior)           &\n
!!     &-abeorg(ior)+abeowk(ior)-abeork(ior)-ro2dr(ior)                   &\n
!!     &-rO2HNF(ior)                                                      \n
!! \n\n
!! zurück zu: \ref Sauerstoffgehalt ; Quelle: oxygen_huelle.f95
!
!> \page diskretO2 zeitliche Diskretisierung der Sauerstoffänderung
!! Weil die Belüftungsrate vom Sauerstoffgehalt selbst abhängig ist, wird hier eine semi implizite Diskretisierung
!! zur Berechnung des Sauerstoffgehaltes am Ende des aktuellen Zeitschritts  \f$ {O_2}(t + \Delta t)\f$ verwendet:
!! \f[ 
!!   {O_2}(t + \Delta t) - {O_2}(t) = \frac{\widetilde{\Delta O_2}+(b \cdot \Delta_{saett})}{1 + 0.5 \cdot b}
!! \f]\n mit
!! \f[ 
!!    b= \Delta t \cdot \frac{ ( k_l + k_w ) \cdot f_{temp} }{H} 
!! \f] siehe \ref lueftO2\n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th><th> Herkunft </th></tr>
!!<tr><td> \f$ \widetilde{\Delta O_2} \f$  </td><td> v </td><td> \ref ZehrProdO2 </td><td> mgO2/l je Zeitschritt </td><td> ? </td><td> - </td></tr>
!!<tr><td> \f$ \Delta_{saett} \f$  </td><td> Defiz </td><td> Sauerstoffdefizit (Untersättigung) siehe \ref lueftO2 </td><td>  mgO2/l </td><td> ? </td><td>  </td></tr>
!!</table>
!! \n\n
!! zurück zu: \ref Sauerstoffgehalt ; Quelle: oxygen_huelle.f95
!
!> \page prodO2alg Sauerstoffproduktion der Kiesel-, Gruen- und Blaualgen
!!     planktische Gruenalgen  \n                                                       
!!      if(agrnh4(ior)==0.0)agrnh4(ior) = 0.00001 \n
!!      falgo = agrno3(ior)/agrnh4(ior) \n
!!      falgo = (opgrmi+falgo*opgrma)/(1.+falgo) \n
!!      dalgo(ior) = dalggr(ior)*falgo \n
!!     Benthische Gruenalgen   \n                                          
!!      abeowg(ior) = albewg(ior)*falgo 
!!     planktische kieselalgen!     \n                                                 
!!      if(akinh4(ior)==0.0)akinh4(ior) = 0.00001 \n
!!      falgo = akino3(ior)/akinh4(ior) \n
!!      falgo = (opkimi+falgo*opkima)/(1.+falgo) \n
!!      dalgo(ior) = dalgo(ior)+dalgki(ior)*falgo \n
!!     Benthische Kieselalgen                    \n                        
!!      abeowk(ior) = albewk(ior)*falgo \n
!!     planktische Blaualgen!            \n                                            
!!      if(ablnh4(ior)==0.0)ablnh4(ior) = 0.00001 \n
!!      falgo = ablno3(ior)/ablnh4(ior) \n
!!      falgo = (opblmi+falgo*opblma)/(1.+falgo) \n
!!      dalgo(ior) = dalgo(ior)+dalgbl(ior)*falgo \n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th width="60%" > Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta {O_2}_{alg,prod} \f$  </td><td> dalgo </td><td> Sauerstoff-Produktion der Algen je Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta M_{alg,ki} \f$  </td><td> dalgki </td><td> Massenzuwachs der Kieselalgen je Zeitschritt </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta M_{alg,gr} \f$  </td><td> dalggr </td><td> Massenzuwachs der Grünalgen je Zeitschritt </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta M_{alg,bl} \f$  </td><td> dalgbl </td><td> Massenzuwachs der Blaualgen je Zeitschritt </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!</table>\n\n
!! zurück zu: \ref Sauerstoffgehalt ; Quelle: oxygen_huelle.f95
!                                                                       
!> \page Respirationalgen Respiration der Gruen-, Kiesel-, und Blaualgen
!!      dalgao(ior) = dalgag(ior)*opgrmi+dalgak(ior)*opkimi+dalgab(ior)*opblmi    \n                                           
!!      if(nkzs(ior)>1)then     ! 2D-Modellierung \n
!!        do nkz = 1,nkzs(ior) \n
!!          algaoz(nkz,ior) = algagz(nkz,ior)*opgrmi+algakz(nkz,ior)*opkimi+algabz(nkz,ior)*opblmi\n
!!        enddo\n
!!      endif                                    \n
!!!     Respiration der benthischen Gruenalgen und Kieselalgen   \n         
!!      abeorg(ior) = alberg(ior)*opgrmi \n
!!      abeork(ior) = alberk(ior)*opkimi \n
!!      if(nkzs(ior)>1)then \n
!!        do nkz = 1,nkzs(ior) ! O2-Produktion der Algen bei 2D-Modellierung \n
!!          if(agnh4z(nkz,ior)==0.0)agnh4z(nkz,ior) = 0.00001   ! Gruenalgen \n
!!          falgo = agno3z(nkz,ior)/agnh4z(nkz,ior) \n
!!          falgo = (opgrmi+falgo*opgrma)/(1.+falgo) \n
!!          dalgoz(nkz,ior) = dalggz(nkz,ior)*falgo \n
!!          if(aknh4z(nkz,ior)==0.0)aknh4z(nkz,ior) = 0.00001    ! Kieselalgen \n
!!          falgo = akno3z(nkz,ior)/aknh4z(nkz,ior) \n
!!          falgo = (opkimi+falgo*opkima)/(1.+falgo) \n
!!          dalgoz(nkz,ior) = dalgoz(nkz,ior)+dalgkz(nkz,ior)*falgo \n
!!          if(abnh4z(nkz,ior)==0.0)abnh4z(nkz,ior) = 0.00001   !Blaualgen \n
!!          falgo = abno3z(nkz,ior)/abnh4z(nkz,ior) \n
!!          falgo = (opblmi+falgo*opblma)/(1.+falgo) \n
!!          dalgoz(nkz,ior) = dalgoz(nkz,ior)+dalgbz(nkz,ior)*falgo \n
!!        enddo                                                     \n                  
!!      endif                                                       \n           
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta {O_2}_{alg,resp} \f$  </td><td> dalgao </td><td> Sauerstoff-Verbrauch der Algen je Zeitschritt </td><td> mgO2/l je Zeitschritt </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  pro Zeitschritt </td><td> mgO2/l </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  </td><td>  </td><td>  </td><td>  </td></tr>
!!</table>\n\n
!! zurück zu: \ref Sauerstoffgehalt ; Quelle: oxygen_huelle.f95
!                                                                       
!> \page Zooplanktonrespiration Sauerstoffverbrauch durch Zooplanktonrespiration                  
!!      ft = 1.047**(tempw(ior)-20.) \n
!!      vo2leb = (dzres1(ior)+dzres2(ior))*1.5 \n
!!      vo2leb = vo2leb*ft \n
!!      zooro2(ior) = vo2leb \n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta O_2 \f$  </td><td> v </td><td> lokale Änderung des Sauerstoffgehalts pro Zeitschritt </td><td> mgO2/l </td><td>  </td></tr>
!!<tr><td> \f$ \Delta {O_2}_{orgC} \f$  </td><td> bsbt </td><td> Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt </td><td> mgO2/l zeitschrittbezogen </td><td> ? </td></tr>
!!<tr><td> \f$  \f$  </td><td> tempw </td><td>  pro Zeitschritt </td><td> mgO2/l </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  </td><td>  </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  pro Zeitschritt </td><td> mgO2/l </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  </td><td>  </td><td>  </td><td>  </td></tr>
!!</table>\n\n
!! zurück zu: \ref Sauerstoffgehalt ; Quelle: oxygen_huelle.f95
!                                                                       
!> \page SedFluxO2 Sauerstoffaustausch mit dem Sediment
!! \f[ 
!!    \Delta {O_2}_{Sed} = Sedflux_{O_2} \cdot \frac{\Delta t}{h}
!! \f]
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta {O_2}_{Sed} \f$  </td><td> hSchlr </td><td> Sauerstoffzehrung des Sediments je Zeitschritt </td><td> mgO2/l zeitschrittbezogen </td><td> ? </td></tr>
!!<tr><td> \f$ Sedflux_{O_2} \f$  </td><td> hJO2 </td><td> Sauerstofffluss ins Sediments aus sedflux() </td><td> mgO2/(m² d) </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> tflie </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!<tr><td> \f$ h \f$  </td><td> tiefe </td><td> Wassertiefe </td><td> m </td><td> 0 ... ca. 25 </td></tr>
!!</table>\n\n
!! zurück zu: \ref Sauerstoffgehalt ; Quelle: oxygen_huelle.f95
!
!> \page lueftO2 Belüftung über die Gewässeroberfläche
!! Zur Erläuterung wird hier der Massenstrom an Sauerstoff über die Gewässeroberfläche 
!! zunächst als Änderungsrate einer tiefengemittelten Konzentration angeschrieben.
!! \f[ 
!!    {\frac{\delta O_2}{\delta t}}_{lueft} = (k_l + k_w) \cdot \Delta_{saett}  \cdot f_{temp} \cdot \frac{1}{H}
!! \f]
!! darin ist das Sauerstoffdefizit (Untersättigung):
!! \f[ 
!!    \Delta_{saett}={O_2}_{saett}-O_2
!! \f]
!! Die Sättigungskonzentration von Sauerstoff im Wasser ist temperaturabhängig 
!! und lässt sich wie folgt berechnen: \n
!! (Wassertemperatur T in Grad Celsius)
!! \f[ 
!!    {O_2}_{saett}= 14.603 - 0.40215 \cdot T + 0.007687 \cdot T^2 - 0.0000693 \cdot T^3
!! \f]
!! Für die Temperaturabhängigkeit des Belüftungsprozesses wird der folgende empirische Faktor angesetzt:\n
!! \f[ 
!!    f_{temp} = 1.024^{T-20}
!! \f]
!! \n
!! <h3>Einmischung mittels Turbulenz infolge Sohlreibung</h3>
!! Zur Berechnung der Belüftung des Gewässers über die Oberfläche wird standardgemäß die Formel nach Wolf(1972) verwendet: 
!! Der vom Fließzustand abhängige Beiwert \f$ k_l \f$ hat die Dimension einer Geschwindigkeit;\n
!! für ihn gibt Wolf(1972) die folgende dimensionsbehaftete empirische Formel an:
!! \f[ 
!!    k_l = (3+\frac{40}{K_{St}})\frac{V}{H} + 0.5 
!! \f]
!!   78 bbeiw = ((3.+40./rau(ior))*abs(vmitt(ior))/tiefe(ior)**2)+0.5/tiefe(ior)   \n                                                    
!! mit \f$ k_l \f$ in m/s, dem Stricklerbeiwert \f$ K_{St} \f$ in m^(1/3)/s, 
!! dem Geschwindigkeitsbetrag V in m/s und der Wassertiefe H in m.
!!\n
!! <h3>Alternative</h3>
!! Als Alternative kann die Belüftungsformel von MELCHING und FLORES (1999) angewählt werden (iphy = 2)                
!!    81 FN = 1./RAU(ior) \n
!!       G = 9.81 \n
!!       UST = ((FN*G**0.5)/tiefe(ior)**0.166667)*abs(VMITT(ior)) \n
!!       Slope = UST**2/(g*Tiefe(ior)) \n
!!       Breite = flae(ior)/tiefe(ior) \n
!!       bbeiw = (142.*(abs(vmitt(ior))*Slope)**0.333)/((tiefe(ior)**0.66)*(Breite**0.243))             \n                
!! \f[ 
!!    k_l = 
!! \f]
!! \n
!!........BELUEFTUNGSBEIWERT NACH HAJEK,NEUMANN,BISCHOFSBERGER IN     \n   
!!   73 BBEIw = (3+40./RAU(ior))*(abs(VMITT(ior))**0.7+0.5*tiefe(ior)**0.7)/tiefe(ior)**1.7   \n                              
!! \n
!! <h3>Windeinfluss</h3>
!! Der Windeinfluss auf die Belüftung wird berücksichtigt, wenn er mittels (iphyw != 0) angewählt wurde:
!! \n\n
!! Die gemessene Windgeschwindigkeit in der Höhe der Messung wird in eine 
!! Windgeschwindigkeit 10 m über der Wasseroberfläche umgerechnet:
!! \f[ 
!!    W_{10} = W_{mess} /  {\left( \frac{z_{mess}}{10.} \right) }^{0.17} 
!! \f]
!! \image html windgeschw.svg "Windgeschwindigkeit"  
!! \f[ 
!!    k_w = 0.19 \cdot W_{10} - 0.015 \cdot {W_{10}}^2 + 0.002 \cdot {W_{10}}^3
!! \f]\n
!! \image html windwirk_flaute.svg. "Belüftungswirkung schwache Winde"  
!! \image html windwirk.svg "Belüftungswirkung starke Winde"  \n
!! Im Falle der Nichtberücksichtigung des Windes wird \f$ k_w=0 \f$ gesetzt
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ H \f$  </td><td> tiefe </td><td> Wassertiefe </td><td> m </td><td> 0 ... ca. 25 </td></tr>
!!<tr><td> \f$ T \f$  </td><td> TEMPW </td><td> Wasser-Temperatur </td><td> Grad Celsius </td><td> 0 ... ca. 25 </td></tr>
!!<tr><td> \f$ O_2 \f$  </td><td> vo2 </td><td> Sauerstoffgehalt </td><td> mg O2 /l </td><td> 0 ... 10 ... 20 </td></tr>
!!<tr><td> \f$ k_l \f$  </td><td> (bbeiw) </td><td> Belüftungsgeschwindigkeit Wasserbewegung</td><td> m/d </td><td> ? </td></tr>
!!<tr><td> \f$ k_w \f$  </td><td> (bbeil) </td><td> Belüftungsgeschwindigkeit Luftbewegung</td><td> m/d </td><td> ? </td></tr>
!!</table>\n\n
!! Die \subpage diskretO2 ergibt dann den Wert der Sauerstoffkonzentration am Ende eines jeweiligen Zeitschritt
!! \n\n
!! zurück zu: \ref Sauerstoffgehalt ; Quelle: oxygen_huelle.f95

!> SUBROUTINE oxygen_huelle() ruft QSim-Subroutine oxygen() auf.\n                               
!! (Zum Hüllroutinen-Konzept siehe: !\ref hüllen )
!! \n\n 
!! Beschreibung: \ref Sauerstoffgehalt ; Quelle: oxygen_huelle.f95
!! \n\n
!! oxygen_huelle(i) WIRD parallel VON ALLEN PROZOREN AUFGERUFEN !!! ; i ist der prozessor-lokale Knotenzähler !!
      SUBROUTINE oxygen_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none
      integer :: i,j,nk,i2,k

!     i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
      iglob=(i+meinrang*part) ! globale Knotennummer 
      if (iglob.gt. knotenanzahl2D) return ! überstehende Nummern nicht bearbeiten.
      i2=zone(point_zone(iglob))%wettstat%wetterstations_nummer !! ist parallel !!!
      nk=(i-1)*number_plankt_vari ! Ort im Feld der transporterten, planktischen Variablen 
      kontroll=(iglob.eq.kontrollknoten)

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
      rau(1:2)= strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
      if(kontroll)print*,'oxygen_huelle: kst, ust=', rau(1)  &
     &                  , abs(VMITT(1)) *  (9.81**0.5) / ( (tiefe(1)**0.166667)*rau(1) )
      !FN = 1./RAU(ior) 
      !G = 9.81 
      !UST = ((FN*G**0.5)/tiefe(ior)**0.166667)*abs(VMITT(ior)) 

      FLAE(1)=tiefe(1)*500.0 !! Breite konstant 500 m ; wird in der Belüftungsformel verwendet, 
      ! hat aber keine Entsprechung im Mehrdimensionalen, daher sinnvoller Wert fürs Ästuar
      FLAE(2)=FLAE(1)
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
      flag(1)=0         ! keine Einleitungen
      flag(2)=flag(1)
      elen(1)=1         ! Elementlänge (nicht verwendet)
      elen(2)=elen(1)
      ior=1             ! Laufindex im Strang (T-QSim verwendet nur erstes Profil(Punkt) im Strang)
      anze=1            ! Anzahl der Profile im aktuellen Strang
      dzres1(1) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
      dzres1(2) = dzres1(1)
      dzres2(1) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten
      dzres2(2) = dzres2(1)
      hSchlr(1,1) = benthic_distribution_p(16+(i-1)*number_benth_distr) ! Sauerstoffzehrung durch das Sediments, Ausgabe in mgO2/(l*h) 
      hSchlr(1,2) = hSchlr(1,1)

      eo2(1)=0.0         ! Einleite-Konzentration (keine Einleitungen - nicht verwendet)
      qeinl(1)=0.0       ! kein Abfluss Einleitung
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
      jiein(1)=0        ! keine Punkt-Einleitungen

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
      wge(1)=wge_T(i2)        ! Windgeschwindigkeit  aus Wetterstationsdaten
      wge(2)=wge(1)
      idwe(1,1)= 1 ! Vorgaben so als ob nur eine Wetterstation an dem knoten existiert (i2 ist falsch)  ! Wetterstation Nr.
      idwe(1,2)=idwe(1,1)

      fkm (1)=0.0  ! Flusskilometer (unbenutzt)
      uhrz=uhrzeit_stunde ! Uhrzeit module ::modell zeitsekunde() 
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
      ilbuhn=0          ! keine Buhnen
      iwied=0      ! unbenutzte Variable

      do j=1,num_lev ! Sauerstoffgehalt tiefenaufgelöst
         vo2z(j,1)=plankt_vari_vert_p(j+(2-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
         vo2z(j,2)=vo2z(j,1)
      end do ! alle j tiefenlevels
      susO2N(1)= 0.0     ! unbenutzte Variable
      susO2N(2)=susO2N(1)
      nkzs(1)=1          ! nur eine Tiefenschicht
      nkzs(2)=nkzs(1)
      dH2D = 0.25 ! Dicke Tiefenschicht ??? Übergabe an Einleiter_Misch() | qsim.f90: dH2D = 0.25

      o2L(1)= 0.0     ! keine linienquelle
      qeinlL(1) = 0.0 ! keine linienquelle
      iorLa(1)= 0 ! keine linienquelle
      iorLe(1)= 0 ! keine linienquelle
      ieinLs(1)= 0 ! keine linienquelle

!!<tr><td>     agnh4z,aknh4z,abnh4z	</td><td>  Ammoniumaufnahme der Algen gruen, kiesel und blau, tiefenaufgel.	</td><td> OXYGEN nur Rückgabewert	</td><td>   </td></tr>
!!<tr><td>     agrnh4,akinh4,ablnh4	</td><td>  Ammoniumaufnahme der Algen gruen, kiesel und blau, tiefengem.	</td><td> OXYGEN nur Rückgabewert	</td><td>   </td></tr>
!!<tr><td>     agno3z,akno3z,abno3z	</td><td>  Nitrataufnahme der Algen gruen, kiesel und blau, tiefenaufgel.	</td><td> OXYGEN nur Rückgabewert	</td><td>   </td></tr>
      do j=1,num_lev_trans
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

      do j=1,num_lev_trans
         algakz(j,1) = trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! 
         algakz(j,2) = algakz(j,1)
         algagz(j,1) = trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! 
         algagz(j,2) = algagz(j,1)
         algabz(j,1) = trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! 
         algabz(j,2) = algabz(j,1)
         vz1(j,1) = trans_quant_vert_p(j+(21-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! lokale Sauerstoffzehrung/produktion tiefenaufgelöst  ! 
         vz1(j,2) = vz1(j,1)
         tempwz(j,1)= plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! wassertemperatur tiefenaufgelöst
         tempwz(j,2)= tempwz(j,1)
      end do ! alle j tiefenlevels
      saett(1) = transfer_quantity_p(45+(i-1)*number_trans_quant) ! Sauerstoff Sättigungs-Konzentration in mgO2/l 
      saett(2) = saett(1)
      mstr=1            ! Strangzähler ein Strang mit nur einem Profil in Hüllroutine
      write(cpfad,'(A)')'cpfad unbenutzt in oxygen, funk.dat ausser Betrieb'
      ! Belüftungsparameter werden nicht mehr von funk.dat gelesen, sondern im Aufruf übergeben.
      ij=0         ! zeitschritt im Tag
      ! itags=tag           ! direkt aus QSimDatenfelder
      ! monats=monat          ! direkt aus QSimDatenfelder

      dC_DenW(1:2) = transfer_quantity_p(90 +(i-1)*number_trans_quant)! dC_DenW(ior) = dNO3Den/0.93 ! C-Abbau durch Denitrifikation in der Wassersäule  
      TOC_CSB = 3.1  !....Berechnung der "BSB-Komponenten" am oberen Rand   !     (auch bei Stundenwert-Generierung)!!!!!                           
      Wlage(1,1:2)=zone(point_zone(iglob))%wettstat%wetterstations_lage ! Höhenlage der zuständigen Wetterstation mü.NHN 
      hWS(1,1:2)= rb_hydraul_p(3+(i-1)*number_rb_hydraul) ! Wasserspiegellage, von holen_trans() gesetzt

      etemp(1)=0.0         ! Einleite-Temperatur (da keine Einleitungen - nicht verwendet)
      dH2De =0.0   ! unbenutzt                                                  &

      ! Ansteuerung der Belüftungsformel: 
      !! Erübrigt das Lesen von funk.dat
      !iphyw = 0
      !iphy = 0 
      !zwgmes = 0.0

      ifehl=0  ! if ISNAN(tempmt)(zwischenwert Wassertemperatur) > ifehl=24
      ifhStr=0 ! Strangnummer in dem der Fehler auftrat

      ! azStrs - direkt aus QSimDatenfelder
      zooind(1) = planktonic_variable_p(50+nk)  ! Anzahl der Rotatorien
      zooind(2) = zooind(1)
!     iphy = 1	! neue Formel von mir mit Wind
!     iphy = 2	! neue Formel von mir ohne Wind
!     iphy = 3	! Formel von Wolf
!     iphy = 4	! Formel von Melching
      ! direkt aus EREIGG.txt gelesen
      if((iphy.lt. 1).or.(iphy.gt. 4))then
         call qerror('iphy out of range 1...4 , aeration method, set in EREIGG.txt') ! Reaeration rate
      endif

      if(kontroll) print*,'oxygen (davor): vo2(1),vo2(2),i,FLAE(1)=',vo2(1),vo2(2),i, FLAE(1)
      do k=1,number_benth_distr
         if(isnan(benthic_distribution_p(k+(i-1)*number_benth_distr)))then
            print*,'vor oxygen: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
            if(meinrang==0)print*,'benth_distr_name:',benth_distr_name(k)
         endif
      end do

!qsim13.40 15okt18
  call oxygen(VO2,TEMPW,RAU,VMITT,TIEFE,rhyd,FLAE,TFLIE,go2n,dalgki,dalggr,dalgak,dalgag,akinh4              &
                    ,agrnh4,akino3,agrno3,bsbt,hJO2,flag,elen,ior,anze,dzres1,dzres2,hschlr                        &
                    ,eo2,qeinl,vabfl,po2p,po2r,so2ein,dO2o2D,salgo,dalgo,dalgao,o2ein1,jiein                       &
                    ,opgrmi,opgrma,opkimi,opkima,albewg,alberg,abeowg,abeorg,opblmi,opblma,ablnh4                  &
                    ,ablno3,dalgbl,dalgab,albewk,alberk,abeowk,abeork,ro2dr,wge,IDWe,fkm,uhrz                      &
                    ,zooro2,rO2HNF,ilbuhn,iwied,vo2z,susO2N,nkzs,dH2D,o2L,qeinlL                                   &
                    ,iorLa,iorLe,ieinLs,agnh4z,aknh4z,abnh4z,dalgkz,dalgbz,dalggz,agno3z,akno3z                    &
                    ,abno3z,algakz,algagz,algabz,vz1,tempwz,saett,mstr,cpfad,ij,itags,monats                       &
                    ,dC_DenW,TOC_CSB,WLage,hWS,etemp,dH2De,ifehl,ifhStr,azStrs,zooind,GROTe,iphy            &       !,chlagr  unbenutzt                                          
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
      do j=1,num_lev ! Sauerstoffgehalt tiefenaufgelöst
         plankt_vari_vert_p(j+(2-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = vo2z(j,1)
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
      do j=1,num_lev_trans
         trans_quant_vert_p(j+(21-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = vz1(j,1) ! lokale Sauerstoffzehrung/produktion tiefenaufgelöst  ! 
      end do ! alle j tiefenlevels
      transfer_quantity_p(45+(i-1)*number_trans_quant) = saett(1) ! Sauerstoff Sättigungs-Konzentration in mgO2/l 

      ! checks
      if(kontroll) print*,'oxygen (danach): plankt2,vo2(1),vo2(2),i=',planktonic_variable_p(2+nk),vo2(1),vo2(2),i
      do k=1,number_plankt_vari
         if(isnan(planktonic_variable_p(k+nk)))then
            print*,'nach oxygen: isnan(planktonic_variable_p  node#',iglob,' variable# ',k
            if(meinrang==0)print*,'planktonic_variable_name:',planktonic_variable_name(k)
         endif
      end do
      do k=1,number_trans_quant
         if(isnan(transfer_quantity_p(k+(i-1)*number_trans_quant)))then
            print*,'nach oxygen: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang=',meinrang
            if(meinrang==0)print*,'trans_quant_name:',trans_quant_name(k)
         endif
      end do
      do k=1,number_benth_distr
         if(isnan(benthic_distribution_p(k+(i-1)*number_benth_distr)))then
            print*,'nach oxygen: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
            if(meinrang==0)print*,'benth_distr_name:',benth_distr_name(k)
         endif
      end do

      RETURN 
      END subroutine oxygen_huelle
!----+-----+----
!> die Subroutine ini_oxygen() steht in oxygen_huelle.f95\n
!! schreibt zunächst testwerte in die Sauerstoff-variablen. \n
!! prozess 0 alleine
!! ### ausgeschaltet in initialisieren() ### Vorbelegung durch randwerte
      SUBROUTINE ini_oxygen()
      use modell                                                 
      implicit none
      integer i,j,nk
      real :: sauerstoffsaettigung

      !! 100% Sauerstoffsättigung:
      do i=1,number_plankt_point
         nk=(i-1)*number_plankt_vari
         planktonic_variable(2+nk) = &
         sauerstoffsaettigung(planktonic_variable(1+nk)) !! vO2_saett(T)
         do j=1,num_lev
            plankt_vari_vert(j+(2-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) =  &
            planktonic_variable(2+nk) ! Sauerstoffgehalt tiefenaufgelöst
         end do ! alle j tiefenlevels
      end do
      print*,'Anfangsbedingung: 100% Sauerstoffsättigung vO2(1)=',planktonic_variable(2+(1-1)*number_plankt_vari)

      RETURN 
      END subroutine ini_oxygen
                                          
!----+-----+----
      real function sauerstoffsaettigung(T)
      implicit none
      real :: T
         sauerstoffsaettigung =  &
     &   14.603-T*0.40215+(T**2) *0.007687-(T**3)*0.0000693  
      END function sauerstoffsaettigung
                                          
!----+-----+----
      SUBROUTINE aufteilung_oxygen()
      RETURN 
      END subroutine aufteilung_oxygen
                                          
!----+-----+----
!      end module oxygen_module
!! 
!! \n\n
!!       UST = [(((1./RAU(ior))*9.81**0.5)/tiefe(ior)**0.166667)*abs(VMITT(ior))] \n
!!       Slope = UST**2/(9.81*Tiefe(ior)) \n
!!       Breite = flae(ior)/tiefe(ior) \n
!!       wge10 = wge(IDWe(mstr,ior))/((zwgmes/10.)**0.17 ) \n
!! \n
!!       b0= ((3.+40./rau(ior))*abs(vmitt(ior))/tiefe(ior)**2)+0.5/tiefe(ior) \n
!!       b2= (142.*(abs(vmitt(ior))*Slope)**0.333)/((tiefe(ior)**0.66)*(Breite**0.243))\n
!!       w1= (0.19*wge10-0.015*wge10**2+0.002*wge10**3)/tiefe(ior)\n
!! \n
!!       if((iphy.eq.0).and.(iphyw.eq.0)) then \n
!!       bbei(ior) = b0\n
!! \n
!!       if((iphy.eq.0).and.(iphyw.eq.1)) then \n
!!       bbei(ior) = b0 + w1\n
!!                                              \n         
!!       if((iphy.eq.2).and.(iphyw.eq.0)) then \n
!!       bbei(ior) = b2                          \n   
!!                                               \n        
!!       if((iphy.eq.2).and.(iphyw.eq.1)) then \n
!!       bbei(ior) = b2 + w1                    \n     
!! \n
!!      DELO2 = (V+BBEI(ior)*DEFIZ)/(1.+BBEI(ior)*0.5) \n
!!      vo2t = vo2(ior)+delo2 \n
!! \f[ 
!! O_2(t+\delta t) = O_2(t) + \frac{( \delta O_2 + BBEI(ior)*DEFIZ)}{(1.+BBEI(ior)*0.5)}
!! \f]
!! mit:
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \delta t \f$  </td><td> tflie </td><td> Zeitschrittweite </td><td> h </td><td> 0.25 ... 24 </td></tr>
!!<tr><td> \f$ t \f$  </td><td> - </td><td> Zeit </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  pro Zeitschritt </td><td> mgO2/l </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  </td><td>  </td><td>  </td><td>  </td></tr>
!!</table>\n

