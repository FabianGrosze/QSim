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

!> \page Stickstoff Stickstoff
!!
!! Stickstoff wird von den Algen als Nährstoff benötigt, sein Fehlen kann das Algenwachstum limitieren.\n
!! <center> 
!! \image html schemaN.png ""
!! <a href="./img/SchemaN.ppt" target="_blank">Download des Schaubilds als .ppt</a>
!! \image latex schemaN.png "Teil-Prozesse des Stickstoffkreislaufs" width=0.95\textwidth
!! </center>
!!
!! <h2>Herkunft</h2>
!!     ncyc()\n
!!     UNTERPROGRAMM ZUR BERECHNUNG DEs Ammoniums und des Nitrits und Nitrats   \n                                               
!!     AUTOR:VOLKER KIRCHESCH      \n                                                                                                           
!!     entnommen aus Version qsim13.301_28mae18\n 
!! 
!! <h2>Teilprozesse</h2>
!! <h3>Gesamtsticktoff</h3>
!! <ul>
!!   <li>Sedimentation von organischem Material </li>
!!   <li>Sedimentation von Algen </li>
!!   <li>Ammoniumstickstofffluss in bzw. aus dem Sediment </li>
!!   <li>Nitratstickstofffluss in bzw. aus dem Sediment </li>
!!   <li>Aufnahme von Luftstickstoff durch Blaualgen </li>
!!   <li>Denitrifikation in der Wassersäule </li>
!! </ul>
!! 
!! <h3>Nitrifikanten</h3> 
!! <ul>
!!   <li>Wachstum von Nitrosomonas und Nitrobacter </li>
!!   <li>Mortalität von Nitrosomonas und Nitrobacter </li>
!!   <li>Sedimentation von Nitrosomonas und Nitrobacter </li>
!! </ul>
!! 
!! <h3>Ammonium</h3>
!! <ul>
!!   <li>Verbrauch beim Algenwachstum </li>
!!   <li>Verbrauch durch Nitritation von Nitrosomonas </li>
!!   <li>Fluss in bzw. aus dem Sediment  </li>
!!   <li>Umsatz durch Biofilme auf Makrophyten </li>
!!   <li>Freisetzung durch bakteriellen Abbau von Biomasse </li>
!!   <li>Freisetzung durch Algen (Respiration) planktisch und bentisch </li>
!!   <li>Freisetzung durch Rotatorien (Respiration+Fraß) </li>
!!   <li>Freisetzung durch Muscheln (Respiration+Fraß) </li>
!! </ul>
!!
!! <h3>Nitrat</h3>
!! <ul>
!!   <li>Verbrauch beim Algenwachstum </li>
!!   <li>Fluss in bzw. aus dem Sediment  </li>
!!   <li>Umsatz durch Biofilme auf Makrophyten </li>
!!   <li>Freisetzung durch Nitratation von Nitrobacter </li>
!!   <li>Denitrifikation in der Wassersäule </li>
!! </ul>
!!
!! <h3>Nitrit</h3>
!! <ul>
!!   <li>Verbrauch durch Nitratation von Nitrobacter </li>
!!   <li>Freisetzung durch Nitritation von Nitrosomonas </li>
!!   <li>Umsatz durch Biofilme auf Makrophyten </li>
!! </ul>
!!
!! <h3>Sauerstoffverbrauch</h3>
!! <ul>
!!   <li>O2-Verbrauch durch Nitrifikation NH4N -> NO2N -> NO3N</li>
!! </ul>
!! \n \n
!! 
!! <h2>Schnittstellenbeschreibung</h2>
!!       Subroutine ncyc()\n
!! ( \ref tempw, \ref  vx0, \ref  vnh4, \ref  tflie , \ref  rau, \ref  tiefe, \ref  vmitt, \ref  rhyd
!! , \ref   vo2, \ref   go2n, \ref   vno3, \ref   dc_denw, \ref  flag, \ref  elen, \ref  ior, \ref  anze  & \n                
!! , *enh4*, \ref  eno3, \ref  ex0, \ref  qeinl, \ref  vabfl, \ref   pfl, \ref   sgo2n, \ref   sedx0, \ref   don, \ref   susn
!! , \ref   bettn, \ref   susno, \ref   agrnh4, \ref   akinh4, \ref   dzres1, \ref   dzres2  & \n                
!! , \ref   agrno3, \ref   akino3, \ref  jiein, \ref  ischif, \ref   ynmx1e, \ref   stks1e, \ref   anitrie, \ref   bnmx1e, \ref   bnks1e, \ref   vph, \ref   vno2, \ref  ij  &\n
!! , \ref   albewg, \ref   alberg, \ref   albewk, \ref   alberk, \ref   resdr, \ref   aki, \ref   agr, \ref   exdrvk
!! , \ref   exdrvg, \ref   vx02, \ref   ex02, \ref   eno2, \ref   ynmx2e, \ref   stks2e, \ref   anitri2e  & \n                
!! , \ref   abl, \ref   ablnh4, \ref   ablno3, \ref   exdrvb, \ref   bnmx2e, \ref   bnks2e
!! , \ref   nl0, \ref   zooind, \ref   grote, \ref   nzoo, \ref   gesn, \ref   orgcsd  &\n
!! , \ref  egesn, \ref   sedalk, \ref   sedalb, \ref   sedalg, \ref  ilbuhn, \ref  iwied, \ref  fkm
!! , cd( \ref cd1, \ref   cd2), cp( \ref cp1, \ref   cp2), \ref   cm, \ref   bac
!! , \ref   bsbct, \ref  nkzs, \ref   vnh4z, \ref   vno2z, \ref   vno3z, \ref  dh2d  & \n                
!! , \ref   hjno3, \ref   hjnh4, \ref  hjn2, *suso2n*, \ref   hflun3, \ref   akksn, \ref   agksn, \ref   abksn
!! , \ref   qmx_nk, \ref   q_nk, \ref   up_nkz, \ref   qmx_ng, \ref   q_ng, \ref   up_ngz, \ref   qmx_nb      &\n
!! , \ref   q_nb, \ref   up_nbz, \ref   dalgkz, \ref   dalgbz, \ref   dalggz, \ref   agnh4z, \ref   aknh4z
!! , \ref   abnh4z, \ref   agno3z, \ref   akno3z, \ref   abno3z, \ref   vo2z, \ref   abltbr             &\n
!! , \ref   akitbr, \ref   agrtbr, \ref   agrbrz, \ref   akibrz, \ref   ablbrz, \ref  mstr, \ref  uhrz, \ref  itags, \ref  monats, \ref  enl0
!! , \ref   algakz, \ref   algagz, \ref   algabz                &\n
!! , \ref   up_n2z ,iorla, \ref  iorle, \ref  ieinls, \ref  flae, *qeinll*, *enh4l*, *eno2l*, *eno3l*, *gesnl*, \ref   hgesnz, \ref   algdrk, \ref   algdrg, \ref   algdrb  &\n
!! , \ref  ifehl, \ref  ifhstr, \ref   azstrs, \ref  ialloc2, \ref  kontroll , \ref  iglob )
!! \n\n
!! (Die nicht verlinkten Variablen haben in QSim3D keine Bedeutung oder werden in ncyc nicht verwendet)
!! \n \n
!! z.Z. in Bearbeitung: \subpage N_Isotope
!! \n\n
!! <h2>Rand und Anfangsbedingungen</h2>
!! Aufteilung im Zufluss mit naehr_start(); 
!! Siehe dazu auch \ref randbedingungen_ergaenzen .
!! 
!! <h2>Dokumentation</h2>
!! Bisher existiert eine Dokumentation des Stickstoff-Moduls als Kapitel 10 der
!! <a href="./pdf/QSimDoku_ncycWy.pdf" target="_blank">Kurzdoku</a> (Version vom 22. Nov. 2017)
!! \n\n
!! Dazu auch das <a href="./pdf/NITROLIMIT2014diskpapier3.pdf" target="_blank">Diskussionspapier</a> aus NITROLIMIT zu gelösten organischen Stickstoffverbinungen.
!!\n\n
!! zurück: \ref lnk_ueberblick ; Code: ncyc_huelle.f95

!! <code>\verbatim Z:\U\U2\Intern\U2Gewässergruppe\QSim\Dokumentation_u_Handbuecher\QSim\QSimDokumentation\QSimDoku.docx \endverbatim</code>
!! <a href="./pdf/QSimDoku_10Stickstoff.pdf" target="_blank">Stickstoff-Moduls</a> als Kapitel 10 von 

!! <h4>Direkter Datenaustausch mit anderen Bausteinen</h4>
!!<table >
!!<tr><th> Baustein 		</th><th> QSim-Subroutine	</th><th> Teilprozess </th></tr>
!!<tr><td> Sauerstoff 		</td><td> oxygen		</td><td> n,o,r </td></tr>
!!<tr><td> Kohlenstoff 		</td><td> orgc 			</td><td> p </td></tr>
!!<tr><td> Rotatorien 		</td><td> konsum		</td><td> f </td></tr>
!!<tr><td> Muscheln 		</td><td> dreissen		</td><td> g </td></tr>
!!<tr><td> Algen (schwebend) 	</td><td> algaesgr, -ki, -bl	</td><td> d,e,l,q </td></tr>
!!<tr><td> Algen (benthisch) 	</td><td> albenth		</td><td> d,e,l </td></tr>
!!<tr><td> Macrophyten 		</td><td> mphyt			</td><td> b,j,k </td></tr>
!!<tr><td> PH-Wert 		</td><td> ph			</td><td> a,i,n,o </td></tr>
!!<tr><td> Temperatur 		</td><td> temperw		</td><td> n,o </td></tr>
!!</table>
!!

!! Die T-QSim Subroutine ncyc_huelle() dient dem Aufruf der QSim-subroutine ncyc(), die folgende Übergabeparameter benötigt:
!! \n(Zum Hüllroutinen-Konzept siehe: \ref hüllen )
!!<table >
!!<tr><th>     Variablen-Name QSim	 </th><th> Daten-Feld T-QSim	</th><th> Beschreibung </th></tr>
!!<tr><td> tempw(1) </td><td> \ref planktische_variablen 1 </td><td> aktuelle, lokale Wassertemperatur </td></tr>
!!<tr><td> vx0(1)  </td><td> planktische_variablen::planktonic_variable_p (15+nk)  </td><td> nitrosomonas </td></tr>
!!<tr><td> vNH4(1) </td><td> \ref planktische_variablen 3 </td><td> ammonium </td></tr>
!!<tr><td> tflie </td><td> real(deltat)/86400 </td><td> Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim) </td></tr>
!!<tr><td> rau(1)</td><td>benthische_verteilungen::benthic_distribution_p (5+(i-1)*number_benth_distr) </td><td> Strickler Reibungsbeiwert </td></tr>
!!<tr><td> tiefe(1) </td><td> randbedingungen rb_hydraul_p (2+(i-1)*number_rb_hydraul) </td><td> Wassertiefe </td></tr>
!!<tr><td> vmitt(1) </td><td> randbedingungen rb_hydraul_p (1+(i-1)*number_rb_hydraul) </td><td> Geschwindigkeitsbetrag | für die ABHAENGIGKEIT DER NITRIFIKATIONSLEISTUNG SESSILER NITRIFIKANTEN VON DER FLIESSGESCHWINDIGKEIT: </td></tr>
!!<tr><td> rhyd(1) </td><td> tiefe(1) </td><td> hydraulischer Radius | sinnvollste Annahme im mehrdimensionalen </td></tr>
!!<tr><td> vo2(1) </td><td> \ref planktische_variablen 2 </td><td> Sauerstoffgehalt tiefengemittelt </td></tr>
!!<tr><td> go2n(1) </td><td> uebergabe_werte::transfer_quantity_p (32+(i-1)*number_trans_quant) </td><td> FUER DIE STICKSTOFFOXYDATION VERBRAUCHTE SAUERSTOFFMENGE </td></tr>
!!<tr><td> vno3(1) </td><td> \ref planktische_variablen 5 </td><td> Nitrat in mgN/l</td></tr>
!!<tr><td> bsbt(1) </td><td> uebergabe_werte::transfer_quantity_p (1+(i-1)*number_trans_quant) </td><td> Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt (nur Rückgabe) </td></tr>
!!<tr><td> flag(1)</td><td>0 </td><td> keine Einleitungen | ncyc so benutzen , dass nur der 1. Strang mit nur einem Knoten/Profil berechnet wird </td></tr>
!!<tr><td> elen(1)</td><td>1 </td><td> Elementlänge (nicht verwendet) </td></tr>
!!<tr><td> ior</td><td>1 </td><td> Laufindex </td></tr>
!!<tr><td> anze</td><td>1 </td><td> Anzahl der Profile im aktuellen Strang </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> enh4(1) </td><td> 0.0  </td><td> keine Einleitung </td></tr>
!!<tr><td> eno3(1) </td><td> 0.0  </td><td> keine Einleitung </td></tr>
!!<tr><td> ex0(1) </td><td> 0.0   </td><td> keine Einleitung </td></tr>
!!<tr><td> qeinl(1) </td><td> 0.0 </td><td> kein Abfluss Einleitung </td></tr>
!!<tr><td> vabfl(1) </td><td> 2.5 !! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet. </td></tr>
!!<tr><td> pfl(1) </td><td> benthische_verteilungen::benthic_distribution_p (3+(i-1)*number_benth_distr) </td><td> Trockengewicht Wasserpflanzen in g/m² </td></tr>
!!<tr><td> sgo2n(1) </td><td> uebergabe_werte::transfer_quantity_p (51+(i-1)*number_trans_quant) </td><td> Aufsummierter Nitrifikationssauerstoffverbrauch, nur Ausgabe </td></tr>
!!<tr><td> sedx0(1) </td><td> benthische_verteilungen::benthic_distribution_p (33+(i-1)*number_benth_distr) </td><td> sedimentierte Nitrosomonasbiomasse in µg/l, nur Ausgabewert </td></tr>
!!<tr><td> doN(1) </td><td> uebergabe_werte::transfer_quantity_p (3+(i-1)*number_trans_quant) </td><td> mineralisierter N-Gehalt in der Wassersäule ; Ammoniumfreisetzung beim Abbau org. Kohlenstoffverbidungen </td></tr>
!!<tr><td> susn(1) </td><td> uebergabe_werte::transfer_quantity_p (29+(i-1)*number_trans_quant) </td><td> Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE </td></tr>
!!<tr><td> bettn(1) </td><td> benthische_verteilungen::benthic_distribution_p (34+(i-1)*number_benth_distr) </td><td> OXYDIERTE STICKSTOFFMENGE AM GEWAESSERBETT </td></tr>
!!<tr><td> susno(1) </td><td> uebergabe_werte::transfer_quantity_p (52+(i-1)*number_trans_quant) </td><td> ? </td></tr>
!!<tr><td> agrnh4(1) </td><td> uebergabe_werte::transfer_quantity_p (34+(i-1)*number_trans_quant) </td><td> Ammoniumaufnahme der gruen-Algen </td></tr>
!!<tr><td> akinh4(1) </td><td> uebergabe_werte::transfer_quantity_p (33+(i-1)*number_trans_quant) </td><td> Ammoniumaufnahme der kiesel-Algen </td></tr>
!!<tr><td> dzres1(1) </td><td> uebergabe_werte::transfer_quantity_p (27+(i-1)*number_trans_quant) </td><td> Grund-Respiration Konsumenten </td></tr>
!!<tr><td> dzres2(1) </td><td> uebergabe_werte::transfer_quantity_p (28+(i-1)*number_trans_quant) </td><td> Fraßabhängige Respirationsrate Konsumenten </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> agrno3(1) </td><td> uebergabe_werte::transfer_quantity_p (37+(i-1)*number_trans_quant) </td><td> Nitrataufnahme der gruen-Algen </td></tr>
!!<tr><td> akino3(1) </td><td> uebergabe_werte::transfer_quantity_p (36+(i-1)*number_trans_quant) </td><td> Nitrataufnahme der kiesel-Algen </td></tr>
!!<tr><td> jiein(1)</td><td>0 </td><td> null Punkt-Einleitungen </td></tr>
!!<tr><td> ischif(1) </td><td> 0 </td><td> hier unbenutzt </td></tr>
!!<tr><td> ynmx1e </td><td> ! direkt aus QSimDatenfelder </td><td>  max. Wachstumsr. Nitrosomonas  </td></tr>
!!<tr><td> stks1e</td><td> ! direkt aus QSimDatenfelder </td><td> Halbsättigungskonzen. Nitrosomonas </td></tr>
!!<tr><td> anitrie</td><td> ! direkt aus QSimDatenfelder </td><td> in (79) nochmal vorhanden </td><td>  Absterberate für Nitrifikanten </td></tr>
!!<tr><td> bnmx1e</td><td> ! direkt aus QSimDatenfelder </td><td> Max. Umsatzrate Ammonium sessile Organismen </td></tr>
!!<tr><td> bnks1e</td><td> ! direkt aus QSimDatenfelder </td><td> Michaelis-Menten Konstante für den sessilen Umsatz von Ammonium </td></tr>
!!<tr><td> vph(1) </td><td> planktische_variablen::planktonic_variable_p (66+nk) </td><td> PH-Wert </td></tr>
!!<tr><td> vno2(1) </td><td> planktische_variablen::planktonic_variable_p (4+nk)  </td><td> nitrit </td></tr>
!!<tr><td> ij</td><td>0 </td><td> unbenutzte Variable </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> albewg(1) </td><td> benthische_verteilungen::benthic_distribution_p (13+(i-1)*number_benth_distr) </td><td> Wachstum benthischer gruen-Algen </td></tr>
!!<tr><td> alberg(1) </td><td> benthische_verteilungen::benthic_distribution_p (11+(i-1)*number_benth_distr) </td><td> Respiration benthischer gruen-Algen </td></tr>
!!<tr><td> albewk(1) </td><td> benthische_verteilungen::benthic_distribution_p (14+(i-1)*number_benth_distr) </td><td> Wachstum benthischer kiesel-Algen </td></tr>
!!<tr><td> alberk(1) </td><td> benthische_verteilungen::benthic_distribution_p (12+(i-1)*number_benth_distr) </td><td> Respiration benthischer kiesel-Algen </td></tr>
!!<tr><td> resdr(1)</td><td>benthische_verteilungen::benthic_distribution_p (15+(i-1)*number_benth_distr) </td><td> Respirationsrate benthischer Filtrierer (Dreissena-Muscheln) </td></tr>
!!<tr><td> aki(1) </td><td> planktische_variablen::planktonic_variable_p (8+nk) </td><td> Anteil kiesel-Algen  </td></tr>
!!<tr><td> agr(1) </td><td> planktische_variablen::planktonic_variable_p (9+nk) </td><td> Anteil gruen-Algen </td></tr>
!!<tr><td> exdrvk(1)</td><td>benthische_verteilungen::benthic_distribution_p (29+(i-1)*number_benth_distr) </td><td> exkretierte Biomasse der Muscheln beim Verzehr von Kiesel-Algen </td></tr>
!!<tr><td> exdrvg(1)</td><td>benthische_verteilungen::benthic_distribution_p (30+(i-1)*number_benth_distr) </td><td> exkretierte Biomasse der Muscheln beim Verzehr von Grün-Algen </td></tr>
!!<tr><td> vx02(1) </td><td> planktische_variablen::planktonic_variable_p (16+nk)  </td><td> nitrobacter </td></tr>
!!<tr><td> ex02(1) </td><td> 0.0 </td><td> keine Einleitung </td></tr>
!!<tr><td> eno2(1) </td><td> 0.0 </td><td> keine Einleitung </td></tr>
!!<tr><td> ynmx2e</td><td> ! direkt aus QSimDatenfelder </td><td> max. Wachstumsr. Nitrobacter </td></tr>
!!<tr><td> stks2e</td><td> ! direkt aus QSimDatenfelder </td><td> Halbsättigungskonzen. Nitrobacter </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> abl(1) </td><td> planktische_variablen::planktonic_variable_p (10+nk) </td><td> Anteil ? Blau-Algen </td></tr>
!!<tr><td> ablnh4(1) </td><td> uebergabe_werte::transfer_quantity_p (35+(i-1)*number_trans_quant) </td><td> Ammoniumaufnahme der blau-Algen, tiefengem. </td></tr>
!!<tr><td> ablno3(1) </td><td> uebergabe_werte::transfer_quantity_p (38+(i-1)*number_trans_quant) </td><td> Nitrataufnahme der blau-Algen, tiefengem. </td></tr>
!!<tr><td> exdrvb(1)</td><td>benthische_verteilungen::benthic_distribution_p (31+(i-1)*number_benth_distr) </td><td> exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen </td></tr>
!!<tr><td> bnmx2e</td><td> ! direkt aus QSimDatenfelder </td><td> Maximale Umsatzrate von Nitrit durch sessile Organismen in g/(m²*d) #### geht nicht ?????? </td></tr>
!!<tr><td> bnks2e</td><td> ! direkt aus QSimDatenfelder </td><td> Michaelis-Menten Konst. sessil Umsatz Nitrit  in mg/l ?  </td></tr>
!!<tr><td> \ref nl0 </td><td> </td><td> Verhältnis von Stickstoff zu Kohlenstoff in organischem Material </td></tr>
!!<tr><td> zooind(1) </td><td> planktische_variablen::planktonic_variable_p (50+nk) </td><td> Anzahl der Rotatorien </td></tr>
!!<tr><td> GRote</td><td> ! direkt aus QSimDatenfelder </td><td> Gewicht einer Rotatorie µg </td></tr>
!!<tr><td> nZoo  </td><td> transfer_value_p(2) </td><td> Stickstoffanteil in der Rotatorienbiomasse mgN/mgBiom. </td></tr>
!!<tr><td> gesN(1) </td><td> planktische_variablen::planktonic_variable_p (67+nk) </td><td> gesamtstickstoff </td></tr>
!!<tr><td> orgCsd(1) </td><td> benthische_verteilungen::benthic_distribution_p (6+(i-1)*number_benth_distr) </td><td> Sedimentierte Menge an organischem Kohlenstoff (ohne lebende Algen und Rotatorien) </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> egesN(1) </td><td> 0.0 </td><td> hier keine Einleitung </td></tr>
!!<tr><td> sedalk(1)</td><td>benthische_verteilungen::benthic_distribution_p (26+(i-1)*number_benth_distr) </td><td> Sedimentierte Menge an Kiesel-Algen </td></tr>
!!<tr><td> sedalb(1)</td><td>benthische_verteilungen::benthic_distribution_p (28+(i-1)*number_benth_distr) </td><td> Sedimentierte Menge an Blau-Algen </td></tr>
!!<tr><td> sedalg(1)</td><td>benthische_verteilungen::benthic_distribution_p (27+(i-1)*number_benth_distr) </td><td> Sedimentierte Menge an Grün-Algen </td></tr>
!!<tr><td> ilbuhn</td><td>0 </td><td> keine Buhnen </td></tr>
!!<tr><td> iwied</td><td>0 </td><td> unbenutzte Variable </td></tr>
!!<tr><td> fkm (1)</td><td>0.0 </td><td> Flusskilometer (unbenutzt) </td></tr>
!!<tr><td> CD(1,1) </td><td> planktische_variablen::planktonic_variable_p (37+nk) </td><td> leicht abbaubare gelöste organische C-Verbindungen </td></tr>
!!<tr><td> CD(2,1) </td><td> planktische_variablen::planktonic_variable_p (38+nk) </td><td> schwer abbaubare gelöste organische C-Verbindungen </td></tr>
!!<tr><td> CP(1,1) </td><td> planktische_variablen::planktonic_variable_p (39+nk) </td><td> leicht abbaubare partikuläre organische C-Verbindungen </td></tr>
!!<tr><td> CP(2,1) </td><td> planktische_variablen::planktonic_variable_p (40+nk) </td><td> schwer abbaubare partikuläre organische C-Verbindungen </td></tr>
!!<tr><td> CM(1) </td><td> planktische_variablen::planktonic_variable_p (41+nk) </td><td> monomolekularen organischen C-Verbindungen </td></tr>
!!<tr><td> BAC(1) </td><td> planktische_variablen::planktonic_variable_p (42+nk) </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td></tr>
!!<tr><td> bsbct(1) </td><td> uebergabe_werte::transfer_quantity_p (47+(i-1)*number_trans_quant) </td><td> mineralisierter Kohlenstoffgehalt in der Wassersäule | unbenutzt </td></tr>
!!<tr><td> nkzs(1)</td><td>1 </td><td> bisher nur eine Tiefenschicht </td></tr>
!!<tr><td>    vNH4z(j,1) </td><td> planktische_variablen::plankt_vari_vert_p (j+(3-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) </td></tr>
!!<tr><td>    vno2z(j,1) </td><td> planktische_variablen::plankt_vari_vert_p (j+(4-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) </td></tr>
!!<tr><td>    vno3z(j,1) </td><td> planktische_variablen::plankt_vari_vert_p (j+(5-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) </td></tr>
!!<tr><td> dH2D </td><td> 0.25 </td><td> Dicke Tiefenschicht ??? Übergabe an Einleiter_Misch() | qsim.f90: dH2D </td><td> 0.25 </td></tr>
!!<tr><td> </td><td> </td><td>  </td></tr>
!!<tr><td> hJNO3(1,1)</td><td>benthische_verteilungen::benthic_distribution_p (35+(i-1)*number_benth_distr) </td><td> Nitrat-Freisetzung aus dem Sediment </td></tr>
!!<tr><td> hJNH4(1,1)</td><td>benthische_verteilungen::benthic_distribution_p (36+(i-1)*number_benth_distr) </td><td> Ammonium-Freisetzung aus dem Sediment </td></tr>
!!<tr><td> susO2N(1)</td><td> 0.0     </td><td> hier unbenutzte Variable </td></tr>
!!<tr><td> hFluN3(1,1)</td><td>benthische_verteilungen::benthic_distribution_p (37+(i-1)*number_benth_distr) </td><td> Ausgabe NitratFlux Wasser/Sediment in mgN/(l*h) </td></tr>
!!<tr><td> akksn</td><td> ! direkt aus QSimDatenfelder </td><td> N-Halbsättigung Kieselalgen </td></tr>
!!<tr><td> agksn</td><td> ! direkt aus QSimDatenfelder </td><td> N-Halbsättigung Grünalgen </td></tr>
!!<tr><td> Qmx_NK</td><td> ! direkt aus QSimDatenfelder </td><td> max. Stickstoffanteil Algenbiomasse kiesel </td></tr>
!!<tr><td> Q_NK(1) </td><td> planktische_variablen::planktonic_variable_p (30+nk) </td><td> Stickstoffanteil der Algenbiomasse kiesel </td></tr>
!!<tr><td>    up_NKz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> N-Aufnahmerate der Algengruppe kiesel </td></tr>
!!<tr><td> Qmx_NG</td><td> ! direkt aus QSimDatenfelder </td><td> max. N-Gehalt der Grünalgen </td></tr>
!!<tr><td> Q_NG(1) </td><td> planktische_variablen::planktonic_variable_p (33+nk) </td><td> Stickstoffanteil der Algenbiomasse gruen </td></tr>
!!<tr><td>    up_NGz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(2-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> N-Aufnahmerate der Algengruppe gruen </td></tr>
!!<tr><td> Qmx_NB</td><td> ! direkt aus QSimDatenfelder </td><td> max. Stickstoffanteil Algenbiomasse blau </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> Q_NB(1) </td><td> planktische_variablen::planktonic_variable_p (35+nk) </td><td> Stickstoffanteil der Algenbiomasse blau </td></tr>
!!<tr><td>    up_NBz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(3-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> N-Aufnahmerate der Algengruppe blau </td></tr>
!!<tr><td>    dalgkz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> </td></tr>
!!<tr><td>    dalgbz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> </td></tr>
!!<tr><td>    dalggz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> </td></tr>
!!<tr><td>    agnh4z(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(10-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Ammoniumaufnahme Grün-Algen  </td></tr>
!!<tr><td>    aknh4z(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+( 9-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Ammoniumaufnahme Kiesel-Algen </td></tr>
!!<tr><td>    abnh4z(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(11-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Ammoniumaufnahme Blau-Algen </td></tr>
!!<tr><td>    agno3z(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(16-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Nitrataufnahme Grün-Algen </td></tr>
!!<tr><td>    akno3z(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(15-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Nitrataufnahme Kiesel-Algen </td></tr>
!!<tr><td>    abno3z(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(17-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Nitrataufnahme Blau-Algen </td></tr>
!!<tr><td>    vo2z(j,1)  </td><td> planktische_variablen::plankt_vari_vert_p (j+(2-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) </td><td> Sauerstoffgehalt tiefenaufgelöst </td></tr>
!!<tr><td> abltbr(1) </td><td> uebergabe_werte::transfer_quantity_p (50+(i-1)*number_trans_quant) </td><td> ??? Blaualgen ??? </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> akitbr(1) </td><td> uebergabe_werte::transfer_quantity_p (48+(i-1)*number_trans_quant) </td><td> ? hier unbenutzt </td></tr>
!!<tr><td> agrtbr(1) </td><td> uebergabe_werte::transfer_quantity_p (49+(i-1)*number_trans_quant) </td><td> ? hier unbenutzt </td></tr>
!!<tr><td>    agrbrz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Wachstum ? Grün-Algen-Biomasse </td></tr>
!!<tr><td>    akibrz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Wachstum ? Kiesel-Algen-Biomasse </td></tr>
!!<tr><td>    ablbrz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Wachstum ? Blau-Algen-Biomasse </td></tr>
!!<tr><td> mstr</td><td>1 </td><td> Strangzähler </td></tr>
!!<tr><td> uhrz</td><td>uhrzeit_stunde </td><td> Uhrzeit module::modell zeitsekunde()  </td></tr>
!!<tr><td> itags</td><td>tag </td><td> Tag im Monat module::modell zeitsekunde() </td></tr>
!!<tr><td> monats</td><td>monat </td><td> Monat im Jahr module::modell zeitsekunde() </td></tr>
!!<tr><td> enl0(1) </td><td> 0.0 </td><td> hier keine Einleitung </td></tr>
!!<tr><td>    algakz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Respiration Kiesel-Algen </td></tr>
!!<tr><td>    algagz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Respiration Grün-Algen </td></tr>
!!<tr><td>    algabz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Respiration Blau-Algen
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td>    up_N2z(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(8-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Aufnahmerate von Luftstickstoff durch Blaualgen </td></tr>
!!<tr><td> iorLa(1)</td><td>0 </td><td> AnfangsKnoten der Linienquelle; nicht verwendet </td></tr>
!!<tr><td> iorLe(1)</td><td>0 </td><td> EndKnoten der Linienquelle; nicht verwendetiorLa, </td></tr>
!!<tr><td> ieinLs(1)</td><td>0 </td><td> null Linien-Einleitungen </td></tr>
!!<tr><td> flae(1) </td><td> 1000.0 !! unbenutzt da keine Einleitung </td></tr>
!!<tr><td> qeinlL(1)</td><td>0.0 </td><td> Zufluss Linienquelle; nicht verwendet </td></tr>
!!<tr><td> eNH4L(1) </td><td> 0.0 </td><td> hier keine Linienquelle </td></tr>
!!<tr><td> eNO2L(1) </td><td> 0.0 </td><td> hier keine Linienquelle </td></tr>
!!<tr><td> eNO3L(1) </td><td> 0.0 </td><td> hier keine Linienquelle </td></tr>
!!<tr><td> gesNL(1) </td><td> 0.0 </td><td> hier keine Linienquelle </td></tr>
!!<tr><td> </td><td> </td><td>
!!</table>
!!\n\n
!! zurück: \ref lnk_ueberblick ; Code: ncyc_huelle.f95


!! <h2>Übergabe-Parameter:</h2>
!!<table >
!!<tr><th>     Variablen-Name		</th><th> Beschreibung				</th><th> Kommuniziert mit:						</th><th> Daten-Typ, Feld-Dimension </th></tr>
!!
!!<tr><td>     vnh4			</td><td> Ammonium-Stickstoffkonzentration g/m³ tiefengemittelt		</td><td> Transportkonzentration ALGAESGR, ALGAESKI, ALGAESBL		</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     vnh4z			</td><td> Ammonium-Stickstoffkonzentration g/m³ tiefenaufgelöst		</td><td> (Transportkonzentration) ALGAESGR, ALGAESKI, ALGAESBL		</td><td> real, Dimension(50,1000) </td></tr>
!!<tr><td>     vno2			</td><td> Nitrit-Stickstoffkonzentration g/m³ tiefengemittelt		</td><td> Transportkonzentration					</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     vno2z			</td><td> Nitrit-Stickstoffkonzentration g/m³ tiefenaufgelöst		</td><td> (Transportkonzentration)					</td><td> real, Dimension(50,1000) </td></tr>
!!<tr><td>     vno3			</td><td> Nitrat-Stickstoffkonzentration g/m³ tiefengemittelt		</td><td> Transportkonzentration ALGAESGR, ALGAESKI, ALGAESBL		</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     vno3z			</td><td> Nitrat-Stickstoffkonzentration g/m³ tiefenaufgelöst		</td><td> (Transportkonzentration) ALGAESGR, ALGAESKI, ALGAESBL		</td><td> real, Dimension(50,1000) </td></tr>
!!<tr><td>     vx0			</td><td> Nitrosomonas 				</td><td> Transportkonzentration					</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     vx02			</td><td> Nitrobacter 				</td><td> Transportkonzentration					</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     gesN			</td><td> Gesamt-Stickstoffkonzentration g/m³ 			</td><td> Transportkonzentration					</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     Q_NG,Q_NK,Q_NB		</td><td>  Stickstoffanteil der Algenbiomasse gruen, kiesel und blau 		</td><td> Transportkonzentrationen ALGAESGR, ALGAESKI, ALGAESBL	</td><td> real, Dimension(1000)  </td></tr>
!!<tr><td>     \ref nl0			</td><td>  Verhältnis von Stickstoff zu Kohlenstoff in organischem Material	</td><td> Transportkonzentration ORGC				</td><td> real, Dimension(1000) </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     vo2			</td><td>  Sauerstoffgehalt tiefengemittelt					</td><td> OXYGEN			</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     vo2z			</td><td>  Sauerstoffgehalt tiefenaufgelöst					</td><td> OXYGEN			</td><td> real, Dimension(50,1000) </td></tr>
!!<tr><td>     go2n			</td><td>  FUER DIE STICKSTOFFOXYDATION VERBRAUCHTE SAUERSTOFFMENGE		</td><td> OXYGEN nur Rückgabewert	</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     sgo2n			</td><td>  Aufsummierter Nitrifikationssauerstoffverbrauch, nur Ausgabe		</td><td> OXYGEN nur Rückgabewert	</td><td>   </td></tr>
!!<tr><td>     susn			</td><td>  Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE		</td><td> PH	 nur Rückgabewert	</td><td>   </td></tr>
!!<tr><td>     vph			</td><td>  ph-wert								</td><td> PH				</td><td> real, Dimension(1000)  </td></tr>
!!<tr><td>     susno			</td><td>  ????									</td><td> Ausgabewert				</td><td>   </td></tr>
!!<tr><td>     susO2N			</td><td>  unbenutzt								</td><td> .				</td><td>   </td></tr>
!!<tr><td>     CD,CP,CM,BAC 		</td><td>  Transportkonzentrationen ??? 					</td><td> orgc	unbenutzt		</td><td td></tr>
!!<tr><td>     orgCsd 			</td><td>  Sedimentierte Menge an organischem Kohlenstoff (ohne lebende Algen und Rotatorien)	</td><td> ORGC		</td><td> real </td></tr>
!!<tr><td>     don	 		</td><td>  Ammoniumfreisetzung beim Abbau org. Kohlenstoffverbidungen je Zeitschritt</td><td> NCYC.f90: don(ior) = bsbctN(ior) ORGC.f90: = bsbct(ior)*nl0(ior)	</td><td>   </td></tr>
!!<tr><td>     bsbct			</td><td>  hier unbenutzt aus orgc						</td><td> ORGC unbenutzt		</td><td>   </td></tr>
!!<tr><td>     bsbctN 			</td><td>  Ammoniumfreisetzung beim Abbau org. Kohlenstoffverbidungen		</td><td> ORGC.f90 s. o.		</td><td>   </td></tr>
!!<tr><td>     bsbt 			</td><td>  unbenutzt								</td><td> ORGC unbenutzt		</td><td>   </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     nzoo			</td><td>  Stickstoffanteil in der Rotatorienbiomasse				</td><td> QSIM.f90 nZoo = 0.09		</td><td>   </td></tr>
!!<tr><td>     zooind			</td><td>  Anzahl der Rotatorien						</td><td> KONSUM			</td><td>   </td></tr>
!!<tr><td>     dzres1			</td><td>  Grundrespirationsrate Rotatorien					</td><td> KONSUM			</td><td>   </td></tr>
!!<tr><td>     dzres2			</td><td>  Futterabhängige Respirationsrate Rotatorien				</td><td> KONSUM			</td><td>   </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     agr,aki,abl		</td><td>  Anteil gruen, kiesel und blau -Algen					</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     abltbr,akitbr,agrtbr	</td><td>  Biomasse gruen, kiesel und blau -Algen ( nur blau für N2-Aufn.)	</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     agrbrz,akibrz,ablbrz	</td><td>  ???									</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     algagz,algakz,algabz	</td><td>  Respirierte Algenbiomasse der Algengruppen gruen, kiesel und blau	</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>  real, Dimension(50,1000) </td></tr>
!!<tr><td>     agksN,akksN,abksN	</td><td>  Halbsättigungskonstante für die Stickstoffaufnahme gr, ki bl; in 13_00 raus</td><td> ALGAESGR, ALGAESKI, ALGAESBL	, ALBENTH	</td><td>   </td></tr>
!!<tr><td>     agnh4z,aknh4z,abnh4z	</td><td>  Ammoniumaufnahme der Algen gruen, kiesel und blau, tiefenaufgel.	</td><td> OXYGEN nur Rückgabewert	</td><td>   </td></tr>
!!<tr><td>     agrnh4,akinh4,ablnh4	</td><td>  Ammoniumaufnahme der Algen gruen, kiesel und blau, tiefengem.	</td><td> OXYGEN nur Rückgabewert	</td><td>   </td></tr>
!!<tr><td>     agno3z,akno3z,abno3z	</td><td>  Nitrataufnahme der Algen gruen, kiesel und blau, tiefenaufgel.	</td><td> OXYGEN nur Rückgabewert	</td><td>   </td></tr>
!!<tr><td>     agrno3,akino3,ablno3	</td><td>  Nitrataufnahme der Algen gruen, kiesel und blau, tiefengem.		</td><td> OXYGEN nur Rückgabewert	</td><td>   </td></tr>
!!<tr><td>     Qmx_NK,Qmx_NG,Qmx_NB	</td><td>  max. Stickstoffanteil der Algenbiomasse gruen, kiesel und blau ??? 	</td><td> \ref globaleParameter APARAM.txt	</td><td>   </td></tr>
!!<tr><td>     up_NGz,up_NKz,up_NBz	</td><td>  N-Aufnahmerate der Algengruppen gruen, kiesel und blau		</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     sedalk,sedalb,sedalg	</td><td>  Sedimentierte Menge an Algen der Algenklassen gruen, kiesel und blau </td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     up_N2z			</td><td>  Aufnahmerate von Luftstickstoff durch Blaualgen			</td><td> ALGAESBL			</td><td>   </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     bnmx1e			</td><td> Maximale Umsatzrate von Ammonium durch sessile Organismen 		</td><td> \ref globaleParameter APARAM.txt 	</td><td>   </td></tr>
!!<tr><td>     bnks1e			</td><td> Michaelis-Menten Konstante für den sessilen Umsatz von Ammonium 	</td><td> \ref globaleParameter APARAM.txt	</td><td>   </td></tr>
!!<tr><td>     bnmx2e			</td><td> Maximale Umsatzrate von Nitrit durch sessile Organismen 		</td><td> \ref globaleParameter APARAM.txt	</td><td>   </td></tr>
!!<tr><td>     bnks2e 			</td><td> Michaelis-Menten Konstante für den sessilen Umsatz von Nitrit		</td><td> \ref globaleParameter APARAM.txt	</td><td>   </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     albewg,albewk		</td><td> Wachstum bentischer grün- und kiesel-Algen  				</td><td> ALBENTH		</td><td> real </td></tr>
!!<tr><td>     alberg,alberk		</td><td> Respiration bentischer grün- und kiesel-Algen  			</td><td> ALBENTH		</td><td> real </td></tr>
!!<tr><td></td></tr>
!!<tr><td>    hJNO3,hJNH4 		</td><td>  Ammonium- und Nitrit-Flüsse in/aus Sediment				</td><td> Sedimentbaustein seit QSim 13.??? </td><td> real, Dimension(50,1000)  </td></tr>
!!<tr><td>    sedx0 			</td><td>  sedimentierte Nitrosomonasbiomasse nur Ausgabewert in µg/l		</td><td> nur Ausgabewert	</td><td>   </td></tr>
!!<tr><td>    hFluN3 			</td><td>  Ausgabewert des NitratFluxes Wasser/Sediment in mgN/(l*h)		</td><td> nur Ausgabewert	</td><td>   </td></tr>
!!<tr><td>    bettn 			</td><td>  OXYDIERTE STICKSTOFFMENGE AM GEWAESSERBETT				</td><td> nur Ausgabewert	</td><td>   </td></tr>
!!<tr><td></td></tr>
!!<tr><td>    pfl 			</td><td>  Pflanzentrockengewicht in g/m2					</td><td>  benthische_verteilung MPHYT	</td><td> real, Dimension(1000)  </td></tr>
!!<tr><td></td></tr>
!!<tr><td>    resdr 			</td><td>  Respirationsrate benthischer Filtrierer (Muscheln)			</td><td> DREISSEN		</td><td>   </td></tr>
!!<tr><td>    exdrvg,exdrvk,exdrvb 	</td><td>  exkretierte Biomasse der Muscheln beim Verzehr von g,k,b Algen	</td><td> DREISSEN		</td><td>   </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     tempw			</td><td>  Wassertemperatur							</td><td> TEMPERW		</td><td>   </td></tr>
!!<tr><td>     rau,tiefe,vmitt,rhyd,vabfl	</td><td>  hydraulische Parameter					</td><td> eingelesen ???	</td><td>   </td></tr>
!!<tr><td>     flae			</td><td>  Querschnittsfläche des Gewässerkörpers				</td><td> nur für Linienquelle benötigt, eingelesen sysgenou 	</td><td>   </td></tr>
!!<tr><td>     tflie			</td><td>  Fließzeit in Tagen (Zeitschrittweite)				</td><td> ?			</td><td>   </td></tr>
!!<tr><td>     uhrz,itags,monats	</td><td>  Uhrzeit/Datum							</td><td> hier unbenutzt	</td><td>   </td></tr>
!!<tr><td>     ior,anze,mstr,elen	</td><td>  Zähler, Anzahl im Strang strangnr., Elementlänge			</td><td> eingelesen ??? 	</td><td>   </td></tr>
!!<tr><td>     flag			</td><td>  Einleitungsflag, 							</td><td> eingelesen ???	</td><td> integer, Dimension(1000) </td></tr>
!!<tr><td>     jiein 			</td><td>  Anzahl Einleitungen 							</td><td> eingelesen ???	</td><td> integer, Dimension(1000) 	</td></tr>
!!<tr><td>     qeinl,qeinlL 		</td><td>  Volumenstrom Punkteinl. Linieneinl.			</td><td> eingelesen ???	</td><td> real </td></tr>
!!<tr><td>     fkm			</td><td>  Kilometrierung 							</td><td> hier unbenutzt	</td><td>   </td></tr>
!!<tr><td>     ilbuhn			</td><td>  Schalter, ob Buhnen im Querprofil 					</td><td> eingelesen ???	</td><td>   </td></tr>
!!<tr><td></td></tr>
!!<tr><td>    ETEMP 			</td><td>  Temperatur des Einleiters/Nebengewässers iein 			</td><td> eingelesen ???	</td><td>   	</td></tr>
!!<tr><td>    ieinLs 			</td><td>  Anzahl der Linienquellen im Strang (mstr) 				</td><td> eingelesen ???	</td><td> integer, Dimension(50) 	</td></tr>
!!<tr><td>    iorLa 			</td><td>  AnfangsKnoten der Linienquelle ieinL des Strangs mstr 		</td><td> eingelesen ???	</td><td>    	</td></tr>
!!<tr><td>    iorLe 			</td><td>  EndKnoten der Linienquelle ieinL des Strangs mstr 			</td><td> eingelesen ???	</td><td>   	</td></tr>
!!<tr><td>    enh4,eno2,eno3,ex0,ex02,egesN </td><td> Punkt-eingeleitete Transportkonzentrationen 			</td><td> eingelesen ???	</td><td>   	</td></tr>
!!<tr><td>    eNH4L,eNO2L,eNO3L,gesNL 	</td><td>  Linieneinleitung  Transportkonzentrationen				</td><td> eingelesen ???	</td><td>   	</td></tr>
!!<tr><td>    enl0 			</td><td>  Verhältnis von Stickstoff zu Kohlenstoff in organischem Material, eingeleitet</td><td> eingelesen ???</td><td>   	</td></tr>
!!<tr><td></td></tr>
!!<tr><td>    ynmx1e,stks1e 		</td><td>  max. Wachstumsr. + Halbsättigungskonzen. Nitrosomonas 		</td><td> \ref globaleParameter APARAM.txt </td><td> real </td></tr>
!!<tr><td>    ynmx2e,stks2e 		</td><td>  max. Wachstumsr. + Halbsättigungskonzen. Nitrobacter 		</td><td> \ref globaleParameter APARAM.txt </td><td> real </td></tr>
!!<tr><td>    anitrie			</td><td>  Absterberate für Nitrifikanten					</td><td> \ref globaleParameter APARAM.txt </td><td> real </td></tr>
!!<tr><td></td></tr>
!!<tr><td>    nkzs 			</td><td>  Anzahl der Schichten (Tiefenverteilung 2D) 				</td><td> .			</td><td> integer, Dimension(1000) </td></tr>
!!<tr><td>    dH2D 			</td><td>  Schichtdicke Tiefenverteilung (tiefenaufgel.2D) konstante Abstände	</td><td> .			</td><td> real </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     dalgkz,dalgbz,dalggz	</td><td>  unbenutzt ???							</td><td> .			</td><td>   </td></tr>
!!<tr><td>     ij,ischif,iwied,GRote	</td><td>  unbenutzt ???  							</td><td> .			</td><td>   </td></tr>
!!<tr><td>     itrein			</td><td>  nicht verwendet in QSim 12.40 ???  aber in 13_00			</td><td> .			</td><td> integer, Dimension(50,100,100) </td></tr>
!!</table>
!! \n\n
!!
!!\n ! >>>> Plausible Funktionen bei: 
!!\n ! a Nitrifikation durch Nitrosomonas
!!\n ! b+k Ammonium/nitrat-Aufnahme durch Wachstum bentischer Algen
!!\n ! c Ammoniumfreisetzung durch bakteriellen Abbau von Biomasse 
!!\n ! d+l Ammonium/nitrat-Aufnahme durch Algenwachstum planktisch
!!\n ! e1 Ammoniumfreisetzung durch Algenrespiration
!!\n ! e2 Ammoniumfreisetzung durch Respiration bentischer Algen
!!\n ! f Ammoniumfreisetzung durch Rotatorien (Respiration+Fraß) 
!!\n ! g Ammoniumfreisetzung durch Muscheln (Respiration+Fraß)
!!\n ! h+m Sedimentflüsse Ammonium, Nitrat
!!\n ! i Nitrifikation durch Nitrobacter
!!\n ! j Nitritumsatz durch Biofilme auf Makrophyten 
!!\n ! n Wachstum, Mortalität und Sedimentation von Nitrosomonas
!!\n ! o Wachstum, Mortalität und Sedimentation von Nitrobacter
!!\n ! q N2-AufnahmeBlaualgen
!!\n ! p Sedimentation organisches Material
!!\n \n
!! zurück: \ref lnk_ueberblick ; Code: ncyc_huelle() ; Quelle: ncyc_huelle.f95
!
!> Beschreibung: \ref Stickstoff \n\n
!! WIRD VON ALLEN PROZESSEN AUFGERUFEN !!! (parallel)
      SUBROUTINE ncyc_huelle(i)
      use modell                                                 
      use isotope
      use QSimDatenfelder
      implicit none
      integer :: i,j,nk
      real :: f

      iglob=(i+meinrang*part)
      kontroll=iglob.eq.kontrollknoten
      nk=(i-1)*number_plankt_vari

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe: 

      tempw(1) = planktonic_variable_p( 1+nk)  ! Wassertemperatur
      tempw(2)= tempw(1) 
      vx0(1)  = planktonic_variable_p(15+nk)  ! nitrosomonas
      vx0(2)  = vx0(1)
      vNH4(1) = planktonic_variable_p(3+nk)  ! ammonium
      vNH4(2) = vNH4(1)
      tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
      tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe
      if(tiefe(1).le.min_tief)print*,'ncyc_huelle: tiefe(1).le.min_tief,iglob,meinrang',tiefe(1),iglob,meinrang
      rau(1:2)= strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
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
      flag(1)=0         ! keine Einleitungen
      flag(2)=flag(1)   ! ncyc so benutzen , dass nur der 1. Strang mit nur einem Knoten/Profil berechnet wird
      elen(1)=1         ! Elementlänge (nicht verwendet)
      elen(2)=elen(1)
      ior=1             ! Laufindex
      anze=1            ! Anzahl der Profile im aktuellen Strang
          
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
      jiein(1)=0        ! null Punkt-Einleitungen
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
      ij=0         ! unbenutzte Variable

      albewg(1) = benthic_distribution_p(13+(i-1)*number_benth_distr) ! Wachstum benthischer gruen-Algen
      albewg(2) = albewg(1)
      alberg(1) = benthic_distribution_p(11+(i-1)*number_benth_distr) ! Respiration benthischer gruen-Algen
      alberg(2) = alberg(1)
      albewk(1) = benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
      albewk(2) = albewk(1)
      alberk(1) = benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
      alberk(2) = alberk(1)
      resdr(1)=benthic_distribution_p(15+(i-1)*number_benth_distr) ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
      resdr(2) = resdr(1)
      aki(1) = planktonic_variable_p(8+nk) ! Anteil kiesel-Algen                       
      aki(2) = aki(1)
      agr(1) = planktonic_variable_p(9+nk) ! Anteil gruen-Algen
      agr(2) = agr(1)
      exdrvk(1)=benthic_distribution_p(29+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Kiesel-Algen
      exdrvk(2) = exdrvk(1)
      exdrvg(1)=benthic_distribution_p(30+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Grün-Algen
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
      exdrvb(1)=benthic_distribution_p(31+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen
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
      gesN(2) =  gesN(1)
      orgCsd(1,1) = benthic_distribution_p(6+(i-1)*number_benth_distr) ! Sedimentierte Menge an organischem Kohlenstoff (ohne lebende Algen und Rotatorien)
      orgCsd(1,2) = orgCsd(1,1)

      egesN(1) = 0.0        ! hier keine Einleitung
      sedalk(1)=benthic_distribution_p(26+(i-1)*number_benth_distr) ! Sedimentierte Menge an Kiesel-Algen
      sedalk(2) = sedalk(1)
      sedalb(1)=benthic_distribution_p(28+(i-1)*number_benth_distr) ! Sedimentierte Menge an Blau-Algen
      sedalb(2) = sedalb(1)
      sedalg(1)=benthic_distribution_p(27+(i-1)*number_benth_distr) ! Sedimentierte Menge an Grün-Algen
      sedalg(2) = sedalg(1)
      ilbuhn=0          ! keine Buhnen
      iwied=0           ! unbenutzte Variable
      fkm (1)=0.0       ! Flusskilometer (unbenutzt)
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
      nkzs(1)=1         ! bisher nur eine Tiefenschicht
      nkzs(2)=nkzs(1)
      do j=1,num_lev
         vNH4z(j,1) = plankt_vari_vert_p(j+(3-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
         vNH4z(j,2) = vNH4z(j,1) ! Ammonium-Stickstoffkonzentration g/m³ tiefenaufgelöst
         vno2z(j,1) = plankt_vari_vert_p(j+(4-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
         vno2z(j,2) = vno2z(j,1) ! Nitrit-Stickstoffkonzentration g/m³ tiefenaufgelöst
         vno3z(j,1) = plankt_vari_vert_p(j+(5-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
         vno3z(j,2) = vno3z(j,1) ! Nitrat-Stickstoffkonzentration g/m³ tiefenaufgelöst
      end do ! alle tiefenlevels
      dH2D = 0.25 ! Dicke Tiefenschicht ??? Übergabe an Einleiter_Misch() | qsim.f90: dH2D = 0.25
         
      hJNO3(1,1)=benthic_distribution_p(35+(i-1)*number_benth_distr) ! Nitrat-Freisetzung aus dem Sediment
      hJNO3(1,2) = hJNO3(1,1)
      !### veri13.3 ### benthic_distribution_p(36+(i-1)*number_benth_distr)=0.1 !### veri13.3 ###
      hJNH4(1,1)=benthic_distribution_p(36+(i-1)*number_benth_distr) ! Ammonium-Freisetzung aus dem Sediment
      hJNH4(1,2) = hJNH4(1,1)
      hJN2(1,1)=benthic_distribution_p(47+(i-1)*number_benth_distr) ! N2 Freisetzung aus dem Sediment
      hJN2(1,2) = hJN2(1,1)
      susO2N(1)= 0.0     ! unbenutzte Variable
      susO2N(2)=susO2N(1)
      hFluN3(1,1)=benthic_distribution_p(37+(i-1)*number_benth_distr) ! Ausgabe NitratFlux Wasser/Sediment in mgN/(l*h)
      hFluN3(1,2) = hFluN3(1,1)
      ! direkt aus QSimDatenfelder akksn=transfer_parameter_p(24) ! N-Halbsättigung Kieselalgen
      ! direkt aus QSimDatenfelder agksn=transfer_parameter_p( 4) ! N-Halbsättigung Grünalgen
      ! direkt aus QSimDatenfelder Qmx_NK=transfer_parameter_p(31) ! max. Stickstoffanteil Algenbiomasse kiesel
      Q_NK(1) = planktonic_variable_p(30+nk) ! Stickstoffanteil der Algenbiomasse kiesel
      Q_NK(2) = Q_NK(1)
      do j=1,num_lev_trans ! N-Aufnahmerate der Algengruppe kiesel
         up_NKz(j,1) = trans_quant_vert_p(j+(1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
         up_NKz(j,2) = up_NKz(j,1)
      end do ! alle j tiefenlevels
      ! direkt aus QSimDatenfelder Qmx_NG=transfer_parameter_p(10) ! max. N-Gehalt der Grünalgen
      Q_NG(1) = planktonic_variable_p(33+nk) ! Stickstoffanteil der Algenbiomasse gruen
      Q_NG(2) = Q_NG(1)
      do j=1,num_lev_trans ! N-Aufnahmerate der Algengruppe gruen
         up_NGz(j,1) = trans_quant_vert_p(j+(2-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
         up_NGz(j,2) = up_NGz(j,1)
      end do ! alle j tiefenlevels
      ! direkt aus QSimDatenfelder Qmx_NB=transfer_parameter_p(54) ! max. Stickstoffanteil Algenbiomasse blau

      Q_NB(1) = planktonic_variable_p(35+nk) ! Stickstoffanteil der Algenbiomasse blau
      Q_NB(2) = Q_NB(1)
      do j=1,num_lev_trans ! N-Aufnahmerate der Algengruppe blau
         up_NBz(j,1) = trans_quant_vert_p(j+(3-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
         up_NBz(j,2) = up_NBz(j,1)
      end do 
      do j=1,num_lev_trans ! ??? unbenutzt
         dalgkz(j,1) = trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         dalgkz(j,2) = dalgkz(j,1)
         dalgbz(j,1) = trans_quant_vert_p(j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         dalgbz(j,2) = dalgbz(j,1)
         dalggz(j,1) = trans_quant_vert_p(j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         dalggz(j,2) = dalggz(j,1)
      end do
      do j=1,num_lev_trans
     
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
      do j=1,num_lev ! Sauerstoffgehalt tiefenaufgelöst
         vo2z(j,1)  = plankt_vari_vert_p(j+(2-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
         vo2z(j,2)  = vo2z(j,1)
      end do
      abltbr(1) = transfer_quantity_p(50+(i-1)*number_trans_quant) ! ??? Blaualgen ???
      abltbr(2) = abltbr(1)

      akitbr(1) = transfer_quantity_p(48+(i-1)*number_trans_quant) ! ? hier unbenutzt
      akitbr(2) = akitbr(1)
      agrtbr(1) = transfer_quantity_p(49+(i-1)*number_trans_quant) ! ? hier unbenutzt
      agrtbr(2) = agrtbr(1)
      do j=1,num_lev_trans
         agrbrz(j,1) = trans_quant_vert_p(j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         agrbrz(j,2) = agrbrz(j,1) ! Wachstum ? Grün-Algen-Biomasse
         akibrz(j,1) = trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         akibrz(j,2) = akibrz(j,1) !  Wachstum ? Kiesel-Algen-Biomasse
         ablbrz(j,1) = trans_quant_vert_p(j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         ablbrz(j,2) = ablbrz(j,1) !  Wachstum ? Blau-Algen-Biomasse
      end do
      mstr=1            ! Strangzähler
      uhrz=uhrzeit_stunde ! Uhrzeit module::modell zeitsekunde() 
      itags=tag           ! Tag im Monat module::modell zeitsekunde()
      monats=monat        ! Monat im Jahr module::modell zeitsekunde()
      enl0(1) = 0.0       ! hier keine Einleitung
      do j=1,num_lev_trans
         algakz(j,1) = trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         algakz(j,2) = algakz(j,1) ! Respiration Kiesel-Algen
         algagz(j,1) = trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         algagz(j,2) = algagz(j,1) ! Respiration Grün-Algen
         algabz(j,1) = trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         algabz(j,2) = algabz(j,1)
      end do

      do j=1,num_lev_trans
         up_N2z(j,1) = trans_quant_vert_p(j+(8-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
         up_N2z(j,2) = up_N2z(j,1) ! Aufnahmerate von Luftstickstoff durch Blaualgen
      end do ! alle j tiefenlevels
      iorLa(1)=0              ! AnfangsKnoten der Linienquelle; nicht verwendet
      iorLe(1)=0              ! EndKnoten der Linienquelle; nicht verwendetiorLa,
      ieinLs(1)=0       ! null Linien-Einleitungen
      ieinLs(2)=ieinLs(1)
      flae(1) = 1000.0 !! unbenutzt da keine Einleitung
      flae(2) = flae(1)
      qeinlL(1)=0.0        ! Zufluss Linienquelle; nicht verwendet
      eNH4L(1) = 0.0       ! hier keine Linienquelle
      eNO2L(1) = 0.0       ! hier keine Linienquelle
      eNO3L(1) = 0.0       ! hier keine Linienquelle
      gesNL(1) = 0.0       ! hier keine Linienquelle

      do j=1,num_lev_trans
         hgesNz(1,j,1) = plankt_vari_vert_p(j+(16-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
         hgesNz(1,j,2) = hgesNz(1,j,1)
      end do ! alle j tiefenlevels

      !if(kontroll)then
      !   print*,' ncyc  vorher:'
      !   print*,' Nitrit, vx0(1) nitrosomonas=',planktonic_variable_p( 4+nk), vx0(1), planktonic_variable_p(15+nk)
      !   print*,' rau(1),tiefe(1),vmitt(1),rhyd(1)=',rau(1),tiefe(1),vmitt(1),rhyd(1)
      !endif
      if(kontroll)print*,'ncyc vorher: Ammonium,Nitrit,Nitrat,nitrosomonas,nitrobacter='  &
     &                    ,vNH4(1),vno2(1),vno3(1),vx0(1),vx02(1)
      if(kontroll)print*,'ncyc vorher: hJNO3,hJNH4,hJN2=',hJNO3(1,1),hJNH4(1,1),hJN2(1,1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

            call ncyc(tempw,vx0,vnh4,tflie,rau,tiefe,vmitt,rhyd,vo2,go2n,vno3,dC_DenW,flag,elen,ior,anze                 &                 
                     ,enh4,eno3,ex0,qeinl,vabfl,pfl,sgo2n,sedx0,don,susn,bettn,susno,agrnh4,akinh4,dzres1,dzres2         &                 
                     ,agrno3,akino3,jiein,ischif,ynmx1e,stks1e,anitrie,bnmx1e,bnks1e,vph,vno2,ij                         &
                     ,albewg,alberg,albewk,alberk,resdr,aki,agr,exdrvk,exdrvg,vx02,ex02,eno2,ynmx2e,stks2e,anitri2e      &                 
                     ,abl,ablnh4,ablno3,exdrvb,bnmx2e,bnks2e,nl0,zooind,GRote,nzoo,gesN,orgCsd                           &
                     ,egesN,sedalk,sedalb,sedalg,ilbuhn,iwied,fkm,CD,CP,CM,BAC,bsbct,nkzs,vnh4z,vno2z,vno3z,dH2D         &                 
                     ,hJNO3,hJNH4,hJN2,susO2N,hFluN3,akksN,agksN,abksN,Qmx_NK,Q_NK,up_NKz,Qmx_NG,Q_NG,up_NGz,Qmx_NB      &
                     ,Q_NB,up_NBz,dalgkz,dalgbz,dalggz,agnh4z,aknh4z,abnh4z,agno3z,akno3z,abno3z,vo2z,abltbr             &
                     ,akitbr,agrtbr,agrbrz,akibrz,ablbrz,mstr,uhrz,itags,monats,enl0,algakz,algagz,algabz                &
                     ,up_N2z,iorLa,iorLe,ieinLs,flae,qeinlL,eNH4L,eNO2L,eNO3L,gesNL,hgesNz,algdrk,algdrg,algdrb          &
                     ,ifehl,ifhstr, azStrs,    kontroll ,iglob )                                       

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe: 

      if( isnan(vno2z(1,1)) )print*,'ncyc nachher: isnan(vno2z)  node#',iglob
      if(kontroll)print*,'ncyc nachher: gesN,Ammonium,vNH4z,Nitrit,vno2z,Nitrat, nitrosomona,nitrobacter,nl0s=',  &
     &                    gesN(1),vNH4(1),vNH4z(1,1),vno2(1),vno2z(1,1),vno3(1),vx0(1),vx02(1),nl0(1)

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

      do j=1,num_lev
         plankt_vari_vert_p(j+(3-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = vNH4z(j,1)
         plankt_vari_vert_p(j+(4-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = vno2z(j,1)
         plankt_vari_vert_p(j+(5-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = vno3z(j,1)
         plankt_vari_vert_p(j+(16-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hgesNz(1,j,1)
      end do ! alle j tiefenlevels

      benthic_distribution_p(37+(i-1)*number_benth_distr) = hFluN3(1,1) ! Ausgabe NitratFlux Wasser/Sediment in mgN/(l*h)


!!    Isotope:
!      if(nkzs(ior)>1)      algN3m = agrno3(ior)+akino3(ior)+ablno3(ior) 
!      else                 algN3m = agno3z(1,ior)+akno3z(1,ior)+abno3z(1,ior)                                                    
!      if(vno3(ior).gt. 0.0)then
!         f=algN3m/vno3(ior) !! fraction consumed in this timestep
!         call isotop_no3konsum(f)
!      call isotop_no3konsum(f,kontroll)

      RETURN 
      END subroutine ncyc_huelle 
!----+-----+----
!      end module ncyc_module

!> \page ncyc_aufteilung Stickstoff-Aufteilung Zufluss
!! Q_NK = Qmx_NK \n
!! Q_NG = Qmx_NG \n
!! Q_NB = Qmx_NB \n
!! nl0  ### noch unklar ### \n
!! alle anderen stickstoff-relevanten Transportkonzentrationen VNH4, VNO2, VNO3, VX0, VX02 und gesN werden vorgegeben.
!! \n\n
!! zurück: \ref lnk_ueberblick oder  \ref Stickstoff ; Quelle: ncyc_huelle.f95


