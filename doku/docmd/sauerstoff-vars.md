Sauerstoff - Formelzeichen/Variablennamen {#lnk_sauerstoff_vars}
=========================================

## Liste der Formelzeichen und Variablennamen des Rotatorien-Bausteins: ##

| Formelzeichen               | Codevariable  | Wert  | Einheit           | Bedeutung                                         | Herkunft |
| --------------------------- | ------------- | ----- | ----------------- | --------------------------------------------------| -------- |
| \f$ \Delta O_{A,j} \f$ | dalgo | - | mg O_2/L je Zeitschritt | Sauerstoffproduktion der Gruen-, Kiesel-, und Blaualgen  | b (?) |
| \f$ O_2 \f$            | vo2   | - | mg O_2/L                |  Sauerstoffgehalt                                        | e, b |
| \f$ \Delta O_2 \f$     | v     | - | mg O_2/L je Zeitschritt | lokale Änderung des Sauerstoffgehalts pro Zeitschritt    | b     |
| \f$ \Delta_{saett} \f$ | Defiz | - | mg O_2/L                | Sauerstoffdefizit (Untersättigung) siehe \ref lnk_o2_oberflaechenaustausch    |       |
|           |   |       |   |     |       |





Herkunft:
+ x - Bilanzvariable QSim
+ b - berechnet, Zwischengröße / Übergabevariable
+ e - Eingabe
+ v - Vorgabe im Quellcode gesetzt

<hr>
aus Datei: sauerstoff-vars.md; Codesource: oxygen.f90; 
zurück: \ref lnk_sauerstoff

<!-- alte Var-Tabelle -->

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
!! <tr><td> opgrmi  </td><td> ! \ref lnk_globale_parameter direkt aus QSimDatenfelder </td><td> Min. O2-Prod. Grünalgen </td></tr>
!! <tr><td>  opgrma  </td><td> ! \ref lnk_globale_parameter direkt aus QSimDatenfelder  </td><td> Max. O2-Prod. Grünalgen </td></tr>
!! <tr><td>  opkimi  </td><td> ! \ref lnk_globale_parameter direkt aus QSimDatenfelder  </td><td> Min. O2-Prod. Kieselalgen </td></tr>
!! <tr><td>  opkima  </td><td> ! \ref lnk_globale_parameter direkt aus QSimDatenfelder  </td><td> Max. O2-Prod. Kieselalgen </td></tr>
!! <tr><td>  albewg(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (13+(i-1)*number_benth_distr) </td><td> Wachstum benthischer gruen-Algen </td></tr>
!! <tr><td>  alberg(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (11+(i-1)*number_benth_distr) </td><td> Respiration benthischer gruen-Algen </td></tr>
!! <tr><td>  abeowg(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (20+(i-1)*number_benth_distr) </td><td> Sauerstoffproduktion benthischer Grünalge </td></tr>
!! <tr><td>  abeorg(1)  </td><td>  benthische_verteilungen::benthic_distribution_p (21+(i-1)*number_benth_distr) </td><td> Sauerstoffverbrauch benthischer Grünalgen </td></tr>
!! <tr><td>   </td><td>  </td><td>  </td></tr>
!! <tr><td> opblmi  </td><td> ! \ref lnk_globale_parameter direkt aus QSimDatenfelder  </td><td> Min. O2-Prod. Blaualgen </td></tr>
!! <tr><td>  opblma  </td><td> ! \ref lnk_globale_parameter direkt aus QSimDatenfelder  </td><td> Max. O2-Prod. Blaualgen </td></tr>
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
