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
!> \page Waermebilanz  temperw_Vorläuferversion
!!
!! Nachfolge: Markdown Doku \ref lnk_waerme
!!
!! <h2>Teilprozesse</h2>
!! \subpage lnk_wtemp_equ
!!
!! <h2>Parameter</h2>
!! \subpage lnk_wtemp_pars
!!
!! <h2>Umsetzung</h2>
!! \subpage lnk_wtemp_num
!!
!! <center>
!! \image html Waermehaushalts.png
!! </center>
!!\n\n
!! aus Datei temperw_huelle.f95; zurück zu \ref lnk_waerme
!
!
!> \page lnk_wtemp_equ  Wassertemperatur - Prozesse
!!
!! <h2>Teilprozesse</h2>
!! \n
!! <h3>Eingabe der Randbedingungen</h3>
!! Von entscheidender Bedeutung für die Berechnung der lokalen Wärmeflüsse sind die meteorologischen Bedingungen:\n
!! Sonnenstand, Bewölkung, Lufttemperatur und -feuchte sowie die Windgeschwindigkeit.\n\n
!! Diese Wetterdaten werden bei der eingabe() von Subroutine wetter_readallo() aus der Datei
!! <a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a> gelesen.
!! \n\n
!!       ! Wetterdaten für Waermebilanz in diesem Zeitschritt wurden in randbedingungen_setzen() ermittelt\n
!!       ! call wettles_wetter()  ! ersetzt wettles(), interpoliert Wetterdaten für den aktuellen Zeitpunkt\n
!!       ! call temperl_wetter()  ! ersetzt Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.\n
!!       ! call strahlg_wetter()  ! berechnet aus der Globalstrahlung den Strahlungsanteil, der im Gewässer ankommt.
!!\n\n
!! Im 1-dimensionalen QSim besteht die Möglichkeit <b>Wärmeeinleitungen</b> z.B. durch Kraftwerke direkt in der Temperaturberechnung
!! zu berücksichtigen. Im mehrdimensionalen T-QSim muss dies über Randbedingungen vorgegeben werden. D. h. einen Ausströmrand
!! an dem der Volumenstrom entnommen wird und einem Einströmrand, an dem das erwärmte Wasser ins Gewässer(Modellgebiet) zurückfließt.
!!
!! \n
!! aus Datei temperw_huelle.f95; zurück zu \ref Waermebilanz
!
! ------
!
!> \page lnk_wtemp_pars  Wassertemperatur - Parameter
!!<table >
!!<tr><th>     Variablen-Name QSim    </th><th> Daten-Feld T-QSim   </th><th> Beschreibung </th></tr>
!!<tr><td> ro(1)</td><td>ro_T (i2) </td><td> relative Luftfeuchte in % , subroutine wettles() </td></tr>
!!<tr><td> templ(1)</td><td>tlmed_T (i2)    </td><td> Lufttemperatur aus Wetterstationsdaten berechnet von temperl_module() </td></tr>
!!<tr><td> tempw(1)</td><td> planktonic_variable_p (1+nk)    </td><td> Wasser-Temperatur - auch Rückgabewert !!! </td></tr>
!!<tr><td> schwi(1)</td><td>schwi_T (i2)    </td><td> Globalstrahlung in cal/(cm2*h) von strahlg() berechnet 1,0 cal/(cm2*h) ~ 11,6 W/m²</td></tr>
!!<tr><td> wge(1)</td><td>wge_T (i2) </td><td> Windgeschwindigkeit  aus Wetterstationsdaten </td></tr>
!!<tr><td> tiefe(1)</td><td> rb_hydraul_p (2+(i-1)*number_rb_hydraul) </td><td> Wassertiefe aus randbedingungen.h </td></tr>
!!<tr><td> tflie </td><td> real(deltat)/86400 </td><td> Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim) </td></tr>
!!<tr><td> flag(1)</td><td>0 </td><td> keine Einleitungen </td></tr>
!!<tr><td> elen(1)</td><td>1 </td><td> Elementlänge (nicht verwendet) </td></tr>
!!<tr><td> ior</td><td>1 </td><td> Laufindex im Strang (T-QSim verwendet nur erstes Profil(Punkt) im Strang) </td></tr>
!!<tr><td> anze</td><td>1 </td><td> Anzahl der Profile im aktuellen Strang </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> etemp(1)</td><td>0.0  </td><td> Einleitetemperatur - keine Einleitungen </td></tr>
!!<tr><td> ewaerm(1)</td><td>0.0 </td><td> Eingeleitete Wärmemenge - keine Einleitungen </td></tr>
!!<tr><td> typ(1)</td><td>0  </td><td> unbenutzt </td></tr>
!!<tr><td> qeinl(1)</td><td>0.0  </td><td> Abfluss Einleitung - keine Einleitungen </td></tr>
!!<tr><td> vabfl(1)</td><td>0.0  </td><td> Abfluss wird aber hier nicht benutzt </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> jiein(1)</td><td>0  </td><td> keine Punkt-Einleitungen </td></tr>
!!<tr><td> cloud(1)</td><td>cloud_T (i2)    </td><td> Bewölkungsdichte  aus Wetterstationsdaten </td></tr>
!!<tr><td> typw(1)</td><td>typw_T (i2) </td><td> Wolkentyp  aus Wetterstationsdaten </td></tr>
!!<tr><td> iwied</td><td>0 </td><td> unbenutzte Variable </td></tr>
!!<tr><td> uhrz</td><td>uhrzeit_stunde </td><td> Uhrzeit module::modell zeitsekunde()  </td></tr>
!!<tr><td> ilbuhn</td><td>0  </td><td> keine Buhnen </td></tr>
!!<tr><td> uhrz</td><td>uhrzeit_stunde </td><td> Uhrzeit module:: </td></tr>
!!<tr><td> nwaerm</td><td>0 </td><td> unbenutzte Variable </td></tr>
!!<tr><td> fkm (1)</td><td>0.0  </td><td> Flusskilometer (unbenutzt) </td></tr>
!!<tr><td> nkzs(1)</td><td>1 </td><td> Anzahl Tiefenschichten </td></tr>
!!<tr><td>    tempwz(j,1) </td><td> planktische_variablen::plankt_vari_vert_p (j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) </td><td> Wassertemperatur tiefenaufgelöst auch Rückgabe </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> dH2D</td><td>tiefe(1) </td><td> delta_z ??? !tiefe(1)   </td></tr>
!!<tr><td> iorLa(1)</td><td>0  </td><td> zur Berücksichtigung der Linienquelle; nicht verwendet </td></tr>
!!<tr><td> iorLe(1)</td><td>0  </td><td> zur Berücksichtigung der Linienquelle; nicht verwendet </td></tr>
!!<tr><td> ieinLs(1)</td><td>0 </td><td> keine Linienquellen </td></tr>
!!<tr><td> FLAE(1)</td><td>tiefe(1)*500.0  </td><td> Breite konstant 500 m ; wird in der Belüftungsformel verwendet, hat aber keine Entsprechung im Mehrdimensionalen, daher sinnvoller Wert fürs Ästuar </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> qeinlL(1)</td><td>0.0 </td><td> für Linienquelle; nicht verwendet </td></tr>
!!<tr><td> etempL(1)</td><td>0.0 </td><td> für Linienquelle; nicht verwendet </td></tr>
!!<tr><td> mstr</td><td>1 </td><td> Strangzähler | nur ein Profil in einem Strang </td></tr>
!!<tr><td> idwe(1,1)</td><td> i2 </td><td> Wetterstation Nr. </td></tr>
!!<tr><td> ilang</td><td>0 </td><td> (unbenutzt) </td></tr>
!!<tr><td>    dtemp(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(22-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> lokaler Wärmeeintrag tiefenaufgelöst (tiefenprofil ausserhalb temperw) </td></tr>
!!<tr><td> FluxT1(1) </td><td> uebergabe_werte::transfer_quantity_p (46+(i-1)*number_trans_quant) </td><td> Wärmefluss tiefenintegriert ??? wohl Rückgabewert </td></tr>
!!<tr><td> itags</td><td>tag </td><td> Tag im Monat module::modell zeitsekunde()   (unbenutzt) </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> monats</td><td>monat </td><td> Monat im Jahr module::modell zeitsekunde() (unbenutzt) </td></tr>
!!<tr><td> tsed(1)</td><td>benthische_verteilungen::benthic_distribution_p (1+(i-1)*number_benth_distr)    </td><td> Temperatur des Sediments    - auch Rückgabewert  </td></tr>
!!<tr><td> Wlage(1,1)</td><td>wetterstations_lage(point_zone(iglob)) </td><td> Höhenlage der zuständigen Wetterstation mü.NHN  </td></tr>
!!<tr><td> hWS(1,1)</td><td> rb_hydraul_p (3+(i-1)*number_rb_hydraul) </td><td> Wasserspiegellage, von holen_trans() gesetzt </td></tr>
!!<tr><td> iRHKW</td><td>0 </td><td> (unbenutzt) </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> htempw(1,1) </td><td> 0.0 </td><td> zur Berücksichtigung von Linienquellen (in T-QSim nicht verwendet) Ausgabewert </td></tr>
!!<tr><td> htempz(1,1,1) </td><td> 0.0 </td><td> zur Berücksichtigung von Linienquellen (in T-QSim nicht verwendet) Ausgabewert </td></tr>
!!<tr><td> dH2De </td><td>0.0   </td><td> unbenutzt  </td></tr>
!!</table>
!! \n\n
!! zurück: \ref lnk_wtemp ; Code: temperw_huelle(); in: temperw_huelle.f95
!! \n
!!<table >
!!<tr><th>Variablen-Name </th><th> Beschreibung         </th><th> T-QSim Daten-Feld       </th></tr>
!!<tr><td>    RO   </td><td>   relative Luftfeuchte in % (subroutine wettles)         </td></tr>
!!<tr><td>    TEMPL   </td><td>   Lufttemperatur Subr. Temperl(...,TEMPL,...)         </td></tr>
!!<tr><td>    TEMPW   </td><td>   Wassertemperatur in Grad Celsius            </td></tr>
!!<tr><td>    SCHWI   </td><td>   Globalstrahlung in cal/(h*cm²))               </td></tr>
!!<tr><td>    WGE   </td><td>   Windgeschwindigkeit (an einer der 20 möglichen Wetterstationen) im m/s   </td></tr>
!!
!!<tr><td>    TIEFE   </td><td>   Wassertiefe   in m                  </td></tr>
!!<tr><td>    TFLIE   </td><td>   FLIESSZEIT IN TAGEN                   </td></tr>
!!<tr><td>    flag   </td><td>   flag ob Einleitung                  </td></tr>
!!<tr><td>    elen   </td><td>   Länge des Elements                  </td></tr>
!!
!!<tr><td>    ior   </td><td>   Laufindex (Übergabe wozu?)               </td></tr>
!!<tr><td>    anze   </td><td>   Anzahl Profile/Zellen in diesem Strang mstr !!!         </td></tr>
!!<tr><td>    etemp   </td><td>   Temperatur des Einleiters/Nebengewässers iein         </td></tr>
!!<tr><td>    ewaerm   </td><td>   Waermeeinleitung MJ/s (wird hier in Mcal/s umgerechnet "/4.2")   </td></tr>
!!<tr><td>    typ   </td><td>   %%% hier unbenutzt                  </td></tr>
!!
!!<tr><td>    qeinl   </td><td>   Abfluss Einleitung                  </td></tr>
!!<tr><td>    vabfl   </td><td>   Abfluss im Vorfluter m3/s               </td></tr>
!!<tr><td>    jiein   </td><td>   Anzahl der Einleitungen                  </td></tr>
!!<tr><td>    cloud   </td><td>   Bewölkungsdichte in Achteln (an einer der 20 möglichen Wetterstationen)   </td></tr>
!!<tr><td>    typw   </td><td>   Wolkentyp (an einer der 20 möglichen Wetterstationen)      </td></tr>
!!
!!<tr><td>    iwied   </td><td>   %%% hier unbenutzt                  </td></tr>
!!<tr><td>    uhrz   </td><td>   Uhrzeit ???                     </td></tr>
!!<tr><td>    ilbuhn   </td><td>   ???                        </td></tr>
!!<tr><td>    nwaerm   </td><td>   %%% hier unbenutzt                  </td></tr>
!!<tr><td>    fkm   </td><td>   Fluss Km, wird nur an Subr. Dichte weitergegeben      </td></tr>
!!
!!<tr><td>    nkzs   </td><td>   Anzahl Tiefenschichten                  </td></tr>
!!<tr><td>    tempwz   </td><td>   Tiefenverteilung Wassertemperatur            </td></tr>
!!<tr><td>    dH2D   </td><td>   delta_z (unbenutzt)                     </td></tr>
!!<tr><td>    iorLa   </td><td>   AnfangsKnoten der Linienquelle ieinL des Strangs mstr      </td></tr>
!!<tr><td>    iorLe   </td><td>   EndKnoten der Linienquelle ieinL des Strangs mstr        </td></tr>
!!
!!<tr><td>    ieinLs   </td><td>   Anzahl der Linienquellen im Strang (mstr)         </td></tr>
!!<tr><td>    flae   </td><td>   Querschnittsfläche des Gewässerkörpers            </td></tr>
!!<tr><td>    qeinlL   </td><td>   für Einleitung #                  </td></tr>
!!<tr><td>    etempL   </td><td>   für Einleitung #                  </td></tr>
!!<tr><td>    mstr   </td><td>   Strangzähler     !!!                  </td></tr>
!!
!!<tr><td>    IDWe   </td><td>   Kennung der zuständigen Wetterstation (in diesem von 1000 möglichen Strängen für dieses Profil in dieser von 50 möglichen Tiefenschichten)   </td></tr>
!!<tr><td>    ilang   </td><td>   %%% hier unbenutzt                  </td></tr>
!!<tr><td>    dtemp   </td><td>   Temperaturänderung ???                  </td></tr>
!!<tr><td>    FluxT1   </td><td>   ???                        </td></tr>
!!<tr><td>    extk   </td><td>   Extinktionskoeffizient                  </td></tr>
!!
!!<tr><td>    itags   </td><td>   Tag im Monat                     </td></tr>
!!<tr><td>    monats   </td><td>   Monat im Jahr %%% hier unbenutzt            </td></tr>
!!<tr><td>    Tsed   </td><td>   Sedimenttemperatur                  </td></tr>
!!<tr><td>    Wlage   </td><td>   Höhenlage der zuständigen Wetterstation mü.NHN         </td></tr>
!!<tr><td>    hWS   </td><td>   Höhenlage des Wasserspiegels mü.NHN            </td></tr>
!!
!!<tr><td>    iRHKW   </td><td>   %%% hier unbenutzt                  </td></tr>
!!<tr><td>    htempw   </td><td>   ?   </td></tr>
!!<tr><td>    htempz   </td><td>   ?   </td></tr>
!!<tr><td>    dH2De   </td><td>   ?                      </td></tr>
!!</table>
!! \n
!!<table >
!!<tr><th>   Name  </th><th> Beschreibung                     </th></tr>
!!<tr><td>      VABFL </td><td>   Abfluss im Vorfluter m3/s               </td></tr>
!!<tr><td>      EL    </td><td>   Dampfdruck der Luft in mm Hg od. *1.3333 in mbar      </td></tr>
!!<tr><td>      EW    </td><td>   Dampfdruck des Wassers in mm Hg od. *1.3333 in mbar      </td></tr>
!!<tr><td>      stbk  </td><td>   Stefan-Boltzmann-Konstante in KJ/(m2*h*k**4) <br> (2.0411e-7)   </td></tr>
!!<tr><td>      SCHWI </td><td>   Globalstrahlung in cal/(cm2*h)                </td></tr>
!!<tr><td>      A     </td><td>   Ausstrahlung                     </td></tr>
!!<tr><td>      G     </td><td>   Gegenstrahlung                     </td></tr>
!!<tr><td>      HR    </td><td>   Verdunstungshoehe in mm/d               </td></tr>
!!<tr><td>      VDW   </td><td>   Verdunstungswaerme in Kcal/Kg               </td></tr>
!!<tr><td>      WV    </td><td>   Waermestromdichte durch Verdunstung in cal/cm2/h      </td></tr>
!!<tr><td>      ROH2O </td><td>   Dichte des Wassers (1000.[Kg/m3])            </td></tr>
!!<tr><td>      SDDW  </td><td>   Saettigungsdampfdruck bei Wassertemperatur an der <br> Wasseroberflaeche [hPa]</td></tr>
!!<tr><td>      SDTT  </td><td>   Saettigungsdampfdruck bei Lufttemperatur [hPa]         </td></tr>
!!<tr><td>      PDLTT </td><td>   Partialdampfdruck der Luft bei der Temperatur TT [hPa]      </td></tr>
!!<tr><td>      speWKW</td><td>   spezifische Wärmekapazität des Wassers in KJ/(Kg*K)      </td></tr>
!!<tr><td>      speWKS</td><td>   spezifische Wärmekapazität des Sediments in KJ/(Kg*K)      </td></tr>
!!<tr><td>      rohS  </td><td>   Dichte des Sediments Kg/m3               </td></tr>
!!<tr><td>      WUEBK </td><td>   Wärmeübergangskoeffizient in KJ/(K*m2*h)         </td></tr>
!!<tr><td>      APARS </td><td>   Anteil PARS an der Globalstrahlung            </td></tr>
!!<tr><td>      EWAERM</td><td>   Waermeeinleitung MJ/s (wird hier in Mcal/s umgerechnet "/4.2")   </td></tr>
!!<tr><td>      ETEMP </td><td>   Temperatur des Einleiters/Nebengewässers iein         </td></tr>
!!<tr><td></td><td></td></tr>
!!<tr><td>      ieinLs</td><td>   Anzahl der Linienquellen im Strang (mstr)         </td></tr>
!!<tr><td>      ieinL </td><td>   Laufvariable der Linienquellen               </td></tr>
!!<tr><td>      iorLa </td><td>   AnfangsKnoten der Linienquelle ieinL des Strangs mstr      </td></tr>
!!<tr><td>      iorLe </td><td>   EndKnoten der Linienquelle ieinL des Strangs mstr        </td></tr>
!!
!!</table>
!!
!! <h2>Rueckgabewerte/Resultate:</h2>
!! Der Ergebnisrückgabe dienen die folgenden beiden Variablen (-Felder)
!! <table >
!! <tr><td>TEMPW    </td><td> tiefengemittelte Temperatur - im aktuellen Zeitschritt bis zu Knoten anze+1   </td></tr>
!! <tr><td>tempwz    </td><td> Tiefenverteilung der Temperatur -                   </td></tr>
!! </table>
!! \n\n
!!       ! Wetterdaten für Waermebilanz in diesem Zeitschritt wurden in randbedingungen_setzen() ermittelt
!!       ! call wettles_wetter()  ! ersetzt wettles(), interpoliert Wetterdaten für den aktuellen Zeitpunkt
!!       ! call temperl_wetter()  ! ersetzt Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.
!!       ! call strahlg_wetter()  ! berechnet aus der Globalstrahlung den Strahlungsanteil, der im Gewässer ankommt.
!! \n\n
!! zurück: \ref lnk_wtemp ; Code: temperw_huelle(); in: temperw_huelle.f95
!
!> Die Hüllroutine temperw_huelle(i) wird in "\ref lnk_wtemp " beschrieben;
!! dort findet sich auch eine ausfühliche schnittstellenbeschreibung zu temperw().
!! Quelle: temperw_huelle.f95
!! \n\n
!! Zum Hüllroutinen-Konzept siehe \ref lnk_huellen
!! \n\n
!! Parallelisierung ist erfolgt; die Subroutine wird von allen Prozessoren für ihre jeweiligen Knoten aufgerufen.
!!
!! aus Datei temperw_huelle.f95; zurück zu \ref Waermebilanz
!
! ------
!
!> \page lnk_wtemp_num  Wassertemperatur - Umsetzung
!! <h2>Herkunft</h2>
!!     temperw()\n
!!     EIN PROGRAMM ZUR BERECHNUNG DER WASSERTEMPERATUR\n
!!     AUTOR : VOLKER KIRCHESCH\n
!!     entnommen aus Version qsim13.301_28mae18\n
!!\n
!! Numerisch wird die Temperatur behandelt wie eine Konzentrationen.
!! Die im folgenden genannten lokalen Wärmeeinträge und -austräge (Wärmeflüsse)
!! sind quasi der *Stoffumsatz* der "Temperatur-Konzentration":\n
!!
!! <h2>Schnittstellenbeschreibung</h2>
!! call temperw()\n
!! ( \ref ro, \ref templ, \ref tempw, \ref schwi, \ref wge, \ref tiefe, \ref tflie, \ref flag
!! , \ref elen, \ref ior, \ref anze, *etemp*, *ewaerm*, *typ*, \ref qeinl, \ref vabfl   &\n
!! , \ref jiein, \ref cloud, \ref typw, \ref iwied, \ref uhrz, \ref ilbuhn, *nwaerm*
!! , \ref fkm, \ref nkzs, \ref tempwz, \ref dh2d, \ref iorla, \ref iorle, \ref ieinls, \ref flae &\n
!! , *qeinll*, *etempl*, \ref mstr, *idwe*, \ref ilang, \ref dtemp, \ref fluxt1
!! , \ref extk, \ref itags, \ref monats, \ref tsed, \ref wlage, \ref hws, *irhkw*      &\n
!! , *htempw*, *htempz*, \ref wuebks, \ref spewkss, \ref psrefss, \ref extks
!! , \ref ifehl, \ref ifhstr, \ref azstrs, \ref iwsim                   &\n
!! &, \ref kontroll, \ref iglob, *meinrang*)  \n
!! \n
!! temperw wird von der Hüllroutine temperw_huelle() aufgerufen. Zum Hüllroutinen-Konzept siehe: \ref lnk_huellen
!! <h3>Berechnungsablauf</h3>
!! in jedem Zeitschritt müssen für jede Wetterstation die folgenden Subroutinen abgearbeitet werden:\n
!! wettles_module()  ersetzt QSim-Subroutine wettles(), interpoliert die Wetterdaten für den aktuellen Zeitpunkt.\n
!! temperl_module()  ersetzt QSim-Subroutine Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.\n
!! strahlg_huelle()  berechnet aus der von <a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a>
!! eingelesenen Globalstrahlung den Strahlungsanteil, der im Gewässer ankommt.
!! durch Aufruf der QSim-Subroutine strahlg() unter Benutzung der QSim-Subroutinen sasu() und tage()\n
!! \n
!! <h3>Aufruf</h3>
!! Die Hüllroutine temperw_huelle(), wird von stoffumsatz() für alle Knoten
!! (die von dem jeweiligen prozessor parallel bearbeitet werden) aufgerufen.
!! Die Wassertemperatur und die Sedimenttemperatur werden dann von der QSim-Subroutine temperw() berechnet.
!! Dieser werden dafür die folgenden Variablen übergeben:
!! \n(Zum Hüllroutinen-Konzept siehe: \ref lnk_huellen )
!!
!! aus Datei temperw_huelle.f95; zurück zu \ref Waermebilanz
!! \n
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Daten-übergabe:
!ro(1)=transfer_quantity_p(63+(i-1)*number_trans_quant)        ! relative Luftfeuchte in % , subroutine wettles()
!ro(2)=ro(1)
!templ(1)=transfer_quantity_p(62+(i-1)*number_trans_quant)      ! air temperature at node i
!templ(2)=templ(1)
!tempw(1)=planktonic_variable_p(1+nk)    ! water-Temperatur - auch Rückgabewert !!!
!tempw(2)=tempw(1)
!schwi(1)=transfer_quantity_p(64+(i-1)*number_trans_quant)      ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
!schwi(2)=schwi(1)
!wge(1)=transfer_quantity_p(65+(i-1)*number_trans_quant)        ! Windgeschwindigkeit aus Wetterstationsdaten
!wge(2)=wge(1)
!tiefe(1)= rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
!if(i.eq.1)tiefe(1)= 0.01 ! #### test
!if(i.eq.2)tiefe(1)= 0.05 ! #### test
!if(i.eq.3)tiefe(1)= 0.25 ! #### test
!if(i.eq.4)tiefe(1)= 2.5 ! #### test
!if(i.eq.5)tiefe(1)= 8.0 ! #### test
!rb_hydraul_p(2+(i-1)*number_rb_hydraul)=tiefe(1) ! #### test
!if(tiefe(1).lt. min_tief )tiefe(1)=min_tief ! Minimaltiefe an trockenen Knoten
!tiefe(2)= tiefe(1)

!flag(1)=0         ! keine Einleitungen
!flag(2)=flag(1)
!elen(1)=1         ! Elementlänge (nicht verwendet)
!elen(2)=elen(1)
!ior=1             ! Laufindex im Strang (T-QSim verwendet nur erstes Profil(Punkt) im Strang)
!anze=1            ! Anzahl der Profile im aktuellen Strang
!etemp(1)=0.0              ! Einleitetemperatur - keine Einleitungen
!ewaerm(1)=0.0             ! Eingeleitete Wärmemenge - keine Einleitungen
!typ(1)=0                  ! unbenutzt
!qeinl(1)=0.0              ! Abfluss Einleitung - keine Einleitungen
!vabfl(1)=0.0              ! Abfluss wird aber hier nicht benutzt
!vabfl(2) = vabfl(1)
!jiein(1)=0        ! keine Punkt-Einleitungen
!cloud(1)=transfer_quantity_p(66+(i-1)*number_trans_quant)    ! Bewölkungsdichte  aus Wetterstationsdaten
!cloud(2)=cloud(1)
!typw(1)=transfer_quantity_p(67+(i-1)*number_trans_quant)      ! Wolkentyp  aus Wetterstationsdaten
!typw(2)=typw(1)
!iwied=0      ! unbenutzte Variable
!uhrz=uhrzeit_stunde ! Uhrzeit module::modell zeitsekunde()
!ilbuhn=0          ! keine Buhnen
!nwaerm=0      ! unbenutzte Variable
!fkm (1)=0.0  ! Flusskilometer (unbenutzt)
!nkzs(1)=1                 ! Anzahl Tiefenschichten
!nkzs(2)=nkzs(1)
!do j=1,num_lev
!   tempwz(j,1) =  & ! Wassertemperatur tiefenaufgelöst auch Rückgabe
!   plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
!   tempwz(j,2) = tempwz(j,1)
!end do ! alle j tiefenlevels
!iorLa(1)=0              ! zur Berücksichtigung der Linienquelle; nicht verwendet
!iorLe(1)=0              ! zur Berücksichtigung der Linienquelle; nicht verwendet
!ieinLs(1)=0             ! keine Linienquellen
!FLAE(1)=tiefe(1)*500.0  ! Breite konstant 500 m ; wird in der Belüftungsformel verwendet,
! hat aber keine Entsprechung im Mehrdimensionalen, daher sinnvoller Wert fürs Ästuar
!FLAE(2)=FLAE(1)
!qeinlL(1)=0.0              ! für Linienquelle; nicht verwendet
!etempL(1)=0.0              ! für Linienquelle; nicht verwendet
!mstr=1                     ! Strangzähler | nur ein Profil in einem Strang
!IDWe(1,1)= 1               ! Eigentlich Wetterstationsnummer ,muss aber 1 sein,
! weil in typw(1) wge(1) ro(1) die Daten der aktuellen Wetterstation haben
!IDWe(1,2)= IDWe(1,1)
!do j=1,num_lev_trans ! lokaler Wärmeeintrag tiefenaufgelöst (tiefenprofil ausserhalb temperw)
!   dtemp(j,1) = trans_quant_vert_p(j+(22-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
!   dtemp(j,2) = dtemp(j,1)
!end do ! alle j tiefenlevels
!FluxT1(1) = transfer_quantity_p(46+(i-1)*number_trans_quant) ! Wärmefluss tiefenintegriert ??? wohl Rückgabewert
!FluxT1(2) = FluxT1(1)
!extk(1) = transfer_quantity_p(54+(i-1)*number_trans_quant) ! mittlerer Extinktionskoeffizient
!extk(2) = extk(1)
!if((extk(1)<=0.0).and.(iglob.eq. 1))  print*,'temperw_huelle extk(1)<=0.0 ,extkS, iglob',extk(1),extkS(1,1),iglob
!if((extk(1)<=0.0).and.(iglob.eq. 2))  print*,'algaeski schon aktiv ??? ',extk(1),extkS(1,1),iglob
!itags=tag           ! Tag im Monat module::modell zeitsekunde()   (unbenutzt)
!monats=monat          ! Monat im Jahr module::modell zeitsekunde() (unbenutzt)
!tsed(1:2)=benthic_distribution_p(1+(i-1)*number_benth_distr)    ! Temperatur des Sediments    - auch Rückgabewert !!!
!Wlage(1,1:2)=zone(point_zone(iglob))%wettstat%wetterstations_lage! Höhenlage der zuständigen Wetterstation mü.NHN
!hWS(1,1:2)= rb_hydraul_p(3+(i-1)*number_rb_hydraul) ! Wasserspiegellage, von holen_trans() gesetzt
!iRHKW=0 ! (unbenutzt)
!htempw(1,1) = 0.0 ! zur Berücksichtigung von Linienquellen (in QSim3D nicht verwendet) Ausgabewert
!htempz(1,1,1) = 0.0 ! zur Berücksichtigung von Linienquellen (in QSim3D nicht verwendet) Ausgabewert
! Sedimenteigenschaften ggf von: MODELLG.3D.txt
!WUEBKS(1,1:2) = zone(point_zone(iglob))%seditemp%wuebk ! von Modellg gelesen, wenn 0.0=Standartwert Wärmeübergangskoeffizient verwenden.
!SPEWKSS(1,1:2)= zone(point_zone(iglob))%seditemp%spewks ! von Modellg gelesen, wenn 0.0= Standartwert spezifische Wärmekapazität des Sediments verwenden.
!PSREFSS ! unbenutzt  ,psrefs(izoni) ... trotzdem:
!PSREFSS(1,1:2) = zone(point_zone(iglob))%seditemp%psrefs ! Reflektionsanteil der Strahlung an der Sedimentoberfläche
!extkS ! nicht belegt, Verwendung abgefangen. ... dennoch:
!EXTKS(1,1:2) = zone(point_zone(iglob))%seditemp%extiks ! Extinktionskoeffizient für PARS (nur bei Temperaturmodellierung erforderlich!)
!ifehl=0  ! if ISNAN(tempmt)(zwischenwert Wassertemperatur) > ifehl=24
!ifhStr=0 ! Strangnummer in dem der Fehler auftrat
!iwsim = 3 ! Simulations-typ 3=alles
!version qsim13.40_15okt18
!      call temperw(      RO,TEMPL,TEMPW,SCHWI,WGE,TIEFE,TFLIE,flag,elen,ior,anze,etemp,ewaerm,typ,qeinl,vabfl    &
!                         ,jiein,cloud,typw,iwied,uhrz,ilbuhn,nwaerm,fkm,nkzs,tempwz,dH2D,iorLa,iorLe,ieinLs,flae &
!                         ,qeinlL,etempL,mstr,IDWe,ilang,dtemp,FluxT1,extk,itags,monats,Tsed,Wlage,hWS,iRHKW      &
!                         ,htempw,htempz,WUEBKS,SPEWKSS,PSREFSS,extkS,ifehl,ifhStr,azStrs,iwsim                   &
!                         ,kontroll ,iglob )
!
!      call temperw_kern(nkz,xnkzs,xtypw,xschwi,xextk,xhWS,xtempl,xro,xwge,xcloud,xWlage,dH2D, xdtemp_mit                           &
!                          ,tflie,WUEBK,SPEWKS,PSREFS,xtempwz,tempmt,xtempw,btiefe,xTsed,xdtemp,dtempS_mit,iform_VerdR)
subroutine temperw_huelle(i)
   ! SUBROUTINE temperw_huelle(ausgeben, temperatur_wa, tiefe_wa, geschw_wa, temperatur_sed, &
   !&                          temperatur_lu, luftfeuchte, wind, strahlung, bewoelkung, wolkentyp, delta_zeit)
   use modell
   use QSimDatenfelder
   implicit none
   !real :: temperatur_lu, luftfeuchte, wind, strahlung, bewoelkung, wolkentyp
   real :: xdtemp_nkz ! delte_temp_3D
   real :: xdtemp_mit ! delte_temp_2D
   integer :: i,j,nk
   !! vermutlich fehlerhaft verwendete Variablen:
   real :: tempmt
   real :: dtempS_mit
   real :: btiefe
   !        call temperw_kern(nkz,xnkzs,xtypw,xschwi,xextk,xhWS,xtempl,xro,xwge,xcloud,xWlage,dH2D, xdtemp_mit                                &
   !                          ,tflie,WUEBK,SPEWKS,PSREFS,xtempwz(1),tempmt,xtempw,btiefe,xTsed,xdtemp(nkz),dtempS_mit,iform_VerdR)
   !    nkz        :   Zähler Tiefenschichten (nkz=1: Oberflächenschicht; nkz=xnkzs: Sohlschicht)
   !    xnkzs      :   Anzahl der Tiefenschichten am Querprofil
   ! ### Die Tiefenschichten müssen immer von 1 bis xnkzs nacheinander aufgerufen werden ####
   !    xtypw      :   Wolkentyp (0-6)
   !    xschwi     :   Globalstrahlung am Querprofil [cal/(cm2*h)]
   !    xextk      :   Lichtextinktion [1/m]
   !    xhWS       :   Wasserspiegellage am Querprofil, Höhe ü. NN [m]
   !    xtempl     :   Lufttemperatur im Zeitschritt [°C]
   !    xro        :   relative Luftfeuchte im Zeitschritt [%]
   !    xwge       :   die in der Höhe zWmess gemessene Windgeschwindigkeit [m/s]
   !    xcloud     :   Bedeckungsgrad in achtel
   !    xWlage     :   Lage der Wetterstation, Höhe ü. NN [m]
   !    dH2D eigentlich delta_z Tiefenschicht-Dicke  z.Z. Wassertiefe ???
   !    xdtemp_mit :   mittlere Temperaturänderung in der Wassersäule [°C]
   !    tflie  Zeitschritt
   !    WUEBK      :   Wärmeübergangskoeffizient
   !    SPEWKS     :   spezifische Wärmekapazität des Sediments
   !    PSREFS     :   Reflektionsanteil der Strahlung an der Sedimentoberfläche
   !    xtempwz    :   Temperatur in der Tiefenschicht nkz am Querprofil [°C] 1 ???
   !    tempmt     :   Mittelwert der Wassertemperatur im Querprofil nach dem Zeitschritt tflie [°C]
   !    xtempw     :   Mittelwert der Wassertemperatur im Querprofil [°C]
   !    btiefe = tiefe
   !    xTsed      :   Sedimenttemperatur [°C]
   !    xdtemp_nkz :   Temperaturänderung in den einzelnen Tiefenschichten [°C/h]
   !    dtempS_mit :   Temperaturänderung durch Sedimenteinfluss (bezogen auf die gesamte Wassersäule) [°C]
   !    IFORM_VERDR:   Schalter für die Auswahl der Verdunstungsformeln
   !    iform_VerdR==1 ! WMO (FGSM-Handbuch)
   !    iform_VerdR==2 ! Sweers (1976) over Land
   !    iform_VerdR==3 ! Rimsha & Donschenko
   !    iform_VerdR==4 ! Priestley-Taylor (1972)
   !    iform_VerdR==5 ! Delclaux et al. (2007)
   
   !> i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   iglob = (i+meinrang*part)
   kontroll = (iglob == kontrollknoten)
   !if (kontroll) print*,'temperw_huelle meinrang,i,iglob,wetterstations_nummer,tlmed_T'
   nk = (i-1)*number_plankt_vari ! Ort im Feld der transportierten planktischen Variablen
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   
   if (num_lev > 1)call qerror("temperw_huelle not ready for 3D")
   if (kontroll) print*,'temperw vorher: temperw, extk, tiefe, temperwz1',planktonic_variable_p(1+nk)  &
       ,transfer_quantity_p(54+(i-1)*number_trans_quant),rb_hydraul_p(2+(i-1)*number_rb_hydraul)  &
       ,plankt_vari_vert_p(1+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
   !########################################################################################
   !  das Abarbeiten der einzelnen Schichten erfolgt von
   !  der Oberfläche zur Gewässersohle.(nkz=1: Oberflächenschicht; nkz=xnkzs: Sohlschicht)
   !  übergeben wird die Temperaturänderung dtemp in den einzelnen Schichten
   !########################################################################################
   btiefe = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   if (btiefe <= min_tief)btiefe = min_tief ! minimale Wassertiefe erhalten
   dH2D = btiefe  ! =tiefe           ! eigentlich delta_z
   
   if ((iform_VerdR < 1) .or. (iform_VerdR > 5)) then
      print*,meinrang,'temperw_huelle iform_VerdR = ',iform_VerdR
      call qerror('iform_VerdR unzulässiger Wert in temperw_huelle')
   endif
   do j = 1,num_lev ! Wassertemperatur tiefenaufgelöst von oben nach unten
      call temperw_kern(                          &
                        j                          &
                        ,num_lev  &
                        ,transfer_quantity_p(67+(i-1)*number_trans_quant)                           &
                        ,transfer_quantity_p(64+(i-1)*number_trans_quant)                           &
                        ,transfer_quantity_p(54+(i-1)*number_trans_quant)                            &
                        ,rb_hydraul_p(3+(i-1)*number_rb_hydraul)                            &
                        ,transfer_quantity_p(62+(i-1)*number_trans_quant)                           &
                        ,transfer_quantity_p(63+(i-1)*number_trans_quant)                           &
                        ,transfer_quantity_p(65+(i-1)*number_trans_quant)                           &
                        ,transfer_quantity_p(66+(i-1)*number_trans_quant)                           &
                        ,zone(point_zone(iglob))%wettstat%wetterstations_lage                            &
                        ,dH2D                          &
                        ,xdtemp_mit                                          &
                        ,tflie                          &
                        ,zone(point_zone(iglob))%seditemp%wuebk                             &
                        ,zone(point_zone(iglob))%seditemp%spewks                            &
                        ,zone(point_zone(iglob))%seditemp%psrefs                            &
                        ,plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)       &
                        ,tempmt                          &
                        ,planktonic_variable_p(1+nk)                          &
                        ,btiefe                           &
                        ,benthic_distribution_p(1+(i-1)*number_benth_distr)                           &
                        ,xdtemp_nkz                          &
                        ,dtempS_mit                          &
                        ,iform_VerdR                         &
                        ,kontroll, iglob )
      !   subroutine temperw_kern(nkz,xnkzs,xtypw,xschwi,xextk,xhWS,xtempl,xro,xwge,xcloud,xWlage,dH2D, xdtemp_mit                           &
      !                          ,tflie,WUEBK,SPEWKS,PSREFS,xtempwz1,tempmt,xtempw,btiefe,xTsed,xdtemp_nkz,dtempS_mit,iform_VerdR            &
      !                    ,kontroll ,jjj )
      if (kontroll) print*,meinrang,i,'nach temperw_kern tempw = ',planktonic_variable_p(1+nk),(1+nk)
      !! error check
      if ( isNaN(xdtemp_nkz) ) then
         write(fehler,*)meinrang," isNaN(xdtemp_nkz) ",j,i,iglob
         call qerror(fehler)
      endif
      if ( isNaN(xdtemp_mit) ) then
         write(fehler,*)meinrang," isNaN(xdtemp_mit) ",j,i,iglob
         call qerror(fehler)
      endif
      !! Temperatur change ...
      if (rb_hydraul_p(2+(i-1)*number_rb_hydraul) > min_tief) then ! wet nodes
         planktonic_variable_p(1+nk) = tempmt  ! = planktonic_variable_p(1+nk) + xdtemp_mit ! tempw(1)
         !plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) =       &
         !plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) * xdtemp_nkz ! tempwz(j,1)
      else ! dry nodes
         planktonic_variable_p(1+nk) = transfer_quantity_p(62+(i-1)*number_trans_quant) ! water temperature equals air temp.
      endif ! wet nodes
      !keine extra Tiefenauflösung im 3D
      plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = planktonic_variable_p(1+nk)
   end do ! all j num_lev
   return
end subroutine temperw_huelle
!----+-----+----
!! \ page Strahlungsbilanz Strahlungsbilanz
!! <ol>
!! <li><h2>STRAHLUNGSBILANZ</h2>\n
!! Als Gegenstrahlung wird die Wärmestrahlung der Athmosphäre ins Wasser bezeichnet. Abhängig von der Lufttemperatur und der Bewölkung
!! wird dadurch Wärme ins Gewässer eingestrahlt (auch ohne Sonnenlicht).\n
!! \f[
!!  G = 0.97 \cdot (1.0 + wtyp * (\frac{cloud}{8})^{2.6}) \cdot 9.37*10^{-6} \cdot K_{StB} \cdot (T_l+273.16)^6
!! \f]
!!    mit:<ul>
!!      <li>\f$ G \f$  Gegenstrahlung; von der Luft ins Wasser; in (Kj/(h*m²)) * K² ???? </li>
!!      <li>\f$ wtyp \f$  Wolkentyp von 0.00 ... 0.25</li>
!!      <li>\f$ cloud \f$  Bedeckungsgrad in Achteln</li>
!!      <li>\f$ K_{StB} \f$  Stefan-Boltzmann-Konstante \f$= 2.0411 \cdot 10^{-7} \frac{KJ}{m^2 \cdot h \cdot K^4}\f$ \n
!!           oder auch \f$= (5.670373\pm0.000021) \cdot 10^{-8} \frac{W}{m^2K^4}\f$ \n
!!      <li>\f$ T_l \f$  Luft-Temperatur in Grad Celsius</li>
!!    </ul>\n\n
!! Als Ausstrahlung wird die Wärmestrahlung des Wassers nach oben bezeichnet. Diese ist abhängig von der Wassertemperatur.
!! \f[
!!  A = 0.97 \cdot K_{StB} \cdot (T_w + 273.16)^4
!! \f]
!!    mit:<ul>
!!      <li>\f$ A \f$  Ausstrahlung; in KiloJoule je Stunde und Quadratmeter Waseroberfläche </li>
!!      <li>\f$ T_w \f$ Wasser-Temperatur in Grad Celsius</li>
!!    </ul>\n\n
!! Die Strahlungssumme ergibt sich dann wie folgt:
!! \f[
!!  W_B = G-A
!! \f]
!!    mit:<ul>
!!      <li>\f$ W_B \f$  Wärmestromdichte aus Strahlung; in KiloJoule je Stunde und Quadratmeter Wasseroberfläche</li>
!!    </ul>\n\n
!! </li>
!! <li> VERDUNSTUNG
!! \f[
!!  W_V =
!! \f]
!! </li>
!! <li> KONVEKTION\n
!! \f[
!!  W_L =
!! \f]
!! </li>
!! <li>Sedimenttemperatur
!! \f[
!! T_{sed} =  \frac{\partial c_m}{\partial x_i}
!! \f]
!! </li>
!! </ol>
!! \n\n
!! <h2>lokale Temperaturänderung</h2>
!! Wird nun ein Massepartikelchen betrachtet, das sich in der Strömung bewegt (Lagrangesche Betrachtungsweise),\n
!! dann ergibt sich die Temperaturänderung des Wasserpartikelchens aus der Bilanz der Wärmeflüsse.\n
!! (In 2D-tiefengemittelten Berechnungen hat jede Vertikale eine konstante Temperatur,
!! daher werden auch nur Wärmeströme über die Oberfläche und die Sohle berücksichtigt
!! und keine Wärmetransporte in vertikaler Richtung)
!! \f[
!! DT =  \frac{ W_B - W_V - W_L }{ \rho_{20} \cdot c_v } \cdot \frac{\Delta t}{H}
!! \f]
!!    mit:<ul>
!!      <li>\f$ DT \f$  Lagrangesche Temperaturänderung im aktuellen Zeitschritt; in Kelvin </li>
!!      <li>\f$ \rho_{20}\f$ Dichte des Wassers bei 20 Grad Celsius, 1000 Kilogramm pro Kubikmeter</li>
!!      <li>\f$ c_v \f$ spezifische Wärmekapazität des Wassers, 4.187 KiloJoule je Kilogramm und Kelvin</li>
!!      <li>\f$ \Delta t \f$ Zeitschritt in Stunden </li>
!!      <li>\f$ H \f$ Wassertiefe in Meter</li>
!!    </ul>\n\n
!> \page kalterwind Konvektion ohne Verdunstung
!! An der Formel 11 aus der <a href="./pdf/Temperatur_Doku_Volker.pdf" target="_blank">Dokumentation Temperatur</a>
!! irritiert, dass ohne Verdunstung auch keine Wärmeleitung zwischen Luft und Wasser stattfinden könnte.\n\n
!! Zudem fällt auf, dass wenn Sättigungsdampfdruck des Wassers und Partialdampfdruck der Luft gleich sind,
!! es zu einer nicht abgefangenen Division durch Null kommen kann.\n
!! aus temperw.f90 : WL = ((TEMPW1-TEMPL(ior))/(1.53*(sddw-pdltt)))*WV\n\n
!! Quelle: temperw_huelle() zurück zu \ref klaerung
