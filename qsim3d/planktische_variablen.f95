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
!
!> \page planktische_variablen planktische, transportierte Variablen
!!
!! Bei diesen Variablen handelt es sich zumeist um Konzentrationen von gelösten Wasserinhaltsstoffen
!! oder um Güteparameter die als gleichmäßig im Volumen verteilt betrachtet werden können
!! und nur vernachlässigbare Relativbewegung im Wasser ausführen ;
!! z.B. Temperatur, Sauerstoff-Kohlenstoffverhältnis oder Anzahl der Rotatorien
!! \n\n
!! Diese Feldgrößen werden transportiert (advektiert und diffundiert). Siehe dazu \ref Stofftransport.
!! Dadurch werden sie auch in den nächsten Zeitschritt mitgenommen. (Im Gegensatz zu: \ref uebergabe_werte )
!! \n\n
!! Es definiert Felder für tiefengemittelte (planktonic_variable_p) \n
!! und vertikal aufgelöste Variablen (plankt_vari_vert, plankt_vari_vert_p).\n
!! mit dem Suffix _p werden diejenigen Felder bezeichnet, welche für die parallele Programmbearbeitung benötigten werden.
!! Sie sind auf allen (beim parallelen Rechnen zusammenwirkenden) Prozessen definiert (allokiert) und
!! enthalten nur jene Knoten, die von dem zugehörigen Prozezess bearbeitet werden.
!! Die Gesamtfelder (Variablennamen ohne _p), enthalten alle Knoten,
!! sind aber nur auf dem Prozess 0 angelegt (allokiert).
!! \n\n
!! Für die Parallelisierung mit MPI war es erforderlich, diese Felder einfach zu indizieren.
!! Diese Feldnummern werden bei jedem Zugriff aus Knotennummer, Variablennummer und ggf. Tiefenschichtnummer
!! explizit berechnet und nicht implizit aus der Mehrfachdimensionierung wie sonst in Fortran üblich.
!! \n\n
!! Den Zusammenhang zu den in QSim1D verwendeten Variablennamen wird in den nachstehenden Tabellen aufgelistet
!! und wird darüberhinaus auch vom Programm in den Feldern
!! modell::planktonic_variable_name und modell::plankt_vari_vert_name mitgeführt. Die Namen werden in ini_planktkon0() gesetzt.
!!
!! <h2> tiefengemittelte Konzentrationen </h2> \anchor tiefengemittelte_planktische_variable
!! Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::planktonic_variable und  modell::planktonic_variable_p\n
!! Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n
!!<table planktonic_variable>
!!<tr><th> Nr. QSim-3D </th><th> Name QSim-1D    </th><th> Beschreibung                         </th><th> Dimension   </th></tr>
!!<tr><td>  1 </td><td> \anchor tempw tempw   </td><td> Wasser-Temperatur                     </td><td> Grad Celsius   </td></tr>
!!<tr><td>  2 </td><td> \anchor vo2 vo2      </td><td> Sauerstoff                         </td><td> mg O2 / l   </td></tr>
!!<tr><td>  3 </td><td> \anchor vnh4 vnh4   </td><td> Ammonium-Sticktoff                     </td><td> mg NH4-N / l   </td></tr>
!!<tr><td>  4 </td><td> \anchor vno2 vno2   </td><td> Nitrit-Sticktoff                     </td><td> mg NO2-N / l   </td></tr>
!!<tr><td>  5 </td><td> \anchor vno3 vno3   </td><td> Nitrat-Sticktoff                     </td><td> mg NO3-N / l   </td></tr>
!!<tr><td>  6 </td><td> \anchor gelp gelp   </td><td> gelöster ortho-Phosphat-Phosphor               </td><td> mg PO4-P / l   </td></tr>
!!<tr><td>  7 </td><td> \anchor si si      </td><td> Silikat-Silizium                     </td><td> mg Si / l   </td></tr>
!!<tr><td>  8 </td><td> \anchor aki aki      </td><td> Kiesel-Algen \ref Biomasse               </td><td> mgBio/l    </td></tr>
!!<tr><td>  9 </td><td> \anchor agr agr      </td><td> Gruen-Algen \ref Biomasse                   </td><td> mgBio/l    </td></tr>
!!<tr><td> 10 </td><td> \anchor abl abl      </td><td> Blau-Algen \ref Biomasse                  </td><td> mgBio/l    </td></tr>
!!<tr><td> 11 </td><td> \anchor chla chla   </td><td> Gesamt Chlorophyll-a                     </td><td> µgChla/l    </td></tr>
!!<tr><td> 12 </td><td> \anchor chlaki chlaki   </td><td> Chlorophyll-a Kiesela.                     </td><td> µgChla/l    </td></tr>
!!<tr><td> 13 </td><td> \anchor chlagr chlagr   </td><td> Chlorophyll-a Grüna.                     </td><td> µgChla/l    </td></tr>
!!<tr><td> 14 </td><td> \anchor chlabl chlabl   </td><td> Chlorophyll-a Blaua.                     </td><td> µgChla/l    </td></tr>
!!
!!<tr><td> 15 </td><td> \anchor vx0 vx0      </td><td> Biomasse der Nitrosomonas                  </td><td> mgBio/l   </td></tr>
!!<tr><td> 16 </td><td> \anchor vx02 vx02   </td><td> Biomasse der Nitrobacter                  </td><td> mgBio/l   </td></tr>
!!<tr><td> 17 </td><td> \anchor obsb obsb   </td><td> C-BSB5 ohne lebende Organismen, biologischer Sauerstoffbedarf in 5 Tage der organischen Kohlenstoffverbindungen</td><td> mgO2/l   </td></tr>
!!<tr><td> 18 </td><td> \anchor ocsb ocsb   </td><td> C-CSB, chemischer Sauerstoffbedarf der organischen Kohlenstoffverbindungen         </td><td> mgO2/l   </td></tr>
!!<tr><td> 19 </td><td> \anchor vkigr vkigr   </td><td> Anteil Kiesela. an Gesamtalgenmasse               </td><td> -      </td></tr>
!!<tr><td> 20 </td><td> \anchor antbl antbl   </td><td> Anteil Blau an Gesamtalgenmasse               </td><td> -      </td></tr>
!!<tr><td> 21 </td><td> \anchor svhemk svhemk   </td><td> Lichtinhibition Kieselalgenwachstum                </td><td>      </td></tr>
!!<tr><td> 22 </td><td> \anchor svhemg svhemg   </td><td> Lichtinhibition Grünalgenwachstum               </td><td>      </td></tr>
!!<tr><td> 23 </td><td> \anchor svhemb svhemb   </td><td> Lichtinhibition Blaualgenwachstum               </td><td>      </td></tr>
!!<tr><td> 24 </td><td> \anchor akbcm akbcm   </td><td> Kohlenstoff zu Chlorophyll-a Verhältnis in Kiesel-Algen      </td><td> mg C/ mg Chla   </td></tr>
!!<tr><td> 25 </td><td> \anchor agbcm agbcm   </td><td> Kohlenstoff zu Chlorophyll-a Verhältnis in Grün-Algen         </td><td> mg C/ mg Chla   </td></tr>
!!<tr><td> 26 </td><td> \anchor abbcm abbcm   </td><td> Kohlenstoff zu Chlorophyll-a Verhältnis in Blau-Algen         </td><td> mg C/ mg Chla   </td></tr>
!!<tr><td> 27 </td><td> \anchor akiiv akiiv   </td><td> entallen                        </td><td>       </td></tr>
!!<tr><td> 28 </td><td> \anchor agriv agriv   </td><td> entallen                        </td><td>       </td></tr>
!!<tr><td> 29 </td><td> \anchor abliv abliv   </td><td> entallen                        </td><td>      </td></tr>
!!<tr><td> 30 </td><td> \anchor q_nk q_nk   </td><td> Stickstoffanteil der Algenbiomasse kiesel            </td><td> mg N /mg bio   </td></tr>
!!<tr><td> 31 </td><td> \anchor q_pk q_pk   </td><td> Phosphoranteil der Algenbiomasse kiesel            </td><td> mg P /mg bio   </td></tr>
!!<tr><td> 32 </td><td> \anchor q_sk q_sk   </td><td> Siliziumanteil der Algenbiomasse kiesel            </td><td> mg Si/mg bio   </td></tr>
!!<tr><td> 33 </td><td> \anchor q_ng q_ng   </td><td> Stickstoffanteil der Algenbiomasse gruen            </td><td> mg N /mg bio   </td></tr>
!!<tr><td> 34 </td><td> \anchor q_pg q_pg   </td><td> Phosphoranteil der Algenbiomasse gruen            </td><td> mg P /mg bio   </td></tr>
!!<tr><td> 35 </td><td> \anchor q_nb q_nb   </td><td> Stickstoffanteil der Algenbiomasse blau            </td><td> mg N /mg bio   </td></tr>
!!<tr><td> 36 </td><td> \anchor q_pb q_pb   </td><td> Phosphoranteil der Algenbiomasse blau               </td><td> mg P /mg bio   </td></tr>
!!<tr><td> 37 </td><td> \anchor cd1 cd1      </td><td> leicht abbaubare gelöste organische C-Verbindungen         </td><td> mg C / l   </td></tr>
!!<tr><td> 38 </td><td> \anchor cd2 cd2      </td><td> schwer abbaubare gelöste organische C-Verbindungen         </td><td> mg C / l   </td></tr>
!!<tr><td> 39 </td><td> \anchor cp1 cp1      </td><td> leicht abbaubare partikuläre organische C-Verbindungen      </td><td> mg C / l   </td></tr>
!!<tr><td> 40 </td><td> \anchor cp2 cp2      </td><td> schwer abbaubare partikuläre organische C-Verbindungen      </td><td> mg C / l   </td></tr>
!!<tr><td> 41 </td><td> \anchor cm cm      </td><td> monomolekularen organischen C-Verbindungen            </td><td> mg C / l   </td></tr>
!!<tr><td> 42 </td><td> \anchor bac bac      </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen   </td><td> mg C / l   </td></tr>
!!<tr><td> 43 </td><td> \anchor o2bsb o2bsb   </td><td> Sauerstoff-Kohlenstoffverhältnis beim C-Abbau            </td><td> mgO2/mgC   </td></tr>
!!<tr><td> 44 </td><td> \anchor bl01 bl01   </td><td> schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent    </td><td> mg O2 / l    </td></tr>
!!<tr><td> 45 </td><td> \anchor bl02 bl02   </td><td> leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent    </td><td> mg O2 / l    </td></tr>
!!<tr><td> 46 </td><td> \anchor vbsb vbsb   </td><td> C-BSB5 incl. lebender Organismen, messbar, Randwert (BSB5 ohne Sauerstoffbedarf Nitrifikation)</td><td> mg O2 / l   </td></tr>
!!<tr><td> 47 </td><td> \anchor vcsb vcsb   </td><td> C-CSB, messbar, Randwert         </td><td> mg O2 / l   </td></tr>
!!<tr><td> 48 </td><td> \anchor chnf chnf   </td><td> C-Masse der heterotrophen Nanoflagelaten             </td><td> mg C / l   </td></tr>
!!<tr><td> 49 </td><td> \anchor bvhnf bvhnf   </td><td> Biovolumen der HNF ??                     </td><td> µm3 ??   </td></tr>
!!<tr><td> 50 </td><td> \anchor zooind zooind   </td><td> Anzahl der Rotatorien                     </td><td> Ind/l      </td></tr>
!!<tr><td> 51 </td><td> \anchor abrzo abrzo1   </td><td> leer ?                        </td><td>      </td></tr>
!!<tr><td> 52 </td><td> \anchor ssalg ssalg   </td><td> GESAMTSCHWEBSTOFFE = ss+Algen+Rotatorien             </td><td> mg/l      </td></tr>
!!<tr><td> 53 </td><td> \anchor ss ss      </td><td> nicht lebender Schwebstoffgehalt                </td><td> mg/l      </td></tr>
!!<tr><td> 54 </td><td> \anchor fssgr fssgr   </td><td> ??? zu schweb()                          </td><td>      </td></tr>
!!<tr><td> 55 </td><td> \anchor fbsgr fbsgr   </td><td> Faktor zur Berechnung der ablagerungsfreien Grenzkonzentration von zehrungsfähigem, organischem Material ???? siehe: \ref Sediorgc </td><td> - </td></tr>
!!<tr><td> 56 </td><td> \anchor frfgr frfgr   </td><td> Faktor zur Berechnung der ablagerungsfreien Grenzkonzentration von refraktärem, organischem Material ???? siehe: \ref Sediorgc</td><td> - </td></tr>
!!<tr><td> 57 </td><td> \anchor nl0 nl0      </td><td> N/C Verhältnis von Stickstoff zu Kohlenstoff in organischem Material   </td><td> mg N / mg C   </td></tr>
!!<tr><td> 58 </td><td> \anchor pl0 pl0      </td><td> P/C Verhältnis von Phosphor zu Kohlenstoff in organischem Material   </td><td> mg P / mg C   </td></tr>
!!<tr><td> 59 </td><td> \anchor stind stind   </td><td> ph() ???? Minutenzähler; Versuch einer Altersvariablen ?      </td><td>      </td></tr>
!!<tr><td> 60 </td><td> \anchor dlarvn dlarvn   </td><td> Anzahl der Dreissena-Larven                   </td><td> Ind/l    </td></tr>
!!<tr><td> 61 </td><td> \anchor coli coli   </td><td> Fäkalcoliforme Bakterien                  </td><td> Ind/100 ml   </td></tr>
!!<tr><td> 62 </td><td> \anchor mw mw      </td><td> Säurekapazität (m-Wert)                   </td><td> mmol/l   </td></tr>
!!<tr><td> 63 </td><td> \anchor pw pw      </td><td> Basenkapazität (p-wert)                  </td><td> mmol/l   </td></tr>
!!<tr><td> 64 </td><td> \anchor ca ca      </td><td> Calciumkonzentration                     </td><td> mg Ca /l   </td></tr>
!!<tr><td> 65 </td><td> \anchor lf lf      </td><td> Leitfähigkeit                           </td><td> µS/cm      </td></tr>
!!<tr><td> 66 </td><td> \anchor vph vph    </td><td> pH-Wert                              </td><td>   -      </td></tr>
!!<tr><td> 67 </td><td> \anchor gesn gesn   </td><td> Gesamt-Stickstoff                     </td><td> mg N / l   </td></tr>
!!<tr><td> 68 </td><td> \anchor gesp gesp   </td><td> Gesamt-Phosphor                      </td><td> mg PO4-P / l   </td></tr>
!!<tr><td> 69 </td><td> \anchor skmor skmor   </td><td> Silizium in schwebenden, abgestorbenen Kieselalgen         </td><td>      </td></tr>
!!<tr><td> 70 </td><td> \anchor doscf doscf   </td><td> zu coliform() ?                     </td><td>      </td></tr>
!!<tr><td> 71 </td><td> \anchor tracer tracer    </td><td> passiver Tracer (nur QSim3D)                  </td><td> -      </td></tr>
!!<tr><td> 72 </td><td> \anchor salz salz      </td><td> Salzgehalt (nur QSim3D)                  </td><td> Psu       </td></tr>
!!<tr><td> 73 </td><td> \anchor alter_decay alter_decay   </td><td> Altersvariable Zerfall  (nur QSim3D)            </td><td> d      </td></tr>
!!<tr><td> 74 </td><td> \anchor alter_arith alter_arith   </td><td> Altersvariable arithmetisches Mittel  (nur QSim3D)      </td><td> d      </td></tr>
!!<tr><td> 75 </td><td> \anchor alter_growth alter_growth </td><td> Altersvariable Wachstum  (nur QSim3D)         </td><td> d      </td></tr>
!!<tr><td> 76 </td><td> \anchor tgzoo tgzoo   </td><td> Gewicht einer einzelnen Rotatorie (Zooplankton) (noch nicht aktiv)   </td><td> µg      </td></tr>
!!<tr><td> 77 </td><td> \anchor akmor_1 akmor_1   </td><td> max.?? Algenmortalität??   Kiesel</td><td> ?      </td></tr>
!!<tr><td> 78 </td><td> \anchor agmor_1 agmor_1   </td><td> ???? Grün </td><td> ?      </td></tr>
!!<tr><td> 79 </td><td> \anchor abmor_1 abmor_1   </td><td> ???? Blau </td><td> ?      </td></tr>
!!<tr><td> 80 </td><td> \anchor hgsZn hgsZn </td><td> Zink gesamt       </td><td> µg/l </td></tr>
!!<tr><td> 81 </td><td> \anchor hglZn hglZn </td><td> Zink gelöst       </td><td> µg/l </td></tr>
!!<tr><td> 82 </td><td> \anchor hgsCad hgsCad </td><td> Cadmium gesamt    </td><td> µg/l </td></tr>
!!<tr><td> 83 </td><td> \anchor hglCad hglCad </td><td> Cadmium gelöst    </td><td> µg/l </td></tr>
!!<tr><td> 84 </td><td> \anchor hgsCu hgsCu </td><td> Kupfer gesamt    </td><td> µg/l </td></tr>
!!<tr><td> 85 </td><td> \anchor hglCu hglCu </td><td> Kupfer gelöst    </td><td> µg/l </td></tr>
!!<tr><td> 86 </td><td> \anchor hgsNi hgsNi </td><td> Nickel gesamt    </td><td> µg/l </td></tr>
!!<tr><td> 87 </td><td> \anchor hglNi hglNi </td><td> Nickel gelöst    </td><td> µg/l </td></tr>
!!<tr><td> 88 </td><td> \anchor hgsAs hgsAs </td><td> Arsen gesamt       </td><td> µg/l </td></tr>
!!<tr><td> 89 </td><td> \anchor hglAs hglAs </td><td> Arsen gelöst       </td><td> µg/l </td></tr>
!!<tr><td> 90 </td><td> \anchor hgsPb hgsPb </td><td> Blei gesamt       </td><td> µg/l </td></tr>
!!<tr><td> 91 </td><td> \anchor hglPb hglPb </td><td> Blei gelöst       </td><td> µg/l </td></tr>
!!<tr><td> 92 </td><td> \anchor hgsCr hgsCr </td><td> Chrom gesamt       </td><td> µg/l </td></tr>
!!<tr><td> 93 </td><td> \anchor hglCr hglCr </td><td> Chrom gelöst       </td><td> µg/l </td></tr>
!!<tr><td> 94 </td><td> \anchor hgsFe hgsFe </td><td> Eisen gesamt       </td><td> µg/l </td></tr>
!!<tr><td> 95 </td><td> \anchor hglFe hglFe </td><td> Eisen gelöst       </td><td> µg/l </td></tr>
!!<tr><td> 96 </td><td> \anchor hgsHg hgsHg </td><td> Quecksilber gesamt</td><td> µg/l </td></tr>
!!<tr><td> 97 </td><td> \anchor hglHg hglHg </td><td> Quecksilber gelöst</td><td> µg/l </td></tr>
!!<tr><td> 98 </td><td> \anchor hgsMn hgsMn </td><td> Mangan gesamt    </td><td> µg/l </td></tr>
!!<tr><td> 99 </td><td> \anchor hglMn hglMn </td><td> Mangan gelöst    </td><td> µg/l </td></tr>
!!<tr><td>100 </td><td> \anchor hgsU hgsU     </td><td> Uran gesamt       </td><td> µg/l </td></tr>
!!<tr><td>101 </td><td> \anchor hglU hglU     </td><td> Uran gelöst       </td><td> µg/l </td></tr>
!!</table planktonic_variable>
!!
!! * <b> \anchor Biomasse Biomasse </b> \n
!! Der Begriff "Biomasse" wird möglicherweise nur in Bezug auf QSim in dieser Form verwendet. Eine Begriffsklärung ist in Arbeit (April2021).
!! Er wird vorläufig beibehalten, weil sich andere Größen (Nährstoffgehalte, Schwebstoffmassen) darauf beziehen.
!!
!! \n\n
!! <i>Transport eigentlich unnötig: fbsgr, frfgr</i>
!! \n\n
!! <h2> tiefenaufgelöste Konzentrationen </h2> \anchor tiefenaufgelöste_planktische_variable \n
!! Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::plankt_vari_vert und modell::plankt_vari_vert_p\n
!! Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n
!!<table plankt_vari_vert>
!!<tr><th> Nr.</th><th> Name         </th><th> Beschreibung          </th><th> Dimension   </th></tr>
!!<tr><td>  1 </td><td> \anchor tempwz tempwz   </td><td> Wasser-Temperatur      </td><td> Grad Celsius    </td></tr>
!!<tr><td>  2 </td><td> \anchor vo2z vo2z   </td><td> Sauerstoff          </td><td> mg O2 / l   </td></tr>
!!<tr><td>  3 </td><td> \anchor vnh4z vnh4z   </td><td> Ammonium-Stickstoff       </td><td> mg NH4-N / l   </td></tr>
!!<tr><td>  4 </td><td> \anchor vno2z vno2z   </td><td> Nitrit-Stickstoff      </td><td> mg NO2-N / l   </td></tr>
!!<tr><td>  5 </td><td> \anchor vno3z vno3z   </td><td> Nitrat-Stickstoff      </td><td> mg NO3-N / l   </td></tr>
!!<tr><td>  6 </td><td> \anchor gelpz gelpz   </td><td> gelöster Phosphor      </td><td> mg PO4-P / l   </td></tr>
!!<tr><td>  7 </td><td> \anchor siz siz      </td><td> Silikat-Silizium      </td><td> mg Si / l   </td></tr>
!!<tr><td>  8 </td><td> \anchor akiz akiz   </td><td> Biomasse Kiesel-Algen      </td><td> mgBio/l   </td></tr>
!!<tr><td>  9 </td><td> \anchor agrz agrz   </td><td> Biomasse Gruen-Algen      </td><td> mgBio/l   </td></tr>
!!<tr><td> 10 </td><td> \anchor ablz ablz   </td><td> Biomasse Blau-Algen      </td><td> mgBio/l   </td></tr>
!!<tr><td> 11 </td><td> \anchor chlaz chlaz   </td><td> Chlorophyll-a         </td><td> µgChla/l   </td></tr>
!!<tr><td> 12 </td><td> \anchor hchlkz hchlkz   </td><td> Chlorophyll in Kieselalgen   </td><td> µgChla/l    </td></tr>
!!<tr><td> 13 </td><td> \anchor hchlgz hchlgz   </td><td> Chlorophyll in Gruenalgen   </td><td> µgChla/l    </td></tr>
!!<tr><td> 14 </td><td> \anchor hchlbz hchlbz   </td><td> Chlorophyll in Blaualgen   </td><td> µgChla/l    </td></tr>
!!<tr><td> 15 </td><td> \anchor hgespz hgespz   </td><td> gesamt Phosphor    </td><td>     </td></tr>
!!<tr><td> 16 </td><td> \anchor hgesnz hgesnz   </td><td> gesamt Stickstoff   </td><td>     </td></tr>
!!<tr><td> 17 </td><td> \anchor hq_nkz hq_nkz   </td><td> Stickstoffanteil der Algenbiomasse kiesel    </td><td> mg N /mg bio   </td></tr>
!!<tr><td> 18 </td><td> \anchor hq_ngz hq_ngz   </td><td> Stickstoffanteil der Algenbiomasse grün    </td><td> mg N /mg bio   </td></tr>
!!<tr><td> 19 </td><td> \anchor hq_nbz hq_nbz   </td><td> Stickstoffanteil der Algenbiomasse blau    </td><td> mg N /mg bio   </td></tr>
!!<tr><td> 20 </td><td> \anchor hcchlkz hcchlkz   </td><td> c-chla Verhältnis Kiesel entspricht \ref akbcm</td><td> mg C/ mg Chla   </td></tr>
!!<tr><td> 21 </td><td> \anchor hcchlgz hcchlgz   </td><td> c-chla Verhältnis grün entspricht \ref agbcm   </td><td> mg C/ mg Chla   </td></tr>
!!<tr><td> 22 </td><td> \anchor hcchlbz hcchlbz   </td><td> c-chla Verhältnis blau entspricht \ref abbcm   </td><td> mg C/ mg Chla   </td></tr>
!!</table plankt_vari_vert>\n\n
!!<code>\verbatim
!!       subroutine transportz(anze,deltat,izeits,isub_dt,hvmitt,elen,flag &
!!      &,\ref tempwz,\ref vnh4z,\ref vno2z,\ref vno3z,\ref vo2z,\ref gelpz,\ref siz,\ref akiz,\ref agrz                &
!!      &,ablz,chlaz,nkzs,dh2d,i2ds,iwsim,mstr,r1,r2,r3                    &
!!      &,htempz,ho2z,hnh4z,hno2z,hno3z,hgelpz,hsiz,hakiz,hagrz            &
!!      &,hablz,hchlaz,iflri,dl,imac,uvert,tflie,jpoin1,itags,monats       &
!!      &,iwied,uhrz,iverfahren)
!!
!!       call advdiff(anze,elen,vmitt,uvert,dl,flag,ktrans,u,temp0,tempn   &
!!      &,deltat,sumdet,itime,izeits,mstr,iwied,iwahld,nkz,nkzs,tflie,iflri&
!!      &,jpoin1,itags,monats,isub_dt,imac,iverfahren,uhrz)
!! \endverbatim</code>
!! \n\n
!! aus planktische_variablen.f95; zurück zu \ref Stofftransport oder \ref lnk_Datentechnik
!----+-----+----
!> Anlegen der Datenfelder für die Prozesse > 0\n
!! und anschließend verteilen \n\n
subroutine planktkon_parallel()
   use modell
   implicit none
   integer as,j,k,i,iloka
   !print*,meinrang, ' planktkon_parallel starting'
   if ((meinrang == 0) .and. (kontrollknoten > 0))print*,'0 planktkon_parallel starting GlMn = (',kontrollknoten,') = '   &
       ,planktonic_variable(99+(kontrollknoten-1)*number_plankt_vari)
   ! depth averaged
   allocate (planktonic_variable_p(number_plankt_vari*part), stat = as )
   if (as /= 0) then
      write(fehler,*)' return value allocate planktonic_variable_p :', as
      call qerror(fehler)
   end if
   do k = 1,part ! i
      do j = 1,number_plankt_vari ! initialise all concentrations to -1
         planktonic_variable_p(j+(k-1)*number_plankt_vari) = -2.0
      end do
   end do
   !planktonic_variable_p(:)=-2.0
   ! vertical profiles i.e. full 3D
   allocate (plankt_vari_vert_p(num_lev*number_plankt_vari_vert*part), stat = as )
   if (as /= 0) then
      write(fehler,*)' return value  plankt_vari_vert_p :', as
      call qerror(fehler)
   end if
   do k = 1,part
      do j = 1,number_plankt_vari_vert !
         do i = 1,num_lev ! initialise all concentrations to -1
            plankt_vari_vert_p(i+(j-1)*num_lev+(k-1)*number_plankt_vari_vert*num_lev) = -1.0
         end do ! all i levels
      end do !all j variables
   end do ! all k nodes in subdomain
   !call mpi_barrier (mpi_komm_welt, ierr)
   call scatter_planktkon()
   call mpi_barrier (mpi_komm_welt, ierr)
   
   if (kontrollknoten > 0) then
      iloka = kontrollknoten-(meinrang*part)
      if ((iloka > 0) .and. (iloka <= part))print*,meinrang,part,iloka,kontrollknoten,number_plankt_vari,  &
          'planktkon_parallel finish GlMn_p = ',planktonic_variable_p(99+(iloka-1)*number_plankt_vari)
      if (meinrang == 0)print*,'0 planktkon_parallel finish GlMn = (',kontrollknoten,') = '   &
          ,planktonic_variable(99+(kontrollknoten-1)*number_plankt_vari)
   endif ! kontrollknoten
   return
end subroutine planktkon_parallel
!----+-----+----
!> Verteilen der transportierten Konzentrationen auf die parallelen Prozesse.
!! \n\n
subroutine scatter_planktkon()
   use modell
   implicit none
   !print*,'scatter_planktkon part,number_plankt_vari,meinrang=',part, number_plankt_vari, meinrang
   call MPI_Scatter(planktonic_variable, part*number_plankt_vari, MPI_FLOAT,  &
                    planktonic_variable_p, part*number_plankt_vari, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 13 MPI_Scatter(planktonic_variable failed :', ierr
      call qerror(fehler)
   end if
   call MPI_Scatter(plankt_vari_vert, part*number_plankt_vari_vert*num_lev, MPI_FLOAT,  &
                    plankt_vari_vert_p, part*number_plankt_vari_vert*num_lev, MPI_FLOAT, 0,mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 14 MPI_Scatter(plankt_vari_vert failed :', ierr
      call qerror(fehler)
   end if
   !print*,meinrang, ' scatter_planktkon finish'
   return
end subroutine scatter_planktkon
!----+-----+----
!> wieder zusammensammeln der transportierten Konzentrationen von den parallelen Prozesse.
!! \n\n
subroutine gather_planktkon()
   use modell
   implicit none
   !print*,'gather_planktkon'
   call MPI_Gather(planktonic_variable_p, part*number_plankt_vari, MPI_FLOAT,  &
                   planktonic_variable, part*number_plankt_vari, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 15 MPI_Gather(planktonic_variable failed :', ierr
      call qerror(fehler)
   end if
   call MPI_Gather(plankt_vari_vert_p, part*number_plankt_vari_vert*num_lev, MPI_FLOAT,  &
                   plankt_vari_vert, part*number_plankt_vari_vert*num_lev, MPI_FLOAT, 0,mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 16 MPI_Gather(plankt_vari_vert failed :', ierr
      call qerror(fehler)
   end if
   
end subroutine gather_planktkon
!----+-----+----
!> Initialisierung der transportierten Konzentrationen auf 0.0.
!! \n\n
subroutine ini_planktkon0(nk)
   use modell
   implicit none
   integer nk,k,n,as,j,l,ini
   if (meinrang == 0) then ! prozess 0 only
      print*,'ini_planktkon0'
      number_plankt_point = nk
      ! number_plankt_vari= s. o.
      !------- tiefengemittelte planktische variablen
      do j = 1,number_plankt_vari ! initialise
         write(planktonic_variable_name(j),'(18x)')
      end do
      include "planktonic_variable_name.h"
      
      do j = 1,number_plankt_vari ! zunächst nix ausgeben
         output_plankt(j) = .false.
      end do
      !!!!!!!!! allocate and initialize planktonic_variable
      print*,"ini_planktkon0 going to: allocate (planktonic_variable( "  &
      ,"part*proz_anz,part,proz_anz,number_plankt_point,number_plankt_vari = " &
      ,part*proz_anz,part,proz_anz,number_plankt_point,number_plankt_vari
      allocate (planktonic_variable(number_plankt_vari*part*proz_anz), stat = as )
      !allocate (planktonic_variable(number_plankt_vari*number_plankt_point), stat = as )
      if (as /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate planktonic_variable_ :', as
         call qerror(fehler)
      else
         print*,'planktonic_variable allocated to array size = ',  &
                                                               size(planktonic_variable)
      end if
      do k = 1,number_plankt_point ! i
         do j = 1,number_plankt_vari ! initialisierung aller konzentrationen zunächt auf Null
            planktonic_variable(j+(k-1)*number_plankt_vari) = 0.0 !!!####! 0.0
            !planktonic_variable(71+(k-1)*number_plankt_vari) = real(knoten_zone(k))  !  tracer test annu ####
            !planktonic_variable(72+(k-1)*number_plankt_vari) = 10*real(knoten_zone(k))  !  salz test annu ####
         end do
      end do
      ! ------- tiefenaufgelöst, planktonic variables
      do j = 1,number_plankt_vari_vert ! initialise
         write(plankt_vari_vert_name(j),'(18x)')
      end do
      include "plankt_vari_vert_name.h"
      ! allocate and initialize plankt_vari_vert
      allocate (plankt_vari_vert(num_lev*number_plankt_vari_vert*part*proz_anz), stat = as )
      !allocate (plankt_vari_vert(num_lev*number_plankt_vari_vert*number_plankt_point), stat = as )
      if (as /= 0) then
         write(fehler,*)' Rueckgabewert   von   plankt_vari_vert :', as
         call qerror(fehler)
      end if
      do k = 1,number_plankt_point ! initialisierung aller konzentrationen zunächt auf Null
         do j = 1,number_plankt_vari_vert !
            do l = 1,num_lev
               plankt_vari_vert(l+(j-1)*num_lev+(k-1)*number_plankt_vari_vert*num_lev) = 0.0 !!!####! 0.0
               !plankt_vari_vert(i,j,k)=0.0
               !plankt_vari_vert(k)%level(j)%value(i)=0.0
            end do ! i alle
         end do ! j alle levels
      end do
      do j = 1,number_plankt_vari_vert ! zunächst nix ausgeben
         output_plankt_vert(j) = .false.
      end do
      allocate (point_zone(number_plankt_point), stat = as )
      if (as /= 0) then
         print*,' allocate failed in zonen_parallel point_zone :', as
         call qerror(fehler)
      end if
      select case (hydro_trieb)
         case(1) ! casu-transinfo
            do ini = 1,number_plankt_point
               point_zone(ini) = knoten_zone(ini)
            end do
         case(2) ! Untrim² netCDF
            do ini = 1,number_plankt_point
               point_zone(ini) = element_zone(ini)
            end do
         case(3) ! SCHISM netCDF (doch noch von zone.gr3)
            do ini = 1,number_plankt_point
               point_zone(ini) = knoten_zone(ini)
            end do
            !call qerror('ini_planktkon0: SCHISM zone not yet worked out')
            case default
            call qerror('ini_planktkon0: unknown hydraulic driver')
      end select
      
   end if !! nur prozessor 0
end subroutine ini_planktkon0
