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
!> \page Silizium Silizium_Vorläuferversion
!! \n
!! <center>
!! \image html Haeckel_Diatomea_klein.jpg "Zeichnung Diatomeen, aus Ernst Haeckel's ''Kunstformen der Natur'' (1904)"
!! <a href="img/Haeckel_Diatomea.jpg">Bild groß</a> \n
!! Quelle: https://commons.wikimedia.org/wiki/File:Haeckel_Diatomea.jpg Heruntergeladen 12. Nov. 2019
!! </center>
!! \n
!! <h2>Herkunft</h2>
!!     silikat() \n
!!     EIN PROGRAMM zu Berechnung des geloesten Silikats    \n
!!     AUTOR : VOLKER KIRCHESCH                      \n
!!     entnommen aus Version qsim13.301_28mae18\n
!!
!! <h2>Teilprozesse</h2>
!! Silizium wird von Kieselalgen, lat.: Diatomeen, zum Aufbau ihrer Zellgerüste (Frustule) benötigt. Es ist nur als im
!! Wasser gelöstes Silikat bioverfügbar. Folgende Teilprozesse wirken sich auf den Silikatgehalt des Wassers aus:
!! \n
!! <ul>
!! <li> Rücklösung aus dem Sediment </li>
!! <li> Rücklösung aus schwebendem Detritus (noch Unklarheiten)</li>
!! <li> Verbrauch durch Wachstum planktischer Kiesel-Algen </li>
!! <li> Verbrauch durch Wachstum bentischer Kiesel-Algen </li>
!! </ul>
!!
!! <h2>Schnittstellenbeschreibung</h2>
!! SUBROUTINE silikat()\n
!! ( \ref si, \ref flag, \ref elen, \ref ior, *esi*, *qeinl*, \ref vabfl, \ref anze, \ref tflie, \ref jiein, \ref aki         &\n
!! , \ref albewk, \ref alberk, \ref tiefe, \ref tempw, \ref ilbuhn, \ref akkssi, \ref qmx_sk, \ref q_sk          &\n
!! , \ref up_siz, \ref siz, \ref algakz, \ref akitbr, \ref akibrz, \ref hjsi, \ref nkzs, \ref dh2d, \ref dh2de, \ref mstr    &\n
!! , \ref iorla, \ref iorle, \ref ieinls, \ref flae, *qeinll*, *sil*, \ref itags, \ref uhrz, \ref azstrs         &\n
!! , \ref kontroll , \ref iglob )    \n
!! \n
!! silikat() wird von der Hüllroutine silikat_huelle() aufgerufen. Zum Hüllroutinen-Konzept siehe: \ref lnk_huellen
!!
!! <h2>Dokumentation</h2>
!! Bisher existiert eine Dokumentation des Silikat-Moduls als Kapitel 12 der
!! <a href="./pdf/QSimDoku_ncycWy.pdf" target="_blank">Kurzdoku</a> (Version vom 22. Nov. 2017)
!! \n\n
!! zurück: \ref lnk_ueberblick ; Quelle: silikat_huelle.f95
!!
!! <h1>Schnittstellenbeschreibung</h1>
!!
!!<table >
!!<tr><th>     Variablen-Name QSim    </th><th> Daten-Feld T-QSim   </th><th> Beschreibung </th></tr>
!!<tr><td> si(1) </td><td> planktische_variablen::planktonic_variable_p ( 7+nk)  </td><td> silikat-Silizium-Konzentration (tiefengemittelt) </td></tr>
!!<tr><td> flag(1)</td><td>0         </td><td> keine Einleitungen </td></tr>
!!<tr><td> elen(1) </td><td> 1         </td><td> Elementlänge (nicht verwendet) </td></tr>
!!<tr><td> ior</td><td>1             </td><td> Laufindex </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> esi(1) </td><td> 0.0       </td><td> keine Einleitung </td></tr>
!!<tr><td> qeinl(1)</td><td> 0.0      </td><td> kein Abfluss Einleitung </td></tr>
!!<tr><td> vabfl(1) </td><td> 2.5     </td><td> wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet. </td></tr>
!!<tr><td> anze</td><td>1            </td><td> Anzahl der Profile im aktuellen Strang </td></tr>
!!<tr><td> tflie </td><td> real(deltat)/86400 </td><td> Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim) </td></tr>
!!<tr><td> jiein(1)</td><td>0        </td><td> null Punkt-Einleitungen </td></tr>
!!<tr><td> aki(1) </td><td> planktische_variablen::planktonic_variable_p (8+(i-1)*number_plankt_vari) </td><td> Anteil kiesel-Algen (wird nicht verwendet) </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> albewk(1)</td><td>benthische_verteilungen::benthic_distribution_p (14+(i-1)*number_benth_distr) </td><td> Wachstum benthischer kiesel-Algen </td></tr>
!!<tr><td> alberk(1)</td><td>benthische_verteilungen::benthic_distribution_p (12+(i-1)*number_benth_distr) </td><td> Respiration benthischer kiesel-Algen </td></tr>
!!<tr><td> tiefe(1) </td><td>  rb_hydraul_p (2+(i-1)*number_rb_hydraul) </td><td> Wassertiefe aus  randbedingungen </td></tr>
!!<tr><td> tempw(1) </td><td> planktische_variablen::planktonic_variable_p (1+nk)  </td><td> Wassertemperatur
!!<tr><td> sised(1) </td><td>benthische_verteilungen::benthic_distribution_p (2+(i-1)*number_benth_distr) </td><td> Siliziumgehalt im Sediment </td></tr>
!!<tr><td> ilbuhn</td><td>0          </td><td> keine Buhnen </td></tr>
!!<tr><td> akkssi</td><td> in QSimDatenfelder </td><td> unbenutzt </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> Qmx_SK</td><td> in QSimDatenfelder </td><td> max. Siliziumanteil der Kiesel-Algenbiomasse aus APARAM.txt</td></tr>
!!<tr><td> Q_SK(1) </td><td> planktische_variablen::planktonic_variable_p (32+nk)  </td><td> Siliziumgehalt Kieselalgen</td></tr>
!!<tr><td>    up_Siz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)</td></tr>
!!<tr><td>    siz(j,1) </td><td> planktische_variablen::plankt_vari_vert_p (j+(7-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) </td><td> silikat(7)-Silizium-Konzentration (tiefenaufgelöst)</td></tr>
!!<tr><td>    dalgkz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> ??</td></tr>
!!<tr><td> akitbr(1) </td><td> uebergabe_werte::transfer_quantity_p (48+(i-1)*number_trans_quant) </td><td> Kieselalgen ??
!!<tr><td>    akibrz(j,1) </td><td> uebergabe_werte::trans_quant_vert_p (j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> ??</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> SiRuek(1) </td><td>benthische_verteilungen::benthic_distribution_p (25+(i-1)*number_benth_distr) </td><td> Siliziumgehalt im Sediment</td></tr>
!!<tr><td> Skmor(1) </td><td> planktische_variablen::planktonic_variable_p (69+nk)  </td><td> Silizium in schwebenden, abgestorbenen Kieselalgen</td></tr>
!!<tr><td> nkzs(1)</td><td>1         </td><td> nur eine Tiefenschicht</td></tr>
!!<tr><td> dH2D</td><td>-2.0         </td><td> bisher nur 2D-Tiefengemittelt ??? konstante Tiefenschichtung ???</td></tr>
!!<tr><td> dH2De </td><td>0.0   </td><td> unbenutzt</td></tr>
!!<tr><td> mstr</td><td>1            </td><td> Strangzähler</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> iorLa(1)</td><td>0              </td><td> AnfangsKnoten der Linienquelle; nicht verwendet</td></tr>
!!<tr><td> iorLe(1)</td><td>0              </td><td> EndKnoten der Linienquelle; nicht verwendet</td></tr>
!!<tr><td> ieinLs(1)</td><td>0             </td><td> keine Linienquellen</td></tr>
!!<tr><td> flae(1) </td><td> 0.0           </td><td> unbenutzt da keine Einleitung</td></tr>
!!<tr><td> qeinlL(1)</td><td>0.0           </td><td> Zufluss Linienquelle; nicht verwendet</td></tr>
!!<tr><td> SiL(1) </td><td> 0.0            </td><td> Siliziumgehalt Linienquelle; nicht verwendet </td></tr>
!!<tr><td> itags</td><td>tag               </td><td> Tag im Monat module::modell zeitsekunde()        (unbenutzt)</td></tr>
!!<tr><td> uhrz</td><td>uhrzeit_stunde     </td><td> Uhrzeit module::modell zeitsekunde()        (unbenutzt)</td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!</table>
!! \n\n
!! zurück: \ref lnk_ueberblick ; Quelle: silikat_huelle.f95
!> \page si_aufteilung Ergänzungen Silikat
!! an den Zufluss-Rändern wird der Siliziumanteil in den Kieselalgen,\n
!! QSqm-Variable Q_SK, gespeichert in  planktonic_variable(32 \n
!! gleich dem maximalen Siliziumanteil der Kiesel-Algenbiomasse, \n
!! QSqm-Variable Qmx_SK, gespeichert in QSimDatenfelder aus APARAM.txt (siehe dazu eingabe())\n
!! in RB_werte_aktualisieren() gesetzt.
!!\n\n
!!zurück: \ref lnk_silikat ; \ref zuflussranddaten ; Quelle: silikat_huelle.f95
!! VORSICHT veraltet \n\n
!! zurück: \ref lnk_silikat \n\n
!! <h2>Aufruf</h2>
!! silikat_huelle ist eine T-QSim Hüllroutine (siehe dazu auch: \ref lnk_huellen) zum Aufruf der QSim Subroutine:\n\n
!! \n
!! <h2>Übergabe-Parameter:</h2>
!!<table >
!!<tr><th>     Variablen-Name      </th><th> Beschreibung                     </th><th> Kommuniziert mit:      </th><th> Daten-Typ, Feld-Dimension </th></tr>
!!
!!<tr><td>     si         </td><td> geloestes Silikat-Silizium, tiefengemittelt         </td><td> Transportkonzentration   </td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     Skmor         </td><td> schwebende,abgestorbene Kieselalgen            </td><td> algaeski Transportkonzentration</td><td>  </td></tr>
!!<tr><td>     Q_SK         </td><td> Siliziumgehalt Kieselalgen                </td><td> Transportkonzentration    </td><td>  </td></tr>
!!<tr><td>     tempw         </td><td> Wassertemperatur                  </td><td> TEMPERW Transportkonzentration</td><td>   </td></tr>
!!<tr><td></td><td></td><td></td><td></td></tr>
!!<tr><td>     Siz          </td><td> geloestes Silikat-Silizium, tiefenaufgelöst         </td><td>     </td><td>  </td></tr>
!!<tr><td>     SiRuek         </td><td> Rückgelöste Menge Si.                  </td><td> Rückgabeparameter    </td><td>  </td></tr>
!!<tr><td>     sised         </td><td> Siliziumgehalt im Sediment               </td><td> lokale Konzentration silikat + algaeski    </td><td>  </td></tr>
!!<tr><td></td><td></td><td></td><td></td></tr>
!!<tr><td>     akibrz         </td><td> Gehalt (Wachstum?) Biomasse Kiesel-Algen         </td><td> ALGAESKI      </td><td>   </td></tr>
!!<tr><td>     albewk         </td><td> Wachstum bentischer kiesel-Algen              </td><td> ALBENTH      </td><td> real </td></tr>
!!<tr><td>     up_Siz         </td><td> Si-Aufnahmerate der Kiesel-Algen            </td><td> ALGAESKI      </td><td> real, Dimension(1000)  </td></tr>
!!<tr><td>     Qmx_SK         </td><td> max. Siliziumanteil der Kiesel-Algenbiomasse         </td><td> ALGAESKI      </td><td>   </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td>     itrein         </td><td> unklar  Übergabe in 13.00                   </td><td> diverse      </td><td> integer, Dimension(50,100,100)   </td></tr>
!!<tr><td>     flag         </td><td> Einleitungsflag,                      </td><td> eingelesen ???   </td><td> integer, Dimension(1000) </td></tr>
!!<tr><td>     jiein          </td><td> Anzahl Einleitungen                      </td><td> eingelesen ???   </td><td> integer, Dimension(1000)    </td></tr>
!!<tr><td>     qeinl,qeinlL       </td><td> Volumenstrom Punkteinl. Linieneinl.               </td><td> eingelesen ???   </td><td> real </td></tr>
!!<tr><td>     ieinLs          </td><td> Anzahl der Linienquellen im Strang (mstr)             </td><td> eingelesen ???   </td><td> integer, Dimension(50)    </td></tr>
!!<tr><td>     iorLa          </td><td> AnfangsKnoten der Linienquelle ieinL des Strangs mstr       </td><td> eingelesen ???   </td><td>       </td></tr>
!!<tr><td>     iorLe          </td><td> EndKnoten der Linienquelle ieinL des Strangs mstr          </td><td> eingelesen ???   </td><td>      </td></tr>
!!<tr><td>     SiL         </td><td> Linienquelle für si                     </td><td>    </td><td>  real, Dimension(100) </td></tr>
!!<tr><td>     esi         </td><td> Punkteinleitung si                     </td><td>          </td><td> real, Dimension(100)   </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td>     tiefe,vabfl      </td><td>  hydraulische Parameter                  </td><td> eingelesen ???   </td><td>   </td></tr>
!!<tr><td>     flae         </td><td>  Querschnittsfläche des Gewässerkörpers            </td><td> nur für Linienquelle benötigt, eingelesen sysgenou    </td><td>   </td></tr>
!!<tr><td>     tflie         </td><td>  Fließzeit in Tagen (Zeitschrittweite)            </td><td> ?         </td><td>   </td></tr>
!!<tr><td>     ior,anze,mstr,elen   </td><td>  Zähler, Anzahl im Strang strangnr., Elementlänge         </td><td> eingelesen ???    </td><td>   </td></tr>
!!<tr><td>     ilbuhn         </td><td>  Schalter, ob Buhnen im Querprofil                </td><td> eingelesen ???   </td><td>   </td></tr>
!!<tr><td>     nkzs          </td><td>  Anzahl der Schichten (Tiefenverteilung 2D)             </td><td> .         </td><td> integer, Dimension(1000) </td></tr>
!!<tr><td>     dH2D          </td><td>  Schichtdicke Tiefenverteilung (tiefenaufgel.2D) konstante Abstände   </td><td> .         </td><td> real </td></tr>
!!<tr><td> </td><td> </td><td> </td><td> </td></tr>
!!<tr><td>     aki         </td><td> unbenutzt Anteil Kiesel-Algen                  </td><td> ALGAESKI      </td><td> real, Dimension(1000)  </td></tr>
!!<tr><td>     alberk         </td><td> unbenutzt Respiration bentischer kiesel-Algen              </td><td> ALBENTH      </td><td> real </td></tr>
!!<tr><td>     uhrz,itags      </td><td>  unbenutzt Uhrzeit, Tag ?                  </td><td> .         </td><td>   </td></tr>
!!<tr><td>     dH2De         </td><td>  unbenutzt                        </td><td> .         </td><td>   </td></tr>
!!<tr><td>     akkssi,dalgkz,akitbr   </td><td>  unbenutzt                        </td><td> .         </td><td>   </td></tr>
!!</table>
!> SUBROUTINE silikat_huelle()
!! WIRD VON ALLEN PROZESSEN AUFGERUFEN !!! (parallel)
!! \n\n
!!Beschreibung: \ref lnk_silikat ; Quelle: silikat_huelle.f95
subroutine silikat_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,j,nk
   iglob = (i+meinrang*part)
   kontroll = iglob == kontrollknoten ! Erweiterung QSim3D wy
   nk = ((i-1)*number_plankt_vari)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe:
   si(1) = planktonic_variable_p( 7+nk)  ! silikat-Silizium-Konzentration (tiefengemittelt)
   si(2) = si(1)
   flag(1) = 0         ! keine Einleitungen
   flag(2) = flag(1)
   elen(1) = 1         ! Elementlänge (nicht verwendet)
   elen(2) = elen(1)
   ior = 1             ! Laufindex
   esi(1) = 0.0       ! keine Einleitung
   qeinl(1) = 0.0      ! kein Abfluss Einleitung
   vabfl(1) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   vabfl(2) = vabfl(1)
   anze = 1            ! Anzahl der Profile im aktuellen Strang
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   jiein(1) = 0        ! null Punkt-Einleitungen
   aki(1) = planktonic_variable_p(8+(i-1)*number_plankt_vari) ! Anteil kiesel-Algen (wird nicht verwendet)
   aki(2) = aki(1)
   albewk(1) = benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
   albewk(2) = albewk(1)
   alberk(1) = benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
   alberk(2) = alberk(1)
   tiefe(1) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
   tiefe(2) = tiefe(1)
   tempw(1) = planktonic_variable_p(1+nk)  ! Wassertemperatur
   tempw(2) = tempw(1)
   !sised(1) =benthic_distribution_p(2+(i-1)*number_benth_distr) ! Siliziumgehalt im Sediment
   !sised(2) = sised(1)
   ilbuhn = 0          ! keine Buhnen
   ! direkt aus QSimDatenfelder akkssi=transfer_parameter_p(26)! unbenutzt
   ! direkt aus QSimDatenfelder Qmx_SK=transfer_parameter_p(33) ! max. Siliziumanteil der Kiesel-Algenbiomasse aus APARAM.txt
   Q_SK(1) = planktonic_variable_p(32+nk)  ! Siliziumgehalt Kieselalgen
   Q_SK(2) = Q_SK(1)
   do j = 1,num_lev_trans ! Si-Aufnahmerate der Kiesel-Algen tiefenaufgelöst
      !### veri13.3 ### trans_quant_vert_p(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = 0.1!### veri13.3 ###
      up_Siz(j,1) = trans_quant_vert_p(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
      up_Siz(j,2) = up_Siz(j,1)
   end do
   do j = 1,num_lev
      siz(j,1) = & !plankt_vari_vert_p(7,j,i) ! silikat(7)-Silizium-Konzentration (tiefenaufgelöst)
                 plankt_vari_vert_p(j+(7-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
      siz(j,2) = siz(j,1)
   end do
   do j = 1,num_lev_trans ! ?? unbenutzt
      dalgkz(j,1) = trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      dalgkz(j,2) = dalgkz(j,1)
   end do
   akitbr(1) = transfer_quantity_p(48+(i-1)*number_trans_quant) ! Kieselalgen ?? unbenutzt !
   akitbr(2) = akitbr(1)
   do j = 1,num_lev_trans ! brutto Wachstum Biomasse Kiesel-Algen tiefenaufgelöst
      !### veri13.3 ###trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = 0.1!### veri13.3 ###
      akibrz(j,1) = trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      akibrz(j,2) = akibrz(j,1)
   end do
   !### veri13.3 ### benthic_distribution_p(46+(i-1)*number_benth_distr) = 0.1!### veri13.3 ###
   hJSi(1,1) = benthic_distribution_p(46+(i-1)*number_benth_distr) ! Silizium-Flux aus dem Sediment
   hJSi(1,2) = hJSi(1,1)
   !SiRuek(1) =benthic_distribution_p(25+(i-1)*number_benth_distr) ! Siliziumgehalt im Sediment
   !SiRuek(2) = SiRuek(1)
   !Skmor(1) = planktonic_variable_p(69+nk)  ! Silizium in schwebenden, abgestorbenen Kieselalgen
   !Skmor(2) = Skmor(1)
   nkzs(1) = 1         ! nur eine Tiefenschicht
   nkzs(2) = nkzs(1)
   dH2D = -2.0         ! bisher nur 2D-Tiefengemittelt ??? konstante Tiefenschichtung ???
   dH2De = 0.0   ! unbenutzt
   mstr = 1            ! Strangzähler
   iorLa(1) = 0              ! AnfangsKnoten der Linienquelle; nicht verwendet
   iorLe(1) = 0              ! EndKnoten der Linienquelle; nicht verwendet
   ieinLs(1) = 0             ! keine Linienquellen
   ieinLs(2) = ieinLs(1)
   flae(1) = 0.0           ! unbenutzt da keine Einleitung
   flae(2) = flae(1)
   qeinlL(1) = 0.0           ! Zufluss Linienquelle; nicht verwendet
   SiL(1) = 0.0            ! Siliziumgehalt Linienquelle; nicht verwendet
   itags = tag               ! Tag im Monat module::modell zeitsekunde()        (unbenutzt)
   uhrz = uhrzeit_stunde     ! Uhrzeit module::modell zeitsekunde()        (unbenutzt)
   if (iglob == kontrollknoten) print*,'silikat_huelle vorher knoten ', iglob  &
       ,' si,hJSi(1,1),Skmor,Q_SK,sised,SiRuek,tiefe,tempw = ' &
       ,  si(1),hJSi(1,1),Skmor(1),Q_SK(1),sised(1),SiRuek(1),tiefe(1),tempw(1)
   ! qsim1d 13.40 15okt18
   call silikat(si,flag,elen,ior,esi,qeinl,vabfl,anze,tflie,jiein,aki         &
                ,albewk,alberk,tiefe,tempw,ilbuhn,akkssi,Qmx_SK,Q_SK          &
                ,up_Siz,Siz,algakz,akitbr,akibrz,hJSi,nkzs,dH2D,dH2De,mstr    &
                ,iorLa,iorLe,ieinLs,flae,qeinlL,SiL,itags,Uhrz,azStrs         &
                ,kontroll ,iglob )
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe:
   planktonic_variable_p( 7+nk) = si(1)
   benthic_distribution_p(2+(i-1)*number_benth_distr) = sised(1)
   planktonic_variable_p(32+nk) = Q_SK(1)
   do j = 1,num_lev
      plankt_vari_vert_p(j+(7-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = &
                                                                                  siz(j,1) ! silikat(7)-Silizium-Konzentration (tiefenaufgelöst)
   end do
   benthic_distribution_p(25+(i-1)*number_benth_distr) = SiRuek(1) ! Siliziumgehalt im Sediment
   planktonic_variable_p(69+nk) = Skmor(1)
   if (iglob == kontrollknoten) print*,'silikat_huelle nachher knoten ', iglob  &
       ,' si,hJSi(1,1),Skmor,Q_SK,sised,SiRuek = ',planktonic_variable_p(7+nk),hJSi(1,1)       &
       ,planktonic_variable_p(69+nk),planktonic_variable_p(32+nk) &
       ,benthic_distribution_p(2+(i-1)*number_benth_distr),benthic_distribution_p(25+(i-1)*number_benth_distr)
   if (isnan(si(1))) then
      write(fehler,*)'isnan(si(1)) i = ',iglob
      call qerror(fehler)
   end if ! isnan
   return
end subroutine silikat_huelle
!----+-----+----
!> die Subroutine ini_silikat() steht in silikat_huelle.f95\n
!! schreibt zunächst testwerte in die Siliziumvariablen. \n
!! ### in initialisieren() ausgeschaltet wird über Randbedingungen gesetzt ### skmor bleibt am Start auf 0 ???
subroutine ini_silikat()
   use modell
   implicit none
   integer :: i,j,nk
   do i = 1,number_plankt_point
      nk = ((i-1)*number_plankt_vari)
      planktonic_variable( 7+nk) = 10 ! si
      do j = 1,num_lev
         plankt_vari_vert(j+(7-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = planktonic_variable( 7+nk) ! siz(j,1) ! silikat(7)-Silizium-Konzentration (tiefenaufgelöst)
      end do
      planktonic_variable(32+nk) = 0.004 ! Q_SK
      planktonic_variable(69+nk) = 0.0 ! Skmor
   end do
   do i = 1,number_benthic_points
      benthic_distribution(2+(i-1)*number_benth_distr) = 0.0 !!  sised
   end do
   do i = 1,number_trans_quant_points
      do j = 1,num_lev_trans
         trans_quant_vert(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = 0.0 ! 0.025 ! up_Siz(j,1) ! Si-Aufnahmerate der Kiesel-Algen
      end do
   end do
   return
end subroutine ini_silikat

!----+-----+----
!      end module silikat_module
!
!! <h2>Formeln</h2>
!! !!!algaeski: SKmor = (SKmor + (dkimor+dalgak)*Q_SK)*(1.-(sedalk/aki) \n
!! !!!algaeski: sised = sised + sedalk*Q_SK + SKmor*(sedalk/aki)*tiefe\n
!!! ....Neuberechnung der Silikatmenge an der Gewässersohle nach Rücklösung
!! !.....Rücklösungsrate (Kopf, Pöhlmann Abb.39b)
!!     hconsi = (0.009*1.066**(tempw(ior)-20.))*tflie
!!     Sizt-siz= hconsi*((sised/tiefe)+Skmor) -up_Siz*akibrz -albewk*Qmx_SK
!!     sised = sised*(1-hconsi)
!algaeski:
!      hconmn = Qmx_SK/3.
!      hcon = Si(ior)/(Si(ior)+akkssi)
!      hcon = (Qmx_SK-hconmn)*hcon
!      up_Siz(nkz,ior) = (hconmn+hcon)*dalgkz(nkz,ior)/akibrz(nkz,ior)
!--
!      hcon = (Qmx_SK*2/3)*Si/(Si+akkssi)
!      up_Siz = ( (Qmx_SK/3.)+((Qmx_SK*2/3)*Si/(Si+akkssi)) )*dalgkz/akibrz
!      up_Siz = ( (Qmx_SK/3)*1+[(Qmx_SK/3)*2*(Si/(Si+akkssi))] )*dalgkz/akibrz
!      up_Siz = (Qmx_SK/3)*(1+[2*Si/(Si+akkssi)])  *dalgkz/akibrz
