
!> \page Algen Algen
!!
!!<table border="0" ><tr><td  width="50%" >
!! \image html blaualgenbluetHavelAnnette11_600.jpg "Blau-Algen-Blüte in der Havel bei Berlin (Annette Becker, BfG, 2011)"  
!! </td><td  width="50%" align="left" valign="top">
!! Algen spielen für die Gewässergüte eine zentrale Rolle:\n
!! Durch Photosynthese setzen sie Sauerstoff im Wasser frei.\n
!! Sie bauen Biomasse auf, deren Abbau Sauerstoff zehrt.\n
!! \n 
!! In QSim wurden die Algen bisher in 3 funktionale Gruppen aufgeteilt:\n 
!! <b>Kieselalgen</b>, die Silizium brauchen;\n
!! <b>Grünalgen</b>; \n
!! <b>Blaualgen</b>, die auch elementaren Stickstoff fixieren können.
!!</td></tr></table>
!!
!! <h2>Herkunft</h2>
!!     algaeski() 
!!***** UNTERPROGRAMM ZUR BERECHNUNG DES KIESELALGENWACHSTUMS ****** \n                  
!!     AUTOR: VOLKER KIRCHESCH            \n                               
!!     entnommen aus Version qsim13.301_28mae18\n 
!!
!! <h2>Teilprozesse</h2>
!! Die \subpage Veränderung-Algenmasse ergibt als Summe aus:
!!<ol> 
!!    <li>\ref Algen-Wachstum, wobei die folgenden Einflüsse berücksichtigt werden: </li>
!!    <ul> 
!!       <li>\ref Licht_algen </li>
!!       <li>\ref Nährstoff_algen </li>
!!       <li>\ref Temperatur_algen </li>
!!       <li>\ref chla </li>
!!    </ul>
!!    <li>\ref Algen-Respiration </li>
!!    <li>\ref Algen-Mortalität </li>
!!    <li>\ref Algen-Sedimentation </li>
!!    <li>\ref Algen-Konsum-planktisch </li>
!!    <li>\ref Algen-Konsum-bentisch </li>
!!    <li>\ref Algen-Abspülung </li>
!!</ol>
!!
!! <h2>Schnittstellenbeschreibung</h2>
!!
!! Die QSim3D Subroutine algae_huelle() dient dem Aufruf der 
!! QSim-subroutinen algaeski(), algaesgr() und algaesbl(),
!! (Zum Hüllroutinen-Konzept siehe: \ref hüllen )
!! \n\n
!! Die im folgenden dargestellten source-code Ausschnitte zeigen die Subroutinen-Aufrufe. Diese Aufrufe geschehen über die in 
!! QSim-1D verwendeten Varablen-Namen (QSimDatenfelder).\n
!! Die Links auf den QSim1D-Übergabe-Variablen führen zu den Erläuterung der in QSim3D verwendeten Variablenfelder.
!! \n
!! ### VORSICHT ### Aufrufreihenfolge (erst Kieselalgen, zuletzt Grünalgen) muss eingehalten werden !!! ###
!! \n
!! <h4>Kieselalgen</h4>
!! Version qsim13.301_28mae18\n\n <code>
!!      call algaeski() \ref schwi, \ref tflie, \ref tempw, \ref tempwz                                            &\n
!!     & , \ref rau, \ref tiefe, \ref vmitt, \ref flae, \ref vno3, \ref vnh4, \ref gelp, \ref svhemk, \ref svhemb, \ref svhemg                        &\n
!!     & , \ref chla, \ref ir                                                                         &\n
!!     & , \ref si, \ref dalgki, \ref dalgak, \ref flag, \ref elen, \ref ior, \ref anze                                             &\n
!!     & , \ref sedalk, \ref algzok, \ref echla, \ref qeinl, \ref vabfl                                                 &\n
!!     & , \ref dkimor, \ref fkm, \ref jiein, \ref evkigr, \ref vkigr, \ref antbl, \ref eantbl                                      &\n
!!     & , \ref akchl, \ref akgmax, \ref akksn, \ref akksp, \ref akkssi, \ref saettk, \ref akremi, \ref akrema                            &\n
!!     & , \ref sbioki, \ref vco2, \ref iph, \ref akbcm, \ref abbcm, \ref agbcm, \ref aki, \ref abl, \ref agr, \ref extk, \ref extk_lamda                   &\n
!!     & , \ref ilamda, \ref eta, \ref aw, \ref ack, \ref acg, \ref acb, \ref ah, \ref as, \ref al                              		& !!wy\n
!!     & , \ref uhrz, \ref sised, \ref tpki, \ref iwied, \ref akmuea, \ref ftaaus, \ref fiaus, \ref fheaus                                &\n
!!     & , \ref akraus, \ref tauscs, \ref ischif, \ref ilbuhn, \ref ieros, \ref askie, \ref cmatki, \ref algdrk                           &\n
!!     & , \ref algcok, \ref ess, \ref zooind, \ref grote, \ref ss, \ref q_pk, \ref q_nk, \ref q_sk			                &\n
!!     & , \ref vnh4z, \ref vno3z, \ref gelpz, \ref siz, \ref dalgkz, \ref nkzs, \ref dh2d, \ref cpfad			                &\n
!!     & , \ref up_pkz, \ref up_nkz, \ref up_siz, \ref qmx_pk, \ref qmn_pk, \ref upmxpk                   		        &\n
!!     & , \ref qmx_nk, \ref qmn_nk, \ref upmxnk, \ref qmx_sk, \ref qmn_sk, \ref upmxsk, \ref skmor, \ref ikke, \ref frmuke		        &\n
!!     & , \ref alamda, \ref akitbr, \ref chlaz, \ref akibrz, \ref akiz                                                 &\n
!!     & , \ref chlal, \ref qeinll, \ref ieinls, \ref algakz, \ref algzkz, \ref ablz, \ref agrz                                     &\n
!!     & , \ref chlaki, \ref hchlkz, \ref hchlgz, \ref hchlbz    							&\n
!!     & , \ref hcchlkz, \ref hcchlbz, \ref hcchlgz    							&\n
!!     & , \ref dz2d, \ref toptk 		  							&\n
!!     & , \ref ktemp_ki          								&\n 
!!     & , \ref ifix 										&\n
!!     & , \ref chlabl, \ref chlagr, \ref a1ki, \ref a2ki, \ref a3ki, \ref sedalg_mq, \ref sedalk0, \ref hq_nkz    		        & !! noch unbearbeitet #################\n
!!     & , \ref hQ_NGz, \ref hQ_NBz, \ref Q_PG, \ref Q_NG, \ref Q_PB, \ref Q_NB                        &\n
!!     & , \ref extkS, \ref akmor_1, \ref agmor_1, \ref abmor_1
!!     & , \ref mstr, \ref it_h, \ref itags, \ref monats, \ref isim_end, \ref azstrs                                          &\n
!!     & , \ref kontroll , \ref iglob )                 !!wy\n
!! </code>\n
!! <h4>Blaualgen</h4>
!! Version qsim13.301_28mae18\n\n <code>
!! call algaesbl()  \ref schwi, \ref tflie, \ref tempw, \ref flag, \ref elen, \ref rau, \ref tiefe
!! , \ref vmitt, \ref vno3, \ref vnh4, \ref gelp, \ref svhemb, \ref chla, \ref ir                              &\n
!! , \ref dalgbl, \ref dalgab, \ref ior, \ref anze, \ref sedalb, \ref algzob, \ref dblmor, \ref fkm
!! , \ref vabfl, \ref abchl, \ref abgmax, \ref abksn, \ref abksp, \ref saettb, \ref abremi     &\n
!! , \ref vco2, \ref iph, \ref vkigr, \ref abbcm, \ref abl, \ref tpbl, \ref uhrz, \ref iwied
!! , \ref fibaus, \ref abmuea, \ref fhebas, \ref abreau, \ref tauscs, \ref ischif, \ref ilbuhn, \ref ieros  &\n
!! , \ref zakie, \ref zagre, \ref zable, \ref asble, \ref qeinl, \ref jiein, \ref echla, \ref ess
!! , \ref algdrb, \ref algcob, \ref antbl, \ref zooind, \ref grote, \ref ss, \ref extk           &\n
!! , \ref extk_lamda                                                                                       &\n
!! , \ref ilamda, \ref eta, \ref aw, \ref ack, \ref acg, \ref acb, \ref ah, \ref as, \ref al       							   & !!wy, einlesen von e_extnct.dat nicht hier\n
!! , \ref vnh4z, \ref vno3z, \ref gelpz, \ref dalgbz, \ref nkzs, \ref dh2d, \ref tempwz, \ref cpfad
!! , \ref up_pbz, \ref up_nbz, \ref qmx_pb, \ref qmn_pb                      &\n
!! , \ref upmxpb, \ref qmx_nb, \ref qmn_nb, \ref upmxnb, \ref q_nb, \ref q_pb, \ref ikbe
!! , \ref frmube, \ref alamda, \ref abltbr, \ref ablbrz, \ref up_n2z, \ref ablz               &\n
!! , \ref chlabl, \ref a1bl, \ref a2bl, \ref a3bl, \ref hchlbz, \ref hcchlbz, \ref algabz, \ref algzbz
!! , \ref dz2d, \ref toptb, \ref ktemp_bl, \ref ifix, \ref sedalg_mq            &                      \n         
!! , \ref sedalb0, \ref hq_nbz, \ref  mstr, \ref itags, \ref monats, \ref isim_end, \ref azstrs                                                &\n
!! , \ref ialloc2  , \ref kontroll , \ref iglob )              !!wy                            
!! </code>
!! \n\n
!! <h4>Grünalgen</h4>
!! Version qsim13.301_28mae18\n\n <code>
!! call algaesgr() \ref schwi, \ref tflie, \ref tempw, \ref rau, \ref tiefe, \ref vmitt, \ref vno3, \ref vnh4
!! , \ref gelp, \ref svhemg, \ref chla, \ref ssalg, \ref dalggr, \ref dalgag                    &\n
!! , \ref flag, \ref elen, \ref ior, \ref anze, \ref sedalg, \ref algzog, \ref dgrmor, \ref fkm, \ref vkigr, \ref chlaki
!! , \ref chlagr, \ref vabfl, \ref qeinl, \ref jiein, \ref evkigr, \ref eantbl        &\n
!! , \ref agchl, \ref aggmax, \ref agksn, \ref agksp, \ref agremi, \ref vco2, \ref algdrg, \ref pbiogr, \ref q_pk
!! , \ref q_nk, \ref iph, \ref akbcm, \ref agbcm, \ref aki, \ref agr, \ref cmatgr            &\n
!! , \ref cmatki, \ref abbcm, \ref antbl, \ref abl, \ref pbiobl, \ref chlabl, \ref extk, \ref extk_lamda &\n
!! , \ref ilamda, \ref eta, \ref aw, \ref ack, \ref acg, \ref acb, \ref ah, \ref as, \ref al & !!wy, \ref  einlesen von e_extnct.dat nicht hier\n
!! , \ref tpgr, \ref uhrz, \ref iwied, \ref algcog  &\n
!! , \ref figaus, \ref agmuea, \ref fhegas, \ref agreau, \ref tauscs, \ref ischif, \ref ilbuhn, \ref ieros
!! , \ref asgre, \ref echla, \ref ess, \ref ss, \ref zooind, \ref grote, \ref q_pg, \ref q_ng       &\n
!! , \ref vnh4z, \ref vno3z, \ref gelpz, \ref dalggz, \ref nkzs, \ref dh2d, \ref tempwz, \ref cpfad, \ref itags
!! , \ref monats, \ref mstr, \ref up_pgz, \ref up_ngz, \ref qmx_pg                 &\n
!! , \ref qmn_pg, \ref upmxpg, \ref qmx_ng, \ref qmn_ng, \ref upmxng, \ref ikge, \ref frmuge, \ref alamda
!! , \ref agrtbr, \ref agrbrz, \ref akiz, \ref agrz, \ref ablz                     &\n
!! , \ref chlaz, \ref hchlkz, \ref hchlgz, \ref hchlbz, \ref hcchlgz, \ref algagz, \ref algzgz, \ref dz2d
!! , \ref toptg, \ref ktemp_gr, \ref ifix, \ref sedalg_mq, \ref sedalg0, \ref  hq_ngz    &\n
!! , \ref a1gr, \ref a2gr, \ref a3gr, \ref ifehl, \ref ifhstr, \ref isim_end, \ref azstrs & \n                 
!! , \ref kontroll , \ref iglob )  !!wy \n                           
!! </code>
!!
!! <h2>Rand und Anfangsbedingungen</h2>
!! \ref algenaufteilung Aufteilung Algen-Variablen im Zufluss mittels Subroutine algae_start(). 
!! Siehe dazu auch \ref randbedingungen_ergaenzen .
!!
!! <h2>Dokumentation und Veröffentlichungen</h2>
!! Bisher existiert eine Dokumentation des Algen/Phytoplankton-Moduls als Kapitel 11 der
!! <a href="./pdf/QSimDoku_ncycWy.pdf" target="_blank">Kurzdoku</a> (Version vom 22. Nov. 2017)
!! \n\n
!! Einen etwas tieferen Einblick in die Berechnungsverfahren und ihre numerische Umsetzung liefert die 
!! <a href="./pdf/QSimUndergroundDokuAnnette10.pdf" target="_blank"> Dokumentation von Annette Becker</a>.\n
!! Der Algen-Baustein in QSim wurden seit Erstellung o.g. Dokumentationen weiterentwickelt.
!! \n\n
!! <a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+118" target="_blank"> 
!! Influence of global change on phytoplankton and nutrient cycling in the Elbe River,</a>
!! Quiel et al. 2011
!! \n\n
!!  <a href="http://bibliothek.bafg.de/index.asp?detsuche_systematik=sep+8989" target="_blank">
!!      Modelling the chlorophyll a content of the river Rhine - 
!!      interrelation between riverine algal production and population biomass of grazers, rotifers and the zebra mussel, Dreissena polymorpha</a>
!!      Schöl et al. 2002 
!! \n\n
!! Für die Diskussion um die Algen-Module hat Annette Becker ihre Argumente in einer \n
!! <a href="./pdf/AlgenDiskussionKurz.pdf" target="_blank"> Kurz-Fassung</a> und einer 
!! <a href="./pdf/AlgenDiskussion.pdf" target="_blank"> Langfassung</a> zusammengetragen.\n
!! sowie einen <a href="./pdf/UmstrukturierungAlgenbaustein.pdf" target="_blank">Vorschlag</a>
!! zur Umstrukturierung / Zerlegung ausgearbeitet:\n
!! \n\n
!! zurück: \ref Stoffumsatz ; Code: algae_huelle.f95
!
!-----------------------------------------------------------------------------
!> \page Veränderung-Algenmasse Veränderung der Algen-Biomasse
!!
!! Die Veränderung der Algen-Biomasse in einem Zeitschritt wird als Summe der folgenden Teilprozesse modelliert:\n
!! <ul>
!!    <li>\subpage Algen-Wachstum</li>
!!    <li>\subpage Algen-Mortalität</li>
!!    <li>\subpage Algen-Respiration</li>
!!    <li>\subpage Algen-Abspülung</li>
!!    <li>\subpage Algen-Sedimentation</li>
!!    <li>\subpage Algen-Konsum-planktisch</li>
!!    <li>\subpage Algen-Konsum-bentisch</li>
!! </ul>\n
!! Die folgende Formel dient der Einführung der Bezeichnungen für die Teilprozesse
!! \f{eqnarray*} {
!! & A_i(t + \Delta t) - A_i(t) & \\
!! &=&\left( {\Delta grow}_i - {\Delta mor}_i - {\Delta resp}_i \right) \\
!! &+&\left( cmatki - sedalk - algzok - algdrk - algcok\right) 
!! \f}
!! 
!! <h2>Algen-Wachstum</h2>
!! Dabei wird die Zunahme der Algenbiomasse als exponentielles Wachstum disktretisiert:
!! \f[ {\Delta grow}_i = A_i(t) \cdot e^{\left(\mu A_i \cdot \Delta t \right)} \f]
!! Wie die Rate des Algenwachstums, \f$ \muA_i \f$, bestimmt wird, findet sich im Abschnitt: \ref Algen-Wachstum
!! \n\n
!! Dieser und die folgenden exponentiellen Formeln gehen auf die Lösung einer Wachstums-Differentialgleichung
!! zurück, bei welcher der Zuwachs proportional zur vorhandenen Konzentration ist: 
!! \f[\frac{ \partial A_i  }{\partial t} = \mu_i  \cdot  A_i \f]
!! 
!! <h2>Algen-Mortalität</h2>
!! Die Mortalität der Algen wird ebenfalls als exponentieller Rückgang disktretisiert:
!! \f[ {\Delta mor}_i = A_i(t) \cdot (1 - e^{\left( mor_i \cdot \Delta t \right)}  )\f]
!! Wie die Rate des Algenmortalität, \f$ mor_i \f$, bestimmt wird, findet sich im Abschnitt: \ref Algen-Mortalität
!! 
!! <h2>Respiration</h2>
!! Der Biomasseverlust an Algen, der infolge Respiration (Atmung) eintritt, wird ebenfalls exponentiell diskretisiert:\n
!! Wie die Respirationsrate \f$ resp_i \f$ berechnet wird, findet sich im Abschnitt: \ref Algen-Respiration
!! \f[ {\Delta resp}_i  = A_i(t) \cdot (1 - e^{\left( resp_i \cdot \Delta t \right)}  )\f]
!! 
!! \n Formelzeichen: \n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ A_i \f$  </td><td> \ref aki,\ref agr,\ref abl </td><td> Algenbiomasse pro Wasservolumen, i=ki,gr,bl </td><td> mgBio/l </td><td> ? </td></tr>
!!<tr><td> \f$ t \f$  </td><td> - </td><td> Zeit, (Zeitpunkt des vorangegangenen Zeitschritts) </td><td> d </td><td> - </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> \ref tflie </td><td> Zeitschrittweite </td><td> d </td><td> 1/96 ... 1 </td></tr>
!!<tr><td> \f$ A_i(t + \Delta t) \f$  </td><td> akit,agrt,ablt </td><td> Algenbiomasse zum aktuellen Zeitpunkt </td><td> mg/l  </td><td>  </td></tr>
!!<tr><td> \f$ {\Delta grow}_i \f$  </td><td> \ref dalgki, \ref dalggr, \ref dalgbl</td><td> Zunahme infolge \ref Algen-Wachstum ; i=ki,gr,bl pro Zeitschritt </td><td> mgBio/l </td><td>  </td></tr>
!!<tr><td> \f$ \mu A_i \f$  </td><td> akgrow, aggrow, abgrow </td><td> Rate des \ref Algen-Wachstum i=ki,gr,bl </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ {\Delta mor}_i \f$  </td><td> \ref dkimor, \ref dgrmor, \ref dblmor </td><td> Abnahme infolge \ref Algen-Mortalität, i=ki,gr,bl pro Zeitschritt </td><td>  mgBio/l  </td><td>  </td></tr>
!!<tr><td> \f$ mor_i \f$  </td><td> akmor, agmor, abmor </td><td> Rate der \ref Algen-Mortalität i=ki,gr,bl  </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ {\Delta resp}_i \f$  </td><td> \ref dalgak, \ref dalgag, \ref dalgab </td><td> Abnahme infolge \ref Algen-Respiration, i=ki,gr,bl pro Zeitschritt </td><td>  mgBio/l  </td><td>  </td></tr>
!!<tr><td> \f$ resp_i \f$  </td><td> akres, agres, abres </td><td> Rate der \ref Algen-Respiration i=ki,gr,bl </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ cmatki \f$  </td><td> \ref cmatki </td><td> \ref Algen-Abspülung pro Zeitschritt </td><td> mgBio/l  </td><td>  </td></tr>
!!<tr><td> \f$ sedalk \f$  </td><td> \ref sedalk </td><td> \ref Algen-Sedimentation </td><td> mgBio/l </td><td>  </td></tr>
!!<tr><td> \f$ algzok \f$  </td><td> \ref algzok </td><td> \ref Algen-Konsum-planktisch </td><td> mgBio/l </td><td>  </td></tr>
!!<tr><td> \f$ algdrk \f$  </td><td> \ref algdrk </td><td> \ref Algen-Konsum-bentisch Muscheln </td><td> mgBio/l </td><td>  </td></tr>
!!<tr><td> \f$ algcok \f$  </td><td> \ref algcok </td><td> \ref Algen-Konsum-bentisch Corophium </td><td> mgBio/l </td><td>  </td></tr>
!!</table>\n
!! zurück: \ref Algen ; Code: algae_huelle.f95
! 
!-----------------------------------------------------------------------------
!> \page Algen-Wachstum Algen-Wachstum
!! \n
!! \section loka_wachs lokales Wachstum
!! Das Wachstum benötigt Licht und Nährstoffe; ausserdem ist es temperaturabhängig.\n 
!! Dies wird in QSim dergestalt modelliert, dass die Wachstumsrate \f$ \mu A_j \f$
!! der lichtabhängigen Produktionsrate \f$ P \f$ entspricht, 
!! die durch Temperatur und Nährstoffverfügbarkeit limitiert sein kann:
!! \f[ 
!!      \mu A_j = P \cdot FTA \cdot F_{Naehr} 
!! \f]
!! Die Limitierungsfaktoren für Temperatur  \f$ FTA \f$ und Nährstoffe \f$ F_{Naehr} \f$ 
!! bewegen sich in einem Wertebereich von 0 bis 1, und bewirken somit eine Limitierung von vollständig (0) 
!! bis zu nicht vorhandenen (1).\n
!! Die mathematische Erfassung dieser drei Einflüsse ist in den folgenden Unterkapiteln dargestellt
!!    <ul> 
!!       <li>\subpage Licht_algen \f$ P \f$</li>
!!       <li>\subpage Nährstoff_algen \f$ F_{Naehr} \f$</li>
!!       <li>\subpage Temperatur_algen \f$ FTA \f$</li>
!!    </ul>
!! \n
!! \section verti_wachs vertikale Verteilung des Wachstums
!! \n
!! Vertikal geschichtete Gewässer weisen vertikale Unterschiede in Temperatur und Nährstoffkonzentrationen auf;
!! und bieten Algen dadurch z. T. sehr unterschiedliche Lebensräume.
!! Dies kann durch die 2D-Option in QSim1D 
!! (Die Tiefenverteilung macht aus dem querschnittsgemittelten Modell ein breitengemitteltes)
!! berücksichtigt werden. \n zur Realisierung von Tiefenverteilungen in QSim3D siehe \ref tiefenverteilung \n
!! \n Aber auch vertikal gut durchmischte, zumeist schnell fließende Gewässer, in denen sich 
!! keine großen Unterschiede in Temperatur und Nährstoffen ausbilden können, weisen Lichtgradienten auf.
!! Diese Fälle lassen sich hydraulisch und vom Stofftransport zwar in guter Näherung tiefengemittelt berechnen,
!! es ist aber erforderlich, zu berücksichtigen, dass das Licht zur Sohle hin abnimmt 
!! und die Algen infolge turbulenter Durchmischung einem wechselnden Lichtklima ausgesetzt sind.
!! \n\n
!! Dem wird in QSim durch Einführung einer separaten, von der Durchmischung gesteuerten Licht-Tiefenschichtung
!! Rechnung getragen.\n 
!! Dazu wird angenommen, dass in einem 
!! flachen, gleichförmigen Gerinne mit freier Oberfläche die Turbulenz durch die Sohlreibung produziert wird.
!! Die vertikale Diffusivität nimmt dann nach 
!! <a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=A+224" target="_blank">
!! Fischer et al. 1979</a> \ref b 
!! (siehe auch <a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=A+7042" target="_blank">
!! Nezu+Nakagawa 1993</a> \ref b) 
!! im Mittel über die Tiefe folgenden Wert an: 
!! \f[ 
!!      \nu_{turb} = 0.06667 \cdot u^\star \cdot h
!! \f]
!! wobei sich die Sohlschubspannungsgeschwindigkeit folgendermaßen aus der Sohlschubspannung berechnet:
!! \f[ 
!!      u^\star = \sqrt{\frac{\tau_{Sohle}}{\rho_{Wasser}}}
!! \f]
!! \n
!! Wird nun angenommen, dass sich Algen innerhalb einer gewissche Zeitspanne an das sie erreichende Licht anpassen,
!! z.Z. wird diese Relaxationszeit in QSim mit 100 Sekunden angesetzt, dann lässt sich eine Schichtdicke im Wasser 
!! berechnen, die von der Hälfte der darin enthaltenen Algen innerhalb der Relaxationszeit verlassen wird. 
!! (Die andere Hälfte verbleibt darin):
!! \f[ 
!!      dz = \sqrt{ t_{relax} \cdot 2.0 \cdot \nu_{turb} } 
!! \f]
!! \n
!! Die Wachstumsrate \f$ \mu A_j \f$ wird für alle Licht-Tiefenschichten separat berechnet.\n
!! Im Falle von tiefengemittelten Berechnungen werden die Wachstumsraten dann zu einer mittleren aufsummiert.\n
!! Im Fall von tiefenaufgelösten Simulationen wird die Tiefenverteilung der Wachstumsrate auf die 
!! vertikalen Schichten interpoliert.
!! 
!!  <hr> mit: \n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \mu A_j \f$  </td><td> akgrow, aggrow, abgrow </td><td> Algen-Wachstumsrate siehe: \ref Veränderung-Algenmasse </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ P \f$  </td><td> Pcmit </td><td> Produktionsrate infolge \ref Licht_algen - Genuss unter sonst optimalen Bedingungen </td><td>  1/d  </td><td>  ?  </td></tr>
!!<tr><td> \f$ F_{Naehr} \f$ </td><td> F5 </td><td> \ref nähr_limi </td><td> - </td><td>  0-1  </td></tr>
!!<tr><td> \f$ FTA \f$  </td><td> FTA </td><td> \ref Temperatur_algen - Einfluss </td><td> - </td><td>  0-1  </td></tr>
!!<tr><td> \f$ j \f$  </td><td> - </td><td> Laufindex über Algengruppen j=kiesel, grün, blau \n (nur bei der Wachstumsrate mit angeschrieben) </td><td> - </td><td>  ?  </td></tr>
!!<tr><td> \f$  \f$  </td><td>  </td><td>  </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ \nu_{turb} \f$  </td><td> dztot </td><td> turbulente Diffusivität </td><td> m²/s </td><td>  </td></tr>
!!<tr><td> \f$ u^\star \f$  </td><td> ust </td><td> Sohlschubspannungsgeschwindigkeit </td><td> m/s </td><td>  </td></tr>
!!<tr><td> \f$ h \f$  </td><td> tiefe </td><td> Wassertiefe </td><td> m </td><td>  </td></tr>
!!<tr><td> \f$ \tau_{Sohle} \f$  </td><td> - </td><td> Sohlschubspannung </td><td> N/m² </td><td>  </td></tr>
!!<tr><td> \f$ \rho_{Wasser} \f$  </td><td> - </td><td> Dichte </td><td> Kg/m³ </td><td>  </td></tr>
!!<tr><td> \f$ t_{relax} \f$  </td><td> tauad </td><td> Relaxationszeit der Algen </td><td> sec </td><td> 100 </td></tr>
!!<tr><td> \f$ dz \f$  </td><td> dz </td><td> Dicke der Licht-Tiefenschichten </td><td> m </td><td> - </td></tr>
!!</table>\n\n
!! zurück: \ref Veränderung-Algenmasse ; Code: algae_huelle.f95

!-----------------------------------------------------------------------------
!> \page Licht_algen Licht
!! \n 
!! \section PAR Licht, das die Alge erreicht
!! Nicht die gesamte von der Sonne eingestrahlte Lichtleistung erreicht auch die Algen in tieferen Wasserschichten.
!! Daher wird in QSim die Absorpion durch das Wasser selbst sowie die Algen, 
!! Humin-Stoffe(Gelbstoffe) und suspendierte Schwebstoffe berücksichtigt. 
!! Die Absorpion ist aber stark von der Wellenlänge abhängig.
!! \n\n
!! QSim arbeitet mit dem Lichtspektrum von 400 bis 700 nano-meter (Wellenlänge in Luft), 
!! das ungefähr dem sichtbaren Lichtbereich entspricht. (400nm - blau, 700 nm - rot).
!! Durch eine Aufrasterung mit einer Schrittweite von 10 nm entstehen so 31 "Farben".
!! Mithilfe der Datei <a href="./exp/e_extnct.dat" target="_blank">e_extnct.dat</a> 
!! werden die Absorptions-Spektren oder auch \ref extnct_rb als \ref zuflussranddaten vorgegeben.\n
!! Dabei wird auch eine spektrale Verteilung der Sonneneinstrahlung angegeben.\n
!! Das nachstehende Bild ist eine graphische Darstellung der aktuell verwendeten Spektren.
!! \image html absorbtionsspectrum.png aus 
!! Die Graphik ist der 
!! <a href="./pdf/QSimUndergroundDokuAnnette10.pdf" target="_blank"> Dokumentation von Annette Becker</a> entnommen.
!! \n\n
!! Wie im Baustein der \ref Waermebilanz näher ausgeführt, wird mittels der subroutine strahlg()
!! aus den Tagessummen eine aktuelle Strahlungsleistung \f$ Q_{s,g} \f$ der Sonne an der Gewässeroberfläche ermittelt.
!! Diese wird mithilfe des Sonnenlichtspektrum  \f$a_l(i)\f$ in ein 
!! Spektrum der Strahlungsintensität \f$ I_0(i) \f$ an der Gewässeroberfläche umgerechnet.
!! \f[ 
!!     I_0(i)= 5.846 \cdot Q_{s,g} \cdot 4.2 \cdot a_l(i)
!! \f]  
!! der Gesamtextiktions-koeffizinet auf einer Wellenlänge ergibt sich dann aus der Summe aller Inhaltsstoffe: 
!! \f[ 
!!     extk(i) = a_{c,ki} \cdot Chla_{ki}
!!             + a_s(i) \cdot S_{schweb}
!!             + a_w(i) + a_h(i) 
!! \f]  
!! (zu den Schwebstoffen werden die Rotatorien hinzugezählt),\n 
!! (die anderen Algengruppen werden in gleicher Weise wie die Kieselalgen hinzuaddiert)
!!\n\n
!! damit läßt sich dann die Strahlungsintensität in einer gewissen Tiefe ermitteln:
!! \f[ 
!!     I(i,z) = I_0(i) \cdot e^{\left(extk(i) \cdot z \right)}
!! \f]  
!! Um das Licht, das eine Alge erreicht zu ermitteln wird nun noch über die im Abschnitt \ref verti_wachs
!! beschriebenen Licht-Schichten gemittelt. Dies geht bei geringen Schwebstoffgehalten, indem der Mittelwert von 
!! einer schicht gebildet wird. Bei hohen Schwebstoffgehalten, bei denen das Licht schon in den ersten Centimetern
!! einer infolge stärkerer Mischung dichteren Schicht verschwindet, ist das Integral über die Lichtintensität genauer:
!! \f[ 
!!    I_l(i,j)=\frac{ \int\limits^{zo}_{zu} I(i,z) dz }{zo-zu} = I_0(i) \cdot 
!!    \frac{e^{\left(extk(i) \cdot zo \right)}-e^{\left(extk(i) \cdot zu \right)}}{extk(i)\cdot(zo-zu)}
!! \f]  
!! \n 
!! \section Lichtgenuss Licht, das von der Alge genutzt wird
!!
!! Obwohl von der Photosynthese nicht alle Wellenlängen des Lichts gleichmäßig ausgenutzt werden können (Grünlücke),
!! werden die Spektren der Lichtintensität in QSim aufsummiert. Dies geschieht, 
!! um mit Messungen, die mittels PAR-Lichtsensoren vorgenommen wurden, vergleichen zu können. \n\n
!! PAR (photosynthetic active radiation) - Sensoren haben eine im sichtbaren Spektrum 
!! recht gleichverteilte Charakteristik. 
!! Beispielhaft im folgenden der von uns verwendete LI-COR_Lichtsensors LI-193SA:
!! \image html LI-COR_PAR_klein.png 
!! Abbildung aus der <a href="./pdf/LI-COR_Lichtsensor.pdf" target="_blank">Hersteller-Broschüre</a>
!! \n\n
!! \f[ 
!!     Ic(j)= \sum_{i=1}}^{ilamda}  I_l(i,j)
!! \f]  
!! Zwischen dieser über alle ilamda Lichtwellenlängen aufsummierten Lichtintensität 
!! in der j-ten Schicht des \ref verti_wachs, \f$ Ic \f$, und der Produktionsrate \f$ P \f$ des \ref Algen-Wachstum s
!! wird folgender Zusammenhang angesetzt:  \anchor Produktionsrate
!! \f[ 
!!     \frac{ P}{P_{max} \cdot h} = 1.-e^{-Ic/b} 
!! \f]
!! Die Größe \f$ h \f$ dient der Erfassung der \ref lichtinhibition .\n
!! Die Größe \f$ b \f$ berücksichtigt den variablen \ref chla  \n  
!! Die maximale C-spezifische Photosyntheserate bei optimal Temperatur \f$ P_{max} \f$ berechnet sich
!! mittels der in APARAM.txt vorgegebenen \ref globaleParameter \n
!! akgmax - Max. Wachstumsate d. Kieselalgen, \n
!! akremi - Grundrespiration d. Kieselalgen und \n
!! frespg = frmuke - Anteil der vom Wachstum abhängigigen Respiration, Synthesekosten \n
!! nach der Formel: \f$ P_{max} = (akgmax+akremi)/(1.-frmuke) \f$
!!  \n
!! \image html P-I_scaliert.svg "Illustration des Algenwachstums in Abhängigkeit der Lichtintensität"
!!  \n \n
!! \n
!! \subsection lichtinhibition Lichtinhibition
!! Der Hemmfaktor \f$ h \f$ in der Gleichung für die \ref Produktionsrate wird in der 
!! Subroutine LichtHemmung() berechnet. Er spiegelt eine Lichtinhibition des Algenwachstums wieder,
!! die bei übermäßigem Lichtgenuss ansteigt und bei fehlendem Licht wieder abnimmt.\n
!! Dazu wird die folgende Entwicklungs-Differentialgleichung gelöst:
!! \f[ 
!!     \frac{ dh}{dt} = -0.006048 \cdot Ic \cdot h + 3.888 \cdot (1-h)
!! \f]
!! \n
!! \subsection chla Chlorophyll-Gehalt
!! Algen können ihren Chlorophyll-Gehalt an die Lichtverhältnisse anpassen.
!! Dies wird in QSim mittels des Faktors \f$ b \f$ berücksichtigt, der in obiger Gleichung 
!! für die \ref Produktionsrate die Lichtintensität skaliert:
!! \f[ 
!!     b = I_{saett}(T) \cdot \frac{Chla:C}{CChl^{\star}(T)}
!! \f]
!! \n Dabei wird die temperaturabhängige Sättigungslichtstärke \f$ I_{saett}(T) \f$ in QSim folgendermaßen angesetzt:
!! \f[ 
!!     I_{saett}(T)  = Ik_{ki} \cdot 0.576 \cdot e^{0.0276 \cdot T}
!! \f]
!! \n Für das temperaturabhängige Referenzverhältnis \f$ CChl^{\star}(T) \f$ verwendet QSim den folgenden Ansatz:
!! \f[ 
!!     CChl^{\star}(T)  = ((1000. \cdot C_{aki}/a_{Chla,ki} + 22.8) - 1.14 \cdot T)
!!                      + (1.85 \cdot (Ik_{ki} \cdot 0.576 \cdot e^{0.0276 \cdot T}) \cdot e^{-0.126 \cdot T})
!! \f] 
!! \n Das Chlorophyll-a zu Kohlenstoff-Verhältnis,\f$ Chla:C \f$, wird von der Subroutine C_chla()  nach 
!! folgender Entwicklungs-Differentialgleichung berechnet:
!! \f[ 
!!     \frac{ d (Chla:C)}{dt} = \tau_{hell/dunkel} \cdot ( Y_I- Chla:C )
!! \f]
!! mit dem Grenzverhältnis \f$ Y_I = (1000. \cdot C_{aki}/a_{Chla,ki}) 
!!               - 1.14 \cdot T 
!!               + 1.85 \cdot Ic \cdot e^{-0.126 \cdot T} \f$ 
!! \n und \n
!! \f$ \tau=0.05 \f$ wenn \f$ Y_I > Chla:C\f$ (im Hellen),\n
!! \f$ \tau=0.15 \f$ wenn \f$ Y_I < Chla:C\f$ (im Dunkel). 
!! \n
!! <hr> <i>die empirischen Formeln in diesem Abschnitt sind z.T. nicht dimensionsrein</i>\n
!! mit:\n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ Q_{s,g} \f$  </td><td> schwi </td><td> aktuelle Globalstrahlung an der Wasseroberfläche </td><td> cal/(cm2*h) </td><td>  </td></tr>
!!<tr><td> \f$ I_0(i) \f$  </td><td> I0 </td><td> Lichtintensität (Photonenstrom) an der Gewässeroberfläche in der i-ten Wellenlänge </td><td> µE/(s*m²) </td><td>  </td></tr>
!!<tr><td> \f$ a_l(i) \f$  </td><td> al </td><td> Spektralverteilung Sonnenlicht </td><td> - </td><td>  </td></tr>
!!<tr><td> \f$ i \f$  </td><td> </td><td> Zähler über die Lichtwellenlängen </td><td> - </td><td> - </td></tr>
!!<tr><td> \f$ extk \f$  </td><td> extk_lamda(i) </td><td> Extinktionskoeffizient </td><td> 1/m </td><td>  </td></tr>
!!<tr><td> \f$ z \f$  </td><td> dz </td><td> hier Tiefe unter der Wasseroberfläche </td><td> m </td><td> - </td></tr>
!!<tr><td> \f$ a_w(i) \f$  </td><td> aw </td><td> Spektralverteilung Extinktion Wasser  </td><td>  1/m  </td><td>  </td></tr>
!!<tr><td> \f$ a_h(i) \f$  </td><td> ah </td><td> Spektralverteilung Extinktion Humin-Stoffe </td><td> 1/m </td><td>  </td></tr>
!!<tr><td> \f$ a_{c,ki} \f$  </td><td> ack,acg,acb </td><td> Spektralverteilung Extinktion (Kiesel)-Algen </td><td> 1/(m*mgChla/l)) </td><td>  </td></tr>
!!<tr><td> \f$ Chla_{ki} \f$  </td><td>  </td><td> Chlorophylgehalt in Kieselalgen </td><td> µgChla/l </td><td>  </td></tr>
!!<tr><td> \f$ a_s(i) \f$  </td><td> as </td><td>  Spektralverteilung Extinktion Schwebstoffe </td><td> 1/(m*mg/l)) </td><td>  </td></tr>
!!<tr><td> \f$ S_{schweb} \f$  </td><td> ss </td><td> Schwebstoffgehalt </td><td> mg/l </td><td>  </td></tr>
!!<tr><td> \f$ P_{mit} \f$  </td><td> Pmit </td><td> mittlere Produktionsrate infolge Licht - Genuss unter sonst optimalen Bedingungen </td><td>  1/d  </td><td>  ?  </td></tr>
!!<tr><td> \f$ P \f$  </td><td> Pc </td><td> Produktionsrate </td><td>  1/d  </td><td>  ?  </td></tr>
!!<tr><td> \f$ Ic \f$  </td><td> Ic </td><td> Lichtintensität (Photonenstrom) in der betrachteten Schicht </td><td> µE/(s*m²) </td><td>  </td></tr>
!!<tr><td> \f$ h \f$  </td><td> hemm=1-svhemk </td><td> Hemmfaktor Lichtinhibition </td><td> - </td><td> 0-1 </td></tr>
!!<tr><td> \f$ t \f$  </td><td> - </td><td> Zeit </td><td>  </td><td> - </td></tr>
!!<tr><td> \f$ T \f$  </td><td> tempw </td><td> aktuelle Wassertemperatur \n \ref tiefengemittelte_planktische_variable 1 </td><td>  °C </td><td> </td></tr>
!!<tr><td> \f$ {Chla:C} \f$  </td><td> akbcm,agbcm,abbcm </td><td> aktuelles \ref chla der Algen</td><td> mgChla/mgC </td><td>  ?  </td></tr>
!!<tr><td> \f$ a_{Chla,ki} \f$  </td><td> akchl </td><td>  Chlorophyll/Biomasse Verhältnis in Kieselalgen \n \ref globaleParameter </td><td> µgChla/mgBio </td><td>  </td></tr>
!!<tr><td> \f$ CChl^{\star}(T) \f$  </td><td> CChl_Stern </td><td> temperaturabhängiges Referenzverhältnis </td><td> mgChla/mgC </td><td>  </td></tr>
!!<tr><td> \f$ I_{saett}(T) \f$  </td><td> Saettk </td><td> temperatuabhängige Lichtsättigung </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ Ik_{ki} \f$  </td><td> IKke </td><td> Lichtsättigungs-Konstante für Photosynthese der Kieselalgen \n \ref globaleParameter </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ C_{aki} \f$  </td><td> Caki </td><td> Kohlenstoffgehalt der Algenbiomasse (konstant) </td><td> mgC/mgBio </td><td> 0.48 </td></tr>
!!<tr><td> \f$ Y_I \f$  </td><td> yK_I </td><td> Grenzverhältnis </td><td> mgChla/mgC </td><td>  </td></tr>
!!<tr><td> \f$ \tau_{hell/dunkel} \f$  </td><td> KI0,TauI0 </td><td> Aenderungsrate des CHla:C-Verhältnisses im Hellen/Dunklen, nach Cullen (1988) </td><td> 1/h </td><td>  </td></tr>
!!</table>\n\n
!! \n\n zurück: \ref Algen-Wachstum ; Code: algae_huelle.f95

!-----------------------------------------------------------------------------
!> \page Nährstoff_algen Nährstoffe
!! \section nähr_limi Nährstoff-Limitierung des Algen Wachstums
!! Aus der großen Menge der Nähr und Spurenstoffe, die von Algen zum Wachsen benötigen, werden in QSim
!! nur Stickstoff, Phosphor und bei Kieselalgen Silizium als potentiell limitierend angenommen.\n
!! Das Wachstum wird dann jeweils durch den am knappsten vorhandenen Nährstoff limitiert. 
!! (z.B. kann ein Mangel an Phosphor nicht durch einen Überschuss an Silizium ausgeglichen werden)
!! \f[ 
!!      F_{Naehr} = min(F_{N},F_{P},F_{Si})
!! \f]
!! Die Nähstoff-spezifischen Faktoren berechnen sich wie folgt 
!! (hier am Beispiel des Sticktoffs; die Limitierungsfaktoren für Phosphor und Silizium berechnen sich auf analoge Weise):
!! \f[
!!      F_{N} = \frac{Q_{N,Ki}-{Q^{min}}_{N,Ki}}{{Q^{max}}_{N,Ki}-{Q^{min}}_{N,Ki}}
!! \f]
!! Nur für den seltenen Fall, dass \f$ \frac{{Q^{max}}_{N,Ki}}{{Q^{min}}_{N,Ki}} < 1.25 \f$ ist, wird angesetzt: 
!! \f[ 
!!      F_N = \frac {NO3+NH4}{NO3+NH4+(a_{kks}(N) \cdot f_{Ks}(T))}
!! \f] 
!! wobei:
!! \f[ 
!!      f_{Ks}(T) = 1.15^{(20-T)}
!! \f]
!! \section nähr_gehalt Nährstoff-Gehalte der Algen
!! Der Algenbaustein algaeski() berechnet auch die Nährstoffgehalte in den Algen.
!! Im folgenden ist dies wieder anhand der Stickstoffe dargestellt:\n
!! Die Entwicklung des Nährstoffgehaltes ist abhängig von
!! den im Wasser gelösten Nährstoffgehalten \f$ NO3 \f$ und \f$ NH4 \f$, 
!! der tiefenverteilten Wachstumsrate akgrwz(nkz), der Halbsättigungskonstante \f$ a_{kks}(N) \f$,
!! und dem Temperaturkoffizienten \f$ f_{Ks}(T) \f$ (s.o.) . \n
!! Die Entwicklung des Nährstoffgehaltes innerhalb eines Zeitschrittes wird durch die folgende 
!! Differential-Gleichung beschrieben:
!! \f[ 
!!      \frac {\delta Q_{N,Ki}(t)}{\delta t } = - \left( \mu A_{N,Ki} \cdot Q_{N,Ki} \right) + u
!! \f] mit \f[ 
!!      u = \frac { {Q^{max}}_{N,Ki}-Q_{N,Ki} }{{Q^{max}}_{N,Ki}-{Q^{min}}_{N,Ki}} \cdot {up^{max}}_{N,Ki} \cdot \frac {NO3+NH4}{NO3+NH4+(a_{kks}(N) \cdot f_{Ks}(T))}
!! \f]
!! und mittels des runge_kutta() Verfahrens gelöst.
!! \n
!! Die obige Gleichung besagt, dass der Nährstoffgehalt in den Algen infolge ihres Wachstums sinkt und 
!! durch Nährstoffaufnahme steigt. Die Nährstoffaufnahme u ist abhängig von der relativen Nährstoffsättigung der Algen, 
!! ihrer maximalen Aufnamerate und vom vorhandenen Nährstoffgehalt des Wassers.
!! \n
!! So werden der Nährstoffgehalt nach Ablauf des aktuellen Zeitschritts \f$ Q_{N,Ki}(t+dt) \f$ 
!! und die tiefenverteilte Aufnahmerate \f$ {up}_{N,Ki}(z) \f$ bestimmt
!! und an die \ref Stickstoff -Bilanz im Stickstoffbaustein ncyc() weitergegebn.\n
!! <hr>mit:\n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ F_{Naehr} \f$  </td><td> F5 </td><td> Faktor der Nährstofflimitation des \ref Algen-Wachstum s</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ Q_{N,Ki} \f$  </td><td> Q_NK </td><td> aktueller Stickstoffanteil der Kiesel-Algenbiomasse \n \ref planktische_variablen 30 </td><td> mgN/mgBio </td><td>  </td></tr>
!!<tr><td> \f$ {Q^{max}}_{N,Ki} \f$  </td><td> Qmx_NK </td><td> max. N-Gehalt der Kieselalgen \n \ref globaleParameter </td><td> mgN/mgBio </td><td>  </td></tr>
!!<tr><td> \f$ {Q^{min}}_{N,Ki} \f$  </td><td> Qmn_NK </td><td> min. N-Gehalt der Kieselalgen \n \ref globaleParameter </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ NO3 \f$  </td><td> VNO3 </td><td> Nitrat \n \ref planktische_variablen 5 </td><td> mgN/l </td><td>  </td></tr>
!!<tr><td> \f$ NH4 \f$  </td><td> VNH4 </td><td> Ammonium \n \ref planktische_variablen 3 </td><td> mgN/l </td><td>  </td></tr>
!!<tr><td> \f$ F_{N},F_{P},F_{Si} \f$  </td><td> F51,F52,F53 </td><td> Limitationsfaktoren für Stickstoff, Phosphor und Silizium </td><td> - </td><td> 0-1 </td></tr>
!!<tr><td> \f$ f_{Ks}(T) \f$  </td><td> fT_Ks </td><td> Temperaturkoeffizient </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ T \f$  </td><td> TEMPW </td><td> aktuelle, lokale Wassertemperatur \n \ref planktische_variablen 1 </td><td> °C </td><td>  </td></tr>
!!<tr><td> \f$ t \f$  </td><td> - </td><td> Zeit </td><td> - </td><td>  </td></tr>
!!<tr><td> \f$ a_{kks}(N) \f$  </td><td> akksN, akksp, akkssi</td><td> Halbsättigungskonstante für N,P,Si bei Kieselalgen \n \ref globaleParameter </td><td> mgN/l </td><td>  </td></tr>
!!<tr><td> \f$ \mu A \f$ </td><td> akgrwz(nkz) </td><td> tiefenverteilte Kiesel-Algen-Wachstumsrate \n siehe: \ref Veränderung-Algenmasse \n interne Variable</td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ {up}_{N,Ki}(z) \f$  </td><td> up_NKz </td><td> Aufnahmerate Stickstoff der Kieselalgen \n \ref tiefenaufgelöste_übergabe_variable 1 </td><td> mgN/(l*d) </td><td> ? </td></tr>
!!<tr><td> \f$ {up^{max}}_{N,Ki}\f$  </td><td> upmxNK </td><td> maximale Aufnahmerate Stickstoff der Kieselalgen \n \ref globaleParameter </td><td> mgN/(l*d) </td><td> ? </td></tr>
!!</table>\n\n
!! zurück: \ref Algen-Wachstum ; Quelle: algae_huelle.f95
! 
!-----------------------------------------------------------------------------
!> \page Temperatur_algen Temperatur
!! Die Wachstumsraten von Algen steigen bis zu einer Optimaltemperatur an. Darüber fallen sie wieder ab.
!! Oberhalb ihrer Letaltemperatur wachsen Algen gar nicht mehr.\n
!! In QSim 13.10 vom 20.3.2014 ist fogende Funktion für den Faktor der Temperaturabhängigkeit implementiert:\n
!! \f[ 
!!      FTA = {\hat{T}}^W \cdot e^{ W \cdot (1-\hat{T})} 
!! \f]
!! mit: \n
!! \f[ 
!!      \hat{T}= \frac{T_{MAX}-T}{T_{MAX}-T_{OPT}}
!! \f]
!! und \n
!! \f[ 
!!      W = 0.0025 \cdot \left(0.61519(T_{MAX}-T_{OPT})\right)^2 \cdot \left(1+\sqrt{1+\frac{40}{0.61519(T_{MAX}-T_{OPT})} }\right)^2
!! \f]
!! W=1.51 für Kieselalgen mit Topt=20 und Tmax=31\n
!! \image html FTA.svg 
!! \n 
!! <hr>mit:\n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ FTA \f$  </td><td> FTA </td><td> Faktor Temperaturabhängigkeit \ref Algen-Wachstum </td><td> - </td><td> 0-1 </td></tr>
!!<tr><td> \f$ T \f$  </td><td> TEMPW </td><td> aktuelle, lokale Wassertemperatur \n \ref planktische_variablen 1 </td><td> °C </td><td>  </td></tr>
!!<tr><td> \f$ T_{MAX} \f$  </td><td> TMAXK,TMAXG,TMAXB </td><td> Letal-Temperatur für das Algenwachstum (Kiesel,Grün,Blau) \n \ref globaleParameter  </td><td> °C </td><td> ?</td></tr>
!!<tr><td> \f$ T_{OPT} \f$  </td><td> TOPTK,TOPTG,TOPTB </td><td> Optimal-Temperatur (Kiesel,Grün,Blau) \n \ref globaleParameter</td><td> °C </td><td> ? </td></tr>
!!</table>
!! \n\n
!! zurück: \ref Algen-Wachstum ; Code: algae_huelle.f95

!-----------------------------------------------------------------------------
!> \page  Algen-Respiration Algen-Respiration
!! 
!! Die Respiration stellt einen wichtigen Verlustterm für die Algenbiomasse dar.\n
!! Die Abnahmerate \f$ resp_i \f$ welche in die Berechnung des \ref Algen-Wachstum s eingeht,\n
!! setzt sich aus einer temperaturabhängigen Grundrespiration und einer,\n
!! durch das Wachstum hervorgerufenen, Zusatzrespiration zusammen.\n
!! \f[ 
!!      resp_i = {a^{min}}_{res,ki} \cdot f(T) + \mu A \cdot f_{res}
!! \f]
!! mit dem Temperaturfaktor\n
!! \f[ 
!!     f(T)= 0.75 \cdot 1.108^{T-15}
!! \f] 
!! für T<15° und \n
!! \f[ 
!!     f(T)= \frac{1.5}{1+\left((T-32)/17\right)^2} 
!! \f] für T>15° \n
!! <hr>mit:\n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ resp_i \f$  </td><td> akres, agres, abres </td><td> Rate der \ref Algen-Respiration i=ki,gr,bl </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ {a^{min}}_{res,ki} \f$  </td><td> akremi </td><td> Grundrespiration d. Kieselalgen \n \ref globaleParameter </td><td>  1/d  </td><td>  </td></tr>
!!<tr><td> \f$ \mu A \f$  </td><td> akgrow </td><td>  Rate des Kiesel- \ref Algen-Wachstum </td><td> 1/d </td><td>  </td></tr>
!!<tr><td> \f$ f_{res} \f$  </td><td> frespg=frmuke </td><td> Anteil der vom Wachstum abhängigigen Respiration \n \ref globaleParameter </td><td>  </td><td>  </td></tr>
!!</table>
!! \n\n
!! zurück: \ref Algen-Wachstum  ; Code: algae_huelle.f95

!! <code>\verbatim
!! \endverbatim</code>

!-----------------------------------------------------------------------------
!> \page Algen-Mortalität Algen-Mortalität
!! Das Absterben der Algen stellt einen Verlustterm bei der Berechnung des \ref Algen-Wachstum s dar.
!! \n\n
!! Es wird als konstante Rate \f$ mor_i = 0.02 \f$ pro Tag angesetzt.\n
!! In Fällen von Nährstoffmangel 
!! \f$ f_{mor,N}=\frac{Q_{N,Ki}-{Q^{min}}_{N,Ki}}{{Q^{max}}_{N,Ki}-{Q^{min}}_{N,Ki}} < 0.6\f$
!! (Die anderen Nährstoffmangel-Faktoren werden in gleicher Weise berechnet; 
!! Der am stärksten limitierende Nährstoff ,d.h. kleinste Faktor, wird für die Berechnung der Mortalität verwendet.)
!! vegrößert sich die Mortalität nach folgendem Ansatz:
!! \f[ 
!!     mor_i = 0.02+((0.02/0.6)*(0.6-f_{mor}))
!! \f] 
!! \n
!! <code>\verbatim
!! !     neu!! algenmortalitaet                             
!!       akmor = 0.02 
!!       akmoma = 0.02 
!!       fmor1 = f51
!!       fmor2 = f52
!!       fmor3 = f53
!!       if((Qmx_NK/Qmn_Nz).lt.1.25)goto 580
!!      fmor1 = (Q_NK(ior)-Qmn_NK)/(Qmx_NK-Qmn_NK)
!!   580 if((Qmx_PK/Qmn_Pz).lt.1.25)goto 582
!!       fmor2 = (Q_PK(ior)-Qmn_PK)/(Qmx_PK-Qmn_PK)
!!   582 if((Qmx_SK/Qmn_Sz).lt.1.25)goto 585
!!       fmor3 = (Q_SK(ior)-Qmn_SK)/(Qmx_SK-Qmn_SK)
!!   585 fmor = fmor1
!!       if(fmor2.lt.fmor)fmor = fmor2
!!       if(fmor3.lt.fmor)fmor = fmor3
!!       if(fmor.le.0.6)akmor = 0.02+((akmoma/0.6)*(0.6-fmor))
!!      dkimor(ior) = aki(ior)*(1.-(exp(-akmor*tflie)))
!! \endverbatim </code> \n
!! <hr>mit:\n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ {\Delta mor}_i \f$  </td><td> dkimor, dgrmor, dblmor </td><td> Abnahme infolge Mortalität, i=ki,gr,bl pro Zeitschritt </td><td>  mg/l  </td><td>  </td></tr>
!!<tr><td> \f$ mor_i \f$  </td><td> akmor, agmor, abmor </td><td> Rate der Algen-Mortalität i=ki,gr,bl </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{N,Ki} \f$  </td><td> Q_NK </td><td> aktueller Stickstoffanteil der Kiesel-Algenbiomasse \n \ref planktische_variablen 30 </td><td> mgN/mgBio </td><td>  </td></tr>
!!<tr><td> \f$ {Q^{max}}_{N,Ki} \f$  </td><td> Qmx_NK </td><td> max. N-Gehalt der Kieselalgen \n \ref globaleParameter </td><td> mgN/mgBio </td><td>  </td></tr>
!!<tr><td> \f$ {Q^{min}}_{N,Ki} \f$  </td><td> Qmn_NK </td><td> min. N-Gehalt der Kieselalgen \n \ref globaleParameter </td><td>  </td><td>  </td></tr>
!!</table>\n\n
!! zurück: \ref Algen-Wachstum ; Code: algae_huelle.f95

!-----------------------------------------------------------------------------
!> \page Algen-Sedimentation Sedimentation von lebenden Algen
!! 
!! Auch lebende Algen können durch Absinken auf die Sohle der Wassersäule entzogen werden.\n
!! \n
!! Zunächst wird die Schubspannungsgeschwindigkeit nach der Formel von Gauckler/Manning/Strickler berechnet:\n
!! <code>\verbatim
!!      FN = 1./RAU(ior) 
!!      G = 9.81 
!!      UST = ((FN*G**0.5)/TIEFE(ior)**0.166667)*abs(VMITT(ior)) 
!! \endverbatim</code>
!! \f[ 
!!    u_* = \sqrt{ \frac{g \cdot (\bar{u})^2}{ (K_{st})^2 \cdot h^{1/3} }  }
!! \f]
!! zusätzlich kann auch noch der Schiffseinfluss berücksichtigt werden (hier formelmäßig noch nicht aufgeführt)
!! 
!! SEDALk  =  askie*aki*(1-qsgr) * (1.-1./(EXP(0.53*wst*dt*86400./h)) ) 
!! wst = 10**((log(1./qssed)-log(aseda))/(-bseda)) * ( 1.14*exp(-188.2*ust) )
!!
!! <code>\verbatim
!!       aseda = 6.67e-7 
!!       bseda = 2.78 
!!       akis = askie*aki(ior) 
!!       prop = 0.53 
!! !                                                                       
!!       wsgr = 0.625*ust**2.1 
!!       qsgr = 1./(1+aseda*exp(-bseda*alog10(wsgr))) 
!!       ceq = akis*qsgr 
!!       qssed = ((1+qsgr)/2. ) - 1.0 !!wy
!!       if(qssed .le. 0.0) qssed = 0.000000001 !!wy
!!       ws = (log(1./qssed)-log(asedn))/(-bsedn)  !!wy
!!       !!wy qssed = (1+qsgr)/2. 
!!       !!wy ws = (log(1./qssed-1)-log(aseda))/(-bseda) 
!!       ws = 10**ws 
!! !                                                                       
!!       fwst = 1.14*exp(-188.2*ust) 
!!       if(fwst.gt.1.)fwst = 1. 
!! !                                                                       
!!       wst = ws*fwst 
!!       Oc = 1./(EXP(prop*wst*TFLIE*86400./TIEFE(ior))) 
!!       oc = 1.-Oc 
!!       akist = akis-ceq 
!!       SEDALk(ior) = akist*oc 
!! \endverbatim</code>
!!
!! <hr> aus orgc:
!!      ASEDC = 1.44E-6  \n 
!!      BSEDC = 3.13  \n 
!!      wsgr = 0.625*ust**2.1  \n 
!!      qsgr = 1./(1+asedc*exp(-bsedc*alog10(wsgr)))  \n 
!! \f[
!!    q_{s,gr} = \frac{1}{ 1 + (1.44 \cdot 10^{-6}) \cdot e^{ -3.13 \cdot \log_{10}( 0.625 \cdot {u_*}^{2.1}) } }
!! \f]
!!       qssed = (1+qsgr)/2. \n
!!       WS = (LOG(1./QSSED-1)-LOG(ASEDC))/(-BSEDC) \n
!!       WS = 10**WS \n
!!       fwst = 1.14*exp(-188.2*ust) \n
!!       if(fwst>1.0)fwst = 1. \n
!!       wst = ws*fwst \n
!! \f[
!!    w_* = \left(1.14 \cdot  e^{(-188.2 \cdot u_*)} \right) \cdot 
!! 10^{ \left( \log( \frac{2.}{1+q_{s,gr}}-1) - \log(1.44 \cdot 10^{-6})\right) / -3.13 }
!! \f]
!!       prop = 0.6  \n
!!       OC = 1./(EXP(prop*WST*TFLIE*86400./TIEFE(ior)))  \n
!!       OC = 1.-OC  \n
!!       CP1sd = fbsgr(ior)*CP(1,ior)  \n
!!       Ceq1 = CP1sd*qsgr  \n
!!       CP1sdt = CP1sd-Ceq1  \n
!!       sedCP1 = CP1sdt*oc  \n
!!       CPt(1) = CPt(1)-sedCP1  \n  
!! exemplarisch:                                                              
!! \f{eqnarray*}{
!!  sedCP_1 &=& 
!! - \frac{ \left(f_{bs,gr} \cdot CP_1 \cdot q_{s,gr}\right)}{\Delta } \cdot 
!!   \left(1.- 1./e^{(0.6 \cdot w_* \cdot \Delta t \cdot 86400 / h)} \right) \\
!! \f}
!! <hr>
!! \n\n
!! <hr>mit:\n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ sedalk \f$  </td><td> sedalk </td><td>  </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ askie \f$  </td><td> askie	</td><td> \ref globaleParameter Sedimentierbarer Anteil an Kieselalgen </td><td>  </td><td>  </td></tr>
!! zurück: \ref Algen-Wachstum ; Code: algae_huelle.f95

!-----------------------------------------------------------------------------
!> \page Algen-Konsum-planktisch Konsum durch planktische Lebewesen
!! Bezüglich des Konsums von Algen werden
!! in QSim stellvertretend für das gesamte Zooplankton die \ref rotatorien modelliert. (konsum.f90)
!! \n<i> #### in QSim3D z.Z. noch nicht in Betrieb #### </i>\n
!! \n\n
!! <hr>mit:\n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ algzok \f$  </td><td> algzok </td><td> \ref Algen-Konsum-planktisch </td><td> mg/l </td><td>  </td></tr>
!!</table>\n\n
!! zurück: \ref Algen-Wachstum ; Code: algae_huelle.f95

!-----------------------------------------------------------------------------
!> \page Algen-Konsum-bentisch Konsum durch benthische Lebewesen (Muscheln)
!! 
!! Auch manche benthischen Lebewesen konsumieren Algen. \n
!! In QSim wird die Filtration durch  \n
!! die Zebramuschel Dreissena polymorpha (dreissen.f90) <i>(z.Z. noch nicht in Betrieb)</i> und \n
!! den Schlickkrebs Chelicorophium curvispinum (COROPH.f90) <i>(noch im Aufbau)</i> modelliert.
!! \n\n
!! <hr>mit:\n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ algdrk \f$  </td><td> algdrk </td><td> Fraßrate Dreissena(Muscheln) </td><td> mg/l </td><td>  </td></tr>
!!<tr><td> \f$ algcok \f$  </td><td> algcok </td><td> Fraßrate Corophium </td><td> mg/l </td><td>  </td></tr>
!!</table>\n\n
!! zurück: \ref Algen-Wachstum ; Code: algae_huelle.f95

!-----------------------------------------------------------------------------
!> \page Algen-Abspülung Abspülung benthischer Algen
!! 
!! Benthische (an der Sohle haftende) Kiesel und Grünalgen werden z. T. von der Strömung abgespült und vergrößern so
!! die Menge der planktischen (im Wasser treibenden) Algenmenge.\n
!! ### MOMENTAN ist albenth() noch nicht angebunden  ### 
!! \n\n
!! <code>\verbatim
!! albenth.f90:      cmatki(ior) = 0.0
!! albenth.f90:!      cmatki(ior) = hconC*(vabfl(ior)/Vol)*abeki
!! albenth.f90:!      cmatki(ior) = cmatki(ior)*tflie*86400.
!! \endverbatim</code>
!! \n\n
!! mit:\n <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ cmatki \f$  </td><td> cmatki </td><td> Abspülung von benthischen Algen pro Zeitschritt </td><td>  </td><td>  </td></tr>
!!</table>\n\n
!! zurück: \ref Algen-Wachstum  ; Code: algae_huelle.f95

!-----------------------------------------------------------------------------
!> \page algenaufteilung Aufteilung Algen-Variablen im Zufluss
!! jetzt mittels algae_start() in zuflussrand.f90 \n \n
!! <hr> folgendes Code-Fragment aus Version 13.30\n\n
!!    Ausgangspunkt sind die Angaben: \n
!!      !planktonic_variable(11+nk) = rabe(zaehl)%wert_jetzt(13)  !  CHLA   Chorophyl-a  \n
!!      !planktonic_variable(19+nk) = rabe(zaehl)%wert_jetzt(14)  !  VKIGR  Anteil Kieselalgen (falls unbekannt 0) \n
!!      !planktonic_variable(20+nk) = rabe(zaehl)%wert_jetzt(15)  !  ANTBL  Anteil Blaualgen (falls unbekannt 0) \n
!! \n\n
!! chlaz = chla in allen Schichten, d.h. Tiefenverteilung konstant -> plankt_vari_vert(l+(11-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) \n
!! akiz, agrz, ablz ebenso
!! \n\n
!!    ! agbcm:    planktonic_variable(25+nk) = (agchl/(1000.*Cagr))*fTChlC ! agbcm \n
!!    ! akbcm:    planktonic_variable(24+nk) = (akchl/(1000.*Caki))*fTChlC ! akbcm \n
!!    ! abbcm:    planktonic_variable(26+nk) = (abchl/(1000.*Cabl))*fTChlC ! abbcm \n
!! \n\n
!! aki = (chla*VKIGR)/(1000*akbcm*Caki) -> 
!! planktonic_variable( 8+nk) =  planktonic_variable(11+nk)*planktonic_variable(19+nk)/(1000.*planktonic_variable(24+nk)*Caki) \n
!! agr = (chla*(1-VKIGR-ANTBL))/(1000*agbcm*Cagr)) -> 
!! planktonic_variable( 9+nk)=planktonic_variable(11+nk)*(1.-planktonic_variable(19+nk)-planktonic_variable(20+nk))/(1000.*planktonic_variable(25+nk)*Cagr) \n
!! abl = (chla*ANTBL)/(1000*abbcm*Cabl) -> 
!! planktonic_variable(10+nk)=planktonic_variable(11+nk)*planktonic_variable(20+nk)/(1000.*planktonic_variable(26+nk)*Cabl)
!! \n\n
!! chlaki = chla * vkigr -> planktonic_variable(12+nk) = planktonic_variable(11+nk)*planktonic_variable(19+nk) \n
!! chlagr = chla * (1-vkigr-antbl) -> planktonic_variable(13+nk) = planktonic_variable(11+nk)*(1.-planktonic_variable(19+nk)-planktonic_variable(20+nk)) \n
!! chlabl = chla * antbl -> planktonic_variable(14+nk) = planktonic_variable(11+nk)*planktonic_variable(20+nk)
!! \n\n
!!      planktonic_variable(21+nk)= 0.01 ! svhemk ### unklar ### \n
!!      planktonic_variable(22+nk)= 0.01 ! svhemg ### unklar ### \n
!!      planktonic_variable(23+nk)= 0.01 ! svhemb ### unklar ### \n
!!      planktonic_variable(27+nk)= 0.01 ! akiiv ### unklar ### \n
!!      planktonic_variable(28+nk)= 0.01 ! agriv ### unklar ### \n
!!      planktonic_variable(29+nk)= 0.01 ! abliv ### unklar ### \n
!! \n \n
!! Quelle: algae_huelle.f95 , zurück: \ref zuflussranddaten, \ref Algen oder \ref Anfangsbedingungen

!!<tr><td> \f$  \f$  </td><td>  </td><td>  </td><td>  </td><td>  </td></tr>
!
!> SUBROUTINE algae_huelle()
!!\n zurück: \ref Stoffumsatz oder \ref Algen ; Code: algae_huelle.f95
      SUBROUTINE algae_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none
      integer :: i,j,k,nk,i2,string_write_error
      integer :: ieros_flag
      logical error

      !if(i==1)print*,'algae_huelle läuft an'
      iglob=(i+meinrang*part)

      do k=1,number_trans_quant
         if(isnan(transfer_quantity_p(k+(i-1)*number_trans_quant)))then
            print*,'vorher: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang=',meinrang
            if(meinrang==0)print*,'trans_quant_name:',trans_quant_name(k)
         endif
      end do
      do j=1,num_lev_trans
         do k=1,number_trans_quant_vert
            if(isnan(trans_quant_vert_p(j+(k-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)))then
               print*,'algaes** vorher: isnan(trans_quant_vert_p  node#',iglob,' level#', j,' variable# ',k
               if(meinrang==0)print*,'trans_quant_vert_name:',trans_quant_vert_name(k)
            endif
         end do
      end do

      nk=(i-1)*number_plankt_vari
      i2=zone(point_zone(iglob))%wettstat%wetterstations_nummer !! ist parallel !!!
      kontroll = iglob.eq.kontrollknoten
      !if(i.eq.1)print*,'algae_huelle: lesen von e_extnct.dat (algaeski) bei jedem Knoten ist noch viiieel zu umständlich ###'
      if(kontroll)print*,'algae_huelle: nk,i,iglob=',nk,i,iglob

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe: 

      schwi(1)=schwi_T(i2)    ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
      schwi(2)=schwi(1)
      tflie = real(dt)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
      tempw(1)=planktonic_variable_p(1+nk)    ! Wasser-Temperatur
      tempw(2)=tempw(1)

      do j=1,num_lev
         tempwz(j,1)= plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Wassertemperatur tiefenaufgelöst
         tempwz(j,2) = tempwz(j,1)
      end do

      tiefe(1:2)= rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
      rau(1:2)= strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
      vmitt(1) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag; randbedingungen.h
      vmitt(2) = vmitt(1)
      vno3(1) = planktonic_variable_p(5+nk)  ! nitrat
      vno3(2) = vno3(1)
      vNH4(1) = planktonic_variable_p(3+nk)  ! ammonium
      vNH4(2) = vNH4(1)
      gelp(1) = planktonic_variable_p( 6+nk)  ! gelöster ortho-Phosphat-Phosphor tiefengemittelt
      gelp(2) = gelp(1)
      svhemk(1) = planktonic_variable_p(21+nk)  ! Mittelwertbildung der Licht-Hemmung ?
      svhemk(2) = svhemk(1)
      svhemg(1) = planktonic_variable_p(22+nk)  ! Mittelwertbildung der Licht-Hemmung ?
      svhemg(2) = svhemg(1)
      svhemb(1) = planktonic_variable_p(23+nk)  ! Mittelwertbildung der Licht-Hemmung ?
      svhemb(2) = svhemb(1)

      CHLA(1) = planktonic_variable_p(11+nk)  ! Chlorophyl-A
      CHLA(2) = CHLA(1)
      ssalg(1:2) = planktonic_variable_p(52+nk) ! GESAMTSCHWEBSTOFFE incl. lebender Organismen, messbar, Randwert
      ir(1) = transfer_quantity_p(42+(i-1)*number_trans_quant) ! Ingestionsrate der Rotatorien in mg/(l*h) | konsum() (unbenutzt)
      ir(2) = ir(1)

      si(1) = planktonic_variable_p( 7+nk)  ! silikat-Silizium-Konzentration (tiefengemittelt)
      si(2) = si(1)
      dalgki(1) = transfer_quantity_p(20+(i-1)*number_trans_quant) ! Zuwachs Kiesel-Algen
      dalgki(2) = dalgki(1)
      dalggr(1) = transfer_quantity_p(21+(i-1)*number_trans_quant) ! Zuwachs grün-Algen
      dalggr(2) = dalggr(1)
      dalgbl(1) = transfer_quantity_p(22+(i-1)*number_trans_quant) ! Zuwachs blau-Algen
      dalgbl(2) = dalgbl(1)
      dalgak(1) = transfer_quantity_p(23+(i-1)*number_trans_quant) ! Respiration Kiesel-Algen
      dalgak(2) = dalgak(1)
      dalgag(1) = transfer_quantity_p(24+(i-1)*number_trans_quant) ! Respiration grün-Algen
      dalgag(2) = dalgag(1)
      dalgab(1) = transfer_quantity_p(25+(i-1)*number_trans_quant) ! Respiration blau-Algen
      dalgab(2) = dalgab(1)
      flag(1)=0         ! keine Einleitungen
      flag(2)=flag(1)
      elen(1)=1         ! Elementlänge (nicht verwendet)
      elen(2)=elen(1)
      ior=1             ! Laufindex
      anze=1            ! Anzahl der Profile im aktuellen Strang

      sedalk(1) = benthic_distribution_p(26+(i-1)*number_benth_distr) ! Sedimentierte Menge an Kiesel-Algen  benthische_verteilungen.f95
      sedalk(2) = sedalk(1)
      sedalg(1) = benthic_distribution_p(27+(i-1)*number_benth_distr) ! Sedimentierte Menge an Grün-Algen
      sedalg(2) = sedalg(1)
      sedalb(1) = benthic_distribution_p(28+(i-1)*number_benth_distr) ! Sedimentierte Menge an Blau-Algen
      sedalb(2) = sedalb(1)
      algzok(1) = transfer_quantity_p(53+(i-1)*number_trans_quant) ! kiesel-Algen-Konsum Zoo-Plankton in mg/l
      algzok(2) = algzok(1)
      algzog(1) = transfer_quantity_p(72+(i-1)*number_trans_quant) ! grün-Algen-Konsum Zoo-Plankton in mg/l
      algzog(2) = algzog(1)
      algzob(1) = transfer_quantity_p(73+(i-1)*number_trans_quant) ! blau-Algen-Konsum Zoo-Plankton in mg/l
      algzob(2) = algzob(1)
      echla(1) = 0.0     ! keine Einleitung
      qeinl(1)= 0.0      ! kein Abfluss Einleitung
      vabfl(1) = 0.0     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
      vabfl(2) = vabfl(1)

      dkimor(1) = transfer_quantity_p(7+(i-1)*number_trans_quant) ! Absterberate Kieselalgen
      dkimor(2) = dkimor(1)
      dgrmor(1) = transfer_quantity_p(8+(i-1)*number_trans_quant) ! Absterberate Grünalgen
      dgrmor(2) = dgrmor(1)
      dblmor(1) = transfer_quantity_p(9+(i-1)*number_trans_quant) ! Absterberate Blaualgen
      dblmor(2) = dblmor(1)
      fkm (1)=0.0      ! Flusskilometer (unbenutzt)
      jiein(1)=0       ! keine Punkt-Einleitungen
      evkigr(1) = 0.0  ! Einleitungswert (keine Einleitungen in T-QSim)
      vkigr(1) = planktonic_variable_p(19+(i-1)*number_plankt_vari) ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ??
      vkigr(2) = vkigr(1)
      antbl(1) = planktonic_variable_p(20+(i-1)*number_plankt_vari) ! Anteil der Blaualgen am Gesamt-Chlorophyll-a
      antbl(2) = antbl(1)
      eantbl(1) = 0.0  ! Einleitungswert (keine Einleitungen in T-QSim)

      ! akchl, agchl, abchl ; Biomasse/Chlorophyll Verhältnis in Algen, direkt aus QSimDatenfelder | Aparam.txt
      ! akgmax, aggmax, abgmax ; Max. Wachstumsate d. Algen, direkt aus QSimDatenfelder | Aparam.txt
      ! akksn, agksn, abksn ; N-Halbsättigung Algen, direkt aus QSimDatenfelder | Aparam.txt
      ! akksp, agksp, abksp; P-Halbsättigung Algen, direkt aus QSimDatenfelder | Aparam.txt
      ! akkssi ; Si-Halbsättigung Kieselalgen, direkt aus QSimDatenfelder | Aparam.txt
      saettk = transfer_value_p(6)    ! Rückgabewert ???
      saettg = transfer_value_p(8)    ! ???
      saettb = transfer_value_p(9)    ! ???
      ! akremi, agremi, abremi ; Grundrespiration d. Algen, direkt aus QSimDatenfelder | Aparam.txt
      akrema = 0.0 ! unbenutzt
      !if(kontroll)print*,'algae_huelle: PCmax =',(akgmax+akremi)/(1.-frmuke) ! max C-spezifische Photosyntheserate bei optimal Temperatur

      sbioki = 0.0 ! unbenutzt
      vco2(1) = transfer_quantity_p(26+(i-1)*number_trans_quant) ! Kohlendioxyd ! unbenutzt
      vco2(2) = vco2(1)
      ! iph ! mit/ohne ph-Wertberechnung  kommt aus modul modell
      akbcm(1) = planktonic_variable_p(24+nk) ! Verhältnis Chlorophyll-a zu Kohlenstoff Kieselalgen
      akbcm(2) = akbcm(1)
      agbcm(1) = planktonic_variable_p(25+nk) ! Verhältnis Chlorophyll-a zu Kohlenstoff Gruenalgen
      agbcm(2) = agbcm(1) ! nur Einleitung
      abbcm(1) = planktonic_variable_p(26+nk) ! Verhältnis Chlorophyll-a zu Kohlenstoff der blau-Algen
      abbcm(2) = abbcm(1) ! nur Einleitung
      aki(1) = planktonic_variable_p(8+nk) ! Biomasse an Kiesel-Algen
      aki(2) = aki(1)
      agr(1) = planktonic_variable_p(9+nk) ! Biomasse an gruen-Algen
      agr(2) = agr(1)
      abl(1) = planktonic_variable_p(10+nk) ! Biomasse an Blau-Algen
      abl(2) = abl(1)
      extk(1) = transfer_quantity_p(54+(i-1)*number_trans_quant) ! mittlerer Extinktionskoeffizient
      extk(2) = extk(1)

      ilamda=rb_extnct_ilamda
      !if(kontroll)print*,'algae_huelle: ilamda=',ilamda,' anz_extnct_koeff=',anz_extnct_koeff
      do j=1,ilamda                                                    
         eta(j) = rb_extnct_p(1 + (j-1)*anz_extnct_koeff)
         !if(kontroll)print*,'eta(',j,')=',eta(j)
         aw(j)  = rb_extnct_p(2 + (j-1)*anz_extnct_koeff)
         ack(j) = rb_extnct_p(3 + (j-1)*anz_extnct_koeff)
         acg(j) = rb_extnct_p(4 + (j-1)*anz_extnct_koeff)
         acb(j) = rb_extnct_p(5 + (j-1)*anz_extnct_koeff)
         ah(j)  = rb_extnct_p(6 + (j-1)*anz_extnct_koeff)
         as(j)  = rb_extnct_p(7 + (j-1)*anz_extnct_koeff)
         al(j)  = rb_extnct_p(8 + (j-1)*anz_extnct_koeff)
         ! extk_lamda wird nur von algaeski an algaesbl und algaesgr übergeben. an jedem Knoten hier in algae_huelle
         !extk_lamda(j,1) = rb_extnct_p(9 + (j-1)*anz_extnct_koeff) ! eigentlich nur Rückgabewert
         extk_lamda(j,1) = 0.0 ! initialize
         !if(kontroll)print*,'extk_lamda(',j,',1)=',extk_lamda(j,1)
      end do
      !if(kontroll)print*,'eta(',ilamda,')=',eta(ilamda),' aw(',ilamda,')=',aw(ilamda),' meinrang',meinrang

      uhrz=uhrzeit_stunde     ! Uhrzeit module::modell zeitsekunde()
      sised(1) = benthic_distribution_p(2+(i-1)*number_benth_distr) ! Siliziumgehalt im Sediment
      sised(2) = sised(1)
      tpki(1) = transfer_quantity_p(55+(i-1)*number_trans_quant) ! Ausgabeparameter ?? Kieselalgen Phosphor ??
      tpki(2) = tpki(1)
      tpgr(1) = transfer_quantity_p(80+(i-1)*number_trans_quant) ! Ausgabeparameter ?? Grünalgen Phosphor ??
      tpgr(2) = tpgr(1)
      tpbl(1) = transfer_quantity_p(81+(i-1)*number_trans_quant) ! Ausgabeparameter ?? Blaualgen Phosphorhemmung ??
      tpbl(2) = tpbl(1)
      iwied=0      ! unbenutzte Variable
      akmuea(1) = transfer_quantity_p(56+(i-1)*number_trans_quant) ! Wachstumsrate Ausgabeparameter algaeski()
      akmuea(2) = akmuea(1)
      agmuea(1) = transfer_quantity_p(84+(i-1)*number_trans_quant) ! Wachstumsrate Ausgabeparameter algaesgr()
      agmuea(2) = agmuea(1)
      abmuea(1) = transfer_quantity_p(85+(i-1)*number_trans_quant) ! Wachstumsrate Ausgabeparameter algaesbl()
      abmuea(2) = abmuea(1)
      ftaaus(1) = transfer_quantity_p(57+(i-1)*number_trans_quant) ! Ausgabeparameter algaeski() fta
      ftaaus(2) = ftaaus(1)
      fiaus(1) = transfer_quantity_p(58+(i-1)*number_trans_quant) ! Ausgabeparameter algaeski() Pmit/(Pmax*3.6) 
      fiaus(2) = fiaus(1)
      figaus(1) = transfer_quantity_p(82+(i-1)*number_trans_quant) ! Ausgabeparameter algaesgr() Pmit/(Pmax*3.6) 
      figaus(2) = figaus(1)
      fibaus(1) = transfer_quantity_p(83+(i-1)*number_trans_quant) ! Ausgabeparameter algaesbl() Pmit/(Pmax*3.6) 
      fibaus(2) = fibaus(1)
      fheaus(1) = transfer_quantity_p(59+(i-1)*number_trans_quant) ! Ausgabeparameter algaeski() svhemk
      fheaus(2) = fheaus(1)
      fhegas(1) = transfer_quantity_p(86+(i-1)*number_trans_quant) ! Ausgabeparameter algaesi() svhemg
      fhegas(2) = fhegas(1)
      fhebas(1) = transfer_quantity_p(87+(i-1)*number_trans_quant) ! Ausgabeparameter algaes() svhemb
      fhebas(2) = fhebas(1)

      akraus(1) = transfer_quantity_p(60+(i-1)*number_trans_quant) ! Ausgabe akbcm algaeski()
      akraus(2) = akraus(1)
      agreau(1) = transfer_quantity_p(88+(i-1)*number_trans_quant) ! Ausgabe agbcm algaesgr()
      agreau(2) = agreau(1)
      abreau(1) = transfer_quantity_p(89+(i-1)*number_trans_quant) ! Ausgabe von abbcm (Chlorophyll-a zu Kohlenstoff der blau-Algen) algaesbl() 
      abreau(2) = abreau(1)
      tauscs = transfer_value_p(7)    ! Schiffseinfluss     qsim.f90: tauscs = 1.25
      ischif = zone(point_zone(iglob))%schiff%schifffahrts_zone ! schifffahrt in dieser module::zonen ; 1->Schiffsverkehr  , 0-> kein Schiffsverkehr; MODELLG.txt "F"
      ilbuhn = 0          ! keine Buhnen
      ieros_flag = ieros ! Erosionsflag unbenutzt

      ! zakie,zagre,zable ! Filtrierbarkeit der Algen in den Algenroutinen unbenutzt

      ! askie, ASGRE, asble, Sedimentierbarer Anteil der Algen; jetzt direkt aus QSimDatenfelder | APARAM.txt

      cmatki(1)=benthic_distribution_p(9+(i-1)*number_benth_distr) ! Abspülung benthischer kiesel-Algen
      cmatki(2) = cmatki(1)
      cmatgr(1)=benthic_distribution_p(10+(i-1)*number_benth_distr) ! Abspülung benthischer grün-Algen
      cmatgr(2) = cmatgr(1)
      algdrk(1)=benthic_distribution_p(38+(i-1)*number_benth_distr) ! \ref Algen-Konsum-bentisch (Muscheln) in mg/l
      algdrk(2) = algdrk(1)
      algdrg(1)=benthic_distribution_p(40+(i-1)*number_benth_distr) ! grün-Algen-Konsum-bentisch (Muscheln) in mg/l
      algdrg(2) = algdrg(1)
      algdrb(1)=benthic_distribution_p(41+(i-1)*number_benth_distr) ! blau-Algen-Konsum-bentisch (Muscheln) in mg/l
      algdrb(2) = algdrb(1)
!
      algcok(1)=benthic_distribution_p(39+(i-1)*number_benth_distr) ! Kiesel-Algen Konsum durch Corophium ?
      algcok(2) = algcok(1)
      algcog(1)=benthic_distribution_p(42+(i-1)*number_benth_distr) ! grün-Algen Konsum durch Corophium ?
      algcog(2) = algcog(1)
      algcob(1)=benthic_distribution_p(43+(i-1)*number_benth_distr) ! blau-Algen Konsum durch Corophium ?
      algcob(2) = algcob(1)
      ess = 0.0 ! unbenutzt
      ! pbiogr, nbiogr, nbiobl, pbiobl ! unbenutzt
      zooind(1) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien in Ind/l
      zooind(2) = zooind(1)
      !jetzt direkt aus QSimDatenfelder GRote=transfer_parameter_p(67) ! Gewicht einer Rotatorie µg  | Aparam.txt
      ss(1) = planktonic_variable_p(53+nk) ! ORG. UND ANORG. SCHWEBSTOFFE(OHNE ALGEN UND ZOOPLANKTER) schweb()
      ss(2) = ss(1)
      Q_PK(1) = planktonic_variable_p(31+nk) ! Phosphoranteil der Kiesel-Algenbiomasse
      Q_PK(2) = Q_PK(1)
      Q_NK(1) = planktonic_variable_p(30+nk) ! Stickstoffanteil der Algenbiomasse kiesel
      Q_NK(2) = Q_NK(1)
      Q_SK(1) = planktonic_variable_p(32+nk)  ! Siliziumgehalt Kieselalgen
      Q_SK(2) = Q_SK(1)
      Q_NG(1) = planktonic_variable_p(33+nk) ! Stickstoffanteil der Algenbiomasse grün
      Q_NG(2) = Q_NG(1)
      Q_PG(1) = planktonic_variable_p(34+nk) ! Phosphoranteil der grün-Algenbiomasse
      Q_PG(2) = Q_PG(1)
      Q_NB(1) = planktonic_variable_p(35+nk) ! Stickstoffanteil der Algenbiomasse blau
      Q_NB(2) = Q_NB(1)
      Q_PB(1) = planktonic_variable_p(36+nk) ! Phosphoranteil der blau-Algenbiomasse
      Q_PB(2) = Q_PB(1)

      do j=1,num_lev
         vNH4z(j,1) = plankt_vari_vert_p(j+(3-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Ammonium-Stickstoffkonzentration g/m³ tiefenaufgelöst
         vNH4z(j,2) = vNH4z(j,1)
         vno3z(j,1) = plankt_vari_vert_p(j+(5-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Nitrat-Stickstoffkonzentration g/m³ tiefenaufgelöst
         vno3z(j,2) = vno3z(j,1)
         gelpz(j,1) = plankt_vari_vert_p(j+(6-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! gelöster Phosphor tiefenaufgelöst
         gelpz(j,2) = gelpz(j,1)
         siz(j,1) =  plankt_vari_vert_p(j+(7-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Silizium-Konzentration (tiefenaufgelöst)
         siz(j,2) = siz(j,1)
         dalgkz(j,1) = trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zuwachs Kieselalgen tiefenaufgelöst
         dalgkz(j,2) = dalgkz(j,1)
         dalggz(j,1) = trans_quant_vert_p(j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zuwachs Kieselalgen tiefenaufgelöst
         dalggz(j,2) = dalggz(j,1)
         dalgbz(j,1) = trans_quant_vert_p(j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zuwachs Kieselalgen tiefenaufgelöst
         dalgbz(j,2) = dalgbz(j,1)
       end do
      nkzs(1)=1         ! nur eine Tiefenschicht
      nkzs(2)=nkzs(1)
      dH2D = 0.25 ! Dicke Tiefenschicht ???  
      dH2De =0.25 ! unklar
      write(cpfad,'(A)', iostat = string_write_error )adjustl(trim(modellverzeichnis))! =  ! modellverzeichnis zum einlesen vone_extnct.dat
      if(string_write_error.ne.0)then
         write(fehler,*)'241: Übergabe modellverzeichnis->cpfad in algae_huelle() fehlgeschalgen'
         call qerror(fehler)

      end if

      do j=1,num_lev
         up_NKz(j,1) = trans_quant_vert_p(j+(1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! N (Stickstoff) Aufnahmerate der Kiesel-Algen
         up_NKz(j,2) = up_NKz(j,1)
         up_NGz(j,1) = trans_quant_vert_p(j+(2-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! N (Stickstoff) Aufnahmerate der grün-Algen
         up_NGz(j,2) = up_NGz(j,1)
         up_NBz(j,1) = trans_quant_vert_p(j+(3-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! N (Stickstoff) Aufnahmerate der blau-Algen
         up_NBz(j,2) = up_NBz(j,1)
         up_Siz(j,1) = trans_quant_vert_p(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Si-Aufnahmerate der Kiesel-Algen
         up_Siz(j,2) = up_Siz(j,1)
         up_PKz(j,1) = trans_quant_vert_p(j+(5-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! P (Phosphor) Aufnahmerate der Kiesel-Algen
         up_PKz(j,2) = up_PKz(j,1)
         up_PGz(j,1) = trans_quant_vert_p(j+(6-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! P (Phosphor) Aufnahmerate der grün-Algen
         up_PGz(j,2) = up_PGz(j,1)
         up_PBz(j,1) = trans_quant_vert_p(j+(7-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! P (Phosphor) Aufnahmerate der blau-Algen
         up_PBz(j,2) = up_PBz(j,1)
      end do 
      ! Qmx_PK, Qmx_PG, Qmx_PB ; max. Phosphoranteil Algenbiomasse, direkt aus QSimDatenfelder | APARAM.txt
      ! Qmn_PK, Qmn_PG, Qmn_PB ; min. Phosphoranteil Algenbiomasse, direkt aus QSimDatenfelder | Aparam.txt
      ! upmxPK, upmxPG, upmxPB ; max. P-Aufnahmerate der Algen, direkt aus QSimDatenfelder | Aparam.txt

      ! Qmx_NK, Qmx_NG, Qmx_NB ; ! max. Stickstoffanteil Algenbiomasse, direkt aus QSimDatenfelder | APARAM.txt
      ! Qmn_NK, Qmn_NG, Qmn_NB ; ! min. Stickstoffanteil Algenbiomasse, direkt aus QSimDatenfelder | Aparam.txt
      ! upmxNK, upmxNG, upmxNB ; ! max. N-Aufnahmerate der Algen, direkt aus QSimDatenfelder | Aparam.txt
      ! Qmx_SK ! max. Siliziumanteil der Kiesel-Algenbiomasse, direkt aus QSimDatenfelder aus APARAM.txt
      ! Qmn_SK ! min. Siliziumanteil der Kiesel-Algenbiomasse, direkt aus QSimDatenfelder | Aparam.txt
      ! upmxSK ! max. Si-Aufnahmerate der Kieselalgen, direkt aus QSimDatenfelder | Aparam.txt
      Skmor(1) = planktonic_variable_p(69+nk)  ! Silizium in schwebenden, abgestorbenen Kieselalgen
      Skmor(2) = Skmor(1)
      ! IKke, IKge, IKbe ; ! Lichtsättigung für Photosynthese der Algen, direkt aus QSimDatenfelder | Aparam.txt
      ! frmuke, frmuge, frmube ; ! Anteil der vom Wachstum abhängigigen Respiration, direkt aus QSimDatenfelder | Aparam.txt

      !alamda ! Absorptionskoeff. für Gelbstoffe bei 440 nm, direkt aus QSimDatenfelder | APARAM.txt
      akitbr(1) = transfer_quantity_p(48+(i-1)*number_trans_quant) ! Kieselalgen ??
      akitbr(2) = akitbr(1)
      !if(iglob.eq.kontrollknoten)print*,'vor algaeski; akitbr=',akitbr(1)
      agrtbr(1) = transfer_quantity_p(49+(i-1)*number_trans_quant) ! 
      agrtbr(2) = agrtbr(1)
      abltbr(1) = transfer_quantity_p(50+(i-1)*number_trans_quant) ! Zwischengröße Algenbiomasse ???
      abltbr(2) = abltbr(1)
      do j=1,num_lev
         chlaz(j,1) = plankt_vari_vert_p(j+(11-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Chlorophyl-A tiefenaufgelöst
         chlaz(j,2) = chlaz(j,1)
      end do
      do j=1,num_lev_trans
         akibrz(j,1) = trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zwischengröße Kiesel-Algen-Biomasse ?
         akibrz(j,2) = akibrz(j,1)
         agrbrz(j,1) = trans_quant_vert_p(j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zwischengröße grün-Algen-Biomasse ?
         agrbrz(j,2) = agrbrz(j,1)
         ablbrz(j,1) = trans_quant_vert_p(j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Zwischengröße blau-Algen-Biomasse ?
         ablbrz(j,2) = ablbrz(j,1)
         up_N2z(j,1) = trans_quant_vert_p(j+(8-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !  Aufnahmerate von Luftstickstoff durch Blaualgen
         up_N2z(j,2) = up_N2z(j,1)
      end do

      do j=1,num_lev
         akiz(j,1) = plankt_vari_vert_p(j+( 8-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse kiesel-Algen tiefenaufgelöst
         akiz(j,2) = akiz(j,1)
         agrz(j,1) = plankt_vari_vert_p(j+( 9-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse gruen-Algen tiefenaufgelöst
         agrz(j,2) = agrz(j,1)
         ablz(j,1) = plankt_vari_vert_p(j+(10-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse blau-Algen tiefenaufgelöst
         ablz(j,2) = ablz(j,1)
      end do

      chlaL(1)=0.0            ! für Linienquelle; nicht verwendet
      qeinlL(1)=0.0           ! für Linienquelle; nicht verwendet
      iorLa(1)=0              ! zur Berücksichtigung der Linienquelle; nicht verwendet
      iorLe(1)=0              ! zur Berücksichtigung der Linienquelle; nicht verwendet
      ieinLs(1)=0             ! keine Linienquellen

      do j=1,num_lev_trans
         algakz(j,1) = trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !  Respirierte Kiesel-Algenbiomasse, 
         algakz(j,2) = algakz(j,1)
         algagz(j,1) = trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !  Respirierte grün-Algenbiomasse, 
         algagz(j,2) = algagz(j,1)
         algabz(j,1) = trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !  Respirierte blau-Algenbiomasse
         algabz(j,2) = algabz(j,1)
         algzkz(j,1) = trans_quant_vert_p(j+(26-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l 
         algzkz(j,2) = algzkz(j,1)
         algzgz(j,1) = trans_quant_vert_p(j+(27-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! grün-Algen-Konsum durch Zoo-Plankton in mg/l 
         algzgz(j,2) = algzgz(j,1)
         algzbz(j,1) = trans_quant_vert_p(j+(28-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! blau-Algen-Konsum durch Zoo-Plankton in mg/l 
         algzbz(j,2) = algzbz(j,1)
      end do

      Chlaki(1) = planktonic_variable_p(12+nk)  ! Chlorophyl in Kieselalgen muegchla/l
      Chlaki(2) = Chlaki(1)
      chlagr(1) = planktonic_variable_p(13+nk)  ! Chlorophyl in gruenalgen muegchla/l
      chlagr(2) = chlagr(1)
      chlabl(1) = planktonic_variable_p(14+nk)  ! Chlorophyl in Blaualgen muegchla/l
      chlabl(2) = chlabl(1)
     do j=1,num_lev ! wohl nur Rückgabeparameter ???
         hchlkz(1,j,1) = plankt_vari_vert_p(j+(12-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Chlorophylgehalt der Kieselalgen
         hchlkz(1,j,2) = hchlkz(1,j,1)
         hchlgz(1,j,1) = plankt_vari_vert_p(j+(13-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Chlorophylgehalt der Gruenalgen
         hchlgz(1,j,2) = hchlgz(1,j,1)
         hchlbz(1,j,1) = plankt_vari_vert_p(j+(14-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Chlorophylgehalt der Blaualgen
         hchlbz(1,j,2) = hchlbz(1,j,1)
         hCChlkz(1,j,1) = plankt_vari_vert_p(j+(20-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! c-chla Verhältnis Kiesel
         hCChlkz(1,j,2) = hCChlkz(1,j,1)
         hCChlgz(1,j,1) = plankt_vari_vert_p(j+(21-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! c-chla Verhältnis grün
         hCChlgz(1,j,2) = hCChlgz(1,j,1)
         hCChlbz(1,j,1) = plankt_vari_vert_p(j+(22-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! c-chla Verhältnis blau
         hCChlbz(1,j,2) = hCChlbz(1,j,1)
      end do

      ! in tiefengemittelten Berechnungen eigentlich inaktiv
      Dz2D(1:2) = transfer_quantity_p(61+(i-1)*number_trans_quant) ! vertikalen Dispersionskoeffizient aus k_eps()
      ! vorsichtshalber:
      Dz2D(1:2) = 0.0

      !ToptK, ToptG, ToptB ; ! Optimal-Temperatur für Kieselalgenwachstum, direkt aus QSimDatenfelder | APARAM.txt
      !TmaxK, TmaxG, TmaxB ; ! Letal-Temperatur für Kieselalgenwachstum, direkt aus QSimDatenfelder | APARAM.txt
      !TkTemp_Ki  ! empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent), direkt aus QSimDatenfelder | APARAM.txt

      ifix=1 ! neu in runge-kutta ???

      sedAlg_MQ(1,1)=benthic_distribution_p(52+(i-1)*number_benth_distr) ! ?? wird aus sedflux kommen
      sedAlg_MQ(1,2) = sedAlg_MQ(1,1)
      if(kontroll) print*,'vor algaeski: sedAlg_MQ=', sedAlg_MQ(1,1)
     do j=1,num_lev ! 
         hQ_NKz(1,j,1) = plankt_vari_vert_p(j+(17-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Stickstoffanteil der Algenbiomasse kiesel
         hQ_NKz(1,j,2) = hQ_NKz(1,j,1)
         hQ_NGz(1,j,1) = plankt_vari_vert_p(j+(18-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! grün
         hQ_NGz(1,j,2) = hQ_NGz(1,j,1)
         hQ_NBz(1,j,1) = plankt_vari_vert_p(j+(19-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! blau
         hQ_NBz(1,j,2) = hQ_NBz(1,j,1)
      end do

!     Für die Berechnung des Chlorophyll-a/Kohlenstoff-Verhaeltnisses           
!     Angabe in mgChla/mgC  ; in ini_algae() gesetzt in . Variablendefinition in module QSimDatenfelder
      !a1Ki = -0.059
      !a2Ki = 0.28
      !a3Ki = 0.076

      EXTKS(1,1:2) = zone(point_zone(iglob))%seditemp%extiks ! Extinktionskoeffizient für PARS ((nicht mehr)nur bei Temperaturmodellierung erforderlich!) 

      akmor_1(1,1:2) = planktonic_variable_p(77+nk) ! ?? Kiesel-Algen
      agmor_1(1,1:2) = planktonic_variable_p(78+nk) ! ?? Gruen-Algen
      abmor_1(1,1:2) = planktonic_variable_p(79+nk) ! ?? Blau-Algen

      mstr = 1        ! Strangzähler | nur ein Profil in einem Strang
      it_h(1,1) = 1   ! ???
      it_h(1,2)= it_h(1,1)
      itags = tag     ! Tag im Monat module::modell zeitsekunde()   
      monats = monat  ! Monat im Jahr module::modell zeitsekunde()
      ! ifehl,ifhStr ! Fehlernummer und Strang in dem der Fehler auftrat
      ifehl = 0
      isim_end = 0

      !if(kontroll) print*,'vor algaeski: svhemk,fheaus,ToptK=',svhemk(1),fheaus(1),ToptK
      !if(kontroll) print*,'vor  algaes**: si(1),dalgki(1),ifix,hchlkz(1,j,1) = ',si(1),dalgki(1),ifix,hchlkz(1,j,1)
      if(kontroll) print*,'vor algaes**: up_Siz= ',up_Siz(1,1)
      if(kontroll) print*,'vor algaes**: chlaki,chlagr,chlabl= ',chlaki(1),chlagr(1),chlabl(1)
      if(kontroll) print*,'vor algaes**: aki,agr,abl= ',aki(1),agr(1),abl(1)
      if(kontroll) print*,'vor algaeski: extk,EXTKS=',extk(1:2),EXTKS(1,1:2)
      if(kontroll) print*,'vor algaes**: schwi(1),CHLA,up_NKz= ',schwi(1),CHLA(1),up_NKz(1,1)
      if(kontroll) print*,'vor algaes**: Q_NK,hQ_NKz,akbcm,hCChlgz=',Q_NK(1),hQ_NKz(1,1,1),akbcm(1),hCChlgz(1,1,1)
      if(kontroll) print*,'vor algaes**: algzok,zooind,GRote=',algzok(1),zooind(1),GRote
      if(kontroll) print*,'vor algaeski: svhemk=',svhemk(1),extk(1)
      if(kontroll) print*,'vor algaes**: akmor_1,agmor_1,abmor_1=',akmor_1(1,1),agmor_1(1,1),abmor_1(1,1)
      do k=1,number_benth_distr
         if(isnan(benthic_distribution_p(k+(i-1)*number_benth_distr)))then
            print*,'vor algaes**: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
            if(meinrang==0)print*,'benth_distr_name:',benth_distr_name(k)
         endif
      end do

!***********Algen***************         ! Algenaufruf in genau der Reihenfolge Kiesel, Blau, Grün !

!version  Stoffumsatzroutinen aus der QSim1D Version 13_40 vom 15. Oktober 2018 in QSim3D
      call algaeski(SCHWI,TFLIE,TEMPW,tempwz                                                &
                ,RAU,TIEFE,VMITT,flae,VNO3,VNH4,GELP,svhemk,svhemb,svhemg                   &
                ,CHLA,ir                                                                    &
                ,SI,dalgki,dalgak,flag,elen,ior,anze                                        &
                ,sedalk,algzok,echla,qeinl,vabfl                                            &
                ,dkimor,fkm,jiein,evkigr,vkigr,antbl,eantbl                                 &
                ,akchl,akgmax,akksn,akksp,akkssi,saettk,akremi,akrema                       &
                ,sbioki,vco2,iph,akbcm,abbcm,agbcm,aki,abl,agr,extk,extk_lamda              &
                ,ilamda,eta,aw,ack,acg,acb,ah,as,al                                         &
                ,uhrz,sised,tpki,iwied,akmuea,ftaaus,fiaus,fheaus                           &
                ,akraus,tauscs,ischif,ilbuhn,ieros,askie,cmatki,algdrk                      & 
                ,algcok,ess,zooind,GRote,SS,Q_PK,Q_NK,Q_SK                                  &
                ,vNH4z,vNO3z,gelPz,Siz,dalgkz,nkzs,dH2D,cpfad                               & 
                ,up_PKz,up_NKz,up_Siz,Qmx_PK,Qmn_PK,upmxPK                                  &
                ,Qmx_NK,Qmn_NK,upmxNK,Qmx_SK,Qmn_SK,upmxSK,SKmor,IKke,frmuke                & 
                ,alamda,akitbr,chlaz,akibrz,akiz                                            & 
                ,chlaL,qeinlL,ieinLs,algakz,algzkz,ablz,agrz                                & 
                ,Chlaki,hchlkz,hchlgz,hchlbz                                                & 
                ,hCChlkz,hCChlbz,hCChlgz                                                    & 
                ,Dz2D,ToptK                                                                 & 
                ,kTemp_Ki                                                                   & 
                ,ifix,Chlabl,Chlagr,a1Ki,a2Ki,a3Ki,sedAlg_MQ,sedAlk0,hQ_NKz                 &
                ,hQ_NGz,hQ_NBz,Q_PG,Q_NG,Q_PB,Q_NB                                          &
                ,mstr,it_h,itags,monats,isim_end                                            &
                ,extkS                                                                      &
                ,akmor_1,agmor_1,abmor_1                                                    &
                ,azStrs                                                                     &
                ,kontroll ,iglob )              !!wy                                     
! qsim13.301_28mae18
!      call algaeski(SCHWI,TFLIE,TEMPW,tempwz                                            &
!     & ,RAU,TIEFE,VMITT,flae,VNO3,VNH4,GELP,svhemk,svhemb,svhemg                        &
!     & ,CHLA,ir                                                                         &
!     & ,SI,dalgki,dalgak,flag,elen,ior,anze                                             &
!     & ,sedalk,algzok,echla,qeinl,vabfl                                                 &
!     & ,dkimor,fkm,jiein,evkigr,vkigr,antbl,eantbl                                      &
!     & ,akchl,akgmax,akksn,akksp,akkssi,saettk,akremi,akrema                            &
!     & ,sbioki,vco2,iph,akbcm,abbcm,agbcm,aki,abl,agr,extk,extk_lamda                   &
!     & ,ilamda,eta,aw,ack,acg,acb,ah,as,al                              		& !!wy
!     & ,uhrz,sised,tpki,iwied,akmuea,ftaaus,fiaus,fheaus                                &
!     & ,akraus,tauscs,ischif,ilbuhn,ieros,askie,cmatki,algdrk                           &
!     & ,algcok,ess,zooind,GRote,SS,Q_PK,Q_NK,Q_SK			                &
!     & ,vNH4z,vNO3z,gelPz,Siz,dalgkz,nkzs,dH2D,cpfad			                &
!     & ,up_PKz,up_NKz,up_Siz,Qmx_PK,Qmn_PK,upmxPK                   		        &
!     & ,Qmx_NK,Qmn_NK,upmxNK,Qmx_SK,Qmn_SK,upmxSK,SKmor,IKke,frmuke		        &
!     & ,alamda,akitbr,chlaz,akibrz,akiz                                                 &
!     & ,chlaL,qeinlL,ieinLs,algakz,algzkz,ablz,agrz                                     &
!     & ,Chlaki,hchlkz,hchlgz,hchlbz    							&
!     & ,hCChlkz,hCChlbz,hCChlgz    							&
!     & ,Dz2D,ToptK 		  							&
!     & ,kTemp_Ki          								& 
!     & ,ifix 										&
!     & ,Chlabl,Chlagr,a1Ki,a2Ki,a3Ki,sedAlg_MQ,sedAlk0,hQ_NKz    		        & !! noch unbearbeitet ###################
!     & ,mstr,it_h,itags,monats,isim_end,azStrs                                          &
!     & ,ialloc2 ,kontroll ,iglob )                 !!wy                                                                     
! /home/Wyrwa/QSim1D/src/algaes_22feb17                         
!      call algaeski(SCHWI,TFLIE,TEMPW,tempwz						&
!     & ,RAU,TIEFE,VMITT,flae,VNO3,VNH4,GELP,svhemk,svhemb,svhemg              		&
!     & ,CHLA,ir              								&
!     & ,SI,dalgki,dalgak,flag,elen,ior,anze              				&
!     & ,sedalk,algzok,echla,qeinl,vabfl              					&
!     & ,dkimor,fkm,jiein,evkigr,vkigr,antbl,eantbl              			&
!     & ,akchl,akgmax,akksn,akksp,akkssi,saettk,akremi,akrema                 		&
!     & ,sbioki,vco2,iph,akbcm,abbcm,agbcm,aki,abl,agr,extk,extk_lamda               	&
!     & ,ilamda,eta,aw,ack,acg,acb,ah,as,al                              		& !!wy
!     & ,uhrz,sised,tpki,iwied,akmuea,ftaaus,fiaus,fheaus  				&
!     & ,akraus,tauscs,ischif,ilbuhn,ieros,askie,cmatki,algdrk        		      	&
!     & ,algcok,ess,zooind,GRote,SS,Q_PK,Q_NK,Q_SK			                &
!     & ,vNH4z,vNO3z,gelPz,Siz,dalgkz,nkzs,dH2D,cpfad			                &
!     & ,up_PKz,up_NKz,up_Siz,Qmx_PK,Qmn_PK,upmxPK                   		        &
!     & ,Qmx_NK,Qmn_NK,upmxNK,Qmx_SK,Qmn_SK,upmxSK,SKmor,IKke,frmuke    			&
!     & ,alamda,akitbr,chlaz,akibrz,akiz   						&
!     & ,chlaL,qeinlL,ieinLs,algakz,algzkz,ablz,agrz 		  			&
!     & ,Chlaki,hchlkz,hchlgz,hchlbz    							&
!     & ,hCChlkz,hCChlbz,hCChlgz    							& !! noch unklar ########
!     & ,Dz2D,ToptK 		  							&
!     & ,kTemp_Ki          								& 
!     & ,ifix 										&
!     & ,Chlabl,Chlagr,a1Ki,a2Ki,a3Ki,sedAlg_MQ,sedAlk0,hQ_NKz    		        & !! noch unbearbeitet ###################
!     & ,mstr,it_h,itags,monats,isim_end,1,   kontroll, iglob)  ! azStrs=1    
                        
      !if(kontroll) print*,'nach algaeski: svhemk,Chlaki,aki=',svhemk(1),Chlaki(1),aki(1)
      !if(iglob==1) print*,'vor algaesbl: kontroll ,iglob=',kontroll ,iglob
      !if(kontroll) print*,'nach algaeski: extk(1),extk(2)=',extk(1),extk(2)

!version  Stoffumsatzroutinen aus der QSim1D Version 13_40 vom 15. Oktober 2018 in QSim3D
      call algaesbl(SCHWI,TFLIE,TEMPW,flag,elen,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemb,CHLA,ir                        &
                         ,dalgbl,dalgab,ior,anze,sedalb,algzob,dblmor,fkm,vabfl,abchl,abgmax,abksn,abksp,saettb,abremi     &
                         ,vco2,iph,vkigr,abbcm,abl,tpbl,uhrz,iwied,fibaus,abmuea,fhebas,abreau,tauscs,ischif,ilbuhn,ieros  &
                         ,zakie,zagre,zable,asble,qeinl,jiein,echla,ess,algdrb,algcob,antbl,zooind,GRote,SS,extk           &
                         ,extk_lamda                                                                                       &
                         ,ilamda,eta,aw,ack,acg,acb,ah,as,al       						           &
                         ,vNH4z,vNO3z,gelPz,dalgbz,nkzs,dH2D,tempwz,cpfad,up_PBz,up_NBz,Qmx_PB,Qmn_PB                      &
                         ,upmxPB,Qmx_NB,Qmn_NB,upmxNB,Q_NB,Q_PB,IKbe,frmube,alamda,abltbr,ablbrz,up_N2z,ablz               &
                         ,chlabl,a1Bl,a2Bl,a3Bl,hchlbz,hCChlbz,algabz,algzbz,Dz2D,ToptB,kTemp_Bl,ifix,sedAlg_MQ            &                               
                         ,sedAlb0,hQ_NBz, mstr,itags,monats,isim_end,abmor_1,azStrs                                        &
                         ,kontroll ,iglob )                                                                        
! qsim13.301_28mae18
!      call algaesbl(SCHWI,TFLIE,TEMPW,flag,elen,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemb,CHLA,ir                              &
!                         ,dalgbl,dalgab,ior,anze,sedalb,algzob,dblmor,fkm,vabfl,abchl,abgmax,abksn,abksp,saettb,abremi     &
!                         ,vco2,iph,vkigr,abbcm,abl,tpbl,uhrz,iwied,fibaus,abmuea,fhebas,abreau,tauscs,ischif,ilbuhn,ieros  &
!                         ,zakie,zagre,zable,asble,qeinl,jiein,echla,ess,algdrb,algcob,antbl,zooind,GRote,SS,extk           &
!                         ,extk_lamda                                                                                       &
!                         ,ilamda,eta,aw,ack,acg,acb,ah,as,al       		  					    & !!wy, Einlesen von e_extnct.dat nicht hier
!                         ,vNH4z,vNO3z,gelPz,dalgbz,nkzs,dH2D,tempwz,cpfad,up_PBz,up_NBz,Qmx_PB,Qmn_PB                      &
!                         ,upmxPB,Qmx_NB,Qmn_NB,upmxNB,Q_NB,Q_PB,IKbe,frmube,alamda,abltbr,ablbrz,up_N2z,ablz               &
!                         ,chlabl,a1Bl,a2Bl,a3Bl,hchlbz,hCChlbz,algabz,algzbz,Dz2D,ToptB,kTemp_Bl,ifix,sedAlg_MQ            &                               
!                         ,sedAlb0,hQ_NBz, mstr,itags,monats,isim_end,azStrs                                                &
!                         ,ialloc2  ,kontroll ,iglob )              !!wy                            
!!Version 13.30:
!      call algaesbl(SCHWI,TFLIE,TEMPW,flag,elen,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemb,CHLA,ir                        	&
!                         ,dalgbl,dalgab,ior,anze,sedalb,algzob,dblmor,fkm,vabfl,abchl,abgmax,abksn,abksp,saettb,abremi     	&
!                         ,vco2,iph,vkigr,abbcm,abl,tpbl,uhrz,iwied,fibaus,abmuea,fhebas,abreau,tauscs,ischif,ilbuhn,ieros  	&
!                         ,zakie,zagre,zable,asble,qeinl,jiein,echla,ess,algdrb,algcob,antbl,zooind,GRote,SS,extk           	&
!                         ,extk_lamda           											&
!                         ,ilamda,eta,aw,ack,acg,acb,ah,as,al       								& !!wy, da Einlesen von e_extnct.dat nicht parallel
!                         ,vNH4z,vNO3z,gelPz,dalgbz,nkzs,dH2D,tempwz,cpfad,up_PBz,up_NBz,Qmx_PB,Qmn_PB           		&
!                         ,upmxPB,Qmx_NB,Qmn_NB,upmxNB,Q_NB,Q_PB,IKbe,frmube,alamda,abltbr,ablbrz,up_N2z,ablz               	&
!                         ,chlabl,a1Bl,a2Bl,a3Bl,hchlbz,hCChlbz,algabz,algzbz,Dz2D,ToptB,kTemp_Bl,ifix,sedAlg_MQ              	&                               
!                         ,sedAlb0,hQ_NBz, mstr,itags,monats,isim_end,1           						&  ! azStrs=1 
!                         ,kontroll, iglob)              !!wy

!      if(kontroll) print*,'nach algaesbl: extk,chlabl,abl=',extk(1),chlabl(1),abl(1)
!      if(kontroll) print*,'nach algaesbl: svhemb,extk,Chlabl,abl=',svhemb(1),extk(1),Chlabl(1),abl(1)
!      if(kontroll) print*,'nach algaesbl: abbcm hchlbz hCChlbz=', abbcm(1), hchlbz(1,1,1), hCChlbz(1,1,1)

!version  Stoffumsatzroutinen aus der QSim1D Version 13_40 vom 15. Oktober 2018 in QSim3D
      call algaesgr(SCHWI,TFLIE,TEMPW,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemg,CHLA,SSALG,dalggr,dalgag              &
                ,flag,elen,ior,anze,sedalg,algzog,dgrmor,fkm,vkigr,chlaki,chlagr,vabfl,qeinl,jiein,evkigr,eantbl        &
                ,agchl,aggmax,agksn,agksp,agremi,vco2,algdrg,pbiogr,Q_PK,Q_NK,iph,akbcm,agbcm,aki,agr,cmatgr            &
                ,cmatki,abbcm,antbl,abl,pbiobl,chlabl,extk,extk_lamda		       	                             	&
                ,ilamda,eta,aw,ack,acg,acb,ah,as,al       					       	                &
                ,tpgr,uhrz,iwied,algcog                                                                                 &
                ,figaus,agmuea,fhegas,agreau,tauscs,ischif,ilbuhn,ieros,asgre,echla,ess,ss,zooind,GRote,Q_PG,Q_NG       &
                ,vNH4z,vNO3z,gelPz,dalggz,nkzs,dH2D,tempwz,cpfad,itags,monats,mstr,up_PGz,up_NGz,Qmx_PG                 &
                ,Qmn_PG,upmxPG,Qmx_NG,Qmn_NG,upmxNG,IKge,frmuge,alamda,agrtbr,agrbrz,akiz,agrz,ablz                     &
                ,chlaz,hchlkz,hchlgz,hchlbz,hCChlgz,algagz,algzgz,Dz2D,ToptG,kTemp_Gr,ifix,sedAlg_MQ,sedAlg0, hQ_NGz    &
                ,a1Gr,a2Gr,a3Gr,ifehl,ifhstr,isim_end,agmor_1,azStrs                                                    &
                ,kontroll ,iglob )              !!wy                                                                           
! qsim13.301_28mae18
!      call algaesgr(SCHWI,TFLIE,TEMPW,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemg,CHLA,SSALG,dalggr,dalgag                    &
!                ,flag,elen,ior,anze,sedalg,algzog,dgrmor,fkm,vkigr,chlaki,chlagr,vabfl,qeinl,jiein,evkigr,eantbl        &
!                ,agchl,aggmax,agksn,agksp,agremi,vco2,algdrg,pbiogr,Q_PK,Q_NK,iph,akbcm,agbcm,aki,agr,cmatgr            &
!                ,cmatki,abbcm,antbl,abl,pbiobl,chlabl,extk,extk_lamda                                                   &
!                ,ilamda,eta,aw,ack,acg,acb,ah,as,al       							        & !!wy, Einlesen von e_extnct.dat nicht hier
!                ,tpgr,uhrz,iwied,algcog                                                                                 &
!                ,figaus,agmuea,fhegas,agreau,tauscs,ischif,ilbuhn,ieros,asgre,echla,ess,ss,zooind,GRote,Q_PG,Q_NG       &
!                ,vNH4z,vNO3z,gelPz,dalggz,nkzs,dH2D,tempwz,cpfad,itags,monats,mstr,up_PGz,up_NGz,Qmx_PG                 &
!                ,Qmn_PG,upmxPG,Qmx_NG,Qmn_NG,upmxNG,IKge,frmuge,alamda,agrtbr,agrbrz,akiz,agrz,ablz                     &
!                ,chlaz,hchlkz,hchlgz,hchlbz,hCChlgz,algagz,algzgz,Dz2D,ToptG,kTemp_Gr,ifix,sedAlg_MQ,sedAlg0, hQ_NGz    &
!                ,a1Gr,a2Gr,a3Gr,ifehl,ifhstr,isim_end,azStrs                                                            &                  
!                ,kontroll ,iglob )                                                                              !!wy                            
!!Version 13.30:
!      call algaesgr(SCHWI,TFLIE,TEMPW,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemg,CHLA,SSALG,dalggr,dalgag              &
!     & ,flag,elen,ior,anze,sedalg,algzog,dgrmor,fkm,vkigr,chlaki,chlagr,vabfl,qeinl,jiein,evkigr,eantbl        &
!     & ,agchl,aggmax,agksn,agksp,agremi,vco2,algdrg,pbiogr,Q_PK,Q_NK,iph,akbcm,agbcm,aki,agr,cmatgr            &
!     & ,cmatki,abbcm,antbl,abl,pbiobl,chlabl,extk,extk_lamda                                                   &
!     & ,ilamda,eta,aw,ack,acg,acb,ah,as,al       			                                        & !!wy, da Einlesen von e_extnct.dat nicht parallel
!     & ,tpgr,uhrz,iwied,algcog                                                                                &
!     & ,figaus,agmuea,fhegas,agreau,tauscs,ischif,ilbuhn,ieros,asgre,echla,ess,ss,zooind,GRote,Q_PG,Q_NG       &
!     & ,vNH4z,vNO3z,gelPz,dalggz,nkzs,dH2D,tempwz,cpfad,itags,monats,mstr,up_PGz,up_NGz,Qmx_PG                 &
!     & ,Qmn_PG,upmxPG,Qmx_NG,Qmn_NG,upmxNG,IKge,frmuge,alamda,agrtbr,agrbrz,akiz,agrz,ablz                     &
!     & ,chlaz,hchlkz,hchlgz,hchlbz,hCChlgz,algagz,algzgz,Dz2D,ToptG,kTemp_Gr,ifix,sedAlg_MQ,sedAlg0, hQ_NGz    &
!     & ,a1Gr,a2Gr,a3Gr,ifehl,ifhstr,isim_end,1          	                                                &           ! azStrs=1          
!     & ,kontroll , iglob )              !!wy   

!!**********************Gruenalgen******************   !! Grünalgen zuletzt, weil dort zusammengefasst wird !!  sonst nachfolgende zusammenfassung:                
     ! vkigr(ior) = chlaki(ior)/(chlagrt+chlaki(ior)+chlabl(ior)) 
     ! antbl(ior) = chlabl(ior)/(chlagrt+chlaki(ior)+chlabl(ior)) 
     ! if(kontroll) print*,'nach vorläufiger Ersatz: chla,chlaki,chlabl,chlagr= ',chla(1),chlaki(1),chlabl(1),chlagr(1)
     ! chla(1) = chlaki(1) + chlabl(1) + chlagr(1) !!!### = chlaki(ior) + chlabl(ior) +chlagrt 
     ! if(chla(1).le. 0.0) then
     !    write(fehler,*)'algae_huelle chla .le. 0.0 am Knoten #',iglob
     !    call qerror(fehler)
     ! end if
     ! vkigr(1) = chlaki(1)/chla(1) 
     ! antbl(1) = chlabl(1)/chla(1)

      if(kontroll) print*,'nach algaes**: up_Siz= ',up_Siz(1,1)
      if(kontroll) print*,'nach algaes**: chlaki,chlagr,chlabl= ',chlaki(1),chlagr(1),chlabl(1)
      if(kontroll) print*,'nach algaes**: aki,agr,abl= ',aki(1),agr(1),abl(1)
      if(kontroll) print*,'nach algaesgr: vkigr(1),chlaki(1),chla(1)=',vkigr(1),chlaki(1),chla(1)
      if(kontroll) print*,'nach algaesgr: svhemk,svhemg,svhemb=',svhemk(1),svhemg(1),svhemb(1)
      if(kontroll) print*,'nach algaesgr: extk=',extk(1)
      if(kontroll) print*,'nach algaesgr: agbcm, hchlgz, hCChlgz=', agbcm(1), hchlgz(1,1,1), hCChlgz(1,1,1)
      if(kontroll) print*,'nach algaes**: akmor_1,agmor_1,abmor_1=',akmor_1(1,1),agmor_1(1,1),abmor_1(1,1)
      if(kontroll) print*,'nach algaes**: up_NKz= ',up_NKz(1,1)
      if(kontroll) print*,'nach algaes**:hchlkz hchlgz hchlbz hCChlgz hCChlkz hCChlbz=' &
     &                   ,hchlkz(1,1,1), hchlgz(1,1,1), hchlbz(1,1,1), hCChlgz(1,1,1), hCChlkz(1,1,1), hCChlbz(1,1,1)

      if(ifehl.gt.0)then
         print*,'algae_huelle: nk,i,iglob=',nk,i,iglob
         print*,'ifehl,ifhstr=',ifehl,ifhstr
         call qerror('algaesgr ifehl.gt.0')
      end if
  778 continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe: 
      planktonic_variable_p(21+nk) = svhemk(1)  
      planktonic_variable_p(22+nk) = svhemg(1)  
      planktonic_variable_p(23+nk) = svhemb(1)

      planktonic_variable_p(11+nk) = CHLA(1)    ! Chlorophyl-A tiefengemittelt
      planktonic_variable_p(12+nk) = Chlaki(1)  ! Chlorophyl in Kiesel-Algen muegchla/l
      planktonic_variable_p(13+nk) = chlagr(1)  ! Chlorophyl in Gruenalgen muegchla/l
      planktonic_variable_p(14+nk) = chlabl(1)  ! Chlorophyl in Blaualgen muegchla/l

      transfer_quantity_p(20+(i-1)*number_trans_quant) = dalgki(1) ! Zuwachs Kiesel-Algen
      transfer_quantity_p(21+(i-1)*number_trans_quant) = dalggr(1) ! Zuwachs grün-Algen
      transfer_quantity_p(22+(i-1)*number_trans_quant) = dalgbl(1) ! Zuwachs blau-Algen

      transfer_quantity_p(23+(i-1)*number_trans_quant) = dalgak(1) ! Respiration Kiesel-Algen
      transfer_quantity_p(24+(i-1)*number_trans_quant) = dalgag(1) ! Respiration grün-Algen
      transfer_quantity_p(25+(i-1)*number_trans_quant) = dalgab(1) ! Respiration blau-Algen

      benthic_distribution_p(26+(i-1)*number_benth_distr) = sedalk(1) ! Sedimentierte Menge an Kiesel-Algen
      benthic_distribution_p(27+(i-1)*number_benth_distr) = sedalg(1) ! Sedimentierte Menge an Grün-Algen
      benthic_distribution_p(28+(i-1)*number_benth_distr) = sedalb(1) ! Sedimentierte Menge an Blau-Algen

      transfer_quantity_p(7+(i-1)*number_trans_quant) = dkimor(1) ! Absterberate Kieselalgen
      transfer_quantity_p(8+(i-1)*number_trans_quant) = dgrmor(1) ! Absterberate Grünalgen
      transfer_quantity_p(9+(i-1)*number_trans_quant) = dblmor(1) ! Absterberate Blaualgen

      planktonic_variable_p(19+nk) = vkigr(1) ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ??
      planktonic_variable_p(20+nk) = antbl(1) ! Anteil der Blaualgen am Gesamt-Chlorophyll-a

      transfer_value_p(6) = saettk    ! Rückgabewert ???
      transfer_value_p(8) = saettg    ! ???
      transfer_value_p(9) = saettb    ! ???

      planktonic_variable_p(24+nk) = akbcm(1) ! Verhältnis Chlorophyll-a zu Kohlenstoff Kieselalgen
      planktonic_variable_p(25+nk) = agbcm(1) ! Verhältnis Chlorophyll-a zu Kohlenstoff Gruenalgen
      planktonic_variable_p(26+nk) = abbcm(1) ! Verhältnis Chlorophyll-a zu Kohlenstoff der blau-Algen

      planktonic_variable_p( 8+nk) = aki(1) ! Biomasse Kiesel-Algen
      planktonic_variable_p( 9+nk) = agr(1) ! Trockenmasse Gruen-Algen
      planktonic_variable_p(10+nk) = abl(1) ! Trockenmasse Blau-Algen

      transfer_quantity_p(54+(i-1)*number_trans_quant) = extk(1) ! mittlerer Extinktionskoeffizient
      if(extk(1)<=0.0)   print*,'algae_huelle extk(1+2) <=0.0 ,extkS, iglob',extk(1),extk(2),extkS(1,1),iglob

      ! extk_lamda wird nur von algaeski an algaesbl und algaesgr übergeben. an jedem Knoten hier in algae_huelle
      !do j=1,ilamda
      !   rb_extnct_p(9 + (j-1)*anz_extnct_koeff)= extk_lamda(j,1) ! ??? an allen Knoten auf allen Prozessen immer derselbe Wert??
      !end do

      benthic_distribution_p(2+(i-1)*number_benth_distr) = sised(1) ! Siliziumgehalt im Sediment (wird von algaeski verändert)

      transfer_quantity_p(55+(i-1)*number_trans_quant) = tpki(1) ! Ausgabeparameter ?? Kieselalgen Phosphor ??
      transfer_quantity_p(80+(i-1)*number_trans_quant) = tpgr(1) ! Ausgabeparameter ?? Grünalgen Phosphor ??
      transfer_quantity_p(81+(i-1)*number_trans_quant) = tpbl(1) ! Ausgabeparameter ?? Blaualgen Phosphorhemmung ??

      transfer_quantity_p(56+(i-1)*number_trans_quant) = akmuea(1) ! Ausgabeparameter algaeski()
      transfer_quantity_p(84+(i-1)*number_trans_quant) = agmuea(1) ! Wachstumsrate Ausgabeparameter algaesgr()
      transfer_quantity_p(85+(i-1)*number_trans_quant) = abmuea(1) ! Wachstumsrate Ausgabeparameter algaesbl()

      transfer_quantity_p(57+(i-1)*number_trans_quant) = ftaaus(1) ! Ausgabeparameter algaeski() fta

      transfer_quantity_p(58+(i-1)*number_trans_quant) = fiaus(1)  ! Ausgabeparameter algaeski() Pmit/(Pmax*3.6) 
      transfer_quantity_p(82+(i-1)*number_trans_quant) = figaus(1) ! Ausgabeparameter algaesgr() Pmit/(Pmax*3.6) 
      transfer_quantity_p(83+(i-1)*number_trans_quant) = fibaus(1) ! Ausgabeparameter algaesbl() Pmit/(Pmax*3.6) 

      transfer_quantity_p(59+(i-1)*number_trans_quant) = fheaus(1) ! Ausgabeparameter algaeski() svhemk
      transfer_quantity_p(86+(i-1)*number_trans_quant) = fhegas(1) ! Ausgabeparameter algaesi() svhemg
      transfer_quantity_p(87+(i-1)*number_trans_quant) = fhebas(1) ! Ausgabeparameter algaesbl() svhemb

      transfer_quantity_p(60+(i-1)*number_trans_quant) = akraus(1) ! Ausgabeparameter algaeski() F53
      transfer_quantity_p(88+(i-1)*number_trans_quant) = agreau(1) ! Ausgabe agbcm algaesgr()
      transfer_quantity_p(89+(i-1)*number_trans_quant) = abreau(1) ! Ausgabe von abbcm (Chlorophyll-a zu Kohlenstoff der blau-Algen) algaesbl() 

      planktonic_variable_p(30+nk) = Q_NK(1) ! Stickstoffanteil der Algenbiomasse kiesel
      planktonic_variable_p(31+nk) = Q_PK(1) ! Phosphoranteil der Kiesel-Algenbiomasse
      planktonic_variable_p(32+nk) = Q_SK(1) ! Siliziumgehalt Kieselalgen
      planktonic_variable_p(33+nk) = Q_NG(1) ! Stickstoffanteil der Algenbiomasse grün
      planktonic_variable_p(34+nk) = Q_PG(1) ! Phosphoranteil der grün-Algenbiomasse
      planktonic_variable_p(35+nk) = Q_NB(1) ! Stickstoffanteil der Algenbiomasse blau
      planktonic_variable_p(36+nk) = Q_PB(1) ! Phosphoranteil der blau-Algenbiomasse

      planktonic_variable_p(77+nk) = akmor_1(1,1) ! ?? Kiesel-Algen
      planktonic_variable_p(78+nk) = agmor_1(1,1) ! ?? Gruen-Algen
      planktonic_variable_p(79+nk) = abmor_1(1,1) ! ?? Blau-Algen


      do j=1,num_lev
         trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = dalgkz(j,1) ! Zuwachs Kieselalgen tiefenaufgelöst
         trans_quant_vert_p(j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = dalggz(j,1) ! Zuwachs Kieselalgen tiefenaufgelöst
         trans_quant_vert_p(j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = dalgbz(j,1) ! Zuwachs Kieselalgen tiefenaufgelöst
      end do

      do j=1,num_lev
         trans_quant_vert_p(j+(1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_NKz(j,1) ! N (Stickstoff) Aufnahmerate der Kiesel-Algen
         trans_quant_vert_p(j+(2-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_NGz(j,1) ! N (Stickstoff) Aufnahmerate der grün-Algen
         trans_quant_vert_p(j+(3-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_NBz(j,1) ! N (Stickstoff) Aufnahmerate der blau-Algen
         trans_quant_vert_p(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_Siz(j,1) ! Si-Aufnahmerate der Kiesel-Algen
         trans_quant_vert_p(j+(5-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_PKz(j,1) ! P (Phosphor) Aufnahmerate der Kiesel-Algen
         trans_quant_vert_p(j+(6-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_PGz(j,1) ! P (Phosphor) Aufnahmerate der grün-Algen
         trans_quant_vert_p(j+(7-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_PBz(j,1) ! P (Phosphor) Aufnahmerate der blau-Algen
      end do 

      planktonic_variable_p(69+nk) = Skmor(1) ! Silizium in schwebenden, abgestorbenen Kieselalgen

      transfer_quantity_p(48+(i-1)*number_trans_quant) = akitbr(1) ! Kieselalgen ??
      transfer_quantity_p(49+(i-1)*number_trans_quant) = agrtbr(1) ! 
      transfer_quantity_p(50+(i-1)*number_trans_quant) = abltbr(1) ! Zwischengröße Algenbiomasse ???

      do j=1,num_lev
         plankt_vari_vert_p(j+(11-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = chlaz(j,1) ! Chlorophyl-A tiefenaufgelöst
      end do

      do j=1,num_lev_trans
         trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = akibrz(j,1) ! Kiesel-Algen-Biomasse? Wachstum? tiefenaufgelöst
         trans_quant_vert_p(j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = agrbrz(j,1) ! Zwischengröße grün-Algen-Biomasse ?
         trans_quant_vert_p(j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = ablbrz(j,1) ! Zwischengröße blau-Algen-Biomasse ?
         trans_quant_vert_p(j+(8-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = up_N2z(j,1) !  Aufnahmerate von Luftstickstoff durch Blaualgen
      end do

      do j=1,num_lev
         plankt_vari_vert_p(j+( 8-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = akiz(j,1) ! Biomasse kiesel-Algen tiefenaufgelöst
         plankt_vari_vert_p(j+( 9-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = agrz(j,1) ! Biomasse gruen-Algen tiefenaufgelöst
         plankt_vari_vert_p(j+(10-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = ablz(j,1) ! Biomasse blau-Algen tiefenaufgelöst
      end do

      do j=1,num_lev_trans
         trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algakz(j,1) !  Respirierte Kiesel-Algenbiomasse, tiefenaufgelöst
         trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algagz(j,1) !  Respirierte grün-Algenbiomasse, 
         trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algabz(j,1) !  Respirierte blau-Algenbiomasse

         trans_quant_vert_p(j+(26-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzkz(j,1) ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l 
         trans_quant_vert_p(j+(27-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzgz(j,1) ! grün-Algen-Konsum durch Zoo-Plankton in mg/l 
         trans_quant_vert_p(j+(28-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzbz(j,1) ! blau-Algen-Konsum durch Zoo-Plankton in mg/l 
      end do

      do j=1,num_lev
         plankt_vari_vert_p(j+(12-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hchlkz(1,j,1) ! Chlorophylgehalt der Kieselalgen
         plankt_vari_vert_p(j+(13-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hchlgz(1,j,1) ! Chlorophylgehalt der Gruenalgen
         plankt_vari_vert_p(j+(14-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hchlbz(1,j,1) ! Chlorophylgehalt der Blaualgen
         plankt_vari_vert_p(j+(20-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hCChlkz(1,j,1) ! c-chla Verhältnis Kiesel
         plankt_vari_vert_p(j+(21-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hCChlgz(1,j,1) ! c-chla Verhältnis grün
         plankt_vari_vert_p(j+(22-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hCChlbz(1,j,1) ! c-chla Verhältnis blau
      end do

      benthic_distribution_p(52+(i-1)*number_benth_distr) = sedAlg_MQ(1,1) ! ??
      do j=1,num_lev
         plankt_vari_vert_p(j+(17-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hQ_NKz(1,j,1) ! Stickstoffanteil der Algenbiomasse kiesel
         plankt_vari_vert_p(j+(18-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hQ_NGz(1,j,1) ! grün
         plankt_vari_vert_p(j+(19-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = hQ_NBz(1,j,1) ! blau
      end do

      benthic_distribution_p(53+(i-1)*number_benth_distr) = sedAlk0(1) ! !sedAlk0 wird nur an k_eps() übergeben.
!--------------------------------------------------------------------------------------------Kontrolle und Prüfung NaN
      if (num_lev_trans .gt. nkzs(1)) call qerror('algae_huelle Tiefenschichtung trans_quant_vert passt nicht')
      !if(kontroll) print*,'aki(1),chla(1),akiz(1,1),up_NKz(1,1)',aki(1),chla(1),akiz(1,1),up_NKz(1,1)
      !call ini_algae() !! ### algae_huelle zu Testzwecken überbrückt

      do k=1,number_plankt_vari
         if(isnan(planktonic_variable_p(k+nk)))then
            print*,'nach algaes**: isnan(planktonic_variable_p  node#',iglob,' variable# ',k
            if(meinrang==0)print*,'planktonic_variable_name:',planktonic_variable_name(k)
         endif
      end do
      do j=1,num_lev
         do k=1,number_plankt_vari_vert
            if(isnan(plankt_vari_vert_p(j+(k-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)))then
               print*,'nach algaes**: isnan(plankt_vari_vert_p  node#',iglob,' level#', j,' variable# ',k
               if(meinrang==0)print*,'plankt_vari_vert_name:',plankt_vari_vert_name(k)
            endif
         end do
      end do

      do k=1,number_trans_quant
         if(isnan(transfer_quantity_p(k+(i-1)*number_trans_quant)))then
            print*,'nach algaes**: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang=',meinrang
            if(meinrang==0)print*,'trans_quant_name:',trans_quant_name(k)
         endif
      end do
      do j=1,num_lev_trans
         do k=1,number_trans_quant_vert
            if(isnan(trans_quant_vert_p(j+(k-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)))then
               print*,'nach algaes**: isnan(trans_quant_vert_p  node#',iglob,' level#', j,' variable# ',k
               if(meinrang==0)print*,'trans_quant_vert_name:',trans_quant_vert_name(k)
            endif
         end do
      end do

      do k=1,number_benth_distr
         if(isnan(benthic_distribution_p(k+(i-1)*number_benth_distr)))then
            print*,'nach algaes**: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
            if(meinrang==0)print*,'benth_distr_name:',benth_distr_name(k)
         endif
      end do

!      if(fehler_nan)then
!         !print*,'fehler_nan algae_huelle Knoten #',iglob, ' Tiefe=', rb_hydraul_p(2+(i-1)*number_rb_hydraul)
!         write(fehler,*)'fehler_nan algae_huelle Knoten #(global)',iglob,'  #(lokal)',i ,' meinrang=',meinrang,  &
!     &   ' Tiefe=', rb_hydraul_p(2+(i-1)*number_rb_hydraul)
!         call qerror(fehler)
!      end if

      RETURN
      END subroutine algae_huelle
!----+-----+----

!> SUBROUTINE algae_aufteilung()
!!\n zurück: \ref algenaufteilung ; Code: algae_huelle.f95
      SUBROUTINE algae_aufteilung(i)!! ### ERSETZT 
      use modell                                                 
      use QSimDatenfelder
      implicit none
      integer i,nk,l
      real T, Te0, Sum_N, f_NK
      real CChl0k,CChl0g,CChl0b! Version 13.30.01

      if(meinrang.ne.0)call qerror("algae_aufteilung darf nur auf Prozessor 0 laufen")

      nk=(i-1)*number_plankt_vari

!!    Ausgangspunkt sind die Angaben:
      !planktonic_variable(11+nk) = rabe(zaehl)%wert_jetzt(13)  !  CHLA   Chorophyl-a 
      !planktonic_variable(19+nk) = rabe(zaehl)%wert_jetzt(14)  !  VKIGR  Anteil Kieselalgen (falls unbekannt 0)
      !planktonic_variable(20+nk) = rabe(zaehl)%wert_jetzt(15)  !  ANTBL  Anteil Blaualgen (falls unbekannt 0)

!....ag(k,b)chl gilt für 20°C  in mgC/mgChla                                         
      Te0 = 20.
      T=planktonic_variable(1+nk) ! Wassertemperatur

      CChl0k = akchl * exp(-a1Ki * Te0)              ! C:Chla bei 0°C für Kieselalgen mgC/mgChla 
      CChl0g = agchl * exp(-a1Gr * Te0)              ! Grünalgen            
      CChl0b = abchl * exp(-a1Bl * Te0)              ! Blaualgen              

!...temperaturabhängiges des C:Chla-Verhältnisses                     
      planktonic_variable(24+nk) = CChl0k * exp(a1Ki * T) ! akbcm
      planktonic_variable(25+nk) = CChl0g * exp(a1Gr * T) ! agbcm
      planktonic_variable(26+nk) = CChl0b * exp(a1Bl * T) ! abbcm

! mg Algenbiomasse, Chla in µg/l 
      planktonic_variable(12+nk) = &   ! chlaki = chla * vkigr
     &   planktonic_variable(11+nk)*planktonic_variable(19+nk)
     planktonic_variable(13+nk) = &   ! chlagr = chla * (1-vkigr-antbl)
     &   planktonic_variable(11+nk)*(1.-planktonic_variable(19+nk)-planktonic_variable(20+nk))
      planktonic_variable(14+nk) = &   ! chlabl = chla * antbl	
     &   planktonic_variable(11+nk)*planktonic_variable(20+nk)

!      akis(mstr,mRB) = (chlas(mstr,mRB)*vkigrs(mstr,mRB)/1000.)*(akbcms(mstr,mRB)/Caki)                                   
      planktonic_variable( 8+nk) = ( planktonic_variable(12+nk)*planktonic_variable(24+nk) ) / (1000.0*Caki)
!     &   planktonic_variable(11+nk)*planktonic_variable(19+nk)     & ! aki = (chla*VKIGR)/(1000*akbcm*Caki)
!     &   /(1000.*planktonic_variable(24+nk)*Caki)
      do l=1,num_lev ! akiz konstante Verteilung in der Vertikalen
         plankt_vari_vert(l+(8-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)=  &
         planktonic_variable(8+nk)
      end do ! alle l levels

!      agrs(mstr,mRB) = (chlas(mstr,mRB)*(1.-vkigrs(mstr,mRB)-antbls(mstr,mRB))/1000.)*(agbcms(mstr,mRB)/Cagr)                  
      planktonic_variable(9+nk)= ( planktonic_variable(13+nk)*planktonic_variable(25+nk) ) / (1000.0*Cagr)
!     &   planktonic_variable(11+nk)*(1.-planktonic_variable(19+nk)-planktonic_variable(20+nk)) &
!     &   /(1000.*planktonic_variable(25+nk)*Cagr) ! agr = (chla*(1-VKIGR-ANTBL))/(1000*agbcm*Cagr)
      do l=1,num_lev ! ablz konstante Verteilung in der Vertikalen
         plankt_vari_vert(l+(9-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)=  &
         planktonic_variable(9+nk)
      end do ! alle l levels
                                    
!      abls(mstr,mRB) = (chlas(mstr,mRB)*antbls(mstr,mRB)/1000.)*(abbcms(mstr,mRB)/Cabl)                                    
       planktonic_variable(10+nk)= ( planktonic_variable(14+nk)*planktonic_variable(26+nk) ) / (1000.0*Cabl)
!     &   planktonic_variable(11+nk)*planktonic_variable(20+nk) &
!     &   /(1000.*planktonic_variable(26+nk)*Cabl)  ! abl = (chla*ANTBL)/(1000*abbcm*Cabl)
      do l=1,num_lev ! agrz konstante Verteilung in der Vertikalen
         plankt_vari_vert(l+(10-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)=  &
         planktonic_variable(10+nk)
      end do ! alle l levels

!....zelluläre Nährstoffgehalte in den Algen
!      Sum_N = vnh4s(mstr,mRB)+vNO3s(mstr,mRB) 
!      f_NK = 0.582*(Sum_N/(Sum_N+0.011))
!      f_NK = 1.                                       
!      Q_NKs(mstr,mRB) = Qmx_NK-(1.-f_NK)*(Qmx_NK-Qmn_NK) 
      planktonic_variable(30+nk) = Qmx_NK ! Stickstoffanteil der Algenbiomasse kiesel
!      Q_PKs(mstr,mRB) = Qmn_PK+(Qmx_PK-Qmn_PK)/1.
      planktonic_variable(31+nk) = Qmx_PK ! Phosphoranteil der Kiesel-Algenbiomasse
!      Q_SKs(mstr,mRB) = Qmx_SK
      planktonic_variable(32+nk) = Qmx_SK  ! Siliziumgehalt Kieselalgen
!      Q_NGs(mstr,mRB) = Qmn_NG+(Qmx_NG-Qmn_NG)/1. 
      planktonic_variable(33+nk) = Qmx_NG ! Stickstoffanteil der Algenbiomasse grün
!      Q_PGs(mstr,mRB) = Qmn_PG+(Qmx_PG-Qmn_PG)/1. 
      planktonic_variable(34+nk) = Qmx_PG ! Phosphoranteil der grün-Algenbiomasse
!      Q_NBs(mstr,mRB) = Qmn_NB+(Qmx_NB-Qmn_NB)/1. 
      planktonic_variable(35+nk) = Qmx_NB ! Stickstoffanteil der Algenbiomasse blau
!      Q_PBs(mstr,mRB) = Qmn_PB+(Qmx_PB-Qmn_PB)/1. 
      planktonic_variable(36+nk) = Qmx_PB ! Phosphoranteil der blau-Algenbiomasse
                   
      planktonic_variable(21+nk)= 0.01 ! svhemk ### unklar ###
      planktonic_variable(22+nk)= 0.01 ! svhemg ### unklar ###
      planktonic_variable(23+nk)= 0.01 ! svhemb ### unklar ###

      planktonic_variable(27+nk)= 0.01 ! akiiv ### unklar ###
      planktonic_variable(28+nk)= 0.01 ! agriv ### unklar ###
      planktonic_variable(29+nk)= 0.01 ! abliv ### unklar ###

      RETURN 
      END subroutine algae_aufteilung

!----!----!----!----!----!----!----!----!----!----!----!----!
!!<table >
!!<tr><th> Variablen-Name QSim-1D </th><th> Daten-Feld QSim-3D </th><th> Beschreibung </th><th> Einheit </th></tr>
!!<tr><td> \ref schwi	</td><td> aus \ref wetter_rb </td><td> Globalstrahlung von strahlg_wetter() berechnet </td><td> cal/(cm2*h) </td></tr>
!!<tr><td> \ref tflie	</td><td> real(dt)/86400 </td><td> Umwandlung des Zeitschritt </td><td> sekunden (QSim-3D) Tage (QSim-1D)</td></tr>
!!<tr><td> \ref tempw	</td><td> \ref tiefengemittelte_planktische_variable 1 </td><td> Wasser-Temperatur </td><td> °C </td></tr>
!!<tr><td> \ref tempwz	</td><td> \ref tiefenaufgelöste_planktische_variable 1 </td><td> Wassertemperatur tiefenaufgelöst  </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> rau		</td><td> \ref benthische_verteilungen 5 </td><td> Strickler Reibungsbeiwert </td><td> m**(1/3) / s </td></tr>
!!<tr><td> \ref tiefe	</td><td> \ref hydraul_rb 2 </td><td> Wassertiefe </td><td> m </td></tr>
!!<tr><td> \ref vmitt	</td><td> \ref hydraul_rb 1 </td><td> Geschwindigkeitsbetrag </td><td> m/s </td></tr>
!!<tr><td> \ref vno3		</td><td> \ref tiefengemittelte_planktische_variable 5  </td><td> nitrat </td><td> mgN/l </td></tr>
!!<tr><td> \ref vnh4		</td><td> \ref tiefengemittelte_planktische_variable 3 </td><td> ammonium </td><td> </td></tr>
!!<tr><td> \ref gelp		</td><td> \ref tiefengemittelte_planktische_variable 6 </td><td> gelöster ortho-Phosphat-Phosphor tiefengemittelt </td><td> </td></tr>
!!<tr><td> svhemk, svhemg, svhemb </td><td> \ref tiefengemittelte_planktische_variable 21 22 23 </td><td> Mittelwertbildung der Licht-Hemmung ? </td><td> </td></tr>
!!<tr><td> \ref chla		</td><td> \ref tiefengemittelte_planktische_variable 11 </td><td> Chlorophyl-A </td><td> </td></tr>
!!<tr><td> ir		</td><td> \ref tiefengemittelte_übergabe_variable 42 </td><td> Ingestionsrate der Rotatorien konsum() </td><td> mg/(l*h)</td></tr>
!!<tr><td>  </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> \ref si 		</td><td> \ref tiefengemittelte_planktische_variable 7 </td><td> silikat-Silizium-Konzentration (tiefengemittelt) </td><td> </td></tr>
!!<tr><td> dalgki, dalggr, dalgbl </td><td> \ref tiefengemittelte_übergabe_variable 20 21 22 </td><td> Zuwachs kiesel,grün,blau-Algen </td><td> </td></tr>
!!<tr><td> dalgak, dalgag, dalgab </td><td> \ref tiefengemittelte_übergabe_variable 23 24 25 </td><td> Respiration kiesel,grün,blau-Algen </td><td> </td></tr>
!!<tr><td> flag		</td><td>0   </td><td> keine Einleitungen </td><td> </td></tr>
!!<tr><td> elen		</td><td>1   </td><td> Elementlänge (nicht verwendet) </td><td> </td></tr>
!!<tr><td> ior		</td><td>1 </td><td> Laufindex </td><td> </td></tr>
!!<tr><td> anze		</td><td>1 </td><td> Anzahl der Profile im aktuellen Strang </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> sedalk, sedalg, sedalb </td><td> \ref benthische_verteilungen 26 27 28 </td><td> Sedimentierte Menge an kiesel,grün,blau-Algen </td><td> </td></tr>
!!<tr><td> algzok	</td><td> \ref tiefengemittelte_übergabe_variable 53 </td><td> Algen-Konsum Zoo-Plankton in mg/l </td><td> </td></tr>
!!<tr><td> echla	</td><td> 0.0     </td><td> keine Einleitung </td><td> </td></tr>
!!<tr><td> vabfl	</td><td> 2.5     </td><td> wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet. </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> dkimor, dgrmor, dblmor </td><td> \ref tiefengemittelte_übergabe_variable 7 6 8 </td><td> Absterberate kiesel,grün,blau-algen </td><td> </td></tr>
!!<tr><td> fkm		</td><td>0.0  </td><td> Flusskilometer (unbenutzt) </td><td> </td></tr>
!!<tr><td> jiein	</td><td>0 </td><td> keine Punkt-Einleitungen </td><td> </td></tr>
!!<tr><td> evkigr	</td><td> 0.0  </td><td> Einleitungswert (keine Einleitungen in T-QSim) </td><td> </td></tr>
!!<tr><td> vkigr	</td><td> \ref tiefengemittelte_planktische_variable 19 </td><td> Anteil der Kieselalgen am Gesamt-Chlorophyll-a ?? </td><td> </td></tr>
!!<tr><td> antbl	</td><td> \ref tiefengemittelte_planktische_variable 20 </td><td> Anteil der Blaualgen am Gesamt-Chlorophyll-a </td><td> </td></tr>
!!<tr><td> eantbl	</td><td> 0.0  </td><td> Einleitungswert (keine Einleitungen in T-QSim) </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> akchl	</td><td> \ref globaleParameter </td><td> Biomasse/Chlorophyll Verhältnis in Kieselalgen (Parameter APARAM.txt) </td><td> </td></tr>
!!<tr><td> akgmax	</td><td> \ref globaleParameter </td><td> Max. Wachstumsate d. Kieselalgen (Parameter APARAM.txt) </td><td> </td></tr>
!!<tr><td> akksn	</td><td> \ref globaleParameter </td><td> N-Halbsättigung Kieselalgen (Parameter APARAM.txt) </td><td> </td></tr>
!!<tr><td> akksp	</td><td> \ref globaleParameter </td><td> P-Halbsättigung Kieselalgen (Parameter APARAM.txt) </td><td> </td></tr>
!!<tr><td> akkssi	</td><td> \ref globaleParameter </td><td> Si-Halbsättigung Kieselalgen (Parameter APARAM.txt) </td><td> </td></tr>
!!<tr><td> saettk, saettg, saettb </td><td> \ref übergabe_wert 6 8 9 </td><td> \ref Licht_algen Rückgabewert</td><td> </td></tr>
!!<tr><td> akremi	</td><td> \ref globaleParameter </td><td> Grundrespiration d. Kieselalgen (Parameter APARAM.txt) </td><td> </td></tr>
!!<tr><td> akrema	</td><td> 0.0 </td><td> unbenutzt </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> sbioki	</td><td> 0.0 </td><td> unbenutzt </td><td> </td></tr>
!!<tr><td> vco2		</td><td> \ref tiefengemittelte_übergabe_variable 26 </td><td> Kohlendioxyd | unbenutzt ? </td><td> </td></tr>
!!<tr><td> iph		</td><td> 0 </td><td> unbenutzt </td><td> </td></tr>
!!<tr><td> akbcm, agbcm, abbcm </td><td> \ref tiefengemittelte_planktische_variable 24 25 26 </td><td> Verhältnis Chlorophyll-a zu Kohlenstoff kiesel,grün,blau-Algen </td><td> </td></tr>
!!<tr><td> \ref aki , \ref agr, \ref abl </td><td> \ref tiefengemittelte_planktische_variable 8 9 10 </td><td> Biomasse an kiesel,grün,blau-Algen </td><td> </td></tr>
!!<tr><td> extk		</td><td> \ref tiefengemittelte_übergabe_variable 54 </td><td> mittlerer Extinktionskoeffizient </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> ilamda	</td><td> siehe: \ref extnct_rb </td><td> Anzahl der Wellenlängen </td><td> </td></tr>
!!<tr><td> eta		</td><td> \ref extnct_rb 1 </td><td> Wellenlänge (wohl in Nm in der Luft) </td><td> </td></tr>
!!<tr><td> aw		</td><td> \ref extnct_rb 2 </td><td> Wasser </td><td> </td></tr>
!!<tr><td> ack		</td><td> \ref extnct_rb 3 </td><td> Kieselalgen </td><td> </td></tr>
!!<tr><td> acg		</td><td> \ref extnct_rb 4 </td><td> Gruenalgen </td><td> </td></tr>
!!<tr><td> acb		</td><td> \ref extnct_rb 5 </td><td> Blaualgen </td><td> </td></tr>
!!<tr><td> ah		</td><td> \ref extnct_rb 6 </td><td> Humin-Stoffe </td><td> </td></tr>
!!<tr><td> as		</td><td> \ref extnct_rb 7 </td><td> susp. Schwebstoff </td><td> </td></tr>
!!<tr><td> al		</td><td> \ref extnct_rb 8 </td><td> Sonnenlicht </td><td> </td></tr>
!!<tr><td> extk_lamda	</td><td> \ref extnct_rb 9 </td><td> Rückgabeparameter Gesamtextinktion ??? </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> uhrz		</td><td>uhrzeit_stunde     </td><td> Uhrzeit module::modell zeitsekunde() </td><td> </td></tr>
!!<tr><td> sised	</td><td> \ref benthische_verteilungen 2 </td><td> Siliziumgehalt im Sediment </td><td> </td></tr>
!!<tr><td> tpki, tpgr, tpbl </td><td> \ref tiefengemittelte_übergabe_variable 55 80 81 </td><td> Ausgabeparameter ?? Kieselalgen Phosphor ?? </td><td> </td></tr>
!!<tr><td> iwied	</td><td>0 </td><td> unbenutzte Variable </td><td> </td></tr>
!!<tr><td> akmuea, agmuea, abmuea </td><td> \ref tiefengemittelte_übergabe_variable 56 84 85 </td><td> Ausgabeparameter algaeski() </td><td> </td></tr>
!!<tr><td> ftaaus	</td><td> \ref tiefengemittelte_übergabe_variable 57 </td><td> Ausgabeparameter, Faktor \ref Temperatur_algen -Abhängigkeit \ref Algen-Wachstum </td><td> </td></tr>
!!<tr><td> fiaus, figaus, fibaus </td><td> \ref tiefengemittelte_übergabe_variable 58 82 83 </td><td> Ausgabeparameter algaeski() Pmit/(Pmax*3.6)  </td><td> </td></tr>
!!<tr><td> fheaus, fhegas, fhebas </td><td> \ref tiefengemittelte_übergabe_variable 59 86 87 </td><td> Ausgabeparameter algaeski() svhemk </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> akraus, agreau, abreau </td><td> \ref tiefengemittelte_übergabe_variable 60 88 89 </td><td>Ausgabe von abbcm (Chlorophyll-a zu Kohlenstoff der blau-Algen) algaesbl()</td><td> </td></tr>
!!<tr><td> tauscs	</td><td> \ref übergabe_wert 7 </td><td> Schiffseinfluss qsim.f90: tauscs = 1.25 </td><td> </td></tr>
!!<tr><td> ischif	</td><td> schifffahrts_zone(point_zone(iglob)) </td><td> schifffahrt in dieser module::zonen ; 1->Schiffsverkehr  , 0-> kein Schiffsverkehr; MODELLG.txt "F" </td><td> </td></tr>
!!<tr><td> ilbuhn	</td><td> 0   </td><td> keine Buhnen </td><td> </td></tr>
!!<tr><td> ieros	</td><td> 0   </td><td> unbenutzt </td><td> </td></tr>
!!<tr><td> askie	</td><td> \ref globaleParameter </td><td> Sedimentierbarer Anteil an Kieselalgen | APARAM.txt </td><td> </td></tr>
!!<tr><td> cmatki, cmatgr </td><td> \ref benthische_verteilungen 9 10 </td><td> Abspülung benthischer kiesel+grün-Algen, Einganbeparameter</td><td> </td></tr>
!!<tr><td> algdrk	</td><td> \ref benthische_verteilungen 38 </td><td> \ref Algen-Konsum-bentisch (Muscheln) in mg/l </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> algcok	</td><td> \ref benthische_verteilungen 39 </td><td> Kiesel-Algen Konsum durch Corophium ? </td><td> </td></tr>
!!<tr><td> ess		</td><td> 0.0 </td><td> unbenutzt </td><td> </td></tr>
!!<tr><td> zooind	</td><td> \ref tiefengemittelte_planktische_variable 50 </td><td> Anzahl der Rotatorien in Ind/l </td><td> </td></tr>
!!<tr><td> GRote	</td><td> \ref globaleParameter </td><td> Gewicht einer Rotatorie µg  | Aparam.txt </td><td> </td></tr>
!!<tr><td> ss		</td><td> \ref tiefengemittelte_planktische_variable 53 \n \ref schwebstoff_salz </td><td> ORG. UND ANORG. SCHWEBSTOFFE(OHNE ALGEN UND ZOOPLANKTER) schweb() schwebstoff_salz()</td><td> </td></tr>
!!<tr><td> Q_PK		</td><td> \ref tiefengemittelte_planktische_variable 31 </td><td> Phosphoranteil der Kiesel-Algenbiomasse </td><td> </td></tr>
!!<tr><td> Q_NK		</td><td> \ref tiefengemittelte_planktische_variable 30 </td><td> Stickstoffanteil der Algenbiomasse kiesel </td><td> </td></tr>
!!<tr><td> Q_SK		</td><td> \ref tiefengemittelte_planktische_variable 32 </td><td> Siliziumgehalt Kieselalgen </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> vNH4z	</td><td> \ref tiefenaufgelöste_planktische_variable 3 </td><td> Ammonium-Stickstoffkonzentration g/m³ tiefenaufgelöst </td><td> </td></tr>
!!<tr><td> vno3z	</td><td> \ref tiefenaufgelöste_planktische_variable 5 </td><td> Nitrat-Stickstoffkonzentration g/m³ tiefenaufgelöst </td><td> </td></tr>
!!<tr><td> gelpz	</td><td> \ref tiefenaufgelöste_planktische_variable 6 </td><td> gelöster Phosphor tiefenaufgelöst </td><td> </td></tr>
!!<tr><td> siz		</td><td> \ref tiefenaufgelöste_planktische_variable 7 </td><td> Silizium-Konzentration (tiefenaufgelöst) </td><td> </td></tr>
!!<tr><td> dalgkz, dalggz, dalgbz </td><td> \ref tiefenaufgelöste_übergabe_variable 12 13 14 </td><td> Zuwachs kiesel,grün,blau-Algen tiefenaufgelöst </td><td> </td></tr>
!!<tr><td> nkzs		</td><td>1  </td><td> nur eine Tiefenschicht </td><td> </td></tr>
!!<tr><td> dH2D		</td><td> 0.25 </td><td> Dicke Tiefenschicht ???   </td><td> </td></tr>
!!<tr><td> dH2De	</td><td>0.25 </td><td> unklar </td><td> </td></tr>
!!<tr><td> cpfad	</td><td> - </td><td> Modellverzeichnis zum einlesen von \ref extnct_rb (in QSim-3D ausgelagert) </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> up_NKz, up_NGz, up_NBz </td><td> \ref tiefenaufgelöste_übergabe_variable 1 2 3 </td><td> N (Stickstoff) Aufnahmerate der kiesel,grün,blau-Algen </td><td> </td></tr>
!!<tr><td> up_Siz	</td><td> \ref tiefenaufgelöste_übergabe_variable 4 </td><td> Si-Aufnahmerate der Kiesel-Algen </td><td> </td></tr>
!!<tr><td> up_PKz, up_PGz, up_PBz </td><td> \ref tiefenaufgelöste_übergabe_variable 5 6 7 </td><td> P (Phosphor) Aufnahmerate der kiesel,grün,blau-Algen </td><td> </td></tr>
!!<tr><td> Qmx_PK, Qmx_PG, Qmx_PB </td><td> \ref globaleParameter </td><td> max. Phosphoranteil Algenbiomasse | Aparam.txt </td><td> </td></tr>
!!<tr><td> Qmn_PK, Qmn_PG, Qmn_PB </td><td> \ref globaleParameter </td><td> min. Phosphoranteil Algenbiomasse | Aparam.txt </td><td> </td></tr>
!!<tr><td> upmxPK, upmxPG, upmxPB </td><td> \ref globaleParameter </td><td> max. P-Aufnahmerate der Algen | Aparam.txt </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> Qmx_NK, Qmx_NG, Qmx_NB </td><td> \ref globaleParameter </td><td> max. Stickstoffanteil Algenbiomasse | APARAM.txt </td><td> </td></tr>
!!<tr><td> Qmx_NK, Qmx_NG, Qmx_NB </td><td> \ref globaleParameter </td><td> min. Stickstoffanteil Algenbiomasse | Aparam.txt </td><td> </td></tr>
!!<tr><td> upmxNK, upmxNG, upmxNB </td><td> \ref globaleParameter </td><td> max. N-Aufnahmerate der Algen | Aparam.txt </td><td> </td></tr>
!!<tr><td> Qmx_SK 	</td><td> \ref globaleParameter </td><td> max. Siliziumanteil der Kiesel-Algenbiomasse aus APARAM.txt </td><td> </td></tr>
!!<tr><td> Qmn_SK	</td><td> \ref globaleParameter </td><td> min. Siliziumanteil der Kiesel-Algenbiomasse | Aparam.txt </td><td> </td></tr>
!!<tr><td> upmxSK	</td><td> \ref globaleParameter </td><td> max. Si-Aufnahmerate der Kieselalgen | Aparam.txt </td><td> </td></tr>
!!<tr><td> Skmor	</td><td> \ref tiefengemittelte_planktische_variable 69 </td><td> Silizium in schwebenden, abgestorbenen Kieselalgen </td><td> </td></tr>
!!<tr><td> IKke		</td><td> \ref globaleParameter </td><td> Lichtsättigung für Photosynthese der Kieselalgen | Aparam.txt </td><td> </td></tr>
!!<tr><td> frmuke	</td><td> \ref globaleParameter </td><td> Anteil der vom Wachstum abhängigigen Respiration (Kieselalgen) | Aparam.txt </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> alamda	</td><td> \ref globaleParameter </td><td> Absorptionskoeff. für Gelbstoffe bei 440 nm | APARAM.txt </td><td> </td></tr>
!!<tr><td> akitbr, agrtbr, abltbr </td><td> \ref tiefengemittelte_übergabe_variable 48 49 50 </td><td>  </td><td> </td></tr>
!!<tr><td> chlaz	</td><td> \ref tiefenaufgelöste_planktische_variable 11 </td><td> Chlorophyl-A tiefenaufgelöst </td><td> </td></tr>
!!<tr><td> akibrz, agrbrz, ablbrz </td><td> \ref tiefenaufgelöste_übergabe_variable 23 24 25 </td><td> Zwischengöße Algen-Biomasse </td><td> </td></tr>
!!<tr><td> up_N2z 	</td><td>  \ref tiefenaufgelöste_übergabe_variable 8 </td><td> Aufnahmerate von Luftstickstoff durch Blaualgen </td><td> </td></tr>
!!<tr><td> akiz, agrz, ablz </td><td> \ref tiefenaufgelöste_planktische_variable 8 9 10</td><td> Biomasse kiesel,grün,blau-Algen tiefenaufgelöst </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> chlaL,qeinlL,iorLa,iorLe,ieinLs </td><td> für Linienquelle in 1D; in QSim-3D nicht verwendet </td><td>  </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> algakz, algagz, algabz </td><td> \ref tiefenaufgelöste_übergabe_variable 18 19 20 </td><td>  respirierte kiesel,grün,blau-Algenbiomasse, tiefenaufgelöst </td><td> </td></tr>
!!<tr><td> algzkz, algzgz, algzbz </td><td> \ref tiefenaufgelöste_übergabe_variable 26 27 28</td><td> kiesel,grün,blau-Algen-Konsum durch Zoo-Plankton </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> Chlaki, chlagr, chlabl </td><td>  \ref tiefengemittelte_planktische_variable 12 13 14 </td><td> Chlorophyll-a Gehalt in kiesel,grün,blau-Algen </td><td> </td></tr>
!!<tr><td> hchlkz, hchlgz, hchlbz </td><td> \ref tiefenaufgelöste_planktische_variable 12 13 14 </td><td> Chlorophyll-a Gehalt der kiesel,grün,blau-Algen </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> Dz2D		</td><td> \ref tiefengemittelte_übergabe_variable 61 </td><td> min. vertikalen Dispersionskoeffizient </td><td> </td></tr>
!!<tr><td> ToptK	</td><td> \ref globaleParameter 9/3 </td><td> optimal Temperatur für Kieselalgenwachstum | APARAM.txt </td><td> </td></tr>
!!<tr><td> TmaxK	</td><td> \ref globaleParameter 9/4 </td><td> Letal-Temperatur für Kieselalgenwachstum | APARAM.txt </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> ifix		</td><td> ?? </td><td> nicht in Gebrauch </td><td> </td></tr>
!!<tr><td> </td><td> </td><td>  </td><td> </td></tr>
!!<tr><td> mstr		</td><td> 1  </td><td> Strangzähler | nur ein Profil in einem Strang </td><td> </td></tr>
!!<tr><td> itags	</td><td> tag     </td><td> Tag im Monat module::modell zeitsekunde()    </td><td> </td></tr>
!!<tr><td> monats	</td><td> monat  </td><td> Monat im Jahr module::modell zeitsekunde()  </td><td> </td></tr>
!!<tr><td> kontroll	</td><td> wahr wenn iglob.eq.kontrollknoten </td><td> Erweiterung wy</td><td> </td></tr>
!!<tr><td> error	</td><td> Fehlerschalter </td><td> Erweiterung wy </td><td> </td></tr>
!!</table>
!! \n\n 
