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


!! enthält die Definition der unveränderlichen biologischen u.&nbsp;a. die Güte-Berechnung definierenden Verhältnisse in den Zonen des Modells.
!! Es gelten die \ref qsimdateiregeln "allgemeinen Regeln für QSim-Dateien".
!! \n\n
!! enthält die Definition der unveränderlichen biologischen u.&nbsp;a. die Güte-Berechnung definierenden Verhältnisse in den Zonen des Modells.
!! <a href="./exp/ModellG.3D.txt" target="_blank">ModellG.3D.txt</a>\n
!! \ref qsimdateikopfzeilev 

!> \page qsimdateimodellg3 ModellG.3D.txt
!! Die Datei <a href="./exp/MODELLG.3D.txt" target="_blank">ModellG.3D.txt</a> 
!! dient der zonenweisen Zuordnung von Parametern, Rand- und Anfangsbedingungen.\n
!! Sie ist hervorgegangen aus der Datei <a href="./exp/ModellG_beispiel.txt" target="_blank">ModellG.txt</a> 
!! in QSim-1D, welche eine Zuordnung zu Abschnitten in Strängen des 1D-Modells erlaubte.
!! \n\n
!! Welcher Knoten zu welcher <b>Zone</b> gehört, ist in der Datei points (siehe \ref Datenmodell) festgelegt. 
!! Die Zonen-Einteilung des Modellgebiets wird z. Z. mit dem
!! Netzerstellungswerkzeug JANET vorgenommen. Diese Zonen-Einteilung geschieht vor der hydraulischen Simulation
!! und wird vom Gütemodell übernommen.
!! \n\n
!! Beispiele:\n
!! 3D: <a href="./exp/MODELLG.3D.txt" target="_blank">MODELLG.3D.txt</a>
!! Zeilen, die mit # beginnen, sind Kommentarzeilen und werden von QSim3D überlesen.\n
!! 1D: <a href="./exp/ModellG_beispiel.txt" target="_blank">ModellG_beispiel.txt</a>
!! Die mit # gekennzeichneten Zeilen dienen der Erläuterung und müssen entfernt werden um die Datei als Eingabe für QSim1D zu verwenden.
!! \n\n
!! Die Datei MODELLG.3D.txt gliedert sich in einen Kopfbereich und nachfolgende Datenblöcke für die einzelnen Zonen (Zonen-Blöcke).
!! \n
!! <h2>Kopfbereich</h2>
!! Der Kopfbereich besteht aus drei Zeilen.
!! <table>
!! <tr><th>Position</th><th>Inhalt</th></tr>
!! <tr><td>Kopf, 1.&nbsp;Zeile</td><td>Versions-Zeile\n Die Zeile wird von Gerris benutzt und von QSim-3D nicht ausgewertet.</td></tr>
!! <tr><td>Kopf, 2.&nbsp;Zeile</td><td>Modellname und ggf. weitere Angaben, maximal 255 Zeichen.\n
!!   Die Zeile wird von Gerris benutzt und von QSim-3D nicht ausgewertet.</td></tr>
!! <tr><td>Kopf, 3.&nbsp;Zeile</td><td>Anzahl der Zonen des Modells, d.&nbsp;h. Anzahl der in der Datei folgenden Zonenblöcke</td></tr>
!! </table>
!!
!! <h2>Zonenblock</h2>
!! Vor jedem Zonenblock muss sich eine Leerzeile befinden.\n
!! Für jede Zone des Modells muss ein eigener Block vorhanden sein, der aus folgenden Zeilen besteht:\n
!! Die unten aufgeführten Kennbuschstaben müssen in der ersten Spalte stehen.\n
!! <table>
!! <tr><th>Kenn- \n Buchstabe</th><th>Inhalt</th><th>Bemerkung</th><th>1D <-> 3D</th></tr>
!! <tr><td> ' ' </td><td><b>Zonennummer</b>, "Bezeichnung"</td><td> Die erste Zeile eines Zonenblocks muss die Zonen-nummer enthalten. 
!!              Die "Bezeichnung" dient zur Orientierung der Bearbeiter*in</td><td></td></tr>
!! <tr><td> T </td><td>anfangsKm(unbenutzt),endKm(unbenutzt)  <b>Wetterstations-Nummer</b>, Wetterstations-Lage (mueNHN)</td><td>
!!            Wetterstations-Nummer gemäß der Datei WETTER.txt (siehe \ref Datenmodell)</td><td>3D identisch 1D</td></tr>
!! <tr><td> I </td><td>Nummer des Randes, der zur <b>Initialisierung </b> dieser Zone dient </td><td> 
!!            Randnummer entsprechend der Datei EREIGG.txt (siehe \ref Datenmodell)</td><td>in 1D nicht vorhanden</td></tr>
!! <tr><td> R </td><td>Reibungsbeiwert "Rauheit" für o.g. Zone als ks-Wert nach Nikuradse (Sandrauheit) in m 
!!            </td><td> casu-Werte müssen z. Z noch von Hand übertragen werden. SCHISM kennt keine Zonen.</td><td>in 1D nicht vorhanden</td></tr>
!! <tr><td> M </td><td>Macrophyten </td><td> Makrophyten-Wachstum  </td><td>3D identisch 1D [noch nicht implementiert]</td></tr>
!! <tr><td> P </td><td>Macrophytendichte </td><td> </td><td>[noch nicht implementiert]</td></tr>
!! <tr><td> F </td><td>Schiffsverkehr </td><td>  </td><td> wenn F vorhanden, ganze Zone Schiffsverkehr \ref schifffahrts_zone =1</td></tr>
!! <tr><td> D </td><td>Dreissena (Muscheln) </td><td> Dreissena-Bewuchs in den Gewässer-Abschnitten </td><td>[noch nicht implementiert]</td></tr>
!! <tr><td> C </td><td>Corophium (Schlickkrebs) </td><td> Corophium-Vorkommen in den Gewässer-Abschnitten </td><td>[noch nicht implementiert]</td></tr> 
!! <tr><td> B </td><td>benthische Algen </td><td> Benth.Algen-Vorkommen in den Gewässer-Abschnitten </td><td>[noch nicht implementiert]</td></tr>
!! <tr><td> V </td><td>tiefenaufgelöste Berechnung </td><td> Abschnitte mit 2D-tiefenaufgelöster-Modellierung</td><td>[noch nicht implementiert]</td></tr> 
!! <tr><td> U </td><td>Buhnen </td><td> Parameter für Buhnenfelder </td><td>[noch nicht implementiert]</td></tr>
!! <tr><td> L </td><td>Laichperiode </td><td> Laichperiode  </td><td>[noch nicht implementiert]</td></tr>
!! <tr><td> O </td><td>Verschattung durch Uferbewuchs ? </td><td> Anteil der Vegetationstypen</td><td>[noch nicht implementiert]</td></tr>
!! <tr><td> Z </td><td> Sediment-Kenngrößen </td><td> Belegungen für Stoffumsatz im Sediment </td>
!! <td>aPOM(irrelevant),ePOM(irrelevant),\ref sedom,\ref bedgs,\ref sedvvert ,(optional) \ref kornd ,\ref burial </td></tr>
!! <tr><td> S </td><td> Sedimenttemperatur  </td><td> Kenngrössen für Temperatur/Sedimenttemperatur </td><td>[noch nicht implementiert]</td></tr>
!! <tr><td> W </td><td> Wehr  </td><td> Parameter des Wehres </td><td>in 3D nicht sinnvoll</td></tr>
!! <tr><td> E </td><td> Erosion  </td><td> kritische Sohlschubspannung, Erodibilitätskonstante, Exponent, Dichte</td><td> </td></tr>
!! <tr><td>  </td><td>   </td><td>  </td><td></td></tr>
!! </table>
!! \n\n
!! Die Datei ist ähnlich wie ModellG.txt für QSim aufgebaut, die in der 
!! <a href="./pdf/Schnittstelle_QSIM2004.pdf" target="_blank">Schnittstellenbeschreibung 2004 Gerris-QSim</a> \n
!! <a href="./pdf/schnittstellenbeschreibung2006_gerris.pdf" target="_blank">Schnittstellenbeschreibung 2006 Gerris-QSim</a>\n
!! genauer erläutert wird.\n\n
!! Aktuelle Parameter siehe: ModellGParam().
!! 
!! \n\n
!!
!! <h1>Wetter</h1>
!! Die Wetter Randbedingungen (meteorologischen Bedingungen) sind zeitabhängig. \n
!! Sie sind in Gerris über die Schaltfläche "Wetter" bearbeitbar.\n
!! Details dazu in: \ref wetter_rb \n
!!
!! <h2>T Wetterstation</h2>
!! In der Datei \ref qsimdateimodellg3 wird lediglich vorgegeben, welche Wetterstation zur jeweiligen Zone gehört. \n
!! <code>\verbatim
!! <ParamSetDef Id="QT" Text="Wetterstation" Help="Wetterstations-Zuordnung" Scope="Abschnitt">
!! <Parameter Ident="WStation" Text="Wetterstation" Unit="" Format="I4" Null="-1" Help="Zugehörige Wetterstation" Min="" Max="" Default="" />
!! <Parameter Ident="WLage" Text="Lage der Station" Unit="m üb. NN" Format="F7.1" Null="-1" Help="" Min="" Max="" Default="" />
!! \endverbatim</code>
!! 
!! <h1>Gebiets-Eigenschaften</h1>
!! In der Gerris-Benutzeroberfläche werden sie unter der Schaltfläche "Strang-Optionen" geführt.\n
!!
!! <h2>R Reibungsbeiwert</h2>
!! nur in 3D:\n
!! Es wird der ks-Wert nach Nikuradse (Sandrauheit) eingelesen.
!!
!! <h2>I Initialisierung</h2>
!! nur in 3D:\n
!! Initialisierung für die jeweilige Zone mittels Randbedingung.\n
!! d.h. es wird eine Randnummer eingelesen.\n
!! Alle Berechnungspunkte der jeweiligen Zone bekommen die Werte die am zugeordneten Rand zum Berechnungs-Startzeitpunkt vorliegen.
!!
!! <h2>Z Sediment</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Id="QZ" Text="Sediment-Kenngrößen" Help="Sediment-Kenngrößen in den Gewässer-Abschnitten" Scope="Abschnitt">'
!!   <Parameter Ident="POMsed" Text="Anteil org. Materials" Unit="%" Format="F6.2" Null="-1" Help="Anteil des organischen Materials im Sediment" Min="" Max="" Default="-1" />
!!   <Parameter Ident="BedGSed" Text="Bedeckungsgrad der Sohle mit Sediment (0-1)" Unit="-" Format="F5.2" Null="-1" Help="" Min="0" Max="1" Default="-1." />
!!   <Parameter Ident="VVERTZ" Text="volumenbezogene Eindringgeschwindigkeit ins Sediment" Unit="mm/h" Format="F9.4" Null="-1" Help="" Min="" Max="" Default="-1." />
!! \endverbatim</code>
!! 
!! <h2>S Sedimenttemperatur</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Id="QS" Text="Kenngrössen für Temperatur/Sedimenttemperatur" Help="Kenngrößen für die Gewässerabschnitten" Scope="Abschnitt">'
!!   <Parameter Ident="SPEWKS" Text="Spez. WärmeKapazität Sediment" Unit="KJ/(kg*K)" Format="F6.2" Null="-1" Help="Ton: 0.83; Sand: 0.88" Min="0.8" Max="4.5" Default="-1" />
!!   <Parameter Ident="WUEBK" Text="Wärmeübergangskoeffizient" Unit="KJ/(K*m2*h)" Format="F7.2" Null="-1" Help="" Min="0" Max="1000" Default="-1." />
!!   <Parameter Ident="PSREFS" Text="Reflektionsanteil der Strahlung an der Sedimentoberfläche" Unit="-" Format="F5.2" Null="-1" Help="" Min="0" Max="1" Default="-1." />
!!   <Parameter Ident="EXTKS" Text="Extinktionskoeffizient für PARS (nur bei Temperaturmodellierung erforderlich!)" Unit="-" Format="F5.2" Null="-1" Help="" Min="" Max="" Default="-1." />
!! \endverbatim</code>
!! 
!! <h2>L Laichperiode</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Ident="QL" Text="Laichperiode" Help="Laichperiode" Scope="Strang">'
!!   <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Beginns der Laichperiode" Min="1" Max="31" Default="" />
!!   <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Beginns der Laichperiode" Min="1" Max="12" Default="" />
!!   <Parameter Ident="Dauer" Text="Dauer" Unit="" Format="I2" Null="-1" Help="Dauer der Laichperiode in Tagen" Min="0" Max="9999" Default="" />
!! \endverbatim</code>
!! 
!! <h2>D Dreissena-Muschel</h2>
!! Im 3D gibt es keine Böschungen, daher wird dort die Muscheldichte Null gesetzt.
!! <code>\verbatim
!! <ParamSetDef Id="QD" Text="Dreissena" Help="Dreissena-Bewuchs in den Gewässer-Abschnitten" Scope="Abschnitt">
!!   <Parameter Ident="mboesch0" Text="Biomasse 0.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte (Schalenlänge kl. 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />
!!   <Parameter Ident="msohle0" Text="Biomasse 0.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />
!!   <Parameter Ident="gewicht0" Text="Mittl. Muschelgewicht 0.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 0. Kohorte" Min="" Max="" Default="" />
!!   <Parameter Ident="mboesch1" Text="Biomasse 1.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte (Schalenlänge gr.= 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />
!!   <Parameter Ident="msohle1" Text="Biomasse 1.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />
!!   <Parameter Ident="gewicht1" Text="Mittl. Muschelgewicht 1.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 1. Kohorte." Min="" Max="" Default="" />
!! \endverbatim</code>
!! 
!! <h2>M Makrophyten</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Id="QM" Text="Makrophyten" Help="Makrophyten-Wachstum" Scope="Strang">
!!   <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Wachstumsbeginns der Makrophyten" Min="1" Max="31" Default="" />
!!   <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Wachstumsbeginns der Makrophyten" Min="1" Max="12" Default="" />
!!   <Parameter Ident="MaxTag" Text="Max.-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="31" Default="" />
!!   <Parameter Ident="MaxMonat" Text="Max.-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="12" Default="" />
!!   <Parameter Ident="EndTag" Text="Ende-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Minimum erreicht hat. Hier endet das Makrophytenwachstum" Min="1" Max="31" Default="" />
!!   <Parameter Ident="EndMonat" Text="Ende-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Minimum erreicht hat" Min="1" Max="12" Default="" />
!! \endverbatim</code>
!! 
!! <h2>P Makrophyten-Dichte</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Id="QP" Text="Dichte der Makrophyten" Help="Makrophyten-Dichte" Scope="Abschnitt">'
!!   <Parameter Ident="PflMin" Text="min. Dichte (Winter)" Unit="g/m²" Format="F7.2" Null="-1" Help="Minimale Dichte der Makrophyten im Winter" Min="" Max="" Default="" />
!!   <Parameter Ident="PflMax" Text="max. Dichte (Sommer)" Unit="g/m²" Format="F7.2" Null="-1" Help="Maximale Dichte der Makrophyten im Sommer" Min="" Max="" Default="" />
!! \endverbatim</code>
!! 
!! <h2>F Schiffsverkehr</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Id="QF" Text="Schiffsverkehr" Help="Schiffsverkehr auf den Gewässer-Abschnitten" Scope="Abschnitt">'
!!   <Parameter Ident="VSCHIFF" Text="Schiffsgeschwindigkeit" Unit="m/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="1.5" />
!!   <Parameter Ident="UPROP" Text="Drehzahl des Propellers" Unit="U/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="3.33" />
!! <ParamSetDef Id="QC" Text="Corophium" Help="Corophium-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt">'
!!   <Parameter Ident="DBoesch" Text="Ind.dichte Böschung" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Böschung" Min="" Max="" Default="" />
!!   <Parameter Ident="DSohle" Text="Ind.dichte Sohle" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Sohle" Min="" Max="" Default="" />
!! \endverbatim</code>
!! 
!! <h2>B Benthische Algen</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Id="QB" Text="Benth.Algen" Help="Benth.Algen-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt">'
!!   <Parameter Ident="GGruen" Text="Gewicht Grünalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Grünalgen" Min="" Max="" Default="-1" />
!!   <Parameter Ident="GKiesel" Text="Gewicht Kieselalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Kieselalgen" Min="" Max="" Default="-1" />
!! \endverbatim</code>
!! 
!! <h2>C Corophium</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Id="QC" Text="Corophium" Help="Corophium-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt">'
!!   <Parameter Ident="DBoesch" Text="Ind.dichte Böschung" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Böschung" Min="" Max="" Default="" />
!!   <Parameter Ident="DSohle" Text="Ind.dichte Sohle" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Sohle" Min="" Max="" Default="" />
!! \endverbatim</code>
!! 
!! <h2>O Vegetationstypen</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Id="QO" Text="Anteil der Vegetationstypen" Help="Anteil der Vegetationstypen" Scope="Abschnitt">
!!   <Parameter Ident="VTYP1" Text="Niedervegetation (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP2" Text="Buschwerk (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP3" Text="Weichholzaue (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP4" Text="Laubwald (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP5" Text="Nadelwald (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP6" Text="Bebauung (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VALTBL" Text="Höhe d. Bebauung (links)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="EDUFBL" Text="Uferabstand d. Bebauung (links)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP7" Text="Niedervegetation (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP8" Text="Buschwerk (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP9" Text="Weichholzaue (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP10" Text="Laubwald (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP11" Text="Nadelwald (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP12" Text="Bebauung (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VALTBR" Text="Höhe d. Bebauung (rechts)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="EDUFBR" Text="Uferabstand d. Bebaung (rechts)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP13" Text="Laubwald (beidseitig)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!!   <Parameter Ident="VTYP14" Text="Nadelwald (beidseitig)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
!! \endverbatim</code>
!! 
!! <h2> W V U - Keine Entsprechung im 3D</h2>
!! ...
!! <code>\verbatim
!! <ParamSetDef Id="QW" Text="Wehr" Help="Parameter des Wehres" Scope="Strang">'
!!   <Parameter Ident="IstAktiv" Text="Aktiv" Unit="" Format="I1" Null="0" Help="Wehr nicht berücksichtigen: 0; Wehr berücksichtigen: 1" Min="" Max="" Default="0" />
!!   <Parameter Ident="Breite" Text="Breite [m]" Unit="m" Format="F7.2" Null="-1" Help="Breite des Wehres >0 : Wehrbelüftung" Min="0" Max="9999.999" Default="" />
!!   <Parameter Ident="Hoehe" Text="Höhe [m]" Unit="m" Format="F7.2" Null="-1" Help="Falls Breite >0 Wehrhöhe aus Wasserspiegeldifferenz OW/UW berechnen: -1; ansonsten Höhe >0" Min="0" Max="999.999" Default="" />
!! </ParamSetDef>'
!! <ParamSetDef Id="QV" Text="2D-Modellierung" Help="Abschnitte mit 2D-Modellierung" Scope="Abschnitt">'
!! </ParamSetDef>'
!! <ParamSetDef Id="QU" Text="Buhnen" Help="Parameter für Buhnenfelder" Scope="Abschnitt">'
!!   <Parameter Ident="DLB" Text="Long. Disp.koeff." Unit="m²/s" Format="F7.2" Null="-1" Help="Longitudinaler Dispersionskoeffizient" Min="" Max="" Default="" />
!!   <Parameter Ident="TAU2B" Text="max. Austauschzeit" Unit="h" Format="F7.2" Null="-1" Help="Austauschzeit zwischen Buhnen und Hauptstrom" Min="" Max="" Default="" />
!!   <Parameter Ident="ALPHAB" Text="Alpha" Unit="" Format="F6.2" Null="-1" Help="Exponent beim -Breite/Tiefe-Term- zur Berechnung der longitudinalen Dispersion (nicht bei Elder) Deng: 1.67; Li: 1.3; Iwasa: 1.5" Min="" Max="" Default="" />
!!   <Parameter Ident="POMsedb" Text="Anteil org. Materials" Unit="%" Format="F6.2" Null="-1" Help="Anteil des organischen Materials im Sediment" Min="" Max="" Default="-1." />
!! </ParamSetDef>'
!! \endverbatim</code>
!! 
!! <h2> E Erosionsparameter</h2>
!! <code>\verbatim
!!  WRITE(1, '(A)') '<ParamSetDef Id="QE" Text="Erosions-Parameter" Help="Kenngrößen für die Gewässerabschnitte" Scope="Abschnitt">'
!!  WRITE(1, '(A)') '  <Parameter Ident="tau_krit" Text="kritische Sohlschubspannung ab der Erosion auftritt"       Unit="N/m²"      Format="F7.3" Null="-1" Help="" Max="" Default="9999.99" />'
!!  WRITE(1, '(A)') '  <Parameter Ident="M_eros"   Text="Erodibilitätskonstante"                                    Unit="kg/(m²*s)" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="0." />'
!!  WRITE(1, '(A)') '  <Parameter Ident="n_eros"   Text="Exponent in der Erosionsformel, potenziert den relativen Sohlspannungsüberschuss" Unit="-" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="1." />'
!!  WRITE(1, '(A)') '  <Parameter Ident="sed_roh"  Text="Dichte des liegenden Sediments"                            Unit="kg/m³"     Format="F7.3" Null="-1" Help="" Min="" Max="" Default="2650.0" />'
!!  WRITE(1, '(A)') '</ParamSetDef>'
!! \endverbatim</code>
!! \n\n
!! aus Datei modellg.f95 ; zurück zu \ref Datenmodell oder \ref lokaleParameter

!
!----+-----+----
!> Dient der eingabe() von MODELLG.3D.txt. \n siehe \ref qsimdateimodellg3 \n
!! \n\n aus modellg.f95 , zurück: \ref Datenmodell
      SUBROUTINE modellg()
      use modell
      implicit none
      character(300) dateiname, text
      integer :: open_error, string_read_error, nzon, ion, izoni, alloc_status, n, i, ini
      integer :: tfolgt, rfolgt, ifolgt, knozoanz
      logical :: vorhanden, readable
      real :: anfangsKm, endKm, zonflae

      write(dateiname,'(2A)')trim(modellverzeichnis),'MODELLG.3D.txt'
      ion=103
      open ( unit =ion , file = dateiname, status ='old', action ='read ', iostat = open_error )
      if(open_error.ne.0) then
         write(fehler,*)'open_error MODELLG.3D.txt'
         call qerror(fehler)
      end if ! open_error.ne.0
      
      if(zeile(ion))then
         print*,'MODELLG.3D.txt Versionskennzeichnung:',ctext(1:50)
      else
         write(fehler,*)'keine Versionskennzeichnung im Kopf von MODELLG.3D.txt'
         call qerror(fehler)
      end if 
      if(zeile(ion))then
         print*,'MODELLG.3D.txt Modellname:',ctext(1:50)
      else
         write(fehler,*)'keine Modellname im Kopf von MODELLG.3D.txt'
         call qerror(fehler)
      end if

      if(zeile(ion))then !Zonenanzahl
         read(ctext, *, iostat = string_read_error ) zonen_anzahl
         if(string_read_error.ne.0) then
            write(fehler,*)'string_read_error SUBROUTINE modellg nzon'
            call qerror(fehler)
         end if ! open_error.ne.0
         print*,'MODELLG.3D.txt:',zonen_anzahl, 'Zonen sollen drin sein'
      else
         write(fehler,*)'keine Zonenanzahl im Kopf von MODELLG.3D.txt'
         call qerror(fehler)
      end if !Zonenanzahl

      allocate (zone(zonen_anzahl), stat = alloc_status )
      if(alloc_status.ne.0) call qerror('allocate zone failed in modellg() zonen.f95')

      zone(:)%zonen_nummer=0 ! eingelesen Nummer der Zone
      zone(:)%ini_randnr=0 ! Randnummer mit dem initialisiert wird
      zone(:)%nr_zone=0 ! ?
      zone(:)%reib=0.0 ! 
      zone(:)%sediflux%sedom=0.0 ! Anteil des organischen Materials im Sediment
      zone(:)%sediflux%bedgs=0.0 ! Bedeckungsgrad der Sohle mit Sediment (0-1)
      zone(:)%sediflux%sedvvert=0.0 ! volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h
      zone(:)%sediflux%kornd=0.0 ! Vorgabe Korndurchmesser d50
      zone(:)%sediflux%burial=0.0 ! Burial-Geschwindigkeit (Sedimentation)
      zone(:)%seditemp%spewks=0.0 ! Spez. WärmeKapazität Sediment" unit="KJ/(kg*K)
      zone(:)%seditemp%wuebk=0.0 ! Wärmeübergangskoeffizient" unit="KJ/(K*m2*h)
      zone(:)%seditemp%psrefs=0.0 ! Reflektionsanteil der Strahlung an der Sedimentoberfläche
      zone(:)%seditemp%extiks=0.0 ! Extinktionskoeffizient für PARS
      zone(:)%laich%lait=1 ! Tag des Beginns der Laichperiode
      zone(:)%laich%laim=4 ! Monat des Beginns der Laichperiode
      zone(:)%laich%laid=7 ! Dauer der Laichperiode in Tagen
      zone(:)%schiff%vschiff=0.0 ! 
      zone(:)%schiff%uprop=0.0 ! 
      zone(:)%schiff%schifffahrts_zone=0 ! ; 1->Schiffsverkehr  , 0-> kein Schiffsverkehr; MODELLG.txt "F"
      zone(:)%wettstat%wetterstations_nummer=0 ! zugehörige Wetterstation
      zone(:)%wettstat%wetterstations_lage=0.0 ! Höhe ü. NHN
      zone(:)%dreissen%mboesch0=0.0 ! 
      zone(:)%dreissen%msohle0=0.0 ! 
      zone(:)%dreissen%gewicht0=0.0 ! 
      zone(:)%dreissen%mboesch1=0.0 ! 
      zone(:)%dreissen%msohle1=0.0 ! 
      zone(:)%dreissen%gewicht1=0.0 ! 
      zone(:)%dreissen%dreissena_aktiv=0 !
      zone(:)%albenthi%ggruen=0.0 ! 
      zone(:)%albenthi%gkiesel=0.0 ! 
      zone(:)%macrophyt%starttag=0 ! 
      zone(:)%macrophyt%startmonat=0 ! 
      zone(:)%macrophyt%maxtag=0 ! 
      zone(:)%macrophyt%maxmonat=0 ! 
      zone(:)%macrophyt%endtag=0 ! 
      zone(:)%macrophyt%endmonat=0 ! 
      zone(:)%macrodicht%pflmin=0.0 ! 
      zone(:)%macrodicht%pflmax=0.0 ! 
      zone(:)%erosi%tau_krit=9999.9 
	  zone(:)%erosi%M_eros=0.0 !
	  zone(:)%erosi%n_eros=1.0 !
	  zone(:)%erosi%sed_roh=2650.0

!     print*,'*** Lesen aus der Datei MODELLG.3D.txt ***'
      izoni=0
      tfolgt=1
      rfolgt=1
      ifolgt=1
      do while( zeile(ion)) ! Datei zeilenweise lesen ...
         readable=.false.
         !if ((ctext(1:1).eq.'S').or.(ctext(1:1).eq.'s'))then
         !### if (ctext(1:1).eq.' ')then ! Leerzeile nächster Block nächste Zone ### mit Z-Sedflux zeile
         if ((ctext(1:1).eq.' ').or.((ctext(1:1).eq.'Z').or.(ctext(1:1).eq.'z')))then ! Leer- oder Z-Zeile, nächster Block nächste Zone
            if(.not.zeile(ion))exit ! erste Zeile im Block lesen
            izoni=izoni+1
            !zone(izoni)%schifffahrts_zone=0
            if (izoni.gt.zonen_anzahl)then
               write(fehler,*)' zu viele Zonen in MODELLG.3D.txt; izoni zonen_anzahl', izoni, zonen_anzahl
               call qerror(fehler)
            end if
            read(ctext(2:2000), *, iostat = string_read_error ) zone(izoni)%zonen_nummer, zone(izoni)%zonen_name
            if(string_read_error.ne.0) then
               print*,'Block-Anfangs-Zeile aus MODELLG.3D.txt nicht ganz gelesen; string_read_error=',string_read_error
               print*,'zonen_nummer(',izoni,'),=',zone(izoni)%zonen_nummer,' zonen_name=',trim(zone(izoni)%zonen_name)
               call qerror('Lesefehler Z-Zeile MODELLG.3D.txt')
            else
               print*,'Block-Anfangs-Zeile aus MODELLG.3D.txt:',  &
     &                'zonen_nummer(',izoni,'),=',zone(izoni)%zonen_nummer,' zonen_name=',trim(zone(izoni)%zonen_name)
            end if ! 
            if(tfolgt.ne.1)call qerror('Fehler in MODELLG.3D.txt ; jede Zone braucht genau eine Wetterstation !')
            if(rfolgt.ne.1)call qerror('Fehler in MODELLG.3D.txt ; jede Zone braucht genau einen rauheitswert !')
            if(ifolgt.ne.1)  &
     &      call qerror('Fehler in MODELLG.3D.txt ; jede Zone braucht genau eine Randnr. zur Initialisierung !')
            !print*,'Zuordnung Zonen-Wetterstationen aus MODELLG.3D.txt wird noch nirgendwo gespeichert modellg.f95'
            tfolgt=0
            rfolgt=0
            ifolgt=0
         end if ! nächster Block

!! T - Wetterstationszuordnung für o.g. Zone für die Temperaturberechnung \n
         if ((ctext(1:1).eq.'T').or.(ctext(1:1).eq.'t'))then
            read(ctext(2:2000), *, iostat = string_read_error ) &
     &          zone(izoni)%wettstat%wetterstations_nummer, zone(izoni)%wettstat%wetterstations_lage 
            if(string_read_error.ne.0) call qerror('Lesefehler T-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt:','zur ',izoni,'. Zone mit Nummer ',zone(izoni)%zonen_nummer, &
     &             ' gehört Wetterstation # ', zone(izoni)%wettstat%wetterstations_nummer, &
     &             ' in der Höhe über NHN ', zone(izoni)%wettstat%wetterstations_lage
            tfolgt=tfolgt+1
         end if ! T-Zeile

!! R - Reibungsbeiwert "Rauheit" für o.g. Zone als ks-Wert nach Nikuradse (Sandrauheit) in m [neu in 3D]\n
         if ((ctext(1:1).eq.'R').or.(ctext(1:1).eq.'r'))then
            read(ctext(2:2000), *, iostat = string_read_error ) &
     &           zone(izoni)%reib
            if(string_read_error.ne.0) call qerror('Lesefehler R-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt: Sand-Rauheit ',zone(izoni)%reib,' in m zur '  &
                  ,izoni,'. Zone mit Nummer ',zone(izoni)%zonen_nummer
            if(zone(izoni)%reib.le. 0.0)call qerror('Reibungsbeiwert unzulässig')
            rfolgt=rfolgt+1
         end if ! R-Zeile

!! I - Initialisierung für o.g. Zone mittels Randbedingung, Angabe der Randnummer [neu in 3D]\n
         if ((ctext(1:1).eq.'I').or.(ctext(1:1).eq.'i'))then
            read(ctext(2:2000), *, iostat = string_read_error ) zone(izoni)%ini_randnr
            if(string_read_error.ne.0) call qerror('Lesefehler I-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt:','Zone ',izoni,' mit Nummer ',zone(izoni)%zonen_nummer &
                  ,' wird initialisiert von Randbedingung Nr. ',zone(izoni)%ini_randnr
            ifolgt=ifolgt+1
         end if ! I-Zeile

!! F - schifffahrts_zone
         if ((ctext(1:1).eq.'F').or.(ctext(1:1).eq.'f'))then
            zone(izoni)%schiff%schifffahrts_zone=1  ! ; 1->Schiffsverkehr  , 0-> kein Schiffsverkehr
            read(ctext(2:2000), *, iostat = string_read_error )  &
     &           zone(izoni)%schiff%vschiff, zone(izoni)%schiff%uprop
            if(string_read_error.ne.0) call qerror('Lesefehler F-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt: schifffahrt in Zone', izoni,' vschiff, uprop='  &
     &            ,zone(izoni)%schiff%vschiff,zone(izoni)%schiff%uprop
         end if ! F-Zeile

!! O - Verschattung durch Uferbewuchs Anteil der Vegetationstypen
         if ((ctext(1:1).eq.'O').or.(ctext(1:1).eq.'o'))then
            print*,'ACHTUNG: O-Zeile in MODELLG.3D.txt'
            write(fehler,*)'### Warnung #### Verschattung durch Uferbewuchs wird in QSim3D noch nicht implementiert.'
            call qerror(fehler)
!            read(ctext(2:2000), *, iostat = string_read_error )			&
!     &		aVeg(mstr,mV),eVeg(mstr,mV),(VTYPA(mstr,mV,iV)       		&
!     &		,iV=1,6),VALTAL(mstr,mV),EDUFAL(mstr,mV)                        &
!     &		,(VTYPA(mstr,mV,iV),iV=7,12),VALTAR(mstr,mV),EDUFAR(mstr,mV)    &
!     &		,(VTYPA(mstr,mV,iV),iV=13,14) 
         end if ! O-Zeile

!! S - Kenngrössen für Temperatur/Sedimenttemperatur
!WRITE(1, '(A)') '<ParamSetDef id="QS" text="Kenngrössen für Temperatur/Sedimenttemperatur" help="Kenngrößen für die Gewässerabschnitten" scope="Abschnitt">'
!WRITE(1, '(A)') ' <Parameter ident="SPEWKS" text="Spez. WärmeKapazität Sediment" unit="KJ/(kg*K)" format="F6.2" null="-1" help="Ton: 0.83; Sand: 0.88" min="0.8" max="4.5" default="-1">'
!WRITE(1, '(A)') ' <Parameter ident="WUEBK" text="Wärmeübergangskoeffizient" unit="KJ/(K*m2*h)" format="F7.2" null="-1" help="" min="0" max="1000" default="-1.">'
!WRITE(1, '(A)') ' <Parameter ident="PSREFS" text="Reflektionsanteil der Strahlung an der Sedimentoberfläche" unit="-" format="F5.2" null="-1" help="" min="0" max="1" default="-1.">'
!WRITE(1, '(A)') ' <Parameter ident="EXTKS" text="Extinktionskoeffizient für PARS (nur bei Temperaturmodellierung erforderlich!)" unit="-" format="F5.2" null="-1" help="" min="" max="" default="-1.">'
!read(77,1047)aKSED(mstr,mA),eKSED(mstr,mA),SPEWKSx(mstr,mA),WUEBKx(mstr,mA),PSREFSx(mstr,mA),extkx(mstr,mA)                                    
         if ((ctext(1:1).eq.'S').or.(ctext(1:1).eq.'s'))then
            read(ctext(2:2000), *, iostat = string_read_error ) &
     &          zone(izoni)%seditemp%spewks, zone(izoni)%seditemp%wuebk,  &
     &          zone(izoni)%seditemp%psrefs, zone(izoni)%seditemp%extiks
            if(string_read_error.ne.0) call qerror('Lesefehler S-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt:','Sedimenttemperatur_zone zonen_nummer(izoni)', zone(izoni)%zonen_nummer, izoni
            print*,'spewks,wuebk,psrefs,extiks',  &
     &            zone(izoni)%seditemp%spewks, zone(izoni)%seditemp%wuebk,  &
     &            zone(izoni)%seditemp%psrefs, zone(izoni)%seditemp%extiks
            readable=.true.
         end if ! S-Zeile

!! Z - Sediment-Kenngrößen, Belegungen für Stoffumsatz im Sediment
!!###      read(77,1045)aPOM(mstr,mZ),ePOM(mstr,mZ),POMz(mstr,mZ),BedGSz(mstr,mz),Sedvvertz(mstr,mz)                                    
!!      zone(:)%sediflux%sedom=0.0 ! Anteil des organischen Materials im Sediment
!!      zone(:)%sediflux%bedgs=0.0 ! Bedeckungsgrad der Sohle mit Sediment (0-1)
!!      zone(:)%sediflux%sedvvert=0.0 ! volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h
!!      zone(:)%sediflux%kornd=0.0 ! Vorgabe Korndurchmesser d50
!!      zone(:)%sediflux%burial=0.0 ! Burial-Geschwindigkeit (Sedimentation)
!         if ((ctext(1:1).eq.'Z').or.(ctext(1:1).eq.'z'))then
!            read(ctext(2:2000), *, iostat = string_read_error )  &
!     &          zone(izoni)%sediflux%sedom,zone(izoni)%sediflux%bedgs,zone(izoni)%sediflux%sedvvert
!            if(string_read_error.ne.0)then
!               call qerror('Lesefehler Z-Zeile MODELLG.3D.txt')
!            end if ! Lesefehler Z-Zeile
!            readable=.true.
!            ! optional Korndurchmesser und burialgeschwindigkeit
!            read(ctext(2:2000), *, iostat = string_read_error ) dummy,dummy,dummy,dummy,dummy,kornd(izoni)
!            if(string_read_error.ne. 0) kornd(izoni)=0.0
!            read(ctext(2:2000), *, iostat = string_read_error ) dummy,dummy,dummy,dummy,dummy,dummy,burial(izoni)
!            if(string_read_error.ne. 0) burial(izoni)=0.0
!            print*,'Z-Zeile in MODELLG.3D.txt: ,izoni,sedom,bedgs,sedvvert,kornd,burial='  &
!     &            ,izoni,sedom(izoni),bedgs(izoni),sedvvert(izoni),kornd(izoni),burial(izoni)
!         end if ! Z-Zeile

!! L - Kenngrössen für Laichperiode Muscheln Dreissena
!! read(77,2306)laits(mstr),laims(mstr),laids(mstr) 
         if ((ctext(1:1).eq.'L').or.(ctext(1:1).eq.'l'))then
            read(ctext(2:2000), *, iostat = string_read_error ) &
     &          zone(izoni)%laich%lait, zone(izoni)%laich%laim, zone(izoni)%laich%laid
            if(string_read_error.ne.0) call qerror('Lesefehler L-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt:','Laichperiode zonen_nummer(izoni)', zone(izoni)%zonen_nummer, izoni
            print*,'lait, laim, laid',zone(izoni)%laich%lait, zone(izoni)%laich%laim, zone(izoni)%laich%laid
         end if ! L-Zeile

!! D - Dreissena-Bewuchs in den Gewässer-Abschnitten
!  WRITE(1, '(A)') '<ParamSetDef Id="QD" Text="Dreissena" Help="Dreissena-Bewuchs in den Gewässer-Abschnitten" Scope="Abschnitt">'
!  WRITE(1, '(A)') '  <Parameter Ident="mboesch0" Text="Biomasse 0.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte (Schalenlänge kl. 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />'
!  WRITE(1, '(A)') '  <Parameter Ident="msohle0" Text="Biomasse 0.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />'
!  WRITE(1, '(A)') '  <Parameter Ident="gewicht0" Text="Mittl. Muschelgewicht 0.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 0. Kohorte" Min="" Max="" Default="" />'
!  WRITE(1, '(A)') '  <Parameter Ident="mboesch1" Text="Biomasse 1.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte (Schalenlänge gr.= 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />'
!  WRITE(1, '(A)') '  <Parameter Ident="msohle1" Text="Biomasse 1.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />'
!  WRITE(1, '(A)') '  <Parameter Ident="gewicht1" Text="Mittl. Muschelgewicht 1.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 1. Kohorte." Min="" Max="" Default="" />'
         if ((ctext(1:1).eq.'D').or.(ctext(1:1).eq.'d'))then
            read(ctext(2:2000), *, iostat = string_read_error )  &
     &           zone(izoni)%dreissen%mboesch0, zone(izoni)%dreissen%msohle0, zone(izoni)%dreissen%gewicht0   &
     &          ,zone(izoni)%dreissen%mboesch1, zone(izoni)%dreissen%msohle1, zone(izoni)%dreissen%gewicht1
            if(string_read_error.ne.0) call qerror('Lesefehler D-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt: Dreissena-Bewuchs in zonen_nummer(izoni)', zone(izoni)%zonen_nummer, izoni    &
     &            ,'msohle0,gewicht0,msohle1,gewicht1',zone(izoni)%dreissen%msohle0, zone(izoni)%dreissen%gewicht0   &
     &            ,zone(izoni)%dreissen%msohle1, zone(izoni)%dreissen%gewicht1
            if( (zone(izoni)%dreissen%msohle0+zone(izoni)%dreissen%msohle1).gt. 0.0) zone(izoni)%dreissen%dreissena_aktiv = 1 ! muscheln nur aktiv wenn vorbelegt
         end if ! D-Zeile

!! B Benthische Algen
!! subroutine ModellGParam(cpfad1,j1)\n
!!  WRITE(1, '(A)') '<ParamSetDef Id="QB" Text="Benth.Algen" Help="Benth.Algen-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt"\n
!!  WRITE(1, '(A)') '  <Parameter Ident="GGruen" Text="Gewicht Grünalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Grünalgen \n
!!  WRITE(1, '(A)') '  <Parameter Ident="GKiesel" Text="Gewicht Kieselalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Kieselalgen \n
         if ((ctext(1:1).eq.'B').or.(ctext(1:1).eq.'b'))then
            read(ctext(2:2000), *, iostat = string_read_error )  &
     &           zone(izoni)%albenthi%ggruen,zone(izoni)%albenthi%gkiesel
            if(string_read_error.ne.0) call qerror('Lesefehler B-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt: Benthische Algen in Zone', izoni,' albenthi%ggruen , albenthi%gkiesel ='  &
     &            ,zone(izoni)%albenthi%ggruen,zone(izoni)%albenthi%gkiesel
         end if ! B-Zeile

!! M  Makrophyten
!!  WRITE(1, '(A)') '<ParamSetDef Id="QM" Text="Makrophyten" Help="Makrophyten-Wachstum" Scope="Strang">'
!!  WRITE(1, '(A)') '  <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Wachstumsbeginns der Makrophyten" Min="1" Max="31" Default="" />'
!!  WRITE(1, '(A)') '  <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Wachstumsbeginns der Makrophyten" Min="1" Max="12" Default="" />'
!!  WRITE(1, '(A)') '  <Parameter Ident="MaxTag" Text="Max.-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="31" Default="" />'
!!  WRITE(1, '(A)') '  <Parameter Ident="MaxMonat" Text="Max.-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="12" Default="" />'
!!  WRITE(1, '(A)') '  <Parameter Ident="EndTag" Text="Ende-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Minimum erreicht hat. Hier endet das Makrophytenwachstum" Min="1" Max="31" Default="" />'
!!  WRITE(1, '(A)') '  <Parameter Ident="EndMonat" Text="Ende-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Minimum erreicht hat" Min="1" Max="12" Default="" />'
         if ((ctext(1:1).eq.'M').or.(ctext(1:1).eq.'m'))then
            read(ctext(2:2000), *, iostat = string_read_error )  &
     &           zone(izoni)%macrophyt%starttag,zone(izoni)%macrophyt%startmonat,  &
     &           zone(izoni)%macrophyt%maxtag,zone(izoni)%macrophyt%maxmonat,  &
     &           zone(izoni)%macrophyt%endtag,zone(izoni)%macrophyt%endmonat
            if(string_read_error.ne.0) call qerror('Lesefehler M-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt: Makrophyten-Wachstum in Zone', izoni,' maxtag='  &
			      ,zone(izoni)%macrophyt%maxtag
         end if ! M-Zeile

!! P Dichte der Makrophyten
!!  WRITE(1, '(A)') '<ParamSetDef Id="QP" Text="Dichte der Makrophyten" Help="Makrophyten-Dichte" Scope="Abschnitt \n
!!  WRITE(1, '(A)') '  <Parameter Ident="PflMin" Text="min. Dichte (Winter)" Unit="g/m²" Format="F7.2" Null="-1" Help="Minimale Dichte der Makrophyten im Winter" \n
!!  WRITE(1, '(A)') '  <Parameter Ident="PflMax" Text="max. Dichte (Sommer)" Unit="g/m²" Format="F7.2" Null="-1" Help="Maximale Dichte der Makrophyten im Sommer" \n
         if ((ctext(1:1).eq.'P').or.(ctext(1:1).eq.'p'))then
            read(ctext(2:2000), *, iostat = string_read_error )  &
     &           zone(izoni)%macrodicht%pflmin,zone(izoni)%macrodicht%pflmax
            if(string_read_error.ne.0) call qerror('Lesefehler P-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt: Dichte der Makrophyten in Zone', izoni,' %macrodicht%pflmin , %macrodicht%pflmax ='  &
			      ,zone(izoni)%macrodicht%pflmin,zone(izoni)%macrodicht%pflmax
         end if ! P-Zeile
		 
!! E Erosionsparameter
!!  WRITE(1, '(A)') '<ParamSetDef Id="QE" Text="Erosions-Parameter" Help="Kenngrößen für die Gewässerabschnitte" Scope="Abschnitt">'
!!  WRITE(1, '(A)') '  <Parameter Ident="tau_krit" Text="kritische Sohlschubspannung ab der Erosion auftritt"       Unit="N/m²"      Format="F7.3" Null="-1" Help="" Max="" Default="9999.99" />'
!!  WRITE(1, '(A)') '  <Parameter Ident="M_eros"   Text="Erodibilitätskonstante"                                    Unit="kg/(m²*s)" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="0." />'
!!  WRITE(1, '(A)') '  <Parameter Ident="n_eros"   Text="Exponent in der Erosionsformel, potenziert den relativen Sohlspannungsüberschuss" Unit="-" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="1." />'
!!  WRITE(1, '(A)') '  <Parameter Ident="sed_roh"  Text="Dichte des liegenden Sediments"                            Unit="kg/m³"     Format="F7.3" Null="-1" Help="" Min="" Max="" Default="2650.0" />'
!!  WRITE(1, '(A)') '</ParamSetDef>'
         if ((ctext(1:1).eq.'E').or.(ctext(1:1).eq.'e'))then
            read(ctext(2:2000), *, iostat = string_read_error )  &
     &           zone(izoni)%erosi%tau_krit, zone(izoni)%erosi%M_eros, zone(izoni)%erosi%n_eros, zone(izoni)%erosi%sed_roh
            if(string_read_error.ne.0) call qerror('Lesefehler E-Zeile MODELLG.3D.txt')
            print*,'MODELLG.3D.txt: Erosionsparameter in Zone', izoni,' tau_krit, M_eros,  n_eros,  sed_roh='  &
     &            ,zone(izoni)%erosi%tau_krit, zone(izoni)%erosi%M_eros, zone(izoni)%erosi%n_eros, zone(izoni)%erosi%sed_roh
         end if ! e-Zeile

      end do ! while(zeile(ion))
      close (ion) 


      if (izoni.ne.zonen_anzahl)then
         !zonen_anzahl=izoni
         write(fehler,*)'Zonen-Anzahl',izoni,' ungleich der im Kopf von MODELLG.3D.txt angegeben ',zonen_anzahl
         call qerror(fehler)
      end if   

      do i=1,zonen_anzahl
         do n=i+1,zonen_anzahl
            if( zone(i)%zonen_nummer.eq.zone(n)%zonen_nummer)then
               write(fehler,*)' 8 Zonennummer von Zone',i ,' =',zone(i)%zonen_nummer,' ist gleich Zonennummer von Zone',n
               call qerror(fehler)
            endif
         end do ! alle n Zonen
         if(zone(i)%reib.le. 0.0)then
               write(fehler,*)'Reibungsbeiwert',zone(i)%reib,' von Zone',i,' ist falsch '
               call qerror(fehler)
            endif
      end do ! alle i Zonen

      do n=1,number_plankt_point
         vorhanden=.false.
         do i=1,zonen_anzahl
            if( point_zone(n).eq.zone(i)%zonen_nummer ) then
               if(.not.vorhanden)point_zone(n)=i
               vorhanden=.true. ! zone vorhanden + zugeordnet
            end if !zonen_nummer vorhanden
         end do ! alle i Zonen
         if(.not.vorhanden)then
            write(fehler,*)'2 Die von Knoten #',n ,' benötigte Zonennummer #',point_zone(n)  &
                           , 'ist nicht in MODELLG.3D.txt beschrieben'
            call qerror(fehler)
         end if ! nicht vorhanden
      end do ! alle n Knoten
      print*,'MODELLG.3D.txt: an allen Knoten wurde die Zonennummer'  &
            ,' in den Zonenzähler korrekt umgewandelt (point_zone()).' 

      select case (hydro_trieb)
      case(1) ! casu-transinfo                                           
         do i=1,zonen_anzahl
            knozoanz=0
            zonflae=0.0
            do n=1,knotenanzahl2D
               if(knoten_zone(n).eq.i)then
                  knozoanz=knozoanz+1
                  zonflae=zonflae+knoten_flaeche(n)
               endif ! knoten in zone
            end do ! alle n Knoten
            print*,'MODELLG.3D.txt: Die ',i,'-te Zone hat die Nummer ',zone(i)%zonen_nummer   &
     &            ,'heißt: ',trim(zone(i)%zonen_name)  &
     &            ,', enthält ',knozoanz,' Knoten und bedeckt eine Fläche von ',zonflae, ' m**2'
         end do ! alle i Zonen

      case(2) ! Untrim² netCDF
         do i=1,zonen_anzahl
            knozoanz=0
            do n=1,n_elemente 
               if(element_zone(n).eq.i) knozoanz=knozoanz+1
            end do ! alle n Elemente
            print*,'MODELLG.3D.txt: Die ',i,'-te Zone hat die Nummer ',zone(i)%zonen_nummer   &
     &            ,'heißt: ',trim(zone(i)%zonen_name)  &
     &            ,' und enthält ',knozoanz,' Elemente.'
         end do ! alle i Zonen

      case(3) ! SCHISM netCDF
         do i=1,zonen_anzahl
            knozoanz=0
            zonflae=0.0
            do n=1,knotenanzahl2D
               if(knoten_zone(n).eq.i)then
                  knozoanz=knozoanz+1
                  zonflae=zonflae+knoten_flaeche(n)
               endif ! knoten in zone
            end do ! alle n Knoten
            print*,'MODELLG.3D.txt: Die ',i,'-te Zone hat die Nummer ',zone(i)%zonen_nummer   &
     &            ,'heißt: ',trim(zone(i)%zonen_name)  &
     &            ,', enthält ',knozoanz,' Knoten und bedeckt eine Fläche von ',zonflae, ' m**2'
         end do ! alle i Zonen
         !!!### call sc_read_rough()
      case default
         call qerror('modellg Hydraulischer Antrieb unbekannt')
      end select

      RETURN
      END subroutine modellg


!----+-----+----
!      
