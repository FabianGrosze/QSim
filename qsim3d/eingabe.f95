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
!> \page Modellerstellung Modellerstellung
!! 
!! \section strukda Stuktur des Datenmodells 
!! Um eine Gewässergüte-Simulation (in QSim als "Ereignis" bezeichnet)
!! in einem Gewässerabschnitt (Modell) durchführen zu können, ist ein Daten-Modell erforderlich.\n
!! Dabei müssen Vorgaben zu den folgenden Punkten gemacht werden:\n 
!! <ul>
!! <li> \subpage Netz  </li>
!! <li> \subpage Ablaufsteuerung </li>
!! <li> \ref Transportinformationen </li>
!! <li> \subpage Anfangsbedingungen  </li>
!! <li> \subpage zuflussranddaten </li>
!! <li> \subpage globaleParameter  </li> 
!! <li> \subpage lokaleParameter </li>
!! </ul>
!!\n\n
!! Informationstechnisch wird ein Daten-Modell für QSim-3D durch ein Verzeichnis definiert, 
!! das im weiteren als <b>Modellverzeichnis</b> bezeichnet wird.\n
!! In diesem befindet sich ein Satz von Dateien (deren Vollständigkeit beim Programmstart von QSim-3D abgeprüft wird). \n
!! Im Abschnitt \subpage Datenmodell finden sie eine detaillierte Beschreibung der einzelnen Dateien.\n
!! 
!! \section Gerris Modellerstellung mit der Benutzeroberfläche Gerris 
!! Die Bedienung von QSim-1D mittels Gerris kann im 
!! <a href="./pdf/BfG_1778.pdf" target="_blank">BfG-Bericht 1778</a> nachgelesen werden.
!! <a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=gs+1697" target="_blank">   
!! (Bibliotheks-Exemplar)</a> 
!! \n\n
!! Eine Anleitung zur Benutzung von Gerris für mehrdimensionale Modelle ist z.Z. (Feb.2018) noch in Arbeit.
!! \n\n
!! Für die Kopplung von Gerris mit QSim3D existiert eine <a href="./pdf/QSim3D_Gerris_Schnittstelle_1p0.pdf" target="_blank">Schnittstellenbeschreibung</a>
!! (auch im <a href="./html/Qsim3d_Gerris_Schnittstelle/index.html" target="_blank">html-Format</a>);\n
!! desweiteren wurde ein <a href="./pdf/Gerris123D.Test.3D.1.0.pdf" target="_blank">Testbericht für die Gerris 3D NetCDF Erweiterung (casu)</a>, 
!! sowie ein <a href="./pdf/Gerris123D_Test_3D_NetCDF_0p1.pdf" target="_blank">Testbericht für die Gerris 3D NetCDF Erweiterung (Untrim)</a> angefertigt.
!! \n\n
!! Die grafische Benutzeroberfläche GERRIS wurde 
!! auf der Basis des <a href="./pdf/Gerris123D.Feinkonzept.3D.1.0.pdf" target="_blank">DV-technischen Feinkonzepts</a>
!! vom <a href="http://www.wasserundumwelt.de" target="_blank">Ingenieurbüro Schumacher</a>  produziert.
!! \n\n
!! Die Netzerstellung und Zonierung erfolgt mit dem externen Netzgenerator Janet
!! \n\n
!! Für Modellbedienung mittels Gerris ist es erforderlich, vorher eine ssh-Verbindung 
!! zwischen dem eigenen Arbeits-PC, auf dem Gerris installiert ist und dem server (HPC), auf dem gerechnet werden soll, herzustelllen.
!! Details dazu enthalten die Dokumente: 
!! <a href="./pdf/Gerris3D_nachvollzogenJens.pdf" target="_blank">Gerris3D_nachvollzogen</a> 
!! und <a href="./pdf/Schluesselkonvertierung.pdf" target="_blank">Schluesselkonvertierung</a>
!! \n\n 
!! Die Kommunikation zwischen Gerris und dem Server erfolgt, indem Scripte auf dem Server gestartet werden. 
!! Diese kommunikation wird in den 3D Optionen eingerichtet:
!! \n\n
!! <em> hier fehlt noch das Bild (siehe Code, auskommentiert) </em>
! ! \image html 3Doptionen.png 
!! \n\n 
!!! Server-URL\n
!! 	URL oder ip-Adresse des Servers, auf dem QSim3D gerechnet weden soll. Zwischen dem Server und dem lokalen Arbeitsrechner muss eine ssh-Verbindung möglich sein.
!! \n\n
!! User-Name \n
!! 	Name des Benutzers, mit dem auf dem Server gerechnet werden soll. Die ssh-Anmeldung erfolgt über diesen Benutzer.
!! \n\n
!! Ereignisliste-Skript \n
!! 	Pfad zu einem Skript auf dem Server, welches eine Liste aller auf dem Server vorhandenen hydraulischen Modelle/Ereignisse auf die Standartausgabe ausgibt.
!! \n\n
!! 	Jede Zeile der Ausgabe steht dabei für ein Modell/Ereignis und besteht aus vier bis sechs Feldern, die durch ein Semikolon(;) getrennt sind:
!! \n\n
!!<table Ereignisliste>
!!<tr><td> Verzeichnispfad  </td><td> Vollständiger Pfad des Modellverzeichnisses auf dem Server	</td></tr>
!!<tr><td> Startzeitpunkt  </td><td> Startzeitpunkt des Ereignisses im Format Jahr-Monat-Tag Stunde:Minute:Sekunde	</td></tr>
!!<tr><td> Endzeitpunkt  </td><td> Endzeitpunkt des Ereignisses im Format Jahr-Monat-Tag Stunde:Minute:Sekunde	</td></tr>
!!<tr><td> Zeitschrittweite  </td><td> Zeitschrittweite des Ereignisses in Sekunden als ganze Zahl	</td></tr>
!!<tr><td> Modellname  </td><td> optional: Modellname	</td></tr>
!!<tr><td> Projektname  </td><td> optional: Projektname (nur wenn auch Modellname angegegeben)	</td></tr>
!!</table>
!! \n\n
!! 	Beispiel Ausgabezeile: \n
!! <code>\verbatim
!! 	/home/username/fluss/;2011-01-01 00:00:00;2011-08-15 00:00:00;600;Flussmodell;Flussprojekt
!! \endverbatim</code>
!! \n\n
!! QSim-3D-Skript \n
!! 	Pfad zu einem Skript auf dem Server, welches den QSim3D-Rechenlauf startet.
!! 	Wird in Gerris ein Rechenlauf gestartet, wird dieses Skript zusammen mit dem Projektverzeichnis als Parameter aufgerufen. Im Projektverzeichnis wird dadurch das Skript "qsim3d.sbatch" ausgeführt.
!! 	\n
!! 	Dabei müssen in "qsim3d.sbatch" alle für den Rechenlauf relevanten SLURM-Parameter definiert und das QSim-Executable zusammen mit dem Projektverzeichnis aufgerufen werden.
!! \n\n
!! Definitionen-Skript \n
!! 	Pfad zu einem Skript auf dem Server, welches Definitionsdateien, die an der Schnittstelle von Qsim3d und Gerris benötigt werden, bereitstellt. Aus einem zentralen Verzeichnis werden dazu die folgenden Dateien in das Projektverzeichnis kopiert.
!! 		AParamParam.xml 						enthält Definitionen der globalen QSim-Modellparameter
!! 		ausgabekonzentrationen_beispiel.txt 	enthält eine Liste der Namen der Modell-Variablen, die von der aktuellen QSim-3D Version ausgegeben werden können
!! 		EreigGParam.xml							enthält Definitionen der Ereignisparameter
!! 		ModellG3Param.xml						enthält Definitionen der Zonen-Parameter
!! 		WetterParam.xml							enthält Definitionen der Wetter-Parameter
!! \n\n
!! IsoQ-Skript \n
!! 	Pfad zu einem Skript auf dem Server, welches die Visualisierung QSim-3D-Ergebnissen ermöglicht.
!! \n\n
!! Ncdump \n
!! 	Name des Programmaufrufs, um auf dem Server Daten aus NetCDF-Dateien zu lesen.
!! \n\n
!! E-Mail \n
!! 	E-Mail-Adresse, an die eine Benachrichtigung bei Berechnungsende geschickt wird.
!! \n\n
!! aus Datei eingabe.f95 ; zurück zu \ref lnk_modellstruktur

!> \page Datenmodell Datenmodell
!! <h1>Bestandteile des Datenmodells</h1>
!! Um das Modell und die damit durchzuführende Berechnung zu definieren
!! werden von QSim-3D die folgenden Dateien eingelesen.
!! \n\n
!! Dabei wird das Konzept verfolgt, soweit wie möglich, die selben Dateiformate zu verwenden wie QSim.\n
!! Hier werden daher nur die Änderungen beschrieben die sich im Bezug zur\n
!! <a href="./pdf/Schnittstelle_QSIM2004.pdf" target="_blank">Schnittstellenbeschreibung 2004 Gerris-QSim</a> 
!! <a href="./pdf/schnittstellenbeschreibung2006_gerris.pdf" target="_blank">Schnittstellenbeschreibung 2006 Gerris-QSim</a>\n
!! ergeben.
!! \n\n
!!<table >
!!<tr><th> Datei-Name\n(Spezifikation verlinkt)				</th><th> Format </th><th> Änderung zu QSim	</th><th> Kurz-Beschreibung		</th><th> wird gelesen von (Details) </th></tr>
!!<tr><td> Gütemodell </td></tr>
!!<tr><td> \subpage qsimdateiereigg </td>	<td> ascii </td><td> keine		</td><td> \ref Ablaufsteuerung </td><td> ereigg_modell(), ereigg_Randbedingungen_lesen() </td></tr>
!!<tr><td> \subpage qsimdateimodella </td>	<td> ascii </td><td> keine		</td><td> es werden von QSim-3D nur noch Modellname und die Geologischen Breiten- und Längenkoordinaten daraus gelesen </td><td> modella() </td></tr>
!!<tr><td> \subpage qsimdateiwetter </td>	<td> ascii </td><td> keine		</td><td> \ref wetter_rb </td><td> wetter_readallo0() </td></tr>
!!<tr><td> \subpage qsimdateimodellg3 </td>	<td> ascii </td><td> ja, ehemals ModellG.txt </td><td> \ref lokaleParameter und \ref Anfangsbedingungen</td><td> modellg() </td></tr>
!!<tr><td><a href="./exp/APARAM_200314.txt" target="_blank">APARAM.txt</a></td><td>ascii</td><td>  keine		</td><td> \ref globaleParameter </td><td> aparam_lesen() </td></tr>
!!<tr><td><a href="./exp/e_extnct.dat" target="_blank">e_extnct.dat</a></td><td>ascii</td><td>  keine		</td><td> \subpage extnct_rb </td><td> e_extnct_lesen() </td></tr>
!!<tr><td> Hydraulischer Treiber </td></tr>
!!<tr><td><a href="./exp/points" target="_blank">points</a> <br>		</td><td>ascii</td><td> neu, wird von casu (transinfo) ausgegeben. Nicht die converti-Version verwenden! </td><td> Infos zu den knoten: Lage (x+y), Höhe (z), Zonen_nummer(n), Randnummer, Zellfläche </td><td> points() </td></tr>
!!<tr><td><a href="./exp/file.elements" target="_blank">file.elements</a>	</td><td>ascii</td><td> converti, casu-eingabe-Datei	</td><td> Vermaschung Netz, wird zur Ergebnis-Darstellung benötigt	</td><td> elements() </td></tr>
!!<tr><td><a href="./exp/transinfometa" target="_blank">transinfo/meta</a>  </td><td>ascii</td><td>	neu, wird von casu ausgegeben	</td><td> Meta-Infos zu den Transportinformationen </td><td> transinfo_sichten() </td></tr>
!!<tr><td>  transinfo/t*							</td><td>binär</td><td> neu, wird von casu ausgegeben	</td><td> \ref Transportinformationen </td><td> holen_trans() </td></tr>
!!<tr><td> Ausgabesteuerung </td></tr>
!!<tr><td><a href="./exp/ganglinien_knoten.txt" target="_blank">ganglinien_knoten.txt</a></td><td>ascii</td><td> neu </td><td> Knotennummern, an denen Ganglinien ausgegeben werden sollen </td><td> ganglinien_lesen(), ganglinien_zeitschritt(), ganglinien_schliessen() </td></tr>
!!<tr><td><a href="./exp/ausgabezeitpunkte.txt" target="_blank">ausgabezeitpunkte.txt</a></td><td>ascii</td><td> neu </td><td> Zeitpunkte, zu denen komplette Konzentrationsfelder ausgegeben werden sollen </td><td> ausgabezeitpunkte() </td></tr>
!!<tr><td><a href="./exp/ausgabekonzentrationen.txt" target="_blank">ausgabekonzentrationen.txt</a> ausgabekonzentrationen_beispiel.txt </td><td>ascii</td><td> neu	</td><td>Angabe, welche Konzentrationen als Ganglinen und als komplette Konzentrrationsfelder ausgegeben werden sollen (Liste zum Ankreuzen x=ja 0=nein). Die Liste aller verfügbaren Variablenn wird von qsim3d als "ausgabekonzentrationen_beispiel.txt" bei jedem lauf angegeben.</td><td> ausgabekonzentrationen() </td></tr>
!!<tr><td><a href="./exp/kilonummer" target="_blank">kilonummer</a></td><td>ascii</td><td> neu	</td><td>Zuordnung von Längsschnittknoten zu Kilometern </td><td> \ref laengsschnitt </td></tr>
!!<tr><td><a href="./exp/email.txt" target="_blank">email.txt</a></td><td>ascii</td><td> neu	</td><td>eine Zeile mit der Emailadresse unter der/die Bearbeiter/in über das Ende des Rechenlaufes unterrichtet werden möchte</td><td> modell_vollstaendig() </td></tr>
!!</table>\n\n
!! Ein- und Ausgabe sind nicht parallelisiert, d.h. sie laufen nur auf Prozess 0 !
!!\n\n
!! \subpage qsimdateikopfzeilev
!!\n\n
!! aus Datei eingabe.f95 ; zurück zu \ref Modellerstellung

!> \page qsimdateimodella ModellA.txt
!! Die Datei ModellA.txt definiert für QSim-1D alle Stränge des eindimensionalen Modells mit ihren Stationen, Wehren und Randbedingungen.
!! \n\n
!! In QSim-3D enthält die Datei lediglich die Kopfzeilen und die Angabe einer geographischen Position des Modells.
!! \n\n
!! Beispiel:\n
!! <a href="./exp/MODELLA.txt" target="_blank">MODELLA.txt</a>
!! \n\n
!! <table>
!! <tr>
!! <th>Zeile</th>
!! <th>Inhalt</th>
!! </tr>
!! <tr>
!! <td>Zeile 1</td>
!! <td>Versions-Kopfzeile\n Die Zeile wird von Gerris benutzt und von QSim-3D nicht ausgewertet.</td>
!! </tr>
!! <tr>
!! <td>Zeile 2</td>
!! <td>Modellname und ggf. weitere Angaben, maximal 255 Zeichen.\n Die Zeile wird von Gerris benutzt und von QSim-3D nicht ausgewertet.</td>
!! </tr>
!! <tr>
!! <td>Zeile 3</td>
!! <td>Für das Modellgebiet repräsentative geographische Breite und Länge, jeweils als Dezimalgrad und im Format F5.2.</td>
!! </tr>
!! </table>
!!\n
!! aus Datei eingabe.f95 ; zurück zu \ref Datenmodell

!> \page Ablaufsteuerung Ereignis-Steuerung / Zeit-Diskretisierung 
!! Die Angabe, welcher \n
!! - Zeitraum (Ereignis) mit welchem \n
!! - Zeitschritt (konstant), \n
!! d. h. die Zeitdiskretisierung wird in der Datei \ref qsimdateiereigg vorgegeben.
!! \n\n
!! aus Datei eingabe.f95 ; zurück zu \ref Modellerstellung oder \ref Datenmodell

!> \page qsimdateiereigg EreigG.txt
!! In der Datei EreigG.txt wird das zu simulierende Ereignis definiert.
!! Das Ereignis umfasst einen definierten Zeitraum.
!! Die Simulation erfolgt in äquidistanten Zeitschritten, deren Schrittweite und Anzahl hier angegeben ist.
!! \n\n
!! Für jede Randbedingung gemäß <a href="./exp/points" target="_blank">points-Datei</a> werden ein Werte-Satz (konstante Verhältnisse im ganzen Ereignis)
!! oder mehrere Werte-Sätze angegeben (Ganglinie).\n
!! Die Ganglinien können ein unregelmäßiges Zeitschema haben.
!! Das Zeitschema der verschiedenen Randbedingungen kann verschieden sein.
!! QSim-3D interpoliert fehlende Zeitschritte.
!! \n\n
!! Beispiel:\n
!! <a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a>
!! \n\n
!! <h2>Gliederung der Datei</h2>
!! <ul>
!! <li>Kopfbereich</li>
!! <li>Detailbereich</li>
!! </ul>
!! Der Detailbereich besteht mehreren Blöcken, je einem pro Randbedingung des Modells.\n
!! Jeder Randbedingungsblock besteht aus einer Kopfzeile und mindestens einer Wertzeile.
!! \n\n
!! <h2>Kopfbereich</h2>
!! Der Kopfbereich hat sechs Zeilen.
!! \n\n
!! <table>
!! <tr>
!! <th>Position</th>
!! <th>Inhalt</th>
!! </tr>
!! <tr>
!! <td>Kopf, 1.&nbsp;Zeile</td>
!! <td>\ref qsimdateikopfzeilev "Versions-Kopfzeile" mit "EreigG" als Dateityp</td>
!! </tr>
!! <tr>
!! <td>Kopf, 2.&nbsp;Zeile</td>
!! <td>Modellname und ggf. weitere Angaben, maximal 255 Zeichen.\n
!!   Die Zeile wird von QSim-3D nicht ausgewertet.</td>
!! </tr>
!! <tr>
!! <td>Kopf, 3.&nbsp;Zeile</td>
!! <td>Ereignisname und ggf. weitere Angaben, maximal 255 Zeichen.\n
!!   Die Zeile wird von QSim-3D nicht ausgewertet.</td>
!! </tr>
!! <tr>
!! <td>Kopf, 4.&nbsp;Zeile</td>
!! <td>Datum und Uhrzeit des Ereignisstarts in vier Feldern.</td>
!! </tr>
!! <tr>
!! <td>Kopf, 5.&nbsp;Zeile</td>
!! <td>Datum und Uhrzeit des Ereignisendes in vier Feldern.\n
!!   In einem fünften Feld folgt die Zeitschrittweite der Berechnung in Minuten.</td>
!! </tr>
!! <tr>
!! <td>Kopf, 6.&nbsp;Zeile</td>
!! <td>Verschiedene Optionen und Steuerwerte der QSim-Berechnung, wie im Folgenden beschrieben.</td>
!! </tr>
!! </table>
!! <h3>Kopfbereich - Optionszeile</h3>
!! Bis auf das 10. Feld haben alle Felder die Länge 1.
!! \n\n
!! <table>
!! <tr>
!! <th>Position</th>
!! <th>Inhalt</th>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile,&nbsp;1.&nbsp;Feld</td>
!! <td>Ausgabe-Option, für QSim-3D ohne Bedeutung.</td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile, 2.&nbsp;Feld</td>
!! <td>Option pH-Berechnung: 0 = nein, 1 = ja.</td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile, 3.&nbsp;Feld</td>
!! <td>Option Dispersionskoeffizient: 1 = Einlesen, 0 = Berechnen.</td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile, 4.&nbsp;Feld</td>
!! <td>Option Temperaturberechnung: 1 = nur Temperaturberechnung, 0 = alle Berechnungen.</td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile, 5.&nbsp;Feld</td>
!! <td>Option Tracer: 1 = nur Tracerberechnung, 0 = alle Berechnungen.</td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile, 6.&nbsp;Feld</td>
!! <td>Option Erosion: 1 = mit Erosion, 0 = ohne Erosion.</td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile, 7.&nbsp;Feld</td>
!! <td>Option Abweichung, für QSim-3D ohne Bedeutung.</td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile, 8.&nbsp;Feld</td>
!! <td>Nummer der zu verwendenden Transportgleichung gemäß den in <a href="./exp/EreigGParam.xml" 
!! target="_blank">EreigGParam.xml</a> definierten (TpEquations-Element).\n
!! iverfahren Adv.Diff. 1-cip, 2-Lax_wendroff, 3-ultimate Quickest </td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile, 9.&nbsp;Feld</td>
!! <td>Nummer der zu verwendenden Methode zur Berechnung des longitudinalen Dispersionskoeffizienten
!!   gemäß den in <a href="./exp/EreigGParam.xml" target="_blank">EreigGParam.xml</a> definierten (DlEquations-Element).\n
!!   Wird von QSim-3D nur berücksichtigt, wenn der Schalter für den Dispersionskoeffizienten (Feld 3) auf 0 gesetzt ist.\n
!!  ilongDis long.Disp.koeff. 1-Deng 2-Li 3-Iwasa 4-Elder</td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile,&nbsp;10.&nbsp;Feld</td>
!! <td>Gewichtungsfaktor im Format F4.2 für die Berechnung des longitudinalen Dispersionskoeffizienten (0 bis 1).\n
!!   Wird von QSim-3D nur berücksichtigt, wenn der Schalter für den Dispersionskoeffizienten (Feld 3) auf 0 gesetzt ist.\n
!!   FlongDis</td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile,&nbsp;11.&nbsp;Feld</td>
!! <td>iColi 1-Gewässerhygiene-berechnung
!!   </td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile,&nbsp;12.&nbsp;Feld</td>
!! <td>ikonsS ??\n
!!   </td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile,&nbsp;13.&nbsp;Feld</td>
!! <td>iSchwer ??\n
!!   </td>
!! </tr>
!! <tr>
!! <td>6.&nbsp;Kopfzeile,&nbsp;14.&nbsp;Feld</td>
!! <td>iphy Verfahren für die Oberflächenbelüftung oxygen() 1-Kirchesch,2-Kirchesch,3-Wolf,4-Melching</td>
!! </tr>
!! </table>
!! <h2>Detailbereich - Randbedingungsblock - Kopfzeile</h2>
!! Der Randbedingungsblock-Kopf besteht aus einer Zeile mit folgenden Feldern:
!! \n\n
!! <table>
!! <tr>
!! <th>Position</th><th>Inhalt</th></tr>
!! <tr>
!! <td>Randb.block-Kopfzeile, 1.&nbsp;Feld</td>
!! <td>Nummer der Zone der Randbedingung gemäß <a href="./exp/points" target="_blank">points-Datei</a>, Feldlänge ist 5.</td>
!! </tr>
!! <tr>
!! <td>Randb.block-Kopfzeile, 2.&nbsp;Feld</td>
!! <td>Nummer der Randbedingung gemäß points-Datei, Feldlänge ist 5</td>
!! </tr>
!! <tr>
!! <td>Randb.block-Kopfzeile, 3.&nbsp;Feld</td>
!! <td>Art der Eingabewerte der Randbedingung (Feldlänge 1):\n
!!   0 = Tagesmittelwerte,\n
!!   1 = uhrzeitbezogene Einzelwerte</td>
!! </tr>
!! <tr>
!! <td>Randb.block-Kopfzeile, 4.&nbsp;Feld</td>
!! <td>Anzahl der Werte der Randbedingung im Simulationszeitraum:\n
!!   1 = konstante Randbedingung über den gesamten Zeitraum,\n
!!   2 = Zeitreihe;\n
!!   Der Wert ist gleichzeitig die Anzahl der folgenden Wertzeilen im Randbedingungsblock.</td>
!! </tr>
!! </table>
!! <h2>Detailbereich - Randbedingungsblock - Wertzeilen</h2>
!! Auf die Kopfzeilen folgen so viele Wertzeilen, wie im vierten Feld der Kopfzeile des Randbedingungsblocks angegeben.
!! Jede Wertzeile definiert die Parameterwerte der Randbedingung für einen Zeitpunkt.
!! \n\n
!! <table>
!! <tr>
!! <th>Position</th>
!! <th>Inhalt</th>
!! </tr>
!! <tr>
!! <td>Randb.block-Wertzeile, Felder&nbsp;1 bis 4</td>
!! <td>Zeitpunkt der Wertzeile </td>
!! </tr>
!! <tr>
!! <td>Randb.block-Wertzeile, 5.&nbsp;Feld</td>
!! <td>In QSim der Q-Wert der Randbedingung zum angegebenen Zeitpunkt, in QSim-3D nicht benutzt und auf 0 gesetzt.
!!   Die Feldlänge ist 13.</td>
!! </tr>
!! <tr>
!! <td>Randb.block-Wertzeile, 6.&nbsp;Feld und folgende</td>
!! <td>Parameterwerte aller in <a href="./exp/EreigGParam.xml" target="_blank">EreigGParam.xml</a> definierten Parameter (ParamSetDef-Element).
!!   Jedes Feld hat die im Format-Attribut der Parameter-Elemente der Definitionsdatei angegebene Länge.</td>
!! </tr>
!! </table>
!!\n\n
!! aus Datei eingabe.f95 ; zurück zu \ref Datenmodell

!> \page qsimdateiwetter Wetter.txt
!! Die Wetter-Datei definiert für die Wetterstationen des Modells jeweils eine Zeitreihe verschiedener meteorologischer Parameter.
!! \n\n
!! Beispiel:\n
!! <a href="./exp/Wetter.txt" target="_blank">Wetter.txt</a>\n
!! <a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a>
!! \n\n
!! <h2>Gliederung der Datei</h2>
!! <ul>
!! <li>Kopfbereich</li>
!! <li>Detailbereich</li>
!! </ul>
!! Der Detailbereich besteht mehreren Blöcken, je einem pro Wetterstation des Modells.\n
!! Jeder Wetterstationsblock besteht aus einer Kopfzeile und mindestens einer Wertzeile.
!! \n\n
!! <h2>Kopfbereich</h2>
!! Der Kopfbereich hat vier Zeilen.
!! \n\n
!! <table>
!! <tr>
!! <th>Position</th>
!! <th>Inhalt</th>
!! </tr>
!! <tr>
!! <td>Kopf, 1.&nbsp;Zeile</td>
!! <td>\ref qsimdateikopfzeilev "Versions-Kopfzeile" mit "Wetter" als Dateityp</td>
!! </tr>
!! <tr>
!! <td>Kopf, 2.&nbsp;Zeile</td>
!! <td>Modellname und ggf. weitere Angaben, maximal 255 Zeichen.\n
!!   Die Zeile wird von QSim-3D nicht ausgewertet.</td>
!! </tr>
!! <tr>
!! <td>Kopf, 3.&nbsp;Zeile</td>
!! <td>Ereignisname und ggf. weitere Angaben, maximal 255 Zeichen.\n
!!   Die Zeile wird von QSim-3D nicht ausgewertet.</td>
!! </tr>
!! <tr>
!! <td>Kopf, 4.&nbsp;Zeile</td>
!! <td>Zwei Felder, jeweils mit Feldlänge 1.\n
!!   Das erste Feld gibt die Anzahl der Wetterstationen an, d.&nbsp;h. die Anzahl der Wetterstationsblöcke der Datei.\n
!!   Das zweite Feld ist ein Schalter für die Zeitbasis der Werte der Wertzeilen:\n
!!   0 = Werte sind Tageswerte,\n
!!   1 = Werte sind uhrzeitbezogene Einzelwerte</td>
!! </tr>
!! </table>
!! <h2>Detailbereich - Wetterstationsblock - Kopfzeile</h2>
!! Der Wetterstationsblock-Kopf besteht aus einer Zeile mit zwei Feldern.\n
!! Das erste Feld gibt die Nummer der Wetterstation an, Feldlänge ist 8.\n
!! Die Nummern der Wetterstationen korrespondieren mit den Werten des ersten Parameters (WStation) der T-Parametergruppe
!! der \ref qsimdateimodellg3 , d.&nbsp;h. der Wetterstations-Zuordnung der Zonen des Modells.
!! \n\n
!! Das zweite Feld gibt die Anzahl der Wertzeilen der Wetterstation an, Feldlänge ist 5.
!! \n\n
!! <h2>Detailbereich - Wetterstationsblock - Wertzeilen</h2>
!! Auf die Kopfzeilen folgen so viele Wertzeilen, wie im zweiten Feld der Kopfzeile des Wetterstationsblocks angegeben.
!! Jede Wertzeile definiert die meteorologischen Parameter der Wetterstation für einen Zeitpunkt.
!! \n\n
!! <table>
!! <tr>
!! <th>Position</th>
!! <th>Inhalt</th>
!! </tr>
!! <tr>
!! <td>Stationsblock-Wertzeile, Felder&nbsp;1 bis 4</td>
!! <td>Zeitpunkt der Wertzeile</td>
!! </tr>
!! <tr>
!! <td>Stationsblock-Wertzeile, 5.&nbsp;Feld und folgende</td>
!! <td>Werte aller in <a href="./exp/WetterParam.xml" target="_blank">WetterParam.xml</a> definierten Parameter (ParamSetDef-Element).
!!   Jedes Feld hat die im Format-Attribut der Parameter-Elemente der Definitionsdatei angegebene Länge.\n
!!   \n
!!   Wenn die Wetterdaten als Tageswerte angegeben sind (zweites Feld der vierten Kopfzeile ist 0), ist die Uhrzeit der Wertzeilen irrelevant.
!!   Maximale und minimale Lufttemperatur des Tages (sechstes und siebtes Feld) sind belegt.\n
!!   Wenn die Uhrzeitoption gesetzt ist (1), ist nur das erste der beiden Temperaturfelder belegt und gibt die Lufttemperatur zum entsprechenden Zeitpunkt an.
!!   Das zweite Lufttemperaturfeld wird von QSim-3D ignoriert.
!!   (Es ist entweder auf den Nullwert gesetzt &ndash; -99.99 &ndash; oder auf den selben Wert wie das erste Temperaturfeld.)</td>
!! </tr>
!! </table>
!!\n\n
!! aus Datei eingabe.f95 ; zurück zu \ref Datenmodell

!> <h1> SUBROUTINE eingabe </h1>
!! bewerkstelligt das Einlesen vom \ref Datenmodell. \n
!! aus Datei eingabe.f95 ; zurück zu \ref Modellerstellung
      SUBROUTINE eingabe()   !!!! arbeite nur auf Prozessor 0 !!!!
!
      use modell                                                   
      use QSimDatenfelder
      use aparam	  
                         
      implicit none
      integer :: i, j, n, n_cal
      logical :: vorhanden, only
      integer mtag, mmonat ,mjahr
      real :: muhrzeit_stunde
      integer , allocatable , dimension (:) :: randzaehl
      logical , allocatable , dimension (:) :: randda
      !print*,'eingabe() startet'
      only=.false.

      select case (hydro_trieb)
      case(1) ! casu-transinfo                                           
         if(meinrang.eq.0)then ! prozess 0 only
            call netz_lesen() ! Lage der Knoten, Zonen, Randnummern und Vermaschung einlesen
            ! Konzentrationen anlegen und initialisieren:
            n_cal=knotenanzahl2D
         end if ! only prozessor 0
         call mpi_barrier (mpi_komm_welt, ierr)
         call MPI_Bcast(n_cal,1,MPI_INT,0,mpi_komm_welt,ierr)
      case(2) ! Untrim² netCDF
         if(meinrang.eq.0)then ! prozess 0 only
            call read_mesh_nc()  ! Lage der Knoten und Vermaschung aus der netcdf-hydraulik-Datei einlesen                     
            call read_elemente_gerris()  ! Zonen und Randnummern von ELEMENTE.txt einlesen, die von Gerris erzeugt wurde                     
            n_cal=n_elemente
            print*,'Untrim netCDF read mesh'
         end if ! only prozessor 0
         call mpi_barrier (mpi_komm_welt, ierr)
         call MPI_Bcast(n_cal,1,MPI_INT,0,mpi_komm_welt,ierr)
      case(3) ! SCHISM netCDF
         !!!### call read_mesh_nc_sc()
         n_cal=n_elemente !!??
         n_cal=knotenanzahl2D
         if(meinrang.eq.0)print*,'got SCHISM netCDF mesh ##### but n_cal=knotenanzahl2D ?????????########'
      case default
         call qerror('Hydraulischer Antrieb unbekannt netz_lesen')
      end select

      ! partitioning of variable arrays
         part=n_cal/proz_anz
         n=part*proz_anz
         !print*,'ini_par knotenanzahl=', nk,' proz_anz=', proz_anz, ' part=', part, ' part*proz_anz=', n
         if(n.lt.n_cal)part=part+1
         print*,'part=', part, ' part*proz_anz=',part*proz_anz," meinrang=",meinrang  &
     &         ," modell_parallel() n_cal=", n_cal

      call mpi_barrier (mpi_komm_welt, ierr)
      call ini_planktkon0(n_cal)
      call ini_benthic0(n_cal)
      call ini_ueber(n_cal)
      call allo_trans() !! Felder für Transportinformationen und Strömungsfeld allocieren
      if(meinrang.eq.0)then ! only prozessor 0
         call ausgabekonzentrationen_beispiel()
         if (kontrollknoten .eq. 0)then
            print*,"### special option #### only writing output variable list ausgabekonzentrationen_beispiel.txt"
            call qerror('modeverz: control node = 0  ### special option #### (error is regular exit)')
         end if
      end if ! only prozessor 0
      call mpi_barrier (mpi_komm_welt, ierr)
      call show_mesh()
      call ini_zeit() ! initialise time preliminary to reference-year
      call mpi_barrier (mpi_komm_welt, ierr)

      select case (hydro_trieb)
      case(1) ! casu-transinfo  
         if(meinrang.eq.0)then ! prozess 0 only
            call transinfo_sichten()      ! Transportinformationen sichten:
         end if ! only prozessor 0
         call mpi_barrier (mpi_komm_welt, ierr)
      case(2) ! Untrim² netCDF
         call nc_sichten()
      case(3) ! SCHISM netCDF
         !!call screen_schism_nc()
      case default
         call qerror('Hydraulischer Antrieb unbekannt; sichten')
      end select

   if(meinrang.eq.0)then ! only prozessor 0
      call modellg() ! read zone-information aus from MODELLG.3D.txt
      call modella() ! read lat. lon. at first ( zunächst nur Geographische Breiten- und Längenkoordinaten )
      call ereigg_modell() ! read time-stepping information at first
      call ereigg_Randbedingungen_lesen() ! next read BC-development
!     read global model-parameters now in module ::uebergabe_werte
      call aparam_lesen()
      call extnct_lesen()
      call ausgabezeitpunkte() !! reading points in time for output
      call ausgabekonzentrationen() !! reading output-values
!
      call transinfo_schritte(startzeitpunkt, startzeitpunkt+deltat) !! sollte eigentlich für beide Antriebe gleichermaßen funktionieren

      call wetter_readallo0()
      print*,"wetter_readallo0() gemacht"

      call ganglinien_lesen()
      !! darin rand_zusammenhang() und querschnitt_zusammenhang()

      !! nachschauen, ob und zu welchen Zeitpunkten 
      !! Verteilungen der Trübung/Schwebstoff und des Salzgehalts offline bereitliegen.
      call schwebstoff_salz_sichten()

      !! Daten für die Aufenthaltszeitberrechnung von Datei alter.txt lesen
      if(nur_alter) call alter_lesen() 
   end if ! only prozessor 0
   call mpi_barrier (mpi_komm_welt, ierr)

      RETURN
  222 FORMAT (A,'rechenzeit=',I15,' Temperatur_Wasser=',F8.3,' Temperatur_Sediment=',F8.3)
      END subroutine eingabe
!----+-----+----

!> die Subroutine ereigg_modell()\n
!! Die <a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a> Dateien für QSim sind weiterverwendbar,\n
!! hier wird zunächst nur die Zeitsteuerung (Anfang, Ende, Zeitschrittweite) daraus gelesen. \n
!! Die SUBROUTINE ereigg_Randbedingungen_lesen() entnimmt dann die Rand-Werte aus der Datei \n
!! \n\n
!! aus Datei eingabe.f95 ; zurück zu \ref Modellerstellung
      SUBROUTINE ereigg_modell()
      use modell
      use QSimDatenfelder
      implicit none
      character (len=500) :: dateiname
      integer :: open_error, ion, read_error
      real :: dt_min, tictac
!      real :: lesezeit

      write(dateiname,'(2A)')trim(modellverzeichnis),'/EREIGG.txt'
      ion=92
      open ( unit =ion , file = dateiname, status ='old', action ='read ', iostat = open_error )
      if(open_error.ne.0) then
         write(fehler,*)'open_error EREIGG.txt ... Datei vorhanden?'
         call qerror(fehler)
      end if ! open_error.ne.0

      rewind (ion) 
!                                                                       
      if(.not.zeile(ion)) call qerror('ereigg_modell 1 read_error.ne.0')
      print*,'EREIGG Version: ', trim(ctext)
      if(.not.zeile(ion)) call qerror('ereigg_modell 2 read_error.ne.0')
      print*,'EREIGG Modell: ', trim(ctext)
      if(.not.zeile(ion)) call qerror('ereigg_modell 3 read_error.ne.0')
      print*,'EREIGG Ereignis: ', trim(ctext)
!
      if(.not.zeile(ion)) call qerror('Zeile 3 von EREIGG.txt nicht da')
      read(ctext, *, iostat = read_error) tag, monat, jahr, uhrzeit_stunde ! itags,monats,jahrs,uhrs 
      !if(read_error.ne.0) call qerror('read_error in Zeile 3 von EREIGG.txt; Anfangszeitpunkt der Berechnung')
      !tictac=int(uhrzeit_stunde)+((uhrzeit_stunde-int(uhrzeit_stunde))/0.6) ! Umrechnung stunde.minute in dezimal-stunden
      !print*,"gelesen start:", tag, monat, jahr, uhrzeit_stunde,int(uhrzeit_stunde),uhrzeit_stunde-int(uhrzeit_stunde),tictac
      !uhrzeit_stunde = tictac
      call sekundenzeit(2)
      startzeitpunkt=zeitpunkt
      itags=tag
      monats=monat
      jahrs=jahr
      uhrs=uhrzeit_stunde
      print*,'EREIGG.txt, Berechnungsbeginn: tag,monat,jahr, Uhrzeit, Startzeitpunkt' &
     &      , itags, monats, jahrs, uhrs, startzeitpunkt
!
      if(.not.zeile(ion)) call qerror('Zeile 4 von EREIGG.txt nicht da')
      read(ctext, *, iostat = read_error) tag, monat, jahr, uhrzeit_stunde, dt_min ! itage,monate,jahre,uhren,izdt 
      if(read_error.ne.0) call qerror('read_error in Zeile 4 von EREIGG.txt; Endzeitpunkt der Berechnung')
      !print*,"gelesen ende:", tag, monat, jahr, uhrzeit_stunde,int(uhrzeit_stunde),uhrzeit_stunde-int(uhrzeit_stunde)
      !uhrzeit_stunde = int(uhrzeit_stunde)+((uhrzeit_stunde-int(uhrzeit_stunde))/0.6) ! Umrechnung stunde.minute in dezimal-stunden
      call sekundenzeit(2)
      endzeitpunkt=zeitpunkt
      itage=tag
      monate=monat
      jahre=jahr
      uhren=uhrzeit_stunde
      print*,'EREIGG.txt,   Berechnungsende: tag,monat,jahr, Uhrzeit, Endzeitpunkt' &
     &      , itage, monate, jahre, uhren, endzeitpunkt
      deltat=int(dt_min*60)
      if(deltat.le.0) then
         write(fehler,*)'ereigg_modell: zeitschrittweite=',deltat,' , und das ist falsch!'
         call qerror(fehler)
      end if ! zeitschrittweite deltat ist falsch
      if(abs(real(deltat)-(dt_min*60)) .gt. 0.01) then
         write(fehler,*)'ereigg_modell: angegebene zeitschrittweite=',dt_min,' minuten d.h.',(dt_min*60)  &
     &                 ,' sekunden ist falsch weil sekundenzeitschritt nicht ganzzahlig'
         call qerror(fehler)
      end if ! Zeitschritt als ganze sekunden
      zeitschrittanzahl=(endzeitpunkt-startzeitpunkt)/(deltat)
      print*,'zeitschrittanzahl, startzeitpunkt, endzeitpunkt, deltat=',zeitschrittanzahl, startzeitpunkt, endzeitpunkt, deltat
      if(zeitschrittanzahl.le.0) then
         print*,'WARNUNG zeitschrittanzahl=',zeitschrittanzahl,' , wollen Sie das wirklich ????'
         !! zeitschrittanzahl=null durchlaufen lassen, um Initialisierung ausgeben zu können 
         if(zeitschrittanzahl.lt.0) then !! nur abbrechen wenn unter null.
            call qerror('zeitschrittanzahl.lt.0')
         end if ! zeitschrittanzahl.lt.0
      end if ! zeitschrittanzahl.le.0
      !if(hydro_trieb.eq. 3)then !## preliminary SCHISM all hydro steps
      !   deltat=dttrans
      !   startzeitpunkt=transinfo_zeit(transinfo_zuord(1))
      !   endzeitpunkt  =transinfo_zeit(transinfo_zuord(transinfo_anzahl))
      !   zeitschrittanzahl=(endzeitpunkt-startzeitpunkt)/deltat
      !   print*,'##preliminary## all ',zeitschrittanzahl,' SCHISM steps ',startzeitpunkt,' until ',endzeitpunkt,' deltat=',deltat
      !end if !SCHISM

      print*,"hydro_trieb=",hydro_trieb      !case(2) ! Untrim² netCDF
      if((hydro_trieb.eq. 2).and.(deltat.ne. 1200))then
         !call qerror('Untrim-Läufe bisher nur mit Zeitschrittweite 20 min. möglich')
         print*,"Untrim Lauf hydro_trieb=",hydro_trieb," deltat=",deltat, "sollte jetzt (okt20) eigentlich gehen"
      endif

      print*,'transinfo_zeit,Anfang+Ende=',transinfo_zeit(transinfo_zuord(1)), transinfo_zeit(transinfo_zuord(transinfo_anzahl))
      if(startzeitpunkt.lt.transinfo_zeit(transinfo_zuord(1)))  then
         print*,"startzeitpunkt, transinfo_zeit(transinfo_zuord(1)), transinfo_zuord(1)="
         print*,startzeitpunkt, transinfo_zeit(transinfo_zuord(1)), transinfo_zuord(1)
         call qerror('### Abbruch ### zum startzeitpunkt liegen noch keine Transportinformationen vor')
      end if !wrong start time
      if(endzeitpunkt.gt.transinfo_zeit(transinfo_zuord(transinfo_anzahl)))  &
         call qerror('### Abbruch ### zum endzeitpunkt liegen keine Transportinformationen mehr vor')
      print*,'EREIGG.txt, Berechnungs-Zeitraum von ',startzeitpunkt,' bis ', endzeitpunkt  &
            ,' mit zeitschrittweite ',deltat  &
            ,' liegt innerhalb des Zeitraums ',transinfo_zeit(transinfo_zuord(1)),' bis '  &
            ,transinfo_zeit(transinfo_zuord(transinfo_anzahl)),', in dem Transportinformationen vorliegen.'

      rechenzeit=startzeitpunkt

      if(.not.zeile(ion)) call qerror('Zeile 5 von EREIGG.txt nicht da')
      read(ctext, *, iostat = read_error) imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren  &
     &                                   ,ilongDis,FlongDis,iColi,ikonsS,iSchwer,iphy,iformVert,iform_verdr
      print*,'Zeile 5 von EREIGG.txt:'
	  print*,'imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren,ilongDis,FlongDis,iColi,ikonsS,iSchwer,iphy,iformVert,iform_verdr'
      print*, imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren,ilongDis,FlongDis,iColi,ikonsS,iSchwer,iphy,iformVert,iform_verdr
      if(read_error.ne.0) then
         print*,'EREIGG.txt Zeilentext: ',trim(ctext)
         write(fehler,*)'read_error in Zeile 5 von EREIGG.txt; Berechnungs-Flags'
         call qerror(fehler)
      end if ! open_error.ne.0
      if(imitt.eq. 1) &
     &   print*,'### Warnung ###, die in EREIGG.txt mittels imitt=1 angeforderte Ausgabe von Tagesmittelwerten ', &
     &          'ist in QSim3D nicht implementiert.'
      if(ipH.eq. 0) &
     &   print*,'### Warnung ###, in EREIGG.txt wird mittels ipH=0 eine Sim. ohne ph-Wert Berechnung angefordert !!!'
      if(idl.eq. 0) &
     &   print*,'### Warnung ###, die in EREIGG.txt mittels idl=0 angeforderte Einlesen von Disp.Koeff.', &
     &          'ist in QSim3D nicht implementiert.'
      if(idl.eq. 1) &
     &   print*,'### Warnung ###, die in EREIGG.txt mittels idl=1 angeforderte Berechnen von Disp.Koeff.', &
     &          'ist in QSim3D nicht implementiert.'
      if(itemp .eq. 1) then
         print*,'### Warning ### Temperature-simulation only. Asked for by itemp=1 in EREIGG.txt'
         nur_temp=.true.
      else
         nur_temp=.false.
      endif
      if(itracer.eq. 1) &
     &   print*,'### Warnung ###, die in EREIGG.txt mittels itracer=1 angeforderte Tracer-Sim.', &
     &          'ist in QSim3D nicht implementiert.'
      if((iphy<1).or.(iphy>4))then
         write(fehler,*)'ereigg_modell: aeration flag iphy out of bounds'
         call qerror(fehler)
      endif
!qsim_201314:
!             read(92,'(a50)')modell 
!             read(92,'(a50)')cEreig 
!             read(92,9200)itags,monats,jahrs,uhrs 
!             read(92,9210)itage,monate,jahre,uhren,izdt 
!             read(92,9220)imitt,ipH,idl,itemp,itracer,ieros                    &
!             ,ischwa,iverfahren,ilongDis,FlongDis
! 9220 format(I1,2x,I1,2x,I1,2x,I1,2x,I1,2x,i1,2x,I1,2x,I1,2x,I1,2x,f4.2) 

      close (ion)
      rewind (ion)

      END subroutine ereigg_modell
!----+-----+----

!> \page qsimdateikopfzeilev Versions-Kopfzeile der QSim-Dateien
!! Die Versions-Kopfzeile gibt die Version des beteiligten QSim-Programms an.
!! Sie ist in der Regel die erste Zeile der QSim-Dateien wie \ref qsimdateimodella "ModellA.txt".
!! \n\n
!! Die Zeile hat maximal 255 Zeichen.
!! \n\n
!! Die Anführungszeichen in der folgenden Beschreibung kennzeichnen konstante Texte.
!! \n\n
!! <table>
!! <tr>
!! <th>Feld</th>
!! <th>Inhalt</th><th>Erläuterung</th></tr>
!! <tr>
!! <td>1</td>
!! <td>"*V"</td>
!! <td>Zeilenkennung der Versions-Kopfzeile</td>
!! </tr>
!! <tr>
!! <td>2</td>
!! <td>"QSim"</td>
!! <td>Kennzeichnung der Datei als QSim-Schnittstelle</td>
!! </tr>
!! <tr>
!! <td>3</td>
!! <td>Typ</td>
!! <td>Bezeichnung des Dateityps, z.&nbsp;B. "ModellA"</td>
!! </tr>
!! <tr>
!! <td>4</td>
!! <td>QSim-Version</td>
!! <td>Im Fall einer Eingangsdatei die Version des QSim-Programms, das die verwendete Definitionsdatei (EreigGParam.xml o.&nbsp;ä.) erzeugt hat.\n
!!   Im Fall einer Ausgangsdatei die Version des QSim-Programms, das die Datei berechnet hat.</td>
!! </tr>
!! <tr>
!! <td>5</td>
!! <td>Weitere Angaben</td>
!! <td>Optional weitere Angaben, z.&nbsp;B. die Version des Gerris-Programms, das die Datei erzeugt hat. Diese Angaben werden von QSim nicht berücksichtigt.</td>
!! </tr>
!! </table>
!!\n\n
!! aus Datei eingabe.f95 ; zurück zu \ref Datenmodell


