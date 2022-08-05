Modellerstellung {#lnk_modellerstellung}
================

# Stuktur des Datenmodells {#lnk_struktur_datenmodell} 

Um eine Gewässergüte-Simulation (in QSim als "Ereignis" bezeichnet)
in einem Gewässerabschnitt (Modell) durchführen zu können, ist ein Daten-Modell 
erforderlich.

Dabei müssen Vorgaben zu den folgenden Punkten gemacht werden:

- \subpage lnk_diskretisierung 
- \subpage lnk_ereignissteuerung
- \ref lnk_transport_numerik
- \subpage lnk_anfangsbedingungen 
- \subpage lnk_randbedingungen
- \subpage lnk_globale_parameter
- \subpage lnk_lokale_parameter



Informationstechnisch wird ein Daten-Modell für QSim-3D durch ein Verzeichnis definiert,
das im weiteren als <b>Modellverzeichnis</b> bezeichnet wird.

In diesem befindet sich ein Satz von Dateien (deren Vollständigkeit beim Programmstart von QSim-3D abgeprüft wird).

Im Abschnitt \subpage lnk_datenmodell finden sie eine detaillierte Beschreibung der einzelnen Dateien.

# Modellerstellung mit der Benutzeroberfläche Gerris {#lnk_gerris} 

Die Bedienung von QSim-1D mittels Gerris kann im
<a href="./pdf/BfG_1778.pdf" target="_blank">BfG-Bericht 1778</a> nachgelesen werden.
<a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=gs+1697" target="_blank">
(Bibliotheks-Exemplar)</a>


Eine Anleitung zur Benutzung von Gerris für mehrdimensionale Modelle ist z.Z. 
(Feb.2018) noch in Arbeit.

Für die Kopplung von Gerris mit QSim3D existiert eine <a href="./pdf/QSim3D_Gerris_Schnittstelle_1p0.pdf" target="_blank">Schnittstellenbeschreibung</a>
(auch im <a href="./html/Qsim3d_Gerris_Schnittstelle/index.html" target="_blank">html-Format</a>);\n
desweiteren wurde ein <a href="./pdf/Gerris123D.Test.3D.1.0.pdf" target="_blank">Testbericht für die Gerris 3D NetCDF Erweiterung (casu)</a>,
sowie ein <a href="./pdf/Gerris123D_Test_3D_NetCDF_0p1.pdf" target="_blank">Testbericht für die Gerris 3D NetCDF Erweiterung (Untrim)</a> angefertigt.


Die grafische Benutzeroberfläche GERRIS wurde
auf der Basis des <a href="./pdf/Gerris123D.Feinkonzept.3D.1.0.pdf" target="_blank">DV-technischen Feinkonzepts</a>
vom <a href="http://www.wasserundumwelt.de" target="_blank">Ingenieurbüro Schumacher</a>  produziert.


Die Netzerstellung und Zonierung erfolgt mit dem externen Netzgenerator Janet.


Für Modellbedienung mittels Gerris ist es erforderlich, vorher eine ssh-Verbindung
zwischen dem eigenen Arbeits-PC, auf dem Gerris installiert ist und dem 
Server (HPC), auf dem gerechnet werden soll, herzustelllen.
Details dazu enthalten die Dokumente:
<a href="./pdf/Gerris3D_nachvollzogenJens.pdf" target="_blank">Gerris3D_nachvollzogen</a>
und <a href="./pdf/Schluesselkonvertierung.pdf" target="_blank">Schluesselkonvertierung</a>


Die Kommunikation zwischen Gerris und dem Server erfolgt, indem Scripte auf 
dem Server gestartet werden.
Diese kommunikation wird in den 3D Optionen eingerichtet:


<em> hier fehlt noch das Bild (siehe Code, auskommentiert) </em>
\image html 3Doptionen.png
\n\n
!Server-URL\n
   URL oder ip-Adresse des Servers, auf dem QSim3D gerechnet weden soll. Zwischen dem Server und dem lokalen Arbeitsrechner muss eine ssh-Verbindung möglich sein.
\n\n
User-Name \n
   Name des Benutzers, mit dem auf dem Server gerechnet werden soll. Die ssh-Anmeldung erfolgt über diesen Benutzer.
\n\n
Ereignisliste-Skript \n
   Pfad zu einem Skript auf dem Server, welches eine Liste aller auf dem Server vorhandenen hydraulischen Modelle/Ereignisse auf die Standartausgabe ausgibt.
\n\n
   Jede Zeile der Ausgabe steht dabei für ein Modell/Ereignis und besteht aus vier bis sechs Feldern, die durch ein Semikolon(;) getrennt sind:
\n\n
<table Ereignisliste>
<tr><td> Verzeichnispfad  </td><td> Vollständiger Pfad des Modellverzeichnisses auf dem Server   </td></tr>
<tr><td> Startzeitpunkt  </td><td> Startzeitpunkt des Ereignisses im Format Jahr-Monat-Tag Stunde:Minute:Sekunde   </td></tr>
<tr><td> Endzeitpunkt  </td><td> Endzeitpunkt des Ereignisses im Format Jahr-Monat-Tag Stunde:Minute:Sekunde   </td></tr>
<tr><td> Zeitschrittweite  </td><td> Zeitschrittweite des Ereignisses in Sekunden als ganze Zahl   </td></tr>
<tr><td> Modellname  </td><td> optional: Modellname   </td></tr>
<tr><td> Projektname  </td><td> optional: Projektname (nur wenn auch Modellname angegegeben)   </td></tr>
</table>
\n\n
   Beispiel Ausgabezeile: \n
<code>\verbatim
   /home/username/fluss/;2011-01-01 00:00:00;2011-08-15 00:00:00;600;Flussmodell;Flussprojekt
\endverbatim</code>
\n\n
QSim-3D-Skript \n
   Pfad zu einem Skript auf dem Server, welches den QSim3D-Rechenlauf startet.
   Wird in Gerris ein Rechenlauf gestartet, wird dieses Skript zusammen mit dem Projektverzeichnis als Parameter aufgerufen. Im Projektverzeichnis wird dadurch das Skript "qsim3d.sbatch" ausgeführt.
   \n
   Dabei müssen in "qsim3d.sbatch" alle für den Rechenlauf relevanten SLURM-Parameter definiert und das QSim-Executable zusammen mit dem Projektverzeichnis aufgerufen werden.
\n\n
Definitionen-Skript \n
   Pfad zu einem Skript auf dem Server, welches Definitionsdateien, die an der Schnittstelle von Qsim3d und Gerris benötigt werden, bereitstellt. Aus einem zentralen Verzeichnis werden dazu die folgenden Dateien in das Projektverzeichnis kopiert.
      AParamParam.xml                   enthält Definitionen der globalen QSim-Modellparameter
      ausgabekonzentrationen_beispiel.txt    enthält eine Liste der Namen der Modell-Variablen, die von der aktuellen QSim-3D Version ausgegeben werden können
      EreigGParam.xml                     enthält Definitionen der Ereignisparameter
      ModellG3Param.xml                  enthält Definitionen der Zonen-Parameter
      WetterParam.xml                     enthält Definitionen der Wetter-Parameter
\n\n
IsoQ-Skript \n
   Pfad zu einem Skript auf dem Server, welches die Visualisierung QSim-3D-Ergebnissen ermöglicht.
\n\n
Ncdump \n
   Name des Programmaufrufs, um auf dem Server Daten aus NetCDF-Dateien zu lesen.
\n\n
E-Mail \n
   E-Mail-Adresse, an die eine Benachrichtigung bei Berechnungsende geschickt wird.
\n\n
aus Datei: modellerstellung-doc.md; Code: eingabe.f95 ; zurück zu \ref lnk_modellstruktur
