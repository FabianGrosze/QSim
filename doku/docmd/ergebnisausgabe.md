Ergebnisse ausgeben, darstellen und auswerten  {#lnk_ergebnisausgabe}
=============================================

Zur Zeit arbeitet QSim-3D noch 2D-tiefengemittelt  (wyrwa mai 2018).

# Ausgabe-Konzept {#lnk_ausgabekonzept}

Da bei der Simulation mit mehrdimensionalen Modellen sehr große Datenmengen als 
Ergebnisse anfallen, wird in QSim-3D das Konzept verfolgt,
bereits vor dem Programmstart festzulegen, was ausgegeben werden soll:

# Ausgabe-Variablen {#lnk_ausgabekonzentrationen}

Die Datei <a href="./exp/ausgabekonzentrationen.txt" target="_blank">ausgabekonzentrationen.txt</a>
dient dazu, anzugeben, welche Variablen ausgegeben werden sollen.
Für jede Variable gibt es in dieser Datei eine Spalte. Steht in der ersten Zeile 
ein "x" findet eine Ausgabe statt.

Von QSim-3D wird die Datei
<a href="./exp/ausgabekonzentrationen_beispiel.txt" target="_blank">ausgabekonzentrationen_beispiel.txt</a>
ausgegeben, damit der Nutzer weiß welche Variablen als mögliche Ausgabe 
verfügbar sind.
Nach "Ankreuzen" in der ersten Spalte kann diese Datei als Eingabe verwendet 
werden.

Diese Variablen-Auswahl wirkt sich sowohl auf die Ausgabe von 
\ref lnk_ganglinienausgabe als auch auf die Ausgabe von \ref lnk_mehrdimausgabe aus.

# Ganglinien an einzelnen Knoten {#lnk_ganglinienausgabe}

Mit der Datei <a href="./exp/ganglinien_knoten.txt" target="_blank">ganglinien_knoten.txt</a>
wird angegeben, an welchen Knoten eine Ganglinie ausgegeben werden soll.

<i> Dabei bitte beachten, dass Knotennummern in casu, Paravies und Janet ab 0 
zählen, in QSim-3D aber ab 1 </i>

So entsteht am Ende des Berechnungslaufs im Modellverzeichnis ein 
Unterverzeichnis "ganglinien".

Dort ist für jeden gewählten Knoten eine mit "g" beginnende Datei abgelegt
<a href="./exp/g38503.txt" target="_blank">Beispiel g38503.txt</a>,

welche mit <a href="http://voss-mod02/wiki/doku.php?id=gnuplot" target="_blank"> gnuplot</a>
visualisiert werden kann.

# Mehrdimensionale Felder zu einzelnen Zeitpunkten {#lnk_mehrdimausgabe}

Mittels der Datei <a href="./exp/ausgabezeitpunkte.txt" target="_blank">ausgabezeitpunkte.txt</a>
wird angegeben, zu welchen Zeitpunkten mehrdimensionale Felder ausgegeben werden 
sollen.

Im Verlauf der Berechnung werden diese dann im Modellverzeichnis (siehe 
\ref lnk_datenmodell) im .vtk-Format abgelegt. Der Dateiname (z. B. 
ausgabe_734850.vtk) enthält den Zeitpunkt als ganzahligen Integer-Wert in 
Sekunden. Der Ursprung dieser Sekundenzählung ist in der Datei meta im 
traninfo-Verzeichnis (\ref lnk_transport_numerik) festgelegt. Die 
Bildschirm-Ausgabe von QSim-3D gibt für jeden Zeitpunkt das Datum zusammen mit 
dem Sekundenzähler aus.

Das .vtk-Format dient der Visualisierung mit
<a href="http://www.visitusers.org/index.php?title=Main_Page" target="_blank">VisIt</a> oder
<a href="http://voss-mod02/wiki/doku.php?id=paraview" target="_blank">paraview</a>.

netCDF-Formate sind für unstrukturierte Netze noch nicht praktikabel.

# Schnitte

## (Linienausgabe) {#lnk_laengsschnitt}

Das Programm "laengsschnitt" ermöglicht es, auf der Basis der 
\ref lnk_ganglinienausgabe von QSim-3D Ergebnissen, einen Schnitt entlang einer 
horizonzalen Linie zusammenzusetzen; zumeist handelt es sich dabei um 
Längsschnitte entlang der Flußachse. Voraussetzung ist es, dass alle Punkte der 
Linie zur Ganglinienausgabe angewählt wurden.
Diese Punkte werden nicht daraufhin überprüft, ob sie im Netz zusammenhängend 
(jeweils nur von einer Elementkante verbunden) sind.

Dazu ließt das externe Programm "laengsschnitt" die Datei 
<a href="./exp/kilonummer" target="_blank">kilonummer</a>,
in der je einer Abstandskoordinate (Flußkilometrierung) eine Punktnummer 
zugeordnet ist.

Zu allen Punktnummern in kilonummer müssen im Unterverzeichnis "ganglinien" 
Dateien mit Ganglinien-Ergebnissen vorliegen.
Aus den dort vorgefundenen Zeitpunkten kann der Benutzer dann einen wählen, der 
in die Datei
<a href="./exp/schnittig" target="_blank">schnittig</a> ausgegeben wird.

<a href="./taz/laengsschnitt_source.taz">laengsschnitt Programm-Quelle</a>


## (Flux-Ermittlung) {#lnk_querschnitte}

Zur Durchflussermittlung können Querschnitte spezifiziert werden:

Wenn die Datei <a href="./exp/quer.txt" target="_blank">schnitt.txt </a> 
im Modellverzeichnis vorhanden ist, werden Querschnitte ausgewertet.

Dazu werden die auf schnitt.txt angegebenen Punktfolgen auf Zusammenhang
(Aufeinande folgende Knoten sind durch jeweils eine Elementkante verbunden)
von der Subroutine querschnitt_lesen() geprüft.
Dabei wird auch geprüft, ob der Querschnitt am Rand anfängt und endet.

Die Fluss-Ermittlung bewerkstelligt die Subroutine querschnitt_flux() durch 
Aufruf von flux().
Es wird der Volumenstrom des Wassers und der Massenfluss aller 
\ref lnk_ausgabekonzentrationen ermittelt.
Die Ausgabe der Flux-Ermittlung wird im Unterverzeichnis "ganglinen" abgelegt. 
Die Dateien beginnen mit dem Buchstaben "q"
und sind in der Reihenfolge nummeriert, in der Die Querschnitte in "schnitt.txt" 
enthalten sind.
Diese Ausgabe wird von der Subroutine ganglinien_schliessen() mit erledigt.

## Randflüsse
Um Volumen- und Massenströme über einzelne Ränder ermitteln zu können, müssen 
zunächst die Knoten eines Randes in eine zusammenhängende Folge (aufeinande 
folgende Knoten sind durch jeweils eine Elementkante verbunden) gebracht werden.
Dies übernimmt die Subroutine rand_zusammenhang().
Die Fluss-Ermittlung bewerkstelligt die Subroutine rand_flux().
Die Ausgabe der Rand-Flux-Ermittlung wird im Unterverzeichnis "ganglinen" 
abgelegt.
Die Dateien beginnen mit dem Buchstaben "r", gefolgt von der Randnummer.
Diese Ausgabe wird von der Subroutine ganglinien_schliessen() mit erledigt.

# Sonstiges
- Es wurde eine Abschätzung für die \subpage lnk_numerische_diff implementiert.

Textquelle: ergebnisausgabe.md ; Codesources: ausgabe.f95 ;  
zurück: \ref lnk_modellstruktur ; siehe auch: \ref lnk_datenmodell
 