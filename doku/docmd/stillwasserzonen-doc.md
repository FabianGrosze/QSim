Stillwasserzonenerweiterung/Buhnen {#lnk_stillwasserzonen}
==================================

Als Stillwasserzonen wird eine an den Hauptstrom seitlich angeschlossene Kette 
von Buhnenfeldern bezeichnet, deren Wasserkörper bei geringen und mittleren 
Abflüssen wesentlich geringere Fließgeschwindigkeiten aufweisen als der 
Hauptstrom. Somit bewirken die Buhnen z.B. in der Mittelelbe eine 
Quergliederung des Flusses in einen Hauptstrom und Stillwasserzonen, die in 
QSim1D in der Stillwasserzonenerweiterung abgebildet werden. 

![Luftbildaufnahme der Mittelelbe mit schematischer Angabe der Berechnungsgitter im Modell QSim, Einteilung in Hauptstrom und Buhnenfeld sowie Kennzeichnung des lateralen Stoffaustausches und longitudinalen Stofftransportes (Foto: A. Prange, GKSS).](img/stillwasser_luftbild.png)
\n\n

Es werden zwei miteinander korrespondierende Querprofildatensätze für 
hydraulische Berechnungen verwendet: zum einen Querprofile ohne Buhnen und damit 
ohne Stillwasserzonen bzw. Buhnenfelder, zum anderen Querprofile mit 
eingearbeiteten Buhnenschatten (Abb. 2) (EIDNER et al. 2001). Aus der Differenz 
der beiden in den parallelen Modellläufen ermittelten Wasserspiegelbreiten und 
durchflossenen Flächen werden die effektiven Querschnittsflächen der 
Buhnenfelder, deren Breiten und deren mittlere Wassertiefen errechnet. 

Auf diese Weise können in QSim nun für jede Abflusssituation bis zum mittleren 
Abfluss (MQ) der Wasserkörper des Hauptstroms und des Buhnenfeldes bezüglich 
mittlerer Wassertiefe und durchflossener Fläche beschrieben werden. Für Abflüsse 
größer als MQ treten keine Differenzen der Wasserspiegelbreiten zwischen den 
Modellläufen mit und ohne Buhnenschatten mehr auf, somit ist auch keine 
Berechnung der Wassertiefen der Buhnenfelder möglich. Daher wird auf die bei 
MQ ermittelten Wassertiefen zurückgegriffen und diese Werte in den 
Modellierungen der Abflüsse größer MQ verwendet.

Wesentlich für das Gelingen der Modellierungen der Gewässergüte ist eine genaue 
Bestimmung der lateralen Austauschrate zwischen Buhnenfeld und Hauptstrom bzw. 
der Aufenthaltszeiten des Wassers in den Buhnenfeldern. Bei alleiniger 
Verwendung hydraulischer Berechnungen wird eine zu kurze Verweilzeit in 
Buhnenfeldern abgeschätzt, u.a. weil das gesamte Volumen im Querschnitt als 
austauschbar angesehen wird. Die hydraulischen Berechnungen werden zunächst mit 
dem Querprofildatensatz „mit Buhnenschatten“ durchgeführt, da als vereinfachende 
Annahme davon ausgegangen wird, dass zumindest bis zu Mittelwasserbedingungen 
nur der Hauptstrom, also die durchflossene Fläche zwischen den Buhnen, 
abflusswirksam für den Fluss ist. Die in den Modellläufen auf der Basis der 
Querprofile „mit Buhnenschatten“ ermittelten Wasserspiegellagen werden als 
Eingangsbedingung für die anschließenden Modellläufe mit dem Querprofildatensatz 
„ohne Buhnenschatten“ gesetzt. In diesen Modellläufen wird daraus die 
durchflossene Fläche des gesamten Profils errechnet.

![Oben: Querprofile ohne Buhnenschatten – abflusswirksam ist die gesamte Querschnittsfläche einschließlich der Buhnenfelder. Unten: Querprofile mit Buhnenschatten – abflusswirksam ist die durch Buhnenschatten reduzierte Querschnittsfläche.](img/stillwasser_querprofile.png)
\n\n

Die In QSim1D enthaltenen Buhnenfelder sind nicht hydraulisch an den Hauptstrom 
angeschlossen. Daher kann im Modell keine mittlere Fließgeschwindigkeit in den 
Buhnenfeldern errechnet werden. Sie muss abgeschätzt werden, da dieser Parameter 
bei wichtigen Prozessen, etwa dem Stoffaustausch zur Atmosphäre und zur 
Gewässersohle, benötigt wird. Als Wert für die Modellanwendung „Mittelelbe“ wird 
eine Fließgeschwindigkeit von 0,1 m/s gesetzt. Untersuchungen in Buhnenfeldern 
ergaben mittlere Werte in diesem Bereich.


Weitere Details zum Baustein sind in den folgenden Abschnitten
beschrieben:
- \subpage lnk_stillwasser_prozesse : Erläuterung der Prozesse
- \subpage lnk_stillwasser_vars : Auflistung der verwendeten Formelzeichen und Variablen
- \subpage lnk_stillwasser_umsetzung : Details zum Code und der numerischen Umsetzung


\n\n

Textquelle: stillwasserzonen-doc.md ; Codesource: baustein.f90

