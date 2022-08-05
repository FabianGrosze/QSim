Parallelisierung    {#lnk_parallelisierung}
================

# Parallelisierung der Stoffumsetzungsvorgänge {#lnk_parallel_umsatz} 

Die Parallelisierung von QSim3D nutzt den Umstand, dass die mathematischen Formulierungen, 
welche die bio-chemischen Stoffumsetzungsvorgänge modellieren, keine Raumgradienten enthalten. 
Z.B. ist das Algenwachstum nur vom lokalen Licht-, Nährstoffangebot, Temperatur etc. abhängig.
Die Verhältnisse im Nachbarwassertropfen "interessieren" die Alge im hiesigen Wassertropfen nicht.\n\n
Somit ist es möglich, die Berechnungsstützstellen (Knoten) einfach fortlaufend anhand ihrer Nummer auf die parallelen Prozesse
zu verteilen. Umständliche Gebietszerlegungen, wie sie bei der Parallelisierung von Approximationsverfahren zur Lösung
partieller Differentialgleichungen (die Raumgradienten enthalten) erforderlich sind, werden nicht benötigt.


# Parallelisierung der Transportvorgänge {#lnk_parallel_transport} 

QSim3D nutzt die Stofftransport-Lösung des vorgeschalteten hydraulischen Treibers indem es die Transportinformation
anhand einer Matrix einließt. Diese wird dann auf alle Vektoren multipliziert, 
welche die Diskretisierungen des Feldes jeweils einer Gütevariablen enthalten. 
Diese Matrix-Vektor-Multiplikation könnte evt. mit PETSc parallelisiert werden.\n\n
Das o.g. Verfahren ist auf explizite Zeitdiskretisierungen beschränkt.
Implizite Diskretisierungen würden das Lösen von linearen Gleichungssystemen erfordern, was bei der Vielzahl
an transportierten Variablen-Feldern in einem Gütemodell sehr aufwändig wäre.


# Ein- und Ausgabe {#lnk_parallel_io} 
 
Die Datenein- und -ausgabe erfolgt in QSim3D zentral. Nur der Prozess 0 
beschäftigt sich mit Lesen und Schreiben.
Alle anderen parallelen Prozesse bekommen Daten nur via MPI (Message-Passing-Interface).

\n
Die Subroutinen eingabe() und initialisieren() werden daher auch nur von Prozess 0 aufgerufen.
Die Verteilung der Variablenfelder auf die multiplen Prozesse erfogt wie im 
Abschnitt \ref lnk_datenstruktur erläutert.

Variablendefinition der für die Parallelisierung benötigten Datenfelder in module_modell.f95\n
\n aus Datei parallel.f95; zurück: \ref index


parallel_ini()

startet mpi
\n\n

Textquelle: parallelisierung.md ; Codesource: parallel.f95 ;  
zurück: \ref lnk_datenstruktur