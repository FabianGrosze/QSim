Einlesen von Verteilungen (Schwebstoff, Salz)  {#lnk_schwebstoff_salz}
=========================

Einige Eigenschaften des Wassers wie der Salzgehalt, die Konzentration an 
suspendierten Sedimenten und auch die Temperatur verändern dessen Dichte so 
nennenswert, dass sie sich auf die Strömung auswirken können und werden dann 
auch bereits in der hydraulischen Simulation berücksichtigt.

Es macht bei der Gewässergütesimulation mit QSim3D, die sich ja ohnehin auf die 
von einem hydraulischen Treiber offline abgelegten Transportinformationen 
stützt, Sinn, auch dessen Schwebstoff- oder Salz-Gehalt zu übernehmen,
falls der dort berechnet wurde. Für Temperaturen ist solch eine Übernahme zur 
Zeit noch nicht geplant.

# Schwebstoffkonzentrationen {#lnk_schwebstoff}
Bisher realisiert ist die Übernahme von Schwebstoffkonzentrationen:
Subroutine schwebstoff_salz_sichten() stellt fest ob ein Unterverzeichnis 
./trueb im aktuellen Modellverzeichnis existiert.
Ist keines vorhanden, werden auch keine Schwebstoffverteilungen eingelesen.

Im Unterverzeichnis trueb wird nach mit d beginnenden Dateien gesucht 
(z.B. ./trueb/d86400 ).
Die Zahl hinter dem Buchstaben d im Dateinamen wird als Zeit in ganzen Sekunden 
nach Berechnungsbeginn interpretiert.
Die Subroutine schwebstoff_salz() interpoliert dann die Schwebstoffverteilung 
für den aktuellen Rechenzeitpunkt (stoffumsatz-Zeitschritte).

Die Schwebstoffdateien müssen im Elcirc .gr3 Format vorliegen.
Dieses Format dient auch zur Definition der Berechnungsnetze von SCHISM.
Dort wo in den Netzdateien die Knotenhöhe stehen, muss nun die 
Schwebstoffkonzentration in ??? mg/l ??? angegeben werden.
Dadurch ist es möglich, die Zuordnung von Schwebstoffverteilungen mit denselben 
Werkzeugen (z.B. Netzgenerator Janet) vorzunehmen, mit denen auch die 
Höhenzuordnungen des Berechnungsnetzes erstellt wurden.

Die vorgegebene Schwebstoffkonzentration wird  in die Variable
\ref ss eingelesen und repräsentiert nur den ??? zusätzlichen ??? 
Schwebstoffanteil.
Das \ref lnk_phy_licht , das den \ref lnk_phytoplankton für die 
Photosynthese zur Verfügung steht, wird noch durch weitere Wasserinhaltsstoffe 
abgeschwächt.

# Salzkonzentrationen {#lnk_salz}
Zur Zeit noch nicht realisiert.

Quelle schwebstoff_salz.f95 zurück zu \ref lnk_weitere_stoffe
