Aufbau von QSim {#lnk_qsim_aufbau}
===============

\image html qsim_konzept.png "konzeptioneller Aufbau der Gütemodelle QSim-1D und QSim-3D"

Das Ineinandergreifen der Komponenten der QSim-Gütemodelle ist in obigem 
Schaubild zusammengestellt.

Beide, QSim1D und QSim3D beziehen die Informationen über den Strömungsvorgang 
aus einem als sog. "hydraulischen Treiber" (in Grün dargestellt), der seine 
Ergebnisse für ein zu berechnendes Güte-Ereignis abspeichert.
Die QSim-Programme laufen nach dem hydraulischen Treiber und lesen dessen 
Ergebnisse ein.

Zurzeit werden folgende hydraulische Treibe verwendet:
- \subpage lnk_hydrax (QSim1D)
- UnTRIM
- casu (QSim3D)

Zur Simulation der lokalen biochemischen und biologischen Stoffumsetzungsprozesse 
werden in beiden QSim's dieselben Module, die als Fortran Subroutinen realisiert 
sind, verwendet.


Die unterschiedliche räumliche Auflösung führt dazu, dass äquivalente 
Ergebnisauswertungen in QSim-3D mindestens eine Dimension größer sind.

Bedient werden beide QSim's mit \subpage lnk_gerris . Eine Anleitung zur 
Benutzung von GERRIS für mehrdimensionale Modelle findet sich 
[hier](\ref lnk_gerris_modellerstellung).

Textquelle: qsim_aufbau.md ; Codesources: code_file.f95 ;  
zurück: \ref lnk_ueberblick
 