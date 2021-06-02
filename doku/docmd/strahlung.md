Strahlung {#lnk_strahlung}
=========

Die Strahlung wird im BfG-Gewässergütemodell zur Simulation folgender 
Prozesse benötigt:

- Wärmehaushalt des Fließgewässers <br>
 (Wellenlänge 290 - 4000 nm);
- Wachstum der Algen und Makrophyten durch Photosynthese <br>
 (Wellenlänge 400 – 700 nm);
- photolytisches Absterben der fäkalcoliformen Bakterien <br>
 (Wellenlänge 290 - 380 nm). 

Die einzelnen Strahlungskomponenten werden in QSim aus der Globalstrahlung 
berechnet. Diese wird als Messdaten von repräsentativen Wetterstationen 
des Modellgebiets bezogen. Die Eingabe der Globalstrahlung kann in Form 
einer Tagesstrahlungssumme oder aber als Zeitreihe in Stundenwerten erfolgen. 
Liegt eine Tagesstrahlungssumme vor, so wird aus dieser im Modell für jeden
Berechnungszeitschritt (i. d. R. eine Stunde) ein Strahlungswert ermittelt:

\f[
I_{\Delta t} = I_{max} \cdot 0,5 \cdot \left(1 + cos\left(2 \cdot \pi \cdot
\frac{time}{t_{hell}} \right) \right)
\f]

mit:
\f[ I_{max} = 2 \cdot \frac{I_{Glob}}{4,2 \cdot t_{hell}} \f]

\f[time = -0,5 + \frac{Uhrz - SA}{t_{hell}}\f]

\f[t_{hell} = SU - SA \f]

\f$I_{\Delta t}\f$: Globalstrahlung während des Berechnungszeitschritts 
\f$\Delta t\f$ [J cm<sup>-2</sup> h<sup>-1</sup>]

\f$I_{max}\f$: maximale Globalstrahlung am Tag [J cm<sup>-2</sup> h<sup>-1</sup>]

\f$\sum_{i=0}^n i^2 = \frac{(n^2+n)(2n+1)}{6}\f$

zurück: \ref lnk_waerme; Code in Datei *.f90

aus Datei strahlung.md
