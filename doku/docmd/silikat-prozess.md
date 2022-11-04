Silikat - Prozesse {#lnk_silikat_prozesse}
===================== 

## Teilprozesse ##
Folgende Teilprozesse wirken sich in QSim auf den Silikatgehalt des Wassers aus: 

* Verbrauch durch Wachstum planktischer Kieselalgen 
* Verbrauch durch Wachstum bentischer Kieselalgen
<!-- * Rücklösung aus dem Sediment -->

Momentan sind im Modell die Nährstofffluxe am Sediment ausgeschaltet, ebenso 
der Baustein benthischer Kieselalgen. Somit berechnet sich die Änderung an 
Silikat im Gewässer momentan allein aus den Si-Zuflüssen und der Si-Aufnahme 
durch planktische Kieselalgen.

Vereinfacht lässt sich die Änderung des Silikats wie folgt beschreiben:

\f[ \frac{dSi}{dt} = - Si_{up} \f]

\f$ Si \f$:     Si-Konzentration im Gewässer [\f$ mg Si \cdot L^{-1} \f$] \n
\f$ Si_{up}\f$: Verlustrate von Si: Aufnahme durch Primärproduzenten [\f$ mg Si \cdot L^{-1} \cdot t^{-1} \f$] \n

### Aufnahme durch Primärproduzenten ###
Der Verlust von Silikat findet hauptsächlich durch die Aufnahme durch Primärproduzenten
statt.

\f[ Si_{up} = - u_{Si} \cdot (\mu_{diat} - A_{resp, diat}) \f]
<!-- akisi = -up_Si_s * (akibr_S - algak_s) - albewk_s * Qmx_SK -->

\f$ Si_{up} \f$:     Verlustrate von Si: Aufnahme durch Primärproduzenten [\f$mg Si \cdot L^{-1} \cdot t^{-1}\f$] \n
\f$ u_{Si} \f$:      Si-Aufnahmerate der Kieselalgen [\f$ mg Si \cdot mg Bio^{-1} \f$] \n
\f$ \mu_{diat} \f$:  Brutto-Zuwachs Kiesel-Algen-Biomasse [\f$ mg Bio \cdot L^{-1} \cdot t^{-1}\f$] \n
\f$ A_{resp, diat} \f$:    Respirierte Algenbiomasse der Kieselalgen [\f$ mg Bio \cdot L^{-1} \cdot t^{-1} \f$] \n

## QSim-Veröffentlichungen, die den Silikat-Baustein beschreiben und/oder anwenden: 
...

\n\n

Textquelle: silikat-prozess.md; Codesource: silikat.f90; zurück: \ref lnk_silikat
