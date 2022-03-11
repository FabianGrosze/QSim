Silikat - Prozesse {#lnk_silikat_prozesse}
===================== 

## Teilprozesse ##
Folgende Teilprozesse wirken sich in QSim auf den Silikatgehalt des Wassers aus: 

* Rücklösung aus dem Sediment
<!-- * Rücklösung aus schwebendem Detritus (*noch Unklarheiten*) -->
<!-- #mf: ich glaube schwebendes Detritus spielt momentan keine Rolle -->
* Verbrauch durch Wachstum planktischer Kieselalgen 
* Verbrauch durch Wachstum bentischer Kieselalgen

Vereinfacht lässt sich die Änderung des Silikats wie folgt beschreiben:

\f[ \frac{dSi}{dt} = Si_{Sed} - Si_{up} \f]

\f$ Si \f$:     Si-Konzentration im Gewässer [mg Si/L] \n
\f$ Si_{Sed} \f$: rückgelöstes Silikat aus dem Sediment [g Si/m3] \n
<!-- Si_{Sed} = dSised; nachprüfen ob Zeiteinheit fehlt oder nicht (hängt von tflie ab)-->
\f$ Si_{up}\f$: Si-Aufnahme durch pelagische und benthische Kieselalgen [mg Si/d] \n
 
### Rücklösung aus dem Sediment ###

Die Silikatfreisetzung wird über eine konstante Rücklösungsrate, 
\f$ r_{Sed,Si} \f$, berechnet.
<!-- wahrscheinlich ist die Rate nicht konstant; muss überprüft werden -->

\f[ Si_{Sed} = r_{Sed,Si} \cdot \Delta t/h ]\f
<!-- dSised = hJSi(mstr,ior)*tflie/tiefe(ior) --> 
 
\f$ Si_{Sed} \f$:   rückgelöstes Silikat aus dem Sediment [g Si/m3] \n
\f$ r_{Sed,Si} \f$:  Silizium-Flux aus dem Sediment [g Si/m2/d] \n <!-- hJsi -->
\f$ \Delta t \f$:   Zeitschrittweite [\f$ d?? \f$] \n
\f$ h \f$:  mittlere Wassertiefe [m] \n
 
### Aufnahme durch Primärproduzenten ###
Der Verlust von Silikat findet hauptsächlich durch die Aufnahme durch Primärproduzenten
statt. In Fließgewässern spielen sowohl pelagische als auch benthische Diatomeen
dabei eine Rolle.

\f[ Si_{up} = - u_{Si} \cdot (\mu_{diat} - r_{diat}) - \mu_{bent} \cdot q_{Si, max} \f]
<!-- akisi(nkz) = -up_Siz(nkz,ior)*(akibrz(nkz,ior)-algakz(nkz,ior))-albewk(ior)*Qmx_SK  -->

\f$ Si_{up} \f$:     Verlustrate von Si: Aufnahme durch Primärproduzenten [mg Si/L/Zeitschritt] \n
\f$ u_{Si} \f$:      Si-Aufnahmerate der Kieselalgen [mgSi/(mgBio*d)] \n
\f$ \mu_{diat} \f$:  Brutto-Zuwachs Kiesel-Algen-Biomasse [mgBio/l je Zeitschritt] \n
\f$ r_{diat} \f$:    Respirierte Algenbiomasse der Kieselalgen [mgBio/l je Zeitschritt] \n
\f$ \mu_{bent} \f$:  Wachstum benthischer Kieselalgen [mg Bio/l je Zeitschritt] \n
\f$ q_{Si, max} \f$: max. Si-Gehalt der Kieselalgenzelle [mg Si/mgBio] \n
 
## QSim-Veröffentlichungen, die den Silikat-Baustein beschreiben und/oder anwenden: 
...
<!-- - [U2 et al., 2002](./pdf/U2_et_al_2002rhein.pdf) #mf: finde pdf nicht -->
 

\n\n

Textquelle: silikat-prozess.md; Codesource: silikat.f90; zurück: \ref lnk_silikat