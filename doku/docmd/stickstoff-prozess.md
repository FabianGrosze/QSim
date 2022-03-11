Stickstoff - Prozesse {#lnk_stickstoff_prozesse}
===================== 

In QSim werden mehrere Fraktionen des Stickstoffs berechnet: der 
Gesamtstickstoff, \f$ N_ges \f$, Ammoniumstickstoff, \f$ NH_4^+ \f$, 
Nitritstickstoff, \f$ NO_2^- \f$ und Nitratstickstoff, \f$ NO_3^- \f$.
Jede dieser Fraktionen wird einzeln bilanziert.

Im Stickstoff-Baustein wird auch die Biomasse der Nitrifikanten Nitrosomonas
und Nitrobacter berechnet. 

## Teilprozesse ##
Gesamtsticktoff

* Sedimentation von organischem Material
* Sedimentation von Algen
<!-- * Ammoniumstickstofffluss in bzw. aus dem Sediment
* Nitratstickstofffluss in bzw. aus dem Sediment -->
* Aufnahme von Luftstickstoff durch Blaualgen
* Denitrifikation in der Wassersäule

Nitrifikanten

* Wachstum von Nitrosomonas und Nitrobacter
* Mortalität von Nitrosomonas und Nitrobacter
* Sedimentation von Nitrosomonas und Nitrobacter

Ammonium

* Verbrauch beim Algenwachstum
* Verbrauch durch Nitritation von Nitrosomonas
<!-- * Fluss in bzw. aus dem Sediment -->
<!-- * Umsatz durch Biofilme auf Makrophyten -->
* Freisetzung durch bakteriellen Abbau von Biomasse
* Freisetzung durch Algen (Respiration) planktisch und bentisch
* Freisetzung durch Rotatorien (Respiration+Fraß)
<!-- * Freisetzung durch Muscheln (Respiration+Fraß) -->

Nitrat

* Verbrauch beim Algenwachstum
<!-- * Fluss in bzw. aus dem Sediment -->
<!-- * Umsatz durch Biofilme auf Makrophyten -->
* Freisetzung durch Nitratation von Nitrobacter
* Denitrifikation in der Wassersäule

Nitrit

* Verbrauch durch Nitratation von Nitrobacter
* Freisetzung durch Nitritation von Nitrosomonas
<!-- * Umsatz durch Biofilme auf Makrophyten -->


<!-- Sauerstoffverbrauch

* O2-Verbrauch durch Nitrifikation NH4N -> NO2N -> NO3N -->
<!-- #mf: den Verbrauch von O2 würde ich im Baustein O2 beschreiben, weil es 
in die O2-Bilanz mit eingeht -->


## Bilanz des Gesamtsticktoffs ##

\f[ \frac{N_ges}{dt} = - C_{org, sed} \cdot q_{N:C} - 
\sum\limits_{j=1}^3 A_{sed, i} \cdot q_N_i +
u_N_2 \cdot (abltbr - algabz) - dNO3Den \f]
<!-- nicht mehr enthalten: Sediment-Flux, Verlust durch 
Konsum benthischer Filtrierer, Aufnahme durch benth. 
Algen -->

<!-- Einheiten in der folgenden Liste sind noch nicht geprüft --> 
\f$ N_ges \f$:   Gesamtstickstoff [mg N/L] \n
\f$ C_{org, sed} \f$: sedimentierte Menge des organischen Kohlenstoffs   [g/m3/d] \n
\f$ q_{N:C} \f$: Stickstoff zu Kohlenstoff-Verhältnis in org. gelöstem Material [-] \n
<!-- N:C oder C:N-Verhältnis? molar oder Masse? -->
\f$ A_{sed, i} \f$:  Sedimentierte Algenbiomasse der Algengruppe i  [g/m3/d] \n
\f$ q_N_i \f$: Stickstoffanteil der Algenbiomasse der Algengruppe i   [gN/g Biomasse ] \n
\f$ u_N_2 \f$:  Aufnahme von atm. Stickstoff durch Cyanobakterien  [ ] \n
\f$ abltbr \f$: Nettowachstumsrate Cyanobakterien   [ ] \n
\f$ algabz \f$: Respiration Cyanobakterien   [ ] \n
\f$ dNO3Den \f$:  Denitrifikation  [ ] \n
<!-- letzte drei noch durch Formelzeichen ersetzen -->

mit *dNO3Den*

dNO3Den = 0.93 * DenWatz

mit DenWatz = bsbCt(ior)*(KMO_NO3/(KMO_NO3 + vO2z(nkz,ior)))*(vNO3z(nkz,ior)/(vNO3z(nkz,ior) + KM_NO3))

mit 
KM_NO3 =  0.4
KMO_NO3 = 0.26


## Bilanz des Nitratstickstoffs ##

\f[ \frac{NO_3^-}{dt} =  \f]
vno3t = SUSN(ior)+PFLN1+hJNO3(mstr,ior)*tflie/Tiefe(ior) -algN3m - dNO3Den

\f$  \f$:    [ ] \n
\f$  \f$:    [ ] \n




\f[ eine Gleichung \f]

\f$ Si \f$:     Si-Konzentration im Gewässer [mg Si/L] \n
\f$  \f$:    [ ] \n
 
## QSim-Veröffentlichungen, die den ...-Baustein beschreiben und/oder anwenden: 
...
 

\n\n

Textquelle: stickstoff-prozess.md; Codesource: ncyc.f90; zurück: \ref lnk_stickstoff