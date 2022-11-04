Coliforme Bakterien/Keime (Hygiene) - Prozesse {#lnk_coliform_prozesse}
===============================================

## Teilprozesse
Fäkalcoliforme Bakterien vermehren sich im Gewässer nicht, sondern sterben 
rasch ab.\n\n

Berücksichtigt wird dabei:\n
* Grundverlustrate (temperaturabhängig)\n
* Licht-Dosis abhängige Verlustrate\n\n
Nicht gesondert berücksichtigt ist bisher:\n
* Grazing / Sedimentation

Die Verlustrate \f$k_C\f$ für Coli-Bakterien in einem Gewässer lässt sich wie 
folgt bestimmen:

\f{equation}{k_C = k_{0,T} + \Delta k_{C,I}\,c \f}

\f$k0,T\f$:	Verlustrate in Abhängigkeit von der Temperatur in °C [h-1] \n
\f$\Delta kC,I\f$:	Veränderung der Verlustrate unter Lichteinfluss [h-1] \n
\f$C\f$: Anzahl an Coli-Bakterien im Gewässer (Bakterien*ml-1) \n
\n\n

Der Einfluss der Temperatur auf die Verlustrate fäkalcoliformer Bakterien kann 
mit folgendem Ansatz beschrieben werden (Mancini, 1978):

\f{equation}{k_{0,T}  = k_{0,20} \cdot \theta^{T-20}\f}

\f$k0,20\f$: Verlustrate bei 20 °C ohne Berücksichtigung von Licht- und pH-Einfluss [h-1] \n
\f$\theta\f$: Faktor für die Temperaturabhängigkeit [-] \n
\f$T\f$: Gemessene Wassertemperatur [°C] \n
\n\n

Die Verlustrate fäkalcoliformer Bakterien steigt proportional zur Stärke der 
UV-Strahlung. Anstelle der UV-Strahlung wurde die Globalstrahlung zur 
Quantifizierung der Verlustrate verwendet (Auer & Niehaus, 1993). Die dort 
verwendete Einheit der Strahlungsstärke cal*cm-2*d-1 wurde auf die im Modell 
verwendete Einheit J*cm-2*h-1 umgerechnet.

![Abhängigkeit der Absterberate von fäkalcoliformen Bakterien von der Globalstrahlung (nach Auer & Niehaus, 1993).](img/coliform_Absterberate_Beleuchtung.png)
\n\n



Die Veränderung der Verlustrate fäkalcoliformer Bakterien durch Strahlung 
berechnet sich nach:

 
\f{equation}{\Delta k_{C,I}  = \alpha_{C,I} \cdot I \f}

\f$\alpha_{C,I} \f$: Steigung der Ausgleichsgeraden [J-1*cm2] \n
\f$I\f$: Gemessene Globalstrahlung [J*cm-2*h-1] \n
\n\n


# Rand- und Anfangsbedingungen 
...

# Veröffentlichungen

- <a href="./pdf/18_Becker_ReWaM_intern_BfG_FLUSSHYGIENE_Koblenz26_10_2018_AB_IH.pdf" target="_blank">Modellierung hygienischer Belastungen in Fließgewässern</a>

- <a href="./pdf/19_Fischer_SAME16-Potsdam_Hygienemodellierung-Berlin.pdf" 
target="_blank">To swim or not to swim</a>

- <a href="./pdf/18_Fischer_FLUSSHYGIENE_Abschluss_final_Mri.pdf" target="_blank">
Simulation von Maßnahmen zur langfristigen Verbesserung der hygienischen 
Wasserqualität</a>


\n\n

Textquelle: coliform-prozess.md; Codesource: coliform.f90; zurück: \ref lnk_coliform
