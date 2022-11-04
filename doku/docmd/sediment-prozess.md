Sediment - Prozesse {#lnk_sediment_prozesse}
=====================

## Teilprozesse
Im Sediment-Baustein werden folgende Teilprozesse berücksichtigt:

- Umlagerung durch Benthosorganismen
- Diffusion im Sediment
- Sauerstoffverbrauch
- Mineralisation des organischen Materials im Sediment (Diagenese)
- Methanbildung
- Ammonium und Nitrat Freisetzung
- Phosphor Freisetzung
- Silizium Freisetzung

Es wird von folgender molarer Zusammensetzung des partikulären Materials im Sediment ausgegangen (Modellstart):

\f{equation}{C_{106} H_{240} O_{107} N_{9} P_{0,35} \f}

Hieraus ergibt sich bezogen auf das Gewicht des organischen Materials ein 
Kohlenstoffanteil \f$f_{POM_C}\f$ von 0.378, ein Stickstoffanteil \f$f_{POM_N}\f$ 
von 0.037 und ein Phosphoranteil \f$f_{POM_P}\f$ von 0.0032 am Modellstart. 
Diese Kohlenstoff-, Stickstoff- und Phosphoranteile des Detritus sind keine 
konstanten Werte, sondern werden im Modell fortlaufend aus den 
Gesamt-Kohlenstoff, -Stickstoff- und -Phosphorkonzentrationen berechnet. Für 
frisch abgelagerte Algenbiomasse wird von einem Kohlenstoffanteil von 0.48 sowie 
von den zellinternen Stickstoff- und Phosphoranteilen ausgegangen. Diese Anteile 
werden aus dem [Phytoplankton-Modul](\ref lnk_phytoplankton) übergeben.

Prozesse, die zu einem Stofftransport im Sediment führen, sind die Umlagerung 
von Sediment durch Benthosorganismen (Bioturbation), der Massentransfer von 
gelösten anorganischen und organischen Verbindungen und der Transport von 
organischem Material in tiefere Sedimentschichten durch Aufwachsen des 
Sedimentes mit frisch sedimentiertem organischem Material (Diagenese). Die 
Vermischungsgeschwindigkeit \f$\omega_{misch}\f$ beschreibt die Geschwindigkeit 
der Mischung partikulären Materials zwischen der 1. aeroben und der 2. anaeroben 
Schicht im Sediment durch Makrozoobenthos:
 
\f{equation}{\omega_{misch} = \frac{\omega_{misch,0} \cdot 
  \Theta_{misch}^{T-20}}{H_2} \cdot \frac{\frac{POC_1(2)}{1000 \cdot 
  (\rho_S \cdot (1 - \phi))}}{POC_{1,R}}
\f}

\f$\omega_{misch,0}\f$:	Vermischungsgeschwindigkeit für partikuläres organisches Material durch Bioturbation ohne Berücksichtigung der organischen Belastung und des Sauerstoffgehalts [m*d-1] \n
\f$\Theta_{misch}\f$: Temperaturkoeffizient für ωmisch [-] \n
\f$T\f$: Wassertemperatur [°C] \n
\f$H_2\f$: Dicke der anaeroben 2. Schicht im Sediment [10 cm] \n
\f$POC_1(2)\f$:	Gehalt an leicht abbaubaren partikulären organischen Kohlenstoffverbindungen in der 2. Schicht [g*m-3] \n
\f$\rho_S\f$: Dichte des Sediments [kg*m-3] \n
\f$\phi\f$:	Sedimentporosität [dm3*dm-3]  \n
\f$POC_{1,R}\f$: Referenzkonzentration an leicht abbaubaren partikulären organischen Kohlenstoffverbindungen [g C*kg-1].  \n
\n\n

Dann gilt: 

\f{equation}{\omega_{misch} = \frac{D_P \cdot \Theta_{Dp}^{(T-20)}}{H_2} \f}

Der Massenfluss von gelöstem Sauerstoff in das Sediment (SOD) lässt sich mit 
Hilfe des 1. Fick’schen Gesetz ermitteln:
 
\f{equation}{SOD = D_{O_2} \cdot \frac{dO_2}{dz} \f}

\f$DO2\f$: Diffusionskoeffizient für Sauerstoff im Sediment [m2*d-1] \n
\f$dO2\f$: Sauerstoffänderung als Funktion der Sedimenttiefe dz [g*m-3] \n
\f$dz\f$:  Sedimenttiefe, hier die Dicke der obersten aeroben Schicht H1 [m] \n
\n\n

Die Sauerstoffänderung im Sediment lässt sich auch wie folgt beschreiben:
 
\f{equation}{
 \begin{split}
  SOD = K_{L0,1,O2} \cdot (O_2(0) - O_2(1)) = \\
  D_{O_2} \cdot \frac{O2(0) - O_2(1)}{H_1}
 \end{split}
\f}

\f$DO2\f$:  Diffusionskoeffizient für Sauerstoff im Sediment [m2*d-1] \n
\f$H1\f$:   Dicke der obersten aeroben Schicht [m] \n
\f$KL0,1,O2\f$: Massentransferkoeffizient für Sauerstoff [m*d-1] \n
\f$O2(0)\f$: Sauerstoffkonzentration an der Sedimentoberfläche [mg*L-1] \n
\f$O2(1)\f$: Sauerstoffkonzentration am Boden der obersten Sedimentschicht [mg*L-1] \n
\n\n

Daraus folgt für den Massentransferkoeffizienten für Sauerstoff:
 
\f{equation}{K_{L0,1,O_2} = \frac{D_{O_2}}{H_1} \f}

In gleicher Weise lassen sich für die anderen gelösten Stoffe die 
Massentransferkoeffizienten bestimmen:

\f{equation}{K_{L0,1,NH_4} = \frac{D_{NH_4}}{H_1} \f}
\f{equation}{K_{L0,1,NO_3} = \frac{D_{NO_3}}{H_1} \f}
\f{equation}{K_{L0,1,PO_4} = \frac{D_{PO_4}}{H_1} \f}
\n\n

Die Beziehung zwischen der Turbulenz aus der Wassersäule und dem diffusiven 
Stofftransport im Sediment lässt sich über die Schmidt-Zahl herstellen:
 
\f{equation}{Sc = \frac{\nu_t}{D} \f}

\f$Sc\f$: Schmidt-Zahl [-] \n
\f$vt\f$: Eddy-Diffusionskoeffizient (Wirbel-, turbulente Viskosität) [m2*s-1] \n
\f$D\f$:  Diffusionskoeffizient eines Stoffes [m2*s-1] \n
\n\n

In einem hoch turbulenten Medium ist die Schmidt-Zahl 1. Der Übergang von der 
turbulenten zur molekularen Schmidt-Zahl ist jedoch nicht ganz klar. Im Modell 
wird deshalb angenommen, wenn:

\f{equation}{\frac{v_t}{v_0} \geq 10 \; \text{dann:}\; Sc = 1 \; \text{und} \; 
 D = v_t \f}

Ansonsten wird von molekularen Verhältnissen ausgegangen. Nach 
HIGASHINO & STEFAN (2008) besteht ein Zusammenhang zwischen der Eddy-Diffusion 
und der Reynoldszahl Re im Sediment. So lässt sich auf Berechnungsgrundlagen der 
Autoren für eine normierte Referenztiefe (z*) im Sediment ein 
Eddy-Diffusionskoeffizient aus der Reynoldszahl wie folgt bestimmen:

\f{equation}{\nu^{+}_{t,2} = \frac{1}{10^3} \cdot \left(12,84 \cdot 
  Re^{1,385} \cdot \frac{12,84 \cdot Re^{1,385}}{50,61^{1,385} + Re^{1,385}} \right)
\f}
<!-- #todo: herausfinden wo 1,385 herkommt und in Parameter beschreiben -->

\f{equation}{\nu_t^{+} = \frac{\nu_t}{U \cdot W \cdot T_P} \f}

\f{equation}{z^{*} = \frac{z}{W \cdot T_P \cdot \frac{W}{U}} \f}

\f$\nu^{+}_{t,2}\f$: normierter Eddy-Diffusionskoeffizient in z* = 2 [-] \n
\f$\nu_t^{+}\f$:    Eddy-Diffusionskoeffizient [m2*s-1] \n
\f$Re\f$:		Reynoldszahl [-] \n
\f$\nu_t\f$:	Eddy-Diffusionskoeffizient (Wirbel-, turbulente Viskosität) [m2*s-1] \n
\f$U\f$:	Amplitude des sinusförmigen Pulses der Turbulenz ins Sediment [m*s-1] \n
\f$W\f$:	Eindringtiefe des sinusförmigen Pulses der Turbulenz ins Sediment [m*s-1] \n
\f$T_P\f$:	Periode des Geschwindigkeitspulses [s] \n
\n\n

Die treibende Kraft für die Stoffkonzentrationen und -prozesse im Sediment ist 
der Fluss an partikulärem organischem Material aus der Wassersäule an die 
Gewässersohle (Sedimentation), der über eine längere Zeitspanne andauern muss, 
da sich sonst keine entsprechend dicke Sedimentschicht bilden kann. Der 
Sedimentationsfluss JPOC_neu des während der Simulation sedimentierenden 
partikulären organischen Kohlenstoffs errechnet sich als Mittelwert aus den 
einzelnen Zeitschritten jeweils bis zum aktuellen Simulationszeitpunkt:

\f{equation}{J_{POC_neu} = \frac{\frac{\sum_{i 0 1}^n(POC_{Phyto,sed} + 
  POC_{Det,sed}) \cdot Tiefe}{\Delta t}}{i} \f}

\f$J_{POC_neu}\f$:	mittlerer Sedimentationsfluss an partikulärem organischen Kohlenstoff 		während der Simulation [gC*m-2*d-1] \n
\f$n\f$:	Anzahl der Zeitschritte \n
\f$i\f$:	Laufvariable der Zeitschritte \n
\f$POC_{Phyto,sed}\f$:	pro Zeitschritt sedimentierter Phytoplankton-Kohlenstoff [gC*m-2*d-1] \n
\f$POC_{Det,sed}\f$:	pro Zeitschritt sedimentierte Menge an Detritus-Kohlenstoff [gC*m-2*d-1] \n
\f$Tiefe\f$:	Höhe der Wassersäule über dem Sediment am Berechnungspunkt [m] \n
\f$\Delta t\f$:	Berechnungszeitschrittweite [d] \n

In ähnlicher Weise erfolgt die Bestimmung des Sedimentationsflusses JPON_neu 
für Stickstoff:
 
\f{equation}{J_{PON_neu} = \frac{\frac{\sum_{i 0 1}^n(PON_{Phyto,sed} + 
  PON_{Det,sed}) \cdot Tiefe}{\Delta t}}{i} \f}

und Phosphor (JPOP_neu):

 
\f{equation}{J_{POP_neu} = \frac{\frac{\sum_{i 0 1}^n(POP_{Phyto,sed} + 
  POP_{Det,sed}) \cdot Tiefe}{\Delta t}}{i} \f}

mit

\f$PONPhyto,sed\f$:	pro Zeitschritt sedimentierte Menge an Phytoplankton-Stickstoff [gN*m-2*Δt-1] \n
\f$PONDet,sed\f$:	pro Zeitschritt sedimentierte Menge an Detritus-Stickstoff [gN*m-2*Δt-1] \n
\f$POPPhyto,sed\f$:	pro Zeitschritt sedimentierte Menge an Phytoplankton-Phosphor [gP*m-2*Δt-1] \n
\f$POPDet,sed\f$:	pro Zeitschritt sedimentierte Menge an Detritus-Phosphor [gP*m-2*Δt-1] \n

Schließlich ergibt sich die Schichtdicke \f$H_1^*\f$ der alt abgelagerten 
partikulären Kohlenstoffverbindungen aus:

\f{equation}{H_1^* = H_1 - H_{sed,orgC} \; \; H_1^* \geq 0 \f}

\f$H1*\f$: Schichtdicke der alt abgelagerten partikulären organischen Kohlenstoffverbindungen [m] \n
\f$H1\f$:	Dicke der aeroben Schicht [m] \n
\f$Hsed,orgC\f$: Schichtdicke der frisch abgelagerten Kohlenstoffverbindungen [m] \n


# Rand- und Anfangsbedingungen 

# QSim-Veröffentlichungen, die den BausteinA beschreiben und/oder anwenden:

- Zur Dokumentation des Sediment-Moduls existiert der
<a href="./pdf/BfG1843_Sedimentmodul.pdf" target="_blank">BfG-Bericht 1843</a> (vom August 2016)
"Das Sediment-Modul SEDFLUX im Gewässergütemodell QSim"


\n\n

Textquelle: sediment-prozess.md ; Codesource: SedFlux.f90, sedflux_huelle.f95 ;
zurück: \ref lnk_sediment
