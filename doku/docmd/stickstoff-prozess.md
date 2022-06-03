Stickstoff - Prozesse {#lnk_stickstoff_prozesse}
===================== 

In QSim werden mehrere Fraktionen des Stickstoffs berechnet: der 
Gesamtstickstoff, \f$ N_ges \f$, Ammoniumstickstoff, \f$ NH_4^+ \f$, 
Nitritstickstoff, \f$ NO_2^- \f$ und Nitratstickstoff, \f$ NO_3^- \f$.
Jede dieser Fraktionen wird einzeln bilanziert.

Im Stickstoff-Baustein wird auch die Biomasse der Nitrifikanten Nitrosomonas
und Nitrobacter berechnet. 

## Teilprozesse ##
Gesamtsticktoff (\f$ N_ges \f$)

* Sedimentation von organischem Material
* Sedimentation von Algen
<!-- * Ammoniumstickstofffluss in bzw. aus dem Sediment
* Nitratstickstofffluss in bzw. aus dem Sediment -->
* Aufnahme von Luftstickstoff durch Blaualgen
* Denitrifikation in der Wassersäule

Nitrifikanten (Nitrosomonas + Nitrobacter)

* Wachstum von Nitrosomonas und Nitrobacter
* Mortalität von Nitrosomonas und Nitrobacter
* Sedimentation von Nitrosomonas und Nitrobacter

Ammonium (NH4-N)

* Verbrauch beim Algenwachstum
* Verbrauch durch Nitritation von Nitrosomonas
<!-- * Fluss in bzw. aus dem Sediment -->
<!-- * Umsatz durch Biofilme auf Makrophyten -->
* Freisetzung durch bakteriellen Abbau von Biomasse
* Freisetzung durch Algen (Respiration) planktisch und bentisch
* Freisetzung durch Rotatorien (Respiration+Fraß)
<!-- * Freisetzung durch Muscheln (Respiration+Fraß) -->

Nitrat (NO3-N)

* Verbrauch beim Algenwachstum
<!-- * Fluss in bzw. aus dem Sediment -->
<!-- * Umsatz durch Biofilme auf Makrophyten -->
* Freisetzung durch Nitratation von Nitrobacter
* Denitrifikation in der Wassersäule

Nitrit (NO2-N)

* Verbrauch durch Nitratation von Nitrobacter
* Freisetzung durch Nitritation von Nitrosomonas
<!-- * Umsatz durch Biofilme auf Makrophyten -->


<!-- Sauerstoffverbrauch

* O2-Verbrauch durch Nitrifikation NH4N -> NO2N -> NO3N -->
<!-- #mf: den Verbrauch von O2 würde ich im Baustein O2 beschreiben, weil es 
in die O2-Bilanz mit eingeht -->


## Bilanz des Gesamtsticktoffs ##

\f[ \frac{N_ges}{dt} = - C_{org, sed} \cdot q_{N:C} - 
\sum\limits_{j=1}^3 A_{sed, i} \cdot q_N{_i} +
u_N{_2} \cdot (A_{wachs, cyan} - A_{resp, cyan}) - r_{denitrif} \f]
<!-- nicht mehr enthalten: Sediment-Flux, Verlust durch 
Konsum benthischer Filtrierer, Aufnahme durch benth. 
Algen -->

<!-- Einheiten in der folgenden Liste sind noch nicht geprüft --> 
\f$ N_ges \f$:   Gesamtstickstoff [mg N/L] \n
\f$ C_{org, sed} \f$: sedimentierte Menge des organischen Kohlenstoffs   [mg C/L/t] \n
\f$ q_{N:C} \f$: Stickstoff zu Kohlenstoff-Verhältnis in org. gelöstem Material [mg N/mg C] \n
<!-- N:C oder C:N-Verhältnis? molar oder Masse? -->
\f$ A_{sed, i} \f$:  Sedimentierte Algenbiomasse der Algengruppe i  [mg Biomasse/L/t] \n
\f$ q_N{_i} \f$: Stickstoffanteil der Algenbiomasse der Algengruppe i   [mg N/mg Biomasse ] \n
\f$ u_N{_2} \f$:  Aufnahme von atm. Stickstoff durch Cyanobakterien  [ ? mg N/t ?] \n
\f$ A_{wachs, cyan} \f$: Nettowachstums Cyanobakterien   [mg Biomasse/t ] \n
<!-- integriertes Wachstum; Änderung der Cyanos im Zeitschritt -->
\f$ A_{resp, cyan} \f$: respirierte Biomasse der Cyanobakterien  [ mg Bio/L/t ] \n
\f$ r_{denitrif} \f$:  Denitrifizierung  [ ] \n
<!-- letzte drei noch durch Formelzeichen ersetzen -->


## Bilanz des Nitrat-Stickstoffs 
\f[ \frac{NO_3^-}{dt} = r_{NH_4, nitrif} - up_{NO3, A_i} - r_{denitrif} \f]

<!-- vno3t = vno3(ior) + SUSN2 + PflN2 + hJNO3(mstr,ior) * tflie/Tiefe(ior) 
             - algN3m - dNO3Den
bzw.
vno3t = vno3(ior) + SUSN(ior) + PFLN1 + hJNO3(mstr,ior) * tflie/Tiefe(ior) 
             - algN3m - dNO3Den-->
<!-- PFLN1/PFLN2, hJNO3 sind ausgeschaltet --> 			 

\f$ NO_3^- \f$: Nitrat-N Konzentration [mg N/L] \n
\f$ r_{NH_4, nitrif} \f$: durch suspendierte Nitrifikanten oxidierte NH4-Menge [??] \n <!-- susn -->
\f$ up_{NO3, A_i} \f$:  Nitrataufnahme der i-ten Algengruppe [mg N/L/t] \n
\f$ r_{denitrif} \f$: Denitrifizierung [\f$  \f$] <!-- dNO3Den -->


## Bilanz des Nitrit-Stickstoffs 
Ammonium wird durch die 1. Stufe der Nitrifikation durch Nitrosomonas abgebaut 
und als Nitrit angereichert (NH4N -> NO2N), \f$ r_{NH_4, nitrif} \f$. 
Anschließend wird Nitrit durch Nitrobacter abgebaut (NO2N-> NO3N), 
\f$ r_{NO_2, nitrif} \f$:

\f[ \frac{NO_2^-}{dt} = r_{NH_4, nitrif} - r_{NO_2, nitrif} \f]

<!-- vno2t = vno2(ior) + susn(ior) + PFLN1 - susN2 - PfLN2 -->

\f$ r_{NH_4, nitrif} \f$: durch suspendierte Nitrifikanten oxidierte NH4-Menge [??] \n <!-- susn -->
\f$ r_{NO_2, nitrif} \f$: durch suspendierte Nitrifikanten oxidierte NO2-Menge [??] \n <!-- susn2 -->


## Bilanz des Ammonium-Stickstoffs
\f[ \frac{NH_4^+}{dt} = - r_{NH_4, nitrif} + doN + dzN - up_{NH4, A_i} 
		 + arN4m  \f]

<!-- NH4t = - susN(ior) + hJNH4(mstr,ior) * tFlie/Tiefe(ior) - pflN1 
         + doN(ior) + dzN + ddrN - aGrNH4(ior) - aKiNH4(ior) - aBlNH4(ior) 
		 + arN4m -->

Der Sedimentflux, die Aufnahme durch sessile Nitrosomonas und die NH4-Freisetzung 
durch die Respiration benthischer Filtrierer sind ausgeschaltet.		 
Die Ammoniumfreisetzung beim C-Abbau \f$ doN \f$ wird im Baustein <orgC> berechnet.
<!-- #mf: beizeiten Link zu entsprechender Stelle orgC-Doku einfügen -->

\f$ r_{NH_4, nitrif} \f$: durch suspendierte Nitrifikanten oxidierte NH4-Menge [??] \n <!-- susn -->
\f$ doN \f$: Ammoniumfreisetzung beim C-Abbau [\f$ mg N \cdot L^{-1} \cdot t^{-1} \f$] \n
\f$ dzN \f$: Ammoniumfreisetzung durch Zooplankton (Rotatorienrespiration) [\f$  \f$] \n
\f$ up_{NH4, A_i} \f$: Ammonium-Aufnahme durch Algengruppe *i* [\f$  \f$] \n <!-- aGrNH4, aKiNH4, aBlNH4 -->
\f$ arN4m \f$:  [\f$  \f$] \n


arN4m = nresgz(1) + nreskz(1) + nresbz(1)  \n
! = respiriertes NH4? woher kommt der Wert? 
+ warum steckt das dann nicht in NO3-N-Bilanz auch für NO3?


## Teilprozesse

### Oxidation von NH4-N durch suspendierte Nitrifikanten
\f[ r_{denitrif} = (VXzt - VX0)/EKX0 \f]

<!-- susN = (VXzt - VX0)/EKX0 -->

vxzt: (s.u.)
vx0: Biomasse der Nitrosomonas
ekx0: = 0,06 (konstant)

VXzT(nkz) = VX0 * exp(YN * TFLIE)
vx0: Biomasse der Nitrosomonas
yn: Wachstumsrate (s.u.)
tflie: Zeitschritt


### Wachstum der Nitrosomonas
YN = YNMAX1 * fph1n3 * (VNH4/(STKS1 * fph1n2 + VNH4z)) * ALPHAO * alphat
	  
YNMAX1: Max. Wachstum Nitrosomonas (0,74) [1/d]
fph1n3: Einfluss des pH (s.u.)
VNH4: Ammonium-Stickstoff-Konzentration
STKS1: Halbsättigung Nitrosomonas (Standard: 0,83) [mgNH4-N/l] (= glob Param)
fph1n2: Einfluss des pH, wenn vx02 > 0 (s.u.) 
VNH4z: Ammonium-N Konzentration, tiefenaufgelöst

fph1n3 = 1./(1.+vnh3/KNH3_X1)
KNH3_X1: = 35

fph1n2 = 1. + vhno2/KHNO2_X1
KHNO2_X1 = 5.e-5

### susn2
SUSN2 = (VX2zt-VX02)/EKX02

VX2zt: (s.u.)
VX02: Biomasse der Nitrobacter
EKX02: = 0,02 (konstant)

*wenn vx02 <= 0, dann ist susn2 = 0*

VX2z(nkz) = VX02 * exp(YN*TFLIE) 

vx02: Biomasse der Nitrobacter
yn: Wachstumsrate (s.u.)
tflie: Zeitschritt

YN = YNMAX2*fph2n2*(vno2(ior)/(STKS2*fph2n3+VNO2z(nkz,ior)))*ALPHAO*alphat

YNMAX2: Max. Wachstum Nitrobacter (0,38) [1/d]
fph2n2: Einfluss des pH (s.u.)
vno2: Nitrit-Stickstoff-Konzentration
STKS2: Halbsättigung Nitrobacter (Standard: 0,05) [mgNO2-N/l] (= glob Param)
fph2n3: Einfluss des pH, wenn vx02 > 0 (s.u.) 
VNO2z: Nitrit-N Konzentration, tiefenaufgelöst
ALPHAO: Faktor für die O2-Abhängigkeit
alphat: Faktor für die T-Abhängigkeit

fph2n2 = 1./(1.+vhno2/KHNO2_X2)
KHNO2_X2 = 0.18

fph2n3 = 1.+vnh3/KNH3_X2
KNH3_X2 = 0.75


### Denitrifikation

*dNO3Den*

dNO3Den = 0.93 * DenWatz

mit DenWatz = bsbCt(ior)*(KMO_NO3/(KMO_NO3 + vO2z(nkz,ior)))*(vNO3z(nkz,ior)/(vNO3z(nkz,ior) + KM_NO3))

mit 
KM_NO3 =  0.4
KMO_NO3 = 0.26


### Wachstum der Nitrifikanten - Nitrobacter

\f[ eine Gleichung \f]

\f$ Symbol \f$:    Beschreibung [Einheit] \n
\f$  \f$:    [ ] \n



### Abhängigkeit der Nitrifikationsleitung sessiler Nitrifikanten von der
 Fließgeschwindigkeit
FV = 1.+(9.5*(vmod-0.045))
VMOD = abs(VMITT)*(RHYMO/RHYD)**0.5 

! vmod:
! vmitt: Strömungsgeschwindigkeit
! rhymo: = 0.00875
! rhyd: hydraulischer Radius

### alphao
alphao = 1 - exp(-1.39 * (O_2 - 0.5)) ! NACH HAJEK,NEUMANN,BISCHOFSBERGER 
if (ALPHAO < 0.0) ALPHAO = 0.0 

! vo2: Sauerstoffkonz.

### alphat			
if (tempw(ior) < 15.) then 
	ALPHAT = 0.75*1.108**(TEMPW(ior)-15.) 
else
	ALPHAT = 1.5/(((TEMPW(ior)-32.)/17.)**2+1) 
endif
		

\f[ eine Gleichung \f]

\f$ Symbol \f$:    Beschreibung [Einheit] \n
\f$  \f$:    [ ] \n
 
## QSim-Veröffentlichungen, die den ...-Baustein beschreiben und/oder anwenden: 
...
 

\n\n

Textquelle: stickstoff-prozess.md; Codesource: ncyc.f90; zurück: \ref lnk_stickstoff