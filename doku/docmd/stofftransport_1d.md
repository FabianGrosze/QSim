Stofftransport QSim1D - Prozesse {#lnk_stofftransport_1d}
================================

Für die Berechnung der Verteilung eines Stoffes wird in QSim die 
1D-Advektions-Dispersions-Gleichung verwendet. Diese beschreibt die 
Massenänderung eines Stoffes auf dem Fließweg in Abhängigkeit von der Fließzeit 
in der folgenden Form:

\f{equation}{\frac{\partial C}{\partial t} = -\nu \frac{\partial C}{\partial x} +
  D_L \frac{\partial^2C}{\partial x^2} \f}

_C_:	Stoffkonzentration	[\f$\ugL\f$] \n
_t_:	Zeit	[\f$s\f$]  \n
_v_:	querschnittsgemittelte Fließgeschwindigkeit 	[\f$\ms\f$] \n
_x_:	Weg in Fließrichtung	[\f$m\f$] \n
\f$D_L\f$:	longitudinaler Dispersionskoeffizient	[\f$\mqs\f$] \n
  
Die zeitliche Änderung der Stoffkonzentration an jedem festen Ort _x_ zur Zeit 
_t_ ist gegeben durch \f$\frac{\partial C}{\partial t}\f$, während der erste Term die 
Advektion und der zweite Term die Dispersion beschreibt. Demnach enthält die 
Transportgleichung sowohl hyperbolische (Advektion) als auch parabolische 
(Dispersion) Terme, die mit Hilfe eines Operator-Splitting-Verfahrens getrennt 
von einander gelöst werden. 

Die querschnittsgemittelte Fließgeschwindigkeit \f$\nu\f$ wird vorab im 
hydrodynamischen Modell HYDRAX auf der Grundlage der Saint-Venant-Gleichung 
berechnet und durch eine offline Kopplung in QSim bereitgestellt. 
\n\n

# Lösung des Advektionsterms

Für die Brechnung des gelösten Stofftranpsortes, wird der Flusslauf entsprechend 
der Querprofile in Segmente _i_ unterteilt (s. Abbildung unten). 
Zum Zeitpunkt _n_ sind 
die Konzentrationen _C_ im Segment bekannt. Um die Konzentrationen zu einem 
neuen Zeitpunkt _n+1_ zu bestimmen, werden diese während eines Zeitschrittes 
durch das Einströmen und Ausströmen von Stoffmengen über die linke bzw. rechte 
Kante bilanziert (Abbildung unten, gestrichelte Linie). Demnach müssen die 
Konzentrationen auf den Kanten bestimmt werden, wofür in QSim verschiedende 
numerische Lösungsverfahren (CIP, LAX-Wendroff und QUICKEST) zur Verfügung 
stehen. 
\n\n 

![Eindimensionale Diskretisierung eines Flusslaufes.](img/transport_1d_flusslauf.png )
\n\n


# CIP

Der Advektionsterm kann mit der Cubic Interpolated Pseudo-particle (CIP) Methode 
gelöst werden. Hierbei handelt es sich um ein Semi-Lagrange-Verfahren, welches 
für jeden Punkt eines festen Berechnungsgitters die Bahnlinie, die ein 
Fluidpartikel während des Zeitschrittes von einem Startpunkt aus bis zum 
jeweiligen Gitterpunkt verfolgt, ermittelt. Die Konzentration _C_ am Startpunkt 
entspricht dann der Stoffkonzentration zum neuen Zeitpunkt am jeweilgen 
Gitterpunkt \f$C_i^{n+1}\f$. Da der Startpunkt aber meist zwischen zwei Punkten 
des Berechnungsgitters liegt, für die die Stoffkonzentrationen bekannt sind, 
muss die Konzentrationam Startpunkt interpoloert werden. Dies erfolgt in QSim 
mit Hilfe eines kubischen Splines. Die Methode weist eine geringe numerische 
Diffusion auf und kontruiert eine Lösung innerhalb der Gitterzelle, die der 
realen Lösung sehr nahe kommt. Die Lösung des Advektionsterms mit Hilfe des 
CIP-Verfahren lautet:

\f{equation}{C_i^{n+1} = a \cdot x_{i,b}^3 + b \cdot x_{i,b}^2 + C_i^{'n} \cdot 
  x_{i,b} + C_i^n \f}

mit 

\f{equation}{a = \frac{C_i^{'n} + C_{i-1}^{'n}}{-\Delta x_{i-1}^2} + 
  \frac{2 \cdot(C_i^n - C_{i-1}^n )}{-\Delta x^3_{i-1}} ;   
  b = \frac{3 \cdot (C_{i-1}^n - C_i^n)}{-\Delta x_{i-1}^2} - 
  \frac{2 \cdot (C_i^{'n} + C_{i-1}^{'n})}{- \Delta x_{i-1}} ; \f}

\f{equation}{x_{i,b} = \nu_i \cdot \left(\Delta t - \frac{\Delta x_{i-1}}{\nu_i} \right)\f}
 
<!-- #rv: Gleichung x_{i,b} Muss im Code nochmal überprüft werden, 
ob das so stimmt. --> 
 
| Parameter | Beschreibung | Einheit | 
| --------- | ------------ | ------- | 
| i              | Gitterpunkt   |  | 
| n              | 	Zeitschritt	 |  | 
| \f$C^{'}\f$      | räumliche Ableitung der Konzentration | [\f$\ugL\f$] | 
| \f$\Delta x\f$ | Länge des Querprofils | [m] | 
| \f$\Delta t\f$ | Iterationslänge       | [s] | 

\n\n

# LAX-Wendroff

Das LAX-Wendroff-Verfahren ist ein explizites Verfahren, welches die CFL-Zahl 
(Courant-Friedrichs-Lewy-Zahl) verwendet. Diese setzt die Fließgeschwindigkeit, 
Elementlänge und den Rechenzeitschritt  ins Verhältnis. Es ist stabil für 
CFL ≤ 1 bzw. exakt für CFL = 1. Die Lösung des Advektionsterms mit Hilfe des 
LAX-Wendroff-Verfahren lautet:

\f{equation}{C_i^{n+1} = C_i^n - (\overline{Co} \cdot (C_i^n - C_{i-1}^n)) - 
  \frac{1}{2} \overline{Co} \cdot (1 - \overline{Co}) \cdot \phi_i + 
  \frac{1}{2} \overline{Co} \cdot \phi_{i-1} \f}
  
<!-- #rv: In der Literatur findet man vor allem: 
 C_i^{n+1} = C_i^n - \frac{1}{2} \overline{Co} \cdot (C_{i+1}^n - C_{i-1}^n) +
  \frac{1}{2} \overline{Co}^2 \cdot (C_{i+1}^n - 2 C_i^n + C_{i-1}^n)
  Sollte auch nochmal überprüft werden?!
-->  

mit \f{equation}{\phi_i = C_{i+1}^n - C_i^n  \f};

\f{equation}{\overline{Co} = \frac{Co_i + Co_{i-1}}{2} \; mit \;
  Co_i = \frac{\overline\nu_i \Delta t}{\Delta x_i} ; \; 
  \overline\nu_i = \frac{1}{2}(\nu_i + \nu_{i+1});   \;
  \overline x = \frac{1}{2}(\Delta x_{i-1} + \Delta x_i) \f}
  
| Parameter | Beschreibung | Einheit | 
| --------- | ------------ | ------- | 
| \f$\overline{Co}\f$ | mittlere Courant Zahl zweier benachbarter Gitterpunkte |  | 
| \f$Co\f$ | Courant Zahl 	 |  | 
| \f$\overline\nu\f$ | mittelere Fließgeschwindigkeit zweier benachbarter Gitterpunkte | [m s-1] | 
| \f$\overline x\f$ | mittlere Länge zweier benachbarter Gitterpunkte | [m] | 
\n\n

# QUICKEST

Das QUICKEST-Verfahren ist eine Erweiterung des QUICK-Schemas. Für die 
Bestimmung von \f$C_i^{n+1}\f$ werden Konzentrationen aus den Elementen 
unterstrom und oberstrom verwendet. Für die Stabilisierung der Lösung werden 
zudem weitere Terme basierend auf der CFL-Zahl berücksichtigt. Numerisch 
gesehen ist diese Methode stabil, weist aber eine starke künstliche Diffusion 
auf. Die Lösung des Advektionsterms mit Hilfe des QUICKEST-Verfahren lautet:

\f{equation}{C_i^{n+1} = \frac{1}{2}(C_i^n + C_{i+1}^n) - \frac{1}{2}Co_i \cdot 
  (C_{i+1}^n - C_i^n) - \frac{1}{6}\Delta x_i^2 \cdot (1 - Co_i^2) \cdot 
  \frac{\frac{C_{i+1}^n - C_i^n}{\Delta x_i} - 
  \frac{C_i^n - C_{i-1}^n}{\Delta x_{i-1}}}{\overline x} \f}

\n\n

# Lösung des Dispersionsterms

## Crank-Nicolson-Verfahren

Wird der Advektionsterm mit Hilfe des CIP-Verfahren gelöst, folgt die Lösung 
des Dispersionsterms mit Hilfe des Crank-Nicolson-Verfahrens. 

## MacCormack-Verfahren

Wird der Advektionterm mit Hilfe des Lax-Wendroff oder des QUICKEST-Verfahren 
gelöst, folgt die Lösung des Dispersionsterms mit Hilfe des 
MacCormack-Verfahrens. Hierbei handelt es sich um ein Zweitschritt 
Finite-Differenzen-Verfahren. Im ersten Schritt (predictor step) wird C wie 
folgt berechnet:

\f{equation}{C^{* n+1}_i = C_i^n + S1_i \cdot \Delta t\f}

mit
\f{equation}{S1_i = \frac{\frac{1}{2}(D_{L_{i-1}} + D_{L_i}) \cdot 
  \left(C_{i+1}^n - \left(1 + \frac{\Delta x_i}{\Delta x_{i-1}} \right) \cdot 
  C_i^n + \frac{\Delta x_i}{\Delta x_{i-1}} \cdot C_{i-1}^n \right)}{
  \frac{1}{2} \frac{\Delta x_i}{\Delta x_{i-1}} \cdot \left(1 + \frac{∆x_i}{
  \Delta x_{i-1}} \right) \cdot \Delta x_{i-1}^2} \f}

Im zweiten Schritt (corrector step) wird der zuvor berechnete Wert für C 
entsprechend korrigiert:

\f{equation}{C_i^{n+1} = C_i^n + \frac{S1_i + S2_i}{2} \cdot \Delta t\f}

mit

\f{equation}{S2_i = \frac{\frac{1}{2}(D_{L_{i-1}} + D_{L_i}) \cdot 
  \left(C_{i+1}^n - \left(1 + \frac{\Delta x_i}{\Delta x_{i-1}} \right) \cdot 
  C^{* n}_i + \frac{\Delta x_i}{\Delta x_{i-1}} \cdot C^{* n}_{i-1} \right)}{
  \frac{1}{2} \frac{\Delta x_i}{\Delta x_{i-1}} \cdot \left(1 + \frac{∆x_i}{
  \Delta x_{i-1}} \right) \cdot \Delta x_{i-1}^2} \f}
\n\n
  
## Dispersionskoeffizient

Für die Berechnung des Dispersionskoeffizienten stehen in QSim vier 
verschiedenen Gleichungen zur Verfügung (Deng et al., 2001, Li et al., 1998, 
Iwasa und Aya, 1991, Elder, 1959). 
Diese werden bereits vor dem Aufruf der Transportroutine in der Hauptroutine 
qsim.f90 berechnet und entsprechend übergeben. Alle Gleichungen benötigen die
 Schubspannungsgeschwindigkeit \f$u*\f$, die wie folgt berechnet wird:

\f{equation}{u_i^* = \frac{\frac{1}{k_{st}} \sqrt 9,81}{T_i^{\frac{1}{6}}} \nu_i \f}

| Parameter | Beschreibung | Einheit | 
| --------- | ------------ | ------- | 
| kst | Rauheitsbeiwert | [m1/3 s-1] | 
| T | Tiefe | [m]

Zudem wird für drei Gleichungen die Breite B der einzelnen Querschnitte 
benötigt, welche sich gemäß

\f{equation}{B_i = \frac{A_i}{T_i} \f}

berechnen lässt.

| Parameter | Beschreibung | Einheit | 
| --------- | ------------ | ------- | 
| A | Fläche | [m²] | 

Mit Hilfe dieser Variablen ergibt sich die Berechnung der 
Dispersionskoeffizienten wie folgt:

Deng et al., 2001 (\cite Deng_2001)
\f{equation}{D_{L_i} = \frac{0,15}{8\varepsilon_l} \cdot 
  \left(\frac{B_i}{T_i} \right)^{\frac{5}{3}} \cdot 
  \left(\frac{\nu_i}{u_i^*}\right)^2 \cdot T_i u_i^* \f}
  
mit 
\f{equation}{\varepsilon_{l_i} = 0,145 + \frac{1}{3520} \cdot 
  \left(\frac{B_i}{T_i} \right)^{1,38} \cdot \frac{\nu_i}{u_i^*} \f}

Li et al., 1998	(\cite Li_1998)	
\f{equation}{D_{L_i} = 0,2 \cdot \left(\frac{B_i}{T_i} \right)^{1,3} \cdot 
  \left(\frac{\nu_i}{u_i^*}\right)^{1,2} \cdot T_i \cdot u_i^*  \f}

Iwasa und Aya, 1991 (\cite Iwasa_Aya_1991)
\f{equation}{D_{L_i} = 2 \cdot (\left(\frac{B_i}{T_i} \right)^{1,5} \cdot T_i \cdot u_i^*  \f}

Elder, 1959 (\cite Elder_1959)		
\f{equation}{D_{L_i} = 5,93 \cdot T_i \cdot u_i^* \f}

| Parameter | Beschreibung | Einheit | 
| --------- | ------------ | ------- | 
| \f$\varepsilon_l\f$ | transversaler Mischungskoeffizient | [m² s-1] | 
  
Textquelle: stofftransport_1d.md ; Codesources: stofftransport.f95 ;  
zurück: \ref lnk_stofftransport
 
