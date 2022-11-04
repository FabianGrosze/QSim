Benthische Algen - Prozesse {#lnk_albenth_prozesse}
===================== 

\warning Momentan ist das Modul abgeschaltet. Vor einer neuen Anwendung, sollte 
es dringend überarbeitet werden!

\note Die folgende Darstellung BESCHREIBT derzeit nur das, was das derzeitige 
Modul rechnet, die Sinnhaftigkeit ist an vielen Stellen unklar bzw. muss 
dringend fachlich diskutiert werden. 

\note Offene Fragen ergeben sich v.a. bezüglich: Licht (2 Faktoren!), 
Stoffbilanzen, Berücksichtigung der Wuchsfläche, Abbildung von Wachstum und 
Respiration.
Wegen der bisher weitestgehend fehlenden Dokumentation wurden sehr viele 
Bezeichnungen aus dem Kontext „erraten“ bzw. es wurden Kommentare im Code 
genutzt. Dies kann zu unzusammenhängenden und teilweise falschen 
Interpretationen/logischen Brüchen geführt haben und sollte beizeiten überprüft
werden.

\note Extinktion und Wassertiefe werden derzeit nur scheinbar berücksichtigt 
(kürzen sich heraus), in Überarbeitung!

\note Die Fläche, die durch benthische Algen besiedelt wird, sollte eigentlich 
der Sedimentoberfläche des Abschnitts (Länge in Querprofil * Abschnitt) 
entsprechen. Ist nicht so: elen (Abschnittslänge) wird zwar übergeben, aber 
nicht genutzt, und Sohl- und Böschungslängen werden nicht übergeben. Flae wird 
übergeben aber ebenfalls nicht genutzt, unklar ob hier die Oberfläche des 
Gewässers oder die Sedimentoberfläche gemeint ist. Die Quantifizierung der 
benth. Algen erfolgt lediglich über Umrechnung mit mittlerer Wassertiefe.


Die einzelnen Teilprozesse, die im Benthische Algen Baustein berücksichtigt 
werden sind:
* ...
* ...


# Grundgleichung und Übersicht

Die aktuelle Wachstumsrate der benthischen Algen P errechnet sich aus der 
maximalen Wachstumsrate bei 20°C Pmaxi (aus Parameterliste für Planktonalgen 
der jeweiligen Gruppe, Grün aggmax bzw. Kiesel agkmax) mal einem Faktor für 
Temperatur f(T) (ftag, ftak), für Licht f(L) (fig, fik), Nährstoffe 
f(N) (fng, fnk) und Strömung f(S) (vlim).

\f[ P_i^C = P_{max, i}^{C} \cdot f(T)_i \cdot f(L)_i \cdot f(N)_i \cdot f(S) \f]

\f$ P_i^C \f$:  benthische Algen der Gruppe *i* [mg Bio/L] \n
...

<!-- aggrow 	aggmax*fng*fig*vlim*(ftag)	Grünalgenwachstumsrate
akgrow 	akgmax*fnk*fik*vlim*(ftak)	Kieselalgenwachstumsrate nicht bei Resp -->

Variante:

\f[ P_o = P_{max, i} \cdot f(L)_i \cdot f(N)_i \cdot f(S) \f]

ohne Temperaturfaktor für Respirationsberechnung (verwendet abweichenden 
Temperaturfaktor, s.u.).

Bei der Berechnung der Biomasse der benthischen Algen unter Verwendung der 
aktuellen Wachstumsrate fließt ein weiterer Lichtfaktor (Flicht) ein:

\f[ Abenth_t = Abenth_0 \cdot Flicht \cdot e^{P \cdot dT} \f]

\f[ Zuwachsbent = Abenth_t - Abenth_0 \f]

\f$ dT \f$:   Zeitschrittweite [-] \n
\f$ Abenth_0 \f$: Biomasse vor Zeitschritt [mg Bio/L] \n
\f$ Abenth_t \f$: Biomasse nach Zeitschritt [mg Bio/L] \n

\note Unklar, wo der Faktor Flicht herkommt und was er abbilden soll -> zu 
prüfen

<!-- abegrt 	abegr*flicht*exp(aggrow*tflie) 	Grünalgen nach Zeitschritt (mit Lichtfaktor)
abekit 	abeki*flicht*exp(akgrow*tflie) 	Kieselalgen nach Zeitschritt (mit Lichtfaktor)
albewg(ior) 	abegrt-abegr	Zuwachs Grünalgen (ohne Resp)
albewk(ior) 	abekit-abeki	Zuwachs Kieselalgen (ohne Resp) -->


\f[ Abenth_{Zbrutto} = Abenth_t - Abenth_0 \f]

Der brutto-Zuwachs an benthischen Algen Abenth_{zbrutto} (albewg) wird aus der 
Differenz der von Abenth_t und Abent_0 berechnet.

\note Anders als im Algenmodul werden NICHT die Raten von Wachstum und 
Respiration verrechnet, um anschließend die gebildete Biomasse zu berechnen. 
Vielmehr werden die jeweiligen Raten getrennt eingesetzt, um auf die
aktuelle(Abentht) bzw. die respirierte (Abentht) Biomassen zu kommen. In diesem 
Schritt wird bei der Respiration zusätzlich ein Temperaturfaktor, beim Wachstum 
ein Lichtfaktor verwendet. Die Ergebnisse werden schließlich verrechnet. 
Es ergibt sich der absolute Netto-Zuwachs benthischer Algen Abenth_Z 
(abegm2, abekm2, werden übergeben aber nirgends weiterverwendet oder 
ausgegeben):

\f[ Abenth_Z = Abenth_t- Abenth_R - Abenth_0 \f]

Die errechneten Brutto-Zuwachs- bzw. Respritions-Biomassen der beiden 
benthischen Algengruppen werden schließlich über die mittlere Wassertiefe in 
mg/l umgerechnet und übergeben (albewg, albewk, alberg, alberk). Auf sie wird 
an anderen Stellen im Modell zurückgegriffen, um den Einfluss der benthischen 
Algen auf den Sauerstoff- und Nährstoffhaushalt zu berücksichtigen.

## Faktoren des Wachstums

### Temperaturabhängigkeit

Sowohl das Wachstum selbst als auch das Chla:C Verhältnis der benthischen Algen 
werden temperaturabhängig abgebildet.

Temperaturabhängigkeit des Chla:C-Verhältnisses (*aktuell auskommentiert*)

Die Biomasse der Algenklasse wird standardmäßig aus dem abschnittsweise 
eingegebenen Anteil am Gesamt-Chlorophyll-a-Gehalt wie folgt berechnet:
 
\f[ A_i = \frac{Chla_i \cdot Chla(T)_i}{1000 \cdot C_i}  \f]

mit 

\f[ Chla_i = \alpha_{A,i} \cdot Chla \f]

\f$ (C:Chla(T)_{d,i} \f$: Kohlenstoff/Chlorophyll-a-Verhältnis der Algen der 
                          Klasse *i* bei der Temperatur *T* [\f$ mgC \cdot 
						  mgChla^{-1} \f$] *akch, agchl = planktische Algen [-]* \n
\f$ Chla_i \f$:  Chlorophyll-a-Konzentration der Algenklasse *i* [µg l^{-1}] \n
\f$ \alpha_{A,i} \f$:	Anteil der Algenklasse *i* an Chl a
\f$ C_i \f$		C-Biomasse, gesetzt Code 0,48 für beide AG \n

Die Temperaturabhängigkeit des C:Chla wird dabei von der Gleichung her analog 
zu den planktischen Algen formuliert, die Parametrisierung unterscheidet sich 
jedoch:

\f[ (C:Chla)_T = C:Chla_0 \cdot e^{-b_i \cdot T}  \f]

Der Wert von *b_i* ist derzeit für beide benthischen Algengruppen im Code auf 
0,0465 gesetzt, *C:Chl_0* auf 0,398. Bei 20 °C entspricht Faktor 1 (Angabe des 
C:Chla für 20 °C aus Parameterliste), bei 5 °C 0,5 und bei 35 °C 2.

![Plot des Temperatur-Faktors des C:Chl benthischer Algen, x: Wassertemperatur (°C), y: Faktor FtChlC.](img/abenth_Temperaturfaktor.png)

<!-- #mf: plot braucht noch Achsenbeschriftung -->



### Temperaturabhängigkeit der Wachstumsrate 

In QSim wird die Temperaturabhängigkeit der Wachstumsrate der benthischen 
Algengruppen analog zu den entsprechenden Faktoren der Phytoplanktongruppen nach 
der folgenden Formel berechnet. 

\f[ F_{mue}(T) = \left( \frac{T_{max} - T}{T_{max} - T_{opt}} \right)^z \cdot
    e^{\left( z \cdot \left( 1 - \frac{T_{max} - T}{T_{max} - T_{opt}} \right) 
	\right)} \f]

aber: \f$ T > T_{max} \f$: F(T) = 0,01

Wobei
\f[ z = \frac{W^2}{400} \cdot \left( 1 + \left( 1 + \frac{40}{W} \right)^{0,5} 
   \right)^2 \f]
  und  

\f[ W = (T_{max} - T_{opt}) \cdot ln(Q10) \f]
  
Mit

\f$ Q10 \f$:  Van’t Hoffscher Temperaturkoeffizient, ln(Q10)=0,61519 gesetzt, 
    entspricht Q10 von 1,85 \n
\f$ T \f$: Wassertemperatur (Tempw(ior)) \n
\f$ T_{max} \f$: Letaltemperatur, s.o. (TMAX, für Grünalgen 45, Kieselalgen 31 
    im Code gesetzt) \n
\f$ T_{opt} \f$: optimale Wachstumstemperatur, s.o. (TOPT, für Grünalgen 27, 
    Kieselalgen 20 im Code gesetzt)

Dabei werden die in der Tabelle unten aufgeführten Q10, Temperaturoptima und –maxima verwendet. F(T) (ftag: Grünalgen, ftak: Kieselalgen) wird schließlich mit der Wachstumsrate bei Optimaltemperatur multipliziert, siehe Grundgleichungen.

![Da alle Parameter derzeit fest codiert sind, zeigt die Abbildung die 
Temperaturfaktoren für benthische Kiesel- (FTAk) und Grünalgen (FTAg) so, 
wie derzeit in OSim verwendet.](img/abenth_Temperaturfaktor_ii.png)
 
\note Die Temperaturabhängigkeit wird hier anders als für Planktonalgen 
abgebildet, obwohl bezüglich Licht- und Nährstoffabhängigkeit 
Parameterlistenwerte für diese Gruppen auch bei den benthischen Algen verwendet 
werden. 

\n\n

### Lichtabhängigkeit

**Berechnung des vertikalen Lichtklimas**

Das Licht am Gewässerboden *I_B* (algipl) wird unter Berücksichtigung der 
Wassertiefe, dem Extinktionskoeffizienten und der Lichtintensität an der 
Oberfläche *I_{Z0}* berechnet.
 
\f[ I_B = I_{z0} \cdot e^{-\varepsilon \cdot \Delta z}  \f] 

Als Grenzlichtintensität \f$ I_G \f$ (Igrenz) wird 1% der Lichtintensität am 
Gewässerboden \f$ I_B \f$ angenommen. 

\f[ I_G 0 I_B \cdot 0,01 \f]

<!-- #AB: Warum??? Ganz unabhängig von der Wassertiefe scheint das doch eher 
gewagt (100% könnte ja schon sehr geringer Lichtintensität entsprechen). --> 
 
Schließlich wird aus *I_B* und *I_G* die Schichtdicke der produktiven Schicht 
in der Wassersäule  KxI (KxI) abgeleitet:
<!-- AB: ? Zitat, zu prüfen -->

\f[ KxI = ln(I_B) - ln(I_G) = ln(\frac{I_B}{I_G}) = 
   ln(\frac{I_B}{I_B \cdot 0,01}) = ln(100) = 4,605 \f]

\note Die Vereinfachung der Gleichung zeigt, dass sich derzeit sowohl 
Wassertiefe als auch Extinktionskoeffizient herauskürzen, eine Überarbeitung 
des Ansatzes ist erforderlich.

<!-- AB: Ähnliches mit dem Herauskürzen findet bei den Makrophyten statt! -->

_Abhängigkeit der Wachstumsrate (F(L)) und der Biomassebildung (FLicht) vom Licht_

Es werden als Sättigungslichtstärke Imax für Kieselalgen 147 (saetbk) und für 
Grünalgen 176 (saetbg) µE/m²/s gesetzt. 

Die Lichtfaktoren F(L) (fik, fig für Kiesel- und Grünalgen) der Wachstumsrate 
werden nach folgenden Gleichungen berechnet:

\f[ F(L)i = (IB2/Imax) \cdot e^(1.-(IB2/Imax)) (aber: max. 1) \f]

Lichtfaktor benth. Algen, Imax gruppenspez.

Mit:

\f[ I_{B 2} = I_B \cdot e^{-Kx} \f]

Falls abegr + abeki >= 306 (zPeri > zPeriI): 

    Kx = 0.015 * (abegr + abeki)/2 = 0,007 * Abenth
    \f[ IB2 = algip1 = algip1 \cdot e^{-0,0075 \cdot (abeg + abeki)} = 
	   IB \cdot e^(-0.007 \cdot Abenth) \f]
    Flicht = 1
	
Sonst 

    Kx = 0.015 * roPeri * zPeriI/2 = 0.015 * roPeri * KxI/(0.015 * roPeri) = KxI/2 = 2.3 

\f[ IB2 = algip1 = algip1 \cdot e^{-2,3} = algip1 \cdot 0,1 = IB \cdot 0,1 \f]

    Flicht = zPeriI/zPeri= KxI/(0.015*roPeri)/( (abegr+abeki)/roPeri)= =4.6/0.015/(abegr+abeki)=306.7/(abegr+abeki)=306,7/Abenth

IB2   Licht in Periphyton???

zPeriI	Dicke Licht in Periph.?

zPeri	Dicke Periphyton?

roPeri 	Dichte des Periphytons [g/m³], gesetzt 1030000

Abenth: Biomasse benthische Algen (Grün+Kiesel, abegr, abeki g/m²)

![*hat noch keine Bildunterschrift*](img/abenth_fig_fik_IB2.png)

![*hat noch keine Bildunterschrift*](img/abenth_FLicht.png) 
 
\note Herkunft des Lichtfaktor und Wirkung sehr unklar/nicht plausibel

\n\n

### Nährstoffabhängigkeit der Wachstumsrate
Die Berechnung des Nährstofffaktors F(N) (FNK) für benthische Algen ist 
vergleichbar der Modellierung des Nährstoffaktors beim Phytoplanktonwachstums 
ohne Speicher (Michaelis-Menten-Kinetik):

\f[ F(NS) = \frac{NS}{NS + k_{s, NS}} \f]
 
mit

\f$ NS \f$ : N und P (Grün) Si, N und P (Kiesel) \n
\f$ k_{s, NS} \f$:	Ks-Wert des jeweiligen NS der Algengruppe 
         (aus Parameterliste plankt. Algen)

Aus den Faktoren für die einzelnen Nährstoffe der Algengruppen wird jeweils das 
Minimum gebildet und als Gesamt-Nährstofffaktor F(N) verwendet:

\f[ F(N) = min(F(NS)) \f]

Es werden hier die Ks-Werte, die für planktische Grün- und Kieselalgen in der 
Parameterliste eingegeben wurden, verwendet.

\n
**Strömungsabhängigkeit der Wachstumsrate**

Die Berechnung des Faktors Strömung wurde angelehnt an das Modell AQUATOX 
(laut Volker Kirchesch).

\f[ V_lim = Koeffizient1 + \frac{(Koeffizient2 \cdot V_{mitt,cm^{-1}}}
     {1 + Koeffizient2 \cdot V_{mitt,cm^{-1}}} 
	 = 0,2 + \frac{0,057 \cdot V}{1 + 0,057 \cdot V} \f]

Wobei Werte > 1 gleich 1 gesetzt werden. Dies ist bei der verwendeten 
Parametrisierung ab einer Fließgeschwindigkeit von 0,7 m/s der Fall.

| ------ | ----	| ------------ |
| vkoff1 | 0.2	| Koeffizient1 |
| vkoff2 | 0.057 | Koeffizient2 |
| veloci | vmitt(ior)*100 | Fließgeschwindigkeit in cm/s |
| vlim   | vkoff1 + ((vkoff2 * veloci)/(1. + (vkoff2 * veloci))) (aber: max 1) | Vlim: Faktor für Limitation durch Fließgeschwindigkeit |
 
![*hat noch keine Bildunterschrift*](img/abenth_Fliessgeschwindigkeit.png) 
 
Unterschreitet die mittlere Fließgeschwindigkeit 0.7 m/s, so sinkt der Faktor 
F(S) zunächst langsam und schließlich steil, bis er das Minimum von 0.2 erreicht.

\n\n

## Wachstum (siehe Grundgleichungen )

Die aktuelle Wachstumsrate der benthischen Algen P errechnet sich aus der 
maximalen Wachstumsrate bei 20 °C Pmaxi (aus Parameterliste für Planktonalgen 
der jeweiligen Gruppe, Grün aggmax bzw. Kiesel agkmax) mal einem Faktor für 
Temperatur f(T) (ftag, ftak), für Licht f(L) (fig, fik), 
Nährstoffe f(N) (fng, fnk) und Strömung f(S) (vlim).

\f[ P_i^C = P_{max, i}^{C} \cdot f(T)_i \cdot f(L)_i \cdot f(N)_i \cdot f(S) \f]

Die Biomassen der benthischen Algen Abentht ergeben sich nach:

\f[ A_{bentht} = Abenth0 \cdot Flicht \cdot e^{P \cdot dT} \f]

\f$ dT \f$:      Zeitschrittweite \n
\f$ Abent0 \f$:  Biomasse vor Zeitschritt [g/m²] \n
\f$ Abentht \f$: Biomasse nach Zeitschritt (abegrt, abekit) \n

   
## Respiration

Die Berechnung der Respirationsrate der benthischen Algen mit Grund und 
Licht-abhängiger Komponente erfolgt zunächst analog der des Phytoplanktons, 
nur die Temperaturabhängigkeit fließt abweichend ein.

\f[ Resp_{benth} = Resp_{Grund} + Resp_{Akt} \cdot Po = 0.085 + 0.2 \cdot P_o \f]

Mit

\f$ P_o \f$: aktuelle Wachstumsrate, wobei der hier Temperaturfaktor weggelassen wurde \n
\f$ Resp_{Akt} \f$: Lichtabhängige Respirationsrate (frespL), gesetzt 0,2 \n
\f$ Resp_{Grund} \f$:	Grundrespiration (respG), gesetzt 0,085 \n

Erst bei der Berechnung des Respriationsverlusts wird ein Temperaturfaktor 
verwendet. Anders als beim Phytoplankton wird hier der gesamte 
Respirationsverlust (nicht nur die Grundrespirationsrate) mal folgendem 
temperaturabhängigen Term multipliziert:

\f[ Abenth_r = Abenth_0 \cdot (1 - e^{-Respbent \cdot dT}) \cdot FResp(T) \f]

mit 

\f[ F_{Resp}(T) = \frac{1}{1 + e^{b1 - b2 \cdot T}} = 
  \frac{1}{1 + e^{1,7 - 0,187 \cdot T}} \f]

\f$ Abenth0 \f$: Algenbiomasse [mg/m²] vor Zeitschritt (ohne akt. Wachstum) \n
\f$ Abentr \f$:	 resp. Algenbiomasse [mg/m²] nach Zeitschritt (alberg, alberk) \n
\f$ b1 \f$:      Koeffizient, gesetzt 1,7 \n
\f$ b2 \f$:      Koeffizient, gesetzt 0,0187 \n


![*hat noch keine Bildunterschrift*](img/abenth_FResp.png) 
 


# Listen/Tabelle

\note nicht aufgeräumt

**Faktoren**

*g: Grünalgen, k: Kieselalgen*

| Codename  | Faktor für |
|-----------|------------|
| Fig, fik* | Licht (auf Wachstumsrate) |
| Fng, fnk*	| Nährstoffe N und P (u. Si) |
| Vlim	    | Fließgeschwindigkeitsfaktor |
| Flicht	| Licht (auf Biomasse) |


| algip1 | obfli*exp(-extk(ior)*tiefe(ior))	 | Licht am Boden |
| Igrenz | algip1*0.01 	 | 1% Licht = Kompensationstiefe |
| KxI | log(algip1)-log(Igrenz)=ln(algip1)-ln(agip1*0.01)=ln(algip1/(algip1*0.01))=ln(100)=4.6 (Log Fortran = ln Excel)	 | Schichtdicke produktiver Schicht (Wassersäule) |
| Kx | s.u.	 | Beruecksichtigt, dass mit zunehmender Schichtdicke die unteren Schichten weniger Licht bekommen |
| zPeriI | KxI/(0.015*roPeri)=4.6/(0.015*1030000=15450)=0.000298 | (roPeri: Schichtdicke des Periphytons) |
| zPeri | (abegr+abeki)/roPeri=(abegr+abeki)/1030000 | Abegr+abeki: benth. Algen ges.,
Dicke des Periphytonrasens |


|  | Grünalgen | Kieselalgen	| Blaualgen |
|--|-----------|----------------|-----------|
| Energiekosten für die Zell-Synthese η [s-1] | 0,26  | 0,26 | 0,26  |
| min. Respirationsrate rj,0 [1/d]            | 0,085 | 0,08 | 0,085 |
| Brutto-Wachstumsrate                        | 2,65  | 2,73 | 1,35  |
| Q10 van t’Hoffscher Temperaturkoeffizient  | 1,73 (1,85 benth) | 1,85 | 1,85 |
| Letaltemperatur Tmax             | 47   | (45 benth.) | 31 (benth. auch) | 35 |
| Temperaturoptimum Topt           | 33,5 | (27 benth.) | 20 (benth. auch) | 26 |
| C:Bio Startwert Caki i [mgC/mgBio], Anteil C an Trockenbiomasse | 0,48 (benth. auch) | 0,48 (benth. auch) | 0,48 |
| max.Lichtquantenausbeute Qmax               | 0,5   | 0,9  | 1     |
| algenspezifische Absorption ac,j [m²/mg Chl a], Mittel über 400-700nm aus E_EXTNCT.DAT | 0,01452	 | 0,00944 | 0,01631 |
| max.Lichtquantenausbeute Qmax               | 0,5   | 0,9  | 1     |
| ? frespg                                    | 0,2   | 0,2  | 0,2   |


### Netto Wachstum
Die neue Dichte für die benthischen Algen ergibt sich, indem von der Dichte 
nach dem Zeitschritt (albegrt, abekit, nur Wachstum berücksichtigt) der 
Respirationsverlust (alberg, alberk) abgezogen wird.


| abegm2(ior) | abegrt-alberg(ior)-(cmatgr(ior)*tiefe(ior)) | Nettozuwachs Grünalgen (cmatgr: 0) |
| abekm2(ior) | abekit-alberk(ior)-(cmatki(ior)*tiefe(ior)) | Nettozuwachs Kieselalgen (cmatki: 0) |

**Umrechnung auf mg/l**

Schließlich wird der Zuwachs der benthischen Algen auf mg/l umgerechnet

| albewg(ior) | albewg(ior)/tiefe(ior) |
| albewk(ior) | albewk(ior)/tiefe(ior) |	
| alberg(ior) | alberg(ior)/tiefe(ior) |	
| alberk(ior) | alberk(ior)/tiefe(ior) |	



<!-- Zusätzliche Kommentare:
* Elen Abschnittslänge wird derzeitig nicht genutzt, aber steht in Aufruf, Sohl- oder Böschungslängen werden nicht genutzt
--> 
 
 

 
## QSim-Veröffentlichungen, die den ...-Baustein beschreiben und/oder anwenden: 
...
 

\n\n

Textquelle: albenth-prozess.md; Codesource: albenth.f90; zurück: \ref lnk_albenth