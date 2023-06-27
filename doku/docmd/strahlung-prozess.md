Strahlung - Prozesse {#lnk_strahlung_prozesse} 
=====================

Die einzelnen Strahlungskomponenten werden in QSim aus der Globalstrahlung 
berechnet. Diese wird aus Messdaten von repräsentativen Wetterstationen 
des Modellgebiets bezogen. Die Eingabe der Globalstrahlung kann in Form 
einer Tagesstrahlungssumme oder als Zeitreihe in Stundenwerten erfolgen. 
Liegt eine Tagesstrahlungssumme vor, so wird aus dieser im Modell für jeden
Berechnungszeitschritt ein Strahlungswert ermittelt:

\f{align}{ 
    I_{\Delta t} &= \frac{I_\max}{2} \left(1 + \cos\left(\frac{2 \pi \cdot \mathrm{time}}{t_\mathrm{hell}} \right) \right) \\
    I_\max &= 2 \cdot \frac{I_\mathrm{Glob}}{4.2 \cdot t_{\mathrm{hell}}} \\
    \mathrm{time} &= \frac{t_\mathrm{Uhr} - SA}{t_\mathrm{hell}} - \frac{1}{2} \\
    t_\mathrm{hell} &= SU - SA 
\f}


\f$I_{\Delta t}\f$: Globalstrahlung während des Berechnungszeitschritts 
\f$\Delta t\f$ [cal cm<sup>-2</sup> h<sup>-1</sup>]
<!-- nochmal prüfen ob cal oder Joule -->

\f$I_{max}\f$: maximale Globalstrahlung am Tag [cal cm<sup>-2</sup> h<sup>-1</sup>]
<!-- nochmal prüfen ob cal oder Joule -->

\f$I_{Glob}\f$: Tagessumme der Globalstrahlung \f$ [J cm^{-2}] \f$ oder Globalstrahlungsintensität \f$ [J cm^{-2} h^{-1}] \f$ 

\f$ t_{Uhr} \f$: Zeit [h]
<!-- nachprüfen -->

\f$ SA, SU \f$: Sonnenaufgang, Sonnenuntergang (wird aus der geographischen Breite und geographischen Länge im Modell berechnet)

<!-- \f$\sum_{i=0}^n i^2 = \frac{(n^2+n)(2n+1)}{6}\f$ -->
<!-- #mf: nicht klar wo die Zeile her kommt -> nachverfolgen -->

_Durch den Faktor 4.2 wird die Strahlung_ \f$I_{Glob}\f$ _von Joule in cal umgerechnet._

Die Dämpfung des Lichtklimas durch die an das Gewässer angrenzende Vegetation kann im 
Modell differenziert nach Vegetationstypen, abhängig vom jahreszeitlichen
Belaubungszustand, berücksichtigt werden. Somit können die Auswirkungen von
Renaturierungsmaßnahmen auf den Wärmehaushalt des Gewässers und auf die photochemischen
und photobiologischen Prozesse betrachtet werden. Ebenso kann die Abschattung durch 
die in Gewässernähe errichtete Bebauung berücksichtigt werden. 

Die Reflektion der Strahlung an der Wasseroberfläche wird abhängig von dem Sonnenstand 
zum jeweiligen Simulationszeitpunkt und abhängig von der Bewölkung berechnet. Im Modell 
wird die Lichtextinktion der in das Wasser eindringenden Strahlen mit Hilfe von 
Absorptionskoeffizienten für Schwebstoffe, Huminstoffe und Algen ermittelt. Wahlweise 
kann dies auch wellenlängenabhängig erfolgen.

Die photosynthetisch aktive Strahlung (Photosynthetically Active Radiation PAR, 
Wellenlängenbereich 400-700 nm) errechnet sich aus der Globalstrahlung zu:

\f{align}{
    \mathrm{PAR} = 8.2051 \cdot I_{\Delta t} 
\f}


<!-- #mf: prüfen, ob Delta richtig kompiliert ist -->
<!-- #mf: Code überprüfen, scheint nicht konsistent zu sein: 
in albenth.f)=: OBFLI =  5.846 * (schwi(ior)*4.2) (der Wert 8.2051 ist auskommentiert)
In mphyt.f90 steht noch OBFLI = 8.2051*(schwi(ior)*4.2)
In algaesbl.f90 (ki & gr) steht OBFLI = 5.846*(schwi(ior)*4.2) -->

\f$ \mathrm{PAR} \f$: photosynthetisch aktive Strahlung \f$ [\mu E \, m^{-2} \, s^{-1}] \f$

\f$ I_{\Delta t} \f$: Globalstrahlung während des Berechnungszeitschritts  \f$ \Delta t \; [J \cdot cm^{-2} \cdot h^{-1}] \f$
  
Für die Berechnung der UV-Strahlung (Wellenlängenbereich 290-380 nm) wird im Modell von 
einem festen Anteil zwischen UV- und Globalstrahlung ausgegangen:
\f{align}{ 
    I_\mathrm{UV} = 0.032 \cdot I_{\Delta t} 
\f}
 
\f$ I_\mathrm{UV} \f$: UV-Strahlung [\f$ J cm^{-2} h^{-1} \f$] 

<!-- #mf: WLn = Schwi(ior)*(1.-0.032)*(1.-APARS)+G  ! 0.032: Anteil UVA/UVB  -->
<!-- #tbw: Im Code wird noch mit 4,2 multipliziert? Coliform.f90 -->

Der Einfluss von Strahlung auf den Wärmehaushalt eines Gewässers, die sogenannte 
Wärmestromdichte, wird in den Einfluss aus Globalstrahlung, aus atmosphärischer Strahlung 
sowie aus der Ausstrahlung der Wasseroberfläche unterteilt. Alle Anteile der 
Wärmestromdichte haben positive Werte: 
<!-- #tbw: den Wärmehaushalt-Absatz/Abschnitt in Kapitel zu Wassertemp. schieben? -->

\f{align}{ 
    q_S = q_{S, G} + q_{S, A} - q_{S, W} 
\f}

\f$ q_S \f$:      Wärmestromdichte aus Strahlung \f$ [kJ \cdot h^{-1} \cdot m^{-2} ]\f$

\f$ q_{S, G} \f$: Wärmestromdichte aus Globalstrahlung \f$ [kJ \cdot h^{-1} \cdot m^{-2})] \f$

\f$ q_{S, A} \f$: Wärmestromdichte aus atmosphärischer Gegenstrahlung \f$ [kJ \cdot h^{-1} \cdot m^{-2})] \f$

\f$ q_{S, W} \f$: Wärmestromdichte aus Ausstrahlung der Wasseroberfläche  \f$ [kJ \cdot h^{-1} \cdot m^{-2})] \f$

<!-- #mf: folgender Absatz aus alter Temp-Doku von Volkerkopiert (noch nicht gecheckt) -->
Obwohl es durchaus möglich ist, die Gesamtstrahlungsbilanz durch Messungen zu ermitteln, 
liefern die angewendeten Messmethoden oftmals keine hinreichend genauen Ergebnisse, so 
dass es sich im Allgemeinen nicht vermeiden lässt, einen Teil der gesuchten Werte durch 
empirische Betrachtung oder Berechnung der einzelnen Komponenten zu erhalten.
   

## Wärmestromdichte aus Globalstrahlung ##
Die Wärmestromdichte aus der Globalstrahlung \f$ q_{S,G}\f$ ist die Summe aus direkter
Sonnenstrahlung und diffuser Himmelsstrahlung. Allerdings wird nur ein Teil dieser 
Strahlung wirklich in Wärme umgewandelt. Der Rest, im Allgemeinen ca. 15 %, wird an der 
Wasseroberfläche reflektiert. Die Globalstrahlung ist abhängig von den Faktoren 
-	Stand der Sonne
-	Grad der Bewölkung
-	Wasserdampfgehalt der Luft
-	Verteilung der Temperatur
-	Horizontabschirmung.
   
Aufgrund der vielfältigen, schwer quantifizierbaren Abhängigkeiten werden in QSim gemessene
Globalstrahlungswerte für die weitere Berechnung verwendet. 

## Atmosphärische Gegenstrahlung ##
Als atmosphärische Gegenstrahlung \f$q_{S,A}\f$ bezeichnet man die Strahlung, die aus 
der Reflexion an Wasserdampf oder Kohlensäureteilchen in großen Höhen resultiert. 
Sie ist daher im Wesentlichen abhängig von folgenden Größen:
-	Wasserdampfgehalt der Luft.
-	Stand der Sonne
-	Verteilung der Temperatur

Da in der Regel keine höhendifferenzierten Messungen über den Wasserdampfgehalt und die 
Temperatur der Umgebung vorliegen, wird in QSim die atmosphärische Gegenstrahlung mit Hilfe
empirischer Betrachtungen ermittelt, von denen nach einer Untersuchung von Kasten (1989) 
die Formel von Swinbank für den wolkenlosen Himmel die besten Ergebnisse liefert:

\f{equation}{ q_{S,A,0} = 9,3 \cdot 10^{-6} \cdot \sigma \cdot (T_{L,tr} + 273,16)^6 \f}
<!-- #tbw: Die Einheiten von °C bzw. K passen nicht. Muss die Klammer nur E04 sein und nicht E06?
Muss hinter 273,16 noch die Einheit °C? -->


\f$ q_{S,A,0} \f$: Wärmestromdichte aus atmosphärischer Gegenstrahlung bei wolkenlosem Himmel 
 [\f$ kJ h^{-1} m^{-2}\f$]
 

\f$ T_{L,tr} \f$: Lufttemperatur, gemessen am trockenen Thermometer [°C]


Bei bewölktem Himmel erhöht sich die atmosphärische Gegenstrahlung durch die Reflexion 
an den Wassertropfen und Eiskristallen der Wolkenunterseiten. Diese Erhöhung ist abhängig 
vom Bedeckungsgrad und der Höhe der Wolkenunterkante über der Erdoberfläche. Die Höhe der
Wolkenunterkante kann mit dem Wolkentyp in Beziehung gesetzt werden. Der Einfluss der 
Bewölkung wird über den Koeffizienten \f$ kB,A \f$ berücksichtigt:

\f{align}{
    q_{S,A} = k_{B,A} \cdot q_{S,A,0} 
\f}


\f$ q_{S, A} \f$:   Wärmestromdichte aus atmosphärischer Gegenstrahlung \f$ [kJ \cdot h^{-1} \cdot m^{-2})] \f$

\f$ k_{B,A} \f$:	Koeffizient zur Berücksichtigung der Bewölkung [-]

\f$ q_{S,A,0} \f$:	Wärmestromdichte aus atmosphärischer Gegenstrahlung bei wolkenlosem Himmel \f$ [kJ/(h*m²) ] \f$

Der Bewölkungskoeffizient \f$ k_{B,A} \f$ berechnet sich in Abhängigkeit vom 
Bedeckungsgrad \f$ \alpha_B \f$ und Wolkentyp \f$ f_{Typ} \f$ (siehe Tabelle XX).
<!-- Link zur Tabelle unten einbauen --> 
In QSim wird ein \f$ f_{Typ} \f$ von 0,08 (Cirrostratus) als Standard verwendet.

\f{align}{
    k_{B,A} = 1 + f_{Typ} \left(\frac{\alpha_B}{8}\right)^{2.6} 
\f}

\f$ k_{B,A} \f$:	Koeffizient zur Berücksichtigung der Bewölkung

\f$ f_{Typ} \f$:	Wolkentyp. Der Faktor bestimmt sich je nach Wolkentyp entsprechend Tabelle XX.  

\f$ \alpha_B \f$:	Gemessener Bedeckungsgrad in Achteln zwischen 0 (wolkenlos) und 8 (bedeckt)


Tabelle XX: Faktor zur Berechnung des Bewölkungseinflusses auf die atmosphärische 
Gegenstrahlung 
<!-- #tbw: Zitat? -->
| Nr. | Wolkentyp	 | 	fTyp | 	Nr. | Wolkentyp	    | fTyp |
| --- | ------------ | ----- | ---- | ------------- | ---- |
| 1	  | Cirrus       | 	0.04 | 	6	| Nimbosstratus | 0.25 |
| 2	  | Cirrocumulus | 	0.06 | 	7   | Stratocumulus | 0.25 |
| 3	  | Cirrostratus | 	0.08 | 	8   | Stratus	    | 0.25 |
| 4	  | Altocumulus  | 	0.20 | 	9   | Cumulus	    | 0.25 |
| 5	  | Altostratus	 | 	0.20 | 	10  | Cumulonimbus	| 0.25 |


## Ausstrahlung der Wasseroberfläche ##

Obwohl ein Gewässer den größten Teil der anfallenden Strahlung absorbiert, wird eine 
gewisse Menge an Strahlung wieder aus dem Wasserkörper abgegeben. Die Wärmestrahlung des 
Wassers wird durch das Stefan-Boltzmann-Gesetz für einen grauen Körper erfasst und hängt 
von der absoluten Temperatur der Wasseroberfläche und dem effektiven langwelligen 
Emissionsgrad \f$ \epsilon_W \f$ ab:

\f{align}{ 
    q_{S,W} = \varepsilon_{W} \cdot \sigma \cdot (T_W + 273.15)^4 
\f}

<!-- #tbw: Muss hinter 273.15 wieder die Einheit °C? Muss es nicht 1- \epsilon_W heißen, um die Ausstrahlung zu berechnen? -->

\f$ q_{S,W} \f$: Wärmestromdichte aus Ausstrahlung der Wasseroberfläche \f$ [kJ \cdot h^{-1} \cdot m^{-2}] \f$

\f$ \varepsilon_W	\f$:  effektiver langwelliger Emissionsgrad = 0,97 [-]

\f$ \sigma \f$: Stefan-Boltzmann-Konstante; 

\f$ \sigma = 2.0413 \cdot 10^{-7} [kJ m^{-2} K^{-4}] = 5.670367 \cdot 10^{-8} [W h m^{-2} K^{-4}] \f$

\f$T_W \f$: Wassertemperatur [°C]


## Berechnung der Strahlungsabsorption im vertikal aufgelösten Modell

Wird QSim nicht in der eindimensionalen Version, sondern mehrdimensional gerechnet,
so kann für einzelne Wassertiefen abgeschätzt werden, wieviel
Licht von der Oberfläche im Wasserkörper absorbiert wird. Das ist zum einen für die 
Berechnung des Wärmehaushalts wichtig, zum anderen lässt sich daraus berechnen, 
wieviel Licht die Primärproduzenten in den verschiedenen Gewässertiefen erhalten und 
für ihr Wachstum verwenden können.

Die langwellige Strahlung wurde in zwei Wellenlängenbereiche unterteilt, wobei 
der Extinktionskoeffizient für beide Bereiche als gleich angenommen wurde 
(4 \f$m^{-1} \f$). Des Weiteren wurde  die von der Sohle reflektierte Strahlung in 
jeder vertikalen Schicht berücksichtigt. Der Reflexionsanteil sollte laut Literatur 
20% betragen.
<!-- #mf: im Code schauen, ob das eine Empfehlung ist oder tatsächlich umgesetz wurde -->

### Absorption der Globalstrahlung und atmosphärische Gegenstrahlung ###
Bei der einfallenden Globalstrahlung wird zwischen drei Wellenlängenbereichen unterschieden:

Der Wellenlängenbereich <=700 nm (UV-und photosynthetisch aktive Strahlung):

\f{align}{
    GS_{pars, k} = GS \cdot (f_{UV} + f_{pars})	
\f}


und die Wellenlängenbereiche >700 <= 910 nm und >910 nm. Diese beiden
Wellenlängenbereiche gelten auch für die langwellige Gegenstrahlung.

\f{align}{ 
    SL1_k &= GS * fL_1 + G * fL_1/(fL_1 + fL_2) \\
    SL2_k &= GS * fL_2 + G * fL_2/(fL_1 + fL_2) 
\f}


\f$ GS: \f$		     Globalstrahlung an der Gewässeroberfläche [kJ*m-2*h-1]   \n
\f$ G: \f$		     langwellige atmosphärische Gegenstrahlung                 \n
\f$ fUV, fPARS: \f$	 Anteil der UV- und fotosynthetisch aktiven Strahlung an der Globalstrahlung [-]  \n
\f$ fL1: \f$		 Anteil der Strahlung der Wellenlänge 700-910 nm an der Globalstrahlung [-]  \n
\f$ fL2: \f$		 Anteil der Strahlung der Wellenlänge >910 nm an der Globalstrahlung [-]  \n
 
Die Strahlungsintensität in der Tiefe z errechnet sich für die drei Wellenlängenbereiche zu: 
\f{align}{ 
    GS_{pars, k+1} &= GS_{pars,k} \cdot \exp⁡(-\lambda \cdot z) \\
    SL1_{k+1} &= SL1_k \cdot \exp⁡(-\lambda_{L1} \cdot z)         \\
    GS_{pars, k+1} &= GS_{pars,k} \cdot \exp⁡(-\lambda{L_2} \cdot z)  für j = 1, n-1	
\f}


\f$ GS_{pars,k}, SL1_k, SL2_k: \f$	Strahlung der drei Wellenlängenbereiche am Anfang der jeweiligen vertikalen Schicht [kJ*m-2*h-1]

\f$ GS_{pars,k+1}, SL1_{k+1}, SL2_{k+1}: \f$	Strahlung am Ende der jeweiligen vertikalen Schicht \n
(Schichtdicke = z) [kJ*m-2*h-1]  \n
\f$ z: \f$				Dicke der jeweiligen vertikalen Schichten [m] \n
\f$ j: \f$				Laufvariable für die vertikalen Schichten \n
\f$ n: \f$				Anzahl der vertikalen Schichten \n
\f$ k: \f$				kennzeichnet Schichtanfang (1) und Schichtende (2)  \n
\f$ \lambda, \lambda_{L1}, \lambda_{L2}: \f$		Lichtextinktionskoeffizient für Licht der drei Wellenlängenbereiche [m-1] 


Die Wärmestromdichte q_{s,j} [\f$kJ m^{-2} h^{-1}\f$] durch die einfallende Strahlung
errechnet sich somit für alle Schichten aus:


\f{align}{ 
    q_{s,j} &= (GS_\mathrm{pars,k} - GS_\mathrm{pars,k+1}) + (SL1_k - SL1_{k+1}) + (SL2_k - SL2_{k+1}) \\
    GS_\mathrm{pars,k} &= GS_\mathrm{pars,k+1} \\
    SL1_k &= SL1_{k+1} \\
    SL2_k &= SL2_{k+1} 
\f}

<!-- #mf: Tabelle beziffern/ref erstellen -->
Tab.XX: Aufteilung der Strahlung und entsprechende Extinktionskoeffizienten
(Abbasi et al. 2017):

| Wellenlänge | Anteil \f$f_i\f$ an der gesamten Strahlung [-]   | Extinktionskoeffizient [\f$ m^{-1}\f$] |
| ----------- | ------------------------------------------------ | -------------------------------------- |
| <400 	      | 0.046                                            | wie pars                               |
| 400 - 700   | 0.45                                             |                                        |
|             |(eigene Messungen)                                | wird berechnet                         |
| 700 - 910	  | 0.21                                             | 2.92                                   |
|             |                                                  | (eigene Messungen ~4)                  |
| >910	      | 0.294                                            | 93                                     | 

### Absorption der von der Gewässersohle reflektierten Strahlung ###

Für die drei Wellenlängenbereiche errechnet sich die von der Gewässersohle reflektierte Strahlung aus:

\f{align}{
    GSr_{pars} &= GS \cdot \mathrm{PSREFS} \cdot (f_{UV} + f_{pars}) e^{-\lambda z} \\
    SL1r &= (GS \cdot fL_1 \cdot PSREFS + G \cdot \mathrm{PSREFS} \cdot fL_1/(fL_1 + fL_2 )) e^{-\lambda_{L1} z} \\
    SL2r &= (GS \cdot fL_2 \cdot PSREFS + G \cdot PSREFS \cdot fL_2/(fL_1 + fL_2 )) e^{⁡-\lambda_{L2} z} 
\f}

\f$ GSr_{pars} \f$:	    reflektierte Strahlungsmenge der Wellenlängen <=700 nm an der Gewässersohle [kJ*m-2*h-1]   \n
\f$ SL1r \f$:	        reflektierte Strahlungsmenge der Wellenlängen >700 <=910 nm an der Gewässersohle [kJ*m-2*h-1]   \n
\f$ SL1r \f$:	        reflektierte Strahlungsmenge der Wellenlängen >910 nm an der Gewässersohle [kJ*m-2*h-1]  \n
\f$ PSREFS\f$:	        Reflektionsanteil der auf die Gewässersohle auftreffenden Strahlung [-] \n
\f$ z \f$:              Gewässertiefe [m] \n  

Die Strahlungsmenge die von der reflektierten Strahlung die Gewässeroberfläche wieder erreicht ergibt sich aus: 

\f{align}{
    GSr_\mathrm{pars,k} &= GSr_\mathrm{pars} \cdot \exp⁡(-\lambda  z) \\
    SL1r_k &= SL1r \cdot \exp⁡(-\lambda_{L1} z) \\
    SL2r_k &= SL2r \cdot \exp⁡(-\lambda_{L2} z) \\
\f}	

Die Wärmestromdichte \f$q_{US,j} \f$ [\f$kJ m^{-2} h^{-1}\f$] aus der reflektierten 
Strahlung errechnet sich für die einzelnen Schichten aus:

\f{align}{ 
    q_{US,j} &= (GSr_{pars,k+1} - GSr_{pars,k}) + (SL1r_{k+1} - SL1r_k)+ (SL2r_{k+1} - SL2r_k) \\
    GSr_{pars,k+1} &= GSr_{pars,k} \cdot \exp⁡(-\lambda z) \\
    SL1r_{k+1} &= SL1r_k \cdot \exp⁡(-\lambda_{L1} z) \\
    SL2r_{k+1} &= SL2r_k \cdot \exp⁡(-\lambda_{L2} z) \\
    GSr_{pars,k} &= GSr_{pars, k+1} \\
    SL1r_k &= SL1r_{k+1} \\
    SL2r_k &= SL2r_{k+1} 
\f}



\f$ GSr_{pars,k}, SL1r_k, SL2r_k:\f$	        reflektierte Strahlung der drei Wellenlängenbereiche am Anfang der jeweiligen vertikalen Schicht [kJ*m-2*h-1]  \n
\f$ GSr_{pars,k+1}, SL1r_{k+1}, SL2r_{k+1}:\f$	Strahlung am Ende der jeweiligen vertikalen Schicht (Schichtdicke = z) [\f$kJ \cdot m^{-2} \cdot h^{-1}\f$]  \n
\f$ z: \f$                                      Dicke der jeweiligen vertikalen Schichten [m]  \n
\f$ j: \f$                                      Laufvariable für die vertikalen Schichten  \n
\f$ n: \f$                                      Anzahl der vertikalen Schichten  \n 
\f$ \lambda, \lambda_{L1}, \lambda_{L2}: \f$    Lichtextinktionskoeffizient für Licht der drei Wellenlängenbereiche [m-1] 


Textquelle: strahlung-prozess.md; Codesources: strahlg.f90, sasu.f95; zurück: \ref lnk_strahlung

<!-- #mf: letzten Teil zu mehr-D: schauen in welchem File das steht und Codesource ggf. korrigieren -->
<!-- #mf: schauen, ob noch irgendwo anders das Licht für die Algen berechnet wird und ob
es hier oder im Algenteil beschrieben wird/werden sollte -->