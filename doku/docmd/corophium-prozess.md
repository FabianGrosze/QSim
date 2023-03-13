Chelicorophium - Prozesse {#lnk_corophium_prozesse}
==========================

_Letzte Änderung am 26.10.2022_

\warning Momentan ist das Modul ausgeschaltet, das heißt, Corophium wird 
nicht simuliert. Es handelt sich um eine alte Dokumentation.


# Entwicklung von Chelicorophium - Überblick

Die Entwicklung der Chelicorophiumpopulation als Startgeneration G1 am 01.Januar 
mit drei Vermehrungszyklen wird in QSim in Abhängigkeit von der Jahreszeit 
berücksichtigt. Um die unterschiedlichen Lebensdauern der einzelnen Generationen 
abbilden zu können, rechnet QSim im Jahresverlauf mit insgesamt 5 Generationen.

Wie Abb. 1 zeigt, spielen dabei die drei Zeitpunkte TagG1 (15.April), 
TagG2 (15.Juni) und TagG3 (15.August) eine maßgebliche Rolle. Sie werden 
analog zu dem unter Makrophyten beschriebenen Vorgehen aus gesetzten Tagen und 
Monaten als Tage des Jahres berechnet. Am Start wurden für Chelicorophium 
100 Individuen pro m² eingegeben. An TagG1 bildet diese Startgeneration G1 
Eier (=G2) und stirbt selbst ab, an TagG2 bildet die Generation G2 Eier (=G3), 
wobei ihr Gewicht reduziert wird. An TagG3 bilden schließlich die Generationen 
G2 (=G4) und G3 (=G5) Eier, G2 stirbt ab und G3 bleibt bestehen. Nach dem TagG1 
(15.4.) zeigen alle Generationen einen leichten Verlust mit der Zeit, ab dem 
15.8. steigt die Mortalitätsrate generationsspezifisch an, nur G5 hat weiterhin 
vergleichsweise geringe Verluste und kann so die Ausgangspopulation für das 
nächste Jahr bilden. Mit jeder Generation steigt die Abundanz von 
Chelicorophium zunächst ca. um einen Faktor 10 an.

![Entwicklung der 5 Generationen von Corophium in QSim im Jahresverlauf, Überblick](img/corophium_Generationen_Entwicklung.png)


## Populationsdichte
Vor jeder weiteren Berechnung wird die Ausgangsindividuendichte auf der Sohle 
und der Böschung bestimmt sowie die Anzahl der Individuen.

Zunächst werden die Chelicorophiumdichten auf Böschung (`coroi_s`) und Sohle 
(`corois_s`) ermittelt, indem die Summe über alle 5 Generationen gebildet wird.

```
coroi_s  = sum(coro_s)
corois_s = sum(coros_s)    
```

Daraus werden wiederum die in dem modellierten Gewässerabschnitt vorhandenen 
absoluten Chelicorophiumzahlen auf Böschung und Sohle (`corg` bzw. `corsg`) 
ermittelt, wobei die Länge des Gewässerabschnittes (`elen_s`) und die Böschungs- 
und Sohllängen (`lboem_s`, `bsohlm_s`) verwendet werden.

```
corg  = coroi_s  * lboem_s  * elen_s * 2.
corsg = corois_s * bsohlm_s * elen_s
```


## Filtration der Algen
Das von Chelicorophium filtrierte Wasservolumen [m³/s] (`volfm3`) ergibt sich 
schließlich durch die folgend Formel:

```
volfm3  = filtration_rate * (corg + corsg)* tflie / 1000.
```

mit 

  * `filtration_rate`  = Filtrierrate in Liter pro Individuum und Tag 
  * `tflie` = Zeitschritt (d)

Der Anteil des Wassers, der von Chelicorophium innerhalb des
Zeitschritts filtriert wird (`hconf`), ergibt sich durch

```
hconf = min(1., volfm3 / flae_s * elen_s) 
```
mit
  * `flae_s` = Querschnittsfläche des Abschnitts
  * `elen_s` = Elementlänge

Die Variable `hconf` kann dabei maximal einen Wert von 1.0 annehmen.

Die von Corophium aufgenommenen Algen-Biomassen (`algcok`, `algcog`, `algcob`) 
errechnen sich folglich

```
algcok_s = aki_s * hconf
algcog_s = agr_s * hconf
algcob_s = abl_s * hconf
``` 

# Generation 2

Falls das akutelle Simulationsdatum (`date_qsim`) kleiner als TagG1 (`date_g1`) ist,
erfolgt keine weitere Berechnung.

Falls  das akutelle Simulationsdatum gleich TagG1 (Saisonstart) ist, wird  mit der 
Berechnung der Nachkommen der Jahresgründungspopulation begonnen. 


Die Anzahl an Eiern (`n_eggs`) wird aus der Chelicorophiumdichte der ersten 
Generation (`coro_s(1)`) sowie dem Anteil weiblicher Individuen in G1 und G2 
ab 15.8. (`alpha_female1`) abgeleitet und ergibt multipliziert mit der Schlupfrate 
(`alpha_hatch`) die Dichte der zweiten Generation (`coro_s(2)`), gleichzeitig 
wird die Dichte der ersten Generation auf Null gesetzt (sie stirbt ab). Die
Berechnung werden sowohl für die Gewässersohle als auch für die Böschung berechnet.

```
! generation 2 hatches
n_eggs  = (-13.4 + 6.95 * lw1) * coro_s(1)  * alpha_female1
n_eggss = (-13.4 + 6.95 * lw1) * coros_s(1) * alpha_female1
 
coro_s(2)  = n_eggs  * alpha_hatching
coros_s(2) = n_eggss * alpha_hatching

! generation 1 dies
coro_s(1)  = 0.0
coros_s(1) = 0.0
```

Bei jedem weiteren Berechnungsdurchlauf für einen Tag des Jahres zwischen 
TagG1 (`date_g1`) (einschließlich) und TagG2 (`date_g2`) nimmt die Chelicorophiumdichte 
mit jeder Stunde leicht ab.

``` 
! generation 2 decreases
coro_s(2)  = coro_s(2)  * exp(-mortality_rate0 * tflie)
coros_s(2) = coros_s(2) * exp(-mortality_rate0 * tflie)
```

mit:
  * `mortality_rate0` = Mortalitätsrate vor 15. August [1/d]
  * `tflie` = Zeitschritt [d]

# Generation 3

Falls der aktuell modellierte Tag gleich TagG2ist , wird mit der Berechnung
der Nachkommen der G2-Population begonnen. Die Anzahl an Eiern  wird aus der 
Chelicorophiumdichte der zweiten Generation (`coro_s(2)`) abgeleitet und 
ergibt mal der Schlupfrate (`alpha_hatching`) die Dichte der dritten Generation 
(`coro_s(3)`), gleichzeitig wird die Dichte der zweiten Generation um den 
Faktor `0,3` vermindert (ein Teil stirbt ab). 

```
! generation 3 hatches
n_eggs  = (-13.4+6.95 * lw2) * coro_s(2)  * alpha_female2
n_eggss = (-13.4+6.95 * lw2) * coros_s(2) * alpha_female2

coro_s(3)  = n_eggs  * alpha_hatching
coros_s(3) = n_eggss * alpha_hatching

! generation 2 decreases
coro_s(2)  = coro_s(2)  * 0.3
coros_s(2) = coros_s(2) * 0.3
```

Bei jedem weiteren Berechnungsdurchlauf für einen Tag  zwischen 
TagG2 (`date_g2`) (einschließlich) und TagG3 (`date_g3`) nimmt die Chelicorophiumdichte 
der Generationen 2 und 3 mit jeder Stunde leicht ab.

```
! generation 2 decreases
coro_s(2)  = coro_s(2)  * exp(-mortality_rate0 * tflie)
coros_s(2) = coros_s(2) * exp(-mortality_rate0 * tflie)

! generation 3 decreases
coro_s(3)  = coro_s(3)  * exp(-mortality_rate0 * tflie)
coros_s(3) = coros_s(3) * exp(-mortality_rate0 * tflie)
```


# Generationen 4 und 5

Falls der aktuell modellierte Tag gleich TagG3, wird mit der Berechnung der 
Nachkommen der G2- und G3-Population begonnen. Die Anzahl an Eiern wird aus der 
Chelicorophiumdichte der zweiten und dritten Generationen (`coro_s(2)`) 
abgeleitet und ergibt mal einer Schlupfrate (`alpha_hatching`) die Dichte der 4.
und 5. Generation (`coro_s(4)`, `coro_s(5)`), gleichzeitig wird die 
Dichte der zweiten Generation nullgesetzt. 

```
! generation 4 hatches
n_eggs  = (-13.4+6.95 * lw1) * coro_s(2)  * alpha_female1
n_eggss = (-13.4+6.95 * lw1) * coros_s(2) * alpha_female1

coro_s(4)  = n_eggs  * alpha_hatching
coros_s(4) = n_eggss * alpha_hatching

! generation 5 hatches
n_eggs  = (-13.4+6.95 * lw2) * coro_s(3)  * alpha_female2
n_eggss = (-13.4+6.95 * lw2) * coros_s(3) * alpha_female2

coro_s(5)  = n_eggs  * alpha_hatching
coros_s(5) = n_eggss * alpha_hatching

! generation 2 dies
coro_s(2)  = 0.0
coros_s(2) = 0.0
```


Bei jedem weiteren Berechnungsdurchlauf für einen Tag des Jahres größer gleich 
TagG3 nimmt die Chelicorophiumdichte der Generationen 3, 4 und 5 entsprechend
einer eigenen Mortalitätsrate ab:

```
! generation 3 dies off
coro_s(3)  = coro_s(3)  * exp(-mortality_rate3 * tflie)
coros_s(3) = coros_s(3) * exp(-mortality_rate3 * tflie)
if (coro_s(3)  < 1.) coro_s(3)  = 0.0
if (coros_s(3) < 1.) coros_s(3) = 0.0

! generation 4 dies off
coro_s(4)  = coro_s(4)  * exp(-mortality_rate4 * tflie)
coros_s(4) = coros_s(4) * exp(-mortality_rate4 * tflie)
if (coro_s(4)  < 1.) coro_s(4)  = 0.0
if (coros_s(4) < 1.) coros_s(4) = 0.0

! generation 5 decreases
coro_s(5)  = coro_s(5)  * exp(-mortality_rate5 * tflie)
coros_s(5) = coros_s(5) * exp(-mortality_rate5 * tflie)
```
 
![Jahresgang von Chelicorophium mit angenommener Startdichte von 100 (nur Böschung)   (A = Generationen 1 bis 5 / B = Gesamtpopulation)](img/corophium_Jahresgang.png)


Das Wachstum von Chelicorophium erfolgt nur in Abhänigkeit der Ausgangskonzentration,
es werden keine Einflussfaktoren (Temperatur, Futterverfügbarkeit etc.) verwendet. Die 
Verlustraten sind ebenfalls unbeeinflusst von irgendwelchen Faktoren.




Textquelle: corophium-prozess.md; Codesource: coroph.f90; zurück: \ref lnk_corophium