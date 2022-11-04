Makrophyten - Prozesse {#lnk_makrophyt_prozesse}
=====================

_Letzte Änderung am 26.10.2022_

\warning Momentan ist das Modul ausgeschaltet, das heißt, Makrophyten werden 
nicht simuliert. Es handelt sich um eine alte Dokumentation.

Im Makrophytenbaustein werden folgende Teilprozesse berücksichtigt:
- \ref lnk_makrophyt_wachstum
- ...

# Wachstum der Makrophyten {#lnk_makrophyt_wachstum}

## Idealer Wachstumsverlauf
Der idealisierte Wachstumsverlauf der Makrophyten wird nach folgender Formel 
abgebildet:

\f{equation}{ 
  dt = \left(\frac{t - t_S}{t_{max} - t_S} \right)^a \cdot e^{a \cdot 
    \frac{t_{max} - t}{t_{max} - t_S} \cdot 
	\left(\frac{t_E - t_{max}}{t_E - t}\right)^b}
\f}, 

Mit: \n
\f$dt\f$:      Faktor der relativen Makrophytenabundanz für Tag des Jahres [-] \n
\f$t\f$: 	   aktueller Tag des Jahres [-] \n
\f$t_S\f$: 	   Start der Makrophytensaison, abschnittsweise eingegeben 
               (umgerechnet, hier: nrstart) [-] \n
\f$t_E\f$: 	   Ende der Makrophytensaison, abschnittsweise eingegeben 
               (umgerechnet, hier: nrend) [-] \n
\f$t_{max}\f$: Start der Makrophytensaison, abschnittsweise eingegeben 
               umgerechnet, (hier: nrmax) [-] \n
\f$a\f$: 	   Parameter zur Beschreibung des idealtypischen Wachstumsverlaufs 
               der Makrophyten, in Code 2 gesetzt [-] \n
\f$b\f$:	   Parameter zur Beschreibung des idealtypischen Wachstumsverlaufs 
               der Makrophyten, in Code 0,5 gesetzt [-] \n

<!-- #ab: evtl. in Gerris nur bestimmte Ranges zulassen? Aber sicherlich wäre 
andere Formulierung besser, bloß: wir wissen ja gerade nicht mal, welche Arten 
wir überhaupt abbilden wollen, und davon hängen ja Saisonverläufe auch ab… -->


Die maximale Makrophytenbiomasse wird während der Modellierung vom Eingabewert 
ausgehend zunehmend modifiziert (s.u.).

In einem Beispiel von beliebig angenommenen Start (Tag des Jahres 50), Max (250) 
und End-Zeitpunkten (300) sowie minimaler (1) und maximaler (500) Pflanzendichte 
wird der mit der Formulierung beschriebene Vegetationskurvenverlauf 
verdeutlicht.
 
![Modellierter Vegetationsverlauf der Makrophyten (Beispiel, Details siehe Text), links: relativer Vegetationsverlauf, rechts: ideale Pflanzendichte (maxMakr) und Zuwachs/d.](img/makrophyt_Vegetationsverlauf.png)
\n\n 
 
Die dem idealisierten Verlauf von mftd bzw. mftd1 entsprechenden 
Makrophytenbiomassen \f$A_{Mak,ideal}\f$ bzw. \f$A_{Mak,ideal+1}\f$ 
(Pfln bzw. Pfln1) werden unter Verwendung der minimalen und maximalen 
Makrophytenbiomassen (\f$A_{Mak,Min}\f$ (Pflmin), \f$A_{Mak,Max}\f$ (Pflmax) 
aus Modell-Abschnittsdaten) wie folgt berechnet:

\f{equation}{A_{Mak,ideal} = A_{Mak,min} + dt \cdot (A_{Mak,max} - A_{Mak,Min}) \f}

mit

\f$dt\f$:      Faktor der relativen Makrophytenabundanz für Tag des Jahres [-] \n
\f$A_{Mak,ideal}\f$: absolut skalierte ideale Makrophtenabundanz, skaliert 
               zwischen min. Dichte und max. Dichte, die strangweise eingegeben 
			   wurden für Tag des Jahres x, _für x+1:_ \f$A_{Mak,ideal+1}\f$
			   _(pfln1)_

## Wachstumsrate
Die maximale Makrophytenbiomasse wird während der Modellierung vom Eingabewert 
ausgehend zunehmend modifiziert (s.u.).

Aus dem täglichen Zuwachs (Differenz der idealen Makrophytendichten des 
aktuellen und des folgenden Tages) wird schließlich eine auf die hellen Stunden 
des Tages (Stunde Sonnenuntergang SU minus Sonnenaufgang SA) bezogene maximale 
Wachstumsrate berechnet: 
 
\f{equation}{\mu_{Mak,max} = log(\frac{A_{Mak,ideal+1}}{A_{Mak,ideal}}) \cdot 
  \frac{24}{SU - SA} \f}

mit: 

\f$\mu_{Mak,max}\f$: maximale Makrophyten Wachstumsrate in hellen Tagesstunden \n
\f$SU\f$: Sonnenuntergangstageszeit \n
\f$SA\f$: Sonnenaufgangstageszeit \n

<!-- 24/(SU - SA) = Tagesanteil -->
<!-- #mf: mir war nicht klar, welche Formel ich übernehmen soll, bei Überarbeitung
ggf. nochmal Word-Doks checken -->

## Lichtabhängigkeit der Makrophyten

<!-- #ab: Den Abschnitt wollte ich eigentlich ganz weglassen inDoku, statt 
dessen nur das bei Wachstum und vielleicht noch die vereinfachte Formel
(fatalistisch zeigen, was tatsächlich gerechnet wird und taktisch die delikaten 
Details weglassen) -->

Zunächst wird aus der Globalstrahlung die zur Verfügung stehende Lichtintensität 
der photosynthetisch aktiven Strahlung \f$I_0\f$ berechnet (vgl. Grundsätzliches 
– Berechnung von PAR: PARS). 

Die Dicke der produktiven Schicht \f$t_{lip}\f$ [m] (tlip1) wird nach folgender 
Formel berechnet (vgl. Algen – Lichtinhibition):

\f{equation}{t_{lip} = \frac{ln(Gr_{str}) - ln(I_0)}{-\varepsilon_{ges}} =
  ln\left(\frac{20}{I_0}\right) \cdot (-\varepsilon_{ges})^{-1} \f}
  
aber: min. 0,01, max. WT (Wassertiefe)   		

mit:

\f$I_0\f$: Lichtintensität an Wasseroberfläche [\f$\uEmh\f$] \n
\f$\varepsilon_{ges}\f$: gesamt-Absorptionskoeffizient [...] \n
\f$t_{lip}\f$ (tlip1): Dicke der produktiven Schicht [\f$m\f$]  \n
\f$Gr_{str}\f$ (GRSTR): Kompensationslichtintensität, in Code gesetzt auf 20 [\f$\uEmh\f$] \n

Mit \f$alg_{ip}\f$ (algip1) wird schließlich das in der produktiven Schicht 
aufgrund der Gesamt-Extinktion des Wassers im Mittel verfügbare Licht 
beschrieben (vgl. s.o.):

\f{equation}{alg_{ip} = \frac{I_0}{t_{lip} \cdot \varepsilon_{ges}} \cdot 
  (1 - e^{-\varepsilon_{ges} \cdot t_{lip}}) \f}
aber: min. \f$Gr_{Str}\f$

Vereinfacht lässt sich algip auch so ausdrücken:

\f{equation}{alg_{ip} = \frac{Gr_{Str} \cdot I_0}{ln(\frac{Gr_{str}}{Io})} =
 \frac{20 - I_0}{ln\left(\frac{20}{I_0}\right)} \f}
 
*(Gleichungen ineinander eingesetzt und vereinfacht)*

<!-- #ms: Diese Umformung kann ich nicht nachvollziehen. 
Aber es stimmt, dass der Extinktionskoeffizient gekürzt wird -->

<!-- #ab: Der ganze Exponent der algip-Formel entspricht ganz einfach 20/Io, 
denn der Extinktionskoeffizient kürzt sich und ln und exp heben sich auf, wenn 
tlip eingesetzt wird. Beim ersten Faktor kürzt sich wieder der 
Extinktionskoeffizient, -Io/(ln(20/Io) bleibt stehen. Wenn dann noch die Klammer 
aufgelöst wird, ergibt sich ganz einfach „meine“ Formel.
Nicht trivial, aber lösbar. Wäre mir lieber, wer anders könnte es nachvollziehen. 
Bin ziemlich sicher dass es stimmt, denn sie ergibt das gleiche Ergebnis. 
Sieht so halt wesentlich trivialer aus. -->

<!-- #ms: Dann scheint das ein Tippfeher zu sein. Der Nenner sollte eigentlich 
kein Produkt sein -->

\warning Der Ansatz setzt derzeit voraus, dass alle Makrophyten in der 
euphotischen Schicht sind, unabhängig von deren Ausprägung (Dicke), was 
anschaulich gesehen nicht plausibel erscheint. 

Aus dem in der euphotischen Schicht absorbierten Licht wird der Lichtfaktor 
\f$F_{Mak}(I)\f$ für das Makrophytenwachstum abgeleitet:

\f{equation}{F_{Mak}(I) = \frac{alg_{ip}}{I_{opt}} \cdot 
  e^{1 - \frac{alg_{ip}}{I_{opt}}} \f}
aber: max. 1

<!-- #ms: Problem: Dieser Faktor ist ebenfalls tiefenabhängig. Es ist als falsch 
lediglich die Lichtmenge tiefenintegriert zu betrachten. Auch dieser Faktor muss 
in die Integration mit eingerechnet werden -->

mit:

\f$I_{opt}\f$: optimale Lichtintensität für Makrophytenwachstum, in Code 60 gesetzt [...] \n
\f$F_{Mak}(I)\f$ (fim): Lichtfaktor des Makrophytenwachstums [...] \n

Faktisch ist derzeit auch der Lichtfaktor aus o.g. Gründen unabhängig von der 
Absorption und er stellt sich folgendermaßen dar:

![Lichtabhängigkeit des Makrophytenwachstums.](img/makrophyt_Fmak_I0.png)
\n\n

<!-- abb. aus Z:\U\U2\QSim\Dokumentation_und_Handbuecher\QSim\Qnnette\qs8Verb!tmp.xlsx -->
<!-- #ab: Eigentlich sollte abgebildet werden, dass Makrophyten bei 
trübungsbedingt geringerer Lichtverfügbarkeit weniger gut wachsen, es wird 
jedoch nur ein Zusammenhang mit der Lichtintensität an der Gewässeroberfläche 
abgebildet. -->
<!-- #ms: Dieses Problem rührt von den oben beschriebenen Fehlern her. So wie es 
aktuell formuliert ist, beschreibt es nur die Makrophyten in der euphotischen 
Zone. Deren Wachstum ist tatsächlich unabhängig von der Trübung. Betrachtet 
werden muss jedoch der gesamte Wasserkörper. -->


## Wachstum der Makrophyten

Die oben beschriebene maximale Wachstumsrate wird mit einem Lichtintensität 
abhängigen Faktor \f$F_{Mak}(I)\f$ zu \f$\mu_{Mak}\f$ modifiziert, falls die 
maximale Wachstumsrate größer als Null ist:

\f{equation}{
  \mu_{Mak} =
 \begin{cases}
  \mu_{Mak} \cdot F_{Mak}(I) &  {wenn} \; \mu_{Mak} > 0,\\
  \mu_{Mak}   &   {wenn} \mu_{Mak} \leq 0
 \end{cases}     
\f}
 
mit: \n
\f$\mu_{Mak}\f$: max. Wachstumsrate der Makrophyten zum Zeitpunkt [...] \n

<!-- #ms: Das erscheint mir nicht sinnvoll. Für Zeiten nach der maximalen Dichte 
nimmt die Biomasse ab. Somit ist dort die Wachstumsrate negativ. Aber auch dann 
sollten die Prozesse anhängig von der Lichtkonzentration sein.
Eine Fallunterscheidung ist daher nicht logisch. -->


Bei der Herleitung und Umsetzung der Lichtabhängigkeit des Makrophytenbausteins 
bestehen einige offene Fragen, faktisch wird der o.g. Lichtfaktor derzeit in 
Abhängigkeit der Lichtintensität an der Oberfläche so berechnet (Details u. 
Formeln s.u. Lichtabhängigkeit).

## Biomasse der Makrophyten

Die Änderung der Wasserpflanzenbiomasse \f$d_{Mak,TFlie}\f$ (dpfl) innerhalb 
eines Zeitschrittes \f$T_{Flie}\f$ ergibt sich durch:
 
\f{equation}{d_{Mak,TFlie} = A_{Mak} \cdot (e^{\mu_{Mak} \cdot T_{Flie}} - 1) \f}
 
mit \n
\f$A_{Mak}\f$: Biomasse der Makrophyten [...] \n
\f$d_{Mak,TFlie}\f$ (dpfl): Änderung der Wasserpflanzenbiomasse [...] \n

Und die maximal mögliche Änderung der Wasserpflanzenbiomasse 
\f$d_{Mak,max,TFlie} \f$ (dpflmax) innerhalb eines Zeitschrittes (Verwendung 
der max. Wachstumsrate):

\f{equation}{d_{Mak,max,TFlie} = A_{Mak} \cdot (e^{\mu_{Mak,max} \cdot T_{Flie}} - 1) \f} 

sowie die neue Makrophytendichte \f$A_{Mak,TFlie}\f$ (pfl) durch

\f{equation}{A_{Mak,TFlie} = A_{Mak} + d_{Mak,TFlie} = A_{Mak} + A_{Mak} \cdot 
  (e^{\mu_{Mak,max} \cdot T_{Flie}} - 1) = A_{Mak} \cdot 
  e^{\mu_{Mak,max} \cdot T_{Flie}} \f} 

Außerdem wird noch die maximale Makrophytendichte \f$A_{Mak,max,TFlie}\f$ 
(pflmax) reduziert. Der ursprüngliche Wert von AMak,max stammt aus den Eingaben 
zum Abschnitt des Gewässers. Mit jedem modellierten Zeitschritt sinkt die 
maximal in der Saison erreichbare Makrophytendichte in dem Maße, in dem der 
jeweilige Zuwachs des Tages sich von dem maximal möglichen Zuwachs 
unterscheidet. 
 
\f{equation}{A_{Mak,max,TFlie} = A_{Mak,max} - (d_{Mak,max,TFlie} - d_{Mak,TFlie}) \f}
\f{equation}{A_{Mak,max,TFlie} = A_{Mak,max} - A_{Mak} \cdot 
  (e^{\mu_{Mak,max} \cdot T_{Flie}} - 1) + A_{Mak} \cdot 
  (e^{\mu_{Mak} \cdot T_{Flie}} - 1)\f}
\f{equation}{A_{Mak,max,TFlie} = A_{Mak,max} - A_{Mak} \cdot (
  e^{\mu_{Mak,max} \cdot T_{Flie}} - e^{\mu_{Mak} \cdot T_{Flie}})  \f}

# Makrophyten und Sauerstoff

Die Sauerstoffproduktion durch Wasserpflanzen \f$O_{2,Mak}\f$ (po2p) wird nach 
folgender Gleichung berechnet:

\f{equation}{O_{2,Prod,Mak} = A_{Mak,l} \cdot O_{2,max,Mak} \cdot 
  \left(1 - e^{-\frac{\alpha \cdot alg_{ip}}{P_{O2,max,Mak}}} \right) \cdot 
  F_{Mak} \cdot F_{TMak} \cdot T_{Flie} \cdot 24 \f}
 
mit 

\f$\alpha\f$: in Code gesetzt = 0,11 [-] \n
\f$P_{O2,max,Mak}\f$ (maxpfl): max. Sauerstoffproduktionsrate der Wasserpflanzen, 
    in Code 5,66 gesetzt [mgO2/(gTG*h)] \n
\f$Tiefe_{TFlie}\f$: Wassertiefe im aktuellen Zeitschritt [...] \n
\f$F_{TMak}\f$: Temperaturfaktor der Sauerstofffreisetzung nach der Formulierung [...] \n

\f{equation}{F_{TMak} = e^{-\frac{(T-18.)^2}{13.7^2}} \f}

![Sauerstofffreisetzung in Abhängigkeit von der Wassertemperatur.](img/makrophyt_FTMak_WT.png)
 
\f$F_{Mak}\f$ (hconpfl): Biomassefaktor Makrophyten-Sauerstofffreisetzung mit

\f{equation}{F_{Mak} = \frac{A_{Mak,TFlie}}{Tiefe_{TFlie} \cdot 1000} \f}

<!-- #ab: Plausibel, hier einfach die Wassertiefe einzurechnen? -->
<!-- #ms: F ist ein Umrechnungsfaktor. Im Modell liegt die Werte für Makrophyten 
als Biomasse pro Fläche vor. Diese Faktor rechnet sie um in Biomasse pro Volumen. 
Daher die Tiefe. Die Umrechnung wird für den Makrophytenbaustein an sich nicht 
gebraucht. Sie ist nur relevant für die Übergabewerte an andere Bausteine -->
<!-- #ab: Aber: die Makrophyten gibt es ja nur in der Wuchszone, also einem 
Teilbereich des Querprofils, und die Wassertiefe ist ja vermutlich eher die des 
gesamten Querprofils, oder? Wo ist da die Logik? Wie werden bewachsene und nicht 
bewachsene Bereiche verrechnet? So war meine Anmerkung gemeint. Dass prinzipiell 
unter Berücksichtigung der Tiefe auf das Volumen umgerechnet wird leuchtet mir 
durchaus ein! Es würde stimmen, wenn jeweils für die Wuchszone und für die Sohle 
die mittlere Wassertiefe ermittelt würden, und dann entsprechend der Breiten
 beider Zonen gewichtet würde. Habe allerdings nicht nachvollzogen, in welcher 
 Schleife der Makrophytenaufruf steckt…Kannst Du da was zu sagen, Michael?
Und was ist mit den Buhnen? -->
 <!-- #ms: Ok, dann verstehe ich den Punkt. Und ja, es wäre richtig die Tiefe 
 in der Wachstumszone zu verwenden -->


Und deren Sauerstoffzehrung (po2r) durch:

\f{equation}{O_{2,Resp,Mak} = 2 \cdot A_{Mak,l \cdot F_{Mak}} \cdot 
  T_{Flie} \cdot 24 \f}
 
<!-- #ab: Hier erstaunlicherweise kein Temperaturfaktor -->
<!-- #ms: Auch hier wäre es sinnvoller zunächst nur die Rate zu definieren --> 
 

![Für AMak=1, Tiefe=1, O2prod==2Resp bei 20µE/m².](img/makrophyt_FProd_Resp_I0.png)
\n\n 
 
<!-- Abbildung aus: Z:\U\U2\QSim\Dokumentation_und_Handbuecher\QSim\Qnnette\qs8Verb!tmp.xlsx -->

Beide Größen fließen in Oxygen.f90 in die O2-Bilanz der Modellierung ein.


# Details der Berechnungen  
<!-- #ab: Wollte ich für Doku eigentlich weglassen, ist mehr so ein Steinbruch… -->

## Jahresgang
Zunächst wird der Starttag der Wachstumssaison der Makrophyten 
\f$Tag_{Start,Mak}\f$ (NrStart) als Tag des Jahres mit folgender Näherung 
berechnet:

\f{equation}{
  Tag_{Start,Mak} =
 \begin{cases}
  (Start_{Mon,Mak} - 1) \cdot 31 + Start_{Tag,Mak} &  {wenn} Start_{Mon,Mak} \leq 2,\\
  (Start_{Mon,Mak} - 1) \cdot 31 + Start_{Tag,Mak} - 
    abrunden(0,4 \cdot Start_{Mon,Mak} + 2,3)   &  {wenn} Start_{Mon,Mak} > 2
 \end{cases}     
\f}
 
Entsprechend werden auch der Tag des Maximums der Wachstumssaison der 
Makrophyten \f$Tag_{Max,Mak}\f$ (NrMax) und der Tag des Endes der 
Wachstumssaison \f$Tag_{End,Mak}\f$ (NrEnd) ermittelt. 

Die Makrophyten Start- und Ende-Tage begrenzen die Wachstumssaison, außerhalb 
derer nur mit einer minimalen Makrophytendichte (\f$A_{Mak,Min}\f$) gerechnet 
wird (s.u.). Das Maximum der Makrophytendichte \f$A_{Mak,Max}\f$ wird am Tag 
des Maximums der Wachstumssaison \f$Tag_{Max,Mak}\f$ (NrMax) erreicht.

Der dem aktuellen Datum der Modellierung entsprechende Tag des Jahres 
\f$Tag_{Akt}\f$ (NRS) wird entsprechend berechnet:

\f{equation}{
  Tag_{Akt} =
 \begin{cases}
  (Akt_{Mon} - 1) \cdot 31 + Akt_{Tag} &  {wenn} Akt_{Tag} \leq 2,\\
  (Akt_{Mon} - 1) \cdot 31 + Akt_{Tag} - abrunden(0,4 \cdot Akt_{Mon} + 2,3) &  
  {wenn} Akt_{Mon} > 2
 \end{cases}     
\f}

Die in allen Fällen verwendete Formel nimmt für den Februar immer 28 Tage an, 
ansonsten entspricht der in QSim errechnete Tag des Jahres dem tatsächlichen 
Tag des Jahres (1.Januar = 1).

# Rand- und Anfangsbedingungen 
...

# QSim-Veröffentlichungen, die den BausteinA beschreiben und/oder anwenden:

- ...

\n\n

Textquelle: makrophyt-prozess.md; Codesource: mphyt.f90; zurück: \ref lnk_makrophyt
