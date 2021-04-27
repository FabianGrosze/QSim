Rotatorien - Prozesse {#lnk_rotatorien_equ}
=====================

## Teilprozesse ##
- Wachstum verbunden mit Fraß
- Respiration
- Mortalität

## Bilanzgleichung ##
Die im Modell berücksichtigte Bilanzgleichung für die Rotatorien 
lautet:

\f$ 
  \frac{dROT}{dt} = (\mu_{ROT}-resp_{ROT,G}-mor_{ROT})\cdot ROT
\f$, 

mit

| Parameter           | Beschreibung                                     | Einheit          |
|---------------------|--------------------------------------------------|--------------------|
| ROT                 | Biomasse der Rotatorien                          | [g m<sup>-3</sup>] |
| &mu;<sub>ROT</sub>  | Wachstumsrate der Rotatorien                     | [d<sup>-1</sup>] |
|resp<sub>ROT,G</sub> | Grundrespirationsrate der Rotatorien             | [d<sup>-1</sup>] |
| mor<sub>ROT</sub>   | altersspezifische Mortalitätsrate der Rotatorien | [d<sup>-1</sup>] |

## Wachstum ##
Die Wachstumsrate errechnet sich aus der Aufnahmerate und dem 
Ertragskoeffizient für Rotatorienbiomasse:

\f$ 
  \mu_{ROT} = up_{ROT} \cdot Y_{ROT}
\f$, 

mit

| Parameter        | Beschreibung                | Einheit          |
|------------------|-----------------------------|------------------|
| up<sub>ROT</sub> | Aufnahmerate der Rotatorien | [d<sup>-1</sup>] |
| Y <sub>ROT</sub> | Ertragskoeffiziennt         | [-]              |


### Aufnahme ###
Die Aufnahme wird wie folgt berechnet: 

\f$ 
  up_{ROT} = \frac{up_{C,ROT} \cdot G_{ROT}^{2/3}}{G_{ROT}} 
 \cdot \frac{F_{ROT}}{F_{ROT}+K_{s,ROT}}
\f$, 

mit

| Parameter        | Beschreibung                | Einheit          |
|------------------|-----------------------------|------------------|
| up<sub>C,ROT</sub> | gewichtsspezifische Aufnahmerate | [&mu;g C &mu;g C<sup>-2/3</sup> d<sup>-1</sup>] |
| G<sub>ROT</sub> | Gewicht einer Rotatorie | [%mu g C] |
| F<sub>ROT</sub> | verwertbare Algenbiomasse/Nahrungskonzentration | [g m<sup>-3</sup>] |
| K<sub>s,ROT</sub> | Futterkonzentration, bei der die halbe maximale Aufnahmerate erreicht wird | [g m<sup>-3</sup>] |


Mit der Formel für den Ertragskoeffizient:

\f$ 
  Y_{ROT} = ASS_{ROT} \cdot (1 - RESP_{ROT})
\f$, 

mit

| Parameter          | Beschreibung                | Einheit          |
|--------------------|-----------------------------|------------------|
| ASS<sub>ROT</sub>  | Assimilationsanteil               | [-] |
| RESP<sub>ROT</sub> | Anteil aktiver Respirationsanteil | [-] |


Die Mortalitätsrate der Rotatorien ist neben der Temperatur abhängig 
von der Futterkon-zentration und dem Sauerstoffgehalt. Für die
Sauerstoffabhängigkeit wird angenommen, dass ab einem kritischen
Sauerstoffwert eine lineare Erhöhung der Absterberate bis zu einem
Maximalwert hin erfolgt. 

Die temperaturabhängigen Größen sowie die Beschreibung der
Temperaturabhängigkeit sind in folgender Tabelle zusammengefasst:
<i> hier folgt in der KurzDoku eine Tabelle mit Temp-abhängigen 
Gleichungen, der Parameter nicht weiter erklärt sind. Diese Gleichungen
sollten nach Möglichkeit in die "Hauptgleichungen" der Rotatorien mit
einfließen </i>

Bei der Beprobung werden diese Organismen mit Netzen/Filtern von 50 µm 
Maschenweite gefangen und danach bestimmt und gezählt. Entsprechend werden 
sie im Modell als Individuendichte pro Volumen modelliert, aus der mit 
einem mittleren Individuengewicht die Biomasse-Konzentration berechnet 
werden kann.

aus Datei: rotatorien-equ.md; 

Code in Datei konsum.f90

