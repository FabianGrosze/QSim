Wachstum / Zerfall  {#lnk_wachstum_zerfall}
===================

# Einführung {#lnk_stoffumsatz_intro}
Der Stoffumsatz,

\f[
  \frac{\partial c_m}{\partial t} = Q ( c_1 \ldots c_m \ldots c_M, x_i, t )
\f]

mit:

- \f$ c_m \f$ = m-te Konzentration; Zwischenwert im Sinne des des fractional step algorithm, \n siehe: \ref lnk_fracStep 

- \f$ t \f$ = Zeit

- \f$ Q_m \f$ = Quelle/Senke der Konzentration m (lokale Veränderungsrate, Stoffumsatz);


wird explizit in der folgenden Form diskretisiert:

\f[
 \frac{ {c_m}^{n+1}-{c_m}^{n} }{ \Delta t}=  Q ( {c_1}^{n} \ldots 
  {c_m}^{n} \ldots {c_M}^{n}, {x_i}^{n}, t^{n} )
\f]

mit: 

- \f$ n \f$ = Zeitschrittzähler \n
- \f$ \Delta t \f$ = Zeitschrittweite \n

# Abschätzung Näherungsgüte {#lnk_abumex}

Bei biologischen Stoffumsetzungsprozesse handelt es sich zumeist um Wachstums- 
und Zerfallsprozesse, die sich mittels einer Wachstumsrate mathematisch 
beschreiben lassen:
\f[
  \frac{\partial c_m}{\partial t} = Q  = \mu_m (c_1 \ldots c_M) \cdot c_m
\f]

mit:

\f$ \mu_m \f$ = Wachstumsrate der *m*-ten Konzentration, die von anderen 
Konzentrationen abhängig sein kann. 

Durch Normierung entstehen dimensionslose Größen: \f$ t' = \mu \cdot t\f$ und 
\f$ c' = \frac{c^n}{c^0}\f$.

Unter der Annahme, dass die Wachstumsrate konstant über die Zeit ist,
vereinfacht sich die obige Differentialgleichung zu: 

\f$ \frac{\partial c'}{\partial t'} = c'\f$

mit der analytische Lösung: \f$ c' = e^{t'} \f$. 

Diese Differentialgleichung ist nun ein leicht handhabbarer Test für 
Diskretisierungsverfahren:

| Verfahren | Ansatz | umgeformt |
| --------- | ------ | --------- |
| explizit  | \f$ \frac{c'^{n+1}-c'^{n}}{\Delta t'} = c'^{n}\f$ | \f$ c'^{n+1}=c'^{n}\cdot (1+\Delta t') \f$ |
| semi-implizit| \f$ \frac{c'^{n+1}-c'^{n}}{\Delta t'} = 1/2 \cdot ( c'^{n+1} + c'^{n} ) \f$  |  \f$   c'^{n+1}=c'^{n}\cdot ( (1+\Delta t'/2)/(1-\Delta t'/2) )\f$|
| implizit  |  \f$ \frac{c'^{n+1}-c'^{n}}{\Delta t'} = c'^{n+1} \f$ |  \f$ c'^{n+1}=c'^{n}\cdot 1/(1-\Delta t') \f$  |


\image html wachstum_disc_grob.svg "" width=5cm

grobe Diskretisierung \f$ \Delta t'=0.2 \f$  Abweichung explizit 8.5%\n\n

\image html wachstum_disc_mittel.svg ""  width=5cm

mittelfeine Diskretisierung \f$ \Delta t'=0.05 \f$ Abweichung explizit 2.4%\n\n

\image html wachstum_disc_fein.svg ""  width=5cm
feine Diskretisierung \f$ \Delta t'=0.01 \f$  Abweichung explizit 1.6%\n\n

Die größten Wachstumsraten treten in der Gewässermikrobiologie beim 
Algenwachstum auf, das unter optimalen Bedingungen maximale Werte von 
2 pro Tag annimmt. Das bedeutet, dass die obigen Abbildungen sich über einen 
halben Tag erstrecken.

Der mittelfeine Zeitschritt entspricht dann 2160 Sekunden und der feine 
432 Sekunden. Daraus folgert, dass sich die im Gewässergütemodell
erwartbaren Wachstums und Zerfallsprozesse bei Zeitschritten von 900 s mit guter 
Genauigkeit und bei Zeitschritten von 300 s mit sehr guter Genauigkeit
numerisch erfassen lassen.

## Stoffumsetzungs-Module {#lnk_stoffumsetzungsmodule}

Das Obengenannte bezieht sich nun nur ganz allgemein auf den Stoffumsatz 
irgendeiner Konzentration. Eine konkrete Konzentration z. B. der 
Sauerstoffgehalt verändert sich durch Beiträge aus verschiedenen 
Stoffumsetzungsprozessen, die in den Modulen des Gewässergütemodells modelliert 
sind. \n
Im Abschnitt \ref lnk_ueberblick sind alle von QSim modellierten Prozesse 
aufgeführt. \n
Die Zuordnung, welche Konzentrationen von welchem Modul geändert wird, befindet 
sich in der Liste in: \ref lnk_var_planktisch .


Textquelle: wachstum_zerfall.md ; Codesources: stoffumsatz.f95 ;  
zurück: \ref lnk_numerik
 
