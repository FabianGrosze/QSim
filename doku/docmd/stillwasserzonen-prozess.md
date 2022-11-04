Stillwasserzonen - Prozesse {#lnk_stillwasser_prozesse}
======================================

Die Diskretisierung des Simulationsgebietes liefert im Falle der 
Buhnenfelderweiterung zwei Gitter mit gleicher Maschenweite für den Hauptstrom 
und die angeschlossenen Buhnenfelder. Der laterale Stoffaustausch 
zwischen Buhnenfeld und Hauptstrom wird wie folgt beschrieben:

## Änderung der Stoffkonzentration im Hauptstrom (C_H)

\f{equation}{ 
  C_H = C_H + \tau^*_1 \cdot (C_B - C_H) \cdot \Delta t
\f} 

\f$ C_H \f$:  Stoffkonzentration im Hauptstrom [\f$ ... \f$] \n
\f$ \tau^*_1 \f$:  Austauschrate [\f$ h^{-1} \f$] \n
\f$ C_B \f$:  Stoffkonzentration im Buhnenfeld [\f$ ... \f$] \n
\f$ \Delta t \f$:  Berechnungszeitschrittweite [\f$ h \f$] \n

## Änderung der Stoffkonzentration im Buhnenfeld (C_B)
\f{equation}{ 
  C_B = C_B + \tau^*_2 \cdot (C_H - C_B) \cdot \Delta t
\f} 

\f$ \Delta t \f$:  Berechnungszeitschrittweite [\f$ h \f$] \n
\f$ \tau^*_2 \f$:  Austauschrate [\f$ h^{-1} \f$] \n

\f{equation}{\tau^*_1 = \frac{F_B}{F_H} \cdot \frac{1}{\tau_2} \; \text{und} \;
  \tau^*_2 = \frac{1}{\tau_2} \; \text{bei} \; \tau^*_1; \tau^*_2 \leq 
  \frac{1}{2 \cdot \Delta t}  
\f}
 
\f$F_B\f$:	Querschnittsfläche des Buhnenfeldes [m2] \n
\f$F_H\f$:	Querschnittsfläche im Hauptstrom [m2] \n
\f$\tau_2\f$: Austauschzeit [h] \n

wobei nach BAUMERT et al. (2001) gilt

\f{equation}{\tau_2 = \frac{\tau^0_2}{1 + \frac{Q}{q2}} \; 
  \text{mit q2} \approx 400 m^3 \, s^{-1}\f}

\f$\tau^0_2\f$: maximale Austauschzeit bei Q = 0 [h] \n
\f$Q\f$: Abfluss [m³/s] \n


Wesentlich für das Gelingen der Modellierungen des Stoffhaushaltes ist somit 
eine genaue Bestimmung der lateralen Austauschrate zwischen Buhnenfeld und 
Hauptstrom bzw. der Aufenthaltszeiten des Wassers in den Buhnenfeldern. Bei 
alleiniger Verwendung hydraulischer Berechnungen wird eine zu kurze Verweilzeit 
in Buhnenfeldern abgeschätzt, u.a. weil das gesamte Volumen im Querschnitt als 
austauschbar angesehen wird. Daher wurden Austauschzeiten verwendet, die von 
BAUMERT et al. 2001 anhand von Ergebnissen aus Tracerversuchen und mittels 
virtueller, numerischer Teilchensimulation bestimmt worden waren und in eine 
Formel zur Abhängigkeit der Austauschzeit vom Abfluss abgeleitet werden konnten. 
Diese Formel wurde in das Modell QSim eingefügt.

Um Austauschzeiten für die gesamte Mittelelbe zu erhalten, wurden 
abschnittsweise die maximalen Austauschzeiten mit Daten aus dem 
Elbe-Tracerexperiment vom Februar 1997 (EIDNER et al. 1999) bestimmt und an 
einem weiteren Tracerexperiment 1999 validiert. 

![Abhängigkeit der Austauschzeit Tau2 vom Abfluss für fünf Elbabschnitte.](img/stillwasser_tau2_q.png)
\n\n

Tabelle: Kenngrößen und optimierte Parameter zur Beschreibung des 
Buhnenfeldeinflusses in fünf unterschiedlich strukturierten Elbabschnitten. 
\f$D_x\f$ = longitudinaler Dispersionskoeffizient; \f$F_2/F_1\f$ = 
Quotient FlächeBuhnenfeld/FlächeHauptstrom

| Anfang Elbe-km | Ende Elbe-km | Dx [m2/s] | F2/F1 | \f$\tau^0_2\f$ [h] | \f$\tau_2\f$ [h] |
| --- | --- |---- | ---- | --- | --- |
| 1,9 | 130 | 149 | 0,05 | 4,5 | 3,3 | 
| 130 | 250 |  51 | 0,30 | 1,5 | 1,1 | 
| 250 | 295 |  89 | 0,23 | 2,0 | 1,4 | 
| 295 | 500 | 127 | 0,23 | 5,0 | 3,0 | 
| 500 | 585 | 128 | 0,21 | 8,0 | 4,2 | 

Aus der beschriebenen Modellerweiterung ist ersichtlich, dass die Buhnenfelder 
als Stillwasserzonen betrachtet werden und nicht hydraulisch an den Hauptstrom 
angeschlossen sind. Daher kann im Modell keine mittlere Fließgeschwindigkeit in 
den Buhnenfeldern errechnet werden. Sie muss abgeschätzt werden, da dieser 
Parameter bei wichtigen Prozessen, etwa dem Stoffaustausch zur Atmosphäre und 
zur Gewässersohle, benötigt wird. Als Wert für die Modellanwendung „Mittelelbe“ 
wird eine Fließgeschwindigkeit von 0,1 m/s gesetzt. Untersuchungen in 
Buhnenfeldern ergaben mittlere Werte in diesem Bereich.

\n\n

Textquelle: stillwasserzonen-prozess.md; Codesource: qsim1d.f90; 
zurück: \ref lnk_stillwasserzonen
