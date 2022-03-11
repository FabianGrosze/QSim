Rotatorien - Formelzeichen/Variablennamen {#lnk_rotatorien_pars}
=========================================

## Liste der Formelzeichen und Variablennamen des Rotatorien-Bausteins: ##

| Formelzeichen               | Codevariable  | Wert  | Einheit           | Bedeutung                                         | Herkunft |
| --------------------------- | ------------- | ----- | ----------------- | --------------------------------------------------| -------- |
| \f$ A_i \f$                 | agr, aki, abl |       | mg<sub>tr</sub>/l | Konzentration der Grün-, Kiesel- bzw. Blaualgen   | x        |
| \f$ ASS \f$                 | ASS           |       | -                 | Assimilationsanteil der ingestierten Beute        | b        |
| \f$ ASS_0 \f$               | ASSmax        | 0.84  | -                 | max. Assimilationsanteil                          | v        |
| \f$ \alpha_{ASS} \f$        | eASS          | 0.705 | -                 | Exponent des Assimilationsanteil                  | v        |
| \f$ \alpha_\mathrm{mort}\f$ | eMort         | 2.0   | -                 | Exponent der Mortalitätsrate                      | v        |
| \f$ \eta \f$                | eta           |       | -                 | Ertragskoeffizient                                | b        |
| \f$ A_\mathrm{Filtr} \f$    | filAGes       |       | mg<sub>tr</sub>/l | Konzentration aller filtrierbaren Algen           | b        |
| \f$ f_\mathrm{Nahr} \f$     | fNahr         |       | -                 | Einflussfaktor des Nahrungsangebots               | b        |
| \f$ f_{O_2} \f$             | fO2           |       | -                 | Einflussfaktor des Sauerstoffs                    | v        |
| \f$ f_{Temp,I} \f$          | fTempIng      |       | -                 | Einflussfakor der Temperatur auf Ingestionsrate   | b        |
| \f$ f_{T, mort} \f$         | fTempMort     |       | -                 | Einflussfakor der Temperatur auf Mortalitätsrate  | b        |
| \f$ I \f$                   | I             |       | 1/d               | Ingestionsrate der Rottorien bzgl. Algen          | b        |
| \f$ I_0 \f$                 | Imax          |       | 1/d               | max. Ingestionsrate bei T = 20°C                  | e        |
| \f$ K_m \f$                 | KmRot         |       | mg<sub>tr</sub>/l | Halbsättigungskonstante der Rotatorien            | e        |
| \f$ mort \f$                | mort          |       | 1/d               | Mortalitätsrate    der Rotatorien                 | b        |
| \f$ mort_\max \f$           | mortmax       | 0.15  | 1/d               | maximale Mortalitätsrate bei T = 20°C             | v        |
| \f$ \mu \f$                 | mu            |       | 1/d               | Wachstumsrate der Rotatorien                      | b        |
| \f$ [O_2]_\mathrm{krit} \f$ | o2Krit        | 2.5   | mg/l              | kritische Sauerstoffkonzentration                 | v        |
| \f$ Q_{10, mort}\f$         | Q10Mort       | 2.10  | -                 | Q10-Wert der Mortalitätsrate                      | v        |
| \f$ Q_{10,I} \f$            | Q10Ing        | 2.16  | -                 | Q10-Wert Ingestion                                | v        |
| \f$ Q_{10, respG} \f$       | Q10RespG      | 3.11  | -                 | Q10-Wert der Grundrespirationsrate                | v        |
| \f$ Rot \f$                 | Rot           |       | Ind/l             | Konzentration der Rotatorien                      | x        |
| \f$ resp_G \f$              | respG         |       | 1/d               | Grundrespirationsrate der Rotatorien              | b        |
| \f$ RESP \f$                | RESP          | 0.203 | -                 | Respirationsanteil der assimilierten Beute        | v        |
| \f$ resp_G \f$              | respG         |       | 1/d               | Grundrespirationsrate                             | b        |
| \f$ resp_{G,0} \f$          | respG0        |       | 1/d               | Grundrespirationsrate bei T = 20°C                | e        |
| \f$ T \f$                   | tempw         |       | °C                | Wassertemperatur                                  | x        |
| \f$ [O_2] \f$               | vo2           |       | mg/l              | Sauerstoffkonzentration                           | x        |
| \f$ \Phi_i \f$              | zagr,zaki,zabl|       | -                 | Filtrierbarkeit der Grün-, Kiesel- bzw. Blaualgen | e        |


Anmerkung:
Die Einheit mg<sub>tr</sub>/l bezeichnet das Trockengewicht pro Liter.


Herkunft:
+ x - Bilanzvariable QSim
+ b - berechnet, Zwischengröße / Übergabevariable
+ e - Eingabe
+ v - Vorgabe im Quellcode gesetzt

<hr>
aus Datei: rotatorien-pars.md

Code in Datei konsum_kern.f90 

