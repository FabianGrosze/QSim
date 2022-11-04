Phosphor - Prozesse {#lnk_phosphor_prozesse}
===================== 

In QSim werden der Gesamt-Phosphorgehalt und der Phosphatgehalt bilanziert.

## Teilprozesse ##
Folgende Teilprozesse wirken sich auf den Gesamt-Phosphorgehalt aus:
* Sedimentation der Schwebstoffe
* Sedimentation der Algen

Zurzeit ausgeschaltet sind der Phosphor-Fluss aus dem Sediment sowie die 
Phosphor-Aufnahme durch Dreissena und durch benthische Algen.
 

Folgende Teilprozesse wirken sich auf den Phosphatgehalt aus:
* Mineralisierung durch Bakterien
* Netto-Ausscheidungen durch Algen
* Ausscheidungen der Rotatorien

Zurzeit ausgeschaltet sind der Phosphat-Fluss aus dem Sediment und die 
Ausscheidung der Dreissena.

# Bilanzgleichungen

Die Bilanzgleichung für den Gesamt-Phosphorgehalt, \f$ P_{ges} \f$, lautet:

\f{equation}{ \frac{dP_{ges}}{dt} = - C_{org, sed} \cdot Y_{C:P} - A_{sed, i} \cdot Q_{P,A_i}\f} 
<!-- 
            gesPt = gesP(ior)                                               &
                    - orgCsd(mstr,ior) * pl0(ior)                           &
                    - sedalk(ior) * Q_PK(ior)                               & 
                    - sedalb(ior) * Q_PB(ior)                               &
                    - sedalg(ior) * Q_PG(ior)                               &
                    + Psed                                                  &
                    - algdrk(ior) * Q_PK(ior)                               &
                    - algdrg(ior) * Q_PG(ior)                               &
                    - algdrb(ior) * Q_PB(ior)                               &    
                    - (albewg(ior) - alberg(ior)) * Qmx_PG                  &
                    - (albewk(ior) - alberk(ior)) * Qmx_PK 
-->

\f$ P_{ges} \f$:    Gesamt-Phosphorgehalt im Gewässer [\f$ mg P \cdot L^{-1} \f$] \n 
\f$ C_{org, sed} \f$:  Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert [\f$ mg C \cdot L^{-1} \cdot t^{-1}\f$] \n
\f$ Y_{C:P} \f$:  P/C Verhältnis von Phosphor zu Kohlenstoff in organischem Material [ \f$ mg P \cdot mg C^{-1} \f$ ] \n
\f$ A_{sed, i} \f$: Sedimentierte Menge der Algengruppe *i*  [\f$ mgBio \cdot L^{-1} \cdot t^{-1}\f$] \n
\f$ Q_{P,A_i} \f$:  Phosphoranteil der Algenbiomasse der Algengruppe *i*  [\f$ mgP \cdot mgBio^{-1} \f$] \n
\n\n

Die Bilanzgleichung des gelöstem reaktivem Phosphats, \f$ SRP \f$, lautet:
\f{equation}{ \frac{SRP}{dt} = r_{P, bac} + a_{Alg} + a_{Rot} \f}

<!-- 
        gelPt = gelP(ior)                           &
                + doP                               &
                + (mw_agrP + mw_akiP + mw_ablP)     &
                + pSed                              &
                + gelPzo                            &
                + gelPdr				
--> 

\f$ SRP \f$:  Konzentration gelöster reaktiver Phosphat  [ \f$ mg P \cdot L^{-1} \f$ ] \n
\f$ r_{P, bac} \f$: Phosphat-P-Freisetzung beim Abbau org. Kohlenstoffverbidungen  [ \f$ mg P \cdot L^{-1} \cdot t^{-1} \f$ ] \n
\f$ a_{Alg} \f$: Netto-Ausscheidung durch Algen  [ \f$ mg P \cdot L^{-1} \cdot t^{-1} \f$ ] \n
\f$ a_{Rot} \f$: Ausscheidung der Rotatorien   [  \f$ mg P \cdot L^{-1} \cdot t^{-1} \f$ ] \n
\n\n

### Netto-P-Ausscheidungen der Algen ###
Die planktischen Algen nehmen für ihr Wachstum Phosphat auf, ein Teil davon 
wird durch Respiration wieder freigesetzt.

\f{equation}{ a_{Alg} = u_{P, i} \cdot (\mu_{A_i} - A_{resp, A_i}) \f}
<!--
            agrP(nkz) = -up_PGz(nkz,ior) * (agrbrz(nkz,ior) - algagz(nkz,ior)) &
                        - (albewg(ior)-alberg(ior)) * Qmx_PG
...
                sagP = sagP + ((agrP(nkz) + agrP(nkz-1)) /2.) * dH2D
--> 
\f$ a_{Alg} \f$: Netto-Ausscheidung durch Algen  [ \f$ mg P \cdot L^{-1} \cdot t^{-1} ?? \f$ ] \n
\f$ u_{P, i} \f$:  P-Aufnahme der Algengruppe *i* [\f$ mg P \cdot mg Bio^{-1} \f$] \n
\f$ \mu_{A_i} \f$: Brutto-Zuwachs der Algenbiomasse der Algengruppe *i* [\f$ mg Bio \cdot L^{-1} \cdot t^{-1} \f$ ] \n
\f$ A_{resp, A_i} \f$: Respirierte Algenbiomasse der Algengruppe *i* [\f$ mg Bio \cdot L^{-1} \cdot t^{-1} \f$ ] \n
\n\n

### P-Ausscheidungen Zooplankton ###
Die P-Ausscheidung des Zooplanktons setzt sich aus der Grundrespiration 
\f$ r_{z, grund} \f$ und der aktiven Respiration \f$ r_{z, A_i} \f$
der jeweiligen Algenmenge zusammen.

\f{equation}{ a_{Rot} = r_{z, grund} \cdot 0,01 + r_{z, A_i} \cdot \theta_{A_i} \cdot Q_{P,A_i} \f}

<!--
        gelpzo = dzres1(ior) * 0.01                &
               + dzres2(ior) * hconKi * Q_PK(ior)  &
               + dzres2(ior) * hcongr * Q_PG(ior)  &
               + dzres2(ior) * hconbl * Q_PB(ior)
-->

\f$ a_{Rot} \f$: Ausscheidung der Rotatorien   [  \f$ mg P \cdot L^{-1} \cdot t^{-1} \f$ ] \n
\f$ r_{z, grund} \f$: Grundrespiration des Zooplanktons [\f$ mg Bio \cdot L^{-1} \cdot t^{-1}\f$] \n 
\f$ r_{z, A_i} \f$: Fraßabhängige Respirationsrate des Zooplanktons [\f$ mg Bio \cdot L^{-1} \cdot t^{-1}\f$] \n
\f$ \theta_{A_i} \f$: Anteil der Algenklasse *i* an der Gesamtalgenmenge  [\f$ - \f$] \n
\f$ Q_{P,A_i} \f$: Phosphoranteil der Algenbiomasse der Algengruppe *i* [\f$ mgP \cdot mgBio^{-1} \f$] \n

<!-- #mf: ist r_{z, grund} (=dzres1) == repG? --> 
 
## QSim-Veröffentlichungen, die den ...-Baustein beschreiben und/oder anwenden: 
...
 

\n\n

Textquelle: phosphor-prozess.md; Codesource: ncyc.f90; zurück: \ref lnk_phosphor