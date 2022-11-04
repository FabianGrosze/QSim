Organischer Kohlenstoff - Formelzeichen/Variablennamen {#lnk_orgC_vars} 
========================================

## Liste der Formelzeichen und Variablennamen, Silikat:(Stand QSim xx.yy) ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft | 
|----------------|------------|--------------|---------|---------|---------|
|  |  |  |  |  | |
|  |  |  |  |  | |
|  |  |  |  |  | |


Herkunft: 
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

# Variablen-Tabelle aus dem Kurzdoku-Teil

| Bezeichnung im Text | Bedeutung | Einheit | Wert in QSim | Bezeichnung im Code | Herkunft | Referenz| 
| --- | --- | ---- | --- | --- | ---- | --- | 
| Amor,j | abgestorbene Algenbiomasse der Algenklasse j | g*m-3*d-1 |  | aki, agr, abl |  | | 
| αCD,i | Anteil der gelösten organischen C-Verbindungen am Gesamtkohlenstoffgehalt für die Stoffgruppe i | - |  | hcCD1, hcCD2 |  | | 
| αCP,i | Anteil der partikulären organischen C-Verbindungen am Gesamtkohlenstoffgehalt für die Stoffgruppe i | - |  | hcCP1, hcCP2 |  | | 
| αCref | Anteil der refraktären C-Verbindungen am Gesamtkohlenstoffgehalt | - |  | hcfrfg |  | | 
| BAC | Bakterienbiomasse | g C*m-3 |  | BAC(ior) |  | | 
| CA,j | Kohlenstoffanteil der abgestorbenen Algenbiomasse der Algenklasse j | - | 0.48 | Cagr, Caki, Cabl |  | | 
| CDi | gelöste organische C-Verbindungen der Stoffgruppe i | g*m-3 |  | hcCDi |  | | 
| CM | Konzentration an monomeren Substanzen | mgC*l-1 |  | hcCM |  | | 
| |  |  |  | CMt |  | | 
| CPi | partikuläre organische C-Verbindungen der Stoffgruppe i | g*m-3 |  | hcCPi |  | | 
| CPsed,i | sedimentierte partikuläre organische Kohlenstoffverbindungen der Stoffklasse i | g*m-3*d-1 |  | sedCPi |  | | 
| Cref | refraktäre organische Kohlenstoffverbindungen | g*m-3*d-1 |  | hcFRFG |  | | 
| Cref,sed | sedimentierte refraktäre organische Kohlenstoffverbindungen | g*m-3*d-1 |  | sedCRF |  | | 
| CRot | Kohlenstoffanteil der Rotatorienbiomasse | - | 0.45 | CROT |  | | 
| DRfaec,j | ausgeschiedene Algenbiomasse (Faeces) der Algenklasse j durch Dreissena | g*m-3*d-1 |  | drfaek(ior) |  | | 
| dti  | Temperaturspanne für Bakterien | °C | 20 | dti |  | Billen 1991| 
| fBac(T) | Temperaturabhängiger Faktor des Bakterienwachstums | - | Formel [8] | ftemp |  | Billen 1991| 
| GrazBac,HNF | Grazingrate der HNF an Bakterien | mgC*l-1*d-1 | Formel Fehler! Verweisquelle konnte nicht gefunden werden. | upHNF |  | | 
| fC_O | Umrechnung veratmeter Kohlenstoff in Sauerstoff | gO2*gC-1 | Feb 67 | - |  | | 
 | |  |  | (32 g O2*mol-1 pro 12 g C-1 * mol -1) |  |  | | 
 | | Maximale Grazingrate der HNF an Bakterien | mgC*l-1*d-1 |  | upmHNF |  | | 
 | |  |  |  | upHNFe |  | | 
| GRot | Gewicht einer Rotatorie | µg TG | Messwert | GROT |  | | 
| hyDi | Hydrolyserate für die gelöste Stoffgruppe i | d-1 | hymxD(2): 0.474**(obsb(ior)/ocsb(ior))**(-1.346) | hymxD(i) |  | | 
| hyPi | Hydrolyserate für die partikuläre organische Stoffgruppe i | d-1 | hyP(2): 1.51*(obsb(ior)/ocsb(ior))**2.31 | hyP(i) |  | | 
| IRot,mor | Anzahl abgestorbener Rotatorien | Ind*dm-3 |  |  |  | | 
| Ks,CM | Halbsättigungskonstante der Bakterien für monomere Substanzen | µg C * l-1 | 100 | ksM |  | Billen 1991| 
| respBac | Respirationsrate der Bakterien | d-1 | Formel [9] | resBAC |  | | 
| respBac,G bei 20 °C | Grundrespirationsrate der Bakterien | d-1 | 0.03 | resGBAC |  | Abgeleitet von Baretta-Bekker et al. 1998| 
| ROTfaec,j | ausgeschiedene Algenbiomasse (Faeces) der Algenklasse j durch Rotatorien | g*m-3*d-1 |  | zexki, zexgr, zexbl |  | | 
| T | Wassertemperatur | °C | Messw. | TempW |  | | 
| Topt,Bac | optimale Wassertemperatur für Bakterien | °C | 25 | Topt |  | Billen 1991| 
| upBAC | Aufnahmerate der monomeren C-Verbindungen durch Bakterien | d-1 |  | upBAC |  | Formel [7]| 
| upBac,max | maximale Aufnahmerate der Bakterien | d-1 | 24. Jul | upBACm |  | Baretta-Bekker et al. 1998| 
| YBac | Ertragskoeffizient für Bakterienbiomasse | - | 0.25 | YBAC |  | Billen 1991| 

i = 1 entspricht leicht abbaubar, i = 2 entspricht schwer abbaubar; 
j = 1: Kieselalgen, j = 2: Grünalgen, j = 3: Blaualgen


\n\n

Textquelle: kohlenstoff-vars.md; Codesource: orgC.f90; orgc_huelle.f95; zurück: \ref lnk_orgC

# Tabelle zum alten Dokuportal.

| Formelzeichen | Beschreibung | Dimension | Wertebereich | Variablen-Name | Herkunft |
| ------------- | ------------ | --------- | ------------ | -------------- | -------- |
| \f$  hy_{P,i} \f$ | Hydrolyserate für die partikulären organischen C-Verbindungen der i-ten Stoffgruppe |  1/d  |  0.12  | hyP(i) | |
| \f$ \Delta {C_{ref}}_{drs} \f$ |  Änderung an refrakteren C-Verbindungen in einem Zeitschritt   | mgC/l je Zeitschritt |  |  -  | |
| \f$ \Delta {CP_1}_{faec} \f$ |  Zunahme der \ref lnk_bilaCP infolge Faeces (1-leicht-abbaubar)  |  mg/l je Zeitschritt |  ?  |  -  | |
| \f$ \Delta {CP_i}_{drs} \f$ |  Änderung an partikulären C-Verbindungen in einem Zeitschritt  |  mgC/l je Zeitschritt |    |  CD(i = \ref cd1 + \ref cd2  | |
| \f$ \Delta {O_2}_{orgC} \f$ |  Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt  |  mgO2/l zeitschrittbezogen |  ?  | lnk_bsbt | |
| \f$ \Delta SS_{org} \f$   |  Veränderung von suspendierten Sedimenten aus C-Verbindungen  |  mg SS /l je Zeitschritt |  ?  |   *dorgSS*  | |
| \f$ \Delta t \f$ |  Zeitschrittweite  |  d  |  0.01 ... 1  |  tflie  | |
| \f$ BSB_{C} \f$ |  zu CO2 mineralisierter Kohlenstoff, siehe: \ref lnk_o2zehr (_hier stimmt was nicht_) |  mgC/l je Zeitschritt |  ?  |  \ref bsbct  | |
| \f$ BSB_{HNF} \f$ |  *BSBHNF*  |  Absterben und Exkretion Heterotropher Naloflagelaten  |  mgC/l je Zeitschritt  | | |
| \f$ BSB_{NH4} \f$ |  Ammoniumfreisetzung infolge Kohlenstoffmineralisierung  |  mgN/l je Zeitschritt | ? |  *doN*  | |
| \f$ BSB_{PO4} \f$   |  ortho-Phosphat-Freisetzung infolge Kohlenstoffmineralisierung  |  mgP/l je Zeitschritt | ? |  *bsbctP*  | |
| \f$ C_{HNF} \f$ |  C-Masse der heterotrophen Nanoflagelaten  |  mgC/l  |  ?  |  *CHNF*  | |
| \f$ C_{ref} \f$ |  Konzentration der refraktären (nicht abbaubaren) C-Verbindungen, \ref lnk_bilaCref|  mgC/l | ? | Cref  | |
| \f$ CD_i \f$ |  Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe | ||  CD(i, \ref cd1 + \ref cd2  | |
| \f$ CM \f$   |  Konzentration der monomolekularen organischen C-Verbindungen , \ref lnk_bilaCM|  mgC/l  |  ?  |  *CM*  | |
| \f$ CP_i \f$   |  Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe| ||  CP(i, \ref cp1 + \ref cp2  | |
| \f$ CSB \f$ |  Kohlenstoffbürtiger chemischer Sauerstoffbedarf| ||  \ref ocsb  | |
| \f$ dbl_{mort} \f$ |  \ref dblmor  |  Absterberate Blaualgen  |   mgBiom./l je Zeitschritt   | | |
| \f$ dgr_{mort} \f$ |  \ref dgrmor  |  Absterberate Grünalgen  |   mgBiom./l je Zeitschritt   | | |
| \f$ dki_{mort} \f$ |  \ref dkimor  |  Absterberate Kieselalgen  |  mgBiom./l je Zeitschritt  | | |
| \f$ DR_{faec,Bl} \f$ |  Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen  |   mg Biom./l je Zeitschritt |  ?  |  \ref drfaeb  | |
| \f$ DR_{faec,Gr} \f$ |  Ausscheidungen der Dreissena-Muscheln infolge Konsums von Grünalgen  |   mg Biom./l je Zeitschritt |  ?  |  \ref drfaeg  | |
| \f$ DR_{faec,Ki} \f$ |  Ausscheidungen der Dreissena-Muscheln infolge Konsums von Kieselalgen  | mgBiom./l je Zeitschritt |  ?  |  \ref drfaek  | |
| \f$ f_{BSB,gr} \f$ |  Faktor zur Berechnung der ablagerungsfreien Grenzkonzentration von zehrungsfähigem Material ????  |  -  |  ?  |  \ref fbsgr  | |
| \f$ f_{ref,gr} \f$ |  Faktor zur Berechnung der ablagerungsfreien Grenzkonzentration von refraktärem Material ???? |  -  |  ?  |  \ref frfgr  | |
| \f$ f_N \f$ |  ## fehlt  |  zu implementierendes Verhältnis  |  mgN/mgC  | | |
| \f$ G_{Rot} \f$  |  *GRote*  |  durchschnittliches Gewicht einer Rotatorie  |  µgBiom./Ind.  | | |
| \f$ h \f$ |  Wassertiefe  |  m  |  0 ... ca. 25  |  *TIEFE*  | |
| \f$ HBAC \f$ |  Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen| || *BAC*  | |
| \f$ HNFBAC \f$   |  Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen, die in jedem Zeitschritt infolge Wegfraß durch heterotrophe Nanoflagelaten verloren geht  |  mgC/l (zeitschrittbezogen)  |  ?  |  *HNFBAC*  | |
| \f$ hy_{max,D,i} \f$   |  maximale Hydrolyserate für die gelösten organischen C-Verbindungen  |  1/d  |  APARAM.txt hymxDe 17/5  |  hymxD(i) *hymxDe*  | |
| \f$ i \f$   |  Index Abbaubarkeit  - i=1: leicht abbaubar| ||  i  | |
| \f$ N_{org} \f$   |  Verhältnis von Stickstoff zu Kohlenstoff in organischem Material  |  mgN/mgC  | ? | \ref  nl0  | |
| \f$ N_R \f$  |  \ref nzoo  |  Stickstoffanteil in der Rotatorienbiomasse  |  mgN/mgBiom.  | | |
| \f$ O_2dC \f$   |  Sauerstoff-Kohlenstoffverhältnis beim C-Abbau  |  mgO2/mgC  |    |  \ref o2bsb  | |
| \f$ orgC_{sed} \f$ |  Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert  |  mgC/l je Zeitschritt |  ?  |  *orgCsd*  | |
| \f$ orgN \f$ | Gesamt-Stickstoffgehalt im organischem Material  |  mgN/l  |  ?  |  orgn  | |
| \f$ orgP \f$ | Gesamt-Phosphorgehalt im organischem Material  |  mgP/l  |  ?  |  orgp  | |
| \f$ P_{org} \f$ |  Verhältnis von Phosphor zu Kohlenstoff in organischem Material  |  mgP/mgC zeitschrittbezogen |  ?  |  \ref pl0  | |
| \f$ P_R \f$ | \ref pZoo  | Phosphoranteil in der Rotatorienbiomasse |  mgP/mgBiom.  | | |
| \f$ Q_{N,Bl} \f$ | Stickstoffanteil in der Biomasse der Blaualgen |  mgN/mgBiom.  |  ?  |  *Q_NB*  | |
| \f$ Q_{N,Gr} \f$ | Stickstoffanteil in der Biomasse der Grünalgen  |  mgN/mgBiom.  |  ?  |  *Q_NG*  | |
| \f$ Q_{N,Ki} \f$ | Stickstoffanteil in der Biomasse der Kieselalgen  |  mgN/mgBiom.  |  ?  |  *Q_NK*  | |
| \f$ Q_{P,Bl} \f$ | Phosphoranteil in der Biomasse der Blaualgen  |  mgP/mgBiom.  |  ?  |  *Q_PB*  | |
| \f$ Q_{P,Gr} \f$ | Phosphoranteil in der Biomasse der Grünalgen  |  mgP/mgBiom.  |  ?  | *Q_PG* | |
| \f$ res_{HBAC} \f$ | Respirationsrate der heterotrophen Bakterien, siehe: \ref lnk_BACm  |  1/d  |  ?  |  resbac  | |
| \f$ ROT_{faec,Bl} \f$ |  Ausscheidungen der Rotatorien infolge Konsums von Blaualgen  |  mgBiom./l je Zeitschritt  |  ?  |  \ref zexbl  | |
| \f$ ROT_{faec,Gr} \f$ |  Ausscheidungen der Rotatorien infolge Konsums von Grünalgen  |  mgBiom./l je Zeitschritt  |  ?  |  \ref zexgr  | |
| \f$ ROT_{faec,Ki} \f$ |  Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen  |  mgBiom./l je Zeitschritt|  ?  |  \ref zexki  | |
| \f$ Rot_{mort} \f$ | \ref abszo | Absterberate der Rotatorien | Ind. je Liter und Zeitschritt  | | |
| \f$ sedBAC \f$ | in Bakterien enthaltene C-Verbindungen, die in einem Zeitschritt sedimentieren | mgC/l (zeitschrittbezogen)  |  ?  |  sedBAC  | |
| \f$ sedC_{ref} \f$ |  refraktäre, partikuläre C-Verbindungen, die in einem Zeitschritt sedimentieren  | mgC/l (zeitschrittbezogen)  |  ?  |  sedCrf  | |
| \f$ sedCP_1 \f$ |  1-leicht abbaubare partikuläre C-Verbindungen, die in einem Zeitschritt sedimentieren  | mgC/l (zeitschrittbezogen)  |  ?  |  sedCP1  | |
| \f$ sedCP_2 \f$ |  2-schwer abbaubare partikuläre C-Verbindungen, die in einem Zeitschritt sedimentieren  | mgC/l (zeitschrittbezogen)  |  ?  |  sedCP2  | |
| \f$ ssdr \f$ | Schwebstoffaufnahme durch Dreissena  |   mg/l je Zeitschritt   |    |  \ref ssdr  | |
| \f$ \bar{u} \f$ |  tiefengemittelte Geschwindigkeit  |  m/s  |  0 ... 2.5  |  vmitt  | |
| \f$ \Delta CD_1 \f$   |  Änderung an leicht abbaubaren gelösten C-Verbindungen infolge des Absterbens von Biomasse in einem Zeitschritt  |  mgC/l je Zeitschritt  |    |  -  | |
| \f$ \Delta t \f$   |  Zeitschrittweite  |  d  |  0.01 ... 1  |  *TFLIE*  | |
| \f$ \frac{C-BSB_5}{CSB} \f$   |  Anteil biologisch mittelfristig zehrbaren Materials am Gesamt-Kohlenstoff |  -  |    |  vcb  | |
| \f$ BSB_{HNF} \f$ | Absterben und Exkretion Heterotropher Naloflagelaten  |  mgC/l je Zeitschritt  | ? | *BSBHNF* | |
| \f$ C-BSB_5 \f$   |  biologischer Sauerstoffbedarf  siehe: \ref lnk_o2zehr |    |    |  \ref obsb  | |
| \f$ dbl_{mort} \f$ |  Absterberate Blaualgen  |   mgBiom./l je Zeitschritt   |  ?  | \ref dblmor  | |
| \f$ dgr_{mort} \f$ |  Absterberate Grünalgen  |   mgBiom./l je Zeitschritt   | ? | \ref dgrmor  | |
| \f$ dki_{mort} \f$ |  Absterberate Kieselalgen  |  mgBiom./l je Zeitschritt  | ?  | \ref dkimor  | |
| \f$ dti \f$ | Bezugs-Temperatur  |   C°   |  20 hartcodiert  |  dti  | |
| \f$ f_P \f$ | ## fehlt  |  zu implementierendes Verhältnis  |  mgP/mgC  | | |
| \f$ f_T \f$ | Faktor der Temperaturabhängigkeit, siehe: \ref lnk_hyp  |  -  |  0.2 ... 1  |  ftemp  | |
| \f$ g \f$ | Gravitation (Erdbeschleunigung)  |  m/s²  |  9.81  |  g  | |
| \f$ h \f$ | Wassertiefe  |  m  |  0 ... 25  |  tiefe  | |
| \f$ HBAC \f$ |  Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen  |  mgC/l  |  ? | *BAC*  | |
| \f$ hy_{D,i} \f$ | Hydrolyserate für die gelösten organischen C-Verbindungen der i-ten Stoffgruppe  | 1/d |  ??  |  hyD(i)   (?)| |
| \f$ hy_{P,e} \f$ | Parameter (APARAM.txt 17/4)  |  1/d  |    | *hyPe* | |
| \f$ K_{s,D,i} \f$ | Halbsättigungskonstante für die Hydrolyse der gelösten organischen C-Verbindungen | mgC/l | APARAM.txt KsD1e 18/1, KsD2e 18/2 | ksD(i) | |
| \f$ k_{s,M} \f$   |  Halbsättigungskonstante für die Aufnahme von Kohlenstoff durch heterotrophe Bakterien |  mgC/l  |  APARAM.txt KsMe 18/3  |  ksM *KsMe*  | |
| \f$ K_{st} \f$ | Reibungsbeiwert nach Strickler (Manning)  |  (m**0.3333)/s  |  5 ... 50  |  rau  | |
| \f$ pfl \f$ | Pflanzentrockengewicht Makrophyten  |  g/m²  |    |  \ref pfl  | |
| \f$ Q_{P,Ki} \f$ |  Phosphoranteil in der Biomasse der Kieselalgen  |  mgP/mgBiom.  | ?  | *Q_PK* | |
| \f$ res_{G,HBAC} \f$ |  Grundrespirationsrate bei Optimumstemperatur  |  1/d  |   APARAM.txt rsGBACe 19/1  |  *rsGBACe*  | |
| \f$ Rot_{mort} \f$ | Absterberate der Rotatorien  |  Ind. je Liter und Zeitschritt  |    | \ref abszo  | |
| \f$ T \f$   |  Wassertemperatur  |  C°  |  0 ... 30  |  \ref tempw  | |
| \f$ T_{opt} \f$   |  optimale Temperatur  |   C°   |   25 hartcodiert   |  Topt  | |
| \f$ u_* \f$ |  Sohlschubspannungsgeschwindigkeit sqrt(tau/rho)  |   m/s   |  ~ vmitt/10  |  ust  | |
| \f$ up_{HBAC,max} \f$   |  maximale Aufnahmerate für die Aufnahme von Kohlenstoff durch heterotrophe Bakterien |   1/d   |  APARAM.txt upBACe 18/4   |  upBACm *upBACe*  | |
| \f$ up_{HBAC} \f$   |  Aufnahmerate von Kohlenstoff durch heterotrophe Bakterien  |  1/d  |  ?  |  upBAC  | |
| \f$ Y \f$   |  Ertragskoeffizient (Ernährungseffizienz)  |  -  |  APARAM.txt YBACe 18/5  |  YBAC *YBACe*  | |

 
Textquelle: kohlenstoff-vars.md ; Codesource: orgC.f90 , orgc_huelle.f95