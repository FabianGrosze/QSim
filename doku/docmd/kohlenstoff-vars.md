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

\n\n

Textquelle: kohlenstoff-vars.md; Codesource: orgC.f90; orgc_huelle.f95; zurück: \ref lnk_orgC

<!-- #mf: ab hier noch aufräumen 
!! <h2>Formelzeichen</h2>
!! Der Baustein orgc() dient dazu, die nachfolgend genannten Wasserinhaltsstoffe zu bilanzieren.\n
!! Dabei wird Kohlenstoff im folgenden vielfach mit dem Elementsymbol C (von lat. carbo „Holzkohle“) abgekürzt.\n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i=\ref cp1 + \ref cp2, </td><td> \subpage bilaCP in der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i=\ref cd1 + \ref cd2, </td><td> \subpage bilaCD in der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ CM \f$    </td><td> *CM* </td><td> \subpage bilaCM </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> *BAC* </td><td> \subpage bilaBAC </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ C_{ref} \f$  </td><td> Cref (nur intern orgc() ) </td><td> \subpage bilaCref </td><td> mgC/l </td><td> ? </td></tr>
!!</table>\n
!! mit \f$ i \f$ als Index der Abbaubarkeit  -  i=1: leicht abbaubar; i=2: schwer abbaubar\n
!! \n\n

\n\n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2 </td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$  hy_{P,i} \f$  </td><td> hyP(i) </td><td> Hydrolyserate für die partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> 1/d </td><td> 0.12 </td></tr>
!!<tr><td> \f$ f_T \f$  </td><td> ftemp </td><td> Faktor der Temperaturabhängigkeit </td><td> - </td><td> 0.2 ... 1 </td></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ T \f$  </td><td> \ref tempw </td><td> Wassertemperatur </td><td> C° </td><td> 0 ... 30 </td></tr>
!!<tr><td> \f$ T_{opt} \f$  </td><td> Topt </td><td> optimale Temperatur </td><td>  C°  </td><td>  25 hartcodiert  </td></tr>
!!<tr><td> \f$ dti \f$  </td><td> dti </td><td> Bezugs-Temperatur </td><td>  C°  </td><td> 20 hartcodiert </td></tr>
!!<tr><td> \f$ hy_{P,e} \f$  </td><td> *hyPe* </td><td> Parameter (APARAM.txt 17|4) </td><td> 1/d </td><td>  </td></tr>
!!<tr><td> \f$ C-BSB_5 \f$  </td><td> \ref obsb </td><td> biologischer Sauerstoffbedarf siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> chemischer Sauerstoffbedarf  siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> *TFLIE* </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CM \f$  </td><td> *CM* </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2 </td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ hy_{D,i} \f$  </td><td> hyP(i) </td><td> Hydrolyserate für die gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> 1/d </td><td> ?? </td></tr>
!!<tr><td> \f$ f_T \f$  </td><td> ftemp </td><td> Faktor der Temperaturabhängigkeit, siehe: \ref hyp </td><td> - </td><td> 0.2 ... 1 </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> *BAC* </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ hy_{max,D,i} \f$  </td><td> hymxD(i) *hymxDe* </td><td> maximale Hydrolyserate für die gelösten organischen C-Verbindungen </td><td> 1/d </td><td> APARAM.txt hymxDe 17|5 </td></tr>
!!<tr><td> \f$ K_{s,D,i} \f$  </td><td> ksD(i) </td><td> Halbsättigungskonstante für die Hydrolyse der gelösten organischen C-Verbindungen </td><td> mgC/l </td><td> APARAM.txt KsD1e 18|1, KsD2e 18|2 </td></tr>
!!<tr><td> \f$ C-BSB_5 \f$  </td><td> \ref obsb </td><td> biologischer Sauerstoffbedarf  siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> chemischer Sauerstoffbedarf  siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> *TFLIE* </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> *BAC* </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ up_{HBAC} \f$  </td><td> upBAC </td><td> Aufnahmerate von Kohlenstoff durch heterotrophe Bakterien </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ up_{HBAC,max} \f$  </td><td> upBACm *upBACe* </td><td> maximale Aufnahmerate für die Aufnahme von Kohlenstoff durch heterotrophe Bakterien</td><td>  1/d  </td><td> APARAM.txt upBACe 18|4  </td></tr>
!!<tr><td> \f$ k_{s,M} \f$  </td><td> ksM *KsMe* </td><td> Halbsättigungskonstante für die Aufnahme von Kohlenstoff durch heterotrophe Bakterien</td><td> mgC/l </td><td> APARAM.txt KsMe 18|3 </td></tr>
!!<tr><td> \f$ res_{HBAC} \f$  </td><td> resbac </td><td> Respirationsrate der heterotrophen Bakterien </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ CM \f$  </td><td> *CM* </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ Y \f$  </td><td> YBAC *YBACe* </td><td> Ertragskoeffizient (Ernährungseffizienz) </td><td> - </td><td> APARAM.txt YBACe 18|5 </td></tr>
!!<tr><td> \f$ res_{G,HBAC} \f$  </td><td> *rsGBACe* </td><td> Grundrespirationsrate bei Optimumstemperatur </td><td> 1/d </td><td>  APARAM.txt rsGBACe 19|1 </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> *TFLIE* </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2 </td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ pfl \f$  </td><td> \ref pfl </td><td> Pflanzentrockengewicht Makrophyten </td><td> g/m² </td><td>  </td></tr>
!!<tr><td> \f$ h \f$  </td><td> tiefe </td><td> Wassertiefe </td><td> m </td><td> 0 ... 25 </td></tr>
!!<tr><td> \f$ f_T \f$  </td><td> ftemp </td><td> Faktor der Temperaturabhängigkeit, siehe: \ref hyp </td><td> - </td><td> 0.2 ... 1 </td></tr>
!!<tr><td> \f$ \frac{C-BSB_5}{CSB} \f$  </td><td> vcb </td><td> Anteil biologisch mittelfristig zehrbaren Materials am Gesamt-Kohlenstoff</td><td> - </td><td>  </td></tr>
!!<tr><td> \f$ C-BSB_5 \f$  </td><td> \ref obsb </td><td> biologischer Sauerstoffbedarf  siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> chemischer Sauerstoffbedarf  siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!</table>\n
mit \f$ i \f$ als Index der Abbaubarkeit  -  i=1: leicht abbaubar; i=2: schwer abbaubar\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta CD_1 \f$  </td><td> - </td><td> Änderung an leicht abbaubaren gelösten C-Verbindungen infolge des Absterbens von Biomasse in einem Zeitschritt </td><td> mgC/l je Zeitschritt </td><td>  </td></tr>
!!<tr><td> \f$ BSB_{HNF} \f$  	</td><td> *BSBHNF* </td><td> Absterben und Exkretion Heterotropher Naloflagelaten </td><td> mgC/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ dki_{mort} \f$  	</td><td> \ref dkimor </td><td> Absterberate Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ dgr_{mort} \f$  	</td><td> \ref dgrmor </td><td> Absterberate Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ dbl_{mort} \f$  	</td><td> \ref dblmor </td><td> Absterberate Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ Rot_{mort} \f$  	</td><td> \ref abszo </td><td> Absterberate der Rotatorien </td><td> Ind. je Liter und Zeitschritt </td><td>  </td></tr>
!!<tr><td> \f$ G_{Rot} \f$  	</td><td> *GRote* </td><td> durchschnittliches Gewicht einer Rotatorie </td><td> µgBiom./Ind. </td><td>  </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \bar{u} \f$  </td><td> vmitt </td><td> tiefengemittelte Geschwindigkeit </td><td> m/s </td><td> 0 ... 2.5 </td></tr>
!!<tr><td> \f$ u_* \f$  </td><td> ust </td><td> Sohlschubspannungsgeschwindigkeit sqrt(tau/rho) </td><td>  m/s  </td><td> ~ vmitt/10 </td></tr>
!!<tr><td> \f$ h \f$  </td><td> tiefe </td><td> Wassertiefe </td><td> m </td><td> 0 ... 25 </td></tr>
!!<tr><td> \f$ g \f$  </td><td> g </td><td> Gravitation (Erdbeschleunigung) </td><td> m/s² </td><td> 9.81 </td></tr>
!!<tr><td> \f$ K_{st} \f$  </td><td> rau </td><td> Reibungsbeiwert nach Strickler (Manning) </td><td> (m**0.3333)/s </td><td> 5 ... 50 </td></tr>
!!<tr><td> \f$ sedCP_1 \f$  </td><td> sedCP1 </td><td> 1-leicht abbaubare partikuläre C-Verbindungen, die in einem Zeitschritt sedimentieren </td><td> mgC/l (zeitschrittbezogen) </td><td> ? </td></tr>
!!<tr><td> \f$ sedCP_2 \f$  </td><td> sedCP2 </td><td> 2-schwer abbaubare partikuläre C-Verbindungen, die in einem Zeitschritt sedimentieren </td><td> mgC/l (zeitschrittbezogen) </td><td> ? </td></tr>
!!<tr><td> \f$ sedBAC \f$  </td><td> sedBAC </td><td> in Bakterien enthaltene C-Verbindungen, die in einem Zeitschritt sedimentieren </td><td> mgC/l (zeitschrittbezogen) </td><td> ? </td></tr>
!!<tr><td> \f$ sedC_{ref} \f$  </td><td> sedCrf </td><td> refraktäre, partikuläre C-Verbindungen, die in einem Zeitschritt sedimentieren </td><td> mgC/l (zeitschrittbezogen) </td><td> ? </td></tr>
!!<tr><td> \f$ f_{BSB,gr} \f$  </td><td> \ref fbsgr </td><td> Faktor zur Berechnung der ablagerungsfreien Grenzkonzentration von zehrungsfähigem Material ???? </td><td> - </td><td> ? </td></tr>
!!<tr><td> \f$ f_{ref,gr} \f$  </td><td> \ref frfgr </td><td> Faktor zur Berechnung der ablagerungsfreien Grenzkonzentration von refraktärem Material ???? </td><td> - </td><td> ? </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta {CP_i}_{drs} \f$  </td><td> CD(i = \ref cd1+\ref cd2 </td><td> Änderung an partikulären C-Verbindungen in einem Zeitschritt </td><td> mgC/l je Zeitschritt </td><td>  </td></tr>
!!<tr><td> \f$ i \f$  </td><td> - </td><td> Index 1 leicht abbaubar, 2 schwer abbaubar </td><td> - </td><td> 1,2 </td></tr>
!!<tr><td> \f$ \Delta {C_{ref}}_{drs} \f$  </td><td> - </td><td> Änderung an refrakteren C-Verbindungen in einem Zeitschritt  </td><td>  mgC/l je Zeitschritt  </td><td>  </td></tr>
!!<tr><td> \f$ ssdr \f$  </td><td> \ref ssdr </td><td> Schwebstoffaufnahme durch Dreissena </td><td>  mg/l je Zeitschritt  </td><td>  </td></tr>
!!</table>\n\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> *BAC* </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ HNFBAC \f$  </td><td> *HNFBAC* </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen, die in jedem Zeitschritt infolge Wegfraß durch heterotrophe Nanoflagelaten verloren geht </td><td> mgC/l (zeitschrittbezogen) </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> *TFLIE* </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n

!!<table > 
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta {CP_1}_{faec} \f$ </td><td> - </td><td> Zunahme der \ref bilaCP infolge Faeces (1-leicht-abbaubar) </td><td> mg/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Ki} \f$  </td><td> \ref zexki </td><td> Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen </td><td> mg/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Gr} \f$  </td><td> \ref zexgr </td><td> Ausscheidungen der Rotatorien infolge Konsums von Grünalgen </td><td> mg/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Bl} \f$  </td><td> \ref zexbl </td><td> Ausscheidungen der Rotatorien infolge Konsums von Blaualgen </td><td> mg/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Ki} \f$  </td><td> \ref drfaek </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Kieselalgen </td><td>  mg/l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Gr} \f$  </td><td> \ref drfaeg </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Grünalgen </td><td>  mg/l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Bl} \f$  </td><td> \ref drfaeb </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen </td><td>  mg/l je Zeitschritt  </td><td> ? </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta {O_2}_{orgC} \f$  </td><td> \ref bsbt </td><td> Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt </td><td> mgO2/l zeitschrittbezogen </td><td> ? </td></tr>
!!<tr><td> \f$ res_{HBAC} \f$  </td><td> resbac </td><td> Respirationsrate der heterotrophen Bakterien, siehe: \ref BACm </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> *BAC* </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ O_2dC \f$  </td><td> \ref o2bsb </td><td> Sauerstoff-Kohlenstoffverhältnis beim C-Abbau </td><td> mgO2/mgC </td><td>  </td></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2 </td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ C-BSB_5 \f$  </td><td> \ref obsb </td><td> - </td><td> - </td><td> - </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> Kohlenstoffbürtiger chemischer Sauerstoffbedarf </td><td> mgO2/l </td><td> ? </td></tr>
!!<tr><td> \f$ pfl \f$  </td><td> \ref pfl </td><td> Pflanzentrockengewicht Makrophyten </td><td> g/m² </td><td>  </td></tr>
!!<tr><td> \f$ h \f$  </td><td> *TIEFE* </td><td> Wassertiefe </td><td> m </td><td> 0 ... ca. 25 </td></tr>
!!<tr><td> \f$ f_T \f$  </td><td> ftemp </td><td> Faktor der Temperaturabhängigkeit, siehe: \ref hyp </td><td> - </td><td> 0.2 ... 1 </td></tr>
!!<tr><td> \f$ C_{ref} \f$  </td><td> Cref </td><td> Konzentration der refraktären (nicht abbaubaren) C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CM \f$  </td><td> *CM* </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ C_{HNF} \f$  </td><td> *CHNF* </td><td> C-Masse der heterotrophen Nanoflagelaten </td><td> mgC/l </td><td> ? </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ BSB_{NH4} \f$  </td><td> *doN* </td><td> Ammoniumfreisetzung infolge Kohlenstoffmineralisierung </td><td> mgN/l je Zeitschritt</td><td> ? </td></tr>
!!<tr><td> \f$ BSB_{C} \f$  </td><td> \ref bsbct </td><td> zu CO2 mineralisierter Kohlenstoff, siehe: \ref o2zehr (### hier stimmt was nicht ###)</td><td> mgC/l je Zeitschritt</td><td> ? </td></tr>
!!<tr><td> \f$ N_{org} \f$  </td><td>\ref  nl0 </td><td> Verhältnis von Stickstoff zu Kohlenstoff in organischem Material </td><td> mgN/mgC </td><td> ? </td></tr>
!!<tr><td> \f$ orgN \f$  </td><td> orgn </td><td> Gesamt-Stickstoffgehalt im organischem Material </td><td> mgN/l </td><td> ? </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> Kohlenstoffbürtiger chemischer Sauerstoffbedarf </td><td> mgO2/l </td><td> ? </td></tr>
!!<tr><td> \f$ BSB_{HNF} \f$  	</td><td> *BSBHNF* </td><td> Absterben und Exkretion Heterotropher Naloflagelaten </td><td> mgC/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ f_N \f$  	</td><td> ## fehlt </td><td> zu implementierendes Verhältnis </td><td> mgN/mgC </td><td> ? </td></tr>
!!<tr><td> \f$ dki_{mort} \f$  	</td><td> \ref dkimor </td><td> Absterberate Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ dgr_{mort} \f$  	</td><td> \ref dgrmor </td><td> Absterberate Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ dbl_{mort} \f$  	</td><td> \ref dblmor </td><td> Absterberate Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{N,Ki} \f$  </td><td> *Q_NK* </td><td> Stickstoffanteil in der Biomasse der Kieselalgen </td><td> mgN/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{N,Gr} \f$  </td><td> *Q_NG* </td><td> Stickstoffanteil in der Biomasse der Grünalgen </td><td> mgN/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{N,Bl} \f$  </td><td> *Q_NB* </td><td> Stickstoffanteil in der Biomasse der Blaualgen </td><td> mgN/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Rot_{mort} \f$  	</td><td> \ref abszo </td><td> Absterberate der Rotatorien </td><td> Ind. je Liter und Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ G_{Rot} \f$  	</td><td> *GRote* </td><td> durchschnittliches Gewicht einer Rotatorie </td><td> µgBiom./Ind. </td><td> ? </td></tr>
!!<tr><td> \f$ N_R \f$  	</td><td> \ref nzoo </td><td> Stickstoffanteil in der Rotatorienbiomasse </td><td> mgN/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Ki} \f$  </td><td> \ref zexki </td><td> Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Gr} \f$  </td><td> \ref zexgr </td><td> Ausscheidungen der Rotatorien infolge Konsums von Grünalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Bl} \f$  </td><td> \ref zexbl </td><td> Ausscheidungen der Rotatorien infolge Konsums von Blaualgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Ki} \f$  </td><td> \ref drfaek </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Kieselalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Gr} \f$  </td><td> \ref drfaeg </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Bl} \f$  </td><td> \ref drfaeb </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ BSB_{PO4} \f$  </td><td> *bsbctP* </td><td> ortho-Phosphat-Freisetzung infolge Kohlenstoffmineralisierung </td><td> mgP/l je Zeitschritt</td><td> ? </td></tr>
!!<tr><td> \f$ BSB_{C} \f$  </td><td> \ref bsbct </td><td> zu CO2 mineralisierter Kohlenstoff, siehe: \ref o2zehr (### hier stimmt was nicht ###)</td><td> mgC/l je Zeitschritt</td><td> ? </td></tr>
!!<tr><td> \f$ P_{org} \f$  </td><td> \ref pl0 </td><td> Verhältnis von Phosphor zu Kohlenstoff in organischem Material </td><td> mgP/mgC zeitschrittbezogen </td><td> ? </td></tr>
!!<tr><td> \f$ orgP \f$  </td><td> orgp </td><td> Gesamt-Phosphorgehalt im organischem Material </td><td> mgP/l </td><td> ? </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> Kohlenstoffbürtiger chemischer Sauerstoffbedarf </td><td> mgO2/l </td><td> ? </td></tr>
!!<tr><td> \f$ BSB_{HNF} \f$  	</td><td> *BSBHNF* </td><td> Absterben und Exkretion Heterotropher Naloflagelaten </td><td> mgC/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ f_P \f$  	</td><td> ## fehlt </td><td> zu implementierendes Verhältnis </td><td> mgP/mgC </td><td> ? </td></tr>
!!<tr><td> \f$ dki_{mort} \f$  	</td><td> \ref dkimor </td><td> Absterberate Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ dgr_{mort} \f$  	</td><td> \ref dgrmor </td><td> Absterberate Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ dbl_{mort} \f$  	</td><td> \ref dblmor </td><td> Absterberate Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{P,Ki} \f$  </td><td> *Q_PK* </td><td> Phosphoranteil in der Biomasse der Kieselalgen </td><td> mgP/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{P,Gr} \f$  </td><td> *Q_PG* </td><td> Phosphoranteil in der Biomasse der Grünalgen </td><td> mgP/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{P,Bl} \f$  </td><td> *Q_PB* </td><td> Phosphoranteil in der Biomasse der Blaualgen </td><td> mgP/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Rot_{mort} \f$  	</td><td> \ref abszo </td><td> Absterberate der Rotatorien </td><td> Ind. je Liter und Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ G_{Rot} \f$  	</td><td> *GRote* </td><td> durchschnittliches Gewicht einer Rotatorie </td><td> µgBiom./Ind. </td><td> ? </td></tr>
!!<tr><td> \f$ P_R \f$  	</td><td> \ref pZoo </td><td> Phosphoranteil in der Rotatorienbiomasse </td><td> mgP/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Ki} \f$  </td><td> \ref zexki </td><td> Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Gr} \f$  </td><td> \ref zexgr </td><td> Ausscheidungen der Rotatorien infolge Konsums von Grünalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Bl} \f$  </td><td> \ref zexbl </td><td> Ausscheidungen der Rotatorien infolge Konsums von Blaualgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Ki} \f$  </td><td> \ref drfaek </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Kieselalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Gr} \f$  </td><td> \ref drfaeg </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Bl} \f$  </td><td>\ref  drfaeb </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!</table>\n

 <table>
<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta SS_{org} \f$  </td><td>  *dorgSS* </td><td> Veränderung von suspendierten Sedimenten aus C-Verbindungen </td><td> mg SS /l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> *BAC* </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ C_{ref} \f$  </td><td> Cref </td><td> Konzentration der refraktären (nicht abbaubaren) C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!</table>\n\n

!!<table>
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ orgC_{sed} \f$  </td><td> *orgCsd* </td><td> Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert </td><td> mgC/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ sedCP_1 \f$, \f$sedCP_2 \f$, \f$sedBAC \f$, \f$sedC_{ref} \f$  </td><td> sedCP1, sedCP2, sedBAC, sedCrf </td><td> siehe \ref Sediorgc </td><td>  mgC/l je Zeitschritt  </td><td> ? </td></tr>
!!</table>\n\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> tflie </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2</td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> *TFLIE* </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CM \f$  </td><td> *CM* </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> tflie </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> *BAC* </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> *TFLIE* </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n

!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ C_{ref} \f$  </td><td> Cref </td><td> Konzentration der refraktären (nicht abbaubaren) C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> Kohlenstoffbürtiger chemischer Sauerstoffbedarf </td><td> mgO2/l </td><td> ? </td></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2 </td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ CM \f$  </td><td> *CM* </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> *BAC* </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ C_{HNF} \f$  </td><td> *CHNF* </td><td> C-Masse der heterotrophen Nanoflagelaten </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> *TFLIE* </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n

--> 
Textquelle: kohlenstoff-vars.md ; Codesource: orgC.f90 , orgc_huelle.f95