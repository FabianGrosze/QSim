!> \page BSB organischer Kohlenstoff
!! 
!! Der Baustein orgc() dient dazu, die Fraktionen des organischen Kohlenstoffs zu bilanzieren,
!! deren Abbau den biochemischen Sauerstoffbedarf (BSB) hervorruft\n
!! Dabei wird Kohlenstoff im folgenden vielfach mit dem Elementsymbol C (von lat. carbo „Holzkohle“) abgekürzt.\n
!! <center> 
!! \image html orgc.klein.png ""
!! <a href="./img/orgc.ppt" target="_blank">Download des Schaubilds als .ppt</a>
!! \image latex orgc.png "Bilanz des organischen Kohlenstoffs" width=0.95\textwidth
!! </center>
!!\n
!! <h2>Herkunft</h2>
!!      SUBROUTINE orgc()\n
!!      Programm zur Berechnung des biochemischen Sauerstoffbedarfs (BSB)\n
!!      AUTOR : VOLKER KIRCHESCH      \n                                   
!!      entnommen aus Version qsim13.301_28mae18\n 
!! 
!! <h2>Teilprozesse</h2>
!! <ol>
!!    <li>\subpage hyp</li>
!!    <li>\subpage hyd</li>
!!    <li>\subpage BACm</li>
!!    <li>\subpage fluxmaphy</li>
!!    <li>\subpage mortalgC</li>
!!    <li>\subpage facesC</li>
!!    <li>\subpage Cschwebdreiss</li>
!!    <li>\subpage Sediorgc</li>
!!    <li>\subpage bacHNFgraz</li>
!! </ol>
!! Der gesamte orgc - Baustein basiert auf einer tiefengemittelten Betrachtungsweise.
!! \n
!! <h4>Auswirkungen</h4>
!! Die Umwandlung von organischen C-Verbindungen wirkt sich darüber hinaus aus auf:
!! <ol>
!!    <li>\subpage o2zehr</li>
!!    <li>\subpage nh4freis</li>
!!    <li>\subpage pfreis</li>
!!    <li>\subpage kohlensauer</li>
!!    <li>\subpage schwebkohl</li>
!!    <li>\subpage sedkohl</li>
!! </ol>
!! <h4>Randbedingungen ergänzen/erschließen</h4>
!! An Rändern kann nur der C-BSB5 und CSB-Wert vorgegeben werden. Intern werden aber Bilanzen für die o.g. 7
!! Konzentrationen aufgestellt. Aufgrund plausibler Annahmen wird eine Aufteilung in der Subroutine 
!! orgc_start() vorgenommen siehe dazu \ref orgc_aufteilung.
!! Siehe dazu auch \ref randbedingungen_ergaenzen .
!!
!! <h2>Schnittstellenbeschreibung</h2>
!! <code>
!! call orgc() \ref obsb, \ref ocsb, \ref tiefe, \ref rau, \ref tflie, \ref vmitt
!! , \ref flae, \ref zooind, \ref abszo, \ref tempw, \ref vbsb, \ref bsbt, \ref flag, \ref elen, \ref ior, \ref anze  &  \n                 
!! , \ref ecsb, \ref ebsb, \ref qeinl, \ref vabfl, \ref sdbsb, \ref zexki, \ref zexgr
!! , \ref bsbbet, \ref dkimor, \ref dgrmor, \ref jiein, \ref bsbgr, \ref bsbki, \ref akbcm &\n
!! , \ref agbcm, \ref pfl, \ref ezind, \ref abl, \ref abbcm, \ref bsbbl, \ref csbbl
!! , \ref dblmor, \ref zexbl, \ref drfaeb, \ref csbki, \ref csbgr, \ref ischif, \ref echla &\n
!! , \ref evkigr, \ref eantbl, \ref aki, \ref agr, \ref drfaek, \ref drfaeg
!! , \ref drfaes, \ref ssdr, \ref orgcsd, \ref orgcsd0, \ref orgcsd_abb, \ref cd, \ref cp, \ref cm, \ref bac, \ref ecd &\n
!! , \ref ecp, \ref ecm, \ref ebac, \ref toc_csb, \ref grote, \ref vcsb, \ref vkigr
!! , \ref antbl, \ref hnfbac, \ref bsbhnf, \ref chnf, \ref zbac &\n
!! , \ref bvhnf, \ref echnf, \ref fbsgr, \ref frfgr, \ref fbsgrs, \ref frfgrs
!! , \ref bacmua, \ref dorgss, \ref ilbuhn, \ref iwied, \ref fkm, \ref bsbct, \ref qeinll &\n
!! , \ref iorla, \ref iorle, \ref ieinls, \ref pl0, \ref q_pk, \ref q_pb, \ref q_pg
!! , \ref pzoo, \ref nl0, \ref q_nk, \ref q_nb, \ref q_ng, \ref nzoo, \ref etemp, \ref bsbctp &\n
!! , \ref don, \ref hsdflub, \ref hype, \ref hymxde, \ref ksd1e, \ref ksd2e
!! , \ref ksme, \ref upbace, \ref jdoc1, \ref jdoc2, \ref ybace, \ref rsgbace &\n
!! , \ref nkzs, \ref mstr, \ref itags, \ref monats, \ref uhrz, \ref  azstrs
!! , \ref  bsbzoo &\n
!! , \ref kontroll, \ref i) !!wy\n      
!! </code> \n
!!
!! <h2>Dokumentation und Veröffentlichungen</h2>
!!
!! Bisher existiert eine Dokumentation des Moduls zum 
!! organischen Kohlenstoff (Biochemischer / Chemischer Sauerstoffbedarf) als Kapitel 7 der
!! <a href="./pdf/QSimDoku_ncycWy.pdf" target="_blank">Kurzdoku</a> (Version vom 22. Nov. 2017)
!! \n\n
!! Der orgc-Baustein wurde verwendet, um die Zehrung von schwer abbaubarem organischen Kohlenstoff für den Vortrag:\n
!! <a href="./pdf/FutureEms_Schoel2013.pdf"  target="_blank">
!! Oxygen - Conceptual Model and QSim Approach</a>
!! \n Schöl et al. 2013 \n zu simulieren. 
!! \n\n
!! Siehe dazu auch :Validierung orgc in \ref vali
!! (war mal ./vali/orgc_oxygen.html )
!! \n
!! <h2>Formelzeichen</h2>
!! Der Baustein orgc() dient dazu, die nachfolgend genannten Wasserinhaltsstoffe zu bilanzieren.\n
!! Dabei wird Kohlenstoff im folgenden vielfach mit dem Elementsymbol C (von lat. carbo „Holzkohle“) abgekürzt.\n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i=\ref cp1 + \ref cp2, </td><td> \subpage bilaCP in der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i=\ref cd1 + \ref cd2, </td><td> \subpage bilaCD in der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ CM \f$    </td><td> \ref CM </td><td> \subpage bilaCM </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> \ref BAC </td><td> \subpage bilaBAC </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ C_{ref} \f$  </td><td> Cref (nur intern orgc() ) </td><td> \subpage bilaCref </td><td> mgC/l </td><td> ? </td></tr>
!!</table>\n
!! mit \f$ i \f$ als Index der Abbaubarkeit  -  i=1: leicht abbaubar; i=2: schwer abbaubar\n
!! \n\n
!! zurück zu: \ref Stoffumsatz , Quelle: orgc_huelle.f95

!

!13.1
!!        call orgC(\ref obsb, \ref ocsb, \ref TIEFE, \ref rau, \ref TFLIE, \ref vmitt, \ref bl01,   \ref bl02   \n
!!                 , \ref zooind, \ref abszo, \ref tempw, \ref vbsb, \ref bsbt, \ref flag, \ref elen, \ref ior, \ref anze   \n
!!                 , \ref ecsb, \ref ebsb, \ref qeinl, \ref vabfl, \ref sdbsb, \ref zexki, \ref zexgr, \ref bsbbet, \ref dkimor, \ref dgrmor   \n
!!                 , \ref jiein, \ref bsbgr, \ref bsbki, \ref akbcm, \ref agbcm, \ref pfl, \ref ezind   \n
!!                 , \ref abl, \ref abbcm, \ref bsbbl, \ref csbbl, \ref dblmor, \ref zexbl, \ref drfaeb  \n
!!                 , \ref csbki, \ref csbgr, \ref ischif, \ref echla, \ref evkigr, \ref eantbl            \n
!!                 , \ref aki, \ref agr, \ref drfaek, \ref drfaeg, \ref drfaes, \ref ssdr, \ref orgCsd     \n
!!                 , CD=\ref cd1+\ref cd2, CP=\ref cp1+\ref cp2, \ref CM, \ref BAC, \ref o2bsb, \ref GRote, \ref vcsb, \ref vkigr, \ref antbl   \n
!!                 , \ref HNFBAC \ref BSBHNF, \ref CHNF, \ref echnf, \ref fbsgr, \ref frfgr, \ref fbsgrs, \ref frfgrs   \n
!!                 , \ref bacmua, \ref dorgSS, \ref ilbuhn, \ref iwied, \ref fkm, \ref bsbct   \n
!!                 , \ref pl0, \ref Q_PK, \ref Q_PB, \ref Q_PG, \ref pZoo, \ref nl0, \ref Q_NK, \ref Q_NB, \ref Q_NG, \ref nzoo   \n
!!                 , \ref bsbctP, \ref doN, \ref hyPe, \ref hymxDe, \ref KsD1e, \ref ksd2e, \ref KsMe, \ref upBACe   \n
!!                 , \ref YBACe, \ref rsGBACe, \ref nkzs, \ref mstr, \ref itags, \ref monats, \ref uhrz   \n
!!                 , kontroll, i)
! -------------------------------------------------------------------------------------------Teilprozesse Umwandlung-----------------------
!> \page hyp Hydrolyse partikulärer C-Verbindungen in gelöste organische C-Verbindungen
!! Bei der Hydrolyse (Umwandlung, Zerfall) von partikulären organischen C-Verbindungen in
!! gelöste organische C-Verbindungen wird eine temperaturabhängige Hydrolyserate angesetzt.\n
!! In Q-Sim werden 2 Fraktionen unterschieden, 1. leicht und 2. schwer abbaubare C-Verbindungen.
!! Dabei wird angenommen, dass sich schwer abbaubare partikuläre C-Verbindungen nur in 
!! schwer abbaubare gelöste C-Verbindungen umwandeln; analog wandeln sich leichtabbaubare 
!! partikuläre C-Verbindungen auch nur in leicht abbaubare gelöste C-Verbindungen um:
!! \f[ 
!! \frac{\partial CD_i}{\partial t}_{hyp} = hy_{P,i}*f_T*CP_i
!! \f]
!! Im gleichen Maße wie die Konzentrationen der gelösten C-Verbindungen zunehmen, müssen natürlich die der 
!! partikulären organischen C-Verbindungen abnehmen:
!! \f[ 
!! \frac{\partial CP_i}{\partial t}_{hyp} = -\frac{\partial CD_i}{\partial t}_{hyp}
!! \f]
!! Bei der Temperaturabhängigkeit wird angenommen, dass sich bei 25 °C ein Optimum ergibt:                                         
!! \f[ 
!! f_T= e^{ -\frac{ {(T-T_{opt})}^2}{dti^2} }
!! \f]
!! \image html ftemp.svg 
!! \image latex ftemp.eps 
!!
!! Die Hydrolyseraten (bei Optimumstemperatur) für die partikulären organischen C-Verbindungen 
!! werden bei leicht und schwer abbaubaren C-Verbindungen unterschiedlich bestimmt:
!! Bei den 1. leicht abbaubaren C-Verbindungen wird eine Konstante angesetzt, die vorgegeben werden muss (APARAM.txt);
!! bei den 2. schwer abbaubare C-Verbindungen wird eine Abhängigkeit vom Verhältnis des 
!! biologischen zum chemischen Sauerstoffbedarf berücksichtigt.
!! \f{eqnarray*}{
!! hy_{P,1}=& hy_{P,e}  \\
!! hy_{P,2}=& 1.51*(\frac{C-BSB_5}{CSB})^{2.31}
!! \f}
!! Die Veränderungen werden explizit diskretisiert:
!! \f{eqnarray*}{
!! \Delta {CD_i}_{hyp} = \frac{\partial CD_i}{\partial t}_{hyp} \cdot \Delta t  \\
!! \Delta {CP_i}_{hyp} = \frac{\partial CP_i}{\partial t}_{hyp} \cdot \Delta t
!! \f}
!! Negative Werte werden ausgeschlossen, weil wo nichts ist, kann auch nichts verbraucht werden.
!! hartes Klipping bei:\f$ CP_i(t+\Delta t) > 0.00001 \f$ ist nicht massenerhaltend in Bezug auf :\f$ CD_i \f$
!! \n\n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2 </td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$  hy_{P,i} \f$  </td><td> hyP(i) </td><td> Hydrolyserate für die partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> 1/d </td><td> 0.12 </td></tr>
!!<tr><td> \f$ f_T \f$  </td><td> ftemp </td><td> Faktor der Temperaturabhängigkeit </td><td> - </td><td> 0.2 ... 1 </td></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ T \f$  </td><td> \ref tempw </td><td> Wassertemperatur </td><td> °C </td><td> 0 ... 30 </td></tr>
!!<tr><td> \f$ T_{opt} \f$  </td><td> Topt </td><td> optimale Temperatur </td><td>  °C  </td><td>  25 hartcodiert  </td></tr>
!!<tr><td> \f$ dti \f$  </td><td> dti </td><td> Bezugs-Temperatur </td><td>  °C  </td><td> 20 hartcodiert </td></tr>
!!<tr><td> \f$ hy_{P,e} \f$  </td><td> \ref hyPe </td><td> Parameter (APARAM.txt 17|4) </td><td> 1/d </td><td>  </td></tr>
!!<tr><td> \f$ C-BSB_5 \f$  </td><td> \ref obsb </td><td> biologischer Sauerstoffbedarf siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> chemischer Sauerstoffbedarf  siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> \ref TFLIE </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
!
!> \page hyd Hydrolyse gelöster organischer C-Verbindungen in monomolekulare organische C-Verbindungen
!! 1. leicht und 2. schwer abbaubare gelöste C-Verbindungen werden in monomolekulare organische C-Verbindungen umgewandelt.
!! Diese Hydrolyse wird befördert von extracellulären Enzymen, die von heterotrophen Bakterien ins Wasser abgegeben werden,
!! um monomolekulare organische C-Verbindungen zu erhalten, von denen selbige sich ernähren.
!! 
!! \f[ 
!! \frac{\partial CM}{\partial t}_{hyd} = (hy_{D,1}+hy_{D,2})*f_T*HBAC
!! \f]
!! im gleichen Maße wie die Konzentrationen der monomolekularen C-Verbindungen zunimmt, 
!! mussen natürlich die Konzentrationen der gelösten organischen C-Verbindungen abnehmen:
!! \f[ 
!! \frac{\partial CD_i}{\partial t}_{hyd} = -hy_{D,i}*f_T*HBAC
!! \f]
!! Die Hydrolyseraten für die gelösten organischen C-Verbindungen werden abhängig vom 
!! vorhandenen gelösten organischen Kohlenstoff unter Zuhilfenahme von Maximalwert und
!! Halbsättigungskonstante gemäß fogender Formel berechnet:
!! \f[ 
!! hy_{D,i}= hy_{max,D,i}* \frac{CD_i}{(CD_i+K_{s,D,i})}
!! \f]
!! Dabei sind die Halbsättigungskonstanten Parameter, die vorgegeben werden müssen 
!! (APARAM.txt KsD1e 18|1, KsD2e 18|2).\n
!! Die Konzentrationen \f$ CD_i \f$, die hier verwendet werden, sind die aus dem vorangegangenen Zeitschritt, zu dem
!! bereits die \ref hyp hinzuaddiert wurde.
!! \n\n
!! Die maximale Hydrolyserate für die 1. leicht abbaubaren gelösten C-Verbindungen wird ebenfalls
!! vorgegeben (APARAM.txt hymxDe 17|5), die für die 2. schwer abbaubaren wird in Abhängigkeit
!! vom Verhältnis des biologischen zum chemischen Sauerstoffbedarf bestimmt:
!! \f{eqnarray*}{
!! hy_{max,D,1}=& const.  =hymxDe \\
!! hy_{max,D,2}=& min(0.474*(\frac{C-BSB_5}{CSB})^{-1.346},6)
!! \f}\n
!! Die Veränderungen werden explizit diskretisiert:
!! \f{eqnarray*}{
!! \Delta {CM}_{hyd} = \frac{\partial CM}{\partial t}_{hyd} \cdot \Delta t  \\
!! \Delta {CD_i}_{hyd} = \frac{\partial CD_i}{\partial t}_{hyd} \cdot \Delta t
!! \f}\n\n
!! Negative Werte werden ausgeschlossen, weil wo nichts ist, kann auch nichts verbraucht werden.
!! hartes Klipping bei:\f$ CD_i(t+\Delta t) > 0.00001 \f$ ist nicht massenerhaltend in Bezug auf :\f$ CM \f$
!! \n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CM \f$  </td><td> \ref CM </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2 </td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ hy_{D,i} \f$  </td><td> hyP(i) </td><td> Hydrolyserate für die gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> 1/d </td><td> ?? </td></tr>
!!<tr><td> \f$ f_T \f$  </td><td> ftemp </td><td> Faktor der Temperaturabhängigkeit, siehe: \ref hyp </td><td> - </td><td> 0.2 ... 1 </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> \ref BAC </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ hy_{max,D,i} \f$  </td><td> hymxD(i) \ref hymxDe </td><td> maximale Hydrolyserate für die gelösten organischen C-Verbindungen </td><td> 1/d </td><td> APARAM.txt hymxDe 17|5 </td></tr>
!!<tr><td> \f$ K_{s,D,i} \f$  </td><td> ksD(i) </td><td> Halbsättigungskonstante für die Hydrolyse der gelösten organischen C-Verbindungen </td><td> mgC/l </td><td> APARAM.txt KsD1e 18|1, KsD2e 18|2 </td></tr>
!!<tr><td> \f$ C-BSB_5 \f$  </td><td> \ref obsb </td><td> biologischer Sauerstoffbedarf  siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> chemischer Sauerstoffbedarf  siehe: \ref o2zehr</td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> \ref TFLIE </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n
!!<code>
!!       vcb =  \ref obsb / \ref ocsb \n
!!       hymxD(1) = \ref hymxDe \n
!!       hymxD(2) = 0.474*vcb**(-1.346)  \n
!!       if(hymxD(2)>6.)hymxD(2) = 6.  \n
!!       hyP(1) = \ref hyPe  \n
!!       hyP(2) = 1.51*vcb**2.31 \n
!!       ksD(1) = \ref KsD1e  \n
!!       ksD(2) = \ref KsD2e  \n
!!</code> 
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
! 
!> \page BACm Stoffwechsel der heterotrophen Bakterien
!! Die Zunahme der Masse von in heterotrophen Bakterien gespeicherten C-Verbindungen 
!! infolge der Aufnahme von monomolekularen C-Verbindungen
!! und der Rückgang infolge von Respiration (Atmung) wird wie folgt ermittelt:
!! \f[ 
!! \frac{\partial HBAC}{\partial t}_{BAC} = (up_{HBAC}-res_{HBAC})*HBAC
!! \f]
!! Im gleichen Maße wie die Masse des Kohlenstoff in heterotrophen Bakterien zunimmt, 
!! muss die Konzentration von monomolekularen organischen Kohlenstoff abnehmen:
!! \f[ 
!! \frac{\partial CM}{\partial t}_{BAC} = - up_{HBAC}*HBAC
!! \f]
!! Dabei berechnet sich die Aufnahmerate von Kohlenstoff durch heterotrophen Bakterien wie folgt:
!! \f[ 
!! up_{HBAC}= up_{HBAC,max}* \frac{CM}{(CM+K_{s,M})}
!! \f]
!! Die in obiger Formel auftretende maximale Aufnahmerate und die Halbsättigungskonstante
!! sind Parameter, die vorgegeben werden (APARAM.txt upBACe 18|4, KsMe 18|3)\n
!! Die Gesamtmasse der monomolekularen organischen C-Verbindungen \f$ CM \f$ , die von der \ref  hyd im
!! aktuellen Zeitschritt gebildet werden, sind für die Bakterien schon im aktuellen Zeitschritt verfügbar.
!! \n\n
!! Die Respirationsrate der heterotrophen Bakterien wird wie folgt ermittelt:
!! \f[ 
!! res_{HBAC}= res_{G,HBAC}*f_T+up_{HBAC}*(1-Y)
!! \f]
!! Die in obiger Formel auftretende Grundrespirationsrate und der Ertragskoeffizient
!! sind Parameter, die vorgegeben werden (APARAM.txt rsGBACe 19|1 , YBACe 18|5 )\n\n
!! Ausserdem wird die Massenzunahme der Bakterien durch das 
!! Vorhandensein von monomolekularen organischen C-Verbindungen limitiert.\n
!! \f[ 
!! up_{HBAC} < \frac{CM}{\Delta t * HBAC}
!! \f]\n
!! Die Veränderungen werden auch hier explizit diskretisiert:
!! \f{eqnarray*}{
!! \Delta {HBAC}_{BAC} = \frac{\partial HBAC}{\partial t}_{BAC} \cdot \Delta t \\
!! \Delta {CM}_{BAC} = \frac{\partial CM}{\partial t}_{BAC} \cdot \Delta t
!! \f}\n\n
!! Negative Werte werden ausgeschlossen, weil wo nichts ist, kann auch nichts verbraucht werden.
!! hartes Klipping bei:\f$ CM(t+\Delta t) > 0.00001 \f$ und \f$ HBAC(t+\Delta t) > 0.00001 \f$ 
!! ist nicht notwendigerweise massenerhaltend für die aufnehmende Größe.
!! \n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> \ref BAC </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ up_{HBAC} \f$  </td><td> upBAC </td><td> Aufnahmerate von Kohlenstoff durch heterotrophe Bakterien </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ up_{HBAC,max} \f$  </td><td> upBACm \ref upBACe </td><td> maximale Aufnahmerate für die Aufnahme von Kohlenstoff durch heterotrophe Bakterien</td><td>  1/d  </td><td> APARAM.txt upBACe 18|4  </td></tr>
!!<tr><td> \f$ k_{s,M} \f$  </td><td> ksM \ref KsMe </td><td> Halbsättigungskonstante für die Aufnahme von Kohlenstoff durch heterotrophe Bakterien</td><td> mgC/l </td><td> APARAM.txt KsMe 18|3 </td></tr>
!!<tr><td> \f$ res_{HBAC} \f$  </td><td> resbac </td><td> Respirationsrate der heterotrophen Bakterien </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ CM \f$  </td><td> \ref CM </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ Y \f$  </td><td> YBAC \ref YBACe </td><td> Ertragskoeffizient (Ernährungseffizienz) </td><td> - </td><td> APARAM.txt YBACe 18|5 </td></tr>
!!<tr><td> \f$ res_{G,HBAC} \f$  </td><td> \ref rsGBACe </td><td> Grundrespirationsrate bei Optimumstemperatur </td><td> 1/d </td><td>  APARAM.txt rsGBACe 19|1 </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> \ref TFLIE </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
!                                                                     
!> \page fluxmaphy Aufnahme gelöster C-Verbindungen durch Organismen auf Makrophyten
!! sessile Organismen, die sich auf Makrophyten (Wasserpflanzen) als Biofilm anlagern, entziehen dem
!! sie umgebenden Wasser gelöste C-Verbindungen. Diese Entnahme wird für die leicht und schwer abbaubare 
!! Fraktion der gelösten C-Verbindungen getrennt nach folgendem Ansatz (woher??) berechnet:
!!
!! \f[ FluxD_1 = (0.62*{(CD_1+CD_2)}^{0.817})*(0.62*log(\frac{C-BSB_5}{CSB})+2.2)  \f]
!! \f[ FluxD_2 = (0.56*{(CD_1+CD_2)}^{0.916})*(-3.11*(\frac{C-BSB_5}{CSB})+1.407)  \f]
!! \f[ \Delta {CD_i}_{pfl} = -FluxD_i \cdot \frac{pfl}{300 h} \cdot f_t \cdot \Delta t \f] 
!! Der gesamte orgc- Baustein basiert auf einer tiefengemittelten Betrachtungsweise, was hier besonders deutlich wird.
!! \n\n
!! Es wird angenommen, dass die Biofilme auf Makrophyten sich nur aus dem Teil der gelösten organischen C-Verbindungen
!! \f$ CD_i \f$ bedienen können, der ihnen von der \ref hyd übriggelassen wurde.
!! \n\n
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
!! mit \f$ i \f$ als Index der Abbaubarkeit  -  i=1: leicht abbaubar; i=2: schwer abbaubar\n
!!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
!
!> \page mortalgC Absterben von lebender Biomasse
!! Bei der absterbenden Algenbiomasse wird ein Kohlenstoffanteil von 48% angenommen.\n
!! Desweiteren wird angesetzt, dass sich die absterbende Algenbiomasse folgendermaßen in die verschiedenen
!! Kohlenstofffraktionen verteilt: 
!! <ul> <li> 10% in die 1-leicht abbaubaren gelösten organischen C-Verbindungen, </li>
!! <li> 10% in die 2-schwer abbaubaren gelösten organischen C-Verbindungen, </li>
!! <li> 35% in die 1-leicht abbaubaren partikulären organischen C-Verbindungen, </li>
!! <li> 35% in die 2-schwer abbaubaren partikulären organischen C-Verbindungen und </li>
!! <li> 10% in die refraktären organischen C-Verbindungen </li></ul>
!! \n\n
!! Bei der Absterbenden Biomasse aus Heterotrophen Naloflagelaten und Rotatorien
!! wird angesetzt, dass sie sich in die verschiedenen
!! Kohlenstofffraktionen wie folgt verteilen: 
!! <ul> <li> 20% in die 1-leicht abbaubaren gelösten organischen C-Verbindungen, </li>
!! <li> 20% in die 2-schwer abbaubaren gelösten organischen C-Verbindungen, </li>
!! <li> 20% in die 1-leicht abbaubaren partikulären organischen C-Verbindungen, </li>
!! <li> 20% in die 2-schwer abbaubaren partikulären organischen C-Verbindungen und </li>
!! <li> 20% in die refraktären organischen C-Verbindungen </li></ul>
!! Bei der absterbenden Rotatorienbiomasse wird ein Kohlenstoffanteil von 40% angenommen.\n
!! \n\n
!! dies führt auf folgende Formel \n (Exemplarisch anhand der leicht abbaubaren gelösten organischen C-Verbindungen)
!! \f{eqnarray*}{
!! {\Delta CD_1}_{mort} &=& \\
!! &+& 0.1 \cdot \left( dki_{mort} + dgr_{mort} + dbl_{mort}               \right) \cdot 0.48 \\
!! &+& 0.2 \cdot \left( BSB_{HNF} + Rot_{mort}  \cdot \frac{G_{Rot}}{1000} \right) \cdot 0.4
!! \f}\n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta CD_1 \f$  </td><td> - </td><td> Änderung an leicht abbaubaren gelösten C-Verbindungen infolge des Absterbens von Biomasse in einem Zeitschritt </td><td> mgC/l je Zeitschritt </td><td>  </td></tr>
!!<tr><td> \f$ BSB_{HNF} \f$  	</td><td> \ref BSBHNF </td><td> Absterben und Exkretion Heterotropher Naloflagelaten </td><td> mgC/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ dki_{mort} \f$  	</td><td> \ref dkimor </td><td> Absterberate Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ dgr_{mort} \f$  	</td><td> \ref dgrmor </td><td> Absterberate Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ dbl_{mort} \f$  	</td><td> \ref dblmor </td><td> Absterberate Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ Rot_{mort} \f$  	</td><td> \ref abszo </td><td> Absterberate der Rotatorien </td><td> Ind. je Liter und Zeitschritt </td><td>  </td></tr>
!!<tr><td> \f$ G_{Rot} \f$  	</td><td> \ref GRote </td><td> durchschnittliches Gewicht einer Rotatorie </td><td> µgBiom./Ind. </td><td>  </td></tr>
!!</table>\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
! 
!> \page Sediorgc Sedimentation kohlenstoffhaltigen Materials
!! Berechnungsverfahren dem Dokumentator noch unklar (wy 12dez12)\n \n 
!!!     einfluss der Sedimentation \n                                       
!!       g = sqrt(9.81)  \n 
!!       ust = (((1/rau(ior))*g)/(tiefe(ior)**0.16667))*abs(vmitt(ior))  \n 
!! \f[ 
!!    u_* = \sqrt{ \frac{g}{ (K_{st})^2 \cdot h^{1/3} }  }  \cdot \sqrt{(\bar{u})^2}
!! \f]
!!      ASEDC = 1.44E-6  \n 
!!      BSEDC = 3.13  \n 
!!      wsgr = 0.625*ust**2.1  \n 
!!      qsgr = 1./(1+asedc*exp(-bsedc*alog10(wsgr)))  \n 
!! \f[
!!    q_{s,gr} = \frac{1}{ 1 + (1.44 \cdot 10^{-6}) \cdot e^{ -3.13 \cdot \log_{10}( 0.625 \cdot {u_*}^{2.1}) } }
!! \f]
!!       qssed = (1+qsgr)/2. \n
!!       WS = (LOG(1./QSSED-1)-LOG(ASEDC))/(-BSEDC) \n
!!       WS = 10**WS \n
!!       fwst = 1.14*exp(-188.2*ust) \n
!!       if(fwst>1.0)fwst = 1. \n
!!       wst = ws*fwst \n
!! \f[
!!    w_* = \left(1.14 \cdot  e^{(-188.2 \cdot u_*)} \right) \cdot 
!! 10^{ \left( \log( \frac{2.}{1+q_{s,gr}}-1) - \log(1.44 \cdot 10^{-6})\right) / -3.13 }
!! \f]
!!       prop = 0.6  \n
!!       OC = 1./(EXP(prop*WST*TFLIE*86400./TIEFE(ior)))  \n
!!       OC = 1.-OC  \n
!!       CP1sd = fbsgr(ior)*CP(1,ior)  \n
!!       Ceq1 = CP1sd*qsgr  \n
!!       CP1sdt = CP1sd-Ceq1  \n
!!       sedCP1 = CP1sdt*oc  \n
!!       CPt(1) = CPt(1)-sedCP1  \n  
!! exemplarisch:                                                              
!! \f{eqnarray*}{
!! sedCP_1 &=& 
!! - \frac{ \left(f_{bs,gr} \cdot CP_1 \cdot q_{s,gr}\right)}{\Delta } \cdot 
!!   \left(1.- 1./e^{(0.6 \cdot w_* \cdot \Delta t \cdot 86400 / h)} \right) \\
!! sedCP_2 &=& 
!! - \frac{ \left(f_{bs,gr} \cdot CP_2 \cdot q_{s,gr}\right)}{\Delta } \cdot 
!!   \left(1.- 1./e^{(0.6 \cdot w_* \cdot \Delta t \cdot 86400 / h)} \right) \\
!! sedBAC &=& 
!! - \frac{ \left(f_{bs,gr} \cdot BAC \cdot q_{s,gr}\right)}{\Delta } \cdot 
!!   \left(1.- 1./e^{(0.6 \cdot w_* \cdot \Delta t \cdot 86400 / h)} \right) \\
!! sedC_{ref} &=& 
!!   andere \cdot Werte + sonst + wie + oben \\
!! \\
!! \f}
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
!! <h2> Neuberechnung des Faktors zur Berechnung der ablagerungsfreien Grenzkonzentration</h2> 
!!       hc1 = CP(2,ior)-sedCP2+0.2*BSBHNF(ior)+0.2*dkimor(ior)*0.48+0.2*dgrmor(ior)*0.483+0.2*dblmor(ior)*0.48 \n                      
!!       hc1 = hc1+0.2*abszo(ior)*(GRote/1000.)*0.4 \n
!!       hc1 = hc1+0.2*zexki(ior)*0.48+0.2*zexgr(ior)*0.483+0.2*drfaek(ior)*0.48+0.2*drfaeg(ior)*0.483 \n                      
!!       hc1 = hc1+0.2*zexbl(ior)*0.48+0.2*drfaeb(ior)*0.48\n
!!       hc1 = hc1-(ssdr(ior)*0.3*0.4*(CP(2,ior)/(CP(1,ior)+CP(2,ior)+0.1*Cref)))   \n                    
!!                                                                        \n
!!       hc2 = CP2sd-sedCP2+0.2*BSBHNF(ior)+0.2*dkimor(ior)*0.48+0.2*dgrmor(ior)*0.483+0.2*dblmor(ior)*0.48\n                       
!!       hc2 = hc2+0.2*abszo(ior)*(GRote/1000.)*0.4 \n
!!       hc2 = hc2+0.2*zexki(ior)*0.48+0.2*zexgr(ior)*0.483+0.2*drfaek(ior)*0.48+0.2*drfaeg(ior)*0.483 \n                      
!!       hc2 = hc2+0.2*zexbl(ior)*0.48+0.2*drfaeb(ior)*0.48 \n
!!       hc2 = hc2-(ssdr(ior)*0.3*0.4*(CP(2,ior)/(CP(1,ior)+CP(2,ior)+0.1*Cref)))    \n                   
!!                        \n                                                
!!       fbsgrt = hc2/hc1 \n
!! \n \n
!!       hc1 = Cref-sedCrf+0.2*BSBHNF(ior)+0.2*dkimor(ior)*0.48+0.2*dgrmor(ior)*0.483+0.2*dblmor(ior)*0.48   \n                     
!!       hc1 = hc1+0.2*abszo(ior)*(GRote/1000.)*0.4  \n
!!       hc1 = hc1+0.2*zexki(ior)*0.48+0.2*zexgr(ior)*0.483+0.2*drfaek(ior)*0.48+0.2*drfaeg(ior)*0.483     \n                   
!!       hc1 = hc1+0.2*zexbl(ior)*0.48+0.2*drfaeb(ior)*0.48  \n
!!       hc1 = hc1-(ssdr(ior)*0.3*0.4*(cref/(CP(1,ior)+CP(2,ior)+0.1*Cref)))                         \n    
!!                                                                         \n
!!       hc2 = Crfsd-sedCrf+0.2*BSBHNF(ior)+0.2*dkimor(ior)*0.48+0.2*dgrmor(ior)*0.483+0.2*dblmor(ior)*0.48    \n                    
!!       hc2 = hc2+0.2*abszo(ior)*(GRote/1000.)*0.4  \n
!!       hc2 = hc2+0.2*zexki(ior)*0.48+0.2*zexgr(ior)*0.483+0.2*drfaek(ior)*0.48+0.2*drfaeg(ior)*0.483    \n                    
!!       hc2 = hc2+0.2*zexbl(ior)*0.48+0.2*drfaeb(ior)*0.48  \n
!!       hc2 = hc2-(ssdr(ior)*0.3*0.4*(cref/(CP(1,ior)+CP(2,ior)+0.1*Cref)))   \n                          
!!                                                                    \n     
!!      frfgrt = hc2/hc1 \n 
!! \n 
!! zurück zu: \ref BSB     , Quelle: orgc_huelle.f95                                                                 
!
!> \page Cschwebdreiss Schwebstoffaufnahme der Dreissena Muscheln   
!! Dieser Teilprozess ist momentan ausgeschaltet, weil in
!! dreissen.f90: ".....Schwebstoffaufnahme durch Dreissena wird vorläufig auf Null gesetz". \n 
!! Teil-Prozess momentan fachlich unklar.\n\n
!! Die hier codierte Formel würde annehmen, dass von dem durch Dreissena-Muscheln aufgenommenen Schwebstoff
!! 30% partikuläre C-Verbindungen sind und dass die Muscheln davon 60% ungenutzt wieder ausscheiden (oder andersrum).
!! Vom refraktären Kohlenstoff wird angenommen, dass nur 10% partikulär vorliegen.\n
!! \f[ 
!!\Delta {CP_1}_{drs} = - ssdr \cdot 0.3 \cdot 0.4 \cdot \frac{CP_1}{CP_1 + CP_2 + 0.1 \cdot C_{ref}}
!! \f]
!! \f[ 
!!\Delta {CP_2}_{drs} = - ssdr \cdot 0.3 \cdot 0.4 \cdot \frac{CP_2}{CP_1 + CP_2 + 0.1 \cdot C_{ref}}
!! \f]
!! \f[ 
!!\Delta {C_{ref}}_{drs} = - ssdr \cdot 0.3 \cdot 0.4 \cdot \frac{C_{ref}}{CP_1 + CP_2 + 0.1 \cdot C_{ref}}
!! \f]
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta {CP_i}_{drs} \f$  </td><td> CD(i = \ref cd1+\ref cd2 </td><td> Änderung an partikulären C-Verbindungen in einem Zeitschritt </td><td> mgC/l je Zeitschritt </td><td>  </td></tr>
!!<tr><td> \f$ i \f$  </td><td> - </td><td> Index 1 leicht abbaubar, 2 schwer abbaubar </td><td> - </td><td> 1,2 </td></tr>
!!<tr><td> \f$ \Delta {C_{ref}}_{drs} \f$  </td><td> - </td><td> Änderung an refrakteren C-Verbindungen in einem Zeitschritt  </td><td>  mgC/l je Zeitschritt  </td><td>  </td></tr>
!!<tr><td> \f$ ssdr \f$  </td><td> \ref ssdr </td><td> Schwebstoffaufnahme durch Dreissena </td><td>  mg/l je Zeitschritt  </td><td>  </td></tr>
!!</table>\n\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
! 
!> \page bacHNFgraz Verlust der Bakterien durch HNF-Grazing 
!! Heterotrophe Nanoflagelaten konsumieren (fressen, engl.: to graze) Bakterien.
!! Die Masse an C-Verbindungen in Bakterien, die dadurch je Zeitschritt verloren geht,
!! wird in der Subroutine HNF() ermittelt. \n                          
!! \f[ 
!! \Delta {HBAC}_{HNF} = - HNFBAC
!! \f]
!! Klipping bei:\f$ HBA(t+\Delta t) > 0.00001 \f$
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> \ref BAC </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ HNFBAC \f$  </td><td> \ref HNFBAC </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen, die in jedem Zeitschritt infolge Wegfraß durch heterotrophe Nanoflagelaten verloren geht </td><td> mgC/l (zeitschrittbezogen) </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> \ref TFLIE </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n
!! zurück zu: \ref BSB
! 
!> \page facesC Ausscheidungen von Zooplanktern und Dreissena-Muscheln
!! Verbunden mit der Nahrungsaufnahme exkretieren Zooplankter (Rotatorien) und Dreissena-Muscheln
!! sogenannte Faeces. Genauso wie bei der absterbenden Algenbiomasse wird davon ausgegangen, dass diese Faeces
!! ein Kohlenstoffanteil von 48% haben. Und ebenfalls genauso wie bei der absterbenden Algenbiomasse wird die 
!! Verteilung in die verschiedenen Kohlenstofffraktionen angesetzt: \n
!! <ul> <li> 10% in die 1-leicht abbaubaren gelösten organischen C-Verbindungen, </li>
!! <li> 10% in die 2-schwer abbaubaren gelösten organischen C-Verbindungen, </li>
!! <li> 35% in die 1-leicht abbaubaren partikulären organischen C-Verbindungen, </li>
!! <li> 35% in die 2-schwer abbaubaren partikulären organischen C-Verbindungen und </li>
!! <li> 10% in die refraktären organischen C-Verbindungen </li></ul>\n
!! Untenstehend sei hier exemplarisch für alle Kohlenstofffraktionen nur die Formel für die
!! 1-leicht abbaubaren partikulären organischen C-Verbindungen angegeben:\n
!! \f{eqnarray*}{
!! \Delta {CP_1}_{faec} &=& 0.35 \cdot \\
!! &[& \\
!! &+& 0.48 \cdot \left( ROT_{faec,Ki} + ROT_{faec,Gr} + ROT_{faec,Bl}\right)  \\
!! &+& 0.48 \cdot \left( DR_{faec,Ki} + DR_{faec,Gr} + DR_{faec,Bl}   \right)  \\
!! &]& \\
!! \f}
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
!! zurück zu: \ref BSB  , Quelle: orgc_huelle.f95                                                                    
! 
! -----------------------------------------------------------------------------------------------Auswirkungen----------
!> \page o2zehr Sauerstoff-Verbrauch
!! <h1> aktuell stattfindender Sauerstoff-Verbrauch </h1>
!! bei den folgenden Prozessen beim Umsatz von C-Verbindungen kommt es zum Verbrauch von Sauerstoff:
!! <ul> <li> \ref BACm und </li>
!! <li> \ref fluxmaphy. </li></ul>\n\n
!! \f[ 
!!    \Delta {O_2}_{BAC} = res_{HBAC} \cdot HBAC \cdot O_2dC
!! \f]
!! \f[ 
!!   \Delta {O_2}_{pfl} = \left(0.758 \left(CD_1 + CD_2 \right) + 0.21 \right)  \cdot  
!!        \left( -5.476 \left(\frac{C-BSB_5}{CSB}\right)^2 + 2.256 \frac{C-BSB_5}{CSB} + 0.789 \right)
!!        \cdot   ( \frac{pfl}{300  \cdot h} \cdot f_T )
!! \f]
!! Allerdings scheint der obenstehende Ansatz für den Sauerstoffverbrauch nicht mit den Ansätzen zur \ref fluxmaphy 
!! zusammenzupassen.
!! \f[ 
!!      \Delta {O_2}_{orgC} = \Delta {O_2}_{BAC} + \Delta {O_2}_{pfl}
!! \f]
!! 
!! <h1> Zehrungsfähigkeit der C-Verbindungen </h1>
!! 
!! <h2> biologischer Sauerstoffbedarf in 5 Tagen </h2>
!! Bei den hier als leicht zehrbar bilanzierten C-Verbindungen wird davon ausgegangen, dass mehr als 92% in 5 Tagen
!! biologisch abgebaut werden. Die monomolekularen C-Verbindungen
!! und 40% der in Bakterien und Nanoflagelaten gespeicherten C-Verbindungen werden ebenfalls 
!! als leicht abbaubar angesetzt.\n
!! Bei den  hier als schwer abbaubar bilanzierten C-Verbindungen wird davon ausgegangen, dass sich nur knapp 10%
!! in 5 Tagen biologisch abbauen lassen. 40% der in Bakterien und Nanoflagelaten gespeicherten C-Verbindungen werden
!! als schwer abbaubar angenommen.\n
!! \f{eqnarray*}{
!! C-BSB_5 &=& O_2dC \cdot \\
!! &[& \\
!! &+& 0.922 \cdot \left[ CD_1 + CP_1 + CM + 0.4 \cdot \left( HBAC + C_{HNF} \right) \right]  \\
!! &+& 0.095 \cdot \left[ CD_2 + CP_2      + 0.4 \cdot \left( HBAC + C_{HNF} \right) \right]  \\
!! &]& \\
!! \f}
!!                                                                   
!! <h2> chemischer Sauerstoffbedarf</h2>
!! Nahezu der gesamte im Wasserenthaltene Kohlenstoff kann auf Chemischem Wege oxidiert werden.\n
!! Lediglich bei dem in Bakterien und Nanoflagelaten gebundenen Kohlenstoff wird davon ausgegangen,
!! dass nur 80% chemisch zehrungsfähig ist, was der Summe des biologisch abbaubaren Materials entspricht.\n
!! Fernerhin wird angenommen, dass ein kleiner Teil des chemischen Sauerstoffbedarfs nicht zur Bildung von 
!! Kohlendioxid verwendet wird (2.8 statt 2.667). \n
!! Die Neuberechnung erfasst den Zustand nach erfolgtem Stoffumsatz. D. h. \f$ CD_1 = CD_1(t+\Delta t)\f$
!! \f[ 
!!      CSB = \left[ C_{ref} + CD_1 + CD_2 + CP_1 +CP_2 + CM + 0.8 \cdot \left( HBAC + C_{HNF} \right) \right] \cdot 2.8 
!! \f]
!! \n\n
!! mit:
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta {O_2}_{orgC} \f$  </td><td> \ref bsbt </td><td> Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt </td><td> mgO2/l zeitschrittbezogen </td><td> ? </td></tr>
!!<tr><td> \f$ res_{HBAC} \f$  </td><td> resbac </td><td> Respirationsrate der heterotrophen Bakterien, siehe: \ref BACm </td><td> 1/d </td><td> ? </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> \ref BAC </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ O_2dC \f$  </td><td> \ref o2bsb </td><td> Sauerstoff-Kohlenstoffverhältnis beim C-Abbau </td><td> mgO2/mgC </td><td>  </td></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2 </td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ C-BSB_5 \f$  </td><td> \ref obsb </td><td> - </td><td> - </td><td> - </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> Kohlenstoffbürtiger chemischer Sauerstoffbedarf </td><td> mgO2/l </td><td> ? </td></tr>
!!<tr><td> \f$ pfl \f$  </td><td> \ref pfl </td><td> Pflanzentrockengewicht Makrophyten </td><td> g/m² </td><td>  </td></tr>
!!<tr><td> \f$ h \f$  </td><td> \ref TIEFE </td><td> Wassertiefe </td><td> m </td><td> 0 ... ca. 25 </td></tr>
!!<tr><td> \f$ f_T \f$  </td><td> ftemp </td><td> Faktor der Temperaturabhängigkeit, siehe: \ref hyp </td><td> - </td><td> 0.2 ... 1 </td></tr>
!!<tr><td> \f$ C_{ref} \f$  </td><td> Cref </td><td> Konzentration der refraktären (nicht abbaubaren) C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CM \f$  </td><td> \ref CM </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ C_{HNF} \f$  </td><td> \ref CHNF </td><td> C-Masse der heterotrophen Nanoflagelaten </td><td> mgC/l </td><td> ? </td></tr>
!!</table>\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
! 
!> \page nh4freis Ammonium-Freisetzung
!! <h2> Freisetzung von Ammonium </h2>
!! Bei der Umwandlung von C-Verbindungen in Kohlendioxyd
!! wird der in ersteren gebundene Stickstoff als Ammonium freigesetzt:
!! \f[ 
!!    BSB_{NH4} = BSB_{C} \cdot N_{org}
!! \f]
!! \n\n
!! <h2> Neuberechnung Stickstoffgehalt </h2>
!! Beim \ref mortalgC Absterben von lebender Biomasse 
!! und bei den \ref facesC Ausscheidungen von Zooplanktern und Dreissena-Muscheln
!! geht nicht nur deren Kohlenstoffanteil in die hier bilanzierten C-Verbindungen über,
!! sondern es wird auch deren Stickstoffgehalt freigesetzt. Da diese aus Biomasse mit unterschiedlichen 
!! Stickstoffgehalten stammen, muss hier eine Neuberechnung des Gesamt-Stickstoffgehalts der 
!! organischen C-Verbindungen stattfinden:
!! \f{eqnarray*}{
!!    orgN(t + \Delta t) &=& orgN(t) \\
!!                       &+& BSB_{HNF}  \cdot  f_N \\
!!                       &+& dki_{mort} \cdot Q_{N,Ki} + dgr_{mort} \cdot Q_{N,Gr} + dbl_{mort} \cdot Q_{N,Bl}\\
!!                       &+& Rot_{mort} \cdot (G_{Rot}/1000.) \cdot N_R\\
!!                       &+& ROT_{faec,Ki} \cdot Q_{N,Ki} + ROT_{faec,Gr} \cdot Q_{N,Gr} + ROT_{faec,Bl} \cdot Q_{N,Bl}\\
!!                       &+& DR_{faec,Ki} \cdot Q_{N,Ki} + DR_{faec,Gr} \cdot Q_{N,Gr} + DR_{faec,Bl}  \cdot Q_{N,Bl}\\
!! \f}
!! \n
!! Das Verhältnis von Stickstoff zu Kohlenstoff ergibt sich dann aus der Division des Gesamt-Stickstoffgehalts 
!! durch den Kohlenstoffanteil im CSB.   (2.8 mgC/mgO2)         
!! \f[ 
!!      N_{org}(t + \Delta t) = \frac{orgN(t + \Delta t)}{CSB(t + \Delta t)/2.8}
!! \f]
!! mit:
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ BSB_{NH4} \f$  </td><td> \ref doN </td><td> Ammoniumfreisetzung infolge Kohlenstoffmineralisierung </td><td> mgN/l je Zeitschritt</td><td> ? </td></tr>
!!<tr><td> \f$ BSB_{C} \f$  </td><td> \ref bsbct </td><td> zu CO2 mineralisierter Kohlenstoff, siehe: \ref o2zehr (### hier stimmt was nicht ###)</td><td> mgC/l je Zeitschritt</td><td> ? </td></tr>
!!<tr><td> \f$ N_{org} \f$  </td><td>\ref  nl0 </td><td> Verhältnis von Stickstoff zu Kohlenstoff in organischem Material </td><td> mgN/mgC </td><td> ? </td></tr>
!!<tr><td> \f$ orgN \f$  </td><td> orgn </td><td> Gesamt-Stickstoffgehalt im organischem Material </td><td> mgN/l </td><td> ? </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> Kohlenstoffbürtiger chemischer Sauerstoffbedarf </td><td> mgO2/l </td><td> ? </td></tr>
!!<tr><td> \f$ BSB_{HNF} \f$  	</td><td> \ref BSBHNF </td><td> Absterben und Exkretion Heterotropher Naloflagelaten </td><td> mgC/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ f_N \f$  	</td><td> ## fehlt </td><td> zu implementierendes Verhältnis </td><td> mgN/mgC </td><td> ? </td></tr>
!!<tr><td> \f$ dki_{mort} \f$  	</td><td> \ref dkimor </td><td> Absterberate Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ dgr_{mort} \f$  	</td><td> \ref dgrmor </td><td> Absterberate Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ dbl_{mort} \f$  	</td><td> \ref dblmor </td><td> Absterberate Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{N,Ki} \f$  </td><td> \ref Q_NK </td><td> Stickstoffanteil in der Biomasse der Kieselalgen </td><td> mgN/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{N,Gr} \f$  </td><td> \ref Q_NG </td><td> Stickstoffanteil in der Biomasse der Grünalgen </td><td> mgN/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{N,Bl} \f$  </td><td> \ref Q_NB </td><td> Stickstoffanteil in der Biomasse der Blaualgen </td><td> mgN/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Rot_{mort} \f$  	</td><td> \ref abszo </td><td> Absterberate der Rotatorien </td><td> Ind. je Liter und Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ G_{Rot} \f$  	</td><td> \ref GRote </td><td> durchschnittliches Gewicht einer Rotatorie </td><td> µgBiom./Ind. </td><td> ? </td></tr>
!!<tr><td> \f$ N_R \f$  	</td><td> \ref nzoo </td><td> Stickstoffanteil in der Rotatorienbiomasse </td><td> mgN/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Ki} \f$  </td><td> \ref zexki </td><td> Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Gr} \f$  </td><td> \ref zexgr </td><td> Ausscheidungen der Rotatorien infolge Konsums von Grünalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Bl} \f$  </td><td> \ref zexbl </td><td> Ausscheidungen der Rotatorien infolge Konsums von Blaualgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Ki} \f$  </td><td> \ref drfaek </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Kieselalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Gr} \f$  </td><td> \ref drfaeg </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Bl} \f$  </td><td> \ref drfaeb </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!</table>\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
! 
!> \page pfreis Phosphat Freisetzung
!! <h2> Freisetzung von ortho-Phospshat </h2>
!! Bei der Umwandlung von C-Verbindungen in Kohlendioxyd
!! wird der in ersteren gebundene Phosphor als ortho-Phosphat freigesetzt:
!! \f[ 
!!    BSB_{PO4} = BSB_{C} \cdot P_{org}
!! \f]
!! \n
!! <h2> Neuberechnung Phosphatgehalt </h2>
!! Beim \ref mortalgC Absterben von lebender Biomasse 
!! und bei den \ref facesC Ausscheidungen von Zooplanktern und Dreissena-Muscheln
!! geht nicht nur deren Kohlenstoffanteil in die hier bilanzierten C-Verbindungen über,
!! sondern es wird auch deren Phosphatgehalt freigesetzt. Da diese aus Biomasse mit unterschiedlichen 
!! Phosphatgehalten stammen, muss hier eine Neuberechnung des Gesamt-Phosphatgehalts der 
!! organischen C-Verbindungen stattfinden:
!! \f{eqnarray*}{
!!    orgP(t + \Delta t) &=& orgP(t) \\
!!                       &+& BSB_{HNF}  \cdot  f_P \\
!!                       &+& dki_{mort} \cdot Q_{P,Ki} + dgr_{mort} \cdot Q_{P,Gr} + dbl_{mort} \cdot Q_{P,Bl}\\
!!                       &+& Rot_{mort} \cdot (G_{Rot}/1000.) \cdot P_R\\
!!                       &+& ROT_{faec,Ki} \cdot Q_{P,Ki} + ROT_{faec,Gr} \cdot Q_{P,Gr} + ROT_{faec,Bl} \cdot Q_{P,Bl}\\
!!                       &+& DR_{faec,Ki} \cdot Q_{P,Ki} + DR_{faec,Gr} \cdot Q_{P,Gr} + DR_{faec,Bl}  \cdot Q_{P,Bl}\\
!! \f}
!! \n
!! Das Verhältnis von Phosphor zu Kohlenstoff ergibt sich dann aus der Division des Gesamt-Phosphorgehalts 
!! durch den Kohlenstoffanteil im CSB.   (2.8 mgC/mgO2)         
!! \f[ 
!!      P_{org} (t + \Delta t) = \frac{orgP(t + \Delta t)}{CSB(t + \Delta t)/2.8}
!! \f]
!! mit:
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ BSB_{PO4} \f$  </td><td> \ref bsbctP </td><td> ortho-Phosphat-Freisetzung infolge Kohlenstoffmineralisierung </td><td> mgP/l je Zeitschritt</td><td> ? </td></tr>
!!<tr><td> \f$ BSB_{C} \f$  </td><td> \ref bsbct </td><td> zu CO2 mineralisierter Kohlenstoff, siehe: \ref o2zehr (### hier stimmt was nicht ###)</td><td> mgC/l je Zeitschritt</td><td> ? </td></tr>
!!<tr><td> \f$ P_{org} \f$  </td><td> \ref pl0 </td><td> Verhältnis von Phosphor zu Kohlenstoff in organischem Material </td><td> mgP/mgC zeitschrittbezogen </td><td> ? </td></tr>
!!<tr><td> \f$ orgP \f$  </td><td> orgp </td><td> Gesamt-Phosphorgehalt im organischem Material </td><td> mgP/l </td><td> ? </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> Kohlenstoffbürtiger chemischer Sauerstoffbedarf </td><td> mgO2/l </td><td> ? </td></tr>
!!<tr><td> \f$ BSB_{HNF} \f$  	</td><td> \ref BSBHNF </td><td> Absterben und Exkretion Heterotropher Naloflagelaten </td><td> mgC/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ f_P \f$  	</td><td> ## fehlt </td><td> zu implementierendes Verhältnis </td><td> mgP/mgC </td><td> ? </td></tr>
!!<tr><td> \f$ dki_{mort} \f$  	</td><td> \ref dkimor </td><td> Absterberate Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ dgr_{mort} \f$  	</td><td> \ref dgrmor </td><td> Absterberate Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ dbl_{mort} \f$  	</td><td> \ref dblmor </td><td> Absterberate Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{P,Ki} \f$  </td><td> \ref Q_PK </td><td> Phosphoranteil in der Biomasse der Kieselalgen </td><td> mgP/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{P,Gr} \f$  </td><td> \ref Q_PG </td><td> Phosphoranteil in der Biomasse der Grünalgen </td><td> mgP/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Q_{P,Bl} \f$  </td><td> \ref Q_PB </td><td> Phosphoranteil in der Biomasse der Blaualgen </td><td> mgP/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ Rot_{mort} \f$  	</td><td> \ref abszo </td><td> Absterberate der Rotatorien </td><td> Ind. je Liter und Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ G_{Rot} \f$  	</td><td> \ref GRote </td><td> durchschnittliches Gewicht einer Rotatorie </td><td> µgBiom./Ind. </td><td> ? </td></tr>
!!<tr><td> \f$ P_R \f$  	</td><td> \ref pZoo </td><td> Phosphoranteil in der Rotatorienbiomasse </td><td> mgP/mgBiom. </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Ki} \f$  </td><td> \ref zexki </td><td> Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Gr} \f$  </td><td> \ref zexgr </td><td> Ausscheidungen der Rotatorien infolge Konsums von Grünalgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ ROT_{faec,Bl} \f$  </td><td> \ref zexbl </td><td> Ausscheidungen der Rotatorien infolge Konsums von Blaualgen </td><td> mgBiom./l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Ki} \f$  </td><td> \ref drfaek </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Kieselalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Gr} \f$  </td><td> \ref drfaeg </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Grünalgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!<tr><td> \f$ DR_{faec,Bl} \f$  </td><td>\ref  drfaeb </td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen </td><td>  mgBiom./l je Zeitschritt  </td><td> ? </td></tr>
!!</table>\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
! 
!> \page  kohlensauer Änderung des ph-Werts durch Bildung von Kohlensäure
!! Das beim Abbau von C-Verbindungen entstehende Kohlendioxyd (CO2) verändert den \ref PH-Wert.
!! Dort wird die CO2 Lieferung aus C-abbau aus \f$ \Delta O_2 \f$ bestimmt, das hier im 
!! Zusammenhang mit dem \ref o2zehr berechnet wurde. \n
!!<table>
!!<tr><td> \f$ \Delta O_2 \f$  </td><td> \ref bsbt </td><td> Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt </td><td> mgO2/l zeitschrittbezogen </td><td> ? </td></tr>
!!</table>\n\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
!
!> \page schwebkohl Schwebstoffgehalt
!! Zur Schwebstoffmasse tragen die partikulär vorliegenden C-Verbindungen bei, dies sind 
!! \f$ CP_i \f$, \f$ HBAC \f$ und 20% von \f$ C_{ref} \f$
!! ( hier ergibt sich ein Widerspruch mit der Annahme von 10% bei: \ref Cschwebdreiss .\n
!! Es wird fernerhin angesetzt, dass Kohlenstoff die Hälfte dieser Schwebstoffmasse ausmacht.\n
!! \f{eqnarray*}{
!!    \Delta SS_{org} &=& 2 \cdot \\
!!                   &[&  \\
!!                   &+& CP_1(t+\Delta t) - CP_1(t) + sedCP_1 \\
!!                   &+& CP_2(t+\Delta t) - CP_2(t) + sedCP_2 \\
!!                   &+& HBAC(t+\Delta t) - HBAC(t) + sedBAC \\
!!                   &+& 0.2 \cdot \left( C_{ref}(t+\Delta t) - C_{ref}(t) + sedC_{ref} \right) \\
!!                   &]& 
!! \f}
!! Wie obige Formel zeigt, sind in der Größe \f$ \Delta SS_{org} \f$, die an schweb() übergeben wird,
!!  die sedimentierten Partikel mit enthalten !  \n\n
!!<table>
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ \Delta SS_{org} \f$  </td><td>  \ref dorgSS </td><td> Veränderung von suspendierten Sedimenten aus C-Verbindungen </td><td> mg SS /l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> \ref BAC </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ C_{ref} \f$  </td><td> Cref </td><td> Konzentration der refraktären (nicht abbaubaren) C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!</table>\n\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
!
!> \page sedkohl Sedimentation
!! Die Gesamtmasse des sedimentierten Kohlenstoffs wird aufsummiert und nachfolgenden Modulen zur Verfügung gestellt:
!! \ref Stickstoff , \ref Phosphor , SedFlux().\n
!! \f[ 
!!    orgC_{sed} = sedCP_1 + sedCP_2 + sedBAC + sedC_{ref} 
!! \f]
!!<table>
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ orgC_{sed} \f$  </td><td> \ref orgCsd </td><td> Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert </td><td> mgC/l je Zeitschritt </td><td> ? </td></tr>
!!<tr><td> \f$ sedCP_1 \f$, \f$sedCP_2 \f$, \f$sedBAC \f$, \f$sedC_{ref} \f$  </td><td> sedCP1, sedCP2, sedBAC, sedCrf </td><td> siehe \ref Sediorgc </td><td>  mgC/l je Zeitschritt  </td><td> ? </td></tr>
!!</table>\n\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
! ----------------------------------------------------------------------------------------------Bilanzen-----------------------
!> \page bilaCP Konzentration von partikulären organischen C-Verbindungen
!! Die Konzentration der partikulären organischen C-Verbindungen
!! ändert sich aufgrund der folgenden Teilprozesse: 
!! <ul> <li> \ref hyp, </li>
!! <li> \ref Sediorgc, </li>
!! <li> \ref mortalgC, </li>
!! <li> \ref Cschwebdreiss (z.Z ausgeschaltet), </li>
!! <li> \ref facesC. </li></ul>\n\n
!! Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
!! plus der Summe der Veränderungen:
!! \f[ 
!! CP_i(t+\Delta t) = CP_i(t) 
!! + \Delta {CP_i}_{hyp} + \Delta {CP_i}_{sed}
!! + \Delta {CP_i}_{mort} + \Delta {CP_i}_{drs} + \Delta {CP_i}_{faec} 
!! \f]
!! Negative Werte werden ausgeschlossen, zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
!! welches nicht massenerhaltend ist.\n
!! \f[ 
!! CP_i(t+\Delta t)_{klipp}= \frac{CP_i(t)}{ 2 + \left|CP_i(t+\Delta t)/CP_i(t)\right|} 
!! \f]
!! mit:
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> tflie </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
!! 
!
!> \page bilaCD  Konzentrationen von gelöstenen organischen C-Verbindungen
!! Die Konzentrationen von gelöstenen organischen C-Verbindungen ändert sich aufgrund der folgenden Teilprozesse: 
!! <ul> <li> \ref hyp, </li>
!! <li> \ref hyd, </li>
!! <li> \ref fluxmaphy, </li>
!! <li> \ref mortalgC und </li>
!! <li> \ref facesC. </li></ul>\n\n
!! Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
!! plus der Summe der Veränderungen:
!! \f[ 
!! CD_i(t+\Delta t) = CD_i(t)
!! + (\Delta CD_i)_{hyp} + (\Delta CD_i)_{hyd} + (\Delta CD_i)_{pfl} + (\Delta CD_i)_{mort} + (\Delta CD_i)_{faec}
!! \f]
!! Negative Werte werden ausgeschlossen. Zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
!! welches auch nicht massenerhaltend ist.\n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2</td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> \ref TFLIE </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
!
!> \page bilaCM Konzentration von monomolekularen organischen Kohlenstoff
!! Die Konzentration von monomolekularen organischem Kohlenstoff
!! ändert sich aufgrund der folgenden Teilprozesse: 
!! <ul> <li> \ref hyd, </li>
!! <li> \ref BACm, </li>
!! <li> \ref mortalgC und </li>
!! <li> \ref facesC. </li></ul>\n\n
!! Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
!! plus der Summe der Veränderungen:
!! \f[ 
!! CM(t+\Delta t) = CM(t) + \Delta {CM}_{hyd} + \Delta {CM}_{BAC} + (\Delta CM)_{mort} + (\Delta CM)_{faec} 
!! \f]
!! Negative Werte werden ausgeschlossen. Zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
!! welches auch nicht massenerhaltend ist.\n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ CM \f$  </td><td> \ref CM </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> tflie </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n
!! zurück zu: \ref BSB
!
!> \page bilaBAC Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
!! Die Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
!! ändert sich aufgrund der folgenden Teilprozesse: 
!! <ul> <li> \ref BACm, </li>
!! <li> \ref bacHNFgraz und </li>
!! <li> \ref Sediorgc. </li></ul>\n\n
!! Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
!! plus der Summe der Veränderungen:
!! \f[ 
!! HBAC(t+\Delta t) = HBAC(t) + \Delta {HBAC}_{BAC} + \Delta {HBAC}_{HNF} + \Delta {HBAC}_{sed}
!! \f]
!! Negative Werte werden ausgeschlossen. Zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
!! welches auch nicht massenerhaltend ist.\n
!! Abgesehen von Massenverlusten durch Respiration, HNF-Grazing und Sedimentation sind Bakterien unsterblich.
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> \ref BAC </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> \ref TFLIE </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n
!! zurück zu: \ref BSB , Quelle: orgc_huelle.f95
!
!> \page bilaCref refraktäre (nicht abbaubare) C-Verbindungen
!! Die Konzentration der refraktären (nicht abbaubaren) C-Verbindungen
!! ändert sich aufgrund der folgenden Teilprozesse: 
!! <ul> <li> \ref facesC, </li>
!! <li> \ref Cschwebdreiss (z.Z ausgeschaltet), </li>
!! <li> \ref Sediorgc, </li>
!! <li> \ref mortalgC und </li>
!! <li> \ref facesC. </li></ul>\n\n
!! zu Beginn der Berechnungen in orgc() wird der Gehalt an refraktären (nicht abbaubaren) C-Verbindungen
!! aus dem chemischen Sauerstoffbedarf durch Abzug aller anderen C-Verbindungen rückgerechnet
!! (Dies ist notwendig, weil \f$ C_{ref} \f$ keine transportierte, planktische Feldgröße ist).
!! \f[ 
!!      C_{ref} = \frac{1}{2.8} \cdot CSB - CP_1 - CP_2 - CD_1 - CD_2  - CM - 0.8 \cdot HBAC  - 0.8 \cdot C_{HNF}                    
!! \f]
!! Dabei wird angenommen, dass ein kleiner Teil des chemischen Sauerstoffbedarfs nicht zur Bildung von 
!! Kohlendioxid verwendet wird (2.8 statt 2.667). Ausserdem wird angenommen, dass nur 80% des Kohlenstoffs in
!! Bakterien und Nanoflagelaten chemisch zehrungsfähig ist.
!! \n\n
!! Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
!! plus der Summe der Veränderungen:
!! \f[ 
!! C_{ref}(t+\Delta t) = C_{ref}(t) + \Delta C_{ref,fac}
!! + \Delta C_{ref,drs} + \Delta C_{ref,sed} + \Delta C_{ref,mort} + \Delta C_{ref,faec}
!! \f]
!! Negative Werte werden ausgeschlossen. Zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
!! welches auch nicht massenerhaltend ist.\n
!!<table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ C_{ref} \f$  </td><td> Cref </td><td> Konzentration der refraktären (nicht abbaubaren) C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ CSB \f$  </td><td> \ref ocsb </td><td> Kohlenstoffbürtiger chemischer Sauerstoffbedarf </td><td> mgO2/l </td><td> ? </td></tr>
!!<tr><td> \f$ CP_i \f$  </td><td> CP(i, \ref cp1+\ref cp2 </td><td> Konzentration der partikulären organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ i \f$  </td><td> i </td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td><td> 1, 2 </td></tr>
!!<tr><td> \f$ CD_i \f$  </td><td> CD(i, \ref cd1+\ref cd2 </td><td> Konzentration der gelösten organischen C-Verbindungen der i-ten Stoffgruppe </td><td> mgC/l</td><td> ? </td></tr>
!!<tr><td> \f$ CM \f$  </td><td> \ref CM </td><td> Konzentration der monomolekularen organischen C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ HBAC \f$  </td><td> \ref BAC </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ C_{HNF} \f$  </td><td> \ref CHNF </td><td> C-Masse der heterotrophen Nanoflagelaten </td><td> mgC/l </td><td> ? </td></tr>
!!<tr><td> \f$ \Delta t \f$  </td><td> \ref TFLIE </td><td> Zeitschrittweite </td><td> d </td><td> 0.01 ... 1 </td></tr>
!!</table>\n
!! zurück zu: \ref BSB , Quelle orgc_huelle.f95

!----------------------------------------------------------------------------------------------------------- orgc_huelle
!> SUBROUTINE orgc_huelle()
!! Beschreibung: \ref BSB ; ruft orgc() auf; siehe auch: \ref hüllen ; Quelle: orgc_huelle.f95
      SUBROUTINE orgc_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none

      integer :: i,j

    real, Dimension(1,2,100)        :: eCD, eCP
    real, Dimension(1,100)          :: eCM, eBAC
    real :: hsdFluB

!> i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
      iglob=(i+meinrang*part)
      if (iglob.gt. knotenanzahl2D) return ! überstehende Nummern nicht bearbeiten.

      !if(iglob.eq.kontrollknoten)print*,'orgc test return ',i
      !RETURN 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe: 

      obsb(1) = planktonic_variable_p(17+(i-1)*number_plankt_vari) ! C-BSB5 Randvorgabe
      obsb(2) = obsb(1)
      ocsb(1) = planktonic_variable_p(18+(i-1)*number_plankt_vari) ! CSB Randvorgabe
      ocsb(2) = ocsb(1)
      tiefe(1) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! water_depth(i) 
      tiefe(2) = tiefe(1)
      rau(1:2)= strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
      tflie = real(dt)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim3D) in real Tage (QSim)
      vmitt(1) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag
      vmitt(2) = vmitt(1)
      bl01(1) = planktonic_variable_p(44+(i-1)*number_plankt_vari) !! schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
      bl01(2) = bl01(1)
      bl02(1) = planktonic_variable_p(45+(i-1)*number_plankt_vari) ! leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
      bl02(2) = bl02(1)

      zooind(1) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien
      zooind(2) = zooind(1)
      abszo(1) = transfer_quantity_p(6+(i-1)*number_trans_quant) ! Absterberate Zooplankton
      abszo(2) = abszo(1)
      tempw(1) = planktonic_variable_p( 1+(i-1)*number_plankt_vari)  ! Wassertemperatur
      tempw(2) = tempw(1)
      vbsb(1) = planktonic_variable_p(46+(i-1)*number_plankt_vari) ! Rückgabe/Kontroll-wert ??
      vbsb(2) = vbsb(1)
      bsbt(1) = transfer_quantity_p(1+(i-1)*number_trans_quant) ! Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt (nur Rückgabe)
      bsbt(2) = bsbt(1)
      flag(1)=0         ! keine Einleitungen
      flag(2)=flag(1)
      elen(1)=1         ! Elementlänge (nicht verwendet)
      elen(2)=elen(1)
      ior=1             ! Laufindex
      anze=1            ! Anzahl der Profile im aktuellen Strang

      ecsb(1) = 0.0      ! keine Einleitung
      ebsb(1) = 0.0      ! keine Einleitung
      qeinl(1)= 0.0      ! kein Abfluss Einleitung
      vabfl(1) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
      vabfl(2) = vabfl(1)
      sdbsb(1)= 0.0      ! unbenutzt
      sdbsb(2)= sdbsb(1)
      zexki(1) = transfer_quantity_p(16+(i-1)*number_trans_quant) ! Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen in mg/l je Zeitschritt
      zexki(2) = zexki(1)
      zexgr(1) = transfer_quantity_p(17+(i-1)*number_trans_quant) ! Ausscheidungen der Rotatorien infolge Konsums von Grünalgen in mg/l je Zeitschritt
      zexgr(2) = zexgr(1)
      bsbbet(1) = benthic_distribution_p(7+(i-1)*number_benth_distr) ! Sauerstoffverbrauch durch Organismen auf Makrophyten; Ausgabevariable für bsbtb
      bsbbet(2) = bsbbet(1)
      dkimor(1) = transfer_quantity_p(7+(i-1)*number_trans_quant) ! Absterberate Kieselalgen
      dkimor(2) = dkimor(1)
      dgrmor(1) = transfer_quantity_p(8+(i-1)*number_trans_quant) ! Absterberate Grünlalgen
      dgrmor(2) = dgrmor(1)

      jiein(1)=0        ! null Punkt-Einleitungen
      jiein(2)=jiein(1)
      !jetzt direkt aus QSimDatenfelder bsbgr=transfer_parameter_p( 8) ! C-BSB5-Erhöhung Grünalgen  ?? Aparam.txt
      !jetzt direkt aus QSimDatenfelder bsbki=transfer_parameter_p(29) ! C-BSB5-Erhöhung Kieselalgen ?? Aparam.txt
      akbcm(1) = planktonic_variable_p(24+(i-1)*number_plankt_vari) ! Verhältnis Chlorophyll-a zu Kohlenstoff Kieselalgen
      akbcm(2) = akbcm(1)
      agbcm(1) = planktonic_variable_p(25+(i-1)*number_plankt_vari) ! Verhältnis Chlorophyll-a zu Kohlenstoff Gruenalgen
      agbcm(2) = agbcm(1)
      pfl(1)=benthic_distribution_p(3+(i-1)*number_benth_distr) ! Trockengewicht Wasserpflanzen in g/m²
      pfl(2)=pfl(1)
      ezind(1) = 0.0 ! Einleitungswert Rotatorien (keine Einleitungen in QSim3D)
      ezind(2) = ezind(1)

      abl(1) = planktonic_variable_p(10+(i-1)*number_plankt_vari) ! Anteil blau-Algen
      abl(2) = abl(1)
      abbcm(1) = planktonic_variable_p(26+(i-1)*number_plankt_vari) ! Verhältnis Chlorophyll-a zu Kohlenstoff der blau-Algen
      abbcm(2) = abbcm(1)
      !jetzt direkt aus QSimDatenfelder bsbbl=transfer_parameter_p(52) ! C-BSB5-Erhöhung Blaualgen ?? Aparam.txt
      !jetzt direkt aus QSimDatenfelder csbbl=transfer_parameter_p(53) ! > CSB-Erhöhung Blaualgen | Aparam.txt
      dblmor(1) = transfer_quantity_p(9+(i-1)*number_trans_quant) ! Absterberate Blaulalgen
      dblmor(2) = dblmor(1)
      zexbl(1) = transfer_quantity_p(18+(i-1)*number_trans_quant) ! Ausscheidungen der Rotatorien infolge Konsums von Blaualgen
      zexbl(2) = zexbl(1)

      !jetzt direkt aus QSimDatenfelder csbki=transfer_parameter_p(30) ! CSB-Erhöhung Kieselalgen | Aparam.txt
      !jetzt direkt aus QSimDatenfelder csbgr=transfer_parameter_p(9) ! CSB-Erhöhung Grünalgen | Aparam.txt
      ischif(1:2) = zone(point_zone(iglob))%schiff%schifffahrts_zone ! hier unbenutzt
      echla(1) = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
      evkigr(1) = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
      eantbl(1) = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)

      aki(1) = planktonic_variable_p(8+(i-1)*number_plankt_vari) ! Anteil kiesel-Algen
      aki(2) = aki(1)
      agr(1) = planktonic_variable_p(9+(i-1)*number_plankt_vari) ! Anteil gruen-Algen
      agr(2) = agr(1)
      drfaeb(1:2) = transfer_quantity_p(15+(i-1)*number_trans_quant) ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen
      drfaek(1:2) = transfer_quantity_p(13+(i-1)*number_trans_quant) ! Faecesbildung der Muscheln infolge Konsum Kieselalgen
      drfaeg(1:2) = transfer_quantity_p(14+(i-1)*number_trans_quant) ! Faecesbildung der Muscheln infolge Konsum Grünlalgen
      drfaes(1:2) = transfer_quantity_p(95+(i-1)*number_trans_quant) ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen
      ssdr(1)=benthic_distribution_p(4+(i-1)*number_benth_distr) ! Schwebstoffaufnahme durch Dreissena ??
      ssdr(2)=ssdr(1)
      orgCsd(1,1)=benthic_distribution_p(6+(i-1)*number_benth_distr) ! Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert
      orgCsd(1,2)=orgCsd(1,1)
      orgCsd0(1)=benthic_distribution_p(50+(i-1)*number_benth_distr) !  sedimentiertes organ. Material 
      orgCsd0(2)=orgCsd0(1)
      orgCsd_abb(1,1)=benthic_distribution_p(51+(i-1)*number_benth_distr) ! sedimentiertes biologisch abbaubares organ. Material 
      orgCsd_abb(1,2)=orgCsd_abb(1,1)

      CD(1,1) = planktonic_variable_p(37+(i-1)*number_plankt_vari) ! leicht abbaubare gelöste organische C-Verbindungen
      CD(1,2) = CD(1,1)
      CD(2,1) = planktonic_variable_p(38+(i-1)*number_plankt_vari) ! schwer abbaubare gelöste organische C-Verbindungen
      CD(2,2) = CD(2,1)
      CP(1,1) = planktonic_variable_p(39+(i-1)*number_plankt_vari) ! leicht abbaubare partikuläre organische C-Verbindungen
      CP(1,2) = CP(1,1)
      CP(2,1) = planktonic_variable_p(40+(i-1)*number_plankt_vari) ! schwer abbaubare partikuläre organische C-Verbindungen
      CP(2,2) = CP(2,1)
      CM(1) = planktonic_variable_p(41+(i-1)*number_plankt_vari) ! monomolekularen organischen C-Verbindungen
      CM(2) = CM(1)
      BAC(1) = planktonic_variable_p(42+(i-1)*number_plankt_vari) ! Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
      BAC(2) = BAC(1)
      !keine Einleitung
      eCD(1,1,1) = 0.0 ! 
      eCD(1,1,2) = 0.0 ! 
      eCD(1,2,1) = 0.0 ! 
      eCD(1,2,2) = 0.0 ! 
      eCP(1,1,1) = 0.0 !
      eCP(1,1,2) = 0.0 !
      eCP(1,2,1) = 0.0 !
      eCP(1,2,2) = 0.0 !
      eCM(1,1) =  0.0 !
      eCM(1,2) =  0.0 !
      eBAC (1,1) =   0.0 !                                                                               &
      eBAC (1,2) =   0.0 !                                                                               &

      O2BSB(1) = planktonic_variable_p(43+(i-1)*number_plankt_vari) ! Sauerstoff-Kohlenstoffverhältnis beim C-Abbau (mgO2/mgC) 
      O2BSB(2) = O2BSB(1)
      !jetzt direkt aus QSimDatenfelder TOC_CSB = 3.1 !  in orgc_start hart codiert
      !jetzt direkt aus QSimDatenfelder GRote=transfer_parameter_p(67) ! Gewicht einer Rotatorie µg  | Aparam.txt
      vcsb(1) = planktonic_variable_p(47+(i-1)*number_plankt_vari) ! Rückgabe/Kontroll-wert ??
      vcsb(2) = vcsb(1)
      vkigr(1) = planktonic_variable_p(19+(i-1)*number_plankt_vari) ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ??
      vkigr(2) = vkigr(1)
      antbl(1) = planktonic_variable_p(20+(i-1)*number_plankt_vari) ! Anteil der Blaualgen am Gesamt-Chlorophyll-a
      antbl(2) = antbl(1)

      HNFBAC(1) = transfer_quantity_p(11+(i-1)*number_trans_quant) ! Verlust der Bakterien durch HNF-Grazing                          
      HNFBAC(2) = HNFBAC(1)
      BSBHNF(1) = transfer_quantity_p(10+(i-1)*number_trans_quant) ! Absterben und Exkretion Heterotropher Naloflagelaten 
      BSBHNF(2) = BSBHNF(1)
      CHNF(1:2) = planktonic_variable_p(48+(i-1)*number_plankt_vari) ! C-Masse der heterotrophen Nanoflagelaten 
      if(CHNF(1).le. 0.0) CHNF(1:2) = 0.0 ! CHNF=-1 meint keine HNF
      eCHNF(1) = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
      fbsgr(1) = planktonic_variable_p(55+(i-1)*number_plankt_vari) ! Faktors zur Berechnung der ablagerungsfreien Grenzkonzentration von refraktärem Material ???? 
      fbsgr(2) = fbsgr(1)
      frfgr(1) = planktonic_variable_p(56+(i-1)*number_plankt_vari) ! Faktors zur Berechnung der ablagerungsfreien Grenzkonzentration von refraktärem Material ???? 
      frfgr(2) = frfgr(1)
      fbsgrs = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
      frfgrs = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)

      BACmua(1) = transfer_quantity_p(4+(i-1)*number_trans_quant) !  Ausgabekonzentration Summe Aufnahme+Respirationsrate heterotrophe Bakterien
      BACmua(2) = BACmua(1)
      dorgSS(1) = transfer_quantity_p(19+(i-1)*number_trans_quant) ! Veränderung von suspendierten Sedimenten aus C-Verbindungen
      dorgSS(2) = dorgSS(1)
      ilbuhn=0          ! keine Buhnen
      iwied=0      ! unbenutzte Variable
      fkm (1)=0.0  ! Flusskilometer unbenutzt
      bsbct(1) = transfer_quantity_p(47+(i-1)*number_trans_quant) ! mineralisierter Kohlenstoffgehalt in der Wassersäule | Rückgabewert
      bsbct(2) = bsbct(1)

      qeinlL(1) = 0.0 ! keine linienquelle
      iorLa(1)= 0 ! keine linienquelle
      iorLe(1)= 0 ! keine linienquelle
      ieinLs(1)= 0 ! keine linienquelle


      pl0(1) = planktonic_variable_p(58+(i-1)*number_plankt_vari)  !  P/C Verhältnis
      pl0(2) = pl0(1)
      Q_PK(1) = planktonic_variable_p(31+(i-1)*number_plankt_vari) ! Phosphoranteil der Algenbiomasse kiesel
      Q_PK(2) = Q_PK(1)
      Q_PB(1) = planktonic_variable_p(36+(i-1)*number_plankt_vari) ! Phosphoranteil der Algenbiomasse blau
      Q_PB(2) = Q_PB(1)
      Q_PG(1) = planktonic_variable_p(34+(i-1)*number_plankt_vari) ! Phosphornteil der Algenbiomasse gruen
      Q_PG(2) = Q_PG(1)
      pZoo  = transfer_value_p(3) !! Phosphoranteil in der Rotatorienbiomasse mgP/mgBiom.
      nl0(1) = planktonic_variable_p(57+(i-1)*number_plankt_vari) ! Verhältnis von Stickstoff zu Kohlenstoff in organischem Material
      nl0(2) = nl0(1)
      Q_NK(1) = planktonic_variable_p(30+(i-1)*number_plankt_vari) ! Stickstoffanteil der Algenbiomasse kiesel
      Q_NK(2) = Q_NK(1)
      Q_NB(1) = planktonic_variable_p(35+(i-1)*number_plankt_vari) ! Stickstoffanteil der Algenbiomasse blau
      Q_NB(2) = Q_NB(1)
      Q_NG(1) = planktonic_variable_p(33+(i-1)*number_plankt_vari) ! Stickstoffanteil der Algenbiomasse gruen
      Q_NG(2) = Q_NG(1)
      nZoo  = transfer_value_p(2) ! Stickstoffanteil in der Rotatorienbiomasse mgN/mgBiom.

      bsbctP(1) = transfer_quantity_p(2+(i-1)*number_trans_quant) ! Phosphorfreisetzung beim Abbau org. Kohlenstoffverbidungen
      bsbctP(2) = bsbctP(1)
      doN(1) = transfer_quantity_p(3+(i-1)*number_trans_quant) ! mineralisierter N-Gehalt in der Wassersäule ; Ammoniumfreisetzung beim Abbau org. Kohlenstoffverbidungen
      doN(2) = doN(1)

      hsdFluB=0.0 ! unbenutzte Variable

      JDOC1(1)=benthic_distribution_p(48+(i-1)*number_benth_distr) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, leicht abbaubar
      JDOC1(2)=JDOC1(1)
      JDOC2(1)=benthic_distribution_p(49+(i-1)*number_benth_distr) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, schwer abbaubar
      JDOC2(2)=JDOC2(1)

      ! aparam_lesen() jetzt direkt in QSimDatenfelder::hype ! Hydrolyserate für die leichtabbaubaren partikulären organischen C-Verbindungen APARAM.txt
      !jetzt direkt aus QSimDatenfelder hymxDe  ! maximale Hydrolyserate für die gelösten organischen C-Verbindungen | APARAM.txt
      !jetzt direkt aus QSimDatenfelder KsD1e  ! Halbsättigungskonstante für die Hydrolyse der gelösten leichtabbaubaren organischen C-Verbindungen | APARAM.txt
      !jetzt direkt aus QSimDatenfelder KsD2e  ! Halbsättigungskonstante für die Hydrolyse der gelösten schwerabbaubaren organischen C-Verbindungen | APARAM.txt
      !jetzt direkt aus QSimDatenfelder KsMe  ! Halbsättigungskonstante für die Aufnahme von Kohlenstoff durch heterotrophen Bakterien | APARAM.txt
      !jetzt direkt aus QSimDatenfelder upBACe  ! max. Aufnahmerate monomerer C-Verbindungen d. Bakterien | APARAM.txt

      !jetzt direkt aus QSimDatenfelder YBACe ! Ertragskoeffizient für Bakterienbiomasse | APARAM.txt
      !jetzt direkt aus QSimDatenfelder rsGBACe ! Grundrespirationsrate HBAC bei Optimumstemperatur | APARAM.txt
      nkzs(1)=1         ! nur eine Tiefenschicht
      nkzs(2)=nkzs(1)
      mstr=1            ! Strangzähler; orgc so benutzen , dass nur der 1. Strang mit nur einem Knoten/Profil berechnet wird
      !itags=tag         ! Startzeitpunkt aus module_QSimDatenfelder
      !monats=monat      
      uhrz=uhrzeit_stunde ! Uhrzeit module::modell zeitsekunde()

      zBAC(1) = transfer_quantity_p(91+(i-1)*number_trans_quant) ! Konsum von BAC durch Zoopankton
      zBAC(2) = zBAC(1)

      !bsbZoo = 1.6  ! in orgc_start hart codiert jetzt direkt aus QSimDatenfelder

      ! kontroll,i ! Für QSim3D hinzugefügt (kontrollausgabemöglichkeit)
      !kontroll=(i.eq.number_plankt_point)
      kontroll=(iglob.eq.kontrollknoten)
      if(kontroll) print*,'orgc_huelle davor: CHNF,BAC,obsb,ocsb,bsbt,bl01,bl02,vbsb,bsbt'  &
     &                   ,CHNF(1),BAC(1),obsb(1),ocsb(1),bsbt(1),bl01(1),bl02(1),vbsb(1),bsbt(1)
      if(kontroll) print*,'orgc_huelle davor: BAC,bacmua,CM,zBAC',BAC(1),BACmua(1),CM(1),zBAC(1)

!version  Stoffumsatzroutinen aus der QSim1D Version 13_40 vom 15. Oktober 2018 in QSim3D
       call orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,flae,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze            &                   
                ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor,jiein,bsbgr,bsbki,akbcm             &
                ,agbcm,pfl,ezind,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb,csbki,csbgr,ischif,echla               &
                ,evkigr,eantbl,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd,orgCsd0,orgCsd_abb,CD,CP,CM,BAC,eCD       &
                ,eCP,eCM,eBAC,TOC_CSB,GRote,vcsb,vkigr,antbl,HNFBAC,BSBHNF,CHNF,zBAC                              &
                ,BVHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct,qeinlL                &
                ,iorLa,iorLe,ieinLs,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo,etemp,bsbctP                  &
                ,doN,hsdFluB,hyPe,hymxDe,KsD1e,KsD2e,KsMe,upBACe,JDOC1,JDOC2,YBACe,rsGBACe                        &
                ,nkzs,mstr,itags,monats,uhrz,azStrs,bsbZoo                                                        &                                                   
                ,kontroll ,i )
!version qsim13.301_28mae18wy_3D
!       call orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,flae,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze             &                   
!                ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor,jiein,bsbgr,bsbki,akbcm             &
!                ,agbcm,pfl,ezind,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb,csbki,csbgr,ischif,echla               &
!                ,evkigr,eantbl,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd,orgCsd0,orgCsd_abb,CD,CP,CM,BAC,eCD       &
!                ,eCP,eCM,eBAC,TOC_CSB,GRote,vcsb,vkigr,antbl,HNFBAC,BSBHNF,CHNF,zBAC                              &
!                ,BVHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct,qeinlL                &
!                ,iorLa,iorLe,ieinLs,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo,etemp,bsbctP                  &
!                ,doN,hsdFluB,hyPe,hymxDe,KsD1e,KsD2e,KsMe,upBACe,JDOC1,JDOC2,YBACe,rsGBACe                        &
!                ,nkzs,mstr,itags,monats,uhrz, azStrs, bsbZoo                                                      &
!                ,kontroll,i)           !!wy      
!version 13_30
!  call orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,flae,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze            &                   
!                ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor,jiein,bsbgr,bsbki,akbcm             &
!                ,agbcm,pfl,ezind,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb,csbki,csbgr,ischif,echla               &
!                ,evkigr,eantbl,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd,orgCsd0,orgCsd_abb,CD,CP,CM,BAC,eCD       &
!                ,eCP,eCM,eBAC,TOC_CSB,GRote,vcsb,vkigr,antbl,HNFBAC,BSBHNF,CHNF                                   &
!                ,BVHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct,qeinlL                &
!                ,iorLa,iorLe,ieinLs,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo,etemp,bsbctP                  &
!                ,doN,hsdFluB,hyPe,hymxDe,KsD1e,KsD2e,KsMe,upBACe,JDOC1,JDOC2,YBACe,rsGBACe                        &
!                ,nkzs,mstr,itags,monats,uhrz,1                                                               &
!                ,kontroll,i)           !!wy      
!version 13_10
!       call orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,bl01,bl02                        &
!                ,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze                  &
!                ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor     &
!                ,jiein,bsbgr,bsbki,akbcm,agbcm,pfl,ezind                          &
!                ,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb                        &
!                ,csbki,csbgr,ischif,echla,evkigr,eantbl                           &
!                ,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd                         &
!                ,CD,CP,CM,BAC,O2BSB,GRote,vcsb,vkigr,antbl                        &
!                ,HNFBAC,BSBHNF,CHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs               &
!                ,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct                             &
!                ,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo                  &
!                ,bsbctP,doN,hyPe,hymxDe,KsD1e,KsD2e,KsMe,upBACe                   &
!                ,YBACe,rsGBACe,nkzs,mstr,itags,monats,uhrz                        &
!                ,kontroll,i)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe: 
      if(kontroll) print*,'orgc_huelle nachher: BAC,bacmua,CM,zBAC',BAC(1),BACmua(1),CM(1),zBAC(1)

      planktonic_variable_p(17+(i-1)*number_plankt_vari) = obsb(1) !
      planktonic_variable_p(18+(i-1)*number_plankt_vari) = ocsb(1) !
      ! entfallen planktonic_variable_p(44+(i-1)*number_plankt_vari) = bl01(1) !
      ! entfallen planktonic_variable_p(45+(i-1)*number_plankt_vari) = bl02(1) !

      planktonic_variable_p(46+(i-1)*number_plankt_vari) = vbsb(1) !
      planktonic_variable_p(47+(i-1)*number_plankt_vari) = vcsb(1) !
      transfer_quantity_p(1+(i-1)*number_trans_quant) = bsbt(1) !

      benthic_distribution_p(7+(i-1)*number_benth_distr) = bsbbet(1) ! Sauerstoffverbrauch durch Organismen auf Makrophyten, Ausgabewert bsbtb

      benthic_distribution_p(6+(i-1)*number_benth_distr) = orgCsd(1,1) ! Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert
      benthic_distribution_p(50+(i-1)*number_benth_distr) = orgCsd0(1) ! teil des? sedimentierten organ. Material 
      benthic_distribution_p(51+(i-1)*number_benth_distr) = orgCsd_abb(1,1) ! sedimentiertes biologisch abbaubares organ. Material 

      planktonic_variable_p(37+(i-1)*number_plankt_vari) = CD(1,1) !
      planktonic_variable_p(38+(i-1)*number_plankt_vari) = CD(2,1) !
      planktonic_variable_p(39+(i-1)*number_plankt_vari) = CP(1,1) !
      planktonic_variable_p(40+(i-1)*number_plankt_vari) = CP(2,1) !
      planktonic_variable_p(41+(i-1)*number_plankt_vari) = CM(1) !
      planktonic_variable_p(42+(i-1)*number_plankt_vari) = BAC(1) !
      ! entfallen planktonic_variable_p(43+(i-1)*number_plankt_vari) = O2BSB(1) !

      planktonic_variable_p(55+(i-1)*number_plankt_vari) = fbsgr(1) !
      planktonic_variable_p(56+(i-1)*number_plankt_vari) = frfgr(1) !

      transfer_quantity_p(4+(i-1)*number_trans_quant) = BACmua(1) ! Ausgabekonzentration
      transfer_quantity_p(19+(i-1)*number_trans_quant) = dorgSS(1) !
      transfer_quantity_p(47+(i-1)*number_trans_quant) = bsbct(1) ! mineralisierter Kohlenstoffgehalt in der Wassersäule | Rückgabewert

      planktonic_variable_p(58+(i-1)*number_plankt_vari) = pl0(1) !
      planktonic_variable_p(57+(i-1)*number_plankt_vari) = nl0(1) !

      transfer_quantity_p(2+(i-1)*number_trans_quant) = bsbctP(1) !
      transfer_quantity_p(3+(i-1)*number_trans_quant) = doN(1) !

      benthic_distribution_p(48+(i-1)*number_benth_distr)=JDOC1(1) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, leicht abbaubar
      benthic_distribution_p(49+(i-1)*number_benth_distr)=JDOC2(1) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, schwer abbaubar

      !if(kontroll) print*,'orgc_huelle danach: bsbt, obsb, vbsb,bl01,' &
      !  ,transfer_quantity_p(1+(i-1)*number_trans_quant),planktonic_variable_p(17+(i-1)*number_plankt_vari),  &
      !   planktonic_variable_p(46+(i-1)*number_plankt_vari),bl01(1)
      if(kontroll) print*,'orgc_huelle danach: CHNF,BAC,obsb,ocsb,bsbt,bl01,bl02,vbsb,bsbt'  &
     &                   ,CHNF(1),BAC(1),obsb(1),ocsb(1),bsbt(1),bl01(1),bl02(1),vbsb(1),bsbt(1)

      RETURN 
      END subroutine orgc_huelle
!----+-----+----

!> \page orgc_aufteilung Aufteilung des organischen Kohlenstoffs
!!
!! Nicht alle in der Simulation verwendeten Größen können in Gewässern (einfach) gemessen werden. 
!! Daher ergibt sich die Frage, welche Werte für nicht gemessene Größen an den Rändern des Simulationsgebiets
!! für das einströmende Wasser angesetzt werden sollen. 
!! \n\n
!! In Bezug auf \ref BSB werden als Randwerte
!! nur der \f$ C-BSB5_{Rand} \f$, d. h. der biologische Sauerstoffbedarf in 5 Tagen 
!! und der \f$ CSB_{Rand} \f$,  d. h. der chemische Sauerstoffbedarf vorgegeben. 
!! Ausserdem können die \f$ C_{HNF} \f$,  d. h. die Masse des Kohlenstoffs in heterotrophen Nanoflagelaten (wie will man denn das messen?)
!! und das  \f$ V_{HNF} \f$,  d. h. das Biovolumen der HNF vorgegeben werden.
!! \n\n
!! Eine Aufteilung in die Berechnungs-Variablen geschieht dann mittels empirischer Ansätze. 
!! Für \ref BSB ist diese Aufteilung jetzt in die Subroutine orgc_start() ausgelagert worden.
!! Der entsprechende Programmteil wurde aus dem QSim1D Hauptprogramm entnommen.
!! \n\n
!! Für \ref BSB wird folgende empirische Aufteilung angesetzt:\n
!! Zuerst werden die Sauerstoffbedarfe abgezogen, die nicht aus dem organischen Kohlenstoff, sondern aus den
!! Algen und dem Zooplankton kommen:
!! \f[ 
!!       C-BSB5 = C-BSB5_{Rand}-algb5-zoobsb 
!! \f]\f[
!!       CSB = CSB_{Rand}-algcs-zoocsb                                                   
!! \f]
!! \n
!! Dann werden Minima erzwungen, d.h. am Rand sind immer mindestens \f$ C-BSB5 = 0.25 mg/l \f$ und \f$ CSB = 2.5 mg/l \f$
!! \n
!! daraufhin wird ein Verhältnis aus den beiden Sauerstoffbedarfen gebildet:
!! \f[ 
!! v = \frac{C-BSB5}{CSB}
!! \f]
!! \n
!! Die monomolekularen organischen Kohlenstoffverbindungen werden mit immer dem gleichen kleinen Wert angesetzt:
!! \f[ 
!! CM = 0.03
!! \f]
!! \n
!! Als nächster Schritt erfolgt dann folgender Ansatz für den Gesamtgehalt an organischem Kohlenstoff (total organic carbon)                                                
!! \f[ 
!!       TOC = 0.25 \cdot C-BSB5 \cdot 0.782 \cdot v^{-0.921}
!! \f]
!! \n
!! Dieser wird dann aufgeteilt in gelöste und partikuläre Anteile:
!! \f[ 
!!       CD_{ges} = BTOC \cdot \alpha_D
!! \f]
!! \f[ 
!!       CP_{ges} = BTOC \cdot (1-\alpha_D)
!! \f]
!! mit: 
!! \f[  \alpha_D = 0.21 \cdot ln(v) + 0.629     \f]
!! \n
!! Die Aufteilung in der gelösten Fraktion in leicht(1) und schwer(2) abbaubar geschieht mittels folgenden Ansatzes: \n
!! (Die Setzung des monomolekularen Anteils auf 0.03 ist bereits erfolgt)
!! ausserdem wird noch die C-Masse der heterotrophen Nanoflagelaten abgezogen:
!! \f[ 
!!       CD_1 = ( (CD_{ges}-CM) \cdot \alpha_l ) - C_{HNF}
!! \f]
!! \f[ 
!!       CD_2 = ( (CD_{ges}-CM) \cdot (1-\alpha_l) ) - C_{HNF}
!! \f]
!! mit: 
!! \f[  \alpha_l = 0.218 \cdot ln(v) + 0.717  \f]
!! \n
!! In ähnlicher Art werden die partikulären Fraktionen in leicht(1) und schwer(2) abbaubar aufgeteilt:
!! auch hier wird noch die C-Masse der heterotrophen Nanoflagelaten abgezogen:
!! \f[ 
!!       CP_1 = ( (CP_{ges} \cdot 0.9538 \cdot v ) \cdot \alpha_P ) - C_{HNF}
!! \f]
!! \f[ 
!!       CP_2 = ( (CP_{ges} \cdot 0.9538 \cdot v ) \cdot (1-\alpha_P) ) - C_{HNF}
!! \f]
!! mit: 
!! \f[  \alpha_P =  0.426 \cdot ln(v)+1.114 \f]
!! 
!! mittels des Faktors  0.9538 war vorher noch der Anteil abgetrennt worden, 
!! der in den heterotrophen Bakterien gespeichert ist:
!! \f[ 
!!       HBAC = ( CP_{ges} \cdot 0.0462 \cdot v )
!! \f]
!! Zur Ermittlung der Sauerstoffäquivalente und des Sauerstoff-Kohlenstoffverhältnisse wird dann zunächst eine 
!! Hilfsvariable bx1 gebildet
!! \f[ 
!! bx1 = {\alpha_D}^2 + (1.-0.0462 \cdot v) \cdot (1-\alpha_D) \cdot \alpha_P+0.0462 \cdot v \cdot (1.-\alpha_D) \cdot 0.4\n
!! \f]
!! Mit den Parametern bk1 = 0,51 und bk2 = 0,02 bestimmt sich dann das Gesamt-Sauerstoffäquivalent zu:
!! \f[ 
!!  BL = \frac{C-BSB5}{ 1.-(bx1 \cdot e^{-bk1 \cdot 5.}+(1.-bx1) \cdot e^{-bk2 \cdot 5.}) }
!! \f]
!! Sauerstoff-Kohlenstoffverhältnis beim C-Abbau bestimmt sich dann aus dem 
!! Gesamt-Sauerstoffäquivalent zum Gesamtgehalt an organischem Kohlenstoff
!! \f[ 
!!  O_{BSB} = \frac{BL}{BTOC}
!! \f]
!! Der leichtabbaubare Anteil am Sauerstoffäquivalent wird mittels des Hilfsfaktors bx1 ermittelt
!! \f[ 
!!  BL_{1} = BL \cdot bx1
!! \f]
!! Der verblieben Rest ist dann schwerabbaubar:
!! \f[ 
!!  BL_{2} = BL \cdot (1.-bx1)
!! \f]
!! \n
!! Ganz am Schluss werden Werte, die unter Null berechnet wurden auf 0.000001 geklippt.
!! \n
!! mit:
!!<table >
!!<tr><th> Formelzeichen 	</th><th> Name QSim 	</th><th> Beschreibung </th><th> Dimension h</th></tr>
!!<tr><td> \f$ C-BSB5 \f$  	</td><td> \ref obsb 	</td><td> Kohlenstoffbürtiger biologischer Sauerstoffbedarf in 5 Tagen </td><td> mgO2/l </td></tr>
!!<tr><td> \f$ CSB \f$  	</td><td> \ref ocsb 	</td><td> Kohlenstoffbürtiger chemischer Sauerstoffbedarf </td><td> mgO2/l </td></tr>
!!<tr><td> \f$ C_{HNF} \f$  	</td><td> \ref CHNF 	</td><td> C-Masse der heterotrophen Nanoflagelaten </td><td> mgC/l </td></tr>
!!<tr><td> \f$ V_{HNF} \f$  	</td><td> \ref BVHNF	</td><td> Biovolumen der HNF  </td><td> µm3 ?? </td></tr>
!!<tr><td> \f$ v \f$  		</td><td> vcbe 		</td><td> Verhältnis \ref obsb/ \ref ocsb </td><td> - </td></tr>
!!<tr><td> \f$ CP_i \f$  	</td><td> CP(i=\ref cp1+\ref cp2, </td><td> \ref bilaCP in der i-ten Stoffgruppe </td><td> mgC/l </td></tr>
!!<tr><td> \f$ CD_i \f$  	</td><td> CD(i=\ref cd1+\ref cd2 </td><td> \ref bilaCD in der i-ten Stoffgruppe </td><td> mgC/l</td></tr>
!!<tr><td> \f$ i \f$  		</td><td> i 		</td><td> Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar</td><td> - </td></tr>
!!<tr><td> \f$ CM \f$  		</td><td> \ref CM 	</td><td> planktonic_variable 41</td><td> \ref bilaCM </td><td> mgC/l /td></tr>
!!<tr><td> \f$ HBAC \f$  	</td><td> \ref BAC	</td><td> planktonic_variable 42</td><td> \ref bilaBAC </td><td> mgC/l </td></tr>
!!<tr><td> \f$ C_{ref} \f$  	</td><td> Cref 		</td><td> \ref bilaCref </td><td> mgC/l </td></tr>
!!<tr><td> \f$ TOC \f$  	</td><td> BTOC 		</td><td> Gesamtgehalt an organischem Kohlenstoff (total organic carbon)  </td><td> mgC/l </td></tr>
!!<tr><td> \f$ O_{BSB} \f$  	</td><td> \ref o2bsb 	</td><td> Sauerstoff-Kohlenstoffverhältnis beim C-Abbau </td><td> mgO2/mgC </td></tr>
!!<tr><td> \f$ BL_{1} \f$  	</td><td> \ref bl01 	</td><td> leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent </td><td> mg O2 / l </td></tr>
!!<tr><td> \f$ BL_{2} \f$  	</td><td> \ref bl02 	</td><td> schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent </td><td> mg O2 / l </td></tr>
!!</table>
!! \n\n
!! zurück zu: \ref BSB oder \ref zuflussranddaten , Quelle: orgc_huelle.f95
!....Berechnung der "BSB-Komponenten" am oberen Rand                    
!     (auch bei Stundenwert-Generierung)!!!!!                           

!> \page sauerkohl Sauerstoff-Kohlenstoffverhältnis beim Abbau organsichen Kohlenstoffs
!!
!! QSim berechnet dieses Sauerstoff-Kohlenstoffverhältnis (Variable 43, O2BSB, aus dem planktonic_variablen.h ) 
!! nur bei der Aufteilung
!! der vorgebenen Randbedingungen in die Berechnungsvariablen siehe:\ref orgc_aufteilung.\n
!! D.h. Dieses Verhältnis kann sich zwar mit anderen Einleitungen mischen, bleibt aber ansonsten konstant.
!! \n\n
!! In dem schematischen Testfall 
!! <a href="http://voss-mod01.bafg.de/wyrwa/modell_wiki/dokuwiki/doku.php?id=schema:annu:3guete:2orgc:schwer"> 
!! schwer abbaubarer, partikuärer, organischer Kohlenstoff</a> wurde für das Sauerstoff-Kohlenstoffverhältnis
!! 1,8 mgO2/mgC berechnet.
!! \n\n
!! Aus Überlegungen zum
!! <a href="http://www.buetzer.info/fileadmin/pb/pdf-Dateien/Respirationsquotient.pdf">Respirationsquotienten</a>
!! menschlicher Nahrung folgt, dass dabei das Sauerstoff-Kohlenstoffverhältnis zwischen 
!! 2,7 mgO2/mgC (Zucker) und 3,8 mgO2/mgC (Fett) liegt. (mit C 12g/mol und O 16g/mol)
!! \n\n
!! in der subroutine ph() findet sich:\n
!! !     CO2 Lieferung durch C-abbau \n                                                                                                            
!! !     1mg O2 Verbrauch liefert 1.77 mg CO2 (aus der allgemeinen Biomassenformel) \n                                                
!!       co2bsb = bsbt(ior)*1.3 \n
!! \n\n
!! zurück zu \ref klaerung  , Quelle: orgc_huelle.f95\n

