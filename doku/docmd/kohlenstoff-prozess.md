Organischer Kohlenstoff  - Prozesse {#lnk_orgC_prozesse}
===================== 
Der Baustein orgc() dient dazu, die Fraktionen des organischen Kohlenstoffs zu bilanzieren,
deren Abbau den biochemischen Sauerstoffbedarf (BSB) hervorruft\n

Das "c" in orgc kommt aus dem Lateinischen (carbo = „Holzkohle“) und wird 
als Elementsymbol für Kohlenstoff verwendet.

# Teilprozesse
Folgende Teilprozesse wirken sich in QSim auf den Kohlenstoffgehalt des Wassers aus: 


1. [Hydrolyse partikulärer C-Verbindungen](\ref lnk_hyp) in gelöste organische C-Verbindungen
2. [Hydrolyse gelöster organischer C-Verbindungen](\ref lnk_hyd) in monomolekulare
    organische    C-Verbindungen]
3. [Stoffwechsel der heterotrophen Bakterien](\ref lnk_BACm)
4. [Aufnahme gelöster C-Verbindungen durch Organismen auf Makrophyten](\ref lnk_fluxmaphy)
5. [Absterben von lebender Biomasse](\ref lnk_mortalgC)
6. [Ausscheidungen von Zooplanktern und Dreissena-Muscheln](\ref lnk_facesC)
7. [Schwebstoffaufnahme der Dreissena Muscheln](\ref lnk_Cschwebdreiss)
8. [Sedimentation kohlenstoffhaltigen Materials](\ref lnk_Sediorgc)
9. [Verlust der Bakterien durch HNF-Grazing](\ref lnk_bacHNFgraz)

Der gesamte orgc - Baustein basiert auf einer tiefengemittelten Betrachtungsweise.

Die Umwandlung von organischen C-Verbindungen wirkt sich auf viele weitere Prozesse im
Gewässer aus:

* [Sauerstoff-Verbrauch](\ref lnk_o2zehr)
* [Ammonium-Freisetzung](\ref lnk_nh4freis)
* [Phosphat Freisetzung](\ref lnk_pfreis)
* [Änderung des ph-Werts durch Bildung von Kohlensäure](\ref lnk_kohlensauer)
* [Schwebstoffgehalt](\ref lnk_schwebkohl)
* [Sedimentation](\ref lnk_sedkohl)

Für jede modellierte Kohlenstofffraktionen sowie für die Bakterienbiomasse 
existieren eigene Bilanzgleichungen.

Die Bilanzgleichung für die refraktären C-Verbindungen lautet:

\f[ 
 \frac{dC_{ref}}{dt} = \alpha_{C,ref} \cdot \left( \sum_{j = 1}^{j = 3} A_{mor,j} 
   \cdot C_{A,j} + I_{rot,mor} \cdot \frac{G_rot}{1000} \cdot C_{rot} +
   \sum_{j = 1}^{j = 3} ROT_{faec, j} \cdot C_{A,j} + 
   \sum_{j = 1}^{j = 3} DR_{faec,j} \cdot C_{A,j}  \right) - C_{ref, sed}
\f]
<!-- #mf: beim Summenzeichen schauen, da stimmt die Notation noch nicht -->

\f$ C_{ref} \f$:   Kohlenstoffanteil der refraktären organischen Substanz [g*m-3*d-1] \n
\f$ \alpha_{C,ref} \f$:  Anteil der refraktären C-Verbindungen an der 
  abgestorbenen bzw. gefressenen Biomasse [\f$ - \f$] \n
\f$ A_{mor,j} \f$:  abgestorbene Algenbiomasse der Algenklasse *j* [\f$ g \cdot m^{-3} \cdot d^{-1} \f$] \n
\f$ C_{A,j} \f$:  Kohlenstoffanteil der abgestorbenen Algenbiomasse der Algenklasse *j* [\f$ - \f$] \n
\f$ I_{rot,mor} \f$:  Anzahl abgestorbener Rotatorien [\f$ Ind \cdot d^{-1}m^{-3} \f$] \n
\f$ G_rot \f$:  Gewicht einer Rotatorie [\f$ \mu g TG \f$] \n
\f$ C_{rot} \f$:  Kohlenstoffanteil der abgestorbenen Rotatorienbiomasse [\f$ - \f$] \n
\f$ ROT_{faec, j} \f$:  ausgeschiedene Algenbiomasse (Faeces) der Algenklasse *j* durch Rotatorien [\f$ g m^{-3} d^{-1} \f$] \n
\f$ DR_{faec,j} \f$:  ausgeschiedene Algenbiomasse (Faeces) der Algenklasse *j* durch Dreissena [\f$ g m^{-3} d^{-1} \f$] \n
\f$ C_{ref, sed} \f$:  Kohlenstoffanteil der sedimentierten refraktären organischen Substanz [\f$ g m^{-3} d^{-1} \f$] \n
\f$ j \f$:  Algengruppe (1 = Kieselalgen, 2 = Grünalgen, 3 = Cyanobakterien) [\f$ - \f$] \n

Die Bilanzgleichungen für die beiden partikulären Kohlenstoff- (C-) Verbindungen lauten:

<!-- #mf: ab hier noch das Word Dok weiter einarbeiten, aber am besten gleich 
mit Text unten abgleichen -->


# Hydrolyse partikulärer C-Verbindungen in gelöste organische C-Verbindungen {#lnk_hyp} 
 
Bei der Hydrolyse (Umwandlung, Zerfall) von partikulären organischen C-Verbindungen in
gelöste organische C-Verbindungen wird eine temperaturabhängige Hydrolyserate angesetzt.\n
In Q-Sim werden 2 Fraktionen unterschieden, 1. leicht und 2. schwer abbaubare
C-Verbindungen.
Dabei wird angenommen, dass sich schwer abbaubare partikuläre C-Verbindungen nur und 
schwer abbaubare gelöste C-Verbindungen umwandeln; analog wandeln sich leichtabbaubare 
partikuläre C-Verbindungen auch nur in leicht abbaubare gelöste C-Verbindungen um:

\f[ 
 \frac{\partial CD_i}{\partial t}_{hyp} = hy_{P,i} * f_T * CP_i
\f]

\f$ \partial CD_i \f$:  Beschreibung [\f$ Einheit \f$] \n
\f$ hy_{P,i} \f$:  Beschreibung [\f$ Einheit \f$] \n
\f$ f_T \f$:  Beschreibung [\f$ Einheit \f$] \n
\f$ CP_i \f$:  Beschreibung [\f$ Einheit \f$] \n

Im gleichen Maße wie die Konzentrationen der gelösten C-Verbindungen zunehmen, müssen 
natürlich die der partikulären organischen C-Verbindungen abnehmen:
\f[ 
 \frac{\partial CP_i}{\partial t}_{hyp} = -\frac{\partial CD_i}{\partial t}_{hyp}
\f]

\f$ \partial CP_i \f$:  Beschreibung [\f$ Einheit \f$] \n
\f$ \partial CD_i \f$:  Beschreibung [\f$ Einheit \f$] \n


Bei der Temperaturabhängigkeit wird angenommen, dass sich bei 25 °C ein Optimum 
ergibt:                                         
\f[ 
 f_T= e^{ -\frac{ {(T - T_{opt})}^2}{dti^2} }
\f]

\f$ f_T \f$:  Beschreibung [\f$ Einheit \f$] \n
\f$ T \f$:  Beschreibung [\f$ Einheit \f$] \n
\f$ T_{opt} \f$:  Beschreibung [\f$ Einheit \f$] \n
\f$ dti \f$:  Beschreibung [\f$ Einheit \f$] \n

\image html ftemp.svg 
<!-- \image latex ftemp.eps -->

Die Hydrolyseraten (bei Optimumstemperatur) für die partikuläre organische C-Verbindungen 
werden bei leicht und schwer abbaubaren C-Verbindungen unterschiedlich bestimmt: \n
Bei den 1. leicht abbaubaren C-Verbindungen wird eine Konstante angesetzt, die 
vorgegeben werden muss (APARAM.txt); \n
bei den 2. schwer abbaubare C-Verbindungen wird eine Abhängigkeit vom Verhältnis des 
biologischen zum chemischen Sauerstoffbedarf berücksichtigt.

\f{eqnarray*}{
 hy_{P,1}=& hy_{P,e}  \\
 hy_{P,2}=& 1.51*(\frac{C-BSB_5}{CSB})^{2.31}
\f}

Die Veränderungen werden explizit diskretisiert:

\f{eqnarray*}{
 \Delta {CD_i}_{hyp} = \frac{\partial CD_i}{\partial t}_{hyp} \cdot \Delta t  \\
 \Delta {CP_i}_{hyp} = \frac{\partial CP_i}{\partial t}_{hyp} \cdot \Delta t
\f}

Negative Werte werden ausgeschlossen, weil wo nichts ist, kann auch nichts 
verbraucht werden.

Hartes Klipping bei: \f$ CP_i(t+\Delta t) > 0.00001 \f$ ist nicht massenerhaltend 
in Bezug auf: \f$ CD_i \f$

# Hydrolyse gelöster organischer C-Verbindungen in monomolekulare organische C-Verbindungen {#lnk_hyd} 

<!-- #mf: hier weiter aufräumen: -->

1. leicht und 2. schwer abbaubare gelöste C-Verbindungen werden in monomolekulare 
organische C-Verbindungen umgewandelt.
Diese Hydrolyse wird befördert von extrazellulären Enzymen, die von heterotrophen 
Bakterien ins Wasser abgegeben werden, um monomolekulare organische C-Verbindungen 
zu erhalten, von denen selbige sich ernähren.

\f[ 
 \frac{\partial CM}{\partial t}_{hyd} = (hy_{D,1} + hy_{D,2}) * f_T * HBAC
\f]

im gleichen Maße wie die Konzentrationen der monomolekularen C-Verbindungen zunimmt, 
müssen natürlich die Konzentrationen der gelösten organischen C-Verbindungen abnehmen:

\f[ 
 \frac{\partial CD_i}{\partial t}_{hyd} = -hy_{D,i}*f_T*HBAC
\f]

Die Hydrolyseraten für die gelösten organischen C-Verbindungen werden abhängig vom 
vorhandenen gelösten organischen Kohlenstoff unter Zuhilfenahme von Maximalwert und
Halbsättigungskonstante gemäß fogender Formel berechnet:

\f[ 
 hy_{D,i}= hy_{max,D,i}* \frac{CD_i}{(CD_i+K_{s,D,i})}
\f]

Dabei sind die Halbsättigungskonstanten Parameter, die vorgegeben werden müssen 
(APARAM.txt KsD1e 18|1, KsD2e 18|2).\n
Die Konzentrationen \f$ CD_i \f$, die hier verwendet werden, sind die aus dem 
vorangegangenen Zeitschritt, zu dem bereits die \ref lnk_hyp hinzuaddiert wurde.
\n

Die maximale Hydrolyserate für die 1. leicht abbaubaren gelösten C-Verbindungen 
wird ebenfalls vorgegeben (APARAM.txt hymxDe 17|5), die für die 2. schwer abbaubaren 
wird in Abhängigkeit vom Verhältnis des biologischen zum chemischen Sauerstoffbedarf 
bestimmt:

\f{eqnarray*}{
 hy_{max,D,1}=& const.  =hymxDe \\
 hy_{max,D,2}=& min(0.474*(\frac{C-BSB_5}{CSB})^{-1.346},6)
\f}\n

Die Veränderungen werden explizit diskretisiert:

\f{eqnarray*}{
 \Delta {CM}_{hyd} = \frac{\partial CM}{\partial t}_{hyd} \cdot \Delta t  \\
 \Delta {CD_i}_{hyd} = \frac{\partial CD_i}{\partial t}_{hyd} \cdot \Delta t
\f}\n\n

Negative Werte werden ausgeschlossen, weil wo nichts ist, kann auch nichts 
verbraucht werden.
hartes Klipping bei:\f$ CD_i(t+\Delta t) > 0.00001 \f$ ist nicht massenerhaltend 
in Bezug auf :\f$ CM \f$


<code>
      vcb =  \ref obsb / \ref ocsb \n
      hymxD(1) = *hymxDe* \n
      hymxD(2) = 0.474*vcb**(-1.346)  \n
      if(hymxD(2)>6.)hymxD(2) = 6.  \n
      hyP(1) = *hyPe*  \n
      hyP(2) = 1.51*vcb**2.31 \n
      ksD(1) = *KsD1e*  \n
      ksD(2) = *KsD2e*  \n
</code> 
zurück zu: \ref lnk_orgC , Quelle: kohlenstoff-prozess.md 
<!-- alt: orgc_huelle.f95 -->
 

# Stoffwechsel der heterotrophen Bakterien {#lnk_BACm}

Die Zunahme der Masse von in heterotrophen Bakterien gespeicherten C-Verbindungen 
infolge der Aufnahme von monomolekularen C-Verbindungen
und der Rückgang infolge von Respiration (Atmung) wird wie folgt ermittelt:

\f[ 
 \frac{\partial HBAC}{\partial t}_{BAC} = (up_{HBAC}-res_{HBAC})*HBAC
\f]

Im gleichen Maße wie die Masse des Kohlenstoff in heterotrophen Bakterien zunimmt, 
muss die Konzentration von monomolekularen organischen Kohlenstoff abnehmen:

\f[ 
 \frac{\partial CM}{\partial t}_{BAC} = - up_{HBAC}*HBAC
\f]

Dabei berechnet sich die Aufnahmerate von Kohlenstoff durch heterotrophen 
Bakterien wie folgt:
\f[ 
 up_{HBAC}= up_{HBAC,max}* \frac{CM}{(CM+K_{s,M})}
\f]

Die in obiger Formel auftretende maximale Aufnahmerate und die Halbsättigungskonstante
sind Parameter, die vorgegeben werden (APARAM.txt upBACe 18|4, KsMe 18|3)\n
Die Gesamtmasse der monomolekularen organischen C-Verbindungen \f$ CM \f$ , 
die von der \ref  hyd im aktuellen Zeitschritt gebildet werden, sind für die 
Bakterien schon im aktuellen Zeitschritt verfügbar.
\n

Die Respirationsrate der heterotrophen Bakterien wird wie folgt ermittelt:

\f[ 
 res_{HBAC}= res_{G,HBAC}*f_T+up_{HBAC}*(1-Y)
\f]

Die in obiger Formel auftretende Grundrespirationsrate und der Ertragskoeffizient
sind Parameter, die vorgegeben werden (APARAM.txt rsGBACe 19|1 , YBACe 18|5 )\n

Ausserdem wird die Massenzunahme der Bakterien durch das 
Vorhandensein von monomolekularen organischen C-Verbindungen limitiert.\n

\f[ 
 up_{HBAC} < \frac{CM}{\Delta t * HBAC}
\f]\n

Die Veränderungen werden auch hier explizit diskretisiert:

\f{eqnarray*}{
 \Delta {HBAC}_{BAC} = \frac{\partial HBAC}{\partial t}_{BAC} \cdot \Delta t \\
 \Delta {CM}_{BAC} = \frac{\partial CM}{\partial t}_{BAC} \cdot \Delta t
\f}\n\n

Negative Werte werden ausgeschlossen, weil wo nichts ist, kann auch nichts 
verbraucht werden.
hartes Klipping bei:\f$ CM(t+\Delta t) > 0.00001 \f$ und \f$ HBAC(t+\Delta t) > 0.00001 \f$ 
ist nicht notwendigerweise massenerhaltend für die aufnehmende Größe.
\n

zurück zu: \ref lnk_orgC , Quelle: kohlenstoff-prozess.md
<!-- orgc_huelle.f95 -->
!                                                                     
# Aufnahme gelöster C-Verbindungen durch Organismen auf Makrophyten {#lnk_fluxmaphy} 
Sessile Organismen, die sich auf Makrophyten (Wasserpflanzen) als Biofilm 
anlagern, entziehen dem sie umgebenden Wasser gelöste C-Verbindungen. Diese 
Entnahme wird für die leicht und schwer abbaubare 
Fraktion der gelösten C-Verbindungen getrennt nach folgendem Ansatz (woher??) 
berechnet:

\f[ FluxD_1 = (0.62*{(CD_1+CD_2)}^{0.817})*(0.62*log(\frac{C-BSB_5}{CSB})+2.2)  \f]
\f[ FluxD_2 = (0.56*{(CD_1+CD_2)}^{0.916})*(-3.11*(\frac{C-BSB_5}{CSB})+1.407)  \f]
\f[ \Delta {CD_i}_{pfl} = -FluxD_i \cdot \frac{pfl}{300 h} \cdot f_t \cdot \Delta t \f] 

Der gesamte orgc- Baustein basiert auf einer tiefengemittelten Betrachtungsweise, 
was hier besonders deutlich wird.
\n\n

Es wird angenommen, dass die Biofilme auf Makrophyten sich nur aus dem Teil der 
gelösten organischen C-Verbindungen
\f$ CD_i \f$ bedienen können, der ihnen von der \ref lnk_hyd übriggelassen wurde.
\n\n

zurück zu: lnk_orgC , Quelle: kohlenstoff-prozess.md
<!-- zurück zu: \ref BSB , Quelle: orgc_huelle.f95 -->

# Absterben von lebender Biomasse {#lnk_mortalgC} 
Bei der absterbenden Algenbiomasse wird ein Kohlenstoffanteil von 48% angenommen.\n
Desweiteren wird angesetzt, dass sich die absterbende Algenbiomasse 
folgendermaßen in die verschiedenen Kohlenstofffraktionen verteilt: 

<ul> <li> 10% in die 1-leicht abbaubaren gelösten organischen C-Verbindungen, </li>
<li> 10% in die 2-schwer abbaubaren gelösten organischen C-Verbindungen, </li>
<li> 35% in die 1-leicht abbaubaren partikulären organischen C-Verbindungen, </li>
<li> 35% in die 2-schwer abbaubaren partikulären organischen C-Verbindungen und </li>
<li> 10% in die refraktären organischen C-Verbindungen </li></ul>

\n\n

Bei der Absterbenden Biomasse aus Heterotrophen Naloflagelaten und Rotatorien
wird angesetzt, dass sie sich in die verschiedenen
Kohlenstofffraktionen wie folgt verteilen: 

<ul> <li> 20% in die 1-leicht abbaubaren gelösten organischen C-Verbindungen, </li>
<li> 20% in die 2-schwer abbaubaren gelösten organischen C-Verbindungen, </li>
<li> 20% in die 1-leicht abbaubaren partikulären organischen C-Verbindungen, </li>
<li> 20% in die 2-schwer abbaubaren partikulären organischen C-Verbindungen und </li>
<li> 20% in die refraktären organischen C-Verbindungen </li></ul>

Bei der absterbenden Rotatorienbiomasse wird ein Kohlenstoffanteil von 40% angenommen.\n
\n

Dies führt auf folgende Formel \n 
*(Exemplarisch anhand der leicht abbaubaren gelösten organischen C-Verbindungen)*

\f{eqnarray*}{
 {\Delta CD_1}_{mort} &=& \\
 &+& 0.1 \cdot \left( dki_{mort} + dgr_{mort} + dbl_{mort}               \right) \cdot 0.48 \\
 &+& 0.2 \cdot \left( BSB_{HNF} + Rot_{mort}  \cdot \frac{G_{Rot}}{1000} \right) \cdot 0.4
\f}\n

zurück zu: \ref lnk_orgC , Quelle kohlenstoff-prozess.md
<!-- zurück zu: \ref BSB , Quelle: orgc_huelle.f95 -->
 
# Sedimentation kohlenstoffhaltigen Materials {#lnk_Sediorgc} 
Berechnungsverfahren dem Dokumentator noch unklar (wy 12dez12)\n \n 
    einfluss der Sedimentation \n                                       
      g = sqrt(9.81)  \n 
      ust = (((1/rau(ior))*g)/(tiefe(ior)**0.16667))*abs(vmitt(ior))  \n 
\f[ 
   u_* = \sqrt{ \frac{g}{ (K_{st})^2 \cdot h^{1/3} }  }  \cdot \sqrt{(\bar{u})^2}
\f]
     ASEDC = 1.44E-6  \n 
     BSEDC = 3.13  \n 
     wsgr = 0.625*ust**2.1  \n 
     qsgr = 1./(1+asedc*exp(-bsedc*alog10(wsgr)))  \n 
\f[
   q_{s,gr} = \frac{1}{ 1 + (1.44 \cdot 10^{-6}) \cdot e^{ -3.13 \cdot \log_{10}( 0.625 \cdot {u_*}^{2.1}) } }
\f]
      qssed = (1+qsgr)/2. \n
      WS = (LOG(1./QSSED-1)-LOG(ASEDC))/(-BSEDC) \n
      WS = 10**WS \n
      fwst = 1.14*exp(-188.2*ust) \n
      if(fwst>1.0)fwst = 1. \n
      wst = ws*fwst \n
\f[
   w_* = \left(1.14 \cdot  e^{(-188.2 \cdot u_*)} \right) \cdot 
10^{ \left( \log( \frac{2.}{1+q_{s,gr}}-1) - \log(1.44 \cdot 10^{-6})\right) / -3.13 }
\f]
      prop = 0.6  \n
      OC = 1./(EXP(prop*WST*TFLIE*86400./TIEFE(ior)))  \n
      OC = 1.-OC  \n
      CP1sd = fbsgr(ior)*CP(1,ior)  \n
      Ceq1 = CP1sd*qsgr  \n
      CP1sdt = CP1sd-Ceq1  \n
      sedCP1 = CP1sdt*oc  \n
      CPt(1) = CPt(1)-sedCP1  \n  
exemplarisch:                                                              
\f{eqnarray*}{
sedCP_1 &=& 
- \frac{ \left(f_{bs,gr} \cdot CP_1 \cdot q_{s,gr}\right)}{\Delta } \cdot 
  \left(1.- 1./e^{(0.6 \cdot w_* \cdot \Delta t \cdot 86400 / h)} \right) \\
sedCP_2 &=& 
- \frac{ \left(f_{bs,gr} \cdot CP_2 \cdot q_{s,gr}\right)}{\Delta } \cdot 
  \left(1.- 1./e^{(0.6 \cdot w_* \cdot \Delta t \cdot 86400 / h)} \right) \\
sedBAC &=& 
- \frac{ \left(f_{bs,gr} \cdot BAC \cdot q_{s,gr}\right)}{\Delta } \cdot 
  \left(1.- 1./e^{(0.6 \cdot w_* \cdot \Delta t \cdot 86400 / h)} \right) \\
sedC_{ref} &=& 
  andere \cdot Werte + sonst + wie + oben \\
\\
\f}
!!
<h2>Sedimentation von Schwebstoff</h2>
                                      
      SSSED = fssgr(ior)*SS(ior) \n
      sedimentation() -> ,qsgr,oc \n
      ceq = sssed*qsgr ) \n
      sedss(ior) = max(0.0,(sssed-ceq)) * oc) \n
) \n
\f[
 sedss = SS(ior) * fssgr(ior) *(1-qsgr) * oc
\f]
mit:  \ref fssgr Faktor zur Berechnung der ablagerungsfreien Grenzkonzentration von Schwebstoff \n
      hc1 = SS(ior)-sedss(ior)+exzo+dkimor(ior) \n 
      hc1 = hc1+dgrmor(ior)+dblmor(ior)+zomor-ssdr(ior) \n 
      hc1 = hc1+dorgSS(ior)+drfaek(ior)+drfaeg(ior) \n 
      hc1 = hc1+drfaeb(ior)+drfaes(ior) \n 
      hc2 = sssed-sedss(ior)+exzo+dkimor(ior) \n 
      hc2 = hc2+dgrmor(ior)+dblmor(ior)+zomor-ssdr(ior) \n 
      hc2 = hc2+dorgSS(ior)+drfaek(ior)+drfaeg(ior) \n 
      hc2 = hc2+drfaeb(ior)+drfaes(ior)\n 
      fssgr(ior) = hc2/hc1 \n 
\f[
 qsgr = 1./(1+ased \cdot e^{(-bsed \cdot log_{10}(0.14 \cdot {u_*}^2 + 0.0054 \cdot u_* + 1.25e-6))})
\f]
      wsgr =  0.14*ust**2+0.0054*ust+1.25e-6(0.14 * {u_*}^2 + 0.0054 * u_* + 1.25e-6) 
      qsgr = 1./(1+ased*exp(-bsed*alog10(wsgr)))  \n 
mit ased = 1.55e-7 ;  bsed = 2.8
\f[
 oc = 1.-(1./e^{(prop \cdot w_* \cdot \Delta t / h )}
\f] 
mit prop=0.75 und
\f[
w_* = (log(1./((1+(1./(1+ased*exp(-bsed*alog10((0.14 \cdot {u_*}^2 + 0.0054 \cdot u_* + 1.25e-6))))))/2.)-1.)-log(ased))/(-bsed) \cdot e^{(-604.2 \cdot u_*)}
\f] 
      qsgr = 1./(1+ased*exp(-bsed*alog10(wsgr)))  \n
      qssed = (1+qsgr)/2.  \n
      ws = (log(1./qssed-1.)-log(ased))/(-bsed) \n
      fwst = exp(-604.2*ust) \n
      wst = ws*fwst  \n

\n\n

<h2> Neuberechnung des Faktors zur Berechnung der ablagerungsfreien Grenzkonzentration</h2> 
      hc1 = CP(2,ior)-sedCP2+0.2*BSBHNF(ior)+0.2*dkimor(ior)*0.48+0.2*dgrmor(ior)*0.483+0.2*dblmor(ior)*0.48 \n                      
      hc1 = hc1+0.2*abszo(ior)*(GRote/1000.)*0.4 \n
      hc1 = hc1+0.2*zexki(ior)*0.48+0.2*zexgr(ior)*0.483+0.2*drfaek(ior)*0.48+0.2*drfaeg(ior)*0.483 \n                      
      hc1 = hc1+0.2*zexbl(ior)*0.48+0.2*drfaeb(ior)*0.48\n
      hc1 = hc1-(ssdr(ior)*0.3*0.4*(CP(2,ior)/(CP(1,ior)+CP(2,ior)+0.1*Cref)))   \n                    
                                                                       \n
      hc2 = CP2sd-sedCP2+0.2*BSBHNF(ior)+0.2*dkimor(ior)*0.48+0.2*dgrmor(ior)*0.483+0.2*dblmor(ior)*0.48\n                       
      hc2 = hc2+0.2*abszo(ior)*(GRote/1000.)*0.4 \n
      hc2 = hc2+0.2*zexki(ior)*0.48+0.2*zexgr(ior)*0.483+0.2*drfaek(ior)*0.48+0.2*drfaeg(ior)*0.483 \n                      
      hc2 = hc2+0.2*zexbl(ior)*0.48+0.2*drfaeb(ior)*0.48 \n
      hc2 = hc2-(ssdr(ior)*0.3*0.4*(CP(2,ior)/(CP(1,ior)+CP(2,ior)+0.1*Cref)))    \n                   
                       \n                                                
      fbsgrt = hc2/hc1 \n
\n \n
      hc1 = Cref-sedCrf+0.2*BSBHNF(ior)+0.2*dkimor(ior)*0.48+0.2*dgrmor(ior)*0.483+0.2*dblmor(ior)*0.48   \n                     
      hc1 = hc1+0.2*abszo(ior)*(GRote/1000.)*0.4  \n
      hc1 = hc1+0.2*zexki(ior)*0.48+0.2*zexgr(ior)*0.483+0.2*drfaek(ior)*0.48+0.2*drfaeg(ior)*0.483     \n                   
      hc1 = hc1+0.2*zexbl(ior)*0.48+0.2*drfaeb(ior)*0.48  \n
      hc1 = hc1-(ssdr(ior)*0.3*0.4*(cref/(CP(1,ior)+CP(2,ior)+0.1*Cref)))                         \n    
                                                                        \n
      hc2 = Crfsd-sedCrf+0.2*BSBHNF(ior)+0.2*dkimor(ior)*0.48+0.2*dgrmor(ior)*0.483+0.2*dblmor(ior)*0.48    \n                    
      hc2 = hc2+0.2*abszo(ior)*(GRote/1000.)*0.4  \n
      hc2 = hc2+0.2*zexki(ior)*0.48+0.2*zexgr(ior)*0.483+0.2*drfaek(ior)*0.48+0.2*drfaeg(ior)*0.483    \n                    
      hc2 = hc2+0.2*zexbl(ior)*0.48+0.2*drfaeb(ior)*0.48  \n
      hc2 = hc2-(ssdr(ior)*0.3*0.4*(cref/(CP(1,ior)+CP(2,ior)+0.1*Cref)))   \n                          
                                                                   \n     
     frfgrt = hc2/hc1 \n 
\n
zurück zu: \ref BSB     , Quelle: orgc_huelle.f95                                                                 

# Schwebstoffaufnahme der Dreissena Muscheln {#lnk_Cschwebdreiss} 
Dieser Teilprozess ist momentan ausgeschaltet, weil in
dreissen.f90: ".....Schwebstoffaufnahme durch Dreissena wird vorläufig auf Null gesetz". \n 
Teil-Prozess momentan fachlich unklar.\n\n
Die hier codierte Formel würde annehmen, dass von dem durch Dreissena-Muscheln aufgenommenen Schwebstoff
30% partikuläre C-Verbindungen sind und dass die Muscheln davon 60% ungenutzt wieder ausscheiden (oder andersrum).
Vom refraktären Kohlenstoff wird angenommen, dass nur 10% partikulär vorliegen.\n
\f[ 
 \Delta {CP_1}_{drs} = - ssdr \cdot 0.3 \cdot 0.4 \cdot \frac{CP_1}{CP_1 + CP_2 + 0.1 \cdot C_{ref}}
\f]

\f[ 
 \Delta {CP_2}_{drs} = - ssdr \cdot 0.3 \cdot 0.4 \cdot \frac{CP_2}{CP_1 + CP_2 + 0.1 \cdot C_{ref}}
\f]

\f[ 
 \Delta {C_{ref}}_{drs} = - ssdr \cdot 0.3 \cdot 0.4 \cdot \frac{C_{ref}}{CP_1 + CP_2 + 0.1 \cdot C_{ref}}
\f]


zurück zu: \ref BSB , Quelle: orgc_huelle.f95
 
# Verlust der Bakterien durch HNF-Grazing {#lnk_bacHNFgraz}
Heterotrophe Nanoflagelaten konsumieren (fressen, engl.: to graze) Bakterien.
Die Masse an C-Verbindungen in Bakterien, die dadurch je Zeitschritt verloren geht,
wird in der Subroutine HNF() ermittelt. \n                          
\f[ 
 \Delta {HBAC}_{HNF} = - HNFBAC
\f]
Klipping bei:\f$ HBA(t+\Delta t) > 0.00001 \f$

zurück zu: \ref BSB

# Ausscheidungen von Zooplanktern und Dreissena-Muscheln {#lnk_facesC} 
Verbunden mit der Nahrungsaufnahme exkretieren Zooplankter (Rotatorien) und Dreissena-Muscheln
sogenannte Faeces. Genauso wie bei der absterbenden Algenbiomasse wird davon ausgegangen, dass diese Faeces
ein Kohlenstoffanteil von 48% haben. Und ebenfalls genauso wie bei der absterbenden Algenbiomasse wird die 
Verteilung in die verschiedenen Kohlenstofffraktionen angesetzt: \n
<ul> <li> 10% in die 1-leicht abbaubaren gelösten organischen C-Verbindungen, </li>
<li> 10% in die 2-schwer abbaubaren gelösten organischen C-Verbindungen, </li>
<li> 35% in die 1-leicht abbaubaren partikulären organischen C-Verbindungen, </li>
<li> 35% in die 2-schwer abbaubaren partikulären organischen C-Verbindungen und </li>
<li> 10% in die refraktären organischen C-Verbindungen </li></ul>\n
Untenstehend sei hier exemplarisch für alle Kohlenstofffraktionen nur die Formel für die
1-leicht abbaubaren partikulären organischen C-Verbindungen angegeben:\n
\f{eqnarray*}{
 \Delta {CP_1}_{faec} &=& 0.35 \cdot \\
 &[& \\
 &+& 0.48 \cdot \left( ROT_{faec,Ki} + ROT_{faec,Gr} + ROT_{faec,Bl}\right)  \\
 &+& 0.48 \cdot \left( DR_{faec,Ki} + DR_{faec,Gr} + DR_{faec,Bl}   \right)  \\
 &]& \\
\f}

zurück zu: \ref BSB  , Quelle: orgc_huelle.f95                                                                    

# Sauerstoff-Verbrauch {#lnk_o2zehr} 
<h1> aktuell stattfindender Sauerstoff-Verbrauch </h1>
bei den folgenden Prozesse beim Umsatz von C-Verbindungen kommt es zum Verbrauch von Sauerstoff:
<ul> <li> \ref lnk_BACm und </li>
<li> \ref lnk_fluxmaphy. </li></ul>\n\n

\f[ 
   \Delta {O_2}_{BAC} = res_{HBAC} \cdot HBAC \cdot O_2dC
\f]

\f[ 
  \Delta {O_2}_{pfl} = \left(0.758 \left(CD_1 + CD_2 \right) + 0.21 \right)  \cdot  
       \left( -5.476 \left(\frac{C-BSB_5}{CSB}\right)^2 + 2.256 \frac{C-BSB_5}{CSB} + 0.789 \right)
       \cdot   ( \frac{pfl}{300  \cdot h} \cdot f_T )
\f]

Allerdings scheint der obenstehende Ansatz für den Sauerstoffverbrauch nicht 
mit den Ansätzen zur \ref lnk_fluxmaphy 
zusammenzupassen.

\f[ 
     \Delta {O_2}_{orgC} = \Delta {O_2}_{BAC} + \Delta {O_2}_{pfl}
\f]

<h1> Zehrungsfähigkeit der C-Verbindungen </h1>

<h2> biologischer Sauerstoffbedarf in 5 Tagen </h2>
Bei den hier als leicht zehrbar bilanzierten C-Verbindungen wird davon ausgegangen, dass mehr als 92% in 5 Tagen
biologisch abgebaut werden. Die monomolekularen C-Verbindungen
und 40% der in Bakterien und Nanoflagelaten gespeicherten C-Verbindungen werden ebenfalls 
als leicht abbaubar angesetzt.\n
Bei den  hier als schwer abbaubar bilanzierten C-Verbindungen wird davon ausgegangen, dass sich nur knapp 10%
in 5 Tagen biologisch abbauen lassen. 40% der in Bakterien und Nanoflagelaten gespeicherten C-Verbindungen werden
als schwer abbaubar angenommen.\n

\f{eqnarray*}{
 C-BSB_5 &=& O_2dC \cdot \\
 &[& \\
 &+& 0.922 \cdot \left[ CD_1 + CP_1 + CM + 0.4 \cdot \left( HBAC + C_{HNF} \right) \right]  \\
 &+& 0.095 \cdot \left[ CD_2 + CP_2      + 0.4 \cdot \left( HBAC + C_{HNF} \right) \right]  \\
 &]& \\
\f}
                                                                  
<h2> chemischer Sauerstoffbedarf</h2>
Nahezu der gesamte im Wasserenthaltene Kohlenstoff kann auf Chemischem Wege oxidiert werden.\n
Lediglich bei dem in Bakterien und Nanoflagelaten gebundenen Kohlenstoff wird davon ausgegangen,
dass nur 80% chemisch zehrungsfähig ist, was der Summe des biologisch abbaubaren Materials entspricht.\n
Fernerhin wird angenommen, dass ein kleiner Teil des chemischen Sauerstoffbedarfs nicht zur Bildung von 
Kohlendioxid verwendet wird (2.8 statt 2.667). \n
Die Neuberechnung erfasst den Zustand nach erfolgtem Stoffumsatz. D. h. \f$ CD_1 = CD_1(t+\Delta t)\f$

\f[ 
     CSB = \left[ C_{ref} + CD_1 + CD_2 + CP_1 +CP_2 + CM + 0.8 \cdot \left( HBAC + C_{HNF} \right) \right] \cdot 2.8 
\f]

\n\n

zurück zu: \ref BSB , Quelle: orgc_huelle.f95
 
# Ammonium-Freisetzung {#lnk_nh4freis} 
<h2> Freisetzung von Ammonium </h2>
Bei der Umwandlung von C-Verbindungen in Kohlendioxyd
wird der in ersteren gebundene Stickstoff als Ammonium freigesetzt:

\f[ 
   BSB_{NH4} = BSB_{C} \cdot N_{org}
\f]

\n

<h2> Neuberechnung Stickstoffgehalt </h2>
Beim \ref lnk_mortalgC Absterben von lebender Biomasse 
und bei den \ref lnk_facesC Ausscheidungen von Zooplanktern und Dreissena-Muscheln
geht nicht nur deren Kohlenstoffanteil in die hier bilanzierten C-Verbindungen über,
sondern es wird auch deren Stickstoffgehalt freigesetzt. Da diese aus Biomasse mit unterschiedlichen 
Stickstoffgehalten stammen, muss hier eine Neuberechnung des Gesamt-Stickstoffgehalts der 
organischen C-Verbindungen stattfinden:

\f{eqnarray*}{
   orgN(t + \Delta t) &=& orgN(t) \\
                      &+& BSB_{HNF}  \cdot  f_N \\
                      &+& dki_{mort} \cdot Q_{N,Ki} + dgr_{mort} \cdot Q_{N,Gr} + dbl_{mort} \cdot Q_{N,Bl}\\
                      &+& Rot_{mort} \cdot (G_{Rot}/1000.) \cdot N_R\\
                      &+& ROT_{faec,Ki} \cdot Q_{N,Ki} + ROT_{faec,Gr} \cdot Q_{N,Gr} + ROT_{faec,Bl} \cdot Q_{N,Bl}\\
                      &+& DR_{faec,Ki} \cdot Q_{N,Ki} + DR_{faec,Gr} \cdot Q_{N,Gr} + DR_{faec,Bl}  \cdot Q_{N,Bl}\\
\f}

Das Verhältnis von Stickstoff zu Kohlenstoff ergibt sich dann aus der Division des Gesamt-Stickstoffgehalts 
durch den Kohlenstoffanteil im CSB.   (2.8 mgC/mgO2)         

\f[ 
     N_{org}(t + \Delta t) = \frac{orgN(t + \Delta t)}{CSB(t + \Delta t)/2.8}
\f]

mit:

zurück zu: \ref BSB , Quelle: orgc_huelle.f95

# Phosphat Freisetzung {#lnk_pfreis} 
<h2> Freisetzung von ortho-Phospshat </h2>
Bei der Umwandlung von C-Verbindungen in Kohlendioxyd
wird der in ersteren gebundene Phosphor als ortho-Phosphat freigesetzt:

\f[ 
   BSB_{PO4} = BSB_{C} \cdot P_{org}
\f]


<h2> Neuberechnung Phosphatgehalt </h2>
Beim \ref lnk_mortalgC Absterben von lebender Biomasse 
und bei den \ref lnk_facesC Ausscheidungen von Zooplanktern und Dreissena-Muscheln
geht nicht nur deren Kohlenstoffanteil in die hier bilanzierten C-Verbindungen über,
sondern es wird auch deren Phosphatgehalt freigesetzt. Da diese aus Biomasse mit unterschiedlichen 
Phosphatgehalten stammen, muss hier eine Neuberechnung des Gesamt-Phosphatgehalts der 
organischen C-Verbindungen stattfinden:

\f{eqnarray*}{
   orgP(t + \Delta t) &=& orgP(t) \\
                      &+& BSB_{HNF}  \cdot  f_P \\
                      &+& dki_{mort} \cdot Q_{P,Ki} + dgr_{mort} \cdot Q_{P,Gr} + dbl_{mort} \cdot Q_{P,Bl}\\
                      &+& Rot_{mort} \cdot (G_{Rot}/1000.) \cdot P_R\\
                      &+& ROT_{faec,Ki} \cdot Q_{P,Ki} + ROT_{faec,Gr} \cdot Q_{P,Gr} + ROT_{faec,Bl} \cdot Q_{P,Bl}\\
                      &+& DR_{faec,Ki} \cdot Q_{P,Ki} + DR_{faec,Gr} \cdot Q_{P,Gr} + DR_{faec,Bl}  \cdot Q_{P,Bl}\\
\f}

Das Verhältnis von Phosphor zu Kohlenstoff ergibt sich dann aus der Division des Gesamt-Phosphorgehalts 
durch den Kohlenstoffanteil im CSB.   (2.8 mgC/mgO2)         

\f[ 
     P_{org} (t + \Delta t) = \frac{orgP(t + \Delta t)}{CSB(t + \Delta t)/2.8}
\f]



zurück zu: \ref BSB , Quelle: orgc_huelle.f95

# Änderung des ph-Werts durch Bildung von Kohlensäure {#lnk_kohlensauer} 
Das beim Abbau von C-Verbindungen entstehende Kohlendioxyd (CO2) verändert den \ref lnk_ph.
Dort wird die CO2 Lieferung aus C-abbau aus \f$ \Delta O_2 \f$ bestimmt, das hier im 
Zusammenhang mit dem \ref lnk_o2zehr berechnet wurde. \n
<table>
<tr><td> \f$ \Delta O_2 \f$  </td><td> \ref bsbt </td><td> Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt </td><td> mgO2/l zeitschrittbezogen </td><td> ? </td></tr>
</table>\n\n
zurück zu: \ref BSB , Quelle: orgc_huelle.f95

# Schwebstoffgehalt {#lnk_schwebkohl} 
Zur Schwebstoffmasse tragen die partikulär vorliegenden C-Verbindungen bei, dies sind 

\f$ CP_i \f$, \f$ HBAC \f$ und 20% von \f$ C_{ref} \f$

( hier ergibt sich ein Widerspruch mit der Annahme von 10% bei: \ref lnk_Cschwebdreiss .\n
Es wird fernerhin angesetzt, dass Kohlenstoff die Hälfte dieser Schwebstoffmasse ausmacht.\n

\f{eqnarray*}{
   \Delta SS_{org} &=& 2 \cdot \\
                  &[&  \\
                  &+& CP_1(t+\Delta t) - CP_1(t) + sedCP_1 \\
                  &+& CP_2(t+\Delta t) - CP_2(t) + sedCP_2 \\
                  &+& HBAC(t+\Delta t) - HBAC(t) + sedBAC \\
                  &+& 0.2 \cdot \left( C_{ref}(t+\Delta t) - C_{ref}(t) + sedC_{ref} \right) \\
                  &]& 
\f}

Wie obige Formel zeigt, sind in der Größe \f$ \Delta SS_{org} \f$, die an schweb() übergeben wird,
 die sedimentierten Partikel mit enthalten !  \n\n


zurück zu: \ref BSB , Quelle: orgc_huelle.f95

# Sedimentation {#lnk_sedkohl} 
Die Gesamtmasse des sedimentierten Kohlenstoffs wird aufsummiert und nachfolgenden Modulen zur Verfügung gestellt:
\ref Stickstoff , \ref Phosphor , SedFlux().\n

\f[ 
   orgC_{sed} = sedCP_1 + sedCP_2 + sedBAC + sedC_{ref} 
\f]


zurück zu: \ref BSB , Quelle: orgc_huelle.f95

<!--  ----------------------------Bilanzen----------------------- -->
# Konzentration von partikulären organischen C-Verbindungen {#lnk_bilaCP} 
Die Konzentration der partikulären organischen C-Verbindungen
ändert sich aufgrund der folgenden Teilprozesse: 
<ul> <li> \ref lnk_hyp, </li>
<li> \ref lnk_Sediorgc, </li>
<li> \ref lnk_mortalgC, </li>
<li> \ref lnk_Cschwebdreiss (z.Z ausgeschaltet), </li>
<li> \ref lnk_facesC. </li></ul>\n\n
Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
plus der Summe der Veränderungen:

\f[ 
CP_i(t+\Delta t) = CP_i(t) 
+ \Delta {CP_i}_{hyp} + \Delta {CP_i}_{sed}
+ \Delta {CP_i}_{mort} + \Delta {CP_i}_{drs} + \Delta {CP_i}_{faec} 
\f]

Negative Werte werden ausgeschlossen, zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
welches nicht massenerhaltend ist.\n

\f[ 
 CP_i(t+\Delta t)_{klipp}= \frac{CP_i(t)}{ 2 + \left|CP_i(t+\Delta t)/CP_i(t)\right|} 
\f]

mit:

zurück zu: \ref BSB , Quelle: orgc_huelle.f95

# Konzentrationen von gelöstenen organischen C-Verbindungen {#lnk_bilaCD} 
Die Konzentrationen von gelöstenen organischen C-Verbindungen ändert sich aufgrund der folgenden Teilprozesse: 
<ul> <li> \ref lnk_hyp, </li>
<li> \ref lnk_hyd, </li>
<li> \ref lnk_fluxmaphy, </li>
<li> \ref lnk_mortalgC und </li>
<li> \ref lnk_facesC. </li></ul>\n\n
Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
plus der Summe der Veränderungen:

\f[ 
 CD_i(t+\Delta t) = CD_i(t)
 + (\Delta CD_i)_{hyp} + (\Delta CD_i)_{hyd} + (\Delta CD_i)_{pfl} + (\Delta CD_i)_{mort} + (\Delta CD_i)_{faec}
\f]

Negative Werte werden ausgeschlossen. Zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
welches auch nicht massenerhaltend ist.\n

zurück zu: \ref BSB , Quelle: orgc_huelle.f95

# Konzentration von monomolekularen organischen Kohlenstoff {#lnk_bilaCM} 
Die Konzentration von monomolekularen organischem Kohlenstoff
ändert sich aufgrund der folgenden Teilprozesse: 
<ul> <li> \ref lnk_hyd, </li>
<li> \ref lnk_BACm, </li>
<li> \ref lnk_mortalgC und </li>
<li> \ref lnk_facesC. </li></ul>\n\n
Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
plus der Summe der Veränderungen:

\f[ 
 CM(t+\Delta t) = CM(t) + \Delta {CM}_{hyd} + \Delta {CM}_{BAC} + (\Delta CM)_{mort} + (\Delta CM)_{faec} 
\f]

Negative Werte werden ausgeschlossen. Zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
welches auch nicht massenerhaltend ist.\n

zurück zu: \ref BSB

# Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen  {#lnk_bilaBAC} 
Die Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
ändert sich aufgrund der folgenden Teilprozesse: 
<ul> <li> \ref lnk_BACm, </li>
<li> \ref lnk_bacHNFgraz und </li>
<li> \ref lnk_Sediorgc. </li></ul>\n\n
Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
plus der Summe der Veränderungen:

\f[ 
 HBAC(t+\Delta t) = HBAC(t) + \Delta {HBAC}_{BAC} + \Delta {HBAC}_{HNF} + \Delta {HBAC}_{sed}
\f]

Negative Werte werden ausgeschlossen. Zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
welches auch nicht massenerhaltend ist.\n
Abgesehen von Massenverlusten durch Respiration, HNF-Grazing und Sedimentation sind Bakterien unsterblich.

zurück zu: \ref BSB , Quelle: orgc_huelle.f95

# refraktäre (nicht abbaubare) C-Verbindungen  {#lnk_bilaCref} 
Die Konzentration der refraktären (nicht abbaubaren) C-Verbindungen
ändert sich aufgrund der folgenden Teilprozesse: 
<ul> <li> \ref lnk_facesC, </li>
<li> \ref lnk_Cschwebdreiss (z.Z ausgeschaltet), </li>
<li> \ref lnk_Sediorgc, </li>
<li> \ref lnk_mortalgC und </li>
<li> \ref lnk_facesC. </li></ul>\n\n
zu Beginn der Berechnungen in orgc() wird der Gehalt an refraktären (nicht abbaubaren) C-Verbindungen
aus dem chemischen Sauerstoffbedarf durch Abzug aller anderen C-Verbindungen rückgerechnet
(Dies ist notwendig, weil \f$ C_{ref} \f$ keine transportierte, planktische Feldgröße ist).

\f[ 
     C_{ref} = \frac{1}{2.8} \cdot CSB - CP_1 - CP_2 - CD_1 - CD_2  - CM - 0.8 \cdot HBAC  - 0.8 \cdot C_{HNF}                    
\f]

Dabei wird angenommen, dass ein kleiner Teil des chemischen Sauerstoffbedarfs nicht zur Bildung von 
Kohlendioxid verwendet wird (2.8 statt 2.667). Ausserdem wird angenommen, dass nur 80% des Kohlenstoffs in
Bakterien und Nanoflagelaten chemisch zehrungsfähig ist.
\n\n
Die Konzentration im nächsten Zeitschritt berechnet sich aus derjenigen im vorangegangen Zeitschritt
plus der Summe der Veränderungen:

\f[ 
 C_{ref}(t+\Delta t) = C_{ref}(t) + \Delta C_{ref,fac}
 + \Delta C_{ref,drs} + \Delta C_{ref,sed} + \Delta C_{ref,mort} + \Delta C_{ref,faec}
\f]

Negative Werte werden ausgeschlossen. Zuletzt durch sanftes Klipping (asymtotische Nullannäherung), 
welches auch nicht massenerhaltend ist.\n

zurück zu: \ref BSB , Quelle orgc_huelle.f95 
 
 
# QSim-Veröffentlichungen, die den Kohlenstoff-Baustein beschreiben und/oder anwenden: 
<!-- #mf: noch aufräumen -->
Der orgc-Baustein wurde verwendet, um die Zehrung von schwer abbaubarem organischen Kohlenstoff für den Vortrag:\n
<a href="./pdf/FutureEms_Schoel2013.pdf"  target="_blank">
 Oxygen - Conceptual Model and QSim Approach</a>
 \n Schöl et al. 2013 \n zu simulieren. 
 \n\n
 Siehe dazu auch :Validierung orgc in \ref lnk_validierung
 (war mal ./vali/orgc_oxygen.html )
 \n
 

\n\n


Textquelle: kohlenstoff-prozess.md; Codesource: orgC.f90; orgc_huelle.f95; 
zurück: \ref lnk_orgC