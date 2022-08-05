Organischer Kohlenstoff - Umsetzung {#lnk_orgc_umsetzung}
========================= 

## Herkunft ##
SUBROUTINE orgc()\n
Programm zur Berechnung des biochemischen Sauerstoffbedarfs (BSB)\n
AUTOR : VOLKER KIRCHESCH      \n                                   
entnommen aus Version qsim13.301_28mae18\n 

## Schnittstellenbeschreibung ##

<code>
call orgc() \ref obsb, \ref ocsb, \ref tiefe, \ref rau, \ref tflie, \ref vmitt
, \ref flae, \ref zooind, \ref abszo, \ref tempw, \ref vbsb, \ref bsbt, \ref flag, \ref elen, 
\ref ior, \ref anze  &  \n                 
, \ref ecsb, \ref ebsb, \ref qeinl, \ref vabfl, \ref sdbsb, \ref zexki, \ref zexgr
, \ref bsbbet, \ref dkimor, \ref dgrmor, \ref jiein, \ref bsbgr, \ref bsbki, \ref akbcm &\n
, \ref agbcm, \ref pfl, \ref ezind, \ref abl, \ref abbcm, \ref bsbbl, \ref csbbl
, \ref dblmor, \ref zexbl, \ref drfaeb, \ref csbki, \ref csbgr, \ref ischif, \ref echla &\n
, \ref evkigr, \ref eantbl, \ref aki, \ref agr, \ref drfaek, \ref drfaeg
, \ref drfaes, \ref ssdr, *orgcsd*, \ref orgcsd0, \ref orgcsd_abb, *cd*, *cp*, *cm*, *bac*, *ecd* &\n
, *ecp*, *ecm*, *ebac*, \ref toc_csb, *grote*, \ref vcsb, \ref vkigr
, \ref antbl, \ref hnfbac, \ref bsbhnf, \ref chnf, \ref zbac &\n
, \ref bvhnf, \ref echnf, \ref fbsgr, \ref frfgr, \ref fbsgrs, \ref frfgrs
, \ref bacmua, \ref dorgss, \ref ilbuhn, \ref iwied, \ref fkm, \ref bsbct, *qeinll* &\n
, \ref iorla, \ref iorle, \ref ieinls, \ref pl0, \ref q_pk, \ref q_pb, \ref q_pg
, *pzoo*, \ref nl0, \ref q_nk, \ref q_nb, \ref q_ng, \ref nzoo, *etemp*, \ref bsbctp &\n
, \ref don, *hsdflub*, \ref hype, \ref hymxde, \ref ksd1e, \ref ksd2e
, \ref ksme, \ref upbace, \ref jdoc1, \ref jdoc2, \ref ybace, \ref rsgbace &\n
, \ref nkzs, \ref mstr, \ref itags, \ref monats, \ref uhrz, \ref  azstrs
, *bsbzoo* &\n
, \ref kontroll, *i*) wy\n      
</code> \n

## Randbedingungen ergänzen/erschließen ##
An Rändern kann nur der C-BSB5 und CSB-Wert vorgegeben werden. Intern werden aber
Bilanzen für die o.g. 7 Konzentrationen aufgestellt. 
Aufgrund plausibler Annahmen wird eine Aufteilung in der Subroutine 
orgc_start() vorgenommen siehe dazu \ref orgc_aufteilung.
Siehe dazu auch \ref lnk_randbedingungen_ergaenzen .
<!-- #mf: welche 7 Konz. sind gemeint? (bezieht sich auf Listen in lnk_orgC_prozesse -->


## Aufteilung des organischen Kohlenstoffs {#lnk_orgc_aufteilung}

Nicht alle in der Simulation verwendeten Größen können in Gewässern (einfach) 
gemessen werden. Daher ergibt sich die Frage, welche Werte für nicht gemessene 
Größen an den Rändern des Simulationsgebiets für das einströmende Wasser 
angesetzt werden sollen.

In Bezug auf \ref lnk_orgC werden als Randwerte
nur der \f$ C-BSB5_{Rand} \f$, d. h. der biologische Sauerstoffbedarf in 5 Tagen
und der \f$ CSB_{Rand} \f$,  d. h. der chemische Sauerstoffbedarf vorgegeben.
Ausserdem können die \f$ C_{HNF} \f$,  d. h. die Masse des Kohlenstoffs in 
heterotrophen Nanoflagelaten (*wie will man denn das messen?*)
und das  \f$ V_{HNF} \f$,  d. h. das Biovolumen der HNF vorgegeben werden.

Eine Aufteilung in die Berechnungs-Variablen geschieht dann mittels empirischer 
Ansätze. Für \ref lnk_orgC ist diese Aufteilung jetzt in die Subroutine 
orgc_start() ausgelagert worden.
Der entsprechende Programmteil wurde aus dem QSim1D Hauptprogramm entnommen.

Für \ref lnk_orgC wird folgende empirische Aufteilung angesetzt:\n
Zuerst werden die Sauerstoffbedarfe abgezogen, die nicht aus dem organischen 
Kohlenstoff, sondern aus den Algen und dem Zooplankton kommen:
\f[
      C-BSB5 = C-BSB5_{Rand}-algb5-zoobsb
\f]\f[
      CSB = CSB_{Rand}-algcs-zoocsb
\f]

Dann werden Minima erzwungen, d.h. am Rand sind immer mindestens 
\f$ C-BSB5 = 0.25 mg/l \f$ und \f$ CSB = 2.5 mg/l \f$

daraufhin wird ein Verhältnis aus den beiden Sauerstoffbedarfen gebildet:
\f[
  v = \frac{C-BSB5}{CSB}
\f]

Die monomolekularen organischen Kohlenstoffverbindungen werden mit immer dem 
gleichen kleinen Wert angesetzt:
\f[
 CM = 0.03
\f]

Als nächster Schritt erfolgt dann folgender Ansatz für den Gesamtgehalt an 
organischem Kohlenstoff (total organic carbon)
\f[
      TOC = 0.25 \cdot C-BSB5 \cdot 0.782 \cdot v^{-0.921}
\f]

Dieser wird dann aufgeteilt in gelöste und partikuläre Anteile:
\f[
      CD_{ges} = BTOC \cdot \alpha_D
\f]

\f[
      CP_{ges} = BTOC \cdot (1-\alpha_D)
\f]

mit:
\f[  \alpha_D = 0.21 \cdot ln(v) + 0.629     \f]

Die Aufteilung in der gelösten Fraktion in leicht(1) und schwer(2) abbaubar 
geschieht mittels folgenden Ansatzes: \n
(Die Setzung des monomolekularen Anteils auf 0.03 ist bereits erfolgt)
ausserdem wird noch die C-Masse der heterotrophen Nanoflagelaten abgezogen:
\f[
      CD_1 = ( (CD_{ges}-CM) \cdot \alpha_l ) - C_{HNF}
\f]
\f[
      CD_2 = ( (CD_{ges}-CM) \cdot (1-\alpha_l) ) - C_{HNF}
\f]
mit:
\f[  \alpha_l = 0.218 \cdot ln(v) + 0.717  \f]

In ähnlicher Art werden die partikulären Fraktionen in leicht(1) und schwer(2) 
abbaubar aufgeteilt:
auch hier wird noch die C-Masse der heterotrophen Nanoflagelaten abgezogen:
\f[
      CP_1 = ( (CP_{ges} \cdot 0.9538 \cdot v ) \cdot \alpha_P ) - C_{HNF}
\f]
\f[
      CP_2 = ( (CP_{ges} \cdot 0.9538 \cdot v ) \cdot (1-\alpha_P) ) - C_{HNF}
\f]
mit:
\f[  \alpha_P =  0.426 \cdot ln(v)+1.114 \f]

mittels des Faktors  0.9538 war vorher noch der Anteil abgetrennt worden,
der in den heterotrophen Bakterien gespeichert ist:
\f[
      HBAC = ( CP_{ges} \cdot 0.0462 \cdot v )
\f]

Zur Ermittlung der Sauerstoffäquivalente und des 
Sauerstoff-Kohlenstoffverhältnisse wird dann zunächst eine Hilfsvariable bx1 
gebildet

\f[
  bx1 = {\alpha_D}^2 + (1. - 0.0462 \cdot v) \cdot (1 - \alpha_D) \cdot 
  \alpha_P + 0.0462 \cdot v \cdot (1. - \alpha_D) \cdot 0.4\n
\f]

Mit den Parametern bk1 = 0,51 und bk2 = 0,02 bestimmt sich dann das 
Gesamt-Sauerstoffäquivalent zu:

\f[
 BL = \frac{C-BSB5}{1. - (bx1 \cdot e^{-bk1 \cdot 5.} + (1. - bx1) \cdot 
 e^{-bk2 \cdot 5.}) }
\f]

Sauerstoff-Kohlenstoffverhältnis beim C-Abbau bestimmt sich dann aus dem
Gesamt-Sauerstoffäquivalent zum Gesamtgehalt an organischem Kohlenstoff

\f[
  O_{BSB} = \frac{BL}{BTOC}
\f]

Der leichtabbaubare Anteil am Sauerstoffäquivalent wird mittels des Hilfsfaktors 
bx1 ermittelt

\f[
 BL_{1} = BL \cdot bx1
\f]

Der verblieben Rest ist dann schwerabbaubar:

\f[
 BL_{2} = BL \cdot (1.-bx1)
\f]

Ganz am Schluss werden Werte, die unter Null berechnet wurden auf 0.000001 geklippt.

mit:

| Formelzeichen | Name QSim | Beschreibung | Dimension |
| ------------- | --------- | ------------ | --------- |
| \f$ C-BSB5 \f$ | \ref obsb | Kohlenstoffbürtiger biologischer Sauerstoffbedarf in 5 Tagen | mgO2/l |
| \f$ CSB \f$    | \ref ocsb | Kohlenstoffbürtiger chemischer Sauerstoffbedarf | mgO2/l |
| \f$ C_{HNF} \f$ | *CHNF*   | C-Masse der heterotrophen Nanoflagelaten | mgC/l |
| \f$ V_{HNF} \f$ | \ref bvhnf | Biovolumen der HNF  | µm3 ?? |
| \f$ v \f$      | vcbe      | Verhältnis \ref obsb/ \ref ocsb | - |
| \f$ CP_i \f$   | CP(i=\ref cp1 + \ref cp2, | \ref lnk_bilaCP in der i-ten Stoffgruppe | mgC/l |
| \f$ CD_i \f$   | CD(i=\ref cd1 + \ref cd2 | \ref lnk_bilaCD in der i-ten Stoffgruppe | mgC/l|
| \f$ i \f$      | i      | Index Abbaubarkeit  - i=1: leicht abbaubar; i=2: schwer abbaubar| - |
| \f$ CM \f$     | *CM*   | planktonic_variable 41| \ref lnk_bilaCM | mgC/l /td></tr>
| \f$ HBAC \f$   | *BAC*  | planktonic_variable 42| \ref lnk_bilaBAC | mgC/l |
| \f$ C_{ref} \f$ | Cref  | \ref lnk_bilaCref | mgC/l |
| \f$ TOC \f$    | BTOC | Gesamtgehalt an organischem Kohlenstoff (total organic carbon)  | mgC/l |
| \f$ O_{BSB} \f$ | \ref o2bsb | Sauerstoff-Kohlenstoffverhältnis beim C-Abbau | mgO2/mgC |
| \f$ BL_{1} \f$ | \ref bl01 | leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent | mg O2 / l |
| \f$ BL_{2} \f$ | \ref bl02 | schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent | mg O2 / l |

\n\n

Textquelle: kohlenstoff-umsetzung.md; Code: orgC.f90; orgc_huelle.f95; 
zurück \ref lnk_orgC oder \ref lnk_randbedingungen
