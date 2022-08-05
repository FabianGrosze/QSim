Randbedingungen {#lnk_randbedingungen} 
===============

# Wetter-Randbedingungen

\subpage lnk_wetter_rb

# Güte-Randbedingungen
Dies sind die Konzentrationen im zufließend Wasser an den Rändern, sie
werden aus der Datei <a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a> 
gelesen

und entsprechend den Randnummern zu den am Rand liegenden Knoten/Elementen zugeordnet.

Knoten mit der Randnummer 0 liegen im inneren des Gebiets.

Datentechnische Detail in: ereigg_Randbedingungen_lesen()


Nur die üblicherweise aus Messungen verfügbaren Variablen sind angebbar, ( randbedingungen_setzen() )

Bei denjenigen \ref lnk_tiefengemittelte_plankt_var , die dadurch keine Werte erhalten,
werden diese plausibel ergänzt; siehe: \subpage lnk_randbedingungen_ergaenzen.

Im Programmablauf geschieht die Randbedingungs-Zuordnung vor Stoffumsatz und Stofftransport,
damit der Transport die am Rand gesetzten Konzentrationen (besonders an den Einströmstellen)
ins Gebiet einträgt.

Aus der <a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a>
Datei werden die folgenden Variablen (werts) gelesen und den in nachfolgender Tabelle
genannten \ref lnk_tiefengemittelte_plankt_var zugeordnet.


Für den aktuellen Zeitschrit wird der Wert zwischen gültigen Randwerten interpoliert.\n

Gültig sind nur positive Werte. Negative Werte werden als ungültig betrachtet.\n
Wird an einem Rand kein gültiger Randwert gefunden, bricht die Simulation ab, ausser:\n
bei \ref lnk_hnf und \ref lnk_coliform.
Diese werden bei ausschließlich ungültigen Randwerten als an dem Rand nicht 
vorhanden (Null) betrachtet.\n

| werts(Nr.) | plankt-var. Nr. | QSim-Variablen-Name | Beschreibung | Einheit | wird gesetzt in Subrout. | 
| --- | --- | --- | --- | --- | --- | 
|  1 |  - | QEINL   | Einleiter-Abfluss / in QSim3D unberücksichtigt | m³/s |  |
|  2 | 46 | \ref vbsb   | C-BSB5                            | mg/l  |   |
|  3 | 47 | \ref vcsb   | C-CSB                             | mg/l  |   |
|  4 |  3 | \ref vnh4   | NH4-N Gehalt                      | mg/l  | ncyc() |
|  5 |  4 | \ref vno2   | Nitrit-Stickstoffgehalt           | mg/l  | ncyc() |
|  6 |  5 | \ref vno3   | Nitrat-Stickstoffgehalt           | mg/l  | ncyc() |
|  7 | 67 | \ref gesn   | Gesamt-Stickstoff                 | mg/l  | ncyc() |
|  8 |  6 | \ref vx0    | suspendierte Nitrosomonasbiomasse | mg/l  | ncyc() |
|  9 |  7 | \ref vx02   | suspendierte Nitrobacterbiomasse  | mg/l  | ncyc() |
| 10 |  9 | \ref gelp   | gelöster Phosphorgehalt           | mg/l  | po4s() |
| 11 | 68 | \ref gesp   | Gesamt-Phosphor                   | mg/l  | po4s() |
| 12 |  8 | \ref si     | gelöster Siliziumgehalt           | mg/l  | silikat() |
| 13 | 12 | \ref chla   | Chlorophyll-a-Gehalt              | µg/l  |        |
| 14 | 19 | \ref vkigr  | Anteil der Kieselalgen am Gesamt-Chlorophyll-a | 0-1 | |
| 15 | 20 | \ref antbl  | Anteil der Blaualgen am Gesamt-Chlorophyll-a   | 0-1 | |
| 16 | 50 | \ref zooind | Rotatoriendichte                  | Ind/l      |   |
| 17 | 66 | \ref vph    | pH-Wert                           | -          |   |
| 18 | 62 | \ref mw     | m-Wert                            | mmol/l     |   |
| 19 | 64 | \ref ca     | Calciumkonzentration              | mg/l       |   |
| 20 | 65 | \ref lf     | Leitfähigkeit                     | µS/cm      |   |
| 21 | 53 | \ref ss     | Schwebstoffgehalt                 | mg/l       |   |
| 22 |  1 | \ref tempw  | Wassertemperatur                  | °C         |   |
| 23 |  2 | \ref vo2    | Sauerstoffgehalt                  | mg/l       |   |
| 24 | 48 | \ref chnf   | Heterotrophe Nanoflagelaten       | Ind/ml     |   |
| 25 | 49 | \ref bvhnf  | Biovolumen der HNF                | µm3        |   |
| 26 | 61 | \ref coli   | Fäkalcoliforme Bakterien          | Ind/100 ml |   |
| 27 |  - | EWAERM | Menge der eingeleiteten Wärme (Kraftwerk) / in QSim3D unberücksichtigt | Mcal/s|  |
| 28 | 71 | TRACER | Tracer-Menge (nur im Tracer-Prozessbaustein berücksichtigt) | g | - |

<code>\verbatim aus funkstar.f90:
if(ipp==1)abfls(mstr,RBNR) = ywert
if(ipp==2)vbsbs(mstr,RBNR) = ywert
if(ipp==3)vcsbs(mstr,RBNR) = ywert
if(ipp==4)vnh4s(mstr,RBNR) = ywert
if(ipp==5)vno2s(mstr,RBNR) = ywert
if(ipp==6)vno3s(mstr,RBNR) = ywert
if(ipp==7)gesNs(mstr,RBNR) = ywert
if(ipp==8)vx0s(mstr,RBNR) = ywert
if(ipp==9)vx02s(mstr,RBNR) = ywert
if(ipp==10)gelps(mstr,RBNR) = ywert
if(ipp==11)gesPs(mstr,RBNR) = ywert
if(ipp==12)sis(mstr,RBNR) = ywert
if(ipp==13)chlas(mstr,RBNR) = ywert
if(ipp==14)vkigrs(mstr,RBNR) = ywert
if(ipp==15)antbls(mstr,RBNR) = ywert
if(ipp==16)zooins(mstr,RBNR) = ywert
if(ipp==17)vphs(mstr,RBNR) = ywert
if(ipp==18)mws(mstr,RBNR) = ywert
if(ipp==19)cas(mstr,RBNR) = ywert
if(ipp==20)lfs(mstr,RBNR) = ywert
if(ipp==21)ssalgs(mstr,RBNR) = ywert
if(ipp==22)tempws(mstr,RBNR) = ywert
if(ipp==23)vo2s(mstr,RBNR) = ywert
if(ipp==24)CHNFs(mstr,RBNR) = ywert
if(ipp==25)BVHNFs(mstr,RBNR) = ywert
if(ipp==26)colis(mstr,RBNR) = ywert
if(ipp==27)waers(mstr,RBNR) = ywert
28 und 29 Tracer und kons. Substanz in tempws
if(ipp==30)gsPbs(mstr,RBNR) = ywert
if(ipp==31)glPbs(mstr,RBNR) = ywert
if(ipp==32)gsCads(mstr,RBNR) = ywert
if(ipp==33)glCads(mstr,RBNR) = ywert
if(ipp==34)gsCrs(mstr,RBNR) = ywert
if(ipp==35)glCrs(mstr,RBNR) = ywert
if(ipp==36)gsFes(mstr,RBNR) = ywert
if(ipp==37)glFes(mstr,RBNR) = ywert
if(ipp==38)gsCus(mstr,RBNR) = ywert
if(ipp==39)glCus(mstr,RBNR) = ywert
if(ipp==40)gsMns(mstr,RBNR) = ywert
if(ipp==41)glMns(mstr,RBNR) = ywert
if(ipp==42)gsNis(mstr,RBNR) = ywert
if(ipp==43)glNis(mstr,RBNR) = ywert
if(ipp==44)gsHgs(mstr,RBNR) = ywert
if(ipp==45)glHgs(mstr,RBNR) = ywert
if(ipp==46)gsUs(mstr,RBNR) = ywert
if(ipp==47)glUs(mstr,RBNR) = ywert
if(ipp==48)gsZns(mstr,RBNR) = ywert
if(ipp==49)glZns(mstr,RBNR) = ywert
if(ipp==50)gsAss(mstr,RBNR) = ywert
if(ipp==51)glAss(mstr,RBNR) = ywert
 \endverbatim</code>\n\n
type(rb) , allocatable , dimension (:) :: rabe

Zufluss-Randbedingungen aus <a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a>

werden von der subroutine ereigg_Randbedingungen_lesen() eingelesen.

QSim3D ließt in die Struktur \ref rb ein.

Textquelle: randbedingungen.md ; Codesource: randbedingungen.f95 ;  
zurück: \ref lnk_datenstruktur 

<!-- #mf folgende Zeilen noch einsortieren. Evtl. wieder zurück zu randbedingungen.f95 ? -->
!> Dient dem Anbringen der \ref lnk_randbedingungen \n
in Datei randbedingungen.f95


# Randbedingungen ergänzen {#lnk_randbedingungen_ergaenzen} 
<!-- dieser Teil war eine eigene Seite... --> 

Wird nicht nur am Zuflussrand, sondern auch bei der initialisierung mit einer 
Randbedingung aufgerufen, so dass alle Transportkonzentrationen belegt werden

Jetzt:

algae_start() , orgc_start() , naehr_start() , pwert() \n
in ../metabol/zuflussrand.f90 \n


Ehedem: 

Randbedingungen werden nur für die 27 in ereigg_Randbedingungen_lesen() 
beschriebenen Konzentrationen vorgegeben. Alle übrigen werden auf der Basis 
plausibler Abschätzungen erschlossen.

Diese Vorgehensweise hat ihren Ursprung in der Verfügbarkeit von Messdaten.\n
Ergänzungen finden für die folgenden QSim-Bausteine statt:

 1. \subpage lnk_algenaufteilung_zufluss
 
 2. \subpage lnk_orgc_aufteilung  
 
 3. \subpage lnk_stickstoff_aufteilung  
 
 4. *ph_aufteilung*
 
 5. \subpage lnk_po4s_aufteilung 
 
 6. \subpage lnk_si_aufteilung

ereigg_Randbedingungen_lesen() 

RB_werte_aktualisieren()

wert_randbedingung()


qsim.f90:
....Berechnung von nL0 und pL0 (N und P Gehalt der Abwasserbuertigen   \n
   org Substanz)
\n\n
läuft nur auf prozessor 0
noch fehlende ergänzungen für: <code>\verbatim
                  planktonic_variable(51+nk)= 0.0 ! abrzo1 ### wohl unbenutzt ###
schweb.f90:      SSALG(ior) = SSt+agr(ior)+aki(ior)+abl(ior)+(ZOOind(ior)*GROT/1000.)
                  planktonic_variable(52+nk)= 0.0 ! ssalg GESAMTSCHWEBSTOFFE provisorisch zusammenaddiert in \ref lnk_randbedingungen_ergaenzen
                  planktonic_variable(54+nk)= 0.0 ! fssgr schweb.f90
                  planktonic_variable(55+nk)= 0.0 ! fbsgr orgc.f90
                  planktonic_variable(56+nk)= 0.0 ! frfgr orgc.f90
                  planktonic_variable(57+nk)= 0.0 ! nl0 Verhältnis von Stickstoff zu Kohlenstoff in organischem Material | qsim.f90: nl0s(mstr,mRB) = 0.04
                  planktonic_variable(58+nk)= 0.0 ! pl0 Verhältnis von Phosphor zu Kohlenstoff in organischem Material | qsim.f90: pl0s(mstr,mRB) = 0.005 wenn nicht berechenbar
                  planktonic_variable(59+nk)= 0.0 ! stind, Zeitsumme ph(), Funktion unklar ###
                  planktonic_variable(60+nk)= 0.0 ! dlarvn, dreissen.f90
                  planktonic_variable(69+nk)= 0.0 ! SKmor, Silizium in schwebenden, abgestorbenen Kieselalgen, algaeski<->silikat
                  planktonic_variable(70+nk)= 0.0 ! DOSCF, coliform()
                  planktonic_variable(72+nk)= 0.0 ! Salz (neu QSim-3D)
 \endverbatim</code>
\n\n zurück: \ref lnk_randbedingungen ; Quelle: randbedingungen.f95


## Transportkonzentrationen für welche die R.B. nicht vorgegeben sondern erschlossen werden

| Nr. | QSim-Name | Wie |  Subroutine  | 
| --- | --------- | --- | ------------ | 
| 13 | chlaki |         |  |
| 14 | chlagr |         |  |
| 15 | chlabl |         |  |
| 16 | aki    |         |  |
| 17 | agr    |         |  |
| 18 | abl    |         |  |
| 21 | svhemk |         |  |
| 22 | svhemg |         |  |
| 23 | svhemb |         |  |
| 24 | akbcm  |         |  |
| 25 | agbcm  |         |  |
| 26 | abbcm  |         |  |
| 27 | akiiv  |         |  |
| 28 | agriv  |         |  |
| 29 | abliv  |         |  |
| 30 | Q_NK   | =Qmx_NK | ncyc()    |
| 31 | Q_PK   | =Qmx_PK | po4s()    |
| 32 | Q_SK   | =Qmx_SK | silikat() |
| 33 | Q_NG   | =Qmx_NG | ncyc()    |
| 34 | Q_PG   | =Qmx_PG | po4s()    |
| 35 | Q_NB   | =Qmx_NB | ncyc()    |
| 36 | Q_PB   | =Qmx_PB | po4s()    |
| 37 | CD(1   |         |  |
| 38 | CD(2   |         |  |
| 39 | CP(1   |         |  |
| 40 | CP(2   |         |  |
| 41 | CM     |         |  |
| 42 | BAC    |         |  |
| 43 | O2BSB  |         |  |
| 44 | BL01   |         |  |
| 45 | BL02   |         |  |
| 46 | vbsb   |         |  |
| 47 | vcsb   |         |  |
| 51 | abrzo1 |         |  |
| 52 | ssalg  |         |  |
| 54 | fssgr  |         |  |
| 55 | fbsgr  |         |  |
| 56 | frfgr  |         |  |
| 57 | nl0    | nl0(ior) = orgNn/(ocsbt/2.8) | orgc |
| 58 | pl0    | pl0s(mstr,mRB) = orgP/(ocsbs(mstr,mRB)/2.8) pl0(ior) = orgPn/(ocsbt/2.8)| qsim + orgc |
| 59 | stind  |         |  |
| 60 | dlarvn |         |  |
| 63 | pw     |         |  |
| 69 | SKmor  | ?? unklar ?? | nicht in silikat(), qsim() ??? |
| 70 | DOSCF  |         |  |


Textquelle: randbedingungen.md ; Codesource: randbedingungen.f95 ;  
zurück: \ref lnk_modelldetails oder ...
<!-- \ref lnk_modellerstellung -->