Wassertemperatur - Umsetzung {#lnk_wtemp_umsetzung}
===============================

## Herkunft ##

temperw()\n

EIN PROGRAMM ZUR BERECHNUNG DER WASSERTEMPERATUR\n

AUTOR : VOLKER KIRCHESCH\n

entnommen aus Version qsim13.301_28mae18\n 

Numerisch wird die Temperatur behandelt wie eine Konzentrationen.
Die im folgenden genannten lokalen Wärmeeinträge und -austräge (Wärmeflüsse) 
sind quasi der *Stoffumsatz* der "Temperatur-Konzentration":\n


## Schnittstellenbeschreibung ##
call temperw()\n
( \ref ro, \ref templ, \ref tempw, \ref schwi, \ref wge, \ref tiefe, \ref tflie, \ref flag
 , \ref elen, \ref ior, \ref anze, *etemp*, *ewaerm*, *typ*, \ref qeinl, \ref vabfl   &\n
 , \ref jiein, \ref cloud, \ref typw, \ref iwied, \ref uhrz, \ref ilbuhn, *nwaerm*
 , \ref fkm, \ref nkzs, \ref tempwz, \ref dh2d, \ref iorla, \ref iorle, \ref ieinls, \ref flae &\n
 , *qeinll*, *etempl*, \ref mstr, *idwe*, \ref ilang, \ref dtemp, \ref fluxt1
 , \ref extk, \ref itags, \ref monats, \ref tsed, \ref wlage, \ref hws, *irhkw*      &\n
 , *htempw*, *htempz*, \ref wuebks, \ref spewkss, \ref psrefss, \ref extks
 , *ifehl*, *ifhstr*, \ref azstrs, \ref iwsim, *iform_VerdR*,                  &\n
 &, \ref kontroll, *jjj*, \ref iglob, *meinrang*)  \n
 \n
 <!-- check, ob jjj, iglob und meinrang noch dabei sind (sind nicht in Volkers Doku -->
 <!-- wenn eine Var noch nicht in Var-Liste definiert, dann in Volkers Update-Doku schauen -->

 
## IT-Realisierung ##
Die QSim Subroutine temperw wird von der Hüllroutine temperw_huelle() aufgerufen. 
Zum Hüllroutinen-Konzept siehe: \ref lnk_huellen). 

Parallelisierung ist erfolgt; die Subroutine wird von allen Prozessoren für ihre jeweiligen Knoten aufgerufen.

### Berechnungsablauf ###
In jedem Zeitschritt müssen für jede Wetterstation die folgenden Subroutinen abgearbeitet werden:\n
wettles_module()  ersetzt QSim-Subroutine wettles(), interpoliert die Wetterdaten für den aktuellen Zeitpunkt.\n
temperl_module()  ersetzt QSim-Subroutine Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.\n
strahlg_huelle()  berechnet aus der von <a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a>
eingelesenen Globalstrahlung den Strahlungsanteil, der im Gewässer ankommt.
durch Aufruf der QSim-Subroutine strahlg() unter Benutzung der QSim-Subroutinen sasu() und tage()\n

<!-- kurzer Abschnitt aus "alter Doku" (war in temperw_huelle.f95) -->
Wetterdaten für Waermebilanz in diesem Zeitschritt wurden in randbedingungen_setzen() ermittelt
call wettles_wetter()  ! ersetzt wettles(), interpoliert Wetterdaten für den aktuellen Zeitpunkt
call temperl_wetter()  ! ersetzt Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.
call strahlg_wetter()  ! berechnet aus der Globalstrahlung den Strahlungsanteil, der im Gewässer ankommt.
<!-- Abschnittsende --> 

### Aufruf ###
Die Hüllroutine temperw_huelle(), wird von stoffumsatz() für alle Knoten 
(die von dem jeweiligen prozessor parallel bearbeitet werden) aufgerufen.
Die Wassertemperatur und die Sedimenttemperatur werden dann von der QSim-Subroutine temperw() berechnet.
Dieser werden dafür die folgenden Variablen übergeben:
\n

(Zum Hüllroutinen-Konzept siehe: \ref lnk_huellen )

## Rueckgabewerte/Resultate: ##
Der Ergebnisrückgabe dienen die folgenden beiden Variablen (-Felder)

| TEMPW 	|  tiefengemittelte Temperatur - im aktuellen Zeitschritt bis zu Knoten anze+1	| 
| tempwz 	|  Tiefenverteilung der Temperatur - 						| 

 \n\n
! Wetterdaten für Waermebilanz in diesem Zeitschritt wurden in randbedingungen_setzen() ermittelt
! call wettles_wetter()  ! ersetzt wettles(), interpoliert Wetterdaten für den aktuellen Zeitpunkt
! call temperl_wetter()  ! ersetzt Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.
! call strahlg_wetter()  ! berechnet aus der Globalstrahlung den Strahlungsanteil, der im Gewässer ankommt.
\n\n

Die Hüllroutine temperw_huelle(i) wird in "\ref lnk_wtemp " beschrieben; 
dort findet sich auch eine ausfühliche schnittstellenbeschreibung zu temperw().

\n\n
Zum Hüllroutinen-Konzept siehe \ref lnk_huellen
\n\n
Parallelisierung ist erfolgt; die Subroutine wird von allen Prozessoren für ihre jeweiligen Knoten aufgerufen.


&nbsp 
Textquelle: wtemp-umsetzung.md ; Codesources: TEMPERW.f90, temperw_huelle() in temperw_huelle.f95 ;  
zurück: \ref lnk_wtemp