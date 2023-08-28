Wassertemperatur - Umsetzung {#lnk_wtemp_umsetzung}
===============================

Numerisch wird die Temperatur behandelt wie eine Konzentrationen.
Die im folgenden genannten lokalen Wärmeeinträge und -austräge (Wärmeflüsse) 
sind quasi der *Stoffumsatz* der "Temperatur-Konzentration".


### Berechnungsablauf ###
In jedem Zeitschritt müssen für jede Wetterstation die folgenden Subroutinen abgearbeitet werden:
    * wettles_module()  ersetzt QSim-Subroutine wettles(), interpoliert die Wetterdaten für den aktuellen Zeitpunkt.\n
    * temperl_module()  ersetzt QSim-Subroutine Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.\n
    * strahlg_huelle()  berechnet aus der von <a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a> eingelesenen Globalstrahlung den Strahlungsanteil, der im Gewässer ankommt. durch Aufruf der QSim-Subroutine strahlg() unter Benutzung der QSim-Subroutinen sasu() und tage()\n



Textquelle: wtemp-umsetzung.md ; Codesources: TEMPERW.f90, temperw_huelle() in temperw_huelle.f95 ;  
zurück: \ref lnk_wtemp