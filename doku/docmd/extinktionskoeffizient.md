Extinktionskoeffizienten {#lnk_extnct_rb}  
========================

Die Absorptionsspektren sigma(Lambda) für Wasser, Kiesel-,Gruen- und Blaualgen, 
Humin und susp. Schwebstoff und das Spektrum des eingestrahlten Sonnenlichts
werden in der Datei 
<a href="./exp/e_extnct.dat" target="_blank">e_extnct.dat</a> angegeben;
von der Subroutine extnct_lesen() aus dem Modellverzeichnis gelesen
und mithilfe des eindimensionalen Datenfeld: 
rb_extnct_p(n + (i - 1) * anz_extnct_koeff) gespeichert 
(i-Knotennummer, anz_extnct_koeff = 8)
und auf alle parallelen Prozesse kopiert (MPI_Bcast).

## Licht-Extinktions-Koeffizienten 
Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::rb_extnct_p \n
Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n

| Nr. | Name              | Beschreibung                           | Dimension |
| --- | ----------------- | -------------------------------------- | --- |
| 1   | \anchor eta eta   | Wellenlänge (in nm in Luft)            | |
| 2   | \anchor aw aw     | Extinktions-Spektrum Wasser            | ?? |
| 3   | \anchor ack ack   | Extinktions-Spektrum Kieselalgen       | |
| 4   | \anchor acg acg   | Extinktions-Spektrum Gruenalgen        | |
| 5   | \anchor acb acb   | Extinktions-Spektrum Blaualgen         | |
| 6   | \anchor ah ah     | Extinktions-Spektrum Humin-Stoffe      | |
| 7   | \anchor as as     | Extinktions-Spektrum susp. Schwebstoff | |
| 8   | \anchor al al     | Sonnenlicht-Spektrum                   | |
| 9   | \anchor extk_lamda extk_lamda   | Rückgabeparameter Gesamtextinktion | |

Im Folgenden ist die aktuelle Vorgabe graphisch dargestellt:

\image html absorbtionsspectrum.png "Absorpions-Spektren" 
siehe: \ref lnk_phy_licht


Textquelle: extinktionskoeffizient.md ; Codesources: module_modell.f95 ;  
zurück: \ref lnk_randbedingungen
