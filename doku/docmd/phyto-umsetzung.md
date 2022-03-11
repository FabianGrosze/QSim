Phytoplankton - Umsetzung {#lnk_phyto_umsetzung}
========================= 

## Herkunft ##

## Schnittstellenbeschreibung ##


## IT-Realisierung ##

### Vorbelegung der Algen ### {#phy_vorbelegung}

<!-- debugging

In QSim müssen die Biomassen der Algenklassen, ihre Chlorophyll-a:Kohlenstoff- sowie ihre zellinternen Nährstoff:Biomasse-Verhältnisse zu Beginn einer Simulation für die Durchführung einer Simulation vorbelegt werden. Dies erfolgt in der Subroutine \mbtt{algae\_start} (s. \mbtt{zuflussrand.f90}) über die bereitgestellten Gewässergüterandwerte und einen Teil der gesetzten Eingabeparameter.

Die Algenbiomasse zu Beginn einer QSim-Simulation wird in QSim wie folgt berechnet:
\f[
 A_i = \frac{1}{\CBio{i}} \cdot \text{Chl-a} \cdot \aChl{i} \cdot 
 \CChld{i}(T),\quad \quad \text{[\mgL]}
\f]

mit dem Kohlenstoff:Biomasse-Verhältnis $\CBio{i} = 0.48$\,\gXg{C} für alle 
Algenklassen, der Gesamt-Chlorophyll-a-Konzentration Chl-a (in \ugChlL), dem 
Anteil am Gesamt-Chlorophyll-a der Algenklasse \aChl{i} (dimensionlos; Güterandwert), 
und dem Kohlenstoff:Chlorophyll-a-Verhältnis dunkeladaptierter Algen der Klasse $i$ 
(in \gCmgChl) bei Temperatur $T$ (in \degC):

\f[
 \CChld{i}(T) = \CChld{i}(T_{ref}) \cdot e^{\aCChl{i}\cdot\left( T - T_{ref} \right)}, \quad \quad \text{[\gCmgChl]} \label{Eq:CChla_d}
\f]

mit $\CChld{i}(T_{ref})$ bei $T_{ref} = 20\degC$ als Eingabeparameter und dem Koeffizienten \aCChl{i} (in \degCinv), welcher in QSim in \mbtt{zuflussrand.f90} in Abhängigkeit von $\CChld{i}(\Tref)$ gesetzt wird. Die Standardwerte für \aCChl{i} sind wie folgt:

* \aCChl{ki}$ = -0.059$ für Kieselalgen
* \aCChl{gr}$ = -0.032$ für Grünalgen
* \aCChl{bl}$ = -0.062$ für Blaualgen

\textcolor{red}{Hinweis: Werte sollten nicht in \mbtt{zuflussrand.f90} gesetzt werden und Werte passen nicht zu Daten von \citet{Fanesi2015}.}\par

Zu Beginn der Simulation wird somit auch angenommen, dass alle Algen dunkeladaptiert sind (s. Glg.~\eqref{Eq:CChla_d}). Die zellinternen Nährstoff:Biomasse-Verhältnisse werden auf die vom Anwender als Eingabeparameter gesetzten maximalen Quotas \Qmax{X} (in \mgXL{$X$}; mit $X$ = N, P oder Si) gesetzt, d.h. die Nährstoffspeicher der Algen sind vollständig gefüllt.

end debugging -->

\n\n

Textquelle: phyto-umsetzung.md; Code: albenth.f90, algaesbl.f90,
algaesgr.f90; zurück \ref lnk_phytoplankton
