Phytoplankton - Umsetzung {#lnk_phyto_umsetzung}
========================= 

# Herkunft  
*tbd*

# Schnittstellenbeschreibung {#lnk_phyto_schnittstellen}
*tbd*

# IT-Realisierung {#lnk_phyto_it_real}
*tbd*

## Vorbelegung der Algen ### {#phy_vorbelegung}

In QSim müssen die Biomassen der Algenklassen, ihre Chlorophyll-a:Kohlenstoff- 
sowie ihre zellinternen Nährstoff:Biomasse-Verhältnisse zu Beginn einer 
Simulation für die Durchführung einer Simulation vorbelegt werden. Dies erfolgt 
in der Subroutine algae_start() (s. zuflussrand.f90) über die 
bereitgestellten Gewässergüterandwerte und einen Teil der gesetzten 
Eingabeparameter.

Die Algenbiomasse zu Beginn einer QSim-Simulation wird in QSim wie folgt 
berechnet:
\f{equation}{
 A_i = \frac{1}{\CBio{i}} \cdot \text{Chl-a} \cdot \aChl{i} \cdot 
 \CChld{i}(T),\quad \quad \text{[\f$\mgL\f$]}
\f}

mit dem Kohlenstoff:Biomasse-Verhältnis \f$\CBio{i} = 0.48\f$ \f$\gXg{C}\f$ für 
alle Algenklassen, der Gesamt-Chlorophyll-a-Konzentration Chl-a 
(in \f$\ugChlL\f$), dem Anteil am Gesamt-Chlorophyll-a der Algenklasse 
\f$\aChl{i}\f$ (dimensionlos; Güterandwert), und dem 
Kohlenstoff:Chlorophyll-a-Verhältnis dunkeladaptierter Algen der 
Klasse *i* (in \f$\gCmgChl\f$) bei Temperatur *T* (in \f$\degC\f$):

\f{equation}{
 \CChld{i}(T) = \CChld{i}(T_{ref}) \cdot e^{\aCChl{i} \cdot \left( T - T_{ref} 
 \right)}, \quad \quad \text{[\f$\gCmgChl\f$]} 
 \label{Eq:CChla_d}
\f}

mit \f$\CChld{i}(T_{ref})\f$ bei \f$T_{ref} = 20\degC\f$ als Eingabeparameter 
und dem Koeffizienten \f$\aCChl{i}\f$ (in \f$\degCinv\f$), welcher in QSim in
zuflussrand.f90 in Abhängigkeit von \f$\CChld{i}(\Tref)\f$ gesetzt wird. Die 
Standardwerte für \f$\aCChl{i}\f$ sind wie folgt:

- \f$\aCChl{ki} = -0.059\f$ für Kieselalgen
- \f$\aCChl{gr} = -0.032\f$ für Grünalgen
- \f$\aCChl{bl} = -0.062\f$ für Blaualgen

\note Werte sollten nicht in zuflussrand.f90 gesetzt werden und Werte 
passen nicht zu Daten von Fanesi, 2015 \cite Fanesi2015.

Zu Beginn der Simulation wird somit auch angenommen, dass alle Algen 
dunkeladaptiert sind (s. Glg. \f$\eqref{Eq:CChla_d}\f$). Die zellinternen
Nährstoff:Biomasse-Verhältnisse werden auf die vom Anwender als Eingabeparameter 
gesetzten maximalen Quotas \f$\Qmax{X}\f$ (in \f$\mgXL{$X$}\f$; mit *X* = N, P 
oder Si) gesetzt, d.h. die Nährstoffspeicher der Algen sind vollständig gefüllt.


## Aufteilung Algen-Variablen im Zufluss {#lnk_algenaufteilung_zufluss}

jetzt mittels algae_start() in zuflussrand.f90

folgendes Code-Fragment aus Version 13.30

Ausgangspunkt sind die Angaben: \n
     !planktonic_variable(11+nk) = rabe(zaehl)%wert_jetzt(13)  !  CHLA   Chorophyl-a  \n
     !planktonic_variable(19+nk) = rabe(zaehl)%wert_jetzt(14)  !  VKIGR  Anteil Kieselalgen (falls unbekannt 0) \n
     !planktonic_variable(20+nk) = rabe(zaehl)%wert_jetzt(15)  !  ANTBL  Anteil Blaualgen (falls unbekannt 0) \n

	 
     chlaz = chla in allen Schichten, d.h. Tiefenverteilung konstant -> plankt_vari_vert(l+(11-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) \n
     akiz, agrz, ablz ebenso


     ! agbcm:    planktonic_variable(25+nk) = (agchl/(1000.*Cagr))*fTChlC ! agbcm \n
     ! akbcm:    planktonic_variable(24+nk) = (akchl/(1000.*Caki))*fTChlC ! akbcm \n
     ! abbcm:    planktonic_variable(26+nk) = (abchl/(1000.*Cabl))*fTChlC ! abbcm \n

	 
     aki = (chla*VKIGR)/(1000*akbcm*Caki) ->
     planktonic_variable( 8+nk) =  planktonic_variable(11+nk)*planktonic_variable(19+nk)/(1000.*planktonic_variable(24+nk)*Caki) \n
	 agr = (chla*(1-VKIGR-ANTBL))/(1000*agbcm*Cagr)) ->
	 planktonic_variable( 9+nk)=planktonic_variable(11+nk)*(1.-planktonic_variable(19+nk)-planktonic_variable(20+nk))/(1000.*planktonic_variable(25+nk)*Cagr) \n
	 abl = (chla*ANTBL)/(1000*abbcm*Cabl) ->
	 planktonic_variable(10+nk)=planktonic_variable(11+nk)*planktonic_variable(20+nk)/(1000.*planktonic_variable(26+nk)*Cabl)

  	 chlaki = chla * vkigr -> planktonic_variable(12+nk) = planktonic_variable(11+nk)*planktonic_variable(19+nk) \n
	 chlagr = chla * (1-vkigr-antbl) -> planktonic_variable(13+nk) = planktonic_variable(11+nk)*(1.-planktonic_variable(19+nk)-planktonic_variable(20+nk)) \n
	 chlabl = chla * antbl -> planktonic_variable(14+nk) = planktonic_variable(11+nk)*planktonic_variable(20+nk)

     planktonic_variable(21+nk)= 0.01 ! svhemk ### unklar ### \n
     planktonic_variable(22+nk)= 0.01 ! svhemg ### unklar ### \n
     planktonic_variable(23+nk)= 0.01 ! svhemb ### unklar ### \n
     planktonic_variable(27+nk)= 0.01 ! akiiv ### unklar ### \n
     planktonic_variable(28+nk)= 0.01 ! agriv ### unklar ### \n
     planktonic_variable(29+nk)= 0.01 ! abliv ### unklar ### \n


Textquelle: phyto-umsetzung.md; Code: albenth.f90, algaesbl.f90,
algaesgr.f90, algae_huelle.f95 ; zurück \ref lnk_phytoplankton oder 
 \ref lnk_randbedingungen oder lnk_anfangsbedingungen
