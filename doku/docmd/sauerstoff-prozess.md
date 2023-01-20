Sauerstoff - Prozesse {#lnk_sauerstoff_prozesse}
=====================

# Teilprozesse 
Folgende Prozesse führen in QSim zu einer Veränderung im 
Sauerstoffgehalt:

- die Sauerstoffproduktion durch die 
  [Photosyntheseleistungen von Kiesel-, Grün- und Blaualgen](\ref lnk_o2_prod_phyto) 
  und [Makrophyten](\ref lnk_o2_makrophyt) \n
  
- der Sauerstoffverbrauch durch die 
  [Respiration von Kiesel-, Grün- und Blaualgen](\ref lnk_o2_resp_phyto) und 
  [Makrophyten](\ref lnk_o2_makrophyt)
  
- der Sauerstoffverbrauch durch mikrobielle 
  [Oxidation organischer Kohlenstoffverbindungen](\ref lnk_o2_corg)
  
- der [Sauerstoffverbrauch durch die Nitrifikation](\ref lnk_o2_nitrif) in 
  der Wassersäule 
  
- der Sauerstoffverbrauch durch die 
  [Respiration des Zooplanktonplanktons (Rotatorien)](\ref lnk_o2_resp_zoop) und 
  [der benthischen Filtrierer (Dreissena)](\ref lnk_o2_dreissena)  
  
- der [Sauerstoffverbrauch im Sediment](\ref lnk_o2_sediment)


# Bilanzgleichung 

Die vollständige Bilanzgleichung für die Änderung des Sauerstoffgehaltes 
\f$O_2\f$ lautet:

\f{equation}{
 \begin{split}
	\frac{dO_2}{dt} = k_{2,OB} \cdot (O_{2,Saett} - O_2) - BSB - 
	\Delta O_{Nitri} + \sum_{j = 1}^{3}\Delta O_{A,j} + \\
	\Delta O_{mphyt} + \sum_{j = 1}^{3}\Delta O_{Ab,j} - 
	Delta O_{rot} - \Delta O_{drs} + \frac{JO_2}{h}
 \end{split}	
\f}

mit

\f{equation}{k_{2,OB} = \left(3 + \frac{40}{K_S} \right) \cdot \frac{\nu}{h^2} +
 \frac{0.5}{h} \f}

<!-- \Delta O_{Sed} bzw. JO_2 ist momentan ausgeschaltet -->

<!--       if(nkzs(ior).eq.1)goto 190 \n
      do 189 nkz = 1,nkzs(ior) \n
      vz1(nkz,ior) = -go2n(ior)-bsbt(ior)+dalgoz(nkz,ior)               &\n
     &-algaoz(nkz,ior)-vo2leb+po2p(ior)-po2r(ior)+abeowg(ior)           &\n
     &-abeorg(ior)+abeowk(ior)-abeork(ior)-ro2dr(ior)                   &\n
     &-rO2HNF(ior)                                                      \n
 \n\n -->

\f$ O_2 \f$:    Sauerstoffgehalt im Wasser [\f$ g O_2 m^-3 \f$] \n
\f$ k_{2,OB} \f$: Sauerstoffänderungsrate für den physikalischen 
                  Sauerstoffeintrag bzw. -austrag über die Gewässeroberfläche
				  [d^{-1}] \n
\f$ O_{2,Saett} \f$: Sauerstoffsättigungskonzentration [\f$ g m^-3 \f$] \n
\f$ BSB \f$:      Sauerstoffverbrauch durch die mikrobielle Oxidation 
                  organischer Kohlenstoffverbindungen [\f$ g m^{-3} d^{-1} \f$] \n
\f$ \Delta O_{Nitri} \f$: Sauerstoffverbrauch in der Wassersäule durch 
                  Nitrifikation  [\f$ g m^{-3} d^{-1} \f$] \n
\f$ \Delta O_{A,j} \f$:   Sauerstoffänderung durch die Algen der Klasse j 
                  [\f$ g m^{-3} d^{-1} \f$] \n
\f$ \Delta O_{Ab,j} \f$:   Sauerstoffänderung durch die benthischen Algen der 
                  Klasse j [\f$ g m^{-3} d^{-1} \f$] \n				  
\f$ \Delta O_{mphyt} \f$: Sauerstoffänderung durch die Makrophyten 
                  [\f$ g m^{-3} d^{-1} \f$] \n
\f$ \Delta O_{rot} \f$: Sauerstoffänderung durch die Respiration der Rotatorien  
                  [\f$ g m^{-3} d^{-1} \f$] \n
\f$ \Delta O_{drs} \f$: Sauerstoffänderung durch Respiration benthischer Filtrierer 
                  (*Dreissena*) [\f$ g m^{-3} d^{-1} \f$] \n
\f$ JO_2 \f$:     Sauerstofffluss in das Sediment [\f$ g m^{-2} d^{-1} \f$] \n
\f$ h \f$:        mittlere Wassertiefe [m] \n
\f$ K_S \f$:      Stricklerbeiwert [\f$ m^{1/3} \cdot s^{-1}\f$] \n
\f$ \nu \f$:      Fließgeschwindigkeit [\f$\ms\f$]


# Austausch über die Gewässeroberfläche {#lnk_o2_oberflaechenaustausch}
Gewässer können an der Gewässeroberfläche Sauerstoff an die Luft abgeben, oder 
aus ihr aufnehmen.

Der Massenstrom an Sauerstoff über die Gewässeroberfläche wird hier zunächst als
Änderungsrate einer tiefengemittelten Konzentration angeschrieben:
\f{equation}{
   {\frac{\Delta O_2}{\Delta t}}_{lueft} = (k_l + k_w) \cdot \Delta_{saett}  
   \cdot f_{temp} \cdot \frac{1}{H}
\f}
darin ist das Sauerstoffdefizit (Untersättigung):
\f{equation}{
   \Delta_{saett} = {O_2, Saett} - O_2
\f}

Die Sauerstoffsättigungskonzentration \f$ O_{2,Saett} \f$ ist temperaturabhängig 
und wird wie folgt abgeschätzt:

\f{equation}{
   O_{2,Saett} = 14,603 - 0,40215 \cdot T + 0,007687 \cdot T^2 - 
   0,0000693 \cdot T^3
\f}
<!-- #mf: Gleichung stimmt, siehe oxygen.f90, Z.112 f) -->

Für die Temperaturabhängigkeit des Belüftungsprozesses wird der folgende 
empirische Faktor angesetzt:\n
\f{equation}{
   f_{temp} = 1,024^{T - 20}
\f}


## Einmischung mittels Turbulenz infolge Sohlreibung 

Zur Berechnung der Belüftung des Gewässers über die Oberfläche wird standardgemäß 
die Formel nach Wolf(1972) verwendet: 

Die Sauerstoffänderungsrate *k_{2,OB}* für den physikalischen Sauerstoffeintrag 
bzw. -austrag über die Gewässeroberfläche wird über den Stricklerbeiwert *K_{St}*, 
die Fließgeschwindigkeit \f$\nu\f$ und die mittlere Wassertiefe *h* berechnet. Sie 
hat die Dimension einer Geschwindigkeit. Wolf (1972) gibt zur Berechnung die 
folgende empirische Formel an:
\f{equation}{
   k_{2,OB} = \left(3 + \frac{40}{K_{St}} \right) \cdot \frac{\nu}{h^2} + \frac{0,5}{h} 
\f}

<!-- Code: bbeiw = ((3.+40./rau(ior))*abs(vmitt(ior))/tiefe(ior)**2)+0.5/tiefe(ior) -->
  
mit 
\f$ k_{2,OB} \f$: Sauerstoffänderungsrate für den physikalischen 
                  Sauerstoffeintrag bzw. -austrag über die Gewässeroberfläche
				  [m s^{-1}] \n
\f$ K_S \f$: Stricklerbeiwert [\f$ m^{1/3} s^{-1} \f$] \n
\f$ \nu \f$: Fließgeschwindigkeit [\f$ m s^{-1} \f$] \n
\f$ h \f$: mittlere Wassertiefe [\f$ m \f$] \n


## Alternative 
Als Alternative kann die Belüftungsformel von MELCHING und FLORES (1999) 
angewählt werden 

     (iphy = 2)              \n  
       81 FN = 1./RAU(ior) \n
       G = 9.81 \n
       UST = ((FN*G**0.5)/tiefe(ior)**0.166667)*abs(VMITT(ior)) \n
       Slope = UST**2/(g*Tiefe(ior)) \n
       Breite = flae(ior)/tiefe(ior) \n
       bbeiw = (142.*(abs(vmitt(ior))*Slope)**0.333)/((tiefe(ior)**0.66)*(Breite**0.243))            

\f{equation}{
   k_l = ...
\f}
\f$ k_l \f$: Belüftungsgeschwindigkeit Wasserbewegung  [m/d]


........BELUEFTUNGSBEIWERT NACH HAJEK,NEUMANN,BISCHOFSBERGER IN    
 
      73 BBEIw = (3+40./RAU(ior))*(abs(VMITT(ior))**0.7+0.5*tiefe(ior)**0.7)/tiefe(ior)**1.7   \n                              


## Windeinfluss

Der Windeinfluss auf die Belüftung wird berücksichtigt, wenn er mittels 
(iphyw != 0) angewählt wurde: \n

<!-- #mf: was ist mit iphy ? (iphyw gilt nur für 3D, oder? -->

Die gemessene Windgeschwindigkeit in der Höhe der Messung wird in eine 
Windgeschwindigkeit 10 m über der Wasseroberfläche umgerechnet:
\f{equation}{
   W_{10} = W_{mess} / {\left( \frac{z_{mess}}{10.} \right) }^{0.17} 
\f}
\f$ W_10 \f$: Windgeschwindigkeit auf 10 m Höhe [\f$ m s^{-1} \f$] \n

![Windgeschwindigkeit] (windgeschw.svg " ")  

\f{equation}{
   k_w = 0.19 \cdot W_{10} - 0.015 \cdot {W_{10}}^2 + 0.002 \cdot {W_{10}}^3
\f}\n
\f$ k_w \f$: Belüftungsgeschwindigkeit durch Luftbewegung [m/d]

![bild_belueftung_schwach](windwirk_flaute.svg. "Belüftungswirkung schwache Winde")  

![bild belueftung_stark](windwirk.svg "Belüftungswirkung starke Winde")  \n

Im Falle der Nichtberücksichtigung des Windes wird \f$ k_w = 0 \f$ gesetzt.


# Sauerstoffproduktion durch die Photosyntheseleistungen von Algen {#lnk_o2_prod_phyto}

Die Sauerstoffproduktion durch die Photosyntheseleistungen von Algen 
berücksichtigt den photosynthetischen Quotienten opkima (etc) und den 
respiratorischen Quotienten ... opkimi (etc) und das 
Verhältnis von agrno3(ior)/agrnh4(ior)

<!-- im Dokuport war bisher ein Codeschnipsel eingefügt:
!!     planktische Gruenalgen  \n                                                       
!!      if(agrnh4(ior)==0.0)agrnh4(ior) = 0.00001 \n
!!      falgo = agrno3(ior)/agrnh4(ior) \n
!!      falgo = (opgrmi+falgo*opgrma)/(1.+falgo) \n
!!      dalgo(ior) = dalggr(ior)*falgo \n
!!     Benthische Gruenalgen   \n                                          
!!      abeowg(ior) = albewg(ior)*falgo 
!!     planktische kieselalgen!     \n                                                 
!!      if(akinh4(ior)==0.0)akinh4(ior) = 0.00001 \n
!!      falgo = akino3(ior)/akinh4(ior) \n
!!      falgo = (opkimi+falgo*opkima)/(1.+falgo) \n
!!      dalgo(ior) = dalgo(ior)+dalgki(ior)*falgo \n
!!     Benthische Kieselalgen                    \n                        
!!      abeowk(ior) = albewk(ior)*falgo \n
!!     planktische Blaualgen!            \n                                            
!!      if(ablnh4(ior)==0.0)ablnh4(ior) = 0.00001 \n
!!      falgo = ablno3(ior)/ablnh4(ior) \n
!!      falgo = (opblmi+falgo*opblma)/(1.+falgo) \n
!!      dalgo(ior) = dalgo(ior)+dalgbl(ior)*falgo \n -->



# Sauerstoffverbrauch durch die Respiration von Algen  {#lnk_o2_resp_phyto}

<!-- im Dokuport war bisher ein Codeschnipsel eingefügt:
!> \page Respirationalgen Respiration der Gruen-, Kiesel-, und Blaualgen
!!      dalgao(ior) = dalgag(ior)*opgrmi+dalgak(ior)*opkimi+dalgab(ior)*opblmi    \n                                           
!!      if(nkzs(ior)>1)then     ! 2D-Modellierung \n
!!        do nkz = 1,nkzs(ior) \n
!!          algaoz(nkz,ior) = algagz(nkz,ior)*opgrmi+algakz(nkz,ior)*opkimi+algabz(nkz,ior)*opblmi\n
!!        enddo\n
!!      endif                                    \n
!!!     Respiration der benthischen Gruenalgen und Kieselalgen   \n         
!!      abeorg(ior) = alberg(ior)*opgrmi \n
!!      abeork(ior) = alberk(ior)*opkimi \n
!!      if(nkzs(ior)>1)then \n
!!        do nkz = 1,nkzs(ior) ! O2-Produktion der Algen bei 2D-Modellierung \n
!!          if(agnh4z(nkz,ior)==0.0)agnh4z(nkz,ior) = 0.00001   ! Gruenalgen \n
!!          falgo = agno3z(nkz,ior)/agnh4z(nkz,ior) \n
!!          falgo = (opgrmi+falgo*opgrma)/(1.+falgo) \n
!!          dalgoz(nkz,ior) = dalggz(nkz,ior)*falgo \n
!!          if(aknh4z(nkz,ior)==0.0)aknh4z(nkz,ior) = 0.00001    ! Kieselalgen \n
!!          falgo = akno3z(nkz,ior)/aknh4z(nkz,ior) \n
!!          falgo = (opkimi+falgo*opkima)/(1.+falgo) \n
!!          dalgoz(nkz,ior) = dalgoz(nkz,ior)+dalgkz(nkz,ior)*falgo \n
!!          if(abnh4z(nkz,ior)==0.0)abnh4z(nkz,ior) = 0.00001   !Blaualgen \n
!!          falgo = abno3z(nkz,ior)/abnh4z(nkz,ior) \n
!!          falgo = (opblmi+falgo*opblma)/(1.+falgo) \n
!!          dalgoz(nkz,ior) = dalgoz(nkz,ior)+dalgbz(nkz,ior)*falgo \n
!!        enddo                                                     \n                  
!!      endif                                                       \n           

-->


# Produktion und Respiration durch Makrophyten {#lnk_o2_makrophyt}

Hier steht etwas Text. tralalala


# Sauerstoffverbrauch durch mikrobielle Oxidation organischer Kohlenstoffverbindungen {#lnk_o2_corg} 
*some text*

# Sauerstoffverbrauch durch Nitrifikation {#lnk_o2_nitrif} 
*some text*

 
# Sauerstoffverbrauch durch Zooplanktonplanktonrespiration {#lnk_o2_resp_zoop}
*some text*
<!-- Link Unterseite ehemals: Zooplanktonrespiration --> 

<!-- auf der Seite war ein Codeschnipsel eingefügt 
!!      ft = 1.047**(tempw(ior)-20.) \n
!!      vo2leb = (dzres1(ior)+dzres2(ior))*1.5 \n
!!      vo2leb = vo2leb*ft \n
!!      zooro2(ior) = vo2leb \n
-->

# Sauerstoffverbrauch durch benthischen Filtrierer (Dreissena) {#lnk_o2_dreissena}
*some text*

# Sauerstoffverbrauch im Sediment {#lnk_o2_sediment}
*some text*

<!-- oder Sauerstoffaustausch mit dem Sediment -->

\f{equation}{
    \Delta {O_2}_{Sed} = Sedflux_{O_2} \cdot \frac{\Delta t}{h}
\f}


# Sauerstoffänderung durch Wehranlagen

Die Sauerstoffänderungen durch Wehranlagen werden für jeden neuen Zeitschritt in
Abhängigkeit von der Wehrbreite, der Froude- und Reynolds-Zahl des Wehrüberfalls
und der Sauerstoffsättigung des Wassers im Oberwasser des Wehres separat 
berechnet:

\f{equation}{
  \Delta O_{2,Wehr} = (O_{2, Saett} - O_{2, OW}) \cdot \left(1 -
  \frac{1}{\alpha_{Wehr}} \right)
\f}

\f$ \Delta O_{2,Wehr} \f$: eingetragene Menge Sauerstoff beim Wehrüberfall 
      [\f$ g m^{-3} \f$] \n
\f$  O_{2, OW} \f$: Sauerstoffgehalt im Oberwasser [\f$ g m^{-3} \f$] \n
\f$ 1 - \frac{1}{\alpha_{Wehr}} \f$: Anteil des Defizits, der beim Wehrüberfall 
eingetragen wird [-]

\f$ \alpha_{Wehr} \f$ errechnet sich nach:

\f{equation}{
  \alpha_{Wehr} = 1 + 0,627 \cdot 10^{-4} \cdot Fr^{1,78} \cdot Re^{0,53}
\f}

*Fr*: Froudezahl des Wehrüberfalls [-] \n
*Re*: Reynoldszahl des Wehrüberfalls [-] \n

Mit der Froudezahl *Fr*:
\f{equation}{
  Fr = 1,488 \cdot \left(\frac{H^3_{Wehr} \cdot B_{Wehr}^2}{Q^2} \right)^{0,25}
\f}
<!-- #mf: Gleichung scheint noch nicht zu stimmen - schaue in Code -->

und der Reynoldszahl *Re*:
\f{equation}{
  Re = \frac{Q}{B_{Wehr}} \cdot 1,143 \cdot 10^6
\f}

\f$ H_{Wehr} \f$: Fallhöhe [m] (Differenz zwischen dem Wasserspiegel im Ober- 
                  und Unterwasser) \n
\f$ B_{Wehr} \f$: Wehrbreite [m] \n
\f$ Q \f$:        Abfluss [\f$\mqqs\f$] \n

Die temperaturabhängigen Größen sowie die Beschreibung der Temperaturabhängigkeit
sind in folgender Tabelle zusammengefasst:

Tabelle XX: Temperaturabhängige Größen und Beschreibung der Temperaturabhängigkeit 
im Sauerstoffbaustein

<!-- #mf: die Tabelle macht für mich keinen Sinn, weil die Größen oben anders
berechnet werden; in Code und Kurzdoku recherchieren -->

| Größen (G) |     |
| ---------- | --- |
| \f$ k_{2,OB} \f$      | \f$ f(T) = 1,024^{(T - 20)}   \f$ |
| \f$ \alpha_{Wehr} \f$ | \f$ f(T) = (1 + 0,046 \cdot T)/1,69 \f$ |



# QSim-Veröffentlichungen, zum Sauerstoff-Baustein:
* [Becker et al. 2009. Modelling the effects of thermal stratification on the 
   oxygen budget of an impounded river](./pdf/Becker_et_al_RRA_2009.pdf)  

* Schöl et al. 1999. <a href="./pdf/Schoel_et_al_1999mosel-saar.pdf" target="_blank"> 
  Model-based analysis of oxygen budget and biological processes in the
  regulated rivers Moselle and Saar: modelling the influence of benthic
  filter feeders on phytoplankton</a>\n 


Textquelle: sauerstoff-prozess.md; Code sources: module_oxygen.f90, oxygen.f90, 
oxygen_saturation_concentration.f90 and oxygen_wrapper_3d.f95;  
zurück: \ref lnk_sauerstoff