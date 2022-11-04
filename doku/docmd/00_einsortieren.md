Doku-Schnipsel  {#lnk_dokuschnipsel}
===============

* Doku-Schnipsel, die in altem Dokuportal nicht aufgetaucht sind und ggf.
 noch an richtiger Stelle einsortiert werden müssen *



-- aus stofftransport.f95 --

# Massenerhaltung {#lnk_massenerhalt}
nicht konservatives ELM-Verfahren.

Verfahren zur Abschätzung der Massenerhaltung: ... *to be continued*


Textquelle: diskretisierung.md ; Codesources: stofftransport.f95 ;  
zurück: \ref lnk_stofftransport_3d


<!--- nächster Schnipsel -->

# Diffusion {#lnk_diffusion}

\f[
\frac{\partial c_m}{\partial t} =
- \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
+ \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{Diffusion}
+ \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{Stoffumsatz}
- \underbrace {w_{s,3} \frac{\partial c_m}{\partial x_3} }_{Sinken}
\f]
 
zurück: \ref lnk_stofftransport_3d  ;  Quelle: stofftransport.f95


<!--- nächster Schnipsel -->

# mathematische Beschreibung als partielle Differentialgleichung {#lnk_pde}

In QSIM werden im Wasser gelöste Stoffe und planktisch lebende Organismen, die 
näherungsweise passiv im Wasser treiben, als Konzentrationen modelliert.

Die Veränderungen dieser Konzentrationen werden mit der untenstehenden 
Transport-Gleichung beschrieben.

\f[
  \frac{\partial c_m}{\partial t} =
  - \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
  + \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{Diffusion}
  + \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{Stoffumsatz}
  - \underbrace {w_{s,3} \frac{\partial c_m}{\partial x_3} }_{Sinken}
\f]

\f[
  \frac{\partial c_m}{\partial t} =
  - \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{advection}
  + \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{diffusion}
  + \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{source}
  - \underbrace {w_{s,3} \frac{\partial c_m}{\partial x_3} }_{settling/rise}
\f]

\f[
  c_m(t+\Delta t,\underline {x}) = c_m(t,{\underline {x}}_{orig}) + {\Delta c}_m^{diff} + {\Delta c}_m^{react}
\f]

\f[
  c_{m,k}(t+\Delta t) = A_{kl}(\Delta t^{adv}){c}_{m,l}^{diff,react} \quad ; \quad
  {c}_{m,k}^{diff,react}=D_{kl}(\Delta t^{diff}){c}_{m,l}^{react} \quad ; \quad
  {c}_{m,k}^{react}= Q_m(c_1(t) \ldots c_M(t), \underline {x}, t, \Delta t^{react})
\f]

\f[  
  n_1 \cdot \Delta t^{adv} = n_2 \cdot \Delta t^{diff} = \Delta t^{react}
\f]

\f[
  \frac{\partial c_m}{\partial t} = Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  - w_{s,3} \frac{\partial c_m}{\partial x_3} - v_i \frac{\partial c_m}{\partial x_i} + \frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}
\f]

mit: 
 - \f$ c_m \f$ = m-te Konzentration \n\n
 - \f$ t \f$ = Zeit\n\n
 - \f$ x_i \f$ = Ortskoordinaten in den mit i indizierten 3 Raumrichtungen\n\n
 - \f$ v_i \f$ = Strömungsgeschwindigkeiten in den mit i indizierten 3 Raumrichtungen (Einsteinsche Summationskonvention); \n
        siehe dazu \ref advect 
		
 - \f$ D_{ij} \f$ = Diffusivitätstensor (symetrisch), \n
        meist als isotrop angenommen \f$ D_{ij} = d \cdot \delta_{ij} \f$ mit \f$ d\f$=Diffusivität und \f$\delta_{ij}\f$=Einheitstensor; \n
        siehe dazu >> \ref diff \n\n
 - \f$ Q_m \f$ = Quelle/Senke der Konzentration m (lokale Veränderungsrate, Stoffumsatz); \n
         siehe dazu >> \ref lnk_wachstum_zerfall \n\n
 - \f$ w_{s,3} \f$ = Sinkgeschwindigkeit in der vertikalen (3) Raumrichtung;\n
         siehe dazu >> \ref sinken \n\n

\f[
  A_{ij} c_{j}^{n+1} = B_{ij} c_{j}^{n} + R_i
\f]

mit: 
  - \f$ c_{j}^{n} \f$ Konzentration am Knoten j zum Zeitpunkt n \n\n
  - \f$ c_{j}^{n+1} \f$ Konzentration am Knoten j zum auf n folgenden Zeitpunkt \n\n


\f[
  c^{n+1}_{m,i} = \sum_{k=1}^4 c^{n}_{m,Nr_i(k)}  \cdot w_i(k)
\f]

\f[
  c_{m,i}^{n+1a} = A_{ij} c_{m,j}^{n}
\f]

\f[
  c_{m,i}^{n+1d} = D_{ij} c_{m,j}^{n}
\f]

\f[
  c_{m,i}^{n+1q}( c_{1,i} \ldots c_{m,i} \ldots c_{M,i}, x_i, t )
\f]

\f[
  \frac{c_{i}^{n+1}-c_{j}^{n}}{\Delta t} =
\f]

mit: 
- \f$ c_{j}^{n} \f$ Konzentration am Knoten j zum Zeitpunkt n \n\n
- \f$ c_{j}^{n+1} \f$ Konzentration am Knoten j zum auf n folgenden Zeitpunkt


\f[
  c^{n+1}_i = \sum_{k=1}^4 c^{n}_{Nr_i(k)} \cdot w_i(k) + R_i
\f]

<!--- nächster Schnipsel -->

# Numerische Näherungslösung für o. g. partielle Differentialgleichung {#lnk_fractional}

Die numerische Näherungs-Lösung dieser Transportgleichungen an diskreten 
Berechnungspunkten im Raum und zu diskreten Zeitpunkten
erfolgt in zwei aufeinanderfolgen Schritten (<b>fractional step algorithm</b>):

In einem ersten Schritt werden \ref lnk_wachstum_zerfall und \ref sinken (im Weiteren als 
Stoffumsatz oder lokale Änderung bezeichnet) berechnet.
Daraus ergeben sich Konzentrationsfelder für den nächsten Zeitpunkt (als ob das 
Wasser steht).

Diese (Zwischen-) Konzentrationsfelder werden dann in einem zweiten Schritt der 
\ref lnk_advektion und \ref lnk_diffusion (im Weiteren als Transport bezeichnet) 
unterzogen.

Diese Teilung der numerischen Näherungs-Lösung auf der obersten Ebene in 
Stoffumsatz und Transport ist zulässig unter der Voraussetzung,
dass die Bedingungen für den Stoffumsatz entlang des Transportweges 
näherungsweise gleich bleiben. D. h., dass sich Konzentrationszusammensetzung 
und Randbedingungen (z.B. Sonneneinstrahlung) entlang des in einem Zeitschritt 
zurückgelegten Transportweges nur unwesentlich ändern.
Dies dürfte in Tidengewässern bei hinreichend kleinen Zeitschritten der Fall 
sein.\n\n
Für die Berechnungen in den beiden Schritten werden jeweils eigene numerische Näherungsverfahren eingesetzt, die in den jeweiligen Unterkapiteln 
(\ref lnk_wachstum_zerfall und \ref sinken, sowie \ref lnk_advektion und \ref lnk_diffusion) beschrieben 
sind.

<!-- nächster Schnipsel -->

* folgender Abschnitt z. Z. noch in Bearbeitung * 

SUBROUTINE  stofftransport(), allo_trans(), holen_trans(), transinfo_schritte(), 
transinfo_sichten() \n
trans_read_() und lossy_input_() in trans_read.c\n

Die Veränderungen dieser Konzentrationen wird mittels der untenstehenden 
Transport-Gleichung beschrieben:

\f[
  \frac{\partial c_m}{\partial t} =
  - \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
  + \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}
  {\partial x_j}}_{Diffusion}
  + \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{Stoffumsatz}
  - \underbrace {w_{s,3} \frac{\partial c_m}{\partial x_3} }_{Sinken}
\f]

\f[
  \frac{\partial c_m}{\partial t} = Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  - 
  w_{s,3} \frac{\partial c_m}{\partial x_3} - v_i \frac{\partial c_m}
  {\partial x_i} + \frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}
  {\partial x_j}
\f]

mit: 
 - \f$ c_m \f$ = m-te Konzentration \n\n
 - \f$ t \f$ = Zeit \n\n
 - \f$ x_i \f$ = Ortskoordinaten in den mit i indizierten 3 Raumrichtungen\n\n
 - \f$ v_i \f$ = Strömungsgeschwindigkeiten in den mit i indizierten 3 
    Raumrichtungen (Einsteinsche Summationskonvention); \n
    siehe dazu \ref advect \n\n
 - \f$ D_{ij} \f$ = Diffusivitätstensor (symetrisch), \n
        meist als isotrop angenommen \f$ D_{ij} = d \cdot \delta_{ij} \f$ mit 
		\f$ d\f$=Diffusivität und \f$\delta_{ij}\f$=Einheitstensor; \n
        siehe dazu \ref lnk_diffusion \n\n
 - \f$ Q_m \f$ = Quelle/Senke der Konzentration m (lokale Veränderungsrate, 
    Stoffumsatz); \n
    siehe dazu \ref lnk_wachstum_zerfall \n\n
 - \f$ w_{s,3} \f$ = Sinkgeschwindigkeit in der vertikalen (3) Raumrichtung;\n
         siehe dazu \ref sinken \n\n


eindimensional

\f[
  \frac{\partial c}{\partial t} = - v \frac{\partial c}{\partial x} + 
   \frac{\partial}{\partial x} D \frac{\partial c}{\partial x} + 
   Quelle/Senke(Stoffumsatz)
\f]

\f[
  \frac{\partial v}{\partial t} = - v \frac{\partial v}{\partial x} + 
  \frac{\partial}{\partial x} D \frac{\partial v}{\partial x} - 
  Wasserspiegel(Druck)gradient + Sohlreibung
\f]

\f$\frac{\partial c}{\partial x}\f$ ist groß
\f$\frac{\partial v}{\partial x}\f$ ist klein

Der Transport einer Konzentrationen im fließenden Wasser setzt sich aus 
\ref lnk_advektion und \ref lnk_diffusion zusammen. D. h. die in einem imaginären Wassertropfen 
gelösten Inhalte werden mit der mittleren Strömungsgeschwindigkeit entlang einer 
Bahnlinie verdriftet (\ref lnk_advektion) und durch die Turbulente Wirbelbewegung mit 
benachbarten "Wassertropfen" vermischt (\ref lnk_diffusion).

Die komplett ausgeschriebene Transportgleichung inclusive der aus dem 
Stoffumsatz stammenden Quellen und Senken finden Sie im Abschnitt 
\ref lnk_numerik .

Der Fractional Step Algorithmus, der in QSim3D realisiert ist (siehe 
\ref lnk_numerik), geht dabei von der Annahme aus, dass die 
Konzentrationsänderungen, die während eines Zeitschritts infolge 
von stoffumsatz() stattfinden, mit hinreichender Genauigkeit vor dem 
Transportvorgang berechnet werden können.
Damit diese Näherung zutrifft, muss der stoffumsatz() am Strombahnurspung
annähernd genauso stattfinden wie entlang des Transportweges.


Die Verwendung von unterschiedlichen Netzen für die Strömungsberechnung und die 
Gütesimulation ist zunächst nicht weiterverfolgt worden; würde sie angewendet, 
müsste eine Zwischeninterpolatin erfolgen, z. B. mit dem Programm "Raster" von 
Herrn Wirth.

zurück: \ref lnk_numerik ;  Quelle: stofftransport.f95

!----+-----+----
!> Subroutine stofftransport() macht jetzt nur noch die Verzweigung zu den 
unterschiedlichen Hydraulischen Treibern und Transportalgorithmen

-- aus ausgabe.f95 --

<!-- nächster Schnipsel -->
!> macht nur Verzweigung nach hydraulischem treiber wegen deren unterschiedlichen Datenstrukturen
\n\n
aus: ausgabe.f95 ; zurück: \ref lnk_ergebnisausgabe

-- aus module_uebergabe_werte.f95 --
!----+-----+----
!> <h1>broadcast_parameter() alle prozessoren bekommen die Parameter aus aparam.txt</h1>
nicht mehr über transfer_parameter_p sondern direkt aus module_QSimDatenfelder.f95
aus qsim13.3:
<code>\verbatim
      read(55,5500,iostat=read_error)agchl,aggmax,IKge,agksn,agksp
      read(55,5502,iostat=read_error)agremi,frmuge,bsbgr,csbgr,Qmx_NG
      read(55,5504,iostat=read_error)Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG
      read(55,5506,iostat=read_error)opgrmi,opgrma,asgre,ToptG,kTemp_Gr
      read(55,5507,iostat=read_error)akchl,akgmax,IKke,akksn,akksp
      read(55,5508,iostat=read_error)akkssi,akremi,frmuke,bsbki,csbki
      read(55,5510,iostat=read_error)Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK
      read(55,5512,iostat=read_error)Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi
      read(55,5514,iostat=read_error)opkima,askie,ToptK,kTemp_Ki,abchl
      read(55,5516,iostat=read_error)abgmax,IKbe,abksn,abksp,abremi
      read(55,5518,iostat=read_error)frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB
      read(55,5520,iostat=read_error)Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi
      read(55,5522,iostat=read_error)opblma,asble,ToptB,kTemp_Bl,ifix
      read(55,5524,iostat=read_error)irmaxe,FopIRe,GROT,zresge,zakie
      read(55,5526,iostat=read_error)zagre,zable,ynmx1e,stks1e,anitrie
      read(55,5530,iostat=read_error)bnmx1e,bnks1e,ynmx2e,stks2e,anitrie
      read(55,5528,iostat=read_error)bnmx2e,bnks2e,KNH4e,KapN3e,hyPe
      read(55,5533,iostat=read_error)hymxDe,KsD1e,KsD2e,KsMe,upBACe
      read(55,5535,iostat=read_error)YBACe,rsGBACe,FoptDe,upHNFe,BACkse
      read(55,5538,iostat=read_error)alamda,fPOC1e,fPOC2e,SorpCape,Klange
      read(55,5540,iostat=read_error)KdNh3e
\endverbatim</code>
aus module_uebergabe_werte.f95

<!-- nächster Schnipsel -->

-- aus zonen.f95 --

!> Die Datei zonen.f95 dient dazu ... ??? die Informationen zu einer Zone
(räumlich abgegrenzter bereich mit gleichen Eigenschaften) zu speichern.
Jeder 2D-Knoten (3D-Vertikale) des HN-Modells bringt eine Zonennummer mit,
über die die Zuordnung der Eigenschaften erfolgt.

!       module zonen
!       implicit none
!       save
!       integer :: zonen_anzahl
!       integer , allocatable , dimension (:) :: wetterstations_nummer, zonen_nummer, schifffahrts_zone, ini_randnr
!       real , allocatable , dimension (:)  :: wetterstations_lage, reib_ks
!       PUBLIC :: modellg, zonen_parallel
!       CONTAINS
!
!----+-----+----
!> verteilt Zoneninformationen auf die Prozesse \n
aus zonen.f95


<!-- nächster Schnipsel -->

-- aus schwebstoff_salz.f95 --

!----+-----+----
!> die SUBROUTINE schwebstoff_salz() belegt die \ref lnk_tiefengemittelte_plankt_var 53 planktonic_variable(53 ,
QSim1D-Name "ss", in der die tiefengemittelte Schwebstoffverteilung in ??? gespeichert wird.
Abhängig vom rechenzeit-Punkt werden die Verteilungen mit der subroutine verteilung_holen_gr3() aus den
d-Dateien im Unterverszeichnis trueb geholt. Zwischen den in trueb vorhandenen Zeitpunkten wird interpoliert.
vor dem ersten und nach dem letzten wird die erste, resp. letzte Verteilung genommen.
\n\n
Quelle schwebstoff_salz.f95 zurück zu \ref lnk_schwebstoff_salz oder \ref lnk_ueberblick


<!-- nächster Schnipsel -->

-- aus stofftransport_casu.f95 --
in der Subroutine transport.f90 (QSim v12.40) werden 70 Größen transportiert. Sprunglabel 911 goto ktrans \n
Die folgende Liste enthält nun die Angaben, von welcher Subroutine/Modul die jeweiligen planktischen Konzentrationen geändert werden.

| Sprunglabel | Variablenname | Beschreibung    | Subroutine    |

| 600 | tempw    | Temperatur          | temperw_huelle()    |
||
| 602 | vo2       | Sauerstoffgehalt       | oxygen    |
||
| 604 | vNH4       | Ammonium          | ncyc_huelle()       |
| 606 | vNO2       | Nitrit          | ncyc_huelle()       |
| 608 | vNO3       | Nitrat          | ncyc_huelle()       |
| 610 | vx0       | Nitrosomonas        | ncyc_huelle()       |
| 612 | vx02       | Nitrobacter         | ncyc_huelle()       |
| 532 | gesN       | GesamtStickstoff       | ncyc_huelle()       |
||
| 534 | gesP       | GesamtPhosphat??       | po4s       |
| 616 | gelP       | ortho-Phosphat       | po4s       |
||
| 614 | Si       | Silikat          | silikat    |
||
| 618 | obsb       | biochemischer Sauerstoffbed.    | orgC       |
| 672 | CD(1       | ??             | orgC       |
| 674 | CD(2       | ??             | orgC       |
| 676 | CP(1       | ??             | orgC       |
| 678 | CP(2       | ??             | orgC       |
| 680 | CM       | ??             | orgC       |
| 682 | BAC       | ??             | orgC       |
| 684 | O2BSB    | ??             | orgC       |
| 686 | BL01       | ??             | orgC       |
| 688 | BL02       | ??             | orgC       |
| 690 | vbsb       | ??             | orgC       |
| 692 | vcsb       | ??             | orgC       |
| 508 | fbsgr    | ??             | orgC          |
| 510 | frfgr    | ??             | orgC          |
| 512 | nl0       | Verhältnis von Stickstoff zu Kohlenstoff in organischem Material   | orgC          |
| 514 | pl0       | Phosphor?             | orgC          |
||
| 622 | chla       | ??                | algaesgr    |
| 624 | chlaki    | ??                | algaesgr    |
| 626 | chlagr    | ??                | algaesgr    |
| 648 | agbcm    | Biomasse/Kohlenstoff gruen-Algen   | algaesgr    |
| 628 | chlabl    | ??                | algaesgr    |
| 632 | agr       | ??                | algaesgr    |
| 636 | vkigr    | ??                | algaesgr    |
| 638 | antbl    | ??                | algaesgr    |
| 642 | svhemg    | ??                | algaesgr    |
| 664 | Q_NG       | Stickstoffanteil der Gruen-Algen    | algaesgr    |
| 666 | Q_PG       | ??                | algaesgr    |
||
| 658 | Q_NK       | Stickstoffanteil der Kiesel-Algen    | algaeski    |
| 630 | aki       | ??                | algaeski    |
| 640 | svhemk    | ??                | algaeski    |
| 646 | akbcm    | Biomasse/Kohlenstoff kiesel-Algen   | algaeski    |
| 536 | SKmor    | ??                | algaeski    |
| 660 | Q_PK       | Phosporanteil Kieselalgen       | algaeski    |
| 662 | Q_SK       | Siliziumanteil Kieselalgen      | algaeski    |
||
| 634 | abl       | ??                | algaesbl    |
| 644 | svhemb    | ??                | algaesbl    |
| 650 | abbcm    | Biomasse/Kohlenstoff blau-Algen   | algaesbl    |
| 668 | Q_NB       | Stickstoffanteil der Blau-Algen    | algaesbl    |
| 670 | Q_PB       | ??                | algaesbl    |
||
| 652 | akiiv    | wohl unbenutzt       | ????       |
| 654 | agriv    | wohl unbenutzt       | ????       |
| 656 | abliv    | wohl unbenutzt       | ????       |
| 500 | abrzo1    | wohl unbenutzt       | ????       |
||
| 694 | CHNF       | ??             | HNF       |
| 696 | BVHNF    | ??             | HNF       |
||
| 504 | ss       | ORG. UND ANORG. SCHWEBSTOFFE    | algaeski + SCHWEB    |
||
| 502 | ssalg    | GESAMTSCHWEBSTOFFE       | SCHWEB       |
| 506 | fssgr    | ??             | SCHWEB       |
||
| 698 | zooind    | Anzahl der Rotatorien      | konsum    |
||
| 518 | dlarvn    | ??             | dreissen       |
||
| 516 | stind    | ??             | ph          |
| 522 | mw       | ??             | ph       |
| 524 | pw       | ??             | ph       |
| 526 | ca       | ??             | ph       |
| 528 | lf       | Leitfähigkeit         | ph       |
| 530 | vh       | ??             | ph(vph)    |
||
| 537 | DOSCF    | ??             | COLIFORM    |
| 520 | coli       | ??             | COLIFORM    |

--------------------------------------------------------------------------

<!-- nächster Schnipsel -->

-- aus stoffumsatz.f95 --

!      ! Lasttest
!      ilast=10000
!      i1last=ilast/proz_anz
!      rcount=0.0
!      rlast = 7
!      do i1=1,i1last !! Rechenlast erzeugen !!
!         rlast = rlast + (1000   + meinrang + real(i1))
!         do i2=1,ilast
!            rlast = rlast + (1000   + meinrang + real(i2))
!            do i3=1,ilast
!               rlast = rlast + (1000   + meinrang + real(i3))
!               rlast = rlast - (1000   + meinrang + real(i3))
!               rcount=rcount+0.00001
!            end do !! i3
!            rlast = rlast - (1000   + meinrang + real(i2))
!         end do !! i2
!         rlast = rlast - (1000   + meinrang + real(i1))
!      end do !! i1
!      print*,'Rechenlast, rcount=',rcount,' meinrang', meinrang,' part=',part
!      1 Prozessor>16:49:34-17:10:42=21:08=1268s  16 Prozess.>17:12:41-17:14:32=01:51=111s*16=1776s  71%
!
!! *\f[
!! {c_m}^{n+1}={c_m}^{n} + \delta t \cdot Q ( {c_1}^{n} \ldots {c_m}^{n} \ldots {c_M}^{n}, {x_i}^{n}, t^{n} )
!! \f] \n


--------------------------------------------------------------------------

<!-- nächster Schnipsel -->

-- aus eingabe.f95 --

!! enthält die Definition der unveränderlichen biologischen u.&nbsp;a. die Güte-Berechnung definierenden Verhältnisse in den Zonen des Modells.
!! Es gelten die \ref lnk_datei_regeln "allgemeinen Regeln für QSim-Dateien".
!! \n\n
!! enthält die Definition der unveränderlichen biologischen u.&nbsp;a. die Güte-Berechnung definierenden Verhältnisse in den Zonen des Modells.
!! <a href="./exp/ModellG.3D.txt" target="_blank">ModellG.3D.txt</a>\n
!! \ref lnk_kopfzeile

!
!----+-----+----
!> Dient der eingabe() von MODELLG.3D.txt. \n siehe \ref lnk_modell_g3 \n
\n\n aus modellg.f95 , zurück: \ref lnk_datenmodell


--------------------------------------------------------------------------

<!-- nächster Schnipsel -->

-- aus stoffumsatz.f95 --

!! Der Stoffumsatz wird im mehrdimensionalen QSim3D auf identische Weise wie im 1-dimensionalen QSim modelliert. 
!! Um dies sicherzustellen werden programmtechnisch dieselben Quellcode-Dateien einkompiliert, 
!! aus denen auch die Software QSim erzeugt wird (QSim-Subroutinen). 
!! Im Detail ist dies im Abschnitt \ref lnk_huellen beschrieben.\n
!! Der aktuell verwendete QSim1D-source-code wird im Abschnitt \ref lnk_download bereitgestellt.
!! \n\n
!! Die lokal ablaufenden biologischen und chemischen Stoffumsetzungs-Prozesse werden 
!! numerisch als \ref lnk_wachstum_zerfall diskretisiert.
!! \n\n
!! Wird QSim zur Ermittlung der \ref lnk_aufenthaltszeit eingesetzt, ist der Stoffumsatz komplett ausgeschaltet.

!!
!!\n\n zurück: \ref lnk_ueberblick; Code: stoffumsatz.f95
!! \n
!! <hr>
!! <a name="1">1)</a>
!!     Obwohl es sich eigentlich um einen physikalischen Vorgang handelt, wird die Wärmebilanz hier trotzdem mit aufgeführt. 
!!     Zum einen, weil auch die Temperatur wie eine Konzentration behandelt wird, 
!!     die durch Wärmeaustausch mit der Umgebung anwachsen oder abnehmen kann.
!!     Zum anderen, weil auch dafür die bewährte QSim-Subroutine temperw() verwendet wird.
!! <hr>





--------------------------------------------------------------------------

<!-- nächster Schnipsel -->

-- aus xx.f95 --



--------------------------------------------------------------------------

<!-- nächster Schnipsel -->

-- aus xx.f95 --

