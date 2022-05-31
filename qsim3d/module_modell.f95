!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

!> \page lnk_Datentechnik Informationstechnische Umsetzung
!!
!! \section Datenstruktur Datenstruktur
!! In Qsim3D wurden Variablenfelder gemäß ihrer Funktion in den untenstehenden Gruppen zusammengeasst.
!! In QSim1D existiert diese Gruppierung noch nicht. 
!! Die Namen der Variablenfelder aus QSim1D wurden in QSim3D übernommen. Siehe dazu: \subpage hüllen
!! 
!! <ol>
!! <li>\subpage planktische_variablen, die im fließenden Wasser transportiert werden. 
!!     Diese Datenfelder werden nach jedem parallelen Stoffumsetzungs-Schritt zusammengesammelt 
!!     (MPI_gather in gather_planktkon()),
!!     um dann transportiert (stofftransport()) zu werden. 
!!     Nach dem gemeinsamen Transport (Advektions-Diffusions-Simulation)
!!     werden die Datenfelder wieder auf die parallelen Prozesse verteilt 
!!     (MPI_scatter in scatter_planktkon()).
!!     \n \subpage lnk_partik_planktik
!! \n\n</li>
!! <li>\subpage benthische_verteilungen, beschreibt Eigenschaften der Gewässersohle
!!     Diese Variablen sind ortsfest und 2-dimensional. 
!!     Die Initialisierungswerte werden vor dem ersten Zeitschritt verteilt (scatter_benth()).
!!     Eingesammelt werden müssen diese Variablenfelder nur zum Zweck der Ausgabe (gather_benth()).
!! \n\n</li>
!! <li>\subpage uebergabe_werte hält Daten, die dem Datenaustausch zwischen den verschiedenen 
!!     Stoffumsetzungsmodulen dienen
!!     und für die kein Transport berechnet werden muss. 
!!     Es handelt sich um 0D, 2D und 3D Variablen.\n
!!     Dieses Modul hält auch die \ref globaleParameter .\n
!!     nach dem Einlesen/Initialisierung müssen die Werte auf die parallelen Prozesse verteilt werden (scatter_uebergabe()).
!!     Eingesammelt werden müssen diese Variablenfelder nur zum Zweck der Ausgabe (gather_uebergabe()).
!! \n\n</li>
!! <li>Randdaten, d.h Vorgaben, die sich im Verlauf der Simulationszeitschritte infolge externer Setzungen verändern,
!!     sind in die folgenden Datenstrukturen aufgeteilt:
!!     \ref zuflussranddaten, \n
!!     \ref hydraul_rb , deren Änderung vom Hydraulischen Treiber vorab berechnet wurde und\n
!!     \ref wetter_rb \n
!!     Ihre Werte müssen vor jedem Stoffumsetzungs-Zeitschritt verteilt werden (scatter_RB()). 
!!     Ein Widereinsammeln ist nicht erforderlich.
!! \n\n</li>
!! <li> Je nach hydraulischem Treiber werden unterschiedliche Datenfelder für die Übernahme verwendet.\n\n
!!      - \subpage Transportinformationen \n
!!      - Zellrandflüsse aus Untrim\n
!!      - Anbindung von SCHISM steht noch aus.
!! \n\n</li>
!! <li> \subpage lnk_steuerparameter werden Programmintern zur Ablaufsteuerung und zur Verfahrensauswahl benutzt.
!! \n\n</li>
!! </ol>
!! 
!! \subpage Parallelisierung \n
!! 
!! \n\n aus Datei: module_modell.f95 ; zurück:\ref index

!--------------------------------------------------------------------------------------------------------------

!> \page hüllen Verbindung von QSim-3D mit QSim-1D 
!! \n\n
!! Die Idee von QSim-3D ist es, dieselbe Gewässergüte-Simulation wie QSim-1D durchzuführen,\n\n
!! Nur die räumliche Auflösung des Wasserkörpers erfolgt 2D-tiefengemittelt oder voll 3-dimensional in QSim-3D
!! statt 1D oder 2D-breitengemittelt wie in QSim-1D.\n
!! Die Simulation der im Gewässer lokal ablaufenden Prozesse (lokaler stoffumsatz()) bleibt identisch.
!! \n\n
!! Der aktuell verwendete QSim1D-source-code wird im Abschnitt \ref lnk_download bereitgestellt.
!! \n\n
!! Um mit denselben Subroutinen arbeiten zu können, die auch das Programm QSim verwendet, wurden hier Hüllroutinen geschaffen,
!! deren einzige Aufgabe darin besteht, die in QSim-3D geänderte Datenstruktur an die QSim-Subroutinen zu übergeben.\n
!! Die wichtigste Umstellung ist dabei, dass die Hüllroutinen punktweise aufrufbar sind, während die QSim-Subroutinen
!! strangweise arbeiten. Beim Aufruf durch die Hüllroutine wird die QSim-Subroutine in den Glauben versetzt,
!! einen Strang zu bearbeiten, der nur aus einem Profil besteht.
!! \n\n
!! Ausserdem werden die Programmdateien der Hüllroutinen zur Dokumentation der QSim-Subroutinen genutzt.
!! \n\n
!! Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n 
!! \n\n
!! Die Tiefenverteilung von QSim1D (welche dort als 2D bezeichnet wird) ist dort nicht komplett für ale Konzentrationen formuliert. 
!! Eine Übernahme ins mehrdimensionale ist nicht geplant.
!! QSim3D arbeitet z. Zt. (Mai 2018) noch durchgängig tiefenintegriert.
!! \n\n 
!! aus Datei  module_modell.f95 ; zurück zu \ref lnk_Datentechnik

!--------------------------------------------------------------------------------------------------------------- modell
!> Das module ::modell 
!! speichert die Information zum 
!! <ul>
!!    <li>Modellverzeichnis</li>
!!    <li>Netz</li>
!!    <li>Zeit</li>
!!    <li>Vermaschung</li>
!! </ul>
!! und wird mehr und mehr zum zentralen common-block \n
!! es inkludiert bereits die Definitionen der
!! <ul>
!!    <li>\ref planktische_variablen</li>
!!    <li>\ref Ergebnisse</li>
!! </ul>
!! \n\n
!! aus Datei module_modell.f95
      module modell
      implicit none
      save   ! stellt sicher, dass der Inhalt in den Speicherplaetzen
             ! zwischen den einzelnen Einbindevorgaengen in den
             ! einzelnen Programmeinheiten unveraendert bleibt 

include 'mpif.h' !!/mreferate/wyrwa/casulli/mpich2/mpich2-1.3.2p1/src/include/mpif.h: integer*8 und real*8 raus
       ! die in mpif.h enthaltenen SAVE statements sind dann 
       ! doppelt gemoppelt und führen zu einer Warning, die aber ignoriert werden kann.

       real :: PI
       real , parameter :: grav=9.81
!-------------------------------------------------------------------------------parallel_datenfelder
! Beschreibung in parallel.f95
!> nummer und Gesamtzahl prozessoren (MPI)
      integer :: meinrang, part, proz_anz
      !!wy integer :: mpi_komm_welt, ierror
      integer :: mpi_komm_welt, ierr

!-------------------------------------------------------------------------------Modell+Netz
!> modellverzeichnis etc.
      integer , parameter :: longname=3000
      character (len=longname) :: pfad, modellverzeichnis, codesource, email,fehler
      !character (len=4000) :: progressfile
      logical :: send_email
      integer strlaeng
!> \anchor kontrollknoten Nummer des Kontrollknotens (Untrim-Elementnummer)
      integer  :: kontrollknoten
!> \anchor kontroll Schalter ob Kontrollausgabe aus Stoffumsatzroutinen heraus erfolgen soll
      logical  :: kontroll
!> \anchor iglob globale Knotennummer, Übergabe an Stoffumsatzroutinen aus parallel laufenden Hüll-routinen
      integer  :: iglob


 !! Simulations-otionen ! Berechnungs- und Ausgabeflags
      !aus EREIGG.txt:  read(92,9220)imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren,ilongDis,FlongDis,iColi,ikonsS,iSchwer,iphy
!> \anchor imitt Tagesmittelwertausgabe=1 Zeitwerte=0 | nicht aktiv in 3D
      integer :: imitt 
!> \anchor iph Schalter für die ph-Wert-berechnung; 1-ein , 0-aus ; wird von ereigg_modell() eingelesen.
      integer :: ipH
!> \anchor idl Dispersionskoeffizienten einlesen=0 berechnen=1 in QSim3D bisher unbenutzt
      integer :: idl
!> \anchor itemp nur Temperatursimulation=1 alles=0 in QSim3D bisher unbenutzt
      integer :: itemp
!> \anchor itracer nur Tracersimulation=1 alles=0 in QSim3D bisher unbenutzt
      integer :: itracer
!> \anchor ieros Erosions-flag mit Erosion=1 ohne=0 in QSim3D bisher unbenutzt
      integer :: ieros
!> \anchor ischwa mit ereigg2=1 ohne=0 ??? in QSim3D bisher unbenutzt
      integer :: ischwa
!> \anchor iverfahren Transportverfahren in QSim3D bisher unbenutzt
      integer :: iverfahren
!> \anchor ilongdis ??? in QSim3D bisher unbenutzt ilongDis
      integer :: ilongDis
!> \anchor flongdis Faktor Dispersionskoeffizient, Eingabe über EREIGG.txt ??? in QSim3D bisher unbenutzt FlongDis
      real :: FlongDis
!> \anchor icoli ??? in QSim3D bisher unbenutzt
      integer :: iColi
!> \anchor ikonss ??? in QSim3D bisher unbenutzt
      integer :: ikonsS
!> \anchor ischwer ??? in QSim3D bisher unbenutzt
      integer :: iSchwer
!> \anchor iphy Berechnungsoption für Oberflächenbelüftung in oxygen.f90:\n
!!     iphy = 1       ! neue Formel von mir mit Wind\n
!!     iphy = 2       ! neue Formel von mir ohne Wind\n
!!     iphy = 3       ! Formel von Wolf ##Formelfehler## k2=10.47*v^0.43*H^-1.37*S^0.22+K2wind (Dantengrundlage Wolf 1974)" Help="Berechnung nach Wolf (überarbeitete Form)" />'\n
!!     iphy = 4       ! Formel von Melching\n
      integer :: iphy
!> \anchor iformVert Verteilungsfunktion Schwermetalle 1-DWA-Modell 2-Deltares 2010
      integer :: iformVert
!> \anchor IFORM_VERDR:   Schalter für die Auswahl der Verdunstungsformeln in temperw_kern.f90 \n
!!    iform_VerdR==1 ! WMO (FGSM-Handbuch)
!!    iform_VerdR==2 ! Sweers (1976) over Land
!!    iform_VerdR==3 ! Rimsha & Donschenko
!!    iform_VerdR==4 ! Priestley-Taylor (1972)
!!    iform_VerdR==5 ! Delclaux et al. (2007)
      integer :: iform_verdr

!> \anchor iwsim Kennung, Simulationstyp ?
      integer :: iwsim


!> Netz-variablen
      integer knotenanzahl2D, knotenanzahl3D
      real , allocatable , dimension (:) :: knoten_x, knoten_y, knoten_z
      integer , allocatable , dimension (:) :: knoten_rand
!> \anchor knoten_zone Zonen-zähler an den Knoten bei Antrieb mit casu-Hydraulik aus points
      integer , allocatable , dimension (:) :: knoten_zone, knoten_rang
      real , allocatable , dimension (:) :: knoten_flaeche, knoten_volumen
      integer min_rand, max_rand, min_zone, max_zone
      real :: modell_geob, modell_geol, modell_flaeche
      real :: mittelflaech, mittelvolumen
      integer , allocatable , dimension (:) :: knot_ele ! Anzahl der Elemente an einem Knoten (nur untrim)

!> Strings bis zu 2000 Zeichen lang
      character (len=2000) :: ctext 

!> \anchor rechenzeit aktuelle rechenzeit in ganzen Sekunden
      integer :: rechenzeit
!> \anchor deltat Zeitschrittweite (Stoffumsatz) in ganzen Sekunden
      integer :: deltat !! in ganzen Sekunden
!> \anchor zeitschrittanzahl Zeitschrittanzahl die von der Berechnung (Ereignis) durchlaufen werden.
      integer :: zeitschrittanzahl
!> \anchor izeit izeit Zeitschrittzähler
      integer :: izeit
!> \anchor startzeitpunkt startzeitpunkt in ganzen Sekunden
      integer :: startzeitpunkt
!> \anchor endzeitpunkt endzeitpunkt in ganzen Sekunden
      integer :: endzeitpunkt
!> \anchor referenzjahr Referenzjahr\n
!! Darf nur ein Schaltjahr sein, sonst werden die Tage flasch gezählt\n
!! darf nur max. 65 Jahre vor Berechnungsjahr liegen damit int*4 zum Sekunden zählen reicht und\n
!! darf nur max. 20 Jahre vor Berechnungsjahr liegen, um Zeitfehler abfangen zu können.\n
      integer , parameter :: referenzjahr=2008
      integer , parameter :: vier=4
      integer :: time_offset !! von transinfo/meta
      character (len=200) :: time_offset_string
!> \anchor zeitpunkt Variable zur Zwischenspeicherung eines Zeitpunkts in ganzen Sekunden (siehe \ref rechenzeit)
      integer :: zeitpunkt !! in ganzen Sekunden
!> \anchor jahr jahr berechnet von zeitsekunde() aus \ref zeitpunkt
      integer jahr
!> \anchor monat monat berechnet von zeitsekunde() aus \ref zeitpunkt
      integer monat
!> \anchor tagdesjahres tagdesjahres berechnet von zeitsekunde() aus \ref zeitpunkt
      integer tagdesjahres
!> \anchor tag tag berechnet von zeitsekunde() aus \ref zeitpunkt
      integer tag
!> \anchor uhrzeit_stunde Uhrzeit als Stunde.Minute zum Einlesen von EREIGG.txt\n
!!  sekundenzeit(2) rechnet es Stundendezimale um
      real :: uhrzeit_stunde
      real :: uhrzeit_stunde_vorher
!> \anchor stunde Stunde als ganze Zahl berechnet von zeitsekunde() aus \ref zeitpunkt
      integer stunde
!> \anchor minute minute berechnet von zeitsekunde() aus \ref zeitpunkt
      integer minute
!> \anchor sekunde sekunde berechnet von zeitsekunde() aus \ref zeitpunkt
      integer sekunde
!> \anchor anzZeit anzZeit Anzahl der erosionslosen Zeitschritte im bisherigen Rechenlauf ; zurÃ¼ck: \ref lnk_schwermetalle
      integer anzZeit
	  
!> Vermaschung Elemente
      logical :: element_vorhanden
      integer :: n_elemente, summ_ne
      integer , allocatable , dimension (:) :: cornernumber
      integer , allocatable , dimension (:) :: element_rand ! aus Mesh2_face_bc, netCDF
!> \anchor element_zone Element zonen, null gesetzt, wird bei Antrieb mit untrim-Hydraulik aus netCDF-Dateien verwendet
      integer , allocatable , dimension (:) :: element_zone
      integer , allocatable , dimension (:,:) :: elementnodes, elementedges
      real , allocatable , dimension (:) :: element_x, element_y

!> Vermaschung Kanten
      logical :: kanten_vorhanden
      integer :: kantenanzahl
      real , allocatable , dimension (:) :: edge_normal_x,edge_normal_y
      real , allocatable , dimension (:) :: edge_ground, cell_bound_length
      integer , allocatable , dimension (:) :: top_node,bottom_node,left_element,right_element, boundary_number, zon_num
      real , allocatable , dimension (:) :: edge_mid_x, edge_mid_y ! mid-side location of edges

!> toleranzen und clipping-Werte
      real , parameter :: min_tief=0.01

!> Steuerung des Stoffumsatzes eingelesen über itemp
      logical :: nur_temp

!-------------------------------------------------------------------------------zonen_datenfelder
!> Beschreibung in zonen.f95
      integer :: zonen_anzahl
!> \anchor wetterstations_nummer enthält zu jedem Zonen-zähler den dazugehörigen 
!!  Wetterstations-Zähler, der in eingabe() aus Wetterstationskennung_T bestimmt wird
!      integer , allocatable , dimension (:) :: wetterstations_nummer
!      integer , allocatable , dimension (:) :: zonen_nummer
!      integer , allocatable , dimension (:) :: ini_randnr
!      real , allocatable , dimension (:)  :: wetterstations_lage, reib_ks
!inaktiv      real , allocatable , dimension (:)  :: tracermasse
!> Feld Zonen-namen
!      character(200) , allocatable , dimension (:) :: zonen_name

!! Sed-Flux variablen
!> \anchor sedom sedom Anteil des organischen Materials im Sediment von modellg.3D.txt, POMz -> hsedom
!      real , allocatable , dimension (:)  :: sedom
!> \anchor bedgs bedgs Bedeckungsgrad der Sohle mit Sediment (0-1), von modellg.3D.txt, BedGSz -> hbedgs
!      real , allocatable , dimension (:)  :: bedgs
!> \anchor sedvvert sedvvert volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h, von modellg.3D.txt, Sedvvertz -> hsedvvert
!      real , allocatable , dimension (:)  :: sedvvert
!> \anchor kornd kornd Vorgabe Korndurchmesser d50 sediment in der Zone ggf.
!      real , allocatable , dimension (:)  :: kornd
!> \anchor burial burial Burial-Geschwindigkeit (Sedimentation) Vorgabe in der Zone ggf.
!      real , allocatable , dimension (:)  :: burial 
      type :: sedimentfluss
         real :: sedom,bedgs,sedvvert,kornd,burial
      end type sedimentfluss

!! Sediment-Temperatuf variablen
!> \anchor spewks spewks Spez. WärmeKapazität Sediment" unit="KJ/(kg*K)
!      real , allocatable , dimension (:)  :: spewks
!> \anchor wuebk wuebk Wärmeübergangskoeffizient" unit="KJ/(K*m2*h)
!      real , allocatable , dimension (:)  :: wuebk
!> \anchor psrefs psrefs Reflektionsanteil der Strahlung an der Sedimentoberfläche
!      real , allocatable , dimension (:)  :: psrefs
!> \anchor extiks extiks Extinktionskoeffizient für PARS (nur bei Temperaturmodellierung erforderlich!) zonenweise, siehe \ref extks
!      real , allocatable , dimension (:)  :: extiks
      type :: sedimenttemperatur
         real :: spewks,wuebk,psrefs,extiks
      end type sedimenttemperatur

!! Dreissena Laichperiode variablen
!> \anchor lait lait Dreissena Laichperiode: Tag des Beginns der Laichperiode, aus der L-Zeile von <a href="./exp/ModellG.3D.txt" target="_blank">ModellG.3D.txt</a>
!      integer , allocatable , dimension (:)  :: lait
!> \anchor laim laim Dreissena Laichperiode: Monat des Beginns der Laichperiode, aus der L-Zeile von <a href="./exp/ModellG.3D.txt" target="_blank">ModellG.3D.txt</a>
!      integer , allocatable , dimension (:)  :: laim
!> \anchor laid laid Dreissena Laichperiode: Dauer der Laichperiode in Tagen, aus der L-Zeile von <a href="./exp/ModellG.3D.txt" target="_blank">ModellG.3D.txt</a>
!      integer , allocatable , dimension (:)  :: laid
      type :: laichperiode
         integer :: lait,laim,laid
      end type laichperiode

!! WRITE(1, '(A)') '<ParamSetDef Id="QF" Text="Schiffsverkehr" Help="Schiffsverkehr auf den Gewässer-Abschnitten" Scope="Abschnitt">'
!! WRITE(1, '(A)') '  <Parameter Ident="VSCHIFF" Text="Schiffsgeschwindigkeit" Unit="m/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="1.5" />'
!! WRITE(1, '(A)') '  <Parameter Ident="UPROP" Text="Drehzahl des Propellers" Unit="U/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="3.33" />'
!> \anchor schifffahrts_zone schifffahrts_zone Schiffsverkehr, von modellg.3D.txt F-Zeile schifffahrts_zone -> mss
!      integer , allocatable , dimension (:) :: schifffahrts_zone
      type :: schiffsverkehr
         real :: vschiff,uprop
         integer :: schifffahrts_zone
      end type schiffsverkehr

!! WRITE(1, '(A)') '<ParamSetDef Id="QT" Text="Wetterstation" Help="Wetterstations-Zuordnung" Scope="Abschnitt">'
!! WRITE(1, '(A)') '  <Parameter Ident="WStation" Text="Wetterstation" Unit="" Format="I4" Null="-1" Help="Zugehörige Wetterstation" Min="" Max="" Default="" />'
!! WRITE(1, '(A)') '  <Parameter Ident="WLage" Text="Lage der Station" Unit="m üb. NN" Format="F7.1" Null="-1" Help="" Min="" Max="" Default="" />'
      type :: wetterstation
         integer :: wetterstations_nummer
         real :: wetterstations_lage
      end type wetterstation

!! <ParamSetDef Id="QD" Text="Dreissena" Help="Dreissena-Bewuchs in den Gewässer-Abschnitten" Scope="Abschnitt">
!!   <Parameter Ident="mboesch0" Text="Biomasse 0.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte (Schalenlänge kl. 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />
!!   <Parameter Ident="msohle0" Text="Biomasse 0.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />
!!   <Parameter Ident="gewicht0" Text="Mittl. Muschelgewicht 0.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 0. Kohorte" Min="" Max="" Default="" />
!!   <Parameter Ident="mboesch1" Text="Biomasse 1.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte (Schalenlänge gr.= 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />
!!   <Parameter Ident="msohle1" Text="Biomasse 1.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />
!!   <Parameter Ident="gewicht1" Text="Mittl. Muschelgewicht 1.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 1. Kohorte." Min="" Max="" Default="" />
      type :: dreissena
         real :: mboesch0,msohle0,gewicht0,mboesch1,msohle1,gewicht1
         integer :: dreissena_aktiv
      end type dreissena

!!  WRITE(1, '(A)') '<ParamSetDef Id="QB" Text="Benth.Algen" Help="Benth.Algen-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt"\n
!!  WRITE(1, '(A)') '  <Parameter Ident="GGruen" Text="Gewicht Grünalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Grünalgen \n
!!  WRITE(1, '(A)') '  <Parameter Ident="GKiesel" Text="Gewicht Kieselalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Kieselalgen \n
      type :: benth_al
         real :: ggruen,gkiesel
      end type benth_al

!! <ParamSetDef Id="QM" Text="Makrophyten" Help="Makrophyten-Wachstum" Scope="Strang">
!!   <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Wachstumsbeginns der Makrophyten" Min="1" Max="31" Default="" />
!!   <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Wachstumsbeginns der Makrophyten" Min="1" Max="12" Default="" />
!!   <Parameter Ident="MaxTag" Text="Max.-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="31" Default="" />
!!   <Parameter Ident="MaxMonat" Text="Max.-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="12" Default="" />
!!   <Parameter Ident="EndTag" Text="Ende-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Minimum erreicht hat. Hier endet das Makrophytenwachstum" Min="1" Max="31" Default="" />
!!   <Parameter Ident="EndMonat" Text="Ende-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Minimum erreicht hat" Min="1" Max="12" Default="" />
      type :: maphy
         integer :: starttag,startmonat,maxtag,maxmonat,endtag,endmonat
      end type maphy

!! <ParamSetDef Id="QP" Text="Dichte der Makrophyten" Help="Makrophyten-Dichte" Scope="Abschnitt">'
!!   <Parameter Ident="PflMin" Text="min. Dichte (Winter)" Unit="g/m²" Format="F7.2" Null="-1" Help="Minimale Dichte der Makrophyten im Winter" Min="" Max="" Default="" />
!!   <Parameter Ident="PflMax" Text="max. Dichte (Sommer)" Unit="g/m²" Format="F7.2" Null="-1" Help="Maximale Dichte der Makrophyten im Sommer" Min="" Max="" Default="" />
      type :: madi
!>    \anchor pflmin zone()%macrodicht%pflmin Minimale Dichte der Makrophyten im Winter von MODELLG.3D Zeile P gelesen
!>    \anchor pflmax zone()%macrodicht%pflmax Maximale Dichte der Makrophyten im Sommer von MODELLG.3D Zeile P gelesen
         real :: pflmin,pflmax
      end type madi

!!  WRITE(1, '(A)') '<ParamSetDef Id="QE" Text="Erosions-Parameter" Help="KenngrÃ¶ÃŸen fÃ¼r die GewÃ¤sserabschnitte" Scope="Abschnitt">'
!!  WRITE(1, '(A)') '  <Parameter Ident="tau_krit" Text="kritische Sohlschubspannung ab der Erosion auftritt"       Unit="N/mÂ²"      Format="F7.3" Null="-1" Help="" Max="" Default="9999.99" />'
!!  WRITE(1, '(A)') '  <Parameter Ident="M_eros"   Text="ErodibilitÃ¤tskonstante"                                    Unit="kg/(mÂ²*s)" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="0." />'
!!  WRITE(1, '(A)') '  <Parameter Ident="n_eros"   Text="Exponent in der Erosionsformel, potenziert den relativen SohlspannungsÃ¼berschuss" Unit="-" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="1." />'
!!  WRITE(1, '(A)') '  <Parameter Ident="sed_roh"  Text="Dichte des liegenden Sediments"                            Unit="kg/mÂ³"     Format="F7.3" Null="-1" Help="" Min="" Max="" Default="2650.0" />'
      type :: Erosion
!>    \anchor tau_krit zone()%erosi%tau_krit kritische Sohlschubspannung ab der Erosion auftritt in N/mÂ², von MODELLG.3D Zeile E gelesen
!>    \anchor M_eros zone()%erosi%M_eros ErodibilitÃ¤tskonstante in kg/(mÂ²*s) , von MODELLG.3D Zeile E gelesen
!>    \anchor n_eross zone()%erosi%n_eross Exponent in der Erosionsformel, potenziert den relativen SohlspannungsÃ¼berschuss , von MODELLG.3D Zeile E gelesen
!>    \anchor sed_roh zone()%erosi%sed_roh Dichte des liegenden Sediments in kg/mÂ³ , von MODELLG.3D Zeile E gelesen
         real ::tau_krit, M_eros, n_eros, sed_roh
      end type Erosion
	  
      type :: ddr
         character(200) :: zonen_name
         integer :: nr_zone, ini_randnr, zonen_nummer
         real  :: reib                           ! Reibungsbeiwert Sandrauheit nach Nikuradse in m
         type (sedimentfluss) :: sediflux        ! Z Sediment-Kenngrößen
         type (sedimenttemperatur) :: seditemp   ! S Kenngrössen für Temperatur/Sedimenttemperatur
         type (laichperiode) :: laich            ! L Laichperiode
         type (schiffsverkehr) :: schiff         ! F Schiffsverkehr
         type (dreissena) :: dreissen         ! D Dreissena
         type (maphy) :: macrophyt         ! M Makrophyten
         type (madi) :: macrodicht         ! P Dichte der Makrophyten
         !type () ::          ! C Corophium
         type (benth_al) ::  albenthi        ! B Benthische Algen
         type (wetterstation) :: wettstat         ! T Wetterstation
         !type () ::          ! O Anteil der Vegetationstypen
		 type (Erosion) :: erosi                 ! E Erosions-Parameter
      end type ddr
      type(ddr) , allocatable , dimension (:) :: zone

!-------------------------------------------------------------------------------wetter_datenfelder
!> Modell- und Ereignisname.
      character(100) VERSION_T, MODNAME_T, ERENAME_T
!> Anzahl der Wetterstationen
      integer :: iWETTs_T, mwettmax_T
!> Kennung 1=Stundenwerte 0=Tagesmittelwerte
      integer IMET_T
!> Stationsnummer, Stationskennung  ACHTUNG UMSTELLUNG
      integer , allocatable , dimension (:) :: iWSta_T, Wetterstationskennung_T
!> Anzahl der Wetterwerte
      integer , allocatable , dimension (:) :: mwetts_T
!> Datum des Wetterwertes
      integer , allocatable , dimension (:,:) :: itagw_T, monatw_T, jahrw_T
!> Uhrzeit des Wetterwertes in Sekunden
      real , allocatable , dimension (:,:) :: uhrzw_T
!> Wertefeld für den jeweiligen Zeitpunkt
!! 1: 2: 3: 4: 5: 6: 7:
      real , allocatable , dimension (:,:,:) :: wertw_T
!> Zeitpunkt des Wetterwertes
      integer , allocatable , dimension (:,:) :: zeitpunktw
!> Interploierte Wetterwerte am jeweiligen Berechnungszeitpunkt:
      real , allocatable , dimension (:) :: glob_T,  tlmax_T,  tlmin_T, tlmed_T
      real , allocatable , dimension (:) :: ro_T,  wge_T,  cloud_T,  typw_T
!> lokale Strahlung ?:
      real , allocatable , dimension (:) :: schwi_T
! Veraltet:
!      integer, parameter :: number_rb_wetter=6
!      real , allocatable , dimension (:) :: rb_wetter, rb_wetter_p

!-------------------------------------------------------------------------------transportinfo_datenfelder
! Beschreibung in stofftransport()
!> Flag für die Quelle des hydraulischen Antriebs: =1 Knotenbahnlinien aus casu (transinfo); =2 Elementrandflüsse aus Untrim² (netCDF)
      integer :: hydro_trieb
!> Pointer auf die netCDF Datei(en), aus der der hydraulische Antrieb gelesen wird (Untrim² und SCHISM)
      integer :: ncid
!> Flag für den Advektionsalgorithmus (noch unbenutzt, d.h. z.Z. casu lin. ELM,)
      integer :: advect_algo
!> Knotenanzahl als Kontrollwert zum Strömungsfeld.
      integer :: nonu
!> Felder für die 4 Eckknoten-nummern des Elements aus dem die Strombahn kommt, 
!! immer 4 hintereinander (ELM,semi-lagrange casu hydro_trieb=1 )
!! (Untrim, FV,Euler hydro_trieb=2) immer 
      integer , allocatable , dimension (:) :: intereck
!> und die 4 Wichtungsfaktoren, mit denen die Konzentrations-Werte von den interecken übertragen werden
!! immer 4 hintereinander (ELM, semi-lagrange, casu hydro_trieb=1)
!! (Untrim, FV,Euler hydro_trieb=2) immer 5 hintereinander, wobei der erste die eigene Konzentration ist und die 4 folgenden die aus den umliegenden Elementen
      real , allocatable , dimension (:) :: wicht
      real , allocatable , dimension (:) :: cu !! Courant-zahl bei Untrim
!> Feld der Zeitpunkte, an denen Transportinfo Dateien vorliegen
      integer , allocatable , dimension (:) :: transinfo_zeit
!> Feld Dateinamen der Transportinfo Dateien
      character(250) , allocatable , dimension (:) :: transinfo_datei
!> Zuordnungsfeld in welcher Reihenfolge die Transportinfo Dateien aufeinander folgen
      integer , allocatable , dimension (:) :: transinfo_zuord
!> (SCHISM) stack in which the timestep is stored
      integer , allocatable , dimension (:) :: transinfo_stack, transinfo_instack
      integer transinfo_anzahl, maxstack
      integer,allocatable :: ne_sc(:),np_sc(:),ns_sc(:) ! all numbers on process 0
      integer,allocatable :: ielg_sc(:,:),iplg_sc(:,:),islg_sc(:,:) ! global numbers all ranks on process 0

      integer :: nst_prev ! stack number of preveously read timestep
!> Anfang und Ende (Transportzähler) im Gütezeitschritt, Anzahl
      integer :: na_transinfo, ne_transinfo, anz_transinfo, n_trans
!> SCHISM netCDF output, number of stacks (output is subdivided in stacks, each containing only a part of the simulated time interval)
      integer ::n_stacks
!> \anchor dttrans timestep for transport simulation in sec.
      real :: dttrans
!> \anchor deltatrans timestep for transport simulation in whole sec. (integer)
      integer :: deltatrans !! in ganzen Sekunden
!> \anchor nub_sub_trans number of sub-steps in transport simulation 
      integer :: nub_sub_trans
!> Felder für Druck-p d.h. Wasserspiegellage, Gescheindigkeitsbetrag-u horizontal, Richtung-dir horizontal in Kompass-Grad, Vertikalgeschwindigkeit-w
!! werden im  /ref zuflussranddaten ??? übernommen und auf die Prozesse verteilt.
      real , allocatable , dimension (:) :: p, u, dir, w, vel_x, vel_y !! , tief
      real , allocatable , dimension (:) :: ur_x, ur_y, ur_z
!> Felder für Untrim, elemente/faces=Zellen 
      real , allocatable , dimension (:) :: el_vol, el_area
!> Felder für (untrim), edges
      double precision , allocatable , dimension (:) :: ed_vel_x, ed_vel_y, ed_flux, ed_area
!> annahme stationäres Strömungsfeld, nur eine transportinfo-datei verwenden.
      logical , parameter  :: stationaer=.false.
!> Einströmränder detektieren.
      logical , allocatable , dimension (:) :: inflow

!-------------------------------------------------------------------------------planktische_variablen_datenfelder
! Beschreibung in planktische_variablen.f95

!>    Anzahl der planktischen, transportierten, tiefengemittelten Variablen
!!    71 nur mit Leitfähigkeit !! 72 incl. salz !! 75 Alter(varianten) 76 TGzoo, 79 akmor_1,agmor_1,abmor_1
!!    101 mit Schwermetallen,
      integer, parameter :: number_plankt_vari=101
	  
!>    point-number ???
      integer :: number_plankt_point
!>    QSim-1D Namen, die zu den planktischen, transportierten, tiefengemittelten Variablen gehören.
      character(18) ::  planktonic_variable_name(number_plankt_vari) 
!>    Kennzeichnung, ob diese Variable ausgegeben werden soll. Siehe dazu ausgabekonzentrationen()
      logical ::  output_plankt(number_plankt_vari)
!>    globales (Prozess 0) Datenfeld für alle planktischen, transportierten, tiefengemittelten Variablen. \n
!!    Details in: \ref planktische_variablen
      real , allocatable , dimension (:) :: planktonic_variable
!>    lokales (parallel alle Prozesse) Datenfeld für alle planktischen, transportierten, tiefengemittelten Variablen. \n
!!    bei parallelen Rechnungen enthält es nur einen Teil des Datenfeldes modell::planktonic_variable das zu den Knoten gehört,
!!    die vom jeweiligen Prozess bearbeitet werden. \n
!!    Details in: \ref planktische_variablen siehe auch \ref Parallelisierung
      real , allocatable , dimension (:) :: planktonic_variable_p

!>    Number of vertically distributed planctonic, i.e. transported variables | depth-profiles 
      integer, parameter :: number_plankt_vari_vert=22
!>    Names and Descriptions of vertically distributed planktonic_variables
      character(18) :: plankt_vari_vert_name(number_plankt_vari_vert)
!>    output-flag
      logical ::  output_plankt_vert(number_plankt_vari_vert)
!>    Data array for all vertically distributed planctonic, transported variables. 
!!    Connection with QSim-variable-names through plankt_vari_vert_name
!!    to start with a fixed number of levels at overall same elevation
      integer, parameter :: num_lev=1 
      real :: z_plankt_lev(num_lev)
      real, allocatable , dimension (:) :: plankt_vari_vert, plankt_vari_vert_p     
                                      
!> \anchor point_zone Zonen-nummer am Berechnungspunkt (je nachdem ob an Elementzentren oder an Knoten gerechnet wird)
      integer , allocatable , dimension (:) :: point_zone

!-------------------------------------------------------------------------------uebergabe_werte_datenfelder
! Beschreibung in uebergabe_werte.f95

!     single (global) transfer values
!>    numbe of transfer values
      integer, parameter :: number_trans_val=10
!>    Names and Descriptions of transfer values
      character(18) ::  trans_val_name(number_trans_val)
!>    output-flag
      logical ::  output_trans_val(number_trans_val)
!>    data array for single (global) transfer values
      real :: transfer_value_p(number_trans_val)

!     depth averaged quantities
!>    points where transfer quantities are defined (all mesh points in general)
      integer :: number_trans_quant_points
!>    number of transfer quantities
      integer, parameter :: number_trans_quant=96
!>    Names and Descriptions of transfer quantities
      character(18) ::  trans_quant_name(number_trans_quant)
!>    output-flag
      logical  ::  output_trans_quant(number_trans_quant)
!>    globales (Prozess 0) Datenfeld für alle tiefengemittelten Variablen, die beim Stoffumsatz für den Informationsaustasch zwischen den einzelnen Modulen
!!    benötigt werden. \n
!!    Details in: \ref uebergabe_werte
      real , allocatable , dimension (:) :: transfer_quantity
!>    Lokales (parallel auf allen Prozessoren) Datenfeld für alle tiefengemittelten Variablen,
!!    die beim Stoffumsatz für den Informationsaustasch zwischen den einzelnen Modulen benötigt werden.  \n
!!    Bei parallelen Rechnungen enthält es nur einen Teil des Datenfeldes modell::transfer_quantity das zu den Knoten gehört,
!!    die vom jeweiligen Prozess bearbeitet werden. \n
!!    Details in: \ref uebergabe_werte siehe auch \ref Parallelisierung
      real , allocatable , dimension (:) :: transfer_quantity_p

!>    vertically distributed transfer quantities (depthprofiles)
!>    number of vertically distributed transfer quantities
      integer, parameter :: number_trans_quant_vert=28
!>    Names and Descriptions of transfer values
      character(18) :: trans_quant_vert_name(number_trans_quant_vert)
!>    output-flag
      logical  ::  output_trans_quant_vert(number_trans_quant_vert)

!>    Data array for all vertically distributed transfer quantities. 
      integer, parameter :: num_lev_trans=1
      real :: z_trans_lev(num_lev_trans)
      real, allocatable , dimension (:) :: trans_quant_vert, trans_quant_vert_p                                           
!>    creating an "objekt" for depth profiles with an individual depth discretisation (difficult in parallel)
!      type :: vert_point
!         real :: value(number_trans_quant_vert)                                           
!      end type vert_point
!      type :: depthprofile
!         integer :: num_lev
!         real , pointer :: z(:)
!         type(vert_point) , pointer :: level(:)                                       
!      end type depthprofile
!      type(depthprofile) , allocatable , dimension (:) :: trans_quant_vert

!-------------------------------------------------------------------------------benthische_verteilungen_datenfelder
! Beschreibung in benthische_verteilungen.f95
!>    points where benthic distributions are defined (all mesh points in general)
      integer :: number_benthic_points ! knotenanzahl_benth
!>    number of benthic distributions
      integer, parameter :: number_benth_distr=73 ! anzahl_benthvert
!>    Names and Descriptions of benthic_distributions
      character(18) ::  benth_distr_name(number_benth_distr) ! NameBenthischeVerteilung
!>    output-flag
      logical :: output_benth_distr(number_benth_distr) 
!>    globales (Prozess 0) Datenfeld für alle \ref benthische_verteilungen
      real , allocatable , dimension (:) :: benthic_distribution
      real , allocatable , dimension (:) :: benthic_distribution_p ! benthische_verteilung
!>    uedau überstaudauer-flag
      logical , parameter :: uedau_flag=.false.

      integer, parameter :: anzrawe=51  ! 28 ! max number of Boundary Concentrations
      integer :: n_active_concentrations ! used number of Boundary Concentrations
      integer :: ianz_rb, max_rand_nr
      type :: rb_zeile
         integer :: itag,imonat,ijahrl
         real :: uhrl, werts(anzrawe)
      end type rb_zeile
      type :: rb_punkt
         integer :: zeit_sek
         type(rb_zeile) zeil
      end type rb_punkt
      ! allocate (rabe(n)%punkt(i), stat = alloc_status )
      ! rabe(n)%anz_rb=i
      type :: rb_kante
         integer :: top, bottom, element, num
         real :: normal_x, normal_y, laengs ! Auswärts gerichteter Normalenvektor mit Kantenlänge !
      end type rb_kante
      type :: rb_streckenzug
         integer :: anzkanten, start_knoten, end_knoten
         type (rb_kante) , pointer :: kante(:)
         integer , allocatable , dimension (:) :: knoten !! alle Knotennummern in der Linie
      end type rb_streckenzug
!>    \anchor rb rb Randbedingungs-Struktur
      type :: rb
         integer :: anz_rb, nr_rb, tagesmittelwert_flag,t_guelt
         type (rb_punkt) , pointer :: punkt(:)
         real :: wert_jetzt(anzrawe)
         type (rb_streckenzug) :: randlinie
      end type rb
      type(rb) , allocatable , dimension (:) :: rabe

!> \page hydraul_rb Datenfelder offline Hydraulik-Randbedingungen
!! Die hydraulischen Randbedingungen werden als \ref Transportinformationen offline von holen_trans() eingelesen/berechnet.\n
!! Die QSim-3D Nummern beziehen sich auf das Datenfeld rb_hydraul resp. rb_hydraul_p\n
!! Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n 
!!<table rb_hydraul>
!!<tr><th> Nr. QSim-3D </th><th> Name QSim-1D 	</th><th> Beschreibung 						</th><th> Dimension	</th><th> Wertebereich</th></tr>
!!<tr><td> 1 </td><td> \anchor vmitt  vmitt 	</td><td> Geschwindigkeitsbetrag				</td><td> m/s		</td><td> 0,0 ... 3,0 </td></tr>
!!<tr><td> 2 </td><td> \anchor tiefe  tiefe 	</td><td> Wassertiefe						</td><td> m		</td><td> 0,0 ...  </td></tr>
!!<tr><td> 3 </td><td> \anchor wsp wsp  	</td><td> Wasserspiegellage					</td><td> m ü NHN	</td><td> </td></tr>
!!</table rb_hydraule>\n
!! \n aus module_modell.f95 , zurück: \ref Transportinformationen
      integer, parameter :: number_rb_hydraul=3
!> siehe: \ref hydraul_rb 
      real , allocatable , dimension (:) :: rb_hydraul
!> siehe: \ref hydraul_rb 
      real , allocatable , dimension (:) :: rb_hydraul_p

!> \page extnct_rb Extinktionskoeffizienten
!! Die Absorptionsspektren sigma(Lambda) fuer Wasser, Kiesel-,Gruen- und Blaualgen, Humin und susp. Schwebstoff \n
!! und das Spektrum des eingestrahlten Sonnenlichts\n
!! werden in der Datei <a href="./exp/e_extnct.dat" target="_blank">e_extnct.dat</a> angegeben;\n
!! von der Subroutine extnct_lesen() aus dem Modellverzeichnis gelesen 
!! und mithilfe des eindimensionalen Datenfeld: rb_extnct_p(n + (i-1)*anz_extnct_koeff ) gespeichert (i-Knotennummer, anz_extnct_koeff=8) 
!! und auf alle parallelen Prozesse kopiert (MPI_Bcast).
!! \n
!! <h2> Licht-Extinktions-Koeffizienten </h2> 
!! Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::rb_extnct_p\n 
!! Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n 
!!<table rb_extnct>
!!<tr><th> Nr. </th><th> Name		</th><th> Beschreibung </th><th> Dimension	</th></tr>
!!<tr><td> 1 </td><td> \anchor eta eta	</td><td> Wellenlänge (in nm in Luft)</td><td> </td></tr>
!!<tr><td> 2 </td><td> \anchor aw aw	</td><td> Extinktions-Spektrum Wasser	</td><td> ?? </td></tr>
!!<tr><td> 3 </td><td> \anchor ack ack	</td><td> Extinktions-Spektrum Kieselalgen	</td><td> </td></tr>
!!<tr><td> 4 </td><td> \anchor acg acg	</td><td> Extinktions-Spektrum Gruenalgen	</td><td> </td></tr>
!!<tr><td> 5 </td><td> \anchor acb acb	</td><td> Extinktions-Spektrum Blaualgen	</td><td> </td></tr>
!!<tr><td> 6 </td><td> \anchor ah ah	</td><td> Extinktions-Spektrum Humin-Stoffe	</td><td> </td></tr>
!!<tr><td> 7 </td><td> \anchor as as	</td><td> Extinktions-Spektrum susp. Schwebstoff	</td><td> </td></tr>
!!<tr><td> 8 </td><td> \anchor al al	</td><td> Sonnenlicht-Spektrum	</td><td> </td></tr>
!!<tr><td> 9 </td><td> \anchor extk_lamda extk_lamda	</td><td> Rückgabeparameter Gesamtextinktion </td><td> </td></tr>
!!</table>\n
!! im folgenden ist die aktuelle Vorgabe graphisch dargestellt:\n
!! \image html absorbtionsspectrum.png "Absorpions-Spektren" siehe: \ref Licht_algen \n
!! \n\n aus module_modell.f95 , zurück: \ref zuflussranddaten
      integer, parameter :: anz_extnct_koeff=8
!>  \anchor ilamda rb_extnct_ilamda Anzahl der Wellenlängen (siehe \ref extnct_rb). D. h. spektrale Auflösung des Lichts und dessen Extiktion im Wasser.
      integer :: rb_extnct_ilamda
      real , allocatable , dimension (:) :: rb_extnct_p

!-------------------------------------------------------------------------------schwebstoff_salz_datenfelder
! Beschreibung in schwebstoff_salz.f95

!> Feld der Zeitpunkte, an denen schwebstoff_salz Dateien vorliegen
      integer , allocatable , dimension (:) :: trueb_zeit, salz_zeit
!> Feld Dateinamen der schwebstoff_salz Dateien
      character (len=300) , allocatable , dimension (:) :: trueb_datei, salz_datei
!> Zuordnungsfeld in welcher Reihenfolge die schwebstoff_salz Dateien aufeinander folgen
      integer , allocatable , dimension (:) :: trueb_zuord, salz_zuord
      integer trueb_anzahl, salz_anzahl
      real , allocatable , dimension (:) :: vert1, vert2 ! nur auf Prozess 1 vorhanden

!-------------------------------------------------------------------------------Alter/Aufenthaltszeit
! Beschreibung in alter.f95
      logical :: nur_alter
      integer :: wie_altern ! 0-nix, 1-Zone, 2-Rand
      integer :: alter_nummer ! zonen oder randnummer für die alter berechnet wird
      real , allocatable , dimension (:,:) :: tr_integral_zone, vol_integral_zone, ent_integral_zone

!-------------------------------------------------------------------------------Querschnitte - cross-sections
! description in schnitt.f95 fluxes in array: schnittflux_gang
      logical :: querschneiden
      integer :: anzahl_quer
      type :: qusch
         integer ::  nr_qs
         type (rb_streckenzug) :: schnittlinie
      end type qusch
      type(qusch) , allocatable , dimension (:) :: querschnitt

!-------------------------------------------------------------------------------ausgabe_datenfelder
! Beschreibung in ausgabe.f95

!>    Daten-Feld für alle Feldgrößen,die in QSim nur zwischen den Modulen übergeben werden.
      real , allocatable , dimension (:,:) :: ausgabe_konzentration
!>    Knotenanzahl
      integer :: knotenanzahl_ausgabe
!>    Anzahl der insgesamt vorhandenen Übergabe-Konzentrationen. Z. Z. 1 
      integer :: anzahl_auskonz
!>    Namen der Transportkonzentrationen. für Ausgabe
      character(18) , allocatable , dimension (:) ::  AusgabeKonzentrationsName

!> Ausgabezeitpunkte
      integer :: n_ausgabe
      integer , allocatable , dimension (:) :: ausgabe_zeitpunkt, ausgabe_bahnlinie
      logical :: bali
!> output-concentrations
      integer :: k_ausgabe
      integer , allocatable , dimension (:) :: ausgabe_konz

!-------------------------------------------------------------------------------ganglinien_datenfelder
! Beschreibung in ganglinien.f95 ???

      integer :: anz_gangl, ionumber
      integer , allocatable , dimension (:) :: knot_gangl
      real , allocatable , dimension (:) :: c3Ammonium, IntAmmonium
      integer , allocatable , dimension (:,:) :: r_gang
      real , allocatable , dimension (:,:) :: t_gang, u_gang, tlmax_gang,  tlmin_gang
      real , allocatable , dimension (:,:,:) :: pl_gang, ue_gang, bn_gang
      real , allocatable , dimension (:,:,:) :: randflux_gang, schnittflux_gang
	  integer , allocatable , dimension (:) :: q_gangl
      integer :: n_pl, n_ue, n_bn
      integer, parameter :: gangl_level=1

!----------------------------------------------------------------------------------------------------------

      PUBLIC :: modeverz, zeile, zeitschritt_halb, ini_zeit, sekundenzeit, zeitsekunde, &
                modell_vollstaendig, strickler, lambda,  &
                antriebsart
!                points, netz_lesen, netz_gr3, 

!!  modell_parallel in parallel.f95

      CONTAINS
!----+-----+----
!> Die suboutine modeverz() setzt den Namen vom modellverzeichnis aus dem beim Aufruf angegebenen
!! Modellnamen, das ist ein Unterverzeichnis, das alle Modelldateien enthält,
!! und dem Modellordner, dessen Name in der Umgebungsvariablen TQM gespeichert ist, zusammen.
!! (export TQM=... | z. B. in .bashrc)
!! \n\n nur auf rang 0 !!\n\n
!! aus Datei module_modell.f95 ; zurück zu \ref Modellerstellung
      subroutine modeverz()
         implicit none
         character (len=longname) :: aufrufargument, systemaufruf, cmd
         integer io_error,sysa, icount, i, stat, length, errcode
         !integer antriebsart
         pi=4.0*atan(1.0)

         call get_command(cmd,length,stat)
         !print*, 'Aufruf:', cmd(1:length)
         icount= command_argument_count()
         !    ia=iargc()
         if((icount.lt.1).or.(icount.gt.2))  &
            call qerror('Auftruf QSim3D >qsim3d "Modellverzeichnis" (kontrollknoten optional)')
        call get_command_argument(1, aufrufargument)
         !print*,' Modell-Pfad ',trim(pfad),' Modell (Verzeichnis) ',trim(aufrufargument)
         !write(modellverzeichnis,'(3A)')trim(pfad),'/',trim(aufrufargument)!ohne TQM
         write(modellverzeichnis,'(A)')trim(aufrufargument)

         if(icount.eq.2)then ! kontrollknoten angegeben
            !call getarg(2,aufrufargument)
            call get_command_argument(2, aufrufargument)
            read(aufrufargument, * , iostat = io_error)kontrollknoten
            !print*,'get_command_argument(2 (kontrollknoten) #', kontrollknoten
            if (kontrollknoten .lt. 0)call qerror('modeverz: negative control node not permitted')
            print*,'control node, kontrollknoten #', kontrollknoten
          else! no controll node
            kontrollknoten=-1
            print*,'no control node, no extra output ', kontrollknoten
         end if ! kontrollknoten

         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),' >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modeverz writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         !print*,'sysa',sysa
         if(sysa.ne.0)then
            call qerror('Das Verzeichnis, welches das Modell enthalten sollte, existiert nicht.')
            !print*,'Umgebungsvariabel $TQM (Pfad des Modellordners)  >',trim(pfad)
            !print*,'export $TQM=... | in .bashrc ???'
            !print*,'Modell (Unter-Verzeichnis)  >',trim(aufrufargument)
         else
            print*,'QSim3D Modell: >', trim(modellverzeichnis)!!wird nacher in eingabe ausgegeben...
         end if ! io_error.ne.0
         strlaeng=len(trim(modellverzeichnis))
         !print*,'modellverzeichnis >', trim(modellverzeichnis), '<  len=',strlaeng

         !call getarg(2,aufrufargument)
         !read(aufrufargument, * , iostat = io_error)zeitschrittanzahl
         !print*,'zeitschrittanzahl=', zeitschrittanzahl

         ! Art des hydraulichen Antriebs ermitteln:
         hydro_trieb=antriebsart()

         return
      END subroutine modeverz
!----+-----+----

!> Unterscheidung welche den für den hydraulischen Antrieb verweendet werden.\\
!! hydro_trieb = Flag für die Quelle des hydraulischen Antriebs: =1 Knotenbahnlinien aus casu (transinfo); =2 Elementrandflüsse aus Untrim² (netCDF)\\
!! advect_algo = Flag für den Advektionsalgorithmus (noch unbenutzt, d.h. z.Z. casu lin. ELM,)
!! \n\n
      integer function antriebsart()
      implicit none
      character(len=longname) :: systemaufruf
      integer :: system_error, errcode

      antriebsart=0
      print*,'antriebsart: find hydraulic driver'

      !! falls casu-transinfo Directory vorhanden, das nehmen
      write(systemaufruf,'(4A)',iostat = errcode)'stat ',trim(modellverzeichnis),'transinfo',' >/dev/null 2>/dev/null'
      if(errcode .ne. 0)call qerror('antriebsart writing filename elemente_ failed')
      call system(trim(systemaufruf),system_error)
      if(system_error.ne.0) then
         print*,"antriebsart: casu (transinfo) not available; directory transinfo not accessible"
      else
         antriebsart=1
         print*,'antriebsart casu (transinfo)'
         return
      end if ! io_error.ne.0

      !! falls Antribsdatei transport.nc vorhanden, Untrim²-Resultate im NetCDF-Format verwenden
      write(systemaufruf,'(4A)',iostat = errcode)'stat ',trim(modellverzeichnis),'transport.nc',' >/dev/null 2>/dev/null'
      if(errcode .ne. 0)call qerror('antriebsart writing filename elemente_ failed')
      call system(trim(systemaufruf),system_error)
      if(system_error.ne.0) then
         print*,"antriebsart: untrim netDCF not available; file transport.nc not accessible"
      else
         antriebsart=2
         print*,'antriebsart untrim netDCF'
         return
      end if ! io_error.ne.0

      !! 
      write(systemaufruf,'(4A)',iostat = errcode)'stat ',trim(modellverzeichnis),'outputs_schism',' >/dev/null 2>/dev/null'
      if(errcode .ne. 0)call qerror('antriebsart writing filename elemente_ failed')
      call system(trim(systemaufruf),system_error)
      if(system_error.ne.0) then
         print*,"antriebsart: SCHISM netDCF not available; directory outputs_schism not accessible"
      else
         antriebsart=3
         print*,'antriebsart SCHISM netDCF'
         return
      end if ! io_error.ne.0

      END function antriebsart
!!----+-----+----
!> die Subroutine modella() ist Teil des module ::modell\n
!! Die <a href="./exp/MODELLA.txt" target="_blank">MODELLA.txt</a> Dateien für QSim-1D sind weiterverwendbar, aber \n
!! es werden in QSim-3D nur noch die Geologischen Breiten- und Längenkoordinaten daraus gelesen.
!! \n\n
!! aus Datei module_modell.f95 ; zurück zu \ref Modellerstellung
      SUBROUTINE modella()
      implicit none
      character (len=500) :: dateiname, text
      integer :: open_error, ion, read_error

      write(dateiname,'(2A)')trim(modellverzeichnis),'/MODELLA.txt'
      ion=103
      open ( unit =ion , file = dateiname, status ='old', action ='read ', iostat = open_error )
      if(open_error.ne.0) call qerror('open_error MODELLA.txt')
!
      if(.not.zeile(ion)) call qerror('modella Version nicht da')
      print*,'MODELLA.txt: Version=', trim(ctext)

      if(.not.zeile(ion)) call qerror('modellname lesen aus MODELLA.txt fehlgeschlagen')
      print*,'MODELLA.txt: modellname=', trim(ctext)
!
      if(.not.zeile(ion)) call qerror('modell_geob fehlt in MODELLA.txt')
      read(ctext, *, iostat = read_error) modell_geob, modell_geol
      if(read_error.ne.0)then
         write(fehler,*)' modell_geob lesen aus MODELLA.txt fehlgeschlagen ', read_error
         call qerror(fehler)
      else
         print*,'MODELLA.txt: Breitengrad, Längengrad =',  modell_geob, modell_geol
      end if 
!
      close (ion)
      END subroutine modella
!----+-----+----


!> Dient dem Einlesen der nächsten nicht-#-Kommentar Zeile \n\n 
!! \n\n
      logical function zeile(ion)
         implicit none
         integer :: io_error, ion
         zeile=.FALSE.
         do 
            read(ion, '(A)', iostat = io_error ) ctext 
            if(io_error.ne.0) then
               !!print*,'io_error SUBROUTINE zeile'
               zeile=.FALSE.
               return
            end if ! io_error.ne.0
            if (ctext(1:1).ne.'#') exit
         end do ! alle Zeilen
         zeile=.TRUE.
         RETURN
      END function zeile
!----+-----+----

!> Dient dem Erkennen, \n ob ctext (Variable in die function zeile einließt und übergibt) \n
!! eine Leerzeile ist \n 
!! \n\n
      logical function leerzeile()
         implicit none
         integer :: i
         leerzeile=.true.
         do i=1,len(trim(ctext))
            !print*,i,">",ctext(i:i)
            if(ctext(i:i).ne." ")leerzeile=.false.
         end do ! alle zeichen in ctext
         RETURN
      END function leerzeile
!----+-----+----

!> ließt bis zur nächsten zeile, die kein Kommentar und keine Leerzeile ist \n
!! Ergebnis in ctext \n 
!! \n\n
      logical function naechste_zeile(ion)
         implicit none
         integer :: ion

         do while( zeile(ion) ) !! nächste nicht Kommentar Zeile auffinden:
            if(.not.leerzeile()) then
               naechste_zeile=.true.
               RETURN
            end if ! keine Leerzeile
         end do ! nächste nicht Kommentar Zeile gefunden

         naechste_zeile=.false.
         RETURN
      END function naechste_zeile

!----+-----+----
!> Führt das Voranschreiten der Zeit aus.
!! \n\n
      subroutine zeitschritt_halb(vorher)
         logical :: vorher
         rechenzeit=rechenzeit+(deltat/2)
         uhrzeit_stunde=uhrzeit_stunde+(real((deltat/2))/3600.0)
         if(uhrzeit_stunde.ge.24.0)uhrzeit_stunde=uhrzeit_stunde-24.0
         zeitpunkt=rechenzeit
         !call zeitsekunde()
         if(vorher)then ! vor dem Zeitschritt
            startzeitpunkt=rechenzeit-(deltat/2)
            endzeitpunkt=startzeitpunkt+deltat 
         end if
      END subroutine zeitschritt_halb

!----+-----+----
!> Dient der initialisierung der Zeitsteuerung.
!! \n\n
      subroutine ini_zeit()
      if(meinrang.eq.0)then ! prozess 0 only
         !rechenzeit=252590400  ! 2008
         !rechenzeit=189432000  ! 2006
         rechenzeit=0
         !deltat=900 !3600
         deltat=3600
         uhrzeit_stunde=12.0
         izeit=0
         zeitschrittanzahl=1
      end if ! only prozessor 0
      END subroutine ini_zeit

!----+-----+----
!> Dient der Berechnung von zeitpunkt in ganzen sekunden aus \n
!! sekunde,minute,stunde Opt=1 \n
!! uhrzeit_stunde Opt=2 \n
!! tag, monat, jahr
!! Quelle: module_modell.f95 
!! \n\n
      subroutine sekundenzeit(opt)
         implicit none
         integer vormonatstage, jahrestage, jahre, maxjahre,opt
         logical schaltjahr
         real ustunde, uminute

         schaltjahr=.false.
         if(mod(jahr,4).eq.0)schaltjahr=.true.
         !write (*,*) tag, monat, jahr, schaltjahr

         jahre=jahr-referenzjahr
         if(jahre.gt. 25)call qerror('sekundenzeit: Berechnungsjahr zu lange nach Referenzjahr')
         if(jahre.lt. 0 )call qerror('sekundenzeit: Berechnungsjahr vor Referenzjahr')
         maxjahre=huge(zeitpunkt)/(365*86400)
         if(jahre.ge.(maxjahre-1))then
            write(fehler,*)'zeitpunkt in sec. nicht als integer angebbar, weil Referenzjahr zu klein'  &
               ,maxjahre,jahre,jahr,referenzjahr
            call qerror(fehler)
         end if
         jahrestage=int(jahre/4)*1461 ! Schaltjahperioden als ganzes abziehen
         jahre=jahre - (int(jahre/4)*4)
         if (jahre.gt.0)then ! Referenzjahr immer Schaltjahr !!!
            jahrestage=jahrestage+366
            jahre=jahre-1
         end if
         jahrestage=jahrestage+jahre*365 ! 0 ... 2 verbleibende jahre in Schaltjahperiode nach Schaltjahr

         select case (monat)
         case (1) !Januar
            vormonatstage=0
         case (2) !Februar
            vormonatstage=31
         case (3) !März
            vormonatstage=59
         case (4) !April
            vormonatstage=90
         case (5) !Mai
            vormonatstage=120
         case (6) !Juni
            vormonatstage=151
         case (7) !Juli
            vormonatstage=181
         case (8) !August
            vormonatstage=212
         case (9) !September
            vormonatstage=243
         case (10) !Oktober
            vormonatstage=273
         case (11) !November
            vormonatstage=304
         case (12) !Dezember
            vormonatstage=334
         case default
            print*,'Monate gibt es nur von 1 bis 12, Fehler in sekundenzeit'
            write(fehler,*)'tag=',tag,' monat=',monat,' jahr=',jahr,' Uhrzeit=',uhrzeit_stunde
            call qerror(fehler)
         end select
         if(schaltjahr.and.(monat.gt.2))vormonatstage=vormonatstage+1
         
         zeitpunkt=0

         zeitpunkt=zeitpunkt+(jahrestage*86400)
         zeitpunkt=zeitpunkt+(vormonatstage*86400)
         zeitpunkt=zeitpunkt+((tag-1)*86400) ! erster Tag beginnt bei 0 Sekunden !!!

         select case (opt)
         case (1) !stunde,minute,sekunde
            zeitpunkt=zeitpunkt+(stunde*3600)
            zeitpunkt=zeitpunkt+(minute*60)
            zeitpunkt=zeitpunkt+sekunde
         case (2) !stunde.minute
            !! zuerst Umwandeln in Stundendezimale
            ustunde=aint(uhrzeit_stunde)
            uminute=aint((uhrzeit_stunde-ustunde)*100.0)
            zeitpunkt=zeitpunkt+int(ustunde)*3600+int(uminute)*60
            !uhrzeit_stunde=aint(uhrzeit_stunde)+(uhrzeit_stunde-aint(uhrzeit_stunde))*(100.0/60.0) !! vormals minu_stund()
            !zeitpunkt=zeitpunkt+int(uhrzeit_stunde*3600)
         case default
            call qerror("Diese option der Zeitangabe ist in subroutine sekundenzeit nicht vorgesehen")
         end select

         zeitpunkt=zeitpunkt-time_offset

      END subroutine sekundenzeit
!! ausgabe.f95:            !call sekundenzeit()
!! ausgabe.f95:            call sekundenzeit()
!! module_modell.f95:      call sekundenzeit()
!! module_modell.f95:      call sekundenzeit()
!! randbedingungen.f95:               call sekundenzeit()
!! stofftransport.f95:                  call sekundenzeit()
! wetter.f95:            call sekundenzeit()
 
!----+-----+----
!> Dient der Rückrechnung von sekunde,minute,stunde,Tag,Monat,Jahr aus zeitpunkt in ganzen Sekunden seit Beginn Referenzjahr\n
!! Quelle: module_modell.f95 
!! \n\n
      subroutine zeitsekunde()
         implicit none
         integer schaltjahre, monatstage, tage
         logical schaltjahr

         uhrzeit_stunde=0.0
         monat=0
         tag=0
         zeitpunkt=zeitpunkt+time_offset

         tage=int(zeitpunkt/86400)
         if((tage.ge. 7*1461).or.(tage.lt. 0))then
            write(fehler,*)'zeitsekunde: zeitpunkt vor oder zu lang nach Referenzjahr| zeitpunkt,tage,referenzjahr=' &
               ,zeitpunkt,tage,referenzjahr
            call qerror(fehler)
         end if
         uhrzeit_stunde=real(zeitpunkt-(tage*86400))/3600.0
         stunde=(zeitpunkt-(tage*86400))/3600
         minute=(zeitpunkt-(tage*86400)-(stunde*3600))/60
         sekunde=(zeitpunkt-(tage*86400)-(stunde*3600)-(minute*60))

         schaltjahre=tage/1461 !(3*365+366)
         tage=tage-(schaltjahre*1461)
         jahr=schaltjahre*4
         if(tage.ge.366)then ! 4-Jahresperode beginnt mit schaltjahr
            jahr=jahr+1
            tage=tage-366
            do while (tage.ge.365) ! weitere Jahre
               jahr=jahr+1
               tage=tage-365
            end do !while
         end if
         jahr=jahr+referenzjahr
         schaltjahr=.false.
         if( mod(jahr,4) .eq. 0 )then
            schaltjahr=.true.
            !print*,'zeitsekunde: schaltjahr'
         end if
         !print*,'zeitsekunde: tage, jahr, schaltjahre,zeitpunkt,zeitpunkt-time_offset,mod(jahr,4)',  &
         !          tage, jahr, schaltjahre,zeitpunkt,zeitpunkt-time_offset,mod(jahr,4)
         tag=tage+1 ! erster tag nach 0 tagen im Jahr
         tagdesjahres=tag ! für Wetter und Licht
         monat=1 ! im Januar (oder später im Jahr)
         monatstage=31
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im Februar (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=28
         if(schaltjahr)monatstage=29
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im März (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=31
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im april (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=30
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im mai (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=31
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im juni (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=30
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im juli (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=31
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im august (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=31
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im september (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=30
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im oktober (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=31
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im november (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=30
         if(tag.le.monatstage) goto 111
         if(tag.gt.monatstage)then ! im dezember (oder später im Jahr)
            monat=monat+1
            tag=tag-monatstage
         end if
         monatstage=31
         if(tag.gt.monatstage)then ! Jahr rum
            write(fehler,*)'Jahr rum: zeitpunkt,jahr,monat,tag,stunde,minute,sekunde,referenzjahr,time_offset=',  &
     &                      zeitpunkt,jahr,monat,tag,stunde,minute,sekunde,referenzjahr,time_offset
            call qerror(fehler)
         end if
  111    continue

         zeitpunkt=zeitpunkt-time_offset

      END subroutine zeitsekunde

!-----+-----+-----+-----+
!> Wenn die Nachkommastelle in lesezeit die Minuten angibt, in dezimaldarstellung rückrechnen
!! \n\n
      real function minu_stund(lesezeit)
      implicit none
      real lesezeit

         !!minu_stund=(10/6)*lesezeit-(4/6)*aint(lesezeit)
         !!minu_stund=aint(lesezeit)
         minu_stund=aint(lesezeit)+(lesezeit-aint(lesezeit))*(100.0/60.0)

      END function minu_stund
!----+-----+----

      logical function modell_vollstaendig()
      implicit none
         character (len=longname) :: aufrufargument,systemaufruf, emaildatei
         integer io_error,sysa, ia, ion, errcode
         logical zeile_vorhanden

         modell_vollstaendig=.true.

         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'EREIGG.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei EREIGG.txt'
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'MODELLA.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei MODELLA.txt'
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'WETTER.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei WETTER.txt'
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'MODELLG.3D.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei MODELLG.3D.txt'
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'APARAM.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei APARAM.txt'
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'e_extnct.dat >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei e_extnct.dat'
         end if
!
      select case (hydro_trieb)
      case(1) ! casu-transinfo                                           
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'transinfo/meta >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei /transinfo/meta'
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'transinfo/points >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei transinfo/points'
            !print*,'sysa=',sysa,' systemaufruf=',trim(systemaufruf)
         !else
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'transinfo/file.elements >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei transinfo/file.elements'
         end if

      case(2) ! Untrim² netCDF
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'transport.nc >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei transport.nc'
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'ELEMENTE.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei ELEMENTE.txt'
         end if

      case(3) ! SCHISM netCDF
         print*,'not yet checking completeness of SCHISM model'

      case default
         call qerror('Hydraulischer Antrieb unbekannt')
      end select
!
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'ganglinien_knoten.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei ganglinien_knoten.txt'
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'ausgabezeitpunkte.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei ausgabezeitpunkte.txt'
         end if
         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis)  &
     &        ,'ausgabekonzentrationen.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            modell_vollstaendig=.false.
            print*,'In Ihrem Modellverzeichnis fehlt die Datei ausgabekonzentrationen.txt'
         end if

         send_email=.False.
         write(emaildatei,'(2A)')trim(modellverzeichnis),'email.txt'
         ion=101
         open ( unit =ion , file = emaildatei, status ='old', action ='read ', iostat = sysa )
         if(sysa.ne.0) then
            print*,'ohne Datei email.txt keine Benachrichtigung'
         else
            do while( zeile(ion))
               if (ctext(1:1).ne.'#') then !! keine Kommentarzeile
                  write(email,'(A)')trim(adjustl(ctext))
                  print*,'Über das Programmende werden Sie unter der Emailadresse ',trim(email),' benachrichtigt.'
                  send_email=.True.
               end if ! keine Kommentarzeile
            end do ! alle Zeilen
            if(.not.send_email)print*,'nix brauchbares aus Datei email.txt gelesen'
         end if ! Datei lässt sich öffnen

         write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'alter.txt >/dev/null 2>/dev/null'
         if(errcode .ne. 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
         call system(systemaufruf,sysa)
         if(sysa.ne.0) then
            print*,'Datei alter.txt nicht vorhanden, ergo: vollständige Gütesimulation'
            nur_alter=.false.
         else
            print*,'##### Datei alter.txt vorhanden, nur Aufenthaltszeit-Simulation, ggf incl. Temp. #####'
            nur_alter=.true.
         end if

      END function modell_vollstaendig

!----+-----+----
!> Reibungsbeiwert \f$ \lambda \f$ aus Wassertiefe/Sohlabstand und Rauheitshöhe gemäß dem Colebrook-White Gesetz berechnen (nach DVWK 220).\n
!! \f[ 
!! \lambda = \frac{8}{\left[ (1.0/ \kappa)*(log10(h/ks)) + 8.5 \right]^2}
!! \f]
!! mit:\n
!! \f$ \kappa \f$  - von Karman Konstante ca. 0,4 \n
!! \f$ h \f$ - Wassertiefe und \n
!! \f$ ks \f$ - äquivalente Sandrauheit nach Nikuradse
!!
!! \n\n aus module_modell.f95
real  function lambda(ks,zet)
      implicit none
      real :: ks,zet,l
      real , parameter :: kappa=0.4 ! vomKarman-Konstante

      lambda = 10000.0 ! default

      if ((zet.gt. 0.0).and.(ks.gt. 0.0)) then
         l=(1.0/kappa)*(log10(zet/ks)) + 8.5
         if (l.gt. 0.0)then
            lambda = 8/(l**2)
            !!logg<<"level too close to bottom for wall function; zet="
            !!    <<zet<<" zone #"<<ddr<<" fric_par="<<mesh.zone[ddr].fric_par[0]<<" l="<<l<<endl;
            !!cout<<"wall function not applicable, lambda negative, wall distance propably too small"<<endl;
            !!l=-777.77;
            !!l=1/tolerance; //no distance/depth no slip
         endif
      endif

      RETURN
      END function lambda

!----+-----+----
!> DerGauckler-Manning-Strickler Reibungs-Beiwert wird aus der Sandrauheit und der Wassertiefe berechnet:\n
!! \f[ 
!! K_{st}=\sqrt{\frac{8 \cdot g}{ \lambda \cdot h^{1/3}}}
!! \f]
!! mit:\n
!! \f$ \lambda \f$  siehe: modell::lambda() \n
!! \f$ h \f$ - Wassertiefe und \n
!! \f$ g \f$ - Gravitation=Erdbeschleunigung.
!! \n\n aus module_modell.f95
real  function strickler(ks,zet)
      implicit none
      real :: ks,zet

      if(zet.gt. 0.0)then
         strickler = (lambda(ks,zet)*(zet**0.33333))
         if(strickler.gt. 0.0)then
            strickler = (8*grav)/strickler
            strickler = (strickler**0.5)
         end if
      else ! trocken
         strickler=1.0
      endif

      RETURN
      END function strickler
!-----+-----+-----+-----+

      end module modell
