! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !

!> Das module ::modell
!! speichert die Information zum
!! <ul>
!!    <li> Modellverzeichnis </li>
!!    <li> Netz </li>
!!    <li> Zeit </li>
!!    <li> Vermaschung </li>
!! </ul>
!! und wird mehr und mehr zum zentralen common-block. \n
!! Es inkludiert bereits die Definitionen der
!! <ul>
!!    <li> \ref lnk_var_planktisch </li>
!!    <li> \ref lnk_ergebnisausgabe </li>
!! </ul>
!! \n\n
!! aus Datei module_modell.f95
module modell
   use iso_fortran_env
   implicit none
   
   public :: modeverz, zeile, zeitschritt_halb, ini_zeit, &
             modell_vollstaendig, strickler, lambda, antriebsart
   
   
   include 'mpif.h' !!/mreferate/wyrwa/casulli/mpich2/mpich2-1.3.2p1/src/include/mpif.h: integer*8 und real*8 raus
                    ! die in mpif.h enthaltenen SAVE statements sind dann
                    ! doppelt definiert und führen zu einer Warning, die aber ignoriert werden kann.
   
   real, parameter :: grav = 9.81
   
   ! --------------------------------------------------------------------------
   ! parallel_datenfelder
   ! --------------------------------------------------------------------------
   ! Beschreibung in parallel.f95
   integer :: meinrang, part, proz_anz, ierr !> nummer und Gesamtzahl prozessoren (MPI)
   integer :: mpi_komm_welt
  
   !---------------------------------------------------------------------------
   ! model and mesh
   !---------------------------------------------------------------------------
   !> modellverzeichnis etc.
   integer, parameter :: longname = 3000
   
   character(longname) :: pfad, modellverzeichnis, codesource, fehler
   integer             :: kontrollknoten !> \anchor kontrollknoten Nummer des Kontrollknotens (Untrim-Elementnummer)
   logical             :: control        !> \anchor control Schalter ob Kontro llausgabe aus Stoffumsatzroutinen heraus erfolgen soll
   integer             :: iglob          !> \anchor iglob globale Knotennummer, Übergabe an Stoffumsatzroutinen aus parallel laufenden Hüllroutinen
   
   ! --- simulation settings ---
   integer :: imitt       !< \anchor imitt Tagesmittelwertausgabe=1 Zeitwerte=0 | nicht aktiv in 3D
   integer :: ipH         !< \anchor iph Schalter für die ph-Wert-berechnung; 1-ein , 0-aus ; wird von read_ereigg_model() eingelesen.
   integer :: idl         !< \anchor idl Dispersionskoeffizienten einlesen=0 berechnen=1 in QSim3D bisher unbenutzt
   integer :: itemp       !< \anchor itemp nur Temperatursimulation=1 alles=0 in QSim3D bisher unbenutzt
   integer :: itracer     !< \anchor itracer nur Tracersimulation=1 alles=0 in QSim3D bisher unbenutzt
   integer :: ieros       !< \anchor ieros Erosions-flag mit Erosion=1 ohne=0 in QSim3D bisher unbenutzt
                          !! #FG: temporary dirty hack: iEros<0 for reading 'SS' from UnTRIM2; 'abs(iEros)' sets the number of classes to be read (finest to coarsest)
   integer :: ischwa      !< \anchor ischwa mit ereigg2=1 ohne=0 ??? in QSim3D bisher unbenutzt
   integer :: iverfahren  !< \anchor iverfahren Transportverfahren in QSim3D bisher unbenutzt
   integer :: ilongDis    !< \anchor ilongdis ??? in QSim3D bisher unbenutzt ilongDis
   integer :: iColi       !< \anchor icoli ??? in QSim3D bisher unbenutzt
   integer :: ikonsS      !< \anchor ikonss ??? in QSim3D bisher unbenutzt
   integer :: iSchwer     !< \anchor ischwer ??? in QSim3D bisher unbenutzt
   integer :: iphy        !< \anchor iphy Berechnungsoption für Oberflächenbelüftung in oxygen.f90:\n
                          !!         iphy = 1       ! neue Formel von mir mit Wind\n
                          !!         iphy = 2       ! neue Formel von mir ohne Wind\n
                          !!         iphy = 3       ! Formel von Wolf *Formelfehler* k2=10.47*v^0.43*H^-1.37*S^0.22+K2wind (Dantengrundlage Wolf 1974)" Help="Berechnung nach Wolf (überarbeitete Form)" />'\n
                          !!         iphy = 4       ! Formel von Melching\n
   integer :: iformVert   !< \anchor iformVert Verteilungsfunktion Schwermetalle 1-DWA-Modell 2-Deltares 2010
   integer :: iform_verdr !< \anchor iform_verdr:   Schalter für die Auswahl der Verdunstungsformeln in water_temperature.f90
   integer :: iwsim       !< \anchor iwsim Kennung, Simulationstyp ?
   real    :: FlongDis    !< \anchor flongdis Faktor Dispersionskoeffizient, Eingabe über EREIGG.txt ??? in QSim3D bisher unbenutzt FlongDis
   logical :: nur_temp    !> Steuerung des Stoffumsatzes eingelesen über itemp
    
   
   ! --- mesh variables ---
   integer                            :: min_rand, max_rand, min_zone, max_zone
   integer                            :: knotenanzahl2D, knotenanzahl3D
   integer, allocatable, dimension(:) :: knoten_rand
   integer, allocatable, dimension(:) :: knoten_zone, knoten_rang !< \anchor knoten_zone Zonen-zähler an den Knoten bei Antrieb mit casu-Hydraulik aus points
   integer, allocatable, dimension(:) :: knot_ele                 !< Anzahl der Elemente an einem Knoten (nur untrim)
   real                               :: modell_geob, modell_geol, modell_flaeche
   real                               :: mittelflaech, mittelvolumen
   real,    allocatable, dimension(:) :: knoten_flaeche, knoten_volumen
   real,    allocatable, dimension(:) :: knoten_x, knoten_y, knoten_z
   
   character (len = 2000) :: ctext 
   
   ! --- time variables ---
   ! TODO (Schönung, August 2023)
   ! This should be user definable.
   real, parameter :: tz_qsim = 1.0        !> \anchor tz_qsim standard timezone that QSim uses in its input files [UTC + 1]
   
   integer(int64) :: rechenzeit            !> \anchor rechenzeit current time of running simulation in unixtime
   integer(int64) :: startzeitpunkt        !> \anchor startzeitpunkt start time of current timestep in unixtime
   integer(int64) :: endzeitpunkt          !> \anchor endzeitpunkt end time of current timestep in unixtime
   integer        :: deltat                !> \anchor deltat Zeitschrittweite (Stoffumsatz) in ganzen Sekunden
   integer        :: zeitschrittanzahl     !> \anchor zeitschrittanzahl Zeitschrittanzahl die von der Berechnung (Ereignis) durchlaufen werden.
   integer        :: izeit                 !> \anchor izeit izeit Zeitschrittzähler
   integer        :: anzZeit               !> \anchor anzZeit anzZeit Anzahl der erosionslosen Zeitschritte im bisherigen Rechenlauf ; zurÃ¼ck: \ref lnk_schwermetalle
   
   ! --- Vermaschung Elemente ---
   logical                              :: element_vorhanden
   integer                              :: n_elemente, summ_ne
   integer, allocatable, dimension(:)   :: cornernumber
   integer, allocatable, dimension(:)   :: element_rand ! aus Mesh2_face_bc, netCDF
   integer, allocatable, dimension(:)   :: element_zone !> \anchor element_zone Element zonen, null gesetzt, wird bei Antrieb mit untrim-Hydraulik aus netCDF-Dateien verwendet
   integer, allocatable, dimension(:,:) :: elementnodes, elementedges
   real,    allocatable, dimension(:)   :: element_x, element_y
   
   ! --- Vermaschung Kanten ---
   logical                              :: kanten_vorhanden
   integer                              :: kantenanzahl
   integer, allocatable, dimension(:)   :: top_node, bottom_node, left_element, right_element, boundary_number, zon_num
   real,    allocatable, dimension(:)   :: edge_normal_x, edge_normal_y
   real,    allocatable, dimension(:)   :: edge_ground, cell_bound_length
   real,    allocatable, dimension(:)   :: edge_mid_x, edge_mid_y ! mid-side location of edges
   
   real,parameter :: min_tief = 0.01 !> toleranzen und clipping-Werte
   
   
   !---------------------------------------------------------------------------
   ! zonen_datenfelder
   !---------------------------------------------------------------------------
   !> Beschreibung in zonen.f95
   integer :: zonen_anzahl !> \anchor wetterstations_nummer enthält zu jedem Zonenzähler den dazugehörigen
                           !!  Wetterstationszähler, der in eingabe() aus Wetterstationskennung_T bestimmt wird
   
   ! Sed-Flux variablen
   type :: sedimentfluss
      real :: sedom    !> \anchor sedom sedom Anteil des organischen Materials im Sediment von modellg.3D.txt, POMz -> hsedom
      real :: bedgs    !> \anchor bedgs bedgs Bedeckungsgrad der Sohle mit Sediment (0-1), von modellg.3D.txt, BedGSz -> hbedgs
      real :: sedvvert !> \anchor sedvvert sedvvert volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h, von modellg.3D.txt, Sedvvertz -> hsedvvert
      real :: kornd    !> \anchor kornd kornd Vorgabe Korndurchmesser d50 sediment in der Zone ggf.
      real :: burial   !> \anchor burial burial Burial-Geschwindigkeit (Sedimentation) Vorgabe in der Zone ggf.
   end type sedimentfluss
   
   ! Sediment-Temperatuf variablen
   type :: sedimenttemperatur
      real :: spewks !> \anchor spewks spewks Spez. WärmeKapazität Sediment" unit="KJ/(kg*K)
      real :: wuebk  !> \anchor wuebk wuebk Wärmeübergangskoeffizient" unit="KJ/(K*m2*h)
      real :: psrefs !> \anchor psrefs psrefs Reflektionsanteil der Strahlung an der Sedimentoberfläche
      real :: extiks !> \anchor extiks extiks Extinktionskoeffizient für PARS (nur bei Temperaturmodellierung erforderlich!) zonenweise, siehe \ref extks
   end type sedimenttemperatur
   
   ! Dreissena Laichperiode variablen
   type :: laichperiode
      integer :: lait !> \anchor lait lait Dreissena Laichperiode: Tag des Beginns der Laichperiode 
      integer :: laim !> \anchor laim laim Dreissena Laichperiode: Monat des Beginns der Laichperiode
      integer :: laid !> \anchor laid laid Dreissena Laichperiode: Dauer der Laichperiode in Tagen
   end type laichperiode
   
   
   type :: schiffsverkehr
      real    :: vschiff
      real    :: uprop
      integer :: schifffahrts_zone !> \anchor schifffahrts_zone schifffahrts_zone Schiffsverkehr, von modellg.3D.txt F-Zeile schifffahrts_zone -> mss
   end type schiffsverkehr
   

   type :: wetterstation
      integer :: wetterstations_nummer 
      real    :: wetterstations_lage
   end type wetterstation

   type :: dreissena
      real    :: mboesch0
      real    :: msohle0
      real    :: gewicht0
      real    :: mboesch1
      real    :: msohle1
      real    :: gewicht1
      integer :: dreissena_aktiv ! TODO: This should be of type logical
   end type dreissena
   
   type :: benth_al
      real :: ggruen
      real :: gkiesel
   end type benth_al
   
   type :: maphy
      integer :: starttag,startmonat,maxtag,maxmonat,endtag,endmonat
   end type maphy
   
   type :: madi
      real :: pflmin !> \anchor pflmin zone()%macrodicht%pflmin Minimale Dichte der Makrophyten im Winter
      real :: pflmax !> \anchor pflmax zone()%macrodicht%pflmax Maximale Dichte der Makrophyten im Sommer
   end type madi
   
   type :: tErosion
      real :: tau_krit !> \anchor tau_krit zone()%erosi%tau_krit kritische Sohlschubspannung ab der Erosion auftritt in N/m2
      real :: m_eros   !> \anchor M_eros zone()%erosi%M_eros Erodibilitätskonstante in kg/(m2*s)
      real :: n_eros   !> \anchor n_eross zone()%erosi%n_eross Exponent in der Erosionsformel, potenziert den relativen Sohlspannungsüberschuss
      real :: sed_roh  !> \anchor sed_roh zone()%erosi%sed_roh Dichte des liegenden Sediments in kg/m3
   end type tErosion
   
   type :: ddr
      character(200)           :: zonen_name
      integer                  :: nr_zone
      integer                  :: ini_randnr
      integer                  :: zonen_nummer
      real                     :: reib       ! Reibungsbeiwert Sandrauheit nach Nikuradse in m
      type(sedimentfluss)      :: sediflux   ! Z Sediment-Kenngrößen
      type(sedimenttemperatur) :: seditemp   ! S Kenngrössen für Temperatur/Sedimenttemperatur
      type(laichperiode)       :: laich      ! L Laichperiode
      type(schiffsverkehr)     :: schiff     ! F Schiffsverkehr
      type(dreissena)          :: dreissen   ! D Dreissena
      type(maphy)              :: macrophyt  ! M Makrophyten
      type(madi)               :: macrodicht ! P Dichte der Makrophyten
      type(benth_al)           :: albenthi   ! B Benthische Algen
      type(wetterstation)      :: wettstat   ! T Wetterstation
      type(tErosion)           :: erosi      ! E Erosions-Parameter
   end type ddr
   
   type(ddr), allocatable, dimension(:) :: zone
   
   ! --------------------------------------------------------------------------
   ! wetter_datenfelder
   ! --------------------------------------------------------------------------
   integer                                       :: iwetts_t, mwettmax_t              !< Anzahl der Wetterstationen
   integer                                       :: imet_t                            !< Kennung 1=Stundenwerte 0=Tagesmittelwerte
   integer,        allocatable, dimension(:)     :: iwsta_t, wetterstationskennung_T  !< Stationsnummer, Stationskennung  ACHTUNG UMSTELLUNG
   integer,        allocatable, dimension(:)     :: mwetts_t                          !< Anzahl der Wetterwerte
   integer,        allocatable, dimension(:,:)   :: itagw_T, monatw_T, jahrw_T        !< Datum des Wetterwertes
   integer(int64), allocatable, dimension(:,:)   :: zeitpunktw                        !< Zeitpunkt des Wetterwertes
   real,           allocatable, dimension(:)     :: glob_T, tlmax_T, tlmin_T, tlmed_T !< Interploierte Wetterwerte am jeweiligen Berechnungszeitpunkt:
   real,           allocatable, dimension(:)     :: ro_T, wge_T, cloud_T,  wtyp_T
   real,           allocatable, dimension(:)     :: schwi_T                           !< Globalstrahlung
   real,           allocatable, dimension(:,:)   :: uhrzw_T                           !< Uhrzeit des Wetterwertes in Sekunden
   real,           allocatable, dimension(:,:,:) :: wertw_T                           !< Wertefeld für den jeweiligen Zeitpunkt
   
   
   ! --------------------------------------------------------------------------
   ! transportinfo_datenfelder
   ! --------------------------------------------------------------------------
   ! Beschreibung in stofftransport()
  
   integer                                     :: hydro_trieb                !< Flag für die Quelle des hydraulischen Antriebs: =1 Knotenbahnlinien aus casu (transinfo); =2 Elementrandflüsse aus Untrim² (netCDF)
   integer                                     :: ncid                       !< Pointer auf die netCDF Datei(en), aus der der hydraulische Antrieb gelesen wird (Untrim² und SCHISM)
   integer                                     :: advect_algo                !< Flag für den Advektionsalgorithmus (noch unbenutzt, d.h. z.Z. casu lin. ELM,)
   integer                                     :: nonu                       !< Knotenanzahl als Kontrollwert zum Strömungsfeld.
   integer                                     :: transinfo_anzahl, maxstack
   integer                                     :: na_transinfo, ne_transinfo !< Anfang und Ende (Transportzähler) im Gütezeitschritt, Anzahl
   integer                                     :: anz_transinfo, n_trans     !< Anfang und Ende (Transportzähler) im Gütezeitschritt, Anzahl
   integer                                     :: deltatrans                 !< \anchor deltatrans timestep for transport simulation in whole sec. (integer)
   integer                                     :: nub_sub_trans              !< \anchor nub_sub_trans number of sub-steps in transport simulation
   integer                                     :: nst_prev                   !< stack number of preveously read timestep
   integer                                     :: n_stacks                   !< SCHISM netCDF output, number of stacks (output is subdivided in stacks, each containing only a part of the simulated time interval)
   integer,        allocatable, dimension(:,:) :: ielg_sc, iplg_sc, islg_sc  !< global numbers all ranks on process 0
   integer,        allocatable, dimension(:)   :: transinfo_stack            !< (SCHISM) stack in which the timestep is stored
   integer,        allocatable, dimension(:)   :: transinfo_instack          !v (SCHISM) stack in which the timestep is stored
   integer,        allocatable, dimension(:)   :: transinfo_zuord            !< Zuordnungsfeld in welcher Reihenfolge die Transportinfo Dateien aufeinander folgen
   integer(int64), allocatable, dimension(:)   :: transinfo_zeit             !< timestamps of transport file in unixtime
   integer,        allocatable, dimension(:)   :: ne_sc, np_sc, ns_sc        ! all numbers on process 0
   integer,        allocatable, dimension(:)   :: intereck                   !< Felder für die 4 Eckknoten-Nummern des Elements aus dem die Strombahn kommt,
                                                                             !! immer 4 hintereinander (ELM,semi-lagrange casu hydro_trieb=1 )
                                                                             !! (Untrim, FV,Euler hydro_trieb=2) immer
                                                                             
   real                            :: dttrans                    !> \anchor dttrans timestep for transport simulation in sec.
   real, allocatable, dimension(:) :: cu                         ! Courant-zahl bei Untrim
   real, allocatable, dimension(:) :: p, u, dir, w, vel_x, vel_y !> Felder für Druck-p d.h. Wasserspiegellage, Gescheindigkeitsbetrag-u horizontal, Richtung-dir horizontal in Kompass-Grad, Vertikalgeschwindigkeit-w
   real, allocatable, dimension(:) :: ur_x, ur_y, ur_z
   real, allocatable, dimension(:) :: el_vol, el_area            !> Felder für Untrim, elemente/faces=Zellen
   real, allocatable, dimension(:) :: wicht                      !> und die 4 Wichtungsfaktoren, mit denen die Konzentrations-Werte von den interecken übertragen werden
                                                                 !! immer 4 hintereinander (ELM, semi-lagrange, casu hydro_trieb=1)
                                                                 !! (Untrim, FV,Euler hydro_trieb=2) immer 5 hintereinander, wobei der erste die eigene Konzentration ist und die 4 folgenden die aus den umliegenden Elementen
                                                                 !! werden im  /ref zuflussranddaten ??? übernommen und auf die Prozesse verteilt.
   double precision, allocatable, dimension(:) :: ed_vel_x, ed_vel_y, ed_flux, ed_area !< Felder für (untrim), edges
   logical,          allocatable, dimension(:) :: inflow                               !< Einströmränder detektieren.
   character(250),   allocatable, dimension(:) :: transinfo_datei                      !< Feld Dateinamen der Transportinfo Dateien
   
   logical, parameter :: stationaer = .false.  !> Annahme stationäres Strömungsfeld, nur eine transportinfo-datei verwenden.
   
   ! --------------------------------------------------------------------------
   ! planktische_variablen_datenfelder
   ! -------------------------------------------------------------------------- 
   ! Beschreibung in planktische_variablen.f95
   
   !> Anzahl der planktischen, transportierten, tiefengemittelten Variablen
   !! 71 nur mit Leitfähigkeit !! 72 incl. salz !! 75 Alter(varianten) 76 TGzoo, 79 akmor_1,agmor_1,abmor_1
   !! 101 mit Schwermetallen,
   integer, parameter :: number_plankt_vari = 101
   integer, parameter :: number_plankt_vari_vert = 22 !< Number of vertically distributed planctonic, i.e. transported variables | depth-profiles
   integer, parameter :: num_lev = 1
   
   integer                                      :: number_plankt_point !<   point-number
   character(18), dimension(number_plankt_vari) ::  planktonic_variable_name !< QSim-1D Namen, die zu den planktischen, transportierten, tiefengemittelten Variablen gehören.
   logical,       dimension(number_plankt_vari) ::  output_plankt            !< Kennzeichnung, ob diese Variable ausgegeben werden soll. Siehe dazu ausgabekonzentrationen()
   real,    allocatable , dimension (:)         :: planktonic_variable       !< globales (Prozess 0) Datenfeld für alle planktischen, transportierten, tiefengemittelten Variablen. 
   
   !>    lokales (parallel alle Prozesse) Datenfeld für alle planktischen, transportierten, tiefengemittelten Variablen. \n
   !!    bei parallelen Rechnungen enthält es nur einen Teil des Datenfeldes modell::planktonic_variable das zu den Knoten gehört,
   !!    die vom jeweiligen Prozess bearbeitet werden. \n
   !!    Details in: \ref lnk_var_planktisch siehe auch \ref lnk_parallelisierung
   real , allocatable , dimension (:) :: planktonic_variable_p
  
    
   character(18), dimension(number_plankt_vari_vert) :: plankt_vari_vert_name !>    Names and Descriptions of vertically distributed planktonic_variables
   logical ::  output_plankt_vert(number_plankt_vari_vert)  !>    output-flag
   
   !>    Data array for all vertically distributed planctonic, transported variables.
   !!    Connection with QSim-variable-names through plankt_vari_vert_name
   !!    to start with a fixed number of levels at overall same elevation
   real                               :: z_plankt_lev(num_lev)
   real,    allocatable, dimension(:) :: plankt_vari_vert, plankt_vari_vert_p
   integer, allocatable, dimension(:) :: point_zone !< \anchor point_zone Zonen-nummer am Berechnungspunkt (je nachdem ob an Elementzentren oder an Knoten gerechnet wird)
   
   ! --------------------------------------------------------------------------
   ! uebergabe_werte_datenfelder
   ! --------------------------------------------------------------------------
   ! Beschreibung in uebergabe_werte.f95
   
   ! single (global) transfer values
   integer, parameter :: number_trans_val = 10  !<    number of transfer values
   
   character(18), dimension(number_trans_val) :: trans_val_name   !< Names and Descriptions of transfer values
   logical,       dimension(number_trans_val) :: output_trans_val !< output-flag
   real,          dimension(number_trans_val) :: transfer_value_p !< data array for single (global) transfer values
   
   ! depth averaged quantities
   integer, parameter :: number_trans_quant = 99  !< number of transfer quantities
   
   integer                                      :: number_trans_quant_points !< points where transfer quantities are defined (all mesh points in general)
   character(18), dimension(number_trans_quant) :: trans_quant_name          !< Names and Descriptions of transfer quantities
   logical,       dimension(number_trans_quant) :: output_trans_quant        !< output-flag
   
   !> globales (Prozess 0) Datenfeld für alle tiefengemittelten Variablen, die beim Stoffumsatz für den Informationsaustasch zwischen den einzelnen Modulen
   !! benötigt werden.
   !! Details in: \ref lnk_uebergabewerte
   real, allocatable, dimension(:) :: transfer_quantity 
   
   !>    Lokales (parallel auf allen Prozessoren) Datenfeld für alle tiefengemittelten Variablen,
   !!    die beim Stoffumsatz für den Informationsaustasch zwischen den einzelnen Modulen benötigt werden.
   !!    Bei parallelen Rechnungen enthält es nur einen Teil des Datenfeldes modell::transfer_quantity das zu den Knoten gehört,
   !!    die vom jeweiligen Prozess bearbeitet werden. 
   !!    Details in: \ref lnk_uebergabewerte siehe auch \ref lnk_parallelisierung
   real, allocatable, dimension(:) :: transfer_quantity_p
   
   ! vertically distributed transfer quantities (depthprofiles)
   integer, parameter :: number_trans_quant_vert = 28 !< number of vertically distributed transfer quantities
   integer, parameter :: num_lev_trans = 1
   
   character(18),     dimension(number_trans_quant_vert) :: trans_quant_vert_name   !>    Names and Descriptions of transfer values
   logical,           dimension(number_trans_quant_vert) :: output_trans_quant_vert !>    output-flag
   real,              dimension(num_lev_trans)           :: z_trans_lev
   real, allocatable, dimension(:)                       :: trans_quant_vert, trans_quant_vert_p

   
   ! --------------------------------------------------------------------------
   ! data fields benthic distributions
   ! --------------------------------------------------------------------------
   integer, parameter :: anz_extnct_koeff = 8
   integer, parameter :: number_benth_distr = 73 !<    number of benthic distributions
   logical, parameter :: uedau_flag = .false.    !<    uedau überstaudauer-flag
   integer, parameter :: anzrawe = 51            !< max number of Boundary Concentrations
   integer, parameter :: number_rb_hydraul = 3
   
   ! Beschreibung in benthische_verteilungen.f95
   type :: rb_zeile
      integer                  :: itag,imonat,ijahrl
      real                     :: uhrl
      real, dimension(anzrawe) :: werts
   end type rb_zeile
   
   type :: rb_punkt
      integer(int64) :: zeit_sek
      type(rb_zeile) :: zeil
   end type rb_punkt
   
   type :: rb_kante
      integer :: top, bottom, element, num
      real    :: normal_x, normal_y, laengs ! Auswärts gerichteter Normalenvektor mit Kantenlänge
   end type rb_kante
   
   type :: rb_streckenzug
      integer                            :: anzkanten, start_knoten, end_knoten
      type(rb_kante), pointer            :: kante(:)
      integer, allocatable, dimension(:) :: knoten ! alle Knotennummern in der Linie
   end type rb_streckenzug
   
   !> \anchor rb rb Randbedingungs-Struktur
   type :: rb
      integer                               :: anz_rb
      integer                               :: nr_rb
      integer                               :: tagesmittelwert_flag
      integer                               :: t_guelt
      type(rb_punkt), dimension(:), pointer :: punkt
      real, dimension(anzrawe)              :: wert_jetzt
      type(rb_streckenzug)                  :: randlinie
   end type rb
   
   
   !> points where benthic distributions are defined (all mesh points in general)
   integer                                      :: number_benthic_points   ! knotenanzahl_benth
   integer                                      :: rb_extnct_ilamda        !>  \anchor ilamda rb_extnct_ilamda Anzahl der Wellenlängen (siehe \ref lnk_extnct_rb). D. h. spektrale Auflösung des Lichts und dessen Extiktion im Wasser.
   integer                                      :: n_active_concentrations ! used number of Boundary Concentrations
   integer                                      :: ianz_rb, max_rand_nr    !
   character(18), dimension(number_benth_distr) :: benth_distr_name        !>    Names and Descriptions of benthic_distributions
   logical,       dimension(number_benth_distr) :: output_benth_distr      !>    output-flag
   real,     allocatable, dimension(:)          :: benthic_distribution    !>    globales (Prozess 0) Datenfeld für alle \ref lnk_var_benthisch
   real,     allocatable, dimension(:)          :: benthic_distribution_p  ! benthische_verteilung
   real,     allocatable, dimension(:)          :: rb_hydraul              !> siehe: \ref lnk_hydraul_rb
   real,     allocatable, dimension(:)          :: rb_hydraul_p            !> siehe: \ref lnk_hydraul_rb
   real,     allocatable, dimension(:)          :: rb_extnct_p
   type(rb), allocatable, dimension(:)          :: rabe
   
   
   ! --------------------------------------------------------------------------
   ! data field suspended matter and salinity
   ! --------------------------------------------------------------------------
   ! Beschreibung in schwebstoff_salz.f95
   integer                                   :: trueb_anzahl, salz_anzahl
   integer,        allocatable, dimension(:) :: trueb_zeit, salz_zeit      !< Feld der Zeitpunkte, an denen schwebstoff_salz Dateien vorliegen
   integer,        allocatable, dimension(:) :: trueb_zuord, salz_zuord    !< Zuordnungsfeld in welcher Reihenfolge die schwebstoff_salz Dateien aufeinander folgen
   character(300), allocatable, dimension(:) :: trueb_datei, salz_datei    !< Feld Dateinamen der schwebstoff_salz Dateien
   real,           allocatable, dimension(:) :: vert1, vert2               ! nur auf Prozess 1 vorhanden
   
   ! --------------------------------------------------------------------------
   ! water age
   ! --------------------------------------------------------------------------
   ! Beschreibung in alter.f95
   logical                           :: nur_alter
   integer                           :: wie_altern   ! 0-nix, 1-Zone, 2-Rand
   integer                           :: alter_nummer ! zonen oder randnummer für die alter berechnet wird
   real, allocatable, dimension(:,:) :: tr_integral_zone, vol_integral_zone, ent_integral_zone
   
   real, parameter :: minimum_age_tracer = 0.00001 ! minimum concentration of passive tracer for age calculation
 
   !---------------------------------------------------------------------------
   ! cross-sections
   !---------------------------------------------------------------------------
   ! description in schnitt.f95 fluxes in array: schnittflux_gang
   type :: qusch
      integer              :: nr_qs
      type(rb_streckenzug) :: schnittlinie
   end type qusch
   
   logical                                :: querschneiden
   integer                                :: anzahl_quer
   type(qusch), allocatable, dimension(:) :: querschnitt
   
   !---------------------------------------------------------------------------
   ! ausgabe_datenfelder
   !---------------------------------------------------------------------------
   ! Beschreibung in ausgabe.f95
   logical                                     :: bali
   integer                                     :: knotenanzahl_ausgabe      !< Knotenanzahl
   integer                                     :: n_output                  !< number of output times
   integer                                     :: k_ausgabe                 !< output-concentrations
   integer(int64), allocatable, dimension(:)   :: ausgabe_zeitpunkt         !< output times in unixtime
   integer,        allocatable, dimension(:)   :: ausgabe_bahnlinie
   integer,        allocatable, dimension(:)   :: ausgabe_konz
   real,           allocatable, dimension(:,:) :: ausgabe_konzentration     !< Datenfeld für alle Feldgrößen,die in QSim nur zwischen den Modulen übergeben werden.
   character(18),  allocatable, dimension(:)   :: ausgabeKonzentrationsName !< Namen der Transportkonzentrationen für Ausgabe
   
   ! -------------------------------------------------------------------------
   ! ganglinien_datenfelder
   ! -------------------------------------------------------------------------
   ! Beschreibung in ganglinien.f95
   integer                                       :: anz_gangl, ionumber
   integer                                       :: n_pl, n_ue, n_bn
   real,           allocatable, dimension(:)     :: c3ammonium, intammonium
   real,           allocatable, dimension(:,:)   :: t_gang, u_gang, tlmax_gang,  tlmin_gang
   real,           allocatable, dimension(:,:,:) :: pl_gang, ue_gang, bn_gang
   real,           allocatable, dimension(:,:,:) :: randflux_gang, schnittflux_gang
   integer,        allocatable, dimension(:)     :: knot_gangl
   integer(int64), allocatable, dimension(:)     :: q_gangl
   integer(int64), allocatable, dimension(:,:)   :: r_gang
   
   integer, parameter :: gangl_level = 1
   
   
  
contains
   
   !> Die suboutine modeverz() setzt den Namen vom modellverzeichnis aus dem beim Aufruf angegebenen
   !! Modellnamen, das ist ein Unterverzeichnis, das alle Modelldateien enthält,
   !! und dem Modellordner, dessen Name in der Umgebungsvariablen TQM gespeichert ist, zusammen.
   !! (export TQM=... | z. B. in .bashrc)
   !! 
   !! n nur auf rang 0  
   !!
   !! aus Datei module_modell.f95 ; zurück zu \ref lnk_modellerstellung
   subroutine modeverz()
      character(longname) :: aufrufargument, systemaufruf, cmd
      integer             :: io_error, sysa, icount, i, stat, length, errcode
      logical             :: exists
      call get_command(cmd, length, stat)
      
      icount = command_argument_count()
      if (icount < 1 .or. icount > 2)  then
          call qerror('Auftruf QSim3D > qsim3d "Modellverzeichnis" (kontrollknoten optional)')
      endif
      
      call get_command_argument(1, aufrufargument)
      modellverzeichnis = trim(aufrufargument)
      
      if (icount == 2) then ! kontrollknoten angegeben
         call get_command_argument(2, aufrufargument)
         read(aufrufargument, *) kontrollknoten
         if (kontrollknoten < 0) call qerror('modeverz: negative control node not permitted')
         print "(a,i0)", 'control node: ', kontrollknoten
      else
         kontrollknoten = -1
         print "(a)", "control node: -"
      endif 
      
      inquire(file = trim(modellverzeichnis), exist = exists)
      if (.not. exists) then
         call qerror('Could not find model directory: ' // trim(modellverzeichnis))
      else
         print "(2a)",'model directory: ', trim(modellverzeichnis)
      endif
      
      ! Art des hydraulichen Antriebs ermitteln:
      hydro_trieb = antriebsart()
      print "(a,i0)", "hydro_trieb = ", hydro_trieb 
      
   end subroutine modeverz
   
   
   !----+-----+----
   !> Unterscheidung welche den für den hydraulischen Antrieb verweendet werden.
   !!
   !! hydro_trieb = Flag für die Quelle des hydraulischen Antriebs: =1 Knotenbahnlinien aus casu (transinfo); =2 Elementrandflüsse aus Untrim² (netCDF)
   !!
   !! advect_algo = Flag für den Advektionsalgorithmus (noch unbenutzt, d.h. z.Z. casu lin. ELM,)
   integer function antriebsart()
      logical :: exists
      
      ! casu
      inquire(file = trim(modellverzeichnis) // 'transinfo', exist = exists)
      if (exists) then
         print "(a)", "Hydrodynamic driver: casu"
         antriebsart = 1
         return
      endif
      
      ! UnTrim²
      inquire(file = trim(modellverzeichnis) // 'transport.nc', exist = exists)
      if (exists) then
         print"(a)", "Hydrodynamic driver: NetCDF UnTrim2"
         antriebsart = 2
         return
      endif
      
      ! SCHISM
      inquire(file = trim(modellverzeichnis) // 'outputs_schism', exist = exists)
      if (exists) then
         print "(a)", "Hydrodynamic driver: NetCDF SCHISM"
         antriebsart = 3
         return
      endif
      
      call qerror("Hydrodynamic driver is missing.")
   end function antriebsart
   
   
   !!----+-----+----
   !> Read model coordinates from ModellA.txt
   !!
   !! Die <a href="./exp/MODELLA.txt" target="_blank">MODELLA.txt</a> Dateien für QSim-1D sind weiterverwendbar, aber 
   !! es werden in QSim-3D nur noch die Geologischen Breiten- und Längenkoordinaten daraus gelesen.
   !!
   !! aus Datei module_modell.f95 ; zurück zu \ref lnk_modellerstellung
   subroutine modella()
      character(500) :: dateiname, text, version, mod_name
      integer        :: open_error, ion, read_error
      
      dateiname = trim(modellverzeichnis) // '/MODELLA.txt'
      ion = 103
      open(unit = ion, file = dateiname, status = 'old', action = 'read ', iostat = open_error)
      if (open_error /= 0) call qerror('Could not open MODELLA.txt')
      
      
      if (.not. zeile(ion)) call qerror('Model version is missing in ModellA.txt')
      version = ctext
      
      if (.not. zeile(ion)) call qerror('Model name is missing in ModellA.txt')
      mod_name = ctext
      
      if (.not. zeile(ion)) call qerror('Model coordinates are missing in ModellA.txt')
      read(ctext, *, iostat = read_error) modell_geob, modell_geol
      if (read_error /= 0) call qerror("Error while reading coordinates from ModellA.txt")
      
      print*
      print "(a)", repeat("-", 80)
      print "(a)", "ModellA.txt"
      print "(a)", repeat("-", 80)
      
      print "(a,a)",    'version:   ', trim(version)
      print "(a,a)",    'name:      ', trim(mod_name)
      print "(a,f0.5)", 'latitude:  ', modell_geob
      print "(a,f0.5)", 'longitude: ', modell_geol
     
      close (ion)
      
   end subroutine modella
   
   !----+-----+----
   !> Dient dem Einlesen der nächsten nicht-#-Kommentar Zeile
   logical function zeile(ion)
      integer, intent(in) :: ion
      integer             :: io_error
      
      zeile = .FALSE.
      
      do
         read(ion, '(A)', iostat = io_error) ctext
         if (io_error /= 0) then
            zeile = .FALSE.
            return
         endif
         if (ctext(1:1) /= '#') exit
      enddo
      
      zeile = .TRUE.
      return
   end function zeile
   
   !----+-----+----
   !> Dient dem Erkennen, ob ctext (Variable in die function zeile einließt und übergibt) 
   !! eine Leerzeile ist
   logical function leerzeile()
      integer :: i
   
      leerzeile = .true.
      do i = 1,len(trim(ctext))
         if (ctext(i:i) /= " ") leerzeile = .false.
      enddo 
      return
   end function leerzeile
   
   !----+-----+----
   !> ließt bis zur nächsten zeile, die kein Kommentar und keine Leerzeile ist
   !! Ergebnis in ctext
   logical function naechste_zeile(ion)
      integer :: ion
      do while (zeile(ion)) !! nächste nicht Kommentar Zeile auffinden:
         if (.not. leerzeile()) then
            naechste_zeile = .true.
            return
         endif
      enddo 
      
      naechste_zeile = .false.
      return
   end function naechste_zeile
   
  
   !> Increment simulation time.
   subroutine zeitschritt_halb(vorher)
      use module_datetime
      
      logical, intent(in) :: vorher
      type(datetime)      :: datetime_now
      
      rechenzeit = rechenzeit + (deltat / 2)
      
      ! vor dem Zeitschritt
      if (vorher) then 
         startzeitpunkt = rechenzeit - (deltat / 2)
         endzeitpunkt = startzeitpunkt + deltat
      endif
   
   end subroutine zeitschritt_halb
   
   !----+-----+----
   !> Dient der initialisierung der Zeitsteuerung.
   subroutine ini_zeit()
      if (meinrang == 0) then ! prozess 0 only
         !rechenzeit=252590400  ! 2008
         !rechenzeit=189432000  ! 2006
         rechenzeit = 0
         !deltat=900 !3600
         deltat = 3600
         izeit = 0
         zeitschrittanzahl = 1
      endif ! only prozessor 0
   end subroutine ini_zeit
   
   !> Wenn die Nachkommastelle in lesezeit die Minuten angibt, in dezimaldarstellung rückrechnen
   real function minu_stund(lesezeit)
      real :: lesezeit
      minu_stund = aint(lesezeit)+(lesezeit-aint(lesezeit))*(100.0/60.0)
   end function minu_stund
   
   !> Prüfen, ob alle notwendigen Dateien im Modellverzeichnis vorliegen.
   logical function modell_vollstaendig()
      character(len = longname) :: aufrufargument, systemaufruf
      integer                   :: io_error,sysa, ia, ion, errcode
      logical                   :: zeile_vorhanden, age_exists
      
      modell_vollstaendig = .true.
      
      ! EREIGG.txt
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'EREIGG.txt >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         modell_vollstaendig = .false.
         print*, 'In Ihrem Modellverzeichnis fehlt die Datei EREIGG.txt'
      endif
      
      ! MODELLA.txt
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'MODELLA.txt >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         modell_vollstaendig = .false.
         print*, 'In Ihrem Modellverzeichnis fehlt die Datei MODELLA.txt'
      endif
      
      ! WETTER.txt
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'WETTER.txt >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         modell_vollstaendig = .false.
         print*, 'In Ihrem Modellverzeichnis fehlt die Datei WETTER.txt'
      endif
      
      ! MODELLG.3D.txt
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'MODELLG.3D.txt >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         modell_vollstaendig = .false.
         print*, 'In Ihrem Modellverzeichnis fehlt die Datei MODELLG.3D.txt'
      endif
      
      ! APARAM.txt
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'APARAM.txt >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         modell_vollstaendig = .false.
         print*,'In Ihrem Modellverzeichnis fehlt die Datei APARAM.txt'
      endif
      
      ! e_extnct.dat
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'e_extnct.dat >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         modell_vollstaendig = .false.
         print*,'In Ihrem Modellverzeichnis fehlt die Datei e_extnct.dat'
      endif
      
      
      select case (hydro_trieb)
         case(1) ! casu-transinfo
            write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'transinfo/meta >/dev/null 2>/dev/null'
            if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
            call system(systemaufruf,sysa)
            if (sysa /= 0) then
               modell_vollstaendig = .false.
               print*,'In Ihrem Modellverzeichnis fehlt die Datei /transinfo/meta'
            endif
            
            write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'transinfo/points >/dev/null 2>/dev/null'
            if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
            call system(systemaufruf,sysa)
            if (sysa /= 0) then
               modell_vollstaendig = .false.
               print*,'In Ihrem Modellverzeichnis fehlt die Datei transinfo/points'
               !print*,'sysa=',sysa,' systemaufruf=',trim(systemaufruf)
               !else
            endif

            write(systemaufruf,'(3A)',iostat = errcode) &
               'stat ',trim(modellverzeichnis),'transinfo/file.elements >/dev/null 2>/dev/null'
            if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
            call system(systemaufruf,sysa)
            if (sysa /= 0) then
               modell_vollstaendig = .false.
               print*,'In Ihrem Modellverzeichnis fehlt die Datei transinfo/file.elements'
            endif
            
         case(2) ! Untrim² netCDF
            write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'transport.nc >/dev/null 2>/dev/null'
            if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
            call system(systemaufruf,sysa)
            if (sysa /= 0) then
               modell_vollstaendig = .false.
               print*,'In Ihrem Modellverzeichnis fehlt die Datei transport.nc'
            endif
            
            write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'ELEMENTE.txt >/dev/null 2>/dev/null'
            if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
            call system(systemaufruf,sysa)
            if (sysa /= 0) then
               modell_vollstaendig = .false.
               print*,'In Ihrem Modellverzeichnis fehlt die Datei ELEMENTE.txt'
            endif
         
         case(3) ! SCHISM netCDF
            print*,'not yet checking completeness of SCHISM model'
            case default
            call qerror('Hydraulischer Antrieb unbekannt')
      end select
      
      ! ganglinien_knoten.txt
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'ganglinien_knoten.txt >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         modell_vollstaendig = .false.
         print*,'In Ihrem Modellverzeichnis fehlt die Datei ganglinien_knoten.txt'
      endif
      
      ! ausgabezeitpunkte.txt
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'ausgabezeitpunkte.txt >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         modell_vollstaendig = .false.
         print*,'In Ihrem Modellverzeichnis fehlt die Datei ausgabezeitpunkte.txt'
      endif
      
      ! ausgabekonzentrationen.txt
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis), &
                                                 'ausgabekonzentrationen.txt >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('modell_vollstaendig writing filename elemente_ failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         modell_vollstaendig = .false.
         print*,'In Ihrem Modellverzeichnis fehlt die Datei ausgabekonzentrationen.txt'
      endif
      
      ! alter.txt
      inquire(file = trim(modellverzeichnis) // "alter.txt", exist = age_exists) 
      if (age_exists) then
         print "(a)", "alter.txt detected: Simulation will only calculate water age"
         nur_alter = .true.
      else
         nur_alter = .false.
      endif
      
      ! qusave
      call system('which qusave >/dev/null 2>/dev/null', sysa)
      if (sysa /= 0) then
         print "(a)", 'Script `qusave` is not available. Input data will not be archived.'
      endif
      
      ! quzip
      call system('which quzip  >/dev/null 2>/dev/null', sysa)
      if (sysa /= 0) then
         print "(a)", 'Script `quzip`  is not available. Input data will not be archived.'
      endif
      
   end function modell_vollstaendig
   
   !----+-----+----
   !> Reibungsbeiwert \f$ \lambda \f$ aus Wassertiefe/Sohlabstand und Rauheitshöhe gemäß dem Colebrook-White Gesetz berechnen (nach DVWK 220).\n
   !! \f[
   !!    \lambda = \frac{8}{\left[ (1.0/ \kappa)*(log10(h/ks)) + 8.5 \right]^2}
   !! \f]
   !! mit:\n
   !! \f$ \kappa \f$  - von Karman Konstante ca. 0,4 \n
   !! \f$ h \f$ - Wassertiefe und \n
   !! \f$ ks \f$ - äquivalente Sandrauheit nach Nikuradse
   !!
   !! aus module_modell.f95
   real function lambda(ks, zet)
      real, intent(in) :: ks
      real, intent(in) :: zet
      real             :: l
      
      real, parameter :: kappa = 0.4 ! vomKarman-Konstante
      
      lambda = 10000.0 ! default
      if ((zet > 0.0) .and. (ks > 0.0)) then
         l = (1.0/kappa) * (log10(zet/ks)) + 8.5
         if (l > 0.0) lambda = 8/(l**2)
      endif
      
      return
   end function lambda
   
   
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
      real, intent(in) :: ks
      real, intent(in) :: zet
      
      if (zet > 0.0) then
         strickler = (lambda(ks,zet)*(zet**0.33333))
         if (strickler > 0.0) then
            strickler = (8*grav)/strickler
            strickler = (strickler**0.5)
         endif
      else ! trocken
         strickler = 1.0
      endif
      return
   end function strickler
   
end module modell