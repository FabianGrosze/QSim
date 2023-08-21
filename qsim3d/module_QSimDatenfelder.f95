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
!> Datenfelder QSim
!! Das Module QSimDatenfelder enthält alle von QSim verwendeten Datenfelder \n
!! Felder dürfen nicht runterdimensioniert werden, da sonst Speicherzugriffsfehler auftreten !!\n
!! QSim3D ist in Fortran95 geschrieben. Darin ist es gleichgültig, ob ein Variablenname mit Groß- oder Kleinbuchstaben geschrieben wird.
!! z. B. bezeichnet Q_NK den gleichen Speicherplatz wie q_nk.\n
!! In dieser Doxygen-Dokumentation werden alle Variablennamen klein geschrieben, weil Groß/Kleinschreibung für Doxygen signifikant ist und
!! die Verweise innerhalb der Dokumentation anders nicht funktionieren. Beil Lesen ist daher leider etwas Umdenken erforderlich, weil bestimmte
!! Groß- und Klein-Schreibungen bei QSim1D-Variablen üblich sind.
!!\n\n
!! zurück zu: \ref lnk_huellen
module QSimDatenfelder

   use module_alloc_dimensions, only: ialloc2
   
   implicit none
   public 
   
   integer, parameter, private :: azstrs = 1
   
   ! Zeitpunkt Berechnungs-Start
   integer :: itags  !< \anchor itags Tag Berechnungs-Start
   integer :: monats !< \anchor monats Monat Berechnungs-Start
   integer :: jahrs  !< \anchor jahrs Jahr Berechnungs-Start
   real    :: uhrs   !< \anchor uhrs Uhrzeit_stunden Berechnungs-Start
   
   ! Zeitpunkt Berechnungs-Ende
   integer :: itage  !< \anchor itage Tag Berechnungs-Ende
   integer :: monate !< \anchor monate Monat Berechnungs-Ende
   integer :: jahre  !< \anchor jahre Jahr Berechnungs-Ende
   real    :: uhren  !< \anchor uhren Uhrzeit_stunden Berechnungs-Ende
   
   ! --------------------------------------------------------------------------
   ! Modell- und Steuerparameter:
   ! --------------------------------------------------------------------------
   integer                  :: anze   !> \anchor anze Anzahl der Profile im Strang. In QSim3d immer 1. Siehe dazu auch \ref lnk_huellen.
   integer                  :: mstr   !> \anchor mstr Strangnummer. In QSim3d immer 1. Siehe dazu auch \ref lnk_huellen.
   integer                  :: ior    !> \anchor ior Profilnummernzähler. Läuft in QSim3D immer von 1 bis 2. Siehe dazu auch \ref lnk_huellen.
   integer                  :: ilbuhn !> \anchor ilbuhn =0 keine Buhnen in QSim3D. Siehe dazu auch \ref lnk_huellen.
   integer                  :: iwied  !> \anchor iwied  ### iwied = 0 : allererster Zeitschritt, danach iwied = 1 ###  in 3D immer 1
   integer, dimension(1000) :: flag   !> \anchor flag  flag(1+2)=0 d.h. keine Einleitungen in QSim3D. Siehe dazu auch \ref lnk_huellen.
   integer, dimension(1000) :: jiein  !> \anchor jiein =0 keine Punkt-Einleitungen in QSim3D. Siehe dazu auch \ref lnk_huellen.
   integer, dimension(1000) :: ischif !> \anchor ischif  = 0 Keine Schiffahrt. Noch unbenutzt in QSim3D. Siehe dazu auch \ref lnk_huellen.
   integer, dimension(1000) :: nkzs   !> \anchor nkzs Anzahl Tiefenschichten (tiefenaufgelöste Variante von QSim1D). In QSim3d momentan immer 1. Siehe dazu auch \ref lnk_huellen.
   real                     :: tflie  !> \anchor tflie Zeitschrittlänge in Tagen in QSim3D berechnet aus dem Zeitschritt in Sekunden \ref deltat \n
   real                     :: dh2d   !> \anchor dh2d dH2D = 0.25 ! Dicke Tiefenschicht ??? Siehe dazu auch \ref lnk_huellen.
   real, dimension(1000)    :: fkm    !> \anchor fkm =0.0 Flusskilometer unbenutzt. Siehe dazu auch \ref lnk_huellen.
   real, dimension(1000)    :: flae   !> \anchor flae Querschnittsfläche des Gewässerkörpers
                                      !!     daraus wird die Breite berechnet, die in der Belüftungsformel verwendet wird. 
                                      !!     hat keine Entsprechung im Mehrdimensionalen, daher sinnvoller Wert fürs Ästuar, so dass Breite konstant 500 m beträgt ;\n
                                      !!     Siehe dazu auch \ref lnk_huellen.
   ! --------------------------------------------------------------------------
   ! hydraulische Parameter:
   ! --------------------------------------------------------------------------
   real, dimension(1000) :: tiefe !> \ref tiefe : Wassertiefe
   real, dimension(1000) :: vmitt !> \ref vmitt Fließgeschwindigkeit (Querschnittsgemittelt in QSim-1D)
   real, dimension(1000) :: rhyd  !> \anchor rhyd rhyd, hydraulischer Radius, im Mehrdimensionalen der *TIEFE* gleichgesetzt.
   
   ! --------------------------------------------------------------------------
   ! planktische Konzentrationen, die es auch als Tiefenprofil gibt:
   ! --------------------------------------------------------------------------
   real, dimension(1000) :: tempw !> \ref tempw  Wassertemperatur
   real, dimension(1000) :: vo2   !> \ref vo2  Sauerstoffgehalt aus \ref lnk_var_planktisch
   real, dimension(1000) :: vnh4  !> \ref vnh4 Ammonium
   real, dimension(1000) :: vno2  !> \ref vno2 Nitrit
   real, dimension(1000) :: vno3  !> \ref vno3 Nitrat
   real, dimension(1000) :: gelP  !> \ref gelp Phosphor
   real, dimension(1000) :: Si    !> \ref si Silizium
   real, dimension(1000) :: aki   !> \ref aki Biomasse der Kieselalgen, \f$A_{ki}\f$ [mg L\f$^{-1}\f$]
   real, dimension(1000) :: agr   !> \ref agr Biomasse der Gruenalgen, \f$A_{gr}\f$ [mg L\f$^{-1}\f$]
   real, dimension(1000) :: abl   !> \ref abl Biomasse der Blaualgen, \f$A_{bl}\f$ [mg L\f$^{-1}\f$]
   real, dimension(1000) :: chla  !> \ref chla Chlorophyll-a
   
   ! --------------------------------------------------------------------------
   ! tiefenaufgelöste planktische Konzentrationen:
   ! --------------------------------------------------------------------------
   real, dimension(50,1000) :: tempwz !> \ref tempwz tiefenaufgelöst Wassertemperatur
   real, dimension(50,1000) :: vo2z   !> \ref vo2z tiefenaufgelöst Sauerstoffgehalt
   real, dimension(50,1000) :: vnh4z  !> \ref vnh4z tiefenaufgelöst Ammonium
   real, dimension(50,1000) :: vno2z  !> \ref vno2z tiefenaufgelöst Nitrit
   real, dimension(50,1000) :: vno3z  ! > \ref vno3z tiefenaufgelöst Nitrat
   real, dimension(50,1000) :: gelPz  !> \ref gelpz tiefenaufgelöst Phosphor
   real, dimension(50,1000) :: Siz    !> \ref siz tiefenaufgelöst Silizium
   real, dimension(50,1000) :: akiz   !> \ref akiz tiefenaufgelöst Kieselalgen
   real, dimension(50,1000) :: agrz   !> \ref agrz tiefenaufgelöst Gruenalgen
   real, dimension(50,1000) :: ablz   !> \ref ablz tiefenaufgelöst Blaualgen
   real, dimension(50,1000) :: chlaz  !> \ref chlaz tiefenaufgelöst Chlorophyll-a
   
   ! --------------------------------------------------------------------------
   ! weitere transportierte, nur tiefengemittelte, planktische Konzentrationen
   ! --------------------------------------------------------------------------
   real, dimension(1000)   :: chlaki  !> \ref chlaki Chlorophyl-a Kiesela
   real, dimension(1000)   :: chlagr  !> \ref chlagr Chlorophyl-a Grüna.
   real, dimension(1000)   :: chlabl  !> \ref chlabl Chlorophyl-a Blaua
   real, dimension(1000)   :: vx0     !> \ref vx0 Nitrosomonas
   real, dimension(1000)   :: vx02    !> \ref vx02 Nitrobacter
   real, dimension(1000)   :: obsb    !> \ref obsb kohlenstoffbürtiger biologischer Sauerstoffbedarf in 5 Tage
   real, dimension(1000)   :: ocsb    !> \ref ocsb Kohlenstoffbürtiger chemischer Sauerstoffbedarf
   real, dimension(1000)   :: vkigr   !> \ref vkigr Anteil Kiesela. an Gesamtalgenmasse
   real, dimension(1000)   :: antbl   !> \ref antbl Anteil Blau an Gesamtalgenmasse
   real, dimension(1000)   :: svhemk  !> \ref svhemk
   real, dimension(1000)   :: svhemg  !> \ref svhemg
   real, dimension(1000)   :: svhemb  !> \ref svhemb
   real, dimension(1000)   :: akbcm   !> \ref akbcm Chlorophyl-a zu Kohlenstoff Verhältnis in Kiesel-Algen
   real, dimension(1000)   :: agbcm   !> \ref agbcm Chlorophyl-a zu Kohlenstoff Verhältnis in Grün-Algen
   real, dimension(1000)   :: abbcm   !> \ref abbcm Chlorophyl-a zu Kohlenstoff Verhältnis in Blau-Algen
   real, dimension(1000)   :: akiiv   !> \ref akiiv
   real, dimension(1000)   :: agriv   !> \ref agriv
   real, dimension(1000)   :: abliv   !> \ref abliv
   real, dimension(1000)   :: Q_NK    !> \ref q_nk Sticktoff in Kieselalgen
   real, dimension(1000)   :: Q_PK    !> \ref q_pk Phosphor in Kieselalgen
   real, dimension(1000)   :: Q_SK    !> \ref q_sk Silizium in Kieselalgen
   real, dimension(1000)   :: Q_NG    !> \ref q_ng Stickstoff in Grünalgen
   real, dimension(1000)    :: Q_PG   !> \ref q_pg Phosphor in Grünalgen
   real, dimension(1000)   :: Q_NB    !> \ref q_nb Stickstoff in Blaualgen
   real, dimension(1000)   :: Q_PB    !> \ref q_pb Phosphor in Blaualgen
   real, dimension(2,1000) :: CD      !> \ref cd CD = \ref cd1 + \ref cd2 leicht und schwer abbaubare gelöste organische C-Verbindungen
   real, dimension(2,1000) :: CP      !> \ref cp CP = \ref cp1 + \ref cp2 leicht und schwer abbaubare partikuläre organische C-Verbindungen
   real, dimension(1000)   :: CM      !> \ref cm monomolekularen organischen C-Verbindungen
   real, dimension(1000)   :: BAC     !> \ref bac Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
   real, dimension(1000)   :: zBAC    !> \ref zBAC Aufnahmerate der Bakterien
   real, dimension(1000)   :: O2BSB   !> \ref o2bsb Sauerstoff-Kohlenstoffverhältnis beim C-Abbau
   real, dimension(1000)   :: BL01    !> \ref bl01 schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
   real, dimension(1000)   :: BL02    !> \ref bl02 leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
   real, dimension(1000)   :: vbsb    !> \ref vbsb BSB5 incl. lebender Organismen
   real, dimension(1000)   :: vcsb    !> \ref vcsb CSB incl. lebender Organismen
   real, dimension(1000)   :: CHNF    !> \ref chnf C-Masse der heterotrophen Nanoflagelaten
   real, dimension(1000)   :: BVHNF   !> \ref bvhnf Biovolumen der HNF  ?
   real, dimension(1000)   :: zooind  !> \ref zooind Anzahl der Rotatorien
   real, dimension(1,1000) :: TGZoo   !>  
   real, dimension(1,1000) :: akmor_1 !> Kiesel-Algen
   real, dimension(1,1000) :: agmor_1 !> Gruen-Algen
   real, dimension(1,1000) :: abmor_1 !> Blau-Algen
   real                    :: bsbZoo  !> biol. Sauerstoffbedarf in 5d je Biomasse Zooplankton =1.6 hard coded
   real, dimension(1000)   :: abrzo1  !> \ref abrzo unbenutzt ??
   real, dimension(1000)   :: ssalg   !> \ref ssalg
   real, dimension(1000)   :: ss      !> \ref ss
   real, dimension(1000)   :: fssgr   !> \ref fssgr
   real, dimension(1000)   :: fbsgr   !> \ref fbsgr ablagerungsfreien Grenzkonzentration zehrungsfähig
   real, dimension(1000)   :: frfgr   !> \ref frfgr ablagerungsfreien Grenzkonzentration refraktär
   real, dimension(1000)   :: nl0     !> \ref nl0 N/C Verhältnis
   real, dimension(1000)   :: pl0     !> \ref pl0 P/C Verhältnis
   real, dimension(1000)   :: stind   !> \ref stind
   real, dimension(1000)   :: dlarvn  !> \ref dlarvn Dreissena Larven im Wasser treibend, Ind/l
   real, dimension(1000)   :: coli   !>  \ref coli
   real, dimension(1000)   :: mw
   real, dimension(1000)   :: pw
   real, dimension(1000)   :: ca
   real, dimension(1000)   :: lf
   real, dimension(1000)   :: vph
   real, dimension(1000)   :: gesN
   real, dimension(1000)   :: gesP
   real, dimension(1000)   :: SKmor
   real, dimension(1000)   :: DOSCF
   
   ! --------------------------------------------------------------------------
   ! Übergabekonzentrationen
   ! --------------------------------------------------------------------------
   real, dimension(50,1000)            :: up_NKz, up_NGz, up_NBz, up_Siz, up_PKz, up_PGz, up_PBz, up_N2z
   real, dimension(1000)               :: bsbt, bsbctP, doN, BACmua
   real, dimension(1000)               :: abszo, dkimor, dgrmor, dblmor, BSBHNF, HNFBAC
   real, dimension(1000)               :: drfaek, drfaeg, drfaeb, zexki, zexgr, zexbl
   real, dimension(1000)               :: dorgSS, dalgki, dalggr, dalgbl, dalgak, dalgag, dalgab
   real, dimension(1000)               :: vco2, dzres1, dzres2, susn
   
   ! --------------------------------------------------------------------------
   ! benthische Verteilungen
   ! --------------------------------------------------------------------------
   real                         :: akrema !>  \anchor akrema = 0.0 
   real                         :: sbioki !>  \anchor sbioki = 0.0
   real                         :: tauscs
   real                         :: NDR, KD_N2, KNH3_X1, KHNO2_X1, KNH3_X2, KHNO2_X2
   real                         :: nwgr, nwki, nhno, nreski, nresgr, nresbl, nl0t
   real, dimension(1000)        :: tsed    ! Temperatur des Sediments
   real, dimension(1000)        :: sised   !>  \ref sised Menge an Silikat an der Gewässersohle infolge sedimentierter Algen
   real, dimension(1000)        :: pfl     ! Pflanzentrockengewicht in g/m2
   real, dimension(1000)        :: ssdr    ! Schwebstoffaufnahme durch Dreissena
   real, dimension(1000)        :: coroI, coroIs                  ! Corophium an der Böschung und an der Sohle
   real, dimension(1000,4)      :: zdrei, zdreis            ! Dreissena Masse pro Fläche Böschung, Sohle in 4 Kohorten
   real, dimension(1000,4)      :: gewdr                             ! Dreissena Muschelgewicht  in 4 Kohorten
   real, dimension(1000)        :: dlmax
   real, dimension(1000)        :: dlmaxs
   real, dimension(1000)        :: gwdmax
   real, dimension(1000)        :: sgwmue
   real, dimension(1000)        :: drHNF
   real, dimension(1000)        :: rau     !>  \anchor rau Gauckler-Manning-Strickler Reibungsbeiwert in (m**1/3)/s
   real, dimension(1,1000)      :: orgCsd  ! Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert
   real, dimension(1000)        :: bsbbet  ! Ausgabekonzentration Sauerstoffverbrauch durch Organismen auf Makrophyten
   real, dimension(50,1000)     :: hJO2    ! Sauerstoffverbrauch des Sediments (in allen Schichten ???)
   real, dimension(1000)        :: cmatki  ! Abspülung benthischer kiesel-Algen
   real, dimension(1000)        :: cmatgr  ! Abspülung benthischer gruen-Algen
   real, dimension(50)          :: senh4, seno2, seno3, vnh4zt, vno2zt, vno3zt, nwgrz, nwkiz, nwblz
   real, dimension(50)          :: nresgz, nreskz, nresbz, hcvNH4z, hcvNO2z, hcvNO3z
   real, dimension(100)         :: enh4
   real, dimension(100)         :: qeinl
   real, dimension(100)         :: ex0
   real, dimension(100)         :: eno3
   real, dimension(100)         :: ex02
   real, dimension(100)         :: eno2
   real, dimension(100)         :: egesN
   real, dimension(100)         :: enl0
   real, dimension(100)         :: qeinlL
   real, dimension(100)         :: bsbL, csbL, x0L, x02L, o2L
   real, dimension(100)         :: chlaL
   real, dimension(100)         :: eNH4L, eNO2L, eNO3L, gesNL
   real, dimension(1000)        :: go2n, agrnh4, akinh4
   real, dimension(1000)        :: sgo2n, abltbr, akitbr, agrtbr
   real, dimension(1000)        :: vabfl !> \anchor vabfl Durchfluss in QSim3D unbenutzt.  Siehe dazu auch \ref lnk_huellen.
   real, dimension(1000)        :: elen  !> \anchor elen  elen(1)=1  Elementlänge (nicht verwendet) Siehe dazu auch \ref lnk_huellen.
   real, dimension(1000)        :: sedx0, bettn, susno, agrno3, akino3
   real, dimension(1000)        :: resdr, ablno3, albewg, alberg, albewk, alberk,abegm2,abekm2
   real, dimension(1000)        :: exdrvk, exdrvg, ablnh4, exdrvb, susO2N
   real, dimension(1000)        :: sedalk, sedalb, sedalg
   real, dimension(1000)        :: bsbct, bsbctN, betO2N
   real, dimension(azStrs,1000) :: hJNH4, hJNO3
   real, dimension(50,1000)     :: dalggz, dalgkz, dalgbz, algagz, algakz, algabz, agnh4z, aknh4z, abnh4z
   real, dimension(50,1000)     :: agno3z, akno3z, abno3z, agrbrz, akibrz, ablbrz
   integer, dimension(50)       :: ieinLs !>  \anchor ieinls =0  keine Linienquellen. Siehe dazu auch \ref lnk_huellen.
   integer, dimension(100)      :: iorLa  !>  \anchor iorla =0  keine Linienquellen. Siehe dazu auch \ref lnk_huellen.
   integer, dimension(100)      :: iorLe  !>  \anchor iorle =0  keine Linienquellen. Siehe dazu auch \ref lnk_huellen.
   
   ! --------------------------------------------------------------------------
   ! konsum
   ! --------------------------------------------------------------------------
   real, dimension(1000)          :: rmuas, iras, rakr, rbar
   real, dimension(1000)          :: zHNF, HNFza, algzok, algzog, algzob
   real, dimension(50,1000)       :: algzkz, algzgz, algzbz
   
   ! --------------------------------------------------------------------------
   ! algae
   ! --------------------------------------------------------------------------
   character(255)                     :: cpfad    !> \anchor cpfad in QSim3D unbenutzt; war in QSim1D das modellverzeichnis zum einlesen von e_extnct.dat,  siehe auch \ref lnk_extnct_rb
   integer                            :: ij       !> \anchor ij Nummer des zeitschrittes während eines Tages (unbenutzt in 3D)
   integer                            :: isim_end !> \anchor isim_end von strahlg ermitteltes Simulationsende=1, von Algenroutinen für Ausgabe verwendet (unbenutzt in 3D)   real                               :: nlq
   integer                            :: ilamda   ! Anzahl der Wellenlängen
   integer, dimension(azStrs,ialloc2) :: it_h     !> \ref it_h Anzahl der Zeitschritte während der Hellphase des jeweiligen Tages (unbenutzt in 3D)
   real                               :: ma
   real                               :: ihemm
   real                               :: Iac, Ic, Iprod
   real                               :: lamda0, kreg, koeffa, koeffb, koeffc
   real                               :: Kaneu, Kbneu, Kcneu, khemmt
   real                               :: nbiogr
   real                               :: k1_P, k1_N, k1_S, IKk
   real                               :: pbiogr
   real                               :: nbiobl
   real                               :: pbiobl
   real                               :: saettk, saettg, saettb
   real                               :: a1Ki
   real                               :: a1Bl
   real                               :: a1Gr
   real,         dimension(40)        :: eta, aw, ack, acg, acb, ah, as, al
   real,         dimension(40)        :: I0, Iz
   real,         dimension(50)        :: Pz, F5z
   real,         dimension(50)        :: akgrwz
   real,         dimension(50)        :: hcchlaz
   real,         dimension(100)       :: echla  !> \anchor echla = 0.0  keine Einleitung Chlorophyll-a in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real,         dimension(100)       :: ess    !> \anchor ess = 0.0 Einleitung Schwebstoff  (nicht verwendet) Siehe dazu auch \ref lnk_huellen.
   real,         dimension(100)       :: eantbl !> \anchor eantbl  = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real,         dimension(100)       :: evkigr !> \anchor evkigr = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real(kind=8), dimension(100)       :: hemm
   real,         dimension(1000)      :: ir
   real,         dimension(1000)      :: akit_1
   real,         dimension(1000)      :: tpki, tpgr, tpbl
   real,         dimension(1000)      :: akmuea, agmuea, abmuea
   real,         dimension(1000)      :: ftaaus
   real,         dimension(1000)      :: fiaus, figaus, fibaus
   real,         dimension(1000)      :: fheaus, fhegas, fhebas
   real,         dimension(1000)      :: akraus, agreau, abreau
   real,         dimension(1000)      :: algdrk, algdrg, algdrb
   real,         dimension(1000)      :: algcok, algcog, algcob
   real,         dimension(1000)      :: sedAlk0, sedalg0, sedalb0
   real,         dimension(1000)      :: schwi
   real,         dimension(1000)      :: extk
   real,         dimension(1000)      :: dzz, dPz
   real,         dimension(1000)      :: dH2De !> \anchor dh2de dH2De = 0.25 ! unklar
   real,         dimension(1000)      :: Dz2D
   real,         dimension(40,1000)   :: extk_lamda
   real,         dimension(50,1000)   :: dkmorz
   real,         dimension(1,50,1000) :: hchlkz, hchlgz, hchlbz
   real,         dimension(1,50,1000) :: hCChlkz, hCChlbz, hCChlgz
   real,         dimension(1,50,1000) :: hQ_NKz, hQ_NBz, hQ_NGz
   
   ! --------------------------------------------------------------------------
   ! orgc, hnf
   ! --------------------------------------------------------------------------
   real                  :: frfgrs !> \anchor frfgrs  = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real                  :: fbsgrs !> \anchor fbsgrs = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real, dimension(100)  :: ecsb   !> \anchor ecsb  = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real, dimension(100)  :: ebsb   !> \anchor ebsb = 0.0      ! keine Einleitung in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real, dimension(100)  :: ezind  !> \anchor ezind =0.0  keine Einleitung Rotatorien in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real, dimension(100)  :: eCHNF  !> \anchor echnf eCHNF=0.0 keine Einleitung HNF in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real, dimension(100)  :: ebvhnf !> \anchor ebvhnf eBVHNF=0.0 keine Einleitung HNF in QSim3D. Siehe dazu auch \ref lnk_huellen.
   real, dimension(1000) :: HNFmua !> \anchor hnfmua  hnf-Kontrollausgabe
   real, dimension(1000) :: HNFrea !> \anchor hnfrea  hnf-Kontrollausgabe
   real, dimension(1000) :: HNFupa !> \anchor hnfupa  hnf-Kontrollausgabe
   real, dimension(1000) :: HNFmoa !> \anchor hnfmoa  hnf-Kontrollausgabe
   real, dimension(1000) :: HNFexa !> \anchor hnfexa  hnf-Kontrollausgabe
   real, dimension(1000) :: bsbCNB
   real, dimension(1000) :: drfaes !> \ref drfaes Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen | dreissen -> orgc,schweb | mgBiom./l je Zeitschritt
   
   ! --------------------------------------------------------------------------
   ! ph
   ! --------------------------------------------------------------------------
   real                               :: MUE, lgk1, lgk2, lgkca, lgh, lgoh
   real                               :: moco2, mohco3, moco3
   real                               :: mwv,mwt,lft, mgco2,mghco3,mgco3,kca,moca,lfv
   real(kind=8)                       :: oh,h,k1,k2
   real,    dimension(20)             :: wge
   real,    dimension(100)            :: eph,emw,elf,eca
   real,    dimension(100)            :: elfL, caL 
   real,    dimension(1000)           :: po2p,po2r
   integer, dimension(azStrs,ialloc2) :: IDWe
   character(2)                       :: cwert
   
   ! --------------------------------------------------------------------------
   ! phosphor
   ! --------------------------------------------------------------------------
   real, dimension(100)         :: ep
   real, dimension(100)         :: egesp
   real, dimension(100)         :: epl0
   real, dimension(100)         :: gpl, gespl
   real, dimension(50)          :: gelpzt, segelp
   real, dimension(50)          :: agrp, akip, ablp
   real, dimension(50)          :: hcgelpz
   real, dimension(50)          :: hcgelpez
   real, dimension(50,1000)     :: hjpo4
   real, dimension(50,1000)     :: gelpz1
   
   ! --------------------------------------------------------------------------
   ! silicate
   ! --------------------------------------------------------------------------
   real, dimension(50)          :: sizt, hcsiez, akisi, hcsiz
   real, dimension(100)         :: esi,sil
   real, dimension(1000)        :: siruek
   
   ! --------------------------------------------------------------------------
   ! oxygen
   ! --------------------------------------------------------------------------
   real, dimension(1000)           :: dalgo, dalgao, o2ein1, do2o2d, salgo, so2ein
   real, dimension(1000)           :: abeowg, abeorg, abeowk, abeork, ro2dr
   real, dimension(1000)           :: zooro2, ro2hnf, vz11
   real, dimension(azstrs,1000)    :: hschlr
   real, dimension(50,1000)        :: vo2z1, hcvo2z, vz1
   real, dimension(50,1000)        :: algaoz
   real, dimension(100)            :: eo2, etemp
   real, dimension(50)             :: vo2zt, seo2, hcvo2_2d, vo2e, hco2ez, cpart, d
   real                            :: zwgmes
   real                            :: toc_csb !> \anchor toc_csb Sauerstoffanteil beim C-Abbau durch Denitrifikation in der Wassersäule, siehe *dC_DenW* \n
                                           !! im qsim hauptprogramm auf den konstanten Wert = 3.1  gesetzt. (in QSim3d ebenfalls)
   
   ! --------------------------------------------------------------------------
   ! water temperature
   ! --------------------------------------------------------------------------
   integer                         :: irhkw, nwaerm
   integer, dimension(100)         :: typ
   real                            :: lagem
   real                            :: sonnenaufgang, sonnenuntergang
   real, dimension(20)             :: ro, wtyp, cloud, glob
   real, dimension(1000)           :: templ,fluxT1
   real, dimension(100)            :: etempL, ewaerm
   real, dimension(50,1000)        :: dtemp
   real, dimension(azStrs,1000)    :: Wlage  !< \anchor wlage Lage der zugeordneten Wetter-Stationin m ue NN
   real, dimension(azStrs,1000)    :: hWS    !< \anchor hws Höhenlage des Wasserspiegels in m ü.NHN.
   real, dimension(azStrs,1000)    :: htempw
   real, dimension(azStrs,50,1000) :: htempz
   real, dimension(1,1000)         :: WUEBKS !> \anchor wuebks wuebks Wärmeübergangskoeffizient Wasser-Sediment in KJ/(K*m2*h)
   real, dimension(1,1000)         :: spewkss !> \anchor spewkss spewkss spezifische Wärmekapazität des Sediments in KJ/(Kg*K)
   real, dimension(azStrs,1000)    :: psrefss
   real, dimension(azStrs,1000)    :: extks !> \anchor extks extks Extinktionskoeffizient für PARS
   real, dimension(azStrs,50,1000) :: hgesPz
   real, dimension(1,1000)         :: hJSi !> Silizium-Flux aus dem Sediment
   
   ! ### ilang = 0 : Vorlauf (1d) wird nicht abgelegt, danach ilang = 1 ###
   integer , parameter :: ilang = 1 !> \anchor ilang ilang Schalter, um in qsim1d einen 1d-Vorlauf zu machen | im 3D immer 1
      
   ! --------------------------------------------------------------------------   
   ! Stickstoffflüsse
   ! --------------------------------------------------------------------------
   real, dimension(azStrs,1000)    :: hFluN3, hJN2
   real, dimension(1000)           :: dC_DenW! C-Abbau durch Denitrifikation in der Wassersäule
   real, dimension(azStrs,50,1000) :: hgesNz
   real, dimension(1000)           :: JDOC1, JDOC2
   real, dimension(1000)           :: orgCsd0
   real, dimension(1,1000)         :: orgCsd_abb
   
   ! --------------------------------------------------------------------------
   ! Sedimentbezogenes
   ! --------------------------------------------------------------------------
   real, dimension(azStrs,1000)   :: hSedOM    !> \anchor hsedom hSedOM ,SedOM <- POMz, Anteil des organischen Materials im Sediment (0-1)
   real, dimension(azStrs,1000)   :: hw2       !> \anchor hw2 hw2 aus sysgenou gelesen für sedflux
   real, dimension(azStrs,1000)   :: hbedgs    !> \anchor hbedgs hBedGS ,BedGSz , Bedeckungsgrad der Sohle mit Sediment (0-1)
   real, dimension(azStrs,1000)   :: hsedvvert !> \anchor hsedvvert hsedvvert , Sedvvertz , volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h
   real, dimension(azStrs,1000)   :: hdKorn    !> \anchor hdkorn hdKorn 
   real, dimension(azStrs,1000)   :: hSised    !> \anchor hsised hSised siehe \ref sised
   real, dimension(azStrs,2,1000) :: hCD       !> \anchor hcd hCD siehe \ref cd1 und \ref cd2
   real, dimension(1,1000)        :: sedalg_mq !> \ref sedalg_mq
   real, dimension(1,1000)        :: sedss_mq  !> \anchor sedss_mq sedss_mq  Sedimentation, die auftreten würde ohne Erosion
 
   ! --------------------------------------------------------------------------
   ! macrophytes
   ! --------------------------------------------------------------------------
   ! real :: sa !> \anchor sa sa= \ref sonnenaufgang von sasu() in temperl_wetter() in update_weather() berechnet
   ! real :: su !> \anchor su su= \ref sonnenuntergang von sasu() in temperl_wetter() in update_weather() berechnet
   integer :: itstart !> \anchor itstart itstart = *starttag*
   integer :: mstart  !> \anchor mstart mstart = *startmonat*
   integer :: itmax   !> \anchor itmax itmax = *maxtag*
   integer :: mmax    !> \anchor mmax mmax = *maxmonat*
   integer :: itend   !> \anchor itend itend = *endtag*
   integer :: mend    !> \anchor mend mend = *endmonat*
   

contains
   ! Steuer-Parameter übergeben
   subroutine ini_QSimDatenfelder()
      logical :: control
      
      anze       = 1    ! Anzahl der Profile im aktuellen Strang
      mstr       = 1    ! Strangzähler
      ior        = 1    ! Laufindex
      flag(1:2)  = 0    ! keine Einleitungen
      jiein(1:2) = 0    ! null Punkt-Einleitungen
      ilbuhn     = 0    ! keine Buhnen
      nkzs(1:2)  = 1    ! nur eine Tiefenschicht
      dH2D       = -2.  !
   end subroutine ini_QSimDatenfelder
end module QSimDatenfelder
