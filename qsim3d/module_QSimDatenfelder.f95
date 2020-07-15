
!> <h1>Datenfelder QSim</h1>
!! Das module QSimDatenfelder enthält alle von QSim verwendeten Datenfelder \n
!! Felder dürfen nicht runterdimensioniert werden, da sonst Speicherzugriffsfehler auftreten !!\n
!! QSim3D ist in Fortran95 geschrieben. Darin ist es gleichgültig, ob ein Variablenname mit Groß- oder Kleinbuchstaben geschrieben wird.
!! z. B. bezeichnet Q_NK den gleichen Speicherplatz wie q_nk.\n
!! In dieser Doxygen-Dokumentation werden alle Variablennamen klein geschrieben, weil Groß/Kleinschreibung für Doxygen signifikant ist und
!! die Verweise innerhalb der Dokumentation anders nicht funktionieren. Beim Lesen ist daher leider etwas Umdenken erforderlich, weil bestimmte
!! Groß- und Klein-Schreibungen bei QSim1D-Variablen üblich sind.
!!\n\n
!! zurück zu: \ref hüllen
      module QSimDatenfelder
      implicit none
      save

!> \anchor azstrs Stranganzahl Felddimensionierungs-Parameter
    integer , parameter :: azStrs=1
!> \anchor ialloc2 Querprofile im Strang Felddimensionierungs-Parameter
    integer , parameter :: ialloc2=1000  !!!##### do not change !!!!

! Zeitpunkt Berechnungs-Start
!> \anchor itags Tag Berechnungs-Start
      integer itags
!> \anchor monats Monat Berechnungs-Start
      integer monats
!> \anchor jahrs Jahr Berechnungs-Start
      integer jahrs
!> \anchor uhrs Uhrzeit_stunden Berechnungs-Start
      real :: uhrs
! Zeitpunkt Berechnungs-Ende
!> \anchor itage Tag Berechnungs-Ende
      integer itage
!> \anchor monate Monat Berechnungs-Ende
      integer monate
!> \anchor jahre Jahr Berechnungs-Ende
      integer jahre
!> \anchor uhren Uhrzeit_stunden Berechnungs-Ende
      real :: uhren
!                                       Modell- und Steuer-Parameter:
!> \anchor anze Anzahl der Profile im Strang. In QSim3d immer 1. Siehe dazu auch \ref hüllen.
    integer                             :: anze
!> \anchor mstr Strangnummer. In QSim3d immer 1. Siehe dazu auch \ref hüllen.
    integer                             :: mstr
!> \anchor ior Profilnummernzähler. Läuft in QSim3D immer von 1 bis 2. Siehe dazu auch \ref hüllen.
    integer                             :: ior
!> \anchor ilbuhn =0 keine Buhnen in QSim3D. Siehe dazu auch \ref hüllen.
    integer                             :: ilbuhn
!> \anchor uhrz aus \ref uhrzeit_stunde berechnet von module::modell zeitsekunde()
    real                                :: uhrz
!> \anchor iwied  ### iwied = 0 : allererster Zeitschritt, danach iwied = 1 ###  in 3D immer 1              
    integer                             :: iwied
!> \anchor flag  flag(1+2)=0 d.h. keine Einleitungen in QSim3D. Siehe dazu auch \ref hüllen.
    integer, Dimension(1000)            :: flag
!> \anchor jiein =0 keine Punkt-Einleitungen in QSim3D. Siehe dazu auch \ref hüllen.
    integer, Dimension(1000)            :: jiein
!> \anchor ischif  = 0 Keine Schiffahrt. Noch unbenutzt in QSim3D. Siehe dazu auch \ref hüllen.
    integer, Dimension(1000)            :: ischif
!> \anchor nkzs Anzahl Tiefenschichten (tiefenaufgelöste Variante von QSim1D). In QSim3d momentan immer 1. Siehe dazu auch \ref hüllen.
    integer, Dimension(1000)            :: nkzs
!>  \anchor tflie Zeitschrittlänge in Tagen in QSim3D berechnet aus dem Zeitschritt in Sekunden \ref dt \n
!!  tflie = real(dt)/86400
    real                                :: TFLIE
!> \anchor dh2d dH2D = 0.25 ! Dicke Tiefenschicht ??? Siehe dazu auch \ref hüllen.
    real                                :: dh2d
!> \anchor fkm =0.0 Flusskilometer unbenutzt. Siehe dazu auch \ref hüllen.
    real, Dimension(1000)               :: fkm
!> \anchor flae Querschnittsfläche des Gewässerkörpers\n
!! daraus wird die Breite berechnet, die in der Belüftungsformel verwendet wird.  \n
!! hat keine Entsprechung im Mehrdimensionalen, daher sinnvoller Wert fürs Ästuar, so dass Breite konstant 500 m beträgt ;\n
!! Siehe dazu auch \ref hüllen.
    real, Dimension(1000)               :: flae
!                                       hydraulische Parameter:
!>  \ref TIEFE Wassertiefe
    real, Dimension(1000)               :: tiefe
!> \ref vmitt Fließgeschwindigkeit (Querschnittsgemittelt in QSim-1D)
    real, Dimension(1000)               :: vmitt
!>  \anchor rhyd rhyd, hydraulischer Radius, im Mehrdimensionalen der \ref TIEFE gleichgesetzt.
    real, Dimension(1000)               :: rhyd

!                                       planktische Konzentrationen, die es auch als Tiefenprofil gibt:
!> \ref tempw  Wassertemperatur 
    real, Dimension(1000)               :: tempw
!> \ref vo2  Sauerstoffgehalt 
    real, Dimension(1000)               :: vo2
!> \ref vnh4 Ammonium
    real, Dimension(1000)               :: vnh4
!> \ref vno2 Nitrit
    real, Dimension(1000)               :: vno2
!> \ref vno3 Nitrat
    real, Dimension(1000)               :: vno3
!> \ref gelp Phosphor
    real, Dimension(1000)               :: gelP
!> \ref si Silizium
    real, Dimension(1000)               :: Si
!> \ref aki Kieselalgen
    real, Dimension(1000)               :: aki
!> \ref agr Gruenalgen
    real, Dimension(1000)               :: agr
!> \ref abl Blaualgen
    real, Dimension(1000)               :: abl
!> \ref chla Chlorophyll-a
    real, Dimension(1000)               :: chla
!                                       tiefenaufgelöste planktische Konzentrationen:
!> \ref tempwz tiefenaufgelöst Wassertemperatur 
    real, Dimension(50,1000)            :: tempwz
!> \ref vo2z tiefenaufgelöst Sauerstoffgehalt 
    real, Dimension(50,1000)            :: vo2z
!> \ref vnh4z tiefenaufgelöst Ammonium
    real, Dimension(50,1000)            :: vnh4z
!> \ref vno2z tiefenaufgelöst Nitrit
    real, Dimension(50,1000)            :: vno2z
!> \ref vno3z tiefenaufgelöst Nitrat
    real, Dimension(50,1000)            :: vno3z
!> \ref gelpz tiefenaufgelöst Phosphor
    real, Dimension(50,1000)            :: gelPz
!> \ref siz tiefenaufgelöst Silizium
    real, Dimension(50,1000)            :: Siz
!> \ref akiz tiefenaufgelöst Kieselalgen
    real, Dimension(50,1000)            :: akiz
!> \ref agrz tiefenaufgelöst Gruenalgen
    real, Dimension(50,1000)            :: agrz
!> \ref ablz tiefenaufgelöst Blaualgen
    real, Dimension(50,1000)            :: ablz
!> \ref chlaz tiefenaufgelöst Chlorophyll-a
    real, Dimension(50,1000)            :: chlaz
!                                       weitere transportierte, nur tiefengemittelte, planktische Konzentrationen
!> \ref chlaki Chlorophyl-a Kiesela
    real, Dimension(1000)               :: chlaki
!> \ref chlagr Chlorophyl-a Grüna.
    real, Dimension(1000)               :: chlagr
!> \ref chlabl Chlorophyl-a Blaua
    real, Dimension(1000)               :: chlabl
!> \ref vx0 Nitrosomonas
    real, Dimension(1000)               :: vx0
!> \ref vx02 Nitrobacter
    real, Dimension(1000)               :: vx02
!> \ref obsb kohlenstoffbürtiger biologischer Sauerstoffbedarf in 5 Tage
    real, Dimension(1000)               :: obsb
!> \ref ocsb Kohlenstoffbürtiger chemischer Sauerstoffbedarf
    real, Dimension(1000)               :: ocsb
!>  \ref vkigr Anteil Kiesela. an Gesamtalgenmasse
    real, Dimension(1000)               :: vkigr
!>  \ref antbl Anteil Blau an Gesamtalgenmasse
    real, Dimension(1000)               :: antbl
!>  \ref svhemk
    real, Dimension(1000)               :: svhemk
!>  \ref svhemg
    real, Dimension(1000)               :: svhemg
!>  \ref svhemb
    real, Dimension(1000)               :: svhemb
!>  \ref akbcm Chlorophyl-a zu Kohlenstoff Verhältnis in Kiesel-Algen
    real, Dimension(1000)               :: akbcm
!>  \ref agbcm Chlorophyl-a zu Kohlenstoff Verhältnis in Grün-Algen
    real, Dimension(1000)               :: agbcm
!>  \ref abbcm Chlorophyl-a zu Kohlenstoff Verhältnis in Blau-Algen
    real, Dimension(1000)               :: abbcm
!>  \ref akiiv
    real, Dimension(1000)               :: akiiv
!>  \ref agriv
    real, Dimension(1000)               :: agriv
!>  \ref abliv
    real, Dimension(1000)               :: abliv
!>  \ref q_nk Sticktoff in Kieselalgen
    real, Dimension(1000)               :: Q_NK
!>  \ref q_pk Phosphor in Kieselalgen
    real, Dimension(1000)               :: Q_PK
!>  \ref q_sk Silizium in Kieselalgen
    real, Dimension(1000)               :: Q_SK
!>  \ref q_ng Stickstoff in Grünalgen
    real, Dimension(1000)               :: Q_NG
!>  \ref q_pg Phosphor in Grünalgen
   real, Dimension(1000)                :: Q_PG
!>  \ref q_nb Stickstoff in Blaualgen
    real, Dimension(1000)               :: Q_NB
!>  \ref q_pb Phosphor in Blaualgen
    real, Dimension(1000)               :: Q_PB
!> CD=\ref cd1+\ref cd2 leicht und schwer abbaubare gelöste organische C-Verbindungen
    real, Dimension(2,1000)             :: CD
!> CP=\ref cp1+\ref cp2 leicht und schwer abbaubare partikuläre organische C-Verbindungen
    real, Dimension(2,1000)             :: CP
!>  \ref cm monomolekularen organischen C-Verbindungen
    real, Dimension(1000)               :: CM
!> \ref bac Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
    real, Dimension(1000)               :: BAC
!> \ref zBAC Aufnahmerate der Bakterien
    real, Dimension(1000)               :: zBAC 
!> \ref o2bsb Sauerstoff-Kohlenstoffverhältnis beim C-Abbau
    real, Dimension(1000)               :: O2BSB
!>  \ref bl01 schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
    real, Dimension(1000)               :: BL01
!>  \ref bl02 leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
    real, Dimension(1000)               :: BL02
!>  \ref vbsb BSB5 incl. lebender Organismen
    real, Dimension(1000)               :: vbsb
!>  \ref vcsb CSB incl. lebender Organismen	
    real, Dimension(1000)               :: vcsb
!>  \ref chnf C-Masse der heterotrophen Nanoflagelaten
    real, Dimension(1000)               :: CHNF
!>  \ref bvhnf Biovolumen der HNF  ?
    real, Dimension(1000)               :: BVHNF
!>  \ref zooind Anzahl der Rotatorien
    real, Dimension(1000)               :: zooind
!>  \ref TGZoo Gewicht einer Rotatorie
    real, Dimension(1,1000)               :: TGZoo
!>  \ref akmor_1 akmor_1 ?? unklar ??
    real, Dimension(1,1000)               :: akmor_1 !  Kiesel-Algen
!>  \ref agmor_1 agmor_1 ?? unklar ??
    real, Dimension(1,1000)               :: agmor_1 !  Gruen-Algen
!>  \ref abmor_1 abmor_1 ?? unklar ??
    real, Dimension(1,1000)               :: abmor_1 !  Blau-Algen


!>  \ref bsbZoo biol. Sauerstoffbedarf in 5d je Biomasse Zooplankton =1.6 hard coded
    real bsbZoo
!>  \ref abrzo unbenutzt ??
    real, Dimension(1000)               :: abrzo1
!>  \ref ssalg
    real, Dimension(1000)               :: ssalg
!>  \ref ss
    real, Dimension(1000)               :: ss
!>  \ref fssgr
    real, Dimension(1000)               :: fssgr
!>  \ref fbsgr ablagerungsfreien Grenzkonzentration zehrungsfähig ???
    real, Dimension(1000)               :: fbsgr
!>  \ref frfgr ablagerungsfreien Grenzkonzentration refraktär ???
    real, Dimension(1000)               :: frfgr
!>   ref nl0 N/C Verhältnis
    real, Dimension(1000)               :: nl0
!>  \ref pl0 P/C Verhältnis
    real, Dimension(1000)               :: pl0
!>  \ref stind
    real, Dimension(1000)               :: stind
! dlarvn Dreissena Larven im Wasser treibend, Ind/l
    real, Dimension(1000)               :: dlarvn
!>  \ref coli
    real, Dimension(1000)               :: coli
!>  \ref mw
    real, Dimension(1000)               :: mw
!>  \ref pw
    real, Dimension(1000)               :: pw
!>  \ref ca
    real, Dimension(1000)               :: ca
!>  \ref lf
    real, Dimension(1000)               :: lf
!>  \ref vph
    real, Dimension(1000)               :: vph
!>  \ref gesn
    real, Dimension(1000)               :: gesN
!>  \ref gesp
    real, Dimension(1000)               :: gesP
!>  \ref skmor
    real, Dimension(1000)               :: SKmor
!>  \ref doscf
    real, Dimension(1000)               :: DOSCF

!                                       Übergabekonzentrationen Konzentrationen
    real, Dimension(50,1000)            :: up_NKz, up_NGz, up_NBz, up_Siz, up_PKz, up_PGz, up_PBz, up_N2z
    real, Dimension(1000)               :: bsbt, bsbctP, doN, BACmua
    real, Dimension(1000)               :: abszo, dkimor, dgrmor, dblmor, BSBHNF, HNFBAC
    real, Dimension(1000)               :: drfaek, drfaeg, drfaeb, zexki, zexgr, zexbl
    real, Dimension(1000)               :: dorgSS, dalgki, dalggr, dalgbl, dalgak, dalgag, dalgab
    real, Dimension(1000)               :: vco2, dzres1, dzres2, susn
!>  \ref nzoo nzoo Stickstoffanteil in der Rotatorienbiomasse  mgN/mgBiom.
    real                                :: nzoo
!>  \ref pZoo pZoo Phosphoranteil in der Rotatorienbiomasse  mgP/mgBiom.
    real                                :: pZoo

!                                       benthische Verteilungen
    real, Dimension(1000)               :: tsed    ! Temperatur des Sediments
!>  \anchor sised sised Menge an Silikat an der Gewässersohle infolge sedimentierter Algen
    real, Dimension(1000)               :: sised   ! Menge an Silikat an der Gewässersohle infolge sedimentierter Algen (aufsummiert für den Simulationszeitraum) in algaeski()
    real, Dimension(1000)               :: pfl     ! Pflanzentrockengewicht in g/m2
    real, Dimension(1000)               :: ssdr    ! Schwebstoffaufnahme durch Dreissena
    real coroI(1000),coroIs(1000)                  ! Corophium an der Böschung und an der Sohle
    real zdrei(1000,4) , zdreis(1000,4)            ! Dreissena Masse pro Fläche Böschung, Sohle in 4 Kohorten
    real gewdr(1000,4)                             ! Dreissena Muschelgewicht  in 4 Kohorten
    real dlmax(1000)                               ! Dreissena ??
    real dlmaxs(1000)                              ! Dreissena ??
    real gwdmax(1000)                              ! Dreissena ??
    real sgwmue(1000)                              ! Dreissena ??
    real drHNF(1000)                               ! Dreissena fressen HNF

!>  \anchor rau Gauckler-Manning-Strickler Reibungsbeiwert in (m**1/3)/s 
!!  wird von function strickler() (module_modell.f95) aus der äuivalenten Sandrauheit \ref ks und der \ref TIEFE ermittelt.
    real, Dimension(1000)               :: rau     ! Gauckler-Manning-Strickler Reibungsbeiwert
    real, Dimension(1,1000)             :: orgCsd  ! Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert
    real, Dimension(1000)               :: bsbbet  ! Ausgabekonzentration Sauerstoffverbrauch durch Organismen auf Makrophyten
    real, Dimension(50,1000)            :: hJO2    ! Sauerstoffverbrauch des Sediments (in allen Schichten ???)
    real, Dimension(1000)               :: cmatki  ! Abspülung benthischer kiesel-Algen
    real, Dimension(1000)               :: cmatgr  ! Abspülung benthischer gruen-Algen

!>                                      global parameter from APARAM.txt
      real :: agchl,aggmax,IKge,agksn,agksp 
      real :: agremi,frmuge,bsbgr,csbgr,Qmx_NG 
      real :: Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG 
      real :: opgrmi,opgrma,asgre,ToptG,kTemp_gr
      real :: akchl,akgmax,IKke,akksn,akksp 
      real :: akkssi,akremi,frmuke,bsbki,csbki 
      real :: Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK 
      real :: Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi 
!      real :: opkima,askie,ToptK,TmaxK,abchl 
      real :: opkima,askie,ToptK,kTemp_Ki,abchl
      real :: abgmax,IKbe,abksn,abksp,abremi 
      real :: frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB 
      real :: Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi 
      real :: opblma,asble,ToptB,kTemp_bl
      integer :: ifix
      real :: irmaxe,FopIRe,GRote,zresge,zakie 
      real :: zagre,zable,ynmx1e,stks1e,anitrie 
      real :: bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e
      real :: bnmx2e,bnks2e,KNH4e,KapN3e,hyPe 
      real :: hymxDe,KsD1e,KsD2e,KsMe,upBACe 
      real :: YBACe,rsGBACe,FoptDe,upHNFe,BACkse 
      real :: alamda,fPOC1e,fPOC2e,SorpCape,Klange
      real :: KdNh3e,ratecde,etacde,ratecie,xnuece
      real :: ratecge,ratecse

!!--------------------------------------------------------------------------------------------
!    integer                             :: ieros, iph !!jetzt in module::modell
!>  \anchor ieinls =0  keine Linienquellen. Siehe dazu auch \ref hüllen.
    integer, Dimension(50)              :: ieinLs
!>  \anchor iorla =0  keine Linienquellen. Siehe dazu auch \ref hüllen.
    integer, Dimension(100)             :: iorLa
!>  \anchor iorle =0  keine Linienquellen. Siehe dazu auch \ref hüllen.
    integer, Dimension(100)             :: iorLe
!>  \anchor akrema =  0.0    unbenutzt
    real                                :: akrema
!>  \anchor sbioki = 0.0 ! unbenutzt
    real                                :: sbioki
    real                                :: tauscs
    real                                :: NDR, KD_N2, KNH3_X1, KHNO2_X1, KNH3_X2, KHNO2_X2
    real                                :: nwgr, nwki, nhno, nreski, nresgr, nresbl, nl0t  
    real, Dimension(50)                 :: senh4, seno2, seno3, vnh4zt, vno2zt, vno3zt, nwgrz, nwkiz, nwblz
    real, Dimension(50)                 :: nresgz, nreskz, nresbz, hcvNH4z, hcvNO2z, hcvNO3z    
    real, Dimension(100)                :: enh4
!>  \anchor qeinl  0.0      ! kein Volumenstrom Punkteinl. Siehe dazu auch \ref hüllen.
    real, Dimension(100)                :: qeinl
!>  \anchor ex0
    real, Dimension(100)                :: ex0
!>  \anchor eno3
    real, Dimension(100)                :: eno3
!>  \anchor ex02
    real, Dimension(100)                :: ex02
!>  \anchor eno2
    real, Dimension(100)                :: eno2
!>  \anchor egesn
    real, Dimension(100)                :: egesN
!>  \anchor enl0
    real, Dimension(100)                :: enl0
!>  \anchor qeinlL =0.0  für Linienquelle; nicht verwendet. Siehe dazu auch \ref hüllen.
    real, Dimension(100)                :: qeinlL
!   Linienquellen werden im 3D nicht verwendet
    real, Dimension(100)                    ::  bsbL, csbL, x0L, x02L, o2L
!>  \anchor chlal =0.0  für Linienquelle; nicht verwendet. Siehe dazu auch \ref hüllen.
    real, Dimension(100)                :: chlaL
    real, Dimension(100)                :: eNH4L, eNO2L, eNO3L, gesNL 

    real, Dimension(1000)               :: go2n, agrnh4, akinh4
    real, Dimension(1000)               :: sgo2n, abltbr, akitbr, agrtbr
!> \anchor vabfl Durchfluss in QSim3D unbenutzt.  Siehe dazu auch \ref hüllen.
    real, Dimension(1000)               :: vabfl
!> \anchor elen  elen(1)=1  Elementlänge (nicht verwendet) Siehe dazu auch \ref hüllen.
    real, Dimension(1000)               :: elen
    real, Dimension(1000)               :: sedx0, bettn, susno, agrno3, akino3  
    real, Dimension(1000)               :: resdr, ablno3, albewg, alberg, albewk, alberk,abegm2,abekm2

    real, Dimension(1000)               :: exdrvk, exdrvg, ablnh4, exdrvb, susO2N    
    real, Dimension(1000)               :: sedalk, sedalb, sedalg
    real, Dimension(1000)               :: bsbct, bsbctN, betO2N      
    real, Dimension(azStrs,1000)            :: hJNH4, hJNO3 
    real, Dimension(50,1000)            :: dalggz, dalgkz, dalgbz, algagz, algakz, algabz, agnh4z, aknh4z, abnh4z 
    real, Dimension(50,1000)            :: agno3z, akno3z, abno3z, agrbrz, akibrz, ablbrz 

! konsum...
    real, Dimension(1000)          :: rmuas, iras, rakr, rbar 
    real, Dimension(1000)          :: zHNF, HNFza, algzok, algzog, algzob 
    real, Dimension(50,1000)       :: algzkz, algzgz, algzbz 

! algae...
!> \anchor cpfad in QSim3D unbenutzt; war in QSim1D das modellverzeichnis zum Einlesen von e_extnct.dat,  siehe auch \ref extnct_rb 
      character(255) cpfad
      REAL LNQ,ir(1000)
!> \anchor echla = 0.0  keine Einleitung Chlorophyll-a in QSim3D. Siehe dazu auch \ref hüllen.
      real echla(100)
!> \anchor ess = 0.0 Einleitung Schwebstoff  (nicht verwendet) Siehe dazu auch \ref hüllen.
      real ess(100) 
      real ma
!> \anchor eantbl  = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref hüllen.
      real eantbl(100)
!> \anchor evkigr = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref hüllen.
      real evkigr(100) 
      real akit_1(1000), ihemm 
      real tpki(1000),tpgr(1000),tpbl(1000)
      real akmuea(1000),agmuea(1000),abmuea(1000)
      real ftaaus(1000)
      real fiaus(1000),figaus(1000),fibaus(1000)
      real fheaus(1000),fhegas(1000),fhebas(1000)
      real akraus(1000),agreau(1000),abreau(1000)
      real algdrk(1000),algdrg(1000),algdrb(1000)
      real algcok(1000),algcog(1000),algcob(1000) 
      real eta(40),aw(40),ack(40),acg(40),acb(40) 
      real(kind=8) hemm(100) !! real*8 hemm(100) 
      real ah(40),as(40),al(40),schwi(1000) 
      real I0(40),Iz(40),Iac,Ic,Iprod,extk(1000) 

      real dzz(1000),dPz(1000),Pz(50),F5z(50) 
!> \anchor dh2de dH2De = 0.25 ! unklar
      real dH2De(1000) 
      real akgrwz(50),Dz2D(1000) 
      real lamda0,kreg,koeffa,koeffb,koeffc 
      real Kaneu,Kbneu,Kcneu,khemmt 
      real dkmorz(50,1000) 
      real k1_P,k1_N,k1_S,IKk 
      real hcchlaz(50) 
      real extk_lamda(40,1000)
      integer ilamda    ! Anzahl der Wellenlängen
      real, Dimension(1,50,1000)        :: hchlkz, hchlgz, hchlbz
!> \anchor nbiogr  ! unbenutzt
      real nbiogr
!> \anchor pbiogr ! unbenutzt
      real pbiogr
!> \anchor nbiobl ! unbenutzt
      real nbiobl
!> \anchor pbiobl ! unbenutzt
      real pbiobl
      real saettk, saettg, saettb
!> \anchor it_h Anzahl der Zeitschritte während der Hellphase des jeweiligen Tages (unbenutzt in 3D)
      integer, Dimension(azStrs,ialloc2)            :: it_h
!> \anchor ij Nummer des zeitschrittes während eines Tages (unbenutzt in 3D)
      integer            :: ij
!> \anchor isim_end von strahlg ermitteltes Simulationsende=1, von Algenroutinen für Ausgabe verwendet (unbenutzt in 3D)
      integer            :: isim_end

      real, Dimension(1,50,1000)            :: hCChlkz, hCChlbz, hCChlgz  
!> \anchor caki \anchor cagr \anchor cabl \anchor czoo Kohlenstoffgehalt der Biomassen 
!!Kiese-,Grün- und Balualgen, Zooplankton ; mgC/mgBio; in ini_algae() konstant gesetzt.
      real  Caki, Cagr, Cabl, CZoo
!> \anchor a1Ki \anchor a2Ki \anchor a3Ki 
!! ; in ini_algae() gesetzt
      real  a1Ki, a2Ki, a3Ki
!> \anchor a1bl \anchor a2bl \anchor a3bl
!! ; in ini_algae() gesetzt
      real  a1Bl, a2Bl, a3Bl
!> \anchor a1gr \anchor a2gr \anchor a3gr
!! ; in ini_algae() gesetzt
      real a1Gr, a2Gr, a3Gr

      real, Dimension(1000)                 :: sedAlk0, sedalg0, sedalb0       ! sedAlk0 wird nur an k_eps() übergeben.
      real, Dimension(1,50,1000)            :: hQ_NKz, hQ_NBz, hQ_NGz

! orgc, hnf:
!>    \anchor ecsb  = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref hüllen.
      real, Dimension(100)                 :: ecsb
!>    \anchor ebsb = 0.0      ! keine Einleitung in QSim3D. Siehe dazu auch \ref hüllen.
      real, Dimension(100)                 :: ebsb
!>    \anchor ezind =0.0  keine Einleitung Rotatorien in QSim3D. Siehe dazu auch \ref hüllen.
      real, Dimension(100)                 :: ezind
!>    \anchor echnf eCHNF=0.0 keine Einleitung HNF in QSim3D. Siehe dazu auch \ref hüllen.
      real, Dimension(100)                 :: eCHNF
!>    \anchor ebvhnf eBVHNF=0.0 keine Einleitung HNF in QSim3D. Siehe dazu auch \ref hüllen.
      real, Dimension(100)                 :: ebvhnf
!>    \anchor hnfmua  hnf-Kontrollausgabe
!>    \anchor hnfrea  hnf-Kontrollausgabe
!>    \anchor hnfupa  hnf-Kontrollausgabe
!>    \anchor hnfmoa  hnf-Kontrollausgabe
!>    \anchor hnfexa  hnf-Kontrollausgabe
      real HNFmua(1000),HNFrea(1000),HNFupa(1000),HNFmoa(1000),HNFexa(1000) ! hnf-Kontrollausgaben

!>  \anchor sdbsb = 0.0      ! unbenutzt
    real, Dimension(1000)                :: sdbsb
!>  \anchor bsbcnb bsbCNB
    real, Dimension(1000)                :: bsbCNB 
!>  \anchor drfaes drfaes Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen | dreissen -> orgc,schweb | mgBiom./l je Zeitschritt
    real, Dimension(1000)                :: drfaes  
!     bisher implizit:
!>  \anchor frfgrs  = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref hüllen.
      real    :: frfgrs
!>  \anchor fbsgrs = 0.0  keine Einleitung in QSim3D. Siehe dazu auch \ref hüllen.
      real    :: fbsgrs
! ph:
      real, Dimension(100)       :: eph,emw,elf,eca
      real, Dimension(100)       :: elfL, caL ! Linienenleitung?
      real, Dimension(20)        :: wge
      real(kind=8)               :: oh,h,k1,k2 !!wy
      real                       :: MUE,lgk1,lgk2,lgkca,lgh,lgoh,moco2,mohco3,moco3 
      real                       :: mwv,mwt,lft, mgco2,mghco3,mgco3,kca,moca,lfv 
      real, Dimension(1000)      :: po2p,po2r
      integer, Dimension(azStrs,ialloc2)            :: IDWe
      character(2)               :: cwert 
! po4s:
      real ep(100)
      real egesP(100)
      real epl0(100) 
      real gPL(100),gesPL(100) 
      real gelpz1(50,1000)
      real gelpzt(50),segelP(50) 
      real agrP(50),akiP(50),ablP(50),hJPO4(50,1000),hcgelPz(50)
      real hcgelpEz(50) 
! silikat:
      real, Dimension(50)            :: Sizt,hcSiEz,akisi,hcSiz
      real, Dimension(100)           :: esi,SiL
      real, Dimension(1000)          :: SiRuek
! oxygen
      real, Dimension(1000)      :: dalgo, dalgao, o2ein1, dO2o2D, salgo, so2ein
      real, Dimension(1000)      :: abeowg, abeorg, abeowk, abeork, ro2dr
      real, Dimension(1000)      :: zooro2, rO2HNF, vz11, saett
      real, Dimension(azStrs,1000)    :: hschlr
      real, Dimension(50,1000)   :: vo2z1, hcvo2z, vz1 
      real, Dimension(50,1000)   :: algaoz
      real, Dimension(100)       :: eo2, etemp
      real, Dimension(50)        :: vo2zt, seo2, hcvo2_2D, vo2e, hco2Ez, Cpart, D
      real    :: zwgmes
!>  \anchor toc_csb Sauerstoffanteil beim C-Abbau durch Denitrifikation in der Wassersäule, siehe \ref dC_DenW \n
!! im qsim hauptprogramm auf den konstanten Wert = 3.1  gesetzt. (in QSim3d ebenfalls)
      real    :: TOC_CSB

! temperw
!! Felder dürfen nicht runterdimensioniert werden, da sonst Speicherzugriffsfehler auftreten !!
real                        :: lagem
Real, Dimension(20)         :: ro, typw, cloud, glob
real, Dimension(1000)       :: templ,fluxT1
real, Dimension(100)        :: etempL, ewaerm
real, Dimension(50,1000)    :: dtemp

!>  \anchor wlage Lage der zugeordneten Wetter-Stationin m ue NN
real, Dimension(azStrs,1000)    :: Wlage !  anchor Wlage Lage der Wetter-Stationin m ue NN 
!>  \anchor hws Höhenlage des Wasserspiegels in m ü.NHN. Wird aus \ref wsp der \ref hydraul_rb zugewiesen.
real, Dimension(azStrs,1000)    :: hWS 
real, Dimension(azStrs,1000)    :: htempw
real, Dimension(azStrs,50,1000) :: htempz
! bisher implizit definiert:
integer                     :: irhkw, nwaerm
real                        :: sonnenaufgang, sonnenuntergang

! ### ilang = 0 : Vorlauf (1d) wird nicht abgelegt, danach ilang = 1 ###       
!> \anchor ilang ilang Schalter, um in qsim1d einen 1d-Vorlauf zu machen | im 3D immer 1
    integer , parameter :: ilang=1

integer, Dimension(100)     :: typ
! temperw_korr27jan14
!>  \anchor ifehl Fehlernummer 
integer                     :: ifehl
!>  \anchor ifhstr Strang in dem der Fehler auftrat
integer                     :: ifhStr

!> \anchor wuebks wuebks Wärmeübergangskoeffizient Wasser-Sediment in KJ/(K*m2*h) wenn vorhanden aus \ref wuebk , sonst WUEBK0 = 350. \n
!! qsim.f90:1215: ,temp.dat=77, read(77,1047)aKSED(mstr,mA),eKSED(mstr,mA),SPEWKSx(mstr,mA),WUEBKx(mstr,mA),PSREFSx(mstr,mA) \n
!! QSim3D benutzt WUEBK0                            
real, Dimension(1,1000)    :: WUEBKS
!> \anchor spewkss spewkss spezifische Wärmekapazität des Sediments in KJ/(Kg*K) wenn vorhanden aus \ref spewks , sonst speWKS0 = 0.8 \n
!! qsim.f90:1215: ,temp.dat=77, read(77,1047)aKSED(mstr,mA),eKSED(mstr,mA),SPEWKSx(mstr,mA),WUEBKx(mstr,mA),PSREFSx(mstr,mA)  \n  
!! QSim3D benutzt speWKS0                     
real, Dimension(1,1000)    :: SPEWKSS  
!> \anchor psrefss psrefss unbenutzt?, PSREFS0 = 0.8 wenn vorhanden aus \ref psrefs
!! qsim.f90:1215: ,temp.dat=77, read(77,1047)aKSED(mstr,mA),eKSED(mstr,mA),SPEWKSx(mstr,mA),WUEBKx(mstr,mA),PSREFSx(mstr,mA)                                    
real, Dimension(azStrs,1000)    :: PSREFSS 
!> \anchor extks extks Extinktionskoeffizient für PARS (nur bei Temperaturmodellierung erforderlich!)
!! algaeski:      if(EXTKS(mstr,ior)>0.0)EXTK(ior) = EXTKS(mstr,ior) falls größer null überschreibt er auch Algen-Extinktion
      real, Dimension(azStrs,1000)    :: extks  

!>   ??
real, Dimension(azStrs,50,1000) :: hgesPz 

!> Silizium-Flux aus dem Sediment
real, Dimension(1,1000)   :: hJSi 

!> Stickstoffflüsse
real, Dimension(azStrs,1000)   :: hFluN3, hJN2

real, Dimension(1000) ::dC_DenW! C-Abbau durch Denitrifikation in der Wassersäule
real, Dimension(azStrs,50,1000) :: hgesNz 

    real, Dimension(1000)         :: JDOC1, JDOC2
    real, Dimension(1000)         :: orgCsd0
    real, Dimension(1,1000)       :: orgCsd_abb 

! Sediment-bezogenes
!> \anchor hsedom hSedOM ,SedOM <- POMz, Anteil des organischen Materials im Sediment (0-1)
    real, Dimension(azStrs,1000)   :: hSedOM
!> \anchor hw2 hw2 aus sysgenou gelesen für sedflux
    real, Dimension(azStrs,1000)   :: hw2
!> \anchor hbedgs hBedGS ,BedGSz , Bedeckungsgrad der Sohle mit Sediment (0-1)
    real, Dimension(azStrs,1000)   :: hbedgs
!> \anchor hsedvvert hsedvvert , Sedvvertz , volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h 
    real, Dimension(azStrs,1000)   :: hsedvvert
!> \anchor hdkorn hdKorn
    real, Dimension(azStrs,1000)   :: hdKorn
!> \anchor hsised hSised siehe \ref sised
    real, Dimension(azStrs,1000)   :: hSised  
!> \anchor hcd hCD siehe \ref cd1 und \ref cd2
     real, Dimension(azStrs,2,1000) :: hCD 
!> \anchor sedalg_mq sedalg_mq
    real, Dimension(1,1000)         :: sedalg_mq
!> \anchor sedss_mq sedss_mq
    real, Dimension(1,1000)         :: sedss_mq

! mphyt Makrophyten
!>    \anchor pfldalg pfldalg unbenutzte Variable in mphyt()
      real :: pfldalg(1000)
!>    \anchor sa sa= \ref sonnenaufgang von sasu() in temperl_wetter() in update_weather() berechnet
!>    \anchor su su= \ref sonnenuntergang von sasu() in temperl_wetter() in update_weather() berechnet
!      real :: sa, su 
!>    \anchor itstart itstart = \ref starttag
!>    \anchor mstart mstart = \ref startmonat
!>    \anchor itmax itmax = \ref maxtag
!>    \anchor mmax mmax = \ref maxmonat
!>    \anchor itend itend = \ref endtag
!>    \anchor mend mend = \ref endmonat
      integer :: itstart,mstart,itmax,mmax,itend,mend

!! aus temperw_module.f95\n
!!<table >
!!<tr><th>Variablen-Name</th><th> Daten-Typ, Feld-Dimension	</th><th>	Beschreibung							</th></tr>
!!<tr><td>    RO	</td><td>	Real, Dimension(20)	</td><td>	relative Luftfeuchte in % (subroutine wettles)						</td></tr>
!!<tr><td>    TEMPL	</td><td>	real, Dimension(1000)	</td><td>	Lufttemperatur Subr. Temperl(...,TEMPL,...)			</td></tr>
!!<tr><td>    TEMPW	</td><td>	real, Dimension(1000)	</td><td>	Wassertemperatur in Grad Celsius						</td></tr>
!!<tr><td>    SCHWI	</td><td>	real, Dimension(1000)	</td><td>	Globalstrahlung in cal/(h*cm²))					</td></tr>
!!<tr><td>    WGE	</td><td>	Real, Dimension(20)	</td><td>	Windgeschwindigkeit (an einer der 20 möglichen Wetterstationen) im m/s	</td></tr>
!!
!!<tr><td>    TIEFE	</td><td>	real, Dimension(1000)	</td><td>	Wassertiefe	in m						</td></tr>
!!<tr><td>    TFLIE	</td><td>	real (implizit?)	</td><td>	FLIESSZEIT IN TAGEN 						</td></tr>
!!<tr><td>    vmitt	</td><td>	real, Dimension(1000)	</td><td>	mittlere Geschwindigkeit %%% hier unbenutzt			</td></tr>
!!<tr><td>    flag	</td><td>	integer, Dimension(1000)</td><td>	flag ob Einleitung						</td></tr>
!!<tr><td>    elen	</td><td>	real, Dimension(1000)	</td><td>	Länge des Elements						</td></tr>
!!
!!<tr><td>    ior	</td><td>	integer (implizit?)	</td><td>	Laufindex (Übergabe wozu?)					</td></tr>
!!<tr><td>    anze	</td><td>	integer			</td><td>	Anzahl Profile/Zellen in diesem Strang mstr !!!			</td></tr>
!!<tr><td>    etemp	</td><td>	real, Dimension(100)	</td><td>	Temperatur des Einleiters/Nebengewässers iein			</td></tr>
!!<tr><td>    ewaerm	</td><td>	real, Dimension(100)	</td><td>	Waermeeinleitung MJ/s (wird hier in Mcal/s umgerechnet "/4.2")	</td></tr>
!!<tr><td>    typ	</td><td>	integer, Dimension(100)	</td><td>	%%% hier unbenutzt						</td></tr>
!!
!!<tr><td>    qeinl	</td><td>	real, Dimension(100)	</td><td>	Abfluss Einleitung						</td></tr>
!!<tr><td>    vabfl	</td><td>	real, Dimension(1000)	</td><td>	Abfluss im Vorfluter m3/s					</td></tr>
!!<tr><td>    jiein	</td><td>	integer, Dimension(1000)</td><td>	Anzahl der Einleitungen						</td></tr>
!!<tr><td>    cloud	</td><td>	Real, Dimension(20)	</td><td>	Bewölkungsdichte in Achteln (an einer der 20 möglichen Wetterstationen)		</td></tr>
!!<tr><td>    typw	</td><td>	Real, Dimension(20)	</td><td>	Wolkentyp (an einer der 20 möglichen Wetterstationen)		</td></tr>
!!
!!<tr><td>    iwied	</td><td>	integer (implizit?)	</td><td>	%%% hier unbenutzt						</td></tr>
!!<tr><td>    uhrz	</td><td>	real (implizit?)	</td><td>	Uhrzeit ???							</td></tr>
!!<tr><td>    ilbuhn	</td><td>	integer (implizit?)	</td><td>	???								</td></tr>
!!<tr><td>    nwaerm	</td><td>	integer (implizit?)	</td><td>	%%% hier unbenutzt						</td></tr>
!!<tr><td>    fkm	</td><td>	real, Dimension(1000)	</td><td>	Fluss Km, wird nur an Subr. Dichte weitergegeben		</td></tr>
!!
!!<tr><td>    nkzs	</td><td>	integer, Dimension(1000)</td><td>	Anzahl Tiefenschichten						</td></tr>
!!<tr><td>    tempwz	</td><td>	real, Dimension(50,1000)</td><td>	Tiefenverteilung Wassertemperatur				</td></tr>
!!<tr><td>    dH2D	</td><td>	real (implizit?)	</td><td>	delta_z 							</td></tr>
!!<tr><td>    iorLa	</td><td>	integer, Dimension(100)	</td><td>	AnfangsKnoten der Linienquelle ieinL des Strangs mstr		</td></tr>
!!<tr><td>    iorLe	</td><td>	integer, Dimension(100)	</td><td>	EndKnoten der Linienquelle ieinL des Strangs mstr  		</td></tr>
!!
!!<tr><td>    ieinLs	</td><td>	integer, Dimension(50)	</td><td>	Anzahl der Linienquellen im Strang (mstr)			</td></tr>
!!<tr><td>    flae	</td><td>	real, Dimension(1000)	</td><td>	Querschnittsfläche des Gewässerkörpers				</td></tr>
!!<tr><td>    qeinlL	</td><td>	real, Dimension(100)	</td><td>	für Einleitung #						</td></tr>
!!<tr><td>    etempL	</td><td>	real, Dimension(100)	</td><td>	für Einleitung #						</td></tr>
!!<tr><td>    mstr	</td><td>	integer (implizit?)	</td><td>	Strangzähler     !!!						</td></tr>
!!
!!<tr><td>    IDWe	</td><td>	integer, Dimension(50,1000)</td><td>	Kennung der zuständigen Wetterstation (in diesem von 1000 möglichen Strängen für dieses Profil in dieser von 50 möglichen Tiefenschichten)	</td></tr>
!!<tr><td>    ilang	</td><td>	integer (implizit?)	</td><td>	%%% hier unbenutzt						</td></tr>
!!<tr><td>    dtemp	</td><td>	real, Dimension(50,1000)</td><td>	Temperaturänderung ???						</td></tr>
!!<tr><td>    FluxT1	</td><td>	real, Dimension(1000) 	</td><td>	???								</td></tr>
!!<tr><td>    extk	</td><td>	real, Dimension(1000)	</td><td>	Extinktionskoeffizient						</td></tr>
!!
!!<tr><td>    itags	</td><td>	integer (implizit?)	</td><td>	Tag im Monat							</td></tr>
!!<tr><td>    monats	</td><td>	integer (implizit?)	</td><td>	Monat im Jahr %%% hier unbenutzt				</td></tr>
!!<tr><td>    Tsed	</td><td>	real, Dimension(1000)	</td><td>	Sedimenttemperatur						</td></tr>
!!<tr><td>    Wlage	</td><td>	real, Dimension(50,1000)</td><td>	Höhenlage der zuständigen Wetterstation mü.NHN			</td></tr>
!!<tr><td>    hWS	</td><td>	real, Dimension(50,1000)</td><td>	Höhenlage des Wasserspiegels mü.NHN				</td></tr>
!!
!!<tr><td>    iRHKW	</td><td>	integer (implizit?)	</td><td>	%%% hier unbenutzt						</td></tr>
!!<tr><td>    htempw	</td><td>	real, Dimension(50,1000) 	</td><td>	?	</td></tr>
!!<tr><td>    htempz	</td><td>	real, Dimension(50,50,1000) 	</td><td>	?	</td></tr>
!!<tr><td>    dH2De	</td><td>	real, Dimension(1000) 	</td><td>	? 							</td></tr>
!!
!!</table>
!!
!! <h2>Rueckgabewerte/Resultate:</h2>
!! Der Ergebnisrückgabe dienen die folgenden beiden Variablen (-Felder)
!! <table >
!! <tr><td>TEMPW 	</td><td> tiefengemittelte Temperatur - im aktuellen Zeitschritt bis zu Knoten anze+1	</td></tr>
!! <tr><td>tempwz 	</td><td> Tiefenverteilung der Temperatur - 						</td></tr>
!! </table>
!!
!! <h2>Parameterliste</h2>
!!
!!<table >
!!<tr><th>	Name  </th><th> Beschreibung							</th></tr>
!!<tr><td>      VABFL </td><td>	Abfluss im Vorfluter m3/s					</td></tr>
!!<tr><td>      EL    </td><td>	Dampfdruck der Luft in mm Hg od. *1.3333 in mbar		</td></tr>
!!<tr><td>      EW    </td><td>	Dampfdruck des Wassers in mm Hg od. *1.3333 in mbar		</td></tr>
!!<tr><td>      stbk  </td><td>	Stefan-Boltzmann-Konstante in KJ/(m2*h*k**4) <br> (2.0411e-7)	</td></tr>
!!<tr><td>      SCHWI </td><td>	Globalstrahlung in cal/(cm2*h) 					</td></tr>
!!<tr><td>      A     </td><td>	Ausstrahlung							</td></tr>
!!<tr><td>      G     </td><td>	Gegenstrahlung							</td></tr>
!!<tr><td>      HR    </td><td>	Verdunstungshoehe in mm/d					</td></tr>
!!<tr><td>      VDW   </td><td>	Verdunstungswaerme in Kcal/Kg					</td></tr>
!!<tr><td>      WV    </td><td>	Waermestromdichte durch Verdunstung in cal/cm2/h		</td></tr>
!!<tr><td>      ROH2O </td><td>	Dichte des Wassers (1000.[Kg/m3])				</td></tr>
!!<tr><td>      SDDW  </td><td>	Saettigungsdampfdruck bei Wassertemperatur an der <br> Wasseroberflaeche [hPa]</td></tr>
!!<tr><td>      SDTT  </td><td>	Saettigungsdampfdruck bei Lufttemperatur [hPa]			</td></tr>
!!<tr><td>      PDLTT </td><td>	Partialdampfdruck der Luft bei der Temperatur TT [hPa]		</td></tr>
!!<tr><td>      speWKW</td><td>	spezifische Wärmekapazität des Wassers in KJ/(Kg*K)		</td></tr>
!!<tr><td>      speWKS</td><td>	spezifische Wärmekapazität des Sediments in KJ/(Kg*K)		</td></tr>
!!<tr><td>      rohS  </td><td>	Dichte des Sediments Kg/m3					</td></tr>
!!<tr><td>      WUEBK </td><td>	Wärmeübergangskoeffizient in KJ/(K*m2*h)			</td></tr>
!!<tr><td>      APARS </td><td>	Anteil PARS an der Globalstrahlung				</td></tr>
!!<tr><td>      EWAERM</td><td>	Waermeeinleitung MJ/s (wird hier in Mcal/s umgerechnet "/4.2")	</td></tr>
!!<tr><td>      ETEMP </td><td>	Temperatur des Einleiters/Nebengewässers iein			</td></tr>
!!<tr><td></td><td></td></tr>
!!<tr><td>      ieinLs</td><td>	Anzahl der Linienquellen im Strang (mstr)			</td></tr>
!!<tr><td>      ieinL </td><td>	Laufvariable der Linienquellen					</td></tr>
!!<tr><td>      iorLa </td><td>	AnfangsKnoten der Linienquelle ieinL des Strangs mstr		</td></tr>
!!<tr><td>      iorLe </td><td>	EndKnoten der Linienquelle ieinL des Strangs mstr  		</td></tr>
!!</table>
!!
!! <h2>bisherige Dimensionierung</h2>
!! temperw() wird fuer jeden Strang "mstr" aufgerufen und geht alle "anze" Zellen/Profile/Knoten durch.\n
!!<table >
!!<tr><th>Dimension   </th><th> 	Zweck			</th></tr>
!!<tr><td>	 1000 </td><td>	 	Knoten im Strang	</td></tr>
!!<tr><td>	   50 </td><td>		Straenge		</td></tr>
!!<tr><td>	   50 </td><td>		Tiefenschichten		</td></tr>
!!<tr><td>	  100 </td><td>		Einleitungen		</td></tr>
!!<tr><td>	   20 </td><td>		Wetterstationen		</td></tr>
!!</table>
!! \n\n


      PUBLIC :: ini_QSimDatenfelder
      CONTAINS 

!--------------------------------------------------------------------------------------------
      SUBROUTINE ini_QSimDatenfelder()
      logical kontroll

      ! Steuer-Parameter übergeben...
      ! algaeski so benutzen , dass nur der 1. Strang mit nur einem Knoten/Profil berechnet wird
      anze=1            ! Anzahl der Profile im aktuellen Strang
      mstr=1            ! Strangzähler
      ior=1             ! Laufindex
      flag(1)=0         ! keine Einleitungen
      flag(2)=flag(1)
      jiein(1)=0        ! null Punkt-Einleitungen
      jiein(2)=jiein(1)
      ilbuhn=0          ! keine Buhnen
      nkzs(1)=1         ! nur eine Tiefenschicht
      nkzs(2)=nkzs(1)
      dH2D=-2.0         ! bisher nur 2D-Tiefengemittelt ??? konstante Tiefenschichtung ???

      RETURN 
      END subroutine ini_QSimDatenfelder

      end module QSimDatenfelder
!
