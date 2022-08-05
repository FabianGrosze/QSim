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

subroutine modellg()
   use modell
   implicit none
   character(300) dateiname, text
   integer :: open_error, string_read_error, nzon, ion, izoni, alloc_status, n, i, ini
   integer :: tfolgt, rfolgt, ifolgt, knozoanz
   logical :: vorhanden, readable
   real :: anfangsKm, endKm, zonflae
   write(dateiname,'(2A)')trim(modellverzeichnis),'MODELLG.3D.txt'
   ion = 103
   open ( unit = ion , file = dateiname, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error MODELLG.3D.txt'
      call qerror(fehler)
   end if ! open_error.ne.0
   
   if (zeile(ion)) then
      print*,'MODELLG.3D.txt Versionskennzeichnung:',ctext(1:50)
   else
      write(fehler,*)'keine Versionskennzeichnung im Kopf von MODELLG.3D.txt'
      call qerror(fehler)
   end if
   if (zeile(ion)) then
      print*,'MODELLG.3D.txt Modellname:',ctext(1:50)
   else
      write(fehler,*)'keine Modellname im Kopf von MODELLG.3D.txt'
      call qerror(fehler)
   end if
   if (zeile(ion)) then !Zonenanzahl
      read(ctext, *, iostat = string_read_error ) zonen_anzahl
      if (string_read_error /= 0) then
         write(fehler,*)'string_read_error subroutine modellg nzon'
         call qerror(fehler)
      end if ! open_error.ne.0
      print*,'MODELLG.3D.txt:',zonen_anzahl, 'Zonen sollen drin sein'
   else
      write(fehler,*)'keine Zonenanzahl im Kopf von MODELLG.3D.txt'
      call qerror(fehler)
   end if !Zonenanzahl
   allocate (zone(zonen_anzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate zone failed in modellg() zonen.f95')
   zone(:)%zonen_nummer = 0 ! eingelesen Nummer der Zone
   zone(:)%ini_randnr = 0 ! Randnummer mit dem initialisiert wird
   zone(:)%nr_zone = 0 ! ?
   zone(:)%reib = 0.0 !
   zone(:)%sediflux%sedom = 0.0 ! Anteil des organischen Materials im Sediment
   zone(:)%sediflux%bedgs = 0.0 ! Bedeckungsgrad der Sohle mit Sediment (0-1)
   zone(:)%sediflux%sedvvert = 0.0 ! volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h
   zone(:)%sediflux%kornd = 0.0 ! Vorgabe Korndurchmesser d50
   zone(:)%sediflux%burial = 0.0 ! Burial-Geschwindigkeit (Sedimentation)
   zone(:)%seditemp%spewks = 0.0 ! Spez. WärmeKapazität Sediment" unit="KJ/(kg*K)
   zone(:)%seditemp%wuebk = 0.0 ! Wärmeübergangskoeffizient" unit="KJ/(K*m2*h)
   zone(:)%seditemp%psrefs = 0.0 ! Reflektionsanteil der Strahlung an der Sedimentoberfläche
   zone(:)%seditemp%extiks = 0.0 ! Extinktionskoeffizient für PARS
   zone(:)%laich%lait = 1 ! Tag des Beginns der Laichperiode
   zone(:)%laich%laim = 4 ! Monat des Beginns der Laichperiode
   zone(:)%laich%laid = 7 ! Dauer der Laichperiode in Tagen
   zone(:)%schiff%vschiff = 0.0 !
   zone(:)%schiff%uprop = 0.0 !
   zone(:)%schiff%schifffahrts_zone = 0 ! ; 1->Schiffsverkehr  , 0-> kein Schiffsverkehr; MODELLG.txt "F"
   zone(:)%wettstat%wetterstations_nummer = 0 ! zugehörige Wetterstation
   zone(:)%wettstat%wetterstations_lage = 0.0 ! Höhe ü. NHN
   zone(:)%dreissen%mboesch0 = 0.0 !
   zone(:)%dreissen%msohle0 = 0.0 !
   zone(:)%dreissen%gewicht0 = 0.0 !
   zone(:)%dreissen%mboesch1 = 0.0 !
   zone(:)%dreissen%msohle1 = 0.0 !
   zone(:)%dreissen%gewicht1 = 0.0 !
   zone(:)%dreissen%dreissena_aktiv = 0 !
   zone(:)%albenthi%ggruen = 0.0 !
   zone(:)%albenthi%gkiesel = 0.0 !
   zone(:)%macrophyt%starttag = 0 !
   zone(:)%macrophyt%startmonat = 0 !
   zone(:)%macrophyt%maxtag = 0 !
   zone(:)%macrophyt%maxmonat = 0 !
   zone(:)%macrophyt%endtag = 0 !
   zone(:)%macrophyt%endmonat = 0 !
   zone(:)%macrodicht%pflmin = 0.0 !
   zone(:)%macrodicht%pflmax = 0.0 !
   zone(:)%erosi%tau_krit = 9999.9
   zone(:)%erosi%M_eros = 0.0 !
   zone(:)%erosi%n_eros = 1.0 !
   zone(:)%erosi%sed_roh = 2650.0
   !     print*,'*** Lesen aus der Datei MODELLG.3D.txt ***'
   izoni = 0
   tfolgt = 1
   rfolgt = 1
   ifolgt = 1
   do while ( zeile(ion)) ! Datei zeilenweise lesen ...
      readable = .false.
      !if ((ctext(1:1).eq.'S').or.(ctext(1:1).eq.'s'))then
      !### if (ctext(1:1).eq.' ')then ! Leerzeile nächster Block nächste Zone ### mit Z-Sedflux zeile
      if ((ctext(1:1) == ' ') .or. ((ctext(1:1) == 'Z') .or. (ctext(1:1) == 'z'))) then ! Leer- oder Z-Zeile, nächster Block nächste Zone
         if ( .not. zeile(ion))exit ! erste Zeile im Block lesen
         izoni = izoni+1
         !zone(izoni)%schifffahrts_zone=0
         if (izoni > zonen_anzahl) then
            write(fehler,*)' zu viele Zonen in MODELLG.3D.txt; izoni zonen_anzahl', izoni, zonen_anzahl
            call qerror(fehler)
         end if
         read(ctext(2:2000), *, iostat = string_read_error ) zone(izoni)%zonen_nummer, zone(izoni)%zonen_name
         if (string_read_error /= 0) then
            print*,'Block-Anfangs-Zeile aus MODELLG.3D.txt nicht ganz gelesen; string_read_error = ',string_read_error
            print*,'zonen_nummer(',izoni,'), = ',zone(izoni)%zonen_nummer,' zonen_name = ',trim(zone(izoni)%zonen_name)
            call qerror('Lesefehler Z-Zeile MODELLG.3D.txt')
         else
            print*,'Block-Anfangs-Zeile aus MODELLG.3D.txt:',  &
            'zonen_nummer(',izoni,'), = ',zone(izoni)%zonen_nummer,' zonen_name = ',trim(zone(izoni)%zonen_name)
         end if !
         if (tfolgt /= 1)call qerror('Fehler in MODELLG.3D.txt ; jede Zone braucht genau eine Wetterstation !')
         if (rfolgt /= 1)call qerror('Fehler in MODELLG.3D.txt ; jede Zone braucht genau einen rauheitswert !')
         if (ifolgt /= 1)  &
             call qerror('Fehler in MODELLG.3D.txt ; jede Zone braucht genau eine Randnr. zur Initialisierung !')
         !print*,'Zuordnung Zonen-Wetterstationen aus MODELLG.3D.txt wird noch nirgendwo gespeichert modellg.f95'
         tfolgt = 0
         rfolgt = 0
         ifolgt = 0
      end if ! nächster Block
      !! T - Wetterstationszuordnung für o.g. Zone für die Temperaturberechnung \n
      if ((ctext(1:1) == 'T') .or. (ctext(1:1) == 't')) then
         read(ctext(2:2000), *, iostat = string_read_error ) &
              zone(izoni)%wettstat%wetterstations_nummer, zone(izoni)%wettstat%wetterstations_lage
         if (string_read_error /= 0) call qerror('Lesefehler T-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt:','zur ',izoni,'. Zone mit Nummer ',zone(izoni)%zonen_nummer, &
         ' gehört Wetterstation # ', zone(izoni)%wettstat%wetterstations_nummer, &
         ' in der Höhe über NHN ', zone(izoni)%wettstat%wetterstations_lage
         tfolgt = tfolgt+1
      end if ! T-Zeile
      !! R - Reibungsbeiwert "Rauheit" für o.g. Zone als ks-Wert nach Nikuradse (Sandrauheit) in m [neu in 3D]\n
      if ((ctext(1:1) == 'R') .or. (ctext(1:1) == 'r')) then
         read(ctext(2:2000), *, iostat = string_read_error ) &
              zone(izoni)%reib
         if (string_read_error /= 0) call qerror('Lesefehler R-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt: Sand-Rauheit ',zone(izoni)%reib,' in m zur '  &
         ,izoni,'. Zone mit Nummer ',zone(izoni)%zonen_nummer
         if (zone(izoni)%reib <= 0.0)call qerror('Reibungsbeiwert unzulässig')
         rfolgt = rfolgt+1
      end if ! R-Zeile
      !! I - Initialisierung für o.g. Zone mittels Randbedingung, Angabe der Randnummer [neu in 3D]\n
      if ((ctext(1:1) == 'I') .or. (ctext(1:1) == 'i')) then
         read(ctext(2:2000), *, iostat = string_read_error ) zone(izoni)%ini_randnr
         if (string_read_error /= 0) call qerror('Lesefehler I-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt:','Zone ',izoni,' mit Nummer ',zone(izoni)%zonen_nummer &
         ,' wird initialisiert von Randbedingung Nr. ',zone(izoni)%ini_randnr
         ifolgt = ifolgt+1
      end if ! I-Zeile
      !! F - schifffahrts_zone
      if ((ctext(1:1) == 'F') .or. (ctext(1:1) == 'f')) then
         zone(izoni)%schiff%schifffahrts_zone = 1  ! ; 1->Schiffsverkehr  , 0-> kein Schiffsverkehr
         read(ctext(2:2000), *, iostat = string_read_error )  &
              zone(izoni)%schiff%vschiff, zone(izoni)%schiff%uprop
         if (string_read_error /= 0) call qerror('Lesefehler F-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt: schifffahrt in Zone', izoni,' vschiff, uprop = '  &
               ,zone(izoni)%schiff%vschiff,zone(izoni)%schiff%uprop
      end if ! F-Zeile
      !! O - Verschattung durch Uferbewuchs Anteil der Vegetationstypen
      if ((ctext(1:1) == 'O') .or. (ctext(1:1) == 'o')) then
         print*,'ACHTUNG: O-Zeile in MODELLG.3D.txt'
         write(fehler,*)'### Warnung #### Verschattung durch Uferbewuchs wird in QSim3D noch nicht implementiert.'
         call qerror(fehler)
         !            read(ctext(2:2000), *, iostat = string_read_error )         &
         !     &      aVeg(mstr,mV),eVeg(mstr,mV),(VTYPA(mstr,mV,iV)             &
         !     &      ,iV=1,6),VALTAL(mstr,mV),EDUFAL(mstr,mV)                        &
         !     &      ,(VTYPA(mstr,mV,iV),iV=7,12),VALTAR(mstr,mV),EDUFAR(mstr,mV)    &
         !     &      ,(VTYPA(mstr,mV,iV),iV=13,14)
      end if ! O-Zeile
      !! S - Kenngrössen für Temperatur/Sedimenttemperatur
      !WRITE(1, '(A)') '<ParamSetDef id="QS" text="Kenngrössen für Temperatur/Sedimenttemperatur" help="Kenngrößen für die Gewässerabschnitten" scope="Abschnitt">'
      !WRITE(1, '(A)') ' <Parameter ident="SPEWKS" text="Spez. WärmeKapazität Sediment" unit="KJ/(kg*K)" format="F6.2" null="-1" help="Ton: 0.83; Sand: 0.88" min="0.8" max="4.5" default="-1">'
      !WRITE(1, '(A)') ' <Parameter ident="WUEBK" text="Wärmeübergangskoeffizient" unit="KJ/(K*m2*h)" format="F7.2" null="-1" help="" min="0" max="1000" default="-1.">'
      !WRITE(1, '(A)') ' <Parameter ident="PSREFS" text="Reflektionsanteil der Strahlung an der Sedimentoberfläche" unit="-" format="F5.2" null="-1" help="" min="0" max="1" default="-1.">'
      !WRITE(1, '(A)') ' <Parameter ident="EXTKS" text="Extinktionskoeffizient für PARS (nur bei Temperaturmodellierung erforderlich!)" unit="-" format="F5.2" null="-1" help="" min="" max="" default="-1.">'
      !read(77,1047)aKSED(mstr,mA),eKSED(mstr,mA),SPEWKSx(mstr,mA),WUEBKx(mstr,mA),PSREFSx(mstr,mA),extkx(mstr,mA)
      if ((ctext(1:1) == 'S') .or. (ctext(1:1) == 's')) then
         read(ctext(2:2000), *, iostat = string_read_error ) &
              zone(izoni)%seditemp%spewks, zone(izoni)%seditemp%wuebk,  &
              zone(izoni)%seditemp%psrefs, zone(izoni)%seditemp%extiks
         if (string_read_error /= 0) call qerror('Lesefehler S-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt:','Sedimenttemperatur_zone zonen_nummer(izoni)', zone(izoni)%zonen_nummer, izoni
         print*,'spewks,wuebk,psrefs,extiks',  &
         zone(izoni)%seditemp%spewks, zone(izoni)%seditemp%wuebk,  &
         zone(izoni)%seditemp%psrefs, zone(izoni)%seditemp%extiks
         readable = .true.
      end if ! S-Zeile
      !! Z - Sediment-Kenngrößen, Belegungen für Stoffumsatz im Sediment
      !!###      read(77,1045)aPOM(mstr,mZ),ePOM(mstr,mZ),POMz(mstr,mZ),BedGSz(mstr,mz),Sedvvertz(mstr,mz)
      !!      zone(:)%sediflux%sedom=0.0 ! Anteil des organischen Materials im Sediment
      !!      zone(:)%sediflux%bedgs=0.0 ! Bedeckungsgrad der Sohle mit Sediment (0-1)
      !!      zone(:)%sediflux%sedvvert=0.0 ! volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h
      !!      zone(:)%sediflux%kornd=0.0 ! Vorgabe Korndurchmesser d50
      !!      zone(:)%sediflux%burial=0.0 ! Burial-Geschwindigkeit (Sedimentation)
      !         if ((ctext(1:1).eq.'Z').or.(ctext(1:1).eq.'z'))then
      !            read(ctext(2:2000), *, iostat = string_read_error )  &
      !     &          zone(izoni)%sediflux%sedom,zone(izoni)%sediflux%bedgs,zone(izoni)%sediflux%sedvvert
      !            if(string_read_error.ne.0)then
      !               call qerror('Lesefehler Z-Zeile MODELLG.3D.txt')
      !            end if ! Lesefehler Z-Zeile
      !            readable=.true.
      !            ! optional Korndurchmesser und burialgeschwindigkeit
      !            read(ctext(2:2000), *, iostat = string_read_error ) dummy,dummy,dummy,dummy,dummy,kornd(izoni)
      !            if(string_read_error.ne. 0) kornd(izoni)=0.0
      !            read(ctext(2:2000), *, iostat = string_read_error ) dummy,dummy,dummy,dummy,dummy,dummy,burial(izoni)
      !            if(string_read_error.ne. 0) burial(izoni)=0.0
      !            print*,'Z-Zeile in MODELLG.3D.txt: ,izoni,sedom,bedgs,sedvvert,kornd,burial='  &
      !     &            ,izoni,sedom(izoni),bedgs(izoni),sedvvert(izoni),kornd(izoni),burial(izoni)
      !         end if ! Z-Zeile
      !! L - Kenngrössen für Laichperiode Muscheln Dreissena
      !! read(77,2306)laits(mstr),laims(mstr),laids(mstr)
      if ((ctext(1:1) == 'L') .or. (ctext(1:1) == 'l')) then
         read(ctext(2:2000), *, iostat = string_read_error ) &
              zone(izoni)%laich%lait, zone(izoni)%laich%laim, zone(izoni)%laich%laid
         if (string_read_error /= 0) call qerror('Lesefehler L-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt:','Laichperiode zonen_nummer(izoni)', zone(izoni)%zonen_nummer, izoni
         print*,'lait, laim, laid',zone(izoni)%laich%lait, zone(izoni)%laich%laim, zone(izoni)%laich%laid
      end if ! L-Zeile
      !! D - Dreissena-Bewuchs in den Gewässer-Abschnitten
      !  WRITE(1, '(A)') '<ParamSetDef Id="QD" Text="Dreissena" Help="Dreissena-Bewuchs in den Gewässer-Abschnitten" Scope="Abschnitt">'
      !  WRITE(1, '(A)') '  <Parameter Ident="mboesch0" Text="Biomasse 0.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte (Schalenlänge kl. 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />'
      !  WRITE(1, '(A)') '  <Parameter Ident="msohle0" Text="Biomasse 0.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />'
      !  WRITE(1, '(A)') '  <Parameter Ident="gewicht0" Text="Mittl. Muschelgewicht 0.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 0. Kohorte" Min="" Max="" Default="" />'
      !  WRITE(1, '(A)') '  <Parameter Ident="mboesch1" Text="Biomasse 1.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte (Schalenlänge gr.= 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />'
      !  WRITE(1, '(A)') '  <Parameter Ident="msohle1" Text="Biomasse 1.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />'
      !  WRITE(1, '(A)') '  <Parameter Ident="gewicht1" Text="Mittl. Muschelgewicht 1.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 1. Kohorte." Min="" Max="" Default="" />'
      if ((ctext(1:1) == 'D') .or. (ctext(1:1) == 'd')) then
         read(ctext(2:2000), *, iostat = string_read_error )  &
              zone(izoni)%dreissen%mboesch0, zone(izoni)%dreissen%msohle0, zone(izoni)%dreissen%gewicht0   &
              ,zone(izoni)%dreissen%mboesch1, zone(izoni)%dreissen%msohle1, zone(izoni)%dreissen%gewicht1
         if (string_read_error /= 0) call qerror('Lesefehler D-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt: Dreissena-Bewuchs in zonen_nummer(izoni)', zone(izoni)%zonen_nummer, izoni    &
         ,'msohle0,gewicht0,msohle1,gewicht1',zone(izoni)%dreissen%msohle0, zone(izoni)%dreissen%gewicht0   &
         ,zone(izoni)%dreissen%msohle1, zone(izoni)%dreissen%gewicht1
         if ( (zone(izoni)%dreissen%msohle0+zone(izoni)%dreissen%msohle1) > 0.0) zone(izoni)%dreissen%dreissena_aktiv = 1 ! muscheln nur aktiv wenn vorbelegt
      end if ! D-Zeile
      !! B Benthische Algen
      !! subroutine ModellGParam(cpfad1,j1)\n
      !!  WRITE(1, '(A)') '<ParamSetDef Id="QB" Text="Benth.Algen" Help="Benth.Algen-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt"\n
      !!  WRITE(1, '(A)') '  <Parameter Ident="GGruen" Text="Gewicht Grünalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Grünalgen \n
      !!  WRITE(1, '(A)') '  <Parameter Ident="GKiesel" Text="Gewicht Kieselalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Kieselalgen \n
      if ((ctext(1:1) == 'B') .or. (ctext(1:1) == 'b')) then
         read(ctext(2:2000), *, iostat = string_read_error )  &
              zone(izoni)%albenthi%ggruen,zone(izoni)%albenthi%gkiesel
         if (string_read_error /= 0) call qerror('Lesefehler B-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt: Benthische Algen in Zone', izoni,' albenthi%ggruen , albenthi%gkiesel = '  &
               ,zone(izoni)%albenthi%ggruen,zone(izoni)%albenthi%gkiesel
      end if ! B-Zeile
      !! M  Makrophyten
      !!  WRITE(1, '(A)') '<ParamSetDef Id="QM" Text="Makrophyten" Help="Makrophyten-Wachstum" Scope="Strang">'
      !!  WRITE(1, '(A)') '  <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Wachstumsbeginns der Makrophyten" Min="1" Max="31" Default="" />'
      !!  WRITE(1, '(A)') '  <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Wachstumsbeginns der Makrophyten" Min="1" Max="12" Default="" />'
      !!  WRITE(1, '(A)') '  <Parameter Ident="MaxTag" Text="Max.-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="31" Default="" />'
      !!  WRITE(1, '(A)') '  <Parameter Ident="MaxMonat" Text="Max.-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="12" Default="" />'
      !!  WRITE(1, '(A)') '  <Parameter Ident="EndTag" Text="Ende-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Minimum erreicht hat. Hier endet das Makrophytenwachstum" Min="1" Max="31" Default="" />'
      !!  WRITE(1, '(A)') '  <Parameter Ident="EndMonat" Text="Ende-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Minimum erreicht hat" Min="1" Max="12" Default="" />'
      if ((ctext(1:1) == 'M') .or. (ctext(1:1) == 'm')) then
         read(ctext(2:2000), *, iostat = string_read_error )  &
              zone(izoni)%macrophyt%starttag,zone(izoni)%macrophyt%startmonat,  &
              zone(izoni)%macrophyt%maxtag,zone(izoni)%macrophyt%maxmonat,  &
              zone(izoni)%macrophyt%endtag,zone(izoni)%macrophyt%endmonat
         if (string_read_error /= 0) call qerror('Lesefehler M-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt: Makrophyten-Wachstum in Zone', izoni,' maxtag = '  &
                                                                                 ,zone(izoni)%macrophyt%maxtag
      end if ! M-Zeile
      !! P Dichte der Makrophyten
      !!  WRITE(1, '(A)') '<ParamSetDef Id="QP" Text="Dichte der Makrophyten" Help="Makrophyten-Dichte" Scope="Abschnitt \n
      !!  WRITE(1, '(A)') '  <Parameter Ident="PflMin" Text="min. Dichte (Winter)" Unit="g/m²" Format="F7.2" Null="-1" Help="Minimale Dichte der Makrophyten im Winter" \n
      !!  WRITE(1, '(A)') '  <Parameter Ident="PflMax" Text="max. Dichte (Sommer)" Unit="g/m²" Format="F7.2" Null="-1" Help="Maximale Dichte der Makrophyten im Sommer" \n
      if ((ctext(1:1) == 'P') .or. (ctext(1:1) == 'p')) then
         read(ctext(2:2000), *, iostat = string_read_error )  &
              zone(izoni)%macrodicht%pflmin,zone(izoni)%macrodicht%pflmax
         if (string_read_error /= 0) call qerror('Lesefehler P-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt: Dichte der Makrophyten in Zone', izoni,' %macrodicht%pflmin , %macrodicht%pflmax = '  &
               ,zone(izoni)%macrodicht%pflmin,zone(izoni)%macrodicht%pflmax
      end if ! P-Zeile
      
      !! E Erosionsparameter
      !!  WRITE(1, '(A)') '<ParamSetDef Id="QE" Text="Erosions-Parameter" Help="Kenngrößen für die Gewässerabschnitte" Scope="Abschnitt">'
      !!  WRITE(1, '(A)') '  <Parameter Ident="tau_krit" Text="kritische Sohlschubspannung ab der Erosion auftritt"       Unit="N/m²"      Format="F7.3" Null="-1" Help="" Max="" Default="9999.99" />'
      !!  WRITE(1, '(A)') '  <Parameter Ident="M_eros"   Text="Erodibilitätskonstante"                                    Unit="kg/(m²*s)" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="0." />'
      !!  WRITE(1, '(A)') '  <Parameter Ident="n_eros"   Text="Exponent in der Erosionsformel, potenziert den relativen Sohlspannungsüberschuss" Unit="-" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="1." />'
      !!  WRITE(1, '(A)') '  <Parameter Ident="sed_roh"  Text="Dichte des liegenden Sediments"                            Unit="kg/m³"     Format="F7.3" Null="-1" Help="" Min="" Max="" Default="2650.0" />'
      !!  WRITE(1, '(A)') '</ParamSetDef>'
      if ((ctext(1:1) == 'E') .or. (ctext(1:1) == 'e')) then
         read(ctext(2:2000), *, iostat = string_read_error )  &
              zone(izoni)%erosi%tau_krit, zone(izoni)%erosi%M_eros, zone(izoni)%erosi%n_eros, zone(izoni)%erosi%sed_roh
         if (string_read_error /= 0) call qerror('Lesefehler E-Zeile MODELLG.3D.txt')
         print*,'MODELLG.3D.txt: Erosionsparameter in Zone', izoni,' tau_krit, M_eros,  n_eros,  sed_roh = '  &
               ,zone(izoni)%erosi%tau_krit, zone(izoni)%erosi%M_eros, zone(izoni)%erosi%n_eros, zone(izoni)%erosi%sed_roh
      end if ! e-Zeile
   end do ! while(zeile(ion))
   close (ion)
   if (izoni /= zonen_anzahl) then
      !zonen_anzahl=izoni
      write(fehler,*)'Zonen-Anzahl',izoni,' ungleich der im Kopf von MODELLG.3D.txt angegeben ',zonen_anzahl
      call qerror(fehler)
   end if
   do i = 1,zonen_anzahl
      do n = i+1,zonen_anzahl
         if ( zone(i)%zonen_nummer == zone(n)%zonen_nummer) then
            write(fehler,*)' 8 Zonennummer von Zone',i ,' = ',zone(i)%zonen_nummer,' ist gleich Zonennummer von Zone',n
            call qerror(fehler)
         endif
      end do ! alle n Zonen
      if (zone(i)%reib <= 0.0) then
         write(fehler,*)'Reibungsbeiwert',zone(i)%reib,' von Zone',i,' ist falsch '
         call qerror(fehler)
      endif
   end do ! alle i Zonen
   do n = 1,number_plankt_point
      vorhanden = .false.
      do i = 1,zonen_anzahl
         if ( point_zone(n) == zone(i)%zonen_nummer ) then
            if ( .not. vorhanden)point_zone(n) = i
            vorhanden = .true. ! zone vorhanden + zugeordnet
         end if !zonen_nummer vorhanden
      end do ! alle i Zonen
      if ( .not. vorhanden) then
         write(fehler,*)'2 Die von Knoten #',n ,' benötigte Zonennummer #',point_zone(n)  &
               , 'ist nicht in MODELLG.3D.txt beschrieben'
         call qerror(fehler)
      end if ! nicht vorhanden
   end do ! alle n Knoten
   print*,'MODELLG.3D.txt: an allen Knoten wurde die Zonennummer'  &
   ,' in den Zonenzähler korrekt umgewandelt (point_zone()).'
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         do i = 1,zonen_anzahl
            knozoanz = 0
            zonflae = 0.0
            do n = 1,knotenanzahl2D
               if (knoten_zone(n) == i) then
                  knozoanz = knozoanz+1
                  zonflae = zonflae+knoten_flaeche(n)
               endif ! knoten in zone
            end do ! alle n Knoten
            print*,'MODELLG.3D.txt: Die ',i,'-te Zone hat die Nummer ',zone(i)%zonen_nummer   &
            ,'heißt: ',trim(zone(i)%zonen_name)  &
            ,', enthält ',knozoanz,' Knoten und bedeckt eine Fläche von ',zonflae, ' m**2'
         end do ! alle i Zonen
      case(2) ! Untrim² netCDF
         do i = 1,zonen_anzahl
            knozoanz = 0
            do n = 1,n_elemente
               if (element_zone(n) == i) knozoanz = knozoanz+1
            end do ! alle n Elemente
            print*,'MODELLG.3D.txt: Die ',i,'-te Zone hat die Nummer ',zone(i)%zonen_nummer   &
            ,'heißt: ',trim(zone(i)%zonen_name)  &
            ,' und enthält ',knozoanz,' Elemente.'
         end do ! alle i Zonen
      case(3) ! SCHISM netCDF
         do i = 1,zonen_anzahl
            knozoanz = 0
            zonflae = 0.0
            do n = 1,knotenanzahl2D
               if (knoten_zone(n) == i) then
                  knozoanz = knozoanz+1
                  zonflae = zonflae+knoten_flaeche(n)
               endif ! knoten in zone
            end do ! alle n Knoten
            print*,'MODELLG.3D.txt: Die ',i,'-te Zone hat die Nummer ',zone(i)%zonen_nummer   &
            ,'heißt: ',trim(zone(i)%zonen_name)  &
            ,', enthält ',knozoanz,' Knoten und bedeckt eine Fläche von ',zonflae, ' m**2'
         end do ! alle i Zonen
         !!!### call sc_read_rough()
         case default
         call qerror('modellg Hydraulischer Antrieb unbekannt')
   end select
   return
end subroutine modellg
!----+-----+----
!
