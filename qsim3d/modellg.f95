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
   
   character(300) :: filename, text
   integer        :: open_error, read_error, nzon, ion, izoni, alloc_status, n, i, ini
   integer        :: tfolgt, rfolgt, ifolgt, knozoanz
   logical        :: vorhanden
   real           :: anfangsKm, endKm, zonflae
   character      :: token
   
   filename = trim(modellverzeichnis) // 'MODELLG.3D.txt'
   open (newunit = ion , file = filename, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0) call qerror("Could not open " // trim(filename))
   
   print*
   print "(a)", repeat("-", 80)
   print "(a)", "ModellG.3D.txt"
   print "(a)", repeat("-", 80)

   
   ! -------------------------------------------------------------------------
   ! read file header
   ! -------------------------------------------------------------------------
   if (.not. zeile(ion)) call qerror('keine Versionskennzeichnung im Kopf von MODELLG.3D.txt')
   print "(2a)", 'version: ', ctext(1:50)
   
   if (.not. zeile(ion)) call qerror('keine Modellname im Kopf von MODELLG.3D.txt')
   print "(2a)",'model:   ', ctext(1:50)
   
   ! total zone number
   if (.not. zeile(ion)) call qerror('keine Zonenanzahl im Kopf von MODELLG.3D.txt')
   read(ctext, *, iostat = read_error ) zonen_anzahl
   
   if (read_error /= 0) call qerror('read_error subroutine modellg nzon')
   print "(a,i0)", "number of zones = ", zonen_anzahl
   
   ! -------------------------------------------------------------------------
   ! initialize zone objects
   ! -------------------------------------------------------------------------
   allocate (zone(zonen_anzahl), stat = alloc_status)
   if (alloc_status /= 0) call qerror('allocate zone failed in modellg() zonen.f95')
   
   zone(:) % zonen_nummer = 0   ! eingelesen Nummer der Zone
   zone(:) % ini_randnr   = 0   ! Randnummer mit dem initialisiert wird
   zone(:) % nr_zone      = 0 
   zone(:) % reib         = 0.0 
   
   zone(:) % sediflux % sedom    = 0.0 ! Anteil des organischen Materials im Sediment
   zone(:) % sediflux % bedgs    = 0.0 ! Bedeckungsgrad der Sohle mit Sediment (0-1)
   zone(:) % sediflux % sedvvert = 0.0 ! volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h
   zone(:) % sediflux % kornd    = 0.0 ! Vorgabe Korndurchmesser d50
   zone(:) % sediflux % burial   = 0.0 ! Burial-Geschwindigkeit (Sedimentation)
   zone(:) % seditemp % spewks   = 0.0 ! Spez. WärmeKapazität Sediment" unit="KJ/(kg*K)
   zone(:) % seditemp % wuebk    = 0.0 ! Wärmeübergangskoeffizient" unit="KJ/(K*m2*h)
   zone(:) % seditemp % psrefs   = 0.0 ! Reflektionsanteil der Strahlung an der Sedimentoberfläche
   zone(:) % seditemp % extiks   = 0.0 ! Extinktionskoeffizient für PARS
   
   zone(:) % laich % lait = 1 ! Tag des Beginns der Laichperiode
   zone(:) % laich % laim = 4 ! Monat des Beginns der Laichperiode
   zone(:) % laich % laid = 7 ! Dauer der Laichperiode in Tagen
   
   zone(:) % schiff % vschiff           = 0.0 !
   zone(:) % schiff % uprop             = 0.0 !
   zone(:) % schiff % schifffahrts_zone = 0   ! 1) Schiffsverkehr  0) kein Schiffsverkehr
   
   zone(:) % wettstat % wetterstations_nummer = 0   ! zugehörige Wetterstation
   zone(:) % wettstat % wetterstations_lage   = 0.0 ! Höhe ü. NHN
   
   zone(:) % dreissen % mboesch0 = 0.0 
   zone(:) % dreissen % msohle0  = 0.0
   zone(:) % dreissen % gewicht0 = 0.0
   zone(:) % dreissen % mboesch1 = 0.0
   zone(:) % dreissen % msohle1  = 0.0
   zone(:) % dreissen % gewicht1 = 0.0
   zone(:) % dreissen % dreissena_aktiv = 0
   
   zone(:) % albenthi % ggruen  = 0.0
   zone(:) % albenthi % gkiesel = 0.0
   
   zone(:) % macrophyt % starttag   = 0
   zone(:) % macrophyt % startmonat = 0
   zone(:) % macrophyt % maxtag     = 0
   zone(:) % macrophyt % maxmonat   = 0
   zone(:) % macrophyt % endtag     = 0
   zone(:) % macrophyt % endmonat   = 0
   
   zone(:) % macrodicht % pflmin    = 0.0
   zone(:) % macrodicht % pflmax    = 0.0
   
   zone(:) % erosi % tau_krit = 9999.9 
   zone(:) % erosi % M_eros   = 0.0
   zone(:) % erosi % n_eros   = 1.0
   zone(:) % erosi % sed_roh  = 2650.0
   
   
   ! --------------------------------------------------------------------------
   ! read data
   ! --------------------------------------------------------------------------
   izoni  = 0
   tfolgt = 1
   rfolgt = 1
   ifolgt = 1
   
   do while (zeile(ion))
      token = ctext(1:1)
      
      select case(token)
         case(" ", "Z", "z")
            ! Leer- oder Z-Zeile, nächster Block nächste Zone
            if ( .not. zeile(ion))exit ! erste Zeile im Block lesen
            izoni = izoni+1
      
            if (izoni > zonen_anzahl) then
               write(fehler,*)' zu viele Zonen in MODELLG.3D.txt; izoni zonen_anzahl', izoni, zonen_anzahl
               call qerror(fehler)
            endif
            
            read(ctext(2:2000), *, iostat = read_error ) zone(izoni) % zonen_nummer, zone(izoni) % zonen_name
            if (read_error /= 0) call qerror('Lesefehler Z-Zeile MODELLG.3D.txt')
            
            print*
            print "(i0,a,i0,2a)", izoni, ") zone ", zone(izoni) % zonen_nummer, ": ", trim(zone(izoni) % zonen_name)
            
            if (tfolgt /= 1) call qerror('Fehler in MODELLG.3D.txt ; jede Zone braucht genau eine Wetterstation.')
            if (rfolgt /= 1) call qerror('Fehler in MODELLG.3D.txt ; jede Zone braucht genau einen Rauheitswert.')
            if (ifolgt /= 1) call qerror('Fehler in MODELLG.3D.txt ; jede Zone braucht genau eine Randnummer zur Initialisierung.')

            tfolgt = 0
            rfolgt = 0
            ifolgt = 0

         ! --- weather station ---
         case('T', 't')
            read(ctext(2:2000), *, iostat = read_error )  &
               zone(izoni) % wettstat % wetterstations_nummer, &
               zone(izoni) % wettstat % wetterstations_lage
            if (read_error /= 0) call qerror('Lesefehler T-Zeile MODELLG.3D.txt')
            
            print "(3x,a)", "* weather station"
            print "(10x,a,i0)",   "station = ", zone(izoni) % wettstat % wetterstations_nummer
            print "(10x,a,f0.3)", "height  = ", zone(izoni) % wettstat % wetterstations_lage
            tfolgt = tfolgt+1
            
         
         case('R', 'r')
            ! R - Reibungsbeiwert "Rauheit" für o.g. Zone als ks-Wert nach Nikuradse (Sandrauheit) in m [neu in 3D]
            read(ctext(2:2000), *, iostat = read_error ) zone(izoni) % reib
            if (read_error /= 0) call qerror('Lesefehler R-Zeile MODELLG.3D.txt')
            if (zone(izoni) % reib <= 0.0)call qerror('Reibungsbeiwert unzulässig')
            rfolgt = rfolgt + 1
            
            print "(3x,a)",      "* friction"
            print "(10x,a,f0.3)", "coefficient =  ", zone(izoni) % reib
         
         case('I', 'i')
            ! I - Initialisierung für o.g. Zone mittels Randbedingung, Angabe der Randnummer [neu in 3D]
            read(ctext(2:2000), *, iostat = read_error ) zone(izoni) % ini_randnr
            if (read_error /= 0) call qerror('Lesefehler I-Zeile MODELLG.3D.txt')
            
            print "(3x,a)", "* initializing boundary"
            print "(10x,a,i0)", "boundary id = ", zone(izoni) % ini_randnr
            ifolgt = ifolgt+1
         
         
         case('F', 'f')
            ! F - schifffahrts_zone
            zone(izoni) % schiff % schifffahrts_zone = 1 
            read(ctext(2:2000), *, iostat = read_error)  &
               zone(izoni) % schiff % vschiff,        &
               zone(izoni) % schiff % uprop
            
            if (read_error /= 0) call qerror('Lesefehler F-Zeile MODELLG.3D.txt')
            
            print "(3x,a)",    "shipping traffic"
            print "(10x,a,f0.3)", "vschiff = ",  zone(izoni) % schiff % vschiff
            print "(10x,a,f0.3)", "uprop   = ",    zone(izoni) % schiff % uprop
         
         case('O', 'o')
            ! O - Verschattung durch Uferbewuchs Anteil der Vegetationstypen
            print*,'ACHTUNG: O-Zeile in MODELLG.3D.txt'
            write(fehler,*)'### Warnung #### Verschattung durch Uferbewuchs wird in QSim3D noch nicht implementiert.'
            call qerror(fehler)
         
         
         case('S', 's')
            ! S - Kenngrössen für Temperatur/Sedimenttemperatur
            read(ctext(2:2000), *, iostat = read_error ) &
               zone(izoni) % seditemp % spewks, zone(izoni) % seditemp % wuebk,  &
               zone(izoni) % seditemp % psrefs, zone(izoni) % seditemp % extiks
            if (read_error /= 0) call qerror('Lesefehler S-Zeile MODELLG.3D.txt')
            
            print "(3x,a)",      "* sediment"
            print "(10x,a,f0.3)", 'spewks = ', zone(izoni) % seditemp % spewks
            print "(10x,a,f0.3)", 'wuebk  = ',  zone(izoni) % seditemp % wuebk
            print "(10x,a,f0.3)", 'psrefs = ', zone(izoni) % seditemp % psrefs
            print "(10x,a,f0.3)", 'extiks = ', zone(izoni) % seditemp % extiks
               
         
         
         ! case  ('Z', 'z')
            ! Z - Sediment-Kenngrößen, Belegungen für Stoffumsatz im Sediment
            !!###      read(77,1045)aPOM(mstr,mZ),ePOM(mstr,mZ),POMz(mstr,mZ),BedGSz(mstr,mz),Sedvvertz(mstr,mz)
            !!      zone(:) % sediflux % sedom=0.0 ! Anteil des organischen Materials im Sediment
            !!      zone(:) % sediflux % bedgs=0.0 ! Bedeckungsgrad der Sohle mit Sediment (0-1)
            !!      zone(:) % sediflux % sedvvert=0.0 ! volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h
            !!      zone(:) % sediflux % kornd=0.0 ! Vorgabe Korndurchmesser d50
            !!      zone(:) % sediflux % burial=0.0 ! Burial-Geschwindigkeit (Sedimentation)
            !         if ((ctext(1:1).eq.'Z').or.(ctext(1:1).eq.'z'))then
            !            read(ctext(2:2000), *, iostat = read_error )  &
            !     &          zone(izoni) % sediflux % sedom,zone(izoni) % sediflux % bedgs,zone(izoni) % sediflux % sedvvert
            !            if(read_error.ne.0)then
            !               call qerror('Lesefehler Z-Zeile MODELLG.3D.txt')
            !            endif ! Lesefehler Z-Zeile
            !            ! optional Korndurchmesser und burialgeschwindigkeit
            !            read(ctext(2:2000), *, iostat = read_error ) dummy,dummy,dummy,dummy,dummy,kornd(izoni)
            !            if(read_error.ne. 0) kornd(izoni)=0.0
            !            read(ctext(2:2000), *, iostat = read_error ) dummy,dummy,dummy,dummy,dummy,dummy,burial(izoni)
            !            if(read_error.ne. 0) burial(izoni)=0.0
            !            print*,'Z-Zeile in MODELLG.3D.txt: ,izoni,sedom,bedgs,sedvvert,kornd,burial='  &
            !     &            ,izoni,sedom(izoni),bedgs(izoni),sedvvert(izoni),kornd(izoni),burial(izoni)
            !         endif ! Z-Zeile
      
         
         case('L', 'l')
            ! L - Kenngrössen für Laichperiode Muscheln Dreissena
            read(ctext(2:2000), *, iostat = read_error ) &
              zone(izoni) % laich % lait, zone(izoni) % laich % laim, zone(izoni) % laich % laid
            if (read_error /= 0) call qerror('Lesefehler L-Zeile MODELLG.3D.txt')
            
            print "(3x,a)",    "* dreissena spawning"
            print "(10x,a,i0)", 'lait = ', zone(izoni) % laich % lait
            print "(10x,a,i0)", 'laim = ', zone(izoni) % laich % laim
            print "(10x,a,i0)", 'laid = ', zone(izoni) % laich % laid
      
         
         case('D', 'd')
            ! D - Dreissena-Bewuchs in den Gewässer-Abschnitten
            read(ctext(2:2000), *, iostat = read_error )  &
               zone(izoni) % dreissen % mboesch0, zone(izoni) % dreissen % msohle0, zone(izoni) % dreissen % gewicht0   &
              ,zone(izoni) % dreissen % mboesch1, zone(izoni) % dreissen % msohle1, zone(izoni) % dreissen % gewicht1
            if (read_error /= 0) call qerror('Lesefehler D-Zeile MODELLG.3D.txt')
         
            if ((zone(izoni) % dreissen % msohle0+zone(izoni) % dreissen % msohle1) > 0.0) then
               ! muscheln nur aktiv wenn vorbelegt
               zone(izoni) % dreissen % dreissena_aktiv = 1 
            endif
            
            print "(3x,a)",      "* dreissena growth"
            print "(10x,a,f0.3)", "msohle0  = ", zone(izoni) % dreissen % msohle0
            print "(10x,a,f0.3)", "gewicht0 = ", zone(izoni) % dreissen % gewicht0   
            print "(10x,a,f0.3)", "msohle1  = ", zone(izoni) % dreissen % msohle1
            print "(10x,a,f0.3)", "gewicht1 = ", zone(izoni) % dreissen % gewicht1
            
      
      
         case('B', 'b')
            ! B Benthische Algen
            read(ctext(2:2000), *, iostat = read_error )  &
               zone(izoni) % albenthi % ggruen,zone(izoni) % albenthi % gkiesel
            if (read_error /= 0) call qerror('Lesefehler B-Zeile MODELLG.3D.txt')
            
            print "(3x,a)",      "* benthic algae"
            print "(10x,a,f0.3)", "ggruen  = ", zone(izoni) % albenthi % ggruen
            print "(10x,a,f0.3)", "gkiesel = ", zone(izoni) % albenthi % gkiesel
         
         case('M', 'm')
            ! M  Makrophyten
            read(ctext(2:2000), *, iostat = read_error )  &
               zone(izoni) % macrophyt % starttag,zone(izoni) % macrophyt % startmonat,  &
               zone(izoni) % macrophyt % maxtag,zone(izoni) % macrophyt % maxmonat,  &
               zone(izoni) % macrophyt % endtag,zone(izoni) % macrophyt % endmonat
            if (read_error /= 0) call qerror('Lesefehler M-Zeile MODELLG.3D.txt')
            
            print "(3x,a)",    "* macrophytes growth period"
            print "(10x,a,i0)", "maxtag = ", zone(izoni) % macrophyt % maxtag
      
         case('P', 'p')
            ! P Dichte der Makrophyten
            read(ctext(2:2000), *, iostat = read_error )  &
               zone(izoni) % macrodicht % pflmin,zone(izoni) % macrodicht % pflmax
            if (read_error /= 0) call qerror('Lesefehler P-Zeile MODELLG.3D.txt')
           
            print "(3x,a)", "* macrophytes growth"
            print "(10x,a,f0.3)", "pflmin = " ,  zone(izoni) % macrodicht % pflmin
            print "(10x,a,f0.3)", "pflmax = ", zone(izoni) % macrodicht % pflmax
         
         case('E', 'e')
            ! E Erosionsparameter
            read(ctext(2:2000), *, iostat = read_error )  &
               zone(izoni) % erosi % tau_krit, zone(izoni) % erosi % M_eros, zone(izoni) % erosi % n_eros, zone(izoni) % erosi % sed_roh
            if (read_error /= 0) call qerror('Lesefehler E-Zeile MODELLG.3D.txt')
            
            print "(3x,a)", "* erosion"
            print "(10x,a,f0.3)", "tau_krit = ", zone(izoni) % erosi % tau_krit
            print "(10x,a,f0.3)", "M_eros   = ", zone(izoni) % erosi % M_eros
            print "(10x,a,f0.3)", "n_eros   = ", zone(izoni) % erosi % n_eros 
            print "(10x,a,f0.3)", "sed_roh  = ", zone(izoni) % erosi % sed_roh
         
         case default
            call qerror("Unknown identifier in ModellG.3D: " // token)
      
      end select
   enddo
   
   close(ion)
   
   if (izoni /= zonen_anzahl) then
      !zonen_anzahl=izoni
      write(fehler,*)'Zonen-Anzahl',izoni,' ungleich der im Kopf von MODELLG.3D.txt angegeben ',zonen_anzahl
      call qerror(fehler)
   endif
   
   do i = 1,zonen_anzahl
      do n = i+1,zonen_anzahl
         if ( zone(i) % zonen_nummer == zone(n) % zonen_nummer) then
            write(fehler,*)' 8 Zonennummer von Zone',i ,' = ',zone(i) % zonen_nummer,' ist gleich Zonennummer von Zone',n
            call qerror(fehler)
         endif
      enddo ! alle n Zonen
      if (zone(i) % reib <= 0.0) then
         write(fehler,*)'Reibungsbeiwert',zone(i) % reib,' von Zone',i,' ist falsch '
         call qerror(fehler)
      endif
   enddo ! alle i Zonen

   ! --------------------------------------------------------------------------
   ! count nodes/elements per zone
   ! --------------------------------------------------------------------------
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         print*
         print "(a)", "nodes per zone:"
         do i = 1,zonen_anzahl
            knozoanz = 0
            zonflae = 0.0
            do n = 1,knotenanzahl2D
               if (knoten_zone(n) == i) then
                  knozoanz = knozoanz+1
                  zonflae = zonflae+knoten_flaeche(n)
               endif
            enddo
            
            print "(3x,a,i0,x,i0,a,f0.2,a)", "zone ", i, knozoanz, " nodes [", zonflae, " m2]"
         enddo
      
      case(2,3) ! Untrim² + SCHISM
         print*
         print "(a)", "elements per zone:"
         
         do i = 1,zonen_anzahl
            knozoanz = 0
            do n = 1,n_elemente
               if (element_zone(n) == zone(i) % zonen_nummer)then
                  knozoanz = knozoanz+1
                  point_zone(n) = i
               endif
            enddo
            
            print "(3x,a,i0,a,i6,a)", "zone ", i,": ", knozoanz, " elements"
         enddo
      
      case default
         call qerror('modellg Hydraulischer Antrieb unbekannt')
   end select
   
   do n = 1,number_plankt_point
      vorhanden = .false.
      do i = 1,zonen_anzahl
         if ( point_zone(n) == i ) then
            vorhanden = .true. ! zone vorhanden + zugeordnet
         endif 
      enddo
      
      if (.not. vorhanden) then
         write(fehler,'(a,i0,a,i0,a)'), 'Der von Berechnungs-Punkt #', n,' benötigte Zonen-Zähler #', point_zone(n), &
                                       ' ist nicht in MODELLG.3D.txt beschrieben.'
         call qerror(fehler)
      endif 
   enddo 
   
end subroutine modellg
