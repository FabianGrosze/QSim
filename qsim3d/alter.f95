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

subroutine alter(i)
   !!    läuft parallel
   use modell
   use QSimDatenfelder
   use aparam
   
   implicit none
   integer :: i,nk
   real :: depth
   real , parameter :: rate = 0.1 ! 1/d
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim3D) in real Tage (QSim1D)
   !     i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   iglob = (i+meinrang*part) ! globale Knotennummer
   if (iglob > number_plankt_point) return ! überstehende Nummern nicht bearbeiten.
   kontroll = iglob == kontrollknoten
   nk = (i-1)*number_plankt_vari
   depth = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen
   ! planktonic_variable_name(71)= "            Tracer"  - c bei Shen&Wang 2007
   ! planktonic_variable_name(73)= "         age_decay"
   ! planktonic_variable_name(74)= "         age_arith"  - alfa bei Shen&Wang 2007
   ! planktonic_variable_name(75)= "        age_growth"
   ! decaying tracer
   planktonic_variable_p(73+nk) = planktonic_variable_p(73+nk)*(1.0 - rate*tflie)
   !!  planktonic_variable_p(73+nk) = 0.0
   ! Altersberechnung wie in Shen&Wang 2007
   planktonic_variable_p(74+nk) = planktonic_variable_p(74+nk) + planktonic_variable_p(71+nk)*tflie
   ! growing tracer
   planktonic_variable_p(75+nk) = planktonic_variable_p(75+nk)*(1.0 + rate*tflie)!! becomes NAN ???
   !! planktonic_variable_p(75+nk) = 0.0
   
   !! #### Testweise trockengefalle Knoten Tracer Null setzen 15apr15 ???
   !tiefe(1)= rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
   !if(rb_hydraul_p(2+(i-1)*number_rb_hydraul) .ge. min_tief ) then !! knoten nass
   if (depth >= 0.0 ) then !! knoten nass
      if (kontroll) print*,"Kontrollknoten nass in alter() weil depth = ",depth &
          , " Tiefe = ",rb_hydraul_p(2+(i-1)*number_rb_hydraul)," min_tief = ",min_tief
   else !! knoten trockengefallen
      !! planktonic_variable_p(71+nk)=0.0 !! Tracer null im trockenen
      if (kontroll) print*,"Kontrollknoten trockengefallen in alter() weill depth = ",depth &
          ," Tiefe = ",rb_hydraul_p(2+(i-1)*number_rb_hydraul)," min_tief = ",min_tief
   endif ! Knoten nass
   if (kontroll) print*,'alter: tracer, decaying,linear,grow ; nk,i', planktonic_variable_p(71+nk)  &
       , planktonic_variable_p(73+nk), planktonic_variable_p(74+nk), planktonic_variable_p(75+nk), nk, i
   return
end subroutine alter


!----+-----+----
!> Einlesen der Steuerdatein für die Altersberechnung
!! läuft nur auf prozessor 0
subroutine alter_lesen()
   use modell
   implicit none
   character (len = 300) :: dateiname, b
   integer open_error, ion, i,j, read_stat, alloc_status,n, altzaehl
   logical dummy
   print*," "
   print*,"alter_lesen() ..."
   ion = 345
   i = 0
   wie_altern = 0
   alter_nummer = -1
   write(dateiname,'(2A)')trim(modellverzeichnis),'alter.txt'
   open ( unit = ion , file = dateiname, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0)call qerror('alter.txt lässt sich nicht öffnen ?')
   do while ( zeile(ion) .and. (wie_altern == 0) )
      if ( .not. leerzeile()) then
         !i=i+1
         write(b,'(A)')trim(ctext)
         !print*,i," : ",trim(b)
         if ((b(1:1) == "z") .or. (b(1:1) == "Z"))wie_altern = 1
         if ((b(1:1) == "r") .or. (b(1:1) == "R"))wie_altern = 2
      end if !keine leerzeile
      print*, "alter_lesen(): wie_altern = ",wie_altern
   end do ! keine weitere Zeile in alter.txt
   select case (wie_altern)
      case(0) ! Keine Angabe gefunden
         nur_alter = .false.
         print*, "### Doch keine ausschließliche Altersberechnung ###"
      case(1) ! Zone
         !print*,"Z:",trim(b)
         read(b(2:300), *, iostat = read_stat ) alter_nummer
         if (read_stat /= 0) call qerror("Lesen der Zonen-zeile:", trim(b)," in Datei alter.txt fehlgeschlagen")
         print*,"Aufenthaltszeiten-Berechnung für zone Nummer:",alter_nummer
         print*," "
         do i = 1,zonen_anzahl ! alle i zonen
            if (alter_nummer == zone(i)%zonen_nummer)altzaehl = i
         end do ! alle i zonen
         alter_nummer = altzaehl
         print*,'selbige Zone hat den Zähler:  ',alter_nummer
         if ((alter_nummer < 1) .or. (alter_nummer > zonen_anzahl)) &
             call qerror("alter_lesen(): Zonen-Nummer ungültig")
      case(2) ! Rand
         !print*,"R:",trim(b)
         read(b(2:300), *, iostat = read_stat ) alter_nummer
         if (read_stat /= 0) call qerror("Lesen der Rand-zeile", trim(b), " in Datei alter.txt fehlgeschlagen")
         print*,"Es soll also das Alter des Wassers, das über Rand #",alter_nummer," zufließt, ermittelt werden:"
         altzaehl = 0
         do n = 1,ianz_rb !! alle Randbedingungen
            if (alter_nummer == rabe(n)%nr_rb) then
               print*,'selbiger Rand wird von der ',n,'-ten Randbedingung bedient'
               altzaehl = n
            end if
         end do !! alle n Randbedingungen
         alter_nummer = altzaehl
         if ((alter_nummer < 1) .or. (alter_nummer > ianz_rb)) &
             call qerror("alter_lesen(): Rand-Nummer ungültig")
         case default
         call qerror("Angaben in Datei alter.txt unklar")
   end select
   rewind (ion)
   close (ion)
   ! Datenfelder anlegen für Integration Tracer-"Massen" und Wasservolumina
   allocate (tr_integral_zone(zonen_anzahl,zeitschrittanzahl+1), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)'allocate tr_integral_zone(zonen_anzahl, fehlgeschlagen alloc_status = ', alloc_status
      call qerror(fehler)
   end if !
   allocate (vol_integral_zone(zonen_anzahl,zeitschrittanzahl+1), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)'allocate vol_integral_zone(zonen_anzahl, fehlgeschlagen alloc_status = ', alloc_status
      call qerror(fehler)
   end if !
   allocate (ent_integral_zone(zonen_anzahl,zeitschrittanzahl+1), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)'allocate ent_integral_zone(zonen_anzahl fehlgeschlagen alloc_status = ', alloc_status
      call qerror(fehler)
   end if !
   do i = 1,zeitschrittanzahl+1 !! initialisieren...
      do j = 1,zonen_anzahl
         tr_integral_zone(j,i) = 0.0
         vol_integral_zone(j,i) = 0.0
         ent_integral_zone(j,i) = 0.0
      end do ! alle zonen
   end do ! alle i Zeitschritte
   return
end subroutine alter_lesen


!----+-----+----
!> alter_parallel bereitet parallel für alter vor.
!! läuft parallel
subroutine alter_parallel()
   use modell
   implicit none
   call MPI_Bcast(nur_alter,1,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(nur_temp,1,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   return
end subroutine alter_parallel
!----+-----+----
!> Anfangsbedingungen für die Altersberechnung
!! läuft nur auf prozessor 0
subroutine alter_ini()
   use modell
   implicit none
   integer i, nk, nura,nuzo
   !! vorab alles 0.0
   do i = 1,number_plankt_point ! alle i knoten
      nk = (i-1)*number_plankt_vari
      planktonic_variable(71+nk) = 1.0 ! Tracer
      planktonic_variable(73+nk) = 1.0 ! alter_d
      planktonic_variable(74+nk) = 1.0 ! alter_a
      planktonic_variable(75+nk) = 1.0 ! alter_g
   end do ! alle i knoten
   select case (wie_altern)
      case(1) ! Zone
         print*,"alter_ini() für ",alter_nummer,"-te zone mit nummer = ",zone(alter_nummer)%zonen_nummer
         do i = 1,number_plankt_point ! alle i knoten/elemente
            nuzo = point_zone(i)
            if (nuzo == alter_nummer ) then
               planktonic_variable(71+(i-1)*number_plankt_vari) = 1.0 ! Tracer = 1.0
            end if !
         end do ! alle i berechnungsstützstellen
      case(2) ! Rand
         print*,"alter_ini() für ",alter_nummer,"-ten Rand mit nummer = ",rabe(alter_nummer)%nr_rb
         do i = 1,number_plankt_point ! alle i knoten/elemente
            select case (hydro_trieb)
               case(1) ! casu-transinfo
                  nura = knoten_rand(i)
               case(2) ! Untrim² netCDF
                  nura = element_rand(i)
                  case default
                  call qerror('Hydraulischer Antrieb unbekannt alter_ini nura')
            end select
            if (nura == alter_nummer) then
               planktonic_variable(71+(i-1)*number_plankt_vari) = 1.0 ! Tracer = 1.0
            end if !
         end do ! alle i berechnungsstützstellen
         case default
         call qerror("wie_altern muss vor alter_ini() geklärt werden")
   end select
   print*,'alter_ini() geschafft'
   return
end subroutine alter_ini
!----+-----+----
!> Integration der Tracer-"Masse" und des Wasservolumens
!! innerhalb jeder Zone \n
!! und \n
!! Integration der Flüsse von Tracer-"Masse" und des Wasservolumen
!! über alle Ränder \n
!! sowie Vorbereitung zur Ausgabe mit den Ganglinien in die Datei tracer.txt\n
!! nur auf prozessor 0, Variablenfelder werden in alter_lesen() angelegt.
!! \n\n
subroutine alter_zeitschritt(izeit_gang)
   use modell
   implicit none
   integer :: j, izeit_gang
   real volumen, tracer, entropy, c, tief
   !         allocate (tr_integral_zone(zonen_anzahl,zeitschrittanzahl+1), stat = alloc_status )
   !         allocate (vol_integral_zone(zonen_anzahl,zeitschrittanzahl+1), stat = alloc_status )
   do j = 1,number_plankt_point
      tief = rb_hydraul_p(2+(j-1)*number_rb_hydraul)
      if (tief >= min_tief ) then
         c = planktonic_variable(71+(j-1)*number_plankt_vari)
         volumen = tief*knoten_flaeche(j)
         vol_integral_zone(point_zone(j),izeit_gang) = vol_integral_zone(point_zone(j),izeit_gang)  &
                                                       + volumen
         tracer = volumen*c
         tr_integral_zone(point_zone(j),izeit_gang) = tr_integral_zone(point_zone(j),izeit_gang)  &
                                                      + tracer
         !! Entropie wie Lauritzen und Thuburn 2011 (http://doi.wiley.com/10.1002/qj.986)
         if ( tracer > 0.0) then
            entropy = volumen*c*log10(c)
         else ! tracer 0
            entropy = 0.0
         end if ! tracer > 0
         ent_integral_zone(point_zone(j),izeit_gang) = ent_integral_zone(point_zone(j),izeit_gang) - entropy
      endif ! Knoten nass
      if (j == kontrollknoten) then ! Ausgabe kontrollknoten
         if (tief >= min_tief ) then
            print*,'tracer_volumen_gangl: c,tief,flaech,point_zone,zonen_nummer, volumen, tracer, entropy = ',  &
                   c,tief,knoten_flaeche(j),  &
                   point_zone(j),zone(point_zone(j))%zonen_nummer, volumen, tracer, entropy
         endif ! Knoten nass
      end if ! kontrollknoten
   end do ! alle j Knoten
end subroutine alter_zeitschritt
!----+-----+----
!> alter_ausgabe Integrale zusammen mit den ganglinien ausgeben in jedem Zeitschritt
!!
!! läuft nur auf prozessor 0
subroutine alter_ausgabe()
   use modell
   implicit none
   character(4000) beschriftung1
   character(300) dateiname, zeitig
   integer i, j, open_error
   ! Ganglinienausgabe Tracer+Volumen-Integrale auf tracer.txt
   write(dateiname,'(4A)')trim(modellverzeichnis),'ganglinien/tracer_zonen_integrale.txt'
   print*,'Tracer+Volumen-Integrale Ausgabe auf:', trim(dateiname), ' meinrang = ',meinrang
   open ( unit = 444444 , file = dateiname, status = 'new', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      print*,'ganglinien/tracer_zonen_integrale.txt lässt sich nicht öffnen'
      return
   end if ! open_error.ne.0
   rewind (444444) ! tracer.txt zurückspulen
   write(444444,*)'# Zonen-Anzahl = ',zonen_anzahl,'jeweils Tracermasse | Wasservolumen | Entropie '
   do j = 1,zeitschrittanzahl+1
      zeitpunkt = r_gang(1,j)
      call zeitsekunde()
      write(zeitig,'(I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
            jahr  ,monat ,tag   ,stunde,minute,sekunde   !r_gang(i,j)
      write(beschriftung1,'(A)')trim(zeitig)
      ! Ganglinienausgabe zonenintegrale auf tracer.txt
      do i = 1,zonen_anzahl
         write(beschriftung1,'(A,"       ",F16.0," ",F16.0," ",F18.2)')trim(beschriftung1)  &
               ,tr_integral_zone(i,j),vol_integral_zone(i,j),ent_integral_zone(i,j)
      end do ! alle i zonen
      write(444444,'(A)')trim(beschriftung1)
   end do ! alle j Zeitpunkte
   close (444444) ! Dateien tracer.txt wieder schließen
   return
end subroutine alter_ausgabe
!----+-----+----
!> Randbedingungen setzen für die Altersberechnung
!! in       SUBROUTINE randwert_planctonic(j,zaehl)
!! nicht hier !!
!! läuft nur auf prozessor 0 ???
subroutine alter_rand(j)
   use modell
   implicit none
   integer :: j,nk, nura
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         nura = knoten_rand(j)
      case(2) ! Untrim² netCDF
         nura = element_rand(j)
         case default
         call qerror('Hydraulischer Antrieb unbekannt alter_rand(')
   end select
   if (j == kontrollknoten) then ! Ausgabe kontrollknoten
      print*,"alter_rand ",nura,alter_nummer
   end if ! kontrollknoten
   nk = (j-1)*number_plankt_vari
   if (nura == alter_nummer) then
      planktonic_variable(71+nk) = 1.0 !  tracer
      planktonic_variable(73+nk) = 1.0 ! alter_d
      planktonic_variable(75+nk) = 1.0 ! alter_g
   else
      planktonic_variable(71+nk) = 0.0 !  tracer
      planktonic_variable(73+nk) = 0.0 ! alter_d
      planktonic_variable(75+nk) = 0.0 ! alter_g
   end if
   planktonic_variable(74+nk) = 0.0 ! alter_a
   return
end subroutine alter_rand
!----+-----+----
