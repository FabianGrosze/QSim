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

!! läuft parallel
subroutine alter(i)
   use modell
   use QSimDatenfelder
   use module_aparam
   
   implicit none
   integer :: i,nk
   real    :: depth
   
   real, parameter :: rate = 0.1 ! 1/d
   
   
   
   ! i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   iglob = i + meinrang * part
   if (iglob > number_plankt_point) return ! überstehende Nummern nicht bearbeiten.
   
   tflie = real(deltat) / 86400.
   control = iglob == kontrollknoten
   nk = (i-1)*number_plankt_vari
   depth = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen
   
   ! Altersberechnung wie in Shen&Wang 2007
   
   ! planktonic_variable_name(71) = "Tracer"  - c bei Shen&Wang 2007
   ! planktonic_variable_name(73) = "age_decay"
   ! planktonic_variable_name(74) = "age_arith"  - alfa bei Shen&Wang 2007
   ! planktonic_variable_name(75) = "age_growth"
   ! decaying tracer
   planktonic_variable_p(73+nk) = planktonic_variable_p(73+nk)*(1.0 - rate*tflie)
   planktonic_variable_p(74+nk) = planktonic_variable_p(74+nk) + planktonic_variable_p(71+nk)*tflie
   ! growing tracer
   planktonic_variable_p(75+nk) = planktonic_variable_p(75+nk)*(1.0 + rate*tflie)!! becomes NAN ???
   
   if (depth >= 0.0 ) then !! knoten nass
      if (control) then 
         print*,"Kontrollknoten nass in alter() weil depth = ",depth &
          , " Tiefe = ",rb_hydraul_p(2+(i-1)*number_rb_hydraul)," min_tief = ",min_tief
      endif
   else !! knoten trockengefallen
      !! planktonic_variable_p(71+nk)=0.0 !! Tracer null im trockenen
      if (control) then
         print*,"Kontrollknoten trockengefallen in alter() weill depth = ",depth &
          ," Tiefe = ",rb_hydraul_p(2+(i-1)*number_rb_hydraul)," min_tief = ",min_tief
      endif
   endif
   
   if (control) then
      print*,'alter: tracer, decaying,linear,grow ; nk,i', planktonic_variable_p(71+nk)  &
       , planktonic_variable_p(73+nk), planktonic_variable_p(74+nk), planktonic_variable_p(75+nk), nk, i
   endif
   
end subroutine alter


!----+-----+----
!> Einlesen der Steuerdatein für die Altersberechnung
!! läuft nur auf prozessor 0
subroutine alter_lesen()
   
   use modell
   
   implicit none
   character(300) :: filename, b
   integer        :: open_error, ion, i, read_stat, alloc_status, altzaehl
   
   wie_altern = 0
   alter_nummer = -1
   
   print*
   print*, "alter_lesen() "
   
   filename = trim(modellverzeichnis) // 'alter.txt'
   open(newunit = ion , file = filename, status = 'old', action = 'read ', iostat = open_error)
   if (open_error /= 0) call qerror('Could not open ' // filename)
   
   do while ( zeile(ion) .and. (wie_altern == 0) )
      if ( .not. leerzeile()) then
         write(b,'(A)') trim(ctext)
         if ((b(1:1) == "z") .or. (b(1:1) == "Z")) wie_altern = 1
         if ((b(1:1) == "r") .or. (b(1:1) == "R")) wie_altern = 2
      endif 
      print*, "alter_lesen(): wie_altern = ",wie_altern
   enddo
   close(ion)   
   
   select case (wie_altern)
      case(0) ! Keine Angabe gefunden
         nur_alter = .false.
         print*, "### Doch keine ausschließliche Altersberechnung ###"
      
      case(1) ! Zone
         read(b(2:300), *, iostat = read_stat ) alter_nummer
         if (read_stat /= 0) call qerror("Lesen der Zonen-zeile:", trim(b), " in Datei alter.txt fehlgeschlagen")
         
         print*, "Aufenthaltszeiten-Berechnung für Zone:", alter_nummer
         print*
         
         do i = 1,zonen_anzahl 
            if (alter_nummer == zone(i)%zonen_nummer) altzaehl = i
         enddo 
         alter_nummer = altzaehl
         print*,'selbige Zone hat den Zähler:  ', alter_nummer
         if (alter_nummer < 1 .or. alter_nummer > zonen_anzahl) then
            call qerror("alter_lesen(): Zonen-Nummer ungültig")
         endif
      
      case(2) ! Rand
         read(b(2:300), *, iostat = read_stat ) alter_nummer
         if (read_stat /= 0) call qerror("Lesen der Rand-zeile", trim(b), " in Datei alter.txt fehlgeschlagen")
         print*,"Es soll also das Alter des Wassers, das über Rand #", alter_nummer, " zufließt, ermittelt werden:"
         altzaehl = 0
         do i = 1,ianz_rb !! alle Randbedingungen
            if (alter_nummer == rabe(i)%nr_rb) then
               print*,'selbiger Rand wird von der ', i, '-ten Randbedingung bedient'
               altzaehl = i
            endif
         enddo !! alle Randbedingungen
         alter_nummer = altzaehl
         if ((alter_nummer < 1) .or. (alter_nummer > ianz_rb)) &
             call qerror("alter_lesen(): Rand-Nummer ungültig")
      
      case default
         call qerror("Angaben in Datei alter.txt unklar")
   end select
   
   ! Datenfelder anlegen für Integration Tracer-"Massen" und Wasservolumina
   allocate (tr_integral_zone(zonen_anzahl,zeitschrittanzahl+1), source = 0.0, stat = alloc_status )
   if (alloc_status /= 0) call qerror("Error while allocating variable `tr_integral_zone`")
   
   allocate (vol_integral_zone(zonen_anzahl,zeitschrittanzahl+1), source = 0.0, stat = alloc_status )
   if (alloc_status /= 0) call qerror("Error while allocating variable `vol_integral_zone`")
   
   allocate (ent_integral_zone(zonen_anzahl,zeitschrittanzahl+1), source = 0.0, stat = alloc_status )
   if (alloc_status /= 0) call qerror("Error while allocating variable `ent_integral_zone`")

   
end subroutine alter_lesen


!----+-----+----
!> alter_parallel bereitet parallel für alter vor.
!! läuft parallel
subroutine alter_parallel()
   use modell
   
   implicit none
   
   call MPI_Bcast(nur_alter, 1, MPI_LOGICAL, 0, mpi_komm_welt, ierr)
   call MPI_Bcast(nur_temp , 1, MPI_LOGICAL, 0, mpi_komm_welt, ierr)
end subroutine alter_parallel

!----+-----+----
!> Anfangsbedingungen für die Altersberechnung
!! läuft nur auf prozessor 0
subroutine alter_ini()
   use modell
   
   implicit none
   integer :: i, nk, nura
   
   ! initialise tracers with age variables with 0
   do i = 1,number_plankt_point ! alle i knoten
      nk = (i - 1) * number_plankt_vari
      planktonic_variable(71 + nk) = 0.0 ! Tracer
      planktonic_variable(73 + nk) = 0.0 ! alter_decay
      planktonic_variable(74 + nk) = 0.0 ! alter_arith
      planktonic_variable(75 + nk) = 0.0 ! alter_growth
   enddo ! alle i knoten
   
   select case (wie_altern)
      case(1) ! Zone
         print*,"alter_ini() für ",alter_nummer,"-te zone mit nummer = ",zone(alter_nummer)%zonen_nummer
         do i = 1,number_plankt_point ! alle i knoten/elemente
            if (point_zone(i) == alter_nummer ) then
               planktonic_variable(71 + (i - 1) * number_plankt_vari) = 1.0 ! Tracer
            endif !
         enddo ! alle i berechnungsstützstellen
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
               nk = (i - 1) * number_plankt_vari
               planktonic_variable(71 + nk) = 1.0 ! Tracer
               planktonic_variable(73 + nk) = 1.0 ! age_decay
               planktonic_variable(75 + nk) = 1.0 ! age_growth
            endif !
         enddo ! alle i berechnungsstützstellen
      case default
         call qerror("wie_altern muss vor alter_ini() geklärt werden")
   end select
   write(*, '(A)') 'alter_ini() done'
   
end subroutine alter_ini
!----+-----+----

!> Integration der Tracer-"Masse" und des Wasservolumens innerhalb jeder Zone 
!! und  Integration der Flüsse von Tracer-"Masse" und des Wasservolumen über 
!! alle Ränder sowie Vorbereitung zur Ausgabe mit den Ganglinien in die Datei
!! `tracer.txt`
!!
!! nur auf prozessor 0, Variablenfelder werden in alter_lesen() angelegt.
subroutine alter_zeitschritt(izeit_gang)
   use modell
   implicit none
   
   integer, intent(in) :: izeit_gang
   integer             :: j
   real                :: volumen, tracer, entropy, c, tief
   
   do j = 1,number_plankt_point
      tief = rb_hydraul_p(2+(j-1)*number_rb_hydraul)
      
      if (tief >= min_tief ) then
         volumen = tief * knoten_flaeche(j)
         vol_integral_zone(point_zone(j), izeit_gang) = vol_integral_zone(point_zone(j), izeit_gang) + volumen
         c = planktonic_variable(71 + (j - 1) * number_plankt_vari)
         tracer = volumen * c
         tr_integral_zone(point_zone(j), izeit_gang) =  tr_integral_zone(point_zone(j), izeit_gang) + tracer
         
         ! Entropie wie Lauritzen und Thuburn 2011 (http://doi.wiley.com/10.1002/qj.986)
         if (tracer > 0.0) then
            entropy = tracer * log10(c)
            ent_integral_zone(point_zone(j),izeit_gang) = ent_integral_zone(point_zone(j),izeit_gang) - entropy
         else
            entropy = 0.0
         endif
         
         if (j == kontrollknoten) then ! Ausgabe kontrollknoten
            print*, 'tracer_volumen_gangl: c,tief,flaech,point_zone,zonen_nummer, volumen, tracer, entropy = ', &
                    c, tief, knoten_flaeche(j),                                                                 &
                    point_zone(j), zone(point_zone(j))%zonen_nummer, volumen, tracer, entropy
         endif
      endif
   enddo
   
end subroutine alter_zeitschritt


!> alter_ausgabe Integrale zusammen mit den ganglinien ausgeben in jedem Zeitschritt
!!
!! läuft nur auf prozessor 0
subroutine alter_ausgabe()
   use module_datetime
   use modell
   
   implicit none
   
   character(4000)     :: beschriftung1
   character(longname) :: filename
   integer             :: i, j, open_error, u_age
   integer             :: year, month, day, hour, minute, second
   type(datetime)      :: datetime_output
   
   ! Ganglinienausgabe Tracer+Volumen-Integrale auf tracer.txt
   filename = trim(modellverzeichnis) // 'ganglinien/tracer_zonen_integrale.txt'
   open(newunit = u_age , file = filename, status = 'new', action = 'write ', iostat = open_error)
   if (open_error /= 0) call qerror("Could not open " // trim(filename))
   
   write(u_age,*)'# Zonen-Anzahl = ',zonen_anzahl,'jeweils Tracermasse | Wasservolumen | Entropie '
   
   do j = 1,zeitschrittanzahl+1
      datetime_output = as_datetime(r_gang(1,j), tz_qsim)
      
      year   = datetime_output % get_year()
      month  = datetime_output % get_month()
      day    = datetime_output % get_day()
      hour   = datetime_output % get_hour()
      minute = datetime_output % get_minute()
      second = datetime_output % get_second()
      
      write(beschriftung1,'(I4,"-",I2.2,"-",I2.2,X,I2.2,":",I2.2,":",I2.2)') &
            year, month, day, hour, minute, second
      
   
      ! Ganglinienausgabe zonenintegrale auf tracer.txt
      do i = 1,zonen_anzahl
         write(beschriftung1,'(A,7X,F16.0,X,F16.0,X,F18.2)') trim(beschriftung1),  &
               tr_integral_zone(i,j), vol_integral_zone(i,j), ent_integral_zone(i,j)
      enddo 
      
      write(u_age,'(A)')trim(beschriftung1)
   enddo
   
   close(u_age)
   
end subroutine alter_ausgabe
!----+-----+----

!> Randbedingungen setzen für die Altersberechnung
!! in       SUBROUTINE randwert_planktonic(j,zaehl)
!! nicht hier !!
!! läuft nur auf prozessor 0 ???
subroutine alter_rand(j)
   use modell
   
   implicit none
   integer :: j, nk, nura
   
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         nura = knoten_rand(j)
      case(2) ! Untrim² netCDF
         nura = element_rand(j)
      case default
         call qerror('Hydraulischer Antrieb unbekannt alter_rand(')
   end select
   
   if (j == kontrollknoten) then
      print*,"alter_rand ", nura, alter_nummer
   endif
   
   nk = (j - 1) * number_plankt_vari
   if (nura == alter_nummer) then
      planktonic_variable(71 + nk) = 1.0 !  tracer
      planktonic_variable(73 + nk) = 1.0 ! alter_d
      planktonic_variable(75 + nk) = 1.0 ! alter_g
   else
      planktonic_variable(71 + nk) = 0.0 !  tracer
      planktonic_variable(73 + nk) = 0.0 ! alter_d
      planktonic_variable(75 + nk) = 0.0 ! alter_g
   endif
   planktonic_variable(74 + nk) = 0.0 ! alter_a

end subroutine alter_rand