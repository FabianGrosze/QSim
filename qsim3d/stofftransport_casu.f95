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
!> Subroutine stofftransport_casu()
!! Beschreibung in \ref lnk_transport_casu
subroutine stofftransport_casu()
   use modell
   implicit none
   
   integer                                :: i, wrong, n, j, as, l, nullzae, trockzae
   integer                                :: nt, stranglang
   integer, dimension(4)                  :: ieck
   real, dimension(:,:), allocatable      :: zwischen
   real, dimension(4)                     :: tiwicht
   real                                   :: summ, anteil, tracervolumen, tief
   logical, dimension(number_plankt_vari) :: negativ
   
   if (meinrang /= 0)call qerror('stofftransport_casu must only be called from processor 0.')
   
   
   ! stationaer=.true. !! wird in TQsim.f95 gesetzt; erprobung einde
   if (stationaer) then
      write(fehler,*)'stationaer so noch nicht vorgesehen (stofftransport.f95)'
      call qerror(fehler)
   endif
   
   call transinfo_schritte(startzeitpunkt, endzeitpunkt)
   print "(2(a,i0))", "stofftransport: timestep from ", na_transinfo, " to ", ne_transinfo
   
   do nt = na_transinfo, ne_transinfo ! alle Transport (zwischen) Zeitschritte
      call holen_trans(nt)
      allocate (zwischen(number_plankt_vari, number_plankt_point), stat = as )
      
      negativ(:) = .false.
      nullzae = 0
      trockzae = 0
      do j = 1,number_plankt_point ! alle j Knoten
         tief = rb_hydraul(2+(j-1)*number_rb_hydraul)
         do n = 1,number_plankt_vari !! Kontrolle auf negative Werte vorab:
            if (planktonic_variable(n+(j-1)*number_plankt_vari) < 0.0) then
               negativ(n) = .true.
            endif
      
            if (isNaN( planktonic_variable(n+(j-1)*number_plankt_vari) ))  then
               print*,'isNaN_planktonic_variable plankt_vari = ',n,' plankt_point = ',j
            endif
         enddo ! alle variablen
      
         do l = 1,4
            ieck(l) = intereck((j-1)*4+l)
            if (isNaN( wicht((j-1)*4+l) )) print*,'isNaN_wicht,j,l,intereck',wicht((j-1)*4+l),j,l,intereck((j-1)*4+l)
         enddo ! alle 4 wichtungsfaktoren
         
         do l = 1,3
            !1! tiwicht(l)=wicht(nm*4+l)
            !2! tiwicht(l)=wicht(nm*4+l)*tief(ieck(l))
            tiwicht(l) = wicht((j-1)*4+l) * rb_hydraul(2+(ieck(l)-1)*number_rb_hydraul) !  neu31jan14 * tief(ieck(l))
            if ( rb_hydraul(2+(ieck(l)-1)*number_rb_hydraul) <= min_tief)tiwicht(l) = 0.0
         enddo ! alle 3 wichtungsfaktoren
         
         if (ieck(4) > 0) then !! nur bei vierecken 4.Knoten
            !1! tiwicht(4)=wicht(nm*4+4)
            !2! tiwicht(4)=wicht(nm*4+4)*tief(ieck(4))
            tiwicht(4) = wicht((j-1)*4+4)  * rb_hydraul(2+(ieck(4)-1)*number_rb_hydraul) ! ! neu31jan14 * tief(ieck(l))
            if (rb_hydraul(2+(ieck(4)-1)*number_rb_hydraul) <= min_tief) tiwicht(4) = 0.0  !4!
         else  !3!
            tiwicht(4) = 0.0  !3!
         endif
         
         summ = sum(tiwicht)
         
         if (summ > 0.0) then
            do l = 1,4
               tiwicht(l) = tiwicht(l)/summ
            enddo ! alle 4 wichtungsfaktoren
         else ! summ=0 konzentration bleibt liegen !
            !do l=1,4 !! Versuch 30jul15 wy
            !   tiwicht(l)=0.25
            !   ieck(l)=j
            !enddo ! alle 4 wichtungsfaktoren
            nullzae = nullzae+1
         endif ! summ.gt.0
         
         if (tief <= min_tief) then
            trockzae = trockzae+1
         endif ! trocken
         
         if (j == kontrollknoten) then
            print*,'ieck,l = 1,4',(ieck(l),l = 1,4),'tiwicht,l = 1,4',(tiwicht(l),l = 1,4), summ
         endif
         
         if ((summ <= 0.0) .or. (tief <= min_tief)) then ! summ=0 oder alles trocken?, Wert bleibt | 19jul17 wy
            do n = 1,number_plankt_vari
               zwischen(n,j) = planktonic_variable(n+(j-1)*number_plankt_vari)
            enddo ! alle n Konzentrationen
         
         else ! summ!=0 und nass?
            do n = 1,number_plankt_vari
               zwischen(n,j) = planktonic_variable(n+(ieck(1)-1)*number_plankt_vari)*tiwicht(1) +  &
                               planktonic_variable(n+(ieck(2)-1)*number_plankt_vari)*tiwicht(2) +  &
                               planktonic_variable(n+(ieck(3)-1)*number_plankt_vari)*tiwicht(3)
         
               if (ieck(4) > 0) then !! nur bei vierecken 4.Knoten
                  zwischen(n,j) = zwischen(n,j) + planktonic_variable(n+(ieck(4)-1)*number_plankt_vari)*tiwicht(4)
               endif
               
               if (isNaN(zwischen(n,j))) then
                  print*,'stofftransport isNaN(zwischen(',n,',',j,')), summ',zwischen(n,j),summ
                  print*,'ieck,l = 1,4',(ieck(l),l = 1,4)
                  print*,'tiwicht,l = 1,4',(tiwicht(l),l = 1,4)
                  print*,'wicht,l = 1,4',(wicht((j-1)*4+l),l = 1,4)
                  print*,'tief,l = 1,3',(rb_hydraul(2+(ieck(l)-1)*number_rb_hydraul),l = 1,3)
                  if (ieck(4) > 0)print*,'tief(ieck(4) = ',rb_hydraul(2+(ieck(4)-1)*number_rb_hydraul) ! nur bei vierecken 4.Knoten
                  print*,'(planktonic_variable(n+(ieck(l)-1)*number_plankt_vari),l = 1,3) = ',  &
                         (planktonic_variable(n+(ieck(l)-1)*number_plankt_vari),l = 1,3)
                  if (ieck(4) > 0)print*,'planktonic_variable(n+(ieck(4) = ',planktonic_variable(n+(ieck(4)-1)*number_plankt_vari) ! nur bei vierecken 4.Knoten
                  print*,'planktonic_variable_name',n, planktonic_variable_name(n)
                  write(fehler,*)'stofftransport: isNaN(zwischen planktonic_variable_name',planktonic_variable_name(n)
                  call qerror(fehler)
               endif ! isNaN
            enddo ! alle n Konzentrationen
         endif ! summ=0 alles trocken?, Wert bleibt
      enddo ! alle j Knoten
      
      do j = 1,number_plankt_point ! alle j Knoten
         do n = 1,number_plankt_vari
            planktonic_variable(n+(j-1)*number_plankt_vari) = zwischen(n,j)
         enddo ! alle n Konzentrationen
      enddo ! alle j Knoten
      
      deallocate(zwischen, stat = as)
      do n = 1,number_plankt_vari !!
         if (negativ(n)) then
            print*,'stofftransport negativ, planktonic_variable #',n,' ',planktonic_variable_name(n)
         endif
      enddo ! alle planktischen variablen
      
      if (nullzae > 0)  print*,'stofftransport_casu: an ',nullzae,' Berechnungspunkten war die Wichtungssumme Null ?!'
      if (trockzae > 0) print*,'stofftransport_casu: ',trockzae,' Berechnungspunkte waren trocken'
      
      tracervolumen = 0.0
      do j = 1,number_plankt_point ! alle j Knoten
         tief = rb_hydraul(2+(j-1)*number_rb_hydraul)
         if (tief >= min_tief) then !nass
            tracervolumen = tracervolumen+( tief*knoten_flaeche(j)*planktonic_variable(71+(j-1)*number_plankt_vari) )
         endif !nass
      enddo ! alle j Knoten
      print*,'tracervolumen = ', transinfo_zeit(transinfo_zuord(nt)), tracervolumen
   
   enddo ! nt, alle Transport (zwischen) Zeitschritte
   
end subroutine stofftransport_casu


!> Die subroutine holen_trans(nt) , mit nt - Zeitschrittzähler,
!! holt die Transportinformationen für einen Zeitschritt mittels der 
!! C++-Funktion trans_read() aus Datei trans_read.c.
!!
!! In den Dateien aus dem Verzeichnis transinfo, deren Name mit t beginnt und 
!! danach den Zeitpunkt als Zahl enthält, ist der Strombahnursprung, 
!! die Wasserspiegellage und der Geschwindigkeitsbetrag abgelegt
!!
!! holen_trans() macht daraus die \ref lnk_hydraul_rb Geschwindigkeitsbetrag, 
!! Wassertiefe und Wasserspiegellage.\n
!! holen_trans() wird nur aus stofftransport() heraus von Prozess 0 aufgerufen
subroutine holen_trans(nt)
   use modell
   implicit none
   
   integer, intent(in) :: nt
   
   integer        :: ntist, trockzae
   integer        :: wrong, lang, j,i,jj,ll
   real           :: ubetr, utau, infl, flaeche, volumen, anteil, ks, zet, tief
   character(500) :: filename
   
   trockzae = 0
   if (stationaer) then
      ntist = 1
   else !! instationaer
      ntist = nt
   endif
   
   filename = trim(modellverzeichnis) // 'transinfo/' // trim(transinfo_datei(transinfo_zuord(ntist)))
   lang = len(trim(filename))
   call trans_read(trim(filename), lang, nonu, intereck, wicht, &
                   wrong, p, u, dir, kontrollknoten)
   
   if (wrong /= 0) then
      write(fehler,*)' trans_read Lesen der Transportinformationen fehlgeschlagen ', wrong
      call qerror(fehler)
   endif
   
   if (nonu /= number_plankt_point) then
      write(fehler,*)'holen_trans: nonu /= number_plankt_point',nonu,number_plankt_point
      call qerror(fehler)
   endif
   
   do jj = 1,number_plankt_point ! alle j Knoten
      do ll = 1,4
         if (isNaN( wicht((jj-1)*4+ll))) then
            print*,'holen_trans: isNaN( wicht((jj-1)*4+ll)  jj,ll,((jj-1)*4+ll) = ' &
                    ,jj,ll,((jj-1)*4+ll)
         endif
      enddo
   enddo
   
   flaeche = 0.0
   volumen = 0.0
   anteil = 1.0/real(zeitschrittanzahl*anz_transinfo) !! Zeit-Anteil am Gesamt-Simulations-Zeitraum
   
   do j = 1,nonu !! 2D-Knoten
      tief = p(j) - knoten_z(j)  ! Wassertiefe ermitteln:
      if (tief <= min_tief ) then ! trocken ! min_tief parameter aus module_modell
         trockzae = trockzae + 1
      else ! nass
         flaeche = flaeche + knoten_flaeche(j)
         volumen = volumen + (tief * knoten_flaeche(j))
         if (uedau_flag) call qerror(" holen_trans(nt) Überstaudauer nicht mehr implementiert")
      endif
   enddo
   
   mittelflaech  = mittelflaech  + (flaeche * anteil)
   mittelvolumen = mittelvolumen + (volumen * anteil)
   
   ! Bahnlinienursprünge ermitteln +
   ! Zuflussränder detektieren: d.h. Rand an dem Geschwindigkeit ohne Bahnlinielänge
   do j = 1,nonu ! all nodes
      inflow(j) = .false.
      ur_x(j) = 0.0
      ur_y(j) = 0.0
      ur_z(j) = 0.0
      do i = 1,4
         if (intereck((j-1)*4+i) > 0) then ! nur bei vierecken 4.Knoten
            ur_x(j) = ur_x(j) + knoten_x(intereck((j-1)*4+i)) * wicht((j-1)*4+i)
            ur_y(j) = ur_y(j) + knoten_y(intereck((j-1)*4+i)) * wicht((j-1)*4+i)
            ur_z(j) = ur_z(j) + knoten_z(intereck((j-1)*4+i)) * wicht((j-1)*4+i)
         endif
      enddo ! alle 4 ecken
      ubetr = (((knoten_x(j)-ur_x(j))**2 + (knoten_y(j)-ur_y(j))**2 + (knoten_z(j)-ur_z(j))**2)**0.5)/dttrans
      infl = 10.0
   
      if (u(j) > 0.0) infl = ubetr / u(j)
      if ((infl < 0.1) .and. (knoten_rand(j) > 0)) then
         inflow(j) = .true.
      endif
      
      !! Sohlschubspannungsgeschwindigkeit berechnen (Nikuradse-Sandrauh Darcy-Weißbach)
      !!lami=0.1 !! ### vorläufig =zone(point_zone(j))%reib_ks reib_ks(point_zone(j))
      if (tief <= min_tief ) then ! trocken ! min_tief parameter aus module_modell
         utau = 0.0
      else ! nass
         ks = zone(point_zone(j))%reib
         zet = tief * 0.4665 ! 2D
         utau = ((lambda(ks,zet) / 8.)**0.5) * u(j)
      endif
      
      
      rb_hydraul(1+(j-1)*number_rb_hydraul) = u(j)
      rb_hydraul(2+(j-1)*number_rb_hydraul) = tief
      rb_hydraul(3+(j-1)*number_rb_hydraul) = p(j)
      !! benthic_distribution(44+(j-1)*number_benth_distr)=ks ! da sollte eigentlich der strickler-Wert stehen
      benthic_distribution(45+(j-1)*number_benth_distr) = utau
      
      if (j == kontrollknoten) then
         print*,'holen_trans: ', j,' p = ',p(j),' u = ', u(j), ' tief = ',tief,   &
          " utau = ",utau," Ks = ",zone(point_zone(j))%reib,' knoten_lage = ',knoten_x(j),knoten_y(j),knoten_z(j),  &
          ' ursprung',ur_x(j),ur_y(j),ur_z(j), ' inflow',inflow(j), ' ubetr', ubetr,' nt = ',nt
      endif
      
   enddo ! all j nodes
   
   print*,'Transport mit Datei ',trim(filename),' nt = ',nt,' Wasserpiegelflaeche = ',flaeche  &
         ,' Wasservolumen = ',volumen,' Anzahl trockene Knoten = ',trockzae
   
end subroutine holen_trans

!> Determine number of timesteps within a given period.
subroutine transinfo_schritte(start_zeitschritt, ende_zeitschritt)
   use modell
   implicit none

   integer(int64), intent(in) :: start_zeitschritt !< start time of interval
   integer(int64), intent(in) :: ende_zeitschritt  !< end time of interval
   
   integer        :: n, deti
   logical        :: gefunden

   if (ende_zeitschritt < start_zeitschritt) then
      call qerror("transinfo_schritte: start time is after end time.")
   endif
   
   gefunden = .false.
   na_transinfo = -1
   ne_transinfo = -1
   do n = 1,transinfo_anzahl
      if ((transinfo_zeit(transinfo_zuord(n)) > start_zeitschritt) .and. ( .not. gefunden)) then
         na_transinfo = n
         gefunden = .true.
      endif
   enddo
   
   if (stationaer) then
      na_transinfo = 1
      gefunden = .true.
   endif
   
   if ( .not. gefunden) then
      write(fehler,*)'Zum Anfangszeitpunkt des abgefragten Zeitintervalls existiert keine passende transportinfo Datei ' &
      ,start_zeitschritt
      call qerror(fehler)
   endif
   
   do n = 1,transinfo_anzahl
      if ((transinfo_zeit(transinfo_zuord(n)) > start_zeitschritt) .and. &
          (transinfo_zeit(transinfo_zuord(n)) <= ende_zeitschritt )) then
         ne_transinfo = n
      endif
   enddo
   
   if (stationaer) then
      ne_transinfo = deltat/dttrans
      gefunden = .true.
   endif
   
   if (ne_transinfo < na_transinfo) ne_transinfo = na_transinfo
   
   anz_transinfo = 1 + ne_transinfo - na_transinfo
   
   if (stationaer) then
      deti = dttrans*anz_transinfo
   else !instationär
      deti = transinfo_zeit(transinfo_zuord(ne_transinfo)) - transinfo_zeit(transinfo_zuord(na_transinfo))
      deti = deti + dttrans
   endif
   
   if (deti /= deltat .and. hydro_trieb == 1) then
      ! nur bei casu-Strombahnen abbrechen
      print*,'Zeitschrittweiten Transport = ',dttrans,' - Güte = ',deltat,' passen nicht zueinander.'
      print*,'ganzzahlige Vielfache erforderlich.'
      print*,'deti = ',deti
      print*,'anfang ',na_transinfo, transinfo_zuord(na_transinfo), transinfo_zeit(transinfo_zuord(na_transinfo))
      print*,'  ende ',ne_transinfo, transinfo_zuord(ne_transinfo), transinfo_zeit(transinfo_zuord(ne_transinfo))
      call qerror('deti /= deltat')
   endif
   
   print "(3(a,i0),a)", "Transport from ", start_zeitschritt,    &
                        " until ", ende_zeitschritt , " uses ",  &
                        anz_transinfo, " transport timesteps"
   
end subroutine transinfo_schritte


!> Read and order transport information.
subroutine transinfo_sichten()
   use module_datetime
   use modell
   implicit none
   
   character(200)      :: irgendeinstring
   character(longname) :: file_name, systemaufruf
   integer             :: system_error, open_error, alloc_status, io_error, errcode
   integer             :: n, nt, nz, np, i, ion, zwischenwert, delt, didi
   integer             :: year, month, day, hour, minute, second
   logical             :: offset_exists
   type(datetime)      :: datetime_meta, datetime_tmp
   
   print*,'Transportinformationen casu-transinfo sichten ...'
   file_name = trim(modellverzeichnis) // 'trafo'
   write(systemaufruf,'(4A)',iostat = errcode)'ls ',trim(modellverzeichnis),'transinfo > ', trim(file_name)
   if (errcode /= 0)call qerror('transinfo_sichten writing filename elemente_ failed')
   
   call system(trim(systemaufruf),system_error)
   if (system_error /= 0) then
      print*,trim(systemaufruf)
      call qerror('Auflisten der Transportinformationen fehlgeschlagen.')
   endif
   
   ion = 333
   open(unit = ion , file = file_name, status = 'old', action = 'read', iostat = open_error)
   if (open_error /= 0) call qerror('open_error trafo')

   nz = 0
   nt = 0
   do while (zeile(ion))
      nz = nz+1
      if (ctext(1:1) == 't') nt = nt+1
   enddo
   
   transinfo_anzahl = nt
   if (transinfo_anzahl < 1) call qerror('No transport info')
   
   rewind(ion)
   allocate(transinfo_zeit(transinfo_anzahl),  stat = alloc_status)
   allocate(transinfo_datei(transinfo_anzahl), stat = alloc_status)
   allocate(transinfo_zuord(transinfo_anzahl), stat = alloc_status)
   
   nt = 0
   np = 0
   do n = 1,nz,1
      if (.not. zeile(ion)) call qerror('2 .not. zeile(ion)')
      
      if (ctext(1:1) == 't') then
         nt = nt+1
         transinfo_datei(nt) = trim(ctext)
         
         i = len(trim(ctext))
         do while (ctext(i:i) /= 't')
            i = i-1
         enddo
         write(irgendeinstring,'(A)') ctext(i+1:len(trim(ctext)))
         read(irgendeinstring,*) transinfo_zeit(nt)
         
         transinfo_zuord(nt) = nt
      endif !! alle t* Dateien
   enddo ! alle zeilen aus trafo
   
   close(ion)
   
   
   file_name = trim(modellverzeichnis) // "transinfo/meta"
   open(unit = ion , file = file_name, status = 'old', action = 'read', iostat = open_error)
   if (open_error /= 0) call qerror('open_error transinfo/meta')
   
   offset_exists = .false.
   do while (zeile(ion))
      if (ctext(1:1) == '#') then ! Infos zum Modell
         print*, trim(ctext)
      
      else
         ! get timestep
         read(ctext,*,iostat = io_error) dttrans
         if (io_error /= 0) call qerror("Could not read `dttrans` from /transinfo/meta.")
            
         if (.not. zeile(ion)) then
            call qerror("Could not read second line in " // trim(file_name))
         endif
         
         if (.not. offset_exists) then
            read(ctext,*,iostat = io_error) day, month, year, hour, minute, second
            if (io_error /= 0) call qerror("Error while reading time_offset from file " // trim(file_name))
            
            offset_exists = .true.
            datetime_meta = datetime(year, month, day, hour, minute, second, tz = tz_qsim)
            
            print "(a,*(i0,x))", "meta-time       = ", day, month, year, hour, minute, second
            print "(a,i0)",      "meta-epoch-time = ", datetime_meta%seconds_since_epoch()
            
         endif 
      endif 
   enddo
   
   if (.not. offset_exists) call qerror("Could not find time offset in file " // file_name)
   
   do n = 1,transinfo_anzahl,1
      do i = n+1,transinfo_anzahl,1
         if (transinfo_zeit(transinfo_zuord(n)) > transinfo_zeit(transinfo_zuord(i))) then ! tauschen
            zwischenwert = transinfo_zuord(n)
            transinfo_zuord(n) = transinfo_zuord(i)
            transinfo_zuord(i) = zwischenwert
         endif ! Zeitreihenfolge falsch
      enddo ! alle weiteren i durch
   enddo ! alle n durch
   
   
   do n = 2,transinfo_anzahl,1
      delt = transinfo_zeit(transinfo_zuord(n)) - transinfo_zeit(transinfo_zuord(n-1))
      if (delt /= dttrans) then
         write(fehler,*)' ERROR unregelmäßiger Transportzeitschritt ',delt, 'sollte sein: ', dttrans &
         ,' n = ', n,trim(transinfo_datei(transinfo_zuord(n)))
         call qerror(fehler)
      endif 
   enddo
   
   
   print "(i0,a)", transinfo_anzahl,' transport  timesteps'
   datetime_tmp = as_datetime(transinfo_zeit(transinfo_zuord(1)), tz_qsim)
   print*, "from: ", datetime_tmp%date_string(), " ", trim(transinfo_datei(transinfo_zuord(1)))
   
   datetime_tmp = as_datetime(transinfo_zeit(transinfo_zuord(transinfo_anzahl)), tz_qsim)
   print*, "to: ", datetime_tmp%date_string(), " ", trim(transinfo_datei(transinfo_zuord(transinfo_anzahl)))
   print*, "with timesteps of ", dttrans, " seconds."

end subroutine transinfo_sichten
!----+-----+----
!> Die suboutine netz_lesen() ließt:\n
!! Das casu Netz wird mittels der Subroutinen points(), elements() und edges() 
!! von den Dateien point, file.elements und edges gelesen. \n
!! Das Netz von SCHISM wird mit netz_gr3() aus einer ELCIRC .gr3 Datei gelesen.
subroutine netz_lesen()
   use modell
   implicit none
   logical :: points, elements, netz_gr3, edges
   
   if (points()) then
      print*,"netz_lesen() aus transinfo/points + transinfo/file.elements"
      if ( .not. elements()) then
         write(fehler,*)'wenn transinfo/points da ist, müsste es auch ein transinfo/file.elements geben !!'
         call qerror(fehler)
      endif !! elements()
      kanten_vorhanden = edges()
   else
      if ( netz_gr3()) then
         print*,"netz_lesen() aus ecirc .gr3 Datei"
      else
         call qerror("netz_lesen() findet kein Netz, weder in points noch in .gr3")
      endif !netz_gr3
   endif ! points
end subroutine netz_lesen


!> Die suboutine netz_gr3() ließt das Netz aus der GR3-Datei: 
!! ## noch nicht implementiert ##
logical function netz_gr3()
   implicit none
   call qerror('netz_gr3 noch nicht implementiert')
   netz_gr3 = .true.
end function netz_gr3


!> Die suboutine points() ließt die Datei points:
!!
!! diese enthält die horizontalen Knotenorte, die Knotenhöhen und die 
!! Zonen-Nummern der Knoten.\n
!! <a href="./exp/points" target="_blank">Beispiel</a> 
!! Die Datei points wird von 
!! <a href="http://www.wasserimunterricht.de/wyrwa/casu12.html"  target="_blank">casu</a>
!! in das transinfo Verzeichnis ausgegeben. \n
!! Generiert wird points auch von casu:out dir t.
!! 
!! ## Dateiaufbau
!! In der ersten Zeile steht die Anzahl der Knoten; in den Folgezeilen je ein 
!! Knoten. Die ersten drei Zahlen in einer Knotenzeile sind die beiden 
!! horizontalen Koordinaten und die Sohlhöhe (x,y,z). \n
!! Danach folgen Zonennummer und Randnummer; als letztes ist die horizontale 
!! Fläche der Finite-Volumen Zelle aufgeführt, die von dem Knoten repräsentiert 
!! wird.
logical function points()
   use modell
   implicit none
   character(300) :: filename
   integer        :: open_error, string_read_error, ion, nknot, n, alloc_status
   
   modell_flaeche = 0.0
   print*,'Netzknoten aus Datei'
   
   filename = trim(modellverzeichnis) // 'transinfo/points'
   print*, trim(filename)
   
   open(newunit = ion , file = filename, status = 'old', action = 'read ', iostat = open_error)
   if (open_error /= 0) then
      points = .false.
      print*,'points open error; Annahme: nicht vorhanden.'
      return
   else
      points = .true.
   endif ! open_error.ne.0
   
   
   ! TODO: Remove goto
   777 continue
   if (zeile(ion)) then
      if (ctext(1:1) == '#') then
         print*,trim(ctext)
         goto 777
      endif
      read(ctext, *, iostat = string_read_error ) knotenanzahl2D
      if (string_read_error /= 0) then
         write(fehler,*)'string_read_error subroutine points nknot'
         call qerror(fehler)
      endif ! open_error.ne.0
      print*,knotenanzahl2D, 'Knoten sollen in points sein'
      nonu = knotenanzahl2D ! kontrollwert
   else
      write(fehler,*)'Lesen der knotenanzahl2D im Kopf von point fehlgeschlagen'
      call qerror(fehler)
   endif !erste zeilen aus points gelesen
   
   !knotenanzahl2D=knotenanzahl3D
   print*,'Momentan noch 2D-Tiefengemittelt'
   allocate (knoten_x(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_x(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   endif
   
   allocate (knoten_y(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_y(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   endif
   
   allocate (knoten_z(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_z(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   endif
   
   allocate (knoten_rand(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_rand(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   endif
   
   allocate (knoten_zone(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_zone(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   endif
   
   allocate (knoten_flaeche(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_flaeche(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   endif
   
   ! Knotenzeilen nacheinander einlesen
   n = 0
   do while ( zeile(ion))
      n = n+1
      read(ctext(1:2000), *, iostat = string_read_error ) &
           knoten_x(n), knoten_y(n), knoten_z(n),          &
           knoten_zone(n), knoten_rand(n), knoten_flaeche(n)
      if (string_read_error /= 0) then
         write(fehler,*)'Lesen fehlgeschlagen aus points an knoten #', n,' read_error:', string_read_error, &
         ' points wird von casu auf /transinfo ausgegeben. Nicht die converti-Version verwenden!'
         call qerror(fehler)
      endif
      if (knoten_zone(n) < 0) then
         write(fehler,*)' knoten #',n,': Zonennummer darf nicht negativ sein'
         call qerror(fehler)
      endif
      if (knoten_rand(n) < 0) then
         write(fehler,*)' knoten #',n,': Randnummer darf nicht negativ sein'
         call qerror(fehler)
      endif
      if (knoten_flaeche(n) <= 0.0) then
         write(fehler,*)' knoten #',n,': Knotenfläche muss größer Null sein'
         call qerror(fehler)
      endif
      modell_flaeche = modell_flaeche+knoten_flaeche(n)
   enddo ! zeile
   
   if (n /= knotenanzahl2D) call qerror('Zeilenzahl falsch in Datei points')
   
   close (ion)
   
   print*, 'x-koordinate max+min', maxval(knoten_x), minval(knoten_x)
   print*, 'y-koordinate max+min', maxval(knoten_y), minval(knoten_y)
   print*, 'Sohlhöhe max+min',     maxval(knoten_z),  minval(knoten_z)
   print*, 'Zonen# von ',          minval(knoten_zone), ' bis ', maxval(knoten_zone)
   print*, 'Rand# von ',           minval(knoten_rand), ' bis ', maxval(knoten_rand)
   print*, 'modell_flaeche = ',modell_flaeche
   print*, 'Von ',knotenanzahl2D,' Knoten sind ', count(knoten_rand > 0) ,' Randknoten'
   
   mittelflaech = 0.0
   mittelvolumen = 0.0
   
end function points

!> Die subroutine elements() ließt Vermaschung von der Datei
!! <a href="./exp/file.elements" target="_blank">file.elements</a>.
!! In der ersten Zeile steht die Anzahl der Elemente,
!! in den folgezeilen steht je ein Element . der erste Integer in der Zeile ist 
!! 3 oder 4 und gibt an, ob es sich um ein Drei- oder Vieleck handelt. 
!! Danach folgen 3 oder 4 Knotennummern 
!! ## ACHTUNG ## Knotennummerierung beginnt bei Null
logical function elements()
   use modell
   implicit none
   character(longname) :: filename, systemaufruf
   integer             :: ndumm, n, j, alloc_status, ion, open_error, string_read_error, system_error, errcode
   logical             :: zeile_vorhanden, exists
   
   ! Datei file.elements lesen falls vorhanden
   filename = trim(modellverzeichnis) // 'transinfo/file.elements'
   inquire(file = filename, exist = exists)
   
   if (.not. exists) call qerror("Missing file " // trim(filename))
   
   element_vorhanden = .true.
   open(newunit = ion , file = filename, status = 'old', action = 'read ', iostat = open_error)
   zeile_vorhanden = zeile(ion)
   read(ctext, *, iostat = string_read_error ) n_elemente
   if (string_read_error == 0) then
      print*,'file.elements mit ',n_elemente,' Elementen'
   else
      call qerror('string_read_error ausgabe.f95, file.elements')
   endif
   
   allocate(cornernumber(n_elemente))
   allocate(elementnodes(n_elemente,4))
   
   summ_ne = 0
   do n = 1,n_elemente
      if (zeile(ion)) then
         read(ctext, *, iostat = string_read_error ) cornernumber(n)
         if (cornernumber(n) == 3) then ! Dreieck
            read(ctext, *, iostat = string_read_error ) ndumm, &
                 elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
         else ! nicht dreieck
            if (cornernumber(n) == 4) then ! Vieleck
               read(ctext, *, iostat = string_read_error ) ndumm, &
                    elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
            else ! weder Drei- noch Viereck
               write(fehler,*)'weder Drei- noch Viereck ',n
               call qerror(fehler)
            endif !Viereck
         endif !Dreieck
      else ! Zeile nicht lesbar
         write(fehler,*)'Lesen aus file.elements fehlgeschlagen'
         call qerror(fehler)
      endif !Zeile gelesen
      do j = 1,cornernumber(n)
         elementnodes(n,j) = elementnodes(n,j)+1
      enddo ! alle Knoten im Element
      summ_ne = summ_ne+cornernumber(n)+1
   enddo
   close(ion)

   ! bei casu-Netzen hat der Knoten die Zone, vorsichtshalber wird sie hier auf -7 initialisiert
   allocate (element_zone(n_elemente), source = -7, stat = alloc_status )
   if (alloc_status /= 0) call qerror("Error while allocating variable `element_zone`.")
   print*,' allocate (element_zone(n_elemente) worked elements',meinrang,n_elemente
   elements = .true.
   print*,'logical function elements(), module_modell.f95, hat aus file.elements ',n_elemente,' Elemente gelesen'

end function elements
!----+-----+----
!> function edges()
!! liesst die Datei edges, falls vorhanden:
!! Informationen zu den Elementkanten
!! die Zonen-Nummern der Knoten.
!! <a href="./exp/edges" target="_blank">Beispiel</a> 
!!
!! ## Ausschnitt
!!  casu Modell: /mreferate/wyrwa/casulli/test08/ue4
!!  casu Version 5. 5. 2015  edgenumber=141544
!!  top_node,bottom_node ; left_element,right_element ; edge_length,ground ; cell_bound_length,dist_left,dist_right,false_dist ; e.x,e.y
!! ; boundary_type,boundary_face,boundary_number ; zone
!! 12  16  0  3769  4.78167  -0.42545  11.0632  5.78884  5.27436  3.31291e-12  0.836527 -0.547925  -1  -1  -1  5\n
!! 4517  12  0  3763  11.8354  -0.48355  3.87221  2.05119  1.82102  -5.05151e-13  0.498503 0.866888  -1  -1  -1  5\n
logical function edges()
   use modell
   character (len = 300) :: filename
   integer :: n, ion, alloc_status, io_error
   real    :: ground, dist_left, dist_right, false_dist, dummy1, dummy2
   integer :: boundary_type, boundary_face
   
   filename = trim(modellverzeichnis) // 'transinfo/edges'
   print*,trim(filename)
   
   open(newunit = ion , file = filename, status = 'old', action = 'read ', iostat = io_error)
   if (io_error /= 0) then
      edges = .false.
      print*,'transinfo/edges open error; wohl keine kanteninformationen vorhanden'
      return
   else
      edges = .true.
      print*,'Datei transinfo/edges vorhanden'
   endif ! open_error.ne.0
   
   if (zeile(ion))read(ctext, *) kantenanzahl
   n = 0
   do while ( zeile(ion))
      n = n+1
   enddo ! nächste zeile vorhanden
   
   print*,"transinfo/edges: kantenanzahl = ",kantenanzahl," n = ",n
   allocate (top_node(kantenanzahl), stat = io_error )
   allocate (bottom_node(kantenanzahl), stat = io_error )
   allocate (left_element(kantenanzahl), stat = io_error )
   allocate (right_element(kantenanzahl), stat = io_error )
   allocate (cell_bound_length(kantenanzahl), stat = io_error )
   allocate (edge_normal_x(kantenanzahl),edge_normal_y(kantenanzahl), stat = io_error )
   allocate (boundary_number(kantenanzahl), stat = io_error )
   allocate (zon_num(kantenanzahl), stat = io_error )
   if (io_error /= 0) then
      edges = .false.
      write(fehler,*)'alloc_error in edges = ', io_error
      call qerror(fehler)
   endif !! alloc_error
   rewind(ion) !! nochmal von vorne:
   if (zeile(ion))read(ctext, *) kantenanzahl
   n = 0
   do while ( zeile(ion))
      n = n+1
      read(ctext, *, iostat = io_error ) top_node(n), bottom_node(n), left_element(n), right_element(n),  &
           dummy1, ground, cell_bound_length(n), dist_left, dist_right, false_dist,  &
           edge_normal_x(n),edge_normal_y(n),boundary_type,boundary_face,boundary_number(n),zon_num(n)
      
      if (io_error /= 0) then
         edges = .false.
         write(fehler,*)'io_error = ', io_error,' at edge #',n
         call qerror(fehler)
      endif
      top_node(n) = top_node(n) + 1 !! Zählweise Fortran ab 1, C++ ab 0
      bottom_node(n) = bottom_node(n) + 1
      left_element(n) = left_element(n) + 1
      right_element(n) = right_element(n) + 1
      
      if ((top_node(n) == kontrollknoten) .or. (bottom_node(n) == kontrollknoten)) then
         print*,'Kante #', n, "hat den kontrollknoten # ",kontrollknoten," als Top oder Bottom",top_node(n),bottom_node(n)
         print*,"cell_bound_length = ",cell_bound_length(n)
         print*,"left_element right_element = ",left_element(n), right_element(n)
         print*,"boundary_number zone = ",boundary_number(n),zon_num(n)
      endif
   enddo 
end function edges

