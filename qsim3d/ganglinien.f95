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

!> ganglinien_parallel
!! \n\n
subroutine ganglinien_parallel()
   use modell
   implicit none
   integer :: i,j, alloc_status, agp
   call MPI_Bcast(anz_gangl,1,MPI_INT,0,mpi_komm_welt,ierr)
   if (meinrang /= 0) allocate (knot_gangl(anz_gangl), stat = alloc_status )
   call MPI_Bcast(knot_gangl,anz_gangl,MPI_INT,0,mpi_komm_welt,ierr)
   !! Knoten aussortieren, die nicht zu diesem Prozess gehören
   agp = 0 !!
   do i = 1,anz_gangl
      if ((knot_gangl(i) > meinrang*part) .and. (knot_gangl(i) <= (meinrang+1)*part)) then
         agp = agp+1
         print*,'ganglinien_parallel: Knoten #', knot_gangl(i),' macht Prozess #',meinrang," part = ",part
      else
         knot_gangl(i) = 0
      end if
   end do
   call MPI_Bcast(zeitschrittanzahl,1,MPI_INT,0,mpi_komm_welt,ierr)
   allocate (r_gang(anz_gangl,zeitschrittanzahl+1), stat = alloc_status )
   allocate (t_gang(anz_gangl,zeitschrittanzahl+1), stat = alloc_status )
   allocate (u_gang(anz_gangl,zeitschrittanzahl+1), stat = alloc_status )
   
   ! planktonic variables
   call MPI_Bcast(output_plankt,number_plankt_vari,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(output_plankt_vert,number_plankt_vari_vert,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(n_pl,1,MPI_INT,0,mpi_komm_welt,ierr)
   print*,'ganglinien_parallel:', n_pl,' planktische Variablen für ganglinienausgabe (n_pl)'
   allocate (pl_gang(anz_gangl,zeitschrittanzahl+1,n_pl), stat = alloc_status )
   if (alloc_status /= 0) call qerror("allocate (pl_gang fehlgeschlagen")
   
   ! Randflüsse:
   ! 1 WSP-Länge, 2 Querschnittsfläche 3=Volumenrstrom, 4=potentielle und 5=kinetische Energie;
   ! 6 = Fluss des passiven alters-tracers planktonic_variable(71
   allocate (randflux_gang(ianz_rb,zeitschrittanzahl+1,6), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)'allocate (randflux_gang(ianz_rb, fehlgeschlagen'
      call qerror(fehler)
      !         else
      !            print*,"Speicherplatz wurde allociert für Randflüsse an ",ianz_rb," Rändern für ",zeitschrittanzahl+1  &
      !     &            ," Zeitpunkte (Initialisierung wird mitnotiert)."
      !            print*,"An diesen Rändern werden 5 Größen ermittelt"  &
      !     &            ," (Wasserspiegellänge, Querschnittsfläche, Volumenstrom, Fluss der potentiellen und kinetischen Energie)."  &
      !     &            ," Ausserdem werden die Massenflüsse für die ",n_pl  &
      !     &            ," Variablen bestimmt, die in ausgabekonzentrationen.txt gewählt wurden."
   end if ! alloc_status .ne.0
   ! benthic distributions are not gathered. therefore process 0 can do no time-series output
   call MPI_Bcast(output_benth_distr,number_benth_distr,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(n_bn,1,MPI_INT,0,mpi_komm_welt,ierr)
   allocate (bn_gang(anz_gangl,zeitschrittanzahl+1,n_bn), stat = alloc_status )
   !transfer_quantities
   call MPI_Bcast(output_trans_val,number_trans_val,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(output_trans_quant,number_trans_quant,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(output_trans_quant_vert,number_trans_quant_vert,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(n_ue,1,MPI_INT,0,mpi_komm_welt,ierr)
   allocate (ue_gang(anz_gangl,zeitschrittanzahl+1,n_ue), stat = alloc_status )
end subroutine ganglinien_parallel
!
!----+-----+----
!> ganglinien_oeffnen
!! \n\n
subroutine ganglinien_lesen()
   use modell
   implicit none
   character(200) dateiname, nummer, systemaufruf
   integer :: open_error, io_error, alloc_status, knumm, nn, i, sys_error, j
   logical :: querschnitt_lesen
   if (meinrang > 0) then !! has to be on process 0
      write(fehler,*)' 7 ganglinien_oeffnen auf process #',meinrang,' ... darf nicht sein'
      call qerror(fehler)
   end if
   anz_gangl = 0
   nn = 0
   ionumber = 777
   write(dateiname,'(2A)')trim(modellverzeichnis),'ganglinien_knoten.txt'
   open ( unit = ionumber , file = dateiname, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0) then
      print*,'keine ganglinien_knoten.txt'
      return
   end if ! open_error.ne.0
   do while ( zeile(ionumber)) !! zunächst Anzahl der Ganglinien feststellen
      if (ctext(1:1) /= '#') then !! keine Kommentarzeile
         anz_gangl = anz_gangl+1
         read(ctext,*,iostat = io_error)knumm
         if (io_error /= 0) then
            write(fehler,*)'knumm nicht richtig aus ganglinien_knoten.txt gelesen'
            call qerror(fehler)
         end if ! io_error.ne.0
         print*,'ganglinien_knoten.txt  ',anz_gangl,' :', trim(ctext), ' #',knumm
      end if !! keine Kommentarzeile
   end do ! zeile
   rewind (ionumber) ! ganglinien_knoten.txt zurückspulen
   allocate (knot_gangl(anz_gangl), stat = alloc_status )
   !allocate (c3Ammonium(anz_gangl), stat = alloc_status )
   !allocate (IntAmmonium(anz_gangl), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)'allocate knot_gangl(anz_gangl) fehlgeschlagen alloc_status = ', alloc_status
      call qerror(fehler)
   end if !
   do while ( zeile(ionumber)) !! alle Zeilen
      if (ctext(1:1) /= '#') then !! keine Kommentarzeile
         nn = nn+1
         if (nn > anz_gangl) then
            write(fehler,*)'Fehler bei ganglinien_knoten.txt nochmal lesen ',nn,anz_gangl
            call qerror(fehler)
         end if
         read(ctext,*,iostat = io_error)knot_gangl(nn)
      end if !! keine Kommentarzeile
   end do ! alle zeilen
   close (ionumber) ! ganglinien_knoten.txt wieder geschlossen
   print*,' ganglinien_lesen fertig | anz_gangl = ',anz_gangl
   !do i=1,anz_gangl
   !   print*,'knot_gangl(',i,')=', knot_gangl(i)
   !end do
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         call randlinie_zusammenstellen()
         do i = 1,anz_gangl
            if ((knot_gangl(i) <= 0) .or. (knot_gangl(i) > knotenanzahl2D)) then
               write(fehler,*)i,'ganglinien_knoten.txt nummer ',knot_gangl(i),' nicht zwischen 1 und ',knotenanzahl2D
               call qerror(fehler)
            end if ! Knotennummer ungültig in diesem Modell
         end do
      case(2) ! Untrim² netCDF
         print*,'Konzentrationsvariablen bei Untrim-Netzen sind Element-Mitten,'
         print*,'daher müssen in ganglinien_knoten.txt Elementnummern eingetragen werden'
         do i = 1,anz_gangl
            if ((knot_gangl(i) < 1) .or. (knot_gangl(i) > n_elemente)) then
               write(fehler,*)'ganglinien_lesen,Untrim ### Element-Nummer falsch ###: knot_gangl(',i,') = ' &
                                                                                                          ,knot_gangl(i),'nicht zwischen 1 und ',n_elemente
               call qerror(fehler)
            else
               print*,'ganglinien nr. ',i,' am Element ',knot_gangl(i),' Ort: ',element_x(knot_gangl(i)), element_y(knot_gangl(i))
            endif
         end do
         !print*,'### randlinie_zusammenstellen und querschnitt_lesen für Untrim netCDF noch nicht implementiert ###'
      case(3) ! SCHISM
         print*,'no cross sections possible with SCHISM'
         case default
         call qerror('ganglinien_lesen: Hydraulischer Antrieb unbekannt')
   end select
   
end subroutine ganglinien_lesen
!
!----+-----+----
!> ganglinien_zeitschritt
!! \n\n
subroutine ganglinien_zeitschritt(izeit_gang)
   use modell
   implicit none
   integer :: i, k, izeit_gang, n,nk !! Zeitschrittzähler
   !print*,'ganglinien_zeitschritt meinrang=', meinrang,' izeit_gang=',izeit_gang,' anz_gangl=',anz_gangl
   do i = 1,anz_gangl
      nk = knot_gangl(i)-meinrang*part
      if ((nk > 0) .and. (nk <= part)) then
         !            print*,'ganglinien_zeitschritt knot_gangl(',i,')=', knot_gangl(i),  &
         !    &              ' macht Prozess #',meinrang, ' part=',part,' zeitpunkt=',zeitpunkt
         ! Zeitpunkt
         r_gang(i,izeit_gang) = zeitpunkt
         ! Randbedingungen:
         u_gang(i,izeit_gang) = rb_hydraul_p(1+(nk-1)*number_rb_hydraul) !u(knot_gangl(i))  randbedingungen.f95
         !t_gang(i,izeit_gang)= rb_hydraul_p(2+(nk-1)*number_rb_hydraul) !tief(knot_gangl(i))   randbedingungen.f95
         t_gang(i,izeit_gang) = rb_hydraul_p(3+(nk-1)*number_rb_hydraul) !wsp
         !planktonic_variable
         n = 0
         do k = 1,number_plankt_vari
            if (output_plankt(k)) then ! planktic output conc.
               n = n+1
               if (n > n_pl) then
                  write(fehler,*)'ganglinien_zeitschritt output_plankt(k) (n > n_pl)'  &
                                                                         ,k,n,n_pl, trim(planktonic_variable_name(k))
                  call qerror(fehler)
               endif
               pl_gang(i,izeit_gang,n) = planktonic_variable_p(k+(nk-1)*number_plankt_vari)
            endif ! planktic output conc.
         end do ! alle k planktonic_variable
         do k = 1,number_plankt_vari_vert
            if (output_plankt_vert(k)) then ! planktic_vert output conc.
               n = n+1
               if (n > n_pl) then
                  write(fehler,*)'ganglinien_zeitschritt output_plankt_vert(k) (n > n_pl)'  &
                                                                              ,k,n,n_pl, trim(plankt_vari_vert_name(k))
                  call qerror(fehler)
               endif
               pl_gang(i,izeit_gang,n) = &
                                         plankt_vari_vert_p(gangl_level+(k-1)*num_lev+(nk-1)*number_plankt_vari_vert*num_lev)
            endif ! planktic output conc.
         end do ! alle k planktonic_variable
         ! benthic distributions are not gathered. therefore process 0 can do no time-series output
         n = 0
         do k = 1,number_benth_distr
            if (output_benth_distr(k)) then ! benth. distr. output
               n = n+1
               if (n > n_bn) call qerror('ganglinien_zeitschritt n > n_bn')
               bn_gang(i,izeit_gang,n) = benthic_distribution_p(k+(nk-1)*number_benth_distr)
            endif
         end do
         !transfer_quantities are not gathered. therefore process 0 can do no time-series output
         n = 0
         do k = 1,number_trans_val
            if (output_trans_val(k)) then ! globale Übergabe Werte
               n = n+1
               if (n > n_ue) call qerror('1 ganglinien_zeitschritt (n > n_ue) ')
               ue_gang(i,izeit_gang,n) = transfer_value_p(k)
            endif ! exchange output conc.
         end do
         do k = 1,number_trans_quant
            if (output_trans_quant(k)) then ! exchange con. output
               n = n+1
               if (n > n_ue) call qerror('2 ganglinien_zeitschritt (n > n_ue) ')
               ue_gang(i,izeit_gang,n) = transfer_quantity_p(k+(nk-1)*number_trans_quant)
            endif ! exchange output conc.
         end do
         do k = 1,number_trans_quant_vert
            if (output_trans_quant_vert(k)) then ! exchange con. output
               n = n+1
               if (n > n_ue) call qerror('3 ganglinien_zeitschritt (n > n_ue) ')
               ue_gang(i,izeit_gang,n) = &
                                         trans_quant_vert_p(gangl_level+(k-1)*num_lev_trans+(nk-1)*num_lev_trans*number_trans_quant_vert)
            endif ! exchange output conc.
         end do
         ! Integrations
         ! IntAmmonium(i)=IntAmmonium(i)+ (planktonic_variable(3,knot_gangl(i))+c3Ammonium(i))/2.0
         ! c3Ammonium(i)=planktonic_variable(3,knot_gangl(i))
      end if ! knoten auf diesem Prozess
   end do ! alle i Ausgabe-Knoten
   if (meinrang == 0) then
      if (hydro_trieb == 1) then ! hydro_trieb=1=casu
         if ( nur_alter ) call alter_zeitschritt(izeit+1)
         !   call tagesmittelwert()
         if (n_pl > 200)call qerror("n_pl > 200 geht nicht wegen fester Feldgröße massen_flux")
         !print*,'ganglinien_zeitschritt:, izeit_gang=',izeit_gang,' zeitpunkt=',zeitpunkt,'  Knoten-Werte notiert'
         !call rand_flux(izeit+1)
         !print*,'ganglinien_zeitschritt: rand_flux durchgelaufen'
         !call querschnitt_flux(izeit+1)
         !print*,'ganglinien_zeitschritt: querschnitt_flux durchgelaufen'
      end if ! hydro_trieb=1=casu
   end if ! meinrang.eq. 0
   call mpi_barrier (mpi_komm_welt, ierr)
end subroutine ganglinien_zeitschritt
!!
!----+-----+----
!> ganglinien_schliessen
!! \n\n
subroutine ganglinien_schliessen()
   use modell
   implicit none
   character (len = longname) :: systemaufruf, dateiname
   character (len = 300) nummer
   character(40000) beschriftung, r_zeile
   character(3) spalte
   integer :: i,j,k,n, sys_error, open_error, ngnu, nk, errcode
   integer :: time_style
   time_style = 0 ! Gerris
   !time_style=1 ! Ausgabeformat Stil wsa_cux
   !time_style=2 ! sekunden a la SCHISM
   if (meinrang == 0) then ! prozess 0 only
      if (time_style == 0)print*,"ganglinien_schliessen: Ausgabeformat Stil: portal-Tideelbe + gerris"
      if (time_style == 1)print*,"ganglinien_schliessen: Ausgabeformat Stil wsa_cux"
      if (time_style == 2)print*,"ganglinien_schliessen: Ausgabeformat Stil SCHISM"
      !write(dateiname,'(2A)')trim(modellverzeichnis),'ganglinien_knoten.txt'
      write(systemaufruf,'(3A)',iostat = errcode)'rm -rf ',trim(modellverzeichnis),'ganglinien_bisher'
      if (errcode /= 0)call qerror('ganglinien_schliessen writing systemcall rm -rf ganglinien_bisher failed')
      call system(systemaufruf,sys_error)
      if (sys_error /= 0) then
         write(fehler,*)'rm -rf ganglinien_bisher  failed sys_error = ', sys_error
         call qerror(fehler)
      end if !
      write(systemaufruf,'(5A)',iostat = errcode)'mv -f ',trim(modellverzeichnis),'ganglinien '  &
                                         ,trim(modellverzeichnis),'ganglinien_bisher > /dev/null 2 > /dev/null'
      if (errcode /= 0)call qerror('ganglinien_schliessen writing systemcall mv ganglinien failed')
      call system(systemaufruf,sys_error)
      if (sys_error /= 0) then
         print*,'mv -f ganglinien ganglinien_bisher  meldet: sys_error = ', sys_error
         !write(fehler,*)'mv -f ganglinien ganglinien_bisher  meldet: sys_error=', sys_error
         !call qerror(fehler)
      end if !
      write(systemaufruf,'(3A)',iostat = errcode)'mkdir ',trim(modellverzeichnis),'ganglinien'
      if (errcode /= 0)call qerror('ganglinien_schliessen writing systemcall mkdir ganglinien failed')
      call system(systemaufruf,sys_error)
      if (sys_error /= 0) then
         write(fehler,*)'mkdir ganglinien  fehlgeschlagen sys_error = ', sys_error
         call qerror(fehler)
      end if !
      do i = 1,40000
         beschriftung(i:i) = ' '
      end do
      
      select case (time_style)
         case(0) !  Gerris
            write(beschriftung,'(A)')"#   Datum| Uhrzeit|    Zeitpunkt|       WSP004|   Geschw.005|"
            !                         2011-03-01 00:03:00      36622980          1.43          0.27
            !write(beschriftung,'(A)')"#            Datum|    Zeitpunkt|     Tiefe004|   Geschw.005|"
            !write(beschriftung,'(A)')"#        Zeitpunkt|     Tiefe003|   Geschw.004|"
            ! write(beschriftung,'(I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
            !&                     jahr  ,monat ,tag   ,stunde,minute,sekunde   !r_gang(i,j)
            ngnu = 5
         case(1) ! Ausgabeformat Stil wsa_cux
            write(beschriftung,'(A)')"#        Zeitpunkt|     Tiefe003|   Geschw.004|"
            !   write(beschriftung,'(I2.2,".",I2.2,".",I4.4," ",I2.2,":",I2.2,":",I2.2)') &
            !&                                 tag  ,monat ,Jahr   ,stunde,minute,sekunde
            ngnu = 4
         case(2) !  sekunden a la SCHISM
            write(beschriftung,'(A)')"#        Zeitpunkt|"
            !   write(beschriftung,*) zeitpunkt
            ngnu = 2
            case default
            call qerror("time_style falsch in Kopfzeile ganglinien_schliessen()")
      end select
      !! planktonic variables
      do i = 1,number_plankt_vari
         if (output_plankt(i)) then ! planktic output conc.
            ngnu = ngnu+1
            write(spalte,'(I3.3)')ngnu
            beschriftung = trim(beschriftung)//planktonic_variable_name(i)//spalte//"|"
            !print*,"output_plankt:",i,ngnu,trim(beschriftung)
         end if
      end do
      do i = 1,number_plankt_vari_vert
         if (output_plankt_vert(i)) then ! planktic_vert output conc.
            ngnu = ngnu+1
            write(spalte,'(I3.3)')ngnu
            beschriftung = trim(beschriftung)//plankt_vari_vert_name(i)//spalte//"|"
            !print*,"output_plankt_vert:",i,ngnu,trim(beschriftung)
         end if
      end do
      !! benthic distributions are not gathered. therefore process 0 can do no time-series output
      do i = 1,number_benth_distr
         if (output_benth_distr(i)) then ! benth. distr. output
            ngnu = ngnu+1
            write(spalte,'(I3.3)')ngnu
            beschriftung = trim(beschriftung)//benth_distr_name(i)//spalte//"|"
            !print*,"output_benth_distr:",i,ngnu,trim(beschriftung)
         end if
      end do
      !transfer_quantities
      do i = 1,number_trans_val
         if (output_trans_val(i)) then ! globale Übergabe Werte
            ngnu = ngnu+1
            write(spalte,'(I3.3)')ngnu
            beschriftung = trim(beschriftung)//trans_val_name(i)//spalte//"|"
            !print*,"output_trans_val:",i,ngnu,trim(beschriftung)
         end if
      end do
      do i = 1,number_trans_quant
         if (output_trans_quant(i)) then ! exchange con. output
            ngnu = ngnu+1
            write(spalte,'(I3.3)')ngnu
            beschriftung = trim(beschriftung)//trans_quant_name(i)//spalte//"|"
            !print*,"output_trans_quant:",i,ngnu,trim(beschriftung)
         end if
      end do
      do i = 1,number_trans_quant_vert
         if (output_trans_quant_vert(i)) then ! exchange con. output
            ngnu = ngnu+1
            write(spalte,'(I3.3)')ngnu
            beschriftung = trim(beschriftung)//trans_quant_vert_name(i)//spalte//"|"
            !print*,"output_trans_quant_vert:",i,ngnu,trim(beschriftung)
         end if
      end do
      print*,'Ganglinenausgabe der tiefenverteilten Variablen nur in der Schicht gangl_level = ',gangl_level
      ! if(nur_alter) call alter_ausgabe() !! Alters-Ausgabe
      ! Rand-Flüsse ausgeben
      print*,'Z.Z. keine Ausgabe von Rand-Flüssen'
      goto 123
      do n = 1,ianz_rb !! alle Ränder
         write(nummer,*)rabe(n)%nr_rb
         write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'ganglinien/r',trim(adjustl(nummer)),'.txt'
         if (errcode /= 0)call qerror('ganglinien_schliessen writing filename ganglinien/r*.txt failed')
         open ( unit = 12345+n , file = dateiname, status = 'new', action = 'write ', iostat = open_error )
         write(12345+n,*)"## Randflüsse (lang,flaeche,vol_strom, pot_ener_flux(MW), kin_ener_flux, massen_flux71) für "  &
                                                                                   ,n,"-ten Rand, Nr. = ",rabe(n)%nr_rb,"  ##" ! Kopfzeile schreiben
         do j = 1,zeitschrittanzahl+1
            zeitpunkt = r_gang(1,j)
            call zeitsekunde()
            write(r_zeile,'(I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
                                                                              jahr  ,monat ,  tag  ,   stunde , minute , sekunde
            do i = 1, 6
               write(r_zeile,'(A,6x,F16.9)')trim(r_zeile), randflux_gang(n,j,i)   ! r_gang(i,j)
            end do ! 5 (eigentlich alle Randfüsse incl. Massenflüsse)
            write(12345+n,'(A)')trim(adjustl(r_zeile))
         end do ! alle j Zeitschritte
         rewind(12345+n)
         close(12345+n) !
         print*,'Ausgabe Rand-Fluss ', trim(dateiname), ' meinrang = ',meinrang," n_pl = ",n_pl
      end do ! alle Ränder
      123  continue
      ! --- output cross section fluxes ---
      if (querschneiden) then
         do n = 1,anzahl_quer !! all n cross sections
            write(nummer,*)n
            write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'ganglinien/q',trim(adjustl(nummer)),'.txt'
            if (errcode /= 0)call qerror('ganglinien_schliessen writing filename ganglinien/q*.txt failed')
            open ( unit = 9876+n , file = dateiname, status = 'new', action = 'write ', iostat = open_error )
            write(r_zeile,*)"#   Datum| Uhrzeit| Zeitpunkt(sec.) | Durchfluss | Wasservolumen im Zeitschritt = ",deltat
            do i = 1,number_plankt_vari
               if (output_plankt(i)) then ! planktic output conc.
                  r_zeile = trim(r_zeile)//planktonic_variable_name(i)//" | "
               end if
            end do
            write(9876+n,*)trim(adjustl(r_zeile))! write header
            do j = 1,zeitschrittanzahl
               zeitpunkt = q_gangl(j)
               call zeitsekunde()
               write(r_zeile,'(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2,x,I13)')  &
                                                                                         jahr  ,monat ,tag   ,stunde,minute,sekunde,zeitpunkt
               do i = 1,n_pl+2
                  write(r_zeile,'(A,6X,E16.10)')trim(r_zeile),schnittflux_gang(n,j,i)
               end do ! all i fluxes
               write(9876+n,'(A)')trim(r_zeile)
            end do ! all j timesteps
            rewind(9876+n) !
            close(9876+n) !
            print*,'Ausgabe Querschnitts-Fluss ', trim(dateiname), ' meinrang = ',meinrang," n_pl = ",n_pl
         end do ! alle n cross sections
      endif ! querschneiden
   end if ! only prozessor 0
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Bcast(beschriftung,40000,MPI_CHARACTER,0,mpi_komm_welt,ierr)
   ! lueftung! beschriftung=trim(beschriftung)//"|        delo2_last|"
   do i = 1,anz_gangl
      if (knot_gangl(i) > 0) then !Knoten auf diesem Prozess
         write(nummer,*)knot_gangl(i)
         write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'ganglinien/g',trim(adjustl(nummer)),'.txt'
         if (errcode /= 0)call qerror('ganglinien_schliessen writing filename ganglinien/g*.txt failed')
         open ( unit = ionumber+i , file = dateiname, status = 'new', action = 'write ', iostat = open_error )
         if (open_error /= 0) then
            print*,'Ganglinien-Ausgabedatei #',i," fuer Knoten ",knot_gangl(i),' geht nicht'
            return
         end if ! open_error.ne.0
         write(ionumber+i,'(A)')trim(beschriftung) ! Kopfzeile schreiben
         nk = knot_gangl(i)-meinrang*part
         write(ionumber+i,*)"# Gelaendehoehe = ", &
                                               rb_hydraul_p(3+(nk-1)*number_rb_hydraul)-rb_hydraul_p(2+(nk-1)*number_rb_hydraul) ! Geländehöhe rückrechnen
      end if ! knoten auf diesem Prozess
   end do ! alle Ausgabe-Knoten
   do i = 1,anz_gangl
      if (knot_gangl(i) > 0) then !Knoten auf diesem Prozess
         do j = 1,zeitschrittanzahl+1
            !write(beschriftung,'(I12.2, 2("    ",F7.2))')r_gang(i,j), t_gang(i,j), u_gang(i,j)
            zeitpunkt = r_gang(i,j)
            call zeitsekunde()
            select case (time_style)
               case(0) !  Gerris
                  write(beschriftung,'(I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2,x,I13)') &
                                                                                               jahr  ,monat ,tag   ,stunde,minute,sekunde,zeitpunkt   !r_gang(i,j)
               case(1) ! Ausgabeformat Stil wsa_cux
                  write(beschriftung,'(I2.2,".",I2.2,".",I4.4," ",I2.2,":",I2.2,":",I2.2)') &
                                                                                           tag  ,monat ,Jahr   ,stunde,minute,sekunde
               case(2) !  sekunden a la SCHISM
                  write(beschriftung,*) zeitpunkt
                  case default
                  call qerror("time_style falsch in ganglinien_schliessen()")
            end select
            !            write(beschriftung,'(I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
            !     &                           monat ,tag   ,stunde,minute,sekunde   !r_gang(i,j)
            write(beschriftung,'(A, 2("       ",F7.2))')trim(beschriftung), t_gang(i,j), u_gang(i,j)
            do k = 1,n_pl
               write(beschriftung,2578)trim(beschriftung),pl_gang(i,j,k)
            end do
            do k = 1,n_bn
               write(beschriftung,2578)trim(beschriftung),bn_gang(i,j,k)
            end do
            do k = 1,n_ue
               write(beschriftung,2578)trim(beschriftung),ue_gang(i,j,k)
            end do
            2578       format( A,6X,E16.10 )
            ! 2578       format(A,6X,F16.9)
            write(ionumber+i,'(A)')trim(beschriftung) ! final line output
         end do ! alle j Zeitschritte
      end if ! knoten auf diesem Prozess
   end do ! alle i Ausgabe-Knoten
   do i = 1,anz_gangl
      if (knot_gangl(i) > 0) then !Knoten auf diesem Prozess
         close (ionumber+i) ! Dateien glk... schließen
         !print*,'IntAmmonium(',knot_gangl(i),')=',(IntAmmonium(i)*real(deltat)), deltat
         !print*,' ganglinien_schliessen ',knot_gangl(i) , ionumber+i,' meinrang=',meinrang
      end if ! knoten auf diesem Prozess
   end do ! alle Ausgabe-Knoten
end subroutine ganglinien_schliessen
