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
subroutine ganglinien_parallel()
   use modell
   implicit none

   integer :: i,j, alloc_status, agp
   
   call MPI_Bcast(anz_gangl,1,MPI_INT,0,mpi_komm_welt,ierr)
   if (meinrang /= 0) allocate(knot_gangl(anz_gangl), stat = alloc_status)
   call MPI_Bcast(knot_gangl,anz_gangl,MPI_INT,0,mpi_komm_welt,ierr)
   
   !! Knoten aussortieren, die nicht zu diesem Prozess gehören
   agp = 0 !!
   do i = 1,anz_gangl
      if ((knot_gangl(i) > meinrang*part) .and. (knot_gangl(i) <= (meinrang+1)*part)) then
         agp = agp+1
         print ("(3(a,i0))"), 'ganglinien_parallel: Node #', knot_gangl(i), &
                             ' has process #', meinrang,                    &
                             ', part = ', part 
      else 
         knot_gangl(i) = 0
      endif
   enddo
   
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
   endif
   
   ! benthic distributions are not gathered. therefore process 0 can do no time-series output
   call MPI_Bcast(output_benth_distr,number_benth_distr,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(n_bn,1,MPI_INT,0,mpi_komm_welt,ierr)
   allocate (bn_gang(anz_gangl,zeitschrittanzahl+1,n_bn), stat = alloc_status )
   
   ! transfer_quantities
   call MPI_Bcast(output_trans_val,number_trans_val,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(output_trans_quant,number_trans_quant,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(output_trans_quant_vert,number_trans_quant_vert,MPI_LOGICAL,0,mpi_komm_welt,ierr)
   call MPI_Bcast(n_ue,1,MPI_INT,0,mpi_komm_welt,ierr)
   allocate (ue_gang(anz_gangl,zeitschrittanzahl+1,n_ue), stat = alloc_status )
end subroutine ganglinien_parallel

!> ganglinien_oeffnen
subroutine ganglinien_lesen()
   use modell
   implicit none
   
   character(200) :: file_name, nummer, systemaufruf
   integer        :: open_error, io_error, alloc_status, knumm, nn, i, sys_error, j
   logical        :: querschnitt_lesen
   
   if (meinrang > 0) call qerror("ganglinien_lesen() must only be called from processor 0.")
   
   anz_gangl = 0
   nn = 0
   
   file_name = trim(modellverzeichnis) // 'ganglinien_knoten.txt'
   open(newunit = ionumber , file = file_name, status = 'old', action = 'read ', iostat = open_error)
   if (open_error /= 0) call qerror("Could not open " // trim(file_name))
   
   
   ! --------------------------------------------------------------------------
   ! determine number of nodes
   ! --------------------------------------------------------------------------
   do while (zeile(ionumber)) 
      if (ctext(1:1) /= '#') then !! keine Kommentarzeile
         anz_gangl = anz_gangl + 1
         read(ctext,*,iostat = io_error)knumm
         if (io_error /= 0) call qerror("Error while reading " // trim(file_name))
      endif
   enddo
   
   ! --------------------------------------------------------------------------
   ! read nodes
   ! --------------------------------------------------------------------------
   rewind(ionumber) 
   allocate (knot_gangl(anz_gangl), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating variable `knot_gangl`.")
   
   do while (zeile(ionumber)) !! alle Zeilen
      if (ctext(1:1) /= '#') then !! keine Kommentarzeile
         nn = nn+1
         if (nn > anz_gangl) call qerror("Error while reading " // trim(file_name))
         read(ctext,*,iostat = io_error) knot_gangl(nn)
      endif
   enddo 
   close(ionumber)
   
   
   ! --------------------------------------------------------------------------
   ! check nodes
   ! --------------------------------------------------------------------------
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         call randlinie_zusammenstellen()
         do i = 1,anz_gangl
            if (knot_gangl(i) <= 0 .or. knot_gangl(i) > knotenanzahl2D) then
               write(fehler, "(4a,i0,a)") "Invalid node number in ", trim(file_name), &
                                          " Node", knot_gangl(i), " does not exist."
               call qerror(fehler)
            endif 
         enddo
      
      case(2) ! Untrim² netCDF
         do i = 1,anz_gangl
            if (knot_gangl(i) < 1 .or. knot_gangl(i) > n_elemente) then
               write(fehler, "(4a,i0,a)") "Invalid element number in ", trim(file_name), &
                                          " Element ", knot_gangl(i), " does not exist."
               call qerror(fehler)
            endif
         enddo
         
      case(3) ! SCHISM
         print*,'no cross sections possible with SCHISM'
      
      case default
         call qerror('ganglinien_lesen: Hydraulischer Antrieb unbekannt')
   end select
   
   ! --------------------------------------------------------------------------
   ! print summary to console
   ! --------------------------------------------------------------------------
   print*
   print "(a)", repeat("-", 80)
   print "(a)", "ganglinien_knoten.txt"
   print "(a)", repeat("-", 80)
   
   select case(hydro_trieb)
      case(1)
         print "(a,i0)", "number of nodes: ", anz_gangl
         do i = 1,anz_gangl
            print "(3x,i4, a,i0)", i, ":  node ", knot_gangl(i)
         enddo
      
      case(2)
         print "(a,i0)", "number of elements: ", anz_gangl
         print*
         do i = 1,anz_gangl
            print "(3x,i4, a,i0)", i, ":  element ", knot_gangl(i)
         enddo
         
         print*
         print "(a)", "Note: On UnTrim meshes metabolism variables are located at element"
         print "(a)", "      centers. Therefore numbers in `ganglinien_knoten.txt` are"
         print "(a)", "      expected to be element numbers."
   end select
   
end subroutine ganglinien_lesen


!> ganglinien_zeitschritt
subroutine ganglinien_zeitschritt(izeit_gang)
   use modell
   implicit none
   
   integer :: i, k, izeit_gang, n, nk
   real    :: age_tracer
   
   
   do i = 1,anz_gangl
      nk = knot_gangl(i)-meinrang*part
      if ((nk > 0) .and. (nk <= part)) then
         ! Zeitpunkt
         r_gang(i,izeit_gang) = rechenzeit
   
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
               
               if (nur_alter .and. k == 74) then
                  ! calculate mean age
                  ! concentration of passive tracer used for age calculation after Shen & Wang (2007)
                  age_tracer = planktonic_variable_p(71+(nk-1)*number_plankt_vari)
                  if (age_tracer > minimum_age_tracer) then
                     pl_gang(i,izeit_gang,n) = planktonic_variable_p(k+(nk-1)*number_plankt_vari) / age_tracer
                  else
                     pl_gang(i,izeit_gang,n) = 0.0
                  endif
               else
                  pl_gang(i,izeit_gang,n) = planktonic_variable_p(k+(nk-1)*number_plankt_vari)
               endif
            endif ! planktic output conc.
         enddo ! alle k planktonic_variable
         
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
         enddo ! alle k planktonic_variable
         ! benthic distributions are not gathered. therefore process 0 can do no time-series output
         
         n = 0
         do k = 1,number_benth_distr
            if (output_benth_distr(k)) then ! benth. distr. output
               n = n+1
               if (n > n_bn) call qerror('ganglinien_zeitschritt n > n_bn')
               bn_gang(i,izeit_gang,n) = benthic_distribution_p(k+(nk-1)*number_benth_distr)
            endif
         enddo
         !transfer_quantities are not gathered. therefore process 0 can do no time-series output
         n = 0
         do k = 1,number_trans_val
            if (output_trans_val(k)) then ! globale Übergabe Werte
               n = n+1
               if (n > n_ue) call qerror('1 ganglinien_zeitschritt (n > n_ue) ')
               ue_gang(i,izeit_gang,n) = transfer_value_p(k)
            endif ! exchange output conc.
         enddo
         do k = 1,number_trans_quant
            if (output_trans_quant(k)) then ! exchange con. output
               n = n+1
               if (n > n_ue) call qerror('2 ganglinien_zeitschritt (n > n_ue) ')
               ue_gang(i,izeit_gang,n) = transfer_quantity_p(k+(nk-1)*number_trans_quant)
            endif ! exchange output conc.
         enddo
         do k = 1,number_trans_quant_vert
            if (output_trans_quant_vert(k)) then ! exchange con. output
               n = n+1
               if (n > n_ue) call qerror('3 ganglinien_zeitschritt (n > n_ue) ')
               ue_gang(i,izeit_gang,n) = &
                  trans_quant_vert_p(gangl_level+(k-1)*num_lev_trans+(nk-1)*num_lev_trans*number_trans_quant_vert)
            endif ! exchange output conc.
         enddo
         ! Integrations
         ! IntAmmonium(i)=IntAmmonium(i)+ (planktonic_variable(3,knot_gangl(i))+c3Ammonium(i))/2.0
         ! c3Ammonium(i)=planktonic_variable(3,knot_gangl(i))
      endif ! knoten auf diesem Prozess
   enddo ! alle i Ausgabe-Knoten
   
   if (meinrang == 0) then
      if (hydro_trieb == 1) then ! hydro_trieb=1=casu
         if ( nur_alter ) call alter_zeitschritt(izeit+1)
         if (n_pl > 200)call qerror("n_pl > 200 geht nicht wegen fester Feldgröße massen_flux")
      endif
      endif ! meinrang.eq. 0
   call mpi_barrier (mpi_komm_welt, ierr)
end subroutine ganglinien_zeitschritt


subroutine ganglinien_schliessen()
   use modell
   use module_datetime
   implicit none
   
   
   character(longname) :: systemaufruf, file_name
   character(300)      :: nummer
   character(40000)    :: column_names, r_zeile
   character(3)        :: spalte
   integer             :: i,j,k,n, sys_error, open_error, nk, errcode, ngnu
   integer             :: year, month, day, hour, minute, second, u_cross
   real                :: h
   type(datetime)      :: datetime_output
   
   integer, parameter  :: time_style = 0 
   ! 0: Gerris
   ! 1: Ausgabeformat Stil wsa_cux
   ! 2: sekunden a la SCHISM

   if (meinrang == 0) then ! prozess 0 only
      
      ! --- remove files `ganglinien_bisher` ---
      write(systemaufruf,'(3A)',iostat = errcode) 'rm -rf ',trim(modellverzeichnis),'ganglinien_bisher'
      if (errcode /= 0)call qerror('ganglinien_schliessen writing systemcall rm -rf ganglinien_bisher failed')
      call system(systemaufruf,sys_error)
      if (sys_error /= 0) then
         write(fehler,*)'rm -rf ganglinien_bisher  failed sys_error = ', sys_error
         call qerror(fehler)
      endif
      
      ! --- move existing ganglinen files into `ganglinien_bisher` ---
      write(systemaufruf,'(5A)',iostat = errcode)'mv -f ',trim(modellverzeichnis),'ganglinien '  &
                                         ,trim(modellverzeichnis),'ganglinien_bisher >/dev/null 2>/dev/null'
      if (errcode /= 0)call qerror('ganglinien_schliessen writing systemcall mv ganglinien failed')
      call system(systemaufruf,sys_error)
      if (sys_error /= 0) then
         print*,'mv -f ganglinien ganglinien_bisher  meldet: sys_error = ', sys_error
      endif 
      
      ! --- create directory `ganglinien` ---
      write(systemaufruf,'(3A)',iostat = errcode)'mkdir ',trim(modellverzeichnis),'ganglinien'
      if (errcode /= 0)call qerror('ganglinien_schliessen writing systemcall mkdir ganglinien failed')
      call system(systemaufruf,sys_error)
      if (sys_error /= 0) then
         write(fehler,*)'mkdir ganglinien  fehlgeschlagen sys_error = ', sys_error
         call qerror(fehler)
      endif
      
      
      ! -----------------------------------------------------------------------
      ! create file header
      ! -----------------------------------------------------------------------
      column_names = repeat(" ", len(column_names))
      
      select case (time_style)
         case(0) !  Gerris
            write(column_names,'(A)') "#   Datum| Uhrzeit|    Zeitpunkt|       WSP004|   Geschw.005|"
            ngnu = 5
            
         case(1) ! Ausgabeformat Stil wsa_cux
            write(column_names,'(A)')"#        Zeitpunkt|     Tiefe003|   Geschw.004|"
            ngnu = 4
            
         case(2) !  sekunden a la SCHISM
            write(column_names,'(A)')"#        Zeitpunkt|"
            ngnu = 2
         
         case default
            call qerror("time_style falsch in Kopfzeile ganglinien_schliessen()")
      end select
      
      ! --- planktonic variables ---
      do i = 1,number_plankt_vari
         if (output_plankt(i)) then
            ngnu = ngnu + 1
            write(spalte,'(I3.3)')ngnu
            column_names = trim(column_names)//adjustr(planktonic_variable_name(i))//spalte//"|"
         endif
      enddo
      
      do i = 1,number_plankt_vari_vert
         if (output_plankt_vert(i)) then
            ngnu = ngnu + 1
            write(spalte,'(I3.3)')ngnu
            column_names = trim(column_names)//adjustr(plankt_vari_vert_name(i))//spalte//"|"
         endif
      enddo
      
      ! --- benthic distributions ---
      ! they are not gathered. therefore process 0 can do no time-series output
      do i = 1,number_benth_distr
         if (output_benth_distr(i)) then ! benth. distr. output
            ngnu = ngnu + 1
            write(spalte,'(I3.3)')ngnu
            column_names = trim(column_names)//adjustr(benth_distr_name(i))//spalte//"|"
         endif
      enddo
      
      ! --- transfer_quantities ---
      do i = 1,number_trans_val
         if (output_trans_val(i)) then ! globale Übergabe Werte
            ngnu = ngnu + 1
            write(spalte,'(I3.3)')ngnu
            column_names = trim(column_names)//adjustr(trans_val_name(i))//spalte//"|"
         endif
      enddo
      
      ! --- exchange concentrations ---
      do i = 1,number_trans_quant
         if (output_trans_quant(i)) then 
            ngnu = ngnu + 1
            write(spalte,'(I3.3)')ngnu
            column_names = trim(column_names)//adjustr(trans_quant_name(i))//spalte//"|"
         endif
      enddo
      
      
      do i = 1,number_trans_quant_vert
         if (output_trans_quant_vert(i)) then
            ngnu = ngnu + 1
            write(spalte,'(I3.3)')ngnu
            column_names = trim(column_names)//adjustr(trans_quant_vert_name(i))//spalte//"|"
         endif
      enddo

      
      ! if(nur_alter) call alter_ausgabe() !! Alters-Ausgabe
      ! Rand-Flüsse ausgeben
      !! do n = 1,ianz_rb !! alle Ränder
      !!    write(nummer,*)rabe(n)%nr_rb
      !!    write(file_name,'(4A)',iostat = errcode)trim(modellverzeichnis),'ganglinien/r',trim(adjustl(nummer)),'.txt'
      !!    if (errcode /= 0)call qerror('ganglinien_schliessen writing filename ganglinien/r*.txt failed')
      !!    open ( unit = 12345+n , file = file_name, status = 'new', action = 'write ', iostat = open_error )
      !!    write(12345+n,*)"## Randflüsse (lang,flaeche,vol_strom, pot_ener_flux(MW), kin_ener_flux, massen_flux71) für "  &
      !!                    ,n,"-ten Rand, Nr. = ",rabe(n)%nr_rb,"  ##" ! Kopfzeile schreiben
      !!    do j = 1,zeitschrittanzahl+1
      !!       zeitpunkt = r_gang(1,j)
      !!       call zeitsekunde()
      !!       write(r_zeile,'(I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
      !!             jahr  ,monat ,  tag  ,   stunde , minute , sekunde
      !!       do i = 1, 6
      !!          write(r_zeile,'(A,6x,F16.9)')trim(r_zeile), randflux_gang(n,j,i)   ! r_gang(i,j)
      !!       enddo ! 5 (eigentlich alle Randfüsse incl. Massenflüsse)
      !!       write(12345+n,'(A)')trim(adjustl(r_zeile))
      !!    enddo ! alle j Zeitschritte
      !!    rewind(12345+n)
      !!    close(12345+n) !
      !!    print*,'Ausgabe Rand-Fluss ', trim(file_name), ' meinrang = ',meinrang," n_pl = ",n_pl
      !! enddo ! alle Ränder
      
      ! -----------------------------------------------------------------------
      ! write crosssection
      ! -----------------------------------------------------------------------
      if (querschneiden) then
         do n = 1,anzahl_quer !! all n cross sections
            write(nummer,"(i0)") n
            file_name = trim(modellverzeichnis) // 'ganglinien/q' // trim(adjustl(nummer)) // '.txt'
            
            open(newunit = u_cross , file = file_name, status = 'new', action = 'write ', iostat = open_error)
            write(r_zeile,*)"#   Datum| Uhrzeit| Zeitpunkt(sec.) | Durchfluss | Wasservolumen im Zeitschritt = ",deltat
            
            do i = 1,number_plankt_vari
               if (output_plankt(i)) then ! planktic output conc.
                  r_zeile = trim(r_zeile)//planktonic_variable_name(i)//" | "
               endif
            enddo
            
            ! write header
            write(u_cross,*) trim(adjustl(r_zeile))
            do j = 1,zeitschrittanzahl
               datetime_output = gmtime(q_gangl(j))
               write(r_zeile,'(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2,x,I13)')  &
                  datetime_output % get_year(),            &
                  datetime_output % get_month(),           &
                  datetime_output % get_day(),             &
                  datetime_output % get_hour(),            &
                  datetime_output % get_minute(),          &
                  datetime_output % get_second(),          &
                  datetime_output % seconds_since_epoch()
                  
                  
               do i = 1,n_pl+2
                  write(r_zeile,'(A,6X,E16.10)') trim(r_zeile), schnittflux_gang(n,j,i)
               enddo 
               write(u_cross,'(a)') trim(r_zeile)
            enddo
            
            close(u_cross)
            print*,'Ausgabe Querschnitts-Fluss ', trim(file_name), ' meinrang = ',meinrang," n_pl = ",n_pl
         enddo 
      endif 
   
   
   endif ! only prozessor 0
   
   call mpi_barrier(mpi_komm_welt, ierr)
   call MPI_Bcast(column_names, len(column_names), MPI_CHARACTER, 0, mpi_komm_welt, ierr)
   
   do i = 1,anz_gangl
      ! --- write fileheader ---
      if (knot_gangl(i) > 0) then !Knoten auf diesem Prozess
         write(nummer,*) knot_gangl(i)
         file_name =  trim(modellverzeichnis) // 'ganglinien/g' // trim(adjustl(nummer)) // '.txt'
         
         open(newunit = ionumber , file = file_name, status = 'new', action = 'write ', iostat = open_error)
         if (open_error /= 0) call qerror("Could not open " // file_name)
         
         ! write column names
         write(ionumber,'(A)') trim(column_names) 
         
         ! Geländehöhe rückrechnen
         nk = knot_gangl(i) - meinrang * part
         h = rb_hydraul_p(3+(nk-1)*number_rb_hydraul) - rb_hydraul_p(2+(nk-1)*number_rb_hydraul) 
         write(ionumber,*) "# Gelaendehoehe = ", h
            
         
         ! --- write data ---
         do j = 1,zeitschrittanzahl+1
            datetime_output = gmtime(r_gang(i,j))
            year   = datetime_output % get_year()
            month  = datetime_output % get_month()
            day    = datetime_output % get_day()
            hour   = datetime_output % get_hour()
            minute = datetime_output % get_minute()
            second = datetime_output % get_second()
            
            select case (time_style)
               case(0) ! gerris
                  write(column_names,'(i4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2,x,i13)') &
                     year, month, day, hour, minute, second, datetime_output % seconds_since_epoch()
               
               case(1) ! wsa_cux
                  write(column_names,'(i2.2,".",i2.2,".",i4.4," ",i2.2,":",i2.2,":",i2.2)') &
                     day, month, year, hour, minute, second
               
               case(2) ! schism
                  write(column_names,*) datetime_output % seconds_since_epoch()
               
               case default
                  call qerror("time_style falsch in ganglinien_schliessen()")
            end select
            
            
            write(column_names,'(a, 2(7x,f7.2))') trim(column_names), t_gang(i,j), u_gang(i,j)
            
            do k = 1,n_pl
               write(column_names,2578) trim(column_names), pl_gang(i,j,k)
            enddo
            
            do k = 1,n_bn
               write(column_names,2578) trim(column_names), bn_gang(i,j,k)
            enddo
            
            do k = 1,n_ue
               write(column_names,2578) trim(column_names), ue_gang(i,j,k)
            enddo
            2578 format(a,6x,e16.10)
            
            write(ionumber,'(A)') trim(column_names) ! final line output
         enddo ! alle j Zeitschritte
   
         close(ionumber)
      endif
   enddo 
end subroutine ganglinien_schliessen
