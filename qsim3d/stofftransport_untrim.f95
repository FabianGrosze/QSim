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

subroutine holen_trans_untrim(nt)
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'

   integer, intent(in)      :: nt
   
   integer, dimension(3)    :: start3, count3
   integer, dimension(2)    :: start2, count2
   integer                  :: n,j,k, varid, alloc_status
   
   character(50), parameter :: nc_error_prefix = 'holen_trans_untrim'
   
   ! --------------------------------------------------------------------------
   ! elements
   ! --------------------------------------------------------------------------
   start3 = (/ 1, 1, nt /)
   count3 = (/ n_elemente, 1, 1 /)
   !float Mesh2_face_Wasservolumen_2d(nMesh2_data_time, nMesh2_layer_2d, nMesh2_face) ;
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_Wasservolumen_2d', varid) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, el_vol, start3, count3 ) )

   do n = 1,n_elemente
      if ((el_vol(n) <= 0.0) .or. (el_vol(n) > 1.e+30)) el_vol(n) = 0.0
   enddo
   
   start2 = (/ 1, nt /)
   count2 = (/ n_elemente, 1 /)

   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_Wasserstand_2d', varid) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, p, start2, count2 ) )
   do n = 1,n_elemente !
      if ( abs(p(n)) > 1.e+30) p(n) = -999.9
   enddo
   
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_Wasserflaeche_2d', varid) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, el_area, start2, count2 ) )
   do n = 1,n_elemente 
      if ((el_area(n) <= 0.0) .or. (el_area(n) > 1.e+30)) el_area(n) = 0.0
   enddo 
     
   ! --------------------------------------------------------------------------
   ! edges
   ! --------------------------------------------------------------------------
   ed_vel_x(:) = 0.0
   ed_vel_y(:) = 0.0
   start3 = (/ 1, 1, nt /)
   count3 = (/ kantenanzahl, 1, 1 /)
   !float Mesh2_edge_Durchflussflaeche_2d(nMesh2_data_time, nMesh2_layer_2d, nMesh2_edge) ;

   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_edge_Durchflussflaeche_2d', varid) )
   !call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_edge_mit_hor_durchstroemte_Kantenflaeche_2d', varid) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, ed_area, start3, count3 ) )
   !float Mesh2_edge_Stroemungsgeschwindigkeit_x_R_2d(nMesh2_data_time, nMesh2_layer_2d, nMesh2_edge) ;
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_edge_Stroemungsgeschwindigkeit_x_R_2d', varid) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, ed_vel_x, start3, count3 ) )
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_edge_Stroemungsgeschwindigkeit_y_R_2d', varid) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, ed_vel_y, start3, count3 ) )
   !call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_edge_hor_Wassertransport_Kantenflaeche_2d', varid) )
   !call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, ed_flux, start3, count3 ) )

   do n = 1,kantenanzahl
      !Mesh2_edge_Durchflussflaeche_2d:_FillValue = 1.e+31f ;
      if ((ed_area(n) <= 0.0) .or. (ed_area(n) > 1.e+30)) then ! Kante trocken ?
         ed_area(n) = 0.0
         ed_vel_x(n) = 0.0
         ed_vel_y(n) = 0.0
      endif ! Kante trocken
      !print*,n,ed_vel_x(n),ed_vel_y(n)
      ! Mesh2_edge_Stroemungsgeschwindigkeit_x_R_2d:_FillValue = 1.e+31f ;
      if (ed_vel_x(n) > 100.0)ed_vel_x(n) = 0.0
      if (ed_vel_y(n) > 100.0)ed_vel_y(n) = 0.0
   enddo ! alle n kanten
   
   do n = 1,n_elemente ! mean velocity magnitude in element
      u(n) = 0.0
      do k = 1,cornernumber(n)
         u(n) = u(n)+ (ed_vel_x(elementedges(n,k))**2 + ed_vel_y(elementedges(n,k))**2)**0.5
         if (u(n) > huge(u(n))) then
            print*,"ed_vel_x,y,elementedges,n,k = ",  &
                  ed_vel_x(elementedges(n,k)),ed_vel_y(elementedges(n,k)),elementedges(n,k),n,k
            call qerror("holen_trans_untrim u infinity")
         endif
      enddo ! alle k Kanten im Element
      
      if (cornernumber(n) <= 0) call qerror("cornernumber(n) is less or equal 0.")
      u(n) = u(n) / real(cornernumber(n))
      inflow(n) = .false.
      if (element_rand(n) > 0) inflow(n) = .true.
   enddo ! alle n Elemente
   
   do j = 1,number_plankt_point 
      rb_hydraul(1+(j-1)*number_rb_hydraul) = u(j)
      
      ! mean depth in element
      if ((el_area(j) > 0.0) .and. (el_vol(j) > 0.0)) then
         rb_hydraul(2+(j-1)*number_rb_hydraul) = el_vol(j) / el_area(j) 
      else
         rb_hydraul(2+(j-1)*number_rb_hydraul) = 0.0 !  Tiefe zunächst null
      endif
      
      rb_hydraul(3+(j-1)*number_rb_hydraul) = p(j)
      benthic_distribution(44+(j-1)*number_benth_distr) = 0.1        ! ks - jetzt anders zone()%reib
      benthic_distribution(45+(j-1)*number_benth_distr) = 0.1 * u(j) ! utau
   enddo
   
end subroutine holen_trans_untrim


! TODO: add description
subroutine stofftransport_untrim()
   use netcdf
   use modell
   use module_datetime
   implicit none
   include 'netcdf.inc'
   
   integer                           :: nti, nt, n, j, k, alloc_status, iq, jq, no, nedel
   integer(int64)                    :: subtime, diffprev, diff
   logical                           :: found
   real                              :: laeng, dt_sub, sumwicht
   real                              :: cu_mean_cugt1, volfrac_cugt1, fluxi, flow
   real, allocatable, dimension(:,:) :: zwischen
   type(datetime)                    :: datetime_sub
   
   integer, parameter :: num_sub = 6
   
   if (meinrang /= 0) call qerror('subroutine `stofftransport_untrim` must only be called from processor 0.')
   
   allocate(zwischen(number_plankt_vari, number_plankt_point), stat = alloc_status)
   if (alloc_status /= 0) call qerror('Error while allocating variable `zwischen`.')
   
   dt_sub = real(deltat) / real(num_sub)
   print "(i0,a,f0.2,a)", num_sub," subtimesteps of length ", dt_sub, " seconds"
   
   do nt = 1,num_sub ! alle Transport (zwischen) Zeitschritte
      
      ! control output
      if (kontrollknoten > 0 .and. kontrollknoten <= number_plankt_point) then 
         print "(2(a, f0.3))", ' vor transportschritt untrim T_wass(kontrollelement) = ',      &
                                planktonic_variable(1+(kontrollknoten-1)*number_plankt_vari),  &
                               ' Tracer = ', planktonic_variable(71+(kontrollknoten-1)*number_plankt_vari)
      endif
      
      subtime = startzeitpunkt + nint(real((2*nt-1)*deltat) / real(2*num_sub) )
      datetime_sub = as_datetime(subtime, tz_qsim)
      
      if (subtime < transinfo_zeit(transinfo_zuord(1)) .or. &
          subtime > transinfo_zeit(transinfo_zuord(transinfo_anzahl))) then
         call qerror("stofftransport_untrim: subtimestep outside range of current timestep")
      endif
      
      nti = 0
      diffprev = transinfo_zeit(transinfo_zuord(transinfo_anzahl)) - transinfo_zeit(transinfo_zuord(1))
      do n = 1,transinfo_anzahl
         diff = abs(subtime - transinfo_zeit(transinfo_zuord(n)))
         if (diff <= diffprev) nti = n
         diffprev = diff
      enddo 
      if (nti <= 0) call qerror('stofftransport_untrim: No untrim timestamp identified.')
      diff = transinfo_zeit(transinfo_zuord(nti)) - subtime      
      
      print "(a,i0,2a,2(a,i0))", "subtimestep ", nt, ": ", datetime_sub % date_string(),  ", nti = ", nti, ", diff = ", diff
      
      call holen_trans_untrim(nti)
      
      ! initialize volume exchange
      wicht(:) = 0.
      
      do n = 1,kantenanzahl ! edge fluxes
         laeng = ( (edge_normal_x(n)**2.0)+(edge_normal_y(n)**2.0) )**0.5
         ed_flux(n) = 0.0
         ! volume-flux to the left
         if (laeng > 0.0) then
            ed_flux(n) = (ed_vel_x(n)*edge_normal_x(n)+ed_vel_y(n)*edge_normal_y(n))/laeng
            ed_flux(n) = ed_flux(n)*ed_area(n)
         endif
         
         ! --- summing outflow into my own wicht
         ! left_element !flux to the right
         if ( (left_element(n) > 0) .and. (ed_flux(n) < 0.0) )  &
             wicht((left_element(n)-1)*5+1) = wicht((left_element(n)-1)*5+1)-ed_flux(n)
         if ( (right_element(n) > 0) .and. (ed_flux(n) > 0.0) ) &
             wicht((right_element(n)-1)*5+1) = wicht((right_element(n)-1)*5+1)+ed_flux(n)
         if (querschneiden) then
            do iq = 1,anzahl_quer ! all iq cross sections
               do jq = 1,querschnitt(iq)%schnittlinie%anzkanten !! all jq edges of this cross section
                  found = .false.
                  if (n == querschnitt(iq)%schnittlinie%kante(jq)%num) then
                     fluxi = ed_flux(n)*dt_sub
                     nedel = left_element(n)
                     if (fluxi <= 0.0)nedel = right_element(n)
                     found = .true.
                  endif
                  
                  if ((-1*n) == querschnitt(iq)%schnittlinie%kante(jq)%num) then
                     fluxi = (-1)*ed_flux(n)*dt_sub
                     nedel = right_element(n)
                     if (fluxi <= 0.0)nedel = left_element(n)
                     found = .true.
                  endif
                  
                  if (found) then
                     flow = 0.0
                     if (ed_area(n) > 0.0)flow = ed_flux(n)/ed_area(n)
                     
                     schnittflux_gang(iq,izeit, 1 ) = schnittflux_gang(iq,izeit, 1 )+ fluxi/dt_sub
                     schnittflux_gang(iq,izeit, 2 ) = schnittflux_gang(iq,izeit, 2 )+ fluxi
                     
                     if ((nedel < 1) .or. (nedel > n_elemente)) then
                        write(fehler,*)'nedel not in range n,nedel,iq,jq,izeit = ',n,nedel,iq,jq,izeit
                        call qerror(fehler)
                     endif
                     
                     no = 0
                     do k = 1,number_plankt_vari
                        if (output_plankt(k)) then ! planktic output conc.
                           no = no+1
                           schnittflux_gang(iq,izeit, no+2) = schnittflux_gang(iq,izeit, no+2 )+  &
                                                               fluxi * planktonic_variable(k+(nedel-1)*number_plankt_vari)
                        endif ! planktic output conc.
                     enddo ! all k planktic variables
                  endif ! found
               enddo ! all jq
            enddo ! all iq cross sections
         endif ! querschneiden
         
      enddo ! alle n edges
      do n = 1,n_elemente ! gathering inflows
         do k = 1,cornernumber(n)
            ! if I am the left_element of my n-th edge and the flux is to the left
            if ( (left_element(elementedges(n,k)) == n) .and. (ed_flux(elementedges(n,k)) > 0.0) )  &
                wicht((n-1)*5+(1+k)) = ed_flux(elementedges(n,k))
            ! if I am the right_element of my n-th edge and the flux is to the right
            if ( (right_element(elementedges(n,k)) == n) .and. (ed_flux(elementedges(n,k)) < 0.0) )  &
                wicht((n-1)*5+(1+k)) = -1.0 * ed_flux(elementedges(n,k))
         enddo
      enddo
     
      ! Gewichtsfaktoren aus Volumenverhältnissen ermitteln
      cu = 0.
      do n = 1,n_elemente 
         if (el_vol(n) > 0.0) then ! Element wet
            cu(n) = (wicht((n-1)*5+1)*dt_sub)/el_vol(n)
            wicht((n-1)*5+1) = 1.0 - cu(n)
            do j = 2,5
               wicht((n-1)*5+j) = (wicht((n-1)*5+j)*dt_sub)/el_vol(n)
            enddo
         else 
            ! element dry
            wicht((n-1)*5+1) = 1.0
            do j = 2,5
               wicht((n-1)*5+j) = 0.0
            enddo
         endif
         
         ! clipping 
         sumwicht = 0.0
         do j = 1,5 
            if (wicht((n-1)*5+j) < 0.0) wicht((n-1)*5+j) = 0.0
            if (wicht((n-1)*5+j) > 1.0) wicht((n-1)*5+j) = 1.0
            sumwicht = sumwicht+wicht((n-1)*5+j)
         enddo
         
         if (sumwicht > 0.0) then
            do j = 1,5 !
               wicht((n-1)*5+j) = wicht((n-1)*5+j)/sumwicht
            enddo
         else
            wicht((n-1)*5+1) = 1.0 ! Wert belibt wie er ist
            do j = 2,5
               wicht((n-1)*5+j) = 0.0
            enddo
         endif
      enddo
      
      ! calculate water volume fraction with Courant number > 1 and 
      ! average cu in cells with cu > 1
      cu_mean_cugt1 = sum(el_vol * cu, cu > 1.) / max(1., sum(el_vol, cu > 1.))
      volfrac_cugt1 = sum(el_vol     , cu > 1.) / max(1., sum(el_vol))
      
      print "(3x,a,e10.3)", "cu_max                   = ", maxval(cu)
      print "(3x,a,e10.3)", "cu_min                   = ", minval(cu)
      print "(3x,a,e10.3)", "cu_mean(cu > 1)          = ", cu_mean_cugt1
      print "(3x,a,e10.3)", "volumen fraction(cu > 1) = ", volfrac_cugt1
      
      
      do j = 1,number_plankt_point ! all j elements (*levels?)
         do n = 1,number_plankt_vari ! all transported concentrations i.e. variables
            if (iEros < 0 .and. (n == 52 .or. n == 53)) cycle    ! module_suspendedMatter: skip SSalg and SS if SS read from file
            if (n == 72) cycle                                   ! module_salinity: skip transport of salinity read from file
            zwischen(n,j) = planktonic_variable(n+(j-1)*number_plankt_vari) *wicht((j-1)*5+1) !! self
            do k = 1,4 ! all 4 neighbour (elements) if existing
               if (intereck((j-1)*4+k) > 0) zwischen(n,j) = zwischen(n,j) +  &
                   planktonic_variable(n+(intereck((j-1)*4+k)-1)*number_plankt_vari) *wicht((j-1)*5+1+k)
            enddo
         enddo ! alle n Konzentrationen
      enddo ! alle j Elemente
      
      do j = 1,number_plankt_point ! alle j Elemente
         
         ! Zuflusselemente auslassen
         if ( .not. inflow(j)) then 
            do n = 1,number_plankt_vari
               if (iEros < 0 .and. (n == 52 .or. n == 53)) cycle    ! module_suspendedMatter: skip SSalg and SS if SS read from file
               if (n == 72) cycle                                   ! module_salinity: skip transport of salinity read from file
              
              if (isNaN(zwischen(n,j))) then
                  print*,'stofftransport_untrim isNaN(zwischen) , plankt_point = ',j,' plankt_vari = ',n
                  print*,'planktonic_variable_name',n, planktonic_variable_name(n)
                  print*,'self !! wicht,plankt =',wicht((j-1)*5+1),planktonic_variable(n+(j-1)*number_plankt_vari)
                  print*,'intereck((j-1)*4+k) = ',( intereck((j-1)*4+k),k = 1,4 )
                  print*,'wicht !! neighbours = ',(wicht((j-1)*5+1+k),k=1,4)
                  do k = 1,4 ! all 4 neighbour (elements) if existing
                     if (intereck((j-1)*4+k) > 0) then
                        print*,'planktonic_variable(',k,') = '                                      &
                              , planktonic_variable(n+(intereck((j-1)*4+k)-1)*number_plankt_vari)   &
                              ,' tief = ',rb_hydraul(2+(intereck((j-1)*4+k)-1)*number_rb_hydraul)
                     endif
                  enddo !all 4 k
                  write(fehler,*)'stofftransport_untrim: isNaN(zwischen planktonic_variable_name', planktonic_variable_name(n)
                  call qerror(fehler)
               endif
               
               planktonic_variable(n+(j-1)*number_plankt_vari) = zwischen(n,j)
               
            enddo ! alle n Konzentrationen
         endif !kein Zuflusselement
      enddo ! alle j Elemente
      
      if ((kontrollknoten > 0) .and. (kontrollknoten <= number_plankt_point)) then ! Ausgabe
         print*,'Nach transportschritt untrim am kontrollelement:'
         print*,'Wassertiefe = ',rb_hydraul(2+(kontrollknoten-1)*number_rb_hydraul)          &
               ,'Temp_wass = ',planktonic_variable(1+(kontrollknoten-1)*number_plankt_vari)  &
               ,'chla = '   ,planktonic_variable(11+(kontrollknoten-1)*number_plankt_vari)
         print*,'wicht !! self =',wicht((kontrollknoten-1)*5+1)
         print*,'intereck((j-1)*4+k) = ',( intereck((kontrollknoten-1)*4+k),k = 1,4 )
         print*,'wicht !! neighbours = ',(wicht((kontrollknoten-1)*5+1+k),k=1,4)
      endif
   
   enddo ! alle nt Subzeitschritte
   
   deallocate(zwischen, stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while deallocating variable `zwischen`.")
   
   ! all iq cross sections
   do iq = 1,anzahl_quer 
      q_gangl(izeit) = rechenzeit
      schnittflux_gang(iq,izeit, 1 ) = schnittflux_gang(iq,izeit, 1 )/real(num_sub)
      print*,"schnittflux: ",rechenzeit,izeit,iq," flux = ",schnittflux_gang(iq,izeit, 1 )
   enddo
   
end subroutine stofftransport_untrim



subroutine read_mesh_nc()
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'

   integer                                   :: iret, ndims, nVars, nGlobalAtts, unlimdimid, nAtts, ndumm, errcode
   integer                                   :: j,k,n, didi, alloc_status, open_error, nele_links, nele_rechts, anzahl_randkanten
   integer, allocatable, dimension(:)        :: dlength, vxtype, vndims, nbc
   integer, allocatable, dimension(:,:)      :: fa_no, ed_fa
   integer, dimension(nf90_max_var_dims)     :: dimids
   character(longname)                       :: filename
   character(256)                            :: aname
   character(256), allocatable, dimension(:) :: dname, vname
   real, allocatable, dimension(:)           :: zeiten
   real                                      :: nach_links
   logical                                   :: nixlinks, nixrechts
   
   character(50), parameter :: nc_error_prefix = 'read_mesh_nc'
   
   
   print*
   print "(a)", repeat("-", 80)
   print "(a)", "read_mesh_nc"
   print "(a)", repeat("-", 80)
   
   
   open ( unit = 123 , file = 'netcdf.log', status = 'replace', action = 'write', iostat = open_error )
   filename =  trim(modellverzeichnis) // 'transport.nc'
   call check_err(trim(nc_error_prefix), nf_open(filename, NF_NOWRITE, ncid))
   
   ! Überblick
   call check_err(trim(nc_error_prefix), nf90_inquire(ncid, ndims, nVars, nGlobalAtts, unlimdimid))
   write(123,*)"ndims, nVars, nGlobalAtts = ",ndims, nVars, nGlobalAtts
   
   ! --- dimensions --
   allocate (dlength(ndims), stat = alloc_status )
   allocate (dname(ndims), stat = alloc_status )
   do j = 1,ndims
      call check_err(trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j)))
      write(123,*)'dimension  ' ,trim(adjustl(dname(j))),' wert = ', dlength(j)
   enddo !j
   write(123,*)'--'
   
   ! --- variables ---
   allocate (vxtype(nVars), stat = alloc_status )
   allocate (vndims(nVars), stat = alloc_status )
   allocate (vname(nVars), stat = alloc_status )
   !write(123,*)'NF_INT=', NF_INT," NF_FLOAT=", NF_FLOAT," NF_DOUBLE=", NF_DOUBLE
   do j = 1,nVars
      call check_err(trim(nc_error_prefix), nf90_inquire_variable(ncid,j,vname(j),vxtype(j),vndims(j),dimids, nAtts))
      if (vxtype(j) == NF_DOUBLE) write(123,*)'NF_DOUBLE '
      if (vxtype(j) == NF_FLOAT) write(123,*)'NF_FLOAT '
      if (vxtype(j) == NF_INT) write(123,*)'NF_INT '
      write(123,*)"Variable named :",trim(adjustl(vname(j)))
      write(123,*)"Dimensionen: "
      do k = 1,vndims(j)
         write(123,*)'   ', trim(adjustl(dname(dimids(k)))), dlength(dimids(k))
      enddo !k Dimensionen von Variable j
      !rint*,'inquire_variable ',j,' : ' ,trim(adjustl(vname)),' - ',xtype,ndims,dimids
      write(123,*)"Attribute : "
      call print_attributes(j, nAtts)
      write(123,*)'--'
   enddo ! Variable j
   
   ! --- global attributes ---
   write(123,*)'Globale Attribute: '
   call print_attributes(NF90_GLOBAL, nGlobalAtts)
   write(123,*)'--'
   close (123)

   ! -------------------------------------------------------------------------
   ! dimensions
   ! -------------------------------------------------------------------------
   call check_err( trim(nc_error_prefix), nf90_inq_dimid(ncid, "nMesh2_node", didi) )
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, didi, aname, knotenanzahl2D) )
   
   call check_err( trim(nc_error_prefix), nf90_inq_dimid(ncid, "nMesh2_edge", didi) )
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, didi, aname, kantenanzahl) )
   kanten_vorhanden = .true.
   
   call check_err( trim(nc_error_prefix), nf90_inq_dimid(ncid, "nMesh2_face", didi) )
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, didi, aname, n_elemente) )
   element_vorhanden = .true.
   
   call check_err( trim(nc_error_prefix), nf90_inq_dimid(ncid, "nMesh2_data_time", didi) )
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, didi, aname, transinfo_anzahl) )
   
   print "(a)",    "dimensions:"
   print "(a,i0)", "   nMesh2_node      = ", knotenanzahl2D
   print "(a,i0)", "   nMesh2_edge      = ", kantenanzahl
   print "(a,i0)", "   nMesh2_face      = ", n_elemente
   print "(a,i0)", "   nMesh2_data_time = ", transinfo_anzahl
   
   ! -------------------------------------------------------------------------
   ! nodes
   ! -------------------------------------------------------------------------
   allocate (knoten_x(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (knoten_x failed')
   call check_err( trim(nc_error_prefix), nf90_inq_varid(ncid,'Mesh2_node_x', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, knoten_x) )
 
   allocate (knoten_y(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (knoten_y failed')
   call check_err( trim(nc_error_prefix), nf90_inq_varid(ncid,'Mesh2_node_y', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, knoten_y) )
   
   allocate (knoten_z(knotenanzahl2D), source = 0.0, stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (knoten_z failed')
   allocate (knoten_zone(knotenanzahl2D), source = 0, stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (knoten_zone( failed')
   
   print*
   print "(a)", "bounding box of nodes:"
   print "(3x,a,f0.2,x,f0.2)", "x: ", minval(knoten_x), maxval(knoten_x)
   print "(3x,a,f0.2,x,f0.2)", "y: ", minval(knoten_y), maxval(knoten_y)
   
   ! -------------------------------------------------------------------------
   ! faces
   ! -------------------------------------------------------------------------
   allocate (element_x(n_elemente),element_y(n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate element_xy failed')
   call check_err( trim(nc_error_prefix), nf90_inq_varid(ncid,'Mesh2_face_x', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, element_x) )
   call check_err( trim(nc_error_prefix), nf90_inq_varid(ncid,'Mesh2_face_y', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, element_y) )
   
   allocate (fa_no(4,n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate fa_no( failed')
   allocate (elementnodes(n_elemente,4), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (elementnodes( failed')
   allocate (cornernumber(n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (cornernumber( failed')

   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_nodes', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, fa_no) )

   summ_ne = 0
   do n = 1,n_elemente ! alle Elemente
      if (fa_no(4,n) < 0) then
         cornernumber(n) = 3
      else
         cornernumber(n) = 4
      endif
      
      do k = 1,cornernumber(n)
         elementnodes(n,k) = fa_no(k,n)+1
         if ((elementnodes(n,k) < 1) .or. (elementnodes(n,k) > knotenanzahl2D)) then
            write(fehler,*)'read_mesh_nc elementnodes falsch:',elementnodes(n,k),n,k
            call qerror(fehler)
         endif
      enddo
      summ_ne = summ_ne+cornernumber(n)+1
   enddo 
   
   ! number of elements at node
   allocate (knot_ele(knotenanzahl2D), stat = alloc_status ) 
   if (alloc_status /= 0) call qerror('allocate (knot_ele(knotenanzahl2D) failed')
   do n = 1,knotenanzahl2D
      knot_ele(n) = 0
   enddo
   
   do j = 1,n_elemente ! alle Elemente
      do k = 1,cornernumber(j)
         knot_ele(elementnodes(j,k)) = knot_ele(elementnodes(j,k))+1
      enddo ! alle Element-ecken
   enddo
   if (any(knot_ele == 0)) call qerror("Invalid mesh: Nodes belonging to no element.")
   
   allocate (element_rand(n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (element_rand failed')
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_bc', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, element_rand) )
   allocate (element_zone(n_elemente), source = 0, stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (element_zone failed')
   
   allocate (elementedges(n_elemente,4), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (elementedges failed')
   elementedges = 0
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_edges', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, fa_no) )

   do n = 1,n_elemente ! alle Elemente
      do k = 1,cornernumber(n)
         elementedges(n,k) = fa_no(k,n)+1
         if ((elementedges(n,k) < 1) .or. (elementedges(n,k) > kantenanzahl)) then
            write(fehler,*)'netcdf_mesh_only elementedges(n,k) falsch:',elementedges(n,k),n,k
            call qerror(fehler)
         endif
      enddo ! alle Elementecken
   enddo ! alle Elemente
   deallocate (fa_no, stat = alloc_status )
   
   ! -------------------------------------------------------------------------
   ! edges
   ! -------------------------------------------------------------------------
   kanten_vorhanden = .true.

   allocate (top_node(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror(' allocate (top_node failed')
   allocate (bottom_node(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (bottom_node failed')
   allocate (ed_fa(2,kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (ed_fa(2 failed')
   call check_err( trim(nc_error_prefix), nf90_inq_varid(ncid,'Mesh2_edge_nodes', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, ed_fa) )

   do n = 1,kantenanzahl ! alle Kanten
      bottom_node(n) = ed_fa(1,n)+1
      if ((bottom_node(n) < 1) .or. (bottom_node(n) > knotenanzahl2D))call qerror("read_mesh_nc:bottom_node falsch")
      top_node(n) = ed_fa(2,n)+1
      if ((top_node(n) < 1) .or. (top_node(n) > knotenanzahl2D))call qerror("read_mesh_nc:top_node falsch")
   enddo ! alle n Kanten

   allocate (edge_normal_x(kantenanzahl),edge_normal_y(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (edge_normal failed')
   do n = 1,kantenanzahl ! alle Kanten
      ! Normalenvektor von Kantenlänge nach links
      edge_normal_x(n) = knoten_y(bottom_node(n)) - knoten_y(top_node(n))
      edge_normal_y(n) = knoten_x(top_node(n))    - knoten_x(bottom_node(n))
   enddo ! alle n Kanten
   allocate (left_element(kantenanzahl),right_element(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (left,right_element  failed')
   call check_err( trim(nc_error_prefix), nf90_inq_varid(ncid,'Mesh2_edge_faces', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, ed_fa) )

   ! links-rechts-sortieren, Randkanten
   allocate (boundary_number(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (boundary_number(kantenanzahl) failed')
   anzahl_randkanten = 0
   do n = 1,kantenanzahl ! alle Kanten
      nele_links = ed_fa(1,n)+1
      nele_rechts = ed_fa(2,n)+1
      nixlinks = ( (nele_links < 1) .or. (nele_links > n_elemente) )
      nixrechts = ( (nele_rechts < 1) .or. (nele_rechts > n_elemente) )
      left_element(n) = -999
      right_element(n) = -999
      if (nixlinks .and. nixrechts) call qerror("read_mesh_nc:kanten-elemente konnektivität schrott")
      if (nixlinks .or. nixrechts) then  !Randkante
         anzahl_randkanten = anzahl_randkanten + 1
         boundary_number(n) = 1
      else !keine Randkante
         boundary_number(n) = 0
      endif   !Randekante
      if ( .not. nixlinks ) then !left_element(n) vorhanden ?
         nach_links = edge_normal_x(n) * (element_x(nele_links) - knoten_x(bottom_node(n))) +  &
                      edge_normal_y(n) * (element_y(nele_links) - knoten_y(bottom_node(n)))
         if (nach_links >= 0.0) then ! links auf der linken seite?
            left_element(n) = nele_links
            if ( .not. nixrechts) right_element(n) = nele_rechts
         else !links ist rechts
            right_element(n) = nele_links
            if ( .not. nixrechts) left_element(n) = nele_rechts
         endif ! links auf der linken seite?
      else ! left_element(n) nicht vorhanden
         nach_links = edge_normal_x(n) * (element_x(nele_rechts) - knoten_x(bottom_node(n))) +  &
                      edge_normal_y(n) * (element_y(nele_rechts) - knoten_y(bottom_node(n)))
         if (nach_links >= 0.0) then ! rechts auf der linken seite?
            left_element(n) = nele_rechts
         else !rechts ist rechts
            right_element(n) = nele_rechts
         endif ! rechts auf der linken seite?
      endif ! left_element(n) vorhanden ???
   enddo ! alle n Kanten
   deallocate (ed_fa, stat = alloc_status )
   
   print*
   print "(a,i0)", "boundary edges = ", anzahl_randkanten
   
   allocate (nbc(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (nbc( failed')
   call check_err( trim(nc_error_prefix), nf90_inq_varid(ncid,'Mesh2_edge_bc', didi) )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, didi, nbc ) )

   do n = 1,kantenanzahl ! alle Kanten
      if (boundary_number(n) == 0) then ! Kante mit zwei Elementen links und rechts
         if (nbc(n) /= 0)call qerror("Randkantenfehler 00")
      else ! Kante mit nur einem Element
         if (nbc(n) == 0)call qerror("Randkantenfehler 11")
         boundary_number(n) = nbc(n)
      endif ! Kante mit zwei Elementen links und rechts
   enddo ! alle n Kanten
   deallocate (nbc, stat = alloc_status )
   allocate (intereck(4*n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (intereck(4  failed')
   ! Nachbarelemente ermitteln und in intereck speichern
   do n = 1,n_elemente ! alle Elemente
      do k = 1,4
         intereck((n-1)*4+k) = 0
      enddo
      do k = 1,cornernumber(n)
         ! Nachbarelemente
         if ( left_element(elementedges(n,k)) == n) intereck((n-1)*4+k) = right_element(elementedges(n,k))
         if (right_element(elementedges(n,k)) == n) intereck((n-1)*4+k) = left_element(elementedges(n,k))
      enddo ! alle k Kanten im Element
   enddo ! alle n Elemente
   
   ! -------------------------------------------------------------------------
   ! boundaries
   ! -------------------------------------------------------------------------
   ! Die Variablen sind bei Untrim-Antrieb an den Elementen definiert, daher 
   ! müssen doert auch die Randbedingungen angebracht werden. Zuflussränder 
   ! sind aber nur an den Kanten erkennbar, daher müssen sie hier jetzt in 
   ! element_rand eingearbeitet werden:
   do n = 1,n_elemente ! alle Elemente
      ndumm = element_rand(n)
      element_rand(n) = 0
      do k = 1,cornernumber(n)
         ! Element an Randkante(Durchflussrand)
         if (boundary_number(elementedges(n,k)) >= 2) then !! nur Zuflussrand aus Kanten in Elemente übernehmen
            ! bei mehreren Randkanten am Element, element_rand auf die größte Kanten-Randnummer setzen
            if (boundary_number(elementedges(n,k)) > element_rand(n)) then ! winner takes it all
               element_rand(n) = boundary_number(elementedges(n,k))
            endif ! winner
         endif ! Zufluss-Randkante
      enddo ! alle k Kanten im Element
      
      if (ndumm > 0) then ! Element hatte schon Randnummer
         if (ndumm /= element_rand(n)) then ! Randunummernkonflikt vermeiden
            element_rand(n) = ndumm !! in der Regel ist das ein Wasserstandsrand
         else
            print*,'Randunummernkonflikt read_mesh_nc in Element #',n   &
            ,' element_rand# aus edges = ',element_rand(n),'element_rand# aus faces = ',ndumm
            call qerror('Randunummernkonflikt read_mesh_nc')
         endif !Randunummernkonflikt
      endif ! Element hatte schon Randnummer
   enddo ! alle n Elemente
   
end subroutine read_mesh_nc


!> Zonen und Randnummern von der Datei ELEMENTE.txt einlesen,
!! selbige wurde von Gerris erzeugt.
subroutine read_elemente_gerris()
   use modell
   implicit none
   
   integer                         :: k, n, alloc_status, io_error, nelli, neln, nelz, nelr, errcode
   real                            :: elx, ely, dist
   real, allocatable, dimension(:) :: rbc
   character(longname)             :: filename
   
   
   filename = trim(modellverzeichnis) // 'ELEMENTE.txt'
   open(unit = 22 , file = filename, status = 'old', action = 'read ', iostat = io_error)
   if (io_error /= 0) call qerror("Could not open " // trim(filename))
   
   if (.not. zeile(22)) call qerror('Lesen erste Zeile von ELEMENTE.txt fehlgeschlagen')
   if (.not. zeile(22)) call qerror('Lesen zweite Zeile von ELEMENTE.txt fehlgeschlagen')
   if (.not. zeile(22)) call qerror('Zeile 2(3) in ELEMENTE.txt fehlt')
   
   read(ctext, *, iostat = io_error) nelli
   if ( (io_error /= 0) .or. ( nelli /= n_elemente) ) then
      write(fehler, "(a,*(i0))") 'Elementanzahl in ELEMENTE.txt falsch oder nicht lesbar',nelli,n_elemente,io_error
      call qerror(fehler)
   endif
   
   do n = 1,n_elemente ! alle Elemente
      if ( .not. zeile(22))call qerror('Zeile in ELEMENTE.txt nicht lesbar')
      read(ctext, *, iostat = io_error) neln, elx, ely, nelz, nelr
      if (n /= neln)call qerror('Elementnummer in ELEMENTE.txt falsch')
      dist = (elx-element_x(n))**2 + (ely-element_y(n))**2
      if (dist > 25)call qerror('Elementzentren mehr als 5 Meter Entfernung')
      element_zone(n) = nelz
      element_rand(n) = nelr
   enddo ! alle n Elemente
   !     knoten_rand ebenfalls nur zu Darstellungszwecken ... zeigt kanten-randnummern
   !     allocate (cell_bound_length(kantenanzahl), stat = alloc_status )
   allocate (rbc(knotenanzahl2D), source = 0.0, stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (rbc failed')
   
   do n = 1,kantenanzahl
      rbc(top_node(n)) = rbc(top_node(n))+0.5*real(boundary_number(n))
      rbc(bottom_node(n)) = rbc(bottom_node(n))+0.5*real(boundary_number(n))
   enddo 
   
   allocate(knoten_rand(knotenanzahl2D), stat = alloc_status)
   if (alloc_status /= 0) call qerror('allocate (knoten_rand failed')
   do n = 1,knotenanzahl2D
      knoten_rand(n) = nint(rbc(n))
   enddo
   deallocate(rbc)
   
   ! Knoten-zonen zu Darstellungszwecken
   knoten_zone(:) = 0
   
   do n = 1,n_elemente ! alle Elemente
      do k = 1,cornernumber(n)
         !! höchste Zonennummer am Knoten, wenn verschiedene aus angrenzenden Elementen
         if ( element_zone(n) > knoten_zone(elementnodes(n,k)) ) knoten_zone(elementnodes(n,k)) = element_zone(n)
         !! Randknoten auf Element-nummern setzen:
         if ((knoten_rand(elementnodes(n,k)) > 0) .and. (element_rand(n) > 0))knoten_rand(elementnodes(n,k)) = element_rand(n)
      enddo
   enddo
   
end subroutine read_elemente_gerris


subroutine nc_sichten()
   use netcdf
   use modell
   use module_datetime
   implicit none
   include 'netcdf.inc'

   integer, parameter                         :: attstrlen = 2000
   character(attstrlen)                       :: attstring
   integer                                    :: i, n, alloc_status, didi
   integer                                    :: year, month, day, hour, minutes, second, tz_hour, tz_minute
   integer                                    :: expected_deltat, delta_t, time_seconds
   real                                       :: tz
   real, dimension(:), allocatable            :: time_hours
   type(datetime)                             :: datetime_reference
   type(datetime), dimension(:), allocatable  :: datetime_step
   type(timedelta)                            :: timedelta_step
   
   character(50), parameter :: nc_error_prefix = 'nc_sichten'
   
   if (meinrang == 0) then
      ! transinfo_anzahl bereits bekannt
      if (transinfo_anzahl < 1) call qerror('No transport info')
      
      allocate (time_hours(transinfo_anzahl), stat = alloc_status )
      if (alloc_status /= 0) call qerror("allocate (time_hours(transinfo_anzahl) failed")
      
      allocate (transinfo_zeit(transinfo_anzahl),transinfo_zuord(transinfo_anzahl), stat = alloc_status )
      if (alloc_status /= 0) call qerror("allocate (transinfo_zeit(transinfo_anzahl) failed")
      
      allocate(datetime_step(transinfo_anzahl))
      
      ! --- read timestamps from NetCDF file ---
      do i = 1,attstrlen
         attstring(i:i) = ' '
      enddo
      
      call check_err(trim(nc_error_prefix), nf90_inq_varid(ncid,"nMesh2_data_time", didi))
      
      
      ! --- get reference date ---
      ! attsting should look like this: "hours since 2010-01-01 03:00:00 01:00
      call check_err(trim(nc_error_prefix), nf_get_att_text(ncid, didi, 'units', attstring))
      if (attstring(1:5) /= "hours") then
         call qerror("Error in transport.nc: Unit of variable `nMesh2_data_time` must be hours.")
      endif
      
      read(attstring(13:16), "(i4)") year
      read(attstring(18:19), "(i2)") month
      read(attstring(21:22), "(i2)") day
      read(attstring(24:25), "(i2)") hour
      read(attstring(27:28), "(i2)") minutes
      read(attstring(30:31), "(i2)") second
      read(attstring(33:34), "(i2)") tz_hour
      read(attstring(36:37), "(i2)") tz_minute
      
      ! convert timezone into hours
      tz = real(tz_hour) + real(tz_minute) / 60.
      datetime_reference = datetime(year, month, day, hour, minutes, second, tz = tz)
      
      
      ! --- get timestamps ---
      ! timestamps are given in hours since reference time
      call check_err(trim(nc_error_prefix), nf90_get_var(ncid, didi, time_hours))
      
      ! convert timestamps into unixtime
      do n = 1,transinfo_anzahl
         time_seconds = nint(time_hours(n) * 3600.)
         timedelta_step = timedelta(seconds = time_seconds)
         
         ! datetime of timestep
         datetime_step(n) = datetime_reference + timedelta_step
         
         ! unixtime of timestep
         transinfo_zeit(n) = datetime_step(n) % seconds_since_epoch()
         transinfo_zuord(n) = n
      enddo
      
      expected_deltat = transinfo_zeit(2) - transinfo_zeit(1)
      do n = 2, transinfo_anzahl
         delta_t = transinfo_zeit(n) - transinfo_zeit(n-1)
         
         ! In the NetCDF file the timestep is defined as `double`. Therefore
         ! some timesteps are not represented exactly, i.e. a timestep of
         ! 20 minutes is represented as 0.3333.. hours. When converted to an 
         ! integer, as we do here, this can cause small rounding issues.
         ! Therefore we use a tolerance of 10 seconds when checking for 
         ! equality
         if (abs(delta_t - expected_deltat) > 10) then 
            print*, n , abs(delta_t - expected_deltat)
            call qerror("Error in transport.nc: Timestep is not constant.")
         endif
      enddo
   
      deallocate(time_hours)
      
      ! -----------------------------------------------------------------------
      ! print summary to console
      ! -----------------------------------------------------------------------
      print*
      print "(a)", repeat("-", 80)
      print "(a)", "timesteps transport.nc"
      print "(a)", repeat("-", 80)
      
      print "(a,i0)",    "n-timesteps:    ", transinfo_anzahl
      print "(2a)",      "unit string:    ", trim(attstring)
      print "(3a,i0,a)", "reference time: ", datetime_reference % date_string(), " [", datetime_reference % seconds_since_epoch(), "]"
      print "(3a,i0,a)", "start time:     ", datetime_step(1) % date_string() ,               " [", transinfo_zeit(1),"]"
      print "(3a,i0,a)", "end time:       ", datetime_step(transinfo_anzahl) % date_string(), " [", transinfo_zeit(transinfo_anzahl),"]"
      print "(a,i0,a)",  "timestep:       ", expected_deltat, " seconds"
      
   endif
   
   
   call mpi_barrier (mpi_komm_welt, ierr)
   
end subroutine nc_sichten


!! nv_read.f95
subroutine nvread()
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   
   integer                                     :: nAtts, attnum, alen, xtype, varid
   integer                                     :: j, k, ival, u_log
   integer                                     :: ndims, nVars, nGlobalAtts, unlimdimid
   integer                                     :: alloc_status, open_error
   integer, allocatable , dimension(:)         :: dlength
   integer, allocatable, dimension(:)          :: vxtype, vndims
   integer, dimension(nf90_max_var_dims)       :: dimids
   character(256)                              :: aname
   character(2000)                             :: attstring
   character(longname)                         :: filename
   character(256), allocatable , dimension (:) :: vname, dname
   real, dimension(4)                          :: rval
   real, dimension(8)                          :: dval
   real, dimension(:), allocatable             :: zeiten
   
   character(len=50), parameter :: nc_error_prefix = 'nvread'
   
   write(*,*) 'nvread() started'
   open(newunit = u_log , file = 'netcdf.log', status = 'replace', &
        action = 'write', iostat = open_error)
   
   filename = trim(modellverzeichnis) // 'transport.nc'
   call check_err( trim(nc_error_prefix), nf_open(filename, NF_NOWRITE, ncid))
   
   !!  Überblick
   call check_err( trim(nc_error_prefix), nf90_inquire(ncid, ndims, nVars, nGlobalAtts, unlimdimid))
   write(u_log,*)"ndims, nVars, nGlobalAtts = ", ndims, nVars, nGlobalAtts
   
   ! --- dimensions ---
   allocate (dlength(ndims), stat = alloc_status )
   allocate (dname(ndims), stat = alloc_status )
   do j = 1,ndims
      call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j)))
      write(u_log,*)'dimension  ' ,trim(adjustl(dname(j))),' wert = ', dlength(j)
   enddo !j
   write(u_log,*)'--'
   
   ! --- variables ---
   allocate (vxtype(nVars), stat = alloc_status )
   allocate (vndims(nVars), stat = alloc_status )
   allocate (vname(nVars), stat = alloc_status )
   do j = 1,nVars
      call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,j,vname(j),vxtype(j),vndims(j),dimids, nAtts))
      write(u_log,*)'Variable : ' ,trim(adjustl(vname(j)))
      write(u_log,*)"Dimensionen: "
      do k = 1,vndims(j)
         write(u_log,*)'   ', trim(adjustl(dname(dimids(k)))), dlength(dimids(k))
      enddo 
      write(u_log,*)"Attribute : "
      call print_attributes(j, nAtts)
      write(u_log,*)'--'
   enddo
   
   ! --- global attributes ---
   write(u_log,*)'Globale Attribute: '
   call print_attributes(NF90_GLOBAL, nGlobalAtts)
   write(u_log,*)'--'
   
   
   ! --------------------------------------------------------------------------
   ! mesh
   ! --------------------------------------------------------------------------
   call netcdf_mesh_only()
   !! Zeiten:
   !! double nMesh2_data_time(nMesh2_data_time)
   write(u_log,*)'Ausgabe Zeiten'
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'nMesh2_data_time', varid))
   write(u_log,*)'nMesh2_data_time: '," varid = ",varid
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts))
   do k = 1,vndims(varid)
      write(u_log,*)'   nMesh2_data_time:dim  ', trim(adjustl(dname(dimids(k)))), dlength(dimids(k))
   enddo
   
   if (dlength(dimids(1)) > 0) then
      allocate (zeiten(dlength(dimids(1))), stat = alloc_status )
      call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, zeiten))
      write(u_log,*)'Zeit  von:',zeiten(1),zeiten(2),' bis: ',zeiten(dlength(dimids(1)))
      do k = dlength(dimids(1))-12,dlength(dimids(1))!! nur mal die letzten 12 Zeitschritte ausgeben
         call untrim_vtk(k)
      enddo !k Dimensionen von Variable j
   endif
   
   write(u_log,*)'--'
   call check_err( trim(nc_error_prefix), nf_close(ncid))
   close(u_log)
   
   write(*,*)'nvread() regular end'
   
end subroutine nvread
!-----------------------------------------------------------------------
subroutine check_err(nc_error_prefix, i_netcdf_error)
   implicit none
   
   character(len=*), intent(in) :: nc_error_prefix
   integer         , intent(in) :: i_netcdf_error
   
   include 'netcdf.inc'
   
   
   if (i_netcdf_error /= NF_NOERR) then
      call qerror(trim(nc_error_prefix) // ": " // trim(nf_strerror(i_netcdf_error)))
   endif
   
end subroutine check_err


subroutine print_attributes(nvar, nAtts)
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   
   integer, intent(in) :: nvar, natts
   
   integer, parameter  :: attstrlen = 2000
   integer              :: i, j, k, nd, nv, xtype, alen, attnum, ival
   real, dimension(4)   :: rval
   real, dimension(8)   :: dval
   character(256)       :: aname
   character(attstrlen) :: attstring
   
   character(len=50), parameter :: nc_error_prefix = 'print_attributes'
   
   do k = 1,nAtts
      call check_err( trim(nc_error_prefix), nf_inq_attname(ncid, nvar, k, aname))
      call check_err( trim(nc_error_prefix), nf90_Inquire_Attribute(ncid, nvar, aname, xtype, alen, attnum))
      
      select case (xtype)
      case(NF90_BYTE)
         call check_err( trim(nc_error_prefix), nf_get_att_int(ncid, nvar, aname, ival))
         write(123,*)'b   ',trim(adjustl(aname))," = ", ival
      
      case(NF90_CHAR)
         do i = 1,attstrlen
            attstring(i:i) = ' '
         enddo
         call check_err( trim(nc_error_prefix), nf_get_att_text(ncid, nvar, aname, attstring))
         write(123,"(3A,3x,I10,3x,A)")'c   ',trim(adjustl(aname))," has length = ",len(trim(attstring)),trim(adjustl(attstring))
      
      case(nf90_short)
         call check_err( trim(nc_error_prefix), nf_get_att_int(ncid, nvar, aname, ival))
         write(123,*)'ishort   ',trim(adjustl(aname))," = ", ival
      
      case(NF90_INT)
         call check_err( trim(nc_error_prefix), nf_get_att_int(ncid, nvar, aname, ival))
         write(123,*)'i   ',trim(adjustl(aname))," = ", ival
      
      case(NF90_FLOAT)
         call check_err( trim(nc_error_prefix), nf_get_att_real(ncid, nvar, aname, rval))
         write(123,*)'i   ',trim(adjustl(aname))," = ", rval
      
      case(NF90_DOUBLE)
         call check_err( trim(nc_error_prefix), nf_get_att_double (ncid, nvar, aname, dval))
         write(123,*)'d   ',trim(adjustl(aname))," = ", dval
      
      case default
         call qerror('netCDF external data typ unkown')
      end select
   enddo 
   
end subroutine print_attributes



subroutine untrim_vtk(nt)
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   
   integer, intent(in) :: nt
   
   integer                               :: alloc_status, open_error, errcode
   integer                               :: j, k, n, nvar, ion, varid, vxtype, vndims, dlength
   integer                               ::  i_corn, kantenanzahl2D
   integer, dimension(nf90_max_var_dims) :: dimids
   integer, dimension(2)                 :: start2, count2
   integer, dimension(3)                 :: start3, count3
   real, allocatable, dimension(:)       :: wsp, volume, qarea, velmag
   character(256)                        :: dname
   character(longname)                   :: filename, systemaufruf, zahl, vname
   
   character(len=50), parameter :: nc_error_prefix = 'untrim_vtk'
   
   write(*,*)"untrim_vtk"
   if (nt > 0) then ! there are timesteps
      write(zahl,*)nt
      zahl = adjustl(zahl)
      filename = trim(modellverzeichnis) // 'netcdf_face_' // trim(zahl) // '.vtk'
   else 
      ! no timesteps
      return
   endif
   
   open(newunit = ion , file = filename, status = 'replace', action = 'write ', iostat = open_error)
   if (open_error /= 0) call qerror("Error while opening " // trim(filename))
   
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'reading SCHISM/untrim2 netCDF'
   write(ion,'(A)')'ASCII'
   !write(ion,'(A)')'DATASET POLYDATA'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   !Variable :   Mesh2_face_x
   !Dimensionen: nMesh2_face       11103
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_x', varid))
   write(*,*)'Mesh2_face_x: varid = ',varid
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids))
   do k = 1,vndims
      call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength))
      write(*,*)'   Mesh2_face_x:dim  ', trim(adjustl(dname)), dlength
   enddo !k Dimensionen von Variable j
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(1), dname, dlength))
   knotenanzahl2D = dlength
   allocate (knoten_x(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror("Error while allocating variable `knoten_x`")
   
   allocate (knoten_y(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror("Error while allocating variable `knoten_y`")
   
   allocate (knoten_z(knotenanzahl2D), source = 0.0, stat = alloc_status )
   if (alloc_status /= 0) call qerror("Error while allocating variable `knoten_z`")
   
   allocate(wsp(knotenanzahl2D))
   allocate(volume(knotenanzahl2D))
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, knoten_x))
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_y', varid))
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, knoten_y))
   
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
   do n = 1,knotenanzahl2D
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
   enddo
   
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
   !write(ion,'(A)')'SCALARS Gelaendehoehe float 1'
   !write(ion,'(A)')'LOOKUP_TABLE default'
   !do n=1,knotenanzahl2D
   !   write(ion,'(f27.6)') knoten_z(n)
   !enddo ! alle Knoten
   ! Variable : Mesh2_face_Wasserstand_2d
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_Wasserstand_2d', varid))
   start2 = (/ 1, nt /)
   count2 = (/ knotenanzahl2D, 1 /)
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, wsp, start2, count2 ))
   write(ion,'(A)')'SCALARS WSP float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,*) wsp(n)
   enddo ! alle Knoten
   ! Variable : Mesh2_face_Salzgehalt_2d
   start3 = (/ 1, 1, nt /)
   count3 = (/ knotenanzahl2D, 1, 1 /)
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_Salzgehalt_2d', varid))
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, knoten_z, start3, count3 ))
   write(ion,'(A)')'SCALARS Salz_PSU float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,*) knoten_z(n)
   enddo 
   
   start3 = (/ 1, 1, nt /)
   count3 = (/ knotenanzahl2D, 1, 1 /)
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_Wasservolumen_2d', varid))
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, volume, start3, count3 ))
   write(ion,'(A)')'SCALARS Wasservolumen float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,*) volume(n)
   enddo
   
   close(ion)
   
   deallocate(wsp, volume)
   deallocate(knoten_x, knoten_y, knoten_z)
   
   ! --------------------------------------------------------------------------
   ! edges
   ! --------------------------------------------------------------------------
   if (nt > 0) then ! there are timesteps
      write(zahl,*)nt
      zahl = adjustl(zahl)
      filename = trim(modellverzeichnis) // 'netcdf_edge_' // trim(zahl) // '.vtk'
   else
      ! no timesteps
      return
   endif
   
   open(newunit = ion , file = filename, status = 'replace', action = 'write ', iostat = open_error )
   if (open_error /= 0) call qerror("Could not open " // filename)
   
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'reading SCHISM/untrim2 netCDF'
   write(ion,'(A)')'ASCII'
   !write(ion,'(A)')'DATASET POLYDATA'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   !Mesh2_edge_x
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_edge_x', varid))
   write(*,*)'Mesh2_edge_x: varid = ',varid
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids))
   do k = 1,vndims
      call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength))
      write(*,*)'   Mesh2_edge_x:dim  ', trim(adjustl(dname)), dlength
   enddo 
   
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(1), dname, dlength))
   kantenanzahl2D = dlength
   allocate (knoten_x(kantenanzahl2D),knoten_y(kantenanzahl2D),knoten_z(kantenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_x(kantenanzahl2D) :', alloc_status
      call qerror(fehler)
   endif
   
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, knoten_x))
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid, 'Mesh2_edge_y', varid))
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, knoten_y))
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',kantenanzahl2D, ' float'
   do n = 1,kantenanzahl2D
      knoten_z(n) = 0.0
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
   enddo
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', kantenanzahl2D
   
   ! Variable : Mesh2_edge_Durchflussflaeche_2d
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_edge_Durchflussflaeche_2d', varid))
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids))
   print *,'Mesh2_edge_Durchflussflaeche_2d: vndims = ',vndims
   do k = 1,vndims
      call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength))
      print *,'   Mesh2_edge_Durchflussflaeche_2d::dim  ', trim(adjustl(dname)), dlength
   enddo !k Dimensionen von Variable j
   
   allocate (qarea(kantenanzahl2D), stat = alloc_status )
   allocate (velmag(kantenanzahl2D), stat = alloc_status )
   start3 = (/ 1, 1, nt /)
   count3 = (/ kantenanzahl2D, 1, 1 /)
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, qarea, start3, count3 ))
   write(ion,'(A)')'SCALARS Qflaech float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,kantenanzahl2D
      write(ion,*) qarea(n)
   enddo
   start3 = (/ 1, 1, nt /)
   count3 = (/ kantenanzahl2D, 1, 1 /)
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_edge_skalare_Stroemungsgeschwindigkeit_2d', varid))
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, velmag, start3, count3 ))
   write(ion,'(A)')'SCALARS vbetr float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,kantenanzahl2D
      write(ion,*) velmag(n)
   enddo
   
   close(ion)
   deallocate(qarea, velmag)
   deallocate(knoten_x, knoten_y, knoten_z)
   
end subroutine untrim_vtk


subroutine netcdf_mesh_only()
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   
   integer                               :: ion, alloc_status, open_error, errcode
   integer                               :: j, k, n, nvar, varid, vxtype, vndims, dlength
   integer                               :: i_corn, kantenanzahl2D
   integer, allocatable, dimension(:,:)  :: fa_no, ed_fa
   integer, dimension(nf90_max_var_dims) :: dimids
   integer, dimension(2)                 :: start2, count2
   integer, dimension(3)                 :: start3, count3
   character(256)                        :: dname
   character(longname)                   :: filename, systemaufruf, vname
   
   character(len=50), parameter :: nc_error_prefix = 'netcdf_mesh_only'
   
   ! --------------------------------------------------------------------------
   ! nodes
   ! --------------------------------------------------------------------------
   filename = trim(modellverzeichnis) // 'netcdf_mesh_node.vtk'
   open(newunit = ion, file = filename, status = 'replace', action = 'write ', iostat = open_error )
   if (open_error /= 0) call qerror("Could not open " // trim(filename))
  
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'mesh_only SCHISM/untrim2 netCDF'
   write(ion,'(A)')'ASCII'
   !write(ion,'(A)')'DATASET POLYDATA'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_node_x', varid))
   write(*,*)'Mesh2_node_x: varid = ',varid
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids))
   do k = 1,vndims
      call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength))
      write(*,*)'   Mesh2_node_x:dim  ', trim(adjustl(dname)), dlength
   enddo !k Dimensionen von Variable j
   
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(1), dname, knotenanzahl2D))
   print*,'knotenanzahl2D nodes = ', knotenanzahl2D
   allocate(knoten_x(knotenanzahl2D))
   allocate(knoten_y(knotenanzahl2D))
   allocate(knoten_z(knotenanzahl2D), source = 0.0)
   
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, knoten_x))
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_node_y', varid))
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, knoten_y))
   
   print*, 'netcdf_mesh_only minx, maxx,miny,maxy = ', &
            minval(knoten_x), maxval(knoten_x),        &
            minval(knoten_y), maxval(knoten_y)
   
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
   do n = 1,knotenanzahl2D
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
   enddo
   
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_nodes', varid))
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids))
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(1), dname, dlength))
   write(*,*)'   Mesh2_face_nodes:dim 1  ', trim(adjustl(dname)), dlength
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(2), dname, n_elemente))
   write(*,*)'   Mesh2_face_nodes:dim 2  ', trim(adjustl(dname)), n_elemente
   allocate (fa_no(4,n_elemente), stat = alloc_status )
   allocate (elementnodes(n_elemente,4), stat = alloc_status )
   allocate (cornernumber(n_elemente), stat = alloc_status )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, fa_no))
   
   i_corn = 0
   do n = 1,n_elemente ! alle Elemente
      if (fa_no(4,n) < 0) then
         cornernumber(n) = 3
      else
         cornernumber(n) = 4
      endif
      i_corn = i_corn+cornernumber(n)+1
      do k = 1,cornernumber(n)
         elementnodes(n,k) = fa_no(k,n)+1
         if ((elementnodes(n,k) < 1) .or. (elementnodes(n,k) > knotenanzahl2D)) then
            write(fehler,*)'netcdf_mesh_only elementnodes falsch:',elementnodes(n,k),n,k
            call qerror(fehler)
         endif
      enddo ! alle Elementecken
   enddo ! alle Elemente
   deallocate (fa_no, stat = alloc_status )
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,I12)')'CELLS ', n_elemente,i_corn
   do n = 1,n_elemente ! alle Elemente
      if (cornernumber(n) == 4) then
         write(ion,'(A,2x,4(I8,2x))') &
                                  '4',elementnodes(n,1)-1,elementnodes(n,2)-1,elementnodes(n,3)-1,elementnodes(n,4)-1
      else
         write(ion,'(A,2x,3(I8,2x))') &
                                  '3',elementnodes(n,1)-1,elementnodes(n,2)-1,elementnodes(n,3)-1
      endif
   enddo ! alle Elemente
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'CELL_TYPES ', n_elemente
   do n = 1,n_elemente ! alle Elemente
      if (cornernumber(n) == 4) then
         write(ion,'(A)') '9'
      else
         write(ion,'(A)') '5'
      endif
   enddo ! alle Elemente
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
   write(ion,'(A)')'SCALARS zet float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') knoten_z(n)
   enddo ! alle Knoten
   
   close (ion)
   deallocate(knoten_x, knoten_y, knoten_z)
   
   ! --------------------------------------------------------------------------
   ! elements
   ! --------------------------------------------------------------------------
   filename = trim(modellverzeichnis) // 'netcdf_mesh_element.vtk'
   open(newunit = ion , file = filename, status = 'replace', action = 'write ', iostat = open_error )
   if (open_error /= 0) call qerror("Could not open " // trim(filename))
   
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'mesh_only SCHISM/untrim2 netCDF'
   write(ion,'(A)')'ASCII'
   !write(ion,'(A)')'DATASET POLYDATA'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   !Variable :   Mesh2_face_x
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_x', varid))
   write(*,*)'Mesh2_face_x: varid = ',varid
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids))
   do k = 1,vndims
      call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength))
      write(*,*)'   Mesh2_face_x:dim  ', trim(adjustl(dname)), dlength
   enddo !k Dimensionen von Variable j
   
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(1), dname, knotenanzahl2D))
   print*,'knotenanzahl2D faces = ', knotenanzahl2D
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
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, knoten_x))
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_face_y', varid))
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, knoten_y))
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
   do n = 1,knotenanzahl2D
      knoten_z(n) = 0.0
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
   enddo ! alle Knoten
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,'Mesh2_edge_faces', varid))
   print *,'Mesh2_edge_faces: varid = ',varid
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids))
   print *,'Mesh2_edge_faces: vndims = ',vndims
   do k = 1,vndims
      call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength))
      print *,'   Mesh2_edge_faces::dim  ', trim(adjustl(dname)), dlength
   enddo !k Dimensionen von Variable j
   call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, dimids(2), dname, kantenanzahl2D))
   allocate (ed_fa(2,kantenanzahl2D), stat = alloc_status )
   call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, ed_fa))
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl2D, 3*kantenanzahl2D
   do n = 1,kantenanzahl2D
      if (ed_fa(2,n) <= 0) ed_fa(2,n) = ed_fa(1,n)
      if (ed_fa(1,n) <= 0) ed_fa(1,n) = ed_fa(2,n)
      write(ion,'(A,2x,I8,2x,I8)')'2', ed_fa(1,n), ed_fa(2,n)
   enddo ! alle Knoten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl2D
   do n = 1,kantenanzahl2D
      write(ion,'(A)')'3'
   enddo ! alle Knoten
   
   !int Mesh2_edge_faces(nMesh2_edge, two) ;
   !      Mesh2_edge_faces:long_name = "Face- (Polygon-) Verzeichnis der Kanten, linker und rechter Nachbar" ;
   !      Mesh2_edge_faces:cf_role = "edge_face_connectivity" ;
   !      Mesh2_edge_faces:_FillValue = -999 ;
   deallocate (ed_fa, stat = alloc_status )
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', kantenanzahl2D
   write(ion,'(A)')'SCALARS zet float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,kantenanzahl2D
      write(ion,'(f27.6)') 0.01
   enddo
   
   close(ion)
   deallocate(knoten_x, knoten_y, knoten_z)
   
   ! --------------------------------------------------------------------------
   ! edges
   ! --------------------------------------------------------------------------
   ! int Mesh2_edge_nodes(nMesh2_edge, two) ;
   !     Mesh2_edge_nodes:long_name = "Knotenverzeichnis der Kanten, Anfangs- und Endpunkt" ;
   !     Mesh2_edge_nodes:cf_role = "edge_node_connectivity" ;
   !     Mesh2_edge_nodes:start_index = 0 ;
   print*,'netcdf_mesh_only under development'
   
   
end subroutine netcdf_mesh_only
