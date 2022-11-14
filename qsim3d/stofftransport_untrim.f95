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

subroutine stofftransport_untrim()
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   integer nti, nt, n,j,k, subtim, diff, diffprev, alloc_status, iq, jq, no, nedel
   real :: laeng, cu_max, cu_min, dt_sub, sumwicht, cu_mean_CuGT1, volFrac_CuGT1, fluxi, flow
   real , allocatable , dimension (:,:) :: zwischen
   !integer , parameter :: num_sub=12
   integer , parameter :: num_sub = 6
   logical found
   if (meinrang /= 0)call qerror('stofftransport_untrim darf nur von prozessor 0 aufgerufen werden')
   allocate (zwischen(number_plankt_vari, number_plankt_point), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (zwischen(number_plankt_vari failed')
   print*,'stofftransport_untrim: izeit, startzeitpunkt, zeitpunkt, deltat, endzeitpunkt' &
   ,izeit,startzeitpunkt, zeitpunkt, deltat, endzeitpunkt
   dt_sub = real(deltat)/real(num_sub)
   print*,'stofftransport_untrim:',num_sub,' Sub-zeitschritte von der Länge = ',dt_sub
   do nt = 1,num_sub ! alle Transport (zwischen) Zeitschritte
      if ((kontrollknoten > 0) .and. (kontrollknoten <= number_plankt_point)) then ! Ausgabe
         print*  &
         ,' vor transportschritt untrim T_wass(kontrollelement) = '  &
         , planktonic_variable(1+(kontrollknoten-1)*number_plankt_vari) &
         ,' Tracer = ', planktonic_variable(71+(kontrollknoten-1)*number_plankt_vari)
      end if !kontrollknoten
      subtim = startzeitpunkt + int( real((2*nt-1)*deltat)/real(num_sub*2) )
      
      if (subtim < transinfo_zeit(transinfo_zuord(1)))call qerror('subzeitpunkt vor untrim Zeitraum')
      if (subtim > transinfo_zeit(transinfo_zuord(transinfo_anzahl)))call qerror('subzeitpunkt nach untrim Zeitraum')
      nti = 0
      diffprev = transinfo_zeit(transinfo_zuord(transinfo_anzahl))-transinfo_zeit(transinfo_zuord(1))
      do n = 1,transinfo_anzahl
         diff = abs(subtim-transinfo_zeit(transinfo_zuord(n)) )
         if (diff <= diffprev) then
            nti = n
         endif
         diffprev = diff
      end do ! alle n Zeitschritte
      print*,'stofftransport_untrim: substep-time,nti,diff = ',subtim,nti,diff
      if (nti <= 0) then
         call qerror('stofftransport_untrim: kein untrim Zeitpunkt identifiziert')
      else
         print*,'transportiert mit untrim-Strömungsfeld zeit = ',transinfo_zeit(transinfo_zuord(nti))
      end if
      call holen_trans_untrim(nti)
      !do n=1,number_plankt_point
      !   if(inflow(n))planktonic_variable(71+(n-1)*number_plankt_vari)=1.0 ! test tracer zuflussränder untrim  ################
      !end do ! alle n Elemente
      do n = 1,number_plankt_point ! initialize volume exchange
         do j = 1,5
            wicht((n-1)*5+j) = 0.0
         end do !alle 5
      end do ! alle n Elemente
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
                     !print*,fluxi,'fluxi edge n=',n,' part=',jq,' of section=',iq,  &
                     !      'l,A,fl,v=',laeng,ed_area(n),flow,((ed_vel_x(n)**2.0)+(ed_vel_y(n)**2.0) )**0.5 ,  &
                     !      'left,right=',left_element(n),right_element(n)
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
                           schnittflux_gang(iq,izeit, no+2 ) = schnittflux_gang(iq,izeit, no+2 )+  &
                                                               fluxi * planktonic_variable(k+(nedel-1)*number_plankt_vari)
                        endif ! planktic output conc.
                     end do ! all k planktic variables
                  endif ! found
               end do ! all jq
            end do ! all iq cross sections
         endif ! querschneiden
         
      end do ! alle n edges
      do n = 1,n_elemente ! gathering inflows
         do k = 1,cornernumber(n)
            !if I am the left_element of my n-th edge and the flux is to the left
            if ( (left_element(elementedges(n,k)) == n) .and. (ed_flux(elementedges(n,k)) > 0.0) )  &
                wicht((n-1)*5+(1+k)) = ed_flux(elementedges(n,k))
            !if I am the right_element of my n-th edge and the flux is to the right
            if ( (right_element(elementedges(n,k)) == n) .and. (ed_flux(elementedges(n,k)) < 0.0) )  &
                wicht((n-1)*5+(1+k)) = -1.0 * ed_flux(elementedges(n,k))
         end do ! alle k Kanten im Element
      end do ! alle n Elemente
      cu_max = -50000.0
      cu_min = 50000.0
      cu = 0.
      do n = 1,n_elemente ! Gewichtsfaktoren aus Volumenverhältnissen ermitteln
         if (el_vol(n) > 0.0) then ! Element wet
            cu(n) = (wicht((n-1)*5+1)*dt_sub)/el_vol(n)
            if (cu(n) > cu_max)cu_max = cu(n)
            if (cu(n) < cu_min)cu_min = cu(n)
            wicht((n-1)*5+1) = 1.0 - cu(n)
            do j = 2,5
               wicht((n-1)*5+j) = (wicht((n-1)*5+j)*dt_sub)/el_vol(n)
            end do !alle 5
         else !Element dry
            wicht((n-1)*5+1) = 1.0
            do j = 2,5
               wicht((n-1)*5+j) = 0.0
            end do !alle 5
         end if !Element wet
         sumwicht = 0.0
         do j = 1,5 !clipping ######
            if (wicht((n-1)*5+j) < 0.0) wicht((n-1)*5+j) = 0.0
            if (wicht((n-1)*5+j) > 1.0) wicht((n-1)*5+j) = 1.0
            sumwicht = sumwicht+wicht((n-1)*5+j)
         end do !alle 5
         if (sumwicht > 0.0) then
            do j = 1,5 !
               wicht((n-1)*5+j) = wicht((n-1)*5+j)/sumwicht
            end do !alle 5
         else
            wicht((n-1)*5+1) = 1.0 ! Wert belibt wie er ist
            do j = 2,5
               wicht((n-1)*5+j) = 0.0
            end do !alle 5
         endif ! sumwicht.gt.0.0
      end do ! alle n Elemente
      ! calculate water volume fraction with Courant number > 1 and average cu in cells with cu > 1
      cu_mean_CuGT1 = sum(el_vol * cu, cu > 1.) / max(1., sum(el_vol, cu > 1.))
      volFrac_CuGT1 = sum(el_vol     , cu > 1.) / max(1., sum(el_vol))
      print*,'stofftransport_untrim: cu_max, cu_min, cu_mean (cu > 1), volume fraction (cu > 1), deltat = ', &
              cu_max, cu_min, cu_mean_CuGT1, volFrac_cuGT1, deltat

      do j = 1,number_plankt_point ! all j elements (*levels?)
         do n = 1,number_plankt_vari ! all transported concentrations i.e. variables
            if (iEros < 0 .and. (n == 52 .or. n == 53)) cycle    ! skip SSalg and SS if SS read from file
            zwischen(n,j) = planktonic_variable(n+(j-1)*number_plankt_vari) *wicht((j-1)*5+1) !! self
            do k = 1,4 ! all 4 neighbour (elements) if existing
               if (intereck((j-1)*4+k) > 0) zwischen(n,j) = zwischen(n,j) +  &
                   planktonic_variable(n+(intereck((j-1)*4+k)-1)*number_plankt_vari) *wicht((j-1)*5+1+k)
            end do
         end do ! alle n Konzentrationen
      end do ! alle j Elemente
      
      do j = 1,number_plankt_point ! alle j Elemente
         if ( .not. inflow(j)) then ! Zuflusselemente auslassen
            do n = 1,number_plankt_vari
               if (iEros < 0 .and. (n == 52 .or. n == 53)) cycle    ! skip SSalg and SS if SS read from file
               if (isNaN(zwischen(n,j))) then
                  print*,'stofftransport_untrim isNaN(zwischen) , plankt_point = ',j,' plankt_vari = ',n
                  print*,'planktonic_variable_name',n, planktonic_variable_name(n)
                  print*,'self !! wicht,plankt =',wicht((j-1)*5+1),planktonic_variable(n+(j-1)*number_plankt_vari)
                  print*,'intereck((j-1)*4+k) = ',( intereck((j-1)*4+k),k = 1,4 )
                  print*,'wicht !! neighbours = ',(wicht((j-1)*5+1+k),k=1,4)
                  do k = 1,4 ! all 4 neighbour (elements) if existing
                     if ( intereck((j-1)*4+k) > 0) then
                        print*,'planktonic_variable(',k,') = '                                      &
                              , planktonic_variable(n+(intereck((j-1)*4+k)-1)*number_plankt_vari)   &
                              ,' tief = ',rb_hydraul(2+(intereck((j-1)*4+k)-1)*number_rb_hydraul)
                     endif
                  end do !all 4 k
                  write(fehler,*)'stofftransport_untrim: isNaN(zwischen planktonic_variable_name', planktonic_variable_name(n)
                  call qerror(fehler)
               else
                  planktonic_variable(n+(j-1)*number_plankt_vari) = zwischen(n,j)
               endif
            end do ! alle n Konzentrationen
         end if !kein Zuflusselement
      end do ! alle j Elemente
      !call ausgeben_untrim( subtim )
      if ((kontrollknoten > 0) .and. (kontrollknoten <= number_plankt_point)) then ! Ausgabe
         print*,'Nach transportschritt untrim am kontrollelement:'
         print*,'Wassertiefe = ',rb_hydraul(2+(kontrollknoten-1)*number_rb_hydraul)          &
               ,'Temp_wass = ',planktonic_variable(1+(kontrollknoten-1)*number_plankt_vari)  &
               ,'chla = '   ,planktonic_variable(11+(kontrollknoten-1)*number_plankt_vari)
         print*,'wicht !! self =',wicht((kontrollknoten-1)*5+1)
         print*,'intereck((j-1)*4+k) = ',( intereck((kontrollknoten-1)*4+k),k = 1,4 )
         print*,'wicht !! neighbours = ',(wicht((kontrollknoten-1)*5+1+k),k=1,4)
      end if
      print*,'transport nt = ',nt,' start ende = ',startzeitpunkt, endzeitpunkt
      !do iq=1,anzahl_quer !! all iq cross sections
      !   print*,nt,"schnittflux: ",zeitpunkt,izeit,iq," flux,volume="  &
      !         ,schnittflux_gang(iq,izeit, 1 ),schnittflux_gang(iq,izeit, 2 )
      !end do
   end do ! alle nt Subzeitschritte
   !do n=1,number_plankt_point
   !   if(inflow(n))planktonic_variable(71+(n-1)*number_plankt_vari)=1.0 ! test tracer zuflussränder untrim ##############
   !end do ! alle n Elemente
   deallocate (zwischen, stat = alloc_status )
   if (alloc_status /= 0) call qerror('deallocate (zwischen, failed')
   !call allo_trans(n_elemente) !! Felder für Transportinformationen und Strömungsfeld allocieren
   !allocate( el_vol(nonu), el_area(nonu), stat = alloc_status )
   do iq = 1,anzahl_quer !! all iq cross sections
      q_gangl(izeit) = zeitpunkt
      schnittflux_gang(iq,izeit, 1 ) = schnittflux_gang(iq,izeit, 1 )/real(num_sub)
      print*,"schnittflux: ",zeitpunkt,izeit,iq," flux = ",schnittflux_gang(iq,izeit, 1 )
   end do
   return
end subroutine stofftransport_untrim
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+

subroutine nvread()
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   integer iret,j,k
   character (len = longname) :: dateiname
   integer :: ndims, nVars, nGlobalAtts, unlimdimid
   integer alloc_status, open_error, errcode
   integer , allocatable , dimension (:) :: dlength
   character(256) , allocatable , dimension (:) :: dname
   integer, dimension(nf90_max_var_dims) :: dimids
   integer , allocatable , dimension (:) :: vxtype, vndims
   character(256) , allocatable , dimension (:) :: vname
   integer nAtts, attnum, alen, xtype, varid
   character(256) :: aname
   character(2000) :: attstring
   integer :: ival
   real(4) :: rval
   real(8) :: dval
   real , allocatable , dimension (:) :: zeiten
   write(*,*)'nvread() started'
   open ( unit = 123 , file = 'netcdf.log', status = 'replace', action = 'write', iostat = open_error )
   write(dateiname,'(2A)',iostat = errcode)trim(modellverzeichnis),'transport.nc'
   if (errcode /= 0)call qerror('nvread writing filename transport.nc failed')
   iret = nf_open(dateiname, NF_NOWRITE, ncid)
   call check_err(iret)
   !!  Überblick
   iret = nf90_inquire(ncid, ndims, nVars, nGlobalAtts, unlimdimid)
   call check_err(iret)
   write(123,*)"ndims, nVars, nGlobalAtts = ",ndims, nVars, nGlobalAtts
   !! Dimensionen
   allocate (dlength(ndims), stat = alloc_status )
   allocate (dname(ndims), stat = alloc_status )
   do j = 1,ndims
      iret = nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j))
      call check_err(iret)
      write(123,*)'dimension  ' ,trim(adjustl(dname(j))),' wert = ', dlength(j)
   end do !j
   write(123,*)'--'
   !! Variablen
   allocate (vxtype(nVars), stat = alloc_status )
   allocate (vndims(nVars), stat = alloc_status )
   allocate (vname(nVars), stat = alloc_status )
   do j = 1,nVars
      !iret = nf90_inquire_variable(ncid,j,vname(j),xtype(j),ndims(j),dimids, nAtts)
      iret = nf90_inquire_variable(ncid,j,vname(j),vxtype(j),vndims(j),dimids, nAtts)
      call check_err(iret)
      write(123,*)'Variable : ' ,trim(adjustl(vname(j)))
      write(123,*)"Dimensionen: "
      do k = 1,vndims(j)
         write(123,*)'   ', trim(adjustl(dname(dimids(k)))), dlength(dimids(k))
      end do !k Dimensionen von Variable j
      !rint*,'inquire_variable ',j,' : ' ,trim(adjustl(vname)),' - ',xtype,ndims,dimids
      write(123,*)"Attribute : "
      call print_attributes(j, nAtts)
      write(123,*)'--'
   end do ! Variable j
   ! nGlobalAtts
   write(123,*)'Globale Attribute: '
   call print_attributes(NF90_GLOBAL, nGlobalAtts)
   write(123,*)'--'
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Netz lesen und ausgeben
   call netcdf_mesh_only()
   !! Zeiten:
   !! double nMesh2_data_time(nMesh2_data_time)
   write(123,*)'Ausgabe Zeiten'
   iret = nf_inq_varid(ncid,'nMesh2_data_time', varid)
   call check_err(iret)
   write(123,*)'nMesh2_data_time: '," varid = ",varid
   iret = nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
   call check_err(iret)
   do k = 1,vndims(varid)
      write(123,*)'   nMesh2_data_time:dim  ', trim(adjustl(dname(dimids(k)))), dlength(dimids(k))
   end do !k Dimensionen von Variable j
   if (dlength(dimids(1)) > 0) then
      allocate (zeiten(dlength(dimids(1))), stat = alloc_status )
      iret = nf90_get_var(ncid, varid, zeiten)
      write(123,*)'Zeit  von:',zeiten(1),zeiten(2),' bis: ',zeiten(dlength(dimids(1)))
      call check_err(iret)
      do k = dlength(dimids(1))-12,dlength(dimids(1))!! nur mal die letzten 12 Zeitschritte ausgeben
         !do k=4320,5400
         !write(123,*)'Zeit  ',k,zeiten(k)
         !! Knoten:
         !! double nMesh2_data_time(nMesh2_data_time)
         call untrim_vtk(k)
      end do !k Dimensionen von Variable j
   end if !more than 0 timesteps
   write(123,*)'--'
   iret = nf_close(ncid)
   call check_err(iret)
   close(123)
   write(*,*)'nvread() regular end'
   return
end subroutine nvread
!-----------------------------------------------------------------------
subroutine check_err(iret)
   implicit none
   integer iret
   include 'netcdf.inc'
   if (iret /= NF_NOERR) then
      write(*,*) nf_strerror(iret)
   endif
   return
end subroutine check_err
!-----------------------------------------------------------------------
subroutine print_attributes( nvar, nAtts)
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   integer, parameter :: attstrlen = 2000
   integer iret,i,j,k,nvar, nd, nv, nAtts, xtype, alen, attnum
   character(256) :: aname
   character(attstrlen) :: attstring
   integer :: ival
   real(4) :: rval
   real(8) :: dval
   do k = 1, nAtts
      iret = nf_inq_attname(ncid, nvar, k, aname)
      call check_err(iret)
      !print*,trim(adjustl(aname))," = "
      !print*,'Attribute ',k, trim(adjustl(aname))
      !iret = nf_inq_atttype(ncid, nvar, xtype, aname)
      iret = nf90_Inquire_Attribute(ncid, nvar, aname, xtype, alen, attnum)
      call check_err(iret)
      !print*,'type=',xtype
      select case (xtype)
            ! NF90_BYTE, NF90_CHAR, N90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE
         case(NF90_BYTE)
            iret = nf_get_att_int(ncid, nvar, aname, ival)
            call check_err(iret)
            write(123,*)'b   ',trim(adjustl(aname))," = ", ival
         case(NF90_CHAR)
            do i = 1,attstrlen
               attstring(i:i) = ' '
            end do
            iret = nf_get_att_text(ncid, nvar, aname, attstring)
            call check_err(iret)
            write(123,"(3A,3x,I10,3x,A)")'c   ',trim(adjustl(aname))," has length = ",len(trim(attstring)),trim(adjustl(attstring))
         case(nf90_short)
            iret = nf_get_att_int(ncid, nvar, aname, ival)
            call check_err(iret)
            write(123,*)'ishort   ',trim(adjustl(aname))," = ", ival
         case(NF90_INT)
            iret = nf_get_att_int(ncid, nvar, aname, ival)
            call check_err(iret)
            write(123,*)'i   ',trim(adjustl(aname))," = ", ival
         case(NF90_FLOAT)
            iret = nf_get_att_real(ncid, nvar, aname, rval)
            call check_err(iret)
            write(123,*)'i   ',trim(adjustl(aname))," = ", rval
         case(NF90_DOUBLE)
            iret = nf_get_att_double (ncid, nvar, aname, dval )
            call check_err(iret)
            write(123,*)'d   ',trim(adjustl(aname))," = ", dval
            case default
            call qerror('netCDF external data typ unkown')
      end select
   end do !k Attribute von Variable nvar
   return
end subroutine print_attributes
!-----------------------------------------------------------------------
subroutine untrim_vtk(nt)
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   integer alloc_status, open_error, errcode
   integer iret,j,k,n, nvar, nt, ion, varid, vxtype, vndims, dlength
   integer, dimension(nf90_max_var_dims) :: dimids
   character(256) :: dname
   character (len = longname) :: dateiname, systemaufruf, zahl, vname
   integer :: start2(2), count2(2)
   integer :: start3(3), count3(3)
   integer i_corn, kantenanzahl2D
   real , allocatable , dimension (:) :: wsp, volume, qarea, velmag
   write(*,*)"untrim_vtk"
   if (nt > 0) then ! there are timesteps
      write(zahl,*)nt
      zahl = adjustl(zahl)
      write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'netcdf_face_',trim(zahl),'.vtk'
      if (errcode /= 0)call qerror('untrim_vtk writing filename netcdf_face failed')
   else ! no timesteps
      return
   end if
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('untrim_vtk writing systemcall rm -rf dateiname failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error untrim*.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'reading SCHISM/untrim2 netCDF'
   write(ion,'(A)')'ASCII'
   !write(ion,'(A)')'DATASET POLYDATA'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   !Variable :   Mesh2_face_x
   !Dimensionen: nMesh2_face       11103
   iret = nf_inq_varid(ncid,'Mesh2_face_x', varid)
   call check_err(iret)
   write(*,*)'Mesh2_face_x: varid = ',varid
   iret = nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids)
   call check_err(iret)
   do k = 1,vndims
      iret = nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength)
      call check_err(iret)
      write(*,*)'   Mesh2_face_x:dim  ', trim(adjustl(dname)), dlength
   end do !k Dimensionen von Variable j
   iret = nf90_Inquire_Dimension(ncid, dimids(1), dname, dlength)
   call check_err(iret)
   knotenanzahl2D = dlength
   allocate (knoten_x(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_x(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   allocate (knoten_y(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_y(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   allocate (knoten_z(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_z(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   allocate (wsp(knotenanzahl2D), stat = alloc_status )
   allocate (volume(knotenanzahl2D), stat = alloc_status )
   iret = nf90_get_var(ncid, varid, knoten_x)
   call check_err(iret)
   iret = nf_inq_varid(ncid,'Mesh2_face_y', varid)
   call check_err(iret)
   iret = nf90_get_var(ncid, varid, knoten_y)
   call check_err(iret)
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
   do n = 1,knotenanzahl2D
      knoten_z(n) = 0.0
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
   end do ! alle Knoten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
   !write(ion,'(A)')'SCALARS Gelaendehoehe float 1'
   !write(ion,'(A)')'LOOKUP_TABLE default'
   !do n=1,knotenanzahl2D
   !   write(ion,'(f27.6)') knoten_z(n)
   !end do ! alle Knoten
   ! Variable : Mesh2_face_Wasserstand_2d
   ! Dimensionen:
   !    nMesh2_face       11103
   !    nMesh2_data_time        8760
   iret = nf_inq_varid(ncid,'Mesh2_face_Wasserstand_2d', varid)
   call check_err(iret)
   !check(nf90_get_var(nc_id,var_id,var_dummy,start=(/1,1,1,ist/),count=(/lo,la,le,1/)))
   start2 = (/ 1, nt /)
   count2 = (/ knotenanzahl2D, 1 /)
   iret = nf90_get_var(ncid, varid, wsp, start2, count2 )
   call check_err(iret)
   write(ion,'(A)')'SCALARS WSP float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,*) wsp(n)
   end do ! alle Knoten
   ! Variable : Mesh2_face_Salzgehalt_2d
   ! Dimensionen:
   !    nMesh2_face       11103
   !    nMesh2_layer_2d           1
   !    nMesh2_data_time        8760
   iret = nf_inq_varid(ncid,'Mesh2_face_Salzgehalt_2d', varid)
   call check_err(iret)
   start3 = (/ 1, 1, nt /)
   count3 = (/ knotenanzahl2D, 1, 1 /)
   iret = nf90_get_var(ncid, varid, knoten_z, start3, count3 )
   call check_err(iret)
   write(ion,'(A)')'SCALARS Salz_PSU float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,*) knoten_z(n)
   end do ! alle Knoten
   iret = nf_inq_varid(ncid,'Mesh2_face_Wasservolumen_2d', varid)
   call check_err(iret)
   start3 = (/ 1, 1, nt /)
   count3 = (/ knotenanzahl2D, 1, 1 /)
   iret = nf90_get_var(ncid, varid, volume, start3, count3 )
   call check_err(iret)
   write(ion,'(A)')'SCALARS Wasservolumen float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,*) volume(n)
   end do ! alle faces(elemente)
   close (ion)
   deallocate (wsp,volume)
   deallocate (knoten_x,knoten_y,knoten_z)
   !------------------------------------------------------------------------------------------ edges
   if (nt > 0) then ! there are timesteps
      write(zahl,*)nt
      zahl = adjustl(zahl)
      write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'netcdf_edge_',trim(zahl),'.vtk'
      if (errcode /= 0)call qerror('untrim_vtk writing filename netcdf_edge_ failed')
   else ! no timesteps
      return
   end if
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('untrim_vtk writing systemcall rm -rf dateiname edges failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error untrim*.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'reading SCHISM/untrim2 netCDF'
   write(ion,'(A)')'ASCII'
   !write(ion,'(A)')'DATASET POLYDATA'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   !Mesh2_edge_x
   iret = nf_inq_varid(ncid,'Mesh2_edge_x', varid)
   call check_err(iret)
   write(*,*)'Mesh2_edge_x: varid = ',varid
   iret = nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids)
   call check_err(iret)
   do k = 1,vndims
      iret = nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength)
      call check_err(iret)
      write(*,*)'   Mesh2_edge_x:dim  ', trim(adjustl(dname)), dlength
   end do !k Dimensionen von Variable j
   iret = nf90_Inquire_Dimension(ncid, dimids(1), dname, dlength)
   call check_err(iret)
   kantenanzahl2D = dlength
   allocate (knoten_x(kantenanzahl2D),knoten_y(kantenanzahl2D),knoten_z(kantenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_x(kantenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   iret = nf90_get_var(ncid, varid, knoten_x)
   call check_err(iret)
   iret = nf_inq_varid(ncid,'Mesh2_edge_y', varid)
   call check_err(iret)
   iret = nf90_get_var(ncid, varid, knoten_y)
   call check_err(iret)
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',kantenanzahl2D, ' float'
   do n = 1,kantenanzahl2D
      knoten_z(n) = 0.0
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
   end do ! alle Knoten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', kantenanzahl2D
   ! Variable : Mesh2_edge_Durchflussflaeche_2d
   ! Dimensionen:
   !    nMesh2_edge       25581
   !    nMesh2_layer_2d           1
   !    nMesh2_data_time        8760
   iret = nf_inq_varid(ncid,'Mesh2_edge_Durchflussflaeche_2d', varid)
   call check_err(iret)
   iret = nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids)
   print *,'Mesh2_edge_Durchflussflaeche_2d: vndims = ',vndims
   call check_err(iret)
   do k = 1,vndims
      iret = nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength)
      call check_err(iret)
      print *,'   Mesh2_edge_Durchflussflaeche_2d::dim  ', trim(adjustl(dname)), dlength
   end do !k Dimensionen von Variable j
   allocate (qarea(kantenanzahl2D), stat = alloc_status )
   allocate (velmag(kantenanzahl2D), stat = alloc_status )
   start3 = (/ 1, 1, nt /)
   count3 = (/ kantenanzahl2D, 1, 1 /)
   iret = nf90_get_var(ncid, varid, qarea, start3, count3 )
   call check_err(iret)
   write(ion,'(A)')'SCALARS Qflaech float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,kantenanzahl2D
      !write(ion,'(f27.6)') p(n)
      write(ion,*) qarea(n)
   end do ! alle Knoten
   iret = nf_inq_varid(ncid,'Mesh2_edge_skalare_Stroemungsgeschwindigkeit_2d', varid)
   call check_err(iret)
   start3 = (/ 1, 1, nt /)
   count3 = (/ kantenanzahl2D, 1, 1 /)
   iret = nf90_get_var(ncid, varid, velmag, start3, count3 )
   call check_err(iret)
   write(ion,'(A)')'SCALARS vbetr float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,kantenanzahl2D
      !write(ion,'(f27.6)') p(n)
      write(ion,*) velmag(n)
   end do ! alle Knoten
   close (ion)
   deallocate (qarea,velmag)
   deallocate (knoten_x,knoten_y,knoten_z)
   return
end subroutine untrim_vtk
!----+-----+----
subroutine netcdf_mesh_only()
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   integer , allocatable , dimension (:,:) :: fa_no, ed_fa
   real minx,maxx,miny,maxy
   integer  ion, alloc_status, open_error, errcode
   integer iret,j,k,n, nvar, varid, vxtype, vndims, dlength
   integer, dimension(nf90_max_var_dims) :: dimids
   character(256) :: dname
   character(len = longname) :: dateiname, systemaufruf, vname
   integer :: start2(2), count2(2)
   integer :: start3(3), count3(3)
   integer i_corn, kantenanzahl2D
   !-------------------------------------------------------------------------------------------- nodes
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'netcdf_mesh_node.vtk'
   if (errcode /= 0)call qerror('netcdf_mesh_only writing filename netcdf_mesh_node.vtk failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('untrim_vtk writing systemcall rm -rf dateiname nodes failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error netcdf_mesh_node.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'mesh_only SCHISM/untrim2 netCDF'
   write(ion,'(A)')'ASCII'
   !write(ion,'(A)')'DATASET POLYDATA'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   iret = nf_inq_varid(ncid,'Mesh2_node_x', varid)
   call check_err(iret)
   write(*,*)'Mesh2_node_x: varid = ',varid
   iret = nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids)
   call check_err(iret)
   do k = 1,vndims
      iret = nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength)
      call check_err(iret)
      write(*,*)'   Mesh2_node_x:dim  ', trim(adjustl(dname)), dlength
   end do !k Dimensionen von Variable j
   iret = nf90_Inquire_Dimension(ncid, dimids(1), dname, dlength)
   call check_err(iret)
   knotenanzahl2D = dlength
   print*,'knotenanzahl2D nodes = ', knotenanzahl2D
   allocate (knoten_x(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_x(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   allocate (knoten_y(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_y(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   allocate (knoten_z(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_z(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   iret = nf90_get_var(ncid, varid, knoten_x)
   call check_err(iret)
   iret = nf_inq_varid(ncid,'Mesh2_node_y', varid)
   call check_err(iret)
   iret = nf90_get_var(ncid, varid, knoten_y)
   call check_err(iret)
   minx = 9999999.9
   maxx = -9999999.9
   miny = 9999999.9
   maxy = -9999999.9
   do n = 1,knotenanzahl2D
      if (knoten_x(n) > maxx)maxx = knoten_x(n)
      if (knoten_y(n) > maxy)maxy = knoten_y(n)
      if (knoten_x(n) < minx)minx = knoten_x(n)
      if (knoten_y(n) < miny)miny = knoten_y(n)
   end do ! alle Knoten
   print*,'netcdf_mesh_only minx, maxx,miny,maxy = ',minx,maxx,miny,maxy
   !do n=1,knotenanzahl2D
   !   knoten_x(n)=knoten_x(n)-minx
   !   knoten_y(n)=knoten_y(n)-miny
   !end do ! alle Knoten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
   do n = 1,knotenanzahl2D
      knoten_z(n) = 0.0
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
   end do ! alle Knoten
   iret = nf_inq_varid(ncid,'Mesh2_face_nodes', varid)
   call check_err(iret)
   iret = nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids)
   call check_err(iret)
   !do k=1,vndims
   iret = nf90_Inquire_Dimension(ncid, dimids(1), dname, dlength)
   call check_err(iret)
   write(*,*)'   Mesh2_face_nodes:dim 1  ', trim(adjustl(dname)), dlength
   iret = nf90_Inquire_Dimension(ncid, dimids(2), dname, dlength)
   call check_err(iret)
   write(*,*)'   Mesh2_face_nodes:dim 2  ', trim(adjustl(dname)), dlength
   n_elemente = dlength
   !end do !k Dimensionen von Variable j
   allocate (fa_no(4,n_elemente), stat = alloc_status )
   allocate (elementnodes(n_elemente,4), stat = alloc_status )
   allocate (cornernumber(n_elemente), stat = alloc_status )
   iret = nf90_get_var(ncid, varid, fa_no)
   call check_err(iret)
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
      end do ! alle Elementecken
   end do ! alle Elemente
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
   end do ! alle Elemente
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'CELL_TYPES ', n_elemente
   do n = 1,n_elemente ! alle Elemente
      if (cornernumber(n) == 4) then
         write(ion,'(A)') '9'
      else
         write(ion,'(A)') '5'
      endif
   end do ! alle Elemente
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
   write(ion,'(A)')'SCALARS zet float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') knoten_z(n)
   end do ! alle Knoten
   close (ion)
   deallocate (knoten_x)
   deallocate (knoten_y)
   deallocate (knoten_z)
   !-------------------------------------------------------------------------------------------- faces=elements
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'netcdf_mesh_element.vtk'
   if (errcode /= 0)call qerror('netcdf_mesh_only writing filename netcdf_mesh_element.vtk failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('netcdf_mesh_only writing system call rm -rf dateiname failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error netcdf_mesh_element.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'mesh_only SCHISM/untrim2 netCDF'
   write(ion,'(A)')'ASCII'
   !write(ion,'(A)')'DATASET POLYDATA'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   !Variable :   Mesh2_face_x
   !Dimensionen: nMesh2_face       11103
   iret = nf_inq_varid(ncid,'Mesh2_face_x', varid)
   call check_err(iret)
   write(*,*)'Mesh2_face_x: varid = ',varid
   iret = nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids)
   call check_err(iret)
   do k = 1,vndims
      iret = nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength)
      call check_err(iret)
      write(*,*)'   Mesh2_face_x:dim  ', trim(adjustl(dname)), dlength
   end do !k Dimensionen von Variable j
   iret = nf90_Inquire_Dimension(ncid, dimids(1), dname, dlength)
   call check_err(iret)
   knotenanzahl2D = dlength
   print*,'knotenanzahl2D faces = ', knotenanzahl2D
   allocate (knoten_x(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_x(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   allocate (knoten_y(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_y(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   allocate (knoten_z(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate knoten_z(knotenanzahl2D) :', alloc_status
      call qerror(fehler)
   end if
   iret = nf90_get_var(ncid, varid, knoten_x)
   call check_err(iret)
   iret = nf_inq_varid(ncid,'Mesh2_face_y', varid)
   call check_err(iret)
   iret = nf90_get_var(ncid, varid, knoten_y)
   call check_err(iret)
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
   do n = 1,knotenanzahl2D
      knoten_z(n) = 0.0
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
   end do ! alle Knoten
   iret = nf_inq_varid(ncid,'Mesh2_edge_faces', varid)
   call check_err(iret)
   print *,'Mesh2_edge_faces: varid = ',varid
   iret = nf90_inquire_variable(ncid,varid,vname,vxtype,vndims,dimids)
   print *,'Mesh2_edge_faces: vndims = ',vndims
   call check_err(iret)
   do k = 1,vndims
      iret = nf90_Inquire_Dimension(ncid, dimids(k), dname, dlength)
      call check_err(iret)
      print *,'   Mesh2_edge_faces::dim  ', trim(adjustl(dname)), dlength
   end do !k Dimensionen von Variable j
   iret = nf90_Inquire_Dimension(ncid, dimids(2), dname, dlength)
   call check_err(iret)
   kantenanzahl2D = dlength
   allocate (ed_fa(2,kantenanzahl2D), stat = alloc_status )
   iret = nf90_get_var(ncid, varid, ed_fa)
   call check_err(iret)
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl2D, 3*kantenanzahl2D
   do n = 1,kantenanzahl2D
      if (ed_fa(2,n) <= 0) ed_fa(2,n) = ed_fa(1,n)
      if (ed_fa(1,n) <= 0) ed_fa(1,n) = ed_fa(2,n)
      write(ion,'(A,2x,I8,2x,I8)')'2', ed_fa(1,n), ed_fa(2,n)
   end do ! alle Knoten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl2D
   do n = 1,kantenanzahl2D
      write(ion,'(A)')'3'
   end do ! alle Knoten
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
   end do ! alle Knoten
   close (ion)
   deallocate (knoten_x)
   deallocate (knoten_y)
   deallocate (knoten_z)
   !-------------------------------------------------------------------------------------------- edges=Kanten
   !   int Mesh2_edge_nodes(nMesh2_edge, two) ;
   !      Mesh2_edge_nodes:long_name = "Knotenverzeichnis der Kanten, Anfangs- und Endpunkt" ;
   !      Mesh2_edge_nodes:cf_role = "edge_node_connectivity" ;
   !      Mesh2_edge_nodes:start_index = 0 ;
   print*,'netcdf_mesh_only under development'
   return
end subroutine netcdf_mesh_only
!----+-----+----
