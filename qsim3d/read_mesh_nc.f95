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

subroutine read_mesh_nc()
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   integer :: iret, ndims, nVars, nGlobalAtts, unlimdimid, nAtts, ndumm, errcode
   integer , allocatable , dimension (:) :: dlength, vxtype, vndims, nbc
   integer, dimension(nf90_max_var_dims) :: dimids
   character(256) , allocatable , dimension (:) :: dname, vname
   integer j,k,n, didi, alloc_status, open_error, nele_links, nele_rechts, anzahl_randkanten
   integer , allocatable , dimension (:,:) :: fa_no, ed_fa
   character(256) :: aname
   character (len = longname) :: dateiname
   real , allocatable , dimension (:) :: zeiten
   real minx,maxx,miny,maxy, nach_links
   logical nixlinks, nixrechts, singlenodes
   
   write(*,*)'read_mesh_nc() started'
   open ( unit = 123 , file = 'netcdf.log', status = 'replace', action = 'write', iostat = open_error )
   write(dateiname,'(2A)',iostat = errcode)trim(modellverzeichnis),'transport.nc'
   if (errcode /= 0)call qerror('read_mesh_nc writing filename transport.nc failed')
   iret = nf_open(dateiname, NF_NOWRITE, ncid)
   call check_err(iret)
   write(*,*)'transport.nc ließ sich öffnen'
   !!----------------------------------------------------------------------  Überblick
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
   !write(123,*)'NF_INT=', NF_INT," NF_FLOAT=", NF_FLOAT," NF_DOUBLE=", NF_DOUBLE
   do j = 1,nVars
      !iret = nf90_inquire_variable(ncid,j,vname(j),xtype(j),ndims(j),dimids, nAtts)
      iret = nf90_inquire_variable(ncid,j,vname(j),vxtype(j),vndims(j),dimids, nAtts)
      call check_err(iret)
      if (vxtype(j) == NF_DOUBLE) write(123,*)'NF_DOUBLE '
      if (vxtype(j) == NF_FLOAT) write(123,*)'NF_FLOAT '
      if (vxtype(j) == NF_INT) write(123,*)'NF_INT '
      write(123,*)"Variable named :",trim(adjustl(vname(j)))
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
   close (123)
   !----------------------------------------------------------------------  Dimensionen
   ! Dimension  nMesh2_node wert=       14464
   ! Dimension  nMesh2_edge wert=       25581
   ! Dimension  nMesh2_face wert=       11103
   ! Dimension  nMesh2_data_time wert=        8760
   call check_err( nf90_inq_dimid(ncid, "nMesh2_node", didi) )
   call check_err( nf90_Inquire_Dimension(ncid, didi, aname, knotenanzahl2D) )
   print*,"read_mesh_nc: knotenanzahl2D = ",knotenanzahl2D," - ",aname
   call check_err( nf90_inq_dimid(ncid, "nMesh2_edge", didi) )
   call check_err( nf90_Inquire_Dimension(ncid, didi, aname, kantenanzahl) )
   kanten_vorhanden = .true.
   print*,"read_mesh_nc: kantenanzahl = ",kantenanzahl," - ",aname
   call check_err( nf90_inq_dimid(ncid, "nMesh2_face", didi) )
   call check_err( nf90_Inquire_Dimension(ncid, didi, aname, n_elemente) )
   element_vorhanden = .true.
   print*,"read_mesh_nc: n_elemente = ",n_elemente," - ",aname
   call check_err( nf90_inq_dimid(ncid, "nMesh2_data_time", didi) )
   call check_err( nf90_Inquire_Dimension(ncid, didi, aname, transinfo_anzahl) )
   print*,"read_mesh_nc: anzahl möglicher Transportzeitschritte = ",transinfo_anzahl," - ",aname
   !----------------------------------------------------------------------  nodes
   allocate (knoten_x(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (knoten_x failed')
   call check_err(  nf90_inq_varid(ncid,'Mesh2_node_x', didi) )
   call check_err(  nf90_get_var(ncid, didi, knoten_x) )
   allocate (knoten_y(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (knoten_y failed')
   call check_err(  nf90_inq_varid(ncid,'Mesh2_node_y', didi) )
   call check_err(  nf90_get_var(ncid, didi, knoten_y) )
   allocate (knoten_z(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (knoten_z failed')
   knoten_z(1:knotenanzahl2D) = 0.0
   !allocate (knoten_rand(knotenanzahl2D), stat = alloc_status )
   !if(alloc_status.ne.0) call qerror('allocate (knoten_rand( failed')
   !call check_err(  nf_inq_varid(ncid,'', didi) )
   !call check_err(  nf90_get_var(ncid, didi,  )
   allocate (knoten_zone(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (knoten_zone( failed')
   do n = 1,knotenanzahl2D !initialize zones
      knoten_zone(n) = 0
   end do ! alle Knoten
   !call check_err(  nf_inq_varid(ncid,'', didi) )
   !call check_err(  nf90_get_var(ncid, didi,  )
   !allocate (knoten_flaeche(knotenanzahl2D), stat = alloc_status )
   !if(alloc_status.ne.0) call qerror('allocate (knoten_flaeche failed')
   !call check_err(  nf_inq_varid(ncid,'', didi) )
   !call check_err(  nf90_get_var(ncid, didi,  )
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
   print*,'read_mesh_nc: minx, maxx,miny,maxy = ',minx,maxx,miny,maxy
   !do n=1,knotenanzahl2D
   !   knoten_x(n)=knoten_x(n)-minx
   !   knoten_y(n)=knoten_y(n)-miny
   !end do ! alle Knoten
   !----------------------------------------------------------------------  elements/faces
   allocate (element_x(n_elemente),element_y(n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate element_xy failed')
   call check_err(  nf90_inq_varid(ncid,'Mesh2_face_x', didi) )
   call check_err(  nf90_get_var(ncid, didi, element_x) )
   call check_err(  nf90_inq_varid(ncid,'Mesh2_face_y', didi) )
   call check_err(  nf90_get_var(ncid, didi, element_y) )
   allocate (fa_no(4,n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate fa_no( failed')
   allocate (elementnodes(n_elemente,4), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (elementnodes( failed')
   allocate (cornernumber(n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (cornernumber( failed')
   call check_err( nf_inq_varid(ncid,'Mesh2_face_nodes', didi) )
   call check_err( nf90_get_var(ncid, didi, fa_no) )
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
      end do ! alle Elementecken
      summ_ne = summ_ne+cornernumber(n)+1
   end do ! alle Elemente
   allocate (knot_ele(knotenanzahl2D), stat = alloc_status ) ! number of elements at node
   if (alloc_status /= 0) call qerror('allocate (knot_ele(knotenanzahl2D) failed')
   do n = 1,knotenanzahl2D
      knot_ele(n) = 0
   end do ! alle Knoten
   do j = 1,n_elemente ! alle Elemente
      do k = 1,cornernumber(j)
         knot_ele(elementnodes(j,k)) = knot_ele(elementnodes(j,k))+1
      end do ! alle Element-ecken
   end do ! alle Elemente
   singlenodes = .false.
   do n = 1,knotenanzahl2D
      if (knot_ele(n) == 0) then
         print*,'no elements at node',n
         singlenodes = .true.
      end if
   end do ! alle Knoten
   if (singlenodes) call qerror('read_mesh_nc: nodes belonging to no element in mesh')
   allocate (element_rand(n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (element_rand failed')
   call check_err( nf_inq_varid(ncid,'Mesh2_face_bc', didi) )
   call check_err( nf90_get_var(ncid, didi, element_rand) )
   allocate (element_zone(n_elemente), stat = alloc_status )
   if (alloc_status /= 0) then
      call qerror('allocate (element_zone failed')
   else
      print*,' allocate (element_zone(n_elemente) worked read_mesh_nc',meinrang,n_elemente
   endif
   do n = 1,n_elemente ! alle Elemente
      element_zone(n) = 0
   end do ! alle Elemente
   ! print*,'nach lesen von untrim netcdf-Datei alle Elemente zunächst in zone 0'
   allocate (elementedges(n_elemente,4), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (elementedges failed')
   elementedges = 0
   call check_err( nf_inq_varid(ncid,'Mesh2_face_edges', didi) )
   call check_err( nf90_get_var(ncid, didi, fa_no) )
   do n = 1,n_elemente ! alle Elemente
      do k = 1,cornernumber(n)
         elementedges(n,k) = fa_no(k,n)+1
         if ((elementedges(n,k) < 1) .or. (elementedges(n,k) > kantenanzahl)) then
            write(fehler,*)'netcdf_mesh_only elementedges(n,k) falsch:',elementedges(n,k),n,k
            call qerror(fehler)
         endif
      end do ! alle Elementecken
   end do ! alle Elemente
   deallocate (fa_no, stat = alloc_status )
   !----------------------------------------------------------------------  edges,Kanten
   kanten_vorhanden = .true.
   !allocate (edge_x(kantenanzahl), stat = alloc_status )
   !if(alloc_status.ne.0) call qerror('allocate (edge_x failed')
   !call check_err(  nf90_inq_varid(ncid,'Mesh2_edge_x', didi) )
   !call check_err(  nf90_get_var(ncid, didi, edge_x) )
   !allocate (edge_y(kantenanzahl), stat = alloc_status )
   !if(alloc_status.ne.0) call qerror('allocate (edge_y failed')
   !call check_err(  nf90_inq_varid(ncid,'Mesh2_edge_y', didi) )
   !call check_err(  nf90_get_var(ncid, didi, edge_y) )
   allocate (top_node(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror(' allocate (top_node failed')
   allocate (bottom_node(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (bottom_node failed')
   allocate (ed_fa(2,kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (ed_fa(2 failed')
   call check_err(  nf90_inq_varid(ncid,'Mesh2_edge_nodes', didi) )
   call check_err(  nf90_get_var(ncid, didi, ed_fa) )
   do n = 1,kantenanzahl ! alle Kanten
      bottom_node(n) = ed_fa(1,n)+1
      if ((bottom_node(n) < 1) .or. (bottom_node(n) > knotenanzahl2D))call qerror("read_mesh_nc:bottom_node falsch")
      top_node(n) = ed_fa(2,n)+1
      if ((top_node(n) < 1) .or. (top_node(n) > knotenanzahl2D))call qerror("read_mesh_nc:top_node falsch")
   end do ! alle n Kanten
   allocate (edge_normal_x(kantenanzahl),edge_normal_y(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (edge_normal failed')
   do n = 1,kantenanzahl ! alle Kanten
      ! Normalenvektor von Kantenlänge nach links
      edge_normal_x(n) = knoten_y(bottom_node(n)) - knoten_y(top_node(n))
      edge_normal_y(n) = knoten_x(top_node(n))    - knoten_x(bottom_node(n))
   end do ! alle n Kanten
   allocate (left_element(kantenanzahl),right_element(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (left,right_element  failed')
   call check_err(  nf90_inq_varid(ncid,'Mesh2_edge_faces', didi) )
   call check_err(  nf90_get_var(ncid, didi, ed_fa) )
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
         end if ! links auf der linken seite?
      else ! left_element(n) nicht vorhanden
         nach_links = edge_normal_x(n) * (element_x(nele_rechts) - knoten_x(bottom_node(n))) +  &
                      edge_normal_y(n) * (element_y(nele_rechts) - knoten_y(bottom_node(n)))
         if (nach_links >= 0.0) then ! rechts auf der linken seite?
            left_element(n) = nele_rechts
         else !rechts ist rechts
            right_element(n) = nele_rechts
         end if ! rechts auf der linken seite?
      end if ! left_element(n) vorhanden ???
   end do ! alle n Kanten
   deallocate (ed_fa, stat = alloc_status )
   print*,'read_mesh_nc: von ',kantenanzahl,' Kanten, sind ',anzahl_randkanten,' Randkanten.'
   !     Mesh2_edge_bc:flag_meanings = "none closed dirichlet" ;
   !     Mesh2_edge_bc:flag_values = 0, 1, 2 ;
   allocate (nbc(kantenanzahl), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (nbc( failed')
   call check_err(  nf90_inq_varid(ncid,'Mesh2_edge_bc', didi) )
   call check_err(  nf90_get_var(ncid, didi, nbc ) )
   do n = 1,kantenanzahl ! alle Kanten
      if (boundary_number(n) == 0) then ! Kante mit zwei Elementen links und rechts
         if (nbc(n) /= 0)call qerror("Randkantenfehler 00")
      else ! Kante mit nur einem Element
         if (nbc(n) == 0)call qerror("Randkantenfehler 11")
         boundary_number(n) = nbc(n)
      end if ! Kante mit zwei Elementen links und rechts
   end do ! alle n Kanten
   deallocate (nbc, stat = alloc_status )
   allocate (intereck(4*n_elemente), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (intereck(4  failed')
   !Nachbarelemente ermitteln und in intereck speichern
   do n = 1,n_elemente ! alle Elemente
      do k = 1,4
         intereck((n-1)*4+k) = 0
      end do
      do k = 1,cornernumber(n)
         ! Nachbarelemente
         if ( left_element(elementedges(n,k)) == n) intereck((n-1)*4+k) = right_element(elementedges(n,k))
         if (right_element(elementedges(n,k)) == n) intereck((n-1)*4+k) = left_element(elementedges(n,k))
      end do ! alle k Kanten im Element
   end do ! alle n Elemente
   !----------------------------------------------------------------------  Ränder,boundaries
   ! Die Variablen sind bei Untrim-Antrieb an den Elementen definiert, daher müssen doert auch die Randbedingungen angebracht werden.
   ! Zuflussränder sind aber nur an den Kanten erkennbar, daher müssen sie hier jetzt in element_rand eingearbeitet werden:
   do n = 1,n_elemente ! alle Elemente
      ndumm = element_rand(n)
      element_rand(n) = 0
      do k = 1,cornernumber(n)
         ! Element an Randkante(Durchflussrand)
         if (boundary_number(elementedges(n,k)) >= 2) then !! nur Zuflussrand aus Kanten in Elemente übernehmen !!
            ! bei mehreren Randkanten am Element, element_rand auf die größte Kanten-Randnummer setzen
            if (boundary_number(elementedges(n,k)) > element_rand(n)) then ! winner takes it all
               element_rand(n) = boundary_number(elementedges(n,k))
            end if ! winner
         end if ! Zufluss-Randkante
      end do ! alle k Kanten im Element
      if (ndumm > 0) then ! Element hatte schon Randnummer
         if (ndumm /= element_rand(n)) then ! Randunummernkonflikt vermeiden
            element_rand(n) = ndumm !! in der Regel ist das ein Wasserstandsrand
         else
            print*,'Randunummernkonflikt read_mesh_nc in Element #',n   &
            ,' element_rand# aus edges = ',element_rand(n),'element_rand# aus faces = ',ndumm
            call qerror('Randunummernkonflikt read_mesh_nc')
         end if !Randunummernkonflikt
      end if ! Element hatte schon Randnummer
   end do ! alle n Elemente
   return
end subroutine read_mesh_nc
