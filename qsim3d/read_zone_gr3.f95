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

!> get zones and boundaries from zone.gr3,
!! created by Janet like manning.gr3
logical function read_zone_gr3(minzoni,maxzoni)
      use modell
      use schism_glbl
      implicit none
      integer                           :: i,j,k,n,m,maxzoni,minzoni,istat,alloc_status,iegb
      real                              :: x,y,zoni
      character (len = longname)        :: dateiname

      if (meinrang /= 0) call qerror('read_zone_gr3 on process 0 only')
      
      write(dateiname,"(2A)")trim(modellverzeichnis),"outputs_schism/zone.gr3"
      open(14,file = trim(dateiname),status = 'old',iostat = istat)
      if (istat /= 0) call qerror('read_mesh_nc_sc: zone.gr3 open failure')
      read(14,*); read(14,*) n,k
      if(n.ne.n_elemente)call qerror('zone.gr3 element number missmatch')
      if(k.ne.knotenanzahl2D)call qerror('zone.gr3 node number missmatch')
      print*,'read_zone_gr3: zone.gr3: n_elemente,knotenanzahl2D = ',n,k
!         allocate (knoten_x(knotenanzahl2D),knoten_y(knotenanzahl2D),knoten_z(knotenanzahl2D),   &
!               knoten_zone(knotenanzahl2D),knoten_rang(knotenanzahl2D),    &
!               knoten_rand(knotenanzahl2D),knoten_flaeche(knotenanzahl2D), stat = istat )
      maxzoni=-777 ; minzoni=777
      do i = 1,knotenanzahl2D
         read(14,*)n,x,y,zoni
         if (n /= i) call qerror('reading zone.gr3 nodes: something gone wrong')
         knoten_zone(n)=nint(zoni)
         if(maxzoni.lt.knoten_zone(n))maxzoni=knoten_zone(n)
         if(minzoni.gt.knoten_zone(n))minzoni=knoten_zone(n)
      enddo ! all i nodes
      !print*,'zone.gr3 Zonen-Nummern von ',minzoni,' bis ',maxzoni
      
      do i=1,n_elemente
         read(14,*) iegb,cornernumber(iegb),(elementnodes(iegb,k),k=1,cornernumber(iegb))
         if(cornernumber(iegb)/=3 .and. cornernumber(iegb)/=4) then
            write(errmsg,*) 'zone.gr3 : Unknown type of element',iegb,cornernumber(iegb)
            call qerror(errmsg)
         endif
      enddo

      element_zone(:)=-1
      do i = 1,n_elemente
         do j = 1,cornernumber(i)
            ! largest node-zone-number applied to Element
            if(element_zone(i).le.knoten_zone(elementnodes(i,j)))   &
               element_zone(i) =  knoten_zone(elementnodes(i,j))
         enddo ! all j nodes of element
      enddo ! all i elements
   
      read_zone_gr3=.true.
      return
end function read_zone_gr3