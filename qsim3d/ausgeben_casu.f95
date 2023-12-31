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

!> Ausgabe in Dateien, die von Paraview dargestellt werden können ??\n
!! zunächst Visualisierung der Ergebnisse mit Paraview \n
!! (casu: Ausgabe der Datei output.vtk mittels Aufruf des Programms 
!! >out08 [modell], paraview starten mit >para) \n\n
!! aus: ausgeben_casu.f95 ; zurück: \ref lnk_ergebnisausgabe
subroutine ausgeben_casu()
   use modell
   implicit none
   
   character(longname) :: filename, systemaufruf, zahl
   integer             :: i,j,n, open_error, ion, system_error, string_read_error, alloc_status
   integer             :: sysa, errcode
   real                :: t, nue_num, nue_elder, reibgesch, sandrauh, wati, summwicht
   real                :: ubetr, infl, aus, nx, ny,nz, relnumdiff, tr,al
   
   if (meinrang /= 0)call qerror('ausgeben_casu sollte eigentlich nur von Prozessor 0 aufgerufen werden')
   if (knotenanzahl2D /= number_plankt_point) then
      write(fehler,*)'knotenanzahl2D /= number_plankt_point ',knotenanzahl2D, number_plankt_point
      call qerror(fehler)
   endif
   if (knotenanzahl2D /= number_trans_quant_points) then
      write(fehler,*)'knotenanzahl2D /= number_trans_quant_points'
      call qerror(fehler)
   endif
   if (knotenanzahl2D /= number_benthic_points) then
      write(fehler,*)'knotenanzahl2D /= number_benthic_points ... full 3D output not yet implemented'
      call qerror(fehler)
   endif
   
   write(zahl,*)rechenzeit
   zahl = adjustl(zahl)
   
   !-------------------------------------
   if (bali) then !! kontrollausgabe der Bahnlinien
      print*,'Ausgabe bahnlinien/stromstriche'
      filename = trim(modellverzeichnis) // 'bahnlinien_' // trim(zahl) // '.vtk'
      write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(filename)
      
      open(newunit = ion , file = filename, status = 'replace', action = 'write', iostat = open_error)
      if (open_error /= 0) call qerror("Could not open " // trim(filename))
      
      write(ion,'(A)')'# vtk DataFile Version 3.0'
      write(ion,'(A)')'Simulation QSim3D casu'
      write(ion,'(A)')'ASCII'
      !write(ion,'(A)')'DATASET POLYDATA'
      write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
      
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D*2, ' float'
      do n = 1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
      enddo ! alle Knoten
      do n = 1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') ur_x(n), ur_y(n), ur_z(n)
      enddo ! alle Knoten
      ! Punkte als vtk-vertices
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', knotenanzahl2D, 3*knotenanzahl2D
      do n = 1,knotenanzahl2D
         write(ion,'(A,2x,I8,2x,I8)')'2', n-1, knotenanzahl2D+(n-1)
      enddo ! alle Knoten
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'CELL_TYPES ', knotenanzahl2D
      do n = 1,knotenanzahl2D
         write(ion,'(A)')'3'
      enddo ! alle Knoten
      
      !write(ion,'(A)')' '
      !write(ion,'(A,2x,I12)')'POINT_DATA ',knotenanzahl2D*2
      
      close(ion)
   endif ! bali
   
   
   !-------------------------------------
   print*,"Ausgabe Knotenpunkte"
   filename = trim(modellverzeichnis) // 'ausgabe_' // trim(zahl) // '.vtk'
   open(newunit = ion , file = filename, status = 'unknown', action = 'write ', iostat = open_error)
   if (open_error /= 0) call qerror("Could not open " // trim(filename))
   
   if (knotenanzahl2D /= number_benthic_points) call qerror('3D noch nicht vorgesehen hier')
   if (number_plankt_point /= knotenanzahl2D)   call qerror('number_plankt_point und knotenanzahl2D passen nicht zusammen')
   
   call mesh_output(ion)
   write(ion,'(A)')'SCALARS WSP float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') rb_hydraul(3+(n-1)*number_rb_hydraul)
   enddo
   
   write(ion,'(A)')'SCALARS tief float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') rb_hydraul(2+(n-1)*number_rb_hydraul)
   enddo 
   
   write(ion,'(A)')'SCALARS summwicht float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      summwicht = 0.0
      do j = 1,4
         summwicht = summwicht + wicht((n-1)*4+j)
      enddo ! alle Ecken im Herkunftselement der Bahnlinie
      write(ion,'(f27.6)') summwicht
   enddo ! alle Knoten
   write(ion,'(A)')'SCALARS Rang float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      !write(ion,'(f27.6)') tief(n)
      write(ion,'(f27.6)') real(n/part)
   enddo ! alle Knoten
   
   write(ion,'(A)')'SCALARS flaeche float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      !write(ion,'(f27.6)') tief(n)
      write(ion,'(f27.6)') knoten_flaeche(n)
   enddo ! alle Knoten
   write(ion,'(A)')'VECTORS u float'
   
   !write(ion,'(A)')'LOOKUP_TABLE default' ! nicht bei Vektoren ??
   do n = 1,knotenanzahl2D
      nx = u(n)*cos(dir(n))
      ny = u(n)*sin(dir(n))
      nz = 0.0
      write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') nx, ny, nz
   enddo ! alle Knoten
   write(ion,'(A)')'SCALARS Geschwindigkeitsbetrag float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      !write(ion,'(f27.6)') u(n)
      write(ion,'(f27.6)') rb_hydraul(1+(n-1)*number_rb_hydraul)
   enddo ! alle Knoten
   write(ion,'(A)')'SCALARS rau float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(6x,f27.6)') zone(point_zone(n))%reib
   enddo
   write(ion,'(A)')'SCALARS strickler float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(6x,f27.6)') strickler( zone(point_zone(n))%reib , rb_hydraul(2+(n-1)*number_rb_hydraul) )
   enddo
   !write(ion,'(A)')'SCALARS Utau float 1'
   !write(ion,'(A)')'LOOKUP_TABLE default'
   !do n=1,knotenanzahl2D
   !   write(ion,'(f27.6)') rb_hydraul(4+(n-1)*number_rb_hydraul)
   !enddo ! alle Knoten
   !write(ion,'(A)')'SCALARS Ks float 1'
   !write(ion,'(A)')'LOOKUP_TABLE default'
   !do n=1,knotenanzahl2D
   !   write(ion,'(f27.6)') reib_ks(knoten_zone(n))
   !enddo ! alle Knoten
   !write(ion,'(A)')'SCALARS Kst float 1'
   !write(ion,'(A)')'LOOKUP_TABLE default'
   !do n=1,knotenanzahl2D
   !   sandrauh=reib_ks(knoten_zone(n))
   !   wati=rb_hydraul(2+(n-1)*number_rb_hydraul)
   !   write(ion,'(f27.6)') strickler(sandrauh,wati)
   !enddo ! alle Knoten
   write(ion,'(A)')'SCALARS numDiff.rel float 1'
   !write(ion,'(A)')'SCALARS nue_elder float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      relnumdiff = 10000.0
      nue_num = 0.25*(knoten_flaeche(n)/real(deltat))
      reibgesch = benthic_distribution(45+(n-1)*number_benth_distr)
      wati = rb_hydraul(2+(n-1)*number_rb_hydraul)
      nue_elder = 5.93*reibgesch*wati
      if (nue_elder > 0.0) relnumdiff = nue_num/nue_elder
      write(ion,'(6x,f27.6)')relnumdiff
      !write(ion,'(6x,f27.6)')nue_elder
   enddo ! alle Knoten
   write(ion,'(A)')'SCALARS Randnummer float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      if ((knoten_rand(n) >= 1) .and. (knoten_rand(n) <= ianz_rb)) then
         write(ion,'(6x,f27.6)') real( rabe(knoten_rand(n))%nr_rb )
      else
         write(ion,'(6x,f27.6)') real( knoten_rand(n) )
      endif
   enddo ! alle Knoten
   !write(ion,'(A)')'SCALARS inflow float 1'
   !write(ion,'(A)')'LOOKUP_TABLE default'
   !do n=1,knotenanzahl2D
   !   infl=0.0
   !   if(inflow(n))infl=1.0
   !   write(ion,'(f27.6)') infl
   !enddo ! alle Knoten
   if (nur_alter) then
      ! Altersberechnung nach Shen&Wang 2007
      write(ion,'(A)')'SCALARS age_arith float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,number_plankt_point
         tr = planktonic_variable(71+(n-1)*number_plankt_vari)
         al = planktonic_variable(74+(n-1)*number_plankt_vari)
         aus = 0.0
         if (tr > minimum_age_tracer ) aus = al / tr
         write(ion,'(6x,f27.6)') aus
      enddo ! alle Knoten
      ! Aufenthaltszeit in Tagen
      write(ion,'(A)')'SCALARS Tage_Aufenthalt float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,knotenanzahl2D
         tr = planktonic_variable(71+(n-1)*number_plankt_vari)
         al = planktonic_variable(73+(n-1)*number_plankt_vari)
         aus = 0.0
         if (tr > minimum_age_tracer ) aus = al / tr
         write(ion,'(6x,f27.6)') aus
      enddo ! alle Knoten
   endif !nur_alter
   ! planktische, transportierte Konzentrationen entsprechend ausgabeflag
   do j = 1,number_plankt_vari ! alle tiefengemittelten
      if (output_plankt(j)) then ! zur ausgabe vorgesehen
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(planktonic_variable_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D ! alle Knoten
            aus = planktonic_variable(j+(n-1)*number_plankt_vari)
            !if (tief(n).le.0.05)aus=-999.999
            write(ion,'(f27.6)') aus
         enddo ! alle Knoten
      endif ! zur ausgabe vorgesehen
   enddo ! alle planktonic_variable
   do j = 1,number_plankt_vari_vert ! alle tiefenaufgelösten z.Z. nur level 1
      if (output_plankt_vert(j)) then ! zur ausgabe vorgesehen
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(plankt_vari_vert_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D ! alle Knoten
            aus = plankt_vari_vert(1+(j-1)*num_lev+(n-1)*number_plankt_vari_vert*num_lev)
            write(ion,'(f27.6)') aus
         enddo ! alle Knoten
      endif ! zur ausgabe vorgesehen
   enddo ! done all plankt_vari_vert
   ! Übergabe_Konzentrationen entsprechend ausgabeflag
   do j = 1,number_trans_quant ! alle tiefengemittelten
      if (output_trans_quant(j)) then ! zur ausgabe vorgesehen
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(trans_quant_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D
            write(ion,'(f27.6)') transfer_quantity(j+(n-1)*number_trans_quant)
         enddo ! alle Knoten
      endif ! zur ausgabe vorgesehen
   enddo ! alle transkon
   do j = 1,number_trans_quant_vert ! alle tiefenaufgelösten z.Z. nur level 1
      if (output_trans_quant_vert(j)) then ! zur ausgabe vorgesehen
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(trans_quant_vert_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D
            aus = trans_quant_vert(1+(j-1)*num_lev_trans+(n-1)*num_lev_trans*number_trans_quant_vert)
            write(ion,'(f27.6)') aus
         enddo ! alle Knoten
      endif ! zur ausgabe vorgesehen
   enddo ! done all vertically distributed transfer quantities
   ! benthic distributions according to output flag
   do j = 1,number_benth_distr ! all benthic distributions
      if (output_benth_distr(j)) then ! flagged?
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(benth_distr_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,number_benthic_points ! all nodes
            write(ion,'(f27.6)') benthic_distribution(j+(n-1)*number_benth_distr)
         enddo ! all nodes
      endif ! flagged
   enddo ! all benthic distributions
   !>
   !!
   ! Benthische_verteilungen entsprechend ausgabeflag
   !write(ion,'(A)')'SCALARS Temperatur_Sediment float 1'
   !write(ion,'(A)')'LOOKUP_TABLE default'
   !do n=1,knotenanzahl2D
   !   write(ion,'(f27.6)') benthische_verteilung(1,n)
   !enddo
   ! write(ion,'(A)')'VECTORS normaltop float'
   ! !write(ion,'(A)')'LOOKUP_TABLE default' ! nicht bei Vektoren ??
   ! do n=1,knotenanzahl2D
   !    nx=0.0
   !    ny=0.0
   !    nz=0.0
   !    if((knoten_rand(n).gt.0).and.(knoten_rand(n).le.ianz_rb)) then
   !       do i=1,rabe(knoten_rand(n))%randlinie%anzkanten ! alle i Kanten an diesem Rand
   !       if(rabe(knoten_rand(n))%randlinie%kante(i)%top .eq. n)then
   !          nx=rabe(knoten_rand(n))%randlinie%kante(i)%normal_x
   !          ny=rabe(knoten_rand(n))%randlinie%kante(i)%normal_y
   !       endif !top
   !       enddo ! alle i Kanten
   !    endif ! randknoten
   !    write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') nx, ny, nz
   ! enddo ! alle Knoten
   ! write(ion,'(A)')'VECTORS normalbottom float'
   ! !write(ion,'(A)')'LOOKUP_TABLE default' ! nicht bei Vektoren ??
   ! do n=1,knotenanzahl2D
   !    nx=0.0
   !    ny=0.0
   !    nz=0.0
   !    if((knoten_rand(n).gt.0).and.(knoten_rand(n).le.ianz_rb)) then
   !       do i=1,rabe(knoten_rand(n))%randlinie%anzkanten ! alle i Kanten an diesem Rand
   !       if(rabe(knoten_rand(n))%randlinie%kante(i)%bottom .eq. n)then
   !          nx=rabe(knoten_rand(n))%randlinie%kante(i)%normal_x
   !          ny=rabe(knoten_rand(n))%randlinie%kante(i)%normal_y
   !       endif !bottom
   !       enddo ! alle i Kanten
   !    endif ! randknoten
   !    write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') nx, ny, nz
   ! enddo ! alle Knoten
   ! write(ion,'(A)')'SCALARS tief_diff float 1'
   ! write(ion,'(A)')'LOOKUP_TABLE default'
   ! do n=1,knotenanzahl2D
   !    aus=p(n)-knoten_z(n)
   !    write(ion,'(f27.6)') aus
   ! enddo ! alle Knoten
   !! Kontrollwerte wg. Zuflussrandermittlung:
   !goto 777 ! z. Z. nicht ausgeben:
   !write(ion,'(A)')'SCALARS ubetr float 1'
   !write(ion,'(A)')'LOOKUP_TABLE default'
   !do n=1,knotenanzahl2D
   !   ubetr=(((knoten_x(n)-ur_x(n))**2 + (knoten_y(n)-ur_y(n))**2 + (knoten_z(n)-ur_z(n))**2)**0.5)/dttrans
   !   write(ion,'(f27.6)') ubetr
   !enddo ! alle Knoten
   !777  continue
   close (ion)
   ! Visualisierung anwerfen ...
   ! write(systemaufruf,*)trim(modellverzeichnis),'visu/visu ',trim(modellverzeichnis),'  ', trim(zahl)
   ! call system(systemaufruf,sysa)
   ! if(sysa.ne.0) then
   !    print*,'Aufruf von visu fehlgeschalgen'
   !    ! call qerror(fehler) 7
   ! endif ! systemaufruf fehlgeschlagen
end subroutine ausgeben_casu
