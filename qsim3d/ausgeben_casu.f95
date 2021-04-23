
!> Ausgabe in Dateien, die von Paraview dargestellt werden können ??\n
!! zunächst Visualisierung der Ergebnisse mit Paraview\n 
!! (casu: Ausgabe der Datei output.vtk mittels Aufruf des Programms >out08 [modell], paraview starten mit >para) 
!! \n\n
!! aus: ausgeben_casu.f95 ; zurück: \ref Ergebnisse
      SUBROUTINE ausgeben_casu()
      use modell                                                   
      implicit none
      character(len=longname) :: dateiname, systemaufruf, zahl
      integer :: i,j,n, open_error, ion, system_error, string_read_error, alloc_status
      integer :: sysa, errcode
      real :: t, nue_num, nue_elder, reibgesch, sandrauh, wati, summwicht
      real :: ubetr, infl, aus, nx, ny,nz, relnumdiff, tr,al,aufenthaltszeit

      !bali=.false. !! z. Z. Keine bahnlinien_ausgabe
      !bali=.true.  jetzt in jetzt_ausgeben !! bahnlinien_ausgabe bitte

      if(meinrang.ne.0)call qerror('ausgeben_casu sollte eigentlich nur von Prozessor 0 aufgerufen werden')

      if(knotenanzahl2D.ne.number_plankt_point)then
         write(fehler,*)'knotenanzahl2D.ne.number_plankt_point ',knotenanzahl2D, number_plankt_point
         call qerror(fehler)
      end if
      if(knotenanzahl2D.ne.number_trans_quant_points)then
         write(fehler,*)'knotenanzahl2D.ne.number_trans_quant_points'
         call qerror(fehler)
      end if
      if(knotenanzahl2D.ne.number_benthic_points)then
         write(fehler,*)'knotenanzahl2D.ne.number_benthic_points ... full 3D output not yet implemented'
         call qerror(fehler)
      end if

      write(zahl,*)rechenzeit
      zahl=adjustl(zahl)

!-------------------------------------
if(bali) then !! kontrollausgabe der Bahnlinien
      print*,'Ausgabe bahnlinien/stromstriche'

      write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'bahnlinien_',trim(zahl),'.vtk'
      if(errcode .ne. 0)call qerror('ausgeben_casu writing filename bahnlinien_ failed')
      write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
      if(errcode .ne. 0)call qerror('ausgeben_casu writing system call rm -rf dateiname bahnlinien_ failed')
      call system(systemaufruf)
      ion=104
      open ( unit =ion , file = dateiname, status ='unknown', action ='write ', iostat = open_error )
      if(open_error.ne.0) then
         write(fehler,*)'open_error bahnlinien.vtk'
         call qerror(fehler)
      end if ! open_error.ne.0
      !write(ion,*)'huhu ausgabe'
      write(ion,'(A)')'# vtk DataFile Version 3.0'
      write(ion,'(A)')'Simulation QSim3D casu'
      write(ion,'(A)')'ASCII'
      !write(ion,'(A)')'DATASET POLYDATA'
      write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
!
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D*2, ' float'
      do n=1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
      end do ! alle Knoten
      do n=1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') ur_x(n), ur_y(n), ur_z(n)
      end do ! alle Knoten

      ! Punkte als vtk-vertices
      write(ion,'(A)')' ' 
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', knotenanzahl2D, 3*knotenanzahl2D
      do n=1,knotenanzahl2D
         write(ion,'(A,2x,I8,2x,I8)')'2', n-1, knotenanzahl2D+(n-1)
      end do ! alle Knoten

      write(ion,'(A)')' ' 
      write(ion,'(A,2x,I12)')'CELL_TYPES ', knotenanzahl2D
      do n=1,knotenanzahl2D
         write(ion,'(A)')'3'
      end do ! alle Knoten

      !print*,"-------- Bahnlinien gemacht, jetzt beginnen Randnormalvektoren ---------------",ianz_rb
      !write(ion,'(A)')' '
      !write(ion,'(A,2x,I12)')'POINT_DATA ',knotenanzahl2D*2

 222  continue
      close (ion) 
end if ! bali
!-------------------------------------
      !print*,"Ausgabe Knotenpunkte Ausgabe Knotenpunkte Ausgabe Knotenpunkte Ausgabe Knotenpunkte Ausgabe Knotenpunkte1234567890123"
      print*,"Ausgabe Knotenpunkte"

      write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'ausgabe_',trim(zahl),'.vtk'
      if(errcode .ne. 0)call qerror('ausgeben_casu writing filename ausgabe_ failed')
      write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
      if(errcode .ne. 0)call qerror('ausgeben_casu writing system call rm -rf dateiname ausgabe_ failed')
      call system(systemaufruf)
      ion=106
      open ( unit =ion , file = dateiname, status ='unknown', action ='write ', iostat = open_error )
      if(open_error.ne.0) then
         write(fehler,*)'open_error ausgabe.vtk'
         call qerror(fehler)
      end if ! open_error.ne.0
      if(knotenanzahl2D .ne. number_benthic_points) then
         write(fehler,*)'3D noch nicht vorgesehen hier'
         call qerror(fehler)
      end if ! 
      if(number_plankt_point .ne. knotenanzahl2D) then
         write(fehler,*)'number_plankt_point und knotenanzahl2D passen nicht zusammen ???'
         call qerror(fehler)
      end if ! 
      call mesh_output(ion)

      write(ion,'(A)')'SCALARS WSP float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         !write(ion,'(f27.6)') p(n)
         write(ion,'(f27.6)') rb_hydraul(3+(n-1)*number_rb_hydraul)
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS tief float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         !write(ion,'(f27.6)') tief(n) 
         write(ion,'(f27.6)') rb_hydraul(2+(n-1)*number_rb_hydraul)
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS summwicht float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         summwicht=0.0
         do j=1,4
            summwicht=summwicht + wicht((n-1)*4+j)
         end do ! alle Ecken im Herkunftselement der Bahnlinie
         write(ion,'(f27.6)') summwicht
      end do ! alle Knoten
      write(ion,'(A)')'SCALARS Rang float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         !write(ion,'(f27.6)') tief(n) 
         write(ion,'(f27.6)') real(n/part) 
      end do ! alle Knoten

! Birte Anfang 17.02.2016
      write(ion,'(A)')'SCALARS flaeche float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         !write(ion,'(f27.6)') tief(n) 
         write(ion,'(f27.6)') knoten_flaeche(n)
      end do ! alle Knoten
! Birte Ende

      write(ion,'(A)')'VECTORS u float'
      !write(ion,'(A)')'LOOKUP_TABLE default' ! nicht bei Vektoren ??
      do n=1,knotenanzahl2D
         nx=u(n)*cos(dir(n))
         ny=u(n)*sin(dir(n))
         nz=0.0
         write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') nx, ny, nz
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS Geschwindigkeitsbetrag float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         !write(ion,'(f27.6)') u(n)
         write(ion,'(f27.6)') rb_hydraul(1+(n-1)*number_rb_hydraul)
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS rau float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,number_plankt_point
         write(ion,'(6x,f27.6)') zone(point_zone(n))%reib
      end do 
      write(ion,'(A)')'SCALARS strickler float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,number_plankt_point
         write(ion,'(6x,f27.6)') strickler( zone(point_zone(n))%reib , rb_hydraul(2+(n-1)*number_rb_hydraul) )
      end do 

      !write(ion,'(A)')'SCALARS Utau float 1'
      !write(ion,'(A)')'LOOKUP_TABLE default'
      !do n=1,knotenanzahl2D
      !   write(ion,'(f27.6)') rb_hydraul(4+(n-1)*number_rb_hydraul)
      !end do ! alle Knoten

      !write(ion,'(A)')'SCALARS Ks float 1'
      !write(ion,'(A)')'LOOKUP_TABLE default'
      !do n=1,knotenanzahl2D
      !   write(ion,'(f27.6)') reib_ks(knoten_zone(n))
      !end do ! alle Knoten

      !write(ion,'(A)')'SCALARS Kst float 1'
      !write(ion,'(A)')'LOOKUP_TABLE default'
      !do n=1,knotenanzahl2D
      !   sandrauh=reib_ks(knoten_zone(n))
      !   wati=rb_hydraul(2+(n-1)*number_rb_hydraul)
      !   write(ion,'(f27.6)') strickler(sandrauh,wati)
      !end do ! alle Knoten

!> \page numdiff numerische Diffusion
!! Abschätzung maximale numerische Diffusion Bezogen auf die Elder Diffusivität\n
!! \f[ 
!! \frac{\nu_{numerisch}}{\nu_{Elder}} \leq \frac{(0.25 ... 0.5) \cdot (\Delta x)^2 / \Delta t}{5.93 \cdot u_{\tau} \cdot h}
!! \f]
!! \n\n
!! aus: ausgabe.f95 ; zurück: \ref Ergebnisse
      write(ion,'(A)')'SCALARS numDiff.rel float 1'
      !write(ion,'(A)')'SCALARS nue_elder float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         relnumdiff=10000.0
         nue_num=0.25*(knoten_flaeche(n)/real(deltat))
         reibgesch=benthic_distribution(45+(n-1)*number_benth_distr)
         wati=rb_hydraul(2+(n-1)*number_rb_hydraul)
         nue_elder=5.93*reibgesch*wati
         if(nue_elder.gt. 0.0) relnumdiff=nue_num/nue_elder
         write(ion,'(6x,f27.6)')relnumdiff
         !write(ion,'(6x,f27.6)')nue_elder
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS Randnummer float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         if((knoten_rand(n).ge.1).and.(knoten_rand(n).le.ianz_rb))then
            write(ion,'(6x,f27.6)') real( rabe(knoten_rand(n))%nr_rb )
         else
            write(ion,'(6x,f27.6)') real( knoten_rand(n) )
         endif
      end do ! alle Knoten

      !write(ion,'(A)')'SCALARS inflow float 1'
      !write(ion,'(A)')'LOOKUP_TABLE default'
      !do n=1,knotenanzahl2D
      !   infl=0.0
      !   if(inflow(n))infl=1.0
      !   write(ion,'(f27.6)') infl
      !end do ! alle Knoten

      if(nur_alter) then ! Aufenthaltszeit in Tagen ! Altersberechnung wie in Shen&Wang 2007 mit real function aufenthaltszeit (tracer,alter)
         write(ion,'(A)')'SCALARS Tage_Aufenthalt float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n=1,knotenanzahl2D
            tr=planktonic_variable(71+(n-1)*number_plankt_vari)
            al=planktonic_variable(73+(n-1)*number_plankt_vari)
            aufenthaltszeit = 0.0
            if(tr .gt. 0.00001 ) aufenthaltszeit= al / tr
            write(ion,'(6x,f27.6)') aufenthaltszeit
         end do ! alle Knoten
      end if !nur_alter

      ! planktische, transportierte Konzentrationen entsprechend ausgabeflag 
      do j=1,number_plankt_vari ! alle tiefengemittelten 
         if(output_plankt(j))then ! zur ausgabe vorgesehen
            write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(planktonic_variable_name(j))),' float 1'
            write(ion,'(A)')'LOOKUP_TABLE default'
            do n=1,knotenanzahl2D ! alle Knoten
               aus=planktonic_variable(j+(n-1)*number_plankt_vari)
               !if (tief(n).le.0.05)aus=-999.999
               write(ion,'(f27.6)') aus
            end do ! alle Knoten
         end if ! zur ausgabe vorgesehen
      end do ! alle planktonic_variable
      do j=1,number_plankt_vari_vert ! alle tiefenaufgelösten z.Z. nur level 1
         if(output_plankt_vert(j))then ! zur ausgabe vorgesehen
            write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(plankt_vari_vert_name(j))),' float 1'
            write(ion,'(A)')'LOOKUP_TABLE default'
            do n=1,knotenanzahl2D ! alle Knoten
               aus=plankt_vari_vert(1+(j-1)*num_lev+(n-1)*number_plankt_vari_vert*num_lev)
               write(ion,'(f27.6)') aus
            end do ! alle Knoten
         end if ! zur ausgabe vorgesehen
      end do ! done all plankt_vari_vert


      ! Übergabe_Konzentrationen entsprechend ausgabeflag 
      do j=1,number_trans_quant ! alle tiefengemittelten
         if(output_trans_quant(j))then ! zur ausgabe vorgesehen
            write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(trans_quant_name(j))),' float 1'
            write(ion,'(A)')'LOOKUP_TABLE default'
            do n=1,knotenanzahl2D
               write(ion,'(f27.6)') transfer_quantity(j+(n-1)*number_trans_quant)
            end do ! alle Knoten
         end if ! zur ausgabe vorgesehen
      end do ! alle transkon
      do j=1,number_trans_quant_vert ! alle tiefenaufgelösten z.Z. nur level 1
         if(output_trans_quant_vert(j))then ! zur ausgabe vorgesehen
            write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(trans_quant_vert_name(j))),' float 1'
            write(ion,'(A)')'LOOKUP_TABLE default'
            do n=1,knotenanzahl2D
               aus=trans_quant_vert(1+(j-1)*num_lev_trans+(n-1)*num_lev_trans*number_trans_quant_vert)
               write(ion,'(f27.6)') aus
            end do ! alle Knoten
         end if ! zur ausgabe vorgesehen
      end do ! done all vertically distributed transfer quantities


      ! benthic distributions according to output flag
      do j=1,number_benth_distr ! all benthic distributions
         if(output_benth_distr(j))then ! flagged?
            write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(benth_distr_name(j))),' float 1'
            write(ion,'(A)')'LOOKUP_TABLE default'
            do n=1,number_benthic_points ! all nodes
               write(ion,'(f27.6)') benthic_distribution(j+(n-1)*number_benth_distr)

            end do ! all nodes
         end if ! flagged
      end do ! all benthic distributions

!>
!!

      ! Benthische_verteilungen entsprechend ausgabeflag 
      !write(ion,'(A)')'SCALARS Temperatur_Sediment float 1'
      !write(ion,'(A)')'LOOKUP_TABLE default'
      !do n=1,knotenanzahl2D
      !   write(ion,'(f27.6)') benthische_verteilung(1,n)
      !end do

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
      !       end do ! alle i Kanten
      !    end if ! randknoten 
      !    write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') nx, ny, nz
      ! end do ! alle Knoten

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
      !       end do ! alle i Kanten
      !    end if ! randknoten 
      !    write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') nx, ny, nz
      ! end do ! alle Knoten

      ! write(ion,'(A)')'SCALARS tief_diff float 1'
      ! write(ion,'(A)')'LOOKUP_TABLE default'
      ! do n=1,knotenanzahl2D
      !    aus=p(n)-knoten_z(n)
      !    write(ion,'(f27.6)') aus 
      ! end do ! alle Knoten

      !! Kontrollwerte wg. Zuflussrandermittlung:
      !goto 777 ! z. Z. nicht ausgeben:
      !write(ion,'(A)')'SCALARS ubetr float 1'
      !write(ion,'(A)')'LOOKUP_TABLE default'
      !do n=1,knotenanzahl2D
      !   ubetr=(((knoten_x(n)-ur_x(n))**2 + (knoten_y(n)-ur_y(n))**2 + (knoten_z(n)-ur_z(n))**2)**0.5)/dttrans
      !   write(ion,'(f27.6)') ubetr
      !end do ! alle Knoten
      !777  continue

      close (ion)

      ! Visualisierung anwerfen ...
      ! write(systemaufruf,*)trim(modellverzeichnis),'visu/visu ',trim(modellverzeichnis),'  ', trim(zahl)
      ! call system(systemaufruf,sysa)
      ! if(sysa.ne.0) then
      !    print*,'Aufruf von visu fehlgeschalgen'
      !    ! call qerror(fehler) 7
      ! end if ! systemaufruf fehlgeschlagen

      ! call aus_grd() !! Ausgabe als Janet-lesbare Datei

      RETURN
      END subroutine ausgeben_casu
