!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

!> \page Ergebnisse Ergebnisse ausgeben, darstellen und auswerten
!!
!! Z. Z. arbeitet QSim-3D noch 2D-tiefengemittelt  (wyrwa mai 2018).
!! 
!! \section ausgabekonzept Ausgabe-Konzept
!! Da bei der Simulation mit mehrdimensionalen Modellen sehr große Datenmengen als Ergebnisse anfallen,\n
!! wird in QSim-3D das Konzept verfolgt,\n
!! bereits vor dem Programmstart festzulegen, was ausgegeben werden soll:
!! \n
!! \section ausgabekonzentrationen Ausgabe-Variablen
!! Die Datei <a href="./exp/ausgabekonzentrationen.txt" target="_blank">ausgabekonzentrationen.txt</a>
!! dient dazu, anzugeben, welche Variablen ausgegeben werden sollen. 
!! Für jede Variable gibt es in dieser Datei eine Spalte. Steht in der ersten Zeile ein "x" findet eine Ausgabe statt.\n 
!! Von QSim-3D wird die Datei
!! <a href="./exp/ausgabekonzentrationen_beispiel.txt" target="_blank">ausgabekonzentrationen_beispiel.txt</a>
!! ausgegeben, damit der Nutzer weiß welche Variablen als mögliche Ausgabe verfügbar sind. 
!! Nach "Ankreuzen" in der ersten Spalte kann diese Datei als Eingabe verwendet werden.\n
!! Diese Variablen-Auswahl wirkt sich sowohl auf die Ausgabe von \ref ganglinienausgabe als auch 
!! auf die Ausgabe von \ref mehrdimausgabe aus
!! \n
!! \section ganglinienausgabe Ganglinien an einzelnen Knoten
!! Mit der Datei <a href="./exp/ganglinien_knoten.txt" target="_blank">ganglinien_knoten.txt</a>
!! wird angegeben, an welchen Knoten eine Ganglinie ausgegeben werden soll.
!!\n
!! <i> Dabei bitte beachten, dass Knotennummern in casu, Paravies und Janet ab 0 zählen\n
!! in QSim-3D aber ab 1 </i>
!! \n
!! So entsteht am Ende des Berechnungslaufs im Modellverzeichnis ein Unterverzeichnis "ganglinien".\n
!! Dort ist für jeden gewählten Knoten eine mit "g" beginnende Datei abgelegt
!! <a href="./exp/g38503.txt" target="_blank">Beispiel g38503.txt</a>,\n
!!! welche mit <a href="http://voss-mod02/wiki/doku.php?id=gnuplot" target="_blank"> gnuplot</a>  
!! visualisiert werden kann.
!! \n
!! \section mehrdimausgabe mehrdimensionale Felder zu einzelnen Zeitpunkten
!! Mittels der Datei <a href="./exp/ausgabezeitpunkte.txt" target="_blank">ausgabezeitpunkte.txt</a>
!! wird angegeben, zu welchen Zeitpunkten mehrdimensionale Felder ausgegeben werden sollen.\n 
!! Im Verlauf der Berechnung werden diese dann im Modellverzeichnis (siehe \ref Datenmodell) im
!! .vtk-Format abgelegt. Der Dateiname (z. B. ausgabe_734850.vtk) enthält den Zeitpunkt 
!! als ganzahligen Integer-Wert in Sekunden. Der Ursprung dieser Sekundenzählung ist in der 
!! Datei meta im traninfo-Verzeichnis (\ref Transportinformationen) festgelegt. Die Bildschirm-Ausgabe
!! von QSim-3D gibt für jeden Zeitpunkt das Datum zusammen mit dem Sekundenzähler aus.
!! \n
!! Das .vtk-Format dient der Visualisierung mit 
!! <a href="http://www.visitusers.org/index.php?title=Main_Page" target="_blank">VisIt</a> oder
!! <a href="http://voss-mod02/wiki/doku.php?id=paraview" target="_blank">paraview</a>.\n
!! netCDF-Formate sind für unstrukturierte Netze noch nicht praktikabel.
!! \n
!! \section Schnitte
!!
!! \subsection laengsschnitt (Linienausgabe)
!! Das Programm "laengsschnitt" ermöglicht es, auf der Basis der \ref ganglinienausgabe von QSim-3D Ergebnissen, einen 
!! Schnitt entlang einer horizonzalen Linie zusammenzusetzen; zumeist handelt es sich dabei um Längsschnitte
!! entlang der Flußachse. Voraussetzung ist es, dass alle Punkte der Linie zur Ganglinienausgabe angewählt wurden. 
!! Diese Punkte werden nicht daraufhin überprüft, ob sie im Netz zusammenhängend (jeweils nur von einer Elementkante verbunden)
!! sind.\n
!! Dazu ließt das externe Programm "laengsschnitt" die Datei <a href="./exp/kilonummer" target="_blank">kilonummer</a>,
!! in der je einer Abstandskoordinate (Flußkilometrierung) eine Punktnummer zugeordnet ist.\n
!! Zu allen Punktnummern in kilonummer müssen im Unterverzeichnis "ganglinien" Dateien mit Ganglinien-Ergebnissen vorliegen.
!! Aus den dort vorgefundenen Zeitpunkten kann der Benutzer dann einen wählen, der in die Datei 
!! <a href="./exp/schnittig" target="_blank">schnittig</a> ausgegeben wird.\n
!! <a href="./taz/laengsschnitt_source.taz">laengsschnitt Programm-Quelle</a>\n
!!
!! \subsection Querschnitte (Flux-Ermittlung)
!! Zur Durchflussermittlung können Querschnitte spezifiziert werden:\n
!! Wenn die Datei <a href="./exp/quer.txt" target="_blank">schnitt.txt </a> im Modellverzeichnis vorhanden ist, werden 
!! Querschnitte ausgewertet.
!! \n\n
!! Dazu werden die auf schnitt.txt angegebenen Punktfolgen auf Zusammenhang 
!! (Aufeinande folgende Knoten sind durch jeweils eine Elementkante verbunden) 
!! von der Subroutine querschnitt_lesen() geprüft.
!! Dabei wird auch geprüft, ob der Querschnitt am Rand anfängt und endet.
!! \n\n
!! Die Fluss-Ermittlung bewerkstelligt die Subroutine querschnitt_flux() durch Aufruf von flux().
!! Es wird der Volumenstrom des Wassers und der Massenfluss aller \ref ausgabekonzentrationen ermittelt. 
!! Die Ausgabe der Flux-Ermittlung wird im Unterverzeichnis "ganglinen" abgelegt. Die Dateien beginnen mit dem Buchstaben "q"
!! und sind in der Reihenfolge nummeriert, in der Die Querschnitte in "schnitt.txt" enthalten sind.
!! Diese Ausgabe wird von der Subroutine ganglinien_schliessen() mit erledigt.
!!
!! \subsection Randflüsse
!! Um Volumen- und Massenströme über einzelne Ränder ermitteln zu können, müssen zunächst die Knoten eines Randes in eine 
!! zusammenhängende Folge (aufeinande folgende Knoten sind durch jeweils eine Elementkante verbunden) gebracht werden.
!! Dies übernimmt die Subroutine rand_zusammenhang().
!! Die Fluss-Ermittlung bewerkstelligt die Subroutine rand_flux().
!! Die Ausgabe der Rand-Flux-Ermittlung wird im Unterverzeichnis "ganglinen" abgelegt. 
!! Die Dateien beginnen mit dem Buchstaben "r", gefolgt von der Randnummer. 
!! Diese Ausgabe wird von der Subroutine ganglinien_schliessen() mit erledigt.
!!
!! \section sonstiges
!! - Es wurde eine Abschätzung für die \subpage numdiff implementiert.
!! \n\n
!! zurück: \ref lnk_modellstruktur; Quelle: ausgabe.f95 ; siehe auch: \ref Datenmodell


!> macht nur Verzweigung nach hydraulischem treiber wegen deren unterschiedlichen Datenstrukturen 
!! \n\n
!! aus: ausgabe.f95 ; zurück: \ref Ergebnisse
      SUBROUTINE ausgeben()
      use modell                                                   
      implicit none

      call mpi_barrier (mpi_komm_welt, ierr)
      call gather_benthic()
      call gather_ueber()
      !! Aufruf immer nach stofftransport() daher ist gather_planktkon() immer schon gemacht

if(meinrang.eq.0)then ! nur auf Prozessor 0 bearbeiten
      select case (hydro_trieb)
      case(1) ! casu-transinfo                                           
         call ausgeben_casu()
      case(2) ! Untrim² netCDF
         call ausgeben_untrim(rechenzeit)
      case(3) ! SCHISM
         !!!### call ausgeben_schism(rechenzeit)
      case default
         print*,'hydro_trieb=',hydro_trieb
         call qerror('ausgeben: Hydraulischer Antrieb unbekannt')
      end select
end if ! nur Prozessor 0 
      call mpi_barrier (mpi_komm_welt, ierr)
      RETURN
      END subroutine ausgeben
!----+-----+----+-----+----+-----+----+-----+----

!> Suboutine tagesmittelwert() macht tagesmittelwerte
!! \n\n
      subroutine tagesmittelwert()
      use modell                                                   
      implicit none
      integer j,n, ion, open_error, system_error, errcode
      real tagesanteil, null
      character(len=longname) :: dateiname, dateiname2, systemaufruf, zahl
      character(50) tm,tt,tj
      null=0.0

      !if(.true.) then ! heute mittelwertausgabe
      if((monat.eq. 7).and.(tag.ge. 5).and.(tag.le. 25)) then ! heute mittelwertausgabe

if(uhrzeit_stunde .lt. uhrzeit_stunde_vorher)then ! Tageswechsel 

      write(zahl,*)rechenzeit
      write(tj,'(I4.4)')jahr
      write(tm,'(I2.2)')monat
      write(tt,'(I2.2)')tag
      zahl=adjustl(zahl)
      tj=adjustl(tj)
      tm=adjustl(tm)
      tt=adjustl(tt)
      ion=105

      write(dateiname,'(8A)',iostat = errcode)trim(modellverzeichnis),'mittelwert',trim(tj),'_',trim(tm),'_',trim(tt),'.vtk'
      if(errcode .ne. 0)call qerror('tagesmittelwert writing filename mittelwert failed')
      print*,'Ausgabe Mittelwert auf: ',trim(dateiname)
      write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
      if(errcode .ne. 0)call qerror('tagesmittelwert writing system call rm -rf dateiname mittelwert failed')
      call system(trim(systemaufruf),system_error)
      if(system_error.ne.0) then
         print*,'rm -rf mittelwert_*** failed.'
      end if ! system_error.ne.0
      open ( unit =ion , file = dateiname, status ='new', action ='write', iostat = open_error )
      if(open_error.ne.0) then
         write(fehler,*)'open_error mittelwert_vtk open_error=',open_error
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
      !write(ion,*)'huhu ausgabe'
      write(ion,'(A)')'# vtk DataFile Version 3.0'
      write(ion,'(A)')'Simlation tiqusim'
      write(ion,'(A)')'ASCII'
      !write(ion,'(A)')'DATASET POLYDATA'
      write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
!
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
      do n=1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
      end do ! alle Knoten

      if(element_vorhanden) then
         ! Elemente ausgeben
         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12,2x,I12)')'CELLS ', n_elemente, summ_ne
         do n=1,n_elemente ! alle Elemente
            if (cornernumber(n).eq.3)then
                write(ion,'(4(I8,2x))') & 
               cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
            end if
            if (cornernumber(n).eq.4)then
               write(ion,'(5(I8,2x))') &
               cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
            end if
         end do ! alle Elemente

         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12)')'CELL_TYPES ', n_elemente
         do n=1,n_elemente ! alle Elemente
            if (cornernumber(n).eq.3)write(ion,'(A)') '5' 
            if (cornernumber(n).eq.4)write(ion,'(A)') '9' 
         end do ! alle Elemente
      else ! keine file.elements vorhanden
         ! Punkte als vtk-vertices
         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12,2x,I12)')'CELLS ', knotenanzahl2D, 2*knotenanzahl2D
         do n=1,knotenanzahl2D
            write(ion,'(A,2x,I8)')'1', n-1
         end do ! alle Knoten

         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12)')'CELL_TYPES ', knotenanzahl2D
         do n=1,knotenanzahl2D
            write(ion,'(A)')'1'
         end do ! alle Knoten
      end if !! element_vorhanden

      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
      write(ion,'(A)')'SCALARS Gelaendehoehe float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         write(ion,'(f27.6)') knoten_z(n)
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS T_wass_mittel float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         write(ion,'(f27.6)') transfer_quantity_p(68+(n-1)*number_trans_quant)
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS T_sed_mittel float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         write(ion,'(f27.6)') transfer_quantity_p(69+(n-1)*number_trans_quant)
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS mittel_tief float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         if(transfer_quantity_p(71+(n-1)*number_trans_quant).gt. 0.0)then
            write(ion,'(f27.6)') transfer_quantity_p(70+(n-1)*number_trans_quant)  &
                                  / transfer_quantity_p(71+(n-1)*number_trans_quant)    ! tagesmittelwert Wassertiefe
         else
            write(ion,'(f27.6)') null
         end if
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS Bedeckungsdauer float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         write(ion,'(f27.6)') transfer_quantity_p(71+(n-1)*number_trans_quant)
      end do ! alle Knoten

      do n=1,knotenanzahl2D
         transfer_quantity_p(68+(n-1)*number_trans_quant) = 0.0
         transfer_quantity_p(69+(n-1)*number_trans_quant) = 0.0
         transfer_quantity_p(70+(n-1)*number_trans_quant) = 0.0
         transfer_quantity_p(71+(n-1)*number_trans_quant) = 0.0
      end do ! alle Knoten wieder null setzen

      close(ion)
endif ! Tageswechsel

      tagesanteil = real(deltat)/real(86400)
      do n=1,knotenanzahl2D  !!!!!!!!!!!  mittelwerte aufsummieren
         transfer_quantity_p(68+(n-1)*number_trans_quant) = transfer_quantity_p(68+(n-1)*number_trans_quant)  &
         + (planktonic_variable_p(1+(n-1)*number_plankt_vari)  * tagesanteil) ! Wasser-Temperatur Rückgabewert
         transfer_quantity_p(69+(n-1)*number_trans_quant) = transfer_quantity_p(69+(n-1)*number_trans_quant)  &
         + (benthic_distribution_p(1+(n-1)*number_benth_distr) * tagesanteil) ! Temperatur des Sediments - Rückgabewert 
         if(rb_hydraul(2+(n-1)*number_rb_hydraul).gt. 0.02)then ! tief(n)
            transfer_quantity_p(70+(n-1)*number_trans_quant) = transfer_quantity_p(70+(n-1)*number_trans_quant)  &
            + (rb_hydraul(2+(n-1)*number_rb_hydraul) * tagesanteil) ! TagesSumme Tiefe wenn bedeckt
            transfer_quantity_p(71+(n-1)*number_trans_quant) = transfer_quantity_p(71+(n-1)*number_trans_quant)  &
            + (tagesanteil) ! Bedeckungsdauer (Tageanteil)
         endif
      end do ! alle Knoten

      endif ! heute mittelwertberechnung
      uhrzeit_stunde_vorher = uhrzeit_stunde

      RETURN
      END subroutine tagesmittelwert

!----+-----+----
!> Suboutine ausgabekonzentrationen() ließt aus der Datei 
!! <a href="./exp/ausgabekonzentrationen.txt" target="_blank">ausgabekonzentrationen.txt</a>
!! welche variablen ausgegeben werden sollen.\n
!! als Beispiel-Datei, der entnehmbar ist, welche Variablen ausgegeben werden könnnen, schreibt ausgabekonzentrationen()
!! die Datei ausgabekonzentrationen_beispiel.txt \n
!! Die angekreuzten, gewählten Variablen werden sowohl bei den Ganglinien als auch bei den ausgabezeitpunkten verwendet.
!!\n\n
!! aus Datei ausgabe.f95 ; zurück zu \ref Modellerstellung
      subroutine ausgabekonzentrationen()
      use modell                                                   
      implicit none
      integer :: ion, ibei, open_error, io_error, alloc_status, iscan, j, n, sysa
      character (len=200) :: dateiname, text
      logical :: found
      character(300) systemaufruf
      !>integer :: k_ausgabe
      !>integer , allocatable , dimension (:) :: ausgabe_konz

      write(dateiname,'(2A)')trim(modellverzeichnis),'ausgabekonzentrationen.txt'
      ion=103
      open ( unit =ion , file = dateiname, status ='old', action ='read ', iostat = open_error )
      if(open_error.ne.0) then
         print*,'keine ausgabekonzentrationen, open_error=',open_error
         close (ion)
         return
      else
         print*,'ausgabekonzentrationen.txt geoeffnet ...'
      end if ! open_error.ne.0

      do while( zeile(ion)) !!  read all lines and understand
         if((ctext(1:1).eq.'x').or.(ctext(1:1).eq.'X'))then ! line marked ?
            found=.false.
			!print*,trim(ctext)

            do j=1,number_plankt_vari ! all depth averaged planktic con.
               write(text,'(A18)')trim(planktonic_variable_name(j))
               iscan=index(trim(ctext),trim(text))
               if(iscan.gt.0)then ! found
                  print*,meinrang,iscan,' output for planktic concentration j=',j,' parameter: ',trim(text)
                  !print*,trim(ctext)
                  output_plankt(j)=.true.
                  found=.true.
               end if !! in string ctext
            end do ! done all planktic con.
            do j=1,number_plankt_vari_vert ! all vertically distributed planktonic variables
               write(text,'(A18)')trim(plankt_vari_vert_name(j))
               iscan=index(trim(ctext),trim(text))
               if(iscan.gt.0)then ! found
                  if(meinrang==0)print*,'output only for level 1; plankt_vari_vert j=',j,' parameter: ',trim(text)
                  !print*,trim(ctext)
                  output_plankt_vert(j)=.true.
                  found=.true.
               end if !! in string ctext
            end do ! done all plankt_vari_vert

            do j=1,number_benth_distr ! all benthic distributions
               write(text,'(A)')ADJUSTL(trim(benth_distr_name(j)))
               iscan=index(trim(ctext),trim(text))
               if(iscan.gt.0)then ! found
                  if(meinrang==0)print*,'output for benthic distribution j=',j,' parameter: ',trim(text)
                  !print*,trim(ctext)
                  output_benth_distr(j)=.true.
                  found=.true.
               end if !! in string ctext
               ! ausgabe_bentver(j)=.true. ! überbrückt: ### alle
               ! ausgabe_bentver(j)=.false. ! überbrückt: ### keine
            end do ! done all all benthic distributions

            do j=1,number_trans_val  ! alle globalen Übergabe Werte
               write(text,'(A)')ADJUSTL(trim(trans_val_name(j)))
               iscan=index(trim(ctext),trim(text))
               if(iscan.gt.0)then ! found
                  print*,'ausgabe globaler uebergabe wert j=',j,' parameter: ',trim(text)
                  !print*,trim(ctext)
                  output_trans_val(j)=.true.
                  found=.true.
               end if !! in string ctext
            end do ! 
            do j=1,number_trans_quant ! all exchange con.
               write(text,'(A)')ADJUSTL(trim(trans_quant_name(j)))
               iscan=index(trim(ctext),trim(text))
               if(iscan.gt.0)then ! found
                  if(meinrang==0)print*,'output for exchange concentration j=',j,' parameter: ',trim(text)
                  !print*,trim(ctext)
                  output_trans_quant(j)=.true.
                  found=.true.
               end if !! in string ctext
               ! output_trans_quant(j)=.true. ! überbrückt: ### alle
               ! output_trans_quant(j)=.false. ! überbrückt: ### keine
            end do ! done all exchange con.
            do j=1,number_trans_quant_vert  ! all vertically distributed transfer quantities
                  write(text,'(A)')ADJUSTL(trim(trans_quant_vert_name(j)))
                  iscan=index(trim(ctext),trim(text))
                  if(iscan.gt.0)then ! found
                     if(meinrang==0)print*,'output only for level 1; trans_quant_vert j=',j,' parameter: ',trim(text)
                     !print*,trim(ctext)
                     output_trans_quant_vert(j)=.true.
                     found=.true.
                  end if !! in string ctext
            end do ! done all vertically distributed transfer quantities

            if (.not.found) then
               print*,'no parameter found for choice:'
               !print*,trim(ctext)
            end if ! not found
         end if ! marked line
      end do ! no further line
      close (ion)

      if(nur_alter)then ! allways write age concentrations in age simulation
         output_plankt(71)=.true. ! Tracer
         output_plankt(73)=.true. ! age_decay
         output_plankt(74)=.true. ! age_arith
         output_plankt(75)=.true. ! age_growth
      end if ! nuralter

!     writing output variable list moved to SUBROUTINE eingabe()
      !text='ausgabekonzentrationen_beispiel.txt'
      !dateiname=trim(adjustl(modellverzeichnis))//trim(adjustl(text))
      !systemaufruf='cp '//trim(adjustl(codesource))//'/'//trim(adjustl(text))//' '//trim(dateiname)
      !call system(systemaufruf,sysa)
      !if(sysa.ne.0) Print*,'### kopieren von ',trim(adjustl(text)),' ausgabekonzentrationen_beispiel.txt fehlgeschlagen ###'

      RETURN
      END subroutine ausgabekonzentrationen

!----+-----+----
!> suboutine ausgabekonzentrationen_beispiel writes file ausgabekonzentrationen_beispiel.txt to inform about available output variables
!! \n\n
      subroutine ausgabekonzentrationen_beispiel()
      use modell                                                   
      implicit none
      integer :: j,open_error
      character (len=300) :: dateiname

      write(dateiname,'(2A)')trim(modellverzeichnis),'ausgabekonzentrationen_beispiel.txt'
      open ( unit =104 , file = dateiname, status ='replace', action ='write ', iostat = open_error )
      if(open_error.ne.0) then
         print*,'ausgabekonzentrationen_beispiel.txt open_error=',open_error
         close (104)
         return
      else
         print*,'ausgabekonzentrationen_beispiel.txt opened for write ...'
      end if ! open_error.ne.0

      write(104,'(A)')"# depth averaged, planctonic, transported concentrations"
      do j=1,number_plankt_vari ! all depth averaged planktic con.
         write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(planktonic_variable_name(j))
      end do ! done all planktic con.

      write(104,'(A)')"# depth resolving, planctonic, transported concentrations"
      do j=1,number_plankt_vari_vert ! all vertically distributed planktonic variables
         write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(plankt_vari_vert_name(j))
      end do ! done all plankt_vari_vert

      write(104,'(A)')"# bentic distributions"
      do j=1,number_benth_distr ! all benthic distributions
         write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(benth_distr_name(j))
      end do ! done all benthic distributions

      write(104,'(A)')"# global transfer variables"
      do j=1,number_trans_val  ! alle globalen Übergabe Werte
         write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(trans_val_name(j))
      end do

      write(104,'(A)')"# depth averaged transfer variables"
      do j=1,number_trans_quant ! all exchange con.
         write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(trans_quant_name(j))
      end do

      write(104,'(A)')"# depth resolving transfer variables"
      do j=1,number_trans_quant_vert  ! all vertically distributed transfer quantities
         write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(trans_quant_vert_name(j))
      end do

      close (104)
      RETURN
      END subroutine ausgabekonzentrationen_beispiel

!----+-----+----
!> Die suboutine ausgabezeitpunkte() ließt Datei ausgabezeitpunkte.txt und schreibt Feld ausgabe_zeitpunkt
!! \n\n
      subroutine ausgabezeitpunkte()
      use modell                                                   
      implicit none
      integer :: n, ion, open_error, io_error, alloc_status, nba
      character (len=200) :: dateiname
      !integer :: n_ausgabe
      !integer , allocatable , dimension (:) :: ausgabe_punkt

      write(dateiname,'(2A)')trim(modellverzeichnis),'ausgabezeitpunkte.txt'
      ion=103
      open ( unit =ion , file = dateiname, status ='old', action ='read ', iostat = open_error )
      if(open_error.ne.0) then
         print*,'keine Ausgabezeitpunkte, open_error=',open_error
         n_ausgabe=0
         close (ion)
         return
      end if ! open_error.ne.0

      n=0
      do while( zeile(ion))
         if(ctext(1:1).ne.'#')then ! keine Kommentarzeile
            !print*,'1 ',trim(ctext)
            n=n+1
            read(ctext,*, iostat = io_error)tag, monat, jahr, stunde, minute, sekunde !, uhrzeit_stunde
            if(io_error.ne.0) then
               print*,'unlesbare Angabe in ausgabezeitpunkte.txt'
               write(fehler,*)trim(ctext)
               call qerror(fehler)
            end if ! io_error.ne.0
            !call sekundenzeit()
         endif ! keine Kommentarzeile
      end do ! zeile

      n_ausgabe=n
      print*,n_ausgabe,' Ausgabezeitpunkte'
      allocate (ausgabe_zeitpunkt(n_ausgabe), stat = alloc_status )
      allocate (ausgabe_bahnlinie(n_ausgabe), stat = alloc_status )
      rewind (ion)

      n=0
      do while( zeile(ion))
         if(ctext(1:1).ne.'#')then ! keine Kommentarzeile
            !print*,'2 ',trim(ctext)
            n=n+1
            read(ctext,*, iostat = io_error)tag, monat, jahr, stunde, minute, sekunde !, uhrzeit_stunde
            print*,"ausgabezeitpunkt=",tag, monat, jahr, stunde, minute, sekunde
            call sekundenzeit(1)
            !call zeitsekunde() !! damit auch die Uhrzeit stimmt
            ausgabe_zeitpunkt(n)=zeitpunkt
            print*,'Ausgabezeitpunkt ',n,' Datum: ', tag, monat, jahr,' ; Uhrzeit', stunde, minute, sekunde,  &
                   ' uhrzeit_stunde=',uhrzeit_stunde,  &
                   'Stunden |  ergibt: ',zeitpunkt,' Sekunden seit ', trim(time_offset_string)
            if(zeitpunkt.lt.startzeitpunkt)print*,'### keine Ausgabe ### liegt vor dem startzeitpunkt. \n'
            if(zeitpunkt.gt.endzeitpunkt)print*,'### keine Ausgabe ### liegt nach dem endzeitpunkt. \n'
            read(ctext,*, iostat = io_error)tag, monat, jahr, stunde, minute, sekunde, nba
            if(io_error.eq.0) then
               ausgabe_bahnlinie(n)=nba
               print*,'mit Bahnlinienausgabe ',ausgabe_bahnlinie(n)
            else
               ausgabe_bahnlinie(n)=0
            end if ! io_error.ne.0
         endif ! keine Kommentarzeile
      end do ! zeile

      close (ion)
      return
      END subroutine ausgabezeitpunkte

!-----+-----+-----+-----+
!> true if output required now 
!! \n\n
      Subroutine ausgeben_parallel()
      use modell                                                   
      implicit none
      integer :: alloc_status

      !print*,meinrang,'ausgeben_parallel() n_ausgabe=',n_ausgabe

      call MPI_Bcast(n_ausgabe,1,MPI_INT,0,mpi_komm_welt,ierr)
      if(ierr.ne.0)then
         write(fehler,*)'14  ',meinrang, 'MPI_Bcast(n_ausgabe,  ierr=', ierr
         call qerror(fehler)
      end if
      !print*,'MPI_Bcast(n_ausgabe gemacht',meinrang
      if(meinrang.ne. 0)then
         allocate (ausgabe_zeitpunkt(n_ausgabe), stat = alloc_status )
         allocate (ausgabe_bahnlinie(n_ausgabe), stat = alloc_status )
      end if
      call MPI_Bcast(ausgabe_zeitpunkt,n_ausgabe,MPI_INT,0,mpi_komm_welt,ierr)
      if(ierr.ne.0)then
         write(fehler,*)meinrang, 'MPI_Bcast(ausgabe_zeitpunkt,  ierr=', ierr
         call qerror(fehler)
      end if
      !print*,'MPI_Bcast(ausgabe_zeitpunkt gemacht',meinrang
      call MPI_Bcast(ausgabe_bahnlinie,n_ausgabe,MPI_INT,0,mpi_komm_welt,ierr)
      if(ierr.ne.0)then
         write(fehler,*)meinrang, 'MPI_Bcast(ausgabe_bahnlinie,  ierr=', ierr
         call qerror(fehler)
      end if
      !print*,'MPI_Bcast(ausgabe_bahnlinie gemacht',meinrang

      return
      END subroutine ausgeben_parallel
!-----+-----+-----+-----+
!> true if output required now 
!! \n\n
      logical function jetzt_ausgeben()
      use modell                                                   
      implicit none
      integer :: n , diff

      jetzt_ausgeben=.false.
      bali=.false.

      !if(hydro_trieb.eq. 3)then
      !   jetzt_ausgeben=.FALSE.
      !   if(meinrang.eq. 0)print*,'SCHISM preliminary: no output for all timesteps'
      !   return
      !end if ! SCHISM
                 
      do n=1,n_ausgabe,1
         diff=ausgabe_zeitpunkt(n)-rechenzeit
         if( (diff.ge.(-1*(deltat/2))) .and. (diff.lt.(deltat/2)) )then
            jetzt_ausgeben=.TRUE.
            if (ausgabe_bahnlinie(n).ne. 0) bali=.TRUE.
         end if !
         !print*,'ausgeben? ', rechenzeit, ausgabe_punkt(n), deltat, (rechenzeit-ausgabe_punkt(n))
         !if(((rechenzeit-ausgabe_punkt(n)).lt.deltat).and.((rechenzeit-ausgabe_punkt(n)).ge.0))then
         !if(((rechenzeit-ausgabe_punkt(1)).lt.deltat).and.((rechenzeit-ausgabe_punkt(1)).ge.0))then
         !   print*,'jetzt jede Stunde ausgeben'
         !   ausgabe_punkt(1)=ausgabe_punkt(1)+3600
         !   jetzt_ausgeben=.TRUE.
         !end if !
      end do
      !if(.not.jetzt_ausgeben)jetzt_ausgeben=(zeitschrittanzahl.eq.izeit) !! Ausgabe am Ende

      if(jetzt_ausgeben)print*,'jetzt_ausgeben ,meinrang',meinrang
      return
      END function jetzt_ausgeben

!----+-----+----
!> Initialisierung der transportierten Übergabe-Konzentrationen.
!! \n\n
      subroutine ini_aus(nk)
         use modell                                                   
         implicit none
         integer nk,k,n,as,j
if(meinrang.eq.0)then ! nur auf Prozessor 0 bearbeiten

         knotenanzahl_ausgabe=nk
         anzahl_auskonz=1

         allocate (AusgabeKonzentrationsName(anzahl_auskonz), stat = as )
         if(as.ne.0)then
            write(fehler,*)' Rueckgabewert   von   allocate AusgabeKonzentrationsName :', as
            call qerror(fehler)
         end if 
         AusgabeKonzentrationsName( 1)= "            BACmua"

!!!!!!!!! ausgabe_konzentration allokieren und initialisieren
         allocate (ausgabe_konzentration(anzahl_auskonz,knotenanzahl_ausgabe), stat = as )
         if(as.ne.0)then
            write(fehler,*)' Rueckgabewert   von   allocate transfer_quantity :', as
            call qerror(fehler)
         end if 
         do k=1,knotenanzahl_ausgabe ! alle knoten
            do j=1,anzahl_auskonz ! initialisierung aller konzentrationen zunächt auf Null
               ausgabe_konzentration(j,k)=0.0
            end do
         end do

end if !! nur prozessor 0
      END subroutine ini_aus
!----+-----+----

!> ELCIRC .grd Format ausgabe momentan Sept15 Überstaudauern für Elbestabil
!! \n\n
      subroutine aus_grd()
         use modell
         implicit none
         integer :: ion, open_error, io_error, n
         character (len=200) :: dateiname

         if(.not. uedau_flag) return !! Überstaudauern nur ausgeben wenn parameter in module_modell.f95 gesetzt
         if(uedau_flag) call qerror(" aus_grd() Überstaudauer nicht mehr implementiert")

if(meinrang.eq.0)then ! nur auf Prozessor 0 bearbeiten
         write(dateiname,'(2A)')trim(modellverzeichnis),'uedau0.grd'
         ion=107
         open ( unit =ion , file = dateiname, status ='unknown', action ='write ', iostat = open_error )
            if(open_error.ne.0) then
            print*,'uedau0.grd, open_error=',open_error
            close (ion)
            return
         end if ! open_error.ne.0
          
         write(ion,'(A)') 'Grid written by QSim3D'
         write(ion,'(I9,2x,I9)')n_elemente, knotenanzahl2D
         
         do n=1,knotenanzahl2D
            ! write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
            write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n),   &
     &      benthic_distribution(44+(n-1)*number_benth_distr)  !! Überstaudauer
         end do ! alle Knoten
              
         do n=1,n_elemente ! alle Elemente
            if (cornernumber(n).eq.3)then
                write(ion,'(5(I8,2x))') & 
                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
            end if
            if (cornernumber(n).eq.4)then
               write(ion,'(6(I8,2x))') &
               n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
            end if
         end do ! alle Elemente
                
         close (ion)
         print*,'Überstaudauer 0-15 cm (44) ausgegeben auf: uedau0.grd'
!!!!!!!!!
         write(dateiname,'(2A)')trim(modellverzeichnis),'uedau15.grd'
         ion=107
         open ( unit =ion , file = dateiname, status ='unknown', action ='write ', iostat = open_error )
            if(open_error.ne.0) then
            print*,'uedau15.grd, open_error=',open_error
            close (ion)
            return
         end if ! open_error.ne.0
          
         write(ion,'(A)') 'Grid written by QSim3D'
         write(ion,'(I9,2x,I9)')n_elemente, knotenanzahl2D
         
         do n=1,knotenanzahl2D
            ! write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
            write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n),   &
     &      benthic_distribution(45+(n-1)*number_benth_distr)  !! Überstaudauer
         end do ! alle Knoten
              
         do n=1,n_elemente ! alle Elemente
            if (cornernumber(n).eq.3)then
                write(ion,'(5(I8,2x))') & 
                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
            end if
            if (cornernumber(n).eq.4)then
               write(ion,'(6(I8,2x))') &
               n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
            end if
         end do ! alle Elemente
                
         close (ion)
         print*,'Überstaudauer 15-25 cm (45) ausgegeben auf: uedau15.grd'
!!!!!!!!!
         write(dateiname,'(2A)')trim(modellverzeichnis),'uedau25.grd'
         ion=107
         open ( unit =ion , file = dateiname, status ='unknown', action ='write ', iostat = open_error )
            if(open_error.ne.0) then
            print*,'uedau25.grd, open_error=',open_error
            close (ion)
            return
         end if ! open_error.ne.0
          
         write(ion,'(A)') 'Grid written by QSim3D'
         write(ion,'(I9,2x,I9)')n_elemente, knotenanzahl2D
         
         do n=1,knotenanzahl2D
            ! write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
            write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n),   &
     &      benthic_distribution(46+(n-1)*number_benth_distr)  !! Überstaudauer
         end do ! alle Knoten
              
         do n=1,n_elemente ! alle Elemente
            if (cornernumber(n).eq.3)then
                write(ion,'(5(I8,2x))') & 
                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
            end if
            if (cornernumber(n).eq.4)then
               write(ion,'(6(I8,2x))') &
               n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
            end if
         end do ! alle Elemente
                
         close (ion)
         print*,'Überstaudauer 25-35 cm (46) ausgegeben auf: uedau25.grd'
!!!!!!!!!
         write(dateiname,'(2A)')trim(modellverzeichnis),'uedau35.grd'
         ion=107
         open ( unit =ion , file = dateiname, status ='unknown', action ='write ', iostat = open_error )
            if(open_error.ne.0) then
            print*,'uedau35.grd, open_error=',open_error
            close (ion)
            return
         end if ! open_error.ne.0
          
         write(ion,'(A)') 'Grid written by QSim3D'
         write(ion,'(I9,2x,I9)')n_elemente, knotenanzahl2D
         
         do n=1,knotenanzahl2D
            ! write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
            write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n),   &
     &      benthic_distribution(47+(n-1)*number_benth_distr)  !! Überstaudauer
         end do ! alle Knoten
              
         do n=1,n_elemente ! alle Elemente
            if (cornernumber(n).eq.3)then
                write(ion,'(5(I8,2x))') & 
                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
            end if
            if (cornernumber(n).eq.4)then
               write(ion,'(6(I8,2x))') &
               n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
            end if
         end do ! alle Elemente
                
         close (ion)
         print*,'Überstaudauer 35-undendl. cm (47) ausgegeben auf: uedau35.grd'
end if !! nur prozessor 0
         return
      END subroutine aus_grd
!----+-----+----

!> Kontrollausgabe des Netzes\n
!! \n\n
!! aus: ausgabe.f95 ; zurück: \ref Ergebnisse
      SUBROUTINE show_mesh()
      use modell                                                   
      implicit none
      character(len=longname) :: dateiname, systemaufruf
      integer n, ion, open_error, nel, ner, alloc_status,errcode
      real :: dummy

      !if(hydro_trieb.eq. 3) return ! SCHISM not available yet

if(meinrang.eq.0)then ! nur auf Prozessor 0 bearbeiten
!-------------------------------------------------------------------------------------------- nodes
      write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'mesh_node.vtk'
      if(errcode .ne. 0)call qerror('show_mesh writing filename mesh_node failed')
      write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
      if(errcode .ne. 0)call qerror('show_mesh writing system call rm -rf dateiname mesh_node failed')
      call system(systemaufruf)
      ion=106
      open ( unit =ion , file = dateiname, status ='new', action ='write ', iostat = open_error )
      if(open_error.ne.0) then
         write(fehler,*)'open_error mesh_node.vtk'
         call qerror(fehler)
      end if ! open_error.ne.0
      call mesh_output(ion)
      print*,'show_mesh:mesh_node.vtk done'
      close (ion)
!-------------------------------------------------------------------------------------------- edges=sides

      if(hydro_trieb.eq. 3)then ! schism
         write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'mesh_midedge.vtk'
         if(errcode .ne. 0)call qerror('show_mesh writing filename mesh_midedge failed')
         write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
         if(errcode .ne. 0)call qerror('show_mesh writing system call rm -rf dateiname mesh_midedge failed')
         call system(systemaufruf)
         open ( unit =ion , file = dateiname, status ='new', action ='write', iostat = open_error )
         if(open_error.ne.0) then
            write(fehler,*)'open_error mesh_midedge.vtk'
            call qerror(fehler)
         end if ! open_error.ne.0
		 print*,

         write(ion,'(A)')'# vtk DataFile Version 3.0'
         write(ion,'(A)')'Simlation QSim3D SCHISM'
         write(ion,'(A)')'ASCII'
         write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'

         write(ion,'(A)')' '
         write(ion,'(A,2x,I12,2x,A)')'POINTS ',kantenanzahl, ' float'
         do n=1,kantenanzahl
            write(ion,'(f17.5,2x,f17.5,2x,f8.3)') 0.5*(knoten_x(top_node(n))+knoten_x(bottom_node(n)))  &
                                                , 0.5*(knoten_y(top_node(n))+knoten_y(bottom_node(n))), 0.0
         end do ! alle kanten

         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl, kantenanzahl*2
         do n=1,kantenanzahl
            write(ion,'(A,2x,I12)')'1',n-1
         end do ! alle kanten

         write(ion,'(A)')' ' ! vtk-vertex
         write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl
         do n=1,kantenanzahl
            write(ion,'(A)')'1'
         end do ! alle kanten

         write(ion,'(A)')' '
         write(ion,'(A,2x,I12)')'POINT_DATA ', kantenanzahl
         write(ion,'(A)')'SCALARS length float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n=1,kantenanzahl
            write(ion,'(f27.6)') cell_bound_length(n) ! real(n) ! ed_area(n)
         end do ! alle kanten
        ! write(ion,'(A)')'SCALARS volume_flux float 1'
        ! write(ion,'(A)')'LOOKUP_TABLE default'
        ! do n=1,kantenanzahl
        !    write(ion,'(f27.6)') ed_flux(n)
        ! end do ! alle kanten
		dummy=0.0
         write(ion,'(A)')'VECTORS normal float'
         do n=1,kantenanzahl
            write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') edge_normal_x(n),edge_normal_y(n),dummy

         end do ! all edges/sides

         close (ion)
		 print*,'show_mesh:mesh_midedge.vtk schism done',kantenanzahl
		 
		 write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'mesh_side.vtk'
         if(errcode .ne. 0)call qerror('show_mesh writing filename mesh_side failed')
         write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
         if(errcode .ne. 0)call qerror('show_mesh writing system call rm -rf dateiname mesh_side failed')
         call system(systemaufruf)
         open ( unit =ion , file = dateiname, status ='new', action ='write', iostat = open_error )
         if(open_error.ne.0) then
            write(fehler,*)'open_error mesh_side.vtk'
            call qerror(fehler)
         end if ! open_error.ne.0
		 print*,

         write(ion,'(A)')'# vtk DataFile Version 3.0'
         write(ion,'(A)')'Simlation QSim3D SCHISM'
         write(ion,'(A)')'ASCII'
         write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'

         write(ion,'(A)')' '
         write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
         do n=1,knotenanzahl2D
		    write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
         end do ! alle kanten

         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl, kantenanzahl*3
         do n=1,kantenanzahl
            write(ion,'(A,2x,I12,2x,I12)')'2',top_node(n)-1,bottom_node(n)-1
         end do ! alle kanten

         write(ion,'(A)')' ' ! vtk-vertex
         write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl
         do n=1,kantenanzahl
            write(ion,'(A)')'3'
         end do ! alle kanten

         dummy=123.4
         write(ion,'(A)')' '
         write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
         write(ion,'(A)')'SCALARS dummy float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n=1,knotenanzahl2D
            write(ion,'(f27.6)') dummy ! real(n) ! ed_area(n)
         end do ! alle kanten
        ! write(ion,'(A)')'SCALARS volume_flux float 1'
        ! write(ion,'(A)')'LOOKUP_TABLE default'
        ! do n=1,kantenanzahl
        !    write(ion,'(f27.6)') ed_flux(n)
        ! end do ! alle kanten

         close (ion)
		 print*,'show_mesh:mesh_side.vtk schism done',kantenanzahl

	  end if ! schism

!-------------------------------------------------------------------------------------------- faces=elements
      kanten_vorhanden=.false. !! geht schief bei casu ????
      !! if(kanten_vorhanden)then
      if((hydro_trieb.eq. 2).or.(kanten_vorhanden))then ! untrim
      write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'mesh_element.vtk'
      if(errcode .ne. 0)call qerror('show_mesh writing filename mesh_element failed')
      write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
      if(errcode .ne. 0)call qerror('show_mesh writing system call rm -rf dateiname mesh_element failed')
      call system(systemaufruf)
      ion=106
      open ( unit =ion , file = dateiname, status ='unknown', action ='write ', iostat = open_error )
      if(open_error.ne.0) then
         write(fehler,*)'open_error mesh_element.vtk'
         call qerror(fehler)
      end if ! open_error.ne.0
      write(ion,'(A)')'# vtk DataFile Version 3.0'
      write(ion,'(A)')'mesh_element '
      write(ion,'(A)')'ASCII'
      write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'

      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',n_elemente+knotenanzahl2D, ' float'
      dummy=0.0
      do n=1,n_elemente
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') element_x(n), element_y(n), dummy
      end do ! all elements/faces
      do n=1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
      end do ! alle Knoten

      write(ion,'(A)')' ' 
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl, 3*kantenanzahl
      do n=1,kantenanzahl
         nel=left_element(n)
         ner=right_element(n)
         if(boundary_number(n) .gt. 0 ) then
            ner=n_elemente+top_node(n)
            nel=n_elemente+bottom_node(n)
         endif
         write(ion,'(A,2x,I8,2x,I8)')'2', nel-1, ner-1
      end do ! alle Knoten
      write(ion,'(A)')' ' 
      write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl
      do n=1,kantenanzahl
         write(ion,'(A)')'3'
      end do ! alle kanten

      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'POINT_DATA ', n_elemente+knotenanzahl2D
      write(ion,'(A)')'SCALARS boundary float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,n_elemente
         write(ion,'(f27.6)') real(element_rand(n))
      end do
      do n=1,knotenanzahl2D
         write(ion,'(f27.6)') real(knoten_rand(n))
      end do ! alle Knoten
      print*,'show_mesh: mesh_element.vtk done'

      close (ion)
      end if! edges
end if ! nur Prozessor 0 

      RETURN
      END subroutine show_mesh
!----+-----+----+-----+----+-----+----+-----+----+-----+----

      SUBROUTINE mesh_output(ion)
      use modell                                                   
      implicit none
      integer ion,n
      !print*,'mesh_output: starting'

      write(ion,'(A)')'# vtk DataFile Version 3.0'
      write(ion,'(A)')'Simlation QSim3D'
      write(ion,'(A)')'ASCII'
      !write(ion,'(A)')'DATASET POLYDATA'
      write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
!
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
      do n=1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
      end do ! alle Knoten

      if(element_vorhanden) then
         ! Elemente ausgeben (Knotennummern in paraview wieder von 0 beginnend !!
         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12,2x,I12)')'CELLS ', n_elemente, summ_ne
         do n=1,n_elemente ! alle Elemente
            if (cornernumber(n).eq.3)then
                write(ion,'(4(I8,2x))') & 
               cornernumber(n),elementnodes(n,1)-1,elementnodes(n,2)-1,elementnodes(n,3)-1
            end if
            if (cornernumber(n).eq.4)then
               write(ion,'(5(I8,2x))') &
               cornernumber(n),elementnodes(n,1)-1,elementnodes(n,2)-1,elementnodes(n,3)-1,elementnodes(n,4)-1
            end if
         end do ! alle Elemente

         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12)')'CELL_TYPES ', n_elemente
         do n=1,n_elemente ! alle Elemente
            if (cornernumber(n).eq.3)write(ion,'(A)') '5' 
            if (cornernumber(n).eq.4)write(ion,'(A)') '9' 
         end do ! alle Elemente
      else ! keine file.elements vorhanden
         ! Punkte als vtk-vertices
         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12,2x,I12)')'CELLS ', knotenanzahl2D, 2*knotenanzahl2D
         do n=1,knotenanzahl2D
            write(ion,'(A,2x,I8)')'1', n-1
         end do ! alle Knoten

         write(ion,'(A)')' ' 
         write(ion,'(A,2x,I12)')'CELL_TYPES ', knotenanzahl2D
         do n=1,knotenanzahl2D
            write(ion,'(A)')'1'
         end do ! alle Knoten
      end if !! element_vorhanden

      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
      write(ion,'(A)')'SCALARS Gelaendehoehe float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         write(ion,'(f27.6)') knoten_z(n)
      end do ! alle Knoten

      !write(ion,'(A)')'SCALARS zonen_nummer float 1'
      !write(ion,'(A)')'LOOKUP_TABLE default'
      !do n=1,knotenanzahl2D
      !   if(knoten_zone(n).eq. 0)call qerror('mesh_output: knoten_zone must not be zero')
      !   write(ion,'(f27.6)') real( zonen_nummer(knoten_zone(n)) )
      !end do ! alle Knoten

      write(ion,'(A)')'SCALARS knoten_zone float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         write(ion,'(f27.6)') real( knoten_zone(n) )
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS knoten_rand float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,knotenanzahl2D
         write(ion,'(f27.6)') real(knoten_rand(n))
      end do ! alle Knoten

      !print*,'mesh_output: finished'
      RETURN
      END subroutine mesh_output
!----+-----+----+-----+----+-----+----+-----+----+-----+----
!> raus ist true, wenn in diesem Zeitschritt das Geschwindigkeitsfeld ausgegeben werden soll \n\n 
!! \n\n
!      SUBROUTINE ausgabezeitpunkt(raus)
!      use modell                                                   
!      implicit none
!      logical :: raus                                          
!      raus=.TRUE.
!      return
!      END SUBROUTINE ausgabezeitpunkt

