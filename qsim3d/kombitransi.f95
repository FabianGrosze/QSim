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
!> \page kombitransi Zusammenfügen von Teilzeiträumen
!! Das Programm >>kombi<<
!! dient der Kombination von Transinfo-Verzeichnissen
!! \n\n
!! Wenn ein längerer Zeitabschnitt (z.B. ein Jahresgang)
!! für die hydraulische Simulation in Teile (Zeitabschnitte) zerlegt wurde,
!! um ihn auf mehreren Prozessoren parallel rechnen zu können,
!! ist es nach Abschluss aller Teil-Rechenläufe erforderlich, die
!! Transportinformationen für die Gütesimulation mit QSim-3D in ein Verzeichnis zusammenzuführen,
!! so dass ein durchgängiger Jahresgang entsteht.
!! \n\n
!! Dies ist möglich, weil das Erinnerungsvermögen von Impuls und Wasserstand, die vom hydraulischen Treiber simuliert werden,
!! in einem Ästuar kaum läger zurückreicht als einen Tag.
!! Konzentrationsverteilungen, die im Gütemodell simuliert werden, haben in einem Ästuar
!! mit mehreren Monaten Wasseraufenthaltszeit ein viel längeres Gedächtnis.
!! \n\n
!! Quelle: kombitransi.f95 zurück zu \ref Transportinformationen
program kombitransi
   implicit none
   integer :: dttrans, start, ende, transinfo_anzahl, sysa, i
   integer :: sammel_start, sammel_ende, sammel_dt, errcode
   character (len = longname) :: aufrufargument, modellverzeichnis, systemaufruf, sammelverzeichnis
   logical :: anschluss
   print*,"kombi startet"
   sammel_start = -1
   sammel_ende = -1
   anschluss = .false.
   print*,'Bitte Verzeichnis für die Sammlung der Transportinformationen eingeben:'
   read(*,"(A)")sammelverzeichnis
   !print*,trim(sammelverzeichnis)
   !call get_command_argument(1, aufrufargument)
   !write(sammelverzeichnis,'(A)')trim(aufrufargument)
   write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(sammelverzeichnis),' > /dev/null 2 > /dev/null'
   if (errcode /= 0)call qerror('kombitransi writing system call stat failed')
   call system(systemaufruf,sysa)
   if (sysa /= 0) then
      print*,"angegebenes Sammel-Verzeichnis",trim(sammelverzeichnis)," existiert nicht."
      stop
   end if
   call transinfo_sichten(sammelverzeichnis, dttrans, start, ende)
   transinfo_anzahl = 1+((ende-start)/dttrans)
   print*,"sammelverzeichnis: ", transinfo_anzahl,' Transport-Zeitschritte von:',  &
   start, ' bis', ende, 'in regelmäßigen Schritten von  ',dttrans, ' Sekunden'
   if (ende > 0) then ! Transportinformationen im Sammelverzeichnis enthalten?
      sammel_start = start
      sammel_ende = ende
      sammel_dt = dttrans
   end if !
   !stop ! test
   do ! nächstes Verzeichnis zum Kombinieren
      print*,'Bitte das nächste Verzeichnis zum Kombinieren der Transportinformationen eingeben:'
      print*,"(Die Angabe eines nicht vorhandenen Namens führt zum Abbruch des Kombinationsvorgangs)"
      read(*,"(A)")modellverzeichnis
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),' > /dev/null 2 > /dev/null'
      if (errcode /= 0)call qerror('kombitransi writing system call stat2 failed')
      call system(systemaufruf,sysa)
      if (sysa /= 0) then
         print*,"angegebenes Verzeichnis",trim(modellverzeichnis)," existiert nicht."
         exit
      end if
      call transinfo_sichten(modellverzeichnis, dttrans, start, ende)
      transinfo_anzahl = 1+((ende-start)/dttrans)
      print*, transinfo_anzahl,' Transport-Zeitschritte von:',  &
      start, ' bis', ende, 'in regelmäßigen Schritten von  ',dttrans, ' Sekunden'
      if (sammel_ende < 0) then ! erster zeitraum
         sammel_start = start
         sammel_ende = start
         sammel_dt = dttrans
      else !! weitere zeiträume
         if (sammel_ende < start+86400) then
            print*,"Anschluss mit mehr als einem Tag Überlappung nicht gewährleistet. - > Abbruch"
            exit
         end if
         if (ende <= sammel_ende) then
            print*,"Das Verzeichnis enthält keine Fortsetzung. - > Abbruch"
            exit
         end if
         if (sammel_dt /= dttrans) then
            print*,"Zeitschrittweite inkompatibel. - > Abbruch"
            exit
         endif
         if (mod(sammel_ende,sammel_dt) /= mod(ende,sammel_dt)) then
            print*,"Zeitschritt-Raster nicht fortlaufend. - > Abbruch"
            exit
         endif
      end if ! weitere Zeiträume
      print*,"Kopiervorgang startet und kann etwas dauern ...."
      do i = sammel_ende,ende,dttrans !! brauchbare zeitpunkte kopieren
         !trans_write.c:   sprintf(text,"%s/transinfo/t%09d",dirname,itime);
         write(systemaufruf,'(3A,I9.9,x,2A)',iostat = errcode)  &
                                                      'cp ',trim(modellverzeichnis),'transinfo/t',i,trim(sammelverzeichnis),'transinfo/'
         if (errcode /= 0)call qerror('kombitransi writing system call cp failed')
         call system(systemaufruf,sysa)
         if (sysa /= 0) then
            print*,"Kopieren fehlgeschlagen: ",trim(systemaufruf)
            stop
         end if
      end do ! alle i brauchbaren Zeitschritte
      print*,"es wurden die Zeitpunkte",sammel_ende," bis ", ende," kopiert"
      sammel_ende = ende
   end do ! alle weiteren Verzeichnisse
   print*,"es wurden die ",((sammel_ende-sammel_start)/sammel_dt)+1," Zeitpunkte von "  &
   ,sammel_start," bis ",sammel_ende," zusammengetragen."
   print*,"kombi endet regulär"
   stop
end program kombitransi
!----+-----+----
subroutine transinfo_sichten(modellverzeichnis, dttrans, start, ende)
   implicit none
   integer :: dttrans, start, ende
   integer :: i, n, ion, nz, nt, transinfo_anzahl
   integer :: sysa, open_error, io_error, system_error, alloc_status
   integer :: delt, zwischenwert,errcode
   integer :: tag, monat, jahr, stunde, minute, sekunde
   logical :: naechste_zeile
   character (len = longname) :: aufrufargument, systemaufruf, modellverzeichnis, dateiname, irgendeinstring
   character (len = 2000) :: ctext
   character(250) , allocatable , dimension (:) :: transinfo_datei
   integer , allocatable , dimension (:) :: transinfo_zeit
   integer , allocatable , dimension (:) :: transinfo_zuord
   write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),' > /dev/null 2 > /dev/null'
   if (errcode /= 0)call qerror('transinfo_sichten writing system call stat failed')
   call system(systemaufruf,sysa)
   !print*,'sysa',sysa
   if (sysa /= 0) then
      print*,"angegebenes modellverzeichnis",trim(modellverzeichnis)," existiert nicht."
      stop
   else
      print*,'Sichten von modellverzeichnis: > ', trim(modellverzeichnis)
   end if ! io_error.ne.0
   !print*,'Transportinformationen sichten ...'
   write(dateiname,'(2A)')trim(modellverzeichnis),'trafo'
   write(systemaufruf,'(4A)',iostat = errcode)'ls ',trim(modellverzeichnis),'transinfo > ', trim(dateiname)
   if (errcode /= 0)call qerror('transinfo_sichten writing system call ls failed')
   !print*,systemaufruf
   call system(trim(systemaufruf),system_error)
   if (system_error /= 0) then
      print*,trim(systemaufruf)
      print*,'Auflisten der Transportinformationen fehlgeschlagen.'
      stop
   end if ! io_error.ne.0
   ion = 333
   open ( unit = ion , file = dateiname, status = 'old', action = 'read', iostat = open_error )
   if (open_error /= 0) then
      print*,'open_error trafo'
      stop
   end if
   ! call system("tail trafo",system_error)
   nz = 0
   transinfo_anzahl = 0
   do while (naechste_zeile(ion,ctext))
      nz = nz+1
      if (ctext(1:1) == 't')transinfo_anzahl = transinfo_anzahl+1
   end do ! while Zeile
   if (transinfo_anzahl < 1) then
      print*,'keine Zeitpunkte, weitermachen sinnlos'
      stop
   else
      print*,trim(modellverzeichnis),'transinfo enthält ',nz," Dateien, davon sind "  &
      ,transinfo_anzahl,' transport-informations-zeitpunkte.'
   end if
   allocate (transinfo_zeit(transinfo_anzahl), stat = alloc_status )
   allocate (transinfo_datei(transinfo_anzahl), stat = alloc_status )
   allocate (transinfo_zuord(transinfo_anzahl), stat = alloc_status )
   rewind(ion)
   nt = 0
   do n = 1,nz,1
      if ( .not. naechste_zeile(ion,ctext)) then
         print*,'lesen 2 trafo fehlgeschlagen'
         stop
      end if
      !write(*,*)trim(ctext)
      if (ctext(1:1) == 't') then
         nt = nt+1
         write(transinfo_datei(nt),'(A)')trim(ctext)
         i = len(trim(ctext))
         do while (ctext(i:i) /= 't')
            i = i-1
         end do ! while Zeile
         write(irgendeinstring,'(A)')ctext(i+1:len(trim(ctext)))
         !print*,'irgendeinstring:',trim(irgendeinstring)
         read(irgendeinstring,*)transinfo_zeit(nt)
         transinfo_zuord(nt) = nt
         !print*,"transinfo   zuord=", transinfo_zuord(n), '  transinfo_zeit=',transinfo_zeit(n), &
         !       '  Datei:', trim(transinfo_datei(n))
      end if !! alle t* Dateien
   end do ! alle zeilen aus trafo
   close(ion)
   write(dateiname,'(2A)')trim(modellverzeichnis),'transinfo/meta'
   open ( unit = ion , file = dateiname, status = 'old', action = 'read', iostat = open_error )
   if (open_error /= 0) then
      print*,'open_error transinfo/meta'
      stop
   end if
   if ( .not. naechste_zeile(ion,ctext)) then
      print*,'deltat-zeile aus transinfo/meta nicht lesbar'
      stop
   else
      read(ctext,*,iostat = io_error) dttrans
      if (io_error /= 0) then
         print*,'Transportinfo-Zeitschritt in /transinfo/meta nicht lesbar',io_error
         stop
         !else
         !   print*,"dttrans(meta)=",dttrans
      endif ! Lesen fehlgeschlagen
   end if ! keine Zeile
   if ( .not. naechste_zeile(ion,ctext)) then
      print*,'Zeitursprung in transinfo/meta nicht vorhanden'
   else
      read(ctext,*,iostat = io_error) tag, monat, jahr, stunde, minute, sekunde
      print*,"meta-zeit-offset = ",tag, monat, jahr, stunde, minute, sekunde
   end if
   !! transinfo-Dateien in aufsteigende Reihenfolge bringen
   do n = 1,transinfo_anzahl,1
      do i = n+1,transinfo_anzahl,1
         if (transinfo_zeit(transinfo_zuord(n)) > transinfo_zeit(transinfo_zuord(i))) then ! tauschen
            zwischenwert = transinfo_zuord(n)
            transinfo_zuord(n) = transinfo_zuord(i)
            transinfo_zuord(i) = zwischenwert
         end if ! Zeitreihenfolge falsch
      end do ! alle weiteren i durch
   end do ! alle n durch
   !! Zeitschritt prüfen
   do n = 2,transinfo_anzahl,1
      delt = transinfo_zeit(transinfo_zuord(n))-transinfo_zeit(transinfo_zuord(n-1))
      if (delt /= dttrans) then
         print*,' ERROR unregelmäßiger Transportzeitschritt ',delt, 'sollte sein: ', dttrans &
         ,' n = ', n,trim(transinfo_datei(transinfo_zuord(n)))
         stop
      end if ! mehr als ein Transportzeitschritt
   end do ! alle Transportzeitschritte ab 2
   !! Rückgabewerte
   start = transinfo_zeit(transinfo_zuord(1))
   ende = transinfo_zeit(transinfo_zuord(transinfo_anzahl))
   deallocate (transinfo_zeit)
   deallocate (transinfo_datei)
   deallocate (transinfo_zuord)
   return
end subroutine transinfo_sichten
!----+-----+----
!> ließt bis zur nächsten zeile, die kein Kommentar und keine Leerzeile ist \n
!! Ergebnis in ctext \n
!! \n\n
logical function naechste_zeile(ion,ctext)
   implicit none
   integer :: ion, i, io_error
   character (len = 2000) :: ctext
   logical naechste, leerzeile, kommentar
   naechste = .true.
   naechste_zeile = .false.
   do while ( naechste ) !! nächste nicht Kommentar- und nicht Leer-Zeile auffinden:
      read(ion, '(A)', iostat = io_error ) ctext  ! eine Zeile einlesen
      if (io_error /= 0) then ! keine Zeile mehr vorhanden.
         !!print*,'io_error SUBROUTINE zeile'
         naechste_zeile = .false.
         naechste = .false.
         return ! Rückgabe: keine nächste zeile
      end if ! io_error.ne.0
      kommentar = .false.
      if (ctext(1:1) == '#') kommentar = .true. ! Kommentarzeile, nächste Zeile probieren
      leerzeile = .true.
      do i = 1,len(trim(ctext))
         if (ctext(i:i) /= " ")leerzeile = .false.
      end do ! alle zeichen in ctext
      if (( .not. leerzeile) .and. ( .not. kommentar)) then
         naechste_zeile = .true. ! nächste Zeile gefunden
         naechste = .false. !keine weiteren einlesen
         return
      end if ! keine Leerzeile
   end do ! nächste nicht Kommentar Zeile gefunden
   return
end function naechste_zeile
