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

subroutine schwebstoff_salz()
   use modell
   implicit none
   save
   
   integer :: i, j, n, nk, nvor, nach, nvor_alt, nach_alt, error
   real    :: a
   logical :: aenderung
   
   ! keine Schwebstoffvorgaben
   if (trueb_anzahl < 1) return 
   
   ! nur auf Prozessor 0 bearbeiten
   if (meinrang /= 0) return
   
   
   
   nvor = 0
   nach = 0
   do n = 1,trueb_anzahl,1
      if (trueb_zeit(trueb_zuord(n)) <= rechenzeit)nvor = n
      if (trueb_zeit(trueb_zuord(1+trueb_anzahl-n)) > rechenzeit)nach = 1+trueb_anzahl-n
   enddo
   
   aenderung = (nvor /= nvor_alt) .or. (nach /= nach_alt)
   
   if ((nvor > 0) .and. (nach <= 0)) then
      print*,'aktuelle rechenzeit nach Schwebstoffvorgabe-Zeitpunkt ',trueb_zeit(trueb_zuord(nvor))
      if (aenderung) then
         call verteilung_holen_gr3(trueb_datei(trueb_zuord(nvor)),vert1,knotenanzahl2D)
         print*,'geänderte Schwebstoffvorgabe geholt aus: ', trueb_datei(trueb_zuord(nvor))
      endif
   
      do i = 1,knotenanzahl2D ! Alle Knoten
         planktonic_variable(53+(i-1)*number_plankt_vari) = vert1(i)
      enddo 
   endif
   
   if ((nvor > 0) .and. (nach > 0)) then
      print*,'Schwebstoffvorgabe zwischen',trueb_zeit(trueb_zuord(nvor)),' und ', trueb_zeit(trueb_zuord(nach))
   
      if (aenderung) then
         call verteilung_holen_gr3(trueb_datei(trueb_zuord(nvor)),vert1,knotenanzahl2D)
         call verteilung_holen_gr3(trueb_datei(trueb_zuord(nach)),vert2,knotenanzahl2D)
         print*,'geänderte Schwebstoffvorgabe geholt aus: ', trueb_datei(trueb_zuord(nvor))  &
         ,' und ', trueb_datei(trueb_zuord(nach))
      endif
      
      a = real(trueb_zeit(trueb_zuord(nach))-trueb_zeit(trueb_zuord(nvor)))
      if (a <= 0.0)  call qerror('schwebstoff_salz trueb_zeit davor ist danach')
      
      a = real(rechenzeit-trueb_zeit(trueb_zuord(nvor)))/a
      
      print*,'schwebstoff_salz() anteil = ',a
      do i = 1,knotenanzahl2D ! Alle Knoten
         planktonic_variable(53+(i-1)*number_plankt_vari) = ((1.0-a)*vert1(i))+(a*vert2(i))
      enddo
   endif
   
   if ((nvor <= 0) .and. (nach > 0)) then
      print*,'aktuelle rechenzeit vor Schwebstoffvorgabe-Zeitpunkt', trueb_zeit(trueb_zuord(nach))
      if (aenderung) then
         call verteilung_holen_gr3(trueb_datei(trueb_zuord(nach)),vert2,knotenanzahl2D)
         print*,'geänderte Schwebstoffvorgabe geholt aus: ', trueb_datei(trueb_zuord(nach))
      endif
   
      do i = 1,knotenanzahl2D ! Alle Knoten
         planktonic_variable(53+(i-1)*number_plankt_vari) = vert2(i)
      enddo ! alle i Knoten
   endif
   
   if (aenderung) then
      nvor_alt = nvor
      nach_alt = nach
   endif
   
end subroutine schwebstoff_salz


!----+-----+----
!> die Subroutine ini_schwebstoff_salz() setzt die Anfangswerte für
!! Leitfähigkeit - planktonic_variable_name(65)= "                lf"
!! planktonic_variable_p(53+nk)= trueb(iglob) !  Schwebstoff\n
!! planktonic_variable_name(53)= "                ss"\n
!! planktonic_variable_name(65)= "                lf"\n
!! planktonic_variable_name(72)= "              salz"\n
!!
subroutine ini_schwebstoff_salz()
   use modell
   implicit none
   integer :: j, k
   
   if (meinrang /= 0) call qerror('ini_schwebstoff_salz() must only be called from processor 0.')
   
   print*,' ini_schwebstoff_salz() rechenzeit = ',rechenzeit
   call schwebstoff_salz()
   
   ! do k=1,number_plankt_point ! i
   !    planktonic_variable(53+(k-1)*number_plankt_vari) = 0.0  !  Schwebstoffgehalt ss
   !    planktonic_variable(65+(k-1)*number_plankt_vari) = 0.0  !  Leitfähigkeit lf
   !    planktonic_variable(72+(k-1)*number_plankt_vari) = 0.0  !  salz
   ! enddo
   
end subroutine ini_schwebstoff_salz

!> verteilung_holen_gr3 ließt aus unterverzeichnis trueb die angegebene Datei im gr3 (Elcirc) Format.
!! und gibt die dort angegebene Geländehöhe als verteilung zurück.
!! \n\n
!! Quelle schwebstoff_salz.f95 zurück zu \ref lnk_schwebstoff_salz
subroutine  verteilung_holen_gr3(datei,verteilung,anz)
   use modell
   real verteilung(*), dumm_x, dumm_y
   integer anz,i, ne, np, ion, sysa, dumm_n, errcode
   character (len = longname) :: datei, systemaufruf
   character (len = 600) :: dateipfad
   print*,'verteilung_holen_gr3: aus ',trim(datei)
   write(dateipfad,'(3A)',iostat = errcode)trim(modellverzeichnis),'trueb/',trim(datei)
   if (errcode /= 0)call qerror('verteilung_holen_gr3 writing system call failed')
   write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(dateipfad),' >/dev/null 2>/dev/null'
   if (errcode /= 0)call qerror('verteilung_holen_gr3 writing system call failed')
   call system(systemaufruf,sysa)
   if (sysa /= 0) then
      call qerror(' Keine Datei kein verteilung_holen_gr3 ')
   else
      print*,'Datei vorhanden'
   endif
   ion = 567
   open ( unit = ion , file = trim(dateipfad), status = 'old', action = 'read', iostat = sysa )
   if (sysa /= 0)call qerror('open_error dateipfad')
   rewind(ion)
   if (zeile(ion))continue ! print*,'1. Zeile:',trim(ctext)
   if (zeile(ion))continue ! print*,'2. Zeile:',trim(ctext)
   read(ctext, *, iostat = sysa ) ne, np
   if (np == anz) then
      do i = 1,anz ! Dummybelegung
         if (zeile(ion)) then
            read(ctext, *, iostat = sysa ) dumm_n, dumm_x, dumm_y, verteilung(i)
            ! print*,dumm_n,' verteilung(',i,')=', verteilung(i)
         else
            call qerror(' verteilung_holen_gr3: zeile nicht lesbar ???')
         endif
      enddo ! alle i Knoten
      print*,anz,' Werte gelesen'
   else
      call qerror(' verteilung_holen_gr3: Punktanzahl in Datei passt nicht zum aktuellen Modell')
   endif
   close (ion)
   do i = 1,anz ! Dummybelegung
      if (isnan(verteilung(i)))call qerror('isnan(verteilung(i)) ')
   enddo ! alle i Knoten
   return
end subroutine verteilung_holen_gr3
!----+-----+----
!> schwebstoff_salz_informationen sichten und sortieren - läuft nur auf Prozess 0\n
!! schaut, ob das Unterverzeichnis "trueb" vorhanden ist. Falls ja, wird in trueb nach
!! Dateien gesucht, die mit einem d beginnen. Die Zeichen hintem d im Dateinamen werden alz Zeitpunkt
!! in sekunden interpretiert. (analog wie in transinfo)
!! \n\n
!! Quelle schwebstoff_salz.f95 zurück zu \ref lnk_schwebstoff_salz
subroutine schwebstoff_salz_sichten()
   use modell
   implicit none
   character (len = longname) :: systemaufruf, filename, irgendeinstring
   integer sysa, nz, ion, n, is, i, zwischenwert, errcode
   if (meinrang /= 0)call qerror('schwebstoff_salz_sichten() darf nur auf prozess 0')
   write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'trueb >/dev/null 2>/dev/null'
   if (errcode /= 0)call qerror('schwebstoff_salz_sichten writing system call failed')
   call system(systemaufruf,sysa)
   if (sysa /= 0) then
      print*,'schwebstoff_salz: Kein Verzeichnis trueb, keine Vorgabe von Schwebstoffdaten'
      return
   else
      print*,'schwebstoff_salz: Vorgabe von Schwebstoffdaten im Verzeichnis trueb'
   endif
   print*,'schwebstoff_salz Vorgaben sichten ...'
   write(filename,'(2A)')trim(modellverzeichnis),'schwebe'
   if (errcode /= 0)call qerror('schwebstoff_salz_sichten writing system call failed')
   write(systemaufruf,'(4A)',iostat = errcode)'ls ',trim(modellverzeichnis),'trueb > ', trim(filename)
   if (errcode /= 0)call qerror('schwebstoff_salz_sichten writing system call failed')
   call system(trim(systemaufruf),sysa)
   if (sysa /= 0) then
      print*,trim(systemaufruf)
      call qerror('Auflisten der Schwebstoffvorgaben fehlgeschlagen.')
   endif
   ion = 444
   open ( unit = ion , file = filename, status = 'old', action = 'read', iostat = sysa )
   if (sysa /= 0)call qerror('open_error schwebe')
   rewind(ion)
   trueb_anzahl = 0
   nz = 0
   do while (zeile(ion))
      nz = nz+1
      if (ctext(1:1) == 'd')trueb_anzahl = trueb_anzahl+1
   enddo ! while Zeile
   print*,'schwebstoff_salz_sichten(): trueb_anzah = ',trueb_anzahl
   if (trueb_anzahl >= 1) then
      allocate (trueb_zeit(trueb_anzahl), stat = sysa )
      allocate (trueb_datei(trueb_anzahl), stat = sysa )
      allocate (trueb_zuord(trueb_anzahl), stat = sysa )
      rewind(ion)
      is = 0
      do n = 1,nz,1
         if ( .not. zeile(ion))call qerror('2 .not. zeile(ion)')
         if (ctext(1:1) == 'd') then
            is = is+1
            write(trueb_datei(is),'(A)')trim(ctext)
            i = len(trim(ctext))
            do while (ctext(i:i) /= 'd')
               i = i-1
            enddo ! while Zeile
            write(irgendeinstring,'(A)')ctext(i+1:len(trim(ctext)))
            !print*,'irgendeinstring:',trim(irgendeinstring)
            read(irgendeinstring,*)trueb_zeit(is)
            trueb_zuord(is) = is
         endif !! alle s* Dateien
      enddo ! alle zeilen aus schwebe
      close(ion)
      do n = 1,trueb_anzahl,1
         do i = n+1,trueb_anzahl,1
            if (trueb_zeit(trueb_zuord(n)) > trueb_zeit(trueb_zuord(i))) then ! tauschen
               zwischenwert = trueb_zuord(n)
               trueb_zuord(n) = trueb_zuord(i)
               trueb_zuord(i) = zwischenwert
            endif ! Zeitreihenfolge falsch
         enddo ! alle weiteren i durch
      enddo ! alle n durch
      do n = 1,trueb_anzahl,1
         print*,'schwebe ',n,'  zeit = ',trueb_zeit(trueb_zuord(n)), &
                                       '  Datei:', trim(trueb_datei(trueb_zuord(n)))
      enddo ! alle n durch
      allocate (vert1(knotenanzahl2D), stat = sysa )
      if (sysa /= 0) then
         write(fehler,*)' Rueckgabewert von allocate (vert1(knotenanzahl2D) :', sysa
         call qerror(fehler)
      endif
      allocate (vert2(knotenanzahl2D), stat = sysa )
      if (sysa /= 0) then
         write(fehler,*)' Rueckgabewert von allocate (vert2(knotenanzahl2D) :', sysa
         call qerror(fehler)
      endif
   endif ! Schwebstoffvorgaben vorhanden
   return
end subroutine schwebstoff_salz_sichten
!----+-----+----
!> schwebstoff_salz auf alle Prozessoren verteilen
!! \n\n
subroutine schwebstoff_salz_parallel()
   use modell
   implicit none
   integer alloc_status
   !allocate (trueb(knotenanzahl2D), stat = alloc_status )
   !if(alloc_status.ne.0)then
   !   write(fehler,*)' Rueckgabewert von allocate (trueb(knotenanzahl2D) :', alloc_status
   !   call qerror(fehler)
   !endif
   !allocate (salz(knotenanzahl2D), stat = alloc_status )
   !if(alloc_status.ne.0)then
   !   write(fehler,*)' Rueckgabewert von allocate (salz(knotenanzahl2D) :', alloc_status
   !   call qerror(fehler)
   !endif
   !print*,'schwebstoff_salz_parallel , trueb und salz allociert', meinrang
end subroutine schwebstoff_salz_parallel
