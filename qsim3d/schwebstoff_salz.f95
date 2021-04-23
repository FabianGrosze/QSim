
!> \page schwebstoff_salz Einlesen von Verteilungen (Schwebstoff, Salz)
!! Einige Eigenschaften des Wassers wie der Salzgehalt, die Konzentration an suspendierten Sedimenten
!! und auch die Temperatur verändern dessen Dichte so nennenswert, dass sie sich auf die Strömung auswirken können
!! und werden dann auch bereits in der hydraulischen Simulation berücksichtigt.
!! \n\n
!! Es macht bei der Gewässergütesimulation mit QSim3d, die sich ja ohnehin auf die von einem hydraulischen Treiber 
!! offline abgelegten Transportinformationen stützt, Sinn, auch dessen Schwebstoff- oder Salz-Gehalt zu übernehmen,
!! falls der dort berechnet wurde. Für Temperaturen ist solch eine Übernahme z. Z. noch nicht geplant.
!! \n\n
!! \section trueb Schwebstoffkonzentrationen
!! Bisher realisiert ist die Übernahme von Schwebstoffkonzentrationen:
!! Subroutine schwebstoff_salz_sichten() stellt fest ob ein Unterverzeichnis ./trueb im aktuellen Modellverzeichnis existiert.
!! Ist keines vorhanden, werden auch keine Schwebstoffverteilungen eingelesen. 
!! \n\n
!! Im Unterverzeichnis trueb wird nach mit d beginnenden Dateien gesucht (z.B. ./trueb/d86400 ).
!! Die Zahl hinter dem Buchstaben d im Dateinamen wird als Zeit in ganzen Sekunden nach Berechnungsbeginn interpretiert.
!! Die Subroutine schwebstoff_salz() interpoliert dann die Schwebstoffverteilung für den aktuellen Rechenzeitpunkt (stoffumsatz-Zeitschritte).
!! \n\n
!! Die Schwebstoffdateien müssen im Elcirc .gr3 Format vorliegen. 
!! Dieses Format dient auch zur Definition der Berechnungsnetze von SCHISM.
!! Dort wo in den Netzdateien die Knotenhöhe stehen, muss nun die Schwebstoffkonzentration in ??? mg/l ??? angegeben werden. 
!! Dadurch ist es möglich, die Zuordnung von Schwebstoffverteilungen mit den selben Werkzeugen (z.B. Netzgenerator Janet)
!! vorzunehmen, mit denen auch die Höhenzuordnungen des Berechnungsnetzes erstellt wurden.
!! \n\n
!! Die vorgegebene Schwebstoffkonzentration wird  in die Variable 
!! \ref ss eingelesen und repräsentiert nur den ??? zusätzlichen ??? Schwebstoffanteil.
!! Das \ref Licht_algen , das den \ref lnk_algendoc für die Photosynthese zur Verfügung steht, wird noch durch weitere Wasserinhaltsstoffe abgeschwächt.
!! \n\n
!! \section sali Salzkonzentrationen
!! z.Z. noch nicht realisiert.
!! \n\n
!! Quelle schwebstoff_salz.f95 zurück zu \ref lnk_ueberblick

!----+-----+----

!> die SUBROUTINE schwebstoff_salz() belegt die \ref tiefengemittelte_planktische_variable 53 planktonic_variable(53 ,
!! QSim1D-Name "ss", in der die tiefengemittelte Schwebstoffverteilung in ??? gespeichert wird.
!! Abhängig vom rechenzeit-Punkt werden die Verteilungen mit der subroutine verteilung_holen_gr3() aus den 
!! d-Dateien im Unterverszeichnis trueb geholt. Zwischen den in trueb vorhandenen Zeitpunkten wird interpoliert.
!! vor dem ersten und nach dem letzten wird die erste, resp. letzte Verteilung genommen.
!! \n\n
!! Quelle schwebstoff_salz.f95 zurück zu \ref schwebstoff_salz oder \ref lnk_ueberblick
      SUBROUTINE schwebstoff_salz()
      use modell
      implicit none
      save
      integer :: i,j,n, nk, nvor, nach, nvor_alt, nach_alt, error
      real a
      logical aenderung

      if(trueb_anzahl.lt. 1)return ! keine Schwebstoffvorgaben ...

      !print*,'schwebstoff_salz() momentan=',rechenzeit,' startet.', meinrang

      !call mpi_barrier (mpi_komm_welt, ierr)
if(meinrang.eq.0)then ! nur auf Prozessor 0 bearbeiten
      nvor=0
      nach=0
      do n=1,trueb_anzahl,1
         if(trueb_zeit(trueb_zuord(n)).le.rechenzeit)nvor=n
         if(trueb_zeit(trueb_zuord(1+trueb_anzahl-n)).gt.rechenzeit)nach=1+trueb_anzahl-n
      end do ! alle n durch
      aenderung=(nvor.ne.nvor_alt).or.(nach.ne.nach_alt)
      
      if((nvor.gt. 0).and.(nach.le. 0))then
         print*,'aktuelle rechenzeit nach Schwebstoffvorgabe-Zeitpunkt ',trueb_zeit(trueb_zuord(nvor))
         if(aenderung)then
            call verteilung_holen_gr3(trueb_datei(trueb_zuord(nvor)),vert1,knotenanzahl2D)
            print*,'geänderte Schwebstoffvorgabe geholt aus: ', trueb_datei(trueb_zuord(nvor))
         endif
         do i=1,knotenanzahl2D ! Alle Knoten
            planktonic_variable(53+(i-1)*number_plankt_vari)=vert1(i)
         end do ! alle i Knoten
      end if
      if((nvor.gt. 0).and.(nach.gt. 0))then
         print*,'Schwebstoffvorgabe zwischen',trueb_zeit(trueb_zuord(nvor)),' und ', trueb_zeit(trueb_zuord(nach))
         if(aenderung)then
            call verteilung_holen_gr3(trueb_datei(trueb_zuord(nvor)),vert1,knotenanzahl2D)
            call verteilung_holen_gr3(trueb_datei(trueb_zuord(nach)),vert2,knotenanzahl2D)
            print*,'geänderte Schwebstoffvorgabe geholt aus: ', trueb_datei(trueb_zuord(nvor))  &
                                                    ,' und ', trueb_datei(trueb_zuord(nach))
         endif
         a=real(trueb_zeit(trueb_zuord(nach))-trueb_zeit(trueb_zuord(nvor)))
         if(a.gt.0.0)then
            a=real(rechenzeit-trueb_zeit(trueb_zuord(nvor)))/a
         else
            call qerror('schwebstoff_salz trueb_zeit davor ist danach')
         endif
         print*,'schwebstoff_salz() anteil=',a
         do i=1,knotenanzahl2D ! Alle Knoten
            planktonic_variable(53+(i-1)*number_plankt_vari)=((1.0-a)*vert1(i))+(a*vert2(i))
         end do ! alle i Knoten
      end if
      if((nvor.le. 0).and.(nach.gt. 0))then
         print*,'aktuelle rechenzeit vor Schwebstoffvorgabe-Zeitpunkt', trueb_zeit(trueb_zuord(nach))
         if(aenderung)then
            call verteilung_holen_gr3(trueb_datei(trueb_zuord(nach)),vert2,knotenanzahl2D)
            print*,'geänderte Schwebstoffvorgabe geholt aus: ', trueb_datei(trueb_zuord(nach))
         endif
         do i=1,knotenanzahl2D ! Alle Knoten
            planktonic_variable(53+(i-1)*number_plankt_vari)=vert2(i)
         end do ! alle i Knoten
      end if

      if(aenderung)then
         nvor_alt=nvor
         nach_alt=nach
      endif

end if ! nur auf Prozessor 0 bearbeiten

      RETURN
      END subroutine schwebstoff_salz

!----+-----+----
!> die Subroutine ini_schwebstoff_salz() setzt die Anfangswerte für
!! Leitfähigkeit - planktonic_variable_name(65)= "                lf"\n
!!          planktonic_variable_p(53+nk)= trueb(iglob) !  Schwebstoff\n
!!          planktonic_variable_name(53)= "                ss"\n
!!          planktonic_variable_name(65)= "                lf"\n
 !!         planktonic_variable_name(72)= "              salz"\n
!! 
!! aus Datei initialisieren.f95 ; zurück: \ref Anfangsbedingungen\n\n
      SUBROUTINE ini_schwebstoff_salz()
      use modell
      implicit none
      integer :: j, k

      if(meinrang.ne.0)call qerror('ini_schwebstoff_salz() darf nur auf prozess 0')

      print*,' ini_schwebstoff_salz() rechenzeit=',rechenzeit
      call schwebstoff_salz()

      !do k=1,number_plankt_point ! i
      !   planktonic_variable(53+(k-1)*number_plankt_vari) = 0.0  !  Schwebstoffgehalt ss
      !   planktonic_variable(65+(k-1)*number_plankt_vari) = 0.0  !  Leitfähigkeit lf
      !   planktonic_variable(72+(k-1)*number_plankt_vari) = 0.0  !  salz
      !end do

      RETURN 
      END subroutine ini_schwebstoff_salz

!> verteilung_holen_gr3 ließt aus unterverzeichnis trueb die angegebene Datei im gr3 (Elcirc) Format.
!! und gibt die dort angegebene Geländehöhe als verteilung zurück. 
!! \n\n
!! Quelle schwebstoff_salz.f95 zurück zu \ref schwebstoff_salz
      SUBROUTINE  verteilung_holen_gr3(datei,verteilung,anz)
      use modell
      real verteilung(*), dumm_x, dumm_y
      integer anz,i, ne, np, ion, sysa, dumm_n, errcode
      character (len=longname) :: datei, systemaufruf
      character (len=600) :: dateipfad

      print*,'verteilung_holen_gr3: aus ',trim(datei)
      write(dateipfad,'(3A)',iostat = errcode)trim(modellverzeichnis),'trueb/',trim(datei)
      if(errcode .ne. 0)call qerror('verteilung_holen_gr3 writing system call failed')
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(dateipfad),' >/dev/null 2>/dev/null'
      if(errcode .ne. 0)call qerror('verteilung_holen_gr3 writing system call failed')
      call system(systemaufruf,sysa)
      if(sysa.ne.0)then
         call qerror(' Keine Datei kein verteilung_holen_gr3 ')
      else
         print*,'Datei vorhanden'
      end if

      ion=567
      open ( unit =ion , file = trim(dateipfad), status ='old', action ='read', iostat = sysa )
      if(sysa.ne.0)call qerror('open_error dateipfad')
      rewind(ion)
      if(zeile(ion))continue ! print*,'1. Zeile:',trim(ctext)
      if(zeile(ion))continue ! print*,'2. Zeile:',trim(ctext)
      read(ctext, *, iostat = sysa ) ne, np
      if(np.eq.anz)then
         do i=1,anz ! Dummybelegung
            if(zeile(ion))then
               read(ctext, *, iostat = sysa ) dumm_n, dumm_x, dumm_y, verteilung(i)
               ! print*,dumm_n,' verteilung(',i,')=', verteilung(i)
            else
               call qerror(' verteilung_holen_gr3: zeile nicht lesbar ???')
            endif
         end do ! alle i Knoten
         print*,anz,' Werte gelesen'
      else
         call qerror(' verteilung_holen_gr3: Punktanzahl in Datei passt nicht zum aktuellen Modell')
      endif

      close (ion)

      do i=1,anz ! Dummybelegung
         if(isnan(verteilung(i)))call qerror('isnan(verteilung(i)) ')
      end do ! alle i Knoten

      RETURN
      end subroutine verteilung_holen_gr3
!----+-----+----

!> schwebstoff_salz_informationen sichten und sortieren - läuft nur auf Prozess 0\n
!! schaut, ob das Unterverzeichnis "trueb" vorhanden ist. Falls ja, wird in trueb nach
!! Dateien gesucht, die mit einem d beginnen. Die Zeichen hintem d im Dateinamen werden alz Zeitpunkt
!! in sekunden interpretiert. (analog wie in transinfo)
!! \n\n
!! Quelle schwebstoff_salz.f95 zurück zu \ref schwebstoff_salz
      subroutine schwebstoff_salz_sichten()
      use modell   
      implicit none
      character (len=longname) :: systemaufruf, dateiname, irgendeinstring
      integer sysa, nz, ion, n, is, i, zwischenwert, errcode

      if(meinrang.ne.0)call qerror('schwebstoff_salz_sichten() darf nur auf prozess 0')

      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(modellverzeichnis),'trueb >/dev/null 2>/dev/null'
      if(errcode .ne. 0)call qerror('schwebstoff_salz_sichten writing system call failed')
      call system(systemaufruf,sysa)
      if(sysa.ne.0)then
         print*,'schwebstoff_salz: Kein Verzeichnis trueb, keine Vorgabe von Schwebstoffdaten'
         return
      else
         print*,'schwebstoff_salz: Vorgabe von Schwebstoffdaten im Verzeichnis trueb'
      end if

      print*,'schwebstoff_salz Vorgaben sichten ...'
      write(dateiname,'(2A)')trim(modellverzeichnis),'schwebe'
      if(errcode .ne. 0)call qerror('schwebstoff_salz_sichten writing system call failed')
      write(systemaufruf,'(4A)',iostat = errcode)'ls ',trim(modellverzeichnis),'trueb >', trim(dateiname)
      if(errcode .ne. 0)call qerror('schwebstoff_salz_sichten writing system call failed')
      call system(trim(systemaufruf),sysa)
      if(sysa.ne.0) then
         print*,trim(systemaufruf)
         call qerror('Auflisten der Schwebstoffvorgaben fehlgeschlagen.')
      end if 
      ion=444
      open ( unit =ion , file = dateiname, status ='old', action ='read', iostat = sysa )
      if(sysa.ne.0)call qerror('open_error schwebe')
      rewind(ion)
      trueb_anzahl=0
      nz=0
      do while (zeile(ion))
         nz=nz+1
         if(ctext(1:1).eq.'d')trueb_anzahl=trueb_anzahl+1
      end do ! while Zeile
      print*,'schwebstoff_salz_sichten(): trueb_anzah=',trueb_anzahl

      if(trueb_anzahl.ge. 1)then
         allocate (trueb_zeit(trueb_anzahl), stat = sysa )
         allocate (trueb_datei(trueb_anzahl), stat = sysa )
         allocate (trueb_zuord(trueb_anzahl), stat = sysa )

         rewind(ion)
         is=0
         do n=1,nz,1
            if(.not.zeile(ion))call qerror('2 .not.zeile(ion)')
            if(ctext(1:1).eq.'d')then
               is=is+1
               write(trueb_datei(is),'(A)')trim(ctext)
               i=len(trim(ctext))
               do while (ctext(i:i).ne.'d')
                  i=i-1
               end do ! while Zeile
               write(irgendeinstring,'(A)')ctext(i+1:len(trim(ctext)))
               !print*,'irgendeinstring:',trim(irgendeinstring)
               read(irgendeinstring,*)trueb_zeit(is)
               trueb_zuord(is)=is
            end if !! alle s* Dateien
         end do ! alle zeilen aus schwebe
         close(ion)

         do n=1,trueb_anzahl,1
            do i=n+1,trueb_anzahl,1
               if(trueb_zeit(trueb_zuord(n)).gt.trueb_zeit(trueb_zuord(i)))then ! tauschen
                  zwischenwert=trueb_zuord(n)
                  trueb_zuord(n)=trueb_zuord(i)
                  trueb_zuord(i)=zwischenwert
               end if ! Zeitreihenfolge falsch
            end do ! alle weiteren i durch
         end do ! alle n durch

         do n=1,trueb_anzahl,1
            print*,'schwebe ',n,'  zeit=',trueb_zeit(trueb_zuord(n)), &
                   '  Datei:', trim(trueb_datei(trueb_zuord(n)))
         end do ! alle n durch

      allocate (vert1(knotenanzahl2D), stat = sysa )
      if(sysa.ne.0)then
         write(fehler,*)' Rueckgabewert von allocate (vert1(knotenanzahl2D) :', sysa
         call qerror(fehler)
      end if
      allocate (vert2(knotenanzahl2D), stat = sysa )
      if(sysa.ne.0)then
         write(fehler,*)' Rueckgabewert von allocate (vert2(knotenanzahl2D) :', sysa
         call qerror(fehler)
      end if

      endif ! Schwebstoffvorgaben vorhanden

      RETURN
      END subroutine schwebstoff_salz_sichten
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
      !end if

      !allocate (salz(knotenanzahl2D), stat = alloc_status )
      !if(alloc_status.ne.0)then
      !   write(fehler,*)' Rueckgabewert von allocate (salz(knotenanzahl2D) :', alloc_status
      !   call qerror(fehler)
      !end if

      !print*,'schwebstoff_salz_parallel , trueb und salz allociert', meinrang
      END subroutine schwebstoff_salz_parallel

