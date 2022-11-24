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
!----+-----+----
!> für einen gegebenes Zeitintervall ermitteln welche Transportinformationsschritte es abdecken.
!! \n\n
subroutine transinfo_schritte(start_zeitschritt, ende_zeitschritt)
   use modell
   implicit none
   integer :: start_zeitschritt, ende_zeitschritt
   integer :: n, deti
   logical :: gefunden
   if (ende_zeitschritt < start_zeitschritt) then
      write(fehler,*)'transinfo_schritte: Zeitintervall ' &
      ,start_zeitschritt, ' bis ', ende_zeitschritt,' geht nicht'
      call qerror(fehler)
   end if
   gefunden = .false.
   na_transinfo = -1
   ne_transinfo = -1
   do n = 1,transinfo_anzahl
      if ((transinfo_zeit(transinfo_zuord(n)) >= start_zeitschritt) .and. ( .not. gefunden)) then
         na_transinfo = n
         gefunden = .true.
         !print*,'na_transinfo,n ',na_transinfo,n
         !print*,'Strat Hydraulik Zeitschritt in diesem Qualitäts-Zeitschritt ', transinfo_zeit(transinfo_zuord(n))
      end if
   end do ! alle transportinfo Zeitpunkte
   if (stationaer) then
      na_transinfo = 1
      gefunden = .true.
   end if
   if ( .not. gefunden) then
      write(fehler,*)'Zum Anfangszeitpunkt des abgefragten Zeitintervalls existiert keine passende transportinfo Datei ' &
      ,start_zeitschritt
      call qerror(fehler)
   end if
   do n = 1,transinfo_anzahl
      if ((transinfo_zeit(transinfo_zuord(n)) > start_zeitschritt) .and. &
          (transinfo_zeit(transinfo_zuord(n)) <= ende_zeitschritt )) then
         !print*,'weiterer Hydraulik Zeitschritt in diesem Qualitäts-Zeitschritt ', transinfo_zeit(transinfo_zuord(n))
         ne_transinfo = n
      end if
   end do ! alle transportinfo Zeitpunkte
   if (stationaer) then
      !ne_transinfo=3 !! einde: 3*300s casu-zeitschirtt auf 1*900s tiqu-zeitschritt ######
      !ne_transinfo=6 !! einde: 6*150s casu-zeitschirtt auf 1*900s tiqu-zeitschritt ######
      !ne_transinfo=1 !! einde: 1*900s casu-zeitschirtt auf 1*900s tiqu-zeitschritt ######
      ne_transinfo = deltat/dttrans
      gefunden = .true.
   end if
   if (ne_transinfo < na_transinfo) ne_transinfo = na_transinfo
   !print*,'start_zeitschritt,ende_zeitschritt ',start_zeitschritt,ende_zeitschritt
   !print*,'ne_transinfo ',ne_transinfo
   !print*,'Ende Zeitschritt ', transinfo_zeit(transinfo_zuord(ne_transinfo))
   anz_transinfo = 1+ne_transinfo-na_transinfo
   if (stationaer) then
      deti = dttrans*anz_transinfo
   else !instationär
      deti = transinfo_zeit(transinfo_zuord(ne_transinfo))-transinfo_zeit(transinfo_zuord(na_transinfo))
      deti = deti+dttrans
   end if
   
   if (deti /= deltat) then
      print*,'Zeitschrittweiten Transport = ',dttrans,' - Güte = ',deltat,' passen nicht zueinander.'
      print*,'ganzzahlige Vielfache erforderlich.'
      print*,'deti = ',deti
      print*,'anfang ',na_transinfo, transinfo_zuord(na_transinfo), transinfo_zeit(transinfo_zuord(na_transinfo))
      print*,'  ende ',ne_transinfo, transinfo_zuord(ne_transinfo), transinfo_zeit(transinfo_zuord(ne_transinfo))
      if (hydro_trieb == 1)call qerror('deti /= deltat')!! nur bei casu-Strombahnen abbrechen
   end if
   
   print*," Für den Transport im Gütezeitschritt von ",start_zeitschritt," bis ", ende_zeitschritt
   print*," werden ", anz_transinfo," Transportzeitschritte verwendet. transinfo_zeit(anfang/ende)="
   print*,transinfo_zeit(transinfo_zuord(na_transinfo)),transinfo_zeit(transinfo_zuord(ne_transinfo))
   print*,'Zeitschritnummern Anfang/Ende=',na_transinfo,ne_transinfo

   return
end subroutine transinfo_schritte