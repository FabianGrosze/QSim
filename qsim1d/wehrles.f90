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

!> Ermittlung der Wehrhöhen und Wehrbreiten für eine Zeitschritt
!! @author Volker Kirchesch
!! @date 13.06.2013
subroutine wehrles(itags, monats, Jahrs, uhrz, wehrh, wehrb, mStra,   &
                   jlWO2, janzWt, janzWs, cpfad, iwied)
   
   use allodim
   implicit none
   
   integer                                :: nrs, nrsj, mstr_mx, mstr, monats
   integer                                :: jw, jahrs, iwied, iwe, itags
   integer                                :: ipwe, ipwes, io2w, ihcon, iee2
   integer                                :: iee1, ianz_mx
   integer                                :: azStr, read_error
   real                                   :: hcon1, ywert, wert2, wert1, uhr_dz
   real                                   :: uhrz, tempww, str, saettw, r_nrs
   real                                   :: r_nrs2, r_nrs1, r_nrs0, reyn, qwehr
   real                                   :: o2uw, o2ow, hcont, hconr, hcon2
   real                                   :: froude, do2w, dhcon
   character(1)                           :: cwert
   character(40)                          :: MODNAME, ERENAME
   character(255)                         :: cpfad
   character(275)                         :: pfadstring
   integer, dimension(azStrs)             :: io2Ws, ISWEHR, janzWs, janzWt, mstra, jlwo2
   integer, dimension(:,:),   allocatable :: itagr, monatr, jahrr
   real,    dimension(azStrs)             :: wehrb, wehrh
   real,    dimension(:,:,:), allocatable :: wertwe
   real,    dimension(:,:),   allocatable :: uhrzr
   save uhrzr, itagr, monatr, jahrr, wertwe
   
   
   if (iwied == 0) then
      ! Einlesen der Daten zur Berechnung der Wehrbelüftung
      ! jlWO2 = 1   Vorgabe der OW und UW Werte und Bestimmung der Fallhöhe
      ! jlWO2 = 2   Vorgabe der gezogenen Wehrsegmente
      ! janzWs      Gesamtzahl der Wehrsegmente
      ! ISWEHR      Daten auf stundenbasis=1, Daten auf Tagesbasis=0
      ! io2Ws       Anzahl der Werte für ein Wehr
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'wehro2.txt'
      open(unit = 301, file = pfadstring)
      rewind (301)
      read(301,'(A40)',iostat = read_error)MODNAME
      if (read_error < 0)goto 999
      read(301,'(A40)')ERENAME
      
      mstr_mx = 0
      ianz_mx = 0
      do azStr = 1,azStrs
         read(301,'(I5)',iostat = read_error)mstr
         if (read_error < 0)exit
         read(301,'(I1,2x,I5,2x,I1,2x,I1)')ISWEHR(mstr),io2Ws(mstr),jlWO2(mstr),janzWS(mstr)
         if (mstr > mstr_mx)mstr_mx = mstr
         if (io2Ws(mstr) > ianz_mx)ianz_mx = io2Ws(mstr)
         do io2W = 1,io2Ws(mstr)
            read(301,'(a1)')cwert
         enddo
      enddo
      if (mstr_mx == 0) then
         if (.not. allocated(wertwe)) allocate(wertwe(1:1,1:1,1:4))
         if (.not. allocated(itagr))  allocate(itagr(1:1,1:1))
         if (.not. allocated(monatr)) allocate(monatr(1:1,1:1))
         if (.not. allocated(jahrr))  allocate(jahrr(1:1,1:1))
         if (.not. allocated(uhrzr))  allocate(uhrzr(1:1,1:1))
      else
         if (.not. allocated(wertwe)) allocate(wertwe(1:mstr_mx,1:ianz_mx,1:4))
         if (.not. allocated(itagr))  allocate(itagr(1:mstr_mx,1:ianz_mx))
         if (.not. allocated(monatr)) allocate(monatr(1:mstr_mx,1:ianz_mx))
         if (.not. allocated(jahrr))  allocate(jahrr(1:mstr_mx,1:ianz_mx))
         if (.not. allocated(uhrzr))  allocate(uhrzr(1:mstr_mx,1:ianz_mx))
      endif
      close (301)
      
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'wehro2.txt'
      open(unit = 301, file = pfadstring)
      rewind (301)
      
      read(301,'(A40)') MODNAME
      read(301,'(A40)') ERENAME
      do iWe = 1,100  !Beginn der Schleife über die Anzahl der Wehre
         read(301,'(I5)',iostat = read_error)mstr
         if (read_error < 0)exit
         jlWO2(mstr) = 0
         read(301,'(I1,2x,I5,2x,I1,2x,I1)')ISWEHR(mstr),io2Ws(mstr),jlWO2(mstr),janzWS(mstr)
         if (jlWO2(mstr) == 2) then
            ! Vorgabe Wehrsegmente
            if (ISWEHR(mstr) == 1) then
               do io2W = 1,io2Ws(mstr)
                  read(301,3013)itagr(mstr,io2W),monatr(mstr,io2W),jahrr(mstr,io2W),uhrzr(mstr,io2W),wertwe(mstr,io2W,1)
               enddo
               cycle
            else
               do io2W = 1,io2Ws(mstr)
                  read(301,3012)itagr(mstr,io2W),monatr(mstr,io2W),jahrr(mstr,io2W),wertwe(mstr,io2W,1)
               enddo
               cycle
            endif
         else 
            ! Vorgabe OW/UW O2Werte
            if (ISWEHR(mstr) == 1) then
               do io2W = 1,io2Ws(mstr)
                  read(301,3011)itagr(mstr,io2W),monatr(mstr,io2W),jahrr(mstr,io2W),uhrzr(mstr,io2W),   &
                               (wertwe(mstr,io2W,jw),jw = 1,4)
               enddo
               cycle
            else
               do io2W = 1,io2Ws(mstr)
                  read(301,3010)itagr(mstr,io2W),monatr(mstr,io2W),jahrr(mstr,io2W),(wertwe(mstr,io2W,jw),jw = 1,4)
               enddo
               cycle
            endif
         endif
         
         3010 format(i2,2x,i2,2x,I4,2x,f8.3,2x,f5.2,2x,f5.2,2x,f5.2)
         3011 format(i2,2x,i2,2x,I4,2x,f5.2,2x,f8.3,2x,f5.2,2x,f5.2,2x,f5.2)
         3012 format(i2,2x,i2,2x,I4,2x,f2.0)
         3013 format(i2,2x,i2,2x,I4,2x,f5.2,2x,f2.0)
         
      enddo ! Ende Wehrschleife
      close (301)
   endif
   ipwes = 0
   if (monats > 2) then
      NRS = (ITAGS+31*(MONATS-1)-INT(0.4*MONATS+2.3))
   else
      NRS = ITAGS+31*(MONATS-1)
   endif
   
   
   ! Tage seit 1900 (Berücksichtigung der Schaltjahre
   ! TODO (schoenung): Diese Formel ist falsch. Nicht jedes durch 4 teilbare Jahr
   !                   ist ein Schaltjahr.
   NRSJ = (Jahrs-1900)*365+int((Jahrs-1900)/4) 
   
   R_NRS = NRS + NRSJ + Uhrz/24.
   do azStr = 1,azStrs       ! Beginn Strangschleife
      mstr = mstra(azStr)
      
      if (wehrb(mstr) <= 0.0)cycle ! keine Wehrbreite, keine Wehrbelüftung
      if (jlWO2(mstr) == 0)cycle
      if (jlWO2(mstr) == 1)ipwes = 4
      if (jlWO2(mstr) == 2)ipwes = 1
      do ipwe = 1,ipwes
         
         iee1 = -1
         iee2 = -1
         
         do io2W = 1,io2Ws(mstr)
            if (ISWEHR(mstr) == 0)uhrzr(mstr,io2W) = 0.0
            ! Umrechnen der "Messwertuhrzeit" in Dezimalschreibweise
            uhr_Dz = int(uhrzr(mstr,io2W))+((uhrzr(mstr,io2W)-int(uhrzr(mstr,io2W)))/0.6)
            
            if (MONATr(mstr,io2W) > 2) then
               NRS = (itagr(mstr,io2W)+31*(monatr(mstr,io2W)-1)-INT(0.4*monatr(mstr,io2W)+2.3))
            else
               NRS = itagr(mstr,io2W)+31*(monatr(mstr,io2W)-1)
            endif
            NRSJ = (jahrr(mstr,io2W) - 1900)*365+int((jahrr(mstr,io2W)-1900)/4)
            R_NRS0 = NRS + NRSJ + uhr_Dz/24.
            if (iee1 == -1 .and. R_NRS0 <= R_NRS.and.wertwe(mstr,io2w,ipwe) < 0.0) then
               wert1 = wertwe(mstr,io2W,ipwe)
            else if (R_NRS0 <= R_NRS .and. wertwe(mstr,io2w,ipwe)>=0.0) then
               R_NRS1 = R_NRS0
               iee1 = 1
               wert1 = wertwe(mstr,io2w,ipwe)
            else if (R_NRS0 > R_NRS .and. iee2 == -1.and.wertwe(mstr,io2w,ipwe) < 0.0) then
               wert2 = wertwe(mstr,io2W,ipwe)
            else if (R_NRS0 > R_NRS .and. wertwe(mstr,io2W,ipwe)>=0.0) then
               R_NRS2 = R_NRS0
               iee2 = 1
               wert2 = wertwe(mstr,io2W,ipwe)
               exit
            endif
         enddo
         
         
         ! --- Interpolation ---
         if (iee1 == 1 .and. iee2 == -1) then
            Ywert = wert1
         else if (iee1 == -1 .and. iee2 == 1) then
            Ywert = wert2
         else if (iee1 == -1 .and. iee2 == -1) then
            Ywert = wert1
         else
            hcon1 = R_NRS2 - R_NRS1
            hcon2 = R_NRS - R_NRS1
            Ywert = wert1 + ((wert2 - wert1)/hcon1)*hcon2
         endif
         if (ipwe == 1)QWehr = ywert
         if (ipwe == 2)TempwW = ywert
         if (ipwe == 3)O2OW = ywert
         if (ipwe == 4)O2UW = ywert
      enddo ! Ende Parameterschleife
      if (jlWO2(mstr) == 2) then
         ihcon = int(QWehr)        ! Runden
         dhcon = QWehr-ihcon
         if (dhcon >= 0.5)janzWt(mstr) = ihcon+1
         if (dhcon < 0.5) janzWt(mstr) = ihcon
         cycle
      else
         ! Berechnung der fiktiven Wehrhoehe
         SAETTW = 14.603-TEMPWW*0.40215+(TEMPWW**2)*0.007687-(tempwW**3)*0.0000693
         hcont = (1.+0.046*tempwW)/1.69
         do2W = (O2OW-O2UW)/hcont
         do2W = abs(do2W)
         
         if (o2OW > saettW .and. (o2OW-do2W) < saettW)do2W = (saettW-o2OW)+0.1
         if (o2OW < saettW .and. (o2OW+do2W) > saettW)do2W = (saettW-o2OW)-0.1
         
         str = QWehr/wehrb(mstr)
         
         reyn = str/1.143e-6
         
         hcon1 = (do2W/(saettW-o2OW))-1.
         hconr = -1./hcon1
         froude = ((hconr-1.)/(0.627e-4*reyn**0.53))**(1./1.78)
         
         wehrh(mstr) = ((froude/1.488)**(1./0.25)*str**2)**(1./3.)
      endif
   enddo   !Ende Strangschleife
   
   999 continue
end subroutine wehrles
