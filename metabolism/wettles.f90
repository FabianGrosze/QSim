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

!> Berechnung der Wetterdaten für einen Zeitschritt
!! @author Volker Kirchesch
!! @date 15.04.2003
subroutine wettles(itags, monats, jahrs, uhrz, glob, tlmax, tlmin, ro, wge,   &
                   cloud, wtyp, imet, iwied, cpfad, ckenn_vers1)
   implicit none
   
   integer                               :: iee2, nrs, nrsj, mwett, mwetts_mx
   integer                               :: monats, jahrs, ixw, iwsta_mx, iwied
   integer                               :: iwett, iwetts, itags, ipw, ipws
   integer                               :: imet, iee1
   real                                  :: ywert, wert2, wert1, uhr_dz, uhrz
   real                                  :: r_nrs0, hctmx2, hcon2, hcon1
   character (len = 1)                   :: cwert
   character (len = 2)                   :: ckenn_vers1
   character (len = 40)                  :: MODELL, ERENAME
   character (len = 255)                 :: cpfad
   character (len = 275)                 :: pfadstring, message
   integer                               :: read_error
   integer, dimension(20)                :: iWSta, mwetts
   integer,dimension(:,:), allocatable   :: itagw, monatw, jahrw
   real, dimension(20)                   :: glob, tlmax, tlmin, ro, wge, cloud, wtyp
   real, dimension(:,:), allocatable     :: uhrzw
   real, dimension(:,:,:), allocatable   :: wertw
   logical                               :: is_set_wert1, is_set_wert2
   double precision                      :: R_NRS2, R_NRS1, R_NRS
   
   external                              :: qerror, set_cloud_reflectance
   
   save itagw, monatw, jahrw, uhrzw, wertw, iwetts, R_NRS2, R_NRS1, R_NRS, IWSta, mWetts
   
   ! Parameter
   ! IWETTS  : Anzahl der Wetterstationen
   ! IPWS    : Anzahl der metereologischen Kenngrößen (hier: 7)
   ! mWETTs  : Anzahl der Datensätze für eine Wetterstation
   ! imet    : Daten auf Tagesbasis (0); Daten auf Stundenbasis (1)
   ! iWSta   : ID-Nummer der Wetterstation
   
   ipws = 7
   if (iwied == 0) then
      ! Einlesen der Wetterdaten aus WETTER.TXT
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'WETTER.txt'
      open(unit = 86, file = pfadstring)
      rewind (86)
      read(86,'(A2)')ckenn_vers1
      if (ckenn_vers1 /= '*V') then
         read(86,'(A40)')ERENAME
      else
         read(86,'(A40)')MODELL
         read(86,'(A40)')ERENAME
      endif
      read(86,'(I2,2x,I1)')IWETTs,IMET
      
      iWSta_mx = 1
      mWetts_mx = 1
      do iWETT = 1,iWETTs
         read(86,'(I8,2x,I5)',iostat = read_error) IWSta(iwett), mWetts(iwett)
         if (read_error < 0) exit
         if (mWetts(iwett) > mWetts_mx) mWetts_mx = mWetts(iwett)
         if (iWSta(iwett) > iWSta_mx) iWSta_mx = iWSta(iwett)
         
         if (mWetts(iwett) == 0) then
            write(199,1996)
            1996 format(2x,'Wetterdaten nicht oder unvollständig eingegeben')
         endif
         !
         do mWett = 1,mWetts(iwett)
            read(86,'(a1)')cwert
         enddo
      enddo
      print*,'wettles: mWetts_mx,iWSta_mx,iWETTs,IMET = ',mWetts_mx,iWSta_mx,iWETTs,IMET
      if ( .not. allocated(wertw))  allocate(wertw(1:iWSta_mx,1:mWetts_mx,1:ipws))
      if ( .not. allocated(itagw))  allocate(itagw(1:iWSta_mx,1:mWetts_mx))
      if ( .not. allocated(monatw)) allocate(monatw(1:iWSta_mx,1:mWetts_mx))
      if ( .not. allocated(jahrw))  allocate(jahrw(1:iWSta_mx,1:mWetts_mx))
      if ( .not. allocated(uhrzw))  allocate(uhrzw(1:iWSta_mx,1:mWetts_mx))
      rewind (86)
      read(86,'(A2)')ckenn_vers1
      if (ckenn_vers1 /= '*V') then
         read(86,'(A40)')ERENAME
      else
         read(86,'(A40)')MODELL
         read(86,'(A40)')ERENAME
      endif
      read(86,*,iostat = read_error) IWETTs, IMET
      if (read_error < 0) call qerror("Error while reading Wetter.txt")
      do iWETT = 1,iWETTs
         read(86,'(I8,2x,I5)',iostat = read_error) IWSta(iwett), mWetts(iwett)
         if (read_error < 0) call qerror("Error while reading Wetter.txt")
         
         if (imet == 0) then
            hcTmx2 = -999.
            do mWett = 1,mWetts(iwett)
               read(86, 2013, iostat = read_error) itagw(iWSta(iwett),mwett), monatw(iWSta(iwett),mwett),                &
                                                   jahrw(iWSta(iwett),mwett), (wertw(iWSta(iwett),mwett,ixw),ixw = 1,7)
               if (read_error < 0) call qerror("Error while reading Wetter.txt")
               hcTmx2 = max(hcTmx2, wertw(iWSta(iwett),mwett,3))
            enddo
            
            ! Fehlermeldung keine Minimumtemperaturen an einer oder mehrer Wetterstationen
            if (hcTmx2 == -1.) then
               write(message, "(a,i0)") "No minimum temperature given for weather station ", iwett
               call qerror(trim(message))
            endif
            
         else
            ! Messwerte auf Stundenbasis
            do mWett = 1,mWetts(iwett)
               read(86,2023,iostat = read_error) itagw(iWSta(iwett),mwett), monatw(iWSta(iwett),mwett),        &
                                                 jahrw(iWSta(iwett),mwett), uhrzw(iWSta(iwett),mwett) ,        &
                                                 (wertw(iWSta(iwett),mwett,ixw),ixw = 1,7)
               if (read_error < 0) call qerror("Error while reading Wetter.txt")
            enddo
         endif
      enddo
      close (86)
   else
   endif
   2013 format(i2,2x,i2,2x,I4,9x,f7.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f4.1,2x,f4.1)
   2023 format(i2,2x,i2,2x,I4,2x,f5.2,2x,f7.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f4.1,2x,f4.1)
   if (monats > 2) then
      NRS = (ITAGS+31*(MONATS-1)-INT(0.4*MONATS+2.3))
   else
      NRS = ITAGS+31*(MONATS-1)
   endif
   
   NRSJ = (Jahrs-1900)*365+int((Jahrs-1900)/4)            !Tage seit 1900 (Berücksichtigung der Schaltjahre
   R_NRS = NRS + NRSJ
   if (imet /= 0) R_NRS = R_NRS + Uhrz/24.
   
   do iwett = 1, iwetts
      do ipw = 1,ipws
         iee1 = -1
         iee2 = -1
         is_set_wert1 = .false.
         is_set_wert2 = .false.
         
         do mWett = 1, mWetts(iwett)       ! Beginn der Werteschleife
            NRSJ = (jahrW(iWSta(iwett),mwett) - 1900) * 365 + int((jahrW(iWSta(iwett),mwett)-1900)/4)
            NRS  =  itagW(iWSta(iwett),mwett) + 31 * (monatW(iWSta(iwett),mwett) - 1)
            if (monatw(iWSta(iwett),mwett) > 2) NRS = NRS - INT(0.4*monatW(iWSta(iwett),mwett) + 2.3)
            if (imet == 0) then
               uhrzw(iWSta(iwett),mwett) = 0.0
               R_NRS0 = NRS + NRSJ
            else
               !...Umrechnen der "Messwertuhrzeit" in Dezimalschreibweise
               uhr_Dz = int(uhrzw(iWSta(iwett),mwett)) + ((uhrzw(iWSta(iwett),mwett)-int(uhrzw(iWSta(iwett),mwett)))/0.6)
               R_NRS0 = NRS + NRSJ + uhr_Dz/24.
            endif
            if (iee1 == -1 .and. R_NRS0 <= R_NRS .and. wertw(iWSta(iwett),mwett,ipw) < 0.0) then
               wert1 = wertw(iWSta(iwett),mwett,ipw)
               is_set_wert1 = .true.
            else if (R_NRS0 <= R_NRS .and. wertw(iWSta(iwett),mwett,ipw) >= 0.0) then
               R_NRS1 = R_NRS0
               iee1  = 1
               wert1 = wertw(iWSta(iwett),mwett,ipw)
               is_set_wert1 = .true.
            else if (iee2 == -1 .and. R_NRS0 > R_NRS .and. wertw(iWSta(iwett),mwett,ipw) < 0.0) then
               wert2 = wertw(iWSta(iwett),mwett,ipw)
               is_set_wert2 = .true.
            else if (R_NRS0 > R_NRS .and. wertw(iWSta(iwett),mwett,ipw) >= 0.0) then
               R_NRS2 = R_NRS0
               iee2 = 1
               wert2 = wertw(iWSta(iwett),mwett,ipw)
               is_set_wert2 = .true.
               exit
            endif
         enddo   ! Ende Werteschleife
         
         ! in case of cloud type (0-9) convert to cloud reflectance(?)
         if (ipw == 7) then
            if (is_set_wert1) call set_cloud_reflectance(nint(wert1), wert1)
            if (is_set_wert2) call set_cloud_reflectance(nint(wert2), wert2)
         endif
         
         !+++ Interpolation++++
         if (iee1 == 1 .and. iee2 == -1) then
            Ywert = wert1
         else if (iee1 == -1 .and. iee2 == 1) then
            Ywert = wert2
         else if (iee1 == -1 .and. iee2 == -1) then
            ! TODO FG: introduced switches to avoid uninitialised use of wert1
            if (is_set_wert1) then
               Ywert = wert1
            else if (is_set_wert2) then
               Ywert = wert2
            else
               ! if no valid values are found before and after and error is thrown.
               ! ipw == 7 (cloud type) is an exception. For some station no such data
               ! is available, so it is accepted for timeseries to have no values at all.
               if (ipw == 7) then
                  call set_cloud_reflectance(-1, Ywert)
               else
                  call qerror("wettles.f90: Neither 'wert1' nor 'wert2' set.")
               endif
            endif
         else
            hcon1 = R_NRS2 - R_NRS1
            hcon2 = R_NRS  - R_NRS1
            Ywert = wert1 + ((wert2 - wert1)/hcon1)*hcon2
         endif
         select case (ipw)
            case (1)
               glob(iWSta(iwett))  = ywert
            case (2)
               tlmax(iWSta(iwett)) = ywert
            case(3)
               tlmin(iWSta(iwett)) = ywert
            case(4)
               ro(iWSta(iwett))    = ywert
            case(5)
               wge(iWSta(iwett))   = ywert
            case(6)
               cloud(iWSta(iwett)) = ywert
            case(7)
               wtyp(iWSta(iwett))  = ywert
            case default
               call qerror("wettles.f90: Weather parameter index must be in range 1-7.")
         end select
      enddo   ! Ende Parameterschleife
   enddo      ! Ende Statonenschleife
   989 continue
end subroutine wettles