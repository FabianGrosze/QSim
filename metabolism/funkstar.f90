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

!> Interpolation der Randbedingungen
!! @author Volker Kirchesch
!! @date 13.07.2019
subroutine funkstar(abfls,vbsbs,vcsbs,vnh4s,vno2s,vno3s,gesNs,vx0s,vx02s,gelps,gesPs,sis,chlas,vkigrs                        &
                    ,antbls,zooins,vphs,mws,cas,lfs,ssalgs,tempws,vo2s,CHNFs,BVHNFs,colis,DOSCFs,waers                       &
                    ,iColi, iSchwer,glZns,gsZns,glCads,gsCads,glCus,gsCus,glNis,gsNis,glAss,gsAss,glPbs,gsPbs,glCrs,gsCrs    &
                    ,glFes,gsFes,glHgs,gsHgs,glMns,gsMns,glUs,gsUs                                                           &
                    ,c1Zn,e1Zn,c2Zn,e2Zn,c3Zn,e3Zn,c4Zn,e4Zn,c5Zn,e5Zn,VTKoeffDe_Zn                                          &
                    ,c1Cu,e1Cu,c2Cu,e2Cu,c3Cu,e3Cu,c4Cu,e4Cu,c5Cu,e5Cu,VTKoeffDe_Cu                                          &
                    ,c1Cad,e1Cad,c2Cad,e2Cad,c3Cad,e3Cad,c4Cad,e4Cad,c5Cad,e5Cad,VTKoeffDe_Cad                               &
                    ,c1Ni,e1Ni,c2Ni,e2Ni,c3Ni,e3Ni,c4Ni,e4Ni,c5Ni,e5Ni,VTKoeffDe_Ni                                          &
                    ,c1As,e1As,c2As,e2As,c3As,e3As,c4As,e4As,c5As,e5As,VTKoeffDe_As                                          &
                    ,c1Pb,e1Pb,c2Pb,e2Pb,c3Pb,e3Pb,c4Pb,e4Pb,c5Pb,e5Pb,VTKoeffDe_Pb                                          &
                    ,c1Cr,e1Cr,c2Cr,e2Cr,c3Cr,e3Cr,c4Cr,e4Cr,c5Cr,e5Cr,VTKoeffDe_Cr                                          &
                    ,c1Fe,e1Fe,c2Fe,e2Fe,c3Fe,e3Fe,c4Fe,e4Fe,c5Fe,e5Fe,VTKoeffDe_Fe                                          &
                    ,c1Hg,e1Hg,c2Hg,e2Hg,c3Hg,e3Hg,c4Hg,e4Hg,c5Hg,e5Hg,VTKoeffDe_Hg                                          &
                    ,c1Mn,e1Mn,c2Mn,e2Mn,c3Mn,e3Mn,c4Mn,e4Mn,c5Mn,e5Mn,VTKoeffDe_Mn                                          &
                    ,c1U,e1U,c2U,e2U,c3U,e3U,c4U,e4U,c5U,e5U,VTKoeffDe_U                                                     &
                    ,istund,uhrz,RBtyp,NRSCHr,itags,monats,jahrs,cpfad,iwsim,ilang,iwied,mstrRB,i_Rands                      &
                    ,iw_max,iformVert)
   
   use allodim
   
   character (len = 255)                       :: cpfad
   character (len = 275)                       :: pfadstring
   
   integer                                     :: RBNR, read_error
   integer, dimension(40000)                   :: imstr, iRBNR, ianzW
   integer, dimension(azStrs,100)              :: istund, RBtyp, NRSchr
   integer, dimension(200,40000)               :: itagl, monatl, jahrl
   integer, intent(in)                         :: iColi, iSchwer
   integer, dimension(:,:,:), allocatable      :: mREC
   real                                        :: VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni
   real                                        :: VTKoeff_As,VTKoeff_Pb,VTKoeff_Cr,VTKoeff_Fe
   real                                        :: VTKoeff_Hg,VTKoeff_Mn,VTKoeff_U
   real, dimension(azStrs,100)                 :: vbsbs,vcsbs, vnh4s, vno2s, vno3s, gesNs, vx0s, vx02s
   real, dimension(azStrs,100)                 :: gelps, gesPs, sis, chlas, waers
   real, dimension(azStrs,100)                 :: vkigrs, antbls, zooins, vphs, mws, cas, lfs, ssalgs
   real, dimension(azStrs,100)                 :: tempws, vo2s, CHNFs, BVHNFs, colis, DOSCFs, abfls
   real, dimension(azStrs,100)                 :: glZns,gsZns,glCads,gsCads,glCus,gsCus,glNis,gsNis
   real, dimension(azStrs,100)                 :: glAss,gsAss,glPbs,gsPbs,glCrs,gsCrs,glFes,gsFes
   real, dimension(azStrs,100)                 :: glHgs,gsHgs,glMns,gsMns,glUs,gsUs
   real, dimension(200,40000)                  :: uhrl
   real, dimension(:,:,:), allocatable         :: werts
   real                                        :: NaN_value
   logical                                     :: is_set_wert1, is_set_wert2
   character(len = 200)                        :: message
   
   real, parameter                             :: epsilon = 1.e-8
   
   double precision                            :: R_NRS, R_NRS2, R_NRS1
   
   save ianRBs, mREC, werts, ianzW, itagl, monatl,jahrl, Uhrl, iRBNR, imstr,R_NRS
   save R_NRS2, R_NRS1, VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni
   save VTKoeff_As,VTKoeff_Pb,VTKoeff_Cr,VTKoeff_Fe,VTKoeff_Hg,VTKoeff_Mn,VTKoeff_U
   
   ! ipp=28 ist die Tracerkonzentration. Wird auf den Parameter tempw gelegt
   ! ipp=29 konserv. Substanz wird auf tempw gelegt
   ! ipps=51 Schwermetalle
   ! iwsim = 4 -> Tracer
   ! iwsim = 5 -> konserv. Substanz
   
   ianzRB = 0
   ipps = 51
   if (.not. allocated(werts)) allocate(werts(1:i_Rands,1:ipps,1:iw_max))
   if (.not. allocated(mREC))  allocate(mREC(1:azStrs,1:i_Rands,1:ipps))
   
   ! Einlesen aus EREIGG
   if (ilang == 0) then
      close (92)
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'EREIGG.txt'
      open(unit = 92, file = pfadstring)
      rewind (92)
      
      read(92,'(2x)')
      read(92,'(2x)')
      read(92,'(2x)')
      read(92,'(2x)')
      read(92,'(2x)')
      read(92,'(2x)')
      
      do i_Rand = 1, 200 ! Randbedingungsschleife Beginn
         read(92,9230,iostat = read_error)mstr,RBNR,istund(mstr,RBNR),NrSchr(mstr,RBNR)
         if (read_error < 0.0)exit
         
         !....Fehlermeldung
         if (NrSchr(mstr,RBNR) > 40000) then
            write(199,1899)RBNR,mstr
            1899 format(2x,'fuer die ',I3,'.Randbedingung des ',I2,'. Strangs existieren mehr als 8800 Datensaetze')
         endif
         
         if (NrSchr(mstr,RBNR) == 0) cycle
         ! Summenbildung der Randbedingungen
         ianzRB = ianzRB+1
         imstr(ianzRB) = mstr
         iRBNR(ianzRB) = RBNR
         ianzW(ianzRB) = NrSchr(mstr,RBNR)
         ! Lesen der Zeitreihen an der jeweiligen Randbedingung aus EREIGG.txt
         ! alle Variablen in Feld werts
         do iwe = 1,NrSchr(mstr,RBNR)
            
            read(92,9240,iostat = read_error)itagl(ianzRB,iwe),monatl(ianzRB,iwe),jahrl(ianzRB,iwe),uhrl(ianzRB,iwe)   &
                 ,(werts(ianzRB,ixpp,iwe),ixpp = 1,ipps)
            if (read_error < 0) call qerror("Error while reading EreigG.txt")
            
            if (iwsim == 4) werts(ianzRB,28,iwe) = max(0., werts(ianzRB,28,iwe))
            ! Umrechnung der "Messwert-Uhrzeit" in Dezimalschreibweise
            uhrl(ianzRB,iwe) = int(uhrl(ianzRB,iwe))+((uhrl(ianzRB,iwe)-int(uhrl(ianzRB,iwe)))/0.6)
         enddo
         
         do ixpp = 1,ipps
            mREC(mstr,ianzRB,ixpp) = 0
         enddo
      enddo ! Randbedingungsschleife i_Rand Ende
      
      ianRBs = ianzRB
      9230 format(I5,2x,I5,2x,I1,2x,I5)
      9240 format(i2,2x,i2,2x,I4,2x,f5.2,2x,f13.6,2x,f6.2,2x,f6.2,2x,f6.2,2x      &
      ,f6.3,2x,f5.2,2x,f5.2,2x,f8.5,2x,f8.5,2x,f6.3,2x,f5.2,2x,f5.2               &
      ,2x,f6.2,2x,f5.2,2x,f5.2,2x,f7.1,2x,f5.2,2x,f5.2,2x,f5.1,2x                 &
      ,f8.1,2x,f7.2,2x,f5.2,2x,f5.2,2x,f8.1,2x,f6.1,2x,E9.2,2x,f7.1               &
      ,2x,f9.3,2x,f7.1,2x,F6.2,2x,F6.2,2x,F7.3,2x,F7.3,2x,F6.2,2x                 &
      ,F6.2,2x,F8.1,2x,F8.1,2x,F6.2,2x,F6.2,2x,F8.1,2x,F8.1,2x,F6.2,2x,F6.2       &
      ,2x,F7.3,2x,F7.3,2x,F7.3,2x,F7.3,2x,F8.1,2x,F8.1,2x,F5.1,2x,F5.1)
      !                              abfl    vbsb    vcsb    vnh4
      !01  01  2003  00.00       0.000000    2.91   32.05    0.23
      !i2  i2    I4   f5.2          f13.6    f6.2    f6.2    f6.2
      !    vno2   vno3   gesN       vx0      vx02    gelp   gesP     si
      !   0.015   0.78   1.41   0.00010   0.00010   0.044   0.09   5.55
      !    f6.3   f5.2   f5.2      f8.5      f8.5    f6.3   f5.2   f5.2
      !   chla  vkigr  antbl    zooin    vph     mw     ca
      !  11.79   0.47   0.35     26.1   7.79   3.03   88.1
      !   f6.2   f5.2   f5.2     f7.1   f5.2   f5.2   f5.1
      !        lf    ssalg  tempw    vo2      CHNF   BVHNF       coli     waer
      !     562.4     3.01   1.00  12.00      -1.0    -1.0  -1.00E+00  -9999.9
      !      f8.1     f7.2   f5.2   f5.2      f8.1    f6.1       E9.2     f7.1
      !  TRACER   KONSS           gsPb       glPb     gsCad     glCad     gsCr  ####
      !     -1.000     -1.0       3.00       2.00   -1.0000   -1.0000   -1.000
      !     f9.3   f7.1           F6.2       F6.2      F7.3      F7.3     F6.2
      !  glCr    gsFe    glFe  gsCu  glCu    gsMn    glMn  gsNi  glNi
      !   -1.000   -1.000   -1.000
      !  F6.2    F8.1    F8.1  F6.2  F6.2    F8.1    F8.1  F6.2  F6.2
      !   gsHg   glHg    gsU    glU    gsZn    glZn gsAs glAs
      !   F7.3   F7.3   F7.3   F7.3    F8.1    F8.1 F5.1 F5.1
   endif
   
   NRS = ITAGS+31*(MONATS-1)
   if (monats > 2) NRS = NRS - INT(0.4*MONATS+2.3)
   
   !Tage seit 1900 (Berücksichtigung der Schaltjahre
   NRSJ = (Jahrs-1900)*365 + int((Jahrs-1900)/4)
   
   R_NRS = NRS + NRSJ + Uhrz/24.
   
   ! Schleife über alle Randbedingungen hier: Beginn
   do ianzRB = 1,ianRBs
      mstr = imstr(ianzRB)
      RBNR = iRBNR(ianzRB)
      NRS = 0
      ! Parameterschleife Beginn
      do ipp = 1,ipps
         iee1 = -1
         iee2 = -1
         
         iREC = 1
         if (ilang == 1)iREC = mREC(mstr,ianzRB,ipp)
         if (iREC == 0)iREC = 1
         manzW = ianzW(ianzRB)
         
         is_set_wert1 = .false.
         is_set_wert2 = .false.
         
         ! set fail values depending on the parameter
         if     (ipp == 22) then
            NaN_value = -9.99
         elseif (ipp == 27) then
            NaN_value = -9999.9
         else
            NaN_value = 0.
         endif
         
         ! Beginn Werteschleife
         do iwe = iRec,manzW
            NRS = itagl(ianzRB,iwe)+31*(monatl(ianzRB,iwe)-1)
            if (monatl(ianzRB,iwe) > 2) NRS = NRS - INT(0.4*monatl(ianzRB,iwe)+2.3)
            NRSJ = (jahrl(ianzRB,iwe) - 1900)*365+int((jahrl(ianzRB,iwe)-1900)/4)
            R_NRS0 = NRS + NRSJ + uhrl(ianzRB,iwe)/24.
            if (ipp == 1) then
               if (R_NRS0 <= R_NRS) then
                  R_NRS1 = R_NRS0
                  iee1   = 1
                  wert1  = werts(ianzRB,ipp,iwe)
                  mREC(mstr,ianzRB,ipp) = iwe
                  is_set_wert1 = .true.
               else
                  R_NRS2 = R_NRS0
                  iee2   = 1
                  wert2  = werts(ianzRB,ipp,iwe)
                  is_set_wert2 = .true.
                  exit
               endif
            else
               if (R_NRS0 <= R_NRS) then
                  if (iee1 == -1 .and. abs(werts(ianzRB,ipp,iwe) - NaN_value) <= epsilon) then
                     mREC(mstr,ianzRB,ipp) = iwe
                     wert1 = werts(ianzRB,ipp,iwe)
                     is_set_wert1 = .true.
                  else if (werts(ianzRB,ipp,iwe) - NaN_value > epsilon) then
                     R_NRS1 = R_NRS0
                     iee1   = 1
                     mREC(mstr,ianzRB,ipp) = iwe
                     wert1  = werts(ianzRB,ipp,iwe)
                     is_set_wert1 = .true.
                  endif
               else
                  if (iee2 == -1 .and. abs(werts(ianzRB,ipp,iwe) - NaN_value) <= epsilon) then
                     wert2 = werts(ianzRB,ipp,iwe)
                     is_set_wert2 = .true.
                  else if (werts(ianzRB,ipp,iwe) - NaN_value > epsilon) then
                     R_NRS2 = R_NRS0
                     iee2 = 1
                     wert2 = werts(ianzRB,ipp,iwe)
                     is_set_wert2 = .true.
                     exit
                  endif
               endif
            endif
         
         enddo ! Ende Werteschleife
         
         if       (iee1 == 1 .and. iee2 == -1) then
            Ywert = wert1
         else if (iee1 == -1 .and. iee2 == 1) then
            Ywert = wert2
         else if (iee1 == -1 .and. iee2 == -1) then
            ! TODO FG: introduced switches to prevent uninitialised use of wert1
            if      (is_set_wert1) then
               Ywert = wert1
            else if (is_set_wert2) then
               Ywert = wert2
            else
               Ywert = 0.
               ! report if boundary values are missing (abort simulation if required)
               ! TODO FG: Parts on HNF in if-condition below need to be adapted once HNF is operational again.
               if (         ipp /= 24 .and. ipp /= 25  .and.     &  ! Heterotrophic nanoflagellates (HNF)
                   .not.(iColi   == 1 .and. ipp == 26) .and.     &  ! Coliform bacteria inactive
                   .not.(iwsim   == 4 .and. ipp == 28) .and.     &  ! Passive tracer inactive
                   .not.(iwsim   == 5 .and. ipp == 29) .and.     &  ! Conservative substances inactive
                   .not.(iSchwer == 1 .and. ipp >= 30)      ) then  ! Heavy metals inactive
                  write(*, '("funkstar.f90: No valid data for parameter nr. ",i3," at boundary nr. ",i3,". Set to zero.")') ipp, RBNR
               elseif (ipp /= 24 .and. ipp /= 25) then
                  if (iColi == 1 .and. ipp == 26) then
                     ! Coliform bacteria active
                     write(message, '("funkstar.f90: No valid data for colis at boundary nr. ",i3,".")') RBNR
                  elseif (iwsim   == 4 .and. ipp == 28) then
                     ! Passive tracer active
                     write(message, '("funkstar.f90: No valid data for tracer at boundary nr. ",i3,".")') RBNR
                  elseif (iwsim   == 5 .and. ipp == 29) then
                     ! Conservative substances active
                     write(message, '("funkstar.f90: No valid data for conservative substance at boundary nr. ",i3,".")') RBNR
                  elseif (iSchwer == 1 .and. ipp >= 30) then
                     ! Heavy metals active
                     write(message, '("funkstar.f90: No valid data for heavy metal (ipp =",i3,") at boundary nr. ",i3,".")') ipp, RBNR
                  endif
                  call qerror(trim(message))
               endif
            endif
         else
            hcon1 = R_NRS2 - R_NRS1
            hcon2 = R_NRS  - R_NRS1
            Ywert = wert1 + (wert2 - wert1)/hcon1 * hcon2
         endif
         
         if (RBtyp(mstr,RBNR) >= 0 .and. RBtyp(mstr,RBNR) <= 2) then
            if (ipp == 28 .and. iwsim == 4) then                        ! passive tracer
               tempws(mstr,RBNR) = ywert
               if (iwied == 0)ywert = 0.0
               if (tempws(mstr,RBNR) < 0.0)tempws(mstr,RBNR) = 0.0
               cycle
            elseif (ipp == 29 .and. iwsim ==5) then                     ! conservative substances
               tempws(mstr,RBNR) = ywert
               cycle
            endif
         endif
         
         if (ipp ==  1) abfls(mstr,RBNR)  = ywert
         if (ipp ==  2) vbsbs(mstr,RBNR)  = ywert
         if (ipp ==  3) vcsbs(mstr,RBNR)  = ywert
         if (ipp ==  4) vnh4s(mstr,RBNR)  = ywert
         if (ipp ==  5) vno2s(mstr,RBNR)  = ywert
         if (ipp ==  6) vno3s(mstr,RBNR)  = ywert
         if (ipp ==  7) gesNs(mstr,RBNR)  = ywert
         if (ipp ==  8) vx0s(mstr,RBNR)   = ywert
         if (ipp ==  9) vx02s(mstr,RBNR)  = ywert
         if (ipp == 10) gelps(mstr,RBNR)  = ywert
         if (ipp == 11) gesPs(mstr,RBNR)  = ywert
         if (ipp == 12) sis(mstr,RBNR)    = ywert
         if (ipp == 13) chlas(mstr,RBNR)  = ywert
         if (ipp == 14) vkigrs(mstr,RBNR) = ywert
         if (ipp == 15) antbls(mstr,RBNR) = ywert
         if (ipp == 16) zooins(mstr,RBNR) = ywert
         if (ipp == 17) vphs(mstr,RBNR)   = ywert
         if (ipp == 18) mws(mstr,RBNR)    = ywert
         if (ipp == 19) cas(mstr,RBNR)    = ywert
         if (ipp == 20) lfs(mstr,RBNR)    = ywert
         if (ipp == 21) ssalgs(mstr,RBNR) = max(ywert, 5.)
         if (ipp == 22) tempws(mstr,RBNR) = ywert
         if (ipp == 23) vo2s(mstr,RBNR)   = ywert
         if (ipp == 24) CHNFs(mstr,RBNR)  = ywert
         if (ipp == 25) BVHNFs(mstr,RBNR) = ywert
         if (ipp == 26) colis(mstr,RBNR)  = ywert
         if (colis(mstr,RBNR) >= 0.0) DOSCFs(mstr,RBNR) = 0.0
         if (ipp == 27) waers(mstr,RBNR)  = ywert
         if (ipp == 30) gsPbs(mstr,RBNR)  = ywert
         if (ipp == 31) glPbs(mstr,RBNR)  = ywert
         if (ipp == 32) gsCads(mstr,RBNR) = ywert
         if (ipp == 33) glCads(mstr,RBNR) = ywert
         if (ipp == 34) gsCrs(mstr,RBNR)  = ywert
         if (ipp == 35) glCrs(mstr,RBNR)  = ywert
         if (ipp == 36) gsFes(mstr,RBNR)  = ywert
         if (ipp == 37) glFes(mstr,RBNR)  = ywert
         if (ipp == 38) gsCus(mstr,RBNR)  = ywert
         if (ipp == 39) glCus(mstr,RBNR)  = ywert
         if (ipp == 40) gsMns(mstr,RBNR)  = ywert
         if (ipp == 41) glMns(mstr,RBNR)  = ywert
         if (ipp == 42) gsNis(mstr,RBNR)  = ywert
         if (ipp == 43) glNis(mstr,RBNR)  = ywert
         if (ipp == 44) gsHgs(mstr,RBNR)  = ywert
         if (ipp == 45) glHgs(mstr,RBNR)  = ywert
         if (ipp == 46) gsUs(mstr,RBNR)   = ywert
         if (ipp == 47) glUs(mstr,RBNR)   = ywert
         if (ipp == 48) gsZns(mstr,RBNR)  = ywert
         if (ipp == 49) glZns(mstr,RBNR)  = ywert
         if (ipp == 50) gsAss(mstr,RBNR)  = ywert
         if (ipp == 51) glAss(mstr,RBNR)  = ywert
      enddo ! Ende Parameterschleife
      
      if (RBtyp(mstr,RBNR) == 0) mstrRB = mstr
      if (ischwer == 1 .and. RBtyp(mstr,RBNR) == 0) then
         do i = 1,1
            hcSS = max(0., min(100.,ssalgs(mstr,RBNR)))
            hcph = vphs(mstr,RBNR)
            if (hcph < 0.0) hcph = 7.5
            call Verteilungskoeff(hcSS,hcph,VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni                        &
                                  ,VTKoeff_As,VTKoeff_Pb,VTKoeff_Cr,VTKoeff_Fe                                  &
                                  ,VTKoeff_Hg,VTKoeff_Mn,VTKoeff_U,iformVert,i)
         enddo
         if (gsZns(mstr,RBNR) > 0.0 .and. glZns(mstr,RBNR) <= 0.0) then
            glZns(mstr,RBNR) = gsZns(mstr,RBNR)/(1.+VTKoeff_Zn*hcSS/1000.)
         else if (gsZns(mstr,RBNR) <= 0.0 .and. glZns(mstr,RBNR) > 0.0) then
            gsZns(mstr,RBNR) = glZns(mstr,RBNR)*(1.+VTKoeff_Zn*hcSS/1000.)
            !           else if(gsZns(mstr,RBNR)>0.0.and.glZns(mstr,RBNR)==gsZns(mstr,RBNR))then
            !             glZns(mstr,RBNR) = gsZns(mstr,RBNR)/(1.+VTKoeff_Zn*hcSS/1000.)
         else if (gsZns(mstr,RBNR) > 0.0 .and. glZns(mstr,RBNR) > 0.0 .and. gsZns(mstr,RBNR) < glZns(mstr,RBNR)) then
            glZns(mstr,RBNR) = gsZns(mstr,RBNR)
         endif
         if (gsCads(mstr,RBNR) > 0.0 .and. glCads(mstr,RBNR) <= 0.0) then
            glCads(mstr,RBNR) = gsCads(mstr,RBNR)/(1.+VTKoeff_Cad*hcSS/1000.)
         else if (gsCads(mstr,RBNR) <= 0.0 .and. glCads(mstr,RBNR) > 0.0) then
            gsCads(mstr,RBNR) = glCads(mstr,RBNR)*(1.+VTKoeff_Cad*hcSS/1000.)
            !           else if(gsCads(mstr,RBNR)>0.0.and.glCads(mstr,RBNR)==gsCads(mstr,RBNR))then
            !             glCads(mstr,RBNR) = gsCads(mstr,RBNR)/(1.+VTKoeff_Cad*hcSS/1000.)
         else if (gsCads(mstr,RBNR) > 0.0 .and. glCads(mstr,RBNR) > 0.0.and.gsCads(mstr,RBNR) < glCads(mstr,RBNR)) then
            glCads(mstr,RBNR) = gsCads(mstr,RBNR)
         endif
         if (gsCus(mstr,RBNR) > 0.0 .and. glCus(mstr,RBNR) <= 0.0) then
            glCus(mstr,RBNR) = gsCus(mstr,RBNR)/(1.+VTKoeff_Cu*hcSS/1000.)
         else if (gsCus(mstr,RBNR) <= 0.0 .and. glCus(mstr,RBNR) > 0.0) then
            gsCus(mstr,RBNR) = glCus(mstr,RBNR)*(1.+VTKoeff_Cu*hcSS/1000.)
            !           else if(gsCus(mstr,RBNR)>0.0.and.glCus(mstr,RBNR)==gsCus(mstr,RBNR))then
            !             glCus(mstr,RBNR) = gsCus(mstr,RBNR)/(1.+VTKoeff_Cu*hcSS/1000.)
         else if (gsCus(mstr,RBNR) > 0.0 .and. glCus(mstr,RBNR) > 0.0.and.gsCus(mstr,RBNR) < glCus(mstr,RBNR)) then
            glCus(mstr,RBNR) = gsCus(mstr,RBNR)
         endif
         if (gsNis(mstr,RBNR) > 0.0 .and. glNis(mstr,RBNR) <= 0.0) then
            glNis(mstr,RBNR) = gsNis(mstr,RBNR)/(1.+VTKoeff_Ni*hcSS/1000.)
         else if (gsNis(mstr,RBNR) <= 0.0 .and. glNis(mstr,RBNR) > 0.0) then
            gsNis(mstr,RBNR) = glNis(mstr,RBNR)*(1.+VTKoeff_Ni*hcSS/1000.)
            !           else if(gsNis(mstr,RBNR)>0.0.and.glNis(mstr,RBNR)==gsZns(mstr,RBNR))then
            !             glNis(mstr,RBNR) = gsNis(mstr,RBNR)/(1.+VTKoeff_Ni*hcSS/1000.)
         else if (gsNis(mstr,RBNR) > 0.0 .and. glNis(mstr,RBNR) > 0.0.and.gsNis(mstr,RBNR) < glNis(mstr,RBNR)) then
            glNis(mstr,RBNR) = gsNis(mstr,RBNR)
         endif
         if (gsAss(mstr,RBNR) > 0.0 .and. glAss(mstr,RBNR) <= 0.0) then
            glAss(mstr,RBNR) = gsAss(mstr,RBNR)/(1.+VTKoeff_As*hcSS/1000.)
         else if (gsAss(mstr,RBNR) <= 0.0 .and. glAss(mstr,RBNR) > 0.0) then
            gsAss(mstr,RBNR) = glAss(mstr,RBNR)*(1.+VTKoeff_As*hcSS/1000.)
            !           else if(gsAss(mstr,RBNR)>0.0.and.glAss(mstr,RBNR)==gsAss(mstr,RBNR))then
            !             glAss(mstr,RBNR) = gsAss(mstr,RBNR)/(1.+VTKoeff_As*hcSS/1000.)
         else if (gsAss(mstr,RBNR) > 0.0 .and. glAss(mstr,RBNR) > 0.0.and.gsAss(mstr,RBNR) < glAss(mstr,RBNR)) then
            glAss(mstr,RBNR) = gsAss(mstr,RBNR)
         endif
         if (gsPbs(mstr,RBNR) > 0.0 .and. glPbs(mstr,RBNR) <= 0.0) then
            glPbs(mstr,RBNR) = gsPbs(mstr,RBNR)/(1.+VTKoeff_Pb*hcSS/1000.)
         else if (gsPbs(mstr,RBNR) <= 0.0 .and. glPbs(mstr,RBNR) > 0.0) then
            gsPbs(mstr,RBNR) = glPbs(mstr,RBNR)*(1+VTKoeff_Pb*hcSS/1000.)
            !           else if(gsPbs(mstr,RBNR)>0.0.and.glPbs(mstr,RBNR)==gsPbs(mstr,RBNR))then
            !             glPbs(mstr,RBNR) = gsPbs(mstr,RBNR)/(1.+VTKoeff_Pb*hcSS/1000.)
         else if (gsPbs(mstr,RBNR) > 0.0 .and. glPbs(mstr,RBNR) > 0.0.and.gsPbs(mstr,RBNR) < glPbs(mstr,RBNR)) then
            gsPbs(mstr,RBNR) = glPbs(mstr,RBNR)
         endif
         if (gsCrs(mstr,RBNR) > 0.0 .and. glCrs(mstr,RBNR) <= 0.0) then
            glCrs(mstr,RBNR) = gsCrs(mstr,RBNR)/(1.+VTKoeff_Cr*hcSS/1000.)
         else if (gsCrs(mstr,RBNR) <= 0.0 .and. glCrs(mstr,RBNR) > 0.0) then
            gsCrs(mstr,RBNR) = glCrs(mstr,RBNR)*(1.+VTKoeff_Cr*hcSS/1000.)
            !           else if(gsCrs(mstr,RBNR)>0.0.and.glCrs(mstr,RBNR)==gsCrs(mstr,RBNR))then
            !             glCrs(mstr,RBNR) = gsCrs(mstr,RBNR)/(1.+VTKoeff_Cr*hcSS/1000.)
         else if (gsCrs(mstr,RBNR) > 0.0 .and. glCrs(mstr,RBNR) > 0.0.and.gsCrs(mstr,RBNR) < glCrs(mstr,RBNR)) then
            gsCrs(mstr,RBNR) = glCrs(mstr,RBNR)
         endif
         if (gsFes(mstr,RBNR) > 0.0 .and. glFes(mstr,RBNR) <= 0.0) then
            glFes(mstr,RBNR) = gsFes(mstr,RBNR)/(1.+VTKoeff_Fe*hcSS/1000.)
         else if (gsFes(mstr,RBNR) <= 0.0 .and. glFes(mstr,RBNR) > 0.0) then
            gsFes(mstr,RBNR) = glFes(mstr,RBNR)*(1.+VTKoeff_Fe*hcSS/1000.)
            !           else if(gsFes(mstr,RBNR)>0.0.and.glFes(mstr,RBNR)==gsFes(mstr,RBNR))then
            !             glFes(mstr,RBNR) = gsFes(mstr,RBNR)/(1.+VTKoeff_Fe*hcSS/1000.)
         else if (gsFes(mstr,RBNR) > 0.0 .and. glFes(mstr,RBNR) > 0.0.and.gsFes(mstr,RBNR) < glFes(mstr,RBNR)) then
            gsFes(mstr,RBNR) = glFes(mstr,RBNR)
         endif
         if (gsHgs(mstr,RBNR) > 0.0 .and. glHgs(mstr,RBNR) <= 0.0) then
            glHgs(mstr,RBNR) = gsHgs(mstr,RBNR)/(1.+VTKoeff_Hg*hcSS/1000.)
         else if (gsHgs(mstr,RBNR) <= 0.0 .and. glHgs(mstr,RBNR) > 0.0) then
            gsHgs(mstr,RBNR) = glHgs(mstr,RBNR)*(1+VTKoeff_Hg*hcSS/1000.)
            !           else if(gsHgs(mstr,RBNR)>0.0.and.glHgs(mstr,RBNR)==gsHgs(mstr,RBNR))then
            !             glHgs(mstr,RBNR) = gsHgs(mstr,RBNR)/(1.+VTKoeff_Hg*hcSS/1000.)
         else if (gsHgs(mstr,RBNR) > 0.0 .and. glHgs(mstr,RBNR) > 0.0.and.gsHgs(mstr,RBNR) < glHgs(mstr,RBNR)) then
            gsHgs(mstr,RBNR) = glHgs(mstr,RBNR)
         endif
         if (gsMns(mstr,RBNR) > 0.0 .and. glMns(mstr,RBNR) <= 0.0) then
            glMns(mstr,RBNR) = gsMns(mstr,RBNR)/(1.+VTKoeff_Mn*hcSS/1000.)
         else if (gsMns(mstr,RBNR) <= 0.0 .and. glMns(mstr,RBNR) > 0.0) then
            gsMns(mstr,RBNR) = glMns(mstr,RBNR)*(1.+VTKoeff_Mn*hcSS/1000.)
            !           else if(gsMns(mstr,RBNR)>0.0.and.glMns(mstr,RBNR)==gsMns(mstr,RBNR))then
            !             glMns(mstr,RBNR) = gsMns(mstr,RBNR)/(1.+VTKoeff_Mn*hcSS/1000.)
         else if (gsMns(mstr,RBNR) > 0.0 .and. glMns(mstr,RBNR) > 0.0.and.gsMns(mstr,RBNR) < glMns(mstr,RBNR)) then
            gsMns(mstr,RBNR) = glMns(mstr,RBNR)
         endif
         if (gsUs(mstr,RBNR) > 0.0 .and. glUs(mstr,RBNR) <= 0.0) then
            glUs(mstr,RBNR) = gsUs(mstr,RBNR)/(1.+VTKoeff_U*hcSS/1000.)
         else if (gsUs(mstr,RBNR) <= 0.0 .and. glUs(mstr,RBNR) > 0.0) then
            gsUs(mstr,RBNR) = glUs(mstr,RBNR)*(1.+VTKoeff_U*hcSS/1000.)
            !           else if(gsUs(mstr,RBNR)>0.0.and.glUs(mstr,RBNR)==gsUs(mstr,RBNR))then
            !             glUs(mstr,RBNR) = gsUs(mstr,RBNR)/(1.+VTKoeff_U*hcSS/1000.)
         else if (gsUs(mstr,RBNR) > 0.0 .and. glUs(mstr,RBNR) > 0.0.and.gsUs(mstr,RBNR) < glUs(mstr,RBNR)) then
            gsUs(mstr,RBNR) = glUs(mstr,RBNR)
         endif
      endif
      !...Fehlermeldung
      if (ischwer == 1) then
         if (isnan(gszns(mstr,rbnr)) .or. isnan(glzns(mstr,rbnr))) then
            if (vphs(mstr,RBNR) <= 0.0)   call qerror("Missing values for pH in inflow.")
            if (ssalgs(mstr,RBNR) <= 0.0) call qerror("Missing values for suspended matter in inflow.")
            exit
         endif
      endif
   enddo ! Ende Randbedingungsschleife
   ! deallocate(mREC)
   ! deallocate(werts)
   
end subroutine funkstar
