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

!> Einteilung eines Fliessgewässers in Segmente. 
!!
!! Segmentlaenge variabel, Zeitschritt fest gleich 60 min
!! @author Volker Kirchesch
!! @date 07.09.2015   
subroutine sysgen(ilang,dt,iwsim,nbuhn,akmB,ekmB,DLB,tau2B,alphaB,mUs                                  &
                  ,aschif,eschif,mSs,mStra,raua,bsohla,boeamq,hlboea,hflaea,htiefa                     &
                  ,hvF,hQaus,SedOM,BedGSed,sedvvert,dKorn,abfr,mStas,Startkm,mRBs,RBtyp,RBkm,ij        &
                  ,tflie,STRdt,STRiz,cpfad,wsp_UW,WSP_OW                                               &
                  ,SedOMb,w2,w2b,dKornb,SPEWKSuS,WUEBKuS,PSREFSuS,extkuS,SPEWKSS,WUEBKS,PSREFSS,extkS  &
                  ,itags,monats,uhrz,ifhStr,fhprof,iverfahren,ianze_max,HMQ,bvMQ,bHMQ,ieros)
   
   use allodim
   implicit none
   
   integer                                :: nndr, mu, ms, mstr, mstrl
   integer                                :: monats, lboemz, khyd, jz, jsgn
   integer                                :: jieinz, i, iw, iwsim, iverfahren
   integer                                :: itst, itags, isum_str, iseg, isegst
   integer                                :: ischif, ilang, ij, ihydr, ifhstr
   integer                                :: ie, ies, ieros, ieinsy, ieab
   integer                                :: id, ianzt, ianze_max, i6
   integer                                :: anze, SCHRNR, azStr, dummy
   real                                   :: vabst, wuebkz, wspl, wsplz, wflbz1
   real                                   :: w2z, vmittz, vbumz, u, uhrz
   real                                   :: tstkm, tiefz, tflie, testh, tau2_0
   real                                   :: tau2m, swflbu, sw2, svbu, sumx
   real                                   :: sumt, sumt1, sumbt1, spewksz, shbu
   real                                   :: sflbu, sedvvertzz, sbw2, sbsobu, sbsdom
   real                                   :: sblabu, sbkorn, rh, rhydz, rauz1
   real                                   :: qsausz, psrefsz, h, hcon, hbumz
   real                                   :: fl, flbumz, flaez, fkmneu, fkm1
   real                                   :: fhprof, fhconh, extksz, elenz, dx1
   real                                   :: dt, dtv, dt2, dt1, dlm
   real                                   :: dlalpm, cour, courmx, bw2nz, bvabst
   real                                   :: bsoz1, bsomnz, bsohl, bsobz1, blbumz
   real                                   :: bedgkz, bdknz, asedvvert, akornz, aisch
   real                                   :: aischz, adkorn, ks, lboe
   character (len = 255)                  :: cpfad
   character (len = 275)                  :: pfadstring
   integer, dimension(azStrs)             :: STRiz, nbuhn, mSs, mStra, isegs, mUs, abfr, mStas, mRBs
   integer, dimension(1000)               :: flag, jiein
   integer, dimension(azStrs,100)         :: RBtyp
   real, dimension(100)                   :: einlk
   real, dimension(1000)                  :: elen, tiefe, rau, vmitt, rhyd, fkm, flae, WS, qsaus, aischl, sedvvertz, lboea,lboem
   real, dimension(1000)                  :: bsohlm, flaea, rhyda, absta, tiefea, vunt, vob, fkmhyd, BedGK
   real, dimension(1000)                  :: dKornn, Qaus, WSP, hbum, flbum, bsobum, blbum, WFlbum, vbum, flbu
   real, dimension(1000)                  :: WFlbu, hbu, blabu, bsobu, vmbu, bSdOMn, bw2n, bKornn, w2n
   real, dimension(azStrs)                :: startkm, STRdt, wsp_UW, wsp_OW
   real, dimension(azStrs,100)            :: RBkm
   real, dimension(azStrs,20)             :: eschif, aschif
   real, dimension(azStrs,100)            :: akmB, ekmB, DLB, tau2B, alphaB
   real, dimension(azStrs,1000)           :: segkm, SedOM, BedGSed, sedvvert, dKorn, hQaus, hVF, hFlaea, htiefa
   real, dimension(azStrs,1000)           :: hlboea, raua, bsohla
   real, dimension(azStrs,1000)           :: boeamq, SedOMb, w2,w2b, dKornb, SPEWKSuS, WUEBKuS, PSREFSuS, SPEWKSS, WUEBKS
   real, dimension(azStrs,1000)           :: PSREFSS,extkuS, extkS, HMQ, bvMQ, bHMQ
   integer, dimension(:,:), allocatable   :: iflags, iBliak, iBreak, ieinse
   real, dimension(:,:), allocatable      :: hkmhyd, hWSP, hrhyda, hhbu, hflbu, hbsobu, hblabu, hWFlbu, hvmbu
   character(1000)                        :: message
   
   external                               :: qerror
   
   if (.not.allocated(iflags)) allocate(iflags(azStrs,1000))
   if (.not.allocated(iBliak)) allocate(iBliak(azStrs,1000))
   if (.not.allocated(iBreak)) allocate(iBreak(azStrs,1000))
   if (.not.allocated(ieinse)) allocate(ieinse(azStrs,1000))
   
   if (.not.allocated(hkmhyd)) allocate(hkmhyd(azStrs,1000))
   if (.not.allocated(hWSP))   allocate(hWSP(azStrs,1000))
   if (.not.allocated(hrhyda)) allocate(hrhyda(azStrs,1000))
   if (.not.allocated(hhbu))   allocate(hhbu(azStrs,1000))
   if (.not.allocated(hflbu))  allocate(hflbu(azStrs,1000))
   if (.not.allocated(hbsobu)) allocate(hbsobu(azStrs,1000))
   if (.not.allocated(hblabu)) allocate(hblabu(azStrs,1000))
   if (.not.allocated(hWFlbu)) allocate(hWFlbu(azStrs,1000))
   if (.not.allocated(hvmbu))  allocate(hvmbu(azStrs,1000))
   
   write(pfadstring,'(2A)')trim(adjustl(cpfad)),'sysgenou'
   open(unit = 11, file = pfadstring)
   rewind (11)
   sumx = 0.0
   sumt = 0.0
   sumt1 = 0.0
   sumbt1 = 0.0
   ianzt = 0
   testH = 0.0
   ianze_max = 0
   
   u = 0.0
   h = 0.0
   ks = 0.0
   rh = 0.0
   fl = 0.0
   WSPL = 0.0
   lboe = 0.0
   bsohl = 0.0
   aisch = 0.
   asedvvert = 0.0
   adKorn = 0.0
   sflbu = 0.0
   sWFlbu = 0.0
   shbu = 0.0
   sblabu = 0.0
   sbsobu = 0.0
   svbu = 0.0
   sbSdOM = 0.0
   sbw2 = 0.0
   sw2 = 0.0
   sbKorn = 0.0
   i = 0
   ie = 0
   iw = 0
   id = 1
   ihydr = 0
   itst = 0
   ieab = 0
   courmx = 0.0
   
   ! loeschen
   dtv = 0.0
   dt = 1e30
   dt1 = dt
   ! nndr : Anzahl der Dreissena-Kohorten
   nndr = 2
   
   ! open(unit=36,file='diffus')
   ! rewind (36)
   ! open(unit=44,file='sysgen.tst')
   ! rewind (44)
   
   ! Einlesen der Datei km_sys.dat
   pfadstring = trim(adjustl(cpfad)) // 'km_sys.dat'
   open(unit = 391, file = pfadstring)
   rewind (391)
   
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      read(391,'(I5,2x,I5)')mstr,isegs(mstr)
      do iseg = 1,isegs(mstr)
         read(391,'(f9.4,2x,i1,2x,I2)')segkm(mstr,iseg),iflags(mstr,iseg),ieinse(mstr,iseg)
      enddo
   enddo
   close(391)
   
   ! Systemgenerierung der einzelnen Stränge
   read(110,'(I5)')SCHRNR
   
   do azStr = 1,azStrs
      khyd = 1
      read(110,1111)mstrl,hkmhyd(mstrl,khyd),hWSP(mstrl,khyd)             &
           ,hQaus(mstrl,khyd),hVF(mstrl,khyd),hFlaea(mstrl,khyd), dummy   &
           ,htiefa(mstrl,khyd),hrhyda(mstrl,khyd),hlboea(mstrl,khyd)      &
           ,hflbu(mstrl,khyd),hWFlbu(mstrl,khyd),hhbu(mstrl,khyd)         &
           ,hblabu(mstrl,khyd),hbsobu(mstrl,khyd),hvmbu(mstrl,khyd)       &
           ,iBliak(mstrl,khyd),iBreak(mstrl,khyd)
      if (iwsim == 2 .or. iwsim == 4 .or. iwsim == 5)SedOM(mstrl,khyd) = 0.0
      if (ieros == 1 .and. nbuhn(mstrl) > 0) then
         if (bvmq(mstrl,khyd) > 0.0 .and. (hVF(mstrl,khyd)*0.13) > bvmq(mstrl,khyd)) then
            fhconH = htiefa(mstrl,khyd)/HMQ(mstrl,khyd)-1.
            hvmbu(mstrl,khyd) = (bvmq(mstrl,khyd) + hVF(mstrl,khyd)*0.4*fhconH)/(1.+fhconH)
            if (hhbu(mstrl,khyd) > 0.0)hhbu(mstrl,khyd) = bhMQ(mstrl,khyd)*htiefa(mstrl,khyd)/HMQ(mstrl,khyd)
         endif
      endif
      
      !  Fehlermeldung wenn htiefa kleiner 0
      if (htiefa(mstrl,khyd) <= 0.0) then
         print*, 'sysgen: Water depth is less or equal 0'
         print*, '   kmhyd = ', hkmhyd(mstrl,khyd)
         print*, '   WSP   = ', hWSP(mstrl,khyd)
         print*, '   Q     = ', hQaus(mstrl,khyd)
         write(message, "(2(a,i0))") "Subroutine sysgen: Water depth is less &
                           &or equal 0 in stretch ", mstrl, ", profile ", hkmhyd(mstrl,khyd)
         call qerror(message)
      endif
      
      if (hlboea(mstrl,khyd) > boeamq(mstrl,khyd)) hlboea(mstrl,khyd) = boeamq(mstrl,khyd)
      
      ! hQaus(mstrl,khyd) = abs(hQaus(mstrl,khyd))
      
      if (nbuhn(mstrl) == 0 )goto 233
      
      if (iBliak(mstrl,khyd) == 1 .or. iBreak(mstrl,khyd) == 1) goto 234
      goto 235
      
      234 hcon = hFlaea(mstrl,khyd)*0.01
      if (hhbu(mstrl,khyd) < 0.01 .or. hflbu(mstrl,khyd) < hcon) goto 235
      goto 233
      235 hflbu(mstrl,khyd) = hFlaea(mstrl,khyd)*0.01
      if (hWFlbu(mstrl,khyd) < 0.0)hWFlbu(mstrl,khyd) = 0.0
      hhbu(mstrl,khyd) = htiefa(mstrl,khyd)
      hblabu(mstrl,khyd) = hlboea(mstrl,khyd)
      hbsobu(mstrl,khyd) = bsohla(mstrl,khyd)
      hvmbu(mstrl,khyd) = hVF(mstrl,khyd)
      
      233 continue
      
      do khyd = 2,mStas(mstrl)
         read(110,1111)mstrl,hkmhyd(mstrl,khyd),hWSP(mstrl,khyd)             &
              ,hQaus(mstrl,khyd),hVF(mstrl,khyd),hFlaea(mstrl,khyd), dummy   &
              ,htiefa(mstrl,khyd),hrhyda(mstrl,khyd),hlboea(mstrl,khyd)      &
              ,hflbu(mstrl,khyd),hWFlbu(mstrl,khyd),hhbu(mstrl,khyd)         &
              ,hblabu(mstrl,khyd),hbsobu(mstrl,khyd),hvmbu(mstrl,khyd)       &
              ,iBliak(mstrl,khyd),iBreak(mstrl,khyd)
         
         if (ieros == 1 .and. nbuhn(mstrl) > 0) then
            if (bvmq(mstrl,khyd) > 0.0 .and. (hVF(mstrl,khyd)*0.13) > bvmq(mstrl,khyd)) then
               fhconH = htiefa(mstrl,khyd)/HMQ(mstrl,khyd)-1.
               hvmbu(mstrl,khyd) = (bvmq(mstrl,khyd) + hVF(mstrl,khyd)*0.4*fhconH)/(1.+fhconH)
               if (hhbu(mstrl,khyd) > 0.0)hhbu(mstrl,khyd) = bhMQ(mstrl,khyd)*htiefa(mstrl,khyd)/HMQ(mstrl,khyd)
            endif
         endif
         if (iwsim == 2 .or. iwsim == 4 .or. iwsim == 5)SedOM(mstrl,khyd) = 0.0
         
         !  Fehlermeldung wenn htiefa kleiner 0
         if (htiefa(mstrl,khyd) <= 0.0) then
            print*, 'sysgen: Water depth is less or equal 0'
            print*, '   kmhyd = ', hkmhyd(mstrl,khyd)
            print*, '   WSP   = ', hWSP(mstrl,khyd)
            print*, '   Q     = ', hQaus(mstrl,khyd)
            write(message, "(2(a,i0))") 'Subroutine sysgen: &
               &Water depth is less or equal 0 in stretch ',&
               mstrl, ', profile ', hkmhyd(mstrl,khyd)
            call qerror(message)
         endif
         
         if (hlboea(mstrl,khyd) > boeamq(mstrl,khyd))hlboea(mstrl,khyd) = boeamq(mstrl,khyd)
         
         ! hQaus(mstrl,khyd) = abs(hQaus(mstrl,khyd))
         ! hVF(mstrl,khyd) = abs(hVF(mstrl,khyd))
         
         if (nbuhn(mstrl) == 0) cycle
         
         if (iBliak(mstrl,khyd) == 1 .or. iBreak(mstrl,khyd) == 1)goto 213
         goto 214
         213 hcon = hFlaea(mstrl,khyd)*0.001
         if (hhbu(mstrl,khyd) < 0.01 .or. hflbu(mstrl,khyd) < hcon)goto 214
         cycle
         
         214 hflbu(mstrl,khyd) = hFlaea(mstrl,khyd)*0.001
         if (hWFlbu(mstrl,khyd) < 0.0)hWFlbu(mstrl,khyd) = 0.0
         hhbu(mstrl,khyd) = htiefa(mstrl,khyd)
         hblabu(mstrl,khyd) = hlboea(mstrl,khyd)
         hbsobu(mstrl,khyd) = bsohla(mstrl,khyd)
         hvmbu(mstrl,khyd) = hVF(mstrl,khyd)
      enddo
      Wsp_UW(mstrl) = hwsp(mstrl,1)
      Wsp_OW(mstrl) = hwsp(mstrl,mStas(mstrl))
   enddo
   
   do azStr = 1,azStrs
      jz = 0
      i6 = 0  ! anzahl der "6-flags"
      mstr = mStra(azStr)
      
      iseg = 1
      
      do khyd = 1,mStas(mstr)-1
         vob(khyd) = hVF(mstr,khyd)
         vunt(khyd) = hVF(mstr,khyd+1)
         absta(khyd) = abs(hkmhyd(mstr,khyd)-hkmhyd(mstr,khyd+1))*1000.
         
         WSP(khyd) = hWSP(mstr,khyd)
         Qaus(khyd) = hQaus(mstr,khyd)
         Flaea(khyd) = hFlaea(mstr,khyd)
         tiefea(khyd) = htiefa(mstr,khyd)
         rhyda(khyd) = hrhyda(mstr,khyd)
         lboea(khyd) = hlboea(mstr,khyd)
         fkmhyd(khyd) = hkmhyd(mstr,khyd)
         flbu(khyd) = hflbu(mstr,khyd)
         WFlbu(khyd) = hWFlbu(mstr,khyd)
         hbu(khyd) = hhbu(mstr,khyd)
         blabu(khyd) = hblabu(mstr,khyd)
         bsobu(khyd) = hbsobu(mstr,khyd)
         vmbu(khyd) = hvmbu(mstr,khyd)
      enddo
      
      !      khyd = khyd+1
      WSP(khyd) = hWSP(mstr,khyd)
      Qaus(khyd) = hQaus(mstr,khyd)
      Flaea(khyd) = hFlaea(mstr,khyd)
      tiefea(khyd) = htiefa(mstr,khyd)
      rhyda(khyd) = hrhyda(mstr,khyd)
      lboea(khyd) = hlboea(mstr,khyd)
      fkmhyd(khyd) = hkmhyd(mstr,khyd)
      flbu(khyd) = hflbu(mstr,khyd)
      WFlbu(khyd) = hWFlbu(mstr,khyd)
      hbu(khyd) = hhbu(mstr,khyd)
      blabu(khyd) = hblabu(mstr,khyd)
      bsobu(khyd) = hbsobu(mstr,khyd)
      vmbu(khyd) = hvmbu(mstr,khyd)
      
      1111 format(I5,2x,f8.3,2x,F9.4,2x,F13.6,2x,F8.5,2x,F7.1,I10,2x,f7.4,2x    &
      ,f7.3,2x,f7.2,2x,F7.1,2x,F7.1,2x,F7.4,2x,F6.2,2x,F6.2,2x,F8.5             &
      ,2x,i2,2x,i2)
      
      ieinsy = 1
      do ie = 1,mRBs(mstr)
         if (RBtyp(mstr,ie) == 0) cycle
         einlk(ie) = RBkm(mstr,ie)
         
         if (abfr(mstr) == 0 .and. einlk(ie) >= startkm(mstr)) ieinsy = ieinsy+1
         if (abfr(mstr) == 1 .and. einlk(ie) <= startkm(mstr)) ieinsy = ieinsy+1
      enddo
      
      ies = ie-1
      ie = 0
      if (ies > 0)ie = ieinsy
      
      khyd = 0
      
      20 continue
      khyd = khyd+1
      if (khyd > (mStas(mstr)-1))goto 999
      
      if (flaea(khyd) > 99999.90)flaea(khyd) = -1.0
      if (abfr(mstr) == 0 .and. fkmhyd(khyd) > startkm(mstr)) then
         if (einlk(ie) >= fkmhyd(khyd))ie = ie+1
         goto 20
      endif
      
      if (abfr(mstr) == 1 .and. fkmhyd(khyd) < startkm(mstr)) then
         if (einlk(ie) <= fkmhyd(khyd))ie = ie+1
         goto 20
      endif
      
      if (ihydr == 0) then
         fkm(1) = fkmhyd(khyd)
         ihydr = 1
         if (abfr(mstr) == 0)absta(khyd) = absta(khyd)+                     &
             ((startkm(mstr)-fkmhyd(khyd))*1000.)
         if (abfr(mstr) == 1)absta(khyd) = absta(khyd)+                     &
             ((fkmhyd(khyd)-startkm(mstr))*1000.)
         fkm(1) = startkm(mstr)
         qsaus(1) = qaus(khyd)
      endif
      
      23 vabst = (vob(khyd)+vob(khyd))/2.
      if (nbuhn(mstr) == 1)bvabst = (vmbu(khyd)+vmbu(khyd))/2.
      jsgn = 1
      if (vabst < 0.0)jsgn = -1
      if (abs(vabst) < 0.0001)vabst = 0.0001*jsgn
      
      30 dx1 = abs(vabst)*dt1
      if (iseg > isegs(mstr))goto 130
      
      if (i == 0) then
         fkm1 = fkm(1)
         goto 122
      endif
      if (abfr(mstr) == 0)fkm1 = fkm(i)-elen(i)/1000.
      if (abfr(mstr) == 1)fkm1 = fkm(i)+elen(i)/1000.
      122 if (abfr(mstr) == 0)tstkm = fkm1-((sumx+dx1)/1000.)
      if (abfr(mstr) == 1)tstkm = fkm1+((sumx+dx1)/1000.)
      if (abfr(mstr) == 0 .and. tstkm < segkm(mstr,iseg))                 &
          dx1 = ((fkm1-segkm(mstr,iseg))-sumx/1000.)*1000.
      if (abfr(mstr) == 1 .and. tstkm > segkm(mstr,iseg))                 &
          dx1 = ((abs(fkm1-segkm(mstr,iseg)))-sumx/1000.)*1000.
      
      130 dt2 = dt1
      dt2 = dx1
      if ((dx1-absta(khyd)) > 0.001) then
         dt2 = absta(khyd)/abs(vabst)
         u = u+vabst*dt2
         h = h+tiefea(khyd)*dt2
         ks = ks+raua(mstr,khyd)*dt2
         rh = rh+rhyda(khyd)*dt2
         fl = fl+flaea(khyd)*dt2
         WSPL = WSPL+WSP(khyd)*dt2
         lboe = lboe+lboea(khyd)*dt2
         bsohl = bsohl+bsohla(mstr,khyd)*dt2
         aisch = aisch+SedOM(mstr,khyd)*dt2
         asedvvert = asedvvert+sedvvert(mstr,khyd)*dt2
         adKorn = adKorn+dKorn(mstr,khyd)*dt2
         sw2 = sw2+w2(mstr,khyd)*dt2
         if (nbuhn(mstr) == 0)goto 139
         sflbu = sflbu+flbu(khyd)*dt2
         sWFlbu = sWflbu+WFlbu(khyd)*dt2
         shbu = shbu+hbu(khyd)*dt2
         sblabu = sblabu+blabu(khyd)*dt2
         sbsobu = sbsobu+bsobu(khyd)*dt2
         svbu = svbu+bvabst*dt2
         sbSdOM = sbSdOM+SedOMb(mstr,khyd)*dt2
         sbw2 = sbw2+w2b(mstr,khyd)*dt2
         sbKorn = sbKorn+dKornb(mstr,khyd)*dt2
         sumbt1 = sumbt1+dt2
         !
         139 dt1 = dt1-dt2
         sumx = sumx+absta(khyd)
         sumt = sumt+dt2
         sumt1 = sumt1+dt2
         goto 20
      endif
      
      dt2 = dx1/abs(vabst)
      u = u+vabst*dt2
      h = h+tiefea(khyd)*dt2
      ks = ks+raua(mstr,khyd)*dt2
      rh = rh+rhyda(khyd)*dt2
      fl = fl+flaea(khyd)*dt2
      WSPL = WSPL+WSP(khyd)*dt2
      lboe = lboe+lboea(khyd)*dt2
      bsohl = bsohl+bsohla(mstr,khyd)*dt2
      aisch = aisch+SedOM(mstr,khyd)*dt2
      asedvvert = asedvvert+sedvvert(mstr,khyd)*dt2
      adKorn = adKorn+dKorn(mstr,khyd)*dt2
      sw2 = sw2+w2(mstr,khyd)*dt2
      if (nbuhn(mstr) == 0)goto 138
      sflbu = sflbu+flbu(khyd)*dt2
      sWFlbu = sWflbu+WFlbu(khyd)*dt2
      shbu = shbu+hbu(khyd)*dt2
      sblabu = sblabu+blabu(khyd)*dt2
      sbsobu = sbsobu+bsobu(khyd)*dt2
      svbu = svbu+bvabst*dt2
      sbSdOM = sbSdOM+SedOMb(mstr,khyd)*dt2
      sbw2 = sbw2+w2b(mstr,khyd)*dt2
      sbKorn = sbKorn+dKornb(mstr,khyd)*dt2
      sumbt1 = sumbt1+dt2
      
      138 sumx = sumx+dx1
      sumt = sumt+dt2
      sumt1 = sumt1+dt2
      i = i+1
      
      ! --- Fehlermeldung ---
      if (i > 1000) then
         write(message, "(a,i0)") "Number of nodes exceeds 1000 in stretch ", mstr
         call qerror(message)
      endif
      
      elen(i) = sumx
      if (elen(i) <= 0.001)elen(i) = 0.001
      vmitt(i) = u/sumt1
      rau(i) = ks/sumt1
      tiefe(i) = h/sumt1
      rhyd(i) = rh/sumt1
      flae(i) = fl/sumt1
      WS(i) = WSPL/sumt1
      lboem(i) = lboe/sumt1
      bsohlm(i) = bsohl/sumt1
      aischl(i) = aisch/sumt1
      sedvvertz(i) = asedvvert/sumt1
      BedGK(i) = BedGSed(mstr,khyd)
      dKornn(i) = adKorn/sumt1
      w2n(i) = sw2/sumt1
      SPEWKSS(mstr,i) = SPEWKSuS(mstr,khyd)
      WUEBKS(mstr,i) = WUEBKuS(mstr,khyd)
      PSREFSS(mstr,i) = PSREFSuS(mstr,khyd)
      extkS(mstr,i) = extkuS(mstr,khyd)
      qsaus(i) = qaus(i-i6)
      if (sumbt1 == 0) then
         flbum(i)  = -1.
         WFlbum(i) = -1.
         hbum(i)   = -1.
         blbum(i)  = -1.
         bsobum(i) = -1.
         vbum(i)   = -1.
         bSdOMn(i) = -.1
         bw2n(i)   = -1.
         bKornn(i) = -1.
         goto 141
      endif
      flbum(i)  = sflbu/sumbt1
      WFlbum(i) = sWFlbu/sumbt1
      hbum(i)   = shbu/sumbt1
      blbum(i)  = sblabu/sumbt1
      bsobum(i) = sbsobu/sumbt1
      vbum(i)   = svbu/sumbt1
      bSdOMn(i) = sbSdOM/sumbt1
      bw2n(i)   = sbw2/sumbt1
      bKornn(i) = sbKorn/sumbt1
      141 if (i == 1)goto 930
      
      if (i > 1 .and. abfr(mstr) == 0)fkm(i) = fkm(i-1)-elen(i-1)/1000.
      if (i > 1 .and. abfr(mstr) == 1)fkm(i) = fkm(i-1)+elen(i-1)/1000.
      if (flag(i) == 4 .and. qsaus(i)>=0.0) then
         !      jiein(i+1) = jiein(i)
         jieinz = jiein(i)
         jiein(i) = 0
         elenz = elen(i)
         vmittz = vmitt(i)
         rauz1 = rau(i)
         tiefz = tiefe(i)
         rhydz = rhyd(i)
         flaez = flae(i)
         WSPLz = WS(i)
         lboemz = lboem(i)
         bsoz1 = bsohlm(i)
         aischz = aischl(i)
         sedvvertzz = sedvvertz(i)
         BedGKz = BedGK(i)
         aKornz = dKornn(i)
         w2z = w2n(i)
         SPEWKSz = SPEWKSS(mstr,i)
         WUEBKz = WUEBKS(mstr,i)
         PSREFSz = PSREFSS(mstr,i)
         extkSz = extkS(mstr,i)
         qsausz = qsaus(i)
         flbumz = flbum(i)
         WFlbz1 = WFlbum(i)
         hbumz = hbum(i)
         blbumz = blbum(i)
         bsobz1 = bsobum(i)
         vbumz = vbum(i)
         bSOMnz = bSdOMn(i)
         bw2nz = bw2n(i)
         bdKnz = bKornn(i)
         flag(i) = 6
         qsaus(i) = qsaus(i-1)  ! dies ist neu!
         ! vmitt(i) = vmitt(i-1)  ! dies ist neu
         ! aischl(i) = aischl(i-1)
         ! qsaus(i) = qsaus(i-i6-1)
         qsaus(i) = qsaus(i-1)
         i = i+1
         jiein(i) = jieinz
         fkm(i) = fkm(i-1)
         elen(i) = elenz
         flag(i) = 4
         vmitt(i) = vmittz
         rau(i) = rauz1
         tiefe(i) = tiefz
         rhyd(i) = rhydz
         flae(i) = flaez
         WS(i) = WSPLz
         lboem(i) = lboemz
         bsohlm(i) = bsoz1
         aischl(i) = aischz
         sedvvertz(i) = sedvvertzz
         BedGK(i) = BedGKz
         dKornn(i) = aKornz
         w2n(i) = w2z
         SPEWKSS(mstr,i) = SPEWKSz
         WUEBKS(mstr,i) = WUEBKz
         PSREFSS(mstr,i) = PSREFSz
         extkS(mstr,i) = extkSz
         qsaus(i) = qsausz
         i6 = i6+1
         flbum(i) = flbumz
         WFlbum(i) = WFlbz1
         hbum(i) = hbumz
         blbum(i) = blbumz
         bsobum(i) = bsobz1
         vbum(i) = vbumz
         bSdOMn(i) = bSOMnz
         bw2n(i) = bw2nz
         bKornn(i) = bdKnz
      else if (flag(i) == 4 .and. qsaus(i) < 0.0) then
         ! Q an der Einleitstelle <0.0
         i = i+1
         jiein(i) = 0
         fkm(i) = fkm(i-1)
         elen(i) = elen(i-1)
         flag(i) = 6
         vmitt(i) = vmitt(i-1)
         rau(i) = rau(i-1)
         tiefe(i) = tiefe(i-1)
         rhyd(i) = rhyd(i-1)
         flae(i) = flae(i-1)
         WS(i) = WS(i-1)
         lboem(i) = lboem(i-1)
         bsohlm(i) = bsohlm(i-1)
         aischl(i) = aischl(i-1)
         sedvvertz(i) = sedvvertz(i-1)
         BedGK(i) = BedGK(i-1)
         dKornn(i) = dKornn(i-1)
         w2n(i) = w2n(i-1)
         SPEWKSS(mstr,i) = SPEWKSS(mstr,i-1)
         WUEBKS(mstr,i) = WUEBKS(mstr,i-1)
         PSREFSS(mstr,i) = PSREFSS(mstr,i-1)
         qsaus(i) = qsaus(i-1)
         i6 = i6+1
         flbum(i) = flbum(i-1)
         WFlbum(i) = WFlbum(i-1)
         hbum(i) = hbum(i-1)
         blbum(i) = blbum(i-1)
         bsobum(i) = bsobum(i-1)
         vbum(i) = vbum(i-1)
         bSdOMn(i) = bSdOMn(i-1)
         bw2n(i) = bw2n(i-1)
         bKornn(i) = bKornn(i-1)
      endif
      if (itst == 0)flag(i) = 2
      ie = ie-ieab
      ieab = 0
      
      930 absta(khyd) = absta(khyd)-dx1
      u = 0.0
      h = 0.0
      ks = 0.0
      rh = 0.0
      fl = 0.0
      WSPL = 0.0
      lboe = 0.0
      bsohl = 0.0
      sflbu = 0.0
      sWFlbu = 0.0
      shbu = 0.0
      sblabu = 0.0
      sbsobu = 0.0
      svbu = 0.0
      sbSdOM = 0.0
      sbw2 = 0.0
      sbKorn = 0.0
      if (itst == 0)jiein(i) = 0
      itst = 0
      aisch = 0.
      asedvvert = 0.0
      adKorn = 0.0
      sw2 = 0.0
      if (abs(sumt-dt) < 2.) then
         dt1 = dt
         goto 937
      endif
      dt1 = dt-sumt
      goto 939
      937 sumt = 0.0
      939 sumx = 0.0
      sumt1 = 0.0
      sumbt1 = 0.0
      
      if (i == 1) then
         flag(i) = 1
         qsaus(i) = qaus(1)
      endif
      ! flag(i) = 2
      if (abfr(mstr) == 0)fkmneu = fkm(i)-elen(i)/1000.
      if (abfr(mstr) == 1)fkmneu = fkm(i)+elen(i)/1000.
      if (abs(fkmneu-segkm(mstr,iseg)) < 0.01) flag(i+1) = iflags(mstr,iseg)
      
      jiein(i+1) = ieinse(mstr,iseg)
      itst = 1
      iseg = iseg+1
      
      9 continue
      if (khyd == (mStas(mstr)-1) .and. absta(khyd) < 0.001) then
         ! flag(i) = 2
         if (i == 1)flag(i) = 1
         goto 41
      endif
      if (absta(khyd) < 0.01)goto 20
      goto 30
      
      ! --- Ende der Systemgenerierung ---
      999 continue
      if (sumt < dt) then
         dtv = dt
         dt = sumt
      endif
      !
      if (i == 0) then
         i = 1
         elen(i) = sumx
         if (elen(i) <= 0.001)elen(i) = 0.001
         vmitt(i) = u/sumt1
         rau(i) = ks/sumt1
         tiefe(i) = h/sumt1
         rhyd(i) = rh/sumt1
         aischl(i) = aisch/sumt1
         sedvvertz(i) = asedvvert/sumt1
         BedGK(i) = BedGSed(mstr,1)
         dKornn(i) = adKorn/sumt1
         w2n(i) = sw2/sumt1
         SPEWKSS(mstr,i) = SPEWKSuS(mstr,1)
         WUEBKS(mstr,i) = WUEBKuS(mstr,1)
         SPEWKSS(mstr,i) = SPEWKSuS(mstr,1)
         extkS(mstr,i) = extkuS(mstr,1)
         
         flae(i) = fl/sumt1
         WS(i) = WSPL/sumt1
         lboem(i) = lboe/sumt1
         bsohlm(i) = bsohl/sumt1
         qsaus(i) = qaus(1)
         if (sumbt1 == 0) then
            flbum(i) = -1.
            WFlbum(i) = -1.
            hbum(i) = -1.
            blbum(i) = -1.
            bsobum(i) = -1.
            vbum(i) = -1.
            bSdOMn(i) = -.1
            bw2n(i) = -1.
            bKornn(i) = -1.
            goto 142
         endif
         flbum(i) = sflbu/sumbt1
         WFlbum(i) = sWFlbu/sumbt1
         hbum(i) = shbu/sumbt1
         blbum(i) = sblabu/sumbt1
         bsobum(i) = sbsobu/sumbt1
         vbum(i) = svbu/sumbt1
         bSdOMn(i) = sbSdOM/sumbt1
         bw2n(i) = sbw2/sumbt1
         bKornn(i) = sbKorn/sumbt1
         
         142 if (flag(i) /= 4)flag(i) = 1
         
         goto 41
      endif
      
      i = i+1
      
      if (abfr(mstr) == 0)fkm(i) = fkm(i-1)-elen(i-1)/1000.
      if (abfr(mstr) == 1)fkm(i) = fkm(i-1)+elen(i-1)/1000.
      
      elen(i) = sumx
      if (elen(i) <= 0.001)elen(i) = 0.001
      vmitt(i) = u/sumt1
      rau(i) = ks/sumt1
      tiefe(i) = h/sumt1
      rhyd(i) = rh/sumt1
      aischl(i) = aisch/sumt1
      sedvvertz(i) = asedvvert
      BedGK(i) = BedGSed(mstr,1)
      dKornn(i) = adKorn/sumt1
      w2n(i) = sw2/sumt1
      SPEWKSS(mstr,i) = SPEWKSuS(mstr,1)
      WUEBKS(mstr,i) = WUEBKuS(mstr,1)
      PSREFSS(mstr,i) = PSREFSuS(mstr,1)
      extkS(mstr,i) = extkuS(mstr,1)
      
      flae(i) = fl/sumt1
      WS(i) = WSPL/sumt1
      lboem(i) = lboe/sumt1
      bsohlm(i) = bsohl/sumt1
      ! if(flag(i).ne.4)flag(i) = 8
      ! if(mwehr(mstr).eq.1)flag(i) = 5
      ! qsaus(i) = qaus(mStas(mstr))
      qsaus(i) = qaus(i-i6)
      !
      if (sumbt1 == 0) then
         flbum(i) = -1.
         WFlbum(i) = -1.
         hbum(i) = -1.
         blbum(i) = -1.
         bsobum(i) = -1.
         vbum(i) = -1.
         bSdOMn(i) = -.1
         bw2n(i) = -1.
         bKornn(i) = -1.
         goto 42
      endif
      flbum(i) = sflbu/sumbt1
      WFlbum(i) = sWFlbu/sumbt1
      hbum(i) = shbu/sumbt1
      blbum(i) = sblabu/sumbt1
      bsobum(i) = sbsobu/sumbt1
      vbum(i) = svbu/sumbt1
      bSdOMn(i) = sbSdOM/sumbt1
      bw2n(i) = sbw2/sumbt1
      bKornn(i) = sbKorn/sumbt1
      42 if (flag(i) == 4) then
         jiein(i+1) = jiein(i)
         elenz = elen(i)
         vmittz = vmitt(i)
         rauz1 = rau(i)
         tiefz = tiefe(i)
         rhydz = rhyd(i)
         flaez = flae(i-1)
         WSPLz = WS(i-1)
         lboemz = lboem(i)
         bsoz1 = bsohlm(i)
         aischz = aischl(i)
         sedvvertzz = sedvvertz(i)
         BedGKz = BedGK(i)
         SPEWKSz = SPEWKSS(mstr,i)
         WUEBKz = WUEBKS(mstr,i)
         PSREFSz = PSREFSS(mstr,i)
         extkSz = extkS(mstr,i)
         aKornz = dKornn(i)
         w2z = w2n(i)
         qsausz = qsaus(i)
         flbumz = flbum(i)
         WFlbz1 = WFlbum(i)
         hbumz = hbum(i)
         blbumz = blbum(i)
         bsobz1 = bsobum(i)
         vbumz = vbum(i)
         bSOMnz = bSdOMn(i)
         bw2nz = bw2n(i)
         bdKnz = bKornn(i)
         elen(i) = elen(i-1)
         qsaus(i) = qsaus(i-1)
         vmitt(i) = vmitt(i-1)
         rau(i) = rau(i-1)
         tiefe(i) = tiefe(i-1)
         rhyd(i) = rhyd(i-1)
         flae(i) = flae(i-1)
         WS(i) = WS(i-1)
         lboem(i) = lboem(i-1)
         bsohlm(i) = bsohlm(i-1)
         aischl(i) = aischl(i-1)
         sedvvertz(i) = sedvvertz(i-1)
         BedGK(i) = BedGK(i-1)
         dKornn(i) = dKornn(i-1)
         w2n(i) = w2n(i-1)
         SPEWKSS(mstr,i) = SPEWKSS(mstr,i-1)
         WUEBKS(mstr,i) = WUEBKS(mstr,i-1)
         PSREFSS(mstr,i) = PSREFSS(mstr,i-1)
         extkS(mstr,i) = extkS(mstr,i-1)
         qsaus(i) = qsaus(i-1)
         flbum(i) = flbum(i-1)
         WFlbum(i) = WFlbum(i-1)
         hbum(i) = hbum(i-1)
         blbum(i) = blbum(i-1)
         bsobum(i) = bsobum(i-1)
         vbum(i) = vbum(i-1)
         bSdOMn(i) = bSdOMn(i-1)
         bw2n(i) = bw2n(i-1)
         bKornn(i) = bKornn(i-1)
         flag(i) = 6
         jiein(i) = 0
         i = i+1
         fkm(i) = fkm(i-1)
         elen(i) = elenz
         flag(i) = 4
         vmitt(i) = vmittz
         rau(i) = rauz1
         tiefe(i) = tiefz
         rhyd(i) = rhydz
         flae(i) = flaez
         WS(i) = WSPLz
         lboem(i) = lboemz
         bsohlm(i) = bsoz1
         aischl(i) = aischz
         sedvvertz(i) = sedvvertzz
         BedGK(i) = BedGKz
         dKornn(i) = aKornz
         w2n(i) = w2z
         SPEWKSS(mstr,i) = SPEWKSz
         WUEBKS(mstr,i) = WUEBKz
         PSREFSS(mstr,i) = PSREFSz
         extkS(mstr,i) = extkSz
         qsaus(i) = qsausz
         flbum(i) = flbumz
         WFlbum(i) = WFlbz1
         hbum(i) = hbumz
         blbum(i) = blbumz
         bsobum(i) = bsobz1
         vbum(i) = vbumz
         bSdOMn(i) = bSOMnz
         bw2n(i) = bw2nz
         bKornn(i) = bdKnz
      endif
      41 continue
      
      if (iflags(mstr,isegs(mstr)) == 4 .and. qsaus(i)>=0.0) then
         if (flag(i) == 4) then
            jiein(i) = jiein(i)+ieinse(mstr,isegs(mstr))
         else
            flag(i) = 6
            i = i+1
            ! jiein(i) = jiein(i)+1
            elen(i) = elen(i-1)
            qsaus(i) = qsaus(i-1)
            vmitt(i) = vmitt(i-1)
            rau(i) = rau(i-1)
            tiefe(i) = tiefe(i-1)
            rhyd(i) = rhyd(i-1)
            flae(i) = flae(i-1)
            WS(i) = WS(i-1)
            lboem(i) = lboem(i-1)
            bsohlm(i) = bsohlm(i-1)
            aischl(i) = aischl(i-1)
            sedvvertz(i) = sedvvertz(i-1)
            BedGK(i) = BedGK(i-1)
            dKornn(i) = dKornn(i-1)
            w2n(i) = w2n(i-1)
            SPEWKSS(mstr,i) = SPEWKSS(mstr,i-1)
            WUEBKS(mstr,i) = WUEBKS(mstr,i-1)
            PSREFSS(mstr,i) = PSREFSS(mstr,i-1)
            extkS(mstr,i) = extkS(mstr,i-1)
            qsaus(i) = qsaus(i-1)
            flbum(i) = flbum(i-1)
            WFlbum(i) = WFlbum(i-1)
            hbum(i) = hbum(i-1)
            blbum(i) = blbum(i-1)
            bsobum(i) = bsobum(i-1)
            vbum(i) = vbum(i-1)
            bSdOMn(i) = bSdOMn(i-1)
            bw2n(i) = bw2n(i-1)
            bKornn(i) = bKornn(i-1)
            fkm(i) = fkm(i-1)
            flag(i) = 4
            jiein(i) = ieinse(mstr,isegs(mstr))
         endif
      else if (iflags(mstr,isegs(mstr)) == 4 .and. qsaus(i) < 0.0) then
         if (flag(i-1) == 4) then
            jiein(i-1) = jiein(i-1)+ieinse(mstr,isegs(mstr))
         else
            flag(i) = 4
            jiein(i) = ieinse(mstr,isegs(mstr))
            i = i+1
            flag(i) = 6
            elen(i) = elen(i-1)
            qsaus(i) = qsaus(i)
            vmitt(i) = vmitt(i-1)
            rau(i) = rau(i-1)
            tiefe(i) = tiefe(i-1)
            rhyd(i) = rhyd(i-1)
            flae(i) = flae(i-1)
            WS(i) = WS(i-1)
            lboem(i) = lboem(i-1)
            bsohlm(i) = bsohlm(i-1)
            aischl(i) = aischl(i-1)
            sedvvertz(i) = sedvvertz(i-1)
            BedGK(i) = BedGK(i-1)
            dKornn(i) = dKornn(i-1)
            w2n(i) = w2n(i-1)
            SPEWKSS(mstr,i) = SPEWKSS(mstr,i-1)
            WUEBKS(mstr,i) = WUEBKS(mstr,i-1)
            PSREFSS(mstr,i) = PSREFSS(mstr,i-1)
            extkS(mstr,i) = extkS(mstr,i-1)
            qsaus(i) = qsaus(i-1)
            flbum(i) = flbum(i-1)
            WFlbum(i) = WFlbum(i-1)
            hbum(i) = hbum(i-1)
            blbum(i) = blbum(i-1)
            bsobum(i) = bsobum(i-1)
            vbum(i) = vbum(i-1)
            bSdOMn(i) = bSdOMn(i-1)
            bw2n(i) = bw2n(i-1)
            bKornn(i) = bKornn(i-1)
            fkm(i) = fkm(i-1)
         endif
      endif
      
      anze = i
      isegSt = 1
      mS = 1
      mU = 1
      
      ! Nullsetzen
      u = 0.0
      h = 0.0
      ks = 0.0
      rh = 0.0
      fl = 0.0
      WSPL = 0.0
      lboe = 0.0
      bsohl = 0.0
      sflbu = 0.0
      sWFlbu = 0.0
      shbu = 0.0
      sblabu = 0.0
      sbsobu = 0.0
      svbu = 0.0
      sbSdOM = 0.0
      sbw2 = 0.0
      sbKorn = 0.0
      itst = 0
      aisch = 0.
      asedvvert = 0.0
      sumx = 0.0
      sumt = 0.0
      sumt1 = 0.0
      sumbt1 = 0.0
      if (dtv > 0.0)dt = dtv
      
      ie = ieinsy
      isum_Str = 0.0
      write(11,2000)anze
      
      khyd = 0
      do i = 1,anze
         if (elen(i) <= 0.001)elen(i) = 0.001
         
         ! Elemente mit Schiffsverkehr
         ischif = 0
         
         if (mSs(mstr) == 0 .or. mS > mSs(mstr))goto 944
         if (abfr(mstr) == 1)goto 662
         
         if (fkm(i) <= aschif(mstr,mS) .and. fkm(i) >= eschif(mstr,mS)) then
            ischif = 1
            goto 944
         endif
         
         if (fkm(i) <= eschif(mstr,mS) .and. fkm(i) <= aschif(mstr,mS+1)) then
            mS = mS+1
            ischif = 1
            goto 944
         endif
         
         662 continue
         if (fkm(i) >= aschif(mstr,mS) .and. fkm(i) <= eschif(mstr,mS)) then
            ischif = 1
            goto 944
         endif
         
         if (fkm(i) >= eschif(mstr,mS) .and. fkm(i) >= aschif(mstr,mS+1)) then
            mS = mS+1
            ischif = 1
         endif
         
         ! Buhnenfelder
         944 continue
         dlm = -1.
         dlalpm = -1.
         tau2m = -1.
         
         if (nbuhn(mstr) == 0)goto 444
         
         if (mU > mUs(mstr))goto 444
         if (abfr(mstr) == 1)goto 956
         
         if (fkm(i) <= akmB(mstr,mU) .and. fkm(i) >= ekmB(mstr,mU)) then
            dlm = DLB(mstr,mU)
            dlalpm = alphaB(mstr,mU)
            tau2m = tau2B(mstr,mU)
            goto 444
         endif
         
         if (fkm(i) <= ekmB(mstr,mU) .and. fkm(i) <= akmB(mstr,mU+1)) then
            mU = mU+1
            dlm = DLB(mstr,mU)
            dlalpm = alphaB(mstr,mU)
            tau2m = tau2B(mstr,mU)
            goto 444
         endif
         
         956 continue
         if (fkm(i) >= akmB(mstr,mU) .and. fkm(i) <= ekmB(mstr,mU)) then
            dlm = DLB(mstr,mU)
            dlalpm = alphaB(mstr,mU)
            tau2m = tau2B(mstr,mU)
            goto 444
         endif
         
         if (fkm(i) >= ekmB(mstr,mU) .and. fkm(i) >= akmB(mstr,mU+1)) then
            mU = mU+1
            dlm = DLB(mstr,mU)
            dlalpm = alphaB(mstr,mU)
            tau2m = tau2B(mstr,mU)
         endif
         
         444 continue
         
         ! Bestimmung der Courant-Zahl für Advektion
         cour = tflie*86400.*abs(vmitt(i))/elen(i)
         
         if (cour > courmx)courmx = cour
         
         if (flbum(i) <= 0.0)vbum(i) = -1.
         if (nbuhn(mstr) == 0) goto 646
         
         tau2_0 = tau2m
         if (tau2_0 <= 0.0) then
            call qerror("Missing Value for 'tau2b' (Exchangetime between &
                        &groyne field and main river)")
         endif
         tau2m = tau2_0/(1.+qsaus(i)/400.)
         
         646 continue
         if (rhyd(i) < 0.0)rhyd(i) = tiefe(i)
         if (WFlbum(i) <= 0.01)tau2m = 0.02
         write(11,2010)fkm(i),flag(i),jiein(i),elen(i),vmitt(i),tiefe(i)                                       &
                       ,rau(i),rhyd(i),aischl(i),w2n(i),BedGK(i),sedvvertz(i),dKornn(i),flae(i),WS(i),ischif   &
                       ,lboem(i),bsohlm(i),qsaus(i),hbum(i),flbum(i),bsobum(i)                                 &
                       ,blbum(i),WFlbum(i),dlm,tau2m,vbum(i),bSdOMn(i),bw2n(i),bKornn(i)                       &
                       ,dlalpm
      enddo
      
      2000 format(i4)
      
      2010 format(f8.3,2x,i1,2x,i2                                               &
      ,2x,f9.3,2x,f7.4,2x,f5.2,2x,f5.2,2x,f5.2                                   &
      ,2x,f8.5,2x,e12.5,2x,f5.2,2x,f9.4,2x,e12.5,2x,f8.2,2x,f9.4,2x,i1,2x,f9.4   &
      ,2x,f9.4,2x,f14.6,2x,f5.2,2x,f7.2,2x,f7.2,2x,f7.2,2x                       &
      ,f7.2,2x,f7.2,2x,f7.2,2x,f7.4,2x,f8.5,2x,e12.5,2x,e12.5,2x,f6.2)
      
      sumx = 0.0
      sumt = 0.0
      sumt1 = 0.0
      sumbt1 = 0.0
      u = 0.0
      h = 0.0
      ks = 0.0
      rh = 0.0
      fl = 0.0
      WSPL = 0.0
      bsohl = 0.0
      lboe = 0.0
      aisch = 0.0
      sflbu = 0.0
      sWFlbu = 0.0
      shbu = 0.0
      sblabu = 0.0
      sbsobu = 0.0
      svbu = 0.0
      i = 0
      dt1 = dt
      ihydr = 0
      ianzt = 0
      
      flag(:) = 0
      
      ! Bestimmung der Zeitschrittweite für Advektion und Dispersion
      if (iverfahren == 1) then
         courmx = courmx*0.4
      else
         courmx = courmx*2.
      endif
      STRiz(mstr) = int(courmx)+1
      STRdt(mstr) = tflie*86400./STRiz(mstr)
      courmx = 0.0
      
      if ((anze+1) > ianze_max)ianze_max = anze + 1
   enddo
   
   close (11)
   
   return
end
