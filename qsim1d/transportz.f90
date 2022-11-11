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

!> transportz
!! @author Volker Kirchesch
!! @date 07.01.2014
subroutine transportz(anze, deltat, izeits, isub_dt, isub_dt_Mac, dtmin_Mac,  &
                      hvmitt, elen, flag, tempwz, vnh4z, vno2z, vno3z, vo2z,  &
                      gelPz, Siz, akiz, agrz, ablz, chlaz, hgesPz, hgesNz,    &
                      nkzs, dH2D, i2Ds, iwsim, mstr, htempz, ho2z, hnh4z,     &
                      hno2z, hno3z, hgelPz, hSiz, hQ_NKz, hQ_NBz, hQ_NGz,     &
                      hakiz, hagrz, hablz, hchlaz, hchlkz, hchlgz, hchlbz,    &
                      hCChlkz, hCChlbz, hCChlgz, iflRi, dl, iMAC, Uvert,      &
                      tflie, jpoin1, itags, monats, iwied, uhrz, iverfahren,  &
                      azStrs, ianze_max, nkztot_max, Qmx_NK, Qmx_NB, Qmx_NG,  &
                      mtracer)
   
   integer                            :: anze, azStrs
   integer, dimension(azStrs)         :: i2Ds, iflRi, isub_dt, isub_dt_Mac,imac
   integer, dimension(1000)           :: flag, nkzs, iore
   integer, dimension(azStrs,anze+1)  :: nkzs_1
   
   real, dimension(1000)              :: vmitt, elen, U, DL
   real, dimension(50,1000)           :: Chlaz, tempwz, vnh4z, vno2z, vno3z, vo2z, gelPz, Siz
   real, dimension(50,1000)           :: akiz, agrz, ablz, Uvert
   real, dimension(azStrs,1000)       :: hvmitt
   real, dimension(azStrs,50,1000)    :: htempz, ho2z, hnh4z, hno2z, hno3z, hgelPz, hQ_NKz, hQ_NBz, hQ_NGz
   real, dimension(azStrs,50,1000)    :: hSiz ,hakiz, hagrz, hablz, hchlaz, hchlkz, hchlgz, hchlbz, hgesPz, hgesNz
   real, dimension(azStrs,50,1000)    :: hCChlkz, hCChlbz, hCChlgz
   

   iwahlD = 2
   nkzmax = 1
   nkz = 1
   sumdet = 0.0
   
   nkzs(anze+1) = nkzs(anze)
   kktrans = 22 ! Anzahl der Parameter
   if (iwsim == 2 .or. iwsim == 4)kktrans = 1
   
   if (iwied == 0) then
      do ior = 1,anze+1
         nkzs_1(mstr,ior) = 1
      enddo
   endif
   
   if (i2Ds(mstr) > 0) then
      
      ! Bestimmung der maximalen Anzahl der Tiefenzonen im Zeitschritt
      !$omp parallel do
      do ior = 1,anze+1
         if (nkzs(ior) > nkzmax)nkzmax = nkzs(ior)
      enddo
      !$OMP END PARALLEL Do
      i_point = 0
      call sys_gitterTrans(mstr,anze,nkzs,nkzmax,dH2D,tempwz,vo2z,vnh4z,      &
                           vno2z,vno3z,gelPz,Siz,akiz,agrz,ablz,chlaz,hchlkz, &
                           hchlgz,hchlbz,hCChlkz,hCChlbz,hCChlgz, hgesPz,     &
                           hgesNz,hQ_NKz,hQ_NBz,hQ_NGz,Uvert,i_point,         &
                           itags,monats,uhrz,azStrs)
      !$omp parallel do
      do ior = 1, anze+1
         Uvert(nkzmax,ior) = Uvert(nkzmax-1,ior)
      enddo
      !$OMP END PARALLEL Do
   else
      !$omp parallel do
      do ior = 1,anze + 1
         vmitt(ior) = hvmitt(mstr,ior)
         Uvert(1,ior) = vmitt(ior)
      enddo
      !$OMP END PARALLEL Do
   endif ! Ende i2Ds > 0
   
   do nkz = 1,nkzmax ! Schleife über vertikale Schichten
      jpoin1 = 0
      do ior = 1,anze+1
         if (Uvert(nkz,ior) < 0.0)imac(mstr) = 1
         cour = tflie*86400.*abs(Uvert(nkz,ior))/elen(ior)
         if (cour > courmx)courmx = cour
      enddo
      courmx = courmx*2.
      STRiz_2D = int(courmx)+1
      STRdt_2D = tflie*86400./STRiz_2D
      if (iverfahren > 1) then
         izeits = STRiz_2D
         deltat = STRdt_2D
      endif
      
      isub_dtx = isub_dt(mstr)
      if (imac(mstr) == 1) then
         isub_dtx = isub_dt_Mac(mstr)
         if ((deltat/isub_dtx) > dtmin_Mac)isub_dtx = int(deltat/dtmin_Mac)+1
      endif
      courmx = 0.0
      
      do itime = 1,izeits ! Zeitschleife
         do ktrans = 1, kktrans ! Parameterschleife
            
            !$omp parallel do
            do ior = 1,anze+1
               if (ktrans == 1) then
                  U(ior) = tempwz(1,ior)
                  if (nkzs(ior) > 1)U(ior) = tempwz(nkz,ior)
               endif
               if (ktrans == 2) then
                  U(ior) = vnh4z(1,ior)
                  if (nkzs(ior) > 1)U(ior) = vnh4z(nkz,ior)
               endif
               
               if (ktrans == 3) then
                  U(ior) = vno2z(1,ior)
                  if (nkzs(ior) > 1)U(ior) = vno2z(nkz,ior)
               endif
               
               if (ktrans == 4) then
                  U(ior) = vno3z(1,ior)
                  if (nkzs(ior) > 1)U(ior) = vno3z(nkz,ior)
               endif
               
               if (ktrans == 5) then
                  U(ior) = vo2z(1,ior)
                  if (nkzs(ior) > 1)U(ior) = vo2z(nkz,ior)
               endif
               
               if (ktrans == 6) then
                  U(ior) = gelPz(1,ior)
                  if (nkzs(ior) > 1)U(ior) = gelPz(nkz,ior)
               endif
               
               if (ktrans == 7) then
                  U(ior) = Siz(1,ior)
                  if (nkzs(ior) > 1)U(ior) = Siz(nkz,ior)
               endif
               
               if (ktrans == 8) then
                  U(ior) = akiz(1,ior)
                  if (nkzs(ior) > 1)U(ior) = akiz(nkz,ior)
               endif
               
               if (ktrans == 9) then
                  U(ior) = agrz(1,ior)
                  if (nkzs(ior) > 1)U(ior) = agrz(nkz,ior)
               endif
               
               if (ktrans == 10) then
                  U(ior) = ablz(1,ior)
                  if (nkzs(ior) > 1)U(ior) = ablz(nkz,ior)
               endif
               
               if (ktrans == 11) then
                  U(ior) = chlaz(1,ior)
                  if (nkzs(ior) > 1)U(ior) = chlaz(nkz,ior)
               endif
               
               if (ktrans == 12) then
                  U(ior) = hchlkz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hchlkz(mstr,nkz,ior)
               endif
               
               if (ktrans == 13) then
                  U(ior) = hchlgz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hchlgz(mstr,nkz,ior)
               endif
               
               if (ktrans == 14) then
                  U(ior) = hchlbz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hchlbz(mstr,nkz,ior)
               endif
               
               if (ktrans == 15) then
                  U(ior) = hgesPz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hgesPz(mstr,nkz,ior)
               endif
               
               if (ktrans == 16) then
                  U(ior) = hgesNz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hgesNz(mstr,nkz,ior)
               endif
               
               if (ktrans == 17) then
                  U(ior) = hQ_NKz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hQ_NKz(mstr,nkz,ior)
               endif
               
               if (ktrans == 18) then
                  U(ior) = hQ_NBz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hQ_NBz(mstr,nkz,ior)
               endif
               
               if (ktrans == 19) then
                  U(ior) = hQ_NGz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hQ_NGz(mstr,nkz,ior)
               endif
               if (ktrans == 20) then
                  U(ior) = hCChlkz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hCChlkz(mstr,nkz,ior)
               endif
               if (ktrans == 21) then
                  U(ior) = hCChlbz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hCChlbz(mstr,nkz,ior)
               endif
               if (ktrans == 22) then
                  U(ior) = hCChlgz(mstr,1,ior)
                  if (nkzs(ior) > 1)U(ior) = hCChlgz(mstr,nkz,ior)
               endif
            enddo
            !$OMP END PARALLEL Do
            
            ! if(iflRi(mstr)/=0)then ! Programmteil nur bei Strömung
            call AdvDiff(anze,elen,vmitt,Uvert,dl,flag,ktrans,U,temp0,tempn,  &
                         deltat,sumdet,itime,izeits,mstr,iwied,iwahlD,nkz,    &
                         nkzs,tflie,iFlRi,jpoin1,itags,monats,isub_dtx,imac,  &
                         iverfahren,azStrs,kktrans,nkztot_max,ianze_max,      &
                         mtracer,iwsim,uhrz)
            
            !endif  ! Ende Programmteil "Strömung"
            do ior = 1,anze+1
               if (ktrans == 1) then
                  tempwz(nkz,ior) = U(ior)
                  jpoin1 = 1
               endif
               if (ktrans == 2)vnh4z(nkz,ior) = U(ior)
               if (ktrans == 3)vno2z(nkz,ior) = U(ior)
               if (ktrans == 4)vno3z(nkz,ior) = U(ior)
               if (ktrans == 5)vo2z(nkz,ior) = U(ior)
               if (ktrans == 6)gelPz(nkz,ior) = U(ior)
               if (ktrans == 7)Siz(nkz,ior) = U(ior)
               if (ktrans == 8)akiz(nkz,ior) = U(ior)
               if (ktrans == 9)agrz(nkz,ior) = U(ior)
               if (ktrans == 10)ablz(nkz,ior) = U(ior)
               if (ktrans == 11)chlaz(nkz,ior) = U(ior)
               if (ktrans == 12)hchlkz(mstr,nkz,ior) = U(ior)
               if (ktrans == 13)hchlgz(mstr,nkz,ior) = U(ior)
               if (ktrans == 14)hchlbz(mstr,nkz,ior) = U(ior)
               if (ktrans == 15)hgesPz(mstr,nkz,ior) = U(ior)
               if (ktrans == 16)hgesNz(mstr,nkz,ior) = U(ior)
               if (ktrans == 17)hQ_NKz(mstr,nkz,ior) = min(Qmx_NK,U(ior))
               if (ktrans == 18)hQ_NBz(mstr,nkz,ior) = min(Qmx_NB,U(ior))
               if (ktrans == 19)hQ_NGz(mstr,nkz,ior) = min(Qmx_NG,U(ior))
               if (ktrans == 20)hCChlkz(mstr,nkz,ior) = U(ior)
               if (ktrans == 21)hCChlbz(mstr,nkz,ior) = U(ior)
               if (ktrans == 22)hCChlgz(mstr,nkz,ior) = U(ior)
               if (itime == izeits .and. ktrans == 22)nkzs_1(mstr,ior) = nkzs(ior)
            enddo
            
         enddo ! Ende Parameterschleife
      enddo ! Ende Zeitschleife
   enddo ! Ende Schleife über vertikale Schichten
   if (i2Ds(mstr) > 0) then
      i_point = 1
      call sys_gitterTrans(mstr,anze,nkzs,nkzmax,dH2D,tempwz,vo2z,vnh4z,vno2z, &
                           vno3z,gelPz,Siz,akiz,agrz,ablz,chlaz,hchlkz,hchlgz, &
                           hchlbz,hCChlkz,hCChlbz,hCChlgz,hgesPz,hgesNz,hQ_NKz,&
                           hQ_NBz,hQ_NGz,Uvert,i_point,itags,monats,uhrz,azStrs)
   endif
end subroutine transportz
