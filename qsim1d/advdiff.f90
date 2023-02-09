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

!> advDiff
!! @author Volker Kirchesch
!! @date 27.01.2009
subroutine AdvDiff(anze,elen,vmitt,Uvert,dl,flag,ktrans,U,temp0,tempn,deltat,sumdet,itime,izeits,mstr,iwied         &
                   ,iwahlD,nkz,nkzs,tflie,iFlRi,jpoin1,itags,monats,isub_dtx,imac,iverfahren,kktrans,nkztot_max     &
                   ,ianze_max,mtracer,iwsim,uhrz)
   use allodim
   implicit none
   
   integer                                :: nkztot_max, mtracer, mstr, monats, ktrans
   integer                                :: kktrans, jpoin1, izeits, iwsim, iwied
   integer                                :: iwahld, iverfahren, itime, ithomas, itags
   integer                                :: isub_dtx, ipo, icranickoeff, ianze_max
   real                                   :: vx_cr, uhrz, tflie, tfliesec, tempn
   real                                   :: temp0, sumdet, s2, s1, dx2
   real                                   :: dx1, deltat
   integer                                :: anze, nkz, ior
   integer, dimension(azStrs)             :: iFlRi, imac
   integer, dimension(1000)               :: flag, nkzs
   integer, dimension(1000,50)            :: m, isgn
   real, dimension(1000)                  :: vmitt, elen, dl, U,CUx
   real, dimension(1000,50)               :: xpoint
   real, dimension(50,1000)               :: vmittt_1,Uvert
   real, allocatable, dimension(:,:,:)    :: Uvertt_1
   real, allocatable, dimension(:,:,:)    :: CU
   real, allocatable, dimension(:,:,:,:)  :: CUz
   
   external :: basispoint, cip, lax_wendroff, quickest, crank_nicolson, maccorm
   
   save CUz, vmittt_1, Uvertt_1, isgn, m, CUx, xpoint, CU
   
   
   icraNicKoeff = 0
   ! iverfahren = 1   ! cip
   ! iverfahren = 2   ! Lax_wendroff
   ! iverfahren = 3   ! ultimate Quickest
   
   ithomas = 0      ! Loesung der tridiagonal Matrix nach dem: 1: Thomas-Verfahren; 0: nach Gauss
   if (iwahlD == 2) then
      if ( .not. allocated(CUz))allocate(CUz(1:kktrans,1:azStrs,1:nkztot_max,1:ianze_max))
      if ( .not. allocated(Uvertt_1))allocate(Uvertt_1(azStrs,nkztot_max,ianze_max+1))
   else
      if ( .not. allocated(CU))allocate(CU(1:kktrans,1:azStrs,1:ianze_max+1))
   endif
   tfliesec = tflie*86400.
   deltat = tflie*86400./izeits
   if (iwsim == 4 .and. mtracer == 0)iwied = 0
   if (iwied == 0) then ! CU zu Beginn der Simulation fuer CIP-Verfahren
      do ior = 1,anze+1
         if (ior == 1) then
            s1 = U(ior)
            s2 = U(ior+1)
            dx1 = elen(ior)
            dx2 = 0.0
         else if (ior == anze+1 .or. flag(ior+1) == 4) then
            S1 = U(ior-1)
            S2 = U(ior)
            dx1 = elen(ior-1)
            dx2 = 0.0
         else
            S1 = U(ior-1)
            s2 = U(ior+1)
            dx1 = elen(ior-1)
            dx2 = elen(ior)
         endif
         
         if (iwahlD == 1) then
            CU(ktrans,mstr,ior) = (S2-S1)/(dx1+dx2)
         else
            CUz(ktrans,mstr,nkz,ior) = (S2-S1)/(dx1+dx2)
         endif
      enddo
   endif
   
   if (iwied == 1) then
      do ior = 1,anze+1
         
         if (ior == 1) then
            S1 = U(ior)
            S2 = U(ior+1)
            dx1 = elen(ior)
            dx2 = 0.0
         else if (ior == anze+1 .or. flag(ior+1) == 4) then
            S1 = U(ior-1)
            S2 = U(ior)
            dx1 = elen(ior-1)
            dx2 = 0.0
         else
            S1 = U(ior-1)
            S2 = U(ior+1)
            dx1 = elen(ior-1)
            dx2 = elen(ior)
         endif
         if (iwahlD == 1) then
            if (ior == 1)CU(ktrans,mstr,ior) = (S2-S1)/(dx1+dx2)
         endif
         if (iwahlD == 2) then
            if (CUz(ktrans,mstr,nkz,ior) == 0.0 .or. ior == 1)CUz(ktrans,mstr,nkz,ior) = (S2-S1)/(dx1+dx2)
         endif
      enddo
   endif
   !    ipo = itime
   if (iwsim == 4) then
      mtracer = 1
      iwied = 1
   endif
   if (iverfahren == 1) then
      if (ktrans == 1 .and. nkz == 1)sumdet = sumdet+deltat
      if (jpoin1 == 1) then
      else
         
         call basisPoint(anze,flag,deltat,vmitt,Uvert,Uvertt_1,vmittt_1,elen,xPoint,m,iwied,itime,izeits  &
                         ,jpoin1,sumdet,tfliesec,nkz,nkzs,iwahlD,ipo,mstr,itags,monats,ktrans             &
                         ,isgn,vx_Cr,nkztot_max,ianze_max,uhrz)
      endif
   endif
   if (iverfahren == 1) then
      if (iwahlD == 1) then
         CUx(1:anze+1) = CU(ktrans,mstr,1:anze+1)
      else
         CUx(1:anze+1) = CUz(ktrans,mstr,nkz,1:anze+1)
      endif
   endif
   if (iverfahren == 1) then
      call CIP(U, CUx, elen, DeltaT, anze, xpoint, flag, nkz, nkzs,ktrans,kktrans,itime,mstr    &
               ,iwahld, nkztot_max, ianze_max,itags,isgn,m,vx_Cr,Uhrz,monats,temp0,deltat)
      if (iwahlD == 1) then
         CU(ktrans,mstr,1:anze+1) = CUx(1:anze+1)
      else
         CUz(ktrans,mstr,nkz,1:anze+1) = CUx(1:anze+1)
      endif
   endif
   if (iverfahren == 2) then
      call lax_wendroff(U, vmitt, Uvert, elen, DeltaT, anze, flag, nkz, nkzs,ktrans,itime,isgn,mstr,iwahld,itags,monats,uhrz)
   endif
   if (iverfahren == 3) then
      call quickest(U, vmitt, Uvert, elen, DeltaT, anze, flag, ktrans, &
                    mstr, nkz, nkzs, iwahld, isgn,uhrz, itags, monats, &
                    itime, iwsim)
   endif
   
   ! Ende Gitterschleife
   if (iwahlD == 1 .and. itime == izeits.and.ktrans == 1) vmittt_1(mstr,1:anze+1) = vmitt(1:anze+1)
   if (iwahlD == 2 .and. itime == izeits.and.ktrans == 1) Uvertt_1(mstr,nkz,1:anze+1) = Uvert(nkz,1:anze+1)
   
   
   ! Loesung des Dispersionsterms
   ! if(isgn(2,nkz)==1)U(1) = temp0
   ! if(isgn(anze+1,nkz)==-1)U(anze+1) = tempn
   ! if(ktrans/=44)then
   if (iMac(mstr) == 0) then
      call Crank_Nicolson(U,elen,flag,dl,deltat,anze,temp0,icraNicKoeff,ithomas,mstr,ktrans,iwahlD,nkz,nkzs       &
                          ,itags,monats,itime,isub_dtx,isgn,uhrz)
   else
      call MacCorm(U,elen,flag,dl,deltat,anze,temp0,mstr,ktrans,iwahlD,nkz,nkzs,itags,monats,itime            &
                   ,isub_dtx,isgn,uhrz)
   endif
   ! endif
   ! **Ende Zeitschleife**
   ! if(iwied==1.and.iflRi(mstr)==(-1))then  !Umdrehen bei Fliessumkehr
   !    jR =anze+2
   !    if(iwahlD==1)then
   !       do ior = 1,anze+1
   !          jR = jR-1
   !          CU(ktrans,mstr,ior) = CU(ktrans,mstr,jR)
   !       enddo
   !    else
   !       do ior = 1,anze+1
   !          jR = jR-1
   !          CUz(ktrans,mstr,nkz,ior) = CUz(ktrans,mstr,nkz,jR)
   !       enddo
   !    endif
   ! endif
   if (itime == izeits) then   ! löschen
      ! if(isgn(2,nkz)==1)U(1) = temp0
      ! if(isgn(anze,nkz)==-1)U(anze+1) = tempn
   endif
   ! if(iwahlD==2)then
   !    deallocate(Cuz)
   !    deallocate(imarkerCUz)
   !    deallocate(Uvertt_1)
   ! else if(iwahlD==1)then
   !    deallocate(Cu)
   ! endif
   return
end subroutine AdvDiff
