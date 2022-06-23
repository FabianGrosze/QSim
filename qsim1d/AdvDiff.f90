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
subroutine AdvDiff(anze,elen,vmitt,Uvert,dl,flag,ktrans,U,temp0,tempn,deltat,sumdet,itime,izeits,mstr,iwied           &
                   ,iwahlD,nkz,nkzs,tflie,iFlRi,jpoin1,itags,monats,isub_dtx,imac,iverfahren,azStrs,kktrans,nkztot_max   &
                   ,ianze_max,mtracer,iwsim,uhrz)
   
   ! Lösungsverfahren RCIP nach: F. Xiao et al., Constructing oscillation preventing
   !                             scheme for advection equation by rational function.
   !                             Computer Physics Communications 93, 1-12 (1996)
   !                             
   !                             Lax_Wendroff: High Resolution Shock-Capturing Numerical Methods
   !                             S. 100, Formel (6.20)
   
   integer                                :: azStrs, anze, nkz, ior
   integer, dimension(azStrs)             :: iFlRi, imac
   integer, dimension(1000)               :: flag, nkzs, imarker
   integer, dimension(1000,50)            :: m, isgn
   real, dimension(1000)                  :: vmitt, elen, Calpha, Nenner, Uneu, dl, U,CUx, U_lin
   real, dimension(1000,50)               :: xpoint
   real, dimension(50,1000)               :: vmittt_1,Uvert
   real, allocatable, dimension(:,:,:)    :: Uvertt_1
   real, allocatable, dimension(:,:,:)    :: CU
   real, allocatable, dimension(:,:,:,:)  :: CUz
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
                         ,isgn,vx_Cr,azStrs,nkztot_max,ianze_max,uhrz)
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
               ,iwahld,azSTrs, nkztot_max, ianze_max,itags,isgn,m,vx_Cr,Uhrz,monats,temp0,deltat)
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
      call quickest(U, vmitt, Uvert, elen, DeltaT, anze, flag, ktrans, mstr, nkz, nkzs, iwahld, isgn,uhrz, itags, monats, itime,iwsim)
   endif
   ! **Ende Gitterschleife**
   if (iwahlD == 1 .and. itime == izeits.and.ktrans == 1) vmittt_1(mstr,1:anze+1) = vmitt(1:anze+1)
   if (iwahlD == 2 .and. itime == izeits.and.ktrans == 1) Uvertt_1(mstr,nkz,1:anze+1) = Uvert(nkz,1:anze+1)
   
   
   ! Loesung des Dispersionsterms
   ! if(isgn(2,nkz)==1)U(1) = temp0
   ! if(isgn(anze+1,nkz)==-1)U(anze+1) = tempn
   ! if(ktrans/=44)then
   if (iMac(mstr) == 0) then
      call Crank_Nicolson(U,elen,flag,dl,deltat,anze,temp0,icraNicKoeff,ithomas,mstr,ktrans,iwahlD,nkz,nkzs       &
                          ,itags,monats,itime,isub_dtx,isgn,uhrz,azStrs)
   else
      call MacCorm(U,elen,flag,dl,deltat,anze,temp0,mstr,ktrans,iwahlD,nkz,nkzs,itags,monats,itime            &
                   ,isub_dtx,isgn,uhrz,azStrs)
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


subroutine basisPoint(anze,flag,deltat,vmitt,Uvert,Uvertt_1,vmittt_1,elen,xPoint,m,iwied,itime,izeits  &
                      ,jpoin1,sumdet,tfliesec,nkz,nkzs,iwahlD,ipo,mstr,itags,monats,ktrans              &
                      ,isgn,vx_Cr,azStrs,nkztot_max,ianze_max,uhrz)
   
   integer, dimension(1000)         :: flag, nkzs
   integer, dimension(1000,50)      :: m, isgn
   integer                          :: anze, azStrs
   real, dimension(1000)            :: elen, vmitt
   real, dimension(50,1000)         :: vmittt_1, Uvert
   real, dimension(1000,50)         :: xpoint
   double precision                 :: Ax, hconA
   real, dimension(1:azStrs,1:nkztot_max,1:ianze_max+1) :: Uvertt_1
  
   vmitt(anze+1) = vmitt(anze)
   elen(anze+1) = elen(anze)
   iein = 0
   if (iwahlD == 1) then
      vx0 = vmitt(1)
   else
      vx0 = Uvert(nkz,1)
   endif
   j = 2
   m(1,nkz) = 0
   if (vx0 < 0.0)j = 1
   if (j == 2)isgn(1,nkz) = 1
   do ior = j,anze+1
      m(ior,nkz) = ior
      isgn(ior,nkz) = 1
      if (iwahlD == 1) then
         vx0 = vmitt(m(ior,nkz))
      else
         vx0 = Uvert(nkz,m(ior,nkz))
      endif
      if (vx0 < 0.0) then
         isgn(ior,nkz) = -1
      endif
      if (flag(m(ior,nkz)) == 4) then
         xpoint(ior,nkz) = 0.0
         cycle
      endif
      
      deltatt = deltat
      do i = 1,10
         if (iwahlD == 1) then
            vx0 = vmitt(m(ior,nkz))
            vx1 = vmitt(m(ior,nkz)-isgn(ior,nkz))
         else
            vx0 = Uvert(nkz,m(ior,nkz))
            vx1 = Uvert(nkz,m(ior,nkz)-isgn(ior,nkz))
         endif
         if (isgn(ior,nkz) == 1)dxx = elen(m(ior,nkz)-1)
         if (isgn(ior,nkz) == -1)dxx = elen(m(ior,nkz))
         if (isgn(ior,nkz) == 1 .and. vx1 < 0.0) then
            xpoint(ior,nkz) = -vx0*(dxx/(vx1-vx0))             ! Umstellen der Formel nach Newton (lineare Interpolation)
            exit
         else if (isgn(ior,nkz) == -1 .and. vx1 > 0.0) then
            xpoint(ior,nkz) = -vx0*(dxx/(vx1-vx0))         ! Umstellen der Formel nach Newton (lineare Interpolation)
            exit
         else
         endif
         vx = (vx0+vx1)/2.
         vx = vx1   !loeschen
         hcondt = dxx/abs(vx)
         if (hcondt < deltatt) then         ! I
            if (isgn(ior,nkz) == 1 .and. m(ior,nkz) == 2) then
               xpoint(ior,nkz) = dxx
               exit
            endif
            if (isgn(ior,nkz) == -1 .and. m(ior,nkz) == anze+1) then
               xpoint(ior,nkz) = dxx
               exit
            endif
            if (m(ior,nkz) == 1) then
            else
               if (isgn(ior,nkz) == 1 .and. m(ior,nkz) > 2.and.flag(m(ior,nkz)-1) /= 4) then  ! II
                  m(ior,nkz) = m(ior,nkz)-1
                  deltatt = deltatt-hcondt
                  cycle
               endif
            endif
            if (isgn(ior,nkz) == -1 .and. m(ior,nkz) < anze+1.and.flag(m(ior,nkz)+1) /= 4) then
               m(ior,nkz) = m(ior,nkz)+1
               deltatt = deltatt-hcondt
               cycle
            endif
            if (m(ior,nkz) == 1) then
            else
               if (isgn(ior,nkz) == 1 .and. flag(m(ior,nkz)-1) == 4) then
                  xpoint(ior,nkz) = dxx
                  exit
               endif
            endif
            if (isgn(ior,nkz) == -1 .and. flag(m(ior,nkz)+1) == 4) then
               xpoint(ior,nkz) = dxx
               exit
            endif
            
         else                       ! I
            xpoint(ior,nkz) = abs(vx)*deltatt
            if (isgn(ior,nkz) == -1 .and. flag(m(ior,nkz)+1) == 4)iein = iein+1
            exit
         endif
      enddo
   enddo
   return
end subroutine BasisPoint




subroutine Crank_Nicolson(U,elen,flag,dl,deltat,anze,temp0,icraNicKoeff,ithomas,mstr,ktrans,iwahlD,nkz,nkzs       &
                          ,itags,monats,itime,isub_dtx,isgn,uhrz,azStrs)
   integer                            :: anze,azStrs
   integer, dimension(1000)           :: flag, nkzs, imarker
   integer, dimension(1000,50)        :: isgn
   real, dimension(1000)              :: r1, r2, r3, dl, elen, U
   double precision, dimension(1000)  :: a,b,c,d
   deltat_z = deltat
   deltat = deltat/isub_dtx
   Ux1 = U(1)
   Ux_nx = U(anze+1)
   do itime_sub = 1,isub_dtx
      if (icraNicKoeff == 0) then
         call cra_NicKoeff(elen,r1,r2,r3,dl,flag,deltat,anze,nkzs,nkz,mstr,ktrans,imarker,icraNicKoeff,itags,uhrz)
      endif
      ior = 1
      if (ithomas == 1) then
         b(ior) = 0.0
         c(ior) = 0.0
         b(ior) = 2.+2.*r2(ior)
         c(ior) = -r3(ior)
      else ! Gauss
         a(ior) = 0.0
         b(ior) = 0.0
         a(ior) = 2.+2.*r2(ior)
         b(ior) = -r3(ior)
      endif
      d(ior) = (2.-2.*r2(ior))*U(ior)+r3(ior)*U(ior+1)+r2(ior)*(U(1)+U(1))
      
      !$omp parallel do
      do ior = 2,anze
         if (ithomas == 1) then
            a(ior) = 0.0
            b(ior) = 0.0
            c(ior) = 0.0
         else
            c(ior-1) = 0.0
            a(ior) = 0.0
            b(ior) = 0.0
         endif
         
         if (imarker(ior) == 1) then
            if (ithomas == 1) then
               a(ior) = -2.*r1(ior)
               b(ior) = (2.+r2(ior))
            else
               c(ior-1) = -2.*r1(ior)
               a(ior) = (2.+r2(ior))
            endif
            d(ior) = 2.*r1(ior)*U(ior-1)+(2.-r2(ior))*U(ior)
         else if (imarker(ior) == 2) then
            if (ithomas == 1) then
               b(ior) = 2.+2.*r2(ior)
               c(ior) = -r3(ior)
            else
               a(ior) = 2.+2.*r2(ior)
               b(ior) = -r3(ior)
            endif
            d(ior) = (2.-2.*r2(ior))*U(ior)+r3(ior)*U(ior+1)+r2(ior)*(U(ior)+U(ior))
         else
            if (ithomas == 1) then
               a(ior) = -r1(ior)
               b(ior) = (2.+r2(ior))
               c(ior) = -r3(ior)
            else
               c(ior-1) = -r1(ior)
               a(ior) = (2.+r2(ior))
               b(ior) = -r3(ior)
            endif
            
            d(ior) = r1(ior)*U(ior-1)+(2.-r2(ior))*U(ior)+r3(ior)*U(ior+1)
         endif
      enddo
      !$OMP END PARALLEL Do
      if (ithomas == 1) then
         a(anze+1) = 0.0
         b(anze+1) = 0.0
         a(anze+1) = -2.*r1(anze+1)
         b(anze+1) = (2.+r2(anze+1))
      else
         c(anze) = 0.0
         a(anze+1) = 0.0
         c(anze) = -2.*r1(anze+1)
         a(anze+1) = (2.+r2(anze+1))
      endif
      
      d(anze+1) = 2.*r1(anze+1)*U(anze)+(2.-r2(anze+1))*U(anze+1)
      
      if (ithomas == 1) then
         call thomas(a,b,c,d,anze,U,flag,mstr,ktrans,imarker,iwahlD,itags,nkz,itime)
      else
         call trimat(a,b,c,d,anze,U,flag,mstr,ktrans,imarker,iwahlD,itags,monats,uhrz,nkz,nkzs,itime)
      endif
      if (isgn(1,nkz) == 1)U(1) = Ux1
      if (isgn(anze+1,nkz) == -1)U(anze+1) = Ux_nx
   enddo ! Ende subTime Schleife
   deltat = deltat_z
   return
end subroutine Crank_Nicolson


subroutine cra_NicKoeff(elen,r1,r2,r3,dl,flag,deltat,anze,nkzs,nkz,mstr,ktrans,imarker,icraNicKoeff,itags,uhrz)
   integer                        :: anze
   integer, dimension(1000)       :: flag, imarker, nkzs
   integer, dimension(1000,50)    :: isgn
   real                           :: nenner
   real, dimension(1000)          :: elen, r1, r2, r3, dl
   iein = 0
   ior = 1
   imarker(ior) = 0
   ! if(flag(ior+1)==4)imarker(ior) = 3
   r2(ior) = dl(ior)*deltat/elen(ior)**2
   r3(ior) = dl(ior)*deltat/elen(ior)**2
   do ior = 2,anze
      imarker(ior) = 0
      
      ! if(flag(ior)==4.and.flag(ior+1)==4)imarker(ior) = 3
      if (flag(ior) == 6)imarker(ior) = 1
      
      if (flag(ior) == 4)imarker(ior) = 2
      if (imarker(ior) == 2) then
         r2(ior) = dl(ior)*deltat/elen(ior)**2
         r3(ior) = dl(ior)*deltat/elen(ior)**2
      else if (imarker(ior) == 1) then
         r1(ior) = dl(ior)*deltat/elen(ior-1)**2
         r2(ior) = 2.*dl(ior)*deltat/elen(ior-1)**2
      else
         alpha = elen(ior)/elen(ior-1)
         nenner = 0.5*alpha*(1+alpha)*elen(ior-1)**2
         dlmit = (dl(ior-1)+dl(ior))/2.
         r1(ior) = alpha*dlmit*deltat/nenner
         r2(ior) = (1+alpha)*dlmit*deltat/nenner
         r3(ior) = dlmit*deltat/nenner
      endif
   enddo
   imarker(anze+1) = 0
   r1(anze+1) = dl(anze)*deltat/elen(anze)**2
   r2(anze+1) = 2.*dl(anze)*deltat/elen(anze)**2
   icraNicKoeff = 1
   
   return
end subroutine cra_NicKoeff

!> Berechnung einer tridiagonal-Matrix
!! @author Volker Kirchesch
!! @date 23.01.1994
subroutine trimat(a,b,c,d,anze,U,flag,mstr,ktrans,imarker,iwahlD,itags,monats,uhrz,nkz,nkzs,itime)
   ! Koeffizienten: a,b,c
   
   integer                          :: anze
   integer, dimension(1000)         :: flag, imarker,nkzs
   real, dimension(1000)            :: U
   double precision, dimension(1000):: a, b, c, d, l, mm, y
   
   
   mm(1) = a(1)
   do ior = 1,anze
      l(ior) = c(ior)/mm(ior)
      if (imarker(ior) == 2)mm(ior) = a(ior)
      mm(ior+1) = a(ior+1)-l(ior)*b(ior)
   enddo
   y(1) = d(1)
   do ior = 2,anze+1
      if (imarker(ior) == 2) then
         Y(ior) = d(ior)
         cycle
      endif
      y(ior) = d(ior)-l(ior-1)*y(ior-1)
   enddo
   if (mm(anze+1) > 0.0)U(anze+1) = y(anze+1)/mm(anze+1)
   do ior = anze,1,-1 ! hier
      if (imarker(ior) == 2)cycle
      if (imarker(ior) == 1) then
         U(ior) = Y(ior)/mm(ior)
         cycle
      endif
      U(ior) = (y(ior)-b(ior)*U(ior+1))/mm(ior)
      if (U(ior) > 0.0 .and. abs(U(ior)) < 1.e-15)U(ior) = 1.e-15  ! Falls Fehler evt. hier
      if (ktrans /= 1 .and. ktrans /= 57) then
         if (U(ior) < 0.0)U(ior) = 0.0
      endif
   enddo
   if (mm(anze+1) == 0.0)U(anze+1) = U(anze)
   return
end subroutine trimat


subroutine Thomas(a,b,c,d,anze,U,flag,mstr,ktrans,imarker,iwahlD,itags,nkz,itime)
   integer                           :: anze
   integer, dimension(1000)          :: imarker, flag
   integer, dimension(1000,50)       :: isgn
   real, dimension(1000)             :: U
   double precision, dimension(1000) :: a,b,c,d, bp, dp, bpz, dpz
   double precision                  :: m
   
   bp(1) = b(1)
   dp(1) = d(1)
   do i = 2,anze+1
      m = a(i)/bp(i-1)
      bp(i) = b(i)-m*c(i-1)
      dp(i) = d(i)-m*dp(i-1)
   enddo
   U(anze+1) = dp(anze+1)/bp(anze+1)
   do i = anze,1,-1
      if (imarker(i) == 3) then
         cycle
      else if (imarker(i) == 2) then
         cycle
      else
         U(i) = (dp(i)-c(i)*U(i+1))/bp(i)
      endif
   enddo
   return
end subroutine thomas


subroutine MacCorm(U,elen,flag,dl,deltat,anze,temp0,mstr,ktrans,iwahlD,nkz,nkzs,itags,monats,itime           &
                   ,isub_dtx,isgn,uhrz,azStrs)
   integer                            :: anze, azStrs
   integer, dimension(1000)           :: flag, nkzs
   integer, dimension(1000,50)        :: isgn
   real, dimension(1000)              :: dl, elen, nenner
   real, dimension(1000)              :: Calpha, dlmit, U, Uneu_1
   double precision, dimension(1000)  :: SS1, SS2
   
   
   deltat_z = deltat
   deltat = deltat/isub_dtx
   do itime_sub = 1,isub_dtx
      !    if(isgn(anze,nkz)==-1)U(anze+1) = temp0
      !    if(isgn(anze,nkz)==1)U(1) = temp0
      Ux1 = U(1)
      Ux_nx = U(anze+1)
      !$omp parallel do
      do i = 1,anze+1
         if (isgn(i,nkz) > 0) then
            if (i > 1 .and. i < anze+1) then
               Calpha(i) = elen(i)/elen(i-1)
               nenner(i) = 0.5*Calpha(i)*(1.+Calpha(i))*elen(i-1)**2
               dlmit(i) = (dl(i-1)+dl(i))/2.
               S1 = U(i-1)
               S2 = U(i)
               S3 = U(i+1)
            endif
            if (i == 1) then
               S1 = U(i)
               S2 = U(i)
               S3 = U(i+1)
               Calpha(i) = elen(i)/elen(i)
               nenner(i) = 0.5*Calpha(i)*(1.+Calpha(i))*elen(i)**2
               dlmit(i) = (dl(i)+dl(i))/2.
            endif
            if (i > 1) then
               if (flag(i) == 6 .or. i == anze+1) then
                  S1 = U(i-1)
                  S2 = U(i)
                  S3 = U(i)
                  Calpha(i) = elen(i-1)/elen(i-1)
                  nenner(i) = 0.5*Calpha(i)*(1.+Calpha(i))*elen(i-1)**2
                  dlmit(i) = (dl(i-1)+dl(i-1))/2.
               endif
            endif
         else                                        ! isgn(i,nkz) gleich -1 (Rueckstroemung)
            if (i > 1 .and. i < anze+1) then
               Calpha(i) = elen(i)/elen(i-1)
               nenner(i) = 0.5*Calpha(i)*(1.+Calpha(i))*elen(i-1)**2
               dlmit(i) = (dl(i-1)+dl(i))/2.
               S1 = U(i+1)
               S2 = U(i)
               S3 = U(i-1)
            endif
            
            if (flag(i) == 6 .or. i == 1) then
               S1 = U(i+1)
               S2 = U(i)
               S3 = U(i)
               
               Calpha(i) = elen(i)/elen(i)
               nenner(i) = 0.5*Calpha(i)*(1.+Calpha(i))*elen(i)**2
               dlmit(i) = (dl(i)+dl(i))/2.
            endif
            
            if (i == anze+1) then
               S1 = U(i)
               S2 = U(i)
               S3 = U(i-1)
               
               Calpha(i) = elen(i-1)/elen(i-1)
               nenner(i) = 0.5*Calpha(i)*(1.+Calpha(i))*elen(i-1)**2
               dlmit(i) = (dl(i-1)+dl(i-1))/2.
            endif
            if (i == 1) then
               S1 = U(i+1)
               S2 = U(i)
               S3 = U(i)
               Calpha(i) = elen(i)/elen(i)
               nenner(i) = 0.5*Calpha(i)*(1.+Calpha(i))*elen(i)**2
               dlmit(i) = (dl(i)+dl(i))/2.
            endif
         endif
         SS1(i) = dlmit(i)*(S3-(1.+Calpha(i))*S2+Calpha(i)*S1)/nenner(i)
         if (flag(i) == 4)SS1(i) = 0.0
         Uneu_1(i) = U(i)+SS1(i)*deltat
      enddo
      !$OMP END PARALLEL Do
      if (isgn(1,nkz) == 1)Uneu_1(1) = Ux1
      if (isgn(anze+1,nkz) == -1)Uneu_1(anze+1) = Ux_nx
      !$omp parallel do
      do i = 1,anze+1
         if (isgn(i,nkz) > 0) then
            if (i > 1 .and. i < anze+1) then
               S1 = Uneu_1(i-1)
               S2 = Uneu_1(i)
               S3 = Uneu_1(i+1)
            endif
            
            if (i == 1) then
               S1 = Uneu_1(i)
               S2 = Uneu_1(i)
               S3 = Uneu_1(i+1)
            endif
            
            if (i > 1) then
               if (flag(i) == 6 .or. i == anze+1) then
                  S1 = Uneu_1(i-1)
                  S2 = Uneu_1(i)
                  S3 = Uneu_1(i)
               endif
            endif
         else                                        ! isgn(i,nkz) gleich -1 (Rueckstroemung)
            if (i > 1 .and. i < anze+1) then
               S1 = Uneu_1(i+1)
               S2 = Uneu_1(i)
               S3 = Uneu_1(i-1)
            endif
            if (flag(i) == 6 .or. i == 1) then
               S1 = Uneu_1(i+1)
               S2 = Uneu_1(i)
               S3 = Uneu_1(i)
            endif
            
            if (i == anze+1) then
               S1 = Uneu_1(i)
               S2 = Uneu_1(i)
               S3 = Uneu_1(i-1)
            endif
            if (i == 1) then
               S1 = Uneu_1(i+1)
               S2 = Uneu_1(i)
               S3 = Uneu_1(i)
            endif
         endif
         SS2(i) = dlmit(i)*(S3-(1.+Calpha(i))*S2+Calpha(i)*S1)/nenner(i)
         if (flag(i) == 4)SS2(i) = 0.0
         U(i) = U(i)+((SS1(i)+SS2(i))/2.)*deltat
      enddo
      !$OMP END PARALLEL Do
      if (isgn(1,nkz) == 1)U(1) = Ux1
      if (isgn(anze+1,nkz) == -1)U(anze+1) = Ux_nx
   enddo
   deltat = deltat_z
   return
end subroutine MacCorm


!> Calculating 1D Advection equation based on an cubic Interpolation
subroutine CIP(U, CUx, DX, DT, NX, xpoint, flag, nkz, nkzs,ktrans,kktrans,itime,mstr    &
               ,iwahld,azStrs,nkztot_max, ianze_max,itags,isgn,m,vx_Cr,Uhrz,monats,temp0,deltat)

   integer :: NX, I, azStrs
   integer, dimension(1000) :: nkzs, flag
   integer, dimension(1000,50) :: m, isgn
   real, dimension(1000) :: DX, w, U,CUx, CU_neu,U_neu, Ulin
   real, dimension(1000,50) :: xpoint
   save CU_neu, U_neu, Ulin,a,b
   
   iein = 0
   cx = 0.0
   j = 2
   if (m(1,nkz) > 0) then
      if (isgn(m(1,nkz),nkz) == -1)j = 1
   endif
   !$omp parallel do
   do i = j,nx+1
      m1 = m(i,nkz)
      if (flag(i) == 4) then
         U_neu(i) = U(i)
         Ulin(i) = U(i)
         CU_neu(i) = (U(i+1)-U(i))/dx(i)
         cycle
      endif
      
      ! if(m1==1)then
      !    m2 = m1
      if (isgn(m(i,nkz),nkz) == -1 .and. i == nx+1) then
         m2 = m1
      else
         m2 = m(i,nkz)-isgn(m(i,nkz),nkz)
      endif
      if (m2 > nx+1 .and. m1 == nx+1)m2 = m1
      m3 = m2
      if (isgn(m(i,nkz),nkz) == -1) m3 = m1
      a = (CUx(m1)+CUx(m2))/(-dx(m3)*isgn(m(i,nkz),nkz))**2+(2.*(U(m1)-U(m2)))/(-dx(m3)*isgn(m(i,nkz),nkz))**3
      b = (3.*(U(m2)-U(m1)))/(-dx(m3)*isgn(m(i,nkz),nkz))**2-(2.*CUx(m1)+CUx(m2))/(-dx(m3)*isgn(m(i,nkz),nkz))
      ! if(isgn(m(i,nkz),nkz)==1)then
      !    if(nkzs(i-1)<nkz)then
      !       U_neu(i) = U(i)
      !       cycle
      !    endif
      ! else if(isgn(m(i,nkz),nkz)==-1)then
      !    if(nkzs(i+1)<nkz)then
      !       U_neu(i) = U(i)
      !       cycle
      !    endif
      ! endif
      U_neu(i) = a*(-xPoint(i,nkz)*isgn(m(i,nkz),nkz))**3+b*(-xPoint(i,nkz)*isgn(m(i,nkz),nkz))**2+CUx(m1)*(-xPoint(i,nkz)*isgn(m(i,nkz),nkz))+U(m1)
      CU_neu(i) = 3.*a*(-xpoint(i,nkz)*isgn(m(i,nkz),nkz))**2+2*b*(-xpoint(i,nkz)*isgn(m(i,nkz),nkz))+CUx(m1)
      Ulin(i) = U(m2)+((U(m1)-U(m2))/dx(m3))*(dx(m3)-xpoint(i,nkz))
      Umax = max(U(m1),U(m2))
      Umin = min(U(m1),U(m2))
      ! if(U_neu(i)>Umax)U_neu(i) = Umax
      ! if(U_neu(i)<Umin)U_neu(i) = Umin
      qmax = Umax-Ulin(i)
      qmin = Umin-Ulin(i)
      p = U_neu(i)-Ulin(i)
      if (p == 0.0)cx = 0.0
      if (p > 0.0) then
         hconp = qmax/p
         cx = 1.
         if (hconp < cx)cx = hconp
      endif
      if (p < 0.0) then
         hconp = qmin/p
         cx = 1.
         if (hconp < cx)cx = hconp
      endif
      U_neu(i) = Ulin(i)+cx*(U_neu(i)-Ulin(i))
      if (U_neu(i) > 0.0 .and. abs(U_neu(i)) < 1.e-15)U_neu(i) = 1.e-15
      if (ktrans /= 1 .and. ktrans /= 57) then
         if (U_neu(i) < 0.0)U_neu(i) = 0.0
      endif
   enddo  ! Ende Knotenschleife
   !$OMP END PARALLEL Do
   
   if (nx > 1) then  ! Belegung des ersten Knotens
      if (isgn(1,nkz) == 1 .or. isgn(1,nkz) == 0)U_neu(1) = U(1)
      if (isgn(nx+1,nkz) == -1)U_neu(nx+1) = U(nx+1)
   else  ! Strang besteht aus einem Knoten
      U_neu(1) = U(1)
      U_neu(NX+1) = U_neu(1)
   endif
   Ulin(1) = U_neu(1)
   j = 1
   !     if(isgn(1,nkz)==1)j = 2
   jj = 1
   !     if(isgn(nx+1,nkz)==-1)jj = 0
   !$omp parallel do
   do i = j,nx+jj
      U(i) = U_neu(i)
      CUx(i) = CU_neu(i)
   enddo
   !$OMP END PARALLEL Do
   return
end subroutine CIP


subroutine lax_wendroff(U, vmitt, Uvert, DX, DT, NX, flag, nkz, nkzs,ktrans,itime,isgn,mstr,iwahld,itags,monats,Uhrz)
   integer :: NX, I
   integer, dimension(1000) :: nkzs, flag
   integer, dimension(1000,50) :: isgn
   real, dimension(1000) :: DX, cour,U, U_neu, phi
   real, dimension(1000) :: vmitt
   real, dimension(50,1000)  :: Uvert
   real :: m1, m2
   save U_neu
   j = 2
   jj = 0
   if (iwahlD == 1) then
      vmitt(nx+1) = vmitt(nx)
      dx(nx+1) = dx(nx)
   else
      Uvert(nkz,nx+1) = Uvert(nkz,nx)
      dx(nx+1) = dx(nx)
   endif
   
   if (iwahlD == 1) then
      if (vmitt(1) < 0.0)j = 1
      if (vmitt(nx+1) < 0.0)jj = 1
   else
      if (Uvert(nkz,1) < 0.0)j = 1
      if (Uvert(nkz,nx+1) < 0.0)jj = 1
   endif
   hcon = 0.0
   if (j == 2)isgn(1,nkz) = 1
   if (jj == 1)isgn(nx+1,nkz) = -1
   do i = j,nx+1-jj
      !      if(flag(i)==4)then
      !        U_neu(i) = U(i)
      !        cycle
      !      endif
      if (iwahlD == 1) then
         if (vmitt(i)>=0.0)isgn(i,nkz) = 1
         if (vmitt(i) < 0.0)isgn(i,nkz) = -1
      else
         if (Uvert(nkz,i)>=0.0)isgn(i,nkz) = 1
         if (Uvert(nkz,i) < 0.0)isgn(i,nkz) = -1
      endif
      !  Berechnung der Courant-Zahl
      if (iwahlD == 1) then
         if (isgn(i,nkz) == 1)cour(i) = abs(vmitt(i-1))*dt/dx(i-1)
         if (isgn(i,nkz) == -1)cour(i) = abs(vmitt(i))*dt/dx(i)
      else
         if (isgn(i,nkz) == 1)cour(i) = abs(Uvert(nkz,i-1))*dt/dx(i-1)
         if (isgn(i,nkz) == -1)cour(i) = abs(Uvert(nkz,i))*dt/dx(i)
      endif
      
      !****limiter******
      if (isgn(i,nkz) == 1) then
         if (i == nx+1 .or. flag(i+1) == 4 .or. isgn(i+1,nkz) == -1) then
            S1 = U(i-1)
            S2 = U(i)
            S3 = U(i)
         else if (flag(i) == 4) then
            S1 = U(i)
            S2 = U(i)
            S3 = U(i+1)
         else
            S1 = U(i-1)
            S2 = U(i)
            S3 = U(i+1)
         endif
      endif
      if (isgn(i,nkz) == -1) then
         if (i == 1) then
            S1 = U(i+1)
            S2 = U(i)
            S3 = U(i)
         else if (i > 1 .and. flag(i-1) == 4) then
            S1 = U(i+1)
            S2 = U(i)
            S3 = U(i)
         else if (i == nx+1) then
            S1 = U(i)
            S2 = U(i)
            S3 = U(i-1)
         else if (flag(i) == 4) then
            S1 = U(i)
            S2 = U(i)
            S3 = U(i-1)
         else
            S1 = U(i+1)
            S2 = U(i)
            S3 = U(i-1)
            
         endif
      endif
      
      hcon = (S2-S1)/(max(1.e-10,(S3-S2)))
      ! hcon = (S2-S1)/(1.e-10+(S3-S2))
      ! minmod-Limiter
      ! phi(i) = (S3-S2)*max(0.0,min(1.0,hcon))    ! downwind
      ! superbee-Limiter
      phi(i) = (S3-S2)*max(0.0,min(1.0,2.*hcon),min(2.,hcon))
   enddo
   do i = j,nx+1-jj     ! Beginn Gitterschleife
      if (isgn(i,nkz) == 1) then
         n = 1
         S1 = U(i-1)
         S2 = U(i)
         if (i == 2) then
            phi(i-1) = phi(i)
            cour(i-1) = cour(i)
         endif
         !        if(flag(i-1)==4)phi(i-1) = phi(i)
      endif
      if (isgn(i,nkz) == -1) then
         n = -1
         S1 = U(i+1)
         S2 = U(i)
         !        if(flag(i+1)==4)phi(i+1) = phi(i)
      endif
      
      if (flag(i) == 4 .or. isgn(i,nkz) == 0) then
         U_neu(i) = U(i)
         cycle
      else
         if (isgn(nx,nkz) == -1 .and. i == nx) then
            phi(nx+1) = phi(nx)
            cour(nx+1) = cour(nx)
         endif
         Cour_mit = (Cour(i) + Cour(i-n))/2.
         U_neu(i) = S2-(cour_mit*(S2-S1))-0.5*cour_mit*(1.-cour_mit)*Phi(i)+0.5*cour_mit*(1.-cour_mit)*Phi(i-n)
      endif
      if (abs(U_neu(i)) < 1.e-25)U_neu(i) = 0.0
      if (ktrans /= 1 .and. ktrans /= 57) then
         if (U_neu(i) < 0.0)U_neu(i) = 0.0
      endif
      ! if(i>1.and.isgn(i,nkz)==1)then
      !    if(nkzs(i-1)<nkz)U_neu(i) = U(i)        ! neu
      ! else if(i<(nx+1).and.isgn(i,nkz)==-1.and.nkzs(i+1)<nkz)then   ! neu
      !    U_neu(i) = U(i)
      ! endif
   enddo
   if (NX == 1) then ! Falls der Strang nur aus einem Knoten besteht
      U_neu(1) = U(1)
      U_neu(NX+1) = U_neu(1)
   else
      if (isgn(1,nkz) == 1)U_neu(1) = U(1)
      if (isgn(1,nkz) == -1)U_neu(1) = U(1)-cour(1)*(U(1)-U(2))
      if (isgn(nx+1,nkz) == -1)U_neu(NX+1) = U(NX+1)
      if (isgn(nx+1,nkz) == 1)U_neu(nx+1) = U(nx+1)-cour(nx)*(U(nx+1)-U(nx))
   endif
   do i = 1,nx+1
      U(i) = U_neu(i)
   enddo
end subroutine lax_wendroff



subroutine quickest(U, vmitt, Uvert, dx, DeltaT, nx, flag, ktrans,mstr, nkz, nkzs, &
                    iwahlD, isgn, Uhrz, itags, monats, itime,iwsim)
   
   integer                       :: nx, m1, m2, ktrans
   integer, dimension(1000)      :: n, nm, nkzs
   integer, dimension(1000)      :: flag
   integer, dimension(1000,50)   :: isgn
   real                          :: nenner,lammda
   real, dimension(1000)         :: dx, Crr, w, F_plus, vmitt, vr, vl, U, U_neu, U_lin, Phi
   real, dimension(50,1000)      :: Uvert
   save U_neu
   
   if (iwahlD == 1) then
      vmitt(nx+1) = vmitt(nx)
      dx(nx+1) = dx(nx)
   else
      Uvert(nkz,nx+1) = Uvert(nkz,nx)
      dx(nx+1) = dx(nx)
   endif
   do i = 1,nx+1
      if (iwahlD == 1) then
         if (i < nx+1)vr(i) = (vmitt(i)+vmitt(i+1))/2.
         if (i == nx+1)vr(i) = vmitt(i)
      else
         vr(i) = Uvert(nkz,i)
      endif
      
      if (vr(i)>=0.0) then
         isgn(i,nkz) = 1
      else
         isgn(i,nkz) = -1
      endif
      Crr(i) = abs(vr(i))*deltat/dx(i)
      if (isgn(i,nkz) == 1) then
         if (i == 1 .or. flag(i) == 4) then
            S1 = U(i)
            dx_m1 = dx(i)
         else
            S1 = U(i-1)
            dx_m1 = dx(i-1)
         endif
         S2 = U(i)
         if (i == nx+1 .or. flag(i+1) == 4) then
            S3 = U(i)
         else
            S3 = U(i+1)
         endif
      else           ! vr ist kleiner 0.0
         S1 = U(i)
         if (flag(i) == 4) then
            S1 = U(i+1)
         endif
         
         S2 = U(i+1)
         if (i == nx+1) then
            S2 = U(i)
         endif
         S3 = U(i+2)
         dx_p1 = dx(i+1)
         if (i == nx .or. flag(i+1) == 4) then
            S3 = U(i+1)
            dx_p1 = dx(i)
         endif
         if (i == nx+1) then
            S3 = U(i)
            dx_p1 = dx(i)
         endif
      endif
      if (isgn(i,nkz) == 1) then
         term = S2+S3
         gradr = (S3-S2)
      else
         term = S2+S1
         gradr = (S2-S1)
      endif
      
      if (i > 1) then
         dx_Stern = 0.5*dx(i-1)+0.5*dx(i)
      else
         dx_Stern = 0.5*dx(i)+0.5*dx(i)
      endif
      if (isgn(i,nkz) == 1) then
         Curvr = (((S3-S2)/dx(i))-((S2-S1)/dx_m1))/dx_Stern
      else
         Curvr = (((S3-S2)/dx_p1)-((S2-S1)/dx(i)))/dx_Stern
      endif
      F_plus(i) = 0.5*term-(Crr(i)/2.)*Gradr-(1./6.)*dx(i)**2*(1.-Crr(i)**2)*Curvr
   enddo
   do i = 1,nx+1        ! ultimate-Limiter
      if (isgn(i,nkz) == 1) then
         S2 = U(i-1)
         Crr1 = Crr(i-1)
         if (i == 1 .or. flag(i) == 4) then
            S2 = U(i)
            Crr1 = Crr(i)
         endif
         S3 = U(i)
         Crr2 = Crr(i)
         S4 = U(i+1)
         if (i == nx+1 .or. flag(i+1) == 4) then
            S4 = U(i)
         endif
      else           ! vr ist kleiner 0.0
         
         S4 = U(i)
         if (flag(i) == 4) then
            S4 = U(i+1)
         endif
         
         S3 = U(i+1)
         if (i == nx+1) then
            S3 = U(i)
         endif
         S2 = U(i+2)
         dx_p1 = dx(i+1)
         if (i == nx .or. flag(i+1) == 4) then
            S2 = U(i+1)
            dx_p1 = dx(i)
         endif
         if (i == nx+1) then
            S2 = U(i)
            dx_p1 = dx(i)
         endif
      endif
      CRNT = Crr(i)
      !    if(i>1)CRNT = (Crr(i-1)+Crr(i))/2.
      CRNT = abs(Crr(i))
      if (S2>=S3 .and. S3>=S4) then
         Uv = max(S4, S2+(S3-S2)/CRNT)
         if (F_plus(i) > S3)F_plus(i) = S3
         if (F_plus(I) < Uv)F_plus(i) = Uv
      else if (S2 <= S3 .and. S3 <= S4) then
         Uv = min(S4, S2+(S3-S2)/CRNT)
         if (F_plus(i) < S3)F_plus(i) = S3
         if (F_plus(i) > Uv)F_plus(i) = Uv
      else
         F_plus(i) = S3
      endif
   enddo
   
   do i = 1,nx+1
      if (flag(i) == 4) then
         U_neu(i) = U(i)
         cycle
      endif
      if (i == 1) then
         Crm = Crr(i)
      else
         Crm = (Crr(i-1)+Crr(i))/2.
      endif
      
      hconr = Crm*F_plus(i)
      hconl = Crm*F_plus(i-1)
      if (isgn(i,nkz) == -1) then
         hconr = -hconr
         hconl = -hconl
      endif
      
      U_neu(i) = U(i)-hconr+hconl
      if (i == 1 .and. isgn(i,nkz) == 1)U_neu(i) = U(i)
      if (i == 1 .and. isgn(i,nkz) == -1)U_neu(i) = U(i)-crr(i)*(U(i)-U(i+1))
      if (i == nx+1 .and. isgn(i,nkz) == -1)U_neu(nx+1) = U(nx+1)
      if (i == nx+1 .and. isgn(i,nkz) == 1)U_neu(nx+1) = U(nx+1)-crr(i-1)*(U(nx+1)-U(nx))
      if (abs(U_neu(i)) < 1.e-25)U_neu(i) = 0.0
      if (ktrans /= 1 .and. ktrans /= 57) then
         if (U_neu(i) < 0.0)U_neu(i) = 0.0
      endif
      if (iwsim == 4) then
         if (U_neu(i) < 0.0)U_neu(i) = 0.0
      endif
   enddo !Ende Knoten-Schleife
   if (NX == 1) then ! Falls der Strang nur aus einem Knoten besteht
      U_neu(1) = U(1)
      U_neu(NX+1) = U_neu(1)
   endif
   do i = 1,nx+1
      U(i) = U_neu(i)
   enddo
end subroutine quickest
