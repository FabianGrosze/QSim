subroutine crank_nicolson(u,elen,flag,dl,deltat,anze,temp0,icranickoeff,ithomas,mstr,ktrans,iwahld,nkz,nkzs       &
                          ,itags,monats,itime,isub_dtx,isgn,uhrz)
   
   use allodim
   implicit none
   
   integer                            :: nkz, mstr, monats, ktrans, iwahld
   integer                            :: itime_sub, itime, ithomas, itags, isub_dtx
   integer                            :: ior, icranickoeff
   real                               :: ux_nx, ux1, uhrz, temp0, deltat_z, deltat
   integer                            :: anze
   integer, dimension(1000)           :: flag, nkzs, imarker
   integer, dimension(1000,50)        :: isgn
   real, dimension(1000)              :: r1, r2, r3, dl, elen, U
   double precision, dimension(1000)  :: a,b,c,d
   
   external :: cra_nickoeff , thomas, trimat
   
   
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
end subroutine crank_nicolson