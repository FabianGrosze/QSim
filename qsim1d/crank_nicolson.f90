subroutine crank_nicolson(u, elen, flag, dl, deltat, anze, icranickoeff,  &
                          ithomas, ktrans, nkz, isub_dtx, isgn)
   
   use allodim
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2) :: u
   real,    intent(inout), dimension(ialloc2) :: elen
   integer, intent(in),    dimension(ialloc2) :: flag
   real,    intent(in),    dimension(ialloc2) :: dl
   real,    intent(inout)                     :: deltat
   integer, intent(in)                        :: anze
   integer, intent(inout)                     :: icranickoeff
   integer, intent(in)                        :: ithomas
   integer, intent(in)                        :: ktrans
   integer, intent(in)                        :: nkz
   integer, intent(in)                        :: isub_dtx
   integer, intent(in), dimension(ialloc2,50) :: isgn

   ! --- local variables ---
   integer                              :: itime_sub, ior
   integer, dimension(ialloc2)          :: imarker
   real                                 :: ux_nx, ux1, deltat_z
   real, dimension(ialloc2)             :: r1, r2, r3
   double precision, dimension(ialloc2) :: a,b,c,d
   
   external :: cra_nickoeff , thomas, trimat
   
   deltat_z = deltat
   deltat = deltat / isub_dtx
   Ux1 = U(1)
   Ux_nx = U(anze+1)

   do itime_sub = 1,isub_dtx
      if (icraNicKoeff == 0) then
         call cra_NicKoeff(elen, r1, r2, r3, dl, flag, deltat, &
                           anze, imarker, icraNicKoeff)
      endif
      
      ior = 1
      if (ithomas == 1) then
         b(ior) = 2. + 2. * r2(ior)
         c(ior) = -r3(ior)
      else 
         ! Gauss
         a(ior) = 2. + 2. * r2(ior)
         b(ior) = -r3(ior)
      endif
      d(ior) = (2.-2.*r2(ior)) * U(ior)  &
             + r3(ior) * U(ior+1)        &
             + r2(ior) * (U(1) + U(1))
      
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
               b(ior) = 2. + r2(ior)
            else
               c(ior-1) = -2.*r1(ior)
               a(ior) = 2. + r2(ior)
            endif
            d(ior) = 2. * r1(ior) * U(ior-1) &
                   + (2.-r2(ior)) * U(ior)
         
         else if (imarker(ior) == 2) then
            if (ithomas == 1) then
               b(ior) = 2.+2.*r2(ior)
               c(ior) = -r3(ior)
            else
               a(ior) = 2.+2.*r2(ior)
               b(ior) = -r3(ior)
            endif
            d(ior) = (2.-2. * r2(ior)) * U(ior)  &
                   + r3(ior) * U(ior+1)          &
                   + r2(ior) * (U(ior) + U(ior))
         else
            if (ithomas == 1) then
               a(ior) = -r1(ior)
               b(ior) = 2. + r2(ior)
               c(ior) = -r3(ior)
            else
               c(ior-1) = -r1(ior)
               a(ior) = 2. + r2(ior)
               b(ior) = -r3(ior)
            endif
            d(ior) = r1(ior) * U(ior-1)     &
                   + (2.-r2(ior)) * U(ior)  &
                   + r3(ior) * U(ior+1)
         endif
      enddo
      !$OMP END PARALLEL Do
      
      if (ithomas == 1) then
         a(anze+1) = 0.0
         b(anze+1) = 0.0
         a(anze+1) = -2. * r1(anze+1)
         b(anze+1) =  2. + r2(anze+1)
      else
         c(anze)   = 0.0
         a(anze+1) = 0.0
         c(anze)   = -2. * r1(anze+1)
         a(anze+1) =  2. + r2(anze+1)
      endif
      
      d(anze+1) = 2. * r1(anze+1) * U(anze)   &
                + (2.-r2(anze+1)) * U(anze+1)
      
      if (ithomas == 1) then
         call thomas(a, b, c, d, anze, U, imarker)
      else
         call trimat(a, b, c, d, anze, U, ktrans, imarker)
      endif
      
      if (isgn(1,nkz) == 1) U(1) = Ux1
      if (isgn(anze+1,nkz) == -1) U(anze+1) = Ux_nx
   enddo ! Ende subTime Schleife
   deltat = deltat_z
   return
end subroutine crank_nicolson