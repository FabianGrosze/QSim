subroutine quickest(U, vmitt, Uvert, dx, DeltaT, nx, flag, ktrans, nkz,  &
                    iwahlD, isgn, iwsim)
   use allodim                 
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2)    :: u
   real,    intent(inout), dimension(ialloc2)    :: vmitt
   real,    intent(inout), dimension(50,ialloc2) :: Uvert
   real,    intent(inout), dimension(ialloc2)    :: dx
   real,    intent(in)                           :: deltat
   integer, intent(in)                           :: nx
   integer, intent(in),    dimension(ialloc2)    :: flag
   integer, intent(in)                           :: ktrans
   integer, intent(in)                           :: nkz
   integer, intent(in)                           :: iwahld
   integer, intent(inout), dimension(ialloc2,50) :: isgn
   integer, intent(in)                           :: iwsim
   
   
   ! -- local variables ---
   integer                  :: i
   real                     :: uv, term, s1, s2, s3, s4
   real                     :: gradr, dx_stern, dx_p1
   real                     :: dx_m1, curvr, crr2, crr1, crnt, crm
   real, dimension(1000)    :: Crr, F_plus, vr, U_neu
   
   save U_neu
   
   if (iwahld == 1) then
      vmitt(nx+1) = vmitt(nx)
      dx(nx+1) = dx(nx)
   else
      uvert(nkz,nx+1) = uvert(nkz,nx)
      dx(nx+1) = dx(nx)
   endif
   
   do i = 1,nx+1
      if (iwahld == 1) then
         if (i <  nx+1) vr(i) = (vmitt(i) + vmitt(i+1)) / 2.
         if (i == nx+1) vr(i) = vmitt(i)
      else
         vr(i) = uvert(nkz,i)
      endif
      
      if (vr(i) >= 0.0) then
         isgn(i,nkz) = 1
      else
         isgn(i,nkz) = -1
      endif
      
      crr(i) = abs(vr(i)) * deltat / dx(i)
      
      if (isgn(i,nkz) == 1) then
         if (i == 1 .or. flag(i) == 4) then
            s1 = u(i)
            dx_m1 = dx(i)
         else
            s1 = u(i-1)
            dx_m1 = dx(i-1)
         endif
         s2 = u(i)
         if (i == nx+1 .or. flag(i+1) == 4) then
            s3 = u(i)
         else
            s3 = u(i+1)
         endif
      
      else         
         ! vr ist kleiner 0.0
         s1 = u(i)
         if (flag(i) == 4) then
            s1 = u(i+1)
         endif
         
         s2 = u(i+1)
         if (i == nx+1) then
            s2 = u(i)
         endif
         
         s3 = u(i+2)
         dx_p1 = dx(i+1)
         if (i == nx .or. flag(i+1) == 4) then
            s3 = u(i+1)
            dx_p1 = dx(i)
         endif
         
         if (i == nx+1) then
            s3 = u(i)
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
         dx_Stern = 0.5 * dx(i-1) + 0.5 * dx(i)
      else
         dx_Stern = 0.5 * dx(i) + 0.5 * dx(i)
      endif
      
      if (isgn(i,nkz) == 1) then
         curvr = (((s3-s2)/dx(i)) - ((s2-s1) / dx_m1)) / dx_stern
      else
         curvr = (((s3-s2)/dx_p1) - ((s2-s1) / dx(i))) / dx_stern
      endif
      f_plus(i) = 0.5 * term            &
                - (crr(i)/2.) * gradr   &
                - 1./6. * dx(i)**2 * (1. - crr(i)**2) * curvr
   enddo
   
   do i = 1,nx+1
      ! ultimate-Limiter
      if (isgn(i,nkz) == 1) then
         s2   = u(2)
         s3   = u(i)
         crr1 = crr(i)
         crr2 = crr(i)
         if (i > 1) then
            s2   = u(i-1)
            crr1 = crr(i-1)
         endif
         if (i == 1 .or. flag(i) == 4) then
            s2   = u(i)
            crr1 = crr(i)
         endif
         s3   = u(i)
         if (i <  nx + 1) s4 = u(i+1)
         if (i == nx + 1 .or. flag(i+1) == 4) s4 = u(i)
      
      else  
      ! vr ist kleiner 0.0
         s2    = u(i)
         s3    = u(i)
         s4    = u(i)
         dx_p1 = dx(i)
         
         if (i < nx + 1) then
            s3    = u(i+1)
            if (flag(i) == 4) s4 = u(i+1)
            if (i == nx .or. flag(i+1) == 4) then
               s2 = u(i+1)
            elseif (i < nx) then
               s2 = u(i+2)
               dx_p1 = dx(i+1)
            endif
         endif
      endif
      
      crnt = abs(crr(i))
      
      if (s2 >= s3 .and. s3 >= s4) then
         uv = max(s4, s2 + (s3-s2)/crnt)
         if (f_plus(i) > s3) f_plus(i) = s3
         if (f_plus(i) < uv) f_plus(i) = uv
      
      else if (s2 <= s3 .and. s3 <= s4) then
         uv = min(s4, s2 + (s3-s2)/crnt)
         if (f_plus(i) < s3) f_plus(i) = s3
         if (f_plus(i) > uv) f_plus(i) = uv
      else
         f_plus(i) = s3
      endif
   enddo
   
   do i = 1,nx+1
      if (flag(i) == 4) then
         u_neu(i) = u(i)
         cycle
      endif
      
      if (i == 1) then
         crm = crr(i)
      else
         crm = (crr(i-1) + crr(i)) / 2.
      endif
      
      if (i == 1) then
         u_neu(i) = u(i) + real(min(0, isgn(i,nkz))) * crm * (u(i) - u(i+1))
      elseif (i == nx+1) then
         u_neu(i) = u(i) - real(max(0, isgn(i,nkz))) * crm * (u(i) - u(i-1))
      else
         u_neu(i) = u(i) + real(isgn(i,nkz)) * crm * (f_plus(i-1) - f_plus(i))
      endif
      
      if (abs(u_neu(i)) < 1.e-25) u_neu(i) = 0.0
      
      if ((iwsim == 4 .or. (ktrans /= 1 .and. ktrans /= 57))) then
         if (u_neu(i) < 0.0) u_neu(i) = 0.0
      endif
   enddo !Ende Knoten-Schleife
   
   if (nx == 1) then ! falls der strang nur aus einem knoten besteht
      u_neu(1)    = u(1)
      u_neu(nx+1) = u(1)
   endif
   
   do i = 1,nx+1
      u(i) = u_neu(i)
   enddo
end subroutine quickest