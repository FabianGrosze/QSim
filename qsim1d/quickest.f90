subroutine quickest(U, vmitt, Uvert, dx, DeltaT, nx, flag, ktrans,mstr, nkz, nkzs, &
                    iwahlD, isgn, Uhrz, itags, monats, itime,iwsim)
                    
   implicit none
   
   integer                       :: nkz, mstr, monats, i, iwsim
   integer                       :: iwahld, itime, itags
   integer                       :: nx, ktrans
   real                          :: uv, uhrz, term, s4, s3
   real                          :: s2, s1, gradr, dx_stern, dx_p1
   real                          :: dx_m1, deltat, curvr, crr2, crr1
   real                          :: crnt, crm
   integer, dimension(1000)      :: nkzs
   integer, dimension(1000)      :: flag
   integer, dimension(1000,50)   :: isgn
   real, dimension(1000)         :: dx, Crr, F_plus, vmitt, vr, U, U_neu
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
         if (i <  nx+1) vr(i) = (vmitt(i) + vmitt(i+1)) / 2.
         if (i == nx+1) vr(i) = vmitt(i)
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
         S2   = U(2)
         S3   = U(i)
         Crr1 = Crr(i)
         Crr2 = Crr(i)
         if (i > 1) then
            S2   = U(i-1)
            Crr1 = Crr(i-1)
         endif
         if (i == 1 .or. flag(i) == 4) then
            S2   = U(i)
            Crr1 = Crr(i)
         endif
         S3   = U(i)
         if (i <  nx + 1) S4 = U(i+1)
         if (i == nx + 1 .or. flag(i+1) == 4) S4 = U(i)
      else           ! vr ist kleiner 0.0
         S2    = U(i)
         S3    = U(i)
         S4    = U(i)
         dx_p1 = dx(i)
         if (i < nx + 1) then
            S3    = U(i+1)
            if (flag(i) == 4) S4 = U(i+1)
            if (i == nx .or. flag(i+1) == 4) then
               S2 = U(i+1)
            elseif (i < nx) then
               S2 = U(i+2)
               dx_p1 = dx(i+1)
            endif
         endif
      endif
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
         Crm = (Crr(i-1) + Crr(i)) / 2.
      endif
      if (i == 1) then
         U_neu(i) = U(i) + real(min(0, isgn(i,nkz))) * Crm * (U(i) - U(i+1))
      elseif (i == nx+1) then
         U_neu(i) = U(i) - real(max(0, isgn(i,nkz))) * Crm * (U(i) - U(i-1))
      else
         U_neu(i) = U(i) + real(isgn(i,nkz)) * Crm * (F_plus(i-1) - F_plus(i))
      endif
      if (abs(U_neu(i)) < 1.e-25) U_neu(i) = 0.0
      if ((iwsim == 4 .or. (ktrans /= 1 .and. ktrans /= 57)) .and. U_neu(i) < 0.0) U_neu(i) = 0.0
   enddo !Ende Knoten-Schleife
   if (nx == 1) then ! Falls der Strang nur aus einem Knoten besteht
      U_neu(1)    = U(1)
      U_neu(nx+1) = U(1)
   endif
   do i = 1,nx+1
      U(i) = U_neu(i)
   enddo
end subroutine quickest