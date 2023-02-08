subroutine lax_wendroff(U, vmitt, Uvert, DX, DT, NX, flag, nkz, nkzs,ktrans,itime,isgn,mstr,iwahld,itags,monats,Uhrz)
   
   implicit none
   
   integer                      :: n, nkz, mstr, monats, ktrans
   integer                      :: j, jj, iwahld, itime, itags
   integer                      :: NX, I
   real                         :: uhrz, s3, s2, s1, hcon
   real                         :: dt, cour_mit
   integer, dimension(1000)     :: nkzs, flag
   integer, dimension(1000,50)  :: isgn
   real,    dimension(1000)     :: DX, cour,U, U_neu, phi
   real,    dimension(1000)     :: vmitt
   real,    dimension(50,1000)  :: Uvert
   real                         :: m1, m2
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
         if (i == 1 .or. (i > 1 .and. flag(i-1) == 4)) then
            S1 = U(i+1)
            S2 = U(i)
            S3 = U(i)
         else if (i == nx+1 .or. flag(i) == 4) then
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