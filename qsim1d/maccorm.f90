subroutine MacCorm(U,elen,flag,dl,deltat,anze,temp0,mstr,ktrans,iwahlD,nkz,nkzs,itags,monats,itime           &
                   ,isub_dtx,isgn,uhrz)
   
   use allodim
   implicit none
   
   integer                            :: nkz, mstr, monats, ktrans, i
   integer                            :: iwahld, itime_sub, itime, itags, isub_dtx
   real                               :: ux_nx, ux1, uhrz, temp0, s3
   real                               :: s2, s1, deltat_z, deltat
   integer                            :: anze
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