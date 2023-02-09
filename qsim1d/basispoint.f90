subroutine basisPoint(anze,flag,deltat,vmitt,Uvert,Uvertt_1,vmittt_1,elen,xPoint,m,iwied,itime,izeits  &
                      ,jpoin1,sumdet,tfliesec,nkz,nkzs,iwahlD,ipo,mstr,itags,monats,ktrans              &
                      ,isgn,vx_Cr,nkztot_max,ianze_max,uhrz)
                      
   use allodim
   implicit none           
   
   integer                        :: nkz, nkztot_max, mstr, monats, ktrans
   integer                        :: j, jpoin1, i, izeits, iwied
   integer                        :: iwahld, itime, itags, ipo, ior
   integer                        :: iein, ianze_max
   real                           :: vx_cr, vx, vx1, vx0, uhrz, deltatt
   real                           :: tfliesec, sumdet, hcondt, dxx, deltat
   integer, dimension(1000)       :: flag, nkzs
   integer, dimension(1000,50)    :: m, isgn
   integer                        :: anze
   real, dimension(1000)          :: elen, vmitt
   real, dimension(50,1000)       :: vmittt_1, Uvert
   real, dimension(1000,50)       :: xpoint
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
end subroutine basispoint
