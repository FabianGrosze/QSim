subroutine basispoint(anze, flag, deltat, vmitt, uvert, elen, xpoint, m,      &
                      nkz, iwahld, isgn)
                      
   use module_alloc_dimensions
   implicit none           
   
   ! --- dummy arguments ---
   integer, intent(in)                           :: anze
   integer, intent(in),    dimension(1000)       :: flag
   real,    intent(in)                           :: deltat
   real,    intent(inout), dimension(ialloc2)    :: vmitt
   real,    intent(in),    dimension(50,ialloc2) :: uvert
   real,    intent(inout), dimension(ialloc2)    :: elen
   real,    intent(inout), dimension(ialloc2,50) :: xpoint
   integer, intent(inout), dimension(ialloc2,50) :: m
   integer, intent(in)                           :: nkz
   integer, intent(in)                           :: iwahld
   integer, intent(inout), dimension(ialloc2,50) :: isgn
    
   ! --- local variables ---
   integer :: j, i, ior, iein
   real    :: vx, vx1, vx0, deltatt, hcondt, dxx

   vmitt(anze+1) = vmitt(anze)
   elen(anze+1) = elen(anze)
   iein = 0
   
   if (iwahld == 1) then
      vx0 = vmitt(1)
   else
      vx0 = uvert(nkz,1)
   endif
   
   j = 2
   m(1,nkz) = 0
   if (vx0 < 0.0) j = 1
   if (j == 2) isgn(1,nkz) = 1
   
   do ior = j,anze+1
      m(ior,nkz) = ior
      isgn(ior,nkz) = 1
      
      if (iwahld == 1) then
         vx0 = vmitt(m(ior,nkz))
      else
         vx0 = uvert(nkz,m(ior,nkz))
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
         if (iwahld == 1) then
            vx0 = vmitt(m(ior,nkz))
            vx1 = vmitt(m(ior,nkz)-isgn(ior,nkz))
         else
            vx0 = uvert(nkz,m(ior,nkz))
            vx1 = uvert(nkz,m(ior,nkz)-isgn(ior,nkz))
         endif
         
         if (isgn(ior,nkz) == 1)  then
            dxx = elen(m(ior,nkz)-1)
         else if (isgn(ior,nkz) == -1) then
            dxx = elen(m(ior,nkz))
         endif
         
         ! umstellen der formel nach newton (lineare interpolation)
         if (isgn(ior,nkz) == 1 .and. vx1 < 0.0) then
            xpoint(ior,nkz) = -vx0*(dxx/(vx1-vx0))  
            exit
         else if (isgn(ior,nkz) == -1 .and. vx1 > 0.0) then
            xpoint(ior,nkz) = -vx0*(dxx/(vx1-vx0))
            exit
         endif
         
         vx = (vx0 + vx1) / 2.
         vx = vx1   !loeschen
         hcondt = dxx/abs(vx)
         
         if (hcondt < deltatt) then         ! i
            if (isgn(ior,nkz) == 1 .and. m(ior,nkz) == 2) then
               xpoint(ior,nkz) = dxx
               exit
            endif
            
            if (isgn(ior,nkz) == -1 .and. m(ior,nkz) == anze+1) then
               xpoint(ior,nkz) = dxx
               exit
            endif
            
            if (m(ior,nkz) /= 1) then
               if (isgn(ior,nkz) == 1 .and. m(ior,nkz) > 2 .and. flag(m(ior,nkz)-1) /= 4) then  ! ii
                  m(ior,nkz) = m(ior,nkz)-1
                  deltatt = deltatt-hcondt
                  cycle
               endif
            endif
            
            if (isgn(ior,nkz) == -1 .and. m(ior,nkz) < anze+1 .and. flag(m(ior,nkz)+1) /= 4) then
               m(ior,nkz) = m(ior,nkz)+1
               deltatt = deltatt-hcondt
               cycle
            endif
            
            if (m(ior,nkz) /= 1) then
               if (isgn(ior,nkz) == 1 .and. flag(m(ior,nkz)-1) == 4) then
                  xpoint(ior,nkz) = dxx
                  exit
               endif
            endif
            
            if (isgn(ior,nkz) == -1 .and. flag(m(ior,nkz)+1) == 4) then
               xpoint(ior,nkz) = dxx
               exit
            endif
            
         else ! i
            xpoint(ior,nkz) = abs(vx)  *deltatt
            if (isgn(ior,nkz) == -1 .and. flag(m(ior,nkz)+1) == 4) iein = iein+1
            exit
         endif
      enddo
   
   enddo
   return
end subroutine basispoint
