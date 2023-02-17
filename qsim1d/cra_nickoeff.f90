subroutine cra_NicKoeff(elen,r1,r2,r3,dl,flag,deltat,anze,nkzs,nkz,mstr,ktrans,imarker,icraNicKoeff,itags,uhrz)
   
   implicit none
                             
   integer                   :: nkz, mstr, ktrans, itags, ior
   integer                   :: iein, icranickoeff, anze
   real                      :: uhrz, dlmit, deltat, alpha
   real                      :: nenner
   integer, dimension(1000)  :: flag, imarker, nkzs
   real, dimension(1000)     :: elen, r1, r2, r3, dl
   
   
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