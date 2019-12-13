      subroutine km_sys(azStrs,mstra,StaKm,RBkm,RBkmLe,RBtyp,mRBs       &
     &,mWehr,mStas,iorLah,iorLeh,mstrLe,abfr,cpfad)                   
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
!     AUTOR : VOLKER KIRCHESCH                                          
                                                                       
                                                                      
                                                                       
                                                                       
!     STAND : 21.03.2003                                                
                                                                       
                                                                       
      character (len=255)               :: cpfad
      character (len=275)               :: pfadstring 

      integer                           :: azStrs, azStr, flag 
      integer, Dimension(1000)          :: flaga, jieina
  
      integer, Dimension(azStrs)        :: mwehr, mRBs, mStas, mStra, abfr
      integer, Dimension(azStrs,100)    :: iorLah, iorLeh, mstrLe, RBtyp 

      real, Dimension(1000)             :: RBkma
      real, Dimension(azStrs,100)       :: RBkm, RBkmLe
      real, Dimension(azStrs,1000)      :: StaKm  


      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'km_sys.dat' 
      open(unit=391, file=pfadstring)
      rewind (391) 

!      open(unit=301,file='km_sys.tst')                                 
!                                                                       
!                                                                       
!...Test ob Einleitung oberem oder unterem Rand liegt. Falls ja wird er 
!   stromab/stromauf verschoben                                         
!                                                                       
!      do 79 azStr = 1,azStrs 
!      mstr = mStra(azStr) 
!      if(mRBs(mstr).eq.0)goto 79 
!      do 81 mRB = 1,mRBs(mstr) 
!                                                                       
!      if(StaKm(mstr,1).eq.RBkm(mstr,mRB).and.RBtyp(mstr,mRB).eq.1       &
!     &.and.abfr(mstr).eq.0.and.RBkmLe(mstr,mRB).lt.0.0)then                                         
!      RBkm(mstr,mRB) = RBkm(mstr,mRB)-0.05 
!      write(199,1970)mRB,mstr 
! 1970 format(2x,'die ',I3,'. Randbedingung am Strang:',I3,              &
!     &' wurde um 50 m stromab versetzt')                                
!      endif 
!                                                                       
!      if(StaKm(mstr,1).eq.RBkm(mstr,mRB).and.RBtyp(mstr,mRB).eq.1       &
!     &.and.abfr(mstr).eq.1.and.RBkmLe(mstr,mRB).lt.0.0)then                                         
!      RBkm(mstr,mRB) = RBkm(mstr,mRB)+0.05 
!      write(199,1970)mRB,mstr 
!      endif 
!   81 continue 
!                                                                       
!      if(StaKm(mstr,mStas(mstr)).eq.RBkm(mstr,mRBs(mstr)).and.                    &
!     &abfr(mstr).eq.0.and.RBtyp(mstr,mRB).eq.1.and.RBkmLe(mstr,mRB).lt.0.0)then                    
!      RBkm(mstr,mRBs(mstr)) = RBkm(mstr,mRBs(mstr))+0.05 
!      write(199,1971)mRBs(mstr),mstr 
! 1971 format(2x,'die ',I3,'. Randbedingung am Strang:',I3,                        &
!     &' wurde um 50 m stromauf versetzt')                               
!      endif 
!                                                                       
!                                                                       
!      if(StaKm(mstr,mStas(mstr)).eq.RBkm(mstr,mRBs(mstr)).and.                    &
!     &abfr(mstr).eq.1.and.RBtyp(mstr,mRB).eq.1..and.RBkmLe(mstr,mRB).lt.0.0)then                    
!      RBkm(mstr,mRBs(mstr)) = RBkm(mstr,mRBs(mstr))-0.05 
!      write(199,1971)mRBs(mstr),mstr 
!      endif 
!                                                                       
!   79 continue 
                                                                       
                                                                       
      do 10 azStr = 1,azStrs 
      mstr = mStra(azStr) 
                                                                       
      j = 1 
      RBkma(j) = Stakm(mstr,1) 
      jieina(j) = 0 
      flaga(j) = 0 
      mRB = 1 

      if(RBkmLe(mstr,mRB)/=-1.)mRB = mRB+1   
                                                                      
    4 j =j+1 
      jieina(j) = 0 
      flaga(j) = 0 
      if(abfr(mstr).eq.1)goto 30 
      RBkma(j)  = Stakm(mstr,j) 
      goto 5 
!                                                                       
   30 RBkma(j)  = Stakm(mstr,j) 
!                                                                       
!                                                                       
    5 continue 
      if(RBkm(mstr,mRB).le.RBkma(j-1).and.RBkm(mstr,mRB)                &
     &.ge.RBkma(j).and.abfr(mstr).eq.0)goto 37                          
      if(RBkm(mstr,mRB).ge.RBkma(j-1).and.RBkm(mstr,mRB)                &
     &.le.RBkma(j).and.abfr(mstr).eq.1)goto 37                          
      goto 39 
                                                                       
   37 if(RBtyp(mstr,mRB)==0.or.mstrLe(mstr,mRB)>0)then 
      mRB = mRB+1
          if(mRB.gt.mRBs(mstr).and.abfr(mstr).eq.0)RBkm(mstr,mRB) =99999. 
          if(mRB.gt.mRBs(mstr).and.abfr(mstr).eq.1)RBkm(mstr,mRB) = -1. 
          goto 5 
      endif 
      
      if(RBkmLe(mstr,mRB)/=-1.)then
        mRB = mRB+1   !hier 
          if(mRB.gt.mRBs(mstr).and.abfr(mstr).eq.0)RBkm(mstr,mRB) =99999. 
          if(mRB.gt.mRBs(mstr).and.abfr(mstr).eq.1)RBkm(mstr,mRB) = -1. 
          goto 5 
      endif 
                                                                       
      hcon1 = abs(RBkm(mstr,mRB)-RBkma(j-1)) 
      hcon2 = abs(RBkm(mstr,mRB)-RBkma(j)) 
      if(hcon1<hcon2.and.RBtyp(mstr,mRB)/=2)then 
      flaga(j-1) = 4 
      jieina(j-1) = jieina(j-1)+1 
      endif 
      if(hcon1>=hcon2.and.RBtyp(mstr,mRB)/=2)then 
      flaga(j) = 4 
      jieina(j) = jieina(j)+1 
      endif 
      mRB = mRB+1 
      if(RBkmLe(mstr,mRB)/=-1.)mRB = mRB+1  !hier
      if(mRB.gt.mRBs(mstr).and.abfr(mstr).eq.0)RBkm(mstr,mRB) =99999. 
      if(mRB.gt.mRBs(mstr).and.abfr(mstr).eq.1)RBkm(mstr,mRB) = -1. 
      goto 5 
!                                                                       
   39 if(flaga(j).ne.4)flaga(j) = 2 
      if(abs(RBkma(j)-Stakm(mstr,mStas(mstr))).le.0.001)goto 999 !0.008
      goto 4 
!                                                                       
!                                                                       
!                                                                       
  999 js = j 

      write(391,'(I5,2x,I5)')mstr,(js-1) 
      do 15 j = 2,js 
!      if(mwehr(mstr).eq.1.and.j.eq.(js-2).and.flaga(j+1).eq.4)then 
!      flaga(j) = 4 
!      jieina(j) = jieina(j)+1                                          
!      jieina(j) = jieina(j+1) 
!      endif 
!      if(mwehr(mstr).eq.1.and.j.eq.(js))then 
!      flaga(js) = 5 
!      jieina(js) = 0 
!      endif 
                                                                       
      write(391,'(f9.4,2x,i1,2x,I2)')RBkma(j),flaga(j)                  &
     &,jieina(j)                                                        
   15 continue 
                                                               
                                                                       
!....Bestimmung des Knoten Beginn und Ende der Diffusen Einleitung               
                                                                        
     ieinL = 0
     kk = 0
     j = 1
     do mRB = 1,mRBs(mstr)  
      if(RBtyp(mstr,mRB)==0.or.RBkmLe(mstr,mRB)==-1.or.RBtyp(mstr,mRB)==2)cycle                               

      if(RBkm(mstr,mRB)==Stakm(mstr,j))then
         ieinL = 1
         iorLah(mstr,ieinL) = j
      endif
        
      do j = 2,js
        if(flaga(j).eq.4)kk = kk+1 
          if(RBKm(mstr,mRB)==RBkma(j))then
            ieinL = ieinL + 1
            iorLah(mstr,ieinL) = j + kk
              else if(RBKmLe(mstr,mRB)==RBkma(j))then
                iorLeh(mstr,ieinL) = j + kk
                kk = 0
                exit
         endif
       enddo        
     enddo
     
   10 continue 
                                                                       
      close (391) 

      return 
      END                                           
