! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !

!> km_sys
subroutine km_sys(azStrs,mstra,StaKm,RBkm,RBkmLe,RBtyp,mRBs,mWehr,mStas,iorLah, &
                  iorLeh,mstrLe,abfr,cpfad)
   
   
   character (len = 255)             :: cpfad
   character (len = 275)             :: pfadstring
   integer                           :: azStrs, azStr
   integer, dimension(1000)          :: flaga, jieina
   
   integer, dimension(azStrs)        :: mwehr, mRBs, mStas, mStra, abfr
   integer, dimension(azStrs,100)    :: iorLah, iorLeh, mstrLe, RBtyp
   real, dimension(1000)             :: RBkma
   real, dimension(azStrs,100)       :: RBkm, RBkmLe
   real, dimension(azStrs,1000)      :: StaKm
   
   
   pfadstring = trim(adjustl(cpfad)) // 'km_sys.dat'
   open(unit = 391, file = pfadstring)
   rewind (391)
   
   do azStr = 1,azStrs
      mstr = mStra(azStr)
      
      j = 1
      RBkma(j) = Stakm(mstr,1)
      jieina(j) = 0
      flaga(j) = 0
      mRB = 1
      if (RBkmLe(mstr,mRB) /= -1.)mRB = mRB+1
      
      4 continue
      j = j+1
      jieina(j) = 0
      flaga(j) = 0
      if (abfr(mstr) == 1)goto 30
      RBkma(j) = Stakm(mstr,j)
      goto 5
      
      30 continue
      RBkma(j) = Stakm(mstr,j)

      5 continue
      if (RBkm(mstr,mRB) <= RBkma(j-1) .and. RBkm(mstr,mRB)                &
          >= RBkma(j) .and. abfr(mstr) == 0)goto 37
      if (RBkm(mstr,mRB) >= RBkma(j-1) .and. RBkm(mstr,mRB)                &
          <= RBkma(j) .and. abfr(mstr) == 1)goto 37
      goto 39
      
      37 continue 
      if (RBtyp(mstr,mRB) == 0 .or. mstrLe(mstr,mRB) > 0) then
         mRB = mRB+1
         if (mRB > mRBs(mstr) .and. abfr(mstr) == 0)RBkm(mstr,mRB) = 99999.
         if (mRB > mRBs(mstr) .and. abfr(mstr) == 1)RBkm(mstr,mRB) = -1.
         goto 5
      endif
      
      if (RBkmLe(mstr,mRB) /= -1.) then
         mRB = mRB+1   !hier
         if (mRB > mRBs(mstr) .and. abfr(mstr) == 0)RBkm(mstr,mRB) = 99999.
         if (mRB > mRBs(mstr) .and. abfr(mstr) == 1)RBkm(mstr,mRB) = -1.
         goto 5
      endif
      
      hcon1 = abs(RBkm(mstr,mRB)-RBkma(j-1))
      hcon2 = abs(RBkm(mstr,mRB)-RBkma(j))
      if (hcon1 < hcon2 .and. RBtyp(mstr,mRB) /= 2) then
         flaga(j-1) = 4
         jieina(j-1) = jieina(j-1)+1
      endif
      if (hcon1>=hcon2 .and. RBtyp(mstr,mRB) /= 2) then
         flaga(j) = 4
         jieina(j) = jieina(j)+1
      endif
      mRB = mRB+1
      if (RBkmLe(mstr,mRB) /= -1.)mRB = mRB+1  !hier
      if (mRB > mRBs(mstr) .and. abfr(mstr) == 0)RBkm(mstr,mRB) = 99999.
      if (mRB > mRBs(mstr) .and. abfr(mstr) == 1)RBkm(mstr,mRB) = -1.
      goto 5
      
      39 continue
      if (flaga(j) /= 4)flaga(j) = 2
      if (abs(RBkma(j)-Stakm(mstr,mStas(mstr))) <= 0.001)goto 999 !0.008
      goto 4
      
      999 js = j
      write(391,'(I5,2x,I5)')mstr,(js-1)
      do j = 2,js
         ! if(mwehr(mstr).eq.1.and.j.eq.(js-2).and.flaga(j+1).eq.4)then
         !    flaga(j) = 4
         !    jieina(j) = jieina(j)+1
         !    jieina(j) = jieina(j+1)
         ! endif
         ! if(mwehr(mstr).eq.1.and.j.eq.(js))then
         !    flaga(js) = 5
         !    jieina(js) = 0
         ! endif
         write(391,'(f9.4,2x,i1,2x,I2)')RBkma(j),flaga(j),jieina(j)
      enddo
      
      ! Bestimmung des Knoten Beginn und Ende der Diffusen Einleitung
      ieinL = 0
      kk = 0
      j = 1
      do mRB = 1,mRBs(mstr)
         if (RBtyp(mstr,mRB) == 0 .or. RBkmLe(mstr,mRB) == -1 .or. RBtyp(mstr,mRB) == 2)cycle
         if (RBkm(mstr,mRB) == Stakm(mstr,j)) then
            ieinL = 1
            iorLah(mstr,ieinL) = j
         endif
         
         do j = 2,js
            if (flaga(j) == 4)kk = kk+1
            if (RBKm(mstr,mRB) == RBkma(j)) then
               ieinL = ieinL + 1
               iorLah(mstr,ieinL) = j + kk
            else if (RBKmLe(mstr,mRB) == RBkma(j)) then
               iorLeh(mstr,ieinL) = j + kk
               kk = 0
               exit
            endif
         enddo
      enddo
      
   enddo
   
   close (391)
   return
end
