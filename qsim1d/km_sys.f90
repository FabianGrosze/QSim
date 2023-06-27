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

subroutine km_sys(mstra, stakm, rbkm, rbkmle, rbtyp, mrbs, mstas, iorlah,  &
                  iorleh, mstrle, abfr, cpfad)
   
   use module_alloc_dimensions
   implicit none
   
   integer, intent(in),    dimension(azstrs)         :: mstra
   real,    intent(in),    dimension(azstrs,ialloc2) :: stakm
   real,    intent(inout), dimension(azstrs,ialloc1) :: rbkm
   real,    intent(in),    dimension(azstrs,ialloc1) :: rbkmle
   integer, intent(in),    dimension(azstrs,ialloc1) :: rbtyp
   integer, intent(in),    dimension(azstrs)         :: mrbs
   integer, intent(in),    dimension(azstrs)         :: mstas
   integer, intent(inout), dimension(azstrs,ialloc1) :: iorlah
   integer, intent(inout), dimension(azstrs,ialloc1) :: iorleh
   integer, intent(in),    dimension(azstrs,ialloc1) :: mstrle
   integer, intent(in),    dimension(azstrs)         :: abfr
   character(255), intent(in)                        :: cpfad

   ! --- local variable ---
   integer                     :: mstr, mrb, kk, j, js, ieinl, azstr
   integer, dimension(ialloc2) :: flaga, jieina
   real                        :: hcon1, hcon2
   real, dimension(ialloc2)    :: rbkma
   character(275)              :: pfadstring
   
   
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
      
      do while (.true.)
         j = j+1
         jieina(j) = 0
         flaga(j) = 0
         if (abfr(mstr) == 1) then 
            RBkma(j) = Stakm(mstr,j)
         else
            RBkma(j) = Stakm(mstr,j)
         endif
         
         do while (.true.)
            if ((RBkm(mstr,mRB) <= RBkma(j-1) .and. RBkm(mstr,mRB) >= RBkma(j) .and. abfr(mstr) == 0) .or. &
                (RBkm(mstr,mRB) >= RBkma(j-1) .and. RBkm(mstr,mRB) <= RBkma(j) .and. abfr(mstr) == 1)) then
               
               if (RBtyp(mstr,mRB) == 0 .or. mstrLe(mstr,mRB) > 0) then
                  mRB = mRB+1
                  if (mRB > mRBs(mstr) .and. abfr(mstr) == 0)RBkm(mstr,mRB) = 99999.
                  if (mRB > mRBs(mstr) .and. abfr(mstr) == 1)RBkm(mstr,mRB) = -1.
                  cycle
               endif
               
               if (RBkmLe(mstr,mRB) /= -1.) then
                  mRB = mRB+1 
                  if (mRB > mRBs(mstr) .and. abfr(mstr) == 0)RBkm(mstr,mRB) = 99999.
                  if (mRB > mRBs(mstr) .and. abfr(mstr) == 1)RBkm(mstr,mRB) = -1.
                  cycle
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
               
               if (RBkmLe(mstr,mRB) /= -1.)mRB = mRB+1
               if (mRB > mRBs(mstr) .and. abfr(mstr) == 0)RBkm(mstr,mRB) = 99999.
               if (mRB > mRBs(mstr) .and. abfr(mstr) == 1)RBkm(mstr,mRB) = -1.
            
            else
               exit
            endif
            
         enddo
         
         
         if (flaga(j) /= 4)flaga(j) = 2
         if (abs(RBkma(j)-Stakm(mstr,mStas(mstr))) <= 0.001) exit
      
      enddo
      
      js = j
      write(391, '(I5,2x,I5)')mstr,(js-1)
      do j = 2,js
         write(391,'(f9.4,2x,i1,2x,I2)') RBkma(j), flaga(j), jieina(j)
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
