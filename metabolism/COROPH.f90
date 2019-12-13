  SUBROUTINE coroph(coro,coros,tempw,flae,elen,anze,ior                                &
            &,volfco,aki,agr,algcok,algcog,tflie,bsohlm,lboem,coroI,coroIs             &
            &,abl,algcob,mstr,itags,monats,jahrs,ilang,nbuhn,ilbuhn,azStrs             &                                                   
            &,kontroll ,jjj ) !!wy
                                                                   
                                                                       
!     EIN PROGRAMM zu Berechnung des Einflusses von Corophium auf das   
!     Phytoplankton                                                     
                                                                       
                                                                       
                                                                       
!     AUTOR : VOLKER KIRCHESCH                                          
                                                                       
                                                                       
                                                                       
                                                                       
!     STAND : 10.10.2011                                                
                                                                       
                                                                       
      logical kontroll !!wy
      integer jjj !!wy                                                                       
    integer                       :: anze, azStrs
    integer, Dimension(azStrs)    :: nbuhn

    real                          :: lw1, lw2, kmonach, kmor1, kmor2, kmor3 
    real, Dimension(1000)         :: elen, tempw, flae, volfco, aki, agr, algcok, algcog, algcob, abl, bsohlm,lboem,coroI,coroIs              
    real, Dimension(1000,5)       :: coro, coros 


 integer, Dimension(:), allocatable, save :: itco1,itco2,itco3


    save jahr_tst 


 If (.Not.allocated(itco1)) allocate(itco1(azStrs))
 If (.Not.allocated(itco2)) allocate(itco2(azStrs))
 If (.Not.allocated(itco3)) allocate(itco3(azStrs))

                                                                       
!      open(unit=567,file='coroph.tst')                                 
                                                                       
    if(ilang==0.or.jahr_tst<jahrs)then 
      do j = 1,azStrs 
          itco1(j) = 0 
          itco2(j) = 0 
          itco3(j) = 0 
      enddo 
        else 
          ig1tag = 15 
          ig1mon = 4 
          ig2tag = 15 
          ig2mon = 6 
          ig3tag = 15 
          ig3mon = 8 
                                                                       
          lw1 = 5.5 
          schlupf = 0.7 
          fweib1 = 0.75 
          kmonach = 0.01 
                                                                       
          lw2 = 4.5 
          fweib2 = 0.65 
                                                                       
          kmor1 = 0.23 
          kmor2 = 0.115 
          kmor3 = 0.011 
                                                                       
          rfil = 0.005*24.   ! Filtrierrate in l/(Ind*d)   
                                                                       
          do ior = 1,anze 
            algcok(ior) = 0.0 
            algcog(ior) = 0.0 
            algcob(ior) = 0.0 
                                                                       
            coroI(ior) = coro(ior,1)+coro(ior,2)+coro(ior,3)+coro(ior,4)      &
                        +coro(ior,5)                                                      
                                                                       
            coroIs(ior) = coros(ior,1)+coros(ior,2)+coros(ior,3)+coros(ior,4) &
                       +coros(ior,5)                                                     
                                                                       
            corg = coroI(ior)*2.*lboem(ior)*elen(ior) 
            corsg = coroIs(ior)*bsohlm(ior)*elen(ior) 
                                                                       
                                                                       
!     Berechnung des Filtrierten Wasservolumens in m3                   
            volfl = rfil*corg+rfil*corsg 
            volfm3 = volfl*tflie/1000. 
                                                                       
                                                                       
            vol = flae(ior)*elen(ior) 
            hconf = volfm3/vol 
            if(hconf.gt.1.)hconf = 1. 
            algcok(ior) = aki(ior)*hconf 
            algcog(ior) = agr(ior)*hconf 
            algcob(ior) = abl(ior)*hconf 
                                                                       
            volfco(ior) = hconf*100. 
                                                                       
                                                                       
            if(monats<=2)then 
              NRS = ITAGS+31*(MONATS-1) 
                else
                  NRS = (ITAGS+31*(MONATS-1)-INT(0.4*MONATS+2.3)) 
           endif                                                            

           if(ig1mon<=2)then 
             NRg1 = ig1tag+31*(ig1mon-1) 
               else
                 NRg1 = (ig1tag+31*(ig1mon-1)-INT(0.4*ig1mon+2.3)) 
           endif
                                                                       
           if(ig2mon<=2)then 
             NRg2 = ig2tag+31*(ig2mon-1) 
               else 
                 NRg2 = (ig2tag+31*(ig2mon-1)-INT(0.4*ig2mon+2.3)) 
           endif
                                                                       
           if(ig3mon<=2)then 
             NRg3 = ig3tag+31*(ig3mon-1) 
               else 
                 NRg3 = (ig3tag+31*(ig3mon-1)-INT(0.4*ig3mon+2.3)) 
           endif 
                                                                       
          if(nrs<nrg1)cycle
          if(nrg1==nrs)then
            izeiger=1
              if(itco1(mstr).eq.1)izeiger = 2
          endif

          if(nrs>nrg1.and.nrs<nrg2)izeiger = 2 

          if(nrg2==nrs)then
            izeiger = 3
              if(itco2(mstr)==1)izeiger = 4
          endif 

          if(nrs>nrg2.and.nrs<nrg3)izeiger = 4 

          if(nrg3==nrs)then
            izeiger = 5
              if(itco3(mstr)==1)izeiger = 6  
          endif

          if(nrs>nrg3)izeiger = 6
                                                                       
                   
  
      if(izeiger==1)then   !Nachkommen der Jahresgruendungspopulation nco = 1 
        anzEi = (-13.4+6.95*lw1)*coro(ior,1)*fweib1 
        coro(ior,2) = anzEi*schlupf 
        coro(ior,1) = 0.0 
                                                                       
        anzEi = (-13.4+6.95*lw1)*coros(ior,1)*fweib1 
        coros(ior,2) = anzEi*schlupf 
        coros(ior,1) = 0.0 
        cycle
      endif 

                                                                       
      if(izeiger==2)then 
        coro(ior,2) = coro(ior,2)*exp(-kmonach*tflie) 
        coros(ior,2) = coros(ior,2)*exp(-kmonach*tflie) 
        cycle
      endif 
                                                                       
      if(izeiger==3)then 
        anzEi = (-13.4+6.95*lw2)*coro(ior,2)*fweib2 
        coro(ior,3) = anzEi*schlupf 
        coro(ior,2) = coro(ior,2)*0.3 
                                                                       
        anzEi = (-13.4+6.95*lw2)*coros(ior,2)*fweib2 
        coros(ior,3) = anzEi*schlupf 
        coros(ior,2) = coros(ior,2)*0.3 
        cycle
      endif 
                                                                     
      if(izeiger==4)then 
        coro(ior,2) = coro(ior,2)*exp(-kmonach*tflie) 
        coros(ior,2) = coros(ior,2)*exp(-kmonach*tflie) 
                                                                       
        coro(ior,3) = coro(ior,3)*exp(-kmonach*tflie) 
        coros(ior,3) = coros(ior,3)*exp(-kmonach*tflie) 
        cycle
      endif 
!                                                                       
      if(izeiger==5)then 
        anzEi = (-13.4+6.95*lw1)*coro(ior,2)*fweib1 
        coro(ior,4) = anzEi*schlupf 
        coro(ior,2) = 0.0 
                                                                       
        anzEi = (-13.4+6.95*lw1)*coros(ior,2)*fweib1 
        coros(ior,4) = anzEi*schlupf 
        coros(ior,2) = 0.0 
                                                                       
        anzEi = (-13.4+6.95*lw2)*coro(ior,3)*fweib2 
        coro(ior,5) = anzEi*schlupf 
                                                                       
        anzEi = (-13.4+6.95*lw2)*coros(ior,3)*fweib2 
        coros(ior,5) = anzEi*schlupf 
        cycle
      endif 
                                                                       
      if(izeiger==6)then 
        coro(ior,4) = coro(ior,4)*exp(-kmor1*tflie) 
        coros(ior,4) = coros(ior,4)*exp(-kmor1*tflie) 
        if(coro(ior,4)<1.)coro(ior,4) = 0.0 
        if(coros(ior,4)<1.)coros(ior,4) = 0.0 
                                                                       
        coro(ior,3) = coro(ior,3)*exp(-kmor2*tflie) 
        coros(ior,3) = coros(ior,3)*exp(-kmor2*tflie) 
        if(coro(ior,3)<1.)coro(ior,3) = 0.0 
        if(coros(ior,3)<1.)coros(ior,3) = 0.0 
                                                                       
        coro(ior,5) = coro(ior,5)*exp(-kmor3*tflie) 
        coros(ior,5) = coros(ior,5)*exp(-kmor3*tflie) 
      endif           

        enddo 
                                                                       
      if(nrg1==nrs.and.nbuhn(mstr)==0)itco1(mstr) = 1
      if(nrg1==nrs.and.nbuhn(mstr)==1.and.ilbuhn==1)itco1(mstr) = 1
      if(nrg2==nrs.and.nbuhn(mstr)==0)itco2(mstr) = 1
      if(nrg2==nrs.and.nbuhn(mstr)==1.and.ilbuhn==1)itco2(mstr) = 1
      if(nrg3==nrs.and.nbuhn(mstr)==0)itco3(mstr) = 1
      if(nrg3==nrs.and.nbuhn(mstr)==1.and.ilbuhn==1)itco3(mstr) = 1

    endif

      if(nbuhn(mstr)==1.and.ilbuhn==1)jahr_tst = jahrs
      if(nbuhn(mstr)==0)jahr_tst = jahrs

    return
    end subroutine coroph                                           
