  subroutine Schwermetalle(vabfl,qeinl,mstr,flag,anze,sedss,sedalk,sedalb,sedalg,hSSalg,SSalg,hph,vph,bssalg,bssalg_1,bph      &
                          ,bph_1,hglZn,hgsZn,egsZn,eglZn,hglCad,hgsCad,egsCad,eglCad,hglCu,hgsCu,egsCu,eglCu,hglNi,hgsNi       &
                          ,egsNi,eglNi,eph,ess,jiein,ilbuhn,azStrs,iformVert                                                   &                                                   
                          ,kontroll ,jjj ) !!wy  


!##############################################
!    Berechnung der Schwermetallkonzentrationen
!    Cd, Zn, Cu, Ni
!    
!    Stand: 06.04.2018
!    Autor: Volker Kirchesch
!##############################################


      logical kontroll !!wy
      integer jjj !!wy
      integer                              :: azStrs, anze
      integer, Dimension(1000)             :: flag, jiein
      real, Dimension(2)                   :: VTKoeff_Zn, VTKoeff_Cad, VTKoeff_Cu, VTKoeff_Ni
      real, Dimension(100)                 :: qeinl, eph, ess
      real, Dimension(1000)                :: vabfl, sedss, SSalg, vph, sedalk, sedalb, sedalg, bssalg_1, bph_1 
      real, Dimension(azStrs,100)          :: egsZn,eglZn, egsCad, eglCad, egsCu, eglCu, egsNi, eglNi
      real, Dimension(azStrs,1000)         :: hSSalg, hph, hglZn, hgsZn, hglCad, hgsCad, hglCu, hgsCu, hglNi, hgsNi   
      real, Dimension(azStrs,1000)         :: bssalg, bph

   open(unit=292,file='Schwermetalle.tst')

       iein = 1

      do j=1,anze+1                ! Schleife longitudinale Gitterpunkte
      ior = j 

      ior_flag = 0
      if(flag(ior)==6.and.vabfl(ior)<0.0.and.vabfl(ior+1)>0.0)then
        ior = ior+1
        ior_flag = 1
      endif
                                                                       
      if(ilbuhn==1)then
          else if(flag(ior)/=4)then
            else                        ! Berücksichtigung der Einleitungen
              m = 1
              ihcQ = 0
              if(vabfl(ior-1)<0.0.and.vabfl(ior)<0.0)m = -1
              if(vabfl(ior-1)<0.0.and.vabfl(ior)>0.0)ihcQ = 1 ! Konzentration an der Einleitstelle 
                                                              ! ist gleich der Konzentration der Einleitung 

              
              hcgsCad = hgsCad(mstr,ior-m)        ! Umbenennen der benötigten Variablen; 1D
              hcglCad = hglCad(mstr,ior-m) 
              hcgsZn = hgsZn(mstr,ior-m)
              hcglZn = hglZn(mstr,ior-m)
              hcgsCu = hgsCu(mstr,ior-m)
              hcglCu = hglCu(mstr,ior-m)
              hcgsNi = hgsNi(mstr,ior-m)
              hcglNi = hglNi(mstr,ior-m)

              hcQ = vabfl(ior-m)
              if(hcQ<0.0)hcQ = abs(hcQ)
              if(hcQ==0.0.or.ihcQ==1)hcQ = 1.e-10
              

              do ji=1,jiein(ior)   ! Beginn Einleitungsschleife  
              hcQE = max(0.0,qeinl(iein))

              hcgsCadE = egsCad(mstr,iein)
              hcglCadE = eglCad(mstr,iein)
              hcgsZnE = egsZn(mstr,iein)
              hcglZnE = eglZn(mstr,iein)
              hcgsCuE = egsCu(mstr,iein)
              hcglCuE = eglCu(mstr,iein)
              hcgsNiE = egsNi(mstr,iein)
              hcglNiE = eglNi(mstr,iein)
      
    do i = 1,1

      hcSS = min(100.,ess(iein))
      hcph = eph(iein)
      if(hcSS<0.0)hcss = min(100.,hSSalg(mstr,ior-m))
      if(hcph<0.0)hcph = hph(mstr,ior-m)

      call Verteilungskoeff(hcSS,hcph,VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni,iformVert,i)                                                                           

    enddo

              if(hcgsCadE<0.0.and.hcglCadE<0.0)then
                hcgsCadE = hcgsCad
                hcglCadE = hcglCad 
               endif

               if(hcgsCadE>0.0.and.hcglCadE<=0.0)then
                 hcglCadE = hcgsCadE/(1.+VTKoeff_Cad(1)*hcSS/1000.)                                     
                   else if(hcgsCadE<=0.0.and.hcglCadE>0.0)then
                    hcgsCadE = hcglCadE*(1+VTKoeff_Cad(1)*hcSS/1000.)
                      else if(hcgsCadE>0.0.and.hcglCadE==hcgsCadE)then
                        hcglCadE = hcgsCadE/(1.+VTKoeff_Cad(1)*hcSS/1000.)    
                          else if(hcgsCadE<hcglCadE)then
                            hcgsCadE = hcglCadE*(1+VTKoeff_Cad(1)*hcSS/1000.)
               endif     


              if(hcgsZnE<0.0.and.hcglZnE<0.0)then
                hcgsZnE = hcgsZn
                hcglZnE = hcglZn
              endif

               if(hcgsZnE>0.0.and.hcglZnE<=0.0)then
                 hcglZnE = hcgsZnE/(1.+VTKoeff_Zn(1)*hcSS/1000.)                                     
                   else if(hcgsZnE<=0.0.and.hcglZnE>0.0)then
                    hcgsZnE = hcglZnE*(1+VTKoeff_Zn(1)*hcSS/1000.)
                      else if(hcgsZnE>0.0.and.hcglZnE==hcgsZnE)then
                        hcglZnE = hcgsZnE/(1.+VTKoeff_Zn(1)*hcSS/1000.)    
                          else if(hcgsZnE<hcglZnE)then
                            hcgsZnE = hcglZnE*(1+VTKoeff_Zn(1)*hcSS/1000.)
               endif     

              if(hcgsCuE<0.0.and.hcglCuE<0.0)then
                hcgsCuE = hcgsCu
                hcglCuE = hcglCu
              endif

               if(hcgsCuE>0.0.and.hcglCuE<=0.0)then
                 hcglCuE = hcgsCuE/(1.+VTKoeff_Cu(1)*hcSS/1000.)                                     
                   else if(hcgsCuE<=0.0.and.hcglCuE>0.0)then
                    hcgsCuE = hcglCuE*(1+VTKoeff_Cu(1)*hcSS/1000.)
                      else if(hcgsCuE>0.0.and.hcglCuE==hcgsCuE)then
                        hcglCuE = hcgsCuE/(1.+VTKoeff_Cu(1)*hcSS/1000.)    
                          else if(hcgsCuE<hcglCuE)then
                            hcgsCuE = hcglCuE*(1+VTKoeff_Cu(1)*hcSS/1000.)
               endif     

              if(hcgsNiE<0.0.and.hcglNiE<0.0)then
                hcgsNiE = hcgsNi
                hcglNiE = hcglNi
              endif 

               if(hcgsNiE>0.0.and.hcglNiE<=0.0)then
                 hcglNiE = hcgsNiE/(1.+VTKoeff_Ni(1)*hcSS/1000.)                                     
                   else if(hcgsNiE<=0.0.and.hcglNiE>0.0)then
                    hcgsNiE = hcglNiE*(1+VTKoeff_Ni(1)*hcSS/1000.)
                      else if(hcgsNiE>0.0.and.hcglNiE==hcgsNiE)then
                        hcglNiE = hcgsNiE/(1.+VTKoeff_Ni(1)*hcSS/1000.)    
                          else if(hcgsNiE<hcglNiE)then
                            hcgsNiE = hcglNiE*(1+VTKoeff_Ni(1)*hcSS/1000.)
               endif     
             
             hgsCad(mstr,ior) = (hcQ*hcgsCad+hcQE*hcgsCadE)/(hcQ+hcQE) 
             hglCad(mstr,ior) = (hcQ*hcglCad+hcQE*hcglCadE)/(hcQ+hcQE) 
             hgsZn(mstr,ior) = (hcQ*hcgsZn+hcQE*hcgsZnE)/(hcQ+hcQE) 
             hglZn(mstr,ior) = (hcQ*hcglZn+hcQE*hcglZnE)/(hcQ+hcQE) 
             hgsCu(mstr,ior) = (hcQ*hcgsCu+hcQE*hcgsCuE)/(hcQ+hcQE) 
             hglCu(mstr,ior) = (hcQ*hcglCu+hcQE*hcglCuE)/(hcQ+hcQE) 
             hgsNi(mstr,ior) = (hcQ*hcgsNi+hcQE*hcgsNiE)/(hcQ+hcQE) 
             hglNi(mstr,ior) = (hcQ*hcglNi+hcQE*hcglNiE)/(hcQ+hcQE) 

             if(mstr==1)write(292,*)ior,hglZn(mstr,ior),hcQ,hcglZn,hcQE,hcglZnE,eglZn(mstr,iein) 


             hcQ = hcQ+qeinl(iein) 
             iein = iein+1 

             hcgsCad = hgsCad(mstr,ior)
             hcglCad = hglCad(mstr,ior)
             hcgsZn  = hgsZn(mstr,ior) 
             hcglZn  = hglZn(mstr,ior) 
             hcgsCu  = hgsCu(mstr,ior) 
             hcglCu  = hglCu(mstr,ior) 
             hcgsNi  = hgsNi(mstr,ior) 
             hcglNi  = hglNi(mstr,ior) 
                                                                         
           enddo                        ! Ende Einleitungsschleife

           if(ior_flag==1)then
             iein = iein - jiein(ior)
             ior = ior-1
             hgsCad(mstr,ior) = hgsCad(mstr,ior+1)
             hglCad(mstr,ior) = hglCad(mstr,ior+1)
             hgsZn(mstr,ior) = hgsZn(mstr,ior+1)
             hglZn(mstr,ior) = hglZn(mstr,ior+1)
             hgsCu(mstr,ior) = hgsCu(mstr,ior+1)
             hglCu(mstr,ior) = hglCu(mstr,ior+1)
             hgsNi(mstr,ior) = hgsNi(mstr,ior+1)
             hglNi(mstr,ior) = hglNi(mstr,ior+1)
           endif
    endif                               ! Ende Einleitungs-flag                                                                  
                                                                       
      if(ior>1)then 
        hglZn(mstr,ior-1) = hglZnt
        hgsZn(mstr,ior-1) = hgsZnt
        hglCad(mstr,ior-1) = hglCadt
        hgsCad(mstr,ior-1) = hgsCadt
        hglCu(mstr,ior-1) = hglCut
        hgsCu(mstr,ior-1) = hgsCut
        hglNi(mstr,ior-1) = hglNit
        hgsNi(mstr,ior-1) = hgsNit
      endif 


!########################################################################################
! Berechnung der Verteilungskoeffizienten VTKoeff_Zn, VTKoeff_Cu, VTKoeff_Cad, VTKoeff_Ni
! VTKoff.. in l/g
!########################################################################################

      if(ilbuhn==0)then
        hcSS = hSSalg(mstr,ior)/1000.
          else
            hcSS = bSSalg_1(ior)/1000.
      endif

      if(hglZn(mstr,ior)>0.0.and.hgsZn(mstr,ior)>0.0)then
        VTKoeffZn_vor = (hgsZn(mstr,ior)/hglZn(mstr,ior)-1.)/hcSS
          else 
            VTKoeffZn_vor  = -1.
      endif 

      if(hglCad(mstr,ior)>0.0.and.hgsCad(mstr,ior)>0.0)then
        VTKoeffCad_vor = (hgsCad(mstr,ior)/hglCad(mstr,ior)-1.)/hcSS
          else 
            VTKoeffCad_vor  = -1.
      endif 

      if(hglCu(mstr,ior)>0.0.and.hgsCu(mstr,ior)>0.0)then
        VTKoeffCu_vor = (hgsCu(mstr,ior)/hglCu(mstr,ior)-1.)/hcSS
          else 
            VTKoeffCu_vor  = -1.
      endif 

      if(hglNi(mstr,ior)>0.0.and.hgsNi(mstr,ior)>0.0)then
        VTKoeffNi_vor = (hgsNi(mstr,ior)/hglNi(mstr,ior)-1.)/hcSS
          else 
            VTKoeffNi_vor  = -1.
      endif 

     if(ilbuhn==0)then
       hcSS = min(100.,hSSalg(mstr,ior))
       hcph = max(4.,hph(mstr,ior))
         else
           hcSS = min(100.,bSSalg_1(ior))
           hcph = max(4.,bph_1(ior))
     endif

 
      do i=1,2
 
       call Verteilungskoeff(hcSS,hcph,VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni,iformVert,i)                                                                           
      
        
      if(ilbuhn==0)then
        hcSS = min(100.,SSalg(ior))          ! neue Schwebstoffkonzentration nach tflie
        hcph = max(4.,vph(ior))            ! neuer pH-Wert nach tflie 
          else
        hcSS = min(100.,bSSalg(mstr,ior))
        hcph = max(4.,bph(mstr,ior))
      endif 
            
     enddo

        if(VTKoeffZn_vor>0.0)then
          VTKoeffZn_neu = VTKoeffZn_vor*VTKoeff_Zn(2)/VTKoeff_Zn(1)
            else
              hgsZnt = -1
              hglZnt = -1.
        endif  

        if(VTKoeffCad_vor>0.0)then
          VTKoeffCad_neu = VTKoeffCad_vor*VTKoeff_Cad(2)/VTKoeff_Cad(1)
            else
              hgsCadt = -1
              hglCadt = -1.
        endif  

        if(VTKoeffCu_vor>0.0)then
          VTKoeffCu_neu = VTKoeffCu_vor*VTKoeff_Cu(2)/VTKoeff_Cu(1)
            else
              hgsCut = -1
              hglCut = -1.
        endif  

        if(VTKoeffNi_vor>0.0)then
          VTKoeffNi_neu = VTKoeffNi_vor*VTKoeff_Ni(2)/VTKoeff_Ni(1)
            else
              hgsNit = -1
              hglNit = -1.
        endif  

!+++++ Verringerung der Gesamt-Schwermetallkonz. durch Sedimentation +++++++

  if(ilbuhn==0)then
    hcSS = hSSalg(mstr,ior)
      else
        hcSS = bSSalg(mstr,ior)
   endif 
   
  if(hgsZn(mstr,ior)>0.0)then
    hgsZnt = hgsZn(mstr,ior)-(hgsZn(mstr,ior) - hglZn(mstr,ior))*((sedSS(ior)+sedalk(ior)+sedalb(ior)+sedalg(ior))/hcSS)
  endif

!   if(mstr==1)write(292,*)ior,hgsZn(mstr,ior),hglZn(mstr,ior),sedSS(ior),sedalk(ior),sedalb(ior),sedalg(ior),hcSS

  if(hgsCad(mstr,ior)>0.0)then
    hgsCadt = hgsCad(mstr,ior)-(hgsCad(mstr,ior) - hglCad(mstr,ior))*((sedSS(ior)+sedalk(ior)+sedalb(ior)+sedalg(ior))/hcSS)
  endif

  if(hgsCu(mstr,ior)>0.0)then
    hgsCut = hgsCu(mstr,ior)-(hgsCu(mstr,ior) - hglCu(mstr,ior))*((sedSS(ior)+sedalk(ior)+sedalb(ior)+sedalg(ior))/hcSS)
  endif

  if(hgsNi(mstr,ior)>0.0)then
    hgsNit = hgsNi(mstr,ior)-(hgsNi(mstr,ior) - hglNi(mstr,ior))*((sedSS(ior)+sedalk(ior)+sedalb(ior)+sedalg(ior))/hcSS)
  endif

! #############################################################
!  Neuberechnung des Gelösten Anteils nach dem Zeitschritt tflie 
! ##############################################################

  if(ilbuhn==0)then
     hcSS = SSalg(ior)
       else 
         hcSS = bSSalg(mstr,ior)
   endif

  if(hgsZn(mstr,ior)>0.0)then
     hglZnt = hgsZnt/(1.+VTKoeffZn_neu*hcSS/1000.)
  endif

  if(hgsCad(mstr,ior)>0.0)then
     hglCadt = hgsCadt/(1.+VTKoeffCad_neu*hcSS/1000.)
  endif

  if(hgsCu(mstr,ior)>0.0)then
     hglCut = hgsCut/(1.+VTKoeffCu_neu*hcSS/1000.)
  endif


  if(hgsNi(mstr,ior)>0.0)then
     hglNit = hgsNit/(1.+VTKoeffNi_neu*hcSS/1000.)
  endif

!     if(mstr==1)write(292,*)ior,hgsZnt,hglZnt,VTKoeffZn_neu,hcSS

    enddo     ! Ende Schleife longitudinale Gitterpunkte                             


     hgsZn(mstr,anze+1) = hgsZnt
     hgsCad(mstr,anze+1) = hgsCadt
     hgsCu(mstr,anze+1) = hgsCut
     hgsNi(mstr,anze+1) = hgsNit

     hglZn(mstr,anze+1) = hglZnt
     hglCad(mstr,anze+1) = hglCadt
     hglCu(mstr,anze+1) = hglCut
     hglNi(mstr,anze+1) = hglNit


  end subroutine Schwermetalle
