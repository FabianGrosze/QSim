!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

      SUBROUTINE erosion(anze,fkm,ss,ssalg,tflie,ischic,sedh,tausc,sedss,sedalk,sedalg,sedalb       &
                         ,hgsZn,hgsCad,hgsCu,hgsNi,tiefe,rau,vmitt,mstr,ilbuhn,ischwer,ilang,azStrs &
                         ,iwied,ianze_max                                                           &                                                   
                         ,kontroll ,jjj ) !!wy                                            
                                                                       
                                                                       
                                                                       
!     UNTERPROGRAMM ZUR Bestimmung der Erosionsrate                     
                                                                       
                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
                                                                       
!     STAND:08.02.01                                                    
                                                                       
!     sedroh   -  Rohdichte des Sediments [Kg*m-3]                      
                                                                       
      logical kontroll !!wy
      integer jjj !!wy
      integer                                   :: anze, azStrs 
      integer, Dimension(1000)                  :: ischic

      real, Dimension(1000)                     :: tiefe, fkm, ss, ssalg, sedh, sedss, sedalk, sedalg, sedalb
      real, Dimension(1000)                     :: vmitt, rau 

      real, Dimension(1000,100)                 :: tausc      !!wy

      real                                      :: m,n, JZn, JCad, JCu, JNi 
      real, Dimension(2)                        :: VTKoeff_Zn, VTKoeff_Cad, VTKoeff_Cu, VTKoeff_Ni
      real, allocatable, Dimension(:,:,:)       :: dsedH

      real, Dimension(azStrs,1000)              :: hgsZn, hgsCad, hgsCu, hgsNi 
      real, allocatable, Dimension(:,:)         :: ZnSed, bZnSed, CadSed, bCadSed, CuSed, bCuSed, NiSed, bNiSed 


      save dsedH, ZnSed, bZnSed, CadSed, bCadSed, CuSed, bCuSed, NiSed, bNiSed                                                                      

!      open(unit=111,file='erosion.tst')
                                
      js = 250                                                                                   
      if(.not.allocated(dsedH))allocate(dsedH(1:azStrs,1:ianze_max+1,1:js))
      if(.not.allocated(ZnSed))allocate(ZnSed(1:azStrs,1:ianze_max+1))
      if(.not.allocated(bZnSed))allocate(bZnSed(1:azStrs,1:ianze_max+1))
      if(.not.allocated(CadSed))allocate(CadSed(1:azStrs,1:ianze_max+1))
      if(.not.allocated(bCadSed))allocate(bCadSed(1:azStrs,1:ianze_max+1))
      if(.not.allocated(CuSed))allocate(CuSed(1:azStrs,1:ianze_max+1))
      if(.not.allocated(bCuSed))allocate(bCuSed(1:azStrs,1:ianze_max+1))
      if(.not.allocated(NiSed))allocate(NiSed(1:azStrs,1:ianze_max+1))
      if(.not.allocated(bNiSed))allocate(bNiSed(1:azStrs,1:ianze_max+1))


      sedroh = 1450. 
      roh2o = 1000. 
      g = 9.81 
!                                                                       
!...Sedimentparameter                                                   
!                                                                       
      tau0c = 1.25 
      tauunc = 10. 
      m = 7.5e-4 
      n = 3.2 
      fkomp = 1.43 
      sedhmx = 0.001 
                                                                      
      if(iwied==0)then
        do j1 = 1, azStrs
          do j2 = 1,1000 
            do j = 1,250
              dsedH(mstr,ior,j) = 0.0
            enddo
          enddo
        enddo
      endif 
       
                                                                       
      do ior = 1,anze+1 
                                                                       
      if(ischwer==1.and.ilang==0)then
        iformVert = 2
        i = 1
        hcSS = min(100.,SSalg(ior))

        call Verteilungskoeff(hcSS,hcph,VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni,iformVert,i)                                                                           
  
        ZnSed(mstr,ior) = (1.-(1./(1.+VTKoeff_Zn(i)*hcSS/1000.)))*max(0.0,hgsZn(mstr,ior))
        ZnSed(mstr,ior) = ZnSed(mstr,ior)/SSalg(ior)
        if(ilbuhn==1)bZnSed(mstr,ior) = ZnSed(mstr,ior)

        CadSed(mstr,ior) = (1.-(1./(1.+VTKoeff_Cad(i)*hcSS/1000.)))*max(0.0,hgsCad(mstr,ior))
        CadSed(mstr,ior) = CadSed(mstr,ior)/SSalg(ior)
        if(ilbuhn==1)bCadSed(mstr,ior) = CadSed(mstr,ior)

        CuSed(mstr,ior) = (1.-(1./(1.+VTKoeff_Cu(i)*hcSS/1000.)))*max(0.0,hgsCu(mstr,ior))
        CuSed(mstr,ior) = CuSed(mstr,ior)/SSalg(ior)
        if(ilbuhn==1)bCuSed(mstr,ior) = CuSed(mstr,ior)

        NiSed(mstr,ior) = (1.-(1./(1.+VTKoeff_Ni(i)*hcSS/1000.)))*max(0.0,hgsNi(mstr,ior))
        NiSed(mstr,ior) = NiSed(mstr,ior)/SSalg(ior)
        if(ilbuhn==1)bNiSed(mstr,ior) = NiSed(mstr,ior)

      endif      

      Srate = ((sedss(ior)+sedalk(ior)+sedalg(ior)+sedalb(ior))/1000.)*tiefe(ior)                                                
      ddsedh = Srate/sedroh 
      sedh(ior) = sedh(ior)+ddsedh 
                                                                       
! #### Aufteilen des Sediments in einzelne Schichten ####                     
                                                                       
      ischic(ior) = int(sedh(ior)/sedhmx)
      hcon = (sedh(ior)/sedhmx) - int(sedh(ior)/sedhmx)

      if(hcon>0.0)ischic(ior) = ischic(ior) + 1
      
        do j = ischic(ior),1,-1
          if(j==ischic(ior))then
            if(hcon>0.0)then
              dsedH(mstr,ior,j) = sedhmx * hcon
                else
                  dsedH(mstr,ior,j) = sedhmx
            endif

            if(tausc(ior,j)==0.0)tausc(ior,j) = tau0c
            cycle
          endif
            if(dsedH(mstr,ior,j)==0.0)dsedH(mstr,ior,j) = sedhmx
            hctausc = tau0c+((tauunc-tau0c)*(1.-exp(-fkomp*sedHmx*(ischic(ior)-j)))) 
            if(tausc(ior,j)==0.0.or.tausc(ior,j)<hctausc)tausc(ior,j) = hctausc
         enddo
          
! #### Berechnung der Sohlschubspannung ####
 
     FN = 1./RAU(ior) 
      G = 9.81 
      UST = ((FN*G**0.5)/TIEFE(ior)**0.166667)*abs(VMITT(ior)) 
                                                                       
      taus = (ust**2)*roh2o 
! #############################################################

      SdReros = 0.0
      j_minus = 0

      do j = ischic(ior),1,-1 
        if(taus<tausc(ior,j))exit
        dReros = M*((taus-tausc(ior,j))/tausc(ior,j))**n                                        
        dReros = dReros*tflie*86400. 
        dsedhe = dReros/sedroh
        dsedH(mstr,ior,j) = dsedH(mstr,ior,j) - dsedhe
        if(dsedH(mstr,ior,j)<=0.0)then
          dsedH(mstr,ior,j) = 0.0
          dReros = dsedH(mstr,ior,j)*sedroh
          j_minus = j_minus + 1 
         endif
         SdReros =  SdReros + dReros
         if(dsedH(mstr,ior,j)>0.0)exit
      enddo

     ischic(ior) = ischic(ior) - j_minus
      
      sedh(ior) = 0.0
      do j = ischic(ior),1,-1
        sedh(ior) = sedh(ior) + dsedH(mstr,ior,j)
      enddo 
                                                                     
      SSeros = (SdReros/tiefe(ior))*1000. 
      SS(ior) = SS(ior)+SSeros 
      SSalg(ior) = SSalg(ior)+SSeros

    if(ischwer==1)then   
      if(ilbuhn==0)then
        JZn = SSeros*ZnSed(mstr,ior)
        JCad = SSeros*CadSed(mstr,ior)
        JCu = SSeros*CuSed(mstr,ior)
        JNi = SSeros*NiSed(mstr,ior)
          else
            JZn = SSeros*bZnSed(mstr,ior)
            JCad = SSeros*bCadSed(mstr,ior)
            JCu = SSeros*bCuSed(mstr,ior)
            JNi = SSeros*bNiSed(mstr,ior)
      endif 
      hgsZn(mstr,ior) = hgsZn(mstr,ior) + JZn
      hgsCad(mstr,ior) = hgsCad(mstr,ior) + JCad
      hgsCu(mstr,ior) = hgsCu(mstr,ior) + JCu
      hgsNi(mstr,ior) = hgsNi(mstr,ior) + JNi
    endif
                                                                       
    enddo 
                                                                     
                                                                       
  END subroutine erosion                                           
