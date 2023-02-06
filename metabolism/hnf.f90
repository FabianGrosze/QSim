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

!> Berechnung des Einflusses der heterotrophen Nanoflagelaten
!! @author Volker Kirchesch
!! @date 15.06.2001
subroutine HNF(CHNF,BVHNF,BAC,TEMPW,VO2,TFLIE                                    &
               ,echnf,eBVHNF,flag,elen,ior,anze,qeinl,vabfl                      &
               ,jiein,drHNF,zHNF,HNFBAC,rO2HNF,BSBHNF,HNFmua,upHNFe,BACkse       &
               ,HNFrea,HNFupa,HNFmoa,HNFexa,fkm,mstr,itags,monats,uhrz           &
               ,kontroll,jjj)
   
   implicit none
   
   integer :: mstr, monats, ji, itags, ior, jjj, iein
   real    :: upmhnf, uphnf, uphnfe, uhrz, tflie
   real    :: sqeinl, sechnf, sebvhn, saett, rghnf
   real    :: reshnf, q10, hnfex, hnfass, hcq
   real    :: hcohnf, hcbhnf, ftemp, fo2, exhnf
   real    :: delhnf, chnft, backse
   logical :: kontroll
   real tempw(1000),vo2(1000),BAC(1000)
   real elen(1000),vabfl(1000),qeinl(100)
   real CHNF(1000),BVHNF(1000),eCHNF(100),eBVHNF(100)
   real ksBAC,mTHNF,mOHNF,morHNF,mueHNF
   real drHNF(1000),zHNF(1000),HNFBAC(1000),rO2HNF(1000)
   real BSBHNF(1000),fkm(1000)
   real HNFmua(1000),HNFrea(1000),HNFupa(1000),HNFmoa(1000)
   real HNFexa(1000)
   integer flag(1000),anze,jiein(1000)
   !
   open(unit = 579,file = 'HNF.tst')
   !
   iein = 1
   
   ! Parametervorgabe
   upmHNF = upHNFe
   ksBAC = BACkse
   rGHNF = 0.05
   HNFass = 0.33
   HNFex = 0.5
   mTHNF = 0.1
   mOHNF = 0.25
   
   do ior = 1,anze+1
      if (flag(ior) == 8)goto 107
      
      ! Einleitungen
      206 if (flag(ior) /= 4)goto 107
      hcohnf = CHNF(ior-1)
      hcbhnf = BVHNF(ior-1)
      hcQ = vabfl(ior-1)
      if (hcQ < 0.0)hcQ = 0.0
      
      do ji = 1,jiein(ior)
         if (echnf(iein) <= 0.0) then
            sechnf = hcohnf
            seBVHN = hcbhnf
         endif
         
         sechnf = echnf(iein)
         
         if (eBVHNF(iein) <= 0.0) then
            seBVHN = hcbhnf
            goto 102
         endif
         
         seBVHN = eBVHNF(iein)
         
         102 continue
         sqeinl = qeinl(iein)
         
         if (sqeinl < 0.0)sqeinl = 0.0
         
         if (hcQ == 0.0 .and. sqeinl == 0.0) then
            chnf(ior) = hcohnf
            BVHNF(ior) = hcbhnf
            goto 115
         endif
         chnf(ior) = (hcQ*hcohnf+sechnf*sqeinl)/(hcQ+sqeinl)
         BVHNF(ior) = (hcQ*hcbhnf+seBVHN*sqeinl)/(hcQ+sqeinl)
         
         115 continue
         hcQ = hcQ+qeinl(iein)
         iein = iein+1
         hcohnf = chnf(ior)
         hcbhnf = BVHNF(ior)
      enddo
      
      
      107 continue
      
      if (ior > 1)CHNF(ior-1) = CHNFt
      
      ! TEMPERATURABHAENGIGKEIT (evt.neu!!)
      q10 = 2.
      ftemp = q10**((tempw(ior)-20.)*0.1)
      
      ! HNF-Wachstum
      ! spz. Aufnahmerate
      upHNF = upmHNF*(BAC(ior)/(BAC(ior)+ksBAC))
      upHNF = upHNF*ftemp
      ! Respirationsrate
      resHNF = rGHNF*ftemp+upHNF*(1.-HNFass)*(1.-HNFex)
      ! Excretion
      exHNF = upHNF*(1.-HNFass)*HNFex
      ! Mortalitt
      SAETT = 14.603-TEMPW(ior)*0.40215+(TEMPW(ior)**2)*0.007687        &
              -(tempw(ior)**3)*0.0000693
      fO2 = vo2(ior)/Saett
      if (fO2 > 1.)fO2 = 1.
      !..wieder lschen!!!!
      fo2 = 1.
      morHNF = (1.-fO2)*mOHNF+mTHNF*ftemp
      ! Wachstumsrate
      mueHNF = upHNF-resHNF-exHNF-morHNF
      ! Ausgabe
      HNFmua(ior) = mueHNF
      HNFrea(ior) = resHNF
      HNFupa(ior) = upHNF
      HNFmoa(ior) = morHNF
      HNFexa(ior) = exHNF
      
      CHNFt = CHNF(ior)*exp(mueHNF*tflie)
      
      ! Bercksichtigung von Rotatorien- und Dreissena-Grazing
      CHNFt = CHNFt-drHNF(ior)-zHNF(ior)
      if (CHNFt < 0.0)CHNFt = 0.0000000001
      if (mstr == 14)write(579,*)ior,CHNFt,CHNF(ior),mueHNF,zHNF(ior)
      
      ! Verlustrate der Bakterien
      HNFBAC(ior) = upHNF*CHNF(ior)*tflie
      
      ! Sauerstoffverbrauch durch HNF-Respiration
      ! Annahme: bei der Respiration von 1 mg Biomasse (ausgedrckt
      !          als mg C) werden 3.2 mg O2 verbraucht
      rO2HNF(ior) = resHNF*CHNF(ior)*tflie
      
      ! BSB-Erhhung durch abgestorbene HNF bzw. excretierte Nahrung
      BSBHNF(ior) = (morHNF+exHNF)*CHNF(ior)
      
      delHNF = CHNFt-CHNF(ior)
      if (CHNFt < 0.0) CHNFt = (CHNF(ior)/(CHNF(ior)+abs(delHNF)))*CHNF(ior)
   enddo
   
   CHNF(anze+1) = CHNFt
   
   return
end
