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

      subroutine trimat_z(a,b,c,dT,doo,dN4,dN2,dN3,dP,dSi,dKi,dGr,dBl,dChl, dCChlk,dCChlb,dCChlg                   &            
                         ,dchlk,dchlg,dchlb,dgesP,dgesN,dQ_NKz,dQ_NBz,dQ_NGz,tempwz,vo2z,vNH4z,vNO2z,vNO3z         &
                         ,gelPz,Siz,akiz,agrz,ablz,chlaz,hchlkz,hchlgz,hchlbz,hgesPz,hgesNz,hQ_NKz,hQ_NBz,hQ_NGz   &
                         ,hCChlkz,hCChlbz,hCChlgz,Qmx_NK,Qmx_NB,Qmx_NG,nkzs,mstr,ior,azStrs)                         
                                                                       
                                                                       
!     Ein Programm zur Berechnung einer tridiagonal-Matrix              
                                                                       
!     Koeffizienten: a,b,c                                              
                                                                       
                                                                       
!     AUTOR :VOLKER KIRCHESCH                                           
                                                                       
!     STAND :23.01.1994                                                 
                                                                       
                                                                       
      integer                           :: azStrs
      integer, Dimension(1000)          :: nkzs
      real, Dimension(50)               :: a, b, c, dT, l, doo, dN4, dN2, dN3, dP, dSi, dKi, dGr, dBl, dChl
      real, Dimension(50)               :: dchlk, dchlg, dchlb, dgesP, dgesN, m, yT, yO, yN4, yN2, yN3, yP, ySi, yKi
      real, Dimension(50)               :: dCChlk, dCChlb, dCChlg, dQ_NKz, dQ_NBz,dQ_NGz,yQ_NK,yQ_NB,yQ_NG 
      real, Dimension(50)               :: yGr, yBl, yChl, ychlk, ychlg, ychlb, ygesP, ygesN, yCChlk, yCChlb, yCChlg
      real, Dimension(50,1000)          :: tempwz, vo2z, vNH4z, vNO2z, vNO3z, gelPz 
      real, Dimension(50,1000)          :: Siz, akiz, agrz, ablz, chlaz
      real, Dimension(azStrs,50,1000)   :: hchlkz, hchlgz, hchlbz, hgesPz, hgesNz, hQ_NKz, hQ_NBz, hQ_NGz
      real, Dimension(azStrs,50,1000)   :: hCChlkz, hCChlbz, hCChlgz

                                                                       
!      open(unit=39,file='trimat.tst')                                  
                                                                       
    m(1) = a(1) 
    do nkz=1,nkzs(ior)-1 
      l(nkz) = c(nkz)/m(nkz) 
      m(nkz+1) = a(nkz+1)-l(nkz)*b(nkz) 
    enddo 
                                                                       
      yT(1) = dT(1) 
      yO(1) = doo(1) 
      yN4(1) = dN4(1) 
      yN2(1) = dN2(1) 
      yN3(1) = dN3(1) 
      yP(1) = dP(1) 
      ySi(1) = dSi(1) 
      yKi(1) = dKi(1) 
      yGr(1) = dGr(1) 
      yBl(1) = dBl(1) 
      yChl(1) = dChl(1) 
      yChlk(1) = dChlk(1) 
      yChlg(1) = dChlg(1) 
      yChlb(1) = dChlb(1) 
      ygesP(1) = dgesP(1) 
      ygesN(1) = dgesN(1) 
      yQ_NK(1) = dQ_NKz(1) 
      yQ_NB(1) = dQ_NBz(1) 
      yQ_NG(1) = dQ_NGz(1) 
      yCChlk(1) = dCChlk(1) 
      yCChlb(1) = dCChlb(1) 
      yCChlg(1) = dCChlg(1) 
                                                                       
    do nkz = 2,nkzs(ior) 
      yT(nkz) = dT(nkz)-l(nkz-1)*yT(nkz-1) 
      yO(nkz) = doo(nkz)-l(nkz-1)*yO(nkz-1) 
      yN4(nkz) = dN4(nkz)-l(nkz-1)*yN4(nkz-1) 
      yN2(nkz) = dN2(nkz)-l(nkz-1)*yN2(nkz-1) 
      yN3(nkz) = dN3(nkz)-l(nkz-1)*yN3(nkz-1) 
      yP(nkz) = dP(nkz)-l(nkz-1)*yP(nkz-1) 
      ySi(nkz) = dSi(nkz)-l(nkz-1)*ySi(nkz-1) 
      yKi(nkz) = dKi(nkz)-l(nkz-1)*yKi(nkz-1) 
      yGr(nkz) = dGr(nkz)-l(nkz-1)*yGr(nkz-1) 
      yBl(nkz) = dBl(nkz)-l(nkz-1)*yBl(nkz-1) 
      yChl(nkz) = dChl(nkz)-l(nkz-1)*yChl(nkz-1) 
      yChlk(nkz) = dChlk(nkz)-l(nkz-1)*yChlk(nkz-1) 
      yChlg(nkz) = dChlg(nkz)-l(nkz-1)*yChlg(nkz-1) 
      yChlb(nkz) = dChlb(nkz)-l(nkz-1)*yChlb(nkz-1) 
      ygesP(nkz) = dgesP(nkz)-l(nkz-1)*ygesP(nkz-1) 
      ygesN(nkz) = dgesN(nkz)-l(nkz-1)*ygesN(nkz-1) 
      yQ_NK(nkz) = dQ_NKz(nkz)-l(nkz-1)*yQ_NK(nkz-1) 
      yQ_NB(nkz) = dQ_NBz(nkz)-l(nkz-1)*yQ_NB(nkz-1) 
      yQ_NG(nkz) = dQ_NGz(nkz)-l(nkz-1)*yQ_NG(nkz-1) 
      yCChlk(nkz) = dCChlk(nkz)-l(nkz-1)*yCChlk(nkz-1) 
      yCChlb(nkz) = dCChlb(nkz)-l(nkz-1)*yCChlb(nkz-1) 
      yCChlg(nkz) = dCChlg(nkz)-l(nkz-1)*yCChlg(nkz-1) 
    enddo 
                                                                       
      tempwz(nkzs(ior),ior) = yT(nkzs(ior))/m(nkzs(ior)) 
      vo2z(nkzs(ior),ior) = yO(nkzs(ior))/m(nkzs(ior)) 
      vNH4z(nkzs(ior),ior) = yN4(nkzs(ior))/m(nkzs(ior)) 
      vNO2z(nkzs(ior),ior) = yN2(nkzs(ior))/m(nkzs(ior)) 
      vNO3z(nkzs(ior),ior) = yN3(nkzs(ior))/m(nkzs(ior)) 
      gelPz(nkzs(ior),ior) = yP(nkzs(ior))/m(nkzs(ior)) 
      Siz(nkzs(ior),ior) = ySi(nkzs(ior))/m(nkzs(ior)) 
      akiz(nkzs(ior),ior) = yKi(nkzs(ior))/m(nkzs(ior)) 
      agrz(nkzs(ior),ior) = yGr(nkzs(ior))/m(nkzs(ior)) 
      ablz(nkzs(ior),ior) = yBl(nkzs(ior))/m(nkzs(ior)) 
      chlaz(nkzs(ior),ior) = yChl(nkzs(ior))/m(nkzs(ior)) 
      hchlkz(mstr,nkzs(ior),ior) = yChlk(nkzs(ior))/m(nkzs(ior)) 
      hchlgz(mstr,nkzs(ior),ior) = yChlg(nkzs(ior))/m(nkzs(ior)) 
      hchlbz(mstr,nkzs(ior),ior) = yChlb(nkzs(ior))/m(nkzs(ior)) 
      hgesPz(mstr,nkzs(ior),ior) = ygesP(nkzs(ior))/m(nkzs(ior)) 
      hgesNz(mstr,nkzs(ior),ior) = ygesN(nkzs(ior))/m(nkzs(ior)) 
      hQ_NKz(mstr,nkzs(ior),ior) = min(Qmx_NK,(yQ_NK(nkzs(ior))/m(nkzs(ior)))) 
      hQ_NBz(mstr,nkzs(ior),ior) = min(Qmx_NB,(yQ_NB(nkzs(ior))/m(nkzs(ior)))) 
      hQ_NGz(mstr,nkzs(ior),ior) = min(Qmx_NG,(yQ_NG(nkzs(ior))/m(nkzs(ior)))) 
      hCChlkz(mstr,nkzs(ior),ior) = yCChlk(nkzs(ior))/m(nkzs(ior)) 
      hCChlbz(mstr,nkzs(ior),ior) = yCChlb(nkzs(ior))/m(nkzs(ior)) 
      hCChlgz(mstr,nkzs(ior),ior) = yCChlg(nkzs(ior))/m(nkzs(ior)) 
 
    do nkz = nkzs(ior)-1,1,-1 
      tempwz(nkz,ior) = (yT(nkz)-b(nkz)*tempwz(nkz+1,ior))/m(nkz) 
      vo2z(nkz,ior) = (yO(nkz)-b(nkz)*vo2z(nkz+1,ior))/m(nkz) 
      vNH4z(nkz,ior) = (yN4(nkz)-b(nkz)*vNH4z(nkz+1,ior))/m(nkz) 
      vNO2z(nkz,ior) = (yN2(nkz)-b(nkz)*vNO2z(nkz+1,ior))/m(nkz) 
      vNO3z(nkz,ior) = (yN3(nkz)-b(nkz)*vNO3z(nkz+1,ior))/m(nkz) 
      gelPz(nkz,ior) = (yP(nkz)-b(nkz)*gelPz(nkz+1,ior))/m(nkz) 
      Siz(nkz,ior) = (ySi(nkz)-b(nkz)*Siz(nkz+1,ior))/m(nkz) 
      akiz(nkz,ior) = (yKi(nkz)-b(nkz)*akiz(nkz+1,ior))/m(nkz) 
      agrz(nkz,ior) = (yGr(nkz)-b(nkz)*agrz(nkz+1,ior))/m(nkz) 
      ablz(nkz,ior) = (yBl(nkz)-b(nkz)*ablz(nkz+1,ior))/m(nkz) 
      chlaz(nkz,ior) = (yChl(nkz)-b(nkz)*chlaz(nkz+1,ior))/m(nkz) 
      hchlkz(mstr,nkz,ior) = (yChlk(nkz)-b(nkz)*hchlkz(mstr,nkz+1,ior))/m(nkz) 
      hchlgz(mstr,nkz,ior) = (yChlg(nkz)-b(nkz)*hchlgz(mstr,nkz+1,ior))/m(nkz) 
      hchlbz(mstr,nkz,ior) = (yChlb(nkz)-b(nkz)*hchlbz(mstr,nkz+1,ior))/m(nkz) 
      hgesPz(mstr,nkz,ior) = (ygesP(nkz)-b(nkz)*hgesPz(mstr,nkz+1,ior))/m(nkz) 
      hgesNz(mstr,nkz,ior) = (ygesN(nkz)-b(nkz)*hgesNz(mstr,nkz+1,ior))/m(nkz) 
      hQ_NKz(mstr,nkz,ior) = min(Qmx_NK,((yQ_NK(nkz)-b(nkz)*hQ_NKz(mstr,nkz+1,ior))/m(nkz))) 
      hQ_NBz(mstr,nkz,ior) = min(Qmx_NB,((yQ_NB(nkz)-b(nkz)*hQ_NBz(mstr,nkz+1,ior))/m(nkz))) 
      hQ_NGz(mstr,nkz,ior) = min(Qmx_NG,((yQ_NG(nkz)-b(nkz)*hQ_NGz(mstr,nkz+1,ior))/m(nkz))) 
      hCChlkz(mstr,nkz,ior) = (yCChlk(nkz)-b(nkz)*hCChlkz(mstr,nkz+1,ior))/m(nkz) 
      hCChlbz(mstr,nkz,ior) = (yCChlb(nkz)-b(nkz)*hCChlbz(mstr,nkz+1,ior))/m(nkz) 
      hCChlgz(mstr,nkz,ior) = (yCChlg(nkz)-b(nkz)*hCChlgz(mstr,nkz+1,ior))/m(nkz) 
   enddo 
                                                                       
    END Subroutine trimat_z                                           
