Stofftransport QSim1D - Umsetzung {#lnk_transport_1d_umsetzung}
================================= 

## Herkunft 
Programm zur Berechnung des Stofftransports

Autor: Volker Kirchesch, Jens Wyrwa

Datum: 07.01.2014 bzw. 25.07.2019

## Schnittstellenbeschreibung
    call Transport(anze,deltat,izeits,isub_dt,isub_dt_Mac,hvmitt,elen,flag,tempw,
	  vo2,vnh4,vno3,vno2,vx0                  &
                     ,vx02,Si,mstr,gelP,obsb,ocsb,vbsb,vcsb,CHNF,BVHNF,CD,CP,CM,BAC,zooind,chla,aki,agr,abl,chlaki,chlagr  &
                     ,chlabl,vkigr,antbl,abrzo1,ssalg,ss,svhemk,svhemg,svhemb,akbcm,agbcm,abbcm,fssgr,fbsgr,frfgr,gesN     &
                     ,gesP,nl0,pl0,Q_NK,Q_PK,Q_SK,Q_NG,Q_PG,Q_NB,Q_PB,stind,mw,pw,ca,lf,coli,DOSCF                         &
                     ,dlarvn,vph,iph,iwsim,htempw,hgesN,hgesP,hbsb,hcsb,hCHNF,hBVHNF,hCD,hCP,hCM,hBAC,hnh4,ho2             &
                     ,hno3,hno2,hx0,hx02,hsi,hchla,haki,hagr,habl,hchlak,hchlag,hchlab,hvkigr,hantbl,hssalg,hss,hzooi      &
                     ,hgelp,hmw,hpw,hca,hlf,hph,hdlarn,hcoli,hDOSCF,hvbsb,hvcsb,SKmor,hSKmor,iflRi,dl,Uvert,iMAC           &
                     ,iwied,nkzs,tflie,jpoin1,itags,monats,Uhrz,iverfahren,ianze_max,Qmx_NK,Qmx_NB,Qmx_NG,Qmx_PK           &
                     ,Qmx_PB,Qmx_PG,hFluN3,TGZoo,akmor_1,agmor_1,abmor_1                                                   &
                     ,hgsZn,hglZn,hgsCad,hglCad,hgsCu,hglCu,hgsNi,hglNi,hgsAs,hglAs,hgsPb,hglPb,hgsCr,hglCr,hgsFe,hglFe    &
                     ,hgsHg,hglHg,hgsMn,hglMn,hgsU,hglU,mtracer,nkztot_max,ischwer)

\n\n

    call transportz(anze, deltat, izeits, isub_dt, isub_dt_Mac, dtmin_Mac,  &
                      hvmitt, elen, flag, tempwz, vnh4z, vno2z, vno3z, vo2z,  &
                      gelPz, Siz, akiz, agrz, ablz, chlaz, hgesPz, hgesNz,    &
                      nkzs, dH2D, i2Ds, iwsim, mstr, tempz, ho2z, hnh4z,      &
                      hno2z, hno3z, hgelPz, hSiz, hQ_NKz, hQ_NBz, hQ_NGz,     &
                      hakiz, hagrz, hablz, hchlaz, hchlkz, hchlgz, hchlbz,    &
                      hCChlkz, hCChlbz, hCChlgz, iflRi, dl, iMAC, Uvert,      &
                      tflie, jpoin1, itags, monats, iwied, uhrz, iverfahren,  &
                      azStrs, ianze_max, nkztot_max, Qmx_NK, Qmx_NB, Qmx_NG,  &
                      mtracer)
\n\n

## IT-Realisierung
...

\n\n
					  
Textquelle: stofftransport_1d_umsetzung.md; Codesource: transport.f90 und 
transportz.f90; 
zurück \ref lnk_stofftransport_1d
