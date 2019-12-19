  Program QSIM     
                                                                        
                                                                       
                                                                       
!      izdt Einheiten min oder Stunden Beruecksichtigung bei itime      
!      Bei Tracerrechnung wird für die Variable tempw mit der Tracermenge belegt!!!
!.....testen: WEHRO2                                                    

      character                               :: ckenn,cpoint,CST_end 
      character (len = 2)                     :: cerrts,ctest1,chcon,ckenn_vers,ckenn_vers1
      character (len = 1)                     :: ctaste  
      character (len = 7)                     :: cmin,cmax
      character (len = 40)                    :: ERENAME, MODNAME 
      character (len = 255)                   :: Modell,cEreig 
      character (len = 200)                   :: ctext 
      character (len = 255)                   :: cpfad,cpfad1,cpfad2,filestring
      character (len=275)                     :: pfadstring
      logical kontroll, einmalig, linux !!wy
      integer iglob, open_error !!wy
      character (len = 120)                   :: cfehlr 
      
      character (len = 50), Dimension(50,100) :: CEname  

      integer                                 :: maus, read_error, anze, azstr, azstrs, anzej, Stunde,STRiz_2D, anzema  
      integer                                 :: tdj, SCHRNR, zabfr, RBNR
      integer, Dimension(2)                   :: ikanz
      integer, Dimension(20)                  :: iWSta, mwetts
      integer, Dimension(100)                 :: typ, iorLa, iorle, mWO2
      integer, Dimension(1000)                :: flag, jiein, ischif, ischic, zwnkzs, nkzsy, nkzs, hnkzsz

      integer, Dimension(:), allocatable      :: hanze,ianze, STRiz,isub_dt,imac,isub_dt_Mac, mstr_ist, strNr, mstra 
      integer, Dimension(:), allocatable      :: ieinsh, ieinLs, nbuhn, iFlRi, isegs, STRID, janzWt, janzWs, jlwo2, iRB_K1, izufluss
      integer, Dimension(:), allocatable      :: imRB_K1, mPfs, mSs, mDs, mCs, mBs, mUs, i2Ds, mWes, mVs, mZs, mAs
      integer, Dimension(:), allocatable      :: itsts, msts, itmaxs, mmaxs, itends, mends, laits, laims, laids, mStas
      integer, Dimension(:), allocatable      ::  abfr, mwehr, mRBs, nstrs, nnstrs, iFlRi_l
       
      integer, Dimension(:,:), allocatable    :: it_h, it_hy, iorLah, iorLeh, typh, ischig, ikWSta, idWe, mstrLe, istund
      integer, Dimension(:,:), allocatable    :: RBtyp, Weinl, NRSchr, hnkzs, nkzmx, znkzs, inkzs, ibschi       
      integer, Dimension(:,:), allocatable    :: hflag, hjiein, hischf, ESTRNR 

      real                                    :: N4end, N2end, N3end, Kiend, Klange, KdNh3e, lat_k, nzoo,mues,lgh
      real                                    :: irmaxe, nbiogr
      real                                    :: KsD1e, KsD2e, KsMe, IKge, IKke, IKbe, KapN3e, KNH4e, kTemp_Gr
      real                                    :: kTemp_Ki, kTemp_Bl, mikonsS, mxkonsS

      real, Dimension(2)                      :: xCD, xCP, xdrakr, xdrbar, xdrmor, xidras, xdrmas   
      real, Dimension(4)                      :: gwdre, zdreie, zdrese, xdrbio, xdbios, xgewdr
      real, Dimension(20)                     :: glob, tlmax, tlmin, cloud, typw, ro, wge 
      real, Dimension(24)                     :: astand

      real, Dimension(50)                     :: hcs67, hcs68, hcs69, hcs70, hcs71, hcs72, hcs73, hcs74, hcs75, hcs76
      real, Dimension(50)                     :: hcs84, hcs87, hcs88, hcs89, hcs90, hcs91, hcs92, hcs93, hcs94, dvdz, xU 
      real, Dimension(50)                     :: hcs96, hcs97, hcs98, akiz_vor, akiz_vor1
      real, Dimension(50)                     :: hc212, hc262, hc32, hc42, hc52, hc62, hc92, hc102, hc112, hc122, hc222

      real, Dimension(100)                    :: einlk, qeinl, ebsb, ecsb, enh4, ex0, eo2, etemp, echla, ep
      real, Dimension(100)                    :: elf, eph, emw, eca, ex02, eno2, ess, ewaerm, esi, ezind, eno3
      real, Dimension(100)                    :: eCHNF, eBVHNF, egesN, egesP, ecoli, evkigr,eantbl, enl0, epl0
      real, Dimension(100)                    :: qeinlL, bsbL, csbL, enh4L, x0L, x02L, o2L, etempL, gpL, siL 
      real, Dimension(100)                    :: eno2L, eno3L, gesNL, gesPL, ssL, phL, elfL, caL, coliL, enl0L  
      real, Dimension(100)                    :: pl0L, chlaL 
      real, Dimension(1000)                   :: elen, vmitt, tiefe, flae, breite, rau, rhyd, vabfl, stind, nl0, pl0
      real, Dimension(1000)                   :: Q_NK, Q_PK, Q_SK, Q_NG, Q_PG, Q_NB, Q_PB, akmuea, ftaaus, fiaus 
      real, Dimension(1000)                   :: fheaus, fhegas, fhegy, agmuea, agmuey, akraus, rmuas, rmuasy, agreau
      real, Dimension(1000)                   :: agrey, rakr, rakry, figaus, figy, rbar, rbary, dorgSS, HNFmua, BACmua
      real, Dimension(1000)                   :: HNFmuy, BACmuy, HNFBAy,HNFrey, HNFupy, HNFmoy, HNFexy, HNFdry, HNFzy
      real, Dimension(1000)                   :: HNFrea, HNFupa, HNFmoa, HNFexa, HNFdra, HNFza, akmuey, ftay, fiy, fhey
      real, Dimension(1000)                   :: akry, sgefpm, sedh, dl, resdr, exdrvg, exdrvk, dlarvd, dlarvR, dlarvn
      real, Dimension(1000)                   :: dlarny, pflmin, pflmax, po2p, po2r, pfl, VALTBL, EDUFBL, VALTBR, EDUFBR
      real, Dimension(1000)                   :: drpfey, drpfec, ssdr, drfaek, drfaeg, drfaes, volfdr, Tsed, tempw, zexki
      real, Dimension(1000)                   :: templ, zexgr, dzres1, dzres2, obsb, vcsb, vbsb, CM, BAC, ocsb, vnh4, vno3
      real, Dimension(1000)                   :: vno2, si, chla, ssalg, zooind, gelp, vco2, aki, agr, ro2dr, zooro2, akitbr
      real, Dimension(1000)                   :: agrtbr, dalggr, dalgki, dalgag, dalgak, albewg, alberg, albewk, alberk
      real, Dimension(1000)                   :: vx0, go2n, vo2, sgo2n, vx02, gesN, gesP, sdbsb, abszo, bsbt, bsbct, bsbctP
      real, Dimension(1000)                   :: dlmax, dlmaxs, tracer, svhemk, svhemg, DOSCF, extk, SiRuek, svkh1, sised 
      real, Dimension(1000)                   :: SKmor, schwi, Dz2D, dC_DenW, fkm, dO2o2D, salgo 
      real, Dimension(1000)                   :: CHNF, HNFBAC, BSBHNF, drHNF, BVHNF, coli, zHNF, zBAC, rO2HNF, tpki, tpgr
      real, Dimension(1000)                   :: abl, antbl, abbcm, abltbr, svhemb, nbiobl, dblmor, tpbl, dalgbl, dalgab  
      real, Dimension(1000)                   :: sedalb, algzob, sedalb0, fibaus, abmuea, fhebas, abreau, algdrb, algcob    
      real, Dimension(1000)                   :: chlabl, exdrvb, zexbl, ablnh4, ablno3, drfaeb  

      real, Dimension(1000)                   :: ably, abln4y, sedaby, algzby, algdby, algcby, dalgby, dalaby, dbmory  
      real, Dimension(1000)                   :: abmuey, fiby, fheby, abrey, antbly, tpbly
  
      real, Dimension(1000)                   :: tau2, hctau1, hctau2, zwTsed, zwtemp, zwvm, zwtief,zwextk
      real, Dimension(1000)                   :: zwno3, zwnh4, zwgelp, zwsvhk, zwchla, zwir, zwssa, zwsi, zwdalk,bssalg_1 
      real, Dimension(1000)                   :: zwdaak, zwsedk, zwzok, zwkmor, zwkigr, zwantb, zwkbcm, zwaki, zwagr  
      real, Dimension(1000)                   :: zwkiiv, zwgriv, zwsisd, zwkmua, zwfta, zwfia, zwfhea, zwkrau, zwbsct
      real, Dimension(1000)                   :: zwsvhb, zwsvhg, zwdalg, zwdaag, zwsedg, zwzog, zwgmor, zwgbcm  
      real, Dimension(1000)                   :: zwgmua, zwfiga, zwfhga, zwgrau, zwadrk, zwadrg, zwacok, zwacog, zwvo2 
      real, Dimension(1000)                   :: zwzooi, zwabsz, zwdzr1, zwdzr2, zwzexk, zwzexg, zwrmue, zwiras, zwrakr
      real, Dimension(1000)                   :: zwrbar, zwno2, zwx0, zwgo2n, zwbsbt, zwschr, zwpfl, zwsgon, zwsdx0  
      real, Dimension(1000)                   :: zwdon, zwsusn, zwbetn, zwsuso, zwagn4, zwakn4, zwagn3, zwabn4, zwabn3  
      real, Dimension(1000)                   :: zwakn3, zwph, bph_1, zwx02, zwgesN, zwgesP, zwsedn, zwexdb, zwCsed_abb, zwrdr    
      real, Dimension(1000)                   :: zwexdk, zwexdg, zwzexb, zwobsb, zwocsb, zwvbsb, zwvcsb, zwsbsb, zwbsbe   
      real, Dimension(1000)                   :: zwdfak, zwdfab, zwdfag, zwdfas, zwssdr, zwCsed, zwcm, zwBAC, zwHNFB   
      real, Dimension(1000)                   :: zwBSBH, zwHNF, zwfbgr, zwfrgr, zwnl0, zwpl0, zwpo2p, zwpo2r, zwso2e    
      real, Dimension(1000)                   :: zwsalo,zwdalo, zwdago, zwo2ei, zwabwg, zwabwk, zwabrg, zwabrk, zwrodr
      real, Dimension(1000)                   :: zwrzo, zwrHNF, zworgS, zwss, zwfssg, zwsedS, zwmw, zwpw, zwca, zwlf 
      real, Dimension(1000)                   :: zwstin, zwtpki, zwtpgr, zwchlk, zwchlg, zwbsP, zwbsN, zwchlb   
      real, Dimension(1000)                   :: zwn4z, zwn2z, zwn3z, zwPz, zwgN4z, zwkN4z, zwbN4z, zwbn3z, zwgN3z
      real, Dimension(1000)                   :: zwkN3z, zwsiz, zup_PK, zup_NK, zup_Si, zQ_PK, zQ_NK, zQ_SK, zaktbr 
      real, Dimension(1000)                   :: zup_PG, zup_NG, zagtbr, zQ_PG, zQ_NG, zwakz, zwaakz, zwagz, zwaagz 
      real, Dimension(1000)                   :: zwdalb, zwdaab, zwsedb, zwzob, zwbmor, zwbbcm, zwabl, zwbmua, zwfiba 
      real, Dimension(1000)                   :: zwfhba, zwbrau, zwadrb, zwacob, zwtpbl, zup_PB, zup_NB, zQ_PB, zQ_NB 
      real, Dimension(1000)                   :: zabtbr, zwabz, zwaabz, zwCoIs, zwflae, zwlboe, zwSKmo, zww2, zwSdOM 
      real, Dimension(1000)                   :: zwbso, zwJN2,zwTGZoo, zwColi, zwDOSCF, zwakmor_1, zwagmor_1, zwabmor_1
      real, Dimension(1000)                   :: zwgsZn, zwglZn, zwgsCad, zwglCad, zwgsCu, zwglCu, zwgsNi, zwglNi
      real, Dimension(1000)                   :: zwKorn, zwFlN3, zwJNO3, zwJNH4, zwJPO4, zwJO2, zwJSi, zwJDOC1, zwJDOC2
      real, Dimension(1000)                   :: zwsedAlg_MQ, zwsedSS_MQ, ss, vol, so2ein, ir, gwdmax, sedx0, don, susn 
      real, Dimension(1000)                   :: bettn, agrnh4, akinh4, susno, akino3, agrno3, iras, sedalg, sedalk 
      real, Dimension(1000)                   :: sedAlk0, sedalg0, algzog, algzok, abrzo1, algdrg, algdrk, vkigr, chlagr 
      real, Dimension(1000)                   :: mw, pw(1000),lf, ca, vph, dgrmor, dkimor, dalgo, dalgao, bsbbet, o2ein1  
      real, Dimension(1000)                   :: chlaki, abeowg, abeorg, abeowk, abeork, akbcm, agbcm, akbcmz, pfldalg 
      real, Dimension(1000)                   :: lboem, bsohlm, cmatgr, cmatki, ffood, fssgr, fbsgr, frfgr, sedss, r

      real, Dimension(1000)                   :: lfy, akiy, agry, iry, tempwy, vbsby, vcsby, vnh4y, tiefey, vx02y
      real, Dimension(1000)                   :: vo2y, vno3y, vno2y, vx0y, siy, vkigry, CMy, BACy, CHNFy, BVHNFy, dly  
      real, Dimension(1000)                   :: chlay, chlaky, chlagy, chlaby, ssalgy, zooiny, gelpy, coliy, tau2y, gsPy 
      real, Dimension(1000)                   :: mwy, cay, vphy, tpkiy, tpgry, gsNy, sedn, orgCsd0, susny, bettny, dony
      real, Dimension(1000)                   :: agrn4y, akin4y, FluN3y, sedx0y, susnoy, sedagy, sedaky, algzgy, alNO3y
      real, Dimension(1000)                   :: algzky, algdgy, algdky, volfdy, abowgy, abowky, aborgy, aborky, dalggy 
      real, Dimension(1000)                   :: dalgky, dalagy, dalaky, dgmory, dkmory, sgo2ny, sdbsby, so2eiy, salgoy 
      real, Dimension(1000)                   :: bsbty, dalgoy, dalaoy, schlry, bsbbey, o2ei1y, ro2dry, zoro2y, po2py  
      real, Dimension(1000)                   :: po2ry, nl0y, pl0y, extky, JNO3y, JNH4y, JPO4y, JO2y, JSiy, Q_NKy, Q_PKy 
      real, Dimension(1000)                   :: Q_SKy, Q_NGy, Q_PGy, Q_NBy, Q_PBy, coroy, corosy, ffoody, pfly
      real, Dimension(1000)                   :: alby, CChlky, CChlgy, CChlby
   
      real, Dimension(1000)                   :: gsZny, glZny, gsCady, glCady, gsCuy, glCuy, gsNiy, glNiy

      real, Dimension(1000)                   :: btempy, bno3y, bnh4y, bgelpy, bchlay, bssaly, bsiy, bakiy, bagry, bno2y 
      real, Dimension(1000)                   :: bvbsby, bvcsby, bo2y, bphy, bcay, bmwy, blfy, bably, bnl0y, bpl0y, bgsPy  
      real, Dimension(1000)                   :: bgsNy, bCMy, bBACy, bchlky, bchlgy, bdakiy, bdaaky, bsedky, bazoky, bkmory  
      real, Dimension(1000)                   :: bkigry, bkbcmy, biry, bkiivy, bsisdy, bkmuay, bftkay, bfikay, bfhkay
      real, Dimension(1000)                   :: bkray, btpkiy, btpgry, btpbly, bdagry, bdaagy, bsedgy, bazogy, bgmory 
      real, Dimension(1000)                   :: badrky, badrgy, bacoky, bacogy, bgmuay, bfigay, bfhgay, bgray, bzooiy 
      real, Dimension(1000)                   :: bfibay, bantby, bextky, bdably, bdaaby, bsedby, bazoby, bbmory, badrby
      real, Dimension(1000)                   :: bacoby, bbmuay, bfhbay, bbray, bchlby, bFlN3y, bbetNy, bJNO3y, bJNH4y
      real, Dimension(1000)                   :: bJPO4y, bJSiy, bJO2y, bcoliy, volfco, algcok, algcog, algcky, algcgy
      real, Dimension(1000)                   :: bgsZny, bglZny, bgsCady, bglCady, bgsCuy, bglCuy, bgsNiy, bglNiy  
      real, Dimension(1000)                   :: bJDOC1, bJDOC2, btracer, abegm2, abekm2, coroI, coroIs, corol, corosl
      real, Dimension(1000)                   :: JDOC1, JDOC2, sgwmue, dH2De, FluxT1, saett, susO2N

      real, Dimension(1000,2)                 :: idras, idrasy, dreiy, dreisy, gwdrly, drmas, drmasy, drakr, drakry 
      real, Dimension(1000,2)                 :: drbar, drbary, drmor, drmory
      real, Dimension(1000,5)                 :: coro, coros, hcoro, hcoros, coro2, coros2


      real, Dimension(2,1000)                 ::  bCDy, bCPy

      real, Dimension(50,1000)                :: tempwz, tempzy, vnh4zy, vno2zy, vno3zy, vo2zy, gelPzy, sizy, chlazy  
      real, Dimension(50,1000)                :: akizy, agrzy, ablzy, dtemp, vnh4z, vno2z, vno3z, vo2z, gelPz, siz
      real, Dimension(50,1000)                :: vz1, akiz, agrz, ablz, chlaz, agrbrz, akibrz, ablbrz, algakz, algagz 
      real, Dimension(50,1000)                :: algabz, algzkz, algzgz, algzbz, Uvert, dalgkz, dalgbz, dalggz, akNH4z
      real, Dimension(50,1000)                :: abNH4z, agNH4z, akNO3z, abNO3z, agNO3z,CChlakzy,CChlabzy,CChlagzy
      real, Dimension(50,1000)                :: up_NKz, up_PKz, up_Siz, up_N2z, up_NGz, up_PGz, up_NBz, up_PBz

      real, Dimension(1000,100)                :: tausc

      real, Dimension(:), allocatable         :: STRdt, FZeit, ho2_z, hte_z, hph_z, wsp_UW, WSP_OW, wehrh, wehrb
      real, Dimension(:), allocatable         :: QStrang_1, startkm, endkm 
      real, Dimension(:,:), allocatable       :: yWlage, Wlage, ymax, Ymin, vmq, Hmq, boeamq, segkm, clado, sedhy
      real, Dimension(:,:,:), allocatable     :: hClado, bclado, hidras, hdrmas, hdrakr, hdrbar, hRzuwd, hdrmor, tauscg,btausc  
      real, Dimension(:,:,:), allocatable     :: sCD, sCP                                                                       
                                                                       
      real, Dimension(:,:), allocatable       :: hsusn, hbettN, hdon, hagnh4, haknh4, habnh4, halNO3, hsedx0, hsusno  
      real, Dimension(:,:), allocatable       :: hsedag, hsedak, hsedab, halgzg, halgzk, halgzb, halgdg, halgdk      
      real, Dimension(:,:), allocatable       :: halgdb, halgcg, halgck, halgcb, habowg, habowk, hvolfd, hdrpfe  
      real, Dimension(:,:), allocatable       :: haborg, habork, hdalgg, hdalgk, hdalgb, hdalag, hdalak, hdalab, hdgmor   
      real, Dimension(:,:), allocatable       :: hdkmor, hdbmor, hsgo2n, hsdbsb, hsoein, hsalgo, hbsbt, hdalgo, hdalao   
      real, Dimension(:,:), allocatable       :: hSedOM, hBedGS, hsedvvert, hdKorn, dkorn, hbsbbe, hoein1, hro2dr, hzoro2, hpo2p    
      real, Dimension(:,:), allocatable       :: hpo2r, hiras, hrmuas, hrakr, hrbar, hkmuea, hgmuea, hbmuea, hftaau 
      real, Dimension(:,:), allocatable       :: hfiaus, hfigau, hfibau, hfheau, hfhega, hfheba, hakrau, hagrau, habrau  
      real, Dimension(:,:), allocatable       :: hschlr, hDz2D 
   
      real, Dimension(:,:), allocatable       :: hHNFmu, hHNFre, hHNFup, hHNFmo, hHNFex, hHNFdr, hHNFza, hBAmua  
      real, Dimension(:,:), allocatable       :: sedhg, dlalph, dlbeta, dlgamm, hdlarn, midlan, mxdlan
      real, Dimension(:,:), allocatable       :: zdrei, hpfl, zdrel, zdresl, gewdr, hgewdr, VTYP, Rzuwdr, Rzuwdy
      real, Dimension(:,:), allocatable       :: zdreis, CD, CP, migsP, mxgsP, migsN, mxgsN, miaki, mxaki, miagr, mxagr
      integer ilamda  !!wy
      real, Dimension(40)                     :: eta, aw, ack, acg, acb, ah, as, al !!wy Extinktionskoeffizienten von e_extnct.dat gelesen
      real, Dimension(:,:), allocatable       :: extk_lamda, hsised, hSKmor, mxtemp, mitemp, mxb5, mib5, mxcs, mics, mxnh4
      real, Dimension(:,:), allocatable       :: minh4, mxchla,  michla, mxo2, mio2, mizo, mxzo, misi, mxsi, mivph, mxvph   
      real, Dimension(:,:), allocatable       :: micoli, mxcoli, mica, mxca, mimw, mxmw, mivno3, mxvno3, migp, mxgp, mxvno2      
      real, Dimension(:,:), allocatable       :: mivno2, milf, mxlf, miabl, mxabl, miSS, mxSS, sumte, sumb5, sumcs, sumn4

      real, Dimension(:,:), allocatable       :: migsZn, mxgsZn, miglZn, mxglZn, migsCad, mxgsCad, miglCad, mxglCad
      real, Dimension(:,:), allocatable       :: migsCu, mxgsCu, miglCu, mxglCu, migsNi, mxgsNi, miglNi, mxglNi

      real, Dimension(:,:), allocatable       :: sumgsZn, sumglZn, sumgsCad, sumglCad, sumgsCu, sumglCu, sumgsNi, sumglNi
   
      real, Dimension(:,:), allocatable       :: sumsi, sCM, sBAC, sCHNF, sBVHNF, sumcak, sumcag, sumcab, summw, sumlf  
      real, Dimension(:,:), allocatable       :: sumca, sumo2, sumzo, sumss, sumpfl, sumbal, sgsP, sgsN, scoli, sumvph
      real, Dimension(:,:), allocatable       :: sumno3, sumgp, szooro, sumno2, svkigr, santbl, sumabl, snaehr    
      real, Dimension(:,:), allocatable       :: sabmua, svx02, sumaki, sumagr, zwcd, zwcp, zwo2z, zwgPz, zwakiz, zwCors 
      real, Dimension(:,:), allocatable       :: zwcoro, akmB, ekmB, DlB, zwagrz, zwablz, zwchlz, tau2B, alphaB, POMzb 
      real, Dimension(:,:), allocatable       :: zwtez, sedAlg_MQ, sedSS_MQ, svx0, CDy, CPy, orgCsd, orgCsd_abb

      real, Dimension(:,:), allocatable       :: summsl, sumcal, sumdln, scorIg, scoIsg, ssedal, ssedx0, sdon, sFluN3
      real, Dimension(:,:), allocatable       :: ssusn, sbettn, salgzo, salgn, salNO3, ssusno, salgdr, salmor, salgco 
      real, Dimension(:,:), allocatable       :: svoldr, sdrpfe, sabeow, sabeor, sdalg, sdalga, sblmor, ssgo2n, ssdbsb
      real, Dimension(:,:), allocatable       :: ssoein, ssalgo, s2algo, sbsbt, sschlr, sbsbbe, s2algao, so2phy, sro2dr 
      real, Dimension(:,:), allocatable       :: spo2p, spo2r, sir, srmue, srakr, srbar, sffood, sfik, sfig, sfib, sakmua  
      real, Dimension(:,:), allocatable       :: sagmua, sfheka, sfhega, sfheba, sakrau, sagrea, sabrea, sHNFmu, sHNFre 
      real, Dimension(:,:), allocatable       :: sHNFup, sHNFmo, sHNFex, sHNFdr, sHNFz, sBACmu, sHNFBA, snl0, spl0, sJNO3
      real, Dimension(:,:), allocatable       :: sJNH4, sJPO4, sJSi, sJO2
      real, Dimension(:,:), allocatable       :: sumCChlk, sumCChlg, sumCChlb

      real, Dimension(:,:), allocatable       :: bh, bf, vbm, bvmq, bHmq, bw2, w2b, bSedOM, bdKorn, SedOMb, dkornb, w2, hw2  
      real, Dimension(:,:), allocatable       :: btempw, bTsed, bso, blb, bleb, bno3, bnh4, bgelp, bsvhek, bgesN, bgesP  
      real, Dimension(:,:), allocatable       :: bsvheg, bagbcm, bchla, bir, bssalg, bsi, bdaki, bdaak, bsedak, bazok 
      real, Dimension(:,:), allocatable       :: bdkmor, bvkigr, bakbcm, baki, bagr, bsised,bSKmor, bfheau, bpfl, bakmua
      real, Dimension(:,:), allocatable       :: bftaau, bfiaus, bakrau, bbsbt, bschlr, bbsb, bcsb, bo2, bno2, bx0, bchlak
      real, Dimension(:,:), allocatable       :: bchlag, babrz1, bss, bzooi, bmw, bpw, bvcsb, bca, blf, bph, bvbsb, babewk 
      real, Dimension(:,:), allocatable       :: bdlarn, bx02, bstind, bdagr, bdaag, bsedag, bazog, bdgmor, babewg, baberg  
      real, Dimension(:,:), allocatable       :: baberk, bresdr, badrk, badrg, bacok, bacog, bacob, badrb, bagmua, bfigas 
      real, Dimension(:,:), allocatable       :: bfhgau, bagrau, babszo, bzres1, bzres2, bzexki, bzexgr, brmuas, bzexbl
      real, Dimension(:,:), allocatable       :: biras, brakr, brbar, bfssgr, bfbsgr, bfrfgr, bexdvk, bexdvg, bsgon, bsedx0
      real, Dimension(:,:), allocatable       :: bexdvb, bdon, bsusn, bbettn, bsuso, bagn4, bakn4, bagn3, babn4, babn3
      real, Dimension(:,:), allocatable       :: bakn3, bsedn, bBVHNF, bsdbsb, bbsbbe, bdfaek, bdfaeg, bdfaeb, bdfaes 
      real, Dimension(:,:), allocatable       :: bssdr, borgCs, borgCs_abb, bbsbct, bbsbcP, bcm, bBAC, bHNFBS, bBSBHN 
      real, Dimension(:,:), allocatable       :: bCHNF, bnl0, bpl0, bgo2n, bpo2p, bpo2r, bro2dr, bro2HF, borgSS, bJNO3, bJN2  
      real, Dimension(:,:), allocatable       :: bJNH4, bJSi, bJPO4, bJO2, bsedSS, babbcm, babl, bchlab, bantbl, bsvheb
      real, Dimension(:,:), allocatable       :: btpki, btpgr, bextk, bQ_PK, bQ_NK, bQ_SK, bQ_PG, bQ_NG, bQ_PB
      real, Dimension(:,:), allocatable       :: bQ_NB, bFluN3, bdabl, bdaab, bsedab, bazob, bdbmor, babmua, bfibas, bfhbau
      real, Dimension(:,:), allocatable       :: babrau, btpbl, bup_PB, bup_NB, babtbr, balgbz, balabz, bup_PK, bup_NK
      real, Dimension(:,:), allocatable       :: bup_Si, baktbr, bup_PG, bup_NG, bagtbr, balgkz, balakz, balggz, balagz  
      real, Dimension(:,:), allocatable       :: bkN4z, bkN3z, bgN4z, bgN3z, bbN4z, bbN3z, bsedAlg_MQ, bsedSS_MQ, bTGZoo

      real, Dimension(:,:), allocatable       :: bste, bsno3, bsn4, bsgelp, bsno2, bschla, bsssal, bssi, bszooi, bsvbsb
      real, Dimension(:,:), allocatable       :: bsvcsb, bsgsP, bsgsN, bsaki, bsagr, bsabl, bsFlN3, bso2, bsmw, bslf
      real, Dimension(:,:), allocatable       :: bsca, bsph, bsnl0, bspl0, bsdalg, bsvkg, bsdaa, bsseda,bsalgz, bsamor
      real, Dimension(:,:), allocatable       :: bsadr, bsalco, bsfik, bsfig, bskmue, bsgmue, bshek, bsheg, bskre
      real, Dimension(:,:), allocatable       :: bsgre, bschlk, bschlg, bsbmue, bsheb, bsbre, bschlb, bsantb, bsbetN 
      real, Dimension(:,:), allocatable       :: bsJNO3, bsJNH4, bsJPO4, bsJO2, bsJSi, bscoli
      real, Dimension(:,:), allocatable       :: bsgsZn, bsglZn, bsgsCad, bsglCad, bsgsCu, bsglCu, bsgsNi, bsglNi
      real, Dimension(:,:), allocatable       :: bmxtem, bmitem, bmxno3, bmino3, bmxnh4, bminh4, bmxglp, bmiglp, bmxchl
      real, Dimension(:,:), allocatable       :: bmichl, bmxssa, bmissa, bmxsi, bmisi, bmxzoo, bmizoo, bmxno2, bmino2
      real, Dimension(:,:), allocatable       :: bmibsb, bmxbsb, bmicsb, bmxcsb, bmxgsP, bmigsP, bmxgsN, bmigsN, bmxaki
      real, Dimension(:,:), allocatable       :: bmiaki, bmxagr, bmiagr, bmio2, bmxo2, bmxmw, bmimw, bmxlf, bmilf
      real, Dimension(:,:), allocatable       :: bmxca, bmica, bmxph, bmiph, bnaehr, bcoli, bDOSCF, bakmor_1, bagmor_1, babmor_1

      real, Dimension(:,:), allocatable       :: bmxgsZn, bmigsZn, bmxglZn, bmiglZn, bmxgsCad, bmigsCad, bmxglCad, bmiglCad
      real, Dimension(:,:), allocatable       :: bmxgsCu, bmigsCu, bmxglCu, bmiglCu, bmxgsNi, bmigsNi, bmxglNi, bmiglNi

      real, Dimension(:,:), allocatable       :: bgsZn, bglZn, bgsCad, bglCad, bgsCu, bglCu, bgsNi, bglNi
      real, Dimension(:,:), allocatable       :: bsedh


      real, Dimension(:,:), allocatable       :: hfkm, hqaus, hsvhk, hsvhg, hDOSCF, hsvhb, habbcm, habl, hchlab, hantbl
      real, Dimension(:,:), allocatable       :: htempw, hTsed, hbsb, hcsb, hnh4, hCM, hBAC, ho2, hno3, hno2, hx0, hsi
      real, Dimension(:,:), allocatable       :: hx02, hcoli, hchla, hchlak, hchlag, hvkigr, htpki, htpgr, htpbl, hzooi
      real, Dimension(:,:), allocatable       :: habrz1, hssalg, hss, hgelp, hmw, hpw, hca, hlf, hph, hvbsb, hvcsb, haki
      real, Dimension(:,:), allocatable       :: hstind, hagr, hakbcm, hagbcm, hCHNF, hBVHNF, hHNFBA, hfssgr, hfbsgr, hnl0 
      real, Dimension(:,:), allocatable       :: hQ_NK, hQ_PK, hQ_SK, hQ_NG, hQ_PG, hQ_NB, hQ_PB, hpl0, hfrfgr, hffood
      real, Dimension(:,:), allocatable       :: hdl, htau2, hgesP, hgesN, hCD1, hCD2, hCP1, hCP2, hvo2, hextk, hJNO3
      real, Dimension(:,:), allocatable       :: hJNH4, hJPO4, hJSi, hJO2, hFluN3,hJN2, TGZoo, akmor_1, agmor_1, abmor_1

      real, Dimension(:,:), allocatable       :: hglZn, hgsZn, hglCad, hgsCad, hglCu, hgsCu, hglNi, hgsNi

      real, Dimension(:,:), allocatable       :: apfl, epfl, pflmxs, pflmis, aschif, eschif, awett, ewett, abal, ebal
      real, Dimension(:,:), allocatable       :: ggbal, gkbal, akdrei, ekdrei, aPOM, ePOM, POMz, BedGSz, sedvvertz, acoro, ecoro
      real, Dimension(:,:), allocatable       :: coro1s, aKSED, eKSED, SPEWKSx, WUEBKx, PSREFSx, extkx, coross, aVEG, eVEG
      real, Dimension(:,:), allocatable       :: VALTAL, EDUFAL, VALTAR, EDUFAR

      real, Dimension(:,:), allocatable       :: SedOM, BedGSed, sedvvert, SPEWKSuS, WUEBKuS, PSREFSuS, SPEWKSS, WUEBKS, PSREFSS
      real, Dimension(:,:), allocatable       :: extkuS, extkS, Stakm, Raua, bsohla, hlboea, hflaea, htiefa, hvF, hWS
      real, Dimension(:,:), allocatable       :: helen, hvmitt, htiefe, hrau, hrhyd, hflae, hpfmnl, hpfmxl, habgml 
      real, Dimension(:,:), allocatable       :: hlboem, hbsohl, hvabfl, VALTLH, EDUFLH, VALTRH, EDUFRH, habkml

      real, Dimension(:,:), allocatable       :: hdlmx, hdlmxs, hgwdmx, hsgwmu

      real, Dimension(:,:), allocatable       :: hdH2De, Hmax2D


      real, Dimension(:,:), allocatable       :: RBkm, RBkmLe, RBkm1, WirkLL, abfls, obsbs, ocsbs, vnh4s, vno2s
      real, Dimension(:,:), allocatable       :: vno3s, gesNs, vx0s, vx02s, gelps, gesPs, sis, chlas, vkigrs, antbls 
      real, Dimension(:,:), allocatable       :: zooins, vphs, mws, pws, cas, lfs, ssalgs, tempws, vo2s, CHNFs, BVHNFs
      real, Dimension(:,:), allocatable       :: colis, waers, akis, agrs, abls, agbcms, akbcms, abbcms, frfgrs,DOSCFs
      real, Dimension(:,:), allocatable       :: CMS, BACs, nl0s, pl0s, sss, Chlaks, chlabs, chlags, vbsbs, vcsbs
      real, Dimension(:,:), allocatable       :: Q_NKs, Q_PKs, Q_SKs, Q_NGs, Q_PGs, Q_NBs, Q_PBs 

      real, Dimension(:,:), allocatable       :: glZns, gsZns, glCads, gsCads, glCus, gsCus, glNis, gsNis 

      real, Dimension(:,:), allocatable       :: einlkh, qeinlh, ebsbh, ecsbh, enh4h, ex0h, eo2h, etemph, echlah
      real, Dimension(:,:), allocatable       :: ezindh, egph, esih, eno3h, essh, ewaerh, enl0h, epl0h, ephh, emwh 
      real, Dimension(:,:), allocatable       :: elfh, ecah, ex02h, eno2h, eCHNFh, eBVHNh, egesNh, egesPh, ecolih
      real, Dimension(:,:), allocatable       :: evkgh, eantbh, eCM, eBAC
      real, Dimension(:,:), allocatable       :: egsZn, eglZn, egsCad, eglCad, egsCu, eglCu, egsNi, eglNi

      real, Dimension(:,:), allocatable       :: qLh, bsbLh, csbLh, enh4Lh, eno2Lh, eno3Lh, gesNLh, x0Lh, x02Lh
      real, Dimension(:,:), allocatable       :: gpLh, gesPLh, siLh, phLh, caLh, elfLh, ssLh, tempLh, o2Lh, coliLh  
      real, Dimension(:,:), allocatable       :: enl0Lh, pl0Lh, chlaLh, CML, BACL
      real, Dimension(:,:), allocatable       :: afkm2D, efkm2D

      real, Dimension(:,:), allocatable       :: ho2z_z, htez_z, hchlaz_z, hakiz_z, hagrz_z, hablz_z, hNh4z_z, hNO2z_z
      real, Dimension(:,:), allocatable       :: hNO3z_z, hPz_z, hSiz_z, hchlkz_z, hchlgz_z, hchlbz_z, hgesPz_z, hgesNz_z
      real, Dimension(:,:), allocatable       :: hQ_NKz_z, hQ_NBz_z, hQ_NGz_z, hCChlkz_z, hCChlbz_z, hCChlgz_z 
 
      real, Dimension(:,:,:), allocatable     :: bcd, bcp, hCD, hCP, CDL, CPL, zdrs, zdrss, gwdrs, VTYPA 

      real, Dimension(:,:,:), allocatable     :: sidras, sdrmas, sdrakr, sdrbar, sdrmor, szdrg, szdrsg, sgwdrg, wstand
      real, Dimension(:,:,:), allocatable     :: hzdrel, hzdrsl, hgwdrl, VTYPH 
      real, Dimension(:,:,:), allocatable     :: CDs, CPs, eCD, eCP, hcoro2, hcos2

      real, Dimension(:,:,:), allocatable     :: hnh4z, hno2z, hno3z, ho2z, hgelPz, hgesPz, hgesNz, hsiz
      real, Dimension(:,:,:), allocatable     :: hakiz, hagrz, hablz, hchlaz, hchlkz, hchlgz, hchlbz, htempz
      real, Dimension(:,:,:), allocatable     :: hQ_NKz, hQ_NBz, hQ_NGz, hCChlkz, hCChlbz, hCChlgz


      real, Dimension(:,:,:), allocatable     :: Tzt, o2zt, NH4zt, NO2zt, NO3zt, Pzt, gSizt, akizt, agrzt, ablzt
      real, Dimension(:,:,:), allocatable     :: chlazt, chlkzt, chlgzt, chlbzt, gesPzt, gesNzt, Q_NKzt, Q_NBzt, Q_NGzt
      real, Dimension(:,:,:), allocatable     :: CChlkzt, CChlbzt, CChlgzt

!###### VERSIONSNUMMER ################

      VERSIO = 14.00
      linux  = .true.
      kontroll= .false.
!######################################
                                       
!                                                                       
!.....Schalter für "Regeln bei Kraftwerksbetrieb"                       
!               iRHKW = 1 > Betrieb der HKW's unterliegt gewissen Regeln
      iRHKW = 0 
!                                                                       
!...Dicke der vertikalen Schichten                                      
!                                                                       
      maus = 0
      iend = 0
      iwied = 0 
      ilang = 0 
      ilbuhn = 0 
      jlauf = 0 
      jtag = 0 
      dH2D = 0.25 
!                                                                       
!......Vorbelegungen                                                    
      n1 = 1 
      n2 = 2
      n3 = 3 
      iergeb = 0

      itracer_vor = 0
                                                                       
 
      cmin = 'Minimum' 
      cmax = 'Maximum' 
      cpoint ='.' 
                                                                       
      call GETARG(n1,cpfad) 
      call GETARG(n2,cpfad1)
      call GETARG(n3,cpfad2)

      if(cpfad1.eq.' ')cpfad1 = cpfad 

      if(cpfad=='/F')then
      j1 = 0

      do i=1,255
        if(i<255.and.cpfad1(i:i)==' '.and.cpfad1(i+1:i+1)==' ')exit
      enddo

      j = i
      cpfad1(j:j) = '\'
      if (linux) cpfad1(j:j) = '/' 
      cpfad1 = cpfad1(1:j)
      j1 = j

        call AParamParam(cpfad1,j1)

        call EreigGParam(cpfad1,j1)
        call ModellGParam(cpfad1,j1)
        call E_extnctParam(cpfad1,j1)
        call EreigHParam(cpfad1,j1)
        call Ergeb2DParam(cpfad1,j1)
        call ErgebMParam(cpfad1,j1)
        call ErgebTParam(cpfad1,j1)
        call WetterParam(cpfad1,j1)
          else
           j1 = 0
           j2 = 0
      if(cpfad(1:5)==' ')then
        else

      ifhStr = 0 
      fhProf = 0.0 
      cerrts = 'OK' 

      do i=1,255
        if(i<255.and.cpfad(i:i)==' '.and.cpfad(i+1:i+1)==' ')exit
      enddo

      j = i
      cpfad(j:j) = '\'
      if (linux) cpfad(j:j) = '/' 
      cpfad = cpfad(1:j)
      j1 = j

      do i=1,255
        if(i<255.and.cpfad1(i:i)==' '.and.cpfad1(i+1:i+1)==' ')exit
      enddo

      j = i
      cpfad1(j:j) = '\'
      if (linux) cpfad1(j:j) = '/'
      cpfad1 = cpfad1(1:j)
      j2 = j

        endif
        
        print*,'cpfad=',j1,cpfad;print*,'cpfad1=',cpfad1;print*,'versio=',versio

        call fehlermeldungen(cpfad,j1)

        write(pfadstring,'(2A)')trim(adjustl(cpfad)),'file1.err' 
        open(unit=199, file=pfadstring)
      
      rewind (199) 
      write(199,'(a2)')cerrts 
      rewind (199) 
!                                                                       
!...Ausgabe von Bemerkungen
        write(pfadstring,'(2A)')trim(adjustl(cpfad)),'file2.err' 
        open(unit=299, file=pfadstring)
      rewind (299)

      read(299,'(f5.2)',iostat=read_error)Version_alt
      if(read_error<0)then
        if(versio>=13.20)then
          write(*,1200)
          1200 format(2x,'ab der Version 13.20 wird das C:Chl-Verhaeltnis statt')
          write(*,1202)
          1202 format(2x,'des Chla:Biomasse-Verhaeltnisses benutzt!!')
          write(*,*)
          write(*,1205)
          1205 Format(2x,'Eingabe der max. Algenwachstumsraten fuer 20 Grad C !!!')
          write(*,1207)
          1207 Format(2x,'Default-Werte in <aparam_gerris.xlsx>') 
          write(*,*)
          write(*,1210)
          1210 format(2x,'falls die Werte schon geaendert wurden "weiter mit: w"')
          write(*,1215) 
          1215 format(2x,'ansonsten "Berechnungsabruch mit: s"')
          read(*,'(a1)')ctaste
          if(ctaste=='s'.or.ctaste=='S')then
            rewind(299)
            write(299,'(f5.2)')versio
          stop 1
          endif
          rewind(299)
          write(299,'(f5.2)')versio
        endif    
      endif

      if(Version_alt>13.19.and.Versio<=13.19)then
         write(*,1220)
         1220 format(2x,'Eingabe des Chla:Biomasse-Verhaeltnisses')
         write(*,*)
         write(*,1210)
         write(*,1215)
         read(*,'(a1)')ctaste
         if(ctaste=='s'.or.ctaste=='S')then
           rewind(299)
           write(299,'(f5.2)')versio
           stop 1
         endif
       endif

           if(Version_alt>0.0.and.Version_alt<=13.19.and.Versio>13.19)then
              write(*,1200)
              write(*,1202)
              write(*,*)
              write(*,1205)
              write(*,1207)
              write(*,*)
              write(*,1210)
              write(*,1215) 
              read(*,'(a1)')ctaste
              if(ctaste=='s'.or.ctaste=='S')then
                rewind(299)
                write(299,'(f5.2)')versio
                stop 1
              endif
         endif

              rewind(299)
              write(299,'(f5.2)')versio

        write(pfadstring,'(2A)')trim(adjustl(cpfad)),'qsim.tst' 
        open(unit=89, file=pfadstring)
                                                                       
      if(iRHKW.eq.1)then
        write(pfadstring,'(2A)')trim(adjustl(cpfad)),'Red_HKW.txt' 
        open(unit=177, file=pfadstring)
      endif            
                                                                       
!.....nndr : Anzahl der Kohorten                                        
      nndr = 2 
                                                                       
!.....Einlesen aus ModellA                                              
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'MODELLA.txt' 
      open(unit=10, file=pfadstring)
      rewind (10) 
      jStr = 0 
      read(10,'(2a)')ckenn_vers
        if(ckenn_vers/='*V')then
          else
            rewind(10)
            read(10,1110)VERSIO
            read(10,'(a2)')chcon 
            1110 Format(19x,F5.2)  
      endif
      read(10,'(f5.2,2x,f5.2)')GeoB,GeoL 
      read(10,'(I5)')azStrs
   
!  ########################
!  allocieren der Variablen  
!  ########################

      ialloc1 = 100
      ialloc2 = 1000
      ialloc3 = 20
      ialloc4 = 250
      ialloc5 = 50
      nazStrs = 2 * azStrs

      allocate(hanze(azStrs), ianze(azStrs), STRiz(azStrs),isub_dt(azStrs),imac(azStrs),isub_dt_Mac(azStrs), mstr_ist(azStrs*2))
      allocate(strNr(nazStrs), mstra(azStrs), ieinsh(azStrs), ieinLs(azStrs), nbuhn(azStrs), iFlRi(nazStrs), isegs(azStrs))
      allocate(STRID(azStrs), janzWt(azStrs), janzWs(azStrs), jlwo2(azStrs), iRB_K1(azStrs), ho2_z(azStrs), hte_z(azStrs), izufluss(azStrs))
      allocate(hph_z(azStrs), iFlRi_l(nazStrs), imRB_K1(ialloc1))

      allocate(mPfs(azStrs), mSs(azStrs), mDs(azStrs), mCs(azStrs), mBs(azStrs), mUs(azStrs), i2Ds(azStrs))
      allocate(mWes(azStrs), mVs(azStrs), mZs(azStrs), mAs(azStrs), itsts(azStrs), msts(azStrs), itmaxs(azStrs))
      allocate(mmaxs(azStrs), itends(azStrs), mends(azStrs), laits(azStrs), laims(azStrs), laids(azStrs), mStas(azStrs))
      allocate(abfr(azStrs), mwehr(azStrs), mRBs(azStrs), wsp_UW(azStrs),WSP_OW(azStrs), wehrh(azStrs), wehrb(azStrs))
      allocate(QStrang_1(azStrs),startkm(azStrs),endkm(azStrs))

      allocate(it_h(azStrs,ialloc2), it_hy(azStrs,ialloc2), iorLah(azStrs,ialloc1),iorLeh(azStrs,ialloc1))
      allocate(typh(azStrs,ialloc1), ischig(azStrs,ialloc2), ikWSta(azStrs,ialloc3), idWe(azStrs,ialloc2), ibschi(azStrs,ialloc2))
      allocate(mstrLe(azStrs,ialloc1), istund(azStrs,ialloc1), RBtyp(azStrs,ialloc1), Weinl(azStrs,ialloc1))
      allocate(NRSchr(azStrs,ialloc1), hflag(azStrs,ialloc2), hjiein(azStrs,ialloc2), hischf(azStrs,ialloc2))
      allocate(hnkzs(azStrs,ialloc2), nkzmx(azStrs,ialloc2), znkzs(azStrs,ialloc2), inkzs(azStrs,2), ESTRNR(nazStrs,azStrs))
      allocate(nstrs(azStrs*2), nnstrs(azStrs))

      allocate(STRdt(azStrs), FZeit(azStrs), yWlage(azStrs,ialloc3), Wlage(azStrs,ialloc2), ymax(azStrs,ialloc4))
      allocate(Ymin(azStrs,ialloc4), vmq(azStrs,ialloc2), Hmq(azStrs,ialloc2), boeamq(azStrs,ialloc2))
      allocate(segkm(azStrs,ialloc2), clado(10,ialloc2), hClado(azStrs,5,ialloc2), bclado(azStrs,5,ialloc2))
      allocate(hidras(azStrs,ialloc2,2), hdrmas(azStrs,ialloc2,2), hdrakr(azStrs,ialloc2,2), hdrbar(azStrs,ialloc2,2))
      allocate(hRzuwd(azStrs,ialloc2,2), hdrmor(azStrs,ialloc2,2), tauscg(azStrs,ialloc2,ialloc4), sCD(azStrs,2,ialloc2))  
      allocate(sCP(azStrs,2,ialloc2), hsusn(azStrs,ialloc2), hbettN(azStrs,ialloc2), hdon(azStrs,ialloc2),btausc(azStrs,ialloc2,ialloc4))                                                                        
      allocate(hagnh4(azStrs,ialloc2), haknh4(azStrs,ialloc2), habnh4(azStrs,ialloc2), halNO3(azStrs,ialloc2))
      allocate(hsedx0(azStrs,ialloc2), hsusno(azStrs,ialloc2), hsedag(azStrs,ialloc2), hsedak(azStrs,ialloc2))
      allocate(hsedab(azStrs,ialloc2), halgzg(azStrs,ialloc2), halgzk(azStrs,ialloc2), halgzb(azStrs,ialloc2))
      allocate(halgdg(azStrs,ialloc2), halgdk(azStrs,ialloc2), halgdb(azStrs,ialloc2), halgcg(azStrs,ialloc2))
      allocate(halgck(azStrs,ialloc2), halgcb(azStrs,ialloc2), habowg(azStrs,ialloc2), habowk(azStrs,ialloc2))
      allocate(hvolfd(azStrs,ialloc2), hdrpfe(azStrs,ialloc2),haborg(azStrs,ialloc2), habork(azStrs,ialloc2))
      allocate(hdalgg(azStrs,ialloc2), hdalgk(azStrs,ialloc2), hdalgb(azStrs,ialloc2), hdalag(azStrs,ialloc2))
      allocate(hdalak(azStrs,ialloc2), hdalab(azStrs,ialloc2), hdgmor(azStrs,ialloc2), hsoein(azStrs,ialloc2))
      allocate(hdkmor(azStrs,ialloc2), hdbmor(azStrs,ialloc2), hsgo2n(azStrs,ialloc2), hsdbsb(azStrs,ialloc2))
      allocate(hsalgo(azStrs,ialloc2), hbsbt(azStrs,ialloc2), hdalgo(azStrs,ialloc2), hdalao(azStrs,ialloc2))
      allocate(hSedOM(azStrs,ialloc2), hBedGS(azStrs,ialloc2), hdKorn(azStrs,ialloc2), dkorn(azStrs,ialloc2))
      allocate(hbsbbe(azStrs,ialloc2), hoein1(azStrs,ialloc2), hro2dr(azStrs,ialloc2), hzoro2(azStrs,ialloc2))
      allocate(hpo2p(azStrs,ialloc2), hpo2r(azStrs,ialloc2), hiras(azStrs,ialloc2), hrmuas(azStrs,ialloc2))
      allocate(hrakr(azStrs,ialloc2), hrbar(azStrs,ialloc2), hkmuea(azStrs,ialloc2), hgmuea(azStrs,ialloc2))
      allocate(hbmuea(azStrs,ialloc2), hftaau(azStrs,ialloc2), hfiaus(azStrs,ialloc2), hfigau(azStrs,ialloc2))
      allocate(hfibau(azStrs,ialloc2), hfheau(azStrs,ialloc2), hfhega(azStrs,ialloc2), hfheba(azStrs,ialloc2))
      allocate(hakrau(azStrs,ialloc2), hagrau(azStrs,ialloc2), habrau(azStrs,ialloc2), hschlr(azStrs,ialloc2)) 
      allocate(hDz2D(azStrs,ialloc2), hsedvvert(azStrs,ialloc2))
      allocate(hHNFmu(azStrs,ialloc2), hHNFre(azStrs,ialloc2), hHNFup(azStrs,ialloc2), hHNFmo(azStrs,ialloc2))
      allocate(hHNFex(azStrs,ialloc2), hHNFdr(azStrs,ialloc2), hHNFza(azStrs,ialloc2), hBAmua(azStrs,ialloc2))
      allocate(sedhg(azStrs,ialloc2), dlalph(azStrs,ialloc2),bsedh(azStrs,ialloc2))
      allocate(dlbeta(azStrs,ialloc2), dlgamm(azStrs,ialloc2), hdlarn(azStrs,ialloc2), midlan(azStrs,ialloc2))
      allocate(mxdlan(azStrs,ialloc2), zdrei(ialloc2,2), hpfl(azStrs,ialloc2), zdrel(ialloc2,4))
      allocate(zdresl(ialloc2,4), gewdr(ialloc2,4), hgewdr(ialloc2,4), VTYP(ialloc2,14), Rzuwdr(ialloc2,4))
      allocate(Rzuwdy(ialloc2,4), zdreis(ialloc2,4), CD(2,ialloc2), CP(2,ialloc2), migsP(azStrs,ialloc2)) 
      allocate(mxgsP(azStrs,ialloc2), migsN(azStrs,ialloc2), mxgsN(azStrs,ialloc2), miaki(azStrs,ialloc2))
      allocate( mxaki(azStrs,ialloc2), miagr(azStrs,ialloc2), mxagr(azStrs,ialloc2), extk_lamda(40,ialloc2)) 
      allocate(hsised(azStrs,ialloc2), hSKmor(azStrs,ialloc2), mxtemp(azStrs,ialloc2), mitemp(azStrs,ialloc2)) 
      allocate(mxb5(azStrs,ialloc2), mib5(azStrs,ialloc2), mxcs(azStrs,ialloc2), mics(azStrs,ialloc2))
      allocate(mxnh4(azStrs,ialloc2), minh4(azStrs,ialloc2), mxchla(azStrs,ialloc2),  michla(azStrs,ialloc2))
      allocate(mxo2(azStrs,ialloc2), mio2(azStrs,ialloc2), mizo(azStrs,ialloc2), mxzo(azStrs,ialloc2))
      allocate(misi(azStrs,ialloc2), mxsi(azStrs,ialloc2), mivph(azStrs,ialloc2), mxvph(azStrs,ialloc2))
      allocate(micoli(azStrs,ialloc2), mxcoli(azStrs,ialloc2), mica(azStrs,ialloc2), mxca(azStrs,ialloc2))
      allocate(mimw(azStrs,ialloc2), mxmw(azStrs,ialloc2), mivno3(azStrs,ialloc2), mxvno3(azStrs,ialloc2))
      allocate(migp(azStrs,ialloc2), mxgp(azStrs,ialloc2), mxvno2(azStrs,ialloc2), mivno2(azStrs,ialloc2))
      allocate(milf(azStrs,ialloc2), mxlf(azStrs,ialloc2), miabl(azStrs,ialloc2), mxabl(azStrs,ialloc2))
      allocate(miSS(azStrs,ialloc2), mxSS(azStrs,ialloc2), sumte(azStrs,ialloc2), sumb5(azStrs,ialloc2))

      allocate(migsZn(azStrs,ialloc2), mxgsZn(azStrs,ialloc2), miglZn(azStrs,ialloc2), mxglZn(azStrs,ialloc2))
      allocate(migsCad(azStrs,ialloc2), mxgsCad(azStrs,ialloc2), miglCad(azStrs,ialloc2), mxglCad(azStrs,ialloc2))
      allocate(migsCu(azStrs,ialloc2), mxgsCu(azStrs,ialloc2), miglCu(azStrs,ialloc2), mxglCu(azStrs,ialloc2))
      allocate(migsNi(azStrs,ialloc2), mxgsNi(azStrs,ialloc2), miglNi(azStrs,ialloc2), mxglNi(azStrs,ialloc2))
      allocate(sumgsZn(azStrs,ialloc2), sumglZn(azStrs,ialloc2), sumgsCad(azStrs,ialloc2), sumglCad(azStrs,ialloc2))
      allocate(sumgsCu(azStrs,ialloc2), sumglCu(azStrs,ialloc2), sumgsNi(azStrs,ialloc2), sumglNi(azStrs,ialloc2))

      allocate(sumcs(azStrs,ialloc2), sumn4(azStrs,ialloc2),sumsi(azStrs,ialloc2), sCM(azStrs,ialloc2))
      allocate(sBAC(azStrs,ialloc2), sCHNF(azStrs,ialloc2), sBVHNF(azStrs,ialloc2), sumcak(azStrs,ialloc2))
      allocate(sumcag(azStrs,ialloc2), sumcab(azStrs,ialloc2), summw(azStrs,ialloc2), sumlf(azStrs,ialloc2))
      allocate(sumca(azStrs,ialloc2), sumo2(azStrs,ialloc2), sumzo(azStrs,ialloc2), sumss(azStrs,ialloc2))
      allocate(sumpfl(azStrs,ialloc2), sumbal(azStrs,ialloc2), sgsP(azStrs,ialloc2), sgsN(azStrs,ialloc2))
      allocate(scoli(azStrs,ialloc2), sumvph(azStrs,ialloc2), sumno3(azStrs,ialloc2), sumgp(azStrs,ialloc2))
      allocate(szooro(azStrs,ialloc2), sumno2(azStrs,ialloc2), svkigr(azStrs,ialloc2), santbl(azStrs,ialloc2))
      allocate(sumabl(azStrs,ialloc2), sabmua(azStrs,ialloc2), svx02(azStrs,ialloc2), sumaki(azStrs,ialloc2))
      allocate(sumagr(azStrs,ialloc2),snaehr(azStrs,ialloc2), zwcd(2,ialloc2),zwcp(2,ialloc2), zwo2z(azStrs,ialloc2))
      allocate(zwgPz(azStrs,ialloc2),zwakiz(azStrs,ialloc2), zwCors(ialloc2,5), zwcoro(ialloc2,5), akmB(azStrs,ialloc1))
      allocate(ekmB(azStrs,ialloc1), DlB(azStrs,ialloc1), zwagrz(azStrs,ialloc2),zwablz(azStrs,ialloc2))
      allocate(zwchlz(azStrs,ialloc2), tau2B(azStrs,ialloc1), alphaB(azStrs,ialloc1), POMzb(azStrs,ialloc1))
      allocate(zwtez(ialloc5,ialloc2), sedAlg_MQ(azStrs,ialloc2),sedSS_MQ(azStrs,ialloc2), svx0(azStrs,ialloc2))
      allocate(CDy(2,ialloc2), CPy(2,ialloc2), orgCsd(azStrs,ialloc2), orgCsd_abb(azStrs,ialloc2))

      allocate(summsl(azStrs,ialloc2), sumcal(azStrs,ialloc2), sumdln(azStrs,ialloc2), scorIg(azStrs,ialloc2))
      allocate(scoIsg(azStrs,ialloc2), ssedal(azStrs,ialloc2), ssedx0(azStrs,ialloc2), sdon(azStrs,ialloc2))
      allocate(sFluN3(azStrs,ialloc2), ssusn(azStrs,ialloc2), sbettn(azStrs,ialloc2), salgzo(azStrs,ialloc2))
      allocate(salgn(azStrs,ialloc2), salNO3(azStrs,ialloc2), ssusno(azStrs,ialloc2), salgdr(azStrs,ialloc2))
      allocate(salmor(azStrs,ialloc2), salgco(azStrs,ialloc2), svoldr(azStrs,ialloc2), sdrpfe(azStrs,ialloc2))
      allocate(sabeow(azStrs,ialloc2), sabeor(azStrs,ialloc2), sdalg(azStrs,ialloc2), sdalga(azStrs,ialloc2))
      allocate(sblmor(azStrs,ialloc2), ssgo2n(azStrs,ialloc2), ssdbsb(azStrs,ialloc2), ssoein(azStrs,ialloc2))
      allocate(ssalgo(azStrs,ialloc2), s2algo(azStrs,ialloc2), sbsbt(azStrs,ialloc2), sschlr(azStrs,ialloc2))
      allocate(sbsbbe(azStrs,ialloc2), s2algao(azStrs,ialloc2), so2phy(azStrs,ialloc2), sro2dr(azStrs,ialloc2)) 
      allocate(spo2p(azStrs,ialloc2), spo2r(azStrs,ialloc2), sir(azStrs,ialloc2), srmue(azStrs,ialloc2))
      allocate(srakr(azStrs,ialloc2), srbar(azStrs,ialloc2), sffood(azStrs,ialloc2), sfik(azStrs,ialloc2))
      allocate(sfig(azStrs,ialloc2), sfib(azStrs,ialloc2), sakmua(azStrs,ialloc2), sagmua(azStrs,ialloc2))
      allocate(sfheka(azStrs,ialloc2), sfhega(azStrs,ialloc2), sfheba(azStrs,ialloc2), sakrau(azStrs,ialloc2))
      allocate(sagrea(azStrs,ialloc2), sabrea(azStrs,ialloc2), sHNFmu(azStrs,ialloc2), sHNFre(azStrs,ialloc2)) 
      allocate(sHNFup(azStrs,ialloc2), sHNFmo(azStrs,ialloc2), sHNFex(azStrs,ialloc2), sHNFdr(azStrs,ialloc2))
      allocate(sHNFz(azStrs,ialloc2), sBACmu(azStrs,ialloc2), sHNFBA(azStrs,ialloc2), snl0(azStrs,ialloc2))
      allocate(spl0(azStrs,ialloc2), sJNO3(azStrs,ialloc2), sJNH4(azStrs,ialloc2), sJPO4(azStrs,ialloc2))
      allocate(sJSi(azStrs,ialloc2), sJO2(azStrs,ialloc2), sumCChlk(azStrs,ialloc2), sumCChlg(azStrs,ialloc2))
      allocate(sumCChlb(azStrs,ialloc2))

      allocate(sidras(azStrs,ialloc2,2), sdrmas(azStrs,ialloc2,2), sdrakr(azStrs,ialloc2,2), sdrbar(azStrs,ialloc2,2))
      allocate(sdrmor(azStrs,ialloc2,2), szdrg(azStrs,ialloc2,2), szdrsg(azStrs,ialloc2,2), sgwdrg(azStrs,ialloc2,2))

      allocate(bh(azStrs,ialloc2), bf(azStrs,ialloc2), vbm(azStrs,ialloc2), bvmq(azStrs,ialloc2))
      allocate(bHmq(azStrs,ialloc2), bw2(azStrs,ialloc2), w2b(azStrs,ialloc2), bSedOM(azStrs,ialloc2))
      allocate(w2(azStrs,ialloc2), hw2(azStrs,ialloc2))
      allocate(bdKorn(azStrs,ialloc2), SedOMb(azStrs,ialloc2), dkornb(azStrs,ialloc2), btempw(azStrs,ialloc2))
      allocate(bTsed(azStrs,ialloc2), bso(azStrs,ialloc2), blb(azStrs,ialloc2), bleb(azStrs,ialloc2))
      allocate(bno3(azStrs,ialloc2), bnh4(azStrs,ialloc2), bgelp(azStrs,ialloc2), bsvhek(azStrs,ialloc2))
      allocate(bgesN(azStrs,ialloc2), bgesP(azStrs,ialloc2), bsvheg(azStrs,ialloc2), bagbcm(azStrs,ialloc2))
      allocate(bchla(azStrs,ialloc2), bir(azStrs,ialloc2), bssalg(azStrs,ialloc2), bsi(azStrs,ialloc2))
      allocate(bdaki(azStrs,ialloc2), bdaak(azStrs,ialloc2), bsedak(azStrs,ialloc2), bazok(azStrs,ialloc2)) 
      allocate(bdkmor(azStrs,ialloc2), bvkigr(azStrs,ialloc2), bakbcm(azStrs,ialloc2), baki(azStrs,ialloc2))
      allocate(bagr(azStrs,ialloc2), bsised(azStrs,ialloc2), bSKmor(azStrs,ialloc2), bfheau(azStrs,ialloc2))
      allocate(bpfl(azStrs,ialloc2), bakmua(azStrs,ialloc2), bftaau(azStrs,ialloc2), bfiaus(azStrs,ialloc2))
      allocate(bakrau(azStrs,ialloc2), bbsbt(azStrs,ialloc2), bschlr(azStrs,ialloc2), bbsb(azStrs,ialloc2))
      allocate(bcsb(azStrs,ialloc2), bo2(azStrs,ialloc2), bno2(azStrs,ialloc2), bx0(azStrs,ialloc2))
      allocate(bchlak(azStrs,ialloc2), bchlag(azStrs,ialloc2), babrz1(azStrs,ialloc2), bss(azStrs,ialloc2))
      allocate(bzooi(azStrs,ialloc2), bmw(azStrs,ialloc2), bpw(azStrs,ialloc2), bvcsb(azStrs,ialloc2))
      allocate(bca(azStrs,ialloc2), blf(azStrs,ialloc2), bph(azStrs,ialloc2), bvbsb(azStrs,ialloc2), babewk(azStrs,ialloc2)) 
      allocate(bdlarn(azStrs,ialloc2), bx02(azStrs,ialloc2), bstind(azStrs,ialloc2), bdagr(azStrs,ialloc2))
      allocate(bdaag(azStrs,ialloc2), bsedag(azStrs,ialloc2), bazog(azStrs,ialloc2), bdgmor(azStrs,ialloc2))
      allocate(babewg(azStrs,ialloc2), baberg(azStrs,ialloc2), baberk(azStrs,ialloc2), bresdr(azStrs,ialloc2))
      allocate(badrk(azStrs,ialloc2), badrg(azStrs,ialloc2), bacok(azStrs,ialloc2), bacog(azStrs,ialloc2))
      allocate(bacob(azStrs,ialloc2), badrb(azStrs,ialloc2), bagmua(azStrs,ialloc2), bfigas(azStrs,ialloc2)) 
      allocate(bfhgau(azStrs,ialloc2), bagrau(azStrs,ialloc2), babszo(azStrs,ialloc2), bzres1(azStrs,ialloc2))
      allocate(bzres2(azStrs,ialloc2), bzexki(azStrs,ialloc2), bzexgr(azStrs,ialloc2), brmuas(azStrs,ialloc2))
      allocate(bzexbl(azStrs,ialloc2), biras(azStrs,ialloc2), brakr(azStrs,ialloc2), brbar(azStrs,ialloc2))
      allocate(bfssgr(azStrs,ialloc2), bfbsgr(azStrs,ialloc2), bfrfgr(azStrs,ialloc2), bexdvk(azStrs,ialloc2))
      allocate(bexdvg(azStrs,ialloc2), bsgon(azStrs,ialloc2), bsedx0(azStrs,ialloc2), bexdvb(azStrs,ialloc2))
      allocate(bdon(azStrs,ialloc2), bsusn(azStrs,ialloc2), bbettn(azStrs,ialloc2), bsuso(azStrs,ialloc2))
      allocate(bagn4(azStrs,ialloc2), bakn4(azStrs,ialloc2), bagn3(azStrs,ialloc2), babn4(azStrs,ialloc2))
      allocate(babn3(azStrs,ialloc2), bakn3(azStrs,ialloc2), bsedn(azStrs,ialloc2), bBVHNF(azStrs,ialloc2))
      allocate(bsdbsb(azStrs,ialloc2), bbsbbe(azStrs,ialloc2), bdfaek(azStrs,ialloc2), bdfaeg(azStrs,ialloc2))
      allocate(bdfaeb(azStrs,ialloc2), bdfaes(azStrs,ialloc2), bssdr(azStrs,ialloc2), borgCs(azStrs,ialloc2))
      allocate(borgCs_abb(azStrs,ialloc2), bbsbct(azStrs,ialloc2), bbsbcP(azStrs,ialloc2), bcm(azStrs,ialloc2))
      allocate(bBAC(azStrs,ialloc2), bHNFBS(azStrs,ialloc2), bBSBHN(azStrs,ialloc2), bCHNF(azStrs,ialloc2))
      allocate(bnl0(azStrs,ialloc2), bpl0(azStrs,ialloc2), bgo2n(azStrs,ialloc2), bpo2p(azStrs,ialloc2))
      allocate(bpo2r(azStrs,ialloc2), bro2dr(azStrs,ialloc2), bro2HF(azStrs,ialloc2), borgSS(azStrs,ialloc2))
      allocate(bJNO3(azStrs,ialloc2), bJNH4(azStrs,ialloc2), bJSi(azStrs,ialloc2), bJPO4(azStrs,ialloc2))
      allocate(bJO2(azStrs,ialloc2), bsedSS(azStrs,ialloc2), babbcm(azStrs,ialloc2), babl(azStrs,ialloc2))
      allocate(bchlab(azStrs,ialloc2), bantbl(azStrs,ialloc2), bsvheb(azStrs,ialloc2),bTGZoo(azStrs,ialloc2))
      allocate(bakmor_1(azStrs,ialloc2), bagmor_1(azStrs,ialloc2), babmor_1(azStrs,ialloc2))
      allocate(btpki(azStrs,ialloc2), btpgr(azStrs,ialloc2), bextk(azStrs,ialloc2), bQ_PK(azStrs,ialloc2))
      allocate(bQ_NK(azStrs,ialloc2), bQ_SK(azStrs,ialloc2), bQ_PG(azStrs,ialloc2), bQ_NG(azStrs,ialloc2))
      allocate(bQ_PB(azStrs,ialloc2), bQ_NB(azStrs,ialloc2), bFluN3(azStrs,ialloc2), bdabl(azStrs,ialloc2))
      allocate(bdaab(azStrs,ialloc2), bsedab(azStrs,ialloc2), bazob(azStrs,ialloc2), bdbmor(azStrs,ialloc2))
      allocate(babmua(azStrs,ialloc2), bfibas(azStrs,ialloc2), bfhbau(azStrs,ialloc2), babrau(azStrs,ialloc2))
      allocate(btpbl(azStrs,ialloc2), bup_PB(azStrs,ialloc2), bup_NB(azStrs,ialloc2), babtbr(azStrs,ialloc2))
      allocate(balgbz(azStrs,ialloc2), balabz(azStrs,ialloc2), bup_PK(azStrs,ialloc2), bup_NK(azStrs,ialloc2))
      allocate(bup_Si(azStrs,ialloc2), baktbr(azStrs,ialloc2), bup_PG(azStrs,ialloc2), bup_NG(azStrs,ialloc2))
      allocate(bagtbr(azStrs,ialloc2), balgkz(azStrs,ialloc2), balakz(azStrs,ialloc2), balggz(azStrs,ialloc2))
      allocate(balagz(azStrs,ialloc2), bkN4z(azStrs,ialloc2), bkN3z(azStrs,ialloc2), bgN4z(azStrs,ialloc2))
      allocate(bgN3z(azStrs,ialloc2), bbN4z(azStrs,ialloc2), bbN3z(azStrs,ialloc2), bsedAlg_MQ(azStrs,ialloc2))
      allocate(bsedSS_MQ(azStrs,ialloc2), bcoli(azStrs,ialloc2), bDOSCF(azStrs,ialloc2))

      allocate(bgsZn(azStrs,ialloc2), bglZn(azStrs,ialloc2), bgsCad(azStrs,ialloc2), bglCad(azStrs,ialloc2))
      allocate(bgsCu(azStrs,ialloc2), bglCu(azStrs,ialloc2), bgsNi(azStrs,ialloc2), bglNi(azStrs,ialloc2))

      allocate(bste(azStrs,ialloc2), bsno3(azStrs,ialloc2), bsn4(azStrs,ialloc2), bsgelp(azStrs,ialloc2))
      allocate(bsno2(azStrs,ialloc2), bschla(azStrs,ialloc2), bsssal(azStrs,ialloc2), bssi(azStrs,ialloc2))
      allocate(bszooi(azStrs,ialloc2), bsvbsb(azStrs,ialloc2), bsvcsb(azStrs,ialloc2), bsgsP(azStrs,ialloc2))
      allocate(bsgsN(azStrs,ialloc2), bsaki(azStrs,ialloc2), bsagr(azStrs,ialloc2), bsabl(azStrs,ialloc2))
      allocate(bsFlN3(azStrs,ialloc2), bso2(azStrs,ialloc2), bsmw(azStrs,ialloc2), bslf(azStrs,ialloc2))
      allocate(bsca(azStrs,ialloc2), bsph(azStrs,ialloc2), bsnl0(azStrs,ialloc2), bspl0(azStrs,ialloc2))
      allocate(bsdalg(azStrs,ialloc2), bsvkg(azStrs,ialloc2), bsdaa(azStrs,ialloc2), bsseda(azStrs,ialloc2))
      allocate(bsalgz(azStrs,ialloc2), bsamor(azStrs,ialloc2), bsadr(azStrs,ialloc2), bsalco(azStrs,ialloc2))
      allocate(bsfik(azStrs,ialloc2), bsfig(azStrs,ialloc2), bskmue(azStrs,ialloc2), bsgmue(azStrs,ialloc2))
      allocate(bshek(azStrs,ialloc2), bsheg(azStrs,ialloc2), bskre(azStrs,ialloc2), bsgre(azStrs,ialloc2))
      allocate(bschlk(azStrs,ialloc2), bschlg(azStrs,ialloc2), bsbmue(azStrs,ialloc2), bsheb(azStrs,ialloc2))
      allocate(bsbre(azStrs,ialloc2), bschlb(azStrs,ialloc2), bsantb(azStrs,ialloc2), bsbetN(azStrs,ialloc2)) 
      allocate(bsJNO3(azStrs,ialloc2), bsJNH4(azStrs,ialloc2), bsJPO4(azStrs,ialloc2), bsJO2(azStrs,ialloc2))
      allocate(bsJSi(azStrs,ialloc2), bscoli(azStrs,ialloc2), sedhy(azStrs,ialloc2))
      allocate(bsgsZn(azStrs,ialloc2), bsglZn(azStrs,ialloc2), bsgsCad(azStrs,ialloc2), bsglCad(azStrs,ialloc2))
      allocate(bsgsCu(azStrs,ialloc2), bsglCu(azStrs,ialloc2), bsgsNi(azStrs,ialloc2), bsglNi(azStrs,ialloc2))
      allocate(bmxtem(azStrs,ialloc2), bmitem(azStrs,ialloc2), bmxno3(azStrs,ialloc2), bmino3(azStrs,ialloc2))
      allocate(bmxnh4(azStrs,ialloc2), bminh4(azStrs,ialloc2), bmxglp(azStrs,ialloc2), bmiglp(azStrs,ialloc2))
      allocate(bmxchl(azStrs,ialloc2), bmichl(azStrs,ialloc2), bmxssa(azStrs,ialloc2), bmissa(azStrs,ialloc2))
      allocate(bmxsi(azStrs,ialloc2), bmisi(azStrs,ialloc2), bmxzoo(azStrs,ialloc2), bmizoo(azStrs,ialloc2))
      allocate(bmxno2(azStrs,ialloc2), bmino2(azStrs,ialloc2), bmibsb(azStrs,ialloc2), bmxbsb(azStrs,ialloc2))
      allocate(bmicsb(azStrs,ialloc2), bmxcsb(azStrs,ialloc2), bmxgsP(azStrs,ialloc2), bmigsP(azStrs,ialloc2))
      allocate(bmxgsN(azStrs,ialloc2), bmigsN(azStrs,ialloc2), bmxaki(azStrs,ialloc2), bmiaki(azStrs,ialloc2))
      allocate(bmxagr(azStrs,ialloc2), bmiagr(azStrs,ialloc2), bmio2(azStrs,ialloc2), bmxo2(azStrs,ialloc2))
      allocate(bmxmw(azStrs,ialloc2), bmimw(azStrs,ialloc2), bmxlf(azStrs,ialloc2), bmilf(azStrs,ialloc2))
      allocate(bmxca(azStrs,ialloc2), bmica(azStrs,ialloc2), bmxph(azStrs,ialloc2), bmiph(azStrs,ialloc2))
      allocate(bnaehr(azStrs,ialloc2))

      allocate(bmxgsZn(azStrs,ialloc2), bmigsZn(azStrs,ialloc2), bmxglZn(azStrs,ialloc2), bmiglZn(azStrs,ialloc2))
      allocate(bmxgsCad(azStrs,ialloc2), bmigsCad(azStrs,ialloc2), bmxglCad(azStrs,ialloc2), bmiglCad(azStrs,ialloc2))
      allocate(bmxgsCu(azStrs,ialloc2), bmigsCu(azStrs,ialloc2), bmxglCu(azStrs,ialloc2), bmiglCu(azStrs,ialloc2))
      allocate(bmxgsNi(azStrs,ialloc2), bmigsNi(azStrs,ialloc2), bmxglNi(azStrs,ialloc2), bmiglNi(azStrs,ialloc2))

      allocate(hfkm(azStrs,ialloc2), hqaus(azStrs,ialloc2), hsvhk(azStrs,ialloc2), hsvhg(azStrs,ialloc2))
      allocate(hDOSCF(azStrs,ialloc2), hsvhb(azStrs,ialloc2), habbcm(azStrs,ialloc2), habl(azStrs,ialloc2))
      allocate(hchlab(azStrs,ialloc2), hantbl(azStrs,ialloc2), htempw(azStrs,ialloc2), hTsed(azStrs,ialloc2))
      allocate(hbsb(azStrs,ialloc2), hcsb(azStrs,ialloc2), hnh4(azStrs,ialloc2), hCM(azStrs,ialloc2))
      allocate(hBAC(azStrs,ialloc2), ho2(azStrs,ialloc2), hno3(azStrs,ialloc2), hno2(azStrs,ialloc2))
      allocate(hx0(azStrs,ialloc2), hsi(azStrs,ialloc2), hx02(azStrs,ialloc2), hcoli(azStrs,ialloc2))
      allocate(hchla(azStrs,ialloc2), hchlak(azStrs,ialloc2), hchlag(azStrs,ialloc2), hvkigr(azStrs,ialloc2))
      allocate(htpki(azStrs,ialloc2), htpgr(azStrs,ialloc2), htpbl(azStrs,ialloc2), hzooi(azStrs,ialloc2))
      allocate(habrz1(azStrs,ialloc2), hssalg(azStrs,ialloc2), hss(azStrs,ialloc2), hgelp(azStrs,ialloc2))
      allocate(hmw(azStrs,ialloc2), hpw(azStrs,ialloc2), hca(azStrs,ialloc2), hlf(azStrs,ialloc2))
      allocate(hph(azStrs,ialloc2), hvbsb(azStrs,ialloc2), hvcsb(azStrs,ialloc2), haki(azStrs,ialloc2))
      allocate(hstind(azStrs,ialloc2), hagr(azStrs,ialloc2), hakbcm(azStrs,ialloc2), hagbcm(azStrs,ialloc2))
      allocate(hCHNF(azStrs,ialloc2), hBVHNF(azStrs,ialloc2), hHNFBA(azStrs,ialloc2), hfssgr(azStrs,ialloc2))
      allocate(hfbsgr(azStrs,ialloc2), hnl0(azStrs,ialloc2), hQ_NK(azStrs,ialloc2), hQ_PK(azStrs,ialloc2))
      allocate(hQ_SK(azStrs,ialloc2), hQ_NG(azStrs,ialloc2), hQ_PG(azStrs,ialloc2), hQ_NB(azStrs,ialloc2))
      allocate(hQ_PB(azStrs,ialloc2), hpl0(azStrs,ialloc2), hfrfgr(azStrs,ialloc2), hffood(azStrs,ialloc2))
      allocate(hdl(azStrs,ialloc2), htau2(azStrs,ialloc2), hgesP(azStrs,ialloc2), hgesN(azStrs,ialloc2))
      allocate(hCD1(azStrs,ialloc2), hCD2(azStrs,ialloc2), hCP1(azStrs,ialloc2), hCP2(azStrs,ialloc2))
      allocate(hvo2(azStrs,ialloc2), hextk(azStrs,ialloc2), hJNO3(azStrs,ialloc2), hJNH4(azStrs,ialloc2))
      allocate(hJPO4(azStrs,ialloc2), hJSi(azStrs,ialloc2), hJO2(azStrs,ialloc2), hFluN3(azStrs,ialloc2))
      allocate(SedOM(azStrs,ialloc2), BedGSed(azStrs,ialloc2), SPEWKSuS(azStrs,ialloc2), WUEBKuS(azStrs,ialloc2))
      allocate(extkuS(azStrs,ialloc2), Sedvvert(azStrs,ialloc2), hJN2(azStrs,ialloc2), bJN2(azStrs,ialloc2))
      allocate(PSREFSuS(azStrs,ialloc2), SPEWKSS(azStrs,ialloc2), WUEBKS(azStrs,ialloc2), PSREFSS(azStrs,ialloc2))
      allocate(extkS(azStrs,ialloc2), Stakm(azStrs,ialloc2), Raua(azStrs,ialloc2), bsohla(azStrs,ialloc2), hlboea(azStrs,ialloc2))
      allocate(hflaea(azStrs,ialloc2), htiefa(azStrs,ialloc2), hvF(azStrs,ialloc2), hWS(azStrs,ialloc2))
      allocate(helen(azStrs,ialloc2), hvmitt(azStrs,ialloc2), htiefe(azStrs,ialloc2), hrau(azStrs,ialloc2))
      allocate(hrhyd(azStrs,ialloc2), hflae(azStrs,ialloc2), hpfmnl(azStrs,ialloc2), hpfmxl(azStrs,ialloc2))
      allocate(habgml(azStrs,ialloc2), hlboem(azStrs,ialloc2), hbsohl(azStrs,ialloc2), hvabfl(azStrs,ialloc2))
      allocate(VALTLH(azStrs,ialloc2), EDUFLH(azStrs,ialloc2), VALTRH(azStrs,ialloc2), EDUFRH(azStrs,ialloc2))
      allocate(habkml(azStrs,ialloc2), hdlmx(azStrs,ialloc2), hdlmxs(azStrs,ialloc2), hgwdmx(azStrs,ialloc2))
      allocate(hsgwmu(azStrs,ialloc2), hdH2De(azStrs,ialloc2), Hmax2D(azStrs,ialloc2), TGZoo(azStrs,ialloc2))
      allocate(akmor_1(azStrs,ialloc2), agmor_1(azStrs,ialloc2), abmor_1(azStrs,ialloc2))

      allocate(hglZn(azStrs,ialloc2), hgsZn(azStrs,ialloc2), hglCad(azStrs,ialloc2), hgsCad(azStrs,ialloc2))
      allocate(hglCu(azStrs,ialloc2), hgsCu(azStrs,ialloc2), hglNi(azStrs,ialloc2), hgsNi(azStrs,ialloc2))

      allocate(RBkm(azStrs,ialloc1), RBkmLe(azStrs,ialloc1), RBkm1(azStrs,ialloc1), WirkLL(azStrs,ialloc1))
      allocate(abfls(azStrs,ialloc1), obsbs(azStrs,ialloc1), ocsbs(azStrs,ialloc1), vnh4s(azStrs,ialloc1))
      allocate(vno2s(azStrs,ialloc1), vno3s(azStrs,ialloc1), gesNs(azStrs,ialloc1), vx0s(azStrs,ialloc1))
      allocate(vx02s(azStrs,ialloc1), gelps(azStrs,ialloc1), gesPs(azStrs,ialloc1), sis(azStrs,ialloc1))
      allocate(chlas(azStrs,ialloc1), vkigrs(azStrs,ialloc1), antbls(azStrs,ialloc1), zooins(azStrs,ialloc1))
      allocate(vphs(azStrs,ialloc1), mws(azStrs,ialloc1), pws(azStrs,ialloc1), cas(azStrs,ialloc1), lfs(azStrs,ialloc1))
      allocate(ssalgs(azStrs,ialloc1), tempws(azStrs,ialloc1), vo2s(azStrs,ialloc1), CHNFs(azStrs,ialloc1))
      allocate(BVHNFs(azStrs,ialloc1), colis(azStrs,ialloc1), DOSCFs(azStrs,ialloc1) ,waers(azStrs,ialloc1), akis(azStrs,ialloc1))
      allocate(agrs(azStrs,ialloc1), abls(azStrs,ialloc1), agbcms(azStrs,ialloc1), akbcms(azStrs,ialloc1))
      allocate(abbcms(azStrs,ialloc1), frfgrs(azStrs,ialloc1), CMS(azStrs,ialloc1), BACs(azStrs,ialloc1))
      allocate(nl0s(azStrs,ialloc1), pl0s(azStrs,ialloc1), sss(azStrs,ialloc1), Chlaks(azStrs,ialloc1))
      allocate(chlabs(azStrs,ialloc1), chlags(azStrs,ialloc1), vbsbs(azStrs,ialloc1), vcsbs(azStrs,ialloc1))
      allocate(Q_NKs(azStrs,ialloc1), Q_PKs(azStrs,ialloc1), Q_SKs(azStrs,ialloc1), Q_NGs(azStrs,ialloc1))
      allocate(Q_PGs(azStrs,ialloc1), Q_NBs(azStrs,ialloc1), Q_PBs(azStrs,ialloc1)) 

      allocate(glZns(azStrs,ialloc1), gsZns(azStrs,ialloc1), glCads(azStrs,ialloc1), gsCads(azStrs,ialloc1)) 
      allocate(glCus(azStrs,ialloc1), gsCus(azStrs,ialloc1), glNis(azStrs,ialloc1), gsNis(azStrs,ialloc1)) 

      allocate(einlkh(azStrs,ialloc1), qeinlh(azStrs,ialloc1), ebsbh(azStrs,ialloc1), ecsbh(azStrs,ialloc1))
      allocate(enh4h(azStrs,ialloc1), ex0h(azStrs,ialloc1), eo2h(azStrs,ialloc1), etemph(azStrs,ialloc1))
      allocate(echlah(azStrs,ialloc1), ezindh(azStrs,ialloc1), egph(azStrs,ialloc1), esih(azStrs,ialloc1))
      allocate(eno3h(azStrs,ialloc1), essh(azStrs,ialloc1), ewaerh(azStrs,ialloc1), enl0h(azStrs,ialloc1))
      allocate(epl0h(azStrs,ialloc1), ephh(azStrs,ialloc1), emwh(azStrs,ialloc1), elfh(azStrs,ialloc1))
      allocate(ecah(azStrs,ialloc1), ex02h(azStrs,ialloc1), eno2h(azStrs,ialloc1), eCHNFh(azStrs,ialloc1))
      allocate(eBVHNh(azStrs,ialloc1), egesNh(azStrs,ialloc1), egesPh(azStrs,ialloc1), ecolih(azStrs,ialloc1))
      allocate(evkgh(azStrs,ialloc1), eantbh(azStrs,ialloc1), eCM(azStrs,ialloc1), eBAC(azStrs,ialloc1))

      allocate(egsZn(azStrs,ialloc1), eglZn(azStrs,ialloc1), egsCad(azStrs,ialloc1), eglCad(azStrs,ialloc1))
      allocate(egsCu(azStrs,ialloc1), eglCu(azStrs,ialloc1), egsNi(azStrs,ialloc1), eglNi(azStrs,ialloc1))

      allocate(qLh(azStrs,ialloc1), bsbLh(azStrs,ialloc1), csbLh(azStrs,ialloc1), enh4Lh(azStrs,ialloc1))
      allocate(eno2Lh(azStrs,ialloc1), eno3Lh(azStrs,ialloc1), gesNLh(azStrs,ialloc1), x0Lh(azStrs,ialloc1))
      allocate(x02Lh(azStrs,ialloc1), gpLh(azStrs,ialloc1), gesPLh(azStrs,ialloc1), siLh(azStrs,ialloc1))
      allocate(phLh(azStrs,ialloc1), caLh(azStrs,ialloc1), elfLh(azStrs,ialloc1), ssLh(azStrs,ialloc1))
      allocate(tempLh(azStrs,ialloc1), o2Lh(azStrs,ialloc1), coliLh(azStrs,ialloc1), enl0Lh(azStrs,ialloc1))
      allocate(pl0Lh(azStrs,ialloc1), chlaLh(azStrs,ialloc1), CML(azStrs,ialloc1), BACL(azStrs,ialloc1))
      allocate(afkm2D(azStrs,ialloc1),efkm2D(azStrs,ialloc1))

      allocate(apfl(azStrs,20), epfl(azStrs,20), pflmxs(azStrs,20), pflmis(azStrs,20), aschif(azStrs,20))
      allocate(eschif(azStrs,20), awett(azStrs,20), ewett(azStrs,20), abal(azStrs,20), ebal(azStrs,20))
      allocate(ggbal(azStrs,20), gkbal(azStrs,20), akdrei(azStrs,20), ekdrei(azStrs,20), aPOM(azStrs,20))
      allocate(ePOM(azStrs,20), POMz(azStrs,20), BedGSz(azStrs,20), sedvvertz(azStrs,20), acoro(azStrs,20), ecoro(azStrs,20))
      allocate(coro1s(azStrs,20), aKSED(azStrs,20), eKSED(azStrs,20), SPEWKSx(azStrs,20), WUEBKx(azStrs,20), extkx(azStrs,20))
      allocate(PSREFSx(azStrs,20), coross(azStrs,20), aVEG(azStrs,20), eVEG(azStrs,20), VALTAL(azStrs,20))
      allocate(EDUFAL(azStrs,20), VALTAR(azStrs,20), EDUFAR(azStrs,20))

      allocate(ho2z_z(azStrs,50), htez_z(azStrs,50), hchlaz_z(azStrs,50), hakiz_z(azStrs,50), hagrz_z(azStrs,50))
      allocate(hablz_z(azStrs,50), hNh4z_z(azStrs,50), hNO2z_z(azStrs,50), hNO3z_z(azStrs,50), hPz_z(azStrs,50))
      allocate(hSiz_z(azStrs,50), hchlkz_z(azStrs,50), hchlgz_z(azStrs,50), hchlbz_z(azStrs,50), hgesPz_z(azStrs,50))
      allocate(hgesNz_z(azStrs,50), hQ_NKz_z(azStrs,50), hQ_NBz_z(azStrs,50), hQ_NGz_z(azStrs,50)) 
      allocate(hCChlkz_z(azStrs,50), hCChlbz_z(azStrs,50), hCChlgz_z(azStrs,50))

      allocate(bcd(azStrs,2,ialloc2), bcp(azStrs,2,ialloc2), hCD(azStrs,2,ialloc2),hCP(azStrs,2,ialloc2))
      allocate(CDs(azStrs,2,ialloc1),CPs(azStrs,2,ialloc1), eCD(azStrs,2,ialloc1),eCP(azStrs,2,ialloc1))
      allocate(CDL(azStrs,2,ialloc1),CPL(azStrs,2,ialloc1))

      allocate(wstand(azStrs,100,27), zdrs(azStrs,20,4), zdrss(azStrs,20,4), gwdrs(azStrs,20,4))
      allocate(hzdrel(azStrs,ialloc2,4),hzdrsl(azStrs,ialloc2,4), hgwdrl(azStrs,ialloc2,4))

      allocate(hnh4z(azStrs,50,ialloc2), hno2z(azStrs,50,ialloc2), hno3z(azStrs,50,ialloc2), ho2z(azStrs,50,ialloc2))
      allocate(hgelPz(azStrs,50,ialloc2), hgesPz(azStrs,50,ialloc2), hgesNz(azStrs,50,ialloc2), hsiz(azStrs,50,ialloc2))
      allocate(hakiz(azStrs,50,ialloc2), hagrz(azStrs,50,ialloc2), hablz(azStrs,50,ialloc2), hchlaz(azStrs,50,ialloc2))
      allocate(hchlkz(azStrs,50,ialloc2), hchlgz(azStrs,50,ialloc2), hchlbz(azStrs,50,ialloc2), htempz(azStrs,50,ialloc2))
      allocate(hQ_NKz(azStrs,50,ialloc2), hQ_NBz(azStrs,50,ialloc2), hQ_NGz(azStrs,50,ialloc2), hCChlkz(azStrs,50,ialloc2))
      allocate(hCChlbz(azStrs,50,ialloc2), hCChlgz(azStrs,50,ialloc2))

      allocate(Tzt(azStrs,50,2), o2zt(azStrs,50,2), NH4zt(azStrs,50,2), NO2zt(azStrs,50,2), NO3zt(azStrs,50,2))
      allocate(Pzt(azStrs,50,2), gSizt(azStrs,50,2), akizt(azStrs,50,2), agrzt(azStrs,50,2), ablzt(azStrs,50,2))
      allocate(chlazt(azStrs,50,2), chlkzt(azStrs,50,2), chlgzt(azStrs,50,2), chlbzt(azStrs,50,2), gesPzt(azStrs,50,2))
      allocate(gesNzt(azStrs,50,2), Q_NKzt(azStrs,50,2), Q_NBzt(azStrs,50,2), Q_NGzt(azStrs,50,2))
      allocate(CChlkzt(azStrs,50,2), CChlbzt(azStrs,50,2), CChlgzt(azStrs,50,2)) 
      

      allocate(VTYPA(azStrs,20,18), hcoro2(azStrs,ialloc2,5), hcos2(azStrs,ialloc2,5), VTYPH(azStrs,ialloc2,14))

!###################################################################


! #### Setzen der Ymax/Ymin-Werte zur grafischen Darstellung unter Gerris #####  
      do j = 1,azStrs
        do jj = 1,250 
          Ymax(j,jj) = 0.0 
          Ymin(j,jj) = 999999.99 
        enddo 
      enddo

      isumAnzSta = 0
      do azStr = 1,azStrs                         ! Beginn Strangschleife  10=MODELLA.txt
      ieinL = 0 
      read(10,'(I5,2x,I5,2x,I5,2x,I5,2x,I5)')mstr,mStas(mstr),mRBs(mstr),mwehr(mstr),STRID(mstr)               
      mstra(azStr) = mstr
      isumAnzSta = isumAnzSta+mStas(mstr)

      do mSta = 1,mStas(mstr) 
        read(10,1001)Stakm(mstr,mSta),Raua(mstr,mSta),bsohla(mstr,mSta),boeamq(mstr,mSta),vmq(mstr,mSta)          &
                    ,Hmq(mstr,mSta),bvmq(mstr,mSta),bHmq(mstr,mSta)                   
      enddo 
                                                                       
      if(mwehr(mstr)==0)then 
        else
          read(10,1002)wehrh(mstr),wehrb(mstr)
      endif 
                                                                       
      startkm(mstr) = Stakm(mstr,1) 
      endkm(mstr) = Stakm(mstr,mStas(mstr)) 
      iRB_K1(mstr) = 0
      izufluss(mstr) = 0                     
      do mRB = 1,mRBs(mstr) 
        read(10,1003)RBNR,RBkm(mstr,mRB),RBtyp(mstr,mRB),Weinl(mstr,mRB),mstrLe(mstr,mRB),RBkmLe(mstr,mRB),cEName(mstr,mRB) 

      if(RBkm(mstr,mRB)==Stakm(mstr,1).and.mstrLe(mstr,mRB)<0)then
        RBtyp(mstr,mRB) = 0
        iRB_K1(mstr) = iRB_K1(mstr) + 1
        imRB_K1(iRB_K1(mstr)) = mRB
      endif

      if(RBkm(mstr,mRB)==endkm(mstr).and.mstrLe(mstr,mRB)<0.and.RBtyp(mstr,mRB)==1)then
        RBtyp(mstr,mRB) = 2
        iRB_K1(mstr) = iRB_K1(mstr) + 1
        imRB_K1(iRB_K1(mstr)) = mRB
      endif

     izufluss(mstr) = izufluss(mstr) + 1                                                                      
      
!......Bestimmung der Wirklänge der Diffusen Einleitung                 
                                                                       
      if(mstrLe(mstr,mRB)<0)cycle 
      ieinL = ieinL+1 
      WirkLL(mstr,ieinL) = abs(RBkm(mstr,mRB)-RBkmLe(mstr,mRB))*1000.                                                            
   enddo
 
      if(startkm(mstr).gt.endkm(mstr))abfr(mstr) = 0 
      if(startkm(mstr).lt.endkm(mstr))abfr(mstr) = 1 
      if(abfr(mstr).eq.0)cycle 

    enddo          ! Ende Strangschleife
 
                                                                       
 1001 format(f8.3,2x,f5.2,2x,f7.2,2x,f7.2,2x,f8.5,2x,f7.4               &
     &,2x,f8.5,2x,f7.4)                                                 
 1002 format(f7.2,2x,f7.2) 
 1003 format(I5,2x,f8.3,2x,i1,2x,i1,2x,I5,2x,F8.3,6x,50a) 
!.................................                                      
      close (10) 
                                                                       
                                                                       
!...Ermittlung der Berechnungsgitterpunkte                              
                                                                        
      call km_sys(azStrs,mstra,StaKm,RBkm,RBkmLe,RBtyp,mRBs             &
     &,mWehr,mStas,iorLah,iorLeh,mstrLe,abfr,cpfad)                  

!                                                                       
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'km_sys.dat' 
      open(unit=391, file=pfadstring)
      rewind (391) 
      do azStr = 1,azStrs 
        mstr = mstra(azStr) 
        read(391,'(I5,2x,I5)')mstr,isegs(mstr) 
          do iseg = 1,isegs(mstr) 
            read(391,'(f9.4,2x,i1)')segkm(mstr,iseg),hflag(mstr,iseg) 
          enddo 
       enddo
 
      close (391) 

                                                                       
!.....Einlesen aus EREIGG                                               
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'EREIGG.txt'
      open(unit=92, file=pfadstring)
      rewind (92) 
!                                                                       
      read(92,'(A2)')ckenn_vers1
        if(ckenn_vers1/='*V')then
          read(92,'(a255)')cEreig 
          read(92,9200)itags,monats,jahrs,uhrs 
          read(92,9210)itage,monate,jahre,uhren,izdt  
          read(92,9220)imitt,ipH,idl,itemp,itracer,ieros                             &
          ,ischwa,iverfahren,ilongDis,FlongDis
        else
             read(92,'(a50)')modell 
             read(92,'(a255)')cEreig 
             read(92,9200)itags,monats,jahrs,uhrs 
             read(92,9210)itage,monate,jahre,uhren,izdt 
             read(92,9220,iostat=read_error)imitt,ipH,idl,itemp,itracer,ieros        &
             ,ischwa,iverfahren,ilongDis,FlongDis,iColi,ikonsS,iSchwer               &
             ,iphy,iformVert
             if(read_error.ne. 0)then ! 
                ifehl = 33 
                goto 989 
             endif
        endif

        if(iverfahren==0)iverfahren = 1
        if(ilongDis==0)ilongDis = 1
        if(FlongDis==0.0)FlongDis = 1.

      mtracer = 1

      if(itemp.eq.0.and.itracer.eq.0)iwsim = 3 
      if(itemp.eq.1)iwsim = 2 
      if(itracer==1)then
        iwsim = 4
        mtracer = 0
      endif

      if(icoli==1)iwsim = 2
      if(ikonsS==1)iwsim = 5
      if(iSchwer==1)then
        iwsim = 3
        ipH = 1
      endif
                                                                       
      uhren = int(uhren)+((uhren-int(uhren))/0.6) 
                                                                       
      close (92) 

      if((iphy<1).or.(iphy>4))then !!wy
        ifehl = 34 
        goto 989 !! eror exit
      endif

 9200 Format(I2,2x,I2,2x,I4,2x,f5.2) 
 9210 format(I2,2x,I2,2x,I4,2x,f5.2,2x,I3) 
 9220 format(I1,2x,I1,2x,I1,2x,I1,2x,I1,2x,i1,2x,I1,2x,I1,2x,I1,2x,f4.2,2x,I1,2x,I1,2x,I1,2x,I1,2x,I1) 
                                                                       
                                                                       
      if(iwsim==4.or.iwsim==5)goto 329
      if(iwsim==2.and.icoli==0)goto 329 
                                                                       
!    Einlesen der Konstanten                                            
                                                                       
!....APARAM.txt                                                         
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'APARAM.txt'
      open(unit=55, file=pfadstring)
                                                                        
      read(55,5500,iostat=read_error)agchl,aggmax,IKge,agksn,agksp 
      read(55,5502,iostat=read_error)agremi,frmuge,bsbgr,csbgr,Qmx_NG 
      read(55,5504,iostat=read_error)Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG 
      read(55,5506,iostat=read_error)opgrmi,opgrma,asgre,ToptG,kTemp_Gr
      read(55,5507,iostat=read_error)akchl,akgmax,IKke,akksn,akksp 
      read(55,5508,iostat=read_error)akkssi,akremi,frmuke,bsbki,csbki 
      read(55,5510,iostat=read_error)Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK 
      read(55,5512,iostat=read_error)Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi 
      read(55,5514,iostat=read_error)opkima,askie,ToptK,kTemp_Ki,abchl 
      read(55,5516,iostat=read_error)abgmax,IKbe,abksn,abksp,abremi 
      read(55,5518,iostat=read_error)frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB 
      read(55,5520,iostat=read_error)Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi 
      read(55,5522,iostat=read_error)opblma,asble,ToptB,kTemp_Bl,ifix
      read(55,5524,iostat=read_error)irmaxe,FopIRe,GRote,zresge,zakie 
      read(55,5526,iostat=read_error)zagre,zable,ynmx1e,stks1e,anitrie 
      read(55,5530,iostat=read_error)bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e
      read(55,5528,iostat=read_error)bnmx2e,bnks2e,KNH4e,KapN3e,hyPe 
      read(55,5533,iostat=read_error)hymxDe,KsD1e,KsD2e,KsMe,upBACe 
      read(55,5535,iostat=read_error)YBACe,rsGBACe,FoptDe,upHNFe,BACkse 
      read(55,5538,iostat=read_error)alamda,fPOC1e,fPOC2e,SorpCape,Klange
      read(55,5540,iostat=read_error)KdNh3e,RateCde,etaCde,RateCIe,xnueCe
      read(55,5542,iostat=read_error)RateCGe,RateCSe
                                                                    
      close (55) 

      if(read_error<0)then     ! APARAM.txt fehlt
        ifehl = 3 
        goto 989 
      endif

      if(IKge<0.0.or.IKke<0.0.or.KNH4e<0.0.or.KapN3e<0.0.or.hyPe<0.0.or.ToptG<0.0.or.Klange<0.0.or.ifix.lt.0)then                                
        ifehl = 3 
        goto 989 
      endif 
     if(kTemp_Gr<0.0.or.kTemp_Ki<0.0.or.kTemp_Bl<0.0)then
       ifehl = 3 
       goto 989 
     endif 
     if(iwsim==3.and.irmaxe<-1.or.FopIRe<-1.)then
       ifehl = 3 
        goto 989 
     endif
     if(icoli==1)then
       if(RateCde<0.0.or.etaCde<0.0.or.RateCIe<0.0.or.xnueCe<0.0.or.RateCGe<0.0.or.RateCSe<0.0)then                                                                    
         ifehl = 26 
         goto 989 
       endif
     endif

     if(icoli/=1)then
       if(csbki<1.)then
         ifehl = 27
         goto 989
       endif
     endif

      nZoo = 0.11 
      pZoo = 0.01 
                                                                       
 5500 format(f4.1,2x,f5.2,2x,f6.2,2x,f5.3,2x,f6.4) 
 5502 format(f5.3,2x,f5.2,2x,f6.4,2x,f6.4,2x,f7.5) 
 5504 format(f7.5,2x,f7.5,2x,f7.5,2x,f5.3,2x,f5.3) 
 5506 format(f4.2,2x,f4.2,2x,f5.2,2x,f5.2,2x,f7.5)
 5507 format(f4.1,2x,f5.2,2x,f6.2,2x,f5.3,2x,f6.4) 
 5508 format(f5.3,2x,f5.3,2x,f5.2,2x,f6.4,2x,f6.4) 
 5510 format(f7.5,2x,f7.5,2x,f7.5,2x,f7.5,2x,f7.5) 
 5512 format(f7.5,2x,f5.3,2x,f5.3,2x,f5.3,2x,f4.2) 
 5514 format(f4.2,2x,f5.2,2x,f5.2,2x,f7.5,2x,f5.1) 
 5516 format(f5.2,2x,f6.2,2x,f5.3,2x,f6.4,2x,f5.3) 
 5518 format(f5.2,2x,f6.4,2x,f6.4,2x,f7.5,2x,f7.5) 
 5520 format(f7.5,2x,f7.5,2x,f5.3,2x,f5.3,2x,f4.2) 
 5522 format(f4.2,2x,f5.2,2x,f5.2,2x,f7.5,2x,i2)
 5524 format(f5.2,2x,f5.2,2x,f5.2,2x,f5.3,2x,f5.2) 
 5526 format(f5.2,2x,f5.2,2x,f4.2,2x,f5.2,2x,f4.2) 
 5530 format(f5.2,2x,f5.2,2x,f4.2,2x,f5.2,2x,f4.2) 
 5528 format(f5.2,2x,f5.2,2x,f5.2,2x,f5.2,2x,f6.3) 
 5533 format(f6.3,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3) 
 5535 format(f6.3,2x,f6.3,2x,f5.2,2x,f5.2,2x,f6.4) 
 5538 format(f5.3,2x,f5.2,2x,f5.2,2x,f6.3,2x,f6.3)
 5540 format(f5.2,2x,f6.3,2x,f5.2,2x,f5.2,2x,f6.2)
 5542 format(f5.3,2x,f5.3) 

!     Extinktionskoeffizienten von e_extnct.dat lesen jetzt ausserhalb der Algenroutinen und der Zeitschleife
      call e_extnct_lesen(ilamda,eta,aw,ack,acg,acb,ah,as,al,cpfad) !!wy      extk_lamda                             

  329 tflie = izdt/1440. 
      if(uhrs.le.0.0)uhrz = 0.0 
      if(uhrs.gt.0.0)uhrz = Uhrs 
!                                                                       
!... Umrechnen der Uhrzeit in Dezimalschreibweise hcUhrz                
      hcmin = (Uhrz-int(Uhrz))*100./60. 
      hcUhrz = int(uhrz)+hcmin 
      Uhrz = hcUhrz 
      ij = 1 
!.....Tracer                                                            
      if(iwsim==4)goto 681 
!                                                                       
!......Einlesen aus der Datei EREIGG2.txt, falls ischwa = 1             
      if(ischwa.eq.0)goto 681 
!                                                                       
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'eingenhr.dat'
      open(unit=421, file=pfadstring)
      rewind(421) 
      read(421,'(A2)')ckenn_vers1
      if(ckenn_vers1/='*V')then
        read(421,'(A40)')ERENAME
          else
           read(421,'(A40)')MODNAME 
           read(421,'(A40)')ERENAME
      endif
 
  643 read(421,'(I5,2x,I5)',iostat=read_error)mstr,RBNR 
      if(read_error<0)goto 455
      read(421,4212)RBNR,(wstand(mstr,RBNR,nstpa),nstpa=1,27) 
      goto 643 
 4212 format(f4.1,26(2x,f4.1)) 
  455 close (421) 
!                                                                       
!     Eingangsgenerator-Stundenwerte                                    
                                                                        
!     Einlesen der Werte der Standard-Normalverteilung                  
!                                                                       
      astand(1) = 0.0 
      astand(2) = -0.2 
      astand(3) = -0.5 
      astand(4) = -0.7 
      astand(5) = -1.1 
      astand(6) = -1.6 
      astand(7) = -2.0 
      astand(8) = -1.3 
      astand(9) = -0.9 
      astand(10)= -0.6 
      astand(11) = -0.3 
      astand(12) = -0.1 
      astand(13) = 0.1 
      astand(14) = 0.3 
      astand(15) = 0.6 
      astand(16) = 0.9 
      astand(17) = 1.3 
      astand(18) = 2.0 
      astand(19) = 1.6 
      astand(20) = 1.1 
      astand(21) = 0.7 
      astand(22) = 0.5 
      astand(23) = 0.2 
      astand(24) = 0.0 
                                                                      
  681 continue 
                                                                       
!****************************************************************       
                                                                       
!**** Erstellung des Gitters für ortsfeste Kenngrößen und Organismen****                 
!*****Lesen aus der Datei MODELLG.txt                                   
!                                                                       
      dlmax1 = 0.0 
      dlmax2 = 0.0 
      dlmax3 = 0.0 
      dlmax4 = 0.0 
      coro1 = 0.0 
      i2Daus = 0 

      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'MODELLG.txt' 
      open(unit=103, file=pfadstring)
      rewind(103)
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'temp.dat'
      open(unit=77, file=pfadstring)
      rewind(77)
      read(103,'(A2)')ckenn_vers1
        if(ckenn_vers1/='*V')then
          else
            read(103,'(2x)') 
        endif 
      mstr = 0 
      read(103,2305,err=232)lait1,laim1,laid1 
!                                                                       
      read(103,'(I5)')ihcStr 
      read(103,'(2x)') 
  232 if(mstr.eq.0)goto 6 
!                                                                       
      mPfs(mstr) = mPf 
      mSs(mstr) = mS 
      mDs(mstr) = mD 
      mCs(mstr) = mC 
      mBs(mstr) = mB 
      mUs(mstr) = mU 
      i2Ds(mstr) = mD2 
      mWes(mstr) = mWe 
      mVs(mstr) = mV
      mZs(mstr) = mZ
      mAs(mstr) = mA 
 
!...i2Daus steuert die Ausgabe in ERGEB2D.txt                           
!...nur wenn mindestens in einem Strang 2D gerechnet wird,              
!...wird ERGEB2D.txt erzeugt                                            
!                                                                       
      if(i2Ds(mstr).gt.0)i2Daus = 1 
!                                                                       
    6 continue 
      mPf = 0 
      mS = 0 
      mD = 0 
      mC = 0 
      mB = 0 
      mD2 = 0 
      mU = 0 
      mWe = 0 
      mV = 0
      mZ = 0
      mA = 0
      read(103,'(a1,2x,I5)',iostat=read_error)ckenn,mstr 
      if(read_error/=0)goto 339
      nbuhn(mstr) = 0 
  231 read(103,1030,iostat=read_error)ckenn,ctext
      if(read_error/=0)goto 232
 
      if(ckenn.eq.' ')goto 232 
      write(77,1030)ckenn,ctext 
      rewind (77) 
!                                                                       
      if(ckenn.eq.'L')then 
      read(77,2306)laits(mstr),laims(mstr),laids(mstr) 
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'M')then 
      read(77,1031)itsts(mstr),msts(mstr)                               &
     &,itmaxs(mstr),mmaxs(mstr),itends(mstr),mends(mstr)                
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'P')then 
      mPf = mPf+1 
      read(77,1032)apfl(mstr,mPf),epfl(mstr,mPf)                        &
     &,Pflmis(mstr,mPf),Pflmxs(mstr,mPf)                                
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'F')then 
      mS = mS+1 
      read(77,1033)aschif(mstr,mS),eschif(mstr,mS) 
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'D')then 
      mD = mD+1 
      read(77,1034)akdrei(mstr,mD),ekdrei(mstr,mD)                      &
     &,(zdrs(mstr,mD,ndr),zdrss(mstr,mD,ndr)                            &
     &,gwdrs(mstr,mD,ndr),ndr=1,nndr)                                   
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'C')then 
      mC = mC+1 
      read(77,1035)acoro(mstr,mC),ecoro(mstr,mC)                        &
     &,coro1s(mstr,mC),coross(mstr,mC)                                  
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'B')then 
      mB = mB+1 
      read(77,1036)abal(mstr,mB),ebal(mstr,mB)                          &
     &,ggbal(mstr,mB),gkbal(mstr,mB)                                    
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'V')then 
      mD2 = mD2+1 
      read(77,1037)afkm2D(mstr,mD2),efkm2D(mstr,mD2) 
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'U')then 
      mU = mU+1 
      read(77,1038)akmB(mstr,mU),ekmB(mstr,mU),DlB(mstr,mU)             &
     &,tau2B(mstr,mU),alphaB(mstr,mU),POMzb(mstr,mU)                                   

      nbuhn(mstr) = 1 
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'T')then 
      mWe = mWe+1 
      read(77,1033)aWett(mstr,mWe),eWett(mstr,mWe),ikWSta(mstr,mWe)     &
     &,YWlage(mstr,mWe)                                                 
      rewind (77) 
      goto 231 
      endif 
!                                                                       
      if(ckenn.eq.'O')then 
      mV = mV+1 
      read(77,1040)aVeg(mstr,mV),eVeg(mstr,mV),(VTYPA(mstr,mV,iV)       &
     &,iV=1,6),VALTAL(mstr,mV),EDUFAL(mstr,mV)                          &
     &,(VTYPA(mstr,mV,iV),iV=7,12),VALTAR(mstr,mV),EDUFAR(mstr,mV)      &
     &,(VTYPA(mstr,mV,iV),iV=13,14)                                     
      rewind (77) 
      goto 231 
      endif 

      if(ckenn.eq.'Z')then 
      mZ = mZ+1
      read(77,1045)aPOM(mstr,mZ),ePOM(mstr,mZ),POMz(mstr,mZ),BedGSz(mstr,mz),Sedvvertz(mstr,mz)                                    
      rewind (77) 
      goto 231 
      endif 

      if(ckenn.eq.'S')then 
      mA = mA+1
      read(77,1047)aKSED(mstr,mA),eKSED(mstr,mA),SPEWKSx(mstr,mA),WUEBKx(mstr,mA),PSREFSx(mstr,mA),extkx(mstr,mA)                                    
      rewind (77) 
      goto 231 
      endif 

                                                                       
  339 continue 
      close (77) 
!                                                                       
 1030 format(a1,a200) 
 1031 format(1x,6(2x,i2)) 
 1032 format(3x,f8.3,2x,f8.3,2x,f7.2,2x,f7.2) 
 1033 format(3x,f8.3,2x,f8.3,2x,I4,2x,F6.2,2x,F7.1) 
 1034 format(3x,f8.3,2x,f8.3,2(2x,f7.2,2x,f7.2,2x,f7.3)) 
 1035 format(3x,f8.3,2x,f8.3,2x,f8.1,2x,f8.1) 
 1036 format(3x,f8.3,2x,f8.3,2x,f7.1,2x,f7.1) 
 1037 format(3x,f8.3,2x,f8.3) 
 1038 format(3x,f8.3,2x,f8.3,2x,f7.2,2x,f7.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2) 
 2305 format(i2,2x,i2,2x,i3) 
 2306 format(3xi2,2x,i2,2x,i3) 
 1040 format(3x,f8.3,2x,f8.3,18(2x,f6.2)) 
 1045 format(3x,f8.3,2x,f8.3,2x,f6.2,2x,f5.2,2x,f9.4)
 1047 format(3x,f8.3,2x,f8.3,2x,f6.2,2x,f7.2,2x,f5.2,2x,f5.2)

       do j=1,azStrs
         do jj = 1,1000
           do jjj = 1,14
             VTYPH(j,jj,jjj) = 0.0
           enddo
         enddo
       enddo  

      do 335 azStr = 1,azStrs 
      mstr = mstra(azStr) 
                                                                       
      mSta = 1 
      fkmgit = Stakm(mstr,mSta) 
      mPf = 1 
      mS = 1 
      mD = 1 
      mC = 1 
      mB = 1 
      mWe = 1 
      mV = 1
      mU = 1
      mZ = 1
      mA = 1 
!                                                                       
      do 337 kSta = 0,isegs(mstr) 
      if(kSta.eq.0)goto 343 
!                                                                       
      fkmgit = segkm(mstr,kSta) 
      mSta = mSta+1 
  343 trpmin = 0.0 
      trpmax = 0.0 
      if(mPfs(mstr).eq.0.or.mPf.gt.mPfs(mstr))goto 140 
      if(abfr(mstr).eq.1)goto 761 
!                                                                       
      if(fkmgit.le.apfl(mstr,mPf).and.fkmgit.ge.epfl(mstr,mPf))then 
      trpmin = pflmis(mstr,mPf) 
      trpmax = pflmxs(mstr,mPf) 
      goto 140 
      endif 
!                                                                       
      if(fkmgit.le.epfl(mstr,mPf).and.fkmgit.le.apfl(mstr,mPf+1))then 
  567 mPf = mPf+1 
      if(mPf.gt.mPfs(mstr))goto 140 
      if(epfl(mstr,mPf).gt.fkmgit.and.mPf.lt.mPfs(mstr))goto 567 
      trpmin = pflmis(mstr,mPf) 
      trpmax = pflmxs(mstr,mPf) 
      goto 140 
      endif 
      goto 140 
  761 continue 
      if(fkmgit.ge.apfl(mstr,mPf).and.fkmgit.le.epfl(mstr,mPf))then 
      trpmin = pflmis(mstr,mPf) 
      trpmax = pflmxs(mstr,mPf) 
      goto 140 
      endif 
!                                                                       
      if(fkmgit.ge.epfl(mstr,mPf).and.fkmgit.ge.apfl(mstr,mPf+1))then 
  568 mPf = mPf+1 
      if(mPf.gt.mPfs(mstr))goto 140 
      if(epfl(mstr,mPf).lt.fkmgit.and.mPf.lt.mPfs(mstr))goto 568 
      trpmin = pflmis(mstr,mPf) 
      trpmax = pflmxs(mstr,mPf) 
      goto 140 
      endif 
      goto 140 
!                                                                       
!     Elemente mit benth. Algen                                         
!                                                                       
  140 continue 
!                                                                       
      tggbal = 0.0 
      tgkbal = 0.0 
      if(mBs(mstr).eq.0.or.mB.gt.mBs(mstr))goto 58 
      if(abfr(mstr).eq.1)goto 573 
      if(fkmgit.le.abal(mstr,mB).and.fkmgit.ge.ebal(mstr,mB))then 
      tggbal = ggbal(mstr,mB) 
      tgkbal = gkbal(mstr,mB) 
      goto 58 
      endif 
      if(fkmgit.le.ebal(mstr,mB).and.fkmgit.le.abal(mstr,mB+1))then 
   97 mB = mB+1 
      if(mB.gt.mBs(mstr))goto 58 
      if(ebal(mstr,mB).gt.fkmgit.and.mB.lt.mBs(mstr))goto 97 
      tggbal = ggbal(mstr,mB) 
      tgkbal = gkbal(mstr,mB) 
      goto 58 
      endif 
      goto 58 
!                                                                       
  573 continue 
      if(fkmgit.ge.abal(mstr,mB).and.fkmgit.le.ebal(mstr,mB))then 
      tggbal = ggbal(mstr,mB) 
      tgkbal = gkbal(mstr,mB) 
      goto 58 
      endif 
      if(fkmgit.ge.ebal(mstr,mB).and.fkmgit.ge.abal(mstr,mB+1))then 
   98 mB = mB+1 
      if(mB.gt.mBs(mstr))goto 58 
      if(ebal(mstr,mB).lt.fkmgit.and.mB.lt.mBs(mstr))goto 98 
      tggbal = ggbal(mstr,mB) 
      tgkbal = gkbal(mstr,mB) 
      goto 58 
      endif 
!                                                                       
!     Elemente mit Dreissena                                            
!                                                                       
   58 continue 
!                                                                       
      do 71 ndr=1,nndr 
      zdreie(ndr) = 0.0 
      zdrese(ndr) = 0.0 
   71 gwdre(ndr) = 0.0 
      if(mDs(mstr).eq.0.or.mD.gt.mDs(mstr))goto 59 
      if(abfr(mstr).eq.1)goto 663 
!                                                                       
      if(fkmgit.le.akdrei(mstr,mD).and.fkmgit.ge.ekdrei(mstr,mD))then 
      do 72 ndr=1,nndr 
      zdreie(ndr) = zdrs(mstr,mD,ndr) 
      zdrese(ndr) = zdrss(mstr,mD,ndr) 
   72 gwdre(ndr) = gwdrs(mstr,mD,ndr) 
      goto 59 
      endif 
!                                                                       
      if(fkmgit.le.ekdrei(mstr,mD).and.fkmgit.le.akdrei(mstr,mD+1))then 
  196 mD = mD+1 
      if(mD.gt.mDs(mstr))goto 59 
      if(ekdrei(mstr,mD).gt.fkmgit.and.mD.lt.mDs(mstr))goto 196 
      do 171 ndr=1,nndr 
      zdreie(ndr) = zdrs(mstr,mD,ndr) 
      zdrese(ndr) = zdrss(mstr,mD,ndr) 
  171 gwdre(ndr) = gwdrs(mstr,mD,ndr) 
      goto 59 
      endif 
      goto 59 
!                                                                       
!                                                                       
  663 continue 
      if(fkmgit.ge.akdrei(mstr,mD).and.fkmgit.le.ekdrei(mstr,mD))then 
      do 172 ndr=1,nndr 
      zdreie(ndr) = zdrs(mstr,mD,ndr) 
      zdrese(ndr) = zdrss(mstr,mD,ndr) 
  172 gwdre(ndr) = gwdrs(mstr,mD,ndr) 
      goto 59 
      endif 
!                                                                       
      if(fkmgit.ge.ekdrei(mstr,mD).and.fkmgit.ge.akdrei(mstr,mD+1))then 
  199 mD = mD+1 
      if(mD.gt.mDs(mstr))goto 59 
      if(ekdrei(mstr,mD).lt.fkmgit.and.mD.lt.mDs(mstr))goto 199 
      do 180 ndr=1,nndr 
      zdreie(ndr) = zdrs(mstr,mD,ndr) 
      zdrese(ndr) = zdrss(mstr,mD,ndr) 
  180 gwdre(ndr) = gwdrs(mstr,mD,ndr) 
      endif 
                                                                        
!     Elemente mit Chorophium                                           
!                                                                       
   59 continue 
!                                                                       
      coroe = 0.0 
      corose = 0.0 
      if(mCs(mstr).eq.0.or.mC.gt.mCs(mstr))goto 143 
      if(abfr(mstr).eq.1)goto 763 
!                                                                       
      if(fkmgit.le.acoro(mstr,mC).and.fkmgit.ge.ecoro(mstr,mC))then 
      coroe = coro1s(mstr,mC) 
      corose = coross(mstr,mC) 
      if(coroe.lt.0.0)coroe = 0.0 
      if(corose.lt.0.0)corose = 0.0 
      goto 143 
      endif 
!                                                                       
      if(fkmgit.le.ecoro(mstr,mC).and.fkmgit.le.acoro(mstr,mC+1))then 
  296 mC = mC+1 
      if(mC.gt.mCs(mstr))goto 143 
      if(ecoro(mstr,mC).gt.fkmgit.and.mC.lt.mCs(mstr))goto 296 
      coroe = coro1s(mstr,mC) 
      corose = coross(mstr,mC) 
      if(coroe.lt.0.0)coroe = 0.0 
      if(corose.lt.0.0)corose = 0.0 
      goto 143 
      endif 
      goto 143 
!                                                                       
!                                                                       
  763 continue 
      if(fkmgit.ge.acoro(mstr,mC).and.fkmgit.le.ecoro(mstr,mC))then 
      coroe = coro1s(mstr,mC) 
      corose = coross(mstr,mC) 
      if(coroe.lt.0.0)coroe = 0.0 
      if(corose.lt.0.0)corose = 0.0 
      goto 143 
      endif 
!                                                                       
      if(fkmgit.ge.ecoro(mstr,mC).and.fkmgit.ge.acoro(mstr,mC+1))then 
  299 mC = mC+1 
      if(mC.gt.mCs(mstr))goto 143 
      if(ecoro(mstr,mC).lt.fkmgit.and.mC.lt.mCs(mstr))goto 299 
      coroe = coro1s(mstr,mC) 
      corose = coross(mstr,mC) 
      if(coroe.lt.0.0)coroe = 0.0 
      if(corose.lt.0.0)corose = 0.0 
      endif 
!                                                                       
!                                                                       
  143 continue 
!                                                                       
!     Elemente mit Uferbewuchs                                          

     if(mVs(mstr).eq.0.or.mV.gt.mVs(mstr))goto 848 
      if(abfr(mstr).eq.1)goto 840 
                                                                       
      if(fkmgit.le.aVEG(mstr,mV).and.fkmgit.ge.eVEG(mstr,mV))then 
      Do 841 iV = 1,14 
      VTYPH(mstr,mSta,iV) = VTYPA(mstr,mV,iV) 
  841 continue
      VALTLH(mstr,mSta) = VALTAL(mstr,mV) 
      EDUFLH(mstr,mSta) = EDUFAL(mstr,mV) 
      VALTRH(mstr,mSta) = VALTAR(mstr,mV) 
      EDUFRH(mstr,mSta) = EDUFAR(mstr,mV) 
      goto 848 
      endif 
!                                                                       
      if(fkmgit.le.eVEG(mstr,mV).and.fkmgit.le.aVEG(mstr,mV+1))then 
  842 mV = mV+1 
      if(mV.gt.mVs(mstr))goto 848 
      if(eVEG(mstr,mV).gt.fkmgit.and.mV.lt.mVs(mstr))goto 842 
      Do 843 iV = 1,14 
  843 VTYPH(mstr,mSta,iV) = VTYPA(mstr,mV,iV) 
      VALTLH(mstr,mSta) = VALTAL(mstr,mV) 
      EDUFLH(mstr,mSta) = EDUFAL(mstr,mV) 
      VALTRH(mstr,mSta) = VALTAR(mstr,mV) 
      EDUFRH(mstr,mSta) = EDUFAR(mstr,mV) 
      goto 848 
      endif 
      goto 848 
!                                                                       
!                                                                       
  840 continue 
      if(fkmgit.ge.aVEG(mstr,mV).and.fkmgit.le.eVEG(mstr,mV))then 
      Do 844 iV = 1,14 
  844 VTYPH(mstr,mSta,iV) = VTYPA(mstr,mV,iV) 
      VALTLH(mstr,mSta) = VALTAL(mstr,mV) 
      EDUFLH(mstr,mSta) = EDUFAL(mstr,mV) 
      VALTRH(mstr,mSta) = VALTAR(mstr,mV) 
      EDUFRH(mstr,mSta) = EDUFAR(mstr,mV) 
      goto 848 
      endif 
!                                                                       
      if(fkmgit.ge.eVEG(mstr,mV).and.fkmgit.ge.aVEG(mstr,mV+1))then 
  845 mV = mV+1 
      if(mV.gt.mVs(mstr))goto 848 
      if(eVEG(mstr,mV).lt.fkmgit.and.mV.lt.mVs(mstr))goto 845 
      Do 846 iV = 1,14 
  846 VTYPH(mstr,mSta,iV) = VTYPA(mstr,mV,iV) 
      VALTLH(mstr,mSta) = VALTAL(mstr,mV) 
      EDUFLH(mstr,mSta) = EDUFAL(mstr,mV) 
      VALTRH(mstr,mSta) = VALTAR(mstr,mV) 
      EDUFRH(mstr,mSta) = EDUFAR(mstr,mV) 
      endif 
!                                                                       
  848 continue 

! organisches Material des Sediments   

      POM_sed = -1.0
      BedGS = -1.
      xsedvvertz = -1.0 

      do mZ = 1,mZs(mstr) !alle Z-Zeilen (ModellG.txt) in diesem Strang (Sedimenteigenschaften)
      if(abfr(mstr)==0)then  ! Kilometrierung gegen Fließrichtung 
        if(fkmgit<=aPOM(mstr,mZ).and.fkmgit>=ePOM(mstr,mZ))then
          POM_sed = POMz(mstr,mZ)
          BedGS = BedGSz(mstr,mZ)
          xsedvvertz = sedvvertz(mstr,mz)
          exit
        else
          cycle
        endif 
      else 
            if(fkmgit>=aPOM(mstr,mZ).and.fkmgit<=ePOM(mstr,mZ))then 
              POM_sed = POMz(mstr,mZ)
              BedGS = BedGSz(mstr,mZ)
              xsedvvertz = sedvvertz(mstr,mz)
              exit
            else
              cycle
            endif
      endif
      enddo

! organisches Material des Sediments in Buhnenfelder   

      POM_sedb = -1.0 

      do mU = 1,mUs(mstr)
      if(abfr(mstr)==0)then 
        if(fkmgit<=akmb(mstr,mU).and.fkmgit>=ekmb(mstr,mU))then
          POM_sedb = POMzb(mstr,mU)
          exit
            else
              cycle
         endif 
           else 
            if(fkmgit>=akmb(mstr,mU).and.fkmgit<=ekmb(mstr,mU))then 
              POM_sedb = POMzb(mstr,mU)
              exit
                else
                  cycle
            endif
      endif
      enddo

! Sedimenttemperatur   

      SPEWKSx1 = -1.0
      WUEBKx1 = -1. 
      PSREFSx1 = -1.
      extkx1 = -1.


      do mA = 1,mAs(mstr)

      if(abfr(mstr)==0)then 
        if(fkmgit<=aKSED(mstr,mA).and.fkmgit>=eKSED(mstr,mA))then
          SPEWKSx1 = SPEWKSx(mstr,mA)
          WUEBKx1 = WUEBKx(mstr,mA)
          PSREFSx1 = PSREFSx(mstr,mA)
          extkx1 = extkx(mstr,mA)
          exit
            else
              cycle
         endif 
           else 
            if(fkmgit>=aKSED(mstr,mA).and.fkmgit<=eKSED(mstr,mA))then 
              SPEWKSx1 = SPEWKSx(mstr,mA)
              WUEBKx1 = WUEBKx(mstr,mA)
              PSREFSx1 = PSREFSx(mstr,mA)
              extkx1 = extkx(mstr,mA)
               exit
                else
                  cycle
            endif
      endif
      enddo
                                                                       
!...Abschnittsweise Zuordnung der Wetterstationen                       
!                                                                       
      if(mWe.gt.mWes(mstr))goto 940 
      if(abfr(mstr).eq.1)goto 941 
!                                                                       
      if(fkmgit.le.aWett(mstr,mWe).and.fkmgit.ge.eWett(mstr,mWe))then 
      idWe(mstr,mSta) = ikWSta(mstr,mWe) 
      Wlage(mstr,mSta) = yWlage(mstr,mWe) 
      goto 940 
      endif 
!                                                                       
      if(fkmgit.le.eWett(mstr,mWe).and.fkmgit.le.aWett(mstr,mWe+1))then 
  942 mWe = mWe+1 
      if(mWe.gt.mWes(mstr))goto 940 
      if(eWett(mstr,mWe).gt.fkmgit.and.mWe.lt.mWes(mstr))goto 942 
      idWe(mstr,mSta) = ikWSta(mstr,mWe) 
      Wlage(mstr,mSta) = yWlage(mstr,mWe) 
      goto 940 
      endif 
      goto 940 
!                                                                       
!                                                                       
  941 continue 
      if(fkmgit.ge.aWett(mstr,mWe).and.fkmgit.le.eWett(mstr,mWe))then 
      idWe(mstr,mSta) = ikWSta(mstr,mWe) 
      Wlage(mstr,mSta) = yWlage(mstr,mWe) 
      goto 940 
      endif 
!                                                                       
      if(fkmgit.ge.eWett(mstr,mWe).and.fkmgit.ge.aWett(mstr,mWe+1))then 
  943 mWe = mWe+1 
      if(mWe.gt.mWes(mstr))goto 940 
      if(eWett(mstr,mWe).lt.fkmgit.and.mWe.lt.mWes(mstr))goto 943 
      idWe(mstr,mSta) = ikWSta(mstr,mWe) 
      Wlage(mstr,mSta) = yWlage(mstr,mWe) 
      endif 
!                                                                       
!                                                                       
  940 continue 
!      fkmgn = segkm(mstr,msta+1)                                       
!                                                                       
!                                                                       
      do 688 ndr = 1,nndr 
      hzdrel(mstr,mSta,ndr) = zdreie(ndr) 
      hzdrsl(mstr,mSta,ndr) = zdrese(ndr) 
  688 hgwdrl(mstr,mSta,ndr) = gwdre(ndr) 
      hdlmx(mstr,mSta) = dlmax1 
      hdlmxs(mstr,mSta) = dlmax2 
      hgwdmx(mstr,mSta) = dlmax3 
      hsgwmu(mstr,mSta) = dlmax4 
      habgml(mstr,mSta) = tggbal 
      habkml(mstr,mSta) = tgkbal 
      if(trpmin.lt.0.0)trpmin = 0.0 
      if(trpmax.lt.0.0)trpmax = 0.0 
      hpfmnl(mstr,mSta) = trpmin 
      hpfmxl(mstr,mSta) = trpmax 
      hcoro2(mstr,mSta,1) = coroe 
      hcos2(mstr,mSta,1) = corose 
      do 689 ico = 2,5 
      hcoro2(mstr,mSta,ico) = coro1 
  689 hcos2(mstr,mSta,ico) = coro1 

      sedOM(mstr,kSta+1) = POM_sed                    ! "flag6-Knoten wird in sysgen berücksichtigt.
      BedGSed(mstr,kSta+1) = BedGS
      sedvvert(mstr,kSta+1) = xsedvvertz
      sedOMb(mstr,kSta+1) = POM_sedb

      SPEWKSuS(mstr,kSta+1) = SPEWKSx1  ! Nomenklatur: spezifische Wärmekapazität an den ursprünglichen Stationen  
      WUEBKuS(mstr,kSta+1) = WUEBKx1
      PSREFSuS(mstr,kSta+1) = PSREFSx1
      extkuS(mstr,kSta+1) = extkx1

      if(kSta.eq.0)goto 337 
      if(kSta.gt.0.and.hflag(mstr,kSta).eq.4)then     ! Berücksichtigung des "flag6"-Knotens
      mSta = mSta+1 
      idWe(mstr,mSta) = ikWSta(mstr,mWe) 
      Wlage(mstr,mSta) = yWlage(mstr,mWe) 
      Do 946 iV = 1,14 
  946 VTYPH(mstr,mSta,iV) = VTYPA(mstr,mV,iV) 
      VALTLH(mstr,mSta) = VALTAL(mstr,mV) 
      EDUFLH(mstr,mSta) = EDUFAL(mstr,mV) 
      VALTRH(mstr,mSta) = VALTAR(mstr,mV) 
      EDUFRH(mstr,mSta) = EDUFAR(mstr,mV) 
!                                                                       
      do 988 ndr = 1,nndr 
      hzdrel(mstr,mSta,ndr) = zdreie(ndr) 
      hzdrsl(mstr,mSta,ndr) = zdrese(ndr) 
  988 hgwdrl(mstr,mSta,ndr) = gwdre(ndr) 
      hdlmx(mstr,mSta) = dlmax1 
      hdlmxs(mstr,mSta) = dlmax2 
      hgwdmx(mstr,mSta) = dlmax3 
      hsgwmu(mstr,mSta) = dlmax4 
      habgml(mstr,mSta) = tggbal 
      habkml(mstr,mSta) = tgkbal 
      if(trpmin.lt.0.0)trpmin = 0.0 
      if(trpmax.lt.0.0)trpmax = 0.0 
      hpfmnl(mstr,mSta) = trpmin 
      hpfmxl(mstr,mSta) = trpmax 
      hcoro2(mstr,mSta,1) = coroe 
      hcos2(mstr,mSta,1) = corose 
      do 789 ico = 2,5 
      hcoro2(mstr,mSta,ico) = coro1 
  789 hcos2(mstr,mSta,ico) = coro1 
!                                                                       
      endif 
                                                                        
!      fkmgit = fkmgn                                                   
                                                                       
  337 continue 
  335 continue 
!                                                                       
!***********************************************************************
!                                                                       

      if(iwsim==4.or.iwsim==2.or.iwsim==5)goto 712 
!                                                                       
!     ****************************************                          
!     Berechnung der SedimentKenngroessen                               
!     ****************************************                          
!                                                                       
      jsed = 0

      call Sediment(abfr,azStrs,mStra,Stakm,mStas,mSs,aschif,eschif,SedOM,SedOMb,dKorn,dKornb    &     
                   ,raua,vmq,Hmq,nbuhn,bvmq,bHmq,jsed,w2,w2b,ifehl)
      if(ifehl==26)goto 990
      
                                                                       
  712 continue 
!...EREIGH.txt                                                          
      write(pfadstring,'(2A)')trim(adjustl(cpfad1)),'EREIGH.txt' 
      open(unit=110, file=pfadstring, iostat = open_error)
      if(open_error.ne. 0) then
         print*,'open_error EREIGH.txt',cpfad1
         stop 2
      end if
      rewind (110)
      read(110,'(A2)')ckenn_vers1
      if(ckenn_vers1/='*V')then
        read(110,'(A40)')ERENAME
          else
              rewind (110)
              read(110,'(2x)')
              read(110,'(2x)') 
              read(110,'(2x)') 
              read(110,'(A40)')MODNAME
              read(110,'(A40)')ERENAME 
              read(110,'(2x)') 
              read(110,'(2x)') 
      endif  
                                                                       
! ###### Festlegung der max. Tiefenschichtenanzahl für jeden Ortspunkt bei 2D ######
                                                                       

      nkztot_max = 1

      if(I2Daus.eq.0.or.ilang.eq.1)goto 706 ! Es wird nich 2-Dimensional gerechnet
                                                                       
      write(*,6166) 
 6166 format(2x,'Berechnung der maximalen vertikalen Schichtenanzahl'   &
     &,' kann etwas dauern!')                                           

      do azStr = 1,azStrs 
        mstr = mstra(azStr) 
        do mSta = 1,mStas(mstr)
          Hmax2D(mstr,mSta) = 0.0
          nkzmx(mstr,mSta) = 1   
        enddo 
      enddo

  700 read(110,'(2x)',iostat=read_error) 
      if(read_error<0)goto 707

      do 735 azStr = 1,azStrs 
      mstr = mstra(azStr) 
      i2D = 1 
      do 936 mSta = 1,mStas(mstr) 
      read(110,'(7x,f8.3,57x,f7.4)')Stakmh,Tief2D 
      if(abfr(mstr).eq.1)goto 835 
                                                                       
      if(Stakmh.lt.efkm2D(mstr,i2D).and.i2Ds(mstr).ge.i2D)              &
     &i2D = i2D+1                                                       
      if(i2D.gt.i2Ds(mstr))goto 936 
      if(Stakmh.le.afkm2D(mstr,i2D)                                     &
     &.and.Stakmh.ge.efkm2D(mstr,i2D))goto 538                          
      goto 936 
  835 continue 
      if(Stakmh.gt.efkm2D(mstr,i2D).and.i2Ds(mstr).ge.i2D)              &
     &i2D = i2D+1                                                       
      if(i2D.gt.i2Ds(mstr))goto 936 
      if(Stakmh.ge.afkm2D(mstr,i2D)                                     &
     &.and.Stakmh.le.efkm2D(mstr,i2D))goto 538                          
      goto 936 
!                                                                       
  538 if(Tief2D.gt.Hmax2D(mstr,mSta))Hmax2D(mstr,mSta) = Tief2D 
      nkzmx(mstr,mSta) = int(Hmax2D(mstr,mSta)/dH2D)+2 
      if(nkzmx(mstr,mSta)>nkztot_max)nkztot_max = nkzmx(mstr,mSta)
  936 continue 

        nkzmx(mstr,mStas(mstr)) = nkzmx(mstr,mStas(mstr)-1)  
  735 continue 
      goto 700 
!                                                                       
  707 rewind (110)
      read(110,'(A2)')ckenn_vers1
      if(ckenn_vers1/='*V')then
        read(110,'(A40)')ERENAME
          else
              rewind(110)
              read(110,'(2x)')
              read(110,'(2x)') 
              read(110,'(2x)') 
              read(110,'(A40)')MODNAME
              read(110,'(A40)')ERENAME 
              read(110,'(2x)') 
              read(110,'(2x)') 
      endif  

        do  ! Suchen des Ereignisbeginns in ereigh.txt
          read(110,9708)SCHRNR,itag_Schr, monat_Schr, Jahr_Schr, Uhrz_Schr  ! Lesen der Zeitschritt-Nummer 

          if(itags==itag_Schr.and.monats==monat_Schr.and.Jahrs==Jahr_Schr.and.uhrs==Uhrz_Schr)then
            backspace(unit=110)
            exit
          endif
          do i=1,isumAnzSta
             read(110,'(2x)')
          enddo
          cycle 
        enddo

  9708 format(I5,2x,i2,2x,i2,2x,i4,2x,f5.2)

!##### Test ob an einem Ortspunkt der vertikale Schichtenanzahl größer als 50 ist (DIM = 50) ####

       if(nkztot_max>50)then
         Hmaxtot2D = nkztot_max * dH2D
         dH2D = Hmaxtot2D/48.
       
         do azStr = 1,azStrs 
           mstr = mstra(azStr) 
           do mSta = 1,mStas(mstr) 
             if(Hmax2D(mstr,mSta)>0.0)nkzmx(mstr,mSta) = int(Hmax2D(mstr,mSta)/dH2D) + 2
           enddo
         enddo
       endif   

  706 continue 

      izaehlN = 0   ! Laufvariable für Anzahl der Warnungen gesN 
      izaehlP = 0   ! Laufvariable für Anzahl der Warnungen gesP

                                                                    
! ##### lesen der Vorzeilen in ABLAUF.txt ######                                                        

      write(pfadstring,'(2A)')trim(adjustl(cpfad1)),'ABLAUF.txt'
      open(unit=97, file=pfadstring)
      rewind (97)
      read(97,'(A2)')ckenn_vers1 
      if(ckenn_vers1/='*V')then
        read(97,'(A40)')ERENAME
          else
            read(97,'(A40)')MODNAME 
            read(97,'(A40)')ERENAME
      endif
                                                                     
! ############################################
!   Beginn eines neuen Zeitschritts                                    
! ############################################

                                                                       
! ###### Lesen der Strangreihenfolge ########                                     

        do  ! Suchen des Ereignisbeginns in Ablauf.txt
        read(97,9705,iostat=read_error)SCHRNR,jkenn,itags_Schr, monat_Schr, Jahr_Schr, Uhrz_Schr  ! Lesen der Zeitschritt-Nummer 
        if(jkenn==99)then
          if(itags==itags_Schr.and.monats==monat_Schr.and.Jahrs==Jahr_Schr.and.uhrs==Uhrz_Schr)exit
          cycle
        endif
        enddo

 9999 continue

    do istr = 1,(2*azStrs)
      read(97,9700,iostat=read_error)STRNR(istr),iFlRi_l(istr),(ESTRNR(istr,nstr),nstr=1,azStrs)
      if(iFlRi_l(istr)==99.or.read_error<0)then ! muss wieder ==99 stehen!!!
        jStrNr = StrNr(istr)
        exit
      endif
 
       do nstr = 1,(2*azStrs)
          if(ESTRNR(istr,nstr).eq.0)then 
           nstrs(istr) = nstr-1
           nnstrs(StrNR(istr)) = nstrs(istr)  
           exit 
          endif 
        enddo 
    enddo

 9700 format(I5,2x,I2,500(2x,I5)) 
 9705 format(I5,2x,i2,2x,i2,2x,i2,2x,i4,2x,f5.2)                                                                     
      istrs = istr-1 
                                                                       
!     ***************************************                           
!     Einteilung der Flussstrecke in Segmente                           
!     ***************************************                           
!                                                                       
!...Bildschirmausgabe                                                   
      write(*,6109) 
 6109 format(2x,'SYSTEMGENERIERUNG') 
!                                                                       
!                                                                       
      dt = tflie*86400. 
                                                                       
      call sysgen(ilang,dt,iwsim,nbuhn,akmB,ekmB,DLB,tau2B,alphaB,mUs                                             &
                  ,ifehl,aschif,eschif,mSs,azStrs,mStra,raua,bsohla,boeamq,hlboea,hflaea,htiefa                   &
                  ,hvF,hQaus,SedOM,BedGSed,sedvvert,dKorn,abfr,mStas,Startkm,mRBs,RBtyp,RBkm,ij                   &
                  ,tflie,STRdt,STRiz,cpfad,wsp_UW,WSP_OW                                                          &
                  ,SedOMb,w2,w2b,dKornb,SPEWKSuS,WUEBKuS,PSREFSuS,extkuS,SPEWKSS,WUEBKS,PSREFSS,extkS             & 
                  ,itags,monats,uhrz,ifhStr,fhprof,iverfahren,ianze_max,HMQ,bvMQ,bHMQ,ieros)
      if(ifehl.gt.0)goto 989 

                                                                      
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'sysgenou' 
      open(unit=11, file=pfadstring)
      rewind (11) 

      anzema = 0 
      DO azStr = 1,azStrs 
        mstr = mstra(azStr) 
!                                                                       
      read(11,1000)hanze(mstr) 
!                                                                       
      do istr = 1,istrs
        if(STRNR(istr).eq.mstr)anzema = anzema+int(FZeit(mstr)) 
      enddo 
!                                                                       
      do ior = 1,hanze(mstr) 
        read(11,1010)hfkm(mstr,ior),hflag(mstr,ior),hjiein(mstr,ior),helen(mstr,ior),hvmitt(mstr,ior)                                      &                                
                    ,htiefe(mstr,ior),hrau(mstr,ior),hrhyd(mstr,ior),hSedOM(mstr,ior),hw2(mstr,ior),hBedGS(mstr,ior)                       &                                               
                    ,hsedvvert(mstr,ior),hdKorn(mstr,ior),hflae(mstr,ior),hWS(mstr,ior),hischf(mstr,ior),hlboem(mstr,ior)                  &
                    ,hbsohl(mstr,ior),hvabfl(mstr,ior),bh(mstr,ior),bf(mstr,ior),bso(mstr,ior),blb(mstr,ior)                               &   
                    ,bleb(mstr,ior),hdl(mstr,ior),htau2(mstr,ior),vbm(mstr,ior),bSedOM(mstr,ior),bw2(mstr,ior)                             &
                    ,bdKorn(mstr,ior),dlalph(mstr,ior)
!hanze(mstr)
!   66
!hfkm(mstr,ior)	hflag(mstr,ior)	hjiein(mstr,ior)	helen(mstr,ior)		hvmitt(mstr,ior) 
!   6.240  	1   		0    			140.000   		0.1140
!htiefe(mstr,ior) 	hrau(mstr,ior)	hrhyd(mstr,ior)	hSedOM(mstr,ior)	hw2(mstr,ior)	hBedGS(mstr,ior)
!   2.84  		40.00   	2.70   		0.00500  		0.74000E-05  	-1.00
!hsedvvert(mstr,ior)	hdKorn(mstr,ior)	hflae(mstr,ior)	hWS(mstr,ior)	hischf(mstr,ior)	hlboem(mstr,ior)                  &
!    -1.0000  		0.89157E-05    		181.50    	29.3883  	1    			21.7600
!hbsohl(mstr,ior)	hvabfl(mstr,ior)	bh(mstr,ior)	bf(mstr,ior)	bso(mstr,ior)	blb(mstr,ior)                               &   
!    45.3100       	20.774000  		-1.00    	-1.00    	-1.00    	-1.00  
!bleb(mstr,ior)	hdl(mstr,ior)	htau2(mstr,ior)	vbm(mstr,ior)	bSedOM(mstr,ior)	bw2(mstr,ior)                             & 
!-1.00    	-1.00     	0.02  		-1.0000  	-0.10000  		-.10000E+01
!bdKorn(mstr,ior)	dlalph(mstr,ior)
!  -.10000E+01   	-1.00

        if(hrhyd(mstr,ior)<=0.0)hrhyd(mstr,ior) = htiefe(mstr,ior)
      enddo !do ior = 1,hanze(mstr)
        QStrang_1(mstr) = hvabfl(mstr,1) 
 
!     Vorbelegung fuer Errosionsberechnung                              
                                                                       
      tauscs = 1.25 

      if(iwied/=1.and.iwsim/=2.and.ieros/=0.and.iwsim/=5)then 

        sedhmx = 0.001 
       
        do ior = 1,hanze(mstr)+1 
          sedhg(mstr,ior) = 0.0   
          if(hSedOM(mstr,ior)>0.01)sedhg(mstr,ior) = 0.1

          ischig(mstr,ior) = 100
          do i = 1, ischig(mstr,ior) 
            tauscg(mstr,ior,i) = 0.0
          enddo
 
          if(nbuhn(mstr)==1)then
            bsedh(mstr,ior) = 0.0
            if(bSedOM(mstr,ior)>0.01)bsedh(mstr,ior) = 0.1

            ibschi(mstr,ior) = 100
            do i = 1,ibschi(mstr,ior)
              btausc(mstr,ior,i) = 0.0
            enddo  
          endif  !  if(nbuhn(mstr) 
        enddo ! do ior = 1,hanze(mstr)+1
      endif ! f(iwied/=1
   ENDDO !DO azStr = 1,azStrs 
   
    close (11) 
                                                                       
!... Berechnung der Anzahl der Zeitschritte                             
      if(ilang.eq.1)then 
        else 
      hcon = izdt/60. 
      hcontm = 24./hcon 
      itimeh = nint(hcontm) 
      itimeb = itimeh-1 
!                                                                       
!...Anzahl der Zeitschritte für ersten Simulationstag (itimea)          
!..und letzen Simulationstag (itimee)                                   
!... Umrechnen der End-Uhrzeit in Dezimalschreibweise Uhren in hcUhre   
!                                                                       
!      hcmin = (Uhren-int(Uhren))/0.6                                   
!      hcUhre = int(uhren)+hcmin                                        
                                                                        
      hcontm = (24.-Uhrz)/hcon 
      itimea = nint(hcontm) 
!                                                                       
      hcontm = (Uhren)/hcon 
      itimee = nint(hcontm)+1 
!      if(itimee.eq.0)itimee = 1                                        
                                                                        
      if(itags.eq.itage.and.monats.eq.monate.and.jahrs.eq.jahre)then 
      itimea = itimee 
      itimeh = itimee 
      endif 
                                                                        
      itime = itimeb 
      endif 
                                                                       
!#### Berechnen der Startwerte #####                                         
                                                                       
!## zu Beginn der Simulation werden die Anzahl der Randbedimgungen und die maximale Länge eines Datensatzes bestimmt ##

     if(iwied==0)call Randbedingungen(cpfad, i_Rands, iw_max)

      istr = 0 
                                                                      
      call funkstar(abfls,vbsbs,vcsbs,vnh4s,vno2s,vno3s,gesNs,vx0s,vx02s,gelps,gesPs,sis,chlas,vkigrs                 &
                    ,antbls,zooins,vphs,mws,cas,lfs,ssalgs,tempws,vo2s,CHNFs,BVHNFs,colis,DOSCFs,waers                &         
                    ,ischwer,glZns,gsZns,glCads,gsCads,glCus,gsCus,glNis,gsNis,istund                                 &
                    ,uhrz,RBtyp,NRSCHr,itags,monats,jahrs,cpfad,iwsim,ilang,iwied,mstrRB,azStrs,i_Rands               &
                    ,iw_max,iformVert,ifehl,ifmRB,ifmstr)

                     if(ifehl>0)goto 989

      call aparamles(cpfad,itags,monats,Jahrs,aggmax,akgmax,abgmax)

      write(89,*)itags,monats,aggmax,akgmax

! ##### Berücksichtigung von Eineitern am 1. Ortspunks eines Stranges mit Vorsträngen 1D-Fall#####
      do azStr = 1,azStrs !Strangschleife ANFANG
        mstr = mstra(azStr)
        if(iwied==0)exit
        if(iRB_K1(mstr)<=1)cycle

!.or.nnStrs(mstr)==0)cycle ! noch überprüfen

        sum_QEinl = 0.0

        hcq1 = 0.0
        hcq2 = 0.0
        hcq3 = 0.0
        hcq4 = 0.0
        hcq5 = 0.0
        hcq6 = 0.0
        hcq7 = 0.0
        hcq8 = 0.0
        hcq9 = 0.0
        hcq10 = 0.0
        hcq11 = 0.0
        hcq12 = 0.0
        hcq13 = 0.0
        hcq14 = 0.0
        hcq15 = 0.0
        hcq16 = 0.0
        hcq17 = 0.0
        hcq18 = 0.0
        hcq19 = 0.0
        hcq20 = 0.0
        hcq21 = 0.0
        hcq22 = 0.0
        hcq23 = 0.0
        hcq24 = 0.0
        hcq25 = 0.0
        hcq26 = 0.0

        do iRB = 1,iRB_K1(mstr)
          sum_QEinl = sum_QEinl + abfls(mstr,imRB_K1(iRB))

        enddo

        hcQ1 = max(1.e-10,(QStrang_1(mstr) - sum_QEinl))

        hc1 = hvbsb(mstr,1) * hcQ1
        i_K11 = 0
        hc2 = hvcsb(mstr,1) * hcQ1
        i_K12 = 0
        hc3 = hNH4(mstr,1) * hcQ1
        i_K13 = 0
        hc4 = hNO2(mstr,1) * hcQ1
        i_K14 = 0
        hc5 = hNO3(mstr,1) * hcQ1
        i_K15 = 0  
        hc6 = hgesN(mstr,1) * hcQ1
        i_K16 = 0
        hc7 = hx0(mstr,1) * hcQ1
        i_K17 = 0
        hc8 = hx02(mstr,1) * hcQ1
        i_K18 = 0
        hc9 = hgelP(mstr,1) * hcQ1
        i_K19 = 0
        hc10 = hgesP(mstr,1) * hcQ1
        i_K110 = 0
        hc11 = hSi(mstr,1) * hcQ1
        i_K111 = 0
        hc12 = hchla(mstr,1) * hcQ1
        i_K112 = 0
        hc13 = hvkigr(mstr,1) * hcQ1
        i_K113 = 0
        hc14 = hantbl(mstr,1) * hcQ1
        i_K114 = 0
        hc15 = hzooi(mstr,1) * hcQ1
        i_K115 = 0
        hc16 = hph(mstr,1) * hcQ1
        i_K116 = 0
        hc17 = hmw(mstr,1) * hcQ1
        i_K117 = 0
        hc18 = hca(mstr,1) * hcQ1
        i_K118 = 0
        hc19 = hlf(mstr,1) * hcQ1
        i_K119 = 0
        hc20 = hssalg(mstr,1) * hcQ1
        i_K120 = 0
        hc21 = htempw(mstr,1) * hcQ1
        i_K121 = 0
        hc22 = hO2(mstr,1) * hcQ1
        i_K122 = 0
        hc23 = hCHNF(mstr,1) * hcQ1
        i_K123 = 0
        hc24 = hBVHNF(mstr,1) * hcQ1
        i_K124 = 0
        hc25 = hColi(mstr,1) * hcQ1
        i_K125 = 0
        hc26 = htempw(mstr,1) 
        i_K126 = 0
        hc27 = hDOSCF(mstr,1) * hcQ1

  
        hcq1 = hcq1
        hcq2 = hcq1
        hcq3 = hcq1
        hcq4 = hcq1
        hcq5 = hcq1
        hcq6 = hcq1
        hcq7 = hcq1
        hcq8 = hcq1
        hcq10 = hcq1
        hcq11 = hcq1
        hcq12 = hcq1
        hcq13 = hcq1
        hcq14 = hcq1
        hcq15 = hcq1
        hcq16 = hcq1
        hcq17 = hcq1
        hcq18 = hcq1
        hcq19 = hcq1
        hcq20 = hcq1
        hcq21 = hcq1
        hcq22 = hcq1
        hcq23 = hcq1
        hcq24 = hcq1
        hcq25 = hcq1
        hcq26 = hcq1
        

          do iRB = 1,iRB_K1(mstr)
!            abfls(mstr,imRB_K1(iRB)) = abfls(mstr,imRB_K1(iRB)))

            if(vbsbs(mstr,imRB_K1(iRB))>=0.0)then
              hc1 = hc1 + vbsbs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq1 = hcq1 + abfls(mstr,imRB_K1(iRB)) 
              i_K11 = 1
            endif

            if(vcsbs(mstr,imRB_K1(iRB))>=0.0)then
              hc2 = hc2 + vcsbs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq2 = hcq2 + abfls(mstr,imRB_K1(iRB)) 
              i_K12 = 1
            endif

            if(vNH4s(mstr,imRB_K1(iRB))>=0.0)then
              hc3 = hc3 + vNH4s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq3 = hcq3 + abfls(mstr,imRB_K1(iRB)) 
              i_K13 = 1
            endif

            if(vNO2s(mstr,imRB_K1(iRB))>=0.0)then
              hc4 = hc4 + vNO2s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq4 = hcq4 + abfls(mstr,imRB_K1(iRB)) 
              i_K14 = 1
            endif

            if(vNO3s(mstr,imRB_K1(iRB))>=0.0)then
              hc5 = hc5 + vNO3s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq5 = hcq5 + abfls(mstr,imRB_K1(iRB)) 
              i_K15 = 1
            endif

            if(gesNs(mstr,imRB_K1(iRB))>=0.0)then
              hc6 = hc6 + gesNs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq6 = hcq6 + abfls(mstr,imRB_K1(iRB)) 
              i_K16 = 1
            endif

            if(vx0s(mstr,imRB_K1(iRB))>=0.0)then
              hc7 = hc7 + vx0s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq7 = hcq7 + abfls(mstr,imRB_K1(iRB)) 
              i_K17 = 1
            endif

            if(vx02s(mstr,imRB_K1(iRB))>=0.0)then
              hc8 = hc8 + vx02s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq8 = hcq8 + abfls(mstr,imRB_K1(iRB)) 
              i_K18 = 1
            endif

            if(gelps(mstr,imRB_K1(iRB))>=0.0)then
              hc9 = hc9 + gelps(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq9 = hcq9 + abfls(mstr,imRB_K1(iRB)) 
              i_K19 = 1
            endif

            if(gesPs(mstr,imRB_K1(iRB))>=0.0)then
              hc10 = hc10 + gesPs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq10 = hcq10 + abfls(mstr,imRB_K1(iRB)) 
              i_K110 = 1
            endif

            if(Sis(mstr,imRB_K1(iRB))>=0.0)then
              hc11 = hc11 + Sis(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq11 = hcq11 + abfls(mstr,imRB_K1(iRB)) 
              i_K111 = 1
            endif

            if(chlas(mstr,imRB_K1(iRB))>=0.0)then
              hc12 = hc12 + chlas(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq12 = hcq12 + abfls(mstr,imRB_K1(iRB)) 
              i_K112 = 1
            endif

            if(vkigrs(mstr,imRB_K1(iRB))>=0.0)then
              hc13 = hc13 + vkigrs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq13 = hcq13 + abfls(mstr,imRB_K1(iRB)) 
              i_K113 = 1
            endif

            if(antbls(mstr,imRB_K1(iRB))>=0.0)then
              hc14 = hc14 + antbls(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq14 = hcq14 + abfls(mstr,imRB_K1(iRB)) 
              i_K114 = 1
            endif

            if(zooins(mstr,imRB_K1(iRB))>=0.0)then
              hc15 = hc15 + zooins(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq15 = hcq15 + abfls(mstr,imRB_K1(iRB)) 
              i_K115 = 1
            endif

            if(vphs(mstr,imRB_K1(iRB))>=0.0)then
              hc16 = hc16 + vphs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq16 = hcq16 + abfls(mstr,imRB_K1(iRB)) 
              i_K116 = 1
            endif

            if(mws(mstr,imRB_K1(iRB))>=0.0)then
              hc17 = hc17 + mws(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq17 = hcq17 + abfls(mstr,imRB_K1(iRB)) 
              i_K117 = 1
            endif

            if(cas(mstr,imRB_K1(iRB))>=0.0)then
              hc18 = hc18 + cas(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq18 = hcq18 + abfls(mstr,imRB_K1(iRB)) 
              i_K118 = 1
            endif

            if(lfs(mstr,imRB_K1(iRB))>=0.0)then
              hc19 = hc19 + lfs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq19 = hcq19 + abfls(mstr,imRB_K1(iRB)) 
              i_K119 = 1
            endif

            if(ssalgs(mstr,imRB_K1(iRB))>=0.0)then
              hc20 = hc20 + ssalgs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq20 = hcq20 + abfls(mstr,imRB_K1(iRB)) 
              i_K120 = 1
            endif

            if(iwsim/=4.and.tempws(mstr,imRB_K1(iRB))>-9.99)then
              hc21 = hc21 + tempws(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq21 = hcq21 + abfls(mstr,imRB_K1(iRB)) 
              i_K121 = 1
            endif

            if(iwsim==4.and.tempws(mstr,imRB_K1(iRB))>=0.0)then
              hc21 = hc21 + tempws(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq21 = hcq21 + abfls(mstr,imRB_K1(iRB)) 
              i_K121 = 1
            endif

            if(vo2s(mstr,imRB_K1(iRB))>=0.0)then
              hc22 = hc22 + vo2s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq22 = hcq22 + abfls(mstr,imRB_K1(iRB)) 
              i_K122 = 1
            endif

            if(CHNFs(mstr,imRB_K1(iRB))>=0.0)then
              hc23 = hc23 + CHNFs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq23 = hcq23 + abfls(mstr,imRB_K1(iRB)) 
              i_K123 = 1
            endif

            if(BVHNFs(mstr,imRB_K1(iRB))>=0.0)then
              hc24 = hc24 + BVHNFs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq24 = hcq24 + abfls(mstr,imRB_K1(iRB)) 
              i_K124 = 1
            endif

            if(Colis(mstr,imRB_K1(iRB))>=0.0)then
              hc25 = hc25 + Colis(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hc27 = hc27 + DOSCFs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
              hcq25 = hcq25 + abfls(mstr,imRB_K1(iRB)) 
              i_K125 = 1
            endif
            i_K126 = 0            
            if(waers(mstr,imRB_K1(iRB))>-9999.9.and.abfls(mstr,imRB_K1(iRB))<=2.e-10)then
              hcq26 = hcq26 + abfls(mstr,imRB_K1(iRB)) 
              hc26 = hc26 + waers(mstr,imRB_K1(iRB))/4.2/hcq26
              i_K126 = 1
               else if(waers(mstr,imRB_K1(iRB))>-9999.9.and.abfls(mstr,imRB_K1(iRB))>2.e-10)then
                 hc26 = hc26 + waers(mstr,imRB_K1(iRB))/4.2/(hcq26 + abfls(mstr,imRB_K1(iRB)))
                 hcq26 = hcq26 + abfls(mstr,imRB_K1(iRB)) 
                i_K126 = 1
             endif
          enddo

        do iRB = 1,iRB_K1(mstr)
          mRB = imRB_K1(iRB)
          if(i_K11>0)vbsbs(mstr,mRB) = hc1/hcq1
          if(i_K12>0)vcsbs(mstr,mRB) = hc2/hcq2
          if(i_K13>0)vnh4s(mstr,mRB) = hc3/hcq3
          if(i_K14>0)vno2s(mstr,mRB) = hc4/hcq4
          if(i_K15>0)vno3s(mstr,mRB) = hc5/hcq5
          if(i_K16>0)gesNs(mstr,mRB) = hc6/hcq6
          if(i_K17>0)vx0s(mstr,mRB) = hc7/hcq7
          if(i_K18>0)vx02s(mstr,mRB) = hc8/hcq8
          if(i_K19>0)gelPs(mstr,mRB) = hc9/hcq9
          if(i_K110>0)gesPs(mstr,mRB) = hc10/hcq10
          if(i_K111>0)Sis(mstr,mRB) = hc11/hcq11
          if(i_K112>0)chlas(mstr,mRB) = hc12/hcq12
          if(i_K113>0)vkigrs(mstr,mRB) = hc13/hcq13
          if(i_K114>0)antbls(mstr,mRB) = hc14/hcq14
          if(i_K115>0)zooins(mstr,mRB) = hc15/hcq15
          if(i_K116>0)vphs(mstr,mRB) = hc16/hcq16
          if(i_K117>0)mws(mstr,mRB) = hc17/hcq17
          if(i_K118>0)cas(mstr,mRB) = hc18/hcq18
          if(i_K119>0)lfs(mstr,mRB) = hc19/hcq19
          if(i_K120>0)ssalgs(mstr,mRB) = hc20/hcq20
          if(i_K121>0)tempws(mstr,mRB) = hc21/hcq21
!          if(i_K122>0)vo2s(mstr,mRB) = hc22/hcq22
          if(i_K123>0)CHNFs(mstr,mRB) = hc23/hcq23
          if(i_K124>0)BVHNFs(mstr,mRB) = hc24/hcq24
 
         if(i_K125>0)then
            Colis(mstr,mRB) = hc25/hcq25
            DOSCFs(mstr,mRB) = hc27/hcq25
          endif
          if(i_K126>0)tempws(mstr,mRB) = hc26
         enddo

        enddo ! Strangschleife ENDE
        
      if(iwsim==4)goto 396   !Tracer
!      if(iwsim==2.or.iwied==1.or.iwsim==5)goto 347 
      if(iwsim==2.or.iwsim==5)goto 347
                                                                       
! #### Abfrage ob alle benoetigten Eingaben gemacht wurden (nur beim Modellstart) #####                    

      do azStr = 1,azStrs  
        mstr = mstra(azStr) 
        do mRB = 1,mRBs(mstr)
          if(RBtyp(mstr,mRB)==1.or.RBtyp(mstr,mRB)==2)cycle

!...Nitrosomonas                                                        
      if(vnh4s(mstr,mRB)>0.0.and.vx0s(mstr,mRB)<0.0)then                                   
        ifehl = 6 
        goto 989 
      endif 
                                                                       
!...Nitrobacter                                                         
      if(vnh4s(mstr,mRB)>0.0.and.vno2s(mstr,mRB)>0.0.and.vx02s(mstr,mRB)<0.0)then                                  
        ifehl = 7 
        goto 989 
      endif 
                                                                       
                                                                       
!.....Anteil der Kieselalgen                                            
                                                                       
      if(chlas(mstr,mRB)>0.0.and.vkigrs(mstr,mRB)<0.0)then                                                              
        ifehl = 8 
        goto 989 
      endif 
                                                                       
!.....Anteil der Blaualgen                                              
                                                                       
      if(chlas(mstr,mRB)>0.0.and.antbls(mstr,mRB)<0.0)then                                                              
         ifehl = 9 
         goto 989 
      endif 

!......falls der Anteil der Blaualgen = 0 ist

      if(antbls(mstr,mRB)==0.0)then
        antbls(mstr,mRB) = 0.01
        vkigrs(mstr,mRB) = max(0.01,(vkigrs(mstr,mRB) - antbls(mstr,mRB)))
      endif
                                                                       
!...Silikat                                                             
      if(chlas(mstr,mRB)>0.0.and.vkigrs(mstr,mRB)>0.0.and.Sis(mstr,mRB)<0.0)then                                    
         ifehl = 10 
         goto 989 
      endif 

!... m-Wert

      if(iph==1.and.mws(mstr,mRB)<=0.0)then
        ifehl = 19
        ifhstr = mstr
        goto 989
      endif 

!... Ca-Wert

      if(iph==1.and.Cas(mstr,mRB)<=0.0)then
        ifehl = 20
        goto 989
      endif 

! BSB5 und CSB

    if(vbsbs(mstr,mRB)<0.0.and.vcsbs(mstr,mRB)<0.0)then
      ifehl = 28
      goto 989
    endif

! Schwebstoffe
 
    if(ssalgs(mstr,mRB)<0.0)then
      ifehl = 29
      goto 989
    endif

! ph-Wert

    if(iph==1.and.vphs(mstr,mRB)<0.0)then
      ifehl = 30
      goto 989
    endif

    enddo 
  enddo 
                                                                       
                                                                       
  347 do 731 azStr = 1,azStrs 
      mstr = mstra(azStr) 
      do 732 mRB = 1,mRBs(mstr) 
!                                                                       
      ista = ij 
      if(ista.eq.0)ista = 1 
      if(ista.gt.24)ista = 1 
      if(istund(mstr,mRB).eq.1.or.ischwa.eq.0)goto 1335 
      if(vbsbs(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,1).le.0.0)goto 1310                              
      vbsbs(mstr,mRB) = vbsbs(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,1)*vbsbs(mstr,mRB)/100.                          
 1310 if(vcsbs(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,2).le.0.0)goto 1311                              
      vcsbs(mstr,mRB) = vcsbs(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,2)*vcsbs(mstr,mRB)/100.                          
 1311 if(vnh4s(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,3).le.0.0)goto 1312                              
      vnh4s(mstr,mRB) = vnh4s(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,3)*vnh4s(mstr,mRB)/100.                          
 1312 if(vno2s(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,4).le.0.0)goto 1313                              
      vno2s(mstr,mRB) = vno2s(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,4)*vno2s(mstr,mRB)/100.                          
 1313 if(vno3s(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,5).le.0.0)goto 1314                              
      vno3s(mstr,mRB) = vno3s(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,5)*vno3s(mstr,mRB)/100.                          
 1314 if(gesNs(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,6).le.0.0)goto 1315                              
      gesNs(mstr,mRB) = gesNs(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,6)*gesNs(mstr,mRB)/100.                          
 1315 if(vx0s(mstr,mRB).lt.0.0.or.                                       &
     &wstand(mstr,mRB,7).le.0.0)goto 1316                              
      vx0s(mstr,mRB) = vx0s(mstr,mRB)+astand(ista)                      &
     &*wstand(mstr,mRB,7)*vx0s(mstr,mRB)/100.                           
 1316 if(vx02s(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,8).le.0.0)goto 1317                              
      vx02s(mstr,mRB) = vx02s(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,8)*vx02s(mstr,mRB)/100.                          
 1317 if(gelPs(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,9).le.0.0)goto 1318                              
      gelPs(mstr,mRB) = gelPs(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,9)*gelPs(mstr,mRB)/100.                          
 1318 if(gesPs(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,10).le.0.0)goto 1319                             
      gesPs(mstr,mRB) = gesPs(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,10)*gesPs(mstr,mRB)/100.                         
 1319 if(sis(mstr,mRB).lt.0.0.or.                                        &
     &wstand(mstr,mRB,11).le.0.0)goto 1320                             
      sis(mstr,mRB) = sis(mstr,mRB)+astand(ista)                        &
     &*wstand(mstr,mRB,11)*sis(mstr,mRB)/100.                           
 1320 if(chlas(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,12).le.0.0)goto 1321                             
      chlas(mstr,mRB) = chlas(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,12)*chlas(mstr,mRB)/100.                         
 1321 if(vkigrs(mstr,mRB).lt.0.0.or.                                     &
     &wstand(mstr,mRB,13).le.0.0)goto 1322                             
      vkigrs(mstr,mRB) = vkigrs(mstr,mRB)+astand(ista)                  &
     &*wstand(mstr,mRB,13)*vkigrs(mstr,mRB)/100.                        
 1322 if(antbls(mstr,mRB).lt.0.0.or.                                     &
     &wstand(mstr,mRB,14).le.0.0)goto 1323                             
      antbls(mstr,mRB) = antbls(mstr,mRB)+astand(ista)                  &
     &*wstand(mstr,mRB,14)*antbls(mstr,mRB)/100.                        
 1323 if(zooins(mstr,mRB).lt.0.0.or.                                     &
     &wstand(mstr,mRB,15).le.0.0)goto 1324                             
      zooins(mstr,mRB) = zooins(mstr,mRB)+astand(ista)                  &
     &*wstand(mstr,mRB,15)*zooins(mstr,mRB)/100.                        
 1324 if(lfs(mstr,mRB).lt.0.0.or.                                        &
     &wstand(mstr,mRB,19).le.0.0)goto 1325                             
      lfs(mstr,mRB) = lfs(mstr,mRB)+astand(ista)                        &
     &*wstand(mstr,mRB,19)*lfs(mstr,mRB)/100.                           
!                                                                       
 1325 if(vphs(mstr,mRB).lt.0.0.or.                                       &
     &wstand(mstr,mRB,16).le.0.0)goto 1326                             
      if(lfs(mstr,mRB).lt.0.0)lfs(mstr,mRB) = 0.0 
      mues = 1.7e-5*lfs(mstr,mRB) 
      hk = (0.5*sqrt(mues))/(1.+1.4*sqrt(mues)) 
      lgh = vphs(mstr,mRB)-hk 
      hs = 10**(-lgh) 
      hs = hs+astand(ista)*wstand(mstr,mRB,16)*hs/100. 
      vphs(mstr,mRB) = -1.*alog10(hs)+hk 
!                                                                       
 1326 if(mws(mstr,mRB).lt.0.0.or.                                        &
     &wstand(mstr,mRB,17).le.0.0)goto 1327                             
      mws(mstr,mRB) = mws(mstr,mRB)+astand(ista)                        &
     &*wstand(mstr,mRB,17)*mws(mstr,mRB)/100.                           
 1327 if(cas(mstr,mRB).lt.0.0.or.                                        &
     &wstand(mstr,mRB,18).le.0.0)goto 1328                             
      cas(mstr,mRB) = cas(mstr,mRB)+astand(ista)                        &
     &*wstand(mstr,mRB,18)*cas(mstr,mRB)/100.                           
 1328 if(ssalgs(mstr,mRB).lt.0.0.or.                                     &
     &wstand(mstr,mRB,20).le.0.0)goto 1329                             
      ssalgs(mstr,mRB) = ssalgs(mstr,mRB)+astand(ista)                  &
     &*wstand(mstr,mRB,20)*ssalgs(mstr,mRB)/100.                        
 1329 if(tempws(mstr,mRB).lt.0.0.or.                                     &
     &wstand(mstr,mRB,21).le.0.0)goto 1330                             
      tempws(mstr,mRB) = tempws(mstr,mRB)+astand(ista)                  &
     &*wstand(mstr,mRB,21)*tempws(mstr,mRB)/100.                        
 1330 if(vo2s(mstr,mRB).lt.0.0.or.                                       &
     &wstand(mstr,mRB,22).le.0.0)goto 1331                             
      vo2s(mstr,mRB) = vo2s(mstr,mRB)+astand(ista)                      &
     &*wstand(mstr,mRB,22)*vo2s(mstr,mRB)/100.                          
 1331 if(CHNFs(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,23).le.0.0)goto 1332                             
      CHNFs(mstr,mRB) = CHNFs(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,23)*CHNFs(mstr,mRB)/100.                         
 1332 if(BVHNFs(mstr,mRB).lt.0.0.or.                                     &
     &wstand(mstr,mRB,24).le.0.0)goto 1333                             
      BVHNFs(mstr,mRB) = BVHNFs(mstr,mRB)+astand(ista)                  &
     &*wstand(mstr,mRB,24)*BVHNFs(mstr,mRB)/100.                        
 1333 if(colis(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,25).le.0.0)goto 1334                             
      colis(mstr,mRB) = colis(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,25)*colis(mstr,mRB)/100.                         
 1334 if(waers(mstr,mRB).lt.0.0.or.                                      &
     &wstand(mstr,mRB,26).le.0.0)goto 1335                             
      waers(mstr,mRB) = waers(mstr,mRB)+astand(ista)                    &
     &*wstand(mstr,mRB,26)*waers(mstr,mRB)/100.                         
!                                                                       
!                                                                       
!.....Umrechnung der Zellzahlen von HNF in mgC                          
!                                                                       
 1335 if(CHNFs(mstr,mRB).lt.0.0)then 
      CHNFs(mstr,mRB) = 0.0 
      BVHNFs(mstr,mRB) = 0.0 
      goto 732 
      endif

      if(CHNFs(mstr,mRB)>0.0.and.BVHNFs(mstr,mRB)<=0.0)BVHNFs(mstr,mRB) = 25.      ! in µm3 
      CHNFs(mstr,mRB) = CHNFs(mstr,mRB)*BVHNFs(mstr,mRB)*0.22 
!.......Umrechnung von pg in mg /1.e9; Angabe CHNFs pro ml ergibt /1.e6 
!       bezogen auf ein Liter                                           
      CHNFs(mstr,mRB) = CHNFs(mstr,mRB)/1.e6 
                                                                       
  732 continue 
  731 continue 
      if(iwsim==2.or.iwsim==5)goto 396 
!                                                                       
!     Setzen von Werten am Startprofil                                  
!                                                                       
      fssgrs = 0.7
      fbsgrs = 0.4                     !   0.21 
!      frfgrs = 0.13  

     einmalig=.true. ! Fehlermeldung nur einmal

     do 720 azStr = 1,azStrs 
      mstr = mstra(azStr) 
      do 721 mRB = 1,mRBs(mstr) 
!                                                                       
      if(NRSchr(mstr,mRB).eq.0)goto 721 
      if(iwsim==2.or.iwsim==5)goto 721 
!                                                                       
      hcchla = chlas(mstr,mRB) 
      hczoos = zooins(mstr,mRB) 
      hcnh4s = vnh4s(mstr,mRB) 
      hcno2s = vno2s(mstr,mRB) 
      hcno3s = vno3s(mstr,mRB) 
      hcgePs = gelPs(mstr,mRB) 
      hcbsb = vbsbs(mstr,mRB) 
      hccsb = vcsbs(mstr,mRB) 
      hcchla = chlas(mstr,mRB) 
      hcvkg = vkigrs(mstr,mRB) 
      hcantb = antbls(mstr,mRB) 
!                                                                       
      if(zooins(mstr,mRB).lt.0.0)zooins(mstr,mRB) = 0.0 
      if(vnh4s(mstr,mRB).lt.0.0)vnh4s(mstr,mRB) = 0.0 
      if(vno2s(mstr,mRB).lt.0.0)vno2s(mstr,mRB) = 0.0 
      if(vno3s(mstr,mRB).lt.0.0)vno3s(mstr,mRB) = 0.0 
      if(gelPs(mstr,mRB).lt.0.0)gelPs(mstr,mRB) = 0.0 
      if(vbsbs(mstr,mRB).lt.0.0)vbsbs(mstr,mRB) = 0.0 
      if(vcsbs(mstr,mRB).lt.0.0)vcsbs(mstr,mRB) = 0.0 
      if(chlas(mstr,mRB).lt.0.0)chlas(mstr,mRB) = 0.0 
!                                                                       
      if(vkigrs(mstr,mRB).lt.0.0)vkigrs(mstr,mRB) = 0.0 
      if(antbls(mstr,mRB).lt.0.0)antbls(mstr,mRB) = 0.0 
!...Fehlerausgabe falls AnteilGR+AnteilKI+AnteilBL >1                   
      hconFe = 1.-vkigrs(mstr,mRB)-antbls(mstr,mRB) 

      if(hconFe<0.0)then 
      ifehl = 5 
         ifhStr = mstr 
         goto 989 
         endif 

      if(RBtyp(mstr,mRB)==0)TGZoo(mstr,1) = GRote
      if(RBtyp(mstr,mRB)==2)TGZoo(mstr,hanze(mstr)+1) = GRote

!                                                                       
!     Berechnung des Chlorophyll-a/Kohlenstoff-Verhaeltnisses
!     Angabe in mgChla/mgC                                              
!                                                                         
      call ini_algae(akchl,abchl,agchl, Cagr,Caki,Cabl,CZoo, a1Ki,a2Ki,a3Ki, a1Bl,a2Bl,a3Bl, a1Gr,a2Gr,a3Gr)  !!wy jetzt in zuflussrand.f90

!.....Temperaturabhängigkeit des C:Chla-Verhältnisses   !....ag(k,b)chl gilt für 20°C  in mgC/mgChla       ! mg Algenbiomasse, Chla in µg/l
      !!wy jetzt in zuflussrand.f90          
      call algae_start(                                                       &
     &     chlas(mstr,mRB),vkigrs(mstr,mRB),antbls(mstr,mRB),tempws(mstr,mRB),&
     &     akbcms(mstr,mRB),abbcms(mstr,mRB),agbcms(mstr,mRB),                &
     &     akis(mstr,mRB),abls(mstr,mRB),agrs(mstr,mRB),                      &
     &     a1Ki,a1Bl,a1Gr,Caki,Cabl,Cagr,akchl,abchl,agchl,                   &
     &     chlaks(mstr,mRB),chlabs(mstr,mRB),chlags(mstr,mRB)                 &
     &     )

!....Berechnung der "BSB-Komponenten" am oberen Rand           !!wy jetzt in zuflussrand.f90            
!     (auch bei Stundenwert-Generierung)!!!!!                           
       call orgc_start(                                                                    &
     &     TOC_CSB,bsbZoo,GRote,                                                           &
     &     akis(mstr,mRB),abls(mstr,mRB),agrs(mstr,mRB),                                   &
     &     Caki,Cabl,Cagr,CZoo, bsbki,bsbbl,bsbgr,  csbki,csbbl,csbgr,                     &
     &     zooins(mstr,mRB),vbsbs(mstr,mRB),vcsbs(mstr,mRB),                               &
     &     obsbs(mstr,mRB),ocsbs(mstr,mRB),                                                &
     &     CMs(mstr,mRB), CDs(mstr,1,mRB),CDs(mstr,2,mRB), CPs(mstr,1,mRB),CPs(mstr,2,mRB),&
     &     ssalgs(mstr,mRB),frfgrs(mstr,mRB),BACs(mstr,mRB),CHNFs(mstr,mRB),               &
     &     CPges,CDges,Cref,TOC )

!!....zelluläre Nährstoffgehalte 
       call naehr_start(                                                                   &
     &     akis(mstr,mRB),abls(mstr,mRB),agrs(mstr,mRB),                                   &
     &     vnh4s(mstr,mRB),vNO3s(mstr,mRB),vno2s(mstr,mRB),gesNs(mstr,mRB),                &
     &     zooins(mstr,mRB),nZoo, pZoo, GRote,                                             &
     &     gelPs(mstr,mRB),gesPs(mstr,mRB),                                                &
     &     Q_NKs(mstr,mRB),Q_PKs(mstr,mRB),Q_SKs(mstr,mRB),Q_NGs(mstr,mRB),Q_PGs(mstr,mRB),Q_NBs(mstr,mRB),Q_PBs(mstr,mRB),   &
     &     Qmx_NK,Qmn_NK,Qmx_PK,Qmn_PK,Qmx_SK,Qmn_SK, Qmx_NG,Qmn_NG,Qmx_PG,Qmn_PG, Qmx_NB,Qmn_NB,Qmx_PB,Qmn_PB,               &
     &     CPges,CDges,Cref,BACs(mstr,mRB),CMs(mstr,mRB),                                  &
     &     nl0s(mstr,mRB), pl0s(mstr,mRB),                                                 &
     &     sss(mstr,mRB), ssalgs(mstr,mRB),                                                &
     &     itags,monats,mstr,mRB,  einmalig, .FALSE., 0)
      
      zooins(mstr,mRB) = hczoos 
      vnh4s(mstr,mRB) = hcnh4s 
      vno2s(mstr,mRB) = hcno2s 
      vno3s(mstr,mRB) = hcno3s 
      gelPs(mstr,mRB) = hcgePs
      if(hcbsb==0.0.and.hccsb==0.0)then 
        vbsbs(mstr,mRB) = hcbsb 
        vcsbs(mstr,mRB) = hccsb
      endif 
      chlas(mstr,mRB) = hcchla 
      vkigrs(mstr,mRB) = hcvkg 
      antbls(mstr,mRB) = hcantb 

      if(RBTyp(mstr,mRB).eq.2.and.hcbsb.lt.0.0.and.hccsb.lt.0.0)then                                            
                                                                        
      CDs(mstr,1,mRB) = -1. 
      CDs(mstr,2,mRB) = -1. 
      CPs(mstr,1,mRB) = -1. 
      CPs(mstr,2,mRB) = -1. 
      obsbs(mstr,mRB) = -1. 
      ocsbs(mstr,mRB) = -1. 
      endif 
      if(RBTyp(mstr,mRB).eq.2.and.hcchla.lt.0.0)then                                                              
      akis(mstr,mRB) = -1. 
      abls(mstr,mRB) = -1. 
      agrs(mstr,mRB) = -1.
      chlaks(mstr,mRB) = -1.
      chlabs(mstr,mRB) = -1.
      chlags(mstr,mRB) = -1. 
      endif 
!                                                                       
      if(iph.eq.0)goto 721 
!                                                                       
      if(RBtyp(mstr,mRB).gt.0.or.NRschr(mstr,mRB).eq.0)goto 721 
                                                                        
!     Berechnung des p-Wertes am Start (ohne Algen)                     
!                                                                       
      !!wy call pwert(mws,vphs,lfs,tempws,pws,mRB,mstr,azStrs) 
      call pwert(mws(mstr,mRB),vphs(mstr,mRB),lfs(mstr,mRB),tempws(mstr,mRB),pws(mstr,mRB),mRB,mstr) !!wy Übergabe Einzelwerte, nicht Felder
!                                                                       
!     Berechnung des pH-Wertes und p-Wertes am Start unter Algeneinfluss
!                                                                       
!      call phstart(mws,pws,lfs,chlas,vkigrs,agbcms,akbcms,vphs         
!     *,tempws,abbcms,antbls,mstr,mRB,azStrs)                                  
!                                                                       
!##### Belegen des 1. Knotens eines Strangs, wenn Vorstränge und Einleitung am 1. Knoten #####

    if(nnstrs(mstr)>0)then 
      hgesN(mstr,1) = gesNs(mstr,mRB) 
      hgesP(mstr,1) = gesPs(mstr,mRB) 
      hfssgr(mstr,1) = frfgrs(mstr,mRB)
      hnl0(mstr,1) = nl0s(mstr,mRB) 
      hpl0(mstr,1) = pl0s(mstr,mRB) 
      hbsb(mstr,1) = obsbs(mstr,mRB) 
      hcsb(mstr,1) = ocsbs(mstr,mRB) 
      hCHNF(mstr,1) = CHNFs(mstr,mRB) 
      hBVHNF(mstr,1) = BVHNFs(mstr,mRB) 
      hCD(mstr,1,1) = CDs(mstr,1,mRB)
      hCD(mstr,2,1) = CDs(mstr,2,mRB) 
      hCP(mstr,1,1) = CPs(mstr,1,mRB) 
      hCP(mstr,2,1) = CPs(mstr,2,mRB) 
      hCM(mstr,1) = CMs(mstr,mRB) 
      hBAC(mstr,1) = BACs(mstr,mRB) 
      hnh4(mstr,1) = vNH4s(mstr,mRB) 
      ho2(mstr,1) = vo2s(mstr,mRB) 
      hno3(mstr,1) = vno3s(mstr,mRB) 
      hno2(mstr,1) = vnO2s(mstr,mRB) 
      hx0(mstr,1) = vx0s(mstr,mRB) 
      hx02(mstr,1) = vx02s(mstr,mRB) 
      hsi(mstr,1) = Sis(mstr,mRB) 
      hchla(mstr,1) = chlas(mstr,mRB) 
      haki(mstr,1) = (chlas(mstr,mRB)*vkigrs(mstr,mRB)/1000.) * (hakbcm(mstr,1)/Caki) 
      hagr(mstr,1) = (chlas(mstr,mRB)*(1.-vkigrs(mstr,mRB)-antbls(mstr,mRB))/1000.) * (hagbcm(mstr,1)/Cagr) 
      habl(mstr,1) = (Chlas(mstr,mRB)*antbls(mstr,mRB)/1000.) * (habbcm(mstr,1)/Cabl) 
      hchlak(mstr,1) = chlas(mstr,mRB)* vkigrs(mstr,mRB) 
      hchlag(mstr,1) = chlas(mstr,mRB)* (1.-vkigrs(mstr,mRB)-antbls(mstr,mRB)) 
      hchlab(mstr,1) = chlas(mstr,mRB)* antbls(mstr,mRB) 
      hvkigr(mstr,1) = vkigrs(mstr,mRB) 
      hantbl(mstr,1) = antbls(mstr,mRB) 
      hssalg(mstr,1) = ssalgs(mstr,mRB) 
      hss(mstr,1) = sss(mstr,mRB) 
      hzooi(mstr,1) = zooins(mstr,mRB) 
      hgelp(mstr,1) = gelps(mstr,mRB) 
      hmw(mstr,1) = mws(mstr,mRB) 
      hpw(mstr,1) = pws(mstr,mRB) 
      hca(mstr,1) = cas(mstr,mRB) 
      hlf(mstr,1) = lfs(mstr,mRB) 
      hph(mstr,1) = vphs(mstr,mRB) 
      hcoli(mstr,1) = colis(mstr,mRB)
      hDOSCF(mstr,1) = DOSCFs(mstr,mRB) 
      hvbsb(mstr,1) = vbsbs(mstr,mRB) 
      hvcsb(mstr,1) = vcsbs(mstr,mRB)
      hgsZn(mstr,1) = gsZns(mstr,mRB) 
      hglZn(mstr,1) = glZns(mstr,mRB) 
      hgsCad(mstr,1) = gsCads(mstr,mRB) 
      hglCad(mstr,1) = glCads(mstr,mRB) 
      hgsCu(mstr,1) = gsCus(mstr,mRB) 
      hglCu(mstr,1) = glCus(mstr,mRB) 
      hgsNi(mstr,1) = gsNis(mstr,mRB) 
      hglNi(mstr,1) = glNis(mstr,mRB) 
    endif

!###############################################################

  721 continue 

  720 continue
 
                                                                       
  396 continue


!.....2D-Modellierung                                                   
!.....Ermittlung der Anzahl der vertikalen Schichten fr jeden          
!.....Knotenpunkt in jedem Strang                                       

      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'sysgenou' 
      open(unit=11, file=pfadstring)
      rewind (11) 
!                                                                       
      nkzsmx = 0 
      do 736 azStr = 1,azStrs 
      mstr = mstra(azStr) 
      i2D = 1 

                                                                       
      read(11,1000)hanze(mstr)
!                                                                       
      do 576 ior = 1,hanze(mstr) 
      read(11,1011)fkm(ior),tiefe(ior) 
!....Ruecksetzen                                                        
      hnkzs(mstr,ior) = 1 
      hdH2De(mstr,ior) = 0.0 
!                                                                       
!                                                                       
      if(i2Ds(mstr).eq.0.or.i2D.gt.i2Ds(mstr))then 
      hnkzs(mstr,ior) = 1 
      goto 576 
      endif 
!                                                                       
      if(abfr(mstr).eq.1)goto 577 
      hnkzs(mstr,ior) = 1 
      if(fkm(ior).lt.efkm2D(mstr,i2D))i2D = i2D+1 
      if(i2D.gt.i2Ds(mstr))goto 576 
      if(fkm(ior).le.afkm2D(mstr,i2D)                                   &
     &.and.fkm(ior).ge.efkm2D(mstr,i2D))then                            
      hnkzs(mstr,ior) = int(tiefe(ior)/dH2D) 
      testH = tiefe(ior)-hnkzs(mstr,ior)*dH2D 
      hdH2De(mstr,ior) = 0.0 
!                                                                       
      if(testH.gt.0.001)then 
      hdH2De(mstr,ior) = testH 
      hnkzs(mstr,ior) = hnkzs(mstr,ior)+1 
      endif 
!                                                                       
      hnkzs(mstr,ior) = hnkzs(mstr,ior)+1 
      goto 576 
      endif 
      goto 576 
!                                                                       
  577 continue 
      hnkzs(mstr,ior) = 1 
      if(fkm(ior).gt.efkm2D(mstr,i2D))i2D = i2D+1 
      if(i2D.gt.i2Ds(mstr))goto 576 
      if(fkm(ior).ge.afkm2D(mstr,i2D)                                   &
     &.and.fkm(ior).le.efkm2D(mstr,i2D))then                            
      hnkzs(mstr,ior) = int(tiefe(ior)/dH2D) 
      testH = tiefe(ior)-hnkzs(mstr,ior)*dH2D 
      hdH2De(mstr,ior) = 0.0 
!                                                                       
      if(testH.gt.0.001)then 
      hnkzs(mstr,ior) = hnkzs(mstr,ior)+1 
      hdH2De(mstr,ior) = testH 
      endif 
!                                                                       
      hnkzs(mstr,ior) = hnkzs(mstr,ior)+1 
      goto 576 
      endif 
!                                                                       
 576 continue 
                                                                       
      hnkzs(mstr,hanze(mstr)+1) = hnkzs(mstr,hanze(mstr)) 
      hdH2De(mstr,hanze(mstr)+1) = hdH2De(mstr,hanze(mstr)) 

      do i=1,hanze(mstr)+1
      enddo
!                                                                       
!...maximale Schichtenanzahl fuer den Anfang und das Ende der einzelnen 
      if(hnkzs(mstr,1).gt.nkzsmx)nkzsmx = hnkzs(mstr,1) 
      if(hnkzs(mstr,hanze(mstr)+1).gt.nkzsmx)                           &
     &nkzsmx = hnkzs(mstr,hanze(mstr)+1)                                
                                                                       
                                                                       
  736 continue 
                                                                      
                                                                       
 1011 format(f8.3,28x,f5.2) 
      close (11) 
                                                                       
!....Ermittlung von SA und SU                                           
                                                                       
      call sasu(itags,monats,geob,geol,sa,su,zg,zlk,dk,tdj,ifehl)
      if(ifehl>0)goto 989 
                                                                      
!...  Ermittlung der Kenngroessen zur Berücksichtigung des Wehrueberfalls           
                                                                       
      if(iwsim==2.or.iwsim==5)then
        else 
          call WEHRLES(itags,monats,Jahrs,uhrz,wehrh,wehrb,azStrs,mStra,jlWO2,janzWt,janzWs,cpfad,iwied)                                
      endif                                                                       
                                                                      
! ..Ermittlung der Wetterdaten fuer den Zeitschritt                      
                                                                        
 9191 continue

      if(iwsim==4.or.iwsim==5)then
        else 
          call wettles(itags,monats,jahrs,uhrz,glob,tlmax,tlmin,ro,wge,cloud,typw,imet     &
                       ,iwied,cpfad, ckenn_vers1)

      endif

                                                                      
!#### Neubelegung des vertikalen Rechengitters am ersten und letzten Gitterpunkts eines Strangs ####       
                                                                       


    if(ilang==1)then  

      do azStr = 1,azStrs 
        mstr = mstra(azStr) 

       if(I2Ds(mstr)>0.and.iwsim/=4)then

       do jnkz = 1, 2  
         if(jnkz==1)nkzs_hc = hnkzs(mstr,1)   
         if(jnkz==2)nkzs_hc = hnkzs(mstr,hanze(mstr)+1)
         if(jnkz==1)nkzs_hc1 = znkzs(mstr,1)
         if(jnkz==2)nkzs_hc1 = znkzs(mstr,hanze(mstr)+1)

          i_EstRNR = mstr
          if(nkzs_hc==nkzs_hc1)then
           else
          call  sys_gitterStrang(mstr,nkzs_hc,nkzs_hc1,dH2D,tzt,o2zt,NH4zt                                              &
                                 ,no2zt,no3zt,Pzt,gSizt,akizt,agrzt,ablzt,chlazt,chlkzt,chlgzt,chlbzt,gesPzt,gesNzt             &
                                 ,Q_NKzt, Q_NBzt, Q_NGzt,CChlkzt,CChlbzt,CChlgzt,jnkz,i_EstRNR,itags,monats,uhrz,azStrs)
         endif 
       enddo  
     endif 
    enddo

    endif

!######################################################################################                                                                       
!...Strangschleife  fuer alle Straenge: Einlesen der Einleiterdaten und Randbedingungen 
!######################################################################################                                                                       

    if(iwied==0)then  ! Ermittlung eines Strangs mit Randbedingungen am 1. Ortspunkt
                      ! alle Straenge die keine Randbedingung am 1. Ortspunkt
                      ! und keine Vor- und nachgelagerten Straenge haben, werden mit
                      ! diesen Randbedingungen belegt.   
      mRB_1 = 0
      j = 0
      do istr = 1,azStrs
        mstr = STRNR(istr)
          do mRB = 1,mRBs(mstr)
            if(RBtyp(mstr,mRB)==0.or.RBtyp(mstr,mRB)==2)then
              if(mRB_1==0)then
                mstrRB = mstr
                mRB_1 = mRB
              endif
            endif
            if(RBtyp(mstr,mRB)==0.and.nstrs(istr)==0)then
              j = j+1
              mstr_ist(j) = mstr
              endif
           enddo
         enddo

     endif

     if(iwied==0)j = 0

    do istr = 1,istrs  ! Beginn Strangschleife

      mstr = STRNR(istr)
      ieinsh(mstr) = 0 
      iflRi(mstr) = iflRi_l(istr)

      j_ist = 1 ! Zufließende Straenge sind mit Randbedingungen belegt

      if(iwied==0)then  ! Test, ob zu Beginn der Simulation die zufließenden Stränge mit Anfangsbedingungen belegt sind
      j_ist = 0
        if(nstrs(istr)==0)then
          else 
           do nstr = 1,nstrs(istr)
             do jj = 1,azStrs    !js 
             if(ESTRNR(istr,nstr)==mstr_ist(jj))then
               j_ist = 1
               exit
             endif
           enddo
          enddo
        endif
     j = j+1
     mstr_ist(j) = mstr
     js = j
     endif

     if(iFlRi(mstr)==0.and.iwied==1)cycle   

      iein = 0 
      ieinL = 0
      
                                                                      
      if(iwied==1)mRB_1 = 0     ! Nr der Randbedingung bei Randbedingungstyp 0 und 2    
                                                                        
      mRand = 0                 ! Schalter zur Überprüfung ob am ersten Ortspunkt eines 
                                ! Strangs Randbedingungen vorliegen (Datei Ereigg.txt)
 
      do mRB = 1,mRBs(mstr) ! RandbedingungsSchleife fuer Strang mstr

       if(RBtyp(mstr,mRB).ne.0.and.RBtyp(mstr,mRB).ne.2)then                                      
                                        
          if(mstrLe(mstr,mRB)<0)then ! Abfrage ob Diffuse Einleitung  
            iein = iein+1 
!...Einleiter                                                           
            qeinlh(mstr,iein) = abfls(mstr,mRB) 
            ebsbh(mstr,iein) = vbsbs(mstr,mRB) 
            ecsbh(mstr,iein) = vcsbs(mstr,mRB) 
            enh4h(mstr,iein) = vnh4s(mstr,mRB) 
            eno2h(mstr,iein) = vno2s(mstr,mRB) 
            eno3h(mstr,iein) = vno3s(mstr,mRB) 
            egesNh(mstr,iein) = gesNs(mstr,mRB) 
            ex0h(mstr,iein) = vx0s(mstr,mRB) 
            ex02h(mstr,iein) = vx02s(mstr,mRB) 
            egph(mstr,iein) = gelps(mstr,mRB) 
            egesPh(mstr,iein) = gesPs(mstr,mRB) 
            esih(mstr,iein) = sis(mstr,mRB) 
            echlah(mstr,iein) = chlas(mstr,mRB) 
            evkgh(mstr,iein) = vkigrs(mstr,mRB) 
            eantbh(mstr,iein) = antbls(mstr,mRB) 
            ezindh(mstr,iein) = zooins(mstr,mRB) 
            ephh(mstr,iein) = vphs(mstr,mRB) 
            emwh(mstr,iein) = mws(mstr,mRB) 
            ecah(mstr,iein) = cas(mstr,mRB) 
            elfh(mstr,iein) = lfs(mstr,mRB) 
            essh(mstr,iein) = ssalgs(mstr,mRB) 
            etemph(mstr,iein) = tempws(mstr,mRB) 
            eo2h(mstr,iein) = vo2s(mstr,mRB) 
            eCHNFh(mstr,iein) = CHNFs(mstr,mRB) 
            eBVHNh(mstr,iein) = BVHNFs(mstr,mRB) 
            ecolih(mstr,iein) = colis(mstr,mRB) 
            ewaerh(mstr,iein) = waers(mstr,mRB)
            typh(mstr,iein) = weinl(mstr,mRB) 
            enl0h(mstr,iein) = nl0s(mstr,mRB) 
            epl0h(mstr,iein) = pl0s(mstr,mRB)
            eCD(mstr,1,iein) = CDs(mstr,1,mRB) 
            eCD(mstr,2,iein) = CDs(mstr,2,mRB) 
            eCP(mstr,1,iein) = CPs(mstr,1,mRB) 
            eCP(mstr,2,iein) = CPs(mstr,2,mRB)
            eCM(mstr,iein) = CMs(mstr,mRB)
            eBAC(mstr,iein) = BACs(mstr,mRB)
            egsZn(mstr,iein) = gsZns(mstr,mRB)
            eglZn(mstr,iein) = glZns(mstr,mRB)
            egsCad(mstr,iein) = gsCads(mstr,mRB)
            eglCad(mstr,iein) = glCads(mstr,mRB)
            egsCu(mstr,iein) = gsCus(mstr,mRB)
            eglCu(mstr,iein) = glCus(mstr,mRB)
            egsNi(mstr,iein) = gsNis(mstr,mRB)
            eglNi(mstr,iein) = glNis(mstr,mRB)
         
              else 
                                                                        
!....Diffuse Einleitung                                                 
                ieinL = ieinL+1 
                qLh(mstr,ieinL) = abfls(mstr,mRB)/WirkLL(mstr,ieinL) 
                bsbLh(mstr,ieinL) = vbsbs(mstr,mRB) 
                csbLh(mstr,ieinL) = vcsbs(mstr,mRB) 
                enh4Lh(mstr,ieinL) = vnh4s(mstr,mRB) 
                eno2Lh(mstr,ieinL) = vno2s(mstr,mRB) 
                eno3Lh(mstr,ieinL) = vno3s(mstr,mRB) 
                gesNLh(mstr,ieinL) = gesNs(mstr,mRB) 
                x0Lh(mstr,ieinL) = vx0s(mstr,mRB) 
                x02Lh(mstr,ieinL) = vx02s(mstr,mRB) 
                gpLh(mstr,ieinL) = gelps(mstr,mRB) 
                gesPLh(mstr,ieinL) = gesPs(mstr,mRB) 
                siLh(mstr,ieinL) = sis(mstr,mRB) 
                phLh(mstr,ieinL) = vphs(mstr,mRB) 
                caLh(mstr,ieinL) = cas(mstr,mRB) 
                elfLh(mstr,ieinL) = lfs(mstr,mRB) 
                ssLh(mstr,ieinL) = ssalgs(mstr,mRB) 
                tempLh(mstr,ieinL) = tempws(mstr,mRB) 
                o2Lh(mstr,ieinL) = vo2s(mstr,mRB) 
                coliLh(mstr,ieinL) = colis(mstr,mRB) 
                enl0Lh(mstr,ieinL) = nl0s(mstr,mRB) 
                pl0Lh(mstr,ieinL) = pl0s(mstr,mRB)
                chlaLh(mstr,ieinL) = chlas(mstr,mRB) 
!                CDL(mstr,1,iein) = CDs(mstr,1,mRB) 
!                CDL(mstr,2,iein) = CDs(mstr,2,mRB) 
!                CPL(mstr,1,iein) = CPs(mstr,1,mRB) 
!                CPL(mstr,2,iein) = CPs(mstr,2,mRB)
!                CML(mstr,iein) = CMs(mstr,mRB)
!                BACL(mstr,iein) = BACs(mstr,mRB) 
          endif 
            else
               
              if(iFlRi(mstr)==1.and.RBtyp(mstr,mRB)==0.and.nStrs(istr)==0)then  ! nStrs(mstr) Es gibt eine Randbedingung am 1. Ortspunkt eines Strangs
                mRand = 1
                mstrRB = mstr                                         ! mstrRB: StrangNummer mit Randbedingung am 1. Ortspunkt 
                mRB_1 = mRB                                           ! Nummer der Randbedingung für den ersten (letzten)Ortspunkt eines Strangs
              endif
      
              if(iFlRi(mstr)==-1.and.RBtyp(mstr,mRB)==2)then
                mRand = 2
                mstrRB = mstr
                mRB_1 = mRB
              endif                                                    
                                                          
        endif 

      enddo  ! Randbedingungsschleife 
                                                                       
      ieinsh(mstr) = iein 
      ieinLs(mstr)= ieinL 

      iwahl = 1 

!      if(nstrs(istr)==0)iwahl = 1  
      if(nstrs(istr)>0.and.j_ist==1) iwahl = 2 ! j_ist: gilt nur bei iwied=0. Zufließende Straenge sind bereits mit
                                               ! Randedingungen belegt 
      Rand_Wahl: select case (iwahl)
       
        case (1)
        inkzmx = nkzsmx 
         mRB = mRB_1

         mstr1 = mstr 
                                                       !if(iwied==0.and.iwsim/=4)then
         if(iwied==0)then

           ianze(mstr) = hanze(mstr)+1 
           iB = 1
           anzej = hanze(mstr)+1
           if(iwsim==4)anzej = 1                       ! Tracer 

             if(mRand==0.and.iwsim/=4)mstr1 = mstrRB   ! Falls keine Randbedingung fuer diesen Strang vorhanden, wird
                                                       ! dieser Strang mit der Randbedingung eines anderen
                                                       ! Strangs belegt (nicht bei Tracer)
                                                       ! else if(iwied==1.and.iwsim/=4)then
             if(mRand==0.and.iwsim==4)then
               mstr1 = mstr
               tempws(mstr1,mRB) = 0.0
             endif

            else if(iwied==1)then
              iB = 1
              anzej = 1
              mstr1 = mstr
                if(mRand/=1)anzej = 0     ! keine Belegung des 1. Ortspunkts, da keine Randbedigung vorhanden 
                  if(iflRi(mstr)==-1)then
                    IB = hanze(mstr)+1
                    anzej = hanze(mstr)+1
                    if(mRand/=2)anzej = hanze(mstr) ! keine Belegung des 1. Ortspunkts, da keine Randbedigung vorhanden

                  endif
            endif  

        do ior = iB,anzej ! Schleife ueber die Ortspunkte, Beginn 
        hsvhk(mstr,ior) = 0.0 
        hsvhg(mstr,ior) = 0.0 
        hsvhb(mstr,ior) = 0.0 
        hfssgr(mstr,ior) = fssgrs 
        hfbsgr(mstr,ior) = fbsgrs 
        hfrfgr(mstr,ior) = frfgrs(mstr1,mRB)
        hsised(mstr,ior) = 0.0 
        hdlarn(mstr,ior) = 0.0 
        hstind(mstr,ior) = 0.0

       if(RBtyp(mstr,mRB).eq.0)then 
         Wtst = -1.0 
         Wtst_T = -9.99 
           else 
             Wtst = 0.0 
             Wtst_T = -9.99 
       endif 

       if(tempws(mstr1,mRB)>Wtst_T)then
         hakbcm(mstr,ior) = akbcms(mstr1,mRB)
         hagbcm(mstr,ior) = agbcms(mstr1,mRB) 
         habbcm(mstr,ior) = abbcms(mstr1,mRB) 
       endif
                                                                       
       if(nl0s(mstr1,mRB).gt.0.0)hnl0(mstr,ior) = nl0s(mstr1,mRB) 
       if(pl0s(mstr1,mRB).gt.0.0)hpl0(mstr,ior) = pl0s(mstr1,mRB) 
       if(gesNs(mstr1,mRB).ge.0.0)hgesN(mstr,ior) = gesNs(mstr1,mRB) 
       if(gesPs(mstr1,mRB).ge.0.0)hgesP(mstr,ior) = gesPs(mstr1,mRB) 
       if(Q_NKs(mstr1,mRB).gt.0.0)hQ_NK(mstr,ior) = Q_NKs(mstr1,mRB) 
       if(Q_PKs(mstr1,mRB).gt.0.0)hQ_PK(mstr,ior) = Q_PKs(mstr1,mRB) 
       if(Q_SKs(mstr1,mRB).gt.0.0)hQ_SK(mstr,ior) = Q_SKs(mstr1,mRB) 
       if(Q_NGs(mstr1,mRB).gt.0.0)hQ_NG(mstr,ior) = Q_NGs(mstr1,mRB) 
       if(Q_PGs(mstr1,mRB).gt.0.0)hQ_PG(mstr,ior) = Q_PGs(mstr1,mRB) 
       if(Q_NBs(mstr1,mRB).gt.0.0)hQ_NB(mstr,ior) = Q_NBs(mstr1,mRB) 
       if(Q_PBs(mstr1,mRB).gt.0.0)hQ_PB(mstr,ior) = Q_PBs(mstr1,mRB) 
       if(tempws(mstr1,mRB).gt.Wtst_T)htempw(mstr,ior) = tempws(mstr1,mRB)
                             
!...Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser            
       if(iwied.eq.0)hTsed(mstr,ior) = htempw(mstr,ior) 
       if(obsbs(mstr1,mRB).ge.Wtst)hbsb(mstr,ior) = obsbs(mstr1,mRB) 
       if(ocsbs(mstr1,mRB).ge.Wtst)hcsb(mstr,ior) = ocsbs(mstr1,mRB) 
       if(CHNFs(mstr1,mRB).ge.Wtst)hCHNF(mstr,ior) = CHNFs(mstr1,mRB) 
       if(BVHNFs(mstr1,mRB).ge.Wtst)hBVHNF(mstr,ior) = BVHNFs(mstr1,mRB) 
       if(CDs(mstr1,1,mRB).ge.Wtst)hCD(mstr,1,ior) = CDs(mstr1,1,mRB) 
       if(CDs(mstr1,2,mRB).ge.Wtst)hCD(mstr,2,ior) = CDs(mstr1,2,mRB) 
       if(CPs(mstr1,1,mRB).ge.Wtst)hCP(mstr,1,ior) = CPs(mstr1,1,mRB) 
       if(CPs(mstr1,2,mRB).ge.Wtst)hCP(mstr,2,ior) = CPs(mstr1,2,mRB) 
       if(CMs(mstr1,mRB).ge.Wtst)hCM(mstr,ior) = CMs(mstr1,mRB) 
       if(BACs(mstr1,mRB).ge.Wtst)hBAC(mstr,ior) = BACs(mstr1,mRB) 
       if(vnh4s(mstr1,mRB).ge.Wtst)hnh4(mstr,ior) = vnh4s(mstr1,mRB) 
       if(vo2s(mstr1,mRB).ge.Wtst)ho2(mstr,ior) = vo2s(mstr1,mRB) 
       if(vno3s(mstr1,mRB).ge.Wtst)hno3(mstr,ior) = vno3s(mstr1,mRB) 
       if(vno2s(mstr1,mRB).ge.Wtst)hno2(mstr,ior) = vno2s(mstr1,mRB) 
       if(vx0s(mstr1,mRB).ge.Wtst)hx0(mstr,ior) = vx0s(mstr1,mRB) 
       if(vx02s(mstr1,mRB).ge.Wtst)hx02(mstr,ior) = vx02s(mstr1,mRB) 
       if(sis(mstr1,mRB).ge.Wtst)hsi(mstr,ior) = sis(mstr1,mRB) 
       if(chlas(mstr1,mRB).ge.Wtst)hchla(mstr,ior) = chlas(mstr1,mRB) 
       if(akis(mstr1,mRB).ge.Wtst)haki(mstr,ior) = akis(mstr1,mRB) 
       if(agrs(mstr1,mRB).ge.Wtst)hagr(mstr,ior) = agrs(mstr1,mRB) 
       if(abls(mstr1,mRB).ge.Wtst)habl(mstr,ior) = abls(mstr1,mRB) 
       if(chlaks(mstr1,mRB).ge.Wtst)hchlak(mstr,ior) = chlaks(mstr1,mRB) 
       if(chlags(mstr1,mRB).ge.Wtst)hchlag(mstr,ior) = chlags(mstr1,mRB) 
       if(chlabs(mstr1,mRB).ge.Wtst)hchlab(mstr,ior) = chlabs(mstr1,mRB) 
       if(vkigrs(mstr1,mRB).ge.Wtst)hvkigr(mstr,ior) = vkigrs(mstr1,mRB) 
       if(antbls(mstr1,mRB).ge.Wtst)hantbl(mstr,ior) = antbls(mstr1,mRB) 
       habrz1(mstr,ior) = 0.0 
       if(ssalgs(mstr1,mRB).ge.Wtst)hssalg(mstr,ior) = ssalgs(mstr1,mRB) 
       if(sss(mstr1,mRB).ge.Wtst)hss(mstr,ior) = sss(mstr1,mRB) 
       if(zooins(mstr1,mRB).ge.Wtst)hzooi(mstr,ior) = zooins(mstr1,mRB) 
       if(gelps(mstr1,mRB).ge.Wtst)hgelp(mstr,ior) = gelps(mstr1,mRB) 

       if(mws(mstr1,mRB).ge.Wtst)hmw(mstr,ior) = mws(mstr1,mRB) 
       if(mws(mstr1,mRB).ge.Wtst)hpw(mstr,ior) = pws(mstr1,mRB) 
       if(cas(mstr1,mRB).ge.Wtst)hca(mstr,ior) = cas(mstr1,mRB) 
       if(lfs(mstr1,mRB).ge.Wtst)hlf(mstr,ior) = lfs(mstr1,mRB) 
       if(vphs(mstr1,mRB).ge.Wtst)hph(mstr,ior) = vphs(mstr1,mRB) 
       if(colis(mstr1,mRB).ge.Wtst)then
         hcoli(mstr,ior) = colis(mstr1,mRB)
         hDOSCF(mstr,ior) = DOSCFs(mstr1,mRB)
       endif 
       if(gesPs(mstr1,mRB).ge.Wtst)hgesP(mstr,ior) = gesPs(mstr1,mRB) 
       if(gesNs(mstr1,mRB).ge.Wtst)hgesN(mstr,ior) = gesNs(mstr1,mRB)

       if(gsZns(mstr1,mRB).ge.Wtst)hgsZn(mstr,ior) = gsZns(mstr1,mRB) 
       if(glZns(mstr1,mRB).ge.Wtst)hglZn(mstr,ior) = glZns(mstr1,mRB) 
       if(gsCads(mstr1,mRB).ge.Wtst)hgsCad(mstr,ior) = gsCads(mstr1,mRB) 
       if(glCads(mstr1,mRB).ge.Wtst)hglCad(mstr,ior) = glCads(mstr1,mRB) 
       if(gsCus(mstr1,mRB).ge.Wtst)hgsCu(mstr,ior) = gsCus(mstr1,mRB) 
       if(glCus(mstr1,mRB).ge.Wtst)hglCu(mstr,ior) = glCus(mstr1,mRB) 
       if(gsNis(mstr1,mRB).ge.Wtst)hgsNi(mstr,ior) = gsNis(mstr1,mRB) 
       if(glNis(mstr1,mRB).ge.Wtst)hglNi(mstr,ior) = glNis(mstr1,mRB) 

       if(iwsim==4)cycle  ! bei Tracer wird dieser Programmteil nicht ausgeführt!
       algb5 = haki(mstr,ior)*Caki*bsbki+hagr(mstr,ior)*Cagr*bsbgr+habl(mstr,ior)*Cabl*bsbbl
       hvbsb(mstr,ior) = hbsb(mstr,ior)+algb5 

       zoobsb = (hzooi(mstr,ior)*GRote/1000.)*bsbZoo 
       hvbsb(mstr,ior) = hvbsb(mstr,ior)+zoobsb 
                                                                       
       algcs = haki(mstr,ior)*Caki*csbki+habl(mstr,ior)*Cabl*csbbl+hagr(mstr,ior)*Cagr*csbgr

       hvcsb(mstr,ior) = hcsb(mstr,ior)+algcs 
       zoocsb = hzooi(mstr,ior)*(GRote*CZoo/1000.)*TOC_BSB 
       hvcsb(mstr,ior) = hvcsb(mstr,ior)+zoocsb

       hFluN3(mstr,ior) = 0.0 
                                                                       
        do nkz = 1,hnkzs(mstr,ior)   ! Belegung des Gitters bei 2D-Modellierung, Schleifenanfang   
 
       if(tempws(mstr1,mRB).gt.(-99.99))                                 &
       &htempz(mstr,nkz,ior) = tempws(mstr1,mRB)                          
        if(vnh4s(mstr1,mRB).ge.Wtst)hnh4z(mstr,nkz,ior) = vnh4s(mstr1,mRB) 
        if(vno2s(mstr1,mRB).ge.Wtst)hno2z(mstr,nkz,ior) = vno2s(mstr1,mRB) 
        if(vno3s(mstr1,mRB).ge.Wtst)hno3z(mstr,nkz,ior) = vno3s(mstr1,mRB) 
        if(vo2s(mstr1,mRB).ge.Wtst)ho2z(mstr,nkz,ior) = vo2s(mstr1,mRB) 
        if(gelPs(mstr1,mRB).ge.Wtst)hgelPz(mstr,nkz,ior) = gelPs(mstr1,mRB)                           
        if(Sis(mstr1,mRB).ge.Wtst)hsiz(mstr,nkz,ior) = Sis(mstr1,mRB) 
        if(akis(mstr1,mRB).ge.Wtst)hakiz(mstr,nkz,ior) = akis(mstr1,mRB) 
        if(agrs(mstr1,mRB).ge.Wtst)hagrz(mstr,nkz,ior) = agrs(mstr1,mRB) 
        if(abls(mstr1,mRB).ge.Wtst)hablz(mstr,nkz,ior) = abls(mstr1,mRB) 
        if(chlas(mstr1,mRB).ge.Wtst)hchlaz(mstr,nkz,ior) = chlas(mstr1,mRB)                           
        if(chlaks(mstr1,mRB).ge.Wtst)hchlkz(mstr,nkz,ior) = chlaks(mstr1,mRB)                           
        if(chlags(mstr1,mRB).ge.Wtst)hchlgz(mstr,nkz,ior) = chlags(mstr1,mRB)                           
        if(chlabs(mstr1,mRB).ge.Wtst)hchlbz(mstr,nkz,ior) = chlabs(mstr1,mRB)                           
        if(gesPs(mstr1,mRB).ge.Wtst)hgesPz(mstr,nkz,ior) = gesPs(mstr1,mRB)                           
        if(gesNs(mstr1,mRB).ge.Wtst)hgesNz(mstr,nkz,ior) = gesNs(mstr1,mRB)
        if(Q_NKs(mstr1,mRB).ge.Wtst)hQ_NKz(mstr,nkz,ior) = Q_NKs(mstr1,mRB)
        if(Q_NBs(mstr1,mRB).ge.Wtst)hQ_NBz(mstr,nkz,ior) = Q_NBs(mstr1,mRB)
        if(Q_NGs(mstr1,mRB).ge.Wtst)hQ_NGz(mstr,nkz,ior) = Q_NGs(mstr1,mRB)

       if(tempws(mstr1,mRB)>Wtst_T)then
         hCChlkz(mstr,nkz,ior) = akbcms(mstr1,mRB)
         hCChlbz(mstr,nkz,ior) = abbcms(mstr1,mRB) 
         hCChlgz(mstr,nkz,ior) = agbcms(mstr1,mRB) 
       endif

      enddo   ! Schleifenende 

       if(nbuhn(mstr)==0.or.iwied==1)cycle 
!   Buhnenfelder                                                      
                                                                       
       bsvhek(mstr,ior) = hsvhk(mstr,ior) 
       bsvheg(mstr,ior) = hsvhg(mstr,ior) 
       bsvheb(mstr,ior) = hsvhb(mstr,ior) 
       bakbcm(mstr,ior) = hakbcm(mstr,ior) 
       babbcm(mstr,ior) = habbcm(mstr,ior) 
       bagbcm(mstr,ior) = hagbcm(mstr,ior) 
       bnl0(mstr,ior) = hnl0(mstr,ior) 
       bpl0(mstr,ior) = hpl0(mstr,ior) 
       bgesN(mstr,ior) = hgesN(mstr,ior) 
       bgesP(mstr,ior) = hgesP(mstr,ior) 
       bstind(mstr,ior) = hstind(mstr,ior) 
       btempw(mstr,ior) = htempw(mstr,ior) 
!...Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser            
       if(iwied==0)bTsed(mstr,ior) = htempw(mstr,ior) 
       bbsb(mstr,ior) = hbsb(mstr,ior) 
       bcsb(mstr,ior) = hcsb(mstr,ior) 
       bnh4(mstr,ior) = hnh4(mstr,ior) 
       bo2(mstr,ior) = ho2(mstr,ior) 
       bno3(mstr,ior) = hno3(mstr,ior) 
       bno2(mstr,ior) = hno2(mstr,ior) 
       bx0(mstr,ior) = hx0(mstr,ior) 
       bx02(mstr,ior) = hx02(mstr,ior) 
       bsi(mstr,ior) = hsi(mstr,ior) 
       bsised(mstr,ior) = hsised(mstr,ior) 
       bSKmor(mstr,ior) = hSKmor(mstr,ior) 
       bchla(mstr,ior) = hchla(mstr,ior) 
       baki(mstr,ior) = haki(mstr,ior) 
       bagr(mstr,ior) = hagr(mstr,ior) 
       babl(mstr,ior) = habl(mstr,ior) 
       bchlak(mstr,ior) = hchlak(mstr,ior) 
       bchlag(mstr,ior) = hchlag(mstr,ior) 
       bchlab(mstr,ior) = hchlab(mstr,ior) 
       bvkigr(mstr,ior) = hvkigr(mstr,ior) 
       bantbl(mstr,ior) = hantbl(mstr,ior) 
       babrz1(mstr,ior) = habrz1(mstr,ior) 
       bssalg(mstr,ior) = hssalg(mstr,ior) 
       bfssgr(mstr,ior) = hfssgr(mstr,ior) 
       bfbsgr(mstr,ior) = hfbsgr(mstr,ior) 
       bfrfgr(mstr,ior) = hfrfgr(mstr,ior) 
       bss(mstr,ior) = hss(mstr,ior) 
       bzooi(mstr,ior) = hzooi(mstr,ior) 
       bgelp(mstr,ior) = hgelp(mstr,ior) 
       bmw(mstr,ior) = hmw(mstr,ior) 
       bpw(mstr,ior) = hpw(mstr,ior) 
       bca(mstr,ior) = hca(mstr,ior) 
       blf(mstr,ior) =hlf(mstr,ior) 
       bdlarn(mstr,ior) = hdlarn(mstr,ior)
       bph(mstr,ior) = hph(mstr,ior) 
       bvbsb(mstr,ior) = hvbsb(mstr,ior) 
       bvcsb(mstr,ior) = hvcsb(mstr,ior) 
       bCD(mstr,1,ior) = hCD(mstr,1,ior) 
       bCD(mstr,2,ior) = hCD(mstr,2,ior) 
       bCP(mstr,1,ior) = hCP(mstr,1,ior) 
       bCP(mstr,2,ior) = hCP(mstr,2,ior) 
       bCM(mstr,ior) = hCM(mstr,ior) 
       bBAC(mstr,ior) = hBAC(mstr,ior)
       bCHNF(mstr,ior) = hCHNF(mstr,ior) 
       bQ_PK(mstr,ior) = hQ_PK(mstr,ior) 
       bQ_NK(mstr,ior) = hQ_NK(mstr,ior) 
       bQ_SK(mstr,ior) = hQ_SK(mstr,ior) 
       bQ_PG(mstr,ior) = hQ_PG(mstr,ior) 
       bQ_NG(mstr,ior) = hQ_NG(mstr,ior) 
       bQ_PB(mstr,ior) = hQ_PB(mstr,ior) 
       bQ_NB(mstr,ior) = hQ_NB(mstr,ior)
       bFluN3(mstr,ior) = hFluN3(mstr,ior) 
       bcoli(mstr,ior) = hcoli(mstr,ior) 
       bDOSCF(mstr,ior) = hDOSCF(mstr,ior)

       bgsZn(mstr,ior) = hgsZn(mstr,ior) 
       bglZn(mstr,ior) = hglZn(mstr,ior) 
       bgsCad(mstr,ior) = hgsCad(mstr,ior) 
       bglCad(mstr,ior) = hglCad(mstr,ior) 
       bgsCu(mstr,ior) = hgsCu(mstr,ior) 
       bglCu(mstr,ior) = hglCu(mstr,ior) 
       bgsNi(mstr,ior) = hgsNi(mstr,ior) 
       bglNi(mstr,ior) = hglNi(mstr,ior) 
                                                                
      enddo ! Ende Schleife über die Ortspunkte 
     cycle 

      case(2) ! Strang hat Vor- bzw. Nachstränge
      if(iwied==0)ianze(mstr) = hanze(mstr)+1 
     
      hcs1 = 0.0 
      hcs2 = 0.0 
      hcs3 = 0.0 
      hcs6 = 0.0 
      hcs7 = 0.0 
      hcs8 = 0.0 
      hcs9 = 0.0 
      hcs10 = 0.0 
      hcs20 = 0.0 
      hcs21 = 0.0 
      hcs22 = 0.0 
      hcs23 = 0.0 
      hcs24 = 0.0 
      hcs25 = 0.0 
      hcs26 = 0.0 
      hcs27 = 0.0 
      hcs28 = 0.0 
      hcs29 = 0.0 
      hcs30 = 0.0 
      hcs31 = 0.0 
      hcs32 = 0.0 
      hcs33 = 0.0 
      hcs34 = 0.0 
      hcs35 = 0.0 
      hcs36 = 0.0 
      hcs37 = 0.0 
      hcs38 = 0.0 
      hcs39 = 0.0 
      hcs40 = 0.0 
      hcs41 = 0.0 
      hcs42 = 0.0 
      hcs43 = 0.0 
      hcs44 = 0.0 
      hcs45 = 0.0 
      hcs46 = 0.0 
      hcs47 = 0.0 
      hcs48 = 0.0 
      hcs49 = 0.0 
      hcs50 = 0.0 
      hcs51 = 0.0 
      hcs52 = 0.0 
      hcs53 = 0.0 
      hcs54 = 0.0 
      hcs55 = 0.0 
      hcs56 = 0.0 
      hcs57 = 0.0 
      hcs58 = 0.0 
      hcs59 = 0.0 
      hcs60 = 0.0 
      hcs61 = 0.0 
      hcs62 = 0.0 
      hcs63 = 0.0 
      hcs64 = 0.0 
      hcs65 = 0.0 
      hcs66 = 0.0 
                                                                       
      do nkz = 1,inkzmx  ! 2D 
        hcs67(nkz) = 0.0 
        hcs68(nkz) = 0.0 
        hcs69(nkz) = 0.0 
        hcs70(nkz) = 0.0 
        hcs71(nkz) = 0.0 
        hcs72(nkz) = 0.0 
        hcs73(nkz) = 0.0 
        hcs74(nkz) = 0.0 
        hcs75(nkz) = 0.0 
        hcs76(nkz) = 0.0 
        hcs84(nkz) = 0.0 
        hcs87(nkz) = 0.0 
        hcs88(nkz) = 0.0 
        hcs89(nkz) = 0.0 
        hcs90(nkz) = 0.0 
        hcs91(nkz) = 0.0 
        hcs92(nkz) = 0.0 
        hcs93(nkz) = 0.0 
        hcs94(nkz) = 0.0 

      enddo
                                                                       
      hcs77 = 0.0 
      hcs78 = 0.0 
      hcs79 = 0.0 
      hcs80 = 0.0 
      hcs81 = 0.0 
      hcs82 = 0.0 
      hcs83 = 0.0 
      hcs85 = 0.0 
      hcs86 = 0.0
      hcs95 = 0.0 
      hcs96 = 0.0 
      hcs97 = 0.0 
      hcs98 = 0.0
      hcs99 = 0.0
      hcs100 = 0.0 
      hcs101 = 0.0 
      hcs102 = 0.0 
      hcs103 = 0.0 
      hcs104 = 0.0 
      hcs105 = 0.0 
      hcs106 = 0.0 
      hcs107 = 0.0 
      hcs108 = 0.0
      hcs110 = 0.0 
      hcs111 = 0.0 
      hcs112 = 0.0 
                                                                       
      hcq = 0.0 

      do nstr = 1,nstrs(istr)  !Schleife ueber die Anzahl der Vor-, bzw. Nachstraenge 

                                                                      
!....jnkz = 1 > Werte am ersten Knoten im Strang                        
!....jnkz = 2 > Werte am letzten Knoten im Strang                       
!++++von Hier

      if(iflRi(mstr)==0.and.iwied==1)cycle
      if(iflRi(ESTRNR(istr,nstr))==0.and.iwied==1)cycle
      kanz = ianze(ESTRNR(istr,nstr)) 
      iSta = mStas(ESTRNR(istr,nstr))
      iB = 1
      anzej = 1
      if(iwied==0)anzej = hanze(mstr)+1
      jnkz = 2

      if(iFlRi(mstr)==-1.and.iflRi(ESTRNR(istr,nstr))==-1)then
        kanz = 1
        ista = 1
        iB = hanze(mstr)+1
        if(iwied==0)iB = 1
        anzej = hanze(mstr)+1
        jnkz = 1
      endif

      if(iFlRi(mstr)==1.and.iflRi(ESTRNR(istr,nstr))==-1)then
        kanz = 1
        ista = 1
        iB = 1
        anzej = 1
        if(iwied==0)anzej = hanze(mstr)+1
        jnkz = 1
      endif

      if(iFlRi(mstr)==-1.and.iflRi(ESTRNR(istr,nstr))==1)then
        kanz = ianze(ESTRNR(istr,nstr))
        ista = mStas(ESTRNR(istr,nstr))
        iB = hanze(mstr)+1
        anzej = hanze(mstr)+1
        if(iwied==0)iB = 1
        jnkz = 2
      endif
!+++bis hier in QSim13_20 

      if(iwied==0.and.iwsim/=4)then
!    2D-Modellierung (Gitterbelegung zu Beginn der Simulation)          
!......wird sonst übersprungen, ebenso bei Tracer 
                                                                       
      kanz2 = 1
      jnkz2 = 1

      do kanz1=1,2
      do nkz = 1,hnkzs(ESTRNR(istr,nstr),kanz) ! Schleife ueber die vertikalen Schichten am Stranganfang und -Ende
 
        Tzt(ESTRNR(istr,nstr),nkz,jnkz2) = htempz(ESTRNR(istr,nstr),1      &
       &,kanz2)                                                            
        o2zt(ESTRNR(istr,nstr),nkz,jnkz2) = ho2z(ESTRNR(istr,nstr),1       &
       &,kanz2)                                                            
        NH4zt(ESTRNR(istr,nstr),nkz,jnkz2) = hnh4z(ESTRNR(istr,nstr),1     &
       &,kanz2)                                                            
        NO2zt(ESTRNR(istr,nstr),nkz,jnkz2) = hno2z(ESTRNR(istr,nstr),1     &
       &,kanz2)                                                            
        NO3zt(ESTRNR(istr,nstr),nkz,jnkz2) = hno3z(ESTRNR(istr,nstr),1     &
       &,kanz2)                                                            
        Pzt(ESTRNR(istr,nstr),nkz,jnkz2) = hgelpz(ESTRNR(istr,nstr),1      &
       &,kanz2)                                                            
        gSizt(ESTRNR(istr,nstr),nkz,jnkz2) = hsiz(ESTRNR(istr,nstr),1      &
       &,kanz2)                                                            
        akizt(ESTRNR(istr,nstr),nkz,jnkz2) = hakiz(ESTRNR(istr,nstr),1     &
       &,kanz2)                                                            
        agrzt(ESTRNR(istr,nstr),nkz,jnkz2) = hagrz(ESTRNR(istr,nstr),1     &
       &,kanz2)                                                            
        ablzt(ESTRNR(istr,nstr),nkz,jnkz2) = hablz(ESTRNR(istr,nstr),1     &
       &,kanz2)                                                            
        chlazt(ESTRNR(istr,nstr),nkz,jnkz2) = hchlaz(ESTRNR(istr,nstr),1   &
       &,kanz2)
        chlkzt(ESTRNR(istr,nstr),nkz,jnkz2) = hchlkz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
         chlgzt(ESTRNR(istr,nstr),nkz,jnkz2) = hchlgz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
        chlbzt(ESTRNR(istr,nstr),nkz,jnkz2) = hchlbz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
        gesPzt(ESTRNR(istr,nstr),nkz,jnkz2) = hgesPz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
        gesNzt(ESTRNR(istr,nstr),nkz,jnkz2) = hgesNz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
        Q_NKzt(ESTRNR(istr,nstr),nkz,jnkz2) = hQ_NKz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
        Q_NBzt(ESTRNR(istr,nstr),nkz,jnkz2) = hQ_NBz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
        Q_NGzt(ESTRNR(istr,nstr),nkz,jnkz2) = hQ_NGz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
        CChlkzt(ESTRNR(istr,nstr),nkz,jnkz2) = hCChlkz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
         CChlbzt(ESTRNR(istr,nstr),nkz,jnkz2) = hCChlbz(ESTRNR(istr,nstr),1   &       
       &,kanz2)
        CChlgzt(ESTRNR(istr,nstr),nkz,jnkz2) = hCChlgz(ESTRNR(istr,nstr),1   &       
       &,kanz2)



      enddo  ! Schleifenende  

      jnkz2 = 2
      kanz2 = ianze(ESTRNR(istr,nstr))

     enddo
     endif

      if(iwsim/=4)then

! ....Einfluss der Wehre auf O2,pH und Temperatur,Chla,Algen,Stickstoff und Phosphor                       
!                                                                       
      call WEHR(wehrh,wehrb,ho2,hQaus,O2zt,htempw,ho2_z,ho2z_z,hlf,hpw,hmw,hph,hph_z,iph                   &                                   
          ,tzt,hte_z,htez_z,chlazt,hchlaz_z,akizt,hakiz_z,agrzt,hagrz_z,ablzt,hablz_z                      &
          ,NH4zt,hNH4z_z,NO2zt,hNO2z_z,NO3zt,hNO3z_z,Pzt,hPz_z,gSizt,hsiz_z,chlkzt,hchlkz_z                &
          ,chlgzt,hchlgz_z,chlbzt,hchlbz_z,gesPzt,hgesPz_z,gesNzt,hgesNz_z,Q_NKzt,hQ_NKz_z                 &
          ,Q_NBzt,hQ_NBz_z,Q_NGzt,hQ_NGz_z,dH2D,ESTRNR,kanz,inkzmx,iSta,nstr,istr,jnkz,iflRi,jlWO2         &
          ,CChlkzt,hCChlkz_z,CChlbzt,hCChlbz_z,CChlgzt,hCChlgz_z,janzWS,janzWt,hnkzs,mwehr,mstr            &
          ,WSP_UW,WSP_OW,iB,azStrs)                                                           
    endif

      hcs1 = hcs1+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
     &*hsvhk(ESTRNR(istr,nstr),kanz)                                    
      hcs2 = hcs2+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
     &*hsvhg(ESTRNR(istr,nstr),kanz)                                    
      hcs3 = hcs3+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
     &*hsvhb(ESTRNR(istr,nstr),kanz)                                    
      hcs6 = hcs6+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
     &*hakbcm(ESTRNR(istr,nstr),kanz)                                   
      hcs7 = hcs7+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
     &*hagbcm(ESTRNR(istr,nstr),kanz)                                   
      hcs8 = hcs8+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
     &*habbcm(ESTRNR(istr,nstr),kanz)                                   
      hcs9 = hcs9+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
     &*hgesN(ESTRNR(istr,nstr),kanz)                                    
      hcs10 = hcs10+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hgesP(ESTRNR(istr,nstr),kanz)                                    
      hcs20 = hcs20+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hfssgr(ESTRNR(istr,nstr),kanz)                                   
      hcs21 = hcs21+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hfbsgr(ESTRNR(istr,nstr),kanz)                                   
      hcs22 = hcs22+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hfrfgr(ESTRNR(istr,nstr),kanz)                                   
      hcs23 = hcs23+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hnl0(ESTRNR(istr,nstr),kanz)                                     
      hcs24 = hcs24+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hpl0(ESTRNR(istr,nstr),kanz)                                     
      hcs25 = hcs25+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*htempw(ESTRNR(istr,nstr),kanz) 
      hcs26 = hcs26+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hbsb(ESTRNR(istr,nstr),kanz)                                     
      hcs27 = hcs27+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hcsb(ESTRNR(istr,nstr),kanz)                                     
      hcs28 = hcs28+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hCHNF(ESTRNR(istr,nstr),kanz)                                    
      hcs29 = hcs29+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hBVHNF(ESTRNR(istr,nstr),kanz)                                   
      hcs30 = hcs30+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hCD(ESTRNR(istr,nstr),1,kanz)                                    
      hcs31 = hcs31+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hCD(ESTRNR(istr,nstr),2,kanz)                                    
      hcs32 = hcs32+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hCP(ESTRNR(istr,nstr),1,kanz)                                    
      hcs33 = hcs33+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hCP(ESTRNR(istr,nstr),2,kanz)                                    
      hcs34 = hcs34+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hCM(ESTRNR(istr,nstr),kanz)                                      
      hcs35 = hcs35+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hBAC(ESTRNR(istr,nstr),kanz)                                     
      hcs39 = hcs39+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hnh4(ESTRNR(istr,nstr),kanz)
      hcs40 = hcs40+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*ho2(ESTRNR(istr,nstr),kanz)                                      
      hcs41 = hcs41+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hno3(ESTRNR(istr,nstr),kanz)                                     
      hcs42 = hcs42+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hno2(ESTRNR(istr,nstr),kanz)                                     
      hcs43 = hcs43+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hx0(ESTRNR(istr,nstr),kanz)                                      
      hcs44 = hcs44+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hx02(ESTRNR(istr,nstr),kanz)                                     
      hcs45 = hcs45+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hsi(ESTRNR(istr,nstr),kanz)                                      
      hcs46 = hcs46+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hchla(ESTRNR(istr,nstr),kanz)                                    
      hcs47 = hcs47+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*haki(ESTRNR(istr,nstr),kanz)                                     
      hcs48 = hcs48+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hagr(ESTRNR(istr,nstr),kanz)                                     
      hcs49 = hcs49+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*habl(ESTRNR(istr,nstr),kanz)                                     
      hcs50 = hcs50+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hchlak(ESTRNR(istr,nstr),kanz)                                   
      hcs51 = hcs51+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hchlag(ESTRNR(istr,nstr),kanz)                                   
      hcs52 = hcs52+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hchlab(ESTRNR(istr,nstr),kanz)                                   
      hcs53 = hcs53+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hvkigr(ESTRNR(istr,nstr),kanz)                                   
      hcs54 = hcs54+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hantbl(ESTRNR(istr,nstr),kanz)                                   
      hcs55 = hcs55+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hssalg(ESTRNR(istr,nstr),kanz)                                   
      hcs56 = hcs56+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hss(ESTRNR(istr,nstr),kanz)                                      
      hcs57 = hcs57+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hzooi(ESTRNR(istr,nstr),kanz)                                    
      hcs58 = hcs58+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hgelp(ESTRNR(istr,nstr),kanz)                                    
      hcs59 = hcs59+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hmw(ESTRNR(istr,nstr),kanz)                                      
      hcs60 = hcs60+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hpw(ESTRNR(istr,nstr),kanz)                                      
      hcs61 = hcs61+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hca(ESTRNR(istr,nstr),kanz)                                      
      hcs62 = hcs62+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hlf(ESTRNR(istr,nstr),kanz)                                      
!                                                                       
!..pH-Wert in H+-umrechnen!!!                                           
!                                                                       
      hmue = 1.7e-5*hlf(ESTRNR(istr,nstr),kanz) 
      if(hmue.lt.0.0)hmue = 0.0 
      hk = (0.5*sqrt(hmue))/(1.+1.4*sqrt(hmue)) 
      lgh = hph(ESTRNR(istr,nstr),kanz)-hk 
      vhplus = 10**(-lgh) 
!                                                                       
      hcs63 = hcs63+abs(hQaus(ESTRNR(istr,nstr),iSta))*vhplus                                                           
      hcs64 = hcs64+abs(hQaus(ESTRNR(istr,nstr),iSta))*hcoli(ESTRNR(istr,nstr),kanz)                                    
      hcs100 = hcs64+abs(hQaus(ESTRNR(istr,nstr),iSta))*hDOSCF(ESTRNR(istr,nstr),kanz)                                    

      hcs65 = hcs65+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hvbsb(ESTRNR(istr,nstr),kanz)                                    
      hcs66 = hcs66+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hvcsb(ESTRNR(istr,nstr),kanz)                                    
      hcs77 = hcs77+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hQ_NK(ESTRNR(istr,nstr),kanz)                                    
      hcs78 = hcs78+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hQ_PK(ESTRNR(istr,nstr),kanz)                                    
      hcs79 = hcs79+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hQ_SK(ESTRNR(istr,nstr),kanz)                                    
      hcs80 = hcs80+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hQ_NG(ESTRNR(istr,nstr),kanz)                                    
      hcs81 = hcs81+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hQ_PG(ESTRNR(istr,nstr),kanz)                                    
      hcs82 = hcs82+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hQ_NB(ESTRNR(istr,nstr),kanz)                                    
      hcs83 = hcs83+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hQ_PB(ESTRNR(istr,nstr),kanz)                                    
      hcs85 = hcs85+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hSKmor(ESTRNR(istr,nstr),kanz)                                   
      hcs86 = hcs86+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hDOSCF(ESTRNR(istr,nstr),kanz)                                   
      hcs95 = hcs95+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
     &*hFluN3(ESTRNR(istr,nstr),kanz)                                   
      hcs99 = hcs99+abs(hQaus(ESTRNR(istr,nstr),iSta))*TGZoo(ESTRNR(istr,nstr),kanz)                                   
      hcs110 = hcs110+abs(hQaus(ESTRNR(istr,nstr),iSta))*akmor_1(ESTRNR(istr,nstr),kanz)
      hcs111 = hcs111+abs(hQaus(ESTRNR(istr,nstr),iSta))*agmor_1(ESTRNR(istr,nstr),kanz)
      hcs112 = hcs112+abs(hQaus(ESTRNR(istr,nstr),iSta))*abmor_1(ESTRNR(istr,nstr),kanz)
                                                                       
     hcs101 = hcs101+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsZn(ESTRNR(istr,nstr),kanz)                                    
     hcs102 = hcs102+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglZn(ESTRNR(istr,nstr),kanz)                                    
     hcs103 = hcs103+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsCad(ESTRNR(istr,nstr),kanz)                                    
     hcs104 = hcs104+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglCad(ESTRNR(istr,nstr),kanz)                                    
     hcs105 = hcs105+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsCu(ESTRNR(istr,nstr),kanz)                                    
     hcs106 = hcs106+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglCu(ESTRNR(istr,nstr),kanz)                                    
     hcs107 = hcs107+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsNi(ESTRNR(istr,nstr),kanz)                                    
     hcs108 = hcs108+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglNi(ESTRNR(istr,nstr),kanz)                                    

                                                                       
!......2D-Modellierung                                                  

!    2D-Gitterbelegung des Strangs mit Werten der Vor- bzw. Nachsträngen


         nkzs_hc = hnkzs(mstr,iB)   
         nkzs_hc1 = hnkzs(ESTRNR(istr,nstr),kanz)

        if(nkzs_hc>1.and.nkzs_hc1>1)then
          i_EstRNR = ESTRNR(istr,nstr)
          call  sys_gitterStrang(mstr,nkzs_hc,nkzs_hc1,dH2D,tzt,o2zt,NH4zt                                              &
                                 ,no2zt,no3zt,Pzt,gSizt,akizt,agrzt,ablzt,chlazt,chlkzt,chlgzt,chlbzt,gesPzt,gesNzt             &
                                 ,Q_NKzt, Q_NBzt, Q_NGzt, CChlkzt,CChlbzt,CChlgzt, jnkz,i_EstRNR,itags,monats,uhrz,azStrs)

        endif 

      do nkz = 1,nkzs_hc  ! 2D_modellierung, Schleifenbeginn
        hcs67(nkz) = hcs67(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*Tzt(ESTRNR(istr,nstr),nkz,jnkz)                           
        hcs68(nkz) = hcs68(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*O2zt(ESTRNR(istr,nstr),nkz,jnkz)                          
        hcs69(nkz) = hcs69(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*NH4zt(ESTRNR(istr,nstr),nkz,jnkz)                         
        hcs70(nkz) = hcs70(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*NO2zt(ESTRNR(istr,nstr),nkz,jnkz)                         
        hcs71(nkz) = hcs71(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*NO3zt(ESTRNR(istr,nstr),nkz,jnkz)                         
        hcs72(nkz) = hcs72(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*Pzt(ESTRNR(istr,nstr),nkz,jnkz)                           
        hcs73(nkz) = hcs73(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*gSizt(ESTRNR(istr,nstr),nkz,jnkz)                         
        hcs74(nkz) = hcs74(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*akizt(ESTRNR(istr,nstr),nkz,jnkz)                         
        hcs75(nkz) = hcs75(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*agrzt(ESTRNR(istr,nstr),nkz,jnkz)                         
        hcs76(nkz) = hcs76(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*ablzt(ESTRNR(istr,nstr),nkz,jnkz)                         
        hcs84(nkz) = hcs84(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*chlazt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs87(nkz) = hcs87(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*chlkzt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs88(nkz) = hcs88(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*chlgzt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs89(nkz) = hcs89(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*chlbzt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs90(nkz) = hcs90(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*gesPzt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs91(nkz) = hcs91(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*gesNzt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs92(nkz) = hcs92(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*Q_NKzt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs93(nkz) = hcs93(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*Q_NBzt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs94(nkz) = hcs94(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*Q_NGzt(ESTRNR(istr,nstr),nkz,jnkz)
        hcs96(nkz) = hcs96(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*CChlkzt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs97(nkz) = hcs97(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*CChlbzt(ESTRNR(istr,nstr),nkz,jnkz)                        
        hcs98(nkz) = hcs98(nkz)+abs(hQaus(ESTRNR(istr,nstr),iSta))*CChlgzt(ESTRNR(istr,nstr),nkz,jnkz)                        
     enddo  ! Schleifenende 
                                                                     
       hcq = hcq+abs(hqaus(ESTRNR(istr,nstr),iSta)) 
                                                                       
!...Umspeichern der Daten am Ende des Wehrstrangs                       
!....Wehrbelüftung wird am letzten Knoten des oberstromigen Strangs (OW)
!..(Berechnung erfolgt in der Subroutine <WEHR>                         
!... nach Übergabe an den unterstromigen Strang (UW) müssen die Werte ohne
! ...Wehrüberfall wieder am letzten Knoten im oberstromigen Strang gesetzt werden
!....ho2_z in hO2                                                       

!..Bei Tracer-Berechnung werden die Wehre nicht berücksichtigt          
      if(iwsim==4)cycle 
      htempw(ESTRNR(istr,nstr),kanz) = hte_z(ESTRNR(istr,nstr)) 
      ho2(ESTRNR(istr,nstr),kanz) = ho2_z(ESTRNR(istr,nstr)) 
      hph(ESTRNR(istr,nstr),kanz) = hph_z(ESTRNR(istr,nstr)) 
!                                                                       
      do nkz = 1,hnkzs(ESTRNR(istr,nstr),kanz)  ! Schleifenbeginn; falls ein Strang in mehrere Stränge mündet

        Tzt(ESTRNR(istr,nstr),nkz,jnkz) = htez_z(ESTRNR(istr,nstr),nkz) 
        o2zt(ESTRNR(istr,nstr),nkz,jnkz) = ho2z_z(ESTRNR(istr,nstr),nkz) 
        chlazt(ESTRNR(istr,nstr),nkz,jnkz) = hchlaz_z(ESTRNR(istr,nstr),nkz) 
        akizt(ESTRNR(istr,nstr),nkz,jnkz) = hakiz_z(ESTRNR(istr,nstr),nkz) 
        agrzt(ESTRNR(istr,nstr),nkz,jnkz) = hagrz_z(ESTRNR(istr,nstr),nkz) 
        ablzt(ESTRNR(istr,nstr),nkz,jnkz) = hablz_z(ESTRNR(istr,nstr),nkz) 
        NH4zt(ESTRNR(istr,nstr),nkz,jnkz) = hNH4z_z(ESTRNR(istr,nstr),nkz) 
        NO2zt(ESTRNR(istr,nstr),nkz,jnkz) = hNO2z_z(ESTRNR(istr,nstr),nkz) 
        NO3zt(ESTRNR(istr,nstr),nkz,jnkz) = hNO3z_z(ESTRNR(istr,nstr),nkz) 
        Pzt(ESTRNR(istr,nstr),nkz,jnkz) = hPz_z(ESTRNR(istr,nstr),nkz) 
        gSizt(ESTRNR(istr,nstr),nkz,jnkz) = hSiz_z(ESTRNR(istr,nstr),nkz) 
        chlkzt(ESTRNR(istr,nstr),nkz,jnkz) = hchlkz_z(ESTRNR(istr,nstr),nkz) 
        chlgzt(ESTRNR(istr,nstr),nkz,jnkz) = hchlgz_z(ESTRNR(istr,nstr),nkz) 
        chlbzt(ESTRNR(istr,nstr),nkz,jnkz) = hchlbz_z(ESTRNR(istr,nstr),nkz) 
        gesPzt(ESTRNR(istr,nstr),nkz,jnkz) = hgesPz_z(ESTRNR(istr,nstr),nkz) 
        gesNzt(ESTRNR(istr,nstr),nkz,jnkz) = hgesNz_z(ESTRNR(istr,nstr),nkz) 
        Q_NKzt(ESTRNR(istr,nstr),nkz,jnkz) = hQ_NKz_z(ESTRNR(istr,nstr),nkz) 
        Q_NBzt(ESTRNR(istr,nstr),nkz,jnkz) = hQ_NBz_z(ESTRNR(istr,nstr),nkz) 
        Q_NGzt(ESTRNR(istr,nstr),nkz,jnkz) = hQ_NGz_z(ESTRNR(istr,nstr),nkz) 
        CChlkzt(ESTRNR(istr,nstr),nkz,jnkz) = hCChlkz_z(ESTRNR(istr,nstr),nkz) 
        CChlbzt(ESTRNR(istr,nstr),nkz,jnkz) = hCChlbz_z(ESTRNR(istr,nstr),nkz) 
        CChlgzt(ESTRNR(istr,nstr),nkz,jnkz) = hCChlgz_z(ESTRNR(istr,nstr),nkz) 

      enddo  ! Schleifenende
                                                                        
   enddo  ! Schleife ueber die Anzahl der Vor-, bzw. Nachstraenge 
                                                                       
      if(hcq>0.0)then
 
      hcs1 = hcs1/hcq 
      hcs2 = hcs2/hcq 
      hcs3 = hcs3/hcq 
      hcs6 = hcs6/hcq 
      hcs7 = hcs7/hcq 
      hcs8 = hcs8/hcq 
      hcs9 = hcs9/hcq 
      hcs10 = hcs10/hcq 
      hcs20 = hcs20/hcq 
      hcs21 = hcs21/hcq 
      hcs22 = hcs22/hcq 
      hcs23 = hcs23/hcq 
      hcs24 = hcs24/hcq 
      hcs25 = hcs25/hcq 
      hcs26 = hcs26/hcq 
      hcs27 = hcs27/hcq 
      hcs28 = hcs28/hcq 
      hcs29 = hcs29/hcq 
      hcs30 = hcs30/hcq 
      hcs31 = hcs31/hcq 
      hcs32 = hcs32/hcq 
      hcs33 = hcs33/hcq 
      hcs34 = hcs34/hcq 
      hcs35 = hcs35/hcq 
      hcs36 = hcs36/hcq 
      hcs37 = hcs37/hcq 
      hcs38 = hcs38/hcq 
      hcs39 = hcs39/hcq 
      hcs40 = hcs40/hcq 
      hcs41 = hcs41/hcq 
      hcs42 = hcs42/hcq 
      hcs43 = hcs43/hcq 
      hcs44 = hcs44/hcq 
      hcs45 = hcs45/hcq 
      hcs46 = hcs46/hcq 
      hcs47 = hcs47/hcq 
      hcs48 = hcs48/hcq 
      hcs49 = hcs49/hcq
      hcs50 = hcs50/hcq 
      hcs51 = hcs51/hcq 
      hcs52 = hcs52/hcq 
      hcs53 = hcs53/hcq 
      hcs54 = hcs54/hcq 
      hcs55 = hcs55/hcq 
      hcs56 = hcs56/hcq 
      hcs57 = hcs57/hcq 
      hcs58 = hcs58/hcq 
      hcs59 = hcs59/hcq 
      hcs60 = hcs60/hcq 
      hcs61 = hcs61/hcq 
      hcs62 = hcs62/hcq 
      hcs63 = hcs63/hcq 
!....Umrechnung von H+ in pH-Wert                                       
      hmue = 1.7e-5*hcs62 
      if(hmue.lt.0.0)hmue = 0.0 
      hk = (0.5*sqrt(hmue))/(1.+1.4*sqrt(hmue)) 
      hcs63 = log10(hcs63) 
      hcs63 = (-1.*hcs63)+hk 
                                                                       
      hcs64 = hcs64/hcq 
      hcs100 = hcs100/hcq 
      hcs65 = hcs65/hcq 
      hcs66 = hcs66/hcq 
                                                                       
      do nkz = 1,nkzs_hc !2D-Modellierung, Schleifenanfang
        hcs67(nkz) = hcs67(nkz)/hcq 
        hcs68(nkz) = hcs68(nkz)/hcq 
        hcs69(nkz) = hcs69(nkz)/hcq 
        hcs70(nkz) = hcs70(nkz)/hcq 
        hcs71(nkz) = hcs71(nkz)/hcq 
        hcs72(nkz) = hcs72(nkz)/hcq 
        hcs73(nkz) = hcs73(nkz)/hcq 
        hcs74(nkz) = hcs74(nkz)/hcq 
        hcs75(nkz) = hcs75(nkz)/hcq 
        hcs76(nkz) = hcs76(nkz)/hcq 
        hcs84(nkz) = hcs84(nkz)/hcq 
        hcs87(nkz) = hcs87(nkz)/hcq 
        hcs88(nkz) = hcs88(nkz)/hcq 
        hcs89(nkz) = hcs89(nkz)/hcq 
        hcs90(nkz) = hcs90(nkz)/hcq 
        hcs91(nkz) = hcs91(nkz)/hcq 
        hcs92(nkz) = hcs92(nkz)/hcq 
        hcs93(nkz) = hcs93(nkz)/hcq 
        hcs94(nkz) = hcs94(nkz)/hcq 
        hcs96(nkz) = hcs96(nkz)/hcq 
        hcs97(nkz) = hcs97(nkz)/hcq 
        hcs98(nkz) = hcs98(nkz)/hcq 
      enddo ! Schleifenende 2D 
                                                                       
      hcs77 = hcs77/hcq 
      hcs78 = hcs78/hcq 
      hcs79 = hcs79/hcq 
      hcs80 = hcs80/hcq 
      hcs81 = hcs81/hcq 
      hcs82 = hcs82/hcq 
      hcs83 = hcs83/hcq 
      hcs85 = hcs85/hcq 
      hcs86 = hcs86/hcq 
      hcs95 = hcs95/hcq
      hcs99 = hcs99/hcq
      hcs110 = hcs110/hcq
      hcs111 = hcs111/hcq
      hcs112 = hcs112/hcq
      hcs101 = hcs101/hcq
      hcs102 = hcs102/hcq
      hcs103 = hcs103/hcq
      hcs104 = hcs104/hcq
      hcs105 = hcs105/hcq
      hcs106 = hcs106/hcq
      hcs107 = hcs107/hcq
      hcs108 = hcs108/hcq
     

      do ior = iB,anzej ! Beginn Schleife Belegung des 1. oder letzten Ortspunkts eines Strangs
      hsvhk(mstr,ior) = hcs1 
      hsvhg(mstr,ior) = hcs2 
      hsvhb(mstr,ior) = hcs3 
      hgesN(mstr,ior) = hcs9 
      hgesP(mstr,ior) = hcs10 
      hfssgr(mstr,ior) = hcs20 
      hfbsgr(mstr,ior) = hcs21 
      hfrfgr(mstr,ior) = hcs22 
      hnl0(mstr,ior) = hcs23 
      hpl0(mstr,ior) = hcs24 
      htempw(mstr,ior) = hcs25

!...Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser            
      if(iwied.eq.0)hTsed(mstr,ior) = htempw(mstr,ior) 
      hbsb(mstr,ior) = hcs26 
      hcsb(mstr,ior) = hcs27 
      hCHNF(mstr,ior) = hcs28 
      hBVHNF(mstr,ior) = hcs29 
      hCD(mstr,1,ior) = hcs30 
      hCD(mstr,2,ior) = hcs31 
      hCP(mstr,1,ior) = hcs32 
      hCP(mstr,2,ior) = hcs33 
      hCM(mstr,ior) = hcs34 
      hBAC(mstr,ior) = hcs35 
      hnh4(mstr,ior) = hcs39 
      ho2(mstr,ior) = hcs40 
      hno3(mstr,ior) = hcs41 
      hno2(mstr,ior) = hcs42 
      hx0(mstr,ior) = hcs43 
      hx02(mstr,ior) = hcs44 
      hsi(mstr,ior) = hcs45 

!      hsised(mstr,ior) = 0.0                                           
!      hSKmor(mstr,ior) = 0.0                                           
      hchla(mstr,ior) = hcs46 
      haki(mstr,ior) = hcs47 
      hagr(mstr,ior) = hcs48 
      habl(mstr,ior) = hcs49 
      hchlak(mstr,ior) = hcs50 
      hchlag(mstr,ior) = hcs51 
      hchlab(mstr,ior) = hcs52 
      hakbcm(mstr,ior) =hcs6
      hagbcm(mstr,ior) = hcs7
      habbcm(mstr,ior) = hcs8
      hvkigr(mstr,ior) = hcs53 
      hantbl(mstr,ior) = hcs54 
      habrz1(mstr,ior) = 0.0 
      hssalg(mstr,ior) = hcs55 
      hss(mstr,ior) = hcs56 
      hzooi(mstr,ior) = hcs57 
      hgelp(mstr,ior) = hcs58 
      hmw(mstr,ior) = hcs59 
      hpw(mstr,ior) = hcs60 
      hca(mstr,ior) = hcs61 
      hlf(mstr,ior) = hcs62 
      hph(mstr,ior) = hcs63 
      hdlarn(mstr,ior) = 0.0 
      hstind(mstr,ior) = 0.0 
      hcoli(mstr,ior) = hcs64 
      hDOSCF(mstr,ior) = hcs100 
      hvbsb(mstr,ior) = hcs65 
      hvcsb(mstr,ior) = hcs66 
      hQ_NK(mstr,ior) = min(Qmx_NK,hcs77) 
      hQ_PK(mstr,ior) = min(Qmx_PK,hcs78) 
      hQ_SK(mstr,ior) = min(Qmx_SK,hcs79) 
      hQ_NG(mstr,ior) = min(Qmx_NG,hcs80) 
      hQ_PG(mstr,ior) = min(Qmx_PG,hcs81) 
      hQ_NB(mstr,ior) = min(Qmx_NB,hcs82) 
      hQ_PB(mstr,ior) = min(Qmx_PB,hcs83) 
      hSKmor(mstr,ior) = hcs85 
      hDOSCF(mstr,ior) = hcs86 

      hFluN3(mstr,ior) = hcs95
      TGZoo(mstr,ior) = hcs99
      akmor_1(mstr,ior) = hcs110
      agmor_1(mstr,ior) = hcs111
      abmor_1(mstr,ior) = hcs112

      hgsZn(mstr,ior) = hcs101
      hglZn(mstr,ior) = hcs102
      hgsCad(mstr,ior) = hcs103
      hglCad(mstr,ior) = hcs104
      hgsCu(mstr,ior) = hcs105
      hglCu(mstr,ior) = hcs106
      hgsNi(mstr,ior) = hcs107
      hglNi(mstr,ior) = hcs108
                                                                      
!...nur Tracer                                                          
      if(iwsim==4)cycle 

        do nkz = 1,hnkzs(mstr,ior)              ! Gitterbelegung 2D 
          if(nkz>nkzs_hc)then
            hcs67(nkz) = hcs67(nkz-1)
            hcs68(nkz) = hcs68(nkz-1)
            hcs69(nkz) = hcs69(nkz-1)
            hcs70(nkz) = hcs70(nkz-1)
            hcs71(nkz) = hcs71(nkz-1)
            hcs72(nkz) = hcs72(nkz-1)
            hcs73(nkz) = hcs73(nkz-1)
            hcs74(nkz) = hcs74(nkz-1)
            hcs75(nkz) = hcs75(nkz-1)
            hcs76(nkz) = hcs76(nkz-1)
            hcs84(nkz) = hcs84(nkz-1)
            hcs87(nkz) = hcs87(nkz-1)
            hcs88(nkz) = hcs88(nkz-1)
            hcs89(nkz) = hcs89(nkz-1)
            hcs90(nkz) = hcs90(nkz-1)
            hcs91(nkz) = hcs91(nkz-1)
            hcs92(nkz) = hcs92(nkz-1)
            hcs93(nkz) = hcs93(nkz-1)
            hcs94(nkz) = hcs94(nkz-1)
            hcs96(nkz) = hcs96(nkz-1)
            hcs97(nkz) = hcs97(nkz-1)
            hcs98(nkz) = hcs98(nkz-1)
          endif

          htempz(mstr,nkz,ior) = hcs67(nkz) 
          ho2z(mstr,nkz,ior) = hcs68(nkz) 
          hnh4z(mstr,nkz,ior) = hcs69(nkz) 
          hno2z(mstr,nkz,ior) = hcs70(nkz) 
          hno3z(mstr,nkz,ior) = hcs71(nkz) 
          hgelPz(mstr,nkz,ior) = hcs72(nkz)
          hSiz(mstr,nkz,ior) = hcs73(nkz)
          gSizt(mstr,nkz,jnkz) = hcs73(nkz)
          hakiz(mstr,nkz,ior) = hcs74(nkz)
          hagrz(mstr,nkz,ior) = hcs75(nkz) 
          hablz(mstr,nkz,ior) = hcs76(nkz)
          hchlaz(mstr,nkz,ior) = hcs84(nkz)
          hchlkz(mstr,nkz,ior) = hcs87(nkz)
          hchlgz(mstr,nkz,ior) = hcs88(nkz)
          hchlbz(mstr,nkz,ior) = hcs89(nkz)
          hgesPz(mstr,nkz,ior) = hcs90(nkz)
          hgesNz(mstr,nkz,ior) = hcs91(nkz)
          hQ_NKz(mstr,nkz,ior) = hcs92(nkz)
          hQ_NBz(mstr,nkz,ior) = hcs93(nkz)
          hQ_NGz(mstr,nkz,ior) = hcs94(nkz)
          hCChlkz(mstr,nkz,ior) = hcs96(nkz)
          hCChlbz(mstr,nkz,ior) = hcs97(nkz)
          hCChlgz(mstr,nkz,ior) = hcs98(nkz)
        enddo 

      if(nbuhn(mstr)==0.or.iwied==1)cycle 
!     Buhnenfelder                                                      
                                                                       
      bsvhek(mstr,ior) = hsvhk(mstr,ior) 
      bsvheg(mstr,ior) = hsvhg(mstr,ior)
      bsvheb(mstr,ior) = hsvhb(mstr,ior) 
      bakbcm(mstr,ior) = hakbcm(mstr,ior) 
      babbcm(mstr,ior) = habbcm(mstr,ior) 
      bagbcm(mstr,ior) = hagbcm(mstr,ior) 
      bnl0(mstr,ior) = hnl0(mstr,ior) 
      bpl0(mstr,ior) = hpl0(mstr,ior) 
      bgesN(mstr,ior) = hgesN(mstr,ior) 
      bgesP(mstr,ior) = hgesP(mstr,ior) 
      bstind(mstr,ior) = hstind(mstr,ior) 
      btempw(mstr,ior) = htempw(mstr,ior)

!...Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser            

      if(iwied.eq.0)bTsed(mstr,ior) = htempw(mstr,ior) 
      bbsb(mstr,ior) = hbsb(mstr,ior) 
      bcsb(mstr,ior) = hcsb(mstr,ior) 
      bnh4(mstr,ior) = hnh4(mstr,ior) 
      bo2(mstr,ior) = ho2(mstr,ior) 
      bno3(mstr,ior) = hno3(mstr,ior) 
      bno2(mstr,ior) = hno2(mstr,ior) 
      bx0(mstr,ior) = hx0(mstr,ior) 
      bx02(mstr,ior) = hx02(mstr,ior) 
      bsi(mstr,ior) = hsi(mstr,ior) 
      bsised(mstr,ior) = hsised(mstr,ior) 
      bSKmor(mstr,ior) = hSKmor(mstr,ior) 
      bchla(mstr,ior) = hchla(mstr,ior) 
      baki(mstr,ior) = haki(mstr,ior) 
      bagr(mstr,ior) = hagr(mstr,ior) 
      babl(mstr,ior) = habl(mstr,ior) 
      bchlak(mstr,ior) = hchlak(mstr,ior) 
      bchlag(mstr,ior) = hchlag(mstr,ior) 
      bchlab(mstr,ior) = hchlab(mstr,ior) 
      bvkigr(mstr,ior) = hvkigr(mstr,ior) 
      bantbl(mstr,ior) = hantbl(mstr,ior) 
      babrz1(mstr,ior) = habrz1(mstr,ior) 
      bssalg(mstr,ior) = hssalg(mstr,ior) 
      bfssgr(mstr,ior) = hfssgr(mstr,ior) 
      bfbsgr(mstr,ior) = hfbsgr(mstr,ior) 
      bfrfgr(mstr,ior) = hfrfgr(mstr,ior) 
      bss(mstr,ior) = hss(mstr,ior) 
      bzooi(mstr,ior) = hzooi(mstr,ior) 
      bgelp(mstr,ior) = hgelp(mstr,ior) 
      bmw(mstr,ior) = hmw(mstr,ior) 
      bpw(mstr,ior) = hpw(mstr,ior) 
      bca(mstr,ior) = hca(mstr,ior) 
      blf(mstr,ior) =hlf(mstr,ior) 
      bdlarn(mstr,ior) = hdlarn(mstr,ior)
      bph(mstr,ior) = hph(mstr,ior) 
      bvbsb(mstr,ior) = hvbsb(mstr,ior) 
      bvcsb(mstr,ior) = hvcsb(mstr,ior) 
      bCD(mstr,1,ior) = hCD(mstr,1,ior) 
      bCD(mstr,2,ior) = hCD(mstr,2,ior) 
      bCP(mstr,1,ior) = hCP(mstr,1,ior) 
      bCP(mstr,2,ior) = hCP(mstr,2,ior) 
      bCM(mstr,ior) = hCM(mstr,ior) 
      bBAC(mstr,ior) = hBAC(mstr,ior)
      bCHNF(mstr,ior) = hCHNF(mstr,ior) 
      bQ_PK(mstr,ior) = hQ_PK(mstr,ior) 
      bQ_NK(mstr,ior) = hQ_NK(mstr,ior) 
      bQ_SK(mstr,ior) = hQ_SK(mstr,ior) 
      bQ_PG(mstr,ior) = hQ_PG(mstr,ior) 
      bQ_NG(mstr,ior) = hQ_NG(mstr,ior) 
      bQ_PB(mstr,ior) = hQ_PB(mstr,ior) 
      bQ_NB(mstr,ior) = hQ_NB(mstr,ior) 
      bFluN3(mstr,ior) = hfluN3(mstr,ior)
      bcoli(mstr,ior) = hcoli(mstr,ior) 
      bDOSCF(mstr,ior) = hDOSCF(mstr,ior)

      bgsZn(mstr,ior) = hgsZn(mstr,ior)
      bglZn(mstr,ior) = hglZn(mstr,ior)
      bgsCad(mstr,ior) = hgsCad(mstr,ior)
      bglCad(mstr,ior) = hglCad(mstr,ior)
      bgsCu(mstr,ior) = hgsCu(mstr,ior)
      bglCu(mstr,ior) = hglCu(mstr,ior)
      bgsNi(mstr,ior) = hgsNi(mstr,ior)
      bglNi(mstr,ior) = hglNi(mstr,ior)
                                                                       
   enddo ! Ende Schleife Neubelegung des erten oder letzten Ortspunkts eines Strangs      
      endif ! Abfluss >0.0
   end select Rand_Wahl
       
      enddo ! Ende Schleife ueber alle Straenge 

      if(jlauf.eq.1)goto 7777 ! Ablegen der berechneten Werte aus dem Zeitschritt t-1 und den Randbedingungen zum Zeitpunkt t   

 9998 continue  ! Sprungziel nach Ablegen der Werte für jeden Ortspunkt 

  
!.....Vorlauf ilang = 0                                                 
!      if(iwied.eq.1.and.ilang.eq.0)goto 5555 
!                                                                       
!                                                                       
!.........Einlesen der hydraulischen Daten aus sysgenou                 
!                                                                       
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'sysgenou'
      open(unit=11, file=pfadstring)
      rewind (11) 
!                                                                       
      do 773 azStr = 1,azStrs 
      mstr = mstra(azStr) 
!                                                                       
      read(11,1000)hanze(mstr) 
 1000 format(i4) 
      do 30 ior=1,hanze(mstr)

     read(11,1010)hfkm(mstr,ior),hflag(mstr,ior)                                         &
     &,hjiein(mstr,ior),helen(mstr,ior),hvmitt(mstr,ior)                                 &
     &,htiefe(mstr,ior),hrau(mstr,ior),hrhyd(mstr,ior)                                   &
     &,hSedOM(mstr,ior),hw2(mstr,ior),hBedGS(mstr,ior),hsedvvert(mstr,ior)               &
     &,hdKorn(mstr,ior),hflae(mstr,ior),hWS(mstr,ior),hischf(mstr,ior)                   &                                 
     &,hlboem(mstr,ior),hbsohl(mstr,ior),hvabfl(mstr,ior)                                &
     &,bh(mstr,ior),bf(mstr,ior),bso(mstr,ior),blb(mstr,ior)                             &
     &,bleb(mstr,ior),hdl(mstr,ior),htau2(mstr,ior)                                      &
     &,vbm(mstr,ior),bSedOM(mstr,ior),bw2(mstr,ior),bdKorn(mstr,ior)                     &
     &,dlalph(mstr,ior) 

     if(hrhyd(mstr,ior)<=0.0)hrhyd(mstr,ior) = htiefe(mstr,ior)
                                                                     
 1010 format(f8.3,2x,i1,2x,i2                                                           &
     &,2x,f9.3,2x,f7.4,2x,f5.2,2x,f5.2,2x,f5.2                                          &
     &,2x,f8.5,2x,e11.5,2x,f5.2,2x,f9.4,2x,E11.5,2x,f8.2,2x,f9.4,2x,i1,2x,f9.4          &
     &,2x,f9.4,2x,f14.6,2x,f5.2,2x,f7.2,2x,f7.2,2x,f7.2,2x                              &
     &,f7.2,2x,f7.2,2x,f7.2,2x,f7.4,2x,f8.5,2x,E11.5,2x,E11.5,2x,f6.2)  

       imac(mstr) = 0
       if(hflag(mstr,ior)==4.and.hvmitt(mstr,ior)<0.0)imac(mstr) = 1 !falls Fliessumkehr an einer Einleiterstelle
                                                                     !wird zur Lösung des Dispersionsterms das McCormack-
                                                                     !Verfahren benutzt   


        if(hvmitt(mstr,ior)<0.0)imac(mstr) = 1

    30 continue 

!....Belegung des letzten Knotens mit hydraulischen Daten..... 
      hvabfl(mstr,hanze(mstr)+1) = hvabfl(mstr,hanze(mstr))         
      htiefe(mstr,hanze(mstr)+1) = htiefe(mstr,hanze(mstr)) 
      hvmitt(mstr,hanze(mstr)+1) = hvmitt(mstr,hanze(mstr)) 
      hrau(mstr,hanze(mstr)+1) = hrau(mstr,hanze(mstr)) 
      hrhyd(mstr,hanze(mstr)+1) = hrhyd(mstr,hanze(mstr)) 
      hflae(mstr,hanze(mstr)+1) = hflae(mstr,hanze(mstr)) 
      hWS(mstr,hanze(mstr)+1) = hWS(mstr,hanze(mstr)) 
      hlboem(mstr,hanze(mstr)+1) = hlboem(mstr,hanze(mstr)) 
      hbsohl(mstr,hanze(mstr)+1) = hbsohl(mstr,hanze(mstr)) 
      bh(mstr,hanze(mstr)+1) = bh(mstr,hanze(mstr)) 
      bf(mstr,hanze(mstr)+1) = bf(mstr,hanze(mstr)) 
      bso(mstr,hanze(mstr)+1) = bso(mstr,hanze(mstr)) 
      blb(mstr,hanze(mstr)+1) = blb(mstr,hanze(mstr)) 
      bleb(mstr,hanze(mstr)+1) = bleb(mstr,hanze(mstr)) 
      hdl(mstr,hanze(mstr)+1) = hdl(mstr,hanze(mstr)) 
      htau2(mstr,hanze(mstr)+1) = htau2(mstr,hanze(mstr)) 
      vbm(mstr,hanze(mstr)+1) = vbm(mstr,hanze(mstr)) 
      dlalph(mstr,hanze(mstr)+1) = dlalph(mstr,hanze(mstr)) 
      hSedOM(mstr,hanze(mstr)+1) = hSedOM(mstr,hanze(mstr))
      hw2(mstr,hanze(mstr)+1) = hw2(mstr,hanze(mstr))
      hsedvvert(mstr,hanze(mstr)+1) = hsedvvert(mstr,hanze(mstr)) 
      hdKorn(mstr,hanze(mstr)+1) = hdKorn(mstr,hanze(mstr)) 
      hBedGS(mstr,hanze(mstr)+1) = hBedGS(mstr,hanze(mstr))
      bSedOM(mstr,hanze(mstr)+1) = bSedOM(mstr,hanze(mstr)) 
      bw2(mstr,hanze(mstr)+1) = bw2(mstr,hanze(mstr)) 
      bdKorn(mstr,hanze(mstr)+1) = bdKorn(mstr,hanze(mstr))
      SPEWKSS(mstr,hanze(mstr)+1) = SPEWKSS(mstr,hanze(mstr))
      WUEBKS(mstr,hanze(mstr)+1) = WUEBKS(mstr,hanze(mstr))
      PSREFSS(mstr,hanze(mstr)+1) = PSREFSS(mstr,hanze(mstr))
      extkS(mstr,hanze(mstr)+1) = extkS(mstr,hanze(mstr))
      hflag(mstr,hanze(mstr)+1) = 2
      hjiein(mstr,hanze(mstr)+1) = 0
                                                                       
  773 continue 
!                                                                       
      close (11) 
!                                                                       
      if(ilang.eq.0)goto 5555 

                                                                      
!#### Neubelegung des vertikalen Rechengitters an jedem Gitterpunkt ####       
                                                                       
      call sys_z_Gitter(azStrs,mstra,hanze,znkzs,hnkzs,dH2D,iFlRi,htempz,ho2z,hnh4z,hno2z,hno3z        &    
                        ,hgelPz,hSiz,hakiz,hagrz,hablz,hchlaz,hchlkz,hchlgz,hchlbz,hgesPz,hgesNz       &
                        ,hQ_NKz, hQ_NBz, hQ_NGz, hCChlkz,hCChlbz,hCChlgz,itags,monats)                                              
!                                                                       
!     **********************************************************        
!     Zeitschleife                                                      
!                                                                       
 5555 continue

! ##### Berücksichtigung von Eineitern am 1. Ortspunks eines Stranges mit Vorsträngen (2D-Fall) ##### 

      do azStr = 1,azStrs  ! Strangschleife ANFANG
        if(iwied==0.or.i2Daus==0)exit
        mstr = mstra(azStr)
        if(iRB_K1(mstr)==0.or.nnStrs(mstr)==0)cycle

        sum_QEinl = 0.0
        hcq1 = 0.0
        hcq2 = 0.0
        hcq3 = 0.0
        hcq4 = 0.0
        hcq5 = 0.0
        hcq6 = 0.0
        hcq7 = 0.0
        hcq8 = 0.0
        hcq9 = 0.0
        hcq10 = 0.0
        hcq11 = 0.0

        do iRB = 1,iRB_K1(mstr)
          sum_QEinl = sum_QEinl + abfls(mstr,imRB_K1(iRB))
        enddo

        hcQ1 = max(1.e-10,(QStrang_1(mstr) - sum_QEinl))

        do nkz = 1, hnkzs(mstr,1)  ! 2D
          hc32(nkz) = hNh4z(mstr,nkz,1) * hcq1
          i_K13 = 0
          hc42(nkz) = hNO2z(mstr,nkz,1) * hcq1
          i_K14 = 0
          hc52(nkz) = hNO3z(mstr,nkz,1) * hcq1
          i_K15 = 0
          hc62(nkz) = hgesNz(mstr,nkz,1) * hcq1
          i_K16 = 0
          hc92(nkz) = hgelPz(mstr,nkz,1) * hcq1
          i_K19 = 0
          hc102(nkz) = hgesPz(mstr,nkz,1) * hcq1
          i_K110 = 0
          hc112(nkz) = hSiz(mstr,nkz,1) * hcq1
          i_K111 = 0 
          hc122(nkz) = hChlaz(mstr,nkz,1) * hcq1
          i_K112 = 0
          hc212(nkz) = htempz(mstr,nkz,1) * hcq1
          i_K121 = 0
          hc222(nkz) = hO2z(mstr,nkz,1) * hcq1
          i_K122 = 0 
          hc262(nkz) = htempz(mstr,nkz,1)
          i_K126 = 0
        enddo

        hcq1 = hcq1
        hcq2 = hcq1
        hcq3 = hcq1
        hcq4 = hcq1
        hcq5 = hcq1
        hcq6 = hcq1
        hcq7 = hcq1
        hcq8 = hcq1
        hcq9 = hcq1
        hcq10 = hcq1
        hcq11 = hcq1

        do iRB = 1,iRB_K1(mstr)
          abfls(mstr,imRB_K1(iRB)) = max(1.e-10,abfls(mstr,imRB_K1(iRB)))

          if(vNH4s(mstr,imRB_K1(iRB))>=0.0)then
            do nkz = 1,hnkzs(mstr,1)
              hc32(nkz) = hc32(nkz) + vNH4s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
              hcq1 = hcq1 + abfls(mstr,imRB_K1(iRB))
            i_K13 = 1
          endif

          if(vNO2s(mstr,imRB_K1(iRB))>=0.0)then
            do nkz = 1,hnkzs(mstr,1)
              hc42(nkz) = hc42(nkz) + vNO2s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
            hcq2 = hcq2 + abfls(mstr,imRB_K1(iRB))
            i_K14 = 1
          endif

          if(vNO3s(mstr,imRB_K1(iRB))>=0.0)then
            do nkz = 1,hnkzs(mstr,1) ! 2D
              hc52(nkz) = hc52(nkz) + vNO3s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
            hcq3 = hcq3 + abfls(mstr,imRB_K1(iRB))
            i_K15 = 1
          endif

          if(gesNs(mstr,imRB_K1(iRB))>=0.0)then
            do nkz = 1,hnkzs(mstr,1)  ! 2D
              hc62(nkz) = hc62(nkz) + gesNs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
            hcq4 = hcq4 + abfls(mstr,imRB_K1(iRB))
            i_K16 = 1
          endif

          if(gelps(mstr,imRB_K1(iRB))>=0.0)then
            do nkz = 1,hnkzs(mstr,1)  ! 2D
              hc92(nkz) = hc92(nkz) + gelPs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
            hcq5 = hcq5 + abfls(mstr,imRB_K1(iRB))
            i_K19 = 1
          endif

          if(gesPs(mstr,imRB_K1(iRB))>=0.0)then
            do nkz = 1,hnkzs(mstr,1)  ! 2D
              hc102(nkz) = hc102(nkz) + gesPs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
            hcq6 = hcq6 + abfls(mstr,imRB_K1(iRB))
            i_K110 = 1
          endif

          if(Sis(mstr,imRB_K1(iRB))>=0.0)then
            do nkz = 1,hnkzs(mstr,1)  ! 2D
              hc112(nkz) = hc112(nkz) + Sis(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
            hcq7 = hcq7 + abfls(mstr,imRB_K1(iRB))
            i_K111 = 1
          endif

          if(chlas(mstr,imRB_K1(iRB))>=0.0)then
            do nkz = 1,hnkzs(mstr,1)  ! 2D
              hc122(nkz) = hc122(nkz) + chlas(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
            hcq8 = hcq8 + abfls(mstr,imRB_K1(iRB))
            i_K112 = 1
          endif

          if(iwsim/=4.and.tempws(mstr,imRB_K1(iRB))>-9.99)then
            do nkz = 1,hnkzs(mstr,1) 
              hc212(nkz) = hc212(nkz) + tempws(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
            hcq9 = hcq9 + abfls(mstr,imRB_K1(iRB))
            i_K121 = 1
          endif

          if(vo2s(mstr,imRB_K1(iRB))>=0.0)then
            do nkz = 1,hnkzs(mstr,1)  
              hc222(nkz) = hc222(nkz) + vo2s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            enddo
            hcq10 = hcq10 + abfls(mstr,imRB_K1(iRB))
            i_K122 = 1
          endif

          if(waers(mstr,imRB_K1(iRB))>-99.9.and.abfls(mstr,imRB_K1(iRB))<=2.e-10)then

              hcq11 = hcq11 + abfls(mstr,imRB_K1(iRB))
            do nkz = 1, hnkzs(mstr,1)
              hc262(nkz) = hc262(nkz) + waers(mstr,imRB_K1(iRB))/4.2/hcq11
            enddo
            i_K126 = 1
               else if(waers(mstr,imRB_K1(iRB))>-99.9.and.abfls(mstr,imRB_K1(iRB))>2.e-10)then
                 do nkz = 1, hnkzs(mstr,1)
                   hc262(nkz) = hc262(nkz) + waers(mstr,imRB_K1(iRB))/4.2/(hcq11 + abfls(mstr,imRB_K1(iRB)))
                 enddo
                 i_K126 = 1
                 hcq11 = hcq11 + abfls(mstr,imRB_K1(iRB))
           endif
         enddo

          if(i_K13>0)then
            do nkz = 1, hnkzs(mstr,1)
              hNH4z(mstr,nkz,1) = hc32(nkz)/hcq1
            enddo
          endif                  

          if(i_K14>0)then
            do nkz = 1, hnkzs(mstr,1)
              hNO2z(mstr,nkz,1) = hc42(nkz)/hcq2
            enddo
          endif                  

          if(i_K15>0)then
            do nkz = 1, hnkzs(mstr,1)
              hNO3z(mstr,nkz,1) = hc52(nkz)/hcq3
            enddo
          endif                  

          if(i_K16>0)then
            do nkz = 1, hnkzs(mstr,1)
              hgesNz(mstr,nkz,1) = hc62(nkz)/hcq4
            enddo
          endif                  

          if(i_K19>0)then
            do nkz = 1, hnkzs(mstr,1)
              hgelPz(mstr,nkz,1) = hc92(nkz)/hcq5
            enddo
          endif                  

          if(i_K110>0)then
            do nkz = 1, hnkzs(mstr,1)
              hgesPz(mstr,nkz,1) = hc102(nkz)/hcq6
            enddo
          endif                  

          if(i_K111>0)then
            do nkz = 1, hnkzs(mstr,1)
              hSiz(mstr,nkz,1) = hc112(nkz)/hcq7
            enddo
          endif                  

          if(i_K112>0)then
            do nkz = 1, hnkzs(mstr,1)
              hchlaz(mstr,nkz,1) = hc122(nkz)/hcq8
              hakiz(mstr,nkz,1) = (hchlaz(mstr,nkz,1)*hvkigr(mstr,1)/1000.) * (hakbcm(mstr,1)/Caki) 
              hagrz(mstr,nkz,1) = (hchlaz(mstr,nkz,1)*(1.-hvkigr(mstr,1)-hantbl(mstr,1))/1000.) * (hagbcm(mstr,1)/Cagr) 
              hablz(mstr,nkz,1) = (hChlaz(mstr,nkz,1)*hantbl(mstr,1)/1000.) * (habbcm(mstr,1)/Cabl) 
            enddo
          endif                  

          if(i_K121>0)then
            do nkz = 1, hnkzs(mstr,1)
              htempz(mstr,nkz,1) = hc212(nkz)/hcq9
            enddo
          endif                  
            
          if(i_K122>0)then
            do nkz = 1, hnkzs(mstr,1)
              hO2z(mstr,nkz,1) = hc222(nkz)/hcq10
            enddo
          endif                  

          if(i_K126>0)then
            do nkz = 1, hnkzs(mstr,1)
              htempz(mstr,nkz,1) = hc262(nkz)
            enddo
          endif 
        enddo ! Strangschleife ENDE            

!#####################################################################
!                                                                       
!...Umrechnung der Uhrzeit in das Format <h.mm>                         
!                                                                       
      Stunde = int(Uhrz) 
      hcmin = (Uhrz-Stunde)*60. 
      minute = nint(hcmin) 
      if(minute.eq.60)then 
        minute = 0 
        Stunde = Stunde+1 
      endif 
      rmin = minute/100. 
      Uhrzhm = Stunde+rmin 

!##### Bildschirmausgabe #####                                                                       
      write(*,6161) 
 6161 format(2x,'GUETESIMULATION') 
      write(*,6162)itime 
 6162 format(2x,'Anzahl der Zeitschritte: ',I4) 
      write(*,6163)ij,itags,monats,jahrs,Uhrzhm 
      !!wy write(222,6163)ij,itags,monats,jahrs,Uhrzhm 
 6163 format(2x,'Zeitschritt: ',I4,2x,i2,'.',i2,'.',I4,2x,F5.2) 
                                                                       
!....Strangschleife fr Berechnung

      if(iwsim==4)sumTracer = 0.0  ! Aufsummierung der "Tracermasse" 
                                     
 9898 do 8888 azStr = 1,azStrs
      mstr = mstra(azStr) 
      anze = hanze(mstr) 
      iein = 0
                                                                    
      do kein=1,ieinsh(mstr) ! Einleiter
        iein = iein+1 
        einlk(iein) = einlkh(mstr,kein) 
        qeinl(iein) = qeinlh(mstr,kein)
        ebsb(iein) = ebsbh(mstr,kein) 
        ecsb(iein) = ecsbh(mstr,kein) 
        eBVHNF(iein) = eBVHNh(mstr,kein) 
        eCHNF(iein) = eCHNFh(mstr,kein) 
        enh4(iein) = enh4h(mstr,kein) 
        ex0(iein) = ex0h(mstr,kein) 
        ex02(iein) = ex02h(mstr,kein) 
        eo2(iein) = eo2h(mstr,kein) 
        etemp(iein) = etemph(mstr,kein) 
        echla(iein) = echlah(mstr,kein) 
        ezind(iein) = ezindh(mstr,kein) 
        ep(iein) = egph(mstr,kein) 
        esi(iein) = esih(mstr,kein) 
        eno2(iein) = eno2h(mstr,kein) 
        eno3(iein) = eno3h(mstr,kein) 
        egesN(iein) = egesNh(mstr,kein) 
        egesP(iein) = egesPh(mstr,kein) 
        ess(iein) = essh(mstr,kein) 
        ewaerm(iein) = ewaerh(mstr,kein) 
        typ(iein) = typh(mstr,kein) 
        eph(iein) = ephh(mstr,kein) 
        emw(iein) = emwh(mstr,kein) 
        elf(iein) = elfh(mstr,kein) 
        eca(iein) = ecah(mstr,kein) 
        ecoli(iein) = ecolih(mstr,kein) 
        evkigr(iein) = evkgh(mstr,kein) 
        eantbl(iein) = eantbh(mstr,kein) 
        enl0(iein) = enl0h(mstr,kein) 
        epl0(iein) = epl0h(mstr,kein) 
      enddo 
                                                                       
      ieinL = 0 
                                                                       
      do kein=1,ieinLs(mstr)  ! Linienquellen
        ieinL = ieinL+1 
        qeinlL(ieinL) = qLh(mstr,kein) 
        bsbL(ieinL) = bsbLh(mstr,kein) 
        csbL(ieinL) = csbLh(mstr,kein) 
        enh4L(ieinL) = enh4Lh(mstr,kein) 
        x0L(ieinL) = x0Lh(mstr,kein) 
        x02L(ieinL) = x02Lh(mstr,kein) 
        o2L(ieinL) = o2Lh(mstr,kein) 
        etempL(ieinL) = tempLh(mstr,kein) 
        gpL(ieinL) = gpLh(mstr,kein) 
        siL(ieinL) = siLh(mstr,kein) 
        eno2L(ieinL) = eno2Lh(mstr,kein) 
        eno3L(ieinL) = eno3Lh(mstr,kein) 
        gesNL(ieinL) = gesNLh(mstr,kein) 
        gesPL(ieinL) = gesPLh(mstr,kein) 
        ssL(ieinL) = ssLh(mstr,kein) 
        phL(ieinL) = phLh(mstr,kein) 
        elfL(ieinL) = elfLh(mstr,kein) 
        caL(ieinL) = caLh(mstr,kein) 
        coliL(ieinL) = coliLh(mstr,kein) 
        enl0L(ieinL) = enl0Lh(mstr,kein) 
        pl0L(ieinL) = pl0Lh(mstr,kein)
        chlaL(ieinL) = chlaLh(mstr,kein) 
                                                                       
        iorLa(ieinL) = iorLah(mstr,kein) 
        iorLe(ieinL) = iorLeh(mstr,kein) 
      enddo 
!                                                                       
!.....Kenngroessen für Pflanzenwachstum,und Dreissenawachstum           
!.....nur strangweise nicht Abschnittsweise                             
      itstart = itsts(mstr) 
      mstart = msts(mstr) 
      itmax = itmaxs(mstr) 
      mmax = mmaxs(mstr) 
      itend = itends(mstr) 
      mend = mends(mstr) 
      lait1 = laits(mstr) 
      laim1 = laims(mstr) 
      laid1 = laids(mstr) 
                                                                       
      do ior=1,anze+1 
      svhemk(ior) = hsvhk(mstr,ior) 
      svhemg(ior) = hsvhg(mstr,ior) 
      svhemb(ior) = hsvhb(mstr,ior) 
      DOSCF(ior) = hDOSCF(mstr,ior) 
      akbcm(ior) = hakbcm(mstr,ior) 
      agbcm(ior) = hagbcm(mstr,ior) 
      abbcm(ior) = habbcm(mstr,ior) 
      fssgr(ior) = hfssgr(mstr,ior) 
      fbsgr(ior) = hfbsgr(mstr,ior) 
      frfgr(ior) = hfrfgr(mstr,ior) 
      nl0(ior) = hnl0(mstr,ior) 
      pl0(ior) = hpl0(mstr,ior) 
      gesN(ior) = hgesN(mstr,ior) 
      gesP(ior) = hgesP(mstr,ior) 
      Q_NK(ior) = hQ_NK(mstr,ior) 
      Q_PK(ior) = hQ_PK(mstr,ior) 
      Q_SK(ior) = hQ_SK(mstr,ior) 
      Q_NG(ior) = hQ_NG(mstr,ior) 
      Q_PG(ior) = hQ_PG(mstr,ior) 
      Q_NB(ior) = hQ_NB(mstr,ior) 
      Q_PB(ior) = hQ_PB(mstr,ior) 
      tempw(ior) = htempw(mstr,ior) 
      Tsed(ior) = hTsed(mstr,ior) 
      obsb(ior) = hbsb(mstr,ior) 
      ocsb(ior) = hcsb(mstr,ior) 
!                                                                       
      CHNF(ior) = hCHNF(mstr,ior) 
      BVHNF(ior) = hBVHNF(mstr,ior) 
      CD(1,ior) = hCD(mstr,1,ior) 
      CD(2,ior) = hCD(mstr,2,ior) 
      CP(1,ior) = hCP(mstr,1,ior) 
      CP(2,ior) = hCP(mstr,2,ior) 
      CM(ior) = hCM(mstr,ior) 
      BAC(ior) = hBAC(mstr,ior) 
!                                                                       
      vnh4(ior) = hnh4(mstr,ior) 
      vo2(ior) = ho2(mstr,ior)
      vno3(ior) = hno3(mstr,ior) 
      vno2(ior) = hno2(mstr,ior) 
      vx0(ior) = hx0(mstr,ior) 
      vx02(ior) = hx02(mstr,ior) 
      si(ior) = hsi(mstr,ior) 
      sised(ior) = hsised(mstr,ior) 
      SKmor(ior) = hSKmor(mstr,ior) 
      chla(ior) = hchla(mstr,ior)
      aki(ior) = haki(mstr,ior) 
      agr(ior) = hagr(mstr,ior) 
      abl(ior) = habl(mstr,ior)
      chlaki(ior) = hchlak(mstr,ior) 
      chlagr(ior) = hchlag(mstr,ior) 
      chlabl(ior) = hchlab(mstr,ior) 
      vkigr(ior) = hvkigr(mstr,ior) 
      antbl(ior) = hantbl(mstr,ior) 
      abrzo1(ior) = habrz1(mstr,ior) 
      ssalg(ior) = hssalg(mstr,ior) 
      ss(ior) = hss(mstr,ior) 
      zooind(ior) = hzooi(mstr,ior) 
      gelp(ior) = hgelp(mstr,ior) 
      mw(ior) = hmw(mstr,ior) 
      pw(ior) = hpw(mstr,ior) 
      ca(ior) = hca(mstr,ior) 
      lf(ior) = hlf(mstr,ior) 
      vph(ior) = hph(mstr,ior) 
      dlarvn(ior) = hdlarn(mstr,ior) 
      vbsb(ior) = hvbsb(mstr,ior) 
      vcsb(ior) = hvcsb(mstr,ior) 
      stind(ior) = hstind(mstr,ior) 
      coli(ior) = hcoli(mstr,ior)
      DOSCF(ior) = hDOSCF(mstr,ior) 
      jiein(ior) = hjiein(mstr,ior) 
      Dz2D(ior) = hDz2D(mstr,ior)

      sedalg(ior) = hsedag(mstr,ior) 
      sedalk(ior) = hsedak(mstr,ior) 
      sedalb(ior) = hsedab(mstr,ior) 
                                                                       
!...nur Tracer                                                          
      if(iwsim==4)cycle 
!  2D-Modellierung                                                      
!                                                                       
      nkzs(ior) = hnkzs(mstr,ior) 
      dH2De(ior) = hdH2De(mstr,ior) 

      do nkz = 1,nkzs(ior) 
        tempwz(nkz,ior) = htempz(mstr,nkz,ior) 
        vnh4z(nkz,ior) = hnh4z(mstr,nkz,ior) 
        vno2z(nkz,ior) = hno2z(mstr,nkz,ior) 
        vno3z(nkz,ior) = hno3z(mstr,nkz,ior) 
        vo2z(nkz,ior) = ho2z(mstr,nkz,ior) 
        gelPz(nkz,ior) = hgelPz(mstr,nkz,ior) 
        Siz(nkz,ior) = hSiz(mstr,nkz,ior) 
        akiz(nkz,ior) = hakiz(mstr,nkz,ior) 
        agrz(nkz,ior) = hagrz(mstr,nkz,ior) 
        ablz(nkz,ior) = hablz(mstr,nkz,ior) 
        chlaz(nkz,ior) = hchlaz(mstr,nkz,ior) 
      enddo
                                                                       
      pflmin(ior) = hpfmnl(mstr,ior) 
      pflmax(ior) = hpfmxl(mstr,ior) 
      ischif(ior) = hischf(mstr,ior) 

      do ndr = 1,nndr 
        zdrei(ior,ndr) = hzdrel(mstr,ior,ndr) 
        zdreis(ior,ndr) = hzdrsl(mstr,ior,ndr) 
        gewdr(ior,ndr) = hgwdrl(mstr,ior,ndr) 
      enddo

      dlmax(ior) = hdlmx(mstr,ior) 
      dlmaxs(ior) = hdlmxs(mstr,ior) 
      gwdmax(ior) = hgwdmx(mstr,ior) 
      sgwmue(ior) = hsgwmu(mstr,ior) 
      abegm2(ior) = habgml(mstr,ior) 
      abekm2(ior) = habkml(mstr,ior) 

      do ico = 1,5 
        coro(ior,ico) = hcoro2(mstr,ior,ico) 
        coros(ior,ico) = hcos2(mstr,ior,ico) 
      enddo
                                                                       
!.....Ufervegetation                                                    
      Do iV = 1,14 
        VTYP(ior,iV) = VTYPH(mstr,ior,iV) 
      enddo

      VALTBL(ior) = VALTLH(mstr,ior) 
      EDUFBL(ior) = EDUFLH(mstr,ior) 
      VALTBR(ior) = VALTRH(mstr,ior) 
      EDUFBR(ior) = EDUFRH(mstr,ior) 
                                                                       
      if(ieros>0)then 
        sedh(ior) = sedhg(mstr,ior)
  
        ischic(ior) = ischig(mstr,ior)
        do itau = 1,ischic(ior) 
          tausc(ior,itau) = tauscg(mstr,ior,itau) 
        enddo
      endif                                                                     
   enddo
                                                                       
      dtmin = Strdt(mstr)
      dtmin_Mac = Strdt(mstr) 

      do 614 ior = 1,anze+1 
      fkm(ior) = hfkm(mstr,ior) 
      flag(ior) = hflag(mstr,ior)
      jiein(ior) = hjiein(mstr,ior) 
      elen(ior) = helen(mstr,ior) 
      if(ior<=anze)vmitt(ior) = (abs(hvmitt(mstr,ior))+abs(hvmitt(mstr,ior+1)))/2. 
      if(ior==(anze+1))vmitt(ior) = abs(vmitt(anze))
      tiefe(ior) = htiefe(mstr,ior) 
      rau(ior) = hrau(mstr,ior) 
      rhyd(ior) = hrhyd(mstr,ior) 
      flae(ior) = hflae(mstr,ior) 
      lboem(ior) = hlboem(mstr,ior) 
      if(lboem(ior).le.0.0)lboem(ior) = 0.0000001 
      bsohlm(ior) = hbsohl(mstr,ior) 
      vabfl(ior) = hvabfl(mstr,ior) 
!                                                                       
      if(nbuhn(mstr).eq.0)goto 300 
      tau2(ior) = 1./(htau2(mstr,ior)*3600.) 
!                                                                       
!                                                                       
!     Berechnung des Abschnittsvolumens vol(ior)                        
!                                                                       
  300 elenl = elen(ior) 
      if(elenl.le.0.01)elenl = 0.0 
      vol(ior) = flae(ior)*elenl 
!                                                                       
!....falls idl = 1 wird der long. Dispersionskoeff eingelesen           
!....andernfalls wird er berechnet                                      
      if(idl.eq.1.and.hdl(mstr,ior).gt.0.0)then 
      dl(ior) = hdl(mstr,ior) 
      goto 613 
      endif 
!                                                                       
      ust = (((1./rau(ior))*9.81**0.5)/(tiefe(ior)**0.16667))*abs(vmitt(ior)) 
      if(ust==0.0)ust = 1.e-10

      Breite(ior) = flae(ior)/Tiefe(Ior) 
                                                                       
     if(ilongDis==1)then
!....Berechnung des longitudinalen Dispersionskoeff. nach DENG          
!                                                                       
       alpha = 1.67
       if(dlalph(mstr,ior).gt.0.0)alpha = dlalph(mstr,ior) 
 
       lat_K = 0.145+(1./3520.)*(abs(vmitt(ior))/Ust)*(Breite(ior)/Tiefe(ior))**1.38
       DL(ior) = (0.15/(8.*lat_K))*(abs(vmitt(ior))/Ust)**2*(Breite(ior)/Tiefe(ior))**alpha*Tiefe(ior)*ust

       DL(Ior) = DL(ior)*FlongDis
     endif

     if(ilongDis==2)then 
!    Li et al. (1998)

     alpha = 1.3
     if(dlalph(mstr,ior).gt.0.0)alpha = dlalph(mstr,ior) 

       DL(ior) = 0.2*(Breite(ior)/Tiefe(ior))**alpha*(abs(vmitt(ior))/Ust)**1.2*Tiefe(ior)*Ust    
       DL(ior) = DL(ior)*FlongDis
     endif

     if(ilongDis==3)then   ! Iwasa and Aya (1991)
 
     alpha = 1.5
    if(dlalph(mstr,ior).gt.0.0)alpha = dlalph(mstr,ior) 

       DL(ior) = 2.*(Breite(ior)/Tiefe(ior))**alpha*Tiefe(ior)*ust
       DL(ior) = DL(Ior)*FlongDis
     endif

     if(ilongDis==4)then  ! Elder (1959)
       DL(ior) = 5.93*Tiefe(ior)*ust
       DL(Ior) = DL(ior)*FlongDis
     endif
    !!wy write(222,*)'DL(',ior,')=',DL(Ior),' ust=',ust,mstr

  613 Cr_zahl = 5.
      Cr_zahl_Mac = 0.48
      if(iverfahren>1)imac(mstr) = 1      
      if(imac(mstr)==1)Cr_zahl = Cr_zahl_Mac

      if(ior>1)then
        hc_alpha = elen(ior)/elen(ior-1)
        hc_dl = (DL(ior-1)+DL(ior))/2. 
          else
            hc_alpha = 1.
            hc_DL = DL(ior)
      endif

      dtneu = 0.5*hc_alpha*(1.+hc_alpha)*elen(ior)**2*Cr_zahl/hc_DL
      dtneu_Mac = 0.4*hc_alpha*(1.+hc_alpha)*elen(ior)**2*Cr_zahl_Mac/hc_DL
      if(dtneu.lt.dtmin.and.ior.le.anze)dtmin = dtneu
      if(dtneu_Mac.lt.dtmin_Mac.and.ior.le.anze)dtmin_Mac = dtneu_Mac
  614 continue 

!      flag(anze+1) = 0 
                                                                       
       iSTRiz_neu = int((tflie*86400.)/dtmin)+1
!      Strdt(mstr) = tflie*86400./StRiz(mstr)                        
       isub_dt(mstr) = (iStriz_neu/Striz(mstr))+1 
       if(isub_dt(mstr)==0)isub_dt(mstr) = 1
     
       iSTRiz_neu = int((tflie*86400.)/dtmin_Mac)+1
       isub_dt_Mac(mstr) = (iStriz_neu/Striz(mstr))+1 
       if(isub_dt_Mac(mstr)==0)isub_dt_Mac(mstr) = 1

!..........................................                             
!..         BAUSTEINE                    ..                             
!..........................................                             
!                                                                       
!...nur Tracer                                                          
  615 if(iwsim==4.or.iwsim==5)goto 705

      call strahlg(glob,uhrz,sa,su,schwi,tflie,geol,tdj,geob,dk,cloud,schwia,imet,mstr,IDWe,itags,monats,VTYP         &      
                  ,VALTBL,EDUFBL,VALTBR,EDUFBR,breite,anze,ifehl,ifhStr,it_h,ij,jahrs,itage,monate,jahre,uhren        &
                  ,isim_end,azStr,azStrs)
      if(ifehl.gt.0)goto 989 
                                                                       
      call Temperl(SA,SU,Uhrz,TEMPL,mstr,IDWe,TLMAX,TLMIN,anze,imet,azStrs) 

  705 if(nbuhn(mstr).eq.0)goto 709 
!                                                                       
!....Berechnung der Austauschraten zwischen Hauptstrom und Buhnenfelder 
      do ior = 1,anze+1 
        hctau1(ior) = ((bf(mstr,ior)/flae(ior))*tau2(ior))*tflie*86400.                                                     
        hctau2(ior) = tau2(ior)*tflie*86400. 
        hconF = bf(mstr,ior)/flae(ior) 
        htau1m = -0.3196*hconF**2+0.8026*hconF+0.0122 

!        if(hctau1(ior).gt.htau1m)then 
!          hctau1(ior) = htau1m 
!          hctau2(ior) = 1.-htau1m 
!        endif 
      enddo 

  709 if(iwsim==2.or.iwsim==5.or.iwsim==4)goto 113 
                                                                       
!***************Sediment-Stofffluxe********                             
 1712 continue     
 
       call sedflux(tiefe,vmitt,rau,sedAlg_MQ,hSedOM,hw2,hBedGS,hsedvvert,hdKorn,vO2,vNO3,vNH4,gelP           &
                   ,Tempw,anze,mstr,hJNO3,hJNH4,hJPO4,hJO2,hJN2,sedalk,sedalg                                 &
                   ,sedalb,sedSS_MQ,KNH4e,kapN3e,tflie,ilbuhn,itags,monats,uhrz,vo2z                          &
                   ,vnh4z,vno3z,gelpz,nkzs,SorpCape,Klange,KdNh3e,fPOC1e,fPOC2e                               &
                   ,orgCsd_abb,hCD,JDOC1,JDOC2,Q_NK,Q_PK,Q_NG,Q_PG,Q_NB,Q_PB,pl0,nl0,Si,hSised,hJSi           &
                   ,aki,agr,abl,Chlaki,Chlagr,Chlabl,hFluN3,ilang,azStrs,iwied,yNmx1e,Stks1e,obsb,ocsb        &
                   , .FALSE., 0) !!wy ,kontroll, iglob 3D
      if(nbuhn(mstr).eq.0)goto 1612 
      if(ilbuhn.eq.0)then 
      do 1710 ior=1,anze+1 
      zww2(ior) = hw2(mstr,ior) 
      zwSdOM(ior) = hSedOM(mstr,ior) 
      zwKorn(ior) = hdKorn(mstr,ior) 
      zwtemp(ior) = tempw(ior) 
      zwvm(ior) = vmitt(ior) 
      zwtief(ior) = tiefe(ior) 
      zwvo2(ior) = vo2(ior) 
      zwno3(ior) = vno3(ior) 
      zwnh4(ior) = vnh4(ior) 
      zwgelp(ior) = gelp(ior)
      zwsi(ior) = si(ior) 
      zwobsb(ior) = obsb(ior) 
      zwocsb(ior) = ocsb(ior) 
      zwJNO3(ior) = hJNO3(mstr,ior) 
      zwJNH4(ior) = hJNH4(mstr,ior) 
      zwJPO4(ior) = hJPO4(mstr,ior) 
      zwJO2(ior) = hJO2(mstr,ior)
      zwJN2(ior) = hJN2(mstr,ior)
      zwJSi(ior) = hJSi(mstr,ior) 
      zwsedS(ior) = sedss(ior) 
      zwCsed(ior) = orgCsd(mstr,ior)
      zwCsed_abb(ior) = orgCsd_abb(mstr,ior)
      zwsedk(ior) = sedalk(ior) 
      zwsedg(ior) = sedalg(ior) 
      zwsedb(ior) = sedalb(ior) 
      zwnkzs(ior) = nkzs(ior)
      zQ_PK(ior) = Q_PK(ior) 
      zQ_NK(ior) = Q_NK(ior) 
      zQ_PG(ior) = Q_PG(ior) 
      zQ_NG(ior) = Q_NG(ior) 
      zQ_PB(ior) = Q_PB(ior) 
      zQ_NB(ior) = Q_NB(ior) 
      zwpl0(ior) = pl0(ior) 
      zwnl0(ior) = nl0(ior)
      zwcd(1,ior) = CD(1,ior) 
      zwcd(2,ior) = CD(2,ior)
      zwJDOC1(ior) = JDOC1(ior)
      zwJDOC2(ior) = JDOC2(ior)
      zwsedAlg_MQ(ior) = sedAlg_MQ(mstr,ior)
      zwsedSS_MQ(ior) = sedSS_MQ(mstr,ior)
      zwSisd(ior) = hSised(mstr,ior)
      zwFlN3(ior) = hFluN3(mstr,ior) 
 
                                                                       
      hw2(mstr,ior) = bw2(mstr,ior) 
      hSedOM(mstr,ior) = bSedOM(mstr,ior) 
      hdKorn(mstr,ior) = bdKorn(mstr,ior) 
      tempw(ior) = btempw(mstr,ior) 
      vmitt(ior) = vbm(mstr,ior) 
      tiefe(ior) = bh(mstr,ior) 
      vo2(ior) = bo2(mstr,ior) 
      vno3(ior) = bno3(mstr,ior) 
      vnh4(ior) = bnh4(mstr,ior) 
      gelp(ior) = bgelp(mstr,ior)
      Si(ior) = bSi(mstr,ior)
      obsb(ior) = bbsb(mstr,ior) 
      ocsb(ior) = bcsb(mstr,ior) 
      sedSS(ior) = bsedSS(mstr,ior) 
      sedalk(ior) = bsedak(mstr,ior) 
      sedalg(ior) = bsedag(mstr,ior) 
      sedalb(ior) = bsedab(mstr,ior) 
      orgCsd(mstr,ior) = borgCs(mstr,ior) 
      orgCsd_abb(mstr,ior) = borgCs_abb(mstr,ior)
      Q_PK(ior) = bQ_PK(mstr,ior) 
      Q_NK(ior) = bQ_NK(mstr,ior) 
      Q_PG(ior) = bQ_PG(mstr,ior) 
      Q_NG(ior) = bQ_NG(mstr,ior) 
      Q_PB(ior) = bQ_PB(mstr,ior) 
      Q_NB(ior) = bQ_NB(mstr,ior) 
      pl0(ior) = bpl0(mstr,ior) 
      nl0(ior) = bnl0(mstr,ior)
      CD(1,ior) = bCD(mstr,1,ior) 
      CD(2,ior) = bCD(mstr,2,ior)
      JDOC1(ior) = bJDOC1(ior)
      JDOC2(ior) = bJDOC2(ior)
      hFluN3(mstr,ior) = bFluN3(mstr,ior)
      sedAlg_MQ(mstr,ior) = bsedAlg_MQ(mstr,ior)
      sedSS_MQ(mstr,ior) = bsedSS_MQ(mstr,ior)
      hSised(mstr,ior) = bSised(mstr,ior) 
      nkzs(ior) = 1 
                                                                       
 1710 continue 
      ilbuhn = 1 
      goto 1712 
      endif 
!                                                                       
      if(ilbuhn.eq.1)then 
      do 1714 ior=1,anze+1 
      bJNO3(mstr,ior) = hJNO3(mstr,ior) 
      bJNH4(mstr,ior) = hJNH4(mstr,ior) 
      bJPO4(mstr,ior) = hJPO4(mstr,ior) 
      bJO2(mstr,ior) = hJO2(mstr,ior)
      bJN2(mstr,ior) = hJN2(mstr,ior)
      bJSi(mstr,ior) = hJSi(mstr,ior)
      bJDOC1(ior) = JDOC1(ior)
      bJDOC2(ior) = JDOC2(ior)
      bFluN3(mstr,ior) = hFluN3(mstr,ior) 

      tempw(ior) = zwtemp(ior) 
      tiefe(ior) = zwtief(ior) 
      vmitt(ior) = zwvm(ior) 
      vo2(ior) = zwvo2(ior) 
      vno3(ior) = zwno3(ior) 
      vnh4(ior) = zwnh4(ior) 
      gelp(ior) = zwgelp(ior)
      si(ior) = zwSi(ior) 
      obsb(ior) = zwobsb(ior) 
      ocsb(ior) = zwocsb(ior) 
      hw2(mstr,ior) = zww2(ior) 
      hSedOM(mstr,ior) = zwSdOM(ior) 
      hdKorn(mstr,ior) = zwKorn(ior) 
      hJNO3(mstr,ior) = zwJNO3(ior) 
      hJNH4(mstr,ior) = zwJNH4(ior) 
      hJPO4(mstr,ior) = zwJPO4(ior) 
      hJO2(mstr,ior) = zwJO2(ior)
      hJN2(mstr,ior) = zwJN2(ior)
      sedss(ior) = zwsedS(ior) 
      sedalk(ior) = zwsedk(ior) 
      sedalg(ior) = zwsedg(ior) 
      sedalb(ior) = zwsedb(ior) 
      orgCsd(mstr,ior) = zwCsed(ior)
      orgCsd_abb(mstr,ior) = zwCsed_abb(ior)   
      Q_PK(ior) = zQ_PK(ior) 
      Q_NK(ior) = zQ_NK(ior) 
      Q_PG(ior) = zQ_PG(ior) 
      Q_NG(ior) = zQ_NG(ior) 
      Q_PB(ior) = zQ_PB(ior) 
      Q_NB(ior) = zQ_NB(ior) 
      pl0(ior) = zwpl0(ior) 
      nl0(ior) = zwnl0(ior)
      cd(1,ior) = zwCD(1,ior) 
      cd(2,ior) = zwCD(2,ior)
      JDOC1(ior) = zwJDOC1(ior)
      JDOC2(ior) = zwJDOC2(ior)
      hFluN3(mstr,ior) = zwFlN3(ior)
      sedAlg_MQ(mstr,ior) = zwsedAlg_MQ(ior)
      sedSS_MQ(mstr,ior) = zwsedSS_MQ(ior) 
      hSised(mstr,ior) = zwSisd(ior)
      hJSi(mstr,ior) = zwJSi(ior)
      nkzs(ior) = zwnkzs(ior)

 1714 continue 
      ilbuhn = 0 
      endif 
!                                                                       
!***********Rotatorien***********************                           
!                                                                       
                                                                       
 1612 continue       
      call konsum(vkigr,TEMPW,VO2,TFLIE                                 &
     &,ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl             &
     &,jiein,FopIRe,GRote,dzres1,dzres2,zresge                          &
     &,irmaxe,zexki,zexgr,zexbl                                         &
     &,aki,agr,abl,iwied,rmuas,iras,TGZoo,BAC,zBAC                      &
     &,rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
     &,algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
     &,itags,uhrz,mstr,azStrs , .FALSE., 0) !!wy ,kontroll, iglob 3D
      
      if(nbuhn(mstr)==0)goto 1415 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwvm(ior) = vmitt(ior) 
        zwir(ior) = ir(ior) 
        zwkigr(ior) = vkigr(ior) 
        zwantb(ior) = antbl(ior) 
        zwaki(ior) = aki(ior) 
        zwagr(ior) = agr(ior) 
        zwabl(ior) = abl(ior)
        zwTGZoo(ior) = TGZoo(mstr,ior) 

      do nkz = 1,nkzs(ior) 
        zwakiz(nkz,ior) = akiz(nkz,ior) 
        zwagrz(nkz,ior) = agrz(nkz,ior) 
        zwablz(nkz,ior) = ablz(nkz,ior) 
        zwchlz(nkz,ior) = chlaz(nkz,ior) 
      enddo

        zwvo2(ior) = vo2(ior) 
        zwzooi(ior) = zooind(ior) 
        zwabsz(ior) = abszo(ior) 
        zwdzr1(ior) = dzres1(ior) 
        zwdzr2(ior) = dzres2(ior) 
        zwzexk(ior) = zexki(ior) 
        zwzexg(ior) = zexgr(ior) 
        zwzexb(ior) = zexbl(ior) 
        zwrmue(ior) = rmuas(ior) 
        zwiras(ior) = iras(ior) 
        zwrakr(ior) = rakr(ior) 
        zwrbar(ior) = rbar(ior) 
        zwzok(ior) = algzok(ior) 
        zwzog(ior) = algzog(ior) 
        zwzob(ior) = algzob(ior) 
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior) 
        vkigr(ior) = bvkigr(mstr,ior) 
        antbl(ior) = bantbl(mstr,ior) 
        aki(ior) = baki(mstr,ior) 
        agr(ior) = bagr(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
        vo2(ior) = bo2(mstr,ior) 
        zooind(ior) = bzooi(mstr,ior)
        if(iwied==1)TGZoo(mstr,ior) = bTGZoo(mstr,ior) 
      enddo                                                                 

      ilbuhn = 1 
      goto 1612 
    endif 
                                                                       
      if(ilbuhn==1)then 
        do ior=1,anze+1 
          bir(mstr,ior) = ir(ior) 
          bzooi(mstr,ior) = zooind(ior)
          bTGZoo(mstr,ior) = TGZoo(mstr,ior) 
          babszo(mstr,ior) = abszo(ior) 
          bzres1(mstr,ior) = dzres1(ior) 
          bzres2(mstr,ior) = dzres2(ior) 
          bzexki(mstr,ior) = zexki(ior) 
          bzexgr(mstr,ior) = zexgr(ior) 
          bzexbl(mstr,ior) = zexbl(ior) 
          brmuas(mstr,ior) = rmuas(ior) 
          biras(mstr,ior) = iras(ior) 
          brakr(mstr,ior) = rakr(ior) 
          brbar(mstr,ior) = rbar(ior) 
          bazok(mstr,ior) = algzok(ior) 
          bazog(mstr,ior) = algzog(ior) 
          bazob(mstr,ior) = algzob(ior) 
                                                                       
          tempw(ior) = zwtemp(ior) 
          vmitt(ior) = zwvm(ior) 
          ir(ior) = zwir(ior) 
          vkigr(ior) = zwkigr(ior) 
          antbl(ior) = zwantb(ior) 
          aki(ior) = zwaki(ior) 
          agr(ior) = zwagr(ior) 
          abl(ior) = zwabl(ior) 

          do nkz = 1,nkzs(ior) 
            akiz(nkz,ior) = zwakiz(nkz,ior) 
            agrz(nkz,ior) = zwagrz(nkz,ior) 
            ablz(nkz,ior) = zwablz(nkz,ior) 
            chlaz(nkz,ior) = zwchlz(nkz,ior)
          enddo 

          vo2(ior) = zwvo2(ior) 
          zooind(ior) = zwzooi(ior) 
          TGZoo(mstr,ior) = zwTGZoo(ior) 
          abszo(ior) = zwabsz(ior) 
          dzres1(ior) = zwdzr1(ior) 
          dzres2(ior) = zwdzr2(ior) 
          zexki(ior) = zwzexk(ior) 
          zexgr(ior) = zwzexg(ior) 
          zexbl(ior) = zwzexb(ior) 
          rmuas(ior) = zwrmue(ior) 
          iras(ior) = zwiras(ior) 
          rakr(ior) = zwrakr(ior) 
          rbar(ior) = zwrbar(ior) 
          algzok(ior) = zwzok(ior) 
          algzog(ior) = zwzog(ior) 
          algzob(ior) = zwzob(ior) 

      diff1 = bzooi(mstr,ior)-zooind(ior)
      diff2 = bTGZoo(mstr,ior)-TGZoo(mstr,ior)
      bdiff1 = zooind(ior)-bzooi(mstr,ior)
      bdiff2 = TGZoo(mstr,ior)-bTGZoo(mstr,ior)
 
      if(bleb(mstr,ior)>0.0)then
        zooind(ior) = zooind(ior)+diff1*(1.-exp(-hctau1(ior))) 
        TGZoo(mstr,ior) = TGZoo(mstr,ior)+diff2*(1.-exp(-hctau1(ior))) 
      endif      
                                                                    
      if(hctau2(ior)>0.0)then 
        bzooi(mstr,ior) = bzooi(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
        bTGZoo(mstr,ior) = bTGZoo(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
      endif
     enddo 
                                                                       
    ilbuhn = 0 
  endif 
!                                                                       
!***************Chorophium******************                            
!                                                                       
 1415 continue 
                                                                       
      if(nbuhn(mstr).eq.0)goto 1440 
      do 1783 ior = 1,anze+1 
      do 1784 jC=1,5 
      zwcoro(ior,jC) = coro(ior,jC) 
 1784 Coro(ior,jC) = 0.0 
 1783 continue 
!                                                                       
 1440 call coroph(coro,coros,tempw,flae,elen,anze,ior                                  &
     &,volfco,aki,agr,algcok,algcog,tflie,bsohlm,lboem,coroI                           &
     &,coroIs,abl,algcob,mstr,itags,monats,jahrs,ilang,nbuhn,ilbuhn,azStrs , .FALSE., 0) !!wy ,kontroll, iglob 3D
                                                                       
      if(nbuhn(mstr)==0)goto 1441 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwflae(ior) = flae(ior) 
        zwlboe(ior) = lboem(ior) 
        zwaki(ior) = aki(ior) 
        zwagr(ior) = agr(ior) 
        zwabl(ior) = abl(ior) 
        zwacok(ior) = algcok(ior) 
        zwacog(ior) = algcog(ior) 
        zwacob(ior) = algcob(ior) 
        zwCoIs(ior) = coroIs(ior) 

        do jC=1,5 
          zwcors(ior,jC) = coros(ior,jC) 
          Coros(ior,jC) = 0.0 
          coro(ior,jC) = zwcoro(ior,jC)
        enddo 
                                                                       
        flae(ior) = bf(mstr,ior) 
        lboem(ior) = blb(mstr,ior) 
        tempw(ior) = btempw(mstr,ior) 
        aki(ior) = baki(mstr,ior) 
        agr(ior) = bagr(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
      enddo

      ilbuhn = 1 
      goto 1440 
      endif 
                                                                       
      if(ilbuhn==1)then 
      do 1787 ior=1,anze+1 
      bacok(mstr,ior) = algcok(ior) 
      bacog(mstr,ior) = algcog(ior) 
      bacob(mstr,ior) = algcob(ior) 
!                                                                       
      flae(ior) = zwflae(ior) 
      lboem(ior) = zwlboe(ior) 
      tempw(ior) = zwtemp(ior) 
      CoroIs(ior) = zwCoIs(ior) 
      do 1788 jC=1,5 
 1788 coros(ior,jC) = zwcors(ior,jC) 
      aki(ior) = zwaki(ior) 
      agr(ior) = zwagr(ior) 
      abl(ior) = zwabl(ior) 
      algcok(ior) = zwacok(ior) 
      algcog(ior) = zwacog(ior) 
      algcob(ior) = zwacob(ior) 
!                                                                       
 1787 continue 
!                                                                       
      ilbuhn = 0 
      endif 
!                                                                       
!                                                                       
!*************Dreissena*******************                              
!                                                                       
 1441 continue
                                                                       
      if(nbuhn(mstr)==1)then ! Annahme: Dreissena nur im Buhnenfeld
         do ior=1,anze+1 
           zwtemp(ior) = tempw(ior)
           tempw(ior) = btempw(mstr,ior) 
           zwflae(ior) = flae(ior)
           flae(ior) = max(0.001,bf(mstr,ior)) 
           zwlboe(ior) = lboem(ior)
           lboem(ior) = max(0.001,blb(mstr,ior))
           zwbso(ior) = bsohlm(ior)
           bsohlm(ior) = max(0.001,bso(mstr,ior)) 
           zwaki(ior) = aki(ior)
           aki(ior) = baki(mstr,ior) 
           zwagr(ior) = agr(ior) 
           agr(ior) = bagr(mstr,ior)
           zwabl(ior) = abl(ior) 
           abl(ior) = babl(mstr,ior)
           zwssa(ior) = ssalg(ior)
           ssalg(ior) = bssalg(mstr,ior)
           zwss(ior) = ss(ior)
           ss(ior) = bss(mstr,ior)
           zwkbcm(ior) = akbcm(ior)
           akbcm(ior) = bakbcm(mstr,ior)
           zwgbcm(ior) = agbcm(ior)
           agbcm(ior) = bagbcm(mstr,ior)
           zwbbcm(ior) = abbcm(ior) 
           abbcm(ior) = babbcm(mstr,ior)
           dlarvn(ior) = bdlarn(mstr,ior)
         enddo   

      endif

      call dreissen(zdrei,zdreis,tempw,flae,elen,anze                   &
     &,ior,volfdr,akbcm,agbcm,aki,agr,algdrk,algdrg                     &
     &,tflie,ro2dr,lboem,bsohlm,ss,vo2,ssdr,drfaek                      &
     &,drfaeg,drfaes,gewdr,dlarvn,itags,monats,jahrs                    &
     &,lait1,laim1,laid1,ilang                                          &
     &,resdr,exdrvg,exdrvk,ssalg,drpfec                                 &
     &,abl,exdrvb,abbcm,algdrb,drfaeb                                   &
     &,idras,drmas,drakr,drbar,drmor,ffood,coroI,coroIs                 &
     &,CHNF,drHNF,HNFdra,dlmax,dlmaxs,gwdmax                            &
     &,sgwmue,fkm,FoptDe,mstr,azStr, .FALSE., 0) !!wy ,kontroll, iglob 3D

      if(nbuhn(mstr)==1)then 
        do ior=1,anze+1
          tempw(ior) = zwtemp(ior)  
           flae(ior) = zwflae(ior)
           lboem(ior) = zwlboe(ior) 
           bsohlm(ior) = zwbso(ior)
           aki(ior) = zwaki(ior)
           agr(ior) = zwagr(ior) 
           abl(ior) = zwabl(ior) 
           ssalg(ior) = zwssa(ior) 
           ss(ior) = zwss(ior)
           akbcm(ior) = zwkbcm(ior) 
           agbcm(ior) = zwgbcm(ior) 
           abbcm(ior) = zwbbcm(ior)
           badrk(mstr,ior) = algdrk(ior)
           badrg(mstr,ior) = algdrg(ior)  
           badrb(mstr,ior) = algdrb(ior)
           bdfaek(mstr,ior) = drfaek(ior)
           bdfaeg(mstr,ior) = drfaeg(ior)
           bdfaeb(mstr,ior) = drfaeb(ior)
           bdfaes(mstr,ior) = drfaes(ior)
           bexdvk(mstr,ior) = exdrvk(ior)
           bexdvg(mstr,ior) = exdrvg(ior)
           bexdvb(mstr,ior) = exdrvb(ior)
           bssdr(mstr,ior) = ssdr(ior)
           bresdr(mstr,ior) = resdr(ior)
           algdrk(ior) = 0.0
           algdrg(ior) = 0.0
           algdrb(ior) = 0.0
           drfaek(ior) = 0.0
           drfaeg(ior) = 0.0
           drfaeb(ior) = 0.0
           drfaes(ior) = 0.0
           exdrvk(ior) = 0.0
           exdrvg(ior) = 0.0
           exdrvb(ior) = 0.0
           ssdr(ior) = 0.0
           resdr(ior) = 0.0
           bdlarn(mstr,ior) = dlarvn(ior)
           dlarvn(ior) = 0.0
         enddo
      endif     
                                                                       
!***************hetrero. Nanoflagelaten (HNF)********                   
                                                                       
  218 continue 
      if(CHNF(1)<=0.0)then
        if(nbuhn(mstr)>0)then
          do ior=1,anze+1
            bro2HF(mstr,ior) = 0.0
            bHNFBS(mstr,ior) = 0.0
            bBSBHN(mstr,ior) = 0.0 
          enddo
        endif 
        goto 1412
      endif
         
      call HNF(CHNF,BVHNF,BAC,TEMPW,VO2,TFLIE                           &
     &,echnf,eBVHNF,flag,elen,ior,anze,qeinl,vabfl                      &
     &,jiein,drHNF,zHNF,HNFBAC,rO2HNF,BSBHNF,HNFmua,upHNFe,BACkse       &
     &,HNFrea,HNFupa,HNFmoa,HNFexa,fkm,mstr,itags,monats,uhrz, .FALSE., 0) !!wy ,kontroll, iglob 3D
!                                                                       
!                                                                       
!***********Kieselalgen***************                                  
!                                                                       
 1412 continue 

!      if(hChla(mstr,1)<0.0)goto 1513

      call       algaeski(SCHWI,TFLIE,TEMPW,tempwz,RAU,TIEFE,VMITT,flae,VNO3,VNH4,GELP,svhemk,svhemb,svhemg,CHLA,ir                    &
                ,SI,dalgki,dalgak,flag,elen,ior,anze,sedalk,algzok,echla,qeinl,vabfl                                                   &
                ,dkimor,fkm,jiein,evkigr,vkigr,antbl,eantbl,akchl,akgmax,akksn,akksp,akkssi,saettk,akremi,akrema                       &
                ,sbioki,vco2,iph,akbcm,abbcm,agbcm,aki,abl,agr,extk,extk_lamda                                                         &
     &          ,ilamda,eta,aw,ack,acg,acb,ah,as,al                                                                                    & !!wy Licht-Extinktionsparameter
                ,uhrz,sised,tpki,iwied,akmuea,ftaaus,fiaus,fheaus                                                                      &
                ,akraus,tauscs,ischif,ilbuhn,ieros,askie,cmatki,algdrk,algcok,ess,zooind,GRote,SS,Q_PK,Q_NK,Q_SK                       &
                ,vNH4z,vNO3z,gelPz,Siz,dalgkz,nkzs,dH2D,cpfad,up_PKz,up_NKz,up_Siz,Qmx_PK,Qmn_PK,upmxPK                                &
                ,Qmx_NK,Qmn_NK,upmxNK,Qmx_SK,Qmn_SK,upmxSK,SKmor,IKke,frmuke,alamda,akitbr,chlaz,akibrz,akiz,chlaL,qeinlL              &
                ,ieinLs,algakz,algzkz,ablz,agrz,Chlaki,hchlkz,hchlgz,hchlbz,hCChlkz,hCChlbz,hCChlgz,Dz2D,ToptK,kTemp_Ki                &
                ,ifix,Chlabl,Chlagr,a1Ki,a2Ki,a3Ki,sedAlg_MQ,sedAlk0,hQ_NKz,hQ_NGz,hQ_NBz,Q_PG,Q_NG,Q_PB,Q_NB                          &
                ,mstr,it_h,itags,monats,isim_end,extkS,akmor_1,agmor_1,abmor_1,azStrs                                                  &
     &          ,.false.,0)     !!wy kontroll,iglob
                                             
      if(nbuhn(mstr)==0)goto 1413 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwtief(ior) = tiefe(ior) 
        zwvm(ior) = vmitt(ior) 
        zwno3(ior) = vno3(ior) 
        zwnh4(ior) = vnh4(ior) 
        zwgelp(ior) = gelp(ior) 
        zwsvhk(ior) = svhemk(ior) 
        zwchla(ior) = chla(ior)
        zwchlk(ior) = chlaki(ior) 
        zwzooi(ior) = zooind(ior) 
        zwir(ior) = ir(ior) 
        zwssa(ior) = ssalg(ior) 
        zwsi(ior) = si(ior) 
        zwdalk(ior) = dalgki(ior) 
        zwdaak(ior) = dalgak(ior) 
        zwsedk(ior) = sedalk(ior) 
        zwzok(ior) = algzok(ior) 
        zwadrk(ior) = algdrk(ior) 
        zwacok(ior) = algcok(ior) 
        zwkmor(ior) = dkimor(ior) 
        zwkigr(ior) = vkigr(ior) 
        zwantb(ior) = antbl(ior) 
        zwkbcm(ior) = akbcm(ior) 
        zwgbcm(ior) = agbcm(ior) 
        zwbbcm(ior) = abbcm(ior) 
        zwaki(ior) = aki(ior) 
        zwagr(ior) = agr(ior) 
        zwabl(ior) = abl(ior) 
        zwsisd(ior) = sised(ior) 
        zwSKmo(ior) = SKmor(ior) 
        zwkmua(ior) = akmuea(ior) 
        zwfta(ior) = ftaaus(ior) 
        zwfia(ior) = fiaus(ior) 
        zwfhea(ior) = fheaus(ior) 
        zwkrau(ior) = akraus(ior) 
        zwtpki(ior) = tpki(ior) 
        zwextk(ior) = extk(ior) 
        zup_PK(ior) = up_PKz(1,ior) 
        zup_NK(ior) = up_NKz(1,ior) 
        zup_Si(ior) = up_Siz(1,ior) 
        zQ_PK(ior) = Q_PK(ior) 
        zQ_NK(ior) = Q_NK(ior) 
        zQ_SK(ior) = Q_SK(ior) 
        zaktbr(ior) = akibrz(1,ior) 
        zwakz(ior) = dalgkz(1,ior) 
        zwaakz(ior) = algakz(1,ior) 
        zwnkzs(ior) = nkzs(ior) 
        zwN4z(ior) = vNH4z(1,ior) 
        zwN3z(ior) = vNO3z(1,ior) 
        zwSiz(ior) = Siz(1,ior) 
        zwPz(ior) = gelPz(1,ior) 
        zwsedAlg_MQ(ior) = sedAlg_MQ(mstr,ior)
        zwakmor_1(ior) = akmor_1(mstr,ior)
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior) 
        vno3(ior) = bno3(mstr,ior) 
        vnh4(ior) = bnh4(mstr,ior) 
        gelp(ior) = bgelp(mstr,ior) 
        svhemk(ior) = bsvhek(mstr,ior) 
        chla(ior) = bchla(mstr,ior)
        chlaki(ior) = bchlak(mstr,ior) 
        zooind(ior) = bzooi(mstr,ior) 
        ir(ior) = bir(mstr,ior) 
        ssalg(ior) = bssalg(mstr,ior) 
        si(ior) = bsi(mstr,ior) 
        dalgki(ior) = bdaki(mstr,ior) 
        dalgak(ior) = bdaak(mstr,ior) 
        sedalk(ior) = bsedak(mstr,ior) 
        dkimor(ior) = bdkmor(mstr,ior) 
        algzok(ior) = bazok(mstr,ior) 
        algdrk(ior) = badrk(mstr,ior) 
        algcok(ior) = bacok(mstr,ior) 
        vkigr(ior) = bvkigr(mstr,ior) 
        antbl(ior) = bantbl(mstr,ior) 
        akbcm(ior) = bakbcm(mstr,ior) 
        agbcm(ior) = bagbcm(mstr,ior) 
        abbcm(ior) = babbcm(mstr,ior) 
        aki(ior) = baki(mstr,ior) 
        agr(ior) = bagr(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
        sised(ior) = bsised(mstr,ior) 
        SKmor(ior) = bSKmor(mstr,ior) 
        Q_PK(ior) = bQ_PK(mstr,ior) 
        Q_NK(ior) = bQ_NK(mstr,ior) 
        Q_SK(ior) = bQ_SK(mstr,ior) 
        vNH4z(1,ior) = bnh4(mstr,ior) 
        vNO3z(1,ior) = bno3(mstr,ior) 
        Siz(1,ior) = bsi(mstr,ior) 
        gelPz(1,ior) = bgelp(mstr,ior) 
        sedAlg_MQ(mstr,ior) = bsedAlg_MQ(mstr,ior)
        akmor_1(mstr,ior) = bakmor_1(mstr,ior)
      enddo

      ilbuhn = 1 
      goto 1412 
      endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        bsvhek(mstr,ior) = svhemk(ior) 
        bdaki(mstr,ior) = dalgki(ior) 
        bdaak(mstr,ior) = dalgak(ior) 
        bsedak(mstr,ior) = sedalk(ior) 
        bdkmor(mstr,ior) = dkimor(ior) 
        bakbcm(mstr,ior) = akbcm(ior) 
        bagbcm(mstr,ior) = agbcm(ior) 
        babbcm(mstr,ior) = abbcm(ior) 
        baki(mstr,ior) = aki(ior) 
        bagr(mstr,ior) = agr(ior) 
        babl(mstr,ior) = abl(ior) 
        bsised(mstr,ior) = sised(ior) 
        bSKmor(mstr,ior) = SKmor(ior) 
        bakmua(mstr,ior) = akmuea(ior) 
        bftaau(mstr,ior) = ftaaus(ior) 
        bfiaus(mstr,ior) = fiaus(ior) 
        bfheau(mstr,ior) = fheaus(ior) 
        bakrau(mstr,ior) = akraus(ior) 
        btpki(mstr,ior) = tpki(ior) 
        bextk(mstr,ior) = extk(ior) 
        bQ_PK(mstr,ior) = Q_PK(ior) 
        bQ_NK(mstr,ior) = Q_NK(ior) 
        bQ_SK(mstr,ior) = Q_SK(ior)
        bchlak(mstr,ior) = chlaki(ior) 
        bup_PK(mstr,ior) = up_PKz(1,ior) 
        bup_NK(mstr,ior) = up_NKz(1,ior) 
        bup_Si(mstr,ior) = up_Siz(1,ior) 
        baktbr(mstr,ior) = akibrz(1,ior) 
        balgkz(mstr,ior) = dalgkz(1,ior) 
        balakz(mstr,ior) = algakz(1,ior) 
        bsedAlg_MQ(mstr,ior) = sedAlg_MQ(mstr,ior)
        bakmor_1(mstr,ior) = akmor_1(mstr,ior)
                                                                       
        tempw(ior) = zwtemp(ior) 
        tiefe(ior) = zwtief(ior) 
        vmitt(ior) = zwvm(ior) 
        vno3(ior) = zwno3(ior) 
        vnh4(ior) = zwnh4(ior) 
        gelp(ior) = zwgelp(ior) 
        svhemk(ior) = zwsvhk(ior) 
        chla(ior) = zwchla(ior)
        chlaki(ior) = zwchlk(ior) 
        zooind(ior) = zwzooi(ior) 
        ir(ior) = zwir(ior) 
        ssalg(ior) = zwssa(ior) 
        si(ior) = zwsi(ior) 
        dalgki(ior) = zwdalk(ior) 
        dalgak(ior) = zwdaak(ior) 
        sedalk(ior) = zwsedk(ior) 
        algzok(ior) = zwzok(ior) 
        algdrk(ior) = zwadrk(ior) 
        algcok(ior) = zwacok(ior) 
        dkimor(ior) = zwkmor(ior) 
        vkigr(ior) = zwkigr(ior) 
        antbl(ior) = zwantb(ior) 
        akbcm(ior) = zwkbcm(ior) 
        agbcm(ior) = zwgbcm(ior)
        abbcm(ior) = zwbbcm(ior)  
        aki(ior) = zwaki(ior) 
        agr(ior) = zwagr(ior) 
        abl(ior) = zwabl(ior) 
        sised(ior) = zwsisd(ior) 
        SKmor(Ior) = zwSKmo(ior) 
        akmuea(ior) = zwkmua(ior) 
        ftaaus(ior) = zwfta(ior) 
        fiaus(ior) = zwfia(ior) 
        fheaus(ior) = zwfhea(ior) 
        akraus(ior) = zwkrau(ior) 
        tpki(ior) = zwtpki(ior) 
        extk(ior) = zwextk(ior) 
        up_PKz(1,ior) = zup_PK(ior) 
        up_NKz(1,ior) = zup_NK(ior) 
        up_Siz(1,ior) = zup_Si(ior) 
        Q_PK(ior) = zQ_PK(ior) 
        Q_NK(ior) = zQ_NK(ior) 
        Q_SK(ior) = zQ_SK(ior) 
        akibrz(1,ior) = zaktbr(ior) 
        dalgkz(1,ior) = zwakz(ior) 
        algakz(1,ior) = zwaakz(ior) 
        sedAlg_MQ(mstr,ior) = zwsedAlg_MQ(ior)
        akmor_1(mstr,ior) = zwakmor_1(ior)
                                                                       
        nkzs(ior) = zwnkzs(ior) 
        vNH4z(1,ior) = zwN4z(ior) 
        vNO3z(1,ior) = zwN3z(ior) 
        siz(1,ior) = zwsiz(ior) 
        gelPz(1,ior) = zwPz(ior) 

        diff1 = bsvhek(mstr,ior)-svhemk(ior)
        diff2 = bSKmor(mstr,ior)-SKmor(ior)

        bdiff1 = svhemk(ior)-bsvhek(mstr,ior)
        bdiff2 = SKmor(ior)-bSKmor(mstr,ior)

      if(bleb(mstr,ior)>0.0)then
        svhemk(ior) = svhemk(ior)+diff1*(1.-exp(-hctau1(ior))) 
        SKmor(ior) = SKmor(ior)+diff2*(1.-exp(-hctau1(ior))) 

        if(svhemk(ior)<0.0)svhemk(ior) = 0.0 
        if(SKmor(ior)<0.0)SKmor(ior) = 0.0 
      endif     
                                                                       
        if(hctau2(ior)>0.0)then 
          bsvhek(mstr,ior) = bsvhek(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bSKmor(mstr,ior) = bSKmor(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
        endif

        if(bsvhek(mstr,ior)<0.0)bsvhek(mstr,ior) = 0.0 
        if(bSKmor(mstr,ior)<0.0)bSKmor(mstr,ior) = 0.0 

       enddo 
                                                                       
      ilbuhn = 0 
      endif    
                                                                       
!**********************Blaualgen******************                      
                                                                       
 1413 continue 
                                                                       


      call                algaesbl(SCHWI,TFLIE,TEMPW,flag,elen,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemb,CHLA,ir               &
                         ,dalgbl,dalgab,ior,anze,sedalb,algzob,dblmor,fkm,vabfl,abchl,abgmax,abksn,abksp,saettb,abremi     &
                         ,vco2,iph,vkigr,abbcm,abl,tpbl,uhrz,iwied,fibaus,abmuea,fhebas,abreau,tauscs,ischif,ilbuhn,ieros  &
                         ,zakie,zagre,zable,asble,qeinl,jiein,echla,ess,algdrb,algcob,antbl,zooind,GRote,SS,extk           &
                         ,extk_lamda                                                                                       &
     &                   ,ilamda,eta,aw,ack,acg,acb,ah,as,al                                                               & !!wy Licht-Extinktionsparameter
     &                   ,vNH4z,vNO3z,gelPz,dalgbz,nkzs,dH2D,tempwz,cpfad,up_PBz,up_NBz,Qmx_PB,Qmn_PB                      &
                         ,upmxPB,Qmx_NB,Qmn_NB,upmxNB,Q_NB,Q_PB,IKbe,frmube,alamda,abltbr,ablbrz,up_N2z,ablz               &
                         ,chlabl,a1Bl,a2Bl,a3Bl,hchlbz,hCChlbz,algabz,algzbz,Dz2D,ToptB,kTemp_Bl,ifix,sedAlg_MQ            &
                         ,sedAlb0,hQ_NBz, mstr,itags,monats,isim_end,abmor_1,azStrs                                        &
     &                   ,.false.,0)     !!wy kontroll,iglob

      if(nbuhn(mstr)==0)goto 1414 
      if(ilbuhn==0)then 

      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwtief(ior) = tiefe(ior) 
        zwvm(ior) = vmitt(ior) 
        zwno3(ior) = vno3(ior) 
        zwnh4(ior) = vnh4(ior) 
        zwgelp(ior) = gelp(ior) 
        zwsvhb(ior) = svhemb(ior) 
        zwchla(ior) = chla(ior)
        zwchlb(ior) = chlabl(ior) 
        zwzooi(ior) = zooind(ior) 
        zwir(ior) = ir(ior) 
        zwssa(ior) = ssalg(ior) 
        zwsi(ior) = si(ior) 
        zwdalb(ior) = dalgbl(ior) 
        zwdaab(ior) = dalgab(ior) 
        zwsedb(ior) = sedalb(ior) 
        zwzob(ior) = algzob(ior) 
        zwadrb(ior) = algdrb(ior) 
        zwacob(ior) = algcob(ior) 
        zwbmor(ior) = dblmor(ior) 
        zwkigr(ior) = vkigr(ior) 
        zwantb(ior) = antbl(ior) 
        zwbbcm(ior) = abbcm(ior) 
        zwabl(ior) = abl(ior) 
        zwbmua(ior) = abmuea(ior) 
        zwfiba(ior) = fibaus(ior) 
        zwfhba(ior) = fhebas(ior) 
        zwbrau(ior) = abreau(ior) 
        zwtpbl(ior) = tpbl(ior) 
        zwextk(ior) = extk(ior) 
        zup_PB(ior) = up_PBz(1,ior) 
        zup_NB(ior) = up_NBz(1,ior) 
        zQ_PB(ior) = Q_PB(ior) 
        zQ_NB(ior) = Q_NB(ior) 
        zabtbr(ior) = ablbrz(1,ior) 
        zwabz(ior) = dalgbz(1,ior) 
        zwaabz(ior) = algabz(1,ior) 
        zwnkzs(ior) = nkzs(ior) 
        zwN4z(ior) = vNH4z(1,ior) 
        zwN3z(ior) = vNO3z(1,ior) 
        zwPz(ior) = gelPz(1,ior) 
        zwsedAlg_MQ(ior) = sedAlg_MQ(mstr,ior)
        zwabmor_1(ior) = abmor_1(mstr,ior) 
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior) 
        vno3(ior) = bno3(mstr,ior) 
        vnh4(ior) = bnh4(mstr,ior) 
        gelp(ior) = bgelp(mstr,ior) 
        svhemb(ior) = bsvheb(mstr,ior) 
        chla(ior) = bchla(mstr,ior)
        chlabl(ior) = bchlab(mstr,ior) 
        zooind(ior) = bzooi(mstr,ior) 
        ir(ior) = bir(mstr,ior) 
        ssalg(ior) = bssalg(mstr,ior) 
        si(ior) = bsi(mstr,ior) 
        vkigr(ior) = bvkigr(mstr,ior) 
        antbl(ior) = bantbl(mstr,ior) 
        abbcm(ior) = babbcm(mstr,ior) 
        extk(ior) = bextk(mstr,ior) 
        algzob(ior) = bazob(mstr,ior) 
        algdrb(ior) = badrb(mstr,ior) 
        algcob(ior) = bacob(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
        Q_PB(ior) = bQ_PB(mstr,ior) 
        Q_NB(ior) = bQ_NB(mstr,ior) 
        vNH4z(1,ior) = bnh4(mstr,ior) 
        vNO3z(1,ior) = bno3(mstr,ior) 
        gelPz(1,ior) = bgelP(mstr,ior) 
        sedAlg_MQ(mstr,ior) = bsedAlg_MQ(mstr,ior)
        abmor_1(mstr,ior) = babmor_1(mstr,ior) 
      enddo                                                                      

      ilbuhn = 1 
      goto 1413 
    endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        bsvheb(mstr,ior) = svhemb(ior) 
        bdabl(mstr,ior) = dalgbl(ior) 
        bdaab(mstr,ior) = dalgab(ior) 
        bsedab(mstr,ior) = sedalb(ior) 
        bdbmor(mstr,ior) = dblmor(ior) 
        babbcm(mstr,ior) = abbcm(ior) 
        babl(mstr,ior) = abl(ior)
        bchlab(mstr,ior) = chlabl(ior) 
        babmua(mstr,ior) = abmuea(ior) 
        bfibas(mstr,ior) = fibaus(ior) 
        bfhbau(mstr,ior) = fhebas(ior) 
        babrau(mstr,ior) = abreau(ior) 
        btpbl(mstr,ior) = tpbl(ior) 
        bQ_PB(mstr,ior) = Q_PB(ior) 
        bQ_NB(mstr,ior) = Q_NB(ior) 
        bup_PB(mstr,ior) = up_PBz(1,ior) 
        bup_NB(mstr,ior) = up_NBz(1,ior) 
        babtbr(mstr,ior) = ablbrz(1,ior) 
        balgbz(mstr,ior) = dalgbz(1,ior) 
        balabz(mstr,ior) = algabz(1,ior) 
        bsedAlg_MQ(mstr,ior) = sedAlg_MQ(mstr,ior)
        babmor_1(mstr,ior) = abmor_1(mstr,ior)
                                                                       
        tempw(ior) = zwtemp(ior) 
        tiefe(ior) = zwtief(ior) 
        vmitt(ior) = zwvm(ior) 
        vno3(ior) = zwno3(ior) 
        vnh4(ior) = zwnh4(ior) 
        gelp(ior) = zwgelp(ior) 
        svhemk(ior) = zwsvhk(ior) 
        chla(ior) = zwchla(ior)
        chlabl(ior) = zwchlb(ior)  
        zooind(ior) = zwzooi(ior) 
        ir(ior) = zwir(ior) 
        ssalg(ior) = zwssa(ior) 
        si(ior) = zwsi(ior) 
        dalgbl(ior) = zwdalb(ior) 
        dalgab(ior) = zwdaab(ior) 
        sedalb(ior) = zwsedb(ior) 
        algzob(ior) = zwzob(ior) 
        algdrb(ior) = zwadrb(ior) 
        algcob(ior) = zwacob(ior) 
        dblmor(ior) = zwbmor(ior) 
        vkigr(ior) = zwkigr(ior) 
        antbl(ior) = zwantb(ior) 
        abbcm(ior) = zwbbcm(ior) 
        abl(ior) = zwabl(ior) 
        abmuea(ior) = zwbmua(ior) 
        fibaus(ior) = zwfiba(ior) 
        fhebas(ior) = zwfhba(ior) 
        abreau(ior) = zwbrau(ior) 
        tpbl(ior) = zwtpbl(ior) 
        extk(ior) = zwextk(ior) 
        up_PBz(1,ior) = zup_PB(ior) 
        up_NBz(1,ior) = zup_NB(ior) 
        Q_PB(ior) = zQ_PB(ior) 
        Q_NB(ior) = zQ_NB(ior) 
        ablbrz(1,ior) = zabtbr(ior) 
        dalgbz(1,ior) = zwabz(ior) 
        algabz(1,ior) = zwaabz(ior) 
                                                                       
        nkzs(ior) = zwnkzs(ior) 
        vNH4z(1,ior) = zwN4z(ior) 
        vNO3z(1,ior) = zwN3z(ior) 
        gelPz(1,ior) = zwPz(ior) 
        sedAlg_MQ(mstr,ior) = zwsedAlg_MQ(ior)
        abmor_1(mstr,ior) = zwabmor_1(ior)

        diff1 = bsvheb(mstr,ior)-svhemb(ior)
     
        bdiff1 = svhemb(ior)-bsvheb(mstr,ior)
                                                                       
      if(bleb(mstr,ior)>0.0)then
        svhemb(ior) = svhemb(ior)+diff1*(1.-exp(-hctau1(ior))) 
        if(svhemb(ior)<0.0)svhemb(ior) = 0.0 
      endif
                                                                       
        if(hctau2(ior)>0.0)then 
          bsvheb(mstr,ior) = bsvheb(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
        if(bsvheb(mstr,ior)<0.0)bsvheb(mstr,ior) = 0.0 
      endif
    enddo
                                                                       
      ilbuhn = 0 
      endif 
                                                                       
!                                                                       
!**********************Gruenalgen******************                     
                                                                       
 1414 continue 

      call       algaesgr(SCHWI,TFLIE,TEMPW,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemg,CHLA,SSALG,dalggr,dalgag              &
                ,flag,elen,ior,anze,sedalg,algzog,dgrmor,fkm,vkigr,chlaki,chlagr,vabfl,qeinl,jiein,evkigr,eantbl        &
                ,agchl,aggmax,agksn,agksp,agremi,vco2,algdrg,pbiogr,Q_PK,Q_NK,iph,akbcm,agbcm,aki,agr,cmatgr            &
                ,cmatki,abbcm,antbl,abl,pbiobl,chlabl,extk,extk_lamda                                                   &
     &          ,ilamda,eta,aw,ack,acg,acb,ah,as,al                                                                     & !!wy Licht-Extinktionsparameter
                ,tpgr,uhrz,iwied,algcog                                                                                 &
                ,figaus,agmuea,fhegas,agreau,tauscs,ischif,ilbuhn,ieros,asgre,echla,ess,ss,zooind,GRote,Q_PG,Q_NG       &
                ,vNH4z,vNO3z,gelPz,dalggz,nkzs,dH2D,tempwz,cpfad,itags,monats,mstr,up_PGz,up_NGz,Qmx_PG                 &
                ,Qmn_PG,upmxPG,Qmx_NG,Qmn_NG,upmxNG,IKge,frmuge,alamda,agrtbr,agrbrz,akiz,agrz,ablz                     &
                ,chlaz,hchlkz,hchlgz,hchlbz,hCChlgz,algagz,algzgz,Dz2D,ToptG,kTemp_Gr,ifix,sedAlg_MQ,sedAlg0, hQ_NGz    &
                ,a1Gr,a2Gr,a3Gr,ifehl,ifhstr,isim_end,agmor_1,azStrs                                                    &
     &          ,.false.,0)     !!wy kontroll,iglob

      
       if(ifehl>0)goto 989                  

      if(nbuhn(mstr)==0)goto 1513 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwtief(ior) = tiefe(ior) 
        zwvm(ior) = vmitt(ior) 
        zwno3(ior) = vno3(ior) 
        zwnh4(ior) = vnh4(ior) 
        zwgelp(ior) = gelp(ior) 
        zwsvhg(ior) = svhemg(ior) 
        zwchla(ior) = chla(ior) 
        zwzooi(ior) = zooind(ior) 
        zwchlk(ior) = chlaki(ior) 
        zwchlg(ior) = chlagr(ior) 
        zwchlb(ior) = chlabl(ior) 
        zwir(ior) = ir(ior) 
        zwssa(ior) = ssalg(ior) 
        zwdalg(ior) = dalggr(ior) 
        zwdaag(ior) = dalgag(ior) 
        zwsedg(ior) = sedalg(ior) 
        zwzog(ior) = algzog(ior) 
        zwgmor(ior) = dgrmor(ior) 
        zwkigr(ior) = vkigr(ior) 
        zwantb(ior) = antbl(ior) 
        zwkbcm(ior) = akbcm(ior) 
        zwgbcm(ior) = agbcm(ior) 
        zwbbcm(ior) = abbcm(ior) 
        zwaki(ior) = aki(ior) 
        zwagr(ior) = agr(ior) 
        zwabl(ior) = abl(ior) 
        zwgmua(ior) = agmuea(ior) 
        zwfiga(ior) = figaus(ior) 
        zwfhga(ior) = fhegas(ior) 
        zwgrau(ior) = agreau(ior) 
        zwadrg(ior) = algdrg(ior) 
        zwacog(ior) = algcog(ior) 
        zwtpgr(ior) = tpgr(ior) 
        zwextk(ior) = extk(ior) 
        zup_PG(ior) = up_PGz(1,ior) 
        zup_NG(ior) = up_NGz(1,ior) 
        zQ_PG(ior) = Q_PG(ior) 
        zQ_NG(ior) = Q_NG(ior) 
        zagtbr(ior) = agrbrz(1,ior) 
        zwagz(ior) = dalggz(1,ior) 
        zwaagz(ior) = algagz(1,ior) 
        zwnkzs(ior) = nkzs(ior) 
        zwN4z(ior) = vNH4z(1,ior) 
        zwN3z(ior) = vNO3z(1,ior) 
        zwPz(ior) = gelPz(1,ior) 
        zwsedAlg_MQ(ior) = sedAlg_MQ(mstr,ior)
        zwagmor_1(ior) = agmor_1(mstr,ior)
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior) 
        vno3(ior) = bno3(mstr,ior) 
        vnh4(ior) = bnh4(mstr,ior) 
        gelp(ior) = bgelp(mstr,ior) 
        svhemg(ior) = bsvheg(mstr,ior) 
        chla(ior) = bchla(mstr,ior)
        chlaki(ior) = bchlak(mstr,ior)  
        chlabl(ior) = bchlab(mstr,ior)  
        chlagr(ior) = bchlag(mstr,ior)  
        zooind(ior) = bzooi(mstr,ior) 
        ir(ior) = bir(mstr,ior) 
        ssalg(ior) = bssalg(mstr,ior) 
        dalggr(ior) = bdagr(mstr,ior) 
        dalgag(ior) = bdaag(mstr,ior) 
        algzog(ior) = bazog(mstr,ior) 
        dgrmor(ior) = bdgmor(mstr,ior) 
        vkigr(ior) = bvkigr(mstr,ior) 
        antbl(ior) = bantbl(mstr,ior) 
        akbcm(ior) = bakbcm(mstr,ior) 
        agbcm(ior) = bagbcm(mstr,ior) 
        abbcm(ior) = babbcm(mstr,ior) 
        aki(ior) = baki(mstr,ior) 
        agr(ior) = bagr(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
        algdrg(ior) = badrg(mstr,ior) 
        algcog(ior) = bacog(mstr,ior) 
        extk(ior) = bextk(mstr,ior) 
        Q_PG(ior) = bQ_PG(mstr,ior) 
        Q_NG(ior) = bQ_NG(mstr,ior) 
        vNH4z(1,ior) = bnh4(mstr,ior) 
        vNO3z(1,ior) = bno3(mstr,ior) 
        gelPz(1,ior) = bgelp(mstr,ior) 
        sedAlg_MQ(mstr,ior) = bsedAlg_MQ(mstr,ior)
        agmor_1(mstr,ior) = bagmor_1(mstr,ior)
      enddo                                                                 

      ilbuhn = 1 
      goto 1414 
     endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        bsvheg(mstr,ior) = svhemg(ior) 
        bchla(mstr,ior) = chla(ior) 
        bchlak(mstr,ior) = chlaki(ior) 
        bchlag(mstr,ior) = chlagr(ior) 
        bchlab(mstr,ior) = chlabl(ior) 
        bdagr(mstr,ior) = dalggr(ior) 
        bdaag(mstr,ior) = dalgag(ior)
        bsedag(mstr,ior) = sedalg(ior) 
        bdgmor(mstr,ior) = dgrmor(ior) 
        bvkigr(mstr,ior) = vkigr(ior) 
        bantbl(mstr,ior) = antbl(ior) 
        bagbcm(mstr,ior) = agbcm(ior) 
        bagr(mstr,ior) = agr(ior) 
        bagmua(mstr,ior) = agmuea(ior) 
        bfigas(mstr,ior) = figaus(ior) 
        bfhgau(mstr,ior) = fhegas(ior) 
        bagrau(mstr,ior) = agreau(ior) 
        btpgr(mstr,ior) = tpgr(ior) 
        bextk(mstr,ior) = extk(ior) 
        bQ_PG(mstr,ior) = Q_PG(ior) 
        bQ_NG(mstr,ior) = Q_NG(ior) 
        bup_PG(mstr,ior) = up_PGz(1,ior) 
        bup_NG(mstr,ior) = up_NGz(1,ior) 
        bagtbr(mstr,ior) = agrbrz(1,ior) 
        balggz(mstr,ior) = dalggz(1,ior) 
        balagz(mstr,ior) = algagz(1,ior) 
        bsedAlg_MQ(mstr,ior) = sedAlg_MQ(mstr,ior)
        bagmor_1(mstr,ior) = agmor_1(mstr,ior)
                                                                       
        tempw(ior) = zwtemp(ior) 
        tiefe(ior) = zwtief(ior) 
        vmitt(ior) = zwvm(ior) 
        vno3(ior) = zwno3(ior) 
        vnh4(ior) = zwnh4(ior) 
        gelp(ior) = zwgelp(ior) 
        svhemg(ior) = zwsvhg(ior) 
        chla(ior) = zwchla(ior) 
        zooind(ior) = zwzooi(ior) 
        chlaki(ior) = zwchlk(ior) 
        chlagr(ior) = zwchlg(ior) 
        chlabl(ior) = zwchlb(ior) 
        ir(ior) = zwir(ior) 
        ssalg(ior) = zwssa(ior) 
        dalggr(ior) = zwdalg(ior) 
        dalgag(ior) = zwdaag(ior) 
        sedalg(ior) = zwsedg(ior) 
        algzog(ior) = zwzog(ior) 
        dgrmor(ior) = zwgmor(ior) 
        vkigr(ior) = zwkigr(ior) 
        antbl(ior) = zwantb(ior) 
        akbcm(ior) = zwkbcm(ior) 
        agbcm(ior) = zwgbcm(ior) 
        abbcm(ior) = zwbbcm(ior) 
        aki(ior) = zwaki(ior) 
        agr(ior) = zwagr(ior) 
        abl(ior) = zwabl(ior) 
        agmuea(ior) = zwgmua(ior) 
        figaus(ior) = zwfiga(ior) 
        fhegas(ior) = zwfhga(ior) 
        agreau(ior) = zwgrau(ior) 
        algdrg(ior) = zwadrg(ior) 
        algcog(ior) = zwacog(ior) 
        tpgr(ior) = zwtpgr(ior) 
        extk(ior) = zwextk(ior) 
        up_PGz(1,ior) = zup_PG(ior) 
        up_NGz(1,ior) = zup_NG(ior) 
        Q_PG(ior) = zQ_PG(ior) 
        Q_NG(ior) = zQ_NG(ior) 
        agrbrz(1,ior) = zagtbr(ior) 
        sedAlg_MQ(mstr,ior) = zwsedAlg_MQ(ior)
        agmor_1(mstr,ior) = zwagmor_1(ior) 
                                                                       
        nkzs(ior) = zwnkzs(ior) 
                                                                       
        vNH4z(1,ior) = zwN4z(ior) 
        vNO3z(1,ior) = zwN3z(ior) 
        gelPz(1,ior) = zwPz(ior) 

        diff1 = bsvheg(mstr,ior)-svhemg(ior)
        diff2 = bchlak(mstr,ior)-chlaki(ior)
        diff3 = bchlag(mstr,ior)-chlagr(ior)
        diff4 = bchlab(mstr,ior)-chlabl(ior)
        diff5 = bchla(mstr,ior)-chla(ior)
        diff6 = baki(mstr,ior)-aki(ior)
        diff7 = bagr(mstr,ior)-agr(ior)
        diff8 = babl(mstr,ior)-abl(ior)
        diff9 = bakbcm(mstr,ior)-akbcm(ior)
        diff10 = bagbcm(mstr,ior)-agbcm(ior)
        diff11 = babbcm(mstr,ior)-abbcm(ior)
        diff12 = bakmor_1(mstr,ior)-akmor_1(mstr,ior)
        diff13 = babmor_1(mstr,ior)-abmor_1(mstr,ior)
        diff14 = bagmor_1(mstr,ior)-agmor_1(mstr,ior)

        bdiff1 = svhemg(ior)-bsvheg(mstr,ior)
        bdiff2 = chlaki(ior)-bchlak(mstr,ior)
        bdiff3 = chlagr(ior)-bchlag(mstr,ior)
        bdiff4 = chlabl(ior)-bchlab(mstr,ior)  
        bdiff5 = chla(ior)-bchla(mstr,ior)  
        bdiff6 = aki(ior)-baki(mstr,ior)   
        bdiff7 = agr(ior)-bagr(mstr,ior)   
        bdiff8 = abl(ior)-babl(mstr,ior)
        bdiff9 = akbcm(ior)-bakbcm(mstr,ior)  
        bdiff10 = agbcm(ior)-bagbcm(mstr,ior)  
        bdiff11 = abbcm(ior)-babbcm(mstr,ior)
        bdiff12 = akmor_1(mstr,ior)-bakmor_1(mstr,ior)  
        bdiff13 = abmor_1(mstr,ior)-babmor_1(mstr,ior)  
        bdiff14 = agmor_1(mstr,ior)-bagmor_1(mstr,ior)  

        if(bleb(mstr,ior)>0.0)then
        svhemg(ior) = svhemg(ior)+diff1*(1.-exp(-hctau1(ior))) 
        chlaki(ior) = chlaki(ior)+diff2*(1.-exp(-hctau1(ior))) 
        chlagr(ior) = chlagr(ior)+diff3*(1.-exp(-hctau1(ior))) 
        chlabl(ior) = chlabl(ior)+diff4*(1.-exp(-hctau1(ior))) 
        chla(ior) = chla(ior)+diff5*(1.-exp(-hctau1(ior))) 
        aki(ior) = aki(ior)+diff6*(1.-exp(-hctau1(ior))) 
        agr(ior) = agr(ior)+diff7*(1.-exp(-hctau1(ior))) 
        abl(ior) = abl(ior)+diff8*(1.-exp(-hctau1(ior))) 
        akbcm(ior) = akbcm(ior)+diff9*(1.-exp(-hctau1(ior))) 
        agbcm(ior) = agbcm(ior)+diff10*(1.-exp(-hctau1(ior))) 
        abbcm(ior) = abbcm(ior)+diff11*(1.-exp(-hctau1(ior))) 
        akmor_1(mstr,ior) = akmor_1(mstr,ior)+diff12*(1.-exp(-hctau1(ior))) 
        abmor_1(mstr,ior) = abmor_1(mstr,ior)+diff13*(1.-exp(-hctau1(ior))) 
        agmor_1(mstr,ior) = agmor_1(mstr,ior)+diff14*(1.-exp(-hctau1(ior))) 

        if(svhemg(ior)<0.0)svhemg(ior) = 0.0 

        vkigr(ior) = chlaki(ior)/(Chlaki(ior)+chlagr(ior)+chlabl(ior)) 
        antbl(ior) = chlabl(ior)/(Chlaki(ior)+chlagr(ior)+chlabl(ior))!

       endif
                                                              
        if(hctau2(ior)>0.0)then 
          bsvheg(mstr,ior) = bsvheg(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bchlak(mstr,ior) = bchlak(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
          bchlag(mstr,ior) = bchlag(mstr,ior)+bdiff3*(1.-exp(-hctau2(ior))) 
          bchlab(mstr,ior) = bchlab(mstr,ior)+bdiff4*(1.-exp(-hctau2(ior))) 
          bchla(mstr,ior) = bchla(mstr,ior)+bdiff5*(1.-exp(-hctau2(ior))) 
          baki(mstr,ior) = baki(mstr,ior)+bdiff6*(1.-exp(-hctau2(ior))) 
          bagr(mstr,ior) = bagr(mstr,ior)+bdiff7*(1.-exp(-hctau2(ior))) 
          babl(mstr,ior) = babl(mstr,ior)+bdiff8*(1.-exp(-hctau2(ior))) 
          bakbcm(mstr,ior) = bakbcm(mstr,ior)+bdiff9*(1.-exp(-hctau2(ior))) 
          bagbcm(mstr,ior) = bagbcm(mstr,ior)+bdiff10*(1.-exp(-hctau2(ior))) 
          babbcm(mstr,ior) = babbcm(mstr,ior)+bdiff11*(1.-exp(-hctau2(ior))) 
          bakmor_1(mstr,ior) = bakmor_1(mstr,ior)+bdiff12*(1.-exp(-hctau2(ior))) 
          babmor_1(mstr,ior) = babmor_1(mstr,ior)+bdiff13*(1.-exp(-hctau2(ior))) 
          bagmor_1(mstr,ior) = bagmor_1(mstr,ior)+bdiff14*(1.-exp(-hctau2(ior))) 

          if(bsvheg(mstr,ior)<0.0)bsvheg(mstr,ior) = 0.0 
          bvkigr(mstr,ior) = bchlak(mstr,ior)/(bChlak(mstr,ior)+bchlag(mstr,ior)+bchlab(mstr,ior))             
          bantbl(mstr,ior) = bchlab(mstr,ior)/(bChlak(mstr,ior)+bchlag(mstr,ior)+bchlab(mstr,ior))             
        endif                                                               
      enddo 

      ilbuhn = 0 
    endif 

!**********benthische Algen**********************                       
!                                                                       
 1513 continue
      call albenth(SCHWI,TFLIE,TEMPW,TIEFE,VMITT,VNO3,VNH4,GELP         &
     &,albewg,alberg,elen,flae,ior,anze,aggmax,agksn,agksp              &
     &,si,akksn,akksp,akkssi,akgmax,albewk,alberk,abegm2,abekm2         &
     &,vabfl,cmatgr,cmatki,akchl,agchl,extk,ilang,mstr                  &
     &,.false.,0)     !!wy kontroll,iglob
                                                                       
!***********************Makrophythen*****************                   
!                                                                       
      call mphyt(tiefe,tempw,anze,po2p,po2r,pfldalg,tflie               &
     &,itags,monats,itstart,mstart,itmax,mmax,itend,mend,schwi          &
     &,pflmin,pflmax,pfl,sa,su,ilang,extk,mstr,ifehl,ifhStr             &
     &,.false.,0)     !!wy kontroll,iglob
      if(ifehl.gt.0)goto 989 
!                                                                       
!*************orgC********************************                      
                                                                       
      if(nbuhn(mstr)>0)then
        do ior=1,anze+1
          bpfl(mstr,ior) = pfl(ior)
          pfl(ior) = 0.0
        enddo 
      endif   
                                                                       
 1530 continue 
      if(vbsb(1).lt.0.0.and.vbsb(1).lt.0.0)goto 1514
      call orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,flae,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze              &                   
                ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor,jiein,bsbgr,bsbki,akbcm             &
                ,agbcm,pfl,ezind,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb,csbki,csbgr,ischif,echla               &
                ,evkigr,eantbl,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd,orgCsd0,orgCsd_abb,CD,CP,CM,BAC,eCD       &
                ,eCP,eCM,eBAC,TOC_CSB,GRote,vcsb,vkigr,antbl,HNFBAC,BSBHNF,CHNF,zBAC                              &
                ,BVHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct,qeinlL        &
                ,iorLa,iorLe,ieinLs,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo,etemp,bsbctP          &
                ,doN,hsdFluB,hyPe,hymxDe,KsD1e,KsD2e,KsMe,upBACe,JDOC1,JDOC2,YBACe,rsGBACe                &
                ,nkzs,mstr,itags,monats,uhrz,azStrs,bsbZoo                                                &
                ,.false.,0)     !!wy kontroll,iglob
      
      if(nbuhn(mstr)==0)goto 1514 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwtief(ior) = tiefe(ior) 
        zwvm(ior) = vmitt(ior) 
        zwobsb(ior) = obsb(ior) 
        zwocsb(ior) = ocsb(ior) 
        zwvbsb(ior) = vbsb(ior) 
        zwvcsb(ior) = vcsb(ior) 
        zwbsbt(ior) = bsbt(ior) 
        zwbsct(ior) = bsbct(ior) 
        zwbsP(ior) = bsbctP(ior) 
        zwbsN(ior) = doN(ior) 
        zwzooi(ior) = zooind(ior) 
        zwabsz(ior) = abszo(ior) 
        zwsbsb(ior) = sdbsb(ior) 
        zwzexk(ior) = zexki(ior) 
        zwzexg(ior) = zexgr(ior) 
        zwzexb(ior) = zexbl(ior) 
        zwbsbe(ior) = bsbbet(ior) 
        zwkmor(ior) = dkimor(ior) 
        zwgmor(ior) = dgrmor(ior) 
        zwbmor(ior) = dblmor(ior) 
        zwkbcm(ior) = akbcm(ior) 
        zwgbcm(ior) = agbcm(ior) 
        zwbbcm(ior) = abbcm(ior) 
        zwkigr(ior) = vkigr(ior) 
        zwantb(ior) = antbl(ior) 
        zwaki(ior) = aki(ior) 
        zwagr(ior) = agr(ior) 
        zwabl(ior) = abl(ior) 
        zwdfak(ior) = drfaek(ior) 
        zwdfag(ior) = drfaeg(ior) 
        zwdfab(ior) = drfaeb(ior) 
        zwdfas(ior) = drfaes(ior) 
        zwssdr(ior) = ssdr(ior) 
        zwCsed(ior) = orgCsd(mstr,ior) 
        zwCsed_abb(ior) = orgCsd_abb(mstr,ior) 
        zwcd(1,ior) = CD(1,ior) 
        zwcd(2,ior) = CD(2,ior) 
        zwcp(1,ior) = CP(1,ior) 
        zwcp(2,ior) = CP(2,ior) 
        zwcm(ior) = CM(ior) 
        zwBAC(ior) = BAC(ior) 
        zwHNFB(ior) = HNFBAC(ior) 
        zwBSBH(ior) = BSBHNF(ior) 
        zwHNF(ior) = CHNF(ior) 
        zwfbgr(ior) = fbsgr(ior) 
        zwfrgr(ior) = frfgr(ior) 
        zworgS(ior) = dorgSS(ior) 
        zwnkzs(ior) = nkzs(ior) 
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior) 
        obsb(ior) = bbsb(mstr,ior) 
        ocsb(ior) = bcsb(mstr,ior) 
        vbsb(ior) = bvbsb(mstr,ior) 
        vcsb(ior) = bvcsb(mstr,ior) 
        zooind(ior) = bzooi(mstr,ior) 
        abszo(ior) = babszo(mstr,ior) 
        sdbsb(ior) = bsdbsb(mstr,ior) 
        zexki(ior) = bzexki(mstr,ior) 
        zexgr(ior) = bzexgr(mstr,ior) 
        zexbl(ior) = bzexbl(mstr,ior) 
        dkimor(ior) = bdkmor(mstr,ior) 
        dgrmor(ior) = bdgmor(mstr,ior) 
        dblmor(ior) = bdbmor(mstr,ior) 
        akbcm(ior) = bakbcm(mstr,ior) 
        agbcm(ior) = bagbcm(mstr,ior) 
        abbcm(ior) = babbcm(mstr,ior) 
        vkigr(ior) = bvkigr(mstr,ior) 
        antbl(ior) = bantbl(mstr,ior) 
        pfl(ior) = bpfl(mstr,ior)
        aki(ior) = baki(mstr,ior) 
        agr(ior) = bagr(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
        drfaek(ior) = bdfaek(mstr,ior) 
        drfaeg(ior) = bdfaeg(mstr,ior) 
        drfaeg(ior) = bdfaeb(mstr,ior) 
        drfaes(ior) = bdfaes(mstr,ior) 
        ssdr(ior) = bssdr(mstr,ior) 
        CD(1,ior) = bCD(mstr,1,ior) 
        CD(2,ior) = bCD(mstr,2,ior) 
        CP(1,ior) = bCP(mstr,1,ior) 
        CP(2,ior) = bCP(mstr,2,ior) 
        CM(ior) = bCM(mstr,ior) 
        BAC(ior) = bBAC(mstr,ior) 
        HNFBAC(ior) = bHNFBS(mstr,ior) 
        BSBHNF(ior) = bBSBHN(mstr,ior) 
        CHNF(ior) = bCHNF(mstr,ior) 
        fbsgr(ior) = bfbsgr(mstr,ior) 
        frfgr(ior) = bfrfgr(mstr,ior) 
        JDOC1(ior) = bJDOC1(ior)
        JDOC2(ior) = bJDOC2(ior)
      enddo
                                                                       
      ilbuhn = 1 
      goto 1530 
    endif 
                                                                       
    if(ilbuhn==1)then 
      do ior=1,anze+1 
        bbsb(mstr,ior) = obsb(ior) 
        bcsb(mstr,ior) = ocsb(ior) 
        bvbsb(mstr,ior) = vbsb(ior) 
        bvcsb(mstr,ior) = vcsb(ior) 
        bbsbt(mstr,ior) = bsbt(ior) 
        bbsbct(mstr,ior) = bsbct(ior) 
        bbsbcP(mstr,ior) = bsbctP(ior) 
        bdoN(mstr,ior) = doN(ior) 
        bsdbsb(mstr,ior) = sdbsb(ior) 
        bbsbbe(mstr,ior) = bsbbet(ior) 
        borgCs(mstr,ior) = orgCsd(mstr,ior) 
        borgCs_abb(mstr,ior) = orgCsd_abb(mstr,ior) 
        bcd(mstr,1,ior) = CD(1,ior) 
        bcd(mstr,2,ior) = CD(2,ior) 
        bcp(mstr,1,ior) = CP(1,ior) 
        bcp(mstr,2,ior) = CP(2,ior) 
        bcm(mstr,ior) = CM(ior) 
        bBAC(mstr,ior) = BAC(ior) 
        bCHNF(mstr,ior) = CHNF(ior) 
        bfbsgr(mstr,ior) = fbsgr(ior) 
        bfrfgr(mstr,ior) = frfgr(ior) 
        borgSS(mstr,ior) = dorgSS(ior) 
                                                                       
        tempw(ior) = zwtemp(ior) 
        tiefe(ior) = zwtief(ior) 
        vmitt(ior) = zwvm(ior) 
        obsb(ior) = zwobsb(ior) 
        ocsb(ior) = zwocsb(ior) 
        vbsb(ior) = zwvbsb(ior) 
        vcsb(ior) = zwvcsb(ior) 
        bsbt(ior) = zwbsbt(ior) 
        bsbct(ior) = zwbsct(ior) 
        bsbctP(ior) = zwbsP(ior) 
        doN(ior) = zwbsN(ior) 
        zooind(ior) = zwzooi(ior) 
        abszo(ior) = zwabsz(ior) 
        sdbsb(ior) = zwsbsb(ior) 
        zexki(ior) = zwzexk(ior) 
        zexgr(ior) = zwzexg(ior) 
        zexbl(ior) = zwzexb(ior) 
        bsbbet(ior) = zwbsbe(ior) 
        dkimor(ior) = zwkmor(ior) 
        dgrmor(ior) = zwgmor(ior) 
        dblmor(ior) = zwbmor(ior) 
        akbcm(ior) = zwkbcm(ior) 
        agbcm(ior) = zwgbcm(ior) 
        abbcm(ior) = zwbbcm(ior) 
        vkigr(ior) = zwkigr(ior) 
        antbl(ior) = zwantb(ior) 
        aki(ior) = zwaki(ior) 
        agr(ior) = zwagr(ior) 
        abl(ior) = zwabl(ior) 
        drfaek(ior) = zwdfak(ior) 
        drfaeg(ior) = zwdfag(ior) 
        drfaeb(ior) = zwdfab(ior) 
        drfaes(ior) = zwdfas(ior) 
        ssdr(ior) = zwssdr(ior) 
        orgCsd(mstr,ior) = zwCsed(ior) 
        orgCsd_abb(mstr,ior) = zwCsed_abb(ior) 
        CD(1,ior) = zwcd(1,ior) 
        CD(2,ior) = zwcd(2,ior) 
        CP(1,ior) = zwcp(1,ior) 
        CP(2,ior) = zwcp(2,ior) 
        CM(ior) = zwcm(ior) 
        BAC(ior) = zwBAC(ior) 
        HNFBAC(ior) = zwHNFB(ior) 
        BSBHNF(ior) = zwBSBH(ior) 
        CHNF(ior) = zwHNF(ior) 
        fbsgr(ior) = zwfbgr(ior) 
        frfgr(ior) = zwfrgr(ior) 
        dorgSS(ior) = zworgS(ior) 
        nkzs(ior) = zwnkzs(ior) 

        diff1 = bbsb(mstr,ior)-obsb(ior)
        diff2 = bcsb(mstr,ior)-ocsb(ior)
        diff3 = bvbsb(mstr,ior)-vbsb(ior)
        diff4 = bvcsb(mstr,ior)-vcsb(ior)
        diff5 = bcd(mstr,1,ior)-CD(1,ior)
        diff6 = bcd(mstr,2,ior)-CD(2,ior)
        diff7 = bcp(mstr,1,ior)-CP(1,ior)
        diff8 = bcp(mstr,2,ior)-CP(2,ior)
        diff9 = bcm(mstr,ior)-CM(ior)
        diff10 = bBAC(mstr,ior)-BAC(ior)
        diff11 = bfbsgr(mstr,ior)-fbsgr(ior)
        diff12 = bfrfgr(mstr,ior)-frfgr(ior) 
   
        bdiff1 = obsb(ior)-bbsb(mstr,ior)
        bdiff2 = ocsb(ior)-bcsb(mstr,ior)
        bdiff3 = vbsb(ior)-bvbsb(mstr,ior)
        bdiff4 = vcsb(ior)-bvcsb(mstr,ior)
        bdiff5 = CD(1,ior)-bcd(mstr,1,ior)
        bdiff6 = CD(2,ior)-bcd(mstr,2,ior)
        bdiff7 = CP(1,ior)-bcp(mstr,1,ior)
        bdiff8 = CP(2,ior)-bcp(mstr,2,ior)
        bdiff9 = CM(ior)-bcm(mstr,ior)
        bdiff10 = BAC(ior)-bBAC(mstr,ior)
        bdiff11 = fbsgr(ior)-bfbsgr(mstr,ior)
        bdiff12 = frfgr(ior)-bfrfgr(mstr,ior)   
                                                                       
      if(bleb(mstr,ior)>0.0)then
        obsb(ior) = obsb(ior)+diff1*(1.-exp(-hctau1(ior))) 
        ocsb(ior) = ocsb(ior)+diff2*(1.-exp(-hctau1(ior))) 
        vbsb(ior) = vbsb(ior)+diff3*(1.-exp(-hctau1(ior))) 
        vcsb(ior) = vcsb(ior)+diff4*(1.-exp(-hctau1(ior))) 
        CD(1,ior) = CD(1,ior)+diff5*(1.-exp(-hctau1(ior))) 
        CD(2,ior) = CD(2,ior)+diff6*(1.-exp(-hctau1(ior))) 
        CP(1,ior) = CP(1,ior)+diff7*(1.-exp(-hctau1(ior))) 
        CP(2,ior) = CP(2,ior)+diff8*(1.-exp(-hctau1(ior))) 
        CM(ior) = CM(ior)+diff9*(1.-exp(-hctau1(ior))) 
        BAC(ior) = BAC(ior)+diff10*(1.-exp(-hctau1(ior))) 
        fbsgr(ior) = fbsgr(ior)+diff11*(1.-exp(-hctau1(ior))) 
        frfgr(ior) = frfgr(ior)+diff12*(1.-exp(-hctau1(ior))) 
      endif 
                                                                       
        if(hctau2(ior)>0.0)then 
          bbsb(mstr,ior) = bbsb(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bcsb(mstr,ior) = bcsb(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
          bvbsb(mstr,ior) = bvbsb(mstr,ior)+bdiff3*(1.-exp(-hctau2(ior))) 
          bvcsb(mstr,ior) = bvcsb(mstr,ior)+bdiff4*(1.-exp(-hctau2(ior))) 
          bCD(mstr,1,ior) = bCD(mstr,1,ior)+bdiff5*(1.-exp(-hctau2(ior))) 
          bCD(mstr,2,ior) = bCD(mstr,2,ior)+bdiff6*(1.-exp(-hctau2(ior))) 
          bCP(mstr,1,ior) = bCP(mstr,1,ior)+bdiff7*(1.-exp(-hctau2(ior))) 
          bCP(mstr,2,ior) = bCP(mstr,2,ior)+bdiff8*(1.-exp(-hctau2(ior))) 
          bCM(mstr,ior) = bCM(mstr,ior)+bdiff9*(1.-exp(-hctau2(ior))) 
          bBAC(mstr,ior) = bBAC(mstr,ior)+bdiff10*(1.-exp(-hctau2(ior))) 
          bfbsgr(mstr,ior) = bfbsgr(mstr,ior)+bdiff11*(1.-exp(-hctau2(ior))) 
          bfrfgr(mstr,ior) = bfrfgr(mstr,ior)+bdiff12*(1.-exp(-hctau2(ior))) 
        endif
      enddo                                                                       
                                                                       
      ilbuhn = 0 
      endif 
                                                                       
!************Stickstoff****************                                 
                                                                       
 1514 continue

                                                                           
      if(vnh4(1).lt.0.0)goto 1515 

      if(nbuhn(mstr)>0.and.ilbuhn==0)then
        do ior=1,anze+1
          babewg(mstr,ior) = albewg(ior) 
          baberg(mstr,ior) = alberg(ior) 
          babewk(mstr,ior) = albewk(ior) 
          baberk(mstr,ior) = alberk(ior) 

          pfl(ior) = 0.0
          albewg(ior) = 0.0
          alberg(ior) = 0.0
          albewk(ior) = 0.0 
          alberk(ior) = 0.0
        enddo
      endif

      call ncyc(tempw,vx0,vnh4,tflie,rau,tiefe,vmitt,rhyd,vo2           &
     &,go2n,vno3,dC_DenW,flag,elen,ior,anze                             &
     &,enh4,eno3,ex0,qeinl,vabfl,pfl,sgo2n,sedx0,don                    &
     &,susn,bettn,susno,agrnh4,akinh4,dzres1,dzres2                     &
     &,agrno3,akino3,jiein,ischif                                       &
     &,ynmx1e,stks1e,anitrie,bnmx1e,bnks1e,vph,vno2,ij                  &
     &,albewg,alberg,albewk,alberk,resdr,aki,agr                        &
     &,exdrvk,exdrvg,vx02,ex02,eno2,ynmx2e,stks2e,anitri2e              &
     &,abl,ablnh4,ablno3,exdrvb                                         &
     &,bnmx2e,bnks2e,nl0,zooind,GRote,nzoo,gesN,orgCsd                  &
     &,egesN,sedalk,sedalb,sedalg,ilbuhn,iwied,fkm                      &
     &,CD,CP,CM,BAC,bsbct,nkzs,vnh4z,vno2z,vno3z,dH2D                   &
     &,hJNO3,hJNH4,hJN2,susO2N,hFluN3,akksN,agksN,abksN                 &
     &,Qmx_NK,Q_NK,up_NKz,Qmx_NG,Q_NG,up_NGz,Qmx_NB,Q_NB,up_NBz         &
     &,dalgkz,dalgbz,dalggz,agnh4z,aknh4z,abnh4z,agno3z,akno3z          &
     &,abno3z,vo2z,abltbr,akitbr,agrtbr,agrbrz,akibrz,ablbrz            &
     &,mstr,uhrz,itags,monats,enl0,algakz,algagz,algabz                 &
     &,up_N2z,iorLa,iorLe,ieinLs,flae,qeinlL,eNH4L                      &
     &,eNO2L,eNO3L,gesNL,hgesNz,algdrk,algdrg,algdrb,ifehl              &
     &,ifhstr,azStrs                                                    &
     &,.false.,0)     !!wy kontroll,iglob
      if(ifehl>0)goto 989 

      if(nbuhn(mstr)==0)goto 1515 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwtief(ior) = tiefe(ior) 
        zwvm(ior) = vmitt(ior) 
        zwno3(ior) = vno3(ior) 
        zwnh4(ior) = vnh4(ior) 
        zwno2(ior) = vno2(ior) 
        zwgesN(ior) = gesN(ior) 
        zwn4z(ior) = vnh4z(1,ior) 
        zwn2z(ior) = vno2z(1,ior) 
        zwn3z(ior) = vno3z(1,ior) 
        zwx0(ior) = vx0(ior) 
        zwx02(ior) = vx02(ior) 
        zwvo2(ior) = vo2(ior) 
        zwgo2n(ior) = go2n(ior) 
        zwbsbt(ior) = bsbt(ior) 
        zwbsct(ior) = bsbct(ior) 
        zwbsN(ior) = doN(ior) 
        zwsedk(ior) = sedalk(ior) 
        zwsedg(ior) = sedalg(ior) 
        zwsedb(ior) = sedalb(ior) 
        zwsgon(ior) = sgo2n(ior) 
        zwsdx0(ior) = sedx0(ior) 
        zwdon(ior) = don(ior) 
        zwsusn(ior) = susn(ior) 
        zwbetn(ior) = bettn(ior) 
        zwsuso(ior) = susno(ior) 
        zwagn4(ior) = agrnh4(ior) 
        zwakn4(ior) = akinh4(ior) 
        zwabn4(ior) = ablnh4(ior) 
        zwagn3(ior) = agrno3(ior) 
        zwakn3(ior) = akino3(ior) 
        zwabn3(ior) = ablno3(ior) 
        zwdzr1(ior) = dzres1(ior) 
        zwdzr2(ior) = dzres2(ior) 
        zwph(ior) = vph(ior) 
        zwsedn(ior) = sedn(ior) 
        zwrdr(ior) = resdr(ior) 
        zwaki(ior) = aki(ior) 
        zwagr(ior) = agr(ior) 
        zwabl(ior) = abl(ior) 
        zwexdk(ior) = exdrvk(ior) 
        zwexdg(ior) = exdrvg(ior) 
        zwexdb(ior) = exdrvb(ior)
        zwadrk(ior) = algdrk(ior) 
        zwadrg(ior) = algdrg(ior)
        zwadrb(ior) = algdrb(ior) 
        zwnl0(ior) = nl0(ior) 
        zwCsed(ior) = orgCsd(mstr,ior) 
        zup_NK(ior) = up_NKz(1,ior) 
        zup_NG(ior) = up_NGz(1,ior) 
        zup_NB(ior) = up_NBz(1,ior) 
        zQ_NK(ior) = Q_NK(ior) 
        zQ_NG(ior) = Q_NG(ior) 
        zQ_NB(ior) = Q_NB(ior) 
        zwJNO3(ior) = hJNO3(mstr,ior) 
        zwJNH4(ior) = hJNH4(mstr,ior) 
        zwFlN3(ior) = hFluN3(mstr,ior)
        zwJN2(ior) = hJN2(mstr,ior) 
        zaktbr(ior) = akibrz(1,ior) 
        zagtbr(ior) = agrbrz(1,ior) 
        zabtbr(ior) = ablbrz(1,ior) 
        zwakz(ior) = dalgkz(1,ior) 
        zwaakz(ior) = algakz(1,ior) 
        zwagz(ior) = dalggz(1,ior) 
        zwaagz(ior) = algagz(1,ior) 
        zwabz(ior) = dalgbz(1,ior) 
        zwaabz(ior) = algabz(1,ior) 
                                                                       
        zwkN4z(ior) = akNH4z(1,ior) 
        zwkN3z(ior) = akNO3z(1,ior) 
        zwgN4z(ior) = agNH4z(1,ior) 
        zwgN3z(ior) = agNO3z(1,ior) 
        zwbN4z(ior) = abNH4z(1,ior) 
        zwbN3z(ior) = abNO3z(1,ior) 
                                                                       
        zwnkzs(ior) = nkzs(ior) 
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior)
        vmitt(ior) = vbm(mstr,ior) 
        vno3(ior) = bno3(mstr,ior) 
        vnh4(ior) = bnh4(mstr,ior) 
        vno2(ior) = bno2(mstr,ior) 
        gesN(ior) = bgesN(mstr,ior) 
        vnh4z(1,ior) = bnh4(mstr,ior) 
        vno2z(1,ior) = bno2(mstr,ior) 
        vno3z(1,ior) = bno3(mstr,ior) 
        vx0(ior) = bx0(mstr,ior) 
        vx02(ior) = bx02(mstr,ior) 
        vo2(ior) = bo2(mstr,ior) 
        bsbt(ior) = bbsbt(mstr,ior) 
        bsbct(ior) = bbsbct(mstr,ior) 
        dON(ior) = bdoN(mstr,ior) 
        sedalk(ior) = bsedak(mstr,ior) 
        sedalg(ior) = bsedag(mstr,ior) 
        sedalb(ior) = bsedab(mstr,ior) 
        pfl(ior) = bpfl(mstr,ior)                                        
        dzres1(ior) = bzres1(mstr,ior) 
        dzres2(ior) = bzres2(mstr,ior) 
        vph(ior) = bph(mstr,ior) 
        albewg(ior) = babewg(mstr,ior) 
        albewk(ior) = babewk(mstr,ior) 
        alberg(ior) = baberg(mstr,ior) 
        alberk(ior) = baberk(mstr,ior) 
        resdr(ior) = bresdr(mstr,ior) 
        aki(ior) = baki(mstr,ior) 
        agr(ior) = bagr(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
        exdrvk(ior) = bexdvk(mstr,ior) 
        exdrvg(ior) = bexdvg(mstr,ior) 
        exdrvb(ior) = bexdvb(mstr,ior)
        algdrk(ior) = badrk(mstr,ior) 
        algdrg(ior) = badrg(mstr,ior) 
        algdrb(ior) = badrb(mstr,ior) 
        nl0(ior) = bnl0(mstr,ior) 
        orgCsd(mstr,ior) = borgCs(mstr,ior) 
        up_NKz(1,ior) = bup_NK(mstr,ior) 
        up_NGz(1,ior) = bup_NG(mstr,ior) 
        up_NBz(1,ior) = bup_NB(mstr,ior) 
        Q_NK(ior) = bQ_NK(mstr,ior) 
        Q_NG(ior) = bQ_NG(mstr,ior) 
        Q_NB(ior) = bQ_NB(mstr,ior) 
        hJNO3(mstr,ior) = bJNO3(mstr,ior) 
        hJNH4(mstr,ior) = bJNH4(mstr,ior)
        hFluN3(mstr,ior) = bFluN3(mstr,ior)
        hJN2(mstr,ior) = bJN2(mstr,ior) 
        akibrz(1,ior) = baktbr(mstr,ior) 
        agrbrz(1,ior) = bagtbr(mstr,ior) 
        ablbrz(1,ior) = babtbr(mstr,ior) 
        dalgkz(1,ior) = balgkz(mstr,ior) 
        algakz(1,ior) = balakz(mstr,ior) 
        dalggz(1,ior) = balggz(mstr,ior) 
        algagz(1,ior) = balagz(mstr,ior) 
        dalgbz(1,ior) = balgbz(mstr,ior) 
        algabz(1,ior) = balabz(mstr,ior) 
      enddo

      ilbuhn = 1 
      goto 1514 
      endif 
                                                                       
      if(ilbuhn.eq.1)then 
      do ior=1,anze+1 
        bno3(mstr,ior) = vno3(ior) 
        bnh4(mstr,ior) = vnh4(ior) 
        bno2(mstr,ior)  = vno2(ior) 
        bgesN(mstr,ior) = gesN(ior) 
        bx0(mstr,ior) = vx0(ior) 
        bx02(mstr,ior) = vx02(ior) 
        bsgon(mstr,ior) = sgo2n(ior) 
        bgo2n(mstr,ior) = go2n(ior) 
        bsedx0(mstr,ior) = sedx0(ior) 
        bdon(mstr,ior) = don(ior) 
        bsusn(mstr,ior) = susn(ior) 
        bbettn(mstr,ior) = bettn(ior) 
        bsuso(mstr,ior) = susno(ior) 
        bagn4(mstr,ior) = agrnh4(ior) 
        bakn4(mstr,ior) = akinh4(ior) 
        babn4(mstr,ior) = ablnh4(ior) 
        bagn3(mstr,ior) = agrno3(ior) 
        bakn3(mstr,ior) = akino3(ior) 
        babn3(mstr,ior) = ablno3(ior) 
        bsedn(mstr,ior) = sedn(ior) 
        bnl0(mstr,ior) = nl0(ior) 
        bup_NK(mstr,ior) = up_NKz(1,ior) 
        bup_NG(mstr,ior) = up_NGz(1,ior) 
        bQ_NK(mstr,ior) = Q_NK(ior) 
        bQ_NG(mstr,ior) = Q_NG(ior) 
        bFluN3(mstr,ior) = hFluN3(mstr,ior) 
        baktbr(mstr,ior) = akibrz(1,ior) 
        bagtbr(mstr,ior) = agrbrz(1,ior) 
        balgkz(mstr,ior) = dalgkz(1,ior) 
        balakz(mstr,ior) = algakz(1,ior) 
        balggz(mstr,ior) = dalggz(1,ior) 
        balagz(mstr,ior) = algagz(1,ior) 
        bkN4z(mstr,ior) = akNH4z(1,ior) 
        bkN3z(mstr,ior) = akNO3z(1,ior) 
        bgN4z(mstr,ior) = agNH4z(1,ior) 
        bgN3z(mstr,ior) = agNO3z(1,ior) 
        bbN4z(mstr,ior) = abNH4z(1,ior) 
        bbN3z(mstr,ior) = abNO3z(1,ior) 
                                                                       
        tempw(ior) = zwtemp(ior) 
        tiefe(ior) = zwtief(ior) 
        vmitt(ior) = zwvm(ior) 
        vno3(ior) = zwno3(ior) 
        vnh4(ior) = zwnh4(ior) 
        vno2(ior) = zwno2(ior) 
        gesN(ior) = zwgesN(ior) 
        nkzs(ior) = zwnkzs(ior) 
                                                                       
        vnh4z(1,ior) = zwn4z(ior) 
        vno2z(1,ior) = zwn2z(ior) 
        vno3z(1,ior) = zwn3z(ior) 
                                                                       
        vx0(ior) = zwx0(ior) 
        vx02(ior) = zwx02(ior) 
        vo2(ior) = zwvo2(ior) 
        go2n(ior) = zwgo2n(ior) 
        bsbt(ior) = zwbsbt(ior) 
        bsbct(ior) = zwbsct(ior) 
        doN(ior) = zwbsN(ior) 
        sedalk(ior) = zwsedk(ior) 
        sedalg(ior) = zwsedg(ior) 
        sedalb(ior) = zwsedb(ior) 
        sgo2n(ior) = zwsgon(ior) 
        sedx0(ior) = zwsdx0(ior) 
        don(ior) = zwdon(ior) 
        susn(ior) = zwsusn(ior) 
        bettn(ior) = zwbetn(ior) 
        susno(ior) = zwsuso(ior) 
        agrnh4(ior) = zwagn4(ior) 
        akinh4(ior) = zwakn4(ior) 
        ablnh4(ior) = zwabn4(ior) 
        agrno3(ior) = zwagn3(ior) 
        akino3(ior) = zwakn3(ior) 
        ablno3(ior) = zwabn3(ior) 
        dzres1(ior) = zwdzr1(ior) 
        dzres2(ior) = zwdzr2(ior) 
        vph(ior) = zwph(ior) 
        albewg(ior) = zwabwg(ior) 
        alberg(ior) = zwabrg(ior) 
        albewk(ior) = zwabwk(ior) 
        alberk(ior) = zwabrk(ior)
        sedn(ior) = zwsedn(ior) 
        resdr(ior) = zwrdr(ior) 
        aki(ior) = zwaki(ior) 
        agr(ior) = zwagr(ior) 
        abl(ior) = zwabl(ior) 
        exdrvk(ior) = zwexdk(ior) 
        exdrvg(ior) = zwexdg(ior) 
        exdrvb(ior) = zwexdb(ior) 
        algdrk(ior) = zwadrk(ior)
        algdrg(ior) = zwadrg(ior)
        algdrb(ior) = zwadrb(ior)
        nl0(ior) = zwnl0(ior) 
        orgCsd(mstr,ior) = zwCsed(ior) 
        up_NKz(1,ior) = zup_NK(ior) 
        up_NGz(1,ior) = zup_NG(ior) 
        up_NBz(1,ior) = zup_NB(ior) 
        Q_NK(ior) = zQ_NK(ior) 
        Q_NG(ior) = zQ_NG(ior) 
        Q_NB(ior) = zQ_NB(ior) 
        hJNO3(mstr,ior) = zwJNO3(ior) 
        hJNH4(mstr,ior) = zwJNH4(ior) 
        hFluN3(mstr,ior) = zwFlN3(ior)
        hJN2(mstr,ior) = zwJN2(ior) 
        akibrz(1,ior) = zaktbr(ior) 
        agrbrz(1,ior) = zagtbr(ior) 
        ablbrz(1,ior) = zabtbr(ior) 
        dalgkz(1,ior) = zwakz(ior) 
        algakz(1,ior) = zwaakz(ior) 
        dalggz(1,ior) = zwagz(ior) 
        algagz(1,ior) = zwaagz(ior) 
        dalgbz(1,ior) = zwabz(ior) 
        algabz(1,ior) = zwaabz(ior) 
                                                                       
        vNh4z(1,ior) = zwN4z(ior) 
        vNo2z(1,ior) = zwN2z(ior) 
        vNO3z(1,ior) = zwN3z(ior) 
        akNH4z(1,ior) = zwkN4z(ior) 
        akNO3z(1,ior) = zwkN3z(ior) 
        agNH4z(1,ior) = zwgN4z(ior) 
        agNO3z(1,ior) = zwgN3z(ior) 
        abNH4z(1,ior) = zwbN4z(ior) 
        abNO3z(1,ior) = zwbN3z(ior) 

        diff1 = bx0(mstr,ior)-vx0(ior)
        diff2 = bx02(mstr,ior)-vx02(ior)
        diff3 = bnh4(mstr,ior)-vnh4(ior)
        diff4 = bno2(mstr,ior)-vno2(ior)
        diff5 = bno3(mstr,ior)-vno3(ior)
        diff6 = bnl0(mstr,ior)-nl0(ior)
        diff7 = bgesN(mstr,ior)-gesN(ior)
        diff8 = bQ_NK(mstr,ior)-Q_NK(ior)
        diff9 = bQ_NG(mstr,ior)-Q_NG(ior)
        diff10 = bQ_NB(mstr,ior)-Q_NB(ior)
        diff11 = bFluN3(mstr,ior)-hFluN3(mstr,ior) 

        bdiff1 = vx0(ior)-bx0(mstr,ior)
        bdiff2 = vx02(ior)-bx02(mstr,ior)
        bdiff3 = vnh4(ior)-bnh4(mstr,ior)
        bdiff4 = vno2(ior)-bno2(mstr,ior)
        bdiff5 = vno3(ior)-bno3(mstr,ior)
        bdiff6 = nl0(ior)-bnl0(mstr,ior)
        bdiff7 = gesN(ior)-bgesN(mstr,ior)
        bdiff8 = Q_NK(ior)-bQ_NK(mstr,ior)
        bdiff9 = Q_NG(ior)-bQ_NG(mstr,ior)
        bdiff10 = Q_NB(ior)-bQ_NB(mstr,ior)
        bdiff11 = hFluN3(mstr,ior)-bFluN3(mstr,ior)    

      if(bleb(mstr,ior)>0.0)then
        vx0(ior) = vx0(ior)+diff1*(1.-exp(-hctau1(ior))) 
        vx02(ior) = vx02(ior)+diff2*(1.-exp(-hctau1(ior))) 
        vnh4(ior) = vnh4(ior)+diff3*(1.-exp(-hctau1(ior))) 
        vno2(ior) = vno2(ior)+diff4*(1.-exp(-hctau1(ior))) 
        vno3(ior) = vno3(ior)+diff5*(1.-exp(-hctau1(ior))) 
        nl0(ior) = nl0(ior)+diff6*(1.-exp(-hctau1(ior))) 
        gesN(ior) = gesN(ior)+diff7*(1.-exp(-hctau1(ior))) 
        Q_NK(ior) = Q_NK(ior)+diff8*(1.-exp(-hctau1(ior))) 
        Q_NG(ior) = Q_NG(ior)+diff9*(1.-exp(-hctau1(ior))) 
        Q_NB(ior) = Q_NB(ior)+diff10*(1.-exp(-hctau1(ior))) 
        hFluN3(mstr,ior) = hFluN3(mstr,ior)+diff11*(1.-exp(-hctau1(ior))) 
      endif
                                                                       
        if(hctau2(ior)>0.0)then 
          bx0(mstr,ior) = bx0(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bx02(mstr,ior) = bx02(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
          bnh4(mstr,ior) = bnh4(mstr,ior)+bdiff3*(1.-exp(-hctau2(ior))) 
          bno2(mstr,ior) = bno2(mstr,ior)+bdiff4*(1.-exp(-hctau2(ior))) 
          bno3(mstr,ior) = bno3(mstr,ior)+bdiff5*(1.-exp(-hctau2(ior))) 
          bnl0(mstr,ior) = bnl0(mstr,ior)+bdiff6*(1.-exp(-hctau2(ior))) 
          bgesN(mstr,ior) = bgesN(mstr,ior)+bdiff7*(1.-exp(-hctau2(ior))) 
          bQ_NK(mstr,ior) = bQ_NK(mstr,ior)+bdiff8*(1.-exp(-hctau2(ior))) 
          bQ_NG(mstr,ior) = bQ_NG(mstr,ior)+bdiff9*(1.-exp(-hctau2(ior))) 
          bQ_NB(mstr,ior) = bQ_NB(mstr,ior)+bdiff10*(1.-exp(-hctau2(ior))) 
          bFluN3(mstr,ior) = bFluN3(mstr,ior)+bdiff11*(1.-exp(-hctau2(ior))) 
        endif
      enddo
                                                                       
      ilbuhn = 0 
      endif 
!                                                                       
!*****************pH-Wert***********************                        
!                                                                       
 1515 continue 

      if(iph.eq.0)goto 113 
      if(vph(1).lt.0.0)goto 113

        if(nbuhn(mstr)>0.and.ilbuhn==0)then
        do ior=1,anze+1
          albewg(ior) = 0.0
          alberg(ior) = 0.0
          albewk(ior) = 0.0 
          alberk(ior) = 0.0
        enddo
      endif
                                                                        
      call ph(mw,pw,ca,lf,tempw,tflie,susn,bsbct,dalgki,dalggr,dalgak,dalgag,po2p,po2r,rau,vmitt,tiefe,flae,vabfl        &
             ,flag,elen,ior,anze,vph,elfL,caL,qeinlL,iorLa,iorLe,ieinLs,ssalg,stind,albewg,alberg,albewk,alberk,wge      &
             ,abl,dalgbl,dalgab,IDWe,iwied,fkm,ij,resdr,dzres1,dzres2,aki,agr,ilbuhn,eph,emw,elf,eca,vco2,qeinl          &
             ,jiein,mstr,cpfad,rhyd,WLage,hWS,itags,monats,uhrz,azStrs,iphy                                              &
             ,.false.,0)     !!wy kontroll,iglob
      if(nbuhn(mstr)==0)goto 113 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwtief(ior) = tiefe(ior) 
        zwvm(ior) = vmitt(ior) 
        zwmw(ior) = mw(ior) 
        zwpw(ior) = pw(ior) 
        zwca(ior) = ca(ior) 
        zwlf(ior) = lf(ior) 
        zwph(ior) = vph(ior)
        bph_1(ior) = bph(mstr,ior)  
        zwsusn(ior) = susn(ior) 
        zwbetn(ior) = bettn(ior) 
        zwbsbt(ior) = bsbct(ior) 
        zwdalk(ior) = dalgki(ior) 
        zwdalg(ior) = dalggr(ior) 
        zwdalb(ior) = dalgbl(ior) 
        zwdaak(ior) = dalgak(ior) 
        zwdaag(ior) = dalgag(ior) 
        zwdaab(ior) = dalgab(ior) 
        zwpo2p(ior) = po2p(ior) 
        zwpo2r(ior) = po2r(ior) 
        zwssa(ior) = ssalg(ior) 
        zwstin(ior) = stind(ior) 
        zwrdr(ior) = resdr(ior) 
        zwaki(ior) = aki(ior) 
        zwagr(ior) = agr(ior) 
        zwabl(ior) = abl(ior) 
        zwdzr1(ior) = dzres1(ior) 
        zwdzr2(ior) = dzres2(ior) 
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior) 
        mw(ior) = bmw(mstr,ior) 
        pw(ior) = bpw(mstr,ior) 
        ca(ior) = bca(mstr,ior) 
        lf(ior) = blf(mstr,ior) 
        vph(ior) = bph(mstr,ior) 
        susn(ior) = bsusn(mstr,ior) 
        bettn(ior) = bbettn(mstr,ior) 
        bsbct(ior) = bbsbct(mstr,ior) 
        dalgki(ior) = bdaki(mstr,ior) 
        dalggr(ior) = bdagr(mstr,ior) 
        dalgbl(ior) = bdabl(mstr,ior) 
        dalgak(ior) = bdaak(mstr,ior) 
        dalgag(ior) = bdaag(mstr,ior) 
        dalgab(ior) = bdaab(mstr,ior) 
        po2p(ior) = bpo2p(mstr,ior) 
        po2r(ior) = bpo2r(mstr,ior) 
        stind(ior) = bstind(mstr,ior) 
        ssalg(ior) = bssalg(mstr,ior) 
        albewg(ior) = babewg(mstr,ior) 
        albewk(ior) = babewk(mstr,ior) 
        alberg(ior) = baberg(mstr,ior) 
        alberk(ior) = baberk(mstr,ior) 
        resdr(ior) = bresdr(mstr,ior) 
        dzres1(ior) = bzres1(mstr,ior) 
        dzres2(ior) = bzres2(mstr,ior) 
        resdr(ior) = bresdr(mstr,ior) 
        aki(ior) = baki(mstr,ior) 
        agr(ior) = bagr(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
      enddo 

      ilbuhn = 1 
      goto 1515 
      endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        bmw(mstr,ior) = mw(ior) 
        bpw(mstr,ior) = pw(ior) 
        bca(mstr,ior) = ca(ior) 
        blf(mstr,ior) = lf(ior) 
        bph(mstr,ior) = vph(ior) 
        bstind(mstr,ior) = stind(ior) 
                                                                       
        tempw(ior) = zwtemp(ior) 
        tiefe(ior) = zwtief(ior) 
        vmitt(ior) = zwvm(ior) 
        mw(ior) = zwmw(ior) 
        pw(ior) = zwpw(ior) 
        ca(ior) = zwca(ior) 
        lf(ior) = zwlf(ior) 
        vph(ior) = zwph(ior) 
        susn(ior) = zwsusn(ior) 
        bettn(ior) = zwbetn(ior) 
        bsbct(ior) = zwbsbt(ior) 
        dalgki(ior) = zwdalk(ior) 
        dalggr(ior) = zwdalg(ior) 
        dalgbl(ior) = zwdalb(ior) 
        dalgak(ior) = zwdaak(ior) 
        dalgag(ior) = zwdaag(ior) 
        dalgab(ior) = zwdaab(ior) 
        po2p(ior) = zwpo2p(ior) 
        po2r(ior) = zwpo2r(ior) 
        ssalg(ior) = zwssa(ior) 
        stind(ior) = zwstin(ior) 
        resdr(ior) = zwrdr(ior) 
        aki(ior) = zwaki(ior) 
        agr(ior) = zwagr(ior) 
        abl(ior) = zwabl(ior) 
        dzres1(ior) = zwdzr1(ior) 
        dzres2(ior) = zwdzr2(ior) 
                                                                       
        diff1 = bmw(mstr,ior)-mw(ior)
        diff2 = bpw(mstr,ior)-pw(ior)
        diff3 = bca(mstr,ior)-ca(ior)
        diff4 = blf(mstr,ior)-lf(ior)
        diff5 = bph(mstr,ior)-vph(ior)
        diff6 = bstind(mstr,ior)-stind(ior)

        bdiff1 = mw(ior)-bmw(mstr,ior)
        bdiff2 = pw(ior)-bpw(mstr,ior)
        bdiff3 = ca(ior)-bca(mstr,ior)
        bdiff4 = lf(ior)-blf(mstr,ior)
        bdiff5 = vph(ior)-bph(mstr,ior)
        bdiff6 = stind(ior)-bstind(mstr,ior)

      if(bleb(mstr,ior)>0.0)then
        mw(ior) = mw(ior)+diff1*(1.-exp(-hctau1(ior))) 
        pw(ior) = pw(ior)+diff2*(1.-exp(-hctau1(ior))) 
        ca(ior) = ca(ior)+diff3*(1.-exp(-hctau1(ior))) 
        lf(ior) = lf(ior)+diff4*(1.-exp(-hctau1(ior))) 
        vph(ior) = vph(ior)+diff5*(1.-exp(-hctau1(ior))) 
        stind(ior) = stind(ior)+diff6*(1.-exp(-hctau1(ior))) 
      endif
                                                                         
        if(hctau2(ior)>0.0)then 
          bmw(mstr,ior) = bmw(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bpw(mstr,ior) = bpw(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
          bca(mstr,ior) = bca(mstr,ior)+bdiff3*(1.-exp(-hctau2(ior))) 
          blf(mstr,ior) = blf(mstr,ior)+bdiff4*(1.-exp(-hctau2(ior))) 
          bph(mstr,ior) = bph(mstr,ior)+bdiff5*(1.-exp(-hctau2(ior))) 
          bstind(mstr,ior) = bstind(mstr,ior)+bdiff6*(1.-exp(-hctau2(ior))) 
        endif
      enddo 
                                                                       
      ilbuhn = 0 
      endif 
!                                                                       
!****************Temperatur*******************                          
!                                                                       
  113 continue 
                                                                       
      if(iwsim/=4)goto 408 

      call CTracer(TEMPW,flag,anze,qeinl,etemp,vabfl,jiein,ilbuhn,nkzs,itags,uhrz,mstr)

      if(iwsim==4)goto 409
                                                                       
  408 continue 
      call temperw(RO,TEMPL,TEMPW,SCHWI,WGE,TIEFE,TFLIE,flag,elen,ior,anze,etemp,ewaerm,typ,qeinl,vabfl                 &
                  ,jiein,cloud,typw,iwied,uhrz,ilbuhn,nwaerm,fkm,nkzs,tempwz,dH2D,iorLa,iorLe,ieinLs,flae,qeinlL,etempL &
                  ,mstr,IDWe,ilang,dtemp,FluxT1,extk,itags,monats,Tsed,Wlage,hWS,iRHKW,htempw,htempz                    &
                  ,WUEBKS,SPEWKSS,PSREFSS,extkS,ifehl,ifhstr,azStrs,iwsim                                               &
                  ,.false.,0)     !!wy kontroll,iglob
     if(ifehl>0)goto 989
!...dtemp zwischenspeichern bei Buhnen                                  
  409 if(nbuhn(mstr)==0)goto 413 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwTsed(ior) = Tsed(ior) 
        zwextk(ior) = extk(ior) 
        do nkz = 1,nkzs(ior) 
          zwtez(nkz,ior) = tempwz(nkz,ior) 
        enddo

        zwnkzs(ior) = nkzs(ior) 
        zwtief(ior) = tiefe(ior) 
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        Tsed(ior) = bTsed(mstr,ior) 
        tempwz(1,ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior) 
        extk(ior) = bextk(mstr,ior) 
      enddo

      ilbuhn = 1 
      goto 113 
      endif 
                                                                       
      if(ilbuhn==1)then 
        do ior=1,anze+1 
          btempw(mstr,ior) = tempw(ior) 
          bTsed(mstr,ior) = Tsed(ior) 
          tempw(ior) = zwtemp(ior) 
          Tsed(ior) = zwTsed(ior) 
          nkzs(ior) = zwnkzs(ior) 
          extk(ior) = zwextk(ior) 

         do nkz = 1,nkzs(ior) 
           tempwz(nkz,ior) = zwtez(nkz,ior) 
         enddo

         tiefe(ior) = zwtief(ior)

         diff1 = btempw(mstr,ior)-tempw(ior) 
         bdiff1 = tempw(ior)-btempw(mstr,ior) 

      if(bleb(mstr,ior)>0.0)then
         tempw(ior) = tempw(ior)+diff1*(1.-exp(-hctau1(ior))) 
                                                                       
         do nkz = 1,nkzs(ior) 
           tempwz(nkz,ior) = tempwz(nkz,ior)+(btempw(mstr,ior)-tempw(ior))*(1.-exp(-hctau1(ior)))  
         enddo
      endif
                                                                        
         if(hctau2(ior)>0.0)then 
           btempw(mstr,ior) = btempw(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
         endif
       enddo
                                                                 
       ilbuhn = 0 
      endif 
                                                                       
!************ortho-Phosphat**********                                   
!                                                                       
  413 continue 
      if(iwsim==2.and.icoli==1)goto 1525
      if(iwsim==4.or.iwsim==2.or.iwsim==5)goto 118 
      if(gelP(1)<0.0)goto 1516

        if(nbuhn(mstr)>0.and.ilbuhn==0)then
        do ior=1,anze+1
          albewg(ior) = 0.0
          alberg(ior) = 0.0
          albewk(ior) = 0.0 
          alberk(ior) = 0.0
        enddo
      endif

      call po4s(gelp,flag,elen,ior,tiefe                                &
     &,dalggr,dalgki,dalgag,dalgak                                      &
     &,ep,qeinl,vabfl,anze,tflie,dzres1,dzres2                          &
     &,jiein,sedalk,sedalb,sedalg                                       &
     &,albewg,alberg,albewk,alberk,resdr,aki,agr,exdrvk,exdrvg,pl0      &
     &,abl,dalgbl,dalgab,exdrvb,gesP,orgCsd                             &
     &,zooind,GRote,pZoo,egesP,ilbuhn,iwied                             &
     &,CD,CP,CM,BAC,bsbctP,Qmx_PK,Q_PK,up_PKz                           &
     &,Qmx_PG,Q_PG,up_PGz,Qmx_PB,Q_PB,up_PBz,epl0                       &
     &,gelpz,agrtbr,akitbr,abltbr,agrbrz                                &
     &,akibrz,ablbrz,algakz,algagz,algabz,hJPO4,nkzs,dH2D               &
     &,dH2De,mstr,iorLa,iorLe,ieinLs,flae,qeinlL,gPL,gesPL,hgesPz       &
     &,algdrk,algdrg,algdrb,itags,monats,uhrz,azStrs                    &
     &,.false.,0)     !!wy kontroll,iglob      

      if(nbuhn(mstr)==0)goto 1516 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtief(ior) = tiefe(ior) 
        zwgelp(ior) = gelp(ior) 
        zwgesP(ior) = gesP(ior) 
        zwbsP(ior) = bsbctP(ior) 
        zwsedk(ior) = sedalk(ior) 
        zwsedg(ior) = sedalg(ior) 
        zwsedb(ior) = sedalb(ior) 
        zwdzr1(ior) = dzres1(ior) 
        zwdzr2(ior) = dzres2(ior) 
        zwrdr(ior) = resdr(ior) 
        zwaki(ior) = aki(ior) 
        zwagr(ior) = agr(ior) 
        zwabl(ior) = abl(ior) 
        zwexdk(ior) = exdrvk(ior) 
        zwexdg(ior) = exdrvg(ior) 
        zwexdb(ior) = exdrvb(ior)
        zwadrk(ior) = algdrk(ior) 
        zwadrg(ior) = algdrg(ior) 
        zwadrb(ior) = algdrb(ior) 
        zwpl0(ior) = pl0(ior) 
        zup_PK(ior) = up_PKz(1,ior) 
        zup_PG(ior) = up_PGz(1,ior) 
        zup_PB(ior) = up_PBz(1,ior) 
        zQ_PK(ior) = Q_PK(ior) 
        zQ_PG(ior) = Q_PG(ior) 
        zQ_PB(ior) = Q_PB(ior) 
        zwJPO4(ior) = hJPO4(mstr,ior) 
        zaktbr(ior) = akibrz(1,ior) 
        zagtbr(ior) = agrbrz(1,ior) 
        zabtbr(ior) = ablbrz(1,ior) 
        zwakz(ior) = dalgkz(1,ior) 
        zwaakz(ior) = algakz(1,ior) 
        zwagz(ior) = dalggz(1,ior) 
        zwaagz(ior) = algagz(1,ior) 
        zwabz(ior) = dalgbz(1,ior) 
        zwaabz(ior) = algabz(1,ior) 
        zwPz(ior) = gelPz(1,ior) 
        zwCsed(ior) = orgCsd(mstr,ior) 
                                                                       
        zwnkzs(ior) = nkzs(ior) 
                                                                       
        tiefe(ior) = bh(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior) 
        gelp(ior) = bgelp(mstr,ior) 
        gesP(ior) = bgesP(mstr,ior) 
        bsbct(ior) = bbsbct(mstr,ior) 
        bsbctP(ior) = bbsbcP(mstr,ior) 
        sedalk(ior) = bsedak(mstr,ior) 
        sedalg(ior) = bsedag(mstr,ior) 
        sedalb(ior) = bsedab(mstr,ior) 
        dzres1(ior) = bzres1(mstr,ior) 
        dzres2(ior) = bzres2(mstr,ior) 
        albewg(ior) = babewg(mstr,ior) 
        albewk(ior) = babewk(mstr,ior) 
        alberg(ior) = baberg(mstr,ior) 
        alberk(ior) = baberk(mstr,ior) 
        resdr(ior) = bresdr(mstr,ior) 
        aki(ior) = baki(mstr,ior) 
        agr(ior) = bagr(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
        exdrvk(ior) = bexdvk(mstr,ior) 
        exdrvg(ior) = bexdvg(mstr,ior) 
        exdrvb(ior) = bexdvb(mstr,ior)
        algdrk(ior) = badrk(mstr,ior) 
        algdrg(ior) = badrg(mstr,ior) 
        algdrb(ior) = badrb(mstr,ior) 
        pl0(ior) = bpl0(mstr,ior) 
        orgCsd(mstr,ior) = borgCs(mstr,ior) 
        up_PKz(1,ior) = bup_PK(mstr,ior) 
        up_PGz(1,ior) = bup_PG(mstr,ior) 
        up_PBz(1,ior) = bup_PB(mstr,ior) 
        Q_PK(ior) = bQ_PK(mstr,ior) 
        Q_PG(ior) = bQ_PG(mstr,ior) 
        Q_PB(ior) = bQ_PB(mstr,ior) 
        hJPO4(mstr,ior) = bJPO4(mstr,ior) 
        akibrz(1,ior) = baktbr(mstr,ior) 
        agrbrz(1,ior) = bagtbr(mstr,ior) 
        ablbrz(1,ior) = babtbr(mstr,ior) 
        dalgkz(1,ior) = balgkz(mstr,ior) 
        algakz(1,ior) = balakz(mstr,ior) 
        dalggz(1,ior) = balggz(mstr,ior) 
        algagz(1,ior) = balagz(mstr,ior) 
        dalgbz(1,ior) = balgbz(mstr,ior) 
        algabz(1,ior) = balabz(mstr,ior) 
        gelPz(1,ior) = bgelP(mstr,ior) 
      enddo

      ilbuhn = 1 
      goto 413 
      endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        bgelp(mstr,ior) = gelp(ior) 
        bgesP(mstr,ior) = gesP(ior) 
        bpl0(mstr,ior) = pl0(ior) 
                                                                       
        tiefe(ior) = zwtief(ior) 
        vmitt(ior) = zwvm(ior) 
        gelp(ior) = zwgelp(ior) 
        gesP(ior) = zwgesP(ior) 
        bsbct(ior) = zwbsct(ior) 
        bsbctP(ior) = zwbsP(ior) 
        sedalk(ior) = zwsedk(ior) 
        sedalg(ior) = zwsedg(ior) 
        sedalb(ior) = zwsedb(ior) 
        dzres1(ior) = zwdzr1(ior) 
        dzres2(ior) = zwdzr2(ior) 
        resdr(ior) = zwrdr(ior) 
        aki(ior) = zwaki(ior) 
        agr(ior) = zwagr(ior) 
        abl(ior) = zwabl(ior) 
        exdrvk(ior) = zwexdk(ior) 
        exdrvg(ior) = zwexdg(ior) 
        exdrvb(ior) = zwexdb(ior)
        algdrk(ior) = zwadrk(ior) 
        algdrg(ior) = zwadrg(ior) 
        algdrb(ior) = zwadrb(ior) 
        pl0(ior) = zwpl0(ior) 
        up_PKz(1,ior) = zup_PK(ior) 
        up_PGz(1,ior) = zup_PG(ior) 
        up_PBz(1,ior) = zup_PB(ior) 
        Q_PK(ior) = zQ_PK(ior) 
        Q_PG(ior) = zQ_PG(ior) 
        Q_PB(ior) = zQ_PB(ior) 
        hJPO4(mstr,ior) = zwJPO4(ior) 
        akibrz(1,ior) = zaktbr(ior) 
        agrbrz(1,ior) = zagtbr(ior) 
        ablbrz(1,ior) = zabtbr(ior) 
        dalgkz(1,ior) = zwakz(ior) 
        algakz(1,ior) = zwaakz(ior) 
        dalggz(1,ior) = zwagz(ior) 
        algagz(1,ior) = zwaagz(ior) 
        dalgbz(1,ior) = zwabz(ior) 
        algabz(1,ior) = zwaabz(ior) 
        orgCsd(mstr,ior) = zwCsed(ior) 
                                                                       
        nkzs(ior) = zwnkzs(ior) 
        gelPz(1,ior) = zwPz(ior) 

        diff1 = bgelp(mstr,ior)-gelp(ior)
        diff2 = bpl0(mstr,ior)-pl0(ior)
        diff3 = bgesP(mstr,ior)-gesP(ior)
        diff4 = bQ_PK(mstr,ior)-Q_PK(ior)
        diff5 = bQ_PG(mstr,ior)-Q_PG(ior)
        diff6 = bQ_PB(mstr,ior)-Q_PB(ior)

        bdiff1 = gelp(ior)-bgelp(mstr,ior)
        bdiff2 = pl0(ior)-bpl0(mstr,ior)
        bdiff3 = gesP(ior)-bgesP(mstr,ior)
        bdiff4 = Q_PK(ior)-bQ_PK(mstr,ior)
        bdiff5 = Q_PG(ior)-bQ_PG(mstr,ior)
        bdiff6 = Q_PB(ior)-bQ_PB(mstr,ior)  
                                                                       
      if(bleb(mstr,ior)>0.0)then
        gelp(ior) = gelp(ior)+diff1*(1.-exp(-hctau1(ior))) 
        pl0(ior) = pl0(ior)+diff2*(1.-exp(-hctau1(ior))) 
        gesP(ior) = gesP(ior)+diff3*(1.-exp(-hctau1(ior))) 
        Q_PK(ior) = Q_PK(ior)+diff4*(1.-exp(-hctau1(ior))) 
        Q_PG(ior) = Q_PG(ior)+diff5*(1.-exp(-hctau1(ior))) 
        Q_PB(ior) = Q_PB(ior)+diff6*(1.-exp(-hctau1(ior))) 
      endif
                                                                       
        if(hctau2(ior)>0.0)then 
          bgelp(mstr,ior) = bgelp(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bpl0(mstr,ior) = bpl0(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
          bgesP(mstr,ior) = bgesP(mstr,ior)+bdiff3*(1.-exp(-hctau2(ior))) 
          bQ_PK(mstr,ior) = bQ_PK(mstr,ior)+bdiff4*(1.-exp(-hctau2(ior))) 
          bQ_PG(mstr,ior) = bQ_PG(mstr,ior)+bdiff5*(1.-exp(-hctau2(ior))) 
          bQ_PB(mstr,ior) = bQ_PB(mstr,ior)+bdiff6*(1.-exp(-hctau2(ior))) 
        endif                                                                       
      enddo                                                                       
      ilbuhn = 0 
      endif 
!                                                                       
!***************Silikat**********************                           
!                                                                       
 1516 continue 
      if(si(1)<0.0)goto 1517 

      call silikat(si,flag,elen,ior,esi,qeinl,vabfl,anze,tflie,jiein,aki         &
                   ,albewk,alberk,tiefe,tempw,ilbuhn,akkssi,Qmx_SK,Q_SK          &
                   ,up_Siz,Siz,algakz,akitbr,akibrz,hJSi,nkzs,dH2D,dH2De,mstr    &
                   ,iorLa,iorLe,ieinLs,flae,qeinlL,SiL,itags,Uhrz,azStrs         &
                   ,.false.,0)     !!wy kontroll,iglob
      if(nbuhn(mstr)==0)goto 1517 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtief(ior) = tiefe(ior) 
        zwtemp(ior) = tempw(ior) 
        zwsi(ior) = si(ior) 
        zwabwk(ior) = albewk(ior) 
        zwabrk(ior) = alberk(ior) 
        zwsisd(ior) = sised(ior) 
        zwSKmo(ior) = SKmor(ior) 
        zup_Si(ior) = up_Siz(1,ior) 
        zQ_SK(ior) = Q_SK(ior) 
        zwaakz(ior) = algakz(1,ior)
        zwJSi(ior) = hJSi(mstr,ior) 
                                                                       
        zwnkzs(ior) = nkzs(ior) 
        zwsiz(ior) = siz(1,ior) 
                                                                       
        tiefe(ior) = bh(mstr,ior) 
        tempw(ior) = btempw(mstr,ior) 
        si(ior) = bsi(mstr,ior) 
        albewk(ior) = babewk(mstr,ior) 
        alberk(ior) = baberk(mstr,ior) 
        sised(ior) = bsised(mstr,ior) 
        SKmor(ior) = bSKmor(mstr,ior) 
        up_Siz(1,ior) = bup_Si(mstr,ior) 
        Q_SK(ior) = bQ_SK(mstr,ior)
        hJSi(mstr,ior) = bJSi(mstr,ior) 
        akibrz(1,ior) = baktbr(mstr,ior) 
        algakz(1,ior) = balakz(mstr,ior) 
        Siz(1,ior) = bsi(mstr,ior) 
      enddo

      ilbuhn = 1 
      goto 1516 
      endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        bsi(mstr,ior) = si(ior) 
        bsised(mstr,ior) = sised(ior) 
        bSKmor(mstr,ior) = SKmor(ior) 
                                                                       
        tiefe(ior) = zwtief(ior) 
        tempw(ior) = zwtemp(ior) 
        si(ior) = zwsi(ior) 
        albewk(ior) = zwabwk(ior) 
        alberk(ior) = zwabrk(ior) 
        sised(ior) = zwsisd(ior) 
        SKmor(ior) = zwSKmo(ior) 
        up_Siz(1,ior) = zup_Si(ior) 
        Q_SK(ior) = zQ_SK(ior)
        hJSi(mstr,ior) = zwJSi(ior) 
        algakz(1,ior) = zwaakz(ior) 
                                                                       
        nkzs(ior) = zwnkzs(ior) 
        siz(1,ior) = zwsiz(ior)

        diff1 = bsi(mstr,ior)-si(ior)
        diff2 = bQ_SK(mstr,ior)-Q_SK(ior)

        bdiff1 = si(ior)-bsi(mstr,ior)
        bdiff2 = Q_SK(ior)-bQ_SK(mstr,ior)
                                                                       
      if(bleb(mstr,ior)>0.0)then
        si(ior) = si(ior)+diff1*(1.-exp(-hctau1(ior))) 
        Q_SK(ior) = Q_SK(ior)+diff2*(1.-exp(-hctau1(ior))) 
      endif
                                                                         
        if(hctau2(ior)>0.0)then 
          bsi(mstr,ior) = bsi(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bQ_SK(mstr,ior) = bQ_SK(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
        endif
      enddo
                                                                       
      ilbuhn = 0 
      endif 
!*********Sauerstoff******************                                  
!                                                                       
 1517 continue 
 



     if(vo2(1)<0.0)goto 1518 

     if(nbuhn(mstr)>0.and.ilbuhn==0)then
     do ior=1,anze+1
       bpo2p(mstr,ior) = po2p(ior)
       bpo2r(mstr,ior) = po2r(ior)
       bro2dr(mstr,ior) = ro2dr(ior)
       
       po2p(ior) = 0.0
       po2r(ior) = 0.0
       albewg(ior) = 0.0
       alberg(ior) = 0.0
       albewk(ior) = 0.0 
       alberk(ior) = 0.0
       ro2dr(ior) = 0.0
     enddo
     endif

  call               oxygen(VO2,TEMPW,RAU,VMITT,TIEFE,RHYD,FLAE,TFLIE,go2n,dalgki,dalggr,dalgak,dalgag,akinh4    &
                    ,agrnh4,akino3,agrno3,bsbt,hJO2,flag,elen,ior,anze,dzres1,dzres2,hschlr                      &
                    ,eo2,qeinl,vabfl,po2p,po2r,so2ein,dO2o2D,salgo,dalgo,dalgao,o2ein1,jiein                     &
                    ,opgrmi,opgrma,opkimi,opkima,albewg,alberg,abeowg,abeorg,opblmi,opblma,ablnh4                &
                    ,ablno3,dalgbl,dalgab,albewk,alberk,abeowk,abeork,ro2dr,wge,IDWe,fkm,uhrz                    &
                    ,zooro2,rO2HNF,ilbuhn,iwied,vo2z,susO2N,nkzs,dH2D,o2L,qeinlL                                 &
                    ,iorLa,iorLe,ieinLs,agnh4z,aknh4z,abnh4z,dalgkz,dalgbz,dalggz,agno3z,akno3z                  &
                    ,abno3z,algakz,algagz,algabz,vz1,tempwz,saett,mstr,cpfad,ij,itags,monats                     &
                    ,dC_DenW,TOC_CSB,WLage,hWS,etemp,dH2De,ifehl,ifhStr,azStrs,zooind,GRote,iphy                 &  ! chlagr unbenutzt
                    ,kontroll,0)     !!wy ,iglob=0
      if(ifehl>0)goto 989 
                                                          
      if(nbuhn(mstr)==0)goto 1518 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwtief(ior) = tiefe(ior) 
        zwvm(ior) = vmitt(ior) 
        zwno3(ior) = vno3(ior) 
        zwnh4(ior) = vnh4(ior) 
        zwvo2(ior) = vo2(ior) 

        do nkz = 1,nkzs(ior) 
          zwo2z(nkz,ior) = vo2z(nkz,ior) 
        enddo

        zwnkzs(ior) = nkzs(ior) 
        zwgo2n(ior) = go2n(ior) 
        zwbsbt(ior) = bsbt(ior) 
        zwdalk(ior) = dalgki(ior) 
        zwdalg(ior) = dalggr(ior) 
        zwdalb(ior) = dalgbl(ior) 
        zwdaak(ior) = dalgak(ior) 
        zwdaag(ior) = dalgag(ior) 
        zwdaab(ior) = dalgab(ior) 
        zwschr(ior) = hschlr(mstr,ior) 
        zwagn4(ior) = agrnh4(ior) 
        zwakn4(ior) = akinh4(ior) 
        zwabn4(ior) = ablnh4(ior) 
        zwagn3(ior) = agrno3(ior) 
        zwakn3(ior) = akino3(ior) 
        zwabn3(ior) = ablno3(ior) 
        zwdzr1(ior) = dzres1(ior) 
        zwdzr2(ior) = dzres2(ior) 
        zwso2e(ior) = so2ein(ior) 
        zwsalo(ior) = salgo(ior) 
        zwdalo(ior) = dalgo(ior) 
        zwdago(ior) = dalgao(ior) 
        zwo2ei(ior) = o2ein1(ior) 
        zwabwg(ior) = abeowg(ior) 
        zwabwk(ior) = abeowk(ior) 
        zwabrg(ior) = abeorg(ior) 
        zwabrk(ior) = abeork(ior) 
        zwrzo(ior) =  zooro2(ior) 
        zwrHNF(ior) = ro2HNF(ior) 
        zwJO2(ior) = hJO2(mstr,ior) 
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior) 
        vno3(ior) = bno3(mstr,ior) 
        vnh4(ior) = bnh4(mstr,ior) 
        vo2(ior) = bo2(mstr,ior) 
        vo2z(1,ior) = bo2(mstr,ior) 
        go2n(ior) = bgo2n(mstr,ior) 
        bsbt(ior) = bbsbt(mstr,ior) 
        dalgki(ior) = bdaki(mstr,ior) 
        dalggr(ior) = bdagr(mstr,ior) 
        dalgbl(ior) = bdabl(mstr,ior) 
        dalgak(ior) = bdaak(mstr,ior) 
        dalgag(ior) = bdaag(mstr,ior) 
        dalgab(ior) = bdaab(mstr,ior) 
        agrnh4(ior) = bagn4(mstr,ior) 
        akinh4(ior) = bakn4(mstr,ior) 
        ablnh4(ior) = babn4(mstr,ior) 
        agrno3(ior) = bagn3(mstr,ior) 
        akino3(ior) = bakn3(mstr,ior) 
        ablno3(ior) = babn3(mstr,ior) 
        dzres1(ior) = bzres1(mstr,ior) 
        dzres2(ior) = bzres2(mstr,ior) 
        po2p(ior) = bpo2p(mstr,ior) 
        po2r(ior) = bpo2r(mstr,ior) 
        albewg(ior) = babewg(mstr,ior) 
        albewk(ior) = babewk(mstr,ior) 
        alberg(ior) = baberg(mstr,ior) 
        alberk(ior) = baberk(mstr,ior) 
        ro2dr(ior) = bro2dr(mstr,ior) 
        ro2HNF(ior) = bro2HF(mstr,ior) 
        dalgkz(1,ior) = balgkz(mstr,ior) 
        algakz(1,ior) = balakz(mstr,ior) 
        dalggz(1,ior) = balggz(mstr,ior) 
        algagz(1,ior) = balagz(mstr,ior) 
        akNH4z(1,ior) = bkN4z(mstr,ior) 
        akNO3z(1,ior) = bkN3z(mstr,ior) 
        agNH4z(1,ior) = bgN4z(mstr,ior) 
        agNO3z(1,ior) = bgN3z(mstr,ior) 
        hJO2(mstr,ior) = bJO2(mstr,ior) 
      enddo                                                                       

      ilbuhn = 1 
      goto 1517 
      endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        bo2(mstr,ior) = vo2(ior) 
        bschlr(mstr,ior) = hschlr(mstr,ior) 
                                                                       
        tempw(ior) = zwtemp(ior) 
        tiefe(ior) = zwtief(ior) 
        vmitt(ior) = zwvm(ior) 
        vno3(ior) = zwno3(ior) 
        vnh4(ior) = zwnh4(ior) 
        vo2(ior) = zwvo2(ior) 
        nkzs(ior) = zwnkzs(ior) 
        do nkz = 1,nkzs(ior) 
          vo2z(nkz,ior) = zwo2z(nkz,ior) 
        enddo

        go2n(ior) = zwgo2n(ior) 
        bsbt(ior) = zwbsbt(ior) 
        dalgki(ior) = zwdalk(ior) 
        dalggr(ior) = zwdalg(ior) 
        dalgbl(ior) = zwdalb(ior) 
        dalgak(ior) = zwdaak(ior) 
        dalgag(ior) = zwdaag(ior) 
        dalgab(ior) = zwdaab(ior) 
        hschlr(mstr,ior) = zwschr(ior) 
        agrnh4(ior) = zwagn4(ior) 
        akinh4(ior) = zwakn4(ior) 
        ablnh4(ior) = zwabn4(ior) 
        agrno3(ior) = zwagn3(ior) 
        akino3(ior) = zwakn3(ior) 
        ablno3(ior) = zwabn3(ior) 
        dzres1(ior) = zwdzr1(ior) 
        dzres2(ior) = zwdzr2(ior) 
        so2ein(ior) = zwso2e(ior) 
        salgo(ior) = zwsalo(ior) 
        dalgo(ior) = zwdalo(ior) 
        dalgao(ior) = zwdago(ior) 
        o2ein1(ior) = zwo2ei(ior) 
        abeowg(ior) = zwabwg(ior) 
        abeowk(ior) = zwabwk(ior) 
        abeorg(ior) = zwabrg(ior) 
        abeork(ior) = zwabrk(ior) 
        zooro2(ior) = zwrzo(ior) 
        ro2HNF(ior) = zwrHNF(ior) 
        hJO2(mstr,ior) = zwJO2(ior) 
                                                                       
        diff1 = bo2(mstr,ior)-vo2(ior)

        bdiff1 = vo2(ior)-bo2(mstr,ior)

      if(bleb(mstr,ior)>0.0)then
        vo2(ior) = vo2(ior)+diff1*(1.-exp(-hctau1(ior))) 
      endif
                                                                       
        if(hctau2(ior)>0.0)then 
          bo2(mstr,ior) = bo2(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
        endif
      enddo                                                               
                                                                       
      ilbuhn = 0 
      endif 
!                                                                       
!***************Schwebstoffe**************                              
!                                                                       
 1518 continue 
      if(ssalg(1)<0.0)goto 1519 
      call SCHWEB(zooind,dorgSS,ss,ssalg,tiefe,rau                      &
     &,tflie,VMITT,flae,flag,elen,ior,anze,ess,ssL,qeinl,qeinlL,vabfl   &
     &,dkimor,dgrmor,abszo,zexki,zexgr,iorLa,iorLe,ieinLs               &
     &,abl,zexbl,dblmor,drfaeb,jiein                                    &
     &,aki,agr,ssdr,drfaek,drfaeg,drfaes,fssgr,sedss,sedSS_MQ,fssgrs    &
     &,tauscs,ischif,ilbuhn,fkm,ieros,iwied                             &
     &,echla,vkigr,akbcm,agbcm,antbl,abbcm,ezind,GROTe,mstr,itags,monats,uhrz, azStrs &
     &,.false.,0)     !!wy kontroll,iglob
      if(nbuhn(mstr)==0)goto 1519 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtief(ior) = tiefe(ior) 
        zwvm(ior) = vmitt(ior) 
        zwzooi(ior) = zooind(ior) 
        zworgS(ior) = dorgSS(ior) 
        zwss(ior) = ss(ior) 
        zwssa(ior) = ssalg(ior)
        bssalg_1(ior) = bssalg(mstr,ior)         
        zwkmor(ior) = dkimor(ior) 
        zwgmor(ior) = dgrmor(ior) 
        zwbmor(ior) = dblmor(ior) 
        zwabsz(ior) = abszo(ior) 
        zwzexk(ior) = zexki(ior) 
        zwzexg(ior) = zexgr(ior) 
        zwzexb(ior) = zexbl(ior) 
        zwaki(ior) = aki(ior) 
        zwagr(ior) = agr(ior) 
        zwabl(ior) = abl(ior) 
        zwkigr(ior) = vkigr(ior) 
        zwantb(ior) = antbl(ior) 
        zwdfak(ior) = drfaek(ior) 
        zwdfag(ior) = drfaeg(ior) 
        zwdfab(ior) = drfaeb(ior) 
        zwdfas(ior) = drfaes(ior) 
        zwssdr(ior) = ssdr(ior) 
        zwfssg(ior) = fssgr(ior) 
        zwsedS(ior) = sedss(ior)
        zwsedSS_MQ = sedSS_MQ(mstr,ior) 
                                                                       
        tiefe(ior) = bh(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior) 
        zooind(ior) = bzooi(mstr,ior) 
        dorgSS(ior) = borgSS(mstr,ior) 
        ss(ior) = bss(mstr,ior) 
        ssalg(ior) = bssalg(mstr,ior) 
        dkimor(ior) = bdkmor(mstr,ior) 
        dgrmor(ior) = bdgmor(mstr,ior) 
        dblmor(ior) = bdbmor(mstr,ior) 
        abszo(ior) = babszo(mstr,ior) 
        zexki(ior) = bzexki(mstr,ior) 
        zexgr(ior) = bzexgr(mstr,ior) 
        zexbl(ior) = bzexbl(mstr,ior) 
        aki(ior) = baki(mstr,ior) 
        agr(ior) = bagr(mstr,ior) 
        abl(ior) = babl(mstr,ior) 
        vkigr(ior) = bvkigr(mstr,ior) 
        antbl(ior) = bantbl(mstr,ior) 
        drfaek(ior) = bdfaek(mstr,ior) 
        drfaeg(ior) = bdfaeg(mstr,ior) 
        drfaeb(ior) = bdfaeb(mstr,ior) 
        drfaes(ior) = bdfaes(mstr,ior) 
        ssdr(ior) = bssdr(mstr,ior) 
        fssgr(ior) = bfssgr(mstr,ior) 
        sedSS(ior) = bsedSS(mstr,ior) 
        sedSS_MQ(mstr,ior) = bsedSS_MQ(mstr,ior) 
      enddo

      ilbuhn = 1 
      goto 1518 
      endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        borgSS(mstr,ior) = dorgSS(ior) 
        bss(mstr,ior) = ss(ior) 
        bssalg(mstr,ior) = ssalg(ior) 
        bssdr(mstr,ior) = ssdr(ior) 
        bfssgr(mstr,ior) = fssgr(ior) 
        bsedSS(mstr,ior) = sedSS(ior) 
        bsedSS_MQ(mstr,ior) = sedSS_MQ(mstr,ior) 
                                                                       
        tempw(ior) = zwtemp(ior) 
        tiefe(ior) = zwtief(ior) 
        vmitt(ior) = zwvm(ior) 
        zooind(ior) = zwzooi(ior) 
        dorgSS(ior) = zworgS(ior) 
        ss(ior) = zwss(ior) 
        ssalg(ior) = zwssa(ior) 
        dkimor(ior) = zwkmor(ior) 
        dgrmor(ior) = zwgmor(ior) 
        dblmor(ior) = zwbmor(ior) 
        abszo(ior) = zwabsz(ior) 
        zexki(ior) = zwzexk(ior) 
        zexgr(ior) = zwzexg(ior) 
        zexbl(ior) = zwzexb(ior) 
        aki(ior) = zwaki(ior) 
        agr(ior) = zwagr(ior) 
        abl(ior) = zwabl(ior) 
        vkigr(ior) = zwkigr(ior) 
        antbl(ior) = zwantb(ior) 
        drfaek(ior) = zwdfak(ior) 
        drfaeg(ior) = zwdfag(ior) 
        drfaes(ior) = zwdfas(ior) 
        ssdr(ior) = zwssdr(ior) 
        fssgr(ior) = zwfssg(ior) 
        sedss(ior) = zwsedS(ior) 
        sedSS_MQ(mstr,ior) = zwsedSS_MQ(ior) 

       if(ieros==0)then
        diff1 = bss(mstr,ior)-ss(ior)
        diff2 = bssalg(mstr,ior)-ssalg(ior)
        diff3 = bfssgr(mstr,ior)-fssgr(ior)

        bdiff1 = ss(ior)-bss(mstr,ior)
        bdiff2 = ssalg(ior)-bssalg(mstr,ior)
        bdiff3 = fssgr(ior)-bfssgr(mstr,ior)                                                                     
                                                                       
    if(bleb(mstr,ior)>0.0)then
      ss(ior) = ss(ior)+diff1*(1.-exp(-hctau1(ior))) 
      ssalg(ior) = ssalg(ior)+diff2*(1.-exp(-hctau1(ior))) 
      fssgr(ior) = fssgr(ior)+diff3*(1.-exp(-hctau1(ior))) 
    endif
                                                                       
        if(hctau2(ior)>0.0)then 
          bss(mstr,ior) = bss(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bssalg(mstr,ior) = bssalg(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
          bfssgr(mstr,ior) = bfssgr(mstr,ior)+bdiff3*(1.-exp(-hctau2(ior))) 
        endif
     endif
      enddo

      ilbuhn = 0 
      endif 
                                                                       
                                                                       
 1519 continue 

!********Schwermetalle***********                                            

      if(ischwer==1)then 
  1521  call Schwermetalle(vabfl,qeinl,mstr,flag,anze,sedss,sedalk,sedalb,sedalg,hSSalg,SSalg,hph,vph,bssalg,bssalg_1,bph      &
                          ,bph_1,hglZn,hgsZn,egsZn,eglZn,hglCad,hgsCad,egsCad,eglCad,hglCu,hgsCu,egsCu,eglCu,hglNi,hgsNi       &
                          ,egsNi,eglNi,eph,ess,jiein,ilbuhn,azStrs,iformVert                                                   &
                          ,.false.,0)     !!wy kontroll,iglob

        if(nbuhn(mstr)==0)goto 1525
        if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwsedk(ior) = sedalk(ior) 
        zwsedg(ior) = sedalg(ior) 
        zwsedb(ior) = sedalb(ior) 
        zwsedS(ior) = sedss(ior)
        zwgsZn(ior) = hgsZn(mstr,ior)
        zwglZn(ior) = hglZn(mstr,ior)
        zwgsCad(ior) = hgsCad(mstr,ior)
        zwglCad(ior) = hglCad(mstr,ior)
        zwgsCu(ior) = hgsCu(mstr,ior)
        zwglCu(ior) = hglCu(mstr,ior)
        zwgsNi(ior) = hgsNi(mstr,ior)
        zwglNi(ior) = hglNi(mstr,ior)

        sedalk(ior) = bsedak(mstr,ior)
        sedalg(ior) = bsedag(mstr,ior)
        sedalb(ior) = bsedab(mstr,ior)
        sedss(ior) = bsedss(mstr,ior) 
        hgsZn(mstr,ior) = bgsZn(mstr,ior)
        hglZn(mstr,ior) = bglZn(mstr,ior)
        hgsCad(mstr,ior) = bgsCad(mstr,ior) 
        hglCad(mstr,ior) = bglCad(mstr,ior)
        hgsCu(mstr,ior) = bgsCu(mstr,ior)
        hglCu(mstr,ior) = bglCu(mstr,ior)
        hgsNi(mstr,ior) = bgsNi(mstr,ior)
        hglNi(mstr,ior) = bglNi(mstr,ior)
      enddo
      ilbuhn = 1 
      goto 1521 
      endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        bgsZn(mstr,ior) = hgsZn(mstr,ior)
        bglZn(mstr,ior) = hglZn(mstr,ior)
        bgsCad(mstr,ior) = hgsCad(mstr,ior)
        bglCad(mstr,ior) = hglCad(mstr,ior)
        bgsCu(mstr,ior) = hgsCu(mstr,ior)
        bglCu(mstr,ior) = hglCu(mstr,ior)
        bgsNi(mstr,ior) = hgsNi(mstr,ior)
        bglNi(mstr,ior) = hglNi(mstr,ior)

        hgsZn(mstr,ior) = zwgsZn(ior)
        hglZn(mstr,ior) = zwglZn(ior)
        hgsCad(mstr,ior) = zwgsCad(ior)
        hglCad(mstr,ior) = zwglCad(ior)
        hgsCu(mstr,ior) = zwgsCu(ior)
        hglCu(mstr,ior) = zwglCu(ior)
        hgsNi(mstr,ior) = zwgsNi(ior)
        hglNi(mstr,ior) = zwglNi(ior)

        sedalk(ior) = zwsedk(ior)
        sedalg(ior) = zwsedg(ior)
        sedalb(ior) = zwsedb(ior)
        sedss(ior) = zwsedS(ior)

      if(ieros==0)then
        diff1 = bgsZn(mstr,ior)-hgsZn(mstr,ior)
        diff2 = bglZn(mstr,ior)-hglZn(mstr,ior)
        diff3 = bgsCad(mstr,ior)-hgsCad(mstr,ior)
        diff4 = bglCad(mstr,ior)-hglCad(mstr,ior)
        diff5 = bgsCu(mstr,ior)-hgsCu(mstr,ior)
        diff6 = bglCu(mstr,ior)-hglCu(mstr,ior)
        diff7 = bgsNi(mstr,ior)-hgsNi(mstr,ior)
        diff8 = bglNi(mstr,ior)-hglNi(mstr,ior)

        bdiff1 = hgsZn(mstr,ior)-bgsZn(mstr,ior)
        bdiff2 = hglZn(mstr,ior)-bglZn(mstr,ior)
        bdiff3 = hgsCad(mstr,ior)-bgsCad(mstr,ior)
        bdiff4 = hglCad(mstr,ior)-bglCad(mstr,ior)
        bdiff5 = hgsCu(mstr,ior)-bgsCu(mstr,ior)
        bdiff6 = hglCu(mstr,ior)-bglCu(mstr,ior)
        bdiff7 = hgsNi(mstr,ior)-bgsNi(mstr,ior)
        bdiff8 = hglNi(mstr,ior)-bglNi(mstr,ior)
                                                                     
    if(bleb(mstr,ior)>0.0)then
      hgsZn(mstr,ior) = hgsZn(mstr,ior)+diff1*(1.-exp(-hctau1(ior))) 
      hglZn(mstr,ior) = hglZn(mstr,ior)+diff2*(1.-exp(-hctau1(ior))) 
      hgsCad(mstr,ior) = hgsCad(mstr,ior)+diff3*(1.-exp(-hctau1(ior))) 
      hglCad(mstr,ior) = hglCad(mstr,ior)+diff4*(1.-exp(-hctau1(ior))) 
      hgsCu(mstr,ior) = hgsCu(mstr,ior)+diff5*(1.-exp(-hctau1(ior))) 
      hglCu(mstr,ior) = hglCu(mstr,ior)+diff6*(1.-exp(-hctau1(ior))) 
      hgsNi(mstr,ior) = hgsNi(mstr,ior)+diff7*(1.-exp(-hctau1(ior))) 
      hglNi(mstr,ior) = hglNi(mstr,ior)+diff8*(1.-exp(-hctau1(ior))) 
    endif
                                                                       
        if(hctau2(ior)>0.0)then 
          bgsZn(mstr,ior) = bgsZn(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bglZn(mstr,ior) = bglZn(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
          bgsCad(mstr,ior) = bgsCad(mstr,ior)+bdiff3*(1.-exp(-hctau2(ior))) 
          bglCad(mstr,ior) = bglCad(mstr,ior)+bdiff4*(1.-exp(-hctau2(ior))) 
          bgsCu(mstr,ior) = bgsCu(mstr,ior)+bdiff5*(1.-exp(-hctau2(ior))) 
          bglCu(mstr,ior) = bglCu(mstr,ior)+bdiff6*(1.-exp(-hctau2(ior))) 
          bgsNi(mstr,ior) = bgsNi(mstr,ior)+bdiff7*(1.-exp(-hctau2(ior))) 
          bglNi(mstr,ior) = bglNi(mstr,ior)+bdiff8*(1.-exp(-hctau2(ior))) 
        endif
      endif
      enddo

      ilbuhn = 0 
      endif 
   endif

                                                                       
!********Coliform***********                                            

 1525 Continue

      if(iwsim==5)goto 118 
      if(iwsim/=2)goto 1520 
      if(hcoli(mstr,1)<0.0.and.iwsim==2)goto 118
                                                                       
 1522 call COLIFORM(tiefe,rau,vmitt,vabfl,elen,flae,flag,tflie,schwi,ss,zooind,GRote,Chla,tempw,jiein,ecoli     &
                   ,qeinl,coliL,qeinlL,anze,iorLa,iorLe,ieinLs,ilbuhn,coli,DOSCF,extkS,mstr,azStrs              &
                   ,RateCde,etaCde,RateCIe,xnueCe,RateCGe,RateCSe,ifehl                                         &
                   ,.false.,0)     !!wy kontroll,iglob

      if(ifehl>0)goto 989                             

      if(nbuhn(mstr)==0.and.iwsim==2)goto 118 
      if(nbuhn(mstr)==0.and.iwsim/=2)goto 1520 
      if(ilbuhn==0)then 
      do ior=1,anze+1 
        zwtemp(ior) = tempw(ior) 
        zwDOSCF(ior) = DOSCF(ior)
        zwtief(ior) = tiefe(ior)
        zwcoli(ior) = coli(ior)     
                                                                       
        tempw(ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior) 
        DOSCF(ior) = bDOSCF(mstr,ior)
        coli(ior) = bcoli(mstr,ior)
      enddo
      ilbuhn = 1 
      goto 1522 
      endif 
                                                                       
      if(ilbuhn==1)then 
      do ior=1,anze+1 
        btempw(mstr,ior) = tempw(ior) 
        bh(mstr,ior) = tiefe(ior)  
        bDOSCF(mstr,ior) = DOSCF(ior)
        bcoli(mstr,ior) = coli(ior)
        
        tempw(ior) = zwtemp(ior) 
        DOSCF(ior) = zwDOSCF(ior)
        tiefe(ior) = zwtief(ior)
        coli(ior) = zwcoli(ior)
       

        diff1 = bcoli(mstr,ior)-coli(ior)
        diff2 = bDOSCF(mstr,ior)-DOSCF(ior)

        bdiff1 = coli(ior)-bcoli(mstr,ior)
        bdiff2 = DOSCF(ior)-bDOSCF(mstr,ior)
                                                                       
    if(bleb(mstr,ior)>0.0)then
      coli(ior) = coli(ior)+diff1*(1.-exp(-hctau1(ior))) 
      DOSCF(ior) = DOSCF(ior)+diff2*(1.-exp(-hctau1(ior))) 
    endif
                                                                       
        if(hctau2(ior)>0.0)then 
          bcoli(mstr,ior) = bcoli(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bDOSCF(mstr,ior) = bDOSCF(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
        endif
      enddo

      ilbuhn = 0 
      endif 

      if(iwsim==2.or.iwsim==5)goto 118

!**************Erosion*******************                               
                                                                       
 1520 if(ieros==0)goto 118 

      call erosion(anze,fkm,ss,ssalg,tflie,ischic,sedh,tausc,sedss,sedalk,sedalg,sedalb              &
                          ,hgsZn,hgsCad,hgsCu,hgsNi,tiefe,rau,vmitt,mstr,ilbuhn,ischwer,ilang,azStrs &
                          ,iwied,ianze_max                                                           &
                          ,.false.,0)     !!wy kontroll,iglob                                           

      if(ilbuhn==0)then

      do ior = 1,anze+1 
        sedhg(mstr,ior) = sedh(ior) 
        ischig(mstr,ior) = ischic(ior)
        do itau = 1,ischig(mstr,ior)
          tauscg(mstr,ior,itau) = tausc(ior,itau) 
        enddo 
      enddo
      endif
 
       if(nbuhn(mstr).eq.0)goto 118

      if(ilbuhn.eq.0)then 
      do ior=1,anze+1 
        zwvm(ior) = vmitt(ior) 
        zwtief(ior) = tiefe(ior) 
        zwss(ior) = ss(ior) 
        zwssa(ior) = ssalg(ior)
        zwsedS(ior) = sedss(ior)
        zwsedk(ior) = sedalk(ior) 
        zwsedg(ior) = sedalg(ior) 
        zwsedb(ior) = sedalb(ior) 
        zwgsZn(ior) = hgsZn(mstr,ior)
        zwgsCad(ior) = hgsCad(mstr,ior)
        zwgsCu(ior) = hgsCu(mstr,ior)
        zwgsNi(ior) = hgsNi(mstr,ior)

      sedh(ior) = bsedh(mstr,ior) 
      ischic(ior) = ibschi(mstr,ior) 
      do itau = 1,ischic(ior)
        tausc(ior,itau) = btausc(mstr,ior,itau)
      enddo 
        tempw(ior) = btempw(mstr,ior) 
        tiefe(ior) = bh(mstr,ior) 
        vmitt(ior) = vbm(mstr,ior)
        ss(ior) = bss(mstr,ior) 
        ssalg(ior) = bssalg(mstr,ior) 
        sedalk(ior) = bsedak(mstr,ior) 
        sedalg(ior) = bsedag(mstr,ior) 
        sedalb(ior) = bsedab(mstr,ior) 
        sedss(ior) = bsedss(mstr,ior) 
        hgsZn(mstr,ior) = bgsZn(mstr,ior)
        hgsCad(mstr,ior) = bgsCad(mstr,ior) 
        hgsCu(mstr,ior) = bgsCu(mstr,ior)
        hgsNi(mstr,ior) = bgsNi(mstr,ior)
     enddo
      ilbuhn = 1 
      goto 1520 
      endif 

      if(ilbuhn==1)then 
      do ior=1,anze+1 

        bss(mstr,ior) = ss(ior)
        bssalg(mstr,ior) = ssalg(ior)

        bgsZn(mstr,ior) = hgsZn(mstr,ior)
        bgsCad(mstr,ior) = hgsCad(mstr,ior)
        bgsCu(mstr,ior) = hgsCu(mstr,ior)
        bgsNi(mstr,ior) = hgsNi(mstr,ior)

        tiefe(ior) = zwtief(ior)
        vmitt(ior) = zwvm(ior)
        ss(ior) = zwss(ior) 
        ssalg(ior) = zwssa(ior)
        sedss(ior) = zwsedS(ior)
        sedalk(ior) = zwsedk(ior)
        sedalg(ior) = zwsedg(ior)
        sedalb(ior) = zwsedb(ior)

        hgsZn(mstr,ior) = zwgsZn(ior)
        hgsCad(mstr,ior) = zwgsCad(ior)
        hgsCu(mstr,ior) = zwgsCu(ior)
        hgsNi(mstr,ior) = zwgsNi(ior)

        bsedh(mstr,ior) = sedh(ior) 
        ibschi(mstr,ior) = ischic(ior) 
        do itau = 1,ischic(ior)
          btausc(mstr,ior,itau) = tausc(ior,itau)
        enddo 

        diff1 = bssalg(mstr,ior)-ssalg(ior)
        diff2 = bss(mstr,ior)-ss(ior)
        diff3 = bfssgr(mstr,ior)-fssgr(ior)
        diff4 = bgsZn(mstr,ior)-hgsZn(mstr,ior)
        diff5 = bgsCad(mstr,ior)-hgsCad(mstr,ior)
        diff6 = bgsCu(mstr,ior)-hgsCu(mstr,ior)
        diff7 = bgsNi(mstr,ior)-hgsNi(mstr,ior)
        diff8 = bglZn(mstr,ior)-hglZn(mstr,ior)
        diff9 = bglCad(mstr,ior)-hglCad(mstr,ior)
        diff10 = bglCu(mstr,ior)-hglCu(mstr,ior)
        diff11 = bglNi(mstr,ior)-hglNi(mstr,ior)


        bdiff1 = ssalg(ior)-bssalg(mstr,ior)
        bdiff2 = ss(ior)-bss(mstr,ior)
        bdiff3 = fssgr(ior)-bfssgr(mstr,ior)                                                                     
        bdiff4 = hgsZn(mstr,ior)-bgsZn(mstr,ior)
        bdiff5 = hgsCad(mstr,ior)-bgsCad(mstr,ior)
        bdiff6 = hgsCu(mstr,ior)-bgsCu(mstr,ior)
        bdiff7 = hgsNi(mstr,ior)-bgsNi(mstr,ior)
        bdiff8 = hglZn(mstr,ior)-bglZn(mstr,ior)
        bdiff9 = hglCad(mstr,ior)-bglCad(mstr,ior)
        bdiff10 = hglCu(mstr,ior)-bglCu(mstr,ior)
        bdiff11= hglNi(mstr,ior)-bglNi(mstr,ior)

    if(bleb(mstr,ior)>0.0)then
      ssalg(ior) = ssalg(ior)+diff1*(1.-exp(-hctau1(ior))) 
      ss(ior) = ss(ior)+diff2*(1.-exp(-hctau1(ior))) 
      fssgr(ior) = fssgr(ior)+diff3*(1.-exp(-hctau1(ior))) 
      hgsZn(mstr,ior) = hgsZn(mstr,ior)+diff4*(1.-exp(-hctau1(ior))) 
      hgsCad(mstr,ior) = hgsCad(mstr,ior)+diff5*(1.-exp(-hctau1(ior))) 
      hgsCu(mstr,ior) = hgsCu(mstr,ior)+diff6*(1.-exp(-hctau1(ior))) 
      hgsNi(mstr,ior) = hgsNi(mstr,ior)+diff7*(1.-exp(-hctau1(ior))) 
      hglZn(mstr,ior) = hglZn(mstr,ior)+diff8*(1.-exp(-hctau1(ior))) 
      hglCad(mstr,ior) = hglCad(mstr,ior)+diff9*(1.-exp(-hctau1(ior))) 
      hglCu(mstr,ior) = hglCu(mstr,ior)+diff10*(1.-exp(-hctau1(ior))) 
      hglNi(mstr,ior) = hglNi(mstr,ior)+diff11*(1.-exp(-hctau1(ior))) 
    endif
                                                                       
        if(hctau2(ior)>0.0)then 
          bssalg(mstr,ior) = bssalg(mstr,ior)+bdiff1*(1.-exp(-hctau2(ior))) 
          bss(mstr,ior) = bss(mstr,ior)+bdiff2*(1.-exp(-hctau2(ior))) 
          bfssgr(mstr,ior) = bfssgr(mstr,ior)+bdiff3*(1.-exp(-hctau2(ior))) 
          bgsZn(mstr,ior) = bgsZn(mstr,ior)+bdiff4*(1.-exp(-hctau2(ior))) 
          bgsCad(mstr,ior) = bgsCad(mstr,ior)+bdiff5*(1.-exp(-hctau2(ior))) 
          bgsCu(mstr,ior) = bgsCu(mstr,ior)+bdiff6*(1.-exp(-hctau2(ior))) 
          bgsNi(mstr,ior) = bgsNi(mstr,ior)+bdiff7*(1.-exp(-hctau2(ior))) 
          bglZn(mstr,ior) = bglZn(mstr,ior)+bdiff8*(1.-exp(-hctau2(ior))) 
          bglCad(mstr,ior) = bglCad(mstr,ior)+bdiff9*(1.-exp(-hctau2(ior))) 
          bglCu(mstr,ior) = bglCu(mstr,ior)+bdiff10*(1.-exp(-hctau2(ior))) 
          bglNi(mstr,ior) = bglNi(mstr,ior)+bdiff11*(1.-exp(-hctau2(ior))) 
        endif
      enddo

      ilbuhn = 0 
     endif 

  118 continue 
                                                                       
      if(iwsim==4.and.ilang==0.or.itracer_vor==1)then
        else
                                                                       
! #### Transport ####                                        
                                                                       
      izeits = STRiz(mstr) 
      deltat = STRdt(mstr) 
      jpoin1 = 0 

!      if(mstr==17)then
!       do ior=1,anze+1
!          write(89,*)itags,uhrz,ior,akmor_1(mstr,ior)
!        enddo
!      endif
      
      !!wy write(222,*)'vor Transport DL(',anze,')=',DL(anze),' strang=',mstr 
     
 call Transport(anze,deltat,izeits,isub_dt,isub_dt_Mac,hvmitt,elen,flag,tempw,vo2,vnh4,vno3,vno2,vx0,vx02,Si,mstr   &      
                ,gelP,obsb,ocsb,vbsb,vcsb,CHNF,BVHNF,CD,CP,CM,BAC,zooind,chla,aki,agr,abl,chlaki,chlagr             &      
                ,chlabl,vkigr,antbl,abrzo1,ssalg,ss,svhemk,svhemg,svhemb,akbcm,agbcm,abbcm,fssgr,fbsgr,frfgr,gesN   &
                ,gesP,nl0,pl0,Q_NK,Q_PK,Q_SK,Q_NG,Q_PG,Q_NB,Q_PB,stind,mw,pw,ca,lf,coli,DOSCF                       &
                ,dlarvn,vph,iph,iwsim,htempw,hgesN,hgesP,hbsb,hcsb,hCHNF,hBVHNF,hCD,hCP,hCM,hBAC,hnh4,ho2           &                                          
                ,hno3,hno2,hx0,hx02,hsi,hchla,haki,hagr,habl,hchlak,hchlag,hchlab,hvkigr,hantbl,hssalg,hss,hzooi    &
                ,hgelp,hmw,hpw,hca,hlf,hph,hdlarn,hcoli,hDOSCF,hvbsb,hvcsb,SKmor,hSKmor,iflRi,dl,Uvert,iMAC         &
                ,iwied,nkzs,tflie,jpoin1,itags,monats,Uhrz,iverfahren,azStrs,ianze_max,Qmx_NK,Qmx_NB,Qmx_NG,Qmx_PK  &
                ,Qmx_PB,Qmx_PG,hFluN3,TGZoo,akmor_1,agmor_1,abmor_1,GRote                                           &
                ,hgsZn,hglZn,hgsCad,hglCad,hgsCu,hglCu,hgsNi,hglNi,mtracer,nkztot_max,ischwer)

       !!wy write(222,*)'nach Transport DL(',anze,')=',DL(anze),' Verfahren=',ilongDis,' strang=',mstr 

!     if(mstr==17)then
!        do ior=1,anze+1
!          write(89,*)ior,ior,akmor_1(mstr,ior)
!        enddo
!      endif

!###############################
! Aufsummierung der Tracermasse
!###############################

     if(iwsim==4)then  
       do ior = 1, anze
         sumTracer = sumTracer + ((tempw(ior)+tempw(ior+1))/2.) * vabfl(ior)
       enddo
                                                                       
!       if(azStr==azStrs)write(89,*)itags,uhrz,sumTracer
     endif                                                                       
!#########################################

!      if(iwsim.eq.4)goto 53 
                                                                       

!******* vertikaler Transport ******

      if(I2Ds(mstr)==0.or.iwsim==4)then 
        else
!         courmx = 0.0
          
          
         do ior = 1,anze+1
           if(nkzs(ior)==1)cycle
           i_windP = 1
 
           call van_Veen(rau,tiefe,hvmitt,nkzs,dH2D,zf,xU,dvdz,WGe,IDWe,mstr,ior,hconus,hconub,Uvert,Wlage,hWS,i_windP  &
                         ,azStrs)                                                       

         enddo
      endif          

     if(I2Ds(mstr)==0.or.iwsim==4)then
       do ior=1,anze+1
         tempwz(1,ior) = tempw(ior)
         vnh4z(1,ior) = vnh4(ior) 
         vno2z(1,ior) = vno2(ior) 
         vno3z(1,ior) = vno3(ior) 
         vo2z(1,ior) = vo2(ior) 
         gelPz(1,ior) = gelp(ior) 
         Siz(1,ior) = Si(ior) 
         akiz(1,ior) = aki(ior) 
         agrz(1,ior) = agr(ior) 
         ablz(1,ior) = abl(ior) 
         chlaz(1,ior) = chla(ior) 
         hchlkz(mstr,1,ior) = chlaki(ior)
         hchlgz(mstr,1,ior) = chlagr(ior)
         hchlbz(mstr,1,ior) = chlabl(ior)
         hgesPz(mstr,1,ior) = gesP(ior)
         hgesNz(mstr,1,ior) = gesN(ior)
         hQ_NKz(mstr,1,ior) = Q_NK(ior)
         hQ_NBz(mstr,1,ior) = Q_NB(ior)
         hQ_NGz(mstr,1,ior) = Q_NG(ior) 
         hCChlkz(mstr,1,ior) = akbcm(ior)
         hCChlbz(mstr,1,ior) = abbcm(ior)
         hCChlgz(mstr,1,ior) = agbcm(ior)
       enddo  
         else

      call Transportz(anze,deltat,izeits,isub_dt,isub_dt_Mac,dtmin_Mac,hvmitt,elen,flag         &
          ,tempwz,vnh4z,vno2z,vno3z,vo2z,gelPz,Siz,akiz,agrz                                    &
          ,ablz,chlaz,hgesPz,hgesNz,nkzs,dH2D,i2Ds,iwsim,mstr                                   &
          ,htempz,ho2z,hnh4z,hno2z,hno3z,hgelPz,hSiz,hQ_NKz,hQ_NBz,hQ_NGz                       &
          ,hakiz,hagrz,hablz,hchlaz,hchlkz,hchlgz,hchlbz,hCChlkz,hCChlbz,hCChlgz                &
          ,iflRi,dl,iMAC,Uvert,tflie,jpoin1,itags,monats,iwied,uhrz,iverfahren                  &
          ,azStrs,ianze_max,nkztot_max,Qmx_NK,Qmx_NB,Qmx_NG,mtracer)
      endif
 
!   **** k_eps *****

      
      if(I2Ds(mstr)==0.or.iwsim==4)then
        else 
      do ior = 1,anze+1 
        if(nkzs(ior)==1)cycle 

      call       k_eps(tempwz,tiefe,hvmitt,rau,dH2D,nkzs,tflie,dtemp,IDWe,WGe,mstr,Dz2D,ior,vo2z,hJO2      &                            
                       ,dO2o2D,vz1,vNH4z,vNO2z,vNO3z,hJPO4,hJSi,hJNH4,hJNO3,vx02,gelPz,Siz,iwied,uhrz      &
                       ,FluxT1,akiz,agrz,ablz,chlaz,hchlkz,hchlgz,hchlbz,hgesPz,hgesNz,orgCsd0,pl0,nl0     &
                       ,gesP,gesN,sedalk0,sedalb0,sedalg0,aki,abl,agr,Q_PK,Q_PB,Q_PG,hQ_NKz,hQ_NBz,hQ_NGz  &           
                       ,hCChlkz,hCChlbz,hCChlgz,Q_NK,Q_NB,Q_NG,Qmx_NK,Qmx_NB,Qmx_NG,akbcm,abbcm,agbcm,fkm  &
                       ,Wlage,hWS,itags, monats,azStrs)


         call Masse_neu_Qsim(ior,nkzs,akiz,aki,ablz,abl,agrz,agr,vo2z,vo2,vnh4z,vnh4,vno2z,vno2,vno3z,vno3,gelPz,gelP,Siz,Si              &
                            ,chlaz,chla,hchlkz,chlaki,hchlbz,chlabl,hchlgz,chlagr,hgesPz,gesP,hgesNz,gesN,dH2D,hCChlkz,akbcm              &
                            ,hCChlbz,abbcm,hCChlgz,agbcm,mstr,azStrs,Caki,Cabl,Cagr)

        hDz2D(mstr,ior) = Dz2D(ior) 

      enddo                                                                     
                                                                       
     endif
    endif

    itracer_vor = 0


!.....Mittelwertbildung des Sauerstoffgehalts des Chla-Gehaltes der     
!.....Algenbiomassen und der Naehrstoffe bei 2D-Modellierung            

      do ior = 1,anze+1 

        if(nkzs(ior)==1)cycle 

        sT = 0.0 
        sO2 = 0.0 
        ski = 0.0 
        sgr = 0.0 
        sbl = 0.0 
        schl = 0.0 
        sN4 = 0.0 
        sN2 = 0.0 
        sN3 = 0.0 
        sP = 0.0
        sPges = 0.0
        sNges = 0.0 
        sSi = 0.0 
        sumH = 0.0 

      do nkz = 1,nkzs(ior)-1
        sT = sT+((tempwz(nkz,ior)+tempwz(nkz+1,ior))/2.)*dH2D 
        so2 = so2+((vo2z(nkz,ior)+vo2z(nkz+1,ior))/2.)*dH2D 
        ski = ski+((akiz(nkz,ior)+akiz(nkz+1,ior))/2.)*dH2D 
        sgr = sgr+((agrz(nkz,ior)+agrz(nkz+1,ior))/2.)*dH2D 
        sbl = sbl+((ablz(nkz,ior)+ablz(nkz+1,ior))/2.)*dH2D 
        schl = schl+((chlaz(nkz,ior)+chlaz(nkz+1,ior))/2.)*dH2D 
        sN4 = sN4+((vNH4z(nkz,ior)+vNH4z(nkz+1,ior))/2.)*dH2D 
        sN2 = sN2+((vNO2z(nkz,ior)+vNO2z(nkz+1,ior))/2.)*dH2D 
        sN3 = sN3+((vNO3z(nkz,ior)+vNO3z(nkz+1,ior))/2.)*dH2D 
        sP = sP+((gelPz(nkz,ior)+gelPz(nkz+1,ior))/2.)*dH2D 
        sPges = sPges+((hgesPz(mstr,nkz,ior)+hgesPz(mstr,nkz+1,ior))/2.)*dH2D 
        sNges = sNges+((hgesNz(mstr,nkz,ior)+hgesNz(mstr,nkz+1,ior))/2.)*dH2D 
        sSi = sSi+((Siz(nkz,ior)+Siz(nkz+1,ior))/2.)*dH2D 
        sumH = sumH+dH2D 
      enddo

!      tempw(ior) = sT/sumH   
!      vo2(ior) = so2/sumH 
!      aki(ior) = ski/sumH 
!      agr(ior) = sgr/sumH 
!      abl(ior) = sbl/sumH 
!      chla(ior) = schl/sumH 
!      vNH4(ior) = sN4/sumH 
!      vNO2(ior) = sN2/sumH 
!      vNO3(ior) = sN3/sumH 
!      gelP(ior) = sP/sumH
!      gesP(ior) = sPges/sumH 
!      Si(ior) = sSi/sumH 
   enddo 
                                                                       
!.....Belegung des letzten Knoten bei nicht transportierten Groessen bzw
!....(Buhnenfelder)                                                     
!                                                                       
   53 Pfl(anze+1) = Pfl(anze) 
      do ndr = 1,nndr 
        zdrei(anze+1,ndr) = zdrei(anze,ndr) 
        zdreis(anze+1,ndr) = zdreis(anze,ndr) 
        gewdr(anze+1,ndr) = gewdr(anze,ndr)
      enddo 

      abegm2(anze+1) = abegm2(anze) 
      abekm2(anze+1) = abekm2(anze) 
      do ico = 1,5 
        coro(anze+1,ico) = coro(anze,ico) 
        coros(anze+1,ico) = coros(anze,ico) 
      enddo
                                                                       
!.... Sedimentgroessen                                                  
      sedh(anze+1) = sedh(anze) 
!                                                                       
!     Prozessraten                                                      
!                                                                       
      do 83 ndr=1,nndr 
      idras(anze+1,ndr) = idras(anze,ndr) 
      drmas(anze+1,ndr) = drmas(anze,ndr) 
      drakr(anze+1,ndr) = drakr(anze,ndr) 
      drbar(anze+1,ndr) = drbar(anze,ndr) 
      Rzuwdr(anze+1,ndr) = Rzuwdr(anze,ndr) 
      drmor(anze+1,ndr) = drmor(anze,ndr) 
   83 continue 
!                                                                       
      susn(anze+1) = susn(anze) 
      bettn(anze+1) = bettN(anze) 
      hFluN3(mstr,anze+1) = hFluN3(mstr,anze) 
      hJNO3(mstr,anze+1) = hJNO3(mstr,anze) 
      hJNH4(mstr,anze+1) = hJNH4(mstr,anze) 
      hJPO4(mstr,anze+1) = hJPO4(mstr,anze) 
      hJO2(mstr,anze+1) = hJO2(mstr,anze) 
      don(anze+1) = don(anze) 
      agrnh4(anze+1) = agrnh4(anze) 
      akinh4(anze+1) = akinh4(anze) 
      ablnh4(anze+1) = ablnh4(anze) 
      agrNO3(anze+1) = agrNO3(anze) 
      akiNO3(anze+1) = akiNO3(anze) 
      ablNO3(anze+1) = ablNO3(anze) 
      sedx0(anze+1) = sedx0(anze) 
      susno(anze+1) = susno(anze) 
      sedalg(anze+1) = sedalg(anze) 
      sedalk(anze+1) = sedalk(anze) 
      sedalb(anze+1) = sedalb(anze) 
      algzog(anze+1) = algzog(anze) 
      algzok(anze+1) = algzok(anze) 
      algzob(anze+1) = algzob(anze) 
      algdrg(anze+1) = algdrg(anze) 
      algdrk(anze+1) = algdrk(anze) 
      algdrb(anze+1) = algdrb(anze) 
      algcog(anze+1) = algcog(anze) 
      algcok(anze+1) = algcok(anze) 
      algcob(anze+1) = algcob(anze) 
      volfdr(anze+1) = volfdr(anze) 
      drpfec(anze+1) = drpfec(anze) 
      abeowg(anze+1) = abeowg(anze) 
      abeowk(anze+1) = abeowk(anze) 
      abeorg(anze+1) = abeorg(anze) 
      abeork(anze+1) = abeork(anze) 
      dalggr(anze+1) = dalggr(anze) 
      dalgki(anze+1) = dalgki(anze) 
      dalgbl(anze+1) = dalgbl(anze) 
      dalgag(anze+1) = dalgag(anze) 
      dalgak(anze+1) = dalgak(anze) 
      dalgab(anze+1) = dalgab(anze) 
      dgrmor(anze+1) = dgrmor(anze) 
      dkimor(anze+1) = dkimor(anze) 
      dblmor(anze+1) = dblmor(anze) 
      sgo2n(anze+1) = sgo2n(anze) 
      sdbsb(anze+1) = sdbsb(anze) 
      so2ein(anze+1) = so2ein(anze) 
      salgo(anze+1) = salgo(anze) 
      bsbt(anze+1) = bsbt(anze) 
      dalgo(anze+1) = dalgo(anze) 
      dalgao(anze+1) = dalgao(anze) 
      hschlr(mstr,anze+1) = hschlr(mstr,anze) 
      bsbbet(anze+1) = bsbbet(anze) 
      o2ein1(anze+1) = o2ein1(anze) 
      ro2dr(anze+1) = ro2dr(anze) 
      zooro2(anze+1) = zooro2(anze) 
      po2p(anze+1) = po2p(anze) 
      po2r(anze+1) = po2r(anze) 
      iras(anze+1) = iras(anze) 
      rmuas(anze+1) = rmuas(anze) 
      rakr(anze+1) = rakr(anze) 
      rbar(anze+1) = rbar(anze) 
      akmuea(anze+1) = akmuea(anze) 
      agmuea(anze+1) = agmuea(anze) 
      abmuea(anze+1) = abmuea(anze) 
      ftaaus(anze+1) = ftaaus(anze) 
      fiaus(anze+1) = fiaus(anze) 
      figaus(anze+1) = figaus(anze) 
      fibaus(anze+1) = fibaus(anze) 
      fheaus(anze+1) = fheaus(anze) 
      fhegas(anze+1) = fhegas(anze) 
      fhebas(anze+1) = fhebas(anze) 
      akraus(anze+1) = akraus(anze) 
      agreau(anze+1) = agreau(anze) 
      abreau(anze+1) = abreau(anze) 
      HNFmua(anze+1) = HNFmua(anze) 
      HNFrea(anze+1) = HNFrea(anze) 
      HNFupa(anze+1) = HNFupa(anze) 
      HNFmoa(anze+1) = HNFmoa(anze) 
      HNFexa(anze+1) = HNFexa(anze) 
      HNFdra(anze+1) = HNFdra(anze) 
      HNFza(anze+1) = HNFza(anze) 
      HNFBAC(anze+1) = HNFBAC(anze) 
      BACmua(anze+1) = BACmua(anze) 
      ffood(anze+1) = ffood(anze) 
      extk(anze+1) = extk(anze) 
!...hier auch Belegung für alle anderen Groessen!!!                     
!                                                                       
!..Buhnenfelder                                                         
      if(nbuhn(mstr).eq.0)goto 981 
      bzooi(mstr,anze+1) = bzooi(mstr,anze) 
      bvkigr(mstr,anze+1) = bvkigr(mstr,anze) 
      bchlak(mstr,anze+1) = bchlak(mstr,anze) 
      bchlag(mstr,anze+1) = bchlag(mstr,anze) 
      bchla(mstr,anze+1) = bchla(mstr,anze) 
      baki(mstr,anze+1) = baki(mstr,anze) 
      bagr(mstr,anze+1) = bagr(mstr,anze) 
      bbsb(mstr,anze+1) = bbsb(mstr,anze) 
      bcsb(mstr,anze+1) = bcsb(mstr,anze) 
      bvbsb(mstr,anze+1) = bvbsb(mstr,anze) 
      bvcsb(mstr,anze+1) = bvcsb(mstr,anze) 
      bCD(mstr,1,anze+1) = bCD(mstr,1,anze) 
      bCD(mstr,2,anze+1) = bCD(mstr,2,anze) 
      bCP(mstr,1,anze+1) = bCP(mstr,1,anze) 
      bCP(mstr,2,anze+1) = bCP(mstr,2,anze) 
      bCM(mstr,anze+1) = bCM(mstr,anze) 
      bBAC(mstr,anze+1) = bBAC(mstr,anze) 
      bfbsgr(mstr,anze+1) = bfbsgr(mstr,anze) 
      bfrfgr(mstr,anze+1) = bfrfgr(mstr,anze) 
      bx0(mstr,anze+1) = bx0(mstr,anze) 
      bx02(mstr,anze+1) = bx02(mstr,anze) 
      bnh4(mstr,anze+1) = bnh4(mstr,anze) 
      bno2(mstr,anze+1) = bno2(mstr,anze) 
      bno3(mstr,anze+1) = bno3(mstr,anze) 
      bnl0(mstr,anze+1) = bnl0(mstr,anze) 
      bgesN(mstr,anze+1) = bgesN(mstr,anze) 
      bmw(mstr,anze+1) = bmw(mstr,anze) 
      bpw(mstr,anze+1) = bpw(mstr,anze) 
      bca(mstr,anze+1) = bca(mstr,anze) 
      blf(mstr,anze+1) = blf(mstr,anze) 
      bph(mstr,anze+1) = bph(mstr,anze) 
      bstind(mstr,anze+1) = bstind(mstr,anze) 
      btempw(mstr,anze+1) = btempw(mstr,anze) 
      bgelp(mstr,anze+1) = bgelp(mstr,anze) 
      bpl0(mstr,anze+1) = bpl0(mstr,anze) 
      bgesP(mstr,anze+1) = bgesP(mstr,anze) 
      bsi(mstr,anze+1) = bsi(mstr,anze) 
      bo2(mstr,anze+1) = bo2(mstr,anze) 
      bss(mstr,anze+1) = bss(mstr,anze) 
      bssalg(mstr,anze+1) = bssalg(mstr,anze) 
      bfssgr(mstr,anze+1) = bfssgr(mstr,anze) 
!                                                                       
      bsvhek(mstr,anze+1) = bsvhek(mstr,anze) 
      bsvheg(mstr,anze+1) = bsvheg(mstr,anze) 
      bsvheb(mstr,anze+1) = bsvheb(mstr,anze) 
      bakbcm(mstr,anze+1) = bakbcm(mstr,anze) 
      bagbcm(mstr,anze+1) = bagbcm(mstr,anze) 
      babbcm(mstr,anze+1) = babbcm(mstr,anze) 
!hier Prozessraten                                                      
!                                                                       
      bdaki(mstr,anze+1) = bdaki(mstr,anze) 
      bdagr(mstr,anze+1) = bdagr(mstr,anze) 
      bdabl(mstr,anze+1) = bdabl(mstr,anze) 
      bdaak(mstr,anze+1) = bdaak(mstr,anze) 
      bdaag(mstr,anze+1) = bdaag(mstr,anze) 
      bdaab(mstr,anze+1) = bdaab(mstr,anze) 
      bsedak(mstr,anze+1) = bsedak(mstr,anze) 
      bsedag(mstr,anze+1) = bsedag(mstr,anze) 
      bsedab(mstr,anze+1) = bsedab(mstr,anze) 
      bazok(mstr,anze+1) = bazok(mstr,anze) 
      bazog(mstr,anze+1) = bazog(mstr,anze) 
      bazob(mstr,anze+1) = bazob(mstr,anze) 
      bdkmor(mstr,anze+1) = bdkmor(mstr,anze) 
      bdgmor(mstr,anze+1) = bdgmor(mstr,anze) 
      bdbmor(mstr,anze+1) = bdbmor(mstr,anze) 
      bvkigr(mstr,anze+1) = bvkigr(mstr,anze) 
      bantbl(mstr,anze+1) = bantbl(mstr,anze) 
      bakbcm(mstr,anze+1) = bakbcm(mstr,anze) 
      bir(mstr,anze+1) = bir(mstr,anze) 
      bsised(mstr,anze+1) = bsised(mstr,anze) 
      badrk(mstr,anze+1) = badrk(mstr,anze) 
      badrg(mstr,anze+1) = badrg(mstr,anze) 
      badrb(mstr,anze+1) = badrb(mstr,anze) 
      bacok(mstr,anze+1) = bacok(mstr,anze) 
      bacog(mstr,anze+1) = bacog(mstr,anze) 
      bacob(mstr,anze+1) = bacob(mstr,anze) 
!                                                                       
      bakmua(mstr,anze+1) = bakmua(mstr,anze) 
      bagmua(mstr,anze+1) = bagmua(mstr,anze) 
      babmua(mstr,anze+1) = babmua(mstr,anze) 
      bftaau(mstr,anze+1) = bftaau(mstr,anze) 
      bfiaus(mstr,anze+1) = bfiaus(mstr,anze) 
      bfigas(mstr,anze+1) = bfigas(mstr,anze) 
      bfibas(mstr,anze+1) = bfibas(mstr,anze) 
      bfheau(mstr,anze+1) = bfheau(mstr,anze) 
      bfhgau(mstr,anze+1) = bfhgau(mstr,anze) 
      bfhbau(mstr,anze+1) = bfhbau(mstr,anze) 
      bakrau(mstr,anze+1) = bakrau(mstr,anze) 
      bagrau(mstr,anze+1) = bagrau(mstr,anze) 
      babrau(mstr,anze+1) = babrau(mstr,anze) 
      btpki(mstr,anze+1) = btpki(mstr,anze) 
      btpgr(mstr,anze+1) = btpgr(mstr,anze) 
      btpbl(mstr,anze+1) = btpbl(mstr,anze) 
      bextk(mstr,anze+1) = bextk(mstr,anze) 
      bFluN3(mstr,anze+1) = bFluN3(mstr,anze) 
      bJNO3(mstr,anze+1) = bJNO3(mstr,anze) 
!                                                                       
!                                                                       
  981 dl(anze+1) = dl(anze) 
      if(nbuhn(mstr).gt.0)tau2(anze+1) = tau2(anze) 
!                                                                       
!                                                                       
!  Speichern von Informationen am Stranganfang und Strangende   ++Hier          
!                                                                       
!                                                                       
  793 ianze(mstr) = anze+1 
      inkzs(mstr,1) = nkzs(1) 
      inkzs(mstr,2) = nkzs(anze+1) 
      ikanz(1) = 1 
      ikanz(2) = ianze(mstr) 
                                                                       
                                                                       
!       Umspeichern des ersten und letzten Gitterpunkts eines jeden Strangs
   

      do ke = 1,2                       ! ke = 1: erster Gitterpunkt; ke = 2: letzter Gitterpunkt   
        do nkz = 1,inkzs(mstr,ke) 
          tzt(mstr,nkz,ke) = tempwz(nkz,ikanz(ke)) 
          o2zt(mstr,nkz,ke) = vo2z(nkz,ikanz(ke)) 
          NH4zt(mstr,nkz,ke) = vNH4z(nkz,ikanz(ke)) 
          NO2zt(mstr,nkz,ke) = vNO2z(nkz,ikanz(ke)) 
          NO3zt(mstr,nkz,ke) = vNO3z(nkz,ikanz(ke)) 
          Pzt(mstr,nkz,ke) = gelPz(nkz,ikanz(ke)) 
          gSizt(mstr,nkz,ke) = Siz(nkz,ikanz(ke)) 
          akizt(mstr,nkz,ke) = akiz(nkz,ikanz(ke)) 
          agrzt(mstr,nkz,ke) = agrz(nkz,ikanz(ke)) 
          ablzt(mstr,nkz,ke) = ablz(nkz,ikanz(ke)) 
          chlazt(mstr,nkz,ke) = chlaz(nkz,ikanz(ke)) 
          chlkzt(mstr,nkz,ke) = hchlkz(mstr,nkz,ikanz(ke)) 
          chlgzt(mstr,nkz,ke) = hchlgz(mstr,nkz,ikanz(ke)) 
          chlbzt(mstr,nkz,ke) = hchlbz(mstr,nkz,ikanz(ke)) 
          gesPzt(mstr,nkz,ke) = hgesPz(mstr,nkz,ikanz(ke)) 
          gesNzt(mstr,nkz,ke) = hgesNz(mstr,nkz,ikanz(ke)) 
          Q_NKzt(mstr,nkz,ke) = hQ_NKz(mstr,nkz,ikanz(ke)) 
          Q_NBzt(mstr,nkz,ke) = hQ_NBz(mstr,nkz,ikanz(ke)) 
          Q_NGzt(mstr,nkz,ke) = hQ_NGz(mstr,nkz,ikanz(ke)) 
          CChlkzt(mstr,nkz,ke) = hCChlkz(mstr,nkz,ikanz(ke)) 
          CChlbzt(mstr,nkz,ke) = hCChlbz(mstr,nkz,ikanz(ke)) 
          CChlgzt(mstr,nkz,ke) = hCChlgz(mstr,nkz,ikanz(ke)) 
         enddo 
      enddo
                                                                    
      do ior = 1,hanze(mstr)+1    ! Beginn Hauptschleife
                                                                       
      hfkm(mstr,ior) = fkm(ior) 
      hsvhk(mstr,ior) = svhemk(ior) 
      hsvhg(mstr,ior) = svhemg(ior) 
      hsvhb(mstr,ior) = svhemb(ior) 
      hDOSCF(mstr,ior) = DOSCF(ior) 
      hakbcm(mstr,ior) = akbcm(ior)
      hagbcm(mstr,ior) = agbcm(ior)
      habbcm(mstr,ior) = abbcm(ior)
      hfssgr(mstr,ior) = fssgr(ior) 
      hfbsgr(mstr,ior) = fbsgr(ior) 
      hfrfgr(mstr,ior) = frfgr(ior) 
      hnl0(mstr,ior) = nl0(ior) 
      hpl0(mstr,ior) = pl0(ior) 
      hQ_NK(mstr,ior) = Q_NK(ior) 
      hQ_PK(mstr,ior) = Q_PK(ior) 
      hQ_SK(mstr,ior) = Q_SK(ior) 
      hQ_NG(mstr,ior) = Q_NG(ior) 
      hQ_PG(mstr,ior) = Q_PG(ior) 
      hQ_NB(mstr,ior) = Q_NB(ior) 
      hQ_PB(mstr,ior) = Q_PB(ior) 
      htempw(mstr,ior) = tempw(ior) 
      hTsed(mstr,ior) = Tsed(ior) 
      hbsb(mstr,ior) = obsb(ior) 
      hcsb(mstr,ior) = ocsb(ior) 
                                                                       
      hCHNF(mstr,ior) = CHNF(ior) 
      hBVHNF(mstr,ior) = BVHNF(ior) 
      hCD(mstr,1,ior) = CD(1,ior) 
      hCD(mstr,2,ior) = CD(2,ior) 
      hCP(mstr,1,ior) = CP(1,ior) 
      hCP(mstr,2,ior) = CP(2,ior) 
      hCM(mstr,ior) = CM(ior) 
      hBAC(mstr,ior) = BAC(ior) 
                                                                       
      hnh4(mstr,ior) = vnh4(ior) 
      ho2(mstr,ior) = vo2(ior) 
      hno3(mstr,ior) = vno3(ior) 
      hno2(mstr,ior) = vno2(ior) 
      hgesN(mstr,ior) = gesN(ior) 
      hx0(mstr,ior) = vx0(ior) 
      hx02(mstr,ior) = vx02(ior) 
      hsi(mstr,ior) = si(ior) 
      hchla(mstr,ior) = chla(ior) 
      haki(mstr,ior) = aki(ior) 
      hagr(mstr,ior) = agr(ior) 
      habl(mstr,ior) = abl(ior) 
      hchlak(mstr,ior) = chlaki(ior) 
      hchlag(mstr,ior) = chlagr(ior) 
      hchlab(mstr,ior) = chlabl(ior) 
      hvkigr(mstr,ior) = vkigr(ior) 
      hantbl(mstr,ior) = antbl(ior) 
      htpki(mstr,ior) = tpki(ior) 
      htpgr(mstr,ior) = tpgr(ior) 
      htpbl(mstr,ior) = tpbl(ior) 
      habrz1(mstr,ior) = abrzo1(ior) 
      hssalg(mstr,ior) = ssalg(ior) 
      hss(mstr,ior) = ss(ior) 
      hzooi(mstr,ior) = zooind(ior) 
      hgelp(mstr,ior) = gelp(ior) 
      hgesP(mstr,ior) = gesP(ior) 
      hmw(mstr,ior) = mw(ior) 
      hpw(mstr,ior) = pw(ior) 
      hca(mstr,ior) = ca(ior) 
      hlf(mstr,ior) = lf(ior) 
      hcoli(mstr,ior) = coli(ior) 
      hdlarn(mstr,ior) = dlarvn(ior) 
      hph(mstr,ior) = vph(ior) 
      hvbsb(mstr,ior) = vbsb(ior) 
      hvcsb(mstr,ior) = vcsb(ior) 
      hstind(mstr,ior) = stind(ior) 
!                                                                       
      hpfmnl(mstr,ior) = pflmin(ior) 
      hpfmxl(mstr,ior) = pflmax(ior) 
      hpfl(mstr,ior) = pfl(ior) 
      hischf(mstr,ior) = ischif(ior) 

        do ndr = 1,nndr 
          hzdrel(mstr,ior,ndr) = zdrei(ior,ndr) 
          hzdrsl(mstr,ior,ndr) = zdreis(ior,ndr) 
          hgwdrl(mstr,ior,ndr) = gewdr(ior,ndr) 
        enddo

      hdlmx(mstr,ior) = dlmax(ior) 
      hdlmxs(mstr,ior) = dlmaxs(ior) 
      hgwdmx(mstr,ior) = gwdmax(ior) 
      hsgwmu(mstr,ior) = sgwmue(ior) 
      habgml(mstr,ior) = abegm2(ior) 
      habkml(mstr,ior) = abekm2(ior) 

        do ico = 1,5 
          hcoro2(mstr,ior,ico) = coro(ior,ico) 
          hcos2(mstr,ior,ico) = coros(ior,ico) 
        enddo
                                                                       
!.....Ufervegetation                                                    
        Do iV = 1,14 
          VTYPH(mstr,ior,iV) = VTYP(ior,iV) 
        enddo

      VALTLH(mstr,ior) = VALTBL(ior) 
      EDUFLH(mstr,ior) = EDUFBL(ior) 
      VALTRH(mstr,ior) = VALTBR(ior) 
      EDUFRH(mstr,ior) = EDUFBR(ior) 
                                                                       
!     Prozessraten                                                      
                                                                       
      do ndr=1,nndr 
        hidras(mstr,ior,ndr) = idras(ior,ndr) 
        hdrmas(mstr,ior,ndr) = drmas(ior,ndr) 
        hdrakr(mstr,ior,ndr) = drakr(ior,ndr) 
        hdrbar(mstr,ior,ndr) = drbar(ior,ndr) 
        hRzuwd(mstr,ior,ndr) = Rzuwdr(ior,ndr) 
        hdrmor(mstr,ior,ndr) = drmor(ior,ndr) 
      enddo 
                                                                       
      hsusn(mstr,ior) = susn(ior) 
      hbettN(mstr,ior) = bettn(ior) 
      hdon(mstr,ior) = don(ior) 
      hagnh4(mstr,ior) = agrnh4(ior) 
      haknh4(mstr,ior) = akinh4(ior) 
      habnh4(mstr,ior) = ablnh4(ior) 
      halNO3(mstr,ior) = agrNO3(ior)+akiNO3(ior)+ablNO3(ior) 
      hsedx0(mstr,ior) = sedx0(ior) 
      hsusno(mstr,ior) = susno(ior) 
      hsedag(mstr,ior) = sedalg(ior) 
      hsedak(mstr,ior) = sedalk(ior) 
      hsedab(mstr,ior) = sedalb(ior) 
      halgzg(mstr,ior) = algzog(ior) 
      halgzk(mstr,ior) = algzok(ior) 
      halgzb(mstr,ior) = algzob(ior) 
      halgdg(mstr,ior) = algdrg(ior) 
      halgdk(mstr,ior) = algdrk(ior) 
      halgdb(mstr,ior) = algdrb(ior) 
      halgcg(mstr,ior) = algcog(ior) 
      halgck(mstr,ior) = algcok(ior) 
      halgcb(mstr,ior) = algcob(ior) 
      hvolfd(mstr,ior) = volfdr(ior) 
      hdrpfe(mstr,ior) = drpfec(ior) 
      habowg(mstr,ior) = abeowg(ior) 
      habowk(mstr,ior) = abeowk(ior) 
      haborg(mstr,ior) = abeorg(ior) 
      habork(mstr,ior) = abeork(ior) 
      hdalgg(mstr,ior) = dalggr(ior) 
      hdalgk(mstr,ior) = dalgki(ior) 
      hdalgb(mstr,ior) = dalgbl(ior) 
      hdalag(mstr,ior) = dalgag(ior) 
      hdalak(mstr,ior) = dalgak(ior) 
      hdalab(mstr,ior) = dalgab(ior) 
      hdgmor(mstr,ior) = dgrmor(ior) 
      hdkmor(mstr,ior) = dkimor(ior) 
      hdbmor(mstr,ior) = dblmor(ior) 
      hsgo2n(mstr,ior) = sgo2n(ior) 
      hsdbsb(mstr,ior) = sdbsb(ior) 
      hsoein(mstr,ior) = so2ein(ior) 
      hsalgo(mstr,ior) = salgo(ior) 
      hbsbt(mstr,ior)  = bsbt(ior) 
      hdalgo(mstr,ior) = dalgo(ior) 
      hdalao(mstr,ior) = dalgao(ior) 
      hSedOM(mstr,ior) = hSedOM(mstr,ior) 
!      hw2(mstr,ior) = hw2(mstr,ior) 
      hdkorn(mstr,ior) = hdkorn(mstr,ior) 
      hbsbbe(mstr,ior) = bsbbet(ior) 
      hoein1(mstr,ior) = o2ein1(ior) 
      hro2dr(mstr,ior) = ro2dr(ior) 
      hzoro2(mstr,ior) = zooro2(ior) 
      hpo2p(mstr,ior) = po2p(ior) 
      hpo2r(mstr,ior) = po2r(ior) 
      hiras(mstr,ior) = iras(ior) 
      hrmuas(mstr,ior) = rmuas(ior) 
      hrakr(mstr,ior) = rakr(ior) 
      hrbar(mstr,ior) = rbar(ior) 
      hkmuea(mstr,ior) = akmuea(ior) 
      hgmuea(mstr,ior) = agmuea(ior) 
      hbmuea(mstr,ior) = abmuea(ior) 
      hftaau(mstr,ior) = ftaaus(ior) 
      hfiaus(mstr,ior) = fiaus(ior) 
      hfigau(mstr,ior) = figaus(ior) 
      hfibau(mstr,ior) = fibaus(ior) 
      hfheau(mstr,ior) = fheaus(ior) 
      hfhega(mstr,ior) = fhegas(ior) 
      hfheba(mstr,ior) = fhebas(ior) 
      hakrau(mstr,ior) = akraus(ior) 
      hagrau(mstr,ior) = agreau(ior) 
      habrau(mstr,ior) = abreau(ior) 
      hHNFmu(mstr,ior) = HNFmua(ior) 
      hHNFre(mstr,ior) = HNFrea(ior) 
      hHNFup(mstr,ior) = HNFupa(ior) 
      hHNFmo(mstr,ior) = HNFmoa(ior) 
      hHNFex(mstr,ior) = HNFexa(ior) 
      hHNFdr(mstr,ior) = HNFdra(ior) 
      hHNFza(mstr,ior) = HNFza(ior) 
      hHNFBA(mstr,ior) = HNFBAC(ior) 
      hBAmua(mstr,ior) = BACmua(ior) 
      hffood(mstr,ior) = ffood(ior) 
      hextk(mstr,ior) = extk(ior) 
      hSKmor(mstr,ior) = SKmor(ior)
      hSised(mstr,ior) = Sised(ior) 
      hJNO3(mstr,ior) = hJNO3(mstr,ior) 
      hJNH4(mstr,ior) = hJNH4(mstr,ior) 
      hJPO4(mstr,ior) = hJPO4(mstr,ior) 
      hJO2(mstr,ior) = hJO2(mstr,ior) 
!                                                                       
      hdl(mstr,ior) = dl(ior) 
      htiefe(mstr,ior) = tiefe(ior) 
!                                                                       
      if(iwsim/=4)then        ! wird bei Tracer übersprungen 
                                                        
!  2D-Modellierung                                                      
      hnkzs(mstr,ior) = nkzs(ior)
      hdH2de(mstr,ior) = dH2De(ior) 

      do nkz=1,nkzs(ior) 
        htempz(mstr,nkz,ior) = tempwz(nkz,ior) 
        hnh4z(mstr,nkz,ior) = vnh4z(nkz,ior) 
        hno2z(mstr,nkz,ior) = vno2z(nkz,ior) 
        hno3z(mstr,nkz,ior) = vno3z(nkz,ior) 
        ho2z(mstr,nkz,ior) = vo2z(nkz,ior) 
        hgelPz(mstr,nkz,ior) = gelPz(nkz,ior) 
        hSiz(mstr,nkz,ior) = Siz(nkz,ior) 
        hakiz(mstr,nkz,ior) = akiz(nkz,ior)
        hagrz(mstr,nkz,ior) = agrz(nkz,ior) 
        hablz(mstr,nkz,ior) = ablz(nkz,ior) 
        hchlaz(mstr,nkz,ior) = chlaz(nkz,ior)
      enddo 
    endif

    znkzs(mstr,ior) = hnkzs(mstr,ior) 
                                                                       
  enddo ! Ende Hauptschleife
                                                                       
 8888 continue   !### Ende Strangschleife####
                                                                       
 7777 continue 

! ### iwied = 0 : allererster Zeitschritt, danach iwied = 1 ###                  
! ### ilang = 0 : Vorlauf (1d) wird nicht abgelegt, danach ilang = 1 ###       

      if(iwied==0)then 
        itagv = itags 
        monatv = monats 
        jahrv = jahrs 
        uhrsv = uhrs
        iwied = 1 
      endif 
                                                                       
      if(jlauf==0)then ! Berechnung der neuen Uhrzeit und des neuen Datums
                  
        Uhrz = Uhrz+tflie*24. 
        if((24.-Uhrz)<0.0001)Uhrz = 24. 
        if(Uhrz>=24.)then 
          Uhrz = Uhrz-24. 
          if(jtag/=1)itags = itags+1 
        endif 
                                                                       
      call anztag(monats,jahrs,jtage) 

      if(itags>jtage)then 
        itags = 1 
        monats = monats+1 
      endif 

      if(monats>12)then 
        monats = 1. 
        jahrs = jahrs+1 
      endif 
    endif 

! ##### Ende Datumsberechnung #####                                              
!                                                                       
!....Vorlauf ilang = 0; Werte werden nicht abgelegt!!                   


      if(ilang==0.and.ij<itime)then 
        ij = ij+1 
        istr = 0 
        goto 9191  ! Beim Vorlauf werden keine neuen Randwerte gelesen
      endif 
                                                                        
      if(ilang==0.and.ij==itime)then 
        itracer_vor = 0
        itags = itagv 
        monats = monatv 
        jahrs = jahrv 
        itime = itimea 
        ianfan = 1 
        ij = 1
        if(iwsim==4)itracer_vor = 1 
        rewind(110)
        read(110,'(A2)')ckenn_vers1 
        if(ckenn_vers1/='*V')then
          read(110,'(A40)')ERENAME
            else
              rewind(110)
              read(110,'(2x)')
              read(110,'(2x)') 
              read(110,'(2x)') 
              read(110,'(A40)')MODNAME
              read(110,'(A40)')ERENAME 
              read(110,'(2x)') 
              read(110,'(2x)') 
        endif  

        do  ! Suchen des Ereignisbeginns in ereigh.txt
          read(110,9708)SCHRNR,itag_Schr, monat_Schr, Jahr_Schr, Uhrz_Schr  ! Lesen der Zeitschritt-Nummer 

          if(itags==itag_Schr.and.monats==monat_Schr.and.Jahrs==Jahr_Schr.and.uhrsv==Uhrz_Schr)then
            backspace(unit=110)
            exit
          endif
          do i=1,isumAnzSta
             read(110,'(2x)')
          enddo
          cycle 
        enddo
                                                                       
        rewind (97)
        read(97,'(A2)')ckenn_vers1
        if(ckenn_vers1/='*V')then
          read(97,'(A40)')ERENAME 
          read(97,'(I5)')SCHRNR 
            else
              read(97,'(A40)')MODNAME 
              read(97,'(A40)')ERENAME 
!              read(97,'(I5)')SCHRNR 
        endif

        do  ! Suchen des Ereignisbeginns in Ablauf.txt
        read(97,9705,iostat=read_error)SCHRNR,jkenn,itags_Schr, monat_Schr, Jahr_Schr, Uhrz_Schr  ! Lesen der Zeitschritt-Nummer 
        if(jkenn==99)then
          if(itags==itags_Schr.and.monats==monat_Schr.and.Jahrs==Jahr_Schr.and.uhrsv==Uhrz_Schr)exit
          cycle
        endif
        enddo
      endif 

      if(ij==1.and.jlauf==0)then 
        if(itags==itage.and.monats==monate.and.jahrs==jahre)itime = itimee                                                    
      endif 

      if(ilang==1.and.jlauf==0)then 
        jlauf = 1   
        istr = 0 
        goto 9999      ! Lesen neuer Randbedingungen
      endif  
                                                          
      if(ilang==0)then 
      ilang = 1 
      jtag = 1 
      goto 9191  ! Es werden keine neuen Randwerte gelesen
      endif 

      if(jlauf==1)then 
        ij = ij+1 
        jlauf = 0 
        jtag = 0 
        hconU = abs(uhren-uhrz) 
        if(hconU.lt.0.001)Uhrz = uhren 
        if(itags.eq.itage.and.monats.eq.monate.and.jahrs.eq.jahre.and.uhren.eq.uhrz.and.ilang.eq.1)iend = 1                        
      endif 
                                                                       
      if(ij>itime)maus = 1 

! ########################################################                                                                       
!     Summenbildung Minimum und Maximum beim Hauptlauf                  
!     Ausschreiben von Ergebnissen                                      
! ########################################################

      if(ij>2)goto 247 
                                                                       
      do 94 azStr = 1,azStrs
      mstr = mstra(azStr) 
      do 91 ior=1,mStas(mstr) 
      mitemp(mstr,ior) = 999999.9 
      mxtemp(mstr,ior) = 0.0 
      minh4(mstr,ior) = 999999.9 
      mxnh4(mstr,ior) = 0.0 
      migsP(mstr,ior) = 99999.9 
      migsN(mstr,ior) = 99999.9 
      mxgsP(mstr,ior) = 0.0 
      mxgsN(mstr,ior) = 0.0 
      mib5(mstr,ior) = 999999.9 
      mxb5(mstr,ior) = 0.0 
      mics(mstr,ior) = 999999.9 
      mxcs(mstr,ior) = 0.0 
      michla(mstr,ior) = 9999999.9 
      mxchla(mstr,ior) = 0.0 
      miaki(mstr,ior) = 999999.9 
      mxaki(mstr,ior) = 0.0 
      miagr(mstr,ior) = 999999.9 
      mxagr(mstr,ior) = 0.0 
      miabl(mstr,ior) = 999999.9 
      mxabl(mstr,ior) = 0.0 
      mio2(mstr,ior) = 9999999.9 
      mxo2(mstr,ior) = 0.0 
      mizo(mstr,ior) = 99999999.9 
      mxzo(mstr,ior) = 0.0 
      mivph(mstr,ior) = 9999999.9 
      mxvph(mstr,ior) = 0.0 
      mivno3(mstr,ior) = 999999.9 
      mxvno3(mstr,ior) = 0.0 
      migp(mstr,ior) = 999999.9 
      mxgp(mstr,ior) = 0.0 
      misi(mstr,ior) = 999999.9 
      mxsi(mstr,ior) = 0.0 
      mivno2(mstr,ior) = 999999.9 
      mxvno2(mstr,ior) = 0.0 
      mica(mstr,ior) = 9999999.9 
      mxca(mstr,ior) = 0.0 
      mimw(mstr,ior) = 999999.9 
      mxmw(mstr,ior) = 0.0 
      milf(mstr,ior) = 999999.9 
      mxlf(mstr,ior) = 0.0 
      micoli(mstr,ior) = 9999999999999999.9 
      mxcoli(mstr,ior) = 0.0 
      midlan(mstr,ior) = 99999999.9 
      mxdlan(mstr,ior) = 0.0 
      miSS(mstr,ior) = 99999999.9 
      mxSS(mstr,ior) = 0.0 

      migsZn(mstr,ior) = 99999999.9 
      mxgsZn(mstr,ior) = 0.0 
      miglZn(mstr,ior) = 99999999.9 
      mxglZn(mstr,ior) = 0.0 
      migsCad(mstr,ior) = 99999999.9 
      mxgsCad(mstr,ior) = 0.0 
      miglCad(mstr,ior) = 99999999.9 
      mxglCad(mstr,ior) = 0.0 
      migsCu(mstr,ior) = 99999999.9 
      mxgsCu(mstr,ior) = 0.0 
      miglCu(mstr,ior) = 99999999.9 
      mxglCu(mstr,ior) = 0.0 
      migsNi(mstr,ior) = 99999999.9 
      mxgsNi(mstr,ior) = 0.0 
      miglNi(mstr,ior) = 99999999.9 
      mxglNi(mstr,ior) = 0.0 
                                                                       
      if(nbuhn(mstr).eq.1)then 
!      Buhnenfelder                                                     
                                                                       
      bmxtem(mstr,ior) = 0.0 
      bmitem(mstr,ior) = 9999999.9 
      bmxno3(mstr,ior) = 0.0 
      bmino3(mstr,ior) = 999999.9 
      bmxno2(mstr,ior) = 0.0 
      bmino2(mstr,ior) = 999999.9 
      bmxnh4(mstr,ior) = 0.0 
      bminh4(mstr,ior) = 999999.9 
      bmxbsb(mstr,ior) = 0.0 
      bmibsb(mstr,ior) = 999999.9 
      bmxcsb(mstr,ior) = 0.0 
      bmicsb(mstr,ior) = 999999.9 
      bmxglp(mstr,ior) = 0.0 
      bmiglp(mstr,ior) = 999999.9 
      bmxchl(mstr,ior) = 0.0 
      bmichl(mstr,ior) = 9999999.9 
      bmxssa(mstr,ior) = 0.0 
      bmissa(mstr,ior) = 999999.9 
      bmxsi(mstr,ior) = 0.0 
      bmisi(mstr,ior) = 9999999.9 
      bmxzoo(mstr,ior) = 0.0 
      bmizoo(mstr,ior) = 9999999.9 
      bmxgsP(mstr,ior) = 0.0 
      bmigsP(mstr,ior) = 9999999.9 
      bmxgsN(mstr,ior) = 0.0 
      bmigsN(mstr,ior) = 9999999.9 
      bmxaki(mstr,ior) = 0.0 
      bmiaki(mstr,ior) = 9999999.9 
      bmxagr(mstr,ior) = 0.0 
      bmiagr(mstr,ior) = 9999999.9 
      bmxo2(mstr,ior) = 0.0 
      bmio2(mstr,ior) = 9999999.9 
      bmxmw(mstr,ior) = 0.0 
      bmimw(mstr,ior) = 9999999.9 
      bmxlf(mstr,ior) = 0.0 
      bmilf(mstr,ior) = 9999999.9 
      bmxca(mstr,ior) = 0.0 
      bmica(mstr,ior) = 9999999.9 
      bmxph(mstr,ior) = 0.0 
      bmiph(mstr,ior) = 9999999.9 

      bmxgsZn(mstr,ior) = 0.0 
      bmigsZn(mstr,ior) = 9999999.9 
      bmxglZn(mstr,ior) = 0.0 
      bmiglZn(mstr,ior) = 9999999.9 
      bmxgsCad(mstr,ior) = 0.0 
      bmigsCad(mstr,ior) = 9999999.9 
      bmxglCad(mstr,ior) = 0.0 
      bmiglCad(mstr,ior) = 9999999.9 
      bmxgsCu(mstr,ior) = 0.0 
      bmigsCu(mstr,ior) = 9999999.9 
      bmxglCu(mstr,ior) = 0.0 
      bmiglCu(mstr,ior) = 9999999.9 
      bmxgsNi(mstr,ior) = 0.0 
      bmigsNi(mstr,ior) = 9999999.9 
      bmxglNi(mstr,ior) = 0.0 
      bmiglNi(mstr,ior) = 9999999.9 
                                                                       
      endif 
!                                                                       
!.....Null setzen der Summen fr Mittelwertberechnung der Ausgaben      
!                                                                       
      sumte(mstr,ior) = 0.0 
      sCHNF(mstr,ior) = 0.0 
      sBVHNF(mstr,ior) = 0.0 
      sCD(mstr,1,ior) = 0.0 
      sCD(mstr,2,ior) = 0.0 
      sCP(mstr,1,ior) = 0.0 
      sCP(mstr,2,ior) = 0.0 
      sCM(mstr,ior) = 0.0 
      sBAC(mstr,ior) = 0.0 
      sgsP(mstr,ior) = 0.0 
      sgsN(mstr,ior) = 0.0 
      sumb5(mstr,ior) = 0.0 
      sumcs(mstr,ior) = 0.0 
      sumn4(mstr,ior) = 0.0 
      sumo2(mstr,ior) = 0.0 
      sumca(mstr,ior) = 0.0 
      sumcak(mstr,ior) = 0.0 
      sumcag(mstr,ior) = 0.0 
      sumcab(mstr,ior) = 0.0
      sumCChlk(mstr,ior) = 0.0 
      sumCChlg(mstr,ior) = 0.0 
      sumCChlb(mstr,ior) = 0.0 
      svkigr(mstr,ior) = 0.0 
      santbl(mstr,ior) = 0.0 
      sumaki(mstr,ior) = 0.0 
      sumagr(mstr,ior) = 0.0 
      sumabl(mstr,ior) = 0.0 
      sumzo(mstr,ior) = 0.0 
      sumvph(mstr,ior) = 0.0 
      sumno3(mstr,ior) = 0.0 
      sumno2(mstr,ior) = 0.0 
      sumgp(mstr,ior) = 0.0 
      sumsi(mstr,ior) = 0.0 
      summsl(mstr,ior) = 0.0 
      sumcal(mstr,ior) = 0.0 
      summw(mstr,ior) = 0.0 
      sumlf(mstr,ior) = 0.0 
      scoli(mstr,ior) = 0.0 
      sumdln(mstr,ior) = 0.0 

      sumgsZn(mstr,ior) = 0.0 
      sumglZn(mstr,ior) = 0.0 
      sumgsCad(mstr,ior) = 0.0 
      sumglCad(mstr,ior) = 0.0 
      sumgsCu(mstr,ior) = 0.0 
      sumglCu(mstr,ior) = 0.0 
      sumgsNi(mstr,ior) = 0.0 
      sumglNi(mstr,ior) = 0.0 

!....Dreissena                                                          
      do 836 ndr=1,nndr 
      sidras(mstr,ior,ndr) = 0.0 
      sdrmas(mstr,ior,ndr) = 0.0 
      sdrakr(mstr,ior,ndr) = 0.0 
      sdrbar(mstr,ior,ndr) = 0.0 
      sdrmor(mstr,ior,ndr) = 0.0 
      szdrg(mstr,ior,ndr) = 0.0 
      szdrsg(mstr,ior,ndr) = 0.0 
  836 sgwdrg(mstr,ior,ndr) = 0.0 
!                                                                       
      scorIg(mstr,ior) = 0.0 
      scoIsg(mstr,ior) = 0.0 
!                                                                       
      sumpfl(mstr,ior) = 0.0 
      sumss(mstr,ior) = 0.0 
      sumbal(mstr,ior) = 0.0 
!                                                                       
!.....Raten                                                             
!                                                                       
      ssusn(mstr,ior) = 0.0 
      sbettn(mstr,ior) = 0.0 
      sdon(mstr,ior) = 0.0 
      salgn(mstr,ior) = 0.0 
      salNO3(mstr,ior) = 0.0 
      sFluN3(mstr,ior) = 0.0 
      sJNO3(mstr,ior) = 0.0 
      sJNH4(mstr,ior) = 0.0 
      sJPO4(mstr,ior) = 0.0 
      sJO2(mstr,ior) = 0.0
      sJSi(mstr,ior) = 0.0 
      svx0(mstr,ior) = 0.0 
      svx02(mstr,ior) = 0.0 
      ssedx0(mstr,ior) = 0.0 
      ssusno(mstr,ior) = 0.0 
      ssedal(mstr,ior) = 0.0 
      salgzo(mstr,ior) = 0.0 
      salgdr(mstr,ior) = 0.0 
      salgco(mstr,ior) = 0.0 
      svoldr(mstr,ior) = 0.0 
      sdrpfe(mstr,ior) = 0.0 
      sabeow(mstr,ior) = 0.0 
      sabeor(mstr,ior) = 0.0 
      sdalg(mstr,ior) = 0.0 
      sdalga(mstr,ior) = 0.0 
      salmor(mstr,ior) = 0.0 
      sblmor(mstr,ior) = 0.0 
      ssgo2n(mstr,ior) = 0.0 
      ssdbsb(mstr,ior) = 0.0 
      ssoein(mstr,ior) = 0.0 
      ssalgo(mstr,ior) = 0.0 
      sbsbt(mstr,ior) = 0.0 
      s2algo(mstr,ior) = 0.0 
      s2algao(mstr,ior) = 0.0 
      sschlr(mstr,ior) = 0.0 
      sbsbbe(mstr,ior) = 0.0 
      so2phy(mstr,ior) = 0.0 
      sro2dr(mstr,ior) = 0.0 
      szooro(mstr,ior) = 0.0 
      spo2p(mstr,ior) = 0.0 
      spo2r(mstr,ior) = 0.0 
      sir(mstr,ior) = 0.0 
      srmue(mstr,ior) = 0.0 
      srakr(mstr,ior) = 0.0 
      srbar(mstr,ior) = 0.0 
      sffood(mstr,ior) = 0.0 
      sfik(mstr,ior) = 0.0 
      sfig(mstr,ior) = 0.0 
      sfib(mstr,ior) = 0.0 
      sakmua(mstr,ior) = 0.0 
      sagmua(mstr,ior) = 0.0 
      sabmua(mstr,ior) = 0.0 
      sfheka(mstr,ior) = 0.0 
      sfhega(mstr,ior) = 0.0 
      sfheba(mstr,ior) = 0.0 
      sakrau(mstr,ior) = 0.0 
      sagrea(mstr,ior) = 0.0 
      sabrea(mstr,ior) = 0.0 
      sHNFmu(mstr,ior) = 0.0 
      sHNFre(mstr,ior) = 0.0 
      sHNFup(mstr,ior) = 0.0 
      sHNFmo(mstr,ior) = 0.0 
      sHNFex(mstr,ior) = 0.0 
      sHNFdr(mstr,ior) = 0.0 
      sHNFz(mstr,ior) = 0.0 
      sBACmu(mstr,ior) = 0.0 
      sHNFBA(mstr,ior) = 0.0 
      snl0(mstr,ior) = 0.0 
      spl0(mstr,ior) = 0.0 
      snaehr(mstr,ior) = 0.0 
!                                                                       
!                                                                       
!     Buhnenfelder                                                      
!                                                                       
      if(nbuhn(mstr).eq.1)then 
      bste(mstr,ior) = 0.0 
      bsno3(mstr,ior) = 0.0 
      bsno2(mstr,ior) = 0.0 
      bsn4(mstr,ior) = 0.0 
      bsgelp(mstr,ior) = 0.0 
      bschla(mstr,ior) = 0.0 
      bsaki(mstr,ior) = 0.0 
      bsagr(mstr,ior) = 0.0 
      bsabl(mstr,ior) = 0.0 
      bsssal(mstr,ior) = 0.0 
      bssi(mstr,ior) = 0.0 
      bszooi(mstr,ior) = 0.0 
      bsvbsb(mstr,ior) = 0.0 
      bsvcsb(mstr,ior) = 0.0 
      bsgsP(mstr,ior) = 0.0 
      bsgsN(mstr,ior) = 0.0 
      bso2(mstr,ior) = 0.0 
      bsmw(mstr,ior) = 0.0 
      bslf(mstr,ior) = 0.0 
      bsca(mstr,ior) = 0.0 
      bsph(mstr,ior) = 0.0 
      bsnl0(mstr,ior) = 0.0 
      bspl0(mstr,ior) = 0.0 
      bscoli(mstr,ior) = 0.0

      bsgsZn(mstr,ior) = 0.0
      bsglZn(mstr,ior) = 0.0
      bsgsCad(mstr,ior) = 0.0
      bsglCad(mstr,ior) = 0.0
      bsgsCu(mstr,ior) = 0.0
      bsglCu(mstr,ior) = 0.0
      bsgsNi(mstr,ior) = 0.0
      bsglNi(mstr,ior) = 0.0
                                                                       
!......Raten                                                            
!                                                                       
      bsdalg(mstr,ior) = 0.0 
      bsvkg(mstr,ior) = 0.0 
      bsantb(mstr,ior) = 0.0
      bsdaa(mstr,ior) = 0.0 
      bsseda(mstr,ior) = 0.0 
      bsalgz(mstr,ior) = 0.0 
      bsamor(mstr,ior) = 0.0 
      bsadr(mstr,ior) = 0.0 
!...Pseudofaces = 0.0 da keine Dreissena in Buhnenfeld                  
      bsalco(mstr,ior) = 0.0 
      bsfik(mstr,ior) = 0.0 
      bsfig(mstr,ior) = 0.0 
      bskmue(mstr,ior) = 0.0 
      bsgmue(mstr,ior) = 0.0 
      bsbmue(mstr,ior) = 0.0 
      bshek(mstr,ior) = 0.0 
      bsheg(mstr,ior) = 0.0 
      bskre(mstr,ior) = 0.0 
      bsgre(mstr,ior) = 0.0 
      bsbre(mstr,ior) = 0.0 
      bschlk(mstr,ior) = 0.0 
      bschlg(mstr,ior) = 0.0 
      bschlb(mstr,ior) = 0.0 
      bnaehr(mstr,ior) = 0.0 
      bsFlN3(mstr,ior) = 0.0 
      bsbetN(mstr,ior) = 0.0 
      bsJNO3(mstr,ior) = 0.0 
      bsJNH4(mstr,ior) = 0.0 
      bsJPO4(mstr,ior) = 0.0 
      bsJO2(mstr,ior) = 0.0
      bsJSi(mstr,ior) = 0.0 
      endif 
!                                                                       
   91 continue 
   94 continue 
!                                                                       
  247 continue 
!                                                                       
!                                                                       
!**************************************************                     
!     Belegung des Ausgabegitters                                       
!**************************************************                     
!                                                                       
!                                                                       
 4545 continue 
!                                                                       
      if(iergeb.eq.1)goto 190 
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'ERGEBM.txt' 
      open(unit=45, file=pfadstring)
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'ERGEBT.txt'
      open(unit=155, file=pfadstring)
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'ERGEB2D.txt'
      open(unit=255, file=pfadstring)
      
      if(ckenn_vers=='*V')then
        write(45,4555)Versio
        4555 format('*V  QSim  ErgebM',2x,F5.2)  
          call ergebMFormat() 
        write(45,'(a50)')modell 
        write(45,'(a255)')cEreig

        write(155,4556)Versio
        4556 format('*V  QSim  ErgebT',2x,F5.2)  
          call ergebTFormat() 
        write(155,'(a50)')modell 
        write(155,'(a255)')cEreig 

        write(255,4557)Versio
        4557 format('*V  QSim  Ergeb2D',2x,F5.2)  
          call ergeb2DFormat()
        write(255,'(a50)')modell 
        write(255,'(a255)')cEreig
          else
            write(45,'(a50)')modell 
            write(45,4558)versio 
            write(45,'(a255)')cEreig 
            write(155,'(a50)')modell 
            write(155,4558)versio 
            write(155,'(a255)')cEreig 
            write(255,'(a50)')modell 
            write(255,4558)versio 
            write(255,'(a255)')cEreig 
      endif
  4558 format('QSim-Version  ',F5.2)
                                                                       
      iergeb = 1 
                                                                       
  190 continue 
                                                                        
      do azStr = 1,azStrs                  ! Beginn Strangschleife 
      mstr = mstra(azStr) 
      mSta = 0
    do iior=1,hanze(mstr)+1                ! Beginn Knotenschleife
      if(hflag(mstr,iior).eq.6)cycle
      mSta = mSta+1
                                                                       
      tiefey(mSta) = htiefe(mstr,iior) 
      tempwy(mSta) = htempw(mstr,iior)
      it_hy(mstr,msta) = it_h(mstr,iior) 

      if(iwsim.eq.4)then 
        tracer(mSta) = tempwy(mSta) 
        if(tracer(mSta).lt.0.001)tracer(mSta) = 0.0 
      endif 

      CHNFy(mSta) = hCHNF(mstr,iior) 
      BVHNFy(mSta) = hBVHNF(mstr,iior) 
      CDy(1,mSta) = hCD(mstr,1,iior) 
      CDy(2,mSta) = hCD(mstr,2,iior) 
      CPy(1,mSta) = hCP(mstr,1,iior) 
      CPy(2,mSta) = hCP(mstr,2,iior) 
      CMy(mSta) = hCM(mstr,iior) 
      BACy(mSta) = hBAC(mstr,iior) 
                                                                       
      vbsby(mSta) = hvbsb(mstr,iior) 
      vcsby(mSta) = hvcsb(mstr,iior) 
      vnh4y(mSta) = hnh4(mstr,iior) 
      vo2y(mSta) = ho2(mstr,iior) 
      vno3y(mSta) = hno3(mstr,iior) 
      vno2y(mSta) = hno2(mstr,iior) 
      vx0y(mSta) = hx0(mstr,iior) 
      vx02y(mSta) = hx02(mstr,iior) 
      siy(mSta) = hsi(mstr,iior) 
      chlay(mSta) = hchla(mstr,iior)
      chlaky(mSta) = hchlak(mstr,iior) 
      chlagy(mSta) = hchlag(mstr,iior) 
      chlaby(mSta) = hchlab(mstr,iior)
      CChlky(mSta) = hakbcm(mstr,iior)
      CChlgy(mSta) = hagbcm(mstr,iior)
      CChlby(mSta) = habbcm(mstr,iior)
      akiy(mSta) = haki(mstr,iior) 
      agry(mSta) = hagr(mstr,iior) 
      ably(mSta) = habl(mstr,iior) 
                                                                       
                                                                       
      do ndr=1,nndr 
        idrasy(mSta,ndr) = hidras(mstr,iior,ndr) 
        drmasy(mSta,ndr) = hdrmas(mstr,iior,ndr) 
        drakry(mSta,ndr) = hdrakr(mstr,iior,ndr) 
        drbary(mSta,ndr) = hdrbar(mstr,iior,ndr) 
        Rzuwdy(mSta,ndr) = hRzuwd(mstr,iior,ndr) 
                                                                        
        dreiy(mSta,ndr) = hzdrel(mstr,iior,ndr) 
        dreisy(mSta,ndr) = hzdrsl(mstr,iior,ndr) 
        gwdrly(mSta,ndr) = hgwdrl(mstr,iior,ndr) 
                                                                       
        drmory(mSta,ndr) = hdrmor(mstr,iior,ndr) 
      enddo

!...Makrophythen                                                        
      pfly(mSta) = hpfl(mstr,iior) 
!...benthische Algen                                                    
      alby(mSta) = habgml(mstr,iior)                         &
     &+habkml(mstr,iior)                                     
!                                                                       
      coroy(mSta) = hcoro2(mstr,iior,1)                      &
     &+hcoro2(mstr,iior,2)                                   &
     &+hcoro2(mstr,iior,3)                                   &
     &+hcoro2(mstr,iior,4)                                   &
     &+hcoro2(mstr,iior,5)                                   
!                                                                       
      corosy(mSta) = hcos2(mstr,iior,1)                      &
     &+hcos2(mstr,iior,2)                                    &
     &+hcos2(mstr,iior,3)                                    &
     &+hcos2(mstr,iior,4)                                    &
     &+hcos2(mstr,iior,5)                                    
                                                                       
                                                                        
      ssalgy(mSta) = hssalg(mstr,iior) 
      zooiny(mSta) = hzooi(mstr,iior) 
      gelpy(mSta) = hgelp(mstr,iior) 
      mwy(mSta) = hmw(mstr,iior) 
      cay(mSta) = hca(mstr,iior) 
      vphy(mSta) = hph(mstr,iior) 
      coliy(mSta) = hcoli(mstr,iior) 
      lfy(mSta) = hlf(mstr,iior) 
      dlarny(mSta) = hdlarn(mstr,iior) 

      gsZny(mSta) = hgsZn(mstr,iior) 
      glZny(mSta) = hglZn(mstr,iior) 
      gsCady(mSta) = hgsCad(mstr,iior) 
      glCady(mSta) = hglCad(mstr,iior) 
      gsCuy(mSta) = hgsCu(mstr,iior) 
      glCuy(mSta) = hglCu(mstr,iior) 
      gsNiy(mSta) = hgsNi(mstr,iior) 
      glNiy(mSta) = hglNi(mstr,iior) 
                                                                       
      algzky(mSta) = 0.0
      algzgy(mSta) = 0.0
      algzby(mSta) = 0.0
      susny(mSta) = hsusn(mstr,iior) 
      bettny(mSta) = hbettn(mstr,iior) 
      dony(mSta) = hdon(mstr,iior) 
      akin4y(mSta) = haknh4(mstr,iior) 
      agrn4y(mSta) = hagnh4(mstr,iior) 
      abln4y(mSta) = habnh4(mstr,iior) 
      alNO3y(mSta) = halNO3(mstr,iior) 
      FluN3y(mSta) = hFluN3(mstr,iior) 
      sedx0y(mSta) = hsedx0(mstr,iior) 
      susnoy(mSta) = hsusno(mstr,iior) 
      sedaky(mSta) = hsedak(mstr,iior) 
      sedagy(mSta) = hsedag(mstr,iior) 
      sedaby(mSta) = hsedab(mstr,iior) 
      algzky(mSta) = halgzk(mstr,iior)
      algzgy(mSta) = halgzg(mstr,iior)
      algzby(mSta) = halgzb(mstr,iior)

      algdky(mSta) = halgdk(mstr,iior) 
      algdgy(mSta) = halgdg(mstr,iior) 
      algdby(mSta) = halgdb(mstr,iior) 
      algcky(mSta) = halgck(mstr,iior) 
      algcgy(mSta) = halgcg(mstr,iior) 
      algcby(mSta) = halgcb(mstr,iior) 
      volfdy(mSta) = hvolfd(mstr,iior) 
      drpfey(mSta) = hdrpfe(mstr,iior) 
      abowgy(mSta) = habowg(mstr,iior) 
      abowky(mSta) = habowk(mstr,iior) 
      aborgy(mSta) = haborg(mstr,iior) 
      aborky(mSta) = habork(mstr,iior) 
      dalggy(mSta) = hdalgg(mstr,iior) 
      dalgky(mSta) = hdalgk(mstr,iior) 
      dalgby(mSta) = hdalgb(mstr,iior) 
      dalagy(mSta) = hdalag(mstr,iior) 
      dalaby(mSta) = hdalab(mstr,iior) 
      dalaky(mSta) = hdalak(mstr,iior) 
      dgmory(mSta) = hdgmor(mstr,iior) 
      dkmory(mSta) = hdkmor(mstr,iior) 
      dbmory(mSta) = hdbmor(mstr,iior) 
      sgo2ny(mSta) = hsgo2n(mstr,iior) 
      sdbsby(mSta) = hsdbsb(mstr,iior) 
      so2eiy(mSta) = hsoein(mstr,iior) 
      salgoy(mSta) = hsalgo(mstr,iior) 
      bsbty(mSta) = hbsbt(mstr,iior) 
      dalgoy(mSta) = hdalgo(mstr,iior)
      dalaoy(mSta) = hdalao(mstr,iior) 
      schlry(mSta) = hschlr(mstr,iior) 
      bsbbey(mSta) = hbsbbe(mstr,iior) 
      o2ei1y(mSta) = hoein1(mstr,iior) 
      ro2dry(mSta) = hro2dr(mstr,iior) 
      zoro2y(mSta) = hzoro2(mstr,iior) 
      po2py(mSta) = hpo2p(mstr,iior) 
      po2ry(mSta) = hpo2r(mstr,iior) 
      iry(mSta) = hiras(mstr,iior) 
      rmuasy(mSta) = hrmuas(mstr,iior) 
      rakry(mSta) = hrakr(mstr,iior) 
      rbary(mSta) = hrbar(mstr,iior) 
      ffoody(mSta) = hffood(mstr,iior) 
      nl0y(mSta) = hnl0(mstr,iior) 
      pl0y(mSta) = hpl0(mstr,iior) 
      Q_NKy(mSta) = hQ_NK(mstr,iior) 
      Q_PKy(mSta) = hQ_PK(mstr,iior) 
      Q_SKy(mSta) = hQ_SK(mstr,iior) 
      Q_NGy(mSta) = hQ_NG(mstr,iior) 
      Q_PGy(mSta) = hQ_PG(mstr,iior) 
      Q_NBy(mSta) = hQ_NB(mstr,iior) 
      Q_PBy(mSta) = hQ_PB(mstr,iior) 
!                                                                       
!++++++++++++++++++++++++++                                             
      akmuey(mSta) = hkmuea(mstr,iior) 
      agmuey(mSta) = hgmuea(mstr,iior) 
      abmuey(mSta) = hbmuea(mstr,iior) 
      ftay(mSta) = hftaau(mstr,iior) 
      fiy(mSta) = hfiaus(mstr,iior) 
      figy(mSta) = hfigau(mstr,iior) 
      fiby(mSta) = hfibau(mstr,iior) 
      fhey(mSta) = hfheau(mstr,iior) 
      fhegy(mSta) = hfhega(mstr,iior) 
      fheby(mSta) = hfheba(mstr,iior) 
      akry(mSta) = hakrau(mstr,iior) 
      agrey(mSta) = hagrau(mstr,iior) 
      abrey(mSta) = habrau(mstr,iior) 
      extky(mSta) = hextk(mstr,iior) 
!                                                                       
      tpkiy(mSta) = htpki(mstr,iior) 
      tpgry(mSta) = htpgr(mstr,iior) 
      tpbly(mSta) = htpbl(mstr,iior) 
      vkigry(mSta) = hvkigr(mstr,iior) 
      antbly(mSta) = hantbl(mstr,iior) 
      HNFmuy(mSta) = hHNFmu(mstr,iior) 
      HNFrey(mSta) = hHNFre(mstr,iior) 
      HNFupy(mSta) = hHNFup(mstr,iior) 
      HNFmoy(mSta) = hHNFmo(mstr,iior) 
      HNFexy(mSta) = hHNFex(mstr,iior) 
      HNFdry(mSta) = hHNFdr(mstr,iior) 
      HNFzy(mSta) = hHNFza(mstr,iior) 
      BACmuy(mSta) = hBAmua(mstr,iior) 
      JNO3y(mSta) = hJNO3(mstr,iior) 
      JNH4y(mSta) = hJNH4(mstr,iior) 
      JPO4y(mSta) = hJPO4(mstr,iior) 
      JO2y(mSta) = hJO2(mstr,iior)
      JSiy(mSta) = hJSi(mstr,iior)

      SedOM(mstr,mSta) = hSedOM(mstr,iior) 
!      w2(mstr,mSta) = hw2(mstr,iior) 
      dkorn(mstr,mSta) = hdkorn(mstr,iior) 
!                                                                       
      dly(mSta) = hdl(mstr,iior) 
                                                                        
!....sedhy = Sedimentdicke in mm                                        
      sedhy(mstr,mSta) = -1. 
      if(ieros.eq.1)sedhy(mstr,mSta) = sedhg(mstr,iior)*1000.                                                            
!                                                                       
      gsPy(mSta) = hgesP(mstr,iior) 
      gsNy(mSta) = hgesN(mstr,iior) 
!                                                                       
      if(BACy(mSta)>0.0)then 
        HNFBAy(mSta) = hHNFBA(mstr,iior)/BACy(mSta) 
      endif 

                                                                       
      if(nbuhn(mstr).eq.1)then      ! Buhnenfelder 
      btempy(mSta) = btempw(mstr,iior) 
      if(iwsim.eq.4)then 
      btracer(mSta) = btempy(mSta) 
      if(btracer(mSta).lt.0.001)btracer(mSta) = 0.0 
      endif 
      bno3y(mSta) = bno3(mstr,iior) 
      bno2y(mSta) = bno2(mstr,iior) 
      bnh4y(mSta) = bnh4(mstr,iior) 
      bgelpy(mSta) = bgelp(mstr,iior) 
      bchlay(mSta) = bchla(mstr,iior) 
      bchlky(mSta) = bchlak(mstr,iior) 
      bchlgy(mSta) = bchlag(mstr,iior) 
      bchlby(mSta) = bchlab(mstr,iior) 
      bssaly(mSta) = bssalg(mstr,iior) 
      bsiy(mSta) = bsi(mstr,iior) 
      bakiy(mSta) = baki(mstr,iior) 
      bagry(mSta) = bagr(mstr,iior) 
      bably(mSta) = babl(mstr,iior) 
      bzooiy(mSta) = bzooi(mstr,iior) 
      bvbsby(mSta) = bvbsb(mstr,iior) 
      bvcsby(mSta) = bvcsb(mstr,iior) 
      bCDy(1,mSta) = bCD(mstr,1,iior) 
      bCDy(2,mSta) = bCD(mstr,2,iior) 
      bCPy(1,mSta) = bCP(mstr,1,iior) 
      bCPy(2,mSta) = bCP(mstr,2,iior) 
      bCMy(mSta) = bCM(mstr,iior) 
      bBACy(mSta) = bBAC(mstr,iior) 
      bo2y(mSta) = bo2(mstr,iior) 
      bmwy(mSta) = bmw(mstr,iior) 
      blfy(mSta) = blf(mstr,iior) 
      bcay(mSta) = bca(mstr,iior) 
      bphy(mSta) = bph(mstr,iior) 
      bcoliy(mSta) = bcoli(mstr,iior)

      bgsZny(msta) = bgsZn(mstr,iior) 
      bglZny(msta) = bglZn(mstr,iior) 
      bgsCady(msta) = bgsCad(mstr,iior) 
      bglCady(msta) = bglCad(mstr,iior) 
      bgsCuy(msta) = bgsCu(mstr,iior) 
      bglCuy(msta) = bglCu(mstr,iior) 
      bgsNiy(msta) = bgsNi(mstr,iior) 
      bglNiy(msta) = bglNi(mstr,iior) 
                                                                       
      bdakiy(mSta) = bdaki(mstr,iior) 
      bdagry(mSta) = bdagr(mstr,iior) 
      bdably(mSta) = bdabl(mstr,iior) 
      bdaaky(mSta) = bdaak(mstr,iior) 
      bdaagy(mSta) = bdaag(mstr,iior) 
      bdaaby(mSta) = bdaab(mstr,iior) 
      bsedky(mSta) = bsedak(mstr,iior) 
      bsedgy(mSta) = bsedag(mstr,iior) 
      bsedby(mSta) = bsedab(mstr,iior) 
      bazoky(mSta) = bazok(mstr,iior) 
      bazogy(mSta) = bazog(mstr,iior) 
      bazoby(mSta) = bazob(mstr,iior) 
      bkmory(mSta) = bdkmor(mstr,iior) 
      bgmory(mSta) = bdgmor(mstr,iior) 
      bbmory(mSta) = bdbmor(mstr,iior) 
      bkigry(mSta) = bvkigr(mstr,iior) 
      bantby(mSta) = bantbl(mstr,iior) 
      bkbcmy(mSta) = bakbcm(mstr,iior) 
      biry(mSta) = bir(mstr,iior) 
      bsisdy(mSta) = bsised(mstr,iior) 
      badrky(mSta) = badrk(mstr,iior) 
      badrgy(mSta) = badrg(mstr,iior) 
      badrby(mSta) = badrb(mstr,iior) 
      bacoky(mSta) = bacok(mstr,iior) 
      bacogy(mSta) = bacog(mstr,iior) 
      bacoby(mSta) = bacob(mstr,iior) 
!                                                                       
      bkmuay(mSta) = bakmua(mstr,iior) 
      bgmuay(mSta) = bagmua(mstr,iior) 
      bbmuay(mSta) = babmua(mstr,iior) 
      bftkay(mSta) = bftaau(mstr,iior) 
      bfikay(mSta) = bfiaus(mstr,iior) 
      bfigay(mSta) = bfigas(mstr,iior) 
      bfibay(mSta) = bfibas(mstr,iior) 
      bfhkay(mSta) = bfheau(mstr,iior) 
      bfhgay(mSta) = bfhgau(mstr,iior) 
      bfhbay(mSta) = bfhbau(mstr,iior) 
      bkray(mSta) = bakrau(mstr,iior) 
      bgray(mSta) = bagrau(mstr,iior) 
      bbray(mSta) = babrau(mstr,iior) 
      bnl0y(mSta) = bnl0(mstr,iior) 
      bpl0y(mSta) = bpl0(mstr,iior) 
      btpkiy(mSta) = btpki(mstr,iior) 
      btpgry(mSta) = btpgr(mstr,iior) 
      btpbly(mSta) = btpbl(mstr,iior) 
      bextky(mSta) = bextk(mstr,iior) 
      bbetNy(mSta) = bbettn(mstr,iior) 
      bJNO3y(mSta) = bJNO3(mstr,iior) 
      bJNH4y(mSta) = bJNH4(mstr,iior) 
      bJPO4y(mSta) = bJPO4(mstr,iior) 
      bJO2y(mSta) = bJO2(mstr,iior) 
      bJSiy(mSta) = bJSi(mstr,iior) 
!                                                                       
      bgsPy(mSta) = bgesP(mstr,iior) 
      bgsNy(mSta) = bgesN(mstr,iior) 
      bFlN3y(mSta) = bFluN3(mstr,iior) 
      tau2y(mSta) = htau2(mstr,iior) 
      SedOMb(mstr,mSta) = bSedOM(mstr,iior) 
      w2b(mstr,mSta) = bw2(mstr,iior) 
      dkornb(mstr,mSta) = bdkorn(mstr,iior) 
    endif 
                                                                       
                                                                       
!...2D-Modellierung                                                     
                                                                       
      nkzsy(mSta) = hnkzs(mstr,iior)
      Tend = 0.0 
      vN4end = 0.0 
      vN2end = 0.0 
      vN3end = 0.0 
      vo2end = 0.0 
      vgPend = 0.0 
      Siend = 0.0 
      akiend = 0.0 
      agrend = 0.0 
      ablend = 0.0 
      chlend = 0.0 

   if(hdH2De(mstr,iior).gt.0.0)then 
     tend = htempz(mstr,hnkzs(mstr,iior)-1,iior)+(htempz(mstr,hnkzs(mstr,iior),iior)-htempz(mstr,hnkzs(mstr,iior)-1,iior))   &
           *dH2de(ior)/dH2D
     vN4end = hNH4z(mstr,hnkzs(mstr,iior)-1,iior)+(hNH4z(mstr,hnkzs(mstr,iior),iior)-hNH4z(mstr,hnkzs(mstr,iior)-1,iior))    &
           *dH2de(ior)/dH2D
     vN2end = hNO2z(mstr,hnkzs(mstr,iior)-1,iior)+(hNO2z(mstr,hnkzs(mstr,iior),iior)-hNO2z(mstr,hnkzs(mstr,iior)-1,iior))    &
           *dH2de(ior)/dH2D
     vN3end = hNO3z(mstr,hnkzs(mstr,iior)-1,iior)+(hNO3z(mstr,hnkzs(mstr,iior),iior)-hNO3z(mstr,hnkzs(mstr,iior)-1,iior))    &
           *dH2de(ior)/dH2D
     vO2end = hO2z(mstr,hnkzs(mstr,iior)-1,iior)+(hO2z(mstr,hnkzs(mstr,iior),iior)-hO2z(mstr,hnkzs(mstr,iior)-1,iior))       &
           *dH2de(ior)/dH2D
     vgPend = hgelPz(mstr,hnkzs(mstr,iior)-1,iior)+(hgelPz(mstr,hnkzs(mstr,iior),iior)-hgelPz(mstr,hnkzs(mstr,iior)-1,iior)) &
           *dH2de(ior)/dH2D
     Siend = hSiz(mstr,hnkzs(mstr,iior)-1,iior)+(hSiz(mstr,hnkzs(mstr,iior),iior)-hSiz(mstr,hnkzs(mstr,iior)-1,iior))        &
           *dH2de(ior)/dH2D
     akiend = hakiz(mstr,hnkzs(mstr,iior)-1,iior)+(hakiz(mstr,hnkzs(mstr,iior),iior)-hakiz(mstr,hnkzs(mstr,iior)-1,iior))    &
           *dH2de(ior)/dH2D
     agrend = hakiz(mstr,hnkzs(mstr,iior)-1,iior)+(hagrz(mstr,hnkzs(mstr,iior),iior)-hagrz(mstr,hnkzs(mstr,iior)-1,iior))    &
           *dH2de(ior)/dH2D
     ablend = hakiz(mstr,hnkzs(mstr,iior)-1,iior)+(hablz(mstr,hnkzs(mstr,iior),iior)-hablz(mstr,hnkzs(mstr,iior)-1,iior))    &
           *dH2de(ior)/dH2D
     chlend = hchlaz(mstr,hnkzs(mstr,iior)-1,iior)+(hchlaz(mstr,hnkzs(mstr,iior),iior)-hchlaz(mstr,hnkzs(mstr,iior)-1,iior)) &
           *dH2de(ior)/dH2D
   endif 
                                                                       
      do nkz = 1,hnkzs(mstr,iior)
        tempzy(nkz,mSta) = htempz(mstr,nkz,iior) 
        vnh4zy(nkz,mSta) = hnh4z(mstr,nkz,iior) 
        vNO2zy(nkz,mSta) = hno2z(mstr,nkz,iior) 
        vNO3zy(nkz,mSta) = hno3z(mstr,nkz,iior) 
        vO2zy(nkz,mSta) = ho2z(mstr,nkz,iior) 
        gelPzy(nkz,mSta) = hgelPz(mstr,nkz,iior) 
        Sizy(nkz,mSta) = hSiz(mstr,nkz,iior) 
        akizy(nkz,mSta) = hakiz(mstr,nkz,iior) 
        agrzy(nkz,mSta) = hagrz(mstr,nkz,iior) 
        ablzy(nkz,mSta) = hablz(mstr,nkz,iior) 
        chlazy(nkz,mSta) = hchlaz(mstr,nkz,iior) 
        CChlakzy(nkz,mSta) = hCChlkz(mstr,nkz,iior)
        CChlabzy(nkz,mSta) = hCChlbz(mstr,nkz,iior)
        CChlagzy(nkz,mSta) = hCChlgz(mstr,nkz,iior)
      enddo 

!      if(hdH2De(mstr,iior).gt.0.0)then 
!      tempzy(hnkzs(mstr,iior),mSta) = Tend 
!      vNH4zy(hnkzs(mstr,iior),mSta) = vN4end 
!      vNO2zy(hnkzs(mstr,iior),mSta) = vN2end 
!      vNO3zy(hnkzs(mstr,iior),mSta) = vN3end 
!      vO2zy(hnkzs(mstr,iior),mSta) = vO2end 
!      gelPzy(hnkzs(mstr,iior),mSta) = vgPend 
!      Sizy(hnkzs(mstr,iior),mSta) = Siend 
!      akizy(hnkzs(mstr,iior),mSta) = akiend 
!      agrzy(hnkzs(mstr,iior),mSta) = agrend 
!      ablzy(hnkzs(mstr,iior),mSta) = ablend 
!      chlazy(hnkzs(mstr,iior),mSta) = chlend 
!      endif 

     enddo              ! Ende Knotenschleife
                                                                       
      do iior = 1,mStas(mstr)      ! Beginn Stationenschleife
!                                                                       
!....Ausschreiben der Stndlichen Werte (FALLS imitt = 1)               
!                                                                       
!                                                                       
                                                                        
      if(imitt.eq.0)Goto 618 
      if(iwsim/=4)tracer = -1. 
      if(iwsim==4)then 
      vbsby(iior) = -1. 
      vcsby(iior) = -1. 
      vnh4y(iior) = -1. 
      vno2y(iior) = -1. 
      vno3y(iior) = -1. 
      gsNy(iior) = -1. 
      gelpy(iior) = -.1 
      gsPy(iior) = -1. 
      Siy(iior) = -1. 
      chlay(iior) = -1. 
      zooiny(iior) = -1. 
      vphy(iior) = -1. 
      mwy(iior) = -1. 
      cay(iior) = -1. 
      lfy(iior) = -1. 
      ssalgy(iior) = -1. 
      vo2y(iior) = -1. 
      CHNFy(iior) = -1. 
      coliy(iior) = -1. 
      tempwy(iior) = -1. 
      endif 
!                                                                       
!...Umrechnung der Uhrz in h.mm                                         
!                                                                       
                                                                        
      Stunde = int(Uhrz) 
      hcmin = (Uhrz-Stunde)*60. 
      minute = nint(hcmin) 
      if(minute.eq.60)then 
        minute = 0 
        Stunde = Stunde+1 
      endif 
      rmin = minute/100. 
      Uhrhm = Stunde+rmin 
                                                                      
      write(155,5103)itags,monats,jahrs,uhrhm                           &
     &,mstr,Stakm(mstr,iior),STRID(mstr)                                
      write(155,5105)vbsby(iior),vcsby(iior)                            &
     &,vnh4y(iior),vno2y(iior),vno3y(iior),gsNy(iior),gelpy(iior)       &
     &,gsPy(iior),Siy(iior),chlay(iior),zooiny(iior),vphy(iior)         &
     &,mwy(iior),cay(iior),lfy(iior),ssalgy(iior),tempwy(iior)          &
     &,vo2y(iior),CHNFy(iior),coliy(iior),Dly(iior),sedhy(mstr,iior)    &
     &,tracer(iior)                                                     
!                                                                       
!Umrechnung von Zeitschrittweite auf pro Stunde                         
      hcUmt = 60./(tflie*1440.) 
!                                                                       
      write(155,5205)(bsbty(iior)*hcUmt),(susNOy(iior)*hcUmt),(O2ei1y(iior)*hcUmt)                           &  
                     ,(dalgoy(iior)*hcUmt),(cchlky(iior)*hcUmt),(cchlgy(iior)*hcUmt),(cchlby(iior)*hcUmt)    &
                     ,(zoro2y(iior)*hcUmt),(schlry(iior)*hcUmt),(bettny(iior)*hcUmt)                        
!                                                                       
      if(nbuhn(mstr).eq.1.and.iwsim/=4)goto 617 
      btempy(iior) = -1. 
      bvbsby(iior) = -1. 
      bvcsby(iior) = -1 
      bnh4y(iior) = -1. 
      bno2y(iior) = -.1 
      bno3y(iior) = -1. 
      bgsNy(iior) = -1. 
      bgelpy(iior) = -.1 
      bgsPy(iior) = -1. 
      bsiy(iior) = -1. 
      bchlay(iior) = -1. 
      bzooiy(iior) = -1. 
      bphy(iior) = -1. 
      bmwy(iior) = -1. 
      bcay(iior) = -1. 
      blfy(iior) = -1. 
      bssaly(iior) = -1. 
      btempy(iior) = -1. 
      bo2y(iior) = -1. 
      if(nbuhn(mstr).eq.0)tau2y(iior) = -1. 
!                                                                       
  617 bcoliy = -1. 
      bHNFy = -1. 
      if(nbuhn(mstr).eq.1.and.iwsim.eq.4)goto 620 
      btracer(iior) = -1. 
!                                                                       
  620 write(155,5115)bvbsby(iior),bvcsby(iior),bnh4y(iior)              &
     &,bno2y(iior),bno3y(iior),bgsNy(iior),bgelpy(iior),bgsPy(iior)     &
     &,bsiy(iior),bchlay(iior),bzooiy(iior),bphy(iior),bmwy(iior)       &
     &,bcay(iior),blfy(iior),bssaly(iior),btempy(iior),bo2y(iior)       &
     &,bHNFy,bcoliy(iior),tau2y(iior),btracer(iior)                           
!                                                                       
!                                                                       
 5103 FORMAT(i2,2X,i2,2x,i4,2x,f5.2,2x,i5,2x,f8.3,2x,I5) 
 5104 FORMAT(i2,2X,i2,2x,i4,2x,f5.2,2x,i5,2x,f8.3,2x,I2                 &
     &,2x,I2,2x,I5)                                                     
 5105 FORMAT(f6.2,2x,f6.2,2x,f6.2,2x,f6.3,2x,f9.6,2x,f5.2,2x,f6.3       &
     &,2x,f5.2,2x,f5.2,2x,f6.2,2x,f7.1,2x,f5.2,2x,f5.2,2x,f5.1          &
     &,2x,f8.1,2x,f6.2,2x,f5.2,2x,f5.2,2x,f8.1,2x,E9.2,2x,f7.2          &
     &,2x,f12.6,2x,f7.3)                                                
!                                                                       
 5115 FORMAT(f6.2,2x,f6.2,2x,f6.2,2x,f6.3,2x,f9.6,2x,f5.2,2x,f5.3       &
     &,2x,f5.2,2x,f5.2,2x,f6.2,2x,f7.1,2x,f5.2,2x,f5.2,2x,f5.1          &
     &,2x,f6.1,2x,f6.2,2x,f5.2,2x,f5.2,2x,f8.1,2x,E9.3,2x,f7.3          &
     &,2x,f9.3)                                                         
!                                                                       
 5205 Format(f8.6,2x,f8.6,2x,f8.6,2x,f8.6,2x,f6.2,2x,f6.2,2x,f6.2,2x,F8.6,2x,f10.8,2x,f8.6) 
!                                                                       
!...Ausgabe der Ergebnisse der 2D-Modellierung                          
!                                                                       
  618 continue 
      if(i2Daus.eq.0.or.iwsim.eq.4)goto 619 
      ztiefa = 0.0 
      zPara0 = -.1 
                                                                        
!...Umrechnung der Uhrz in h.mm                                         
!                                                                       
                                                                        
      Stunde = int(Uhrz) 
      hcmin = (Uhrz-Stunde)*60. 
      minute = nint(hcmin) 
      if(minute.eq.60)then 
        minute = 0 
        Stunde = Stunde+1 
      endif 
      rmin = minute/100. 
      Uhrhm = Stunde+rmin 
                                                                       
      write(255,5104)itags,monats,jahrs,Uhrhm,mstr,Stakm(mstr,iior),nkzmx(mstr,iior),nkzsy(iior),STRID(mstr)   
                                                                       
      do 1810 nkz = 1,nkzsy(iior) 
      if(vNO2zy(nkz,iior).le.0.0)vNO2zy(nkz,iior) = 0.001 
      if(gelPzy(nkz,iior).lt.0.0)gelPzy(nkz,iior) = -.10 
      if(ztiefa.gt.tiefey(iior))ztiefa = tiefey(iior)

      write(255,5107)ztiefa,vNH4zy(nkz,iior),vNO2zy(nkz,iior),vNO3zy(nkz,iior),gelPzy(nkz,iior),Sizy(nkz,iior)    &                 
                     ,tempzy(nkz,iior),vO2zy(nkz,iior),chlazy(nkz,iior),CChlakzy(nkz,iior),CChlabzy(nkz,iior)     &
                     ,CChlagzy(nkz,iior),hgesPz(mstr,nkz,iior),hgesNz(mstr,nkz,iior)                
      ztiefa = ztiefa+dH2D 
                                                                       
!     Ermittlung der min- und max-Werte der einzelnen Parameter für die 
                                                                       
      if(vNH4zy(nkz,iior).gt.Ymax(mstr,161))Ymax(mstr,161) = vNH4zy(nkz,iior)                                 
      if(vNH4zy(nkz,iior).lt.Ymin(mstr,161))Ymin(mstr,161) = vNH4zy(nkz,iior)                                 
      if(vNO2zy(nkz,iior).gt.Ymax(mstr,162))                            &
     &Ymax(mstr,162) = vNO2zy(nkz,iior)                                 
      if(vNO2zy(nkz,iior).lt.Ymin(mstr,162))                            &
     &Ymin(mstr,162) = vNO2zy(nkz,iior)                                 
      if(vNO3zy(nkz,iior).gt.Ymax(mstr,163))                            &
     &Ymax(mstr,163) = vNO3zy(nkz,iior)                                 
      if(vNO3zy(nkz,iior).lt.Ymin(mstr,163))                            &
     &Ymin(mstr,163) = vNO3zy(nkz,iior)                                 
      if(gelPzy(nkz,iior).gt.Ymax(mstr,164))                            &
     &Ymax(mstr,164) = gelPzy(nkz,iior)                                 
      if(gelPzy(nkz,iior).lt.Ymin(mstr,164))                            &
     &Ymin(mstr,164) = gelPzy(nkz,iior)                                 
      if(Sizy(nkz,iior).gt.Ymax(mstr,165))                              &
     &Ymax(mstr,165) = Sizy(nkz,iior)                                   
      if(Sizy(nkz,iior).lt.Ymin(mstr,165))                              &
     &Ymin(mstr,165) = Sizy(nkz,iior)                                   
      if(tempzy(nkz,iior).gt.Ymax(mstr,166))                            &
     &Ymax(mstr,166) = tempzy(nkz,iior)                                 
      if(tempzy(nkz,iior).lt.Ymin(mstr,166))                            &
     &Ymin(mstr,166) = tempzy(nkz,iior)                                 
      if(vO2zy(nkz,iior).gt.Ymax(mstr,167))                             &
     &Ymax(mstr,167) = vO2zy(nkz,iior)                                  
      if(vO2zy(nkz,iior).lt.Ymin(mstr,167))                             &
     &Ymin(mstr,167) = vO2zy(nkz,iior)                                  
      if(chlazy(nkz,iior).gt.Ymax(mstr,172))Ymax(mstr,172) = chlazy(nkz,iior)                                 
      if(chlazy(nkz,iior).lt.Ymin(mstr,172))Ymin(mstr,172) = chlazy(nkz,iior)                                 
      if(hgesPz(mstr,nkz,iior).gt.Ymax(mstr,183))Ymax(mstr,183) = hgesPz(mstr,nkz,iior)                                 
      if(hgesPz(mstr,nkz,iior).lt.Ymin(mstr,183))Ymin(mstr,183) = hgesPz(mstr,nkz,iior)                                 
      if(hgesNz(mstr,nkz,iior).gt.Ymax(mstr,184))Ymax(mstr,184) = hgesNz(mstr,nkz,iior)                                 
      if(hgesNz(mstr,nkz,iior).lt.Ymin(mstr,184))Ymin(mstr,184) = hgesNz(mstr,nkz,iior)                                 
      if(CChlakzy(nkz,iior).gt.Ymax(mstr,190))Ymax(mstr,190) = CChlakzy(nkz,iior)                                 
      if(CChlakzy(nkz,iior).lt.Ymin(mstr,190))Ymin(mstr,190) = CChlakzy(nkz,iior)                                 
      if(CChlabzy(nkz,iior).gt.Ymax(mstr,191))Ymax(mstr,191) = CChlabzy(nkz,iior)                                 
      if(CChlabzy(nkz,iior).lt.Ymin(mstr,191))Ymin(mstr,191) = CChlabzy(nkz,iior)                                 
      if(CChlagzy(nkz,iior).gt.Ymax(mstr,192))Ymax(mstr,192) = CChlagzy(nkz,iior)                                 
      if(CChlagzy(nkz,iior).lt.Ymin(mstr,192))Ymin(mstr,192) = CChlagzy(nkz,iior)                                 
                                                                     
 1810 continue 
!                                                                       
      do 1811 nkz = nkzsy(iior)+1,nkzmx(mstr,iior) 
      write(255,5107)zPara0,zPara0,zPara0,zPara0,zPara0,zPara0,zPara0,zPara0,zPara0,zPara0,zPara0,zPara0,zPara0,zPara0                              
 1811 continue 
!                                                                       
 5107 FORMAT(f5.2,2x,f6.2,2x,f5.3,2x,f9.6,2x,f5.3,2x,f5.2,2x,f5.2       &
     &,2x,f5.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f7.3,2x,f7.3)                                                 
!                                                                       
  619 continue 
!                                                                       
!                                                                       
! Summenbildung fuer Ausgabe                                            
!                                                                       
      sumte(mstr,iior) = sumte(mstr,iior)+tempwy(iior) 
      if(tempwy(iior).gt.mxtemp(mstr,iior))                             &
     &mxtemp(mstr,iior) = tempwy(iior)                                  
      if(tempwy(iior).lt.mitemp(mstr,iior))                             &
     &mitemp(mstr,iior) = tempwy(iior)                                  
                                                                       
      sCHNF(mstr,iior) = sCHNF(mstr,iior)+CHNFy(iior) 
      sBVHNF(mstr,iior) = sBVHNF(mstr,iior)+BVHNFy(iior) 
      sCD(mstr,1,iior) = sCD(mstr,1,iior)+CDy(1,iior) 
      sCD(mstr,2,iior) = sCD(mstr,2,iior)+CDy(2,iior) 
      sCP(mstr,1,iior) = sCP(mstr,1,iior)+CPy(1,iior) 
      sCP(mstr,2,iior) = sCP(mstr,2,iior)+CPy(2,iior) 
      sCM(mstr,iior) = sCM(mstr,iior)+CMy(iior) 
      sBAC(mstr,iior) = sBAC(mstr,iior)+BACy(iior) 
!                                                                       
      sgsP(mstr,iior) = sgsP(mstr,iior)+gsPy(iior) 
      if(gsPy(iior).gt.mxgsP(mstr,iior))mxgsP(mstr,iior) = gsPy(iior) 
      if(gsPy(iior).lt.migsP(mstr,iior))migsP(mstr,iior) = gsPy(iior) 
!                                                                       
      sgsN(mstr,iior) = sgsN(mstr,iior)+gsNy(iior) 
      if(gsNy(iior).gt.mxgsN(mstr,iior))mxgsN(mstr,iior) = gsNy(iior) 
      if(gsNy(iior).lt.migsN(mstr,iior))migsN(mstr,iior) = gsNy(iior) 
      sumb5(mstr,iior) = sumb5(mstr,iior)+vbsby(iior) 
      if(vbsby(iior).gt.mxb5(mstr,iior))mxb5(mstr,iior) = vbsby(iior) 
      if(vbsby(iior).lt.mib5(mstr,iior))mib5(mstr,iior) = vbsby(iior) 
      sumcs(mstr,iior) = sumcs(mstr,iior)+vcsby(iior) 
      if(vcsby(iior).gt.mxcs(mstr,iior))mxcs(mstr,iior) = vcsby(iior) 
      if(vcsby(iior).lt.mics(mstr,iior))mics(mstr,iior) = vcsby(iior) 
      sumn4(mstr,iior) = sumn4(mstr,iior)+vnh4y(iior) 
      if(vnh4y(iior).gt.mxnh4(mstr,iior))mxnh4(mstr,iior) = vnh4y(iior) 
      if(vnh4y(iior).lt.minh4(mstr,iior))minh4(mstr,iior) = vnh4y(iior) 
      sumo2(mstr,iior) = sumo2(mstr,iior)+vo2y(iior) 
      if(vo2y(iior).gt.mxo2(mstr,iior))mxo2(mstr,iior) = vo2y(iior) 
      if(vo2y(iior).lt.mio2(mstr,iior))mio2(mstr,iior) = vo2y(iior) 
      sumca(mstr,iior) = sumca(mstr,iior)+chlay(iior) 
      sumcak(mstr,iior) = sumcak(mstr,iior)+chlaky(iior) 
      sumcag(mstr,iior) = sumcag(mstr,iior)+chlagy(iior) 
      sumcab(mstr,iior) = sumcab(mstr,iior)+chlaby(iior)
      sumCChlk(mstr,iior) = sumCChlk(mstr,iior)+ CChlky(iior) 
      sumCChlg(mstr,iior) = sumCChlg(mstr,iior)+ CChlgy(iior) 
      sumCChlb(mstr,iior) = sumCChlb(mstr,iior)+ CChlby(iior)
      if(chlay(iior).gt.mxchla(mstr,iior))                              &
     &mxchla(mstr,iior) = chlay(iior)                                   
      if(chlay(iior).lt.michla(mstr,iior))                              &
     &michla(mstr,iior) = chlay(iior)                                   
      svkigr(mstr,iior) = svkigr(mstr,iior)+vkigry(iior) 
      santbl(mstr,iior) = santbl(mstr,iior)+antbly(iior) 
                                                                       
      sumaki(mstr,iior) = sumaki(mstr,iior)+akiy(iior) 

      if(akiy(iior).gt.mxaki(mstr,iior))mxaki(mstr,iior) = akiy(iior) 
      if(akiy(iior).lt.miaki(mstr,iior))miaki(mstr,iior) = akiy(iior) 
      sumagr(mstr,iior) = sumagr(mstr,iior)+agry(iior) 
      if(agry(iior).gt.mxagr(mstr,iior))mxagr(mstr,iior) = agry(iior) 
      if(agry(iior).lt.miagr(mstr,iior))miagr(mstr,iior) = agry(iior) 
      sumabl(mstr,iior) = sumabl(mstr,iior)+ably(iior) 
      if(ably(iior).gt.mxabl(mstr,iior))mxabl(mstr,iior) = ably(iior) 
      if(ably(iior).lt.miabl(mstr,iior))miabl(mstr,iior) = ably(iior) 
!                                                                       
      sumzo(mstr,iior) = sumzo(mstr,iior)+zooiny(iior) 
      if(zooiny(iior).gt.mxzo(mstr,iior))mxzo(mstr,iior) = zooiny(iior) 
      if(zooiny(iior).lt.mizo(mstr,iior))mizo(mstr,iior) = zooiny(iior) 
      sumvph(mstr,iior) = sumvph(mstr,iior)+vphy(iior) 
      if(vphy(iior).gt.mxvph(mstr,iior))mxvph(mstr,iior) = vphy(iior) 
      if(vphy(iior).lt.mivph(mstr,iior))mivph(mstr,iior) = vphy(iior) 
      sumno3(mstr,iior) = sumno3(mstr,iior)+vno3y(iior) 
      if(vno3y(iior).gt.mxvno3(mstr,iior))                              &
     &mxvno3(mstr,iior) = vno3y(iior)                                   
      if(vno3y(iior).lt.mivno3(mstr,iior))                              &
     &mivno3(mstr,iior) = vno3y(iior)                                   
      sumno2(mstr,iior) = sumno2(mstr,iior)+vno2y(iior) 
      if(vno2(iior).gt.mxvno2(mstr,iior))mxvno2(mstr,iior) = vno2y(iior) 
      if(vno2(iior).lt.mivno2(mstr,iior))mivno2(mstr,iior) = vno2y(iior) 
      sumgp(mstr,iior) = sumgp(mstr,iior)+gelpy(iior) 
      if(gelpy(iior).gt.mxgp(mstr,iior))mxgp(mstr,iior) = gelpy(iior) 
      if(gelpy(iior).lt.migp(mstr,iior))migp(mstr,iior) = gelpy(iior) 
      sumsi(mstr,iior) = sumsi(mstr,iior)+siy(iior) 
      if(siy(iior).gt.mxsi(mstr,iior))mxsi(mstr,iior) = siy(iior) 
      if(siy(iior).lt.misi(mstr,iior))misi(mstr,iior) = siy(iior) 
      sumcal(mstr,iior) = sumcal(mstr,iior)+cay(iior) 
      if(cay(iior).gt.mxca(mstr,iior))mxca(mstr,iior) = cay(iior) 
      if(cay(iior).lt.mica(mstr,iior))mica(mstr,iior) = cay(iior) 
      summw(mstr,iior) = summw(mstr,iior)+mwy(iior) 
      if(mwy(iior).gt.mxmw(mstr,iior))mxmw(mstr,iior) = mwy(iior) 
      if(mwy(iior).lt.mimw(mstr,iior))mimw(mstr,iior) = mwy(iior) 
      sumlf(mstr,iior) = sumlf(mstr,iior)+lfy(iior) 
      if(lfy(iior).gt.mxlf(mstr,iior))mxlf(mstr,iior) = lfy(iior) 
      if(lfy(iior).lt.milf(mstr,iior))milf(mstr,iior) = lfy(iior) 
      sumss(mstr,iior) = sumss(mstr,iior)+ssalgy(iior) 
      if(ssalgy(iior).gt.mxSS(mstr,iior))mxSS(mstr,iior) = ssalgy(iior) 
      if(ssalgy(iior).lt.miSS(mstr,iior))miSS(mstr,iior) = ssalgy(iior) 
      scoli(mstr,iior) = scoli(mstr,iior)+coliy(iior) 
      if(coliy(iior).gt.mxcoli(mstr,iior))mxcoli(mstr,iior) = coliy(iior)                                   
      if(coliy(iior).lt.micoli(mstr,iior))micoli(mstr,iior) = coliy(iior)                                   
                                                                      
      sumdln(mstr,iior) = sumdln(mstr,iior)+dlarny(iior) 
      if(dlarny(iior).gt.mxdlan(mstr,iior))mxdlan(mstr,iior) = dlarny(iior)                                  
      if(dlarny(iior).lt.midlan(mstr,iior))midlan(mstr,iior) = dlarny(iior)                                  

      sumgsZn(mstr,iior) = sumgsZn(mstr,iior)+gsZny(iior) 
      if(gsZny(iior)>mxgsZn(mstr,iior))mxgsZn(mstr,iior) = gsZny(iior)                                  
      if(gsZny(iior)<migsZn(mstr,iior))migsZn(mstr,iior) = gsZny(iior)                                  
      sumglZn(mstr,iior) = sumglZn(mstr,iior)+glZny(iior) 
      if(glZny(iior)>mxglZn(mstr,iior))mxglZn(mstr,iior) = glZny(iior)                                  
      if(glZny(iior)<miglZn(mstr,iior))miglZn(mstr,iior) = glZny(iior)                                  
      sumgsCad(mstr,iior) = sumgsCad(mstr,iior)+gsCady(iior) 
      if(gsCady(iior)>mxgsCad(mstr,iior))mxgsCad(mstr,iior) = gsCady(iior)                                  
      if(gsCady(iior)<migsCad(mstr,iior))migsCad(mstr,iior) = gsCady(iior)                                  
      sumglCad(mstr,iior) = sumglCad(mstr,iior)+glCady(iior) 
      if(glCady(iior)>mxglCad(mstr,iior))mxglCad(mstr,iior) = glCady(iior)                                  
      if(glCady(iior)<miglCad(mstr,iior))miglCad(mstr,iior) = glCady(iior)                                  
      sumgsCu(mstr,iior) = sumgsCu(mstr,iior)+gsCuy(iior) 
      if(gsCuy(iior)>mxgsCu(mstr,iior))mxgsCu(mstr,iior) = gsCuy(iior)                                  
      if(gsCuy(iior)<migsCu(mstr,iior))migsCu(mstr,iior) = gsCuy(iior)                                  
      sumglCu(mstr,iior) = sumglCu(mstr,iior)+glCuy(iior) 
      if(glCuy(iior)>mxglCu(mstr,iior))mxglCu(mstr,iior) = glCuy(iior)                                  
      if(glCuy(iior)<miglCu(mstr,iior))miglCu(mstr,iior) = glCuy(iior)                                  
      sumgsNi(mstr,iior) = sumgsNi(mstr,iior)+gsNiy(iior) 
      if(gsNiy(iior)>mxgsNi(mstr,iior))mxgsNi(mstr,iior) = gsNiy(iior)                                  
      if(gsNiy(iior)<migsNi(mstr,iior))migsNi(mstr,iior) = gsNiy(iior)                                  
      sumglNi(mstr,iior) = sumglNi(mstr,iior)+glNiy(iior) 
      if(glNiy(iior)>mxglNi(mstr,iior))mxglNi(mstr,iior) = glNiy(iior)                                  
      if(glNiy(iior)<miglNi(mstr,iior))miglNi(mstr,iior) = glNiy(iior)                                  

!....Dreissena                                                          
      do 838 ndr=1,nndr 
      sidras(mstr,iior,ndr) = sidras(mstr,iior,ndr)+idrasy(iior,ndr) 
      sdrmas(mstr,iior,ndr) = sdrmas(mstr,iior,ndr)+drmasy(iior,ndr) 
      sdrakr(mstr,iior,ndr) = sdrakr(mstr,iior,ndr)+drakry(iior,ndr) 
      sdrbar(mstr,iior,ndr) = sdrbar(mstr,iior,ndr)+drbary(iior,ndr) 
      sdrmor(mstr,iior,ndr) = sdrmor(mstr,iior,ndr)+drmory(iior,ndr) 
      szdrg(mstr,iior,ndr) = szdrg(mstr,iior,ndr)+dreiy(iior,ndr) 
      szdrsg(mstr,iior,ndr) = szdrsg(mstr,iior,ndr)+dreisy(iior,ndr) 
  838 sgwdrg(mstr,iior,ndr) = sgwdrg(mstr,iior,ndr)+gwdrly(iior,ndr) 
!                                                                       
      scorIg(mstr,iior) = scorIg(mstr,iior)+coroy(iior) 
      scoIsg(mstr,iior) = scoIsg(mstr,iior)+corosy(iior) 
!                                                                       
      sumpfl(mstr,iior) = sumpfl(mstr,iior)+pfly(iior) 
      sumbal(mstr,iior) = sumbal(mstr,iior)+alby(iior) 
!                                                                       
      snaehr(mstr,iior) = snaehr(mstr,iior)+tpkiy(iior) !*vkigry(iior)+tpgry(iior)*(1.-vkigry(iior)-antbly(iior))+tpbly(iior)*antbly(iior) !löschen !                                        
                                                                       
      ssusn(mstr,iior) = ssusn(mstr,iior)+susny(iior) 
      sbettn(mstr,iior) = sbettn(mstr,iior)+bettny(iior) 
      sdon(mstr,iior) = sdon(mstr,iior)+dony(iior) 
      salgn(mstr,iior) = salgn(mstr,iior)+agrn4y(iior)                  &
     &+akin4y(iior)+abln4y(iior)                                        
      salNO3(mstr,iior) = salNO3(mstr,iior)+alNO3y(iior) 
      sFluN3(mstr,iior) = sFluN3(mstr,iior)+FluN3y(iior) 
      sJNO3(mstr,iior) = sJNO3(mstr,iior)+JNO3y(iior) 
      sJNH4(mstr,iior) = sJNH4(mstr,iior)+JNH4y(iior) 
      sJPO4(mstr,iior) = sJPO4(mstr,iior)+JPO4y(iior) 
      sJO2(mstr,iior) = sJO2(mstr,iior)+JO2y(iior) 
      sJSi(mstr,iior) = sJSi(mstr,iior)+JSiy(iior) 
      svx0(mstr,iior) = svx0(mstr,iior)+vx0y(iior) 
      svx02(mstr,iior) = svx02(mstr,iior)+vx02y(iior) 
      ssedx0(mstr,iior) = ssedx0(mstr,iior)+sedx0y(iior) 
      ssusno(mstr,iior) = ssusno(mstr,iior)+susnoy(iior) 
      ssedal(mstr,iior) = ssedal(mstr,iior)+sedagy(iior)                &
     &+sedaky(iior)+sedaby(iior)
      salgzo(mstr,iior) = salgzo(mstr,iior)+algzgy(iior)                &
     &+algzky(iior)+algzby(iior)                                        
      salgdr(mstr,iior) = salgdr(mstr,iior)+algdgy(iior)                &
     &+algdky(iior)+algdby(iior)                                        
      salgco(mstr,iior) = salgco(mstr,iior)+algcgy(iior)                &
     &+algcky(iior)+algcby(iior)                                        
      svoldr(mstr,iior) = svoldr(mstr,iior)+volfdy(iior) 
      sdrpfe(mstr,iior) = sdrpfe(mstr,iior)+drpfey(iior) 
      sabeow(mstr,iior) = sabeow(mstr,iior)+abowgy(iior)+abowky(iior) 
      sabeor(mstr,iior) = sabeor(mstr,iior)+aborgy(iior)+aborky(iior) 
      sdalg(mstr,iior) = sdalg(mstr,iior)+dalggy(iior)+dalgky(iior)     &
     &+dalgby(iior)                                                     
      sdalga(mstr,iior) = sdalga(mstr,iior)+dalagy(iior)+dalaky(iior)   &
     &+dalaby(iior)                                                     
      salmor(mstr,iior) = salmor(mstr,iior)+dgmory(iior)+dkmory(iior)   &
     &+dbmory(iior)                                                     
      sblmor(mstr,iior) = sblmor(mstr,iior)+dbmory(iior) 
      ssgo2n(mstr,iior) = ssgo2n(mstr,iior)+sgo2ny(iior) 
      ssdbsb(mstr,iior) = ssdbsb(mstr,iior)+sdbsby(iior) 
      ssoein(mstr,iior) = ssoein(mstr,iior)+so2eiy(iior) 
      ssalgo(mstr,iior) = ssalgo(mstr,iior)+salgoy(iior) 
      sbsbt(mstr,iior) = sbsbt(mstr,iior)+bsbty(iior) 
      s2algo(mstr,iior) = s2algo(mstr,iior)+dalgoy(iior) 
      s2algao(mstr,iior) = s2algao(mstr,iior)+dalaoy(iior) 
      sschlr(mstr,iior) = sschlr(mstr,iior)+schlry(iior) 
      sbsbbe(mstr,iior) = sbsbbe(mstr,iior)+bsbbey(iior) 
      so2phy(mstr,iior) = so2phy(mstr,iior)+o2ei1y(iior) 
      sro2dr(mstr,iior) = sro2dr(mstr,iior)+ro2dry(iior) 
      szooro(mstr,iior) = szooro(mstr,iior)+zoro2y(iior) 
      spo2p(mstr,iior) = spo2p(mstr,iior)+po2py(iior) 
      spo2r(mstr,iior) = spo2r(mstr,iior)+po2ry(iior) 
      sir(mstr,iior) = sir(mstr,iior)+iry(iior) 
      srmue(mstr,iior) = srmue(mstr,iior)+rmuasy(iior) 
      srakr(mstr,iior) = srakr(mstr,iior)+rakry(iior) 
      srbar(mstr,iior) = srbar(mstr,iior)+rbary(iior) 
      sffood(mstr,iior) = sffood(mstr,iior)+ffoody(iior) 
!.....sfik enthaelt die mittlere Produktionsraten der Algen             
!...fuer die 3 Algengruppen                                             
      sfik(mstr,iior) = sfik(mstr,iior)+fiy(iior)*vkigry(iior)          &
     &+figy(iior)*(1.-vkigry(iior)-antbly(iior))                        &
     &+fiby(iior)*antbly(iior)                                          
!                                                                       
!.....sfig wird mit dem Extinktionskoeffizient belegt                   
!                                                                       
      sfig(mstr,iior) = sfig(mstr,iior)+extky(iior) 
!.....sfib wird nicht mehr erzeugt                                      
      sfib(mstr,iior) = -1. 
      sakmua(mstr,iior) = sakmua(mstr,iior)+akmuey(iior) 
      sagmua(mstr,iior) = sagmua(mstr,iior)+agmuey(iior) 
      sabmua(mstr,iior) = sabmua(mstr,iior)+abmuey(iior) 
      sfheka(mstr,iior) = sfheka(mstr,iior)+fhey(iior) 
      sfhega(mstr,iior) = sfhega(mstr,iior)+fhegy(iior) 
      sfheba(mstr,iior) = sfheba(mstr,iior)+fheby(iior) 
      sakrau(mstr,iior) = sakrau(mstr,iior)+akry(iior) 
      sagrea(mstr,iior) = sagrea(mstr,iior)+agrey(iior) 
      sabrea(mstr,iior) = sabrea(mstr,iior)+abrey(iior) 
      sHNFmu(mstr,iior) = sHNFmu(mstr,iior)+HNFmuy(iior) 
      sHNFre(mstr,iior) = sHNFre(mstr,iior)+HNFrey(iior) 
      sHNFup(mstr,iior) = sHNFup(mstr,iior)+HNFupy(iior) 
      sHNFmo(mstr,iior) = sHNFmo(mstr,iior)+HNFmoy(iior) 
      sHNFex(mstr,iior) = sHNFex(mstr,iior)+HNFexy(iior) 
      sHNFdr(mstr,iior) = sHNFdr(mstr,iior)+HNFdry(iior) 
      sHNFz(mstr,iior) = sHNFz(mstr,iior)+HNFzy(iior) 
      sBACmu(mstr,iior) = sBACmu(mstr,iior)+BACmuy(iior) 
      sHNFBA(mstr,iior) = sHNFBA(mstr,iior)+HNFBAy(iior) 
      snl0(mstr,iior) = snl0(mstr,iior)+nl0y(iior) 
      spl0(mstr,iior) = spl0(mstr,iior)+pl0y(iior) 
!                                                                       
!                                                                       
!     Buhnenfelder                                                      
!                                                                       
      if(nbuhn(mstr).eq.1)then 
      bste(mstr,iior) = bste(mstr,iior)+btempy(iior) 
      if(btempy(iior).gt.bmxtem(mstr,iior))                             &
     &bmxtem(mstr,iior) = btempy(iior)                                  
      if(btempy(iior).lt.bmitem(mstr,iior))                             &
     &bmitem(mstr,iior) = btempy(iior)                                  
      bsno3(mstr,iior) = bsno3(mstr,iior)+bno3y(iior) 
      if(bno3y(iior).gt.bmxno3(mstr,iior))                              &
     &bmxno3(mstr,iior) = bno3y(iior)                                   
      if(bno3y(iior).lt.bmino3(mstr,iior))                              &
     &bmino3(mstr,iior) = bno3y(iior)                                   
      bsno2(mstr,iior) = bsno2(mstr,iior)+bno2y(iior) 
      if(bno2y(iior).gt.bmxno2(mstr,iior))                              &
     &bmxno2(mstr,iior) = bno2y(iior)                                   
      if(bno2y(iior).lt.bmino2(mstr,iior))                              &
     &bmino2(mstr,iior) = bno2y(iior)                                   
      bsn4(mstr,iior) = bsn4(mstr,iior)+bnh4y(iior) 
      if(bnh4y(iior).gt.bmxnh4(mstr,iior))                              &
     &bmxnh4(mstr,iior) = bnh4y(iior)                                   
      if(bnh4y(iior).lt.bminh4(mstr,iior))                              &
     &bminh4(mstr,iior) = bnh4y(iior)                                   
      bsgelp(mstr,iior) = bsgelp(mstr,iior)+bgelpy(iior) 
      if(bgelpy(iior).gt.bmxglp(mstr,iior))                             &
     &bmxglp(mstr,iior) = bgelpy(iior)                                  
      if(bgelpy(iior).lt.bmiglp(mstr,iior))                             &
     &bmiglp(mstr,iior) = bgelpy(iior)                                  
      bschla(mstr,iior) = bschla(mstr,iior)+bchlay(iior) 
      if(bchlay(iior).gt.bmxchl(mstr,iior))                             &
     &bmxchl(mstr,iior) = bchlay(iior)                                  
      if(bchlay(iior).lt.bmichl(mstr,iior))                             &
     &bmichl(mstr,iior) = bchlay(iior)                                  
      bsaki(mstr,iior) = bsaki(mstr,iior)+bakiy(iior) 
      if(bakiy(iior).gt.bmxaki(mstr,iior))                              &
     &bmxaki(mstr,iior) = bakiy(iior)                                   
      if(bakiy(iior).lt.bmiaki(mstr,iior))                              &
     &bmiaki(mstr,iior) = bakiy(iior)                                   
      bsagr(mstr,iior) = bsagr(mstr,iior)+bagry(iior) 
      if(bagry(iior).gt.bmxagr(mstr,iior))                              &
     &bmxagr(mstr,iior) = bagry(iior)                                   
      if(bagry(iior).lt.bmiagr(mstr,iior))                              &
     &bmiagr(mstr,iior) = bagry(iior)                                   
      bsabl(mstr,iior) = bsabl(mstr,iior)+bably(iior) 
      bsssal(mstr,iior) = bsssal(mstr,iior)+bssaly(iior) 
      if(bssaly(iior).gt.bmxssa(mstr,iior))                             &
     &bmxssa(mstr,iior) = bssaly(iior)                                  
      if(bssaly(iior).lt.bmissa(mstr,iior))                             &
     &bmissa(mstr,iior) = bssaly(iior)                                  
      bssi(mstr,iior) = bssi(mstr,iior)+bsiy(iior) 
      if(bsiy(iior).gt.bmxsi(mstr,iior))bmxsi(mstr,iior) = bsiy(iior) 
      if(bsiy(iior).lt.bmisi(mstr,iior))bmisi(mstr,iior) = bsiy(iior) 
      bszooi(mstr,iior) = bszooi(mstr,iior)+bzooiy(iior) 
      if(bzooiy(iior).gt.bmxzoo(mstr,iior))                             &
     &bmxzoo(mstr,iior) = bzooiy(iior)                                  
      if(bzooiy(iior).lt.bmizoo(mstr,iior))                             &
     &bmizoo(mstr,iior) = bzooiy(iior)                                  
      bsvbsb(mstr,iior) = bsvbsb(mstr,iior)+bvbsby(iior) 
      if(bvbsby(iior).gt.bmxbsb(mstr,iior))                             &
     &bmxbsb(mstr,iior) = bvbsby(iior)                                  
      if(bvbsby(iior).lt.bmibsb(mstr,iior))                             &
     &bmibsb(mstr,iior) = bvbsby(iior)                                  
      bsvcsb(mstr,iior) = bsvcsb(mstr,iior)+bvcsby(iior) 
      if(bvcsby(iior).gt.bmxcsb(mstr,iior))                             &
     &bmxcsb(mstr,iior) = bvcsby(iior)                                  
      if(bvcsby(iior).lt.bmicsb(mstr,iior))                             &
     &bmicsb(mstr,iior) = bvcsby(iior)                                  
      bsgsP(mstr,iior) = bsgsP(mstr,iior)+bgsPy(iior) 
      if(bgsPy(iior).gt.bmxgsP(mstr,iior))                              &
     &bmxgsP(mstr,iior) = bgsPy(iior)                                   
      if(bgsPy(iior).lt.bmigsP(mstr,iior))                              &
     &bmigsP(mstr,iior) = bgsPy(iior)                                   
      bsgsN(mstr,iior) = bsgsN(mstr,iior)+bgsNy(iior) 
      if(bgsNy(iior).gt.bmxgsN(mstr,iior))                              &
     &bmxgsN(mstr,iior) = bgsNy(iior)                                   
      if(bgsNy(iior).lt.bmigsN(mstr,iior))                              &
     &bmigsN(mstr,iior) = bgsNy(iior)                                   
      bso2(mstr,iior) = bso2(mstr,iior)+bo2y(iior) 
      if(bo2y(iior).gt.bmxo2(mstr,iior))bmxo2(mstr,iior) = bo2y(iior) 
      if(bo2y(iior).lt.bmio2(mstr,iior))bmio2(mstr,iior) = bo2y(iior) 
      bsmw(mstr,iior) = bsmw(mstr,iior)+bmwy(iior) 
      if(bmwy(iior).gt.bmxmw(mstr,iior))bmxmw(mstr,iior) = bmwy(iior) 
      if(bmwy(iior).lt.bmimw(mstr,iior))bmimw(mstr,iior) = bmwy(iior) 
      bslf(mstr,iior) = bslf(mstr,iior)+blfy(iior) 
      if(blfy(iior).gt.bmxlf(mstr,iior))bmxlf(mstr,iior) = blfy(iior) 
      if(blfy(iior).lt.bmilf(mstr,iior))bmilf(mstr,iior) = blfy(iior) 
      bsca(mstr,iior) = bsca(mstr,iior)+bcay(iior) 
      if(bcay(iior).gt.bmxca(mstr,iior))bmxca(mstr,iior) = bcay(iior) 
      if(bcay(iior).lt.bmica(mstr,iior))bmica(mstr,iior) = bcay(iior) 
      bsph(mstr,iior) = bsph(mstr,iior)+bphy(iior) 
      if(bphy(iior).gt.bmxph(mstr,iior))bmxph(mstr,iior) = bphy(iior) 
      if(bphy(iior).lt.bmiph(mstr,iior))bmiph(mstr,iior) = bphy(iior) 
      bsnl0(mstr,iior) = bsnl0(mstr,iior)+bnl0y(iior) 
      bspl0(mstr,iior) = bspl0(mstr,iior)+bpl0y(iior) 
      bscoli(mstr,iior) = bscoli(mstr,iior) + bcoliy(iior)

      bsgsZn(mstr,iior) = bsgsZn(mstr,iior) + bgsZny(iior)
      if(bgsZny(iior).gt.bmxgsZn(mstr,iior))bmxgsZn(mstr,iior) = bgsZny(iior) 
      if(bgsZny(iior).lt.bmigsZn(mstr,iior))bmigsZn(mstr,iior) = bgsZny(iior) 
      bsglZn(mstr,iior) = bsglZn(mstr,iior) + bglZny(iior)
      if(bglZny(iior).gt.bmxglZn(mstr,iior))bmxglZn(mstr,iior) = bglZny(iior) 
      if(bglZny(iior).lt.bmiglZn(mstr,iior))bmiglZn(mstr,iior) = bglZny(iior) 
      bsgsCad(mstr,iior) = bsgsCad(mstr,iior) + bgsCady(iior)
      if(bgsCady(iior).gt.bmxgsCad(mstr,iior))bmxgsCad(mstr,iior) = bgsCady(iior) 
      if(bgsCady(iior).lt.bmigsCad(mstr,iior))bmigsCad(mstr,iior) = bgsCady(iior) 
      bsglCad(mstr,iior) = bsglCad(mstr,iior) + bglCady(iior)
      if(bglCady(iior).gt.bmxglCad(mstr,iior))bmxglCad(mstr,iior) = bglCady(iior) 
      if(bglCady(iior).lt.bmiglCad(mstr,iior))bmiglCad(mstr,iior) = bglCady(iior) 
      bsgsCu(mstr,iior) = bsgsCu(mstr,iior) + bgsCuy(iior)
      if(bgsCuy(iior).gt.bmxgsCu(mstr,iior))bmxgsCu(mstr,iior) = bgsCuy(iior) 
      if(bgsCuy(iior).lt.bmigsCu(mstr,iior))bmigsCu(mstr,iior) = bgsCuy(iior) 
      bsglCu(mstr,iior) = bsglCu(mstr,iior) + bglCuy(iior)
      if(bglCuy(iior).gt.bmxglCu(mstr,iior))bmxglCu(mstr,iior) = bglCuy(iior) 
      if(bglCuy(iior).lt.bmiglCu(mstr,iior))bmiglCu(mstr,iior) = bglCuy(iior) 
      bsgsNi(mstr,iior) = bsgsNi(mstr,iior) + bgsNiy(iior)
      if(bgsNiy(iior).gt.bmxgsNi(mstr,iior))bmxgsNi(mstr,iior) = bgsNiy(iior) 
      if(bgsNiy(iior).lt.bmigsNi(mstr,iior))bmigsNi(mstr,iior) = bgsNiy(iior) 
      bsglNi(mstr,iior) = bsglNi(mstr,iior) + bglNiy(iior)
      if(bglNiy(iior).gt.bmxglNi(mstr,iior))bmxglNi(mstr,iior) = bglNiy(iior) 
      if(bglNiy(iior).lt.bmiglNi(mstr,iior))bmiglNi(mstr,iior) = bglNiy(iior) 
                                                                       
!......Raten                                                            
!                                                                       
      bnaehr(mstr,iior) = bnaehr(mstr,iior)+ btpkiy(iior)*bkigry(iior)  &
     &+btpgry(iior)*(1.-bkigry(iior)-bantby(iior))                      &
     &+btpbly(iior)*bantby(iior)                                        
!                                                                       
      bsdalg(mstr,iior) = bsdalg(mstr,iior)+bdakiy(iior)+bdagry(iior)   &
     &+bdably(iior)                                                     
      bsvkg(mstr,iior) = bsvkg(mstr,iior)+bkigry(iior) 
      bsantb(mstr,iior) = bsantb(mstr,iior)+bantby(iior) 
      bsdaa(mstr,iior) = bsdaa(mstr,iior)+bdaaky(iior)+bdaagy(iior)     &
     &+bdaaby(iior)                                                     
      bsseda(mstr,iior) = bsseda(mstr,iior)+bsedky(iior)+bsedgy(iior)   &
     &+bsedby(iior)                                                     
      bsalgz(mstr,iior) = bsalgz(mstr,iior)+bazoky(iior)+bazogy(iior)   &
     &+bazoby(iior)                                                     
      bsamor(mstr,iior) = bsamor(mstr,iior)+bkmory(iior)+bgmory(iior)   &
     &+bbmory(iior)                                                     
      bsadr(mstr,iior) = bsadr(mstr,iior)+badrky(iior)+badrgy(iior)     &
     &+badrby(iior)                                                     
!...Pseudofaces = 0.0 da keine Dreissena in Buhnenfeld                  
      bsalco(mstr,iior) = bsalco(mstr,iior)+bacoky(iior)+bacogy(iior)   &
     &+bacoby(iior)                                                     
!.....sfik enthaelt die mittlere Produktionsraten der Algen             
!...fuer die 3 Algengruppen                                             
      bsfik(mstr,iior) = bsfik(mstr,iior)+bfikay(iior)*bkigry(iior)     &
     &+bfigay(iior)*(1.-bkigry(iior)-bantby(iior))                      &
     &+bfibay(iior)*bantby(iior)                                        
!                                                                       
!....bsfig: Summenbildung des extiktionskoeffizients                    
      bsfig(mstr,iior) = bsfig(mstr,iior)+bextky(iior) 
      bskmue(mstr,iior) = bskmue(mstr,iior)+bkmuay(iior) 
      bsgmue(mstr,iior) = bsgmue(mstr,iior)+bgmuay(iior) 
      bsbmue(mstr,iior) = bsbmue(mstr,iior)+bbmuay(iior) 
      bshek(mstr,iior) = bshek(mstr,iior)+bfhkay(iior) 
      bsheg(mstr,iior) = bsheg(mstr,iior)+bfhgay(iior) 
      bsheb(mstr,iior) = bsheb(mstr,iior)+bfhbay(iior) 
      bskre(mstr,iior) = bskre(mstr,iior)+bkray(iior) 
      bsgre(mstr,iior) = bsgre(mstr,iior)+bgray(iior) 
      bsbre(mstr,iior) = bsbre(mstr,iior)+bbray(iior) 
      bschlk(mstr,iior) = bschlk(mstr,iior)+bchlky(iior) 
      bschlg(mstr,iior) = bschlg(mstr,iior)+bchlgy(iior) 
      bschlb(mstr,iior) = bschlb(mstr,iior)+bchlby(iior) 
      bsFlN3(mstr,iior) = bsFlN3(mstr,iior)+bFlN3y(iior) 
      bsBetN(mstr,iior) = bsBetN(mstr,iior)+bbetNy(iior) 
      bsJNO3(mstr,iior) = bsJNO3(mstr,iior)+bJNO3y(iior) 
      bsJNH4(mstr,iior) = bsJNH4(mstr,iior)+bJNH4y(iior) 
      bsJPO4(mstr,iior) = bsJPO4(mstr,iior)+bJPO4y(iior) 
      bsJO2(mstr,iior) = bsJO2(mstr,iior)+bJO2y(iior) 
      bsJSi(mstr,iior) = bsJSi(mstr,iior)+bJSiy(iior) 
!                                                                       
      endif 
!                                                                       
!...Ymax/Ymin-Bildung für grafische Ausgabe                             
!                                                                       
!      
      if(vbsby(iior).gt.Ymax(mstr,1))Ymax(mstr,1) = vbsby(iior) 
      if(vbsby(iior).lt.Ymin(mstr,1))Ymin(mstr,1) = vbsby(iior) 
      if(vcsby(iior).gt.Ymax(mstr,2))Ymax(mstr,2) = vcsby(iior) 
      if(vcsby(iior).lt.Ymin(mstr,2))Ymin(mstr,2) = vcsby(iior) 
      if(vnh4y(iior).gt.Ymax(mstr,3))Ymax(mstr,3) = vnh4y(iior) 
      if(vnh4y(iior).lt.Ymin(mstr,3))Ymin(mstr,3) = vnh4y(iior) 
      if(vno2y(iior).gt.Ymax(mstr,4))Ymax(mstr,4) = vno2y(iior) 
      if(vno2y(iior).lt.Ymin(mstr,4))Ymin(mstr,4) = vno2y(iior) 
      if(vno3y(iior).gt.Ymax(mstr,5))Ymax(mstr,5) = vno3y(iior) 
      if(vno3y(iior).lt.Ymin(mstr,5))Ymin(mstr,5) = vno3y(iior) 
      if(gsNy(iior).gt.Ymax(mstr,6))Ymax(mstr,6) = gsNy(iior) 
      if(gsNy(iior).lt.Ymin(mstr,6))Ymin(mstr,6) = gsNy(iior) 
      if(gelpy(iior).gt.Ymax(mstr,7))Ymax(mstr,7) = gelpy(iior) 
      if(gelpy(iior).lt.Ymin(mstr,7))Ymin(mstr,7) = gelpy(iior) 
      if(gsPy(iior).gt.Ymax(mstr,8))Ymax(mstr,8) = gsPy(iior) 
      if(gsPy(iior).lt.Ymin(mstr,8))Ymin(mstr,8) = gsPy(iior) 
      if(siy(iior).gt.Ymax(mstr,9))Ymax(mstr,9) = siy(iior) 
      if(siy(iior).lt.Ymin(mstr,9))Ymin(mstr,9) = siy(iior) 
      if(chlay(iior).gt.Ymax(mstr,10))Ymax(mstr,10) = chlay(iior) 
      if(chlay(iior).lt.Ymin(mstr,10))Ymin(mstr,10) = chlay(iior) 
      if(zooiny(iior).gt.Ymax(mstr,11))Ymax(mstr,11) = zooiny(iior) 
      if(zooiny(iior).lt.Ymin(mstr,11))Ymin(mstr,11) = zooiny(iior) 
      if(vphy(iior).gt.Ymax(mstr,12))Ymax(mstr,12) = vphy(iior) 
      if(vphy(iior).lt.Ymin(mstr,12))Ymin(mstr,12) = vphy(iior) 
      if(mwy(iior).gt.Ymax(mstr,13))Ymax(mstr,13) = mwy(iior) 
      if(mwy(iior).lt.Ymin(mstr,13))Ymin(mstr,13) = mwy(iior) 
      if(cay(iior).gt.Ymax(mstr,14))Ymax(mstr,14) = cay(iior) 
      if(cay(iior).lt.Ymin(mstr,14))Ymin(mstr,14) = cay(iior) 
      if(lfy(iior).gt.Ymax(mstr,15))Ymax(mstr,15) = lfy(iior) 
      if(lfy(iior).lt.Ymin(mstr,15))Ymin(mstr,15) = lfy(iior) 
      if(ssalgy(iior).gt.Ymax(mstr,16))Ymax(mstr,16) = ssalgy(iior) 
      if(ssalgy(iior).lt.Ymin(mstr,16))Ymin(mstr,16) = ssalgy(iior) 
      if(tempwy(iior).gt.Ymax(mstr,17))Ymax(mstr,17) = tempwy(iior) 
      if(tempwy(iior).lt.Ymin(mstr,17))Ymin(mstr,17) = tempwy(iior) 
      if(vo2y(iior).gt.Ymax(mstr,18))Ymax(mstr,18) = vo2y(iior) 
      if(vo2y(iior).lt.Ymin(mstr,18))Ymin(mstr,18) = vo2y(iior) 
      if(coliy(iior).gt.Ymax(mstr,19))Ymax(mstr,19) = coliy(iior) 
      if(coliy(iior).lt.Ymin(mstr,19))Ymin(mstr,19) = coliy(iior) 

      if(gsZny(iior).gt.Ymax(mstr,193))Ymax(mstr,193) = gsZny(iior) 
      if(gsZny(iior).lt.Ymin(mstr,193))Ymin(mstr,193) = gsZny(iior) 
      if(glZny(iior).gt.Ymax(mstr,194))Ymax(mstr,194) = glZny(iior) 
      if(glZny(iior).lt.Ymin(mstr,194))Ymin(mstr,194) = glZny(iior) 
      if(gsCady(iior).gt.Ymax(mstr,195))Ymax(mstr,195) = gsCady(iior) 
      if(gsCady(iior).lt.Ymin(mstr,195))Ymin(mstr,195) = gsCady(iior) 
      if(glCady(iior).gt.Ymax(mstr,196))Ymax(mstr,196) = glCady(iior) 
      if(glCady(iior).lt.Ymin(mstr,196))Ymin(mstr,196) = glCady(iior) 
      if(gsCuy(iior).gt.Ymax(mstr,197))Ymax(mstr,197) = gsCuy(iior) 
      if(gsCuy(iior).lt.Ymin(mstr,197))Ymin(mstr,197) = gsCuy(iior) 
      if(glCuy(iior).gt.Ymax(mstr,198))Ymax(mstr,198) = glCuy(iior) 
      if(glCuy(iior).lt.Ymin(mstr,198))Ymin(mstr,198) = glCuy(iior) 
      if(gsNiy(iior).gt.Ymax(mstr,199))Ymax(mstr,199) = gsNiy(iior) 
      if(gsNiy(iior).lt.Ymin(mstr,199))Ymin(mstr,199) = gsNiy(iior) 
      if(glNiy(iior).gt.Ymax(mstr,200))Ymax(mstr,200) = glNiy(iior) 
      if(glNiy(iior).lt.Ymin(mstr,200))Ymin(mstr,200) = glNiy(iior) 

      if(CHNFy(iior).le.0.0)then 
      Ymax(mstr,20) = -1. 
      Ymin(mstr,20) = -1 
      goto 139 
      endif 
      if(CHNFy(iior).gt.Ymax(mstr,20))Ymax(mstr,20) = CHNFy(iior) 
      if(CHNFy(iior).lt.Ymin(mstr,20))Ymin(mstr,20) = CHNFy(iior) 
  139 if(dly(iior).gt.Ymax(mstr,21))Ymax(mstr,21) = Dly(iior) 
      if(dly(iior).lt.Ymin(mstr,21))Ymin(mstr,21) = Dly(iior) 
      if(sedhy(mstr,iior).gt.Ymax(mstr,22))Ymax(mstr,22) = sedhy(mstr,iior)                                  
      if(sedhy(mstr,iior).lt.Ymin(mstr,22))Ymin(mstr,22) = sedhy(mstr,iior)                                  
      if(hpfl(mstr,iior).gt.Ymax(mstr,23))Ymax(mstr,23) = hpfl(mstr,iior)                                   
      if(hpfl(mstr,iior).lt.Ymin(mstr,23))Ymin(mstr,23) = hpfl(mstr,iior)                                   
      sbal = habgml(mstr,iior)+habkml(mstr,iior) 
      if(sbal.gt.Ymax(mstr,24))Ymax(mstr,24) = sbal 
      if(sbal.lt.Ymin(mstr,24))Ymin(mstr,24) = sbal 
      IF(hzdrel(mstr,iior,1).gt.Ymax(mstr,25))                          &
     &Ymax(mstr,25) = hzdrel(mstr,iior,1)                               
      if(hzdrsl(mstr,iior,1).gt.Ymax(mstr,25))                          &
     &Ymax(mstr,25) = hzdrsl(mstr,iior,1)                               
      IF(hzdrel(mstr,iior,1).lt.Ymin(mstr,25))                          &
     &Ymin(mstr,25) = hzdrel(mstr,iior,1)                               
      if(hzdrsl(mstr,iior,1).lt.Ymin(mstr,25))                          &
     &Ymin(mstr,25) = hzdrsl(mstr,iior,1)                               
      if(hgwdrl(mstr,iior,1).gt.Ymax(mstr,26))                          &
     &Ymax(mstr,26) = hgwdrl(mstr,iior,1)                               
      if(hgwdrl(mstr,iior,1).lt.Ymin(mstr,26))                          &
     &Ymin(mstr,26) = hgwdrl(mstr,iior,1)                               
      IF(hzdrel(mstr,iior,2).gt.Ymax(mstr,27))                          &
     &Ymax(mstr,27) = hzdrel(mstr,iior,2)                               
      if(hzdrsl(mstr,iior,2).gt.Ymax(mstr,27))                          &
     &Ymax(mstr,27) = hzdrsl(mstr,iior,2)                               
      IF(hzdrel(mstr,iior,2).lt.Ymin(mstr,27))                          &
     &Ymin(mstr,27) = hzdrel(mstr,iior,2)                               
      if(hzdrsl(mstr,iior,2).lt.Ymin(mstr,27))                          &
     &Ymin(mstr,27) = hzdrsl(mstr,iior,2)                               
      if(hgwdrl(mstr,iior,2).gt.Ymax(mstr,28))                          &
     &Ymax(mstr,28) = hgwdrl(mstr,iior,2)                               
      if(hgwdrl(mstr,iior,2).lt.Ymin(mstr,28))                          &
     &Ymin(mstr,28) = hgwdrl(mstr,iior,2)                               
      if(dlarny(iior).gt.Ymax(mstr,29))Ymax(mstr,29) = dlarny(iior) 
      if(dlarny(iior).lt.Ymin(mstr,29))Ymin(mstr,29) = dlarny(iior) 
      if(idrasy(iior,1).gt.Ymax(mstr,30))Ymax(mstr,30) = idrasy(iior,1) 
      if(idrasy(iior,1).lt.Ymin(mstr,30))Ymin(mstr,30) = idrasy(iior,1) 
      if(idrasy(iior,2).gt.Ymax(mstr,31))Ymax(mstr,31) = idrasy(iior,2) 
      if(idrasy(iior,2).lt.Ymin(mstr,31))Ymin(mstr,31) = idrasy(iior,2) 
      if(drmasy(iior,1).gt.Ymax(mstr,32))Ymax(mstr,32) = drmasy(iior,1) 
      if(drmasy(iior,1).lt.Ymin(mstr,32))Ymin(mstr,32) = drmasy(iior,1) 
      if(drmasy(iior,2).gt.Ymax(mstr,33))Ymax(mstr,33) = drmasy(iior,2) 
      if(drmasy(iior,2).lt.Ymin(mstr,33))Ymin(mstr,33) = drmasy(iior,2) 
      if(volfdy(iior).gt.Ymax(mstr,34))Ymax(mstr,34) = volfdy(iior) 
      if(volfdy(iior).lt.Ymin(mstr,34))Ymin(mstr,34) = volfdy(iior) 
      if(drakry(iior,1).gt.Ymax(mstr,35))Ymax(mstr,35) = drakry(iior,1) 
      if(drakry(iior,1).lt.Ymin(mstr,35))Ymin(mstr,35) = drakry(iior,1) 
      if(drakry(iior,2).gt.Ymax(mstr,36))Ymax(mstr,36) = drakry(iior,2) 
      if(drakry(iior,2).lt.Ymin(mstr,36))Ymin(mstr,36) = drakry(iior,2) 
      if(drbary(iior,1).gt.Ymax(mstr,37))Ymax(mstr,37) = drbary(iior,1) 
      if(drbary(iior,1).lt.Ymin(mstr,37))Ymin(mstr,37) = drbary(iior,1) 
      if(drbary(iior,2).gt.Ymax(mstr,38))Ymax(mstr,38) = drbary(iior,2) 
      if(drbary(iior,2).lt.Ymin(mstr,38))Ymin(mstr,38) = drbary(iior,2) 
      if(drmory(iior,1).gt.Ymax(mstr,39))Ymax(mstr,39) = drmory(iior,1) 
      if(drmory(iior,1).lt.Ymin(mstr,39))Ymin(mstr,39) = drmory(iior,1) 
      if(drmory(iior,2).gt.Ymax(mstr,40))Ymax(mstr,40) = drmory(iior,2) 
      if(drmory(iior,2).lt.Ymin(mstr,40))Ymin(mstr,40) = drmory(iior,2) 
      if(ffoody(iior).gt.Ymax(mstr,41))Ymax(mstr,41) = ffoody(iior) 
      if(ffoody(iior).lt.Ymin(mstr,41))Ymin(mstr,41) = ffoody(iior) 
      sco = hcoro2(mstr,iior,1)+hcoro2(mstr,iior,2)+hcoro2(mstr,iior,3) &
     &+hcoro2(mstr,iior,4)+hcoro2(mstr,iior,5)                          
      if(sco.gt.Ymax(mstr,42))Ymax(mstr,42) = sco 
      if(sco.lt.Ymin(mstr,42))Ymin(mstr,42) = sco 
      scoS = hcos2(mstr,iior,1)+hcos2(mstr,iior,2)+hcos2(mstr,iior,3)   &
     &+hcos2(mstr,iior,4)+hcos2(mstr,iior,5)                            
      if(scoS.gt.Ymax(mstr,43))Ymax(mstr,43) = scoS 
      if(scoS.lt.Ymin(mstr,43))Ymin(mstr,43) = scoS 
      salC = akiy(iior)*Caki+ably(iior)*Cabl+agry(iior)*Cagr 
      if(salC.gt.Ymax(mstr,44))Ymax(mstr,44) = salC 
      if(salC.lt.Ymin(mstr,44))Ymin(mstr,44) = salC 
      if((akiy(iior)+agry(iior)+ably(iior)).gt.0.0)                     &
     &salw = ((dalggy(iior)+dalgky(iior)+dalgby(iior))*24.)             &
     &/(akiy(iior)+agry(iior)+ably(iior))                               
      if(salw.gt.Ymax(mstr,45))Ymax(mstr,45) = salw 
      if(salw.lt.Ymin(mstr,45))Ymin(mstr,45) = salw 
      salR = ((dalagy(iior)+dalaky(iior)+dalaby(iior))*24.) 
                                                                        
      if(salR.gt.Ymax(mstr,46))Ymax(mstr,46) = salR 
      if(salR.lt.Ymin(mstr,46))Ymin(mstr,46) = salR 
      if((akiy(iior)+agry(iior)+ably(iior)).gt.0.0)                     &
     &salM = ((dgmory(iior)+dkmory(iior)+dbmory(iior))*24.)             &
     &/(akiy(iior)+agry(iior)+ably(iior))                               
      if(salM.gt.Ymax(mstr,47))Ymax(mstr,47) = salM 
      if(salM.lt.Ymin(mstr,47))Ymin(mstr,47) = salM 
      if((akiy(iior)+agry(iior)+ably(iior)).gt.0.0)                     &
     &salS = ((sedagy(iior)+sedaky(iior)+sedaby(iior))*24.)             &
     &/(akiy(iior)+agry(iior)+ably(iior))                               
      if(salS.gt.Ymax(mstr,48))Ymax(mstr,48) = salS 
      if(salS.lt.Ymin(mstr,48))Ymin(mstr,48) = salS 
      if((akiy(iior)+agry(iior)+ably(iior)).gt.0.0)                     &
     &salZ = ((algzgy(iior)+algzky(iior)+algzby(iior))*24.)             &
     &/(akiy(iior)+agry(iior)+ably(iior))                               
      if(salZ.gt.Ymax(mstr,49))Ymax(mstr,49) = salZ 
      if(salZ.lt.Ymin(mstr,49))Ymin(mstr,49) = salZ 
      if((akiy(iior)+agry(iior)+ably(iior)).gt.0.0)                     &
     &salD = ((algdgy(iior)+algdky(iior)+algdby(iior))*24.)             &
     &/(akiy(iior)+agry(iior)+ably(iior))                               
      if(salD.gt.Ymax(mstr,50))Ymax(mstr,50) = salD 
      if(salD.lt.Ymin(mstr,50))Ymin(mstr,50) = salD 
      if(drpfey(iior).gt.Ymax(mstr,51))Ymax(mstr,51) = drpfey(iior) 
      if(drpfey(iior).lt.Ymin(mstr,51))Ymin(mstr,51) = drpfey(iior) 
      ztp = tpkiy(iior)*vkigry(iior)+tpgry(iior)                        &
     &*(1.-vkigry(iior)-antbly(iior))+tpbly(iior)*antbly(iior)          
      if(ztp.gt.Ymax(mstr,52))Ymax(mstr,52) = ztp 
      if(ztp.lt.Ymin(mstr,52))Ymin(mstr,52) = ztp 
      if(vkigry(iior).gt.Ymax(mstr,53))Ymax(mstr,53) = vkigry(iior) 
      if(vkigry(iior).lt.Ymin(mstr,53))Ymin(mstr,53) = vkigry(iior) 
      if(antbly(iior).gt.Ymax(mstr,54))Ymax(mstr,54) = antbly(iior) 
      if(antbly(iior).lt.Ymin(mstr,54))Ymin(mstr,54) = antbly(iior) 
      if((akiy(iior)+agry(iior)+ably(iior)).gt.0.0)                     &
     &salco = ((algcgy(iior)+algcky(iior)+algcby(iior))*24.)            &
     &/(akiy(iior)+agry(iior)+ably(iior))                               
      if(salco.gt.Ymax(mstr,55))Ymax(mstr,55) = salco 
      if(salco.lt.Ymin(mstr,55))Ymin(mstr,55) = salco 
      if(fiy(iior).gt.Ymax(mstr,56))Ymax(mstr,56) = fiy(iior) 
      if(fiy(iior).lt.Ymin(mstr,56))Ymin(mstr,56) = fiy(iior) 
      if(extky(iior).gt.Ymax(mstr,57))Ymax(mstr,57) = extky(iior) 
      if(extky(iior).lt.Ymin(mstr,57))Ymin(mstr,57) = extky(iior) 
      if(fiby(iior).gt.Ymax(mstr,58))Ymax(mstr,58) = fiby(iior) 
      if(fiby(iior).lt.Ymin(mstr,58))Ymin(mstr,58) = fiby(iior) 
      if(akmuey(iior).gt.Ymax(mstr,59))Ymax(mstr,59) = akmuey(iior) 
      if(akmuey(iior).lt.Ymin(mstr,59))Ymin(mstr,59) = akmuey(iior) 
      if(agmuey(iior).gt.Ymax(mstr,60))Ymax(mstr,60) = agmuey(iior) 
      if(agmuey(iior).lt.Ymin(mstr,60))Ymin(mstr,60) = agmuey(iior) 
      if(abmuey(iior).gt.Ymax(mstr,61))Ymax(mstr,61) = abmuey(iior) 
      if(abmuey(iior).lt.Ymin(mstr,61))Ymin(mstr,61) = abmuey(iior) 
      if(fhey(iior).gt.Ymax(mstr,62))Ymax(mstr,62) = fhey(iior) 
      if(fhey(iior).lt.Ymin(mstr,62))Ymin(mstr,62) = fhey(iior) 
      if(fhegy(iior).gt.Ymax(mstr,63))Ymax(mstr,63) = fhegy(iior) 
      if(fhegy(iior).lt.Ymin(mstr,63))Ymin(mstr,63) = fhegy(iior) 
      if(fheby(iior).gt.Ymax(mstr,64))Ymax(mstr,64) = fheby(iior) 
      if(fheby(iior).lt.Ymin(mstr,64))Ymin(mstr,64) = fheby(iior) 
      if(akry(iior).gt.Ymax(mstr,65))Ymax(mstr,65) = akry(iior) 
      if(akry(iior).lt.Ymin(mstr,65))Ymin(mstr,65) = akry(iior) 
      if(agrey(iior).gt.Ymax(mstr,66))Ymax(mstr,66) = agrey(iior) 
      if(agrey(iior).lt.Ymin(mstr,66))Ymin(mstr,66) = agrey(iior) 
      if(abrey(iior).gt.Ymax(mstr,67))Ymax(mstr,67) = abrey(iior) 
      if(abrey(iior).lt.Ymin(mstr,67))Ymin(mstr,67) = abrey(iior) 
      if(chlaky(iior).gt.Ymax(mstr,68))Ymax(mstr,68) = chlaky(iior) 
      if(chlaky(iior).lt.Ymin(mstr,68))Ymin(mstr,68) = chlaky(iior) 
      if(chlagy(iior).gt.Ymax(mstr,69))Ymax(mstr,69) = chlagy(iior) 
      if(chlagy(iior).lt.Ymin(mstr,69))Ymin(mstr,69) = chlagy(iior) 
      if(chlaby(iior).gt.Ymax(mstr,70))Ymax(mstr,70) = chlaby(iior) 
      if(chlaby(iior).lt.Ymin(mstr,70))Ymin(mstr,70) = chlaby(iior) 

      if(cchlky(iior).gt.Ymax(mstr,187))Ymax(mstr,187) = cchlky(iior) 
      if(cchlky(iior).lt.Ymin(mstr,187))Ymin(mstr,187) = cchlky(iior) 
      if(cchlgy(iior).gt.Ymax(mstr,188))Ymax(mstr,188) = cchlgy(iior) 
      if(cchlgy(iior).lt.Ymin(mstr,188))Ymin(mstr,188) = cchlgy(iior) 
      if(cchlby(iior).gt.Ymax(mstr,189))Ymax(mstr,189) = cchlby(iior) 
      if(cchlby(iior).lt.Ymin(mstr,189))Ymin(mstr,189) = cchlby(iior) 



      if(iry(iior).gt.Ymax(mstr,71))Ymax(mstr,71) = iry(iior) 
      if(iry(iior).lt.Ymin(mstr,71))Ymin(mstr,71) = iry(iior) 
      if(rmuasy(iior).gt.Ymax(mstr,72))Ymax(mstr,72) = rmuasy(iior) 
      if(rmuasy(iior).lt.Ymin(mstr,72))Ymin(mstr,72) = rmuasy(iior) 
      if(rakry(iior).gt.Ymax(mstr,73))Ymax(mstr,73) = rakry(iior) 
      if(rakry(iior).lt.Ymin(mstr,73))Ymin(mstr,73) = rakry(iior) 
      if(rbary(iior).gt.Ymax(mstr,74))Ymax(mstr,74) = rbary(iior) 
      if(rbary(iior).lt.Ymin(mstr,74))Ymin(mstr,74) = rbary(iior) 

      cbsbab = vbsby(iior)-akiy(iior)*Caki*bsbki+ably(iior)*Cabl*bsbbl+agry(iior)*Cagr*bsbgr+(zooiny(iior)*GRote/1000.)*bsbzoo
    
      if(cbsbab.gt.Ymax(mstr,75))Ymax(mstr,75) = cbsbab 
      if(cbsbab.lt.Ymin(mstr,75))Ymin(mstr,75) = cbsbab 
      abbau = -.1 
      if(iwsim.eq.3)abbau = vbsby(iior)/vcsby(iior) 
      if(abbau.gt.Ymax(mstr,76))Ymax(mstr,76) = abbau 
      if(abbau.lt.Ymin(mstr,76))Ymin(mstr,76) = abbau 
      if(CMy(iior).gt.Ymax(mstr,77))Ymax(mstr,77) = CMy(iior) 
      if(CMy(iior).lt.Ymin(mstr,77))Ymin(mstr,77) = CMy(iior) 
      if(BACy(iior).gt.Ymax(mstr,78))Ymax(mstr,78) = BACy(iior) 
      if(BACy(iior).lt.Ymin(mstr,78))Ymin(mstr,78) = BACy(iior) 
      if(CDy(1,iior).gt.Ymax(mstr,79))Ymax(mstr,79) = CDy(1,iior) 
      if(CDy(1,iior).lt.Ymin(mstr,79))Ymin(mstr,79) = CDy(1,iior) 
      if(CDy(2,iior).gt.Ymax(mstr,80))Ymax(mstr,80) = CDy(2,iior) 
      if(CDy(2,iior).lt.Ymin(mstr,80))Ymin(mstr,80) = CDy(2,iior) 
      if(CPy(1,iior).gt.Ymax(mstr,81))Ymax(mstr,81) = CPy(1,iior) 
      if(CPy(1,iior).lt.Ymin(mstr,81))Ymin(mstr,81) = CPy(1,iior) 
      if(CPy(2,iior).gt.Ymax(mstr,82))Ymax(mstr,82) = CPy(2,iior) 
      if(CPy(2,iior).lt.Ymin(mstr,82))Ymin(mstr,82) = CPy(2,iior) 
      if(BACmuy(iior).gt.Ymax(mstr,83))Ymax(mstr,83) = BACmuy(iior) 
      if(BACmuy(iior).lt.Ymin(mstr,83))Ymin(mstr,83) = BACmuy(iior) 
      if(HNFBAy(iior).gt.Ymax(mstr,84))Ymax(mstr,84) = HNFBAy(iior) 
      if(HNFBAy(iior).lt.Ymin(mstr,84))Ymin(mstr,84) = HNFBAy(iior) 
      if(BVHNFy(iior).le.0.0)then 
      Ymax(mstr,85) = -1 
      Ymin(mstr,85) = -1 
      goto 791 
      endif 
      HNFin = CHNFy(iior)*1.e6/(BVHNFy(iior)*0.22) 
      if(HNFin.gt.Ymax(mstr,85))Ymax(mstr,85) = HNFin 
      if(HNFin.lt.Ymin(mstr,85))Ymin(mstr,85) = HNFin 
  791 if(HNFupy(iior).gt.Ymax(mstr,86))Ymax(mstr,86) = HNFupy(iior) 
      if(HNFupy(iior).lt.Ymin(mstr,86))Ymin(mstr,86) = HNFupy(iior) 
      if(HNFrey(iior).gt.Ymax(mstr,87))Ymax(mstr,87) = HNFrey(iior) 
      if(HNFrey(iior).lt.Ymin(mstr,87))Ymin(mstr,87) = HNFrey(iior) 
      if(HNFexy(iior).gt.Ymax(mstr,88))Ymax(mstr,88) = HNFexy(iior) 
      if(HNFexy(iior).lt.Ymin(mstr,88))Ymin(mstr,88) = HNFexy(iior) 
      if(HNFmoy(iior).gt.Ymax(mstr,89))Ymax(mstr,89) = HNFmoy(iior) 
      if(HNFmoy(iior).lt.Ymin(mstr,89))Ymin(mstr,89) = HNFmoy(iior) 
      if(HNFmuy(iior).gt.Ymax(mstr,90))Ymax(mstr,90) = HNFmuy(iior) 
      if(HNFmuy(iior).lt.Ymin(mstr,90))Ymin(mstr,90) = HNFmuy(iior) 
      if(HNFzy(iior).gt.Ymax(mstr,91))Ymax(mstr,91) = HNFzy(iior) 
      if(HNFzy(iior).lt.Ymin(mstr,91))Ymin(mstr,91) = HNFzy(iior) 
      if(HNFdry(iior).gt.Ymax(mstr,92))Ymax(mstr,92) = HNFdry(iior) 
      if(HNFdry(iior).lt.Ymin(mstr,92))Ymin(mstr,92) = HNFdry(iior) 
      if(susny(iior).gt.Ymax(mstr,93))Ymax(mstr,93) = susny(iior) 
      if(susny(iior).lt.Ymin(mstr,93))Ymin(mstr,93) = susny(iior) 
      if(bettny(iior).gt.Ymax(mstr,94))Ymax(mstr,94) = bettny(iior) 
      if(bettny(iior).lt.Ymin(mstr,94))Ymin(mstr,94) = bettny(iior) 
      if(dony(iior).gt.Ymax(mstr,95))Ymax(mstr,95) = dony(iior) 
      if(dony(iior).lt.Ymin(mstr,95))Ymin(mstr,95) = dony(iior) 
      saN = agrn4y(iior)+akin4y(iior)+abln4y(iior) 
      if(saN.gt.Ymax(mstr,96))Ymax(mstr,96) = saN 
      if(saN.lt.Ymin(mstr,96))Ymin(mstr,96) = saN 
      if(FluN3y(iior).gt.Ymax(mstr,97))Ymax(mstr,97) = FluN3y(iior) 
      if(FluN3y(iior).lt.Ymin(mstr,97))Ymin(mstr,97) = FluN3y(iior) 
      vx0mue = vx0y(iior)*1000. 
      if(vx0mue.lt.0.0)vx0mue = -1. 
      if(vx0mue.gt.Ymax(mstr,98))Ymax(mstr,98) = vx0mue 
      if(vx0mue.lt.Ymin(mstr,98))Ymin(mstr,98) = vx0mue 
      if(sedx0y(iior).gt.Ymax(mstr,99))Ymax(mstr,99) = sedx0y(iior) 
      if(sedx0y(iior).lt.Ymin(mstr,99))Ymin(mstr,99) = sedx0y(iior) 
      vx02mu = vx02y(iior)*1000. 
      if(vx02mu.lt.0.0)vx02mu = -1. 
      if(vx02mu.gt.Ymax(mstr,100))ymax(mstr,100) = vx02mu 
      if(vx02mu.lt.Ymin(mstr,100))ymin(mstr,100) = vx02mu 
      Ymax(mstr,101) = (Ymax(mstr,93)+Ymax(mstr,94))*4.33 
      Ymin(mstr,101) = (Ymin(mstr,93)+Ymin(mstr,94))*4.33 
      if(dalgoy(iior).gt.Ymax(mstr,102))Ymax(mstr,102) = dalgoy(iior) 
      if(dalgoy(iior).lt.Ymin(mstr,102))Ymin(mstr,102) = dalgoy(iior) 
      if(dalaoy(iior).gt.Ymax(mstr,103))Ymax(mstr,103) = dalaoy(iior) 
      if(dalaoy(iior).lt.Ymin(mstr,103))Ymin(mstr,103) = dalaoy(iior) 
      if(bsbty(iior).gt.Ymax(mstr,104))Ymax(mstr,104) = bsbty(iior) 
      if(bsbty(iior).lt.Ymin(mstr,104))Ymin(mstr,104) = bsbty(iior) 
      if(schlry(iior).gt.Ymax(mstr,105))Ymax(mstr,105) = schlry(iior) 
      if(schlry(iior).lt.Ymin(mstr,105))Ymin(mstr,105) = schlry(iior) 
      if(bFlN3y(iior).gt.Ymax(mstr,106))Ymax(mstr,106) = bFlN3y(iior) 
      if(bFlN3y(iior).lt.Ymin(mstr,106))Ymin(mstr,106) = bFLN3y(iior) 
      if(bsbbey(iior).gt.Ymax(mstr,107))Ymax(mstr,107) = bsbbey(iior) 
      if(bsbbey(iior).lt.Ymin(mstr,107))Ymin(mstr,107) = bsbbey(iior) 
      if(o2ei1y(iior).gt.Ymax(mstr,108))Ymax(mstr,108) = o2ei1y(iior) 
      if(o2ei1y(iior).lt.Ymin(mstr,108))Ymin(mstr,108) = o2ei1y(iior) 
      sabow = abowgy(iior)+abowky(iior) 
      sabor = aborgy(iior)+aborky(iior) 
      if(sabow.gt.Ymax(mstr,109))Ymax(mstr,109) = sabow 
      if(sabow.lt.Ymin(mstr,109))Ymin(mstr,109) = sabow 
      if(sabor.gt.Ymax(mstr,110))Ymax(mstr,110) = sabor 
      if(sabor.lt.Ymin(mstr,110))Ymin(mstr,110) = sabor 
      if(ro2dry(iior).gt.Ymax(mstr,111))Ymax(mstr,111) = ro2dry(iior) 
      if(ro2dry(iior).lt.Ymin(mstr,111))Ymin(mstr,111) = ro2dry(iior) 
      if(zoro2y(iior).gt.Ymax(mstr,112))Ymax(mstr,112) = zoro2y(iior) 
      if(zoro2y(iior).lt.Ymin(mstr,112))Ymin(mstr,112) = zoro2y(iior) 
      if(po2py(iior).gt.Ymax(mstr,113))Ymax(mstr,113) = po2py(iior) 
      if(po2py(iior).lt.Ymin(mstr,113))Ymin(mstr,113) = po2py(iior) 
      if(po2ry(iior).gt.Ymax(mstr,114))Ymax(mstr,114) = po2ry(iior) 
      if(po2ry(iior).lt.Ymin(mstr,114))Ymin(mstr,114) = po2ry(iior) 
      if(tracer(iior).gt.Ymax(mstr,169))Ymax(mstr,169) = tracer(iior) 
      if(tracer(iior).lt.Ymin(mstr,169))Ymin(mstr,169) = tracer(iior) 
      if(bvbsby(iior).gt.Ymax(mstr,115))Ymax(mstr,115) = bvbsby(iior) 
      if(bvbsby(iior).lt.Ymin(mstr,115))Ymin(mstr,115) = bvbsby(iior) 
      if(bvcsby(iior).gt.Ymax(mstr,116))Ymax(mstr,116) = bvcsby(iior) 
      if(bvcsby(iior).lt.Ymin(mstr,116))Ymin(mstr,116) = bvcsby(iior) 
      if(bnh4y(iior).gt.Ymax(mstr,117))Ymax(mstr,117) = bnh4y(iior) 
      if(bnh4y(iior).lt.Ymin(mstr,117))Ymin(mstr,117) = bnh4y(iior) 
      if(bno2y(iior).gt.Ymax(mstr,118))Ymax(mstr,118) = bno2y(iior) 
      if(bno2y(iior).lt.Ymin(mstr,118))Ymin(mstr,118) = bno2y(iior) 
      if(bno3y(iior).gt.Ymax(mstr,119))Ymax(mstr,119) = bno3y(iior) 
      if(bno3y(iior).lt.Ymin(mstr,119))Ymin(mstr,119) = bno3y(iior) 
      if(bgsNy(iior).gt.Ymax(mstr,120))Ymax(mstr,120) = bgsNy(iior) 
      if(bgsNy(iior).lt.Ymin(mstr,120))Ymin(mstr,120) = bgsNy(iior) 
      if(bgelpy(iior).gt.Ymax(mstr,121))Ymax(mstr,121) = bgelpy(iior) 
      if(bgelpy(iior).lt.Ymin(mstr,121))Ymin(mstr,121) = bgelpy(iior) 
      if(bgsPy(iior).gt.Ymax(mstr,122))Ymax(mstr,122) = bgsPy(iior) 
      if(bgsPy(iior).lt.Ymin(mstr,122))Ymin(mstr,122) = bgsPy(iior) 
      if(bsiy(iior).gt.Ymax(mstr,123))Ymax(mstr,123) = bsiy(iior) 
      if(bsiy(iior).lt.Ymin(mstr,123))Ymin(mstr,123) = bsiy(iior) 
      if(bchlay(iior).gt.Ymax(mstr,124))Ymax(mstr,124) = bchlay(iior) 
      if(bchlay(iior).lt.Ymin(mstr,124))Ymin(mstr,124) = bchlay(iior) 
      if(bzooiy(iior).gt.Ymax(mstr,125))Ymax(mstr,125) = bzooiy(iior) 
      if(bzooiy(iior).lt.Ymin(mstr,125))Ymin(mstr,125) = bzooiy(iior) 
      if(bphy(iior).gt.Ymax(mstr,126))Ymax(mstr,126) = bphy(iior) 
      if(bphy(iior).lt.Ymin(mstr,126))Ymin(mstr,126) = bphy(iior) 
      if(bmwy(iior).gt.Ymax(mstr,127))Ymax(mstr,127) = bmwy(iior) 
      if(bmwy(iior).lt.Ymin(mstr,127))Ymin(mstr,127) = bmwy(iior) 
      if(bcay(iior).gt.Ymax(mstr,128))Ymax(mstr,128) = bcay(iior) 
      if(bcay(iior).lt.Ymin(mstr,128))Ymin(mstr,128) = bcay(iior) 
      if(blfy(iior).gt.Ymax(mstr,129))Ymax(mstr,129) = blfy(iior) 
      if(blfy(iior).lt.Ymin(mstr,129))Ymin(mstr,129) = blfy(iior) 
      if(bssaly(iior).gt.Ymax(mstr,130))Ymax(mstr,130) = bssaly(iior) 
      if(bssaly(iior).lt.Ymin(mstr,130))Ymin(mstr,130) = bssaly(iior) 
      if(btempy(iior).gt.Ymax(mstr,131))Ymax(mstr,131) = btempy(iior) 
      if(btempy(iior).lt.Ymin(mstr,131))Ymin(mstr,131) = btempy(iior) 
      if(bo2y(iior).gt.Ymax(mstr,132))Ymax(mstr,132) = bo2y(iior) 
      if(bo2y(iior).lt.Ymin(mstr,132))Ymin(mstr,132) = bo2y(iior) 
      Ymax(mstr,133) = -1. 
      Ymin(mstr,133) = -1. 
      if(bakiy(iior).le.0.0)bakiy(iior) = 0.0000000001 
      if(bagry(iior).le.0.0)bagry(iior) = 0.0000000001 
      if(bably(iior).le.0.0)bably(iior) = 0.0000000001 
      bakg = bakiy(iior)*Caki+bagry(iior)*Cagr+bably(iior)*Cabl 
      if(bakg.gt.Ymax(mstr,134))Ymax(mstr,134) = bakg 
      if(bakg.lt.Ymin(mstr,134))Ymin(mstr,134) = bakg 
      bdaW = (bdakiy(iior)+bdagry(iior)+bdably(iior))*24.               &
     &/(bakiy(iior)+bagry(iior)+bably(iior))                            
      if(bdaW.gt.Ymax(mstr,135))Ymax(mstr,135) = bdaW 
      if(bdaW.lt.Ymin(mstr,135))Ymin(mstr,135) = bdaW 
      bdaR = (bdaaky(iior)+bdaagy(iior)+bdaagy(iior))*24.               &
     &/(bakiy(iior)+bagry(iior)+bably(iior))                            
      if(bdaR.gt.Ymax(mstr,136))Ymax(mstr,136) = bdaR 
      if(bdaR.lt.Ymin(mstr,136))Ymin(mstr,136) = bdaR 
      baM = (bkmory(iior)+bgmory(iior)+bbmory(iior))*24.                &
     &/(bakiy(iior)+bagry(iior)+bably(iior))                            
      if(baM.gt.Ymax(mstr,137))Ymax(mstr,137) = baM 
      if(baM.lt.Ymin(mstr,137))Ymin(mstr,137) = baM 
      baS = (bsedky(iior)+bsedgy(iior)+bsedby(iior))*24.                &
     &/(bakiy(iior)+bagry(iior)+bably(iior))                            
      if(baS.gt.Ymax(mstr,138))Ymax(mstr,138) = baS 
      if(baS.lt.Ymin(mstr,138))Ymin(mstr,138) = baS 
      baZ = (bazoky(iior)+bazogy(iior)+bazoby(iior))*24.                &
     &/(bakiy(iior)+bagry(iior)+bably(iior))                            
      if(baZ.gt.Ymax(mstr,139))Ymax(mstr,139) = baZ 
      if(baZ.lt.Ymin(mstr,139))Ymin(mstr,139) = baZ 
      Ymax(mstr,140) = -1. 
      Ymin(mstr,140) = -1. 
      Ymax(mstr,141) = -1. 
      Ymin(mstr,141) = -1. 
      ztp = btpkiy(iior)*bkigry(iior)+btpgry(iior)                      &
     &*(1.-bkigry(iior)-bantby(iior))+btpbly(iior)*bantby(iior)         
      if(ztp.gt.Ymax(mstr,142))Ymax(mstr,142) = ztp 
      if(ztp.lt.Ymin(mstr,142))Ymin(mstr,142) = ztp 
      if(bkigry(iior).gt.Ymax(mstr,143))Ymax(mstr,143) = bkigry(iior) 
      if(bkigry(iior).lt.Ymin(mstr,143))Ymin(mstr,143) = bkigry(iior) 
      if(bantby(iior).gt.Ymax(mstr,144))Ymax(mstr,144) = bantby(iior) 
      if(bantby(iior).lt.Ymin(mstr,144))Ymin(mstr,144) = bantby(iior) 
      sBco = (bacoky(iior)+bacogy(iior)+bacoby(iior))*24. 
      if(sBco.gt.Ymax(mstr,145))Ymax(mstr,145) = sBco 
      if(sBco.lt.Ymin(mstr,145))Ymin(mstr,145) = sBco 
      if(bfikay(iior).gt.Ymax(mstr,146))Ymax(mstr,146) = bfikay(iior) 
      if(bfikay(iior).lt.Ymin(mstr,146))Ymin(mstr,146) = bfikay(iior) 
      if(bextky(iior).gt.Ymax(mstr,147))Ymax(mstr,147) = bextky(iior) 
      if(bextky(iior).lt.Ymin(mstr,147))Ymin(mstr,147) = bextky(iior) 
      if(bfibay(iior).gt.Ymax(mstr,148))Ymax(mstr,148) = bfibay(iior) 
      if(bfibay(iior).lt.Ymin(mstr,148))Ymin(mstr,148) = bfibay(iior) 
      if(bkmuay(iior).gt.Ymax(mstr,149))Ymax(mstr,149) = bkmuay(iior) 
      if(bkmuay(iior).lt.Ymin(mstr,149))Ymin(mstr,149) = bkmuay(iior) 
      if(bgmuay(iior).gt.Ymax(mstr,150))Ymax(mstr,150) = bgmuay(iior) 
      if(bgmuay(iior).lt.Ymin(mstr,150))Ymin(mstr,150) = bgmuay(iior) 
      if(bbmuay(iior).gt.Ymax(mstr,151))Ymax(mstr,151) = bbmuay(iior) 
      if(bbmuay(iior).lt.Ymin(mstr,151))Ymin(mstr,151) = bbmuay(iior) 
      if(bfhkay(iior).gt.Ymax(mstr,152))Ymax(mstr,152) = bfhkay(iior) 
      if(bfhkay(iior).lt.Ymin(mstr,152))Ymin(mstr,152) = bfhkay(iior) 
      if(bfhgay(iior).gt.Ymax(mstr,153))Ymax(mstr,153) = bfhgay(iior) 
      if(bfhgay(iior).lt.Ymin(mstr,153))Ymin(mstr,153) = bfhgay(iior) 
      if(bfhbay(iior).gt.Ymax(mstr,154))Ymax(mstr,154) = bfhbay(iior) 
      if(bfhbay(iior).lt.Ymin(mstr,154))Ymin(mstr,154) = bfhbay(iior) 
      if(bkray(iior).gt.Ymax(mstr,155))Ymax(mstr,155) = bkray(iior) 
      if(bkray(iior).lt.Ymin(mstr,155))Ymin(mstr,155) = bkray(iior) 
      if(bgray(iior).gt.Ymax(mstr,156))Ymax(mstr,156) = bgray(iior) 
      if(bgray(iior).lt.Ymin(mstr,156))Ymin(mstr,156) = bgray(iior) 
      if(bbray(iior).gt.Ymax(mstr,157))Ymax(mstr,157) = bbray(iior) 
      if(bbray(iior).lt.Ymin(mstr,157))Ymin(mstr,157) = bbray(iior) 
      if(bchlky(iior).gt.Ymax(mstr,158))Ymax(mstr,158) = bchlky(iior) 
      if(bchlky(iior).lt.Ymin(mstr,158))Ymin(mstr,158) = bchlky(iior) 
      if(bchlgy(iior).gt.Ymax(mstr,159))Ymax(mstr,159) = bchlgy(iior) 
      if(bchlgy(iior).lt.Ymin(mstr,159))Ymin(mstr,159) = bchlgy(iior) 
      if(bchlby(iior).gt.Ymax(mstr,160))Ymax(mstr,160) = bchlby(iior) 
      if(bchlby(iior).lt.Ymin(mstr,160))Ymin(mstr,160) = bchlby(iior) 
      if(tau2y(iior).gt.Ymax(mstr,168))Ymax(mstr,168) = tau2y(iior) 
      if(tau2y(iior).lt.Ymin(mstr,168))Ymin(mstr,168) = tau2y(iior) 
      if(btracer(iior).gt.Ymax(mstr,170))Ymax(mstr,170) = btracer(iior) 
      if(btracer(iior).lt.Ymin(mstr,170))Ymin(mstr,170) = btracer(iior) 
      if(bbetNy(iior).gt.Ymax(mstr,173))Ymax(mstr,173) = bbetNy(iior) 
      if(bbetNy(iior).lt.Ymin(mstr,173))Ymin(mstr,173) = bbetNy(iior) 
      if(bJNO3y(iior).gt.Ymax(mstr,176))Ymax(mstr,176) = bJNO3y(iior) 
      if(bJNO3y(iior).lt.Ymin(mstr,176))Ymin(mstr,176) = bJNO3y(iior) 
      if(bJNH4y(iior).gt.Ymax(mstr,178))Ymax(mstr,178) = bJNH4y(iior) 
      if(bJNH4y(iior).lt.Ymin(mstr,178))Ymin(mstr,178) = bJNH4y(iior) 
      if(bJPO4y(iior).gt.Ymax(mstr,180))Ymax(mstr,180) = bJPO4y(iior) 
      if(bJPO4y(iior).lt.Ymin(mstr,180))Ymin(mstr,180) = bJPO4y(iior) 
      if(bJO2y(iior).gt.Ymax(mstr,182))Ymax(mstr,182) = bJO2y(iior) 
      if(bJO2y(iior).lt.Ymin(mstr,182))Ymin(mstr,182) = bJO2y(iior) 
      if(bJSiy(iior).gt.Ymax(mstr,186))Ymax(mstr,186) = bJSiy(iior) 
      if(bJSiy(iior).lt.Ymin(mstr,186))Ymin(mstr,186) = bJSiy(iior) 
                                                                        
      Ymax(mstr,171) = -1. 
      Ymin(mstr,171) = -1. 
      if(alNO3y(iior).gt.Ymax(mstr,174))Ymax(mstr,174) = alNO3y(iior) 
      if(alNO3y(iior).lt.Ymin(mstr,174))Ymin(mstr,174) = alNO3y(iior) 
      if(JNO3y(iior).gt.Ymax(mstr,175))Ymax(mstr,175) = JNO3y(iior) 
      if(JNO3y(iior).lt.Ymin(mstr,175))Ymin(mstr,175) = JNO3y(iior) 
      if(JNH4y(iior).gt.Ymax(mstr,177))Ymax(mstr,177) = JNH4y(iior) 
      if(JNH4y(iior).lt.Ymin(mstr,177))Ymin(mstr,177) = JNH4y(iior) 
      if(JPO4y(iior).gt.Ymax(mstr,179))Ymax(mstr,179) = JPO4y(iior) 
      if(JPO4y(iior).lt.Ymin(mstr,179))Ymin(mstr,179) = JPO4y(iior) 
      if(JO2y(iior).gt.Ymax(mstr,181))Ymax(mstr,181) = JO2y(iior) 
      if(JO2y(iior).lt.Ymin(mstr,181))Ymin(mstr,181) = JO2y(iior) 
      if(JSiy(iior).gt.Ymax(mstr,185))Ymax(mstr,185) = JSiy(iior) 
      if(JSiy(iior).lt.Ymin(mstr,185))Ymin(mstr,185) = JSiy(iior) 




                                                                        
    enddo               ! Ende Stationenschleife 
  enddo                 ! Ende Strangschleife 
                                                                        
      if(maus.eq.1)goto 105 
      goto 9998 
!                                                                       
!                                                                       
! ++++++++Ausgabe der Mittelwerte++++++++++++                           
!                                                                       
  105 continue 
!      itime = itimeh                                                   
      maus = 0 
      ij = 1 
!                                                                       
!....it_hy - Anzahl der Zeitschritt in der Hellphase                     
                                                                       
 5454 do 578 azStr = 1,azStrs 
      mstr = mstra(azStr) 
      do 110 iior=1,mStas(mstr) 
!                                                                       
      xtempw = sumte(mstr,iior)/itime 
      xCHNF = sCHNF(mstr,iior)/itime 
      xBVHNF = sBVHNF(mstr,iior)/itime 
      xCD1 = sCD(mstr,1,iior)/itime 
      xCD2 = sCD(mstr,2,iior)/itime 
      xCP1 = sCP(mstr,1,iior)/itime 
      xCP2 = sCP(mstr,2,iior)/itime 
      xCM = sCM(mstr,iior)/itime 
      xBAC = sBAC(mstr,iior)/itime 
!                                                                       
      xbsb5 = sumb5(mstr,iior)/itime 
      xcsb = sumcs(mstr,iior)/itime 
      xnh4 = sumn4(mstr,iior)/itime 
      xchla = sumca(mstr,iior)/itime 
      xvkigr = svkigr(mstr,iior)/itime 
      xantbl = santbl(mstr,iior)/itime 
      xaki = sumaki(mstr,iior)/itime 
      xagr = sumagr(mstr,iior)/itime 
      xabl = sumabl(mstr,iior)/itime 
      xchlak = sumcak(mstr,iior)/itime 
      xchlag = sumcag(mstr,iior)/itime 
      xchlab = sumcab(mstr,iior)/itime 
      xCChlk = sumCChlk(mstr,iior)/itime
      xCChlg = sumCChlg(mstr,iior)/itime
      xCChlb = sumCChlb(mstr,iior)/itime
      xo2 = sumo2(mstr,iior)/itime
      xzooind = sumzo(mstr,iior)/itime 
      xvph = sumvph(mstr,iior)/itime 
      xvno3 = sumno3(mstr,iior)/itime 
      xvno2 = sumno2(mstr,iior)/itime 
      xgelp = sumgp(mstr,iior)/itime 
      xsi = sumsi(mstr,iior)/itime 
      xca = sumcal(mstr,iior)/itime 
      xmw = summw(mstr,iior)/itime 
      xlf = sumlf(mstr,iior)/itime 
      xcoli = scoli(mstr,iior)/itime 
!....Der Grenzwert fr Badegewsser (EG-Richtlinie 76/160)              
      BGGRW = 2000. 

      xgsZn = sumgsZn(mstr,iior)/itime 
      xglZn = sumglZn(mstr,iior)/itime 
      xgsCad = sumgsCad(mstr,iior)/itime 
      xglCad = sumglCad(mstr,iior)/itime 
      xgsCu = sumgsCu(mstr,iior)/itime 
      xglCu = sumglCu(mstr,iior)/itime 
      xgsNi = sumgsNi(mstr,iior)/itime 
      xglNi = sumglNi(mstr,iior)/itime 
                                                                      
      xdlarn = sumdln(mstr,iior)/itime 
      xss = sumss(mstr,iior)/itime 
      xpfl = sumpfl(mstr,iior)/itime 
      xgsP = sgsP(mstr,iior)/itime 
      xgsN = sgsN(mstr,iior)/itime 
      do 837 ndr=1,nndr 
      xidras(ndr) = sidras(mstr,iior,ndr)/itime 
      xdrmas(ndr) = sdrmas(mstr,iior,ndr)/itime 
      xdrakr(ndr) = sdrakr(mstr,iior,ndr)/itime 
      xdrbar(ndr) = sdrbar(mstr,iior,ndr)/itime 
      xdrmor(ndr) = sdrmor(mstr,iior,ndr)/itime 
      xdrbio(ndr) = szdrg(mstr,iior,ndr)/itime 
      xdbios(ndr) = szdrsg(mstr,iior,ndr)/itime 
  837 xgewdr(ndr) = sgwdrg(mstr,iior,ndr)/itime 
!                                                                       
      xcorI = scorIg(mstr,iior)/itime 
      xcorIs = scoIsg(mstr,iior)/itime 
!                                                                       
      xbal = sumbal(mstr,iior)/itime 
      xsusn = ssusn(mstr,iior)/itime 
      xbettn = sbettn(mstr,iior)/itime 
      xdon = sdon(mstr,iior)/itime 
      xalgn = salgn(mstr,iior)/itime 
      xalNO3 = salNO3(mstr,iior)/itime 
      xFluN3 = sFluN3(mstr,iior)/itime 
      xvx0 = (svx0(mstr,iior)/itime)*1000. 
      if(xvx0.lt.0.0)xvx0 = -1. 
      xvx02 = (svx02(mstr,iior)/itime)*1000. 
      if(xvx02.lt.0.0)xvx02 = -1. 
      xsedx = ssedx0(mstr,iior)/itime 
      xsedal = ssedal(mstr,iior)/itime 
      xalgzo = salgzo(mstr,iior)/itime 
      xalgdr = salgdr(mstr,iior)/itime 
      xalgco = salgco(mstr,iior)/itime 
      xvoldr = svoldr(mstr,iior)/itime 
      xdrpfe = sdrpfe(mstr,iior)/itime 
      xabeow = sabeow(mstr,iior)/itime 
      xabeor = sabeor(mstr,iior)/itime 
      xdalg = sdalg(mstr,iior)/itime
      xdalga = sdalga(mstr,iior)/itime 
      xalmor = salmor(mstr,iior)/itime 
      xblmor = sblmor(mstr,iior)/itime 
      xsgo2n = ssgo2n(mstr,iior)/itime 
      xsdbsb = ssdbsb(mstr,iior)/itime 
      xsoein = ssoein(mstr,iior)/itime 
      xsalgo = ssalgo(mstr,iior)/itime 
                                                                       
                                                                       
      xo2nit = xsusn*4.33 
      xalgo = s2algo(mstr,iior)/itime
      xalgao = s2algao(mstr,iior)/itime 
      xbsbt = sbsbt(mstr,iior)/itime 
      xschlr = sschlr(mstr,iior)/itime 
      xbsbbe = sbsbbe(mstr,iior)/itime 
      xo2phy = so2phy(mstr,iior)/itime 
      xro2dr = sro2dr(mstr,iior)/itime 
      xzooro = szooro(mstr,iior)/itime 
      xpo2p = spo2p(mstr,iior)/itime 
      xpo2r = spo2r(mstr,iior)/itime 
      xir = sir(mstr,iior)/itime 
      xrmue = srmue(mstr,iior)/itime 
      xrakr = srakr(mstr,iior)/itime 
      xrbar = srbar(mstr,iior)/itime 
      xffood = sffood(mstr,iior)/itime 
      xfik = sfik(mstr,iior)/it_hy(mstr,iior) 
      xfig = sfig(mstr,iior)/itime 
      xfib = -1. 
      xnaehr = snaehr(mstr,iior)/it_hy(mstr,iior)

      xakmua = sakmua(mstr,iior)/itime
      xagmua = sagmua(mstr,iior)/itime 
      xabmua = sabmua(mstr,iior)/itime 
      xfhek = sfheka(mstr,iior)/itime 
      xfheg = sfhega(mstr,iior)/itime 
      xfheb = sfheba(mstr,iior)/itime 
      xakrau = sakrau(mstr,iior)/itime 
      xagrea = sagrea(mstr,iior)/itime 
      xabrea = sabrea(mstr,iior)/itime 
      xHNFmu = sHNFmu(mstr,iior)/itime 
      xHNFre = sHNFre(mstr,iior)/itime 
      xHNFup = sHNFup(mstr,iior)/itime 
      xHNFmo = sHNFmo(mstr,iior)/itime 
      xHNFex = sHNFex(mstr,iior)/itime 
      xHNFdr = sHNFdr(mstr,iior)/itime 
      xHNFz = sHNFz(mstr,iior)/itime 
      xBACmu = sBACmu(mstr,iior)/itime 
      xHNFBA = sHNFBA(mstr,iior)/itime 
      xnl0 = snl0(mstr,iior)/itime 
      xpl0 = spl0(mstr,iior)/itime 
      xJNO3 = sJNO3(mstr,iior)/itime 
      xJNH4 = sJNH4(mstr,iior)/itime 
      xJPO4 = sJPO4(mstr,iior)/itime 
      xJO2 = sJO2(mstr,iior)/itime 
      xJSi = sJSi(mstr,iior)/itime 
!                                                                       
      if(iwsim.eq.4.or.iwsim.eq.2)goto 891 
      if((xaki+xagr+xabl)<=0.0)then 
        xalgdr = 0.0 
        xalgzo = 0.0 
        xdalg = 0.0 
        xdalga = 0.0 
        xalmor = 0.0 
        xsedal = 0.0 
        xalgco = 0.0 
          else 
           xalgdr = xalgdr*(1./tflie)/(xaki+xagr+xabl)
           xalgzo = xalgzo*(1./tflie)/(xaki+xagr+xabl)
           xdalg = xdalg*(1./tflie)/(xaki+xagr+xabl) 
           if(xdalg<0.00001)xdalg = 0.0
           xdalga = xdalga*(1./tflie)/(xaki+xagr+xabl)
           if(xdalga<0.00001)xdalga = 0.0
           xalmor = xalmor*(1./tflie)/(xaki+xagr+xabl) 
           if(xalmor<0.00001)xalmor = 0.0
           xsedal = xsedal*(1./tflie)/(xaki+xagr+xabl) 
           xalgco = xalgco*(1./tflie)/(xaki+xagr+xabl) 
           xakigr = xagr*Cagr + xaki*Caki+xabl*Cabl 
      endif                                                                        

      cbsbab = xbsb5-xaki*Caki*bsbki+xabl*Cabl*bsbbl+xagr*Cagr*bsbgr+(xzooind*GRote/1000.)*bsbzoo
                                                                        
!     Berechnung der Abbaubarkeit                                       
                                                                       
      abbau = xbsb5/xcsb 
                                                                       
      if(xBVHNF.le.0.0)then 
      xCHNFi = -1. 
      xCHNF = -1. 
      goto 891 
      endif 
!                                                                       
!.....Umrechnung in Zellzahlen                                          
!                                                                       
      xCHNFi = xCHNF*1.e6/(xBVHNF*0.22) 
!.....xCHNF in æg/l                                                     
      xCHNF = xCHNF*1000. 
!                                                                       
!                                                                       
!     Buhnenfelder                                                      
!                                                                       
  891 if(nbuhn(mstr).eq.1)then 
      bxtemp = bste(mstr,iior)/itime 
      bxno3 = bsno3(mstr,iior)/itime 
      bxno2 = bsno2(mstr,iior)/itime 
      bxnh4 = bsn4(mstr,iior)/itime 
      bxgelp = bsgelp(mstr,iior)/itime 
      bxchla = bschla(mstr,iior)/itime 
      bxssal = bsssal(mstr,iior)/itime 
      bxsi = bssi(mstr,iior)/itime 
      bxzooi = bszooi(mstr,iior)/itime 
      bxbsb5 = bsvbsb(mstr,iior)/itime 
      bxcsb = bsvcsb(mstr,iior)/itime 
      bxaki = bsaki(mstr,iior)/itime 
      bxagr = bsagr(mstr,iior)/itime 
      bxabl = bsabl(mstr,iior)/itime 
      bxo2 = bso2(mstr,iior)/itime 
      bxph = bsph(mstr,iior)/itime 
      bxca = bsca(mstr,iior)/itime 
      bxmw = bsmw(mstr,iior)/itime 
      bxlf = bslf(mstr,iior)/itime 
      bxlarn = 0.0 
      bxssal = bsssal(mstr,iior)/itime 
      bxnl0 = bsnl0(mstr,iior)/itime 
      bxpl0 = bspl0(mstr,iior)/itime 
      bxgsP = bsgsP(mstr,iior)/itime 
      bxgsN = bsgsN(mstr,iior)/itime 
!......Raten                                                            
      bxdalg = bsdalg(mstr,iior)/itime 
      bxvkg = bsvkg(mstr,iior)/itime 
      bxantb = bsantb(mstr,iior)/itime 
      bxdaa = bsdaa(mstr,iior)/itime 
      bxamor = bsamor(mstr,iior)/itime 
      bxseda = bsseda(mstr,iior)/itime 
      bxalgz = bsalgz(mstr,iior)/itime 
      bxaldr = bsadr(mstr,iior)/itime 
      bxalco = bsalco(mstr,iior)/itime 
      bxfik = bsfik(mstr,iior)/it_hy(mstr,iior) 
      bxfig = bsfig(mstr,iior)/itime 
      xbnaeh = bnaehr(mstr,iior)/it_hy(mstr,iior) 
      bxkmue = bskmue(mstr,iior)/itime 
      bxgmue = bsgmue(mstr,iior)/itime 
      bxbmue = bsbmue(mstr,iior)/itime 
      bxhek = bshek(mstr,iior)/itime 
      bxheg = bsheg(mstr,iior)/itime 
      bxheb = bsheb(mstr,iior)/itime 
      bxkre = bskre(mstr,iior)/itime 
      bxgre = bsgre(mstr,iior)/itime 
      bxbre = bsbre(mstr,iior)/itime 
      bxchlk = bschlk(mstr,iior)/itime 
      bxchlg = bschlg(mstr,iior)/itime 
      bxchlb = bschlb(mstr,iior)/itime 
      bxFLN3 = bsFlN3(mstr,iior)/itime 
      bxBetN = bsbetN(mstr,iior)/itime 
      bxJNO3 = bsJNO3(mstr,iior)/itime 
      bxJNH4 = bsJNH4(mstr,iior)/itime 
      bxJPO4 = bsJPO4(mstr,iior)/itime 
      bxJO2 = bsJO2(mstr,iior)/itime 
      bxJSi = bsJSi(mstr,iior)/itime 
      bxcoli = bscoli(mstr,iior)/itime

      bxgsZn = bsgsZn(mstr,iior)/itime
      bxglZn = bsglZn(mstr,iior)/itime
      bxgsCad = bsgsCad(mstr,iior)/itime
      bxglCad = bsglCad(mstr,iior)/itime
      bxgsCu = bsgsCu(mstr,iior)/itime
      bxglCu = bsglCu(mstr,iior)/itime
      bxgsNi = bsgsZn(mstr,iior)/itime
      bxglNi = bsglZn(mstr,iior)/itime
                                                                       
      bxaldr = bxaldr*24./(bxaki+bxagr+bxabl) 
      bxalco = bxalco*24./(bxaki+bxagr+bxabl) 
      bxalgz = bxalgz*24./(bxaki+bxagr+bxabl) 
      bxdalg = bxdalg*24./(bxaki+bxagr+bxabl) 
      bxdaa = bxdaa*24./(bxaki+bxagr+bxabl) 
      bxamor = bxamor*24./(bxaki+bxagr+bxabl) 
      bxseda = bxseda*24./(bxaki+bxagr+bxabl) 
      bxakg = bxaki*Caki+bxabl*Cabl+bxagr*Cagr 
!                                                                       
      endif 
!                                                                       
      if(nbuhn(mstr).eq.0)then 
      bmibsb(mstr,iior) = -1. 
      bxbsb5 = -1. 
      bmxbsb(mstr,iior) = -1. 
      bmicsb(mstr,iior) = -1. 
      bxcsb = -1. 
      bmxcsb(mstr,iior) = -1. 
      bminh4(mstr,iior) = -1 
      bxnh4 = -1. 
      bmxnh4(mstr,iior) = -1. 
      bmino2(mstr,iior) = -1. 
      bxno2 = -1. 
      bmxno2(mstr,iior) = -1. 
      bmino3(mstr,iior) = -1. 
      bxno3 = -1. 
      bmxno3(mstr,iior) = -1. 
      bmigsN(mstr,iior) = -1. 
      bxgsN = -1. 
      bmxgsN(mstr,iior) = -1. 
      bmiglp(mstr,iior) = -1. 
      bxgelp = -1. 
      bmxglp(mstr,iior) = -1. 
      bmigsP(mstr,iior) = -1. 
      bxgsP = -1. 
      bmxgsP(mstr,iior) = -1. 
      bmisi(mstr,iior) = -1. 
      bxsi = -1. 
      bmxsi(mstr,iior) = -1. 
      bmichl(mstr,iior) = -1. 
      bxchla = -1. 
      bmxchl(mstr,iior) = -1. 
      bmizoo(mstr,iior) = -1. 
      bxzooi = -1. 
      bmxzoo(mstr,iior) = -1. 
      bmiph(mstr,iior) = -1. 
      bxph = -1. 
      bmxph(mstr,iior) = -1. 
      bmimw(mstr,iior) = -1. 
      bxmw = -1. 
      bmxmw(mstr,iior) = -1. 
      bmica(mstr,iior) = -1. 
      bxca = -1. 
      bmxca(mstr,iior) = -1. 
      bmilf(mstr,iior) = -1. 
      bxlf = -1. 
      bmxlf(mstr,iior) = -1. 
      bmissa(mstr,iior) = -1. 
      bxssal = -1. 
      bmxssa(mstr,iior) = -1. 
      bmitem(mstr,iior) = -1. 
      bxtemp = -1. 
      bmxtem(mstr,iior) = -1. 
      bmio2(mstr,iior) = -1. 
      bxo2 = -1. 
      bmxo2(mstr,iior) = -1. 

      bmigsZn(mstr,iior) = -1. 
      bxgsZn = -1. 
      bmxgsZn(mstr,iior) = -1. 
      bmiglZn(mstr,iior) = -1. 
      bxglZn = -1. 
      bmxglZn(mstr,iior) = -1. 
      bmigsCad(mstr,iior) = -1. 
      bxgsCad = -1. 
      bmxgsCad(mstr,iior) = -1. 
      bmiglCad(mstr,iior) = -1. 
      bxglCad = -1. 
      bmxglCad(mstr,iior) = -1. 
      bmigsCu(mstr,iior) = -1. 
      bxgsCu = -1. 
      bmxgsCu(mstr,iior) = -1. 
      bmiglCu(mstr,iior) = -1. 
      bxglCu = -1. 
      bmxglCu(mstr,iior) = -1. 
      bmigsNi(mstr,iior) = -1. 
      bxgsNi = -1. 
      bmxgsNi(mstr,iior) = -1. 
      bmiglNi(mstr,iior) = -1. 
      bxglNi = -1. 
      bmxglNi(mstr,iior) = -1. 
                                                                       
!...Raten                                                               
                                                                       
      bxakg = -1. 
      bxdalg = -1. 
      bxdaa = -1. 
      bxamor = -1. 
      bxseda = -1. 
      bxalgz = -1. 
      bxaldr = -1. 
      bxdrpf = -1. 
      bxvkg = -1. 
      bxantb = -1. 
      bxalco = 0.0 
      bxfik = -1. 
      bxfig = -1. 
      bxfib = -1. 
      bxkmue = -1. 
      bxgmue = -1. 
      bxbmue = -1. 
      bxhek = -1. 
      bxheg = -1. 
      bxheb = -1. 
      bxkre = -1. 
      bxgre = -1. 
      bxbre = -1. 
      bxchlk = -1. 
      bxchlg = -1. 
      bxchlb = -1. 
      bxantb = -1. 
      bxFlN3 = -.1 
      bxBetN = -.1 
      bxJNO3 = -.1 
      bxJNH4 = -.1 
      bxJO2 = -1.
      bxJPO4 = -.1
      bxJSi = -.1
      xbnaeh = -1.
                                                                       
    endif 
                                                                       
!....Ausgabe Dreissena Bschung                                         
      if(xdrbio(2).eq.0.0.and.xdbios(2).gt.0.0)then 
      xdrbio(1) = xdbios(1) 
      xdrbio(2) = xdbios(2) 
      endif 
      if(iwsim==5)then
        miKonsS = mitemp(mstr,iior)
        xKonsS = xtempw
        mxKonsS = mxtemp(mstr,iior) 
        mitemp(mstr,iior) = -9.99
        xtempw = -9.99
        mxtemp(mstr,iior) = -9.99
      endif

      write(45,4100)itags,monats,Jahrs,mstr,Stakm(mstr,iior),STRID(mstr)                                                      
      write(45,4103)mib5(mstr,iior),xbsb5,mxb5(mstr,iior)                               &
                   ,mics(mstr,iior),xcsb,mxcs(mstr,iior),minh4(mstr,iior)               &
                   ,xnh4,mxnh4(mstr,iior),mivno2(mstr,iior),xvno2                       &
                   ,mxvno2(mstr,iior),mivno3(mstr,iior),xvno3,mxvno3(mstr,iior)         &
                   ,migsN(mstr,iior),xgsN,mxgsN(mstr,iior),migP(mstr,iior)              &
                   ,xgelp,mxgp(mstr,iior),migsP(mstr,iior),xgsP,mxgsP(mstr,iior)        &
                   ,miSi(mstr,iior),xSi,mxSi(mstr,iior),miChla(mstr,iior),xchla         &
                   ,mxchla(mstr,iior),mizo(mstr,iior),xzooind,mxzo(mstr,iior)           &
                   ,mivph(mstr,iior),xvph,mxvph(mstr,iior),mimw(mstr,iior)              &
                   ,xmw,mxmw(mstr,iior),mica(mstr,iior),xca,mxca(mstr,iior)             &
                   ,miLf(mstr,iior),xLf,mxLf(mstr,iior),miSS(mstr,iior),xSS             &
                   ,mxSS(mstr,iior),mitemp(mstr,iior),xtempw,mxtemp(mstr,iior)          &
                   ,miO2(mstr,iior),xO2,mxO2(mstr,iior),miColi(mstr,iior)               &
                   ,xColi,mxColi(mstr,iior),miKonsS,xKonsS,mxKonsS,migsZn(mstr,iior)    &                                           
                   ,xgsZn,mxgsZn(mstr,iior),miglZn(mstr,iior),xglZn,mxglZn(mstr,iior)   &
                   ,migsCad(mstr,iior),xgsCad,mxgsCad(mstr,iior),miglCad(mstr,iior)     &
                   ,xglCad,mxglCad(mstr,iior),migsCu(mstr,iior),xgsCu,mxgsCu(mstr,iior) &
                   ,miglCu(mstr,iior),xglCu,mxglCu(mstr,iior),migsNi(mstr,iior)         & 
                   ,xgsNi,mxgsNi(mstr,iior),miglNi(mstr,iior),xglNi,mxglNi(mstr,iior)   
                                                                   
      write(45,4001)xPfl 
                                                                       
      write(45,4002)xbal 
                                                                       
      write(45,4365)xDrBio(1),xgewdr(1),XDrBio(2),xgewdr(2),xdlarn                   &
                   ,xidras(1),xidras(2),xdrmas(1),xdrmas(2)                          &
                   ,xvoldr,xdrakr(1),xdrakr(2),xdrbar(1),xdrbar(2),xdrmor(1)         &
                   ,xdrmor(2),xffood                                                 
                                                                       
      write(45,4004)xCorI,xCorIs 
                                                                       
      write(45,4010)xakigr,xdalg,xdalga,xalmor,xsedal,xalgzo,xalgdr                   &
                   ,xdrpfe,xnaehr,xvkigr,xantbl,xalgco,xfik,xfig,xfib                 &
                   ,xakmua,xagmua,xabmua,xfhek,xfheg,xfheb,xakrau,xagrea              &
                   ,xabrea,xchlak,xchlag,xchlab,xCChlk,xCChlg,xCChlb                                      
                                                                       
      write(45,4265)xir,xRmue,xrakr,xrbar 
                                                                       
      write(45,4111)cbsbab,abbau,xCM,xBAC,xCD1,xCD2,xCP1,xCP2,xBACmu,(xHNFBA*24.)                                              
                                                                       
      write(45,4215)xCHNFi,xCHNF,xHNFup,xHNFre,xHNFex,xHNFmo,xHNFmu,xHNFz,xHNFdr                                       
                                                                       
      write(45,4003)xsusn,xbettn,xdon,xalgn,xalNO3,xFluN3,xJNO3,xJNH4,xvx0,xsedx,xvx02                              
                                                                       
      write(45,4020)xJPO4,xJSi 
                                                                       
      write(45,4017)xo2nit,xalgo,xalgao,xbsbt,xschlr,xJO2,xbsbbe,xo2phy,xabeow,xabeor    &
                   ,xro2dr,xzooro,xpo2p,xpo2r            
                                                                       
      write(45,4018)sedhy(mstr,iior) 
                                                                       
      bxmicl = bxcoli 
      bxmxcl = bxcoli 

      if(iwsim==5)then
        bmiKonsS = bmitem(mstr,iior) 
        bxKonsS = bxtemp
        bmxKonsS = bmxtem(mstr,iior)
        bmitem(mstr,iior) = -9.99
        bxtemp = -9.99
        bmxtem(mstr,iior) = -9.99
      endif

      write(45,4103)bmibsb(mstr,iior),bxbsb5,bmxbsb(mstr,iior),bmicsb(mstr,iior),bxcsb,bmxcsb(mstr,iior),bminh4(mstr,iior)                           &
                   ,bxnh4,bmxnh4(mstr,iior),bmino2(mstr,iior),bxno2,bmxno2(mstr,iior),bmino3(mstr,iior),bxno3,bmxno3(mstr,iior)                      &
                   ,bmigsN(mstr,iior),bxgsN,bmxgsN(mstr,iior),bmiglp(mstr,iior),bxgelp,bmxglp(mstr,iior),bmigsP(mstr,iior),bxgsP                     &
                   ,bmxgsP(mstr,iior),bmisi(mstr,iior),bxsi,bmxsi(mstr,iior),bmichl(mstr,iior),bxchla,bmxchl(mstr,iior),bmizoo(mstr,iior)            &
                   ,bxzooi,bmxzoo(mstr,iior),bmiph(mstr,iior),bxph,bmxph(mstr,iior),bmimw(mstr,iior),bxmw,bmxmw(mstr,iior),bmica(mstr,iior),bxca     &
                   ,bmxca(mstr,iior),bmilf(mstr,iior),bxlf,bmxlf(mstr,iior),bmissa(mstr,iior),bxssal,bmxssa(mstr,iior),bmitem(mstr,iior)             &
                   ,bxtemp,bmxtem(mstr,iior),bmio2(mstr,iior),bxo2,bmxo2(mstr,iior),bxmicl,bxcoli,bxmxcl,bmiKonsS,bxKonsS,bmxKonsS                   &
                   ,bmigsZn(mstr,iior),bxgsZn,bmxgsZn(mstr,iior),bmiglZn(mstr,iior),bxglZn,bmxglZn(mstr,iior)                                        &                                             
                   ,bmigsCad(mstr,iior),bxgsCad,bmxgsCad(mstr,iior),bmiglCad(mstr,iior),bxglCad,bmxglCad(mstr,iior)                                  &                                             
                   ,bmigsCu(mstr,iior),bxgsCu,bmxgsCu(mstr,iior),bmiglCu(mstr,iior),bxglCu,bmxglCu(mstr,iior)                                        &                                             
                   ,bmigsNi(mstr,iior),bxgsNi,bmxgsNi(mstr,iior),bmiglNi(mstr,iior),bxglNi,bmxglNi(mstr,iior)                                                                                   
                                                                     
      write(45,4010)bxakg,bxdalg,bxdaa,bxamor,bxseda,bxalgz,bxaldr,bxdrpf,xbnaeh,bxvkg                               &
                   ,bxantb,bxalco,bxfik,bxfig,bxfib,bxkmue,bxgmue,bxbmue,bxhek,bxheg,bxheb,bxkre,bxgre,bxbre         &
                   ,bxchlk,bxchlg,bxchlb                                             
                                                                       
      write(45,4011)bxFlN3,bxJNO3,bxJNH4,bxBetN 
                                                                       
      write(45,4020)bxJPO4,bxJSi 
                                                                       
      write(45,4021)bxJO2 
                                                                       
                                                                       
 4100 FORMAT(I2,2x,I2,2x,I4,2x,I5,2x,f8.3,2x,I5) 
 4103 FORMAT(3(f6.2,2x),3(f6.2,2x),3(f6.2,2x),3(f6.3,2x),3(f9.6,2x)             &
            ,3(f5.2,2x),3(f6.3,2x),3(f5.2,2x),3(f5.2,2x),3(f6.2,2x)             &
            ,3(f7.1,2x),3(f5.2,2x),3(f5.2,2x),3(f5.1,2x),3(f8.1,2x)             &
            ,3(f8.2,2x),3(f5.2,2x),3(f5.2,2x),3(E9.2,2x),3(f7.1,2x)             & 
            ,6(f9.2,2x),6(f8.4,2x),6(f7.3,2x),6(f7.3,2x))
              
 4001 FORMAT(F7.2) 
 4002 FORMAT(f7.2) 
 4365 FORMAT(f7.2,2x,f7.2,2x,f7.2,2x,f7.2,2x,f7.2,2x,f6.3,2x,f6.3       &
     &,2x,f6.3,2x,f6.3,2x,f7.3,2x,f7.5,2x,f7.5,2x,f7.5,2x,f7.5          &
     &,2x,f7.5,2x,f7.5,2x,f5.3)                                         
 4004 FORMAT(f8.1,2x,f8.1)    
 4010 FORMAT(f6.2,2x,f8.5,2x,f8.5,2x,f8.5,2x,f8.5,2x,f9.5,2x,f9.5       &
     &,2x,f6.2,2x,f5.2,2x,f5.2,2x,f5.2,2x,f12.8,2x,f5.2                 &
     &,2x,f5.2,2x,f5.2,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3                  &
     &,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3                          &
     &,2x,f6.1,2x,f6.1,2x,f6.1,2x,f6.2,2x,f6.2,2x,f6.2)                                         
 4011 Format(F10.7,2x,F10.8,2x,f10.8,2x,F8.6) 
 4265 FORMAT(f6.3,2x,f6.3,2x,f6.4,2x,f6.4) 
 4111 FORMAT(f6.3,2x,f5.3,2x,f6.3,2x,f6.3,2x,f7.3,2x,f7.3,2x            &
     &,f7.3,2x,f7.3,2x,f6.3,2x,f6.3)                                    
 4215 FORMAT(f8.1,2x,f7.3,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3,2x            &
     &,f6.3,2x,f6.3,2x,f6.3)                                            
 4003 FORMAT(f7.5,2x,f10.8,2x,f7.5,2x,f7.5,2x,F7.5                      &
     &,2x,f10.7,2x,f10.8,2x,f10.8,2x,f8.5,2x,f6.3,2x,f8.5)              
 4017 FORMAT(f7.4,2x,f7.4,2x,f7.4,2x,f7.4,2x,f10.8,2x,f10.8             &
     &,2x,f7.4,2x,f7.4,2x,f7.4,2x,f7.4,2x,f7.4,2x,f7.4                  &
     &,2x,f7.4,2x,f7.4)                                                 
 4018 FORMAT(F12.6) 
 4020 FORMAT(F10.8,2x,F10.8) 
 4021 FORMAT(F11.8) 
!                                                                       
!                                                                       
  110 continue 
  578 continue 
!                                                                       
!                                                                       
      if(iend.eq.1)goto 999 

      itime = itimeh
      goto 9998  
                                                                        
!                                                                       
  999 continue 
!...Ausschreiben der Min/Max-Blöcke zur grafischen Darstellung in Gerris
!                                                                       
      do 211 azStr = 1,azStrs 
      mstr = mstra(azStr) 
!                                                                       
      if(Ymin(mstr,20).gt.0.0)Ymin(mstr,20) = Ymin(mstr,20)*1000. 
!                                                                       
      do 210 iior=1,mStas(mstr) 
!                                                                       
      write(155,'(a7,14x,I5,2x,F8.3,2x,i5)')cmin,mstr                   &
     &,Stakm(mstr,iior),STRID(mstr)                                     
      write(155,5105)Ymin(mstr,1),Ymin(mstr,2),Ymin(mstr,3)             &
     &,Ymin(mstr,4),Ymin(mstr,5),Ymin(mstr,6)                           &
     &,Ymin(mstr,7),Ymin(mstr,8),Ymin(mstr,9)                           &
     &,Ymin(mstr,10),Ymin(mstr,11),Ymin(mstr,12)                        &
     &,Ymin(mstr,13),Ymin(mstr,14),Ymin(mstr,15)                        &
     &,Ymin(mstr,16),Ymin(mstr,17),Ymin(mstr,18)                        &
     &,Ymin(mstr,20),Ymin(mstr,19),Ymin(mstr,21),Ymin(mstr,22)          &
     &,Ymin(mstr,169)                                                   
!                                                                       
      write(155,5205)(Ymin(mstr,104)*hcUmt),(Ymin(mstr,93)*4.33*hcUmt),(Ymin(mstr,108)*hcUmt)      &
                      ,(Ymin(mstr,102)*hcUmt),Ymin(mstr,187),Ymin(mstr,188)                        &
                      ,Ymin(mstr,189),(Ymin(mstr,112)*hcUmt),(Ymin(mstr,105)*hcUmt)                &
                      ,(Ymin(mstr,94)*hcUmt)                                            
!                                                                       
      write(155,5115)Ymin(mstr,115),Ymin(mstr,116),Ymin(mstr,117)       &
     &,Ymin(mstr,118),Ymin(mstr,119),Ymin(mstr,120)                     &
     &,Ymin(mstr,121),Ymin(mstr,122),Ymin(mstr,123)                     &
     &,Ymin(mstr,124),Ymin(mstr,125),Ymin(mstr,126)                     &
     &,Ymin(mstr,127),Ymin(mstr,128),Ymin(mstr,129)                     &
     &,Ymin(mstr,130),Ymin(mstr,131),Ymin(mstr,132)                     &
     &,Ymin(mstr,133),Ymin(mstr,171),Ymin(mstr,168)                     &
     &,Ymin(mstr,170)                                                   
!                                                                       
      write(45,'(a7,7x,I5,2x,F8.3,2x,i5)')cmin,mstr,Stakm(mstr,iior),STRID(mstr)                                     
      write(45,4103)Ymin(mstr,1),Ymin(mstr,1),Ymin(mstr,1)                           &
                   ,Ymin(mstr,2),Ymin(mstr,2),Ymin(mstr,2)                           &
                   ,Ymin(mstr,3),Ymin(mstr,3),Ymin(mstr,3)                           &
                   ,Ymin(mstr,4),Ymin(mstr,4),Ymin(mstr,4)                           &
                   ,Ymin(mstr,5),Ymin(mstr,5),Ymin(mstr,5)                           &
                   ,Ymin(mstr,6),Ymin(mstr,6),Ymin(mstr,6)                           &
                   ,Ymin(mstr,7),Ymin(mstr,7),Ymin(mstr,7)                           &
                   ,Ymin(mstr,8),Ymin(mstr,8),Ymin(mstr,8)                           &
                   ,Ymin(mstr,9),Ymin(mstr,9),Ymin(mstr,9)                           &
                   ,Ymin(mstr,10),Ymin(mstr,10),Ymin(mstr,10)                        &
                   ,Ymin(mstr,11),Ymin(mstr,11),Ymin(mstr,11)                        &
                   ,Ymin(mstr,12),Ymin(mstr,12),Ymin(mstr,12)                        &
                   ,Ymin(mstr,13),Ymin(mstr,13),Ymin(mstr,13)                        &
                   ,Ymin(mstr,14),Ymin(mstr,14),Ymin(mstr,14)                        &
                   ,Ymin(mstr,15),Ymin(mstr,15),Ymin(mstr,15)                        &
                   ,Ymin(mstr,16),Ymin(mstr,16),Ymin(mstr,16)                        &
                   ,Ymin(mstr,17),Ymin(mstr,17),Ymin(mstr,17)                        &
                   ,Ymin(mstr,18),Ymin(mstr,18),Ymin(mstr,18)                        &
                   ,Ymin(mstr,19),Ymin(mstr,19),Ymin(mstr,19)                        &                                               
                   ,Ymin(mstr,17),Ymin(mstr,17),Ymin(mstr,17)                        &
                   ,Ymin(mstr,193),Ymin(mstr,193),Ymin(mstr,193)                     &                                               
                   ,Ymin(mstr,194),Ymin(mstr,194),Ymin(mstr,194)                     &                                               
                   ,Ymin(mstr,195),Ymin(mstr,195),Ymin(mstr,195)                     &                                               
                   ,Ymin(mstr,196),Ymin(mstr,196),Ymin(mstr,196)                     &                                               
                   ,Ymin(mstr,197),Ymin(mstr,197),Ymin(mstr,197)                     &                         
                   ,Ymin(mstr,198),Ymin(mstr,198),Ymin(mstr,198)                     &                           
                   ,Ymin(mstr,199),Ymin(mstr,199),Ymin(mstr,199)                     &                          
                   ,Ymin(mstr,200),Ymin(mstr,200),Ymin(mstr,200)                                               

!                                                                       
      write(45,4001)Ymin(mstr,23) 
!                                                                       
      write(45,4002)Ymin(mstr,24) 
!                                                                       
      write(45,4365)Ymin(mstr,25),Ymin(mstr,26),Ymin(mstr,27)           &
     &,Ymin(mstr,28),Ymin(mstr,29),Ymin(mstr,30)                        &
     &,Ymin(mstr,31),Ymin(mstr,32),Ymin(mstr,33)                        &
     &,Ymin(mstr,34),Ymin(mstr,35),Ymin(mstr,36)                        &
     &,Ymin(mstr,37),Ymin(mstr,38),Ymin(mstr,39)                        &
     &,Ymin(mstr,40),Ymin(mstr,41)                                      
!                                                                       
      write(45,4004)Ymin(mstr,42),Ymin(mstr,43) 
!                                                                       
      write(45,4010)Ymin(mstr,44),Ymin(mstr,45),Ymin(mstr,46)           &
     &,Ymin(mstr,47),Ymin(mstr,48),Ymin(mstr,49)                        &
     &,Ymin(mstr,50),Ymin(mstr,51),Ymin(mstr,52)                        &
     &,Ymin(mstr,53),Ymin(mstr,54),Ymin(mstr,55)                        &
     &,Ymin(mstr,56),Ymin(mstr,57),Ymin(mstr,58)                        &
     &,Ymin(mstr,59),Ymin(mstr,60),Ymin(mstr,61)                        &
     &,Ymin(mstr,62),Ymin(mstr,63),Ymin(mstr,64)                        &
     &,Ymin(mstr,65),Ymin(mstr,66),Ymin(mstr,67)                        &
     &,Ymin(mstr,68),Ymin(mstr,69),Ymin(mstr,70)                        &                        
     &,Ymin(mstr,187),Ymin(mstr,188),Ymin(mstr,189)                                             
!                                                                       
      write(45,4265)Ymin(mstr,71),Ymin(mstr,72),Ymin(mstr,73)           &
     &,Ymin(mstr,74)                                                    
!                                                                       
      write(45,4111)Ymin(mstr,75),Ymin(mstr,76),Ymin(mstr,77)           &
     &,Ymin(mstr,78),Ymin(mstr,79),Ymin(mstr,80)                        &
     &,Ymin(mstr,81),Ymin(mstr,82),Ymin(mstr,83)                        &
     &,(Ymin(mstr,84)*24.)                                              
!                                                                       
      write(45,4215)Ymin(mstr,85),Ymin(mstr,20),Ymin(mstr,86)           &
     &,Ymin(mstr,87),Ymin(mstr,88),Ymin(mstr,89)                        &
     &,Ymin(mstr,90),Ymin(mstr,91),Ymin(mstr,92)                        
!                                                                       
      write(45,4003)Ymin(mstr,93),Ymin(mstr,94),Ymin(mstr,95)           &
     &,Ymin(mstr,96),Ymin(mstr,174),Ymin(mstr,97),Ymin(mstr,175)        &
     &,Ymin(mstr,177),Ymin(mstr,98),Ymin(mstr,99),Ymin(mstr,100)        
!                                                                       
      write(45,4020)Ymin(mstr,179),Ymin(mstr,185) 
!                                                                       
      write(45,4017)Ymin(mstr,101),Ymin(mstr,102),Ymin(mstr,103)        &
     &,Ymin(mstr,104),Ymin(mstr,105),Ymin(mstr,181)                     &
     &,Ymin(mstr,107),Ymin(mstr,108),Ymin(mstr,109)                     &
     &,Ymin(mstr,110),Ymin(mstr,111),Ymin(mstr,112)                     &
     &,Ymin(mstr,113),Ymin(mstr,114)                                    
!                                                                       
      write(45,4018)Ymin(mstr,22) 
!                                                                       
      write(45,4103)Ymin(mstr,115),Ymin(mstr,115),Ymin(mstr,115)                     &
                   ,Ymin(mstr,116),Ymin(mstr,116),Ymin(mstr,116)                     &
                   ,Ymin(mstr,117),Ymin(mstr,117),Ymin(mstr,117)                     &
                   ,Ymin(mstr,118),Ymin(mstr,118),Ymin(mstr,118)                     &
                   ,Ymin(mstr,119),Ymin(mstr,119),Ymin(mstr,119)                     &
                   ,Ymin(mstr,120),Ymin(mstr,120),Ymin(mstr,120)                     &
                   ,Ymin(mstr,121),Ymin(mstr,121),Ymin(mstr,121)                     &
                   ,Ymin(mstr,122),Ymin(mstr,122),Ymin(mstr,122)                     &
                   ,Ymin(mstr,123),Ymin(mstr,123),Ymin(mstr,123)                     &
                   ,Ymin(mstr,124),Ymin(mstr,124),Ymin(mstr,124)                     &
                   ,Ymin(mstr,125),Ymin(mstr,125),Ymin(mstr,125)                     &
                   ,Ymin(mstr,126),Ymin(mstr,126),Ymin(mstr,126)                     &
                   ,Ymin(mstr,127),Ymin(mstr,127),Ymin(mstr,127)                     &
                   ,Ymin(mstr,128),Ymin(mstr,128),Ymin(mstr,128)                     &
                   ,Ymin(mstr,129),Ymin(mstr,129),Ymin(mstr,129)                     &
                   ,Ymin(mstr,130),Ymin(mstr,130),Ymin(mstr,130)                     &
                   ,Ymin(mstr,131),Ymin(mstr,131),Ymin(mstr,131)                     &
                   ,Ymin(mstr,132),Ymin(mstr,132),Ymin(mstr,132)                     &
                   ,Ymin(mstr,133),Ymin(mstr,133),Ymin(mstr,133)                     &
                   ,Ymin(mstr,131),Ymin(mstr,131),Ymin(mstr,131)                     &
                   ,Ymin(mstr,193),Ymin(mstr,193),Ymin(mstr,193)                     &                                               
                   ,Ymin(mstr,194),Ymin(mstr,194),Ymin(mstr,194)                     &                                               
                   ,Ymin(mstr,195),Ymin(mstr,195),Ymin(mstr,195)                     &                                               
                   ,Ymin(mstr,196),Ymin(mstr,196),Ymin(mstr,196)                     &                                               
                   ,Ymin(mstr,197),Ymin(mstr,197),Ymin(mstr,197)                     &                         
                   ,Ymin(mstr,198),Ymin(mstr,198),Ymin(mstr,198)                     &                           
                   ,Ymin(mstr,199),Ymin(mstr,199),Ymin(mstr,199)                     &                          
                   ,Ymin(mstr,200),Ymin(mstr,200),Ymin(mstr,200)                                               
                                                                       
      write(45,4010)Ymin(mstr,134),Ymin(mstr,135),Ymin(mstr,136)        &
     &,Ymin(mstr,137),Ymin(mstr,138),Ymin(mstr,139)                     &
     &,Ymin(mstr,140),Ymin(mstr,141),Ymin(mstr,142)                     &
     &,Ymin(mstr,143),Ymin(mstr,144),Ymin(mstr,145)                     &
     &,Ymin(mstr,146),Ymin(mstr,147),Ymin(mstr,148)                     &
     &,Ymin(mstr,149),Ymin(mstr,150),Ymin(mstr,151)                     &
     &,Ymin(mstr,152),Ymin(mstr,153),Ymin(mstr,154)                     &
     &,Ymin(mstr,155),Ymin(mstr,156),Ymin(mstr,157)                     &
     &,Ymin(mstr,158),Ymin(mstr,159),Ymin(mstr,160)                     
!                                                                       
      write(45,4011)Ymin(mstr,106),Ymin(mstr,176),Ymin(mstr,178)        &
     &,Ymin(mstr,173)                                                   
!                                                                       
      write(45,4020)Ymin(mstr,180),Ymin(mstr,186) 
      write(45,4021)Ymin(mstr,182) 
!                                                                       
!                                                                       
  210 continue 
  211 continue 
      do 312 azStr = 1,azStrs 
      mstr = mstra(azStr) 
!                                                                       
      if(Ymax(mstr,20).gt.0.0)Ymax(mstr,20) = Ymax(mstr,20)*1000. 
!                                                                       
      do 313 iior=1,mStas(mstr) 
!                                                                       
      write(155,'(a7,14x,I5,2x,F8.3,2x,i5)')cmax,mstr                   &
     &,Stakm(mstr,iior),STRID(mstr)                                     
      write(155,5105)Ymax(mstr,1),Ymax(mstr,2),Ymax(mstr,3)             &
     &,Ymax(mstr,4),Ymax(mstr,5),Ymax(mstr,6)                           &
     &,Ymax(mstr,7),Ymax(mstr,8),Ymax(mstr,9)                           &
     &,Ymax(mstr,10),Ymax(mstr,11),Ymax(mstr,12)                        &
     &,Ymax(mstr,13),Ymax(mstr,14),Ymax(mstr,15)                        &
     &,Ymax(mstr,16),Ymax(mstr,17),Ymax(mstr,18)                        &
     &,Ymax(mstr,20),Ymax(mstr,19),Ymax(mstr,21),Ymax(mstr,22)          &
     &,Ymax(mstr,169)                                                   
                                                                       
      write(155,5205)(Ymax(mstr,104)*hcUmt),(Ymax(mstr,93)*4.33*hcUmt),(Ymax(mstr,108)*hcUmt)        &
                     ,(Ymax(mstr,102)*hcUmt),Ymax(mstr,187),Ymax(mstr,188)                           &
                     ,Ymax(mstr,189),(Ymax(mstr,112)*hcUmt),(Ymax(mstr,105)*hcUmt)                   &
                     ,(Ymax(mstr,94)*hcUmt)                                            
                                                                       
      write(155,5115)Ymax(mstr,115),Ymax(mstr,116),Ymax(mstr,117)       &
     &,Ymax(mstr,118),Ymax(mstr,119),Ymax(mstr,120)                     &
     &,Ymax(mstr,121),Ymax(mstr,122),Ymax(mstr,123)                     &
     &,Ymax(mstr,124),Ymax(mstr,125),Ymax(mstr,126)                     &
     &,Ymax(mstr,127),Ymax(mstr,128),Ymax(mstr,129)                     &
     &,Ymax(mstr,130),Ymax(mstr,131),Ymax(mstr,132)                     &
     &,Ymax(mstr,133),Ymax(mstr,171),Ymax(mstr,168)                     &
     &,Ymax(mstr,170)                                                   
                                                                       
      write(45,'(a7,7x,I5,2x,F8.3,2x,i5)')cmax,mstr,Stakm(mstr,iior),STRID(mstr)                                     
      write(45,4103)Ymax(mstr,1),Ymax(mstr,1),Ymax(mstr,1)                           &
                   ,Ymax(mstr,2),Ymax(mstr,2),Ymax(mstr,2)                           &
                   ,Ymax(mstr,3),Ymax(mstr,3),Ymax(mstr,3)                           &
                   ,Ymax(mstr,4),Ymax(mstr,4),Ymax(mstr,4)                           &
                   ,Ymax(mstr,5),Ymax(mstr,5),Ymax(mstr,5)                           &
                   ,Ymax(mstr,6),Ymax(mstr,6),Ymax(mstr,6)                           &
                   ,Ymax(mstr,7),Ymax(mstr,7),Ymax(mstr,7)                           &
                   ,Ymax(mstr,8),Ymax(mstr,8),Ymax(mstr,8)                           &
                   ,Ymax(mstr,9),Ymax(mstr,9),Ymax(mstr,9)                           &
                   ,Ymax(mstr,10),Ymax(mstr,10),Ymax(mstr,10)                        &
                   ,Ymax(mstr,11),Ymax(mstr,11),Ymax(mstr,11)                        &
                   ,Ymax(mstr,12),Ymax(mstr,12),Ymax(mstr,12)                        &
                   ,Ymax(mstr,13),Ymax(mstr,13),Ymax(mstr,13)                        &
                   ,Ymax(mstr,14),Ymax(mstr,14),Ymax(mstr,14)                        &
                   ,Ymax(mstr,15),Ymax(mstr,15),Ymax(mstr,15)                        &
                   ,Ymax(mstr,16),Ymax(mstr,16),Ymax(mstr,16)                        &
                   ,Ymax(mstr,17),Ymax(mstr,17),Ymax(mstr,17)                        &
                   ,Ymax(mstr,18),Ymax(mstr,18),Ymax(mstr,18)                        &
                   ,Ymax(mstr,19),Ymax(mstr,19),Ymax(mstr,19)                        &                        
                   ,Ymax(mstr,17),Ymax(mstr,17),Ymax(mstr,17)                        &                        
                   ,Ymax(mstr,193),Ymax(mstr,193),Ymax(mstr,193)                     &                                               
                   ,Ymax(mstr,194),Ymax(mstr,194),Ymax(mstr,194)                     &                                               
                   ,Ymax(mstr,195),Ymax(mstr,195),Ymax(mstr,195)                     &                                               
                   ,Ymax(mstr,196),Ymax(mstr,196),Ymax(mstr,196)                     &                                               
                   ,Ymax(mstr,197),Ymax(mstr,197),Ymax(mstr,197)                     &                         
                   ,Ymax(mstr,198),Ymax(mstr,198),Ymax(mstr,198)                     &                           
                   ,Ymax(mstr,199),Ymax(mstr,199),Ymax(mstr,199)                     &                          
                   ,Ymax(mstr,200),Ymax(mstr,200),Ymax(mstr,200)                                               
                                                                       
      write(45,4001)Ymax(mstr,23) 
                                                                       
      write(45,4002)Ymax(mstr,24) 
                                                                       
      write(45,4365)Ymax(mstr,25),Ymax(mstr,26),Ymax(mstr,27)           &
     &,Ymax(mstr,28),Ymax(mstr,29),Ymax(mstr,30)                        &
     &,Ymax(mstr,31),Ymax(mstr,32),Ymax(mstr,33)                        &
     &,Ymax(mstr,34),Ymax(mstr,35),Ymax(mstr,36)                        &
     &,Ymax(mstr,37),Ymax(mstr,38),Ymax(mstr,39)                        &
     &,Ymax(mstr,40),Ymax(mstr,41)                                      
                                                                       
      write(45,4004)Ymax(mstr,42),Ymax(mstr,43) 
                                                                       
      write(45,4010)Ymax(mstr,44),Ymax(mstr,45),Ymax(mstr,46)           &
     &,Ymax(mstr,47),Ymax(mstr,48),Ymax(mstr,49)                        &
     &,Ymax(mstr,50),Ymax(mstr,51),Ymax(mstr,52)                        &
     &,Ymax(mstr,53),Ymax(mstr,54),Ymax(mstr,55)                        &
     &,Ymax(mstr,56),Ymax(mstr,57),Ymax(mstr,58)                        &
     &,Ymax(mstr,59),Ymax(mstr,60),Ymax(mstr,61)                        &
     &,Ymax(mstr,62),Ymax(mstr,63),Ymax(mstr,64)                        &
     &,Ymax(mstr,65),Ymax(mstr,66),Ymax(mstr,67)                        &
     &,Ymax(mstr,68),Ymax(mstr,69),Ymax(mstr,70)                        &                         
     &,Ymax(mstr,187),Ymax(mstr,188),Ymax(mstr,189)                        
!                                                                       
      write(45,4265)Ymax(mstr,71),Ymax(mstr,72),Ymax(mstr,73)           &
     &,Ymax(mstr,74)                                                    
!                                                                       
      write(45,4111)Ymax(mstr,75),Ymax(mstr,76),Ymax(mstr,77)           &
     &,Ymax(mstr,78),Ymax(mstr,79),Ymax(mstr,80)                        &
     &,Ymax(mstr,81),Ymax(mstr,82),Ymax(mstr,83)                        &
     &,(Ymax(mstr,84)*24.)                                              
!                                                                       
      write(45,4215)Ymax(mstr,85),Ymax(mstr,20),Ymax(mstr,86)           &
     &,Ymax(mstr,87),Ymax(mstr,88),Ymax(mstr,89)                        &
     &,Ymax(mstr,90),Ymax(mstr,91),Ymax(mstr,92)                        
!                                                                       
      write(45,4003)Ymax(mstr,93),Ymax(mstr,94),Ymax(mstr,95)           &
     &,Ymax(mstr,96),Ymax(mstr,174),Ymax(mstr,97),Ymax(mstr,175)        &
     &,Ymax(mstr,177),Ymax(mstr,98),Ymax(mstr,99),Ymax(mstr,100)        
!                                                                       
      write(45,4020)Ymax(mstr,179),Ymax(mstr,185) 
!                                                                       
      write(45,4017)Ymax(mstr,101),Ymax(mstr,102),Ymax(mstr,103)        &
     &,Ymax(mstr,104),Ymax(mstr,105),Ymax(mstr,181)                     &
     &,Ymax(mstr,107),Ymax(mstr,108),Ymax(mstr,109)                     &
     &,Ymax(mstr,110),Ymax(mstr,111),Ymax(mstr,112)                     &
     &,Ymax(mstr,113),Ymax(mstr,114)                                    
!                                                                       
      write(45,4018)Ymax(mstr,22) 
!                                                                       
      write(45,4103)Ymax(mstr,115),Ymax(mstr,115),Ymax(mstr,115)                     &
                   ,Ymax(mstr,116),Ymax(mstr,116),Ymax(mstr,116)                     &
                   ,Ymax(mstr,117),Ymax(mstr,117),Ymax(mstr,117)                     &
                   ,Ymax(mstr,118),Ymax(mstr,118),Ymax(mstr,118)                     &
                   ,Ymax(mstr,119),Ymax(mstr,119),Ymax(mstr,119)                     &
                   ,Ymax(mstr,120),Ymax(mstr,120),Ymax(mstr,120)                     &
                   ,Ymax(mstr,121),Ymax(mstr,121),Ymax(mstr,121)                     &
                   ,Ymax(mstr,122),Ymax(mstr,122),Ymax(mstr,122)                     &
                   ,Ymax(mstr,123),Ymax(mstr,123),Ymax(mstr,123)                     &
                   ,Ymax(mstr,124),Ymax(mstr,124),Ymax(mstr,124)                     &
                   ,Ymax(mstr,125),Ymax(mstr,125),Ymax(mstr,125)                     &
                   ,Ymax(mstr,126),Ymax(mstr,126),Ymax(mstr,126)                     &
                   ,Ymax(mstr,127),Ymax(mstr,127),Ymax(mstr,127)                     &
                   ,Ymax(mstr,128),Ymax(mstr,128),Ymax(mstr,128)                     &
                   ,Ymax(mstr,129),Ymax(mstr,129),Ymax(mstr,129)                     &
                   ,Ymax(mstr,130),Ymax(mstr,130),Ymax(mstr,130)                     &
                   ,Ymax(mstr,131),Ymax(mstr,131),Ymax(mstr,131)                     &
                   ,Ymax(mstr,132),Ymax(mstr,132),Ymax(mstr,132)                     &
                   ,Ymax(mstr,133),Ymax(mstr,133),Ymax(mstr,133)                     &
                   ,Ymax(mstr,131),Ymax(mstr,131),Ymax(mstr,131)                     &                     
                   ,Ymax(mstr,193),Ymax(mstr,193),Ymax(mstr,193)                     &                                               
                   ,Ymax(mstr,194),Ymax(mstr,194),Ymax(mstr,194)                     &                                               
                   ,Ymax(mstr,195),Ymax(mstr,195),Ymax(mstr,195)                     &                                               
                   ,Ymax(mstr,196),Ymax(mstr,196),Ymax(mstr,196)                     &                                               
                   ,Ymax(mstr,197),Ymax(mstr,197),Ymax(mstr,197)                     &                         
                   ,Ymax(mstr,198),Ymax(mstr,198),Ymax(mstr,198)                     &                           
                   ,Ymax(mstr,199),Ymax(mstr,199),Ymax(mstr,199)                     &                          
                   ,Ymax(mstr,200),Ymax(mstr,200),Ymax(mstr,200)                                               




                                                                       
      write(45,4010)Ymax(mstr,134),Ymax(mstr,135),Ymax(mstr,136)        &
     &,Ymax(mstr,137),Ymax(mstr,138),Ymax(mstr,139)                     &
     &,Ymax(mstr,140),Ymax(mstr,141),Ymax(mstr,142)                     &
     &,Ymax(mstr,143),Ymax(mstr,144),Ymax(mstr,145)                     &
     &,Ymax(mstr,146),Ymax(mstr,147),Ymax(mstr,148)                     &
     &,Ymax(mstr,149),Ymax(mstr,150),Ymax(mstr,151)                     &
     &,Ymax(mstr,152),Ymax(mstr,153),Ymax(mstr,154)                     &
     &,Ymax(mstr,155),Ymax(mstr,156),Ymax(mstr,157)                     &
     &,Ymax(mstr,158),Ymax(mstr,159),Ymax(mstr,160)                     
!                                                                       
      write(45,4011)Ymax(mstr,106),Ymax(mstr,176),Ymax(mstr,178)        &
     &,Ymax(mstr,173)                                                   
!                                                                       
      write(45,4020)Ymax(mstr,180),Ymax(mstr,186) 
      write(45,4021)Ymax(mstr,182) 
!                                                                       
  313 continue 
  312 continue 
!                                                                       
      if(i2Daus.eq.0)goto 989 
!                                                                       
!   Ausgabe der min/max-Werte bei 2D-Modellierung                       
!                                                                       
      ztiefa = -1. 
      do 511 azStr = 1,azStrs 
      mstr = mstra(azStr) 
                                                                       
      do 512 iior=1,mStas(mstr) 
      write(255,'(a7,14x,I5,2x,f8.3,2x,I2,2x,I2,2x,i5)')cmin,mstr,Stakm(mstr,iior),nkzmx(mstr,iior),nkzmx(mstr,iior)   &
            ,STRID(mstr)                                                      

      do 513 nkz = 1,nkzmx(mstr,iior) 
      write(255,5107)ztiefa,Ymin(mstr,161),Ymin(mstr,162),Ymin(mstr,163),Ymin(mstr,164),Ymin(mstr,165)                 &
                    ,Ymin(mstr,166),Ymin(mstr,167),Ymin(mstr,172),Ymin(mstr,190),Ymin(mstr,191),Ymin(mstr,192)         &
                    ,Ymin(mstr,183),Ymin(mstr,184)                     
  513 continue 
!                                                                       
  512 continue 
  511 continue 
                                                                       
                                                                       
      do 514 azStr = 1,azStrs 
      mstr = mstra(azStr) 
                                                                       
      do 515 iior=1,mStas(mstr) 
      write(255,'(a7,14x,I5,2x,f8.3,2x,I2,2x,I2,2x,I5)')cmax,mstr,Stakm(mstr,iior),nkzmx(mstr,iior),nkzmx(mstr,iior)    &
               ,STRID(mstr)                                                      

      do 516 nkz = 1,nkzmx(mstr,iior) 
      write(255,5107)ztiefa,Ymax(mstr,161),Ymax(mstr,162),Ymax(mstr,163),Ymax(mstr,164),Ymax(mstr,165)                  &
                    ,Ymax(mstr,166),Ymax(mstr,167),Ymax(mstr,172),Ymax(mstr,190),Ymax(mstr,191),Ymax(mstr,192)          &
                    ,Ymax(mstr,183),Ymax(mstr,184)                     
  516 continue 
                                                                       
  515 continue 
  514 continue 
!                                                                       
!                                                                       
!*****************                                                      
!   Fehlerausgabe                                                       
!*****************                                                      

  
  989 continue 

      if(ifehl.eq.0)goto 990 
      i = 0 
      open(unit=599,file='Fehlermeldungen.txt')                                       
       rewind(599) 
  991 i =i+1 
      read(599,'(a120)')cfehlr 
      if(i.ne.ifehl)goto 991 
      if(ifhStr.gt.0.and.fhprof.gt.0.0)then 
         write(199,'(a120,2x,i3,5x,f8.2)')cfehlr,ifhStr,fhprof 
          goto 990 
      endif 
      if(ifhStr.gt.0.and.fhprof.eq.0.0)then 
         write(199,'(a120,2x,i3)')cfehlr,ifhStr 
          goto 990 
      endif 
      if(ifehl==31)then
        write(199,'(a120,5x,a50)')cfehlr,CEName(ifmstr,ifmRB)
        goto 990
      endif

      if(ifehl==32)then
        write(199,'(a120,5x,a50)')cfehlr,CEName(ifmstr,ifmRB)
        goto 990
      endif

      write(199,'(120a)')cfehlr 
                                                                       
  990 close (45) 

      write(*,*)ifehl
      close (155) 
      close (255) 
      close (199) 
      if(iRHKW.eq.1)close (177) 
      if(ifehl.ne.0)then 
      stop 1 
        else 
      write(*,*)ifehl
         stop 0 
      endif 
  endif     
                                                                       
      END  Program QSIM
                                         
!++++++++++++++++++++++++++++++++++++
   subroutine Masse_neu_Qsim(ior,nkzs,akiz,aki,ablz,abl,agrz,agr,vo2z,vo2,vnh4z,vnh4,vno2z,vno2,vno3z,vno3,gelPz,gelP,Siz,Si              &
                            ,chlaz,chla,hchlkz,chlaki,hchlbz,chlabl,hchlgz,chlagr,hgesPz,gesP,hgesNz,gesN,dH2D,hCChlkz,akbcm              &
                            ,hCChlbz,abbcm,hCChlgz,agbcm,mstr,azStrs,Caki,Cabl,Cagr)


      integer                                   :: azStrs
      integer, Dimension(1000)                  :: nkzs

      real, Dimension(1000)                     :: aki, abl, agr, vo2, vnh4, vno2, vno3, gelP, Si, chla, chlaki
      real, Dimension(1000)                     :: chlabl, chlagr, akbcm, abbcm, agbcm, gesP, gesN  
      real, Dimension(50)                       :: dzMassek,dzMasseb,dzMasseg, dzMasseO,Masse_neuk, Masse_neub, Masse_neug, Masse_neuO 
      real, Dimension(50)                       :: dzMasseN4,dzMasseN2,dzMasseN3,Masse_neuN4, Masse_neuN2, Masse_neuN3 
      real, Dimension(50)                       :: dzMassegP,dzMasseSi,Masse_neugP, Masse_neuSi, dzMasseChl 
      real, Dimension(50)                       :: dzMasseChlk, dzMasseChlb, dzMasseChlg, dzMasseCChlk 
      real, Dimension(50)                       :: dzMasseCChlb, dzMasseCChlg, dzMassegsP, dzMassegsN, Masse_neuChl 
      real, Dimension(50)                       :: Masse_neuChlk, Masse_neuChlb, Masse_neuChlg 
      real, Dimension(50)                       :: Masse_neuCChlk, Masse_neuCChlb, Masse_neuCChlg, Masse_neugsP 
      real, Dimension(50)                       :: Masse_neugsN 
      real, Dimension(50,1000)                  :: akiz, ablz, agrz, vo2z, vnh4z, vno2z, vno3z, gelPz, Siz, chlaz
      real, Dimension(azStrs,50,1000)           :: hchlkz, hchlbz, hchlgz, hCChlkz, hCChlbz, hCChlgz, hgesPz, hgesNz 


!+++++++++ Massenerhalt ++++++++++++++++ 

  sumakiz = 0.0
  sumablz = 0.0
  sumagrz = 0.0
  sumO2 = 0.0 
  sumNH4 = 0.0
  sumNO2 = 0.0
  sumNo3 = 0.0
  sumgP = 0.0
  sumSi = 0.0
  sumChl = 0.0
  sumChlk = 0.0
  sumChlb = 0.0
  sumChlg = 0.0
  sumCChlk = 0.0
  sumCChlb = 0.0
  sumCChlg = 0.0
  sumgsP = 0.0
  sumgsN = 0.0

     do nkz = 1,nkzs(ior)
        if(nkz>1)then
          sumakiz = sumakiz + ((akiz(nkz-1,ior)+akiz(nkz,ior))/2.)*dH2D
          dzMassek(nkz) = ((akiz(nkz-1,ior)+akiz(nkz,ior))/2.)*dH2D
          sumablz = sumablz + ((ablz(nkz-1,ior)+ablz(nkz,ior))/2.)*dH2D
          dzMasseb(nkz) = ((ablz(nkz-1,ior)+ablz(nkz,ior))/2.)*dH2D
          sumagrz = sumagrz + ((agrz(nkz-1,ior)+agrz(nkz,ior))/2.)*dH2D
          dzMasseg(nkz) = ((agrz(nkz-1,ior)+agrz(nkz,ior))/2.)*dH2D
          sumO2 = sumO2 + ((vo2z(nkz-1,ior)+vo2z(nkz,ior))/2.)*dH2D
          dzMasseO(nkz) = ((vo2z(nkz-1,ior)+vo2z(nkz,ior))/2.)*dH2D
          sumNH4 = sumNH4 + ((vnh4z(nkz-1,ior)+vnh4z(nkz,ior))/2.)*dH2D
          dzMasseN4(nkz) = ((vnh4z(nkz-1,ior)+vnh4z(nkz,ior))/2.)*dH2D
          sumNO2 = sumNO2 + ((vno2z(nkz-1,ior)+vno2z(nkz,ior))/2.)*dH2D
          dzMasseN2(nkz) = ((vno2z(nkz-1,ior)+vno2z(nkz,ior))/2.)*dH2D
          sumNO3 = sumNO3 + ((vno3z(nkz-1,ior)+vno3z(nkz,ior))/2.)*dH2D
          dzMasseN3(nkz) = ((vno3z(nkz-1,ior)+vno3z(nkz,ior))/2.)*dH2D
          sumgP = sumgP + ((gelPz(nkz-1,ior)+gelPz(nkz,ior))/2.)*dH2D
          dzMassegP(nkz) = ((gelPz(nkz-1,ior)+gelPz(nkz,ior))/2.)*dH2D
          sumSi = sumSi + ((Siz(nkz-1,ior)+Siz(nkz,ior))/2.)*dH2D
          dzMasseSi(nkz) = ((Siz(nkz-1,ior)+Siz(nkz,ior))/2.)*dH2D
          sumChl = sumChl + ((Chlaz(nkz-1,ior)+Chlaz(nkz,ior))/2.)*dH2D
          dzMasseChl(nkz) = ((Chlaz(nkz-1,ior)+Chlaz(nkz,ior))/2.)*dH2D
          sumChlk = sumChlk + ((hChlkz(mstr,nkz-1,ior)+hChlkz(mstr,nkz,ior))/2.)*dH2D
          dzMasseChlk(nkz) = ((hChlkz(mstr,nkz-1,ior)+hChlkz(mstr,nkz,ior))/2.)*dH2D
          sumChlb = sumChlb + ((hChlbz(mstr,nkz-1,ior)+hChlbz(mstr,nkz,ior))/2.)*dH2D
          dzMasseChlb(nkz) = ((hChlbz(mstr,nkz-1,ior)+hChlbz(mstr,nkz,ior))/2.)*dH2D
          sumChlg = sumChlg + ((hChlgz(mstr,nkz-1,ior)+hChlgz(mstr,nkz,ior))/2.)*dH2D
          dzMasseChlg(nkz) = ((hChlgz(mstr,nkz-1,ior)+hChlgz(mstr,nkz,ior))/2.)*dH2D
          sumCChlk = sumCChlk + ((hCChlkz(mstr,nkz-1,ior)+hCChlkz(mstr,nkz,ior))/2.)*dH2D
          dzMasseCChlk(nkz) = ((hCChlkz(mstr,nkz-1,ior)+hCChlkz(mstr,nkz,ior))/2.)*dH2D
          sumCChlb = sumCChlb + ((hCChlbz(mstr,nkz-1,ior)+hCChlbz(mstr,nkz,ior))/2.)*dH2D
          dzMasseCChlb(nkz) = ((hCChlbz(mstr,nkz-1,ior)+hCChlbz(mstr,nkz,ior))/2.)*dH2D
          sumCChlg = sumCChlg + ((hCChlgz(mstr,nkz-1,ior)+hCChlgz(mstr,nkz,ior))/2.)*dH2D
          dzMasseCChlg(nkz) = ((hCChlgz(mstr,nkz-1,ior)+hCChlgz(mstr,nkz,ior))/2.)*dH2D
          sumgsP = sumgsP + ((hgesPz(mstr,nkz-1,ior)+hgesPz(mstr,nkz,ior))/2.)*dH2D
          dzMassegsP(nkz) = ((hgesPz(mstr,nkz-1,ior)+hgesPz(mstr,nkz,ior))/2.)*dH2D
          sumgsN = sumgsN + ((hgesNz(mstr,nkz-1,ior)+hgesNz(mstr,nkz,ior))/2.)*dH2D
          dzMassegsN(nkz) = ((hgesNz(mstr,nkz-1,ior)+hgesNz(mstr,nkz,ior))/2.)*dH2D
        endif
      enddo

      

      D1Massek = aki(ior) * ((nkzs(ior)-1)*dH2D)
      D1Masseb = abl(ior) * ((nkzs(ior)-1)*dH2D)
      D1Masseg = agr(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseO = vo2(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseN4 = vnh4(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseN2 = vno2(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseN3 = vno3(ior) * ((nkzs(ior)-1)*dH2D)
      D1MassegP = gelP(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseSi = Si(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseChl = Chla(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseChlk = Chlaki(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseChlb = Chlabl(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseChlg = Chlagr(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseCChlk = akbcm(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseCChlb = abbcm(ior) * ((nkzs(ior)-1)*dH2D)
      D1MasseCChlg = agbcm(ior) * ((nkzs(ior)-1)*dH2D)
      D1MassegsP = gesP(ior) * ((nkzs(ior)-1)*dH2D)
      D1MassegsN = gesN(ior) * ((nkzs(ior)-1)*dH2D)


      dMassek = D1Massek - sumakiz
      dMasseb = D1Masseb - sumablz
      dMasseg = D1Masseg - sumagrz
      dMasseO = D1MasseO - sumO2
      dMasseN4 = D1MasseN4 - sumNH4
      dMasseN2 = D1MasseN2 - sumNO2
      dMasseN3 = D1MasseN3 - sumNO3
      dMassegP = D1MassegP - sumgP
      dMasseSi = D1MasseSi - sumSi
      dMasseChl = D1MasseChl - sumChl
      dMasseChlk = D1MasseChlk - sumChlk
      dMasseChlb = D1MasseChlb - sumChlb
      dMasseChlg = D1MasseChlg - sumChlg
      dMasseCChlk = D1MasseCChlk - sumCChlk
      dMasseCChlb = D1MasseCChlb - sumCChlb
      dMasseCChlg = D1MasseCChlg - sumCChlg
      dMassegsP = D1MassegsP - sumgsP
      dMassegsN = D1MassegsN - sumgsN

      do nkz = nkzs(ior),1,-1
        if(sumakiz>0.0)then 
          Masse_neuk(nkz) = dzMassek(nkz)+dMassek*dzMassek(nkz)/sumakiz
            else
              Masse_neuk(nkz) = dzMassek(nkz)
        endif
        if(sumablz>0.0)then 
          Masse_neub(nkz) = dzMasseb(nkz)+dMasseb*dzMasseb(nkz)/sumablz
            else
              Masse_neub(nkz) = dzMasseb(nkz)
        endif
        if(sumagrz>0.0)then 
          Masse_neug(nkz) = dzMasseg(nkz)+dMasseg*dzMasseg(nkz)/sumagrz
            else
              Masse_neug(nkz) = dzMasseg(nkz)
        endif
        if(sumO2>0.0)then 
          Masse_neuO(nkz) = dzMasseO(nkz)+dMasseO*dzMasseO(nkz)/sumO2
            else
              Masse_neuO(nkz) = dzMasseO(nkz)
        endif
        if(sumNH4>0.0)then 
          Masse_neuN4(nkz) = dzMasseN4(nkz)+dMasseN4*dzMasseN4(nkz)/sumNH4
            else
              Masse_neuN4(nkz) = dzMasseN4(nkz)
        endif
        if(sumNO2>0.0)then 
          Masse_neuN2(nkz) = dzMasseN2(nkz)+dMasseN2*dzMasseN2(nkz)/sumNO2
            else
              Masse_neuN2(nkz) = dzMasseN2(nkz)
        endif
        if(sumNO3>0.0)then 
          Masse_neuN3(nkz) = dzMasseN3(nkz)+dMasseN3*dzMasseN3(nkz)/sumNO3
            else
              Masse_neuN3(nkz) = dzMasseN3(nkz)
        endif
        if(sumgP>0.0)then 
          Masse_neugP(nkz) = dzMassegP(nkz)+dMassegP*dzMassegP(nkz)/sumgP
            else
              Masse_neugP(nkz) = dzMassegP(nkz)
        endif
        if(sumSi>0.0)then 
          Masse_neuSi(nkz) = dzMasseSi(nkz)+dMasseSi*dzMasseSi(nkz)/sumSi
            else
              Masse_neuSi(nkz) = dzMasseSi(nkz)
        endif
        if(sumChl>0.0)then 
          Masse_neuChl(nkz) = dzMasseChl(nkz)+dMasseChl*dzMasseChl(nkz)/sumChl
            else
              Masse_neuChl(nkz) = dzMasseChl(nkz)
        endif
        if(sumChlk>0.0)then 
          Masse_neuChlk(nkz) = dzMasseChlk(nkz)+dMasseChlk*dzMasseChlk(nkz)/sumChlk
            else
              Masse_neuChlk(nkz) = dzMasseChlk(nkz)
        endif
        if(sumChlb>0.0)then 
          Masse_neuChlb(nkz) = dzMasseChlb(nkz)+dMasseChlb*dzMasseChlb(nkz)/sumChlb
            else
              Masse_neuChlb(nkz) = dzMasseChlb(nkz)
        endif
        if(sumChlg>0.0)then 
          Masse_neuChlg(nkz) = dzMasseChlg(nkz)+dMasseChlg*dzMasseChlg(nkz)/sumChlg
            else
              Masse_neuChlg(nkz) = dzMasseChlg(nkz)
        endif
        if(sumCChlk>0.0)then 
          Masse_neuCChlk(nkz) = dzMasseCChlk(nkz)+dMasseCChlk*dzMasseCChlk(nkz)/sumCChlk
            else
              Masse_neuCChlk(nkz) = dzMasseCChlk(nkz)
        endif
        if(sumCChlb>0.0)then 
          Masse_neuCChlb(nkz) = dzMasseCChlb(nkz)+dMasseCChlb*dzMasseCChlb(nkz)/sumCChlb
            else
              Masse_neuCChlb(nkz) = dzMasseCChlb(nkz)
        endif
        if(sumCChlg>0.0)then 
          Masse_neuCChlg(nkz) = dzMasseCChlg(nkz)+dMasseCChlg*dzMasseCChlg(nkz)/sumCChlg
            else
              Masse_neuCChlg(nkz) = dzMasseCChlg(nkz)
        endif
        if(sumgsP>0.0)then 
          Masse_neugsP(nkz) = dzMassegsP(nkz)+dMassegsP*dzMassegsP(nkz)/sumgsP
            else
              Masse_neugsP(nkz) = dzMassegsP(nkz)
        endif
        if(sumgsN>0.0)then 
          Masse_neugsN(nkz) = dzMassegsN(nkz)+dMassegsN*dzMassegsN(nkz)/sumgsN
            else
              Masse_neugsN(nkz) = dzMassegsN(nkz)
        endif

        if(nkz==nkzs(ior))then
          akiz(nkz,ior) = (Masse_neuk(nkz)/dH2D) - 0.5*(akiz(nkz-1,ior)-akiz(nkz,ior))
          ablz(nkz,ior) = (Masse_neub(nkz)/dH2D) - 0.5*(ablz(nkz-1,ior)-ablz(nkz,ior))
          agrz(nkz,ior) = (Masse_neug(nkz)/dH2D) - 0.5*(agrz(nkz-1,ior)-agrz(nkz,ior))
          vo2z(nkz,ior) = (Masse_neuO(nkz)/dH2D) - 0.5*(vo2z(nkz-1,ior)-vo2z(nkz,ior))
          vnh4z(nkz,ior) = (Masse_neuN4(nkz)/dH2D) - 0.5*(vnh4z(nkz-1,ior)-vnh4z(nkz,ior))
          vno2z(nkz,ior) = (Masse_neuN2(nkz)/dH2D) - 0.5*(vno2z(nkz-1,ior)-vno2z(nkz,ior))
          vno3z(nkz,ior) = (Masse_neuN3(nkz)/dH2D) - 0.5*(vno3z(nkz-1,ior)-vno3z(nkz,ior))
          gelPz(nkz,ior) = (Masse_neugP(nkz)/dH2D) - 0.5*(gelPz(nkz-1,ior)-gelPz(nkz,ior))
          Siz(nkz,ior) = (Masse_neuSi(nkz)/dH2D) - 0.5*(Siz(nkz-1,ior)-Siz(nkz,ior))
          Chlaz(nkz,ior) = (Masse_neuChl(nkz)/dH2D) - 0.5*(Chlaz(nkz-1,ior)-Chlaz(nkz,ior))
          hChlkz(mstr,nkz,ior) = (Masse_neuChlk(nkz)/dH2D) - 0.5*(hChlkz(mstr,nkz-1,ior)-hChlkz(mstr,nkz,ior))
          hChlbz(mstr,nkz,ior) = (Masse_neuChlb(nkz)/dH2D) - 0.5*(hChlbz(mstr,nkz-1,ior)-hChlbz(mstr,nkz,ior))
          hChlgz(mstr,nkz,ior) = (Masse_neuChlg(nkz)/dH2D) - 0.5*(hChlgz(mstr,nkz-1,ior)-hChlgz(mstr,nkz,ior))
          hCChlkz(mstr,nkz,ior) = (Masse_neuCChlk(nkz)/dH2D) - 0.5*(hCChlkz(mstr,nkz-1,ior)-hCChlkz(mstr,nkz,ior))
          hCChlbz(mstr,nkz,ior) = (Masse_neuCChlb(nkz)/dH2D) - 0.5*(hCChlbz(mstr,nkz-1,ior)-hCChlbz(mstr,nkz,ior))
          hCChlgz(mstr,nkz,ior) = (Masse_neuCChlg(nkz)/dH2D) - 0.5*(hCChlgz(mstr,nkz-1,ior)-hCChlgz(mstr,nkz,ior))
          hgesPz(mstr,nkz,ior) = (Masse_neugsP(nkz)/dH2D) - 0.5*(hgesPz(mstr,nkz-1,ior)-hgesPz(mstr,nkz,ior))
          hgesNz(mstr,nkz,ior) = (Masse_neugsN(nkz)/dH2D) - 0.5*(hgesNz(mstr,nkz-1,ior)-hgesNz(mstr,nkz,ior))
          cycle
        endif
        akiz(nkz,ior) = (Masse_neuk(nkz+1)*2./dH2D)-akiz(nkz+1,ior)
        ablz(nkz,ior) = (Masse_neub(nkz+1)*2./dH2D)-ablz(nkz+1,ior)
        agrz(nkz,ior) = (Masse_neug(nkz+1)*2./dH2D)-agrz(nkz+1,ior)
        vo2z(nkz,ior) = (Masse_neuO(nkz+1)*2./dH2D)-vo2z(nkz+1,ior)
        vnh4z(nkz,ior) = (Masse_neuN4(nkz+1)*2./dH2D)-vnh4z(nkz+1,ior)
        vno2z(nkz,ior) = (Masse_neuN2(nkz+1)*2./dH2D)-vno2z(nkz+1,ior)
        vno3z(nkz,ior) = (Masse_neuN3(nkz+1)*2./dH2D)-vno3z(nkz+1,ior)
        gelPz(nkz,ior) = (Masse_neugP(nkz+1)*2./dH2D)-gelPz(nkz+1,ior)
        Siz(nkz,ior) = (Masse_neuSi(nkz+1)*2./dH2D)-Siz(nkz+1,ior)
        Chlaz(nkz,ior) = (Masse_neuChl(nkz+1)*2./dH2D)-chlaz(nkz+1,ior)
        hChlkz(mstr,nkz,ior) = (Masse_neuChlk(nkz+1)*2./dH2D)-hchlkz(mstr,nkz+1,ior)
        hChlbz(mstr,nkz,ior) = (Masse_neuChlb(nkz+1)*2./dH2D)-hchlbz(mstr,nkz+1,ior)
        hChlgz(mstr,nkz,ior) = (Masse_neuChlg(nkz+1)*2./dH2D)-hchlgz(mstr,nkz+1,ior)
        hCChlkz(mstr,nkz,ior) = (Masse_neuCChlk(nkz+1)*2./dH2D)-hCChlkz(mstr,nkz+1,ior)
        hCChlbz(mstr,nkz,ior) = (Masse_neuCChlb(nkz+1)*2./dH2D)-hCChlbz(mstr,nkz+1,ior)
        hCChlgz(mstr,nkz,ior) = (Masse_neuCChlg(nkz+1)*2./dH2D)-hCChlgz(mstr,nkz+1,ior)
        hgesPz(mstr,nkz,ior) = (Masse_neugsP(nkz+1)*2./dH2D)-hgesPz(mstr,nkz+1,ior)
        hgesNz(mstr,nkz,ior) = (Masse_neugsN(nkz+1)*2./dH2D)-hgesNz(mstr,nkz+1,ior)

!        hCChlkz(mstr,nkz,ior) =  akiz(nkz,ior)*Caki*1000./hChlkz(mstr,nkz,ior)
!        hCChlbz(mstr,nkz,ior) =  ablz(nkz,ior)*Cabl*1000./hChlbz(mstr,nkz,ior)
!        hCChlgz(mstr,nkz,ior) =  agrz(nkz,ior)*Cagr*1000./hChlgz(mstr,nkz,ior)
      enddo  

   end subroutine Masse_neu_qsim     
