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
program qsim
   
   use allodim
   use aparam
   use module_model_settings
   use module_metabolism
   implicit none
   ! izdt Einheiten min oder Stunden Beruecksichtigung bei itime
   ! Bei Tracerrechnung wird für die Variable tempw mit der Tracermenge belegt
   character       :: ckenn,cpoint
   character(2)    :: chcon,ckenn_vers,ckenn_vers1
   character(7)    :: cmin,cmax
   character(40)   :: erename, modname
   character(201)  :: ctext
   character(275)  :: pfadstring
   character(6000) :: langezeile, message
   logical         :: kontroll, einmalig, linux,mitsedflux, write_csv_output
   integer         :: open_error, jjj, iior
   integer         :: iend, iwied, ilang, ilbuhn, jlauf
   integer         :: jtag, iergeb, itracer_vor, nndr, jstr
   integer         :: nazstrs, isumanzsta, ieinl, mstr, msta
   integer         :: mrb, iseg, mtracer, itags, monats
   integer         :: jahrs, ij, lait1, laid1, laim1
   integer         :: mpf, ms, md, mc, mb
   integer         :: mu, mwe, mv, mz, ma
   integer         :: me, ndr, iv, j, ksta
   integer         :: jsed, nkztot_max, jkenn, monat_schr, jahr_schr
   integer         :: itags_schr, istr, nstr, istrs, ifhstr
   integer         :: ianze_max, ior, itimeh, itimeb, itimea
   integer         :: itimee, itime, i_rands, iw_max, mstrrb
   integer         :: irb, i_k11, i_k12, i_k13, i_k14
   integer         :: i_k15, i_k16, i_k17, i_k18, i_k19
   integer         :: i_k110, i_k111, i_k112, i_k113, i_k114
   integer         :: i_k115, i_k116, i_k117, i_k118, i_k119
   integer         :: i_k120, i_k121, i_k122, i_k123, i_k124
   integer         :: i_k125, i_k126, nkzsmx, imet, mrb_1
   integer         :: j_ist, jj, js, iein, mrand
   integer         :: inkzmx, mstr1, ib, nkz, kanz
   integer         :: ista, jnkz, kanz2, jnkz2, kanz1
   integer         :: nkzs_hc, nkzs_hc1, i_estrnr, minute, kein
   integer         :: itstart, mstart, itmax, mmax, itend
   integer         :: mend, istriz_neu, isim_end, nwaerm, izeits
   integer         :: jpoin1, ico, ke, itagv, monatv
   integer         :: jahrv, jtage, ianfan, itag_schr, i
   real            :: lat_k, o2ein
   real            :: mikonss, mxkonss, bxcoli,algae_biomass
   real            :: POM_sed, BedGS, xsedvvertz
   real            :: dh2d, geol, geob, uhrs, uhrz
   real            :: hcmin, hcuhrz, dlmax1, dlmax2, dlmax3
   real            :: dlmax4, coro1, fkmgit, trpmin, trpmax
   real            :: tggbal, tgkbal, coroe, corose, pom_sedb
   real            :: spewksx1, wuebkx1, psrefsx1, extkx1, uhrz_schr
   real            :: hcumt, dt, fhprof, hcon, hcontm
   real            :: sum_qeinl, hcq1, hcq2, hcq3, hcq4
   real            :: hcq5, hcq6, hcq7, hcq8, hcq9
   real            :: hcq10, hcq11, hcq12, hcq13, hcq14
   real            :: hcq15, hcq16, hcq17, hcq18, hcq19
   real            :: hcq20, hcq21, hcq22, hcq23, hcq24
   real            :: hcq25, hcq26, hc1, hc2, hc3
   real            :: hc4, hc5, hc6, hc7, hc8
   real            :: hc9, hc10, hc11, hc12, hc13
   real            :: hc14, hc15, hc16, hc17, hc18
   real            :: hc19, hc20, hc21, hc22, hc23
   real            :: hc24, hc25, hc26, hc27, fssgrs
   real            :: fbsgrs, bsbzoo, hcchla, hczoos, hcnh4s
   real            :: hcno2s, hcno3s, hcgeps, hcbsb, hccsb
   real            :: hcvkg, hcantb, a2ki, a3ki, a1ki
   real            :: a3bl, a2bl, a1bl, a3gr, a1gr
   real            :: a2gr, toc_csb, cdges, cref, cpges
   real            :: toc, dk, sa, zlk, zg
   real            :: su, wtst, wtst_t, algb5, zoobsb
   real            :: algcs, zoocsb, hcs1, hcs2, hcs3
   real            :: hcs6, hcs7, hcs8, hcs9, hcs10
   real            :: hcs20, hcs21, hcs22, hcs23, hcs24
   real            :: hcs25, hcs26, hcs27, hcs28, hcs29
   real            :: hcs30, hcs31, hcs32, hcs33, hcs34
   real            :: hcs35, hcs36, hcs37, hcs38, hcs39
   real            :: hcs40, hcs41, hcs42, hcs43, hcs44
   real            :: hcs45, hcs46, hcs47, hcs48, hcs49
   real            :: hcs50, hcs51, hcs52, hcs53, hcs54
   real            :: hcs55, hcs56, hcs57, hcs58, hcs59
   real            :: hcs60, hcs61, hcs62, hcs63, hcs64
   real            :: hcs65, hcs66, hcs77, hcs78, hcs79
   real            :: hcs80, hcs81, hcs82, hcs83, hcs85
   real            :: hcs86, hcs95, hcs99, hcs100, hcs101
   real            :: hcs102, hcs103, hcs104, hcs105, hcs106
   real            :: hcs107, hcs108, hcs110, hcs111, hcs112
   real            :: hcs113, hcs114, hcs115, hcs116, hcs117
   real            :: hcs118, hcs119, hcs120, hcs121, hcs122
   real            :: hcs123, hcs124, hcs125, hcs126, hcq
   real            :: hmue, hk, vhplus, rmin, uhrzhm
   real            :: sumtracer, dtmin, dtmin_mac, elenl, ust
   real            :: alpha, cr_zahl, cr_zahl_mac, hc_alpha, hc_dl
   real            :: dtneu, dtneu_mac, schwia, diff1, diff2
   real            :: saettk, akrema, sbioki, saettb, pbiogr
   real            :: pbiobl, diff3, diff4, diff5, diff6
   real            :: diff7, diff8, diff9, diff10, diff11
   real            :: diff12, diff13, diff14, vco2s, tiefev
   real            :: algo, diff15, diff16, diff17, diff18
   real            :: diff19, diff20, diff21, diff22, deltat
   real            :: st, so2, ski, sgr, sbl
   real            :: schl, sn4, sn2, sn3, sp
   real            :: spges, snges, ssi, sumh, uhrsv
   real            :: hconu, tend, vn4end, vn2end, vn3end
   real            :: vo2end, vgpend, siend, akiend, agrend
   real            :: ablend, chlend, uhrhm, bhnfy, sbal
   real            :: sco, scos, salc, salw, salr
   real            :: salm, sals, salz, sald, ztp
   real            :: salco, cbsbab, abbau, hnfin, san
   real            :: vx0mue, vx02mu, sabow, sabor, bakg
   real            :: bdaw, bdar, bam, bas, baz
   real            :: sbco, xtempw, xchnf, xbvhnf, xcd1
   real            :: xcd2, xcp1, xcp2, xcm, xbac
   real            :: xbsb5, xcsb, xnh4, xchla, xvkigr
   real            :: xantbl, xaki, xagr, xabl, xchlak
   real            :: xchlag, xchlab, xcchlk, xcchlg, xcchlb
   real            :: xo2, xzooind, xvph, xvno3, xvno2
   real            :: xgelp, xsi, xca, xmw, xlf
   real            :: xcoli, xgszn, xglzn, xgscad, xglcad
   real            :: xgscu, xglcu, xgsni, xglni, xgsas
   real            :: xglas, xgspb, xglpb, xgscr, xglcr
   real            :: xgsfe, xglfe, xgshg, xglhg, xgsmn
   real            :: xglmn, xgsu, xglu, xdlarn, xss
   real            :: xpfl, xgsp, xgsn, xcori, xcoris
   real            :: xbal, xsusn, xbettn, xdon, xalgn
   real            :: xalno3, xflun3, xvx0, xvx02, xsedx
   real            :: xsedal, xalgzo, xalgdr, xalgco, xvoldr
   real            :: xdrpfe, xabeow, xabeor, xdalg, xdalga
   real            :: xalmor, xblmor, xsgo2n, xsdbsb, xsoein
   real            :: xsalgo, xo2nit, xalgo, xalgao, xbsbt
   real            :: xschlr, xbsbbe, xo2phy, xro2dr, xzooro
   real            :: xpo2p, xpo2r, xir, xrmue, xrakr
   real            :: xrbar, xffood, xfik, xfig, xfib
   real            :: xnaehr, xakmua, xagmua, xabmua, xfhek
   real            :: xfheg, xfheb, xakrau, xagrea, xabrea
   real            :: xhnfmu, xhnfre, xhnfup, xhnfmo, xhnfex
   real            :: xhnfdr, xhnfz, xbacmu, xhnfba, xnl0
   real            :: xpl0, xjno3, xjnh4, xjpo4, xjo2
   real            :: xjsi, xakigr, xchnfi, bxtemp, bxno3
   real            :: bxno2, bxnh4, bxgelp, bxchla, bxssal
   real            :: bxsi, bxzooi, bxbsb5, bxcsb, bxaki
   real            :: bxagr, bxabl, bxo2, bxph, bxca
   real            :: bxmw, bxlf, bxlarn, bxnl0, bxpl0
   real            :: bxgsp, bxgsn, bxdalg, bxvkg, bxantb
   real            :: bxdaa, bxamor, bxseda, bxalgz, bxaldr
   real            :: bxalco, bxfik, bxfig, bxfib, xbnaeh
   real            :: bxkmue, bxgmue, bxbmue, bxhek, bxheg
   real            :: bxheb, bxkre, bxgre, bxbre, bxchlk
   real            :: bxchlg, bxchlb, bxfln3, bxbetn, bxjno3
   real            :: bxjnh4, bxjpo4, bxjo2, bxjsi, bmikonss
   real            :: bxkonss, bmxkonss, bxdrpf, bxgszn, bxglzn
   real            :: bxgscad, bxglcad, bxgscu, bxglcu, bxgsni
   real            :: bxglni, bxgsas, bxglas, bxgspb, bxglpb
   real            :: bxgscr, bxglcr, bxgsfe, bxglfe, bxgshg
   real            :: bxglhg, bxgsmn, bxglmn, bxgsu, bxglu
   real            :: bxakg, xkonss, bxmicl, bxmxcl
   character(len=50),dimension(ialloc5,ialloc1) :: cename
   character(len=40),dimension(:),allocatable   :: strname,strnumm
   integer                                 :: maus, read_error, anze, azstr, azstr_read, anzej, stunde, anzema
   integer                                 :: tdj, schrnr, rbnr
   integer, dimension(2)                   :: ikanz
   integer, dimension(ialloc1)             :: typ, iorla, iorle
   integer, dimension(ialloc2)             :: flag, jiein, zwjiein, ischif, zwnkzs, nkzsy, nkzs
   integer, dimension(:), allocatable      :: hanze,ianze, striz,isub_dt,imac,isub_dt_mac, mstr_ist, strnr, mstra
   integer, dimension(:), allocatable      :: ieinsh, ieinls, nbuhn, iflri, isegs, strid, janzwt, janzws, jlwo2, irb_k1, izufluss
   integer, dimension(:), allocatable      :: imrb_k1, mpfs, mss, mds, mcs, mbs, mus, mwes, mvs, mzs, mas, mes
   integer, dimension(:), allocatable      :: itsts, msts, itmaxs, mmaxs, itends, mends, laits, laims, laids, mstas
   integer, dimension(:), allocatable      :: abfr, mwehr, mrbs, nstrs, nnstrs, iflri_l
   
   integer, dimension(:,:), allocatable    :: it_h, it_hy, iorlah, iorleh, typh, ischig, ikwsta, idwe, mstrle, istund
   integer, dimension(:,:), allocatable    :: rbtyp, weinl, nrschr, hnkzs, nkzmx, znkzs, inkzs, ibschi
   integer, dimension(:,:), allocatable    :: hflag, hjiein, hischf, estrnr
   
   real, dimension(2)                      :: xdrakr, xdrbar, xdrmor, xidras, xdrmas
   real, dimension(4)                      :: gwdre, zdreie, zdrese, xdrbio, xdbios, xgewdr
   real, dimension(20)                     :: glob, tlmax, tlmin, cloud, wtyp, ro, wge
   real, dimension(ialloc5)                :: hcs67, hcs68, hcs69, hcs70, hcs71, hcs72, hcs73, hcs74, hcs75, hcs76
   real, dimension(ialloc5)                :: hcs84, hcs87, hcs88, hcs89, hcs90, hcs91, hcs92, hcs93, hcs94
   real, dimension(ialloc5)                :: hcs96, hcs97, hcs98
   real, dimension(ialloc1)                :: einlk, qeinl, ebsb, ecsb, enh4, ex0, eo2, etemp, echla, ep
   real, dimension(ialloc1)                :: elf, eph, emw, eca, ex02, eno2, ess, ewaerm, esi, ezind, eno3
   real, dimension(ialloc1)                :: echnf, ebvhnf, egesn, egesp, ecoli, evkigr,eantbl, enl0, epl0
   real, dimension(ialloc1)                :: qeinll, bsbl, csbl, enh4l, x0l, x02l, o2l, etempl, gpl, sil
   real, dimension(ialloc1)                :: eno2l, eno3l, gesnl, gespl, ssl, phl, elfl, cal, colil, enl0l
   real, dimension(ialloc1)                :: pl0l, chlal
   real, dimension(ialloc2)                :: elen, vmitt, tiefe, flae, breite, rau, rhyd, vabfl, stind, nl0, pl0
   real, dimension(ialloc2)                :: q_nk, q_pk, q_sk, q_ng, q_pg, q_nb, q_pb, akmuea, ftaaus, fiaus
   real, dimension(ialloc2)                :: fheaus, fhegas, fhegy, agmuea, agmuey, akraus, rmuas, rmuasy, agreau
   real, dimension(ialloc2)                :: agrey, rakr, rakry, figaus, figy, rbar, rbary, dorgss, hnfmua, bacmua
   real, dimension(ialloc2)                :: hnfmuy, bacmuy, hnfbay,hnfrey, hnfupy, hnfmoy, hnfexy, hnfdry, hnfzy
   real, dimension(ialloc2)                :: hnfrea, hnfupa, hnfmoa, hnfexa, hnfdra, hnfza, akmuey, ftay, fiy, fhey
   real, dimension(ialloc2)                :: akry, dl, resdr, exdrvg, exdrvk, dlarvn
   real, dimension(ialloc2)                :: dlarny, pflmin, pflmax, po2p, po2r, pfl, valtbl, edufbl, valtbr, edufbr
   real, dimension(ialloc2)                :: drpfey, drpfec, ssdr, drfaek, drfaeg, drfaes, volfdr, tsed, tempw, zexki
   real, dimension(ialloc2)                :: templ, zexgr, dzres1, dzres2, obsb, vcsb, vbsb, cm, bac, ocsb, vnh4, vno3
   real, dimension(ialloc2)                :: vno2, si, chla, ssalg, zooind, gelp, vco2, aki, agr, ro2dr, zooro2, akitbr
   real, dimension(ialloc2)                :: agrtbr, dalggr, dalgki, dalgag, dalgak, albewg, alberg, albewk, alberk
   real, dimension(ialloc2)                :: vx0, go2n, vo2, sgo2n, vx02, gesn, gesp, sdbsb, abszo, bsbt, bsbct, bsbctp
   real, dimension(ialloc2)                :: dlmax, dlmaxs, tracer, svhemk, svhemg, doscf, extk, sised
   real, dimension(ialloc2)                :: skmor, schwi, dz2d, dc_denw, fkm
   real, dimension(ialloc2)                :: chnf, hnfbac, bsbhnf, drhnf, bvhnf, coli, zhnf, zbac, ro2hnf, tpki, tpgr
   real, dimension(ialloc2)                :: abl, antbl, abbcm, abltbr, svhemb, dblmor, tpbl, dalgbl, dalgab
   real, dimension(ialloc2)                :: sedalb, algzob, sedalb0, fibaus, abmuea, fhebas, abreau, algdrb, algcob
   real, dimension(ialloc2)                :: chlabl, exdrvb, zexbl, ablnh4, ablno3, drfaeb
   real, dimension(ialloc2)                :: ably, abln4y, sedaby, algzby, algdby, algcby, dalgby, dalaby, dbmory
   real, dimension(ialloc2)                :: abmuey, fiby, fheby, abrey, antbly, tpbly
   real, dimension(ialloc2)                :: tau2, hctau1, hctau2, zwtsed, zwtemp, zwvm, zwtief,zwextk
   real, dimension(ialloc2)                :: zwno3, zwnh4, zwgelp, zwsvhk, zwchla, zwir, zwssa, zwsi, zwdalk
   real, dimension(ialloc2)                :: zwdaak, zwsedk, zwzok, zwkmor, zwkigr, zwantb, zwkbcm, zwaki, zwagr
   real, dimension(ialloc2)                :: zwsisd, zwkmua, zwfta, zwfia, zwfhea, zwkrau
   real, dimension(ialloc2)                :: zwsvhb, zwsvhg, zwdalg, zwdaag, zwsedg, zwzog, zwgmor, zwgbcm
   real, dimension(ialloc2)                :: zwgmua, zwfiga, zwfhga, zwgrau, zwadrk, zwadrg, zwacok, zwacog, zwvo2
   real, dimension(ialloc2)                :: zwzooi, zwabsz, zwdzr1, zwdzr2, zwzexk, zwzexg, zwrmue, zwiras, zwrakr
   real, dimension(ialloc2)                :: zwrbar, zwph, zwcsed_abb
   real, dimension(ialloc2)                :: zwzexb, zwobsb, zwocsb
   real, dimension(ialloc2)                :: zwdfak, zwdfab, zwdfag, zwdfas, zwssdr, zwcsed
   real, dimension(ialloc2)                :: zwnl0, zwpl0
   real, dimension(ialloc2)                :: zwabwg, zwabwk, zwabrg, zwabrk
   real, dimension(ialloc2)                :: zworgs, zwss, zwfssg, zwseds
   real, dimension(ialloc2)                :: zwtpki, zwtpgr, zwchlk, zwchlg, zwchlb
   real, dimension(ialloc2)                :: zwn4z, zwn3z, zwpz
   real, dimension(ialloc2)                :: zwsiz, zup_pk, zup_nk, zup_si, zq_pk, zq_nk, zq_sk, zaktbr
   real, dimension(ialloc2)                :: zup_pg, zup_ng, zagtbr, zq_pg, zq_ng, zwakz, zwaakz, zwagz, zwaagz
   real, dimension(ialloc2)                :: zwdalb, zwdaab, zwsedb, zwzob, zwbmor, zwbbcm, zwabl, zwbmua, zwfiba
   real, dimension(ialloc2)                :: zwfhba, zwbrau, zwadrb, zwacob, zwtpbl, zup_pb, zup_nb, zq_pb, zq_nb
   real, dimension(ialloc2)                :: zabtbr, zwabz, zwaabz,  zwflae, zwlboe, zwskmo, zww2, zwsdom
   real, dimension(ialloc2)                :: zwbso, zwjn2,zwtgzoo, zwcoli, zwdoscf, zwakmor_1, zwagmor_1, zwabmor_1
   real, dimension(ialloc2)                :: zwgszn, zwglzn, zwgscad, zwglcad, zwgscu, zwglcu, zwgsni, zwglni
   real, dimension(ialloc2)                :: zwgsas, zwglas, zwgspb, zwglpb, zwgscr, zwglcr, zwgsfe, zwglfe
   real, dimension(ialloc2)                :: zwgshg, zwglhg, zwgsmn, zwglmn, zwgsu, zwglu, zwsseros
   real, dimension(ialloc2)                :: zwznsed,zwcadsed,zwcused,zwnised,zwassed,zwpbsed
   real, dimension(ialloc2)                :: zwcrsed,zwfesed,zwhgsed,zwmnsed,zwused
   real, dimension(ialloc2)                :: zwkorn, zwfln3, zwjno3, zwjnh4, zwjpo4, zwjo2, zwjsi, zwjdoc1, zwjdoc2
   real, dimension(ialloc2)                :: zwsedalg_mq, zwsedss_mq, ss, vol, ir, gwdmax, sedx0, don, susn
   real, dimension(ialloc2)                :: bettn, agrnh4, akinh4, susno, akino3, agrno3, iras, sedalg, sedalk
   real, dimension(ialloc2)                :: susn2, pfln1, pfln2
   real, dimension(ialloc2)                :: sedalk0, sedalg0, algzog, algzok, abrzo1, algdrg, algdrk, vkigr, chlagr
   real, dimension(ialloc2)                :: mw, pw,lf, ca, vph, dgrmor, dkimor, dalgo, dalgao, bsbbet, o2ein1
   real, dimension(ialloc2)                :: chlaki, abeowg, abeorg, abeowk, abeork, akbcm, agbcm
   real, dimension(ialloc2)                :: lboem, bsohlm, cmatgr, cmatki, ffood, fssgr, fbsgr, frfgr, sedss
   real, dimension(ialloc2)                :: lfy, akiy, agry, iry, tempwy, vbsby, vcsby, vnh4y, tiefey, vx02y
   real, dimension(ialloc2)                :: vo2y, vno3y, vno2y, vx0y, siy, vkigry, cmy, bacy, chnfy, bvhnfy, dly
   real, dimension(ialloc2)                :: chlay, chlaky, chlagy, chlaby, ssalgy, zooiny, gelpy, coliy, tau2y, gspy
   real, dimension(ialloc2)                :: mwy, cay, vphy, tpkiy, tpgry, gsny, orgcsd0, susny, bettny, dony
   real, dimension(ialloc2)                :: agrn4y, akin4y, flun3y, sedx0y, susnoy, sedagy, sedaky, algzgy, alno3y
   real, dimension(ialloc2)                :: algzky, algdgy, algdky, volfdy, abowgy, abowky, aborgy, aborky, dalggy
   real, dimension(ialloc2)                :: dalgky, dalagy, dalaky, dgmory, dkmory, sgo2ny, sdbsby
   real, dimension(ialloc2)                :: bsbty, dalgoy, dalaoy, schlry, bsbbey, o2ei1y, ro2dry, zoro2y, po2py
   real, dimension(ialloc2)                :: po2ry, nl0y, pl0y, extky, jno3y, jnh4y, jpo4y, jo2y, jsiy, q_nky, q_pky
   real, dimension(ialloc2)                :: q_sky, q_ngy, q_pgy, q_nby, q_pby, coroy, corosy, ffoody, pfly
   real, dimension(ialloc2)                :: alby, cchlky, cchlgy, cchlby
   real, dimension(ialloc2)                :: gszny, glzny, gscady, glcady, gscuy, glcuy, gsniy, glniy
   real, dimension(ialloc2)                :: gsasy, glasy, gspby, glpby, gscry, glcry, gsfey, glfey
   real, dimension(ialloc2)                :: gshgy, glhgy, gsmny, glmny, gsuy, gluy
   real, dimension(ialloc2)                :: btempy, bno3y, bnh4y, bgelpy, bchlay, bssaly, bsiy, bakiy, bagry, bno2y
   real, dimension(ialloc2)                :: bvbsby, bvcsby, bo2y, bphy, bcay, bmwy, blfy, bably, bnl0y, bpl0y, bgspy
   real, dimension(ialloc2)                :: bgsny, bcmy, bbacy, bchlky, bchlgy, bdakiy, bdaaky, bsedky, bazoky, bkmory
   real, dimension(ialloc2)                :: bkigry, bkbcmy, biry, bsisdy, bkmuay, bftkay, bfikay, bfhkay
   real, dimension(ialloc2)                :: bkray, btpkiy, btpgry, btpbly, bdagry, bdaagy, bsedgy, bazogy, bgmory
   real, dimension(ialloc2)                :: badrky, badrgy, bacoky, bacogy, bgmuay, bfigay, bfhgay, bgray, bzooiy
   real, dimension(ialloc2)                :: bfibay, bantby, bextky, bdably, bdaaby, bsedby, bazoby, bbmory, badrby
   real, dimension(ialloc2)                :: bacoby, bbmuay, bfhbay, bbray, bchlby, bfln3y, bbetny, bjno3y, bjnh4y
   real, dimension(ialloc2)                :: bjpo4y, bjsiy, bjo2y, bcoliy, volfco, algcok, algcog, algcky, algcgy
   real, dimension(ialloc2)                :: bgszny, bglzny, bgscady, bglcady, bgscuy, bglcuy, bgsniy, bglniy
   real, dimension(ialloc2)                :: bgsasy, bglasy, bgspby, bglpby, bgscry, bglcry, bgsfey, bglfey
   real, dimension(ialloc2)                :: bgshgy, bglhgy, bgsmny, bglmny, bgsuy, bgluy, bsseros
   real, dimension(ialloc2)                :: bjdoc1, bjdoc2, btracer, abegm2, abekm2, coroi, corois
   real, dimension(ialloc2)                :: jdoc1, jdoc2, sgwmue, dh2de, saett, sseros
   real, dimension(ialloc2,2)              :: idras, idrasy, dreiy, dreisy, gwdrly, drmas, drmasy, drakr, drakry
   real, dimension(ialloc2,2)              :: drbar, drbary, drmor, drmory
   real, dimension(ialloc2,5)              :: coro, coros
   real, dimension(2,ialloc2)              :: bcdy, bcpy
   real, dimension(ialloc5,ialloc2)        :: tempwz, tempzy, vnh4zy, vno2zy, vno3zy, vo2zy, gelpzy, sizy, chlazy
   real, dimension(ialloc5,ialloc2)        :: akizy, agrzy, ablzy, dtemp, vnh4z, vno2z, vno3z, vo2z, gelpz, siz
   real, dimension(ialloc5,ialloc2)        :: akiz, agrz, ablz, chlaz, agrbrz, akibrz, ablbrz, algakz, algagz
   real, dimension(ialloc5,ialloc2)        :: algabz, algzkz, algzgz, algzbz, uvert, dalgkz, dalgbz, dalggz
   real, dimension(ialloc5,ialloc2)        :: cchlakzy,cchlabzy,cchlagzy
   real, dimension(ialloc5,ialloc2)        :: up_nkz, up_pkz, up_siz, up_n2z, up_ngz, up_pgz, up_nbz, up_pbz
   real, dimension(:,:),  allocatable      :: tausc, m_eros, n_eros, sedroh, aeros, eeros, dsedh, zwdsedh ,btausc
   real, dimension(:),    allocatable      :: t1e,m1e,n1e,r1e
   real, dimension(:),    allocatable      :: strdt, fzeit, ho2_z, hte_z, hph_z, wsp_uw, wsp_ow, wehrh, wehrb
   real, dimension(:),    allocatable      :: qstrang_1, startkm, endkm
   real, dimension(:,:),  allocatable      :: ywlage, wlage, ymax, ymin, vmq, hmq, boeamq, segkm, clado
   real, dimension(:,:,:), allocatable     :: hclado, bclado, hidras, hdrmas, hdrakr, hdrbar, hrzuwd, hdrmor
   real, dimension(:,:,:), allocatable     :: scd, scp
   real, dimension(:,:), allocatable       :: hsusn, hbettn, hdon, hagnh4, haknh4, habnh4, halno3, hsedx0, hsusno
   real, dimension(:,:), allocatable       :: hsedag, hsedak, hsedab, halgzg, halgzk, halgzb, halgdg, halgdk
   real, dimension(:,:), allocatable       :: halgdb, halgcg, halgck, halgcb, habowg, habowk, hvolfd, hdrpfe
   real, dimension(:,:), allocatable       :: haborg, habork, hdalgg, hdalgk, hdalgb, hdalag, hdalak, hdalab, hdgmor
   real, dimension(:,:), allocatable       :: hdkmor, hdbmor, hsgo2n, hsdbsb, hbsbt, hdalgo, hdalao
   real, dimension(:,:), allocatable       :: hsedom, hbedgs, hsedvvert, hdkorn, dkorn, hbsbbe, hoein1, hro2dr, hzoro2, hpo2p
   real, dimension(:,:), allocatable       :: hpo2r, hiras, hrmuas, hrakr, hrbar, hkmuea, hgmuea, hbmuea, hftaau
   real, dimension(:,:), allocatable       :: hfiaus, hfigau, hfibau, hfheau, hfhega, hfheba, hakrau, hagrau, habrau
   real, dimension(:,:), allocatable       :: hschlr, hdz2d
   real, dimension(:,:), allocatable       :: hhnfmu, hhnfre, hhnfup, hhnfmo, hhnfex, hhnfdr, hhnfza, hbamua
   real, dimension(:,:), allocatable       :: dlalph, dlbeta, dlgamm, hdlarn, midlan, mxdlan
   real, dimension(:,:), allocatable       :: zdrei, hpfl, zdrel, zdresl, gewdr, hgewdr, vtyp, rzuwdr, rzuwdy
   real, dimension(:,:), allocatable       :: zdreis, cd, cp, migsp, mxgsp, migsn, mxgsn, miaki, mxaki, miagr, mxagr
   integer                                 :: ilamda
   real, dimension(40)                     :: eta, aw, ack, acg, acb, ah, as, al 
   real, dimension(:,:), allocatable       :: extk_lamda, hsised, hskmor, mxtemp, mitemp, mxb5, mib5, mxcs, mics, mxnh4
   real, dimension(:,:), allocatable       :: minh4, mxchla,  michla, mxo2, mio2, mizo, mxzo, misi, mxsi, mivph, mxvph
   real, dimension(:,:), allocatable       :: micoli, mxcoli, mica, mxca, mimw, mxmw, mivno3, mxvno3, migp, mxgp, mxvno2
   real, dimension(:,:), allocatable       :: mivno2, milf, mxlf, miabl, mxabl, miss, mxss, sumte, sumb5, sumcs, sumn4
   real, dimension(:,:), allocatable       :: migszn, mxgszn, miglzn, mxglzn, migscad, mxgscad, miglcad, mxglcad
   real, dimension(:,:), allocatable       :: migscu, mxgscu, miglcu, mxglcu, migsni, mxgsni, miglni, mxglni
   real, dimension(:,:), allocatable       :: migsas, mxgsas, miglas, mxglas, migspb, mxgspb, miglpb, mxglpb
   real, dimension(:,:), allocatable       :: migscr, mxgscr, miglcr, mxglcr, migsfe, mxgsfe, miglfe, mxglfe
   real, dimension(:,:), allocatable       :: migshg, mxgshg, miglhg, mxglhg, migsmn, mxgsmn, miglmn, mxglmn
   real, dimension(:,:), allocatable       :: migsu, mxgsu, miglu, mxglu
   real, dimension(:,:), allocatable       :: sumgszn, sumglzn, sumgscad, sumglcad, sumgscu, sumglcu, sumgsni, sumglni
   real, dimension(:,:), allocatable       :: sumgsas, sumglas, sumgspb, sumglpb, sumgscr, sumglcr, sumgsfe, sumglfe
   real, dimension(:,:), allocatable       :: sumgshg, sumglhg, sumgsmn, sumglmn, sumgsu, sumglu
   real, dimension(:,:), allocatable       :: sumsi, scm, sbac, schnf, sbvhnf, sumcak, sumcag, sumcab, summw, sumlf
   real, dimension(:,:), allocatable       :: sumca, sumo2, sumzo, sumss, sumpfl, sumbal, sgsp, sgsn, scoli, sumvph
   real, dimension(:,:), allocatable       :: sumno3, sumgp, szooro, sumno2, svkigr, santbl, sumabl, snaehr
   real, dimension(:,:), allocatable       :: sabmua, svx02, sumaki, sumagr, zwcd, zwcp, zwo2z, zwgpz, zwakiz, zwcors
   real, dimension(:,:), allocatable       :: zwcoro, akmb, ekmb, dlb, zwagrz, zwablz, zwchlz, tau2b, alphab, pomzb
   real, dimension(:,:), allocatable       :: zwtez, sedalg_mq, sedss_mq, svx0, cdy, cpy, orgcsd, orgcsd_abb
   real, dimension(:,:), allocatable       :: summsl, sumcal, sumdln, scorig, scoisg, ssedal, ssedx0, sdon, sflun3
   real, dimension(:,:), allocatable       :: ssusn, sbettn, salgzo, salgn, salno3, ssusno, salgdr, salmor, salgco
   real, dimension(:,:), allocatable       :: svoldr, sdrpfe, sabeow, sabeor, sdalg, sdalga, sblmor, ssgo2n, ssdbsb
   real, dimension(:,:), allocatable       :: ssoein, ssalgo, s2algo, sbsbt, sschlr, sbsbbe, s2algao, so2phy, sro2dr
   real, dimension(:,:), allocatable       :: spo2p, spo2r, sir, srmue, srakr, srbar, sffood, sfik, sfig, sfib, sakmua
   real, dimension(:,:), allocatable       :: sagmua, sfheka, sfhega, sfheba, sakrau, sagrea, sabrea, shnfmu, shnfre
   real, dimension(:,:), allocatable       :: shnfup, shnfmo, shnfex, shnfdr, shnfz, sbacmu, shnfba, snl0, spl0, sjno3
   real, dimension(:,:), allocatable       :: sjnh4, sjpo4, sjsi, sjo2
   real, dimension(:,:), allocatable       :: sumcchlk, sumcchlg, sumcchlb
   real, dimension(:,:), allocatable       :: bh, bf, vbm, bvmq, bhmq, bw2, w2b, bsedom, bdkorn, sedomb, dkornb, w2, hw2
   real, dimension(:,:), allocatable       :: btempw, btsed, bso, blb, bleb, bno3, bnh4, bgelp, bsvhek, bgesn, bgesp
   real, dimension(:,:), allocatable       :: bsvheg, bagbcm, bchla, bir, bssalg, bsi, bdaki, bdaak, bsedak, bazok
   real, dimension(:,:), allocatable       :: bdkmor, bvkigr, bakbcm, baki, bagr, bsised,bskmor, bfheau, bpfl, bakmua
   real, dimension(:,:), allocatable       :: bftaau, bfiaus, bakrau, bbsbt, bschlr, bbsb, bcsb, bo2, bno2, bx0, bchlak
   real, dimension(:,:), allocatable       :: bchlag, babrz1, bss, bzooi, bmw, bpw, bvcsb, bca, blf, bph, bvbsb, babewk
   real, dimension(:,:), allocatable       :: bdlarn, bx02, bstind, bdagr, bdaag, bsedag, bazog, bdgmor, babewg, baberg
   real, dimension(:,:), allocatable       :: baberk, bresdr, badrk, badrg, bacok, bacog, bacob, badrb, bagmua, bfigas
   real, dimension(:,:), allocatable       :: bfhgau, bagrau, babszo, bzres1, bzres2, bzexki, bzexgr, brmuas, bzexbl
   real, dimension(:,:), allocatable       :: biras, brakr, brbar, bfssgr, bfbsgr, bfrfgr, bexdvk, bexdvg, bsgon, bsedx0
   real, dimension(:,:), allocatable       :: bexdvb, bdon, bsusn, bbettn, bsuso, bagn4, bakn4, bagn3, babn4, babn3
   real, dimension(:,:), allocatable       :: bdalgo, bdalgao,babeowg, babeowk, babeorg, babeork, bzooro2, bo2ein, bo2ein1, balgo
   real, dimension(:,:), allocatable       :: bsusn2, bpfln1, bpfln2
   real, dimension(:,:), allocatable       :: bakn3, bsedn, bbvhnf, bsdbsb, bbsbbe, bdfaek, bdfaeg, bdfaeb, bdfaes
   real, dimension(:,:), allocatable       :: bssdr, borgcs, borgcs_abb, bbsbct, bbsbcp, bcm, bbac, bhnfbs, bbsbhn
   real, dimension(:,:), allocatable       :: bchnf, bnl0, bpl0, bgo2n, bpo2p, bpo2r, bro2dr, bro2hf, borgss, bjno3, bjn2
   real, dimension(:,:), allocatable       :: bjnh4, bjsi, bjpo4, bjo2, bsedss, babbcm, babl, bchlab, bantbl, bsvheb
   real, dimension(:,:), allocatable       :: btpki, btpgr, bextk, bq_pk, bq_nk, bq_sk, bq_pg, bq_ng, bq_pb
   real, dimension(:,:), allocatable       :: bq_nb, bflun3, bdabl, bdaab, bsedab, bazob, bdbmor, babmua, bfibas, bfhbau
   real, dimension(:,:), allocatable       :: babrau, btpbl, bup_pb, bup_nb, babtbr, balgbz, balabz, bup_pk, bup_nk
   real, dimension(:,:), allocatable       :: bup_si, baktbr, bup_pg, bup_ng, bagtbr, balgkz, balakz, balggz, balagz
   real, dimension(:,:), allocatable       :: bkn4z, bkn3z, bgn4z, bgn3z, bbn4z, bbn3z, bsedalg_mq, bsedss_mq, btgzoo
   real, dimension(:,:), allocatable       :: bste, bsno3, bsn4, bsgelp, bsno2, bschla, bsssal, bssi, bszooi, bsvbsb
   real, dimension(:,:), allocatable       :: bsvcsb, bsgsp, bsgsn, bsaki, bsagr, bsabl, bsfln3, bso2, bsmw, bslf
   real, dimension(:,:), allocatable       :: bsca, bsph, bsnl0, bspl0, bsdalg, bsvkg, bsdaa, bsseda,bsalgz, bsamor
   real, dimension(:,:), allocatable       :: bsadr, bsalco, bsfik, bsfig, bskmue, bsgmue, bshek, bsheg, bskre
   real, dimension(:,:), allocatable       :: bsgre, bschlk, bschlg, bsbmue, bsheb, bsbre, bschlb, bsantb, bsbetn
   real, dimension(:,:), allocatable       :: bsjno3, bsjnh4, bsjpo4, bsjo2, bsjsi, bscoli
   real, dimension(:,:), allocatable       :: bsgszn, bsglzn, bsgscad, bsglcad, bsgscu, bsglcu, bsgsni, bsglni
   real, dimension(:,:), allocatable       :: bsgsas, bsglas, bsgspb, bsglpb, bsgscr, bsglcr, bsgsfe, bsglfe
   real, dimension(:,:), allocatable       :: bsgshg, bsglhg, bsgsmn, bsglmn, bsgsu, bsglu
   real, dimension(:,:), allocatable       :: bmxtem, bmitem, bmxno3, bmino3, bmxnh4, bminh4, bmxglp, bmiglp, bmxchl
   real, dimension(:,:), allocatable       :: bmichl, bmxssa, bmissa, bmxsi, bmisi, bmxzoo, bmizoo, bmxno2, bmino2
   real, dimension(:,:), allocatable       :: bmibsb, bmxbsb, bmicsb, bmxcsb, bmxgsp, bmigsp, bmxgsn, bmigsn, bmxaki
   real, dimension(:,:), allocatable       :: bmiaki, bmxagr, bmiagr, bmio2, bmxo2, bmxmw, bmimw, bmxlf, bmilf
   real, dimension(:,:), allocatable       :: bmxca, bmica, bmxph, bmiph, bnaehr, bcoli, bdoscf, bakmor_1, bagmor_1, babmor_1
   real, dimension(:,:), allocatable       :: bmxgszn, bmigszn, bmxglzn, bmiglzn, bmxgscad, bmigscad, bmxglcad, bmiglcad
   real, dimension(:,:), allocatable       :: bmxgscu, bmigscu, bmxglcu, bmiglcu, bmxgsni, bmigsni, bmxglni, bmiglni
   real, dimension(:,:), allocatable       :: bmxgsas, bmigsas, bmxglas, bmiglas, bmxgspb, bmigspb, bmxglpb, bmiglpb
   real, dimension(:,:), allocatable       :: bmxgscr, bmigscr, bmxglcr, bmiglcr, bmxgsfe, bmigsfe, bmxglfe, bmiglfe
   real, dimension(:,:), allocatable       :: bmxgshg, bmigshg, bmxglhg, bmiglhg, bmxgsmn, bmigsmn, bmxglmn, bmiglmn
   real, dimension(:,:), allocatable       :: bmxgsu, bmigsu, bmxglu, bmiglu
   real, dimension(:,:), allocatable       :: bgszn, bglzn, bgscad, bglcad, bgscu, bglcu, bgsni, bglni
   real, dimension(:,:), allocatable       :: bgsas, bglas, bgspb, bglpb, bgscr, bglcr, bgsfe, bglfe
   real, dimension(:,:), allocatable       :: bgshg, bglhg, bgsmn, bglmn, bgsu, bglu
   real, dimension(:,:), allocatable       :: bznsed,bcadsed,bcused,bnised,bassed,bpbsed
   real, dimension(:,:), allocatable       :: bcrsed,bfesed,bhgsed,bmnsed,bused
   real, dimension(:,:), allocatable       :: hfkm, hqaus, hsvhk, hsvhg, hdoscf, hsvhb, habbcm, habl, hchlab, hantbl
   real, dimension(:,:), allocatable       :: htempw, htsed, hbsb, hcsb, hnh4, hcm, hbac, ho2, hno3, hno2, hx0, hsi
   real, dimension(:,:), allocatable       :: hx02, hcoli, hchla, hchlak, hchlag, hvkigr, htpki, htpgr, htpbl, hzooi
   real, dimension(:,:), allocatable       :: habrz1, hssalg, hss, hgelp, hmw, hpw, hca, hlf, hph, hvbsb, hvcsb, haki
   real, dimension(:,:), allocatable       :: hstind, hagr, hakbcm, hagbcm, hchnf, hbvhnf, hhnfba, hfssgr, hfbsgr, hnl0
   real, dimension(:,:), allocatable       :: hq_nk, hq_pk, hq_sk, hq_ng, hq_pg, hq_nb, hq_pb, hpl0, hfrfgr, hffood
   real, dimension(:,:), allocatable       :: hdl, htau2, hgesp, hgesn, hcd1, hcd2, hcp1, hcp2, hvo2, hextk, hjno3
   real, dimension(:,:), allocatable       :: hjnh4, hjpo4, hjsi, hjo2, hflun3,hjn2, tgzoo, akmor_1, agmor_1, abmor_1
   integer, dimension(:,:), allocatable    :: anzzeit, banzzeit, zwanzzeit
   real, dimension(:,:), allocatable       :: hglzn, hgszn, hglcad, hgscad, hglcu, hgscu, hglni, hgsni
   real, dimension(:,:), allocatable       :: hglas, hgsas, hglpb, hgspb, hglcr, hgscr, hglfe, hgsfe
   real, dimension(:,:), allocatable       :: hglhg, hgshg, hglmn, hgsmn, hglu, hgsu
   real, dimension(:,:), allocatable       :: hsseros,hsedalk,hsedalg,hsedalb,hsedss
   real, dimension(:,:), allocatable       :: znsed,cadsed,cused,nised,assed
   real, dimension(:,:), allocatable       :: pbsed,crsed,fesed,hgsed,mnsed,used
   real, dimension(:,:), allocatable       :: apfl, epfl, pflmxs, pflmis, aschif, eschif, awett, ewett, abal, ebal
   real, dimension(:,:), allocatable       :: ggbal, gkbal, akdrei, ekdrei, acoro, ecoro
   real, dimension(:,:), allocatable       :: coro1s, aksed, eksed, spewksx, wuebkx, psrefsx, extkx, coross, aveg, eveg
   real, dimension(:,:), allocatable       :: valtal, edufal, valtar, edufar
   real, dimension(:,:), allocatable       :: sedom, bedgsed, sedvvert, spewksus, wuebkus, psrefsus, spewkss, wuebks, psrefss
   real, dimension(:,:), allocatable       :: extkus, extks, stakm, raua, bsohla, hlboea, hflaea, htiefa, hvf, hws
   real, dimension(:,:), allocatable       :: helen, hvmitt, htiefe, hrau, hrhyd, hflae, hpfmnl, hpfmxl, habgml
   real, dimension(:,:), allocatable       :: hlboem, hbsohl, hvabfl, valtlh, eduflh, valtrh, edufrh, habkml
   real, dimension(:,:), allocatable       :: hdlmx, hdlmxs, hgwdmx, hsgwmu
   real, dimension(:,:), allocatable       :: hdh2de, hmax2d
   real, dimension(:,:), allocatable       :: rbkm, rbkmle, rbkm1, wirkll, abfls, obsbs, ocsbs, vnh4s, vno2s
   real, dimension(:,:), allocatable       :: vno3s, gesns, vx0s, vx02s, gelps, gesps, sis, chlas, vkigrs, antbls
   real, dimension(:,:), allocatable       :: zooins, vphs, mws, pws, cas, lfs, ssalgs, tempws, vo2s, chnfs, bvhnfs
   real, dimension(:,:), allocatable       :: colis, waers, akis, agrs, abls, agbcms, akbcms, abbcms, frfgrs,doscfs
   real, dimension(:,:), allocatable       :: cms, bacs, nl0s, pl0s, sss, chlaks, chlabs, chlags, vbsbs, vcsbs
   real, dimension(:,:), allocatable       :: q_nks, q_pks, q_sks, q_ngs, q_pgs, q_nbs, q_pbs
   real, dimension(:,:), allocatable       :: glzns, gszns, glcads, gscads, glcus, gscus, glnis, gsnis
   real, dimension(:,:), allocatable       :: glass, gsass, glpbs, gspbs, glcrs, gscrs, glfes, gsfes
   real, dimension(:,:), allocatable       :: glhgs, gshgs, glmns, gsmns, glus, gsus
   real, dimension(:,:), allocatable       :: einlkh, qeinlh, ebsbh, ecsbh, enh4h, ex0h, eo2h, etemph, echlah
   real, dimension(:,:), allocatable       :: ezindh, egph, esih, eno3h, essh, ewaerh, enl0h, epl0h, ephh, emwh
   real, dimension(:,:), allocatable       :: elfh, ecah, ex02h, eno2h, echnfh, ebvhnh, egesnh, egesph, ecolih
   real, dimension(:,:), allocatable       :: evkgh, eantbh, ecm, ebac
   real, dimension(:,:), allocatable       :: egszn, eglzn, egscad, eglcad, egscu, eglcu, egsni, eglni
   real, dimension(:,:), allocatable       :: egsas, eglas, egspb, eglpb, egscr, eglcr, egsfe, eglfe
   real, dimension(:,:), allocatable       :: egshg, eglhg, egsmn, eglmn, egsu, eglu
   real, dimension(:,:), allocatable       :: qlh, bsblh, csblh, enh4lh, eno2lh, eno3lh, gesnlh, x0lh, x02lh
   real, dimension(:,:), allocatable       :: gplh, gesplh, silh, phlh, calh, elflh, sslh, templh, o2lh, colilh
   real, dimension(:,:), allocatable       :: enl0lh, pl0lh, chlalh, cml, bacl
   real, dimension(:,:), allocatable       :: afkm2d, efkm2d
   real, dimension(:,:), allocatable       :: ho2z_z, htez_z, hchlaz_z, hakiz_z, hagrz_z, hablz_z, hnh4z_z, hno2z_z
   real, dimension(:,:), allocatable       :: hno3z_z, hpz_z, hsiz_z, hchlkz_z, hchlgz_z, hchlbz_z, hgespz_z, hgesnz_z
   real, dimension(:,:), allocatable       :: hq_nkz_z, hq_nbz_z, hq_ngz_z, hcchlkz_z, hcchlbz_z, hcchlgz_z
   real, dimension(:,:,:), allocatable     :: bcd, bcp, hcd, hcp, cdl, cpl, zdrs, zdrss, gwdrs, vtypa
   real, dimension(:,:,:), allocatable     :: sidras, sdrmas, sdrakr, sdrbar, sdrmor, szdrg, szdrsg, sgwdrg, wstand
   real, dimension(:,:,:), allocatable     :: hzdrel, hzdrsl, hgwdrl, vtyph
   real, dimension(:,:,:), allocatable     :: cds, cps, ecd, ecp, hcoro2, hcos2
   real, dimension(:,:,:), allocatable     :: hnh4z, hno2z, hno3z, ho2z, hgelpz, hgespz, hgesnz, hsiz
   real, dimension(:,:,:), allocatable     :: hakiz, hagrz, hablz, hchlaz, hchlkz, hchlgz, hchlbz, htempz
   real, dimension(:,:,:), allocatable     :: hq_nkz, hq_nbz, hq_ngz, hcchlkz, hcchlbz, hcchlgz
   real, dimension(:,:,:), allocatable     :: tzt, o2zt, nh4zt, no2zt, no3zt, pzt, gsizt, akizt, agrzt, ablzt
   real, dimension(:,:,:), allocatable     :: chlazt, chlkzt, chlgzt, chlbzt, gespzt, gesnzt, q_nkzt, q_nbzt, q_ngzt
   real, dimension(:,:,:), allocatable     :: cchlkzt, cchlbzt, cchlgzt
   character (len = 8)                     :: versionstext, dummy
   
   ! --- settings ---
   linux = .false.
   kontroll = .false.
   mitsedflux = .false.    ! sediment fluxes switched off temporarily
   write_csv_output = .false. ! should simulation results be writting in special csv-files? (usefull for debugging)
   
   ! --- get arguments ---
   call get_paths(linux)
      
   ! ==========================================================================
   ! DEFINITION RUN
   ! writing parameter definition for GUI GERRIS
   ! ==========================================================================
   if (cpfad == '/F') then
      call write_gerris_definitions(cpfad1)
      stop
   endif
   
   ! ==========================================================================
   ! MAIN RUN
   ! ==========================================================================
   ! print header
   call version_string(versionstext)
   print *, repeat('*', 78)
   print *, '*', repeat(' ', 76), '*'
   print *, '*', repeat(' ', 29), 'QSim1D [', versionstext, ']',  repeat(' ',30), '*'
   print *, '*', repeat(' ', 76), '*'
   print *, repeat('*', 78)
   
   ! print paths
   print*, 'cPfad:'
   print*, '   ', trim(cpfad)
   print*, 'cPfad1:'
   print*, '   ', trim(cpfad1)
   
   
   ! Vorbelegungen
   maus = 0
   iend = 0
   iwied = 0
   ilang = 0
   ilbuhn = 0
   jlauf = 0
   jtag = 0
   dH2D = 0.25
   iergeb = 0
   itracer_vor = 0
   
   nndr = 2 ! Anzahl der Kohorten
   
   cmin = 'Minimum'
   cmax = 'Maximum'
   cpoint = '.'
   
  
   ! --------------------------------------------------------------------------
   ! reading from ModellA.txt (unit 10)
   ! --------------------------------------------------------------------------
   write(pfadstring,'(2A)')trim(adjustl(cpfad)),'MODELLA.txt'
   open(unit = 10, file = pfadstring, iostat = open_error)
   if (open_error /= 0) call qerror("Could not open ModellA.txt")
   rewind (10)
   jStr = 0
   read(10,'(2a)')ckenn_vers
   if (ckenn_vers /= '*V') then
   else
      rewind(10)
      read(10,'(a)') dummy !VERSIO
      read(10,'(a2)')chcon
   endif
   read(10,'(f5.2,2x,f5.2)')GeoB,GeoL
   read(10,'(I5)')azstr_read
   
   ! set number of stretches
   call set_azstrs(azstr_read)
   
   ! ==========================================================================
   !  allocieren der Variablen
   ! ==========================================================================

   nazStrs = 2 * azStrs
   allocate(hanze(azStrs), ianze(azStrs), STRiz(azStrs),isub_dt(azStrs),imac(azStrs),isub_dt_Mac(azStrs), mstr_ist(azStrs*2))
   allocate(strNr(nazStrs), mstra(azStrs), ieinsh(azStrs), ieinLs(azStrs), nbuhn(azStrs), iFlRi(nazStrs), isegs(azStrs))
   allocate(STRID(azStrs), janzWt(azStrs), janzWs(azStrs), jlwo2(azStrs), iRB_K1(azStrs), ho2_z(azStrs))
   allocate(hte_z(azStrs), izufluss(azStrs), hph_z(azStrs), iFlRi_l(nazStrs), imRB_K1(ialloc1))
   allocate(strname(azStrs),strnumm(azStrs))
   allocate(mPfs(azStrs), mSs(azStrs), mDs(azStrs), mCs(azStrs), mBs(azStrs), mUs(azStrs))
   allocate(mWes(azStrs), mVs(azStrs), mZs(azStrs), mAs(azStrs), itsts(azStrs), msts(azStrs), itmaxs(azStrs))
   allocate(mEs(azStrs))
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
   allocate(tausc(azStrs,ialloc2), M_eros(azStrs,ialloc2), n_eros(azStrs,ialloc2), sedroh(azStrs,ialloc2) ,btausc(azStrs,ialloc2) )
   allocate(dsedH(azStrs,ialloc2), zwdsedH(azStrs,ialloc2) )
   allocate(t1e(ialloc2), m1e(ialloc2), n1e(ialloc2), r1e(ialloc2) )
   allocate(Ymin(azStrs,ialloc4), vmq(azStrs,ialloc2), Hmq(azStrs,ialloc2), boeamq(azStrs,ialloc2))
   allocate(segkm(azStrs,ialloc2), clado(10,ialloc2), hClado(azStrs,5,ialloc2), bclado(azStrs,5,ialloc2))
   allocate(hidras(azStrs,ialloc2,2), hdrmas(azStrs,ialloc2,2), hdrakr(azStrs,ialloc2,2), hdrbar(azStrs,ialloc2,2))
   allocate(hRzuwd(azStrs,ialloc2,2), hdrmor(azStrs,ialloc2,2), sCD(azStrs,2,ialloc2))
   allocate(sCP(azStrs,2,ialloc2), hsusn(azStrs,ialloc2), hbettN(azStrs,ialloc2), hdon(azStrs,ialloc2))
   allocate(hagnh4(azStrs,ialloc2), haknh4(azStrs,ialloc2), habnh4(azStrs,ialloc2), halNO3(azStrs,ialloc2))
   allocate(hsedx0(azStrs,ialloc2), hsusno(azStrs,ialloc2), hsedag(azStrs,ialloc2), hsedak(azStrs,ialloc2))
   allocate(hsedab(azStrs,ialloc2), halgzg(azStrs,ialloc2), halgzk(azStrs,ialloc2), halgzb(azStrs,ialloc2))
   allocate(halgdg(azStrs,ialloc2), halgdk(azStrs,ialloc2), halgdb(azStrs,ialloc2), halgcg(azStrs,ialloc2))
   allocate(halgck(azStrs,ialloc2), halgcb(azStrs,ialloc2), habowg(azStrs,ialloc2), habowk(azStrs,ialloc2))
   allocate(hvolfd(azStrs,ialloc2), hdrpfe(azStrs,ialloc2),haborg(azStrs,ialloc2), habork(azStrs,ialloc2))
   allocate(hdalgg(azStrs,ialloc2), hdalgk(azStrs,ialloc2), hdalgb(azStrs,ialloc2), hdalag(azStrs,ialloc2))
   allocate(hdalak(azStrs,ialloc2), hdalab(azStrs,ialloc2), hdgmor(azStrs,ialloc2))
   allocate(hdkmor(azStrs,ialloc2), hdbmor(azStrs,ialloc2), hsgo2n(azStrs,ialloc2), hsdbsb(azStrs,ialloc2))
   allocate(hbsbt(azStrs,ialloc2), hdalgo(azStrs,ialloc2), hdalao(azStrs,ialloc2))
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
   allocate(dlalph(azStrs,ialloc2))
   allocate(dlbeta(azStrs,ialloc2), dlgamm(azStrs,ialloc2), hdlarn(azStrs,ialloc2), midlan(azStrs,ialloc2))
   allocate(mxdlan(azStrs,ialloc2), zdrei(ialloc2,2), hpfl(azStrs,ialloc2), zdrel(ialloc2,4))
   allocate(zdresl(ialloc2,4), gewdr(ialloc2,4), hgewdr(ialloc2,4), VTYP(ialloc2,14), Rzuwdr(ialloc2,4))
   allocate(Rzuwdy(ialloc2,4), zdreis(ialloc2,4), CD(2,ialloc2), CP(2,ialloc2), migsP(azStrs,ialloc2))
   allocate(mxgsP(azStrs,ialloc2), migsN(azStrs,ialloc2), mxgsN(azStrs,ialloc2), miaki(azStrs,ialloc2))
   allocate(mxaki(azStrs,ialloc2), miagr(azStrs,ialloc2), mxagr(azStrs,ialloc2), extk_lamda(40,ialloc2))
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
   allocate(migsAs(azStrs,ialloc2), mxgsAs(azStrs,ialloc2), miglAs(azStrs,ialloc2), mxglAs(azStrs,ialloc2))
   allocate(migsPb(azStrs,ialloc2), mxgsPb(azStrs,ialloc2), miglPb(azStrs,ialloc2), mxglPb(azStrs,ialloc2))
   allocate(migsCr(azStrs,ialloc2), mxgsCr(azStrs,ialloc2), miglCr(azStrs,ialloc2), mxglCr(azStrs,ialloc2))
   allocate(migsFe(azStrs,ialloc2), mxgsFe(azStrs,ialloc2), miglFe(azStrs,ialloc2), mxglFe(azStrs,ialloc2))
   allocate(migsHg(azStrs,ialloc2), mxgsHg(azStrs,ialloc2), miglHg(azStrs,ialloc2), mxglHg(azStrs,ialloc2))
   allocate(migsMn(azStrs,ialloc2), mxgsMn(azStrs,ialloc2), miglMn(azStrs,ialloc2), mxglMn(azStrs,ialloc2))
   allocate(migsU(azStrs,ialloc2), mxgsU(azStrs,ialloc2), miglU(azStrs,ialloc2), mxglU(azStrs,ialloc2))
   
   allocate(sumgsZn(azStrs,ialloc2), sumglZn(azStrs,ialloc2), sumgsCad(azStrs,ialloc2), sumglCad(azStrs,ialloc2))
   allocate(sumgsCu(azStrs,ialloc2), sumglCu(azStrs,ialloc2), sumgsNi(azStrs,ialloc2), sumglNi(azStrs,ialloc2))
   allocate(sumgsAs(azStrs,ialloc2), sumglAs(azStrs,ialloc2), sumgsPb(azStrs,ialloc2), sumglPb(azStrs,ialloc2))
   allocate(sumgsCr(azStrs,ialloc2), sumglCr(azStrs,ialloc2), sumgsFe(azStrs,ialloc2), sumglFe(azStrs,ialloc2))
   allocate(sumgsHg(azStrs,ialloc2), sumglHg(azStrs,ialloc2), sumgsMn(azStrs,ialloc2), sumglMn(azStrs,ialloc2))
   allocate(sumgsU(azStrs,ialloc2), sumglU(azStrs,ialloc2))
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
   allocate(bdalgo(azStrs,ialloc2), bdalgao(azStrs,ialloc2), babeowg(azStrs,ialloc2))
   allocate(babeowk(azStrs,ialloc2), balgo(azStrs,ialloc2))
   allocate(babeorg(azStrs,ialloc2), babeork(azStrs,ialloc2), bzooro2(azStrs,ialloc2))
   allocate(bo2ein(azStrs,ialloc2), bo2ein1(azStrs,ialloc2))
   allocate(bsusn2(azstrs,ialloc2), bpfln1(azstrs,ialloc2), bpfln2(azstrs,ialloc2))
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
   allocate(bgsAs(azStrs,ialloc2), bglAs(azStrs,ialloc2), bgsPb(azStrs,ialloc2), bglPb(azStrs,ialloc2))
   allocate(bgsCr(azStrs,ialloc2), bglCr(azStrs,ialloc2), bgsFe(azStrs,ialloc2), bglFe(azStrs,ialloc2))
   allocate(bgsHg(azStrs,ialloc2), bglHg(azStrs,ialloc2), bgsMn(azStrs,ialloc2), bglMn(azStrs,ialloc2))
   allocate(bgsU(azStrs,ialloc2), bglU(azStrs,ialloc2))
   allocate(bZnSed(azStrs,ialloc2),bCadSed(azStrs,ialloc2),bCuSed(azStrs,ialloc2),bNiSed(azStrs,ialloc2))
   allocate(bAsSed(azStrs,ialloc2),bPbSed(azStrs,ialloc2),bCrSed(azStrs,ialloc2),bFeSed(azStrs,ialloc2))
   allocate(bHgSed(azStrs,ialloc2),bMnSed(azStrs,ialloc2),bUSed(azStrs,ialloc2))
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
   allocate(bsJSi(azStrs,ialloc2), bscoli(azStrs,ialloc2))
   allocate(bsgsZn(azStrs,ialloc2), bsglZn(azStrs,ialloc2), bsgsCad(azStrs,ialloc2), bsglCad(azStrs,ialloc2))
   allocate(bsgsCu(azStrs,ialloc2), bsglCu(azStrs,ialloc2), bsgsNi(azStrs,ialloc2), bsglNi(azStrs,ialloc2))
   allocate(bsgsAs(azStrs,ialloc2), bsglAs(azStrs,ialloc2), bsgsPb(azStrs,ialloc2), bsglPb(azStrs,ialloc2))
   allocate(bsgsCr(azStrs,ialloc2), bsglCr(azStrs,ialloc2), bsgsFe(azStrs,ialloc2), bsglFe(azStrs,ialloc2))
   allocate(bsgsHg(azStrs,ialloc2), bsglHg(azStrs,ialloc2), bsgsMn(azStrs,ialloc2), bsglMn(azStrs,ialloc2))
   allocate(bsgsU(azStrs,ialloc2), bsglU(azStrs,ialloc2))
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
   allocate(bmxgsAs(azStrs,ialloc2), bmigsAs(azStrs,ialloc2), bmxglAs(azStrs,ialloc2), bmiglAs(azStrs,ialloc2))
   allocate(bmxgsPb(azStrs,ialloc2), bmigsPb(azStrs,ialloc2), bmxglPb(azStrs,ialloc2), bmiglPb(azStrs,ialloc2))
   allocate(bmxgsCr(azStrs,ialloc2), bmigsCr(azStrs,ialloc2), bmxglCr(azStrs,ialloc2), bmiglCr(azStrs,ialloc2))
   allocate(bmxgsFe(azStrs,ialloc2), bmigsFe(azStrs,ialloc2), bmxglFe(azStrs,ialloc2), bmiglFe(azStrs,ialloc2))
   allocate(bmxgsHg(azStrs,ialloc2), bmigsHg(azStrs,ialloc2), bmxglHg(azStrs,ialloc2), bmiglHg(azStrs,ialloc2))
   allocate(bmxgsMn(azStrs,ialloc2), bmigsMn(azStrs,ialloc2), bmxglMn(azStrs,ialloc2), bmiglMn(azStrs,ialloc2))
   allocate(bmxgsU(azStrs,ialloc2), bmigsU(azStrs,ialloc2), bmxglU(azStrs,ialloc2), bmiglU(azStrs,ialloc2))
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
   allocate(anzZeit(azStrs,ialloc2), banzZeit(azStrs,ialloc2), zwanzZeit(azStrs,ialloc2))
   allocate(hglZn(azStrs,ialloc2), hgsZn(azStrs,ialloc2), hglCad(azStrs,ialloc2), hgsCad(azStrs,ialloc2))
   allocate(hglCu(azStrs,ialloc2), hgsCu(azStrs,ialloc2), hglNi(azStrs,ialloc2), hgsNi(azStrs,ialloc2))
   allocate(hglAs(azStrs,ialloc2), hgsAs(azStrs,ialloc2), hglPb(azStrs,ialloc2), hgsPb(azStrs,ialloc2))
   allocate(hglCr(azStrs,ialloc2), hgsCr(azStrs,ialloc2), hglFe(azStrs,ialloc2), hgsFe(azStrs,ialloc2))
   allocate(hglHg(azStrs,ialloc2), hgsHg(azStrs,ialloc2), hglMn(azStrs,ialloc2), hgsMn(azStrs,ialloc2))
   allocate(hglU(azStrs,ialloc2), hgsU(azStrs,ialloc2))
   allocate(hSSeros(azStrs,ialloc2),hsedalk(azStrs,ialloc2),hsedalg(azStrs,ialloc2),hsedalb(azStrs,ialloc2),hsedss(azStrs,ialloc2))
   allocate(ZnSed(azStrs,ialloc2),CadSed(azStrs,ialloc2),CuSed(azStrs,ialloc2),NiSed(azStrs,ialloc2))
   allocate(AsSed(azStrs,ialloc2),PbSed(azStrs,ialloc2),CrSed(azStrs,ialloc2),FeSed(azStrs,ialloc2))
   allocate(HgSed(azStrs,ialloc2),MnSed(azStrs,ialloc2),USed(azStrs,ialloc2))
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
   allocate(glAss(azStrs,ialloc1), gsAss(azStrs,ialloc1), glPbs(azStrs,ialloc1), gsPbs(azStrs,ialloc1))
   allocate(glCrs(azStrs,ialloc1), gsCrs(azStrs,ialloc1), glFes(azStrs,ialloc1), gsFes(azStrs,ialloc1))
   allocate(glHgs(azStrs,ialloc1), gsHgs(azStrs,ialloc1), glMns(azStrs,ialloc1), gsMns(azStrs,ialloc1))
   allocate(glUs(azStrs,ialloc1), gsUs(azStrs,ialloc1))
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
   allocate(egsAs(azStrs,ialloc1), eglAs(azStrs,ialloc1), egsPb(azStrs,ialloc1), eglPb(azStrs,ialloc1))
   allocate(egsCr(azStrs,ialloc1), eglCr(azStrs,ialloc1), egsFe(azStrs,ialloc1), eglFe(azStrs,ialloc1))
   allocate(egsHg(azStrs,ialloc1), eglHg(azStrs,ialloc1), egsMn(azStrs,ialloc1), eglMn(azStrs,ialloc1))
   allocate(egsU(azStrs,ialloc1), eglU(azStrs,ialloc1))
   allocate(qLh(azStrs,ialloc1), bsbLh(azStrs,ialloc1), csbLh(azStrs,ialloc1), enh4Lh(azStrs,ialloc1))
   allocate(eno2Lh(azStrs,ialloc1), eno3Lh(azStrs,ialloc1), gesNLh(azStrs,ialloc1), x0Lh(azStrs,ialloc1))
   allocate(x02Lh(azStrs,ialloc1), gpLh(azStrs,ialloc1), gesPLh(azStrs,ialloc1), siLh(azStrs,ialloc1))
   allocate(phLh(azStrs,ialloc1), caLh(azStrs,ialloc1), elfLh(azStrs,ialloc1), ssLh(azStrs,ialloc1))
   allocate(tempLh(azStrs,ialloc1), o2Lh(azStrs,ialloc1), coliLh(azStrs,ialloc1), enl0Lh(azStrs,ialloc1))
   allocate(pl0Lh(azStrs,ialloc1), chlaLh(azStrs,ialloc1), CML(azStrs,ialloc1), BACL(azStrs,ialloc1))
   allocate(afkm2D(azStrs,ialloc1),efkm2D(azStrs,ialloc1))
   allocate(apfl(azStrs,ialloc3), epfl(azStrs,ialloc3), pflmxs(azStrs,ialloc3), pflmis(azStrs,ialloc3))
   allocate(aschif(azStrs,ialloc3),eschif(azStrs,ialloc3), awett(azStrs,ialloc3), ewett(azStrs,ialloc3))
   allocate(abal(azStrs,ialloc3), ebal(azStrs,ialloc3),ggbal(azStrs,ialloc3), gkbal(azStrs,ialloc3))
   allocate(akdrei(azStrs,ialloc3), ekdrei(azStrs,ialloc3))
   allocate(acoro(azStrs,ialloc3))
   allocate(ecoro(azStrs,ialloc3), WUEBKx(azStrs,ialloc3), extkx(azStrs,ialloc3), VALTAL(azStrs,ialloc3))
   allocate(coro1s(azStrs,ialloc3), aKSED(azStrs,ialloc3), eKSED(azStrs,ialloc3), SPEWKSx(azStrs,ialloc3))
   allocate(PSREFSx(azStrs,ialloc3), coross(azStrs,ialloc3), aVEG(azStrs,ialloc3), eVEG(azStrs,ialloc3))
   allocate(EDUFAL(azStrs,ialloc3), VALTAR(azStrs,ialloc3), EDUFAR(azStrs,ialloc3))
   allocate(aEros(azStrs,ialloc3),eEros(azStrs,ialloc3))
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
   ! ==========================================================================
   ! ==========================================================================
   
   ! Setzen der Ymax/Ymin-Werte zur grafischen Darstellung unter Gerris
   Ymax = 0.0
   Ymin = 999999.99
   
   
   ! --------------------------------------------------------------------------
   ! reading from ModellA.txt (unit 10)
   ! --------------------------------------------------------------------------
   print *, ''
   print *, repeat('=', 78)
   print *, repeat(' ', 33), 'ModellA.txt'
   print *, repeat('=', 78)
   
   isumAnzSta = 0
   do azStr = 1,azStrs ! Beginn Strangschleife
      ieinL = 0
      ! Strangheader
      read(10,*) mstr,            & ! laufende Nummer des Strangs
                 mStas(mstr),     & ! Anzahl Knoten
                 mRBs(mstr),      & ! Anzahl Randbedingungen
                 mwehr(mstr),     & ! Anzahl Wehre
                 STRID(mstr),     & ! veraltet
                 strnumm(mstr),   & ! Gerris-ID des Strangs
                 strname(mstr)      ! Name des Strangs
      mstra(azStr) = mstr
      isumAnzSta = isumAnzSta+mStas(mstr)
      do mSta = 1,mStas(mstr)
         read(10,1001) Stakm(mstr,mSta),     &
                       Raua(mstr,mSta),      &
                       bsohla(mstr,mSta),    &
                       boeamq(mstr,mSta),    &
                       vmq(mstr,mSta),       &
                       Hmq(mstr,mSta),       &
                       bvmq(mstr,mSta),      &
                       bHmq(mstr,mSta)
      enddo
      
      if (mwehr(mstr) > 0) read(10,1002) wehrh(mstr), wehrb(mstr)
      
      startkm(mstr) = Stakm(mstr,1)
      endkm(mstr) = Stakm(mstr,mStas(mstr))
      iRB_K1(mstr) = 0
      izufluss(mstr) = 0
      do mRB = 1,mRBs(mstr)
         read(10,1003) RBNR,              &
                       RBkm(mstr,mRB),    &
                       RBtyp(mstr,mRB),   &
                       Weinl(mstr,mRB),   &
                       mstrLe(mstr,mRB),  &
                       RBkmLe(mstr,mRB),  &
                       cEName(mstr,mRB)
         
         if (RBkm(mstr,mRB) == Stakm(mstr,1) .and. mstrLe(mstr,mRB) < 0) then
            RBtyp(mstr,mRB) = 0
            iRB_K1(mstr) = iRB_K1(mstr) + 1
            imRB_K1(iRB_K1(mstr)) = mRB
         endif
         
         izufluss(mstr) = izufluss(mstr) + 1
         
         ! Bestimmung der Wirklänge der diffusen Einleitung
         if (mstrLe(mstr,mRB) >= 0) then
            ieinL = ieinL+1
            WirkLL(mstr,ieinL) = abs(RBkm(mstr,mRB)-RBkmLe(mstr,mRB))*1000.
         endif
      enddo
      
      if (startkm(mstr) > endkm(mstr)) then 
         abfr(mstr) = 0
      else
         abfr(mstr) = 1
      endif
      
   enddo ! Ende Strangschleife
   1001 format(f8.3,2x,f5.2,2x,f7.2,2x,f7.2,2x,f8.5,2x,f7.4,2x,f8.5,2x,f7.4)
   1002 format(f7.2,2x,f7.2)
   1003 format(I5,2x,f8.3,2x,i1,2x,i1,2x,I5,2x,F8.3,6x,50a)
   close (10)
   
   
   ! print ModellA to console
   print "('  #     section  start    end      sta  weir  bnd  name                         ')"
   print "('  ----  -------  -------  -------  ---  ----  ---  ---------------------------- ')"
  
   do mstr = 1,azStrs 
      print "('  ',i4,'  ', a7,'  ', f7.2,'  ',f7.2,'  ',i3,'  ',i4,'  ', i3,'  ', a28)", &
         mstr, trim(strnumm(mstr)), startkm(mstr), endkm(mstr), mStas(mstr),              &
         mwehr(mstr), mRBs(mstr), trim(strname(mstr))
   enddo
   
   ! --------------------------------------------------------------------------
   ! Ermittlung der Berechnungsgitterpunkte
   ! --------------------------------------------------------------------------
   call km_sys(mstra,StaKm,RBkm,RBkmLe,RBtyp,mRBs             &
               ,mWehr,mStas,iorLah,iorLeh,mstrLe,abfr,cpfad)
   
   pfadstring = trim(adjustl(cpfad)) // 'km_sys.dat'
   open(unit = 391, file = pfadstring, iostat = open_error)
   rewind (391)
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      read(391,'(I5,2x,I5)')mstr,isegs(mstr)
      do iseg = 1,isegs(mstr)
         read(391,'(f9.4,2x,i1)')segkm(mstr,iseg),hflag(mstr,iseg)
      enddo
   enddo
   
   close (391)
   
   ! --------------------------------------------------------------------------
   ! reading from EreigG.txt (unit 92) 
   ! --------------------------------------------------------------------------
   call read_ereigg_settings()
   
   if (itracer == 1) then
      mtracer = 0
   else
      mtracer = 1
   endif
   
   itags  = itag_start
   monats = monat_start
   jahrs  = jahr_start
   uhrs   = uhr_start
   ! --------------------------------------------------------------------------
   ! reading parameters from AParam
   ! -------------------------------------------------------------------------
   if (iwsim == 4 .or. iwsim == 5)  goto 329
   if (iwsim == 2 .and. icoli == 0) goto 329
   call aparam_lesen(cpfad, iwsim, icoli, ieros, ischwer)
   
   ! --------------------------------------------------------------------------
   ! reading from e_extnct.dat
   ! --------------------------------------------------------------------------
   ! jetzt ausserhalb der Algenroutinen und der Zeitschleife
   call e_extnct_lesen(ilamda,eta,aw,ack,acg,acb,ah,as,al,cpfad)
   
   
   329 continue
   if (uhrs <= 0.0)uhrz = 0.0
   if (uhrs > 0.0)uhrz = Uhrs
   
   ! Umrechnen der Uhrzeit in Dezimalschreibweise hcUhrz
   hcmin = (Uhrz-int(Uhrz))*100./60.
   hcUhrz = int(uhrz)+hcmin
   Uhrz = hcUhrz
   ij = 1
   
   
   ! -------------------------------------------------------------------------
   ! Erstellung des Gitters für ortsfeste Kenngrößen und Organismen
   ! Lesen aus der Datei MODELLG.txt
   ! -------------------------------------------------------------------------
   dlmax1 = 0.0
   dlmax2 = 0.0
   dlmax3 = 0.0
   dlmax4 = 0.0
   coro1 = 0.0
   
   print *, ''
   print *, repeat('=', 78)
   print *, repeat(' ', 33), 'ModellG.txt'
   print *, repeat('=', 78)
   
   pfadstring = trim(adjustl(cpfad)) // 'MODELLG.txt'
   open(unit = 103, file = pfadstring, iostat = open_error)
   if (open_error /= 0) call qerror ("Could not open ModellG.txt")
   rewind(103)

   ! file header
   read(103,'(A2)')ckenn_vers1
   if (ckenn_vers1 == '*V') read(103,'(2x)')
   read(103,2305) lait1, laim1, laid1
   read(103,*)
   read(103,*)
   
   do while (.true.)
      
      ! stretch header
      read(103,'(a1,2x,I5)',iostat = read_error) ckenn, mstr
      if (read_error /= 0) exit 
      nbuhn(mstr) = 0
      
      ! initialise counters
      mPf = 0
      mS  = 0
      mD  = 0
      mC  = 0
      mB  = 0
      mU  = 0
      mWe = 0
      mV  = 0
      mZ  = 0
      mA  = 0
      mE  = 0
      
      ! read data
      do while (.true.)
         read(103,"(a201)",iostat = read_error) ctext
         if (read_error /= 0) exit
         
         ! identifier
         ckenn = ctext(1:1)
         
         select case(ckenn)
            case(' ') ! end of data block
               exit
               
            case('L') ! Laichperiode
               read(ctext,2306)laits(mstr),laims(mstr),laids(mstr)
            
            case('M') ! macrophytes
               read(ctext,1031)itsts(mstr),msts(mstr),itmaxs(mstr),mmaxs(mstr),itends(mstr),mends(mstr)
            
            case('P') ! macrophytes
               mPf = mPf+1
               read(ctext,1032)apfl(mstr,mPf),epfl(mstr,mPf),Pflmis(mstr,mPf),Pflmxs(mstr,mPf)
            
            case('F') ! shipping
               mS = mS+1
               read(ctext,1033)aschif(mstr,mS),eschif(mstr,mS)
            
            case('D') ! dreissena
               mD = mD+1
               read(ctext,1034)akdrei(mstr,mD),ekdrei(mstr,mD)                      &
                  ,(zdrs(mstr,mD,ndr),zdrss(mstr,mD,ndr)                            &
                  ,gwdrs(mstr,mD,ndr),ndr = 1,nndr)
            
            case('C') ! corophium
               call qerror ("You are trying to run a simulation with corophium. &
                           & This is currently not supported by QSim.")
               ! mC = mC+1
               ! read(ctext,1035)acoro(mstr,mC),ecoro(mstr,mC),coro1s(mstr,mC),coross(mstr,mC)
            
            case('B') ! benthic algae
               call qerror ("You are trying to run a simulation with benthic algae. &
                           & This is currently not supported by QSim.")
               ! mB = mB+1
               ! read(ctext,1036)abal(mstr,mB),ebal(mstr,mB),ggbal(mstr,mB),gkbal(mstr,mB)
            
            case('V') ! 2D
               call qerror ("You are trying to run a 2D simulation. &
                           & This is not supported by QSim anymore. &
                           & Please use an older version.")
               
            case('U') ! groyne field
               nbuhn(mstr) = 1
               mU = mU+1
               read(ctext,1038)akmB(mstr,mU),ekmB(mstr,mU),DlB(mstr,mU),tau2B(mstr,mU),alphaB(mstr,mU),POMzb(mstr,mU)
            
            case('T') ! wetter station
               mWe = mWe+1
               read(ctext,1033)aWett(mstr,mWe),eWett(mstr,mWe),ikWSta(mstr,mWe),YWlage(mstr,mWe)
            
            case('O') ! vegetation
               mV = mV+1
               read(ctext,1040)aVeg(mstr,mV),eVeg(mstr,mV),(VTYPA(mstr,mV,iV)   &
                 ,iV = 1,6),VALTAL(mstr,mV),EDUFAL(mstr,mV)                     &
                 ,(VTYPA(mstr,mV,iV),iV = 7,12),VALTAR(mstr,mV),EDUFAR(mstr,mV) &
                 ,(VTYPA(mstr,mV,iV),iV = 13,14)
            
            case('S') ! sediment temperature
               mA = mA+1
               read(ctext,1047)aKSED(mstr,mA),eKSED(mstr,mA),SPEWKSx(mstr,mA),WUEBKx(mstr,mA),PSREFSx(mstr,mA),extkx(mstr,mA)
            
            case('E') ! erosion
               mE = mE+1
               if (mE > ialloc3) then
                  write(message,*) 'mE > ialloc3 zu viele ',mE,' Abschnitte in Strang ',mstr
                  call qerror(message)
               endif
            
               read(ctext,*,iostat = open_error) aEros(mstr,mE), eEros(mstr,mE), tausc(mstr,mE),  &
                                                 M_eros(mstr,mE), n_eros(mstr,mE), sedroh(mstr,mE)
               if (open_error /= 0) call qerror("read error erosion parameters")
               print*,ieros,mstr,mE,' E ModellG tau,M,n,roh = ',tausc(mstr,mE),M_eros(mstr,mE),n_eros(mstr,mE),sedroh(mstr,mE)
            
            case default
               call qerror("Unkown identifier in ModellG: " // ckenn)
         end select
         
      enddo
      
      
      mPfs(mstr) = mPf
      mSs(mstr)  = mS
      mDs(mstr)  = mD
      mCs(mstr)  = mC
      mBs(mstr)  = mB
      mUs(mstr)  = mU
      mWes(mstr) = mWe
      mVs(mstr)  = mV
      mZs(mstr)  = mZ
      mAs(mstr)  = mA
      mEs(mstr)  = mE
   
   enddo
   
   1031 format(1x,6(2x,i2))
   1032 format(3x,f8.3,2x,f8.3,2x,f7.2,2x,f7.2)
   1033 format(3x,f8.3,2x,f8.3,2x,I4,2x,F6.2,2x,F7.1)
   1034 format(3x,f8.3,2x,f8.3,2(2x,f7.2,2x,f7.2,2x,f7.3))
   1035 format(3x,f8.3,2x,f8.3,2x,f8.1,2x,f8.1)
   1036 format(3x,f8.3,2x,f8.3,2x,f7.1,2x,f7.1)
   1037 format(3x,f8.3,2x,f8.3)
   1038 format(3x,f8.3,2x,f8.3,2x,f7.2,2x,f7.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2)
   2305 format(i2,2x,i2,2x,i3)
   2306 format(3x,i2,2x,i2,2x,i3)
   1040 format(3x,f8.3,2x,f8.3,18(2x,f6.2))
   1045 format(3x,f8.3,2x,f8.3,2x,f6.2,2x,f5.2,2x,f9.4)
   1047 format(3x,f8.3,2x,f8.3,2x,f6.2,2x,f7.2,2x,f5.2,2x,f5.2)
   
   jjj   = 0
   vtyph = 0.0
   
   
   ! Erosions-Abschnitte
   do azStr = 1,azStrs ! alle Stränge
      mstr = mstra(azStr)
      mE = mEs(mstr)
      ! umspeichern
      t1e(1:mE) = tausc(mstr,1:mE)
      m1e(1:mE) = M_eros(mstr,1:mE)
      n1e(1:mE) = n_eros(mstr,1:mE)
      r1e(1:mE) = sedroh(mstr,1:mE)
      ! initialisieren = keine Erosion
      tausc(mstr,:) = 99999.99
      M_eros(mstr,:) = 0.0
      n_eros(mstr,:) = 1.0
      sedroh(mstr,:) = 2650.0
      do j = 1,mE ! alle Erosionsabschnitte im Strang
         do mSta = 1,mStas(mstr) ! alle Profile/Stationen im Strang
            fkmgit = Stakm(mstr,mSta)
            ! wenn aktueller km im Abschnitt
            if (aEros(mstr,mE) <= fkmgit .and. eEros(mstr,mE) >= fkmgit) then
               tausc(mstr,mSta) = t1e(j)
               M_eros(mstr,mSta) = m1e(j)
               n_eros(mstr,mSta) = n1e(j)
               sedroh(mstr,mSta) = r1e(j)
               print*,mstr,mSta,' Erosion an km = ',fkmgit
            endif
         enddo  ! alle Profile im Strang
      enddo  ! alle Erosionsabschnitte im Strang
   enddo    ! alle Stränge
   
   
   do azStr = 1,azStrs
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
      mE = 1
      
      do kSta = 0,isegs(mstr)
         if (kSta /= 0) then
            fkmgit = segkm(mstr,kSta)
            mSta = mSta+1
         endif
         trpmin = 0.0
         trpmax = 0.0
         if (mPfs(mstr) == 0 .or. mPf > mPfs(mstr))goto 140
         if (abfr(mstr) == 1)goto 761
         
         if (fkmgit <= apfl(mstr,mPf) .and. fkmgit >= epfl(mstr,mPf)) then
            trpmin = pflmis(mstr,mPf)
            trpmax = pflmxs(mstr,mPf)
            goto 140
         endif
         
         if (fkmgit <= epfl(mstr,mPf) .and. fkmgit <= apfl(mstr,mPf+1)) then
            567 continue
            mPf = mPf+1
            if (mPf > mPfs(mstr))goto 140
            if (epfl(mstr,mPf) > fkmgit .and. mPf < mPfs(mstr))goto 567
            trpmin = pflmis(mstr,mPf)
            trpmax = pflmxs(mstr,mPf)
            goto 140
         endif
         goto 140
         
         761 continue
         if (fkmgit >= apfl(mstr,mPf) .and. fkmgit <= epfl(mstr,mPf)) then
            trpmin = pflmis(mstr,mPf)
            trpmax = pflmxs(mstr,mPf)
            goto 140
         endif
         
         if (fkmgit >= epfl(mstr,mPf) .and. fkmgit >= apfl(mstr,mPf+1)) then
            568 continue
            mPf = mPf+1
            if (mPf > mPfs(mstr))goto 140
            if (epfl(mstr,mPf) < fkmgit .and. mPf < mPfs(mstr))goto 568
            trpmin = pflmis(mstr,mPf)
            trpmax = pflmxs(mstr,mPf)
            goto 140
         endif
         goto 140
         
         ! Elemente mit benthischen Algen
         140 continue
         
         tggbal = 0.0
         tgkbal = 0.0
         if (mBs(mstr) == 0 .or. mB > mBs(mstr))goto 58
         if (abfr(mstr) == 1)goto 573
         if (fkmgit <= abal(mstr,mB) .and. fkmgit >= ebal(mstr,mB)) then
            tggbal = ggbal(mstr,mB)
            tgkbal = gkbal(mstr,mB)
            goto 58
         endif
         if (fkmgit <= ebal(mstr,mB) .and. fkmgit <= abal(mstr,mB+1)) then
            97 continue
            mB = mB+1
            if (mB > mBs(mstr))goto 58
            if (ebal(mstr,mB) > fkmgit .and. mB < mBs(mstr))goto 97
            tggbal = ggbal(mstr,mB)
            tgkbal = gkbal(mstr,mB)
            goto 58
         endif
         goto 58
         
         573 continue
         if (fkmgit >= abal(mstr,mB) .and. fkmgit <= ebal(mstr,mB)) then
            tggbal = ggbal(mstr,mB)
            tgkbal = gkbal(mstr,mB)
            goto 58
         endif
         if (fkmgit >= ebal(mstr,mB) .and. fkmgit >= abal(mstr,mB+1)) then
            98 continue
            mB = mB+1
            if (mB > mBs(mstr))goto 58
            if (ebal(mstr,mB) < fkmgit .and. mB < mBs(mstr))goto 98
            tggbal = ggbal(mstr,mB)
            tgkbal = gkbal(mstr,mB)
            goto 58
         endif
         
         ! Elemente mit Dreissena
         
         58 continue
         zdreie(1:nndr) = 0.0
         zdrese(1:nndr) = 0.0
         gwdre(1:nndr) = 0.0
         
         if (mDs(mstr) == 0 .or. mD > mDs(mstr))goto 59
         if (abfr(mstr) == 1)goto 663
         
         if (fkmgit <= akdrei(mstr,mD) .and. fkmgit >= ekdrei(mstr,mD)) then
            zdreie(1:nndr) = zdrs(mstr,mD,1:nndr)
            zdrese(1:nndr) = zdrss(mstr,mD,1:nndr)
            gwdre(1:nndr) = gwdrs(mstr,mD,1:nndr)
            goto 59
         endif
         
         if (fkmgit <= ekdrei(mstr,mD) .and. fkmgit <= akdrei(mstr,mD+1)) then
            196 continue
            mD = mD+1
            if (mD > mDs(mstr))goto 59
            if (ekdrei(mstr,mD) > fkmgit .and. mD < mDs(mstr))goto 196
            zdreie(1:nndr) = zdrs(mstr,mD,1:nndr)
            zdrese(1:nndr) = zdrss(mstr,mD,1:nndr)
            gwdre(1:nndr) = gwdrs(mstr,mD,1:nndr)
            goto 59
         endif
         goto 59
         
         663 continue
         if (fkmgit >= akdrei(mstr,mD) .and. fkmgit <= ekdrei(mstr,mD)) then
            zdreie(1:nndr) = zdrs(mstr,mD,1:nndr)
            zdrese(1:nndr) = zdrss(mstr,mD,1:nndr)
            gwdre(1:nndr) = gwdrs(mstr,mD,1:nndr)
            goto 59
         endif
         
         if (fkmgit >= ekdrei(mstr,mD) .and. fkmgit >= akdrei(mstr,mD+1)) then
            199 continue
            mD = mD+1
            if (mD > mDs(mstr))goto 59
            if (ekdrei(mstr,mD) < fkmgit .and. mD < mDs(mstr))goto 199
            zdreie(1:nndr) = zdrs(mstr,mD,1:nndr)
            zdrese(1:nndr) = zdrss(mstr,mD,1:nndr)
            gwdre(1:nndr) = gwdrs(mstr,mD,1:nndr)
         endif
         
         ! --- Elemente mit Chorophium ---
         59 continue
         coroe = 0.0
         corose = 0.0
         if (mCs(mstr) == 0 .or. mC > mCs(mstr))goto 143
         if (abfr(mstr) == 1)goto 763
         
         if (fkmgit <= acoro(mstr,mC) .and. fkmgit >= ecoro(mstr,mC)) then
            coroe = coro1s(mstr,mC)
            corose = coross(mstr,mC)
            if (coroe < 0.0)coroe = 0.0
            if (corose < 0.0)corose = 0.0
            goto 143
         endif
         
         if (fkmgit <= ecoro(mstr,mC) .and. fkmgit <= acoro(mstr,mC+1)) then
            296 continue
            mC = mC+1
            if (mC > mCs(mstr))goto 143
            if (ecoro(mstr,mC) > fkmgit .and. mC < mCs(mstr))goto 296
            coroe = coro1s(mstr,mC)
            corose = coross(mstr,mC)
            if (coroe < 0.0)coroe = 0.0
            if (corose < 0.0)corose = 0.0
            goto 143
         endif
         goto 143
         
         
         763 continue
         if (fkmgit >= acoro(mstr,mC) .and. fkmgit <= ecoro(mstr,mC)) then
            coroe = coro1s(mstr,mC)
            corose = coross(mstr,mC)
            if (coroe < 0.0)coroe = 0.0
            if (corose < 0.0)corose = 0.0
            goto 143
         endif
         
         if (fkmgit >= ecoro(mstr,mC) .and. fkmgit >= acoro(mstr,mC+1)) then
            299 mC = mC+1
            if (mC > mCs(mstr))goto 143
            if (ecoro(mstr,mC) < fkmgit .and. mC < mCs(mstr))goto 299
            coroe = coro1s(mstr,mC)
            corose = coross(mstr,mC)
            if (coroe < 0.0)coroe = 0.0
            if (corose < 0.0)corose = 0.0
         endif
         
         ! --- Elemente mit Uferbewuchs ---
         143 continue
         if (mVs(mstr) == 0 .or. mV > mVs(mstr))goto 848
         if (abfr(mstr) == 1)goto 840
         
         if (fkmgit <= aVEG(mstr,mV) .and. fkmgit >= eVEG(mstr,mV)) then
            VTYPH(mstr,mSta,1:14) = VTYPA(mstr,mV,1:14)
            VALTLH(mstr,mSta) = VALTAL(mstr,mV)
            EDUFLH(mstr,mSta) = EDUFAL(mstr,mV)
            VALTRH(mstr,mSta) = VALTAR(mstr,mV)
            EDUFRH(mstr,mSta) = EDUFAR(mstr,mV)
            goto 848
         endif
         
         if (fkmgit <= eVEG(mstr,mV) .and. fkmgit <= aVEG(mstr,mV+1)) then
            842 mV = mV+1
            if (mV > mVs(mstr))goto 848
            if (eVEG(mstr,mV) > fkmgit .and. mV < mVs(mstr))goto 842
            do iV = 1,14
               VTYPH(mstr,mSta,iV) = VTYPA(mstr,mV,iV)
            enddo
            VALTLH(mstr,mSta) = VALTAL(mstr,mV)
            EDUFLH(mstr,mSta) = EDUFAL(mstr,mV)
            VALTRH(mstr,mSta) = VALTAR(mstr,mV)
            EDUFRH(mstr,mSta) = EDUFAR(mstr,mV)
            goto 848
         endif
         goto 848
         
         840 continue
         if (fkmgit >= aVEG(mstr,mV) .and. fkmgit <= eVEG(mstr,mV)) then
            VTYPH(mstr,mSta,1:14) = VTYPA(mstr,mV,1:14)
            VALTLH(mstr,mSta) = VALTAL(mstr,mV)
            EDUFLH(mstr,mSta) = EDUFAL(mstr,mV)
            VALTRH(mstr,mSta) = VALTAR(mstr,mV)
            EDUFRH(mstr,mSta) = EDUFAR(mstr,mV)
            goto 848
         endif
         
         if (fkmgit >= eVEG(mstr,mV) .and. fkmgit >= aVEG(mstr,mV+1)) then
            845 continue
            mV = mV+1
            if (mV > mVs(mstr))goto 848
            if (eVEG(mstr,mV) < fkmgit .and. mV < mVs(mstr))goto 845
            VTYPH(mstr,mSta,1:14) = VTYPA(mstr,mV,1:14)
            VALTLH(mstr,mSta) = VALTAL(mstr,mV)
            EDUFLH(mstr,mSta) = EDUFAL(mstr,mV)
            VALTRH(mstr,mSta) = VALTAR(mstr,mV)
            EDUFRH(mstr,mSta) = EDUFAR(mstr,mV)
         endif
         
         848 continue
         ! organisches Material des Sediments
         POM_sed = -1.0
         BedGS = -1.0
         xsedvvertz = -1.0
         
         ! organisches Material des Sediments in Buhnenfelder
         POM_sedb = -1.0
         do mU = 1,mUs(mstr)
            if (abfr(mstr) == 0) then
               if (fkmgit <= akmb(mstr,mU) .and. fkmgit>=ekmb(mstr,mU)) then
                  POM_sedb = POMzb(mstr,mU)
                  exit
               else
                  cycle
               endif
            else
               if (fkmgit>=akmb(mstr,mU) .and. fkmgit <= ekmb(mstr,mU)) then
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
            if (abfr(mstr) == 0) then
               if (fkmgit <= aKSED(mstr,mA) .and. fkmgit>=eKSED(mstr,mA)) then
                  SPEWKSx1 = SPEWKSx(mstr,mA)
                  WUEBKx1 = WUEBKx(mstr,mA)
                  PSREFSx1 = PSREFSx(mstr,mA)
                  extkx1 = extkx(mstr,mA)
                  exit
               else
                  cycle
               endif
            else
               if (fkmgit>=aKSED(mstr,mA) .and. fkmgit <= eKSED(mstr,mA)) then
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
         
         ! Abschnittsweise Zuordnung der Wetterstationen
         if (mWe > mWes(mstr))goto 940
         if (abfr(mstr) == 1)goto 941
         
         if (fkmgit <= aWett(mstr,mWe) .and. fkmgit >= eWett(mstr,mWe)) then
            idWe(mstr,mSta) = ikWSta(mstr,mWe)
            Wlage(mstr,mSta) = yWlage(mstr,mWe)
            goto 940
         endif
         
         if (fkmgit <= eWett(mstr,mWe) .and. fkmgit <= aWett(mstr,mWe+1)) then
            942 continue
            mWe = mWe+1
            if (mWe > mWes(mstr))goto 940
            if (eWett(mstr,mWe) > fkmgit .and. mWe < mWes(mstr))goto 942
            idWe(mstr,mSta) = ikWSta(mstr,mWe)
            Wlage(mstr,mSta) = yWlage(mstr,mWe)
            goto 940
         endif
         goto 940
         
         941 continue
         if (fkmgit >= aWett(mstr,mWe) .and. fkmgit <= eWett(mstr,mWe)) then
            idWe(mstr,mSta) = ikWSta(mstr,mWe)
            Wlage(mstr,mSta) = yWlage(mstr,mWe)
            goto 940
         endif
         !
         if (fkmgit >= eWett(mstr,mWe) .and. fkmgit >= aWett(mstr,mWe+1)) then
            943 continue
            mWe = mWe+1
            if (mWe > mWes(mstr))goto 940
            if (eWett(mstr,mWe) < fkmgit .and. mWe < mWes(mstr))goto 943
            idWe(mstr,mSta) = ikWSta(mstr,mWe)
            Wlage(mstr,mSta) = yWlage(mstr,mWe)
         endif
         
         940 continue
         ! fkmgn = segkm(mstr,msta+1)
         hzdrel(mstr,mSta,1:nndr) = zdreie(1:nndr)
         hzdrsl(mstr,mSta,1:nndr) = zdrese(1:nndr)
         hgwdrl(mstr,mSta,1:nndr) = gwdre(1:nndr)
         
         hdlmx(mstr,mSta) = dlmax1
         hdlmxs(mstr,mSta) = dlmax2
         hgwdmx(mstr,mSta) = dlmax3
         hsgwmu(mstr,mSta) = dlmax4
         habgml(mstr,mSta) = tggbal
         habkml(mstr,mSta) = tgkbal
         if (trpmin < 0.0)trpmin = 0.0
         if (trpmax < 0.0)trpmax = 0.0
         hpfmnl(mstr,mSta) = trpmin
         hpfmxl(mstr,mSta) = trpmax
         hcoro2(mstr,mSta,1) = coroe
         hcos2(mstr,mSta,1) = corose
         
         hcoro2(mstr,mSta,2:5) = coro1
         hcos2(mstr,mSta,2:5) = coro1
         
         sedOM(mstr,kSta+1) = POM_sed  ! "flag6-Knoten wird in sysgen berücksichtigt.
         BedGSed(mstr,kSta+1) = BedGS
         sedvvert(mstr,kSta+1) = xsedvvertz
         sedOMb(mstr,kSta+1) = POM_sedb
         SPEWKSuS(mstr,kSta+1) = SPEWKSx1  ! Nomenklatur: spezifische Wärmekapazität an den ursprünglichen Stationen
         WUEBKuS(mstr,kSta+1) = WUEBKx1
         PSREFSuS(mstr,kSta+1) = PSREFSx1
         extkuS(mstr,kSta+1) = extkx1
         
         if (kSta == 0) cycle
         if (kSta > 0 .and. hflag(mstr,kSta) == 4) then     ! Berücksichtigung des "flag6"-Knotens
            mSta = mSta+1
            idWe(mstr,mSta) = ikWSta(mstr,mWe)
            Wlage(mstr,mSta) = yWlage(mstr,mWe)
            VTYPH(mstr,mSta,1:14) = VTYPA(mstr,mV,1:14)
            VALTLH(mstr,mSta) = VALTAL(mstr,mV)
            EDUFLH(mstr,mSta) = EDUFAL(mstr,mV)
            VALTRH(mstr,mSta) = VALTAR(mstr,mV)
            EDUFRH(mstr,mSta) = EDUFAR(mstr,mV)
            do ndr = 1,nndr
               hzdrel(mstr,mSta,ndr) = zdreie(ndr)
               hzdrsl(mstr,mSta,ndr) = zdrese(ndr)
               hgwdrl(mstr,mSta,ndr) = gwdre(ndr)
            enddo
            hdlmx(mstr,mSta) = dlmax1
            hdlmxs(mstr,mSta) = dlmax2
            hgwdmx(mstr,mSta) = dlmax3
            hsgwmu(mstr,mSta) = dlmax4
            habgml(mstr,mSta) = tggbal
            habkml(mstr,mSta) = tgkbal
            if (trpmin < 0.0)trpmin = 0.0
            if (trpmax < 0.0)trpmax = 0.0
            hpfmnl(mstr,mSta) = trpmin
            hpfmxl(mstr,mSta) = trpmax
            hcoro2(mstr,mSta,1) = coroe
            hcos2(mstr,mSta,1) = corose
            hcoro2(mstr,mSta,2:5) = coro1
            hcos2(mstr,mSta,2:5) = coro1
            
         endif
         
         ! fkmgit = fkmgn
         
      enddo
   enddo
   
   ! ==========================================================================
   ! Berechnung der Sedimentkenngrößen
   ! ==========================================================================
   if (iwsim /= 4 .and. iwsim /= 2 .and. iwsim /= 5) then
      jsed = 0
      call sediment(abfr, mStra, Stakm, mStas, mSs, aschif, eschif,            &
                    SedOM, SedOMb, dKorn, dKornb, raua, vmq, Hmq, nbuhn, bvmq, &
                    bHmq, jsed, w2, w2b,                                       &
                    kontroll, 0)
   endif
   
   
   ! ==========================================================================
   ! reading EREIGH.txt
   ! ==========================================================================
   pfadstring = trim(adjustl(cpfad1)) // 'EREIGH.txt'
   open(unit = 110, file = pfadstring, iostat = open_error)
   if (open_error /= 0) call qerror("Could not open EreigH.txt")
   
   rewind (110)
   read(110,'(A2)')ckenn_vers1
   if (ckenn_vers1 /= '*V') then
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
   
   ! Festlegung der max. Tiefenschichtenanzahl für jeden Ortspunkt bei 2D
   nkztot_max = 1
   
   
   ! =========================================================================
   ! initialize result files
   ! =========================================================================
   call init_result_files(cpfad, modell, cEreig, write_csv_output)
   
   ! ==========================================================================
   ! ABLAUF.txt vorbereiten
   ! ==========================================================================
   pfadstring = trim(adjustl(cpfad1)) // 'ABLAUF.txt'
   open(unit = 97, file = pfadstring, iostat = open_error)
   rewind (97)
   
   ! skip header
   read(97,'(A2)')ckenn_vers1
   if (ckenn_vers1 /= '*V') then
      read(97,'(A40)')ERENAME
   else
      read(97,'(A40)')MODNAME
      read(97,'(A40)')ERENAME
   endif
   
   ! Vorspulen auf Simulationsbeginn
   do while (.true.)
      read(97,9705,iostat = read_error)SCHRNR,jkenn,itags_Schr, monat_Schr, Jahr_Schr, Uhrz_Schr
      if (read_error /= 0) call qerror('Error while reading Ablauf.txt')
      
      if (jkenn /= 99) cycle
      
      if (itags == itags_Schr .and. monats == monat_Schr .and. &
          Jahrs == Jahr_Schr .and. uhrs-Uhrz_Schr <= 2.*epsilon(uhrs)) exit
      
   enddo
   9705 format(I5,2x,i2,2x,i2,2x,i2,2x,i4,2x,f5.2)
   
   ! ==========================================================================
   ! Initialisation of otherwise uninitialised variables before time loop
   ! ==========================================================================
   ! algae variables
   akmor_1 = 0.
   abmor_1 = 0.
   agmor_1 = 0.
   SKmor   = 0.
   hSKmor  = 0.
   
   if (any(nbuhn > 0)) then
      bakmor_1 = 0.
      babmor_1 = 0.
      bagmor_1 = 0.
   endif
   
   ! ==========================================================================
   9999 continue ! Rücksprunglabel Zeitschleife
   ! ==========================================================================
   
   !---------------------------------------------------------------------------
   ! read from Ablauf.txt
   !---------------------------------------------------------------------------
   do istr = 1,nazStrs
      read(97,9700,iostat = read_error) STRNR(istr), iFlRi_l(istr), (ESTRNR(istr,nstr),nstr = 1,azStrs)
      if (iFlRi_l(istr) == 99 .or. read_error < 0) exit
      
      ! TODO: ESTRNR has dimensions (nazStrs,azStrs) nStr may exceed limit of second dimension
      do nstr = 1,nazStrs
         if (ESTRNR(istr,nstr) == 0) then
            nstrs(istr) = nstr - 1
            nnstrs(StrNR(istr)) = nstrs(istr)
            exit
         endif
      enddo
   enddo
   
   9700 format(I5,2x,I2,500(2x,I5))
   
   istrs = istr-1
   
   ! Conversion factor of time step to hour
   hcUmt = 60./(tflie*1440.)
            
   ! Time step in seconds
   dt = tflie * 86400.
   
   ! --------------------------------------------------------------------------
   ! Einteilung der Flussstrecke in Segmente
   ! --------------------------------------------------------------------------
   call sysgen(ilang,dt,iwsim,nbuhn,akmB,ekmB,DLB,tau2B,alphaB,mUs                                  &
               ,aschif,eschif,mSs,mStra,raua,bsohla,boeamq,hlboea,hflaea,htiefa                     &
               ,hvF,hQaus,SedOM,BedGSed,sedvvert,dKorn,abfr,mStas,Startkm,mRBs,RBtyp,RBkm,ij        &
               ,tflie,STRdt,STRiz,cpfad,wsp_UW,WSP_OW                                               &
               ,SedOMb,w2,w2b,dKornb,SPEWKSuS,WUEBKuS,PSREFSuS,extkuS,SPEWKSS,WUEBKS,PSREFSS,extkS  &
               ,itags,monats,uhrz,ifhStr,fhprof,iverfahren,ianze_max,HMQ,bvMQ,bHMQ,ieros)
   
   pfadstring = trim(adjustl(cpfad)) // 'sysgenou'
   open(unit = 11, file = pfadstring, iostat = open_error)
   rewind (11)
   
   anzema = 0
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      
      read(11,1000)hanze(mstr)
      
      do istr = 1,istrs
         if (STRNR(istr) == mstr)anzema = anzema+int(FZeit(mstr))
      enddo
      
      do ior = 1,hanze(mstr)
         read(11,1010)hfkm(mstr,ior),hflag(mstr,ior),hjiein(mstr,ior),helen(mstr,ior),hvmitt(mstr,ior)              &
              ,htiefe(mstr,ior),hrau(mstr,ior),hrhyd(mstr,ior),hSedOM(mstr,ior),hw2(mstr,ior),hBedGS(mstr,ior)      &
              ,hsedvvert(mstr,ior),hdKorn(mstr,ior),hflae(mstr,ior),hWS(mstr,ior),hischf(mstr,ior),hlboem(mstr,ior) &
              ,hbsohl(mstr,ior),hvabfl(mstr,ior),bh(mstr,ior),bf(mstr,ior),bso(mstr,ior),blb(mstr,ior)              &
              ,bleb(mstr,ior),hdl(mstr,ior),htau2(mstr,ior),vbm(mstr,ior),bSedOM(mstr,ior),bw2(mstr,ior)            &
              ,bdKorn(mstr,ior),dlalph(mstr,ior)
         
         if (hrhyd(mstr,ior) <= 0.0)hrhyd(mstr,ior) = htiefe(mstr,ior)
      enddo
      
      QStrang_1(mstr) = hvabfl(mstr,1)
      
      hSedOM(mstr,hanze(mstr)+1) = hSedOM(mstr,hanze(mstr))
      bSedOM(mstr,hanze(mstr)+1) = bSedOM(mstr,hanze(mstr))
      
   enddo
   close (11)
   
   ! Berechnung der Anzahl der Zeitschritte
   if (ilang == 0) then
      hcon = izdt/60.
      hcontm = 24./hcon
      itimeh = nint(hcontm)
      itimeb = itimeh-1
      
      ! Anzahl der Zeitschritte für ersten Simulationstag (itimea)
      ! und letzen Simulationstag (itimee)
      ! Umrechnen der End-Uhrzeit in Dezimalschreibweise Uhren in hcUhre
      ! hcmin = (Uhren-int(Uhren))/0.6
      ! hcUhre = int(uhren)+hcmin
      hcontm = (24.-Uhrz)/hcon
      itimea = nint(hcontm)
      hcontm = (Uhren)/hcon
      itimee = nint(hcontm)+1
      ! if(itimee.eq.0)itimee = 1
      
      if (itags == itage .and. monats == monate.and.jahrs == jahre) then
         itimea = itimee
         itimeh = itimee
      endif
      
      itime = itimeb
   endif
   
   
   ! ==========================================================================
   ! Randbedingungen lesen
   ! ========================================================================== 
   ! zuerst werden die Anzahl der Randbedingungen und die maximale Länge der 
   ! Randbedingungs-Zeitreihen von EREIGG.txt gelesen
   if (iwied == 0) then 
      print *, ''
      print *, repeat('=', 78)
      print *, repeat(' ',34), 'boundaries'
      print *, repeat('=', 78)
      
      call randbedingungen(cpfad, i_Rands, iw_max)
   endif
   
   istr = 0
   ! dann werden die Zeitreihen selbst aus EREIGG.txt gelesen
   call funkstar(abfls,vbsbs,vcsbs,vnh4s,vno2s,vno3s,gesNs,vx0s,vx02s,gelps,gesPs,sis,chlas,vkigrs                        &
                 ,antbls,zooins,vphs,mws,cas,lfs,ssalgs,tempws,vo2s,CHNFs,BVHNFs,colis,DOSCFs,waers                       &
                 ,iColi, ischwer,glZns,gsZns,glCads,gsCads,glCus,gsCus,glNis,gsNis,glAss,gsAss,glPbs,gsPbs,glCrs,gsCrs    &
                 ,glFes,gsFes,glHgs,gsHgs,glMns,gsMns,glUs,gsUs                                                           &
                 ,c1Zn,e1Zn,c2Zn,e2Zn,c3Zn,e3Zn,c4Zn,e4Zn,c5Zn,e5Zn,VTKoeffDe_Zn                                          &
                 ,c1Cu,e1Cu,c2Cu,e2Cu,c3Cu,e3Cu,c4Cu,e4Cu,c5Cu,e5Cu,VTKoeffDe_Cu                                          &
                 ,c1Cad,e1Cad,c2Cad,e2Cad,c3Cad,e3Cad,c4Cad,e4Cad,c5Cad,e5Cad,VTKoeffDe_Cad                               &
                 ,c1Ni,e1Ni,c2Ni,e2Ni,c3Ni,e3Ni,c4Ni,e4Ni,c5Ni,e5Ni,VTKoeffDe_Ni                                          &
                 ,c1As,e1As,c2As,e2As,c3As,e3As,c4As,e4As,c5As,e5As,VTKoeffDe_As                                          &
                 ,c1Pb,e1Pb,c2Pb,e2Pb,c3Pb,e3Pb,c4Pb,e4Pb,c5Pb,e5Pb,VTKoeffDe_Pb                                          &
                 ,c1Cr,e1Cr,c2Cr,e2Cr,c3Cr,e3Cr,c4Cr,e4Cr,c5Cr,e5Cr,VTKoeffDe_Cr                                          &
                 ,c1Fe,e1Fe,c2Fe,e2Fe,c3Fe,e3Fe,c4Fe,e4Fe,c5Fe,e5Fe,VTKoeffDe_Fe                                          &
                 ,c1Hg,e1Hg,c2Hg,e2Hg,c3Hg,e3Hg,c4Hg,e4Hg,c5Hg,e5Hg,VTKoeffDe_Hg                                          &
                 ,c1Mn,e1Mn,c2Mn,e2Mn,c3Mn,e3Mn,c4Mn,e4Mn,c5Mn,e5Mn,VTKoeffDe_Mn                                          &
                 ,c1U,e1U,c2U,e2U,c3U,e3U,c4U,e4U,c5U,e5U,VTKoeffDe_U                                                     &
                 ,istund,uhrz,RBtyp,NRSCHr,itags,monats,jahrs,cpfad,iwsim,ilang,iwied,mstrRB,i_Rands                      &
                 ,iw_max,iformVert)
   
   ! Berücksichtigung von Einleitern am 1. Ortspunks eines Stranges mit Vorsträngen 1D-Fall
   do azStr = 1,azStrs !Strangschleife ANFANG
      mstr = mstra(azStr)
      if (iwied == 0) exit
      if (iRB_K1(mstr) <= 1) cycle
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
         ! abfls(mstr,imRB_K1(iRB)) = abfls(mstr,imRB_K1(iRB)))
         if (vbsbs(mstr,imRB_K1(iRB))>=0.0) then
            hc1 = hc1 + vbsbs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq1 = hcq1 + abfls(mstr,imRB_K1(iRB))
            i_K11 = 1
         endif
         if (vcsbs(mstr,imRB_K1(iRB))>=0.0) then
            hc2 = hc2 + vcsbs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq2 = hcq2 + abfls(mstr,imRB_K1(iRB))
            i_K12 = 1
         endif
         if (vNH4s(mstr,imRB_K1(iRB))>=0.0) then
            hc3 = hc3 + vNH4s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq3 = hcq3 + abfls(mstr,imRB_K1(iRB))
            i_K13 = 1
         endif
         if (vNO2s(mstr,imRB_K1(iRB))>=0.0) then
            hc4 = hc4 + vNO2s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq4 = hcq4 + abfls(mstr,imRB_K1(iRB))
            i_K14 = 1
         endif
         if (vNO3s(mstr,imRB_K1(iRB))>=0.0) then
            hc5 = hc5 + vNO3s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq5 = hcq5 + abfls(mstr,imRB_K1(iRB))
            i_K15 = 1
         endif
         if (gesNs(mstr,imRB_K1(iRB))>=0.0) then
            hc6 = hc6 + gesNs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq6 = hcq6 + abfls(mstr,imRB_K1(iRB))
            i_K16 = 1
         endif
         if (vx0s(mstr,imRB_K1(iRB))>=0.0) then
            hc7 = hc7 + vx0s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq7 = hcq7 + abfls(mstr,imRB_K1(iRB))
            i_K17 = 1
         endif
         if (vx02s(mstr,imRB_K1(iRB))>=0.0) then
            hc8 = hc8 + vx02s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq8 = hcq8 + abfls(mstr,imRB_K1(iRB))
            i_K18 = 1
         endif
         if (gelps(mstr,imRB_K1(iRB))>=0.0) then
            hc9 = hc9 + gelps(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq9 = hcq9 + abfls(mstr,imRB_K1(iRB))
            i_K19 = 1
         endif
         if (gesPs(mstr,imRB_K1(iRB))>=0.0) then
            hc10 = hc10 + gesPs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq10 = hcq10 + abfls(mstr,imRB_K1(iRB))
            i_K110 = 1
         endif
         if (Sis(mstr,imRB_K1(iRB))>=0.0) then
            hc11 = hc11 + Sis(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq11 = hcq11 + abfls(mstr,imRB_K1(iRB))
            i_K111 = 1
         endif
         if (chlas(mstr,imRB_K1(iRB))>=0.0) then
            hc12 = hc12 + chlas(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq12 = hcq12 + abfls(mstr,imRB_K1(iRB))
            i_K112 = 1
         endif
         if (vkigrs(mstr,imRB_K1(iRB))>=0.0) then
            hc13 = hc13 + vkigrs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq13 = hcq13 + abfls(mstr,imRB_K1(iRB))
            i_K113 = 1
         endif
         if (antbls(mstr,imRB_K1(iRB))>=0.0) then
            hc14 = hc14 + antbls(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq14 = hcq14 + abfls(mstr,imRB_K1(iRB))
            i_K114 = 1
         endif
         if (zooins(mstr,imRB_K1(iRB))>=0.0) then
            hc15 = hc15 + zooins(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq15 = hcq15 + abfls(mstr,imRB_K1(iRB))
            i_K115 = 1
         endif
         if (vphs(mstr,imRB_K1(iRB))>=0.0) then
            hc16 = hc16 + vphs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq16 = hcq16 + abfls(mstr,imRB_K1(iRB))
            i_K116 = 1
         endif
         if (mws(mstr,imRB_K1(iRB))>=0.0) then
            hc17 = hc17 + mws(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq17 = hcq17 + abfls(mstr,imRB_K1(iRB))
            i_K117 = 1
         endif
         if (cas(mstr,imRB_K1(iRB))>=0.0) then
            hc18 = hc18 + cas(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq18 = hcq18 + abfls(mstr,imRB_K1(iRB))
            i_K118 = 1
         endif
         if (lfs(mstr,imRB_K1(iRB))>=0.0) then
            hc19 = hc19 + lfs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq19 = hcq19 + abfls(mstr,imRB_K1(iRB))
            i_K119 = 1
         endif
         if (ssalgs(mstr,imRB_K1(iRB))>=0.0) then
            hc20 = hc20 + ssalgs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq20 = hcq20 + abfls(mstr,imRB_K1(iRB))
            i_K120 = 1
         endif
         if ((iwsim /= 4 .and. tempws(mstr,imRB_K1(iRB)) > -9.99) .or.       &
             (iwsim == 4 .and. tempws(mstr,imRB_K1(iRB)) >= 0.0 )) then
            hc21 = hc21 + tempws(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq21 = hcq21 + abfls(mstr,imRB_K1(iRB))
            i_K121 = 1
         endif
         if (vo2s(mstr,imRB_K1(iRB))>=0.0) then
            hc22 = hc22 + vo2s(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq22 = hcq22 + abfls(mstr,imRB_K1(iRB))
            i_K122 = 1
         endif
         if (CHNFs(mstr,imRB_K1(iRB))>=0.0) then
            hc23 = hc23 + CHNFs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq23 = hcq23 + abfls(mstr,imRB_K1(iRB))
            i_K123 = 1
         endif
         if (BVHNFs(mstr,imRB_K1(iRB))>=0.0) then
            hc24 = hc24 + BVHNFs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq24 = hcq24 + abfls(mstr,imRB_K1(iRB))
            i_K124 = 1
         endif
         if (Colis(mstr,imRB_K1(iRB))>=0.0) then
            hc25 = hc25 + Colis(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hc27 = hc27 + DOSCFs(mstr,imRB_K1(iRB))*abfls(mstr,imRB_K1(iRB))
            hcq25 = hcq25 + abfls(mstr,imRB_K1(iRB))
            i_K125 = 1
         endif
         i_K126 = 0
         if (waers(mstr,imRB_K1(iRB)) > -9999.9 .and. abfls(mstr,imRB_K1(iRB)) <= 2.e-10) then
            hcq26 = hcq26 + abfls(mstr,imRB_K1(iRB))
            hc26 = hc26 + waers(mstr,imRB_K1(iRB))/4.2/hcq26
            i_K126 = 1
         else if (waers(mstr,imRB_K1(iRB)) > -9999.9 .and. abfls(mstr,imRB_K1(iRB)) > 2.e-10) then
            hc26 = hc26 + waers(mstr,imRB_K1(iRB))/4.2/(hcq26 + abfls(mstr,imRB_K1(iRB)))
            hcq26 = hcq26 + abfls(mstr,imRB_K1(iRB))
            i_K126 = 1
         endif
      enddo
      do iRB = 1,iRB_K1(mstr)
         mRB = imRB_K1(iRB)
         if (i_K11 > 0)vbsbs(mstr,mRB) = hc1/hcq1
         if (i_K12 > 0)vcsbs(mstr,mRB) = hc2/hcq2
         if (i_K13 > 0)vnh4s(mstr,mRB) = hc3/hcq3
         if (i_K14 > 0)vno2s(mstr,mRB) = hc4/hcq4
         if (i_K15 > 0)vno3s(mstr,mRB) = hc5/hcq5
         if (i_K16 > 0)gesNs(mstr,mRB) = hc6/hcq6
         if (i_K17 > 0)vx0s(mstr,mRB) = hc7/hcq7
         if (i_K18 > 0)vx02s(mstr,mRB) = hc8/hcq8
         if (i_K19 > 0)gelPs(mstr,mRB) = hc9/hcq9
         if (i_K110 > 0)gesPs(mstr,mRB) = hc10/hcq10
         if (i_K111 > 0)Sis(mstr,mRB) = hc11/hcq11
         if (i_K112 > 0)chlas(mstr,mRB) = hc12/hcq12
         if (i_K113 > 0)vkigrs(mstr,mRB) = hc13/hcq13
         if (i_K114 > 0)antbls(mstr,mRB) = hc14/hcq14
         if (i_K115 > 0)zooins(mstr,mRB) = hc15/hcq15
         if (i_K116 > 0)vphs(mstr,mRB) = hc16/hcq16
         if (i_K117 > 0)mws(mstr,mRB) = hc17/hcq17
         if (i_K118 > 0)cas(mstr,mRB) = hc18/hcq18
         if (i_K119 > 0)lfs(mstr,mRB) = hc19/hcq19
         if (i_K120 > 0)ssalgs(mstr,mRB) = hc20/hcq20
         if (i_K121 > 0)tempws(mstr,mRB) = hc21/hcq21
         ! if(i_K122>0)vo2s(mstr,mRB) = hc22/hcq22
         if (i_K123 > 0)CHNFs(mstr,mRB) = hc23/hcq23
         if (i_K124 > 0)BVHNFs(mstr,mRB) = hc24/hcq24
         if (i_K125 > 0) then
            Colis(mstr,mRB) = hc25/hcq25
            DOSCFs(mstr,mRB) = hc27/hcq25
         endif
         if (i_K126 > 0)tempws(mstr,mRB) = hc26
         
      enddo
   enddo ! Strangschleife ENDE
   
   ! --------------------------------------------------------------------------
   ! Abfrage ob alle benötigten Eingaben gemacht wurden (nur beim Modellstart)
   ! --------------------------------------------------------------------------
   
   ! Initialisation of values moved here to prevent uninitaliised use
   fssgrs = 0.7
   fbsgrs = 0.4  !   0.21
   bsbzoo = 0.
   ! frfgrs = 0.13
   
   if (iwsim == 4) goto 396   !Tracer
   
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      do mRB = 1,mRBs(mstr)
         if (RBtyp(mstr,mRB) == 1 .or. RBtyp(mstr,mRB) == 2) cycle
         
         if (iwsim /= 2 .and. iwsim /= 5) then
            ! Nitrosomonas
            if (vnh4s(mstr,mRB) > 0.0 .and. vx0s(mstr,mRB) < 0.0) then
               call qerror("Missing values for nitrosomonas at boundary.")
            endif
            
            ! Nitrobacter
            if (vnh4s(mstr,mRB) > 0.0 .and. vno2s(mstr,mRB) > 0.0.and.vx02s(mstr,mRB) < 0.0) then
               call qerror("Missing values for nitrobacter at boundary.")
            endif
            
            ! Anteil der Kieselalgen
            if (chlas(mstr,mRB) > 0.0 .and. vkigrs(mstr,mRB) < 0.0) then
               call qerror("Missing values for 'Anteil Kieselalgen' at boundary.")
            endif
            
            ! Anteil der Blaualgen
            if (chlas(mstr,mRB) > 0.0 .and. antbls(mstr,mRB) < 0.0) then
               call qerror("Missing values for 'Anteil Blaualgen' at boundary.")
            endif
            
            ! falls der Anteil der Blaualgen = 0 ist
            if (antbls(mstr,mRB) == 0.0) then
               ! TODO FG: This means one can never run a model without cyanobacteria ...
               antbls(mstr,mRB) = 0.01
               vkigrs(mstr,mRB) = max(0.01, (vkigrs(mstr,mRB) - antbls(mstr,mRB)))
            endif
            
            ! Silikat
            if (chlas(mstr,mRB) > 0.0 .and. vkigrs(mstr,mRB) > 0.0.and.Sis(mstr,mRB) < 0.0) then
               call qerror("Missing values for silicate at boundary.")
            endif
            
            if (iph == 1) then
               ! ph
               if (vphs(mstr,mRB) <= 0.0) call qerror("Missing values for pH at boundary.")
               ! m-value
               if( mws(mstr,mRB)  <= 0.0) call qerror("Missing values for m-value at boundary.")
               ! calcium
               if (Cas(mstr,mRB)  <= 0.0) call qerror("Missing values for calcium at boundary.")
            endif
            
            ! BSB5 und CSB
            if (vbsbs(mstr,mRB) < 0.0 .and. vcsbs(mstr,mRB) < 0.0) then
               call qerror("Missing values for C-BSB5 or CSB at boundary. &
                           &One of them must be given.")
            endif
            
            ! Schwebstoffe
            if (ssalgs(mstr,mRB) < 0.0) then
               call qerror("Missing values for suspended matter at boundary.")
            endif
         endif
            
         ! Temperatur
         if (iwsim == 2 .or. iwsim == 3 .or. iph == 1) then
            if (tempws(mstr,mRB) == -9.99) then
               call qerror("Missing values for temperature at boundary.")
            endif
         endif
         
      enddo
   enddo
   
   
   ! --------------------------------------------------------------------------
   !  Umrechnung der Zellzahlen von HNF in mgC
   ! --------------------------------------------------------------------------
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      do  mRB = 1,mRBs(mstr)
       
         if (CHNFs(mstr,mRB) < 0.0) then
            CHNFs(mstr,mRB)  = 0.0
            BVHNFs(mstr,mRB) = 0.0
         else
            if (CHNFs(mstr,mRB) > 0.0 .and. BVHNFs(mstr,mRB) <= 0.0) BVHNFs(mstr,mRB) = 25. ! in µm3
            CHNFs(mstr,mRB) = CHNFs(mstr,mRB)*BVHNFs(mstr,mRB)*0.22
            
            ! Umrechnung von pg in mg /1.e9; Angabe CHNFs pro ml ergibt /1.e6 bezogen auf ein Liter
            CHNFs(mstr,mRB) = CHNFs(mstr,mRB) * 1.e-6
         endif
      enddo
   enddo
   
   
   ! ==========================================================================
   ! Setzen von Werten am Startprofil
   ! ==========================================================================
   
   ! TODO FG: introduced iColi == 0  to prevent initialisation error
   if ((iwsim == 2 .and. iColi == 0) .or. iwsim == 5) goto 396
   
   einmalig = .true. ! Fehlermeldung nur einmal
   
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      do  mRB = 1,mRBs(mstr)
         if (NRSchr(mstr,mRB) == 0) cycle
         
         hcchla = chlas(mstr,mRB)
         hczoos = zooins(mstr,mRB)
         hcnh4s = vnh4s(mstr,mRB)
         hcno2s = vno2s(mstr,mRB)
         hcno3s = vno3s(mstr,mRB)
         hcgePs = gelPs(mstr,mRB)
         hcbsb  = vbsbs(mstr,mRB)
         hccsb  = vcsbs(mstr,mRB)
         hcchla = chlas(mstr,mRB)
         hcvkg  = vkigrs(mstr,mRB)
         hcantb = antbls(mstr,mRB)
         
         if (zooins(mstr,mRB) < 0.0) zooins(mstr,mRB) = 0.0
         if (vnh4s(mstr,mRB) < 0.0)  vnh4s(mstr,mRB)  = 0.0
         if (vno2s(mstr,mRB) < 0.0)  vno2s(mstr,mRB)  = 0.0
         if (vno3s(mstr,mRB) < 0.0)  vno3s(mstr,mRB)  = 0.0
         if (gelPs(mstr,mRB) < 0.0)  gelPs(mstr,mRB)  = 0.0
         if (vbsbs(mstr,mRB) < 0.0)  vbsbs(mstr,mRB)  = 0.0
         if (vcsbs(mstr,mRB) < 0.0)  vcsbs(mstr,mRB)  = 0.0
         if (chlas(mstr,mRB) < 0.0)  chlas(mstr,mRB)  = 0.0
         if (vkigrs(mstr,mRB) < 0.0) vkigrs(mstr,mRB) = 0.0
         if (antbls(mstr,mRB) < 0.0) antbls(mstr,mRB) = 0.0
         
         ! Fehlerausgabe falls AnteilGR+AnteilKI+AnteilBL >1
         if (vkigrs(mstr,mRB) + antbls(mstr,mRB) > 1.0) then
            write(message, "(a,i0)") 'Die Anteile der Kiesel- und Blaualgen sind zusammen größer 1 (Strang):', mstr
            call qerror(message)
         endif
         
         if (RBtyp(mstr,mRB) == 0) TGZoo(mstr,1) = GROT
         if (RBtyp(mstr,mRB) == 2) TGZoo(mstr,hanze(mstr)+1) = GROT
         
         ! Berechnung des Chlorophyll-a/Kohlenstoff-Verhaeltnisses
         ! Angabe in mgChla/mgC
         call ini_algae(akchl, abchl, agchl,    &
                        Cagr, Caki, Cabl, CZoo, &
                        a1Ki, a2Ki, a3Ki,       &
                        a1Bl, a2Bl, a3Bl,       &
                        a1Gr, a2Gr, a3Gr)
         
         ! Temperaturabhängigkeit des C:Chla-Verhältnisses 
         ! ag(k,b)chl gilt für 20°C  in mgC/mgChla
         ! mg Algenbiomasse, Chla in µg/l
         call algae_start(chlas(mstr,mRB),vkigrs(mstr,mRB),antbls(mstr,mRB),tempws(mstr,mRB),&
                          akbcms(mstr,mRB),abbcms(mstr,mRB),agbcms(mstr,mRB),                &
                          akis(mstr,mRB),abls(mstr,mRB),agrs(mstr,mRB),                      &
                          a1Ki,a1Bl,a1Gr,Caki,Cabl,Cagr,akchl,abchl,agchl,                   &
                          chlaks(mstr,mRB),chlabs(mstr,mRB),chlags(mstr,mRB))
         
         ! Berechnung der "BSB-Komponenten" am oberen Rand
         ! auch bei Stundenwert-Generierung
         call orgc_start(TOC_CSB,bsbZoo,GROT,                                                            &
                         akis(mstr,mRB),abls(mstr,mRB),agrs(mstr,mRB),                                   &
                         Caki,Cabl,Cagr,CZoo, bsbki,bsbbl,bsbgr,  csbki,csbbl,csbgr,                     &
                         zooins(mstr,mRB),vbsbs(mstr,mRB),vcsbs(mstr,mRB),                               &
                         obsbs(mstr,mRB),ocsbs(mstr,mRB),                                                &
                         CMs(mstr,mRB), CDs(mstr,1,mRB),CDs(mstr,2,mRB), CPs(mstr,1,mRB),CPs(mstr,2,mRB),&
                         ssalgs(mstr,mRB),frfgrs(mstr,mRB),BACs(mstr,mRB),CHNFs(mstr,mRB),               &
                         CPges,CDges,Cref,TOC)
                         
         ! zelluläre Nährstoffgehalte
         call naehr_start(akis(mstr,mRB),abls(mstr,mRB),agrs(mstr,mRB),                                   &
                          vnh4s(mstr,mRB),vNO3s(mstr,mRB),vno2s(mstr,mRB),gesNs(mstr,mRB),                &
                          zooins(mstr,mRB),                                                               &
                          gelPs(mstr,mRB),gesPs(mstr,mRB),                                                &
                          Q_NKs(mstr,mRB),Q_PKs(mstr,mRB),Q_SKs(mstr,mRB),Q_NGs(mstr,mRB),Q_PGs(mstr,mRB),&
                          Q_NBs(mstr,mRB),Q_PBs(mstr,mRB),                                                &
                          Qmx_NK,Qmn_NK,Qmx_PK,Qmn_PK,Qmx_SK,Qmn_SK, Qmx_NG,Qmn_NG,Qmx_PG,Qmn_PG, Qmx_NB, &
                          Qmn_NB,Qmx_PB,Qmn_PB,                                                           &
                          CPges,CDges,Cref,BACs(mstr,mRB),CMs(mstr,mRB),                                  &
                          nl0s(mstr,mRB), pl0s(mstr,mRB),                                                 &
                          sss(mstr,mRB), ssalgs(mstr,mRB),                                                &
                          itags,monats,mstr,mRB, einmalig,                                                &
                          .false., 0)
         
         zooins(mstr,mRB) = hczoos
         vnh4s(mstr,mRB) = hcnh4s
         vno2s(mstr,mRB) = hcno2s
         vno3s(mstr,mRB) = hcno3s
         gelPs(mstr,mRB) = hcgePs
         if (hcbsb == 0.0 .and. hccsb == 0.0) then
            vbsbs(mstr,mRB) = hcbsb
            vcsbs(mstr,mRB) = hccsb
         endif
         chlas(mstr,mRB) = hcchla
         vkigrs(mstr,mRB) = hcvkg
         antbls(mstr,mRB) = hcantb
         if (RBTyp(mstr,mRB) == 2 .and. hcbsb < 0.0.and.hccsb < 0.0) then
            
            CDs(mstr,1,mRB) = -1.
            CDs(mstr,2,mRB) = -1.
            CPs(mstr,1,mRB) = -1.
            CPs(mstr,2,mRB) = -1.
            obsbs(mstr,mRB) = -1.
            ocsbs(mstr,mRB) = -1.
         endif
         if (RBTyp(mstr,mRB) == 2 .and. hcchla < 0.0) then
            akis(mstr,mRB) = -1.
            abls(mstr,mRB) = -1.
            agrs(mstr,mRB) = -1.
            chlaks(mstr,mRB) = -1.
            chlabs(mstr,mRB) = -1.
            chlags(mstr,mRB) = -1.
         endif
         
         !!wy if(RBtyp(mstr,mRB).gt.0.or.NRschr(mstr,mRB).eq.0)cycle
         ! auch an Tide- und Ausflussrändern p-Wert berechnen.
         
         ! Berechnung des p-Wertes am Start (ohne Algen)
         if (iph == 1) then
            call pwert(mws(mstr,mRB),vphs(mstr,mRB),lfs(mstr,mRB),tempws(mstr,mRB),pws(mstr,mRB))
         endif
         
         if (RBtyp(mstr,mRB) > 0 .or. NRschr(mstr,mRB) == 0) cycle
         
         ! Belegen des 1. Knotens eines Strangs, wenn Vorstränge und Einleitung am 1. Knoten
         ! TODO FG: check if adding azStrs == 1 causes troubles in Elbe model.
         if (nnstrs(mstr) > 0 .or. azStrs == 1) then
            hgesN(mstr,1)  = gesNs(mstr,mRB)
            hgesP(mstr,1)  = gesPs(mstr,mRB)
            hfssgr(mstr,1) = frfgrs(mstr,mRB)
            hnl0(mstr,1)   = nl0s(mstr,mRB)
            hpl0(mstr,1)   = pl0s(mstr,mRB)
            hbsb(mstr,1)   = obsbs(mstr,mRB)
            hcsb(mstr,1)   = ocsbs(mstr,mRB)
            hCHNF(mstr,1)  = CHNFs(mstr,mRB)
            hBVHNF(mstr,1) = BVHNFs(mstr,mRB)
            hCD(mstr,1,1)  = CDs(mstr,1,mRB)
            hCD(mstr,2,1)  = CDs(mstr,2,mRB)
            hCP(mstr,1,1)  = CPs(mstr,1,mRB)
            hCP(mstr,2,1)  = CPs(mstr,2,mRB)
            hCM(mstr,1)    = CMs(mstr,mRB)
            hBAC(mstr,1)   = BACs(mstr,mRB)
            hnh4(mstr,1)   = vNH4s(mstr,mRB)
            if (isnan(vo2s(mstr,mRB))) print*, "isnan(vo2s) ", mstr, mRB
            ho2(mstr,1)    = vo2s(mstr,mRB)
            hno3(mstr,1)   = vno3s(mstr,mRB)
            hno2(mstr,1)   = vnO2s(mstr,mRB)
            hx0(mstr,1)    = vx0s(mstr,mRB)
            hx02(mstr,1)   = vx02s(mstr,mRB)
            hsi(mstr,1)    = Sis(mstr,mRB)
            hchla(mstr,1)  = chlas(mstr,mRB)
            haki(mstr,1)   = (chlas(mstr,mRB) *       vkigrs(mstr,mRB)                    /1000.) * (hakbcm(mstr,1) / Caki)
            hagr(mstr,1)   = (chlas(mstr,mRB) * (1. - vkigrs(mstr,mRB) - antbls(mstr,mRB))/1000.) * (hagbcm(mstr,1) / Cagr)
            habl(mstr,1)   = (Chlas(mstr,mRB) *                          antbls(mstr,mRB) /1000.) * (habbcm(mstr,1) / Cabl)
            hchlak(mstr,1) = chlas(mstr,mRB) *       vkigrs(mstr,mRB)
            hchlag(mstr,1) = chlas(mstr,mRB) * (1. - vkigrs(mstr,mRB) - antbls(mstr,mRB))
            hchlab(mstr,1) = chlas(mstr,mRB) *                          antbls(mstr,mRB)
            hvkigr(mstr,1) = vkigrs(mstr,mRB)
            hantbl(mstr,1) = antbls(mstr,mRB)
            hssalg(mstr,1) = ssalgs(mstr,mRB)
            hss(mstr,1)    = sss(mstr,mRB)
            hzooi(mstr,1)  = zooins(mstr,mRB)
            hgelp(mstr,1)  = gelps(mstr,mRB)
            hmw(mstr,1)    = mws(mstr,mRB)
            hpw(mstr,1)    = pws(mstr,mRB)
            hca(mstr,1)    = cas(mstr,mRB)
            hlf(mstr,1)    = lfs(mstr,mRB)
            hph(mstr,1)    = vphs(mstr,mRB)
            hcoli(mstr,1)  = colis(mstr,mRB)
            hDOSCF(mstr,1) = DOSCFs(mstr,mRB)
            hvbsb(mstr,1)  = vbsbs(mstr,mRB)
            hvcsb(mstr,1)  = vcsbs(mstr,mRB)
            hgsZn(mstr,1)  = gsZns(mstr,mRB)
            hglZn(mstr,1)  = glZns(mstr,mRB)
            hgsCad(mstr,1) = gsCads(mstr,mRB)
            hglCad(mstr,1) = glCads(mstr,mRB)
            hgsCu(mstr,1)  = gsCus(mstr,mRB)
            hglCu(mstr,1)  = glCus(mstr,mRB)
            hgsNi(mstr,1)  = gsNis(mstr,mRB)
            hglNi(mstr,1)  = glNis(mstr,mRB)
            hgsAs(mstr,1)  = gsAss(mstr,mRB)
            hglAs(mstr,1)  = glAss(mstr,mRB)
            hgsPb(mstr,1)  = gsPbs(mstr,mRB)
            hglPb(mstr,1)  = glPbs(mstr,mRB)
            hgsCr(mstr,1)  = gsCrs(mstr,mRB)
            hglCr(mstr,1)  = glCrs(mstr,mRB)
            hgsFe(mstr,1)  = gsFes(mstr,mRB)
            hglFe(mstr,1)  = glFes(mstr,mRB)
            hgsHg(mstr,1)  = gsHgs(mstr,mRB)
            hglHg(mstr,1)  = glHgs(mstr,mRB)
            hgsMn(mstr,1)  = gsMns(mstr,mRB)
            hglMn(mstr,1)  = glMns(mstr,mRB)
            hgsU(mstr,1)   = gsUs(mstr,mRB)
            hglU(mstr,1)   = glUs(mstr,mRB)
         endif
         
      enddo
   enddo
   
   ! ==========================================================================
   ! 2D-Modellierung
   ! Ermittlung der Anzahl der vertikalen Schichten für jeden Knoten
   ! ==========================================================================
   396 continue
   pfadstring =  trim(adjustl(cpfad)) // 'sysgenou'
   open(unit = 11, file = pfadstring, iostat = open_error)
   rewind (11)
   
   nkzsmx = 0
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      read(11,1000)hanze(mstr)
      
      do ior = 1,hanze(mstr)
         read(11,'(f8.3,28x,f5.2)') fkm(ior),tiefe(ior)
         ! Ruecksetzen
         hnkzs(mstr,ior) = 1
         hdH2De(mstr,ior) = 0.0
         hnkzs(mstr,ior) = 1
      enddo
      
      hnkzs(mstr,hanze(mstr)+1) = hnkzs(mstr,hanze(mstr))
      hdH2De(mstr,hanze(mstr)+1) = hdH2De(mstr,hanze(mstr))
      
      ! maximale Schichtenanzahl für den Anfang und das Ende der einzelnen
      if (hnkzs(mstr,1) > nkzsmx) nkzsmx = hnkzs(mstr,1)
      if (hnkzs(mstr,hanze(mstr)+1) > nkzsmx) nkzsmx = hnkzs(mstr,hanze(mstr)+1)
      
   enddo
   close (11)
   
   ! ==========================================================================
   ! get time for sunrise and sunset
   ! ==========================================================================
   call sasu(itags,monats,geob,geol,sa,su,zg,zlk,dk,tdj)
   
   ! ==========================================================================
   ! Ermittlung der Kenngrößen zur Berücksichtigung des Wehrüberfalls
   ! ==========================================================================
   if (iwsim /= 2 .and. iwsim /= 5) then
      call wehrles(itags, monats, Jahrs, uhrz, wehrh, wehrb, mStra, &
                   jlWO2, janzWt, janzWs, cpfad, iwied)
   endif
   
   ! ==========================================================================
   ! Ermittlung der Wetterdaten für den Zeitschritt
   ! ==========================================================================
   9191 continue
   if (iwsim /= 4 .and. iwsim /= 5) then
      call wettles(itags, monats, jahrs, uhrz, glob, tlmax, tlmin, ro, wge, &
                   cloud, wtyp, imet, iwied, cpfad, ckenn_vers1)
   endif
   
   ! ==========================================================================
   ! Strangschleife für alle Straenge
   ! Einlesen der Einleiterdaten und Randbedingungen
   ! ==========================================================================
   if (iwied == 0) then
      ! Ermittlung eines Strangs mit Randbedingungen am 1. Ortspunkt
      ! alle Stränge die keine Randbedingung am 1. Ortspunkt und keine Vor- 
      ! und nachgelagerten Straenge haben, werden mit diesen Randbedingungen belegt.
      mRB_1 = 0
      j = 0
      do istr = 1,azStrs
         mstr = STRNR(istr)
         do mRB = 1,mRBs(mstr)
            if (RBtyp(mstr,mRB) == 0 .or. RBtyp(mstr,mRB) == 2) then
               if (mRB_1 == 0) then
                  mstrRB = mstr
                  mRB_1  = mRB
               endif
            endif
         
            if (RBtyp(mstr,mRB) == 0 .and. nstrs(istr) == 0) then
               j = j + 1
               mstr_ist(j) = mstr
            endif
         enddo
      enddo
   endif
   
   if (iwied == 0) j = 0
   do istr = 1,istrs  ! Beginn Strangschleife
      mstr = STRNR(istr)
      ieinsh(mstr) = 0
      iflRi(mstr) = iflRi_l(istr)
      j_ist = 1 ! Zufließende Straenge sind mit Randbedingungen belegt
      
      ! Test, ob zu Beginn der Simulation die zufließenden Stränge mit Anfangsbedingungen belegt sind
      if (iwied == 0) then  
         j_ist = 0
         if (nstrs(istr) /= 0) then
            do nstr = 1,nstrs(istr)
               do jj = 1,azStrs    !js
                  if (ESTRNR(istr,nstr) == mstr_ist(jj)) then
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
      !if(iFlRi(mstr)==0.and.iwied==1)cycle
      iein = 0
      ieinL = 0
      
      ! Nr der Randbedingung bei Randbedingungstyp 0 und 2
      if (iwied == 1) mRB_1 = 0
      
      ! Schalter zur Überprüfung ob am ersten Ortspunkt eines Strangs
      ! Randbedingungen vorliegen (Datei Ereigg.txt)
      mRand = 0
      
      do mRB = 1,mRBs(mstr) ! RandbedingungsSchleife fuer Strang mstr
         if (RBtyp(mstr,mRB) /= 0 .and. RBtyp(mstr,mRB) /= 2) then
            
            if (mstrLe(mstr,mRB) < 0) then 
               ! point sources
               iein = iein+1
               
               qeinlh(mstr,iein) = abfls(mstr,mRB)
               ebsbh(mstr,iein)  = vbsbs(mstr,mRB)
               ecsbh(mstr,iein)  = vcsbs(mstr,mRB)
               enh4h(mstr,iein)  = vnh4s(mstr,mRB)
               eno2h(mstr,iein)  = vno2s(mstr,mRB)
               eno3h(mstr,iein)  = vno3s(mstr,mRB)
               egesNh(mstr,iein) = gesNs(mstr,mRB)
               ex0h(mstr,iein)   = vx0s(mstr,mRB)
               ex02h(mstr,iein)  = vx02s(mstr,mRB)
               egph(mstr,iein)   = gelps(mstr,mRB)
               egesPh(mstr,iein) = gesPs(mstr,mRB)
               esih(mstr,iein)   = sis(mstr,mRB)
               echlah(mstr,iein) = chlas(mstr,mRB)
               evkgh(mstr,iein)  = vkigrs(mstr,mRB)
               eantbh(mstr,iein) = antbls(mstr,mRB)
               ezindh(mstr,iein) = zooins(mstr,mRB)
               ephh(mstr,iein)   = vphs(mstr,mRB)
               emwh(mstr,iein)   = mws(mstr,mRB)
               ecah(mstr,iein)   = cas(mstr,mRB)
               elfh(mstr,iein)   = lfs(mstr,mRB)
               essh(mstr,iein)   = ssalgs(mstr,mRB)
               etemph(mstr,iein) = tempws(mstr,mRB)
               eo2h(mstr,iein)   = vo2s(mstr,mRB)
               eCHNFh(mstr,iein) = CHNFs(mstr,mRB)
               eBVHNh(mstr,iein) = BVHNFs(mstr,mRB)
               ecolih(mstr,iein) = colis(mstr,mRB)
               ewaerh(mstr,iein) = waers(mstr,mRB)
               typh(mstr,iein)   = weinl(mstr,mRB)
               enl0h(mstr,iein)  = nl0s(mstr,mRB)
               epl0h(mstr,iein)  = pl0s(mstr,mRB)
               eCD(mstr,1,iein)  = CDs(mstr,1,mRB)
               eCD(mstr,2,iein)  = CDs(mstr,2,mRB)
               eCP(mstr,1,iein)  = CPs(mstr,1,mRB)
               eCP(mstr,2,iein)  = CPs(mstr,2,mRB)
               eCM(mstr,iein)    = CMs(mstr,mRB)
               eBAC(mstr,iein)   = BACs(mstr,mRB)
               egsZn(mstr,iein)  = gsZns(mstr,mRB)
               eglZn(mstr,iein)  = glZns(mstr,mRB)
               egsCad(mstr,iein) = gsCads(mstr,mRB)
               eglCad(mstr,iein) = glCads(mstr,mRB)
               egsCu(mstr,iein)  = gsCus(mstr,mRB)
               eglCu(mstr,iein)  = glCus(mstr,mRB)
               egsNi(mstr,iein)  = gsNis(mstr,mRB)
               eglNi(mstr,iein)  = glNis(mstr,mRB)
               egsAs(mstr,iein)  = gsAss(mstr,mRB)
               eglAs(mstr,iein)  = glAss(mstr,mRB)
               egsPb(mstr,iein)  = gsPbs(mstr,mRB)
               eglPb(mstr,iein)  = glPbs(mstr,mRB)
               egsCr(mstr,iein)  = gsCrs(mstr,mRB)
               eglCr(mstr,iein)  = glCrs(mstr,mRB)
               egsFe(mstr,iein)  = gsFes(mstr,mRB)
               eglFe(mstr,iein)  = glFes(mstr,mRB)
               egsHg(mstr,iein)  = gsHgs(mstr,mRB)
               eglHg(mstr,iein)  = glHgs(mstr,mRB)
               egsMn(mstr,iein)  = gsMns(mstr,mRB)
               eglMn(mstr,iein)  = glMns(mstr,mRB)
               egsU(mstr,iein)   = gsUs(mstr,mRB)
               eglU(mstr,iein)   = glUs(mstr,mRB)
            
            else
               ! diffuse sources
               ieinL = ieinL+1
               qLh(mstr,ieinL)    = abfls(mstr,mRB)/WirkLL(mstr,ieinL)
               bsbLh(mstr,ieinL)  = vbsbs(mstr,mRB)
               csbLh(mstr,ieinL)  = vcsbs(mstr,mRB)
               enh4Lh(mstr,ieinL) = vnh4s(mstr,mRB)
               eno2Lh(mstr,ieinL) = vno2s(mstr,mRB)
               eno3Lh(mstr,ieinL) = vno3s(mstr,mRB)
               gesNLh(mstr,ieinL) = gesNs(mstr,mRB)
               x0Lh(mstr,ieinL)   = vx0s(mstr,mRB)
               x02Lh(mstr,ieinL)  = vx02s(mstr,mRB)
               gpLh(mstr,ieinL)   = gelps(mstr,mRB)
               gesPLh(mstr,ieinL) = gesPs(mstr,mRB)
               siLh(mstr,ieinL)   = sis(mstr,mRB)
               phLh(mstr,ieinL)   = vphs(mstr,mRB)
               caLh(mstr,ieinL)   = cas(mstr,mRB)
               elfLh(mstr,ieinL)  = lfs(mstr,mRB)
               ssLh(mstr,ieinL)   = ssalgs(mstr,mRB)
               tempLh(mstr,ieinL) = tempws(mstr,mRB)
               o2Lh(mstr,ieinL)   = vo2s(mstr,mRB)
               coliLh(mstr,ieinL) = colis(mstr,mRB)
               enl0Lh(mstr,ieinL) = nl0s(mstr,mRB)
               pl0Lh(mstr,ieinL)  = pl0s(mstr,mRB)
               chlaLh(mstr,ieinL) = chlas(mstr,mRB)
               ! CDL(mstr,1,iein) = CDs(mstr,1,mRB)
               ! CDL(mstr,2,iein) = CDs(mstr,2,mRB)
               ! CPL(mstr,1,iein) = CPs(mstr,1,mRB)
               ! CPL(mstr,2,iein) = CPs(mstr,2,mRB)
               ! CML(mstr,iein) = CMs(mstr,mRB)
               ! BACL(mstr,iein) = BACs(mstr,mRB)
            endif
         
         else
            ! nStrs(mstr) Es gibt eine Randbedingung am 1. Ortspunkt eines Strangs
            if (iFlRi(mstr) == 1 .and. RBtyp(mstr,mRB) == 0 .and. nStrs(istr) == 0) then
               mRand = 1
               mstrRB = mstr   ! mstrRB: StrangNummer mit Randbedingung am 1. Ortspunkt
               mRB_1 = mRB     ! Nummer der Randbedingung für den ersten (letzten)Ortspunkt eines Strangs
           elseif (iFlRi(mstr) == -1 .and. RBtyp(mstr,mRB) == 2) then
               mRand = 2
               mstrRB = mstr
               mRB_1 = mRB
            endif
         endif
         
      enddo  ! Randbedingungsschleife
      
      ieinsh(mstr) = iein
      ieinLs(mstr) = ieinL
      
      ! j_ist: gilt nur bei iwied=0. Zufließende Straenge sind bereits mit
      ! Randedingungen belegt
      if (nstrs(istr) <= 0 .or. j_ist /= 1) then
         inkzmx = nkzsmx
         mRB = mRB_1
         mstr1 = mstr
         !if(iwied==0.and.iwsim/=4)then
         if (iwied == 0) then
            ianze(mstr) = hanze(mstr)+1
            iB = 1
            anzej = hanze(mstr)+1
            if (iwsim == 4) then
               anzej = 1                    ! Tracer
               if (mRand == 0) then
                  mstr1 = mstr
                  tempws(mstr1,mRB) = 0.0
               endif
            elseif (mRand == 0) then
               mstr1 = mstrRB               ! Falls keine Randbedingung fuer diesen Strang vorhanden, wird
            endif
         else if (iwied == 1) then
            iB = 1
            anzej = 1
            mstr1 = mstr
            if (mRand /= 1)anzej = 0     ! keine Belegung des 1. Ortspunkts, da keine Randbedigung vorhanden
            if (iflRi(mstr) == -1) then
               IB = hanze(mstr)+1
               anzej = hanze(mstr)+1
               if (mRand /= 2)anzej = hanze(mstr) ! keine Belegung des 1. Ortspunkts, da keine Randbedigung vorhanden
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
            if (RBtyp(mstr,mRB) == 0) then
               Wtst = -1.0
               Wtst_T = -9.99
            else
               Wtst = 0.0
               Wtst_T = -9.99
            endif
            if (tempws(mstr1,mRB) > Wtst_T) then
               hakbcm(mstr,ior) = akbcms(mstr1,mRB)
               hagbcm(mstr,ior) = agbcms(mstr1,mRB)
               habbcm(mstr,ior) = abbcms(mstr1,mRB)
            endif
            
            if (nl0s(mstr1,mRB)   > 0.0) hnl0(mstr,ior)  = nl0s(mstr1,mRB)
            if (pl0s(mstr1,mRB)   > 0.0) hpl0(mstr,ior)  = pl0s(mstr1,mRB)
            if (gesNs(mstr1,mRB)  >=0.0) hgesN(mstr,ior) = gesNs(mstr1,mRB)
            if (gesPs(mstr1,mRB)  >=0.0) hgesP(mstr,ior) = gesPs(mstr1,mRB)
            if (Q_NKs(mstr1,mRB)  > 0.0) hQ_NK(mstr,ior) = Q_NKs(mstr1,mRB)
            if (Q_PKs(mstr1,mRB)  > 0.0) hQ_PK(mstr,ior) = Q_PKs(mstr1,mRB)
            if (Q_SKs(mstr1,mRB)  > 0.0) hQ_SK(mstr,ior) = Q_SKs(mstr1,mRB)
            if (Q_NGs(mstr1,mRB)  > 0.0) hQ_NG(mstr,ior) = Q_NGs(mstr1,mRB)
            if (Q_PGs(mstr1,mRB)  > 0.0) hQ_PG(mstr,ior) = Q_PGs(mstr1,mRB)
            if (Q_NBs(mstr1,mRB)  > 0.0) hQ_NB(mstr,ior) = Q_NBs(mstr1,mRB)
            if (Q_PBs(mstr1,mRB)  > 0.0) hQ_PB(mstr,ior) = Q_PBs(mstr1,mRB)
            if (tempws(mstr1,mRB) > Wtst_T) htempw(mstr,ior) = tempws(mstr1,mRB)
            
            ! Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser
            if (iwied == 0)hTsed(mstr,ior) = htempw(mstr,ior)
            if (obsbs(mstr1,mRB) >= Wtst) hbsb(mstr,ior)   = obsbs(mstr1,mRB)
            if (ocsbs(mstr1,mRB) >= Wtst) hcsb(mstr,ior)   = ocsbs(mstr1,mRB)
            if (CHNFs(mstr1,mRB) >= Wtst) hCHNF(mstr,ior)  = CHNFs(mstr1,mRB)
            if (BVHNFs(mstr1,mRB)>= Wtst) hBVHNF(mstr,ior) = BVHNFs(mstr1,mRB)
            if (CDs(mstr1,1,mRB) >= Wtst) hCD(mstr,1,ior)  = CDs(mstr1,1,mRB)
            if (CDs(mstr1,2,mRB) >= Wtst) hCD(mstr,2,ior)  = CDs(mstr1,2,mRB)
            if (CPs(mstr1,1,mRB) >= Wtst) hCP(mstr,1,ior)  = CPs(mstr1,1,mRB)
            if (CPs(mstr1,2,mRB) >= Wtst) hCP(mstr,2,ior)  = CPs(mstr1,2,mRB)
            if (CMs(mstr1,mRB)   >= Wtst) hCM(mstr,ior)    = CMs(mstr1,mRB)
            if (BACs(mstr1,mRB)  >= Wtst) hBAC(mstr,ior)   = BACs(mstr1,mRB)
            if (vnh4s(mstr1,mRB) >= Wtst) hnh4(mstr,ior)   = vnh4s(mstr1,mRB)
            if (vo2s(mstr1,mRB)  >= Wtst) ho2(mstr,ior)    = vo2s(mstr1,mRB)
            if (isnan(ho2(mstr,ior))) print*,"ho2(mstr,ior) = vo2s(mstr1,mRB)",ho2(mstr,ior),mstr,ior,vo2s(mstr1,mRB),mstr1,mRB
            if (vno3s(mstr1,mRB) >= Wtst) hno3(mstr,ior)   = vno3s(mstr1,mRB)
            if (vno2s(mstr1,mRB) >= Wtst) hno2(mstr,ior)   = vno2s(mstr1,mRB)
            if (vx0s(mstr1,mRB)  >= Wtst) hx0(mstr,ior)    = vx0s(mstr1,mRB)
            if (vx02s(mstr1,mRB) >= Wtst) hx02(mstr,ior)   = vx02s(mstr1,mRB)
            if (sis(mstr1,mRB)   >= Wtst) hsi(mstr,ior)    = sis(mstr1,mRB)
            if (chlas(mstr1,mRB) >= Wtst) hchla(mstr,ior)  = chlas(mstr1,mRB)
            if (akis(mstr1,mRB)  >= Wtst) haki(mstr,ior)   = akis(mstr1,mRB)
            if (agrs(mstr1,mRB)  >= Wtst) hagr(mstr,ior)   = agrs(mstr1,mRB)
            if (abls(mstr1,mRB)  >= Wtst) habl(mstr,ior)   = abls(mstr1,mRB)
            if (chlaks(mstr1,mRB)>= Wtst) hchlak(mstr,ior) = chlaks(mstr1,mRB)
            if (chlags(mstr1,mRB)>= Wtst) hchlag(mstr,ior) = chlags(mstr1,mRB)
            if (chlabs(mstr1,mRB)>= Wtst) hchlab(mstr,ior) = chlabs(mstr1,mRB)
            if (vkigrs(mstr1,mRB)>= Wtst) hvkigr(mstr,ior) = vkigrs(mstr1,mRB)
            if (antbls(mstr1,mRB)>= Wtst) hantbl(mstr,ior) = antbls(mstr1,mRB)
            habrz1(mstr,ior) = 0.0
            if (ssalgs(mstr1,mRB)>= Wtst) hssalg(mstr,ior) = ssalgs(mstr1,mRB)
            if (sss(mstr1,mRB)   >= Wtst) hss(mstr,ior)    = sss(mstr1,mRB)
            if (zooins(mstr1,mRB)>= Wtst) hzooi(mstr,ior)  = zooins(mstr1,mRB)
            if (gelps(mstr1,mRB) >= Wtst) hgelp(mstr,ior)  = gelps(mstr1,mRB)
            if (mws(mstr1,mRB)   >= Wtst) hmw(mstr,ior)    = mws(mstr1,mRB)
            if (mws(mstr1,mRB)   >= Wtst) hpw(mstr,ior)    = pws(mstr1,mRB)
            if (cas(mstr1,mRB)   >= Wtst) hca(mstr,ior)    = cas(mstr1,mRB)
            if (lfs(mstr1,mRB)   >= Wtst) hlf(mstr,ior)    = lfs(mstr1,mRB)
            if (vphs(mstr1,mRB)  >= Wtst) hph(mstr,ior)    = vphs(mstr1,mRB)
            if (colis(mstr1,mRB) >= Wtst) then
               hcoli(mstr,ior)  = colis(mstr1,mRB)
               hDOSCF(mstr,ior) = DOSCFs(mstr1,mRB)
            endif
            if (gesPs(mstr1,mRB) >= Wtst) hgesP(mstr,ior)  = gesPs(mstr1,mRB)
            if (gesNs(mstr1,mRB) >= Wtst) hgesN(mstr,ior)  = gesNs(mstr1,mRB)
            if (gsZns(mstr1,mRB) >= Wtst) hgsZn(mstr,ior)  = gsZns(mstr1,mRB)
            if (glZns(mstr1,mRB) >= Wtst) hglZn(mstr,ior)  = glZns(mstr1,mRB)
            if (gsCads(mstr1,mRB)>= Wtst) hgsCad(mstr,ior) = gsCads(mstr1,mRB)
            if (glCads(mstr1,mRB)>= Wtst) hglCad(mstr,ior) = glCads(mstr1,mRB)
            if (gsCus(mstr1,mRB) >= Wtst) hgsCu(mstr,ior)  = gsCus(mstr1,mRB)
            if (glCus(mstr1,mRB) >= Wtst) hglCu(mstr,ior)  = glCus(mstr1,mRB)
            if (gsNis(mstr1,mRB) >= Wtst) hgsNi(mstr,ior)  = gsNis(mstr1,mRB)
            if (glNis(mstr1,mRB) >= Wtst) hglNi(mstr,ior)  = glNis(mstr1,mRB)
            if (gsAss(mstr1,mRB) >= Wtst) hgsAs(mstr,ior)  = gsAss(mstr1,mRB)
            if (glAss(mstr1,mRB) >= Wtst) hglAs(mstr,ior)  = glAss(mstr1,mRB)
            if (gsPbs(mstr1,mRB) >= Wtst) hgsPb(mstr,ior)  = gsPbs(mstr1,mRB)
            if (glPbs(mstr1,mRB) >= Wtst) hglPb(mstr,ior)  = glPbs(mstr1,mRB)
            if (gsCrs(mstr1,mRB) >= Wtst) hgsCr(mstr,ior)  = gsCrs(mstr1,mRB)
            if (glCrs(mstr1,mRB) >= Wtst) hglCr(mstr,ior)  = glCrs(mstr1,mRB)
            if (gsFes(mstr1,mRB) >= Wtst) hgsFe(mstr,ior)  = gsFes(mstr1,mRB)
            if (glFes(mstr1,mRB) >= Wtst) hglFe(mstr,ior)  = glFes(mstr1,mRB)
            if (gsHgs(mstr1,mRB) >= Wtst) hgsHg(mstr,ior)  = gsHgs(mstr1,mRB)
            if (glHgs(mstr1,mRB) >= Wtst) hglHg(mstr,ior)  = glHgs(mstr1,mRB)
            if (gsMns(mstr1,mRB) >= Wtst) hgsMn(mstr,ior)  = gsMns(mstr1,mRB)
            if (glMns(mstr1,mRB) >= Wtst) hglMn(mstr,ior)  = glMns(mstr1,mRB)
            if (gsUs(mstr1,mRB)  >= Wtst) hgsU(mstr,ior)   = gsUs(mstr1,mRB)
            if (glUs(mstr1,mRB)  >= Wtst) hglU(mstr,ior)   = glUs(mstr1,mRB)
            if (iwsim == 4) cycle  ! bei Tracer wird dieser Programmteil nicht ausgeführt!
            
            algb5 = haki(mstr,ior) * Caki * bsbki  &
                  + hagr(mstr,ior) * Cagr * bsbgr  &
                  + habl(mstr,ior) * Cabl * bsbbl
                  
            hvbsb(mstr,ior) = hbsb(mstr,ior) + algb5
            zoobsb = (hzooi(mstr,ior)*GRot/1000.) * bsbZoo
            hvbsb(mstr,ior) = hvbsb(mstr,ior) + zoobsb
            
            algcs = haki(mstr,ior) * Caki * csbki  &
                  + habl(mstr,ior) * Cabl * csbbl  &
                  + hagr(mstr,ior) * Cagr * csbgr
            hvcsb(mstr,ior) = hcsb(mstr,ior)+algcs
            zoocsb = hzooi(mstr,ior) * (GROT * CZoo / 1000.) * TOC_CSB
            hvcsb(mstr,ior) = hvcsb(mstr,ior)+zoocsb
            hFluN3(mstr,ior) = 0.0
            
            do nkz = 1,hnkzs(mstr,ior)   ! Belegung des Gitters bei 2D-Modellierung, Schleifenanfang
               
               if (tempws(mstr1,mRB) > (-99.99)) htempz(mstr,nkz,ior) = tempws(mstr1,mRB)
               if (vnh4s(mstr1,mRB) >= Wtst)hnh4z(mstr,nkz,ior) = vnh4s(mstr1,mRB)
               if (vno2s(mstr1,mRB) >= Wtst)hno2z(mstr,nkz,ior) = vno2s(mstr1,mRB)
               if (vno3s(mstr1,mRB) >= Wtst)hno3z(mstr,nkz,ior) = vno3s(mstr1,mRB)
               if (vo2s(mstr1,mRB) >= Wtst)ho2z(mstr,nkz,ior) = vo2s(mstr1,mRB)
               if (gelPs(mstr1,mRB) >= Wtst)hgelPz(mstr,nkz,ior) = gelPs(mstr1,mRB)
               if (Sis(mstr1,mRB) >= Wtst)hsiz(mstr,nkz,ior) = Sis(mstr1,mRB)
               if (akis(mstr1,mRB) >= Wtst)hakiz(mstr,nkz,ior) = akis(mstr1,mRB)
               if (agrs(mstr1,mRB) >= Wtst)hagrz(mstr,nkz,ior) = agrs(mstr1,mRB)
               if (abls(mstr1,mRB) >= Wtst)hablz(mstr,nkz,ior) = abls(mstr1,mRB)
               if (chlas(mstr1,mRB) >= Wtst)hchlaz(mstr,nkz,ior) = chlas(mstr1,mRB)
               if (chlaks(mstr1,mRB) >= Wtst)hchlkz(mstr,nkz,ior) = chlaks(mstr1,mRB)
               if (chlags(mstr1,mRB) >= Wtst)hchlgz(mstr,nkz,ior) = chlags(mstr1,mRB)
               if (chlabs(mstr1,mRB) >= Wtst)hchlbz(mstr,nkz,ior) = chlabs(mstr1,mRB)
               if (gesPs(mstr1,mRB) >= Wtst)hgesPz(mstr,nkz,ior) = gesPs(mstr1,mRB)
               if (gesNs(mstr1,mRB) >= Wtst)hgesNz(mstr,nkz,ior) = gesNs(mstr1,mRB)
               if (Q_NKs(mstr1,mRB) >= Wtst)hQ_NKz(mstr,nkz,ior) = Q_NKs(mstr1,mRB)
               if (Q_NBs(mstr1,mRB) >= Wtst)hQ_NBz(mstr,nkz,ior) = Q_NBs(mstr1,mRB)
               if (Q_NGs(mstr1,mRB) >= Wtst)hQ_NGz(mstr,nkz,ior) = Q_NGs(mstr1,mRB)
               if (tempws(mstr1,mRB) > Wtst_T) then
                  hCChlkz(mstr,nkz,ior) = akbcms(mstr1,mRB)
                  hCChlbz(mstr,nkz,ior) = abbcms(mstr1,mRB)
                  hCChlgz(mstr,nkz,ior) = agbcms(mstr1,mRB)
               endif
            enddo   ! Schleifenende nkz
           
            ! Buhnenfelder
            if (nbuhn(mstr) > 0 .and. iwied == 0 ) then
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
               ! Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser
               bTsed(mstr,ior) = htempw(mstr,ior)
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
               blf(mstr,ior) = hlf(mstr,ior)
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
               bgsAs(mstr,ior) = hgsAs(mstr,ior)
               bglAs(mstr,ior) = hglAs(mstr,ior)
               bgsPb(mstr,ior) = hgsPb(mstr,ior)
               bglPb(mstr,ior) = hglPb(mstr,ior)
               bgsCr(mstr,ior) = hgsCr(mstr,ior)
               bglCr(mstr,ior) = hglCr(mstr,ior)
               bgsFe(mstr,ior) = hgsFe(mstr,ior)
               bglFe(mstr,ior) = hglFe(mstr,ior)
               bgsHg(mstr,ior) = hgsHg(mstr,ior)
               bglHg(mstr,ior) = hglHg(mstr,ior)
               bgsMn(mstr,ior) = hgsMn(mstr,ior)
               bglMn(mstr,ior) = hglMn(mstr,ior)
               bgsU(mstr,ior) = hgsU(mstr,ior)
               bglU(mstr,ior) = hglU(mstr,ior)
            endif
            
         enddo ! Ende Schleife über die Ortspunkte ior
         
         ! TODO FG: remove?
         cycle
      
      else
         ! Strang hat Vor- bzw. Nachstränge
         if (iwied == 0)ianze(mstr) = hanze(mstr)+1
         
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
         hcs113 = 0.0
         hcs114 = 0.0
         hcs115 = 0.0
         hcs116 = 0.0
         hcs117 = 0.0
         hcs118 = 0.0
         hcs119 = 0.0
         hcs120 = 0.0
         hcs121 = 0.0
         hcs122 = 0.0
         hcs123 = 0.0
         hcs124 = 0.0
         hcs125 = 0.0
         hcs126 = 0.0
         
         hcq = 0.0
         
         do nstr = 1,nstrs(istr)  !Schleife ueber die Anzahl der Vor-, bzw. Nachstraenge
            
            ! jnkz = 1 > Werte am ersten Knoten im Strang
            ! jnkz = 2 > Werte am letzten Knoten im Strang
            if (iflRi(mstr) == 0 .and. iwied == 1)cycle
            if (iflRi(ESTRNR(istr,nstr)) == 0 .and. iwied == 1)cycle
            
            kanz = ianze(ESTRNR(istr,nstr))
            iSta = mStas(ESTRNR(istr,nstr))
            iB = 1
            anzej = 1
            if (iwied == 0)anzej = hanze(mstr)+1
            jnkz = 2
            if (iFlRi(mstr) == -1 .and. iflRi(ESTRNR(istr,nstr)) == -1) then
               kanz = 1
               ista = 1
               iB = hanze(mstr)+1
               if (iwied == 0)iB = 1
               anzej = hanze(mstr)+1
               jnkz = 1
            endif
            
            if (iFlRi(mstr) == 1 .and. iflRi(ESTRNR(istr,nstr)) == -1) then
               kanz = 1
               ista = 1
               iB = 1
               anzej = 1
               if (iwied == 0)anzej = hanze(mstr)+1
               jnkz = 1
            endif
            
            if (iFlRi(mstr) == -1 .and. iflRi(ESTRNR(istr,nstr)) == 1) then
               kanz = ianze(ESTRNR(istr,nstr))
               ista = mStas(ESTRNR(istr,nstr))
               iB = hanze(mstr)+1
               anzej = hanze(mstr)+1
               if (iwied == 0)iB = 1
               jnkz = 2
            endif
            
            if (iwied == 0 .and. iwsim /= 4) then
               ! 2D-Modellierung (Gitterbelegung zu Beginn der Simulation)
               ! wird sonst übersprungen, ebenso bei Tracer
               
               kanz2 = 1
               jnkz2 = 1
               do kanz1 = 1,2
                  do nkz = 1,hnkzs(ESTRNR(istr,nstr),kanz) ! Schleife ueber die vertikalen Schichten am Stranganfang und -Ende
                     
                     Tzt(ESTRNR(istr,nstr),nkz,jnkz2) = htempz(ESTRNR(istr,nstr),1      &
                                                        ,kanz2)
                     o2zt(ESTRNR(istr,nstr),nkz,jnkz2) = ho2z(ESTRNR(istr,nstr),1       &
                                                         ,kanz2)
                     NH4zt(ESTRNR(istr,nstr),nkz,jnkz2) = hnh4z(ESTRNR(istr,nstr),1     &
                                                          ,kanz2)
                     NO2zt(ESTRNR(istr,nstr),nkz,jnkz2) = hno2z(ESTRNR(istr,nstr),1     &
                                                          ,kanz2)
                     NO3zt(ESTRNR(istr,nstr),nkz,jnkz2) = hno3z(ESTRNR(istr,nstr),1     &
                                                          ,kanz2)
                     Pzt(ESTRNR(istr,nstr),nkz,jnkz2) = hgelpz(ESTRNR(istr,nstr),1      &
                                                        ,kanz2)
                     gSizt(ESTRNR(istr,nstr),nkz,jnkz2) = hsiz(ESTRNR(istr,nstr),1      &
                                                          ,kanz2)
                     akizt(ESTRNR(istr,nstr),nkz,jnkz2) = hakiz(ESTRNR(istr,nstr),1     &
                                                          ,kanz2)
                     agrzt(ESTRNR(istr,nstr),nkz,jnkz2) = hagrz(ESTRNR(istr,nstr),1     &
                                                          ,kanz2)
                     ablzt(ESTRNR(istr,nstr),nkz,jnkz2) = hablz(ESTRNR(istr,nstr),1     &
                                                          ,kanz2)
                     chlazt(ESTRNR(istr,nstr),nkz,jnkz2) = hchlaz(ESTRNR(istr,nstr),1   &
                                                           ,kanz2)
                     chlkzt(ESTRNR(istr,nstr),nkz,jnkz2) = hchlkz(ESTRNR(istr,nstr),1   &
                                                           ,kanz2)
                     chlgzt(ESTRNR(istr,nstr),nkz,jnkz2) = hchlgz(ESTRNR(istr,nstr),1   &
                                                           ,kanz2)
                     chlbzt(ESTRNR(istr,nstr),nkz,jnkz2) = hchlbz(ESTRNR(istr,nstr),1   &
                                                           ,kanz2)
                     gesPzt(ESTRNR(istr,nstr),nkz,jnkz2) = hgesPz(ESTRNR(istr,nstr),1   &
                                                           ,kanz2)
                     gesNzt(ESTRNR(istr,nstr),nkz,jnkz2) = hgesNz(ESTRNR(istr,nstr),1   &
                                                           ,kanz2)
                     Q_NKzt(ESTRNR(istr,nstr),nkz,jnkz2) = hQ_NKz(ESTRNR(istr,nstr),1   &
                                                           ,kanz2)
                     Q_NBzt(ESTRNR(istr,nstr),nkz,jnkz2) = hQ_NBz(ESTRNR(istr,nstr),1   &
                                                           ,kanz2)
                     Q_NGzt(ESTRNR(istr,nstr),nkz,jnkz2) = hQ_NGz(ESTRNR(istr,nstr),1   &
                                                           ,kanz2)
                     CChlkzt(ESTRNR(istr,nstr),nkz,jnkz2) = hCChlkz(ESTRNR(istr,nstr),1   &
                                                            ,kanz2)
                     CChlbzt(ESTRNR(istr,nstr),nkz,jnkz2) = hCChlbz(ESTRNR(istr,nstr),1   &
                                                            ,kanz2)
                     CChlgzt(ESTRNR(istr,nstr),nkz,jnkz2) = hCChlgz(ESTRNR(istr,nstr),1   &
                                                            ,kanz2)
                  enddo  ! Schleifenende
                  jnkz2 = 2
                  kanz2 = ianze(ESTRNR(istr,nstr))
               enddo
            endif
            
            if (iwsim /= 4) then
               ! Einfluss der Wehre auf O2,pH und Temperatur,Chla,Algen,Stickstoff und Phosphor
               call WEHR(wehrh,wehrb,ho2,hQaus,O2zt,htempw,ho2_z,ho2z_z,hlf,hpw,hmw,hph,hph_z,iph                   &
                         ,tzt,hte_z,htez_z,chlazt,hchlaz_z,akizt,hakiz_z,agrzt,hagrz_z,ablzt,hablz_z                      &
                         ,NH4zt,hNH4z_z,NO2zt,hNO2z_z,NO3zt,hNO3z_z,Pzt,hPz_z,gSizt,hsiz_z,chlkzt,hchlkz_z                &
                         ,chlgzt,hchlgz_z,chlbzt,hchlbz_z,gesPzt,hgesPz_z,gesNzt,hgesNz_z,Q_NKzt,hQ_NKz_z                 &
                         ,Q_NBzt,hQ_NBz_z,Q_NGzt,hQ_NGz_z,dH2D,ESTRNR,kanz,inkzmx,iSta,nstr,istr,jnkz,iflRi,jlWO2         &
                         ,CChlkzt,hCChlkz_z,CChlbzt,hCChlbz_z,CChlgzt,hCChlgz_z,janzWS,janzWt,hnkzs,mwehr,mstr            &
                         ,WSP_UW,WSP_OW,iB)
            endif
            hcs1 = hcs1+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
                   *hsvhk(ESTRNR(istr,nstr),kanz)
            hcs2 = hcs2+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
                   *hsvhg(ESTRNR(istr,nstr),kanz)
            hcs3 = hcs3+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
                   *hsvhb(ESTRNR(istr,nstr),kanz)
            hcs6 = hcs6+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
                   *hakbcm(ESTRNR(istr,nstr),kanz)
            hcs7 = hcs7+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
                   *hagbcm(ESTRNR(istr,nstr),kanz)
            hcs8 = hcs8+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
                   *habbcm(ESTRNR(istr,nstr),kanz)
            hcs9 = hcs9+abs(hQaus(ESTRNR(istr,nstr),iSta))                    &
                   *hgesN(ESTRNR(istr,nstr),kanz)
            hcs10 = hcs10+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hgesP(ESTRNR(istr,nstr),kanz)
            hcs20 = hcs20+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hfssgr(ESTRNR(istr,nstr),kanz)
            hcs21 = hcs21+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hfbsgr(ESTRNR(istr,nstr),kanz)
            hcs22 = hcs22+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hfrfgr(ESTRNR(istr,nstr),kanz)
            hcs23 = hcs23+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hnl0(ESTRNR(istr,nstr),kanz)
            hcs24 = hcs24+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hpl0(ESTRNR(istr,nstr),kanz)
            hcs25 = hcs25+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *htempw(ESTRNR(istr,nstr),kanz)
            hcs26 = hcs26+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hbsb(ESTRNR(istr,nstr),kanz)
            hcs27 = hcs27+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hcsb(ESTRNR(istr,nstr),kanz)
            hcs28 = hcs28+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hCHNF(ESTRNR(istr,nstr),kanz)
            hcs29 = hcs29+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hBVHNF(ESTRNR(istr,nstr),kanz)
            hcs30 = hcs30+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hCD(ESTRNR(istr,nstr),1,kanz)
            hcs31 = hcs31+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hCD(ESTRNR(istr,nstr),2,kanz)
            hcs32 = hcs32+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hCP(ESTRNR(istr,nstr),1,kanz)
            hcs33 = hcs33+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hCP(ESTRNR(istr,nstr),2,kanz)
            hcs34 = hcs34+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hCM(ESTRNR(istr,nstr),kanz)
            hcs35 = hcs35+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hBAC(ESTRNR(istr,nstr),kanz)
            hcs39 = hcs39+abs(hQaus(ESTRNR(istr,nstr),iSta))                  &
                    *hnh4(ESTRNR(istr,nstr),kanz)
            if (isnan(hcs40)) then
               print*,"ho2(ESTRNR(istr,nstr),kanz),hQaus(ESTRNR(istr,nstr),iSta) = "&
                      ,ho2(ESTRNR(istr,nstr),kanz),hQaus(ESTRNR(istr,nstr),iSta)
               print*,"ESTRNR(istr,nstr),istr,nstr,kanz,iSt",ESTRNR(istr,nstr),istr,nstr,kanz,iSta
               call qerror("qsim.f90: Variable 'hcs40' became NaN.")
            endif
            
            if (isnan(ho2(ESTRNR(istr,nstr),kanz))) then
               print*,"isnanho2",ESTRNR(istr,nstr),istr,nstr,kanz,ho2(ESTRNR(istr,nstr),kanz)
               call qerror("qsim.f90: Variable 'ho2' became NaN.")
            endif
            hcs40 = hcs40+abs(hQaus(ESTRNR(istr,nstr),iSta)) * ho2(ESTRNR(istr,nstr),kanz)
            hcs41 = hcs41+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hno3(ESTRNR(istr,nstr),kanz)
            hcs42 = hcs42+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hno2(ESTRNR(istr,nstr),kanz)
            hcs43 = hcs43+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hx0(ESTRNR(istr,nstr),kanz)
            hcs44 = hcs44+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hx02(ESTRNR(istr,nstr),kanz)
            hcs45 = hcs45+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hsi(ESTRNR(istr,nstr),kanz)
            hcs46 = hcs46+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hchla(ESTRNR(istr,nstr),kanz)
            hcs47 = hcs47+abs(hQaus(ESTRNR(istr,nstr),iSta)) * haki(ESTRNR(istr,nstr),kanz)
            hcs48 = hcs48+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hagr(ESTRNR(istr,nstr),kanz)
            hcs49 = hcs49+abs(hQaus(ESTRNR(istr,nstr),iSta)) * habl(ESTRNR(istr,nstr),kanz)
            hcs50 = hcs50+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hchlak(ESTRNR(istr,nstr),kanz)
            hcs51 = hcs51+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hchlag(ESTRNR(istr,nstr),kanz)
            hcs52 = hcs52+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hchlab(ESTRNR(istr,nstr),kanz)
            hcs53 = hcs53+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hvkigr(ESTRNR(istr,nstr),kanz)
            hcs54 = hcs54+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hantbl(ESTRNR(istr,nstr),kanz)
            hcs55 = hcs55+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hssalg(ESTRNR(istr,nstr),kanz)
            hcs56 = hcs56+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hss(ESTRNR(istr,nstr),kanz)
            hcs57 = hcs57+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hzooi(ESTRNR(istr,nstr),kanz)
            hcs58 = hcs58+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hgelp(ESTRNR(istr,nstr),kanz)
            hcs59 = hcs59+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hmw(ESTRNR(istr,nstr),kanz)
            hcs60 = hcs60+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hpw(ESTRNR(istr,nstr),kanz)
            hcs61 = hcs61+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hca(ESTRNR(istr,nstr),kanz)
            hcs62 = hcs62+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hlf(ESTRNR(istr,nstr),kanz)
            
            ! pH-Wert in H+-umrechnen
            hmue = sqrt(max(0., 1.7e-5 * hlf(ESTRNR(istr,nstr),kanz)))
            hk   = 0.5 * hmue / (1. + 1.4 * hmue)
            vhplus = 10**(hk - hph(ESTRNR(istr,nstr),kanz))
            
            hcs63 = hcs63+abs(hQaus(ESTRNR(istr,nstr),iSta)) * vhplus
            hcs64 = hcs64+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hcoli(ESTRNR(istr,nstr),kanz)
            hcs100 = hcs64+abs(hQaus(ESTRNR(istr,nstr),iSta))* hDOSCF(ESTRNR(istr,nstr),kanz)
            hcs65 = hcs65+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hvbsb(ESTRNR(istr,nstr),kanz)
            hcs66 = hcs66+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hvcsb(ESTRNR(istr,nstr),kanz)
            hcs77 = hcs77+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hQ_NK(ESTRNR(istr,nstr),kanz)
            hcs78 = hcs78+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hQ_PK(ESTRNR(istr,nstr),kanz)
            hcs79 = hcs79+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hQ_SK(ESTRNR(istr,nstr),kanz)
            hcs80 = hcs80+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hQ_NG(ESTRNR(istr,nstr),kanz)
            hcs81 = hcs81+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hQ_PG(ESTRNR(istr,nstr),kanz)
            hcs82 = hcs82+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hQ_NB(ESTRNR(istr,nstr),kanz)
            hcs83 = hcs83+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hQ_PB(ESTRNR(istr,nstr),kanz)
            hcs85 = hcs85+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hSKmor(ESTRNR(istr,nstr),kanz)
            hcs86 = hcs86+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hDOSCF(ESTRNR(istr,nstr),kanz)
            hcs95 = hcs95+abs(hQaus(ESTRNR(istr,nstr),iSta)) * hFluN3(ESTRNR(istr,nstr),kanz)
            hcs99 = hcs99+abs(hQaus(ESTRNR(istr,nstr),iSta)) * TGZoo(ESTRNR(istr,nstr),kanz)
            hcs110 = hcs110+abs(hQaus(ESTRNR(istr,nstr),iSta)) * akmor_1(ESTRNR(istr,nstr),kanz)
            hcs111 = hcs111+abs(hQaus(ESTRNR(istr,nstr),iSta)) * agmor_1(ESTRNR(istr,nstr),kanz)
            hcs112 = hcs112+abs(hQaus(ESTRNR(istr,nstr),iSta)) * abmor_1(ESTRNR(istr,nstr),kanz)
            
            hcs101 = hcs101+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsZn(ESTRNR(istr,nstr),kanz)
            hcs102 = hcs102+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglZn(ESTRNR(istr,nstr),kanz)
            hcs103 = hcs103+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsCad(ESTRNR(istr,nstr),kanz)
            hcs104 = hcs104+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglCad(ESTRNR(istr,nstr),kanz)
            hcs105 = hcs105+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsCu(ESTRNR(istr,nstr),kanz)
            hcs106 = hcs106+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglCu(ESTRNR(istr,nstr),kanz)
            hcs107 = hcs107+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsNi(ESTRNR(istr,nstr),kanz)
            hcs108 = hcs108+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglNi(ESTRNR(istr,nstr),kanz)
            hcs113 = hcs113+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsAs(ESTRNR(istr,nstr),kanz)
            hcs114 = hcs114+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglAs(ESTRNR(istr,nstr),kanz)
            hcs115 = hcs115+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsPb(ESTRNR(istr,nstr),kanz)
            hcs116 = hcs116+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglPb(ESTRNR(istr,nstr),kanz)
            hcs117 = hcs117+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsCr(ESTRNR(istr,nstr),kanz)
            hcs118 = hcs118+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglCr(ESTRNR(istr,nstr),kanz)
            hcs119 = hcs119+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsFe(ESTRNR(istr,nstr),kanz)
            hcs120 = hcs120+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglFe(ESTRNR(istr,nstr),kanz)
            hcs121 = hcs121+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsHg(ESTRNR(istr,nstr),kanz)
            hcs122 = hcs122+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglHg(ESTRNR(istr,nstr),kanz)
            hcs123 = hcs123+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsMn(ESTRNR(istr,nstr),kanz)
            hcs124 = hcs124+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglMn(ESTRNR(istr,nstr),kanz)
            hcs125 = hcs125+abs(hQaus(ESTRNR(istr,nstr),iSta))*hgsU(ESTRNR(istr,nstr),kanz)
            hcs126 = hcs126+abs(hQaus(ESTRNR(istr,nstr),iSta))*hglU(ESTRNR(istr,nstr),kanz)
            
            ! 2D-Modellierung
            ! 2D-Gitterbelegung des Strangs mit Werten der Vor- bzw. Nachsträngen
            nkzs_hc = hnkzs(mstr,iB)
            nkzs_hc1 = hnkzs(ESTRNR(istr,nstr),kanz)
            if (nkzs_hc > 1 .and. nkzs_hc1 > 1) then
               i_EstRNR = ESTRNR(istr,nstr)
               call sys_gitterStrang(mstr,nkzs_hc,nkzs_hc1,dH2D,tzt,o2zt,NH4zt                                              &
                                     ,no2zt,no3zt,Pzt,gSizt,akizt,agrzt,ablzt,chlazt,chlkzt,chlgzt,chlbzt,gesPzt,gesNzt             &
                                     ,Q_NKzt, Q_NBzt, Q_NGzt, CChlkzt,CChlbzt,CChlgzt, jnkz,i_EstRNR,itags,monats,uhrz)
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
            
            ! Umspeichern der Daten am Ende des Wehrstrangs
            ! Wehrbelüftung wird am letzten Knoten des oberstromigen Strangs (OW)
            ! Berechnung erfolgt in der Subroutine <WEHR>
            ! nach Übergabe an den unterstromigen Strang (UW) müssen die Werte ohne
            ! Wehrüberfall wieder am letzten Knoten im oberstromigen Strang gesetzt werden
            ! ho2_z in hO2
            ! Bei Tracer-Berechnung werden die Wehre nicht berücksichtigt
            if (iwsim == 4)cycle
            
            htempw(ESTRNR(istr,nstr),kanz) = hte_z(ESTRNR(istr,nstr))
            ho2(ESTRNR(istr,nstr),kanz) = ho2_z(ESTRNR(istr,nstr))
            hph(ESTRNR(istr,nstr),kanz) = hph_z(ESTRNR(istr,nstr))
            
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
         
         if (hcq > 0.0) then
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
            
            ! Umrechnung von H+ in pH-Wert
            hmue = sqrt(max(0., 1.7e-5*hcs62))
            hk   = 0.5 * hmue / (1. + 1.4 * hmue)
            if (hcs63 > 0.) then
               hcs63 = hk - log10(hcs63)
            else
               hcs63 = hk
            endif
            
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
            hcs113 = hcs113/hcq
            hcs114 = hcs114/hcq
            hcs115 = hcs115/hcq
            hcs116 = hcs116/hcq
            hcs117 = hcs117/hcq
            hcs118 = hcs118/hcq
            hcs119 = hcs119/hcq
            hcs120 = hcs120/hcq
            hcs121 = hcs121/hcq
            hcs122 = hcs122/hcq
            hcs123 = hcs123/hcq
            hcs124 = hcs124/hcq
            hcs125 = hcs125/hcq
            hcs126 = hcs126/hcq
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
               ! Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser
               if (iwied == 0)hTsed(mstr,ior) = htempw(mstr,ior)
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
               ! hsised(mstr,ior) = 0.0
               ! hSKmor(mstr,ior) = 0.0
               hchla(mstr,ior) = hcs46
               haki(mstr,ior) = hcs47
               hagr(mstr,ior) = hcs48
               habl(mstr,ior) = hcs49
               hchlak(mstr,ior) = hcs50
               hchlag(mstr,ior) = hcs51
               hchlab(mstr,ior) = hcs52
               hakbcm(mstr,ior) = hcs6
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
               hgsAs(mstr,ior) = hcs113
               hglAs(mstr,ior) = hcs114
               hgsPb(mstr,ior) = hcs115
               hglPb(mstr,ior) = hcs116
               hgsCr(mstr,ior) = hcs117
               hglCr(mstr,ior) = hcs118
               hgsFe(mstr,ior) = hcs119
               hglFe(mstr,ior) = hcs120
               hgsHg(mstr,ior) = hcs121
               hglHg(mstr,ior) = hcs122
               hgsMn(mstr,ior) = hcs123
               hglMn(mstr,ior) = hcs124
               hgsU(mstr,ior) = hcs125
               hglU(mstr,ior) = hcs126
               
               ! nur Tracer
               if (iwsim == 4) cycle
               do nkz = 1,hnkzs(mstr,ior)              ! Gitterbelegung 2D
                  if (nkz > nkzs_hc) then
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
               
               ! Buhnenfelder
               if (nbuhn(mstr) == 0 .or. iwied == 1) cycle
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
               ! Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser
               if (iwied == 0) bTsed(mstr,ior) = htempw(mstr,ior)
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
               blf(mstr,ior) = hlf(mstr,ior)
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
               bgsAs(mstr,ior) = hgsAs(mstr,ior)
               bglAs(mstr,ior) = hglAs(mstr,ior)
               bgsPb(mstr,ior) = hgsPb(mstr,ior)
               bglPb(mstr,ior) = hglPb(mstr,ior)
               bgsCr(mstr,ior) = hgsCr(mstr,ior)
               bglCr(mstr,ior) = hglCr(mstr,ior)
               bgsFe(mstr,ior) = hgsFe(mstr,ior)
               bglFe(mstr,ior) = hglFe(mstr,ior)
               bgsHg(mstr,ior) = hgsHg(mstr,ior)
               bglHg(mstr,ior) = hglHg(mstr,ior)
               bgsMn(mstr,ior) = hgsMn(mstr,ior)
               bglMn(mstr,ior) = hglMn(mstr,ior)
               bgsU(mstr,ior) = hgsU(mstr,ior)
               bglU(mstr,ior) = hglU(mstr,ior)
            enddo ! Ende Schleife Neubelegung des erten oder letzten Ortspunkts eines Strangs
            
         else !  Abfluss hcq <= 0.0
            if (ilang == 0 ) then ! Vorlauf
               write(message,*)  "Strang ",mstr, " ",trim(strnumm(mstr)),  &
                 &"  ", trim(strname(mstr)), "hat weder eine Randbedingung,&
                 &noch einen zu ihm gerichteten Zufluss."
               call qerror(message)
            endif
            
         endif ! Abfluss >0.0
      endif
   
   enddo ! Ende Schleife ueber alle Straenge
   
   ! Ablegen der berechneten Werte aus dem Zeitschritt t-1 und den Randbedingungen zum Zeitpunkt
   if (jlauf == 1) goto 7777 
   
   ! ==========================================================================
   9998 continue  ! Sprungziel nach Ablegen der Werte für jeden Ortspunkt
   ! ==========================================================================
   
   
   ! Einlesen der hydraulischen Daten aus sysgenou
   write(pfadstring,'(2A)')trim(adjustl(cpfad)),'sysgenou'
   open(unit = 11, file = pfadstring, action = 'read', iostat = open_error)
   if (open_error /= 0) call qerror("Could not open sysgenou")
   rewind (11)
   
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      
      read(11,1000)hanze(mstr)
      1000 format(i4)
      do ior = 1,hanze(mstr)
         read(11,1010)hfkm(mstr,ior),hflag(mstr,ior)                                &
              ,hjiein(mstr,ior),helen(mstr,ior),hvmitt(mstr,ior)                    &
              ,htiefe(mstr,ior),hrau(mstr,ior),hrhyd(mstr,ior)                      &
              ,hSedOM(mstr,ior),hw2(mstr,ior),hBedGS(mstr,ior),hsedvvert(mstr,ior)  &
              ,hdKorn(mstr,ior),hflae(mstr,ior),hWS(mstr,ior),hischf(mstr,ior)      &
              ,hlboem(mstr,ior),hbsohl(mstr,ior),hvabfl(mstr,ior)                   &
              ,bh(mstr,ior),bf(mstr,ior),bso(mstr,ior),blb(mstr,ior)                &
              ,bleb(mstr,ior),hdl(mstr,ior),htau2(mstr,ior)                         &
              ,vbm(mstr,ior),bSedOM(mstr,ior),bw2(mstr,ior),bdKorn(mstr,ior)        &
              ,dlalph(mstr,ior)
         if (hrhyd(mstr,ior) <= 0.0)hrhyd(mstr,ior) = htiefe(mstr,ior)
         
         1010 format(f8.3,2x,i1,2x,i2                                               &
         ,2x,f9.3,2x,f7.4,2x,f5.2,2x,f5.2,2x,f5.2                                   &
         ,2x,f8.5,2x,e12.5,2x,f5.2,2x,f9.4,2x,e12.5,2x,f8.2,2x,f9.4,2x,i1,2x,f9.4   &
         ,2x,f9.4,2x,f14.6,2x,f5.2,2x,f7.2,2x,f7.2,2x,f7.2,2x                       &
         ,f7.2,2x,f7.2,2x,f7.2,2x,f7.4,2x,f8.5,2x,e12.5,2x,e12.5,2x,f6.2)
         
         
         ! falls Fliessumkehr an einer Einleiterstelle wird zur Lösung des 
         ! Dispersionsterms das McCormack-Verfahren benutzt
         if (hflag(mstr,ior) == 4 .and. hvmitt(mstr,ior) < 0.0) then
             imac(mstr) = 1
         else
            imac(mstr) = 0
         endif
         
         if (hvmitt(mstr,ior) < 0.0)imac(mstr) = 1
      enddo
      
      ! Belegung des letzten Knotens mit hydraulischen Daten
      hvabfl(mstr,hanze(mstr)+1)  = hvabfl(mstr,hanze(mstr))
      htiefe(mstr,hanze(mstr)+1)  = htiefe(mstr,hanze(mstr))
      hvmitt(mstr,hanze(mstr)+1)  = hvmitt(mstr,hanze(mstr))
      hrau(mstr,hanze(mstr)+1)    = hrau(mstr,hanze(mstr))
      hrhyd(mstr,hanze(mstr)+1)   = hrhyd(mstr,hanze(mstr))
      hflae(mstr,hanze(mstr)+1)   = hflae(mstr,hanze(mstr))
      hWS(mstr,hanze(mstr)+1)     = hWS(mstr,hanze(mstr))
      hlboem(mstr,hanze(mstr)+1)  = hlboem(mstr,hanze(mstr))
      hbsohl(mstr,hanze(mstr)+1)  = hbsohl(mstr,hanze(mstr))
      bh(mstr,hanze(mstr)+1)      = bh(mstr,hanze(mstr))
      bf(mstr,hanze(mstr)+1)      = bf(mstr,hanze(mstr))
      bso(mstr,hanze(mstr)+1)     = bso(mstr,hanze(mstr))
      blb(mstr,hanze(mstr)+1)     = blb(mstr,hanze(mstr))
      bleb(mstr,hanze(mstr)+1)    = bleb(mstr,hanze(mstr))
      hdl(mstr,hanze(mstr)+1)     = hdl(mstr,hanze(mstr))
      htau2(mstr,hanze(mstr)+1)   = htau2(mstr,hanze(mstr))
      vbm(mstr,hanze(mstr)+1)     = vbm(mstr,hanze(mstr))
      dlalph(mstr,hanze(mstr)+1)  = dlalph(mstr,hanze(mstr))
      hSedOM(mstr,hanze(mstr)+1)  = hSedOM(mstr,hanze(mstr))
      hw2(mstr,hanze(mstr)+1)     = hw2(mstr,hanze(mstr))
      hsedvvert(mstr,hanze(mstr)+1) = hsedvvert(mstr,hanze(mstr))
      hdKorn(mstr,hanze(mstr)+1)  = hdKorn(mstr,hanze(mstr))
      hBedGS(mstr,hanze(mstr)+1)  = hBedGS(mstr,hanze(mstr))
      bSedOM(mstr,hanze(mstr)+1)  = bSedOM(mstr,hanze(mstr))
      bw2(mstr,hanze(mstr)+1)     = bw2(mstr,hanze(mstr))
      bdKorn(mstr,hanze(mstr)+1)  = bdKorn(mstr,hanze(mstr))
      SPEWKSS(mstr,hanze(mstr)+1) = SPEWKSS(mstr,hanze(mstr))
      WUEBKS(mstr,hanze(mstr)+1)  = WUEBKS(mstr,hanze(mstr))
      PSREFSS(mstr,hanze(mstr)+1) = PSREFSS(mstr,hanze(mstr))
      extkS(mstr,hanze(mstr)+1)   = extkS(mstr,hanze(mstr))
      hflag(mstr,hanze(mstr)+1)   = 2
      hjiein(mstr,hanze(mstr)+1)  = 0
      
   enddo
   close(11)
   
   ! ==========================================================================
   ! Neubelegung des vertikalen Rechengitters an jedem Gitterpunkt
   ! ==========================================================================
   if (ilang == 1) then
      call sys_z_Gitter(mstra,hanze,znkzs,hnkzs,dH2D,iFlRi,htempz,ho2z,hnh4z,hno2z,hno3z               &
                        ,hgelPz,hSiz,hakiz,hagrz,hablz,hchlaz,hchlkz,hchlgz,hchlbz,hgesPz,hgesNz       &
                        ,hQ_NKz, hQ_NBz, hQ_NGz, hCChlkz,hCChlbz,hCChlgz,itags,monats)
   endif
   
   ! ==========================================================================
   ! Zeitschleife
   ! ==========================================================================
   
   ! Umrechnung der Uhrzeit in das Format <h.mm>
   Stunde = int(Uhrz)
   hcmin = (Uhrz-Stunde)*60.
   minute = nint(hcmin)
   if (minute == 60) then
      minute = 0
      Stunde = Stunde + 1
   endif
   rmin = minute/100.
   Uhrzhm = Stunde+rmin
   
   ! Bildschirmausgabe
   write(*,6163)ij,itags,monats,jahrs,Uhrzhm 
   6163 format(2x,'Zeitschritt: ',I4,2x,I0.2,'.',I0.2,'.',I4,2x,F5.2)
   
   ! TODO (schoenung, june 2022): I tried to print dates differently in the console.
   ! For some reasons this does not work well with Gerris
   !write (*,6164) '(', ij,'): ', jahrS,'-', monatS,'-', iTagS, ' ', stunde,':', minute, ':00 UTC+1'
   !6164 format(A,I0.5,A ,I4,A ,I0.2,A, I0.2,A, I0.2,A,I0.2,A)

   ! Strangschleife für Berechnung
   if (iwsim == 4) sumTracer = 0.0  ! Aufsummierung der "Tracermasse"
   
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      anze = hanze(mstr)
      iein = 0
      
      do kein = 1,ieinsh(mstr) ! Einleiter
         iein = iein+1
         einlk(iein)  = einlkh(mstr,kein)
         qeinl(iein)  = qeinlh(mstr,kein)
         ebsb(iein)   = ebsbh(mstr,kein)
         ecsb(iein)   = ecsbh(mstr,kein)
         eBVHNF(iein) = eBVHNh(mstr,kein)
         eCHNF(iein)  = eCHNFh(mstr,kein)
         enh4(iein)   = enh4h(mstr,kein)
         ex0(iein)    = ex0h(mstr,kein)
         ex02(iein)   = ex02h(mstr,kein)
         eo2(iein)    = eo2h(mstr,kein)
         etemp(iein)  = etemph(mstr,kein)
         echla(iein)  = echlah(mstr,kein)
         ezind(iein)  = ezindh(mstr,kein)
         ep(iein)     = egph(mstr,kein)
         esi(iein)    = esih(mstr,kein)
         eno2(iein)   = eno2h(mstr,kein)
         eno3(iein)   = eno3h(mstr,kein)
         egesN(iein)  = egesNh(mstr,kein)
         egesP(iein)  = egesPh(mstr,kein)
         ess(iein)    = essh(mstr,kein)
         ewaerm(iein) = ewaerh(mstr,kein)
         typ(iein)    = typh(mstr,kein)
         eph(iein)    = ephh(mstr,kein)
         emw(iein)    = emwh(mstr,kein)
         elf(iein)    = elfh(mstr,kein)
         eca(iein)    = ecah(mstr,kein)
         ecoli(iein)  = ecolih(mstr,kein)
         evkigr(iein) = evkgh(mstr,kein)
         eantbl(iein) = eantbh(mstr,kein)
         enl0(iein)   = enl0h(mstr,kein)
         epl0(iein)   = epl0h(mstr,kein)
      enddo
      
      ieinL = 0
      
      ! Linienquellen
      do kein = 1,ieinLs(mstr)  
         ieinL = ieinL+1
         qeinlL(ieinL) = qLh(mstr,kein)
         bsbL(ieinL)   = bsbLh(mstr,kein)
         csbL(ieinL)   = csbLh(mstr,kein)
         enh4L(ieinL)  = enh4Lh(mstr,kein)
         x0L(ieinL)    = x0Lh(mstr,kein)
         x02L(ieinL)   = x02Lh(mstr,kein)
         o2L(ieinL)    = o2Lh(mstr,kein)
         etempL(ieinL) = tempLh(mstr,kein)
         gpL(ieinL)    = gpLh(mstr,kein)
         siL(ieinL)    = siLh(mstr,kein)
         eno2L(ieinL)  = eno2Lh(mstr,kein)
         eno3L(ieinL)  = eno3Lh(mstr,kein)
         gesNL(ieinL)  = gesNLh(mstr,kein)
         gesPL(ieinL)  = gesPLh(mstr,kein)
         ssL(ieinL)    = ssLh(mstr,kein)
         phL(ieinL)    = phLh(mstr,kein)
         elfL(ieinL)   = elfLh(mstr,kein)
         caL(ieinL)    = caLh(mstr,kein)
         coliL(ieinL)  = coliLh(mstr,kein)
         enl0L(ieinL)  = enl0Lh(mstr,kein)
         pl0L(ieinL)   = pl0Lh(mstr,kein)
         chlaL(ieinL)  = chlaLh(mstr,kein)
         
         iorLa(ieinL)  = iorLah(mstr,kein)
         iorLe(ieinL)  = iorLeh(mstr,kein)
      enddo
      
      do ior = 1, anze + 1
         if (isnan(ho2(mstr,ior))) then
             print*,"qsim Linienquellen - ho2 is NaN - mstr,ior,vo2(ior),ho2(mstr,ior): ",mstr,ior,vo2(ior),ho2(mstr,ior)
         endif
      enddo
      
      ! Kenngrößen für Pflanzen- und Dreissenawachstum
      ! (nur strangweise nicht abschnittsweise)
      itstart = itsts(mstr)
      mstart  = msts(mstr)
      itmax   = itmaxs(mstr)
      mmax    = mmaxs(mstr)
      itend   = itends(mstr)
      mend    = mends(mstr)
      lait1   = laits(mstr)
      laim1   = laims(mstr)
      laid1   = laids(mstr)
      
      do ior = 1,anze+1
         svhemk(ior) = hsvhk(mstr,ior)
         svhemg(ior) = hsvhg(mstr,ior)
         svhemb(ior) = hsvhb(mstr,ior)
         DOSCF(ior)  = hDOSCF(mstr,ior)
         akbcm(ior)  = hakbcm(mstr,ior)
         agbcm(ior)  = hagbcm(mstr,ior)
         abbcm(ior)  = habbcm(mstr,ior)
         fssgr(ior)  = hfssgr(mstr,ior)
         fbsgr(ior)  = hfbsgr(mstr,ior)
         frfgr(ior)  = hfrfgr(mstr,ior)
         nl0(ior)    = hnl0(mstr,ior)
         pl0(ior)    = hpl0(mstr,ior)
         gesN(ior)   = hgesN(mstr,ior)
         gesP(ior)   = hgesP(mstr,ior)
         Q_NK(ior)   = hQ_NK(mstr,ior)
         Q_PK(ior)   = hQ_PK(mstr,ior)
         Q_SK(ior)   = hQ_SK(mstr,ior)
         Q_NG(ior)   = hQ_NG(mstr,ior)
         Q_PG(ior)   = hQ_PG(mstr,ior)
         Q_NB(ior)   = hQ_NB(mstr,ior)
         Q_PB(ior)   = hQ_PB(mstr,ior)
         tempw(ior)  = htempw(mstr,ior)
         Tsed(ior)   = hTsed(mstr,ior)
         obsb(ior)   = hbsb(mstr,ior)
         ocsb(ior)   = hcsb(mstr,ior)
         
         CHNF(ior)   = hCHNF(mstr,ior)
         BVHNF(ior)  = hBVHNF(mstr,ior)
         CD(1,ior)   = hCD(mstr,1,ior)
         CD(2,ior)   = hCD(mstr,2,ior)
         CP(1,ior)   = hCP(mstr,1,ior)
         CP(2,ior)   = hCP(mstr,2,ior)
         CM(ior)     = hCM(mstr,ior)
         BAC(ior)    = hBAC(mstr,ior)
         
         vnh4(ior)   = hnh4(mstr,ior)
         vo2(ior)    = ho2(mstr,ior)
         vno3(ior)   = hno3(mstr,ior)
         vno2(ior)   = hno2(mstr,ior)
         vx0(ior)    = hx0(mstr,ior)
         vx02(ior)   = hx02(mstr,ior)
         si(ior)     = hsi(mstr,ior)
         sised(ior)  = hsised(mstr,ior)
         SKmor(ior)  = hSKmor(mstr,ior)
         chla(ior)   = hchla(mstr,ior)
         aki(ior)    = haki(mstr,ior)
         agr(ior)    = hagr(mstr,ior)
         abl(ior)    = habl(mstr,ior)
         chlaki(ior) = hchlak(mstr,ior)
         chlagr(ior) = hchlag(mstr,ior)
         chlabl(ior) = hchlab(mstr,ior)
         vkigr(ior)  = hvkigr(mstr,ior)
         antbl(ior)  = hantbl(mstr,ior)
         abrzo1(ior) = habrz1(mstr,ior)
         ssalg(ior)  = hssalg(mstr,ior)
         ss(ior)     = hss(mstr,ior)
         zooind(ior) = hzooi(mstr,ior)
         gelp(ior)   = hgelp(mstr,ior)
         mw(ior)     = hmw(mstr,ior)
         pw(ior)     = hpw(mstr,ior)
         ca(ior)     = hca(mstr,ior)
         lf(ior)     = hlf(mstr,ior)
         vph(ior)    = hph(mstr,ior)
         dlarvn(ior) = hdlarn(mstr,ior)
         vbsb(ior)   = hvbsb(mstr,ior)
         vcsb(ior)   = hvcsb(mstr,ior)
         stind(ior)  = hstind(mstr,ior)
         coli(ior)   = hcoli(mstr,ior)
         DOSCF(ior)  = hDOSCF(mstr,ior)
         jiein(ior)  = hjiein(mstr,ior)
         Dz2D(ior)   = hDz2D(mstr,ior)
         sedalg(ior) = hsedag(mstr,ior)
         sedalk(ior) = hsedak(mstr,ior)
         sedalb(ior) = hsedab(mstr,ior)
         
         ! nur Tracer
         if (iwsim == 4)cycle
         
         ! 2D-Modellierung
         nkzs(ior) = hnkzs(mstr,ior)
         dH2De(ior) = hdH2De(mstr,ior)
         do nkz = 1,nkzs(ior)
            tempwz(nkz,ior) = htempz(mstr,nkz,ior)
            vnh4z(nkz,ior)  = hnh4z(mstr,nkz,ior)
            vno2z(nkz,ior)  = hno2z(mstr,nkz,ior)
            vno3z(nkz,ior)  = hno3z(mstr,nkz,ior)
            vo2z(nkz,ior)   = ho2z(mstr,nkz,ior)
            gelPz(nkz,ior)  = hgelPz(mstr,nkz,ior)
            Siz(nkz,ior)    = hSiz(mstr,nkz,ior)
            akiz(nkz,ior)   = hakiz(mstr,nkz,ior)
            agrz(nkz,ior)   = hagrz(mstr,nkz,ior)
            ablz(nkz,ior)   = hablz(mstr,nkz,ior)
            chlaz(nkz,ior)  = hchlaz(mstr,nkz,ior)
         enddo
         
         pflmin(ior) = hpfmnl(mstr,ior)
         pflmax(ior) = hpfmxl(mstr,ior)
         ischif(ior) = hischf(mstr,ior)
         do ndr = 1,nndr
            zdrei(ior,ndr)  = hzdrel(mstr,ior,ndr)
            zdreis(ior,ndr) = hzdrsl(mstr,ior,ndr)
            gewdr(ior,ndr)  = hgwdrl(mstr,ior,ndr)
         enddo
         dlmax(ior)  = hdlmx(mstr,ior)
         dlmaxs(ior) = hdlmxs(mstr,ior)
         gwdmax(ior) = hgwdmx(mstr,ior)
         sgwmue(ior) = hsgwmu(mstr,ior)
         abegm2(ior) = habgml(mstr,ior)
         abekm2(ior) = habkml(mstr,ior)
         coro(ior,1:5)  = hcoro2(mstr,ior,1:5)
         coros(ior,1:5) = hcos2(mstr,ior,1:5)
         
         ! Ufervegetation
         VTYP(ior,1:14) = VTYPH(mstr,ior,1:14)
         VALTBL(ior) = VALTLH(mstr,ior)
         EDUFBL(ior) = EDUFLH(mstr,ior)
         VALTBR(ior) = VALTRH(mstr,ior)
         EDUFBR(ior) = EDUFRH(mstr,ior)
         
         !if(ieros>0)then
         !  sedh(ior) = sedhg(mstr,ior)
         !  ischic(ior) = ischig(mstr,ior)
         !  do itau = 1,ischic(ior)
         !    tausc(ior,itau) = tauscg(mstr,ior,itau)
         !  enddo
         !endif
         
      enddo !ior=1,anze+1
      
      dtmin = Strdt(mstr)
      dtmin_Mac = Strdt(mstr)
      do ior = 1,anze+1
         fkm(ior)   = hfkm(mstr,ior)
         flag(ior)  = hflag(mstr,ior)
         jiein(ior) = hjiein(mstr,ior)
         elen(ior)  = helen(mstr,ior)
         if (ior <= anze)vmitt(ior) = (abs(hvmitt(mstr,ior))+abs(hvmitt(mstr,ior+1)))/2.
         if (ior == (anze+1))vmitt(ior) = abs(vmitt(anze))
         tiefe(ior) = htiefe(mstr,ior)
         rau(ior)   = hrau(mstr,ior)
         rhyd(ior)  = hrhyd(mstr,ior)
         flae(ior)  = hflae(mstr,ior)
         lboem(ior) = hlboem(mstr,ior)
         if (lboem(ior) <= 0.0)lboem(ior) = 0.0000001
         bsohlm(ior) = hbsohl(mstr,ior)
         vabfl(ior)  = hvabfl(mstr,ior)
         
         if (nbuhn(mstr) > 0) tau2(ior) = 1. / (htau2(mstr,ior)*3600.)
         
         ! Berechnung des Abschnittsvolumens vol(ior)
         elenl = elen(ior)
         if (elenl <= 0.01)elenl = 0.0
         vol(ior) = flae(ior)*elenl
         
         ! --------------------------------------------------------------------
         ! Berechnung des longitudinalen Dispersionskoeffizienten
         ! --------------------------------------------------------------------
         ! falls idl = 1 wird der Koeffizient eingelesen, andernfalls wird er berechnet
         if (idl == 1 .and. hdl(mstr,ior) > 0.0) then
            dl(ior) = hdl(mstr,ior)
         
         else 
            ust = (((1./rau(ior))*9.81**0.5)/(tiefe(ior)**0.16667))*abs(vmitt(ior))
            if (ust == 0.0)ust = 1.e-10
            breite(ior) = flae(ior)/tiefe(Ior)
            
            select case(iLongDis)
                case(1) ! Deng
                  alpha = 1.67
                  if (dlalph(mstr,ior) > 0.0)alpha = dlalph(mstr,ior)
               
                  lat_K = 0.145+(1./3520.)*(abs(vmitt(ior))/Ust)*(Breite(ior)/Tiefe(ior))**1.38
                  DL(ior) = (0.15/(8.*lat_K))*(abs(vmitt(ior))/Ust)**2*(Breite(ior)/Tiefe(ior))**alpha*Tiefe(ior)*ust
                  DL(Ior) = DL(ior)*FlongDis
               
               case(2) ! Li et al. (1998)
                  alpha = 1.3
                  if (dlalph(mstr,ior) > 0.0)alpha = dlalph(mstr,ior)
                  dl(ior) = 0.2*(Breite(ior)/Tiefe(ior))**alpha*(abs(vmitt(ior))/Ust)**1.2*Tiefe(ior)*Ust
                  dl(ior) = DL(ior)*FlongDis
               
               case(3) ! Iwasa and Aya (1991)
                  alpha = 1.5
                  if (dlalph(mstr,ior) > 0.0)alpha = dlalph(mstr,ior)
                  DL(ior) = 2.*(Breite(ior)/Tiefe(ior))**alpha*Tiefe(ior)*ust
                  DL(ior) = DL(Ior)*FlongDis
               
               case(4) ! Elder (1959)
                  DL(ior) = 5.93 * tiefe(ior) * ust
                  DL(Ior) = DL(ior) * FlongDis
            end select
         endif
         
         Cr_zahl = 5.
         Cr_zahl_Mac = 0.48
         if (iverfahren > 1)imac(mstr) = 1
         if (imac(mstr) == 1)Cr_zahl = Cr_zahl_Mac
         if (ior > 1) then
            hc_alpha = elen(ior)/elen(ior-1)
            hc_dl = (DL(ior-1)+DL(ior))/2.
         else
            hc_alpha = 1.
            hc_DL = DL(ior)
         endif
         dtneu = 0.5*hc_alpha*(1.+hc_alpha)*elen(ior)**2*Cr_zahl/hc_DL
         dtneu_Mac = 0.4*hc_alpha*(1.+hc_alpha)*elen(ior)**2*Cr_zahl_Mac/hc_DL
         if (dtneu < dtmin .and. ior <= anze)dtmin = dtneu
         if (dtneu_Mac < dtmin_Mac .and. ior <= anze)dtmin_Mac = dtneu_Mac
      enddo
      ! flag(anze+1) = 0
      
      iSTRiz_neu = int((tflie*86400.)/dtmin)+1
      ! Strdt(mstr) = tflie*86400./StRiz(mstr)
      isub_dt(mstr) = (iStriz_neu/Striz(mstr))+1
      if (isub_dt(mstr) == 0)isub_dt(mstr) = 1
      
      iSTRiz_neu = int((tflie*86400.)/dtmin_Mac)+1
      isub_dt_Mac(mstr) = (iStriz_neu/Striz(mstr))+1
      if (isub_dt_Mac(mstr) == 0)isub_dt_Mac(mstr) = 1
      
      
      ! =======================================================================
      ! metabolism
      ! =======================================================================
      
      if (iwsim /= 4 .and. iwsim /= 5) then
         call strahlg(glob,uhrz,sa,su,schwi,tflie,geol,tdj,geob,dk,cloud,schwia,imet,mstr,IDWe,itags,monats,VTYP         &
                     ,VALTBL,EDUFBL,VALTBR,EDUFBR,breite,anze,it_h,ij,jahrs,itage,monate,jahre,uhren        &
                     ,isim_end,azStr)
         call temperl(sa,su,uhrz,templ,mstr,idwe,tlmax,tlmin,anze,imet)
      endif
      
      ! Berechnung der Austauschraten zwischen Hauptstrom und Buhnenfelder
      hctau1 = 0.
      hctau2 = 0.
      if (nbuhn(mstr) > 0) then
         do ior = 1,anze+1
            if (tau2(ior) > 0.) then
               hctau1(ior) = 1. - exp(-tau2(ior) * tflie * 86400. * bf(mstr,ior) / flae(ior))
               hctau2(ior) = 1. - exp(-tau2(ior) * tflie * 86400.)
            endif
         enddo
      endif
      
      ! -----------------------------------------------------------------------
      ! Sediment-Stofffluxe
      ! -----------------------------------------------------------------------
      if (iwsim == 2 .or. iwsim == 5 .or. iwsim == 4) goto 113
      
      1712 continue
      
      if (mitsedflux) then
         call sedflux(tiefe,vmitt,rau,sedAlg_MQ,hSedOM,hw2,hBedGS,hsedvvert,hdKorn,vO2,vNO3,vNH4,gelP      &
                      ,Tempw,anze,mstr,hJNO3,hJNH4,hJPO4,hJO2,hJN2,sedalk,sedalg                           &
                      ,sedalb,sedSS_MQ,KNH4,KapN3,tflie,ilbuhn,itags,monats,uhrz,vo2z                      &
                      ,vnh4z,vno3z,gelpz,nkzs,SorpCap,Klang,KdNh3,fPOC1,fPOC2                              &
                      ,orgCsd_abb,hCD,JDOC1,JDOC2,Q_NK,Q_PK,Q_NG,Q_PG,Q_NB,Q_PB,pl0,nl0,Si,hSised,hJSi     &
                      ,aki,agr,abl,Chlaki,Chlagr,Chlabl,hFluN3,ilang,iwied,YNMAX1,STKS1,obsb,ocsb   &
                      ,.false., 0)
      else 
         ! without sedflux(), fluxes need to be set zero
         hJNO3(:,:) = 0.0
         hJNH4(:,:) = 0.0
         hJPO4(:,:) = 0.0
         hJO2(:,:) = 0.0
         hJSi(:,:) = 0.0
         hJN2(:,:) = 0.0
         JDOC1(:) = 0.0
         JDOC2(:) = 0.0
      endif 
      
      if (nbuhn(mstr) == 0) goto 1612
      if (ilbuhn == 0) then
         do ior = 1,anze+1
            zww2(ior)    = hw2(mstr,ior)
            zwSdOM(ior)  = hSedOM(mstr,ior)
            zwKorn(ior)  = hdKorn(mstr,ior)
            zwtemp(ior)  = tempw(ior)
            zwvm(ior)    = vmitt(ior)
            zwtief(ior)  = tiefe(ior)
            zwvo2(ior)   = vo2(ior)
            zwno3(ior)   = vno3(ior)
            zwnh4(ior)   = vnh4(ior)
            zwgelp(ior)  = gelp(ior)
            zwsi(ior)    = si(ior)
            zwobsb(ior)  = obsb(ior)
            zwocsb(ior)  = ocsb(ior)
            zwJNO3(ior)  = hJNO3(mstr,ior)
            zwJNH4(ior)  = hJNH4(mstr,ior)
            zwJPO4(ior)  = hJPO4(mstr,ior)
            zwJO2(ior)   = hJO2(mstr,ior)
            zwJN2(ior)   = hJN2(mstr,ior)
            zwJSi(ior)   = hJSi(mstr,ior)
            zwsedS(ior)  = sedss(ior)
            zwCsed(ior)  = orgCsd(mstr,ior)
            zwCsed_abb(ior) = orgCsd_abb(mstr,ior)
            zwsedk(ior)  = sedalk(ior)
            zwsedg(ior)  = sedalg(ior)
            zwsedb(ior)  = sedalb(ior)
            zwnkzs(ior)  = nkzs(ior)
            zQ_PK(ior)   = Q_PK(ior)
            zQ_NK(ior)   = Q_NK(ior)
            zQ_PG(ior)   = Q_PG(ior)
            zQ_NG(ior)   = Q_NG(ior)
            zQ_PB(ior)   = Q_PB(ior)
            zQ_NB(ior)   = Q_NB(ior)
            zwpl0(ior)   = pl0(ior)
            zwnl0(ior)   = nl0(ior)
            zwcd(1,ior)  = CD(1,ior)
            zwcd(2,ior)  = CD(2,ior)
            zwJDOC1(ior) = JDOC1(ior)
            zwJDOC2(ior) = JDOC2(ior)
            zwsedAlg_MQ(ior) = sedAlg_MQ(mstr,ior)
            zwsedSS_MQ(ior)  = sedSS_MQ(mstr,ior)
            zwSisd(ior) = hSised(mstr,ior)
            zwFlN3(ior) = hFluN3(mstr,ior)
                        
            hw2(mstr,ior) = bw2(mstr,ior)
            hSedOM(mstr,ior) = bSedOM(mstr,ior)
            hdKorn(mstr,ior) = bdKorn(mstr,ior)
            tempw(ior)  = btempw(mstr,ior)
            vmitt(ior)  = vbm(mstr,ior)
            tiefe(ior)  = bh(mstr,ior)
            vo2(ior)    = bo2(mstr,ior)
            vno3(ior)   = bno3(mstr,ior)
            vnh4(ior)   = bnh4(mstr,ior)
            gelp(ior)   = bgelp(mstr,ior)
            Si(ior)     = bSi(mstr,ior)
            obsb(ior)   = bbsb(mstr,ior)
            ocsb(ior)   = bcsb(mstr,ior)
            sedSS(ior)  = bsedSS(mstr,ior)
            sedalk(ior) = bsedak(mstr,ior)
            sedalg(ior) = bsedag(mstr,ior)
            sedalb(ior) = bsedab(mstr,ior)
            orgCsd(mstr,ior)     = borgCs(mstr,ior)
            orgCsd_abb(mstr,ior) = borgCs_abb(mstr,ior)
            Q_PK(ior)  = bQ_PK(mstr,ior)
            Q_NK(ior)  = bQ_NK(mstr,ior)
            Q_PG(ior)  = bQ_PG(mstr,ior)
            Q_NG(ior)  = bQ_NG(mstr,ior)
            Q_PB(ior)  = bQ_PB(mstr,ior)
            Q_NB(ior)  = bQ_NB(mstr,ior)
            pl0(ior)   = bpl0(mstr,ior)
            nl0(ior)   = bnl0(mstr,ior)
            CD(1,ior)  = bCD(mstr,1,ior)
            CD(2,ior)  = bCD(mstr,2,ior)
            JDOC1(ior) = bJDOC1(ior)
            JDOC2(ior) = bJDOC2(ior)
            hFluN3(mstr,ior)    = bFluN3(mstr,ior)
            sedAlg_MQ(mstr,ior) = bsedAlg_MQ(mstr,ior)
            sedSS_MQ(mstr,ior)  = bsedSS_MQ(mstr,ior)
            hSised(mstr,ior)    = bSised(mstr,ior)
            nkzs(ior) = 1
         enddo
         
         ilbuhn = 1
         goto 1712
      endif
      
      if (ilbuhn == 1) then
         do  ior = 1,anze+1
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
         enddo
         ilbuhn = 0
      endif
      
      
      ! -----------------------------------------------------------------------
      ! Rotatorien
      ! -----------------------------------------------------------------------
      1612  continue
      call konsum(vkigr,TEMPW,VO2,TFLIE                                          &
                  ,ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl          &
                  ,jiein,FOPTR,GROT,dzres1,dzres2,ZRESG                          &
                  ,irmax,zexki,zexgr,zexbl                                       &
                  ,aki,agr,abl,iwied,rmuas,iras,TGZoo,BAC,zBAC                   &
                  ,rakr,rbar,CHNF,zHNF,ilbuhn,ZAKI,ZAGR,ZABL,HNFza,algzok        &
                  ,algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats &
                  ,itags,uhrz,mstr, .false., 0)
      
      if (nbuhn(mstr) == 0 ) goto 1415
      if (ilbuhn == 0) then
         do ior = 1,anze+1
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
            if (iwied == 1)TGZoo(mstr,ior) = bTGZoo(mstr,ior)
         enddo
         ilbuhn = 1
         goto 1612
      endif
      
      if (ilbuhn == 1) then
         do ior = 1,anze+1
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
            
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) then
               diff1 = bzooi(mstr,ior)  - zooind(ior)
               diff2 = bTGZoo(mstr,ior) - TGZoo(mstr,ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               zooind(ior)     = zooind(ior)     + diff1 * hctau1(ior)
               TGZoo(mstr,ior) = TGZoo(mstr,ior) + diff2 * hctau1(ior)
            endif
            
            if (hctau2(ior) > 0.0) then
               bzooi(mstr,ior)  = bzooi(mstr,ior)  - diff1 * hctau2(ior)
               bTGZoo(mstr,ior) = bTGZoo(mstr,ior) - diff2 * hctau2(ior)
            endif
         enddo
         
         ilbuhn = 0
      endif
      
      ! -----------------------------------------------------------------------
      !  Chorophium
      ! -----------------------------------------------------------------------
      1415 continue
      
      ! if (nbuhn(mstr) == 0)goto 1440
      ! do ior = 1,anze+1
      !    do jC = 1,5
      !       zwcoro(ior,jC) = coro(ior,jC)
      !       Coro(ior,jC) = 0.0
      !    enddo
      ! enddo
      ! 
      ! 1440 continue
      ! call coroph(coro,coros,tempw,flae,elen,anze,ior                                  &
      !            ,volfco,aki,agr,algcok,algcog,tflie,bsohlm,lboem,coroI                &
      !            ,coroIs,abl,algcob,mstr,itags,monats,jahrs,ilang,nbuhn,ilbuhn,        &
      !            .false., 0)
      !
      coro(:,:)  = 0.0
      coros(:,:) = 0.0
      volfco(:)  = 0.0
      algcok(:)  = 0.0
      algcog(:)  = 0.0
      coroI(:)   = 0.0
      coroIs(:)  = 0.0
      algcob(:)  = 0.0
      
      if (nbuhn(mstr) > 0) then
         bacok(mstr,:) = 0.0
         bacog(mstr,:) = 0.0
         bacob(mstr,:) = 0.0
      endif
      
      ! if (nbuhn(mstr) == 0)goto 1441
      ! if (ilbuhn == 0) then
      !    do ior = 1,anze+1
      !       zwtemp(ior) = tempw(ior)
      !       zwflae(ior) = flae(ior)
      !       zwlboe(ior) = lboem(ior)
      !       zwaki(ior) = aki(ior)
      !       zwagr(ior) = agr(ior)
      !       zwabl(ior) = abl(ior)
      !       zwacok(ior) = algcok(ior)
      !       zwacog(ior) = algcog(ior)
      !       zwacob(ior) = algcob(ior)
      !       zwCoIs(ior) = coroIs(ior)
      !       zwcors(ior,1:5) = coros(ior,1:5)
      !       Coros(ior,1:5) = 0.0
      !       coro(ior,1:5) = zwcoro(ior,1:5)
      !
      !       flae(ior) = bf(mstr,ior)
      !       lboem(ior) = blb(mstr,ior)
      !       tempw(ior) = btempw(mstr,ior)
      !       aki(ior) = baki(mstr,ior)
      !       agr(ior) = bagr(mstr,ior)
      !       abl(ior) = babl(mstr,ior)
      !    enddo
      !    ilbuhn = 1
      !    goto 1440
      ! endif
      
      ! if (ilbuhn == 1) then
      !    do ior = 1,anze+1
      !       bacok(mstr,ior) = algcok(ior)
      !       bacog(mstr,ior) = algcog(ior)
      !       bacob(mstr,ior) = algcob(ior)
      ! 
      !       flae(ior) = zwflae(ior)
      !       lboem(ior) = zwlboe(ior)
      !       tempw(ior) = zwtemp(ior)
      !       CoroIs(ior) = zwCoIs(ior)
      !       coros(ior,1:5) = zwcors(ior,1:5)
      !       aki(ior) = zwaki(ior)
      !       agr(ior) = zwagr(ior)
      !       abl(ior) = zwabl(ior)
      !       algcok(ior) = zwacok(ior)
      !       algcog(ior) = zwacog(ior)
      !       algcob(ior) = zwacob(ior)
      !    enddo
      !    ilbuhn = 0
      ! endif
      
      ! -----------------------------------------------------------------------
      ! Dreissena
      ! -----------------------------------------------------------------------
      1441 continue
      
      if (nbuhn(mstr) > 0) then ! Annahme: Dreissena nur im Buhnenfeld
         do ior = 1,anze+1
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
                    ,ior,volfdr,akbcm,agbcm,aki,agr,algdrk,algdrg       &
                    ,tflie,ro2dr,lboem,bsohlm,ss,vo2,ssdr,drfaek        &
                    ,drfaeg,drfaes,gewdr,dlarvn,itags,monats,jahrs      &
                    ,lait1,laim1,laid1,ilang                            &
                    ,resdr,exdrvg,exdrvk,ssalg,drpfec                   &
                    ,abl,exdrvb,abbcm,algdrb,drfaeb                     &
                    ,idras,drmas,drakr,drbar,drmor,ffood,coroI,coroIs   &
                    ,CHNF,drHNF,HNFdra,dlmax,dlmaxs,gwdmax              &
                    ,sgwmue,fkm,FoptD,mstr,azStr,                       &
                    .false., 0)
      
      if (nbuhn(mstr) > 0) then
         do ior = 1,anze+1
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
      
      ! -----------------------------------------------------------------------
      ! heterotrophe Nanoflagelaten (HNF)
      ! [ausgeschaltet]
      ! -----------------------------------------------------------------------
      218 continue
      !if (CHNF(1) <= 0.0) then
      !   if (nbuhn(mstr) > 0) then
      !      do ior = 1,anze+1
      !         bro2HF(mstr,ior) = 0.0
      !         bHNFBS(mstr,ior) = 0.0
      !         bBSBHN(mstr,ior) = 0.0
      !      enddo
      !   endif
      !   goto 1412
      !endif
      
      ! call HNF(CHNF,BVHNF,BAC,TEMPW,VO2,TFLIE                            &
      !         ,echnf,eBVHNF,flag,elen,ior,anze,qeinl,vabfl               &
      !         ,jiein,drHNF,zHNF,HNFBAC,rO2HNF,BSBHNF,HNFmua,upHNF,BACks  &
      !         ,HNFrea,HNFupa,HNFmoa,HNFexa,fkm,mstr,itags,monats,uhrz,   &
      !          .false., 0)
      HNFmua = 0.
      HNFrea = 0.
      HNFupa = 0.
      HNFmoa = 0.
      HNFexa = 0.
      HNFbac = 0.
      rO2HNF = 0.
      bsbHNF = 0.
      
      if (nbuhn(mstr) > 0) then
         bro2HF(mstr,:) = 0.
         bHNFBS(mstr,:) = 0.
         bBSBHN(mstr,:) = 0.
      endif
      
      ! -----------------------------------------------------------------------
      ! Kieselalgen
      ! -----------------------------------------------------------------------
      1412 continue
      ! if(hChla(mstr,1)<0.0)goto 1513
      call algaeski(SCHWI,TFLIE,TEMPW,tempwz,RAU,TIEFE,VMITT,flae,VNO3,VNH4,GELP,svhemk,svhemb,svhemg,CHLA,ir                 &
                    ,SI,dalgki,dalgak,flag,elen,ior,anze,sedalk,algzok,echla,qeinl,vabfl                                      &
                    ,dkimor,fkm,jiein,evkigr,vkigr,antbl,eantbl,akchl,akgmax,akksn,akksp,akkssi,saettk,akremi,akrema          &
                    ,sbioki,vco2,iph,akbcm,abbcm,agbcm,aki,abl,agr,extk,extk_lamda                                            &
                    ,ilamda,eta,aw,ack,acg,acb,ah,as,al                                                                       & 
                    ,uhrz,sised,tpki,iwied,akmuea,ftaaus,fiaus,fheaus                                                         &
                    ,akraus,tausc,ischif,ilbuhn,ieros,askie,cmatki,algdrk,algcok,ess,zooind,GROT,SS,Q_PK,Q_NK,Q_SK            &
                    ,vNH4z,vNO3z,gelPz,Siz,dalgkz,nkzs,dH2D,cpfad,up_PKz,up_NKz,up_Siz,Qmx_PK,Qmn_PK,upmxPK                   &
                    ,Qmx_NK,Qmn_NK,upmxNK,Qmx_SK,Qmn_SK,upmxSK,SKmor,IKke,frmuke,alamda,akitbr,chlaz,akibrz,akiz,chlaL,qeinlL &
                    ,ieinLs,algakz,algzkz,ablz,agrz,Chlaki,hchlkz,hchlgz,hchlbz,hCChlkz,hCChlbz,hCChlgz,Dz2D,ToptK,kTemp_Ki   &
                    ,ifix,Chlabl,Chlagr,a1Ki,a2Ki,a3Ki,sedAlg_MQ,sedAlk0,hQ_NKz,hQ_NGz,hQ_NBz,Q_PG,Q_NG,Q_PB,Q_NB             &
                    ,mstr,it_h,itags,monats,isim_end,extkS,akmor_1,agmor_1,abmor_1                                            &
                    ,.false.,0)
      
      if (nbuhn(mstr) == 0)goto 1413
      if (ilbuhn == 0) then
         do ior = 1,anze+1
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
      
      if (ilbuhn == 1) then
         do ior = 1,anze+1
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
            
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.0) then
               diff1 = bsvhek(mstr,ior) - svhemk(ior)
               diff2 = bSKmor(mstr,ior) - SKmor(ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               svhemk(ior) = max(0., svhemk(ior) + diff1 * hctau1(ior))
               SKmor(ior)  = max(0., SKmor(ior)  + diff2 * hctau1(ior))
            endif
            
            if (hctau2(ior) > 0.0) then
               bsvhek(mstr,ior) = max(0., bsvhek(mstr,ior) - diff1 * hctau2(ior))
               bSKmor(mstr,ior) = max(0., bSKmor(mstr,ior) - diff2 * hctau2(ior))
            endif
         enddo
         
         ilbuhn = 0
      endif
      
      ! -----------------------------------------------------------------------
      ! Blaualgen
      ! -----------------------------------------------------------------------
      1413 continue
      call algaesbl(SCHWI,TFLIE,TEMPW,flag,elen,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemb,CHLA,ir                        &
                    ,dalgbl,dalgab,ior,anze,sedalb,algzob,dblmor,fkm,vabfl,abchl,abgmax,abksn,abksp,saettb,abremi    &
                    ,vco2,iph,vkigr,abbcm,abl,tpbl,uhrz,iwied,fibaus,abmuea,fhebas,abreau,tausc,ischif,ilbuhn,ieros  &
                    ,ZAKI,ZAGR,ZABL,asble,qeinl,jiein,echla,ess,algdrb,algcob,antbl,zooind,GROT,SS,extk              &
                    ,extk_lamda                                                                                      &
                    ,ilamda,eta,aw,ack,acg,acb,ah,as,al                                                              &
                    ,vNH4z,vNO3z,gelPz,dalgbz,nkzs,dH2D,tempwz,cpfad,up_PBz,up_NBz,Qmx_PB,Qmn_PB                     &
                    ,upmxPB,Qmx_NB,Qmn_NB,upmxNB,Q_NB,Q_PB,IKbe,frmube,alamda,abltbr,ablbrz,up_N2z,ablz              &
                    ,chlabl,a1Bl,a2Bl,a3Bl,hchlbz,hCChlbz,algabz,algzbz,Dz2D,ToptB,kTemp_Bl,ifix,sedAlg_MQ           &
                    ,sedAlb0,hQ_NBz, mstr,itags,monats,isim_end,abmor_1                                              &
                    ,.false.,0)
      
      if (nbuhn(mstr) == 0)goto 1414
      if (ilbuhn == 0) then
         do ior = 1,anze+1
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
      
      if (ilbuhn == 1) then
         do ior = 1,anze+1
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
            
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) diff1 = bsvheb(mstr,ior) - svhemb(ior)
            if (bleb(mstr,ior) > 0.0) svhemb(ior)      = max(0., svhemb(ior)      + diff1 * hctau1(ior))
            if (hctau2(ior)    > 0.0) bsvheb(mstr,ior) = max(0., bsvheb(mstr,ior) - diff1 * hctau2(ior))
         enddo
         
         ilbuhn = 0
      endif
      
      ! -----------------------------------------------------------------------
      ! Grünalgen
      ! -----------------------------------------------------------------------
      1414 continue
      call algaesgr(SCHWI,TFLIE,TEMPW,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemg,CHLA,SSALG,dalggr,dalgag                        &
                    ,flag,elen,ior,anze,sedalg,algzog,dgrmor,fkm,vkigr,chlaki,chlagr,vabfl,qeinl,jiein,evkigr,eantbl        &
                    ,agchl,aggmax,agksn,agksp,agremi,vco2,algdrg,pbiogr,Q_PK,Q_NK,iph,akbcm,agbcm,aki,agr,cmatgr            &
                    ,cmatki,abbcm,antbl,abl,pbiobl,chlabl,extk,extk_lamda                                                   &
                    ,ilamda,eta,aw,ack,acg,acb,ah,as,al                                                                     &
                    ,tpgr,uhrz,iwied,algcog                                                                                 &
                    ,figaus,agmuea,fhegas,agreau,tausc,ischif,ilbuhn,ieros,asgre,echla,ess,ss,zooind,GROT,Q_PG,Q_NG         &
                    ,vNH4z,vNO3z,gelPz,dalggz,nkzs,dH2D,tempwz,cpfad,itags,monats,mstr,up_PGz,up_NGz,Qmx_PG                 &
                    ,Qmn_PG,upmxPG,Qmx_NG,Qmn_NG,upmxNG,IKge,frmuge,alamda,agrtbr,agrbrz,akiz,agrz,ablz                     &
                    ,chlaz,hchlkz,hchlgz,hchlbz,hCChlgz,algagz,algzgz,Dz2D,ToptG,kTemp_Gr,ifix,sedAlg_MQ,sedAlg0, hQ_NGz    &
                    ,a1Gr,a2Gr,a3Gr,isim_end,agmor_1                                                                        &
                    ,.false.,0)
      
      if (any(isnan(agr))) call qerror("Division by zero in subroutine algaesgr")
      
      if (nbuhn(mstr) == 0)goto 1513
      if (ilbuhn == 0) then
         do ior = 1,anze+1
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
      
      if (ilbuhn == 1) then
         do ior = 1,anze+1
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
            
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) then
               diff1  = bsvheg(mstr,ior)   - svhemg(ior)
               diff2  = bchlak(mstr,ior)   - chlaki(ior)
               diff3  = bchlag(mstr,ior)   - chlagr(ior)
               diff4  = bchlab(mstr,ior)   - chlabl(ior)
               diff5  = bchla(mstr,ior)    - chla(ior)
               diff6  = baki(mstr,ior)     - aki(ior)
               diff7  = bagr(mstr,ior)     - agr(ior)
               diff8  = babl(mstr,ior)     - abl(ior)
               diff9  = bakbcm(mstr,ior)   - akbcm(ior)
               diff10 = bagbcm(mstr,ior)   - agbcm(ior)
               diff11 = babbcm(mstr,ior)   - abbcm(ior)
               diff12 = bakmor_1(mstr,ior) - akmor_1(mstr,ior)
               diff13 = babmor_1(mstr,ior) - abmor_1(mstr,ior)
               diff14 = bagmor_1(mstr,ior) - agmor_1(mstr,ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               svhemg(ior)       = max(0., svhemg(ior) + diff1  * hctau1(ior))
               chlaki(ior)       = chlaki(ior)       + diff2  * hctau1(ior)
               chlagr(ior)       = chlagr(ior)       + diff3  * hctau1(ior)
               chlabl(ior)       = chlabl(ior)       + diff4  * hctau1(ior)
               chla(ior)         = chla(ior)         + diff5  * hctau1(ior)
               aki(ior)          = aki(ior)          + diff6  * hctau1(ior)
               agr(ior)          = agr(ior)          + diff7  * hctau1(ior)
               abl(ior)          = abl(ior)          + diff8  * hctau1(ior)
               akbcm(ior)        = akbcm(ior)        + diff9  * hctau1(ior)
               agbcm(ior)        = agbcm(ior)        + diff10 * hctau1(ior)
               abbcm(ior)        = abbcm(ior)        + diff11 * hctau1(ior)
               akmor_1(mstr,ior) = akmor_1(mstr,ior) + diff12 * hctau1(ior)
               abmor_1(mstr,ior) = abmor_1(mstr,ior) + diff13 * hctau1(ior)
               agmor_1(mstr,ior) = agmor_1(mstr,ior) + diff14 * hctau1(ior)
               if (Chlaki(ior) + chlagr(ior) + chlabl(ior) > 0.) then
                  vkigr(ior) = chlaki(ior) / (Chlaki(ior) + chlagr(ior) + chlabl(ior))
                  antbl(ior) = chlabl(ior) / (Chlaki(ior) + chlagr(ior) + chlabl(ior))
               else
                  vkigr(ior) = 1./3.
                  antbl(ior) = 1./3.
               endif
            endif
            
            if (hctau2(ior) > 0.0) then
               bsvheg(mstr,ior)   = max(0., bsvheg(mstr,ior) - diff1 * hctau2(ior))
               bchlak(mstr,ior)   = bchlak(mstr,ior)   - diff2 * hctau2(ior)
               bchlag(mstr,ior)   = bchlag(mstr,ior)   - diff3 * hctau2(ior)
               bchlab(mstr,ior)   = bchlab(mstr,ior)   - diff4 * hctau2(ior)
               bchla(mstr,ior)    = bchla(mstr,ior)    - diff5 * hctau2(ior)
               baki(mstr,ior)     = baki(mstr,ior)     - diff6 * hctau2(ior)
               bagr(mstr,ior)     = bagr(mstr,ior)     - diff7 * hctau2(ior)
               babl(mstr,ior)     = babl(mstr,ior)     - diff8 * hctau2(ior)
               bakbcm(mstr,ior)   = bakbcm(mstr,ior)   - diff9 * hctau2(ior)
               bagbcm(mstr,ior)   = bagbcm(mstr,ior)   - diff10 * hctau2(ior)
               babbcm(mstr,ior)   = babbcm(mstr,ior)   - diff11 * hctau2(ior)
               bakmor_1(mstr,ior) = bakmor_1(mstr,ior) - diff12 * hctau2(ior)
               babmor_1(mstr,ior) = babmor_1(mstr,ior) - diff13 * hctau2(ior)
               bagmor_1(mstr,ior) = bagmor_1(mstr,ior) - diff14 * hctau2(ior)
               if (bChlak(mstr,ior) + bchlag(mstr,ior) + bchlab(mstr,ior) > 0.) then
                  bvkigr(mstr,ior) = bchlak(mstr,ior) / (bChlak(mstr,ior) + bchlag(mstr,ior) + bchlab(mstr,ior))
                  bantbl(mstr,ior) = bchlab(mstr,ior) / (bChlak(mstr,ior) + bchlag(mstr,ior) + bchlab(mstr,ior))
               else
                  bvkigr(mstr,ior) = 1./3.
                  bantbl(mstr,ior) = 1./3.
               endif
            endif
         enddo
         ilbuhn = 0
      endif
      
      ! -----------------------------------------------------------------------
      ! benthische Algen
      ! -----------------------------------------------------------------------
      1513 continue
      ! call albenth(SCHWI,TFLIE,TEMPW,TIEFE,VMITT,VNO3,VNH4,GELP               &
      !              ,albewg,alberg,elen,flae,ior,anze,aggmax,agksn,agksp       &
      !              ,si,akksn,akksp,akkssi,akgmax,albewk,alberk,abegm2,abekm2  &
      !              ,vabfl,cmatgr,cmatki,akchl,agchl,extk,ilang,mstr           &
      !              ,.false.,0)
      albewg(:) = 0.0
      albewk(:) = 0.0
      alberg(:) = 0.0
      alberk(:) = 0.0
      cmatgr(:) = 0.0
      cmatki(:) = 0.0
      
      ! -----------------------------------------------------------------------
      ! macrophytes [turned off]
      ! -----------------------------------------------------------------------
      ! call mphyt(tiefe,tempw,anze,po2p,po2r,pfldalg,tflie                    &
      !            ,itags,monats,itstart,mstart,itmax,mmax,itend,mend,schwi    &
      !            ,pflmin,pflmax,pfl,sa,su,ilang,extk,mstr,                   &
      !            ,.false.,0)
      
      ! if (nbuhn(mstr) > 0) then
      !    do ior = 1,anze+1
      !       bpfl(mstr,ior) = pfl(ior)
      !       pfl(ior) = 0.0
      !    enddo
      ! endif
      
      pfl    = 0.
      pflmax = 0.
      pflmin = 0.
      po2p   = 0.
      po2r   = 0.
      
      if (nbuhn(mstr) > 0) then
         bpfl(mstr,:)  = 0.
         bpo2p(mstr,:) = 0.
         bpo2r(mstr,:) = 0.
      endif
      
      ! -----------------------------------------------------------------------
      ! organic carbon
      ! -----------------------------------------------------------------------
      1530 continue
      if (vbsb(1) < 0.0 .and. vbsb(1) < 0.0) goto 1514
      
      ! inflow from diffuse and point sources
      call organic_carbon_inflow_1d(                      &
               ocsb, obsb, CD, CP, CM, BAC, fbsgr, frfgr, &
               vkigr, antbl, ecsb, ebsb, echla, evkigr,   &
               eantbl, ezind, eCD, eCP, eCM, eBAC, frfgrs,&
               fbsgrs, akbcm, agbcm, abbcm, tempw, bsbzoo,&
               toc_csb, mstr, ieinLs, qeinlL, qeinl,      &
               vabfl, iorLe, iorLa, jiein, flae, anze,    &
               flag, tflie)
      
      ! metabolism in main river
      do ior = 1, anze+1
         call organic_carbon(                                                    &
               ocsb(ior), obsb(ior), CD(1,ior), CD(2,ior), CP(1,ior), CP(2,ior), &
               CM(ior), bac(ior), fbsgr(ior), frfgr(ior),  nl0(ior), pl0(ior),   &
               cHNF(ior), bvHNF(ior),                                            &
               tempw(ior), tiefe(ior), pfl(ior), jdoc1(ior), jdoc2(ior),         &
               rau(ior), vmitt(ior), bsbHNF(ior),                                &
               dKiMor(ior), dGrMor(ior), dBlMor(ior), abszo(ior),                &
               Q_PK(ior), Q_PG(ior), Q_PB(ior),                                  &
               Q_NK(ior), Q_NG(ior), Q_NB(ior),                                  &
               zexKi(ior), zexGr(ior), zexbl(ior),                               &
               drfaek(ior), drfaeg(ior), drfaeb(ior),                            &
               ssdr(ior), hnfbac(ior), zBAC(ior),                                &
               abl(ior), agr(ior), aki(ior), zooind(ior),                        &
               bsbzoo, toc_csb, tflie,                                           &
               BAcmua(ior), bsbct(ior), BSBctP(ior), doN(ior), bsbt(ior),        &
               BSBbet(ior), orgCsd0(ior), orgCsd(mstr,ior), orgCsd_abb(mstr,ior),&
               dorgSS(ior), vBSB(ior), vCSB(ior),                                &
               kontroll, jjj)
      enddo
      
      ! groyne-field
      if (nbuhn(mstr) > 0) then 
         do ior = 1, anze+1
            ! metabolism in groyne-field
            call organic_carbon(                                                                       &
                     bcsb(mstr,ior), bbsb(mstr,ior), bCD(mstr,1,ior), bCD(mstr,2,ior),                 &
                     bCP(mstr,1,ior), bCP(mstr,2,ior),                                                 &
                     bCM(mstr,ior), bBAC(mstr,ior), bfbsgr(mstr,ior), bfrfgr(mstr,ior),                &
                     nl0(ior), pl0(ior),                                                               &
                     bCHNF(mstr,ior), bvHNF(ior),                                                      &
                     btempw(mstr,ior), bh(mstr,ior), bpfl(mstr,ior), bJDOC1(ior), bJDOC2(ior),         &
                     rau(ior), vbm(mstr,ior), bBSBHN(mstr,ior),                                        &
                     bdkmor(mstr,ior), bdgmor(mstr,ior), bdbmor(mstr,ior), babszo(mstr,ior),           &
                     Q_PK(ior), Q_PG(ior), Q_PB(ior),                                                  &
                     Q_NK(ior), Q_NG(ior), Q_NB(ior),                                                  &
                     bzexki(mstr,ior), bzexgr(mstr,ior), bzexbl(mstr,ior),                             &
                     bdfaek(mstr,ior), bdfaeg(mstr,ior), bdfaeb(mstr,ior),                             &
                     bssdr(mstr,ior), bHNFBS(mstr,ior), zBAC(ior),                                     &
                     babl(mstr,ior), bagr(mstr,ior), baki(mstr,ior), bzooi(mstr,ior),                  &
                     bsbzoo, toc_csb, tflie,                                                           &
                     BAcmua(ior), bbsbct(mstr,ior), bbsbcP(mstr,ior), bdoN(mstr,ior), bbsbt(mstr,ior), &
                     bbsbbe(mstr,ior), orgCsd0(ior), borgCs(mstr,ior), orgCsd_abb(mstr,ior),           &
                     borgSS(mstr,ior), bvbsb(mstr,ior), bvcsb(mstr,ior),                               &
                     kontroll, jjj)
            
            ! --- mixing between groyne-field and main river ---
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) then
               diff1  = bbsb(mstr,ior)   - obsb(ior)
               diff2  = bcsb(mstr,ior)   - ocsb(ior)
               diff3  = bvbsb(mstr,ior)  - vbsb(ior)
               diff4  = bvcsb(mstr,ior)  - vcsb(ior)
               diff5  = bcd(mstr,1,ior)  - CD(1,ior)
               diff6  = bcd(mstr,2,ior)  - CD(2,ior)
               diff7  = bcp(mstr,1,ior)  - CP(1,ior)
               diff8  = bcp(mstr,2,ior)  - CP(2,ior)
               diff9  = bcm(mstr,ior)    - CM(ior)
               diff10 = bBAC(mstr,ior)   - BAC(ior)
               diff11 = bfbsgr(mstr,ior) - fbsgr(ior)
               diff12 = bfrfgr(mstr,ior) - frfgr(ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               obsb(ior)  = obsb(ior)  + diff1  * hctau1(ior)
               ocsb(ior)  = ocsb(ior)  + diff2  * hctau1(ior)
               vbsb(ior)  = vbsb(ior)  + diff3  * hctau1(ior)
               vcsb(ior)  = vcsb(ior)  + diff4  * hctau1(ior)
               CD(1,ior)  = CD(1,ior)  + diff5  * hctau1(ior)
               CD(2,ior)  = CD(2,ior)  + diff6  * hctau1(ior)
               CP(1,ior)  = CP(1,ior)  + diff7  * hctau1(ior)
               CP(2,ior)  = CP(2,ior)  + diff8  * hctau1(ior)
               CM(ior)    = CM(ior)    + diff9  * hctau1(ior)
               BAC(ior)   = BAC(ior)   + diff10 * hctau1(ior)
               fbsgr(ior) = fbsgr(ior) + diff11 * hctau1(ior)
               frfgr(ior) = frfgr(ior) + diff12 * hctau1(ior)
            endif
            
            if (hctau2(ior) > 0.0) then
               bbsb(mstr,ior)   = bbsb(mstr,ior)   - diff1  * hctau2(ior)
               bcsb(mstr,ior)   = bcsb(mstr,ior)   - diff2  * hctau2(ior)
               bvbsb(mstr,ior)  = bvbsb(mstr,ior)  - diff3  * hctau2(ior)
               bvcsb(mstr,ior)  = bvcsb(mstr,ior)  - diff4  * hctau2(ior)
               bCD(mstr,1,ior)  = bCD(mstr,1,ior)  - diff5  * hctau2(ior)
               bCD(mstr,2,ior)  = bCD(mstr,2,ior)  - diff6  * hctau2(ior)
               bCP(mstr,1,ior)  = bCP(mstr,1,ior)  - diff7  * hctau2(ior)
               bCP(mstr,2,ior)  = bCP(mstr,2,ior)  - diff8  * hctau2(ior)
               bCM(mstr,ior)    = bCM(mstr,ior)    - diff9  * hctau2(ior)
               bBAC(mstr,ior)   = bBAC(mstr,ior)   - diff10 * hctau2(ior)
               bfbsgr(mstr,ior) = bfbsgr(mstr,ior) - diff11 * hctau2(ior)
               bfrfgr(mstr,ior) = bfrfgr(mstr,ior) - diff12 * hctau2(ior)
            endif
         enddo
      endif
      
      ! -----------------------------------------------------------------------
      ! nitrogen
      ! -----------------------------------------------------------------------
      1514 continue
      if (vnh4(1) < 0.0) goto 1515
      
      if (nbuhn(mstr) > 0) then
         do ior = 1,anze+1
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
      
      ! inflow from point and diffuse sources
      call nitrogen_inflow_1d(vnh4, vno2, vno3, gesN, vx0, vx02, nl0, Q_NK,    &
                              Q_NG, Q_NB, hFluN3, mstr, eNH4L, eNO2L, eNO3L,   &
                              gesNL, eNH4, eNO2, eNO3, ex0, ex02, eGesN, enl0, &
                              ieinLs, qeinlL, qeinl, vabfl, iorLe, iorLa,      &
                              jiein, flae, anze, flag, tflie)
                              
      ! metabolism in main river
      do ior = 1, anze+1
         ! nitrifiers
         call nitrifiers(vx0(ior), vx02(ior), pfl(ior), vph(ior), tempw(ior),   &
                         vO2(ior), vNH4(ior), vNO2(ior), rhyd(ior), rau(ior),   &
                         tiefe(ior), vmitt(ior), hJNH4(mstr,ior), tflie,        &
                         susn(ior), susn2(ior), pfln1(ior), pfln2(ior),         &
                         sedx0(ior), bettn(ior), go2n(ior), susno(ior),         &
                         kontroll, jjj)
         
         ! nitrogen
         call nitrogen(vNH4(ior), vNO3(ior), vNO2(ior), gesN(ior), vO2(ior),    &
                       vx02(ior),                                               &
                       aki(ior), agr(ior), abl(ior),                            &
                       Q_NK(ior), Q_NG(ior), Q_NB(ior),                         &
                       up_NKz(1,ior), up_NGz(1,ior), up_NBz(1,ior),             &
                       akibrz(1,ior), agrbrz(1,ior), ablbrz(1,ior),             &
                       algakz(1,ior), algagz(1,ior), algabz(1,ior),             &
                       sedalk(ior), sedalg(ior), sedalb(ior),                   &
                       algdrk(ior), algdrg(ior), algdrb(ior),                   &
                       abltbr(ior),                                             &
                       albewk(ior), albewg(ior),                                &
                       alberk(ior), alberg(ior),                                &
                       resdr(ior), dzres1(ior), dzres2(ior),                    &
                       exdrvk(ior), exdrvg(ior), exdrvb(ior),                   &
                       up_N2z(1,ior), orgCsd(mstr,ior), nl0(ior), bsbct(ior),   &
                       susn(ior), susn2(ior), pfln1(ior), pfln2(ior), don(ior), &
                       hJNH4(mstr,ior), hJNO3(mstr,ior), hJN2(mstr,ior),        &
                       tiefe(ior), tflie,                                       &
                       akiNH4(ior), agrNH4(ior), ablNH4(ior),                   &
                       akiNO3(ior), agrNO3(ior), ablNO3(ior),                   &
                       hFluN3(mstr,ior), dC_DenW(ior),                          &
                       kontroll, jjj)
      enddo
      
      ! --- groyne-field ---
      if (nbuhn(mstr) > 0)then
         do ior = 1,anze+1
            ! nitrifiers
            call nitrifiers(bx0(mstr,ior), bx02(mstr,ior), bpfl(mstr,ior), bph(mstr,ior), btempw(mstr,ior), &
                            bO2(mstr,ior), bNH4(mstr,ior), bNO2(mstr,ior), rhyd(ior), rau(ior),             &
                            bh(mstr,ior), vbm(mstr,ior), bjNH4(mstr,ior), tflie,                            &
                            bsusn(mstr,ior), bsusn2(mstr,ior), bpfln1(mstr,ior), bpfln2(mstr,ior),          &
                            bsedx0(mstr,ior), bbettn(mstr,ior), bgo2n(mstr,ior), bsuso(mstr,ior),           &
                            kontroll, jjj)
            
            ! nitrogen
            call nitrogen(bNH4(mstr,ior), bNO3(mstr,ior), bNO2(mstr,ior), bgesN(mstr,ior), bO2(mstr,ior),        &
                          bx02(mstr,ior),                                                                        &
                          baki(mstr,ior), bagr(mstr,ior), babl(mstr,ior),                                        &
                          bQ_NK(mstr,ior), bQ_NG(mstr,ior), bQ_NB(mstr,ior),                                     &
                          bup_NK(mstr,ior), bup_NG(mstr,ior), bup_NB(mstr,ior),                                  &
                          baktbr(mstr,ior), bagtbr(mstr,ior), babtbr(mstr,ior),                                  &
                          balakz(mstr,ior), balagz(mstr,ior), balabz(mstr,ior),                                  & 
                          bsedak(mstr,ior), bsedag(mstr,ior), bsedab(mstr,ior),                                  &
                          badrk(mstr,ior), badrg(mstr,ior), badrb(mstr,ior),                                     &
                          abltbr(ior),                                                                           &
                          babewk(mstr,ior), babewg(mstr,ior),                                                    &
                          baberk(mstr,ior), baberg(mstr,ior),                                                    &
                          bresdr(mstr,ior), bzres1(mstr,ior), bzres2(mstr,ior),                                  &
                          bexdvk(mstr,ior), bexdvg(mstr,ior), bexdvb(mstr,ior),                                  &
                          up_N2z(1,ior), borgCs(mstr,ior),bnl0(mstr,ior), bbsbct(mstr,ior),                      &
                          bsusn(mstr,ior), bsusn2(mstr,ior), bpfln1(mstr,ior), bpfln2(mstr,ior), bdon(mstr,ior), &
                          bJNH4(mstr,ior), bJNO3(mstr,ior), bJN2(mstr,ior),                                      &
                          bh(mstr,ior), tflie,                                                                   &
                          bakn4(mstr,ior), bagn4(mstr,ior), babn4(mstr,ior),                                     &
                          bakn3(mstr,ior), bagn3(mstr,ior), babn3(mstr,ior),                                     &
                          bFluN3(mstr,ior), dC_DenW(ior),                                                        &
                          kontroll, jjj)
            
            ! TODO (Schönung)
            ! Fehler: Das Buhnenfeld bekommt hier den Wert aus dem Hauptfeld für die Variable 'dC_DenW'
            
            ! Folgende Zuweisungen werden gemacht, um Fehler aus dem bisherigen Code beizubehalten.
            ! Damit soll gewährleistet werden, dass in der Entkernung keine Unterschiede auftreten und beim Testen
            ! auf Identität getestet werden kann
            ! Nach einem Erfolgreichen Test sollten diese Fehler hier ausgebessert werden
            albewg(ior) = zwabwg(ior)
            alberg(ior) = zwabrg(ior)
            albewk(ior) = zwabwk(ior)
            alberk(ior) = zwabrk(ior)
            
            ! mixing between main river and groyne-field
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.0) then
               diff1  = bx0(mstr,ior)    - vx0(ior)
               diff2  = bx02(mstr,ior)   - vx02(ior)
               diff3  = bnh4(mstr,ior)   - vnh4(ior)
               diff4  = bno2(mstr,ior)   - vno2(ior)
               diff5  = bno3(mstr,ior)   - vno3(ior)
               diff6  = bnl0(mstr,ior)   - nl0(ior)
               diff7  = bgesN(mstr,ior)  - gesN(ior)
               diff8  = bQ_NK(mstr,ior)  - Q_NK(ior)
               diff9  = bQ_NG(mstr,ior)  - Q_NG(ior)
               diff10 = bQ_NB(mstr,ior)  - Q_NB(ior)
               diff11 = bFluN3(mstr,ior) - hFluN3(mstr,ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               vx0(ior)         = vx0(ior)         + diff1  * hctau1(ior)
               vx02(ior)        = vx02(ior)        + diff2  * hctau1(ior)
               vnh4(ior)        = vnh4(ior)        + diff3  * hctau1(ior)
               vno2(ior)        = vno2(ior)        + diff4  * hctau1(ior)
               vno3(ior)        = vno3(ior)        + diff5  * hctau1(ior)
               nl0(ior)         = nl0(ior)         + diff6  * hctau1(ior)
               gesN(ior)        = gesN(ior)        + diff7  * hctau1(ior)
               Q_NK(ior)        = Q_NK(ior)        + diff8  * hctau1(ior)
               Q_NG(ior)        = Q_NG(ior)        + diff9  * hctau1(ior)
               Q_NB(ior)        = Q_NB(ior)        + diff10 * hctau1(ior)
               hFluN3(mstr,ior) = hFluN3(mstr,ior) + diff11 * hctau1(ior)
            endif
            
            if (hctau2(ior) > 0.0) then
               bx0(mstr,ior)    = bx0(mstr,ior)    - diff1  * hctau2(ior)
               bx02(mstr,ior)   = bx02(mstr,ior)   - diff2  * hctau2(ior)
               bnh4(mstr,ior)   = bnh4(mstr,ior)   - diff3  * hctau2(ior)
               bno2(mstr,ior)   = bno2(mstr,ior)   - diff4  * hctau2(ior)
               bno3(mstr,ior)   = bno3(mstr,ior)   - diff5  * hctau2(ior)
               bnl0(mstr,ior)   = bnl0(mstr,ior)   - diff6  * hctau2(ior)
               bgesN(mstr,ior)  = bgesN(mstr,ior)  - diff7  * hctau2(ior)
               bQ_NK(mstr,ior)  = bQ_NK(mstr,ior)  - diff8  * hctau2(ior)
               bQ_NG(mstr,ior)  = bQ_NG(mstr,ior)  - diff9  * hctau2(ior)
               bQ_NB(mstr,ior)  = bQ_NB(mstr,ior)  - diff10 * hctau2(ior)
               bFluN3(mstr,ior) = bFluN3(mstr,ior) - diff11 * hctau2(ior)
            endif
         enddo
      endif
      
      ! -----------------------------------------------------------------------
      ! pH-Wert
      ! -----------------------------------------------------------------------
      1515 continue
      if (iph == 0)goto 113
      if (vph(1) < 0.0)goto 113
      if (nbuhn(mstr) > 0 .and. ilbuhn == 0) then
         do ior = 1,anze+1
            albewg(ior) = 0.0
            alberg(ior) = 0.0
            albewk(ior) = 0.0
            alberk(ior) = 0.0
         enddo
      endif
      
      ! inflow from point and diffuse sources
      call ph_inflow_1d(vph, lf, ca, mw, pw, elfL, caL, eph, elf, eca, emw,   &
                        tempw, mstr, ieinLs, qeinlL, qeinl, vabfl, iorLe,     &
                        iorLa, jiein, flae, anze, flag, tflie,                &
                        kontroll, jjj)
   
      ! metabolism in main river
      do ior = 1, anze+1
         call ph(mw(ior), pw(ior), ca(ior), lf(ior), tempw(ior), vph(ior), vco2(ior),           &
                 tflie, rau(ior), vmitt(ior), tiefe(ior), rhyd(ior), flae(ior),                 &
                 wge(IDWe(mstr, ior)), WLage(mstr, ior), hWS(mstr, ior), iphy,                  &
                 bsbct(ior), resdr(ior), dzres1(ior), dzres2(ior),                              &
                 dalgki(ior), dalggr(ior), dalgbl(ior), dalgak(ior), dalgag(ior), dalgab(ior),  &
                 alberg(ior), alberk(ior), albewg(ior), albewk(ior),                            &
                 susn(ior), po2p(ior), po2r(ior), ssalg(ior), stind(ior),                       &
                 kontroll ,jjj)
      enddo
      
      ! --- groyne-field ---
      if (nbuhn(mstr) > 0)then
         do ior = 1,anze+1
            ! metabolism
            call ph(bmw(mstr,ior),bpw(mstr,ior),bca(mstr,ior),blf(mstr,ior),btempw(mstr,ior),bph(mstr,ior),vco2s,    &
                    tflie,rau(ior),vbm(mstr,ior),bh(mstr,ior),rhyd(ior),flae(ior),                                   &
                    wge(IDWe(mstr,ior)), WLage(mstr,ior), hWS(mstr,ior), iphy,                                       &
                    bbsbct(mstr,ior),bresdr(mstr,ior),bzres1(mstr,ior),bzres2(mstr,ior),                             &
                    bdaki(mstr,ior),bdagr(mstr,ior),bdabl(mstr,ior),bdaak(mstr,ior),bdaag(mstr,ior),bdaab(mstr,ior), &
                    baberg(mstr,ior),baberk(mstr,ior),babewg(mstr,ior),babewk(mstr,ior),                             &
                    bsusn(mstr,ior),bpo2p(mstr,ior),bpo2r(mstr,ior),bssalg(mstr,ior),bstind(mstr,ior),               &
                    kontroll, jjj)
             
            ! mixing between main river and groyne-field
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) then
               diff1  = bmw(mstr,ior)    - mw(ior)
               diff2  = bpw(mstr,ior)    - pw(ior)
               diff3  = bca(mstr,ior)    - ca(ior)
               diff4  = blf(mstr,ior)    - lf(ior)
               diff5  = bph(mstr,ior)    - vph(ior)
               diff6  = bstind(mstr,ior) - stind(ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               mw(ior)    = mw(ior)    + diff1 * hctau1(ior)
               pw(ior)    = pw(ior)    + diff2 * hctau1(ior)
               ca(ior)    = ca(ior)    + diff3 * hctau1(ior)
               lf(ior)    = lf(ior)    + diff4 * hctau1(ior)
               vph(ior)   = vph(ior)   + diff5 * hctau1(ior)
               stind(ior) = stind(ior) + diff6 * hctau1(ior)
            endif
            
            if (hctau2(ior) > 0.0) then
               bmw(mstr,ior)    = bmw(mstr,ior)    - diff1 * hctau2(ior)
               bpw(mstr,ior)    = bpw(mstr,ior)    - diff2 * hctau2(ior)
               bca(mstr,ior)    = bca(mstr,ior)    - diff3 * hctau2(ior)
               blf(mstr,ior)    = blf(mstr,ior)    - diff4 * hctau2(ior)
               bph(mstr,ior)    = bph(mstr,ior)    - diff5 * hctau2(ior)
               bstind(mstr,ior) = bstind(mstr,ior) - diff6 * hctau2(ior)
            endif
         enddo
      endif
      
      ! -----------------------------------------------------------------------
      ! Temperatur
      ! -----------------------------------------------------------------------
      113 continue
      
      if (iwsim == 4) then
         call ctracer(tempw,flag,anze,qeinl,etemp,vabfl,jiein,ilbuhn,nkzs)
         
      else
         call temperw(RO,TEMPL,TEMPW,SCHWI,WGE,TIEFE,TFLIE,flag,elen,ior,anze, &
                      etemp,ewaerm,typ,qeinl,vabfl,jiein,cloud,wtyp,iwied,uhrz,&
                      ilbuhn,nwaerm,fkm,nkzs,tempwz,dH2D,iorLa,iorLe,ieinLs,   &
                      flae,qeinlL,etempL,mstr,IDWe,ilang,dtemp,extk,itags,     &
                      monats,Tsed,Wlage,hWS,htempw,htempz,WUEBKS,SPEWKSS,      &
                      PSREFSS,extkS,iwsim,iform_VerdR,                         &
                      .false.,0)
      endif
      
      if (nbuhn(mstr) == 0)goto 413
      if (ilbuhn == 0) then
         do ior = 1,anze+1
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
      
      if (ilbuhn == 1) then
         do ior = 1,anze+1
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
            
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.0) diff1 = btempw(mstr,ior) - tempw(ior)
            if (bleb(mstr,ior) > 0.0) then
               tempw(ior) = tempw(ior) + diff1 * hctau1(ior)
               do nkz = 1,nkzs(ior)
                  tempwz(nkz,ior) = tempwz(nkz,ior) + diff1 * hctau1(ior)
               enddo
            endif
            
            if (hctau2(ior) > 0.0) then
               btempw(mstr,ior) = btempw(mstr,ior) - diff1 * hctau2(ior)
            endif
         enddo
         
         ilbuhn = 0
      endif
      
      ! -----------------------------------------------------------------------
      ! ortho-Phosphat
      ! -----------------------------------------------------------------------
      413 continue
      
      if (iwsim == 2 .and. icoli == 1) goto 1525
      if (iwsim == 4 .or. iwsim == 2 .or. iwsim == 5) goto 118
      if (gelP(1) < 0.0) goto 1516
      
      if (nbuhn(mstr) > 0 .and. ilbuhn == 0) then
         albewg(:) = 0.0
         alberg(:) = 0.0
         albewk(:) = 0.0
         alberk(:) = 0.0
      endif
      
      ! inflow from point and diffuse sources
      call phosphate_inflow_1d(gelp, gesP, pl0, Q_PK, Q_PG, Q_PB, hgesPz,  &
                               gelPz, gPL, gesPL, egesP, eP, epl0, mstr,   &
                               ieinLs, qeinlL, qeinl, vabfl, iorLa, iorLe, &
                               jiein, flae, anze, nkzs, flag, tflie)
   
      ! metabolism
      do ior = 1, anze+1
         call phosphate(gelP(ior), gesP(ior), bsbctP(ior),                      &
                        aki(ior), agr(ior), abl(ior), dzres1(ior), dzres2(ior), &
                        Q_PK(ior), Q_PG(ior), Q_PB(ior),                        &
                        resdr(ior), exdrvk(ior), exdrvg(ior), exdrvb(ior),      &
                        up_PGz(1,ior), up_PKz(1,ior), up_PBz(1,ior),            &
                        agrbrz(1,ior), akibrz(1,ior), ablbrz(1,ior),            &
                        algagz(1,ior), algakz(1,ior), algabz(1,ior),            &
                        albewg(ior), alberg(ior), albewk(ior), alberk(ior),     &
                        tiefev, hJPO4(mstr,ior), orgCsd(mstr, ior), pl0(ior),   &
                        sedalk(ior), sedalb(ior), sedalg(ior),                  &
                        algdrk(ior), algdrb(ior), algdrg(ior),                  &
                        tflie,                                                  &
                        kontroll, jjj)
      enddo
      
      ! --- groyne fields ---
      if (nbuhn(mstr) > 0) then
         do ior = 1, anze+1
            ! metabolism in groyne fields
            call phosphate(bgelp(mstr,ior), bgesP(mstr,ior), bbsbcP(mstr,ior),                                &
                           baki(mstr,ior), bagr(mstr,ior), babl(mstr,ior), bzres1(mstr,ior), bzres2(mstr,ior),&
                           bQ_PK(mstr,ior), bQ_PG(mstr,ior),  bQ_PB(mstr,ior),                                &
                           bresdr(mstr,ior), bexdvk(mstr,ior), bexdvg(mstr,ior), bexdvb(mstr,ior),            &
                           bup_PG(mstr,ior), bup_PK(mstr,ior), bup_PB(mstr,ior),                              &
                           bagtbr(mstr,ior), baktbr(mstr,ior), babtbr(mstr,ior),                              &
                           balagz(mstr,ior), balakz(mstr,ior), balabz(mstr,ior),                              &
                           albewg(ior), alberg(ior), albewk(ior), alberk(ior),                                &
                           bh(mstr,ior), bJPO4(mstr,ior), borgCs(mstr,ior), bpl0(mstr,ior),                   &
                           bsedak(mstr,ior), bsedab(mstr,ior), bsedag(mstr,ior),                              &
                           badrk(mstr,ior), badrb(mstr,ior), badrg(mstr,ior),                                 &
                           tflie,                                                                             &
                           kontroll, jjj)
            
            ! mixing of groyne fields and main river
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) then
               diff1 = bgelp(mstr,ior) - gelp(ior)
               diff2 = bpl0(mstr,ior)  - pl0(ior)
               diff3 = bgesP(mstr,ior) - gesP(ior)
               diff4 = bQ_PK(mstr,ior) - Q_PK(ior)
               diff5 = bQ_PG(mstr,ior) - Q_PG(ior)
               diff6 = bQ_PB(mstr,ior) - Q_PB(ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               gelp(ior) = gelp(ior) + diff1 * hctau1(ior)
               pl0(ior)  = pl0(ior)  + diff2 * hctau1(ior)
               gesP(ior) = gesP(ior) + diff3 * hctau1(ior)
               Q_PK(ior) = Q_PK(ior) + diff4 * hctau1(ior)
               Q_PG(ior) = Q_PG(ior) + diff5 * hctau1(ior)
               Q_PB(ior) = Q_PB(ior) + diff6 * hctau1(ior)
            endif
            
            if (hctau2(ior) > 0.0) then
               bgelp(mstr,ior) = bgelp(mstr,ior) - diff1 * hctau2(ior)
               bpl0(mstr,ior)  = bpl0(mstr,ior)  - diff2 * hctau2(ior)
               bgesP(mstr,ior) = bgesP(mstr,ior) - diff3 * hctau2(ior)
               bQ_PK(mstr,ior) = bQ_PK(mstr,ior) - diff4 * hctau2(ior)
               bQ_PG(mstr,ior) = bQ_PG(mstr,ior) - diff5 * hctau2(ior)
               bQ_PB(mstr,ior) = bQ_PB(mstr,ior) - diff6 * hctau2(ior)
            endif
         enddo
      endif
      
      ! -----------------------------------------------------------------------
      ! Silikat
      ! -----------------------------------------------------------------------
      1516 continue
      if (si(1) < 0.0)goto 1517
      
      ! inflow from point and diffuse sources
      call silicate_inflow_1d(si, q_sk, siL, esi, mstr, ieinLs,    &
                              qeinlL, qeinl, vabfl, iorLe, iorLa,  &
                              jiein, flae, anze, flag, tflie)
      
      ! metabolism in main river
      do ior = 1, anze+1
         call silicate(si(ior), hJSi(mstr,ior), up_Siz(1,ior), akibrz(1,ior), &
                       algakz(1,ior), albewk(ior),                            &
                       tiefe(ior), tflie,                                     &
                       kontroll, jjj)
      enddo
      
      ! --- groyne-field ---
      if (nbuhn(mstr) > 0) then
         do ior = 1, anze+1
            
            ! metabolism
            call silicate(bsi(mstr,ior), bJSi(mstr,ior), bup_Si(mstr,ior), baktbr(mstr,ior), &
                          balakz(mstr,ior), babewk(mstr,ior),                                &
                          bh(mstr,ior), tflie,                                               &
                          kontroll, jjj)
            
            ! mixing between main river and groyne-field
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) then
               diff1 = bsi(mstr,ior)   - si(ior)
               diff2 = bQ_SK(mstr,ior) - Q_SK(ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               si(ior)   = si(ior)   + diff1 * hctau1(ior)
               Q_SK(ior) = Q_SK(ior) + diff2 * hctau1(ior)
            endif
            
            if (hctau2(ior) > 0.0) then
               bsi(mstr,ior)   = bsi(mstr,ior)   - diff1 * hctau2(ior)
               bQ_SK(mstr,ior) = bQ_SK(mstr,ior) - diff2 * hctau2(ior)
            endif
         enddo
      
      endif
      
      ! -----------------------------------------------------------------------
      ! Sauerstoff
      ! -----------------------------------------------------------------------
      1517 continue
      
      if (vo2(1) < 0.0)goto 1518
      
      if (nbuhn(mstr) > 0) then
         do ior = 1,anze+1
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
      
      call oxygen_inflow_1d(vo2, vo2z, o2L, eo2, etemp, tempwz, mstr, nkzs,   &
                         dh2d, ieinLs, qeinlL, qeinl, vabfl, iorLe, iorLa,    &
                         jiein, flae, anze, flag, tflie)
      
      do ior = 1, anze+1
         
         call oxygen(vO2(ior), zooind(ior),                                        &
                  agrNH4(ior), akiNH4(ior), ablNH4(ior),                           &
                  agrNO3(ior), akiNO3(ior), ablNO3(ior),                           &
                  dalggr(ior), dalgki(ior), dalgbl(ior), albewg(ior), albewk(ior), &
                  dalgag(ior), dalgak(ior), dalgab(ior), alberg(ior), alberk(ior), &
                  hJO2(mstr,ior), bsbt(ior), dC_DenW(ior), TOC_CSB, gO2n(ior),     &
                  pO2p(ior), pO2r(ior), rO2dr(ior), rO2hnf(ior),                   &
                  rau(ior), tiefe(ior), rhyd(ior), vmitt(ior), flae(ior),          &
                  wlage(mstr,ior), hws(mstr,ior), wge(IDWe(mstr,ior)), tempw(ior), &
                  iPhy, tflie,                                                     &
                  dalgo(ior), dalgao(ior), algo, abeowg(ior), abeowk(ior),         &
                  abeorg(ior), abeork(ior), zooro2(ior), hSchlr(mstr,ior),         &
                  o2ein, o2ein1(ior), saett(ior),                                  &
                  kontroll, jjj)
         
         if (isnan(vo2(ior))) then
            write(message, "(a,i0)") "Division by zero in subroutine oxygen in stretch ", mstr
            call qerror(message)
         endif
         
         if (nbuhn(mstr) > 0) then
            ! TODO (schoenung, august 2022): Ticket #52
            ! Folgende Größen aus dem Hauptfluss werden fehlerhafterweise an das Buhnenfeld übergeben:
            ! * zooind: hier muss bzooi übergeben werden
            ! * saett:  hier muss eine neue Variable für das Buhnenfeld angelegt und übergeben werden
            call oxygen(bo2(mstr,ior), zooind(ior),                                                                &
                     bagn4(mstr,ior), bakn4(mstr,ior), babn4(mstr,ior),                                            &
                     bagn3(mstr,ior), bakn3(mstr,ior), babn3(mstr,ior),                                            &
                     bdagr(mstr,ior), bdaki(mstr,ior), bdabl(mstr,ior), babewg(mstr,ior), babewk(mstr,ior),        &
                     bdaag(mstr,ior), bdaak(mstr,ior), bdaab(mstr,ior), baberg(mstr,ior), baberk(mstr,ior),        &
                     bJO2(mstr,ior), bbsbt(mstr,ior), dC_DenW(ior), TOC_CSB, bgo2n(mstr,ior),                      &
                     bpo2p(mstr,ior), bpo2r(mstr,ior), bro2dr(mstr,ior), bro2HF(mstr,ior),                         &
                     rau(ior), bh(mstr,ior), rhyd(ior), vbm(mstr,ior), flae(ior),                                  &
                     wlage(mstr,ior), hws(mstr,ior), wge(IDWe(mstr,ior)), btempw(mstr,ior),                        &
                     iPhy, tflie,                                                                                  &
                     bdalgo(mstr,ior), bdalgao(mstr,ior), balgo(mstr,ior), babeowg(mstr,ior), babeowk(mstr,ior),   &
                     babeorg(mstr,ior), babeork(mstr,ior), bzooro2(mstr,ior), bschlr(mstr,ior),                    &
                     bo2ein(mstr,ior), bo2ein1(mstr,ior), saett(ior),                                              &
                     kontroll, jjj)
            
            ! Mixing of main river and groyne-fields
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) diff1 = bo2(mstr,ior) - vo2(ior)
            if (bleb(mstr,ior) > 0.0) vo2(ior)      = vo2(ior)      + diff1 * hctau1(ior)
            if (hctau2(ior)    > 0.0) bo2(mstr,ior) = bo2(mstr,ior) - diff1 * hctau2(ior)
         endif
      enddo
      ! -----------------------------------------------------------------------
      ! Schwebstoffe
      ! -----------------------------------------------------------------------
      1518 continue
      if (ssalg(1) < 0.0) goto 1525
      call SCHWEB(zooind,dorgSS,ss,ssalg,tiefe,rau                                  &
                  ,tflie,VMITT,flae,flag,elen,ior,anze,ess,ssL,qeinl,qeinlL,vabfl   &
                  ,dkimor,dgrmor,abszo,zexki,zexgr,iorLa,iorLe,ieinLs               &
                  ,abl,zexbl,dblmor,drfaeb,jiein                                    &
                  ,aki,agr,ssdr,drfaek,drfaeg,drfaes,fssgr,sedss,sedSS_MQ,fssgrs    &
                  ,tausc,ischif,ilbuhn,fkm,ieros,iwied                              &
                  ,echla,vkigr,akbcm,agbcm,antbl,abbcm,ezind,mstr,itags,monats,uhrz &
                  ,kontroll,0)
      
      if (nbuhn(mstr) == 0)goto 1525
      if (ilbuhn == 0) then
         do ior = 1,anze+1
            zwtief(ior) = tiefe(ior)
            zwvm(ior) = vmitt(ior)
            zwzooi(ior) = zooind(ior)
            zworgS(ior) = dorgSS(ior)
            zwss(ior) = ss(ior)
            zwssa(ior) = ssalg(ior)
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
      
      if (ilbuhn == 1) then
         do ior = 1,anze+1
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
            if (ieros == 0) then
               if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) then
                  diff1 = bss(mstr,ior)    - ss(ior)
                  diff2 = bssalg(mstr,ior) - ssalg(ior)
                  diff3 = bfssgr(mstr,ior) - fssgr(ior)
               endif
               
               if (bleb(mstr,ior) > 0.0) then
                  ss(ior)    = ss(ior)    + diff1 * hctau1(ior)
                  ssalg(ior) = ssalg(ior) + diff2 * hctau1(ior)
                  fssgr(ior) = fssgr(ior) + diff3 * hctau1(ior)
               endif
               
               if (hctau2(ior) > 0.0) then
                  bss(mstr,ior)    = bss(mstr,ior)    - diff1 * hctau2(ior)
                  bssalg(mstr,ior) = bssalg(mstr,ior) - diff2 * hctau2(ior)
                  bfssgr(mstr,ior) = bfssgr(mstr,ior) - diff3 * hctau2(ior)
               endif
            endif
         enddo
         ilbuhn = 0
      endif
      
      ! -----------------------------------------------------------------------
      ! Coliform
      ! -----------------------------------------------------------------------
      1525 Continue
      if (iwsim == 5)goto 118
      if (iwsim /= 2)goto 1520
      if (hcoli(mstr,1) < 0.0 .and. iwsim == 2)goto 118
      1522 continue
      call COLIFORM(tiefe,rau,vmitt,vabfl,elen,flae,flag,tflie,schwi,tempw,jiein,ecoli                     &
                   ,qeinl,coliL,qeinlL,anze,iorLa,iorLe,ieinLs,ilbuhn,coli,DOSCF,extkS,mstr                &
                   ,ratecd,etacd,rateci,xnuec,ratecg,ratecs                                                &
                   ,.false.,0)
      
      if (nbuhn(mstr) == 0 .and. iwsim == 2)goto 118
      if (nbuhn(mstr) == 0 .and. iwsim /= 2)goto 1520
      if (ilbuhn == 0) then
         do ior = 1,anze+1
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
      
      if (ilbuhn == 1) then
         do ior = 1,anze+1
            btempw(mstr,ior) = tempw(ior)
            bh(mstr,ior) = tiefe(ior)
            bDOSCF(mstr,ior) = DOSCF(ior)
            bcoli(mstr,ior) = coli(ior)
            
            tempw(ior) = zwtemp(ior)
            DOSCF(ior) = zwDOSCF(ior)
            tiefe(ior) = zwtief(ior)
            coli(ior) = zwcoli(ior)
            
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.0) then
               diff1 = bcoli(mstr,ior)  - coli(ior)
               diff2 = bDOSCF(mstr,ior) - DOSCF(ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               coli(ior)  = coli(ior)  + diff1 * hctau1(ior)
               DOSCF(ior) = DOSCF(ior) + diff2 * hctau1(ior)
            endif
            
            if (hctau2(ior) > 0.0) then
               bcoli(mstr,ior)  = bcoli(mstr,ior)  - diff1 * hctau2(ior)
               bDOSCF(mstr,ior) = bDOSCF(mstr,ior) - diff2 * hctau2(ior)
            endif
         enddo
         ilbuhn = 0
      endif
      if (iwsim == 2 .or. iwsim == 5)goto 118
      
      ! -----------------------------------------------------------------------
      ! Erosion
      ! -----------------------------------------------------------------------
      1520 continue
      if (ieros == 0)goto 1519
      
      call erosion(ss,ssalg,SSeros,dsedH,tausc,M_eros,n_eros,sedroh  &
                   ,tflie,tiefe,rau,vmitt,anze,mstr,ilang,iwied     &
                   ,kontroll,0)
      
      if (nbuhn(mstr) == 0)goto 1519
      if (ilbuhn == 0) then
         do ior = 1,anze+1
            zwvm(ior) = vmitt(ior)
            zwtief(ior) = tiefe(ior)
            zwss(ior) = ss(ior)
            zwssa(ior) = ssalg(ior)
            zwsedS(ior) = sedss(ior)
            zwsedk(ior) = sedalk(ior)
            zwsedg(ior) = sedalg(ior)
            zwsedb(ior) = sedalb(ior)
            zwSSeros(ior) = SSeros(ior)
            zwdsedH(mstr,ior) = dsedH(mstr,ior)
            !tausc(mstr,ior) = btausc(mstr,ior) Sedimenteigenschaften unterscheiden sich nicht im Buhnenfeld
            tempw(ior) = btempw(mstr,ior)
            tiefe(ior) = bh(mstr,ior)
            vmitt(ior) = vbm(mstr,ior)
            ss(ior) = bss(mstr,ior)
            ssalg(ior) = bssalg(mstr,ior)
            sedalk(ior) = bsedak(mstr,ior)
            sedalg(ior) = bsedag(mstr,ior)
            sedalb(ior) = bsedab(mstr,ior)
            sedss(ior) = bsedss(mstr,ior)
         enddo
         ilbuhn = 1
         goto 1520
      endif
      if (ilbuhn == 1) then
         do ior = 1,anze+1
            bss(mstr,ior) = ss(ior)
            bssalg(mstr,ior) = ssalg(ior)
            bSSeros(ior) = SSeros(ior)
            tiefe(ior) = zwtief(ior)
            vmitt(ior) = zwvm(ior)
            ss(ior) = zwss(ior)
            ssalg(ior) = zwssa(ior)
            sedss(ior) = zwsedS(ior)
            sedalk(ior) = zwsedk(ior)
            sedalg(ior) = zwsedg(ior)
            sedalb(ior) = zwsedb(ior)
            SSeros(ior) = zwSSeros(ior)
            dsedH(mstr,ior) = zwdsedH(mstr,ior)
            ! btausc(mstr,ior) = tausc(mstr,ior)
            
            if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) then
               diff1 = bssalg(mstr,ior) - ssalg(ior)
               diff2 = bss(mstr,ior)    - ss(ior)
               diff3 = bfssgr(mstr,ior) - fssgr(ior)
            endif
            
            if (bleb(mstr,ior) > 0.0) then
               ssalg(ior) = ssalg(ior) + diff1 * hctau1(ior)
               ss(ior)    = ss(ior)    + diff2 * hctau1(ior)
               fssgr(ior) = fssgr(ior) + diff3 * hctau1(ior)
            endif
            
            if (hctau2(ior) > 0.0) then
               bssalg(mstr,ior) = bssalg(mstr,ior) - diff1 * hctau2(ior)
               bss(mstr,ior)    = bss(mstr,ior)    - diff2 * hctau2(ior)
               bfssgr(mstr,ior) = bfssgr(mstr,ior) - diff3 * hctau2(ior)
            endif
         enddo
         ilbuhn = 0
      endif
      
      ! -----------------------------------------------------------------------
      ! Schwermetalle
      ! -----------------------------------------------------------------------
      1519 continue
      if (ischwer == 1) then
         
         1521 continue
         call Schwermetalle(vabfl,qeinl,mstr,flag,anze,anzZeit,jiein,azStr,ieros,iformVert,ianze_max      &
                           ,hglZn,hgsZn,egsZn,eglZn,ZnSed       &
                           ,hglCad,hgsCad,egsCad,eglCad,CadSed  &
                           ,hglCu,hgsCu,egsCu,eglCu,CuSed       &
                           ,hglNi,hgsNi,egsNi,eglNi,NiSed       &
                           ,hglAs,hgsAs,egsAs,eglAs,AsSed       &
                           ,hglPb,hgsPb,egsPb,eglPb,PbSed       &
                           ,hglCr,hgsCr,egsCr,eglCr,CrSed       &
                           ,hglFe,hgsFe,egsFe,eglFe,FeSed       &
                           ,hglHg,hgsHg,egsHg,eglHg,HgSed       &
                           ,hglMn,hgsMn,egsMn,eglMn,MnSed       &
                           ,hglU,hgsU,egsU,eglU,USed            &
                           ,sedss,sedalk,sedalb,sedalg,hssalg,SSalg,ess,hph,vph,eph,SSeros      &
                           ,ilang,iwied                        &
                           ,.false., 0)
         if (nbuhn(mstr) == 0)goto 118
         if (ilbuhn == 0) then
            do ior = 1,anze+1
               hSSeros(mstr,ior) = SSeros(ior)
               hsedalk(mstr,ior) = sedalk(ior)
               hsedalg(mstr,ior) = sedalg(ior)
               hsedalb(mstr,ior) = sedalb(ior)
               hsedss(mstr,ior) = sedss(ior)
               zwsedk(ior) = sedalk(ior)
               zwsedg(ior) = sedalg(ior)
               zwsedb(ior) = sedalb(ior)
               zwsedS(ior) = sedss(ior)
               zwSSeros(ior) = SSeros(ior)
               zwssa(ior) = SSalg(ior)
               zwph(ior) = vph(ior)
               zwanzZeit(mstr,ior) = anzZeit(mstr,ior)
               zwjiein(ior) = jiein(ior); jiein(ior) = 0
               zwgsZn(ior) = hgsZn(mstr,ior)
               zwglZn(ior) = hglZn(mstr,ior)
               zwZnSed(ior) = ZnSed(mstr,ior)
               zwgsCad(ior) = hgsCad(mstr,ior)
               zwglCad(ior) = hglCad(mstr,ior)
               zwCadSed(ior) = CadSed(mstr,ior)
               zwgsCu(ior) = hgsCu(mstr,ior)
               zwglCu(ior) = hglCu(mstr,ior)
               zwCuSed(ior) = CuSed(mstr,ior)
               zwgsNi(ior) = hgsNi(mstr,ior)
               zwglNi(ior) = hglNi(mstr,ior)
               zwNiSed(ior) = NiSed(mstr,ior)
               zwgsAs(ior) = hgsAs(mstr,ior)
               zwglAs(ior) = hglAs(mstr,ior)
               zwAsSed(ior) = AsSed(mstr,ior)
               zwgsPb(ior) = hgsPb(mstr,ior)
               zwglPb(ior) = hglPb(mstr,ior)
               zwPbSed(ior) = PbSed(mstr,ior)
               zwgsCr(ior) = hgsCr(mstr,ior)
               zwglCr(ior) = hglCr(mstr,ior)
               zwCrSed(ior) = CrSed(mstr,ior)
               zwgsFe(ior) = hgsFe(mstr,ior)
               zwglFe(ior) = hglFe(mstr,ior)
               zwFeSed(ior) = FeSed(mstr,ior)
               zwgsHg(ior) = hgsHg(mstr,ior)
               zwglHg(ior) = hglHg(mstr,ior)
               zwHgSed(ior) = HgSed(mstr,ior)
               zwgsMn(ior) = hgsMn(mstr,ior)
               zwglMn(ior) = hglMn(mstr,ior)
               zwMnSed(ior) = MnSed(mstr,ior)
               zwgsU(ior) = hgsU(mstr,ior)
               zwglU(ior) = hglU(mstr,ior)
               zwUSed(ior) = USed(mstr,ior)
               sedalk(ior) = bsedak(mstr,ior)
               sedalg(ior) = bsedag(mstr,ior)
               sedalb(ior) = bsedab(mstr,ior)
               sedss(ior) = bsedss(mstr,ior)
               SSeros(ior) = bSSeros(ior)
               SSalg(ior) = bssalg(mstr,ior)
               bph(mstr,ior) = vph(ior)
               anzZeit(mstr,ior) = banzZeit(mstr,ior)
               hgsZn(mstr,ior) = bgsZn(mstr,ior)
               hglZn(mstr,ior) = bglZn(mstr,ior)
               ZnSed(mstr,ior) = bZnSed(mstr,ior)
               hgsCad(mstr,ior) = bgsCad(mstr,ior)
               hglCad(mstr,ior) = bglCad(mstr,ior)
               CadSed(mstr,ior) = bCadSed(mstr,ior)
               hgsCu(mstr,ior) = bgsCu(mstr,ior)
               hglCu(mstr,ior) = bglCu(mstr,ior)
               CuSed(mstr,ior) = bCuSed(mstr,ior)
               hgsNi(mstr,ior) = bgsNi(mstr,ior)
               hglNi(mstr,ior) = bglNi(mstr,ior)
               NiSed(mstr,ior) = bNiSed(mstr,ior)
               hgsAs(mstr,ior) = bgsAs(mstr,ior)
               hglAs(mstr,ior) = bglAs(mstr,ior)
               AsSed(mstr,ior) = bAsSed(mstr,ior)
               hgsPb(mstr,ior) = bgsPb(mstr,ior)
               hglPb(mstr,ior) = bglPb(mstr,ior)
               PbSed(mstr,ior) = bPbSed(mstr,ior)
               hgsCr(mstr,ior) = bgsCr(mstr,ior)
               hglCr(mstr,ior) = bglCr(mstr,ior)
               CrSed(mstr,ior) = bCrSed(mstr,ior)
               hgsFe(mstr,ior) = bgsFe(mstr,ior)
               hglFe(mstr,ior) = bglFe(mstr,ior)
               FeSed(mstr,ior) = bFeSed(mstr,ior)
               hgsHg(mstr,ior) = bgsHg(mstr,ior)
               hglHg(mstr,ior) = bglHg(mstr,ior)
               HgSed(mstr,ior) = bHgSed(mstr,ior)
               hgsMn(mstr,ior) = bgsMn(mstr,ior)
               hglMn(mstr,ior) = bglMn(mstr,ior)
               MnSed(mstr,ior) = bMnSed(mstr,ior)
               hgsU(mstr,ior) = bgsU(mstr,ior)
               hglU(mstr,ior) = bglU(mstr,ior)
               USed(mstr,ior) = bUSed(mstr,ior)
            enddo
            ilbuhn = 1
            goto 1521
         endif
         
         if (ilbuhn == 1) then
            do ior = 1,anze+1
               bgsZn(mstr,ior) = hgsZn(mstr,ior)
               bglZn(mstr,ior) = hglZn(mstr,ior)
               bZnSed(mstr,ior) = ZnSed(mstr,ior)
               bgsCad(mstr,ior) = hgsCad(mstr,ior)
               bglCad(mstr,ior) = hglCad(mstr,ior)
               bCadSed(mstr,ior) = CadSed(mstr,ior)
               bgsCu(mstr,ior) = hgsCu(mstr,ior)
               bglCu(mstr,ior) = hglCu(mstr,ior)
               bCuSed(mstr,ior) = CuSed(mstr,ior)
               bgsNi(mstr,ior) = hgsNi(mstr,ior)
               bglNi(mstr,ior) = hglNi(mstr,ior)
               bNiSed(mstr,ior) = NiSed(mstr,ior)
               bgsAs(mstr,ior) = hgsAs(mstr,ior)
               bglAs(mstr,ior) = hglAs(mstr,ior)
               bAsSed(mstr,ior) = AsSed(mstr,ior)
               bgsPb(mstr,ior) = hgsPb(mstr,ior)
               bglPb(mstr,ior) = hglPb(mstr,ior)
               bPbSed(mstr,ior) = PbSed(mstr,ior)
               bgsCr(mstr,ior) = hgsCr(mstr,ior)
               bglCr(mstr,ior) = hglCr(mstr,ior)
               bCrSed(mstr,ior) = CrSed(mstr,ior)
               bgsFe(mstr,ior) = hgsFe(mstr,ior)
               bglFe(mstr,ior) = hglFe(mstr,ior)
               bFeSed(mstr,ior) = FeSed(mstr,ior)
               bgsHg(mstr,ior) = hgsHg(mstr,ior)
               bglHg(mstr,ior) = hglHg(mstr,ior)
               bHgSed(mstr,ior) = HgSed(mstr,ior)
               bgsMn(mstr,ior) = hgsMn(mstr,ior)
               bglMn(mstr,ior) = hglMn(mstr,ior)
               bMnSed(mstr,ior) = MnSed(mstr,ior)
               bgsU(mstr,ior) = hgsU(mstr,ior)
               bglU(mstr,ior) = hglU(mstr,ior)
               bUSed(mstr,ior) = USed(mstr,ior)
               hgsZn(mstr,ior) = zwgsZn(ior)
               hglZn(mstr,ior) = zwglZn(ior)
               ZnSed(mstr,ior) = zwZnSed(ior)
               hgsCad(mstr,ior) = zwgsCad(ior)
               hglCad(mstr,ior) = zwglCad(ior)
               CadSed(mstr,ior) = zwCadSed(ior)
               hgsCu(mstr,ior) = zwgsCu(ior)
               hglCu(mstr,ior) = zwglCu(ior)
               CuSed(mstr,ior) = zwCuSed(ior)
               hgsNi(mstr,ior) = zwgsNi(ior)
               hglNi(mstr,ior) = zwglNi(ior)
               NiSed(mstr,ior) = zwNiSed(ior)
               hgsAs(mstr,ior) = zwgsAs(ior)
               hglAs(mstr,ior) = zwglAs(ior)
               AsSed(mstr,ior) = zwAsSed(ior)
               hgsPb(mstr,ior) = zwgsPb(ior)
               hglPb(mstr,ior) = zwglPb(ior)
               PbSed(mstr,ior) = zwPbSed(ior)
               hgsCr(mstr,ior) = zwgsCr(ior)
               hglCr(mstr,ior) = zwglCr(ior)
               CrSed(mstr,ior) = zwCrSed(ior)
               hgsFe(mstr,ior) = zwgsFe(ior)
               hglFe(mstr,ior) = zwglFe(ior)
               FeSed(mstr,ior) = zwFeSed(ior)
               hgsHg(mstr,ior) = zwgsHg(ior)
               hglHg(mstr,ior) = zwglHg(ior)
               HgSed(mstr,ior) = zwHgSed(ior)
               hgsMn(mstr,ior) = zwgsMn(ior)
               hglMn(mstr,ior) = zwglMn(ior)
               MnSed(mstr,ior) = zwMnSed(ior)
               hgsU(mstr,ior) = zwgsU(ior)
               hglU(mstr,ior) = zwglU(ior)
               USed(mstr,ior) = zwUSed(ior)
               sedalk(ior) = zwsedk(ior)
               sedalg(ior) = zwsedg(ior)
               sedalb(ior) = zwsedb(ior)
               sedss(ior) = zwsedS(ior)
               SSeros(ior) = zwSSeros(ior)
               SSalg(ior) = zwssa(ior)
               vph(ior) = zwph(ior)
               anzZeit(mstr,ior) = zwanzZeit(mstr,ior)
               jiein(ior) = zwjiein(ior)
               
               if (bleb(mstr,ior) > 0. .or. hctau2(ior) > 0.) then
                  diff1  = bgsZn(mstr,ior)  - hgsZn(mstr,ior)
                  diff2  = bglZn(mstr,ior)  - hglZn(mstr,ior)
                  diff3  = bgsCad(mstr,ior) - hgsCad(mstr,ior)
                  diff4  = bglCad(mstr,ior) - hglCad(mstr,ior)
                  diff5  = bgsCu(mstr,ior)  - hgsCu(mstr,ior)
                  diff6  = bglCu(mstr,ior)  - hglCu(mstr,ior)
                  diff7  = bgsNi(mstr,ior)  - hgsNi(mstr,ior)
                  diff8  = bglNi(mstr,ior)  - hglNi(mstr,ior)
                  diff9  = bgsAs(mstr,ior)  - hgsAs(mstr,ior)
                  diff10 = bglAs(mstr,ior)  - hglAs(mstr,ior)
                  diff11 = bgsPb(mstr,ior)  - hgsPb(mstr,ior)
                  diff12 = bglPb(mstr,ior)  - hglPb(mstr,ior)
                  diff13 = bgsCr(mstr,ior)  - hgsCr(mstr,ior)
                  diff14 = bglCr(mstr,ior)  - hglCr(mstr,ior)
                  diff15 = bgsFe(mstr,ior)  - hgsFe(mstr,ior)
                  diff16 = bglFe(mstr,ior)  - hglFe(mstr,ior)
                  diff17 = bgsHg(mstr,ior)  - hgsHg(mstr,ior)
                  diff18 = bglHg(mstr,ior)  - hglHg(mstr,ior)
                  diff19 = bgsMn(mstr,ior)  - hgsMn(mstr,ior)
                  diff20 = bglMn(mstr,ior)  - hglMn(mstr,ior)
                  diff21 = bgsU(mstr,ior)   - hgsU(mstr,ior)
                  diff22 = bglU(mstr,ior)   - hglU(mstr,ior)
               endif
               
               if (bleb(mstr,ior) > 0.0) then
                  hgsZn(mstr,ior)  = hgsZn(mstr,ior)  + diff1  * hctau1(ior)
                  hglZn(mstr,ior)  = hglZn(mstr,ior)  + diff2  * hctau1(ior)
                  hgsCad(mstr,ior) = hgsCad(mstr,ior) + diff3  * hctau1(ior)
                  hglCad(mstr,ior) = hglCad(mstr,ior) + diff4  * hctau1(ior)
                  hgsCu(mstr,ior)  = hgsCu(mstr,ior)  + diff5  * hctau1(ior)
                  hglCu(mstr,ior)  = hglCu(mstr,ior)  + diff6  * hctau1(ior)
                  hgsNi(mstr,ior)  = hgsNi(mstr,ior)  + diff7  * hctau1(ior)
                  hglNi(mstr,ior)  = hglNi(mstr,ior)  + diff8  * hctau1(ior)
                  hgsAs(mstr,ior)  = hgsAs(mstr,ior)  + diff9  * hctau1(ior)
                  hglAs(mstr,ior)  = hglAs(mstr,ior)  + diff10 * hctau1(ior)
                  hgsPb(mstr,ior)  = hgsPb(mstr,ior)  + diff11 * hctau1(ior)
                  hglPb(mstr,ior)  = hglPb(mstr,ior)  + diff12 * hctau1(ior)
                  hgsCr(mstr,ior)  = hgsCr(mstr,ior)  + diff13 * hctau1(ior)
                  hglCr(mstr,ior)  = hglCr(mstr,ior)  + diff14 * hctau1(ior)
                  hgsFe(mstr,ior)  = hgsFe(mstr,ior)  + diff15 * hctau1(ior)
                  hglFe(mstr,ior)  = hglFe(mstr,ior)  + diff16 * hctau1(ior)
                  hgsHg(mstr,ior)  = hgsHg(mstr,ior)  + diff17 * hctau1(ior)
                  hglHg(mstr,ior)  = hglHg(mstr,ior)  + diff18 * hctau1(ior)
                  hgsMn(mstr,ior)  = hgsMn(mstr,ior)  + diff19 * hctau1(ior)
                  hglMn(mstr,ior)  = hglMn(mstr,ior)  + diff20 * hctau1(ior)
                  hgsU(mstr,ior)   = hgsU(mstr,ior)   + diff21 * hctau1(ior)
                  hglU(mstr,ior)   = hglU(mstr,ior)   + diff22 * hctau1(ior)
               endif
               
               if (hctau2(ior) > 0.0) then
                  bgsZn(mstr,ior)  = bgsZn(mstr,ior)  - diff1  * hctau2(ior)
                  bglZn(mstr,ior)  = bglZn(mstr,ior)  - diff2  * hctau2(ior)
                  bgsCad(mstr,ior) = bgsCad(mstr,ior) - diff3  * hctau2(ior)
                  bglCad(mstr,ior) = bglCad(mstr,ior) - diff4  * hctau2(ior)
                  bgsCu(mstr,ior)  = bgsCu(mstr,ior)  - diff5  * hctau2(ior)
                  bglCu(mstr,ior)  = bglCu(mstr,ior)  - diff6  * hctau2(ior)
                  bgsNi(mstr,ior)  = bgsNi(mstr,ior)  - diff7  * hctau2(ior)
                  bglNi(mstr,ior)  = bglNi(mstr,ior)  - diff8  * hctau2(ior)
                  bgsAs(mstr,ior)  = bgsAs(mstr,ior)  - diff9  * hctau2(ior)
                  bglAs(mstr,ior)  = bglAs(mstr,ior)  - diff10 * hctau2(ior)
                  bgsPb(mstr,ior)  = bgsPb(mstr,ior)  - diff11 * hctau2(ior)
                  bglPb(mstr,ior)  = bglPb(mstr,ior)  - diff12 * hctau2(ior)
                  bgsCr(mstr,ior)  = bgsCr(mstr,ior)  - diff13 * hctau2(ior)
                  bglCr(mstr,ior)  = bglCr(mstr,ior)  - diff14 * hctau2(ior)
                  bgsFe(mstr,ior)  = bgsFe(mstr,ior)  - diff15 * hctau2(ior)
                  bglFe(mstr,ior)  = bglFe(mstr,ior)  - diff16 * hctau2(ior)
                  bgsHg(mstr,ior)  = bgsHg(mstr,ior)  - diff17 * hctau2(ior)
                  bglHg(mstr,ior)  = bglHg(mstr,ior)  - diff18 * hctau2(ior)
                  bgsMn(mstr,ior)  = bgsMn(mstr,ior)  - diff19 * hctau2(ior)
                  bglMn(mstr,ior)  = bglMn(mstr,ior)  - diff20 * hctau2(ior)
                  bgsU(mstr,ior)   = bgsU(mstr,ior)   - diff21 * hctau2(ior)
                  bglU(mstr,ior)   = bglU(mstr,ior)   - diff22 * hctau2(ior)
               endif
            enddo
            ilbuhn = 0
         endif ! ilbuhn==1
      else ! no heavy metals
         hgsZn(mstr,:) = 0.0
         hglZn(mstr,:) = 0.0
         hgsCad(mstr,:) = 0.0
         hglCad(mstr,:) = 0.0
         hgsCu(mstr,:) = 0.0
         hglCu(mstr,:) = 0.0
         hgsNi(mstr,:) = 0.0
         hglNi(mstr,:) = 0.0
         hgsAs(mstr,:) = 0.0
         hglAs(mstr,:) = 0.0
         hgsPb(mstr,:) = 0.0
         hglPb(mstr,:) = 0.0
         hgsCr(mstr,:) = 0.0
         hglCr(mstr,:) = 0.0
         hgsFe(mstr,:) = 0.0
         hglFe(mstr,:) = 0.0
         hgsHg(mstr,:) = 0.0
         hglHg(mstr,:) = 0.0
         hgsMn(mstr,:) = 0.0
         hglMn(mstr,:) = 0.0
         hgsU(mstr,:) = 0.0
         hglU(mstr,:) = 0.0
      endif ! ischwer==1
      
      ! -----------------------------------------------------------------------
      ! transportation
      ! -----------------------------------------------------------------------
      118 continue
      
      if (iwsim == 4 .and. ilang == 0 .or. itracer_vor == 1) then
      else
         izeits = STRiz(mstr)
         deltat = STRdt(mstr)
         jpoin1 = 0
         call Transport(anze,deltat,izeits,isub_dt,isub_dt_Mac,hvmitt,elen,flag,tempw,vo2,vnh4,vno3,vno2,vx0                  &
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
         
         ! Aufsummierung der Tracermasse
         if (iwsim == 4) then
            do ior = 1, anze
               sumTracer = sumTracer + ((tempw(ior)+tempw(ior+1))/2.) * vabfl(ior)
            enddo
         endif
         
         do ior = 1,anze+1
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
      endif
      
      itracer_vor = 0
      ! Mittelwertbildung des Sauerstoffgehalts des Chla-Gehaltes der
      ! Algenbiomassen und der Naehrstoffe bei 2D-Modellierung
      do ior = 1,anze+1
         if (nkzs(ior) == 1)cycle
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
      enddo
      
      ! Belegung des letzten Knoten bei nicht transportierten Groessen bzw
      ! Buhnenfelder)
      !
      53 continue
      Pfl(anze+1) = Pfl(anze)
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
      
      ! Prozessraten
      do ndr = 1,nndr
         idras(anze+1,ndr) = idras(anze,ndr)
         drmas(anze+1,ndr) = drmas(anze,ndr)
         drakr(anze+1,ndr) = drakr(anze,ndr)
         drbar(anze+1,ndr) = drbar(anze,ndr)
         Rzuwdr(anze+1,ndr) = Rzuwdr(anze,ndr)
         drmor(anze+1,ndr) = drmor(anze,ndr)
      enddo
      
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
      ! hier auch Belegung für alle anderen Groessen
      
      ! Buhnenfelder
      if (nbuhn(mstr) > 0) then
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
      endif
      
      dl(anze+1) = dl(anze)
      if (nbuhn(mstr) > 0) tau2(anze+1) = tau2(anze)
      
      ! Speichern von Informationen am Stranganfang und Strangende
      ianze(mstr) = anze+1
      inkzs(mstr,1) = nkzs(1)
      inkzs(mstr,2) = nkzs(anze+1)
      ikanz(1) = 1
      ikanz(2) = ianze(mstr)
      
      
      ! Umspeichern des ersten und letzten Gitterpunkts eines jeden Strangs
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
         
         ! Ufervegetation
         do iV = 1,14
            VTYPH(mstr,ior,iV) = VTYP(ior,iV)
         enddo
         VALTLH(mstr,ior) = VALTBL(ior)
         EDUFLH(mstr,ior) = EDUFBL(ior)
         VALTRH(mstr,ior) = VALTBR(ior)
         EDUFRH(mstr,ior) = EDUFBR(ior)
         
         ! Prozessraten
         do ndr = 1,nndr
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
         hbsbt(mstr,ior) = bsbt(ior)
         hdalgo(mstr,ior) = dalgo(ior)
         hdalao(mstr,ior) = dalgao(ior)
         hSedOM(mstr,ior) = hSedOM(mstr,ior)
         ! hw2(mstr,ior) = hw2(mstr,ior)
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
         
         hdl(mstr,ior) = dl(ior)
         htiefe(mstr,ior) = tiefe(ior)
         
         if (iwsim /= 4) then  ! wird bei Tracer übersprungen
            
            ! 2D-Modellierung
            hnkzs(mstr,ior) = nkzs(ior)
            hdH2de(mstr,ior) = dH2De(ior)
            do nkz = 1,nkzs(ior)
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
      
   enddo ! Ende Strangschleife
   
   7777 continue
   
   ! iwied = 0 : allererster Zeitschritt, danach iwied = 1
   ! ilang = 0 : Vorlauf (1d) wird nicht abgelegt, danach ilang = 1
   if (iwied == 0) then
      itagv = itags
      monatv = monats
      jahrv = jahrs
      uhrsv = uhrs
      iwied = 1
   endif
   
   if (jlauf == 0) then ! Berechnung der neuen Uhrzeit und des neuen Datums
      
      Uhrz = Uhrz+tflie*24.
      if ((24.-Uhrz) < 0.0001)Uhrz = 24.
      if (Uhrz>=24.) then
         Uhrz = Uhrz-24.
         if (jtag /= 1)itags = itags+1
      endif
      
      call anztag(monats,jahrs,jtage)
      if (itags > jtage) then
         itags = 1
         monats = monats+1
      endif
      if (monats > 12) then
         monats = 1.
         jahrs = jahrs+1
      endif
   endif
   
   
   
   ! Vorlauf ilang = 0; Werte werden nicht abgelegt
   if (ilang == 0 .and. ij < itime) then
      print "(a,i0,a,i0,a)", " Vorlauf (", ij, "/", itime ,")"
      ij = ij+1
      istr = 0
      goto 9191  ! Beim Vorlauf werden keine neuen Randwerte gelesen
   endif
   
   if (ilang == 0 .and. ij == itime) then
      itracer_vor = 0
      itags = itagv
      monats = monatv
      jahrs = jahrv
      itime = itimea
      ianfan = 1
      ij = 1
      if (iwsim == 4)itracer_vor = 1
      rewind(110)
      read(110,'(A2)')ckenn_vers1
      if (ckenn_vers1 /= '*V') then
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
         if (itags == itag_Schr .and. monats == monat_Schr.and.Jahrs == Jahr_Schr.and.uhrsv == Uhrz_Schr) then
            backspace(unit = 110)
            exit
         endif
         do i = 1,isumAnzSta
            read(110,'(2x)')
         enddo
         cycle
      enddo
      9708 format(I5,2x,i2,2x,i2,2x,i4,2x,f5.2)
      
      rewind (97)
      read(97,'(A2)')ckenn_vers1
      if (ckenn_vers1 /= '*V') then
         read(97,'(A40)')ERENAME
         read(97,'(I5)')SCHRNR
      else
         read(97,'(A40)')MODNAME
         read(97,'(A40)')ERENAME
         !              read(97,'(I5)')SCHRNR
      endif
      do  ! Suchen des Ereignisbeginns in Ablauf.txt
         read(97,9705,iostat = read_error)SCHRNR,jkenn,itags_Schr, monat_Schr, Jahr_Schr, Uhrz_Schr  ! Lesen der Zeitschritt-Nummer
         if (jkenn == 99) then
            if (itags == itags_Schr .and. monats == monat_Schr.and.Jahrs == Jahr_Schr.and.uhrsv == Uhrz_Schr)exit
            cycle
         endif
      enddo
   endif
   
   
   if (ij == 1 .and. jlauf == 0) then
      if (itags == itage .and. monats == monate.and.jahrs == jahre)itime = itimee
   endif
   if (ilang == 1 .and. jlauf == 0) then
      jlauf = 1
      istr = 0
      goto 9999      ! Lesen neuer Randbedingungen
   endif
   
   if (ilang == 0) then
      ilang = 1
      jtag = 1
      print '(" Vorlauf (",I0,"/",I0,")")', iTime, iTime
      print*, repeat('-', 78)
      goto 9191  ! Es werden keine neuen Randwerte gelesen
   endif
   
   if (jlauf == 1) then
      ij = ij+1
      jlauf = 0
      jtag = 0
      hconU = abs(uhren-uhrz)
      if (hconU < 0.001)Uhrz = uhren
      if (itags == itage .and. monats == monate.and.jahrs == jahre.and.uhren == uhrz.and.ilang == 1)iend = 1
   endif
   
   if (ij > itime)maus = 1
   
   
   
   ! ==========================================================================
   ! * Summenbildung Minimum und Maximum beim Hauptlauf
   ! * Ausschreiben von Ergebnissen
   ! ==========================================================================
   if (ij <= 2) then
   
      ! Minimums- und Maximumswerte initieren
      mitemp = 999999.9
      mxtemp = 0.0
      minh4  = 999999.9
      mxnh4  = 0.0
      migsP  = 99999.9
      migsN  = 99999.9
      mxgsP  = 0.0
      mxgsN  = 0.0
      mib5   = 999999.9
      mxb5   = 0.0
      mics   = 999999.9
      mxcs = 0.0
      michla = 9999999.9
      mxchla = 0.0
      miaki = 999999.9
      mxaki = 0.0
      miagr = 999999.9
      mxagr = 0.0
      miabl = 999999.9
      mxabl = 0.0
      mio2 = 9999999.9
      mxo2 = 0.0
      mizo = 99999999.9
      mxzo = 0.0
      mivph = 9999999.9
      mxvph = 0.0
      mivno3 = 999999.9
      mxvno3 = 0.0
      migp = 999999.9
      mxgp = 0.0
      misi = 999999.9
      mxsi = 0.0
      mivno2 = 999999.9
      mxvno2 = 0.0
      mica = 9999999.9
      mxca = 0.0
      mimw = 999999.9
      mxmw = 0.0
      milf = 999999.9
      mxlf = 0.0
      micoli = 9999999999999999.9
      mxcoli = 0.0
      midlan = 99999999.9
      mxdlan = 0.0
      miSS = 99999999.9
      mxSS = 0.0
      migsZn = 99999999.9
      mxgsZn = 0.0
      miglZn = 99999999.9
      mxglZn = 0.0
      migsCad = 99999999.9
      mxgsCad = 0.0
      miglCad = 99999999.9
      mxglCad = 0.0
      migsCu = 99999999.9
      mxgsCu = 0.0
      miglCu = 99999999.9
      mxglCu = 0.0
      migsNi = 99999999.9
      mxgsNi = 0.0
      miglNi = 99999999.9
      mxglNi = 0.0
      migsAs = 99999999.9
      mxgsAs = 0.0
      miglAs = 99999999.9
      mxglAS = 0.0
      migsPb = 99999999.9
      mxgsPb = 0.0
      miglPb = 99999999.9
      mxglPb = 0.0
      migsCr = 99999999.9
      mxgsCr = 0.0
      miglCr = 99999999.9
      mxglCr = 0.0
      migsFe = 99999999.9
      mxgsFe = 0.0
      miglFe = 99999999.9
      mxglFe = 0.0
      migsHg = 99999999.9
      mxgsHg = 0.0
      miglHg = 99999999.9
      mxglHg = 0.0
      migsMn = 99999999.9
      mxgsMn = 0.0
      miglMn = 99999999.9
      mxglMn = 0.0
      migsU = 99999999.9
      mxgsU = 0.0
      miglU = 99999999.9
      mxglU = 0.0
      
      ! Buhnenfelder
      do mstr = 1,azStrs
         if (nbuhn(mstr) == 0) cycle
            
         bmxtem(mstr,:) = 0.0
         bmitem(mstr,:) = 9999999.9
         bmxno3(mstr,:) = 0.0
         bmino3(mstr,:) = 999999.9
         bmxno2(mstr,:) = 0.0
         bmino2(mstr,:) = 999999.9
         bmxnh4(mstr,:) = 0.0
         bminh4(mstr,:) = 999999.9
         bmxbsb(mstr,:) = 0.0
         bmibsb(mstr,:) = 999999.9
         bmxcsb(mstr,:) = 0.0
         bmicsb(mstr,:) = 999999.9
         bmxglp(mstr,:) = 0.0
         bmiglp(mstr,:) = 999999.9
         bmxchl(mstr,:) = 0.0
         bmichl(mstr,:) = 9999999.9
         bmxssa(mstr,:) = 0.0
         bmissa(mstr,:) = 999999.9
         bmxsi(mstr,:) = 0.0
         bmisi(mstr,:) = 9999999.9
         bmxzoo(mstr,:) = 0.0
         bmizoo(mstr,:) = 9999999.9
         bmxgsP(mstr,:) = 0.0
         bmigsP(mstr,:) = 9999999.9
         bmxgsN(mstr,:) = 0.0
         bmigsN(mstr,:) = 9999999.9
         bmxaki(mstr,:) = 0.0
         bmiaki(mstr,:) = 9999999.9
         bmxagr(mstr,:) = 0.0
         bmiagr(mstr,:) = 9999999.9
         bmxo2(mstr,:) = 0.0
         bmio2(mstr,:) = 9999999.9
         bmxmw(mstr,:) = 0.0
         bmimw(mstr,:) = 9999999.9
         bmxlf(mstr,:) = 0.0
         bmilf(mstr,:) = 9999999.9
         bmxca(mstr,:) = 0.0
         bmica(mstr,:) = 9999999.9
         bmxph(mstr,:) = 0.0
         bmiph(mstr,:) = 9999999.9
         bmxgsZn(mstr,:) = 0.0
         bmigsZn(mstr,:) = 9999999.9
         bmxglZn(mstr,:) = 0.0
         bmiglZn(mstr,:) = 9999999.9
         bmxgsCad(mstr,:) = 0.0
         bmigsCad(mstr,:) = 9999999.9
         bmxglCad(mstr,:) = 0.0
         bmiglCad(mstr,:) = 9999999.9
         bmxgsCu(mstr,:) = 0.0
         bmigsCu(mstr,:) = 9999999.9
         bmxglCu(mstr,:) = 0.0
         bmiglCu(mstr,:) = 9999999.9
         bmxgsNi(mstr,:) = 0.0
         bmigsNi(mstr,:) = 9999999.9
         bmxglNi(mstr,:) = 0.0
         bmiglNi(mstr,:) = 9999999.9
         bmxgsAs(mstr,:) = 0.0
         bmigsAs(mstr,:) = 9999999.9
         bmxglAs(mstr,:) = 0.0
         bmiglAs(mstr,:) = 9999999.9
         bmxgsPb(mstr,:) = 0.0
         bmigsPb(mstr,:) = 9999999.9
         bmxglPb(mstr,:) = 0.0
         bmiglPb(mstr,:) = 9999999.9
         bmxgsCr(mstr,:) = 0.0
         bmigsCr(mstr,:) = 9999999.9
         bmxglCr(mstr,:) = 0.0
         bmiglCr(mstr,:) = 9999999.9
         bmxgsFe(mstr,:) = 0.0
         bmigsFe(mstr,:) = 9999999.9
         bmxglFe(mstr,:) = 0.0
         bmiglFe(mstr,:) = 9999999.9
         bmxgsHg(mstr,:) = 0.0
         bmigsHg(mstr,:) = 9999999.9
         bmxglHg(mstr,:) = 0.0
         bmiglHg(mstr,:) = 9999999.9
         bmxgsMn(mstr,:) = 0.0
         bmigsMn(mstr,:) = 9999999.9
         bmxglMn(mstr,:) = 0.0
         bmiglMn(mstr,:) = 9999999.9
         bmxgsU(mstr,:) = 0.0
         bmigsU(mstr,:) = 9999999.9
         bmxglU(mstr,:) = 0.0
         bmiglU(mstr,:) = 9999999.9
      enddo

      
      ! Nullsetzen der Summen für Mittelwertberechnung der Ausgaben
      sumte = 0.0
      sCHNF = 0.0
      sBVHNF = 0.0
      sCD = 0.0
      sCP = 0.0
      sCM = 0.0
      sBAC = 0.0
      sgsP = 0.0
      sgsN = 0.0
      sumb5 = 0.0
      sumcs = 0.0
      sumn4 = 0.0
      sumo2 = 0.0
      sumca = 0.0
      sumcak = 0.0
      sumcag = 0.0
      sumcab = 0.0
      sumCChlk = 0.0
      sumCChlg = 0.0
      sumCChlb = 0.0
      svkigr = 0.0
      santbl = 0.0
      sumaki = 0.0
      sumagr = 0.0
      sumabl = 0.0
      sumzo = 0.0
      sumvph = 0.0
      sumno3 = 0.0
      sumno2 = 0.0
      sumgp = 0.0
      sumsi = 0.0
      summsl = 0.0
      sumcal = 0.0
      summw = 0.0
      sumlf = 0.0
      scoli = 0.0
      sumdln = 0.0
      sumgsZn = 0.0
      sumglZn = 0.0
      sumgsCad = 0.0
      sumglCad = 0.0
      sumgsCu = 0.0
      sumglCu = 0.0
      sumgsNi = 0.0
      sumglNi = 0.0
      sumgsAs = 0.0
      sumglAs = 0.0
      sumgsPb = 0.0
      sumglPb = 0.0
      sumgsCr = 0.0
      sumglCr = 0.0
      sumgsFe = 0.0
      sumglFe = 0.0
      sumgsHg = 0.0
      sumglHg = 0.0
      sumgsMn = 0.0
      sumglMn = 0.0
      sumgsU = 0.0
      sumglU = 0.0
      
      ! Dreissena
      sidras(:,:,1:nndr) = 0.0
      sdrmas(:,:,1:nndr) = 0.0
      sdrakr(:,:,1:nndr) = 0.0
      sdrbar(:,:,1:nndr) = 0.0
      sdrmor(:,:,1:nndr) = 0.0
      szdrg(:,:,1:nndr)  = 0.0
      szdrsg(:,:,1:nndr) = 0.0
      sgwdrg(:,:,1:nndr) = 0.0

      scorIg = 0.0
      scoIsg = 0.0
      
      sumpfl = 0.0
      sumss = 0.0
      sumbal = 0.0
      
      ! Raten
      ssusn = 0.0
      sbettn = 0.0
      sdon = 0.0
      salgn = 0.0
      salNO3 = 0.0
      sFluN3 = 0.0
      sJNO3 = 0.0
      sJNH4 = 0.0
      sJPO4 = 0.0
      sJO2 = 0.0
      sJSi = 0.0
      svx0 = 0.0
      svx02 = 0.0
      ssedx0 = 0.0
      ssusno = 0.0
      ssedal = 0.0
      salgzo = 0.0
      salgdr = 0.0
      salgco = 0.0
      svoldr = 0.0
      sdrpfe = 0.0
      sabeow = 0.0
      sabeor = 0.0
      sdalg = 0.0
      sdalga = 0.0
      salmor = 0.0
      sblmor = 0.0
      ssgo2n = 0.0
      ssdbsb = 0.0
      ssoein = 0.0
      ssalgo = 0.0
      sbsbt = 0.0
      s2algo = 0.0
      s2algao = 0.0
      sschlr = 0.0
      sbsbbe = 0.0
      so2phy = 0.0
      sro2dr = 0.0
      szooro = 0.0
      spo2p = 0.0
      spo2r = 0.0
      sir = 0.0
      srmue = 0.0
      srakr = 0.0
      srbar = 0.0
      sffood = 0.0
      sfik = 0.0
      sfig = 0.0
      sfib = 0.0
      sakmua = 0.0
      sagmua = 0.0
      sabmua = 0.0
      sfheka = 0.0
      sfhega = 0.0
      sfheba = 0.0
      sakrau = 0.0
      sagrea = 0.0
      sabrea = 0.0
      sHNFmu = 0.0
      sHNFre = 0.0
      sHNFup = 0.0
      sHNFmo = 0.0
      sHNFex = 0.0
      sHNFdr = 0.0
      sHNFz = 0.0
      sBACmu = 0.0
      sHNFBA = 0.0
      snl0 = 0.0
      spl0 = 0.0
      snaehr = 0.0
            
      ! Buhnenfelder
      do mstr = 1, azStrs
         if (nbuhn(mstr) == 0) cycle
         
         bste(mstr,:) = 0.0
         bsno3(mstr,:) = 0.0
         bsno2(mstr,:) = 0.0
         bsn4(mstr,:) = 0.0
         bsgelp(mstr,:) = 0.0
         bschla(mstr,:) = 0.0
         bsaki(mstr,:) = 0.0
         bsagr(mstr,:) = 0.0
         bsabl(mstr,:) = 0.0
         bsssal(mstr,:) = 0.0
         bssi(mstr,:) = 0.0
         bszooi(mstr,:) = 0.0
         bsvbsb(mstr,:) = 0.0
         bsvcsb(mstr,:) = 0.0
         bsgsP(mstr,:) = 0.0
         bsgsN(mstr,:) = 0.0
         bso2(mstr,:) = 0.0
         bsmw(mstr,:) = 0.0
         bslf(mstr,:) = 0.0
         bsca(mstr,:) = 0.0
         bsph(mstr,:) = 0.0
         bsnl0(mstr,:) = 0.0
         bspl0(mstr,:) = 0.0
         bscoli(mstr,:) = 0.0
         bsgsZn(mstr,:) = 0.0
         bsglZn(mstr,:) = 0.0
         bsgsCad(mstr,:) = 0.0
         bsglCad(mstr,:) = 0.0
         bsgsCu(mstr,:) = 0.0
         bsglCu(mstr,:) = 0.0
         bsgsNi(mstr,:) = 0.0
         bsglNi(mstr,:) = 0.0
         bsgsAs(mstr,:) = 0.0
         bsglAs(mstr,:) = 0.0
         bsgsPb(mstr,:) = 0.0
         bsglPb(mstr,:) = 0.0
         bsgsCr(mstr,:) = 0.0
         bsglCr(mstr,:) = 0.0
         bsgsFe(mstr,:) = 0.0
         bsglFe(mstr,:) = 0.0
         bsgsHg(mstr,:) = 0.0
         bsglHg(mstr,:) = 0.0
         bsgsMn(mstr,:) = 0.0
         bsglMn(mstr,:) = 0.0
         bsgsU(mstr,:) = 0.0
         bsglU(mstr,:) = 0.0
         
         ! Raten
         bsdalg(mstr,:) = 0.0
         bsvkg(mstr,:) = 0.0
         bsantb(mstr,:) = 0.0
         bsdaa(mstr,:) = 0.0
         bsseda(mstr,:) = 0.0
         bsalgz(mstr,:) = 0.0
         bsamor(mstr,:) = 0.0
         bsadr(mstr,:) = 0.0
         ! Pseudofaces = 0.0 da keine Dreissena in Buhnenfeld
         bsalco(mstr,:) = 0.0
         bsfik(mstr,:) = 0.0
         bsfig(mstr,:) = 0.0
         bskmue(mstr,:) = 0.0
         bsgmue(mstr,:) = 0.0
         bsbmue(mstr,:) = 0.0
         bshek(mstr,:) = 0.0
         bsheg(mstr,:) = 0.0
         bskre(mstr,:) = 0.0
         bsgre(mstr,:) = 0.0
         bsbre(mstr,:) = 0.0
         bschlk(mstr,:) = 0.0
         bschlg(mstr,:) = 0.0
         bschlb(mstr,:) = 0.0
         bnaehr(mstr,:) = 0.0
         bsFlN3(mstr,:) = 0.0
         bsbetN(mstr,:) = 0.0
         bsJNO3(mstr,:) = 0.0
         bsJNH4(mstr,:) = 0.0
         bsJPO4(mstr,:) = 0.0
         bsJO2(mstr,:) = 0.0
         bsJSi(mstr,:) = 0.0
      enddo
   
   endif
   
   
   ! ==========================================================================
   ! Belegung des Ausgabegitters
   ! ==========================================================================
   4545 continue
   
   do azStr = 1,azStrs                  ! Beginn Strangschleife
      mstr = mstra(azStr)
      mSta = 0
      do iior = 1,hanze(mstr)+1                ! Beginn Knotenschleife
         if (hflag(mstr,iior) == 6) cycle
         mSta = mSta+1
         
         tiefey(mSta) = htiefe(mstr,iior)
         tempwy(mSta) = htempw(mstr,iior)
         it_hy(mstr,msta) = it_h(mstr,iior)
         if (iwsim == 4) then
            tracer(mSta) = tempwy(mSta)
            if (tracer(mSta) < 0.001)tracer(mSta) = 0.0
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
         
         do ndr = 1,nndr
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
         
         ! Makrophythen
         pfly(mSta) = hpfl(mstr,iior)
         
         ! benthische Algen
         alby(mSta)   = habgml(mstr,iior)    &
                      + habkml(mstr,iior)
         
         coroy(mSta)  = hcoro2(mstr,iior,1)  &
                      + hcoro2(mstr,iior,2)  &
                      + hcoro2(mstr,iior,3)  &
                      + hcoro2(mstr,iior,4)  &
                      + hcoro2(mstr,iior,5)
         
         corosy(mSta) = hcos2(mstr,iior,1)   &
                      + hcos2(mstr,iior,2)   &
                      + hcos2(mstr,iior,3)   &
                      + hcos2(mstr,iior,4)   &
                      + hcos2(mstr,iior,5)
         
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
         gsAsy(mSta) = hgsAs(mstr,iior)
         glAsy(mSta) = hglAs(mstr,iior)
         gsPby(mSta) = hgsPb(mstr,iior)
         glPby(mSta) = hglPb(mstr,iior)
         gsCry(mSta) = hgsCr(mstr,iior)
         glCry(mSta) = hglCr(mstr,iior)
         gsFey(mSta) = hgsFe(mstr,iior)
         glFey(mSta) = hglFe(mstr,iior)
         gsHgy(mSta) = hgsHg(mstr,iior)
         glHgy(mSta) = hglHg(mstr,iior)
         gsMny(mSta) = hgsMn(mstr,iior)
         glMny(mSta) = hglMn(mstr,iior)
         gsUy(mSta) = hgsU(mstr,iior)
         glUy(mSta) = hglU(mstr,iior)
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
         ! w2(mstr,mSta) = hw2(mstr,iior)
         dkorn(mstr,mSta) = hdkorn(mstr,iior)
         
         dly(mSta) = hdl(mstr,iior)
         
         gsPy(mSta) = hgesP(mstr,iior)
         gsNy(mSta) = hgesN(mstr,iior)
         
         if (BACy(mSta) > 0.0) then
            HNFBAy(mSta) = hHNFBA(mstr,iior)/BACy(mSta)
         endif
         
         ! Buhnenfelder
         if (nbuhn(mstr) == 1) then
            btempy(mSta) = btempw(mstr,iior)
            if (iwsim == 4) then
               btracer(mSta) = btempy(mSta)
               if (btracer(mSta) < 0.001)btracer(mSta) = 0.0
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
            bgsAsy(msta) = bgsAs(mstr,iior)
            bglAsy(msta) = bglAs(mstr,iior)
            bgsPby(msta) = bgsPb(mstr,iior)
            bglPby(msta) = bglPb(mstr,iior)
            bgsCry(msta) = bgsCr(mstr,iior)
            bglCry(msta) = bglCr(mstr,iior)
            bgsFey(msta) = bgsFe(mstr,iior)
            bglFey(msta) = bglFe(mstr,iior)
            bgsHgy(msta) = bgsHg(mstr,iior)
            bglHgy(msta) = bglHg(mstr,iior)
            bgsMny(msta) = bgsMn(mstr,iior)
            bglMny(msta) = bglMn(mstr,iior)
            bgsUy(msta) = bgsU(mstr,iior)
            bglUy(msta) = bglU(mstr,iior)
            
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
            
            bgsPy(mSta) = bgesP(mstr,iior)
            bgsNy(mSta) = bgesN(mstr,iior)
            bFlN3y(mSta) = bFluN3(mstr,iior)
            tau2y(mSta) = htau2(mstr,iior)
            SedOMb(mstr,mSta) = bSedOM(mstr,iior)
            w2b(mstr,mSta) = bw2(mstr,iior)
            dkornb(mstr,mSta) = bdkorn(mstr,iior)
         endif
         
         
         ! 2D-Modellierung
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
         if (hdH2De(mstr,iior) > 0.0) then
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
         
         ! --------------------------------------------------------------------
         ! Ausschreiben der stündlichen Werte, falls imitt = 1
         ! --------------------------------------------------------------------
         if (imitt == 1) then
         
            if (iwsim == 4) then
               vbsby(iior)  = -1.
               vcsby(iior)  = -1.
               vnh4y(iior)  = -1.
               vno2y(iior)  = -1.
               vno3y(iior)  = -1.
               gsNy(iior)   = -1.
               gelpy(iior)  = -1.
               gsPy(iior)   = -1.
               Siy(iior)    = -1.
               chlay(iior)  = -1.
               zooiny(iior) = -1.
               vphy(iior)   = -1.
               mwy(iior)    = -1.
               cay(iior)    = -1.
               lfy(iior)    = -1.
               ssalgy(iior) = -1.
               vo2y(iior)   = -1.
               CHNFy(iior)  = -1.
               coliy(iior)  = -1.
               tempwy(iior) = -1.
            else
               tracer = -1.
            endif
            
            ! Umrechnung der Uhrz in h.mm
            Stunde = int(Uhrz)
            hcmin = (Uhrz-Stunde)*60.
            minute = nint(hcmin)
            if (minute == 60) then
               minute = 0
               Stunde = Stunde+1
            endif
            rmin = minute/100.
            Uhrhm = Stunde+rmin
            
            write(155,5103)itags,monats,jahrs,uhrhm,mstr,Stakm(mstr,iior),STRID(mstr)
            
            write(155,5105)vbsby(iior),vcsby(iior)                                           &
                           ,vnh4y(iior),vno2y(iior),vno3y(iior),gsNy(iior),gelpy(iior)       &
                           ,gsPy(iior),Siy(iior),chlay(iior),zooiny(iior),vphy(iior)         &
                           ,mwy(iior),cay(iior),lfy(iior),ssalgy(iior),tempwy(iior)          &
                           ,vo2y(iior),CHNFy(iior),coliy(iior),Dly(iior),dsedH(mstr,iior)    &
                           ,tracer(iior)
                    
            ! Write results to csv-files for debugging
            if (write_csv_output) then 
               write(langezeile,*)itags,';',monats,';',jahrs,';',uhrhm,';',mstr,';',Stakm(mstr,iior),';',STRID(mstr)                   &
                                  ,';',vbsby(iior),';',vcsby(iior),';',vnh4y(iior),';',vno2y(iior),';',vno3y(iior),';',gsNy(iior),';',gelpy(iior)  &
                                  ,';',gsPy(iior),';',Siy(iior),';',chlay(iior),';',zooiny(iior),';',vphy(iior),';',mwy(iior),';',cay(iior)        &
                                  ,';',lfy(iior),';',ssalgy(iior),';',tempwy(iior),';',vo2y(iior),';',CHNFy(iior),';',coliy(iior),';',Dly(iior)    &
                                  ,';',dsedH(mstr,iior),';',tracer(iior)
               write(156,'(a)')adjustl(trim(langezeile))
               
               
               write(langezeile,*)itags,';',monats,';',jahrs,';',uhrhm,';',mstr,';',Stakm(mstr,iior),';',STRID(mstr),';'                &
                                  ,gsPby(iior),';',glPby(iior),';',gsCady(iior),';',glCady(iior),';',gsCry(iior),';',glCry(iior),';'     &
                                  ,gsFey(iior),';',glFey(iior),';',gsCuy(iior),';' ,glCuy(iior),';' ,gsMny(iior),';',glMny(iior),';'     &
                                  ,gsNiy(iior),';',glNiy(iior),';',gsHgy(iior),';' ,glHgy(iior),';' ,gsUy(iior) ,';' ,glUy(iior),';'     &
                                  ,gsZny(iior),';',glZny(iior),';',gsAsy(iior),';' ,glAsy(iior)                                          &
                                  ,hSSeros(mstr,iior),';',hsedalk(mstr,iior),';',hsedalg(mstr,iior),';',hsedalb(mstr,iior),';',hsedss(mstr,iior)
               write(157,'(a)')adjustl(trim(langezeile))
               write(langezeile,*)itags,';',monats,';',jahrs,';',uhrhm,';',mstr,';',Stakm(mstr,iior),';',STRID(mstr),';'                  &
                                  ,ho2(mstr,iior),';',hchla(mstr,iior),';',haki(mstr,iior),';',hagr(mstr,iior),';',habl(mstr,iior),';'  &
                                  ,hchlak(mstr,iior),';',hchlag(mstr,iior),';',hchlab(mstr,iior),';',hssalg(mstr,iior),';',hss(mstr,iior)
               write(158,'(a)')adjustl(trim(langezeile))
            endif
            
            write(155,5205)(bsbty(iior)*hcUmt),(susNOy(iior)*hcUmt),(O2ei1y(iior)*hcUmt)                           &
                           ,(dalgoy(iior)*hcUmt),(cchlky(iior)*hcUmt),(cchlgy(iior)*hcUmt),(cchlby(iior)*hcUmt)    &
                           ,(zoro2y(iior)*hcUmt),(schlry(iior)*hcUmt),(bettny(iior)*hcUmt)
            
            if (nbuhn(mstr) /= 1 .or. iwsim == 4) then
               btempy(iior) = -1.
               bvbsby(iior) = -1.
               bvcsby(iior) = -1
               bnh4y(iior)  = -1.
               bno2y(iior)  = -1.
               bno3y(iior)  = -1.
               bgsNy(iior)  = -1.
               bgelpy(iior) = -1.
               bgsPy(iior)  = -1.
               bsiy(iior)   = -1.
               bchlay(iior) = -1.
               bzooiy(iior) = -1.
               bphy(iior)   = -1.
               bmwy(iior)   = -1.
               bcay(iior)   = -1.
               blfy(iior)   = -1.
               bssaly(iior) = -1.
               btempy(iior) = -1.
               bo2y(iior)   = -1.
               if (nbuhn(mstr) == 0)tau2y(iior) = -1.
            endif
            
            bcoliy = -1.
            bHNFy = -1.
            if (nbuhn(mstr) /= 1 .or. iwsim /= 4) btracer(iior) = -1.
            
            write(155,5115)bvbsby(iior),bvcsby(iior),bnh4y(iior)                          &
                           ,bno2y(iior),bno3y(iior),bgsNy(iior),bgelpy(iior),bgsPy(iior)  &
                           ,bsiy(iior),bchlay(iior),bzooiy(iior),bphy(iior),bmwy(iior)    &
                           ,bcay(iior),blfy(iior),bssaly(iior),btempy(iior),bo2y(iior)    &
                           ,bHNFy,bcoliy(iior),tau2y(iior),btracer(iior)
            
            5103 format(i2,2X,i2,2x,i4,2x,f5.2,2x,i5,2x,f8.3,2x,I5)
            
            5104 format(i2,2X,i2,2x,i4,2x,f5.2,2x,i5,2x,f8.3,2x,I2,2x,I2,2x,I5)
            
            5105 format(f6.2,2x,f6.2,2x,f6.2,2x,f6.3,2x,f9.6,2x,f5.2,2x,f6.3      &
                        ,2x,f5.2,2x,f5.2,2x,f6.2,2x,f7.1,2x,f5.2,2x,f5.2,2x,f5.1  &
                        ,2x,f8.1,2x,f6.2,2x,f5.2,2x,f5.2,2x,f8.1,2x,E9.2,2x,f7.2  &
                        ,2x,f12.6,2x,f7.3)
                        
            5115 format(f6.2,2x,f6.2,2x,f6.2,2x,f6.3,2x,f9.6,2x,f5.2,2x,f5.3      &
                        ,2x,f5.2,2x,f5.2,2x,f6.2,2x,f7.1,2x,f5.2,2x,f5.2,2x,f5.1  &
                        ,2x,f6.1,2x,f6.2,2x,f5.2,2x,f5.2,2x,f8.1,2x,E9.3,2x,f7.3  &
                        ,2x,f9.3)
            
            5205 format(f8.6,2x,f8.6,2x,f8.6,2x,f8.6,2x,f6.2,2x,f6.2,2x,f6.2,2x,F8.6,2x,f10.8,2x,f8.6)
            
            5207 format(F6.2,2x,F6.2,2x,F7.3,2x,F7.3,2x,F6.2,2x,F6.2,2x,F8.1,2x,F8.1,2x,F6.2,2x,F6.2      &
                        ,2x,F8.1,2x,F8.1,2x,F6.2,2x,F6.2,2x,F7.3,2x,F7.3,2x,F7.3,2x,F7.3,2x,F8.1,2x,F8.1  &
                        ,2x,F5.1,2x,F5.1)
            
         endif         
         
         ! --------------------------------------------------------------------
         ! Summenbildung fuer Ausgabe
         ! --------------------------------------------------------------------
         sumte(mstr,iior) = sumte(mstr,iior)+tempwy(iior)
         if (tempwy(iior) > mxtemp(mstr,iior)) mxtemp(mstr,iior) = tempwy(iior)
         if (tempwy(iior) < mitemp(mstr,iior)) mitemp(mstr,iior) = tempwy(iior)
         
         sCHNF(mstr,iior)  = sCHNF(mstr,iior) + CHNFy(iior)
         sBVHNF(mstr,iior) = sBVHNF(mstr,iior) + BVHNFy(iior)
         sCD(mstr,1,iior)  = sCD(mstr,1,iior) + CDy(1,iior)
         sCD(mstr,2,iior)  = sCD(mstr,2,iior) + CDy(2,iior)
         sCP(mstr,1,iior)  = sCP(mstr,1,iior) + CPy(1,iior)
         sCP(mstr,2,iior)  = sCP(mstr,2,iior) + CPy(2,iior)
         sCM(mstr,iior)    = sCM(mstr,iior) + CMy(iior)
         sBAC(mstr,iior)   = sBAC(mstr,iior) + BACy(iior)
         
         sgsP(mstr,iior) = sgsP(mstr,iior)+gsPy(iior)
         if (gsPy(iior) > mxgsP(mstr,iior))mxgsP(mstr,iior) = gsPy(iior)
         if (gsPy(iior) < migsP(mstr,iior))migsP(mstr,iior) = gsPy(iior)
         
         sgsN(mstr,iior) = sgsN(mstr,iior)+gsNy(iior)
         if (gsNy(iior) > mxgsN(mstr,iior))mxgsN(mstr,iior) = gsNy(iior)
         if (gsNy(iior) < migsN(mstr,iior))migsN(mstr,iior) = gsNy(iior)
         
         sumb5(mstr,iior) = sumb5(mstr,iior)+vbsby(iior)
         if (vbsby(iior) > mxb5(mstr,iior))mxb5(mstr,iior) = vbsby(iior)
         if (vbsby(iior) < mib5(mstr,iior))mib5(mstr,iior) = vbsby(iior)
         
         sumcs(mstr,iior) = sumcs(mstr,iior)+vcsby(iior)
         if (vcsby(iior) > mxcs(mstr,iior))mxcs(mstr,iior) = vcsby(iior)
         if (vcsby(iior) < mics(mstr,iior))mics(mstr,iior) = vcsby(iior)
         
         sumn4(mstr,iior) = sumn4(mstr,iior)+vnh4y(iior)
         if (vnh4y(iior) > mxnh4(mstr,iior))mxnh4(mstr,iior) = vnh4y(iior)
         if (vnh4y(iior) < minh4(mstr,iior))minh4(mstr,iior) = vnh4y(iior)
         
         sumo2(mstr,iior) = sumo2(mstr,iior)+vo2y(iior)
         if (vo2y(iior) > mxo2(mstr,iior))mxo2(mstr,iior) = vo2y(iior)
         if (vo2y(iior) < mio2(mstr,iior))mio2(mstr,iior) = vo2y(iior)
         sumca(mstr,iior) = sumca(mstr,iior)+chlay(iior)
         sumcak(mstr,iior) = sumcak(mstr,iior)+chlaky(iior)
         sumcag(mstr,iior) = sumcag(mstr,iior)+chlagy(iior)
         sumcab(mstr,iior) = sumcab(mstr,iior)+chlaby(iior)
         sumCChlk(mstr,iior) = sumCChlk(mstr,iior)+ CChlky(iior)
         sumCChlg(mstr,iior) = sumCChlg(mstr,iior)+ CChlgy(iior)
         sumCChlb(mstr,iior) = sumCChlb(mstr,iior)+ CChlby(iior)
         if (chlay(iior) > mxchla(mstr,iior)) mxchla(mstr,iior) = chlay(iior)
         if (chlay(iior) < michla(mstr,iior)) michla(mstr,iior) = chlay(iior)
         svkigr(mstr,iior) = svkigr(mstr,iior)+vkigry(iior)
         santbl(mstr,iior) = santbl(mstr,iior)+antbly(iior)
         
         sumaki(mstr,iior) = sumaki(mstr,iior)+akiy(iior)
         if (akiy(iior) > mxaki(mstr,iior))mxaki(mstr,iior) = akiy(iior)
         if (akiy(iior) < miaki(mstr,iior))miaki(mstr,iior) = akiy(iior)
         
         sumagr(mstr,iior) = sumagr(mstr,iior)+agry(iior)
         if (agry(iior) > mxagr(mstr,iior))mxagr(mstr,iior) = agry(iior)
         if (agry(iior) < miagr(mstr,iior))miagr(mstr,iior) = agry(iior)
         
         sumabl(mstr,iior) = sumabl(mstr,iior)+ably(iior)
         if (ably(iior) > mxabl(mstr,iior))mxabl(mstr,iior) = ably(iior)
         if (ably(iior) < miabl(mstr,iior))miabl(mstr,iior) = ably(iior)
         
         sumzo(mstr,iior) = sumzo(mstr,iior)+zooiny(iior)
         if (zooiny(iior) > mxzo(mstr,iior))mxzo(mstr,iior) = zooiny(iior)
         if (zooiny(iior) < mizo(mstr,iior))mizo(mstr,iior) = zooiny(iior)
         
         sumvph(mstr,iior) = sumvph(mstr,iior)+vphy(iior)
         if (vphy(iior) > mxvph(mstr,iior))mxvph(mstr,iior) = vphy(iior)
         if (vphy(iior) < mivph(mstr,iior))mivph(mstr,iior) = vphy(iior)
         
         sumno3(mstr,iior) = sumno3(mstr,iior)+vno3y(iior)
         if (vno3y(iior) > mxvno3(mstr,iior)) mxvno3(mstr,iior) = vno3y(iior)
         if (vno3y(iior) < mivno3(mstr,iior)) mivno3(mstr,iior) = vno3y(iior)
         
         sumno2(mstr,iior) = sumno2(mstr,iior)+vno2y(iior)
         if (vno2(iior) > mxvno2(mstr,iior))mxvno2(mstr,iior) = vno2y(iior)
         if (vno2(iior) < mivno2(mstr,iior))mivno2(mstr,iior) = vno2y(iior)
         
         sumgp(mstr,iior) = sumgp(mstr,iior)+gelpy(iior)
         if (gelpy(iior) > mxgp(mstr,iior))mxgp(mstr,iior) = gelpy(iior)
         if (gelpy(iior) < migp(mstr,iior))migp(mstr,iior) = gelpy(iior)
         
         sumsi(mstr,iior) = sumsi(mstr,iior)+siy(iior)
         if (siy(iior) > mxsi(mstr,iior))mxsi(mstr,iior) = siy(iior)
         if (siy(iior) < misi(mstr,iior))misi(mstr,iior) = siy(iior)
         
         sumcal(mstr,iior) = sumcal(mstr,iior)+cay(iior)
         if (cay(iior) > mxca(mstr,iior))mxca(mstr,iior) = cay(iior)
         if (cay(iior) < mica(mstr,iior))mica(mstr,iior) = cay(iior)
         
         summw(mstr,iior) = summw(mstr,iior)+mwy(iior)
         if (mwy(iior) > mxmw(mstr,iior))mxmw(mstr,iior) = mwy(iior)
         if (mwy(iior) < mimw(mstr,iior))mimw(mstr,iior) = mwy(iior)
         
         sumlf(mstr,iior) = sumlf(mstr,iior)+lfy(iior)
         if (lfy(iior) > mxlf(mstr,iior))mxlf(mstr,iior) = lfy(iior)
         if (lfy(iior) < milf(mstr,iior))milf(mstr,iior) = lfy(iior)
         
         sumss(mstr,iior) = sumss(mstr,iior)+ssalgy(iior)
         if (ssalgy(iior) > mxSS(mstr,iior))mxSS(mstr,iior) = ssalgy(iior)
         if (ssalgy(iior) < miSS(mstr,iior))miSS(mstr,iior) = ssalgy(iior)
         
         scoli(mstr,iior) = scoli(mstr,iior)+coliy(iior)
         if (coliy(iior) > mxcoli(mstr,iior))mxcoli(mstr,iior) = coliy(iior)
         if (coliy(iior) < micoli(mstr,iior))micoli(mstr,iior) = coliy(iior)
         
         sumdln(mstr,iior) = sumdln(mstr,iior)+dlarny(iior)
         if (dlarny(iior) > mxdlan(mstr,iior))mxdlan(mstr,iior) = dlarny(iior)
         if (dlarny(iior) < midlan(mstr,iior))midlan(mstr,iior) = dlarny(iior)
         
         sumgsZn(mstr,iior) = sumgsZn(mstr,iior)+gsZny(iior)
         if (gsZny(iior) > mxgsZn(mstr,iior))mxgsZn(mstr,iior) = gsZny(iior)
         if (gsZny(iior) < migsZn(mstr,iior))migsZn(mstr,iior) = gsZny(iior)
         
         sumglZn(mstr,iior) = sumglZn(mstr,iior)+glZny(iior)
         if (glZny(iior) > mxglZn(mstr,iior))mxglZn(mstr,iior) = glZny(iior)
         if (glZny(iior) < miglZn(mstr,iior))miglZn(mstr,iior) = glZny(iior)
         
         sumgsCad(mstr,iior) = sumgsCad(mstr,iior)+gsCady(iior)
         if (gsCady(iior) > mxgsCad(mstr,iior))mxgsCad(mstr,iior) = gsCady(iior)
         if (gsCady(iior) < migsCad(mstr,iior))migsCad(mstr,iior) = gsCady(iior)
         
         sumglCad(mstr,iior) = sumglCad(mstr,iior)+glCady(iior)
         if (glCady(iior) > mxglCad(mstr,iior))mxglCad(mstr,iior) = glCady(iior)
         if (glCady(iior) < miglCad(mstr,iior))miglCad(mstr,iior) = glCady(iior)
         
         sumgsCu(mstr,iior) = sumgsCu(mstr,iior)+gsCuy(iior)
         if (gsCuy(iior) > mxgsCu(mstr,iior))mxgsCu(mstr,iior) = gsCuy(iior)
         if (gsCuy(iior) < migsCu(mstr,iior))migsCu(mstr,iior) = gsCuy(iior)
         
         sumglCu(mstr,iior) = sumglCu(mstr,iior)+glCuy(iior)
         if (glCuy(iior) > mxglCu(mstr,iior))mxglCu(mstr,iior) = glCuy(iior)
         if (glCuy(iior) < miglCu(mstr,iior))miglCu(mstr,iior) = glCuy(iior)
         
         sumgsNi(mstr,iior) = sumgsNi(mstr,iior)+gsNiy(iior)
         if (gsNiy(iior) > mxgsNi(mstr,iior))mxgsNi(mstr,iior) = gsNiy(iior)
         if (gsNiy(iior) < migsNi(mstr,iior))migsNi(mstr,iior) = gsNiy(iior)
         
         sumglNi(mstr,iior) = sumglNi(mstr,iior)+glNiy(iior)
         if (glNiy(iior) > mxglNi(mstr,iior))mxglNi(mstr,iior) = glNiy(iior)
         if (glNiy(iior) < miglNi(mstr,iior))miglNi(mstr,iior) = glNiy(iior)
         
         sumgsAs(mstr,iior) = sumgsAs(mstr,iior)+gsAsy(iior)
         if (gsAsy(iior) > mxgsAs(mstr,iior))mxgsAs(mstr,iior) = gsAsy(iior)
         if (gsAsy(iior) < migsAs(mstr,iior))migsAs(mstr,iior) = gsAsy(iior)
         
         sumglAs(mstr,iior) = sumglAs(mstr,iior)+glAsy(iior)
         if (glAsy(iior) > mxglAs(mstr,iior))mxglAs(mstr,iior) = glAsy(iior)
         if (glAsy(iior) < miglAs(mstr,iior))miglAs(mstr,iior) = glAsy(iior)
         
         sumgsPb(mstr,iior) = sumgsPb(mstr,iior)+gsPby(iior)
         if (gsPby(iior) > mxgsPb(mstr,iior))mxgsPb(mstr,iior) = gsPby(iior)
         if (gsPby(iior) < migsPb(mstr,iior))migsPb(mstr,iior) = gsPby(iior)
         
         sumglPb(mstr,iior) = sumglPb(mstr,iior)+glPby(iior)
         if (glPby(iior) > mxglPb(mstr,iior))mxglPb(mstr,iior) = glPby(iior)
         if (glPby(iior) < miglPb(mstr,iior))miglPb(mstr,iior) = glPby(iior)
         
         sumgsCr(mstr,iior) = sumgsCr(mstr,iior)+gsCry(iior)
         if (gsCry(iior) > mxgsCr(mstr,iior))mxgsCr(mstr,iior) = gsCry(iior)
         if (gsCry(iior) < migsCr(mstr,iior))migsCr(mstr,iior) = gsCry(iior)
         
         sumglCr(mstr,iior) = sumglCr(mstr,iior)+glCry(iior)
         if (glCry(iior) > mxglCr(mstr,iior))mxglCr(mstr,iior) = glCry(iior)
         if (glCry(iior) < miglCr(mstr,iior))miglCr(mstr,iior) = glCry(iior)
         
         sumgsFe(mstr,iior) = sumgsFe(mstr,iior)+gsFey(iior)
         if (gsFey(iior) > mxgsFe(mstr,iior))mxgsFe(mstr,iior) = gsFey(iior)
         if (gsFey(iior) < migsFe(mstr,iior))migsFe(mstr,iior) = gsFey(iior)
         
         sumglFe(mstr,iior) = sumglFe(mstr,iior)+glFey(iior)
         if (glFey(iior) > mxglFe(mstr,iior))mxglFe(mstr,iior) = glFey(iior)
         if (glFey(iior) < miglFe(mstr,iior))miglFe(mstr,iior) = glFey(iior)
         
         sumgsHg(mstr,iior) = sumgsHg(mstr,iior)+gsHgy(iior)
         if (gsHgy(iior) > mxgsHg(mstr,iior))mxgsHg(mstr,iior) = gsHgy(iior)
         if (gsHgy(iior) < migsHg(mstr,iior))migsHg(mstr,iior) = gsHgy(iior)
         
         sumglHg(mstr,iior) = sumglHg(mstr,iior)+glHgy(iior)
         if (glHgy(iior) > mxglHg(mstr,iior))mxglHg(mstr,iior) = glHgy(iior)
         if (glHgy(iior) < miglHg(mstr,iior))miglHg(mstr,iior) = glHgy(iior)
         
         sumgsMn(mstr,iior) = sumgsMn(mstr,iior)+gsMny(iior)
         if (gsMny(iior) > mxgsMn(mstr,iior))mxgsMn(mstr,iior) = gsMny(iior)
         if (gsMny(iior) < migsMn(mstr,iior))migsMn(mstr,iior) = gsMny(iior)
         
         sumglMn(mstr,iior) = sumglMn(mstr,iior)+glMny(iior)
         if (glMny(iior) > mxglMn(mstr,iior))mxglMn(mstr,iior) = glMny(iior)
         if (glMny(iior) < miglMn(mstr,iior))miglMn(mstr,iior) = glMny(iior)
         
         sumgsU(mstr,iior) = sumgsU(mstr,iior)+gsUy(iior)
         if (gsUy(iior) > mxgsU(mstr,iior))mxgsU(mstr,iior) = gsUy(iior)
         if (gsUy(iior) < migsU(mstr,iior))migsU(mstr,iior) = gsUy(iior)
         
         sumglU(mstr,iior) = sumglU(mstr,iior)+glUy(iior)
         if (glUy(iior) > mxglU(mstr,iior))mxglU(mstr,iior) = glUy(iior)
         if (glUy(iior) < miglU(mstr,iior))miglU(mstr,iior) = glUy(iior)
         
         ! Dreissena
         do ndr = 1,nndr
            sidras(mstr,iior,ndr) = sidras(mstr,iior,ndr) + idrasy(iior,ndr)
            sdrmas(mstr,iior,ndr) = sdrmas(mstr,iior,ndr) + drmasy(iior,ndr)
            sdrakr(mstr,iior,ndr) = sdrakr(mstr,iior,ndr) + drakry(iior,ndr)
            sdrbar(mstr,iior,ndr) = sdrbar(mstr,iior,ndr) + drbary(iior,ndr)
            sdrmor(mstr,iior,ndr) = sdrmor(mstr,iior,ndr) + drmory(iior,ndr)
            szdrg(mstr,iior,ndr)  = szdrg(mstr,iior,ndr)  + dreiy(iior,ndr)
            szdrsg(mstr,iior,ndr) = szdrsg(mstr,iior,ndr) + dreisy(iior,ndr)
           sgwdrg(mstr,iior,ndr)  = sgwdrg(mstr,iior,ndr) + gwdrly(iior,ndr)
         enddo
         
         scorIg(mstr,iior)  = scorIg(mstr,iior)  + coroy(iior)
         scoIsg(mstr,iior)  = scoIsg(mstr,iior)  + corosy(iior)
         
         sumpfl(mstr,iior)  = sumpfl(mstr,iior)  + pfly(iior)
         sumbal(mstr,iior)  = sumbal(mstr,iior)  + alby(iior)
         
         snaehr(mstr,iior)  = snaehr(mstr,iior)  + tpkiy(iior)
         
         ssusn(mstr,iior)   = ssusn(mstr,iior)   + susny(iior)
         sbettn(mstr,iior)  = sbettn(mstr,iior)  + bettny(iior)
         sdon(mstr,iior)    = sdon(mstr,iior)    + dony(iior)
         salgn(mstr,iior)   = salgn(mstr,iior)   + agrn4y(iior) + akin4y(iior) + abln4y(iior)
         salNO3(mstr,iior)  = salNO3(mstr,iior)  + alNO3y(iior)
         sFluN3(mstr,iior)  = sFluN3(mstr,iior)  + FluN3y(iior)
         sJNO3(mstr,iior)   = sJNO3(mstr,iior)   + JNO3y(iior)
         sJNH4(mstr,iior)   = sJNH4(mstr,iior)   + JNH4y(iior)
         sJPO4(mstr,iior)   = sJPO4(mstr,iior)   + JPO4y(iior)
         sJO2(mstr,iior)    = sJO2(mstr,iior)    + JO2y(iior)
         sJSi(mstr,iior)    = sJSi(mstr,iior)    + JSiy(iior)
         svx0(mstr,iior)    = svx0(mstr,iior)    + vx0y(iior)
         svx02(mstr,iior)   = svx02(mstr,iior)   + vx02y(iior)
         ssedx0(mstr,iior)  = ssedx0(mstr,iior)  + sedx0y(iior)
         ssusno(mstr,iior)  = ssusno(mstr,iior)  + susnoy(iior)
         ssedal(mstr,iior)  = ssedal(mstr,iior)  + sedagy(iior) + sedaky(iior) + sedaby(iior)
         salgzo(mstr,iior)  = salgzo(mstr,iior)  + algzgy(iior) + algzky(iior) + algzby(iior)
         salgdr(mstr,iior)  = salgdr(mstr,iior)  + algdgy(iior) + algdky(iior) + algdby(iior)
         salgco(mstr,iior)  = salgco(mstr,iior)  + algcgy(iior) + algcky(iior) + algcby(iior)
         svoldr(mstr,iior)  = svoldr(mstr,iior)  + volfdy(iior)
         sdrpfe(mstr,iior)  = sdrpfe(mstr,iior)  + drpfey(iior)
         sabeow(mstr,iior)  = sabeow(mstr,iior)  + abowgy(iior) + abowky(iior)
         sabeor(mstr,iior)  = sabeor(mstr,iior)  + aborgy(iior) + aborky(iior)
         sdalg(mstr,iior)   = sdalg(mstr,iior)   + dalggy(iior) + dalgky(iior) + dalgby(iior)
         sdalga(mstr,iior)  = sdalga(mstr,iior)  + dalagy(iior) + dalaky(iior) + dalaby(iior)
         salmor(mstr,iior)  = salmor(mstr,iior)  + dgmory(iior) + dkmory(iior) + dbmory(iior)
         sblmor(mstr,iior)  = sblmor(mstr,iior)  + dbmory(iior)
         ssgo2n(mstr,iior)  = ssgo2n(mstr,iior)  + sgo2ny(iior)
         ssdbsb(mstr,iior)  = ssdbsb(mstr,iior)  + sdbsby(iior)
         sbsbt(mstr,iior)   = sbsbt(mstr,iior)   + bsbty(iior)
         s2algo(mstr,iior)  = s2algo(mstr,iior)  + dalgoy(iior)
         s2algao(mstr,iior) = s2algao(mstr,iior) + dalaoy(iior)
         sschlr(mstr,iior)  = sschlr(mstr,iior)  + schlry(iior)
         sbsbbe(mstr,iior)  = sbsbbe(mstr,iior)  + bsbbey(iior)
         so2phy(mstr,iior)  = so2phy(mstr,iior)  + o2ei1y(iior)
         sro2dr(mstr,iior)  = sro2dr(mstr,iior)  + ro2dry(iior)
         szooro(mstr,iior)  = szooro(mstr,iior)  + zoro2y(iior)
         spo2p(mstr,iior)   = spo2p(mstr,iior)   + po2py(iior)
         spo2r(mstr,iior)   = spo2r(mstr,iior)   + po2ry(iior)
         sir(mstr,iior)     = sir(mstr,iior)     + iry(iior)
         srmue(mstr,iior)   = srmue(mstr,iior)   + rmuasy(iior)
         srakr(mstr,iior)   = srakr(mstr,iior)   + rakry(iior)
         srbar(mstr,iior)   = srbar(mstr,iior)   + rbary(iior)
         sffood(mstr,iior)  = sffood(mstr,iior)  + ffoody(iior)
         
         ! average light limitation factor of all phytoplankton groups
         if (chlaky(iior) + chlagy(iior) + chlaby(iior) > 0.) then
            sfik(mstr,iior)   = sfik(mstr,iior) +                                                                       &
                                (fiy(iior)  * chlaky(iior) + figy(iior) * chlagy(iior) + fiby(iior) * chlaby(iior)) /   &
                                (chlaky(iior) + chlagy(iior) + chlaby(iior))
         endif
         
         !sfig wird mit dem Extinktionskoeffizient belegt
         sfig(mstr,iior) = sfig(mstr,iior)+extky(iior)
         ! sfib wird nicht mehr erzeugt
         sfib(mstr,iior)   = -1.
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
         sHNFz(mstr,iior)  = sHNFz(mstr,iior)+HNFzy(iior)
         sBACmu(mstr,iior) = sBACmu(mstr,iior)+BACmuy(iior)
         sHNFBA(mstr,iior) = sHNFBA(mstr,iior)+HNFBAy(iior)
         snl0(mstr,iior)   = snl0(mstr,iior)+nl0y(iior)
         spl0(mstr,iior)   = spl0(mstr,iior)+pl0y(iior)
         
         ! Buhnenfelder
         if (nbuhn(mstr) == 1) then
            bste(mstr,iior) = bste(mstr,iior)+btempy(iior)
            if (btempy(iior) > bmxtem(mstr,iior)) bmxtem(mstr,iior) = btempy(iior)
            if (btempy(iior) < bmitem(mstr,iior)) bmitem(mstr,iior) = btempy(iior)
            
            bsno3(mstr,iior) = bsno3(mstr,iior)+bno3y(iior)
            if (bno3y(iior) > bmxno3(mstr,iior))  bmxno3(mstr,iior) = bno3y(iior)
            if (bno3y(iior) < bmino3(mstr,iior))  bmino3(mstr,iior) = bno3y(iior)
            
            bsno2(mstr,iior) = bsno2(mstr,iior)+bno2y(iior)
            if (bno2y(iior) > bmxno2(mstr,iior))  bmxno2(mstr,iior) = bno2y(iior)
            if (bno2y(iior) < bmino2(mstr,iior))  bmino2(mstr,iior) = bno2y(iior)
            
            bsn4(mstr,iior) = bsn4(mstr,iior)+bnh4y(iior)
            if (bnh4y(iior) > bmxnh4(mstr,iior)) bmxnh4(mstr,iior) = bnh4y(iior)
            if (bnh4y(iior) < bminh4(mstr,iior)) bminh4(mstr,iior) = bnh4y(iior)
            
            bsgelp(mstr,iior) = bsgelp(mstr,iior)+bgelpy(iior)
            if (bgelpy(iior) > bmxglp(mstr,iior)) bmxglp(mstr,iior) = bgelpy(iior)
            if (bgelpy(iior) < bmiglp(mstr,iior)) bmiglp(mstr,iior) = bgelpy(iior)
            
            bschla(mstr,iior) = bschla(mstr,iior)+bchlay(iior)
            if (bchlay(iior) > bmxchl(mstr,iior)) bmxchl(mstr,iior) = bchlay(iior)
            if (bchlay(iior) < bmichl(mstr,iior)) bmichl(mstr,iior) = bchlay(iior)
            
            bsaki(mstr,iior) = bsaki(mstr,iior)+bakiy(iior)
            if (bakiy(iior) > bmxaki(mstr,iior))  bmxaki(mstr,iior) = bakiy(iior)
            if (bakiy(iior) < bmiaki(mstr,iior))  bmiaki(mstr,iior) = bakiy(iior)
            
            bsagr(mstr,iior) = bsagr(mstr,iior)+bagry(iior)
            if (bagry(iior) > bmxagr(mstr,iior))  bmxagr(mstr,iior) = bagry(iior)
            if (bagry(iior) < bmiagr(mstr,iior))  bmiagr(mstr,iior) = bagry(iior)
            
            bsabl(mstr,iior) = bsabl(mstr,iior)+bably(iior)
            bsssal(mstr,iior) = bsssal(mstr,iior)+bssaly(iior)
            if (bssaly(iior) > bmxssa(mstr,iior)) bmxssa(mstr,iior) = bssaly(iior)
            if (bssaly(iior) < bmissa(mstr,iior)) bmissa(mstr,iior) = bssaly(iior)
            
            bssi(mstr,iior) = bssi(mstr,iior)+bsiy(iior)
            if (bsiy(iior) > bmxsi(mstr,iior))bmxsi(mstr,iior) = bsiy(iior)
            if (bsiy(iior) < bmisi(mstr,iior))bmisi(mstr,iior) = bsiy(iior)
            
            bszooi(mstr,iior) = bszooi(mstr,iior)+bzooiy(iior)
            if (bzooiy(iior) > bmxzoo(mstr,iior)) bmxzoo(mstr,iior) = bzooiy(iior)
            if (bzooiy(iior) < bmizoo(mstr,iior)) bmizoo(mstr,iior) = bzooiy(iior)
            
            bsvbsb(mstr,iior) = bsvbsb(mstr,iior)+bvbsby(iior)
            if (bvbsby(iior) > bmxbsb(mstr,iior)) bmxbsb(mstr,iior) = bvbsby(iior)
            if (bvbsby(iior) < bmibsb(mstr,iior)) bmibsb(mstr,iior) = bvbsby(iior)
            
            bsvcsb(mstr,iior) = bsvcsb(mstr,iior)+bvcsby(iior)
            if (bvcsby(iior) > bmxcsb(mstr,iior)) bmxcsb(mstr,iior) = bvcsby(iior)
            if (bvcsby(iior) < bmicsb(mstr,iior)) bmicsb(mstr,iior) = bvcsby(iior)
            
            bsgsP(mstr,iior) = bsgsP(mstr,iior)+bgsPy(iior)
            if (bgsPy(iior) > bmxgsP(mstr,iior)) bmxgsP(mstr,iior) = bgsPy(iior)
            if (bgsPy(iior) < bmigsP(mstr,iior)) bmigsP(mstr,iior) = bgsPy(iior)
            
            bsgsN(mstr,iior) = bsgsN(mstr,iior)+bgsNy(iior)
            if (bgsNy(iior) > bmxgsN(mstr,iior)) bmxgsN(mstr,iior) = bgsNy(iior)
            if (bgsNy(iior) < bmigsN(mstr,iior)) bmigsN(mstr,iior) = bgsNy(iior)
            
            bso2(mstr,iior) = bso2(mstr,iior)+bo2y(iior)
            if (bo2y(iior) > bmxo2(mstr,iior))bmxo2(mstr,iior) = bo2y(iior)
            if (bo2y(iior) < bmio2(mstr,iior))bmio2(mstr,iior) = bo2y(iior)
            
            bsmw(mstr,iior) = bsmw(mstr,iior)+bmwy(iior)
            if (bmwy(iior) > bmxmw(mstr,iior))bmxmw(mstr,iior) = bmwy(iior)
            if (bmwy(iior) < bmimw(mstr,iior))bmimw(mstr,iior) = bmwy(iior)
            
            bslf(mstr,iior) = bslf(mstr,iior)+blfy(iior)
            if (blfy(iior) > bmxlf(mstr,iior))bmxlf(mstr,iior) = blfy(iior)
            if (blfy(iior) < bmilf(mstr,iior))bmilf(mstr,iior) = blfy(iior)
            
            bsca(mstr,iior) = bsca(mstr,iior)+bcay(iior)
            if (bcay(iior) > bmxca(mstr,iior))bmxca(mstr,iior) = bcay(iior)
            if (bcay(iior) < bmica(mstr,iior))bmica(mstr,iior) = bcay(iior)
            
            bsph(mstr,iior) = bsph(mstr,iior)+bphy(iior)
            if (bphy(iior) > bmxph(mstr,iior))bmxph(mstr,iior) = bphy(iior)
            if (bphy(iior) < bmiph(mstr,iior))bmiph(mstr,iior) = bphy(iior)
            
            bsnl0(mstr,iior) = bsnl0(mstr,iior)+bnl0y(iior)
            bspl0(mstr,iior) = bspl0(mstr,iior)+bpl0y(iior)
            bscoli(mstr,iior) = bscoli(mstr,iior) + bcoliy(iior)
            
            bsgsZn(mstr,iior) = bsgsZn(mstr,iior) + bgsZny(iior)
            if (bgsZny(iior) > bmxgsZn(mstr,iior))bmxgsZn(mstr,iior) = bgsZny(iior)
            if (bgsZny(iior) < bmigsZn(mstr,iior))bmigsZn(mstr,iior) = bgsZny(iior)
            
            bsglZn(mstr,iior) = bsglZn(mstr,iior) + bglZny(iior)
            if (bglZny(iior) > bmxglZn(mstr,iior))bmxglZn(mstr,iior) = bglZny(iior)
            if (bglZny(iior) < bmiglZn(mstr,iior))bmiglZn(mstr,iior) = bglZny(iior)
            
            bsgsCad(mstr,iior) = bsgsCad(mstr,iior) + bgsCady(iior)
            if (bgsCady(iior) > bmxgsCad(mstr,iior))bmxgsCad(mstr,iior) = bgsCady(iior)
            if (bgsCady(iior) < bmigsCad(mstr,iior))bmigsCad(mstr,iior) = bgsCady(iior)
            
            bsglCad(mstr,iior) = bsglCad(mstr,iior) + bglCady(iior)
            if (bglCady(iior) > bmxglCad(mstr,iior))bmxglCad(mstr,iior) = bglCady(iior)
            if (bglCady(iior) < bmiglCad(mstr,iior))bmiglCad(mstr,iior) = bglCady(iior)
            
            bsgsCu(mstr,iior) = bsgsCu(mstr,iior) + bgsCuy(iior)
            if (bgsCuy(iior) > bmxgsCu(mstr,iior))bmxgsCu(mstr,iior) = bgsCuy(iior)
            if (bgsCuy(iior) < bmigsCu(mstr,iior))bmigsCu(mstr,iior) = bgsCuy(iior)
            
            bsglCu(mstr,iior) = bsglCu(mstr,iior) + bglCuy(iior)
            if (bglCuy(iior) > bmxglCu(mstr,iior))bmxglCu(mstr,iior) = bglCuy(iior)
            if (bglCuy(iior) < bmiglCu(mstr,iior))bmiglCu(mstr,iior) = bglCuy(iior)
            
            bsgsNi(mstr,iior) = bsgsNi(mstr,iior) + bgsNiy(iior)
            if (bgsNiy(iior) > bmxgsNi(mstr,iior))bmxgsNi(mstr,iior) = bgsNiy(iior)
            if (bgsNiy(iior) < bmigsNi(mstr,iior))bmigsNi(mstr,iior) = bgsNiy(iior)
            
            bsglNi(mstr,iior) = bsglNi(mstr,iior) + bglNiy(iior)
            if (bglNiy(iior) > bmxglNi(mstr,iior))bmxglNi(mstr,iior) = bglNiy(iior)
            if (bglNiy(iior) < bmiglNi(mstr,iior))bmiglNi(mstr,iior) = bglNiy(iior)
            
            bsgsAs(mstr,iior) = bsgsAs(mstr,iior) + bgsAsy(iior)
            if (bgsAsy(iior) > bmxgsAs(mstr,iior))bmxgsAs(mstr,iior) = bgsAsy(iior)
            if (bgsAsy(iior) < bmigsAs(mstr,iior))bmigsAs(mstr,iior) = bgsAsy(iior)
            
            bsglAs(mstr,iior) = bsglAs(mstr,iior) + bglAsy(iior)
            if (bglAsy(iior) > bmxglAs(mstr,iior))bmxglAs(mstr,iior) = bglAsy(iior)
            if (bglAsy(iior) < bmiglAs(mstr,iior))bmiglAs(mstr,iior) = bglAsy(iior)
            
            bsgsPb(mstr,iior) = bsgsPb(mstr,iior) + bgsPby(iior)
            if (bgsPby(iior) > bmxgsPb(mstr,iior))bmxgsPb(mstr,iior) = bgsPby(iior)
            if (bgsPby(iior) < bmigsPb(mstr,iior))bmigsPb(mstr,iior) = bgsPby(iior)
            
            bsglPb(mstr,iior) = bsglPb(mstr,iior) + bglPby(iior)
            if (bglPby(iior) > bmxglPb(mstr,iior))bmxglPb(mstr,iior) = bglPby(iior)
            if (bglPby(iior) < bmiglPb(mstr,iior))bmiglPb(mstr,iior) = bglPby(iior)
            
            bsgsCr(mstr,iior) = bsgsCr(mstr,iior) + bgsCry(iior)
            if (bgsCry(iior) > bmxgsCr(mstr,iior))bmxgsCr(mstr,iior) = bgsCry(iior)
            if (bgsCry(iior) < bmigsCr(mstr,iior))bmigsCr(mstr,iior) = bgsCry(iior)
            
            bsglCr(mstr,iior) = bsglCr(mstr,iior) + bglCry(iior)
            if (bglCry(iior) > bmxglCr(mstr,iior))bmxglCr(mstr,iior) = bglCry(iior)
            if (bglCry(iior) < bmiglCr(mstr,iior))bmiglCr(mstr,iior) = bglCry(iior)
            
            bsgsFe(mstr,iior) = bsgsFe(mstr,iior) + bgsFey(iior)
            if (bgsFey(iior) > bmxgsFe(mstr,iior))bmxgsFe(mstr,iior) = bgsFey(iior)
            if (bgsFey(iior) < bmigsFe(mstr,iior))bmigsFe(mstr,iior) = bgsFey(iior)
            
            bsglFe(mstr,iior) = bsglFe(mstr,iior) + bglFey(iior)
            if (bglFey(iior) > bmxglFe(mstr,iior))bmxglFe(mstr,iior) = bglFey(iior)
            if (bglFey(iior) < bmiglFe(mstr,iior))bmiglFe(mstr,iior) = bglFey(iior)
            
            bsgsHg(mstr,iior) = bsgsHg(mstr,iior) + bgsHgy(iior)
            if (bgsHgy(iior) > bmxgsHg(mstr,iior))bmxgsHg(mstr,iior) = bgsHgy(iior)
            if (bgsHgy(iior) < bmigsHg(mstr,iior))bmigsHg(mstr,iior) = bgsHgy(iior)
            
            bsglHg(mstr,iior) = bsglHg(mstr,iior) + bglHgy(iior)
            if (bglHgy(iior) > bmxglHg(mstr,iior))bmxglHg(mstr,iior) = bglHgy(iior)
            if (bglHgy(iior) < bmiglHg(mstr,iior))bmiglHg(mstr,iior) = bglHgy(iior)
            
            bsgsMn(mstr,iior) = bsgsMn(mstr,iior) + bgsMny(iior)
            if (bgsMny(iior) > bmxgsMn(mstr,iior))bmxgsMn(mstr,iior) = bgsMny(iior)
            if (bgsMny(iior) < bmigsMn(mstr,iior))bmigsMn(mstr,iior) = bgsMny(iior)
            
            bsglMn(mstr,iior) = bsglMn(mstr,iior) + bglMny(iior)
            if (bglMny(iior) > bmxglMn(mstr,iior))bmxglMn(mstr,iior) = bglMny(iior)
            if (bglMny(iior) < bmiglMn(mstr,iior))bmiglMn(mstr,iior) = bglMny(iior)
            
            bsgsU(mstr,iior) = bsgsU(mstr,iior) + bgsUy(iior)
            if (bgsUy(iior) > bmxgsU(mstr,iior))bmxgsU(mstr,iior) = bgsUy(iior)
            if (bgsUy(iior) < bmigsU(mstr,iior))bmigsU(mstr,iior) = bgsUy(iior)
            
            bsglU(mstr,iior) = bsglU(mstr,iior) + bglUy(iior)
            if (bglUy(iior) > bmxglU(mstr,iior))bmxglU(mstr,iior) = bglUy(iior)
            if (bglUy(iior) < bmiglU(mstr,iior))bmiglU(mstr,iior) = bglUy(iior)
            
            ! Raten
            bnaehr(mstr,iior) = bnaehr(mstr,iior)+ btpkiy(iior)*bkigry(iior)  &
                                +btpgry(iior)*(1.-bkigry(iior)-bantby(iior))  &
                                +btpbly(iior)*bantby(iior)
            !
            bsdalg(mstr,iior) = bsdalg(mstr,iior)+bdakiy(iior)+bdagry(iior)   &
                                +bdably(iior)
            bsvkg(mstr,iior) = bsvkg(mstr,iior)+bkigry(iior)
            bsantb(mstr,iior) = bsantb(mstr,iior)+bantby(iior)
            bsdaa(mstr,iior) = bsdaa(mstr,iior)+bdaaky(iior)+bdaagy(iior)     &
                               +bdaaby(iior)
            bsseda(mstr,iior) = bsseda(mstr,iior)+bsedky(iior)+bsedgy(iior)   &
                                +bsedby(iior)
            bsalgz(mstr,iior) = bsalgz(mstr,iior)+bazoky(iior)+bazogy(iior)   &
                                +bazoby(iior)
            bsamor(mstr,iior) = bsamor(mstr,iior)+bkmory(iior)+bgmory(iior)   &
                                +bbmory(iior)
            bsadr(mstr,iior) = bsadr(mstr,iior)+badrky(iior)+badrgy(iior)     &
                               +badrby(iior)
            ! Pseudofaces = 0.0 da keine Dreissena in Buhnenfeld
            bsalco(mstr,iior) = bsalco(mstr,iior)     &
                              + bacoky(iior)          &
                              + bacogy(iior)          &
                              + bacoby(iior)
            ! mean light limitation factor of algae in groin field
            if (bchlky(iior) + bchlgy(iior) + bchlby(iior) > 0.) then
               bsfik(mstr,iior) = bsfik(mstr,iior) +                                                                             &
                                  (bfikay(iior)  * bchlky(iior) + bfigay(iior) * bchlgy(iior) + bfibay(iior) * bchlby(iior)) /   &
                                  (bchlky(iior) + bchlgy(iior) + bchlby(iior))
            endif
            
            ! bsfig: Summenbildung des extiktionskoeffizients
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
         endif
         
         ! --------------------------------------------------------------------
         ! Ymax/Ymin-Bildung für grafische Ausgabe
         ! --------------------------------------------------------------------
         if (vbsby(iior)  > Ymax(mstr,1))   Ymax(mstr,1) = vbsby(iior)
         if (vbsby(iior)  < Ymin(mstr,1))   Ymin(mstr,1) = vbsby(iior)
         if (vcsby(iior)  > Ymax(mstr,2))   Ymax(mstr,2) = vcsby(iior)
         if (vcsby(iior)  < Ymin(mstr,2))   Ymin(mstr,2) = vcsby(iior)
         if (vnh4y(iior)  > Ymax(mstr,3))   Ymax(mstr,3) = vnh4y(iior)
         if (vnh4y(iior)  < Ymin(mstr,3))   Ymin(mstr,3) = vnh4y(iior)
         if (vno2y(iior)  > Ymax(mstr,4))   Ymax(mstr,4) = vno2y(iior)
         if (vno2y(iior)  < Ymin(mstr,4))   Ymin(mstr,4) = vno2y(iior)
         if (vno3y(iior)  > Ymax(mstr,5))   Ymax(mstr,5) = vno3y(iior)
         if (vno3y(iior)  < Ymin(mstr,5))   Ymin(mstr,5) = vno3y(iior)
         if (gsNy(iior)   > Ymax(mstr,6))   Ymax(mstr,6) = gsNy(iior)
         if (gsNy(iior)   < Ymin(mstr,6))   Ymin(mstr,6) = gsNy(iior)
         if (gelpy(iior)  > Ymax(mstr,7))   Ymax(mstr,7) = gelpy(iior)
         if (gelpy(iior)  < Ymin(mstr,7))   Ymin(mstr,7) = gelpy(iior)
         if (gsPy(iior)   > Ymax(mstr,8))   Ymax(mstr,8) = gsPy(iior)
         if (gsPy(iior)   < Ymin(mstr,8))   Ymin(mstr,8) = gsPy(iior)
         if (siy(iior)    > Ymax(mstr,9))   Ymax(mstr,9) = siy(iior)
         if (siy(iior)    < Ymin(mstr,9))   Ymin(mstr,9) = siy(iior)
         if (chlay(iior)  > Ymax(mstr,10))  Ymax(mstr,10) = chlay(iior)
         if (chlay(iior)  < Ymin(mstr,10))  Ymin(mstr,10) = chlay(iior)
         if (zooiny(iior) > Ymax(mstr,11))  Ymax(mstr,11) = zooiny(iior)
         if (zooiny(iior) < Ymin(mstr,11))  Ymin(mstr,11) = zooiny(iior)
         if (vphy(iior)   > Ymax(mstr,12))  Ymax(mstr,12) = vphy(iior)
         if (vphy(iior)   < Ymin(mstr,12))  Ymin(mstr,12) = vphy(iior)
         if (mwy(iior)    > Ymax(mstr,13))  Ymax(mstr,13) = mwy(iior)
         if (mwy(iior)    < Ymin(mstr,13))  Ymin(mstr,13) = mwy(iior)
         if (cay(iior)    > Ymax(mstr,14))  Ymax(mstr,14) = cay(iior)
         if (cay(iior)    < Ymin(mstr,14))  Ymin(mstr,14) = cay(iior)
         if (lfy(iior)    > Ymax(mstr,15))  Ymax(mstr,15) = lfy(iior)
         if (lfy(iior)    < Ymin(mstr,15))  Ymin(mstr,15) = lfy(iior)
         if (ssalgy(iior) > Ymax(mstr,16))  Ymax(mstr,16) = ssalgy(iior)
         if (ssalgy(iior) < Ymin(mstr,16))  Ymin(mstr,16) = ssalgy(iior)
         if (tempwy(iior) > Ymax(mstr,17))  Ymax(mstr,17) = tempwy(iior)
         if (tempwy(iior) < Ymin(mstr,17))  Ymin(mstr,17) = tempwy(iior)
         if (vo2y(iior)   > Ymax(mstr,18))  Ymax(mstr,18) = vo2y(iior)
         if (vo2y(iior)   < Ymin(mstr,18))  Ymin(mstr,18) = vo2y(iior)
         if (coliy(iior)  > Ymax(mstr,19))  Ymax(mstr,19) = coliy(iior)
         if (coliy(iior)  < Ymin(mstr,19))  Ymin(mstr,19) = coliy(iior)
         if (gsZny(iior)  > Ymax(mstr,193)) Ymax(mstr,193) = gsZny(iior)
         if (gsZny(iior)  < Ymin(mstr,193)) Ymin(mstr,193) = gsZny(iior)
         if (glZny(iior)  > Ymax(mstr,194)) Ymax(mstr,194) = glZny(iior)
         if (glZny(iior)  < Ymin(mstr,194)) Ymin(mstr,194) = glZny(iior)
         if (gsCady(iior) > Ymax(mstr,195)) Ymax(mstr,195) = gsCady(iior)
         if (gsCady(iior) < Ymin(mstr,195)) Ymin(mstr,195) = gsCady(iior)
         if (glCady(iior) > Ymax(mstr,196)) Ymax(mstr,196) = glCady(iior)
         if (glCady(iior) < Ymin(mstr,196)) Ymin(mstr,196) = glCady(iior)
         if (gsCuy(iior)  > Ymax(mstr,197)) Ymax(mstr,197) = gsCuy(iior)
         if (gsCuy(iior)  < Ymin(mstr,197)) Ymin(mstr,197) = gsCuy(iior)
         if (glCuy(iior)  > Ymax(mstr,198)) Ymax(mstr,198) = glCuy(iior)
         if (glCuy(iior)  < Ymin(mstr,198)) Ymin(mstr,198) = glCuy(iior)
         if (gsNiy(iior)  > Ymax(mstr,199)) Ymax(mstr,199) = gsNiy(iior)
         if (gsNiy(iior)  < Ymin(mstr,199)) Ymin(mstr,199) = gsNiy(iior)
         if (glNiy(iior)  > Ymax(mstr,200)) Ymax(mstr,200) = glNiy(iior)
         if (glNiy(iior)  < Ymin(mstr,200)) Ymin(mstr,200) = glNiy(iior)
         if (gsAsy(iior)  > Ymax(mstr,201)) Ymax(mstr,201) = gsAsy(iior)
         if (gsAsy(iior)  < Ymin(mstr,201)) Ymin(mstr,201) = gsAsy(iior)
         if (glAsy(iior)  > Ymax(mstr,202)) Ymax(mstr,202) = glAsy(iior)
         if (glAsy(iior)  < Ymin(mstr,202)) Ymin(mstr,202) = glAsy(iior)
         if (gsPby(iior)  > Ymax(mstr,203)) Ymax(mstr,203) = gsPby(iior)
         if (gsPby(iior)  < Ymin(mstr,203)) Ymin(mstr,203) = gsPby(iior)
         if (glPby(iior)  > Ymax(mstr,204)) Ymax(mstr,204) = glPby(iior)
         if (glPby(iior)  < Ymin(mstr,204)) Ymin(mstr,204) = glPby(iior)
         if (gsCry(iior)  > Ymax(mstr,205)) Ymax(mstr,205) = gsCry(iior)
         if (gsCry(iior)  < Ymin(mstr,205)) Ymin(mstr,205) = gsCry(iior)
         if (glCry(iior)  > Ymax(mstr,206)) Ymax(mstr,206) = glCry(iior)
         if (glCry(iior)  < Ymin(mstr,206)) Ymin(mstr,206) = glCry(iior)
         if (gsFey(iior)  > Ymax(mstr,207)) Ymax(mstr,207) = gsFey(iior)
         if (gsFey(iior)  < Ymin(mstr,207)) Ymin(mstr,207) = gsFey(iior)
         if (glFey(iior)  > Ymax(mstr,208)) Ymax(mstr,208) = glFey(iior)
         if (glFey(iior)  < Ymin(mstr,208)) Ymin(mstr,208) = glFey(iior)
         if (gsHgy(iior)  > Ymax(mstr,209)) Ymax(mstr,209) = gsHgy(iior)
         if (gsHgy(iior)  < Ymin(mstr,209)) Ymin(mstr,209) = gsHgy(iior)
         if (glHgy(iior)  > Ymax(mstr,210)) Ymax(mstr,210) = glHgy(iior)
         if (glHgy(iior)  < Ymin(mstr,210)) Ymin(mstr,210) = glHgy(iior)
         if (gsMny(iior)  > Ymax(mstr,211)) Ymax(mstr,211) = gsMny(iior)
         if (gsMny(iior)  < Ymin(mstr,211)) Ymin(mstr,211) = gsMny(iior)
         if (glMny(iior)  > Ymax(mstr,212)) Ymax(mstr,212) = glMny(iior)
         if (glMny(iior)  < Ymin(mstr,212)) Ymin(mstr,212) = glMny(iior)
         if (gsUy(iior)   > Ymax(mstr,213)) Ymax(mstr,213) = gsUy(iior)
         if (gsUy(iior)   < Ymin(mstr,213)) Ymin(mstr,213) = gsUy(iior)
         if (glUy(iior)   > Ymax(mstr,214)) Ymax(mstr,214) = glUy(iior)
         if (glUy(iior)   < Ymin(mstr,214)) Ymin(mstr,214) = glUy(iior)
         if (CHNFy(iior) <= 0.0) then
            Ymax(mstr,20) = -1.
            Ymin(mstr,20) = -1
         else
            if (CHNFy(iior) > Ymax(mstr,20))Ymax(mstr,20) = CHNFy(iior)
            if (CHNFy(iior) < Ymin(mstr,20))Ymin(mstr,20) = CHNFy(iior)
         endif
         if (dly(iior)    > Ymax(mstr,21))  Ymax(mstr,21) = Dly(iior)
         if (dly(iior)    < Ymin(mstr,21))  Ymin(mstr,21) = Dly(iior)
         if (dsedH(mstr,iior) > Ymax(mstr,22))Ymax(mstr,22) = dsedH(mstr,iior)
         if (dsedH(mstr,iior) < Ymin(mstr,22))Ymin(mstr,22) = dsedH(mstr,iior)
         if (hpfl(mstr,iior) > Ymax(mstr,23))Ymax(mstr,23) = hpfl(mstr,iior)
         if (hpfl(mstr,iior) < Ymin(mstr,23))Ymin(mstr,23) = hpfl(mstr,iior)
         sbal = habgml(mstr,iior)+habkml(mstr,iior)
         if (sbal > Ymax(mstr,24))Ymax(mstr,24) = sbal
         if (sbal < Ymin(mstr,24))Ymin(mstr,24) = sbal
         if (hzdrel(mstr,iior,1) > Ymax(mstr,25)) Ymax(mstr,25) = hzdrel(mstr,iior,1)
         if (hzdrsl(mstr,iior,1) > Ymax(mstr,25)) Ymax(mstr,25) = hzdrsl(mstr,iior,1)
         if (hzdrel(mstr,iior,1) < Ymin(mstr,25)) Ymin(mstr,25) = hzdrel(mstr,iior,1)
         if (hzdrsl(mstr,iior,1) < Ymin(mstr,25)) Ymin(mstr,25) = hzdrsl(mstr,iior,1)
         if (hgwdrl(mstr,iior,1) > Ymax(mstr,26)) Ymax(mstr,26) = hgwdrl(mstr,iior,1)
         if (hgwdrl(mstr,iior,1) < Ymin(mstr,26)) Ymin(mstr,26) = hgwdrl(mstr,iior,1)
         if (hzdrel(mstr,iior,2) > Ymax(mstr,27)) Ymax(mstr,27) = hzdrel(mstr,iior,2)
         if (hzdrsl(mstr,iior,2) > Ymax(mstr,27)) Ymax(mstr,27) = hzdrsl(mstr,iior,2)
         if (hzdrel(mstr,iior,2) < Ymin(mstr,27)) Ymin(mstr,27) = hzdrel(mstr,iior,2)
         if (hzdrsl(mstr,iior,2) < Ymin(mstr,27)) Ymin(mstr,27) = hzdrsl(mstr,iior,2)
         if (hgwdrl(mstr,iior,2) > Ymax(mstr,28)) Ymax(mstr,28) = hgwdrl(mstr,iior,2)
         if (hgwdrl(mstr,iior,2) < Ymin(mstr,28)) Ymin(mstr,28) = hgwdrl(mstr,iior,2)
         if (dlarny(iior)   > Ymax(mstr,29)) Ymax(mstr,29) = dlarny(iior)
         if (dlarny(iior)   < Ymin(mstr,29)) Ymin(mstr,29) = dlarny(iior)
         if (idrasy(iior,1) > Ymax(mstr,30)) Ymax(mstr,30) = idrasy(iior,1)
         if (idrasy(iior,1) < Ymin(mstr,30)) Ymin(mstr,30) = idrasy(iior,1)
         if (idrasy(iior,2) > Ymax(mstr,31)) Ymax(mstr,31) = idrasy(iior,2)
         if (idrasy(iior,2) < Ymin(mstr,31)) Ymin(mstr,31) = idrasy(iior,2)
         if (drmasy(iior,1) > Ymax(mstr,32)) Ymax(mstr,32) = drmasy(iior,1)
         if (drmasy(iior,1) < Ymin(mstr,32)) Ymin(mstr,32) = drmasy(iior,1)
         if (drmasy(iior,2) > Ymax(mstr,33)) Ymax(mstr,33) = drmasy(iior,2)
         if (drmasy(iior,2) < Ymin(mstr,33)) Ymin(mstr,33) = drmasy(iior,2)
         if (volfdy(iior)   > Ymax(mstr,34)) Ymax(mstr,34) = volfdy(iior)
         if (volfdy(iior)   < Ymin(mstr,34)) Ymin(mstr,34) = volfdy(iior)
         if (drakry(iior,1) > Ymax(mstr,35)) Ymax(mstr,35) = drakry(iior,1)
         if (drakry(iior,1) < Ymin(mstr,35)) Ymin(mstr,35) = drakry(iior,1)
         if (drakry(iior,2) > Ymax(mstr,36)) Ymax(mstr,36) = drakry(iior,2)
         if (drakry(iior,2) < Ymin(mstr,36)) Ymin(mstr,36) = drakry(iior,2)
         if (drbary(iior,1) > Ymax(mstr,37)) Ymax(mstr,37) = drbary(iior,1)
         if (drbary(iior,1) < Ymin(mstr,37)) Ymin(mstr,37) = drbary(iior,1)
         if (drbary(iior,2) > Ymax(mstr,38)) Ymax(mstr,38) = drbary(iior,2)
         if (drbary(iior,2) < Ymin(mstr,38)) Ymin(mstr,38) = drbary(iior,2)
         if (drmory(iior,1) > Ymax(mstr,39)) Ymax(mstr,39) = drmory(iior,1)
         if (drmory(iior,1) < Ymin(mstr,39)) Ymin(mstr,39) = drmory(iior,1)
         if (drmory(iior,2) > Ymax(mstr,40)) Ymax(mstr,40) = drmory(iior,2)
         if (drmory(iior,2) < Ymin(mstr,40)) Ymin(mstr,40) = drmory(iior,2)
         if (ffoody(iior)   > Ymax(mstr,41)) Ymax(mstr,41) = ffoody(iior)
         if (ffoody(iior)   < Ymin(mstr,41)) Ymin(mstr,41) = ffoody(iior)
         
         sco = hcoro2(mstr,iior,1)  &
             + hcoro2(mstr,iior,2)  &
             + hcoro2(mstr,iior,3)  &
             + hcoro2(mstr,iior,4)  &
             + hcoro2(mstr,iior,5)
         if (sco > Ymax(mstr,42)) Ymax(mstr,42) = sco
         if (sco < Ymin(mstr,42)) Ymin(mstr,42) = sco
         
         scoS = hcos2(mstr,iior,1)  &
              + hcos2(mstr,iior,2)  &
              + hcos2(mstr,iior,3)  &
              + hcos2(mstr,iior,4)  &
              + hcos2(mstr,iior,5)
         if (scoS > Ymax(mstr,43)) Ymax(mstr,43) = scoS
         if (scoS < Ymin(mstr,43)) Ymin(mstr,43) = scoS
         
         salC = akiy(iior) * Caki   &
              + ably(iior) * Cabl   &
              + agry(iior) * Cagr
         if (salC > Ymax(mstr,44)) Ymax(mstr,44) = salC
         if (salC < Ymin(mstr,44)) Ymin(mstr,44) = salC
         
         algae_biomass = akiy(iior) + agry(iior) + ably(iior)
         if (algae_biomass > 0.0) then
            salw = (dalggy(iior)+dalgky(iior)+dalgby(iior)) * 24. / algae_biomass
            Ymax(mstr,45) = max(Ymax(mstr,45), salw)
            Ymin(mstr,45) = min(Ymin(mstr,45), salw)
         endif
         
         salR = (dalagy(iior)+dalaky(iior)+dalaby(iior))*24.
         if (salR > Ymax(mstr,46)) Ymax(mstr,46) = salR
         if (salR < Ymin(mstr,46)) Ymin(mstr,46) = salR
         
         if (algae_biomass > 0.0) then
            salM = (dgmory(iior)+dkmory(iior)+dbmory(iior)) * 24. / algae_biomass
            if (salM > Ymax(mstr,47))Ymax(mstr,47) = salM
            if (salM < Ymin(mstr,47))Ymin(mstr,47) = salM
            salS = (sedagy(iior)+sedaky(iior)+sedaby(iior)) * 24. / algae_biomass
            if (salS > Ymax(mstr,48))Ymax(mstr,48) = salS
            if (salS < Ymin(mstr,48))Ymin(mstr,48) = salS
            salZ = (algzgy(iior)+algzky(iior)+algzby(iior)) * 24. / algae_biomass
            if (salZ > Ymax(mstr,49))Ymax(mstr,49) = salZ
            if (salZ < Ymin(mstr,49))Ymin(mstr,49) = salZ
            salD = (algdgy(iior)+algdky(iior)+algdby(iior)) * 24. / algae_biomass
            if (salD > Ymax(mstr,50))Ymax(mstr,50) = salD
            if (salD < Ymin(mstr,50))Ymin(mstr,50) = salD
         endif
         if (drpfey(iior) > Ymax(mstr,51))Ymax(mstr,51) = drpfey(iior)
         if (drpfey(iior) < Ymin(mstr,51))Ymin(mstr,51) = drpfey(iior)
         ztp = tpkiy(iior) * vkigry(iior)                      &
             + tpgry(iior) * (1.-vkigry(iior)-antbly(iior))    &
             + tpbly(iior) * antbly(iior)
         if (ztp > Ymax(mstr,52))Ymax(mstr,52) = ztp
         if (ztp < Ymin(mstr,52))Ymin(mstr,52) = ztp
         if (vkigry(iior) > Ymax(mstr,53))Ymax(mstr,53) = vkigry(iior)
         if (vkigry(iior) < Ymin(mstr,53))Ymin(mstr,53) = vkigry(iior)
         if (antbly(iior) > Ymax(mstr,54))Ymax(mstr,54) = antbly(iior)
         if (antbly(iior) < Ymin(mstr,54))Ymin(mstr,54) = antbly(iior)
         if (algae_biomass> 0.0) then
            salco = (algcgy(iior)+algcky(iior)+algcby(iior)) * 24. / algae_biomass
            Ymax(mstr,55) = max(Ymax(mstr,55), salco)
            Ymin(mstr,55) = min(Ymin(mstr,55), salco)
         endif
         if (fiy(iior) > Ymax(mstr,56))Ymax(mstr,56) = fiy(iior)
         if (fiy(iior) < Ymin(mstr,56))Ymin(mstr,56) = fiy(iior)
         if (extky(iior) > Ymax(mstr,57))Ymax(mstr,57) = extky(iior)
         if (extky(iior) < Ymin(mstr,57))Ymin(mstr,57) = extky(iior)
         if (fiby(iior) > Ymax(mstr,58))Ymax(mstr,58) = fiby(iior)
         if (fiby(iior) < Ymin(mstr,58))Ymin(mstr,58) = fiby(iior)
         if (akmuey(iior) > Ymax(mstr,59))Ymax(mstr,59) = akmuey(iior)
         if (akmuey(iior) < Ymin(mstr,59))Ymin(mstr,59) = akmuey(iior)
         if (agmuey(iior) > Ymax(mstr,60))Ymax(mstr,60) = agmuey(iior)
         if (agmuey(iior) < Ymin(mstr,60))Ymin(mstr,60) = agmuey(iior)
         if (abmuey(iior) > Ymax(mstr,61))Ymax(mstr,61) = abmuey(iior)
         if (abmuey(iior) < Ymin(mstr,61))Ymin(mstr,61) = abmuey(iior)
         if (fhey(iior) > Ymax(mstr,62))Ymax(mstr,62) = fhey(iior)
         if (fhey(iior) < Ymin(mstr,62))Ymin(mstr,62) = fhey(iior)
         if (fhegy(iior) > Ymax(mstr,63))Ymax(mstr,63) = fhegy(iior)
         if (fhegy(iior) < Ymin(mstr,63))Ymin(mstr,63) = fhegy(iior)
         if (fheby(iior) > Ymax(mstr,64))Ymax(mstr,64) = fheby(iior)
         if (fheby(iior) < Ymin(mstr,64))Ymin(mstr,64) = fheby(iior)
         if (akry(iior) > Ymax(mstr,65))Ymax(mstr,65) = akry(iior)
         if (akry(iior) < Ymin(mstr,65))Ymin(mstr,65) = akry(iior)
         if (agrey(iior) > Ymax(mstr,66))Ymax(mstr,66) = agrey(iior)
         if (agrey(iior) < Ymin(mstr,66))Ymin(mstr,66) = agrey(iior)
         if (abrey(iior) > Ymax(mstr,67))Ymax(mstr,67) = abrey(iior)
         if (abrey(iior) < Ymin(mstr,67))Ymin(mstr,67) = abrey(iior)
         if (chlaky(iior) > Ymax(mstr,68))Ymax(mstr,68) = chlaky(iior)
         if (chlaky(iior) < Ymin(mstr,68))Ymin(mstr,68) = chlaky(iior)
         if (chlagy(iior) > Ymax(mstr,69))Ymax(mstr,69) = chlagy(iior)
         if (chlagy(iior) < Ymin(mstr,69))Ymin(mstr,69) = chlagy(iior)
         if (chlaby(iior) > Ymax(mstr,70))Ymax(mstr,70) = chlaby(iior)
         if (chlaby(iior) < Ymin(mstr,70))Ymin(mstr,70) = chlaby(iior)
         if (cchlky(iior) > Ymax(mstr,187))Ymax(mstr,187) = cchlky(iior)
         if (cchlky(iior) < Ymin(mstr,187))Ymin(mstr,187) = cchlky(iior)
         if (cchlgy(iior) > Ymax(mstr,188))Ymax(mstr,188) = cchlgy(iior)
         if (cchlgy(iior) < Ymin(mstr,188))Ymin(mstr,188) = cchlgy(iior)
         if (cchlby(iior) > Ymax(mstr,189))Ymax(mstr,189) = cchlby(iior)
         if (cchlby(iior) < Ymin(mstr,189))Ymin(mstr,189) = cchlby(iior)
         if (iry(iior) > Ymax(mstr,71))Ymax(mstr,71) = iry(iior)
         if (iry(iior) < Ymin(mstr,71))Ymin(mstr,71) = iry(iior)
         if (rmuasy(iior) > Ymax(mstr,72))Ymax(mstr,72) = rmuasy(iior)
         if (rmuasy(iior) < Ymin(mstr,72))Ymin(mstr,72) = rmuasy(iior)
         if (rakry(iior) > Ymax(mstr,73))Ymax(mstr,73) = rakry(iior)
         if (rakry(iior) < Ymin(mstr,73))Ymin(mstr,73) = rakry(iior)
         if (rbary(iior) > Ymax(mstr,74))Ymax(mstr,74) = rbary(iior)
         if (rbary(iior) < Ymin(mstr,74))Ymin(mstr,74) = rbary(iior)
         cbsbab = vbsby(iior)                               &
                - akiy(iior) * Caki * bsbki                 &
                + ably(iior) * Cabl * bsbbl                 &
                + agry(iior) * Cagr * bsbgr                 &
                + (zooiny(iior) * GROT/1000.) * bsbzoo
         if (cbsbab > Ymax(mstr,75))Ymax(mstr,75) = cbsbab
         if (cbsbab < Ymin(mstr,75))Ymin(mstr,75) = cbsbab
         abbau = -.1
         if (iwsim == 3 .and. vcsby(iior) > 0.0) abbau = vbsby(iior)/vcsby(iior)
         if (abbau > Ymax(mstr,76))Ymax(mstr,76) = abbau
         if (abbau < Ymin(mstr,76))Ymin(mstr,76) = abbau
         if (CMy(iior) > Ymax(mstr,77))Ymax(mstr,77) = CMy(iior)
         if (CMy(iior) < Ymin(mstr,77))Ymin(mstr,77) = CMy(iior)
         if (BACy(iior) > Ymax(mstr,78))Ymax(mstr,78) = BACy(iior)
         if (BACy(iior) < Ymin(mstr,78))Ymin(mstr,78) = BACy(iior)
         if (CDy(1,iior) > Ymax(mstr,79))Ymax(mstr,79) = CDy(1,iior)
         if (CDy(1,iior) < Ymin(mstr,79))Ymin(mstr,79) = CDy(1,iior)
         if (CDy(2,iior) > Ymax(mstr,80))Ymax(mstr,80) = CDy(2,iior)
         if (CDy(2,iior) < Ymin(mstr,80))Ymin(mstr,80) = CDy(2,iior)
         if (CPy(1,iior) > Ymax(mstr,81))Ymax(mstr,81) = CPy(1,iior)
         if (CPy(1,iior) < Ymin(mstr,81))Ymin(mstr,81) = CPy(1,iior)
         if (CPy(2,iior) > Ymax(mstr,82))Ymax(mstr,82) = CPy(2,iior)
         if (CPy(2,iior) < Ymin(mstr,82))Ymin(mstr,82) = CPy(2,iior)
         if (BACmuy(iior) > Ymax(mstr,83))Ymax(mstr,83) = BACmuy(iior)
         if (BACmuy(iior) < Ymin(mstr,83))Ymin(mstr,83) = BACmuy(iior)
         if (HNFBAy(iior) > Ymax(mstr,84))Ymax(mstr,84) = HNFBAy(iior)
         if (HNFBAy(iior) < Ymin(mstr,84))Ymin(mstr,84) = HNFBAy(iior)
         if (BVHNFy(iior) <= 0.0) then
            Ymax(mstr,85) = -1
            Ymin(mstr,85) = -1
            goto 791
         endif
         HNFin = CHNFy(iior)*1.e6/(BVHNFy(iior)*0.22)
         if (HNFin > Ymax(mstr,85))Ymax(mstr,85) = HNFin
         if (HNFin < Ymin(mstr,85))Ymin(mstr,85) = HNFin
         791 if (HNFupy(iior) > Ymax(mstr,86))Ymax(mstr,86) = HNFupy(iior)
         if (HNFupy(iior) < Ymin(mstr,86))Ymin(mstr,86) = HNFupy(iior)
         if (HNFrey(iior) > Ymax(mstr,87))Ymax(mstr,87) = HNFrey(iior)
         if (HNFrey(iior) < Ymin(mstr,87))Ymin(mstr,87) = HNFrey(iior)
         if (HNFexy(iior) > Ymax(mstr,88))Ymax(mstr,88) = HNFexy(iior)
         if (HNFexy(iior) < Ymin(mstr,88))Ymin(mstr,88) = HNFexy(iior)
         if (HNFmoy(iior) > Ymax(mstr,89))Ymax(mstr,89) = HNFmoy(iior)
         if (HNFmoy(iior) < Ymin(mstr,89))Ymin(mstr,89) = HNFmoy(iior)
         if (HNFmuy(iior) > Ymax(mstr,90))Ymax(mstr,90) = HNFmuy(iior)
         if (HNFmuy(iior) < Ymin(mstr,90))Ymin(mstr,90) = HNFmuy(iior)
         if (HNFzy(iior) > Ymax(mstr,91))Ymax(mstr,91) = HNFzy(iior)
         if (HNFzy(iior) < Ymin(mstr,91))Ymin(mstr,91) = HNFzy(iior)
         if (HNFdry(iior) > Ymax(mstr,92))Ymax(mstr,92) = HNFdry(iior)
         if (HNFdry(iior) < Ymin(mstr,92))Ymin(mstr,92) = HNFdry(iior)
         if (susny(iior) > Ymax(mstr,93))Ymax(mstr,93) = susny(iior)
         if (susny(iior) < Ymin(mstr,93))Ymin(mstr,93) = susny(iior)
         if (bettny(iior) > Ymax(mstr,94))Ymax(mstr,94) = bettny(iior)
         if (bettny(iior) < Ymin(mstr,94))Ymin(mstr,94) = bettny(iior)
         if (dony(iior) > Ymax(mstr,95))Ymax(mstr,95) = dony(iior)
         if (dony(iior) < Ymin(mstr,95))Ymin(mstr,95) = dony(iior)
         saN = agrn4y(iior)+akin4y(iior)+abln4y(iior)
         if (saN > Ymax(mstr,96))Ymax(mstr,96) = saN
         if (saN < Ymin(mstr,96))Ymin(mstr,96) = saN
         if (FluN3y(iior) > Ymax(mstr,97))Ymax(mstr,97) = FluN3y(iior)
         if (FluN3y(iior) < Ymin(mstr,97))Ymin(mstr,97) = FluN3y(iior)
         vx0mue = vx0y(iior)*1000.
         if (vx0mue < 0.0)vx0mue = -1.
         if (vx0mue > Ymax(mstr,98))Ymax(mstr,98) = vx0mue
         if (vx0mue < Ymin(mstr,98))Ymin(mstr,98) = vx0mue
         if (sedx0y(iior) > Ymax(mstr,99))Ymax(mstr,99) = sedx0y(iior)
         if (sedx0y(iior) < Ymin(mstr,99))Ymin(mstr,99) = sedx0y(iior)
         vx02mu = vx02y(iior)*1000.
         if (vx02mu < 0.0)vx02mu = -1.
         if (vx02mu > Ymax(mstr,100))ymax(mstr,100) = vx02mu
         if (vx02mu < Ymin(mstr,100))ymin(mstr,100) = vx02mu
         Ymax(mstr,101) = (Ymax(mstr,93)+Ymax(mstr,94))*4.33
         Ymin(mstr,101) = (Ymin(mstr,93)+Ymin(mstr,94))*4.33
         if (dalgoy(iior) > Ymax(mstr,102))Ymax(mstr,102) = dalgoy(iior)
         if (dalgoy(iior) < Ymin(mstr,102))Ymin(mstr,102) = dalgoy(iior)
         if (dalaoy(iior) > Ymax(mstr,103))Ymax(mstr,103) = dalaoy(iior)
         if (dalaoy(iior) < Ymin(mstr,103))Ymin(mstr,103) = dalaoy(iior)
         if (bsbty(iior) > Ymax(mstr,104))Ymax(mstr,104) = bsbty(iior)
         if (bsbty(iior) < Ymin(mstr,104))Ymin(mstr,104) = bsbty(iior)
         if (schlry(iior) > Ymax(mstr,105))Ymax(mstr,105) = schlry(iior)
         if (schlry(iior) < Ymin(mstr,105))Ymin(mstr,105) = schlry(iior)
         if (bFlN3y(iior) > Ymax(mstr,106))Ymax(mstr,106) = bFlN3y(iior)
         if (bFlN3y(iior) < Ymin(mstr,106))Ymin(mstr,106) = bFLN3y(iior)
         if (bsbbey(iior) > Ymax(mstr,107))Ymax(mstr,107) = bsbbey(iior)
         if (bsbbey(iior) < Ymin(mstr,107))Ymin(mstr,107) = bsbbey(iior)
         if (o2ei1y(iior) > Ymax(mstr,108))Ymax(mstr,108) = o2ei1y(iior)
         if (o2ei1y(iior) < Ymin(mstr,108))Ymin(mstr,108) = o2ei1y(iior)
         sabow = abowgy(iior)+abowky(iior)
         sabor = aborgy(iior)+aborky(iior)
         if (sabow > Ymax(mstr,109))Ymax(mstr,109) = sabow
         if (sabow < Ymin(mstr,109))Ymin(mstr,109) = sabow
         if (sabor > Ymax(mstr,110))Ymax(mstr,110) = sabor
         if (sabor < Ymin(mstr,110))Ymin(mstr,110) = sabor
         if (ro2dry(iior) > Ymax(mstr,111))Ymax(mstr,111) = ro2dry(iior)
         if (ro2dry(iior) < Ymin(mstr,111))Ymin(mstr,111) = ro2dry(iior)
         if (zoro2y(iior) > Ymax(mstr,112))Ymax(mstr,112) = zoro2y(iior)
         if (zoro2y(iior) < Ymin(mstr,112))Ymin(mstr,112) = zoro2y(iior)
         if (po2py(iior) > Ymax(mstr,113))Ymax(mstr,113) = po2py(iior)
         if (po2py(iior) < Ymin(mstr,113))Ymin(mstr,113) = po2py(iior)
         if (po2ry(iior) > Ymax(mstr,114))Ymax(mstr,114) = po2ry(iior)
         if (po2ry(iior) < Ymin(mstr,114))Ymin(mstr,114) = po2ry(iior)
         if (tracer(iior) > Ymax(mstr,169))Ymax(mstr,169) = tracer(iior)
         if (tracer(iior) < Ymin(mstr,169))Ymin(mstr,169) = tracer(iior)
         if (bvbsby(iior) > Ymax(mstr,115))Ymax(mstr,115) = bvbsby(iior)
         if (bvbsby(iior) < Ymin(mstr,115))Ymin(mstr,115) = bvbsby(iior)
         if (bvcsby(iior) > Ymax(mstr,116))Ymax(mstr,116) = bvcsby(iior)
         if (bvcsby(iior) < Ymin(mstr,116))Ymin(mstr,116) = bvcsby(iior)
         if (bnh4y(iior) > Ymax(mstr,117))Ymax(mstr,117) = bnh4y(iior)
         if (bnh4y(iior) < Ymin(mstr,117))Ymin(mstr,117) = bnh4y(iior)
         if (bno2y(iior) > Ymax(mstr,118))Ymax(mstr,118) = bno2y(iior)
         if (bno2y(iior) < Ymin(mstr,118))Ymin(mstr,118) = bno2y(iior)
         if (bno3y(iior) > Ymax(mstr,119))Ymax(mstr,119) = bno3y(iior)
         if (bno3y(iior) < Ymin(mstr,119))Ymin(mstr,119) = bno3y(iior)
         if (bgsNy(iior) > Ymax(mstr,120))Ymax(mstr,120) = bgsNy(iior)
         if (bgsNy(iior) < Ymin(mstr,120))Ymin(mstr,120) = bgsNy(iior)
         if (bgelpy(iior) > Ymax(mstr,121))Ymax(mstr,121) = bgelpy(iior)
         if (bgelpy(iior) < Ymin(mstr,121))Ymin(mstr,121) = bgelpy(iior)
         if (bgsPy(iior) > Ymax(mstr,122))Ymax(mstr,122) = bgsPy(iior)
         if (bgsPy(iior) < Ymin(mstr,122))Ymin(mstr,122) = bgsPy(iior)
         if (bsiy(iior) > Ymax(mstr,123))Ymax(mstr,123) = bsiy(iior)
         if (bsiy(iior) < Ymin(mstr,123))Ymin(mstr,123) = bsiy(iior)
         if (bchlay(iior) > Ymax(mstr,124))Ymax(mstr,124) = bchlay(iior)
         if (bchlay(iior) < Ymin(mstr,124))Ymin(mstr,124) = bchlay(iior)
         if (bzooiy(iior) > Ymax(mstr,125))Ymax(mstr,125) = bzooiy(iior)
         if (bzooiy(iior) < Ymin(mstr,125))Ymin(mstr,125) = bzooiy(iior)
         if (bphy(iior) > Ymax(mstr,126))Ymax(mstr,126) = bphy(iior)
         if (bphy(iior) < Ymin(mstr,126))Ymin(mstr,126) = bphy(iior)
         if (bmwy(iior) > Ymax(mstr,127))Ymax(mstr,127) = bmwy(iior)
         if (bmwy(iior) < Ymin(mstr,127))Ymin(mstr,127) = bmwy(iior)
         if (bcay(iior) > Ymax(mstr,128))Ymax(mstr,128) = bcay(iior)
         if (bcay(iior) < Ymin(mstr,128))Ymin(mstr,128) = bcay(iior)
         if (blfy(iior) > Ymax(mstr,129))Ymax(mstr,129) = blfy(iior)
         if (blfy(iior) < Ymin(mstr,129))Ymin(mstr,129) = blfy(iior)
         if (bssaly(iior) > Ymax(mstr,130))Ymax(mstr,130) = bssaly(iior)
         if (bssaly(iior) < Ymin(mstr,130))Ymin(mstr,130) = bssaly(iior)
         if (btempy(iior) > Ymax(mstr,131))Ymax(mstr,131) = btempy(iior)
         if (btempy(iior) < Ymin(mstr,131))Ymin(mstr,131) = btempy(iior)
         if (bo2y(iior) > Ymax(mstr,132))Ymax(mstr,132) = bo2y(iior)
         if (bo2y(iior) < Ymin(mstr,132))Ymin(mstr,132) = bo2y(iior)
         Ymax(mstr,133) = -1.
         Ymin(mstr,133) = -1.
         if (bakiy(iior) <= 0.0)bakiy(iior) = 0.0000000001
         if (bagry(iior) <= 0.0)bagry(iior) = 0.0000000001
         if (bably(iior) <= 0.0)bably(iior) = 0.0000000001
         bakg = bakiy(iior)*Caki+bagry(iior)*Cagr+bably(iior)*Cabl
         if (bakg > Ymax(mstr,134))Ymax(mstr,134) = bakg
         if (bakg < Ymin(mstr,134))Ymin(mstr,134) = bakg
         bdaW = (bdakiy(iior)+bdagry(iior)+bdably(iior))*24.               &
                /(bakiy(iior)+bagry(iior)+bably(iior))
         if (bdaW > Ymax(mstr,135))Ymax(mstr,135) = bdaW
         if (bdaW < Ymin(mstr,135))Ymin(mstr,135) = bdaW
         bdaR = (bdaaky(iior)+bdaagy(iior)+bdaagy(iior))*24.               &
                /(bakiy(iior)+bagry(iior)+bably(iior))
         if (bdaR > Ymax(mstr,136))Ymax(mstr,136) = bdaR
         if (bdaR < Ymin(mstr,136))Ymin(mstr,136) = bdaR
         baM = (bkmory(iior)+bgmory(iior)+bbmory(iior))*24.                &
               /(bakiy(iior)+bagry(iior)+bably(iior))
         if (baM > Ymax(mstr,137))Ymax(mstr,137) = baM
         if (baM < Ymin(mstr,137))Ymin(mstr,137) = baM
         baS = (bsedky(iior)+bsedgy(iior)+bsedby(iior))*24.                &
               /(bakiy(iior)+bagry(iior)+bably(iior))
         if (baS > Ymax(mstr,138))Ymax(mstr,138) = baS
         if (baS < Ymin(mstr,138))Ymin(mstr,138) = baS
         baZ = (bazoky(iior)+bazogy(iior)+bazoby(iior))*24.                &
               /(bakiy(iior)+bagry(iior)+bably(iior))
         if (baZ > Ymax(mstr,139))Ymax(mstr,139) = baZ
         if (baZ < Ymin(mstr,139))Ymin(mstr,139) = baZ
         Ymax(mstr,140) = -1.
         Ymin(mstr,140) = -1.
         Ymax(mstr,141) = -1.
         Ymin(mstr,141) = -1.
         ztp = btpkiy(iior)*bkigry(iior)+btpgry(iior)                      &
               *(1.-bkigry(iior)-bantby(iior))+btpbly(iior)*bantby(iior)
         if (ztp > Ymax(mstr,142))Ymax(mstr,142) = ztp
         if (ztp < Ymin(mstr,142))Ymin(mstr,142) = ztp
         if (bkigry(iior) > Ymax(mstr,143))Ymax(mstr,143) = bkigry(iior)
         if (bkigry(iior) < Ymin(mstr,143))Ymin(mstr,143) = bkigry(iior)
         if (bantby(iior) > Ymax(mstr,144))Ymax(mstr,144) = bantby(iior)
         if (bantby(iior) < Ymin(mstr,144))Ymin(mstr,144) = bantby(iior)
         sBco = (bacoky(iior)+bacogy(iior)+bacoby(iior))*24.
         if (sBco > Ymax(mstr,145))Ymax(mstr,145) = sBco
         if (sBco < Ymin(mstr,145))Ymin(mstr,145) = sBco
         if (bfikay(iior) > Ymax(mstr,146))Ymax(mstr,146) = bfikay(iior)
         if (bfikay(iior) < Ymin(mstr,146))Ymin(mstr,146) = bfikay(iior)
         if (bextky(iior) > Ymax(mstr,147))Ymax(mstr,147) = bextky(iior)
         if (bextky(iior) < Ymin(mstr,147))Ymin(mstr,147) = bextky(iior)
         if (bfibay(iior) > Ymax(mstr,148))Ymax(mstr,148) = bfibay(iior)
         if (bfibay(iior) < Ymin(mstr,148))Ymin(mstr,148) = bfibay(iior)
         if (bkmuay(iior) > Ymax(mstr,149))Ymax(mstr,149) = bkmuay(iior)
         if (bkmuay(iior) < Ymin(mstr,149))Ymin(mstr,149) = bkmuay(iior)
         if (bgmuay(iior) > Ymax(mstr,150))Ymax(mstr,150) = bgmuay(iior)
         if (bgmuay(iior) < Ymin(mstr,150))Ymin(mstr,150) = bgmuay(iior)
         if (bbmuay(iior) > Ymax(mstr,151))Ymax(mstr,151) = bbmuay(iior)
         if (bbmuay(iior) < Ymin(mstr,151))Ymin(mstr,151) = bbmuay(iior)
         if (bfhkay(iior) > Ymax(mstr,152))Ymax(mstr,152) = bfhkay(iior)
         if (bfhkay(iior) < Ymin(mstr,152))Ymin(mstr,152) = bfhkay(iior)
         if (bfhgay(iior) > Ymax(mstr,153))Ymax(mstr,153) = bfhgay(iior)
         if (bfhgay(iior) < Ymin(mstr,153))Ymin(mstr,153) = bfhgay(iior)
         if (bfhbay(iior) > Ymax(mstr,154))Ymax(mstr,154) = bfhbay(iior)
         if (bfhbay(iior) < Ymin(mstr,154))Ymin(mstr,154) = bfhbay(iior)
         if (bkray(iior) > Ymax(mstr,155))Ymax(mstr,155) = bkray(iior)
         if (bkray(iior) < Ymin(mstr,155))Ymin(mstr,155) = bkray(iior)
         if (bgray(iior) > Ymax(mstr,156))Ymax(mstr,156) = bgray(iior)
         if (bgray(iior) < Ymin(mstr,156))Ymin(mstr,156) = bgray(iior)
         if (bbray(iior) > Ymax(mstr,157))Ymax(mstr,157) = bbray(iior)
         if (bbray(iior) < Ymin(mstr,157))Ymin(mstr,157) = bbray(iior)
         if (bchlky(iior) > Ymax(mstr,158))Ymax(mstr,158) = bchlky(iior)
         if (bchlky(iior) < Ymin(mstr,158))Ymin(mstr,158) = bchlky(iior)
         if (bchlgy(iior) > Ymax(mstr,159))Ymax(mstr,159) = bchlgy(iior)
         if (bchlgy(iior) < Ymin(mstr,159))Ymin(mstr,159) = bchlgy(iior)
         if (bchlby(iior) > Ymax(mstr,160))Ymax(mstr,160) = bchlby(iior)
         if (bchlby(iior) < Ymin(mstr,160))Ymin(mstr,160) = bchlby(iior)
         if (tau2y(iior) > Ymax(mstr,168))Ymax(mstr,168) = tau2y(iior)
         if (tau2y(iior) < Ymin(mstr,168))Ymin(mstr,168) = tau2y(iior)
         if (btracer(iior) > Ymax(mstr,170))Ymax(mstr,170) = btracer(iior)
         if (btracer(iior) < Ymin(mstr,170))Ymin(mstr,170) = btracer(iior)
         if (bbetNy(iior) > Ymax(mstr,173))Ymax(mstr,173) = bbetNy(iior)
         if (bbetNy(iior) < Ymin(mstr,173))Ymin(mstr,173) = bbetNy(iior)
         if (bJNO3y(iior) > Ymax(mstr,176))Ymax(mstr,176) = bJNO3y(iior)
         if (bJNO3y(iior) < Ymin(mstr,176))Ymin(mstr,176) = bJNO3y(iior)
         if (bJNH4y(iior) > Ymax(mstr,178))Ymax(mstr,178) = bJNH4y(iior)
         if (bJNH4y(iior) < Ymin(mstr,178))Ymin(mstr,178) = bJNH4y(iior)
         if (bJPO4y(iior) > Ymax(mstr,180))Ymax(mstr,180) = bJPO4y(iior)
         if (bJPO4y(iior) < Ymin(mstr,180))Ymin(mstr,180) = bJPO4y(iior)
         if (bJO2y(iior) > Ymax(mstr,182))Ymax(mstr,182) = bJO2y(iior)
         if (bJO2y(iior) < Ymin(mstr,182))Ymin(mstr,182) = bJO2y(iior)
         if (bJSiy(iior) > Ymax(mstr,186))Ymax(mstr,186) = bJSiy(iior)
         if (bJSiy(iior) < Ymin(mstr,186))Ymin(mstr,186) = bJSiy(iior)
         
         Ymax(mstr,171) = -1.
         Ymin(mstr,171) = -1.
         if (alNO3y(iior) > Ymax(mstr,174))Ymax(mstr,174) = alNO3y(iior)
         if (alNO3y(iior) < Ymin(mstr,174))Ymin(mstr,174) = alNO3y(iior)
         if (JNO3y(iior) > Ymax(mstr,175))Ymax(mstr,175) = JNO3y(iior)
         if (JNO3y(iior) < Ymin(mstr,175))Ymin(mstr,175) = JNO3y(iior)
         if (JNH4y(iior) > Ymax(mstr,177))Ymax(mstr,177) = JNH4y(iior)
         if (JNH4y(iior) < Ymin(mstr,177))Ymin(mstr,177) = JNH4y(iior)
         if (JPO4y(iior) > Ymax(mstr,179))Ymax(mstr,179) = JPO4y(iior)
         if (JPO4y(iior) < Ymin(mstr,179))Ymin(mstr,179) = JPO4y(iior)
         if (JO2y(iior) > Ymax(mstr,181))Ymax(mstr,181) = JO2y(iior)
         if (JO2y(iior) < Ymin(mstr,181))Ymin(mstr,181) = JO2y(iior)
         if (JSiy(iior) > Ymax(mstr,185))Ymax(mstr,185) = JSiy(iior)
         if (JSiy(iior) < Ymin(mstr,185))Ymin(mstr,185) = JSiy(iior)
         
      enddo               ! Ende Stationenschleife
   enddo                 ! Ende Strangschleife
   
   if (maus == 1)goto 105
   goto 9998
   
   
   ! --------------------------------------------------------------------------
   ! Ausgabe der Mittelwerte
   ! --------------------------------------------------------------------------
   105 continue
   ! itime = itimeh
   maus = 0
   ij = 1
   
   ! it_hy - Anzahl der Zeitschritt in der Hellphase
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      do iior = 1,mStas(mstr)
         
         xtempw = sumte(mstr,iior)/itime
         xCHNF = sCHNF(mstr,iior)/itime
         xBVHNF = sBVHNF(mstr,iior)/itime
         xCD1 = sCD(mstr,1,iior)/itime
         xCD2 = sCD(mstr,2,iior)/itime
         xCP1 = sCP(mstr,1,iior)/itime
         xCP2 = sCP(mstr,2,iior)/itime
         xCM = sCM(mstr,iior)/itime
         xBAC = sBAC(mstr,iior)/itime
         
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
         xgsZn = sumgsZn(mstr,iior)/itime
         xglZn = sumglZn(mstr,iior)/itime
         xgsCad = sumgsCad(mstr,iior)/itime
         xglCad = sumglCad(mstr,iior)/itime
         xgsCu = sumgsCu(mstr,iior)/itime
         xglCu = sumglCu(mstr,iior)/itime
         xgsNi = sumgsNi(mstr,iior)/itime
         xglNi = sumglNi(mstr,iior)/itime
         xgsAs = sumgsAs(mstr,iior)/itime
         xglAs = sumglAs(mstr,iior)/itime
         xgsPb = sumgsPb(mstr,iior)/itime
         xglPb = sumglPb(mstr,iior)/itime
         xgsCr = sumgsCr(mstr,iior)/itime
         xglCr = sumglCr(mstr,iior)/itime
         xgsFe = sumgsFe(mstr,iior)/itime
         xglFe = sumglFe(mstr,iior)/itime
         xgsHg = sumgsHg(mstr,iior)/itime
         xglHg = sumglHg(mstr,iior)/itime
         xgsMn = sumgsMn(mstr,iior)/itime
         xglMn = sumglMn(mstr,iior)/itime
         xgsU  = sumgsU(mstr,iior)/itime
         xglU  = sumglU(mstr,iior)/itime
         
         xdlarn = sumdln(mstr,iior)/itime
         xss = sumss(mstr,iior)/itime
         xpfl = sumpfl(mstr,iior)/itime
         xgsP = sgsP(mstr,iior)/itime
         xgsN = sgsN(mstr,iior)/itime
         do ndr = 1,nndr
            xidras(ndr) = sidras(mstr,iior,ndr)/itime
            xdrmas(ndr) = sdrmas(mstr,iior,ndr)/itime
            xdrakr(ndr) = sdrakr(mstr,iior,ndr)/itime
            xdrbar(ndr) = sdrbar(mstr,iior,ndr)/itime
            xdrmor(ndr) = sdrmor(mstr,iior,ndr)/itime
            xdrbio(ndr) = szdrg(mstr,iior,ndr)/itime
            xdbios(ndr) = szdrsg(mstr,iior,ndr)/itime
            xgewdr(ndr) = sgwdrg(mstr,iior,ndr)/itime
         enddo
         
         xcorI = scorIg(mstr,iior)/itime
         xcorIs = scoIsg(mstr,iior)/itime
         
         xbal = sumbal(mstr,iior)/itime
         xsusn = ssusn(mstr,iior)/itime
         xbettn = sbettn(mstr,iior)/itime
         xdon = sdon(mstr,iior)/itime
         xalgn = salgn(mstr,iior)/itime
         xalNO3 = salNO3(mstr,iior)/itime
         xFluN3 = sFluN3(mstr,iior)/itime
         xvx0 = (svx0(mstr,iior)/itime)*1000.
         if (xvx0 < 0.0)xvx0 = -1.
         xvx02 = (svx02(mstr,iior)/itime)*1000.
         if (xvx02 < 0.0)xvx02 = -1.
         xsedx = ssedx0(mstr,iior)/itime
         xsedal = ssedal(mstr,iior)/itime
         xalgzo = salgzo(mstr,iior)/itime
         xalgdr = salgdr(mstr,iior)/itime
         xalgco = salgco(mstr,iior)/itime
         xvoldr = svoldr(mstr,iior)/itime
         xdrpfe = sdrpfe(mstr,iior)/itime
         xabeow = sabeow(mstr,iior)/itime
         xabeor = sabeor(mstr,iior)/itime
         xdalg  = sdalg(mstr,iior)/itime
         xdalga = sdalga(mstr,iior)/itime
         xalmor = salmor(mstr,iior)/itime
         xblmor = sblmor(mstr,iior)/itime
         xsgo2n = ssgo2n(mstr,iior)/itime
         xsdbsb = ssdbsb(mstr,iior)/itime
         xsoein = ssoein(mstr,iior)/itime
         xsalgo = ssalgo(mstr,iior)/itime
         xo2nit = xsusn*4.33
         xalgo  = s2algo(mstr,iior)/itime
         xalgao = s2algao(mstr,iior)/itime
         xbsbt  = sbsbt(mstr,iior)/itime
         xschlr = sschlr(mstr,iior)/itime
         xbsbbe = sbsbbe(mstr,iior)/itime
         xo2phy = so2phy(mstr,iior)/itime
         xro2dr = sro2dr(mstr,iior)/itime
         xzooro = szooro(mstr,iior)/itime
         xpo2p  = spo2p(mstr,iior)/itime
         xpo2r  = spo2r(mstr,iior)/itime
         xir    = sir(mstr,iior)/itime
         xrmue  = srmue(mstr,iior)/itime
         xrakr  = srakr(mstr,iior)/itime
         xrbar  = srbar(mstr,iior)/itime
         xffood = sffood(mstr,iior)/itime
         xfik   = sfik(mstr,iior)/it_hy(mstr,iior)
         xfig   = sfig(mstr,iior)/itime
         xfib   = -1.
         xnaehr = snaehr(mstr,iior)/it_hy(mstr,iior)
         xakmua = sakmua(mstr,iior)/itime
         xagmua = sagmua(mstr,iior)/itime
         xabmua = sabmua(mstr,iior)/itime
         xfhek  = sfheka(mstr,iior)/itime
         xfheg  = sfhega(mstr,iior)/itime
         xfheb  = sfheba(mstr,iior)/itime
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
         
         if (iwsim /= 4 .and. iwsim /= 2) then
            algae_biomass = xaki + xagr + xabl
            if (algae_biomass > 0.0) then
               xalgdr = xalgdr * (1./tflie) / algae_biomass
               xalgzo = xalgzo * (1./tflie) / algae_biomass
               xdalg  = xdalg  * (1./tflie) / algae_biomass
               if (xdalg  < 0.00001) xdalg = 0.0
               xdalga = xdalga * (1./tflie) / algae_biomass
               if (xdalga < 0.00001) xdalga = 0.0
               xalmor = xalmor * (1./tflie) / algae_biomass
               if (xalmor < 0.00001) xalmor = 0.0
               xsedal = xsedal * (1./tflie) / algae_biomass
               xalgco = xalgco * (1./tflie) / algae_biomass
               xakigr = xagr*Cagr + xaki*Caki + xabl*Cabl
            else
               xalgdr = 0.0
               xalgzo = 0.0
               xdalg  = 0.0
               xdalga = 0.0
               xalmor = 0.0
               xsedal = 0.0
               xalgco = 0.0
               xakigr = 0.0
            endif
            
            cbsbab = xbsb5 - xaki*Caki*bsbki + xabl*Cabl*bsbbl + xagr*Cagr*bsbgr + (xzooind*GROT/1000.)*bsbzoo
            
            !     Berechnung der Abbaubarkeit
            abbau = -.1
            if (xcsb > 0.0)abbau = xbsb5/xcsb
            
            if (xBVHNF > 0.0) then
               ! Umrechnung in Zellzahlen
               xCHNFi = xCHNF*1.e6/(xBVHNF*0.22)
               ! xCHNF in æg/l
               xCHNF = xCHNF*1000.
            else
               xCHNFi = -1.
               xCHNF  = -1.
            endif
         else
            xalgdr = 0.0
            xalgzo = 0.0
            xdalg  = 0.0
            xdalga = 0.0
            xalmor = 0.0
            xsedal = 0.0
            xalgco = 0.0
            xakigr = 0.0
            xCHNFi = -1.
            xCHNF  = -1.
         endif
         
         ! Buhnenfelder
         if (nbuhn(mstr) == 1) then
            ! river stretch with groins
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
            ! Raten
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
            bxfib = -1.
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
            ! Colibacteria
            bxcoli = bscoli(mstr,iior)/itime
            ! Conservative substances (overwritten if active)
            bmikonss = 0.
            bxkonss  = 0.
            bmxkonss = 0.
            ! In case of groins, Dreissena are only present in groin fields
            bxdrpf = xdrpfe
            xdrpfe = 0.
            ! Heavy metals
            bxgsZn = bsgsZn(mstr,iior)/itime
            bxglZn = bsglZn(mstr,iior)/itime
            bxgsCad = bsgsCad(mstr,iior)/itime
            bxglCad = bsglCad(mstr,iior)/itime
            bxgsCu = bsgsCu(mstr,iior)/itime
            bxglCu = bsglCu(mstr,iior)/itime
            bxgsNi = bsgsNi(mstr,iior)/itime
            bxglNi = bsglNi(mstr,iior)/itime
            bxgsAs = bsgsAs(mstr,iior)/itime
            bxglAs = bsglAs(mstr,iior)/itime
            bxgsPb = bsgsPb(mstr,iior)/itime
            bxglPb = bsglPb(mstr,iior)/itime
            bxgsCr = bsgsCr(mstr,iior)/itime
            bxglCr = bsglCr(mstr,iior)/itime
            bxgsFe = bsgsFe(mstr,iior)/itime
            bxglFe = bsglFe(mstr,iior)/itime
            bxgsHg = bsgsHg(mstr,iior)/itime
            bxglHg = bsglHg(mstr,iior)/itime
            bxgsMn = bsgsMn(mstr,iior)/itime
            bxglMn = bsglMn(mstr,iior)/itime
            bxgsU = bsgsU(mstr,iior)/itime
            bxglU = bsglU(mstr,iior)/itime
            
            if ((bxaki+bxagr+bxabl) > 0.0) then
               bxaldr = bxaldr*24./(bxaki+bxagr+bxabl)
               bxalco = bxalco*24./(bxaki+bxagr+bxabl)
               bxalgz = bxalgz*24./(bxaki+bxagr+bxabl)
               bxdalg = bxdalg*24./(bxaki+bxagr+bxabl)
               bxdaa = bxdaa*24./(bxaki+bxagr+bxabl)
               bxamor = bxamor*24./(bxaki+bxagr+bxabl)
               bxseda = bxseda*24./(bxaki+bxagr+bxabl)
               bxakg = bxaki*Caki+bxabl*Cabl+bxagr*Cagr
            else
               bxaldr = -.1
               bxalco = -.1
               bxalgz = -.1
               bxdalg = -.1
               bxdaa = -.1
               bxamor = -.1
               bxseda = -.1
               bxakg = -.1
            endif
         elseif (nbuhn(mstr) == 0) then
            ! river stretch without groins
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
            ! Colibacteria
            bxcoli = -1.
            ! Conservative substances
            bmikonss = -1.
            bxkonss  = -1.
            bmxkonss = -1.
            ! Heavy metals
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
            bmigsAs(mstr,iior) = -1.
            bxgsAs = -1.
            bmxgsAs(mstr,iior) = -1.
            bmiglAs(mstr,iior) = -1.
            bxglAs = -1.
            bmxglAs(mstr,iior) = -1.
            bmigsPb(mstr,iior) = -1.
            bxgsPb = -1.
            bmxgsPb(mstr,iior) = -1.
            bmiglPb(mstr,iior) = -1.
            bxglPb = -1.
            bmxglPb(mstr,iior) = -1.
            bmigsCr(mstr,iior) = -1.
            bxgsCr = -1.
            bmxgsCr(mstr,iior) = -1.
            bmiglCr(mstr,iior) = -1.
            bxglCr = -1.
            bmxglCr(mstr,iior) = -1.
            bmigsFe(mstr,iior) = -1.
            bxgsFe = -1.
            bmxgsFe(mstr,iior) = -1.
            bmiglFe(mstr,iior) = -1.
            bxglFe = -1.
            bmxglFe(mstr,iior) = -1.
            bmigsHg(mstr,iior) = -1.
            bxgsHg = -1.
            bmxgsHg(mstr,iior) = -1.
            bmiglHg(mstr,iior) = -1.
            bxglHg = -1.
            bmxglHg(mstr,iior) = -1.
            bmigsMn(mstr,iior) = -1.
            bxgsMn = -1.
            bmxgsMn(mstr,iior) = -1.
            bmiglMn(mstr,iior) = -1.
            bxglMn = -1.
            bmxglMn(mstr,iior) = -1.
            bmigsU(mstr,iior) = -1.
            bxgsU = -1.
            bmxgsU(mstr,iior) = -1.
            bmiglU(mstr,iior) = -1.
            bxglU = -1.
            bmxglU(mstr,iior) = -1.
            
            ! Raten
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
         
         ! Ausgabe Dreissena Böschung
         if (xdrbio(2) == 0.0 .and. xdbios(2) > 0.0) then
            xdrbio(1) = xdbios(1)
            xdrbio(2) = xdbios(2)
         endif
         if (iwsim == 5) then
            miKonsS = mitemp(mstr,iior)
            xKonsS = xtempw
            mxKonsS = mxtemp(mstr,iior)
            mitemp(mstr,iior) = -9.99
            xtempw = -9.99
            mxtemp(mstr,iior) = -9.99
         else
            mikonss = 0.
            xkonss  = 0.
            mxkonss = 0.
         endif
         
         write(45,4100)itags,monats,Jahrs,mstr,Stakm(mstr,iior),STRID(mstr)
         write(45,4103)mib5(mstr,iior),xbsb5,mxb5(mstr,iior)                                &
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
                       ,xColi,mxColi(mstr,iior),miKonsS,xKonsS,mxKonsS,migsPb(mstr,iior)    &
                       ,xgsPb,mxgsPb(mstr,iior),miglPb(mstr,iior),xglPb,mxglPb(mstr,iior)   &
                       ,migsCad(mstr,iior),xgsCad,mxgsCad(mstr,iior),miglCad(mstr,iior)     &
                       ,xglCad,mxglCad(mstr,iior),migsCr(mstr,iior),xgsCr,mxgsCr(mstr,iior) &
                       ,miglCr(mstr,iior),xglCr,mxglCr(mstr,iior),migsFe(mstr,iior)         &
                       ,xgsFe,mxgsFe(mstr,iior),miglFe(mstr,iior),xglFe,mxglFe(mstr,iior)
         write(45,4104)migsCu(mstr,iior),xgsCu,mxgsCu(mstr,iior),miglCu(mstr,iior),xglCu    &
                       ,mxglCu(mstr,iior),migsMn(mstr,iior),xgsMn,mxgsMn(mstr,iior)         &
                       ,miglMn(mstr,iior),xglMn,mxglMn(mstr,iior),migsNi(mstr,iior),xgsNi   &
                       ,mxgsNi(mstr,iior),miglNi(mstr,iior),xglNi,mxglNi(mstr,iior)         &
                       ,migsHg(mstr,iior),xgsHg,mxgsHg(mstr,iior),miglHg(mstr,iior),xglHg   &
                       ,mxglHg(mstr,iior),migsU(mstr,iior),xgsU,mxgsU(mstr,iior)            &
                       ,miglU(mstr,iior),xglU,mxglU(mstr,iior),migsZn(mstr,iior)            &
                       ,xgsZn,mxgsZn(mstr,iior),miglZn(mstr,iior),xglZn,mxglZn(mstr,iior)   &
                       ,migsAs(mstr,iior),xgsAs,mxgsAs(mstr,iior),miglAs(mstr,iior),xglAs   &
                       ,mxglAs(mstr,iior)
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
         
         write(45,4018)dsedH(mstr,iior)
         
         bxmicl = bxcoli
         bxmxcl = bxcoli
         if (iwsim == 5) then
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
                       ,bmigsPb(mstr,iior),bxgsPb,bmxgsPb(mstr,iior),bmiglPb(mstr,iior),bxglPb,bmxglPb(mstr,iior)                                        &
                       ,bmigsCad(mstr,iior),bxgsCad,bmxgsCad(mstr,iior),bmiglCad(mstr,iior),bxglCad,bmxglCad(mstr,iior)                                  &
                       ,bmigsCr(mstr,iior),bxgsCr,bmxgsCr(mstr,iior),bmiglCr(mstr,iior),bxglCr,bmxglCr(mstr,iior)                                        &
                       ,bmigsFe(mstr,iior),bxgsFe,bmxgsFe(mstr,iior),bmiglFe(mstr,iior),bxglFe,bmxglFe(mstr,iior)
         write(45,4104)bmigsCu(mstr,iior),bxgsCu,bmxgsCu(mstr,iior),bmiglCu(mstr,iior),bxglCu,bmxglCu(mstr,iior)                                        &
                       ,bmigsMn(mstr,iior),bxgsMn,bmxgsMn(mstr,iior),bmiglMn(mstr,iior),bxglMn,bmxglMn(mstr,iior)                                        &
                       ,bmigsNi(mstr,iior),bxgsNi,bmxgsNi(mstr,iior),bmiglNi(mstr,iior),bxglNi,bmxglNi(mstr,iior)                                        &
                       ,bmigsHg(mstr,iior),bxgsHg,bmxgsHg(mstr,iior),bmiglHg(mstr,iior),bxglHg,bmxglHg(mstr,iior)                                        &
                       ,bmigsU(mstr,iior),bxgsU,bmxgsU(mstr,iior),bmiglU(mstr,iior),bxglU,bmxglU(mstr,iior)                                              &
                       ,bmigsZn(mstr,iior),bxgsZn,bmxgsZn(mstr,iior),bmiglZn(mstr,iior),bxglZn,bmxglZn(mstr,iior)                                        &
                       ,bmigsAs(mstr,iior),bxgsAs,bmxgsAs(mstr,iior),bmiglAs(mstr,iior),bxglAs,bmxglAs(mstr,iior)
         
         write(45,4010)bxakg,bxdalg,bxdaa,bxamor,bxseda,bxalgz,bxaldr,bxdrpf,xbnaeh,bxvkg                               &
                       ,bxantb,bxalco,bxfik,bxfig,bxfib,bxkmue,bxgmue,bxbmue,bxhek,bxheg,bxheb,bxkre,bxgre,bxbre         &
                       ,bxchlk,bxchlg,bxchlb
         
         write(45,4011)bxFlN3,bxJNO3,bxJNH4,bxBetN
         
         write(45,4020)bxJPO4,bxJSi
         
         write(45,4021)bxJO2
         
         
         4100 format(I2,2x,I2,2x,I4,2x,I5,2x,f8.3,2x,I5)
         4103 format(3(f6.2,2x),3(f6.2,2x),3(f6.2,2x),3(f6.3,2x),3(f9.6,2x)  &
         ,3(f5.2,2x),3(f6.3,2x),3(f5.2,2x),3(f5.2,2x),3(f6.2,2x)             &
         ,3(f7.1,2x),3(f5.2,2x),3(f5.2,2x),3(f5.1,2x),3(f8.1,2x)             &
         ,3(f8.2,2x),3(f5.2,2x),3(f5.2,2x),3(E9.2,2x),3(f7.1,2x)             &
         ,6(f6.2,2x),6(f7.3,2x),6(f6.2,2x),6(f8.1,2x))
         4104 format(6(F6.2,2x),6(f8.1,2x),6(f6.2,2x),6(f7.3,2x),6(F7.3,2x)  &
         ,6(f8.1,2x),6(F5.1,2x))
         
         4001 format(F7.2)
         4002 format(f7.2)
         4365 format(f7.2,2x,f7.2,2x,f7.2,2x,f7.2,2x,f7.2,2x,f6.3,2x,f6.3    &
         ,2x,f6.3,2x,f6.3,2x,f7.3,2x,f7.5,2x,f7.5,2x,f7.5,2x,f7.5            &
         ,2x,f7.5,2x,f7.5,2x,f5.3)
         4004 format(f8.1,2x,f8.1)
         4010 format(f6.2,2x,f8.5,2x,f8.5,2x,f8.5,2x,f8.5,2x,f9.5,2x,f9.5    &
         ,2x,f6.2,2x,f5.2,2x,f5.2,2x,f5.2,2x,f12.8,2x,f5.2                   &
         ,2x,f5.2,2x,f5.2,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3                    &
         ,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3                            &
         ,2x,f6.1,2x,f6.1,2x,f6.1,2x,f6.2,2x,f6.2,2x,f6.2)
         4011 format(F10.7,2x,F10.8,2x,f10.8,2x,F8.6)
         4265 format(f6.3,2x,f6.3,2x,f6.4,2x,f6.4)
         4111 format(f6.3,2x,f5.3,2x,f6.3,2x,f6.3,2x,f7.3,2x,f7.3,2x         &
         ,f7.3,2x,f7.3,2x,f6.3,2x,f6.3)
         4215 format(f8.1,2x,f7.3,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3,2x         &
         ,f6.3,2x,f6.3,2x,f6.3)
         4003 format(f7.5,2x,f10.8,2x,f7.5,2x,f7.5,2x,F7.5                   &
         ,2x,f10.7,2x,f10.8,2x,f10.8,2x,f8.5,2x,f6.3,2x,f8.5)
         4017 format(f7.4,2x,f7.4,2x,f7.4,2x,f7.4,2x,f10.8,2x,f10.8          &
         ,2x,f7.4,2x,f7.4,2x,f7.4,2x,f7.4,2x,f7.4,2x,f7.4                    &
         ,2x,f7.4,2x,f7.4)
         4018 format(F12.6)
         4020 format(F10.8,2x,F10.8)
         4021 format(F11.8)
         
      enddo
   enddo
   
   if (iend == 1)goto 999
   itime = itimeh
   goto 9998
   
   ! --------------------------------------------------------------------------
   ! Ausschreiben der Min/Max-Blöcke zur grafischen Darstellung in Gerris
   ! --------------------------------------------------------------------------
   999 continue
   
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      if (Ymin(mstr,20) > 0.0)Ymin(mstr,20) = Ymin(mstr,20)*1000.
      
      do iior = 1,mStas(mstr)
         
         ! EregebT.txt
         write(155,'(a7,14x,I5,2x,F8.3,2x,i5)') 'Minimum', mstr,Stakm(mstr,iior),STRID(mstr)
         
         write(155,5105)Ymin(mstr,1),Ymin(mstr,2),Ymin(mstr,3)                            &
                        ,Ymin(mstr,4),Ymin(mstr,5),Ymin(mstr,6)                           &
                        ,Ymin(mstr,7),Ymin(mstr,8),Ymin(mstr,9)                           &
                        ,Ymin(mstr,10),Ymin(mstr,11),Ymin(mstr,12)                        &
                        ,Ymin(mstr,13),Ymin(mstr,14),Ymin(mstr,15)                        &
                        ,Ymin(mstr,16),Ymin(mstr,17),Ymin(mstr,18)                        &
                        ,Ymin(mstr,20),Ymin(mstr,19),Ymin(mstr,21),Ymin(mstr,22)          &
                        ,Ymin(mstr,169)
         
         write(155,5207)Ymin(mstr,203),Ymin(mstr,204),Ymin(mstr,195),Ymin(mstr,196)       &
                        ,Ymin(mstr,205),Ymin(mstr,206),Ymin(mstr,207),Ymin(mstr,208)      &
                        ,Ymin(mstr,197),Ymin(mstr,198),Ymin(mstr,211),Ymin(mstr,212)      &
                        ,Ymin(mstr,199),Ymin(mstr,200),Ymin(mstr,209),Ymin(mstr,210)      &
                        ,Ymin(mstr,213),Ymin(mstr,214),Ymin(mstr,193),Ymin(mstr,194)      &
                        ,Ymin(mstr,201),Ymin(mstr,202)
         
         write(155,5205)(Ymin(mstr,104)*hcUmt),(Ymin(mstr,93)*4.33*hcUmt),(Ymin(mstr,108)*hcUmt) &
                        ,(Ymin(mstr,102)*hcUmt),Ymin(mstr,187),Ymin(mstr,188)             &
                        ,Ymin(mstr,189),(Ymin(mstr,112)*hcUmt),(Ymin(mstr,105)*hcUmt)     &
                        ,(Ymin(mstr,94)*hcUmt)
         
         write(155,5115)Ymin(mstr,115),Ymin(mstr,116),Ymin(mstr,117)                      &
                        ,Ymin(mstr,118),Ymin(mstr,119),Ymin(mstr,120)                     &
                        ,Ymin(mstr,121),Ymin(mstr,122),Ymin(mstr,123)                     &
                        ,Ymin(mstr,124),Ymin(mstr,125),Ymin(mstr,126)                     &
                        ,Ymin(mstr,127),Ymin(mstr,128),Ymin(mstr,129)                     &
                        ,Ymin(mstr,130),Ymin(mstr,131),Ymin(mstr,132)                     &
                        ,Ymin(mstr,133),Ymin(mstr,171),Ymin(mstr,168)                     &
                        ,Ymin(mstr,170)
         
         write(155,5207)Ymin(mstr,203),Ymin(mstr,204),Ymin(mstr,195),Ymin(mstr,196)       &
                        ,Ymin(mstr,205),Ymin(mstr,206),Ymin(mstr,207),Ymin(mstr,208)      &
                        ,Ymin(mstr,197),Ymin(mstr,198),Ymin(mstr,211),Ymin(mstr,212)      &
                        ,Ymin(mstr,199),Ymin(mstr,200),Ymin(mstr,209),Ymin(mstr,210)      &
                        ,Ymin(mstr,213),Ymin(mstr,214),Ymin(mstr,193),Ymin(mstr,194)      &
                        ,Ymin(mstr,201),Ymin(mstr,202)
         
         ! ErgebM.txt
         write(45,'(a7,7x,I5,2x,F8.3,2x,i5)')cmin,mstr,Stakm(mstr,iior),STRID(mstr)
         write(45,4103)Ymin(mstr,1),Ymin(mstr,1),Ymin(mstr,1)                            &
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
                       ,Ymin(mstr,203),Ymin(mstr,203),Ymin(mstr,203)                     &
                       ,Ymin(mstr,204),Ymin(mstr,204),Ymin(mstr,204)                     &
                       ,Ymin(mstr,195),Ymin(mstr,195),Ymin(mstr,195)                     &
                       ,Ymin(mstr,196),Ymin(mstr,196),Ymin(mstr,196)                     &
                       ,Ymin(mstr,205),Ymin(mstr,205),Ymin(mstr,205)                     &
                       ,Ymin(mstr,206),Ymin(mstr,206),Ymin(mstr,206)                     &
                       ,Ymin(mstr,207),Ymin(mstr,207),Ymin(mstr,207)                     &
                       ,Ymin(mstr,208),Ymin(mstr,208),Ymin(mstr,208)
         write(45,4104)Ymin(mstr,197),Ymin(mstr,197),Ymin(mstr,197)                      &
                       ,Ymin(mstr,198),Ymin(mstr,198),Ymin(mstr,198)                     &
                       ,Ymin(mstr,211),Ymin(mstr,211),Ymin(mstr,211)                     &
                       ,Ymin(mstr,212),Ymin(mstr,212),Ymin(mstr,212)                     &
                       ,Ymin(mstr,199),Ymin(mstr,199),Ymin(mstr,199)                     &
                       ,Ymin(mstr,200),Ymin(mstr,200),Ymin(mstr,200)                     &
                       ,Ymin(mstr,209),Ymin(mstr,209),Ymin(mstr,209)                     &
                       ,Ymin(mstr,210),Ymin(mstr,210),Ymin(mstr,210)                     &
                       ,Ymin(mstr,213),Ymin(mstr,213),Ymin(mstr,213)                     &
                       ,Ymin(mstr,214),Ymin(mstr,214),Ymin(mstr,214)                     &
                       ,Ymin(mstr,193),Ymin(mstr,193),Ymin(mstr,193)                     &
                       ,Ymin(mstr,194),Ymin(mstr,194),Ymin(mstr,194)                     &
                       ,Ymin(mstr,201),Ymin(mstr,201),Ymin(mstr,201)                     &
                       ,Ymin(mstr,202),Ymin(mstr,202),Ymin(mstr,202)
         
         write(45,4001)Ymin(mstr,23)
         
         write(45,4002)Ymin(mstr,24)
         
         write(45,4365)Ymin(mstr,25),Ymin(mstr,26),Ymin(mstr,27)                         &
                       ,Ymin(mstr,28),Ymin(mstr,29),Ymin(mstr,30)                        &
                       ,Ymin(mstr,31),Ymin(mstr,32),Ymin(mstr,33)                        &
                       ,Ymin(mstr,34),Ymin(mstr,35),Ymin(mstr,36)                        &
                       ,Ymin(mstr,37),Ymin(mstr,38),Ymin(mstr,39)                        &
                       ,Ymin(mstr,40),Ymin(mstr,41)
         
         write(45,4004)Ymin(mstr,42),Ymin(mstr,43)
         
         write(45,4010)Ymin(mstr,44),Ymin(mstr,45),Ymin(mstr,46)                         &
                       ,Ymin(mstr,47),Ymin(mstr,48),Ymin(mstr,49)                        &
                       ,Ymin(mstr,50),Ymin(mstr,51),Ymin(mstr,52)                        &
                       ,Ymin(mstr,53),Ymin(mstr,54),Ymin(mstr,55)                        &
                       ,Ymin(mstr,56),Ymin(mstr,57),Ymin(mstr,58)                        &
                       ,Ymin(mstr,59),Ymin(mstr,60),Ymin(mstr,61)                        &
                       ,Ymin(mstr,62),Ymin(mstr,63),Ymin(mstr,64)                        &
                       ,Ymin(mstr,65),Ymin(mstr,66),Ymin(mstr,67)                        &
                       ,Ymin(mstr,68),Ymin(mstr,69),Ymin(mstr,70)                        &
                       ,Ymin(mstr,187),Ymin(mstr,188),Ymin(mstr,189)
         
         write(45,4265)Ymin(mstr,71),Ymin(mstr,72),Ymin(mstr,73),Ymin(mstr,74)
         
         write(45,4111)Ymin(mstr,75),Ymin(mstr,76),Ymin(mstr,77)                         &
                       ,Ymin(mstr,78),Ymin(mstr,79),Ymin(mstr,80)                        &
                       ,Ymin(mstr,81),Ymin(mstr,82),Ymin(mstr,83)                        &
                       ,(Ymin(mstr,84)*24.)
         
         write(45,4215)Ymin(mstr,85),Ymin(mstr,20),Ymin(mstr,86)                         &
                       ,Ymin(mstr,87),Ymin(mstr,88),Ymin(mstr,89)                        &
                       ,Ymin(mstr,90),Ymin(mstr,91),Ymin(mstr,92)
         
         write(45,4003)Ymin(mstr,93),Ymin(mstr,94),Ymin(mstr,95)                         &
                       ,Ymin(mstr,96),Ymin(mstr,174),Ymin(mstr,97),Ymin(mstr,175)        &
                       ,Ymin(mstr,177),Ymin(mstr,98),Ymin(mstr,99),Ymin(mstr,100)
         
         write(45,4020)Ymin(mstr,179),Ymin(mstr,185)
         
         write(45,4017)Ymin(mstr,101),Ymin(mstr,102),Ymin(mstr,103)                      &
                       ,Ymin(mstr,104),Ymin(mstr,105),Ymin(mstr,181)                     &
                       ,Ymin(mstr,107),Ymin(mstr,108),Ymin(mstr,109)                     &
                       ,Ymin(mstr,110),Ymin(mstr,111),Ymin(mstr,112)                     &
                       ,Ymin(mstr,113),Ymin(mstr,114)
         
         write(45,4018)Ymin(mstr,22)
         
         write(45,4103)Ymin(mstr,115),Ymin(mstr,115),Ymin(mstr,115)                      &
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
                       ,Ymin(mstr,203),Ymin(mstr,203),Ymin(mstr,203)                     &
                       ,Ymin(mstr,204),Ymin(mstr,204),Ymin(mstr,204)                     &
                       ,Ymin(mstr,195),Ymin(mstr,195),Ymin(mstr,195)                     &
                       ,Ymin(mstr,196),Ymin(mstr,196),Ymin(mstr,196)                     &
                       ,Ymin(mstr,205),Ymin(mstr,205),Ymin(mstr,205)                     &
                       ,Ymin(mstr,206),Ymin(mstr,206),Ymin(mstr,206)                     &
                       ,Ymin(mstr,207),Ymin(mstr,207),Ymin(mstr,207)                     &
                       ,Ymin(mstr,208),Ymin(mstr,208),Ymin(mstr,208)
         
         write(45,4104)Ymin(mstr,197),Ymin(mstr,197),Ymin(mstr,197)                      &
                       ,Ymin(mstr,198),Ymin(mstr,198),Ymin(mstr,198)                     &
                       ,Ymin(mstr,211),Ymin(mstr,211),Ymin(mstr,211)                     &
                       ,Ymin(mstr,212),Ymin(mstr,212),Ymin(mstr,212)                     &
                       ,Ymin(mstr,199),Ymin(mstr,199),Ymin(mstr,199)                     &
                       ,Ymin(mstr,200),Ymin(mstr,200),Ymin(mstr,200)                     &
                       ,Ymin(mstr,209),Ymin(mstr,209),Ymin(mstr,209)                     &
                       ,Ymin(mstr,210),Ymin(mstr,210),Ymin(mstr,210)                     &
                       ,Ymin(mstr,213),Ymin(mstr,213),Ymin(mstr,213)                     &
                       ,Ymin(mstr,214),Ymin(mstr,214),Ymin(mstr,214)                     &
                       ,Ymin(mstr,193),Ymin(mstr,193),Ymin(mstr,193)                     &
                       ,Ymin(mstr,194),Ymin(mstr,194),Ymin(mstr,194)                     &
                       ,Ymin(mstr,201),Ymin(mstr,201),Ymin(mstr,201)                     &
                       ,Ymin(mstr,202),Ymin(mstr,202),Ymin(mstr,202)
         
         write(45,4010)Ymin(mstr,134),Ymin(mstr,135),Ymin(mstr,136)                      &
                       ,Ymin(mstr,137),Ymin(mstr,138),Ymin(mstr,139)                     &
                       ,Ymin(mstr,140),Ymin(mstr,141),Ymin(mstr,142)                     &
                       ,Ymin(mstr,143),Ymin(mstr,144),Ymin(mstr,145)                     &
                       ,Ymin(mstr,146),Ymin(mstr,147),Ymin(mstr,148)                     &
                       ,Ymin(mstr,149),Ymin(mstr,150),Ymin(mstr,151)                     &
                       ,Ymin(mstr,152),Ymin(mstr,153),Ymin(mstr,154)                     &
                       ,Ymin(mstr,155),Ymin(mstr,156),Ymin(mstr,157)                     &
                       ,Ymin(mstr,158),Ymin(mstr,159),Ymin(mstr,160)
         
         write(45,4011)Ymin(mstr,106),Ymin(mstr,176),Ymin(mstr,178), Ymin(mstr,173)
         
         write(45,4020)Ymin(mstr,180),Ymin(mstr,186)
         
         write(45,4021)Ymin(mstr,182)
      enddo
      
   enddo
   
   
   do azStr = 1,azStrs
      mstr = mstra(azStr)
      
      if (Ymax(mstr,20) > 0.0)Ymax(mstr,20) = Ymax(mstr,20)*1000.
      
      do iior = 1,mStas(mstr)
         
         write(155,'(a7,14x,I5,2x,F8.3,2x,i5)')cmax,mstr,Stakm(mstr,iior),STRID(mstr)
         
         write(155,5105)Ymax(mstr,1),Ymax(mstr,2),Ymax(mstr,3)                            &
                        ,Ymax(mstr,4),Ymax(mstr,5),Ymax(mstr,6)                           &
                        ,Ymax(mstr,7),Ymax(mstr,8),Ymax(mstr,9)                           &
                        ,Ymax(mstr,10),Ymax(mstr,11),Ymax(mstr,12)                        &
                        ,Ymax(mstr,13),Ymax(mstr,14),Ymax(mstr,15)                        &
                        ,Ymax(mstr,16),Ymax(mstr,17),Ymax(mstr,18)                        &
                        ,Ymax(mstr,20),Ymax(mstr,19),Ymax(mstr,21),Ymax(mstr,22)          &
                        ,Ymax(mstr,169)
         
         write(155,5207)Ymax(mstr,203),Ymax(mstr,204),Ymax(mstr,195),Ymax(mstr,196)       &
                        ,Ymax(mstr,205),Ymax(mstr,206),Ymax(mstr,207),Ymax(mstr,208)      &
                        ,Ymax(mstr,197),Ymax(mstr,198),Ymax(mstr,211),Ymax(mstr,212)      &
                        ,Ymax(mstr,199),Ymax(mstr,200),Ymax(mstr,209),Ymax(mstr,210)      &
                        ,Ymax(mstr,213),Ymax(mstr,214),Ymax(mstr,193),Ymax(mstr,194)      &
                        ,Ymax(mstr,201),Ymax(mstr,202)
         write(155,5205)(Ymax(mstr,104)*hcUmt),(Ymax(mstr,93)*4.33*hcUmt),(Ymax(mstr,108)*hcUmt) & 
                        ,(Ymax(mstr,102)*hcUmt),Ymax(mstr,187),Ymax(mstr,188)             &
                        ,Ymax(mstr,189),(Ymax(mstr,112)*hcUmt),(Ymax(mstr,105)*hcUmt)     &
                        ,(Ymax(mstr,94)*hcUmt)
         
         write(155,5115)Ymax(mstr,115),Ymax(mstr,116),Ymax(mstr,117)                      &
                        ,Ymax(mstr,118),Ymax(mstr,119),Ymax(mstr,120)                     &
                        ,Ymax(mstr,121),Ymax(mstr,122),Ymax(mstr,123)                     &
                        ,Ymax(mstr,124),Ymax(mstr,125),Ymax(mstr,126)                     &
                        ,Ymax(mstr,127),Ymax(mstr,128),Ymax(mstr,129)                     &
                        ,Ymax(mstr,130),Ymax(mstr,131),Ymax(mstr,132)                     &
                        ,Ymax(mstr,133),Ymax(mstr,171),Ymax(mstr,168)                     &
                        ,Ymax(mstr,170)
         
         write(155,5207)Ymax(mstr,203),Ymax(mstr,204),Ymax(mstr,195),Ymax(mstr,196)       &
                        ,Ymax(mstr,205),Ymax(mstr,206),Ymax(mstr,207),Ymax(mstr,208)      &
                        ,Ymax(mstr,197),Ymax(mstr,198),Ymax(mstr,211),Ymax(mstr,212)      &
                        ,Ymax(mstr,199),Ymax(mstr,200),Ymax(mstr,209),Ymax(mstr,210)      &
                        ,Ymax(mstr,213),Ymax(mstr,214),Ymax(mstr,193),Ymax(mstr,194)      &
                        ,Ymax(mstr,201),Ymax(mstr,202)
         
         write(45,'(a7,7x,I5,2x,F8.3,2x,i5)')cmax,mstr,Stakm(mstr,iior),STRID(mstr)
         write(45,4103)Ymax(mstr,1),Ymax(mstr,1),Ymax(mstr,1)                            &
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
                       ,Ymax(mstr,203),Ymax(mstr,203),Ymax(mstr,203)                     &
                       ,Ymax(mstr,204),Ymax(mstr,204),Ymax(mstr,204)                     &
                       ,Ymax(mstr,195),Ymax(mstr,195),Ymax(mstr,195)                     &
                       ,Ymax(mstr,196),Ymax(mstr,196),Ymax(mstr,196)                     &
                       ,Ymax(mstr,205),Ymax(mstr,205),Ymax(mstr,205)                     &
                       ,Ymax(mstr,206),Ymax(mstr,206),Ymax(mstr,206)                     &
                       ,Ymax(mstr,207),Ymax(mstr,207),Ymax(mstr,207)                     &
                       ,Ymax(mstr,208),Ymax(mstr,208),Ymax(mstr,208)
         write(45,4104)Ymax(mstr,197),Ymax(mstr,197),Ymax(mstr,197)                      &
                       ,Ymax(mstr,198),Ymax(mstr,198),Ymax(mstr,198)                     &
                       ,Ymax(mstr,211),Ymax(mstr,211),Ymax(mstr,211)                     &
                       ,Ymax(mstr,212),Ymax(mstr,212),Ymax(mstr,212)                     &
                       ,Ymax(mstr,199),Ymax(mstr,199),Ymax(mstr,199)                     &
                       ,Ymax(mstr,200),Ymax(mstr,200),Ymax(mstr,200)                     &
                       ,Ymax(mstr,209),Ymax(mstr,209),Ymax(mstr,209)                     &
                       ,Ymax(mstr,210),Ymax(mstr,210),Ymax(mstr,210)                     &
                       ,Ymax(mstr,213),Ymax(mstr,213),Ymax(mstr,213)                     &
                       ,Ymax(mstr,214),Ymax(mstr,214),Ymax(mstr,214)                     &
                       ,Ymax(mstr,193),Ymax(mstr,193),Ymax(mstr,193)                     &
                       ,Ymax(mstr,194),Ymax(mstr,194),Ymax(mstr,194)                     &
                       ,Ymax(mstr,201),Ymax(mstr,201),Ymax(mstr,201)                     &
                       ,Ymax(mstr,202),Ymax(mstr,202),Ymax(mstr,202)
         
         write(45,4001)Ymax(mstr,23)
         
         write(45,4002)Ymax(mstr,24)
         
         write(45,4365)Ymax(mstr,25),Ymax(mstr,26),Ymax(mstr,27)           &
                       ,Ymax(mstr,28),Ymax(mstr,29),Ymax(mstr,30)                        &
                       ,Ymax(mstr,31),Ymax(mstr,32),Ymax(mstr,33)                        &
                       ,Ymax(mstr,34),Ymax(mstr,35),Ymax(mstr,36)                        &
                       ,Ymax(mstr,37),Ymax(mstr,38),Ymax(mstr,39)                        &
                       ,Ymax(mstr,40),Ymax(mstr,41)
         
         write(45,4004)Ymax(mstr,42),Ymax(mstr,43)
         
         write(45,4010)Ymax(mstr,44),Ymax(mstr,45),Ymax(mstr,46)           &
                       ,Ymax(mstr,47),Ymax(mstr,48),Ymax(mstr,49)                        &
                       ,Ymax(mstr,50),Ymax(mstr,51),Ymax(mstr,52)                        &
                       ,Ymax(mstr,53),Ymax(mstr,54),Ymax(mstr,55)                        &
                       ,Ymax(mstr,56),Ymax(mstr,57),Ymax(mstr,58)                        &
                       ,Ymax(mstr,59),Ymax(mstr,60),Ymax(mstr,61)                        &
                       ,Ymax(mstr,62),Ymax(mstr,63),Ymax(mstr,64)                        &
                       ,Ymax(mstr,65),Ymax(mstr,66),Ymax(mstr,67)                        &
                       ,Ymax(mstr,68),Ymax(mstr,69),Ymax(mstr,70)                        &
                       ,Ymax(mstr,187),Ymax(mstr,188),Ymax(mstr,189)
         !
         write(45,4265)Ymax(mstr,71),Ymax(mstr,72),Ymax(mstr,73)           &
                       ,Ymax(mstr,74)
         !
         write(45,4111)Ymax(mstr,75),Ymax(mstr,76),Ymax(mstr,77)           &
                       ,Ymax(mstr,78),Ymax(mstr,79),Ymax(mstr,80)                        &
                       ,Ymax(mstr,81),Ymax(mstr,82),Ymax(mstr,83)                        &
                       ,(Ymax(mstr,84)*24.)
         !
         write(45,4215)Ymax(mstr,85),Ymax(mstr,20),Ymax(mstr,86)           &
                       ,Ymax(mstr,87),Ymax(mstr,88),Ymax(mstr,89)                        &
                       ,Ymax(mstr,90),Ymax(mstr,91),Ymax(mstr,92)
         !
         write(45,4003)Ymax(mstr,93),Ymax(mstr,94),Ymax(mstr,95)           &
                       ,Ymax(mstr,96),Ymax(mstr,174),Ymax(mstr,97),Ymax(mstr,175)        &
                       ,Ymax(mstr,177),Ymax(mstr,98),Ymax(mstr,99),Ymax(mstr,100)
         !
         write(45,4020)Ymax(mstr,179),Ymax(mstr,185)
         !
         write(45,4017)Ymax(mstr,101),Ymax(mstr,102),Ymax(mstr,103)        &
                       ,Ymax(mstr,104),Ymax(mstr,105),Ymax(mstr,181)                     &
                       ,Ymax(mstr,107),Ymax(mstr,108),Ymax(mstr,109)                     &
                       ,Ymax(mstr,110),Ymax(mstr,111),Ymax(mstr,112)                     &
                       ,Ymax(mstr,113),Ymax(mstr,114)
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
                       ,Ymax(mstr,137),Ymax(mstr,138),Ymax(mstr,139)                     &
                       ,Ymax(mstr,140),Ymax(mstr,141),Ymax(mstr,142)                     &
                       ,Ymax(mstr,143),Ymax(mstr,144),Ymax(mstr,145)                     &
                       ,Ymax(mstr,146),Ymax(mstr,147),Ymax(mstr,148)                     &
                       ,Ymax(mstr,149),Ymax(mstr,150),Ymax(mstr,151)                     &
                       ,Ymax(mstr,152),Ymax(mstr,153),Ymax(mstr,154)                     &
                       ,Ymax(mstr,155),Ymax(mstr,156),Ymax(mstr,157)                     &
                       ,Ymax(mstr,158),Ymax(mstr,159),Ymax(mstr,160)
         !
         write(45,4011)Ymax(mstr,106),Ymax(mstr,176),Ymax(mstr,178)        &
                       ,Ymax(mstr,173)
         !
         write(45,4020)Ymax(mstr,180),Ymax(mstr,186)
         write(45,4021)Ymax(mstr,182)
         !
      enddo
   enddo
   
   ! --------------------------------------------------------------------------
   ! end of program
   ! --------------------------------------------------------------------------
   ! --- delete temporary files ---
   pfadstring = trim(adjustl(cpfad)) // 'sysgenou'
   open(unit = 11, file = pfadstring)
   close(11, status = "delete")
   
   pfadstring = trim(adjustl(cpfad)) // 'km_sys.dat'
   open(unit = 391, file = pfadstring)
   close(391, status = "delete")
   
   pfadstring = trim(adjustl(cpfad)) // 'wehro2.txt'
   open(unit = 301, file = pfadstring)
   close(301, status = "delete")
   
   ! --- close output files --- 
   close(45)                     ! ErgebM.txt
   close(155)                    ! ErgebT.txt
   close(156)
   close(157)
   close(158)
   
   write(*,*) 'Success.'
   write(*,*) 'End of Simulation'
   
end program qsim