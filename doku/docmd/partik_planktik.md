Partikuläre planktische Konzentrationen {#lnk_partik_planktik}
============

## Bilanzvariablen, die partikulär vorliegen aber als im Wasser gleichverteilte Konzentrationen gerechent werden ##
Teil von \ref lnk_var_planktisch

| Formelzeichen/ \n Variablen-Name | Lichtattenuation | sedimentation() | Aufwirbelung | Bilanzroutine | 
| ------ | --------| -------| --------| ------- | 
| aki | ack*vkigr*chla | SEDALk | - | algaeski() |
| agr | acg*(1-vkgr-antbl)*chla | SEDALg | - | algaesgr() |
| abl | acb*antbl*chla | SEDALb | - | algaesbl() |
| cp1 | dorgss -> ss | sedCP1 -> dorgss | - | orgc() |
| cp2 | dorgss -> ss | sedCP2 -> dorgss | - | orgc() |
| bac | dorgss -> ss | sedBAC -> dorgss | - | orgc() |
| (cref) in ocsb | dorgss -> ss | sedCrf -> dorgss | - | orgc() |
| zooind | as*Zooind*GRote/1000. | - | - | konsum() |
| vx0  | - | sednit | - | ncyc() |
| vx02 | - | sednt2 | - | ncyc() |
| ss | as*ss  | sedss  | qsim -> erosion() | schweb() |
| ssalg = SS+agr(ior)+aki(ior)+abl(ior)+(ZOOind(ior)*GROT/1000.) | - | - | - | - |

## Sedimentation ##

sedimentation() in Kapitel 17 der <a href="./pdf/kurzdoku13_1ber.pdf" target="_blank"> Kurz-Dokumentation QSim</a>

## Erosion ##

erosion() : \n
\f[ \Delta SS = M \cdot (\frac{\tau - \tau_{krit} }{ \tau_{krit} })^n \cdot \frac{\Delta t}{H} \f] \n
mit: \n
n=3.2  ; M=7.5e-4  ;  \f$ \tau_{krit}=1.25 \f$ \n

## Schwebstoffänderungen ##

| Schwebstoffänderungen in schweb() | SSt = SS(ior) |
| ------ | ------ |
| Sedimentation         |	  -sedss(ior) |
| part. org. Kohlenstoffe |	  +dorgSS(ior) |
| Zooplankton Ausscheidungen und Absterben |	  +zexki(ior)+zexgr(ior)+zexbl(ior)+abszo(ior) |
| abgestorbene Algen |	  +dkimor(ior)+dgrmor(ior)+dblmor(ior) |
| Muscheln |      -ssdr(ior)+drfaek(ior)+drfaeg(ior)+drfaeb(ior)+drfaes(ior) |

?


                                                            

