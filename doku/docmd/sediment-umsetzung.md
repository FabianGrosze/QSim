Sediment - Umsetzung {#lnk_sediment_umsetzung}
===============================

## Herkunft ##
    sedflux()
    Ein Programm zur Ermittlung der Stoff-Fluxe aus dem Sediment\n
    DiToro 2001; QUAL2K\n
    AUTOR :VOLKER KIRCHESCH \n
    STAND: 12.11.2008\n


## Schnittstellenbeschreibung ##

     call sedflux (\ref tiefe,\ref vmitt,\ref rau,\ref sedalg_mq,\ref hsedom,\ref hw2,\ref hbedgs,\ref hsedvvert
                  ,\ref hdkorn,\ref vo2,\ref vno3,\ref vnh4,\ref gelp,\ref tempw,\ref anze,\ref mstr           &\n
                  ,\ref hjno3,\ref hjnh4,\ref hjpo4,\ref hjo2,\ref hjn2,\ref sedalk,\ref sedalg,\ref sedalb
                  ,\ref sedss_mq,\ref knh4e,\ref kapn3e                                    &\n
                  ,\ref tflie,\ref ilbuhn,\ref itags,\ref monats,\ref uhrz,\ref vo2z,\ref vnh4z,\ref vno3z
                  ,\ref gelpz,\ref nkzs,\ref sorpcape,\ref klange                                &\n
                  ,\ref kdnh3e,\ref fpoc1e,\ref fpoc2e,\ref orgcsd_abb,\ref hcd,\ref jdoc1,\ref jdoc2
                  ,\ref q_nk,\ref q_pk,\ref q_ng,\ref q_pg,\ref q_nb                                  &\n
                  ,\ref q_pb,\ref pl0,\ref nl0,\ref si,\ref hsised,\ref hjsi,\ref aki,\ref agr,\ref abl
                  ,\ref chlaki,\ref chlagr,\ref chlabl,\ref hflun3,\ref ilang,\ref azstrs,\ref iwied,\ref ynmx1e,\ref stks1e     &\n
                  ,\ref obsb,\ref ocsb,  \ref kontroll , *jjj* ) wy  
\n\n

## IT-Realisierung ##
*Sedimenteigenschaften*

    WRITE(1, '(A)') ' &lt;ParamSetDef Id="QZ" Text="Sediment-Kenngrößen" Help="Sediment-Kenngrößen in den Gewässer-Abschnitten" Scope="Abschnitt" &gt;'\n
    WRITE(1, '(A)') ' &lt;Parameter Ident="POMsed" Text="Anteil org. Materials" Unit="%" Format="F6.2" Null="-1" Help="Anteil des organischen Materials im Sediment" Min="" Max="" Default="-1" /&gt; '\n
    WRITE(1, '(A)') ' &lt;Parameter Ident="BedGSed" Text="Bedeckungsgrad der Sohle mit Sediment (0-1)" Unit="-" Format="F5.2" Null="-1" Help="" Min="0" Max="1" Default="-1." /&gt; '\n
    WRITE(1, '(A)') ' &lt;Parameter Ident="VVERTZ" Text="volumenbezogene Eindringgeschwindigkeit ins Sediment" Unit="mm/h" Format="F9.4" Null="-1" Help="" Min="" Max="" Default="-1." /&gt; '\n
    WRITE(1, '(A)') ' &lt;Parameter Ident="kornd" Text="Korndurchmesser D50 Sediment (0-1), optional" Unit="m" Format="F5.2" Null="-1" Help="" Min="0" Max="1" Default="-1." /&gt; '\n
    WRITE(1, '(A)') ' &lt;Parameter Ident="burial" Text="Burial-geschwindigkeit ins Sediment, optional" Unit="mm/h" Format="F9.4" Null="-1" Help="" Min="" Max="" Default="-1." /&gt; '\n
\n\n

MQ-Tiefe =2 m, MQ-Geschwindigkeit =1 m/s\n
ansonsten Korndurchmesser und burialgeschwindigkeit in modellg.txt vorgeben


<code>
   !aPOM(mstr,mZ),ePOM(mstr,mZ),POMz(mstr,mZ),BedGSz(mstr,mz),Sedvvertz(mstr,mz)\n
     sedss_mq(1,1)=sedAlg_MQ(1,1)  *noch nicht verwendet ???*
</code>



\n\n

Textquelle: sediment-umsetzung.md; Codesources: SedFlux.f90, sedflux_huelle.f95 ;
zurück \ref lnk_sediment oder ... 
