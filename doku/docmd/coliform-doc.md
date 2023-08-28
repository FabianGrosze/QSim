Coliform bactera/Germs (Hygiene) {#lnk_coliform}
===================================

\warning The hygiene module is currently switched off. Thus, coliform bacteria 
currently can not be calculated. The text is from an earlier version of the 
documentation and the content is not checked with the code.

Faecal coliform bacteria (usually Escherichia coli) do not multiply in the 
water, but die quickly in the environment. They thus indicate a relatively fresh 
contamination with faeces. Total coliform bacteria, on the other hand, 
survive longer and can multiply in organically rich materials or are even 
exclusively indigenous to the environment. Accordingly, 
this group shows a possible wash-in of older faeces (sewage sludge, 
sludge, liquid manure) and thus also indicates general eutrophication tendencies. 
QSim only looks at the development of faecal coliform bacteria in the study area. 

Quantifying the input pathways of faecal coliform bacteria into waterways has 
proven particularly difficult. 
The turnover processes of the coli bacteria within the water body are essentially 
determined by the strongly decimating effect of sunlight (UV radiation), 
whereas growth does not take place.

In QSim, a first approach was developed and implemented to describe the 
mortality rate of coli bacteria as a function of UV radiation. This approach   
still needs to be revised.

\image html badestelle_rhein.png "Germs within the water can be dangeours for people swimming in the water."


Further information:

- \subpage lnk_coliform_prozesse : Explanation of the processes implemented 
   within the module

- \subpage lnk_coliform_vars : List of formula symbols and variables 

- \subpage lnk_coliform_umsetzung : Details on the code and its numeric 
   implementation


Text source: coliform-doc.md ; Code sources: coliform_huelle.f95 and coliform.f90 ; \n 
Go back: \ref lnk_weitere_stoffe
 