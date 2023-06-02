Residence time     {#lnk_aufenthaltszeit}
===============

Aging of water to determine residence time

Age calculation according to Shen & Wang 2007 doi:10.1016/j.ecss.2007.05.017

!   planktonic_variable_name(71)= "    Tracer"  - c bei Shen&Wang 2007
!   planktonic_variable_name(73)= "    age_decay"
!   planktonic_variable_name(74)= "    age_arith"  - alfa bei Shen&Wang 2007 [in Tagen]
!   planktonic_variable_name(75)= "    age_growth"

Residence time [days] = age_arith / tracer

If the file
<a href="./exp/alter.txt" target="_blank">alter.txt</a>
exists within the model directory, exclusively the calculation of residence 
time is done.

The other turnover of matter is then switched off.

Text source: aufenthaltszeit.md; Code source: alter.f95; \n
Go back to: \ref lnk_tracer_aufenthalt or \ref index