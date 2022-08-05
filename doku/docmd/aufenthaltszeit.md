Aufenthaltszeit     {#lnk_aufenthaltszeit}
===============

Alterung des Wasser zur Aufenthaltsbestimmung

Altersberechnung nach Shen & Wang 2007 doi:10.1016/j.ecss.2007.05.017

!   planktonic_variable_name(71)= "    Tracer"  - c bei Shen&Wang 2007
!   planktonic_variable_name(73)= "    age_decay"
!   planktonic_variable_name(74)= "    age_arith"  - alfa bei Shen&Wang 2007 [in Tagen]
!   planktonic_variable_name(75)= "    age_growth"

Aufenthaltszeit [Tagen] = age_arith / Tracer

Wenn die Datei
<a href="./exp/alter.txt" target="_blank">alter.txt</a>
im Modellverzeichnis vorhanden ist, wird ausschließlich eine 
Aufenthaltszeitermittlung durchgeführt.

Der restliche Stoffumsatz ist dann ausgeschaltet.

Textquelle: aufenthaltszeit.md; Codesource: alter.f95; zurück: \ref index