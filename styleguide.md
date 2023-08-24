# QSim Styleguide 

Seit dem Erscheinen der ersten QSim-Version sind mittlerweile mehrere Jahrzehnte vergangen. In der Zwischenzeit haben sich Soft- und Hardware weiterentwickelt und eine Vielzahl neuer Möglichkeiten geschaffen. Mit neuen Fortran-Standards kamen neue und bessere Möglichkeiten, Anweisungen in Code zu formulieren. Zudem wird QSim seit einigen Jahren nun nicht mehr von einer Person allein entwickelt, sondern ist mittlerweile ein Team-Projekt.

All diese Entwicklungen wirken sich auch auf den Programmcode aus. Um den Code einheitlich und damit lesbarer und professioneller zu gestalten, wird hier ein einheitlicher Schreibstil für den QSim Programmcode vereinbart.

# Dateien

## Codeformat

QSim wird im free form Format geschrieben. Damit entfallen alle Beschränkungen des veralteten fixed form Formats.

## Ordnung

Jede Codeeinheit (Module, Subroutinen und Funktionen) sollte in einer eigenen Datei gespeichert werden. Thematisch zueinander gehörende Subroutinen und Funktionen können auch in entsprechenden Modulen gesammelt werden.

## Dateinamen

Der Dateiname sollte dem Namen der Subroutine oder des Moduls entsprechen und ausschließlich in Kleinbuchstaben geschrieben werden. Fortran-Dateien enden mit der Bezeichnung `.f90` oder `.f95`

Dateien, die ein Modul definieren, beginnen mit dem Prefix `module_`

``` fortran
! module
module_foo.f95

! subroutine
bar.f95
```

## Header

Alle Dateien müssen in UTF-8 kodiert sein und beginnen mit folgendem Header:

``` fortran
!---------------------------------------------------------------------------- !
!   QSim - Programm zur Simulation der Wasserqualität                         !
!                                                                             !
!   Copyright (C) 2022                                                        !
!       Bundesanstalt für Gewässerkunde                                       !
!       Koblenz (Deutschland)                                                 !
!       http://www.bafg.de                                                    !
!                                                                             !
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen   !
!   der GNU General Public License, Version 3, wie von der Free Software      !
!   Foundation veröffentlicht, weitergeben und/oder modifizieren.             !
!                                                                             !
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es    !
!   Ihnen von Nutzen sein wird, aber OHNE IRGENDEINE GARANTIE, sogar ohne die !
!   implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN       !
!   BESTIMMTEN ZWECK.                                                         !
!                                                                             !
!   Details finden Sie in der GNU General Public License.                     !
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit      !
!   diesem Programm erhalten haben.                                           !
!   Falls nicht, siehe http://www.gnu.org/licenses/.                          !
!                                                                             !
!   Programmiert von:                                                         !
!   1979 bis 2018 Volker Kirchesch                                            !
!   seit 2011 Jens Wyrwa, Wyrwa@bafg.de                                       !
! --------------------------------------------------------------------------- !
```

Die Person, die eine Datei überarbeitet soll zudem ihren Namen und E-Mailadresse am Ende des Headers ergänzen. 

## Gliederung

Je länger der Code wird, desto wichtiger wird es, ihn übersichtlich zu strukturieren. Ähnlich wie in einem Text Abschnitte und Unterabschnitte definiert werden, sollte auch Programmcode in Einheiten gegliedert werden.

Für längere Abschnitte bietet es sich daher an mit Über- und Unterüberschriften zu arbeiten.

Für Überschriften werden die folgenden Kommentarzeilen verwendet, wobei die `-` Zeichen bis Zeilenende wiederholt werden (132 characters Länge).

``` fortran
! ------------------------------------------------------------------
! This is a heading
! ------------------------------------------------------------------
```

Für Unterüberschriften wird das folgende Format verwendet

``` fortran
! --- small heading ---
```

Feinere Untergliederungen werden mit einfachen Kommentaren vorgenommen.

Das Einfügen von Leerzeilen zwischen logischen Einheiten erhöht zudem die Lesbarkeit des Codes. Sie sollten dementsprechend häufig verwendet werden.

# Schreibweisen

## Variablennamen

In früheren Fortran-Versionen waren Variablennamen auf eine Länge von sechs Zeichen beschränkt. Das führt dazu, dass viele Variablen in QSim kryptisch benannt sind und sich deren Bedeutung nicht direkt erschließt.

Moderne Fortran-Standards haben diese Beschränkung nicht mehr und Variablennamen können (fast) beliebig lang werden. Variablen sollen mit kurzen, jedoch aussagekräftigen Namen belegt werden und Abkürzungen nur verwendet werden, wenn sie eindeutig sind.

Grundsätzlich gilt: Variablennamen sollten Substantive sein, während Funktionen und Subroutinen mit Verben benannt werden sollten.

Namen sollten ausschließlich mit Kleinbuchstaben und in Snake-Case geschrieben werden, d.h. einzelne Teile des Names werden durch einen Unterstrich getrennt. Doppelte Unterstriche dürfen nicht verwendet werden und ein Name muss mit einem Buchstaben beginnen und enden.

``` fortran
! Gut
my_variable = 2

! Schlecht
_my_variable_ = 2
my__variable = 2
myVARIABLE = 2
```

Umbenennungen von alten, 6-stelligen Variablennamen in neue, selbstbeschreibende Variablen im Zuge von Überarbeitungen können dazu führen, dass bei Textsuchen im Code nicht alle Stellen gefunden werden, an denen eine Variable (z.B. Algenbiomasse) verwendet wird. Um dies zu vermeiden und eine gute Nachvollziehbarkeit zu gewährleisten, sollen überarbeitete Programmteile im Falle solcher Umbenennungen einen Kommentarblock enthalten, in dem alle Paare von alten und neuen Namen jeweils in einer Zeile aufgelistet sind. Es wird empfohlen die alten Namen bei der Variablendeklaration als Kommentar anzufügen.

## Logische Operatoren

Logische Operatoren werden mit Kleinbuchstaben geschrieben.

``` fortran
.true.
.false.
.and.
.or. 
.not.
```

## Vergleichsoperatoren

Für Vergleichsoperatoren sollen symbolische Repräsentationen (`<`, `<=`, `>`, `>=`, `==` und `/=`) verwendet werden und diese von Leerzeichen umschlossen sein.

``` fortran
! Gut 
if (a /= 10) print *, a

! Schlecht 
if (a.ne.10) print *, a
```

## Schlüsselwörter

Sämtliche Schlüsselwörter (`if`, `else`, `end`, `program`, `return` etc.) werden mit Kleinbuchstaben geschrieben.

## end-Anweisungen

Zum Schließen von if-Bedingungen und Schleifen werden die Schlüsselwörter `endif` und `enddo` verwendet und ohne Leerzeichen geschrieben.

Bei allen sonstigen Blöcken ist das `end` mit einem Leerzeichen abzutrennen:

-   `end program`
-   `end module`
-   `end subroutine`
-   `end select` etc.

Die `end`-Anweisung einer Programeinheit sollte zusätzlich auch den Namen den jeweiligen Namen nennen.

``` fortran
subroutine foo
   ...
end subroutine foo
```

## Infix-Operatoren

Infix-Operatoren ist je ein Leerzeichen vor- und nachgestellt.

``` fortran
! Gut 
area = length * width

! Schlecht 
area=length*width
```

Eine Ausnahme bildet der Potenzoperator (`**`), der ohne Leerzeichen geschrieben werden sollte.

``` fortran
! Gut 
a = b**2

! Schlecht 
a = b ** 2
```

### Klammern

Bei Aufrufen von Funktionen und Subroutinen hingegen sollte keine Leerzeichen vor oder nach der Klammer stehen. Gleiches gilt für Indizes bei Arrays und bei Klammern in Berechnungenen.

``` fortran
! Gut
call example(a, b, c)
array(1,3)
new = (a + b) / c

! Schlecht
call example (a,b)
call example( a,b )
array ( 1,3 )
new = ( a+b )/c
```

## Einrückungen

Code in Schleifen oder Verzweigungen wird um **drei** Stellen eingerückt. Die Einrückung wird mit Leerzeichen erzeugt, Tabs dürfen nicht verwendet werden.

Elemente, die einen Block schließen, stehen wieder auf derselben Höhe wie das eröffnende Element.

``` fortran
if (a > 0) then
   b = 2
   c = 3
else 
   b = 0
   c = 0
endif

if (a > 0) then
   do i = 1, 10
      print *, i**3
   enddo
endif
```

## Zusätzliche Leerzeichen

An manchen Stellen kann es sinnvoll sein, zusätzliche Leerzeichen einzufügen, um Code klarer zu stukturieren. Zum Beispiel können `=`-Zeichen, oder Dummy-Variablen in Aufrufen bündig ausgerichtet werden.

``` fortran
length = 1
height = 2
width  = 3

call my_sub(long_variable1,   long_variable2,   long_variable3)
call my_sub(longer_variable1, longer_variable2, longer_variable3)
```

## Lange Zeilen

Die Länge einer Zeile Code wird auf **maximal 132 Zeichen** beschränkt.

Längere Zeilen müssen vorher umgebrochen werden. Die `&`--Zeichen werden dabei mithilfe von Leerzeichen bündig ausgerichtet. Die nächste Zeile beginnt **ohne** `&`-Zeichen. Alle Fortsetzungzeilen werden mit Leerzeichen auf dieselbe Höhe eingerückt. Es ist sinnvoll, vor dem `&`-Zeichen zusätzliche Leerzeichen einzufügen, um die Zeilenumbrüche sichtbarer zu machen.

Bei Aufrufen von Subroutinen mit sehr vielen Dummyvariablen ist es hilfreich, diese sinnvoll auf mehreren Zeilen zu gruppieren. Der Zeilenumbruch erfolgt **nach** einem Komma.

``` fortran
! Gut
call my_subroutine(rate1, rate2, rate3,          &
                   parameter1, parameter2,       &
                   output1, output2, output3)

! Schlecht 
call my_subroutine(rate1,rate2,rate3,paramter1,parameter2,output1&
,output2,output3)
```

# Funktionen und Subroutinen

## Variablendeklaration

Jede Programmeinheit muss mit dem Schlüsselwort `implicit none` beginnen. Danach müssen alle Variablen explizit definiert werden.

## Aufruf von Subroutinen

Der Befehl `call` wird mit **genau einem** Leerzeichen vom Namen der Subroutine getrennt. Das gewährleistet, dass alle Aufrufe einer Subroutine über die Suchfunktion gefunden werden können.

``` fortran
call fancy_subroutine()
```

## Dummy-Argumente

Dummy-Argumente sollen mit einem Leerzeichen nach dem Komma getrennt werden.

Alle Dummy-Argumente sollen mit den Schlüsselwörtern `intent(in)`, `intent(out)` und `intent(inout)` deklariert werden. Außerdem sollte ein Kommentar die Bedeutung jedes Dummy-Arguments und seine Einheit erklären.

``` fortran
subroutine get_hypotenuse(a, b, c)
   implicit none
   
   real, intent(in)  :: a, b  ! length of catheti [m]
   real, intent(out) :: c     ! length of hypotenuse [m] 
   
   c = sqrt(a**2 + b**2)
end subroutine get_hypotenuse
```

# goto

Konstruktionen mit `goto` sind im gesamten Code zu vermeiden.

Verzweigungen sollten stattdessen mit `if`,`else` oder `select case` realisiert werden.

Schleifen können mit `cycle` und `exit` gesteuert werden.

# Code schreiben

## Kommentare

Eines der wichtigsten Hilfsmittel, um Code für andere Entwickler\*innen (oder auch das eigene zukünftige Ich) verständlich zu machen, sind aussagekräftige Kommentare. Sie sollten dementsprechend häufig eingesetzt werden. Ein guter Kommentar erklärt insbesondere nicht nur das, was getan wird, sondern auch warum etwas getan wird. 

Kommentare beginnen mit einem einzelnen Ausrufzeichen `!` gefolgt von einem Leerzeichen. Das Ausrufzeichen sollte dabei bündig mit der aktuellen Einrückungstiefe stehen. Lange Kommentare können auf mehrere Zeilen aufgeteilt werden.

Kommentare sollten im Allgemeinen direkt oberhalb der betreffenden Codezeile stehen. In Einzelfällen ist auch ein Kommentar in derselben Zeile sinnvoll. Kommentare sollten niemals unterhalb der betreffenden Codezeile stehen.

``` fortran
if (a == 10) then
   ! This is a comment
   print *, a
endif
```

## Fehlermeldungen

Fehlermeldungen sollen den aufgetretenden Fehler möglichst eindeutig beschreiben. Sie sollten Informationen darüber enthalten, in welchem Programmteil der Fehler aufgetreten ist und welche Variablen ihn verursacht haben.

Die Beschreibung des Fehlers sollte verständlich sein und ohne kryptische Abkürzungen auskommen.

## Offene Fragen

Code, der weiterer Bearbeitung bedarf, sollte als solcher mit einem Kommentar gekennzeichnet werden. Als Kennzeichnung dient `TODO` gefolgt vom eigenen Namen oder Namenskürzel. Auf diese Weise können offene Stellen global mit einer Suchfunktion gefunden werden. In den folgenden Zeilen sollte eine kurze Problembeschreibung erfolgen. Parallel sollte ein Ticket in [GitLab](https://gitlab.lan.bafg.de/groups/qsim/-/issues) eröffnet werden und dort eine ausführlichere Beschreibung gegeben werden.

``` fortran
! TODO (ms)
! avoid division by 0
a = 1. / b
```

## Natürliche Sprache

QSim wurde bisher auf Deutsch geschrieben. Alle zukünftigen Änderungen und Erweiterungen sollen auf Englisch erfolgen. Das betrifft Kommentare sowie die Benennung von Variablen und Pogrammeinheiten.

Langfristig soll so der gesamte Code auf Englisch umgestellt werden, wenngleich es mittelfristig eine Mischung aus Deutsch und Englisch bedeutet.
