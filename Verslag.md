# Alphi
Naam:: Lissens  
Voornaam:: Tobiah  
Richting:: 2de bachelor Informatica  
year ::  2016-2017

## Inhoud

1. Inleiding
2. Syntax (BNF)
3. Semantiek
4. VoorbeeldProgramma's
  1. Demo_police
  2. Demo_line
  3. Demo_ultra
5. Implementatie
  1. Parsen
  2. Evalueren
6. Conclusie
  1. Algemeen
  2. Definitie
  3. Implementatie


## Inleiding

In dit project wordt de eenvoudige programmeertaal Alphi opgesteld.
Hierbij is het de bedoeling verschillende basiselementen van een imperatieve programmeertalen te implementeren.
zoals bv: toekenning, variablen en volgorde van bewerkingen.
De taal die hieronder wordt uitgewerkt heet Alphi wat staat voor alphanumerical.
Deze taal maakt enkel gebruik van alphanumerische karakters met de uitzondering dat whitespace ook is toegestaan.
Eerst zal de syntax worden vastgelegd.
Vervolgens wordt de semantiek vastgelegd en worden voorbeeld programma's gegeven.
Hierna worden de implementatie aspecten besproken.



## Syntax
```
Pgm ∶∶= Stmt

Stmt   ∶∶= <Var> "Is" <Exp> "Stop"
        | "Command" <Output> <Exp> "Stop"
        ∣ <Stmt> <Stmt>
        ∣ "If" <Exp> "Begin" Stmt "End"
        ∣ "While" <Exp> "Begin" Stmt

Exp    ::= <BExp>
         | <NExp>
         | "Command" <Input>

NExp   ∶∶= <Num>
         ∣ <NVar>
         ∣ <NExp> "Add" <NExp>
         ∣ <NExp> "Sub" <NExp>
         | <NExp> "Mul" <NExp>
         | <NExp> "Div" <NExp>
         | 

BExp   ∶∶= <Bool>
         | <BVar>
         ∣  "Not" <BExp>
         ∣ <NExp> "Gt" <NExp>
         | <NExp> "Lt" <NExp>
         | <NExp> "Eq" <NExp>
         ∣ <BExp> "And" <BExp>
         | <BExp> "Or" <BExp>

Input  ::= "OpenMBot"
         | "CloseMBot"
         | "SensorR"
         | "SensorL"
         | "Ultra"

Output ::= "Print"
         | "MotorR"
         | "MotorL"
         | "Led1"
         | "Led2"

Bool    ∶∶= "True" | "False"
Num     ∶∶=  Int | Float
Int     ::= ["0"-"9"]
Float   ::= <Int>"Float"<Int>
Var     ∶∶= NVar | BVar
NVar    ::= "N"<Letter>+
BVar    ::= "B"<Letter>+
Letter  ::= ["a"-"Z"]

```


## Semantiek

1. Expressies
  1. Numeric









2. Statements







3. Extra








## Programma's
Korte beschrijvingen van het programma
1. demo_police.alp
   Start teller.
   Indien teller even zet Led1 op rood en led 2 op blauw.
   Indien teller oneven zet led2 op rood en led1 op blauw.
   Verhoog Teller met 1
   Begin bij stap 2.

2. demo_line.alp
  Lees beide lichtsensoren uit.
  Indien beide sensoren Zwart zien rij de robot rechtdoor.
  Indien links wit ziet en rechts zwart draai alleen de linker motor.
  Indien rechts wit ziet en links zwart draai alleen de rechter motor.
  Indien Beide wit zien rij achteruit.
  Begin terug bij stap 1.

3. demo_ultra.alp
  Lees Ultrasonesensor uit.
  Indien afstand Groter dan 40 rij rechtdoor.
  Indien afstand Kleiner dan 40 draai de linkermotor vooruit en de rechtermotor achteruit.
  Begin terug bij stap 1.

## Implementatie

1. Parsen

2. Evalueren


## Conclusie
1. Algemeen:
Een alphanumerical taal maken leek in het begin leuk. Dit bracht echter enkele nadelen met zich mee.
Het groote nadeel hieraan is dat je geen special karakters hebt die kunnen instaan voor bv het einde van een statement, haakjes en assignatie.
Verder wordt de taal ook Enorm rap onduidelijk en on leesbaar doordat er weinig tot geen onderscheid gemaakt kan worden tussen keywords en Expressies of variablen.


2. Syntax definitie:
Hierbij zijn er soms onlogische samenstellingen mogelijk zoals Numerical Expression in de IfConditie of WhileStatement
Hierdoor moet dit opgevangen worden tijdens het evalueren dit is ongewild.


3. Implementatie:
De parseLibrary is vrij onduidelijk geschreven er onbreekt een mooie volgbare hierachy die bv wel aanwezig is bij het evalueren.
Dit komt voornamelijk doordat er geen eenduidige manier was om dingen te parsen En er op ieder moment rekening met whitespace en commentaar gehouden moest worden.






#Appendix Broncode:
Geef de volledige code van je project, zorg ervoor dat hierbij lijnnummers
staan zodat je hier makkelijk naar kan refereren.
