# Alphi

Language that only uses alphanumerical characters and whitespace.

#Backus–Naur form
```
    Pgm ∶∶= Stmt

    Stmt ∶∶= <Var> "Is" <Exp> "Stop"
         | "Command" <Output> <Exp> "Stop"
         ∣ <Stmt> <Stmt>
         ∣ "If" <Exp> "Begin" Stmt "End"
         ∣ "While" <Exp> "Begin" Stmt

    Exp  ::= <BExp> | <NExp> | "Command" <Input>

    NExp ∶∶= <Num>
           ∣ <NVar>
           |
           ∣ <NExp> "Add" <NExp>
           ∣ <NExp> "Sub" <NExp>
           | <NExp> "Mul" <NExp>
           | <NExp> "Div" <NExp>

    BExp ∶∶= <Bool>
           | <BVar>
           | <Bool>
           ∣ <NExp> "Gt" <NExp>
           | <NExp> "Lt" <NExp>
           | <NExp> "Eq" <NExp>
           ∣       "Not" <BExp>
           ∣ <BExp> "And" <BExp>
           | <BExp> "Or" <BExp>


    Input ::= "OpenMBot"
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
