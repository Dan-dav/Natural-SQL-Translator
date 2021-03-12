concrete DatabasesEng of Databases = {

lincat
  Statement, ColumnPart, ConditionPart, Column, [Column], CompOp = Str ;

lin
  SColCond col cond = col ++ "all countries" ++ cond ;
  SelectAll = "display all info on" ;
  SelectOne c = "display the" ++ c ++ "of" ;
  SelectMultiple cs = "display the" ++ cs ++ "of" ;
  ColName = "name" ;
  ColCapital = "capital" ;
  ColArea = "area" ;
  ColPopulation = "population" ;
  ColContinent = "continent" ;
  ColCurrency = "currency" ;
  BaseColumn c = c ;
  ConsColumn c cs = c ++ "and" ++ cs ;   -- ", " then "and" ???
  CondNone = "" ;
  CondComp c compOp i = "with a" ++ c ++ compOp ++ i.s ; -- a vs an
  CompOpEq = "at" ;
  CompOpGt = "above" ;
  CompOpLt = "below" ;
  CompOpGEq = "at least" ;
  CompOpLEq = "at most" ;
  CompOpNE = "not at" ;
}