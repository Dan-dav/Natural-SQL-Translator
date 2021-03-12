concrete DatabasesSQL of Databases = {

lincat
  Statement, ColumnPart, ConditionPart, Column, [Column], CompOp = Str ;

lin
  SColCond col cond = col ++ "FROM countries" ++ cond ;
  SelectAll = "SELECT *" ;  
  SelectOne c = "SELECT" ++ c ;
  SelectMultiple cs = "SELECT" ++ cs ;
  ColName = "name" ;
  ColCapital = "capital" ;
  ColArea = "area" ;
  ColPopulation = "population" ;
  ColContinent = "continent" ;
  ColCurrency = "currency" ;
  BaseColumn c = c ;
  ConsColumn c cs = c ++ "," ++ cs ;
  CondNone = ";" ;
  CondComp c compOp i = "WHERE" ++ c ++ compOp ++ i.s ++ ";" ;
  CompOpEq = "=" ;
  CompOpGt = ">" ;
  CompOpLt = "<" ;
  CompOpGEq = ">=" ;
  CompOpLEq = "<=" ;
  CompOpNE = "<>" ;
}