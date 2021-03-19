concrete DatabasesSQL of Databases = {

--oper
  --likeString : LikeOp -> Str -> Str = \op, st ->
  --  case op of {
  --    LikeBegins => "'" ++ st.s ++ "%'" ;
  --    LikeEnds => "'%" ++ st.s ++ "'" ;
  --    LikeContains => "'%" ++ st.s ++ "%'"
  --  } ;

lincat
  Statement, ColumnPart, FromPart, PredicatePart, Predicate, Column, 
  [Column], Table, CompOp, Value, [Value], Order, OrderPart, SortBy, 
  [SortBy] = Str ;
  LikeOp = {before : Str ; after : Str} ;

lin
  
  -- SELECT -----------------------------
  StSelect col from pred order = col ++ from ++ pred ++ order ++ ";" ;

  SColumnAll = "SELECT *" ;  
  SColumnOne c = "SELECT" ++ c ;
  SColumnMultiple cs = "SELECT" ++ cs ;
  ColName = "name" ;
  ColCapital = "capital" ;
  ColArea = "area" ;
  ColPopulation = "population" ;
  ColContinent = "continent" ;
  ColCurrency = "currency" ;
  BaseColumn c1 c2 = c1 ++ "," ++ c2 ;
  ConsColumn c cs = c ++ "," ++ cs ;

  ------- FROM
  
  SFromTable t = "FROM" ++ t ;
  TabCountries = "countries" ;

  ------- WHERE

  PredNothing = "" ;
  PredSomething p = "WHERE" ++ p ;
  PredAnd p1 p2 = p1 ++ "AND" ++ p2 ;
  PredOr p1 p2 = p1 ++ "OR" ++ p2 ;
  PredComp c compOp v = c ++ compOp ++ v ;
  PredIn c vs = c ++ "IN (" ++ vs ++ ")" ;
  PredBetween c v1 v2 = c ++ "BETWEEN" ++ v1 ++ "AND" ++ v2 ;
  --PredLike c op st = c ++ "LIKE" ++ op.before ++ st.s ++ op.after ;
  PredIsNull c = c ++ "IS NULL" ;
  PredIsNotNull c = c ++ "IS NOT NULL" ;

  ValInt i = i.s ;
  --ValStr st = "'" ++ st.s ++ "'" ;

  BaseValue v1 v2 = v1 ++ "," ++ v2 ;
  ConsValue v vs = v ++ "," ++ vs ;

  CompOpEq = "=" ;
  CompOpGt = ">" ;
  CompOpLt = "<" ;
  CompOpGEq = ">=" ;
  CompOpLEq = "<=" ;
  CompOpNE = "<>" ;

  LikeBegins = {before = "'" ; after = "%'"} ;
  LikeEnds = {before = "'%" ; after = "'"} ;
  LikeContains = {before = "'%" ; after = "%'"} ;

  ------- ORDER BY

  OrdNothing = "" ;
  OrdOne sb = "ORDER BY" ++ sb ;
  OrdMultiple sbs = "ORDER BY" ++ sbs ;

  BaseSortBy sb1 sb2 = sb1 ++ "," ++ sb2 ;
  ConsSortBy sb sbs = sb ++ "," ++ sbs ;

  SortColumn c o = c ++ o ;

  OrdUnspec = "" ;
  OrdAsc = "ASC" ;
  OrdDesc = "DESC" ;

  -- DELETE -----------------------------
  StDelete from pred = "DELETE" ++ from ++ pred ++ ";" ;
  ------- FROM and WHERE as above

  -- INSERT -----------------------------
  -- UPDATE -----------------------------
}