concrete DatabasesEng of Databases = open SyntaxEng, (P = ParadigmsEng), SymbolicEng in {

lincat
  Statement, ColumnPart, FromPart, PredicatePart, Predicate, Column, 
  [Column], Table, CompOp, Value, [Value], LikeOp, Order, OrderPart,
  SortBy, [SortBy] = Str ;

lin
  
  -- SELECT -----------------------------
  StSelect col from pred order = col ++ from ++ pred ++ order ;
  
  SColumnAll = "display all info on" ;
  SColumnOne c = "display the" ++ c ++ "of" ;
  SColumnMultiple cs = "display the" ++ cs ++ "of" ;
  ColName = "name" ;
  ColCapital = "capital" ;
  ColArea = "area" ;
  ColPopulation = "population" ;
  ColContinent = "continent" ;
  ColCurrency = "currency" ;
  BaseColumn c1 c2 = c1 ++ "and" ++ c2 ;
  ConsColumn c cs = c ++ "," ++ cs ;   -- ", " then "and" ???

  ------- FROM

  SFromTable t = "all" ++ t ;
  TabCountries = "countries" ;

  ------- WHERE

  PredNothing = "" ;
  PredSomething p = "where" ++ p ;
  PredAnd p1 p2 = p1 ++ "and" ++ p2 ;
  PredOr p1 p2 = p1 ++ "or" ++ p2 ;
  PredComp c compOp v = "the" ++ c ++ "is" ++ compOp ++ v ; -- a vs an
  PredIn c vs = "the" ++ c ++ "is either" ++ vs ; -- v1, v2, v3 or v4
  PredBetween c v1 v2 = "the" ++ c ++ "is between" ++ v1 ++ "and" ++ v2 ;
  --PredLike c op st = "the" ++ c ++ op ++ st.s ;
  PredIsNull c = "the" ++ c ++ "is null" ;
  PredIsNotNull c = "the" ++ c ++ "is not null" ;

  ValInt i = i.s ;
  --ValStr st = st.s ;

  BaseValue v1 v2 = v1 ++ "or" ++ v2 ;
  ConsValue v vs = v ++ "," ++ vs ;

  CompOpEq = "at" ;
  CompOpGt = "above" ;
  CompOpLt = "below" ;
  CompOpGEq = "at least" ;
  CompOpLEq = "at most" ;
  CompOpNE = "not at" ;

  LikeBegins = "begins with" ;
  LikeEnds = "ends with" ;
  LikeContains = "contains" ;

  ------- ORDER BY

  OrdNothing = "" ;
  OrdOne sb = "ordered by" ++ sb ;
  OrdMultiple sbs = "ordered by" ++ sbs ;

  BaseSortBy sb1 sb2 = sb1 ++ "and then" ++ sb2 ;
  ConsSortBy sb sbs = sb ++ "," ++ sbs ;

  SortColumn c o = c ++ o ;

  OrdUnspec = "" ;
  OrdAsc = "ascending" ;
  OrdDesc = "descending" ;

  -- DELETE -----------------------------
  StDelete from pred = "delete" ++ from ++ pred ;
  ------- FROM and WHERE as above

  -- INSERT -----------------------------
  -- UPDATE -----------------------------
}