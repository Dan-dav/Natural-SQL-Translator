concrete DatabasesEng of Databases = open SyntaxEng, (P = ParadigmsEng), SymbolicEng in {

lincat
  Statement, ColumnPart, FromPart, PredicatePart, Predicate, Column, 
  [Column], Table, CompOp, Value, [Value], LikeOp, Order, OrderPart,
  SortBy, [SortBy], LimitPart, InsertPart, InsertCol, [InsertCol], UpdatePart, 
  UpdateCol, [UpdateCol] = Str ;

lin
  -- SELECT -----------------------------

  StSelect col from pred order lim = col ++ lim ++ from ++ pred ++ order ;
  
  SColumnAll = "display all info on" ;
  SColumnOne c = "display the" ++ c ++ "of" ;
  SColumnMultiple cs = "display the" ++ cs ++ "of" ;

  BaseColumn c1 c2 = c1 ++ "and" ++ c2 ;
  ConsColumn c cs = c ++ "," ++ cs ;   -- ", " then "and" ???

  ------- FROM

  SFromTable t = t ;

  ------- WHERE

  PredNothing = "" ;
  PredSomething p = "where" ++ p ;

  PredAnd p1 p2 = p1 ++ "and" ++ p2 ;
  PredOr p1 p2 = p1 ++ "or" ++ p2 ;
  PredComp c compOp v = "the" ++ c ++ "is" ++ compOp ++ v ; -- a vs an
  PredIn c vs = "the" ++ c ++ "is either" ++ vs ; -- v1, v2, v3 or v4
  PredBetween c v1 v2 = "the" ++ c ++ "is between" ++ v1 ++ "and" ++ v2 ;
  PredLike c op st = "the" ++ c ++ op ++ st.s ;
  PredIsNull c = "the" ++ c ++ "is null" ;
  PredIsNotNull c = "the" ++ c ++ "is not null" ;

  ValInt i = i.s ;
  ValStr st = st.s ;

  BaseValue v1 v2 = v1 ++ "or" ++ v2 ;
  ConsValue v vs = v ++ "," ++ vs ;

  CompOpEq = "" ;
  CompOpGt = "above" ;
  CompOpLt = "below" ;
  CompOpGEq = "at least" ;
  CompOpLEq = "at most" ;
  CompOpNE = "not" ;

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

  ------- LIMIT

  LimNone = "all" ;
  LimNum i = "the first" ++ i.s ;

  -- DELETE -----------------------------

  StDelete from pred = "delete" ++ from ++ pred ;

  ------- FROM and WHERE as above

  -- INSERT -----------------------------

  StInsert tab ins = "add a new entry with" ++ ins ++ "to the table" ++ tab ;

  IColValOne ic = ic ;
  IColValMultiple ics = ics ;
  IOnlyValOne v = "the value" ++ v ;
  IOnlyValMultiple vs = "the values" ++ vs ; -- "and" instead of "or" for base case

  BaseInsertCol ic1 ic2 = ic1 ++ "and" ++ ic2 ;
  ConsInsertCol ic ics = ic ++ "," ++ ics ;

  InsertColWith c v = "the" ++ c ++ v ;

  -- UPDATE -----------------------------

  StUpdate tab upd pred = "set" ++ upd ++ "for all" ++ tab ++ pred ;
  -- "set the a to 1 , the b to 2 and the c to 3 for all countries where ..."

  UpdateOne uc = uc ;
  UpdateMultiple ucs = ucs ;

  BaseUpdateCol uc1 uc2 = uc1 ++ "and" ++ uc2 ;
  ConsUpdateCol uc ucs = uc ++ "," ++ ucs ;

  UpdateColWith c v = "the" ++ c ++ "to" ++ v ;

}