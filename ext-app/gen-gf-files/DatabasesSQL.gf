concrete DatabasesSQL of Databases = {

--oper
  --likeString : LikeOp -> Str -> Str = \op, st ->
  --  case op of {
  --    LikeBegins => "'" ++ st.s ++ "%'" ;
  --    LikeEnds => "'%" ++ st.s ++ "'" ;
  --    LikeContains => "'%" ++ st.s ++ "%'"
  --  } ;

lincat
  Statement, Query, ColumnPart, PredicatePart, Predicate, Column, 
  [Column], Table, CompOp, Value, [Value], Order, OrderPart, SortBy, 
  [SortBy], UpdatePart, UpdateCol, [UpdateCol] = Str ;
  FromLimPart = {from : Str ; lim : Str} ;
  TabCol = {tab : Str ; col : Str} ;
  LikeOp = {before : Str ; after : Str} ;
  InsertPart, InsertCol = {col : Str ; val : Str} ;
  [InsertCol] = {cols : Str ; vals : Str} ;

lin
  -- Query statements, UNION, INTERSECT ----

  StQuery q = q ++ ";" ;

  QUnion q1 q2 = q1 ++ "UNION" ++ q2 ;
  QUnionAll q1 q2 = q1 ++ "UNION ALL" ++ q2 ;
  QIntersect q1 q2 = q1 ++ "INTERSECT" ++ q2 ;

  -- SELECT -----------------------------

  QSelect col fromlim pred order = col ++ fromlim.from ++ pred ++ order ++ fromlim.lim ;

  SColumnAll = "SELECT *" ;  
  SColumnOne c = "SELECT" ++ c ;
  SColumnMultiple cs = "SELECT" ++ cs ;
  SColumnCount = "SELECT COUNT(*)" ;
  SColumnCountCol c = "SELECT COUNT(" ++ c ++ ")" ;
  SColumnCountDistinct c = "SELECT COUNT(DISTINCT" ++ c ++ ")" ;
  --SColumnCountDistinctMultiple cs = "SELECT COUNT(DISTINCT" ++ cs ++ ")" ;
  SColumnAvg c = "SELECT AVG(" ++ c ++ ")" ;
  SColumnSum c = "SELECT SUM(" ++ c ++ ")" ;

  BaseColumn c1 c2 = c1 ++ "," ++ c2 ;
  ConsColumn c cs = c ++ "," ++ cs ;

  ------- FROM, LIMIT
  
  --SFromTable t = "FROM" ++ t ;

  FromTab t = {from = "FROM" ++ t ; lim = ""} ;
  -- FromTabLim t i = {from = "FROM" ++ t ; lim = "LIMIT" ++ i.s} ;
  FromJoinInner tc1 tc2 = {from = "FROM" ++ tc1.tab ++ "INNER JOIN" ++ tc2.tab ++ "ON" ++ tc1.tab ++ "." ++ tc1.col ++ "=" ++ tc2.tab ++ "." ++ tc2.col ; lim = ""} ;

  ------- JOIN

  -- CN -> CN -> {NP ; NP} JoinType?
  --JInner = {left = ? ; right = ?} ;
  --JLeft = ? ;
  --JRight = ? ;
  --JFull = ? ;

  -- N CN -> CN
  JTabCol t c = {tab = t ; col = c} ;

  -- CN -> CN
  ColumnTabCol tc = tc.tab ++ "." ++ tc.col ;

  ------- WHERE

  PredNothing = "" ;
  PredSomething p = "WHERE" ++ p ;

  PredAnd p1 p2 = p1 ++ "AND" ++ p2 ;
  PredOr p1 p2 = p1 ++ "OR" ++ p2 ;
  PredComp c compOp v = c ++ compOp ++ v ;
  PredIn c vs = c ++ "IN (" ++ vs ++ ")" ;
  PredBetween c v1 v2 = c ++ "BETWEEN" ++ v1 ++ "AND" ++ v2 ;
  PredLike c op st = c ++ "LIKE" ++ op.before ++ st.s ++ op.after ;
  PredIsNull c = c ++ "IS NULL" ;
  PredIsNotNull c = c ++ "IS NOT NULL" ;

  ValInt i = i.s ;
  ValStr st = st.s ; -- predef.Bind "'" ++ st.s ++ "'" ;

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

  StInsert tab ins = "INSERT INTO" ++ tab ++ ins.col ++ "VALUES" ++ ins.val ++ ";" ;

  IColValOne ic = {col = "(" ++ ic.col ++ ")"; val = "(" ++ ic.val ++ ")"} ;
  IColValMultiple ics = {col = "(" ++ ics.cols ++ ")"; val = "(" ++ ics.vals ++ ")"} ;
  IOnlyValOne v = {col = ""; val = "(" ++ v ++ ")"} ;
  IOnlyValMultiple vs = {col = ""; val = "(" ++ vs ++ ")"} ;

  BaseInsertCol ic1 ic2 = {cols = ic1.col ++ "," ++ ic2.col; vals = ic1.val ++ "," ++ ic2.val} ;
  ConsInsertCol ic ics = {cols = ic.col ++ "," ++ ics.cols; vals = ic.val ++ "," ++ ics.vals} ;

  InsertColWith c v = {col = c; val = v} ;

  -- UPDATE -----------------------------

  StUpdate tab upd pred = "UPDATE" ++ tab ++ "SET" ++ upd ++ pred ++ ";" ;

  UpdateOne uc = uc ;
  UpdateMultiple ucs = ucs ;

  BaseUpdateCol uc1 uc2 = uc1 ++ "," ++ uc2 ;
  ConsUpdateCol uc ucs = uc ++ "," ++ ucs ;

  UpdateColWith c v = c ++ "=" ++ v ;

}