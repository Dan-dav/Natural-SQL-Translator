abstract Databases = {

flags startcat = Statement ;

cat
  Statement ;
  Query ;
  ColumnPart ;
  FromLimPart ;
  JoinType ;
  TabCol ;
  Column ;
  [Column] {2} ;
  PredicatePart ;
  Predicate ;
  Value ;
  [Value] {2} ;
  CompOp ;
  LikeOp ;
  OrderPart ;
  SortBy ;
  [SortBy] {2} ;
  Order ;
  InsertPart ;
  InsertCol ;
  [InsertCol] {2} ;
  UpdatePart ;
  UpdateCol ;
  [UpdateCol] {2} ;
  Table ;

fun
  -- Query statements, UNION, INTERSECT ----

  StQuery : Query -> Statement ;

  QUnion : Query -> Query -> Query ;
  QUnionAll : Query -> Query -> Query ;
  QIntersect : Query -> Query -> Query ;

  -- SELECT --------------------------------
  
  --StSelect : ColumnPart -> FromLimPart -> PredicatePart -> OrderPart -> Statement ;
  QSelect : ColumnPart -> FromLimPart -> PredicatePart -> OrderPart -> Query ;
  
  SColumnAll : ColumnPart ;
  SColumnOne : Column -> ColumnPart ;
  SColumnMultiple : [Column] -> ColumnPart ;
  SColumnCount : ColumnPart ; -- or include ability to use columns and distinct
  SColumnCountCol : Column -> ColumnPart ;
  SColumnCountDistinct : Column -> ColumnPart ;
  --SColumnCountDistinctMultiple : [Column] -> ColumnPart ;
  SColumnAvg : Column -> ColumnPart ;
  SColumnSum : Column -> ColumnPart ;
  
  ------- FROM, LIMIT
  
  --SFromTable : Table -> Limit -> FromLimPart ;

  FromTab : Table -> FromLimPart ;
  -- FromTabLim : Table -> Int -> FromLimPart ;
  -- FromJoinInner : TabCol -> TabCol -> FromLimPart ;
  FromJoin : JoinType -> TabCol -> TabCol -> FromLimPart ;
  -- FromJoinLim

  ------- JOIN

  JInner, JLeft, JRight, JFull : JoinType ;

  JTabCol : Table -> Column -> TabCol ;

  ColumnTabCol : TabCol -> Column ;

  ------- WHERE

  PredNothing : PredicatePart ;
  PredSomething : Predicate -> PredicatePart ;

  PredAnd : Predicate -> Predicate -> Predicate ; -- mind parentheses on sql side
  PredOr : Predicate -> Predicate -> Predicate ;
  PredComp : Column -> CompOp -> Value -> Predicate ; -- =, <>, >, >=, <, <=
  PredIn : Column -> [Value] -> Predicate ;
  PredBetween : Column -> Value -> Value -> Predicate ;
  PredLike : Column -> LikeOp -> String -> Predicate ;
  PredIsNull : Column -> Predicate ;
  PredIsNotNull : Column -> Predicate ;
  PredSubQuery : Column -> Query -> Predicate ;

  ValInt : Int -> Value ;
  ValStr : String -> Value ;
  
  CompOpEq, CompOpGt, CompOpLt, CompOpGEq, CompOpLEq, CompOpNE : CompOp ;
  LikeBegins, LikeEnds, LikeContains : LikeOp ; -- Doesn't cover all of LIKE functionality

  ------- ORDER BY

  OrdNothing : OrderPart ;
  OrdOne : SortBy -> OrderPart ;
  OrdMultiple : [SortBy] -> OrderPart ;

  SortColumn : Column -> Order -> SortBy ;

  OrdUnspec, OrdAsc, OrdDesc : Order ;

  -- DELETE --------------------------------
  
  StDelete : Table -> PredicatePart -> Statement ;
  
  -- INSERT --------------------------------
  
  StInsert : Table -> InsertPart -> Statement ;

  IColValOne : InsertCol -> InsertPart ;
  IColValMultiple : [InsertCol] -> InsertPart ;
  IOnlyValOne : Value -> InsertPart ;
  IOnlyValMultiple : [Value] -> InsertPart ;
  ISubQuery : Query -> InsertPart ;
  ISubQueryColOne : Query -> Column -> InsertPart ;
  ISubQueryColMultiple : Query -> [Column] -> InsertPart ;

  InsertColWith : Column -> Value -> InsertCol ;
  
  -- UPDATE --------------------------------
  
  StUpdate : Table -> UpdatePart -> PredicatePart -> Statement ;

  UpdateOne : UpdateCol -> UpdatePart ;
  UpdateMultiple : [UpdateCol] -> UpdatePart ;
  -- add support for inc by one etc.

  UpdateColWith : Column -> Value -> UpdateCol ;

}