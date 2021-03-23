abstract Databases = {

flags startcat = Statement ;

cat
  Statement ;
  ColumnPart ;
  FromPart ;
  PredicatePart ;
  Predicate ;
  Column ;
  [Column] {2} ;
  Table ;
  CompOp ;
  Value ;
  [Value] {2} ;
  LikeOp ;
  Order ;
  OrderPart ;
  SortBy ;
  [SortBy] {2} ;
  LimitPart ;
  InsertPart ;
  InsertCol ;
  [InsertCol] {2} ;
  UpdatePart ;
  UpdateCol ;
  [UpdateCol] {2} ;

fun
  -- SELECT -----------------------------
  
  StSelect : ColumnPart -> FromPart -> PredicatePart -> OrderPart -> LimitPart -> Statement ;
  
  SColumnAll : ColumnPart ;
  SColumnOne : Column -> ColumnPart ;
  SColumnMultiple : [Column] -> ColumnPart ;
  
  ------- FROM
  
  SFromTable : Table -> FromPart ;

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

  ValInt : Int -> Value ;
  ValStr : String -> Value ;
  
  CompOpEq, CompOpGt, CompOpLt, CompOpGEq, CompOpLEq, CompOpNE : CompOp ;
  LikeBegins, LikeEnds, LikeContains : LikeOp ; -- Doesn't cover all of LIKE functionality

  ------- ORDER BY

  OrdNothing : OrderPart ;
  OrdOne : SortBy -> OrderPart ;
  OrdMultiple : [SortBy] -> OrderPart ;

  SortColumn : Column -> Order -> SortBy ;

  OrdUnspec : Order ;
  OrdAsc : Order ;
  OrdDesc : Order ;

  ------- LIMIT

  LimNone : LimitPart ;
  LimNum : Int -> LimitPart ;

  -- DELETE -----------------------------
  
  StDelete : FromPart -> PredicatePart -> Statement ;
  
  -- INSERT -----------------------------
  
  StInsert : Table -> InsertPart -> Statement ;

  IColValOne : InsertCol -> InsertPart ;
  IColValMultiple : [InsertCol] -> InsertPart ;
  IOnlyValOne : Value -> InsertPart ;
  IOnlyValMultiple : [Value] -> InsertPart ;

  InsertColWith : Column -> Value -> InsertCol ;
  
  -- UPDATE -----------------------------
  
  StUpdate : Table -> UpdatePart -> PredicatePart -> Statement ;

  UpdateOne : UpdateCol -> UpdatePart ;
  UpdateMultiple : [UpdateCol] -> UpdatePart ;
  -- add support for inc by one etc.

  UpdateColWith : Column -> Value -> UpdateCol ;

}