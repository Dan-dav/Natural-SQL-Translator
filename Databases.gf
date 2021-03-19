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

fun
  StSelect : ColumnPart -> FromPart -> PredicatePart -> OrderPart -> Statement ;
  StDelete : FromPart -> PredicatePart -> Statement ;
  -- StInsert : Statement ;
  -- StUpdate : Statement ;

  -- SELECT -----------------------------

  SColumnAll : ColumnPart ;
  SColumnOne : Column -> ColumnPart ;
  SColumnMultiple : [Column] -> ColumnPart ;
  
  ColName, ColCapital, ColArea, ColPopulation, ColContinent, ColCurrency : Column ;

  ------- FROM
  
  SFromTable : Table -> FromPart ;
  TabCountries : Table ;

  ------- WHERE

  PredNothing : PredicatePart ;
  PredSomething : Predicate -> PredicatePart ;
  PredAnd : Predicate -> Predicate -> Predicate ; -- mind parentheses on sql side
  PredOr : Predicate -> Predicate -> Predicate ;
  PredComp : Column -> CompOp -> Value -> Predicate ; -- =, <>, >, >=, <, <=
  PredIn : Column -> [Value] -> Predicate ;
  PredBetween : Column -> Value -> Value -> Predicate ;
  --PredLike : Column -> LikeOp -> Str -> Predicate ;
  PredIsNull : Column -> Predicate ;
  PredIsNotNull : Column -> Predicate ;

  ValInt : Int -> Value ;
  --ValStr : Str -> Value ;
  
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

  -- DELETE -----------------------------
  -- INSERT -----------------------------
  -- UPDATE -----------------------------
}