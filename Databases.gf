abstract Databases = {

flags startcat = Statement ;

cat
  Statement ;
  ColumnPart ;
  ConditionPart ;
  Column ;
  [Column] {1} ;
  CompOp ;

fun
  SColCond : ColumnPart -> ConditionPart -> Statement ;
  SelectAll : ColumnPart ;
  SelectOne : Column -> ColumnPart ;
  SelectMultiple : [Column] -> ColumnPart ;
  ColName, ColCapital, ColArea, ColPopulation, ColContinent, ColCurrency : Column ;
  CondNone : ConditionPart ;
  CondComp : Column -> CompOp -> Int -> ConditionPart ;
  CompOpEq, CompOpGt, CompOpLt, CompOpGEq, CompOpLEq, CompOpNE : CompOp ;
  -- CondEquals : Column -> Int -> ConditionPart ;
  -- CondGt : Column -> Int -> ConditionPart ;
}