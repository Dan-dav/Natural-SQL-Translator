module DBCountries where

import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String  deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int  deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double  deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GColumn =
   GColArea 
 | GColCapital 
 | GColCode 
 | GColContinent 
 | GColCurrency 
 | GColName 
 | GColPopulation 
  deriving Show

data GColumnPart =
   GSColumnAll 
 | GSColumnMultiple GListColumn 
 | GSColumnOne GColumn 
  deriving Show

data GCompOp =
   GCompOpEq 
 | GCompOpGEq 
 | GCompOpGt 
 | GCompOpLEq 
 | GCompOpLt 
 | GCompOpNE 
  deriving Show

data GFromLimPart = GLimNone GTable 
  deriving Show

data GInsertCol = GInsertColWith GColumn GValue 
  deriving Show

data GInsertPart =
   GIColValMultiple GListInsertCol 
 | GIColValOne GInsertCol 
 | GIOnlyValMultiple GListValue 
 | GIOnlyValOne GValue 
  deriving Show

data GLikeOp =
   GLikeBegins 
 | GLikeContains 
 | GLikeEnds 
  deriving Show

newtype GListColumn = GListColumn [GColumn] deriving Show

newtype GListInsertCol = GListInsertCol [GInsertCol] deriving Show

newtype GListSortBy = GListSortBy [GSortBy] deriving Show

newtype GListUpdateCol = GListUpdateCol [GUpdateCol] deriving Show

newtype GListValue = GListValue [GValue] deriving Show

data GOrder =
   GOrdAsc 
 | GOrdDesc 
 | GOrdUnspec 
  deriving Show

data GOrderPart =
   GOrdMultiple GListSortBy 
 | GOrdNothing 
 | GOrdOne GSortBy 
  deriving Show

data GPredicate =
   GPredAnd GPredicate GPredicate 
 | GPredBetween GColumn GValue GValue 
 | GPredComp GColumn GCompOp GValue 
 | GPredIn GColumn GListValue 
 | GPredIsNotNull GColumn 
 | GPredIsNull GColumn 
 | GPredLike GColumn GLikeOp GString 
 | GPredOr GPredicate GPredicate 
  deriving Show

data GPredicatePart =
   GPredNothing 
 | GPredSomething GPredicate 
  deriving Show

data GSortBy = GSortColumn GColumn GOrder 
  deriving Show

data GStatement =
   GStDelete GTable GPredicatePart 
 | GStInsert GTable GInsertPart 
 | GStSelect GColumnPart GFromLimPart GPredicatePart GOrderPart 
 | GStUpdate GTable GUpdatePart GPredicatePart 
  deriving Show

data GTable =
   GTabCountries 
 | GTabCurrencies 
  deriving Show

data GUpdateCol = GUpdateColWith GColumn GValue 
  deriving Show

data GUpdatePart =
   GUpdateMultiple GListUpdateCol 
 | GUpdateOne GUpdateCol 
  deriving Show

data GValue =
   GValInt GInt 
 | GValStr GString 
  deriving Show


instance Gf GColumn where
  gf GColArea = mkApp (mkCId "ColArea") []
  gf GColCapital = mkApp (mkCId "ColCapital") []
  gf GColCode = mkApp (mkCId "ColCode") []
  gf GColContinent = mkApp (mkCId "ColContinent") []
  gf GColCurrency = mkApp (mkCId "ColCurrency") []
  gf GColName = mkApp (mkCId "ColName") []
  gf GColPopulation = mkApp (mkCId "ColPopulation") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "ColArea" -> GColArea 
      Just (i,[]) | i == mkCId "ColCapital" -> GColCapital 
      Just (i,[]) | i == mkCId "ColCode" -> GColCode 
      Just (i,[]) | i == mkCId "ColContinent" -> GColContinent 
      Just (i,[]) | i == mkCId "ColCurrency" -> GColCurrency 
      Just (i,[]) | i == mkCId "ColName" -> GColName 
      Just (i,[]) | i == mkCId "ColPopulation" -> GColPopulation 


      _ -> error ("no Column " ++ show t)

instance Gf GColumnPart where
  gf GSColumnAll = mkApp (mkCId "SColumnAll") []
  gf (GSColumnMultiple x1) = mkApp (mkCId "SColumnMultiple") [gf x1]
  gf (GSColumnOne x1) = mkApp (mkCId "SColumnOne") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "SColumnAll" -> GSColumnAll 
      Just (i,[x1]) | i == mkCId "SColumnMultiple" -> GSColumnMultiple (fg x1)
      Just (i,[x1]) | i == mkCId "SColumnOne" -> GSColumnOne (fg x1)


      _ -> error ("no ColumnPart " ++ show t)

instance Gf GCompOp where
  gf GCompOpEq = mkApp (mkCId "CompOpEq") []
  gf GCompOpGEq = mkApp (mkCId "CompOpGEq") []
  gf GCompOpGt = mkApp (mkCId "CompOpGt") []
  gf GCompOpLEq = mkApp (mkCId "CompOpLEq") []
  gf GCompOpLt = mkApp (mkCId "CompOpLt") []
  gf GCompOpNE = mkApp (mkCId "CompOpNE") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "CompOpEq" -> GCompOpEq 
      Just (i,[]) | i == mkCId "CompOpGEq" -> GCompOpGEq 
      Just (i,[]) | i == mkCId "CompOpGt" -> GCompOpGt 
      Just (i,[]) | i == mkCId "CompOpLEq" -> GCompOpLEq 
      Just (i,[]) | i == mkCId "CompOpLt" -> GCompOpLt 
      Just (i,[]) | i == mkCId "CompOpNE" -> GCompOpNE 


      _ -> error ("no CompOp " ++ show t)

instance Gf GFromLimPart where
  gf (GLimNone x1) = mkApp (mkCId "LimNone") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "LimNone" -> GLimNone (fg x1)


      _ -> error ("no FromLimPart " ++ show t)

instance Gf GInsertCol where
  gf (GInsertColWith x1 x2) = mkApp (mkCId "InsertColWith") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "InsertColWith" -> GInsertColWith (fg x1) (fg x2)


      _ -> error ("no InsertCol " ++ show t)

instance Gf GInsertPart where
  gf (GIColValMultiple x1) = mkApp (mkCId "IColValMultiple") [gf x1]
  gf (GIColValOne x1) = mkApp (mkCId "IColValOne") [gf x1]
  gf (GIOnlyValMultiple x1) = mkApp (mkCId "IOnlyValMultiple") [gf x1]
  gf (GIOnlyValOne x1) = mkApp (mkCId "IOnlyValOne") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "IColValMultiple" -> GIColValMultiple (fg x1)
      Just (i,[x1]) | i == mkCId "IColValOne" -> GIColValOne (fg x1)
      Just (i,[x1]) | i == mkCId "IOnlyValMultiple" -> GIOnlyValMultiple (fg x1)
      Just (i,[x1]) | i == mkCId "IOnlyValOne" -> GIOnlyValOne (fg x1)


      _ -> error ("no InsertPart " ++ show t)

instance Gf GLikeOp where
  gf GLikeBegins = mkApp (mkCId "LikeBegins") []
  gf GLikeContains = mkApp (mkCId "LikeContains") []
  gf GLikeEnds = mkApp (mkCId "LikeEnds") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "LikeBegins" -> GLikeBegins 
      Just (i,[]) | i == mkCId "LikeContains" -> GLikeContains 
      Just (i,[]) | i == mkCId "LikeEnds" -> GLikeEnds 


      _ -> error ("no LikeOp " ++ show t)

instance Gf GListColumn where
  gf (GListColumn [x1,x2]) = mkApp (mkCId "BaseColumn") [gf x1, gf x2]
  gf (GListColumn (x:xs)) = mkApp (mkCId "ConsColumn") [gf x, gf (GListColumn xs)]
  fg t =
    GListColumn (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseColumn" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsColumn" -> fg x1 : fgs x2


      _ -> error ("no ListColumn " ++ show t)

instance Gf GListInsertCol where
  gf (GListInsertCol [x1,x2]) = mkApp (mkCId "BaseInsertCol") [gf x1, gf x2]
  gf (GListInsertCol (x:xs)) = mkApp (mkCId "ConsInsertCol") [gf x, gf (GListInsertCol xs)]
  fg t =
    GListInsertCol (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseInsertCol" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsInsertCol" -> fg x1 : fgs x2


      _ -> error ("no ListInsertCol " ++ show t)

instance Gf GListSortBy where
  gf (GListSortBy [x1,x2]) = mkApp (mkCId "BaseSortBy") [gf x1, gf x2]
  gf (GListSortBy (x:xs)) = mkApp (mkCId "ConsSortBy") [gf x, gf (GListSortBy xs)]
  fg t =
    GListSortBy (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseSortBy" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsSortBy" -> fg x1 : fgs x2


      _ -> error ("no ListSortBy " ++ show t)

instance Gf GListUpdateCol where
  gf (GListUpdateCol [x1,x2]) = mkApp (mkCId "BaseUpdateCol") [gf x1, gf x2]
  gf (GListUpdateCol (x:xs)) = mkApp (mkCId "ConsUpdateCol") [gf x, gf (GListUpdateCol xs)]
  fg t =
    GListUpdateCol (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseUpdateCol" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsUpdateCol" -> fg x1 : fgs x2


      _ -> error ("no ListUpdateCol " ++ show t)

instance Gf GListValue where
  gf (GListValue [x1,x2]) = mkApp (mkCId "BaseValue") [gf x1, gf x2]
  gf (GListValue (x:xs)) = mkApp (mkCId "ConsValue") [gf x, gf (GListValue xs)]
  fg t =
    GListValue (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseValue" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsValue" -> fg x1 : fgs x2


      _ -> error ("no ListValue " ++ show t)

instance Gf GOrder where
  gf GOrdAsc = mkApp (mkCId "OrdAsc") []
  gf GOrdDesc = mkApp (mkCId "OrdDesc") []
  gf GOrdUnspec = mkApp (mkCId "OrdUnspec") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "OrdAsc" -> GOrdAsc 
      Just (i,[]) | i == mkCId "OrdDesc" -> GOrdDesc 
      Just (i,[]) | i == mkCId "OrdUnspec" -> GOrdUnspec 


      _ -> error ("no Order " ++ show t)

instance Gf GOrderPart where
  gf (GOrdMultiple x1) = mkApp (mkCId "OrdMultiple") [gf x1]
  gf GOrdNothing = mkApp (mkCId "OrdNothing") []
  gf (GOrdOne x1) = mkApp (mkCId "OrdOne") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "OrdMultiple" -> GOrdMultiple (fg x1)
      Just (i,[]) | i == mkCId "OrdNothing" -> GOrdNothing 
      Just (i,[x1]) | i == mkCId "OrdOne" -> GOrdOne (fg x1)


      _ -> error ("no OrderPart " ++ show t)

instance Gf GPredicate where
  gf (GPredAnd x1 x2) = mkApp (mkCId "PredAnd") [gf x1, gf x2]
  gf (GPredBetween x1 x2 x3) = mkApp (mkCId "PredBetween") [gf x1, gf x2, gf x3]
  gf (GPredComp x1 x2 x3) = mkApp (mkCId "PredComp") [gf x1, gf x2, gf x3]
  gf (GPredIn x1 x2) = mkApp (mkCId "PredIn") [gf x1, gf x2]
  gf (GPredIsNotNull x1) = mkApp (mkCId "PredIsNotNull") [gf x1]
  gf (GPredIsNull x1) = mkApp (mkCId "PredIsNull") [gf x1]
  gf (GPredLike x1 x2 x3) = mkApp (mkCId "PredLike") [gf x1, gf x2, gf x3]
  gf (GPredOr x1 x2) = mkApp (mkCId "PredOr") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "PredAnd" -> GPredAnd (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "PredBetween" -> GPredBetween (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "PredComp" -> GPredComp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "PredIn" -> GPredIn (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PredIsNotNull" -> GPredIsNotNull (fg x1)
      Just (i,[x1]) | i == mkCId "PredIsNull" -> GPredIsNull (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "PredLike" -> GPredLike (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "PredOr" -> GPredOr (fg x1) (fg x2)


      _ -> error ("no Predicate " ++ show t)

instance Gf GPredicatePart where
  gf GPredNothing = mkApp (mkCId "PredNothing") []
  gf (GPredSomething x1) = mkApp (mkCId "PredSomething") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "PredNothing" -> GPredNothing 
      Just (i,[x1]) | i == mkCId "PredSomething" -> GPredSomething (fg x1)


      _ -> error ("no PredicatePart " ++ show t)

instance Gf GSortBy where
  gf (GSortColumn x1 x2) = mkApp (mkCId "SortColumn") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "SortColumn" -> GSortColumn (fg x1) (fg x2)


      _ -> error ("no SortBy " ++ show t)

instance Gf GStatement where
  gf (GStDelete x1 x2) = mkApp (mkCId "StDelete") [gf x1, gf x2]
  gf (GStInsert x1 x2) = mkApp (mkCId "StInsert") [gf x1, gf x2]
  gf (GStSelect x1 x2 x3 x4) = mkApp (mkCId "StSelect") [gf x1, gf x2, gf x3, gf x4]
  gf (GStUpdate x1 x2 x3) = mkApp (mkCId "StUpdate") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "StDelete" -> GStDelete (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "StInsert" -> GStInsert (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "StSelect" -> GStSelect (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "StUpdate" -> GStUpdate (fg x1) (fg x2) (fg x3)


      _ -> error ("no Statement " ++ show t)

instance Gf GTable where
  gf GTabCountries = mkApp (mkCId "TabCountries") []
  gf GTabCurrencies = mkApp (mkCId "TabCurrencies") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "TabCountries" -> GTabCountries 
      Just (i,[]) | i == mkCId "TabCurrencies" -> GTabCurrencies 


      _ -> error ("no Table " ++ show t)

instance Gf GUpdateCol where
  gf (GUpdateColWith x1 x2) = mkApp (mkCId "UpdateColWith") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "UpdateColWith" -> GUpdateColWith (fg x1) (fg x2)


      _ -> error ("no UpdateCol " ++ show t)

instance Gf GUpdatePart where
  gf (GUpdateMultiple x1) = mkApp (mkCId "UpdateMultiple") [gf x1]
  gf (GUpdateOne x1) = mkApp (mkCId "UpdateOne") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "UpdateMultiple" -> GUpdateMultiple (fg x1)
      Just (i,[x1]) | i == mkCId "UpdateOne" -> GUpdateOne (fg x1)


      _ -> error ("no UpdatePart " ++ show t)

instance Gf GValue where
  gf (GValInt x1) = mkApp (mkCId "ValInt") [gf x1]
  gf (GValStr x1) = mkApp (mkCId "ValStr") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ValInt" -> GValInt (fg x1)
      Just (i,[x1]) | i == mkCId "ValStr" -> GValStr (fg x1)


      _ -> error ("no Value " ++ show t)


