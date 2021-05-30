concrete DatabasesEng of Databases = open SyntaxEng, (P = ParadigmsEng), SymbolicEng, (D = MorphoDictEng) in {

lincat
  Statement      = Imp ;
  Query          = NP ;
  ColumnPart     = {np : NP ; prep : Prep} ;
  FromLimPart    = NP ;
  TabCol         = CN ;
  PredicatePart  = Adv ;
  Predicate      = S ;
  Column         = CN ;
  [Column]       = ListNP ;
  Table          = N ;
  CompOp         = Prep ;
  Value          = NP ;
  [Value]        = ListNP ;
  LikeOp         = V2 ;
  Order          = Adv ;
  OrderPart      = Adv ;
  SortBy         = NP ;
  [SortBy]       = ListNP ;
  InsertPart     = NP ;
  InsertCol      = NP ;
  [InsertCol]    = ListNP ;
  UpdatePart     = NP ;
  UpdateCol      = NP ;
  [UpdateCol]    = ListNP ;

lin
  -- Query statements, UNION, INTERSECT ----

  -- NP -> Imp
  StQuery q = variants {mkImp (P.mkV2 (P.mkV "")) q ; 
                        mkImp (P.mkV2 D.display_V) q ; 
                        mkImp (P.mkV2 D.show_2_V) q} ;

  -- NP NP -> NP
  QUnion q1 q2 = mkNP and_Conj q1 q2 ;
  QUnionAll q1 q2 = mkNP (P.mkConj "as well as") q1 q2 ;
  QIntersect q1 q2 = mkNP (P.mkConj "which are also") q1 q2 ;

  -- SELECT --------------------------------

  -- {NP ; Prep} NP Adv Adv -> Imp
  --StSelect col fromlim pred order = mkImp (P.mkV2 D.display_V) (mkNP (mkNP col.np (mkAdv col.prep (mkNP fromlim pred))) order) ;
  -- {NP ; Prep} NP Adv Adv -> NP
  QSelect col fromlim pred order = mkNP (mkNP col.np (mkAdv col.prep (mkNP fromlim pred))) order ;
  
  -- {NP ; Prep}
  SColumnAll = variants{{np = mkNP all_Predet (mkNP D.info_N) ; prep = P.mkPrep "about"} ;
                        {np = mkNP (P.mkN "") ; prep = P.noPrep}} ;
  -- CN -> {NP ; Prep}
  SColumnOne c = {np = mkNP the_Det c ; prep = possess_Prep} ;
  -- ListNP -> {NP ; Prep}
  SColumnMultiple cs = {np = mkNP and_Conj cs ; prep = possess_Prep} ;
  -- {NP ; Prep}
  SColumnCount = {np = mkNP the_Det D.number_N ; prep = possess_Prep} ;
  -- CN -> {NP ; Prep}
  SColumnCountCol c = {np = mkNP (mkNP the_Det D.number_N) (mkAdv possess_Prep (mkNP aPl_Det c)) ; prep = possess_Prep} ;
  SColumnCountDistinct c = {np = mkNP (mkNP the_Det D.number_N) (mkAdv possess_Prep (mkNP aPl_Det (mkCN D.distinct_A c))) ; prep = possess_Prep} ;
  -- ListNP -> {NP ; Prep}
  --not in postgres SColumnCountDistinctMultiple cs = {np = mkNP (mkNP the_Det D.number_N) (mkAdv possess_Prep (mkNP (mkNP aPl_Det (mkCN D.distinct_A D.set_N)) (mkAdv possess_Prep (mkNP and_Conj cs)))) ; prep = possess_Prep} ;
  -- CN -> {NP ; Prep}
  SColumnAvg c = {np = mkNP the_Det (mkCN D.average_A c) ; prep = possess_Prep} ;
  SColumnSum c = {np = mkNP the_Det (mkCN D.total_A c) ; prep = possess_Prep} ;

  -- CN CN -> ListNP
  BaseColumn c1 c2 = mkListNP (mkNP the_Det c1) (mkNP the_Det c2) ;
  -- only have one "the" in the beginning?
  -- CN ListNP -> ListNP
  ConsColumn c cs = mkListNP (mkNP the_Det c) cs ;

  ------- FROM, LIMIT

  --SFromTable t l = l ++ t ;

  -- N -> NP
  FromTab t = mkNP all_Predet (mkNP aPl_Det t) ;
  -- N -> Int -> NP
  -- FromTabLim t i = mkNP (mkDet the_Quant (mkNum i.s) (mkOrd (mkNumeral n1_Unit))) t ;
  -- mkDet the_Quant num (mkOrd n1_Dig)
  -- (the first 5/five countries)/(the 5/five first countries)
  -- JoinType? CN CN -> NP
  -- FromJoin jt tc1 tc2 = ? ;
  -- CN CN -> NP
  FromJoinInner tc1 tc2 = mkNP (mkNP aPl_Det D.entry_N) (mkAdv with_Prep (mkNP (mkNP the_Det tc1) (mkAdv (P.mkPrep "matched with") (mkNP the_Det tc2)))) ;

--(mkNP the_Det tc2)
  ------- JOIN

  -- CN -> CN -> {NP ; NP} JoinType?
  --JInner = {left = ? ; right = ?} ;
  --JLeft = ? ;
  --JRight = ? ;
  --JFull = ? ;

  -- N CN -> CN
  JTabCol t c = mkCN c (mkAdv possess_Prep (mkNP t)) ; -- aPl_Det t

  -- CN -> CN
  ColumnTabCol tc = tc ;

  ------- WHERE

  -- Adv
  PredNothing = P.mkAdv "" ;
  -- S -> Adv
  PredSomething p = mkAdv (P.mkSubj "where") p ;

  -- S S -> S
  PredAnd p1 p2 = mkS and_Conj p1 p2 ;
  -- S S -> S
  PredOr p1 p2 = mkS or_Conj p1 p2 ;
  -- CN Prep NP -> S
  PredComp c compOp v = mkS (mkCl (mkNP the_Det c) (mkAdv compOp v)) ;
  -- a vs an?
  -- v1, v2, v3 or v4
  -- CN ListNP -> S
  PredIn c vs = mkS (mkCl (mkNP the_Det c) (mkNP either7or_DConj vs)) ;
  -- CN NP NP -> S
  PredBetween c v1 v2 = mkS (mkCl (mkNP the_Det c) (mkAdv between_Prep (mkNP and_Conj v1 v2))) ;
  -- CN V2 String -> S
  PredLike c op st = mkS (mkCl (mkNP the_Det c) (mkVP op (symb st.s))) ;
  -- CN -> S
  PredIsNull c = mkS (mkCl (mkNP the_Det c) D.null_A) ;
  -- CN -> S
  PredIsNotNull c = mkS negativePol (mkCl (mkNP the_Det c) D.null_A) ;

  -- Int -> NP
  ValInt i = symb i ;
  -- String -> NP
  ValStr st = symb st ;

  -- NP NP -> ListNP
  BaseValue v1 v2 = mkListNP v1 v2 ;
  -- NP ListNP -> ListNP
  ConsValue v vs = mkListNP v vs ;

  -- Prep
  CompOpEq = P.noPrep ;
  CompOpGt = above_Prep ;
  CompOpLt = under_Prep ;
  CompOpGEq = P.mkPrep "at least" ;
  CompOpLEq = P.mkPrep "at most" ;
  CompOpNE = P.mkPrep "not" ;

  -- V2
  LikeBegins = P.mkV2 D.begin_1_V with_Prep ;
  LikeEnds = P.mkV2 D.end_V with_Prep ;
  LikeContains = P.mkV2 D.contain_V ;

  ------- ORDER BY

  -- Adv
  OrdNothing = P.mkAdv "" ;
  -- NP -> Adv
  OrdOne sb = mkAdv (P.mkPrep "ordered by") sb ;
  -- ListNP -> Adv
  OrdMultiple sbs = mkAdv (P.mkPrep "ordered by") (mkNP and_Conj sbs) ;
  -- Can I have "and then" as a Conj instead?

  -- NP NP -> ListNP
  BaseSortBy sb1 sb2 = mkListNP sb1 sb2 ;
  -- NP ListNP -> ListNP
  ConsSortBy sb sbs = mkListNP sb sbs ;

  -- CN Adv -> NP
  SortColumn c o = mkNP (mkNP c) o ;

  -- Adv
  OrdUnspec = P.mkAdv "" ;
  OrdAsc = P.mkAdv "ascending" ;
  OrdDesc = P.mkAdv "descending" ;

  -- DELETE --------------------------------

  -- N Adv -> Imp
  StDelete tab pred = mkImp (P.mkV2 D.delete_V) (mkNP (mkNP all_Predet (mkNP tab)) pred) ;

  ------- FROM and WHERE as above

  -- INSERT --------------------------------

  -- N NP -> Imp
  StInsert tab ins = mkImp (mkVP (mkVP (P.mkV2 D.add_V) (mkNP (mkNP a_Det D.row_N) (mkAdv with_Prep ins))) (mkAdv to_Prep (mkNP tab))) ;

  -- NP -> NP
  IColValOne ic = ic ;
  -- ListNP -> NP
  IColValMultiple ics = mkNP and_Conj ics ;
  -- NP -> NP
  IOnlyValOne v = mkNP (mkNP the_Det D.value_N) (mkAdv P.noPrep v) ;
  -- ListNP -> NP
  IOnlyValMultiple vs = mkNP (mkNP thePl_Det D.value_N) (mkAdv P.noPrep (mkNP and_Conj vs)) ;

  -- NP NP -> ListNP
  BaseInsertCol ic1 ic2 = mkListNP ic1 ic2 ;
  -- NP ListNP -> ListNP
  ConsInsertCol ic ics = mkListNP ic ics ;

  -- CN NP -> NP
  InsertColWith c v = mkNP (mkNP the_Det c) (mkAdv P.noPrep v) ;

  -- UPDATE --------------------------------

  -- "set the a to 1 , the b to 2 and the c to 3 for all countries where ..."
  -- N NP Adv -> Imp
  StUpdate tab upd pred = mkImp (mkVP (mkVP (P.mkV2 D.set_1_V) upd) (mkAdv for_Prep (mkNP (mkNP all_Predet (mkNP tab)) pred))) ;

  -- NP -> NP
  UpdateOne uc = uc ;
  -- ListNP -> NP
  UpdateMultiple ucs = mkNP and_Conj ucs ;

  -- NP NP -> ListNP
  BaseUpdateCol uc1 uc2 = mkListNP uc1 uc2 ;
  -- NP ListNP -> ListNP
  ConsUpdateCol uc ucs = mkListNP uc ucs ;

  -- CN NP -> NP
  UpdateColWith c v = mkNP (mkNP the_Det c) (mkAdv to_Prep v) ;
}