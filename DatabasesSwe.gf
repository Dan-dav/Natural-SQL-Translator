concrete DatabasesSwe of Databases = open SyntaxSwe, (P = ParadigmsSwe), SymbolicSwe, (D = MorphoDictSwe), MakeStructuralSwe in {

lincat
  Statement      = Imp ;
  ColumnPart     = {np : NP ; prep : Prep} ;
  FromLimPart    = NP ;
  PredicatePart  = Adv ;
  Predicate      = S ;
  Column         = N ; -- (CN?)
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
  -- SELECT -----------------------------

  -- StSelect col fromlim pred order = col ++ fromlim ++ pred ++ order ;
  -- {NP ; Prep} NP Adv Adv -> Imp
  StSelect col fromlim pred order = mkImp (mkVP (mkVP (P.mkV2 D.visa_V) (mkNP col.np (mkAdv col.prep (mkNP fromlim pred)))) order) ;
  
  -- SColumnAll = "display all info on" ;
  -- {NP ; Prep}
  SColumnAll = {np = mkNP all_Predet (mkNP D.info_N) ; prep = on_Prep} ;
  -- SColumnOne c = "display the" ++ c ++ "of" ;
  -- N -> {NP ; Prep}
  SColumnOne c = {np = mkNP the_Det c ; prep = possess_Prep} ;
  -- SColumnMultiple cs = "display the" ++ cs ++ "of" ;
  -- ListNP -> {NP ; Prep}
  SColumnMultiple cs = {np = mkNP and_Conj cs ; prep = possess_Prep} ;

  -- BaseColumn c1 c2 = c1 ++ "and" ++ c2 ;
  -- N N -> ListNP
  BaseColumn c1 c2 = mkListNP (mkNP the_Det c1) (mkNP the_Det c2) ;
  -- only have one "the" in the beginning?
  -- ConsColumn c cs = c ++ "," ++ cs ;
  -- N ListNP -> ListNP
  ConsColumn c cs = mkListNP (mkNP the_Det c) cs ;

  ------- FROM, LIMIT

  --SFromTable t l = l ++ t ;

  -- LimNone t = "all" ++ t ;
  -- N -> NP
  LimNone t = mkNP all_Predet (mkNP t) ;
  -- LimNum t i = "the first" ++ i.s ++ t ;
  -- N -> Int -> NP
  -- LimNum t i = mkNP (mkDet the_Quant (mkNum i.s) (mkOrd (mkNumeral n1_Unit))) t ;
  -- mkDet the_Quant num (mkOrd n1_Dig)

  ------- WHERE

  -- PredNothing = "" ;
  -- Adv
  PredNothing = P.mkAdv "" ;
  -- PredSomething p = "where" ++ p ;
  -- S -> Adv
  PredSomething p = mkAdv (mkSubj "där") p ;
  -- (mkS där_Adv p)

  -- PredAnd p1 p2 = p1 ++ "and" ++ p2 ;
  -- S S -> S
  PredAnd p1 p2 = mkS and_Conj p1 p2 ;
  -- PredOr p1 p2 = p1 ++ "or" ++ p2 ;
  -- S S -> S
  PredOr p1 p2 = mkS or_Conj p1 p2 ;
  -- PredComp c compOp v = "the" ++ c ++ "is" ++ compOp ++ v ;
  -- N Prep NP -> S
  PredComp c compOp v = mkS (mkCl (mkNP the_Det c) (mkAdv compOp v)) ;
  -- a vs an?
  -- PredIn c vs = "the" ++ c ++ "is either" ++ vs ;
  -- v1, v2, v3 or v4
  -- N ListNP -> S
  PredIn c vs = mkS (mkCl (mkNP the_Det c) (mkNP either7or_DConj vs)) ;
  -- PredBetween c v1 v2 = "the" ++ c ++ "is between" ++ v1 ++ "and" ++ v2 ;
  -- N NP NP -> S
  PredBetween c v1 v2 = mkS (mkCl (mkNP the_Det c) (mkAdv between_Prep (mkNP and_Conj v1 v2))) ;
  -- PredLike c op st = "the" ++ c ++ op ++ st.s ;
  -- N V2 String -> S
  PredLike c op st = mkS (mkCl (mkNP the_Det c) (mkVP op (symb st.s))) ;
  -- PredIsNull c = "the" ++ c ++ "is null" ;
  -- N -> S
  PredIsNull c = mkS (mkCl (mkNP the_Det c) (P.mkA "null")) ;
  -- PredIsNotNull c = "the" ++ c ++ "is not null" ;
  -- N -> S
  PredIsNotNull c = mkS negativePol (mkCl (mkNP the_Det c) (P.mkA "null")) ;

  -- ValInt i = i.s ;
  -- Int -> NP
  ValInt i = symb i ;
  -- ValStr st = st.s ;
  -- String -> NP
  ValStr st = symb st ;

  -- BaseValue v1 v2 = v1 ++ "or" ++ v2 ;
  -- NP NP -> ListNP
  BaseValue v1 v2 = mkListNP v1 v2 ;
  -- ConsValue v vs = v ++ "," ++ vs ;
  -- NP ListNP -> ListNP
  ConsValue v vs = mkListNP v vs ;

  -- Prep
  -- CompOpEq = "" ;
  CompOpEq = P.noPrep ;
  -- CompOpGt = "above" ;
  CompOpGt = above_Prep ;
  -- CompOpLt = "below" ;
  CompOpLt = under_Prep ;
  -- CompOpGEq = "at least" ;
  CompOpGEq = P.mkPrep "at least" ;
  -- CompOpLEq = "at most" ;
  CompOpLEq = P.mkPrep "at most" ;
  -- CompOpNE = "not" ;
  CompOpNE = P.mkPrep "not" ;

  -- V2
  -- LikeBegins = "begins with" ;
  LikeBegins = P.mkV2 D.börja_V with_Prep ;
  -- LikeEnds = "ends with" ;
  LikeEnds = P.mkV2 D.sluta_1_V with_Prep ;
  -- LikeContains = "contains" ;
  LikeContains = P.mkV2 D.innehålla_V ;

  ------- ORDER BY

  -- OrdNothing = "" ;
  -- Adv
  OrdNothing = P.mkAdv "" ;
  -- OrdOne sb = "ordered by" ++ sb ;
  -- NP -> Adv
  OrdOne sb = mkAdv (P.mkPrep "ordered by") sb ;
  -- OrdMultiple sbs = "ordered by" ++ sbs ;
  -- ListNP -> Adv
  OrdMultiple sbs = mkAdv (P.mkPrep "ordered by") (mkNP and_Conj sbs) ;
  -- Can I have "and then" as a Conj instead?

  -- BaseSortBy sb1 sb2 = sb1 ++ "and then" ++ sb2 ;
  -- NP NP -> ListNP
  BaseSortBy sb1 sb2 = mkListNP sb1 sb2 ;
  -- ConsSortBy sb sbs = sb ++ "," ++ sbs ;
  -- NP ListNP -> ListNP
  ConsSortBy sb sbs = mkListNP sb sbs ;

  -- SortColumn c o = c ++ o ;
  -- N Adv -> NP
  SortColumn c o = mkNP (mkNP c) o ;

  -- Adv
  -- OrdUnspec = "" ;
  OrdUnspec = P.mkAdv "" ;
  -- OrdAsc = "ascending" ;
  OrdAsc = P.mkAdv "ascending" ;
  -- OrdDesc = "descending" ;
  OrdDesc = P.mkAdv "descending" ;

  -- DELETE -----------------------------

  -- StDelete tab pred = "delete all" ++ tab ++ pred ;
  -- N Adv -> Imp
  StDelete tab pred = mkImp (P.mkV2 D.radera_V) (mkNP (mkNP all_Predet (mkNP thePl_Det tab)) pred) ;

  ------- FROM and WHERE as above

  -- INSERT -----------------------------

  -- StInsert tab ins = "add a new entry with" ++ ins ++ "to the table" ++ tab ;
  -- N NP -> Imp
  StInsert tab ins = mkImp (mkVP (mkVP (P.mkV2 D.tillägga_V) (mkNP (mkNP a_Det D.rad_2_N) (mkAdv with_Prep ins))) (mkAdv to_Prep (mkNP tab))) ;

  -- IColValOne ic = ic ;
  -- NP -> NP
  IColValOne ic = ic ;
  -- IColValMultiple ics = ics ;
  -- ListNP -> NP
  IColValMultiple ics = mkNP and_Conj ics ;
  -- IOnlyValOne v = "the value" ++ v ;
  -- NP -> NP
  IOnlyValOne v = mkNP (mkNP the_Det D.värde_N) (mkAdv P.noPrep v) ;
  -- IOnlyValMultiple vs = "the values" ++ vs ;
  -- ListNP -> NP
  IOnlyValMultiple vs = mkNP (mkNP thePl_Det D.värde_N) (mkAdv P.noPrep (mkNP and_Conj vs)) ;

  -- BaseInsertCol ic1 ic2 = ic1 ++ "and" ++ ic2 ;
  -- NP NP -> ListNP
  BaseInsertCol ic1 ic2 = mkListNP ic1 ic2 ;
  -- ConsInsertCol ic ics = ic ++ "," ++ ics ;
  -- NP ListNP -> ListNP
  ConsInsertCol ic ics = mkListNP ic ics ;

  -- InsertColWith c v = "the" ++ c ++ v ;
  -- N NP -> NP
  InsertColWith c v = mkNP (mkNP the_Det c) (mkAdv P.noPrep v) ;

  -- UPDATE -----------------------------

  -- StUpdate tab upd pred = "set" ++ upd ++ "for all" ++ tab ++ pred ;
  -- "set the a to 1 , the b to 2 and the c to 3 for all countries where ..."
  -- N NP Adv -> Imp
  StUpdate tab upd pred = mkImp (mkVP (mkVP (P.mkV2 D.sätta_V) upd) (mkAdv for_Prep (mkNP (mkNP all_Predet (mkNP tab)) pred))) ;

  -- UpdateOne uc = uc ;
  -- NP -> NP
  UpdateOne uc = uc ;
  -- UpdateMultiple ucs = ucs ;
  -- ListNP -> NP
  UpdateMultiple ucs = mkNP and_Conj ucs ;

  -- BaseUpdateCol uc1 uc2 = uc1 ++ "and" ++ uc2 ;
  -- NP NP -> ListNP
  BaseUpdateCol uc1 uc2 = mkListNP uc1 uc2 ;
  -- ConsUpdateCol uc ucs = uc ++ "," ++ ucs ;
  -- NP ListNP -> ListNP
  ConsUpdateCol uc ucs = mkListNP uc ucs ;

  -- UpdateColWith c v = "the" ++ c ++ "to" ++ v ;
  -- N NP -> NP
  UpdateColWith c v = mkNP (mkNP the_Det c) (mkAdv to_Prep v) ;

}