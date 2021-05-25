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

  -- {NP ; Prep} NP Adv Adv -> Imp
  StSelect col fromlim pred order = mkImp (mkVP (mkVP (P.mkV2 D.visa_V) (mkNP col.np (mkAdv col.prep (mkNP fromlim pred)))) order) ;
  
  -- {NP ; Prep}
  SColumnAll = {np = mkNP all_Predet (mkNP D.info_N) ; prep = P.mkPrep "om"} ;
  -- N -> {NP ; Prep}
  SColumnOne c = {np = mkNP the_Det c ; prep = possess_Prep} ;
  -- ListNP -> {NP ; Prep}
  SColumnMultiple cs = {np = mkNP and_Conj cs ; prep = possess_Prep} ;

  -- N N -> ListNP
  BaseColumn c1 c2 = mkListNP (mkNP the_Det c1) (mkNP the_Det c2) ;
  -- N ListNP -> ListNP
  ConsColumn c cs = mkListNP (mkNP the_Det c) cs ;

  ------- FROM, LIMIT

  --SFromTable t l = l ++ t ;

  -- N -> NP
  LimNone t = mkNP all_Predet (mkNP aPl_Det t) ;
  -- N -> Int -> NP
  -- LimNum t i = mkNP (mkDet the_Quant (mkNum i.s) (mkOrd (mkNumeral n1_Unit))) t ;
  -- aPl_Det?
  -- mkDet the_Quant num (mkOrd n1_Dig)
  -- (de första fem/5 länderna)/(De fem/5 första länderna)

  ------- WHERE

  -- Adv
  PredNothing = P.mkAdv "" ;
  -- S -> Adv
  PredSomething p = mkAdv (mkSubj "där") p ;
  -- (mkS där_Adv p)

  -- S S -> S
  PredAnd p1 p2 = mkS and_Conj p1 p2 ;
  -- S S -> S
  PredOr p1 p2 = mkS or_Conj p1 p2 ;
  -- N Prep NP -> S
  PredComp c compOp v = mkS (mkCl (mkNP the_Det c) (mkAdv compOp v)) ;
  -- a vs an?
  -- v1, v2, v3 or v4
  -- N ListNP -> S
  PredIn c vs = mkS (mkCl (mkNP the_Det c) (mkNP either7or_DConj vs)) ;
  -- N NP NP -> S
  PredBetween c v1 v2 = mkS (mkCl (mkNP the_Det c) (mkAdv between_Prep (mkNP and_Conj v1 v2))) ;
  -- N V2 String -> S
  PredLike c op st = mkS (mkCl (mkNP the_Det c) (mkVP op (symb st.s))) ;
  -- N -> S
  PredIsNull c = mkS (mkCl (mkNP the_Det c) (P.mkA "null")) ;
  -- N -> S
  PredIsNotNull c = mkS negativePol (mkCl (mkNP the_Det c) (P.mkA "null")) ;

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
  LikeBegins = P.mkV2 D.börja_V with_Prep ;
  LikeEnds = P.mkV2 D.sluta_1_V with_Prep ;
  LikeContains = P.mkV2 D.innehålla_V ;

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

  -- N Adv -> NP
  SortColumn c o = mkNP (mkNP c) o ;

  -- Adv
  OrdUnspec = P.mkAdv "" ;
  OrdAsc = P.mkAdv "ascending" ;
  OrdDesc = P.mkAdv "descending" ;

  -- DELETE -----------------------------

  -- N Adv -> Imp
  StDelete tab pred = mkImp (P.mkV2 D.radera_V) (mkNP (mkNP all_Predet (mkNP aPl_Det tab)) pred) ;

  ------- FROM and WHERE as above

  -- INSERT -----------------------------

  -- N NP -> Imp
  StInsert tab ins = mkImp (mkVP (mkVP (P.mkV2 D.tillägga_V) (mkNP (mkNP a_Det D.rad_2_N) (mkAdv with_Prep ins))) (mkAdv to_Prep (mkNP aPl_Det tab))) ;

  -- NP -> NP
  IColValOne ic = ic ;
  -- ListNP -> NP
  IColValMultiple ics = mkNP and_Conj ics ;
  -- NP -> NP
  IOnlyValOne v = mkNP (mkNP the_Det D.värde_N) (mkAdv P.noPrep v) ;
  -- ListNP -> NP
  IOnlyValMultiple vs = mkNP (mkNP thePl_Det D.värde_N) (mkAdv P.noPrep (mkNP and_Conj vs)) ;

  -- NP NP -> ListNP
  BaseInsertCol ic1 ic2 = mkListNP ic1 ic2 ;
  -- NP ListNP -> ListNP
  ConsInsertCol ic ics = mkListNP ic ics ;

  -- N NP -> NP
  InsertColWith c v = mkNP (mkNP the_Det c) (mkAdv P.noPrep v) ;

  -- UPDATE -----------------------------

  -- "set the a to 1 , the b to 2 and the c to 3 for all countries where ..."
  -- N NP Adv -> Imp
  StUpdate tab upd pred = mkImp (mkVP (mkVP (P.mkV2 D.sätta_V) upd) (mkAdv for_Prep (mkNP (mkNP all_Predet (mkNP aPl_Det tab)) pred))) ;

  -- NP -> NP
  UpdateOne uc = uc ;
  -- ListNP -> NP
  UpdateMultiple ucs = mkNP and_Conj ucs ;

  -- NP NP -> ListNP
  BaseUpdateCol uc1 uc2 = mkListNP uc1 uc2 ;
  -- NP ListNP -> ListNP
  ConsUpdateCol uc ucs = mkListNP uc ucs ;

  -- N NP -> NP
  UpdateColWith c v = mkNP (mkNP the_Det c) (mkAdv to_Prep v) ;

}