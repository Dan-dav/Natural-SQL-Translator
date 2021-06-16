incomplete concrete DatabasesFunctor of Databases = open Syntax, Symbolic in {

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

-- parameters
--  show_V2  -- (P.mkV2 {D.display_V | D.show_2_V}) ;
--  infor_N
--  about_Prep
--  where_Subj
--  null_A
--  begin_V2
--  end_V2
--  contain_V2
--  delete_V2
--  add_V2
--  row_N
--  value_N
--  set_V2 
--  at_least_Prep
--  at_most_Prep
--  not_Prep
--  ordered_by_Prep
--  ascending_Adv
--  descending_Adv

-- exceptions in Swe
--  LimNone
--  

lin
  -- SELECT -----------------------------

  -- {NP ; Prep} NP Adv Adv -> Imp
  StSelect col fromlim pred order = mkImp (mkVP (mkVP show_V2 (mkNP col.np (mkAdv col.prep (mkNP fromlim pred)))) order) ;
  
  -- {NP ; Prep}
  SColumnAll = {np = mkNP all_Predet (mkNP info_N) ; prep = about_Prep} ;
  -- N -> {NP ; Prep}
  SColumnOne c = {np = mkNP the_Det c ; prep = possess_Prep} ;
  -- ListNP -> {NP ; Prep}
  SColumnMultiple cs = {np = mkNP and_Conj cs ; prep = possess_Prep} ;

  -- N N -> ListNP
  BaseColumn c1 c2 = mkListNP (mkNP the_Det c1) (mkNP the_Det c2) ;
  -- only have one "the" in the beginning?
  -- N ListNP -> ListNP
  ConsColumn c cs = mkListNP (mkNP the_Det c) cs ;

  ------- FROM, LIMIT

  --SFromTable t l = l ++ t ;

  -- N -> NP
  LimNone t = mkNP all_Predet (mkNP t) ;
  -- N -> Int -> NP
  -- LimNum t i = mkNP (mkDet the_Quant (mkNum i.s) (mkOrd (mkNumeral n1_Unit))) t ;
  -- mkDet the_Quant num (mkOrd n1_Dig)
  -- (the first 5/five countries)/(the 5/five first countries)

  ------- WHERE

  -- Adv
  PredNothing = P.mkAdv "" ;
  -- S -> Adv
  PredSomething p = mkAdv where_Subj p ;

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
  PredIsNull c = mkS (mkCl (mkNP the_Det c) null_A) ;
  -- N -> S
  PredIsNotNull c = mkS negativePol (mkCl (mkNP the_Det c) null_A) ;

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
  CompOpGEq = at_least_Prep ;
  CompOpLEq = at_most_Prep ;
  CompOpNE = not_Prep ;

  -- V2
  LikeBegins = begin_V2 ;
  LikeEnds = end_V2 ;
  LikeContains = contain_V2 ;

  ------- ORDER BY

  -- Adv
  OrdNothing = P.mkAdv "" ;
  -- NP -> Adv
  OrdOne sb = mkAdv ordered_by_Prep sb ;
  -- ListNP -> Adv
  OrdMultiple sbs = mkAdv ordered_by_Prep (mkNP and_Conj sbs) ;
  -- Can I have "and then" as a Conj instead?

  -- NP NP -> ListNP
  BaseSortBy sb1 sb2 = mkListNP sb1 sb2 ;
  -- NP ListNP -> ListNP
  ConsSortBy sb sbs = mkListNP sb sbs ;

  -- N Adv -> NP
  SortColumn c o = mkNP (mkNP c) o ;

  -- Adv
  OrdUnspec = P.mkAdv "" ;
  OrdAsc = ascending_Adv ;
  OrdDesc = descending_Adv ;

  -- DELETE -----------------------------

  -- N Adv -> Imp
  StDelete tab pred = mkImp delete_V2 (mkNP (mkNP all_Predet (mkNP tab)) pred) ;

  ------- FROM and WHERE as above

  -- INSERT -----------------------------

  -- N NP -> Imp
  StInsert tab ins = mkImp (mkVP (mkVP add_V2 (mkNP (mkNP a_Det row_N) (mkAdv with_Prep ins))) (mkAdv to_Prep (mkNP tab))) ;

  -- NP -> NP
  IColValOne ic = ic ;
  -- ListNP -> NP
  IColValMultiple ics = mkNP and_Conj ics ;
  -- NP -> NP
  IOnlyValOne v = mkNP (mkNP the_Det value_N) (mkAdv P.noPrep v) ;
  -- ListNP -> NP
  IOnlyValMultiple vs = mkNP (mkNP thePl_Det value_N) (mkAdv P.noPrep (mkNP and_Conj vs)) ;

  -- NP NP -> ListNP
  BaseInsertCol ic1 ic2 = mkListNP ic1 ic2 ;
  -- NP ListNP -> ListNP
  ConsInsertCol ic ics = mkListNP ic ics ;

  -- N NP -> NP
  InsertColWith c v = mkNP (mkNP the_Det c) (mkAdv P.noPrep v) ;

  -- UPDATE -----------------------------

  -- "set the a to 1 , the b to 2 and the c to 3 for all countries where ..."
  -- N NP Adv -> Imp
  StUpdate tab upd pred = mkImp (mkVP (mkVP set_V2 upd) (mkAdv for_Prep (mkNP (mkNP all_Predet (mkNP tab)) pred))) ;

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