concrete DatabasesEng of Databases = open SyntaxEng, (P = ParadigmsEng), SymbolicEng, (D = DictEng) in {

lincat
                  -- Idea for RGL Type:
  Statement,      -- Imp
  ColumnPart,     -- {np : NP ; prep : Prep}
  FromLimPart,    -- NP
  PredicatePart,  -- Adv
  Predicate,      -- S
  Column,         -- N (CN?)
  [Column],       -- ListNP
  Table,          -- N
  CompOp,         -- Prep
  Value,          -- NP
  [Value],        -- ListNP
  LikeOp,         -- V2
  Order,          -- Adv
  OrderPart,      -- Adv
  SortBy,         -- NP
  [SortBy],       -- ListNP
  InsertPart,     -- NP
  InsertCol,      -- NP
  [InsertCol],    -- ListNP
  UpdatePart,     -- NP
  UpdateCol,      -- NP
  [UpdateCol]     -- ListNP
  = Str ;

lin
  -- SELECT -----------------------------

  StSelect col fromlim pred order = col ++ fromlim ++ pred ++ order ;
  -- {NP ; Prep} NP Adv Adv -> Imp
  -- StSelect col fromlim pred order = mkImp (mkVP (mkVP display_V2 (mkNP col.np (mkAdv col.prep (mkNP fromlim pred)))) order) ;
  
  SColumnAll = "display all info on" ;
  -- {NP ; Prep}
  -- SColumnAll = {np = mkNP all_Predet (mkNP info_N) ; prep = on_Prep} ;
  SColumnOne c = "display the" ++ c ++ "of" ;
  -- N -> {NP ; Prep}
  -- SColumnOne c = {np = mkNP the_Det c ; prep = possess_Prep} ;
  SColumnMultiple cs = "display the" ++ cs ++ "of" ;
  -- ListNP -> {NP ; Prep}
  -- SColumnMultiple cs = {np = mkNP and_Conj cs ; prep = possess_Prep} ;

  BaseColumn c1 c2 = c1 ++ "and" ++ c2 ;
  -- N N -> ListNP
  -- BaseColumn c1 c2 = mkListNP (mkNP the_Det c1) (mkNP c2) ;
  -- the "the" will appear in the wrong place?
  ConsColumn c cs = c ++ "," ++ cs ;
  -- N ListNP -> ListNP
  -- ConsColumn c cs = mkListNP (mkNP c) cs ;

  ------- FROM, LIMIT

  --SFromTable t l = l ++ t ;

  LimNone t = "all" ++ t ;
  -- N -> NP
  -- LimNone t = mkNP all_Predet (mkNP t) ;
  LimNum t i = "the first" ++ i.s ++ t ;
  -- N -> Int -> NP
  -- LimNum t i = mkNP (mkDet the_Quant (mkCard (mkAdN "first") (mkCard (mkDigits i.s)))) t ;

  ------- WHERE

  PredNothing = "" ;
  -- Adv
  -- PredNothing = mkAdv "" ;
  PredSomething p = "where" ++ p ;
  -- S -> Adv
  -- PredSomething p = mkAdv D.where_Subj p ;

  PredAnd p1 p2 = p1 ++ "and" ++ p2 ;
  -- S S -> S
  -- PredAnd p1 p2 = mkS and_Conj p1 p2 ;
  PredOr p1 p2 = p1 ++ "or" ++ p2 ;
  -- S S -> S
  -- PredOr p1 p2 = mkS or_Conj p1 p2 ;
  PredComp c compOp v = "the" ++ c ++ "is" ++ compOp ++ v ;
  -- N Prep NP -> S
  -- PredComp c compOp v = mkS (mkCl (mkNP the_Det c) (mkAdv compOp v)) ;
  -- a vs an?
  PredIn c vs = "the" ++ c ++ "is either" ++ vs ;
  -- v1, v2, v3 or v4
  -- N ListNP -> S
  -- PredIn c vs = mkS (mkCl (mkNP the_Det c) (mkNP either7or_DConj vs)) ;
  PredBetween c v1 v2 = "the" ++ c ++ "is between" ++ v1 ++ "and" ++ v2 ;
  -- N NP NP -> S
  -- PredBetween c v1 v2 = mkS (mkCl (mkNP the_Det c) (mkAdv between_Prep (mkNP and_Conj v1 v2))) ;
  PredLike c op st = "the" ++ c ++ op ++ st.s ;
  -- N V2 String -> S
  -- PredLike c op st = mkS (mkCl (mkNP the_Det c) (mkVP op (symb st.s))) ;
  PredIsNull c = "the" ++ c ++ "is null" ;
  -- N -> S
  -- PredIsNull c = mkS (mkCl (mkNP the_Det c) D.null_A) ;
  PredIsNotNull c = "the" ++ c ++ "is not null" ;
  -- N -> S
  -- PredIsNotNull c = mkS negativePol (mkCl (mkNP the_Det c) D.null_A) ;

  ValInt i = i.s ;
  -- Int -> NP
  -- ValInt i = symb i ;
  ValStr st = st.s ;
  -- String -> NP
  -- ValStr st = symb st ;

  BaseValue v1 v2 = v1 ++ "or" ++ v2 ;
  -- NP NP -> ListNP
  -- BaseValue v1 v2 = mkListNP v1 v2 ;
  ConsValue v vs = v ++ "," ++ vs ;
  -- NP ListNP -> ListNP
  -- ConsValue v vs = mkListNP v vs ;

  -- Prep
  CompOpEq = "" ;
  -- CompOpEq = noPrep ;
  CompOpGt = "above" ;
  -- CompOpGt = above_Prep ;
  CompOpLt = "below" ;
  -- CompOpLt = under_Prep ;
  CompOpGEq = "at least" ;
  -- CompOpGEq = mkPrep "at least" ;
  CompOpLEq = "at most" ;
  -- CompOpLEq = mkPrep "at most" ;
  CompOpNE = "not" ;
  -- CompOpNE = mkPrep "not" ;

  -- V2
  LikeBegins = "begins with" ;
  -- P.mkV2 D.begin_V with_Prep ;
  LikeEnds = "ends with" ;
  -- P.mkV2 D.end_V with_Prep ;
  LikeContains = "contains" ;
  -- D.contain_V2 ;

  ------- ORDER BY

  OrdNothing = "" ;
  -- Adv
  -- OrdNothing = mkAdv "" ;
  OrdOne sb = "ordered by" ++ sb ;
  -- NP -> Adv
  -- OrdOne sb = mkAdv (P.mkPrep "ordered by") sb ;
  OrdMultiple sbs = "ordered by" ++ sbs ;
  -- ListNP -> Adv
  -- OrdMultiple sbs = mkAdv (P.mkPrep "ordered by") (mkNP and_Conj sbs) ;
  -- Can I have "and then" as a Conj instead?

  BaseSortBy sb1 sb2 = sb1 ++ "and then" ++ sb2 ;
  -- NP NP -> ListNP
  -- BaseSortBy sb1 sb2 = mkListNP sb1 sb2 ;
  ConsSortBy sb sbs = sb ++ "," ++ sbs ;
  -- NP ListNP -> ListNP
  -- ConsSortBy sb sbs = mkListNP sb sbs ;

  SortColumn c o = c ++ o ;
  -- N Adv -> NP
  -- SortColumn c o = mkNP (mkNP c) o ;

  -- Adv
  OrdUnspec = "" ;
  -- OrdUnspec = mkAdv "" ;
  OrdAsc = "ascending" ;
  -- OrdAsc = mkAdv "ascending" ;
  OrdDesc = "descending" ;
  -- OrdDesc = mkAdv "descending" ;

  -- DELETE -----------------------------

  StDelete tab pred = "delete all" ++ tab ++ pred ;
  -- N Adv -> Imp
  -- StDelete tab pred = mkImp delete_V2 (mkNP (mkNP all_Predet (mkNP tab)) pred) ;

  ------- FROM and WHERE as above

  -- INSERT -----------------------------

  StInsert tab ins = "add a new entry with" ++ ins ++ "to the table" ++ tab ;
  -- N NP -> Imp
  -- StInsert tab ins = mkImp (mkVP (mkVP add_V2 (mkNP (mkNP a_Det record_N) (mkAdv with_Prep ins))) (mkAdv to_Prep (mkNP tab))) ;

  IColValOne ic = ic ;
  -- NP -> NP
  -- IColValOne ic = ic ;
  IColValMultiple ics = ics ;
  -- ListNP -> NP
  -- IColValMultiple ics = mkNP and_Conj ics ;
  IOnlyValOne v = "the value" ++ v ;
  -- NP -> NP
  -- IOnlyValOne v = mkNP (mkNP the_Det value_N) (mkAdv noPrep v) ;
  IOnlyValMultiple vs = "the values" ++ vs ;
  -- "and" instead of "or" for base case
  -- ListNP -> NP
  -- IOnlyValMultiple vs = mkNP (mkNP the_Det value_N) (mkAdv noPrep (mkNP and_Conj vs)) ;
  -- does it get that it's plural?

  BaseInsertCol ic1 ic2 = ic1 ++ "and" ++ ic2 ;
  -- NP NP -> ListNP
  -- BaseInsertCol ic1 ic2 = mkListNP ic1 ic2 ;
  ConsInsertCol ic ics = ic ++ "," ++ ics ;
  -- NP ListNP -> ListNP
  -- ConsInsertCol ic ics = mkListNP ic ics ;

  InsertColWith c v = "the" ++ c ++ v ;
  -- N NP -> NP
  -- InsertColWith c v = mkNP (mkNP the_Det c) (mkAdv noPrep v) ;

  -- UPDATE -----------------------------

  StUpdate tab upd pred = "set" ++ upd ++ "for all" ++ tab ++ pred ;
  -- "set the a to 1 , the b to 2 and the c to 3 for all countries where ..."
  -- N NP Adv -> Imp
  -- StUpdate tab upd pred = mkImp (mkVP (mkVP set_V2 upd) (mkAdv for_Prep (mkNP (mkNP all_Predet (mkNP tab)) pred))) ;

  UpdateOne uc = uc ;
  -- NP -> NP
  -- UpdateOne uc = uc ;
  UpdateMultiple ucs = ucs ;
  -- ListNP -> NP
  -- UpdateMultiple ucs = mkNP and_Conj ucs ;

  BaseUpdateCol uc1 uc2 = uc1 ++ "and" ++ uc2 ;
  -- NP NP -> ListNP
  -- BaseUpdateCol uc1 uc2 = mkListNP uc1 uc2 ;
  ConsUpdateCol uc ucs = uc ++ "," ++ ucs ;
  -- NP ListNP -> ListNP
  -- ConsUpdateCol uc ucs = mkListNP uc ucs ;

  UpdateColWith c v = "the" ++ c ++ "to" ++ v ;
  -- N NP -> NP
  -- UpdateColWith c v = mkNP (mkNP the_Det c) (mkAdv to_Prep v) ;

}