concrete DatabasesEng of Databases = open SyntaxEng, (P = ParadigmsEng), SymbolicEng in {

lincat
                  -- Idea for RGL Type:
  Statement,      -- Imp
  ColumnPart,     -- {np : NP ; prep : Prep}
  FromLimPart,    -- NP
  --Limit,          -- 
  PredicatePart,  -- Adv
  Predicate,      -- S
  Column,         -- N (CN?)
  [Column],       -- ListNP
  Table,          -- N
  CompOp,         -- Prep ?
  Value,          -- NP
  [Value],        -- ListNP
  LikeOp,         -- V2
  Order,          -- 
  OrderPart,      -- 
  SortBy,         -- 
  [SortBy],       -- 
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
  -- {NP ; Prep} NP Adv _ -> Imp
  -- mkImp display_V2 (mkNP col.np (mkAdv col.prep (mkNP fromlim pred)))
  
  SColumnAll = "display all info on" ;
  -- {NP ; Prep}
  -- {np = mkNP all_Predet (mkNP info_N) ; prep = on_Prep}
  SColumnOne c = "display the" ++ c ++ "of" ;
  -- N -> {NP ; Prep}
  -- {np = (mkNP the_Det c) ; prep = possess_Prep}
  SColumnMultiple cs = "display the" ++ cs ++ "of" ;
  -- ListNP -> {NP ; Prep}
  -- {np = (mkNP and_Conj cs) ; prep = possess_Prep}

  BaseColumn c1 c2 = c1 ++ "and" ++ c2 ;
  -- N N -> ListNP
  -- mkListNP (mkNP the_Det c1) (mkNP c2)
  -- the "the" will appear in the wrong place
  ConsColumn c cs = c ++ "," ++ cs ;
  -- N ListNP -> ListNP
  -- mkListNP (mkNP c) cs

  ------- FROM, LIMIT

  --SFromTable t l = l ++ t ;

  LimNone t = "all" ++ t ;
  -- N -> NP
  -- mkNP all_Predet (mkNP t) -- (mkN "country")
  LimNum t i = "the first" ++ i.s ++ t ;
  -- N -> Int -> NP
  -- mkNP (mkDet the_Quant (mkCard (mkAdN "first") (mkCard (mkDigits i.s)))) t

  ------- WHERE

  PredNothing = "" ;
  -- Adv
  -- mkAdv (mkA "")
  PredSomething p = "where" ++ p ;
  -- S -> Adv
  -- mkAdv where_Subj p
  -- where_Subj? mkSubj "where" from MakeStructuralEng ? where_Subj in DictEng?

  PredAnd p1 p2 = p1 ++ "and" ++ p2 ;
  -- S S -> S
  -- mkS and_Conj p1 p2
  PredOr p1 p2 = p1 ++ "or" ++ p2 ;
  -- S S -> S
  -- mkS or_Conj p1 p2
  PredComp c compOp v = "the" ++ c ++ "is" ++ compOp ++ v ;
  -- N Prep NP -> S
  -- a vs an, mkS (mkCl (mkNP the_Det c) (mkAdv compOp v))
  PredIn c vs = "the" ++ c ++ "is either" ++ vs ;
  -- v1, v2, v3 or v4
  -- N ListNP -> S
  -- mkS (mkCl (mkNP the_Det c) (mkNP either7or_DConj vs))
  PredBetween c v1 v2 = "the" ++ c ++ "is between" ++ v1 ++ "and" ++ v2 ;
  -- N NP NP -> S
  -- mkS (mkCl (mkNP the_Det c) (mkAdv between_Prep (mkNP and_Conj v1 v2)))
  PredLike c op st = "the" ++ c ++ op ++ st.s ;
  -- N V2 String -> S
  -- mkS (mkCl (mkNP the_Det c) (mkVP (mkV2 (mkV "begin" "began" "begun") with_Prep) (symb st.s)))
  PredIsNull c = "the" ++ c ++ "is null" ;
  -- N -> S
  -- mkS (mkCl (mkNP the_Det c) (mkNP (mkN "null")))
  PredIsNotNull c = "the" ++ c ++ "is not null" ;
  -- N -> S
  -- mkS (mkCl (mkNP the_Det c) (mkNP not_Predet (mkNP (mkN "null"))))

  ValInt i = i.s ;
  -- Int -> NP
  -- symb i.i ?
  ValStr st = st.s ;
  -- String -> NP
  -- symb st.s

  BaseValue v1 v2 = v1 ++ "or" ++ v2 ;
  -- NP NP -> ListNP
  -- mkListNP v1 v2
  ConsValue v vs = v ++ "," ++ vs ;
  -- NP ListNP -> ListNP
  -- mkListNP v vs

  -- Prep
  CompOpEq = "" ;
  -- above_Prep
  CompOpGt = "above" ;
  -- above_Prep
  CompOpLt = "below" ;
  -- above_Prep
  CompOpGEq = "at least" ;
  -- above_Prep
  CompOpLEq = "at most" ;
  -- above_Prep
  CompOpNE = "not" ;
  -- above_Prep

  -- V2
  LikeBegins = "begins with" ;
  LikeEnds = "ends with" ;
  LikeContains = "contains" ;

  ------- ORDER BY

  OrdNothing = "" ;
  -- _
  OrdOne sb = "ordered by" ++ sb ;
  -- _ -> _
  OrdMultiple sbs = "ordered by" ++ sbs ;
  -- _ -> _

  BaseSortBy sb1 sb2 = sb1 ++ "and then" ++ sb2 ;
  -- _ _ -> _
  ConsSortBy sb sbs = sb ++ "," ++ sbs ;
  -- _ _ -> _

  SortColumn c o = c ++ o ;
  -- N _ -> _

  -- _
  OrdUnspec = "" ;
  OrdAsc = "ascending" ;
  OrdDesc = "descending" ;

  -- DELETE -----------------------------

  StDelete tab pred = "delete all" ++ tab ++ pred ;
  -- N Adv -> Imp
  -- mkImp delete_V2 (mkNP (mkNP all_Predet (mkNP tab)) pred) ;

  ------- FROM and WHERE as above

  -- INSERT -----------------------------

  StInsert tab ins = "add a new entry with" ++ ins ++ "to the table" ++ tab ;
  -- N NP -> Imp
  -- mkImp (mkVP (mkVP add_V2 (mkNP (mkNP a_Det record_N) (mkAdv with_Prep ins))) (mkAdv to_Prep (mkNP tab))) ;

  IColValOne ic = ic ;
  -- NP -> NP
  -- ic ;
  IColValMultiple ics = ics ;
  -- ListNP -> NP
  -- mkNP and_Conj ics ;
  IOnlyValOne v = "the value" ++ v ;
  -- NP -> NP
  -- mkNP (mkNP the_Det value_N) (mkAdv (mkPrep "") v)
  IOnlyValMultiple vs = "the values" ++ vs ; -- "and" instead of "or" for base case
  -- ListNP -> NP
  -- mkNP (mkNP the_Det value_N) (mkAdv (mkPrep "") (mkNP and_Conj vs))
  -- does it get that it's plural?

  BaseInsertCol ic1 ic2 = ic1 ++ "and" ++ ic2 ;
  -- NP NP -> ListNP
  -- mkListNP ic1 ic2
  ConsInsertCol ic ics = ic ++ "," ++ ics ;
  -- NP ListNP -> ListNP
  -- mkListNP ic ics

  InsertColWith c v = "the" ++ c ++ v ;
  -- N NP -> NP
  -- mkNP (mkNP the_Det c) (mkAdv (mkPrep "") v)

  -- UPDATE -----------------------------

  StUpdate tab upd pred = "set" ++ upd ++ "for all" ++ tab ++ pred ;
  -- "set the a to 1 , the b to 2 and the c to 3 for all countries where ..."
  -- N NP Adv -> Imp
  -- mkImp set_V2 (mkNP upd (mkAdv for_Prep (mkNP (mkNP all_Predet (mkNP tab)) pred)))

  UpdateOne uc = uc ;
  -- NP -> NP
  -- uc
  UpdateMultiple ucs = ucs ;
  -- ListNP -> NP
  -- mkNP and_Conj ucs

  BaseUpdateCol uc1 uc2 = uc1 ++ "and" ++ uc2 ;
  -- NP NP -> ListNP
  -- mkListNP uc1 uc2
  ConsUpdateCol uc ucs = uc ++ "," ++ ucs ;
  -- NP ListNP -> ListNP
  -- mkListNP uc ucs

  UpdateColWith c v = "the" ++ c ++ "to" ++ v ;
  -- N NP -> NP
  -- mkNP (mkNP the_Det c) (mkAdv to_Prep v) ;

}