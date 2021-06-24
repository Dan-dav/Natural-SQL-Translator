concrete DatabasesEng of Databases = open SyntaxEng, (P = ParadigmsEng), SymbolicEng, (D = MorphoDictEng) in {

lincat
  Statement      = Utt ;
  Query          = NP ;
  ColumnPart     = {np : NP ; prep : Prep} ;
  FromLimPart    = NP ;
  JoinType       = {l : Det ; r : Det} ;
  TabCol         = CN ;
  Column         = CN ;
  [Column]       = ListNP ;
  PredicatePart  = Adv ;
  Predicate      = S ;
  Value          = NP ;
  [Value]        = ListNP ;
  CompOp         = Prep ;
  LikeOp         = V2 ;
  OrderPart      = Adv ;
  SortBy         = NP ;
  [SortBy]       = ListNP ;
  Order          = Adv ;
  InsertPart     = Adv ;
  InsertCol      = NP ;
  [InsertCol]    = ListNP ;
  UpdatePart     = NP ;
  UpdateCol      = NP ;
  [UpdateCol]    = ListNP ;
  Table          = N ;

lin
  -- Query statements, UNION, INTERSECT ----

  -- NP -> Utt
  StQuery q = variants {
    mkUtt q ;
    mkUtt (mkImp (P.mkV2 D.display_V) q) ;
    mkUtt (mkImp (P.mkV2 D.show_2_V) q) ;
    mkUtt (mkImp (P.mkV2 D.list_V) q) ;
    mkUtt (mkImp (P.mkV2 (P.partV D.show_2_V "me")) q) ;
    mkUtt (mkImp (P.mkV2 (P.partV D.give_1_V "me")) q) ;
    mkUtt (mkQCl (whatSg_IP) q) ;
    mkUtt (mkQCl (whatPl_IP) q) ;
    mkUtt (mkQCl (whoSg_IP) q) ;
    mkUtt (mkQCl (whoPl_IP) q)
    } ;

  -- NP NP -> NP
  QUnion q1 q2 = mkNP and_Conj q1 q2 ;
  QUnionAll q1 q2 = mkNP (P.mkConj "as well as") q1 q2 ;
  QIntersect q1 q2 = mkNP (P.mkConj "which are also") q1 q2 ;

  -- SELECT --------------------------------

  -- {NP ; Prep} NP Adv Adv -> NP
  QSelect col fromlim pred order = mkNP (mkNP col.np (mkAdv col.prep (mkNP fromlim pred))) order ;
  
  -- {NP ; Prep}
  SColumnAll = variants{
    {np = mkNP all_Predet (mkNP D.info_N) ; prep = P.mkPrep "about"} ;
    {np = everything_NP ; prep = P.mkPrep "about"} ;
    {np = mkNP (P.mkN "") ; prep = P.noPrep}
    } ;
  -- CN -> {NP ; Prep}
  SColumnOne c = variants {
    {np = mkNP the_Det c ; prep = possess_Prep} ;
    {np = mkNP aSg_Det c ; prep = possess_Prep} ;
    {np = mkNP thePl_Det c ; prep = possess_Prep} ;
    {np = mkNP aPl_Det c ; prep = possess_Prep} ;
    {np = mkNP the_Det c ; prep = for_Prep} ;
    {np = mkNP aSg_Det c ; prep = for_Prep} ;
    {np = mkNP thePl_Det c ; prep = for_Prep} ;
    {np = mkNP aPl_Det c ; prep = for_Prep}
    } ;
  -- ListNP -> {NP ; Prep}
  SColumnMultiple cs = {np = mkNP and_Conj cs ; prep = possess_Prep} ;
  -- {NP ; Prep}
  SColumnCount = {np = mkNP the_Det D.number_N ; prep = possess_Prep} ;
  -- CN -> {NP ; Prep}
  SColumnCountCol c = {np = mkNP (mkNP the_Det D.number_N) (mkAdv possess_Prep (mkNP aPl_Det c)) ; prep = possess_Prep} ;
  SColumnCountDistinct c = {np = mkNP (mkNP the_Det D.number_N) (mkAdv possess_Prep (mkNP aPl_Det (mkCN D.distinct_A c))) ; prep = possess_Prep} ;
  -- ListNP -> {NP ; Prep}
  -- not in postgres: SColumnCountDistinctMultiple cs = {np = mkNP (mkNP the_Det D.number_N) (mkAdv possess_Prep (mkNP (mkNP aPl_Det (mkCN D.distinct_A D.set_N)) (mkAdv possess_Prep (mkNP and_Conj cs)))) ; prep = possess_Prep} ;
  -- CN -> {NP ; Prep}
  SColumnAvg c = variants {
    {np = mkNP the_Det (mkCN D.average_A c) ; prep = possess_Prep} ;
    {np = mkNP the_Det (mkCN D.mean_A c) ; prep = possess_Prep}
    } ;
  SColumnSum c = {np = mkNP the_Det (mkCN D.total_A c) ; prep = possess_Prep} ;

  -- CN CN -> ListNP
  BaseColumn c1 c2 = variants {
    mkListNP (mkNP c1) (mkNP c2) ;
    mkListNP (mkNP c1) (mkNP the_Det c2) ;
    mkListNP (mkNP the_Det c1) (mkNP c2) ;
    mkListNP (mkNP the_Det c1) (mkNP the_Det c2) ;
    mkListNP (mkNP thePl_Det c1) (mkNP thePl_Det c2) ;
    mkListNP (mkNP thePl_Det c1) (mkNP aPl_Det c2) ;
    mkListNP (mkNP aPl_Det c1) (mkNP aPl_Det c2)
    } ;
  -- CN ListNP -> ListNP
  ConsColumn c cs = variants {
    mkListNP (mkNP c) cs ;
    mkListNP (mkNP the_Det c) cs ;
    mkListNP (mkNP aSg_Det c) cs ;
    mkListNP (mkNP thePl_Det c) cs ;
    mkListNP (mkNP aPl_Det c) cs
    } ;

  ------- FROM, LIMIT

  -- N -> NP
  FromTab t = variants {
    mkNP all_Predet (mkNP aPl_Det t) ;
    mkNP thePl_Det t ;
    mkNP aPl_Det t ;
    mkNP theSg_Det t ;
    mkNP aSg_Det t ;
    mkNP every_Det t
    } ;
  -- N -> Int -> NP
  FromTabLim t i = variants {
    mkNP (mkDet the_Quant (mkNum (<symb i : Card>)) (mkOrd (mkNumeral n1_Unit))) t ;
    mkNP (<symb i : Card>) t
    } ;
  -- support (the first 5/five countries)/(the 5/five first countries)?

  -- {Det ; Det} CN CN -> NP
  FromJoin jt tc1 tc2 = variants {
    mkNP (mkNP aPl_Det D.row_N) (mkAdv with_Prep (mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matched with") (mkNP jt.r tc2)))) ;
    mkNP (mkNP aPl_Det D.entry_N) (mkAdv with_Prep (mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matched with") (mkNP jt.r tc2)))) ;
    mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matched with") (mkNP jt.r tc2))
    } ;
  FromJoinLim jt tc1 tc2 i = variants {
    mkNP (mkDet the_Quant (mkNum (<symb i : Card>)) (mkOrd (mkNumeral n1_Unit)))
      (mkCN D.row_N (mkAdv with_Prep (mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matched with") (mkNP jt.r tc2))))) ;
    mkNP (mkDet the_Quant (mkNum (<symb i : Card>)) (mkOrd (mkNumeral n1_Unit)))
      (mkCN D.entry_N (mkAdv with_Prep (mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matched with") (mkNP jt.r tc2)))))
    } ;

  ------- JOIN

  -- {Det ; Det} -- the_Det/thePl_Det/aPl_Det/aSg_Det
  JInner = {l = the_Det ; r = the_Det} ; -- the or a?
  JLeft = {l = every_Det ; r = the_Det} ;
  JRight = {l = the_Det ; r = every_Det} ;
  JFull = {l = every_Det ; r = every_Det} ;

  -- N CN -> CN
  JTabCol t c = variants {
    mkCN c (mkAdv possess_Prep (mkNP aSg_Det t)) ; -- aPl_Det t
    mkCN c (mkAdv possess_Prep (mkNP theSg_Det t))
    } ;

  -- CN -> CN
  ColumnTabCol tc = tc ;

  ------- WHERE

  -- Adv
  PredNothing = P.mkAdv "" ;
  -- S -> Adv
  PredSomething p = variants {
    mkAdv (P.mkSubj "where") p ;
    mkAdv (P.mkSubj "whose") p ;
    } ;

  -- S S -> S
  PredAnd p1 p2 = mkS and_Conj p1 p2 ;
  -- S S -> S
  PredOr p1 p2 = mkS or_Conj p1 p2 ;
  -- CN Prep NP -> S
  PredComp c compOp v = variants {
    mkS (mkCl (mkNP the_Det c) (mkAdv compOp v)) ;
    mkS (mkCl (mkNP c) (mkAdv compOp v)) ;
    mkS (mkCl (mkNP aPl_Det c) (mkAdv compOp v))
    } ;
  -- a vs an?
  -- v1, v2, v3 or v4
  -- CN ListNP -> S
  PredIn c vs = mkS (mkCl (mkNP the_Det c) (mkNP either7or_DConj vs)) ; -- also support "or"?
  -- CN NP NP -> S
  PredBetween c v1 v2 = mkS (mkCl (mkNP the_Det c) (mkAdv between_Prep (mkNP and_Conj v1 v2))) ;
  -- CN V2 String -> S
  PredLike c op st = variants {
    mkS (mkCl (mkNP the_Det c) (mkVP op (symb ("'" ++ st.s ++ "'")))) ;
    mkS (mkCl (mkNP c) (mkVP op (symb ("'" ++ st.s ++ "'")))) ;
    mkS (mkCl (mkNP aPl_Det c) (mkVP op (symb ("'" ++ st.s ++ "'"))))
    } ;
  -- CN -> S
  PredIsNull c = mkS (mkCl (mkNP the_Det c) D.null_A) ;
  -- CN -> S
  PredIsNotNull c = mkS negativePol (mkCl (mkNP the_Det c) D.null_A) ;
  -- CN NP -> S
  PredSubQuery c q = variants {
    mkS (mkCl (mkNP the_Det c) (mkNP (mkNP (mkDet (mkNumeral n1_Unit))) (mkAdv part_Prep q))) ;
    mkS (mkCl (mkNP the_Det c) q)
    } ;
  -- the c is one of q
  -- "the countries where (the capital is one of the names of all countries)"
  -- "the countries where (the capital is the name of a country)"

  -- Int -> NP
  ValInt i = symb i ;
  -- String -> NP
  ValStr st = symb ("'" ++ st.s ++ "'") ;

  -- NP NP -> ListNP
  BaseValue v1 v2 = mkListNP v1 v2 ;
  -- NP ListNP -> ListNP
  ConsValue v vs = mkListNP v vs ;

  -- Prep
  CompOpEq = P.noPrep ;
  CompOpGt = variants {
    above_Prep ;
    P.mkPrep "over"
    } ;
  CompOpLt = under_Prep ;
  CompOpGEq = P.mkPrep "at least" ;
  CompOpLEq = P.mkPrep "at most" ;
  CompOpNE = P.mkPrep "not" ;

  -- V2
  LikeBegins = variants {
    P.mkV2 D.begin_1_V with_Prep ;
    P.mkV2 D.start_V with_Prep
    } ;
  LikeEnds = P.mkV2 D.end_V with_Prep ;
  LikeContains = P.mkV2 D.contain_V ;

  ------- ORDER BY

  -- Adv
  OrdNothing = P.mkAdv "" ;
  -- NP -> Adv
  OrdOne sb = mkAdv (P.mkPrep "ordered by") sb ;
  -- ListNP -> Adv
  OrdMultiple sbs = variants {
    mkAdv (P.mkPrep "ordered by") (mkNP and_Conj sbs) ;
    mkAdv (P.mkPrep "ordered by") (mkNP (P.mkConj "and then") sbs)
    } ;

  -- NP NP -> ListNP
  BaseSortBy sb1 sb2 = mkListNP sb1 sb2 ;
  -- NP ListNP -> ListNP
  ConsSortBy sb sbs = mkListNP sb sbs ;

  -- CN Adv -> NP
  SortColumn c o = mkNP (mkNP c) o ;

  -- Adv
  OrdUnspec = P.mkAdv "" ; -- swap these for A?, have them before column?
  OrdAsc = variants {
    P.mkAdv "ascending" ;
    P.mkAdv "rising"
    } ;
  OrdDesc = variants {
    P.mkAdv "descending" ;
    P.mkAdv "falling"
    } ;

  -- DELETE --------------------------------

  -- N Adv -> Utt
  StDelete tab pred = mkUtt (mkImp (P.mkV2 D.delete_V) (mkNP (mkNP all_Predet (mkNP aPl_Det tab)) pred)) ;

  ------- FROM and WHERE as above

  -- INSERT --------------------------------

  -- N NP -> Utt
  StInsert tab ins = variants { -- adding thePl_Det?
    mkUtt (mkImp (mkVP (P.mkV2 D.add_V) (mkNP (mkNP aSg_Det tab) ins))) ;
    mkUtt (mkImp (mkVP (P.mkV2 D.add_V) (mkNP (mkNP aPl_Det tab) ins)))
    } ;

  -- NP -> NP
  IColValOne ic = mkAdv with_Prep ic ;
  -- ListNP -> NP
  IColValMultiple ics = mkAdv with_Prep (mkNP and_Conj ics) ;
  -- NP -> NP
  IOnlyValOne v = mkAdv with_Prep (mkNP (mkNP the_Det D.value_N) (mkAdv P.noPrep v)) ;
  -- ListNP -> NP
  IOnlyValMultiple vs = mkAdv with_Prep (mkNP (mkNP thePl_Det D.value_N) (mkAdv P.noPrep (mkNP and_Conj vs))) ;
  -- NP -> NP
  ISubQuery q = variants {
    mkAdv with_Prep (mkNP (mkNP aPl_Det D.value_N) (mkAdv from_Prep q)) ; -- with values from q
    mkAdv from_Prep q ;
    mkAdv for_Prep q
    } ;
  -- NP CN -> NP
  ISubQueryColOne q c = variants { -- with/by c from q, add plural and singular, the/a
    mkAdv by8means_Prep (mkNP (mkNP c) (mkAdv from_Prep q)) ;
    mkAdv with_Prep (mkNP (mkNP c) (mkAdv from_Prep q))
    } ;
  -- NP ListNP -> NP
  ISubQueryColMultiple q cs = variants { -- with/by cs from q
    mkAdv by8means_Prep (mkNP (mkNP and_Conj cs) (mkAdv from_Prep q)) ;
    mkAdv with_Prep (mkNP (mkNP and_Conj cs) (mkAdv from_Prep q))
    } ;
  -- some ideas for formulations to support:
  -- "take the currencies where the name is Krona and add them to the name and continent of countries"
  -- "add the currencies where the name is Krona to the name and continent of countries"
  -- "add countries with/by name from the currencies where the name is Krona"
  -- "add countries with/by name and continent from the currencies where the name is Krona"
  -- "add countries (with values) from the currencies where the name is Krona"

  -- NP NP -> ListNP
  BaseInsertCol ic1 ic2 = mkListNP ic1 ic2 ;
  -- NP ListNP -> ListNP
  ConsInsertCol ic ics = mkListNP ic ics ;

  -- CN NP -> NP
  InsertColWith c v = variants { -- the c v | the c of v | the c set to v | the c at v | the c as v
    mkNP (mkNP the_Det c) (mkAdv P.noPrep v) ;
    mkNP (mkNP the_Det c) (mkAdv possess_Prep v) ;
    mkNP (mkNP the_Det c) (mkAdv (P.mkPrep "set to") v) ;
    mkNP (mkNP the_Det c) (mkAdv (P.mkPrep "at") v) ;
    mkNP (mkNP the_Det c) (mkAdv (P.mkPrep "as") v) ;
    mkNP (mkNP aSg_Det c) (mkAdv possess_Prep v) ;
    mkNP (mkNP aSg_Det c) (mkAdv (P.mkPrep "which is") v) ;
    } ;

  -- UPDATE --------------------------------

  -- "set the a to 1 , the b to 2 and the c to 3 for all countries where ..."
  -- N NP Adv -> Utt
  StUpdate tab upd pred = mkUtt (mkImp (mkVP (mkVP (P.mkV2 D.set_1_V) upd) (mkAdv for_Prep (mkNP (mkNP all_Predet (mkNP aPl_Det tab)) pred)))) ;

  -- NP -> NP
  UpdateOne uc = uc ;
  -- ListNP -> NP
  UpdateMultiple ucs = mkNP and_Conj ucs ;

  -- NP NP -> ListNP
  BaseUpdateCol uc1 uc2 = mkListNP uc1 uc2 ;
  -- NP ListNP -> ListNP
  ConsUpdateCol uc ucs = mkListNP uc ucs ;

  -- CN NP -> NP
  UpdateColWith c v = variants {
    mkNP (mkNP the_Det c) (mkAdv to_Prep v) ;
    mkNP (mkNP the_Det c) (mkAdv (P.mkPrep "at") v) ;
    mkNP (mkNP the_Det c) (mkAdv (P.mkPrep "as") v) ;
    mkNP (mkNP aSg_Det c) (mkAdv possess_Prep v)
    } ;
  -- the name to china, the name as china, the population at 5, a population of 5
}