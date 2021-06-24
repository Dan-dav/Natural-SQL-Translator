concrete DatabasesSwe of Databases = open SyntaxSwe, (P = ParadigmsSwe), SymbolicSwe, (D = MorphoDictSwe), (S = MakeStructuralSwe) in {

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
  CompOp         = {prep : Prep ; pol : Pol} ;
  LikeOp         = V2 ;
  OrderPart      = Adv ;
  SortBy         = NP ;
  [SortBy]       = ListNP ;
  Order          = A ;
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
    mkUtt (mkImp (P.mkV2 D.visa_V) q) ;
    mkUtt (mkImp (P.mkV2 D.lista_V) q) ;
    mkUtt (mkImp (P.mkV2 (P.mkV D.visa_V "mig")) q) ;
    mkUtt (mkImp (P.mkV2 (P.mkV D.ge_V "mig")) q) ;
    mkUtt (mkQCl (what_IP) q)
    } ;

  -- NP NP -> NP
  QUnion q1 q2 = mkNP and_Conj q1 q2 ;
  QUnionAll q1 q2 = mkNP (S.mkConj "och därutom") q1 q2 ;
  QIntersect q1 q2 = mkNP (S.mkConj "som också är") q1 q2 ;

  -- SELECT --------------------------------

  -- {NP ; Prep} NP Adv Adv -> NP
  QSelect col fromlim pred order = mkNP (mkNP col.np (mkAdv col.prep (mkNP fromlim pred))) order ;
  
  -- {NP ; Prep}
  SColumnAll = variants{
    {np = mkNP all_Predet (mkNP D.info_N) ; prep = P.mkPrep "om"} ;
    {np = everything_NP ; prep = P.mkPrep "om"} ;
    {np = mkNP (P.mkN "") ; prep = P.noPrep}
    } ;
  -- CN -> {NP ; Prep}
  SColumnOne c = variants {
    {np = mkNP the_Det c ; prep = possess_Prep} ;
    {np = mkNP aSg_Det c ; prep = possess_Prep} ;
    {np = mkNP thePl_Det c ; prep = possess_Prep} ;
    {np = mkNP aPl_Det c ; prep = possess_Prep}
    } ;
  -- ListNP -> {NP ; Prep}
  SColumnMultiple cs = {np = mkNP and_Conj cs ; prep = possess_Prep} ;
  -- {NP ; Prep}
  SColumnCount = {np = mkNP the_Det D.antal_N ; prep = P.noPrep} ;
  -- CN -> {NP ; Prep}
  SColumnCountCol c = {np = mkNP (mkNP the_Det D.antal_N) (mkAdv P.noPrep (mkNP aPl_Det c)) ; prep = possess_Prep} ;
  SColumnCountDistinct c = {np = mkNP (mkNP the_Det D.antal_N) (mkAdv P.noPrep (mkNP aPl_Det (mkCN D.distinkt_A c))) ; prep = possess_Prep} ;
  -- ListNP -> {NP ; Prep}
  -- not in postgres: SColumnCountDistinctMultiple cs = {np = mkNP (mkNP the_Det D.antal_N) (mkAdv possess_Prep (mkNP (mkNP aPl_Det (mkCN D.distinct_A D.set_N)) (mkAdv possess_Prep (mkNP and_Conj cs)))) ; prep = possess_Prep} ;
  -- CN -> {NP ; Prep}
  SColumnAvg c = {np = mkNP the_Det (mkCN D.genomsnittlig_A c) ; prep = possess_Prep} ;
  SColumnSum c = {np = mkNP the_Det (mkCN D.total_A c) ; prep = possess_Prep} ;

  -- CN CN -> ListNP
  BaseColumn c1 c2 = variants {
    mkListNP (mkNP the_Det c1) (mkNP the_Det c2) ;
    mkListNP (mkNP the_Det c1) (mkNP aSg_Det c2) ;
    mkListNP (mkNP the_Det c1) (mkNP thePl_Det c2) ;
    mkListNP (mkNP the_Det c1) (mkNP aPl_Det c2) ;
    mkListNP (mkNP aSg_Det c1) (mkNP the_Det c2) ;
    mkListNP (mkNP aSg_Det c1) (mkNP aSg_Det c2) ;
    mkListNP (mkNP aSg_Det c1) (mkNP thePl_Det c2) ;
    mkListNP (mkNP aSg_Det c1) (mkNP aPl_Det c2) ;
    mkListNP (mkNP thePl_Det c1) (mkNP the_Det c2) ;
    mkListNP (mkNP thePl_Det c1) (mkNP aSg_Det c2) ;
    mkListNP (mkNP thePl_Det c1) (mkNP thePl_Det c2) ;
    mkListNP (mkNP thePl_Det c1) (mkNP aPl_Det c2) ;
    mkListNP (mkNP aPl_Det c1) (mkNP the_Det c2) ;
    mkListNP (mkNP aPl_Det c1) (mkNP aSg_Det c2) ;
    mkListNP (mkNP aPl_Det c1) (mkNP thePl_Det c2) ;
    mkListNP (mkNP aPl_Det c1) (mkNP aPl_Det c2)
    } ;
  -- CN ListNP -> ListNP
  ConsColumn c cs = variants {
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

  -- {Det ; Det} CN CN -> NP
  FromJoin jt tc1 tc2 = variants {
    mkNP (mkNP aPl_Det D.rad_2_N) (mkAdv with_Prep (mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matchad med") (mkNP jt.r tc2)))) ;
    mkNP (mkNP aPl_Det D.post_2_N) (mkAdv with_Prep (mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matchad med") (mkNP jt.r tc2)))) ;
    mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matchad med") (mkNP jt.r tc2))
    } ; -- rader där a är matchad med b?
  FromJoinLim jt tc1 tc2 i = variants {
    mkNP (mkDet the_Quant (mkNum (<symb i : Card>)) (mkOrd (mkNumeral n1_Unit)))
        (mkCN D.rad_2_N (mkAdv with_Prep (mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matchad med") (mkNP jt.r tc2))))) ;
    mkNP (mkDet the_Quant (mkNum (<symb i : Card>)) (mkOrd (mkNumeral n1_Unit)))
        (mkCN D.post_2_N (mkAdv with_Prep (mkNP (mkNP jt.l tc1) (mkAdv (P.mkPrep "matchad med") (mkNP jt.r tc2)))))
    } ;

  ------- JOIN

  -- {Det ; Det} -- the_Det/thePl_Det/aPl_Det/aSg_Det
  JInner = {l = the_Det ; r = the_Det} ; -- the/a?
  JLeft = {l = every_Det ; r = the_Det} ;
  JRight = {l = the_Det ; r = every_Det} ;
  JFull = {l = every_Det ; r = every_Det} ;

  -- N CN -> CN
  JTabCol t c = variants {
    mkCN c (mkAdv possess_Prep (mkNP aSg_Det t)) ;
    mkCN c (mkAdv possess_Prep (mkNP theSg_Det t))
    } ; -- aPl_Det t

  -- CN -> CN
  ColumnTabCol tc = tc ;

  ------- WHERE

  -- Adv
  PredNothing = P.mkAdv "" ;
  -- S -> Adv
  PredSomething p = mkAdv (S.mkSubj "där") p ;

  -- S S -> S
  PredAnd p1 p2 = mkS and_Conj p1 p2 ;
  -- S S -> S
  PredOr p1 p2 = mkS or_Conj p1 p2 ;
  -- CN {Prep, Pol} NP -> S
  PredComp c compOp v = mkS compOp.pol (mkCl (mkNP the_Det c) (mkAdv compOp.prep v)) ;
  -- a vs an?
  -- v1, v2, v3 or v4
  -- CN ListNP -> S
  PredIn c vs = mkS (mkCl (mkNP the_Det c) (mkNP either7or_DConj vs)) ; -- också or?
  -- CN NP NP -> S
  PredBetween c v1 v2 = mkS (mkCl (mkNP the_Det c) (mkAdv between_Prep (mkNP and_Conj v1 v2))) ;
  -- CN V2 String -> S
  PredLike c op st = mkS (mkCl (mkNP the_Det c) (mkVP op (symb ("'" ++ st.s ++ "'")))) ;
  -- CN -> S
  PredIsNull c = mkS (mkCl (mkNP the_Det c) (P.mkA "null")) ;
  -- CN -> S
  PredIsNotNull c = mkS negativePol (mkCl (mkNP the_Det c) (P.mkA "null")) ;
  -- CN NP -> S
  PredSubQuery c q = variants {
    mkS (mkCl (mkNP the_Det c) (mkNP (mkNP (mkDet (mkNumeral n1_Unit))) (mkAdv part_Prep q))) ;
    mkS (mkCl (mkNP the_Det c) q)
    } ;

  -- Int -> NP
  ValInt i = symb i ;
  -- String -> NP
  ValStr st = symb ("'" ++ st.s ++ "'") ;

  -- NP NP -> ListNP
  BaseValue v1 v2 = mkListNP v1 v2 ;
  -- NP ListNP -> ListNP
  ConsValue v vs = mkListNP v vs ;

  -- Prep
  CompOpEq = {prep = P.noPrep ; pol = positivePol} ;
  CompOpGt = {prep = P.mkPrep "över" ; pol = positivePol} ;
  CompOpLt = {prep = under_Prep ; pol = positivePol} ;
  CompOpGEq = {prep = P.mkPrep "som minst" ; pol = positivePol} ;
  CompOpLEq = {prep = P.mkPrep "som mest" ; pol = positivePol} ;
  CompOpNE = {prep = P.noPrep ; pol = negativePol} ;

  -- V2
  LikeBegins = P.mkV2 D.börja_V with_Prep ;
  LikeEnds = P.mkV2 D.sluta_1_V with_Prep ;
  LikeContains = P.mkV2 D.innehålla_V ;

  ------- ORDER BY

  -- Adv
  OrdNothing = P.mkAdv "" ;
  -- NP -> Adv
  OrdOne sb = mkAdv (P.mkPrep "sorterade efter") sb ;
  -- ListNP -> Adv
  OrdMultiple sbs = variants {
      mkAdv (P.mkPrep "sorterade efter") (mkNP and_Conj sbs) ;
      mkAdv (P.mkPrep "sorterade efter") (mkNP (S.mkConj "och sen") sbs)
      } ; -- bara "efter" också?

  -- NP NP -> ListNP
  BaseSortBy sb1 sb2 = mkListNP sb1 sb2 ;
  -- NP ListNP -> ListNP
  ConsSortBy sb sbs = mkListNP sb sbs ;

  -- CN A -> NP
  SortColumn c o = mkNP (mkCN o c) ;
  -- mkNP (mkNP c) o ;

  -- A
  OrdUnspec = P.mkA "" ;
  OrdAsc = P.mkA "stigande" ;
  OrdDesc = variants {
      P.mkA "fallande" ;
      D.nedåtgående_A ;
      } ;

  -- DELETE --------------------------------

  -- N Adv -> Utt
  StDelete tab pred = variants {
      mkUtt (mkImp (P.mkV2 D.radera_V) (mkNP (mkNP all_Predet (mkNP aPl_Det tab)) pred)) ;
      mkUtt (mkImp (P.mkV2 (P.mkV D.ta_2_V "bort")) (mkNP (mkNP all_Predet (mkNP aPl_Det tab)) pred))
      } ;

  ------- FROM and WHERE as above

  -- INSERT --------------------------------

  -- N NP -> Utt
  StInsert tab ins = variants {
    mkUtt (mkImp (mkVP (P.mkV2 D.lägga_V to_Prep) (mkNP (mkNP aSg_Det tab) ins))) ;
    mkUtt (mkImp (mkVP (P.mkV2 D.lägga_V to_Prep) (mkNP (mkNP aPl_Det tab) ins)))
    } ;

  -- NP -> NP
  IColValOne ic = mkAdv with_Prep ic ;
  -- ListNP -> NP
  IColValMultiple ics = mkAdv with_Prep (mkNP and_Conj ics) ;
  -- NP -> NP
  IOnlyValOne v = mkAdv with_Prep (mkNP (mkNP the_Det D.värde_N) (mkAdv P.noPrep v)) ;
  -- ListNP -> NP
  IOnlyValMultiple vs = mkAdv with_Prep (mkNP (mkNP thePl_Det D.värde_N) (mkAdv P.noPrep (mkNP and_Conj vs))) ;
  -- NP -> NP
  ISubQuery q = variants {
    mkAdv with_Prep (mkNP (mkNP aPl_Det D.värde_N) (mkAdv from_Prep q)) ;
    mkAdv from_Prep q ;
    mkAdv for_Prep q
    } ;
  -- NP CN -> NP
  ISubQueryColOne q c = mkAdv with_Prep (mkNP (mkNP c) (mkAdv from_Prep q)) ;
  -- NP ListNP -> NP
  ISubQueryColMultiple q cs = mkAdv with_Prep (mkNP (mkNP and_Conj cs) (mkAdv from_Prep q)) ;

  -- NP NP -> ListNP
  BaseInsertCol ic1 ic2 = mkListNP ic1 ic2 ;
  -- NP ListNP -> ListNP
  ConsInsertCol ic ics = mkListNP ic ics ;

  -- CN NP -> NP
  InsertColWith c v = variants {
    mkNP (mkNP the_Det c) (mkAdv P.noPrep v) ;
    mkNP (mkNP the_Det c) (mkAdv possess_Prep v) ;
    mkNP (mkNP the_Det c) (mkAdv on_Prep v) ;
    mkNP (mkNP the_Det c) (mkAdv (P.mkPrep "satt till") v) ;
    mkNP (mkNP aSg_Det c) (mkAdv possess_Prep v) ;
    mkNP (mkNP aSg_Det c) (mkAdv on_Prep v) ;
    mkNP (mkNP aSg_Det c) (mkAdv (P.mkPrep "som är") v) ;
    } ;

  -- UPDATE --------------------------------

  -- "set the a to 1 , the b to 2 and the c to 3 for all countries where ..."
  -- N NP Adv -> Utt
  StUpdate tab upd pred = mkUtt (mkImp (mkVP (mkVP (P.mkV2 D.sätta_V) upd) (mkAdv for_Prep (mkNP (mkNP all_Predet (mkNP aPl_Det tab)) pred)))) ;

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
    mkNP (mkNP the_Det c) (mkAdv P.noPrep v) ;
    mkNP (mkNP the_Det c) (mkAdv to_Prep v) ;
    mkNP (mkNP the_Det c) (mkAdv (P.mkPrep "som") v) ;
    mkNP (mkNP the_Det c) (mkAdv on_Prep v) ;
    mkNP (mkNP aSg_Det c) (mkAdv on_Prep v) ;
    mkNP (mkNP aSg_Det c) (mkAdv possess_Prep v) ;
    mkNP (mkNP aSg_Det c) (mkAdv (P.mkPrep "som är") v)
    } ;
    -- namnet 'test', namnet till 'test', valutan som 'euro', befolkningen på 5 mil
    -- en befolkning på 5 mil, en befolkning av 55, en befolkning som är 45
}