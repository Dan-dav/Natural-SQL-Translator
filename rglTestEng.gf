concrete rglTestEng of rglTest = open SyntaxEng, (P = ParadigmsEng), SymbolicEng, (D = DictEng) in {

lincat
    Sentence = Utt ;
    Mod = AdA ;
    Qual = A ;
    Predicate = S ;
    Column = N ;
    Value = NP ;
    [Value] = ListNP ;
    LikeOp = V2 ;
    SortBy = NP ;
    Order = V2 ;
    --FromLim = NP ;
    --From = NP ;
    --Lim = NP => NP ;
    CompOp = Prep ;

lin
    -- App m q = mkUtt (mkAP m q) ;
    -- PS p = mkUtt p ;
    -- SortS sb = mkUtt sb ;
    -- AllS = mkUtt (mkNP (mkDet the_Quant (mkCard (ParadigmsEng.mkAdN "first") (mkCard (mkDigits "1")))) (mkCN (P.mkN "country"))) ;
    -- FLS fl = mkUtt fl ;
    -- TestS = mkUtt (mkS negativePol (mkCl she_NP (SyntaxEng.mkAdv (mkCAdv "more" "lal" "than") (mkA "warm") he_NP))) ;
    -- TestS = mkUtt (mkCard (SyntaxEng.mkAdN (mkCAdv "more" "lal" "than")) (mkCard (mkNumeral n8_Unit))) ;
    -- mkVP (SyntaxEng.mkAdv (mkCAdv "more" "lal" "than") (mkA "warm") he_NP)
    -- there are more than five people
    -- TestS = mkUtt (mkS negativePol (mkCl they_NP (mkNP (mkCard (SyntaxEng.mkAdN (P.mkCAdv "more" "lal" "than")) (mkCard "35")) (P.mkN "person" "people")))) ;
    -- OrderS = mkUtt (mkCl (mkNP the_Quant pluralNum D.person_N) (mkAP (P.mkA2 (P.mkA "ordered") by8agent_Prep) (mkNP D.height_N))) ;
    -- OrderS2 = mkUtt (mkImp (mkVP (mkVP (D.display_V2) (mkNP the_Det D.car_N)) (mkAdv (P.mkPrep "ordered by") (mkNP D.price_N)))) ;
    -- CountryS = mkUtt (mkNP D.country_N) ;
    NullS op i = mkUtt (mkS (mkCl (mkNP the_Det D.population_N) (mkAdv op (symb i)))) ;

    CompOpEq = P.noPrep ;
    CompOpGt = above_Prep ;
    CompOpLt = under_Prep ;
    CompOpGEq = P.mkPrep "at least" ;
    CompOpLEq = P.mkPrep "at most" ;
    CompOpNE = P.mkPrep "not" ;

    --FL from lim = lim from ;

    --FCountry = mkNP (mkN "country") ;

    --LAll = mkNP all_Predet ;

    -----------------------------------------

    MVery = very_AdA ;

    QOld = P.mkA "old" ;
    QYoung = P.mkA "young" ;
    QHappy = P.mkA "happy" ;
    QSad = P.mkA "sad" ;

    -------------------------------------------

    PredIn c vs = mkS (mkCl (mkNP the_Det c) (mkNP either7or_DConj vs)) ;
    PredLike c op st = mkS (mkCl (mkNP the_Det c) (mkVP op (symb st.s))) ;

    CPop = P.mkN "population" ;

    VLondon = symb "london" ;
    VParis = symb "paris" ;
    VBerlin = symb "berlin" ;

    BaseValue v1 v2 = mkListNP v1 v2 ;
    ConsValue v vs = mkListNP v vs ;

    LikeBegins = (P.mkV2 (P.mkV "begin" "began" "begun") with_Prep) ;
    LikeEnds = (P.mkV2 (P.mkV "end") with_Prep) ;
    LikeContains = P.mkV2 (P.mkV "contain") ;
    
    -----------------------------------------
    
    SortColumn c o = mkNP (mkNP c) o ;
    OrdAsc = P.mkV2 (P.mkV "ascend") ;

}