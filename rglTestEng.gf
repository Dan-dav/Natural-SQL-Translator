concrete rglTestEng of rglTest = open SyntaxEng, ParadigmsEng, SymbolicEng in {

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

lin
    App m q = mkUtt (mkAP m q) ;
    PS p = mkUtt p ;
    SortS sb = mkUtt sb ;
    AllS = mkUtt (mkNP (mkDet the_Quant (mkCard (ParadigmsEng.mkAdN "first") (mkCard (mkDigits "1")))) (mkCN (mkN "country"))) ;
    -- FLS fl = mkUtt fl ;
    -- TestS = mkUtt (mkS negativePol (mkCl she_NP (SyntaxEng.mkAdv (mkCAdv "more" "lal" "than") (mkA "warm") he_NP))) ;
    -- TestS = mkUtt (mkCard (SyntaxEng.mkAdN (mkCAdv "more" "lal" "than")) (mkCard (mkNumeral n8_Unit))) ;
    -- mkVP (SyntaxEng.mkAdv (mkCAdv "more" "lal" "than") (mkA "warm") he_NP)
    -- there are more than five people
    TestS = mkUtt (mkS negativePol (mkCl they_NP (mkNP (mkCard (SyntaxEng.mkAdN (mkCAdv "more" "lal" "than")) (mkCard "35")) (mkN "person" "people")))) ;

    --FL from lim = lim from ;

    --FCountry = mkNP (mkN "country") ;

    --LAll = mkNP all_Predet ;

    -----------------------------------------

    MVery = very_AdA ;

    QOld = mkA "old" ;
    QYoung = mkA "young" ;
    QHappy = mkA "happy" ;
    QSad = mkA "sad" ;

    -------------------------------------------

    PredIn c vs = mkS (mkCl (mkNP the_Det c) (mkNP either7or_DConj vs)) ;
    PredLike c op st = mkS (mkCl (mkNP the_Det c) (mkVP op (symb st.s))) ;

    CPop = mkN "population" ;

    VLondon = symb "london" ;
    VParis = symb "paris" ;
    VBerlin = symb "berlin" ;

    BaseValue v1 v2 = mkListNP v1 v2 ;
    ConsValue v vs = mkListNP v vs ;

    LikeBegins = (mkV2 (mkV "begin" "began" "begun") with_Prep) ;
    LikeEnds = (mkV2 (mkV "end") with_Prep) ;
    LikeContains = mkV2 (mkV "contain") ;
    
    -----------------------------------------
    
    SortColumn c o = mkNP (mkNP c) o ;
    OrdAsc = mkV2 (mkV "ascend") ;

}