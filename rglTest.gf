abstract rglTest = {

flags startcat = Sentence ;

cat
    Sentence ;
    Mod ;
    Qual ;
    Predicate ;
    Column ;
    Value ;
    [Value] {2} ;
    LikeOp ;
    SortBy ;
    Order ;
    --FromLim ;
    --From ;
    --Lim ;

fun
    App : Mod -> Qual -> Sentence ;
    PS : Predicate -> Sentence ;
    SortS : SortBy -> Sentence ;
    AllS : Sentence ;
    --FLS : FromLim -> Sentence ;
    TestS : Sentence ;

    --FL : From -> Lim -> FromLim ;

    --FCountry : From ;

    --LAll : Lim ;

    ------------------------------------------

    MVery : Mod ;

    QOld : Qual ;
    QYoung : Qual ;
    QHappy : Qual ;
    QSad : Qual ;

    -----------------------------------------------

    PredIn : Column -> [Value] -> Predicate ;
    PredLike : Column -> LikeOp -> String -> Predicate ;

    CPop : Column ;

    VLondon : Value ;
    VParis : Value ;
    VBerlin : Value ;

    LikeBegins, LikeEnds, LikeContains : LikeOp ;

    -----------------------------------------------------

    SortColumn : Column -> Order -> SortBy ;
    OrdAsc : Order ;

}