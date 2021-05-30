--# -path=.:../gf-rgl/src/morphodict:../gen-gf-files

concrete DBcountriesEng of DBcountries = DatabasesEng ** open SyntaxEng, (P = ParadigmsEng), (D = MorphoDictEng) in {

lin

  Col_area = mkCN (P.mkN "area" "areas") ;
  Col_capital = mkCN (P.mkN "capital" "capitals") ;
  Col_code = mkCN (P.mkN "code" "codes") ;
  Col_continent = mkCN (P.mkN "continent" "continents") ;
  Col_currency = mkCN (P.mkN "currency" "currencies") ;
  Col_name = mkCN (P.mkN "name" "names") ;
  Col_population = mkCN (P.mkN "population" "populations") ;

  Tab_countries = P.mkN "country" "countries" ;
  Tab_currencies = P.mkN "currency" "currencies" ;

}