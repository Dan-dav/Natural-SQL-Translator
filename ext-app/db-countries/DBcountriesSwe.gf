--# -path=.:../gf-rgl/src/morphodict:../gen-gf-files

concrete DBcountriesSwe of DBcountries = DatabasesSwe ** open SyntaxSwe, (P = ParadigmsSwe), (D = MorphoDictSwe) in {

lin

  Col_area = mkCN D.yta_N ;
  Col_capital = mkCN D.huvudstad_N ;
  Col_code = mkCN D.kod_2_N ;
  Col_continent = mkCN D.kontinent_N ;
  Col_currency = mkCN D.valuta_N ;
  Col_name = mkCN D.namn_N ;
  Col_population = mkCN D.befolkning_N ;

  Tab_countries = D.land_2_N ;
  Tab_currencies = D.valuta_N ;


--  Col_area = mkCN (P.mkN "yta" "ytor") ;
--  Col_capital = mkCN (P.mkN "huvudstad" "huvudstäder") ;
--  Col_code = mkCN (P.mkN "kod" "koder") ;
--  Col_continent = mkCN (P.mkN "kontinent" "kontinenter") ;
--  Col_currency = mkCN (P.mkN "valuta" "valutor") ;
--  Col_name = mkCN (P.mkN "namn" "namn") ;
--  Col_population = mkCN (P.mkN "befolkning" "befolkningar") ;

--  Tab_countries = P.mkN "land" "länder" ;
--  Tab_currencies = P.mkN "valuta" "valutor" ;

}