--# -path=.:../gf-rgl/src/morphodict

concrete DBcountriesSwe of DBcountries = DatabasesSwe ** open (P = ParadigmsSwe), (D = MorphoDictSwe) in {

lin

  ColName = P.mkN "namn" ;
  ColCapital = P.mkN "huvudstad" ;
  ColArea = P.mkN "yta" ;
  ColPopulation = P.mkN "befolkning" ;
  ColContinent = P.mkN "kontinent" ;
  ColCurrency = P.mkN "valuta" ;
  ColCode = P.mkN "kod" ;

  TabCountries = P.mkN "land" "länder" ;
  TabCurrencies = P.mkN "valuta" "valutor" ;

}