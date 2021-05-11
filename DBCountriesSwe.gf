--# -path=.:../gf-rgl/src/morphodict

concrete DBCountriesSwe of DBCountries = DatabasesSwe ** open (P = ParadigmsSwe), (D = MorphoDictSwe) in {

lin

  -- N
  ColName = P.mkN "namn" ;
  ColCapital = P.mkN "huvudstad" ;
  ColArea = P.mkN "yta" ;
  ColPopulation = P.mkN "befolkning" ;
  ColContinent = P.mkN "kontinent" ;
  ColCurrency = P.mkN "valuta" ;
  ColCode = P.mkN "kod" ;
  
  -- N
  TabCountries = P.mkN "länder" ;
  TabCurrencies = P.mkN "valutor" ;

}