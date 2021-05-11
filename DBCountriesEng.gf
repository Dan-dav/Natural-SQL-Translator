--# -path=.:../gf-rgl/src/morphodict

concrete DBCountriesEng of DBCountries = DatabasesEng ** open (P = ParadigmsEng), (D = MorphoDictEng) in {

lin

  -- N
  ColName = D.name_N ;
  ColCapital = D.capital_N ;
  ColArea = D.area_N ;
  ColPopulation = D.population_N ;
  ColContinent = D.continent_N ;
  ColCurrency = D.currency_N ;
  ColCode = D.code_N ;
  
  -- N
  TabCountries = P.mkN "countries" ;
  --D.country_N ;
  -- plural?
  -- (P.mkN "country").s P.plural ;
  TabCurrencies = P.mkN "currencies" ;
  --D.currency_N ;

}