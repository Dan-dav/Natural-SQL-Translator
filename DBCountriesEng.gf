concrete DBCountriesEng of DBCountries = DatabasesEng ** open (D = DictEng) in {

lin

  -- N
  -- ColName = "name" ;
  ColName = D.name_N ;
  -- ColCapital = "capital" ;
  ColCapital = D.capital_N ;
  -- ColArea = "area" ;
  ColArea = D.area_1_N ;
  -- used to be D.area_N
  -- ColPopulation = "population" ;
  ColPopulation = D.population_N ;
  -- ColContinent = "continent" ;
  ColContinent = D.continent_N ;
  -- ColCurrency = "currency" ;
  ColCurrency = D.currency_N ;
  ColCode = D.code_N ;
  
  -- N
  -- TabCountries = "countries" ;
  TabCountries = D.country_N ;
  -- plural?
  -- (P.mkN "country").s P.plural ;
  TabCurrencies = D.currency_N ;

}