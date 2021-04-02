concrete DBCountriesEng of DBCountries = DatabasesEng ** {

lin

  -- N
  ColName = "name" ;
  -- D.name_N ;
  ColCapital = "capital" ;
  -- D.capital_N ;
  ColArea = "area" ;
  -- D.area_N ;
  -- otherwise try D.area_1_N
  ColPopulation = "population" ;
  -- D.population_N ;
  ColContinent = "continent" ;
  -- D.continent_N ;
  ColCurrency = "currency" ;
  -- D.currency_N ;
  
  -- N
  TabCountries = "countries" ;
  -- D.country_N ;
  -- plural?
  -- (P.mkN "country").s P.plural ;

}