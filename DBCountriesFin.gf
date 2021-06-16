--# -path=.:functor

concrete DBCountriesFin of DBcountries = DatabasesFin ** open ParadigmsFin in {

lin

  -- N
  ColName = mkN "nimi" "nimiä" ;
  ColCapital = mkN "pää" (mkN "kaupunki") ;
  ColArea = mkN "pinta-" (mkN "ala") ;
  ColPopulation = mkN "asukas" (mkN "luku") ;
  ColContinent = mkN "maan" (mkN "osa") ;
  ColCurrency = mkN "valuutta" ;
  ColCode = mkN "koodi" ;
  
  -- N
  TabCountries = mkN "maat" ;  --- not an N
  --D.country_N ;
  -- plural?
  -- (P.mkN "country").s P.plural ; --- not an N
  TabCurrencies = mkN "valuutat" ;
  --D.currency_N ;

}