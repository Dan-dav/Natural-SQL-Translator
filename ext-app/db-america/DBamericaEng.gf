--# -path=.:../gf-rgl/src/morphodict:../gen-gf-files

concrete DBamericaEng of DBamerica = DatabasesEng ** open SyntaxEng, (P = ParadigmsEng), (D = MorphoDictEng) in {

lin

  Col_abbreviation = mkCN (P.mkN "abbreviation" "abbreviations") ;
  Col_accession_date = mkCN (P.mkN "accession date" "accession dates") ;
  Col_admission_date = mkCN (P.mkN "admission date" "admission dates") ;
  Col_age = mkCN (P.mkN "age" "ages") ;
  Col_area = mkCN (P.mkN "area" "areas") ;
  Col_birthdate = mkCN (P.mkN "birthdate" "birthdates") ;
  Col_capital = mkCN (P.mkN "capital" "capitals") ;
  Col_home_state = mkCN (P.mkN "home state" "home states") ;
  Col_name = mkCN (P.mkN "name" "names") ;
  Col_number = mkCN (P.mkN "number" "numbers") ;
  Col_party = mkCN (P.mkN "party" "parties") ;
  Col_population = mkCN (P.mkN "population" "populations") ;
  Col_president = mkCN (P.mkN "president" "presidents") ;
  Col_vice_president = mkCN (P.mkN "vice president" "vice presidents") ;

  Tab_presidents = P.mkN "president" "presidents" ;
  Tab_states = P.mkN "state" "states" ;
  Tab_vice_presidents = P.mkN "vice president" "vice presidents" ;

}