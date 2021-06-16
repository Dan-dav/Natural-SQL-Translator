instance DatabasesInterfaceFin of DatabasesInterface = open
   SyntaxFin,
   ParadigmsFin,
   (P = ParadigmsFin),
   (M = MakeStructuralFin) in {

oper
  show_V2 = P.mkV2 (P.mkV "näyttää") ;
  info_N = P.mkN "info" ;
  about_Prep = P.casePrep elative ;
  where_Subj = P.mkSubj "joissa" ; --- hack
  null_A = P.mkA "tyhjä" ;
  begin_V2 = P.mkV2 (mkV "alkaa") allative ;
  end_V2 = P.mkV2 (P.mkV "loppua") allative ;
  contain_V2 = P.mkV2 (P.mkV "sisältää") ;
  delete_V2 = P.mkV2 (P.mkV "poistaa") ;
  add_V2 = P.mkV2 (P.mkV "lisätä") ;
  row_N = P.mkN "rivi" ; 
  value_N = P.mkN "arvo" ;
  set_V2 = P.mkV2 (P.mkV "asettaa") ;
  at_least_Prep = P.mkPrep "vähintään" ;
  at_most_Prep = P.mkPrep "enintään" ;
  not_Prep = P.mkPrep "ei" ; ---
  ordered_by_Prep = P.mkPrep genitive "mukaan järjestettynä" ;
  ascending_Adv = P.mkAdv "nousevasti" ;
  descending_Adv = P.mkAdv "laskevasti" ;
  noPrep = P.casePrep nominative ; ---- ??
  noAdv = P.mkAdv "" ;
  
}