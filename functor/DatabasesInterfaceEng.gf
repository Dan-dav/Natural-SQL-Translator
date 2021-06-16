instance DatabasesInterfaceEng of DatabasesInterface = open
   SyntaxEng,
   (P = ParadigmsEng),
   (I = IrregEng) in {

oper
  show_V2 = P.mkV2 (P.mkV "display") | P.mkV2 I.show_V ;
  info_N = P.mkN "info" ;
  about_Prep = P.mkPrep "about" ;
  where_Subj = P.mkSubj "where" ;
  null_A = P.mkA "null" ;
  begin_V2 = P.mkV2 I.begin_V with_Prep ;
  end_V2 = P.mkV2 (P.mkV "end") with_Prep ;
  contain_V2 = P.mkV2 (P.mkV "contain") ;
  delete_V2 = P.mkV2 (P.mkV "delete") ;
  add_V2 = P.mkV2 (P.mkV "add") ;
  row_N = P.mkN "row" ; 
  value_N = P.mkN "value" ;
  set_V2 = P.mkV2 I.set_V ;
  at_least_Prep = P.mkPrep "at least" ;
  at_most_Prep = P.mkPrep "at most" ;
  not_Prep = P.mkPrep "not" ;
  ordered_by_Prep = P.mkPrep "ordered by" ;
  ascending_Adv = P.mkAdv "ascending" ;
  descending_Adv = P.mkAdv "descending" ;
  noPrep = P.noPrep ;
  noAdv = P.mkAdv "" ;
  
}