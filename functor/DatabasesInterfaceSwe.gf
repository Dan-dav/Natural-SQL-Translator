instance DatabasesInterfaceSwe of DatabasesInterface =
  open
   SyntaxSwe,
   (P = ParadigmsSwe),
   (M = MakeStructuralSwe),
   (I = IrregSwe) in {

oper
  show_V2 = P.mkV2 (P.mkV "visa") ;
  info_N = P.mkN "info" ;
  about_Prep = P.mkPrep "om" ;
  where_Subj = M.mkSubj "där" ;
  null_A = P.mkA "null" ;
  begin_V2 = P.mkV2 (P.mkV "börja") with_Prep ;
  end_V2 = P.mkV2 (P.mkV "sluta") with_Prep ;
  contain_V2 = P.mkV2 (P.mkV "innehålla" "innehöll" "innehållit") ;
  delete_V2 = P.mkV2 (P.mkV "radera") ;
  add_V2 = P.mkV2 (P.mkV "tillägga" "tillade" "tillagt") ;
  row_N = P.mkN "rad" "rader" ; 
  value_N = P.mkN "värde" ;
  set_V2 = P.mkV2 I.sätta_V ;
  at_least_Prep = P.mkPrep "minst" ;
  at_most_Prep = P.mkPrep "högst" ;
  not_Prep = P.mkPrep "icke" ;
  ordered_by_Prep = P.mkPrep "sorterade enligt" ; ---- ??
  ascending_Adv = P.mkAdv "stigande" ;
  descending_Adv = P.mkAdv "fallande" ;
  noPrep = P.noPrep ;
  noAdv = P.mkAdv "" ;
 
}