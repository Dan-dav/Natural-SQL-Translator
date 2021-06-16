concrete DatabasesSwe of Databases = DatabasesFunctor - [LimNone]

 with
  (Syntax = SyntaxSwe),
  (Symbolic = SymbolicSwe),
  (DatabasesInterface = DatabasesInterfaceSwe)
 ** {

-- exceptions to functor
lin
  LimNone t = mkNP all_Predet (mkNP aPl_Det t) ;
 
}