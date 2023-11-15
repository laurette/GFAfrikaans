concrete LexiconAfr of Lexicon = GrammarAfr ** open ResAfr,ParadigmsAfr,Prelude in {

lin
  man_N = mkN "man" "mans" Masc ;
  woman_N = mkN "vrou" "vrouens" Fem ;
  house_N = mkN "huis" ;
  tree_N = mkN "boom" ;
  lion_N = mkN "leeu" ;

  binoculars_N = mkN "verkyker" ;

  big_A = mkA "groot" ;
  small_A = mkA "klein" ;
  green_A = mkA "groen" ;
  walk_V = mkV "loop" ;
  -- sleep_V = mkV "sleep" "sleeps" "slept" "slept" ;
  arrive_V = mkV "kom" "aan" ;
  -- love_V2 = mkV2 "love" ;
  look_V2 = mkV2 (mkV "kyk") "na" ;
  see_V2 = mkV2 "sien" ;
  -- please_V2 = mkV2 "please" ;
  -- believe_VS = mkV "believe" ;
  know_VS = mkVS (mkV "weet") ;
  wonder_VQ = mkVQ (mkV "wonder") ;

  always_Adv = mkAdv "altyd" TPos ;
  never_Adv = mkAdv "nooit" TNeg ;

  john_PN = { s = "Jan" ; a = Ag Sg Per3 Masc } ;
  mary_PN = { s = "Maria" ; a = Ag Sg Per3 Fem } ;

}
