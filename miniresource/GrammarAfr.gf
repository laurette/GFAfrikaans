concrete GrammarAfr of Grammar = open Prelude, ResAfr in {

  lincat
    Utt = Str ;
    S  = { s : Order => Str ; finNie : Bool } ;
    Cl = { s : TTense => TPol => Order => Str ; finNie : Bool } ;
    NP = { s : Case => Str ; a : Agr ; isPron : Bool ; p : TPol } ; -- gender and number in order to make reflexive verbs work (die vrou verbeel haar, die kinders verbeel hulle)
    VP = ResAfr.VP ;
    AP = { s : AForm => Str } ;
    CN = { s : Number => Str ; g : Gender } ;
    Det = {s : Str ; n : Number } ;
    N = { s : Number => Str ; g : Gender } ;
    A = { s : AForm => Str } ;
    V = Verb ;
    V2 = { v : Verb ; c : Str ; hasC : Bool } ;
    VS = { v : Verb ; c : Str } ;
    Adv = { s : Str ; p : TPol } ;
    -- AdA = {s : Str} ;
    Pol = { s : Str ; p : TPol} ;
    Tense = { s : Str ; t : TTense} ;
    Mood = { s : Str } ;
    -- Conj = {s : Str ; n : Number} ;
  lin
    UttS s = s.s ! SVO ;

    UseCl m t p cl =
      {
        s = \\o => m.s ++ t.s ++ p.s ++ cl.s!t.t!p.p!o ;
        finNie = case p.p of {
          TPos => cl.finNie ;
          TNeg => True
        }
      } ;

    PredVP np vp = {
      s = \\t,p,f =>
        let
          subj = np.s ! Nom ;
          verba = case t of {
            TPres => (vp.v).s!VPres ;
            TPast => "het" ;
            TPerf => "het" ;
            TFut => "sal"
          } ;
          verbb = case t of {
            TPres => (vp.v).p ;
            TPast => (vp.v).s!VPast ;
            TPerf => (vp.v).s!VPerf ;
            TFut => (vp.v).s!VInfa
          } ;
          --verbc =
          obja = vp.n2a ;
          objb = vp.n2b ;
          subcl = vp.subCl ;
          adv = vp.adv ;
          neg1 : TPol => Str = table { TPos => [] ; TNeg => putNie (fillNeg1 t vp.filled)} ;--table { Pos => [] ; Neg => case vp.double1 of {True => "nie" ; False => []}} ;
          neg2 : TPol => Str = table { TPos => putNie (fillNeg2Pos np.p vp.nword vp.finNie) ;
                                       TNeg => putNie (fillNeg2Neg vp.finNie) } ;
        in case f of {
          SVO => subj ++ verba ++ obja ++ neg1!p ++ adv ++ objb ++ verbb ++ subcl ++ neg2!p ;
          SOV => subj ++ obja ++ neg1!p ++ adv ++ objb ++ verba ++ verbb ++ subcl ++ neg2!p ;
          VSO => verba ++ subj ++ obja ++ neg1!p ++ adv ++ objb ++ subcl ++ verbb ++ neg2!p
        } ;
      finNie = finNiePos np.p vp.nword vp.finNie
    } ;

    AdvVP vp adv = {
      v = vp.v ;
      n2a = vp.n2a ;
      n2b = vp.n2b ;
      subCl = vp.subCl ;
      adv = adv.s ;
      filled = True ;
      nword = case adv.p of {
        TPos => vp.nword ;
        TNeg => True
      } ;
      finNie = vp.finNie
    } ;

    ComplVS vs s = {
      v = vs.v ;
      n2a = [] ;
      n2b = [] ;
      subCl = vs.c ++ s.s ! SOV ;
      adv = [] ;
      filled = True ;
      nword = False ;
      finNie = s.finNie
    } ;

    ComplV2 v2 np = {
      v = v2.v ;
      n2a = case <np.isPron,v2.hasC> of {
        <True,False> => v2.c ++ np.s ! Acc ;
        <_,_> => []
      } ;
      n2b = case <np.isPron,v2.hasC> of {
        <True,False> => [] ;
        <_,_> => v2.c ++ np.s ! Acc
      } ;
      subCl = [] ;
      adv = [] ;
      filled = case np.isPron of {
        True => False ;
        False => True
      } ;
      nword = case np.p of {
        TPos => False ;
        TNeg => True
      } ;
      finNie = False
    } ;

    UseV v = {
      v = v ;
      n2a = [] ;
      n2b = [] ;
      subCl = [] ;
      adv = [] ;
      filled = v.hasPart ;
      nword = False ;
      finNie = False
    } ;

    DetCN det cn = {
     s = \\_ => det.s ++ cn.s ! det.n ;
     a = Ag det.n Per3 cn.g ;
     isPron = False ;
     p = TPos
     } ;

    ModCN ap cn = {
     s = \\n => ap.s ! APredic ++ cn.s ! n ;
     g = cn.g
     } ;

    UseN n = n ;

    UseA adj = adj ;

    a_Det = { s = "'n" ; n = Sg } ;
    every_Det = { s = "elke" ; n = Sg } ;
    theSg_Det = { s = "die" ; n = Sg } ;
    thePl_Det = { s = "die" ; n = Pl } ;

    this_Det = { s = "hierdie" ; n = Sg } ;
    these_Det = { s = "hierdie" ; n = Pl } ;
    that_Det = { s = "daardie" ; n = Sg } ;
    those_Det = { s = "daardie" ; n = Pl } ;

    i_NP = pronNP "ek" "my" Sg Per1 Neuter TPos ;
    no_one_NP = pronNP "niemand" "niemand" Sg Per3 Masc TNeg ;
    -- youSg_NP = pronNP "you" "you" Sg Per2 ;
    -- he_NP = pronNP "he" "him" Sg Per3 ;
    -- she_NP = pronNP "she" "her" Sg Per3 ;
    -- we_NP = pronNP "we" "us" Pl Per1 ;
    -- youPl_NP = pronNP "you" "you" Pl Per2 ;
    -- they_NP = pronNP "they" "them" Pl Per3 ;
    --
    -- very_AdA = ss "very" ;
    --
    Pos  = {s = [] ; p = TPos} ;
    Neg  = {s = [] ; p = TNeg} ;

    Pres = {s = [] ; t = TPres} ;
    Perf = {s = [] ; t = TPerf} ;
    Past = {s = [] ; t = TPast} ;
    Fut  = {s = [] ; t = TFut} ;

    Indic = { s = [] } ;

    already_Adv = regAdv "reeds" TPos ;
    still_Adv = regAdv "steeds" TPos ;
  }
