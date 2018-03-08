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
    V2 = { v : Verb ; c : Str ; hasC : Bool } ; -- c is die "na" van "kyk na"
    VS = { v : Verb ; c : Str } ; -- c is die "dat" van "weet dat"
    Adv = { s : Str ; p : TPol } ; -- polarity: altyd/nooit
    -- AdA = {s : Str} ;
    Pol = { s : Str ; p : TPol} ;
    Tense = { s : Str ; t : TTense} ;
    -- Conj = {s : Str ; n : Number} ;
  lin
    UttS s = s.s ! SVO ;

    UseCl t p cl =
      {
        s = \\o => t.s ++ p.s ++ cl.s!t.t!p.p!o ;
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
            TPres => (vp.v).s!VPres ; -- hou / sien
            TPast => "het" ;
            TPerf => "het" ;
            TFut => "sal"
          } ;
          verbb = case t of {
            TPres => (vp.v).p ; -- op / []
            TPast => (vp.v).s!VPast ; -- opgehou / gesien
            TPerf => (vp.v).s!VPerf ; -- opgehou / gesien
            TFut => (vp.v).s!VInfa -- ophou / sien
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
      v = vs.v ; -- weet
      n2a = [] ;
      n2b = [] ;
      subCl = vs.c ++ s.s ! SOV ; -- dat <S>
      adv = [] ;
      filled = True ;
      nword = False ;
      finNie = s.finNie
    } ;

    ComplV2 v2 np = {
      v = v2.v ;
      n2a = case <np.isPron,v2.hasC> of {
        <True,False> => np.s ! Acc ; -- hy sien [my] (nie) nie
        <_,_> => []
      } ;
      n2b = case <np.isPron,v2.hasC> of {
        <True,False> => [] ;
        <_,_> => v2.c ++ np.s ! Acc -- alle ander gevalle: hy kyk nie altyd [na my] nie; hy sien nie [die vrou] nie
      } ;
      subCl = [] ;
      adv = [] ;
      filled = case <np.isPron,v2.hasC> of {
        <True,False> => False ;
        <_,_> => True
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
      filled = v.hasPart ; -- True => "hy hou [nie] *op* nie" ; False => "hy loop [] nie"
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
     s = \\n => ap.s ! AAttrib ++ cn.s ! n ;
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
    no_one_NP = pronNP "niemand" "niemand" Sg Per3 Neuter TNeg ;
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
  }
