concrete GrammarAfr of Grammar = open Prelude, ResAfr in {

  lincat
    Utt = Str ;
    S  = { s : Order => Str ; finNie : Bool } ;
    Cl = { s : TTense => TPol => Order => Str ; finNie : Bool } ;

    QS = { s : Order => Str ; finNie : Bool } ;
    QCl = { s : TTense => TPol => Order => Str ; finNie : TPol => Bool } ;

    NP = { s : Case => Str ; a : Agr ; isPron : Bool ; p : TPol } ; -- gender and number in order to make reflexive verbs work (die vrou verbeel haar, die kinders verbeel hulle)
    VP = ResAfr.VP ;
    AP = { s : AForm => Str } ;
    CN = { s : Number => Str ; g : Gender } ;

    PN = { s : Str ; a : Agr } ;
    Det = {s : Str ; n : Number ; p : TPol } ;
    N = { s : Number => Str ; g : Gender } ;
    A = { s : AForm => Str } ;

    V = Verb ;
    V2 = { v : Verb ; c : Str ; hasC : Bool } ; -- c is die "na" van "kyk na"
    VS = { v : Verb ; c : Str } ; -- c is die "dat" van "weet dat"
    VQ = { v : Verb ; c : Str } ; -- c is die "of" van "wonder of"
    VV = Verb ;

    Adv = { s : Str ; p : TPol } ; -- polarity: altyd/nooit
    AdA = { s : Str} ;

    IP = { s : Str } ;
    Prep = { s : Str } ;

    Subj = { s = Str ; o : Order } ;

    Pol = { s : Str ; p : TPol} ;
    Tense = { s : Str ; t : TTense} ;
    Conj = { s : Str } ;

  lin
    UttS s = s.s ! SVO ;
    UttQS q = q.s ! VSO ++ "?";

    ConjS conj s1 s2 = {
      s = \\o => s1.s!o ++ conj.s ++ s2.s!o ;
      finNie = s2.finNie
    } ;

    UseCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s!t.t!p.p!o ;
      -- finNie = case p.p of {
      --   TPos => cl.finNie ;
      --   TNeg => True
      -- } ***2018-10-03
      finNie = cl.finNie ! p.p
    } ;

    UseQCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s!t.t!p.p!o ;
      -- finNie = case p.p of {
      --   TPos => cl.finNie ;
      --   TNeg => True
      -- } ***2018-10-03
      finNie = qcl.finNie ! p.p
    } ;

    QuestCl cl = cl ;

    SubjCl cl subj s = {
      s = \\t,p,f => cl.s!t!p!f ++ subj.s ++ s.s!subj.o ;
      finNie = \\p => s.finNie
    } ;

    PredVP np vp = {
      s = \\t,p,f =>
        let
          subj = np.s ! Nom ;
          verbaux = case <(vp.v).isAux,t> of {
            <True,TPres> => (vp.v).s!VPres ; -- kan
            <True,TPast> => (vp.v).s!VPast ; -- kon
            <True,TPerf> => (vp.v).s!VPast ; -- kon
            <False,TPres> => (vp.v).s!VPres ; -- sien
            <False,TPast> => [] ;
            <False,TPerf> => [] ;
            <_,TFut> => "sal"
          } ;
          verbhet = case <(vp.v).isAux,t> of {
            <False,TPast> => "het" ;
            <False,TPerf> => "het" ;
            <_,_> => []
          } ;
          verbb = case <(vp.v).isAux,t> of {
            <True,TPres> => vp.inf.p1 ; -- (kan) sien
            <True,TPast> => vp.inf.p1 ; -- (kon) sien
            <True,TPerf> => vp.inf.p2 ; -- (kon) gesien
            <True,TFut> => (vp.v).s!VInfa ; -- (sal) kan

            <False,TPres> => (vp.v).p ; -- (kyk) op
            <False,TPast> => (vp.v).s!VPast ; -- (het) gesien
            <False,TPerf> => (vp.v).s!VPerf ; -- (het) gesien
            <False,TFut> => (vp.v).s!VInfa -- (sal) sien
          } ;
          verbc = case <(vp.v).isAux,t> of {
            <True,TPerf> => case vp.vIsBe of {
              True => "gewees" ; -- (was) gewees
              False => "het" -- (kon gesien) het
            } ;
            <True,TFut> => vp.inf.p1 ; -- (sal kan) sien
            <_,_> => []
          } ;
          obja = vp.n2a ;
          objb = vp.n2b ;
          subcl = vp.subCl ;
          adv = vp.adv ;
          neg1 : TPol => Str = table { TPos => [] ; TNeg => putNie (fillNeg1 t vp.filled)} ;--table { Pos => [] ; Neg => case vp.double1 of {True => "nie" ; False => []}} ;
          neg2 : TPol => Str = table { TPos => putNie (fillNeg2Pos np.p vp.nword vp.finNie) ;
                                       TNeg => putNie (fillNeg2Neg vp.finNie) } ;
        in case f of {
          SVO => subj ++ verbaux ++ verbhet ++ obja ++ neg1!p ++ adv ++ objb ++ verbb ++ verbc ++ subcl ++ neg2!p ;
          SOV => subj ++ obja ++ neg1!p ++ adv ++ objb ++ verbaux ++ verbb ++ verbhet ++ verbc ++ subcl ++ neg2!p ;
          VSO => verbaux ++ verbhet ++ subj ++ obja ++ neg1!p ++ adv ++ objb ++ subcl ++ verbb ++ verbc ++ neg2!p
        } ;
      finNie = table {
        TPos => finNiePos np.p vp.nword vp.finNie ;
        TNeg => True
        }
    } ;

    -- QuestVP ip vp = {
    --   s = \\t,p =>
    --     let
    --       subj = ip.s ;
    --       verba = case <t,vp.v.isAux> of {
    --         <TPres,_> => (vp.v).s!VPres ; -- hou / sien / kan
    --         <TPast,True> => (vp.v).s!VPast ; -- kon
    --         <TPast,False> => "het" ;
    --         <TPerf,True> => (vp.v).s!VPast ; -- kon
    --         <TPerf,False> => "het" ;
    --         <TFut,_> => "sal"
    --       } ;
    --       verbb = case <t,vp.v.isAux> of {
    --         <TPres,True> => vp.compV ! VInfa ; -- (wil) ophou / sien
    --         <TPast,True> => vp.compV ! VInfb ;  -- (wou) ophou / sien
    --         <TPerf,True> => vp.compV ! VPerf ++ "het" ; -- opgehou het / gesien het
    --         <TFut,True> => (vp.v).s ! VPres ++ vp.compV ! VInfa ; -- kan ophou / kan sien
    --
    --         <TPres,False> => (vp.v).p ; -- op / []
    --         <TPast,False> => (vp.v).s!VPast ; -- opgehou / gesien
    --         <TPerf,False> => (vp.v).s!VPerf ; -- opgehou / gesien
    --         <TFut,False> => (vp.v).s!VInfa -- ophou / sien
    --       } ;
    --       --verbc =
    --       obja = vp.n2a ;
    --       objb = vp.n2b ;
    --       subcl = vp.subCl ;
    --       adv = vp.adv ;
    --       neg1a : TPol => Str = table { TPos => [] ; TNeg => putNie (fillNeg1 t vp.filled)} ;--table { Pos => [] ; Neg => case vp.double1 of {True => "nie" ; False => []}} ;
    --       neg1b : TPol => Str = table { TPos => [] ; TNeg => "nie" } ;
    --       neg2 : TPol => Str = table { TPos => putNie (fillNeg2Pos TPos vp.nword vp.finNie) ;
    --                                    TNeg => putNie (fillNeg2Neg vp.finNie) } ;
    --     in
    --       subj ++ verba ++ obja ++ neg1a!p ++ adv ++ objb ++ verbb ++ subcl ++ neg2!p ;
    --   finNie = finNiePos TPos vp.nword vp.finNie
    -- } ;

    AdvVP vp adv = {
      v = vp.v ;
      inf = vp.inf ;
      vIsBe = vp.vIsBe ;
      n2a = vp.n2a ;
      n2b = vp.n2b ;
      subCl = vp.subCl ;
      adv = adv.s ;
      filled = True ;
      nword = case adv.p of {
        TPos => vp.nword ;
        TNeg => True
      } ;
      finNie = vp.finNie --;
      -- compV = vp.compV
    } ;

    ComplVS vs s = {
      v = vs.v ; -- weet
      inf = <[],[]> ;
      vIsBe = False ;
      n2a = [] ;
      n2b = [] ;
      subCl = vs.c ++ s.s ! SOV ; -- dat <S>
      adv = [] ;
      filled = True ;
      nword = False ;
      finNie = s.finNie --;
      -- compV = \\_ => []
    } ;

    ComplVQ vq qs = {
      v = vq.v ; -- weet
      inf = <[],[]> ;
      vIsBe = False ;
      n2a = [] ;
      n2b = [] ;
      subCl = vq.c ++ q.s ! SOV ; -- dat <S>
      adv = [] ;
      filled = True ;
      nword = False ;
      finNie = qs.finNie --;
      -- compV = \\_ => []
    } ;

    ComplVV vv vp = {
      v = vv ;
      inf = <(vp.v).s!VInfa,(vp.v).s!VPerf> ;
      vIsBe = False ;
      n2a = vp.n2a ;
      n2b = vp.n2b ;
      subCl = vp.subCl ;
      adv = vp.adv ;
      filled = True ;
      nword = vp.nword ;
      finNie = vp.finNie --;
      -- compV = addCompV vp.v vp.compV
    } ;

    ComplV2 v2 np = {
      v = v2.v ;
      inf = <[],[]> ;
      vIsBe = False ;
      n2a = case <np.isPron,v2.hasC> of {
        <True,False> => v2.c ++ np.s ! Acc ; -- hy sien [my] (nie) nie
        <_,_> => []
      } ;
      n2b = case <np.isPron,v2.hasC> of {
        <True,False> => [] ;
        <_,_> => v2.c ++ np.s ! Acc -- alle ander gevalle: hy kyk nie altyd [na my] nie; hy sien nie [die vrou] nie
      } ;
      subCl = [] ;
      adv = [] ;
      -- filled = case <np.isPron,v2.hasC> of {
      --   <True,False> => False ;
      --   <_,_> => True
      -- } ; ***2018-10-03
      filled = case np.isPron of {
        True => False ;
        False => True
      } ;
      nword = case np.p of {
        TPos => False ;
        TNeg => True
      } ;
      finNie = False --;
      compV = \\_ => []
    } ;

    UseV v = {
      v = v ;
      n2a = [] ;
      n2b = [] ;
      subCl = [] ;
      adv = [] ;
      filled = v.hasPart ; -- True => "hy hou [nie] *op* nie" ; False => "hy loop [] nie"
      nword = False ;
      finNie = False ;
      compV = \\_ => []
    } ;

    ConjNP conj np1 np2 = {
      s = \\c => np1.s!c ++ conj.s ++ np2.s!c ;
      a = case np1.a of {
        Ag _ Per1 _ => Ag Pl Per1 Neuter ;
        Ag _ Per2 _ => Ag Pl Per2 Neuter ;
        Ag _ Per3 _ => case np2.a of {
          Ag _ Per1 _ => Ag Pl Per1 Neuter ;
          Ag _ Per2 _ => Ag Pl Per2 Neuter ;
          Ag _ Per3 _ => Ag Pl Per3 Neuter
        }
      } ;
      isPron = np1.isPron ;
      p = case np1.p of {
        TNeg => TNeg ;
        TPos => np2.p
      }
    } ;

    ConjAP conj ap1 ap2 = {
      s = \\aform => ap1.s!aform ++ conj.s ++ ap2.s!aform
    } ;

    AdvNP np adv = {
      s = \\c => np.s!c ++ adv.s ;
      a = np.a ;
      isPron = np.isPron ;
      p = np.p
    } ;

    UsePN pn = {
      s = \\_ => pn.s ;
      a = pn.a ;
      isPron = False ;
      p = TPos
    } ;

    DetCN det cn = {
     s = \\_ => det.s ++ cn.s ! det.n ;
     a = Ag det.n Per3 cn.g ;
     isPron = False ;
     p = det.p
     } ;

    ModCN ap cn = {
     s = \\n => ap.s ! AAttrib ++ cn.s ! n ;
     g = cn.g
     } ;

    UseN n = n ;

    PrepNP prep np = {
      s = prep.s ++ np.s!Acc ;
      p = np.p
    } ;

    AdAP ada ap = {
      s = \\aform => ada.s ++ ap.s!aform
    } ;

    UseA adj = adj ;

    a_Det = { s = "'n" ; n = Sg ; p = TPos } ;
    every_Det = { s = "elke" ; n = Sg ; p = TPos } ;
    theSg_Det = { s = "die" ; n = Sg ; p = TPos } ;
    thePl_Det = { s = "die" ; n = Pl ; p = TPos } ;

    this_Det = { s = "hierdie" ; n = Sg ; p = TPos } ;
    these_Det = { s = "hierdie" ; n = Pl ; p = TPos } ;
    that_Det = { s = "daardie" ; n = Sg ; p = TPos } ;
    those_Det = { s = "daardie" ; n = Pl ; p = TPos } ;

    noSg_Det = {s = "geen" ; n = Sg ; p = TNeg } ;
    noPl_Det = {s = "geen" ; n = Pl ; p = TNeg } ;

    no_one_NP = pronNP "niemand" "niemand" Sg Per3 Neuter TNeg ;
    nothing_NP = pronNP "niks" "niks" Sg Per3 Neuter TNeg ;

    i_NP = pronNP "ek" "my" Sg Per1 Neuter TPos ;
    youSg_NP = pronNP "jy" "jou" Sg Per2 Neuter TPos ;
    he_NP = pronNP "hy" "hom" Sg Per3 Masc TPos ;
    she_NP = pronNP "sy" "haar" Sg Per3 Fem TPos ;
    we_NP = pronNP "ons" "ons" Sg Per1 Neuter TPos ;
    youPl_NP = pronNP "julle" "julle" Sg Per2 Neuter TPos ;
    they_NP = pronNP "hulle" "hulle" Sg Per3 Neuter TPos ;

    very_AdA = ss "baie" ;

    can_VV = mkAux "kan" "kon" ;
    must_VV = mkAux "moet" "moes" ;
    want_VV = mkAux "wil" "wou" ;

    who_IP = { s = "wie" } ;

    by_Prep = {s = "deur"}  ;
    in_Prep = {s = "in"}  ;
    of_Prep = {s = "van"}  ;
    with_Prep = {s = "met"}  ;
    to_Prep = {s = "na"} ;

    although_Subj = { s = "alhoewel" ; o = SOV } ;

    and_Conj = { s = "en" } ;
    or_Conj = { s = "of" } ;

    Pos  = {s = [] ; p = TPos} ;
    Neg  = {s = [] ; p = TNeg} ;

    Pres = {s = [] ; t = TPres} ;
    Perf = {s = [] ; t = TPerf} ;
    Past = {s = [] ; t = TPast} ;
    Fut  = {s = [] ; t = TFut} ;
  }
