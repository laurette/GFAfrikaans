concrete MiniGrammarAfr of MiniGrammar = MiniCatAfr ** open Prelude, MiniResAfr in {

  lin
    UttS s = { s = s.s ! SVO } ;
    UttQS q = { s = q.qw ++ q.s ! q.o } ;

    UseCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s!t.t!p.p!o ;
    } ;

    UseQCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s!t.t!p.p!o ;
      o = cl.o ;
      qw = cl.qw ;
      hasQw = cl.hasQw
    } ;

    AdvS adv s = {
      s = \\o => adv.s ++ s.s!VSO
    } ;

    PredVP np vp = {
      s = \\t,p,f =>
        let
          subj = np.s ! Nom ;
          verba = case <(vp.v).vtype,t> of {
            <VAux,TPres> => (vp.v).s!VPres ; -- kan
            <VAux,TPast> => (vp.v).s!VPast ; -- kon
            <VAux,TPerf> => (vp.v).s!VPast ; -- kon
            <_,TPres> => (vp.v).s!VPres ; -- sien
            <VBe,TPast> => (vp.v).s!VPast ;
            <VBe,TPerf> => (vp.v).s!VPres ;
            <VReg,TPast> => [] ;
            <VReg,TPerf> => [] ;
            <_,TFut> => "sal"
          } ;
          verbhet = case <(vp.v).vtype,t> of {
            <VReg,TPast> => "het" ;
            <VReg,TPerf> => "het" ;
            <_,_> => []
          } ;
          verbb = case <(vp.v).vtype,t,vp.vIsBe> of {
            <VAux,TPres,_> => vp.compV!VInfa ; -- (kan) sien
            <VAux,TPast,True> => [] ; -- () gewees het
            <VAux,TPast,False> => vp.compV!VPast ; -- (kon) sien
            <VAux,TPerf,_> => vp.compV!VInfa ; -- (kon) gesien
            <VAux,TFut> => (vp.v).s!VInfa ; -- (sal) kan

            <VReg,TPres,_> => (vp.v).p ; -- (kyk) op
            <VReg,TPast,_> => (vp.v).s!VPast ; -- (het) gesien
            <VReg,TPerf,_> => (vp.v).s!VPerf ; -- (het) gesien
            <VReg,TFut,_> => (vp.v).s!VInfa ;-- (sal) sien

            <_,_,_> => []
          } ;
          verbc = case <(vp.v).vtype,t,vp.vIsBe> of {
            <VBe,TPast,_> => "gewees" ; -- (was) gewees
            <VBe,TFut,_> => "wees" ; -- (sal) wees
            <VAux,TPast,True> => "gewees het" ; -- (kon gesien) het
            <VAux,TPast,False> => "het" ; -- (kon gesien) het
            <VAux,TFut,_> => vp.compV!VInfa ; -- (sal kan) sien

            <_,_,_> => []
          } ;
          obja = case vp.v.vtype of {
            VBe => [] ;
            _ => vp.n2a
          } ;
          objb = case vp.v.vtype of {
            VBe => vp.n2a ;
            _ => vp.n2b
          } ;
          subcl = vp.subCl ;
          adv = vp.adv ;
          neg1 : TPol => Str = table {
            TPos => [] ;
            TNeg => putNie (fillNeg1 t vp.filled)
          } ;
          neg2 : TPol => Str = table {
            TPos => putNie (fillNeg2Pos np.p vp.nword) ;
            TNeg => pre { "nie" => [] ; _ => "nie" }
          } ;
        in case f of {
          SVO => subj ++ verba ++ verbhet ++ obja ++ neg1!p ++ adv ++ objb ++ verbb ++ verbc ++ subcl ++ neg2!p ;
          SOV => subj ++ obja ++ neg1!p ++ adv ++ objb ++ verba ++ verbb ++ verbhet ++ verbc ++ subcl ++ neg2!p ;
          VSO => verba ++ verbhet ++ subj ++ obja ++ neg1!p ++ adv ++ objb ++ subcl ++ verbb ++ verbc ++ neg2!p
        } ;
    } ;

    QuestCl cl = cl ** { o = VSO ; qw = [] ; hasQw = False } ;

    QuestIAdv iadv cl = cl ** { o = VSO ; qw = iadv.s ; hasQw = True } ;

    QuestVP ip vp = {
      s = \\t,p,f =>
        let
          subj = [] ;
          verba = case <(vp.v).vtype,t> of {
            <VAux,TPres> => (vp.v).s!VPres ; -- kan
            <VAux,TPast> => (vp.v).s!VPast ; -- kon
            <VAux,TPerf> => (vp.v).s!VPast ; -- kon
            <_,TPres> => (vp.v).s!VPres ; -- sien
            <VBe,TPast> => (vp.v).s!VPast ;
            <VBe,TPerf> => (vp.v).s!VPres ;
            <VReg,TPast> => [] ;
            <VReg,TPerf> => [] ;
            <_,TFut> => "sal"
          } ;
          verbhet = case <(vp.v).vtype,t> of {
            <VReg,TPast> => "het" ;
            <VReg,TPerf> => "het" ;
            <_,_> => []
          } ;
          verbb = case <(vp.v).vtype,t,vp.vIsBe> of {
            <VAux,TPres,_> => vp.compV!VInfa ; -- (kan) sien
            <VAux,TPast,True> => [] ; -- () gewees het
            <VAux,TPast,False> => vp.compV!VPast ; -- (kon) sien
            <VAux,TPerf,_> => vp.compV!VInfa ; -- (kon) gesien
            <VAux,TFut> => (vp.v).s!VInfa ; -- (sal) kan

            <VReg,TPres,_> => (vp.v).p ; -- (kyk) op
            <VReg,TPast,_> => (vp.v).s!VPast ; -- (het) gesien
            <VReg,TPerf,_> => (vp.v).s!VPerf ; -- (het) gesien
            <VReg,TFut,_> => (vp.v).s!VInfa ;-- (sal) sien

            <_,_,_> => []
          } ;
          verbc = case <(vp.v).vtype,t,vp.vIsBe> of {
            <VBe,TPast,_> => "gewees" ; -- (was) gewees
            <VBe,TFut,_> => "wees" ; -- (sal) wees
            <VAux,TPast,True> => "gewees het" ; -- (kon gesien) het
            <VAux,TPast,False> => "het" ; -- (kon gesien) het
            <VAux,TFut,True> => "wees" ; -- (sal kan) sien
            <VAux,TFut,False> => vp.compV!VInfa ; -- (sal kan) sien

            <_,_,_> => []
          } ;
          obja = case vp.v.vtype of {
            VBe => [] ;
            _ => vp.n2a
          } ;
          objb = case vp.v.vtype of {
            VBe => vp.n2a ;
            _ => vp.n2b
          } ;
          subcl = vp.subCl ;
          adv = vp.adv ;
          neg1 : TPol => Str = table {
            TPos => [] ;
            TNeg => putNie (fillNeg1 t vp.filled)
          } ;
          neg2 : TPol => Str = table {
            TPos => putNie (fillNeg2Pos TPos vp.nword) ;
            TNeg => pre { "nie" => [] ; _ => "nie" }
          } ;
        in case f of {
          SVO => subj ++ verba ++ verbhet ++ obja ++ neg1!p ++ adv ++ objb ++ verbb ++ verbc ++ subcl ++ neg2!p ;
          SOV => subj ++ obja ++ neg1!p ++ adv ++ objb ++ verba ++ verbb ++ verbhet ++ verbc ++ subcl ++ neg2!p ;
          VSO => verba ++ verbhet ++ subj ++ obja ++ neg1!p ++ adv ++ objb ++ subcl ++ verbb ++ verbc ++ neg2!p
        } ;
        o = SVO ;
        qw = ip.s ;
        hasQw = True
    } ;

    -- QuestSlash : IP -> ClSlash -> QCl ;  -- who does she walk with
    -- QuestIAdv  : IAdv -> Cl -> QCl ;     -- why does she walk

    -- SlashV2   : NP -> V2 -> ClSlash ;   -- she loves
    -- SlashPrep : Cl -> Prep -> ClSlash ; -- she walks with

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
      filled = case <np.isPron,v2.hasC> of {
        <True,False> => False ;
        <_,_> => True
      } ;
      nword = case np.p of {
        TPos => False ;
        TNeg => True
      } ;
      compV = \\_ => []
    } ;

    CompAP ap = {
      v = be_V ; -- weet
      inf = <[],[]> ;
      vIsBe = True ;
      n2a = ap.s!APredic ;
      n2b = [] ;
      subCl = [] ; -- dat <S>
      adv = [] ;
      filled = True ;
      nword = False ;
      compV = \\_ => []
    } ;

    CompAdv adv = {
      v = be_V ; -- weet
      inf = <[],[]> ;
      vIsBe = True ;
      n2a = adv.s ;
      n2b = [] ;
      subCl = [] ; -- dat <S>
      adv = [] ;
      filled = True ;
      nword = False ;
      compV = \\_ => []
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
      compV = \\_ => []
    } ;

    ComplVQ vq qs = {
      v = vq.v ; -- weet
      inf = <[],[]> ;
      vIsBe = False ;
      n2a = [] ;
      n2b = [] ;
      subCl = case qs.hasQw of {
        False => vq.c ++ qs.s ! SOV ; -- dat <S>
        True => qs.qw ++ qs.s ! qs.o
      } ;
      adv = [] ;
      filled = True ;
      nword = False ;
      compV = \\_ => []
    } ;

    ComplVV vv vp = {
      v = vv ;
      inf = <(vp.v).s!VInfa,(vp.v).s!VPerf> ;
      vIsBe = vp.vIsBe ;
      n2a = vp.n2a ;
      n2b = vp.n2b ;
      subCl = vp.subCl ;
      adv = vp.adv ;
      filled = True ;
      nword = vp.nword ;
      compV = addCompV vp.v vp.compV
    } ;

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
      compV = vp.compV
    } ;

    UseV v = {
      v = v ;
      vIsBe = False ;
      n2a = [] ;
      n2b = [] ;
      subCl = [] ;
      adv = [] ;
      filled = v.hasPart ; -- True => "hy hou [nie] *op* nie" ; False => "hy loop [] nie"
      nword = False ;
      compV = \\_ => []
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

    UsePN pn = {
      s = \\_ => pn.s ;
      a = pn.a ;
      isPron = False ;
      p = TPos
    } ;

    AdvNP np adv = {
      s = \\c => np.s!c ++ adv.s ;
      a = np.a ;
      isPron = np.isPron ;
      p = np.p
    } ;

    PrepNP prep np = {
      s = prep.s ++ np.s!Acc ;
      p = np.p
    } ;

    AdAP ada ap = {
      s = \\aform => ada.s ++ ap.s!aform
    } ;

    ConjS conj s1 s2 = {
      s = \\o => s1.s!o ++ conj.s ++ s2.s!o ;
    } ;

    ConjAP conj ap1 ap2 = {
      s = \\aform => ap1.s!aform ++ conj.s ++ ap2.s!aform
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

    UseN n = n ;

    CompoundN n1 n2 = {
        s = \\num => n1.s!Sg ++BIND++ n2.s!num ;
        g = n2.g
      } ;

    UseA adj = adj ;

    a_Det = { s = "'n" ; n = Sg ; p = TPos } ;
    aPl_Det = { s = "" ; n = Pl ; p = TPos } ;
    theSg_Det = { s = "die" ; n = Sg ; p = TPos } ;
    thePl_Det = { s = "die" ; n = Pl ; p = TPos } ;
    every_Det = { s = "elke" ; n = Sg ; p = TPos } ;

    this_Det = { s = "hierdie" ; n = Sg ; p = TPos } ;
    these_Det = { s = "hierdie" ; n = Pl ; p = TPos } ;
    that_Det = { s = "daardie" ; n = Sg ; p = TPos } ;
    those_Det = { s = "daardie" ; n = Pl ; p = TPos } ;

    noSg_Det = {s = "geen" ; n = Sg ; p = TNeg } ;
    noPl_Det = {s = "geen" ; n = Pl ; p = TNeg } ;

    i_NP = pronNP "ek" "my" Sg Per1 Neuter TPos ;
    youSg_NP = pronNP "jy" "jou" Sg Per2 Neuter TPos ;
    he_NP = pronNP "hy" "hom" Sg Per3 Masc TPos ;
    she_NP = pronNP "sy" "haar" Sg Per3 Fem TPos ;
    we_NP = pronNP "ons" "ons" Sg Per1 Neuter TPos ;
    youPl_NP = pronNP "julle" "julle" Sg Per2 Neuter TPos ;
    they_NP = pronNP "hulle" "hulle" Sg Per3 Neuter TPos ;
    it_NP = pronNP "dit" "dit" Sg Per3 Masc TPos ;

    no_one_NP = pronNP "niemand" "niemand" Sg Per3 Neuter TNeg ;
    nothing_NP = pronNP "niks" "niks" Sg Per3 Neuter TNeg ;
    very_AdA = ss "baie" ;

    who_IP = { s = "wie" } ;
    here_Adv = { s = "hier" ; p = TPos } ;

    by_Prep = {s = "deur"}  ;
    in_Prep = {s = "in"}  ;
    of_Prep = {s = "van"}  ;
    with_Prep = {s = "met"}  ;
    to_Prep = {s = "na"} ;

    can_VV = mkAux "kan" "kon" ;
    must_VV = mkAux "moet" "moes" ;
    want_VV = mkAux "wil" "wou" ;

    although_Subj = { s = "alhoewel" ; o = SOV } ;
    -- because_Subj, when_Subj : Subj ;
    when_IAdv = { s = "wanneer" } ;
    -- where_IAdv, why_IAdv : IAdv ;

    Pos  = {s = [] ; p = TPos} ;
    Neg  = {s = [] ; p = TNeg} ;

    Pres = {s = [] ; t = TPres} ;
    Perf = {s = [] ; t = TPerf} ;
    Past = {s = [] ; t = TPast} ;
    Fut  = {s = [] ; t = TFut} ;

    and_Conj = { s = "en" } ;
    or_Conj = { s = "of" } ;
  }
