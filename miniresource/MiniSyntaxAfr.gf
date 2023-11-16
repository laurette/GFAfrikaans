resource MiniSyntaxAfr = open MiniResAfr, MiniGrammarAfr, Prelude in {

    oper

        mkUtt = overload {
            mkUtt : S -> Utt = UttS ;
            mkUtt : QS -> Utt = UttQS ;
        } ;

        mkS = overload {
            mkS : Cl -> S = \cl -> lin S (UseCl Pres Pos cl) ;
            mkS : Pol -> Cl -> S = \pol,cl -> lin S (UseCl Pres pol cl) ;
            mkS : Tense -> Cl -> S = \tense,cl -> lin S (UseCl tense Pos cl) ;
            mkS : Tense -> Pol -> Cl -> S = UseCl ;
            mkS : Adv -> S -> S = AdvS ;
        } ;

        mkQS = overload {
            mkQS : QCl -> QS = \cl -> lin QS (UseQCl Pres Pos cl) ;
            mkQS : Pol -> QCl -> QS = \pol,cl -> lin QS (UseQCl Pres pol cl) ;
            mkQS : Tense -> QCl -> QS = \tense,cl -> lin QS (UseQCl tense Pos cl) ;
            mkQS : Tense -> Pol -> QCl -> QS = UseQCl ;
        } ;

        mkCl = overload {
            mkCl : NP -> VP -> Cl = PredVP ;
            mkCl : VP -> Cl = \vp -> lin Cl (PredVP it_NP vp) ;
        } ;

        mkQCl = overload {
            mkQCl : Cl -> QCl = QuestCl ;
            mkQCl : IP -> VP -> QCl = QuestVP ;
            mkQCl : IAdv -> Cl -> QCl = QuestIAdv ;
        } ;

        mkVP = overload {
            mkVP : V2 -> NP -> VP = ComplV2 ;
            mkVP : AP -> VP = CompAP ;
            mkVP : Adv -> VP = CompAdv ;
            mkVP : VS -> S  -> VP = ComplVS ;
            mkVP : VQ -> QS -> VP = ComplVQ ;
            mkVP : VV -> VP -> VP = ComplVV ;
            mkVP : VP -> Adv -> VP = AdvVP ;
            mkVP : V -> VP = UseV ;
        } ;

        mkNP = overload {
            mkNP : Det -> CN -> NP = DetCN ;
            mkNP : Det -> N -> NP = \det,n -> lin NP (DetCN det (UseN n));
            mkNP : PN -> NP = UsePN ;
            mkNP : NP -> Adv -> NP = AdvNP ;
            mkNP : Conj -> NP -> NP -> NP = ConjNP ;
        } ;

        iNP : NP = i_NP ;
        youSgNP : NP = youSg_NP ;
        heNP : NP = he_NP ;
        sheNP : NP = she_NP ;
        weNP : NP = we_NP ;
        youPlNP : NP = youPl_NP ;
        theyNP : NP = they_NP ;
        no_oneNP : NP = no_one_NP ;
        nothingNP : NP = nothing_NP ;

        aSgDet : Det = a_Det ;
        aPlDet : Det = aPl_Det ;
        theSgDet : Det = theSg_Det ;
        thePlDet : Det = thePl_Det ;
        everyDet : Det = every_Det ;
        thisDet : Det = this_Det ;
        theseDet : Det = these_Det ;
        thatDet : Det = that_Det ;
        thoseDet : Det = those_Det ;
        noSgDet : Det = noSg_Det ;
        noPlDet : Det = noPl_Det ;

        mkCN = overload {
            mkCN : AP -> CN -> CN = ModCN ;
            mkCN : A -> CN -> CN = \a,cn -> lin CN (ModCN (UseA a) cn) ;
            mkCN : N -> CN = UseN ;
        } ;

        mkAdv = overload {
            mkAdv : Prep -> NP -> Adv = PrepNP ;
        } ;

        mkAP = overload {
            mkAP : AdA -> AP -> AP = AdAP ;
            mkAP : A -> AP = UseA ;
            mkAP : Conj -> AP -> AP -> AP = ConjAP ;
        } ;

        byPrep = by_Prep ;
        inPrep = in_Prep ;
        ofPrep = of_Prep ;
        withPrep = with_Prep ;
        toPrep = to_Prep ;
        canVV = can_VV ;
        mustVV = must_VV ;
        wantVV = want_VV ;
        althoughSubj = although_Subj ;
        -- becauseSubj = because_Subj ;
        -- whenSubj = when_Subj ;
        whenIAdv = when_IAdv ;
        -- whereIAdv = where_IAdv ;
        -- whyIAdv = why_IAdv ;

        positivePol = Pos ;
        negativePol = Neg ;
        presentTense = Pres ;
        pastTense = Past ;
        futureTense = Fut ;
        perfectTense = Perf ;

        and = and_Conj ;
        or = or_Conj ;

        hereAdv = here_Adv ;
}