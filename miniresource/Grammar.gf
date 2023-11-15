abstract Grammar = {

  flags startcat = Utt ;

  cat
    Utt ;     -- utterance (sentence or question) e.g. "does she walk"
  --   ClSlash ; -- clause missing noun phrase       e.g. "she walks with"
    Subj ;    -- subjunction                      e.g. "because"
  --   IAdv ;    -- interrogative adverb             e.g. "why"

    S ;     -- sentence
    QS ;    -- question (fixed tense)           e.g. "who doesn't walk"
    QCl ;   -- question clause (variable tense) e.g. "who walks"
    Cl ;    -- clause
    VP ;    -- verb phrase
    NP ;    -- noun phrase
    AP ;    -- adjectival phrase

    PN ;    -- proper name                      e.g. "John"
    CN ;    -- common noun
    Det ;   -- determiner
    N ;     -- noun
    A ;     -- adjective

    V ;     -- verb (one-place, intransitive)
    V2 ;    -- two-place verb (two-place, transitive or prepositional)
    VS ;    -- sentence-complement verb         e.g. "know"
    VV ;    -- verb-phrase-complement verb      e.g. "want"
    VQ ;    -- question-complement verb         e.g. "wonder"

    Adv ;   -- adverb                           e.g. "here"
    AdA ;   -- ad-adjective                     e.g. "very"
    IAdv ;

    Prep ;    -- preposition (and/or case)        e.g. "with"
    IP ;    -- interrogative pronoun            e.g. "who"

    Tense ; -- tense
    Pol ;   -- polarity
    Conj ;  -- conjunction                     e.g. "and"

  fun
    UttS  : S -> Utt ;
    UttQS : QS -> Utt ;

    UseCl  : Tense -> Pol -> Cl -> S ;
    UseQCl : Tense -> Pol -> QCl -> QS ;

    PredVP  : NP -> VP -> Cl ;
    -- SubjCl : Cl -> Subj -> S -> Cl ;     -- she walks because we run

    QuestCl    : Cl -> QCl ;             -- does she walk
    QuestVP    : IP -> VP -> QCl ;       -- who walks
  --   QuestSlash : IP -> ClSlash -> QCl ;  -- who does she walk with
    QuestIAdv  : IAdv -> Cl -> QCl ;     -- why does she walk

  --   SlashV2   : NP -> V2 -> ClSlash ;   -- she loves
  --   SlashPrep : Cl -> Prep -> ClSlash ; -- she walks with

    ComplV2 : V2 -> NP -> VP ;
    CompAP  : AP -> VP ;
    -- CompAdv : Adv -> VP ;         -- be here
    ComplVS : VS -> S  -> VP ;  -- know that she walks
    ComplVQ : VQ -> QS -> VP ;  -- wonder who walks
    ComplVV : VV -> VP -> VP ;  -- want to walk
    AdvVP : VP -> Adv -> VP ; -- walk in the city
    UseV    : V -> VP ;

    DetCN   : Det -> CN -> NP ;
    ModCN   : AP -> CN -> CN ;

    UsePN : PN -> NP ;        -- John
    AdvNP : NP -> Adv -> NP ; -- the man in the city

    PrepNP  : Prep -> NP -> Adv ; -- in the house
    AdAP    : AdA -> AP -> AP ;

    ConjS   : Conj -> S  -> S  -> S ;
    ConjAP  : Conj -> AP -> AP -> AP ;
    ConjNP  : Conj -> NP -> NP -> NP ;

    UseN    : N -> CN ;
    UseA    : A -> AP ;

    a_Det, theSg_Det, thePl_Det, every_Det : Det ;
    this_Det, these_Det : Det ;
    that_Det, those_Det : Det ;
    noSg_Det : Det ;
    noPl_Det : Det ;

    i_NP, youSg_NP, he_NP, she_NP, we_NP, youPl_NP, they_NP : NP ;
    no_one_NP : NP ;
    nothing_NP : NP ;
    very_AdA : AdA ;

    who_IP  : IP ;
    here_Adv : Adv ;
    by_Prep, in_Prep, of_Prep, with_Prep, to_Prep : Prep ;
    can_VV, must_VV, want_VV : VV ;
    although_Subj, because_Subj, when_Subj : Subj ;
    when_IAdv, where_IAdv, why_IAdv : IAdv ;

     Pos, Neg : Pol ;
     Pres, Past, Fut, Perf : Tense ;

     and_Conj, or_Conj : Conj ;

 }
