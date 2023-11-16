abstract MiniCat = {
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
}