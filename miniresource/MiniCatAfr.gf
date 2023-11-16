concrete MiniCatAfr of MiniCat = open MiniResAfr, Prelude in {
    lincat
    Utt = { s : Str } ;
    -- ClSlash ; -- clause missing noun phrase       e.g. "she walks with"
    -- Subj ;    -- subjunction                      e.g. "because"

    S  = { s : Order => Str } ; -- ; finNie : Bool } ;
    QS = { s : Order => Str ; o : Order ; qw : Str ; hasQw : Bool } ; -- ; finNie : Bool } ;
    QCl = { s : TTense => TPol => Order => Str ; o : Order ; qw : Str ; hasQw : Bool } ; -- ; finNie : Bool } ;

    Cl = { s : TTense => TPol => Order => Str } ; -- ; finNie : Bool } ;

    VP = MiniResAfr.VP ;

    NP = { s : Case => Str ; a : Agr ; isPron : Bool ; p : TPol } ; -- gender and number in order to make reflexive verbs work (die vrou verbeel haar, die kinders verbeel hulle)
    AP = { s : AForm => Str } ;

    PN = { s : Str ; a : Agr } ;
    CN = { s : Number => Str ; g : Gender } ;
    Det = {s : Str ; n : Number ; p : TPol } ;
    N = { s : Number => Str ; g : Gender } ;
    A = { s : AForm => Str } ;

    V = Verb ;
    V2 = { v : Verb ; c : Str ; hasC : Bool } ; -- c is die "na" van "kyk na"
    VS = { v : Verb ; c : Str } ; -- c is die "dat" van "weet dat"
    VV = Verb ;
    VQ = { v : Verb ; c : Str } ; -- c is die "of" van "wonder of"

    Adv = { s : Str ; p : TPol } ; -- polarity: altyd/nooit
    AdA = { s : Str} ;
    IAdv = { s : Str } ;

    IP = { s : Str } ;
    Prep = { s : Str } ;

    Pol = { s : Str ; p : TPol} ;
    Tense = { s : Str ; t : TTense} ;
    Conj = { s : Str } ;
}