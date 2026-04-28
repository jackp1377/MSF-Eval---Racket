import Std.Diagnostics.DumpMachine;

operation OracleBB(q1 : Qubit, q2 : Qubit) : Unit is Adj {
    I(q1);
    I(q2);
    CZ(q1, q2);
}

operation OracleWB(q1 : Qubit, q2 : Qubit) : Unit is Adj {
    X(q1);
    CZ(q1, q2);
    X(q1);
}

operation groverSearch(oracle:((Qubit, Qubit) => Unit)) : Unit  {
    use (q1, q2) = (Qubit(), Qubit());

    H(q1);
    H(q2);

    oracle(q1, q2);

    Z(q1);
    Z(q2);
    CZ(q1, q2);
    
    H(q1);
    H(q2);

    DumpMachine();

    Reset(q1);
    Reset(q2);

} 

operation groverSearchDisplay() : Unit {
        use (q1, q2) = (Qubit(), Qubit());

    H(q1);
    H(q2);

    // oracle(q1, q2);
    

    Z(q1);
    Z(q2);
    CZ(q1, q2);
    
    H(q1);
    H(q2);

    DumpMachine();

    Reset(q1);
    Reset(q2);
}

@EntryPoint()
operation Main() : Unit {
    // groverSearch(OracleBB);
    groverSearch(OracleWB);
}

