
last_([X],X).
last_([_|Rest],Last):-
    last(Rest,Last).

inputNumbers([_],[]).
inputNumbers([E|Rest],[E|New]):-
    inputNumbers(Rest,New).

operation([E1|Rest],Output,TempOutput,Empty,OpList):-
    TTempOutput is TempOutput+E1,
    operation(Rest,Output,TTempOutput,["+"|Empty],OpList).

operation([E1,E2|Rest],Output,TempOutput,Empty,OpList):-
    TTempOutput is TempOutput + E1,
    TempElement is E2 * (-1), % Negating the right operand of the operation for next step
    operation([TempElement|Rest],Output,TTempOutput,["-"|Empty],OpList).


operation([E1,E2|Rest],Output,TempOutput,Empty,OpList):-
    TempElement is E1*E2,
    operation([TempElement|Rest],Output,TempOutput ,["*"|Empty],OpList).

operation([E1,E2|Rest],Output,TempOutput ,Empty,OpList):-
    TempElement is E1/E2,
    operation([TempElement|Rest],Output,TempOutput ,["/"|Empty],OpList).


operation([],X,X,[_|TempList],OpList):-
    reverse(["="|TempList],OpList).



getEquation(NumberList,Equation):-
    last_(NumberList,Output),
    inputNumbers(NumberList,InputList),
    operation(InputList,Output,0,[],OpList),
    mergeList(InputList,OpList,Equation,Output).

mergeList([],[],[X],X).
mergeList([E1|InputList],[E2|OpList],[E1,E2|Equation],Output):-
    mergeList(InputList,OpList,Equation,Output).

writeToFile([],_).

writeToFile([E|Rest],Stream):-
    write(Stream,E),
    writeToFile(Rest,Stream).

program(NumberList):-

    getEquation(NumberList,Equation),
    open("output.txt",write,Stream),
    writeToFile(Equation,Stream),
    close(Stream).


program():-

    open("input.txt",read,Stream),
    read_stream_to_codes(Stream, Codes),
    read_from_chars(Codes, Term),
    write(Term),
    close(Stream),
    getEquation(Term,Equation),
    open("output.txt",write,Stream2),
    writeToFile(Equation,Stream2),
    close(Stream2).




