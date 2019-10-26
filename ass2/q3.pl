edge_length(g1, g5, Res) :- Res is 4.
edge_length(g2, g5, Res) :- Res is 6.
edge_length(g3, g5, Res) :- Res is 8.
edge_length(g4, g5, Res) :- Res is 9.
edge_length(g1, g6, Res) :- Res is 10.
edge_length(g2, g6, Res) :- Res is 9.
edge_length(g3, g6, Res) :- Res is 3.
edge_length(g4, g6, Res) :- Res is 5.
edge_length(g5, g7, Res) :- Res is 3.
edge_length(g5, g10, Res) :- Res is 4.
edge_length(g5, g11, Res) :- Res is 6.
edge_length(g5, g12, Res) :- Res is 7.
edge_length(g5, g6, Res) :- Res is 7.
edge_length(g5, g8, Res) :- Res is 9.
edge_length(g6, g8, Res) :- Res is 2.
edge_length(g6, g12, Res) :- Res is 3.
edge_length(g6, g11, Res) :- Res is 5.
edge_length(g6, g10, Res) :- Res is 9.
edge_length(g6, g7, Res) :- Res is 10.
edge_length(g7, g10, Res) :- Res is 2.
edge_length(g7, g11, Res) :- Res is 5.
edge_length(g7, g12, Res) :- Res is 7.
edge_length(g7, g8, Res) :- Res is 10.
edge_length(g8, g9, Res) :- Res is 3.
edge_length(g8, g12, Res) :- Res is 3.
edge_length(g8, g11, Res) :- Res is 4.
edge_length(g8, g10, Res) :- Res is 8.
edge_length(g10, g15, Res) :- Res is 5.
edge_length(g10, g11, Res) :- Res is 2.
edge_length(g10, g12, Res) :- Res is 5.
edge_length(g11, g15, Res) :- Res is 4.
edge_length(g11, g13, Res) :- Res is 5.
edge_length(g11, g12, Res) :- Res is 4.
edge_length(g12, g13, Res) :- Res is 7.
edge_length(g12, g14, Res) :- Res is 8.
edge_length(g15, g13, Res) :- Res is 3.
edge_length(g13, g14, Res) :- Res is 4.
edge_length(g14, g17, Res) :- Res is 5.
edge_length(g14, g18, Res) :- Res is 4.
edge_length(g17, g18, Res) :- Res is 8.

get_edge_length(X, Y, Res) :-
    edge_length(X, Y, Res);
    edge_length(Y, X, Res).

is_edge(X, Y) :-
    get_edge_length(X, Y, _).

is_path(Source, Destination) :-
    is_edge(Source, Destination).

is_path(Source, Destination) :-
    is_edge(Source, Next),
    is_path(Next, Destination).

is_valid(_, 0) :-
    5 =:= 3.

is_valid(PathList, 1) :-
    [g17] = PathList.

is_valid(PathList, Len) :-
    [U, V | RemPathList] = PathList,
    is_edge(U, V),
    NewLen is Len-1,
    is_valid([V | RemPathList], NewLen).

valid(X) :-
    [FirstNode | _] = X,
    FirstNode = g1,
    length(X, Length),
    is_valid(X, Length).
    
valid(X) :-
    [FirstNode | _] = X,
    FirstNode = g2,
    length(X, Length),
    is_valid(X, Length).

valid(X) :-
    [FirstNode | _] = X,
    FirstNode = g3,
    length(X, Length),
    is_valid(X, Length).

valid(X) :-
    [FirstNode | _] = X,
    FirstNode = g4,
    length(X, Length),
    is_valid(X, Length).

get_path(Source, Destination, _, Result, PathLength, NewPathLength, Path) :-
    Source = Destination,
    NewPathLength is PathLength,
    Path = Result.

get_path(Source, Destination, Visited, Result, PathLength, NewPathLength, Path) :-
    \+ Source = Destination,
    append(Visited, [Source], NewVisited),    
    is_edge(Source, U),
    \+ member(U, Visited),
    get_edge_length(Source, U, EdgeLength),
    TempPathLength is (EdgeLength + PathLength),
    append(Result, [U], NewResult),    
    get_path(U, Destination, NewVisited, NewResult, TempPathLength, NewPathLength, Path).

get_all_valid_path(X, Path) :-
    get_path(g1, g17, [], [g1], 0, X, Path);
    get_path(g2, g17, [], [g2], 0, X, Path);
    get_path(g3, g17, [], [g3], 0, X, Path);
    get_path(g4, g17, [], [g4], 0, X, Path).

get_all_valid_non_cyclic_path :-
    forall(get_all_valid_path(_, Path), format('Path: ~w ~n', [Path])).   

get_cyclic_path(Source, Destination, Result, PathLength, NewPathLength, Path) :-
    Source = Destination,
    NewPathLength is PathLength,
    Path = Result.

get_cyclic_path(Source, Destination, Result, PathLength, NewPathLength, Path) :-
    \+ Source = Destination,
    is_edge(Source, U),
    get_edge_length(Source, U, EdgeLength),
    TempPathLength is (EdgeLength + PathLength),
    append(Result, [U], NewResult),    
    get_cyclic_path(U, Destination, NewResult, TempPathLength, NewPathLength, Path).

get_valid_cyclic_path(X, Path) :-
    (
        get_cyclic_path(g1, g17, [g1], 0, X, Path);
        get_cyclic_path(g2, g17, [g2], 0, X, Path);
        get_cyclic_path(g3, g17, [g3], 0, X, Path);
        get_cyclic_path(g4, g17, [g4], 0, X, Path) 
    ),
    format('Path: ~w ~n', [Path]).
    
optimal :-
    findall(X, get_all_valid_path(X, Path), L),
    min_member(Min, L),
    forall(get_all_valid_path(Min, Path), format('~w ~n', [Path])).
