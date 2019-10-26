is_Alpha(Num, Res) :-
    Num > 90,
    Res is 0.

is_Alpha(Num, Res) :-
    Num < 65,
    Res is 0.

is_Alpha(Num, Res) :-
    Num >= 65,
    Num =< 90,
    Res is 1.

join_str(Str1, Str2, Str3) :-
    name(Str1, Strlist1),
    name(Str2, Strlist2),
    append(Strlist1, Strlist2, Strlist3),
    name(Str3, Strlist3).

my_loop(_, 0, _, Res) :-
    Res = [].

my_loop(X, 1, Str, Res) :-
    [First_ele | []] = X,
    join_str(Str, First_ele, StrRes),
    % write(StrRes), nl,
    Res = [StrRes].

my_loop(X, _, Str, Res) :-
    [First_ele | Res_list] = X,
    join_str(Str, First_ele, StrRes),
    % write(StrRes), nl,
    length(Res_list, Length),
    my_loop(Res_list, Length, Str, Res2),
    Res = [StrRes | Res2].

concat_str_in_list_str(X, Str, Res) :-
    length(X, Length),
    my_loop(X, Length, Str, Res).
    % write(Res).

dp(_, 0, Res) :- 
    Res = [''].

dp(Strlist, _, Res) :-
    [X|_] = Strlist,
    X = 48,
    Res = [].

dp(Strlist, 1, Res) :-
    [X|_] = Strlist,
    \+ X = 48,
    W is (16 + X),
    name(Char, [W]),
    Res = [Char].

dp(Strlist, _, Res) :-
    [X, Y | Z] = Strlist,
    \+ X = 48,
    Temp is ((X-48)*10 + (Y-48) + 64),
    is_Alpha(Temp, Is_Alpha),
    dp_util(Strlist, Is_Alpha, Res2),
    NewStrList = [Y | Z],
    length(NewStrList, Length),
    dp(NewStrList, Length, Res3),
    W is (16 + X),
    name(FirstChar, [W]),
    concat_str_in_list_str(Res3, FirstChar, Res4),
    append(Res2, Res4, Res).
    
dp_util(_, 0, Res) :-
    Res = [].

dp_util(Strlist, 1, Res) :-
    [X|_] = Strlist,
    X = 48,
    Res = [].

dp_util(Strlist, 1, Res) :-
    [X, Y | RemStrList] = Strlist,
    \+ X = 48,
    Temp is ((X-48)*10 + (Y-48) + 64),
    length(RemStrList, Length),
    dp(RemStrList, Length, Res3),
    name(FirstChar, [Temp]),
    concat_str_in_list_str(Res3, FirstChar, Res).

decode(X) :-
    name(X, StrList),
    check_invalid_input(StrList),
    check_length(StrList),
    length(StrList, Length),
    dp(StrList, Length, Res),
    length(Res, Ans),
    format('Possible no. of strings: ~w ~n', [Ans]),
    format('Possible strings: ~n'),
    write(Res).

is_num(Char) :-
    Char >= 48,
    Char =< 57.

check_length(StrList) :-
    length(StrList, Length),
    Length > 0.

check_length(_) :-
    format('Please enter valid input. ~n'),
    fail.

check_invalid_input([]).

check_invalid_input(StrList) :-
    [Head | Tail] = StrList,
    is_num(Head),
    check_invalid_input(Tail).

check_invalid_input(StrList) :-
    [Head | _] = StrList,
    \+ is_num(Head),
    format('Please enter valid input. ~n'),
    fail.
    