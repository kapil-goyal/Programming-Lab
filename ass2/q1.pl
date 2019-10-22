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

my_loop(X, 0, Str, Res) :-
    Res = [].

my_loop(X, 1, Str, Res) :-
    [First_ele | []] = X,
    join_str(Str, First_ele, StrRes),
    % write(StrRes), nl,
    Res = [StrRes].

my_loop(X, Len, Str, Res) :-
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

dp(Str, 0, Res) :- 
    Res = [''].

dp(Str, 1, Res) :-
    name(Str, Strlist),
    [X|[]] = Strlist,
    W is (16 + X),
    name(Char, [W]),
    Res = [Char].

dp(Str, Len, Res) :-
    name(Str, Strlist),
    [X, Y | Z] = Strlist,
    Temp is ((X-48)*10 + (Y-48) + 64),
    is_Alpha(Temp, Is_Alpha),
    dp_util(Str, Len, Is_Alpha, Res2),
    name(RemStr, [Y | Z]),
    atom_length(RemStr, Length),
    dp(RemStr, Length, Res3),
    W is (16 + X),
    name(FirstChar, [W]),
    concat_str_in_list_str(Res3, FirstChar, Res4),
    append(Res2, Res4, Res).
    
dp_util(Str, Len, 0, Res) :-
    Res = [].

dp_util(Str, Len, 1, Res) :-
    name(Str, Strlist),
    [X, Y | RemStrList] = Strlist,
    Temp is ((X-48)*10 + (Y-48) + 64),
    name(RemStr, RemStrList),
    atom_length(RemStr, Length),
    dp(RemStr, Length, Res3),
    name(FirstChar, [Temp]),
    concat_str_in_list_str(Res3, FirstChar, Res).

decode(X) :-
    atom_length(X, Length),
    dp(X, Length, Res),
    length(Res, Ans),
    write(Ans), nl,
    write(Res).
    