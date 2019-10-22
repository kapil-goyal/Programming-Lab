join_str(Str1, Str2, Str3) :-
    name(Str1, Strlist1),
    name(Str2, Strlist2),
    append(Strlist1, Strlist2, Strlist3),
    name(Str3, Strlist3).

my_loop(X, 0, Str, Res) :-
    Res = [].

my_loop(X, 1, Str, Res) :-
    [First_ele | []] = X,
    join_str(First_ele, Str, StrRes),
    % write(StrRes), nl,
    Res = [StrRes].

my_loop(X, Len, Str, Res) :-
    [First_ele | Res_list] = X,
    join_str(First_ele, Str, StrRes),
    % write(StrRes), nl,
    length(Res_list, Length),
    my_loop(Res_list, Length, Str, Res2),
    Res = [StrRes | Res2].

concat_str_in_list_str(X, Str, Res) :-
    length(X, Length),
    my_loop(X, Length, Str, Res).
    % write(Res).

run_prog(X, Str) :-
    concat_str_in_list_str(X, Str, Res),
    write(Res).