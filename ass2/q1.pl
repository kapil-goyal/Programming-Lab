%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate is used to check if the character is an Alphabet or not.
% Params:
%    Num : Ascii Value of character
%    Res : True if it is an alphabet
%                else fails
% 
% Ascii value is greater than 'Z'

is_Alpha(Num, Res) :-
    Num > 90,
    Res is 0.

% Ascii value is less than 'A'

is_Alpha(Num, Res) :-
    Num < 65,
    Res is 0.

% Ascii Value lies in between 'A' to 'Z'

is_Alpha(Num, Res) :-
    Num >= 65,
    Num =< 90,
    Res is 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This Predicate concatenates two strings

join_str(Str1, Str2, Str3) :-
    name(Str1, Strlist1),
    name(Str2, Strlist2),
    append(Strlist1, Strlist2, Strlist3),
    name(Str3, Strlist3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate is used in predicate concat_str_in_list_str

% No element left in list of strings

my_loop(_, 0, _, Res) :-
    Res = [].

% Single element left in list of strings

my_loop(X, 1, Str, Res) :-
    [First_ele | []] = X,
    join_str(Str, First_ele, StrRes),
    % write(StrRes), nl,
    Res = [StrRes].

% Recursive loop to iterate in each element in list of strings

my_loop(X, _, Str, Res) :-
    [First_ele | Res_list] = X,
    join_str(Str, First_ele, StrRes),
    % write(StrRes), nl,
    length(Res_list, Length),
    my_loop(Res_list, Length, Str, Res2),
    Res = [StrRes | Res2].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate calls my_loop and join string in list of strings
% params :-
%       X : List of strings
%       Str : String to be concatenated
%       Res: To store the resultant list

concat_str_in_list_str(X, Str, Res) :-
    length(X, Length),
    my_loop(X, Length, Str, Res).
    % write(Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate is used to make recursion calls on the given string

% Base Case 1: When complete string is used 

dp(_, 0, Res) :- 
    Res = [''].

% Base Case 2: When first character of string is '0'

dp(Strlist, _, Res) :-
    [X|_] = Strlist,
    X = 48,
    Res = [].

% Base Case 3: When First character is not zero and only one character is left

dp(Strlist, 1, Res) :-
    [X|_] = Strlist,
    \+ X = 48,
    W is (16 + X),
    name(Char, [W]),
    Res = [Char].

% Main Recursion: 
%       Get first character
%       Call Recursion for remaining string
%       If first two characters can make an alphabet then call dp_util
%       Concatenate first character with the Result of remaining string

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

% Base Case 1: No string left  

dp_util(_, 0, Res) :-
    Res = [].

% Base Case 2: It is possible to make a character from first two letters and first character is '0'

dp_util(Strlist, 1, Res) :-
    [X|_] = Strlist,
    X = 48,
    Res = [].

% Main Recursion: It is possible to make a character from first two letters and first character is not '0'

dp_util(Strlist, 1, Res) :-
    [X, Y | RemStrList] = Strlist,
    \+ X = 48,
    Temp is ((X-48)*10 + (Y-48) + 64),
    length(RemStrList, Length),
    dp(RemStrList, Length, Res3),
    name(FirstChar, [Temp]),
    concat_str_in_list_str(Res3, FirstChar, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate is the main function of the program
% X is given input string of numbers

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate checks if the given character is number or not

is_num(Char) :-
    Char >= 48,
    Char =< 57.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate checks if the length of the input string is positive or not.
% If not print error message.

check_length(StrList) :-
    length(StrList, Length),
    Length > 0.

check_length(_) :-
    format('Please enter valid input. ~n'),
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate checks that thed given input only contains numbers.
% If not then print error message

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
