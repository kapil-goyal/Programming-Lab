
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate for storing facts about starters along with their nutrient values
starter('Corn Tikki', 30).
starter('Tomato Soup', 20).
starter('Chilli Paneer', 40).
starter('Crispy chicken', 40).
starter('Papdi Chaat', 20).
starter('Cold Drink', 20).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate for storing facts about main dishes along with their nutrient values
main_dish('Kadhai Paneer with Butter / Plain Naan', 50).
main_dish('Veg Korma with Butter / Plain Naan', 40).
main_dish('Murgh Lababdar with Butter / Plain Naan', 50).
main_dish('Veg Dum Biryani with Dal Tadka', 50).
main_dish('Steam Rice with Dal Tadka', 40).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate for storing facts about desserts along with their nutrient values
dessert('Ice Cream', 20).
dessert('Malai Sandwich', 30).
dessert('Rasmalai', 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate for storing possibilities for starters for different statuses
menu(hungry, 1, 1, 1).
menu(not_so_hungry, 0, 1, 1).
menu(not_so_hungry, 1, 1, 0).
menu(diet, 1, 0, 0).
menu(diet, 0, 1, 0).
menu(diet, 0, 0, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to find lexicographically smaller string
% Case when both strings are equal
less_than_equal_to_compare_str(X, Y) :-
    X = Y.
% Case when string X has size smaller than string Y
less_than_equal_to_compare_str(X, Y) :-
    \+ X = Y,
    name(X, StrListX),
    name(Y, StrListY),
    length(StrListX, LengthX),
    length(StrListY, _),
    LengthX = 0.
% Case when string Y has smaller size than string X
less_than_equal_to_compare_str(X, Y) :-
    \+ X = Y,
    name(X, StrListX),
    name(Y, StrListY),
    length(StrListX, LengthX),
    length(StrListY, LengthY),
    LengthX > 0,
    LengthY = 0,
    fail.
% Rule to implement recursion for the function for different cases for both strings to be compared 
less_than_equal_to_compare_str(X, Y) :-
    \+ X = Y,
    name(X, StrListX),
    name(Y, StrListY),
    length(StrListX, LengthX),
    length(StrListY, LengthY),
    LengthX > 0,
    LengthY > 0,
    [FirstCharX | TailX] = StrListX,
    [FirstCharY | TailY] = StrListY,
    FirstCharX = FirstCharY,
    name(NewX, TailX),
    name(NewY, TailY),
    less_than_equal_to_compare_str(NewX, NewY).

% Rule to return true or false based on current characters of both strings
less_than_equal_to_compare_str(X, Y) :-
    \+ X = Y,
    name(X, StrListX),
    name(Y, StrListY),
    length(StrListX, LengthX),
    length(StrListY, LengthY),
    LengthX > 0,
    LengthY > 0,
    [FirstCharX | _] = StrListX,
    [FirstCharY | _] = StrListY,
    FirstCharX < FirstCharY.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to check the nutrient value of currently considered dishes
% Case when list is empty
check_item_list(_, []).

% Case when list has only one item left
check_item_list(Nutri, List) :-
    [X | []] = List,
    (
        dessert(X, NutriX);
        main_dish(X, NutriX);
        starter(X, NutriX)    
    ),
    NutriX =< Nutri.

% Rule to recursively check the nutrient value of current list 
% and check for repetitions possible by different order of items in the list
check_item_list(Nutri, List) :-
    [X, Y | Tail] = List,
    (
        dessert(X, NutriX);
        main_dish(X, NutriX);
        starter(X, NutriX)    
    ),
    NutriX =< Nutri,
    less_than_equal_to_compare_str(X, Y),
    NutriRem is Nutri - NutriX,
    check_item_list(NutriRem, [Y | Tail]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to form a list of dessert items of given count from the list of available desserts
% Rule for handling empty list
get_dessert_item(0, Result) :-
    Result = [].

% Rule to recursively get all possible groups of dessert items of given count
get_dessert_item(Count, Result) :-
    dessert(X, _),
    Count > 0,
    RemCount is Count - 1,
    get_dessert_item(RemCount, RemResult),
    Result = [X | RemResult].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to get lists of available count and check that it doesn't exceed the upper bound
get_dessert_items_nutri_count(Count, Nutri, Result) :-
    get_dessert_item(Count, ItemList),
    check_item_list(Nutri, ItemList),
    Result = ItemList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to get all possible lists of dessert items with nutrition value not greater than 40
% Rule to handle base case of recursion
make_dessert_items_for_diet(Count, Result) :-
    findall(TempResult, get_dessert_items_nutri_count(Count, 40, TempResult), CountResult),
    CountResult = [],
    Result = [].

% Rule to recursively get the required list of dessert items
make_dessert_items_for_diet(Count, Result) :-
    findall(TempResult, get_dessert_items_nutri_count(Count, 40, TempResult), CountResult),
    \+ CountResult = [],
    make_dessert_items_for_diet(Count+1, RemResult),
    append(CountResult, RemResult, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to form a list of main dish items of given count from the list of available main dishes
% Rule for handling empty list
get_main_dish_item(0, Result) :-
    Result = [].

% Rule to recursively get all possible groups of main dish items of given count
get_main_dish_item(Count, Result) :-
    main_dish(X, _),
    Count > 0,
    RemCount is Count - 1,
    get_main_dish_item(RemCount, RemResult),
    Result = [X | RemResult].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to get lists of available count and check that it doesn't exceed the upper bound
get_main_dish_items_nutri_count(Count, Nutri, Result) :-
    get_main_dish_item(Count, ItemList),
    check_item_list(Nutri, ItemList),
    Result = ItemList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to get all possible lists of main dish items with nutrition value not greater than 40
% Rule to handle base case of recursion
make_main_dish_items_for_diet(Count, Result) :-
    findall(TempResult, get_main_dish_items_nutri_count(Count, 40, TempResult), CountResult),
    CountResult = [],
    Result = [].

% Rule to recursively get the required list of main dish items
make_main_dish_items_for_diet(Count, Result) :-
    findall(TempResult, get_main_dish_items_nutri_count(Count, 40, TempResult), CountResult),
    \+ CountResult = [],
    make_main_dish_items_for_diet(Count+1, RemResult),
    append(CountResult, RemResult, Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to form a list of starter items of given count from the list of available starters
% Rule for handling empty list
get_starter_item(0, Result) :-
    Result = [].

% Rule to recursively get all possible groups of starter items of given count
get_starter_item(Count, Result) :-
    starter(X, _),
    Count > 0,
    RemCount is Count - 1,
    get_starter_item(RemCount, RemResult),
    Result = [X | RemResult].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to get lists of available count and check that it doesn't exceed the upper bound
get_starter_items_nutri_count(Count, Nutri, Result) :-
    get_starter_item(Count, ItemList),
    check_item_list(Nutri, ItemList),
    Result = ItemList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to get all possible lists of starter items with nutrition value not greater than 40
% Rule to handle base case of recursion
make_starter_items_for_diet(Count, Result) :-
    findall(TempResult, get_starter_items_nutri_count(Count, 40, TempResult), CountResult),
    CountResult = [],
    Result = [].

% Rule to recursively get the required list of starter items
make_starter_items_for_diet(Count, Result) :-
    findall(TempResult, get_starter_items_nutri_count(Count, 40, TempResult), CountResult),
    \+ CountResult = [],
    make_starter_items_for_diet(Count+1, RemResult),
    append(CountResult, RemResult, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate to find out the possible combination of items from the selected categories
% Rule when the status is hungry and a dish from all categories is eaten
find_items(hungry, 1, 1, 1) :-
    forall(
        (
            starter(X, _),
            main_dish(Y, _),
            dessert(Z, _)
        ),
        format('Items: [~w, ~w, ~w] ~n', [X, Y, Z])
    ).

% Rule when status is not_so_hungry and starter is not eaten and nutrition value is not greater than 80
find_items(not_so_hungry, 0, 1, 1) :-
    forall((
        dessert(X, NutriX),
        main_dish(Y, NutriY),
        (NutriX + NutriY) =< 80
    ),
    format('Items: [~w, ~w] ~n', [X, Y])).

% Rule when status is not_so_hungry and dessert is not eaten and nutrition value is not greater than 80
find_items(not_so_hungry, 1, 1, 0) :-
    forall((
        starter(X, NutriX),
        main_dish(Y, NutriY),
        (NutriX + NutriY) =< 80
    ),
    format('Items: [~w, ~w] ~n', [X, Y])).

% Rule when status is diet and only starter is and nutrition value is not greater than 40
find_items(diet, 1, 0, 0) :-
    make_starter_items_for_diet(1, Result),
    forall(member(X, Result), format('Items: ~w ~n', [X])).

% Rule when status is diet and only main dish is and nutrition value is not greater than 40
find_items(diet, 0, 1, 0) :-
    make_main_dish_items_for_diet(1, Result),
    forall(member(X, Result), format('Items: ~w ~n', [X])).  

% Rule when status is diet and only dessert is and nutrition value is not greater than 40
find_items(diet, 0, 0, 1) :-
    make_dessert_items_for_diet(1, Result),
    forall(member(X, Result), format('Items: ~w ~n', [X])).
    