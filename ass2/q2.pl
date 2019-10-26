starter('Corn Tikki', 30).
starter('Tomato Soup', 20).
starter('Chilli Paneer', 40).
starter('Crispy chicken', 40).
starter('Papdi Chaat', 20).
starter('Cold Drink', 20).

main_dish('Kadhai Paneer with Butter / Plain Naan', 50).
main_dish('Veg Korma with Butter / Plain Naan', 40).
main_dish('Murgh Lababdar with Butter / Plain Naan', 50).
main_dish('Veg Dum Biryani with Dal Tadka', 50).
main_dish('Steam Rice with Dal Tadka', 40).

dessert('Ice Cream', 20).
dessert('Malai Sandwich', 30).
dessert('Rasmalai', 10).

menu(hungry, 1, 1, 1).
menu(not_so_hungry, 0, 1, 1).
menu(not_so_hungry, 1, 1, 0).
menu(diet, 1, 0, 0).
menu(diet, 0, 1, 0).
menu(diet, 0, 0, 1).

less_than_equal_to_compare_str(X, Y) :-
    X = Y.

less_than_equal_to_compare_str(X, Y) :-
    \+ X = Y,
    name(X, StrListX),
    name(Y, StrListY),
    length(StrListX, LengthX),
    length(StrListY, _),
    LengthX = 0.

less_than_equal_to_compare_str(X, Y) :-
    \+ X = Y,
    name(X, StrListX),
    name(Y, StrListY),
    length(StrListX, LengthX),
    length(StrListY, LengthY),
    LengthX > 0,
    LengthY = 0,
    fail.

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

check_item_list(_, []).

check_item_list(Nutri, List) :-
    [X | []] = List,
    (
        dessert(X, NutriX);
        main_dish(X, NutriX);
        starter(X, NutriX)    
    ),
    NutriX =< Nutri.

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

get_dessert_item(0, Result) :-
    Result = [].

get_dessert_item(Count, Result) :-
    dessert(X, _),
    Count > 0,
    RemCount is Count - 1,
    get_dessert_item(RemCount, RemResult),
    Result = [X | RemResult].

get_dessert_items_nutri_count(Count, Nutri, Result) :-
    get_dessert_item(Count, ItemList),
    check_item_list(Nutri, ItemList),
    Result = ItemList.

make_dessert_items_for_diet(Count, Result) :-
    findall(TempResult, get_dessert_items_nutri_count(Count, 40, TempResult), CountResult),
    CountResult = [],
    Result = [].

make_dessert_items_for_diet(Count, Result) :-
    findall(TempResult, get_dessert_items_nutri_count(Count, 40, TempResult), CountResult),
    \+ CountResult = [],
    make_dessert_items_for_diet(Count+1, RemResult),
    append(CountResult, RemResult, Result).

get_main_dish_item(0, Result) :-
    Result = [].

get_main_dish_item(Count, Result) :-
    main_dish(X, _),
    Count > 0,
    RemCount is Count - 1,
    get_main_dish_item(RemCount, RemResult),
    Result = [X | RemResult].

get_main_dish_items_nutri_count(Count, Nutri, Result) :-
    get_main_dish_item(Count, ItemList),
    check_item_list(Nutri, ItemList),
    Result = ItemList.

make_main_dish_items_for_diet(Count, Result) :-
    findall(TempResult, get_main_dish_items_nutri_count(Count, 40, TempResult), CountResult),
    CountResult = [],
    Result = [].

make_main_dish_items_for_diet(Count, Result) :-
    findall(TempResult, get_main_dish_items_nutri_count(Count, 40, TempResult), CountResult),
    \+ CountResult = [],
    make_main_dish_items_for_diet(Count+1, RemResult),
    append(CountResult, RemResult, Result).

get_starter_item(0, Result) :-
    Result = [].

get_starter_item(Count, Result) :-
    starter(X, _),
    Count > 0,
    RemCount is Count - 1,
    get_starter_item(RemCount, RemResult),
    Result = [X | RemResult].

get_starter_items_nutri_count(Count, Nutri, Result) :-
    get_starter_item(Count, ItemList),
    check_item_list(Nutri, ItemList),
    Result = ItemList.

make_starter_items_for_diet(Count, Result) :-
    findall(TempResult, get_starter_items_nutri_count(Count, 40, TempResult), CountResult),
    CountResult = [],
    Result = [].

make_starter_items_for_diet(Count, Result) :-
    findall(TempResult, get_starter_items_nutri_count(Count, 40, TempResult), CountResult),
    \+ CountResult = [],
    make_starter_items_for_diet(Count+1, RemResult),
    append(CountResult, RemResult, Result).

find_items(hungry, 1, 1, 1) :-
    starter(X, _),
    main_dish(Y, _),
    dessert(Z, _),
    format('Items: [~w, ~w, ~w] ~n', [X, Y, Z]).

find_items(not_so_hungry, 0, 1, 1) :-
    dessert(X, NutriX),
    main_dish(Y, NutriY),
    (NutriX + NutriY) =< 80,
    format('Items: [~w, ~w] ~n', [X, Y]).

find_items(not_so_hungry, 1, 1, 0) :-
    starter(X, NutriX),
    main_dish(Y, NutriY),
    (NutriX + NutriY) =< 80,
    format('Items: [~w, ~w] ~n', [X, Y]).

find_items(diet, 1, 0, 0) :-
    make_starter_items_for_diet(1, Result),
    forall(member(X, Result), format('Items: ~w ~n', [X])).

find_items(diet, 0, 1, 0) :-
    make_main_dish_items_for_diet(1, Result),
    forall(member(X, Result), format('Items: ~w ~n', [X])).  

find_items(diet, 0, 0, 1) :-
    make_dessert_items_for_diet(1, Result),
    forall(member(X, Result), format('Items: ~w ~n', [X])).
    