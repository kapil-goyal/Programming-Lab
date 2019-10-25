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

find_items(hungry, 1, 1, 1) :-
    starter(X, _),
    main_dish(Y, _),
    dessert(Z, _),
    format('Items: ~w, ~w, ~w. ~n', [X, Y, Z]).

find_items(not_so_hungry, 0, 1, 1) :-
    dessert(X, NutriX),
    main_dish(Y, NutriY),
    (NutriX + NutriY) =< 80,
    format('Items: ~w, ~w. ~n', [X, Y]).

find_items(not_so_hungry, 1, 1, 0) :-
    starter(X, NutriX),
    main_dish(Y, NutriY),
    (NutriX + NutriY) =< 80,
    format('Items: ~w, ~w. ~n', [X, Y]).

find_items(diet, 1, 0, 0) :-
    starter(X, NutriX),
    starter(Y, NutriY),
    (NutriX + NutriY) =< 40,
    format('Items: ~w ,~w. ~n', [X, Y]).

find_items(diet, 1, 0, 0) :-
    starter(X, NutriX),
    (NutriX) =< 40,
    format('Items: ~w. ~n', [X]).

find_items(diet, 0, 1, 0) :-
    main_dish(X, NutriX),
    (NutriX) =< 40,
    format('Items: ~w. ~n', [X]).

find_items(diet, 0, 0, 1) :-
    dessert(X, NutriX),
    NutriX =< 40,
    format('Items: ~w. ~n', [X]).

find_items(diet, 0, 0, 1) :-
    dessert(X, NutriX),
    dessert(Y, NutriY),
    (NutriX + NutriY) =< 40,
    format('Items: ~w, ~w. ~n', [X, Y]).

find_items(diet, 0, 0, 1) :-
    dessert(X, NutriX),
    dessert(Y, NutriY),
    dessert(Z, NutriZ),
    (NutriX + NutriY + NutriZ) =< 40,
    format('Items: ~w, ~w, ~w. ~n', [X, Y, Z]).

find_items(diet, 0, 0, 1) :-
    dessert(W, NutriW),
    dessert(X, NutriX),
    dessert(Y, NutriY),
    dessert(Z, NutriZ),
    (NutriW + NutriX + NutriY + NutriZ) =< 40,
    format('Items: ~w, ~w, ~w, ~w. ~n', [W, X, Y, Z]).

