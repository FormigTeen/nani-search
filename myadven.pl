:- dynamic here/1.
:- dynamic location/2.
:- dynamic have/1.
:- dynamic turned_on/1.
:- dynamic turned_off/1.
:- dynamic door/3.
:- dynamic loc_list/2.

room(kitchen).
room(office).
room(hall).
room('dining room').
room(cellar).

object(candle, red, small, 1).
object(apple, red, small, 1).
object(apple, green, small, 1).
object(table, blue, big, 50).
object(desk, brown, big, 50).
object(flashlight, black, small, 1).
object('washing machine', gray, big, 60).
object(nani, yellow, small, 1).
object(broccoli, green, small, 1).
object(crackers, yellow, small, 1).
object(computer, black, big, 15).
object(envelope, white, small, 1).
object(stamp, red, small, 1).
object(key, gold, small, 1).

loc_list([apple, broccoli, crackers], kitchen).
loc_list([desk, computer], office).
loc_list([flashlight, envelope], desk).
loc_list([stamp, key], envelope).
loc_list(['washing machine'], cellar).
loc_list([nani], 'washing machine').
loc_list([], hall).

location(X, Y) :-
    loc_list(List, Y), member(X, List).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], X, X).
append([H|T], X, [H|O]) :- append(T, X, O).

door(office, hall, closed).
door(kitchen, office, closed).
door(hall, 'dining room', closed).
door(kitchen, cellar, closed).
door('dining room', kitchen, closed).

edible(object(apple, _, _, _)).
edible(object(crackers, _, _, _)).

tastes_yucky(object(broccoli, _, _, _)).

turned_off(flashlight).

here(kitchen).

where_food(X,Y) :-
    location(X,Y), edible(X).

where_food(X,Y) :-
    location(X,Y), tastes_yucky(X).

connect(X,Y, S) :- door(X,Y, S).
connect(X,Y, S) :- door(Y,X, S).

list_things(Place) :- 
    location(object(Thing, Color, Size, Weight), Place),
    write('A '),write(Size),tab(1),
    write(Color),tab(1),
    write(Thing),write(', weighing '),
    write_weights(Weight),nl,
    fail.

list_things(_).

list_connections(Place) :- connect(Place, X, S),tab(2),write(X),write(' and is '),write(S),nl,fail.
list_connections(_).

look_in(Place) :-
    write('On '),write(Place),write(' has '),nl,
    list_things(Place).

look :- 
    here(Place),write('You are in the '),write(Place),nl,
    write('You can see:'),nl,
    list_things(Place),
    write('You can go to:'),nl,
    list_connections(Place).

goto(Place) :-
    can_go(Place),
    move(Place),
    look.

can_go(Place) :-
    here(X),
    connect(X, Place, opened).

can_go(_) :-
    write("You can''t get there from here."),nl,
    fail.

move(Place) :-
    retract(here(_)),
    asserta(here(Place)).

take(X) :-
    can_take(X),
    take_object(X).

put_thing(Thing, Place) :-
    retract(loc_list(List, Place)),
    asserta(loc_list([Thing|List], Place)).

put(Thing) :-
    have(Thing),
    here(Place),
    retract(have(Thing)),
    asserta(location(Thing, Place)).

put(X) :-
    write('you dont have '),write(X),nl,fail.

can_take(Thing) :-
    here(Place),
    is_contained_in(Thing, Place).

can_take(Thing) :-
    write("There is no "),write(Thing),
    write(' here.'),
    nl,fail.


can_take_s(Thing) :-
    here(Place),
    location(object(Thing, _, small, _), Place).

can_take_s(Thing) :-
    here(Room),
    location(object(Thing, _, big, _), Room),
    write("The "),write(Thing),
    write(' is too big carry.'),nl,
    fail.

can_take_s(Thing) :-
    here(Room),
    not(location(object(Thing, _, _, _), Room)),
    write("There is no "),write(Thing),write('here'),nl,
    fail.

take_object(X) :-
    retract(location(X, _)),
    asserta(have(X)),
    write('taken'),nl.

inventory :-
    write('You have '),nl,
    tab(2),have(X),write(X),nl,
    fail.

inventory.

turn_on(Thing) :-
    retract(turned_off(Thing)),asserta(turned_on(Thing)),
    write(Thing),write(' is on'),nl.

turn_off(Thing) :-
    retract(turned_on(Thing)),asserta(turned_off(Thing)),
    write(Thing),write(' is off'),nl.

open_door(Y) :-
    here(X),
    connect(X, Y, _),
    (retract(door(X, Y, _)) ; retract(door(Y, X, _))),
    asserta(door(X, Y, opened)).

close_door(Y) :-
    here(X),
    connect(X, Y, _),
    (retract(door(X, Y, opened)) ; retract(door(Y, X, opened))),
    asserta(door(X, Y, closed)).


is_contained_in(T1, T2) :- location(T1, T2).
is_contained_in(T1, T2) :- location(X, T2), is_contained_in(T1, X). 

write_weights(1) :-
    write('1 pound').

write_weights(X) :-
    X > 1,
    write(X),write(' pounds').