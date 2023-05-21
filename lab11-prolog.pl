% search2(?Elem, ?List)
% looks for two consecutive occurrences of Elem
search2(E, [E, E | T]) :- !.
search2(E, [H | T]) :- search2(E, T).

% search_two(?Elem, ?List)
% looks for two occurrences of Elem with any element in between !
search_two(E, [E, X, E | T]) :- !.
search_two(E, [H | T]) :- search_two(E, T).

% size(+List, -Size)
% Size will contain the number of elements in List
size([], 0).
size([H | T], N2) :- size(T, N), N2 is N+1.

% sum(+List , -Sum)
sum([], 0).
sum([H | T], R) :- sum(T, S), R is S+H.

% maxmin(+List, -Max, -Min)
% Max is the biggest element in List
% Min is the smallest element in List
% Suppose the list has at least one element
maxmin([H | T], MX, MN) :- maxmin(T, H, H, MX, MN).
maxmin([], AMN, AMX, AMN, AMX).
maxmin([H | T], AMN, AMX, MN, MX) :- H =< AMN, !, maxmin(T, H, AMX, MN, MX).
maxmin([H | T], AMN, AMX, MN, MX) :- H >= AMX, !, maxmin(T, AMN, H, MN, MX).
maxmin([H | T], AMN, AMX, MN, MX) :- maxmin(T, AMN, AMX, MN, MX).

% sublist(?List1, ?List2)
% List1 should contain elements all also in List2
sublist([], L).
sublist([H | T], L) :- member(H, L), !, sublist(T, L).

% dropAny(+Elem, +List, -OutList)
dropAny(X, [X | T], T).
dropAny(X, [H | Xs], [H | L]) :- dropAny(X, Xs, L).

% dropFirst(+Elem, +List, -OutList)
dropFirst(X, [X | T], T) :- !.
dropFirst(X, [H, Xs], [H, L]) :- dropFirst(X, Xs, L).

% dropLast(+Elem, +List, -OutList)
dropLast(X, [H | Xs], [H | L]) :- dropLast(X, Xs, L), !.
dropLast(X, [X | T], T).

% dropAll(+Elem, +List, -OutList)
dropAll(_, [], []).
dropAll(X, [Y | T], R) :- copy_term(X, Y), !, dropAll(X, T, R).
dropAll(X, [H | Xs], [H | L]) :- H \= X, dropAll(X, Xs, L).

% fromList(+List, -Graph)
fromList([_], []).
fromList([H1, H2 | T], [e(H1, H2) | L]) :- fromList([H2 | T], L).

% fromCircList(+List, -Graph)
fromCircList([H | T], R) :- fromCircList([H | T], H, R).
fromCircList([T], Ls, [e(T, Ls)]).
fromCircList([H1, H2 | T], Ls, [e(H1, H2) | L]) :- fromCircList([H2 | T], Ls, L).

% outDegree(+Graph, +Node, -Deg)
% Deg is the number of edges leading into Node
outDegree([], E, 0).
outDegree([e(E, _) | T], E, R2) :- !, outDegree(T, E, R), R2 is R+1.
outDegree([e(X, _) | T], E, R) :- X \= E, !, outDegree(T, E, R).


% dropNode(+Graph, +Node, -OutGraph)
% drop all edges starting and leaving from a Node
dropNode(G, N, R) :- dropAll(e(N, _), G, G2), dropAll(e(_ , N), G2, R).

% reaching(+Graph, +Node, -List)
% all the nodes that can be reached in 1 step from Node
reaching([], _, []).
reaching(L, N, R) :- findall(X, member(e(N, X), L), R).

% nodes(+Graph, -Nodes)
% Create a list of all nodes (no duplicates) in the graph(inverse of fromList)
nodes([], []).
nodes([e(H1, H2) | T], L) :- nodes(T, L), member(H1, L), member(H2, L).
nodes([e(H1, H2) | T], [H2 | L]) :- nodes(T, L), member(H1, L).
nodes([e(H1, H2) | T], [H1 | L]) :- nodes(T, L), member(H2, L).



