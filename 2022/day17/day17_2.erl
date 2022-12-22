-module(day17_2).
-export([part2/0]).


maybe_move_left(Cave, Rock) ->
    NewRock = lists:map(fun (E) -> E bsl 1 end, Rock),
    TestList = lists:zip3(Rock, NewRock, lists:sublist(Cave, 1, length(NewRock))),
    case lists:all(fun({E1, E2, E3}) -> (E1 < 64) and (E2 band E3 =:= 0) end, TestList) of
        true -> NewRock;
        false -> Rock
    end.

maybe_move_right(Cave, Rock) ->
    NewRock = lists:map(fun (E) -> E bsr 1 end, Rock),
    TestList = lists:zip3(Rock, NewRock, lists:sublist(Cave, 1, length(NewRock))),
    case lists:all(fun({E1, E2, E3}) -> (E1 band 1 =:= 0) and (E2 band E3 =:= 0) end, TestList) of
        true -> NewRock;
        false -> Rock
    end.

can_move_down(Cave, Rock) when length(Rock) > length(Cave) -> false;
can_move_down(Cave, Rock) ->
    TestList = lists:zip(Rock, lists:sublist(Cave, 1, length(Rock))),
    lists:all(fun({E1,E2}) -> E1 band E2 =:= 0 end, TestList).

next_jet({[], JetPattern}) ->
    [Jet | Jets] = JetPattern,
    {Jet, {Jets, JetPattern}};
next_jet({[Jet| Jets], JetPattern}) -> {Jet, {Jets, JetPattern}}.

next_rock({[], RockShapes}) ->
    [Rock | Rocks] = RockShapes,
    {Rock, {Rocks, RockShapes}};
next_rock({[Rock| Rocks], RockShapes}) -> {Rock, {Rocks, RockShapes}}.

new_cave(CaveAbove, CaveBelow, []) ->
    L = lists:foldl(
        fun (Elem, Acc) -> [Elem | Acc] end,
        CaveBelow,
        CaveAbove
    ),
    lists:dropwhile(
        fun (0) -> true;
            (_) -> false
        end,
        L
    );
new_cave(CaveAbove, [CRow|CaveBelow], [RRow|Rock]) ->
    new_cave([CRow bor RRow| CaveAbove], CaveBelow, Rock).

simulate_falling_rock(CaveAbove, CaveBelow, Rock, Jets) ->
    {Jet, NewJets} = next_jet(Jets),
    NewRock = 
        case Jet of
            $< -> maybe_move_left(CaveBelow, Rock);
            $> -> maybe_move_right(CaveBelow, Rock);
            _ -> throw(wtf)
        end,
    [CRow | NewCaveBelow] = CaveBelow,
    case can_move_down(NewCaveBelow, NewRock) of
        false ->
            {NewJets, new_cave(CaveAbove, CaveBelow, NewRock)};
        true ->
            simulate_falling_rock([CRow | CaveAbove], NewCaveBelow, NewRock, NewJets)
    end.

remove_useless_cave([], _, NewCave) -> {0, lists:reverse(NewCave)};
remove_useless_cave(Removed, 127, NewCave) -> {length(Removed), lists:reverse(NewCave)};
remove_useless_cave([CRow|Cave], SoFar, NewCave) ->
    remove_useless_cave(Cave, SoFar bor CRow, [CRow | NewCave]).

simulate_falling_rocks(Cave, _, _, L, 0) ->
    length(Cave) + L;
simulate_falling_rocks(Cave, Jets, Rocks, L, Nb) ->
    
    {Rock, NewRocks} = next_rock(Rocks),

    CaveWithRocksAbove = lists:foldl(
        fun (_, Acc) -> [0 | Acc] end,
        [0, 0, 0| Cave],
        Rock
    ),

    {NewJets, NewCave} = simulate_falling_rock([], CaveWithRocksAbove, Rock, Jets),

    {NewL, CleanedCave} = remove_useless_cave(NewCave, 0, []),

    simulate_falling_rocks(CleanedCave, NewJets, NewRocks, NewL+L, Nb-1).

part2() ->
    {ok, ContentBin} = file:read_file("input_test.txt"),
    JetPattern = binary:bin_to_list(ContentBin),
    RockShapes = [[30], [8, 28, 8], [4, 4, 28], [16, 16, 16, 16], [24, 24]],
    simulate_falling_rocks([], {JetPattern, JetPattern}, {RockShapes, RockShapes}, 0, 10000000).