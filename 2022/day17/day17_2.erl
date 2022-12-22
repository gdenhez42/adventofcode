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

simulate_falling_rocks(Cave, Jets, Rocks, 0) ->
    {Cave, Jets, Rocks};
simulate_falling_rocks(Cave, Jets, Rocks, Nb) ->
    
    {Rock, NewRocks} = next_rock(Rocks),

    CaveWithRocksAbove = lists:foldl(
        fun (_, Acc) -> [0 | Acc] end,
        [0, 0, 0| Cave],
        Rock
    ),

    {NewJets, NewCave} = simulate_falling_rock([], CaveWithRocksAbove, Rock, Jets),

    simulate_falling_rocks(NewCave, NewJets, NewRocks, Nb-1).


find_repeat_idx(_, []) -> false;
find_repeat_idx({BigCave, Jets, BigNb}=E, [{SmallCave, Jets, SmallNb} | Memo]) ->
    BigCaveLen = length(BigCave),
    SmallCaveLen = length(SmallCave),
    Diff = BigCaveLen - SmallCaveLen,
    case Diff >= SmallCaveLen of
        true -> find_repeat_idx(E, Memo);
        false ->
            PotentialRepeatBig = lists:sublist(BigCave, 1, Diff),
            StartBig = lists:sublist(BigCave, Diff*2 + 1, BigCaveLen),
            PotentialRepeatSmall = lists:sublist(SmallCave, 1, Diff),
            StartSmall = lists:sublist(SmallCave, Diff + 1, SmallCaveLen),
            case {PotentialRepeatBig, StartBig} of
                {PotentialRepeatSmall, StartSmall} -> {SmallNb, BigNb-SmallNb, Diff};
                _ -> find_repeat_idx(E, Memo)
            end
    end;
find_repeat_idx(E, [_ | Memo]) -> find_repeat_idx(E, Memo).

find_repeat(Cave, Jets, Rocks, Nb, Memo) ->
    {NewCave, NewJets, NewRocks} = simulate_falling_rocks(Cave, Jets, Rocks, 5),
    case find_repeat_idx({NewCave, NewJets, Nb+5}, Memo) of
        false -> find_repeat(NewCave, NewJets, NewRocks, Nb + 5, [{NewCave, NewJets, Nb+5} | Memo]);
        Idx -> Idx
    end.

cave_heigth(Jets, Rocks, Nb) ->
    {NewCave, _, _} = simulate_falling_rocks([], Jets, Rocks, Nb),
    length(NewCave).

part2() ->
    {ok, ContentBin} = file:read_file("input.txt"),
    JetPattern = binary:bin_to_list(ContentBin),
    RockShapes = [[30], [8, 28, 8], [4, 4, 28], [16, 16, 16, 16], [24, 24]],
    Jets = {JetPattern, JetPattern},
    Rocks = {RockShapes, RockShapes},
    {NbRockInit, NbRockRepeat, HeightRepeat} = find_repeat([], Jets, Rocks, 0, []),
    NbRockTotal = 1000000000000,
    NbRockRem = (NbRockTotal - NbRockInit) rem NbRockRepeat,
    HeightRem = cave_heigth(Jets, Rocks, NbRockInit + NbRockRem),
    ((NbRockTotal - NbRockInit) div NbRockRepeat) * HeightRepeat + HeightRem.

