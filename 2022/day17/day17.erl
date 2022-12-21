-module(day17).
-export([part1/0]).

can_move_left(_, _, 0) -> false;
can_move_left(_, [], _) -> true;
can_move_left([CRow |Cave], [RRow | Rock], HPos) ->
    PaddedRow = lists:duplicate(HPos - 1, $.) ++ RRow ++ lists:duplicate(7 - HPos + 1 - length(RRow), $.),
    CanMoveRow = lists:all(
        fun ({$#, $#}) -> false;
            (_) -> true
        end,
        lists:zip(CRow, PaddedRow)),
    case CanMoveRow of
        true -> can_move_left(Cave, Rock, HPos);
        false -> false
    end.
    
can_move_right(_, [RRow | _], HPos) when HPos + length(RRow) == 7 ->
    false;
can_move_right(_, [], _) -> true;
can_move_right([CRow |Cave], [RRow | Rock], HPos) ->
    PaddedRow = lists:duplicate(HPos + 1, $.) ++ RRow ++ lists:duplicate(7 - HPos - 1 - length(RRow), $.),
    CanMoveRow = lists:all(
        fun ({$#, $#}) -> false;
            (_) -> true
        end,
        lists:zip(CRow, PaddedRow)),
    case CanMoveRow of
        true -> can_move_right(Cave, Rock, HPos);
        false -> false
    end.


can_move_down(_, [], _) -> true;
can_move_down([], _, _) -> false;
can_move_down([CRow| Cave], [RRow | Rock], HPos) ->
    PaddedRow = lists:duplicate(HPos, $.) ++ RRow ++ lists:duplicate(7 - HPos - length(RRow), $.),
    CanMoveRow = lists:all(
        fun ({$#, $#}) -> false;
            (_) -> true
        end,
        lists:zip(CRow, PaddedRow)),
    case CanMoveRow of
        true -> can_move_down(Cave, Rock, HPos);
        false -> false
    end.

next_jet({[], JetPattern}) ->
    [Jet | Jets] = JetPattern,
    {Jet, {Jets, JetPattern}};
next_jet({[Jet| Jets], JetPattern}) -> {Jet, {Jets, JetPattern}}.

next_rock({[], RockShapes}) ->
    [Rock | Rocks] = RockShapes,
    {Rock, {Rocks, RockShapes}};
next_rock({[Rock| Rocks], RockShapes}) -> {Rock, {Rocks, RockShapes}}.

new_cave(CaveAbove, CaveBelow, [], _HPos) ->
    L = lists:foldl(
        fun (Elem, Acc) -> [Elem | Acc] end,
        CaveBelow,
        CaveAbove
    ),
    lists:dropwhile(
        fun (".......") -> true;
            (_) -> false
        end,
        L
    );
new_cave(CaveAbove, [CRow|CaveBelow], [RRow|Rock], HPos) ->
    PaddedRow = lists:duplicate(HPos, $.) ++ RRow ++ lists:duplicate(7 - HPos - length(RRow), $.),
    NewCRow = lists:foldr(
        fun ({$., $.}, Acc) -> [$.| Acc];
            ({$#, $#}, _) -> throw(no);
            (_, Acc) -> [$#| Acc]
        end,
        [],
        lists:zip(PaddedRow, CRow)
    ),
    new_cave([NewCRow| CaveAbove], CaveBelow, Rock, HPos).


simulate_falling_rock(CaveAbove, CaveBelow, Rock, HPos, Jets) ->
    {Jet, NewJets} = next_jet(Jets),
    NewHPos = 
        case Jet of
            $< ->
                case can_move_left(CaveBelow, Rock, HPos) of
                    true -> HPos-1;
                    false -> HPos
                end;
            $> ->
                case can_move_right(CaveBelow, Rock, HPos) of
                    true -> HPos+1;
                    false -> HPos
                end;
            _ -> throw(wtf)
        end,
    [CRow | NewCaveBelow] = CaveBelow,
    case can_move_down(NewCaveBelow, Rock, NewHPos) of
        false ->
            {NewJets, new_cave(CaveAbove, CaveBelow, Rock, NewHPos)};
        true ->
            simulate_falling_rock([CRow | CaveAbove], NewCaveBelow, Rock, NewHPos, NewJets)
    end. 


simulate_falling_rocks(Cave, _, _, 0) -> length(Cave);
simulate_falling_rocks(Cave, Jets, Rocks, Nb) ->
    
    {Rock, NewRocks} = next_rock(Rocks),

    CaveWithRocksAbove = lists:foldl(
        fun (_, Acc) -> ["......." | Acc] end,
        [".......", ".......", "......."] ++ Cave,
        Rock
    ),

    {NewJets, NewCave} = simulate_falling_rock([], CaveWithRocksAbove, Rock, 2, Jets),

    simulate_falling_rocks(NewCave, NewJets, NewRocks, Nb-1).



part1() ->
    {ok, ContentBin} = file:read_file("input.txt"),
    JetPattern = binary:bin_to_list(ContentBin),
    RockShapes = [["####"], [".#.", "###", ".#."], ["..#", "..#", "###"], ["#", "#", "#", "#"], ["##", "##"]],
    simulate_falling_rocks([], {JetPattern, JetPattern}, {RockShapes, RockShapes}, 2022).
