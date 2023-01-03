with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;

procedure day24 is

    type Position is record
        X: Integer;
        Y: Integer;
    end record;

    function "<" (L, R: Position) return Boolean is
    begin
        return (L.Y < R.Y) or
               (L.Y = R.Y and L.X < R.X);
    end "<";

    procedure Display_Position (C: Position) is
    begin
        Ada.Text_IO.Put_Line ("x:" & Integer'Image (C.X) & ", y:" & Integer'Image (C.Y));
    end Display_Position;

    type Direction is
        (Left, Right, Up, Down);

    type Blizzard is record
        X: Integer;
        Y: Integer;
        D: Direction;
    end record;

    function "<" (L, R: Blizzard) return Boolean is
    begin
        return (L.Y < R.Y) or
               (L.Y = R.Y and L.X < R.X) or
               ((L.Y = R.Y and L.X = R.X) and L.D < R.D);
    end "<";

    procedure Display_Blizzard (C: Blizzard) is
    begin
        Ada.Text_IO.Put_Line ("x:" & Integer'Image (C.X)
                              & ", y:" & Integer'Image (C.Y)
                              & ", dir:" & Direction'Image (C.D));
    end Display_Blizzard;

    function Move_Blizzard (C: Blizzard; Valley_H: Integer; Valley_W: Integer) return Blizzard is
    begin
        case C.D is
            when Right =>
                if C.X = Valley_W - 1 then
                    return (2, C.Y, Right);
                else
                    return (C.X + 1, C.Y, Right);
                end if;
            when Left =>
                if C.X = 2 then
                    return (Valley_W - 1, C.Y, Left);
                else
                    return (C.X - 1, C.Y, Left);
                end if;
            when Down =>
                if C.Y = Valley_H - 1 then
                    return (C.X, 2, Down);
                else
                    return (C.X, C.Y + 1, Down);
                end if;
            when Up =>
                if C.Y = 2 then
                    return (C.X, Valley_H - 1, Up);
                else
                    return (C.X, C.Y - 1, Up);
                end if;
        end case;
    end Move_Blizzard;

    package Position_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Position);

    package Blizzard_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Blizzard);

    function Has_Blizzard (X: Integer; Y: Integer; Blizzards: Blizzard_Sets.Set) return Boolean is
    begin
        return Blizzard_Sets.Contains (Blizzards, (X, Y, Right)) or
               Blizzard_Sets.Contains (Blizzards, (X, Y, Left)) or
               Blizzard_Sets.Contains (Blizzards, (X, Y, Up)) or
               Blizzard_Sets.Contains (Blizzards, (X, Y, Down));
    end Has_Blizzard;

    -- To read the input file line by line
    F         : File_Type;
    File_Name : constant String := "input.txt";
    Line      : Unbounded_String;

    -- To represent the valley into memory
    Valley_H : Integer := 0;
    Valley_W : Integer := 0;
    Starting_Point : Position := (0, 0);
    Goal : Position := (0, 0);

    -- The set of blizzards at a given time
    Blizzards : Blizzard_Sets.Set;
    Moved_Blizzards : Blizzard_Sets.Set;

    -- The set of possible positions at a given time
    Positions : Position_Sets.Set;
    Next_Positions : Position_Sets.Set;

    -- The time
    Minute : Integer := 0;

begin
    -- Read the input
    Open (F, In_File, File_Name);
    while not End_Of_File (F) loop
       Line := To_Unbounded_String (Get_Line (F));
       Valley_W := Length (Line);
       Valley_H := Valley_H + 1;
       for I in 1 .. Valley_W loop
            if element (Line, I) = '>' then
                Blizzards.Include((I, Valley_H, Right));
            end if;
            if element (Line, I) = '<' then
                Blizzards.Include((I, Valley_H, Left));
            end if;
            if element (Line, I) = '^' then
                Blizzards.Include((I, Valley_H, Up));
            end if;
            if element (Line, I) = 'v' then
                Blizzards.Include((I, Valley_H, Down));
            end if;
        end loop;
    end loop;
    Close (F);
    Starting_Point := (2, 1);
    Goal := (Valley_W - 1, Valley_H);

    Positions.Include(Starting_Point);

    while (not Position_Sets.Contains (Positions, Goal)) loop
        Moved_Blizzards := Blizzard_Sets.Empty_Set;
        for E of Blizzards loop
            Moved_Blizzards.Include (Move_Blizzard (E, Valley_H, Valley_W));
        end loop;
        Blizzards := Moved_Blizzards;

        Next_Positions := Position_Sets.Empty_Set;
        for E of Positions loop
            -- Consider staying where you are
            if (not Has_Blizzard (E.X, E.Y, Blizzards)) then
            Next_Positions.Include ( (E.X, E.Y) );
            end if;

            -- Consider going left
            if (E.X > 2 and (E.Y /= 1 and E.Y /= Valley_H)) and
            (not Has_Blizzard (E.X-1, E.Y, Blizzards)) then
            Next_Positions.Include ( (E.X-1, E.Y) );
            end if;

            -- Consider going right
            if (E.X < Valley_W - 1 and (E.Y /= 1 and E.Y /= Valley_H)) and
            (not Has_Blizzard (E.X+1, E.Y, Blizzards)) then
            Next_Positions.Include ( (E.X+1, E.Y) );
            end if;

            -- Consider going up
            if (E.Y > 2 or (E.X = 2 and E.Y = 2)) and
            (not Has_Blizzard (E.X, E.Y-1, Blizzards)) then
            Next_Positions.Include ( (E.X, E.Y-1) );
            end if;

            -- Consider going down
            if (E.Y < Valley_H - 1 or (E.X = Valley_W-1 and E.Y = Valley_H - 1)) and
            (not Has_Blizzard (E.X, E.Y+1, Blizzards)) then
            Next_Positions.Include ( (E.X, E.Y+1) );
            end if;
        end loop;

        Positions := Next_Positions;
        Minute := Minute + 1;

        --for E of Positions loop
        --    Display_Position (E);
        --end loop;
        --Put_Line ("");
        
    end loop;

    
    Put_Line ("Part 1:" & Integer'Image(Minute));

    Positions := Position_Sets.Empty_Set;
    Positions.Include(Goal);

    while (not Position_Sets.Contains (Positions, Starting_Point)) loop
        Moved_Blizzards := Blizzard_Sets.Empty_Set;
        for E of Blizzards loop
            Moved_Blizzards.Include (Move_Blizzard (E, Valley_H, Valley_W));
        end loop;
        Blizzards := Moved_Blizzards;

        Next_Positions := Position_Sets.Empty_Set;
        for E of Positions loop
            -- Consider staying where you are
            if (not Has_Blizzard (E.X, E.Y, Blizzards)) then
            Next_Positions.Include ( (E.X, E.Y) );
            end if;

            -- Consider going left
            if (E.X > 2 and (E.Y /= 1 and E.Y /= Valley_H)) and
            (not Has_Blizzard (E.X-1, E.Y, Blizzards)) then
            Next_Positions.Include ( (E.X-1, E.Y) );
            end if;

            -- Consider going right
            if (E.X < Valley_W - 1 and (E.Y /= 1 and E.Y /= Valley_H)) and
            (not Has_Blizzard (E.X+1, E.Y, Blizzards)) then
            Next_Positions.Include ( (E.X+1, E.Y) );
            end if;

            -- Consider going up
            if (E.Y > 2 or (E.X = 2 and E.Y = 2)) and
            (not Has_Blizzard (E.X, E.Y-1, Blizzards)) then
            Next_Positions.Include ( (E.X, E.Y-1) );
            end if;

            -- Consider going down
            if (E.Y < Valley_H - 1 or (E.X = Valley_W-1 and E.Y = Valley_H - 1)) and
            (not Has_Blizzard (E.X, E.Y+1, Blizzards)) then
            Next_Positions.Include ( (E.X, E.Y+1) );
            end if;
        end loop;

        Positions := Next_Positions;
        Minute := Minute + 1;

        --for E of Positions loop
        --    Display_Position (E);
        --end loop;
        --Put_Line ("");
        
    end loop;

    Positions := Position_Sets.Empty_Set;
    Positions.Include(Starting_Point);

    while (not Position_Sets.Contains (Positions, Goal)) loop
        Moved_Blizzards := Blizzard_Sets.Empty_Set;
        for E of Blizzards loop
            Moved_Blizzards.Include (Move_Blizzard (E, Valley_H, Valley_W));
        end loop;
        Blizzards := Moved_Blizzards;

        Next_Positions := Position_Sets.Empty_Set;
        for E of Positions loop
            -- Consider staying where you are
            if (not Has_Blizzard (E.X, E.Y, Blizzards)) then
            Next_Positions.Include ( (E.X, E.Y) );
            end if;

            -- Consider going left
            if (E.X > 2 and (E.Y /= 1 and E.Y /= Valley_H)) and
            (not Has_Blizzard (E.X-1, E.Y, Blizzards)) then
            Next_Positions.Include ( (E.X-1, E.Y) );
            end if;

            -- Consider going right
            if (E.X < Valley_W - 1 and (E.Y /= 1 and E.Y /= Valley_H)) and
            (not Has_Blizzard (E.X+1, E.Y, Blizzards)) then
            Next_Positions.Include ( (E.X+1, E.Y) );
            end if;

            -- Consider going up
            if (E.Y > 2 or (E.X = 2 and E.Y = 2)) and
            (not Has_Blizzard (E.X, E.Y-1, Blizzards)) then
            Next_Positions.Include ( (E.X, E.Y-1) );
            end if;

            -- Consider going down
            if (E.Y < Valley_H - 1 or (E.X = Valley_W-1 and E.Y = Valley_H - 1)) and
            (not Has_Blizzard (E.X, E.Y+1, Blizzards)) then
            Next_Positions.Include ( (E.X, E.Y+1) );
            end if;
        end loop;

        Positions := Next_Positions;
        Minute := Minute + 1;

        --for E of Positions loop
        --    Display_Position (E);
        --end loop;
        --Put_Line ("");
        
    end loop;

    Put_Line ("Part 2:" & Integer'Image(Minute));

end day24;