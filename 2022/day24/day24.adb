with Ada.Text_IO;

procedure day24 is

    type Coord is record
        X: Integer;
        Y: Integer;
    end record;

    procedure Display_Coord (C: Coord) is
    begin
        Ada.Text_IO.Put_Line ("x:" & Integer'Image (C.X) & ", y:" & Integer'Image (C.Y));
    end Display_Coord;

    Current_Pos : Coord := (42,72);

begin
   --  A comment

   Display_Coord (Current_Pos);
end day24;