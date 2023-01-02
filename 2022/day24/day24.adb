with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

procedure day24 is

    type Position is record
        X: Integer;
        Y: Integer;
    end record;

    type Direction is
        (Left, Right, Up, Down);

    type Blizzard is record
        X: Integer;
        Y: Integer;
        D: Direction;
    end record;

    procedure Display_Position (C: Position) is
    begin
        Ada.Text_IO.Put_Line ("x:" & Integer'Image (C.X) & ", y:" & Integer'Image (C.Y));
    end Display_Position;

    procedure Display_Blizzard (C: Blizzard) is
    begin
        Ada.Text_IO.Put_Line ("x:" & Integer'Image (C.X)
                              & ", y:" & Integer'Image (C.Y)
                              & ", dir:" & Direction'Image (C.D));
    end Display_Blizzard;

    Current_Pos : Position := (4211,72);
    A_Blizzard : Blizzard := (1,2,Right);
    Another_Blizzard : Blizzard := (1,2,Right);

    F : Ada.Text_IO.File_Type;
    File_Name : constant String := "input_test.txt";

begin
   --  A comment

   Display_Position (Current_Pos);
   Display_Blizzard (A_Blizzard);
   Ada.Text_IO.Put_Line (Boolean'Image (A_Blizzard = Another_Blizzard));

   Ada.Text_IO.Open (F, Ada.Text_IO.In_File, File_Name);
   while not Ada.Text_IO.End_Of_File (F) loop
      Ada.Text_IO.Put_Line (Ada.Text_IO.Get_Line (F));
   end loop;
   Ada.Text_IO.Close (F);
end day24;