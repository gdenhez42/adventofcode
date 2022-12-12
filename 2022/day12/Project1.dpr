program Project1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils;
type
  Coord = record
    X: Integer;
    Y: Integer;
  end;
  Node = record
    X: Integer;
    Y: Integer;
    Distance: Integer;
    constructor Create(x: Integer; y: Integer; distance: Integer);
  end;
  TGrid = array of array of Integer;
var
  Txt: TextFile;
  s: string;
  grid : TGrid;
  nbRows : integer;
  i : integer;
  startPos, endPos : Coord;
  potentialStarts: array of Coord;
  potentialStart: Coord;
  l: Integer;
  bestStart: Integer;

  constructor Node.Create(x: Integer; y: Integer; distance: Integer);
  begin
    self.X := x;
    self.Y := y;
    self.Distance := distance;
  end;

  function FindNode(nodes: array of Node; node: Node): Integer;
  var
    i, r : Integer;
  begin
    r := -1;
    for i := 1 to length(nodes) do
      if (nodes[i-1].X = node.X) and (nodes[i-1].Y = node.Y) then
        r := i-1;
    Result := r;
  end;

  function PathLength(grid: TGrid; startPos: Coord; endPos: Coord): Integer;
  var
    unsettledNodes : array of Node;
    settledNodes : array of Node;
    smallestDistance : Integer;
    closestNode : Node;
    neighborNode : Node;
    settledIndex, unsettledIndex : Integer;
    i: integer;
  begin
    Result := -1;

    (* Initialize all nodes *)
    SetLength(unsettledNodes, 1);
    SetLength(settledNodes, 0);
    unsettledNodes[0] := Node.Create(startPos.X, startPos.Y, 0);

    while length(unsettledNodes) > 0 do
    begin
      (** find the smallest distance in unsettled node **)
      smallestDistance := length(grid)*length(grid[0]);
      unsettledIndex := -1;
      for i := 1 to length(unsettledNodes) do
        if unsettledNodes[i-1].Distance < smallestDistance then
        begin
          closestNode := unsettledNodes[i-1];
          smallestDistance := unsettledNodes[i-1].Distance;
          unsettledIndex := i-1;
        end;
      settledNodes := settledNodes + [closestNode];
      Delete(unsettledNodes, unsettledIndex, 1);

      if (closestNode.X = endPos.X) and (closestNode.Y = endPos.Y) then
        Result := closestNode.Distance;

      (** Update unsettled **)
      if (closestNode.X > 0) then
        begin
          if (grid[closestNode.Y][closestNode.X - 1] <= (grid[closestNode.Y][closestNode.X] + 1)) then
          begin
            neighborNode := Node.Create(closestNode.X - 1, closestNode.Y, closestNode.Distance + 1);
            settledIndex := FindNode(settledNodes, neighborNode);
            unsettledIndex := FindNode(unsettledNodes, neighborNode);

            if (unsettledIndex >= 0) then
              begin
                if unsettledNodes[unsettledIndex].Distance > neighborNode.Distance then
                  unsettledNodes[unsettledIndex].Distance := neighborNode.Distance;
              end
            else
              if settledIndex = -1 then
                unsettledNodes := unsettledNodes + [neighborNode];
          end;
        end;

      if (closestNode.X < length(grid[0]) - 1) then
        begin
          if (grid[closestNode.Y][closestNode.X + 1] <= (grid[closestNode.Y][closestNode.X] + 1)) then
          begin
            neighborNode := Node.Create(closestNode.X + 1, closestNode.Y, closestNode.Distance + 1);
            settledIndex := FindNode(settledNodes, neighborNode);
            unsettledIndex := FindNode(unsettledNodes, neighborNode);

            if (unsettledIndex >= 0) then
              begin
                if unsettledNodes[unsettledIndex].Distance > neighborNode.Distance then
                  unsettledNodes[unsettledIndex].Distance := neighborNode.Distance;
              end
            else
              if settledIndex = -1 then
                unsettledNodes := unsettledNodes + [neighborNode];
          end;
        end;

      if (closestNode.Y > 0) then
        begin
          if (grid[closestNode.Y - 1][closestNode.X] <= (grid[closestNode.Y][closestNode.X] + 1)) then
          begin
            neighborNode := Node.Create(closestNode.X, closestNode.Y - 1, closestNode.Distance + 1);
            settledIndex := FindNode(settledNodes, neighborNode);
            unsettledIndex := FindNode(unsettledNodes, neighborNode);

            if (unsettledIndex >= 0) then
              begin
                if unsettledNodes[unsettledIndex].Distance > neighborNode.Distance then
                  unsettledNodes[unsettledIndex].Distance := neighborNode.Distance;
              end
            else
              if settledIndex = -1 then
                unsettledNodes := unsettledNodes + [neighborNode];
          end;
        end;

      if (closestNode.Y < length(grid) - 1) then
        begin
          if (grid[closestNode.Y + 1][closestNode.X] <= (grid[closestNode.Y][closestNode.X] + 1)) then
          begin
            neighborNode := Node.Create(closestNode.X, closestNode.Y + 1, closestNode.Distance + 1);
            settledIndex := FindNode(settledNodes, neighborNode);
            unsettledIndex := FindNode(unsettledNodes, neighborNode);

            if (unsettledIndex >= 0) then
              begin
                if unsettledNodes[unsettledIndex].Distance > neighborNode.Distance then
                  unsettledNodes[unsettledIndex].Distance := neighborNode.Distance;
              end
            else
              if settledIndex = -1 then
                unsettledNodes := unsettledNodes + [neighborNode];
          end;
        end;

    end;
  end;


begin

  SetLength(grid, 0);

  AssignFile(Txt, '../../input.txt');
  Reset(Txt);

  SetLength(potentialStarts, 0);

  nbRows := 0;
  while not Eof(Txt) do
  begin
    Readln(Txt, s);
    SetLength(grid, nbRows+1);
    SetLength(grid[nbRows], length(s));
    for i := 1 to length(s) do
      if (s[i] = 'S') or (s[i] = 'E') then
        if s[i] = 'S' then
          begin
            grid[nbRows][i-1] := ord('a');
            startPos.X := i-1;
            startPos.Y := nbRows;
            potentialStarts := potentialStarts + [startPos];
          end
        else
          begin
            grid[nbRows][i-1] := ord('z');
            endPos.X := i-1;
            endPos.Y := nbRows;
          end
      else
        begin
          grid[nbRows][i-1] := ord(s[i]);
          if s[i] = 'a' then
          begin
            potentialStart.X := i-1;
            potentialStart.Y := nbRows;
            potentialStarts := potentialStarts + [potentialStart];
          end;

        end;

    nbRows := nbRows + 1;
  end;
  CloseFile(Txt);

  Writeln(PathLength(grid, startPos, endPos));

  bestStart := Length(grid)*Length(grid[0]);
  for i := 1 to length(potentialStarts) do
  begin
    l := PathLength(grid, potentialStarts[i-1], endPos);
    if (l <> -1) and (bestStart > l) then
      bestStart := l;
  end;
  Writeln(bestStart);

  ReadLn;
end.
