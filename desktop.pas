unit desktop;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, winsys, qvfs, crt;

procedure RenderDesktop(Win: TComponent);

implementation

var
  row: byte;

procedure PlaceIcon(vfs: PVFS; x,y: byte);
begin
  GotoXY(10, row);
  System.Write('<'+vfs^.typ+'>');
  GotoXY(5, row+1);
  System.Write(vfs^.na);
  Inc(row, 5);
end;

procedure RenderDesktop(Win: TComponent);
var
  i: integer;
begin
  {TKevWindow(Win).Colour(0,4);
  TextBackground(3);
  TextColor(0);}
  ClrScr;
end;

end.