unit homecp;

{$mode objfpc}{$h+}

interface

uses Classes, SysUtils, crt, winsys,
{$IFDEF HOMECU}
     homecu
{$ELSE}
     cuclient
{$ENDIF};

var
  {$IFNDEF HOMECU}
  HomeCU: THomeCU;
  {$ENDIF}
  CUOnLog: TLogEvent;
  CUOnSync: TNotifyEvent;

{$IFNDEF HOMECU}
procedure InitHomeCU;
{$ENDIF}
function HandleEvents: byte;

implementation

{$IFNDEF HOMECU}
procedure InitHomeCU;
begin
  if Assigned(HomeCU) then
    Exit;
  HomeCU:=THomeCU.Create(Nil);
  if Assigned(CUOnLog) then
    HomeCU.OnLog:=CUOnLog;
  if Assigned(CUOnSync) then
    HomeCU.OnSync:=CUOnSync;
  HomeCU.LoadAll:=True;
  HomeCU.Connect;
end;
{$ENDIF}

procedure HandleSpecial(cu: THomeCU; sp: char);
begin
  case sp of
    #59: cu.Theme('Romantic');
    #60: cu.Theme('Green');
    #61: cu.Blacklight;
    #62: cu.Hue(48291);
    #63: cu.Hue(58182);
  end;
end;

function HandleEvents: byte;
var
  c: char;
  cu: THomeCU;
  win: TKevWindow;
begin
  win:=Nil;
  {$IFDEF HOMECU}
  cu:=THomeCU.Create;
  {$ELSE}
  InitHomeCU;
  cu:=HomeCU;
  {$ENDIF}
  repeat
    repeat
      Sleep(100);
      {$IFNDEF HOMECU}cu.CheckEvents;{$ENDIF}
    until KeyPressed;
    c:=ReadKey;
    if c <> #0 then
      if not Assigned(win) then
      begin
        win:=CreateWindow('Home Automation Shell', 12, 12, 40, 10, 0, 4);
        win.Draw;
        TextColor(2);
        TextBackground(0);
        ClrScr;
      end;
    { Was looking into a better way... }
    case c of
      {$IFDEF HOMECU}
      #0: HandleSpecial(cu, ReadKey);
      '1': cu.Brightness(11);
      '2': cu.Brightness(20);
      '3': cu.Brightness(30);
      '4': cu.Brightness(40);
      '5': cu.Brightness(50);
      '6': cu.Brightness(60);
      '7': cu.Brightness(70);
      '8': cu.Brightness(80);
      '9': cu.Brightness(90);
      {$ELSE}
      '1': cu.Brightness(10);
      '2': cu.Brightness(16);
      '3': cu.Brightness(32);
      '4': cu.Brightness(64);
      '5': cu.Brightness(128);
      '6': cu.Brightness(160);
      '7': cu.Brightness(192);
      '8': cu.Brightness(234);
      '9': cu.Brightness(254);
      {$ENDIF}
      '0': cu.AllOff;
      'l': cu.LivingRoom;
      'r': cu.Relax;
      'R': cu.hue(8088);
      ' ': cu.Strips;
      'b': cu.Bedroom;
      's': cu.Study;
      'S': cu.Bathroom;
      'B': cu.Blacklight;
      'e': cu.Entrance;
      'A': cu.AllBright;
      {$IFDEF HOMECU}
      'p': cu.Pause:=True;
      'P': cu.Pause:=False;
      {$ENDIF}
      'm': cu.Rooms:=True;
      'M': cu.Rooms:=False;
      {'g': cu.Theme('Green');}
    end;
  until c = #27;
  Result:=Ord(ReadKey);
  {$IFDEF HOMECU}
  cu.Free;
  {$ELSE}
  cu:=Nil;
  {$ENDIF}
  if Assigned(win) then
    win.Free;
  CurWin:=Nil;
  DrawWindows;
end;

{$IFNDEF HOMECU}
initialization
  HomeCU:=Nil;

finalization
  if Assigned(HomeCU) then
    HomeCU.Free;
{$ENDIF}

end.
