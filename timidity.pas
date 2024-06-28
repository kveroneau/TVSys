unit timidity;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, process, winsys, cmdsys;

var
  EnableMusicOS: Boolean;

procedure OpenPlayerWindow;
procedure PlayMIDI(const fname: string);
procedure StopMIDI;
procedure PlaySong(const fname, libname: string);
procedure PlayLibrary(const libname: string);

implementation

var
  Player: TProcess;
  PlayerWin: TKevWindow;

procedure FreePlayer;
begin
  if not Assigned(Player) then
    Exit;
  if Player.Active then
    Player.Terminate(1);
  Player.Free;
  Player:=Nil;
end;

procedure StopMIDI;
begin
  FreePlayer;
end;

function GetDirectory(const dirname: string): TStringList;
var
  info: TSearchRec;
begin
  Result:=TStringList.Create;
  if FindFirst(dirname, faAnyFile, info) = 0 then
  begin
    repeat
      if (info.Attr and faDirectory) <> faDirectory then
        Result.Add(info.Name);
    until FindNext(info) <> 0;
    FindClose(info);
  end;
end;

function GetLibrary(const libname: string): string;
begin
  case libname of
    'col1': Result:='/opt/TVSys/MIDI/';
    'col2': Result:='/home/kveroneau/MIDI/';
    'bach': Result:='/home/kveroneau/MIDI/Bach/';
    'mozart': Result:='/home/kveroneau/MIDI/Mozart/';
    'beethoven': Result:='/home/kveroneau/MIDI/Beethoven/';
  else
    Result:='/opt/TVSys/MIDI/';
  end;
end;

procedure ListMusic(const libname: string);
var
  l: TStringList;
  i: integer;
begin
  l:=GetDirectory(GetLibrary(libname)+'*');
  try
    for i:=0 to l.Count-1 do
      WriteLn(l.Strings[i]);
  finally
    l.Free;
  end;
end;

procedure PlaySong(const fname, libname: string);
begin
  PlayMIDI(GetLibrary(libname)+fname);
end;

procedure PlayLibrary(const libname: string);
begin
  PlayMIDI(GetLibrary(libname));
end;

procedure PlayerInput(line: string);
var
  cmd: string;
begin
  if line = '' then
    Exit;
  if line = 'exit' then
  begin
    if CurWin = PlayerWin then
      CurWin:=Nil;
    FreeAndNil(PlayerWin);
    DrawWindows;
    Exit;
  end;
  try
    cmd:=GetToken(line);
    case cmd of
      'cls': PlayerWin.ClrScr;
      'ls': ListMusic(GetToken(line));
      'play': PlaySong(GetToken(line), GetToken(line));
      'playall': PlayLibrary(GetToken(line));
      'stop': StopMIDI;
      'save': SaveWinState('Timidity', PlayerWin);
    else
      WriteLn('?What');
    end;
  except
    WriteLn('?Bad Parameters!');
  end;
  PlayerWin.LineMode:=lmLine;
  if CurWin = PlayerWin then
    PlayerWin.Draw;
end;

procedure OpenPlayerWindow;
begin
  if Assigned(PlayerWin) then
    Exit;
  PlayerWin:=LoadWinState('Timidity', 'Music Player', 2,2,40,8,0,3);
  with PlayerWin do
  begin
    WriteLn('TVSys Music Player v0.1b');
    ReadLn('& ', @PlayerInput);
  end;
end;

procedure PlayMIDI(const fname: string);
begin
  FreePlayer;
  Player:=TProcess.Create(Nil);
  Player.Options:=[poUsePipes, poStderrToOutPut];
  Player.Executable:='/usr/bin/timidity';
  Player.Parameters.Add(fname);
  Player.Execute;
end;

initialization
  Player:=Nil;
  EnableMusicOS:=False;

finalization
  FreePlayer;

end.
