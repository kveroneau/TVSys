unit shell;

{$mode objfpc}{$h+}

interface

uses Classes, SysUtils, cmdsys, crt, winsys, homecp, qvfs,
     simpleipc, memio, mcshell, forecast, charvm, timidity,
     cuclient, DateUtils, zencode, dos, process;

procedure StartShell;
procedure StopShell;
procedure RunLine(line: string);
procedure RunFile(const fname: string);
procedure SaveShellState(const fname: string);

var
  ShellWin: TKevWindow;
  ReturnCode: integer;
  Weather: PWeatherData;

implementation

const
  VERSION = '0.6b';

var
  Running: boolean;
  s1, s2: string;

procedure StopShell;
begin
  if not Assigned(ShellWin) then
    Exit;
  SaveShellState('KevShell');
  if CurWin = ShellWin then
    CurWin:=Nil;
  FreeAndNil(ShellWin);
  DrawWindows;
end;

procedure ResumeShell;
begin
  with ShellWin do
  begin
    CmdPrompt:='$ ';
    LineMode:=lmLine;
    OnLine:=@RunLine;
    Draw;
  end;
end;

procedure WriteLn(const s: string);
begin
  if Assigned(ShellWin) then
    ShellWin.WriteLn(s)
  else
    System.WriteLn(s);
end;

procedure Write(const s: string);
begin
  if Assigned(ShellWin) then
    ShellWin.Write(s)
  else
    System.Write(s);
end;

procedure ClrScr;
begin
  if Assigned(ShellWin) then
    ShellWin.ClrScr
  else
    crt.ClrScr;
end;

procedure IPC(const msg: string; typ: longint);
begin
  with TSimpleIPCClient.Create(Nil) do
    try
      ServerID:='ipcsrv';
      Active:=True;
      SendStringMessage(msg);
    finally
      Free;
    end;
end;

procedure RunFile(const fname: string);
var
  i: integer;
begin
  with TStringList.Create do
    try
      LoadFromFile(fname);
      for i:=0 to Count-1 do
        RunLine(Strings[i]);
    finally
      Free;
    end;
end;

procedure ShowWeather;
var
  f: File of TWeatherData;
begin
  if not Assigned(Weather) then
  begin
    Assign(f, '/opt/SSD/Apps/weather.dat');
    Reset(f);
    New(Weather);
    Read(f, Weather^);
    Close(f);
  end;
  with Weather^ do
  begin
    WriteLn('Temperature: '+Temperature);
    WriteLn('Conditions: '+conditions);
    WriteLn('====='+forecast[0].title+': '+forecast[0].temperature+'======');
    WriteLn(forecast[0].outlook);
  end;
end;

procedure KeyTester;
var
  c: char;
begin
  repeat
    System.Write('Please press a key...');
    repeat
      Sleep(100);
    until KeyPressed;
    System.WriteLn('Great!');
    c:=ReadKey;
    if c = #0 then
    begin
      c:=ReadKey;
      System.WriteLn('You Pressed: 0+',ord(c));
    end
    else
      System.WriteLn('You Pressed: ',ord(c));
  until c = #27;
end;

procedure WinTester;
begin
  with CreateWindow('WinTester', 12, 12, 40, 10,7,0) do
  begin
    WriteLn('Hello World!');
    WriteLn('This should be on line 2!');
    Draw;
    KeyTester;
    Draw;
    System.ReadLn;
  end;
end;

procedure HomeApp;
begin
  WriteLn('Please press Home Automation keys now...');
  ShellWin.Draw;
  HandleEvents;
  CurWin:=ShellWin;
  ShellWin.Draw;
end;

procedure ls;
var
  i: integer;
  lst: TStringList;
begin
  lst:=FileList;
  for i:=0 to lst.Count-1 do
    WriteLn(lst.Strings[i]);
  lst.Free;
end;

procedure GetF(const fname: string);
var
  fi: PVFS;
begin
  fi:=GetFile(fname, '');
  if Assigned(fi) then
    WriteLn(fi^.data);
end;

procedure RmFile(const fname, typ: string);
begin
  DeleteFile(fname, typ);
end;

function VFSExec(const fname: string): boolean;
var
  fi: PVFS;
  line: string;
begin
  fi:=GetFile(fname, 'eXe');
  if fi <> Nil then
  begin
    {$IFDEF DEBUG}
    WriteLn('Will VFS Execute: '+fi^.data);
    {$ENDIF}
    RunProgram(fi^.data);
    ShellWin.Draw;
    Result:=True;
    Exit;
  end;
  fi:=GetFile(fname, '');
  if fi <> Nil then
  begin
    Result:=True;
    line:=fi^.data;
    case fi^.typ of
      '=&=': PlaySong(GetToken(line),GetToken(line));
      'Pi]': RunProgram('Mmpi.home.lan`3845`HomeCU`0`n'+GetToken(line)+'`\');
      'Ch]': RunProgram('Mmcherry.home.lan`3845`Kevin`1`n'+GetToken(line)+'`\');
    else
      Result:=False;
    end;
  end
  else
    Result:=False;
end;

procedure ShowVersion;
begin
  with ShellWin do
  begin
    WriteLn('KevShell v'+VERSION);
    WriteLn('Build Date: '+{$I %DATE%});
    WriteLn('Build Time: '+{$I %TIME%});
    WriteLn('Compiler Version: '+{$I %FPCVERSION%});
    WriteLn('CPU Target: '+{$I %FPCTARGET%});
  end;
end;

procedure DoCreateFile(line: string);
var
  fi: PVFS;
begin
  fi:=GetFile(s1, s2);
  if fi <> Nil then
    fi^.data:=line
  else
    AddFile(s1, s2, line);
  ResumeShell;
end;

procedure CreateFile(const fname, typ: string);
begin
  ReadLn('', @DoCreateFile);
  s1:=fname;
  s2:=typ;
end;

procedure mkdir(const fname: string);
begin
  MakeDirectory(fname);
end;

procedure chdir(const fname: string);
var
  fi: PVFS;
begin
  if fname = '/' then
  begin
    SetDirectory('FSRoot');
    Exit;
  end;
  fi:=GetFile(fname, '-=-');
  if fi = Nil then
    WriteLn(' * No such directory!')
  else
    SetDirectory(fname);
end;

procedure MoveFile(const dest, fname: string);
var
  fi, fx: PVFS;
  s: string[20];
begin
  fx:=GetFile(fname, '');
  if fx = Nil then
    Exit;
  fi:=GetFile(dest, '');
  if fi = Nil then
    fx^.na:=dest
  else if fi^.typ = '-=-' then
    fx^.wh:=dest
  else
  begin
    s:=fi^.na;
    DeleteFile(fi^.na, fi^.typ);
    fx^.na:=s;
  end;
end;

procedure MemTester;
var
  i, count: integer;
begin
  count:=0;
  for i:=0 to 31 do
    if not Assigned(Memory[i]) then
      Inc(count);
  WriteLn('Blocks Free: '+IntToStr(count));
end;

{procedure TestEvtSys;
begin
  CurWin:=CreateWindow('Event System Tester', 16, 16, 40, 8, 2, 0);
  try
    CurWin.WriteLn('EventSystem Ready.');
    CurWin.Draw;
    EventSystem;
  finally
    FreeAndNil(CurWin);
  end;
  DrawWindows;
end;}

procedure ShowWiFi;
var
  WifiWin: TKevWindow;
  {$IFDEF HOMECU}
  cu: THomeCU;
  {$ENDIF}
begin
  WifiWin:=CreateWindow('WiFi Configuration', 16, 16, 60, 10, 0, 7);
  with WifiWin do
  begin
    WriteLn('WiFi Configuration:');
    WriteLn('Access Point Name = dlink-FCA4');
    WriteLn('Key = kvlex96518');
    {WriteLn(' * Please note that Kevin still has to do something to');
    WriteLn('   allow the device to connect.');}
    WriteLn('Enabling access, please wait...');
    Draw;
    {$IFDEF HOMECU}
    cu:=THomeCU.Create(Nil);
    try
      cu.Connect;
      cu.IpSet:=True;
    finally
      cu.Free;
    end;
    {$ELSE}
    HomeCU.IpSet:=True;
    {$ENDIF}
    WriteLn('Please connect your WiFi device now.');
  end;
  {CurWin:=ShellWin;}
  DrawWindows;
end;

procedure ShowHelp;
begin

end;

procedure TestReadLn(line: string);
begin
  WriteLn('You wrote: '+line);
  ShellWin.CmdPrompt:='$ ';
  ShellWin.OnLine:=@RunLine;
  ShellWin.LineMode:=lmLine;
  ShellWin.Draw;
end;

procedure SaveShellState(const fname: string);
var
  fi: PVFS;
begin
  fi:=GetFile(fname, '[=]');
  if fi = Nil then
    AddFile(fname, '[=]', '');
  fi:=GetFile(fname, '[=]');
  fi^.data[0]:=chr(SizeOf(TWinInfo));
  Move(ShellWin.WinInfo, fi^.data[1], SizeOf(TWinInfo));
end;

procedure LoadShellState(const fname: string);
var
  fi: PVFS;
  wi: PWinInfo;
begin
  fi:=GetFile(fname, '[=]');
  if fi = Nil then
    Exit;
  wi:=@fi^.data[1];
  StopShell;
  ShellWin:=RestoreWindow('KevShell', wi);
  StartShell;
end;

procedure SwapVFS(const fname: string);
begin
  DoneVFS;
  InitVFS(fname);
  WriteLn('VFS Swapped.');
end;

procedure DoCreateUser(line: string);
var
  fi: PVFS;
begin
  fi:=GetFile(s1, '<:)');
  if fi = Nil then
    AddFile(s1, '<:)', nencode(line))
  else
    fi^.data:=nencode(line);
  ResumeShell;
end;

procedure CreateUser(const user: string);
begin
  InputMask:='*';
  ReadLn('Password: ', @DoCreateUser);
  s1:=user;
end;

procedure SysShell;
begin
  Window(1,1,ScreenWidth,ScreenHeight);
  TextColor(2);
  TextBackground(0);
  ClrScr;
  System.WriteLn('Welcome to the Shell!');
  Exec('/home/kveroneau/Supervisor','');
  DrawWindows;
  ResumeShell;
end;

procedure RunCommand(cmd, params: string);
var
  p: TProcess;
  s: string;
begin
  p:=TProcess.Create(Nil);
  try
    p.Executable:=cmd;
    p.Options := [poUsePipes];
    p.Active:=True;
    repeat
      Sleep(100);
    until not p.Running;
    SetLength(s, p.Output.NumBytesAvailable);
    p.Output.Read(s[1], p.Output.NumBytesAvailable);
    WriteLn(s);
  finally
    p.Active:=False;
    p.Free;
  end;
end;

procedure RunLine(line: string);
var
  cmd: string;
begin
  if line = '' then
    Exit;
  try
    cmd:=GetToken(line);
    case cmd of
      'exit': StopShell;
      'ver': ShowVersion;
      'cls': ShellWin.ClrScr;
      'weather': ShowWeather;
      'keytest': KeyTester;
      'wintest': WinTester;
      'cu': HomeApp;
      'wins': WriteLn('Window Count: '+IntToStr(WinList.ComponentCount));
      'draw': DrawWindows;
      'count': WriteLn('File Count: '+IntToStr(FileCount));
      'create': CreateFile(GetToken(line), GetToken(line));
      'ls': ls;
      'get': GetF(GetToken(line));
      'rm': RmFile(GetToken(line), GetToken(line));
      'mkdir': mkdir(GetToken(line));
      'cd': chdir(GetToken(line));
      'mv': MoveFile(GetToken(line), GetToken(line));
      'asdf': ReturnCode:=1;
      'whoami': ShellWin.WriteLn(GetEnvironmentVariable('USER'));
      'ipc': IPC(GetToken(line), 51);
      'mem': MemTester;
      'wifi': ShowWiFi;
      'mcshell': StartMCShell;
      'readln': ShellWin.ReadLn('Give me a line: ', @TestReadLn);
      'save': SaveShellState(GetToken(line));
      'load': LoadShellState(GetToken(line));
      'play': PlayMIDI('/opt/TVSys/MIDI/DestructionSet.MID');
      'stop': StopMIDI;
      'player': OpenPlayerWindow;
      'time': WriteLn(IntToStr(HourOf(Now))+':'+IntToStr(MinuteOf(Now)));
      'vfs': SwapVFS(GetToken(line));
      'user': CreateUser(GetToken(line));
      'shell': SysShell;
      'acpi': RunCommand('/usr/bin/acpi', '');
      {'evt': TestEvtSys;}
    else
      if VFSExec(cmd) then
        Exit
      else if FileExists(cmd) then
        RunFile(cmd)
      else
        WriteLn('?Syntax Error');
    end;
  except
    On ERangeError do WriteLn('?Bad Parameters!');
  end;
  if Assigned(ShellWin) then
  begin
    if CurWin = ShellWin then
      ShellWin.Draw;
    ShellWin.LineMode:=lmLine;
  end;
end;

procedure StartShell;
var
  line: string;
begin
  ReturnCode:=0;
  TextBackground(Blue);
  with ShellWin do
  begin
    Colour(7,1);
    ClrScr;
    WriteLn('KevShell v'+VERSION+'  Build Date: '+{$I %DATE%});
    {$IFDEF DEBUG}WriteLn('DEBUG Version.');{$ENDIF}
    CmdPrompt:='$ ';
    LineMode:=lmLine;
    OnLine:=@RunLine;
    Running:=True;
    Draw;
  end;
  if EnableMusicOS then
    PlayMIDI('/opt/TVSys/MIDI/DestructionSet.MID');
end;

end.
