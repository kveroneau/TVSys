unit charvm;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, winsys, StrUtils, qvfs, login, mcshell, timidity;

procedure RunProgram(const data: string);
procedure ExeProgram(const fname, typ: string);
procedure ProcessVMs;

implementation

type
  TCharVM = class(TComponent)
  private
    FWin: TKevWindow;
    FData: string[60];
    FPtr: byte;
    FRunning: Boolean;
    function GetOp: char;
    function GetStrParam: string;
    procedure Print(const msg: string);
    procedure DoWindow;
    procedure MConfigure;
    procedure ProcList;
  public
    constructor Create(AOwner: TComponent);
    procedure DoCycle;
  end;

var
  Programs: TComponent;

procedure RunProgram(const data: string);
var
  prog: TCharVM;
begin
  prog:=TCharVM.Create(Programs);
  prog.FData:=data;
  prog.FPtr:=1;
  prog.FRunning:=True;
end;

procedure ExeProgram(const fname, typ: string);
var
  fi: PVFS;
begin
  fi:=GetFile(fname, typ);
  if fi = Nil then
    Exit;
  RunProgram(fi^.data);
end;

procedure ProcessVMs;
var
  i: integer;
  prg: TCharVM;
begin
  if Programs.ComponentCount < 1 then
    Exit;
  for i:=Programs.ComponentCount-1 downto 0 do
    if not TCharVM(Programs.Components[i]).FRunning then
    begin
      prg:=TCharVM(Programs.Components[i]);
      FreeAndNil(prg);
    end;
  for i:=0 to Programs.ComponentCount-1 do
    TCharVM(Programs.Components[i]).DoCycle;
end;

function TCharVM.GetOp: char;
begin
  Result:=FData[FPtr];
  Inc(FPtr);
end;

function TCharVM.GetStrParam: string;
begin
  Result:=Copy2Symb(RightStr(FData, Length(FData)-FPtr+1), '`');
  Inc(FPtr, Length(Result)+1);
end;

procedure TCharVM.Print(const msg: string);
begin
  if FWin = Nil then
    WriteLn(msg)
  else
    FWin.WriteLn(msg);
end;

procedure TCharVM.DoWindow;
var
  WTitle: string;
  WFile: string;
begin
  WTitle:=GetStrParam;
  WFile:=GetStrParam;
  LoadWinState(WFile, WTitle, 4,4,30,8,2,0);
end;

procedure TCharVM.MConfigure;
begin
  RunCmd('connect', GetStrParam+' '+GetStrParam);
  RunCmd('login', GetStrParam);
  RunCmd('card', GetStrParam);
end;

procedure TCharVM.ProcList;
var
  i: integer;
  prg: TCharVM;
  buf: string;
begin
  for i:=0 to Programs.ComponentCount-1 do
  begin
    buf:='';
    prg:=TCharVM(Programs.Components[i]);
    if prg.FRunning then
      buf:='R: ';
    if Assigned(prg.FWin) then
      buf:=buf+prg.FWin.Title
    else
      buf:=buf+prg.FData;
    Print(buf);
  end;
  Print(' Total processes: '+IntToStr(Programs.ComponentCount));
end;

procedure TCharVM.DoCycle;
var
  op: char;
begin
  if not FRunning then
    Exit;
  op:=GetOp;
  case op of
    'H': Print('Hello World!');
    'W': FWin:=CreateWindow('CharVM Window', 4,4,30,8,2,0);
    'w': FWin:=LoadWinState(GetStrParam, GetStrParam, 4,4,30,8,2,0);
    'P': Print(GetStrParam);
    'L': StartLogin;
    'R': ExeProgram(GetStrParam, GetStrParam);
    'D': SetDirectory(GetStrParam);
    'M': StartMCShell;
    'm': MConfigure;
    'n': RunCmd('cat', GetStrParam);
    'T': PlaySong(GetStrParam, GetStrParam);
    't': PlayLibrary(GetStrParam);
    '|': ProcList;
    '\': FRunning:=False;
    '.': CurWin.LineMode:=lmLine;
  else
    Print('Invalid Op: `'+op+'` at '+IntToStr(FPtr-1));
    FRunning:=False;
  end;
  if CurWin <> FWin then
    if FWin <> Nil then
      FWin.Draw;
  CurWin.Draw;
end;

constructor TCharVM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRunning:=False;
end;

initialization
  Programs:=TComponent.Create(Nil);

finalization
  Programs.Free;

end.
