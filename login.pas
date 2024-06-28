unit login;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, winsys, qvfs, zencode;

procedure StartLogin;
function IsAuthenticated: Boolean;
procedure DoLogin(line: string);

implementation

var
  OpID: string;
  AuthOK: Boolean;

function IsAuthenticated: Boolean;
begin
  Result:=AuthOK;
end;

procedure DoAuth(line: string);
var
  fi: PVFS;
begin
  SetDirectory('TVAuth');
  fi:=GetFile(OpID, '<:)');
  SetDirectory('FSRoot');
  if fi = Nil then
    ReadLn('Operator ID: ', @DoLogin)
  else if ndecode(fi^.data) = line then
  begin
    WriteLn('Authentication OK, please proceed...');
    CurWin.CmdPrompt:='';
    AuthOK:=True;
    {RunProgram(fi^.data);}
  end
  else
  begin
    WriteLn('Invalid!');
    ReadLn('Operator ID: ', @DoLogin);
  end;
  CurWin.Draw;
end;

procedure DoLogin(line: string);
begin
  OpID:=line;
  InputMask:='*';
  ReadLn('Password: ', @DoAuth);
end;

procedure StartLogin;
var
  LoginWin: TKevWindow;
begin
  LoginWin:=CreateWindow('Login System', 50, 20, 40, 10, 4, 0);
  with LoginWin do
  begin
    WriteLn('System Logon Routine Started.');
    ReadLn('Operator ID: ', @DoLogin);
  end;
end;

initialization
  AuthOK:=False;

end.
