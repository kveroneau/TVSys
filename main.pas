program TVSys;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, shell, winsys, crt, homecp,
  qvfs, memio, eventsys, login, charvm, desktop, timidity, zwin, zencode
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    LogWin: TKevWindow;
    procedure ShowCULog(const msg: string);
    {$IFNDEF HOMECU}
    procedure ProcessSync(Sender: TObject);
    {$ENDIF}
    procedure CheckVFS;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure ShowException(E: Exception); override;
  end;

const
  {$IFDEF DEBUG}
  VFS_FILE = 'debugfs.dat';
  {$ELSE}
  VFS_FILE = 'qvfs.dat';
  {$ENDIF}

{ TMyApplication }

procedure TMyApplication.ShowCULog(const msg: string);
begin
  if not Assigned(LogWin) then
    LogWin:=CreateWindow('CU Log Viewer', 10, 20, 60, 10, 2, 0);
  LogWin.WriteLn(msg);
  LogWin.Draw;
  CurWin.Draw;
end;

{$IFNDEF HOMECU}
procedure TMyApplication.ProcessSync(Sender: TObject);
begin
  Weather:=HomeCU.Weather;
  {$IFDEF DEBUG}
  ShowCULog('Sync event!');
  {$ENDIF}
end;
{$ENDIF}

procedure TMyApplication.CheckVFS;
var
  fi: PVFS;
begin
  SetDirectory('TVSys');
  fi:=GetFile('Boot', 'sYs');
  if fi = Nil then
  begin
    AddFile('Boot', 'sYs', 'PSystem Booting...`DFSRoot`RsYs`Kernel`\');
    {AddFile('MusicKernel', 'sYs', 'If this exists, MusicOS is on!');}
    SetDirectory('FSRoot');
    AddFile('Kernel', 'sYs', 'L\');
    SetDirectory('TVAuth');
    AddFile('user', '<:)', nencode('********'));
    SetDirectory('TVSys');
  end;
  fi:=GetFile('MusicKernel', 'sYs');
  if fi <> Nil then
    EnableMusicOS:=True;
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hdv:', 'help desktop vfs:');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  ClrScr;
  TextBackground(Red);
  System.WriteLn('CHARACTER MATRIX: ',ScreenHeight,'x',ScreenWidth);
  TextBackground(Blue);
  WriteLn('SYSTEM READY.');
  {$IFDEF DEBUG}
  TextColor(Black);
  TextBackground(Red);
  WriteLn('DEBUG MODE.');
  {$ENDIF}
  TextColor(Green);
  TextBackground(Black);
  {$IFNDEF HOMECU}
  WriteLn('Powered by Generation 3 Technology!');
  CUOnLog:=@ShowCULog;
  CUOnSync:=@ProcessSync;
  {$ELSE}
  WriteLn('Running in Legacy Home Automation Mode.');
  {$ENDIF}
  if not HasOption('d', 'desktop') then
    HandleEvents;
  {if HandleEvents = 79 then
  begin
    ReturnCode:=1;
    Terminate;
    Exit;
  end;}
  if HasOption('v', 'vfs') then
    InitVFS(GetOptionValue('v','vfs'))
  else
    InitVFS(VFS_FILE);
  CheckVFS;
  if EnableMusicOS then
    PlayMIDI('/opt/TVSys/MIDI/Baldmoun.mid');
  CurWin:=CreateWindow('Desktop', 1,1, ScreenWidth-1, ScreenHeight-1, 2, 0);
  CurWin.WriteLn('Welcome!');
  CurWin.OnDraw:=@RenderDesktop;
  {ExeProgram('Kernel', 'sYs');}
  ExeProgram('Boot', 'sYs');
  {StartLogin;}
  {ShellWin:=CreateWindow('KevShell', 10, 10, 60, 10,7,0);}
  try
    DrawWindows;
    {StartShell;}
    EventSystem;
  finally
    DoneVFS;
  end;

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  {$IFDEF DEBUG}
  StopOnException:=True;
  {$ELSE}
  StopOnException:=False;
  {$ENDIF}
end;

destructor TMyApplication.Destroy;
begin
  Window(1,1,ScreenWidth,ScreenHeight);
  ClrScr;
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  System.writeln('Usage: ', ExeName, ' -h');
end;

procedure TMyApplication.ShowException(E: Exception);
begin
  with CreateWindow('Unhandled Exception', 3,3,60,4,0,4) do
    try
      WriteLn(E.ToString);
      WriteLn('Press Enter to restart program...');
      Draw;
      System.ReadLn;
    finally
      Free;
    end;
  window(1,1,ScreenWidth,ScreenHeight);
  TextColor(2);
  TextBackground(0);
  ClrScr;
  Halt(0);
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
  if ReturnCode > 0 then
    Halt(ReturnCode);
end.
