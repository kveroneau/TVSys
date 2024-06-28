unit eventsys;

{$mode objfpc}{$h+}

interface

uses Classes, SysUtils, crt, winsys, shell, homecp, qvfs, login, charvm, timidity;

procedure EventSystem;
procedure NextTask;

implementation

type
  TEventKeys = record
    NextTask: byte;
    Redraw: byte;
    LineMode: byte;
    MoveMode: byte;
    SizeMode: byte;
    CloseKey: byte;
    MaxKey: byte;
  end;

var
  Running: boolean;
  CurTask: integer;
  EvtKeys: TEventKeys;

procedure LoadKeyData;
var
  fi: PVFS;
begin
  fi:=GetFile('KeyData', 'sYs');
  if fi = Nil then
    Exit;
  Move(fi^.data[1], EvtKeys, SizeOf(EvtKeys));
  {Dispose(fi);}
end;

procedure SaveKeyData;
var
  data: string[60];
begin
  data[0]:=chr(SizeOf(EvtKeys));
  Move(EvtKeys, data[1], SizeOf(EvtKeys));
  AddFile('KeyData', 'sYs', data);
end;

function CaptureKey: byte;
var
  c: char;
begin
  repeat
    c:=ReadKey;
    if c <> #0 then
      CurWin.WriteLn('Please press a special key.');
  until c = #0;
  Result:=ord(ReadKey);
  {$IFDEF DEBUG}
  CurWin.WriteLn('Key Pressed: '+IntToStr(Result));
  {$ENDIF}
end;

procedure SetKeyData;
var
  kdWin: TKevWindow;
begin
  kdWin:=CreateWindow('Event System Setup', 4,4,40,8,7,0);
  try
    with kdWin do
    begin
      WriteLn('Please hit a key for each operation...');
      WriteLn('NextTask.');
      Draw;
      EvtKeys.NextTask:=CaptureKey;
      WriteLn('Redraw.');
      Draw;
      EvtKeys.Redraw:=CaptureKey;
      WriteLn('LineMode Toggle.');
      Draw;
      EvtKeys.LineMode:=CaptureKey;
      WriteLn('MoveMode.');
      Draw;
      EvtKeys.MoveMode:=CaptureKey;
      WriteLn('SizeMode.');
      Draw;
      EvtKeys.SizeMode:=CaptureKey;
      WriteLn('MaxMode.');
      Draw;
      EvtKeys.MaxKey:=CaptureKey;
      SaveKeyData;
      WriteLn('KeyData saved.');
      Draw;
      ReadKey;
    end;
  finally
    FreeAndNil(kdWin);
    CurWin:=Nil;
    DrawWindows;
  end;
end;

procedure ToggleLineMode;
begin
  if CurWin.LineMode = lmLine then
    CurWin.LineMode:=lmRaw
  else
    CurWin.LineMode:=lmLine;
end;

procedure TestCmdMode;
begin
  if CurWin.LineMode <> lmRaw then
    Exit;
  CurWin.CmdPrompt:='$ ';
  CurWin.LineMode:=lmCmd;
  CurWin.Draw;
end;

procedure HandleSpecial(const sp: byte);
begin
  { Used to be a case/select, but it doesn't work with variables.}
  if sp = EvtKeys.NextTask then
    NextTask
  else if sp = EvtKeys.Redraw then
    DrawWindows
  else if sp = EvtKeys.LineMode then
    ToggleLineMode
  else if sp = EvtKeys.MaxKey then
    CurWin.WinControl(ord('f'))
  else if Assigned(CurWin) then
  begin
    if sp = EvtKeys.MoveMode then
      CurWin.WinMode:=wmMoving
    else if sp = EvtKeys.SizeMode then
      CurWin.WinMode:=wmSizing;
    CurWin.WinControl(sp);
  end;
end;

procedure HandleInput(const c: byte);
var
  s1, s2: string;
  lmx: TLineMode;
  Proc1: TLineEvent;
begin
  if (c > 32) and (c < 128) then
  begin
    if InputMask = #0 then
      CurWin.Write(chr(c))
    else
      CurWin.Write(InputMask);
  end
  else if c = 8 then
    CurWin.BackSpace
  else if c = 32 then
    CurWin.Write(' ')
  else if c = 4 then
    CurWin.LineMode:=lmRaw
  else if c = 13 then
  begin
    with CurWin do
    begin
      if (LineMode = lmLine) and Assigned(OnLine) then
        s1:=Line
      else if (LineMode = lmCmd) or (LineMode = lmParams) then
        if Assigned(OnCmd) then
        begin
          s1:=Cmd;
          s2:=Params;
        end
      else if (WinMode = wmMoving) or (WinMode = wmSizing) then
        WinMode:=wmNormal;
      lmx:=LineMode;
      WriteLn('CurLine');
      InputMask:=#0;
      Draw;
    end;
    if (lmx = lmLine) and Assigned(CurWin.OnLine) then
    begin
      Proc1:=CurWin.OnLine;
      Proc1(s1);
    end
    else if (lmx = lmCmd) or (lmx = lmParams) then
      if Assigned(CurWin.OnCmd) then
        CurWin.OnCmd(s1, s2);
  end;
end;

procedure NextTask;
begin
  if CurTask = WinList.ComponentCount-1 then
    CurTask:=-1;
  Inc(CurTask);
  CurWin:=TKevWindow(WinList.Components[CurTask]);
  DrawWindows;
end;

procedure EventSystem;
var
  c: char;
  fi: PVFS;
  wi: PWinInfo;
begin
  LoadKeyData;
  if EvtKeys.NextTask = 0 then
    SetKeyData;
  Running:=True;
  CurTask:=WinList.ComponentCount-1;
  CurWin:=TKevWindow(WinList.Components[CurTask]);
  {$IFNDEF HOMECU}
  InitHomeCU;
  {$ENDIF}
  repeat
    repeat
      if WinList.ComponentCount = 0 then
      begin
        Running:=False;
        Exit;
      end;
      { Background stuff here, such as ByteCode VM }
      ProcessVMs;
      {$IFNDEF HOMECU}
      HomeCU.CheckEvents;
      {$ENDIF}
      Sleep(50);
    until KeyPressed;
    if not Assigned(CurWin) then
    begin
      CurTask:=WinList.ComponentCount-1;
      CurWin:=TKevWindow(WinList.Components[CurTask]);
    end;
    c:=ReadKey;
    if c = #0 then
      HandleSpecial(ord(ReadKey))
    else if (CurWin.LineMode = lmLine) or (CurWin.LineMode = lmCmd) or (CurWin.LineMode = lmParams) then
      HandleInput(ord(c))
    else if c = #13 then
      CurWin.WinMode:=wmNormal
    else
    begin
      if c = #27 then
        Running:=False
      else if c = #9 then
        NextTask
      else if c = 'N' then
      begin
        CurWin:=CreateWindow('New Window', 5, 14, 40, 8, 4, 0);
        CurWin.WriteLn('Your new window is ready!');
        CurWin.Draw;
        CurTask:=WinList.ComponentCount-1;
      end
      else if c = 'S' then
      begin
        {$IFNDEF DEBUG}
        if not IsAuthenticated then
          StartLogin
        else{$ENDIF}
        if not Assigned(ShellWin) then
        begin
          fi:=GetFile('KevShell', '[=]');
          if fi = Nil then
            ShellWin:=CreateWindow('KevShell', 10, 10, 60, 10,7,0)
          else
          begin
            wi:=@fi^.data[1];
            ShellWin:=RestoreWindow('KevShell', wi);
          end;
          DrawWindows;
          StartShell;
        end;
      end
      else if c = 'M' then
        OpenPlayerWindow
      else if c = 'A' then
        HandleEvents
      else if c = 'M' then
        CurWin.WinMode:=wmMoving
      else if c = 'R' then
        CurWin.WinMode:=wmSizing
      else if c = 'X' then
      begin
        if CurWin = ShellWin then
          ShellWin:=Nil;
        FreeAndNil(CurWin);
        if WinList.ComponentCount > 0 then
        begin
          CurTask:=WinList.ComponentCount-1;
          DrawWindows;
          {CurWin:=TKevWindow(WinList.Components[CurTask]);
          CurWin.Draw;}
        end
        else
          Running:=False;
      end;
    end;
  until not Running;
end;

initialization
  FillChar(EvtKeys, SizeOf(EvtKeys), #0);
  InputMask:=#0;

end.
