unit winsys;

{$mode objfpc}{$h+}

interface

uses Classes, SysUtils, zwin, crt, qvfs;

type

  PWinInfo = ^TWinInfo;
  TWinInfo = packed record
    x,y,width,height,fg,bg: byte;
  end;

  {TKevWindow = class(TComponent);}

  TLineEvent = procedure(line: string);
  TCmdEvent = procedure(cmd, params: string);
  TDrawEvent = procedure(Win: TComponent);

  TLineMode = (lmRaw, lmLine, lmCmd, lmParams);
  TWinMode = (wmNormal, wmMoving, wmSizing);

  TKevWindow = class(TComponent)
  private
    FTitle: string;
    Contents: TStringList;
    FCurLine, FPrompt, FCmd, FParams: string;
    FLineMode: TLineMode;
    FWinMode: TWinMode;
    FStored: PWinInfo;
    FOnLine: TLineEvent;
    FOnCmd: TCmdEvent;
    FOnDraw: TDrawEvent;
    function GetCurLine: string;
  protected
    FWinInfo: TWinInfo;
  public
    property Title: string read FTitle;
    property x: byte read FWinInfo.x;
    property y: byte read FWinInfo.y;
    property width: byte read FWinInfo.width;
    property height: byte read FWinInfo.height;
    property fg: byte read FWinInfo.fg;
    property bg: byte read FWinInfo.bg;
    property WinInfo: TWinInfo read FWinInfo;
    property WinMode: TWinMode read FWinMode write FWinMode;
    property LineMode: TLineMode read FLineMode write FLineMode;
    property CmdPrompt: string write FPrompt;
    property Cmd: string read FCmd;
    property Params: string read FParams;
    property OnLine: TLineEvent read FOnLine write FOnLine;
    property OnCmd: TCmdEvent read FOnCmd write FOnCmd;
    property OnDraw: TDrawEvent read FOnDraw write FOnDraw;
    property Line: string read FCurLine;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw;
    procedure WriteLn(const s: string);
    procedure Write(const s: string);
    procedure ReadLn(const prompt: string; Callback: TLineEvent);
    procedure ClrScr;
    procedure ClrLn;
    procedure BackSpace;
    procedure Colour(const fore,back: byte);
    procedure WinControl(const ctrl: byte);
  end;

var
  WinList: TComponent;
  CurWin: TKevWindow;
  InputMask: char;

function CreateWindow(title: string; x,y,w,h,fg,bg: byte): TKevWindow;
function RestoreWindow(title: string; WinState: PWinInfo): TKevWindow;
procedure DrawWindows;
procedure WriteLn(const s: string);
procedure Write(const s: string);
procedure ReadLn(const prompt: string; Callback: TLineEvent);
function LoadWinState(const fname, title: string; x,y,w,h,fg,bg: byte): TKevWindow;
procedure SaveWinState(const fname: string; w: TKevWindow);

implementation

function LoadWinState(const fname, title: string; x,y,w,h,fg,bg: byte): TKevWindow;
var
  fi: PVFS;
  wi: PWinInfo;
begin
  fi:=GetFile(fname, '[=]');
  if fi = Nil then
    Result:=CreateWindow(title, x,y,w,h,fg,bg)
  else
  begin
    wi:=@fi^.data[1];
    Result:=RestoreWindow(title, wi);
  end;
end;

procedure SaveWinState(const fname: string; w: TKevWindow);
var
  fi: PVFS;
begin
  fi:=GetFile(fname, '[=]');
  if fi = Nil then
  begin
    AddFile(fname, '[=]', '');
    fi:=GetFile(fname, '[=]');
  end;
  fi^.data[0]:=chr(SizeOf(TWinInfo));
  Move(w.WinInfo, fi^.data[1], SizeOf(TWinInfo));
end;

procedure WriteLn(const s: string);
begin
  if Assigned(CurWin) then
    CurWin.WriteLn(s)
  else
    System.WriteLn(s);
end;

procedure Write(const s: string);
begin
  if Assigned(CurWin) then
    CurWin.Write(s)
  else
    System.Write(s);
end;

procedure ReadLn(const prompt: string; Callback: TLineEvent);
var
  s: string;
begin
  if Assigned(CurWin) then
    CurWin.ReadLn(prompt, Callback)
  else
  begin
    System.ReadLn(s);
    Callback(s);
  end;
end;

procedure TKevWindow.ReadLn(const prompt: string; Callback: TLineEvent);
begin
  FPrompt:=prompt;
  FOnLine:=Callback;
  FLineMode:=lmLine;
  Draw;
end;

function CreateWindow(title: string; x,y,w,h,fg,bg: byte): TKevWindow;
begin
  Result:=TKevWindow.Create(WinList);
  Result.FTitle:=title;
  Result.FWinInfo.x:=x;
  Result.FWinInfo.y:=y;
  Result.FWinInfo.width:=w;
  Result.FWinInfo.height:=h;
  Result.FWinInfo.fg:=fg;
  Result.FWinInfo.bg:=bg;
  CurWin:=Result;
end;

function RestoreWindow(title: string; WinState: PWinInfo): TKevWindow;
begin
  Result:=TKevWindow.Create(WinList);
  Result.FTitle:=title;
  Move(WinState^, Result.FWinInfo, SizeOf(WinState^));
  CurWin:=Result;
end;

procedure RenderClock;
begin
  Window(ScreenWidth div 2,1,ScreenWidth,ScreenHeight);
  System.Write(' Clock Here ');
end;

procedure DrawWindows;
var
  i: integer;
  win: TKevWindow;
begin
  for i:=0 to WinList.ComponentCount-1 do
    TKevWindow(WinList.Components[i]).Draw;
  {RenderClock;}
  if Assigned(CurWin) then
    CurWin.Draw;
end;

constructor TKevWindow.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTitle:='Untitled';
  FCurLine:='';
  FLineMode:=lmRaw;
  FPrompt:='';
  FCmd:='';
  FParams:='';
  Contents:=TStringList.Create;
  FStored:=Nil
end;

destructor TKevWindow.Destroy;
begin
  if FStored <> Nil then
    Dispose(FStored);
  Contents.Free;
  inherited Destroy;
end;

function TKevWindow.GetCurLine: string;
begin
  if (FLineMode = lmRaw) or (FLineMode = lmLine) then
    Result:=FPrompt+FCurLine
  else if FLineMode = lmParams then
    Result:=FPrompt+FCmd+' '+FParams
  else if FLineMode = lmCmd then
    Result:=FPrompt+FCmd;
end;

procedure TKevWindow.Draw;
var
  i: integer;
  wattr: string;
begin
  wattr:='';
  if FWinMode = wmMoving then
    wattr:='M'
  else if FWinMode = wmSizing then
    wattr:='S';
  if FLineMode = lmRaw then
    wattr:=wattr+'r'
  else if FLineMode = lmLine then
    wattr:=wattr+'l'
  else if FLineMode = lmCmd then
    wattr:=wattr+'c'
  else if FLineMode = lmParams then
    wattr:=wattr+'p';
  TextColor(fg);
  TextBackground(bg);
  OpenWindow(FTitle+' <'+wattr+'>', x, y, width, height);
  if Assigned(FOnDraw) then
    FOnDraw(Self)
  else
  begin
    for i:=0 to Contents.Count-1 do
    begin
      GotoXY(1,i+1);
      System.WriteLn(Contents.Strings[i]);
    end;
    System.Write(GetCurLine);
  end;
end;

procedure TKevWindow.WriteLn(const s: string);
begin
  if s = 'CurLine' then
  begin
    Contents.Add(GetCurLine);
    FCurLine:='';
    FCmd:='';
    FParams:='';
    FLineMode:=lmRaw;
  end
  else
    Contents.Add(s);
end;

procedure TKevWindow.Write(const s: string);
begin
  if Length(GetCurLine) > width-5 then
    Exit;
  if (FLineMode = lmRaw) or (FLineMode = lmLine) then
    FCurLine:=FCurLine+s
  else if FLineMode = lmParams then
    FParams:=FParams+s
  else if FLineMode = lmCmd then
  begin
    if s = ' ' then
      FLineMode:=lmParams
    else
      FCmd:=FCmd+s;
  end;
  System.Write(s);
end;

procedure TKevWindow.BackSpace;
begin
  GotoXY(1, WhereY);
  ClrEOL;
  if FLineMode = lmLine then
  begin
    if Length(FCurLine) > 0 then
      FCurLine:=LeftStr(FCurLine, Length(FCurLine)-1);
    System.Write(GetCurLine);
  end
  else if FLineMode = lmCmd then
  begin
    if Length(FCmd) > 0 then
      FCmd:=LeftStr(FCmd, Length(FCmd)-1);
    System.Write(GetCurLine);
  end
  else if FLineMode = lmParams then
  begin
    if Length(FParams) > 0 then
      FParams:=LeftStr(FParams, Length(FParams)-1);
    System.Write(GetCurLine);
  end;
end;

procedure TKevWindow.ClrScr;
begin
  Contents.Clear;
  FLineMode:=lmRaw;
  FCurLine:='';
  FCmd:='';
  FParams:='';
  Draw;
end;

procedure TKevWindow.ClrLn;
begin
  FCurLine:='';
  FCmd:='';
  FParams:='';
  Draw;
end;

procedure TKevWindow.Colour(const fore,back: byte);
begin
  FWinInfo.fg:=fore;
  FWinInfo.bg:=back;
end;

procedure TKevWindow.WinControl(const ctrl: byte);
begin
  if ctrl = ord('f') then
  begin
    if FStored = Nil then
    begin
      New(FStored);
      Move(FWinInfo, FStored^, SizeOf(TWinInfo));
      FWinInfo.x:=1;
      FWinInfo.y:=1;
      FWinInfo.width:=ScreenWidth-2;
      FWinInfo.height:=ScreenHeight-2;
    end
    else
    begin
      Move(FStored^, FWinInfo, SizeOf(TWinInfo));
      Dispose(FStored);
      FStored:=Nil;
    end;
    DrawWindows;
  end;
  if FWinMode = wmNormal then
    Exit;
  if FWinMode = wmMoving then
  begin
    case ctrl of
      13: FWinMode:=wmNormal;
      72: Dec(FWinInfo.y);
      80: Inc(FWinInfo.y);
      75: Dec(FWinInfo.x);
      77: Inc(FWinInfo.x);
    end;
    DrawWindows;
  end
  else if FWinMode = wmSizing then
  begin
    case ctrl of
      72: Dec(FWinInfo.height);
      80: Inc(FWinInfo.height);
      75: Dec(FWinInfo.width);
      77: Inc(FWinInfo.width);
    end;
    DrawWindows;
  end;
end;

initialization
  WinList:=TComponent.Create(Nil);
  CurWin:=Nil;

finalization
  CurWin:=Nil;
  WinList.Free;

end.
