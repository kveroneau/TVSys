unit mcshell;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, winsys, netcard, cmdsys, memcard, timidity;

procedure StartMCShell;
procedure RunCmd(cmd, params: string);

implementation

const
  VERSION='0.1b';

var
  ShellWin: TKevWindow;
  Card: TNetCard;
  BlkID: byte;
  info: PBlockInfo;
  NoteBuf: TStringList;

procedure MCConnect(port: word; hostname: string);
begin
  WriteLn('Connecting to '+hostname+':'+IntToStr(port)+'...');
  ShellWin.Draw;
  if Assigned(Card) then
    Card.Free;
  Card:=TNetCard.Create(hostname, port);
  WriteLn('Connected.');
end;

procedure MCLogin(auth_token: string);
begin
  if not Assigned(Card) then
    Exit;
  WriteLn('Authenticating...');
  Card.Authenticate(auth_token);
  WriteLn('Done.');
end;

procedure ListBlocks;
var
  bl: TStringList;
  i, used: integer;
begin
  used:=0;
  bl:=Card.BlockList;
  try
    for i:=0 to bl.Count-1 do
      if bl.Strings[i] <> 'NONAME' then
      begin
        Inc(used);
        WriteLn(IntToStr(i+1)+': '+bl.Strings[i]);
      end;
  finally
    bl.Free;
  end;
  {WriteLn('Next Free Block: '+IntToStr(Card.FindFree));}
  WriteLn('Block Free: '+IntToStr(15-used));
end;

procedure MCInfo(blkid: byte);
var
  info: TBlockInfo;
begin
  Card.GetInfo(blkid, @info);
  WriteLn('Title: '+info.title);
  WriteLn('AppNo: '+IntToStr(info.appno));
  WriteLn('TypNo: '+IntToStr(info.typno));
  WriteLn('Total: '+IntToStr(info.total));
end;

procedure EnterNote(line: string);
var
  blk: TMemoryStream;
begin
  if line <> '.' then
  begin
    NoteBuf.Add(line);
    CurWin.LineMode:=lmLine;
    CurWin.Draw;
    Exit;
  end;
  info^.total:=NoteBuf.Count;
  blk:=Card.ReadBlock(BlkID);
  blk.WriteAnsiString(NoteBuf.Text);
  blk.WriteByte(65);
  blk.Write(CurWin.WinInfo, SizeOf(CurWin.WinInfo));
  Card.WriteBlock(BlkID, blk, info);
  blk.Free;
  Dispose(info);
  info:=Nil;
  NoteBuf.Free;
end;

procedure CreateNote(title: string);
var
  NoteWin: TKevWindow;
begin
  BlkID:=Card.FindFree;
  if BlkID = 0 then
  begin
    WriteLn('There are no free blocks.');
    Exit;
  end;
  if not Assigned(info) then
    New(info);
  info^.title:=title;
  info^.appno:=7;
  info^.typno:=8;
  info^.nextid:=0;
  NoteWin:=CreateWindow('Note - '+title, 40, 4, 40, 20, 0, 7);
  NoteWin.ReadLn('', @EnterNote);
  NoteBuf:=TStringList.Create;
  if EnableMusicOS then
    PlayMIDI('/opt/TVSys/MIDI/Piano.mid');
end;

procedure ShowNote(blkid: integer);
var
  blk: TMemoryStream;
  NoteWin: TKevWindow;
  info: TBlockInfo;
  note: string;
  wi: PWinInfo;
begin
  { Would be nice to store additional meta-data on the note.
    Perhaps it could be placed after the AnsiString itelf, that way it
    is still compatible with the existing NetNotepad software. }
  Card.GetInfo(blkid, @info);
  if info.typno <> 8 then
  begin
    WriteLn(' * Incompatible format!');
    Exit;
  end;
  blk:=Card.ReadBlock(blkid);
  note:=blk.ReadAnsiString;
  if blk.ReadByte = 65 then
  begin
    New(wi);
    blk.Read(wi^, SizeOf(wi^));
    NoteWin:=RestoreWindow('Note - '+info.title, wi);
    Dispose(wi);
  end
  else
    NoteWin:=CreateWindow('Note - '+info.title, 40, 4, 40, 20, 0, 7);
  NoteWin.WriteLn(note);
  DrawWindows;
  blk.Free;
  if EnableMusicOS then
    PlayMIDI('/opt/TVSys/MIDI/Piano2.mid');
end;

procedure RunCmd(cmd, params: string);
begin
  if cmd = 'exit' then
  begin
    SaveWinState('MCShell', ShellWin);
    FreeAndNil(ShellWin);
    CurWin:=Nil;
    DrawWindows;
    Exit;
  end;
  try
    case cmd of
      'connect': MCConnect(StrToInt(GetToken(params)), GetToken(params));
      'login': MCLogin(GetToken(params));
      'card': Card.SelectCard(StrToInt(GetToken(params)));
      'ls': ListBlocks;
      'info': MCInfo(StrToInt(GetToken(params)));
      'cls': CurWin.ClrScr;
      'note': CreateNote(GetToken(params));
      'cat': ShowNote(StrToInt(GetToken(params)));
      'rm': Card.DeleteBlock(StrToInt(GetToken(params)));
    else
      WriteLn('?Huh');
    end;
  except
    On EAuthenticationFailed do WriteLn(' * Authentication Failed!');
    On EInvalidCard do WriteLn(' * Invalid Card Selected!');
    On EAuthError do WriteLn(' * Authorization Error!');
  end;
  ShellWin.LineMode:=lmCmd;
  if CurWin = ShellWin then
    ShellWin.Draw;
end;

procedure OpenShellWin;
begin
  ShellWin:=LoadWinState('MCShell', 'MCShell', 16, 16, 60, 10,7,0);
  with ShellWin do
  begin
    WriteLn('Memory Card Server Shell v'+VERSION);
    {$IFDEF DEBUGX}
    MCConnect(3845, 'pi.home.lan');
    MCLogin('HomeCU');
    Card.SelectCard(0);
    {$ENDIF}
    CmdPrompt:='MC>';
    LineMode:=lmCmd;
    OnCmd:=@RunCmd;
    Draw;
  end;
  if EnableMusicOS then
    PlayMIDI('/opt/TVSys/MIDI/GmaMozart.mid');
end;

procedure StartMCShell;
begin
  OpenShellWin;
end;

initialization
  Card:=Nil;

finalization
  if Assigned(Card) then
    Card.Free;

end.
