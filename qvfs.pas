unit qvfs;

{ Quick Virtual File System: Same as how I did it in QBASIC! }
{$mode objfpc}{$h+}

interface

uses Classes;

type
  PVFS = ^TVFS;
  TVFS = record
    na: string[20];
    typ: string[3];
    wh: string[20];
    data: string[60];
  end;

procedure InitVFS(const fname: string);
procedure DoneVFS;
function FileCount: integer;
procedure AddFile(const fname, typ, data: string);
function FileList: TStringList;
function GetFile(const fname, typ: string): PVFS;
procedure DeleteFile(const fname, typ: string);
procedure MakeDirectory(const fname: string);
procedure SetDirectory(const fname: string);

implementation

const
  VFS_SIZE = 32;

var
  VFSFile: string;
  f: File of TVFS;
  vfs: Array[0..VFS_SIZE-1] of PVFS;
  CurDir: string[20];

function GetFile(const fname, typ: string): PVFS;
var
  i: integer;
begin
  Result:=Nil;
  for i:=0 to High(vfs) do
    if vfs[i] <> Nil then
      if vfs[i]^.wh = CurDir then
      begin
        if vfs[i]^.na = fname then
        begin
          if (typ <> '') and (vfs[i]^.typ = typ) then
            Result:=vfs[i]
          else if typ = '' then
            Result:=vfs[i];
        end;
        if Result <> Nil then
          Exit;
      end;
end;

function FileList: TStringList;
var
  i: integer;
begin
  Result:=TStringList.Create;
  for i:=0 to High(vfs) do
    if vfs[i] <> Nil then
      if vfs[i]^.wh = CurDir then
        Result.Add('<'+vfs[i]^.typ+'> '+vfs[i]^.na);
end;

procedure AddFile(const fname, typ, data: string);
var
  i, i2: integer;
begin
  i2:=-1;
  for i:=High(vfs) downto 0 do
    if vfs[i] = Nil then
      i2:=i;
  if i2 = -1 then
    Exit;
  New(vfs[i2]);
  vfs[i2]^.na:=fname;
  vfs[i2]^.typ:=typ;
  vfs[i2]^.wh:=CurDir;
  vfs[i2]^.data:=data;
end;

procedure DeleteFile(const fname, typ: string);
var
  i: integer;
  fi: PVFS;
begin
  fi:=GetFile(fname, typ);
  if fi = Nil then
    Exit;
  for i:=0 to High(vfs) do
    if vfs[i] = fi then
    begin
      Dispose(vfs[i]);
      vfs[i]:=Nil;
      Exit;
    end;
end;

function FileCount: integer;
var
  i: integer;
begin
  Result:=0;
  for i:=0 to High(vfs) do
    if vfs[i] <> Nil then
      Inc(Result);
end;

procedure MakeDirectory(const fname: string);
begin
  AddFile(fname, '-=-', 'Directory Entry');
end;

procedure SetDirectory(const fname: string);
begin
  CurDir:=fname;
end;

procedure InitVFS(const fname: string);
var
  i: integer;
begin
  VFSFile:=fname;
  FillChar(vfs, SizeOf(vfs), #0);
  Assign(f, fname);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then
    Rewrite(f);
  for i:=0 to FileSize(f)-1 do
  begin
    New(vfs[i]);
    Read(f, vfs[i]^);
  end;
  Close(f);
  CurDir:='FSRoot';
end;

procedure DoneVFS;
var
  i: integer;
begin
  Assign(f, VFSFile);
  Rewrite(f);
  for i:=0 to High(vfs) do
    if vfs[i] <> Nil then
    begin
      Write(f, vfs[i]^);
      Dispose(vfs[i]);
    end;
  Close(f);
end;

initialization
  FillChar(vfs, SizeOf(vfs), #0);

end.
