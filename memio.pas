unit memio;

{$mode objfpc}{$h+}

interface

uses Classes, SysUtils, memcard;

type
  PMemBlock = ^TMemBlock;
  TMemBlock = record
    blkid: byte;
    info: PBlockInfo;
    mem: TMemoryStream;
  end;

var
  Memory: Array[0..31] of PMemBlock;
  Card: TMemCard;

procedure InitMemory(const fname: string; blksize: byte);
procedure DoneMemory;
function ReadBlock(blk: byte): byte;
procedure WriteBlock(blk: byte);
procedure FreeBlock(blk: byte);
procedure ReadInto(blk, target: byte);

implementation

procedure ReadInto(blk, target: byte);
begin
  if not Assigned(Card) then
    Exit;
  if Assigned(Memory[target]) then
    FreeBlock(target);
  New(Memory[target]);
  with Memory[target]^ do
  begin
    blkid:=blk;
    New(info);
    Card.GetInfo(blkid, info);
    mem:=Card.ReadBlock(blkid);
  end;
end;

function NextFree: byte;
var
  i: integer;
begin
  Result:=255;
  for i:=0 to 31 do
    if not Assigned(Memory[i]) then
      Result:=i;
end;

function ReadBlock(blk: byte): byte;
begin
  Result:=255;
  if not Assigned(Card) then
    Exit;
  Result:=NextFree;
  if Result = 255 then
    Exit;
  New(Memory[Result]);
  with Memory[Result]^ do
  begin
    blkid:=blk;
    New(info);
    Card.GetInfo(blkid, info);
    mem:=Card.ReadBlock(blkid);
  end;
end;

procedure WriteBlock(blk: byte);
begin
  if Assigned(Memory[blk]) then
    with Memory[blk]^ do
      Card.WriteBlock(blkid, mem, info);
end;

procedure FreeBlock(blk: byte);
begin
  if Assigned(Memory[blk]) then
  begin
    with Memory[blk]^ do
    begin
      Card.WriteBlock(blkid, mem, info);
      Dispose(info);
      mem.Free;
    end;
    Dispose(Memory[blk]);
    Memory[blk]:=Nil;
  end;
end;

procedure InitMemory(const fname: string; blksize: byte);
begin
  if Assigned(Card) then
    exit;
  Card:=TMemCard.Create(fname, blksize);
end;

procedure DoneMemory;
var
  i: integer;
begin
  if not Assigned(Card) then
    Exit;
  for i:=0 to 31 do
    if Assigned(Memory[i]) then
      FreeBlock(i);
  FreeAndNil(Card);
end;

end.
