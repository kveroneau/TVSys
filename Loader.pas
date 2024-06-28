program Loader;

{$mode objfpc}{$h+}

uses SysUtils, dos;

var
  rt: integer;

begin
  rt:=0;
  while rt = 0 do
  begin
    Exec('/usr/bin/cp', '/opt/TVSys/TVSys '+GetUserDir+'/TVSys');
    Exec(GetUserDir+'/TVSys', '');
    rt:=DosExitCode;
  end;
end.