program netmgr;

{$mode objfpc}{$H+}

uses Classes, SysUtils, eventlog, dos, cuclient;

var
  log: TEventLog;
  cu: THomeCU;

begin
  if ParamStr(2) = 'ac:9b:0a:5e:88:ad' then
    Exit;
  log:=TEventLog.Create(Nil);
  cu:=THomeCU.Create(Nil);
  try
    cu.Connect;
    log.LogType:=ltFile;
    log.FileName:='/tmp/netmgr.log';
    log.AppendContent:=True;
    log.Active:=True;
    log.Info('Netmgr ran with the following params: '+ParamStr(1)+' '+ParamStr(2)+' '+ParamStr(3)+' '+ParamStr(4));
    if not cu.IpSet then
      Exit;
    cu.IpSet:=False;
    if ParamStr(1) = 'add' then
    begin
      cu.Log('Network add for '+ParamStr(3)+' '+ParamStr(4));
      Exec('/usr/sbin/ipset', 'add internet '+ParamStr(3));
    end
    else if ParamStr(1) = 'del' then
    begin
      cu.Log('Network del for '+ParamStr(3)+' '+ParamStr(4));
      Exec('/usr/sbin/ipset', 'del internet '+ParamStr(3));
    end
    else if ParamStr(4) <> '' then
      cu.Log('Network ping from '+ParamStr(4))
    else
      cu.Log(ParamStr(1)+' request by '+ParamStr(3)+'['+ParamStr(2)+']');
  finally
    cu.Free;
    log.Free;
  end;
end.
