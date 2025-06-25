program TestProject;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, httpserver, standardheaders, standardresponses;

type

  { TBrookframeworkTest }

  TBrookframeworkTest = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TBrookframeworkTest }

procedure TBrookframeworkTest.DoRun;
var
  server: THTTPServer;
begin
  server := THTTPServer.Create(nil);
  try
    server.SetupServer;
    server.Open;
    if not server.Active then
    begin
      WriteLn('Unable to start server at https://localhost:', server.Port);
      Terminate(-1);
    end
    else
    begin
      WriteLn('Server running at https://localhost:', server.Port);
      ReadLn;
    end;
  finally
    server.Free;
  end;
  Terminate;
end;

constructor TBrookframeworkTest.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TBrookframeworkTest.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TBrookframeworkTest;
begin
  Application:=TBrookframeworkTest.Create(nil);
  Application.Title:='Brookframework Test Server';
  Application.Run;
  Application.Free;
end.

