unit routeping;

{$mode ObjFPC}{$H+}

interface

uses
  BrookUtility,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookURLRouter;

type

  { TRoutePing }

  TRoutePing = class(TBrookURLRoute)
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TRoutePing }

procedure TRoutePing.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('<html><head><title>Ping</title></head><body>Pong</body></html>', 'text/html; charset=utf-8', 200);
end;

procedure TRoutePing.AfterConstruction;
begin
  Methods:= [rmGET];
  Pattern:= '/ping';
end;

end.

