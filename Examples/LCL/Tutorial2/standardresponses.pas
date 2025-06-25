unit standardresponses;

{$mode ObjFPC}{$H+}

interface

uses
  BrookHTTPResponse,
  BrookHTTPRequest,
  BrookURLRouter;

function HandleOptions(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse): boolean;

implementation

uses
  standardheaders;

function HandleOptions(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse): boolean;
begin
  Result := false;
  if ARequest.Method = 'OPTIONS' then
  begin
    AddStandardHeadersForOptions(AResponse, ARoute);
    AResponse.Send('', 'text/html', 200);
    Result := true;
  end;
end;


end.

