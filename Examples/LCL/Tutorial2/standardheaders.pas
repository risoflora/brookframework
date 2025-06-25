unit standardheaders;

{$mode ObjFPC}{$H+}

interface

uses
  BrookHTTPResponse,
  BrookURLRouter,
  BrookUtility;

procedure AddStandardHeaders(AResponse: TBrookHTTPResponse);
procedure AddStandardHeadersForOptions(AResponse: TBrookHTTPResponse; ARoute: TBrookURLRoute);

implementation

procedure AddStandardHeaders(AResponse: TBrookHTTPResponse);
begin
  //{$IFDEF DEVELOPMENT}
  AResponse.Headers.Add('Access-Control-Allow-Origin', '*');
  //{$ENDIF}
  AResponse.Headers.Add('Server', 'Alien puppies server');
  AResponse.Headers.Add('Keep-Alive', 'timeout=5, max=99');
  AResponse.Headers.Add('Connection', 'Keep-Alive');
  AResponse.Headers.Add('X-Frame-Options', 'SAMEORIGIN'); // https://stackoverflow.com/questions/27358966/how-can-i-set-x-frame-options-on-an-iframe
end;

procedure AddStandardHeadersForOptions(AResponse: TBrookHTTPResponse; ARoute: TBrookURLRoute);
var
  mt, sep : String;
begin
  AddStandardHeaders(AResponse);

  if ARoute.Methods = [] then
    mt := 'GET, PUT, POST, OPTIONS, HEAD'
  else
  begin
    mt := '';
    sep  := '';
    if rmGet in ARoute.Methods then
    begin
      mt := mt + sep + 'GET';
      sep := ', ';
    end;
    if rmPOST in ARoute.Methods then
    begin
      mt := mt + sep + 'POST';
      sep := ', ';
    end;
    if rmPUT in ARoute.Methods then
    begin
      mt := mt + sep + 'PUT';
      sep := ', ';
    end;
    if rmDELETE in ARoute.Methods then
    begin
      mt := mt + sep + 'DELETE';
      sep := ', ';
    end;
    if rmPATCH in ARoute.Methods then
    begin
      mt := mt + sep + 'PATCH';
      sep := ', ';
    end;
    if rmOPTIONS in ARoute.Methods then
    begin
      mt := mt + sep + 'OPTIONS';
      sep := ', ';
    end;
    if rmHEAD in ARoute.Methods then
    begin
      mt := mt + sep + 'HEAD';
      sep := ', ';
    end;
  end;

  AResponse.Headers.Add('Access-Control-Allow-Methods', mt);
  AResponse.Headers.Add('Access-Control-Allow-Headers', 'x-requested-with, content-type, authorization');
end;

end.

