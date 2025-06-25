unit modulealienpets;

{$mode ObjFPC}{$H+}
interface

uses
  BrookUtility,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookURLRouter;


type

  { TRouteSpecies }

  TRouteSpecies = class(TBrookURLRoute)
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;

  { TRoutePetAliens }

  TRoutePetAliens = class(TBrookURLRoute)
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;

  TRoutePetAlien = class(TBrookURLRoute)
  strict private
    procedure Post(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure Put(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure Delete(const aPetId: integer; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;


implementation
uses
  fpjson, jsonparser, SysUtils,
  standardheaders, standardresponses, alienpets;

var
  pets : TAlienPetsArchive;

procedure InitAlienPetsArchive;
var
  tmp : TAlienPet;
begin
  tmp := TAlienPet.Create;
  tmp.Id:= 1;
  tmp.Name:= 'Prootelon';
  tmp.Species:= 'Zog';
  pets.Add(tmp);

  tmp := TAlienPet.Create;
  tmp.Id:= 2;
  tmp.Name:= 'Bidibop';
  tmp.Species:= 'Bloop';
  pets.Add(tmp);

  tmp := TAlienPet.Create;
  tmp.Id:= 3;
  tmp.Name:= 'Sguish';
  tmp.Species:= 'Gleep';
  pets.Add(tmp);
end;


{ TRouteSpecies }

procedure TRouteSpecies.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if HandleOptions(ARoute, ARequest, AResponse) then
    exit;
  AddStandardHeaders(aResponse);
  AResponse.Send('["Zog", "Gleep", "Bloop"]', 'application/json; charset=utf-8', 200);
end;

procedure TRouteSpecies.AfterConstruction;
begin
  Methods:= [rmGET, rmOPTIONS];
  Pattern:= '/species';
end;

procedure ReadAlienFromJson(const aData : TJSONData; alien : TAlienPet);
var
  tmp : TJSONData;
begin
  alien.Clear;
  tmp := aData.FindPath('id');
  if Assigned(tmp) then
    alien.Id := tmp.Value;
  tmp := aData.FindPath('name');
  if Assigned(tmp) then
    alien.Name := tmp.Value;
  tmp := aData.FindPath('species');
  if Assigned(tmp) then
    alien.Species := tmp.Value;
end;

{ TRoutePetAliens }

procedure TRoutePetAliens.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if HandleOptions(ARoute, ARequest, AResponse) then
    exit;
  AddStandardHeaders(AResponse);
  AResponse.Send('{"pets":' + pets.ToJson + '}', 'application/json', 200);
end;

procedure TRoutePetAliens.AfterConstruction;
begin
  Methods:= [rmGET, rmOPTIONS];
  Pattern:= '/alienpets';
end;

procedure TRoutePetAlien.Post(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  jData : TJSONData;
  newAlien : TAlienPet;
begin
  if ARequest.Payload.Text <> '' then
  begin
    jData := GetJSON(ARequest.Payload.Text);
    try
      newAlien := TAlienPet.Create;
      ReadAlienFromJson(jData, newAlien);
      newAlien.Id:= pets.GetNewId;
      pets.Add(newAlien);
    finally
      jData.Free;
    end;
    AResponse.Send(newAlien.ToJson, 'application/json', 200);
  end
  else
    AResponse.Send('Invalid request', 'text/plain', 400);
end;

procedure TRoutePetAlien.Put(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  jData : TJSONData;
  editAlien : TAlienPet;
begin
  if ARequest.Payload.Text <> '' then
  begin
    jData := GetJSON(ARequest.Payload.Text);
    try
      editAlien := TAlienPet.Create;
      try
        ReadAlienFromJson(jData, editAlien);
        pets.Update(editAlien);
        AResponse.Send(editAlien.ToJson, 'application/json', 200);
      finally
        editAlien.Free;
      end;
    finally
      jData.Free;
    end;
  end
  else
    AResponse.Send('Invalid request', 'text/plain', 400);
end;

procedure TRoutePetAlien.Delete(const aPetId: integer; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  pets.Delete(aPetId);
  AResponse.Send('deleted ' + IntToStr(aPetId), 'text/plain', 200);
end;

procedure TRoutePetAlien.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  tmpId : integer;
begin
  if HandleOptions(ARoute, ARequest, AResponse) then
    exit;
  AddStandardHeaders(AResponse);

  tmpId := 0;
  if (Length(ARoute.Segments) >= 1) then
    tmpId := StrToInt(RightStr(ARoute.Segments[Length(ARoute.Segments)-1], Length(ARoute.Segments[Length(ARoute.Segments)-1]) - 1 ));

  if ARequest.Method = 'POST' then
    Post(ARoute, ARequest, AResponse)
  else if ARequest.Method = 'PUT' then
    Put(ARoute, ARequest, AResponse)
  else if ARequest.Method = 'DELETE' then
    Delete(tmpId, ARoute, ARequest, AResponse);
end;

procedure TRoutePetAlien.AfterConstruction;
begin
  Methods:= [rmPOST, rmPUT, rmDELETE, rmOPTIONS];
  Pattern := 'alienpet(([/])|(/[0-9]+))?';
end;


initialization
  pets := TAlienPetsArchive.Create;
  InitAlienPetsArchive;

finalization
  pets.Free;

end.
