{ TODO: WARNING: This unit is experimental! }

unit BrookHTTPCookies;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  DateUtils,
  Classes,
  BrookUtility;

resourcestring
  SBrookInvalidCookieName = 'Invalid cookie name: %s.';

type
  EBrookHTTPCookie = class(Exception);

  TBrookHTTPCookieSameSite = (ssNone, ssStrict, ssLax);

  TBrookHTTPCookie = class(TCollectionItem)
  private
    FName: string;
    FValue: string;
    FDomain: string;
    FPath: string;
    FExpires: TDateTime;
    FHttpOnly: Boolean;
    FSecure: Boolean;
    FMaxAge: Integer;
    FSameSite: TBrookHTTPCookieSameSite;
    procedure SetMaxAge(AValue: Integer);
    procedure SetName(const AValue: string);
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(ASource: TPersistent); override;
    function ToHeader: string; inline;
    function ToString: string; override;
    procedure Clear; virtual;
    procedure Expire; virtual;
    procedure Persist; virtual;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property Domain: string read FDomain write FDomain;
    property Path: string read FPath write FPath;
    property Expires: TDateTime read FExpires write FExpires;
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    property Secure: Boolean read FSecure write FSecure;
    property MaxAge: Integer read FMaxAge write SetMaxAge;
    property SameSite: TBrookHTTPCookieSameSite read FSameSite write FSameSite;
  end;

  TBrookHTTPCookieClass = class of TBrookHTTPCookie;

  TBrookHTTPCookieListEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBrookHTTPCookie;
    property Current: TBrookHTTPCookie read GetCurrent;
  end;

  TBrookHTTPCookieList = class(TOwnedCollection)
  protected
    function GetItem(AIndex: Integer): TBrookHTTPCookie; virtual;
    procedure SetItem(AIndex: Integer; AValue: TBrookHTTPCookie); virtual;
  public
    constructor Create(AOwner: TPersistent); virtual;
    class function GetCookieClass: TBrookHTTPCookieClass; virtual;
    procedure Assign(ASource: TPersistent); override;
    function GetEnumerator: TBrookHTTPCookieListEnumerator;
    function Add: TBrookHTTPCookie; virtual;
    function Remove(const AName: string): Boolean; virtual;
    function IndexOf(const AName: string): Integer; virtual;
    function Find(const AName: string): TBrookHTTPCookie; virtual;
    function First: TBrookHTTPCookie; virtual;
    function Last: TBrookHTTPCookie; virtual;
    function ToString: string; override;
    property Items[AIndex: Integer]: TBrookHTTPCookie read GetItem
      write SetItem; default;
  end;

implementation

{ TBrookHTTPCookie }

constructor TBrookHTTPCookie.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FExpires := -1;
  FMaxAge := -1;
  FPath := '/';
end;

procedure TBrookHTTPCookie.Assign(ASource: TPersistent);
var
  VSrc: TBrookHTTPCookie;
begin
  if ASource is TBrookHTTPCookie then
  begin
    VSrc := ASource as TBrookHTTPCookie;
    Name := VSrc.FName;
    Value := VSrc.FValue;
    Domain := VSrc.FDomain;
    Path := VSrc.FPath;
    Expires := VSrc.FExpires;
    HttpOnly := VSrc.FHttpOnly;
    Secure := VSrc.FSecure;
    MaxAge := VSrc.FMaxAge;
    SameSite := VSrc.FSameSite;
  end
  else
    inherited Assign(ASource);
end;

function TBrookHTTPCookie.ToString: string;
begin
  if FName.IsEmpty then
    Exit('');
  Result := Concat(FName, '=', Brook.EncodeURL(FValue));
  if FMaxAge > -1 then
    Result := Concat(Result, '; Max-Age=', IntToStr(FMaxAge));
  if Length(FDomain) > 0 then
    Result := Concat(Result, '; Domain=', FDomain);
  if Length(FPath) > 0 then
    Result := Concat(Result, '; Path=', FPath);
  if FExpires > -1 then
    Result := Concat(Result, '; Expires=', Brook.DateTimeToGmt(FExpires));
  if FHttpOnly then
    Result := Concat(Result, '; HttpOnly');
  if FSecure then
    Result := Concat(Result, '; Secure');
  case FSameSite of
    ssStrict: Result := Concat(Result, '; SameSite=Strict');
    ssLax: Result := Concat(Result, '; SameSite=Lax');
  end;
end;

function TBrookHTTPCookie.ToHeader: string;
begin
  Result := Concat('Set-Cookie: ', ToString);
end;

procedure TBrookHTTPCookie.SetMaxAge(AValue: Integer);
begin
  if AValue = FMaxAge then
    Exit;
  FMaxAge := AValue;
  if AValue > 0 then
    FExpires := Brook.DateTimeToUTC(IncSecond(Now, AValue))
  else
    Expire;
end;

procedure TBrookHTTPCookie.SetName(const AValue: string);
begin
  if AValue = FName then
    Exit;
  if not IsValidIdent(AValue) then
    raise EBrookHTTPCookie.CreateFmt(SBrookInvalidCookieName, [AValue]);
  FName := AValue;
end;

procedure TBrookHTTPCookie.Clear;
begin
  FName := '';
  FValue := '';
  FMaxAge := -1;
  FDomain := '';
  FPath := '';
  FExpires := -1;
  FHTTPOnly := False;
  FSecure := False;
  FSameSite := ssNone;
end;

procedure TBrookHTTPCookie.Expire;
begin
  FExpires := EncodeDate(1970, 1, 1);
end;

procedure TBrookHTTPCookie.Persist;
begin
  FExpires := EncodeDate(9999, 12, 31) + EncodeTime(23, 59, 59, 999);
end;

{ TBrookHTTPCookieListEnumerator }

function TBrookHTTPCookieListEnumerator.GetCurrent: TBrookHTTPCookie;
begin
  Result := TBrookHTTPCookie(inherited GetCurrent);
end;

{ TBrookHTTPCookieList }

constructor TBrookHTTPCookieList.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetCookieClass);
end;

class function TBrookHTTPCookieList.GetCookieClass: TBrookHTTPCookieClass;
begin
  Result := TBrookHTTPCookie;
end;

function TBrookHTTPCookieList.GetEnumerator: TBrookHTTPCookieListEnumerator;
begin
  Result := TBrookHTTPCookieListEnumerator.Create(Self);
end;

procedure TBrookHTTPCookieList.Assign(ASource: TPersistent);
var
  C: TBrookHTTPCookie;
begin
  if ASource is TBrookHTTPCookieList then
  begin
    Clear;
    for C in (ASource as TBrookHTTPCookieList) do
      Add.Assign(C);
  end
  else
    inherited Assign(ASource);
end;

function TBrookHTTPCookieList.GetItem(AIndex: Integer): TBrookHTTPCookie;
begin
  Result := TBrookHTTPCookie(inherited GetItem(AIndex));
end;

procedure TBrookHTTPCookieList.SetItem(AIndex: Integer;
  AValue: TBrookHTTPCookie);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBrookHTTPCookieList.Add: TBrookHTTPCookie;
begin
  Result := TBrookHTTPCookie(inherited Add);
end;

function TBrookHTTPCookieList.Remove(const AName: string): Boolean;
var
  I: Integer;
begin
  I := IndexOf(AName);
  Result := I > -1;
  if Result then
    inherited Delete(I);
end;

function TBrookHTTPCookieList.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Name, AName) then
      Exit;
  Result := -1;
end;

function TBrookHTTPCookieList.Find(const AName: string): TBrookHTTPCookie;
var
  C: TBrookHTTPCookie;
begin
  for C in Self do
    if SameText(C.Name, AName) then
      Exit(C);
  Result := nil;
end;

function TBrookHTTPCookieList.First: TBrookHTTPCookie;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(0);
end;

function TBrookHTTPCookieList.Last: TBrookHTTPCookie;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(Pred(Count));
end;

function TBrookHTTPCookieList.ToString: string;
const
  CRLF = #13#10;
var
  C: TBrookHTTPCookie;
begin
  Result := '';
  for C in Self do
    Result := Concat(Result, C.ToHeader, CRLF);
  SetLength(Result, Length(Result) - Length(CRLF));
end;

end.
