(*  _                     _
 * | |__  _ __ ___   ___ | | __
 * | '_ \| '__/ _ \ / _ \| |/ /
 * | |_) | | | (_) | (_) |   <
 * |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2021 Silvio Clecio <silvioprog@gmail.com>
 *
 * Brook framework is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Brook framework; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

{ Contains classes which handles server side cookies. }

unit BrookHTTPCookies;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  DateUtils,
  Classes,
{$IFDEF FPC}
  HMAC,
  Base64,
{$ELSE}
  System.Hash,
  System.NetEncoding,
{$ENDIF}
  BrookUtility;

const
{$IFNDEF FPC}
  {$WRITEABLECONST ON}
{$ENDIF}
  { Prefix to identify a signed cookie. }
  BROOK_COOKIE_SIG_PREFIX: string = 's:';
  { Default cookie name. }
  BROOK_COOKIE_NAME_PREFIX: string = 'BrookCookie';
{$IFNDEF FPC}
  {$WRITEABLECONST OFF}
{$ENDIF}

resourcestring
  { Error message @code('Empty cookie name.'). }
  SBrookEmptyCookieName = 'Empty cookie name.';
  { Error message @code('Invalid cookie name: <cookie-name>.'). }
  SBrookInvalidCookieName = 'Invalid cookie name: %s.';

type
  { Handles exceptions related to HTTP cookies classes. }
  EBrookHTTPCookie = class(Exception);

  { SameSite cookie attribute types. }
  TBrookHTTPCookieSameSite = (
    { The browser will send cookies with both cross-site requests and same-site
      requests. }
    ssNone,
    { The browser will only send cookies for same-site requests. }
    ssStrict,
    { Same-site cookies are withheld on cross-site subrequests, but will be
      sent when a user navigates to the URL from an external site. }
    ssLax
  );

  { Server side HTTP cookie item. }
  TBrookHTTPCookie = class(TCollectionItem)
  private
    FName: string;
    FValue: string;
    FOriginalValue: string;
    FDomain: string;
    FPath: string;
    FExpires: TDateTime;
    FHttpOnly: Boolean;
    FSecure: Boolean;
    FMaxAge: Integer;
    FSameSite: TBrookHTTPCookieSameSite;
    procedure SetMaxAge(AValue: Integer);
    procedure SetName(const AValue: string);
    procedure SetValue(const AValue: string);
    procedure SetPath(const AValue: string);
  protected
    property OriginalValue: string read FOriginalValue;
  public
    { Creates an instance of @code(TBrookHTTPCookie).
      @param(ACollection[in] Cookies list.) }
    constructor Create(ACollection: TCollection); override;
    { Copies the properties of the source cookie.
      @param(ASource[in] Cookie source to be copied.) }
    procedure Assign(ASource: TPersistent); override;
    { Signs a cookie value using
      @html(<a href="https://en.wikipedia.org/wiki/HMAC">HMAC-SHA1</a>).
      @param(ASecret[in] Secret key to sign the cookie value.)
      @param(AUnsignedValue[in] Unsigned cookie value to be signed.)
      @returns(Signed cookie value.) }
    class function Sign(const ASecret,
      AUnsignedValue: string): string; overload; static;
    { Tries to unsign a cookie value.
      @param(ASecret[in] Secret key to unsign the cookie value.)
      @param(ASignedValue[out] Signed cookie value.)
      @param(AUnsignedValue[out] Unsigned cookie value.)
      @returns(@True if cookie value is unsigned successfully.) }
    class function TryUnsign(const ASecret, ASignedValue: string;
      out AUnsignedValue: string): Boolean; overload; static;
    { Unsigns a cookie value.
      @param(ASecret[in] Secret key to unsign the cookie value.)
      @param(ASignedValue[in] Signed cookie value.)
      @returns(Unsigned cookie value.) }
    class function Unsign(const ASecret,
      ASignedValue: string): string; overload; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Checks if a cookie value is signed.
      @param(ASignedValue[out] Signed cookie value.)
      @returns(@True if cookie value is signed.) }
    class function IsSigned(
      const ASignedValue: string): Boolean; overload; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Checks if a cookie is signed.
      @returns(@True if cookie is signed.) }
    function IsSigned: Boolean; overload; virtual;
    { Signs a cookie value using
      @html(<a href="https://en.wikipedia.org/wiki/HMAC">HMAC-SHA1</a>).
      @param(ASecret[in] Secret key to sign the cookie value.) }
    procedure Sign(const ASecret: string); overload; virtual;
    { Tries to unsign a cookie.
      @param(ASecret[in] Secret key to unsign the cookie value.)
      @returns(@True if cookie is unsigned successfully.) }
    function TryUnsign(const ASecret: string): Boolean; overload; virtual;
    { Unsigns a cookie.
      @param(ASecret[in] Secret key to unsign the cookie value.) }
    procedure Unsign(const ASecret: string); overload; virtual;
    { Gets the cookie as string. }
    function ToString: string; override;
    { Clears the cookie properties. }
    procedure Clear; virtual;
    { Expires the cookie. }
    procedure Expire; virtual;
    { Persists a cookie to live as long as it can. }
    procedure Persist; virtual;
    { Cookie name. }
    property Name: string read FName write SetName;
    { Cookie value. }
    property Value: string read FValue write SetValue;
    { Allowed domain to receive the cookie. }
    property Domain: string read FDomain write FDomain;
    { Path that must exist in the URL to receive the cookie. }
    property Path: string read FPath write SetPath;
    { Expiration date/time. }
    property Expires: TDateTime read FExpires write FExpires;
    { @True prevents the cookie to be accessed through JavaScript. }
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    { @True indicates cookie sent only through HTTPS protocol. }
    property Secure: Boolean read FSecure write FSecure;
    { Sets an expiration expressed in number of seconds. }
    property MaxAge: Integer read FMaxAge write SetMaxAge;
    { @True indicates that a cookie shouldn't be sent with cross-site requests. }
    property SameSite: TBrookHTTPCookieSameSite read FSameSite write FSameSite;
  end;

  { Class-reference for @code(TBrookHTTPCookie). }
  TBrookHTTPCookieClass = class of TBrookHTTPCookie;

  { List enumerator for @code(TBrookHTTPCookies). }
  TBrookHTTPCookiesEnumerator = class(TCollectionEnumerator)
  public
    { Get current cookie item. }
    function GetCurrent: TBrookHTTPCookie;
    { Current cookie item. }
    property Current: TBrookHTTPCookie read GetCurrent;
  end;

  { Server side HTTP cookie list. }
  TBrookHTTPCookies = class(TOwnedCollection)
  protected
    function GetItem(AIndex: Integer): TBrookHTTPCookie; virtual;
    procedure SetItem(AIndex: Integer; AValue: TBrookHTTPCookie); virtual;
  public
    { Creates an instance of @code(TBrookHTTPCookies).
      @param(AOwner[in] Cookies persistent.) }
    constructor Create(AOwner: TPersistent); virtual;
    { Gets the default class for cookie item creation. }
    class function GetCookieClass: TBrookHTTPCookieClass; virtual;
    { Copies the items of the source cookies.
      @param(ASource[in] Cookies source to be copied.) }
    procedure Assign(ASource: TPersistent); override;
    { Creates an enumerator to iterate the cookies though @code(for..in). }
    function GetEnumerator: TBrookHTTPCookiesEnumerator;
    { Adds a new cookie to the cookies list. }
    function Add: TBrookHTTPCookie; virtual;
    { Removes a cookie from the cookies list by its name.
      @param(AName[in] Cookie name.) }
    function Remove(const AName: string): Boolean; virtual;
    { Gets the cookie index by its name. }
    function IndexOf(const AName: string): Integer; virtual;
    { Finds a cookie in the cookies list by its name.
      @param(AName[in] Cookie name.) }
    function Find(const AName: string): TBrookHTTPCookie; virtual;
    { Gets the first cookie in the cookies list. }
    function First: TBrookHTTPCookie; virtual;
    { Gets the last cookie in the cookies list. }
    function Last: TBrookHTTPCookie; virtual;
    { Gets/sets a cookie from/to the cookies list by its index. }
    property Items[AIndex: Integer]: TBrookHTTPCookie read GetItem
      write SetItem; default;
  end;

implementation

{ TBrookHTTPCookie }

constructor TBrookHTTPCookie.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FName := BROOK_COOKIE_NAME_PREFIX;
  if Assigned(ACollection) then
    FName := Concat(FName, Succ(ID).ToString);
  FExpires := -1;
  FMaxAge := -1;
  FPath := '/';
end;

class function TBrookHTTPCookie.IsSigned(const ASignedValue: string): Boolean;
begin
  Result := (Length(ASignedValue) > 0) and CompareMem(@ASignedValue[1],
    @BROOK_COOKIE_SIG_PREFIX[1], Length(BROOK_COOKIE_SIG_PREFIX) * SizeOf(Char));
end;

class function TBrookHTTPCookie.Sign(const ASecret,
  AUnsignedValue: string): string;
var
{$IFDEF FPC}
  VEncoder: TBase64EncodingStream;
  VStream: TStringStream;
  VDigest: THMACSHA1Digest;
{$ELSE}
  VEncoder: TBase64Encoding;
{$ENDIF}
  VPos: Integer;
begin
  if IsSigned(AUnsignedValue) then
    Exit(AUnsignedValue);
{$IFDEF FPC}
  VStream := TStringStream.Create('');
  try
    VEncoder := TBase64EncodingStream.Create(VStream);
    try
      VDigest := HMACSHA1Digest(ASecret, AUnsignedValue);
      VEncoder.Write(VDigest[0], Length(VDigest));
    finally
      VEncoder.Destroy;
    end;
    Result := VStream.DataString;
  finally
    VStream.Destroy;
  end
{$ELSE}
  VEncoder := TBase64Encoding.Create(0, '');
  try
    Result := VEncoder.EncodeBytesToString(
      THashSHA1.GetHMACAsBytes(AUnsignedValue, ASecret))
  finally
    VEncoder.Free;
  end;
{$ENDIF};
  VPos := Pos('=', Result);
  if VPos > 0 then
    System.Delete(Result, VPos, MaxInt);
  Result := Concat(BROOK_COOKIE_SIG_PREFIX, AUnsignedValue, '.', Result);
end;

class function TBrookHTTPCookie.TryUnsign(const ASecret, ASignedValue: string;
  out AUnsignedValue: string): Boolean;
var
  VPos: Integer;
begin
  if not IsSigned(ASignedValue) then
    Exit(False);
  AUnsignedValue := ASignedValue;
  System.Delete(AUnsignedValue, 1, Length(BROOK_COOKIE_SIG_PREFIX));
  VPos := Pos('.', AUnsignedValue);
  if VPos > 0 then
  begin
    AUnsignedValue := Copy(AUnsignedValue, 1, Pred(VPos));
    if (Length(AUnsignedValue) > 0) and
      (CompareStr(Brook.Sha1(Sign(ASecret, AUnsignedValue)),
        Brook.Sha1(ASignedValue)) = 0) then
      Exit(True);
  end;
  Result := False;
end;

class function TBrookHTTPCookie.Unsign(const ASecret,
  ASignedValue: string): string;
begin
  if not TryUnsign(ASecret, ASignedValue, Result) then
    Result := EmptyStr;
end;

procedure TBrookHTTPCookie.Assign(ASource: TPersistent);
var
  VSrc: TBrookHTTPCookie;
begin
  if ASource is TBrookHTTPCookie then
  begin
    VSrc := ASource as TBrookHTTPCookie;
    FName := VSrc.Name;
    FValue := VSrc.Value;
    FDomain := VSrc.Domain;
    FPath := VSrc.Path;
    FExpires := VSrc.Expires;
    FHttpOnly := VSrc.HttpOnly;
    FSecure := VSrc.Secure;
    FMaxAge := VSrc.MaxAge;
    FSameSite := VSrc.SameSite;
  end
  else
    inherited Assign(ASource);
end;

function TBrookHTTPCookie.IsSigned: Boolean;
begin
  Result := IsSigned(FValue);
end;

procedure TBrookHTTPCookie.Sign(const ASecret: string);
begin
  FValue := Sign(ASecret, FValue);
end;

function TBrookHTTPCookie.TryUnsign(const ASecret: string): Boolean;
var
  R: string;
begin
  Result := TryUnsign(ASecret, FValue, R);
  if Result then
    FValue := R;
end;

procedure TBrookHTTPCookie.Unsign(const ASecret: string);
begin
  FValue := Unsign(ASecret, FValue);
end;

function TBrookHTTPCookie.ToString: string;
begin
  Result := Concat(FName, '=');
  if IsSigned then
    Result := Concat(Result, BROOK_COOKIE_SIG_PREFIX, FOriginalValue,
      FValue.SubString(BROOK_COOKIE_SIG_PREFIX.Length + FOriginalValue.Length))
  else
    Result := Concat(Result, FValue);
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
    ssNone: ;
  end;
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
  if AValue.IsEmpty then
    raise EBrookHTTPCookie.Create(SBrookEmptyCookieName);
  if not IsValidIdent(AValue) then
    raise EBrookHTTPCookie.CreateFmt(SBrookInvalidCookieName, [AValue]);
  FName := AValue;
end;

procedure TBrookHTTPCookie.SetValue(const AValue: string);
begin
  if AValue = FValue then
    Exit;
  FValue := AValue;
  FOriginalValue := FValue;
end;

procedure TBrookHTTPCookie.SetPath(const AValue: string);
begin
  if AValue <> FPath then
    FPath := Brook.FixPath(AValue);
end;

procedure TBrookHTTPCookie.Clear;
begin
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

{ TBrookHTTPCookiesEnumerator }

function TBrookHTTPCookiesEnumerator.GetCurrent: TBrookHTTPCookie;
begin
  Result := TBrookHTTPCookie(inherited GetCurrent);
end;

{ TBrookHTTPCookies }

constructor TBrookHTTPCookies.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetCookieClass);
end;

class function TBrookHTTPCookies.GetCookieClass: TBrookHTTPCookieClass;
begin
  Result := TBrookHTTPCookie;
end;

function TBrookHTTPCookies.GetEnumerator: TBrookHTTPCookiesEnumerator;
begin
  Result := TBrookHTTPCookiesEnumerator.Create(Self);
end;

procedure TBrookHTTPCookies.Assign(ASource: TPersistent);
var
  C: TBrookHTTPCookie;
begin
  if ASource is TBrookHTTPCookies then
  begin
    Clear;
    for C in (ASource as TBrookHTTPCookies) do
      Add.Assign(C);
  end
  else
    inherited Assign(ASource);
end;

function TBrookHTTPCookies.GetItem(AIndex: Integer): TBrookHTTPCookie;
begin
  Result := TBrookHTTPCookie(inherited GetItem(AIndex));
end;

procedure TBrookHTTPCookies.SetItem(AIndex: Integer;
  AValue: TBrookHTTPCookie);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBrookHTTPCookies.Add: TBrookHTTPCookie;
begin
  Result := TBrookHTTPCookie(inherited Add);
end;

function TBrookHTTPCookies.Remove(const AName: string): Boolean;
var
  I: Integer;
begin
  I := IndexOf(AName);
  Result := I > -1;
  if Result then
    inherited Delete(I);
end;

function TBrookHTTPCookies.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Name, AName) then
      Exit;
  Result := -1;
end;

function TBrookHTTPCookies.Find(const AName: string): TBrookHTTPCookie;
var
  C: TBrookHTTPCookie;
begin
  for C in Self do
    if SameText(C.Name, AName) then
      Exit(C);
  Result := nil;
end;

function TBrookHTTPCookies.First: TBrookHTTPCookie;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(0);
end;

function TBrookHTTPCookies.Last: TBrookHTTPCookie;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(Pred(Count));
end;

end.
