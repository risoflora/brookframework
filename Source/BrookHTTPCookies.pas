(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2019 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook framework.
 *
 * Brook framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook framework.  If not, see <http://www.gnu.org/licenses/>.
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
 {$IFDEF VER3_0_0}
  FPC300Fixes,
 {$ENDIF}
{$ELSE}
  System.Hash,
  System.NetEncoding,
{$ENDIF}
  BrookUtility;

const
{$IFNDEF FPC}
  {$WRITEABLECONST ON}
{$ENDIF}
  BROOK_COOKIE_SIG_PREFIX: string = 's:';
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
  TBrookHTTPCookieSameSite = (ssNone, ssStrict, ssLax);

  { Server side HTTP cookie item. }
  TBrookHTTPCookie = class(TCollectionItem)
  private
    FName: string;
    FValue: string;
    FOldValue: string;
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
    class function Sign(const ASecret,
      AUnsignedValue: string): string; overload; static; inline;
    class function TryUnsign(const ASecret, ASignedValue: string;
      out AUnsignedValue: string): Boolean; overload; static; inline;
    class function Unsign(const ASecret,
      ASignedValue: string): string; overload; static; inline;
    class function IsSigned(
      const ASignedValue: string): Boolean; overload; static; inline;
    function IsSigned: Boolean; overload; virtual;
    procedure Sign(const ASecret: string); overload; virtual;
    function TryUnsign(const ASecret: string): Boolean; overload; virtual;
    procedure Unsign(const ASecret: string); overload; virtual;
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

  { Class-reference for @link(TBrookHTTPCookie). }
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
    constructor Create(AOwner: TPersistent); virtual;
    class function GetCookieClass: TBrookHTTPCookieClass; virtual;
    procedure Assign(ASource: TPersistent); override;
    function GetEnumerator: TBrookHTTPCookiesEnumerator;
    function Add: TBrookHTTPCookie; virtual;
    function Remove(const AName: string): Boolean; virtual;
    function IndexOf(const AName: string): Integer; virtual;
    function Find(const AName: string): TBrookHTTPCookie; virtual;
    function First: TBrookHTTPCookie; virtual;
    function Last: TBrookHTTPCookie; virtual;
    property Items[AIndex: Integer]: TBrookHTTPCookie read GetItem
      write SetItem; default;
  end;

implementation

{ TBrookHTTPCookie }

constructor TBrookHTTPCookie.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FName := 'BrookCookie';
  if Assigned(ACollection) then
    FName := Concat(FName, Succ(ID).ToString);
  FExpires := -1;
  FMaxAge := -1;
  FPath := '/';
end;

class function TBrookHTTPCookie.Sign(const ASecret,
  AUnsignedValue: string): string;
var
{$IFDEF FPC}
  VEncoder: TBase64EncodingStream;
  VStream: TStringStream;
  VDigest: THMACSHA1Digest;
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
      VEncoder.Free;
    end;
    Result := VStream.DataString;
  finally
    VStream.Free;
  end
{$ELSE}
  Result := TNetEncoding.Base64.EncodeBytesToString(
    THashSHA1.GetHMACAsBytes(AUnsignedValue, ASecret))
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

class function TBrookHTTPCookie.IsSigned(const ASignedValue: string): Boolean;
begin
  Result := (Length(ASignedValue) > 0) and CompareMem(@ASignedValue[1],
    @BROOK_COOKIE_SIG_PREFIX[1], Length(BROOK_COOKIE_SIG_PREFIX) * SizeOf(Char));
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
    Result := Concat(Result, BROOK_COOKIE_SIG_PREFIX, Brook.EncodeURL(FOldValue),
      FValue.SubString(BROOK_COOKIE_SIG_PREFIX.Length + FOldValue.Length))
  else
    Result := Concat(Result, Brook.EncodeURL(FValue));
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
