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

{ Contains classes for basic HTTP authentication. }

unit BrookHTTPAuthentication;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  Marshalling,
  libsagui,
  BrookHandledClasses;

resourcestring
  { Error message @code('Invalid status code: <code>.'). }
  SBrookInvalidHTTPStatus = 'Invalid status code: %d.';

type
  { Class which holds the user authentication credentials. }
  TBrookHTTPCredentials = class(TBrookHandledPersistent)
  private
    FUserName: string;
    FPassword: string;
    FHandle: Psg_httpauth;
    function GetRealm: string;
    procedure SetRealm(const AValue: string);
  protected
    function GetHandle: Pointer; override;
  published
    { Creates an instance of @link(TBrookHTTPCredentials).
      @param(AHandle[in] Authentication handle.) }
    constructor Create(AHandle: Pointer); virtual;
    { Authentication protection space (realm). }
    property Realm: string read GetRealm write SetRealm;
    { Name of the authenticated user. }
    property UserName: string read FUserName;
    { Password of the authenticated user. }
    property Password: string read FPassword;
  end;

  { Management class to grant, deny, cancel an authentication. }
  TBrookHTTPAuthentication = class(TBrookHandledPersistent)
  private
    FCredentials: TBrookHTTPCredentials;
    FHandle: Psg_httpauth;
  protected
    class procedure CheckStatus(AStatus: Word); static; inline;
    function GetHandle: Pointer; override;
    function CreateCredentials(
      AHandle: Pointer): TBrookHTTPCredentials; virtual;
  public
    { Creates an instance of @link(TBrookHTTPAuthentication).
      @param(AHandle[in] Authentication handle.) }
    constructor Create(AHandle: Pointer); virtual;
    { Destroys an instance of @link(TBrookHTTPAuthentication). }
    destructor Destroy; override;
    { Deny the authentication sending the reason to the user.
      @param(AReason[in] Denial reason.)
      @param(AContentType[in] Content type.)
      @param(AStatus[in] HTTP status code.) }
    procedure Deny(const AReason, AContentType: string;
      AStatus: Word); overload; virtual;
    { Deny the authentication sending the formatted reason to the user.
      @param(AFmt[in] Formatted string.)
      @param(AArgs[in] Arguments to compose the formatted reason.)
      @param(AContentType[in] Content type.)
      @param(AStatus[in] HTTP status code.) }
    procedure Deny(const AFmt: string; const AArgs: array of const;
      const AContentType: string; AStatus: Word); overload; virtual;
    { Deny the authentication sending the reason to the user.
      @param(AReason[in] Denial reason.)
      @param(AContentType[in] Content type.) }
    procedure Deny(const AReason, AContentType: string); overload; virtual;
    { Deny the authentication sending the formatted reason to the user.
      @param(AFmt[in] Formatted string.)
      @param(AArgs[in] Arguments to compose the formatted reason.)
      @param(AContentType[in] Content type.) }
    procedure Deny(const AFmt: string; const AArgs: array of const;
      const AContentType: string); overload; virtual;
    { Cancels the authentication loop while the user is trying to acess
      the server. }
    procedure Cancel; virtual;
    { Credentials holder. }
    property Credentials: TBrookHTTPCredentials read FCredentials;
  end;

implementation

{ TBrookHTTPCredentials }

constructor TBrookHTTPCredentials.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FUserName := TMarshal.ToString(sg_httpauth_usr(AHandle));
  FPassword := TMarshal.ToString(sg_httpauth_pwd(AHandle));
end;

function TBrookHTTPCredentials.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookHTTPCredentials.GetRealm: string;
begin
  SgLib.Check;
  Result := TMarshal.ToString(sg_httpauth_realm(FHandle));
end;

procedure TBrookHTTPCredentials.SetRealm(const AValue: string);
var
  M: TMarshaller;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpauth_set_realm(FHandle, M.ToCString(AValue)));
end;

{ TBrookHTTPAuthentication }

constructor TBrookHTTPAuthentication.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FCredentials := CreateCredentials(FHandle);
end;

destructor TBrookHTTPAuthentication.Destroy;
begin
  FCredentials.Free;
  inherited Destroy;
end;

function TBrookHTTPAuthentication.CreateCredentials(
  AHandle: Pointer): TBrookHTTPCredentials;
begin
  Result := TBrookHTTPCredentials.Create(AHandle);
end;

class procedure TBrookHTTPAuthentication.CheckStatus(AStatus: Word);
begin
  if (AStatus < 100) or (AStatus > 599) then
    raise EArgumentException.CreateFmt(SBrookInvalidHTTPStatus, [AStatus]);
end;

function TBrookHTTPAuthentication.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPAuthentication.Deny(const AReason, AContentType: string;
  AStatus: Word);
var
  M: TMarshaller;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpauth_deny2(FHandle, M.ToCString(AReason),
    M.ToCString(AContentType), AStatus));
end;

procedure TBrookHTTPAuthentication.Deny(const AFmt: string;
  const AArgs: array of const; const AContentType: string; AStatus: Word);
begin
  Deny(Format(AFmt, AArgs), AContentType, AStatus);
end;

procedure TBrookHTTPAuthentication.Deny(const AReason,
  AContentType: string);
var
  M: TMarshaller;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpauth_deny(FHandle, M.ToCString(AReason),
    M.ToCString(AContentType)));
end;

procedure TBrookHTTPAuthentication.Deny(const AFmt: string;
  const AArgs: array of const; const AContentType: string);
begin
  Deny(Format(AFmt, AArgs), AContentType);
end;

procedure TBrookHTTPAuthentication.Cancel;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpauth_cancel(FHandle));
end;

end.
