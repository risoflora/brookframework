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

unit BrookHTTPAuthentication;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  Marshalling,
  libsagui,
  BrookHandledClasses;

type
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
    constructor Create(AHandle: Pointer); virtual;
    property Realm: string read GetRealm write SetRealm;
    property UserName: string read FUserName;
    property Password: string read FPassword;
  end;

  TBrookHTTPAuthentication = class(TBrookHandledPersistent)
  private
    FCredentials: TBrookHTTPCredentials;
    FHandle: Psg_httpauth;
  protected
    function GetHandle: Pointer; override;
    function CreateCredentials(
      AHandle: Pointer): TBrookHTTPCredentials; virtual;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    procedure Deny(const AJustification, AContentType: string); overload; virtual;
    procedure Deny(const AFmt: string; const AArgs: array of const;
      const AContentType: string); overload; virtual;
    procedure Cancel; virtual;
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

function TBrookHTTPAuthentication.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPAuthentication.Deny(const AJustification,
  AContentType: string);
var
  M: TMarshaller;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpauth_deny(FHandle, M.ToCString(AJustification),
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
