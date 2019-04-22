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

unit BrookHTTPRequest;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  Marshalling,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  libsagui,
  BrookUtility,
  BrookHandledClasses,
  BrookString,
  BrookStringMap,
  BrookHTTPUploads;

type
  TBrookHTTPRequest = class(TBrookHandledPersistent)
  private
    FUploads: TBrookHTTPUploads;
    FHeaders: TBrookStringMap;
    FCookies: TBrookStringMap;
    FParams: TBrookStringMap;
    FFields: TBrookStringMap;
    FPayload: TBrookString;
    FVersion: string;
    FMethod: string;
    FPath: string;
    FIsUploading: Boolean;
    FTLSSession: Pointer;
    FHandle: Psg_httpreq;
    function GetIP: string;
    function GetPaths: TArray<string>; inline;
    function GetContentType: string; inline;
    function GetReferer: string; inline;
    function GetUserAgent: string; inline;
  protected
    function CreateUploads(AHandle: Pointer): TBrookHTTPUploads; virtual;
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateCookies(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateParams(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateFields(AHandle: Pointer): TBrookStringMap; virtual;
    function CreatePayload(AHandle: Pointer): TBrookString; virtual;
    function GetHandle: Pointer; override;
    function GetUserData: Pointer; virtual;
    procedure SetUserData(AValue: Pointer); virtual;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    function IsPost: Boolean; inline;
    function IsFavicon: Boolean; inline;
    function IsSecure: Boolean; inline;
    function IsCachable: Boolean; inline;
    function IsXhr: Boolean; inline;
    property Headers: TBrookStringMap read FHeaders;
    property Cookies: TBrookStringMap read FCookies;
    { TODO: signed cookies }
    property Params: TBrookStringMap read FParams;
    property Fields: TBrookStringMap read FFields;
    property Payload: TBrookString read FPayload;
    property Version: string read FVersion;
    property Method: string read FMethod;
    property Path: string read FPath;
    property IP: string read GetIP;
    property ContentType: string read GetContentType;
    property UserAgent: string read GetUserAgent;
    property Referer: string read GetReferer;
    property Paths: TArray<string> read GetPaths;
    property IsUploading: Boolean read FIsUploading;
    property Uploads: TBrookHTTPUploads read FUploads;
    property TLSSession: Pointer read FTLSSession;
    property UserData: Pointer read GetUserData write SetUserData;
  end;

implementation

constructor TBrookHTTPRequest.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FUploads := CreateUploads(sg_httpreq_uploads(AHandle));
  FHeaders := CreateHeaders(sg_httpreq_headers(FHandle));
  FCookies := CreateCookies(sg_httpreq_cookies(FHandle));
  FParams := CreateParams(sg_httpreq_params(FHandle));
  FFields := CreateFields(sg_httpreq_fields(FHandle));
  FPayload := CreatePayload(sg_httpreq_payload(FHandle));
  FVersion := TMarshal.ToString(sg_httpreq_version(FHandle));
  FMethod := TMarshal.ToString(sg_httpreq_method(FHandle));
  FPath := TMarshal.ToString(sg_httpreq_path(FHandle));
  FIsUploading := sg_httpreq_is_uploading(FHandle);
  if Assigned(sg_httpreq_tls_session) then
    FTLSSession := sg_httpreq_tls_session(FHandle);
end;

destructor TBrookHTTPRequest.Destroy;
begin
  FUploads.Free;
  FHeaders.Free;
  FCookies.Free;
  FParams.Free;
  FFields.Free;
  FPayload.Free;
  inherited Destroy;
end;

function TBrookHTTPRequest.CreateUploads(AHandle: Pointer): TBrookHTTPUploads;
begin
  Result := TBrookHTTPUploads.Create(AHandle);
end;

function TBrookHTTPRequest.CreateHeaders(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPRequest.CreateCookies(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPRequest.CreateParams(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPRequest.CreateFields(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPRequest.CreatePayload(AHandle: Pointer): TBrookString;
begin
  Result := TBrookString.Create(AHandle);
end;

function TBrookHTTPRequest.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookHTTPRequest.GetIP: string;
var
  P: array[0..45] of cchar;
  C: Pcvoid;
begin
  SgLib.Check;
  C := sg_httpreq_client(FHandle);
  if not Assigned(C) then
    Exit('');
  SgLib.CheckLastError(sg_ip(C, @P[0], SizeOf(P)));
  Result := TMarshal.ToString(@P[0]);
end;

function TBrookHTTPRequest.GetPaths: TArray<string>;
begin
  Result := Path.Split(['/'], TStringSplitOptions.ExcludeEmpty);
end;

function TBrookHTTPRequest.GetContentType: string;
begin
  Result := FHeaders.Get('Content-Type');
end;

function TBrookHTTPRequest.GetReferer: string;
begin
  if not FHeaders.TryValue('Referer', Result) then
    Result := FHeaders.Get('Referrer');
end;

function TBrookHTTPRequest.GetUserAgent: string;
begin
  Result := FHeaders.Get('User-Agent');
end;

function TBrookHTTPRequest.IsPost: Boolean;
begin
  Result := Sagui.IsPost(FMethod);
end;

function TBrookHTTPRequest.IsFavicon: Boolean;
begin
  Result := SameText(FPath, '/favicon.ico');
end;

function TBrookHTTPRequest.IsSecure: Boolean;
begin
  Result := Assigned(FTLSSession);
end;

function TBrookHTTPRequest.IsCachable: Boolean;
begin
  Result := (FMethod = 'HEAD') or (FMethod = 'GET');
end;

function TBrookHTTPRequest.IsXhr: Boolean;
begin
  Result := SameText(FHeaders.Get('X-Requested-With'), 'xmlhttprequest');
end;

procedure TBrookHTTPRequest.SetUserData(AValue: Pointer);
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpreq_set_user_data(FHandle, AValue));
end;

function TBrookHTTPRequest.GetUserData: Pointer;
begin
  SgLib.Check;
  Result := sg_httpreq_user_data(FHandle);
end;

end.
