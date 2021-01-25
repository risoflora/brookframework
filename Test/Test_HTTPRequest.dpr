(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2020 Silvio Clecio <silvioprog@gmail.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

program Test_HTTPRequest;

{$I Tests.inc}

uses
  RTLConsts,
  SysUtils,
  StrUtils,
  Marshalling,
  libsagui,
  BrookLibraryLoader,
  BrookUtility,
  BrookString,
  BrookStringMap,
  BrookHTTPRequest,
  Test;

type
  TFakeRequest = class
  end;

var
  FakeRequest: TFakeRequest;
  FakeRequestHandle: Pointer;
  FakeHandle: Pointer;
  FakeUserData: Pointer;

function fake_ip(const socket: Pcvoid; buf: Pcchar; size: csize_t): cint; cdecl;
begin
  Move(socket^, buf^, size);
  Result := 0;
end;

function fake_httpreq_headers(req: Psg_httpreq): PPsg_strmap; cdecl;
begin
  Assert(req = FakeRequestHandle);
  FakeHandle := nil;
  Result := @FakeHandle;
end;

function fake_httpreq_version(req: Psg_httpreq): Pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(req = FakeRequestHandle);
  Result := M.ToCString('fake_version');
end;

function fake_httpreq_method(req: Psg_httpreq): Pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(req = FakeRequestHandle);
  Result := M.ToCString('fake_method');
end;

function fake_httpreq_path(req: Psg_httpreq): Pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(req = FakeRequestHandle);
  Result := M.ToCString('fake_path');
end;

function fake_httpreq_is_uploading(req: Psg_httpreq): cbool; cdecl;
begin
  Assert(req = FakeRequestHandle);
  Result := True;
end;

function fakeg_httpreq_tls_session(req: Psg_httpreq): Pcvoid; cdecl;
begin
  Assert(req = FakeRequestHandle);
  Result := Pointer(1);
end;

function fake_httpreq_client(req: Psg_httpreq): Pcvoid; cdecl;
var
  M: TMarshaller;
begin
  Assert(req = FakeRequestHandle);
  Result := M.ToCString('123.123.123.123');
end;

procedure AssignFakeAPI; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  sg_ip := fake_ip;

  sg_httpreq_version := fake_httpreq_version;
  sg_httpreq_method := fake_httpreq_method;
  sg_httpreq_path := fake_httpreq_path;
  sg_httpreq_is_uploading := fake_httpreq_is_uploading;
  sg_httpreq_tls_session := fakeg_httpreq_tls_session;
  sg_httpreq_client := fake_httpreq_client;
end;

procedure AssignFakeHeadersAPI; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  sg_httpreq_headers := fake_httpreq_headers;
  sg_httpreq_cookies := fake_httpreq_headers;
  sg_httpreq_params := fake_httpreq_headers;
end;

procedure DoHTTPRequestCreateParamIsNil;
begin
  TBrookHTTPRequest.Create(nil);
end;

procedure Test_HTTPRequestCreate;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeAPI;
  AssignFakeHeadersAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.Handle = FakeRequestHandle);
    Assert(Assigned(R.Uploads));
    Assert(Assigned(R.Headers));
    Assert(Assigned(R.Cookies));
    Assert(Assigned(R.Params));
    Assert(Assigned(R.Fields));
    Assert(Assigned(R.Payload));
  finally
    R.Free;
  end;
  AssignFakeAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.Version = 'fake_version');
    Assert(R.Method = 'fake_method');
    Assert(R.Path = 'fake_path');
    Assert(R.IsUploading);
    Assert(Assigned(R.TLSSession));
  finally
    R.Free;
  end;

  TBrookLibraryLoader.Unload;
  TBrookLibraryLoader.Load;
  AssertExcept(DoHTTPRequestCreateParamIsNil, EArgumentNilException,
    Format(SParamIsNil, ['AHandle']));
end;

function fake_httpreq_method_get(req: Psg_httpreq): Pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(req = FakeRequestHandle);
  Result := M.ToCString('GET');
end;

function fake_httpreq_method_post(req: Psg_httpreq): Pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(req = FakeRequestHandle);
  Result := M.ToCString('POST');
end;

procedure Test_HTTPRequestIsPost;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeAPI;
  AssignFakeHeadersAPI;
  sg_httpreq_method := fake_httpreq_method_get;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(not R.IsPost);
  finally
    R.Free;
  end;
  sg_httpreq_method := fake_httpreq_method_post;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.IsPost);
  finally
    R.Free;
  end;
end;

function fake_httpreq_path_nofavicon(req: Psg_httpreq): Pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(req = FakeRequestHandle);
  Result := M.ToCString('/foo');
end;

function fake_httpreq_path_favicon(req: Psg_httpreq): Pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(req = FakeRequestHandle);
  Result := M.ToCString('/favicon.ico');
end;

procedure Test_HTTPRequestIsFavicon;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  sg_httpreq_path := fake_httpreq_path_nofavicon;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(not R.IsFavicon);
  finally
    R.Free;
  end;
  sg_httpreq_path := fake_httpreq_path_favicon;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.IsFavicon);
  finally
    R.Free;
  end;
end;

function fake_httpreq_tls_session_nil(req: Psg_httpreq): Pcvoid; cdecl;
begin
  Assert(req = FakeRequestHandle);
  Result := nil;
end;

function fake_httpreq_tls_session_ok(req: Psg_httpreq): Pcvoid; cdecl;
begin
  Assert(req = FakeRequestHandle);
  Result := Pointer(1);
end;

procedure Test_HTTPRequestIsSecure;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  sg_httpreq_tls_session := fake_httpreq_tls_session_nil;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(not R.IsSecure);
  finally
    R.Free;
  end;
  sg_httpreq_tls_session := fake_httpreq_tls_session_ok;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.IsSecure);
  finally
    R.Free;
  end;
end;

function fake_httpreq_method_head(req: Psg_httpreq): Pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(req = FakeRequestHandle);
  Result := M.ToCString('HEAD');
end;

procedure Test_HTTPRequestIsCachable;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  sg_httpreq_method := fake_httpreq_method_head;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.IsCachable);
  finally
    R.Free;
  end;
  sg_httpreq_method := fake_httpreq_method_get;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.IsCachable);
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestIsXhr;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    R.Headers.Clear;
    Assert(not R.IsXhr);
  finally
    R.Free;
  end;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    R.Headers.Add('X-Requested-With', 'xmlhttprequest');
    Assert(R.IsXhr);
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestHeaders;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(Assigned(R.Headers));
    R.Headers.Add('foo', 'bar');
    Assert(R.Headers['foo'] = 'bar');
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestCookies;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  sg_httpreq_cookies := fake_httpreq_headers;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(Assigned(R.Cookies));
    R.Cookies.Add('foo', 'bar');
    Assert(R.Cookies['foo'] = 'bar');
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestParams;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  sg_httpreq_params := fake_httpreq_headers;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(Assigned(R.Params));
    R.Params.Add('foo', 'bar');
    Assert(R.Params['foo'] = 'bar');
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestFields;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  sg_httpreq_fields := fake_httpreq_headers;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(Assigned(R.Fields));
    R.Fields.Add('foo', 'bar');
    Assert(R.Fields['foo'] = 'bar');
  finally
    R.Free;
  end;
end;

function fake_httpreq_payload(req: Psg_httpreq): Psg_str; cdecl;
begin
  Assert(req = FakeRequestHandle);
  FakeHandle := nil;
  Result := FakeHandle;
end;

procedure Test_HTTPRequestPayload;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  sg_httpreq_payload := fake_httpreq_payload;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(Assigned(R.Payload));
    R.Payload.Write('foo');
    Assert(R.Payload.Text = 'foo');
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestVersion;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.Version = 'fake_version');
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestMethod;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  AssignFakeAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.Method = 'fake_method');
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestPath;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.Path = 'fake_path');
  finally
    R.Free;
  end;
end;

function fake_httpreq_client_nil(req: Psg_httpreq): Pcvoid; cdecl;
begin
  Assert(req = FakeRequestHandle);
  Result := nil;
end;

procedure DoHTTPRequestIPLibNotLoaded(const AArgs: array of const);
begin
  Assert(TBrookHTTPRequest(AArgs[0].VObject).IP = '123.123.123.123');
end;

procedure Test_HTTPRequestIP;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  sg_httpreq_client := fake_httpreq_client_nil;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.IP.IsEmpty);
  finally
    R.Free;
  end;
  AssignFakeAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.IP = '123.123.123.123');
  finally
    R.Free;
  end;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    TBrookLibraryLoader.Unload;
    AssertExcept(DoHTTPRequestIPLibNotLoaded, ESgLibNotLoaded,
      Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
        SG_LIB_NAME, SgLib.GetLastName)]), [R]);
    TBrookLibraryLoader.Load;
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestContentType;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeAPI;
  AssignFakeHeadersAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.ContentType.IsEmpty);
    R.Headers.Add('Content-Type', 'html/text');
    Assert(R.ContentType = 'html/text');
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestUserAgent;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.UserAgent.IsEmpty);
    R.Headers.Add('User-Agent', 'Brook');
    Assert(R.UserAgent = 'Brook');
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestReferer;
var
  R: TBrookHTTPRequest;
begin
  AssignFakeHeadersAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.Referer.IsEmpty);
    R.Headers.Add('Referer', '/foo');
    Assert(R.Referer = '/foo');
    R.Headers.Clear;
    R.Headers.Add('Referrer', '/bar');
    Assert(R.Referer = '/bar');
  finally
    R.Free;
  end;
end;

function fake_httpreq_paths(req: Psg_httpreq): Pcchar; cdecl;
begin
  Assert(req = FakeRequestHandle);
  Result := '/foo/bar';
end;

procedure Test_HTTPRequestPaths;
var
  R: TBrookHTTPRequest;
begin
  TBrookLibraryLoader.Unload;
  TBrookLibraryLoader.Load;
  AssignFakeAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(Length(R.Paths) = 1);
  finally
    R.Free;
  end;
  sg_httpreq_path := fake_httpreq_paths;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(Length(R.Paths) = 2);
  finally
    R.Free;
  end;
end;

procedure Test_HTTPRequestIsUploading;
var
  R: TBrookHTTPRequest;
begin
  TBrookLibraryLoader.Unload;
  TBrookLibraryLoader.Load;
  AssignFakeAPI;
  AssignFakeHeadersAPI;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.IsUploading);
  finally
    R.Free;
  end;
end;

function fake_httpreq_uploads(req: Psg_httpreq): Psg_httpupld; cdecl;
begin
  Assert(req = FakeRequestHandle);
  Result := FakeHandle;
end;

procedure Test_HTTPRequestUploads;
var
  R: TBrookHTTPRequest;
begin
  TBrookLibraryLoader.Unload;
  TBrookLibraryLoader.Load;
  AssignFakeAPI;
  sg_httpreq_uploads := fake_httpreq_uploads;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.Uploads.Handle = FakeHandle);
  finally
    R.Free;
  end;
end;

function fake_httpreq_tls_session(req: Psg_httpreq): Pcvoid; cdecl;
begin
  Assert(req = FakeRequestHandle);
  Result := FakeHandle;
end;

procedure Test_HTTPRequestTLSSession;
var
  R: TBrookHTTPRequest;
begin
  TBrookLibraryLoader.Unload;
  TBrookLibraryLoader.Load;
  AssignFakeAPI;
  sg_httpreq_tls_session := fake_httpreq_tls_session;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    Assert(R.TLSSession = FakeHandle);
  finally
    R.Free;
  end;
end;

function fake_httpreq_set_user_data(req: Psg_httpreq; data: Pcvoid): cint;
  cdecl;
begin
  Assert(req = FakeRequestHandle);
  FakeUserData := data;
  Result := 0;
end;

function fake_httpreq_user_data(req: Psg_httpreq): Pcvoid; cdecl;
begin
  Assert(req = FakeRequestHandle);
  Result := FakeUserData;
end;

procedure Test_HTTPRequestUserData;
var
  R: TBrookHTTPRequest;
  X: Integer;
begin
  TBrookLibraryLoader.Unload;
  TBrookLibraryLoader.Load;
  AssignFakeAPI;
  sg_httpreq_set_user_data := fake_httpreq_set_user_data;
  sg_httpreq_user_data := fake_httpreq_user_data;
  R := TBrookHTTPRequest.Create(FakeRequestHandle);
  try
    R.UserData := nil;
    Assert(not Assigned(R.UserData));
    X := 123;
    R.UserData := @X;
    Assert(PInteger(R.UserData)^ = 123);
  finally
    R.Free;
  end;
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  FakeRequest := TFakeRequest.Create;
  try
    FakeRequestHandle := FakeRequest;
    Test_HTTPRequestCreate;
    // Test_HTTPRequestDestroy - not required
    Test_HTTPRequestIsPost;
    Test_HTTPRequestIsFavicon;
    Test_HTTPRequestIsSecure;
    Test_HTTPRequestIsCachable;
    Test_HTTPRequestIsXhr;
    Test_HTTPRequestHeaders;
    Test_HTTPRequestCookies;
    Test_HTTPRequestParams;
    Test_HTTPRequestFields;
    Test_HTTPRequestPayload;
    Test_HTTPRequestVersion;
    Test_HTTPRequestMethod;
    Test_HTTPRequestPath;
    Test_HTTPRequestIP;
    Test_HTTPRequestContentType;
    Test_HTTPRequestUserAgent;
    Test_HTTPRequestReferer;
    Test_HTTPRequestPaths;
    Test_HTTPRequestIsUploading;
    Test_HTTPRequestUploads;
    Test_HTTPRequestTLSSession;
    Test_HTTPRequestUserData;
  finally
    TBrookLibraryLoader.Unload;
    FakeRequest.Free;
  end;
end.
