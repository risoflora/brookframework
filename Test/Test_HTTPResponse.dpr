(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

program Test_HTTPResponse;

{$I Tests.inc}

uses
  SysConst,
  SysUtils,
  StrUtils,
  Classes,
  Platform,
  Marshalling,
  libsagui,
  BrookLibraryLoader,
  BrookHTTPResponse,
  Test;

type
  TFakeResponse = class
  public
    ResponseAlreadySent: Boolean;
    WasCompressed: Boolean;
    LastStatus: Word;
    ErrorCode: Integer;
    Empty: Boolean;
  end;

  TFakeStringStream = class(TStringStream)
  public
    Freed: Boolean;
    destructor Destroy; override;
  end;

  destructor TFakeStringStream.Destroy;
  begin
    inherited Destroy;
    Freed := True;
  end;

var
  FakeResponse: TFakeResponse;
  FakeResponseHandle: Pointer;
  FakeHandle: Pointer;

function fake_httpres_headers(res: Psg_httpres): PPsg_strmap; cdecl;
begin
  Assert(res = FakeResponseHandle);
  FakeHandle := nil;
  Result := @FakeHandle;
end;

function fake_httpres_set_cookie(res: Psg_httpres; const name: Pcchar;
  const val: Pcchar): cint; cdecl;
var
  N: string;
begin
  Assert(res = FakeResponseHandle);
  N := TMarshal.ToString(name);
  if N.IsEmpty then
    Exit(EINVAL);
  Assert(N = 'fake_name');
  Assert(TMarshal.ToString(val) = 'fake_val');
  Result := 0;
end;

function fake_httpres_zsendbinary(res: Psg_httpres; buf: Pcvoid; size: csize_t;
  const content_type: Pcchar; status: cuint): cint; cdecl;
var
  S: string;
begin
  Assert(res = FakeResponseHandle);
  if FakeResponse.ResponseAlreadySent then
    Exit(EALREADY);
  S := TMarshal.ToString(buf);
  SetLength(S, size);
  Assert(S = 'fake_val');
  Assert(size = csize_t(Length(S)));
  Assert(TMarshal.ToString(content_type) = 'fake_content_type');
  Assert(status = 200);
  FakeResponse.WasCompressed := True;
  FakeResponse.ResponseAlreadySent := True;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_sendbinary(res: Psg_httpres; buf: Pcvoid; size: csize_t;
  const content_type: Pcchar; status: cuint): cint; cdecl;
var
  S: string;
begin
  Assert(res = FakeResponseHandle);
  if FakeResponse.ResponseAlreadySent then
    Exit(EALREADY);
  S := TMarshal.ToString(buf);
  SetLength(S, size);
  Assert(S = 'fake_val');
  Assert(size = csize_t(Length(S)));
  Assert(TMarshal.ToString(content_type) = 'fake_content_type');
  Assert((status = 200) or (status = 302) or (status = 307));
  FakeResponse.WasCompressed := False;
  FakeResponse.ResponseAlreadySent := True;
  FakeResponse.LastStatus := status;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_zsendfile(res: Psg_httpres; size: cuint64_t;
  max_size: cuint64_t; offset: cuint64_t; const filename: Pcchar;
  downloaded: cbool; status: cuint): cint; cdecl;
begin
  Assert(res = FakeResponseHandle);
  if FakeResponse.ResponseAlreadySent then
    Exit(EALREADY);
  Assert(size = 123);
  Assert(max_size = 456);
  Assert(offset = 789);
  Assert(TMarshal.ToString(filename) = 'fake_filename');
  Assert(downloaded);
  Assert(status = 200);
  FakeResponse.WasCompressed := True;
  FakeResponse.ResponseAlreadySent := True;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_sendfile(res: Psg_httpres; size: cuint64_t;
  max_size: cuint64_t; offset: cuint64_t; const filename: Pcchar;
  downloaded: cbool; status: cuint): cint; cdecl;
begin
  Assert(res = FakeResponseHandle);
  if FakeResponse.ResponseAlreadySent then
    Exit(EALREADY);
  Assert(size = 123);
  Assert(max_size = 456);
  Assert(offset = 789);
  Assert(TMarshal.ToString(filename) = 'fake_filename');
  Assert(downloaded);
  Assert(status = 200);
  FakeResponse.WasCompressed := False;
  FakeResponse.ResponseAlreadySent := True;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_zsendstream(res: Psg_httpres; read_cb: sg_read_cb;
  handle: Pcvoid; free_cb: sg_free_cb; status: cuint): cint; cdecl;
var
  S: TFakeStringStream;
  B: TBytes{$IFDEF FPC}= nil{$ENDIF};
  Z: csize_t;
begin
  Assert(res = FakeResponseHandle);
  if FakeResponse.ResponseAlreadySent then
  begin
    if Assigned(free_cb) then
      free_cb(handle);
    Exit(EALREADY);
  end;
  Assert(Assigned(read_cb));
  SetLength(B, 256);
  Z := read_cb(handle, 0, @B[0], Length(B));
  SetLength(B, Z);
  Assert(StringOf(B) = 'fake_val');
  S := TFakeStringStream(handle);
  Assert(not S.Freed);
  Assert(Assigned(free_cb));
  free_cb(handle);
  Assert(S.Freed);
  Assert(status = 200);
  FakeResponse.WasCompressed := True;
  FakeResponse.ResponseAlreadySent := True;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_sendstream(res: Psg_httpres; size: cuint64_t;
  read_cb: sg_read_cb; handle: Pcvoid; free_cb: sg_free_cb;
  status: cuint): cint; cdecl;
var
  S: TFakeStringStream;
  B: TBytes{$IFDEF FPC}= nil{$ENDIF};
  Z: csize_t;
begin
  Assert(res = FakeResponseHandle);
  if FakeResponse.ResponseAlreadySent then
  begin
    if Assigned(free_cb) then
      free_cb(handle);
    Exit(EALREADY);
  end;
  Assert(size = 0);
  Assert(Assigned(read_cb));
  SetLength(B, 256);
  Z := read_cb(handle, 0, @B[0], Length(B));
  SetLength(B, Z);
  Assert(StringOf(B) = 'fake_val');
  S := TFakeStringStream(handle);
  Assert(not S.Freed);
  Assert(Assigned(free_cb));
  free_cb(handle);
  Assert(S.Freed);
  Assert(status = 200);
  FakeResponse.WasCompressed := False;
  FakeResponse.ResponseAlreadySent := True;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_reset(res: Psg_httpres): cint; cdecl;
begin
  Assert(res = FakeResponseHandle);
  FakeResponse.ResponseAlreadySent := False;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_clear(res: Psg_httpres): cint; cdecl;
begin
  Assert(res = FakeResponseHandle);
  FakeResponse.ResponseAlreadySent := False;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_zsendfile2(res: Psg_httpres; level: cint; size: cuint64_t;
  max_size: cuint64_t; offset: cuint64_t; const filename: Pcchar;
  const disposition: Pcchar; status: cuint): cint; cdecl;
var
  D: string;
begin
  Assert(res = FakeResponseHandle);
  if FakeResponse.ResponseAlreadySent then
    Exit(EALREADY);
  Assert(level = 1);
  Assert(size = 0);
  Assert(max_size = 0);
  Assert(offset = 0);
  Assert(TMarshal.ToString(filename) = 'fake_filename');
  D := TMarshal.ToString(disposition);
  Assert((D = 'attachment') or (D = 'inline'));
  Assert(status = 200);
  FakeResponse.WasCompressed := True;
  FakeResponse.ResponseAlreadySent := True;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_sendfile2(res: Psg_httpres; size: cuint64_t;
  max_size: cuint64_t; offset: cuint64_t; const filename: Pcchar;
  const disposition: Pcchar; status: cuint): cint; cdecl;
var
  D: string;
begin
  Assert(res = FakeResponseHandle);
  if FakeResponse.ResponseAlreadySent then
    Exit(EALREADY);
  Assert(size = 0);
  Assert(max_size = 0);
  Assert(offset = 0);
  Assert(TMarshal.ToString(filename) = 'fake_filename');
  D := TMarshal.ToString(disposition);
  Assert((D = 'attachment') or (D = 'inline'));
  Assert(status = 200);
  FakeResponse.WasCompressed := False;
  FakeResponse.ResponseAlreadySent := True;
  Result := FakeResponse.ErrorCode;
end;

function fake_httpres_is_empty(res: Psg_httpres): cbool; cdecl;
begin
  Assert(res = FakeResponseHandle);
  Result := FakeResponse.Empty;
end;

procedure AssignFakeAPI; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  sg_httpres_headers := fake_httpres_headers;
  sg_httpres_set_cookie := fake_httpres_set_cookie;
  sg_httpres_zsendbinary := fake_httpres_zsendbinary;
  sg_httpres_sendbinary := fake_httpres_sendbinary;
  sg_httpres_zsendfile := fake_httpres_zsendfile;
  sg_httpres_sendfile := fake_httpres_sendfile;
  sg_httpres_zsendstream := fake_httpres_zsendstream;
  sg_httpres_sendstream := fake_httpres_sendstream;
  sg_httpres_reset := fake_httpres_reset;
  sg_httpres_clear := fake_httpres_clear;
  sg_httpres_zsendfile2 := fake_httpres_zsendfile2;
  sg_httpres_sendfile2 := fake_httpres_sendfile2;
  sg_httpres_is_empty := fake_httpres_is_empty;
end;

procedure Test_HTTPResponseCreate;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    Assert(R.Handle = FakeResponseHandle);
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseSetCookieLibNotLoaded(const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).SetCookie('fake_name', 'fake_val');
end;

procedure DoHTTPResponseSetCookieInvalidArgument(const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).SetCookie('', '');
end;

procedure Test_HTTPResponseSetCookie;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    FakeResponse.ErrorCode := 0;
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseSetCookieLibNotLoaded, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
          SG_LIB_NAME, SgLib.GetLastName)]), [R]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    AssertOSExcept(DoHTTPResponseSetCookieInvalidArgument, EINVAL, [R]);
    R.SetCookie('fake_name', 'fake_val');
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseSendError(const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).Send('fake_val', 'fake_content_type', 200);
end;

procedure Test_HTTPResponseSend;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    R.Compressed := True;
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := 0;
    R.Send('fake_val', 'fake_content_type', 200);
    Assert(FakeResponse.WasCompressed);
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseSendError, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
          SG_LIB_NAME, SgLib.GetLastName)]), [R]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeResponse.ErrorCode := -1;
    FakeResponse.ResponseAlreadySent := False;
    AssertExcept(DoHTTPResponseSendError, EBrookHTTPResponse,
      SBrookZLibError, [R]);

    R.Compressed := False;
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.Send('fake_val', 'fake_content_type', 200);
    Assert(not FakeResponse.WasCompressed);
    AssertExcept(DoHTTPResponseSendError, EBrookHTTPResponse,
      SBrookResponseAlreadySent, [R]);
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPResponseSendError, EINVAL, [R]);
  finally
    R.Free;
  end;
end;

procedure Test_HTTPResponseSendFmt;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.SendFmt('%s_%s', ['fake', 'val'], 'fake_content_type', 200);
    Assert(FakeResponse.ResponseAlreadySent);
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseSendBinaryError(const AArgs: array of const);
var
  B: TBytes;
begin
  B := BytesOf('fake_val');
  TBrookHTTPResponse(AArgs[0].VObject).SendBinary(@B[0], Length(B),
    'fake_content_type', 200);
end;

procedure Test_HTTPResponseSendBinary;
var
  R: TBrookHTTPResponse;
  B: TBytes;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    R.Compressed := True;
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := 0;
    B := BytesOf('fake_val');
    R.SendBinary(@B[0], Length(B), 'fake_content_type', 200);
    Assert(FakeResponse.WasCompressed);
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseSendBinaryError, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
          SG_LIB_NAME, SgLib.GetLastName)]), [R]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeResponse.ErrorCode := -1;
    FakeResponse.ResponseAlreadySent := False;
    AssertExcept(DoHTTPResponseSendBinaryError, EBrookHTTPResponse,
      SBrookZLibError, [R]);

    R.Compressed := False;
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.SendBinary(@B[0], Length(B), 'fake_content_type', 200);
    Assert(not FakeResponse.WasCompressed);
    AssertExcept(DoHTTPResponseSendBinaryError, EBrookHTTPResponse,
      SBrookResponseAlreadySent, [R]);
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPResponseSendBinaryError, EINVAL, [R]);
  finally
    R.Free;
  end;
end;

procedure Test_HTTPResponseSendBytes;
var
  R: TBrookHTTPResponse;
  B: TBytes;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    B := BytesOf('fake_val');
    R.SendBytes(B, Length(B), 'fake_content_type', 200);
    Assert(FakeResponse.ResponseAlreadySent);
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseSendFileError(const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).SendFile(123, 456, 789, 'fake_filename',
    True, 200);
end;

procedure Test_HTTPResponseSendFile;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    R.Compressed := True;
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := 0;
    R.SendFile(123, 456, 789, 'fake_filename', True, 200);
    Assert(FakeResponse.WasCompressed);
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseSendFileError, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
          SG_LIB_NAME, SgLib.GetLastName)]), [R]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeResponse.ErrorCode := -1;
    FakeResponse.ResponseAlreadySent := False;
    AssertExcept(DoHTTPResponseSendFileError, EBrookHTTPResponse,
      SBrookZLibError, [R]);

    R.Compressed := False;
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.SendFile(123, 456, 789, 'fake_filename', True, 200);
    Assert(not FakeResponse.WasCompressed);
    AssertExcept(DoHTTPResponseSendFileError, EBrookHTTPResponse,
      SBrookResponseAlreadySent, [R]);
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := ENOENT;
    AssertExcept(DoHTTPResponseSendFileError, EFileNotFoundException,
      SFileNotFound, [R]);
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPResponseSendFileError, EINVAL, [R]);
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseSendStreamError(const AArgs: array of const);
var
  S: TFakeStringStream;
begin
  S := TFakeStringStream.Create('fake_val');
  try
    TBrookHTTPResponse(AArgs[0].VObject).SendStream(S, True, 200);
  finally
    Assert(S.Freed);
  end;
end;

procedure Test_HTTPResponseSendStream;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    R.Compressed := True;
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := 0;
    R.SendStream(TFakeStringStream.Create('fake_val'), True, 200);
    Assert(FakeResponse.WasCompressed);
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseSendStreamError, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
          SG_LIB_NAME, SgLib.GetLastName)]), [R]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeResponse.ErrorCode := -1;
    FakeResponse.ResponseAlreadySent := False;
    AssertExcept(DoHTTPResponseSendStreamError, EBrookHTTPResponse,
      SBrookZLibError, [R]);

    R.Compressed := False;
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.SendStream(TFakeStringStream.Create('fake_val'), True, 200);
    Assert(not FakeResponse.WasCompressed);
    AssertExcept(DoHTTPResponseSendStreamError, EBrookHTTPResponse,
      SBrookResponseAlreadySent, [R]);
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPResponseSendStreamError, EINVAL, [R]);

    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.SendStream(TFakeStringStream.Create('fake_val'), 200);
  finally
    R.Free;
  end;
end;

function fake_httpres_sendbinary_empty(res: Psg_httpres; buf: Pcvoid;
  size: csize_t; const content_type: Pcchar; status: cuint): cint; cdecl;
var
  C: String;
begin
  Assert(res = FakeResponseHandle);
  Assert(Length(Pcchar(buf)) = 0);
  Assert(size = 0);
  C := TMarshal.ToString(content_type);
  if not C.IsEmpty then
    Assert(C = 'fake_content_type');
  Assert(status = 204);
  Result := FakeResponse.ErrorCode;
end;

procedure Test_HTTPResponseSendEmpty;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    FakeResponse.ErrorCode := 0;
    sg_httpres_sendbinary := fake_httpres_sendbinary_empty;
    R.SendEmpty('fake_content_type');
    R.SendEmpty;
    TBrookLibraryLoader.Unload;
    TBrookLibraryLoader.Load;
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseSendAndRedirectInvalidHTTPStatus(
  const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).SendAndRedirect('fake_val',
    'fake_destination', 'fake_content_type', AArgs[1].VInteger);
end;

procedure Test_HTTPResponseSendAndRedirect;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    AssertExcept(DoHTTPResponseSendAndRedirectInvalidHTTPStatus,
      EBrookHTTPResponse, Format(SBrookInvalidHTTPStatus, [50]), [R, 50]);
    AssertExcept(DoHTTPResponseSendAndRedirectInvalidHTTPStatus,
      EBrookHTTPResponse, Format(SBrookInvalidHTTPStatus, [1000]), [R, 1000]);
    Assert(R.Headers['Location'].IsEmpty);
    R.SendAndRedirect('fake_val', 'fake_destination', 'fake_content_type', 302);
    Assert(FakeResponse.LastStatus = 302);
    Assert(R.Headers['Location'] = 'fake_destination');
    R.Clear;
    R.Headers['Location'] := 'foo';
    R.SendAndRedirect('fake_val', 'fake_destination2', 'fake_content_type', 307);
    Assert(FakeResponse.LastStatus = 307);
    Assert(R.Headers['Location'] = 'fake_destination2');

    R.Clear;
    R.SendAndRedirect('fake_val', 'fake_destination2', 'fake_content_type');
    Assert(FakeResponse.LastStatus = 302);
    Assert(R.Headers['Location'] = 'fake_destination2');
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseDownloadError(const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).Download('fake_filename');
end;

procedure Test_HTTPResponseDownload;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    R.Compressed := True;
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := 0;
    R.Download('fake_filename');
    Assert(FakeResponse.WasCompressed);
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseDownloadError, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
          SG_LIB_NAME, SgLib.GetLastName)]), [R]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeResponse.ErrorCode := -1;
    FakeResponse.ResponseAlreadySent := False;
    AssertExcept(DoHTTPResponseDownloadError, EBrookHTTPResponse,
      SBrookZLibError, [R]);

    R.Compressed := False;
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.Download('fake_filename');
    Assert(not FakeResponse.WasCompressed);
    AssertExcept(DoHTTPResponseDownloadError, EBrookHTTPResponse,
      SBrookResponseAlreadySent, [R]);
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := ENOENT;
    AssertExcept(DoHTTPResponseDownloadError, EFileNotFoundException,
      SFileNotFound, [R]);
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPResponseDownloadError, EINVAL, [R]);
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseRenderError(const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).Render('fake_filename');
end;

procedure Test_HTTPResponseRender;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    R.Compressed := True;
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := 0;
    R.Render('fake_filename');
    Assert(FakeResponse.WasCompressed);
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseRenderError, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
          SG_LIB_NAME, SgLib.GetLastName)]), [R]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeResponse.ErrorCode := -1;
    FakeResponse.ResponseAlreadySent := False;
    AssertExcept(DoHTTPResponseRenderError, EBrookHTTPResponse,
      SBrookZLibError, [R]);

    R.Compressed := False;
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.Render('fake_filename');
    Assert(not FakeResponse.WasCompressed);
    AssertExcept(DoHTTPResponseRenderError, EBrookHTTPResponse,
      SBrookResponseAlreadySent, [R]);
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := ENOENT;
    AssertExcept(DoHTTPResponseRenderError, EFileNotFoundException,
      SFileNotFound, [R]);
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPResponseRenderError, EINVAL, [R]);
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseResetError(const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).Reset;
end;

procedure Test_HTTPResponseReset;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.Send('fake_val', 'fake_content_type', 200);
    Assert(FakeResponse.ResponseAlreadySent);
    R.Reset;
    Assert(not FakeResponse.ResponseAlreadySent);
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseResetError, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '', SG_LIB_NAME,
          SgLib.GetLastName)]), [R]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPResponseResetError, EINVAL, [R]);
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseClearError(const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).Clear;
end;

procedure Test_HTTPResponseClear;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    FakeResponse.ErrorCode := 0;
    FakeResponse.ResponseAlreadySent := False;
    R.Send('fake_val', 'fake_content_type', 200);
    Assert(FakeResponse.ResponseAlreadySent);
    R.Clear;
    Assert(not FakeResponse.ResponseAlreadySent);
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseClearError, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '', SG_LIB_NAME,
          SgLib.GetLastName)]), [R]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeResponse.ResponseAlreadySent := False;
    FakeResponse.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPResponseClearError, EINVAL, [R]);
  finally
    R.Free;
  end;
end;

procedure DoHTTPResponseIsEmptyNotLoaded(const AArgs: array of const);
begin
  TBrookHTTPResponse(AArgs[0].VObject).IsEmpty;
end;

procedure Test_HTTPResponseIsEmpty;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    FakeResponse.Empty := False;
    Assert(not R.IsEmpty);
    FakeResponse.Empty := True;
    Assert(R.IsEmpty);
    FakeResponse.ErrorCode := 0;
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPResponseIsEmptyNotLoaded, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
          SG_LIB_NAME, SgLib.GetLastName)]), [R]);
      AssignFakeAPI;
    finally
      TBrookLibraryLoader.Load;
    end;
  finally
    R.Free;
  end;
end;

procedure Test_HTTPResponseCompressed;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    Assert(not R.Compressed);
    R.Compressed := not R.Compressed;
    Assert(R.Compressed);
  finally
    R.Free;
  end;
end;

procedure Test_HTTPResponseHeaders;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    Assert(R.Headers.Count = 0);
    R.Headers.Add('foo', 'bar');
    R.Headers.Add('bar', 'foo');
    Assert(R.Headers.Count = 2);
  finally
    R.Free;
  end;
end;

procedure Test_HTTPResponseCookies;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    Assert(R.Cookies.Count = 0);
    R.Cookies.Add;
    R.Cookies.Add;
    Assert(R.Cookies.Count = 2);
  finally
    R.Free;
  end;
end;

procedure Test_HTTPResponseEmpty;
var
  R: TBrookHTTPResponse;
begin
  AssignFakeAPI;
  R := TBrookHTTPResponse.Create(FakeResponseHandle);
  try
    FakeResponse.Empty := False;
    Assert(not R.Empty);
    FakeResponse.Empty := True;
    Assert(R.Empty);
  finally
    R.Free;
  end;
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  FakeResponse := TFakeResponse.Create;
  try
    FakeResponseHandle := FakeResponse;
    Test_HTTPResponseCreate;
    // Test_HTTPResponseDestroy - not required
    Test_HTTPResponseSetCookie;
    Test_HTTPResponseSend;
    Test_HTTPResponseSendFmt;
    Test_HTTPResponseSendBinary;
    Test_HTTPResponseSendBytes;
    Test_HTTPResponseSendFile;
    Test_HTTPResponseSendStream;
    Test_HTTPResponseSendEmpty;
    Test_HTTPResponseSendAndRedirect;
    Test_HTTPResponseDownload;
    Test_HTTPResponseRender;
    Test_HTTPResponseReset;
    Test_HTTPResponseClear;
    Test_HTTPResponseIsEmpty;
    Test_HTTPResponseCompressed;
    Test_HTTPResponseHeaders;
    Test_HTTPResponseCookies;
    Test_HTTPResponseEmpty;
  finally
    TBrookLibraryLoader.Unload;
    FakeResponse.Free;
  end;
end.
