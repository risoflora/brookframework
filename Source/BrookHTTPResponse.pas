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

{ Contains class which dispatches data to the client. }

unit BrookHTTPResponse;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysConst,
  SysUtils,
  Classes,
  Platform,
  Marshalling,
  libsagui,
  BrookHandledClasses,
  BrookStringMap,
  BrookHTTPCookies;

resourcestring
  { Error message @code('Invalid status code: <code>.'). }
  SBrookInvalidHTTPStatus = 'Invalid status code: %d.';
  { Error message @code('Response already sent.'). }
  SBrookResponseAlreadySent = 'Response already sent.';
  { Error message @code('Generic ZLib error.'). }
  SBrookZLibError = 'Generic ZLib error.';

type
  { Handles exceptions related to response class. }
  EBrookHTTPResponse = class(Exception);

  { Class which dispatches headers, contents, binaries, files and other data to
    the client. }
  TBrookHTTPResponse = class(TBrookHandledPersistent)
  private
    FCookies: TBrookHTTPCookies;
    FHeaders: TBrookStringMap;
    FHandle: Psg_httpres;
    FCompressed: Boolean;
    procedure SetCookies(AValue: TBrookHTTPCookies);
  protected
    class function DoStreamRead(Acls: Pcvoid; Aoffset: cuint64_t; Abuf: Pcchar;
      Asize: csize_t): cssize_t; cdecl; static;
    class procedure DoStreamFree(Acls: Pcvoid); cdecl; static;
    class procedure CheckStatus(AStatus: Word); static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    class procedure CheckStream(AStream: TStream); static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateCookies(AOwner: TPersistent): TBrookHTTPCookies; virtual;
    function GetHandle: Pointer; override;
    procedure CheckAlreadySent(Aret: cint); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure CheckZLib(Aret: cint); {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    { Creates an instance of @code(TBrookHTTPResponse).
      @param(AHandle[in] Request handle.) }
    constructor Create(AHandle: Pointer); virtual;
    { Frees an instance of @code(TBrookHTTPResponse). }
    destructor Destroy; override;
    { Sets server cookie to the response handle.
      @param(AName[in] Cookie name.)
      @param(AValue[in] Cookie value.) }
    procedure SetCookie(const AName, AValue: string); virtual;
    { Sends a string content to the client.
      @param(AValue[in] String to be sent.)
      @param(AContentType[in] Content type.)
      @param(AStatus[in] HTTP status code.) }
    procedure Send(const AValue, AContentType: string; AStatus: Word); virtual;
    { Sends a formatted string content to the client.
      @param(AFmt[in] Format string.)
      @param(AArgs[in] Arguments to compose the formatted string.)
      @param(AContentType[in] Content type.)
      @param(AStatus[in] HTTP status code.) }
    procedure SendFmt(const AFormat: string; const AArgs: array of const;
      const AContentType: string; AStatus: Word); virtual;
    { Sends a binary content to the client.
      @param(ABinary[in] Binary content to be sent.)
      @param(ASize[in] Content size.)
      @param(AContentType[in] Content type.)
      @param(AStatus[in] HTTP status code.) }
    procedure SendBinary(ABuffer: Pointer; ASize: NativeUInt;
      const AContentType: string; AStatus: Word); virtual;
    { Sends an array of Bytes content to the client.
      @param(ABytes[in] Array of Bytes to be sent.)
      @param(ASize[in] Content size.)
      @param(AContentType[in] Content type.)
      @param(AStatus[in] HTTP status code.) }
    procedure SendBytes(const ABytes: TBytes; ASize: NativeUInt;
      const AContentType: string; AStatus: Word); virtual;
    { Sends a file to the client.
      @param(ASize[in] Size of the file to be sent. Use zero to calculate
        automatically.)
      @param(AMaxSize[in] Maximum allowed file size. Use zero for no limit.)
      @param(AOffset[in] Offset to start reading from in the file to be sent.)
      @param(AFileName[in] Path of the file to be sent.)
      @param(ADownloaded[in] If @True it offers the file as download.)
      @param(AStatus[in] HTTP status code.) }
    procedure SendFile(ASize: NativeUInt; AMaxSize, AOffset: UInt64;
      const AFileName: TFileName; ADownloaded: Boolean; AStatus: Word); virtual;
    { Sends a stream to the client.
      @param(AStream[in] Stream to be sent.)
      @param(AFreed[in] @True frees the stream automatically as soon as it
        is sent.)
      @param(AStatus[in] HTTP status code.) }
    procedure SendStream(AStream: TStream; AFreed: Boolean;
      AStatus: Word); overload; virtual;
    { Sends a stream to the client. The stream is freed automatically as soon as
      it is sent.
      @param(AStream[in] Stream to be sent.)
      @param(AStatus[in] HTTP status code.) }
    procedure SendStream(AStream: TStream; AStatus: Word); overload; virtual;
    { Sends an HTTP status 204 to the client indicating the server has fulfilled
      the request, but does not need to return a content.
      @param(AContentType[in] Content type.) }
    procedure SendEmpty(const AContentType: string); overload; virtual;
    { Sends an HTTP status 204 to the client indicating the server has fulfilled
      the request, but does not need to return a content. }
    procedure SendEmpty; overload; virtual;
    { Sends a string content to the client and redirects it to a new location.
      @param(AValue[in] String to be sent.)
      @param(ADestination[in] Destination to which it will be redirected as soon
        as the content is sent.)
      @param(AContentType[in] Content type.)
      @param(AStatus[in] HTTP status code (must be >=300 and <=307).) }
    procedure SendAndRedirect(const AValue, ADestination, AContentType: string;
      AStatus: Word); overload; virtual;
    { Sends a string content to the client with HTTP status 302 and redirects it
      to a new location.
      @param(AValue[in] String to be sent.)
      @param(ADestination[in] Destination to which it will be redirected as soon
        as the content is sent.)
      @param(AContentType[in] Content type.) }
    procedure SendAndRedirect(const AValue, ADestination,
      AContentType: string); overload; virtual;
    { Offers a file as download.
      @param(AFileName[in] Path of the file to be sent.)
      @param(AStatus[in] HTTP status code.) }
    procedure Download(const AFileName: TFileName;
      AStatus: Word); overload; virtual;
    { Sends a file to be rendered.
      @param(AFileName[in] Path of the file to be sent.)
      @param(AStatus[in] HTTP status code.) }
    procedure Render(const AFileName: TFileName;
      AStatus: Word); overload; virtual;
    { Offers a file as download.
      @param(AFileName[in] Path of the file to be sent.) }
    procedure Download(const AFileName: TFileName); overload; virtual;
    { Sends a file to be rendered.
      @param(AFileName[in] Path of the file to be sent.) }
    procedure Render(const AFileName: TFileName); overload; virtual;
    { Clears all headers, cookies, statuses and internal buffers of the response
      object. }
    procedure Clear; virtual;
    { Checks if the response is empty. }
    function IsEmpty: Boolean;
    { Determines if the content must be compressed while sending.
      The compression is done by the ZLib library using the DEFLATE compression
      algorithm. It uses the Gzip format when the content is a file. }
    property Compressed: Boolean read FCompressed write FCompressed;
    { Hash table containing the headers to be sent to the client. }
    property Headers: TBrookStringMap read FHeaders;
    { Cookies to be sent to the client. }
    property Cookies: TBrookHTTPCookies read FCookies write SetCookies;
    { Determines if the response is empty. }
    property Empty: Boolean read IsEmpty;
  end;

implementation

constructor TBrookHTTPResponse.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FHeaders := CreateHeaders(sg_httpres_headers(FHandle));
  FCookies := CreateCookies(Self);
end;

destructor TBrookHTTPResponse.Destroy;
var
  C: TBrookHTTPCookie;
begin
  for C in FCookies do
    FHeaders.Add('Set-Cookie', C.ToString);
  FCookies.Free;
  FHeaders.Free;
  inherited Destroy;
end;

function TBrookHTTPResponse.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPResponse.CheckAlreadySent(Aret: cint);
begin
  if Aret = EALREADY then
    raise EBrookHTTPResponse.Create(SBrookResponseAlreadySent);
end;

procedure TBrookHTTPResponse.CheckZLib(Aret: cint);
begin
  if Aret < 0 then
    raise EBrookHTTPResponse.Create(SBrookZLibError);
end;

class procedure TBrookHTTPResponse.CheckStatus(AStatus: Word);
begin
  if (AStatus < 100) or (AStatus > 599) then
    raise EArgumentException.CreateFmt(SBrookInvalidHTTPStatus, [AStatus]);
end;

class procedure TBrookHTTPResponse.CheckStream(AStream: TStream);
begin
  if not Assigned(AStream) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AStream']);
end;

function TBrookHTTPResponse.CreateHeaders(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPResponse.CreateCookies(AOwner: TPersistent): TBrookHTTPCookies;
begin
  Result := TBrookHTTPCookies.Create(AOwner);
end;

{$IFDEF FPC}
 {$PUSH}{$WARN 5024 OFF}
{$ENDIF}

class function TBrookHTTPResponse.DoStreamRead(
  Acls: Pcvoid; Aoffset: cuint64_t; //FI:O804
  Abuf: Pcchar; Asize: csize_t): cssize_t;
begin
  Result := TStream(Acls).Read(Abuf^, Asize);
  if Result = 0 then
    Exit(sg_eor(False));
  if Result < 0 then
    Result := sg_eor(True);
end;

{$IFDEF FPC}
 {$POP}
{$ENDIF}

class procedure TBrookHTTPResponse.DoStreamFree(Acls: Pcvoid);
begin
  TStream(Acls).Free;
end;

procedure TBrookHTTPResponse.SetCookies(AValue: TBrookHTTPCookies);
begin
  if AValue = FCookies then
    Exit;
  if Assigned(AValue) then
    FCookies.Assign(AValue)
  else
    FCookies.Clear;
end;

procedure TBrookHTTPResponse.SetCookie(const AName, AValue: string);
var
  M: TMarshaller;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpres_set_cookie(FHandle, M.ToCString(AName),
    M.ToCString(AValue)));
end;

procedure TBrookHTTPResponse.Send(const AValue, AContentType: string;
  AStatus: Word);
var
  M: TMarshaller;
  R: cint;
begin
  if FCompressed then
  begin
    R := sg_httpres_zsendbinary(FHandle, M.ToCString(AValue), Length(AValue),
      M.ToCString(AContentType), AStatus);
    CheckZLib(R);
  end
  else
    R := sg_httpres_sendbinary(FHandle, M.ToCString(AValue), M.Length(AValue),
      M.ToCString(AContentType), AStatus);
  CheckAlreadySent(R);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.SendFmt(const AFormat: string;
  const AArgs: array of const; const AContentType: string; AStatus: Word);
begin
  Send(Format(AFormat, AArgs), AContentType, AStatus);
end;

procedure TBrookHTTPResponse.SendBinary(ABuffer: Pointer; ASize: NativeUInt;
  const AContentType: string; AStatus: Word);
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  SgLib.Check;
  if FCompressed then
  begin
    R := sg_httpres_zsendbinary(FHandle, ABuffer, ASize,
      M.ToCString(AContentType), AStatus);
    CheckZLib(R);
  end
  else
    R := sg_httpres_sendbinary(FHandle, ABuffer, ASize,
      M.ToCString(AContentType), AStatus);
  CheckAlreadySent(R);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.SendBytes(const ABytes: TBytes; ASize: NativeUInt;
  const AContentType: string; AStatus: Word);
begin
  SendBinary(@ABytes[0], ASize, AContentType, AStatus);
end;

procedure TBrookHTTPResponse.SendFile(ASize: NativeUInt; AMaxSize,
  AOffset: UInt64; const AFileName: TFileName; ADownloaded: Boolean;
  AStatus: Word);
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  SgLib.Check;
  if FCompressed then
  begin
    R := sg_httpres_zsendfile(FHandle, ASize, AMaxSize, AOffset,
      M.ToCString(AFileName), ADownloaded, AStatus);
    CheckZLib(R);
  end
  else
    R := sg_httpres_sendfile(FHandle, ASize, AMaxSize, AOffset,
      M.ToCString(AFileName), ADownloaded, AStatus);
  CheckAlreadySent(R);
  if R = ENOENT then
    raise EFileNotFoundException.Create(SFileNotFound);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.SendStream(AStream: TStream; AFreed: Boolean;
  AStatus: Word);
var
  FCb: sg_free_cb;
  R: cint;
begin
  CheckStream(AStream);
  CheckStatus(AStatus);
  SgLib.Check;
  if AFreed then
    FCb := DoStreamFree
  else
    FCb := nil;
  if FCompressed then
  begin
    R := sg_httpres_zsendstream(FHandle, DoStreamRead, AStream, FCb, AStatus);
    CheckZLib(R);
  end
  else
    R := sg_httpres_sendstream(FHandle, 0, DoStreamRead, AStream, FCb, AStatus);
  CheckAlreadySent(R);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.SendStream(AStream: TStream; AStatus: Word);
begin
  SendStream(AStream, True, AStatus);
end;

procedure TBrookHTTPResponse.SendEmpty(const AContentType: string);
begin
  Clear;
  Send('', AContentType, 204);
end;

procedure TBrookHTTPResponse.SendEmpty;
begin
  Clear;
  Send('', '', 204);
end;

procedure TBrookHTTPResponse.SendAndRedirect(const AValue, ADestination,
  AContentType: string; AStatus: Word);
begin
  if (AStatus < 300) or (AStatus > 307) then
    raise EBrookHTTPResponse.CreateFmt(SBrookInvalidHTTPStatus, [AStatus]);
  FHeaders.AddOrSet('Location', ADestination);
  Send(AValue, AContentType, AStatus);
end;

procedure TBrookHTTPResponse.SendAndRedirect(const AValue, ADestination,
  AContentType: string);
begin
  SendAndRedirect(AValue, ADestination, AContentType, 302);
end;

procedure TBrookHTTPResponse.Download(const AFileName: TFileName;
  AStatus: Word);
var
  M: TMarshaller;
  R: cint;
begin
  SgLib.Check;
  if FCompressed then
  begin
    R := sg_httpres_zdownload(FHandle, M.ToCString(AFileName), AStatus);
    CheckZLib(R);
  end
  else
    R := sg_httpres_download(FHandle, M.ToCString(AFileName), AStatus);
  CheckAlreadySent(R);
  if R = ENOENT then
    raise EFileNotFoundException.Create(SFileNotFound);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.Download(const AFileName: TFileName);
begin
  Download(AFileName, 200);
end;

procedure TBrookHTTPResponse.Render(const AFileName: TFileName;
  AStatus: Word);
var
  M: TMarshaller;
  R: cint;
begin
  SgLib.Check;
  if FCompressed then
  begin
    R := sg_httpres_zrender(FHandle, M.ToCString(AFileName), AStatus);
    CheckZLib(R);
  end
  else
    R := sg_httpres_render(FHandle, M.ToCString(AFileName), AStatus);
  CheckAlreadySent(R);
  if R = ENOENT then
    raise EFileNotFoundException.Create(SFileNotFound);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.Render(const AFileName: TFileName);
begin
  Render(AFileName, 200);
end;

procedure TBrookHTTPResponse.Clear;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpres_clear(FHandle));
end;

function TBrookHTTPResponse.IsEmpty: Boolean;
begin
  SgLib.Check;
  Result := sg_httpres_is_empty(FHandle);
end;

end.
