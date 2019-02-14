(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  Microframework which helps to develop web Pascal applications.
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
  BrookStringMap;

resourcestring
  SBrookInvalidHTTPStatus = 'Invalid status code: %d.';
  SBrookResponseAlreadySent = 'Response already sent.';

type
  EBrookHTTPResponse = class(Exception);

  TBrookHTTPResponse = class(TBrookHandledPersistent)
  private
    FHeaders: TBrookStringMap;
    FHandle: Psg_httpres;
  protected
    class function DoStreamRead(Acls: Pcvoid; Aoffset: cuint64_t; Abuf: Pcchar;
      Asize: csize_t): cssize_t; cdecl; static;
    class procedure DoStreamFree(Acls: Pcvoid); cdecl; static;
    class procedure CheckStatus(AStatus: Word); static; inline;
    class procedure CheckStream(AStream: TStream); static; inline;
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function GetHandle: Pointer; override;
    procedure CheckAlreadySent(Aret: cint); inline;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    procedure SetCookie(const AName, AValue: string); virtual;
    procedure Send(const AValue, AContentType: string;
      AStatus: Word); overload; virtual;
    procedure Send(const AFmt: string; const AArgs: array of const;
      const AContentType: string; AStatus: Word); overload; virtual;
    procedure Send(const ABytes: TBytes; ASize: NativeUInt;
      const AContentType: string; AStatus: Word); overload; virtual;
    procedure ZSend(const AValue, AContentType: string;
      AStatus: Word); overload; virtual;
    procedure ZSend(const AFmt: string; const AArgs: array of const;
      const AContentType: string; AStatus: Word); overload; virtual;
    procedure ZSend(const ABytes: TBytes; ASize: NativeUInt;
      const AContentType: string; AStatus: Word); overload; virtual;
    procedure SendBinary(ABuffer: Pointer; ASize: NativeUInt;
      const AContentType: string; AStatus: Word); virtual;
    procedure ZSendBinary(ABuffer: Pointer; ASize: NativeUInt;
      const AContentType: string; AStatus: Word); virtual;
    procedure SendFile(ASize: NativeUInt; AMaxSize, AOffset: UInt64;
      const AFileName: TFileName; ARendered: Boolean; AStatus: Word); virtual;
    procedure SendStream(AStream: TStream; AStatus: Word;
      AFreed: Boolean); overload; virtual;
    procedure SendStream(AStream: TStream; AStatus: Word); overload; virtual;
    procedure ZSendStream(AStream: TStream; AStatus: Word;
      AFreed: Boolean); overload; virtual;
    procedure ZSendStream(AStream: TStream; AStatus: Word); overload; virtual;
    procedure SendEmpty(const AContentType: string); overload; virtual;
    procedure SendEmpty; overload; virtual;
    procedure SendAndRedirect(const AValue, ADestination, AContentType: string;
      AStatus: Word); overload; virtual;
    procedure SendAndRedirect(const AValue, ADestination,
      AContentType: string); overload; virtual;
    procedure Download(const AFileName: TFileName); virtual;
    procedure Render(const AFileName: TFileName); virtual;
    procedure Clear; virtual;
    property Headers: TBrookStringMap read FHeaders;
  end;

implementation

constructor TBrookHTTPResponse.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FHeaders := CreateHeaders(sg_httpres_headers(FHandle));
end;

destructor TBrookHTTPResponse.Destroy;
begin
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
  R := sg_httpres_sendbinary(FHandle, M.ToCString(AValue), Length(AValue),
    M.ToCString(AContentType), AStatus);
  CheckAlreadySent(R);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.Send(const AFmt: string;
  const AArgs: array of const; const AContentType: string; AStatus: Word);
begin
  Send(Format(AFmt, AArgs), AContentType, AStatus);
end;

procedure TBrookHTTPResponse.Send(const ABytes: TBytes; ASize: NativeUInt;
  const AContentType: string; AStatus: Word);
begin
  SendBinary(@ABytes[0], ASize, AContentType, AStatus);
end;

procedure TBrookHTTPResponse.ZSend(const AValue, AContentType: string;
  AStatus: Word);
var
  M: TMarshaller;
  R: cint;
begin
  R := sg_httpres_zsendbinary(FHandle, M.ToCString(AValue), Length(AValue),
    M.ToCString(AContentType), AStatus);
  CheckAlreadySent(R);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.ZSend(const AFmt: string;
  const AArgs: array of const; const AContentType: string; AStatus: Word);
begin
  Send(Format(AFmt, AArgs), AContentType, AStatus);
end;

procedure TBrookHTTPResponse.ZSend(const ABytes: TBytes; ASize: NativeUInt;
  const AContentType: string; AStatus: Word);
begin
  ZSendBinary(@ABytes[0], ASize, AContentType, AStatus);
end;

procedure TBrookHTTPResponse.SendBinary(ABuffer: Pointer; ASize: NativeUInt;
  const AContentType: string; AStatus: Word);
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  SgLib.Check;
  R := sg_httpres_sendbinary(FHandle, ABuffer, ASize,
    M.ToCString(AContentType), AStatus);
  CheckAlreadySent(R);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.ZSendBinary(ABuffer: Pointer; ASize: NativeUInt;
  const AContentType: string; AStatus: Word);
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  SgLib.Check;
  R := sg_httpres_zsendbinary(FHandle, ABuffer, ASize,
    M.ToCString(AContentType), AStatus);
  CheckAlreadySent(R);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.SendFile(ASize: NativeUInt; AMaxSize,
  AOffset: UInt64; const AFileName: TFileName; ARendered: Boolean;
  AStatus: Word);
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  SgLib.Check;
  R := sg_httpres_sendfile(FHandle, ASize, AMaxSize, AOffset,
    M.ToCString(AFileName), ARendered, AStatus);
  CheckAlreadySent(R);
  if R = ENOENT then
    raise EFileNotFoundException.Create(SFileNotFound);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.SendStream(AStream: TStream; AStatus: Word;
  AFreed: Boolean);
var
  FCb: sg_free_cb;
  R: cint;
begin
  CheckStream(AStream);
  CheckStatus(AStatus);
  SgLib.Check;
  if AFreed then
    FCb := {$IFNDEF VER3_0}@{$ENDIF}DoStreamFree
  else
    FCb := nil;
  R := sg_httpres_sendstream(FHandle, AStream.Size,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamRead, AStream, FCb, AStatus);
  CheckAlreadySent(R);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.SendStream(AStream: TStream; AStatus: Word);
begin
  SendStream(AStream, AStatus, True);
end;

procedure TBrookHTTPResponse.ZSendStream(AStream: TStream; AStatus: Word;
  AFreed: Boolean);
var
  FCb: sg_free_cb;
  R: cint;
begin
  CheckStream(AStream);
  CheckStatus(AStatus);
  SgLib.Check;
  if AFreed then
    FCb := {$IFNDEF VER3_0}@{$ENDIF}DoStreamFree
  else
    FCb := nil;
  R := sg_httpres_zsendstream(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamRead, AStream, FCb, AStatus);
  CheckAlreadySent(R);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.ZSendStream(AStream: TStream; AStatus: Word);
begin
  ZSendStream(AStream, AStatus, True);
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

procedure TBrookHTTPResponse.Download(const AFileName: TFileName);
var
  M: TMarshaller;
  R: cint;
begin
  SgLib.Check;
  R := sg_httpres_download(FHandle, M.ToCString(AFileName));
  CheckAlreadySent(R);
  if R = ENOENT then
    raise EFileNotFoundException.Create(SFileNotFound);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.Render(const AFileName: TFileName);
var
  M: TMarshaller;
  R: cint;
begin
  SgLib.Check;
  R := sg_httpres_render(FHandle, M.ToCString(AFileName));
  CheckAlreadySent(R);
  if R = ENOENT then
    raise EFileNotFoundException.Create(SFileNotFound);
  SgLib.CheckLastError(R);
end;

procedure TBrookHTTPResponse.Clear;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpres_clear(FHandle));
end;

end.
