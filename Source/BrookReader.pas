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

{ Contains classes for reading text line by line from a string, stream or
  file. }

unit BrookReader;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysUtils,
  Classes;

const
  BROOK_MIN_BYTES_SIZE = 128;
  BROOK_BYTES_SIZE = 4096;
  BROOK_FILE_RIGHTS = 438;

type
  TBrookTextReader = class abstract(TObject)
  protected
    function GetEncoding: TEncoding; virtual; abstract;
    procedure SetEncoding(AValue: TEncoding); virtual; abstract;
  public
    procedure Reset; virtual; abstract;
    procedure Close; virtual; abstract;
    function IsEof: Boolean; virtual; abstract;
    procedure ReadBytes(out ALine: TBytes); overload; virtual; abstract;
    function ReadBytes: TBytes; overload; virtual; abstract;
    procedure Read(out ALine: string); overload; virtual; abstract;
    function Read: string; overload; virtual; abstract;
    property Eof: Boolean read IsEof;
    property Encoding: TEncoding read GetEncoding write SetEncoding;
  end;

  TBrookStreamReader = class(TBrookTextReader)
  private
    FBytesRead: Integer;
    FBytesPosition: Integer;
    FEncoding: TEncoding;
    FOwnsStream: Boolean;
    FStream: TStream;
    FBytes: TBytes;
  protected
    function GetEncoding: TEncoding; override;
    function GetOwnsStream: Boolean; virtual;
    function GetStream: TStream; virtual;
    procedure SetEncoding(AValue: TEncoding); override;
    procedure SetOwnsStream(AValue: Boolean); virtual;
    procedure FillBytes; virtual;
    property BytesRead: Integer read FBytesRead write FBytesRead;
    property BytesPosition: Integer read FBytesPosition write FBytesPosition;
    property Bytes: TBytes read FBytes write FBytes;
  public
    constructor Create(AEncoding: TEncoding; AStream: TStream;
      ABytesSize: Integer; AOwnsStream: Boolean); reintroduce; overload; virtual;
    constructor Create(AEncoding: TEncoding; AStream: TStream);
      reintroduce; overload; virtual;
    constructor Create(AStream: TStream); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure Reset; override;
    procedure Close; override;
    function IsEof: Boolean; override;
    procedure ReadBytes(out ALine: TBytes); overload; override;
    function ReadBytes: TBytes; overload; override;
    procedure Read(out ALine: string); overload; override;
    function Read: string; overload; override;
    property Stream: TStream read GetStream;
    property OwnsStream: Boolean read GetOwnsStream write SetOwnsStream;
  end;

  TBrookBaseReader = class(TBrookTextReader)
  protected
    procedure SetEncoding(AValue: TEncoding); override;
    procedure SetProxyReader(AValue: TBrookTextReader); virtual; abstract;
    function GetEncoding: TEncoding; override;
    function GetProxyReader: TBrookTextReader; virtual; abstract;
    property ProxyReader: TBrookTextReader read GetProxyReader
      write SetProxyReader;
  public
    destructor Destroy; override;
    procedure Reset; override;
    procedure Close; override;
    function IsEof: Boolean; override;
    procedure ReadBytes(out ALine: TBytes); overload; override;
    function ReadBytes: TBytes; overload; override;
    procedure Read(out ALine: string); overload; override;
    function Read: string; overload; override;
  end;

  TBrookStringReader = class(TBrookBaseReader)
  private
    FProxyReader: TBrookTextReader;
  protected
    procedure SetProxyReader(AValue: TBrookTextReader); override;
    function GetProxyReader: TBrookTextReader; override;
  public
    constructor Create(AEncoding: TEncoding; const AString: string;
      ABytesSize: Integer); reintroduce; overload; virtual;
    constructor Create(AEncoding: TEncoding; const AString: string);
      reintroduce; overload; virtual;
    constructor Create(const AString: string); reintroduce; overload; virtual;
  end;

  TBrookFileReader = class(TBrookBaseReader)
  private
    FProxyReader: TBrookTextReader;
  protected
    procedure SetProxyReader(AValue: TBrookTextReader); override;
    function GetProxyReader: TBrookTextReader; override;
  public
    constructor Create(AEncoding: TEncoding; const AFileName: TFileName;
      AMode: Word; ARights: Cardinal;
      ABytesSize: Integer); reintroduce; overload; virtual;
    constructor Create(AEncoding: TEncoding; const AFileName: TFileName;
      AMode: Word; ABytesSize: Integer); reintroduce; overload; virtual;
    constructor Create(AEncoding: TEncoding; const AFileName: TFileName;
      ABytesSize: Integer); reintroduce; overload; virtual;
    constructor Create(AEncoding: TEncoding;
      const AFileName: TFileName); reintroduce; overload; virtual;
    constructor Create(
      const AFileName: TFileName); reintroduce; overload; virtual;
  end;

implementation

{ TBrookStreamReader }

constructor TBrookStreamReader.Create(AEncoding: TEncoding; AStream: TStream;
  ABytesSize: Integer; AOwnsStream: Boolean);
begin
  inherited Create;
  if not Assigned(AStream) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AStream']);
  FEncoding := AEncoding;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  if ABytesSize >= BROOK_MIN_BYTES_SIZE then
    SetLength(FBytes, ABytesSize)
  else
    SetLength(FBytes, BROOK_MIN_BYTES_SIZE);
end;

constructor TBrookStreamReader.Create(AEncoding: TEncoding; AStream: TStream);
begin
  Create(AEncoding, AStream, BROOK_BYTES_SIZE, False);
end;

constructor TBrookStreamReader.Create(AStream: TStream);
begin
  Create(TEncoding.UTF8, AStream);
end;

destructor TBrookStreamReader.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TBrookStreamReader.FillBytes;
begin
  FBytesRead := FStream.Read(FBytes[0], Pred(Length(FBytes)));
  FBytes[FBytesRead] := 0;
  FBytesPosition := 0;
end;

function TBrookStreamReader.GetEncoding: TEncoding;
begin
  Result := FEncoding;
end;

function TBrookStreamReader.GetOwnsStream: Boolean;
begin
  Result := FOwnsStream;
end;

function TBrookStreamReader.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TBrookStreamReader.SetEncoding(AValue: TEncoding);
begin
  FEncoding := AValue;
end;

procedure TBrookStreamReader.SetOwnsStream(AValue: Boolean);
begin
  FOwnsStream := AValue;
end;

procedure TBrookStreamReader.Reset;
begin
  FBytesRead := 0;
  FBytesPosition := 0;
  if Assigned(FStream) then
    FStream.Seek(0, TSeekOrigin.soBeginning);
end;

procedure TBrookStreamReader.Close;
begin
  if FOwnsStream then
  begin
    FStream.Free;
    FStream := nil;
  end;
end;

function TBrookStreamReader.IsEof: Boolean;
begin
  if not Assigned(FStream) then
    Exit(True);
  Result := FBytesPosition >= FBytesRead;
  if Result then
  begin
    FillBytes;
    Result := FBytesRead = 0;
  end;
end;

procedure TBrookStreamReader.ReadBytes(out ALine: TBytes);
var
  VPByte: PByte;
  VPosition, VStrLength, VLength: Integer;
begin
  VPosition := FBytesPosition;
  ALine := nil;
  repeat
    VPByte := @FBytes[FBytesPosition];
    while (FBytesPosition < FBytesRead) and not (VPByte^ in [10, 13]) do
    begin
      Inc(VPByte);
      Inc(FBytesPosition);
    end;
    if FBytesPosition = FBytesRead then
    begin
      VLength := FBytesPosition - VPosition;
      if VLength > 0 then
      begin
        VStrLength := Length(ALine);
        SetLength(ALine, VStrLength + VLength);
        Move(FBytes[VPosition], ALine[VStrLength], VLength);
      end;
      FillBytes;
      VPosition := FBytesPosition;
    end;
  until (FBytesPosition = FBytesRead) or (VPByte^ in [10, 13]);
  VLength := FBytesPosition - VPosition;
  if VLength > 0 then
  begin
    VStrLength := Length(ALine);
    SetLength(ALine, VStrLength + VLength);
    Move(FBytes[VPosition], ALine[VStrLength], VLength);
  end;
  if (VPByte^ in [10, 13]) and (FBytesPosition < FBytesRead) then
  begin
    Inc(FBytesPosition);
    if VPByte^ = 13 then
    begin
      if FBytesPosition = FBytesRead then
        FillBytes;
      if (FBytesPosition < FBytesRead) and (FBytes[FBytesPosition] = 10) then
        Inc(FBytesPosition);
    end;
  end;
end;

function TBrookStreamReader.ReadBytes: TBytes;
begin
  ReadBytes(Result);
end;

procedure TBrookStreamReader.Read(out ALine: string);
begin
  ALine :=
{$IFDEF FPC}string({$ENDIF}FEncoding.GetString(ReadBytes){$IFDEF FPC}){$ENDIF};
end;

function TBrookStreamReader.Read: string;
begin
  Read(Result);
end;

{ TBrookBaseReader }

destructor TBrookBaseReader.Destroy;
begin
  ProxyReader.Free;
  ProxyReader := nil;
  inherited Destroy;
end;

procedure TBrookBaseReader.SetEncoding(AValue: TEncoding);
begin
  ProxyReader.Encoding := AValue;
end;

function TBrookBaseReader.GetEncoding: TEncoding;
begin
  Result := ProxyReader.Encoding;
end;

procedure TBrookBaseReader.Reset;
begin
  ProxyReader.Reset;
end;

procedure TBrookBaseReader.Close;
begin
  ProxyReader.Close;
end;

function TBrookBaseReader.IsEof: Boolean;
begin
  Result := ProxyReader.IsEof;
end;

procedure TBrookBaseReader.ReadBytes(out ALine: TBytes);
begin
  ProxyReader.ReadBytes(ALine);
end;

function TBrookBaseReader.ReadBytes: TBytes;
begin
  ProxyReader.ReadBytes(Result);
end;

procedure TBrookBaseReader.Read(out ALine: string);
begin
  ProxyReader.Read(ALine);
end;

function TBrookBaseReader.Read: string;
begin
  ProxyReader.Read(Result);
end;

{ TBrookStringReader }

constructor TBrookStringReader.Create(AEncoding: TEncoding;
  const AString: string; ABytesSize: Integer);
{$IFNDEF FPC}
var
  VEncoding: TEncoding;
{$ENDIF}
begin
  inherited Create;
{$IFNDEF FPC}
  if Assigned(AEncoding) then
    VEncoding := AEncoding
  else
    VEncoding := TEncoding.Default;
{$ENDIF}
  FProxyReader := TBrookStreamReader.Create(AEncoding, TStringStream.Create(
    AString{$IFNDEF FPC}, VEncoding{$ENDIF}), ABytesSize, True);
end;

constructor TBrookStringReader.Create(AEncoding: TEncoding;
  const AString: string);
begin
  Create(AEncoding, AString, BROOK_BYTES_SIZE);
end;

constructor TBrookStringReader.Create(const AString: string);
begin
  Create(TEncoding.UTF8, AString);
end;

procedure TBrookStringReader.SetProxyReader(AValue: TBrookTextReader);
begin
  FProxyReader := AValue;
end;

function TBrookStringReader.GetProxyReader: TBrookTextReader;
begin
  Result := FProxyReader;
end;

{ TBrookFileReader }

constructor TBrookFileReader.Create(AEncoding: TEncoding;
  const AFileName: TFileName; AMode: Word; ARights: Cardinal;
  ABytesSize: Integer);
begin
  inherited Create;
  FProxyReader := TBrookStreamReader.Create(AEncoding,
    TFileStream.Create(AFileName, AMode, ARights), ABytesSize, True);
end;

constructor TBrookFileReader.Create(AEncoding: TEncoding;
  const AFileName: TFileName; AMode: Word; ABytesSize: Integer);
begin
  Create(AEncoding, AFileName, AMode, BROOK_FILE_RIGHTS, ABytesSize);
end;

constructor TBrookFileReader.Create(AEncoding: TEncoding;
  const AFileName: TFileName; ABytesSize: Integer);
begin
  Create(AEncoding, AFileName, fmOpenRead or fmShareDenyWrite, ABytesSize);
end;

constructor TBrookFileReader.Create(AEncoding: TEncoding;
  const AFileName: TFileName);
begin
  Create(AEncoding, AFileName, BROOK_BYTES_SIZE);
end;

constructor TBrookFileReader.Create(const AFileName: TFileName);
begin
  Create(TEncoding.UTF8, AFileName);
end;

procedure TBrookFileReader.SetProxyReader(AValue: TBrookTextReader);
begin
  FProxyReader := AValue;
end;

function TBrookFileReader.GetProxyReader: TBrookTextReader;
begin
  Result := FProxyReader;
end;

end.
