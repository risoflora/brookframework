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

{ Contains classes for reading text line by line from a string, stream or file. }

unit BrookReader;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysUtils,
  Classes,
  BrookExtra;

type
  { Abstract class for line reader. }
  TBrookTextReader = class abstract
  protected
    function GetEncoding: TEncoding; virtual; abstract;
    procedure SetEncoding(AValue: TEncoding); virtual; abstract;
  public
    { Resets the reader to its initial state. }
    procedure Reset; virtual; abstract;
    { Closes the reader. }
    procedure Close; virtual; abstract;
    { Checks if the reader has reached the End-Of-File.
      @returns(@True if the reader has reached the End-Of-File.) }
    function IsEOF: Boolean; virtual; abstract;
    { Reads a line as bytes.
      @param(ALine[out] Line read as bytes.) }
    procedure ReadBytes(out ALine: TBytes); overload; virtual; abstract;
    { Reads a line returning it as bytes.
      @returns(Line read as bytes.) }
    function ReadBytes: TBytes; overload; virtual; abstract;
    { Reads a line as static string.
      @param(ALine[out] read as static string.) }
    procedure Read(out ALine: string); overload; virtual; abstract;
    { Reads a line returning it as static string.
      @returns(Line read as static string.) }
    function Read: string; overload; virtual; abstract;
    { @True if the reader has reached the End-Of-File. }
    property EOF: Boolean read IsEOF;
    { Character encoding determined during reading. }
    property Encoding: TEncoding read GetEncoding write SetEncoding;
  end;

  { Line reader which uses stream as source of lines. }
  TBrookStreamReader = class(TBrookTextReader)
  private
    FStream: TStream;
    FEncoding: TEncoding;
    FBuffer: TBytes;
    FBufferRead: Integer;
    FBufferPosition: Integer;
    FOwnsStream: Boolean;
  protected
    function GetEncoding: TEncoding; override;
    function GetOwnsStream: Boolean; virtual;
    function GetStream: TStream; virtual;
    procedure SetEncoding(AValue: TEncoding); override;
    procedure SetOwnsStream(AValue: Boolean); virtual;
    procedure FillBuffer; virtual;
    property BufferRead: Integer read FBufferRead write FBufferRead;
    property BufferPosition: Integer read FBufferPosition write FBufferPosition;
    property Buffer: TBytes read FBuffer write FBuffer;
  public
    { Creates an instance of @link(TBrookStreamReader)
      @param(AEncoding[in] Character encoding determined during reading.)
      @param(AStream[in] Stream to be read line by line.)
      @param(ABufferSize[in] Buffer size for the line reading.)
      @param(AOwnsStream[in] If @True the stream is freed on @link(Destroy).) }
    constructor Create(AEncoding: TEncoding; AStream: TStream;
      ABufferSize: Cardinal; AOwnsStream: Boolean); reintroduce; overload; virtual;
    { Creates an instance of @link(TBrookStreamReader)
      @param(AEncoding[in] Character encoding determined during reading.)
      @param(AStream[in] Stream to be read line by line.) }
    constructor Create(AEncoding: TEncoding; AStream: TStream);
      reintroduce; overload; virtual;
    { Creates an instance of @link(TBrookStreamReader)
      @param(AStream[in] Stream to be read line by line.) }
    constructor Create(AStream: TStream); reintroduce; overload; virtual;
    { Destroys an instance of @link(TBrookStreamReader). }
    destructor Destroy; override;
    { Resets the cursor to the beginning of the stream. }
    procedure Reset; override;
    { Frees the stream if property @link(OwnsStream) is @True. }
    procedure Close; override;
    { Checks if the reader has reached the End-Of-File.
      @returns(@True if the stream has reached the End-Of-File.) }
    function IsEOF: Boolean; override;
    { Reads a line as bytes.
      @param(ALine[out] Line read as bytes.) }
    procedure ReadBytes(out ALine: TBytes); overload; override;
    { Reads a line returning it as bytes.
      @returns(Line read as bytes.) }
    function ReadBytes: TBytes; overload; override;
    { Reads a line as static string.
      @param(ALine[out] read as static string.) }
    procedure Read(out ALine: string); overload; override;
    { Reads a line returning it as static string.
      @returns(Line read as static string.) }
    function Read: string; overload; override;
    { Source stream containing the lines to be read. }
    property Stream: TStream read GetStream;
    { If @True the stream is freed on @link(Destroy). }
    property OwnsStream: Boolean read GetOwnsStream write SetOwnsStream;
  end;

  { Base proxied line reader. }
  TBrookBaseReader = class(TBrookTextReader)
  protected
    procedure SetEncoding(AValue: TEncoding); override;
    procedure SetProxyReader(AValue: TBrookTextReader); virtual; abstract;
    function GetEncoding: TEncoding; override;
    function GetProxyReader: TBrookTextReader; virtual; abstract;
    property ProxyReader: TBrookTextReader read GetProxyReader
      write SetProxyReader;
  public
    { Destroys an instance of @link(TBrookBaseReader). }
    destructor Destroy; override;
    { Resets the reader to its initial state. }
    procedure Reset; override;
    { Closes the reader. }
    procedure Close; override;
    { Checks if the reader has reached the End-Of-File.
      @returns(@True if the reader has reached the End-Of-File.) }
    function IsEOF: Boolean; override;
    { Reads a line as bytes.
      @param(ALine[out] Line read as bytes.) }
    procedure ReadBytes(out ALine: TBytes); overload; override;
    { Reads a line returning it as bytes.
      @returns(Line read as bytes.) }
    function ReadBytes: TBytes; overload; override;
    { Reads a line as static string.
      @param(ALine[out] read as static string.) }
    procedure Read(out ALine: string); overload; override;
    { Reads a line returning it as static string.
      @returns(Line read as static string.) }
    function Read: string; overload; override;
  end;

  { String line reader. }
  TBrookStringReader = class(TBrookBaseReader)
  private
    FProxyReader: TBrookTextReader;
  protected
    procedure SetProxyReader(AValue: TBrookTextReader); override;
    function GetProxyReader: TBrookTextReader; override;
  public
    { Creates an instance of @link(TBrookStringReader)
      @param(AEncoding[in] Character encoding determined during reading.)
      @param(AString[in] String to be read line by line.)
      @param(ABufferSize[in] Buffer size for the line reading.) }
    constructor Create(AEncoding: TEncoding; const AString: string;
      ABufferSize: Integer); reintroduce; overload; virtual;
    { Creates an instance of @link(TBrookStringReader)
      @param(AEncoding[in] Character encoding determined during reading.)
      @param(AString[in] String to be read line by line.) }
    constructor Create(AEncoding: TEncoding; const AString: string);
      reintroduce; overload; virtual;
    { Creates an instance of @link(TBrookStringReader)
      @param(AString[in] String to be read line by line.) }
    constructor Create(const AString: string); reintroduce; overload; virtual;
  end;

  { File line reader. }
  TBrookFileReader = class(TBrookBaseReader)
  private
    FProxyReader: TBrookTextReader;
  protected
    procedure SetProxyReader(AValue: TBrookTextReader); override;
    function GetProxyReader: TBrookTextReader; override;
  public
    { Creates an instance of @link(TBrookStringReader)
      @param(AEncoding[in] Character encoding determined during reading.)
      @param(AFileName[in] File to be read line by line.)
      @param(AMode[in] Open mode and (possibly) a share mode or'ed together.)
      @param(ARights[in] Permission bits with which to create the file on Linux.)
      @param(ABufferSize[in] Buffer size for the line reading.) }
    constructor Create(AEncoding: TEncoding; const AFileName: TFileName;
      AMode: Word; ARights: Cardinal;
      ABufferSize: Integer); reintroduce; overload; virtual;
    { Creates an instance of @link(TBrookStringReader)
      @param(AEncoding[in] Character encoding determined during reading.)
      @param(AFileName[in] File to be read line by line.)
      @param(AMode[in] Open mode and (possibly) a share mode or'ed together.)
      @param(ABufferSize[in] Buffer size for the line reading.) }
    constructor Create(AEncoding: TEncoding; const AFileName: TFileName;
      AMode: Word; ABufferSize: Integer); reintroduce; overload; virtual;
    { Creates an instance of @link(TBrookStringReader)
      @param(AEncoding[in] Character encoding determined during reading.)
      @param(AFileName[in] File to be read line by line.)
      @param(ABufferSize[in] Buffer size for the line reading.) }
    constructor Create(AEncoding: TEncoding; const AFileName: TFileName;
      ABufferSize: Integer); reintroduce; overload; virtual;
    { Creates an instance of @link(TBrookStringReader)
      @param(AEncoding[in] Character encoding determined during reading.)
      @param(AFileName[in] File to be read line by line.) }
    constructor Create(AEncoding: TEncoding;
      const AFileName: TFileName); reintroduce; overload; virtual;
    { Creates an instance of @link(TBrookStringReader)
      @param(AFileName[in] File to be read line by line.) }
    constructor Create(
      const AFileName: TFileName); reintroduce; overload; virtual;
  end;

implementation

{ TBrookStreamReader }

constructor TBrookStreamReader.Create(AEncoding: TEncoding; AStream: TStream;
  ABufferSize: Cardinal; AOwnsStream: Boolean);
begin
  inherited Create;
  if not Assigned(AStream) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AStream']);
  FEncoding := AEncoding;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  if ABufferSize >= BROOK_MIN_BUFFER_SIZE then
    SetLength(FBuffer, ABufferSize)
  else
    SetLength(FBuffer, BROOK_MIN_BUFFER_SIZE);
end;

constructor TBrookStreamReader.Create(AEncoding: TEncoding; AStream: TStream);
begin
  Create(AEncoding, AStream, BROOK_BUFFER_SIZE, False);
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

procedure TBrookStreamReader.FillBuffer;
begin
  FBufferRead := FStream.Read(FBuffer[0], Pred(Length(FBuffer)));
  FBuffer[FBufferRead] := 0;
  FBufferPosition := 0;
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
  FBufferRead := 0;
  FBufferPosition := 0;
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

function TBrookStreamReader.IsEOF: Boolean;
begin
  if not Assigned(FStream) then
    Exit(True);
  Result := FBufferPosition >= FBufferRead;
  if Result then
  begin
    FillBuffer;
    Result := FBufferRead = 0;
  end;
end;

procedure TBrookStreamReader.ReadBytes(out ALine: TBytes);
var
  VBuf: PByte;
  VPos, VLen, VCount: Integer;
begin
  VPos := FBufferPosition;
  ALine := nil;
  repeat
    VBuf := @FBuffer[FBufferPosition];
    while (FBufferPosition < FBufferRead) and not (VBuf^ in [10, 13]) do
    begin
      Inc(VBuf);
      Inc(FBufferPosition);
    end;
    if FBufferPosition = FBufferRead then
    begin
      VCount := FBufferPosition - VPos;
      if VCount > 0 then
      begin
        VLen := Length(ALine);
        SetLength(ALine, VLen + VCount);
        Move(FBuffer[VPos], ALine[VLen], VCount);
      end;
      FillBuffer;
      VPos := FBufferPosition;
    end;
  until (FBufferPosition = FBufferRead) or (VBuf^ in [10, 13]);
  VCount := FBufferPosition - VPos;
  if VCount > 0 then
  begin
    VLen := Length(ALine);
    SetLength(ALine, VLen + VCount);
    Move(FBuffer[VPos], ALine[VLen], VCount);
  end;
  if (VBuf^ in [10, 13]) and (FBufferPosition < FBufferRead) then
  begin
    Inc(FBufferPosition);
    if VBuf^ = 13 then
    begin
      if FBufferPosition = FBufferRead then
        FillBuffer;
      if (FBufferPosition < FBufferRead) and (FBuffer[FBufferPosition] = 10) then
        Inc(FBufferPosition);
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

function TBrookBaseReader.IsEOF: Boolean;
begin
  Result := ProxyReader.IsEOF;
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
  const AString: string; ABufferSize: Integer);
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
    AString{$IFNDEF FPC}, VEncoding{$ENDIF}), ABufferSize, True);
end;

constructor TBrookStringReader.Create(AEncoding: TEncoding;
  const AString: string);
begin
  Create(AEncoding, AString, BROOK_BUFFER_SIZE);
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
  ABufferSize: Integer);
begin
  inherited Create;
  FProxyReader := TBrookStreamReader.Create(AEncoding,
    TFileStream.Create(AFileName, AMode, ARights), ABufferSize, True);
end;

constructor TBrookFileReader.Create(AEncoding: TEncoding;
  const AFileName: TFileName; AMode: Word; ABufferSize: Integer);
begin
  Create(AEncoding, AFileName, AMode, BROOK_FILE_RIGHTS, ABufferSize);
end;

constructor TBrookFileReader.Create(AEncoding: TEncoding;
  const AFileName: TFileName; ABufferSize: Integer);
begin
  Create(AEncoding, AFileName, fmOpenRead or fmShareDenyWrite, ABufferSize);
end;

constructor TBrookFileReader.Create(AEncoding: TEncoding;
  const AFileName: TFileName);
begin
  Create(AEncoding, AFileName, BROOK_BUFFER_SIZE);
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
