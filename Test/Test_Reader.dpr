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

program Test_Reader;

{$I Tests.inc}

{$IF DEFINED(FPC) AND DEFINED(MSWINDOWS)}
 {$CODEPAGE UTF8}
{$ENDIF}

uses
  RTLConsts,
  SysUtils,
  Classes,
  BrookReader,
  Test;

const
  AI_19_CRLF = 'ABÇ'#13#10'dÉf'#13#10'GHÏ'#13#10'123'#13#10'456'#13#10'789';
  AI_19_FILE = 'TextFile.txt';

type
  TMyStreamReader = class(TBrookStreamReader)
  end;

{ General }

procedure Test_Encoding(AReader: TBrookTextReader);
var
  VBytes: TBytes;
begin
  Assert(Assigned(AReader.Encoding));
  VBytes := AReader.Encoding.GetBytes(AI_19_CRLF);
  Assert(AReader.Encoding.GetString(VBytes) = AI_19_CRLF);
  AReader.Encoding := nil;
  Assert(not Assigned(AReader.Encoding));
  AReader.Encoding := TEncoding.UTF8;
  Assert(Assigned(AReader.Encoding));
  Assert(AReader.Encoding.GetString(VBytes) = AI_19_CRLF);
end;

procedure Test_Reset(AReader: TBrookTextReader);
begin
  Assert(UnicodeString(AReader.Read) = 'ABÇ');
  Assert(UnicodeString(AReader.Read) = 'dÉf');
  Assert(UnicodeString(AReader.Read) = 'GHÏ');
  AReader.Reset;
  Assert(UnicodeString(AReader.Read) = 'ABÇ');
  Assert(UnicodeString(AReader.Read) = 'dÉf');
  Assert(UnicodeString(AReader.Read) = 'GHÏ');
end;

procedure Test_IsEOF(AReader: TBrookTextReader);
begin
  AReader.Reset;
  Assert(not AReader.EOF);
  AReader.ReadBytes;
  AReader.ReadBytes;
  AReader.ReadBytes;
  AReader.ReadBytes;
  AReader.ReadBytes;
  AReader.ReadBytes;
  Assert(AReader.EOF);
  AReader.Reset;
  Assert(not AReader.EOF);
  AReader.Read;
  AReader.Read;
  AReader.Read;
  AReader.Read;
  AReader.Read;
  AReader.Read;
  Assert(AReader.EOF);
end;

procedure Test_Close(AReader: TBrookTextReader);
begin
  AReader.Reset;
  AReader.Close;
  Assert(AReader.IsEOF);
end;

procedure Test_Read(AReader: TBrookTextReader);
var
  S: string;
begin
  Assert(UnicodeString(AReader.Read) = 'ABÇ');
  Assert(UnicodeString(AReader.Read) = 'dÉf');
  Assert(UnicodeString(AReader.Read) = 'GHÏ');
  Assert(AReader.Read = '123');
  Assert(AReader.Read = '456');
  Assert(AReader.Read = '789');
  Assert(AReader.EOF);
  AReader.Reset;
  AReader.Read(S);
  Assert('ABÇ' = UnicodeString(S));
  AReader.Read(S);
  Assert('dÉf' = UnicodeString(S));
  AReader.Read(S);
  Assert('GHÏ' = UnicodeString(S));
  AReader.Read(S);
  Assert('123' = S);
  AReader.Read(S);
  Assert('456' = S);
  AReader.Read(S);
  Assert('789' = S);
  Assert(AReader.EOF);
end;

procedure Test_ReadBytes(AReader: TBrookTextReader);
var
  B: TBytes;
begin
  AReader.ReadBytes(B);
  Assert(TEncoding.UTF8.GetString(B) = 'ABÇ');
  AReader.ReadBytes(B);
  Assert(TEncoding.UTF8.GetString(B) = 'dÉf');
  AReader.ReadBytes(B);
  Assert(TEncoding.UTF8.GetString(B) = 'GHÏ');
  AReader.ReadBytes(B);
  Assert(string(TEncoding.UTF8.GetString(B)) = '123');
  AReader.ReadBytes(B);
  Assert(string(TEncoding.UTF8.GetString(B)) = '456');
  AReader.ReadBytes(B);
  Assert(string(TEncoding.UTF8.GetString(B)) = '789');
  Assert(AReader.EOF);
  AReader.Reset;
  Assert(TEncoding.UTF8.GetString(AReader.ReadBytes) = 'ABÇ');
  Assert(TEncoding.UTF8.GetString(AReader.ReadBytes) = 'dÉf');
  Assert(TEncoding.UTF8.GetString(AReader.ReadBytes) = 'GHÏ');
  Assert(string(TEncoding.UTF8.GetString(AReader.ReadBytes)) = '123');
  Assert(string(TEncoding.UTF8.GetString(AReader.ReadBytes)) = '456');
  Assert(string(TEncoding.UTF8.GetString(AReader.ReadBytes)) = '789');
  Assert(AReader.EOF);
end;

procedure Test_Create(AReader: TBrookTextReader);
begin
  Assert(Assigned(AReader.Encoding));
  Assert(AReader.Encoding.ClassType = TUTF8Encoding);
  Assert(not AReader.IsEOF);
  Assert(TEncoding.UTF8.GetByteCount(AI_19_CRLF) = 31);
end;

{ TTestTStreamReader }

procedure DoStreamReaderCreateNilStream1;
begin
  TMyStreamReader.Create(nil, nil, 0, False);
end;

procedure DoStreamReaderCreateNilStream2;
begin
  TMyStreamReader.Create(nil, nil);
end;

procedure DoStreamReaderCreateNilStream3;
begin
  TMyStreamReader.Create(nil);
end;

procedure Test_StreamReaderCreate;
var
  VStream: TStream;
  VReader: TMyStreamReader;
begin
  VReader := TMyStreamReader.Create(nil, TBytesStream.Create, 0, True);
  try
    Assert(not Assigned(VReader.Encoding));
  finally
    VReader.Free;
  end;
  VReader := TMyStreamReader.Create(TEncoding.UTF8, TBytesStream.Create, 0, True);
  try
    Assert(Assigned(VReader.Encoding));
    Assert(Assigned(VReader.Stream));
    Assert(VReader.Stream.ClassType = TBytesStream);
    Assert(VReader.OwnsStream);
  finally
    VReader.Free;
  end;
  VReader := TMyStreamReader.Create(nil, TBytesStream.Create, 512, True);
  try
    Assert(not Assigned(VReader.Encoding));
  finally
    VReader.Free;
  end;
  VReader := TMyStreamReader.Create(TEncoding.UTF8, TBytesStream.Create, 512, True);
  try
    Assert(High(VReader.Buffer) = 511);
  finally
    VReader.Free;
  end;
  VStream := TBytesStream.Create;
  VReader := TMyStreamReader.Create(nil, VStream);
  try
    Assert(not Assigned(VReader.Encoding));
  finally
    VStream.Free;
    VReader.Free;
  end;
  VStream := TBytesStream.Create;
  VReader := TMyStreamReader.Create(TEncoding.UTF8, VStream);
  try
    Assert(VReader.Encoding.ClassType = TUTF8Encoding);
    Assert(not VReader.OwnsStream);
  finally
    VStream.Free;
    VReader.Free;
  end;
  VStream := TBytesStream.Create;
  VReader := TMyStreamReader.Create(VStream);
  try
    Assert(not VReader.OwnsStream);
    Assert(Assigned(VReader.Stream));
    Assert(VReader.Encoding = TEncoding.UTF8);
  finally
    VStream.Free;
    VReader.Free;
  end;

  AssertExcept(DoStreamReaderCreateNilStream1, EArgumentNilException,
    Format(SParamIsNil, ['AStream']));
  AssertExcept(DoStreamReaderCreateNilStream2, EArgumentNilException,
    Format(SParamIsNil, ['AStream']));
  AssertExcept(DoStreamReaderCreateNilStream3, EArgumentNilException,
    Format(SParamIsNil, ['AStream']));
end;

procedure Test_StreamReaderReset;
var
  VBytes: TBytes;
  VStream: TStream;
  VReader: TBrookTextReader;
begin
  VStream := TBytesStream.Create;
  VReader := TBrookStreamReader.Create(VStream);
  try
    VBytes := TEncoding.UTF8.GetBytes(AI_19_CRLF);
    VStream.Write(VBytes[0], TEncoding.UTF8.GetByteCount(AI_19_CRLF));
    VStream.Seek(0, TSeekOrigin.soBeginning);
    Test_Reset(VReader);
  finally
    VStream.Free;
    VReader.Free;
  end;
end;

procedure Test_StreamReaderClose;
var
  VStream: TStream;
  VReader: TBrookTextReader;
begin
  VStream := TBytesStream.Create;
  VReader := TBrookStreamReader.Create(nil, VStream, 0, True);
  try
    Assert(Assigned(VStream));
    VReader.Close;
    Assert(VReader.IsEOF);
    VStream := nil;
    Assert(not Assigned(VStream));
  finally
    VReader.Free;
  end;
end;

procedure Test_StreamReaderIsEOF;
var
  VStream: TStream;
  VReader: TBrookTextReader;
begin
  VStream := TStringStream.Create(AI_19_CRLF, TEncoding.UTF8);
  VReader := TBrookStreamReader.Create(VStream);
  try
    Test_IsEOF(VReader);
  finally
    VStream.Free;
    VReader.Free;
  end;
end;

procedure Test_StreamReaderReadBytes;
var
  VStream: TStream;
  VReader: TBrookTextReader;
begin
  VStream := TStringStream.Create(AI_19_CRLF, TEncoding.UTF8);
  VReader := TBrookStreamReader.Create(VStream);
  try
    Test_ReadBytes(VReader);
  finally
    VStream.Free;
    VReader.Free;
  end;
end;

procedure Test_StreamReaderRead;
var
  VStream: TStream;
  VReader: TBrookTextReader;
begin
  VStream := TStringStream.Create(AI_19_CRLF, TEncoding.UTF8);
  VReader := TBrookStreamReader.Create(VStream);
  try
    Test_Read(VReader);
  finally
    VStream.Free;
    VReader.Free;
  end;
end;

procedure Test_StreamReaderEncoding;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookStreamReader.Create(TEncoding.UTF8,
    TBytesStream.Create, 0, True);
  try
    Test_Encoding(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_StreamReaderStream;
var
  VStream: TStream;
  VReader: TBrookStreamReader;
begin
  VStream := TBytesStream.Create;
  VReader := TBrookStreamReader.Create(VStream);
  try
    Assert(Assigned(VReader.Stream));
    Assert(VReader.Stream.ClassType = TBytesStream);
  finally
    VStream.Free;
    VReader.Free;
  end;
end;

procedure Test_StreamReaderOwnsStream;
var
  VStream: TStream;
  VReader: TBrookTextReader;
begin
  VStream := TBytesStream.Create;
  VReader := TBrookStreamReader.Create(nil, VStream, 0, True);
  VReader.Free;
  VStream := nil;
  Assert(not Assigned(VStream));
  VStream := TBytesStream.Create;
  VReader := TBrookStreamReader.Create(nil, VStream);
  try
    Assert(Assigned(VStream));
  finally
    VStream.Free;
    VReader.Free;
  end;
end;

{ TTestTStringReader }

procedure Test_StringReaderCreate;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookStringReader.Create(TEncoding.UTF8, AI_19_CRLF, 1024);
  try
    Test_Create(VReader);
  finally
    VReader.Free;
  end;
  VReader := TBrookStringReader.Create(nil, AI_19_CRLF, 1024);
  try
    Assert(not Assigned(VReader.Encoding));
  finally
    VReader.Free;
  end;
  VReader := TBrookStringReader.Create(TEncoding.UTF8, AI_19_CRLF);
  try
    Test_Create(VReader);
  finally
    VReader.Free;
  end;
  VReader := TBrookStringReader.Create(nil, AI_19_CRLF);
  try
    Assert(not Assigned(VReader.Encoding));
  finally
    VReader.Free;
  end;
  VReader := TBrookStringReader.Create(AI_19_CRLF);
  try
    Assert(VReader.Encoding = TEncoding.UTF8);
  finally
    VReader.Free;
  end;
end;

procedure Test_StringReaderReset;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookStringReader.Create(AI_19_CRLF);
  try
    Test_Reset(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_StringReaderClose;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookStringReader.Create(nil, AI_19_CRLF);
  try
    Test_Close(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_StringReaderIsEOF;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookStringReader.Create(AI_19_CRLF);
  try
    Test_IsEOF(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_StringReaderReadBytes;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookStringReader.Create(AI_19_CRLF);
  try
    Test_ReadBytes(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_StringReaderRead;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookStringReader.Create(AI_19_CRLF);
  try
    Test_Read(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_StringReaderEncoding;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookStringReader.Create(AI_19_CRLF);
  try
    Test_Encoding(VReader);
  finally
    VReader.Free;
  end;
end;

{ TTestTFileReader }

procedure Test_FileReaderCreate;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookFileReader.Create(TEncoding.UTF8, AI_19_FILE,
    fmOpenRead, 438, 1024);
  try
    Test_Create(VReader);
  finally
    VReader.Free;
  end;
  VReader := TBrookFileReader.Create(nil, AI_19_FILE, fmOpenRead, 438, 1024);
  try
    Assert(not Assigned(VReader.Encoding));
  finally
    VReader.Free;
  end;
  VReader := TBrookFileReader.Create(TEncoding.UTF8, AI_19_FILE, fmOpenRead, 1024);
  try
    Test_Create(VReader);
  finally
    VReader.Free;
  end;
  VReader := TBrookFileReader.Create(nil, AI_19_FILE, fmOpenRead, 1024);
  try
    Assert(not Assigned(VReader.Encoding));
  finally
    VReader.Free;
  end;
  VReader := TBrookFileReader.Create(TEncoding.UTF8, AI_19_FILE, 1024);
  try
    Test_Create(VReader);
  finally
    VReader.Free;
  end;
  VReader := TBrookFileReader.Create(nil, AI_19_FILE, 1024);
  try
    Assert(not Assigned(VReader.Encoding));
  finally
    VReader.Free;
  end;
  VReader := TBrookFileReader.Create(TEncoding.UTF8, AI_19_FILE);
  try
    Test_Create(VReader);
  finally
    VReader.Free;
  end;
  VReader := TBrookFileReader.Create(nil, AI_19_FILE);
  try
    Assert(not Assigned(VReader.Encoding));
  finally
    VReader.Free;
  end;
  VReader := TBrookFileReader.Create(AI_19_FILE);
  try
    Assert(VReader.Encoding = TEncoding.UTF8);
  finally
    VReader.Free;
  end;
end;

procedure Test_FileReaderReset;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookFileReader.Create(AI_19_FILE);
  try
    Test_Reset(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_FileReaderClose;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookFileReader.Create(nil, AI_19_FILE);
  try
    Test_Close(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_FileReaderIsEOF;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookFileReader.Create(AI_19_FILE);
  try
    Test_IsEOF(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_FileReaderReadBytes;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookFileReader.Create(nil, AI_19_FILE);
  try
    Test_ReadBytes(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_FileReaderRead;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookFileReader.Create(AI_19_FILE);
  try
    Test_Read(VReader);
  finally
    VReader.Free;
  end;
end;

procedure Test_TextReaderEncoding;
var
  VReader: TBrookTextReader;
begin
  VReader := TBrookFileReader.Create(AI_19_FILE);
  try
    Test_Encoding(VReader);
  finally
    VReader.Free;
  end;
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Test_StreamReaderCreate;
  Test_StreamReaderReset;
  Test_StreamReaderClose;
  Test_StreamReaderIsEOF;
  Test_StreamReaderReadBytes;
  Test_StreamReaderRead;
  Test_StreamReaderEncoding;
  Test_StreamReaderStream;
  Test_StreamReaderOwnsStream;
  Test_StringReaderCreate;
  Test_StringReaderReset;
  Test_StringReaderClose;
  Test_StringReaderIsEOF;
  Test_StringReaderReadBytes;
  Test_StringReaderRead;
  Test_StringReaderEncoding;
  Test_FileReaderCreate;
  Test_FileReaderReset;
  Test_FileReaderClose;
  Test_FileReaderIsEOF;
  Test_FileReaderReadBytes;
  Test_FileReaderRead;
  Test_TextReaderEncoding;
end.
