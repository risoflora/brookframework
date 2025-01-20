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

program Test_Utility;

{$I Tests.inc}

uses
  RTLConsts,
  SysUtils,
  DateUtils,
  Classes,
  SyncObjs,
{$IFNDEF FPC}
  IOUtils,
  Hash,
{$ENDIF}
  Platform,
  libsagui,
  BrookLibraryLoader,
  BrookUtility,
  Test;

type
  TFakeMutex = class(TCriticalSection)
    Muted: Boolean;
    procedure Acquire; override;
    procedure Release; override;
  end;

  TFakeLocker = class(TBrookLocker)
  protected
    function CreateMutex: TCriticalSection; override;
  public
    property Mutex;
  end;

{ TFakeMutex }

procedure TFakeMutex.Acquire;
begin
  Muted := True;
end;

procedure TFakeMutex.Release;
begin
  Muted := False;
end;

{ TFakeLocker }

function TFakeLocker.CreateMutex: TCriticalSection;
begin
  Result := TFakeMutex.Create;
end;

procedure Test_LockerCreate;
var
  L: TFakeLocker;
begin
  L := TFakeLocker.Create;
  try
    Assert(Assigned(L.Mutex));
    Assert(L.Active);
  finally
    L.Free;
  end;
end;

procedure Test_LockerLock;
var
  L: TFakeLocker;
  M: TFakeMutex;
begin
  L := TFakeLocker.Create;
  try
    M := TFakeMutex(L.Mutex);
    Assert(not M.Muted);
    L.Lock;
    Assert(M.Muted);
    L.Active := False;
    M.Muted := False;
    L.Lock;
    Assert(not M.Muted);
  finally
    L.Free;
  end;
end;

procedure Test_LockerUnlock;
var
  L: TFakeLocker;
  M: TFakeMutex;
begin
  L := TFakeLocker.Create;
  try
    M := TFakeMutex(L.Mutex);
    M.Muted := True;
    L.Unlock;
    Assert(not M.Muted);
    L.Active := False;
    M.Muted := True;
    L.Unlock;
    Assert(M.Muted);
  finally
    L.Free;
  end;
end;

procedure Test_LockerTryLock;
var
  L: TBrookLocker;
begin
  L := TBrookLocker.Create;
  try
    L.Active := False;
    Assert(not L.TryLock);
    L.Active := True;
    Assert(L.TryLock);
  finally
    L.Free;
  end;
end;

procedure Test_LockerActive;
var
  L: TFakeLocker;
begin
  L := TFakeLocker.Create;
  try
    Assert(L.Active);
    L.Active := False;
    Assert(not L.Active);
    L.Active := True;
    Assert(L.Active);
  finally
    L.Free;
  end;
end;

procedure Test_SaguiVersion;
begin
  Assert(Sagui.Version = (SG_VERSION_MAJOR shl 16) or
    (SG_VERSION_MINOR shl 8) or SG_VERSION_PATCH);
  Assert(Sagui.VersionStr = Format('%d.%d.%d', [SG_VERSION_MAJOR,
    SG_VERSION_MINOR, SG_VERSION_PATCH]));
end;

procedure Test_SaguiMalloc;
const
  TEST_MEM_BUF_LEN = 10;
var
  B: Pcchar;
begin
  B := Sagui.Malloc(TEST_MEM_BUF_LEN);
  Assert(Assigned(B));
  FillChar(B[0], Pred(TEST_MEM_BUF_LEN), 'a');
  B[TEST_MEM_BUF_LEN - 1] := #0;
  Assert(Length(B) = Pred(TEST_MEM_BUF_LEN));
  Sagui.Free(B);
end;

procedure Test_SaguiAlloc;
const
  TEST_MEM_BUF_LEN = 10;
var
  B: Pcchar;
  I: Integer;
begin
  B := Sagui.Alloc(TEST_MEM_BUF_LEN);
  Assert(Assigned(B));
  for I := 0 to Pred(TEST_MEM_BUF_LEN) do
    Assert(B[I] = #0);
  FillChar(B[0], Pred(TEST_MEM_BUF_LEN), 'a');
  B[TEST_MEM_BUF_LEN - 1] := #0;
  Assert(Length(B) = Pred(TEST_MEM_BUF_LEN));
  Sagui.Free(B);
end;

procedure Test_SaguiRealloc;
const
  TEST_MEM_BUF_LEN = 10;
var
  B: Pcchar;
begin
  B := Sagui.Alloc(TEST_MEM_BUF_LEN div 2);
  Assert(Assigned(B));
  B := Sagui.Realloc(B, TEST_MEM_BUF_LEN);
  Assert(Assigned(B));
  FillChar(B[0], Pred(TEST_MEM_BUF_LEN), 'a');
  B[TEST_MEM_BUF_LEN - 1] := #0;
  Assert(Length(B) = Pred(TEST_MEM_BUF_LEN));
  Sagui.Free(B);
end;

procedure Test_SaguiFree;
begin
  Sagui.Free(nil);
end;

procedure Test_SaguiStrError;
var
  E: string;
begin
  Sagui.StrError(0, E, 0);
  Assert(E = '');
  Sagui.StrError(EINVAL, E, SG_ERR_SIZE);
  Assert(E = 'Invalid argument');
end;

procedure Test_SaguiIsPost;
begin
  Assert(not Sagui.IsPost(''));
  Assert(not Sagui.IsPost('abc'));
  Assert(not Sagui.IsPost('GET'));
  Assert(not Sagui.IsPost('HEAD'));
  Assert(Sagui.IsPost('POST'));
  Assert(Sagui.IsPost('PUT'));
  Assert(Sagui.IsPost('DELETE'));
  Assert(Sagui.IsPost('OPTIONS'));
end;

procedure Test_SaguiExtractEntryPoint;
begin
  Assert(Sagui.ExtractEntryPoint('') = '/');
  Assert(Sagui.ExtractEntryPoint('/') = '/');
  Assert(Sagui.ExtractEntryPoint('//') = '/');
  Assert(Sagui.ExtractEntryPoint('///////') = '/');
  Assert(Sagui.ExtractEntryPoint('foo') = '/foo');
  Assert(Sagui.ExtractEntryPoint('/foo') = '/foo');
  Assert(Sagui.ExtractEntryPoint('//foo') = '/foo');
  Assert(Sagui.ExtractEntryPoint('///////foo') = '/foo');
  Assert(Sagui.ExtractEntryPoint('foo/') = '/foo');
  Assert(Sagui.ExtractEntryPoint('foo//') = '/foo');
  Assert(Sagui.ExtractEntryPoint('/foo/') = '/foo');
  Assert(Sagui.ExtractEntryPoint('///foo///') = '/foo');
  Assert(Sagui.ExtractEntryPoint('/foo/bar') = '/foo');
  Assert(Sagui.ExtractEntryPoint('///foo/bar') = '/foo');
  Assert(Sagui.ExtractEntryPoint('/foo///bar') = '/foo');
  Assert(Sagui.ExtractEntryPoint('///foo///bar') = '/foo');
  Assert(Sagui.ExtractEntryPoint('/a') = '/a');
  Assert(Sagui.ExtractEntryPoint('/a/b') = '/a');
  Assert(Sagui.ExtractEntryPoint('//a/b') = '/a');
  Assert(Sagui.ExtractEntryPoint('//a//b') = '/a');
end;

procedure Test_SaguiTmpDir;
begin
{$IFDEF ANDROID}
  Assert(Sagui.TmpDir = '/data/local/tmp');
{$ELSE}
  Assert(IncludeTrailingPathDelimiter(Sagui.TmpDir) =
    {$IFDEF FPC}GetTempDir{$ELSE}TPath.GetTempPath{$ENDIF});
{$ENDIF}
end;

procedure DoSaguiIPParamIsNil;
begin
  Sagui.IP(nil);
end;

procedure Test_SaguiIP;
begin
  AssertExcept(DoSaguiIPParamIsNil, EArgumentNilException,
    Format(SParamIsNil, ['ASocket']));
end;

procedure Test_BrookDAYS;
begin
  Assert(Brook.DAYS[1] = 'Sun');
  Assert(Brook.DAYS[2] = 'Mon');
  Assert(Brook.DAYS[3] = 'Tue');
  Assert(Brook.DAYS[4] = 'Wed');
  Assert(Brook.DAYS[5] = 'Thu');
  Assert(Brook.DAYS[6] = 'Fri');
  Assert(Brook.DAYS[7] = 'Sat');
end;

procedure Test_BrookMONTHS;
begin
  Assert(Brook.MONTHS[1] = 'Jan');
  Assert(Brook.MONTHS[2] = 'Feb');
  Assert(Brook.MONTHS[3] = 'Mar');
  Assert(Brook.MONTHS[4] = 'Apr');
  Assert(Brook.MONTHS[5] = 'May');
  Assert(Brook.MONTHS[6] = 'Jun');
  Assert(Brook.MONTHS[7] = 'Jul');
  Assert(Brook.MONTHS[8] = 'Aug');
  Assert(Brook.MONTHS[9] = 'Sep');
  Assert(Brook.MONTHS[10] = 'Oct');
  Assert(Brook.MONTHS[11] = 'Nov');
  Assert(Brook.MONTHS[12] = 'Dec');
end;

procedure Test_BrookFixPath;
begin
  Assert(Brook.FixPath('') = '/');
  Assert(Brook.FixPath('/') = '/');
  Assert(Brook.FixPath('abc') = '/abc');
  Assert(Brook.FixPath('/abc') = '/abc');
  Assert(Brook.FixPath('abc/') = '/abc');
  Assert(Brook.FixPath('/abc/') = '/abc');
  Assert(Brook.FixPath('abc/def') = '/abc/def');
end;

procedure Test_BrookFixEntryPoint;
begin
  Assert(Brook.FixEntryPoint('') = '/');
  Assert(Brook.FixEntryPoint('/') = '/');
  Assert(Brook.FixEntryPoint('abc') = '/abc');
  Assert(Brook.FixEntryPoint('/abc') = '/abc');
  Assert(Brook.FixEntryPoint('abc/') = '/abc');
  Assert(Brook.FixEntryPoint('/abc/') = '/abc');
  Assert(Brook.FixEntryPoint('abc/def') = '/abc');
  Assert(Brook.FixEntryPoint('/abc/def') = '/abc');
  Assert(Brook.FixEntryPoint('abc/def/') = '/abc');
  Assert(Brook.FixEntryPoint('/abc/def/') = '/abc');
end;

procedure Test_BrookDateTimeToUTC;
begin
  Assert(Brook.DateTimeToUTC(Now) > 0);
end;

procedure Test_BrookDateTimeToGMT;
begin
  Assert(Brook.DateTimeToGMT(EncodeDateTime(2019, 11, 29, 14, 27, 52, 0)) =
    Concat(Brook.DAYS[6], ', 29 Nov 2019 14:27:52 GMT'));
end;

procedure Test_BrookSHA1;
begin
  Assert(Brook.SHA1('abc123') = '6367c48dd193d56ea7b0baad25b19455e529f5ee');
end;

procedure Test_HTTPRequestMethodHelperToString;
var
  M: TBrookHTTPRequestMethod;
begin
  M := rmUnknown;
  Assert(M.ToString = 'Unknown');
  M := rmGET;
  Assert(M.ToString = 'GET');
  M := rmPOST;
  Assert(M.ToString = 'POST');
  M := rmPUT;
  Assert(M.ToString = 'PUT');
  M := rmDELETE;
  Assert(M.ToString = 'DELETE');
  M := rmPATCH;
  Assert(M.ToString = 'PATCH');
  M := rmOPTIONS;
  Assert(M.ToString = 'OPTIONS');
  M := rmHEAD;
  Assert(M.ToString = 'HEAD');
end;

procedure Test_HTTPRequestMethodHelperFromString;
var
  M: TBrookHTTPRequestMethod;
begin
  M := Default(TBrookHTTPRequestMethod);
  Assert(M.FromString('Unknown') = rmUnknown);
  Assert(M.FromString('GET') = rmGET);
  Assert(M.FromString('POST') = rmPOST);
  Assert(M.FromString('PUT') = rmPUT);
  Assert(M.FromString('DELETE') = rmDELETE);
  Assert(M.FromString('PATCH') = rmPATCH);
  Assert(M.FromString('OPTIONS') = rmOPTIONS);
  Assert(M.FromString('HEAD') = rmHEAD);
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  Test_LockerCreate;
  // Test_LockerDestroy - not required
  Test_LockerLock;
  Test_LockerUnlock;
  Test_LockerTryLock;
  Test_LockerActive;
  Test_SaguiVersion;
  Test_SaguiMalloc;
  Test_SaguiAlloc;
  Test_SaguiRealloc;
  Test_SaguiFree;
  Test_SaguiStrError;
  Test_SaguiIsPost;
  Test_SaguiExtractEntryPoint;
  Test_SaguiTmpDir;
  // Test_SaguiEOR; - not easy to test
  Test_SaguiIP;
  Test_BrookDAYS;
  Test_BrookMONTHS;
  Test_BrookFixPath;
  Test_BrookFixEntryPoint;
  Test_BrookDateTimeToUTC;
  Test_BrookDateTimeToGMT;
  Test_BrookSHA1;
  Test_HTTPRequestMethodHelperToString;
  Test_HTTPRequestMethodHelperFromString;
end.
