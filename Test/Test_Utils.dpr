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

program Test_Utils;

{$I Tests.inc}

uses
  SysUtils,
{$IFNDEF FPC}
  IOUtils,
{$ENDIF}
  Platform,
  libsagui,
  BrookUtility;

procedure Test_Version;
begin
  Assert(Sagui.Version = (SG_VERSION_MAJOR shl 16) or
    (SG_VERSION_MINOR shl 8) or SG_VERSION_PATCH);
  Assert(Sagui.VersionStr = Format('%d.%d.%d', [SG_VERSION_MAJOR,
    SG_VERSION_MINOR, SG_VERSION_PATCH]));
end;

procedure Test_Malloc;
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

procedure Test_Alloc;
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

procedure Test_Realloc;
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

procedure Test_Free;
begin
  Sagui.Free(nil);
end;

procedure Test_StrError;
var
  E: string;
begin
  Sagui.StrError(0, E, 0);
  Assert(E = '');
  Sagui.StrError(EINVAL, E, SG_ERR_SIZE);
  Assert(E = 'Invalid argument');
end;

procedure Test_IsPost;
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

procedure Test_ExtractEntryPoint;
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

procedure Test_TmpDir;
begin
{$IFDEF ANDROID}
  Assert(Sagui.TmpDir = '/data/local/tmp');
{$ELSE}
  Assert(IncludeTrailingPathDelimiter(Sagui.TmpDir) =
    {$IFDEF FPC}GetTempDir{$ELSE}TPath.GetTempPath{$ENDIF});
{$ENDIF}
end;

procedure Test_EOR;
begin
  Assert(Sagui.EOR(False) = -1);
  Assert(Sagui.EOR(True) = -2);
end;

procedure Test_FixPath;
begin
end;

begin
  SgLib.Load(SG_LIB_NAME);
  Test_Version;
  Test_Malloc;
  Test_Alloc;
  Test_Realloc;
  Test_Free;
  Test_StrError;
  Test_IsPost;
  Test_ExtractEntryPoint;
  Test_TmpDir;
  Test_EOR;
  Test_FixPath;
end.
