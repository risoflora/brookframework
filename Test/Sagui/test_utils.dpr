(*                         _
 *   ___  __ _  __ _ _   _(_)
 *  / __|/ _` |/ _` | | | | |
 *  \__ \ (_| | (_| | |_| | |
 *  |___/\__,_|\__, |\__,_|_|
 *             |___/
 *
 * Cross-platform library which helps to develop web servers or frameworks.
 *
 * Copyright (c) 2016-2019 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Sagui library.
 *
 * Sagui library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Sagui library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Sagui library.  If not, see <http://www.gnu.org/licenses/>.
 *)

program test_utils;

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Platform,
  libc,
  libsagui;

procedure test_version;
var
  ver_original: Pcchar;
  ver_local: array [0..8] of cchar;
  ver_len: csize_t;
begin
  Assert(sg_version() > 0);

  ver_original := sg_version_str();
  Assert(Assigned(ver_original));
  ver_len := strlen(ver_original);
  Assert(ver_len > 0);

  sprintf(@ver_local[0], '%d.%d.%d', SG_VERSION_MAJOR, SG_VERSION_MINOR,
    SG_VERSION_PATCH);
  Assert(strcmp(@ver_original[0], @ver_local[0]) = 0);

  Assert(ver_original[ver_len] = #0);
end;

procedure test_malloc;
const
  TEST_MEM_BUF_LEN = 10;
var
  buf: Pcchar;
begin
  buf := sg_malloc(TEST_MEM_BUF_LEN);
  Assert(Assigned(buf));
  memset(buf, Ord('a'), TEST_MEM_BUF_LEN - 1);
  buf[TEST_MEM_BUF_LEN - 1] := #0;
  Assert(strlen(buf) = TEST_MEM_BUF_LEN - 1);
  sg_free(buf);
end;

procedure test_alloc;
const
  TEST_MEM_BUF_LEN = 10;
var
  buf: Pcchar;
  i: cint;
begin
  buf := sg_alloc(TEST_MEM_BUF_LEN);
  Assert(Assigned(buf));
  for i := 0 to Pred(TEST_MEM_BUF_LEN) do
    Assert(buf[i] = #0);
  memset(buf, Ord('a'), TEST_MEM_BUF_LEN - 1);
  buf[TEST_MEM_BUF_LEN - 1] := #0;
  Assert(strlen(buf) = TEST_MEM_BUF_LEN - 1);
  sg_free(buf);
end;

procedure test_realloc;
const
  TEST_MEM_BUF_LEN = 10;
var
  buf: Pcchar;
begin
  buf := sg_alloc(TEST_MEM_BUF_LEN div 2);
  ASSERT(Assigned(buf));
  buf := sg_realloc(buf, TEST_MEM_BUF_LEN);
  Assert(Assigned(buf));
  memset(buf, Ord('a'), TEST_MEM_BUF_LEN - 1);
  buf[TEST_MEM_BUF_LEN - 1] := #0;
  Assert(strlen(buf) = TEST_MEM_BUF_LEN - 1);
  sg_free(buf);
end;

procedure test_free;
begin
  sg_free(nil);
end;

procedure test_strerror;
var
  err: array [0..255] of cchar;
begin
  Assert(not Assigned(sg_strerror(0, nil, SizeOf(err))));
  Assert(not Assigned(sg_strerror(0, @err[0], 0)));
  Assert(sg_strerror(EINTR, @err[0], sizeof(err)) = @err[0]);
  Assert(strcmp(@err[0], strerror(EINTR)) = 0);
  Assert(sg_strerror(EINVAL, @err[0], sizeof(err)) = @err[0]);
  Assert(strcmp(@err[0], strerror(EINVAL)) = 0);
end;

procedure test_is_post;
begin
  Assert(not sg_is_post(nil));
  Assert(not sg_is_post(''));
  Assert(not sg_is_post('abc'));
  Assert(not sg_is_post('GET'));
  Assert(not sg_is_post('HEAD'));
  Assert(sg_is_post('POST'));
  Assert(sg_is_post('PUT'));
  Assert(sg_is_post('DELETE'));
  Assert(sg_is_post('OPTIONS'));
end;

procedure test_extract_entrypoint;
var
  str: Pcchar;
begin
  seterrno(0);
  Assert(not Assigned(sg_extract_entrypoint(nil)));
  Assert(errno = EINVAL);

  str := sg_extract_entrypoint('');
  Assert(Assigned(str));
  Assert(strcmp(str, '/') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('/');
  Assert(Assigned(str));
  Assert(strcmp(str, '/') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('//');
  Assert(Assigned(str));
  Assert(strcmp(str, '/') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('///////');
  Assert(Assigned(str));
  Assert(strcmp(str, '/') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('foo');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('/foo');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('//foo');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('///////foo');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('foo/');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('foo//');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('/foo/');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('///foo///');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('/foo/bar');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('///foo/bar');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('/foo///bar');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('///foo///bar');
  Assert(Assigned(str));
  Assert(strcmp(str, '/foo') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('/a');
  Assert(Assigned(str));
  Assert(strcmp(str, '/a') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('/a/b');
  Assert(Assigned(str));
  Assert(strcmp(str, '/a') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('//a/b');
  Assert(Assigned(str));
  Assert(strcmp(str, '/a') = 0);
  sg_free(str);
  str := sg_extract_entrypoint('//a//b');
  Assert(Assigned(str));
  Assert(strcmp(str, '/a') = 0);
  sg_free(str);
end;

procedure test_tmpdir;
var
{$IFDEF MSWINDOWS}
  path: array [0..MAX_PATH] of cchar;
  len: csize_t;
{$ENDIF}
  tmp: Pcchar;
begin
  tmp := sg_tmpdir();
  Assert(Assigned(tmp));
{$IF DEFINED(MSWINDOWS)}
  len := GetTempPathA(MAX_PATH + 1, @path[0]);
  if path[len - 1] = Ord('\') then
  begin
    Dec(len);
    path[len] := 0;
  end;
  Assert(strcmp(tmp, @path[0]) = 0);
{$ELSEIF DEFINED(ANDROID)}
  Assert(strcmp(tmp, '/data/local/tmp') = 0);
{$ELSE}
  Assert(strcmp(tmp, '/tmp') = 0);
{$ENDIF}
  sg_free(tmp);
end;

begin
  SgLib.Load(SG_LIB_NAME);
  test_version;
  test_malloc;
  test_alloc;
  test_realloc;
  test_free;
  test_strerror;
  test_is_post;
  test_extract_entrypoint;
  test_tmpdir;
end.
