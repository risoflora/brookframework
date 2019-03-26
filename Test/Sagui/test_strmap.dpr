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

program test_strmap;

{$IFDEF FPC}
 {$WARN 5024 OFF}
{$ENDIF}

uses
  SysUtils,
  Platform,
  Marshalling,
  libc,
  libsagui;

procedure test_strmap_name(pair: Psg_strmap);
var
  M: TMarshaller;
begin
  seterrno(0);
  Assert(sg_strmap_name(nil) = nil);
  Assert(errno = EINVAL);
  Assert(strcmp(sg_strmap_name(pair), M.ToCString('abç')) = 0);
end;

procedure test_strmap_val(pair: Psg_strmap);
var
  M: TMarshaller;
begin
  seterrno(0);
  Assert(sg_strmap_val(nil) = nil);
  Assert(errno = EINVAL);
  Assert(strcmp(sg_strmap_val(pair), M.ToCString('déf')) = 0);
end;

procedure test_strmap_add(map: PPsg_strmap; const name: Pcchar;
  const val: Pcchar);
var
  M: TMarshaller;
begin
  Assert(sg_strmap_add(nil, name, val) = EINVAL);
  Assert(sg_strmap_add(map, nil, val) = EINVAL);
  Assert(sg_strmap_add(map, name, nil) = EINVAL);

  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
  Assert(sg_strmap_add(map, M.ToCString(''), val) = 0);
  Assert(sg_strmap_count(map^) = 1);
  sg_strmap_cleanup(map);
  Assert(sg_strmap_add(map, name, M.ToCString('')) = 0);
  Assert(sg_strmap_count(map^) = 1);

  sg_strmap_cleanup(map);
  Assert(sg_strmap_add(map, name, val) = 0);
  Assert(sg_strmap_add(map, name, val) = 0);
  Assert(sg_strmap_count(map^) = 2);
end;

procedure test_strmap_set(map: PPsg_strmap; const name: Pcchar;
  const val: Pcchar);
var
  M: TMarshaller;
begin
  Assert(sg_strmap_set(nil, name, val) = EINVAL);
  Assert(sg_strmap_set(map, nil, val) = EINVAL);
  Assert(sg_strmap_set(map, name, nil) = EINVAL);

  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
  Assert(sg_strmap_set(map, M.ToCString(''), val) = 0);
  Assert(sg_strmap_count(map^) = 1);
  sg_strmap_cleanup(map);
  Assert(sg_strmap_set(map, name, M.ToCString('')) = 0);
  Assert(sg_strmap_count(map^) = 1);
  sg_strmap_cleanup(map);
  Assert(sg_strmap_set(map, name, val) = 0);
  Assert(sg_strmap_set(map, name, val) = 0);
  Assert(sg_strmap_count(map^) = 1);
end;

procedure test_strmap_find(map: PPsg_strmap; const name: Pcchar;
  const val: Pcchar);
var
  M: TMarshaller;
  pair: Psg_strmap;
begin
  Assert(sg_strmap_find(nil, name, @pair) = EINVAL);
  Assert(sg_strmap_find(map^, nil, @pair) = EINVAL);
  Assert(sg_strmap_find(map^, name, nil) = EINVAL);

  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
  sg_strmap_add(map, name, val);
  Assert(sg_strmap_count(map^) = 1);
  Assert(sg_strmap_find(map^, M.ToCString(''), @pair) = ENOENT);
  Assert(not Assigned(pair));
  Assert(sg_strmap_find(map^, M.ToCString('xxx'), @pair) = ENOENT);
  Assert(not Assigned(pair));
  Assert(sg_strmap_find(map^, M.ToCString('yyy'), @pair) = ENOENT);
  Assert(not Assigned(pair));

  sg_strmap_add(map, M.ToCString(''), M.ToCString(''));
  sg_strmap_add(map, M.ToCString('xxx'), M.ToCString('yyy'));
  sg_strmap_add(map, M.ToCString('yyy'), M.ToCString('xxx'));
  Assert(sg_strmap_count(map^) = 4);
  Assert(sg_strmap_find(map^, name, @pair) = 0);
  Assert(Assigned(pair));
  Assert((strcmp(sg_strmap_name(pair), name) = 0) and
    (strcmp(sg_strmap_val(pair), val) = 0));
  Assert(sg_strmap_find(map^, M.ToCString(''), @pair) = 0);
  Assert(Assigned(pair));
  Assert((strcmp(sg_strmap_name(pair), M.ToCString('')) = 0) and
    (strcmp(sg_strmap_val(pair), M.ToCString('')) = 0));
  Assert(sg_strmap_find(map^, M.ToCString('xxx'), @pair) = 0);
  Assert(Assigned(pair));
  Assert((strcmp(sg_strmap_name(pair), M.ToCString('xxx')) = 0) and
    (strcmp(sg_strmap_val(pair), M.ToCString('yyy')) = 0));
  Assert(sg_strmap_find(map^, M.ToCString('yyy'), @pair) = 0);
  Assert(Assigned(pair));
  Assert((strcmp(sg_strmap_name(pair), M.ToCString('yyy')) = 0) and
    (strcmp(sg_strmap_val(pair), M.ToCString('xxx')) = 0));
end;

procedure test_strmap_get(map: PPsg_strmap; const name: Pcchar;
  const val: Pcchar);
var
  M: TMarshaller;
begin
  Assert(not Assigned(sg_strmap_get(nil, name)));
  Assert(not Assigned(sg_strmap_get(map^, nil)));

  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
  sg_strmap_add(map, name, val);
  Assert(sg_strmap_count(map^) = 1);
  Assert(not Assigned(sg_strmap_get(map^, M.ToCString(''))));
  Assert(not Assigned(sg_strmap_get(map^, M.ToCString('xxx'))));
  Assert(not Assigned(sg_strmap_get(map^, M.ToCString('yyy'))));

  sg_strmap_add(map, M.ToCString(''), M.ToCString(''));
  sg_strmap_add(map, M.ToCString('xxx'), M.ToCString('yyy'));
  sg_strmap_add(map, M.ToCString('yyy'), M.ToCString('xxx'));
  Assert(sg_strmap_count(map^) = 4);
  Assert(strcmp(sg_strmap_get(map^, name), val) = 0);
  Assert(strcmp(sg_strmap_get(map^, M.ToCString('')), M.ToCString('')) = 0);
  Assert(strcmp(sg_strmap_get(map^, M.ToCString('xxx')),
    M.ToCString('yyy')) = 0);
  Assert(strcmp(sg_strmap_get(map^, M.ToCString('yyy')),
    M.ToCString('xxx')) = 0);
end;

procedure test_strmap_rm(map: PPsg_strmap; const name: Pcchar;
  const val: Pcchar);
var
  M: TMarshaller;
  pair: Psg_strmap;
begin
  Assert(sg_strmap_rm(nil, name) = EINVAL);
  Assert(sg_strmap_rm(map, nil) = EINVAL);

  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
  sg_strmap_add(map, name, val);
  Assert(sg_strmap_count(map^) = 1);
  Assert(sg_strmap_rm(map, M.ToCString('')) = ENOENT);
  Assert(sg_strmap_count(map^) = 1);
  Assert(sg_strmap_rm(map, M.ToCString('xxx')) = ENOENT);
  Assert(sg_strmap_count(map^) = 1);
  Assert(sg_strmap_rm(map, M.ToCString('yyy')) = ENOENT);
  Assert(sg_strmap_count(map^) = 1);

  sg_strmap_add(map, M.ToCString(''), M.ToCString(''));
  sg_strmap_add(map, M.ToCString('xxx'), M.ToCString('yyy'));
  sg_strmap_add(map, M.ToCString('yyy'), M.ToCString('xxx'));
  Assert(sg_strmap_count(map^) = 4);

  Assert(sg_strmap_find(map^, name, @pair) = 0);
  Assert(sg_strmap_rm(map, name) = 0);
  Assert(sg_strmap_count(map^) = 3);
  Assert(sg_strmap_find(map^, name, @pair) = ENOENT);

  Assert(sg_strmap_find(map^, M.ToCString(''), @pair) = 0);
  Assert(sg_strmap_rm(map, M.ToCString('')) = 0);
  Assert(sg_strmap_count(map^) = 2);
  Assert(sg_strmap_find(map^, M.ToCString(''), @pair) = ENOENT);

  Assert(sg_strmap_find(map^, M.ToCString('xxx'), @pair) = 0);
  Assert(sg_strmap_rm(map, M.ToCString('xxx')) = 0);
  Assert(sg_strmap_count(map^) = 1);
  Assert(sg_strmap_find(map^, M.ToCString('xxx'), @pair) = ENOENT);

  Assert(sg_strmap_find(map^, M.ToCString('yyy'), @pair) = 0);
  Assert(sg_strmap_rm(map, M.ToCString('yyy')) = 0);
  Assert(sg_strmap_count(map^) = 0);
  Assert(sg_strmap_find(map^, M.ToCString('yyy'), @pair) = EINVAL);
end;

function strmap_iter_empty(cls: Pcvoid; pair: Psg_strmap): cint; cdecl;
begin
  Result := 0;
end;

function strmap_iter_123(cls: Pcvoid; pair: Psg_strmap): cint; cdecl;
begin
  Result := 123;
end;

function strmap_iter_concat(cls: Pcvoid; pair: Psg_strmap): cint; cdecl;
begin
  strcat(cls, sg_strmap_name(pair));
  strcat(cls, sg_strmap_val(pair));
  Result := 0;
end;

procedure test_strmap_iter(map: PPsg_strmap);
var
  M: TMarshaller;
  str: array [0..99] of cchar;
begin
  Assert(sg_strmap_iter(nil, @strmap_iter_empty, M.ToCString('')) = EINVAL);
  Assert(sg_strmap_iter(map^, nil, M.ToCString('')) = EINVAL);

  sg_strmap_cleanup(map);
  sg_strmap_add(map, M.ToCString('abc'), M.ToCString('123'));
  sg_strmap_add(map, M.ToCString('def'), M.ToCString('456'));

  Assert(sg_strmap_iter(map^, @strmap_iter_empty, nil) = 0);
  Assert(sg_strmap_iter(map^, @strmap_iter_123, nil) = 123);

  memset(@str[0], 0, sizeof(str));
  Assert(strcmp(@str[0], M.ToCString('')) = 0);
  Assert(sg_strmap_iter(map^, @strmap_iter_concat, @str[0]) = 0);
  Assert(strcmp(@str[0], M.ToCString('abc123def456')) = 0);
end;

function strmap_sort_empty(cls: Pcvoid; pair_a: Psg_strmap;
  pair_b: Psg_strmap): cint; cdecl;
var
  M: TMarshaller;
begin
  sprintf(cls, M.ToCString('%s%s'), Pcchar(cls), Pcchar(cls));
  Result := 0;
end;

function strmap_sort_name_desc(cls: Pcvoid; pair_a: Psg_strmap;
  pair_b: Psg_strmap): cint; cdecl;
begin
  Result := strcmp(sg_strmap_name(pair_b), sg_strmap_name(pair_a));
end;

function strmap_sort_name_asc(cls: Pcvoid; pair_a: Psg_strmap;
  pair_b: Psg_strmap): cint; cdecl;
begin
  Result := strcmp(sg_strmap_name(pair_a), sg_strmap_name(pair_b));
end;

function strmap_sort_val_desc(cls: Pcvoid; pair_a: Psg_strmap;
  pair_b: Psg_strmap): cint; cdecl;
begin
  Result := strcmp(sg_strmap_val(pair_b), sg_strmap_val(pair_a));
end;

function strmap_sort_val_asc(cls: Pcvoid; pair_a: Psg_strmap;
  pair_b: Psg_strmap): cint; cdecl;
begin
  Result := strcmp(sg_strmap_val(pair_a), sg_strmap_val(pair_b));
end;

procedure test_strmap_sort(map: PPsg_strmap);
var
  M: TMarshaller;
  str: array [0..99] of cchar;
begin
  Assert(sg_strmap_sort(nil, @strmap_sort_empty, M.ToCString('')) = EINVAL);
  Assert(sg_strmap_sort(map, nil, M.ToCString('')) = EINVAL);

  sg_strmap_cleanup(map);
  sg_strmap_add(map, M.ToCString('abc'), M.ToCString('123'));
  sg_strmap_add(map, M.ToCString('def'), M.ToCString('456'));

  memset(@str[0], 0, sizeof(str));
  strcpy(@str[0], M.ToCString('abc'));
  Assert(strcmp(@str[0], M.ToCString('abc')) = 0);
  Assert(sg_strmap_sort(map, @strmap_sort_empty, @str[0]) = 0);
  Assert(strcmp(@str[0], M.ToCString('abcabc')) = 0);

  memset(@str[0], 0, sizeof(str));
  Assert(strcmp(@str[0], M.ToCString('')) = 0);
  sg_strmap_iter(map^, @strmap_iter_concat, @str[0]);
  Assert(strcmp(@str[0], M.ToCString('abc123def456')) = 0);
  Assert(sg_strmap_sort(map, @strmap_sort_name_desc, nil) = 0);
  memset(@str[0], 0, sizeof(str));
  sg_strmap_iter(map^, @strmap_iter_concat, @str[0]);
  Assert(strcmp(@str[0], M.ToCString('def456abc123')) = 0);

  Assert(sg_strmap_sort(map, @strmap_sort_name_asc, nil) = 0);
  memset(@str[0], 0, sizeof(str));
  sg_strmap_iter(map^, @strmap_iter_concat, @str[0]);
  Assert(strcmp(@str[0], M.ToCString('abc123def456')) = 0);

  Assert(sg_strmap_sort(map, @strmap_sort_val_desc, nil) = 0);
  memset(@str[0], 0, sizeof(str));
  sg_strmap_iter(map^, @strmap_iter_concat, @str[0]);
  Assert(strcmp(@str[0], M.ToCString('def456abc123')) = 0);

  Assert(sg_strmap_sort(map, @strmap_sort_val_asc, nil) = 0);
  memset(@str[0], 0, sizeof(str));
  sg_strmap_iter(map^, @strmap_iter_concat, @str[0]);
  Assert(strcmp(@str[0], M.ToCString('abc123def456')) = 0);
end;

procedure test_strmap_count(map: PPsg_strmap; const name: Pcchar;
  const val: Pcchar);
var
  M: TMarshaller;
begin
  Assert(sg_strmap_count(nil) = 0);
  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
  sg_strmap_add(map, name, val);
  Assert(sg_strmap_count(map^) = 1);
  sg_strmap_add(map, M.ToCString('xxx'), M.ToCString('yyy'));
  Assert(sg_strmap_count(map^) = 2);
  sg_strmap_add(map, M.ToCString('yyy'), M.ToCString('xxx'));
  Assert(sg_strmap_count(map^) = 3);
  sg_strmap_add(map, name, val);
  Assert(sg_strmap_count(map^) = 4);
  sg_strmap_rm(map, name);
  Assert(sg_strmap_count(map^) = 3);
  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
end;

procedure test_strmap_next(map: PPsg_strmap);
var
  M: TMarshaller;
  pair: Psg_strmap;
  str: array [0..99] of cchar;
begin
  Assert(sg_strmap_next(nil) = EINVAL);
  sg_strmap_cleanup(map);
  pair := nil;
  Assert(sg_strmap_next(@pair) = 0);
  Assert(not Assigned(pair));
  sg_strmap_add(map, M.ToCString('abc'), M.ToCString('123'));
  sg_strmap_add(map, M.ToCString('def'), M.ToCString('456'));
  sg_strmap_add(map, M.ToCString('xxx'), M.ToCString('yyy'));
  memset(@str[0], 0, sizeof(str));
  Assert(strcmp(@str[0], M.ToCString('')) = 0);
  pair := map^;
  while Assigned(pair) do
  begin
    strcat(@str[0], sg_strmap_name(pair));
    strcat(@str[0], sg_strmap_val(pair));
    sg_strmap_next(@pair);
  end;
  Assert(strcmp(@str[0], M.ToCString('abc123def456xxxyyy')) = 0);
end;

procedure test_strmap_cleanup(map: PPsg_strmap);
var
  M: TMarshaller;
begin
  sg_strmap_cleanup(nil);
  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
  sg_strmap_add(map, M.ToCString('abc'), M.ToCString('123'));
  sg_strmap_add(map, M.ToCString('def'), M.ToCString('456'));
  sg_strmap_add(map, M.ToCString('xxx'), M.ToCString('yyy'));
  Assert(sg_strmap_count(map^) = 3);
  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
  sg_strmap_cleanup(map);
  Assert(sg_strmap_count(map^) = 0);
end;

var
  map, pair: Psg_strmap;
  name, val: Pcchar;
begin
  SgLib.Load(SG_LIB_NAME);
  map := nil;
  name := 'abç';
  val := 'déf';
  sg_strmap_add(@map, name, val);
  sg_strmap_find(map, name, @pair);
  Assert(Assigned(pair));

  test_strmap_name(pair);
  test_strmap_val(pair);
  test_strmap_add(@map, name, val);
  test_strmap_set(@map, name, val);
  test_strmap_find(@map, name, val);
  test_strmap_get(@map, name, val);
  test_strmap_rm(@map, name, val);
  test_strmap_iter(@map);
  test_strmap_sort(@map);
  test_strmap_count(@map, name, val);
  test_strmap_next(@map);
  test_strmap_cleanup(@map);

  sg_strmap_cleanup(@map);
end.
