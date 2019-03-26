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

program test_str;

uses
  SysUtils,
  Platform,
  Marshalling,
  libc,
  libsagui;

procedure test_str_write(str: Psg_str; const val: Pcchar; len: csize_t);
begin
  Assert(sg_str_write(nil, val, len) = EINVAL);
  Assert(sg_str_write(str, nil, len) = EINVAL);
  Assert(sg_str_write(str, val, 0) = EINVAL);

  sg_str_clear(str);
  Assert(sg_str_write(str, val, len) = 0);
  Assert(sg_str_length(str) = len);
end;

procedure test_str_printf_va(str: Psg_str; const fmt: Pcchar; ap: cva_list);
begin
  Assert(sg_str_printf_va(nil, fmt, ap) = EINVAL);
  Assert(sg_str_printf_va(str, nil, ap) = EINVAL);
{$IFDEF ARM}
  ASSERT(sg_str_printf_va(str, fmt, nil) = EINVAL);
{$ENDIF}
end;

procedure test_str_printf(str: Psg_str);
var
  M: TMarshaller;
begin
  Assert(sg_str_printf(nil, M.ToCString('%s'), M.ToCString('')) = EINVAL);
  Assert(sg_str_printf(str, nil) = EINVAL);

  sg_str_clear(str);
  Assert(sg_str_printf(str, M.ToCString('%s'), M.ToCString('')) = 0);
  Assert(strlen(sg_str_content(str)) = 0);
  Assert(sg_str_printf(str, M.ToCString('%s%d'), M.ToCString('abc'), 123) = 0);
  Assert(strcmp(sg_str_content(str), M.ToCString('abc123')) = 0);
end;

procedure test_str_content(str: Psg_str; const val: Pcchar; len: csize_t);
begin
  seterrno(0);
  Assert(not Assigned(sg_str_content(nil)));
  Assert(errno = EINVAL);

  sg_str_clear(str);
  Assert(strlen(sg_str_content(str)) = 0);
  sg_str_write(str, val, len);
  Assert(strcmp(sg_str_content(str), val) = 0);
end;

procedure test_str_length(str: Psg_str; const val: Pcchar; len: csize_t);
begin
  seterrno(0);
  Assert(sg_str_length(nil) = 0);
  Assert(errno = EINVAL);

  sg_str_clear(str);
  Assert(sg_str_length(str) = 0);
  sg_str_write(str, val, len);
  Assert(sg_str_length(str) = len);
end;

procedure test_str_clear(str: Psg_str; const val: Pcchar; len: csize_t);
begin
  Assert(sg_str_clear(nil) = EINVAL);

  sg_str_clear(str);
  sg_str_write(str, val, len);
  Assert(sg_str_length(str) > 0);
  sg_str_clear(str);
  Assert(sg_str_length(str) = 0);
end;

var
  M: TMarshaller;
  str: Psg_str;
  val: Pcchar;
  len: csize_t;
begin
  SgLib.Load(SG_LIB_NAME);
  val := 'abc123def456';
  len := strlen(val);

  str := sg_str_new();
  Assert(Assigned(str));

  test_str_write(str, val, len);
  test_str_printf(str);
  test_str_printf_va(str, M.ToCString(''), Pointer(1));
  test_str_content(str, val, len);
  test_str_length(str, val, len);
  test_str_clear(str, val, len);

  sg_str_free(str);
end.
