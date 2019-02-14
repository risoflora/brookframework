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

program Test_libsagui;

{$I Tests.inc}

uses
  SysUtils,
  libsagui;

procedure Test_LoadLibrary;
begin
  SgUnloadLibrary;
  Assert(SgLoadLibrary('') = NilHandle);
  Assert(SgLoadLibrary('abc') = NilHandle);
  Assert(SgLoadLibrary(SG_LIB_NAME) <> NilHandle);
end;

procedure Test_UnloadLibrary;
begin
  Assert(SgLoadLibrary(SG_LIB_NAME) <> NilHandle);
  Assert(SgUnloadLibrary = NilHandle);
end;

procedure Test_CheckLibrary;
var
  OK: Boolean;
begin
  OK := False;
  try
    SgUnloadLibrary;
    SgCheckLibrary;
  except
    on E: Exception do
      OK := (E.ClassType = ESgLibraryNotLoaded) and (E.Message =
        Format(SSgLibraryNotLoaded, [SG_LIB_NAME]));
  end;
  Assert(OK);
end;

begin
  Test_LoadLibrary;
  Test_UnloadLibrary;
  Test_CheckLibrary;
end.
