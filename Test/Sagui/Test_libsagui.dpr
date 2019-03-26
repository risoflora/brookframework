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
  RTLConsts,
  SysUtils,
  libsagui;

procedure Test_SgLibLoad;
var
  OK: Boolean;
begin
  SgLib.Unload;
  OK := False;
  try
    Assert(SgLib.Load('') = NilHandle);
  except
    on E: Exception do
      OK := (E.ClassType = EArgumentException) and (E.Message = SSgLibEmptyName);
  end;
  Assert(OK);

  OK := False;
  try
    Assert(SgLib.Load('abc') = NilHandle);
  except
    on E: Exception do
      OK := (E.ClassType = ESgLibNotLoaded) and
        (E.Message = Format(SSgLibNotLoaded, ['abc']));
  end;
  Assert(OK);

  Assert(SgLib.Load(SG_LIB_NAME) <> NilHandle);
end;

procedure Test_SgLibUnload;
begin
  Assert(SgLib.Load(SG_LIB_NAME) <> NilHandle);
  Assert(SgLib.Unload = NilHandle);
end;

procedure Test_SgLibCheck;
var
  OK: Boolean;
begin
  OK := False;
  try
    SgLib.Unload;
    SgLib.Check;
  except
    on E: Exception do
      OK := (E.ClassType = ESgLibNotLoaded) and (E.Message =
        Format(SSgLibNotLoaded, [SG_LIB_NAME]));
  end;
  Assert(OK);
end;

begin
  Test_SgLibLoad;
  Test_SgLibUnload;
  Test_SgLibCheck;
end.
