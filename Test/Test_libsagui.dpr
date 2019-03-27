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

program Test_libsagui;

{$I Tests.inc}

uses
  RTLConsts,
  SysUtils,
  Platform,
  libsagui;

procedure Test_SgLibGetLastName;
begin
  SgLib.Unload;
  Assert(SgLib.GetLastName = '');
  SgLib.Load(SG_LIB_NAME);
  Assert(SgLib.GetLastName = SG_LIB_NAME);
end;

procedure Test_SgLibCheckVersion;
var
  OK: Boolean;
begin
  SgLib.Unload;
  SgLib.Load(SG_LIB_NAME);
  OK := False;
  try
    SgLib.CheckVersion((Pred(SG_VERSION_MAJOR) shl 16) or
      (SG_VERSION_MINOR shl 8) or SG_VERSION_PATCH);
  except
    on E: Exception do
      OK := (E.ClassType = EInvalidOpException) and
        (E.Message = Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR,
          SG_VERSION_PATCH]));
  end;
  Assert(OK);
  SgLib.Load(SG_LIB_NAME);
  OK := False;
  try
    SgLib.CheckVersion((Succ(SG_VERSION_MAJOR) shl 16) or
      (SG_VERSION_MINOR shl 8) or SG_VERSION_PATCH);
  except
    on E: Exception do
      OK := (E.ClassType = EInvalidOpException) and
        (E.Message = Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR,
          SG_VERSION_PATCH]));
  end;
  Assert(OK);

  SgLib.Load(SG_LIB_NAME);
  OK := False;
  try
    SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
      (Pred(SG_VERSION_MINOR) shl 8) or SG_VERSION_PATCH);
  except
    on E: Exception do
      OK := (E.ClassType = EInvalidOpException) and
        (E.Message = Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR,
          SG_VERSION_PATCH]));
  end;
  Assert(OK);
  SgLib.Load(SG_LIB_NAME);
  SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
    (Succ(SG_VERSION_MINOR) shl 8) or SG_VERSION_PATCH);

  OK := False;
  try
    SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
      (SG_VERSION_MINOR shl 8) or Pred(SG_VERSION_PATCH));
  except
    on E: Exception do
      OK := (E.ClassType = EInvalidOpException) and
        (E.Message = Format(SSgLibVersion, [SG_VERSION_MAJOR, SG_VERSION_MINOR,
          SG_VERSION_PATCH]));
  end;
  Assert(OK);
  SgLib.Load(SG_LIB_NAME);
  SgLib.CheckVersion((SG_VERSION_MAJOR shl 16) or
    (Succ(SG_VERSION_MINOR) shl 8) or 0);

  SgLib.CheckVersion;
end;

procedure Test_SgLibCheckLastError;
var
  F: Pointer;
  OK: Boolean;
begin
  SgLib.CheckLastError(0);
  F := @sg_strerror;
  sg_strerror := nil;
  SgLib.CheckLastError(123);
  sg_strerror := F;

  OK := False;
  try
    SgLib.CheckLastError(EINVAL);
  except
    on E: Exception do
      OK := (E.ClassType = EOSError) and (EOSError(E).ErrorCode = EINVAL) and
        (E.Message = SysErrorMessage(EINVAL));
  end;
  Assert(OK);
  OK := False;
  try
    SgLib.CheckLastError(456);
  except
    on E: Exception do
      OK := (E.ClassType = EOSError) and (EOSError(E).ErrorCode = 456) and
        (E.Message = 'Unknown error 456');
  end;
  Assert(OK);
end;

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

procedure Test_SgLibIsLoaded;
begin
  SgLib.Unload;
  Assert(not SgLib.IsLoaded);
  SgLib.Load(SG_LIB_NAME);
  Assert(SgLib.IsLoaded);
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
      OK := (E.ClassType = ESgLibNotLoaded) and
        (E.Message = Format(SSgLibNotLoaded, [SG_LIB_NAME]));
  end;
  Assert(OK);
end;

procedure DoLibNotifier1(AClosure: Pointer); cdecl;
begin
  PInteger(AClosure)^ := 123;
end;

procedure DoLibNotifier2(AClosure: Pointer); cdecl;
begin
  PInteger(AClosure)^ := 456;
end;

procedure DoLibNotifier3(AClosure: Pointer); cdecl;
begin
  PInteger(AClosure)^ := 789;
end;

procedure Test_SgLibAddNotifier;
var
  I1, I2, I3: Integer;
  OK: Boolean;
begin
  OK := False;
  try
    SgLib.AddNotifier(nil, Pointer(1));
  except
    on E: Exception do
      OK := (E.ClassType = EArgumentNilException) and
        (E.Message = Format(SParamIsNil, ['ANotifier']));
  end;
  Assert(OK);
  SgLib.AddNotifier(@DoLibNotifier1, nil);
  SgLib.RemoveNotifier(@DoLibNotifier1);

  SgLib.Load(SG_LIB_NAME);
  I1 := 0;
  I2 := 0;
  I3 := 0;
  SgLib.AddNotifier(@DoLibNotifier1, @I1);
  SgLib.AddNotifier(@DoLibNotifier2, @I2);
  SgLib.AddNotifier(@DoLibNotifier3, @I3);
  SgLib.Unload;
  Assert(I1 = 123);
  Assert(I2 = 456);
  Assert(I3 = 789);
  SgLib.Load(SG_LIB_NAME);
  I1 := 0;
  I2 := 0;
  I3 := 0;
  SgLib.Unload;
  Assert(I1 = 123);
  Assert(I2 = 456);
  Assert(I3 = 789);
end;

procedure Test_SgLibRemoveNotifier;
var
  I1, I2, I3: Integer;
  OK: Boolean;
begin
  OK := False;
  try
    SgLib.RemoveNotifier(nil);
  except
    on E: Exception do
      OK := (E.ClassType = EArgumentNilException) and
        (E.Message = Format(SParamIsNil, ['ANotifier']));
  end;
  Assert(OK);

  SgLib.Load(SG_LIB_NAME);
  I1 := 0;
  I2 := 0;
  I3 := 0;
  SgLib.AddNotifier(@DoLibNotifier1, @I1);
  SgLib.AddNotifier(@DoLibNotifier2, @I2);
  SgLib.AddNotifier(@DoLibNotifier3, @I3);
  SgLib.RemoveNotifier(@DoLibNotifier2);
  SgLib.Unload;
  Assert(I1 = 123);
  Assert(I2 = 0);
  Assert(I3 = 789);
end;

begin
  Test_SgLibGetLastName;
  Test_SgLibCheckVersion;
  Test_SgLibCheckLastError;
  Test_SgLibLoad;
  Test_SgLibUnload;
  Test_SgLibIsLoaded;
  Test_SgLibCheck;
  Test_SgLibAddNotifier;
  Test_SgLibRemoveNotifier;
end.
