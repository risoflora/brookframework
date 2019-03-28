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

program Test_String;

{$I Tests.inc}

uses
  SysUtils,
  libsagui,
  BrookString,
  Test;

type
  TLocalString = class(TBrookString)
  public
    procedure LocalDestroy;
  end;

procedure TLocalString.LocalDestroy;
begin
  inherited Destroy;
  SgLib.Check;
  { checks if handle was really freed and 'nilified'. }
  Assert(not Assigned(Handle));
  Assert(sg_str_clear(Handle) <> 0);
end;

procedure Test_StringHandle(AStr: TBrookString);
var
  VStr: TBrookString;
begin
  Assert(Assigned(AStr.Handle));
  VStr := TBrookString.Create(AStr.Handle);
  try
    Assert(VStr.Handle = AStr.Handle);
  finally
    VStr.Free;
  end;
  VStr := TBrookString.Create(nil);
  try
    Assert(Assigned(VStr.Handle));
    Assert(VStr.Handle <> AStr.Handle);
  finally
    VStr.Free;
  end;
end;

procedure Test_StringOwnsHandle;
var
  Vhandle: Psg_str;
  VStr: TBrookString;
begin
  SgLib.Check;
  Vhandle := sg_str_new;
  Assert(Assigned(Vhandle));
  VStr := TBrookString.Create(Vhandle);
  try
    Assert(Assigned(VStr.Handle));
    Assert(VStr.Handle = Vhandle);
  finally
    VStr.Destroy;
    sg_str_free(Vhandle);
  end;
  VStr := TLocalString.Create(nil);
  try
    Assert(Assigned(VStr.Handle));
  finally
    TLocalString(VStr).LocalDestroy;
  end;
end;

procedure DoStrWriteBytes1(const AArgs: array of const);
begin
  Assert(TBrookString(AArgs[0].VObject).WriteBytes(nil, AArgs[1].VInteger) = 0);
end;

procedure DoStrWriteBytes2(const AArgs: array of const);
begin
  Assert(TBrookString(AArgs[0].VObject).WriteBytes(
    TBytes(AArgs[1].VPointer^), 0) = 0);
end;

procedure Test_StringWriteBytes(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AssertOSExcept(DoStrWriteBytes1, [AStr, ALen]);
  AssertOSExcept(DoStrWriteBytes2, [AStr, @AVal]);

  AStr.Clear;
  Assert(AStr.WriteBytes(AVal, ALen) = ALen);
  Assert(AStr.Length = ALen);
end;

procedure DoStrWrite1(const AArgs: array of const);
begin
  TBrookString(AArgs[0].VObject).Write('', TEncoding.UTF8);
end;

procedure DoStrWrite2(const AArgs: array of const);
begin
  TBrookString(AArgs[0].VObject).Write(AArgs[1].VString^, nil);
end;

procedure Test_StringWrite(AStr: TBrookString; const AVal: string;
  ALen: NativeUInt);
begin
  AssertOSExcept(DoStrWrite1, [AStr]);
  AssertExcept(DoStrWrite2, EArgumentNilException, [AStr, @AVal]);

  AStr.Clear;
  AStr.Write(AVal, TEncoding.UTF8);
  Assert(AStr.Length = ALen);
end;

procedure Test_StrincContent(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AStr.Clear;
  Assert(Length(AStr.Content) = 0);
  AStr.WriteBytes(AVal, ALen);
  Assert(CompareMem(@AStr.Content[0], @AVal[0], ALen));
end;

procedure Test_StringLength(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AStr.Clear;
  Assert(AStr.Length = 0);

  AStr.WriteBytes(AVal, ALen);
  Assert(AStr.Length = ALen);
end;

procedure Test_StringClear(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AStr.Clear;
  Assert(AStr.Length = 0);
  AStr.WriteBytes(AVal, ALen);
  Assert(AStr.Length > 0);
  Assert(AStr.Length = ALen);
end;

procedure Test_StringText(AStr: TBrookString; const AVal: string);
begin
  AStr.Clear;
  Assert(AStr.Text.IsEmpty);

  AStr.Text := AVal;
  Assert(AStr.Text = AVal);
end;

procedure Test_StringToString(AStr: TBrookString; const AVal: string);
begin
  AStr.Clear;
  Assert(AStr.Text.IsEmpty);

  AStr.Text := AVal;
  Assert(AStr.ToString = AVal);
end;

procedure Test_StringExtra(AStr: TBrookString);
var
  VStr: TBrookString;
begin
  AStr.Clear;
  AStr.Write('abc');
  Assert(AStr.Text = 'abc');
  VStr := TBrookString.Create(AStr.Handle);
  try
    VStr.Write('123');
  finally
    VStr.Free;;
  end;
  Assert(AStr.Text = 'abc123');
end;

const
  VAL = 'abc123def456';
  LEN: NativeUInt = Length(VAL);
var
  VValB: TBytes;
  VStr: TBrookString;
begin
  SgLib.Load(SG_LIB_NAME);
  VValB := TEncoding.UTF8.GetBytes(VAL);
  VStr := TBrookString.Create(nil);
  try
    Test_StringHandle(VStr);
    Test_StringOwnsHandle;
    Test_StringWriteBytes(VStr, VValB, LEN);
    Test_StringWrite(VStr, VAL, LEN);
    Test_StrincContent(VStr, VValB, LEN);
    Test_StringLength(VStr, VValB, LEN);
    Test_StringClear(VStr, VValB, LEN);
    Test_StringText(VStr, VAL);
    Test_StringToString(VStr, VAL);
    Test_StringExtra(VStr);
  finally
    VStr.Destroy;
  end;
end.
