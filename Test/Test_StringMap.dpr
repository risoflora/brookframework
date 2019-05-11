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

program Test_StringMap;

{$I Tests.inc}

{$IFDEF FPC}
 {$WARN 5024 OFF}
 {$IFNDEF VER3_0} // should be FPC_FULLVERSION>=030100, but we must avoid "[dcc32 Error] Constant expression expected"
  {$WARN 6058 OFF}
 {$ENDIF}
 {$CODEPAGE UTF8}
{$ENDIF}

uses
  SysConst,
  SysUtils,
  libsagui,
  Marshalling,
  BrookLibraryLoader,
  BrookStringMap,
  Test;

type
  TLocalStringMap = class(TBrookStringMap)
  private
    FOperation: TBrookStringMapOperation;
  protected
    procedure DoChange(AOperation: TBrookStringMapOperation); override;
  public
    procedure LocalDestroy;
    property Operation: TBrookStringMapOperation read FOperation;
  end;

procedure TLocalStringMap.DoChange(AOperation: TBrookStringMapOperation);
begin
  FOperation := AOperation;
  inherited DoChange(AOperation);
end;

procedure TLocalStringMap.LocalDestroy;
begin
  inherited Destroy;
  SgLib.Check;
  { checks if the handle was really freed and 'nilified'. }
  Assert(not Assigned(Handle));
  sg_strmap_cleanup(Handle);
end;

procedure Test_StringMapNameValue;
var
  VPair: TBrookStringPair;
begin
  VPair := TBrookStringPair.Create('', '');
  Assert(VPair.Name.IsEmpty);
  Assert(VPair.Value.IsEmpty);
  VPair := TBrookStringPair.Create('abc', '123');
  Assert(VPair.Name.Equals('abc'));
  Assert(VPair.Value.Equals('123'));
end;

procedure Test_StringMapClearOnDestroy;
var
  VMapHandle: Pointer;
  VMap: TBrookStringMap;
begin
  VMapHandle := nil;
  VMap := TBrookStringMap.Create(@VMapHandle);
  try
    Assert(VMap.ClearOnDestroy);
    VMap.ClearOnDestroy := False;
    VMap.Add('abc', '123');
    VMap.Add('def', '456');
    SgLib.Check;
    Assert(sg_strmap_count(VMapHandle) = 2);
  finally
    VMap.Free;
  end;
  Assert(sg_strmap_count(VMapHandle) = 2);
  sg_strmap_cleanup(@VMapHandle);
  VMapHandle := nil;
  VMap := TLocalStringMap.Create(@VMapHandle);
  try
    VMap.Add('abc', '123');
    VMap.Add('def', '456');
    SgLib.Check;
    Assert(sg_strmap_count(VMapHandle) = 2);
  finally
    VMap.Free;
  end;
  Assert(sg_strmap_count(VMapHandle) = 0);
end;

procedure Test_StringMapOnChange;
var
  VMapHandle: Pointer;
  VMap: TLocalStringMap;
begin
  VMapHandle := nil;
  VMap := TLocalStringMap.Create(@VMapHandle);
  try
    Assert(VMap.Operation = sgmoNone);
    VMap.Add('abc', '123');
    Assert(VMap.Operation = sgmoAdd);
    VMap.AddOrSet('def', '456');
    Assert(VMap.Operation = sgmoAddOrSet);
    VMap.Remove('abc');
    Assert(VMap.Operation = sgmoRemove);
    VMap.Clear;
    Assert(VMap.Operation = sgmoNone);
  finally
    VMap.Free;
  end;
end;

procedure Test_StringMapHandle(AMap: TBrookStringMap);
var
  VMapHandle: Pointer;
  VMap: TBrookStringMap;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  Assert(Assigned(AMap.Handle));
  VMap := TBrookStringMap.Create(AMap.Handle);
  try
    Assert(VMap.Handle = AMap.Handle);
  finally
    VMap.Free;
  end;
  VMapHandle := nil;
  VMap := TBrookStringMap.Create(@VMapHandle);
  try
    VMap.Add('abc', '123');
    Assert(Assigned(VMap.Handle));
    Assert(VMap.Handle <> AMap.Handle);
  finally
    VMap.Free;
  end;
end;

procedure Test_StringMapAdd(AMap: TBrookStringMap; const AName, AValue: string);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add('', AValue);
  Assert(AMap.Count = 1);
  AMap.Clear;
  AMap.Add(AName, '');
  Assert(AMap.Count = 1);

  AMap.Clear;
  AMap.Add(AName, AValue);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 2);
end;

procedure Test_StringMapAddOrSet(AMap: TBrookStringMap; const AName,
  AValue: string);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.AddOrSet('', AValue);
  Assert(AMap.Count = 1);
  AMap.Clear;
  AMap.AddOrSet(AName, '');
  Assert(AMap.Count = 1);

  AMap.Clear;
  AMap.AddOrSet(AName, AValue);
  AMap.AddOrSet(AName, AValue);
  Assert(AMap.Count = 1);
end;

procedure Test_StringMapFind(AMap: TBrookStringMap; const AName,
  AValue: string);
var
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 1);
  Assert(not AMap.Find('', VPair));
  Assert(VPair.Name.IsEmpty);
  Assert(VPair.Value.IsEmpty);
  Assert(not AMap.Find('xxx', VPair));
  Assert(VPair.Name.IsEmpty);
  Assert(VPair.Value.IsEmpty);
  Assert(not AMap.Find('yyy', VPair));
  Assert(VPair.Name.IsEmpty);
  Assert(VPair.Value.IsEmpty);

  AMap.Add('', '');
  AMap.Add('xxx', 'yyy');
  AMap.Add('yyy', 'xxx');
  Assert(AMap.Count = 4);
  Assert(AMap.Find(AName, VPair));
  Assert((VPair.Name = AName) and (VPair.Value = AValue));
  Assert(AMap.Find('', VPair));
  Assert((VPair.Name.IsEmpty) and (VPair.Value.IsEmpty));
  Assert(AMap.Find('xxx', VPair));
  Assert(VPair.Name.Equals('xxx') and VPair.Value.Equals('yyy'));
  Assert(AMap.Find('yyy', VPair));
  Assert(VPair.Name.Equals('yyy') and VPair.Value.Equals('xxx'));
end;

procedure Test_StringMapGet(AMap: TBrookStringMap; const AName,
  AValue: string);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 1);
  Assert(AMap.Get('').IsEmpty);
  Assert(AMap.Get('xxx').IsEmpty);
  Assert(AMap.Get('yyy').IsEmpty);

  AMap.Add('', '');
  AMap.Add('xxx', 'yyy');
  AMap.Add('yyy', 'xxx');
  Assert(AMap.Count = 4);
  Assert(AMap.Get(AName).Equals(AValue));

  Assert(AMap.Get('').IsEmpty);
  Assert(AMap.Get('xxx').Equals('yyy'));
  Assert(AMap.Get('yyy').Equals('xxx'));
end;

procedure Test_StringMapRemove(AMap: TBrookStringMap; const AName,
  AValue: string);
var
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 1);
  AMap.Remove('');
  Assert(AMap.Count = 1);
  AMap.Remove('xxx');
  Assert(AMap.Count = 1);
  AMap.Remove('yyy');
  Assert(AMap.Count = 1);

  AMap.Add('', '');
  AMap.Add('xxx', 'yyy');
  AMap.Add('yyy', 'xxx');
  Assert(AMap.Count = 4);

  Assert(AMap.Find(AName, VPair));
  AMap.Remove(AName);
  Assert(AMap.Count = 3);
  Assert(not AMap.Find(AName, VPair));

  Assert(AMap.Find('', VPair));
  AMap.Remove('');
  Assert(AMap.Count = 2);
  Assert(not AMap.Find('', VPair));

  Assert(AMap.Find('xxx', VPair));
  AMap.Remove('xxx');
  Assert(AMap.Count = 1);
  Assert(not AMap.Find('xxx', VPair));

  Assert(AMap.Find('yyy', VPair));
  AMap.Remove('yyy');
  Assert(AMap.Count = 0);
  Assert(not AMap.Find('yyy', VPair));
end;

function StringMapIterateEmpty(AData: Pointer;
  APair: TBrookStringPair): Integer;
begin
  Result := 0;
end;

function StringMapIterate123(AData: Pointer; APair: TBrookStringPair): Integer;
begin
  Result := 123;
end;

function StringMapIterateConcat(AData: Pointer;
  APair: TBrookStringPair): Integer;
var
  S: PString absolute AData;
begin
  S^ := Concat(S^, APair.Name, APair.Value);
  Result := 0;
end;

procedure DoStringMapIterate(const AArgs: array of const);
begin
  TBrookStringMap(AArgs[0].VObject).Iterate(StringMapIterate123, nil);
end;

procedure Test_StringMapIterate(AMap: TBrookStringMap);
var
  S: string;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  AMap.Add('def', '456');

  AMap.Iterate(StringMapIterateEmpty, nil);
  AssertOSExcept(DoStringMapIterate, 123, [AMap]);

  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S.Equals('abc123def456'));
end;

function StringMapSortEmpty(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
var
  S: PString absolute AData;
begin
  S^ := Concat(S^, S^);
  Result := 0;
end;

function StringMapSortNameDesc(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
begin
  Result := CompareStr(APairB.Name, APairA.Name);
end;

function StringMapSortNameAsc(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
begin
  Result := CompareStr(APairA.Name, APairB.Name);
end;

function StringMapSortValueDesc(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
begin
  Result := CompareStr(APairB.Value, APairA.Value);
end;

function StringMapSortValueAsc(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
begin
  Result := CompareStr(APairA.Value, APairB.Value);
end;

procedure Test_StringMapSort(AMap: TBrookStringMap);
var
  S: string;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  AMap.Add('def', '456');

  S := 'abc';
  AMap.Sort(StringMapSortEmpty, @S);
  Assert(S.Equals('abcabc'));

  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S.Equals('abc123def456'));
  AMap.Sort(StringMapSortNameDesc, nil);
  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S.Equals('def456abc123'));

  AMap.Sort(StringMapSortNameAsc, nil);
  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S.Equals('abc123def456'));

  AMap.Sort(StringMapSortValueDesc, nil);
  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S.Equals('def456abc123'));

  AMap.Sort(StringMapSortValueAsc, nil);
  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S.Equals('abc123def456'));
end;

procedure Test_StringMapCount(AMap: TBrookStringMap; const AName,
  AValue: string);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 1);
  AMap.Add('xxx', 'yyy');
  Assert(AMap.Count = 2);
  AMap.Add('yyy', 'xxx');
  Assert(AMap.Count = 3);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 4);
  AMap.Remove(AName);
  Assert(AMap.Count = 3);
  AMap.Clear;
  Assert(AMap.Count = 0);
end;

procedure Test_StringMapTryValue(AMap: TBrookStringMap; const AName,
  AValue: string);
var
  S: string;
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 1);
  Assert(not AMap.TryValue('', S));
  Assert(S.IsEmpty);

  Assert(not AMap.TryValue('xxx', S));
  Assert(S.IsEmpty);
  Assert(not AMap.TryValue('yyy', S));
  Assert(S.IsEmpty);

  AMap.Add('', '');
  AMap.Add('xxx', 'yyy');
  AMap.Add('yyy', 'xxx');
  Assert(AMap.Count = 4);
  Assert(AMap.TryValue(AName, S));
  Assert(S = AValue);
  Assert(AMap.TryValue('', S));
  Assert(S.IsEmpty);
  Assert(AMap.TryValue('xxx', S));
  Assert(S.Equals('yyy'));
  Assert(AMap.TryValue('yyy', S));
  Assert(S.Equals('xxx'));
end;

procedure Test_StringMapFirst(AMap: TBrookStringMap);
var
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  AMap.Add('def', '456');
  AMap.Add('xxx', 'yyy');
  AMap.First(VPair);
  Assert(VPair.Name.Equals('abc') and VPair.Value.Equals('123'));
  AMap.Next(VPair);
  AMap.Next(VPair);
  Assert(VPair.Name.Equals('xxx') and VPair.Value.Equals('yyy'));
  AMap.First(VPair);
  Assert(VPair.Name.Equals('abc') and VPair.Value.Equals('123'));
end;

procedure Test_StringMapValues(AMap: TBrookStringMap);
begin
  AMap.Clear;
  Assert(AMap.Values['abc'].IsEmpty);
  AMap.Values['abc'] := '123';
  Assert(AMap.Values['abc'].Equals('123'));
  Assert(AMap.Values['def'].IsEmpty);
  AMap.Values['def'] := '456';
  Assert(AMap.Values['def'].Equals('456'));
  Assert(AMap.Values['xxx'].IsEmpty);
  AMap.Values['xxx'] := 'yyy';
  Assert(AMap.Values['xxx'].Equals('yyy'));
  Assert(AMap.Count = 3);
  AMap.Values['xxx'] := 'yyy';
  Assert(AMap.Count = 3);
end;

procedure Test_StringMapEOF(AMap: TBrookStringMap);
var
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  AMap.Add('def', '456');
  AMap.Add('xxx', 'yyy');
  AMap.First(VPair);
  Assert(not AMap.EOF);
  AMap.Next(VPair);
  Assert(not AMap.EOF);
  AMap.Next(VPair);
  Assert(not AMap.EOF);
  AMap.Next(VPair);
  Assert(AMap.EOF);
  AMap.Next(VPair);
  Assert(AMap.EOF);
end;

procedure Test_StringMapNext(AMap: TBrookStringMap);
var
  S: string;
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  VPair := Default(TBrookStringPair);
  Assert(not AMap.Next(VPair));
  AMap.Add('abc', '123');
  AMap.Add('def', '456');
  AMap.Add('xxx', 'yyy');
  S := '';
  AMap.First(VPair);
  while not AMap.EOF do
  begin
    S := Concat(S, VPair.Name, VPair.Value);
    AMap.Next(VPair);
  end;
  Assert(S.Equals('abc123def456xxxyyy'));
end;

procedure Test_StringMapEnumerator(AMap: TBrookStringMap);
var
  I: Byte;
  S: string;
  P: TBrookStringPair;
begin
  AMap.Clear;
  for I := 1 to 3 do
  begin
    S := I.ToString;
    AMap.Add(Concat('name', S), Concat('value', S));
  end;
  I := 0;
  for P in AMap do
  begin
    S := Succ(I).ToString;
    Assert(P.Name.Equals(Concat('name', S)) and
      P.Value.Equals(Concat('value', S)));
    Inc(I);
  end;
  Assert(I = 3);
end;

procedure Test_StringMapToString(AMap: TBrookStringMap; const AName,
  AValue: string);
begin
  AMap.Clear;
  Assert(AMap.ToString.IsEmpty);
  AMap.Add(AName, AValue);
  Assert(AMap.ToString = Concat(AName, '=', AValue, sLineBreak));
  AMap.Add('xxx', 'yyy');
  AMap.Add('yyy', 'xxx');
  Assert(AMap.ToString.Equals(Concat(AName, '=', AValue, sLineBreak,
    'xxx=yyy', sLineBreak, 'yyy=xxx', sLineBreak)));
end;

procedure Test_StringMapClear(AMap: TBrookStringMap);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add('abc', '123');
  AMap.Add('def', '456');
  AMap.Add('xxx', 'yyy');
  Assert(AMap.Count = 3);
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Clear;
  Assert(AMap.Count = 0);
end;

const
  NAME = 'abç';
  VAL = 'déf';
var
  VMapHandle: Pointer;
  VMap: TBrookStringMap;
begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  Test_StringMapNameValue;
  Test_StringMapClearOnDestroy;
  Test_StringMapOnChange;
  VMapHandle := nil;
  VMap := TBrookStringMap.Create(@VMapHandle);
  try
    Test_StringMapHandle(VMap);
    Test_StringMapAdd(VMap, NAME, VAL);
    Test_StringMapAddOrSet(VMap, NAME, VAL);
    Test_StringMapFind(VMap, NAME, VAL);
    Test_StringMapGet(VMap, NAME, VAL);
    Test_StringMapRemove(VMap, NAME, VAL);
    Test_StringMapIterate(VMap);
    Test_StringMapSort(VMap);
    Test_StringMapCount(VMap, NAME, VAL);
    Test_StringMapTryValue(VMap, NAME, VAL);
    Test_StringMapFirst(VMap);
    Test_StringMapValues(VMap);
    Test_StringMapEOF(VMap);
    Test_StringMapNext(VMap);
    Test_StringMapEnumerator(VMap);
    Test_StringMapToString(VMap, NAME, VAL);
    Test_StringMapClear(VMap);
  finally
    VMap.Free;
  end;
end.
