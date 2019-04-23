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

program Test_MediaTypes;

{$I Tests.inc}

uses
  SysUtils,
  BrookExtra,
  BrookLibraryLoader,
  BrookMediaTypes,
  Test;

type

  { TFakeMediaTypes }

  TFakeMediaTypes = class(TBrookMediaTypes)
  private
    FPrepared: Boolean;
  protected
    function IsPrepared: Boolean; override;
  public
    class function GetDescription: string; override;
    procedure Prepare; override;
    property Cache;
  end;

{ TFakeMediaTypes }

class function TFakeMediaTypes.GetDescription: string;
begin
  Result := 'Fake';
end;

procedure TFakeMediaTypes.Prepare;
begin
  FPrepared := True;
end;

function TFakeMediaTypes.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

{ MediaTypes }

procedure Test_MediaTypesCreate;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(Assigned(MT.Cache));
    Assert(MT.DefaultType = BROOK_CT_OCTET_STREAM);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesGetRegisterAlias;
begin
  Assert(TFakeMediaTypes.GetRegisterAlias = 'BrookMIME_Fake');
end;

procedure Test_MediaTypesGetDescription;
begin
  Assert(TFakeMediaTypes.GetDescription = 'Fake');
end;

procedure Test_MediaTypesIsValid;
begin
  Assert(not TBrookMediaTypes.IsValid(''));
  Assert(not TBrookMediaTypes.IsValid('abc'));
  Assert(not TBrookMediaTypes.IsValid('abc/'));
  Assert(not TBrookMediaTypes.IsValid('/abc'));
  Assert(not TBrookMediaTypes.IsValid('/abc/def/def'));
  Assert(TBrookMediaTypes.IsValid('abc/def'));
end;

procedure Test_MediaTypesIsText;
begin
  Assert(not TBrookMediaTypes.IsText(''));
  Assert(not TBrookMediaTypes.IsText('abc'));
  Assert(not TBrookMediaTypes.IsText('abc/'));
  Assert(not TBrookMediaTypes.IsText('/abc'));
  Assert(not TBrookMediaTypes.IsText('/abc/def/def'));
  Assert(not TBrookMediaTypes.IsText('abc/def'));
  Assert(not TBrookMediaTypes.IsText('application/xml'));
  Assert(TBrookMediaTypes.IsText('text/html'));
end;

procedure Test_MediaTypesIsExt;
begin
  Assert(not TBrookMediaTypes.IsExt(''));
  Assert(not TBrookMediaTypes.IsExt('.'));
  Assert(not TBrookMediaTypes.IsExt('..'));
  Assert(TBrookMediaTypes.IsExt('js'));
  Assert(TBrookMediaTypes.IsExt('.js'));
end;

procedure Test_MediaTypesNormalizeExt;
begin
  Assert(TBrookMediaTypes.NormalizeExt('') = '');
  Assert(TBrookMediaTypes.NormalizeExt('.') = '.');
  Assert(TBrookMediaTypes.NormalizeExt('a') = '.a');
  Assert(TBrookMediaTypes.NormalizeExt('.a') = '.a');
  Assert(TBrookMediaTypes.NormalizeExt('a.b') = '.a.b');
  Assert(TBrookMediaTypes.NormalizeExt('.abc') = '.abc');
end;

procedure Test_MediaTypesPrepare;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.IsPrepared);
    MT.Prepare;
    Assert(MT.IsPrepared);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesAddEmptyMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Add('', 'bar/foo');
end;

procedure DoMediaTypesAddInvalidMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Add('.', 'bar/foo');
end;

procedure DoMediaTypesAddEmptyMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Add('.foo', '');
end;

procedure DoMediaTypesAddInvalidMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Add('.foo', 'bar');
end;

procedure Test_MediaTypesAdd;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.Prepared);
    Assert(MT.Count = 0);
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Count = 2);

    MT.Clear;
    AssertExcept(DoMediaTypesAddEmptyMediaExt,
      EArgumentException, SBrookEmptyMediaExt, [MT]);
    AssertExcept(DoMediaTypesAddInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [MT]);
    AssertExcept(DoMediaTypesAddEmptyMediaType,
      EArgumentException, SBrookEmptyMediaType, [MT]);
    AssertExcept(DoMediaTypesAddInvalidMediaType,
      EBrookMediaTypes, Format(SBrookInvalidMediaType, ['bar']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesRemoveEmptyMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Remove('');
end;

procedure DoMediaTypesRemoveInvalidMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Remove('.');
end;

procedure Test_MediaTypesRemove;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Count = 2);
    MT.Remove('.foo');
    Assert(MT.Count = 1);
    MT.Remove('.bar');
    Assert(MT.Count = 0);

    AssertExcept(DoMediaTypesRemoveEmptyMediaExt,
      EArgumentException, SBrookEmptyMediaExt, [MT]);
    AssertExcept(DoMediaTypesRemoveInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesTryTypeEmptyMediaExt(const AArgs: array of const);
var
  D: string;
begin
  TFakeMediaTypes(AArgs[0].VObject).TryType('', D);
end;

procedure DoMediaTypesTryTypeInvalidMediaExt(const AArgs: array of const);
var
  D: string;
begin
  TFakeMediaTypes(AArgs[0].VObject).TryType('.', D);
end;

procedure Test_MediaTypesTryType;
var
  MT: TFakeMediaTypes;
  T: string;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.Prepared);
    Assert(not MT.TryType('.foo', T));
    Assert(not MT.TryType('.bar', T));
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.TryType('.foo', T));
    Assert(T = 'bar/foo');
    Assert(MT.TryType('.bar', T));
    Assert(T = 'foo/bar');
    Assert(MT.Prepared);

    AssertExcept(DoMediaTypesTryTypeEmptyMediaExt,
      EArgumentException, SBrookEmptyMediaExt, [MT]);
    AssertExcept(DoMediaTypesTryTypeInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesFindEmptyMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Find('');
end;

procedure DoMediaTypesFindInvalidMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Find('.');
end;

procedure DoMediaTypesFindEmptyMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Find('.foo', '');
end;

procedure DoMediaTypesFindInvalidMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Find('.foo', 'bar');
end;

procedure Test_MediaTypesFind;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.Prepared);
    Assert(MT.Find('.foo') = MT.DefaultType);
    Assert(MT.Find('.foo', 'bar/foo') = 'bar/foo');
    Assert(MT.Find('.bar') = MT.DefaultType);
    Assert(MT.Find('.bar', 'bar/foo') = 'bar/foo');
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Find('.foo') = 'bar/foo');
    Assert(MT.Find('.bar') = 'foo/bar');
    Assert(MT.Prepared);

    AssertExcept(DoMediaTypesFindEmptyMediaExt,
      EArgumentException, SBrookEmptyMediaExt, [MT]);
    AssertExcept(DoMediaTypesFindInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [MT]);
    AssertExcept(DoMediaTypesFindEmptyMediaType,
      EArgumentException, SBrookEmptyMediaType, [MT]);
    AssertExcept(DoMediaTypesFindInvalidMediaType,
      EBrookMediaTypes, Format(SBrookInvalidMediaType, ['bar']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesCount;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(MT.Count = 0);
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Count = 2);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesClear;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(MT.Count = 0);
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Count = 2);
    MT.Clear;
    Assert(MT.Count = 0);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesDefaultTypeEmptyMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).DefaultType := '';
end;

procedure DoMediaTypesDefaultTypeInvalidMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).DefaultType := 'bar';
end;

procedure Test_MediaTypesDefaultType;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(MT.DefaultType = BROOK_CT_OCTET_STREAM);
    MT.DefaultType := 'foo/bar';
    Assert(MT.DefaultType = 'foo/bar');

    AssertExcept(DoMediaTypesDefaultTypeEmptyMediaType,
      EArgumentException, SBrookEmptyMediaType, [MT]);
    AssertExcept(DoMediaTypesDefaultTypeInvalidMediaType,
      EBrookMediaTypes, Format(SBrookInvalidMediaType, ['bar']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesPrepared;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.Prepared);
    MT.Prepare;
    Assert(MT.Prepared);
  finally
    MT.Free;
  end
end;

begin
  TBrookLibraryLoader.Load;
  Test_MediaTypesCreate;
  // Test_MediaTypesDestroy - not required
  Test_MediaTypesGetRegisterAlias;
  Test_MediaTypesGetDescription;
  Test_MediaTypesIsValid;
  Test_MediaTypesIsText;
  Test_MediaTypesIsExt;
  Test_MediaTypesNormalizeExt;
  Test_MediaTypesPrepare;
  Test_MediaTypesAdd;
  Test_MediaTypesRemove;
  Test_MediaTypesTryType;
  Test_MediaTypesFind;
  Test_MediaTypesCount;
  Test_MediaTypesClear;
  Test_MediaTypesDefaultType;
  Test_MediaTypesPrepared;
end.
