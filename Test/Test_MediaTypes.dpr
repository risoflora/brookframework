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
  BrookExtra,
  BrookMediaTypes;

type

  { TFakeMediaTypes }

  TFakeMediaTypes = class(TBrookMediaTypes)
  protected
    function IsPrepared: Boolean; override;
  public
    class function GetDescription: string; override;
    procedure Prepare; override;
    property Cache;
  end;


{ TFakeMediaTypes }

function TFakeMediaTypes.IsPrepared: Boolean;
begin
  Result := False;
end;

class function TFakeMediaTypes.GetDescription: string;
begin
  Result := 'Fake';
end;

procedure TFakeMediaTypes.Prepare;
begin
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

begin
  Test_MediaTypesCreate;
  // Test_MediaTypesDestroy - not required
  Test_MediaTypesGetRegisterAlias;
  // Test_MediaTypesGetDescription - abstract
  Test_MediaTypesIsValid;
  Test_MediaTypesIsText;
  Test_MediaTypesIsExt;
  Test_MediaTypesNormalizeExt;
end.
