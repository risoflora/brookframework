(*  _                     _
 * | |__  _ __ ___   ___ | | __
 * | '_ \| '__/ _ \ / _ \| |/ /
 * | |_) | | | (_) | (_) |   <
 * |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2019 Silvio Clecio <silvioprog@gmail.com>
 *
 * Brook framework is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Brook framework; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

{ Useful types for marshalling arguments. }

unit Marshalling;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils;

type

  { TMarshal* }

{$IFDEF FPC}
  TMarshal = record
{$ELSE}
  TMarshalHelper = class helper for TMarshal
{$ENDIF}
  public
    class function ToBytes(const S: MarshaledAString;
      L: NativeUInt): TBytes; static; inline;
    class function ToString(const S: MarshaledAString): string; static; inline;
  end;

  { TMarshaller* }

{$IFDEF FPC}
  TMarshaller = record
{$ELSE}
  TMarshallerHelper = record helper for TMarshaller
{$ENDIF}
  public
    function ToCString(const S: string): MarshaledAString; inline;
    function ToCNullableString(const S: string): MarshaledAString; inline;
  end;

implementation

{ TMarshal* }

class function {$IFDEF FPC}TMarshal{$ELSE}TMarshalHelper{$ENDIF}.ToBytes(
  const S: MarshaledAString; L: NativeUInt): TBytes;
begin
  if (not Assigned(S)) or (L = 0) then
    Exit(nil);
  SetLength(Result, L);
  System.Move(S^, Result[0], L);
end;

class function {$IFDEF FPC}TMarshal{$ELSE}TMarshalHelper{$ENDIF}.ToString(
  const S: MarshaledAString): string;
begin
  if not Assigned(S) then
    Exit('');
{$IFDEF FPC}
  SetString(Result, S, Length(S));
  SetCodePage(RawByteString(Result), CP_UTF8, False);
{$ELSE}
  Result := TMarshal.ReadStringAsUtf8(TPtrWrapper.Create(S));
{$ENDIF}
end;

{ TMarshaller* }

function {$IFDEF FPC}TMarshaller{$ELSE}TMarshallerHelper{$ENDIF}.ToCString(
  const S: string): MarshaledAString;
begin
  Result :=
{$IFDEF FPC}
    MarshaledAString(S)
{$ELSE}
    AsAnsi(S, CP_UTF8).ToPointer
{$ENDIF};
end;

function {$IFDEF FPC}TMarshaller{$ELSE}TMarshallerHelper{$ENDIF}.ToCNullableString(
  const S: string): MarshaledAString;
begin
  if S = '' then
    Exit(nil);
  Result := ToCString(S);
end;

end.
