(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  Microframework which helps to develop web Pascal applications.
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

{ Utility functions of the framework. }

unit BrookUtility;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  Marshalling,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  libsagui;

type
  TBrookErrorEvent = procedure(ASender: TObject;
    AException: Exception) of object;

  Sagui = record
    {
      Returns the library version number.
      @return(Library version packed into a single integer.)
    }
    class function Version: Cardinal; overload; static;
    { experimental }
    class function Version(out AMajor, AMinor: Byte;
      out APatch: SmallInt): Cardinal; overload; static;
    {
      Returns the library version number as string.
      @return(Library version packed into a static string.)
    }
    class function VersionStr: string; static;
    {
      Allocates a new memory space and zero-initialize it.

      @param(ASize[in] Memory size to be allocated.)

      @return(Pointer of the allocated zero-initialized memory.

        @bold(Returns values:)

        @definitionList(
          @itemLabel(@code(nil))
          @item(When size is @code(0) or no memory space.)
        )
      )
    }
    class function Alloc(ASize: NativeUInt): Pointer; static;
    {
      Frees a memory space previous allocated by @link(BrookAlloc).
      @param(APtr[in] Pointer of the memory to be freed.)
    }
    class procedure Free(APtr: Pointer); static;
    { experimental }
    class function StrError(AErrorNum: Integer): string; static;
    { experimental }
    class function IsPost(const AMethod: string): Boolean; static;
    { experimental }
    class function ExtractEntryPoint(const APath: string): string; static;
    { experimental }
    class function TmpDir: string; static;
  end;

  Brook = record
    { experimental }
    class function FixPath(const APath: string): string; static; inline;
    { experimental }
    class function FixEntryPoint(const APath: string): string; static;
  end;

implementation

{ Sagui }

class function Sagui.Version: Cardinal;
begin
  SgLib.Check;
  Result := sg_version;
end;

class function Sagui.Version(out AMajor, AMinor: Byte;
  out APatch: SmallInt): Cardinal;
begin
  SgLib.Check;
  Result := sg_version;
  AMajor := (Result shr 16) and $FF;
  AMinor := (Result shr 8) and $FF;
  APatch := Result and $FF;
end;

class function Sagui.VersionStr: string;
begin
  SgLib.Check;
  Result := TMarshal.ToString(sg_version_str);
end;

class function Sagui.Alloc(ASize: NativeUInt): Pointer;
begin
  SgLib.Check;
  Result := sg_alloc(ASize);
end;

class procedure Sagui.Free(APtr: Pointer);
begin
  SgLib.Check;
  sg_free(APtr);
end;

class function Sagui.StrError(AErrorNum: Integer): string;
var
  P: array[0..SG_ERR_SIZE-1] of cchar;
begin
  SgLib.Check;
  sg_strerror(AErrorNum, @P[0], SG_ERR_SIZE);
  Result := TMarshal.ToString(@P[0]);
end;

class function Sagui.IsPost(const AMethod: string): Boolean;
var
  M: TMarshaller;
begin
  SgLib.Check;
  Result := sg_is_post(M.ToCString(AMethod));
end;

class function Sagui.ExtractEntryPoint(const APath: string): string;
var
  M: TMarshaller;
  S: Pcchar;
begin
  SgLib.Check;
  S := sg_extract_entrypoint(M.ToCString(APath));
  try
    Result := TMarshal.ToString(S);
  finally
    sg_free(S);
  end;
end;

class function Sagui.TmpDir: string;
var
  S: Pcchar;
begin
  SgLib.Check;
  S := sg_tmpdir;
  try
    Result := TMarshal.ToString(S);
  finally
    sg_free(S);
  end;
end;

{ Brook }

class function Brook.FixPath(const APath: string): string;
begin
  Result := APath;
  if not APath.StartsWith('/') then
    Result := Concat('/', Result);
  if (Length('/') > SizeOf(Char)) and Result.EndsWith('/') then
    SetLength(Result, Length(Result) - Length('/'));
end;

class function Brook.FixEntryPoint(const APath: string): string;
var
  PS: TArray<string>;
begin
  PS := APath.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  Result := '/';
  if Length(PS) > 0 then
    Result := Concat(Result, PS[0]);
end;

end.
