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
  { Callback signature used by stuff that handle errors.
    @param(ASender[in] Sender object.)
    @param(AException[in] Exception object.)}
  TBrookErrorEvent = procedure(ASender: TObject;
    AException: Exception) of object;

  { Global Sagui object containing general purpose functions. }
  Sagui = record
    { Returns the library version number.
      @return(Library version packed into a single integer.) }
    class function Version: Cardinal; overload; static;
    { Returns the library version number.
      @param(AMajor[out] Major number.)
      @param(AMinor[out] Minor number.)
      @param(APatch[out] Patch number.)
      @return(Library version packed into a single integer.) }
    class function Version(out AMajor, AMinor: Byte;
      out APatch: SmallInt): Cardinal; overload; static;
    { Returns the library version number as string in the
      format @code(<MAJOR>.<MINOR>.<PATCH>).
      @return(Library version packed into a static string.) }
    class function VersionStr: string; static;
    { Allocates a new memory space.
      @param(ASize[in] Memory size to be allocated.)
      @return(Pointer of the allocated zero-initialized memory.

        @bold(Returns values:)
        @definitionList(
          @itemLabel(@code(nil))
            @item(If size is @code(0) or no memory space.)
        )
      ) }
    class function Malloc(ASize: NativeUInt): Pointer; static;
    { Allocates a new zero-initialized memory space.
      @param(ASize[in] Memory size to be allocated.)
      @return(Pointer of the allocated zero-initialized memory.

        @bold(Returns values:)
        @definitionList(
          @itemLabel(@code(nil))
            @item(If size is @code(0) or no memory space.)
        )
      ) }
    class function Alloc(ASize: NativeUInt): Pointer; static;
    { Reallocates an existing memory block.
      @param(APointer[in,out] Pointer of the memory to be reallocated.)
      @param(ASize[in] Memory size to be allocated.)
      @return(Pointer of the reallocated memory.) }
    class function Realloc(APointer: Pointer;
      ASize: NativeUInt): Pointer; static;
    { Frees a memory space previous allocated by @link(Sagui.Malloc),
      @link(Sagui.Alloc) or @link(Sagui.Realloc).
      @param(APointer[in] Pointer of the memory to be freed.) }
    class procedure Free(APointer: Pointer); static;
    { Returns string describing an error number.
      @param(AErrorNum[in] Error number.)
      @param(AErrorMsg[out] Referenced string to store the error message.)
      @param(AErrorLen[in] Length of the error message.) }
    class procedure StrError(AErrorNum: Integer; out AErrorMsg: string;
      AErrorLen: Integer); overload; static; inline;
    { Returns string describing an error number.
      @param(AErrorNum[in] Error number.)
      @return(Static string describing the error.) }
    class function StrError(AErrorNum: Integer): string; overload; static;
    { Checks if a string is a HTTP post method. }
    class function IsPost(const AMethod: string): Boolean; static;
    { Extracts the entry-point of a path or resource. For example, given a path
      @code(/api1/customer), the part considered as entry-point is
      @code(/api1).
      @param(APath[in] Path as static string.)
      @return(Entry-point as static string.) }
    class function ExtractEntryPoint(const APath: string): string; static;
    { Returns the system temporary directory.
      @Return(Temporary directory as static string.) }
    class function TmpDir: string; static;
    { Indicates the end-of-read processed in
      @link(TBrookHTTPResponse.SendStream).
      @param(AError[in] @True to return a value indicating a stream
       reading error.)
      @return(Value to end a stream reading.) }
    class function EOR(AError: Boolean): NativeInt; static;
  end;

  { Global Brook object containing general purpose functions. }
  Brook = record
    { Fixes a path by including the leading path delimiter and excluding the
      trailing one. }
    class function FixPath(const APath: string): string; static; inline;
    { Extracts and fixes an entry-point by including the leading path delimiter
      and excluding the trailing one. }
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

class function Sagui.Malloc(ASize: NativeUInt): Pointer;
begin
  SgLib.Check;
  Result := sg_malloc(ASize);
end;

class function Sagui.Alloc(ASize: NativeUInt): Pointer;
begin
  SgLib.Check;
  Result := sg_alloc(ASize);
end;

class function Sagui.Realloc(APointer: Pointer; ASize: NativeUInt): Pointer;
begin
  SgLib.Check;
  Result := sg_realloc(APointer, ASize);
end;

class procedure Sagui.Free(APointer: Pointer);
begin
  SgLib.Check;
  sg_free(APointer);
end;

class procedure Sagui.StrError(AErrorNum: Integer; out AErrorMsg: string;
  AErrorLen: Integer);
var
  P: array[0..SG_ERR_SIZE-1] of cchar;
begin
  SgLib.Check;
  sg_strerror(AErrorNum, @P[0], AErrorLen);
  AErrorMsg := TMarshal.ToString(@P[0]);
end;

class function Sagui.StrError(AErrorNum: Integer): string;
begin
  Sagui.StrError(AErrorNum, Result, SG_ERR_SIZE);
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

class function Sagui.EOR(AError: Boolean): NativeInt;
begin
  SgLib.Check;
  Result := sg_eor(AError);
end;

{ Brook }

class function Brook.FixPath(const APath: string): string;
begin
  Result := APath;
  if not APath.StartsWith('/') then
    Result := Concat('/', Result);
  if (Length(APath) > SizeOf(Char)) and Result.EndsWith('/') then
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
