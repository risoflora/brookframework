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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

{ Utility functions of the framework. }

unit BrookUtility;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysUtils,
  DateUtils,
{$IFDEF FPC}
  SHA1,
  HttpProtocol,
{$ELSE}
  System.Hash,
  System.NetEncoding,
{$ENDIF}
  Marshalling,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  libsagui;

type
  { Event signature used by stuff that handle errors.
    @param(ASender[in] Sender object.)
    @param(AException[in] Exception object.) }
  TBrookErrorEvent = procedure(ASender: TObject;
    AException: Exception) of object;

  { Global Sagui object containing general purpose functions. }
  Sagui = record
    { Returns the library version number.
      @returns(Library version packed into a single integer.) }
    class function Version: Cardinal; overload; static;
    { Returns the library version number.
      @param(AMajor[out] Major number.)
      @param(AMinor[out] Minor number.)
      @param(APatch[out] Patch number.)
      @returns(Library version packed into a single integer.) }
    class function Version(out AMajor, AMinor: Byte;
      out APatch: SmallInt): Cardinal; overload; static;
    { Returns the library version number as string in the
      format @code(<MAJOR>.<MINOR>.<PATCH>).
      @returns(Library version packed into a static string.) }
    class function VersionStr: string; static;
    { Allocates a new memory space.
      @param(ASize[in] Memory size to be allocated.)
      @returns(Pointer of the allocated zero-initialized memory.

        @bold(Returns values:)
        @definitionList(
          @itemLabel(@code(nil))
            @item(If size is @code(0) or no memory space.)
        )
      ) }
    class function Malloc(ASize: NativeUInt): Pointer; static;
    { Allocates a new zero-initialized memory space.
      @param(ASize[in] Memory size to be allocated.)
      @returns(Pointer of the allocated zero-initialized memory.

        @bold(Returns values:)
        @definitionList(
          @itemLabel(@code(nil))
            @item(If size is @code(0) or no memory space.)
        )
      ) }
    class function Alloc(ASize: NativeUInt): Pointer; static;
    { Reallocates an existing memory block.
      @param(APointer[in] Pointer of the memory to be reallocated.)
      @param(ASize[in] Memory size to be allocated.)
      @returns(Pointer of the reallocated memory.) }
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
      @returns(Static string describing the error.) }
    class function StrError(AErrorNum: Integer): string; overload; static;
    { Checks if a string is an HTTP post method. }
    class function IsPost(const AMethod: string): Boolean; static;
    { Extracts the entry-point of a path or resource. For example, given a path
      @code(/api1/customer), the part considered as entry-point is
      @code(/api1).
      @param(APath[in] Path as static string.)
      @returns(Entry-point as static string.) }
    class function ExtractEntryPoint(const APath: string): string; static;
    { Returns the system temporary directory.
      @returns(Temporary directory as static string.) }
    class function TmpDir: string; static;
    { Indicates the end-of-read processed in
      @link(TBrookHTTPResponse.SendStream).
      @param(AError[in] @True to return a value indicating a stream
       reading error.)
      @returns(Value to end a stream reading.) }
    class function EOR(AError: Boolean): NativeInt; static;
    { Obtains the IP of a socket handle into a string.
      @param(ASocket[in] Socket handle.)
      @return(Formatted IP into a string.) }
    class function IP(ASocket: Pointer): string; static;
  end;

  { Global Brook object containing general purpose functions. }
  Brook = record
  public const
{$IFNDEF FPC}
  {$WRITEABLECONST ON}
{$ENDIF}
    { TODO: WARNING: This constant is experimental! }
    MONTHS: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May',
      'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    { TODO: WARNING: This constant is experimental! }
    DAYS: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu',
      'Fri', 'Sat');
{$IFNDEF FPC}
  {$WRITEABLECONST OFF}
{$ENDIF}
    { Fixes a path by including the leading path delimiter and excluding the
      trailing one. }
    class function FixPath(const APath: string): string; static; inline;
    { Extracts and fixes an entry-point by including the leading path delimiter
      and excluding the trailing one. }
    class function FixEntryPoint(const APath: string): string; static; inline;
    { TODO: WARNING: This method is experimental! }
    class function DateTimeToUTC(ADateTime: TDateTime): TDateTime; static; inline;
    { TODO: WARNING: This method is experimental! }
    class function DateTimeToGMT(ADateTime: TDateTime): string; static; inline;
    { TODO: WARNING: This method is experimental! }
    class function Sha1(const S: string): string; static; inline;
  end;

  { TODO: documment }
  TBrookHTTPRequestMethod = (rmUnknown, rmGET, rmPOST, rmPUT, rmDELETE, rmPATCH,
    rmOPTIONS, rmHEAD);

  { TODO: documment }
  TBrookHTTPRequestMethods = set of TBrookHTTPRequestMethod;

  { TODO: documment }
  TBrookHTTPRequestMethodHelper = record helper for TBrookHTTPRequestMethod
  public const
    METHODS: array[TBrookHTTPRequestMethod] of string = ('Unknown', 'GET',
      'POST', 'PUT', 'DELETE', 'PATCH', 'OPTIONS', 'HEAD');
  public
    function ToString: string; inline;
    function FromString(const AMethod: string): TBrookHTTPRequestMethod; inline;
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
  P: array[0..Pred(SG_ERR_SIZE)] of cchar;
begin
  SgLib.Check;
  P[0] := 0;
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

class function Sagui.IP(ASocket: Pointer): string;
var
  P: array[0..45] of cchar;
begin
  if not Assigned(ASocket) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['ASocket']);
  SgLib.Check;
  SgLib.CheckLastError(sg_ip(ASocket, @P[0], SizeOf(P)));
  Result := TMarshal.ToString(@P[0]);
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

class function Brook.DateTimeToUTC(ADateTime: TDateTime): TDateTime;
begin
  Result :=
{$IFDEF FPC}
    LocalTimeToUniversal
{$ELSE}
    TTimeZone.Local.ToUniversalTime
{$ENDIF}(ADateTime);
end;

class function Brook.DateTimeToGMT(ADateTime: TDateTime): string;
var
  Y, M, D: Word;
begin
  DecodeDate(ADateTime, Y, M, D);
  DateTimeToString(Result, Format('"%s", dd "%s" yyy hh":"mm":"ss "GMT"', [
    DAYS[DayOfWeek(ADateTime)], MONTHS[M]]), ADateTime);
end;

class function Brook.Sha1(const S: string): string;
begin
  Result :=
{$IFDEF FPC}SHA1Print(SHA1String(S)){$ELSE}THashSHA1.GetHashString(S){$ENDIF};
end;

{ TBrookHTTPRequestMethodHelper }

function TBrookHTTPRequestMethodHelper.ToString: string;
begin
  Result := METHODS[Self];
end;

function TBrookHTTPRequestMethodHelper.FromString(
  const AMethod: string): TBrookHTTPRequestMethod;
var
  M: string;
  I: TBrookHTTPRequestMethod;
begin
  M := AMethod.ToUpper;
  for I := Low(METHODS) to High(METHODS) do
    if SameStr(M, METHODS[I]) then
      Exit(I);
  Result := rmUnknown;
end;

end.
