(*  _                     _
 * | |__  _ __ ___   ___ | | __
 * | '_ \| '__/ _ \ / _ \| |/ /
 * | |_) | | | (_) | (_) |   <
 * |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2021 Silvio Clecio <silvioprog@gmail.com>
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

{ Utility functions of the framework. }

unit BrookUtility;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysUtils,
  DateUtils,
  Classes,
  TypInfo,
  SyncObjs,
{$IFDEF FPC}
  SHA1,
  HttpProtocol,
{$ELSE}
  System.Hash,
  System.NetEncoding,
{$ENDIF}
  Marshalling,
  libsagui;

const
  { Primitive kinds. }
  tkPrimitives = tkProperties -
{$IFDEF FPC}
    [tkArray..tkObject] - [tkInterfaceRaw] - [tkProcVar] - [tkHelper..tkPointer]
{$ELSE}
    [tkClass] - [tkArray..tkInterface] -
      [tkClassRef..{$IF CompilerVersion >= 33.0}tkMRecord{$ELSE}tkProcedure{$ENDIF}]
{$ENDIF};

type
  { Event signature used by stuff that handles errors.
    @param(ASender[in] Sender object.)
    @param(AException[in] Exception object.) }
  TBrookErrorEvent = procedure(ASender: TObject;
    AException: Exception) of object;

  { Allows to lock other threads from accessing a block of code. }
  TBrookLocker = class(TPersistent)
  private
    FMutex: TCriticalSection;
    FActive: Boolean;
    procedure SetActive(AValue: Boolean);
    function IsActiveStored: Boolean;
  protected
    property Mutex: TCriticalSection read FMutex;
    function CreateMutex: TCriticalSection; virtual;
  public
    { Creates an instance of @code(TBrookLocker). }
    constructor Create; virtual;
    { Frees an instance of @code(TBrookLocker). }
    destructor Destroy; override;
    { Locks all other threads. }
    procedure Lock; virtual;
    { Unlocks all other threads. }
    procedure Unlock; virtual;
    { Tries to lock all other threads. }
    function  TryLock: Boolean; virtual;
  published
    { Activates the locker. (Default: @True) }
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
  end;

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
    { Frees a memory space previous allocated by @code(Sagui.Malloc),
      @link(Sagui.Alloc) or @code(Sagui.Realloc).
      @param(APointer[in] Pointer of the memory to be freed.) }
    class procedure Free(APointer: Pointer); static;
    { Returns string describing an error number.
      @param(AErrorNum[in] Error number.)
      @param(AErrorMsg[out] Referenced string to store the error message.)
      @param(AErrorLen[in] Length of the error message.) }
    class procedure StrError(AErrorNum: Integer; out AErrorMsg: string;
      AErrorLen: Integer); overload; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    { Returns string describing an error number.
      @param(AErrorNum[in] Error number.)
      @returns(Static string describing the error.) }
    class function StrError(AErrorNum: Integer): string; overload; static;
    { Checks if a string is an HTTP post method.
      @param(AMethod[in] HTTP verb.)
      @returns(True if given method is POST, PUT, DELETE or OPTIONS.) }
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
      @code(TBrookHTTPResponse.SendStream).
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
    { Holds the name of days as 'Aaa' format. }
    DAYS: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu',
      'Fri', 'Sat');
    { Holds the name of months as 'Aaa' format. }
    MONTHS: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May',
      'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
{$IFNDEF FPC}
  {$WRITEABLECONST OFF}
{$ENDIF}
    { Fixes a path by including the leading path delimiter and excluding the
      trailing one.
      @param(APath[in] Path as static string.)
      @returns(Fixed path, e.g.: path -> /path and /path/ -> /path) }
    class function FixPath(const APath: string): string; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Extracts and fixes an entry-point by including the leading path delimiter
      and excluding the trailing one.
      @param(APath[in] Path as static string.)
      @returns(Fixed entry-point, e.g.: /foo/bar -> /foo ) }
    class function FixEntryPoint(const APath: string): string; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Converts a given local time to UTC (Coordinated Universal Time).
      @param(ADateTime[in] Local date/time.)
      @returns(Local time converted to UTC.) }
    class function DateTimeToUTC(ADateTime: TDateTime): TDateTime; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Converts a given local time to GMT (Greenwich Mean Time).
      @param(ADateTime[in] Local date/time.)
      @returns(Local time converted to GMT string.) }
    class function DateTimeToGMT(ADateTime: TDateTime): string; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Generates a given string to SHA-1 (Secure Hash Algorithm 1).
      @param(S[in] String to generate the SHA-1.)
      @returns(Generated SHA-1 as static string.) }
    class function SHA1(const S: string): string; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  { HTTP verbs enumeration. }
  TBrookHTTPRequestMethod = (rmUnknown, rmGET, rmPOST, rmPUT, rmDELETE, rmPATCH,
    rmOPTIONS, rmHEAD);

  { Set of HTTP verbs. }
  TBrookHTTPRequestMethods = set of TBrookHTTPRequestMethod;

  { Type helper for HTTP verb conversion. }
  TBrookHTTPRequestMethodHelper = record helper for TBrookHTTPRequestMethod
  public const
    { Holds the name of HTTP verbs. }
    METHODS: array[TBrookHTTPRequestMethod] of string = ('Unknown', 'GET',
      'POST', 'PUT', 'DELETE', 'PATCH', 'OPTIONS', 'HEAD');
  public
    { Converts a @code(TBrookHTTPRequestMethod) to string. }
    function ToString: string; inline;
    { Returns a @code(TBrookHTTPRequestMethod) from a string. }
    function FromString(const AMethod: string): TBrookHTTPRequestMethod;
{$IFNDEF DEBUG}inline;{$ENDIF}
  end;

implementation

{ TBrookLocker }

constructor TBrookLocker.Create;
begin
  inherited Create;
  FMutex := CreateMutex;
  FActive := True;
end;

destructor TBrookLocker.Destroy;
begin
  FMutex.Free;
  inherited Destroy;
end;

function TBrookLocker.CreateMutex: TCriticalSection;
begin
  Result := TCriticalSection.Create;
end;

procedure TBrookLocker.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FMutex.Acquire;
  try
    FActive := AValue;
  finally
    FMutex.Release;
  end;
end;

function TBrookLocker.IsActiveStored: Boolean;
begin
  Result := not FActive;
end;

procedure TBrookLocker.Lock;
begin
  if FActive then
    FMutex.Acquire;
end;

procedure TBrookLocker.Unlock;
begin
  if FActive then
    FMutex.Release;
end;

function TBrookLocker.TryLock: Boolean;
begin
  Result := FActive and FMutex.TryEnter;
end;

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
  AErrorMsg := TMarshal.ToString(@P[0]).TrimRight;
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

class function Brook.SHA1(const S: string): string;
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
