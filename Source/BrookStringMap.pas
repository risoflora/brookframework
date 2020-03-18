(*  _                     _
 * | |__  _ __ ___   ___ | | __
 * | '_ \| '__/ _ \ / _ \| |/ /
 * | |_) | | | (_) | (_) |   <
 * |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2020 Silvio Clecio <silvioprog@gmail.com>
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

{ String map used to represent HTML fields, query-string parameters and more. }

unit BrookStringMap;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysUtils,
  StrUtils,
  Classes,
  TypInfo,
  Platform,
  Marshalling,
  libsagui,
  BrookUtility,
  BrookHandledClasses;

type
  TBrookStringMap = class;

  { Identifies the kind of operation in the map.
    @value(sgmoNone None operation or map cleaned.)
    @value(sgmoAdd Pair added to the map.)
    @value(sgmoAddOrSet Pair added or set to the map.)
    @value(sgmoRemove Pair removed from the map.) }
  TBrookStringMapOperation = (sgmoNone, sgmoAdd, sgmoAddOrSet, sgmoRemove);

  { Event signature used to notify a change in the map.
    @param(ASender Event caller.)
    @param(AOperation Operation kind.) }
  TBrookStringMapChangeEvent = procedure(ASender: TObject;
    AOperation: TBrookStringMapOperation) of object;

  { Pair item of @link(TBrookStringMap). }
  TBrookStringPair = record
  private
    FName: string;
    FValue: string;
  public
    { Initializes a variable of @link(TBrookStringPair).
      @param(AName[in] Name of the pair.)
      @param(AValue[in] Value of the pair.) }
    constructor Create(const AName, AValue: string);
    { Name of the pair. }
    property Name: string read FName;
    { Value of the pair. }
    property Value: string read FValue;
  end;

  { Enumerator used to iterate the map @link(TBrookStringMap). }
  TBrookStringMapEnumerator = class
  private
    FMap: TBrookStringMap;
    FCurr: TBrookStringPair;
    FBOF: Boolean;
  public
    { Creates an instance of @link(TBrookStringMapEnumerator).
      @param(AMap[in] Pairs map.) }
    constructor Create(AMap: TBrookStringMap);
    { Gets the current pair.
      @returns(Current pair.) }
    function GetCurrent: TBrookStringPair;
    { Moves to the next pair.
      @returns(@True when move next reaches the EOF.) }
    function MoveNext: Boolean;
    { Same to @link(GetCurrent). }
    property Current: TBrookStringPair read GetCurrent;
  end;

  { Function signature used by @link(TBrookStringMap.Iterate).
    @param(AData[in,out] User-defined data.)
    @param(APair[out] Current iterated pair.) }
  TBrookStringMapIterator = function(AData: Pointer;
    APair: TBrookStringPair): Integer;

  { Function signature used by @link(TBrookStringMap.Sort).
    @param(AData[in,out] User-defined data.)
    @param(APairA[out] Current left pair (A).)
    @param(APairB[out] Current right pair (B).) }
  TBrookStringMapComparator = function(AData: Pointer;
    APairA, APairB: TBrookStringPair): Integer;

  { String map class and its related methods. }
  TBrookStringMap = class(TBrookHandledPersistent)
  private
    FClearOnDestroy: Boolean;
    FNextHandle: Psg_strmap;
    FHandle: PPsg_strmap;
    FOnChange: TBrookStringMapChangeEvent;
    function GetCount: Integer;
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
  protected
    class function CreatePair(
      Apair: Psg_strmap): TBrookStringPair; static; inline;
    class function DoIterate(Acls: Pcvoid;
      Apair: Psg_strmap): cint; cdecl; static;
    class function DoSort(Acls: Pcvoid; Apair_a: Psg_strmap;
      Apair_b: Psg_strmap): cint; cdecl; static;
    function GetHandle: Pointer; override;
    function IsEOF: Boolean; virtual;
    procedure DoChange(AOperation: TBrookStringMapOperation); virtual;
  public
    { Creates an instance of @link(TBrookStringMap).
      @param(AHandle[in] Pointer to store the string map handle.) }
    constructor Create(AHandle: Pointer); virtual;
    { Frees an instance of @link(TBrookStringMap). }
    destructor Destroy; override;
    { Copies the properties of the source string map.
      @param(ASource[in] String map source to be copied.) }
    procedure Assign(ASource: TPersistent); override;
    { Checks if the map is empty.
      @returns(@True when map is empty, @False otherwise.) }
    function IsEmpty: Boolean; virtual;
    { Gets an instance of @link(TBrookStringMapEnumerator). }
    function GetEnumerator: TBrookStringMapEnumerator;
    { Adds a pair of strings to the map.
      @param(AName[in] Name of the pair.)
      @param(AValue[in] Value of the pair.) }
    procedure Add(const AName, AValue: string); virtual;
    { Adds or sets a pair of strings to the map.
      @param(AName[in] Name of the pair.)
      @param(AValue[in] Value of the pair.) }
    procedure AddOrSet(const AName, AValue: string); virtual;
    { Removes a pair by its name.
      @param(AName[in] Name of the pair.) }
    procedure Remove(const AName: string); virtual;
    { Clears the entire map. }
    procedure Clear; virtual;
    { Finds a pair by its name.
      @param(AName[in] Name of the pair.)
      @param(APair[out] Reference to store found pair.)
      @returns(@True when pair is found, @False otherwise.) }
    function Find(const AName: string;
      out APair: TBrookStringPair): Boolean; virtual;
    { Checks if map contains a pair by its name.
      @param(AName[in] Name of the pair.)
      @returns(@True when map contains the pair, @False otherwise.) }
    function Contains(const AName: string): Boolean; virtual;
    { Gets a pair by name and return its value.
      @param(AName[in] Name of the pair.)
      @returns(Pair value.) }
    function Get(const AName: string): string; virtual;
    { Tries to find a pair value by its name.
      @param(AName[in] Name of the pair.)
      @param(AValue[out] Reference to store found value.)
      @returns(@True when pair is found, @False otherwise.) }
    function TryValue(const AName: string;
      out AValue: string): Boolean; virtual;
    { Retrieves the first pair in the map.
      @param(APair[out] First pair returned.)
      @returns(@True when pair is found, @False otherwise.) }
    function First(out APair: TBrookStringPair): Boolean; virtual;
    { Retrieves the next pair in the map.
      @param(APair[out] Next pair returned.) }
    function Next(out APair: TBrookStringPair): Boolean; virtual;
    { Iterates over pairs map.
      @param(AIterator[in] Function to iterate the pairs.)
      @param(AData[in,out] User-specified value.) }
    procedure Iterate(AIterator: TBrookStringMapIterator;
      AData: Pointer); virtual;
    { Sorts the pairs map.
      @param(AComparator[in] Function to sort the pairs.)
      @param(AData[in,out] User-specified value.) }
    procedure Sort(AComparator: TBrookStringMapComparator;
      AData: Pointer); virtual;
    { Fetches a string map as an object.
      @param(AObject[in] Object with properties that correspond to the fetched
      string map.)
      @param(AAllowed[in] Array of properties to be allowed when fetching.)
      @param(AIgnored[in] Array of properties to be ignored when fetching.) }
    procedure Fetch(AObject: TObject; const AAllowed,
      AIgnored: array of string); overload; virtual;
    { Fetches a string map as an object. }
    procedure Fetch(AObject: TObject); overload; virtual;
    { Gets the map as big string using equal sign to separate each pair and
      ending lines using line break. }
    function ToString: string; override;
    { Counts the total pairs present in the map. }
    property Count: Integer read GetCount;
    { Adds or gets the pair value. }
    property Values[const AName: string]: string read GetValue
      write SetValue; default;
    { Indicates the end of map. }
    property EOF: Boolean read IsEOF; //FI:C110
    { Indicates if the map is empty. }
    property Empty: Boolean read IsEmpty; //FI:C110
    { Clears the list on destroy. }
    property ClearOnDestroy: Boolean read FClearOnDestroy write FClearOnDestroy;
    { Notifies a change in the map. }
    property OnChange: TBrookStringMapChangeEvent read FOnChange write FOnChange;
  end;

implementation

{ TBrookStringPair }

constructor TBrookStringPair.Create(const AName, AValue: string);
begin
  FName := AName;
  FValue := AValue;
end;

{ TBrookStringMapEnumerator }

constructor TBrookStringMapEnumerator.Create(AMap: TBrookStringMap);
begin
  inherited Create;
  FMap := AMap;
  FMap.First(FCurr);
  FBOF := True;
end;

function TBrookStringMapEnumerator.GetCurrent: TBrookStringPair;
begin
  Result := FCurr;
end;

function TBrookStringMapEnumerator.MoveNext: Boolean;
begin
  if FBOF then
    FBOF := False
  else
    FMap.Next(FCurr);
  Result := not FMap.EOF;
end;

{ TBrookStringMap }

constructor TBrookStringMap.Create(AHandle: Pointer);
begin
  inherited Create;
  if not Assigned(AHandle) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AHandle']);
  FHandle := AHandle;
  FClearOnDestroy := True;
end;

destructor TBrookStringMap.Destroy;
begin
  try
    if FClearOnDestroy then
      Clear;
  finally
    inherited Destroy;
  end;
end;

procedure TBrookStringMap.Assign(ASource: TPersistent);
var
  VSource: TBrookStringMap;
  VPair: TBrookStringPair;
begin
  if ASource is TBrookStringMap then
  begin
    VSource := ASource as TBrookStringMap;
    Clear;
    for VPair in VSource do
      Add(VPair.Name, VPair.Value);
    FClearOnDestroy := VSource.FClearOnDestroy;
  end
  else
    inherited Assign(ASource);
end;

class function TBrookStringMap.CreatePair(Apair: Psg_strmap): TBrookStringPair;
begin
  SgLib.Check;
  Result := TBrookStringPair.Create(TMarshal.ToString(sg_strmap_name(Apair)),
    TMarshal.ToString(sg_strmap_val(Apair)));
end;

class function TBrookStringMap.DoIterate(Acls: Pcvoid; Apair: Psg_strmap): cint;
var
  M: PMethod;
begin
  M := Acls;
  if not Assigned(M.Code) then
    Exit(-1);
  Result := TBrookStringMapIterator(M.Code)(M.Data, CreatePair(Apair));
end;

class function TBrookStringMap.DoSort(Acls: Pcvoid; Apair_a: Psg_strmap;
  Apair_b: Psg_strmap): cint;
var
  M: PMethod;
begin
  M := Acls;
  if not Assigned(M.Code) then
    Exit(0);
  Result := TBrookStringMapComparator(M.Code)(M.Data, CreatePair(Apair_a),
    CreatePair(Apair_b));
end;

function TBrookStringMap.GetEnumerator: TBrookStringMapEnumerator;
begin
  Result := TBrookStringMapEnumerator.Create(Self);
end;

function TBrookStringMap.IsEmpty: Boolean;
begin
  Result := (not Assigned(FHandle)) or (not Assigned(FHandle^));
end;

function TBrookStringMap.GetCount: Integer;
begin
  SgLib.Check;
  Result := sg_strmap_count(FHandle^);
end;

function TBrookStringMap.GetValue(const AName: string): string;
begin
  if not TryValue(AName, Result) then
    Result := '';
end;

procedure TBrookStringMap.SetValue(const AName, AValue: string);
begin
  AddOrSet(AName, AValue);
end;

function TBrookStringMap.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookStringMap.IsEOF: Boolean;
begin
  Result := not Assigned(FNextHandle);
end;

procedure TBrookStringMap.DoChange(AOperation: TBrookStringMapOperation);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, AOperation);
end;

procedure TBrookStringMap.Add(const AName, AValue: string);
var
  M: TMarshaller;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_strmap_add(FHandle, M.ToCString(AName),
    M.ToCString(AValue)));
  DoChange(sgmoAdd);
end;

procedure TBrookStringMap.AddOrSet(const AName, AValue: string);
var
  M: TMarshaller;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_strmap_set(FHandle, M.ToCString(AName),
    M.ToCString(AValue)));
  DoChange(sgmoAddOrSet);
end;

procedure TBrookStringMap.Remove(const AName: string);
var
  R: cint;
  M: TMarshaller;
begin
  SgLib.Check;
  R := sg_strmap_rm(FHandle, M.ToCString(AName));
  if (R <> 0) and (R <> ENOENT) then
    SgLib.CheckLastError(R);
  DoChange(sgmoRemove);
end;

procedure TBrookStringMap.Clear;
begin
  if not Assigned(FHandle^) then
    Exit;
  SgLib.Check;
  sg_strmap_cleanup(FHandle);
  DoChange(sgmoNone);
end;

function TBrookStringMap.Find(const AName: string;
  out APair: TBrookStringPair): Boolean;
var
  R: cint;
  P: Psg_strmap;
  M: TMarshaller;
begin
  SgLib.Check;
  if not Assigned(FHandle^) then
    Exit(False);
  R := sg_strmap_find(FHandle^, M.ToCString(AName), @P);
  Result := R = 0;
  if Result then
    APair := TBrookStringPair.Create(AName, TMarshal.ToString(sg_strmap_val(P)))
  else
    if R <> ENOENT then
      SgLib.CheckLastError(R);
end;

function TBrookStringMap.Contains(const AName: string): Boolean;
var
  P: Psg_strmap;
  M: TMarshaller;
begin
  Result := sg_strmap_find(FHandle^, M.ToCString(AName), @P) = 0;
end;

function TBrookStringMap.Get(const AName: string): string;
var
  M: TMarshaller;
begin
  SgLib.Check;
  Result := TMarshal.ToString(sg_strmap_get(FHandle^, M.ToCString(AName)));
end;

function TBrookStringMap.TryValue(const AName: string;
  out AValue: string): Boolean;
var
  P: Pcchar;
  M: TMarshaller;
begin
  SgLib.Check;
  P := sg_strmap_get(FHandle^, M.ToCString(AName));
  Result := Assigned(P);
  if Result then
    AValue := TMarshal.ToString(P);
end;

function TBrookStringMap.First(out APair: TBrookStringPair): Boolean;
begin
  FNextHandle := FHandle^;
  Result := Assigned(FNextHandle);
  if Result then
    APair := CreatePair(FNextHandle);
end;

function TBrookStringMap.Next(out APair: TBrookStringPair): Boolean;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_strmap_next(@FNextHandle));
  Result := Assigned(FNextHandle);
  if Result then
    APair := CreatePair(FNextHandle);
end;

procedure TBrookStringMap.Iterate(AIterator: TBrookStringMapIterator;
  AData: Pointer);
var
  R: cint;
  M: TMethod;
begin
  SgLib.Check;
  if not Assigned(FHandle^) then
    Exit;
  M.Code := @AIterator;
  M.Data := AData;
  R := sg_strmap_iter(FHandle^,
{$IFNDEF VER3_0}@{$ENDIF}DoIterate, @M);
  if R <> -1 then
    SgLib.CheckLastError(R);
end;

procedure TBrookStringMap.Sort(AComparator: TBrookStringMapComparator;
  AData: Pointer);
var
  M: TMethod;
begin
  SgLib.Check;
  M.Code := @AComparator;
  M.Data := AData;
  SgLib.CheckLastError(sg_strmap_sort(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoSort, @M));
end;

procedure TBrookStringMap.Fetch(AObject: TObject; const AAllowed,
  AIgnored: array of string);
var
  VPair: TBrookStringPair;
  VProp: PPropInfo;
begin
  if not Assigned(AObject) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AObject']);
  for VPair in Self do
  begin
    VProp := GetPropInfo(AObject, VPair.Name, tkPrimitives);
    if Assigned(VProp) and Assigned(VProp^.SetProc) and (not
      (((Length(AAllowed) > 0) and (not AnsiMatchText(VPair.Name, AAllowed))) or
      ((Length(AIgnored) > 0) and AnsiMatchText(VPair.Name, AIgnored)))) then
      SetPropValue(AObject, VProp, VPair.Value);
  end;
end;

procedure TBrookStringMap.Fetch(AObject: TObject);
begin
  Fetch(AObject, [], []);
end;

function TBrookStringMap.ToString: string;
var
  P: TBrookStringPair;
begin
  Result := '';
  for P in Self do
    Result := Concat(Result, P.Name, '=', P.Value, sLineBreak);
end;

end.
