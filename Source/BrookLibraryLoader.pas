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

{ Dynamic library loader. }

unit BrookLibraryLoader;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  Classes,
  libsagui,
  BrookHandledClasses,
  BrookUtility;

resourcestring
  { Indicates not allowed operation when the library loader is loaded. }
  SBrookActiveLibLoader = 'Active library loader.';
  { Indicates library already loaded by other loader. }
  SBrookLibraryAlreadyLoaded = 'Library already loaded by other loader.';

type
  { Class for dynamic library loading. }
  TBrookLibraryLoader = class(TBrookHandledComponent)
  public const
    { Default library name. }
    LIB_NAME = SG_LIB_NAME;
  private
    FActive: Boolean;
    FVersion: string;
    FHandle: TLibHandle;
    FLibraryName: TFileName;
    FStreamedActive: Boolean;
    FOnLoad: TNotifyEvent;
    FOnUnload: TNotifyEvent;
    function IsActiveStored: Boolean;
    function IsLibraryNameStored: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetLibraryName(const AValue: TFileName);
    procedure InternalOpen; inline;
  protected
    procedure Loaded; override;
    procedure CheckInactive; inline;
    procedure CheckUnloaded; inline;
    function GetHandle: Pointer; override;
  public
    { Creates an instance of @link(TBrookLibraryLoader).
      @param(AOwner[in] Owner component.) }
    constructor Create(AOwner: TComponent); override;
    { Destroys an instance of @link(TBrookLibraryLoader). }
    destructor Destroy; override;
    { Loads the library dynamically.
      @param(ALibraryName Library name.)
      @returns(@True if the library is succesfully loaded.) }
    class function Load(const ALibraryName: TFileName): Boolean; overload; static;
    { Loads the library dynamically.
      @returns(@True if the library is succesfully loaded.) }
    class function Load: Boolean; overload; static;
    { Unloads the library dynamically.
      @returns(@True if the library is succesfully unloaded.) }
    class function Unload: Boolean; overload; static;
    { Loads the library dynamically. }
    procedure Open; virtual;
    { Unloads the library dynamically. }
    procedure Close; virtual;
    { @exclude }
    procedure DefineProperties(AFiler: TFiler); override;
  published
    { Loads/Unloads the library dynamically. }
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
    { Specifies the library to be loaded dynamically. }
    property LibraryName: TFileName read FLibraryName write SetLibraryName
      stored IsLibraryNameStored;
    { Version of the loaded library. }
    property Version: string read FVersion stored False;
    { Notifies that the library is loaded. }
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
    { Notifies that the library is unloaded. }
    property OnUnload: TNotifyEvent read FOnUnload write FOnUnload;
  end;

implementation

constructor TBrookLibraryLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLibraryName := SG_LIB_NAME;
end;

destructor TBrookLibraryLoader.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TBrookLibraryLoader.CheckInactive;
begin
  if not (csLoading in ComponentState) and Active then
    raise EInvalidOpException.Create(SBrookActiveLibLoader);
end;

procedure TBrookLibraryLoader.CheckUnloaded;
begin
  if not (csLoading in ComponentState) and SgLib.IsLoaded then
    raise EInvalidOpException.Create(SBrookLibraryAlreadyLoaded);
end;

procedure TBrookLibraryLoader.InternalOpen;
begin
  FHandle := SgLib.Load(FLibraryName);
  FActive := FHandle <> NilHandle;
  if FActive then
    FVersion := Sagui.VersionStr
  else
    FVersion := '';
  if Assigned(FOnLoad) then
    FOnLoad(Self);
end;

procedure TBrookLibraryLoader.Loaded;
begin
  inherited Loaded;
  if FActive then
    Open;
end;

procedure TBrookLibraryLoader.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  if FActive and not FStreamedActive then
  begin
    FStreamedActive := True;
    InternalOpen;
  end;
end;

function TBrookLibraryLoader.GetHandle: Pointer;
begin
  Result := @FHandle;
end;

class function TBrookLibraryLoader.Load(const ALibraryName: TFileName): Boolean;
begin
  Result := SgLib.Load(ALibraryName) <> NilHandle;
end;

class function TBrookLibraryLoader.Load: Boolean;
begin
  Result := SgLib.Load(LIB_NAME) <> NilHandle;
end;

class function TBrookLibraryLoader.Unload: Boolean;
begin
  Result := SgLib.Unload = NilHandle;
end;

procedure TBrookLibraryLoader.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csLoading in ComponentState then
  begin
    if AValue then
      CheckUnloaded;
    FActive := AValue;
  end
  else
    if AValue then
      Open
    else
      Close;
end;

function TBrookLibraryLoader.IsActiveStored: Boolean;
begin
  Result := FActive;
end;

function TBrookLibraryLoader.IsLibraryNameStored: Boolean;
begin
  Result := CompareText(FLibraryName, SG_LIB_NAME) <> 0;
end;

procedure TBrookLibraryLoader.SetLibraryName(const AValue: TFileName);
begin
  if AValue = FLibraryName then
    Exit;
  CheckInactive;
  FLibraryName := AValue;
  if FLibraryName = '' then
    FLibraryName := SG_LIB_NAME;
end;

procedure TBrookLibraryLoader.Open;
begin
  if FActive then
    Exit;
  if not FStreamedActive then
    CheckUnloaded;
  InternalOpen;
end;

procedure TBrookLibraryLoader.Close;
begin
  if not FActive then
    Exit;
  FHandle := SgLib.Unload;
  FActive := FHandle <> NilHandle;
  if not FActive then
    FVersion := '';
  if Assigned(FOnUnload) then
    FOnUnload(Self);
end;

end.
