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

{ Contains classes for handling URL entry-points. }

unit BrookURLEntryPoints;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysUtils,
  Classes,
  Platform,
  Marshalling,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  libsagui,
  BrookUtility,
  BrookHandledClasses,
  BrookExtra,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookURLRouter;

resourcestring
  { Error message @code('Entry-point list not prepared.'). }
  SBrookEntryPointListUnprepared = 'Entry-point list not prepared.';
  { Error message @code('Inactive entry-points.'). }
  SBrookInactiveEntryPoints = 'Inactive entry-points.';
  { Error message @code('No entry-points defined.'). }
  SBrookNoEntryPointsDefined = 'No entry-points defined.';
  { Error message @code('<new-class>: entry-point <entry-point> already
    exists in <existing-class>.'). }
  SBrookEntryPointAlreadyExists =
    '%s: entry-point ''%s'' already exists in ''%s''.';
  { Error message @code('<new-class>: entry-point cannot be empty.'). }
  SBrookEmptyEntryPointName = '%s: entry-point cannot be empty.';
  { Error message @code('Entry-point not found: <entry-point>.'). }
  SBrookEntryPointNotFound = 'Entry-point not found: %s.';
  { Error message @code('Router not assigned for entry-point <entry-point>.'). }
  SBrookRouterNotAssigned = 'Router not assigned for entry-point ''%s''.';

type
  TBrookURLEntryPointList = class;

  { Handles exceptions related to entry-point classes. }
  EBrookURLEntryPoint = class(Exception);

  { Class to represent a URL entry-point item. }
  TBrookURLEntryPoint = class(TBrookHandledCollectionItem)
  private
    FList: TBrookURLEntryPointList;
    FHandle: Psg_entrypoint;
    FName: string;
    FUserData: Pointer;
    function GetName: string;
    procedure SetName(const AValue: string);
    function GetUserData: Pointer;
  protected
    function GetHandle: Pointer; override;
    function GetRouter: TBrookURLRouter; virtual;
    procedure SetRouter(AValue: TBrookURLRouter); virtual;
  public
    { Creates an instance of @code(TBrookURLEntryPoint).
      @param(ACollection[in] Entry-point list.) }
    constructor Create(ACollection: TCollection); override;
    { Checks if the entry-point name is valid. }
    procedure Validate; inline;
    { User-defined data to be stored temporally in the entry-point object. }
    property UserData: Pointer read GetUserData write FUserData;
  published
    { Entry-point item name. }
    property Name: string read GetName write SetName;
    { Referenced router to the entry-point. }
    property Router: TBrookURLRouter read GetRouter write SetRouter;
  end;

  { Class-reference for @code(TBrookURLEntryPoint). }
  TBrookURLEntryPointClass = class of TBrookURLEntryPoint;

  { List enumerator for @code(TBrookURLEntryPointList). }
  TBrookURLEntryPointListEnumerator = class(TCollectionEnumerator)
  public
    { Get current entry-point item. }
    function GetCurrent: TBrookURLEntryPoint;
    { Current entry-point item. }
    property Current: TBrookURLEntryPoint read GetCurrent;
  end;

  { Handles exceptions related to URL entry-point list. }
  EBrookURLEntryPointList = class(Exception);

  { Class to represent an list of URL entry-points. }
  TBrookURLEntryPointList = class(TBrookHandledOwnedCollection)
  private
    FHandle: Psg_entrypoints;
    procedure InternalLibUnloadEvent(ASender: TObject);
  protected
    class function GetEntryPointLabel: string; virtual;
    class function GetEntryPointName(
      AEntryPoint: TBrookURLEntryPoint): string; virtual;
    function GetHandle: Pointer; override;
    function GetItem(AIndex: Integer): TBrookURLEntryPoint; virtual;
    procedure SetItem(AIndex: Integer; AValue: TBrookURLEntryPoint); virtual;
    procedure InternalAdd(AEntryPoint: TBrookURLEntryPoint); virtual;
    procedure CheckPrepared; inline;
  public
    { Creates an instance of @code(TBrookURLEntryPointList).
      @param(AOwner[in] Entry-points persistent.)}
    constructor Create(AOwner: TPersistent); virtual;
    { Frees an instance of @code(TBrookURLEntryPointList). }
    destructor Destroy; override;
    { Gets the default class for entry-point item creation. }
    class function GetEntryPointClass: TBrookURLEntryPointClass; virtual;
    { Creates an enumerator to iterate the entry-points though @code(for..in). }
    function GetEnumerator: TBrookURLEntryPointListEnumerator;
    { Prepares entry-points handle. }
    procedure Prepare; virtual;
    { Unprepares entry-points handle. }
    procedure Unprepare; virtual;
    { Checks if entry-points handle is prepared. }
    function IsPrepared: Boolean; virtual;
    { Creates a new entry-point name. }
    function NewName: string; virtual;
    { Adds a new item to the entry-point list.
      @returns(Entry-point item.) }
    function Add: TBrookURLEntryPoint; virtual;
    { Removes a item from the entry-point list by its name.
      @param(AName[in] Entry-point name.)
      @returns(@True if an entry-point is removed.) }
    function Remove(const AName: string): Boolean; virtual;
    { Gets the entry-point index by its name. }
    function IndexOf(const AName: string): Integer; virtual;
    { Finds an entry-point in the entry-point list by its name.
      @param(AName[in] Entry-point name.)
      @returns(Entry-point item.) }
    function FindInList(const AName: string): TBrookURLEntryPoint; virtual;
    { Finds an user-data in the entry-point list by entry-point path.
      @param(APath[in] Entry-point path.)
      @param(AUserData[out] User-defined data.)
      @returns(@True if user-data is found.) }
    function Find(const APath: string; out AUserData): Boolean; virtual;
    { Clears the entry-point list. }
    procedure Clear; virtual;
    { Gets/sets an entry-point from/to the entry-point list by its index. }
    property Items[AIndex: Integer]: TBrookURLEntryPoint read GetItem
      write SetItem; default;
    { @True if entry-points handle is prepared. }
    property Prepared: Boolean read IsPrepared;
  end;

  { Event signature used by @code(TBrookURLEntryPoints) to notify a not found
    entry-point items. }
  TBrookURLEntryPointsNotFoundEvent = procedure(ASender: TObject;
    const AEntryPoint, APath: string; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse) of object;

  { URL entry-points component. }
  TBrookURLEntryPoints = class(TBrookHandledComponent)
  private
    FActive: Boolean;
    FList: TBrookURLEntryPointList;
    FStreamedActive: Boolean;
    FOnNotFound: TBrookURLEntryPointsNotFoundEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    function IsActiveStored: Boolean;
    procedure SetActive(AValue: Boolean);
    function GetItem(AIndex: Integer): TBrookURLEntryPoint;
    procedure SetItem(AIndex: Integer; AValue: TBrookURLEntryPoint);
    procedure SetList(AValue: TBrookURLEntryPointList);
    procedure InternalLibUnloadEvent(ASender: TObject);
  protected
    function CreateList: TBrookURLEntryPointList; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoRoute(ASender: TObject; const AEntryPoint, APath: string;
      ARouter: TBrookURLRouter; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); virtual;
    procedure DoNotFound(ASender: TObject; const AEntryPoint, APath: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckItems; inline;
    procedure CheckActive; inline;
  public
    { Creates an instance of @code(TBrookURLEntryPoints).
      @param(AOwner[in] Owner component.) }
    constructor Create(AOwner: TComponent); override;
    { Destroys an instance of @code(TBrookURLEntryPoints). }
    destructor Destroy; override;
    { Creates an enumerator to iterate the entry-points though @code(for..in). }
    function GetEnumerator: TBrookURLEntryPointListEnumerator;
    { Adds a new item to the entry-point list.
      @returns(Entry-point item.) }
    function Add: TBrookURLEntryPoint; inline;
    { Removes a item from the entry-point list by its name.
      @param(AName[in] Entry-point name.) }
    procedure Remove(const AName: string); inline;
    { Clears the entry-point list. }
    procedure Clear; inline;
    { Enabled the entry-point component. }
    procedure Open;
    { Disables the entry-point component. }
    procedure Close;
    { Enters into entry-points routing them.
      @param(ASender[in] Sender object.)
      @param(APath[in] Entry-point path.)
      @param(ARequest[in] Request object to pass to the entry-point found.)
      @param(AResponse: Response object to pass to the entry-point found.) }
    procedure Enter(ASender: TObject; const APath: string;
      ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); overload; virtual;
    { Enters into entry-points routing them.
      @param(ASender[in] Sender object.)
      @param(ARequest[in] Request object to pass to the entry-point found.)
      @param(AResponse: Response object to pass to the entry-point found.) }
    procedure Enter(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); overload; virtual;
    { Gets/sets an entry-point from/to the entry-point list by its index. }
    property Items[AIndex: Integer]: TBrookURLEntryPoint read GetItem
      write SetItem; default;
  published
    { Enabled/disables the entry-point component. }
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
    { Available entry-point list. }
    property List: TBrookURLEntryPointList read FList write SetList;
    { Event triggered when an entry-point is not found. }
    property OnNotFound: TBrookURLEntryPointsNotFoundEvent read FOnNotFound
      write FOnNotFound;
    { Event triggered when the component is enabled. }
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    { Event triggered when the component is disabled. }
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

implementation

{ TBrookURLEntryPoint }

constructor TBrookURLEntryPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  if Assigned(ACollection) and (ACollection is TBrookURLEntryPointList) then
  begin
    FList := ACollection as TBrookURLEntryPointList;
    FName := FList.NewName;
  end
  else
    FName := '/';
end;

function TBrookURLEntryPoint.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookURLEntryPoint.Validate;
begin
  if FName.IsEmpty then
    raise EBrookURLEntryPoint.CreateFmt(SBrookEmptyEntryPointName,
      [GetNamePath]);
end;

function TBrookURLEntryPoint.GetName: string;
var
  P: Pcchar;
begin
  if not Assigned(FHandle) then
    Exit(FName);
  SgLib.Check;
  P := sg_entrypoint_name(FHandle);
  try
    Result := TMarshal.ToString(P);
  finally
    sg_free(P);
  end;
end;

function TBrookURLEntryPoint.GetRouter: TBrookURLRouter;
begin
  Result := GetUserData;
end;

function TBrookURLEntryPoint.GetUserData: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(FUserData);
  SgLib.Check;
  Result := sg_entrypoint_user_data(FHandle);
end;

procedure TBrookURLEntryPoint.SetName(const AValue: string);
var
  EP: TBrookURLEntryPoint;
  NN: string;
begin
  if (AValue = FName) or (not Assigned(FList)) then
    Exit;
  NN := Brook.FixEntryPoint(AValue);
  EP := FList.FindInList(NN);
  if Assigned(EP) and (EP <> Self) then
    raise EBrookURLEntryPoint.CreateFmt(SBrookEntryPointAlreadyExists,
      [GetNamePath, NN, EP.GetNamePath]);
  FName := NN;
  if Assigned(FList.FHandle) then
  begin
    SgLib.Check;
    FList.InternalAdd(Self);
  end;
end;

procedure TBrookURLEntryPoint.SetRouter(AValue: TBrookURLRouter);
var
  M: TMarshaller;
  EP: Psg_entrypoint;
begin
  FUserData := AValue;
  if not Assigned(FList.FHandle) then
    Exit;
  SgLib.Check;
  if sg_entrypoints_find(FList.FHandle, @EP, M.ToCString(FName)) = 0 then
    SgLib.CheckLastError(sg_entrypoint_set_user_data(EP, FUserData));
end;

{ TBrookURLEntryPointListEnumerator }

function TBrookURLEntryPointListEnumerator.GetCurrent: TBrookURLEntryPoint;
begin
  Result := TBrookURLEntryPoint(inherited GetCurrent);
end;

{ TBrookURLEntryPointList }

constructor TBrookURLEntryPointList.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetEntryPointClass);
  SgLib.AddUnloadEvent(InternalLibUnloadEvent, Self);
end;

destructor TBrookURLEntryPointList.Destroy;
begin
  Unprepare;
  SgLib.RemoveUnloadEvent(InternalLibUnloadEvent);
  inherited Destroy;
end;

class function TBrookURLEntryPointList.GetEntryPointClass: TBrookURLEntryPointClass;
begin
  Result := TBrookURLEntryPoint;
end;

class function TBrookURLEntryPointList.GetEntryPointLabel: string;
begin
  Result := '/api';
end;

class function TBrookURLEntryPointList.GetEntryPointName(
  AEntryPoint: TBrookURLEntryPoint): string;
begin
  Result := AEntryPoint.FName;
end;

procedure TBrookURLEntryPointList.CheckPrepared;
begin
  if not Assigned(FHandle) then
    raise EBrookURLEntryPointList.Create(SBrookEntryPointListUnprepared);
end;

procedure TBrookURLEntryPointList.InternalLibUnloadEvent(ASender: TObject);
begin
  TBrookURLEntryPointList(ASender).Unprepare;
end;

function TBrookURLEntryPointList.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookURLEntryPointList.GetEnumerator: TBrookURLEntryPointListEnumerator;
begin
  Result := TBrookURLEntryPointListEnumerator.Create(Self);
end;

procedure TBrookURLEntryPointList.InternalAdd(AEntryPoint: TBrookURLEntryPoint);
var
  M: TMarshaller;
  R: cint;
begin
  R := sg_entrypoints_add(FHandle, M.ToCString(GetEntryPointName(AEntryPoint)),
    AEntryPoint.FUserData);
  if R = 0 then
    Exit;
  if R = EALREADY then
    raise EBrookURLEntryPointList.CreateFmt(SBrookEntryPointAlreadyExists,
      [AEntryPoint.GetNamePath, AEntryPoint.Name]);
  SgLib.CheckLastError(R);
end;

function TBrookURLEntryPointList.NewName: string;
var
  I: Integer;
begin
  I := 1;
  repeat
    Result := Concat(GetEntryPointLabel, I.ToString);
    Inc(I);
  until IndexOf(Result) < 0;
end;

procedure TBrookURLEntryPointList.Prepare;
var
  EP: TBrookURLEntryPoint;
begin
  if Assigned(FHandle) or (Count = 0) then
    Exit;
  SgLib.Check;
  FHandle := sg_entrypoints_new;
  SgLib.CheckLastError(sg_entrypoints_clear(FHandle));
  for EP in Self do
  begin
    EP.Validate;
    InternalAdd(EP);
  end;
end;

procedure TBrookURLEntryPointList.Unprepare;
begin
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  sg_entrypoints_free(FHandle);
  FHandle := nil;
end;

function TBrookURLEntryPointList.IsPrepared: Boolean;
begin
  Result := Assigned(FHandle);
end;

function TBrookURLEntryPointList.GetItem(AIndex: Integer): TBrookURLEntryPoint;
begin
  Result := TBrookURLEntryPoint(inherited GetItem(AIndex));
end;

procedure TBrookURLEntryPointList.SetItem(AIndex: Integer;
  AValue: TBrookURLEntryPoint);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBrookURLEntryPointList.Add: TBrookURLEntryPoint;
begin
  Result := TBrookURLEntryPoint(inherited Add);
end;

function TBrookURLEntryPointList.Remove(const AName: string): Boolean;
var
  M: TMarshaller;
  I: Integer;
begin
  I := IndexOf(AName);
  Result := I > -1;
  if Result then
  begin
    if Assigned(FHandle) then
      SgLib.CheckLastError(sg_entrypoints_rm(FHandle, M.ToCString(AName)));
    inherited Delete(I);
  end;
end;

function TBrookURLEntryPointList.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Name, AName) then
      Exit;
  Result := -1;
end;

function TBrookURLEntryPointList.FindInList(
  const AName: string): TBrookURLEntryPoint;
var
  EP: TBrookURLEntryPoint;
begin
  for EP in Self do
    if SameText(EP.Name, AName) then
      Exit(EP);
  Result := nil;
end;

function TBrookURLEntryPointList.Find(const APath: string;
  out AUserData): Boolean;
var
  M: TMarshaller;
  EP: Psg_entrypoint;
  R: cint;
begin
  CheckPrepared;
  SgLib.Check;
  R := sg_entrypoints_find(FHandle, @EP, M.ToCString(APath));
  Result := R = 0;
  if Result then
    Pointer(AUserData) := sg_entrypoint_user_data(EP)
  else
    if (R <> ENOENT) then
      SgLib.CheckLastError(R);
end;

procedure TBrookURLEntryPointList.Clear;
begin
  inherited Clear;
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  SgLib.CheckLastError(sg_entrypoints_clear(FHandle));
end;

{ TBrookURLEntryPoints }

constructor TBrookURLEntryPoints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := CreateList;
  SgLib.AddUnloadEvent(InternalLibUnloadEvent, Self);
end;

destructor TBrookURLEntryPoints.Destroy;
begin
  SetActive(False);
  FList.Free;
  SgLib.RemoveUnloadEvent(InternalLibUnloadEvent);
  inherited Destroy;
end;

function TBrookURLEntryPoints.CreateList: TBrookURLEntryPointList;
begin
  Result := TBrookURLEntryPointList.Create(Self);
end;

procedure TBrookURLEntryPoints.CheckItems;
begin
  if FList.Count = 0 then
    raise EBrookURLEntryPointList.Create(SBrookNoEntryPointsDefined);
end;

procedure TBrookURLEntryPoints.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EInvalidOpException.Create(SBrookInactiveEntryPoints);
end;

procedure TBrookURLEntryPoints.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      SetActive(True);
  except
    if csDesigning in ComponentState then
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr);
    end
    else
      raise;
  end;
end;

procedure TBrookURLEntryPoints.InternalLibUnloadEvent(ASender: TObject);
begin
  TBrookURLEntryPoints(ASender).Close;
end;

function TBrookURLEntryPoints.GetEnumerator: TBrookURLEntryPointListEnumerator;
begin
  Result := TBrookURLEntryPointListEnumerator.Create(FList);
end;

function TBrookURLEntryPoints.Add: TBrookURLEntryPoint;
begin
  Result := FList.Add;
end;

procedure TBrookURLEntryPoints.Remove(const AName: string);
begin
  FList.Remove(AName);
end;

procedure TBrookURLEntryPoints.Clear;
begin
  FList.Clear;
end;

function TBrookURLEntryPoints.GetHandle: Pointer;
begin
  Result := FList.FHandle;
end;

procedure TBrookURLEntryPoints.DoOpen;
begin
  FList.Prepare;
  FActive := FList.IsPrepared;
  if Assigned(FOnActivate) then
    FOnActivate(Self);
end;

procedure TBrookURLEntryPoints.DoClose;
begin
  FList.Unprepare;
  FActive := False;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(Self);
end;

procedure TBrookURLEntryPoints.SetList(AValue: TBrookURLEntryPointList);
begin
  if AValue = FList then
    Exit;
  if Assigned(AValue) then
    FList.Assign(AValue)
  else
    FList.Clear;
end;

function TBrookURLEntryPoints.IsActiveStored: Boolean;
begin
  Result := FActive;
end;

function TBrookURLEntryPoints.GetItem(AIndex: Integer): TBrookURLEntryPoint;
begin
  Result := FList.GetItem(AIndex);
end;

procedure TBrookURLEntryPoints.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not (csLoading in ComponentState) then
      SgLib.Check;
    FActive := AValue;
  end
  else
    if AValue then
    begin
      if csReading in ComponentState then
        FStreamedActive := True
      else
        DoOpen;
    end
    else
      DoClose;
end;

procedure TBrookURLEntryPoints.SetItem(AIndex: Integer;
  AValue: TBrookURLEntryPoint);
begin
  FList.SetItem(AIndex, AValue);
end;

procedure TBrookURLEntryPoints.Open;
begin
  SetActive(True);
end;

procedure TBrookURLEntryPoints.Close;
begin
  SetActive(False);
end;

procedure TBrookURLEntryPoints.DoRoute(ASender: TObject; const AEntryPoint,
  APath: string; ARouter: TBrookURLRouter; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  if Assigned(ARouter) then
    ARouter.Route(ASender, APath, ARequest, AResponse)
  else
    AResponse.SendFmt(SBrookRouterNotAssigned, [AEntryPoint],
      BROOK_CT_TEXT_PLAIN, 500);
end;

procedure TBrookURLEntryPoints.DoNotFound(ASender: TObject;
  const AEntryPoint, APath: string; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnNotFound) then
    FOnNotFound(ASender, AEntryPoint, APath, ARequest, AResponse)
  else
    AResponse.SendFmt(SBrookEntryPointNotFound, [AEntryPoint],
      BROOK_CT_TEXT_PLAIN, 404);
end;

procedure TBrookURLEntryPoints.Enter(ASender: TObject; const APath: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  RT: TBrookURLRouter;
  EP, P: string;
begin
  CheckItems;
  CheckActive;
  EP := Brook.FixEntryPoint(APath);
  P := Brook.FixPath(APath.SubString(EP.Length));
  if FList.Find(EP, RT) then
    DoRoute(ASender, EP, P, RT, ARequest, AResponse)
  else
    DoNotFound(ASender, EP, P, ARequest, AResponse);
end;

procedure TBrookURLEntryPoints.Enter(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if not Assigned(ARequest) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['ARequest']);
  Enter(ASender, ARequest.Path, ARequest, AResponse);
end;

end.
