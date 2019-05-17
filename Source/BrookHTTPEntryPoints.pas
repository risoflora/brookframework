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

unit BrookHTTPEntryPoints;

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
  BrookHTTPRouter;

resourcestring
  SBrookEntryPointListUnprepared = 'Entry-point list not prepared.';
  SBrookInactiveEntryPoints = 'Inactive entry-points.';
  SBrookNoEntryPointsDefined = 'No entry-points defined.';
  SBrookEntryPointAlreadyExists =
    '%s: entry-point ''%s'' already exists in ''%s''.';
  SBrookEmptyEntryPointName = '%s: entry-point cannot be empty.';
  SBrookEntryPointNotFound = 'Entry-point not found: %s';
  SBrookRouterNotAssigned = 'Router not assigned for entry-point ''%s''.';

type
  TBrookHTTPEntryPointList = class;

  EBrookHTTPEntryPoint = class(Exception);

  TBrookHTTPEntryPoint = class(TBrookHandleCollectionItem)
  private
    FList: TBrookHTTPEntryPointList;
    FHandle: Psg_entrypoint;
    FName: string;
    FUserData: Pointer;
    function GetName: string;
    procedure SetName(const AValue: string);
    function GetUserData: Pointer;
  protected
    function GetHandle: Pointer; override;
    function GetRouter: TBrookHTTPRouter; virtual;
    procedure SetRouter(AValue: TBrookHTTPRouter); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(ASource: TPersistent); override;
    procedure Validate; inline;
    property UserData: Pointer read GetUserData write FUserData;
  published
    property Name: string read GetName write SetName;
    property Router: TBrookHTTPRouter read GetRouter write SetRouter;
  end;

  TBrookHTTPEntryPointClass = class of TBrookHTTPEntryPoint;

  TBrookHTTPEntryPointListEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBrookHTTPEntryPoint;
    property Current: TBrookHTTPEntryPoint read GetCurrent;
  end;

  EBrookHTTPEntryPointList = class(Exception);

  TBrookHTTPEntryPointList = class(TBrookHandleOwnedCollection)
  private
    FHandle: Psg_entrypoints;
  protected
    class function GetEntryPointLabel: string; virtual;
    class function GetEntryPointName(
      AEntryPoint: TBrookHTTPEntryPoint): string; virtual;
    function GetHandle: Pointer; override;
    function GetItem(AIndex: Integer): TBrookHTTPEntryPoint; virtual;
    procedure SetItem(AIndex: Integer; AValue: TBrookHTTPEntryPoint); virtual;
    procedure InternalAdd(AEntryPoint: TBrookHTTPEntryPoint); virtual;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    procedure CheckPrepared; inline;
  public
    constructor Create(AOwner: TPersistent); virtual;
    class function GetEntryPointClass: TBrookHTTPEntryPointClass; virtual;
    procedure Assign(ASource: TPersistent); override;
    function GetEnumerator: TBrookHTTPEntryPointListEnumerator;
    function IsPrepared: Boolean; virtual;
    function NewName: string; virtual;
    function Add: TBrookHTTPEntryPoint; virtual;
    function Remove(const AName: string): Boolean; virtual;
    function IndexOf(const AName: string): Integer; virtual;
    function FindInList(const AName: string): TBrookHTTPEntryPoint; virtual;
    function Find(const APath: string; out AUserData): Boolean; virtual;
    { TODO: Iterate::sg_entrypoints_iter() }
    procedure Clear; virtual;
    property Items[AIndex: Integer]: TBrookHTTPEntryPoint read GetItem
      write SetItem; default;
  end;

  TBrookHTTPEntryPointsNotFoundEvent = procedure(ASender: TObject;
    const AEntryPoint, APath: string; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse) of object;

  TBrookHTTPEntryPoints = class(TBrookHandledComponent)
  private
    FActive: Boolean;
    FList: TBrookHTTPEntryPointList;
    FStreamedActive: Boolean;
    FOnNotFound: TBrookHTTPEntryPointsNotFoundEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    function IsActive: Boolean;
    procedure SetActive(AValue: Boolean);
    function GetItem(AIndex: Integer): TBrookHTTPEntryPoint;
    procedure SetItem(AIndex: Integer; AValue: TBrookHTTPEntryPoint);
    procedure SetList(AValue: TBrookHTTPEntryPointList);
  protected
    class procedure LibNotifier(AClosure: Pointer); static; cdecl;
    function CreateList: TBrookHTTPEntryPointList; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoRoute(ASender: TObject; const AEntryPoint, APath: string;
      ARouter: TBrookHTTPRouter; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); virtual;
    procedure DoNotFound(ASender: TObject; const AEntryPoint, APath: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckItems; inline;
    procedure CheckActive; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function GetEnumerator: TBrookHTTPEntryPointListEnumerator;
    function Add: TBrookHTTPEntryPoint; inline;
    procedure Remove(const AName: string); inline;
    procedure Clear; inline;
    procedure Open;
    procedure Close;
    procedure Enter(ASender: TObject; const APath: string;
      ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); overload; virtual;
    procedure Enter(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); overload; virtual;
    property Items[AIndex: Integer]: TBrookHTTPEntryPoint read GetItem
      write SetItem; default;
  published
    property Active: Boolean read FActive write SetActive stored IsActive;
    property List: TBrookHTTPEntryPointList read FList write SetList;
    property OnNotFound: TBrookHTTPEntryPointsNotFoundEvent read FOnNotFound
      write FOnNotFound;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

implementation

{ TBrookHTTPEntryPoint }

constructor TBrookHTTPEntryPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  if Assigned(ACollection) and (ACollection is TBrookHTTPEntryPointList) then
  begin
    FList := ACollection as TBrookHTTPEntryPointList;
    FName := FList.NewName;
  end
  else
    SetName('/');
end;

function TBrookHTTPEntryPoint.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPEntryPoint.Assign(ASource: TPersistent);
begin
  if ASource is TBrookHTTPEntryPoint then
    FName := (ASource as TBrookHTTPEntryPoint).FName
  else
    inherited Assign(ASource);
end;

procedure TBrookHTTPEntryPoint.Validate;
begin
  if FName.IsEmpty then
    raise EBrookHTTPEntryPoint.CreateFmt(SBrookEmptyEntryPointName,
      [GetNamePath]);
end;

function TBrookHTTPEntryPoint.GetName: string;
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

function TBrookHTTPEntryPoint.GetRouter: TBrookHTTPRouter;
begin
  Result := GetUserData;
end;

function TBrookHTTPEntryPoint.GetUserData: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(FUserData);
  SgLib.Check;
  Result := sg_entrypoint_user_data(FHandle);
end;

procedure TBrookHTTPEntryPoint.SetName(const AValue: string);
var
  EP: TBrookHTTPEntryPoint;
  NN: string;
begin
  if (AValue = FName) or (not Assigned(FList)) then
    Exit;
  NN := Brook.FixEntryPoint(AValue);
  EP := FList.FindInList(NN);
  if Assigned(EP) and (EP <> Self) then
    raise EBrookHTTPEntryPoint.CreateFmt(SBrookEntryPointAlreadyExists,
      [GetNamePath, NN, EP.GetNamePath]);
  FName := NN;
  if Assigned(FList.FHandle) then
  begin
    SgLib.Check;
    FList.InternalAdd(Self);
  end;
end;

procedure TBrookHTTPEntryPoint.SetRouter(AValue: TBrookHTTPRouter);
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

{ TBrookHTTPEntryPointListEnumerator }

function TBrookHTTPEntryPointListEnumerator.GetCurrent: TBrookHTTPEntryPoint;
begin
  Result := TBrookHTTPEntryPoint(inherited GetCurrent);
end;

{ TBrookHTTPEntryPointList }

constructor TBrookHTTPEntryPointList.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetEntryPointClass);
end;

class function TBrookHTTPEntryPointList.GetEntryPointClass: TBrookHTTPEntryPointClass;
begin
  Result := TBrookHTTPEntryPoint;
end;

class function TBrookHTTPEntryPointList.GetEntryPointLabel: string;
begin
  Result := '/api';
end;

class function TBrookHTTPEntryPointList.GetEntryPointName(
  AEntryPoint: TBrookHTTPEntryPoint): string;
begin
  Result := AEntryPoint.FName;
end;

function TBrookHTTPEntryPointList.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPEntryPointList.Assign(ASource: TPersistent);
var
  EP: TBrookHTTPEntryPoint;
begin
  if ASource is TBrookHTTPEntryPointList then
  begin
    Clear;
    for EP in (ASource as TBrookHTTPEntryPointList) do
      Add.Assign(EP);
  end
  else
    inherited Assign(ASource);
end;

function TBrookHTTPEntryPointList.GetEnumerator: TBrookHTTPEntryPointListEnumerator;
begin
  Result := TBrookHTTPEntryPointListEnumerator.Create(Self);
end;

procedure TBrookHTTPEntryPointList.Prepare;
var
  EP: TBrookHTTPEntryPoint;
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

procedure TBrookHTTPEntryPointList.Unprepare;
begin
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  sg_entrypoints_free(FHandle);
  FHandle := nil;
end;

procedure TBrookHTTPEntryPointList.CheckPrepared;
begin
  if not Assigned(FHandle) then
    raise EInvalidPointer.Create(SBrookEntryPointListUnprepared);
end;

procedure TBrookHTTPEntryPointList.InternalAdd(AEntryPoint: TBrookHTTPEntryPoint);
var
  M: TMarshaller;
  R: cint;
begin
  R := sg_entrypoints_add(FHandle, M.ToCString(GetEntryPointName(AEntryPoint)),
    AEntryPoint.FUserData);
  if R = 0 then
    Exit;
  if R = EALREADY then
    raise EBrookHTTPEntryPointList.CreateFmt(SBrookEntryPointAlreadyExists,
      [AEntryPoint.GetNamePath, AEntryPoint.Name]);
  SgLib.CheckLastError(R);
end;

function TBrookHTTPEntryPointList.NewName: string;
var
  I: Integer;
begin
  I := 1;
  repeat
    Result := Concat(GetEntryPointLabel, I.ToString);
    Inc(I);
  until IndexOf(Result) < 0;
end;

function TBrookHTTPEntryPointList.IsPrepared: Boolean;
begin
  Result := Assigned(FHandle);
end;

function TBrookHTTPEntryPointList.GetItem(AIndex: Integer): TBrookHTTPEntryPoint;
begin
  Result := TBrookHTTPEntryPoint(inherited GetItem(AIndex));
end;

procedure TBrookHTTPEntryPointList.SetItem(AIndex: Integer;
  AValue: TBrookHTTPEntryPoint);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBrookHTTPEntryPointList.Add: TBrookHTTPEntryPoint;
begin
  Result := TBrookHTTPEntryPoint(inherited Add);
end;

function TBrookHTTPEntryPointList.Remove(const AName: string): Boolean;
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

function TBrookHTTPEntryPointList.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Name, AName) then
      Exit;
  Result := -1;
end;

function TBrookHTTPEntryPointList.FindInList(
  const AName: string): TBrookHTTPEntryPoint;
var
  EP: TBrookHTTPEntryPoint;
begin
  for EP in Self do
    if SameText(EP.Name, AName) then
      Exit(EP);
  Result := nil;
end;

function TBrookHTTPEntryPointList.Find(const APath: string;
  out AUserData): Boolean;
var
  M: TMarshaller;
  R: cint;
  EP: Psg_entrypoint;
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

procedure TBrookHTTPEntryPointList.Clear;
begin
  inherited Clear;
  SgLib.Check;
  SgLib.CheckLastError(sg_entrypoints_clear(FHandle));
end;

{ TBrookHTTPEntryPoints }

constructor TBrookHTTPEntryPoints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := CreateList;
  SgLib.AddNotifier({$IFNDEF VER3_0}@{$ENDIF}LibNotifier, Self);
end;

destructor TBrookHTTPEntryPoints.Destroy;
begin
  try
    SetActive(False);
    SgLib.RemoveNotifier({$IFNDEF VER3_0}@{$ENDIF}LibNotifier);
  finally
    inherited Destroy;
    FList.Free;
  end;
end;

function TBrookHTTPEntryPoints.CreateList: TBrookHTTPEntryPointList;
begin
  Result := TBrookHTTPEntryPointList.Create(Self);
end;

class procedure TBrookHTTPEntryPoints.LibNotifier(AClosure: Pointer);
begin
  TBrookHTTPEntryPoints(AClosure).Close;
end;

procedure TBrookHTTPEntryPoints.CheckItems;
begin
  if FList.Count = 0 then
    raise EBrookHTTPEntryPointList.Create(SBrookNoEntryPointsDefined);
end;

procedure TBrookHTTPEntryPoints.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EInvalidOpException.Create(SBrookInactiveEntryPoints);
end;

procedure TBrookHTTPEntryPoints.Loaded;
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

procedure TBrookHTTPEntryPoints.Assign(ASource: TPersistent);
begin
  if ASource is TBrookHTTPEntryPoints then
    FList.Assign((ASource as TBrookHTTPEntryPoints).FList)
  else
    inherited Assign(ASource);
end;

function TBrookHTTPEntryPoints.GetEnumerator: TBrookHTTPEntryPointListEnumerator;
begin
  Result := TBrookHTTPEntryPointListEnumerator.Create(FList);
end;

function TBrookHTTPEntryPoints.Add: TBrookHTTPEntryPoint;
begin
  Result := FList.Add;
end;

procedure TBrookHTTPEntryPoints.Remove(const AName: string);
begin
  FList.Remove(AName);
end;

procedure TBrookHTTPEntryPoints.Clear;
begin
  FList.Clear;
end;

function TBrookHTTPEntryPoints.GetHandle: Pointer;
begin
  Result := FList.FHandle;
end;

procedure TBrookHTTPEntryPoints.DoOpen;
begin
  FList.Prepare;
  FActive := FList.IsPrepared;
  if Assigned(FOnActivate) then
    FOnActivate(Self);
end;

procedure TBrookHTTPEntryPoints.DoClose;
begin
  FList.Unprepare;
  FActive := False;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(Self);
end;

procedure TBrookHTTPEntryPoints.SetList(AValue: TBrookHTTPEntryPointList);
begin
  if AValue = FList then
    Exit;
  if Assigned(AValue) then
    FList.Assign(AValue)
  else
    FList.Clear;
end;

function TBrookHTTPEntryPoints.IsActive: Boolean;
begin
  Result := FActive;
end;

function TBrookHTTPEntryPoints.GetItem(AIndex: Integer): TBrookHTTPEntryPoint;
begin
  Result := FList.GetItem(AIndex);
end;

procedure TBrookHTTPEntryPoints.SetActive(AValue: Boolean);
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

procedure TBrookHTTPEntryPoints.SetItem(AIndex: Integer;
  AValue: TBrookHTTPEntryPoint);
begin
  FList.SetItem(AIndex, AValue);
end;

procedure TBrookHTTPEntryPoints.Open;
begin
  SetActive(True);
end;

procedure TBrookHTTPEntryPoints.Close;
begin
  SetActive(False);
end;

procedure TBrookHTTPEntryPoints.DoRoute(ASender: TObject; const AEntryPoint,
  APath: string; ARouter: TBrookHTTPRouter; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  if Assigned(ARouter) then
    ARouter.Route(ASender, APath, ARequest, AResponse)
  else
    AResponse.SendFmt(SBrookRouterNotAssigned, [AEntryPoint],
      BROOK_CT_TEXT_PLAIN, 500);
end;

procedure TBrookHTTPEntryPoints.DoNotFound(ASender: TObject;
  const AEntryPoint, APath: string; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnNotFound) then
    FOnNotFound(ASender, AEntryPoint, APath, ARequest, AResponse)
  else
    AResponse.SendFmt(SBrookEntryPointNotFound, [AEntryPoint],
      BROOK_CT_TEXT_PLAIN, 404);
end;

procedure TBrookHTTPEntryPoints.Enter(ASender: TObject; const APath: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  RT: TBrookHTTPRouter;
  EP, P: string;
begin
  CheckItems;
  CheckActive;
  EP := Brook.FixEntryPoint(APath);
  P := APath.SubString(EP.Length);
  if FList.Find(EP, RT) then
    DoRoute(ASender, EP, P, RT, ARequest, AResponse)
  else
    DoNotFound(ASender, EP, P, ARequest, AResponse);
end;

procedure TBrookHTTPEntryPoints.Enter(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if not Assigned(ARequest) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['ARequest']);
  Enter(ASender, ARequest.Path, ARequest, AResponse);
end;

end.
