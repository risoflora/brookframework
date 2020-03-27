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

{ Contains classes for fast URL routing. }

unit BrookURLRouter;

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
  BrookStringMap,
  BrookExtra,
  BrookHTTPRequest,
  BrookHTTPResponse;

resourcestring
  SBrookInactiveRouter = 'Inactive router.';
  SBrookNoRoutesDefined = 'No routes defined.';
  SBrookEmptyRoutePattern = '%s: pattern cannot be empty.';
  SBrookRouteAlreadyExists = '%s: pattern ''%s'' already exists.';
  SBrookRequestMethodNotAllowed = 'Request method not allowed: %s';
  SBrookRequestNoMethodDefined = 'No method(s) defined';
  SBrookRouteNotFound = 'Route not found: %s';
  SBrookDefaultRouteAlreadyExists = 'A default route already exists.';

type
  TBrookURLRoute = class;

  TBrookURLRouteMatchEvent = procedure(ARoute: TBrookURLRoute) of object;

  TBrookURLRouteRequestEvent = procedure(ASender: TObject;
    ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse) of object;

  TBrookURLRouteRequestMethodEvent = procedure(ASender: TObject;
    ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse; var AAllowed: Boolean) of object;

  EBrookURLRoute = class(Exception);

  TBrookURLRouteClosure = record
    Request: TBrookHTTPRequest;
    Response: TBrookHTTPResponse;
    Sender: TObject;
  end;

  TBrookURLRoutes = class;

  TBrookURLRoute = class(TBrookHandledCollectionItem)
  public const
    DefaultReqMethods = [rmGET, rmPOST];
  private
    FOnMath: TBrookURLRouteMatchEvent;
    FRoutes: TBrookURLRoutes;
    FVariables: TBrookStringMap;
    FHandle: Psg_route;
    Fvars: Psg_strmap;
    FPattern: string;
    FDefault: Boolean;
    FMethods: TBrookHTTPRequestMethods;
    FOnRequestMethod: TBrookURLRouteRequestMethodEvent;
    FOnRequest: TBrookURLRouteRequestEvent;
    function GetPattern: string;
    function GetPath: string;
    function GetRawPattern: string;
    function GetVariables: TBrookStringMap;
    function GetRegexHandle: Pointer;
    function GetUserData: Pointer;
    function IsDefaultStored: Boolean;
    procedure SetDefault(AValue: Boolean);
    procedure SetPattern(const AValue: string);
    function IsMethodsStored: Boolean;
    function GetSegments: TArray<string>;
  protected
    class procedure DoRouteCallback(Acls: Pcvoid;
      Aroute: Psg_route); cdecl; static;
    class function DoSegmentsIterCallback(Acls: Pcvoid; Aindex: cuint;
      const Asegment: Pcchar): cint; cdecl; static;
    class function DoVarsIterCallback(Acls: Pcvoid;
      const Aname: Pcchar; const Aval: Pcchar): cint; cdecl; static;
    function GetHandle: Pointer; override;
    procedure DoMatch(ARoute: TBrookURLRoute); virtual;
    procedure DoRequestMethod(ASender: TObject; ARoute: TBrookURLRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      var AAllowed: Boolean); virtual;
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    procedure HandleMatch(ARoute: TBrookURLRoute); virtual;
    procedure HandleRequest(ASender: TObject; ARoute: TBrookURLRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    function IsMethodAllowed(const AMethod: string): Boolean; virtual;
    procedure SendMethodNotAllowed(const AMethod: string;
      AResponse: TBrookHTTPResponse); virtual;
    procedure CheckMethods; inline;
    property Routes: TBrookURLRoutes read FRoutes;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Validate; inline;
    property RegexHandle: Pointer read GetRegexHandle;
    property Segments: TArray<string> read GetSegments;
    property Variables: TBrookStringMap read GetVariables;
    property RawPattern: string read GetRawPattern;
    property Path: string read GetPath;
    property UserData: Pointer read GetUserData;
  published
    property Default: Boolean read FDefault write SetDefault
      stored IsDefaultStored default False;
    property Pattern: string read GetPattern write SetPattern;
    property Methods: TBrookHTTPRequestMethods read FMethods write FMethods
      stored IsMethodsStored default TBrookURLRoute.DefaultReqMethods;
    property OnMath: TBrookURLRouteMatchEvent read FOnMath write FOnMath;
    property OnRequestMethod: TBrookURLRouteRequestMethodEvent
      read FOnRequestMethod write FOnRequestMethod;
    property OnRequest: TBrookURLRouteRequestEvent read FOnRequest
      write FOnRequest;
  end;

  TBrookURLRouteClass = class of TBrookURLRoute;

  TBrookURLRoutesEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBrookURLRoute;
    property Current: TBrookURLRoute read GetCurrent;
  end;

  EBrookURLRoutes = class(Exception);

  TBrookURLRoutes = class(TBrookHandledOwnedCollection)
  private
    FHandle: Psg_route;
    procedure InternalLibUnloadEvent(ASender: TObject);
  protected
    function GetHandle: Pointer; override;
    class function GetRoutePattern(ARoute: TBrookURLRoute): string; virtual;
    class function GetRouteLabel: string; virtual;
    function GetItem(AIndex: Integer): TBrookURLRoute; virtual;
    procedure SetItem(AIndex: Integer; AValue: TBrookURLRoute); virtual;
    procedure InternalAdd(ARoute: TBrookURLRoute); virtual;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    class function GetRouterClass: TBrookURLRouteClass; virtual;
    function GetEnumerator: TBrookURLRoutesEnumerator;
    procedure Assign(ASource: TPersistent); override;
    function NewPattern: string; virtual;
    function Add: TBrookURLRoute; virtual;
    function First: TBrookURLRoute; virtual;
    function Last: TBrookURLRoute; virtual;
    function IndexOf(const APattern: string): Integer; virtual;
    function Find(const APattern: string): TBrookURLRoute; virtual;
    function FindDefault: TBrookURLRoute; virtual;
    function Remove(const APattern: string): Boolean; virtual;
    procedure Clear; virtual;
    property Items[AIndex: Integer]: TBrookURLRoute read GetItem
      write SetItem; default;
  end;

  TBrookURLRouterRouteEvent = procedure(ASender: TObject; const ARoute: string;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse) of object;

  TBrookURLRouter = class(TBrookHandledComponent)
  private
    FRoutes: TBrookURLRoutes;
    FHandle: Psg_router;
    FActive: Boolean;
    FStreamedActive: Boolean;
    FOnNotFound: TBrookURLRouterRouteEvent;
    FOnRoute: TBrookURLRouterRouteEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    function IsActiveStored: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetRoutes(AValue: TBrookURLRoutes);
    procedure InternalLibUnloadEvent(ASender: TObject);
  protected
    function CreateRoutes: TBrookURLRoutes; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoRoute(ASender: TObject;  const ARoute: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    procedure DoNotFound(ASender: TObject; const ARoute: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckItems; inline;
    procedure CheckActive; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function DispatchRoute(const APath: string;
      AUserData: Pointer): Boolean; virtual;
    procedure Route(ASender: TObject;
      const ARoute: string; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); overload; virtual;
    procedure Route(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); overload; virtual;
  published
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
    property Routes: TBrookURLRoutes read FRoutes write SetRoutes;
    property OnRoute: TBrookURLRouterRouteEvent read FOnRoute write FOnRoute;
    property OnNotFound: TBrookURLRouterRouteEvent read FOnNotFound
      write FOnNotFound;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

implementation

{ TBrookURLRoute }

constructor TBrookURLRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVariables := TBrookStringMap.Create(@Fvars);
  if Assigned(ACollection) and (ACollection is TBrookURLRoutes) then
  begin
    FRoutes := ACollection as TBrookURLRoutes;
    FPattern := FRoutes.NewPattern;
  end
  else
    SetPattern('/');
  FMethods := DefaultReqMethods;
end;

destructor TBrookURLRoute.Destroy;
begin
  FVariables.ClearOnDestroy := False;
  FVariables.Free;
  inherited Destroy;
end;

class procedure TBrookURLRoute.DoRouteCallback(Acls: Pcvoid; Aroute: Psg_route);
var
  VRoute: TBrookURLRoute;
begin
  VRoute := Acls;
  VRoute.FHandle := Aroute;
  VRoute.HandleMatch(VRoute);
end;

{$IFDEF FPC}
 {$PUSH}{$WARN 5024 OFF}
{$ENDIF}

class function TBrookURLRoute.DoSegmentsIterCallback(Acls: Pcvoid;
  Aindex: cuint; const Asegment: Pcchar): cint;
var
  VSegments: ^TArray<string>;
begin
  VSegments := Acls;
{$IFDEF VER3_0}
  { TODO: use 'Aindex' and remove -w5024. }
  SetLength(VSegments^, Succ(Length(VSegments^)));
  VSegments^[High(VSegments^)] := TMarshal.ToString(Asegment);
{$ELSE}
  VSegments^ := VSegments^ + [TMarshal.ToString(Asegment)];
{$ENDIF}
  Result := 0;
end;

{$IFDEF FPC}
 {$POP}
{$ENDIF}

class function TBrookURLRoute.DoVarsIterCallback(Acls: Pcvoid;
  const Aname: Pcchar; const Aval: Pcchar): cint;
begin
  TBrookStringMap(Acls).Add(TMarshal.ToString(Aname), TMarshal.ToString(Aval));
  Result := 0;
end;

procedure TBrookURLRoute.CheckMethods;
begin
  if FMethods = [rmUnknown] then
    raise EBrookURLRoute.Create(SBrookRequestNoMethodDefined);
end;

function TBrookURLRoute.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookURLRoute.Assign(ASource: TPersistent);
var
  VSource: TBrookURLRoute;
begin
  if ASource is TBrookURLRoute then
  begin
    VSource := ASource as TBrookURLRoute;
    FPattern := VSource.Pattern;
    FMethods := VSource.Methods;
  end
  else
    inherited Assign(ASource);
end;

function TBrookURLRoute.GetSegments: TArray<string>;
begin
  Result := nil;
  if not Assigned(FHandle) then
    Exit(nil);
  SgLib.Check;
  SgLib.CheckLastError(sg_route_segments_iter(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoSegmentsIterCallback, @Result));
end;

function TBrookURLRoute.GetVariables: TBrookStringMap;
begin
  Result := FVariables;
  FVariables.Clear;
  if not Assigned(FHandle) then
    Exit;
  SgLib.CheckLastError(sg_route_vars_iter(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoVarsIterCallback, FVariables));
end;

function TBrookURLRoute.GetRegexHandle: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgLib.Check;
  Result := sg_route_handle(FHandle);
end;

function TBrookURLRoute.GetRawPattern: string;
begin
  if not Assigned(FHandle) then
  begin
    if FPattern.IsEmpty then
      Exit('');
    Exit(Concat('^', FPattern, '$'));
  end;
  SgLib.Check;
  Result := TMarshal.ToString(sg_route_rawpattern(FHandle));
end;

function TBrookURLRoute.GetPattern: string;
var
  P: Pcchar;
begin
  if not Assigned(FHandle) then
    Exit(FPattern);
  SgLib.Check;
  P := sg_route_pattern(FHandle);
  try
    Result := TMarshal.ToString(P);
  finally
    sg_free(P);
  end;
end;

function TBrookURLRoute.GetPath: string;
begin
  if not Assigned(FHandle) then
    Exit('');
  SgLib.Check;
  Result := TMarshal.ToString(sg_route_path(FHandle));
end;

function TBrookURLRoute.GetUserData: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgLib.Check;
  Result := sg_route_user_data(FHandle);
end;

function TBrookURLRoute.IsDefaultStored: Boolean;
begin
  Result := FDefault;
end;

procedure TBrookURLRoute.SetDefault(AValue: Boolean);
begin
  if FDefault = AValue then
    Exit;
  if AValue and Assigned(FRoutes) and Assigned(FRoutes.FindDefault()) then
    raise EInvalidOpException.Create(SBrookDefaultRouteAlreadyExists);
  FDefault := AValue;
end;

procedure TBrookURLRoute.SetPattern(const AValue: string);
var
  RT: TBrookURLRoute;
  NP: string;
begin
  if (AValue = FPattern) or (not Assigned(FRoutes)) then
    Exit;
  NP := Brook.FixPath(AValue);
  RT := FRoutes.Find(NP);
  if Assigned(RT) and (RT <> Self) then
    raise EBrookURLRoute.CreateFmt(SBrookRouteAlreadyExists, [GetNamePath, NP]);
  FPattern := NP;
  if Assigned(FRoutes.FHandle) then
  begin
    SgLib.Check;
    FRoutes.InternalAdd(Self);
  end;
end;

procedure TBrookURLRoute.Validate;
begin
  if FPattern.IsEmpty then
    raise EBrookURLRoute.CreateFmt(SBrookEmptyRoutePattern, [GetNamePath]);
end;

procedure TBrookURLRoute.DoMatch(ARoute: TBrookURLRoute);
begin
  if Assigned(FOnMath) then
    FOnMath(ARoute);
end;

procedure TBrookURLRoute.DoRequestMethod(ASender: TObject;
  ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; var AAllowed: Boolean);
begin
  if Assigned(FOnRequestMethod) then
    FOnRequestMethod(ASender, ARoute, ARequest, AResponse, AAllowed);
end;

procedure TBrookURLRoute.DoRequest(ASender: TObject; ARoute: TBrookURLRoute;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRequest) then
    FOnRequest(ASender, ARoute, ARequest, AResponse)
  else
    AResponse.SendEmpty;
end;

procedure TBrookURLRoute.HandleMatch(ARoute: TBrookURLRoute);
var
  CLS: TBrookURLRouteClosure;
begin
  DoMatch(ARoute);
  CLS := TBrookURLRouteClosure(ARoute.UserData^);
  HandleRequest(CLS.Sender, TBrookURLRoute(ARoute), CLS.Request, CLS.Response);
end;

procedure TBrookURLRoute.HandleRequest(ASender: TObject;
  ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
var
  A: Boolean;
begin
  CheckMethods;
  A := IsMethodAllowed(ARequest.Method);
  DoRequestMethod(ASender, ARoute, ARequest, AResponse, A);
  if A then
    DoRequest(ASender, ARoute, ARequest, AResponse)
  else
    SendMethodNotAllowed(ARequest.Method, AResponse);
end;

function TBrookURLRoute.IsMethodsStored: Boolean;
begin
  Result := FMethods <> DefaultReqMethods;
end;

function TBrookURLRoute.IsMethodAllowed(const AMethod: string): Boolean;
begin
  Result := (FMethods = []) or (rmUnknown.FromString(AMethod) in FMethods);
end;

procedure TBrookURLRoute.SendMethodNotAllowed(const AMethod: string;
  AResponse: TBrookHTTPResponse);
begin
  AResponse.SendFmt(SBrookRequestMethodNotAllowed, [AMethod],
    BROOK_CT_TEXT_PLAIN, 405);
end;

{ TBrookURLRoutesEnumerator }

function TBrookURLRoutesEnumerator.GetCurrent: TBrookURLRoute;
begin
  Result := TBrookURLRoute(inherited GetCurrent);
end;

{ TBrookURLRoutes }

constructor TBrookURLRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetRouterClass);
  SgLib.AddUnloadEvent(InternalLibUnloadEvent, Self);
end;

destructor TBrookURLRoutes.Destroy;
begin
  Unprepare;
  SgLib.RemoveUnloadEvent(InternalLibUnloadEvent);
  inherited Destroy;
end;

class function TBrookURLRoutes.GetRouterClass: TBrookURLRouteClass;
begin
  Result := TBrookURLRoute;
end;

class function TBrookURLRoutes.GetRoutePattern(ARoute: TBrookURLRoute): string;
begin
  Result := ARoute.FPattern;
end;

class function TBrookURLRoutes.GetRouteLabel: string;
begin
  Result := '/route';
end;

procedure TBrookURLRoutes.InternalLibUnloadEvent(ASender: TObject);
begin
  TBrookURLRoutes(ASender).Unprepare;
end;

function TBrookURLRoutes.FindDefault: TBrookURLRoute;
var
  R: TBrookURLRoute;
begin
  for R in Self do
    if R.FDefault then
      Exit(R);
  Result := nil;
end;

function TBrookURLRoutes.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookURLRoutes.GetEnumerator: TBrookURLRoutesEnumerator;
begin
  Result := TBrookURLRoutesEnumerator.Create(Self);
end;

procedure TBrookURLRoutes.Assign(ASource: TPersistent);
var
  R: TBrookURLRoute;
begin
  if ASource is TBrookURLRoutes then
  begin
    Clear;
    for R in (ASource as TBrookURLRoutes) do
      Add.Assign(R);
  end
  else
    inherited Assign(ASource);
end;

procedure TBrookURLRoutes.InternalAdd(ARoute: TBrookURLRoute);
var
  M: TMarshaller;
  P: array[0..SG_ERR_SIZE-1] of cchar;
  H: Psg_route;
  S: string;
  R: cint;
begin
  P[0] := 0;
  R := sg_routes_add2(@FHandle, @H, M.ToCNullableString(GetRoutePattern(ARoute)),
    @P[0], SG_ERR_SIZE, {$IFNDEF VER3_0}@{$ENDIF}ARoute.DoRouteCallback, ARoute);
  if R = 0 then
    Exit;
  if R = EALREADY then
    raise EBrookURLRoutes.CreateFmt(SBrookRouteAlreadyExists,
      [ARoute.GetNamePath, ARoute.Pattern]);
  if R = EINVAL then
    S := Sagui.StrError(R)
  else
    S := TMarshal.ToString(@P[0]).TrimRight;
  raise EBrookURLRoutes.Create(S);
end;

function TBrookURLRoutes.NewPattern: string;
var
  I: Integer;
begin
  I := 1;
  repeat
    Result := Concat(GetRouteLabel, I.ToString);
    Inc(I);
  until IndexOf(Result) < 0;
end;

procedure TBrookURLRoutes.Prepare;
var
  RT: TBrookURLRoute;
begin
  if Assigned(FHandle) or (Count = 0) then
    Exit;
  SgLib.Check;
  SgLib.CheckLastError(sg_routes_cleanup(@FHandle));
  for RT in Self do
  begin
    RT.Validate;
    InternalAdd(RT);
  end;
end;

procedure TBrookURLRoutes.Unprepare;
begin
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  SgLib.CheckLastError(sg_routes_cleanup(@FHandle));
end;

function TBrookURLRoutes.Add: TBrookURLRoute;
begin
  Result := TBrookURLRoute(inherited Add);
end;

function TBrookURLRoutes.First: TBrookURLRoute;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(0);
end;

function TBrookURLRoutes.Last: TBrookURLRoute;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(Pred(Count));
end;

function TBrookURLRoutes.IndexOf(const APattern: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Pattern, APattern) then
      Exit;
  Result := -1;
end;

function TBrookURLRoutes.Find(const APattern: string): TBrookURLRoute;
var
  RT: TBrookURLRoute;
begin
  for RT in Self do
    if SameText(RT.Pattern, APattern) then
      Exit(RT);
  Result := nil;
end;

function TBrookURLRoutes.Remove(const APattern: string): Boolean;
var
  M: TMarshaller;
  I: Integer;
begin
  I := IndexOf(APattern);
  Result := I > -1;
  if Result then
  begin
    if Assigned(FHandle) then
      SgLib.CheckLastError(sg_routes_rm(@FHandle, M.ToCString(APattern)));
    inherited Delete(I);
  end;
end;

function TBrookURLRoutes.GetItem(AIndex: Integer): TBrookURLRoute;
begin
  Result := TBrookURLRoute(inherited GetItem(AIndex));
end;

procedure TBrookURLRoutes.SetItem(AIndex: Integer; AValue: TBrookURLRoute);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TBrookURLRoutes.Clear;
begin
  inherited Clear;
  Unprepare;
end;

{ TBrookURLRouter }

constructor TBrookURLRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := CreateRoutes;
  SgLib.AddUnloadEvent(InternalLibUnloadEvent, Self);
end;

destructor TBrookURLRouter.Destroy;
begin
  SetActive(False);
  FRoutes.Free;
  SgLib.RemoveUnloadEvent(InternalLibUnloadEvent);
  inherited Destroy;
end;

function TBrookURLRouter.CreateRoutes: TBrookURLRoutes;
begin
  Result := TBrookURLRoutes.Create(Self);
end;

procedure TBrookURLRouter.InternalLibUnloadEvent(ASender: TObject);
begin
  TBrookURLRouter(ASender).Close;
end;

procedure TBrookURLRouter.CheckItems;
begin
  if FRoutes.Count = 0 then
    raise EBrookURLRoutes.Create(SBrookNoRoutesDefined);
end;

procedure TBrookURLRouter.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EInvalidOpException.Create(SBrookInactiveRouter);
end;

procedure TBrookURLRouter.Loaded;
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

function TBrookURLRouter.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookURLRouter.SetRoutes(AValue: TBrookURLRoutes);
begin
  if AValue = FRoutes then
    Exit;
  if Assigned(AValue) then
    FRoutes.Assign(AValue)
  else
    FRoutes.Clear;
end;

function TBrookURLRouter.IsActiveStored: Boolean;
begin
  Result := FActive;
end;

procedure TBrookURLRouter.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not (csLoading in ComponentState) then
    begin
      SgLib.Check;
      if AValue then
        CheckItems;
    end;
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

procedure TBrookURLRouter.DoOpen;
begin
  if Assigned(FHandle) then
    Exit;
  FRoutes.Prepare;
  SgLib.Check;
  FHandle := sg_router_new(FRoutes.Handle);
  FActive := Assigned(FHandle);
  if Assigned(FOnActivate) then
    FOnActivate(Self);
end;

procedure TBrookURLRouter.DoClose;
begin
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  sg_router_free(FHandle);
  FHandle := nil;
  FActive := False;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(Self);
end;

procedure TBrookURLRouter.Open;
begin
  SetActive(True);
end;

procedure TBrookURLRouter.Close;
begin
  SetActive(False);
end;

procedure TBrookURLRouter.DoRoute(ASender: TObject; const ARoute: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRoute) then
    FOnRoute(ASender, ARoute, ARequest, AResponse);
end;

procedure TBrookURLRouter.DoNotFound(ASender: TObject; const ARoute: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnNotFound) then
    FOnNotFound(ASender, ARoute, ARequest, AResponse)
  else
    AResponse.SendFmt(SBrookRouteNotFound, [ARoute], BROOK_CT_TEXT_PLAIN, 404);
end;

function TBrookURLRouter.DispatchRoute(const APath: string;
  AUserData: Pointer): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckItems;
  CheckActive;
  SgLib.Check;
  R := sg_router_dispatch(FHandle,
    M.ToCNullableString(Brook.FixPath(APath)), AUserData);
  Result := R = 0;
  if (not Result) and (R <> ENOENT) then
    SgLib.CheckLastError(R);
end;

procedure TBrookURLRouter.Route(ASender: TObject; const ARoute: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  CLS: TBrookURLRouteClosure;
  R: TBrookURLRoute;
begin
  CLS.Request := ARequest;
  CLS.Response := AResponse;
  CLS.Sender := ASender;
  if DispatchRoute(ARoute, @CLS) then
  begin
    DoRoute(ASender, ARoute, ARequest, AResponse);
    Exit;
  end;
  if ARoute = '/' then
  begin
    R := FRoutes.FindDefault;
    if Assigned(R) then
    begin
      R.HandleRequest(ASender, R, ARequest, AResponse);
      Exit;
    end;
  end;
  DoNotFound(ASender, ARoute, ARequest, AResponse);
end;

procedure TBrookURLRouter.Route(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if not Assigned(ARequest) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['ARequest']);
  Route(ASender, ARequest.Path, ARequest, AResponse);
end;

end.
