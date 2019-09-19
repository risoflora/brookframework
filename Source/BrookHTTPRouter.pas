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

unit BrookHTTPRouter;

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
  SBrookRouteAlreadyExists = '%s: pattern ''%s'' already exists in ''%s''.';
  SBrookRequestMethodNotAllowed = 'Request method not allowed: %s';
  SBrookRequestNoMethodDefined = 'No method(s) defined';
  SBrookRouteNotFound = 'Route not found: %s';
  SBrookDefaultRouteAlreadyExists = 'A default route already exists.';

type
  TBrookHTTPRoute = class;

  TBrookHTTPRouteMatchEvent = procedure(ARoute: TBrookHTTPRoute) of object;

  TBrookHTTPRouteRequestEvent = procedure(ASender: TObject;
    ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse) of object;

  TBrookHTTPRouteRequestMethodEvent = procedure(ASender: TObject;
    ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse; var AAllowed: Boolean) of object;

  EBrookHTTPRoute = class(Exception);

  TBrookHTTPRouteClosure = record
    Request: TBrookHTTPRequest;
    Response: TBrookHTTPResponse;
    Sender: TObject;
  end;

  TBrookHTTPRoutes = class;

  TBrookHTTPRoute = class(TBrookHandledCollectionItem)
  public const
    DefaultReqMethods = [rmGET, rmPOST];
  private
    FOnMath: TBrookHTTPRouteMatchEvent;
    FRoutes: TBrookHTTPRoutes;
    FVariables: TBrookStringMap;
    FHandle: Psg_route;
    Fvars: Psg_strmap;
    FPattern: string;
    FDefault: Boolean;
    FMethods: TBrookHTTPRequestMethods;
    FOnRequestMethod: TBrookHTTPRouteRequestMethodEvent;
    FOnRequest: TBrookHTTPRouteRequestEvent;
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
    procedure DoMatch(ARoute: TBrookHTTPRoute); virtual;
    procedure DoRequestMethod(ASender: TObject; ARoute: TBrookHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      var AAllowed: Boolean); virtual;
    procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    procedure HandleMatch(ARoute: TBrookHTTPRoute); virtual;
    procedure HandleRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    function IsMethodAllowed(const AMethod: string): Boolean; virtual;
    procedure SendMethodNotAllowed(const AMethod: string;
      AResponse: TBrookHTTPResponse); virtual;
    procedure CheckMethods; inline;
    property Routes: TBrookHTTPRoutes read FRoutes;
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
      stored IsMethodsStored default TBrookHTTPRoute.DefaultReqMethods;
    property OnMath: TBrookHTTPRouteMatchEvent read FOnMath write FOnMath;
    property OnRequestMethod: TBrookHTTPRouteRequestMethodEvent
      read FOnRequestMethod write FOnRequestMethod;
    property OnRequest: TBrookHTTPRouteRequestEvent read FOnRequest
      write FOnRequest;
  end;

  TBrookHTTPRouteClass = class of TBrookHTTPRoute;

  TBrookHTTPRoutesEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBrookHTTPRoute;
    property Current: TBrookHTTPRoute read GetCurrent;
  end;

  EBrookHTTPRoutes = class(Exception);

  TBrookHTTPRoutes = class(TBrookHandledOwnedCollection)
  private
    FHandle: Psg_route;
  protected
    function GetHandle: Pointer; override;
    class procedure LibNotifier(AClosure: Pointer); static; cdecl;
    class function GetRoutePattern(ARoute: TBrookHTTPRoute): string; virtual;
    class function GetRouteLabel: string; virtual;
    function GetItem(AIndex: Integer): TBrookHTTPRoute; virtual;
    procedure SetItem(AIndex: Integer; AValue: TBrookHTTPRoute); virtual;
    procedure InternalAdd(ARoute: TBrookHTTPRoute); virtual;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    class function GetRouterClass: TBrookHTTPRouteClass; virtual;
    function GetEnumerator: TBrookHTTPRoutesEnumerator;
    procedure Assign(ASource: TPersistent); override;
    function NewPattern: string; virtual;
    function Add: TBrookHTTPRoute; virtual;
    function First: TBrookHTTPRoute; virtual;
    function Last: TBrookHTTPRoute; virtual;
    function IndexOf(const APattern: string): Integer; virtual;
    function Find(const APattern: string): TBrookHTTPRoute; virtual;
    function FindDefault: TBrookHTTPRoute; virtual;
    function Remove(const APattern: string): Boolean; virtual;
    procedure Clear; virtual;
    property Items[AIndex: Integer]: TBrookHTTPRoute read GetItem
      write SetItem; default;
  end;

  TBrookHTTPRouterRouteEvent = procedure(ASender: TObject; const ARoute: string;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse) of object;

  TBrookHTTPRouter = class(TBrookHandledComponent)
  private
    FRoutes: TBrookHTTPRoutes;
    FHandle: Psg_router;
    FActive: Boolean;
    FStreamedActive: Boolean;
    FOnNotFound: TBrookHTTPRouterRouteEvent;
    FOnRoute: TBrookHTTPRouterRouteEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    function IsActiveStored: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetRoutes(AValue: TBrookHTTPRoutes);
  protected
    class procedure LibNotifier(AClosure: Pointer); static; cdecl;
    function CreateRoutes: TBrookHTTPRoutes; virtual;
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
    property Routes: TBrookHTTPRoutes read FRoutes write SetRoutes;
    property OnRoute: TBrookHTTPRouterRouteEvent read FOnRoute write FOnRoute;
    property OnNotFound: TBrookHTTPRouterRouteEvent read FOnNotFound
      write FOnNotFound;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

implementation

{ TBrookHTTPRoute }

constructor TBrookHTTPRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVariables := TBrookStringMap.Create(@Fvars);
  if Assigned(ACollection) and (ACollection is TBrookHTTPRoutes) then
  begin
    FRoutes := ACollection as TBrookHTTPRoutes;
    FPattern := FRoutes.NewPattern;
  end
  else
    SetPattern('/');
  FMethods := DefaultReqMethods;
end;

destructor TBrookHTTPRoute.Destroy;
begin
  FVariables.ClearOnDestroy := False;
  FVariables.Free;
  inherited Destroy;
end;

class procedure TBrookHTTPRoute.DoRouteCallback(Acls: Pcvoid; Aroute: Psg_route);
var
  VRoute: TBrookHTTPRoute;
begin
  VRoute := Acls;
  VRoute.FHandle := Aroute;
  VRoute.HandleMatch(VRoute);
end;

{$IFDEF FPC}
 {$PUSH}{$WARN 5024 OFF}
{$ENDIF}

class function TBrookHTTPRoute.DoSegmentsIterCallback(Acls: Pcvoid;
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

class function TBrookHTTPRoute.DoVarsIterCallback(Acls: Pcvoid;
  const Aname: Pcchar; const Aval: Pcchar): cint;
begin
  TBrookStringMap(Acls).Add(TMarshal.ToString(Aname), TMarshal.ToString(Aval));
  Result := 0;
end;

procedure TBrookHTTPRoute.CheckMethods;
begin
  if FMethods = [rmUnknown] then
    raise EBrookHTTPRoute.Create(SBrookRequestNoMethodDefined);
end;

function TBrookHTTPRoute.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPRoute.Assign(ASource: TPersistent);
var
  VSource: TBrookHTTPRoute;
begin
  if ASource is TBrookHTTPRoute then
  begin
    VSource := ASource as TBrookHTTPRoute;
    FPattern := VSource.FPattern;
    FMethods := VSource.FMethods;
  end
  else
    inherited Assign(ASource);
end;

function TBrookHTTPRoute.GetSegments: TArray<string>;
begin
  Result := nil;
  if not Assigned(FHandle) then
    Exit(nil);
  SgLib.Check;
  SgLib.CheckLastError(sg_route_segments_iter(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoSegmentsIterCallback, @Result));
end;

function TBrookHTTPRoute.GetVariables: TBrookStringMap;
begin
  Result := FVariables;
  FVariables.Clear;
  if not Assigned(FHandle) then
    Exit;
  SgLib.CheckLastError(sg_route_vars_iter(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoVarsIterCallback, FVariables));
end;

function TBrookHTTPRoute.GetRegexHandle: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgLib.Check;
  Result := sg_route_handle(FHandle);
end;

function TBrookHTTPRoute.GetRawPattern: string;
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

function TBrookHTTPRoute.GetPattern: string;
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

function TBrookHTTPRoute.GetPath: string;
begin
  if not Assigned(FHandle) then
    Exit('');
  SgLib.Check;
  Result := TMarshal.ToString(sg_route_path(FHandle));
end;

function TBrookHTTPRoute.GetUserData: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgLib.Check;
  Result := sg_route_user_data(FHandle);
end;

function TBrookHTTPRoute.IsDefaultStored: Boolean;
begin
  Result := FDefault;
end;

procedure TBrookHTTPRoute.SetDefault(AValue: Boolean);
begin
  if FDefault = AValue then
    Exit;
  if AValue and Assigned(FRoutes) and Assigned(FRoutes.FindDefault()) then
    raise EInvalidOpException.Create(SBrookDefaultRouteAlreadyExists);
  FDefault := AValue;
end;

procedure TBrookHTTPRoute.SetPattern(const AValue: string);
var
  RT: TBrookHTTPRoute;
  NP: string;
begin
  if (AValue = FPattern) or (not Assigned(FRoutes)) then
    Exit;
  NP := Brook.FixPath(AValue);
  RT := FRoutes.Find(NP);
  if Assigned(RT) and (RT <> Self) then
    raise EBrookHTTPRoute.CreateFmt(SBrookRouteAlreadyExists,
      [GetNamePath, NP, RT.GetNamePath]);
  FPattern := NP;
  if Assigned(FRoutes.FHandle) then
  begin
    SgLib.Check;
    FRoutes.InternalAdd(Self);
  end;
end;

procedure TBrookHTTPRoute.Validate;
begin
  if FPattern.IsEmpty then
    raise EBrookHTTPRoute.CreateFmt(SBrookEmptyRoutePattern, [GetNamePath]);
end;

procedure TBrookHTTPRoute.DoMatch(ARoute: TBrookHTTPRoute);
begin
  if Assigned(FOnMath) then
    FOnMath(ARoute);
end;

procedure TBrookHTTPRoute.DoRequestMethod(ASender: TObject;
  ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; var AAllowed: Boolean);
begin
  if Assigned(FOnRequestMethod) then
    FOnRequestMethod(ASender, ARoute, ARequest, AResponse, AAllowed);
end;

procedure TBrookHTTPRoute.DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRequest) then
    FOnRequest(ASender, ARoute, ARequest, AResponse)
  else
    AResponse.SendEmpty;
end;

procedure TBrookHTTPRoute.HandleMatch(ARoute: TBrookHTTPRoute);
var
  CLS: TBrookHTTPRouteClosure;
begin
  DoMatch(ARoute);
  CLS := TBrookHTTPRouteClosure(ARoute.UserData^);
  HandleRequest(CLS.Sender, TBrookHTTPRoute(ARoute), CLS.Request, CLS.Response);
end;

procedure TBrookHTTPRoute.HandleRequest(ASender: TObject;
  ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
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

function TBrookHTTPRoute.IsMethodsStored: Boolean;
begin
  Result := FMethods <> DefaultReqMethods;
end;

function TBrookHTTPRoute.IsMethodAllowed(const AMethod: string): Boolean;
begin
  Result := (FMethods = []) or (rmUnknown.FromString(AMethod) in FMethods);
end;

procedure TBrookHTTPRoute.SendMethodNotAllowed(const AMethod: string;
  AResponse: TBrookHTTPResponse);
begin
  AResponse.SendFmt(SBrookRequestMethodNotAllowed, [AMethod],
    BROOK_CT_TEXT_PLAIN, 405);
end;

{ TBrookHTTPRoutesEnumerator }

function TBrookHTTPRoutesEnumerator.GetCurrent: TBrookHTTPRoute;
begin
  Result := TBrookHTTPRoute(inherited GetCurrent);
end;

{ TBrookHTTPRoutes }

constructor TBrookHTTPRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetRouterClass);
  SgLib.AddNotifier({$IFNDEF VER3_0}@{$ENDIF}LibNotifier, Self);
end;

destructor TBrookHTTPRoutes.Destroy;
begin
  Unprepare;
  SgLib.RemoveNotifier({$IFNDEF VER3_0}@{$ENDIF}LibNotifier);
  inherited Destroy;
end;

class procedure TBrookHTTPRoutes.LibNotifier(AClosure: Pointer);
begin
  TBrookHTTPRoutes(AClosure).Unprepare;
end;

class function TBrookHTTPRoutes.GetRouterClass: TBrookHTTPRouteClass;
begin
  Result := TBrookHTTPRoute;
end;

class function TBrookHTTPRoutes.GetRoutePattern(ARoute: TBrookHTTPRoute): string;
begin
  Result := ARoute.FPattern;
end;

class function TBrookHTTPRoutes.GetRouteLabel: string;
begin
  Result := '/route';
end;

function TBrookHTTPRoutes.FindDefault: TBrookHTTPRoute;
var
  R: TBrookHTTPRoute;
begin
  for R in Self do
    if R.FDefault then
      Exit(R);
  Result := nil;
end;

function TBrookHTTPRoutes.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookHTTPRoutes.GetEnumerator: TBrookHTTPRoutesEnumerator;
begin
  Result := TBrookHTTPRoutesEnumerator.Create(Self);
end;

procedure TBrookHTTPRoutes.Assign(ASource: TPersistent);
var
  R: TBrookHTTPRoute;
begin
  if ASource is TBrookHTTPRoutes then
  begin
    Clear;
    for R in (ASource as TBrookHTTPRoutes) do
      Add.Assign(R);
  end
  else
    inherited Assign(ASource);
end;

procedure TBrookHTTPRoutes.InternalAdd(ARoute: TBrookHTTPRoute);
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
    raise EBrookHTTPRoutes.CreateFmt(SBrookRouteAlreadyExists,
      [ARoute.GetNamePath, ARoute.Pattern]);
  if R = EINVAL then
    S := Sagui.StrError(R)
  else
    S := TMarshal.ToString(@P[0]).TrimRight;
  raise EBrookHTTPRoutes.Create(S);
end;

function TBrookHTTPRoutes.NewPattern: string;
var
  I: Integer;
begin
  I := 1;
  repeat
    Result := Concat(GetRouteLabel, I.ToString);
    Inc(I);
  until IndexOf(Result) < 0;
end;

procedure TBrookHTTPRoutes.Prepare;
var
  RT: TBrookHTTPRoute;
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

procedure TBrookHTTPRoutes.Unprepare;
begin
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  SgLib.CheckLastError(sg_routes_cleanup(@FHandle));
end;

function TBrookHTTPRoutes.Add: TBrookHTTPRoute;
begin
  Result := TBrookHTTPRoute(inherited Add);
end;

function TBrookHTTPRoutes.First: TBrookHTTPRoute;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(0);
end;

function TBrookHTTPRoutes.Last: TBrookHTTPRoute;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(Pred(Count));
end;

function TBrookHTTPRoutes.IndexOf(const APattern: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Pattern, APattern) then
      Exit;
  Result := -1;
end;

function TBrookHTTPRoutes.Find(const APattern: string): TBrookHTTPRoute;
var
  RT: TBrookHTTPRoute;
begin
  for RT in Self do
    if SameText(RT.Pattern, APattern) then
      Exit(RT);
  Result := nil;
end;

function TBrookHTTPRoutes.Remove(const APattern: string): Boolean;
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

function TBrookHTTPRoutes.GetItem(AIndex: Integer): TBrookHTTPRoute;
begin
  Result := TBrookHTTPRoute(inherited GetItem(AIndex));
end;

procedure TBrookHTTPRoutes.SetItem(AIndex: Integer; AValue: TBrookHTTPRoute);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TBrookHTTPRoutes.Clear;
begin
  inherited Clear;
  Unprepare;
end;

{ TBrookHTTPRouter }

constructor TBrookHTTPRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := CreateRoutes;
  SgLib.AddNotifier({$IFNDEF VER3_0}@{$ENDIF}LibNotifier, Self);
end;

destructor TBrookHTTPRouter.Destroy;
begin
  SetActive(False);
  FRoutes.Free;
  SgLib.RemoveNotifier({$IFNDEF VER3_0}@{$ENDIF}LibNotifier);
  inherited Destroy;
end;

function TBrookHTTPRouter.CreateRoutes: TBrookHTTPRoutes;
begin
  Result := TBrookHTTPRoutes.Create(Self);
end;

class procedure TBrookHTTPRouter.LibNotifier(AClosure: Pointer);
begin
  TBrookHTTPRouter(AClosure).Close;
end;

procedure TBrookHTTPRouter.CheckItems;
begin
  if FRoutes.Count = 0 then
    raise EBrookHTTPRoutes.Create(SBrookNoRoutesDefined);
end;

procedure TBrookHTTPRouter.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EInvalidOpException.Create(SBrookInactiveRouter);
end;

procedure TBrookHTTPRouter.Loaded;
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

function TBrookHTTPRouter.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPRouter.SetRoutes(AValue: TBrookHTTPRoutes);
begin
  if AValue = FRoutes then
    Exit;
  if Assigned(AValue) then
    FRoutes.Assign(AValue)
  else
    FRoutes.Clear;
end;

function TBrookHTTPRouter.IsActiveStored: Boolean;
begin
  Result := FActive;
end;

procedure TBrookHTTPRouter.SetActive(AValue: Boolean);
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

procedure TBrookHTTPRouter.DoOpen;
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

procedure TBrookHTTPRouter.DoClose;
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

procedure TBrookHTTPRouter.Open;
begin
  SetActive(True);
end;

procedure TBrookHTTPRouter.Close;
begin
  SetActive(False);
end;

procedure TBrookHTTPRouter.DoRoute(ASender: TObject; const ARoute: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRoute) then
    FOnRoute(ASender, ARoute, ARequest, AResponse);
end;

procedure TBrookHTTPRouter.DoNotFound(ASender: TObject; const ARoute: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnNotFound) then
    FOnNotFound(ASender, ARoute, ARequest, AResponse)
  else
    AResponse.SendFmt(SBrookRouteNotFound, [ARoute], BROOK_CT_TEXT_PLAIN, 404);
end;

function TBrookHTTPRouter.DispatchRoute(const APath: string;
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

procedure TBrookHTTPRouter.Route(ASender: TObject; const ARoute: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  CLS: TBrookHTTPRouteClosure;
  R: TBrookHTTPRoute;
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

procedure TBrookHTTPRouter.Route(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if not Assigned(ARequest) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['ARequest']);
  Route(ASender, ARequest.Path, ARequest, AResponse);
end;

end.
