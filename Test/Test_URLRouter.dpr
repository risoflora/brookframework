(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2025 Silvio Clecio <silvioprog@gmail.com>
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

program Test_URLRouter;

{$I Tests.inc}

uses
  SysUtils,
  Classes,
  Platform,
  libsagui,
  BrookLibraryLoader,
  BrookUtility,
  BrookStringMap,
  BrookURLRouter,
  BrookHTTPRequest,
  BrookHTTPResponse,
  Test;

type

  { TFakeURLRoutes }

  TFakeURLRoutes = class(TBrookURLRoutes)
  public
    procedure Prepare; override;
    procedure Unprepare; override;
    function NewPattern: string; override;
  end;

  { TFakeURLRoute }

  TFakeURLRoute = class(TBrookURLRoute)
  protected
    function GetHandle: Pointer; override;
    procedure HandleMatch(ARoute: TBrookURLRoute); override;
    procedure FakeOnMath(ARoute: TBrookURLRoute);
    procedure FakeOnRequestMethod(ASender: TObject; ARoute: TBrookURLRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      var AAllowed: Boolean);
    procedure FakeOnRequest(ASender: TObject; ARoute: TBrookURLRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
  public
    constructor Create(ACollection: TCollection); override;
    procedure TestOnMath;
    procedure TestOnRequestMethod(var AAllowed: Boolean);
    procedure TestOnRequest;
  end;

  { TFakeHTTPRequest }

  TFakeHTTPRequest = class(TBrookHTTPRequest)
  protected
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; override;
    function CreateCookies(AHandle: Pointer): TBrookStringMap; override;
    function CreateParams(AHandle: Pointer): TBrookStringMap; override;
    function CreateFields(AHandle: Pointer): TBrookStringMap; override;
  public
    constructor Create(AHandle: Pointer); override;
    destructor Destroy; override;
  end;

  { TFakeHTTPResponse }

  TFakeHTTPResponse = class(TBrookHTTPResponse)
  public
    constructor Create(AHandle: Pointer); override;
    destructor Destroy; override;
  end;

  { TFakeURLRouter }

  TFakeURLRouter = class(TBrookURLRouter)
  public
    procedure FakeOnRoute(ASender: TObject; const APath: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure FakeOnRequest(ASender: TObject; ARoute: TBrookURLRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure FakeOnNotFound(ASender: TObject; const ARoute: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure FakeOnActivate(ASender: TObject);
    procedure FakeOnDeactivate(ASender: TObject);
  end;

const
  FakeHandle = Pointer(123);

var
  FakeHTTPRequest: TFakeHTTPRequest;
  FakeHTTPResponse: TFakeHTTPResponse;
  FakeFlag: Boolean;

function fake_router_dispatch1(router: Psg_router; const path: Pcchar;
  user_data: Pcvoid): cint; cdecl;
begin
  Assert(user_data = FakeHandle);
  Assert(path = '/route');
  Result := 0;
end;

function fake_router_dispatch2(router: Psg_router; const path: Pcchar;
  user_data: Pcvoid): cint; cdecl;
begin
  if path <> '/route' then
    Exit(ENOENT);
  Result := 0;
end;

function fake_httpreq_path(req: Psg_httpreq): Pcchar; cdecl;
begin
  Result := '/route';
end;

function fake_httpreq_method(req: Psg_httpreq): Pcchar; cdecl;
begin
  Result := 'GET';
end;

procedure TFakeURLRouter.FakeOnRoute(ASender: TObject; const APath: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  Assert(ASender = FakeHandle);
  Assert(APath = '/route');
  Assert(ARequest = FakeHTTPRequest);
  Assert(AResponse = FakeHTTPResponse);
  FakeFlag := True;
end;

procedure TFakeURLRouter.FakeOnRequest(ASender: TObject;
  ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  Assert(ASender = ARoute);
  Assert(ARequest = FakeHTTPRequest);
  Assert(AResponse = FakeHTTPResponse);
  FakeFlag := True;
end;

procedure TFakeURLRouter.FakeOnNotFound(ASender: TObject; const ARoute: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  Assert(ASender = FakeHandle);
  Assert((ARoute = '/') or (ARoute = 'xxx'));
  Assert(ARequest = FakeHTTPRequest);
  Assert(AResponse = FakeHTTPResponse);
  FakeFlag := True;
end;

procedure TFakeURLRouter.FakeOnActivate(ASender: TObject);
begin
  Assert(Assigned(ASender));
  FakeFlag := True;
end;

procedure TFakeURLRouter.FakeOnDeactivate(ASender: TObject);
begin
  Assert(Assigned(ASender));
  FakeFlag := True;
end;

{ TFakeURLRoutes }

procedure TFakeURLRoutes.Prepare;
begin
  inherited Prepare;
end;

procedure TFakeURLRoutes.Unprepare;
begin
  inherited Unprepare;
end;

function TFakeURLRoutes.NewPattern: string;
begin
  Result := '';
end;

{ TFakeURLRoute }

constructor TFakeURLRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  DoRouteCallback(Self, FakeHandle);
end;

function TFakeURLRoute.GetHandle: Pointer;
begin
  Result := FakeHandle;
end;

procedure TFakeURLRoute.HandleMatch(ARoute: TBrookURLRoute);
begin
end;

procedure TFakeURLRoute.FakeOnMath(ARoute: TBrookURLRoute);
begin
  Assert(ARoute = Self);
  FakeFlag := True;
end;

procedure TFakeURLRoute.FakeOnRequestMethod(ASender: TObject;
  ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; var AAllowed: Boolean);
begin
  Assert(ASender = Self);
  Assert(ARoute = Self);
  Assert(ARequest = FakeHTTPRequest);
  Assert(AResponse = FakeHTTPResponse);
  AAllowed := True;
  FakeFlag := True;
end;

procedure TFakeURLRoute.FakeOnRequest(ASender: TObject; ARoute: TBrookURLRoute;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  Assert(ASender = Self);
  Assert(ARoute = Self);
  Assert(ARequest = FakeHTTPRequest);
  Assert(AResponse = FakeHTTPResponse);
  FakeFlag := True;
end;

procedure TFakeURLRoute.TestOnMath;
begin
  DoMatch(Self);
end;

procedure TFakeURLRoute.TestOnRequestMethod(var AAllowed: Boolean);
begin
  DoRequestMethod(Self, Self, FakeHTTPRequest, FakeHTTPResponse, AAllowed);
end;

procedure TFakeURLRoute.TestOnRequest;
begin
  DoRequest(Self, Self, FakeHTTPRequest, FakeHTTPResponse);
end;

{ TFakeHTTPRequest }

constructor TFakeHTTPRequest.Create(AHandle: Pointer);
begin
  sg_httpreq_path := fake_httpreq_path;
  sg_httpreq_method := fake_httpreq_method;
  inherited Create(AHandle);
end;

destructor TFakeHTTPRequest.Destroy;
begin
  inherited Destroy;
end;

function TFakeHTTPRequest.CreateHeaders(AHandle: Pointer): TBrookStringMap;
var
  X: Pointer;
begin
  Result := inherited CreateHeaders(@X);
end;

function TFakeHTTPRequest.CreateCookies(AHandle: Pointer): TBrookStringMap;
var
  X: Pointer;
begin
  Result := inherited CreateCookies(@X);
end;

function TFakeHTTPRequest.CreateParams(AHandle: Pointer): TBrookStringMap;
var
  X: Pointer;
begin
  Result := inherited CreateParams(@X);
end;

function TFakeHTTPRequest.CreateFields(AHandle: Pointer): TBrookStringMap;
var
  X: Pointer;
begin
  Result := inherited CreateFields(@X);
end;

{ TFakeHTTPResponse }

constructor TFakeHTTPResponse.Create(AHandle: Pointer);
begin
end;

destructor TFakeHTTPResponse.Destroy;
begin
end;

function fake_route_handle(route: Psg_route): Pcvoid; cdecl;
begin
  Result := FakeHandle;
end;

function fake_route_segments_iter(route: Psg_route; cb: sg_segments_iter_cb;
  cls: Pcvoid): cint; cdecl;
begin
  cb(cls, 0, 'test1');
  cb(cls, 1, 'test2');
  Result := 0;
end;

function fake_route_vars_iter(route: Psg_route; cb: sg_vars_iter_cb;
  cls: Pcvoid): cint; cdecl;
begin
  cb(cls, 'name1', 'val1');
  cb(cls, 'name2', 'val2');
  Result := 0;
end;

function fake_route_rawpattern(route: Psg_route): Pcchar; cdecl;
begin
  Result := 'foo';
end;

function fake_route_path(route: Psg_route): Pcchar; cdecl;
begin
  Result := 'bar';
end;

function fake_route_user_data(route: Psg_route): Pcvoid; cdecl;
begin
  Result := FakeHandle;
end;

procedure Test_URLRouteCreate(AList: TBrookURLRoutes);
var
  RT: TBrookURLRoute;
begin
  AList.Clear;

  RT := TBrookURLRoute.Create(nil);
  try
    Assert(RT.Pattern = '/');
  finally
    RT.Free;
  end;

  RT := TBrookURLRoute.Create(AList);
  Assert(RT.Pattern = '/route1');
  RT := TBrookURLRoute.Create(AList);
  Assert(RT.Pattern = '/route2');

  Assert(RT.Methods = [rmGET, rmPOST]);
end;

procedure DoURLEntryPointEmptyRoutePattern(const AArgs: array of const);
begin
  TBrookURLRoute(AArgs[0].VObject).Validate;
end;

procedure Test_URLRouteValidate(AList: TBrookURLRoutes);
var
  FL: TFakeURLRoutes;
  RT: TBrookURLRoute;
begin
  AList.Clear;

  FL := TFakeURLRoutes.Create(nil);
  try
    RT := TBrookURLRoute.Create(AList);
    RT.Validate;

    RT := TBrookURLRoute.Create(FL);
    AssertExcept(DoURLEntryPointEmptyRoutePattern, EBrookURLRoute,
      Format(SBrookEmptyRoutePattern, [RT.GetNamePath]), [RT]);
  finally
    FL.Free;
  end;
end;

procedure Test_URLRoutePCRE2Handle;
var
  RT: TBrookURLRoute;
begin
  RT := TFakeURLRoute.Create(nil);
  try
    sg_route_handle := fake_route_handle;

    Assert(Assigned(RT.PCRE2Handle));
    Assert(RT.PCRE2Handle = FakeHandle);
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouteSegments;
var
  RT: TBrookURLRoute;
begin
  RT := TFakeURLRoute.Create(nil);
  try
    sg_route_segments_iter := fake_route_segments_iter;

    Assert(Length(RT.Segments) = 2);
    Assert(RT.Segments[0] = 'test1');
    Assert(RT.Segments[1] = 'test2');
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouteVariables;
var
  RT: TBrookURLRoute;
begin
  RT := TFakeURLRoute.Create(nil);
  try
    sg_route_vars_iter := fake_route_vars_iter;

    Assert(Assigned(RT.Variables));
    Assert(RT.Variables.Count = 2);
    Assert(RT.Variables['name1'] = 'val1');
    Assert(RT.Variables['name2'] = 'val2');
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouteRawPattern(AList: TBrookURLRoutes);
var
  RT: TBrookURLRoute;
begin
  AList.Clear;

  RT := TBrookURLRoute.Create(nil);
  try
    Assert(RT.RawPattern = '^/$');
  finally
    RT.Free;
  end;

  RT := TBrookURLRoute.Create(AList);
  Assert(RT.RawPattern = '^/route1$');
  RT := TBrookURLRoute.Create(AList);
  Assert(RT.RawPattern = '^/route2$');
  RT := TBrookURLRoute.Create(AList);
  Assert(RT.RawPattern = '^/route3$');

  RT := TFakeURLRoute.Create(AList);
  sg_route_rawpattern := fake_route_rawpattern;
  Assert(RT.RawPattern = 'foo');
end;

procedure Test_URLRoutePath;
var
  RT: TBrookURLRoute;
begin
  RT := TBrookURLRoute.Create(nil);
  try
    Assert(RT.Path = '');
  finally
    RT.Free;
  end;

  RT := TFakeURLRoute.Create(nil);
  try
    sg_route_path := fake_route_path;
    Assert(RT.Path = 'bar');
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouteUserData;
var
  RT: TBrookURLRoute;
begin
  RT := TBrookURLRoute.Create(nil);
  try
    Assert(not Assigned(RT.UserData));
  finally
    RT.Free;
  end;

  RT := TFakeURLRoute.Create(nil);
  try
    sg_route_user_data := fake_route_user_data;
    Assert(RT.UserData = FakeHandle);
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouteDefault;
var
  RT: TBrookURLRoute;
begin
  RT := TBrookURLRoute.Create(nil);
  try
    Assert(not RT.Default);
    RT.Default := True;
    Assert(RT.Default);
  finally
    RT.Free;
  end;
end;

procedure Test_URLRoutePattern(AList: TBrookURLRoutes);
var
  RT: TBrookURLRoute;
begin
  RT := TBrookURLRoute.Create(nil);
  try
    Assert(RT.Pattern = '/');
    RT.Pattern := 'foo';
    Assert(RT.Pattern = '/');
  finally
    RT.Free;
  end;

  AList.Clear;
  RT := TBrookURLRoute.Create(AList);
  try
    Assert(RT.Pattern = '/route1');
    RT.Pattern := 'foo';
    Assert(RT.Pattern = '/foo');
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouteMethods;
var
  RT: TBrookURLRoute;
begin
  RT := TBrookURLRoute.Create(nil);
  try
    Assert(RT.Methods = [rmGET, rmPOST]);
    RT.Methods := [rmPUT, rmDELETE];
    Assert(RT.Methods = [rmPUT, rmDELETE]);
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouteOnMath;
var
  RT: TFakeURLRoute;
begin
  RT := TFakeURLRoute.Create(nil);
  try
    FakeFlag := False;
    RT.OnMath := RT.FakeOnMath;
    RT.TestOnMath;
    Assert(FakeFlag);
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouteOnRequestMethod;
var
  RT: TFakeURLRoute;
  A: Boolean;
begin
  RT := TFakeURLRoute.Create(nil);
  FakeHTTPRequest := TFakeHTTPRequest.Create(nil);
  FakeHTTPResponse := TFakeHTTPResponse.Create(nil);
  try
    FakeFlag := False;
    RT.OnRequestMethod := RT.FakeOnRequestMethod;
    A := False;
    RT.TestOnRequestMethod(A);
    Assert(A);
    Assert(FakeFlag);
  finally
    RT.Free;
    FakeHTTPRequest.Free;
    FakeHTTPResponse.Free;
  end;
end;

procedure Test_URLRouteOnRequest;
var
  RT: TFakeURLRoute;
begin
  RT := TFakeURLRoute.Create(nil);
  FakeHTTPRequest := TFakeHTTPRequest.Create(nil);
  FakeHTTPResponse := TFakeHTTPResponse.Create(nil);
  try
    FakeFlag := False;
    RT.OnRequest := RT.FakeOnRequest;
    RT.TestOnRequest;
    Assert(FakeFlag);
  finally
    RT.Free;
    FakeHTTPRequest.Free;
    FakeHTTPResponse.Free;
  end;
end;


procedure Test_URLRoutesCreate;
var
  RS: TFakeURLRoutes;
  P: TPersistent;
begin
  P := TPersistent.Create;
  RS := TFakeURLRoutes.Create(P);
  try
    Assert(RS.Owner = P);
    RS.Add.Pattern := 'foo';
    RS.Prepare;
    Assert(Assigned(RS.Handle));
    TBrookLibraryLoader.Unload;
    Assert(not Assigned(RS.Handle));
    TBrookLibraryLoader.Load;
  finally
    RS.Free;
    P.Free;
  end;
end;

procedure Test_URLRoutesGetRouterClass;
begin
  Assert(TBrookURLRoutes.GetRouterClass = TBrookURLRoute);
end;

procedure Test_URLRoutesNewPattern(AList: TBrookURLRoutes);
var
  RT: TBrookURLRoute;
begin
  AList.Clear;

  RT := TBrookURLRoute.Create(AList);
  Assert(RT.RawPattern = '^/route1$');
  RT := TBrookURLRoute.Create(AList);
  Assert(RT.RawPattern = '^/route2$');
  RT := TBrookURLRoute.Create(AList);
  Assert(RT.RawPattern = '^/route3$');
end;

procedure Test_URLRoutesAdd;
var
  RS: TBrookURLRoutes;
  RT: TBrookURLRoute;
begin
  RS := TBrookURLRoutes.Create(nil);
  try
    Assert(RS.Count = 0);
    RT := RS.Add;
    Assert(Assigned(RT));
    RT := RS.Add;
    Assert(Assigned(RT));
    RT := RS.Add;
    Assert(Assigned(RT));
    Assert(RS.Count = 3);
  finally
    RS.Free;
  end;
end;

procedure Test_URLRoutesFirst(AList: TBrookURLRoutes);
var
  RT: TBrookURLRoute;
begin
  AList.Clear;

  RT := AList.First;
  Assert(not Assigned(RT));
  AList.Add;
  AList.Add;
  RT := AList.First;
  Assert(Assigned(RT));
  Assert(RT.Pattern = '/route1');
end;

procedure Test_URLRoutesLast(AList: TBrookURLRoutes);
var
  RT: TBrookURLRoute;
begin
  AList.Clear;

  RT := AList.Last;
  Assert(not Assigned(RT));
  AList.Add;
  AList.Add;
  RT := AList.Last;
  Assert(Assigned(RT));
  Assert(RT.Pattern = '/route2');
end;

procedure Test_URLRoutesIndexOf(AList: TBrookURLRoutes);
begin
  AList.Clear;

  Assert(AList.IndexOf('/route1') = -1);
  Assert(AList.IndexOf('/route2') = -1);
  AList.Add;
  AList.Add;
  Assert(AList.IndexOf('/route1') = 0);
  Assert(AList.IndexOf('/route2') = 1);
end;

procedure Test_URLRoutesFind(AList: TBrookURLRoutes);
begin
  AList.Clear;

  Assert(not Assigned(AList.Find('/route1')));
  Assert(not Assigned(AList.Find('/route2')));
  AList.Add;
  AList.Add;
  Assert(Assigned(AList.Find('/route1')));
  Assert(Assigned(AList.Find('/route2')));
end;

procedure DoURLRoutesDefaultRouteAlreadyExists(const AArgs: array of const);
begin
  TBrookURLRoute(AArgs[0].VObject).Default := True;
end;

procedure Test_URLRoutesFindDefault(AList: TBrookURLRoutes);
var
  RT: TBrookURLRoute;
  R: TBrookURLRoute;
begin
  AList.Clear;

  R := AList.FindDefault;
  Assert(not Assigned(R));
  AList.Add.Default := True;
  R := AList.FindDefault;
  Assert(Assigned(R));

  RT := AList.Add;
  AssertExcept(DoURLRoutesDefaultRouteAlreadyExists, EBrookURLRoute,
    SBrookDefaultRouteAlreadyExists, [RT]);
end;

procedure Test_URLRoutesRemove(AList: TBrookURLRoutes);
begin
  AList.Clear;

  Assert(not Assigned(AList.Find('/route1')));
  Assert(not Assigned(AList.Find('/route2')));
  AList.Add;
  AList.Add;
  Assert(Assigned(AList.Find('/route1')));
  Assert(Assigned(AList.Find('/route2')));
  Assert(AList.Remove('/route1'));
  Assert(not Assigned(AList.Find('/route1')));
  Assert(Assigned(AList.Find('/route2')));
  Assert(not AList.Remove('/route1'));
  Assert(AList.Remove('/route2'));
  Assert(not Assigned(AList.Find('/route1')));
  Assert(not Assigned(AList.Find('/route2')));
  Assert(not AList.Remove('/route2'));
end;

procedure Test_URLRoutesClear;
var
  RS: TFakeURLRoutes;
begin
  RS := TFakeURLRoutes.Create(nil);
  try
    Assert(RS.Count = 0);
    Assert(not Assigned(RS.Handle));
    RS.Add.Pattern := 'foo';
    RS.Add.Pattern := 'bar';
    RS.Prepare;
    Assert(Assigned(RS.Handle));
    RS.Clear;
    Assert(RS.Count = 0);
    Assert(not Assigned(RS.Handle));
  finally
    RS.Free;
  end;
end;

procedure Test_URLRoutesItems(AList: TBrookURLRoutes);
var
  RT: TBrookURLRoute;
begin
  AList.Clear;

  Assert(AList.Count = 0);
  AList.Add;
  AList.Add;
  Assert(AList.Count = 2);
  RT := AList[0];
  Assert(Assigned(RT));
  Assert(RT.Pattern = '/route1');
  RT := AList[1];
  Assert(Assigned(RT));
  Assert(RT.Pattern = '/route2');
end;

procedure Test_URLRouterCreate;
var
  R: TBrookURLRouter;
begin
  R := TBrookURLRouter.Create(nil);
  try
    Assert(Assigned(R.Routes));
  finally
    R.Free;
  end;
end;

procedure Test_URLRouterAdd;
var
  RT: TBrookURLRouter;
  R: TBrookURLRoute;
begin
  RT := TBrookURLRouter.Create(nil);
  try
    Assert(RT.Routes.Count = 0);
    R := RT.Add;
    Assert(Assigned(R));
    R := RT.Add;
    Assert(Assigned(R));
    R := RT.Add;
    Assert(Assigned(R));
    Assert(RT.Routes.Count = 3);
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouterRemove;
var
  RT: TBrookURLRouter;
  R: TBrookURLRoute;
begin
  RT := TBrookURLRouter.Create(nil);
  try
    Assert(RT.Routes.Count = 0);
    R := RT.Add;
    Assert(Assigned(R));
    R := RT.Add;
    Assert(Assigned(R));
    R := RT.Add;
    Assert(Assigned(R));
    Assert(RT.Routes.Count = 3);
    RT.Remove('/route1');
    RT.Remove('/route2');
    RT.Remove('/route3');
    Assert(RT.Routes.Count = 0);
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouterClear;
var
  RT: TBrookURLRouter;
  R: TBrookURLRoute;
begin
  RT := TBrookURLRouter.Create(nil);
  try
    Assert(RT.Routes.Count = 0);
    R := RT.Add;
    Assert(Assigned(R));
    R := RT.Add;
    Assert(Assigned(R));
    R := RT.Add;
    Assert(Assigned(R));
    Assert(RT.Routes.Count = 3);
    RT.Clear;
    Assert(RT.Routes.Count = 0);
  finally
    RT.Free;
  end;
end;

procedure Test_URLRouterOpen;
var
  R: TBrookURLRouter;
begin
  R := TBrookURLRouter.Create(nil);
  try
    Assert(not R.Active);
    R.Open;
    Assert(not R.Active);
    R.Routes.Add;
    R.Open;
    Assert(R.Active);
    R.Open;
  finally
    R.Free;
  end;
end;

procedure Test_URLRouterClose;
var
  R: TBrookURLRouter;
begin
  R := TBrookURLRouter.Create(nil);
  try
    R.Routes.Add;
    R.Open;
    Assert(R.Active);
    R.Close;
    Assert(not R.Active);
    R.Close;
  finally
    R.Free;
  end;
end;

procedure DoURLRouterNoRoutesDefined(const AArgs: array of const);
begin
  TFakeURLRouter(AArgs[0].VObject).DispatchRoute('/route', nil);
end;

procedure DoURLRouterInactiveRouter(const AArgs: array of const);
begin
  TFakeURLRouter(AArgs[0].VObject).DispatchRoute('/route', nil);
end;

procedure Test_URLRouterDispatchRoute;
var
  R: TFakeURLRouter;
begin
  R := TFakeURLRouter.Create(nil);
  try
    R.Routes.Add.Pattern := '/foo';
    R.Open;
    sg_router_dispatch := fake_router_dispatch1;
    R.DispatchRoute('/route', FakeHandle);

    R.Routes.Clear;
    AssertExcept(DoURLRouterNoRoutesDefined, EBrookURLRoutes,
      SBrookNoRoutesDefined, [R]);
    R.Routes.Add.Pattern := '/foo';
    R.Close;
    AssertExcept(DoURLRouterInactiveRouter, EInvalidOpException,
      SBrookInactiveRouter, [R]);
  finally
    R.Free;
  end;
end;

procedure Test_URLRouterRoute;
var
  RT: TFakeURLRouter;
  R: TBrookURLRoute;
begin
  FakeHTTPRequest := TFakeHTTPRequest.Create(nil);
  FakeHTTPResponse := TFakeHTTPResponse.Create(nil);
  RT := TFakeURLRouter.Create(nil);
  try
    RT.Routes.Add.Pattern := '/foo';
    RT.Routes.Add.Pattern := '/route';
    R := RT.Routes.Add;
    R.Default := True;
    R.Pattern := '/bar';
    R.OnRequest := RT.FakeOnRequest;
    RT.Open;
    RT.OnRoute := RT.FakeOnRoute;
    sg_router_dispatch := fake_router_dispatch2;
    FakeFlag := False;
    RT.Route(FakeHandle, '/route', FakeHTTPRequest, FakeHTTPResponse);
    Assert(FakeFlag);

    FakeFlag := False;
    RT.Route(FakeHandle, FakeHTTPRequest, FakeHTTPResponse);
    Assert(FakeFlag);

    FakeFlag := False;
    RT.Route(R, '/', FakeHTTPRequest, FakeHTTPResponse);
    Assert(FakeFlag);

    RT.Routes.Remove('/bar');
    RT.OnNotFound := RT.FakeOnNotFound;
    FakeFlag := False;
    RT.Route(FakeHandle, '/', FakeHTTPRequest, FakeHTTPResponse);
    Assert(FakeFlag);
  finally
    RT.Free;
    FakeHTTPResponse.Free;
    FakeHTTPRequest.Free;
  end;
end;

procedure Test_URLRouterItems;
var
  R: TBrookURLRouter;
begin
  R := TBrookURLRouter.Create(nil);
  try
    R.Add;
    R.Add;
    R.Add;
    Assert(R[0].Pattern = '/route1');
    Assert(R[1].Pattern = '/route2');
    Assert(R[2].Pattern = '/route3');
  finally
    R.Free;
  end;
end;

procedure Test_URLRouterActive;
var
  R: TBrookURLRouter;
begin
  R := TBrookURLRouter.Create(nil);
  try
    R.Routes.Add;
    Assert(not R.Active);
    R.Active := not R.Active;
    Assert(R.Active);
    Assert(Assigned(R.Routes.Handle));
  finally
    R.Free;
  end;
end;

procedure Test_URLRouterRoutes;
var
  R: TBrookURLRouter;
begin
  R := TBrookURLRouter.Create(nil);
  try
    Assert(Assigned(R.Routes));
  finally
    R.Free;
  end;
end;

procedure Test_URLRouterOnRoute;
var
  RT: TFakeURLRouter;
begin
  FakeHTTPRequest := TFakeHTTPRequest.Create(nil);
  FakeHTTPResponse := TFakeHTTPResponse.Create(nil);
  RT := TFakeURLRouter.Create(nil);
  try
    RT.Routes.Add.Pattern := '/route';
    RT.Open;
    RT.OnRoute := RT.FakeOnRoute;
    FakeFlag := False;
    RT.Route(FakeHandle, FakeHTTPRequest, FakeHTTPResponse);
    Assert(FakeFlag);
  finally
    RT.Free;
    FakeHTTPRequest.Free;
    FakeHTTPResponse.Free;
  end;
end;

procedure Test_URLRouterOnNotFound;
var
  R: TFakeURLRouter;
begin
  FakeHTTPRequest := TFakeHTTPRequest.Create(nil);
  FakeHTTPResponse := TFakeHTTPResponse.Create(nil);
  R := TFakeURLRouter.Create(nil);
  try
    R.Routes.Add;
    R.Open;
    R.OnNotFound := R.FakeOnNotFound;
    FakeFlag := False;
    R.Route(FakeHandle, 'xxx', FakeHTTPRequest, FakeHTTPResponse);
    Assert(FakeFlag);
  finally
    R.Free;
    FakeHTTPRequest.Free;
    FakeHTTPResponse.Free;
  end;
end;

procedure Test_URLRouterOnActivate;
var
  R: TFakeURLRouter;
begin
  R := TFakeURLRouter.Create(nil);
  try
    R.OnActivate := R.FakeOnActivate;
    FakeFlag := False;
    R.Routes.Add.Pattern := '/foo';
    R.Open;
    Assert(FakeFlag);
  finally
    R.Free;
  end;
end;

procedure Test_URLRouterOnDeactivate;
var
  R: TFakeURLRouter;
begin
  R := TFakeURLRouter.Create(nil);
  try
    R.Routes.Add;
    R.Open;
    R.OnDeactivate := R.FakeOnDeactivate;
    FakeFlag := False;
    R.Close;
    Assert(FakeFlag);
  finally
    R.Free;
  end;
end;

var
  RS: TBrookURLRoutes;
begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  RS := TBrookURLRoutes.Create(nil);
  try
    Test_URLRouteCreate(RS);
    Test_URLRouteValidate(RS);
    Test_URLRoutePCRE2Handle;
    Test_URLRouteSegments;
    Test_URLRouteVariables;
    Test_URLRouteRawPattern(RS);
    Test_URLRoutePath;
    Test_URLRouteUserData;
    Test_URLRouteDefault;
    Test_URLRoutePattern(RS);
    Test_URLRouteMethods;
    Test_URLRouteOnMath;
    Test_URLRouteOnRequestMethod;
    Test_URLRouteOnRequest;
    Test_URLRoutesCreate;
    Test_URLRoutesGetRouterClass;
    Test_URLRoutesNewPattern(RS);
    Test_URLRoutesAdd;
    Test_URLRoutesFirst(RS);
    Test_URLRoutesLast(RS);
    Test_URLRoutesIndexOf(RS);
    Test_URLRoutesFind(RS);
    Test_URLRoutesFindDefault(RS);
    Test_URLRoutesRemove(RS);
    Test_URLRoutesClear;
    Test_URLRoutesItems(RS);
    Test_URLRouterCreate;
    Test_URLRouterAdd;
    Test_URLRouterRemove;
    Test_URLRouterClear;
    Test_URLRouterOpen;
    Test_URLRouterClose;
    Test_URLRouterDispatchRoute;
    Test_URLRouterRoute;
    Test_URLRouterItems;
    Test_URLRouterActive;
    Test_URLRouterRoutes;
    Test_URLRouterOnRoute;
    Test_URLRouterOnNotFound;
    Test_URLRouterOnActivate;
    Test_URLRouterOnDeactivate;
  finally
    RS.Free;
    TBrookLibraryLoader.Unload;
  end;
end.
