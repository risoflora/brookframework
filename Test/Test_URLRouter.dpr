(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

program Test_URLRouter;

{$I Tests.inc}
{$IFDEF FPC}
 {$WARN 5024 OFF}
{$ENDIF}

uses
  SysUtils,
  Classes,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  Platform,
  libsagui,
  BrookLibraryLoader,
  BrookUtility,
  BrookURLRouter,
  BrookHTTPRequest,
  BrookHTTPResponse,
  Test;

type

  { TFakeURLRoutes }

  TFakeURLRoutes = class(TBrookURLRoutes)
  public
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

const
  FakeHandle = Pointer(123);

var
  FakeHTTPRequest: TFakeHTTPRequest;
  FakeHTTPResponse: TFakeHTTPResponse;
  FakeFlag: Boolean;

{ TFakeURLRoutes }

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
end;

destructor TFakeHTTPRequest.Destroy;
begin
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
  finally
    RS.Free;
    TBrookLibraryLoader.Unload;
  end;
end.
