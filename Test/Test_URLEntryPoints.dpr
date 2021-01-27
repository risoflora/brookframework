(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

program Test_URLEntryPoints;

{$I Tests.inc}

uses
  RTLConsts,
  SysUtils,
  Classes,
  BrookExtra,
  BrookLibraryLoader,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookURLEntryPoints,
  BrookURLRouter,
  Test;

type

  { TFakeHTTPRequest }

  TFakeHTTPRequest = class(TBrookHTTPRequest)
  private
    FPath: string;
  public
    constructor Create(AHandle: Pointer); override;
    destructor Destroy; override;
    property Path: string read FPath;
  end;

  { TFakeHTTPResponse }

  TFakeHTTPResponse = class(TBrookHTTPResponse)
  public
    constructor Create(AHandle: Pointer); override;
    destructor Destroy; override;
    procedure SendFmt(const AFormat: string; const AArgs: array of const;
      const AContentType: string; AStatus: Word); override;
  end;

  { TFakeURLEntryPointList }

  TFakeURLEntryPointList = class(TBrookURLEntryPointList)
  public
    function NewName: string; override;
  end;

  { TFakeURLEntryPoints }

  TFakeURLEntryPoints = class(TBrookURLEntryPoints)
  public
    procedure FakeOnNotFound(ASender: TObject; const AEntryPoint, APath: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure FakeOnActivate(ASender: TObject);
    procedure FakeOnDeactivate(ASender: TObject);
  end;

  { TFakeURLRouter }

  TFakeURLRouter = class(TBrookURLRouter)
  public
    procedure Route(ASender: TObject; const ARoute: string;
      AReq: TBrookHTTPRequest; ARes: TBrookHTTPResponse); overload; override;
  end;

var
  FakeRoutedPath: string;
  FakeFlag: Boolean;
  FakeResponseFakeFmt: string;
  FakeStatus: Word;

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

procedure TFakeHTTPResponse.SendFmt(const AFormat: string;
  const AArgs: array of const; const AContentType: string; AStatus: Word);
begin
  Assert(AFormat = FakeResponseFakeFmt);
  Assert(SameStr(string(AArgs[0].VPointer), FakeRoutedPath));
  Assert(AContentType = BROOK_CT_TEXT_PLAIN);
  Assert(AStatus = FakeStatus);
end;

{ TFakeURLEntryPointList }

function TFakeURLEntryPointList.NewName: string;
begin
  Result := '';
end;

procedure TFakeURLEntryPoints.FakeOnNotFound(ASender: TObject;
  const AEntryPoint, APath: string; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  Assert(AEntryPoint = '/xxx');
  Assert(APath = '/');
  FakeFlag := True;
end;

procedure TFakeURLEntryPoints.FakeOnActivate(ASender: TObject);
begin
  FakeFlag := True;
end;

procedure TFakeURLEntryPoints.FakeOnDeactivate(ASender: TObject);
begin
  FakeFlag := True;
end;

{ TFakeURLRouter }

procedure TFakeURLRouter.Route(ASender: TObject; const ARoute: string;
  AReq: TBrookHTTPRequest; ARes: TBrookHTTPResponse);
begin
  FakeFlag := ARoute = FakeRoutedPath;
end;

procedure Test_URLEntryPointCreate(AList: TBrookURLEntryPointList);
var
  EP: TBrookURLEntryPoint;
begin
  AList.Clear;

  EP := TBrookURLEntryPoint.Create(nil);
  try
    Assert(EP.Name = '/');
  finally
    EP.Free;
  end;

  EP := TBrookURLEntryPoint.Create(AList);
  Assert(EP.Name = '/api1');
  EP := TBrookURLEntryPoint.Create(AList);
  Assert(EP.Name = '/api2');
end;

procedure DoURLEntryPointEmptyEntryPointName(const AArgs: array of const);
begin
  TBrookURLEntryPoint(AArgs[0].VObject).Validate;
end;

procedure Test_URLEntryPointValidate(AList: TBrookURLEntryPointList);
var
  FL: TFakeURLEntryPointList;
  EP: TBrookURLEntryPoint;
begin
  AList.Clear;

  FL := TFakeURLEntryPointList.Create(nil);
  try
    EP := TBrookURLEntryPoint.Create(AList);
    EP.Validate;

    EP := TBrookURLEntryPoint.Create(FL);
    AssertExcept(DoURLEntryPointEmptyEntryPointName, EBrookURLEntryPoint,
      Format(SBrookEmptyEntryPointName, [EP.GetNamePath]),
      [EP]);
  finally
    FL.Free;
  end;
end;

procedure Test_URLEntryPointUserData(AList: TBrookURLEntryPointList);
var
  EP: TBrookURLEntryPoint;
begin
  AList.Clear;

  EP := TBrookURLEntryPoint.Create(AList);
  Assert(not Assigned(EP.UserData));
  EP.UserData := AList;
  Assert(EP.UserData = AList);
end;

procedure DoURLEntryPointAlreadyExists(const AArgs: array of const);
begin
  TBrookURLEntryPoint(AArgs[0].VObject).Name := '/api1';
end;

procedure Test_URLEntryPointName(AList: TBrookURLEntryPointList);
var
  EP1, EP2: TBrookURLEntryPoint;
begin
  AList.Clear;

  EP1 := TBrookURLEntryPoint.Create(nil);
  try
    Assert(EP1.Name = '/');
    EP1.Name := '';
    Assert(EP1.Name = '/');
  finally
    EP1.Free;
  end;

  EP1 := TBrookURLEntryPoint.Create(AList);
  Assert(EP1.Name = '/api1');
  EP1.Name := '';
  Assert(EP1.Name = '/');

  EP1 := AList.Add;
  Assert(EP1.Name = '/api1');
  EP2 := AList.Add;
  AssertExcept(DoURLEntryPointAlreadyExists, EBrookURLEntryPoint,
    Format(SBrookEntryPointAlreadyExists, [EP2.GetNamePath, '/api1',
    EP1.GetNamePath]), [EP2]);
end;

procedure Test_URLEntryPointRouter(AList: TBrookURLEntryPointList);
var
  EP: TBrookURLEntryPoint;
  R: TBrookURLRouter;
begin
  AList.Clear;

  EP := TBrookURLEntryPoint.Create(AList);
  Assert(not Assigned(EP.Router));
  R := TBrookURLRouter.Create(nil);
  try
    EP.Router := R;
    Assert(EP.Router = R);
    Assert(EP.UserData = R);
  finally
    R.Free;
  end;
end;

procedure Test_URLEntryPointListCreate;
var
  M: TBrookURLEntryPointList;
  P: TPersistent;
begin
  P := TPersistent.Create;
  M := TBrookURLEntryPointList.Create(P);
  try
    Assert(M.Owner = P);
    M.Add;
    M.Prepare;
    Assert(M.Prepared);
    TBrookLibraryLoader.Unload;
    Assert(not M.Prepared);
    TBrookLibraryLoader.Load;
  finally
    M.Free;
    P.Free;
  end;
end;

procedure Test_URLEntryPointListGetEntryPointClass;
begin
  Assert(TBrookURLEntryPointList.GetEntryPointClass = TBrookURLEntryPoint);
end;

procedure DoURLEntryPointListPrepareValidate(const AArgs: array of const);
begin
  TBrookURLEntryPointList(AArgs[0].VObject).Prepare;
end;

procedure Test_URLEntryPointListPrepare;
var
  L1: TBrookURLEntryPointList;
  L2: TFakeURLEntryPointList;
  P: TBrookURLEntryPoint;
begin
  L1 := TBrookURLEntryPointList.Create(nil);
  try
    Assert(not L1.Prepared);
    L1.Prepare;
    Assert(not L1.Prepared);
    L1.Add;
    L1.Prepare;
    L1.Prepare;
    Assert(L1.Prepared);
    TBrookLibraryLoader.Unload;
    Assert(not L1.Prepared);
    TBrookLibraryLoader.Load;
  finally
    L1.Free;
  end;

  L2 := TFakeURLEntryPointList.Create(nil);
  try
    P := L2.Add;
    AssertExcept(DoURLEntryPointListPrepareValidate, EBrookURLEntryPoint,
      Format(SBrookEmptyEntryPointName, [P.GetNamePath]), [L2]);
  finally
    L2.Free;
  end;
end;

procedure Test_URLEntryPointListUnprepare;
var
  L: TBrookURLEntryPointList;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    Assert(not L.Prepared);
    L.Add;
    L.Prepare;
    Assert(L.Prepared);
    L.Unprepare;
    Assert(not L.Prepared);
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointListIsPrepared;
var
  L: TBrookURLEntryPointList;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    Assert(not L.IsPrepared);
    L.Add;
    L.Prepare;
    Assert(L.IsPrepared);
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointListNewName;
var
  L: TBrookURLEntryPointList;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    Assert(L.NewName = '/api1');
    L.Add;
    Assert(L.NewName = '/api2');
    L.Add;
    Assert(L.NewName = '/api3');
    L.Add;
    Assert(L.NewName = '/api4');
    L.Delete(L.Count - 1);
    Assert(L.NewName = '/api3');
    L.Delete(0);
    Assert(L.NewName = '/api1');
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointListAdd;
var
  L: TBrookURLEntryPointList;
  P: TBrookURLEntryPoint;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    Assert(L.Count = 0);
    P := L.Add;
    Assert(Assigned(P));
    Assert(P.Name = '/api1');
    Assert(L.Count = 1);
    P := L.Add;
    Assert(Assigned(P));
    Assert(P.Name = '/api2');
    Assert(L.Count = 2);
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointListRemove;
var
  L: TBrookURLEntryPointList;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    L.Add;
    L.Add;
    L.Add;
    Assert(L.Count = 3);
    Assert(Assigned(L.FindInList('/api1')));
    Assert(Assigned(L.FindInList('/api2')));
    Assert(Assigned(L.FindInList('/api3')));
    L.Remove('/api1');
    Assert(not Assigned(L.FindInList('/api1')));
    Assert(Assigned(L.FindInList('/api2')));
    Assert(Assigned(L.FindInList('/api3')));
    L.Remove('/api3');
    Assert(not Assigned(L.FindInList('/api1')));
    Assert(Assigned(L.FindInList('/api2')));
    Assert(not Assigned(L.FindInList('/api3')));
    L.Remove('/api2');
    Assert(not Assigned(L.FindInList('/api1')));
    Assert(not Assigned(L.FindInList('/api2')));
    Assert(not Assigned(L.FindInList('/api3')));
    Assert(L.Count = 0);
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointListIndexOf;
var
  L: TBrookURLEntryPointList;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    Assert(L.IndexOf('/api1') = -1);
    Assert(L.IndexOf('/api2') = -1);
    Assert(L.IndexOf('/api3') = -1);
    L.Add;
    L.Add;
    L.Add;
    Assert(L.IndexOf('/api1') = 0);
    Assert(L.IndexOf('/api2') = 1);
    Assert(L.IndexOf('/api3') = 2);
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointListFindInList;
var
  L: TBrookURLEntryPointList;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    Assert(not Assigned(L.FindInList('/api1')));
    Assert(not Assigned(L.FindInList('/api2')));
    Assert(not Assigned(L.FindInList('/api3')));
    L.Add;
    L.Add;
    L.Add;
    Assert(Assigned(L.FindInList('/api1')));
    Assert(Assigned(L.FindInList('/api2')));
    Assert(Assigned(L.FindInList('/api3')));
  finally
    L.Free;
  end;
end;

procedure DoURLEntryPointListFindUnprepared(const AArgs: array of const);
var
  UD: Pointer;
begin
  TBrookURLEntryPointList(AArgs[0].VObject).Find('a', UD);
end;

procedure Test_URLEntryPointListFind(AList: TBrookURLEntryPointList);
var
  L: TBrookURLEntryPointList;
  UD: Pointer;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    L.Add.UserData := AList;
    L.Add;
    L.Add.UserData := L;
    L.Prepare;
    UD := nil;
    Assert(not L.Find('/x1', UD));
    Assert(not Assigned(UD));
    Assert(not L.Find('/x2', UD));
    Assert(not Assigned(UD));
    Assert(not L.Find('/x3', UD));
    Assert(not Assigned(UD));
    L.Unprepare;
    L.Prepare;
    Assert(L.Find('/api1', UD));
    Assert(UD = AList);
    Assert(L.Find('api2', UD));
    Assert(not Assigned(UD));
    Assert(L.Find('/api3', UD));
    Assert(UD = L);
    L.Find('', UD);

    L.Unprepare;
    AssertExcept(DoURLEntryPointListFindUnprepared, EBrookURLEntryPointList,
      SBrookEntryPointListUnprepared, [L]);
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointListClear;
var
  L: TBrookURLEntryPointList;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    L.Add;
    L.Add;
    L.Add;
    L.Prepare;
    Assert(L.Prepared);
    Assert(L.Count = 3);
    L.Clear;
    Assert(L.Count = 0);
    L.Clear;
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointListItems;
var
  L: TBrookURLEntryPointList;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    L.Add;
    L.Add;
    L.Add;
    Assert(L[0].Name = '/api1');
    Assert(L[1].Name = '/api2');
    Assert(L[2].Name = '/api3');
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointListPrepared;
var
  L: TBrookURLEntryPointList;
begin
  L := TBrookURLEntryPointList.Create(nil);
  try
    Assert(not L.Prepared);
    L.Add;
    L.Prepare;
    Assert(L.Prepared);
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointsCreate;
var
  EPL: TBrookURLEntryPoints;
begin
  EPL := TBrookURLEntryPoints.Create(nil);
  try
    Assert(Assigned(EPL.List));
  finally
    EPL.Free;
  end;
end;

procedure Test_URLEntryPointsAdd;
var
  EPL: TBrookURLEntryPoints;
  EP: TBrookURLEntryPoint;
begin
  EPL := TBrookURLEntryPoints.Create(nil);
  try
    Assert(EPL.List.Count = 0);
    EP := EPL.Add;
    Assert(Assigned(EP));
    EP := EPL.Add;
    Assert(Assigned(EP));
    EP := EPL.Add;
    Assert(Assigned(EP));
    Assert(EPL.List.Count = 3);
  finally
    EPL.Free;
  end;
end;

procedure Test_URLEntryPointsRemove;
var
  EPL: TBrookURLEntryPoints;
  EP: TBrookURLEntryPoint;
begin
  EPL := TBrookURLEntryPoints.Create(nil);
  try
    Assert(EPL.List.Count = 0);
    EP := EPL.Add;
    Assert(Assigned(EP));
    EP := EPL.Add;
    Assert(Assigned(EP));
    EP := EPL.Add;
    Assert(Assigned(EP));
    Assert(EPL.List.Count = 3);
    EPL.Remove('/api1');
    EPL.Remove('/api2');
    EPL.Remove('/api3');
    Assert(EPL.List.Count = 0);
  finally
    EPL.Free;
  end;
end;

procedure Test_URLEntryPointsClear;
var
  EPL: TBrookURLEntryPoints;
  EP: TBrookURLEntryPoint;
begin
  EPL := TBrookURLEntryPoints.Create(nil);
  try
    Assert(EPL.List.Count = 0);
    EP := EPL.Add;
    Assert(Assigned(EP));
    EP := EPL.Add;
    Assert(Assigned(EP));
    EP := EPL.Add;
    Assert(Assigned(EP));
    Assert(EPL.List.Count = 3);
    EPL.Clear;
    Assert(EPL.List.Count = 0);
  finally
    EPL.Free;
  end;
end;

procedure Test_URLEntryPointsOpen;
var
  EPL: TBrookURLEntryPoints;
begin
  EPL := TBrookURLEntryPoints.Create(nil);
  try
    Assert(not EPL.Active);
    EPL.Open;
    Assert(not EPL.Active);
    EPL.Add;
    EPL.Open;
    Assert(EPL.Active);
    EPL.Open;
  finally
    EPL.Free;
  end;
end;

procedure Test_URLEntryPointsClose;
var
  EPL: TBrookURLEntryPoints;
begin
  EPL := TBrookURLEntryPoints.Create(nil);
  try
    EPL.Add;
    EPL.Open;
    Assert(EPL.Active);
    EPL.Close;
    Assert(not EPL.Active);
    EPL.Close;
  finally
    EPL.Free;
  end;
end;

procedure DoURLEntryPointsEnterNoEntryPointsDefined(const AArgs: array of const);
begin
  TFakeURLEntryPoints(AArgs[0].VObject).Enter(nil, '/api', nil, nil);
end;

procedure DoURLEntryPointsEnterInactiveEntryPoints(const AArgs: array of const);
begin
  TFakeURLEntryPoints(AArgs[0].VObject).Enter(nil, '/api', nil, nil);
end;

procedure DoURLEntryPointsEnterParamIsNil(const AArgs: array of const);
begin
  TFakeURLEntryPoints(AArgs[0].VObject).Enter(nil, nil, nil);
end;

procedure Test_URLEntryPointsEnter;
var
  RQ: TFakeHTTPRequest;
  RS: TFakeHTTPResponse;
  EPL: TFakeURLEntryPoints;
  EP: TBrookURLEntryPoint;
  RT: TFakeURLRouter;
begin
  RQ := TFakeHTTPRequest.Create(nil);
  RS := TFakeHTTPResponse.Create(nil);
  EPL := TFakeURLEntryPoints.Create(nil);
  RT := TFakeURLRouter.Create(nil);
  try
    EPL.Add.Router := RT;
    EPL.Open;
    FakeFlag := False;
    FakeRoutedPath := '/';
    EPL.Enter(nil, 'api1', nil, nil);
    Assert(FakeFlag);
    FakeFlag := False;
    FakeRoutedPath := '/test';
    EPL.Enter(nil, 'api1/test', nil, nil);
    Assert(FakeFlag);

    EPL.Close;
    EPL.Add;
    EPL.Open;
    FakeResponseFakeFmt := SBrookRouterNotAssigned;
    FakeRoutedPath := '/api1';
    FakeStatus := 500;
    EPL.Enter(nil, 'api1', nil, RS);

    EPL.OnNotFound := EPL.FakeOnNotFound;
    FakeResponseFakeFmt := SBrookEntryPointNotFound;
    FakeRoutedPath := '/xxx';
    FakeStatus := 404;
    EPL.Enter(nil, 'xxx', nil, nil);

    EPL.OnNotFound := nil;
    EPL.Enter(nil, 'xxx', nil, RS);

    EPL.Clear;
    AssertExcept(DoURLEntryPointsEnterNoEntryPointsDefined,
      EBrookURLEntryPointList, SBrookNoEntryPointsDefined, [EPL]);

    EPL.Open;
    EPL.Add;
    EPL.Close;
    AssertExcept(DoURLEntryPointsEnterInactiveEntryPoints,
      EInvalidOpException, SBrookInactiveEntryPoints, [EPL]);

    EPL.Clear;
    EP := EPL.Add;
    EP.Router := RT;
    EP.Name := '/';
    EPL.Open;
    EPL.Enter(nil, RQ, nil);

    AssertExcept(DoURLEntryPointsEnterParamIsNil,
      EArgumentNilException, Format(SParamIsNil, ['ARequest']), [EPL]);
  finally
    RT.Free;
    EPL.Free;
    RS.Free;
    RQ.Free;
  end;
end;

procedure Test_URLEntryPointsItems;
var
  L: TBrookURLEntryPoints;
begin
  L := TBrookURLEntryPoints.Create(nil);
  try
    L.Add;
    L.Add;
    L.Add;
    Assert(L[0].Name = '/api1');
    Assert(L[1].Name = '/api2');
    Assert(L[2].Name = '/api3');
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointsActive;
var
  L: TBrookURLEntryPoints;
begin
  L := TBrookURLEntryPoints.Create(nil);
  try
    L.Add;
    Assert(not L.Active);
    L.Active := not L.Active;
    Assert(L.Active);
    Assert(L.List.Prepared);
  finally
    L.Free;
  end;
end;

procedure Test_URLEntryPointsOnNotFound;
var
  EPL: TFakeURLEntryPoints;
begin
  EPL := TFakeURLEntryPoints.Create(nil);
  try
    EPL.Add;
    EPL.Open;
    EPL.OnNotFound := EPL.FakeOnNotFound;
    FakeResponseFakeFmt := SBrookEntryPointNotFound;
    FakeRoutedPath := '/xxx';
    FakeStatus := 404;
    FakeFlag := False;
    EPL.Enter(nil, 'xxx', nil, nil);
    Assert(FakeFlag);
  finally
    EPL.Free;
  end;
end;

procedure Test_URLEntryPointsOnActivate;
var
  EPL: TFakeURLEntryPoints;
begin
  EPL := TFakeURLEntryPoints.Create(nil);
  try
    EPL.OnActivate := EPL.FakeOnActivate;
    FakeFlag := False;
    EPL.Add;
    EPL.Open;
    Assert(FakeFlag);
  finally
    EPL.Free;
  end;
end;

procedure Test_URLEntryPointsOnDeactivate;
var
  EPL: TFakeURLEntryPoints;
begin
  EPL := TFakeURLEntryPoints.Create(nil);
  try
    EPL.Add;
    EPL.Open;
    EPL.OnDeactivate := EPL.FakeOnDeactivate;
    FakeFlag := False;
    EPL.Close;
    Assert(FakeFlag);
  finally
    EPL.Free;
  end;
end;

var
  L: TBrookURLEntryPointList;
begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  L := TBrookURLEntryPointList.Create(nil);
  try
    Test_URLEntryPointCreate(L);
    Test_URLEntryPointValidate(L);
    Test_URLEntryPointUserData(L);
    Test_URLEntryPointName(L);
    Test_URLEntryPointRouter(L);
    Test_URLEntryPointListCreate;
    Test_URLEntryPointListGetEntryPointClass;
    Test_URLEntryPointListPrepare;
    Test_URLEntryPointListUnprepare;
    Test_URLEntryPointListIsPrepared;
    Test_URLEntryPointListNewName;
    Test_URLEntryPointListAdd;
    Test_URLEntryPointListRemove;
    Test_URLEntryPointListIndexOf;
    Test_URLEntryPointListFindInList;
    Test_URLEntryPointListFind(L);
    Test_URLEntryPointListClear;
    Test_URLEntryPointListItems;
    Test_URLEntryPointListPrepared;
    Test_URLEntryPointsCreate;
    Test_URLEntryPointsAdd;
    Test_URLEntryPointsRemove;
    Test_URLEntryPointsClear;
    Test_URLEntryPointsOpen;
    Test_URLEntryPointsClose;
    Test_URLEntryPointsEnter;
    Test_URLEntryPointsItems;
    Test_URLEntryPointsActive;
    Test_URLEntryPointsOnNotFound;
    Test_URLEntryPointsOnActivate;
    Test_URLEntryPointsOnDeactivate;
  finally
    L.Free;
    TBrookLibraryLoader.Unload;
  end;
end.
