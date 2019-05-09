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
 * (at your option) any laterbroo version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook framework.  If not, see <http://www.gnu.org/licenses/>.
 *)

program Test_HTTPAuthentication;

{$I Tests.inc}

uses
  SysUtils,
  StrUtils,
  Platform,
  Marshalling,
  libsagui,
  BrookLibraryLoader,
  BrookHTTPAuthentication,
  Test;

type
  TFakeHTTPAuth = class
  public
    ErrorCode: Word;
    Canceled: Boolean;
  end;

var
  FakeHTTPAuth: TFakeHTTPAuth;
  FakeHTTPAuthHandle: Pointer;
  FakeRealm: string;

function fake_httpauth_usr(auth: Psg_httpauth): Pcchar; cdecl;
begin
  Assert(auth = FakeHTTPAuthHandle);
  Result := 'foo';
end;

function fake_httpauth_pwd(auth: Psg_httpauth): Pcchar; cdecl;
begin
  Assert(auth = FakeHTTPAuthHandle);
  Result := 'bar';
end;

function fake_httpauth_realm(auth: Psg_httpauth): pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(auth = FakeHTTPAuthHandle);
  Result := M.ToCString(FakeRealm);
end;

function fake_httpauth_set_realm(auth: Psg_httpauth;
  const realm: Pcchar): cint; cdecl;
begin
  Assert(auth = FakeHTTPAuthHandle);
  FakeRealm := TMarshal.ToString(realm);
  Result := 0;
end;

function fake_httpauth_deny2(auth: Psg_httpauth; const reason: Pcchar;
  const content_type: Pcchar; status: cuint): cint; cdecl;
begin
  Assert(auth = FakeHTTPAuthHandle);
  Assert(reason = 'fake_reason');
  Assert(content_type = 'fake_content_type');
  Assert(status = 200);
  Result := TFakeHTTPAuth(auth).ErrorCode;
end;

function fake_httpauth_deny(auth: Psg_httpauth; const reason: Pcchar;
  const content_type: Pcchar): cint; cdecl;
begin
  Assert(auth = FakeHTTPAuthHandle);
  Assert(reason = 'fake_reason');
  Assert(content_type = 'fake_content_type');
  Result := TFakeHTTPAuth(auth).ErrorCode;
end;

function fake_httpauth_cancel(auth: Psg_httpauth): cint; cdecl;
var
  A: TFakeHTTPAuth;
begin
  Assert(auth = FakeHTTPAuthHandle);
  A := TFakeHTTPAuth(auth);
  A.Canceled := True;
  Result := A.ErrorCode;
end;

procedure AssignFakeAPI; inline;
begin
  sg_httpauth_usr := fake_httpauth_usr;
  sg_httpauth_pwd := fake_httpauth_pwd;
  sg_httpauth_realm := fake_httpauth_realm;
  sg_httpauth_set_realm := fake_httpauth_set_realm;
  sg_httpauth_deny2 := fake_httpauth_deny2;
  sg_httpauth_deny := fake_httpauth_deny;
  sg_httpauth_cancel := fake_httpauth_cancel;
end;

procedure Test_HTTPCredentialsCreate;
var
  C: TBrookHTTPCredentials;
begin
  C := TBrookHTTPCredentials.Create(FakeHTTPAuthHandle);
  try
    Assert(C.Handle = FakeHTTPAuthHandle);
    Assert(C.UserName = 'foo');
    Assert(C.Password = 'bar');
  finally
    C.Free;
  end;
end;

procedure Test_HTTPCredentialsRealm;
var
  C: TBrookHTTPCredentials;
begin
  C := TBrookHTTPCredentials.Create(FakeHTTPAuthHandle);
  try
    C.Realm := 'foo';
    Assert(C.Realm = 'foo');
    C.Realm := 'bar';
    Assert(C.Realm = 'bar');
  finally
    C.Free;
  end;
end;

procedure Test_HTTPCredentialsUserName;
var
  C: TBrookHTTPCredentials;
begin
  C := TBrookHTTPCredentials.Create(FakeHTTPAuthHandle);
  try
    Assert(C.UserName = 'foo');
  finally
    C.Free;
  end;
end;

procedure Test_HTTPCredentialsPassword;
var
  C: TBrookHTTPCredentials;
begin
  C := TBrookHTTPCredentials.Create(FakeHTTPAuthHandle);
  try
    Assert(C.Password = 'bar');
  finally
    C.Free;
  end;
end;

procedure Test_HTTPAuthenticationCreate;
var
  A: TBrookHTTPAuthentication;
begin
  A := TBrookHTTPAuthentication.Create(FakeHTTPAuthHandle);
  try
    Assert(A.Handle = FakeHTTPAuthHandle);
    Assert(Assigned(A.Credentials));
  finally
    A.Free;
  end;
end;

procedure DoHTTPAuthenticationDenyLibraryNotLoaded1(const AArgs: array of const);
begin
  TBrookHTTPAuthentication(AArgs[0].VObject).Deny('', '', 200);
end;

procedure DoHTTPAuthenticationDenyInvalidArgument1(const AArgs: array of const);
begin
  TBrookHTTPAuthentication(AArgs[0].VObject).Deny('fake_reason',
    'fake_content_type', 200);
end;

procedure DoHTTPAuthenticationDenyLibraryNotLoaded2(const AArgs: array of const);
begin
  TBrookHTTPAuthentication(AArgs[0].VObject).Deny('', '');
end;

procedure DoHTTPAuthenticationDenyInvalidArgument2(const AArgs: array of const);
begin
  TBrookHTTPAuthentication(AArgs[0].VObject).Deny('fake_reason',
    'fake_content_type');
end;

procedure Test_HTTPAuthenticationDeny;
var
  A: TBrookHTTPAuthentication;
begin
  A := TBrookHTTPAuthentication.Create(FakeHTTPAuthHandle);
  try
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPAuthenticationDenyLibraryNotLoaded1, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '', SG_LIB_NAME,
          SgLib.GetLastName)]), [A]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeHTTPAuth.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPAuthenticationDenyInvalidArgument1, EINVAL, [A]);
    FakeHTTPAuth.ErrorCode := 0;
    A.Deny('fake_reason', 'fake_content_type', 200);
    A.Deny('%s_%s', ['fake', 'reason'], 'fake_content_type', 200);

    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPAuthenticationDenyLibraryNotLoaded2, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '', SG_LIB_NAME,
          SgLib.GetLastName)]), [A]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeHTTPAuth.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPAuthenticationDenyInvalidArgument2, EINVAL, [A]);
    FakeHTTPAuth.ErrorCode := 0;
    A.Deny('fake_reason', 'fake_content_type');
    A.Deny('%s_%s', ['fake', 'reason'], 'fake_content_type');
  finally
    A.Free;
  end;
end;

procedure DoHTTPAuthenticationCancelLibraryNotLoaded(const AArgs: array of const);
begin
  TBrookHTTPAuthentication(AArgs[0].VObject).Cancel;
end;

procedure DoHTTPAuthenticationCancelInvalidArgument(const AArgs: array of const);
begin
  TBrookHTTPAuthentication(AArgs[0].VObject).Cancel;
end;

procedure Test_HTTPAuthenticationCancel;
var
  A: TBrookHTTPAuthentication;
begin
  A := TBrookHTTPAuthentication.Create(FakeHTTPAuthHandle);
  try
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoHTTPAuthenticationCancelLibraryNotLoaded, ESgLibNotLoaded,
        Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '', SG_LIB_NAME,
          SgLib.GetLastName)]), [A]);
    finally
      TBrookLibraryLoader.Load;
    end;
    AssignFakeAPI;
    FakeHTTPAuth.ErrorCode := EINVAL;
    AssertOSExcept(DoHTTPAuthenticationCancelInvalidArgument, EINVAL, [A]);
    FakeHTTPAuth.ErrorCode := 0;
    FakeHTTPAuth.Canceled := False;
    A.Cancel;
    Assert(FakeHTTPAuth.Canceled);
  finally
    A.Free;
  end;
end;

procedure Test_HTTPAuthenticationCredentials;
var
  A: TBrookHTTPAuthentication;
begin
  A := TBrookHTTPAuthentication.Create(FakeHTTPAuthHandle);
  try
    Assert(Assigned(A.Credentials));
    Assert(A.Credentials.UserName = 'foo');
    Assert(A.Credentials.Password = 'bar');
  finally
    A.Free;
  end;
end;

begin
  TBrookLibraryLoader.Load;
  FakeHTTPAuth := TFakeHTTPAuth.Create;
  FakeHTTPAuthHandle := FakeHTTPAuth;
  try
    AssignFakeAPI;
    Test_HTTPCredentialsCreate;
    Test_HTTPCredentialsRealm;
    Test_HTTPCredentialsUserName;
    Test_HTTPCredentialsPassword;
    Test_HTTPAuthenticationCreate;
    Test_HTTPAuthenticationCreate;
    // Test_HTTPAuthenticationDestroy - not required
    Test_HTTPAuthenticationDeny;
    Test_HTTPAuthenticationCancel;
    Test_HTTPAuthenticationCredentials;
  finally
    FakeHTTPAuth.Free;
    TBrookLibraryLoader.Unload;
  end;
end.
