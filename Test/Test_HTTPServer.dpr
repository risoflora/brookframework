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

program Test_HTTPServer;

{$I Tests.inc}

uses
  SysUtils,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  BrookLibraryLoader,
  BrookHTTPServer,
  Test;

procedure Test_HTTPServerSecurityAssign;
var
  S, D: TBrookHTTPServerSecurity;
begin
  S := TBrookHTTPServerSecurity.Create;
  D := TBrookHTTPServerSecurity.Create;
  try
    Assert(D.PrivateKey.IsEmpty);
    Assert(D.PrivatePassword.IsEmpty);
    Assert(D.Certificate.IsEmpty);
    Assert(D.Trust.IsEmpty);
    Assert(D.DHParams.IsEmpty);
    S.PrivateKey := 'fake_pkey';
    S.PrivatePassword := 'fake_ppass';
    S.Certificate := 'fake_cert';
    S.Trust := 'fake_trust';
    S.DHParams := 'fake_dhparams';
    D.Assign(S);
    Assert(D.PrivateKey = 'fake_pkey');
    Assert(D.PrivatePassword = 'fake_ppass');
    Assert(D.Certificate = 'fake_cert');
    Assert(D.Trust = 'fake_trust');
    Assert(D.DHParams = 'fake_dhparams');
  finally
    S.Free;
    D.Free;
  end;
end;

procedure Test_HTTPServerSecurityClear;
var
  S: TBrookHTTPServerSecurity;
begin
  S := TBrookHTTPServerSecurity.Create;
  try
    S.Active := True;
    S.PrivateKey := 'fake_pkey';
    S.PrivatePassword := 'fake_ppass';
    S.Certificate := 'fake_cert';
    S.Trust := 'fake_trust';
    S.DHParams := 'fake_dhparams';
    Assert(S.Active);
    Assert(not S.PrivateKey.IsEmpty);
    Assert(not S.PrivatePassword.IsEmpty);
    Assert(not S.Certificate.IsEmpty);
    Assert(not S.Trust.IsEmpty);
    Assert(not S.DHParams.IsEmpty);
    S.Clear;
    Assert(not S.Active);
    Assert(S.PrivateKey.IsEmpty);
    Assert(S.PrivatePassword.IsEmpty);
    Assert(S.Certificate.IsEmpty);
    Assert(S.Trust.IsEmpty);
    Assert(S.DHParams.IsEmpty);
  finally
    S.Free;
  end;
end;

procedure DoHTTPServerSecurityValidateError(const AArgs: array of const);
begin
  TBrookHTTPServerSecurity(AArgs[0].VObject).Validate;
end;

procedure Test_HTTPServerSecurityValidate;
var
  S: TBrookHTTPServerSecurity;
begin
  S := TBrookHTTPServerSecurity.Create;
  try
    S.PrivateKey := 'fake_pkey';
    S.Certificate := 'fake_cert';
    Assert(not S.PrivateKey.IsEmpty);
    Assert(not S.Certificate.IsEmpty);

    S.PrivateKey := '';
    AssertExcept(DoHTTPServerSecurityValidateError, EBrookHTTPServerSecurity,
      SBrookEmptyPrivateKey, [S]);
    S.PrivateKey := 'fake_pkey';
    S.Certificate := '';
    AssertExcept(DoHTTPServerSecurityValidateError, EBrookHTTPServerSecurity,
      SBrookEmptyCertificate, [S]);
  finally
    S.Free;
  end;
end;

procedure Test_HTTPServerSecurityActive;
var
  S: TBrookHTTPServerSecurity;
begin
  S := TBrookHTTPServerSecurity.Create;
  try
    Assert(not S.Active);
    S.Active := not S.Active;
    Assert(S.Active);
  finally
    S.Free;
  end;
end;

procedure Test_HTTPServerSecurityPrivateKey;
var
  S: TBrookHTTPServerSecurity;
begin
  S := TBrookHTTPServerSecurity.Create;
  try
    Assert(S.PrivateKey.IsEmpty);
    S.PrivateKey := 'fake_pkey';
    Assert(S.PrivateKey = 'fake_pkey');
  finally
    S.Free;
  end;
end;

procedure Test_HTTPServerSecurityPrivatePassword;
var
  S: TBrookHTTPServerSecurity;
begin
  S := TBrookHTTPServerSecurity.Create;
  try
    Assert(S.PrivatePassword.IsEmpty);
    S.PrivatePassword := 'fake_ppass';
    Assert(S.PrivatePassword = 'fake_ppass');
  finally
    S.Free;
  end;
end;

procedure Test_HTTPServerSecurityCertificate;
var
  S: TBrookHTTPServerSecurity;
begin
  S := TBrookHTTPServerSecurity.Create;
  try
    Assert(S.Certificate.IsEmpty);
    S.Certificate := 'fake_cert';
    Assert(S.Certificate = 'fake_cert');
  finally
    S.Free;
  end;
end;

procedure Test_HTTPServerSecurityTrust;
var
  S: TBrookHTTPServerSecurity;
begin
  S := TBrookHTTPServerSecurity.Create;
  try
    Assert(S.Trust.IsEmpty);
    S.Trust := 'fake_trust';
    Assert(S.Trust = 'fake_trust');
  finally
    S.Free;
  end;
end;

procedure Test_HTTPServerSecurityDHParams;
var
  S: TBrookHTTPServerSecurity;
begin
  S := TBrookHTTPServerSecurity.Create;
  try
    Assert(S.DHParams.IsEmpty);
    S.DHParams := 'fake_dhparams';
    Assert(S.DHParams = 'fake_dhparams');
  finally
    S.Free;
  end;
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  try
    Test_HTTPServerSecurityAssign;
    Test_HTTPServerSecurityClear;
    Test_HTTPServerSecurityValidate;
    Test_HTTPServerSecurityActive;
    Test_HTTPServerSecurityPrivateKey;
    Test_HTTPServerSecurityPrivatePassword;
    Test_HTTPServerSecurityCertificate;
    Test_HTTPServerSecurityTrust;
    Test_HTTPServerSecurityDHParams;
  finally
    TBrookLibraryLoader.Unload;
  end;
end.
