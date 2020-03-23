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

program Test_HTTPCookies;

{$I Tests.inc}

uses
  SysUtils,
  DateUtils,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  BrookLibraryLoader,
  BrookUtility,
  BrookHTTPCookies,
  Test;

const
  TEST_SIGNED_VALUE = 's:bar.RrTsWGEXFU2s1J1mTl1j/ciO+1E';

procedure Test_BrookHTTPCookieCreate;
var
  VCookie: TBrookHTTPCookie;
  VCookies: TBrookHTTPCookies;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(VCookie.Name = BROOK_COOKIE_NAME_PREFIX);
    Assert(VCookie.Expires = -1);
    Assert(VCookie.MaxAge = -1);
    Assert(VCookie.Path = '/');
  finally
    VCookie.Free;
  end;

  VCookies := TBrookHTTPCookies.Create(nil);
  try
    VCookie := TBrookHTTPCookie.Create(VCookies);
    Assert(VCookie.Name = Format('%s%d', [BROOK_COOKIE_NAME_PREFIX, 1]));
    VCookie := TBrookHTTPCookie.Create(VCookies);
    Assert(VCookie.Name = Format('%s%d', [BROOK_COOKIE_NAME_PREFIX, 2]));
  finally
    VCookies.Free;
  end;
end;

procedure Test_BrookHTTPCookieAssign;
var
  VSrcCookie, VDestCookie: TBrookHTTPCookie;
  VExpires: TDateTime;
begin
  VSrcCookie := TBrookHTTPCookie.Create(nil);
  VDestCookie := TBrookHTTPCookie.Create(nil);
  try
    VSrcCookie.Name := 'abc123';
    VSrcCookie.Value := 'def456';
    VSrcCookie.Domain := 'foo';
    VSrcCookie.Path := '/bar';
    VSrcCookie.Expires := Now;
    VSrcCookie.HttpOnly := True;
    VSrcCookie.Secure := True;
    VSrcCookie.MaxAge := 456;
    VSrcCookie.SameSite := ssLax;
    Assert(VDestCookie.Name = BROOK_COOKIE_NAME_PREFIX);
    Assert(VDestCookie.Value = '');
    Assert(VDestCookie.Domain = '');
    Assert(VDestCookie.Path = '/');
    Assert(VDestCookie.Expires = -1);
    Assert(not VDestCookie.HttpOnly);
    Assert(not VDestCookie.Secure);
    Assert(VDestCookie.MaxAge = -1);
    Assert(VDestCookie.SameSite = ssNone);
    VDestCookie.Assign(VSrcCookie);
    VExpires := VSrcCookie.Expires;
    Assert(VDestCookie.Name = 'abc123');
    Assert(VDestCookie.Value = 'def456');
    Assert(VDestCookie.Domain = 'foo');
    Assert(VDestCookie.Path = '/bar');
    Assert(CompareDateTime(VDestCookie.Expires, VExpires) = 0);
    Assert(VDestCookie.HttpOnly);
    Assert(VDestCookie.Secure);
    Assert(VDestCookie.MaxAge = 456);
    Assert(VDestCookie.SameSite = ssLax);
  finally
    VSrcCookie.Free;
    VDestCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieSign;
var
  VCookie: TBrookHTTPCookie;
begin
  Assert(TBrookHTTPCookie.Sign('foo', 'bar') = TEST_SIGNED_VALUE);

  VCookie := TBrookHTTPCookie.Create(nil);
  try
    VCookie.Value := 'bar';
    VCookie.Sign('foo');
    Assert(VCookie.Value = TEST_SIGNED_VALUE);
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieTryUnsign;
var
  VCookie: TBrookHTTPCookie;
  VValue: string;
begin
  Assert(TBrookHTTPCookie.TryUnsign('foo', TEST_SIGNED_VALUE, VValue));
  Assert(VValue = 'bar');
  VValue := '';
  Assert(not TBrookHTTPCookie.TryUnsign('foo', 'xxx', VValue));
  Assert(VValue = '');
  VValue := '';
  Assert(not TBrookHTTPCookie.TryUnsign('foo', 'xxx.yyy', VValue));
  Assert(VValue = '');

  VCookie := TBrookHTTPCookie.Create(nil);
  try
    VCookie.Value := TEST_SIGNED_VALUE;
    VCookie.TryUnsign('foo');
    Assert(VCookie.Value = 'bar');
    VCookie.Value := 'xxx';
    Assert(not VCookie.TryUnsign('foo'));
    Assert(VCookie.Value = 'xxx');
    VCookie.Value := 'xxx.yyy';
    Assert(not VCookie.TryUnsign('foo'));
    Assert(VCookie.Value = 'xxx.yyy');
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieUnsign;
var
  VCookie: TBrookHTTPCookie;
begin
  Assert(TBrookHTTPCookie.Unsign('foo', TEST_SIGNED_VALUE) = 'bar');
  Assert(TBrookHTTPCookie.Unsign('foo', 'xxx') = '');
  Assert(TBrookHTTPCookie.Unsign('foo', 'xxx.yyy') = '');

  VCookie := TBrookHTTPCookie.Create(nil);
  try
    VCookie.Value := TEST_SIGNED_VALUE;
    VCookie.Unsign('foo');
    Assert(VCookie.Value = 'bar');
    VCookie.Value := 'xxx';
    VCookie.Unsign('foo');
    Assert(VCookie.Value = '');
    VCookie.Value := 'xxx.yyy';
    VCookie.Unsign('foo');
    Assert(VCookie.Value = '');
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieIsSigned;
var
  VCookie: TBrookHTTPCookie;
begin
  Assert(TBrookHTTPCookie.IsSigned(TEST_SIGNED_VALUE));

  VCookie := TBrookHTTPCookie.Create(nil);
  try
    VCookie.Value := TEST_SIGNED_VALUE;
    Assert(VCookie.IsSigned);
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieToString;
const
  COOKIE_FMT =
    'foo=bar; Max-Age=10; Domain=domain; Path=/path; Expires=%s; HttpOnly; ' +
    'Secure; SameSite=Lax';
var
  VCookie: TBrookHTTPCookie;
  VNow: TDateTime;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    VNow := Now;
    VCookie.Name := 'foo';
    VCookie.Value := 'bar';
    VCookie.Domain := 'domain';
    VCookie.Path := '/path';
    VCookie.Expires := VNow;
    VCookie.HttpOnly := True;
    VCookie.Secure := True;
    VCookie.MaxAge := 10;
    VCookie.SameSite := ssLax;
    Assert(VCookie.ToString = Format(COOKIE_FMT, [
      Brook.DateTimeToGmt(Brook.DateTimeToUTC(IncSecond(VNow, 10)))]));
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieClear;
const
  COOKIE_FMT =
    'foo=bar; Max-Age=10; Domain=domain; Path=/path; Expires=%s; HttpOnly; ' +
    'Secure; SameSite=Lax';
var
  VCookie: TBrookHTTPCookie;
  VNow: TDateTime;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    VNow := Now;
    VCookie.Name := 'foo';
    VCookie.Value := 'bar';
    VCookie.Domain := 'domain';
    VCookie.Path := '/path';
    VCookie.Expires := VNow;
    VCookie.HttpOnly := True;
    VCookie.Secure := True;
    VCookie.MaxAge := 10;
    VCookie.SameSite := ssLax;
    Assert(VCookie.ToString = Format(COOKIE_FMT, [
      Brook.DateTimeToGmt(Brook.DateTimeToUTC(IncSecond(VNow, 10)))]));
    VCookie.Clear;
    Assert(VCookie.ToString = 'foo=');
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieExpire;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(Brook.DateTimeToGmt(VCookie.Expires) =
      'Fri, 29 Dec 1899 00:00:00 GMT');
    VCookie.Expire;
    Assert(Brook.DateTimeToGmt(VCookie.Expires) =
      'Thu, 01 Jan 1970 00:00:00 GMT');
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookiePersist;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(Brook.DateTimeToGmt(VCookie.Expires) =
      'Fri, 29 Dec 1899 00:00:00 GMT');
    VCookie.Persist;
    Assert(Brook.DateTimeToGmt(VCookie.Expires) =
      'Fri, 31 Dec 9999 23:59:59 GMT');
  finally
    VCookie.Free;
  end;
end;

procedure DoHTTPCookieNameEmptyCookieName(const AArgs: array of const);
begin
  TBrookHTTPCookie(AArgs[0].VObject).Name := '';
end;

procedure DoHTTPCookieNameInvalidCookieName(const AArgs: array of const);
begin
  TBrookHTTPCookie(AArgs[0].VObject).Name := '123abc';
end;

procedure Test_BrookHTTPCookieName;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(VCookie.Name = BROOK_COOKIE_NAME_PREFIX);
    VCookie.Name := 'foo';
    Assert(VCookie.Name = 'foo');

    AssertExcept(DoHTTPCookieNameEmptyCookieName, EBrookHTTPCookie,
      SBrookEmptyCookieName, [VCookie]);
    AssertExcept(DoHTTPCookieNameInvalidCookieName, EBrookHTTPCookie,
      Format(SBrookInvalidCookieName, ['123abc']), [VCookie]);
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieValue;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(VCookie.Value = '');
    VCookie.Value := 'foo';
    Assert(VCookie.Value = 'foo');
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieDomain;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(VCookie.Domain = '');
    VCookie.Domain := 'foo';
    Assert(VCookie.Domain = 'foo');
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookiePath;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(VCookie.Path = '/');
    VCookie.Path := 'foo';
    Assert(VCookie.Path = '/foo');
    VCookie.Path := '/foo';
    Assert(VCookie.Path = '/foo');
    VCookie.Path := '/foo/';
    Assert(VCookie.Path = '/foo');
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieExpires;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(Brook.DateTimeToGMT(VCookie.Expires) =
      'Fri, 29 Dec 1899 00:00:00 GMT');
    VCookie.Expires := EncodeDate(9999, 12, 31) + EncodeTime(23, 59, 59, 999);
    Assert(Brook.DateTimeToGMT(VCookie.Expires) =
      'Fri, 31 Dec 9999 23:59:59 GMT');
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieHttpOnly;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(not VCookie.HttpOnly);
    VCookie.HttpOnly := True;
    Assert(VCookie.HttpOnly);
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieSecure;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(not VCookie.Secure);
    VCookie.Secure := True;
    Assert(VCookie.Secure);
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieMaxAge;
var
  VCookie: TBrookHTTPCookie;
  VNow: TDateTime;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(VCookie.MaxAge = -1);
    VNow := Now;
    VCookie.Expires := VNow;
    Assert(Brook.DateTimeToGMT(VCookie.Expires) =
      Brook.DateTimeToGMT(VNow));
    VCookie.MaxAge := 10;
    Assert(Brook.DateTimeToGMT(VCookie.Expires) =
      Brook.DateTimeToGMT(Brook.DateTimeToUTC(IncSecond(VNow, 10))));
    VCookie.MaxAge := 0;
    Assert(Brook.DateTimeToGMT(VCookie.Expires) =
      'Thu, 01 Jan 1970 00:00:00 GMT');
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookieSameSite;
var
  VCookie: TBrookHTTPCookie;
begin
  VCookie := TBrookHTTPCookie.Create(nil);
  try
    Assert(VCookie.SameSite = ssNone);
    VCookie.SameSite := ssLax;
    Assert(VCookie.SameSite = ssLax);
  finally
    VCookie.Free;
  end;
end;

procedure Test_BrookHTTPCookiesCreate;
var
  VCookies: TBrookHTTPCookies;
begin
  VCookies := TBrookHTTPCookies.Create(nil);
  try
    Assert(VCookies.GetCookieClass = TBrookHTTPCookie);
  finally
    VCookies.Free;
  end;
end;

procedure Test_BrookHTTPCookiesGetCookieClass;
begin
  Assert(TBrookHTTPCookies.GetCookieClass = TBrookHTTPCookie);
end;

procedure Test_BrookHTTPCookiesAssign;
var
  VSrcCookies, VDestCookies: TBrookHTTPCookies;
  VCookie: TBrookHTTPCookie;
begin
  VSrcCookies := TBrookHTTPCookies.Create(nil);
  VDestCookies := TBrookHTTPCookies.Create(nil);
  try
    VCookie := VSrcCookies.Add;
    VCookie.Name := 'foo';
    VCookie.Value := 'bar';
    VCookie := VSrcCookies.Add;
    VCookie.Name := 'bar';
    VCookie.Value := 'foo';
    Assert(VSrcCookies.Count = 2);
    Assert(VDestCookies.Count = 0);
    VDestCookies.Assign(VSrcCookies);
    Assert(VDestCookies.Count = 2);
    VCookie := VDestCookies[0];
    Assert(VCookie.Name = 'foo');
    Assert(VCookie.Value = 'bar');
    VCookie := VDestCookies[1];
    Assert(VCookie.Name = 'bar');
    Assert(VCookie.Value = 'foo');
  finally
    VSrcCookies.Free;
    VDestCookies.Free;
  end;
end;

procedure Test_BrookHTTPCookiesAdd;
var
  VCookies: TBrookHTTPCookies;
  VCookie: TBrookHTTPCookie;
begin
  VCookies := TBrookHTTPCookies.Create(nil);
  try
    Assert(VCookies.Count = 0);
    VCookie := VCookies.Add;
    Assert(VCookies.Count = 1);
    Assert(VCookie.Name = 'BrookCookie1');
    VCookie := VCookies.Add;
    Assert(VCookies.Count = 2);
    Assert(VCookie.Name = 'BrookCookie2');
  finally
    VCookies.Free;
  end;
end;

procedure Test_BrookHTTPCookiesRemove;
var
  VCookies: TBrookHTTPCookies;
begin
  VCookies := TBrookHTTPCookies.Create(nil);
  try
    Assert(not Assigned(VCookies.Find('BrookCookie1')));
    Assert(not Assigned(VCookies.Find('BrookCookie2')));
    VCookies.Add;
    VCookies.Add;
    Assert(Assigned(VCookies.Find('BrookCookie1')));
    Assert(Assigned(VCookies.Find('BrookCookie2')));
    Assert(VCookies.Remove('BrookCookie1'));
    Assert(not Assigned(VCookies.Find('BrookCookie1')));
    Assert(Assigned(VCookies.Find('BrookCookie2')));
    Assert(not VCookies.Remove('BrookCookie1'));
    Assert(VCookies.Remove('BrookCookie2'));
    Assert(not Assigned(VCookies.Find('BrookCookie1')));
    Assert(not Assigned(VCookies.Find('BrookCookie2')));
    Assert(not VCookies.Remove('BrookCookie2'));
  finally
    VCookies.Free;
  end;
end;

procedure Test_BrookHTTPCookiesIndexOf;
var
  VCookies: TBrookHTTPCookies;
begin
  VCookies := TBrookHTTPCookies.Create(nil);
  try
    Assert(VCookies.IndexOf('BrookCookie1') = -1);
    Assert(VCookies.IndexOf('BrookCookie2') = -1);
    VCookies.Add;
    VCookies.Add;
    Assert(VCookies.IndexOf('BrookCookie1') = 0);
    Assert(VCookies.IndexOf('BrookCookie2') = 1);
  finally
    VCookies.Free;
  end;
end;

procedure Test_BrookHTTPCookiesFind;
var
  VCookies: TBrookHTTPCookies;
begin
  VCookies := TBrookHTTPCookies.Create(nil);
  try
    Assert(not Assigned(VCookies.Find('BrookCookie1')));
    Assert(not Assigned(VCookies.Find('BrookCookie2')));
    VCookies.Add;
    VCookies.Add;
    Assert(Assigned(VCookies.Find('BrookCookie1')));
    Assert(Assigned(VCookies.Find('BrookCookie2')));
  finally
    VCookies.Free;
  end;
end;

procedure Test_BrookHTTPCookiesFirst;
var
  VCookies: TBrookHTTPCookies;
  VCookie: TBrookHTTPCookie;
begin
  VCookies := TBrookHTTPCookies.Create(nil);
  try
    Assert(not Assigned(VCookies.First));
    VCookies.Add;
    VCookies.Add;
    VCookie := VCookies.First;
    Assert(Assigned(VCookie));
    Assert(VCookie.Name = 'BrookCookie1');
  finally
    VCookies.Free;
  end;
end;

procedure Test_BrookHTTPCookiesLast;
var
  VCookies: TBrookHTTPCookies;
  VCookie: TBrookHTTPCookie;
begin
  VCookies := TBrookHTTPCookies.Create(nil);
  try
    Assert(not Assigned(VCookies.Last));
    VCookies.Add;
    VCookies.Add;
    VCookie := VCookies.Last;
    Assert(Assigned(VCookie));
    Assert(VCookie.Name = 'BrookCookie2');
  finally
    VCookies.Free;
  end;
end;

procedure Test_BrookHTTPCookiesItems;
var
  VCookies: TBrookHTTPCookies;
  VCookie: TBrookHTTPCookie;
begin
  VCookies := TBrookHTTPCookies.Create(nil);
  try
    Assert(VCookies.Count = 0);
    VCookies.Add;
    VCookies.Add;
    Assert(VCookies.Count = 2);
    VCookie := VCookies[0];
    Assert(Assigned(VCookie));
    Assert(VCookie.Name = 'BrookCookie1');
    VCookie := VCookies[1];
    Assert(Assigned(VCookie));
    Assert(VCookie.Name = 'BrookCookie2');
  finally
    VCookies.Free;
  end;
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  try
    Test_BrookHTTPCookieCreate;
    Test_BrookHTTPCookieAssign;
    Test_BrookHTTPCookieSign;
    Test_BrookHTTPCookieTryUnsign;
    Test_BrookHTTPCookieUnsign;
    Test_BrookHTTPCookieIsSigned;
    Test_BrookHTTPCookieToString;
    Test_BrookHTTPCookieClear;
    Test_BrookHTTPCookieExpire;
    Test_BrookHTTPCookiePersist;
    Test_BrookHTTPCookieName;
    Test_BrookHTTPCookieValue;
    Test_BrookHTTPCookieDomain;
    Test_BrookHTTPCookiePath;
    Test_BrookHTTPCookieExpires;
    Test_BrookHTTPCookieHttpOnly;
    Test_BrookHTTPCookieSecure;
    Test_BrookHTTPCookieMaxAge;
    Test_BrookHTTPCookieSameSite;
    Test_BrookHTTPCookiesCreate;
    Test_BrookHTTPCookiesGetCookieClass;
    Test_BrookHTTPCookiesAssign;
    Test_BrookHTTPCookiesAdd;
    Test_BrookHTTPCookiesRemove;
    Test_BrookHTTPCookiesIndexOf;
    Test_BrookHTTPCookiesFind;
    Test_BrookHTTPCookiesFirst;
    Test_BrookHTTPCookiesLast;
    Test_BrookHTTPCookiesItems;
  finally
    TBrookLibraryLoader.Unload;
  end;
end.
