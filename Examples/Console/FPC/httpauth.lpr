(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  –– microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
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

program httpauth;

{
  Test using cURL:

  curl -u abc:123 http://localhost:<PORT>
}

{$MODE DELPHI}
{$WARN 5024 OFF}

uses
  SysUtils,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  BrookLibraryLoader,
  BrookHTTPAuthentication,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer;

type
  THTTPServer = class(TBrookHTTPServer)
  protected
    function DoAuthenticate(ASender: TObject;
      AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse): Boolean; override;
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  end;

function THTTPServer.DoAuthenticate(ASender: TObject;
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse): Boolean;
begin
  AAuthentication.Credentials.Realm := 'My realm';
  Result := AAuthentication.Credentials.UserName.Equals('abc') and
    AAuthentication.Credentials.Password.Equals('123');
  if not Result then
    AAuthentication.Deny(
      '<html><head><title>Denied</title></head><body><font color="red">Go away</font></body></html>',
      'text/html; charset=utf-8');
end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  AResponse.Send(
    '<html><head><title>Secret</title></head><body><font color="green">Secret page</font></body></html>',
    'text/html; charset=utf-8', 200);
end;

begin
  if not TBrookLibraryLoader.Load(TBrookLibraryLoader.LIB_NAME) then
  begin
    WriteLn(ErrOutput, 'Library not loaded.');
    Halt(1);
  end;
  with THTTPServer.Create(nil) do
  try
    Authenticated := True;
    NoFavicon := True;
    Open;
    if not Active then
      Exit;
    WriteLn('Server running at http://localhost:', Port);
    ReadLn;
  finally
    Free;
  end;
end.
