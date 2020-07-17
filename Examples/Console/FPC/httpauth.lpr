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

program httpauth;

{
  Test using cURL:

  curl -u abc:123 http://localhost:<PORT>
}

{$MODE DELPHI}
{$WARN 5024 OFF}

uses
  SysUtils,
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
