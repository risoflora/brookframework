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

program httpcookies;

{$IFDEF MSWINDOWS}
 {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  BrookLibraryLoader,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer;

const
  CONTENT_TYPE = 'text/html; charset=utf-8';
  INITIAL_PAGE = '<html><head><title>Cookies</title></head><body>Use F5 to refresh this page ...</body></html>';
  COUNT_PAGE = '<html><head><title>Cookies</title></head><body>Refresh number: %d</body></html>';
  COOKIE_NAME = 'refresh_count';

type
  THTTPServer = class(TBrookHTTPServer)
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
var
  VCount: Integer;
begin
  if ARequest.Cookies.IsEmpty then
    VCount := 0
  else
    VCount := StrToIntDef(ARequest.Cookies.Get(COOKIE_NAME), 0);
  if VCount = 0  then
  begin
    AResponse.Send(INITIAL_PAGE, CONTENT_TYPE, 200);
    VCount := 1;
  end
  else
  begin
    AResponse.SendFmt(COUNT_PAGE, [VCount], CONTENT_TYPE, 200);
    Inc(VCount);
  end;
  AResponse.SetCookie(COOKIE_NAME, VCount.ToString);
end;

begin
  if not TBrookLibraryLoader.Load then
  begin
    WriteLn(ErrOutput, 'Library not loaded.');
    Halt(1);
  end;
  with THTTPServer.Create(nil) do
  try
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
