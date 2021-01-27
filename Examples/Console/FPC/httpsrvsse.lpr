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

program httpsrvsse;

{$MODE DELPHI}

uses
  SysUtils,
  Classes,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer;

const
  PAGE = Concat(
    '<!DOCTYPE html>', sLineBreak,
    '<html>', sLineBreak,
    '<head>', sLineBreak,
    '<title>SSE example</title>', sLineBreak,
    '</head><body><h2 id="counter">Please wait ...</h2>', sLineBreak,
    '<script>', sLineBreak,
    'const es = new EventSource("/");', sLineBreak,
    'es.onmessage = function (ev) {', sLineBreak,
    '  document.getElementById("counter").innerText = "Counting: " + ev.data;', sLineBreak,
    '};', sLineBreak,
    '</script>', sLineBreak,
    '</body>', sLineBreak,
    '</html>'
  );
  SSE_HEADER = 'text/event-stream';
  IGNORED_ERROR = 'Connection was closed while sending response body.';

type

  { TSSEStream }

  TSSEStream = class(TStream)
  private
    FCount: Cardinal;
  public
    function Read(var ABuffer; ACount: LongInt): LongInt; override;
  end;

  { THTTPServer }

  THTTPServer = class(TBrookHTTPServer)
  protected
    procedure DoError(ASender: TObject; AException: Exception); override;
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  end;

{ TSSEStream }

function TSSEStream.Read(var ABuffer; ACount: LongInt): LongInt;
var
  VMsg: string;
begin
  if FCount = 0 then
    VMsg := Concat('retry: 1000', sLineBreak)
  else
  begin
    VMsg := Concat('data: ', FCount.ToString, sLineBreak, sLineBreak);
    Sleep(1000);
  end;
  Inc(FCount);
  Result := Length(VMsg);
  Move(VMsg[1], ABuffer, Result);
end;

{ THTTPServer }

procedure THTTPServer.DoError(ASender: TObject; AException: Exception);
begin
  if AException.Message.TrimRight <> IGNORED_ERROR then
    inherited DoError(ASender, AException);
end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  if ARequest.Headers['Accept'] = SSE_HEADER then
  begin
    AResponse.Headers.Add('Access-Control-Allow-Origin', '*');
    AResponse.Headers.Add('Content-Type', SSE_HEADER);
    AResponse.SendStream(TSSEStream.Create, 200);
  end
  else
    AResponse.Send(PAGE, 'text/html; charset=utf-8', 200);
end;

begin
  with THTTPServer.Create(nil) do
  try
    NoFavicon := True;
    Threaded := True;
    Open;
    if not Active then
      Exit;
    WriteLn('Server running at http://localhost:', Port);
    ReadLn;
  finally
    Free;
  end;
end.
