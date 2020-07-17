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

program urlrouter;

{$MODE DELPHI}
{$WARN 5024 OFF}

uses
  SysUtils,
  Classes,
  BrookUtility,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer,
  BrookURLRouter;

type

  { TRouteHome }

  TRouteHome = class(TBrookURLRoute)
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;

  { TRouteDownload }

  TRouteDownload = class(TBrookURLRoute)
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;

  { TRoutePage }

  TRoutePage = class(TBrookURLRoute)
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;

  { TRouter }

  TRouter = class(TBrookURLRouter)
  protected
    procedure DoNotFound(ASender: TObject; const ARoute: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  end;

  { TServer }

  TServer = class(TBrookHTTPServer)
  private
    FRouter: TRouter;
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TRouteHome }

procedure TRouteHome.AfterConstruction;
begin
  Methods := [rmGET];
  Pattern := '/home';
  Default := True;
end;

procedure TRouteHome.DoRequest(ASender: TObject; ARoute: TBrookURLRoute;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('Home page', 'text/plain', 200);
end;

{ TRouteDownload }

procedure TRouteDownload.AfterConstruction;
begin
  Methods := [rmGET];
  Pattern := '/download/(?P<file>[a-z]+)';
end;

procedure TRouteDownload.DoRequest(ASender: TObject; ARoute: TBrookURLRoute;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.SendFmt('Downloaded file: %s',
    [ARoute.Variables['file']], 'text/plain', 200);
end;

{ TRoutePage }

procedure TRoutePage.AfterConstruction;
begin
  Methods := [rmGET];
  Pattern := '/page/([0-9]+)';
end;

procedure TRoutePage.DoRequest(ASender: TObject; ARoute: TBrookURLRoute;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.SendFmt('Page number: %d', [ARoute.Segments[0].ToInteger],
    'text/plain', 200);
end;

{ TRouter }

procedure TRouter.DoNotFound(ASender: TObject; const ARoute: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('Page not found', 'text/plain', 404);
end;

{ TServer }

constructor TServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRouter := TRouter.Create(Self);
  TRouteHome.Create(FRouter.Routes);
  TRouteDownload.Create(FRouter.Routes);
  TRoutePage.Create(FRouter.Routes);
  FRouter.Active := True;
end;

procedure TServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  FRouter.Route(ASender, ARequest, AResponse);
end;

begin
  with TServer.Create(nil) do
  try
    Open;
    if not Active then
      Exit;
    WriteLn('Server running at http://localhost:', Port);
    ReadLn;
  finally
    Free;
  end;
end.
