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

unit frmMain;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Actions,
  FMX.Types,
  FMX.ActnList,
  FMX.Graphics,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.EditBox,
  FMX.NumberBox,
  FMX.DialogService,
  FMX.Forms,
  FMX.Controls.Presentation,
  BrookHandledClasses,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer,
  Utility,
  DMPersistence;

type
  TfrMain = class(TForm)
    lbPort: TLabel;
    edPort: TNumberBox;
    btStart: TButton;
    btStop: TButton;
    lbLink: TLabel;
    alMain: TActionList;
    acStart: TAction;
    acStop: TAction;
    BrookHTTPServer1: TBrookHTTPServer;
    pnTop: TPanel;
    procedure acStartExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure lbLinkMouseEnter(Sender: TObject);
    procedure lbLinkMouseLeave(Sender: TObject);
    procedure lbLinkClick(Sender: TObject);
    procedure BrookHTTPServer1Request(ASender: TObject;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure BrookHTTPServer1RequestError(ASender: TObject;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      AException: Exception);
    procedure BrookHTTPServer1Start(Sender: TObject);
    procedure BrookHTTPServer1Stop(Sender: TObject);
    procedure edPortChange(Sender: TObject);
    procedure edPortChangeTracking(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateControls; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

var
  frMain: TfrMain;

implementation

{$R *.fmx}

procedure TfrMain.UpdateControls;
begin
  if Application.Terminated then
    Exit;
  if BrookHTTPServer1.Active then
    edPort.Value := BrookHTTPServer1.Port
  else
    BrookHTTPServer1.Port := edPort.Text.ToInteger;
  lbLink.Text := Concat('http://localhost:', edPort.Value.ToString);
  acStart.Enabled := not BrookHTTPServer1.Active;
  acStop.Enabled := not acStart.Enabled;
  edPort.Enabled := acStart.Enabled;
  lbLink.Enabled := not acStart.Enabled;
end;

procedure TfrMain.acStartExecute(Sender: TObject);
begin
  BrookHTTPServer1.Open;
end;

procedure TfrMain.acStopExecute(Sender: TObject);
begin
  BrookHTTPServer1.Close;
end;

procedure TfrMain.lbLinkMouseEnter(Sender: TObject);
begin
  lbLink.Font.Style := lbLink.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TfrMain.lbLinkMouseLeave(Sender: TObject);
begin
  lbLink.Font.Style := lbLink.Font.Style - [TFontStyle.fsUnderline];
end;

procedure TfrMain.lbLinkClick(Sender: TObject);
begin
  Utility.OpenURL(lbLink.Text);
end;

procedure TfrMain.BrookHTTPServer1Request(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if ARequest.Payload.Length > 0 then
    Persistence.SavePersons(ARequest.Payload.Content)
  else
    AResponse.SendStream(Persistence.ListPersons, 200);
end;

procedure TfrMain.BrookHTTPServer1RequestError(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
  AException: Exception);
begin
  AResponse.SendFmt(
    '<html><head><title>Error</title></head><body><font color="red">%s</font></body></html>',
    [AException.Message], 'text/html; charset=utf-8', 500);
end;

procedure TfrMain.BrookHTTPServer1Start(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrMain.BrookHTTPServer1Stop(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrMain.edPortChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrMain.edPortChangeTracking(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrMain.FormShow(Sender: TObject);
begin
  edPort.Value := 8080;
  acStart.Execute;
end;

end.
