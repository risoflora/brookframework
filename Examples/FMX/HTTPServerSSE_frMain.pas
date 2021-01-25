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

unit HTTPServerSSE_frMain;

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
  Utility;

type

  { TSSEStream }

  TSSEStream = class(TStream)
  private
    FCount: Cardinal;
  public
    function Read(var ABuffer; ACount: LongInt): LongInt; override;
  end;

  { TfrMain }

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
    procedure BrookHTTPServer1Error(ASender: TObject; AException: Exception);
    procedure BrookHTTPServer1Start(Sender: TObject);
    procedure BrookHTTPServer1Stop(Sender: TObject);
    procedure edPortChange(Sender: TObject);
    procedure edPortChangeTracking(Sender: TObject);
  public
    procedure UpdateControls; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

const
  PAGE_COUNTING = Concat(
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
  PAGE_ERROR = Concat(
    '<!DOCTYPE html>', sLineBreak,
    '<html>', sLineBreak,
    '<head>', sLineBreak,
    '<title>Error</title>', sLineBreak,
    '</head>', sLineBreak,
    '<body>', sLineBreak,
    '<font color="red">%s</font>', sLineBreak,
    '</body>', sLineBreak,
    '</html>'
  );
  HTML_HEADER = 'text/html; charset=utf-8';
  SSE_HEADER = 'text/event-stream';
  IGNORED_ERROR = 'Connection was closed while sending response body.';

var
  frMain: TfrMain;

implementation

{$R *.fmx}

{ TSSEStream }

function TSSEStream.Read(var ABuffer; ACount: LongInt): LongInt;
var
  VMsg: string;
  VBuffer: TBytes;
begin
  if FCount = 0 then
    VMsg := Concat('retry: 1000', sLineBreak)
  else
  begin
    VMsg := Concat('data: ', FCount.ToString, sLineBreak, sLineBreak);
    Sleep(1000);
  end;
  Inc(FCount);
  VBuffer := TEncoding.ANSI.GetBytes(VMsg);
  Result := TEncoding.ANSI.GetCharCount(VBuffer);
  Move(VBuffer[0], ABuffer, Result);
end;

{ TfrMain }

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
  if ARequest.Headers['Accept'] = SSE_HEADER then
  begin
    AResponse.Headers.Add('Access-Control-Allow-Origin', '*');
    AResponse.Headers.Add('Content-Type', SSE_HEADER);
    AResponse.SendStream(TSSEStream.Create, 200);
  end
  else
    AResponse.Send(PAGE_COUNTING, HTML_HEADER, 200);
end;

procedure TfrMain.BrookHTTPServer1RequestError(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
  AException: Exception);
begin
  AResponse.SendFmt(PAGE_ERROR, [AException.Message], HTML_HEADER, 500);
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

procedure TfrMain.BrookHTTPServer1Error(ASender: TObject;
  AException: Exception);
begin
  if AException.Message.TrimRight <> IGNORED_ERROR then
    TThread.Synchronize(nil,
      procedure
      begin
        TDialogService.MessageDialog(AException.Message, TMsgDlgType.mtError,
          [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
      end);
end;

end.
