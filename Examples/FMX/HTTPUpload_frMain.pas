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

unit HTTPUpload_frMain;

interface

uses
  System.SysUtils,
{$IFDEF ANDROID}
  System.IOUtils,
{$ENDIF}
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
  BrookUtility,
  BrookStringMap,
  BrookHandledClasses,
  BrookHTTPUploads,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer,
  Utility;

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

const
  PAGE_FORM = Concat(
    '<html>',
    '<body>',
    '<form action="" method="post" enctype="multipart/form-data">',
    '<fieldset>',
    '<legend>Choose the files:</legend>',
    'File 1: <input type="file" name="file1"/><br>',
    'File 2: <input type="file" name="file2"/><br>',
    'User 1: <input type="text" name="user1"/><br>',
    'User 2: <input type="text" name="user2"/><br>',
    '<input type="submit"/>',
    '</fieldset>',
    '</form>',
    '</body>',
    '</html>'
  );
  PAGE_DONE = Concat(
    '<html>',
    '<head>',
    '<title>Uploads</title>',
    '</head>',
    '<body>',
    '<strong>Uploaded files:</strong><br>',
    '%s',
    '<strong>Users:</strong><br>',
    '%s',
    '</body>',
    '</html>'
  );
  CONTENT_TYPE = 'text/html; charset=utf-8';

var
  frMain: TfrMain;

implementation

{$R *.fmx}

procedure TfrMain.FormShow(Sender: TObject);
begin
  if BrookHTTPServer1.UploadsDir.IsEmpty then
    BrookHTTPServer1.UploadsDir :=
{$IFDEF ANDROID}TPath.GetTempPath{$ELSE}Sagui.TmpDir{$ENDIF};
end;

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
var
  VUpload: TBrookHTTPUpload;
  VField: TBrookStringPair;
  VFile, VFileList, VUsersList, VError: string;
begin
  if ARequest.IsUploading then
  begin
    VFileList := '<ol>';
    for VUpload in ARequest.Uploads do
      if VUpload.Save(False, VError) then
        VFileList := Concat(VFileList, '<li><a href="?file=', VUpload.Name, '">',
          VUpload.Name, '</a></li>')
      else
        VFileList := Concat(VFileList, '<li><font color="red">', VUpload.Name,
          ' - failed - ', VError, '</font></li>');
    VFileList := Concat(VFileList, '</ol>');
    VUsersList := '<ol>';
    for VField in ARequest.Fields do
      VUsersList := Concat(VUsersList, '<li>', VField.Value, '</li>');
    VUsersList := Concat(VUsersList, '</ol>');
    AResponse.SendFmt(PAGE_DONE, [VFileList, VUsersList], CONTENT_TYPE, 200);
  end
  else
  begin
    if ARequest.Params.TryValue('file', VFile) then
      AResponse.Download(
        Concat(BrookHTTPServer1.UploadsDir, PathDelim, VFile))
    else
      AResponse.Send(PAGE_FORM, CONTENT_TYPE, 200);
  end;
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

end.
