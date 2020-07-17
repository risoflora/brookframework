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

unit MediaTypes_frMain;

interface

uses
  System.Types,
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  System.Masks,
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
  BrookMediaTypes,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer,
  Utility;

const
  // Allowed file extensions
  ALLOWED_EXTS = '*.txt;*.html;*.css;*.js;*.png;*.jpeg;*.jpg;*.gif;*.pas;*.dfm;*.dpr';

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
    BrookMIME1: TBrookMIME;
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
    procedure FormCreate(Sender: TObject);
  public
    procedure UpdateControls; inline;
  end;

var
  frMain: TfrMain;

implementation

{$R *.fmx}

// https://stackoverflow.com/a/12726969/3268398
function MyGetFiles(const APath, AMasks: string): TStringDynArray;
var
  VMaskArray: TStringDynArray;
  VPredicate: TDirectory.TFilterPredicate;
begin
  VMaskArray := SplitString(AMasks, ';');
  VPredicate :=
    function(const APath: string; const ASearchRec: TSearchRec): Boolean
    var
      VMask: string;
    begin
      for VMask in VMaskArray do
        if MatchesMask(ASearchRec.Name, VMask) then
          Exit(True);
      Exit(False);
    end;
  Result := TDirectory.GetFiles(APath, TSearchOption.soTopDirectoryOnly,
    VPredicate);
end;

procedure TfrMain.FormCreate(Sender: TObject);
begin
  BrookMIME1.FileName := '../../../Common/mime.types';
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
  BrookMIME1.Open;
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
const
  DIR = '../../';
var
  VFileNames: TArray<string>;
  VFileName, VFileLinks, VMediaType: string;
  I: Integer;
begin
  // Try to render or download a single file ...
  if Length(ARequest.Paths) = 1 then
  begin
    VFileName := TPath.GetFullPath(TPath.Combine(DIR, ARequest.Paths[0]));
    if FileExists(VFileName) then
    begin
      VMediaType := BrookMIME1.Types.Find(ExtractFileExt(VFileName));
      if BrookMIME1.Types.IsText(VMediaType) then
        AResponse.Render(VFileName)
      else
        AResponse.Download(VFileName);
      AResponse.Headers['Content-Type'] := VMediaType;
    end
    else
      AResponse.SendFmt(
        '<html><head><title>Error</title></head><body>File not found: %s</body></html>',
        [ExtractFileName(VFileName)], 'text/html; charset=utf-8', 200);
    Exit;
  end;
  // ... otherwise, list all files to the screen.
  VFileNames := MyGetFiles(DIR, ALLOWED_EXTS);
  VFileLinks := '<ul style="list-style: none;">';
  for I := Low(VFileNames) to High(VFileNames) do
  begin
    VFileName := TPath.GetFileName(VFileNames[I]);
    VFileLinks := Concat(VFileLinks, '<li>');
    VFileLinks := Concat(VFileLinks, '<a href="/', VFileName, '">',
      VFileName, '</a>');
    VFileLinks := Concat(VFileLinks, '</li>');
  end;
  VFileLinks := Concat(VFileLinks, '</ul>');
  AResponse.SendFmt('<html><head><title>Media types</title></head><body>%s</body></html>',
    [VFileLinks], 'text/html; charset=utf-8', 200);
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

procedure TfrMain.BrookHTTPServer1Error(ASender: TObject;
  AException: Exception);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      TDialogService.MessageDialog(AException.Message, TMsgDlgType.mtError,
        [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    end);
end;

end.
