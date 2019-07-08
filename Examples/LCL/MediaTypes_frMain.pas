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

{$MODE DELPHI}
{$PUSH}{$WARN 5024 OFF}

interface

uses
  SysUtils,
  Classes,
  FileUtil,
  StdCtrls,
  ActnList,
  Graphics,
  Spin,
  Dialogs,
  Forms,
  LCLIntf,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  BrookLibraryLoader,
  BrookMediaTypes,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer;

const
  // Allowed file extensions
  ALLOWED_EXTS = '*.txt;*.html;*.css;*.js;*.png;*.jpeg;*.jpg;*.gif;*.pas;*.lfm;*.lpr';

type
  TfrMain = class(TForm)
    acStart: TAction;
    acStop: TAction;
    alMain: TActionList;
    BrookHTTPServer1: TBrookHTTPServer;
    BrookLibraryLoader1: TBrookLibraryLoader;
    BrookMIME1: TBrookMIME;
    btStart: TButton;
    btStop: TButton;
    edPort: TSpinEdit;
    lbLink: TLabel;
    lbPort: TLabel;
    procedure acStartExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure BrookHTTPServer1Error(ASender: TObject; AException: Exception);
    procedure BrookHTTPServer1Request(ASender: TObject;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure BrookHTTPServer1RequestError(ASender: TObject;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      AException: Exception);
    procedure BrookHTTPServer1Start(Sender: TObject);
    procedure BrookHTTPServer1Stop(Sender: TObject);
    procedure edPortChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbLinkClick(Sender: TObject);
    procedure lbLinkMouseEnter(Sender: TObject);
    procedure lbLinkMouseLeave(Sender: TObject);
  protected
    procedure DoError(AData: PtrInt);
  public
    procedure UpdateControls; inline;
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

procedure TfrMain.FormCreate(Sender: TObject);
begin
  BrookMIME1.FileName := '../Common/mime.types';
end;

procedure TfrMain.DoError(AData: PtrInt);
var
  S: PString absolute AData;
begin
  try
    MessageDlg(S^, mtError, [mbOK], 0);
  finally
    DisposeStr(S);
  end;
end;

procedure TfrMain.UpdateControls;
begin
  if BrookHTTPServer1.Active then
    edPort.Value := BrookHTTPServer1.Port
  else
    BrookHTTPServer1.Port := edPort.Value;
  lbLink.Caption := Concat('http://localhost:', edPort.Value.ToString);
  acStart.Enabled := not BrookHTTPServer1.Active;
  acStop.Enabled := not acStart.Enabled;
  edPort.Enabled := acStart.Enabled;
  lbLink.Enabled := not acStart.Enabled;
end;

procedure TfrMain.acStartExecute(Sender: TObject);
begin
  BrookLibraryLoader1.Open;
  BrookMIME1.Open;
  BrookHTTPServer1.Open;
end;

procedure TfrMain.acStopExecute(Sender: TObject);
begin
  BrookHTTPServer1.Close;
end;

procedure TfrMain.lbLinkMouseEnter(Sender: TObject);
begin
  lbLink.Font.Style := lbLink.Font.Style + [fsUnderline];
end;

procedure TfrMain.lbLinkMouseLeave(Sender: TObject);
begin
  lbLink.Font.Style := lbLink.Font.Style - [fsUnderline];
end;

procedure TfrMain.lbLinkClick(Sender: TObject);
begin
  OpenURL(lbLink.Caption);
end;

procedure TfrMain.BrookHTTPServer1Request(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  VFileNames: TStringList;
  VFileName, VFileLinks, VMediaType: string;
  I: Integer;
begin
  // Try to render or download a single file ...
  if Length(ARequest.Paths) = 1 then
  begin
    VFileName := ARequest.Paths[0];
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
  VFileNames := FindAllFiles(GetCurrentDir, ALLOWED_EXTS, False);
  try
    VFileNames.Sort;
    VFileLinks := '<ul style="list-style: none;">';
    for I := 0 to Pred(VFileNames.Count) do
    begin
      VFileName := ExtractFileName(VFileNames[I]);
      VFileLinks := Concat(VFileLinks, '<li>');
      VFileLinks := Concat(VFileLinks, '<a href="/', VFileName, '">',
        VFileName, '</a>');
      VFileLinks := Concat(VFileLinks, '</li>');
    end;
    VFileLinks := Concat(VFileLinks, '</ul>');
    AResponse.SendFmt('<html><head><title>Media types</title></head><body>%s</body></html>',
      [VFileLinks], 'text/html; charset=utf-8', 200);
  finally
    VFileNames.Free;
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

{$PUSH}{$WARN 4055 OFF}
procedure TfrMain.BrookHTTPServer1Error(ASender: TObject;
  AException: Exception);
begin
  Application.QueueAsyncCall(DoError, PtrInt(NewStr(AException.Message)));
end;
{$POP}

{$POP}

end.
