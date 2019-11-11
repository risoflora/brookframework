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

program mediatypes;

{$IFDEF MSWINDOWS}
 {$APPTYPE CONSOLE}
{$ENDIF}

uses
  System.Types,
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  System.Masks,
  System.Classes,
  BrookLibraryLoader,
  BrookMediaTypes,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer;

const
  // Allowed file extensions
  ALLOWED_EXTS = '*.txt;*.html;*.css;*.js;*.png;*.jpeg;*.jpg;*.gif;*.pas;*.dfm;*.dpr';

type
  THTTPServer = class(TBrookHTTPServer)
  private
    FMIME: TBrookMIME;
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

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

constructor THTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMIME := TBrookMIME.Create(Self);
  FMIME.FileName := '../../Common/mime.types';
end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
const
  DIR = './';
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
      VMediaType := FMIME.Types.Find(ExtractFileExt(VFileName));
      if FMIME.Types.IsText(VMediaType) then
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

begin
  TBrookLibraryLoader.Load;
  with THTTPServer.Create(nil) do
  try
    FMIME.Open;
    Open;
    if not Active then
      Exit;
    WriteLn('Server running at http://localhost:', Port);
    ReadLn;
  finally
    Free;
  end;
end.
