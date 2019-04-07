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

program httpuploads;

{$IFDEF MSWINDOWS}
 {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  IOUtils,
  BrookLibraryLoader,
  BrookUtility,
  BrookHTTPUploads,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer;

const
  PAGE_FORM = Concat(
    '<html>',
    '<body>',
    '<form action="" method="post" enctype="multipart/form-data">',
    '<fieldset>',
    '<legend>Choose the files:</legend>',
    'File 1: <input type="file" name="file1"/><br>',
    'File 2: <input type="file" name="file2"/><br>',
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
    '</body>',
    '</html>'
  );
  CONTENT_TYPE = 'text/html; charset=utf-8';

type
  THTTPServer = class(TBrookHTTPServer)
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
var
  VUpload: TBrookHTTPUpload;
  VFile, VList, VError: string;
begin
  if ARequest.IsUploading then
  begin
    VList := '<ol>';
    for VUpload in ARequest.Uploads do
      if VUpload.Save(False, VError) then
        VList := Concat(VList, '<li><a href="?file=', VUpload.Name, '">',
          VUpload.Name, '</a></li>')
      else
        VList := Concat(VList, '<li><font color="red">', VUpload.Name,
          ' - failed - ', VError, '</font></li>');
    VList := Concat(VList, '</ol>');
    AResponse.Send(PAGE_DONE, [VList], CONTENT_TYPE, 200);
  end
  else
  begin
    if ARequest.Params.TryValue('file', VFile) then
      AResponse.Download(Concat(UploadsDir, PathDelim, VFile))
    else
      AResponse.Send(PAGE_FORM, CONTENT_TYPE, 200);
  end;
end;

begin
  if not TBrookLibraryLoader.Load then
  begin
    WriteLn(ErrOutput, 'Library not loaded.');
    Halt(1);
  end;
  with THTTPServer.Create(nil) do
  try
    UploadsDir := TPath.Combine(Sagui.TmpDir, 'uploads');
    NoFavicon := True;
    Open;
    if not Active then
      Exit;
    ForceDirectories(UploadsDir);
    WriteLn('Server running at http://localhost:', Port);
    ReadLn;
  finally
    Free;
  end;
end.
