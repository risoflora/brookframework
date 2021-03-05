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

program httpuploads;

{$IFDEF MSWINDOWS}
 {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  IOUtils,
  BrookUtility,
  BrookStringMap,
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
      AResponse.Download(Concat(UploadsDir, PathDelim, VFile))
    else
      AResponse.Send(PAGE_FORM, CONTENT_TYPE, 200);
  end;
end;

begin
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
