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

program Test_HTTPUploads;

{$I Tests.inc}

uses
  SysUtils,
  StrUtils,
  Platform,
  Marshalling,
  libsagui,
  BrookLibraryLoader,
  BrookUtility,
  BrookHTTPUploads,
  Test;

type
  TFakeUploads = class(TBrookHTTPUploads)
  public
    Done: Boolean;
  end;

  TFakeUpload = class
  public
    ErrorCode: Integer;
    Path: string;
  end;

var
  FakeHandle: Pointer;
  FakeUploads: TFakeUploads;
  FakeUpload: TFakeUpload;
  HTTPUploadsEnumeratorDone: Boolean = False;

function fake_httpupld_handle(uplds: Psg_httpupld): Pcvoid; cdecl;
begin
  Result := uplds;
end;

function fake_httpupld_dir(uplds: Psg_httpupld): Pcchar; cdecl;
begin
  Assert(uplds = FakeHandle);
  Result := 'fake_dir';
end;

function fake_httpupld_field(uplds: Psg_httpupld): Pcchar; cdecl;
begin
  Assert(uplds = FakeHandle);
  Result := 'fake_field';
end;

function fake_httpupld_name(uplds: Psg_httpupld): Pcchar; cdecl;
begin
  Assert(uplds = FakeHandle);
  Result := 'fake_name';
end;

function fake_httpupld_mime(uplds: Psg_httpupld): Pcchar; cdecl;
begin
  Assert(uplds = FakeHandle);
  Result := 'fake_mime';
end;

function fake_httpupld_encoding(uplds: Psg_httpupld): Pcchar; cdecl;
begin
  Assert(uplds = FakeHandle);
  Result := 'fake_encoding';
end;

function fake_httpupld_size(uplds: Psg_httpupld): cuint64_t; cdecl;
begin
  Assert(uplds = FakeHandle);
  Result := 123;
end;

function fake_httpupld_save(upld: Psg_httpupld; overwritten: cbool): cint; cdecl;
begin
  Assert(upld = FakeHandle);
  Assert(overwritten);
  Result := TFakeUpload(upld).ErrorCode;
end;

function fake_httpupld_save_as(upld: Psg_httpupld; const path: Pcchar;
  overwritten: cbool): cint; cdecl;
var
  U: TFakeUpload;
begin
  U := TFakeUpload(upld);
  Assert(upld = FakeHandle);
  U.Path := TMarshal.ToString(path);
  Assert(overwritten);
  Result := U.ErrorCode;
end;

function fake_httpuplds_count(uplds: Psg_httpupld): cuint; cdecl;
begin
  Assert(uplds = FakeHandle);
  Result := 3;
end;

function fake_httpuplds_next(upld: PPsg_httpupld): cint; cdecl;
begin
  Assert(Assigned(upld));
  if HTTPUploadsEnumeratorDone then
    upld^ := nil;
  Result := 0;
end;

procedure AssignFakeAPI; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  sg_httpupld_handle := fake_httpupld_handle;
  sg_httpupld_dir := fake_httpupld_dir;
  sg_httpupld_field := fake_httpupld_field;
  sg_httpupld_name := fake_httpupld_name;
  sg_httpupld_mime := fake_httpupld_mime;
  sg_httpupld_encoding := fake_httpupld_encoding;
  sg_httpupld_size := fake_httpupld_size;
  sg_httpupld_save := fake_httpupld_save;
  sg_httpupld_save_as := fake_httpupld_save_as;

  sg_httpuplds_count := fake_httpuplds_count;
  sg_httpuplds_next := fake_httpuplds_next;
end;

procedure DoHTTPCreateLibraryNotLoaded;
begin
  TBrookHTTPUpload.Create(FakeUpload);
end;

procedure Test_HTTPUploadCreate;
var
  U: TBrookHTTPUpload;
begin
  FakeHandle := FakeUploads;
  U := TBrookHTTPUpload.Create(FakeUploads);
  Assert(U.Handle = FakeUploads);
  Assert(U.Directory = 'fake_dir');
  Assert(U.Field = 'fake_field');
  Assert(U.Name = 'fake_name');
  Assert(U.Mime = 'fake_mime');
  Assert(U.Encoding = 'fake_encoding');
  Assert(U.Size = 123);
  TBrookLibraryLoader.Unload;
  try
    AssertExcept(DoHTTPCreateLibraryNotLoaded, ESgLibNotLoaded,
      Format(SSgLibNotLoaded, [IfThen(SgLib.GetLastName = '',
        SG_LIB_NAME, SgLib.GetLastName)]));
  finally
    TBrookLibraryLoader.Load;
    AssignFakeAPI;
  end;
end;

procedure DoHTTPSaveInvalidArgument1(const AArgs: array of const);
begin
  TBrookHTTPUpload(AArgs[0].VPointer^).Save(True);
end;

procedure DoHTTPSaveInvalidArgument2(const AArgs: array of const);
begin
  TBrookHTTPUpload(AArgs[0].VPointer^).Save;
end;

procedure Test_HTTPUploadSave;
var
  U: TBrookHTTPUpload;
  E: string;
begin
  FakeHandle := FakeUpload;
  U := TBrookHTTPUpload.Create(FakeUpload);

  FakeUpload.ErrorCode := 0;
  E := '';
  Assert(U.Save(True, E));
  Assert(E = '');
  FakeUpload.ErrorCode := EINVAL;
  E := '';
  Assert(not U.Save(True, E));
  Assert(E = Sagui.StrError(EINVAL));

  FakeUpload.ErrorCode := 0;
  E := '';
  Assert(U.Save(E));
  Assert(E = '');
  FakeUpload.ErrorCode := EINVAL;
  Assert(not U.Save(E));
  Assert(E = Sagui.StrError(EINVAL));

  FakeUpload.ErrorCode := 0;
  E := '';
  U.Save(True);
  Assert(E = '');
  FakeUpload.ErrorCode := EINVAL;
  E := '';
  AssertOSExcept(DoHTTPSaveInvalidArgument1, EINVAL, [@U]);

  FakeUpload.ErrorCode := 0;
  E := '';
  U.Save;
  Assert(E = '');
  FakeUpload.ErrorCode := EINVAL;
  E := '';
  AssertOSExcept(DoHTTPSaveInvalidArgument2, EINVAL, [@U]);
end;

procedure DoHTTPSaveAsInvalidArgument1(const AArgs: array of const);
begin
  TBrookHTTPUpload(AArgs[0].VPointer^).SaveAs('fake_path', True);
end;

procedure DoHTTPSaveAsInvalidArgument2(const AArgs: array of const);
begin
  TBrookHTTPUpload(AArgs[0].VPointer^).SaveAs('fake_path');
end;

procedure Test_HTTPUploadSaveAs;
var
  U: TBrookHTTPUpload;
  E: string;
begin
  FakeHandle := FakeUpload;
  U := TBrookHTTPUpload.Create(FakeUpload);

  FakeUpload.ErrorCode := 0;
  FakeUpload.Path := '';
  E := '';
  Assert(U.SaveAs('fake_path', True, E));
  Assert(FakeUpload.Path = 'fake_path');
  Assert(E = '');
  FakeUpload.ErrorCode := EINVAL;
  FakeUpload.Path := '';
  E := '';
  Assert(not U.SaveAs('fake_path', True, E));
  Assert(FakeUpload.Path = 'fake_path');
  Assert(E = Sagui.StrError(EINVAL));

  FakeUpload.ErrorCode := 0;
  FakeUpload.Path := '';
  E := '';
  Assert(U.SaveAs('fake_path', E));
  Assert(FakeUpload.Path = 'fake_path');
  Assert(E = '');
  FakeUpload.ErrorCode := EINVAL;
  FakeUpload.Path := '';
  E := '';
  Assert(not U.SaveAs('fake_path', E));
  Assert(FakeUpload.Path = 'fake_path');
  Assert(E = Sagui.StrError(EINVAL));

  FakeUpload.ErrorCode := 0;
  FakeUpload.Path := '';
  E := '';
  U.SaveAs('fake_path', True);
  Assert(FakeUpload.Path = 'fake_path');
  Assert(E = '');
  FakeUpload.ErrorCode := EINVAL;
  FakeUpload.Path := '';
  E := '';
  AssertOSExcept(DoHTTPSaveAsInvalidArgument1, EINVAL, [@U]);

  FakeUpload.ErrorCode := 0;
  FakeUpload.Path := '';
  E := '';
  U.SaveAs('fake_path');
  Assert(FakeUpload.Path = 'fake_path');
  Assert(E = '');
  FakeUpload.ErrorCode := EINVAL;
  FakeUpload.Path := '';
  E := '';
  AssertOSExcept(DoHTTPSaveAsInvalidArgument2, EINVAL, [@U]);
end;

procedure Test_HTTPUploadHandle;
begin
  Assert(TBrookHTTPUpload.Create(FakeUpload).Handle = FakeUpload);
end;

procedure Test_HTTPUploadStreamHandle;
begin
  Assert(TBrookHTTPUpload.Create(FakeUpload).StreamHandle = FakeUpload);
end;

procedure Test_HTTPUploadDirectory;
begin
  Assert(TBrookHTTPUpload.Create(FakeUpload).Directory = 'fake_dir');
end;

procedure Test_HTTPUploadField;
begin
  Assert(TBrookHTTPUpload.Create(FakeUpload).Field = 'fake_field');
end;

procedure Test_HTTPUploadName;
begin
  Assert(TBrookHTTPUpload.Create(FakeUpload).Name = 'fake_name');
end;

procedure Test_HTTPUploadMime;
begin
  Assert(TBrookHTTPUpload.Create(FakeUpload).Mime = 'fake_mime');
end;

procedure Test_HTTPUploadEncoding;
begin
  Assert(TBrookHTTPUpload.Create(FakeUpload).Encoding = 'fake_encoding');
end;

procedure Test_HTTPUploadSize;
begin
  Assert(TBrookHTTPUpload.Create(FakeUpload).Size = 123);
end;

procedure Test_HTTPUploadsEnumerator;
var
  UL: TFakeUploads;
  U: TBrookHTTPUpload;
  I: Integer;
begin
  FakeHandle := FakeUploads;
  UL := TFakeUploads.Create(FakeUploads);
  try
    I := 0;
    for U in UL do
    begin
      if I = 3 then
        HTTPUploadsEnumeratorDone := True
      else
        Inc(I);
    end;
    Assert(I = 3);
  finally
    UL.Free;
  end;
end;

procedure Test_HTTPUploadsCreate;
var
  UL: TBrookHTTPUploads;
begin
  UL := TBrookHTTPUploads.Create(FakeUploads);
  try
    Assert(UL.Handle = FakeUploads);
  finally
    UL.Free;
  end;
end;

procedure Test_HTTPUploadsGetEnumerator;
var
  UL: TBrookHTTPUploads;
  E: TBrookHTTPUploadsEnumerator;
begin
  UL := TBrookHTTPUploads.Create(FakeUploads);
  try
    E := UL.GetEnumerator;
    Assert(Assigned(E) and (E is TBrookHTTPUploadsEnumerator));
    E.Destroy;
  finally
    UL.Free;
  end;
end;

procedure Test_HTTPUploadsFirst;
var
  UL: TBrookHTTPUploads;
  U: TBrookHTTPUpload;
begin
  UL := TBrookHTTPUploads.Create(FakeUploads);
  try
    U := UL.First;
    Assert(U.Handle = FakeUploads);
  finally
    UL.Free;
  end;
end;

procedure Test_HTTPUploadsNext;
var
  UL: TBrookHTTPUploads;
  U: TBrookHTTPUpload;
begin
  UL := TBrookHTTPUploads.Create(FakeUploads);
  try
    U := UL.Next;
    Assert(U.Handle = FakeUploads);
  finally
    UL.Free;
  end;
end;

procedure Test_HTTPUploadsEOF;
var
  UL: TBrookHTTPUploads;
begin
  UL := TBrookHTTPUploads.Create(FakeUploads);
  try
    Assert(UL.EOF);
  finally
    UL.Free;
  end;
end;

procedure Test_HTTPUploadsCount;
var
  UL: TBrookHTTPUploads;
begin
  UL := TBrookHTTPUploads.Create(FakeUploads);
  try
    Assert(UL.Count = 3);
  finally
    UL.Free;
  end;
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  FakeUploads := TFakeUploads.Create(nil);
  FakeUpload := TFakeUpload.Create;
  try
    AssignFakeAPI;
    Test_HTTPUploadCreate;
    Test_HTTPUploadSave;
    Test_HTTPUploadSaveAs;
    Test_HTTPUploadHandle;
    Test_HTTPUploadStreamHandle;
    Test_HTTPUploadDirectory;
    Test_HTTPUploadField;
    Test_HTTPUploadName;
    Test_HTTPUploadMime;
    Test_HTTPUploadEncoding;
    Test_HTTPUploadSize;
    Test_HTTPUploadsEnumerator;
    Test_HTTPUploadsCreate;
    Test_HTTPUploadsGetEnumerator;
    Test_HTTPUploadsFirst;
    Test_HTTPUploadsNext;
    Test_HTTPUploadsEOF;
    Test_HTTPUploadsCount;
  finally
    FakeUploads.Free;
    FakeUpload.Free;
    TBrookLibraryLoader.Unload;
  end;
end.
