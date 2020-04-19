(*  _                     _
 * | |__  _ __ ___   ___ | | __
 * | '_ \| '__/ _ \ / _ \| |/ /
 * | |_) | | | (_) | (_) |   <
 * |_.__/|_|  \___/ \___/|_|\_\
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

{ Contains classes for upload handling. }

unit BrookHTTPUploads;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  Marshalling,
  libsagui,
  BrookUtility,
  BrookHandledClasses;

type
  TBrookHTTPUploads = class;

  { Structured type which holds the upload properties and allows to save the
    uploaded file. }
  TBrookHTTPUpload = record
  private
    FHandle: Psg_httpupld;
    FStreamHandle: Pointer;
    FDirectory: string;
    FField: string;
    FName: string;
    FMime: string;
    FEncoding: string;
    FSize: UInt64;
    function GetHandle: Pointer;
  public
    { Creates an instance of @code(TBrookHTTPUpload).
      @param(AHandle[in] Upload handle.) }
    constructor Create(AHandle: Pointer);
    { Saves the uploaded file defining the destination path by upload name and
      directory.
      @param(AOverwritten[in] Overwrite upload file if it exists.)
      @param(AError[out] Variable reference to store string describing the error
      if save fails.)
      @returns(@True if the uploaded file is saved successfully.) }
    function Save(AOverwritten: Boolean; out AError: string): Boolean; overload;
    { Saves the uploaded file defining the destination path by upload name and
      directory.
      @param(AError[out] Variable reference to store string describing the error
      if save fails.)
      @returns(@True if the uploaded file is saved successfully.) }
    function Save(out AError: string): Boolean; overload;
    { Saves the uploaded file defining the destination path by upload name and
      directory.
      @param(AOverwritten[in] Overwrite upload file if it exists.) }
    procedure Save(AOverwritten: Boolean); overload;
    { Saves the uploaded file defining the destination path by upload name and
      directory. }
    procedure Save; overload;
    { Saves the uploaded file allowing to define the destination path.
      @param(APath[in] Absolute destination path.)
      @param(AOverwritten[in] Overwrite upload file if it exists.)
      @param(AError[out] Variable reference to store string describing the error
      if save fails.)
      @returns(@True if the uploaded file is saved successfully.) }
    function SaveAs(const APath: TFileName; AOverwritten: Boolean;
      out AError: string): Boolean; overload;
    { Saves the uploaded file allowing to define the destination path.
      @param(APath[in] Absolute destination path.)
      @param(AError[out] Variable reference to store string describing the error
      if save fails.)
      @returns(@True if the uploaded file is saved successfully.) }
    function SaveAs(const APath: TFileName; out AError: string): Boolean; overload;
    { Saves the uploaded file allowing to define the destination path.
      @param(APath[in] Absolute destination path.)
      @param(AOverwritten[in] Overwrite upload file if it exists.) }
    procedure SaveAs(const APath: TFileName; AOverwritten: Boolean); overload;
    { Saves the uploaded file allowing to define the destination path.
      @param(APath[in] Absolute destination path.) }
    procedure SaveAs(const APath: TFileName); overload;
    { Handle of an upload. }
    property Handle: Pointer read GetHandle;
    { Stream handle of the upload. }
    property StreamHandle: Pointer read FStreamHandle;
    { Directory of the uploaded file. }
    property Directory: string read FDirectory;
    { Field name of the upload. }
    property Field: string read FField;
    { Name of the uploaded file. }
    property Name: string read FName;
    { MIME (content-type) of the upload. }
    property Mime: string read FMime;
    { Encoding (transfer-encoding) of the upload. }
    property Encoding: string read FEncoding;
    { Size of the upload. }
    property Size: UInt64 read FSize;
  end;

  { Enumerator used to iterate the map @code(TBrookHTTPUpload). }
  TBrookHTTPUploadsEnumerator = class
  private
    FUploads: TBrookHTTPUploads;
    FCurr: TBrookHTTPUpload;
    FBOF: Boolean;
  public
    { Creates an instance of @code(TBrookHTTPUploadsEnumerator).
      @param(AUploads[in] Uploads list.) }
    constructor Create(AUploads: TBrookHTTPUploads);
    { Gets the current upload.
      @returns(Current upload.) }
    function GetCurrent: TBrookHTTPUpload;
    { Moves to the next upload.
      @returns(@True when move next reaches the EOF.) }
    function MoveNext: Boolean;
    { Same to @code(GetCurrent). }
    property Current: TBrookHTTPUpload read GetCurrent;
  end;

  { Uploads list which contains all uploaded files and form fields. }
  TBrookHTTPUploads = class(TBrookHandledPersistent)
  private
    FHandle: Psg_httpupld;
    FCurrent: Psg_httpupld;
    function GetCount: Integer;
    function GetCurrent: Pointer;
  protected
    function GetHandle: Pointer; override;
    function IsEOF: Boolean; virtual;
    property Current: Pointer read GetCurrent;
  public
    { Creates an instance of @code(TBrookHTTPUploads).
      @param(AHandle[in] Uploads handle.) }
    constructor Create(AHandle: Pointer); virtual;
    { Gets an instance of @code(TBrookHTTPUploadsEnumerator). }
    function GetEnumerator: TBrookHTTPUploadsEnumerator;
    { Retrieves the first upload in the list.
      @param(AUpload[out] First upload returned.)
      @returns(@True when upload is found, @False otherwise.) }
    procedure First(out AUpload: TBrookHTTPUpload); virtual;
    { Retrieves the next upload in the list.
      @param(AUpload[out] Next upload returned.) }
    procedure Next(out AUpload: TBrookHTTPUpload); virtual;
    { Indicates the end of list. }
    property EOF: Boolean read IsEOF; //FI:C110
    { Counts the total uploads present in the list. }
    property Count: Integer read GetCount;
  end;

implementation

{ TBrookHTTPUploadsEnumerator }

constructor TBrookHTTPUploadsEnumerator.Create(AUploads: TBrookHTTPUploads);
begin
  inherited Create;
  FUploads := AUploads;
  FUploads.First(FCurr);
  FBOF := True;
end;

function TBrookHTTPUploadsEnumerator.GetCurrent: TBrookHTTPUpload;
begin
  Result := FCurr;
end;

function TBrookHTTPUploadsEnumerator.MoveNext: Boolean;
begin
  if FBOF then
    FBOF := False
  else
    FUploads.Next(FCurr);
  Result := not FUploads.EOF;
end;

{ TBrookHTTPUpload }

constructor TBrookHTTPUpload.Create(AHandle: Pointer);
begin
  FHandle := AHandle;
  SgLib.Check;
  FStreamHandle := sg_httpupld_handle(FHandle);
  FDirectory := TMarshal.ToString(sg_httpupld_dir(FHandle));
  FField := TMarshal.ToString(sg_httpupld_field(FHandle));
  FName := TMarshal.ToString(sg_httpupld_name(FHandle));
  FMime := TMarshal.ToString(sg_httpupld_mime(FHandle));
  FEncoding := TMarshal.ToString(sg_httpupld_encoding(FHandle));
  FSize := sg_httpupld_size(FHandle);
end;

function TBrookHTTPUpload.Save(AOverwritten: Boolean;
  out AError: string): Boolean;
var
  R: cint;
begin
  SgLib.Check;
  R := sg_httpupld_save(FHandle, AOverwritten);
  Result := R = 0;
  if not Result then
    AError := Sagui.StrError(R);
end;

function TBrookHTTPUpload.Save(out AError: string): Boolean;
begin
  Result := Save(True, AError);
end;

procedure TBrookHTTPUpload.Save(AOverwritten: Boolean);
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpupld_save(FHandle, AOverwritten));
end;

procedure TBrookHTTPUpload.Save;
begin
  Save(True);
end;

function TBrookHTTPUpload.SaveAs(const APath: TFileName; AOverwritten: Boolean;
  out AError: string): Boolean;
var
  R: cint;
  M: TMarshaller;
begin
  SgLib.Check;
  R := sg_httpupld_save_as(FHandle, M.ToCString(APath), AOverwritten);
  Result := R = 0;
  if not Result then
    AError := Sagui.StrError(R);
end;

function TBrookHTTPUpload.SaveAs(const APath: TFileName;
  out AError: string): Boolean;
begin
  Result := SaveAs(APath, True, AError);
end;

procedure TBrookHTTPUpload.SaveAs(const APath: TFileName; AOverwritten: Boolean);
var
  M: TMarshaller;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpupld_save_as(FHandle, M.ToCString(APath),
    AOverwritten));
end;

procedure TBrookHTTPUpload.SaveAs(const APath: TFileName);
begin
  SaveAs(APath, True);
end;

function TBrookHTTPUpload.GetHandle: Pointer;
begin
  Result := FHandle;
end;

{ TBrookHTTPUploads }

constructor TBrookHTTPUploads.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TBrookHTTPUploads.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookHTTPUploads.IsEOF: Boolean;
begin
  Result := not Assigned(FCurrent);
end;

function TBrookHTTPUploads.GetEnumerator: TBrookHTTPUploadsEnumerator;
begin
  Result := TBrookHTTPUploadsEnumerator.Create(Self);
end;

procedure TBrookHTTPUploads.First(out AUpload: TBrookHTTPUpload);
begin
  FCurrent := FHandle;
  AUpload := TBrookHTTPUpload.Create(FCurrent);
end;

procedure TBrookHTTPUploads.Next(out AUpload: TBrookHTTPUpload);
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpuplds_next(@FCurrent));
  if Assigned(FCurrent) then
    AUpload := TBrookHTTPUpload.Create(FCurrent);
end;

function TBrookHTTPUploads.GetCount: Integer;
begin
  SgLib.Check;
  Result := sg_httpuplds_count(FHandle);
end;

function TBrookHTTPUploads.GetCurrent: Pointer;
begin
  Result := FCurrent;
end;

end.
