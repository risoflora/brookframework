(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  –– microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
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
    constructor Create(AHandle: Pointer);
    function Save(AOverwritten: Boolean; out AError: string): Boolean; overload;
    function Save(out AError: string): Boolean; overload;
    procedure Save(AOverwritten: Boolean); overload;
    procedure Save; overload;
    function SaveAs(const APath: TFileName; AOverwritten: Boolean;
      out AError: string): Boolean; overload;
    function SaveAs(const APath: TFileName; out AError: string): Boolean; overload;
    procedure SaveAs(const APath: TFileName; AOverwritten: Boolean); overload;
    procedure SaveAs(const APath: TFileName); overload;
    property Handle: Pointer read GetHandle;
    property StreamHandle: Pointer read FStreamHandle;
    property Directory: string read FDirectory;
    property Field: string read FField;
    property Name: string read FName;
    property Mime: string read FMime;
    property Encoding: string read FEncoding;
    property Size: UInt64 read FSize;
  end;

  TBrookHTTPUploadsEnumerator = class
  private
    FUploads: TBrookHTTPUploads;
    FCurr: TBrookHTTPUpload;
    FBOF: Boolean;
  public
    constructor Create(AUploads: TBrookHTTPUploads);
    function GetCurrent: TBrookHTTPUpload;
    function MoveNext: Boolean;
    property Current: TBrookHTTPUpload read GetCurrent;
  end;

  TBrookHTTPUploads = class(TBrookHandledPersistent)
  private
    FHandle: Psg_httpupld;
    FCurr: Psg_httpupld;
    function GetCount: Integer;
    function GetCurr: Pointer;
  protected
    property Curr: Pointer read GetCurr;
  public
    constructor Create(AHandle: Pointer); virtual;
    function GetHandle: Pointer; override;
    function GetEnumerator: TBrookHTTPUploadsEnumerator;
    { TODO: Iterate::sg_httpuplds_iter() }
    procedure First(out AUpload: TBrookHTTPUpload); virtual;
    procedure Next(out AUpload: TBrookHTTPUpload); virtual;
    function IsEOF: Boolean; virtual;
    property EOF: Boolean read IsEOF; {FI:C110}
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

function TBrookHTTPUploads.GetEnumerator: TBrookHTTPUploadsEnumerator;
begin
  Result := TBrookHTTPUploadsEnumerator.Create(Self);
end;

procedure TBrookHTTPUploads.First(out AUpload: TBrookHTTPUpload);
begin
  FCurr := FHandle;
  AUpload := TBrookHTTPUpload.Create(FCurr);
end;

procedure TBrookHTTPUploads.Next(out AUpload: TBrookHTTPUpload);
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpuplds_next(@FCurr));
  if Assigned(FCurr) then
    AUpload := TBrookHTTPUpload.Create(FCurr);
end;

function TBrookHTTPUploads.IsEOF: Boolean;
begin
  Result := not Assigned(FCurr);
end;

function TBrookHTTPUploads.GetCount: Integer;
begin
  SgLib.Check;
  Result := sg_httpuplds_count(FHandle);
end;

function TBrookHTTPUploads.GetCurr: Pointer;
begin
  Result := FCurr;
end;

end.
