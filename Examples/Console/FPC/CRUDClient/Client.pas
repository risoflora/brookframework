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

unit Client;

{$MODE DELPHI}

interface

uses
  SysUtils,
  Classes,
  DB,
  BufDataset,
  FPHTTPClient;

function ListPersons(const AURL: string): TDataSet;
procedure SavePersons(const AURL: string; ADataSet: TDataSet);
function CreatePersonsDataSet: TDataSet;

implementation

function ListPersons(const AURL: string): TDataSet;
var
  VData: TStream;
begin
  Result := TBufDataset.Create(nil);
  VData := TBytesStream.Create;
  try
    TFPHTTPClient.SimpleGet(AURL, VData);
    TBufDataset(Result).LoadFromStream(VData, dfBinary);
  finally
    VData.Free;
  end;
end;

procedure SavePersons(const AURL: string; ADataSet: TDataSet);
var
  VClient: TFPHTTPClient;
begin
  if ADataSet.State in dsEditModes then
    ADataSet.Post;
  try
    VClient := TFPHTTPClient.Create(nil);
    VClient.RequestBody := TBytesStream.Create;
    try
      TBufDataset(ADataSet).SaveToStream(VClient.RequestBody, dfBinary);
      VClient.RequestBody.Seek(0, TSeekOrigin.soBeginning);
      VClient.Post(AURL);
    finally
      VClient.RequestBody.Free;
      VClient.Free;
    end;
  finally
    FreeAndNil(ADataSet);
  end;
end;

function CreatePersonsDataSet: TDataSet;
begin
  Result := TBufDataset.Create(nil);
  Result.FieldDefs.Add('name', ftString, 100);
  TBufDataset(Result).CreateDataSet;
  Result.Open;
end;

end.
