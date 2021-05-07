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

interface

uses
  System.SysUtils,
  System.Classes,
  Data.DB,
  System.Net.HTTPClient,
  FireDAC.Stan.Intf,
  FireDAC.Comp.Client,
  FireDAC.Stan.StorageBin;

function ListPersons(const AURL: string): TDataSet;
procedure SavePersons(const AURL: string; ADataSet: TDataSet);
function CreatePersonsDataSet: TDataSet;

implementation

function CreateDataSet: TFDMemTable;
begin
  Result := TFDMemTable.Create(nil);
  Result.CachedUpdates := True;
end;

function ListPersons(const AURL: string): TDataSet;
var
  VClient: THTTPClient;
begin
  Result := CreateDataSet;
  VClient := THTTPClient.Create;
  try
    TFDMemTable(Result).LoadFromStream(VClient.Get(AURL).ContentStream, sfBinary);
  finally
    VClient.Free;
  end;
end;

procedure SavePersons(const AURL: string; ADataSet: TDataSet);
var
  VClient: THTTPClient;
  VData: TStream;
begin
  if ADataSet.State in dsEditModes then
    ADataSet.Post;
  try
    VData := TBytesStream.Create;
    VClient := THTTPClient.Create;
    try
      TFDMemTable(ADataSet).SaveToStream(VData, sfBinary);
      VData.Seek(0, TSeekOrigin.soBeginning);
      VClient.Post(AURL, VData);
    finally
      VClient.Free;
      VData.Free;
    end;
  finally
    FreeAndNil(ADataSet);
  end;
end;

function CreatePersonsDataSet: TDataSet;
begin
  Result := CreateDataSet;
  Result.FieldDefs.Add('name', ftString, 100);
  TFDMemTable(Result).CreateDataSet;
end;

end.
