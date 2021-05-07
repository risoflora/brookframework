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

unit Persistence;

interface

uses
  System.SysUtils,
  System.Classes,
  FireDAC.DApt,
  FireDAC.Stan.Def,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Async,
  FireDAC.Phys.SQLite,
  FireDAC.Comp.Client,
  FireDAC.Stan.StorageBin;

function ListPersons: TStream;
procedure SavePersons(const ABytes: TBytes);

implementation

const
  SQL_SELECT_PERSONS = 'SELECT * FROM persons';
  SQL_INSERT_PERSONS = 'INSERT INTO persons (name) VALUES (:name)';

var
  DBConnection: TFDConnection;

procedure CreateAndConfigureDBConnection;
begin
  DBConnection := TFDConnection.Create(nil);
  DBConnection.DriverName := 'SQLite';
  DBConnection.Params.Database := '../../../DB/DataBase.sqlite3';
end;

procedure DestroyDBConnection;
begin
  FreeAndNil(DBConnection);
end;

function CreateQuery(const ASelectSQL: string;
  const AInsertSQL: string = ''): TFDQuery;
var
  VUpdate: TFDUpdateSQL;
begin
  Result := TFDQuery.Create(nil);
  if not AInsertSQL.IsEmpty then
  begin
    VUpdate := TFDUpdateSQL.Create(Result);
    VUpdate.Connection := DBConnection;
    VUpdate.InsertSQL.Text := AInsertSQL;
    Result.UpdateObject := VUpdate;
  end;
  Result.Connection := DBConnection;
  Result.CachedUpdates := True;
  Result.SQL.Text := ASelectSQL;
end;

function ListPersons: TStream;
var
  VQuery: TFDQuery;
begin
  Result := TBytesStream.Create;
  VQuery := CreateQuery(SQL_SELECT_PERSONS);
  try
    VQuery.Open;
    VQuery.SaveToStream(Result, sfBinary);
    Result.Seek(0, TSeekOrigin.soBeginning);
  finally
    VQuery.Destroy;
  end;
end;

procedure SavePersons(const ABytes: TBytes);
var
  VQuery: TFDQuery;
  VData: TBytesStream;
begin
  VQuery := CreateQuery(SQL_SELECT_PERSONS, SQL_INSERT_PERSONS);
  VData := TBytesStream.Create(ABytes);
  try
    VQuery.LoadFromStream(VData, sfBinary);
    VQuery.ApplyUpdates;
  finally
    VQuery.Destroy;
    VData.Free;
  end;
end;

initialization
  CreateAndConfigureDBConnection;

finalization
  DestroyDBConnection;

end.
