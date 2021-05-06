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

{$MODE DELPHI}

interface

uses
  SysUtils,
  Classes,
  BufDataset,
  SQLdb,
  SQLite3Conn;

function ListPersons: TStream;
procedure SavePersons(const ABytes: TBytes);

implementation

const
  SQL_SELECT_PERSONS = 'SELECT * FROM persons';
  SQL_UDPATE_PERSONS = 'UPDATE persons SET name = :name WHERE id = :id';

var
  DBConnection: TSQLConnector;

procedure CreateAndConfigureDBConnection;
begin
  DBConnection := TSQLConnector.Create(nil);
  DBConnection.Transaction := TSQLTransaction.Create(DBConnection);
  DBConnection.ConnectorType := 'SQLite3';
  DBConnection.DatabaseName := '../../../DB/DataBase.sqlite3';
end;

procedure DestroyDBConnection;
begin
  FreeAndNil(DBConnection);
end;

function CreateQuery(const ASQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.SQLConnection := DBConnection;
  Result.SQLTransaction := DBConnection.Transaction;
  Result.SQL.Text := ASQL;
end;

function ListPersons: TStream;
var
  VQuery: TSQLQuery;
begin
  Result := TBytesStream.Create;
  VQuery := CreateQuery(SQL_SELECT_PERSONS);
  try
    VQuery.Open;
    VQuery.SaveToStream(Result, dfBinary);
    Result.Seek(0, TSeekOrigin.soBeginning);
  finally
    VQuery.Destroy;
  end;
end;

procedure SavePersons(const ABytes: TBytes);
var
  VQuery: TSQLQuery;
  VData: TBytesStream;
begin
  VQuery := CreateQuery(SQL_SELECT_PERSONS);
  VData := TBytesStream.Create(ABytes);
  try
    VQuery.UpdateSQL.Text := SQL_UDPATE_PERSONS;
    VQuery.Prepare;
    VQuery.LoadFromStream(VData, dfBinary);
    VQuery.ApplyUpdates;
    DBConnection.Transaction.Commit;
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
