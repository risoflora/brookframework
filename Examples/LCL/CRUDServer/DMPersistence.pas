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

unit DMPersistence;

{$MODE DELPHI}

interface

uses
  SysUtils,
  Classes,
  SQLDB,
  BufDataset,
  SQLite3Conn;

type
  TPersistence = class(TDataModule)
    SQLConnector: TSQLConnector;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
  public
    function ListPersons: TStream;
    procedure SavePersons(const ABytes: TBytes);
  end;

var
  Persistence: TPersistence;

implementation

{$R *.lfm}

function TPersistence.ListPersons: TStream;
begin
  Result := TBytesStream.Create;
  SQLQuery.Close;
  SQLQuery.Open;
  SQLQuery.SaveToStream(Result, dfBinary);
  Result.Seek(0, TSeekOrigin.soBeginning);
end;

procedure TPersistence.SavePersons(const ABytes: TBytes);
var
  VData: TBytesStream;
begin
  VData := TBytesStream.Create(ABytes);
  try
    SQLQuery.Close;
    SQLQuery.Prepare;
    SQLQuery.LoadFromStream(VData, dfBinary);
    SQLQuery.ApplyUpdates;
    SQLTransaction.Commit;
  finally
    VData.Free;
  end;
end;

end.
