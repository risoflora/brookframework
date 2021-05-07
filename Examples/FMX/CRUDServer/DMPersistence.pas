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

interface

uses
  System.SysUtils,
  System.Classes,
  Data.DB,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.FMXUI.Wait,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Stan.StorageBin;

type
  TPersistence = class(TDataModule)
    FDConnection: TFDConnection;
    FDQuery: TFDQuery;
    FDUpdateSQL: TFDUpdateSQL;
    FDStanStorageBinLink: TFDStanStorageBinLink;
  public
    function ListPersons: TStream;
    procedure SavePersons(const ABytes: TBytes);
  end;

var
  Persistence: TPersistence;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TPersistence }

function TPersistence.ListPersons: TStream;
begin
  Result := TBytesStream.Create;
  FDQuery.Close;
  FDQuery.Open;
  FDQuery.SaveToStream(Result, sfBinary);
  Result.Seek(0, TSeekOrigin.soBeginning);
end;

procedure TPersistence.SavePersons(const ABytes: TBytes);
var
  VData: TBytesStream;
begin
  VData := TBytesStream.Create(ABytes);
  try
    FDQuery.LoadFromStream(VData, sfBinary);
    FDQuery.ApplyUpdates;
    FDConnection.Commit;
  finally
    VData.Free;
  end;
end;

end.
