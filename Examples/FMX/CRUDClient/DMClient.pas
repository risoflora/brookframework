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

unit DMClient;

interface

uses
  System.Classes,
  Data.DB,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  FireDAC.Stan.StorageBin,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TClient = class(TDataModule)
    FDMemTable: TFDMemTable;
    NetHTTPClient: TNetHTTPClient;
    FDStanStorageBinLink: TFDStanStorageBinLink;
  public
    procedure LoadPersons(const AURL: string);
    procedure SavePersons(const AURL: string);
  end;

var
  Client: TClient;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TClient }

procedure TClient.LoadPersons(const AURL: string);
begin
  FDMemTable.Close;
  FDMemTable.LoadFromStream(NetHTTPClient.Get(AURL).ContentStream, sfBinary);
end;

procedure TClient.SavePersons(const AURL: string);
var
  VData: TStream;
begin
  if FDMemTable.State in dsEditModes then
    FDMemTable.Post;
  VData := TBytesStream.Create;
  try
    FDMemTable.SaveToStream(VData, sfBinary);
    VData.Seek(0, TSeekOrigin.soBeginning);
    NetHTTPClient.Post(AURL, VData);
  finally
    VData.Free;
  end;
end;

end.
