(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2019 Silvio Clecio <silvioprog@gmail.com>
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

{$MODE DELPHI}

interface

uses
  DB,
  Classes,
  BufDataset,
  FPHTTPClient;

type
  TClient = class(TDataModule)
    BufDataset: TBufDataset;
  public
    procedure LoadPersons(const AURL: string);
    procedure SavePersons(const AURL: string);
  end;

var
  Client: TClient;

implementation

{$R *.lfm}

procedure TClient.LoadPersons(const AURL: string);
var
  VData: TStream;
begin
  VData := TBytesStream.Create;
  try
    TFPHTTPClient.SimpleGet(AURL, VData);
    VData.Seek(0, TSeekOrigin.soBeginning);
    BufDataset.Close;
    BufDataset.LoadFromStream(VData, dfBinary);
  finally
    VData.Free;
  end;
end;

procedure TClient.SavePersons(const AURL: string);
var
  VClient: TFPHTTPClient;
begin
  if BufDataset.State in dsEditModes then
    BufDataset.Post;
  VClient := TFPHTTPClient.Create(nil);
  VClient.RequestBody := TBytesStream.Create;
  try
    BufDataset.SaveToStream(VClient.RequestBody, dfBinary);
    VClient.RequestBody.Seek(0, TSeekOrigin.soBeginning);
    VClient.Post(AURL);
  finally
    VClient.RequestBody.Free;
    VClient.Free;
  end;
end;

end.
