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

unit frmMain;

{$MODE DELPHI}

interface

uses
  Classes,
  DB,
  StdCtrls,
  ExtCtrls,
  DBGrids,
  DBCtrls,
  Forms,
  DMClient;

const
  URL_SERVER = 'http://localhost:8080';

type
  TfrMain = class(TForm)
    btLoad: TButton;
    btSave: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    pnBottom: TPanel;
    procedure btLoadClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

procedure TfrMain.btLoadClick(Sender: TObject);
begin
  Client.LoadPersons(URL_SERVER);
  btSave.Enabled := DataSource.DataSet.RecordCount > 0;
end;

procedure TfrMain.btSaveClick(Sender: TObject);
begin
  Client.SavePersons(URL_SERVER);
end;

end.
