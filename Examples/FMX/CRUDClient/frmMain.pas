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

unit frmMain;

interface

uses
  System.Classes,
  System.Rtti,
  System.Bindings.Outputs,
  Data.DB,
  Data.Bind.Controls,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.Grid,
  Data.Bind.DBScope,
  FMX.Types,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Grid,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Forms,
  FMX.Layouts,
  Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid,
  Fmx.Bind.Editors,
  Fmx.Bind.Navigator,
  DMClient;

const
  URL_SERVER = 'http://localhost:8080';

type
  TfrMain = class(TForm)
    pnBottom: TPanel;
    btLoad: TButton;
    btSave: TButton;
    DataSource: TDataSource;
    Grid1: TGrid;
    BindSourceDB: TBindSourceDB;
    BindingsList: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    BindNavigator1: TBindNavigator;
    procedure btLoadClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
  end;

var
  frMain: TfrMain;

implementation

{$R *.fmx}

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


