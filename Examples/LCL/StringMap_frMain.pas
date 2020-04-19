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

unit StringMap_frMain;

{$MODE DELPHI}
{$WARN 5024 OFF}

interface

uses
  SysUtils,
  StdCtrls,
  ExtCtrls,
  ValEdit,
  Forms,
  BrookLibraryLoader,
  BrookStringMap;

type
  TfrMain = class(TForm)
    BrookLibraryLoader1: TBrookLibraryLoader;
    btAdd: TButton;
    btRemove: TButton;
    btClear: TButton;
    pnTop: TPanel;
    veMap: TValueListEditor;
    procedure btAddClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMap: TBrookStringMap;
    FMapHandle: Pointer;
    procedure DoMapChange(ASender: TObject;
      AOperation: TBrookStringMapOperation);
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

procedure TfrMain.FormCreate(Sender: TObject);
begin
  FMap := TBrookStringMap.Create(@FMapHandle);
  FMap.OnChange := DoMapChange;
end;

procedure TfrMain.FormDestroy(Sender: TObject);
begin
  FMap.Free;
end;

procedure TfrMain.FormShow(Sender: TObject);
begin
  BrookLibraryLoader1.Open;
end;

procedure TfrMain.btAddClick(Sender: TObject);
var
  S: string;
begin
  S := Succ(FMap.Count).ToString;
  FMap.Add(Concat('Name', S), Concat('Value', S));
end;

procedure TfrMain.btRemoveClick(Sender: TObject);
begin
  FMap.Remove(veMap.Keys[veMap.Row]);
end;

procedure TfrMain.btClearClick(Sender: TObject);
begin
  FMap.Clear;
end;

procedure TfrMain.DoMapChange(ASender: TObject;
  AOperation: TBrookStringMapOperation);
var
  P: TBrookStringPair;
  R: Integer;
begin
  R := veMap.Row;
  veMap.Clear;
  for P in FMap do
    veMap.Strings.
{$IFDEF VER3_0}
      Add(Concat(P.Name, veMap.Strings.NameValueSeparator, P.Value))
{$ELSE}
      AddPair(P.Name, P.Value)
{$ENDIF};
  case AOperation of
    sgmoAdd: veMap.Row := Pred(veMap.RowCount);
    sgmoRemove:
      if R > 0 then
        veMap.Row := Pred(R)
      else
        veMap.Row := 0;
  end;
  btRemove.Enabled := FMap.Count > 0;
  btClear.Enabled := btRemove.Enabled;
end;

end.
