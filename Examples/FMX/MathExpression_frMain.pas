(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2020 Silvio Clecio <silvioprog@gmail.com>
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

unit MathExpression_frMain;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Math,
  System.Classes,
  System.Rtti,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Grid.Style,
  FMX.Grid,
  FMX.ScrollBox,
  FMX.Dialogs,
  FMX.Forms,
  BrookHandledClasses,
  BrookMathExpression;

type
  TfrMain = class(TForm)
    lbExpression: TLabel;
    edExpression: TEdit;
    lbVariables: TLabel;
    gdVariables: TStringGrid;
    clName: TStringColumn;
    clValue: TStringColumn;
    btCalculate: TButton;
    BrookMathExpression1: TBrookMathExpression;
    procedure btCalculateClick(Sender: TObject);
    function BrookMathExpression1Extension(ASender: TObject;
      AExtension: TBrookMathExpressionExtension): Double;
    procedure BrookMathExpression1Error(ASender: TObject;
      AError: TBrookMathExpressionError);
    procedure FormCreate(Sender: TObject);
  end;

var
  frMain: TfrMain;

implementation

{$R *.fmx}

procedure TfrMain.FormCreate(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := '.';
  edExpression.Text := Concat(Double(1.2).ToString, ' + ', Double(3.4).ToString,
    ' + mysum(foo, bar) / mymult(foo, bar)');
  BrookMathExpression1.Extensions.AddStrings(['mysum', 'mymult']);
  gdVariables.Cells[0, 0] := 'foo';
  gdVariables.Cells[0, 1] := 'bar';
  gdVariables.Cells[1, 0] := Double(1.2).ToString;
  gdVariables.Cells[1, 1] := Double(3.4).ToString;
end;

procedure TfrMain.BrookMathExpression1Error(ASender: TObject;
  AError: TBrookMathExpressionError);
begin
  ActiveControl := edExpression;
  edExpression.SelStart := Pred(AError.Near);
  ShowMessageFmt('Error: %s', [AError.Message]);
end;

function TfrMain.BrookMathExpression1Extension(ASender: TObject;
  AExtension: TBrookMathExpressionExtension): Double;
begin
  if AExtension.HasArgs then
    case IndexText(AExtension.Ident, ['mysum', 'mymult']) of
      0: Exit(AExtension.Args[0] + AExtension.Args[1]);
      1: Exit(AExtension.Args[0] * AExtension.Args[1]);
    end;
  Result := NaN;
end;

procedure TfrMain.btCalculateClick(Sender: TObject);
var
  I: Integer;
  N, V: string;
begin
  if not BrookMathExpression1.Active then
    BrookMathExpression1.Open;
  BrookMathExpression1.Clear;
  for I := 0 to Pred(gdVariables.RowCount) do
  begin
    N := gdVariables.Cells[0, I];
    V := gdVariables.Cells[1, I];
    if (not N.IsEmpty) and (not V.IsEmpty) then
      BrookMathExpression1.SetVariable(N, V.ToDouble);
  end;
  if BrookMathExpression1.Compile(edExpression.Text) then
    ShowMessageFmt('Result: %f', [BrookMathExpression1.Evaluate]);
end;

end.
