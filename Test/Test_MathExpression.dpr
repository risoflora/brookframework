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

program Test_MathExpression;

{$I Tests.inc}

uses
  SysUtils,
  StrUtils,
  Math,
  Classes,
  libsagui,
  Marshalling,
  Platform,
  BrookUtility,
  BrookLibraryLoader,
  BrookMathExpression,
  Test;

var
  FakeHandle: Pointer = Pointer(1);
  FakeComponentHandle: TComponent;
  FakeErrType: sg_expr_err_type;
  FakeFlag: Boolean;

type
  TFakeMathExpression = class(TBrookMathExpression)
  private
    FFakeError: TBrookMathExpressionError;
  public
    constructor Create(AOwner: TComponent); override;
    procedure FakeOnError(Sender: TObject; AError: TBrookMathExpressionError);
    function FakeOnExtension(Sender: TObject;
      AExtension: TBrookMathExpressionExtension): Double;
    procedure FakeOnActivate(Sender: TObject);
    procedure FakeOnDeactivate(Sender: TObject);
    property FakeError: TBrookMathExpressionError read FFakeError;
  end;

procedure TFakeMathExpression.FakeOnError(Sender: TObject;
  AError: TBrookMathExpressionError);
begin
  FFakeError := AError;
end;

constructor TFakeMathExpression.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnExtension := FakeOnExtension;
  OnError := FakeOnError;
  OnActivate := FakeOnActivate;
  OnDeactivate := FakeOnDeactivate;
  FFakeError := Default(TBrookMathExpressionError);
end;

{$IFDEF FPC}
 {$PUSH}{$WARN 6058 OFF}
{$ENDIF}

function TFakeMathExpression.FakeOnExtension(Sender: TObject;
  AExtension: TBrookMathExpressionExtension): Double;
begin
  if AExtension.HasArgs then
    case IndexText(AExtension.Ident, ['mysum', 'mymult']) of
      0: Exit(AExtension.Args[0] + AExtension.Args[1]);
      1: Exit(AExtension.Args[0] * AExtension.Args[1]);
    end;
  Result := NaN;
end;

{$IFDEF FPC}
 {$POP}
{$ENDIF}

procedure TFakeMathExpression.FakeOnActivate(Sender: TObject);
begin
  FakeFlag := True;
end;

procedure TFakeMathExpression.FakeOnDeactivate(Sender: TObject);
begin
  FakeFlag := True;
end;

function fake_expr_near(expr: Psg_expr): cint; cdecl;
begin
  Assert(expr = FakeHandle);
  Result := 123;
end;

function fake_expr_err(expr: Psg_expr): sg_expr_err_type; cdecl;
begin
  Assert(expr = FakeHandle);
  Result := FakeErrType;
end;

function fake_expr_strerror(expr: Psg_expr): Pcchar; cdecl;
var
  M: TMarshaller;
begin
  Assert(expr = FakeHandle);
  Result := M.ToCString('abc123');
end;

function fake_expr_arg(args: Psg_expr_argument; index: cint): cdouble; cdecl;
begin
  Assert(args = FakeHandle);
  Assert(index = 123);
  Result := 12.34;
end;

procedure Test_MathExpressionErrorCreate;
var
  E: TBrookMathExpressionError;
begin
  E := TBrookMathExpressionError.Create(FakeHandle);
  Assert(E.Handle = FakeHandle);
end;

procedure DoMathExpressionErrorNearLibNotLoaded;
begin
  TBrookMathExpressionError.Create(FakeHandle).Near;
end;

procedure Test_MathExpressionErrorNear;
var
  E: TBrookMathExpressionError;
begin
  sg_expr_near := fake_expr_near;
  E := TBrookMathExpressionError.Create(FakeHandle);
  Assert(E.Near = 123);

  TBrookLibraryLoader.Unload;
  try
    AssertExcept(DoMathExpressionErrorNearLibNotLoaded, ESgLibNotLoaded,
      Format(SSgLibNotLoaded, [SG_LIB_NAME]));
  finally
    TBrookLibraryLoader.Load;
  end;
end;

procedure DoMathExpressionErrorKindLibNotLoaded;
begin
  TBrookMathExpressionError.Create(FakeHandle).Kind;
end;

procedure Test_MathExpressionErrorKind;
var
  E: TBrookMathExpressionError;
begin
  sg_expr_err := fake_expr_err;
  E := TBrookMathExpressionError.Create(FakeHandle);
  FakeErrType := -1;
  Assert(E.Kind = ekNone);
  FakeErrType := SG_EXPR_ERR_UNKNOWN;
  Assert(E.Kind = ekUnknown);
  FakeErrType := SG_EXPR_ERR_UNEXPECTED_NUMBER;
  Assert(E.Kind = ekUnexpectedNumber);
  FakeErrType := SG_EXPR_ERR_UNEXPECTED_WORD;
  Assert(E.Kind = ekUnexpectedWord);
  FakeErrType := SG_EXPR_ERR_UNEXPECTED_PARENS;
  Assert(E.Kind = ekUnexpectedParens);
  FakeErrType := SG_EXPR_ERR_MISSING_OPERAND;
  Assert(E.Kind = ekMissingOperand);
  FakeErrType := SG_EXPR_ERR_UNKNOWN_OPERATOR;
  Assert(E.Kind = ekUnknownOperator);
  FakeErrType := SG_EXPR_ERR_INVALID_FUNC_NAME;
  Assert(E.Kind = ekInvalidFuncName);
  FakeErrType := SG_EXPR_ERR_BAD_PARENS;
  Assert(E.Kind = ekBadParens);
  FakeErrType := SG_EXPR_ERR_TOO_FEW_FUNC_ARGS;
  Assert(E.Kind = ekTooFewFuncArgs);
  FakeErrType := SG_EXPR_ERR_FIRST_ARG_IS_NOT_VAR;
  Assert(E.Kind = ekFirstArgIsNotVar);
  FakeErrType := SG_EXPR_ERR_BAD_VARIABLE_NAME;
  Assert(E.Kind = ekBadVariableName);
  FakeErrType := SG_EXPR_ERR_BAD_ASSIGNMENT;
  Assert(E.Kind = ekBadAssignment);

  TBrookLibraryLoader.Unload;
  try
    AssertExcept(DoMathExpressionErrorKindLibNotLoaded, ESgLibNotLoaded,
      Format(SSgLibNotLoaded, [SG_LIB_NAME]));
  finally
    TBrookLibraryLoader.Load;
  end;
end;

procedure DoMathExpressionErrorMessageLibNotLoaded;
begin
  TBrookMathExpressionError.Create(FakeHandle).Message;
end;

procedure Test_MathExpressionErrorMessage;
var
  E: TBrookMathExpressionError;
begin
  sg_expr_strerror := fake_expr_strerror;
  E := TBrookMathExpressionError.Create(FakeHandle);
  Assert(E.Message = 'abc123');

  TBrookLibraryLoader.Unload;
  try
    AssertExcept(DoMathExpressionErrorMessageLibNotLoaded, ESgLibNotLoaded,
      Format(SSgLibNotLoaded, [SG_LIB_NAME]));
  finally
    TBrookLibraryLoader.Load;
  end;
end;

procedure Test_MathExpressionErrorHandle;
var
  E1, E2: TBrookMathExpressionError;
begin
  E1 := TBrookMathExpressionError.Create(nil);
  E2 := TBrookMathExpressionError.Create(FakeHandle);
  Assert(not Assigned(E1.Handle));
  Assert(Assigned(E2.Handle));
  Assert(E2.Handle = FakeHandle);
end;

procedure Test_MathExpressionExtensionCreate;
var
  E1, E2: TBrookMathExpressionExtension;
begin
  E1 := TBrookMathExpressionExtension.Create(nil, '');
  E2 := TBrookMathExpressionExtension.Create(FakeHandle, 'abc123');
  Assert(E1.Ident = '');
  Assert(not Assigned(E1.Handle));
  Assert(E2.Ident = 'abc123');
  Assert(Assigned(E2.Handle));
end;

procedure Test_MathExpressionExtensionHasArgs;
var
  E1, E2: TBrookMathExpressionExtension;
begin
  E1 := TBrookMathExpressionExtension.Create(nil, '');
  E2 := TBrookMathExpressionExtension.Create(FakeHandle, 'abc123');
  Assert(not E1.HasArgs);
  Assert(E2.HasArgs);
end;

procedure DoMathExpressionErrorArgsLibNotLoaded;
begin
  TBrookMathExpressionExtension.Create(FakeHandle, 'abc123').Args[0];
end;

procedure Test_MathExpressionExtensionArgs;
var
  E1, E2: TBrookMathExpressionExtension;
begin
  E1 := TBrookMathExpressionExtension.Create(nil, '');
  E2 := TBrookMathExpressionExtension.Create(FakeHandle, 'abc123');
  sg_expr_arg := fake_expr_arg;
  Assert(E1.Args[0].ToString = NaN.ToString);
  Assert(E1[0].ToString = NaN.ToString);
  Assert(E2.Args[123].ToString = Double.ToString(12.34));
  Assert(E2[123].ToString = Double.ToString(12.34));

  TBrookLibraryLoader.Unload;
  try
    AssertExcept(DoMathExpressionErrorArgsLibNotLoaded, ESgLibNotLoaded,
      Format(SSgLibNotLoaded, [SG_LIB_NAME]));
  finally
    TBrookLibraryLoader.Load;
  end;
end;

procedure Test_MathExpressionExtensionIdent;
var
  E1, E2: TBrookMathExpressionExtension;
begin
  E1 := TBrookMathExpressionExtension.Create(nil, '');
  E2 := TBrookMathExpressionExtension.Create(FakeHandle, 'abc123');
  Assert(E1.Ident = '');
  Assert(not Assigned(E1.Handle));
  Assert(E2.Ident = 'abc123');
end;

procedure Test_MathExpressionExtensionHandle;
var
  E1, E2: TBrookMathExpressionExtension;
begin
  E1 := TBrookMathExpressionExtension.Create(nil, '');
  E2 := TBrookMathExpressionExtension.Create(FakeHandle, 'abc123');
  Assert(not Assigned(E1.Handle));
  Assert(Assigned(E2.Handle));
  Assert(E2.Handle = FakeHandle);
end;

procedure Test_MathExpressionCreate;
var
  E1, E2: TBrookMathExpression;
begin
  FakeComponentHandle := TComponent.Create(nil);
  E1 := TBrookMathExpression.Create(nil);
  E2 := TBrookMathExpression.Create(FakeComponentHandle);
  try
    Assert(not Assigned(E1.Owner));
    Assert(Assigned(E2.Owner));
    Assert(E2.Owner = FakeComponentHandle);
    Assert(Assigned(E1.Extensions));

    E1.Active := True;
    Assert(E1.Active);
    TBrookLibraryLoader.Unload;
    try
      Assert(not E1.Active);
    finally
      TBrookLibraryLoader.Load;
    end;
  finally
    E1.Destroy;
    E2.Destroy;
    FakeComponentHandle.Free;
  end;
end;

procedure Test_MathExpressionOpen;
var
  E: TBrookMathExpression;
begin
  E := TBrookMathExpression.Create(nil);
  try
    Assert(not E.Active);
    E.Open;
    Assert(E.Active);
  finally
    E.Free;
  end;
end;

procedure Test_MathExpressionClose;
var
  E: TBrookMathExpression;
begin
  E := TBrookMathExpression.Create(nil);
  try
    Assert(not E.Active);
    E.Open;
    Assert(E.Active);
    E.Close;
    Assert(not E.Active);
  finally
    E.Free;
  end;
end;

procedure DoMathExpressionCompileInactiveMathExpression(
  const AArgs: array of const);
var
  E: TBrookMathExpressionError;
begin
  TBrookMathExpression(AArgs[0].VObject).Compile('1+2', E);
end;

procedure Test_MathExpressionCompile;
var
  M1: TBrookMathExpression;
  M2: TFakeMathExpression;
  E: TBrookMathExpressionError;
begin
  M1 := TBrookMathExpression.Create(nil);
  try
    Assert(not M1.Compiled);
    M1.Open;
    Assert(M1.Compile('1+2', E));
    Assert(M1.Compile('(1+2', E));
    M1.Clear;
    Assert(not M1.Compile('(1+2', E));
    Assert(E.Near = 4);
    Assert(E.Message.Trim = 'Bad parenthesis.');
    Assert(E.Kind = ekBadParens);
    Assert(not M1.Compile('1++2', E));
    Assert(E.Near = 2);
    Assert(E.Message.Trim = 'Missing expected operand.');
    Assert(E.Kind = ekMissingOperand);

    M1.Close;
    AssertExcept(DoMathExpressionCompileInactiveMathExpression,
      EInvalidOpException, SBrookInactiveMathExpression, [M1]);

    M1.Open;
    TBrookLibraryLoader.Unload;
    try
      AssertExcept(DoMathExpressionCompileInactiveMathExpression,
        EInvalidOpException, SBrookInactiveMathExpression, [M1]);
    finally
      TBrookLibraryLoader.Load;
    end;
  finally
    M1.Free;
  end;

  M2 := TFakeMathExpression.Create(nil);
  try
    M2.Open;
    Assert(not M2.Compile('(1+2'));
    Assert(M2.FakeError.Near = 4);
    Assert(M2.FakeError.Message.Trim = 'Bad parenthesis.');
    Assert(M2.FakeError.Kind = ekBadParens);
    Assert(not M2.Compile('1++2'));
    Assert(M2.FakeError.Near = 2);
    Assert(M2.FakeError.Message.Trim = 'Missing expected operand.');
    Assert(M2.FakeError.Kind = ekMissingOperand);
  finally
    M2.Free;
  end;
end;

procedure Test_MathExpressionClear;
var
  M: TBrookMathExpression;
begin
  M := TBrookMathExpression.Create(nil);
  try
    M.Open;
    M.Compile('1+2');
    Assert(M.Compiled);
    M.Clear;
    Assert(not M.Compiled);
    M.Clear;
    M.Clear;
  finally
    M.Free;
  end;
end;

procedure Test_MathExpressionEvaluate;
var
  M: TBrookMathExpression;
begin
  M := TBrookMathExpression.Create(nil);
  try
    M.Open;
    Assert(not M.Compiled);
    M.Expression := '1.2+3.4';
    Assert(M.Evaluate.ToString = Double.ToString(4.6));
    Assert(M.Compiled);
    Assert(M.Evaluate.ToString = Double.ToString(4.6));
  finally
    M.Free;
  end;
end;

procedure DoMathExpressionGetVariableInactiveMathExpression(
  const AArgs: array of const);
begin
  TBrookMathExpression(AArgs[0].VObject).GetVariable('foo');
end;

procedure Test_MathExpressionGetVariable;
var
  M: TBrookMathExpression;
begin
  M := TBrookMathExpression.Create(nil);
  try
    M.Open;
    Assert(M.GetVariable('foo').ToString = Double.ToString(0));
    M.SetVariable('foo', 12.34);
    Assert(M.GetVariable('foo').ToString = Double.ToString(12.34));
    M.Expression := 'foo=56.78';
    M.Evaluate;
    Assert(M.GetVariable('foo').ToString = Double.ToString(56.78));

    M.Close;
    AssertExcept(DoMathExpressionGetVariableInactiveMathExpression,
      EInvalidOpException, SBrookInactiveMathExpression, [M]);
  finally
    M.Free;
  end;
end;

procedure DoMathExpressionSetVariableInactiveMathExpression(
  const AArgs: array of const);
begin
  TBrookMathExpression(AArgs[0].VObject).SetVariable('foo', 12.34);
end;

procedure Test_MathExpressionSetVariable;
var
  M: TBrookMathExpression;
begin
  M := TBrookMathExpression.Create(nil);
  try
    M.Open;
    M.Expression := 'foo+56.78';
    Assert(M.GetVariable('foo').ToString = Double.ToString(0));
    M.SetVariable('foo', 12.34);
    Assert(M.GetVariable('foo').ToString = Double.ToString(12.34));
    Assert(M.Evaluate.ToString = Double.ToString(69.12));

    M.Close;
    AssertExcept(DoMathExpressionSetVariableInactiveMathExpression,
      EInvalidOpException, SBrookInactiveMathExpression, [M]);
  finally
    M.Free;
  end;
end;

procedure Test_MathExpressionCompiled;
var
  M: TBrookMathExpression;
begin
  M := TBrookMathExpression.Create(nil);
  try
    M.Open;
    Assert(not M.Compiled);
    M.Compile('1+2');
    Assert(M.Compiled);

    M.Close;
    Assert(not M.Compiled);
  finally
    M.Free;
  end;
end;

procedure Test_MathExpressionVariables;
var
  M: TBrookMathExpression;
begin
  M := TBrookMathExpression.Create(nil);
  try
    M.Open;
    M.Expression := 'foo+bar';
    M.Variables['foo'] := 12.34;
    M['bar'] := 56.78;
    Assert(M.Evaluate.ToString = Double.ToString(69.12));
  finally
    M.Free;
  end;
end;

procedure Test_MathExpressionActive;
var
  M: TBrookMathExpression;
begin
  M := TBrookMathExpression.Create(nil);
  try
    Assert(not M.Active);
    M.Active := not M.Active;
    Assert(M.Active);
    M.Active := True;
    Assert(M.Active);
  finally
    M.Free;
  end;
end;

procedure Test_MathExpressionExpression;
var
  M: TBrookMathExpression;
begin
  M := TBrookMathExpression.Create(nil);
  try
    M.Open;
    Assert(M.Expression.IsEmpty);
    M.Expression := '1.2+3.4';
    Assert(M.Expression = '1.2+3.4');
    Assert(M.Evaluate.ToString = Double.ToString(4.6));
    Assert(M.Compiled);
    M.Expression := '1+2';
    Assert(not M.Compiled);
  finally
    M.Free;
  end;
end;

procedure Test_MathExpressionExtensions;
var
  M: TFakeMathExpression;
begin
  M := TFakeMathExpression.Create(nil);
  try
    M.Open;
    Assert(Assigned(M.Extensions));
    M.Expression := 'mysum(1.2, 3.4) + mymult(5.6, 7.8)';
    Assert(M.Evaluate.ToString = NaN.ToString);
    M.Extensions.AddStrings(['mysum', 'mymult']);
    Assert(M.Evaluate.ToString = Double.ToString(48.28));
  finally
    M.Free;
  end;
end;

procedure Test_MathExpressionOnExtension;
begin
  Test_MathExpressionExtensions;
end;

procedure Test_MathExpressionOnError;
var
  M: TFakeMathExpression;
begin
  M := TFakeMathExpression.Create(nil);
  try
    M.Open;
    Assert(not M.Compile('(1+2'));
    Assert(M.FakeError.Near = 4);
    Assert(M.FakeError.Message.Trim = 'Bad parenthesis.');
    Assert(M.FakeError.Kind = ekBadParens);
    Assert(not M.Compile('1++2'));
    Assert(M.FakeError.Near = 2);
    Assert(M.FakeError.Message.Trim = 'Missing expected operand.');
    Assert(M.FakeError.Kind = ekMissingOperand);
  finally
    M.Free;
  end;
end;

procedure Test_MathExpressionOnActivate;
var
  M: TFakeMathExpression;
begin
  M := TFakeMathExpression.Create(nil);
  try
    M.OnActivate := M.FakeOnActivate;
    FakeFlag := False;
    M.Open;
    Assert(FakeFlag);
  finally
    M.Free;
  end;
end;

procedure Test_MathExpressionOnDeactivate;
var
  M: TFakeMathExpression;
begin
  M := TFakeMathExpression.Create(nil);
  try
    M.Open;
    M.OnDeactivate := M.FakeOnDeactivate;
    FakeFlag := False;
    M.Close;
    Assert(FakeFlag);
  finally
    M.Free;
  end;
end;

procedure Test_MathExpression_Evaluate;
begin
  Assert(Evaluate('').ToString = NaN.ToString);
  Assert(Evaluate('1.2+3.4').ToString = Double.ToString(4.6));
  Assert(Evaluate('foo=9.8, bar=7.6, foo + bar').ToString = Double.ToString(17.4));
end;

begin
{$IF (NOT DEFINED(FPC)) AND DEFINED(DEBUG)}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  TBrookLibraryLoader.Load;
  try
    Test_MathExpressionErrorCreate;
    Test_MathExpressionErrorNear;
    Test_MathExpressionErrorKind;
    Test_MathExpressionErrorMessage;
    Test_MathExpressionErrorHandle;
    Test_MathExpressionExtensionCreate;
    Test_MathExpressionExtensionHasArgs;
    Test_MathExpressionExtensionArgs;
    Test_MathExpressionExtensionIdent;
    Test_MathExpressionExtensionHandle;
    Test_MathExpressionCreate;
    // Test_MathExpressionDestroy - not required
    Test_MathExpressionOpen;
    Test_MathExpressionClose;
    Test_MathExpressionCompile;
    Test_MathExpressionClear;
    Test_MathExpressionEvaluate;
    Test_MathExpressionGetVariable;
    Test_MathExpressionSetVariable;
    Test_MathExpressionCompiled;
    Test_MathExpressionVariables;
    Test_MathExpressionActive;
    Test_MathExpressionExpression;
    Test_MathExpressionExtensions;
    Test_MathExpressionOnExtension;
    Test_MathExpressionOnError;
    Test_MathExpressionOnActivate;
    Test_MathExpressionOnDeactivate;
    Test_MathExpression_Evaluate;
  finally
    TBrookLibraryLoader.Unload;
  end;
end.
