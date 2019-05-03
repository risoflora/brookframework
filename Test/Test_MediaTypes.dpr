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
 * This file is part of Brook framework.
 *
 * Brook framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook framework.  If not, see <http://www.gnu.org/licenses/>.
 *)

program Test_MediaTypes;

{$I Tests.inc}

uses
  RTLConsts,
  SysUtils,
  Classes,
  BrookExtra,
  BrookLibraryLoader,
  BrookReader,
  BrookMediaTypes,
  Test;

const
  MIME_TYPES_FILE = '../Examples/Common/mime.types';

var
  MIMEFileName: TFileName;

type

  { TFakeMediaTypes }

  TFakeMediaTypes = class(TBrookMediaTypes)
  private
    FPrepared: Boolean;
  protected
    function IsPrepared: Boolean; override;
  public
    class function GetDescription: string; override;
    procedure Prepare; override;
    property Cache;
  end;

{ TFakeMediaTypes }

class function TFakeMediaTypes.GetDescription: string;
begin
  Result := 'Fake';
end;

procedure TFakeMediaTypes.Prepare;
begin
  FPrepared := True;
end;

function TFakeMediaTypes.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

{ MediaTypes }

procedure Test_MediaTypesCreate;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(Assigned(MT.Cache));
    Assert(MT.DefaultType = BROOK_CT_OCTET_STREAM);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesGetRegisterAlias;
begin
  Assert(TFakeMediaTypes.GetRegisterAlias = 'BrookMIME_Fake');
end;

procedure Test_MediaTypesGetDescription;
begin
  Assert(TFakeMediaTypes.GetDescription = 'Fake');
end;

procedure Test_MediaTypesIsValid;
begin
  Assert(not TBrookMediaTypes.IsValid(''));
  Assert(not TBrookMediaTypes.IsValid('abc'));
  Assert(not TBrookMediaTypes.IsValid('abc/'));
  Assert(not TBrookMediaTypes.IsValid('/abc'));
  Assert(not TBrookMediaTypes.IsValid('/abc/def/def'));
  Assert(TBrookMediaTypes.IsValid('abc/def'));
end;

procedure Test_MediaTypesIsText;
begin
  Assert(not TBrookMediaTypes.IsText(''));
  Assert(not TBrookMediaTypes.IsText('abc'));
  Assert(not TBrookMediaTypes.IsText('abc/'));
  Assert(not TBrookMediaTypes.IsText('/abc'));
  Assert(not TBrookMediaTypes.IsText('/abc/def/def'));
  Assert(not TBrookMediaTypes.IsText('abc/def'));
  Assert(not TBrookMediaTypes.IsText('application/xml'));
  Assert(TBrookMediaTypes.IsText('text/html'));
end;

procedure Test_MediaTypesIsExt;
begin
  Assert(not TBrookMediaTypes.IsExt(''));
  Assert(not TBrookMediaTypes.IsExt('.'));
  Assert(not TBrookMediaTypes.IsExt('..'));
  Assert(TBrookMediaTypes.IsExt('js'));
  Assert(TBrookMediaTypes.IsExt('.js'));
end;

procedure Test_MediaTypesNormalizeExt;
begin
  Assert(TBrookMediaTypes.NormalizeExt('') = '');
  Assert(TBrookMediaTypes.NormalizeExt('.') = '.');
  Assert(TBrookMediaTypes.NormalizeExt('a') = '.a');
  Assert(TBrookMediaTypes.NormalizeExt('.a') = '.a');
  Assert(TBrookMediaTypes.NormalizeExt('a.b') = '.a.b');
  Assert(TBrookMediaTypes.NormalizeExt('.abc') = '.abc');
end;

procedure Test_MediaTypesPrepare;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.IsPrepared);
    MT.Prepare;
    Assert(MT.IsPrepared);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesAddEmptyMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Add('', 'bar/foo');
end;

procedure DoMediaTypesAddInvalidMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Add('.', 'bar/foo');
end;

procedure DoMediaTypesAddEmptyMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Add('.foo', '');
end;

procedure DoMediaTypesAddInvalidMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Add('.foo', 'bar');
end;

procedure Test_MediaTypesAdd;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.Prepared);
    Assert(MT.Count = 0);
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Count = 2);

    MT.Clear;
    AssertExcept(DoMediaTypesAddEmptyMediaExt,
      EArgumentException, SBrookEmptyMediaExt, [MT]);
    AssertExcept(DoMediaTypesAddInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [MT]);
    AssertExcept(DoMediaTypesAddEmptyMediaType,
      EArgumentException, SBrookEmptyMediaType, [MT]);
    AssertExcept(DoMediaTypesAddInvalidMediaType,
      EBrookMediaTypes, Format(SBrookInvalidMediaType, ['bar']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesRemoveEmptyMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Remove('');
end;

procedure DoMediaTypesRemoveInvalidMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Remove('.');
end;

procedure Test_MediaTypesRemove;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Count = 2);
    MT.Remove('.foo');
    Assert(MT.Count = 1);
    MT.Remove('.bar');
    Assert(MT.Count = 0);

    AssertExcept(DoMediaTypesRemoveEmptyMediaExt,
      EArgumentException, SBrookEmptyMediaExt, [MT]);
    AssertExcept(DoMediaTypesRemoveInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesTryTypeEmptyMediaExt(const AArgs: array of const);
var
  D: string;
begin
  TFakeMediaTypes(AArgs[0].VObject).TryType('', D);
end;

procedure DoMediaTypesTryTypeInvalidMediaExt(const AArgs: array of const);
var
  D: string;
begin
  TFakeMediaTypes(AArgs[0].VObject).TryType('.', D);
end;

procedure Test_MediaTypesTryType;
var
  MT: TFakeMediaTypes;
  T: string;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.Prepared);
    Assert(not MT.TryType('.foo', T));
    Assert(not MT.TryType('.bar', T));
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.TryType('.foo', T));
    Assert(T = 'bar/foo');
    Assert(MT.TryType('.bar', T));
    Assert(T = 'foo/bar');
    Assert(MT.Prepared);

    AssertExcept(DoMediaTypesTryTypeEmptyMediaExt,
      EArgumentException, SBrookEmptyMediaExt, [MT]);
    AssertExcept(DoMediaTypesTryTypeInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesFindEmptyMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Find('');
end;

procedure DoMediaTypesFindInvalidMediaExt(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Find('.');
end;

procedure DoMediaTypesFindEmptyMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Find('.foo', '');
end;

procedure DoMediaTypesFindInvalidMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).Find('.foo', 'bar');
end;

procedure Test_MediaTypesFind;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.Prepared);
    Assert(MT.Find('.foo') = MT.DefaultType);
    Assert(MT.Find('.foo', 'bar/foo') = 'bar/foo');
    Assert(MT.Find('.bar') = MT.DefaultType);
    Assert(MT.Find('.bar', 'bar/foo') = 'bar/foo');
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Find('.foo') = 'bar/foo');
    Assert(MT.Find('.bar') = 'foo/bar');
    Assert(MT.Prepared);

    AssertExcept(DoMediaTypesFindEmptyMediaExt,
      EArgumentException, SBrookEmptyMediaExt, [MT]);
    AssertExcept(DoMediaTypesFindInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [MT]);
    AssertExcept(DoMediaTypesFindEmptyMediaType,
      EArgumentException, SBrookEmptyMediaType, [MT]);
    AssertExcept(DoMediaTypesFindInvalidMediaType,
      EBrookMediaTypes, Format(SBrookInvalidMediaType, ['bar']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesCount;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(MT.Count = 0);
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Count = 2);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesClear;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(MT.Count = 0);
    MT.Add('.foo', 'bar/foo');
    MT.Add('.bar', 'foo/bar');
    Assert(MT.Count = 2);
    MT.Clear;
    Assert(MT.Count = 0);
  finally
    MT.Free;
  end;
end;

procedure DoMediaTypesDefaultTypeEmptyMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).DefaultType := '';
end;

procedure DoMediaTypesDefaultTypeInvalidMediaType(const AArgs: array of const);
begin
  TFakeMediaTypes(AArgs[0].VObject).DefaultType := 'bar';
end;

procedure Test_MediaTypesDefaultType;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(MT.DefaultType = BROOK_CT_OCTET_STREAM);
    MT.DefaultType := 'foo/bar';
    Assert(MT.DefaultType = 'foo/bar');

    AssertExcept(DoMediaTypesDefaultTypeEmptyMediaType,
      EArgumentException, SBrookEmptyMediaType, [MT]);
    AssertExcept(DoMediaTypesDefaultTypeInvalidMediaType,
      EBrookMediaTypes, Format(SBrookInvalidMediaType, ['bar']), [MT]);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesPrepared;
var
  MT: TFakeMediaTypes;
begin
  MT := TFakeMediaTypes.Create;
  try
    Assert(not MT.Prepared);
    MT.Prepare;
    Assert(MT.Prepared);
  finally
    MT.Free;
  end
end;

procedure DoMediaTypesParserCreateParamIsNilReader(const AArgs: array of const);
begin
  TBrookMediaTypesParser.Create(nil, TBrookMediaTypes(AArgs[0].VObject));
end;

procedure DoMediaTypesParserCreateParamIsNilTypes(const AArgs: array of const);
begin
  TBrookMediaTypesParser.Create(TBrookTextReader(AArgs[0].VObject), nil);
end;

procedure Test_MediaTypesParserCreate;
var
  R: TBrookTextReader;
  T: TBrookMediaTypes;
  P: TBrookMediaTypesParser;
begin
  R := TBrookStringReader.Create('');
  T := TFakeMediaTypes.Create;
  P := TBrookMediaTypesParser.Create(R, T);
  try
    Assert(Assigned(P.Reader) and (P.Reader = R));
    Assert(Assigned(P.Types) and (P.Types = T));

    AssertExcept(DoMediaTypesParserCreateParamIsNilReader,
      EArgumentNilException, Format(SParamIsNil, ['AReader']), [T]);
    AssertExcept(DoMediaTypesParserCreateParamIsNilTypes,
      EArgumentNilException, Format(SParamIsNil, ['ATypes']), [R]);
  finally
    T.Free;
    R.Free;
    P.Free;
  end;
end;

procedure DoMediaTypesParserParseInvalidMediaType(const AArgs: array of const);
begin
  TBrookMediaTypesParser(AArgs[0].VObject).Parse;
end;

procedure DoMediaTypesParserParseInvalidMediaExt(const AArgs: array of const);
begin
  TBrookMediaTypesParser(AArgs[0].VObject).Parse;
end;

procedure Test_MediaTypesParserParse;
var
  R: TBrookTextReader;
  T: TBrookMediaTypes;
  P: TBrookMediaTypesParser;
begin
  R := TBrookStringReader.Create(Concat(
    'text/plain', #9,'txt', sLineBreak,
    'text/html', #9,'html htm xhtml', sLineBreak,
    'application/json', #9,'json', sLineBreak,
    '#application/docbook+xml', #9,'docbook dbk', sLineBreak,
    'application/pdf', #9,'pdf', sLineBreak,
    'foo bar', sLineBreak,
    'foo=bar'
  ));
  T := TFakeMediaTypes.Create;
  P := TBrookMediaTypesParser.Create(R, T);
  try
    Assert(P.Types.Count = 0);
    P.Parse;
    Assert(P.Types.Count = 6);
    Assert(P.Types.Find('.txt') = 'text/plain');
    Assert(P.Types.Find('.html') = 'text/html');
    Assert(P.Types.Find('.htm') = 'text/html');
    Assert(P.Types.Find('.xhtml') = 'text/html');
    Assert(P.Types.Find('.json') = 'application/json');
    Assert(P.Types.Find('.docbook') = P.Types.DefaultType);
    Assert(P.Types.Find('.pdf') = 'application/pdf');
  finally
    T.Free;
    R.Free;
    P.Free;
  end;

  R := TBrookStringReader.Create(Concat(
    'text/plain', #9, 'txt', sLineBreak,
    'application/json', #9, 'json', sLineBreak,
    'foo', #9, 'bar'
  ));
  T := TFakeMediaTypes.Create;
  P := TBrookMediaTypesParser.Create(R, T);
  try
    Assert(P.Types.Count = 0);
    AssertExcept(DoMediaTypesParserParseInvalidMediaType,
      EBrookMediaTypes, Format(SBrookInvalidMediaType, ['foo']), [P]);
    Assert(P.Types.Count = 2);
  finally
    T.Free;
    R.Free;
    P.Free;
  end;

  R := TBrookStringReader.Create(Concat(
    'text/plain', #9, 'txt', sLineBreak,
    'application/json', #9, 'json', sLineBreak,
    'foo/bar', #9, '.'
  ));
  T := TFakeMediaTypes.Create;
  P := TBrookMediaTypesParser.Create(R, T);
  try
    Assert(P.Types.Count = 0);
    AssertExcept(DoMediaTypesParserParseInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [P]);
    Assert(P.Types.Count = 2);
  finally
    T.Free;
    R.Free;
    P.Free;
  end;
end;

procedure Test_MediaTypesParserReader;
var
  R: TBrookTextReader;
  T: TBrookMediaTypes;
  P: TBrookMediaTypesParser;
begin
  R := TBrookStringReader.Create('');
  T := TFakeMediaTypes.Create;
  P := TBrookMediaTypesParser.Create(R, T);
  try
    Assert(Assigned(P.Reader) and (P.Reader = R));
  finally
    T.Free;
    R.Free;
    P.Free;
  end;
end;

procedure Test_MediaTypesParserTypes;
var
  R: TBrookTextReader;
  T: TBrookMediaTypes;
  P: TBrookMediaTypesParser;
begin
  R := TBrookStringReader.Create('');
  T := TFakeMediaTypes.Create;
  P := TBrookMediaTypesParser.Create(R, T);
  try
    Assert(Assigned(P.Types) and (P.Types = T));
  finally
    T.Free;
    R.Free;
    P.Free;
  end;
end;

procedure DoMediaTypesParserNginxParseInvalidMediaType(const AArgs: array of const);
begin
  TBrookMediaTypesParserNginx(AArgs[0].VObject).Parse;
end;

procedure DoMediaTypesParserNginxParseInvalidMediaExt(const AArgs: array of const);
begin
  TBrookMediaTypesParserNginx(AArgs[0].VObject).Parse;
end;

procedure Test_MediaTypesParserNginxParse;
var
  R: TBrookTextReader;
  T: TBrookMediaTypes;
  P: TBrookMediaTypesParser;
begin
  R := TBrookStringReader.Create(Concat(
     sLineBreak,
     'types {', sLineBreak,
     '    text/html                                        html htm shtml;', sLineBreak,
     sLineBreak,
     '    text/css                                         css;', sLineBreak,
     '#    text/xml                                         xml;', sLineBreak,
     '    application/javascript                           js;', sLineBreak,
     '#    application/atom+xml                             atom;', sLineBreak,
     '    application/rss+xml                              rss;', sLineBreak,
     '}', sLineBreak
  ));
  T := TFakeMediaTypes.Create;
  P := TBrookMediaTypesParserNginx.Create(R, T);
  try
    Assert(P.Types.Count = 0);
    P.Parse;
    Assert(P.Types.Count = 6);
    Assert(P.Types.Find('.html') = 'text/html');
    Assert(P.Types.Find('.htm') = 'text/html');
    Assert(P.Types.Find('.shtml') = 'text/html');
    Assert(P.Types.Find('.js') = 'application/javascript');
    Assert(P.Types.Find('.xml') = P.Types.DefaultType);
    Assert(P.Types.Find('.rss') = 'application/rss+xml');
  finally
    T.Free;
    R.Free;
    P.Free;
  end;

  R := TBrookStringReader.Create(Concat(
    'types {', sLineBreak,
    '    text/html html;', sLineBreak,
    '    text/css css;', sLineBreak,
    '    foo bar;', sLineBreak,
    '}'
  ));
  T := TFakeMediaTypes.Create;
  P := TBrookMediaTypesParserNginx.Create(R, T);
  try
    Assert(P.Types.Count = 0);
    AssertExcept(DoMediaTypesParserNginxParseInvalidMediaType,
      EBrookMediaTypes, Format(SBrookInvalidMediaType, ['foo']), [P]);
    Assert(P.Types.Count = 2);
  finally
    T.Free;
    R.Free;
    P.Free;
  end;

  R := TBrookStringReader.Create(Concat(
    'types {', sLineBreak,
    '    text/html html;', sLineBreak,
    '    text/css css;', sLineBreak,
    '    foo .;', sLineBreak,
    '}'
  ));
  T := TFakeMediaTypes.Create;
  P := TBrookMediaTypesParserNginx.Create(R, T);
  try
    Assert(P.Types.Count = 0);
    AssertExcept(DoMediaTypesParserNginxParseInvalidMediaExt,
      EBrookMediaTypes, Format(SBrookInvalidMediaExt, ['.']), [P]);
    Assert(P.Types.Count = 2);
  finally
    T.Free;
    R.Free;
    P.Free;
  end;
end;

procedure DoMediaTypesPathCreateFOpenError(const AArgs: array of const);
begin
  TBrookMediaTypesPath.Create(string(AArgs[0].VPChar));
end;

procedure Test_MediaTypesPathCreate;
var
  MT: TBrookMediaTypesPath;
begin
  MT := TBrookMediaTypesPath.Create(MIME_TYPES_FILE);
  try
    Assert(MT.FileName = MIME_TYPES_FILE);
    Assert(Assigned(MT.Reader));
    Assert(Assigned(MT.Parser));
  finally
    MT.Free;
  end;

  MT := TBrookMediaTypesPath.Create;
  try
    Assert(MT.FileName = MIMEFileName);
    Assert(Assigned(MT.Reader));
    Assert(Assigned(MT.Parser));
  finally
    MT.Free;
  end;

  AssertExcept(DoMediaTypesPathCreateFOpenError, EFOpenError,
    Format(SFOpenError, ['']), [PChar('')]);
  DeleteFile('foo');
  AssertExcept(DoMediaTypesPathCreateFOpenError, EFOpenError,
    Format(SFOpenError, ['foo']), [PChar('foo')]);
end;

procedure Test_MediaTypesPathGetDescription;
begin
  Assert(TBrookMediaTypesPath.GetDescription = 'Default');
end;

procedure Test_MediaTypesPathGetFileName;
begin
  Assert(TBrookMediaTypesPath.GetFileName = MIMEFileName);
end;

procedure Test_MediaTypesPathPrepare;
var
  MT: TBrookMediaTypesPath;
begin
  MT := TBrookMediaTypesPath.Create(MIME_TYPES_FILE);
  try
    Assert(not MT.Prepared);
    MT.Prepare;
    Assert(MT.Prepared);
    Assert(MT.Find('.txt') = 'text/plain');
    Assert(MT.Find('.html') = 'text/html');
    Assert(MT.Find('.json') = 'application/json');
    Assert(MT.Find('.pdf') = 'application/pdf');
    Assert(MT.Find('.xxx') = MT.DefaultType);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesPathClear;
var
  MT: TBrookMediaTypesPath;
begin
  MT := TBrookMediaTypesPath.Create(MIME_TYPES_FILE);
  try
    Assert(MT.Count = 0);
    MT.Prepare;
    Assert(MT.Count > 0);
    MT.Clear;
    Assert(MT.Count = 0);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesPathReader;
var
  MT: TBrookMediaTypesPath;
begin
  MT := TBrookMediaTypesPath.Create(MIME_TYPES_FILE);
  try
    Assert(Assigned(MT.Reader));
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesPathParser;
var
  MT: TBrookMediaTypesPath;
begin
  MT := TBrookMediaTypesPath.Create(MIME_TYPES_FILE);
  try
    Assert(Assigned(MT.Parser));
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesPathFileName;
var
  MT: TBrookMediaTypesPath;
begin
  MT := TBrookMediaTypesPath.Create(MIMEFileName);
  try
    Assert(MT.FileName = MIMEFileName);
  finally
    MT.Free;
  end;
end;

procedure Test_MediaTypesApacheGetDescription;
begin
  Assert(TBrookMediaTypesApache.GetDescription = 'Apache');
end;

procedure Test_MediaTypesNginxGetDescription;
begin
  Assert(TBrookMediaTypesNginx.GetDescription = 'Nginx');
end;

procedure Test_MediaTypesNginxGetFileName;
begin
  Assert(TBrookMediaTypesNginx.GetFileName = Concat(
{$IFDEF UNIX}'/etc/nginx/'{$ELSE}ExtractFilePath(ParamStr(0)){$ENDIF},
    BROOK_MIME_FILE));
end;

procedure Test_MediaTypesWindowsGetDescription;
begin
  Assert(TBrookMediaTypesWindows.GetDescription = 'Windows');
end;

procedure Test_MediaTypesUnixGetDescription;
begin
  Assert(TBrookMediaTypesUnix.GetDescription = 'Unix');
end;

function LocalMediaTypesUnixGetFileName: TFileName;
var
  FNs: TArray<TFileName>;
  FN: TFileName;
begin
  FNs := TArray<TFileName>.Create(
    Concat('/etc/', BROOK_MIME_FILE)
    // Put other 'mime.types' paths here...
  );
  for FN in FNs do
    if FileExists(FN) then
      Exit(FN);
  Result := BROOK_MIME_FILE;
end;

procedure Test_MediaTypesUnixGetFileName;
begin
  Assert(TBrookMediaTypesUnix.GetFileName = LocalMediaTypesUnixGetFileName);
end;

procedure Test_MIMECreate;
var
  M: TBrookMIME;
  C: TComponent;
begin
  C := TComponent.Create(nil);
  M := TBrookMIME.Create(C);
  try
    Assert(M.Owner = C);
    M.Open;
    Assert(M.Active);
    TBrookLibraryLoader.Unload;
    Assert(not M.Active);
    TBrookLibraryLoader.Load;
    Assert(M.DefaultType = BROOK_CT_OCTET_STREAM);
    Assert(M.FileName = MIMEFileName);
    Assert(M.Provider = 'Default');
  finally
    M.Free;
    C.Free;
  end;
end;

procedure Test_MIMEGetProviderClass;
var
  M: TBrookMIME;
begin
  M := TBrookMIME.Create(nil);
  try
    M.Provider := 'Nginx';
    M.Open;
    Assert(M.GetProviderClass = TBrookMediaTypesNginx);
    M.Close;
    M.Provider := 'Apache';
    M.Open;
    Assert(M.GetProviderClass = TBrookMediaTypesApache);
  finally
    M.Free;
  end;
end;

procedure Test_MIMEOpen;
var
  M: TBrookMIME;
begin
  M := TBrookMIME.Create(nil);
  try
    Assert(not M.Active);
    M.Open;
    Assert(M.Active);
  finally
    M.Free;
  end;
end;

procedure Test_MIMEClose;
var
  M: TBrookMIME;
begin
  M := TBrookMIME.Create(nil);
  try
    Assert(not M.Active);
    M.Open;
    Assert(M.Active);
    M.Close;
    Assert(not M.Active);
  finally
    M.Free;
  end;
end;

procedure Test_MIMETypes;
var
  M: TBrookMIME;
begin
  M := TBrookMIME.Create(nil);
  try
    M.Provider := 'Default';
    M.Open;
    Assert(M.Types is TBrookMediaTypesPath);
  finally
    M.Free;
  end;
end;

procedure Test_MIMEActive;
var
  M: TBrookMIME;
begin
  M := TBrookMIME.Create(nil);
  try
    Assert(not M.Active);
    M.Active := not M.Active;
    Assert(M.Active);

    M.Active := False;
    M.Provider := 'Fake';
    RegisterClassAlias(TFakeMediaTypes, TFakeMediaTypes.GetRegisterAlias);
    M.Active := True;
    Assert(M.Types is TFakeMediaTypes);
    UnRegisterClass(TFakeMediaTypes);
    M.Active := False;
    M.Provider := 'Unix';
    M.Active := True;
    Assert(M.Types is TBrookMediaTypesUnix);

    Assert(M.DefaultType = BROOK_CT_OCTET_STREAM);
  finally
    M.Free;
  end;
end;

procedure Test_MIMEDefaultType;
var
  M: TBrookMIME;
begin
  M := TBrookMIME.Create(nil);
  try
    Assert(M.DefaultType = BROOK_CT_OCTET_STREAM);
    M.DefaultType := '';
    Assert(M.DefaultType = BROOK_CT_OCTET_STREAM);
    M.DefaultType := 'abc';
    Assert(M.DefaultType = BROOK_CT_OCTET_STREAM);
    M.DefaultType := 'text/plain';
    Assert(M.DefaultType = 'text/plain');
  finally
    M.Free;
  end;
end;

procedure Test_MIMEFileName;
var
  M: TBrookMIME;
begin
  M := TBrookMIME.Create(nil);
  try
    Assert(M.FileName = MIMEFileName);
    M.FileName := '';
    Assert(M.FileName = MIMEFileName);
    M.FileName := 'abc';
    Assert(M.FileName = 'abc');
    M.FileName := MIME_TYPES_FILE;
    Assert(M.FileName = MIME_TYPES_FILE);
  finally
    M.Free;
  end;
end;

procedure Test_MIMEProvider;
var
  M: TBrookMIME;
begin
  M := TBrookMIME.Create(nil);
  try
    Assert(M.Provider = 'Default');
    M.Provider := '';
    Assert(M.Provider = 'Default');
    M.Provider := 'abc';
    Assert(M.Provider = 'abc');
    M.Provider := 'Unix';
    Assert(M.Provider = 'Unix');
  finally
    M.Free;
  end;
end;

begin
  MIMEFileName := Concat(
{$IFDEF UNIX}'/etc/'{$ELSE}ExtractFilePath(ParamStr(0)){$ENDIF},
    BROOK_MIME_FILE);
  TBrookLibraryLoader.Load;
  Test_MediaTypesCreate;
  // Test_MediaTypesDestroy - not required
  Test_MediaTypesGetRegisterAlias;
  Test_MediaTypesGetDescription;
  Test_MediaTypesIsValid;
  Test_MediaTypesIsText;
  Test_MediaTypesIsExt;
  Test_MediaTypesNormalizeExt;
  Test_MediaTypesPrepare;
  Test_MediaTypesAdd;
  Test_MediaTypesRemove;
  Test_MediaTypesTryType;
  Test_MediaTypesFind;
  Test_MediaTypesCount;
  Test_MediaTypesClear;
  Test_MediaTypesDefaultType;
  Test_MediaTypesPrepared;
  Test_MediaTypesParserCreate;
  Test_MediaTypesParserParse;
  Test_MediaTypesParserReader;
  Test_MediaTypesParserTypes;
  Test_MediaTypesParserNginxParse;
  Test_MediaTypesPathCreate;
  // Test_MediaTypesPathDestroy - not required
  Test_MediaTypesPathGetDescription;
  Test_MediaTypesPathGetFileName;
  Test_MediaTypesPathPrepare;
  Test_MediaTypesPathClear;
  Test_MediaTypesPathReader;
  Test_MediaTypesPathParser;
  Test_MediaTypesPathFileName;
  Test_MediaTypesApacheGetDescription;
  Test_MediaTypesNginxGetDescription;
  Test_MediaTypesNginxGetFileName;
  Test_MediaTypesWindowsGetDescription;
  Test_MediaTypesUnixGetDescription;
  Test_MediaTypesUnixGetFileName;
  Test_MIMECreate;
  // Test_MIMEDestroy - not required
  Test_MIMEGetProviderClass;
  Test_MIMEOpen;
  Test_MIMEClose;
  Test_MIMETypes;
  Test_MIMEActive;
  Test_MIMEDefaultType;
  Test_MIMEFileName;
  Test_MIMEProvider;
end.
