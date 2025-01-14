program Make;
{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  FileUtil,
  Zipper,
  fphttpclient,
  RegExpr,
  openssl,
  opensslsockets,
  Process;

const
  Src: string = 'Examples';
  Use: string = 'Package';
  Tst: string = 'testconsole.lpi';
  Pkg: array of string = ();

type
  Output = record
    Code: integer;
    Output: ansistring;
  end;

var
  Each, Item, PackagePath, TempFile, Url: string;
  Line: ansistring;
  Answer: Output;
  List: TStringList;
  Zip: TStream;

  procedure CheckModules;
  begin
    if FileExists('.gitmodules') then
      if RunCommand('git', ['submodule', 'update', '--init', '--recursive',
        '--force', '--remote'], Answer.Output) then
        Writeln(stderr, #27'[33m', Answer.Output, #27'[0m')
      else
      begin
        ExitCode += 1;
        Writeln(stderr, #27'[31m', Answer.Output, #27'[0m');
      end;
  end;

  procedure AddPackage(Path: string);
  begin
    List := FindAllFiles(Use, '*.lpk', True);
    try
      for Each in List do
        if RunCommand('lazbuild', ['--add-package-link', Each], Answer.Output) then
          Writeln(stderr, #27'[33m', 'added ', Each, #27'[0m')
        else
        begin
          ExitCode += 1;
          Writeln(stderr, #27'[31m', 'added ', Each, #27'[0m');
        end;
    finally
      List.Free;
    end;
  end;

  procedure AddOPM;
  begin
    InitSSLInterface;
    for Each in Pkg do
    begin
      PackagePath :=
        {$IFDEF MSWINDOWS}
        GetEnvironmentVariable('APPDATA') + '\.lazarus\onlinepackagemanager\packages\'
        {$ELSE}
      GetEnvironmentVariable('HOME') + '/.lazarus/onlinepackagemanager/packages/'
        {$ENDIF}
        + Each;
      TempFile := GetTempFileName;
      Url := 'https://packages.lazarus-ide.org/' + Each + '.zip';
      if not DirectoryExists(PackagePath) then
      begin
        Zip := TFileStream.Create(TempFile, fmCreate or fmOpenWrite);
        with TFPHttpClient.Create(nil) do
        begin
          try
            AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
            AllowRedirect := True;
            Get(Url, Zip);
            WriteLn(stderr, 'Download from ', Url, ' to ', TempFile);
          finally
            Free;
          end;
        end;
        Zip.Free;
        CreateDir(PackagePath);
        with TUnZipper.Create do
        begin
          try
            FileName := TempFile;
            OutputPath := PackagePath;
            Examine;
            UnZipAllFiles;
            WriteLn(stderr, 'Unzip from ', TempFile, ' to ', PackagePath);
          finally
            Free;
          end;
        end;
        DeleteFile(TempFile);
        AddPackage(PackagePath);
      end;
    end;
  end;

  procedure BuildProject(Path: string);
  begin
    Write(stderr, #27'[33m', 'build from ', Each, #27'[0m');
    try
      if RunCommand('lazbuild', ['--build-all', '--recursive',
        '--no-write-project', Each], Answer.Output) then
        Answer.Code := 0
      else
      begin
        Answer.Code := 1;
        ExitCode += Answer.Code;
      end;
    except
      on E: Exception do
        WriteLn(stderr, 'Error: ' + E.ClassName + #13#10 + E.Message);
    end;
  end;

  procedure RunTest;
  begin
    List := FindAllFiles('.', Tst, True);
    try
      for Each in List do
      begin
        BuildProject(Each);
        if Answer.Code <> 0 then
        begin
          for Line in SplitString(Answer.Output, LineEnding) do
            with TRegExpr.Create do
            begin
              Expression := '(Fatal|Error):';
              if Exec(Line) then
              begin
                WriteLn(stderr);
                Writeln(stderr, #27'[31m', Line, #27'[0m');
              end;
              Free;
            end;
        end
        else
          for Line in SplitString(Answer.Output, LineEnding) do
            if Pos('Linking', Line) <> 0 then
            try
              begin
                Writeln(stderr, #27'[32m', ' to ', SplitString(Line, ' ')[2], #27'[0m');
                if not RunCommand(ReplaceStr(SplitString(Line, ' ')[2],
                  SplitString(Tst, '.')[0], './' + SplitString(Tst, '.')[0]),
                  ['--all', '--format=plain', '--progress'], Answer.Output) then
                  ExitCode += 1;
                WriteLn(stderr, Answer.Output);
                break;
              end;
            except
              on E: Exception do
                WriteLn(stderr, 'Error: ' + E.ClassName + #13#10 + E.Message);
            end;
      end;
    finally
      List.Free;
    end;
  end;

begin
  CheckModules;
  AddPackage(Use);
  AddOPM;
  {$IFDEF LINUX}
  RunTest;
  {$ENDIF}
  List := FindAllFiles(Src, '*.lpi', True);
  try
    for Each in List do
      if Pos(Tst, Each) = 0 then
      begin
        BuildProject(Each);
        if Answer.Code <> 0 then
        begin
          for Line in SplitString(Answer.Output, LineEnding) do
            with TRegExpr.Create do
            begin
              Expression := '(Fatal|Error):';
              if Exec(Line) then
              begin
                WriteLn(stderr);
                Writeln(stderr, #27'[31m', Line, #27'[0m');
              end;
              Free;
            end;
        end
        else
          for Line in SplitString(Answer.Output, LineEnding) do
            if Pos('Linking', Line) <> 0 then
              Writeln(stderr, #27'[32m', ' to ', SplitString(Line, ' ')[2], #27'[0m');
      end;
  finally
    List.Free;
  end;
  WriteLn(stderr);
  if ExitCode <> 0 then
    WriteLn(stderr, #27'[31m', 'Errors: ', ExitCode, #27'[0m')
  else
    WriteLn(stderr, #27'[32m', 'Errors: ', ExitCode, #27'[0m');
end.
