{ This file is part of Emuteca

  Copyright (C) 2006-2013 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{ Unit of Script Engine class. }
unit uEmutecaScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  // Common units
  Classes, SysUtils, Controls, StrUtils, LazUTF8, Dialogs, Forms, FileUtil,
  // Emuteca classes
  uCHXStrUtils, uEmutecaGame, uEmutecagameGroup, uEmutecaMainManager,
  // Pascal Script main units
  uPSComponent, uPSRuntime, uPSCompiler, uPSUtils,
  // Pascal script common units import
  uPSR_std, uPSR_controls, uPSR_stdctrls, uPSR_forms, uPSR_buttons,
  uPSR_classes, uPSR_dateutils, uPSR_dll, uPSR_DB, uPSR_extctrls,
  uPSR_graphics, uPSR_menus,
  uPSC_std, uPSC_controls, uPSC_stdctrls, uPSC_forms, uPSC_buttons,
  uPSC_classes, uPSC_dateutils, uPSC_dll, uPSC_DB, uPSC_extctrls,
  uPSC_graphics, uPSC_menus, uPSC_strutils,
  // Pascal Script Emuteca clases import
  uPSI_uGame, uPSI_uGameGroup,
  uPSI_uGameManager, uPSI_u7zWrapper, uPSI_uEmulator,
  uPSI_uSystem, uPSI_uPlayingStats,
  // TODO 2: Generalize input and output as events,
  //   and move these units to fScriptManager.
  fSMAskFile, fSMAskFolder,
  // Common
  uRscStr
  // Emuteca
;

type

  { cScriptEngEmuteca }

  cScriptEngEmuteca = class(TObject)

  private
    FCommonUnitFolder: string;
    FGame: cEmutecaGame;
    FGameGroup: cEmutecaGameGroup;
    FGameManager: cEmutecaMainManager;
    FOwnsScriptError: boolean;
    FOwnsScriptInfo: boolean;
    FOwnsScriptOutput: boolean;
    FPasScript: TPSScript;
    FScriptError: TStrings;
    FScriptInfo: TStrings;
    FScriptOutput: TStrings;
    function getScriptFile: string;
    function getScriptText: TStrings;
    procedure SetCommonUnitFolder(AValue: string);
    procedure SetGame(AValue: cEmutecaGame);
    procedure SetGameGroup(AValue: cEmutecaGameGroup);
    procedure SetGameManager(AValue: cEmutecaMainManager);
    procedure SetOwnsScriptError(AValue: boolean);
    procedure SetOwnsScriptInfo(AValue: boolean);
    procedure SetOwnsScriptOutput(AValue: boolean);
    procedure SetPasScript(AValue: TPSScript);
    procedure SetScriptError(AValue: TStrings);
    procedure setScriptFile(AValue: string);
    procedure SetScriptInfo(AValue: TStrings);
    procedure SetScriptOutput(AValue: TStrings);
    procedure setScriptText(AValue: TStrings);

  protected
    property PasScript: TPSScript read FPasScript write SetPasScript;
    {< PSScript object.}

    property OwnsScriptOutput: boolean
      read FOwnsScriptOutput write SetOwnsScriptOutput;
    {< Script output must be freed? }
    property OwnsScriptInfo: boolean read FOwnsScriptInfo
      write SetOwnsScriptInfo;
    {< Script info must be freed? }
    property OwnsScriptError: boolean read FOwnsScriptError
      write SetOwnsScriptError;
    {< Script error must be freed? }

    procedure PasScriptOnCompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure PasScriptOnCompile(Sender: TPSScript);
    procedure PasScriptOnExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure PasScriptOnExecute(Sender: TPSScript);
    function PasScriptOnFindUnknownFile(Sender: TObject; const OrginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
    function PasScriptOnNeedFile(Sender: TObject; const OriginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;

    // Added functions
    // ---------------
    // This functions are those which don't work with a simple
    //   "Sender.AddFunction" (because it's overloaded or it' has default
    //   parameters) or they can help for some common tasks.

    // Input / Output
    procedure WriteLn(const Str: string);
    function ReadLn(const aQuestion, DefAnswer: string): string;

    // Strings
    function RPos(const Substr, Source: string): integer;

    function UTF8LowerCase(const AInStr: string): string;
    function UTF8UpperCase(const AInStr: string): string;

    // Dialog forms
    function AskFile(const aTitle, aExt, DefFile: string): string;
    function AskFolder(const aTitle, DefFolder: string): string;

  public
    property ScriptFile: string read getScriptFile write setScriptFile;
    property CommonUnitFolder: string read FCommonUnitFolder write SetCommonUnitFolder;

    property ScriptText: TStrings read getScriptText write setScriptText;
    property ScriptOutput: TStrings read FScriptOutput write SetScriptOutput;
    property ScriptInfo: TStrings read FScriptInfo write SetScriptInfo;
    property ScriptError: TStrings read FScriptError write SetScriptError;

    property GameManager: cEmutecaMainManager read FGameManager write SetGameManager;
    property Game: cEmutecaGame read FGame write SetGame;
    property GameGroup: cEmutecaGameGroup read FGameGroup write SetGameGroup;

    function RunScript: boolean;
    function CompileScript: boolean;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cScriptEngEmuteca }

procedure cScriptEngEmuteca.SetGame(AValue: cEmutecaGame);
begin
  if FGame = AValue then
    Exit;
  FGame := AValue;
end;

function cScriptEngEmuteca.getScriptFile: string;
begin
  Result := PasScript.MainFileName;
end;

function cScriptEngEmuteca.getScriptText: TStrings;
begin
  Result := PasScript.Script;
end;

procedure cScriptEngEmuteca.SetCommonUnitFolder(AValue: string);
begin
  FCommonUnitFolder := SetAsFolder(AValue);
end;

procedure cScriptEngEmuteca.SetGameGroup(AValue: cEmutecaGameGroup);
begin
  if FGameGroup = AValue then
    Exit;
  FGameGroup := AValue;
end;

procedure cScriptEngEmuteca.SetGameManager(AValue: cEmutecaMainManager);
begin
  if FGameManager = AValue then
    Exit;
  FGameManager := AValue;
end;

procedure cScriptEngEmuteca.SetOwnsScriptError(AValue: boolean);
begin
  if FOwnsScriptError = AValue then
    Exit;
  FOwnsScriptError := AValue;
end;

procedure cScriptEngEmuteca.SetOwnsScriptInfo(AValue: boolean);
begin
  if FOwnsScriptInfo = AValue then
    Exit;
  FOwnsScriptInfo := AValue;
end;

procedure cScriptEngEmuteca.SetOwnsScriptOutput(AValue: boolean);
begin
  if FOwnsScriptOutput = AValue then
    Exit;
  FOwnsScriptOutput := AValue;
end;

procedure cScriptEngEmuteca.SetPasScript(AValue: TPSScript);
begin
  if FPasScript = AValue then
    Exit;
  FPasScript := AValue;
end;

procedure cScriptEngEmuteca.SetScriptError(AValue: TStrings);
begin
  if OwnsScriptError then
    FreeAndNil(FScriptError);

  if AValue = nil then
  begin
    FScriptError := TStringList.Create;
    OwnsScriptError := True;
  end
  else
  begin
    FScriptError := AValue;
    OwnsScriptError := False;
  end;
end;

procedure cScriptEngEmuteca.setScriptFile(AValue: string);
begin
  PasScript.MainFileName := SetAsFile(AValue);
end;

procedure cScriptEngEmuteca.SetScriptInfo(AValue: TStrings);
begin
  if OwnsScriptInfo then
    FreeAndNil(FScriptInfo);

  if AValue = nil then
  begin
    FScriptInfo := TStringList.Create;
    OwnsScriptInfo := True;
  end
  else
  begin
    FScriptInfo := AValue;
    OwnsScriptInfo := False;
  end;
end;

procedure cScriptEngEmuteca.SetScriptOutput(AValue: TStrings);
begin
  if OwnsScriptOutput then
    FreeAndNil(FScriptOutput);

  if AValue = nil then
  begin
    FScriptOutput := TStringList.Create;
    OwnsScriptOutput := True;
  end
  else
  begin
    FScriptOutput := AValue;
    OwnsScriptOutput := False;
  end;
end;

procedure cScriptEngEmuteca.SetScriptText(AValue: TStrings);
begin
  PasScript.Script := AValue;
end;

procedure cScriptEngEmuteca.PasScriptOnCompImport(Sender: TObject;
  x: TPSPascalCompiler);
begin
  SIRegister_StrUtils(x);
  RegisterDateTimeLibrary_C(x);
  RegisterDll_Compiletime(x);
  SIRegister_Std(x);
  SIRegister_Classes(x, True);
  SIRegister_Graphics(x, True);
  SIRegister_Controls(x);
  SIRegister_StdCtrls(x);
  SIRegister_ExtCtrls(x);
  SIRegister_Forms(x);
  SIRegister_Buttons(x);
  SIRegister_Menus(x);
  SIRegister_DB(x);

  SIRegister_u7zWrapper(x);
  SIRegister_uEmulator(x);
  SIRegister_uSystem(x);
  SIRegister_uPlayingStats(x);
  SIRegister_uGame(x);
  SIRegister_uGameGroup(x);
  SIRegister_uGameManager(x);
end;

procedure cScriptEngEmuteca.PasScriptOnCompile(Sender: TPSScript);
begin
  // Input and Output
  Sender.AddMethod(Self, @cScriptEngEmuteca.WriteLn,
    'procedure WriteLn(const s: String)');
  Sender.AddMethod(Self, @cScriptEngEmuteca.ReadLn,
    'function ReadLn(const aQuestion, DefAnswer: String): String;');

  // String handling UTF8 from LazUTF8 unit
  Sender.AddFunction(@UTF8CompareText,
    'function UTF8CompareText(const S1, S2: String): Integer;');
  Sender.AddFunction(@UTF8CompareStr,
    'function UTF8CompareStr(const S1, S2: String): Integer;');
  Sender.AddFunction(@UTF8ToSys,
    'function UTF8ToSys(const S: String): String;');
  Sender.AddFunction(@SysToUTF8,
    'function SysToUTF8(const S: String): String;');
  Sender.AddMethod(Self, @cScriptEngEmuteca.UTF8LowerCase,
    'function UTF8LowerCase(const AInStr: String): String;');
  Sender.AddMethod(Self, @cScriptEngEmuteca.UTF8UpperCase,
    'function UTF8UpperCase(const AInStr: String): String;');

  // Misc string functions
  Sender.AddMethod(Self, @cScriptEngEmuteca.RPos,
    'function RPos(const Substr: String; const Source: String) : Integer;');

  // Path and filename strings
  Sender.AddFunction(@CleanFileName,
    'function CleanFileName(const AFileName: String): String;');
  Sender.AddFunction(@ExcludeTrailingPathDelimiter,
    'function ExcludeTrailingPathDelimiter(const aString: String): String;');
  Sender.AddFunction(@ExtractFilePath,
    'function ExtractFilePath(const aFileName: String): String;');
  Sender.AddFunction(@ExtractFileName,
    'function ExtractFileName(const aFileName: String): String;');
  Sender.AddFunction(@ExtractFileNameOnly,
    'function ExtractFileNameOnly(const AFilename: String): String;');
  Sender.AddFunction(@ExtractFileExt,
    'function ExtractFileExt(const AFilename: String): String;');
  Sender.AddFunction(@ChangeFileExt,
    'function ChangeFileExt(const aFileName, aExtension: String): String;');

  // Files and Folders UTF8
  Sender.AddFunction(@FileExistsUTF8,
    'function FileExistsUTF8(const aFileName: String): Boolean;');
  Sender.AddFunction(@DirectoryExistsUTF8,
    'function DirectoryExistsUTF8(const aFileName: String): Boolean;');

  // Dialogs
  Sender.AddMethod(Self, @cScriptEngEmuteca.AskFile,
    'function AskFile(const aTitle, aExt, DefFile: String): String;');
  Sender.AddMethod(Self, @cScriptEngEmuteca.AskFolder,
    'function AskFolder(const aTitle, DefFolder: String): String;');

  // Variables
  Sender.AddRegisteredPTRVariable('GameManager', 'cGameManager');
  Sender.AddRegisteredPTRVariable('Game', 'cGame');
  Sender.AddRegisteredPTRVariable('GameGroup', 'cGameGroup');
end;

procedure cScriptEngEmuteca.PasScriptOnExecute(Sender: TPSScript);
begin
  Sender.SetPointerToData('GameManager', @FGameManager,
    Sender.FindNamedType('cGameManager'));
  Sender.SetPointerToData('Game', @FGame, Sender.FindNamedType('cGame'));
  Sender.SetPointerToData('GameGroup', @FGameGroup,
    Sender.FindNamedType('cGameGroup'));
end;

function cScriptEngEmuteca.PasScriptOnFindUnknownFile(Sender: TObject;
  const OrginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
var
  FullFileName: String;
  f: TFileStream;
begin
  Result := False;
  FullFileName := SetAsFolder(ExtractFilePath(OrginFileName)) + FileName;
  if not FileExistsUTF8(FullFileName) then
  begin
    FullFileName := SetAsFolder(CommonUnitFolder) + FileName;
    if not FileExistsUTF8(FullFileName) then
      Exit;
  end;

  try
    f := TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;
  try
    SetLength(Output, f.Size);
    f.Read(Output[1], Length(Output));
  finally
    f.Free;
  end;
  Result := True;
end;

function cScriptEngEmuteca.PasScriptOnNeedFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
var
  FullFileName: String;
  F: TFileStream;
begin
  ShowMessage('PSScriptNeedFile:' + sLineBreak +
  'OriginFileName: ' +OriginFileName +  sLineBreak +
  'FileName: ' +FileName +  sLineBreak +
  'Output: ' +Output +  sLineBreak
  );


  Result := False;
  FullFileName := SetAsFolder(ExtractFilePath(OriginFileName)) + FileName;
  if not FileExistsUTF8(FullFileName) then
  begin
    FullFileName := SetAsFolder(CommonUnitFolder) + FileName;
    if not FileExistsUTF8(FullFileName) then
      Exit;
  end;

  try
    F := TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;
  try
    SetLength(Output, f.Size);
    f.Read(Output[1], Length(Output));
  finally
    f.Free;
  end;
  Result := True;
end;

procedure cScriptEngEmuteca.PasScriptOnExecImport(Sender: TObject;
  se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_StrUtils_Routines(se);
  RegisterDateTimeLibrary_R(se);
  RegisterDLLRuntime(se);
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_ExtCtrls(x);
  RIRegister_Forms(x);
  RIRegister_Buttons(x);
  RIRegister_Menus(x);
  RIRegister_DB(x);

  RIRegister_u7zWrapper_Routines(se);
  RIRegister_uEmulator(x);
  RIRegister_uSystem(x);
  RIRegister_uPlayingStats(x);
  RIRegister_uGame(x);
  RIRegister_uGameGroup(x);
  RIRegister_uGameManager(x);
end;

procedure cScriptEngEmuteca.WriteLn(const Str: string);
begin
  ScriptOutput.Add(Str);
end;

function cScriptEngEmuteca.ReadLn(const aQuestion, DefAnswer: string): string;
begin
  Result := InputBox(Application.Title, aQuestion, DefAnswer);
end;

function cScriptEngEmuteca.RPos(const Substr, Source: string): integer;
begin
  Result := strutils.RPos(Substr, Source);
end;

function cScriptEngEmuteca.UTF8LowerCase(const AInStr: string): string;
begin
  Result := LazUTF8.UTF8LowerCase(AInStr, '');
end;

function cScriptEngEmuteca.UTF8UpperCase(const AInStr: string): string;
begin
  Result := LazUTF8.UTF8UpperCase(AInStr, '');
end;

function cScriptEngEmuteca.AskFile(
  const aTitle, aExt, DefFile: string): string;
begin
  Result := '';
  Application.CreateForm(TfrmSMAskFile, frmSMAskFile);
  try
    frmSMAskFile.lTitle.Caption := aTitle;
    frmSMAskFile.eFileName.DialogTitle := aTitle;
    frmSMAskFile.eFileName.Filter := aExt;
    frmSMAskFile.eFileName.FileName := DefFile;
    if frmSMAskFile.ShowModal = mrOk then
      Result := frmSMAskFile.eFileName.FileName;
  finally
    FreeAndNil(frmSMAskFile);
  end;
end;

function cScriptEngEmuteca.AskFolder(const aTitle, DefFolder: string): string;
begin
  Result := '';
  Application.CreateForm(TfrmSMAskFolder, frmSMAskFolder);
  try
    frmSMAskFolder.lTitle.Caption := aTitle;
    frmSMAskFolder.eDirectory.DialogTitle := aTitle;
    frmSMAskFolder.eDirectory.Directory := DefFolder;
    if frmSMAskFolder.ShowModal = mrOk then
      Result := SetAsFolder(frmSMAskFolder.eDirectory.Directory);
  finally
    FreeAndNil(frmSMAskFolder);
  end;
end;

function cScriptEngEmuteca.RunScript: boolean;
begin
  if not CompileScript then
    Exit;

  Result := PasScript.Execute;
  if not Result then
  begin
    ScriptError.BeginUpdate;
    ScriptError.Add(Format(rsSEEExecutionMsg, [rsSEEError]));
    ScriptError.Add(PasScript.ExecErrorToString);
    ScriptError.EndUpdate;
    Exit;
  end;

  ScriptError.Add(Format(rsSEEExecutionMsg, [rsSEEOK]));
end;

function cScriptEngEmuteca.CompileScript: boolean;
var
  i: integer;
begin
  ScriptError.Clear;

  Result := PaSScript.Compile;

  if not Result then
  begin
    ScriptError.BeginUpdate;
    ScriptError.Add(Format(rsSEECompilationMsg, [rsSEEError]));
    for i := 0 to PaSScript.CompilerMessageCount - 1 do
    begin
      ScriptError.Add(PasScript.CompilerErrorToStr(i));
    end;
    ScriptError.EndUpdate;
    Exit;
  end;
  ScriptError.Add(Format(rsSEECompilationMsg, [rsSEEOK]));
end;

constructor cScriptEngEmuteca.Create;
begin
  inherited Create;

  // This assigments autocreates the owned TStringList
  ScriptOutput := nil;
  ScriptInfo := nil;
  ScriptError := nil;

  FPasScript := TPSScript.Create(nil);
  PasScript.OnCompImport := @PasScriptOnCompImport;
  PasScript.OnCompile := @PasScriptOnCompile;
  PasScript.OnExecImport := @PasScriptOnExecImport;
  PasScript.OnExecute := @PasScriptOnExecute;
  PasScript.OnNeedFile := @PasScriptOnNeedFile;
  PasScript.OnFindUnknownFile := @PasScriptOnFindUnknownFile;

end;

destructor cScriptEngEmuteca.Destroy;
begin
  if OwnsScriptError then
    FreeAndNil(FScriptError);
  if OwnsScriptInfo then
    FreeAndNil(FScriptInfo);
  if OwnsScriptOutput then
    FreeAndNil(FScriptOutput);
  FreeAndNil(FPasScript);
  inherited Destroy;
end;

end.
