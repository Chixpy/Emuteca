{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

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

{ Unit of Script Manager form. }
unit fScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, ShellCtrls, StdCtrls, ActnList, Buttons, StdActns,
  strutils, SynHighlighterPas, SynMemo, LazUTF8,
  fSMAskFile, fSMAskFolder,
  uPSComponent, uPSComponent_Default, uPSComponent_Controls, uPSUtils,
  uPSComponent_Forms, uPSComponent_StdCtrls, uPSC_strutils,
  uPSI_uGame,  uPSI_uGameGroup,
  uPSI_uGameManager, uPSI_uGameStats, uPSI_u7zWrapper, uPSI_uEmulator,
  uPSI_uSystem,
  uConfig, uCustomUtils, uGameManager;

resourcestring
      rsCompilationOK = 'Compilation: OK.';
      rsCompilationError = 'Compilation: Error.';
      rsExecutionOK = 'Execution: OK.';
      rsExecutionError = 'Execution: Error.';
      rsScriptFileSaved = 'Script file saved: %s';

const
  // Script file extension
  kFSMScriptExt = '.pas';
  kFSMScriptFilter = 'Emuteca Script (*' + kFSMScriptExt + ')|*' +
    kFSMScriptExt + '|All files|*.*';
type

  TSMLoadingListCallBack = function(const Game, Version: String;
    const Max, Value: Int64): Boolean of object;

  { TfrmScriptManager }

  TfrmScriptManager = class(TForm)
    actExecute: TAction;
    actCompile: TAction;
    actSaveAs: TAction;
    actSaveScript: TAction;
    ActionList: TActionList;
    bExecute: TBitBtn;
    bSave: TBitBtn;
    bSaveAs: TBitBtn;
    cCompile: TBitBtn;
    chkReadOnly: TCheckBox;
    actEditCopy: TEditCopy;
    actEditCut: TEditCut;
    actEditPaste: TEditPaste;
    gbxFile: TGroupBox;
    gbRun: TGroupBox;
    ilActions: TImageList;
    lvScripts: TShellListView;
    mCompMess: TMemo;
    mOutput: TMemo;
    PageControl: TPageControl;
    PSDllPlugin: TPSDllPlugin;
    PSImport_Forms: TPSImport_Forms;
    PSImport_StdCtrls: TPSImport_StdCtrls;
    PSImport_StrUtils: TPSImport_StrUtils;
    pTop: TPanel;
    pButtons: TPanel;
    pInfo: TPanel;
    PSImport_Classes: TPSImport_Classes;
    PSImport_Controls: TPSImport_Controls;
    PSImport_DateUtils: TPSImport_DateUtils;
    PSScript: TPSScript;
    SaveDialog: TSaveDialog;
    sbInfo: TStatusBar;
    pagScriptList: TTabSheet;
    pagSourceCode: TTabSheet;
    SynFreePascalSyn: TSynFreePascalSyn;
    SynMemo: TSynMemo;
    pagOutput: TTabSheet;
    ToolBarEditor: TToolBar;
    bEditCopy: TToolButton;
    bEditCut: TToolButton;
    bEditPaste: TToolButton;
    procedure actCompileExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveScriptExecute(Sender: TObject);
    procedure chkReadOnlyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvScriptsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure PSScriptCompile(Sender: TPSScript);
    procedure PSScriptExecute(Sender: TPSScript);
    function PSScriptNeedFile(Sender: TObject; const OriginFileName: tbtstring;
      var FileName, Output: tbtstring): Boolean;

  private
    { private declarations }
    FConfig: cConfig;
    FGameManager: cGameManager;
    procedure SetConfig(const AValue: cConfig);
    procedure SetGameManager(const AValue: cGameManager);

  protected
    // Added functions
    // ---------------
    // This functions are those which don't work with a simple
    //   "Sender.AddFunction" (Overloaded or default parameters) or
    //   they can help for some tasks.

    // Input / Output
    procedure WriteLn(const Str: String);
    function ReadLn(const aQuestion, DefAnswer: String): String;

    // Strings
    function RPos(const Substr, Source: String): Integer;

    function UTF8LowerCase(const AInStr: String): String;
    function UTF8UpperCase(const AInStr: String): String;

    // Dialog forms
    function AskFile(const aTitle, aExt, DefFile: String): String;
    function AskFolder(const aTitle, DefFolder: String): String;

    function Compile: Boolean;

  public
    { public declarations }
    property Config: cConfig read FConfig write SetConfig;
    property GameManager: cGameManager read FGameManager write SetGameManager;
  end;

var
  frmScriptManager: TfrmScriptManager;

implementation

{ TfrmScriptManager }

procedure TfrmScriptManager.PSScriptCompile(Sender: TPSScript);
begin
  // Input and Output
  Sender.AddMethod(Self, @TfrmScriptManager.WriteLn,
    'procedure WriteLn(const s: String)');
  Sender.AddMethod(Self, @TfrmScriptManager.ReadLn,
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
  Sender.AddMethod(Self, @TfrmScriptManager.UTF8LowerCase,
    'function UTF8LowerCase(const AInStr: String): String;');
  Sender.AddMethod(Self, @TfrmScriptManager.UTF8UpperCase,
    'function UTF8UpperCase(const AInStr: String): String;');

  // Misc string functions
  Sender.AddMethod(Self, @TfrmScriptManager.RPos,
    'function RPos (const Substr: String; const Source: String) : Integer;');

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
  Sender.AddMethod(Self, @TfrmScriptManager.AskFile,
    'function AskFile(const aTitle, aExt, DefFile: String): String;');
  Sender.AddMethod(Self, @TfrmScriptManager.AskFolder,
    'function AskFolder(const aTitle, DefFolder: String): String;');

  // Variables
  Sender.AddRegisteredPTRVariable('GameManager', 'cGameManager');
end;

procedure TfrmScriptManager.PSScriptExecute(Sender: TPSScript);
begin
  PSScript.SetPointerToData('GameManager', @FGameManager,
    PSScript.FindNamedType('cGameManager'));
end;

function TfrmScriptManager.PSScriptNeedFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
var
  aFile: TStringList;
  FullFileName: String;
begin
  Result := False;
  FullFileName := SetAsFolder(ExtractFilePath(OriginFileName)) + FileName;
  if not FileExistsUTF8(FullFileName) then
  begin
    FullFileName := SetAsFolder(Config.ScriptsFolder + 'Common') + FileName;
    if not FileExistsUTF8(FullFileName) then
      Exit;
  end;

  aFile := TStringList.Create;
  try
    aFile.LoadFromFile(UTF8ToSys(FullFileName));
    Output := aFile.Text;
    Result := True
  finally
    FreeAndNil(aFile);
  end;
end;

procedure TfrmScriptManager.actExecuteExecute(Sender: TObject);
var
  Compiled: Boolean;
  Executed: Boolean;
begin
  PageControl.ActivePage := pagOutput;
  mOutput.Clear;

  Compiled := Compile();

  if not Compiled then
  begin
    mCompMess.Lines.Add(rsCompilationError);
    Exit;
  end;

  Executed := PSScript.Execute;

  if not Executed then
  begin
    mCompMess.Lines.Add(rsExecutionError);
    mCompMess.Lines.Add(PSScript.ExecErrorToString);
    Exit;
  end;

  mCompMess.Lines.Add(rsExecutionOK);
end;

procedure TfrmScriptManager.actSaveAsExecute(Sender: TObject);
var
  aFile: String;
begin
  SaveDialog.InitialDir := lvScripts.Root;
  SaveDialog.DefaultExt := kFSMScriptExt;
  SaveDialog.Filter := kFSMScriptFilter;
  if not SaveDialog.Execute then
    Exit;

  aFile := SaveDialog.FileName;
  SynMemo.Lines.SaveToFile(aFile);
  mCompMess.Lines.Add(Format(rsScriptFileSaved, [aFile]));
  if aFile <> PSScript.MainFileName then
    lvScripts.Selected := nil;
  PSScript.MainFileName := aFile;
  lvScripts.Update;
  if SynMemo.CanFocus then
    SynMemo.SetFocus;
end;

procedure TfrmScriptManager.actSaveScriptExecute(Sender: TObject);
begin
  SynMemo.Lines.SaveToFile(PSScript.MainFileName);
  mCompMess.Lines.Add(Format(rsScriptFileSaved, [PSScript.MainFileName]));
  if SynMemo.CanFocus then
    SynMemo.SetFocus;
end;

procedure TfrmScriptManager.actCompileExecute(Sender: TObject);
begin
  Compile();
end;

procedure TfrmScriptManager.chkReadOnlyChange(Sender: TObject);
begin
  SynMemo.ReadOnly := chkReadOnly.Checked;
  if SynMemo.CanFocus then
    SynMemo.SetFocus;
end;

procedure TfrmScriptManager.FormCreate(Sender: TObject);
var
  Plugin: TPSPlugin;
begin
  // Preparamos las unidades que se pueden usar...
  Plugin := TPSImport_uGameStats.Create(Self);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := Plugin;
  Plugin := TPSImport_uEmulator.Create(Self);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := Plugin;
  Plugin := TPSImport_uSystem.Create(Self);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := Plugin;

  Plugin := TPSImport_uGame.Create(Self);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := Plugin;
  Plugin := TPSImport_uGameGroup.Create(Self);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := Plugin;
  Plugin := TPSImport_uGameManager.Create(Self);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := Plugin;
  Plugin := TPSImport_u7zWrapper.Create(Self);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := Plugin;

  PageControl.ActivePageIndex := 0;
end;

procedure TfrmScriptManager.lvScriptsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  aPos: Longint;
begin
  // This metod is called when deselecting something too
  mCompMess.Clear;
  chkReadOnly.Checked := True;
  SynMemo.ReadOnly := True;
  PSScript.MainFileName := '';
  if not Selected then
    Exit;

  PSScript.MainFileName := SetAsFolder(lvScripts.Root) + Item.Caption;

  SynMemo.Lines.LoadFromFile(PSScript.MainFileName);

  // Removing UTF-8 BOM...
  if SynMemo.Lines.Count > 0 then
    aPos := Pos(UTF8FileHeader, SynMemo.Lines[0]);
  if aPos = 1 then
    SynMemo.Lines[0] := Copy(SynMemo.Lines[0], Length(UTF8FileHeader) +
      1, MaxInt);
end;

procedure TfrmScriptManager.SetConfig(const AValue: cConfig);

  procedure Translate;
begin
      Self.Caption := Application.Title + ': ' + Self.Caption;
  end;

begin
  FConfig := AValue;

  Translate;

  // Iconos de las acciones
  ReadActionsIcons(Config.IconsIniFile, Self.Name, Config.ImagesFolder +
    Config.IconsSubfolder, ilActions, ActionList);

  lvScripts.Root := Config.ScriptsFolder + Config.GeneralScriptsSubFolder;
  lvScripts.Update;
end;

procedure TfrmScriptManager.SetGameManager(const AValue: cGameManager);
begin
  if FGameManager = AValue then
    exit;
  FGameManager := AValue;

  pTop.Caption := GameManager.System.ID;
end;

procedure TfrmScriptManager.WriteLn(const Str: String);
begin
  mOutput.Lines.Add(Str);
end;

function TfrmScriptManager.ReadLn(const aQuestion, DefAnswer: String): String;
begin
  Result := InputBox('Emuteca', aQuestion, DefAnswer);
end;

function TfrmScriptManager.RPos(const Substr, Source: String): Integer;
begin
  Result := strutils.RPos(Substr, Source);
end;

function TfrmScriptManager.UTF8LowerCase(const AInStr: String): String;
begin
  Result := LazUTF8.UTF8LowerCase(AInStr, '');
end;

function TfrmScriptManager.UTF8UpperCase(const AInStr: String): String;
begin
  Result := LazUTF8.UTF8UpperCase(AInStr, '');
end;

function TfrmScriptManager.Compile: Boolean;
var
  i: Integer;
begin
  PSScript.Script.Clear;
  mCompMess.Clear;

  PSScript.Script.AddStrings(SynMemo.Lines);
  Result := PSScript.Compile;

  if Result then
    mCompMess.Lines.Add(rsCompilationOK)
  else
  begin
    for i := 0 to PSScript.CompilerMessageCount - 1 do
    begin
      mCompMess.Lines.BeginUpdate;
      mCompMess.Lines.Add(PSScript.CompilerErrorToStr(i));
      mCompMess.Lines.EndUpdate;
    end;
    PageControl.ActivePage := pagSourceCode;
    if PSScript.CompilerMessages[i].Col > 0 then
      SynMemo.CaretX := PSScript.CompilerMessages[i].Col;
    if PSScript.CompilerMessages[i].Row > 0 then
      SynMemo.CaretY := PSScript.CompilerMessages[i].Row;
    SynMemo.SetFocus;
  end;
end;

function TfrmScriptManager.AskFile(const aTitle, aExt, DefFile: String): String;
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

function TfrmScriptManager.AskFolder(const aTitle,
  DefFolder: String): String;
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

initialization
  {$I fScriptManager.lrs}

end.

