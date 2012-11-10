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

{ Unit of Script Manager form. }
unit fScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ActnList, Controls, ComCtrls, ExtCtrls, LResources,
  StdCtrls, ShellCtrls, FileUtil, Dialogs, StdActns, Buttons, IniFiles,
  SynHighlighterPas, SynEdit, SynMacroRecorder,
  // Common
  uRscStr, uConst,
  // Emuteca
  uEmutecaScriptEngine, uEmutecaGameManager, uEmutecaGame, uEmutecaGroup,
   // Custom
  uConfig, uCHXStrUtils, uCHXImageUtils;

type

  TSMLoadingListCallBack = function(const Game, Version: string;
    const Max, Value: int64): boolean of object;

  { TfrmScriptManager }

  TfrmScriptManager = class(TForm)
    actCompile: TAction;
    actExecute: TAction;
    ActionList: TActionList;
    actEditCopy: TEditCopy;
    actEditCut: TEditCut;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    actEditDelete: TEditDelete;
    bExecute2: TBitBtn;
    actFileSaveAs: TFileSaveAs;
    gbxScript: TGroupBox;
    ilActions: TImageList;
    lGame: TLabel;
    lGroup: TLabel;
    lSystem: TLabel;
    mInfo: TMemo;
    mOutPut: TMemo;
    PageControl: TPageControl;
    pCurrentData: TPanel;
    pRight: TPanel;
    pInfo: TPanel;
    sbInfo: TStatusBar;
    pagGeneralScriptList: TTabSheet;
    pagSourceCode: TTabSheet;
    pagOutput: TTabSheet;
    pagGameScripts: TTabSheet;
    pagGroupScripts: TTabSheet;
    pagCommonScripts: TTabSheet;
    actSearchFind: TSearchFind;
    actSearchReplace: TSearchReplace;
    slvGeneral: TShellListView;
    slvGame: TShellListView;
    slvCommon: TShellListView;
    slvGroup: TShellListView;
    SynEdit: TSynEdit;
    SynFreePascalSyn: TSynFreePascalSyn;
    SynMacroRecorder: TSynMacroRecorder;
    tbEditor: TToolBar;
    bEditCopy: TToolButton;
    bEditCut: TToolButton;
    bEditDelete: TToolButton;
    bEditPaste: TToolButton;
    bSeparator1: TToolButton;
    bEditSelectAll: TToolButton;
    bSeparator2: TToolButton;
    bEditUndo: TToolButton;
    bSeparator3: TToolButton;
    bSearchFind: TToolButton;
    bSearchReplace: TToolButton;
    bSeparator4: TToolButton;
    bSaveFileAs: TToolButton;
    bSeparator5: TToolButton;
    bCompile: TToolButton;
    bExecute: TToolButton;
    procedure actCompileExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actFileSaveAsAccept(Sender: TObject);
    procedure actFileSaveAsBeforeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure slvSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
  private
    { private declarations }
    FConfig: cConfig;
    FCurrentFile: string;
    FCurrGame: cGame;
    FCurrGroup: cGameGroup;
    FGameManager: cGameManager;
    FScriptEngine: cScriptEngEmuteca;
    FScriptFolder: string;
    procedure SetConfig(const AValue: cConfig);
    procedure SetCurrentFile(AValue: string);
    procedure SetCurrGame(AValue: cGame);
    procedure SetCurrGroup(AValue: cGameGroup);
    procedure SetGameManager(const AValue: cGameManager);
    procedure SetScriptEngine(AValue: cScriptEngEmuteca);
    procedure SetScriptFolder(AValue: string);

  protected
    property CurrentFile: string read FCurrentFile write SetCurrentFile;
    property ScriptEngine: cScriptEngEmuteca
      read FScriptEngine write SetScriptEngine;

    procedure LoadScriptFile(const aFile: string);

    function Compile: boolean;
    function Execute: boolean;

    procedure UpdateSLV;

  public
    { public declarations }
    property Config: cConfig read FConfig write SetConfig;
    property GameManager: cGameManager read FGameManager write SetGameManager;
    property CurrGame: cGame read FCurrGame write SetCurrGame;
    property CurrGroup: cGameGroup read FCurrGroup write SetCurrGroup;
  end;

var
  frmScriptManager: TfrmScriptManager;

implementation

{ TfrmScriptManager }

procedure TfrmScriptManager.slvSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
var
  aSLV: TShellListView;
begin
  if not (Sender is TShellListView) then
    Exit;
  aSLV := TShellListView(Sender);

  if Selected then
    LoadScriptFile(SetAsFolder(aSLV.Root) + Item.Caption)
  else
    LoadScriptFile('');
end;

procedure TfrmScriptManager.actCompileExecute(Sender: TObject);
begin
  Compile;
end;

procedure TfrmScriptManager.actExecuteExecute(Sender: TObject);
begin
  Execute;
end;

procedure TfrmScriptManager.actFileSaveAsAccept(Sender: TObject);
begin
  SynEdit.Lines.SaveToFile(actFileSaveAs.Dialog.FileName);

  mInfo.Lines.Add(Format(rsFSMScriptFileSaved, [actFileSaveAs.Dialog.FileName]));
  CurrentFile := actFileSaveAs.Dialog.FileName;
  UpdateSLV;
  if SynEdit.CanFocus then
    SynEdit.SetFocus;
end;

procedure TfrmScriptManager.actFileSaveAsBeforeExecute(Sender: TObject);
begin
  actFileSaveAs.Dialog.InitialDir := ExtractFileDir(CurrentFile);
  actFileSaveAs.Dialog.FileName := ExtractFileName(CurrentFile);

  actFileSaveAs.Dialog.Filter := rsFileMaskScriptDescription + '|' + kFileMaskScript
  + '|' + rsFileMaskAllFilesDescription + '|' + kFileMaskAllFiles;
  actFileSaveAs.Dialog.DefaultExt := kFileExtensionScript;
end;

procedure TfrmScriptManager.FormCreate(Sender: TObject);
begin
  FScriptEngine := cScriptEngEmuteca.Create;
end;

procedure TfrmScriptManager.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScriptEngine);
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
  ReadActionsIcons(Config.ImagesFolder + Config.IconsSubfolder + Config.IconsIniFile, Self.Name, '', ilActions, ActionList);

  PageControl.ActivePageIndex := 0;

  // Listing Scripts...
  slvGeneral.Root := Config.ScriptsFolder + Config.GeneralScriptsSubFolder;
  slvGeneral.Update;
  slvGroup.Root := Config.ScriptsFolder + Config.GroupScriptsSubFolder;
  slvGroup.Update;
  slvGame.Root := Config.ScriptsFolder + Config.GameScriptsSubFolder;
  slvGame.Update;
  slvCommon.Root := Config.ScriptsFolder + kFSMUnitsFolder;
  slvCommon.Update;

  ScriptEngine.CommonUnitFolder := slvCommon.Root;
end;

procedure TfrmScriptManager.SetCurrentFile(AValue: string);
begin
  if FCurrentFile = AValue then
    Exit;
  FCurrentFile := AValue;
end;

procedure TfrmScriptManager.SetCurrGame(AValue: cGame);
begin
  FCurrGame := AValue;
  ScriptEngine.Game := AValue;

  if AValue <> nil then
    lGame.Caption := format(rsFSMCurrentGame,
      [AValue.Name, SetAsFolder(AValue.Folder) + AValue.FileName])
  else
    lGame.Caption := ' ';
end;

procedure TfrmScriptManager.SetCurrGroup(AValue: cGameGroup);
begin
  FCurrGroup := AValue;
  ScriptEngine.GameGroup := AValue;

  if AValue <> nil then
    lGroup.Caption := format(rsFSMCurrentGroup, [AValue.Name, AValue.Key] )
  else
    lGroup.Caption := ' ';
end;

procedure TfrmScriptManager.SetGameManager(const AValue: cGameManager);
begin
  FGameManager := AValue;
  ScriptEngine.GameManager := AValue;

  if AValue <> nil then
    lSystem.Caption := format(rsFSMCurrentSystem, [AValue.System.ID])
  else
    lSystem.Caption := ' ';

end;

procedure TfrmScriptManager.SetScriptEngine(AValue: cScriptEngEmuteca);
begin
  if FScriptEngine = AValue then
    Exit;
  FScriptEngine := AValue;
end;

procedure TfrmScriptManager.SetScriptFolder(AValue: string);
begin
  if FScriptFolder = AValue then
    Exit;
  FScriptFolder := AValue;
end;

procedure TfrmScriptManager.LoadScriptFile(const aFile: string);
var
  i: SizeInt;
  aIni: TIniFile;
begin
  if SynEdit.Modified then
    if MessageDlg(Format(rsFSMSaveChanges, [CurrentFile]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      SynEdit.Lines.SaveToFile(CurrentFile);

  CurrentFile := aFile;

  if not FileExistsUTF8(CurrentFile) then
  begin
    SynEdit.Lines.Clear;
    Exit;
  end;

  SynEdit.Lines.LoadFromFile(CurrentFile);

  // Removing UTF-8 BOM...
  if SynEdit.Lines.Count > 0 then
    i := Pos(UTF8FileHeader, SynEdit.Lines[0]);
  if i = 1 then
    SynEdit.Lines[0] := Copy(SynEdit.Lines[0], Length(UTF8FileHeader) +
      1, MaxInt);

  mInfo.Clear;

  // TODO 2: Temporal script info until section is parsed properly.
  aIni := TIniFile.Create(CurrentFile, True);
  try
    aIni.ReadSectionRaw(kFSMDataSection, mInfo.Lines);
  finally
    FreeAndNil(aIni);
  end;
end;

function TfrmScriptManager.Compile: boolean;
begin
  ScriptEngine.ScriptText := SynEdit.Lines;

  // TODO 4: Put this in a better place near ScriptEngine creation.
  //   But search why a SIGEVN error is raised...
  //   ScriptEngine.ScriptOutput, ScriptEngine.ScriptInfo and
  //   ScriptEngine.ScriptError change to nil... When and Where?
  ScriptEngine.ScriptOutput := mOutPut.Lines;
  ScriptEngine.ScriptInfo := mInfo.Lines;
  ScriptEngine.ScriptError := mInfo.Lines;

  Result := ScriptEngine.CompileScript;
end;

function TfrmScriptManager.Execute: boolean;
begin
  PageControl.ActivePage := pagOutput;

  Result := Compile;

  if not Result then
    Exit;

  Result := ScriptEngine.RunScript;
end;

procedure TfrmScriptManager.UpdateSLV;
begin
  slvGeneral.Update;
  slvGame.Update;
  slvGroup.Update;
  slvCommon.Update;
end;

initialization
  {$I fScriptManager.lrs}

end.
