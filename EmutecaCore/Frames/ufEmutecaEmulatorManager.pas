{ This file is part of Emuteca

  Copyright (C) 2006-2016 Chixpy

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

{ TfmEmutecaEmulatorManager unit. }
unit ufEmutecaEmulatorManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, CheckLst, Menus, ComCtrls,
  ExtCtrls, ActnList, Dialogs, Buttons, StdCtrls,
  uCHXImageUtils,
  ufEmutecaEmulatorEditor,
  ucEmutecaEmulatorManager, ucEmutecaEmulator;

resourcestring
  rsEmulatorName = 'Emulator name';

type

  { TfmEmutecaEmulatorManager }

  TfmEmutecaEmulatorManager = class(TFrame)
    actAddItem: TAction;
    actCancelAndExit: TAction;
    actDeleteItem: TAction;
    actExportList: TAction;
    actImportList: TAction;
    actCheckAll: TAction;
    actUncheckAll: TAction;
    actSaveAndExit: TAction;
    actSaveList: TAction;
    ActionList1: TActionList;
    bCancel: TBitBtn;
    bSave: TBitBtn;
    CheckListBox1: TCheckListBox;
    ilActions: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    pBottom: TPanel;
    pLeft: TPanel;
    pmItemList: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure actAddItemExecute(Sender: TObject);
    procedure actCancelAndExitExecute(Sender: TObject);
    procedure actCheckAllExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actSaveAndExitExecute(Sender: TObject);
    procedure actSaveListExecute(Sender: TObject);
    procedure actUncheckAllExecute(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);

  private
    FEmuEditor: TfmEmutecaEmulatorEditor;
    FEmuManager: cEmutecaEmulatorManager;
    FIconsIni: string;
    procedure SetEmuEditor(AValue: TfmEmutecaEmulatorEditor);
    procedure SetEmuManager(AValue: cEmutecaEmulatorManager);
    procedure SetIconsIni(AValue: string);


  protected
    property EmuEditor: TfmEmutecaEmulatorEditor
      read FEmuEditor write SetEmuEditor;

    procedure LoadList;
    procedure SelectItem;
    procedure SaveToFile;
    procedure LoadFromFile;

  public
    { public declarations }
    property IconsIni: string read FIconsIni write SetIconsIni;
    property EmuManager: cEmutecaEmulatorManager
      read FEmuManager write SetEmuManager;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaEmulatorManager }

procedure TfmEmutecaEmulatorManager.SetEmuManager(
  AValue: cEmutecaEmulatorManager);
begin
  FEmuManager := AValue;
  LoadList;
end;

procedure TfmEmutecaEmulatorManager.SetIconsIni(AValue: string);
begin
  if FIconsIni = AValue then
    Exit;
  FIconsIni := AValue;

  if IconsIni <> '' then
    ReadActionsIcons(IconsIni, Self.Name, '', ilActions, ActionList1);
end;

procedure TfmEmutecaEmulatorManager.actAddItemExecute(Sender: TObject);
var
  EmulatorID: string;
  aEmulator: cEmutecaEmulator;
begin
  EmulatorID := Trim(InputBox(actAddItem.Caption, rsEmulatorName, ''));
  if EmulatorID = '' then
    Exit;

  // Duplicates are not added :-P
  aEmulator := EmuManager.Add(EmulatorID);
  aEmulator.Enabled := True;

  // If aEmulator.ID <> EmulatorID then
  EmulatorID := aEmulator.ID;
  LoadList;

  // TODO: Autoselecting last item.
 {
  CheckListBox1.ItemIndex := CheckListBox1.Items.IndexOf(EmulatorID);
  SelectItem;
  }
end;

procedure TfmEmutecaEmulatorManager.actCancelAndExitExecute(Sender: TObject);
begin
  // Restoring data from file
  LoadFromFile;
end;

procedure TfmEmutecaEmulatorManager.actCheckAllExecute(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  while i < EmuManager.FullList.Count do
  begin
    EmuManager.FullList[i].Enabled := True;
    Inc(i);
  end;
  LoadList;
end;

procedure TfmEmutecaEmulatorManager.actDeleteItemExecute(Sender: TObject);
begin
  if CheckListBox1.ItemIndex = -1 then
    exit;
  EmuManager.FullList.Remove(cEmutecaEmulator(
    CheckListBox1.Items.Objects[CheckListBox1.ItemIndex]));
  LoadList;
end;

procedure TfmEmutecaEmulatorManager.actSaveAndExitExecute(Sender: TObject);
begin
  SaveToFile;
end;

procedure TfmEmutecaEmulatorManager.actSaveListExecute(Sender: TObject);
begin
  SaveToFile;
end;

procedure TfmEmutecaEmulatorManager.actUncheckAllExecute(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  while i < EmuManager.FullList.Count do
  begin
    EmuManager.FullList[i].Enabled := False;
    Inc(i);
  end;
  LoadList;
end;

procedure TfmEmutecaEmulatorManager.CheckListBox1Click(Sender: TObject);
begin
  // TODO 3: Until TCheckListBox.OnItemClick works as expected...
  // http://www.lazarus.freepascal.org/index.php?topic=12319.0
  SelectItem;
end;

procedure TfmEmutecaEmulatorManager.CheckListBox1ClickCheck(Sender: TObject);
var
  CurrItem: cEmutecaEmulator;
begin
  CurrItem := cEmutecaEmulator(
    CheckListBox1.Items.Objects[CheckListBox1.ItemIndex]);

  CurrItem.Enabled := CheckListBox1.Checked[CheckListBox1.ItemIndex];
end;

procedure TfmEmutecaEmulatorManager.SetEmuEditor(
  AValue: TfmEmutecaEmulatorEditor);
begin
  if FEmuEditor = AValue then
    Exit;
  FEmuEditor := AValue;
end;

procedure TfmEmutecaEmulatorManager.LoadList;
var
  i: integer;
begin
  EmuEditor.Emulator := nil;

  CheckListBox1.Clear;
  if not assigned(EmuManager) then
    exit;

  i := 0;
  while i < EmuManager.FullList.Count do
  begin
    // Dragons
    CheckListBox1.Checked[
      CheckListBox1.Items.AddObject(EmuManager.FullList[i].EmulatorName,
      EmuManager.FullList[i])
      ] := EmuManager.FullList[i].Enabled;
    Inc(i);
  end;
end;

procedure TfmEmutecaEmulatorManager.SelectItem;
begin
  if CheckListBox1.ItemIndex = -1 then
    EmuEditor.Emulator := nil
  else
    EmuEditor.Emulator := cEmutecaEmulator(
      CheckListBox1.Items.Objects[CheckListBox1.ItemIndex]);
end;

procedure TfmEmutecaEmulatorManager.SaveToFile;
begin
  EmuManager.SaveToFile('', False);
end;

procedure TfmEmutecaEmulatorManager.LoadFromFile;
begin
  EmuManager.LoadFromFile('');
end;

constructor TfmEmutecaEmulatorManager.Create(TheOwner: TComponent);

  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := PageControl1.AddTabSheet;
    FEmuEditor := TfmEmutecaEmulatorEditor.Create(aTabSheet);
    EmuEditor.Parent := aTabSheet;
    EmuEditor.Align := alClient;

    aTabSheet := PageControl1.AddTabSheet;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;
end;

destructor TfmEmutecaEmulatorManager.Destroy;
begin
  inherited Destroy;
end;

end.
