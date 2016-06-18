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


unit ufEmutecaSystemManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Menus, ComCtrls, ExtCtrls,
  CheckLst, ActnList, Buttons, Dialogs, StdCtrls,
  uCHXImageUtils,
  ucEmutecaSystemManager, ucEmutecaSystem, ucEmutecaEmulatorManager,
  ufEmutecaSystemEditor;

resourcestring
  rsSystemName = 'System name';

type

  { TfmEmutecaSystemManager }

  TfmEmutecaSystemManager = class(TFrame)
    actAddItem: TAction;
    actDeleteItem: TAction;
    actExportList: TAction;
    actImportList: TAction;
    actCancelAndExit: TAction;
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
    Panel1: TPanel;
    pBottom: TPanel;
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
    FEmuManager: cEmutecaEmulatorManager;
    FIconsIni: string;
    FSysEditor: TfmEmutecaSystemEditor;
    FSysManager: cEmutecaSystemManager;
    procedure SetEmuManager(AValue: cEmutecaEmulatorManager);
    procedure SetIconsIni(AValue: string);
    procedure SetSysEditor(AValue: TfmEmutecaSystemEditor);
    procedure SetSysManager(AValue: cEmutecaSystemManager);

  protected
    property SysEditor: TfmEmutecaSystemEditor
      read FSysEditor write SetSysEditor;

    procedure LoadList;
    procedure SelectItem;
    procedure SaveToFile;
    procedure LoadFromFile;

  public
    { public declarations }
    property IconsIni: string read FIconsIni write SetIconsIni;
    property SysManager: cEmutecaSystemManager
      read FSysManager write SetSysManager;
    property EmuManager: cEmutecaEmulatorManager
      read FEmuManager write SetEmuManager;
    {< Needed to populate OtherEmulators in SysEditor
    }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemManager }

procedure TfmEmutecaSystemManager.SetSysManager(AValue: cEmutecaSystemManager);
begin
  FSysManager := AValue;
  LoadList;
end;

procedure TfmEmutecaSystemManager.CheckListBox1Click(Sender: TObject);
begin
  // TODO 3: Until TCheckListBox.OnItemClick works as expected...
  // http://www.lazarus.freepascal.org/index.php?topic=12319.0
  SelectItem;
end;

procedure TfmEmutecaSystemManager.CheckListBox1ClickCheck(Sender: TObject);
var
  CurrItem: cEmutecaSystem;
begin
  CurrItem := cEmutecaSystem(
    CheckListBox1.Items.Objects[CheckListBox1.ItemIndex]);

  CurrItem.Enabled := CheckListBox1.Checked[CheckListBox1.ItemIndex];
end;

procedure TfmEmutecaSystemManager.actAddItemExecute(Sender: TObject);
var
  SystemID: string;
  aSystem: cEmutecaSystem;
begin
  SystemID := Trim(InputBox(actAddItem.Caption, rsSystemName, ''));
  if SystemID = '' then
    Exit;

  aSystem := cEmutecaSystem.Create(nil);
  aSystem.ID := SystemID;
  aSystem.Model:=SystemID;
  aSystem.Enabled := True;
  SysManager.FullList.Add(aSystem);

  LoadList;

  // TODO Autoselecting last item.
  {ñññ
  CheckListBox1.ItemIndex := CheckListBox1.Items.IndexOf(SystemID);
  CheckListBox1.Checked[CheckListBox1.ItemIndex] := aSystem.Enabled;
  SelectItem;
  }
end;

procedure TfmEmutecaSystemManager.actCancelAndExitExecute(Sender: TObject);
begin
  // Restoring data from file
  LoadFromFile;
end;

procedure TfmEmutecaSystemManager.actCheckAllExecute(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  while i < SysManager.FullList.Count do
  begin
    SysManager.FullList[i].Enabled := True;
    Inc(i);
  end;
  LoadList;
end;

procedure TfmEmutecaSystemManager.actDeleteItemExecute(Sender: TObject);
begin
  if CheckListBox1.ItemIndex = -1 then
    exit;

  SysEditor.System := nil;
  SysManager.FullList.Remove(
    cEmutecaSystem(CheckListBox1.Items.Objects[CheckListBox1.ItemIndex]) );
  LoadList;
end;

procedure TfmEmutecaSystemManager.actSaveAndExitExecute(Sender: TObject);
begin
  SaveToFile;
end;

procedure TfmEmutecaSystemManager.actSaveListExecute(Sender: TObject);
begin
  SaveToFile;
end;

procedure TfmEmutecaSystemManager.actUncheckAllExecute(Sender: TObject);
begin
  CheckListBox1.CheckAll(cbUnchecked);
end;

procedure TfmEmutecaSystemManager.SetSysEditor(AValue: TfmEmutecaSystemEditor);
begin
  if FSysEditor = AValue then
    Exit;
  FSysEditor := AValue;
end;

procedure TfmEmutecaSystemManager.SetEmuManager(AValue:
  cEmutecaEmulatorManager);
begin
  if FEmuManager = AValue then
    Exit;
  FEmuManager := AValue;

  if Assigned(SysEditor) then
    SysEditor.EmuManager := EmuManager;
end;

procedure TfmEmutecaSystemManager.SetIconsIni(AValue: string);
begin
  if FIconsIni = AValue then
    Exit;
  FIconsIni := AValue;

  if IconsIni <> '' then
    ReadActionsIcons(IconsIni, Self.Name, '', ilActions, ActionList1);
end;

procedure TfmEmutecaSystemManager.LoadList;
var
  i: integer;
begin
  SysEditor.System := nil;

  CheckListBox1.Clear;

  if not assigned(SysManager) then
    exit;

  i := 0;
  while i < SysManager.FullList.Count do
  begin
    // Dragons
    CheckListBox1.Checked[
      CheckListBox1.Items.AddObject(SysManager.FullList[i].Model,
      SysManager.FullList[i])
      ] := SysManager.FullList[i].Enabled;
    Inc(i);
  end;
end;

procedure TfmEmutecaSystemManager.SelectItem;
begin

  if CheckListBox1.ItemIndex = -1 then
    SysEditor.System := nil
  else
    SysEditor.System := cEmutecaSystem(
      CheckListBox1.Items.Objects[CheckListBox1.ItemIndex]);
end;

procedure TfmEmutecaSystemManager.SaveToFile;
begin
  SysManager.SaveToFile('', False);
end;

procedure TfmEmutecaSystemManager.LoadFromFile;
begin
  SysManager.LoadFromFile('');
end;

constructor TfmEmutecaSystemManager.Create(TheOwner: TComponent);

  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := PageControl1.AddTabSheet;
    FSysEditor := TfmEmutecaSystemEditor.Create(aTabSheet);
    SysEditor.Parent := aTabSheet;
    SysEditor.Align := alClient;

    aTabSheet := PageControl1.AddTabSheet;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;
end;

destructor TfmEmutecaSystemManager.Destroy;
begin
  inherited Destroy;
end;

end.
