unit ufEmutecaSystemEditor;
{< TfmEmutecaSystemEditor frame unit.

  ----

  This file is part of Emuteca Core.

  Copyright (C) 2006-2018 Chixpy

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, CheckLst, EditBtn, ActnList, LazFileUtils,
  // CHX units
  uCHXStrUtils, uCHXDlgUtils,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core classes
  ucEmutecaSystem, ucEmutecaEmulator, ucEmutecaEmulatorManager;

type

  { TfmEmutecaSystemEditor }

  TfmEmutecaSystemEditor = class(TfmCHXPropEditor)
    cbxMainEmulator: TComboBox;
    chkExtractAllFiles: TCheckBox;
    clbOtherEmulators: TCheckListBox;
    eBaseFolder: TDirectoryEdit;
    eExtraInfoFilename: TEdit;
    eWorkingFolder: TDirectoryEdit;
    eTitle: TEdit;
    gbxBasicInfo: TGroupBox;
    gbxEmulators: TGroupBox;
    gbxFiles: TGroupBox;
    lBaseFolder: TLabel;
    lFileExtensions: TLabel;
    lFileName: TLabel;
    lMainEmulator: TLabel;
    lMultiCoreIDs: TLabel;
    lOtherEmulators: TLabel;
    lWorkingFolder: TLabel;
    lTitle: TLabel;
    mExtensions: TMemo;
    mMultiCoreIDs: TMemo;
    Panel2: TPanel;
    rgbGameKey: TRadioGroup;
    Splitter1: TSplitter;
    procedure eBaseFolderButtonClick(Sender: TObject);
    procedure eWorkingFolderButtonClick(Sender: TObject);

  private
    FEmuManager: cEmutecaEmulatorManager;
    FSystem: cEmutecaSystem;
    procedure SetEmuManager(AValue: cEmutecaEmulatorManager);
    procedure SetSystem(AValue: cEmutecaSystem);

    procedure UpdateLists;

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    property EmuManager: cEmutecaEmulatorManager
      read FEmuManager write SetEmuManager;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemEditor }

procedure TfmEmutecaSystemEditor.eBaseFolderButtonClick(Sender: TObject);
begin
  SetDirEditInitialDir(eBaseFolder, ProgramDirectory);
end;

procedure TfmEmutecaSystemEditor.eWorkingFolderButtonClick(Sender: TObject);
begin

  if eBaseFolder.Text = '' then
    SetDirEditInitialDir(eWorkingFolder, ProgramDirectory)
  else
    // Using eBaseFolder.Text instead of System.BaseFolder, because
    //   it can be used without saving before;
    SetDirEditInitialDir(eWorkingFolder, eBaseFolder.Text);
end;

procedure TfmEmutecaSystemEditor.SetEmuManager(AValue:
  cEmutecaEmulatorManager);
begin
  if FEmuManager = AValue then
    Exit;
  FEmuManager := AValue;
  UpdateLists;
  Enabled := Assigned(System) and Assigned(EmuManager);
end;

procedure TfmEmutecaSystemEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;
  LoadFrameData;
  Enabled := Assigned(System) and Assigned(EmuManager);
end;

procedure TfmEmutecaSystemEditor.UpdateLists;
begin
  clbOtherEmulators.Clear;
  cbxMainEmulator.Clear;

  if not assigned(EmuManager) then
    exit;

  EmuManager.EnabledList.AssignToStrLst(clbOtherEmulators.Items);
  EmuManager.EnabledList.AssignToStrLst(cbxMainEmulator.Items);
end;

constructor TfmEmutecaSystemEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaSystemEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmutecaSystemEditor.DoClearFrameData;
begin
  eExtraInfoFilename.Clear;
  eTitle.Clear;
  cbxMainEmulator.ItemIndex := -1;
  clbOtherEmulators.CheckAll(cbUnchecked);
  eBaseFolder.Clear;
  eWorkingFolder.Clear;
  rgbGameKey.ItemIndex := 0;
  chkExtractAllFiles.Checked := False;
  mExtensions.Clear;
  mMultiCoreIDs.Clear;
end;

procedure TfmEmutecaSystemEditor.DoSaveFrameData;
var
  i, j: integer;
begin
  System.ListFileName := eExtraInfoFilename.Text;
  System.Title := eTitle.Text;

  if cbxMainEmulator.ItemIndex <> -1 then
    System.MainEmulator :=
      cEmutecaEmulator(cbxMainEmulator.Items.Objects[
      cbxMainEmulator.ItemIndex]).ID;

  // Adding/Removing other emulators,
  // but keeping not listed ones...
  i := 0;
  while i < clbOtherEmulators.Count do
  begin
    if clbOtherEmulators.Checked[i] then
      AddToStringList(System.OtherEmulators,
        cEmutecaEmulator(clbOtherEmulators.Items.Objects[i]).ID)
    else
    begin
      j := System.OtherEmulators.IndexOf(
        cEmutecaEmulator(clbOtherEmulators.Items.Objects[i]).ID);
      if j <> -1 then
        System.OtherEmulators.Delete(j);
    end;
    Inc(i);
  end;

  // Reloading emulator cache
  System.LoadEmulatorsFrom(EmuManager.EnabledList);

  System.BaseFolder := eBaseFolder.Text;
  System.WorkingFolder := eWorkingFolder.Text;
  system.ExtractAll := chkExtractAllFiles.Checked;

  case rgbGameKey.ItemIndex of
    1: System.SoftExportKey := TEFKCRC32;
    2: System.SoftExportKey := TEFKFileName;
    3: System.SoftExportKey := TEFKCustom;
    else  // SHA1 by default
      System.SoftExportKey := TEFKSHA1;
  end;

  System.Extensions.Assign(mExtensions.Lines);
  System.Extensions.Sort;
  System.CoreIDs.Assign(mMultiCoreIDs.Lines);
  System.Extensions.Sort;
end;

procedure TfmEmutecaSystemEditor.DoLoadFrameData;
var
  aEmulator: cEmutecaEmulator;
  i: integer;
begin
  ClearFrameData;

  if (not assigned(System)) or (not assigned(EmuManager)) then
    Exit;

  eTitle.Text := System.Title;
  eExtraInfoFilename.Text := System.ListFileName;

  aEmulator := EmuManager.FullList.ItemById(System.MainEmulator);
  cbxMainEmulator.ItemIndex := cbxMainEmulator.Items.IndexOfObject(aEmulator);

  i := 0;
  while i < clbOtherEmulators.Count do
  begin
    aEmulator := cEmutecaEmulator(clbOtherEmulators.Items.Objects[i]);
    if System.OtherEmulators.IndexOf(aEmulator.ID) <> -1 then
      clbOtherEmulators.Checked[i] := True
    else
      clbOtherEmulators.Checked[i] := False;
    Inc(i);
  end;

  eBaseFolder.Text := SysPath(System.BaseFolder);
  eWorkingFolder.Text := SysPath(System.WorkingFolder);

  case System.SoftExportKey of
    TEFKCRC32: rgbGameKey.ItemIndex := 1;
    TEFKFileName: rgbGameKey.ItemIndex := 2;
    TEFKCustom: rgbGameKey.ItemIndex := 3;
    else  // SHA1 by default
      rgbGameKey.ItemIndex := 0;
  end;

  chkExtractAllFiles.Checked := System.ExtractAll;

  System.Extensions.Sort;
  mExtensions.Lines.Assign(System.Extensions);

  System.CoreIDs.Sort;
  mMultiCoreIDs.Lines.Assign(System.CoreIDs);
end;

end.
