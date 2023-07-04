unit ufETKGUIEmuManager;

{< TfmETKGUIEmuManager frame unit.

  ----

  This file is part of Emuteca GUI.

  Copyright (C) 2011-2018 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, CheckLst, ActnList, Menus,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXChkLstPropEditor,
  // CHX forms
  ufrCHXForm,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core classes
  ucEmutecaEmulatorManager, ucEmutecaEmulator,
  // Emuteca GUI frames
  ufETKGUIFullEmuEditor;

resourcestring
  rsEmulatorName = 'Emulator name.';

type
  { TfmETKGUIEmuManager.

    Emulator Manager frame.
  }

  TfmETKGUIEmuManager = class(TfmCHXChkLstPropEditor)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;

  private
    FfmEmuEditor: TfmETKGUIFullEmuEditor;
    FEmuManager: cEmutecaEmulatorManager;
    FSHA1Folder: string;
    procedure SetEmuManager(AValue: cEmutecaEmulatorManager);
    procedure SetSHA1Folder(AValue: string);

  protected
    property fmEmuEditor: TfmETKGUIFullEmuEditor read FfmEmuEditor;

    procedure AddItemToList; override;
    procedure DeleteItemFromList; override;
    procedure ExportList; override;
    procedure ImportList; override;
    procedure OnListClick(aObject: TObject); override;
    procedure OnListClickCheck(aObject: TObject; aBool: boolean); override;
    procedure SetCheckedAll(aBool: boolean); override;

  protected

  public
    property EmuManager: cEmutecaEmulatorManager
      read FEmuManager write SetEmuManager;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    // Creates a form with Emulator Manager.
    class function SimpleForm(aEmuManager: cEmutecaEmulatorManager;
      aSHA1Folder: string; aGUIConfigIni: string;
      aGUIIconsIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUIEmuManager }

procedure TfmETKGUIEmuManager.SetEmuManager(AValue: cEmutecaEmulatorManager);
begin
  if FEmuManager = AValue then
    Exit;
  FEmuManager := AValue;

  LoadFrameData;
end;

procedure TfmETKGUIEmuManager.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
  fmEmuEditor.SHA1Folder := SHA1Folder;
end;

procedure TfmETKGUIEmuManager.SetCheckedAll(aBool: boolean);
var
  i: integer;
  aEmulator: cEmutecaEmulator;
begin
  if not assigned(EmuManager) then
    Exit;

  i := 0;
  while i < EmuManager.FullList.Count do
  begin
    aEmulator := cEmutecaEmulator(EmuManager.FullList[i]);
    aEmulator.Enabled := aBool;
    Inc(i);
  end;
end;

procedure TfmETKGUIEmuManager.AddItemToList;
var
  EmulatorID: string;
  aEmulator: cEmutecaEmulator;
begin
  if not assigned(EmuManager) then
    Exit;

  EmulatorID := Trim(InputBox(actAddItem.Caption, rsEmulatorName, ''));
  if EmulatorID = '' then
    Exit;

  aEmulator := cEmutecaEmulator.Create(nil);
  aEmulator.ID := EmulatorID;
  aEmulator.Title := EmulatorID;
  aEmulator.Enabled := True;

  EmuManager.FullList.Add(aEmulator);

  LoadFrameData;

  fmEmuEditor.Emulator := aEmulator;
end;

procedure TfmETKGUIEmuManager.DeleteItemFromList;
var
  aEmulator: cEmutecaEmulator;
begin
  if not assigned(EmuManager) then
    Exit;

  if clbPropItems.ItemIndex = -1 then
    Exit;

  fmEmuEditor.Emulator := nil;

  aEmulator := cEmutecaEmulator(
    clbPropItems.Items.Objects[clbPropItems.ItemIndex]);
  try
    // If already in enabled list remove here too.
    EmuManager.EnabledList.Remove(aEmulator);

    // FullList frees the object too.
    EmuManager.FullList.Remove(aEmulator);
    //aEmulator.Free;
  finally
    LoadFrameData;
  end;
end;

procedure TfmETKGUIEmuManager.ExportList;
begin
  if not assigned(EmuManager) then
    Exit;

  if not SaveDialog1.Execute then
    Exit;

  EmuManager.ExportToFile(SaveDialog1.FileName, False);
end;

procedure TfmETKGUIEmuManager.ImportList;
begin
  if not assigned(EmuManager) then
    Exit;

  if not OpenDialog1.Execute then
    Exit;

  EmuManager.ImportFromFile(OpenDialog1.FileName);
end;

procedure TfmETKGUIEmuManager.LoadFrameData;
var
  i: integer;
begin
  inherited LoadFrameData;

  Enabled := Assigned(EmuManager);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  clbPropItems.Clear;
  EmuManager.FullList.AssignToStrLst(clbPropItems.Items);

  i := 0;
  while i < clbPropItems.Items.Count do
  begin
    clbPropItems.Checked[i] :=
      cEmutecaEmulator(clbPropItems.Items.Objects[i]).Enabled;
    Inc(i);
  end;
end;

procedure TfmETKGUIEmuManager.SaveFrameData;
begin
  inherited SaveFrameData;

  if not assigned(EmuManager) then
    Exit;

  // Saving current system data
  if assigned(fmEmuEditor.Emulator) then fmEmuEditor.SaveFrameData;

  EmuManager.UpdateEnabledList;
end;

class function TfmETKGUIEmuManager.SimpleForm(
  aEmuManager: cEmutecaEmulatorManager; aSHA1Folder: string;
  aGUIConfigIni: string; aGUIIconsIni: string): integer;
var
  aFrame: TfmETKGUIEmuManager;
begin
  aFrame := TfmETKGUIEmuManager.Create(nil);

  aFrame.SaveButtons := True;
  aFrame.ButtonClose := True;
  aFrame.Align := alClient;

  aFrame.SHA1Folder := aSHA1Folder;
  aFrame.EmuManager := aEmuManager;

  Result := GenSimpleModalForm(aFrame, 'frmETKGUIEmuManager',
    Format(krsFmtWindowCaption, [Application.Title, 'Emulator Manager']),
    aGUIConfigIni, aGUIIconsIni);
end;

procedure TfmETKGUIEmuManager.OnListClick(aObject: TObject);
begin
  fmEmuEditor.Emulator := cEmutecaEmulator(aObject);
end;

procedure TfmETKGUIEmuManager.OnListClickCheck(aObject: TObject;
  aBool: boolean);
var
  CurrItem: cEmutecaEmulator;
begin
  if not assigned(aObject) then
    Exit;

  CurrItem := cEmutecaEmulator(aObject);
  CurrItem.Enabled := aBool;
end;

constructor TfmETKGUIEmuManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FfmEmuEditor := TfmETKGUIFullEmuEditor.Create(Self);
  fmEmuEditor.SaveButtons := True;
  fmEmuEditor.ButtonClose := False;
  fmEmuEditor.Align := alClient;
  fmEmuEditor.Parent := Self;
end;

destructor TfmETKGUIEmuManager.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmETKGUIEmuManager);

finalization
  UnRegisterClass(TfmETKGUIEmuManager);

end.
