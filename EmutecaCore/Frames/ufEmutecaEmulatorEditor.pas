unit ufEmutecaEmulatorEditor;

{< TfmEmutecaEmulatorEditor frame unit.

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
  Buttons, ActnList, StdCtrls, EditBtn, Spin, Menus, LazFileUtils, LCLIntf,
  // CHX units
  uCHXStrUtils, uCHXDlgUtils,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core abstract
  uaEmutecaCustomEmu,
  // Emuteca Core classes
  ucEmutecaEmulator;

type

  { TfmEmutecaEmulatorEditor }

  TfmEmutecaEmulatorEditor = class(TfmCHXPropEditor)
    actParamSysCoreID: TAction;
    actParamROMExtra: TAction;
    actParamROMFileExtension: TAction;
    actParamROMFileNoExt: TAction;
    actParamROMFilename: TAction;
    actParamROMDir: TAction;
    actParamROMPath: TAction;
    actWFEmuteca: TAction;
    actOpenWebPage: TAction;
    actWFEmulator: TAction;
    actWFROM: TAction;
    bGoWebPage: TSpeedButton;
    bParameters: TSpeedButton;
    eDeveloper: TEdit;
    eExePath: TFileNameEdit;
    eExitCode: TSpinEdit;
    eIcon: TFileNameEdit;
    eName: TEdit;
    eParameters: TEdit;
    eWebPage: TEdit;
    eWorkingFolder: TDirectoryEdit;
    gbxImages: TGroupBox;
    gbxBasic: TGroupBox;
    iIcon: TImage;
    lDeveloper: TLabel;
    lExePath: TLabel;
    lExitCode: TLabel;
    lIcon: TLabel;
    lID: TLabel;
    lName: TLabel;
    lParameters: TLabel;
    lWebPage: TLabel;
    lWorkingFolder: TLabel;
    mParametersHelp: TMemo;
    pmiParamROMSysID: TMenuItem;
    pmiParamROMExtra: TMenuItem;
    pmiParamROMFileExtension: TMenuItem;
    pmiParamROMFileNoExt: TMenuItem;
    pmiParamROMFilename: TMenuItem;
    pmiParamROMDir: TMenuItem;
    pmiParamROMPath: TMenuItem;
    pmiWFEmuteca: TMenuItem;
    pmiWFEmu: TMenuItem;
    pmiWFROM: TMenuItem;
    pmParameters: TPopupMenu;
    pmWFolder: TPopupMenu;
    pParameters: TPanel;
    pWebPage: TPanel;
    pWFolder: TPanel;
    bWorkingFolder: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure actOpenWebPageExecute(Sender: TObject);
    procedure actParamROMDirExecute(Sender: TObject);
    procedure actParamROMExtraExecute(Sender: TObject);
    procedure actParamROMFileExtensionExecute(Sender: TObject);
    procedure actParamROMFilenameExecute(Sender: TObject);
    procedure actParamROMFileNoExtExecute(Sender: TObject);
    procedure actParamROMPathExecute(Sender: TObject);
    procedure actParamSysCoreIDExecute(Sender: TObject);
    procedure actWFEmulatorExecute(Sender: TObject);
    procedure actWFEmutecaExecute(Sender: TObject);
    procedure actWFROMExecute(Sender: TObject);
    procedure bParametersClick(Sender: TObject);
    procedure bWorkingFolderClick(Sender: TObject);
    procedure eFileButtonClick(Sender: TObject);
    procedure eIconAcceptFileName(Sender: TObject; var Value: string);
    procedure eIconButtonClick(Sender: TObject);
  private
    FEmulator: cEmutecaEmulator;
    procedure SetEmulator(AValue: cEmutecaEmulator);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmEmutecaEmulatorEditor }

constructor TfmEmutecaEmulatorEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaEmulatorEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmutecaEmulatorEditor.actWFEmulatorExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaEmuDirKey;
end;

procedure TfmEmutecaEmulatorEditor.actWFEmutecaExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaCurrentDirKey;
end;

procedure TfmEmutecaEmulatorEditor.actOpenWebPageExecute(Sender: TObject);
begin
  if eWebPage.Text = '' then
    Exit;
  OpenURL(eWebPage.Text);
end;

procedure TfmEmutecaEmulatorEditor.actParamROMDirExecute(Sender: TObject);
begin
  eParameters.SelText := kEmutecaROMDirKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMExtraExecute(Sender: TObject);
begin
  eParameters.SelText := kEmutecaROMExtraParamKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMFileExtensionExecute(
  Sender: TObject);
begin
  eParameters.SelText := kEmutecaROMFileExtKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMFilenameExecute(Sender: TObject);
begin
  eParameters.SelText := kEmutecaROMFileNameKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMFileNoExtExecute(
  Sender: TObject);
begin
  eParameters.SelText := kEmutecaROMFileNameNoExtKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMPathExecute(Sender: TObject);
begin
  eParameters.SelText := kEmutecaROMPathKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamSysCoreIDExecute(Sender: TObject);
begin
  eParameters.SelText := kEmutecaROMSysIDKey;
end;

procedure TfmEmutecaEmulatorEditor.actWFROMExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaROMDirKey;
end;

procedure TfmEmutecaEmulatorEditor.bParametersClick(Sender: TObject);
begin
  pmParameters.PopUp;
end;

procedure TfmEmutecaEmulatorEditor.bWorkingFolderClick(Sender: TObject);
begin
  pmWFolder.PopUp;
end;

procedure TfmEmutecaEmulatorEditor.eFileButtonClick(Sender: TObject);
begin
  SetFileEditInitialDir(TFileNameEdit(Sender), ProgramDirectory);
end;

procedure TfmEmutecaEmulatorEditor.eIconAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if FileExistsUTF8(Value) then
    iIcon.Picture.LoadFromFile(Value)
  else
    iIcon.Picture.Clear;
end;

procedure TfmEmutecaEmulatorEditor.eIconButtonClick(Sender: TObject);
begin
  if Assigned(Emulator) then
    SetFileEditInitialDir(eIcon, ExtractFileDir(eExePath.Text))
  else
    SetFileEditInitialDir(eIcon, '');
end;

procedure TfmEmutecaEmulatorEditor.SetEmulator(AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
  LoadFrameData;
end;

procedure TfmEmutecaEmulatorEditor.DoClearFrameData;
begin
  lID.Caption := ' ';
  eName.Clear;
  eDeveloper.Clear;
  eWebPage.Clear;

  eExePath.Clear;
  eWorkingFolder.Clear;
  eParameters.Clear;

  eIcon.Clear;

  eExitCode.Value := 0;
end;

procedure TfmEmutecaEmulatorEditor.DoLoadFrameData;
begin
  ClearFrameData;

  Enabled := assigned(Emulator);

  if not Enabled then
    Exit;

  lID.Caption := Emulator.ID;
  eName.Text := Emulator.Title;
  eDeveloper.Text := Emulator.Developer;
  eWebPage.Text := Emulator.WebPage;

  eExePath.Text := SysPath(Emulator.ExeFile);
  eWorkingFolder.Text := SysPath(Emulator.WorkingFolder);
  eParameters.Text := Emulator.Parameters;

  eIcon.Text := Emulator.IconFile;

  eExitCode.Value := Emulator.ExitCode;
end;

procedure TfmEmutecaEmulatorEditor.DoSaveFrameData;
begin
  Emulator.Title := eName.Text;
  Emulator.Developer := eDeveloper.Text;
  Emulator.WebPage := eWebPage.Text;

  Emulator.ExeFile := eExePath.Text;
  Emulator.WorkingFolder := eWorkingFolder.Text;
  Emulator.Parameters := eParameters.Text;

  Emulator.IconFile := eIcon.Text;

  Emulator.ExitCode := eExitCode.Value;
end;

end.
