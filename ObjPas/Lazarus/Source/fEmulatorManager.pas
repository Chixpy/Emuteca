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

{ Unit of Emulator Manager form }
unit fEmulatorManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, CheckLst, ActnList, Buttons,
  StdCtrls, EditBtn, Spin, Menus,
  uEmulator, uEmulatorManager, uConfig, uCustomUtils;

const
  kFEMEmulatorsFileExt = '.ini';

resourcestring
  rsEmulatorName = 'Emulator name';
  rsEmulatorIniFilter = 'Emulators Ini File';


type
  { Form used to manage emulators and their configuration.

    Note: We asume that clbEmulators don't have the same order that the
      cEmulatorManager object }

  { TfrmEmulatorManager }

  TfrmEmulatorManager = class(TForm)
    actAddEmulator: TAction;
    actExportEmulators: TAction;
    actImportEmulators: TAction;
    ActionList: TActionList;
    actRemoveEmulator: TAction;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    clbEmulators: TCheckListBox;
    eConfigFile: TFileNameEdit;
    eEmulatorAuthor: TEdit;
    eEmulatorIcon: TFileNameEdit;
    eEmulatorImage: TFileNameEdit;
    eEmulatorName: TEdit;
    eExitCode: TSpinEdit;
    eInfoFile: TFileNameEdit;
    eWebPage: TEdit;
    gbxAdvanced: TGroupBox;
    lEmulatorAuthor: TLabel;
    eExecutable: TFileNameEdit;
    eWorkingFolder: TDirectoryEdit;
    eParameters: TEdit;
    gbEmulatorImages: TGroupBox;
    gbWorkingFolder: TGroupBox;
    gbGeneralData: TGroupBox;
    gbOtherFiles: TGroupBox;
    gbProgram: TGroupBox;
    ilActions: TImageList;
    iEmulatorIcon: TImage;
    iEmulatorImage: TImage;
    lConfigFile: TLabel;
    lEmulatorIcon: TLabel;
    lEmulatorImage: TLabel;
    lEmulatorName: TLabel;
    lExecutable: TLabel;
    lExitCode: TLabel;
    lInfoFile: TLabel;
    lParameters: TLabel;
    lWebPage: TLabel;
    miMMEMImportEmulatorList: TMenuItem;
    miMMEMExportEmulatorList: TMenuItem;
    miMMEMELSep1: TMenuItem;
    miMMEMRemoveEmulator: TMenuItem;
    miMMEMAddEmulator: TMenuItem;
    miMMEMList: TMenuItem;
    miMMEMEmulator: TMenuItem;
    mmEmulatorManager: TMainMenu;
    mParametersHelp: TMemo;
    OpenDialog: TOpenDialog;
    pcEmulatorConfig: TPageControl;
    pInfoFile: TPanel;
    pnlButtons: TPanel;
    pnlEmulators: TPanel;
    rgWorkingFolder: TRadioGroup;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    pagInfo: TTabSheet;
    pagCommandLine: TTabSheet;
    bAddEmulator: TToolButton;
    bExportEmulators: TToolButton;
    bImportEmulator: TToolButton;
    bRemoveEmulator: TToolButton;
    pagAdvanced: TTabSheet;
    tbEmulators: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    procedure actAddEmulatorExecute(Sender: TObject);
    procedure actExportEmulatorsExecute(Sender: TObject);
    procedure actImportEmulatorsExecute(Sender: TObject);
    procedure actRemoveEmulatorExecute(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure clbEmulatorsClick(Sender: TObject);
    procedure clbEmulatorsItemClick(Sender: TObject; Index: integer);
    procedure eConfigFileAcceptFileName(Sender: TObject; var Value: string);
    procedure eConfigFileEditingDone(Sender: TObject);
    procedure eEmulatorAuthorEditingDone(Sender: TObject);
    procedure eEmulatorIconAcceptFileName(Sender: TObject; var Value: string);
    procedure eEmulatorIconEditingDone(Sender: TObject);
    procedure eEmulatorImageAcceptFileName(Sender: TObject; var Value: string);
    procedure eEmulatorImageEditingDone(Sender: TObject);
    procedure eEmulatorNameEditingDone(Sender: TObject);
    procedure eExecutableAcceptFileName(Sender: TObject; var Value: string);
    procedure eExecutableEditingDone(Sender: TObject);
    procedure eExitCodeEditingDone(Sender: TObject);
    procedure eInfoFileAcceptFileName(Sender: TObject; var Value: string);
    procedure eInfoFileEditingDone(Sender: TObject);
    procedure eParametersEditingDone(Sender: TObject);
    procedure eWebPageEditingDone(Sender: TObject);
    procedure eWorkingFolderAcceptDirectory(Sender: TObject;
      var Value: string);
    procedure eWorkingFolderEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgWorkingFolderClick(Sender: TObject);
  private
    { private declarations }
    FConfig: cConfig;
    FEmulator: cEmulator;
    FEmulatorManager: cEmulatorManager;
    procedure SetConfig(const AValue: cConfig);
    procedure SetEmulator(AValue: cEmulator);

  protected
    property EmulatorManager: cEmulatorManager read FEmulatorManager;
    {< cEmulatorManager object used by this form }
    property Emulator: cEmulator read FEmulator write SetEmulator;
    {< Current selected emulator }

    procedure ChangeImage(const aFileName: string; aImage: TImage);
    {< Changes the aImage picture or show the default if aFileName don't exists.

    @param aFileName Image file
    @param aImage TImage component where show the aFilename image
    }

    procedure EnableCheckedEmulators;

    procedure LoadEmulatorList;
    procedure SelectEmulator;
    procedure ClearFields;
    procedure FillFields;

  public
    property Config: cConfig read FConfig write SetConfig;
    {< cConfig object where paths and other configuration paramenters are
         stored
    }
  end;

var
  frmEmulatorManager: TfrmEmulatorManager;

implementation

{ TfrmEmulatorManager }

procedure TfrmEmulatorManager.clbEmulatorsClick(Sender: TObject);
begin
  // TODO 3: Until TCheckListBox.OnItemClick works...
  // http://www.lazarus.freepascal.org/index.php?topic=12319.0
  SelectEmulator;
end;

procedure TfrmEmulatorManager.clbEmulatorsItemClick(Sender: TObject;
  Index: integer);
begin
  // Lo de arriba coñojoder
end;

procedure TfrmEmulatorManager.eConfigFileAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if Emulator = nil then
    Exit;
  Emulator.ConfigFile := Value;
end;

procedure TfrmEmulatorManager.actAddEmulatorExecute(Sender: TObject);
var
  EmulatorID: string;
  aEmulator: cEmulator;
begin
  EmulatorID := Trim(InputBox(actAddEmulator.Caption, rsEmulatorName, ''));
  if EmulatorID = '' then
    Exit;

  aEmulator := EmulatorManager.AddEmulator(EmulatorID);
  if aEmulator <> nil then
    aEmulator.Enabled := True;
  EmulatorID := aEmulator.ID;

  LoadEmulatorList;
  clbEmulators.ItemIndex := clbEmulators.Items.IndexOf(EmulatorID);
  SelectEmulator;
end;

procedure TfrmEmulatorManager.actExportEmulatorsExecute(Sender: TObject);
begin
  if EmulatorManager = nil then
    Exit;

  SaveDialog.Filter := rsEmulatorIniFilter + '(*' +
    kFEMEmulatorsFileExt + ')|*' + kFEMEmulatorsFileExt;
  SaveDialog.DefaultExt := kFEMEmulatorsFileExt;
  if not SaveDialog.Execute then
    Exit;
  EmulatorManager.ExportEmulatorsFile(SaveDialog.FileName, True);
end;

procedure TfrmEmulatorManager.actImportEmulatorsExecute(Sender: TObject);
begin
  if EmulatorManager = nil then
    Exit;

  OpenDialog.Filter := rsEmulatorIniFilter + '(*' +
    kFEMEmulatorsFileExt + ')|*' + kFEMEmulatorsFileExt;
  OpenDialog.DefaultExt := kFEMEmulatorsFileExt;
  if not OpenDialog.Execute then
    Exit;
  EmulatorManager.ImportEmulatorsFile(OpenDialog.FileName);
  LoadEmulatorList;
end;

procedure TfrmEmulatorManager.actRemoveEmulatorExecute(Sender: TObject);
var
  Prev: integer;
begin
  if EmulatorManager = nil then
    Exit;
  if clbEmulators.ItemIndex = -1 then
    Exit;
  EmulatorManager.RemoveEmulator(clbEmulators.Items[clbEmulators.ItemIndex]);

  Prev := clbEmulators.ItemIndex;
  LoadEmulatorList;
  if clbEmulators.Count <= Prev then
    clbEmulators.ItemIndex := clbEmulators.Count - 1
  else
    clbEmulators.ItemIndex := Prev;
  SelectEmulator;
end;

procedure TfrmEmulatorManager.btnOKClick(Sender: TObject);
begin
  EnableCheckedEmulators;
  EmulatorManager.SaveEmulatorsFile(False);
end;

procedure TfrmEmulatorManager.eConfigFileEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.ConfigFile := eConfigFile.Text;
end;

procedure TfrmEmulatorManager.eEmulatorAuthorEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.Developer := eEmulatorAuthor.Text;
end;

procedure TfrmEmulatorManager.eEmulatorIconAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if Emulator = nil then
    Exit;
  Emulator.Icon := Value;
  ChangeImage(Emulator.Icon, iEmulatorIcon);
end;

procedure TfrmEmulatorManager.eEmulatorIconEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.Icon := eEmulatorIcon.Text;
  ChangeImage(Emulator.Icon, iEmulatorIcon);
end;

procedure TfrmEmulatorManager.eEmulatorImageAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if Emulator = nil then
    Exit;
  Emulator.Image := Value;
  ChangeImage(Emulator.Image, iEmulatorImage);
end;

procedure TfrmEmulatorManager.eEmulatorImageEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.Image := eEmulatorImage.Text;
  ChangeImage(Emulator.Image, iEmulatorImage);
end;

procedure TfrmEmulatorManager.eEmulatorNameEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.Name := eEmulatorName.Text;
end;

procedure TfrmEmulatorManager.eExecutableAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if Emulator = nil then
    Exit;
  Emulator.ExeFile := Value;
end;

procedure TfrmEmulatorManager.eExecutableEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.ExeFile := eExecutable.Text;
end;

procedure TfrmEmulatorManager.eExitCodeEditingDone(Sender: TObject);
begin
  Emulator.NormalExitCode := eExitCode.Value;
end;

procedure TfrmEmulatorManager.eInfoFileAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if Emulator = nil then
    Exit;
  Emulator.InfoFile := Value;
end;

procedure TfrmEmulatorManager.eInfoFileEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.InfoFile := eInfoFile.Text;
end;

procedure TfrmEmulatorManager.eParametersEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.Parameters := eParameters.Text;
end;

procedure TfrmEmulatorManager.eWebPageEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.WebPage := eWebPage.Text;
end;

procedure TfrmEmulatorManager.eWorkingFolderAcceptDirectory(Sender: TObject;
  var Value: string);
begin
  if Emulator = nil then
    Exit;
  Emulator.WorkingFolder := Value;
end;

procedure TfrmEmulatorManager.eWorkingFolderEditingDone(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  Emulator.WorkingFolder := eWorkingFolder.Text;
end;

procedure TfrmEmulatorManager.FormCreate(Sender: TObject);
begin
  pcEmulatorConfig.ActivePageIndex := 0;
end;

procedure TfrmEmulatorManager.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEmulatorManager);
end;

procedure TfrmEmulatorManager.rgWorkingFolderClick(Sender: TObject);
begin
  if Emulator = nil then
    Exit;
  eWorkingFolder.Enabled := False;

  case rgWorkingFolder.ItemIndex of
    0: Emulator.WorkingFolder := CEmuDir;
    1: Emulator.WorkingFolder := CROMDir;
    2: Emulator.WorkingFolder := CCurrentDir;
    else
    begin
      eWorkingFolder.Enabled := True;
      Emulator.WorkingFolder := eWorkingFolder.Text;
    end;
  end;
end;

procedure TfrmEmulatorManager.SetConfig(const AValue: cConfig);

  procedure Translate;
  begin
    // Título y paneles
    Self.Caption := Application.Title + ': ' + Self.Caption;
  end;

begin
  FConfig := AValue;
  Translate;

  // Iconos de las acciones
  ReadActionsIcons(Config.IconsIniFile, Self.Name, Config.ImagesFolder +
    Config.IconsSubfolder, ilActions, ActionList);

  FreeAndNil(FEmulatorManager);
  FEmulatorManager := cEmulatorManager.Create(Config.DataFolder +
    Config.EmulatorsIniFile);

  LoadEmulatorList;
  SelectEmulator;
end;

procedure TfrmEmulatorManager.SetEmulator(AValue: cEmulator);
begin
  FEmulator := AValue;
end;

procedure TfrmEmulatorManager.FillFields;
begin
  if Emulator = nil then
  begin
    ClearFields;
    Exit;
  end;

  eEmulatorName.Text := Emulator.Name;
  eEmulatorAuthor.Text := Emulator.Developer;
  eWebPage.Text := Emulator.WebPage;

  eEmulatorIcon.Text := Emulator.Icon;
  ChangeImage(Emulator.Icon, iEmulatorIcon);
  eEmulatorImage.Text := Emulator.Image;
  ChangeImage(Emulator.Image, iEmulatorImage);

  eExecutable.Text := Emulator.ExeFile;
  eParameters.Text := Emulator.Parameters;

  // This must be set before rgWorkingFolder.ItemIndex, because it calls
  //   rgWorkingFolderClick automatically;
  eWorkingFolder.Text := Emulator.WorkingFolder;
  rgWorkingFolder.ItemIndex := 3;

  eConfigFile.Text := Emulator.ConfigFile;
  eExitCode.Value := Emulator.NormalExitCode;
end;

procedure TfrmEmulatorManager.ChangeImage(const aFileName: string;
  aImage: TImage);
begin
  if FileExistsUTF8(aFileName) then
    aImage.Picture.LoadFromFile(aFileName)
  else if FileExistsUTF8(Config.ImagesFolder + Config.DefaultImagesSubfolder +
    Config.DefaultEmulatorImage) then
    aImage.Picture.LoadFromFile(Config.ImagesFolder +
      Config.DefaultImagesSubfolder + Config.DefaultEmulatorImage)
  else
    aImage.Picture.Assign(nil);
end;

procedure TfrmEmulatorManager.EnableCheckedEmulators;
var
  Cont: integer;
  aEmulator: cEmulator;
begin
  // Updating active emulators
  // I don't do in OnClickCheck because OnClick is called before.
  if EmulatorManager = nil then
    Exit;
  Cont := 0;
  while Cont < clbEmulators.Count do
  begin
    aEmulator := EmulatorManager.Emulator(clbEmulators.Items[Cont]);
    // The emulator can be deleted, so we need check this.
    if aEmulator <> nil then
      aEmulator.Enabled := clbEmulators.Checked[Cont];
    Inc(Cont);
  end;
end;

procedure TfrmEmulatorManager.LoadEmulatorList;
var
  i: integer;
begin
  EnableCheckedEmulators;
  Emulator := nil;
  clbEmulators.Clear;
  ClearFields;

  if EmulatorManager = nil then
    Exit;

  EmulatorManager.ListEmulators(clbEmulators.Items);

  i := 0;
  while i < clbEmulators.Count do
  begin
    clbEmulators.Checked[i] :=
      EmulatorManager.Emulator(clbEmulators.Items[i]).Enabled;
    Inc(i);
  end;
end;

procedure TfrmEmulatorManager.SelectEmulator;
begin
  Emulator := nil;
  if (clbEmulators.ItemIndex <> -1) then
    Emulator := EmulatorManager.Emulator(
      clbEmulators.Items[clbEmulators.ItemIndex]);

  FillFields;
end;

procedure TfrmEmulatorManager.ClearFields;
begin
  eEmulatorName.Text := '';
  eEmulatorAuthor.Text := '';
  eWebPage.Text := '';

  eEmulatorIcon.Text := '';
  iEmulatorIcon.Picture.Assign(nil);
  eEmulatorImage.Text := '';
  iEmulatorImage.Picture.Assign(nil);

  eExecutable.Text := '';
  eParameters.Text := '';

  eWorkingFolder.Text := '';
  rgWorkingFolder.ItemIndex := 0;

  eConfigFile.Text := '';

  eExitCode.Value := 0;
end;

initialization
  {$I fEmulatorManager.lrs}

end.
