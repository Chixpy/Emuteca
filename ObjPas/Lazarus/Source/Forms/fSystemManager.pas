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

{Unit of System Manager form}
unit fSystemManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CheckLst, ComCtrls, ExtCtrls, Buttons, StdCtrls, EditBtn, ActnList,
    // Common
  uRscStr, uConst,
  // Emuteca
  uEmutecaSystemManager, uEmutecaEmulatorManager, uConfig, uEmutecaSystem,
  uCHXStrUtils, uCHXImageUtils;

// TODO 1: ¡¡REHACER TODO!!, Bueno no tanto...
//   Cambiar los OnClick, OnChange, etc por métodos más 'pofesionales',
//   como se hace en el formulario principal
type

  { TfrmSystemManager }

  TfrmSystemManager = class(TForm)
    actAddSystem: TAction;
    actExportSystems: TAction;
    actImportSystems: TAction;
    actAutoConfig: TAction;
    actAddImageFolder: TAction;
    actAddTextFolder: TAction;
    actAddMusicFolder: TAction;
    actAddVideoFolder: TAction;
    actMakeAbsolutePaths: TAction;
    actMakeRelativePaths: TAction;
    actRemoveVideoFolder: TAction;
    actRemoveMusicFolder: TAction;
    actRemoveTextFolder: TAction;
    actRemoveImageFolder: TAction;
    actRemoveSystem: TAction;
    ActionList: TActionList;
    bAddOtherMusicFolder: TButton;
    bAddOtherFilesFolder: TButton;
    bAddOtherVideoFolder: TButton;
    bAutoConfig: TButton;
    bMakeAbsolutePaths: TButton;
    bMakeRelativePaths: TButton;
    bRemoveOtherMusicFolder: TButton;
    bRemoveOtherFilesFolder: TButton;
    bRemoveOtherVideoFolder: TButton;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    chkExtractAll: TCheckBox;
    chkOtherMusicMultiFile: TCheckBox;
    chkOtherFilesMultiFile: TCheckBox;
    chkOtherVideoMultiFile: TCheckBox;
    chkUseCRC: TCheckBox;
    lbImageFolders: TListBox;
    clbOtherEmulators: TCheckListBox;
    clbSystems: TCheckListBox;
    cbMainEmulator: TComboBox;
    lbTextFolders: TListBox;
    DirectoryDialog: TSelectDirectoryDialog;
    eBaseFolder: TDirectoryEdit;
    eDemoMusicFolder: TDirectoryEdit;
    eDemoVideoFolder: TDirectoryEdit;
    eGameFolder: TDirectoryEdit;
    eIconFolder: TDirectoryEdit;
    eOtherMusicCaption: TEdit;
    eOtherFilesCaption: TEdit;
    eOtherFilesExecutable: TFileNameEdit;
    eOtherFilesExtensions: TEdit;
    eOtherFilesFolder: TDirectoryEdit;
    eOtherFilesParameters: TEdit;
    eOtherVideoCaption: TEdit;
    eOtherMusicExecutable: TFileNameEdit;
    eOtherVideoExecutable: TFileNameEdit;
    eOtherMusicExtensions: TEdit;
    eOtherVideoExtensions: TEdit;
    eOtherMusicFolder: TDirectoryEdit;
    eOtherVideoFolder: TDirectoryEdit;
    eOtherMusicParameters: TEdit;
    eSpineFolder: TDirectoryEdit;
    eOtherVideoParameters: TEdit;
    eBackgroundImage: TFileNameEdit;
    eTempFolder: TDirectoryEdit;
    eCompany: TEdit;
    eExtensions: TEdit;
    eModel: TEdit;
    eLastYear: TEdit;
    eFirstYear: TEdit;
    eSystemImage: TFileNameEdit;
    eSystemTextFile: TFileNameEdit;
    eSystemIcon: TFileNameEdit;
    gbBaseFolder: TGroupBox;
    gbDemoMusicFolder: TGroupBox;
    gbDemoVideoFolder: TGroupBox;
    gbGameFolder: TGroupBox;
    gbImageFolders: TGroupBox;
    gbOtherMusicFolders: TGroupBox;
    gbOtherFilesFolders: TGroupBox;
    gbOtherVideoFolders: TGroupBox;
    gbSystemData: TGroupBox;
    gbSystemImages: TGroupBox;
    gbEmulators: TGroupBox;
    gbReserved: TGroupBox;
    gbTempFolder: TGroupBox;
    gbSystemConfig: TGroupBox;
    gbTextFolders: TGroupBox;
    ilActions: TImageList;
    iSystemIcon: TImage;
    iBackgroundImage: TImage;
    iSystemImage: TImage;
    lBackgroundImage: TLabel;
    mImageCaptions: TMemo;
    mTextCaptions: TMemo;
    lOtherEmulators: TLabel;
    lAutoConfig: TLabel;
    lExtensions: TLabel;
    lIconFolder: TLabel;
    lImageCaptions: TLabel;
    lImageFolders: TLabel;
    lModel: TLabel;
    lCompany: TLabel;
    lLastYear: TLabel;
    lFirstYear: TLabel;
    lOtherMusicCaption: TLabel;
    lOtherFilesCaption: TLabel;
    lOtherFilesExecutable: TLabel;
    lOtherFilesExtensions: TLabel;
    lOtherFilesFolder: TLabel;
    lOtherFilesParameters: TLabel;
    lOtherVideoCaption: TLabel;
    lOtherMusicExecutable: TLabel;
    lOtherVideoExecutable: TLabel;
    lOtherMusicExtensions: TLabel;
    lOtherVideoExtensions: TLabel;
    lOtherMusicFolder: TLabel;
    lOtherVideoFolder: TLabel;
    lOtherMusicParameters: TLabel;
    lSpineFolder: TLabel;
    lOtherVideoParameters: TLabel;
    lSystemIcon: TLabel;
    lSystemImage: TLabel;
    lSystemTextFile: TLabel;
    lMainEmulator: TLabel;
    lTextCaptions: TLabel;
    lTextFolders: TLabel;
    lvOtherMusicFolders: TListView;
    lvOtherFilesFolders: TListView;
    lvOtherVideoFolders: TListView;
    OpenDialog: TOpenDialog;
    pAutoConfig: TPanel;
    pAux1OtherFiles: TPanel;
    pAux1OtherVideo: TPanel;
    pAux2OtherMusic: TPanel;
    pAux2OtherFiles: TPanel;
    pAux2OtherVideo: TPanel;
    pAux3OtherMusic: TPanel;
    pAux1OtherMusic: TPanel;
    pAux3OtherFiles: TPanel;
    pAux3OtherVideo: TPanel;
    pBasefolder: TPanel;
    pEOtherMusicFolder: TPanel;
    pEOtherFilesFolder: TPanel;
    pEOtherFilesFolderExecutable: TPanel;
    pEOtherVideoFolder: TPanel;
    pEOtherMusicFolderExecutable: TPanel;
    pEOtherVideoFolderExecutable: TPanel;
    pIconBack: TPanel;
    pIconFolder: TPanel;
    pImageCaptions: TPanel;
    pImageFolders: TPanel;
    pImages: TPanel;
    pnlSystemData: TPanel;
    pnlYears: TPanel;
    pnlMainGeneral: TPanel;
    pcSystemConfig: TPageControl;
    pnlSystems: TPanel;
    pnlButtons: TPanel;
    pnlSystem: TPanel;
    pOtherMusicButtons: TPanel;
    pOtherFilesButtons: TPanel;
    pOtherVideoButtons: TPanel;
    pSpineFolder: TPanel;
    pSystemBack: TPanel;
    pSystemIcon: TPanel;
    pSystemImage: TPanel;
    pTextCaptions: TPanel;
    pTextFolders: TPanel;
    rgbTempFolder: TRadioGroup;
    rgOtherMusicPlayer: TRadioGroup;
    rgOtherFilesPlayer: TRadioGroup;
    rgOtherVideoPlayer: TRadioGroup;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    pagGeneral: TTabSheet;
    pagMainFolders: TTabSheet;
    pagEmulator: TTabSheet;
    pagMusic: TTabSheet;
    pagVideos: TTabSheet;
    pagOtherFile: TTabSheet;
    tbImages: TToolBar;
    tbSystems: TToolBar;
    tbAddSystem: TToolButton;
    tbTextFolders: TToolBar;
    ToolButton1: TToolButton;
    tbRemoveSystem: TToolButton;
    tbAddImageFolder: TToolButton;
    tbRemoveImageFolder: TToolButton;
    ToolButton3: TToolButton;
    tbExportSystems: TToolButton;
    tbAddTextFolder: TToolButton;
    ToolButton5: TToolButton;
    tbImportSystems: TToolButton;
    tbRemoveTextFolder: TToolButton;
    procedure actAddImageFolderExecute(Sender: TObject);
    procedure actAddMusicFolderExecute(Sender: TObject);
    procedure actAddSystemExecute(Sender: TObject);
    procedure actAddTextFolderExecute(Sender: TObject);
    procedure actAddVideoFolderExecute(Sender: TObject);
    procedure actAutoConfigExecute(Sender: TObject);
    procedure actExportSystemsExecute(Sender: TObject);
    procedure actImportSystemsExecute(Sender: TObject);
    procedure actMakeAbsolutePathsExecute(Sender: TObject);
    procedure actMakeRelativePathsExecute(Sender: TObject);
    procedure actRemoveImageFolderExecute(Sender: TObject);
    procedure actRemoveMusicFolderExecute(Sender: TObject);
    procedure actRemoveSystemExecute(Sender: TObject);
    procedure actRemoveTextFolderExecute(Sender: TObject);
    procedure actRemoveVideoFolderExecute(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbMainEmulatorSelect(Sender: TObject);
    procedure chkExtractAllEditingDone(Sender: TObject);
    procedure chkUseCRCEditingDone(Sender: TObject);
    procedure clbOtherEmulatorsClickCheck(Sender: TObject);
    procedure clbSystemsClick(Sender: TObject);
    procedure clbSystemsItemClick(Sender: TObject; Index: integer);
    procedure eBackgroundImageAcceptFileName(Sender: TObject; var Value: String
      );
    procedure eBackgroundImageEditingDone(Sender: TObject);
    procedure eBaseFolderAcceptDirectory(Sender: TObject; var Value: String);
    procedure eBaseFolderEditingDone(Sender: TObject);
    procedure eCompanyEditingDone(Sender: TObject);
    procedure eDemoMusicFolderAcceptDirectory(Sender: TObject;
      var Value: String);
    procedure eDemoMusicFolderEditingDone(Sender: TObject);
    procedure eDemoVideoFolderAcceptDirectory(Sender: TObject;
      var Value: String);
    procedure eDemoVideoFolderEditingDone(Sender: TObject);
    procedure eExtensionsEditingDone(Sender: TObject);
    procedure eFirstYearEditingDone(Sender: TObject);
    procedure eGameFolderAcceptDirectory(Sender: TObject; var Value: String);
    procedure eGameFolderEditingDone(Sender: TObject);
    procedure eIconFolderAcceptDirectory(Sender: TObject; var Value: String);
    procedure eIconFolderEditingDone(Sender: TObject);
    procedure eLastYearEditingDone(Sender: TObject);
    procedure eSpineFolderAcceptDirectory(Sender: TObject; var Value: String);
    procedure eSpineFolderEditingDone(Sender: TObject);
    procedure eModelEditingDone(Sender: TObject);
    procedure eSystemIconAcceptFileName(Sender: TObject; var Value: String);
    procedure eSystemIconEditingDone(Sender: TObject);
    procedure eSystemImageAcceptFileName(Sender: TObject; var Value: String);
    procedure eSystemImageEditingDone(Sender: TObject);
    procedure eSystemTextFileAcceptFileName(Sender: TObject; var Value: String);
    procedure eSystemTextFileEditingDone(Sender: TObject);
    procedure eTempFolderAcceptDirectory(Sender: TObject; var Value: String);
    procedure eTempFolderEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgbTempFolderClick(Sender: TObject);
    procedure rgOtherMusicPlayerClick(Sender: TObject);
    procedure rgOtherVideoPlayerClick(Sender: TObject);

  private
    FConfig: cConfig;
    FSystem: cSystem;
    FSystemManager: cSystemManager;
    procedure SetConfig(const AValue: cConfig);
    procedure SetSystem(const AValue: cSystem);
    procedure SetSystemManager(const AValue: cSystemManager);

  protected
    property SystemManager: cSystemManager
      read FSystemManager write SetSystemManager;
    property System: cSystem read FSystem write SetSystem;

    procedure AddFolderWithCaption(FolderList, CaptionList: TStrings;
      Folder, FolderCaption: String);
    procedure ChangeImage(const aFileName: String; aImage: TImage);

    procedure EnableCheckedSystems;

    procedure LoadSystemList;
    procedure SelectSystem;
    procedure ClearFields;
    procedure FillFields;

  public
    property Config: cConfig read FConfig write SetConfig;

  end;

var
  frmSystemManager: TfrmSystemManager;

implementation

{ TfrmSystemManager }

procedure TfrmSystemManager.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSystemManager);
end;

procedure TfrmSystemManager.rgbTempFolderClick(Sender: TObject);
begin
  if System = nil then
    Exit;

  eTempFolder.Enabled := False;
  case rgbTempFolder.ItemIndex of
    0: System.TempFolder := '';
    else
    begin
      eTempFolder.Enabled := True;
      System.TempFolder := eTempFolder.Text;
    end;
  end;
end;

procedure TfrmSystemManager.rgOtherMusicPlayerClick(Sender: TObject);
begin
  case rgOtherMusicPlayer.ItemIndex of
    0: // Internal
    begin
      lOtherMusicExecutable.Enabled := False;
      eOtherMusicExecutable.Enabled := False;
      lOtherMusicParameters.Enabled := False;
      eOtherMusicParameters.Enabled := False;
    end;
    1: // Custom executable
    begin
      lOtherMusicExecutable.Enabled := True;
      eOtherMusicExecutable.Enabled := True;
      lOtherMusicParameters.Enabled := True;
      eOtherMusicParameters.Enabled := True;
    end;
  end;
end;

procedure TfrmSystemManager.rgOtherVideoPlayerClick(Sender: TObject);
begin
  case rgOtherMusicPlayer.ItemIndex of
    0: // Internal
    begin
      lOtherVideoExecutable.Enabled := False;
      eOtherVideoExecutable.Enabled := False;
      lOtherVideoParameters.Enabled := False;
      eOtherVideoParameters.Enabled := False;
    end;
    1: // Custom executable
    begin
      lOtherVideoExecutable.Enabled := True;
      eOtherVideoExecutable.Enabled := True;
      lOtherVideoParameters.Enabled := True;
      eOtherVideoParameters.Enabled := True;
    end;
  end;
end;

procedure TfrmSystemManager.actAddSystemExecute(Sender: TObject);
var
  SystemName: String;
  aSystem: cSystem;
begin
  SystemName := Trim(InputBox(actAddSystem.Caption, rsFSMSystemName, ''));
  if SystemName = '' then Exit;

  aSystem := SystemManager.AddSystem(SystemName);
  if aSystem <> nil then
    aSystem.Enabled := True;
  SystemName := aSystem.ID;

  LoadSystemList;
  clbSystems.ItemIndex := clbSystems.Items.IndexOf(SystemName);
  SelectSystem;
end;

procedure TfrmSystemManager.actAddTextFolderExecute(Sender: TObject);
begin
  if System = nil then
    Exit;
  if not DirectoryDialog.Execute then
    Exit;
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    DirectoryDialog.FileName, ExtractFileName(
    ExcludeTrailingPathDelimiter(DirectoryDialog.FileName)));

  FillFields;
end;

procedure TfrmSystemManager.actAddVideoFolderExecute(Sender: TObject);
var
  aItem: TListItem;
begin
  if System = nil then
    Exit;
  if not FileExistsUTF8(eOtherVideoFolder.Directory) then
    Exit;

  System.VideoFolders.Add(eOtherVideoFolder.Directory);
  System.VideoCaptions.Add(eOtherVideoCaption.Text);
  System.VideoExtensions.Add(eOtherVideoExtensions.Text);
  System.VideoExecutables.Add(eOtherVideoExecutable.Text);
  System.VideoParameters.Add(eOtherVideoParameters.Text);

  FillFields;
end;

procedure TfrmSystemManager.actAddImageFolderExecute(Sender: TObject);
begin
  if System = nil then Exit;
  if not DirectoryDialog.Execute then Exit;
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions, DirectoryDialog.FileName, ExtractFileName(ExcludeTrailingPathDelimiter(DirectoryDialog.FileName)));

  FillFields;
end;

procedure TfrmSystemManager.actAddMusicFolderExecute(Sender: TObject);
var
  aItem: TListItem;
begin
  if System = nil then
    Exit;
  if not FileExistsUTF8(eOtherMusicFolder.Directory) then
    Exit;

  System.MusicFolders.Add(eOtherMusicFolder.Directory);
  System.MusicCaptions.Add(eOtherMusicCaption.Text);
  System.MusicExtensions.Add(eOtherMusicExtensions.Text);
  System.MusicExecutables.Add(eOtherMusicExecutable.Text);
  System.MusicParameters.Add(eOtherMusicParameters.Text);

  FillFields;
end;

procedure TfrmSystemManager.actAutoConfigExecute(Sender: TObject);
begin
  if Self.System = nil then
    Exit;
  if (System.BaseFolder = '') or
    (not DirectoryExistsUTF8(System.BaseFolder)) then
    Exit;

  if MessageDlg(Format(rsFSMAutoConfigSystem, [System.BaseFolder]),
    mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Exit;

  // Creating folders
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderGames);
  System.GameFolder := System.BaseFolder + rsFSMAutoFolderGames;

  // Images
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderImg);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgTitle);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgTitle,
    rsFSMAutoFolderImgTitle);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgInGame);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgInGame,
    rsFSMAutoFolderImgInGame);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgFront);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgFront,
    rsFSMAutoFolderImgFront);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgBack);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgBack,
    rsFSMAutoFolderImgBack);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgMedia);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgMedia,
    rsFSMAutoFolderImgMedia);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderImgMarquee);
  System.MarqueeFolder := System.BaseFolder + rsFSMAutoFolderImg +
    rsFSMAutoFolderImgMarquee;
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderIcons);
  System.IconFolder := System.BaseFolder + rsFSMAutoFolderImg + rsFSMAutoFolderIcons;

  // Texts
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderTxt);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtInfo);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtInfo,
    rsFSMAutoFolderTxtInfo);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtManual);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtManual,
    rsFSMAutoFolderTxtManual);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtCheat);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtCheat,
    rsFSMAutoFolderTxtCheat);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtNotes);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtNotes,
    rsFSMAutoFolderTxtNotes);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtCredit);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsFSMAutoFolderTxt + rsFSMAutoFolderTxtCredit,
    rsFSMAutoFolderTxtCredit);

  // TODO 1: Música
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderMusic);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderMusic + rsFSMAutoFolderMusicDemo);
  System.DemoMusicFolder :=
    System.BaseFolder + rsFSMAutoFolderMusic + rsFSMAutoFolderMusicDemo;

  // TODO 1: Videos
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderVideo);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderVideo + rsFSMAutoFolderVideoDemo);
  System.DemoVideoFolder :=
    System.BaseFolder + rsFSMAutoFolderVideo + rsFSMAutoFolderVideoDemo;

  // TODO 1: Otros archivos

  // Un par de directorios extra
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderBIOS);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderTemp);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderSaves);
  CreateDirUTF8(System.BaseFolder + rsFSMAutoFolderTools);

  // Además configuramos el texto del sistema
  System.InfoText := System.BaseFolder + CleanFileName(Self.System.ID) + '.txt';

  SelectSystem;
end;

procedure TfrmSystemManager.actExportSystemsExecute(Sender: TObject);
begin
  if SystemManager = nil then Exit;

  SaveDialog.Filter := kFSMSystemIniFilter + '|*.ini';
  SaveDialog.DefaultExt := 'ini';
  if not SaveDialog.Execute then Exit;
  SystemManager.ExportSystemsFile(SaveDialog.FileName, True);
end;

procedure TfrmSystemManager.actImportSystemsExecute(Sender: TObject);
begin
  if SystemManager = nil then Exit;

  OpenDialog.Filter := kFSMSystemIniFilter + '|*.ini';
  OpenDialog.DefaultExt := 'ini';
  if not OpenDialog.Execute then Exit;
  SystemManager.ImportSystemsFile(OpenDialog.FileName);
  LoadSystemList;
end;

procedure TfrmSystemManager.actMakeAbsolutePathsExecute(Sender: TObject);

  function MakeAbsolute(const aFolder: string): string;
  begin
    Result := aFolder;

    // CreateRelativePath doesn't like Unix Style under Windows... :-(
    {$IFDEF MSWindows}
    Result := StringReplace(Result, '/', '\', [rfReplaceAll, rfIgnoreCase]);
    {$ENDIF}

    Result := CreateAbsolutePath(Result, GetCurrentDirUTF8);

    {$IFDEF MSWindows}
    Result := StringReplace(Result, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    {$ENDIF}
  end;

var
  i: integer;
begin
  if System = nil then
    Exit;

  System.Icon := MakeAbsolute(System.Icon);
  System.Image := MakeAbsolute(System.Image);
  System.BackgroundImage := MakeAbsolute(System.BackgroundImage);
  System.InfoText := MakeAbsolute(System.InfoText);
  System.TempFolder := MakeAbsolute(System.TempFolder);

  System.BaseFolder := MakeAbsolute(System.BaseFolder);
  System.GameFolder := MakeAbsolute(System.GameFolder);
  System.IconFolder := MakeAbsolute(System.IconFolder);
  System.MarqueeFolder := MakeAbsolute(System.MarqueeFolder);

  for i := 0 to System.ImageFolders.Count - 1 do
    System.ImageFolders[i] := MakeAbsolute(System.ImageFolders[i]);

  for i := 0 to System.TextFolders.Count - 1 do
    System.TextFolders[i] := MakeAbsolute(System.TextFolders[i]);

  System.DemoMusicFolder := MakeAbsolute(System.DemoMusicFolder);
  for i := 0 to System.MusicFolders.Count - 1 do
    System.MusicFolders[i] := MakeAbsolute(System.MusicFolders[i]);
  for i := 0 to System.MusicExecutables.Count - 1 do
    System.MusicExecutables[i] := MakeAbsolute(System.MusicExecutables[i]);

  System.DemoVideoFolder := MakeAbsolute(System.DemoVideoFolder);
  for i := 0 to System.VideoFolders.Count - 1 do
    System.VideoFolders[i] := MakeAbsolute(System.VideoFolders[i]);
  for i := 0 to System.VideoExecutables.Count - 1 do
    System.VideoExecutables[i] := MakeAbsolute(System.VideoExecutables[i]);

  FillFields;
end;

procedure TfrmSystemManager.actMakeRelativePathsExecute(Sender: TObject);

  function MakeRelative(const aFolder: string): string;
  begin
    Result := aFolder;

    // CreateRelativePath doesn't like Unix Style under Windows... :-(
    {$IFDEF MSWindows}
    Result := StringReplace(Result, '/', '\', [rfReplaceAll, rfIgnoreCase]);
    {$ENDIF}

    Result := CreateRelativePath(Result, GetCurrentDirUTF8, true);

    {$IFDEF MSWindows}
    Result := StringReplace(Result, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    {$ENDIF}
  end;

var
  i: integer;
begin
  if System = nil then
    Exit;

  System.Icon := MakeRelative(System.Icon);
  System.Image := MakeRelative(System.Image);
  System.BackgroundImage := MakeRelative(System.BackgroundImage);
  System.InfoText := MakeRelative(System.InfoText);
  System.TempFolder := MakeRelative(System.TempFolder);

  System.BaseFolder := MakeRelative(System.BaseFolder);
  System.GameFolder := MakeRelative(System.GameFolder);
  System.IconFolder := MakeRelative(System.IconFolder);
  System.MarqueeFolder := MakeRelative(System.MarqueeFolder);

  for i := 0 to System.ImageFolders.Count - 1 do
    System.ImageFolders[i] := MakeRelative(System.ImageFolders[i]);

  for i := 0 to System.TextFolders.Count - 1 do
    System.TextFolders[i] := MakeRelative(System.TextFolders[i]);

  System.DemoMusicFolder := MakeRelative(System.DemoMusicFolder);
  for i := 0 to System.MusicFolders.Count - 1 do
    System.MusicFolders[i] := MakeRelative(System.MusicFolders[i]);
  for i := 0 to System.MusicExecutables.Count - 1 do
    System.MusicExecutables[i] := MakeRelative(System.MusicExecutables[i]);

  System.DemoVideoFolder := MakeRelative(System.DemoVideoFolder);
  for i := 0 to System.VideoFolders.Count - 1 do
    System.VideoFolders[i] := MakeRelative(System.VideoFolders[i]);
  for i := 0 to System.VideoExecutables.Count - 1 do
    System.VideoExecutables[i] := MakeRelative(System.VideoExecutables[i]);

  FillFields;
end;

procedure TfrmSystemManager.actRemoveImageFolderExecute(Sender: TObject);
begin
  if System = nil then
    Exit;
  if lbImageFolders.ItemIndex = -1 then
    Exit;

  if System.ImageFolders.Count > lbImageFolders.ItemIndex then
    System.ImageFolders.Delete(lbImageFolders.ItemIndex);

  if System.ImageCaptions.Count > lbImageFolders.ItemIndex then
    System.ImageCaptions.Delete(lbImageFolders.ItemIndex);

  FillFields;
end;

procedure TfrmSystemManager.actRemoveMusicFolderExecute(Sender: TObject);
begin
  if System = nil then
    Exit;
  if lvOtherMusicFolders.ItemIndex = -1 then Exit;

  System.MusicFolders.Delete(lvOtherMusicFolders.ItemIndex);
  System.MusicCaptions.Delete(lvOtherMusicFolders.ItemIndex);
  System.MusicExtensions.Delete(lvOtherMusicFolders.ItemIndex);
  System.MusicExecutables.Delete(lvOtherMusicFolders.ItemIndex);
  System.MusicParameters.Delete(lvOtherMusicFolders.ItemIndex);;

  FillFields;
end;

procedure TfrmSystemManager.actRemoveSystemExecute(Sender: TObject);
var
  Prev: integer;
begin
  if SystemManager = nil then Exit;
  if clbSystems.ItemIndex = -1 then
    Exit;
  SystemManager.RemoveSystem(clbSystems.Items[clbSystems.ItemIndex]);

  Prev := clbSystems.ItemIndex;
  LoadSystemList;
  if clbSystems.Count <= Prev then
    clbSystems.ItemIndex := clbSystems.Count - 1
  else
    clbSystems.ItemIndex := Prev;
  SelectSystem;
end;

procedure TfrmSystemManager.actRemoveTextFolderExecute(Sender: TObject);
begin
  if System = nil then
    Exit;
  if lbTextFolders.ItemIndex = -1 then
    Exit;

  if System.TextFolders.Count > lbTextFolders.ItemIndex then
    System.TextFolders.Delete(lbTextFolders.ItemIndex);

  if System.TextCaptions.Count > lbTextFolders.ItemIndex then
    System.TextCaptions.Delete(lbTextFolders.ItemIndex);

  FillFields;
end;

procedure TfrmSystemManager.actRemoveVideoFolderExecute(Sender: TObject);
begin
  if System = nil then
    Exit;
  if lvOtherVideoFolders.ItemIndex = -1 then Exit;

  System.VideoFolders.Delete(lvOtherVideoFolders.ItemIndex);
  System.VideoCaptions.Delete(lvOtherVideoFolders.ItemIndex);
  System.VideoExtensions.Delete(lvOtherVideoFolders.ItemIndex);
  System.VideoExecutables.Delete(lvOtherVideoFolders.ItemIndex);
  System.VideoParameters.Delete(lvOtherVideoFolders.ItemIndex);;

  FillFields;
end;

procedure TfrmSystemManager.btnOKClick(Sender: TObject);
begin
  EnableCheckedSystems;
  SystemManager.SaveSystemsFile(False);
end;

procedure TfrmSystemManager.cbMainEmulatorSelect(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.MainEmulator := cbMainEmulator.Text;
end;

procedure TfrmSystemManager.chkExtractAllEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.ExtractAll := chkExtractAll.Checked;
end;

procedure TfrmSystemManager.chkUseCRCEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.UseCRC := chkUseCRC.Checked;
end;

procedure TfrmSystemManager.clbOtherEmulatorsClickCheck(Sender: TObject);
var
  Cont: integer;
begin
  if Self.System = nil then
    Exit;
  System.OtherEmulators.Clear;
  Cont := 0;
  while Cont < clbOtherEmulators.Count do
  begin
    if clbOtherEmulators.Checked[Cont] then
      System.OtherEmulators.Add(clbOtherEmulators.Items[Cont]);
    Inc(Cont);
  end;
end;

procedure TfrmSystemManager.clbSystemsClick(Sender: TObject);
begin
  // TODO 3: Until TCheckListBox.OnItemClick works...
  // http://www.lazarus.freepascal.org/index.php?topic=12319.0
  SelectSystem;
end;

procedure TfrmSystemManager.clbSystemsItemClick(Sender: TObject;
  Index: integer);
begin
  // TODO 3: clbSystemsClick code must be here...
end;

procedure TfrmSystemManager.eBackgroundImageAcceptFileName(Sender: TObject;
  var Value: String);
begin
  if System = nil then  Exit;
 System.BackgroundImage := Value;
 ChangeImage(System.BackgroundImage, iBackgroundImage);
end;

procedure TfrmSystemManager.eBackgroundImageEditingDone(Sender: TObject);
begin
  if System = nil then Exit;
  System.BackgroundImage := eBackgroundImage.Text;

  ChangeImage(System.BackgroundImage, iBackgroundImage);
end;

procedure TfrmSystemManager.eBaseFolderAcceptDirectory(Sender: TObject;
  var Value: String);
begin
   if System = nil then
    Exit;
  System.BaseFolder :=Value;
end;

procedure TfrmSystemManager.eBaseFolderEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.BaseFolder := eBaseFolder.Text;
end;

procedure TfrmSystemManager.eCompanyEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.Company := eCompany.Text;
end;

procedure TfrmSystemManager.eDemoMusicFolderAcceptDirectory(Sender: TObject;
  var Value: String);
begin
   if System = nil then
    Exit;
  System.DemoMusicFolder := Value;
end;

procedure TfrmSystemManager.eDemoMusicFolderEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.DemoMusicFolder := eDemoMusicFolder.Text;
end;

procedure TfrmSystemManager.eDemoVideoFolderAcceptDirectory(Sender: TObject;
  var Value: String);
begin
    if System = nil then
    Exit;
  System.DemoVideoFolder := Value;
end;

procedure TfrmSystemManager.eDemoVideoFolderEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.DemoVideoFolder := eDemoVideoFolder.Text;
end;

procedure TfrmSystemManager.eExtensionsEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.Extensions.CommaText := eExtensions.Text;
end;

procedure TfrmSystemManager.eFirstYearEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.FirstYear := eFirstYear.Text;
end;

procedure TfrmSystemManager.eGameFolderAcceptDirectory(Sender: TObject;
  var Value: String);
begin
    if System = nil then
    Exit;
  System.GameFolder := Value;
end;

procedure TfrmSystemManager.eGameFolderEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.GameFolder := eGameFolder.Text;
end;

procedure TfrmSystemManager.eIconFolderAcceptDirectory(Sender: TObject;
  var Value: String);
begin
   if System = nil then
    Exit;
  System.IconFolder := Value;
end;

procedure TfrmSystemManager.eIconFolderEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.IconFolder := eIconFolder.Text;
end;

procedure TfrmSystemManager.eLastYearEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.LastYear := eLastYear.Text;
end;

procedure TfrmSystemManager.eSpineFolderAcceptDirectory(Sender: TObject;
  var Value: String);
begin
   if System = nil then
    Exit;
  System.MarqueeFolder := Value;
end;

procedure TfrmSystemManager.eSpineFolderEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.MarqueeFolder := eSpineFolder.Text;
end;

procedure TfrmSystemManager.eModelEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.Model := eModel.Text;
end;

procedure TfrmSystemManager.eSystemIconAcceptFileName(Sender: TObject;
  var Value: String);
begin
  if System = nil then
    Exit;
  System.Icon := Value;
  ChangeImage(System.Icon, iSystemIcon);
end;

procedure TfrmSystemManager.eSystemIconEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.Icon := eSystemIcon.Text;
  ChangeImage(System.Icon, iSystemIcon);
end;

procedure TfrmSystemManager.eSystemImageAcceptFileName(Sender: TObject;
  var Value: String);
begin
   if System = nil then
    Exit;
  System.Image := Value;
  ChangeImage(System.Image, iSystemImage);
end;

procedure TfrmSystemManager.eSystemImageEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.Image := eSystemImage.Text;
  ChangeImage(System.Image, iSystemImage);
end;

procedure TfrmSystemManager.eSystemTextFileAcceptFileName(Sender: TObject;
  var Value: String);
begin
   if System = nil then
    Exit;
  System.InfoText := Value;
end;

procedure TfrmSystemManager.eSystemTextFileEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.InfoText := eSystemTextFile.Text;
end;

procedure TfrmSystemManager.eTempFolderAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  if System = nil then
    Exit;
  System.TempFolder := Value;
end;

procedure TfrmSystemManager.eTempFolderEditingDone(Sender: TObject);
begin
  if System = nil then
    Exit;
  System.TempFolder := eTempFolder.Text;
end;

procedure TfrmSystemManager.FormCreate(Sender: TObject);
begin
  pcSystemConfig.ActivePageIndex := 0;
end;

procedure TfrmSystemManager.SetConfig(const AValue: cConfig);

  procedure Translate;
  begin
    Self.Caption := Application.Title + ': ' + Self.Caption;
  end;

  procedure LoadEmulatorList;
  var
    EmulatorManager: cEmulatorManager;
  begin
    cbMainEmulator.Clear;
    clbOtherEmulators.Clear;
    EmulatorManager := cEmulatorManager.Create(Config.DataFolder +
      Config.EmulatorsIniFile);
    try
      EmulatorManager.ListEnabledEmulators(clbOtherEmulators.Items);
      EmulatorManager.ListEnabledEmulators(cbMainEmulator.Items);
      cbMainEmulator.Items.Add('');
      cbMainEmulator.Sorted := True;
    finally
      FreeAndNil(EmulatorManager);
    end;

  end;

  //procedure TfrmSystemManager.SetConfig(const AValue: cConfig);
begin
  FConfig := AValue;
  Translate;

  // Iconos de las acciones
  ReadActionsIcons(Config.IconsIniFile, Self.Name, Config.ImagesFolder +
    Config.IconsSubfolder, ilActions, ActionList);

  FreeAndNil(FSystemManager);
  FSystemManager := cSystemManager.Create(Config.DataFolder +
    Config.SystemsIniFile);

  LoadEmulatorList;

  LoadSystemList;
  SelectSystem;
end;

procedure TfrmSystemManager.AddFolderWithCaption(FolderList, CaptionList: TStrings;
  Folder, FolderCaption: String);
begin
  Folder := SetAsFolder(Folder);
  if FolderList.IndexOf(Folder) = -1 then
  begin
    FolderList.Add(Folder);
    CaptionList.Add(ExcludeTrailingPathDelimiter(FolderCaption));
  end;
end;

procedure TfrmSystemManager.SetSystem(const AValue: cSystem);
begin
  FSystem := AValue;
end;

procedure TfrmSystemManager.SetSystemManager(const AValue: cSystemManager);
begin
  FSystemManager := AValue;
end;

procedure TfrmSystemManager.ChangeImage(const aFileName: String;
  aImage: TImage);
begin
  if FileExistsUTF8(aFileName) then
    aImage.Picture.LoadFromFile(aFileName)
  else if FileExistsUTF8(Config.ImagesFolder + Config.DefaultImagesSubfolder +
    Config.DefaultSystemImage) then
    aImage.Picture.LoadFromFile(Config.ImagesFolder +
      Config.DefaultImagesSubfolder + Config.DefaultSystemImage)
  else
    aImage.Picture.Assign(nil);
end;

procedure TfrmSystemManager.EnableCheckedSystems;
var
  Cont: integer;
  aSystem: cSystem;
begin
  // Updating active systems
  // I don't use OnClickCheck because OnClick is called before.
  if SystemManager = nil then Exit;

  Cont := 0;
  while Cont < clbSystems.Count do
  begin
    aSystem := SystemManager.System(clbSystems.Items[Cont]);
    // The system can be deleted, so we need check this
    if aSystem <> nil then
      aSystem.Enabled := clbSystems.Checked[Cont];
    Inc(Cont);
  end;
end;

procedure TfrmSystemManager.LoadSystemList;
var
  i: integer;
begin
  EnableCheckedSystems;
  System := nil;
  clbSystems.Clear;
  ClearFields;

  if SystemManager = nil then
    Exit;

  SystemManager.ListSystems(clbSystems.Items);

  i := 0;
  while i < clbSystems.Count do
  begin
    clbSystems.Checked[i] := SystemManager.System(clbSystems.Items[i]).Enabled;
    Inc(i);
  end;
end;

procedure TfrmSystemManager.SelectSystem;
begin
  System := nil;
  if (clbSystems.ItemIndex <> -1) then
    System := SystemManager.System(clbSystems.Items[clbSystems.ItemIndex]);

  FillFields;
end;

procedure TfrmSystemManager.ClearFields;
var
  i: integer;
begin
  pnlSystem.Caption := rsFSMSelectSystem;

  eCompany.Text := '';
  eModel.Text := '';
  eFirstYear.Text := '';
  eLastYear.Text := '';
  eSystemTextFile.Text := '';

  eExtensions.Text := '';
  chkExtractAll.Checked := False;
  chkUseCRC.Checked := False;

  eTempFolder.Text := '';
  eTempFolder.Enabled := False;
  rgbTempFolder.ItemIndex := 0;

  // Imagenes sistema
  eSystemImage.Text := '';
  iSystemImage.Picture.Assign(nil);
  eBackgroundImage.Text := '';
  iBackgroundImage.Picture.Assign(nil);
  eSystemIcon.Text := '';
  iSystemIcon.Picture.Assign(nil);

  // Directorios base, juegos e imagenes
  eBaseFolder.Text := '';
  eGameFolder.Text := '';
  eIconFolder.Text := '';
  eSpineFolder.Text := '';

  lbImageFolders.Clear;
  mImageCaptions.Clear;

  lbTextFolders.Clear;
  mTextCaptions.Clear;

  // Emuladores
  i := 0;
  while i < clbOtherEmulators.Count do
  begin
    clbOtherEmulators.Checked[i] := False;
    Inc(i);
  end;

  cbMainEmulator.ItemIndex := -1;

  // Musica
  eDemoMusicFolder.Text := '';
  lvOtherMusicFolders.Clear;
  eOtherMusicFolder.Text := '';
  eOtherMusicCaption.Text := '';
  eOtherMusicExtensions.Text := '';
  rgOtherMusicPlayer.ItemIndex := 0;
  lOtherMusicExecutable.Enabled := False;
  eOtherMusicExecutable.Text := '';
  eOtherMusicExecutable.Enabled := False;
  lOtherMusicParameters.Enabled := False;
  eOtherMusicParameters.Text := '';
  eOtherMusicParameters.Enabled := False;


  eDemoVideoFolder.Text := '';
  lvOtherVideoFolders.Clear;
  eOtherVideoFolder.Text := '';
  eOtherVideoCaption.Text := '';
  eOtherVideoExtensions.Text := '';
  rgOtherVideoPlayer.ItemIndex := 0;
  lOtherVideoExecutable.Enabled := False;
  eOtherVideoExecutable.Text := '';
  eOtherVideoExecutable.Enabled := False;
  lOtherVideoParameters.Enabled := False;
  eOtherVideoParameters.Text := '';
  eOtherVideoParameters.Enabled := False;

  { TODO 1: Vaciar los campos otros ficheros }

end;

procedure TfrmSystemManager.FillFields;
var
  i: integer;
  Posicion: integer;
  aItem: TListItem;
begin
  if System = nil then
  begin
    ClearFields;
    Exit;
  end;

  pnlSystem.Caption := System.ID;

  // Pasando datos a los campos
  eCompany.Text := System.Company;
  eModel.Text := System.Model;
  eFirstYear.Text := System.FirstYear;
  eLastYear.Text := System.LastYear;
  if System.InfoText <> '' then
  begin
    eSystemTextFile.Text := System.InfoText;
  end
  else
  begin
    if System.BaseFolder <> '' then
      eSystemTextFile.Text :=
        System.BaseFolder + CleanFileName(Self.System.ID) + '.txt'
    else
      eSystemTextFile.Text := '';
  end;

  eExtensions.Text := System.Extensions.CommaText;
  chkExtractAll.Checked := System.ExtractAll;
  chkUseCRC.Checked := System.UseCRC;

  // This must be go before rgbTempFolder.ItemIndex, as rgbTempFolderClick
  //   is called after chaging it.
  eTempFolder.Text := System.TempFolder;
  if System.TempFolder = '' then
    rgbTempFolder.ItemIndex := 0 // Directorio temporal automático
  else
    rgbTempFolder.ItemIndex := 1; // Directorio temporal personalizado

  // Imagenes sistema
  eSystemImage.Text := System.Image;
  ChangeImage(System.Image, iSystemImage);
  eBackgroundImage.Text := System.BackgroundImage;
  ChangeImage(System.BackgroundImage, iBackgroundImage);
  eSystemIcon.Text := System.Icon;
  ChangeImage(System.Icon, iSystemIcon);

  // Directorios base, juegos e imagenes
  eBaseFolder.Text := System.BaseFolder;
  eGameFolder.Text := System.GameFolder;
  eIconFolder.Text := System.IconFolder;
  eSpineFolder.Text := System.MarqueeFolder;

  lbImageFolders.Clear;
  lbImageFolders.Items.AddStrings(System.ImageFolders);
  mImageCaptions.Clear;
  mImageCaptions.Lines.AddStrings(System.ImageCaptions);

  lbTextFolders.Clear;
  lbTextFolders.Items.AddStrings(System.TextFolders);
  mTextCaptions.Clear;
  mTextCaptions.Lines.AddStrings(System.TextCaptions);

  // Emulators
  cbMainEmulator.ItemIndex :=
    cbMainEmulator.Items.IndexOf(System.MainEmulator);

  for i := 0 to clbOtherEmulators.Count - 1 do
  begin
    Posicion := System.OtherEmulators.IndexOf(clbOtherEmulators.Items[i]);
    clbOtherEmulators.Checked[i] := Posicion <> -1;
  end;
  // Checking the main emulator too
  i := clbOtherEmulators.Items.IndexOf(System.MainEmulator);
  if i <> -1 then
    clbOtherEmulators.Checked[i] := True;


  // Music
  // -----
  eDemoMusicFolder.Text := System.DemoMusicFolder;


  for i := 0 to System.MusicFolders.Count - 1 do
  begin
    aItem := lvOtherMusicFolders.Items.Add;
    aItem.Caption := System.MusicFolders[i];
    aItem.SubItems.Add(System.MusicCaptions[i]);
    aItem.SubItems.Add(System.MusicExtensions[i]);
    aItem.SubItems.Add(System.MusicExecutables[i] + ' ' +
      System.MusicParameters[i]);
  end;

  // Cleaning fields anyway...
  eOtherMusicFolder.Text := '';
  eOtherMusicCaption.Text := '';
  eOtherMusicExtensions.Text := '';
  rgOtherMusicPlayer.ItemIndex := 0;
  lOtherMusicExecutable.Enabled := False;
  eOtherMusicExecutable.Text := '';
  eOtherMusicExecutable.Enabled := False;
  lOtherMusicParameters.Enabled := False;
  eOtherMusicParameters.Text := '';
  eOtherMusicParameters.Enabled := False;

  // Video
  // -----
  eDemoVideoFolder.Text := System.DemoVideoFolder;


  for i := 0 to System.VideoFolders.Count - 1 do
  begin
    aItem := lvOtherVideoFolders.Items.Add;
    aItem.Caption := System.VideoFolders[i];
    aItem.SubItems.Add(System.VideoCaptions[i]);
    aItem.SubItems.Add(System.VideoExtensions[i]);
    aItem.SubItems.Add(System.VideoExecutables[i] + ' ' +
      System.VideoParameters[i]);
  end;

  // Cleaning fields anyway...
  eOtherVideoFolder.Text := '';
  eOtherVideoCaption.Text := '';
  eOtherVideoExtensions.Text := '';
  rgOtherVideoPlayer.ItemIndex := 0;
  lOtherVideoExecutable.Enabled := False;;
  eOtherVideoExecutable.Text := '';
  eOtherVideoExecutable.Enabled := False;
  lOtherVideoParameters.Enabled := False;;
  eOtherVideoParameters.Text := '';
  eOtherVideoParameters.Enabled := False;

  { TODO 1: Fill other files fields. }
end;

initialization
  {$I fSystemManager.lrs}

end.

