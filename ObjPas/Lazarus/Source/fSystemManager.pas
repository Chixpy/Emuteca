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
  uSystemManager, uEmulatorManager, uConfig, uSystem, uTranslator,
  uCustomUtils;


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
    clbImageFolders: TCheckListBox;
    clbOtherEmulators: TCheckListBox;
    clbSystems: TCheckListBox;
    clbTextFolders: TCheckListBox;
    cbMainEmulator: TComboBox;
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
    lOtherEmulators: TLabel;
    lAutoConfig: TLabel;
    lbImageCaptions: TListBox;
    lbTextCaptions: TListBox;
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
    procedure actRemoveImageFolderExecute(Sender: TObject);
    procedure actRemoveMusicFolderExecute(Sender: TObject);
    procedure actRemoveSystemExecute(Sender: TObject);
    procedure actRemoveTextFolderExecute(Sender: TObject);
    procedure actRemoveVideoFolderExecute(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbMainEmulatorSelect(Sender: TObject);
    procedure chkExtractAllEditingDone(Sender: TObject);
    procedure chkUseCRCEditingDone(Sender: TObject);
    procedure clbImageFoldersClickCheck(Sender: TObject);
    procedure clbOtherEmulatorsClickCheck(Sender: TObject);
    procedure clbSystemsClick(Sender: TObject);
    procedure clbSystemsItemClick(Sender: TObject; Index: integer);
    procedure clbTextFoldersClickCheck(Sender: TObject);
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
    FrsAutoConfigSystem: String;
    FrsAutoFolderBIOS: String;
    FrsAutoFolderGames: String;
    FrsAutoFolderIcons: String;
    FrsAutoFolderImg: String;
    FrsAutoFolderImgBack: String;
    FrsAutoFolderImgFront: String;
    FrsAutoFolderImgInGame: String;
    FrsAutoFolderImgMarquee: String;
    FrsAutoFolderImgMedia: String;
    FrsAutoFolderImgTitle: String;
    FrsAutoFolderMusic: String;
    FrsAutoFolderMusicDemo: String;
    FrsAutoFolderMusicMix: String;
    FrsAutoFolderMusicOST: String;
    FrsAutoFolderSaves: String;
    FrsAutoFolderTemp: String;
    FrsAutoFolderTools: String;
    FrsAutoFolderTxt: String;
    FrsAutoFolderTxtCheat: String;
    FrsAutoFolderTxtCredit: String;
    FrsAutoFolderTxtInfo: String;
    FrsAutoFolderTxtManual: String;
    FrsAutoFolderTxtNotes: String;
    FrsAutoFolderVideo: String;
    FrsAutoFolderVideoDemo: String;
    FrsAutoFolderVideoInGame: String;
    FrsAutoFolderVideoTAS: String;
    FrsSelectSystem: String;
    FrsSystemIniFilter: String;
    FrsSystemName: String;
    FSystem: cSystem;
    FSystemManager: cSystemManager;
    procedure SetConfig(const AValue: cConfig);
    procedure SetrsAutoConfigSystem(AValue: String);
    procedure SetrsAutoFolderBIOS(AValue: String);
    procedure SetrsAutoFolderGames(AValue: String);
    procedure SetrsAutoFolderIcons(AValue: String);
    procedure SetrsAutoFolderImg(AValue: String);
    procedure SetrsAutoFolderImgBack(AValue: String);
    procedure SetrsAutoFolderImgFront(AValue: String);
    procedure SetrsAutoFolderImgInGame(AValue: String);
    procedure SetrsAutoFolderImgMarquee(AValue: String);
    procedure SetrsAutoFolderImgMedia(AValue: String);
    procedure SetrsAutoFolderImgTitle(AValue: String);
    procedure SetrsAutoFolderMusic(AValue: String);
    procedure SetrsAutoFolderMusicDemo(AValue: String);
    procedure SetrsAutoFolderMusicMix(AValue: String);
    procedure SetrsAutoFolderMusicOST(AValue: String);
    procedure SetrsAutoFolderSaves(AValue: String);
    procedure SetrsAutoFolderTemp(AValue: String);
    procedure SetrsAutoFolderTools(AValue: String);
    procedure SetrsAutoFolderTxt(AValue: String);
    procedure SetrsAutoFolderTxtCheat(AValue: String);
    procedure SetrsAutoFolderTxtCredit(AValue: String);
    procedure SetrsAutoFolderTxtInfo(AValue: String);
    procedure SetrsAutoFolderTxtManual(AValue: String);
    procedure SetrsAutoFolderTxtNotes(AValue: String);
    procedure SetrsAutoFolderVideo(AValue: String);
    procedure SetrsAutoFolderVideoDemo(AValue: String);
    procedure SetrsAutoFolderVideoInGame(AValue: String);
    procedure SetrsAutoFolderVideoTAS(AValue: String);
    procedure SetrsSelectSystem(AValue: String);
    procedure SetrsSystemIniFilter(AValue: String);
    procedure SetrsSystemName(AValue: String);
    procedure SetSystem(const AValue: cSystem);
    procedure SetSystemManager(const AValue: cSystemManager);

  protected
    // Strings para traducir
    property rsSelectSystem: String read FrsSelectSystem
      write SetrsSelectSystem;
    property rsSystemName: String read FrsSystemName write SetrsSystemName;
    property rsAutoConfigSystem: String
      read FrsAutoConfigSystem write SetrsAutoConfigSystem;
    property rsAutoFolderImg: String
      read FrsAutoFolderImg write SetrsAutoFolderImg;
    property rsAutoFolderImgFront: String
      read FrsAutoFolderImgFront write SetrsAutoFolderImgFront;
    property rsAutoFolderImgBack: String
      read FrsAutoFolderImgBack write SetrsAutoFolderImgBack;
    property rsAutoFolderImgMarquee: String
      read FrsAutoFolderImgMarquee write SetrsAutoFolderImgMarquee;
    property rsAutoFolderImgMedia: String
      read FrsAutoFolderImgMedia write SetrsAutoFolderImgMedia;
    property rsAutoFolderImgTitle: String
      read FrsAutoFolderImgTitle write SetrsAutoFolderImgTitle;
    property rsAutoFolderImgInGame: String
      read FrsAutoFolderImgInGame write SetrsAutoFolderImgInGame;
    property rsAutoFolderIcons: String
      read FrsAutoFolderIcons write SetrsAutoFolderIcons;
    property rsAutoFolderGames: String
      read FrsAutoFolderGames write SetrsAutoFolderGames;
    property rsAutoFolderMusic: String
      read FrsAutoFolderMusic write SetrsAutoFolderMusic;
    property rsAutoFolderMusicDemo: String
      read FrsAutoFolderMusicDemo write SetrsAutoFolderMusicDemo;
    property rsAutoFolderMusicOST: String
      read FrsAutoFolderMusicOST write SetrsAutoFolderMusicOST;
    property rsAutoFolderMusicMix: String
      read FrsAutoFolderMusicMix write SetrsAutoFolderMusicMix;
    property rsAutoFolderTemp: String
      read FrsAutoFolderTemp write SetrsAutoFolderTemp;
    property rsAutoFolderTxt: String
      read FrsAutoFolderTxt write SetrsAutoFolderTxt;
    property rsAutoFolderTxtInfo: String
      read FrsAutoFolderTxtInfo write SetrsAutoFolderTxtInfo;
    property rsAutoFolderTxtManual: String
      read FrsAutoFolderTxtManual write SetrsAutoFolderTxtManual;
    property rsAutoFolderTxtCheat: String
      read FrsAutoFolderTxtCheat write SetrsAutoFolderTxtCheat;
    property rsAutoFolderTxtNotes: String
      read FrsAutoFolderTxtNotes write SetrsAutoFolderTxtNotes;
    property rsAutoFolderTxtCredit: String
      read FrsAutoFolderTxtCredit write SetrsAutoFolderTxtCredit;
    property rsAutoFolderVideo: String
      read FrsAutoFolderVideo write SetrsAutoFolderVideo;
    property rsAutoFolderVideoDemo: String
      read FrsAutoFolderVideoDemo write SetrsAutoFolderVideoDemo;
    property rsAutoFolderVideoTAS: String
      read FrsAutoFolderVideoTAS write SetrsAutoFolderVideoTAS;
    property rsAutoFolderVideoInGame: String
      read FrsAutoFolderVideoInGame write SetrsAutoFolderVideoInGame;
    property rsAutoFolderBIOS: String
      read FrsAutoFolderBIOS write SetrsAutoFolderBIOS;
    property rsAutoFolderSaves: String
      read FrsAutoFolderSaves write SetrsAutoFolderSaves;
    property rsAutoFolderTools: String
      read FrsAutoFolderTools write SetrsAutoFolderTools;
    property rsSystemIniFilter: String
      read FrsSystemIniFilter write SetrsSystemIniFilter;

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
  SystemName := Trim(InputBox(actAddSystem.Caption, rsSystemName, ''));
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
  aItem := lvOtherVideoFolders.Items.Add;

  System.VideoFolders.Add(eOtherVideoFolder.Directory);
  System.VideoCaptions.Add(eOtherVideoCaption.Text);
  System.VideoModes.Add(BoolToStr(chkOtherVideoMultiFile.Checked));
  System.VideoExtensions.Add(eOtherVideoExtensions.Text);
  System.VideoExecutables.Add(eOtherVideoExecutable.Text);
  System.VideoParameters.Add(eOtherVideoParameters.Text);;

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
  aItem := lvOtherMusicFolders.Items.Add;

  System.MusicFolders.Add(eOtherMusicFolder.Directory);
  System.MusicCaptions.Add(eOtherMusicCaption.Text);
  System.MusicModes.Add(BoolToStr(chkOtherMusicMultiFile.Checked));
  System.MusicExtensions.Add(eOtherMusicExtensions.Text);
  System.MusicExecutables.Add(eOtherMusicExecutable.Text);
  System.MusicParameters.Add(eOtherMusicParameters.Text);;

  FillFields;
end;

procedure TfrmSystemManager.actAutoConfigExecute(Sender: TObject);
begin
  if Self.System = nil then
    Exit;
  if (System.BaseFolder = '') or
    (not DirectoryExistsUTF8(System.BaseFolder)) then
    Exit;

  if MessageDlg(Format(rsAutoConfigSystem, [System.BaseFolder]),
    mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Exit;

  // Creating folders
  CreateDirUTF8(System.BaseFolder + rsAutoFolderGames);
  System.GameFolder := System.BaseFolder + rsAutoFolderGames;

  // Images
  CreateDirUTF8(System.BaseFolder + rsAutoFolderImg);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgTitle);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgTitle,
    rsAutoFolderImgTitle);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgInGame);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgInGame,
    rsAutoFolderImgInGame);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgFront);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgFront,
    rsAutoFolderImgFront);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgBack);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgBack,
    rsAutoFolderImgBack);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgMedia);
  AddFolderWithCaption(System.ImageFolders, System.ImageCaptions,
    System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgMedia,
    rsAutoFolderImgMedia);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderImg + rsAutoFolderImgMarquee);
  System.MarqueeFolder := System.BaseFolder + rsAutoFolderImg +
    rsAutoFolderImgMarquee;
  CreateDirUTF8(System.BaseFolder + rsAutoFolderImg + rsAutoFolderIcons);
  System.IconFolder := System.BaseFolder + rsAutoFolderImg + rsAutoFolderIcons;

  // Texts
  CreateDirUTF8(System.BaseFolder + rsAutoFolderTxt);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtInfo);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtInfo,
    rsAutoFolderTxtInfo);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtManual);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtManual,
    rsAutoFolderTxtManual);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtCheat);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtCheat,
    rsAutoFolderTxtCheat);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtNotes);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtNotes,
    rsAutoFolderTxtNotes);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtCredit);
  AddFolderWithCaption(System.TextFolders, System.TextCaptions,
    System.BaseFolder + rsAutoFolderTxt + rsAutoFolderTxtCredit,
    rsAutoFolderTxtCredit);

  // TODO 1: Música
  CreateDirUTF8(System.BaseFolder + rsAutoFolderMusic);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderMusic + rsAutoFolderMusicDemo);
  System.DemoMusicFolder :=
    System.BaseFolder + rsAutoFolderMusic + rsAutoFolderMusicDemo;

  // TODO 1: Videos
  CreateDirUTF8(System.BaseFolder + rsAutoFolderVideo);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderVideo + rsAutoFolderVideoDemo);
  System.DemoVideoFolder :=
    System.BaseFolder + rsAutoFolderVideo + rsAutoFolderVideoDemo;

  // TODO 1: Otros archivos

  // Un par de directorios extra
  CreateDirUTF8(System.BaseFolder + rsAutoFolderBIOS);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderTemp);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderSaves);
  CreateDirUTF8(System.BaseFolder + rsAutoFolderTools);

  // Además configuramos el texto del sistema
  System.InfoText := System.BaseFolder + CleanFileName(Self.System.ID) + '.txt';

  SelectSystem;
end;

procedure TfrmSystemManager.actExportSystemsExecute(Sender: TObject);
begin
  if SystemManager = nil then Exit;

  SaveDialog.Filter := rsSystemIniFilter + '|*.ini';
  SaveDialog.DefaultExt := 'ini';
  if not SaveDialog.Execute then Exit;
  SystemManager.ExportSystemsFile(SaveDialog.FileName, True);
end;

procedure TfrmSystemManager.actImportSystemsExecute(Sender: TObject);
begin
  if SystemManager = nil then Exit;

  OpenDialog.Filter := rsSystemIniFilter + '|*.ini';
  OpenDialog.DefaultExt := 'ini';
  if not OpenDialog.Execute then Exit;
  SystemManager.ImportSystemsFile(OpenDialog.FileName);
  LoadSystemList;
end;

procedure TfrmSystemManager.actRemoveImageFolderExecute(Sender: TObject);
begin
  if System = nil then
    Exit;
  if clbImageFolders.ItemIndex = -1 then
    Exit;

  if System.ImageFolders.Count > clbImageFolders.ItemIndex then
    System.ImageFolders.Delete(clbImageFolders.ItemIndex);

  if System.ImageCaptions.Count > clbImageFolders.ItemIndex then
    System.ImageCaptions.Delete(clbImageFolders.ItemIndex);

  if System.ImageModes.Count > clbImageFolders.ItemIndex then
    System.ImageModes.Delete(clbImageFolders.ItemIndex);

  FillFields;
end;

procedure TfrmSystemManager.actRemoveMusicFolderExecute(Sender: TObject);
begin
  if System = nil then
    Exit;
  if lvOtherMusicFolders.ItemIndex = -1 then Exit;

  System.MusicFolders.Delete(lvOtherMusicFolders.ItemIndex);
  System.MusicCaptions.Delete(lvOtherMusicFolders.ItemIndex);
  System.MusicModes.Delete(lvOtherMusicFolders.ItemIndex);
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
  if clbTextFolders.ItemIndex = -1 then
    Exit;

  if System.TextFolders.Count > clbTextFolders.ItemIndex then
    System.TextFolders.Delete(clbTextFolders.ItemIndex);

  if System.TextCaptions.Count > clbTextFolders.ItemIndex then
    System.TextCaptions.Delete(clbTextFolders.ItemIndex);

  if System.TextModes.Count > clbTextFolders.ItemIndex then
    System.TextModes.Delete(clbTextFolders.ItemIndex);

  FillFields;
end;

procedure TfrmSystemManager.actRemoveVideoFolderExecute(Sender: TObject);
begin
  if System = nil then
    Exit;
  if lvOtherVideoFolders.ItemIndex = -1 then Exit;

  System.VideoFolders.Delete(lvOtherVideoFolders.ItemIndex);
  System.VideoCaptions.Delete(lvOtherVideoFolders.ItemIndex);
  System.VideoModes.Delete(lvOtherVideoFolders.ItemIndex);
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

procedure TfrmSystemManager.clbImageFoldersClickCheck(Sender: TObject);

var
  Cont: integer;
begin
  if System = nil then
    Exit;
  // Updating Image modes
  // TODO 3: Hacer un método generico, ya que lo textos usan uno igual,
  //   vendría bien...
  Cont := 0;
  System.ImageModes.Clear;
  while Cont < clbImageFolders.Count do
  begin
    if clbImageFolders.Checked[Cont] then
      System.ImageModes.Add(BoolToStr(True))
    else
      System.ImageModes.Add(BoolToStr(False));
    Inc(Cont);
  end;
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
  ShowMessage('Por fin entra en TfrmSystemManager.clbSystemsItemClick');
end;

procedure TfrmSystemManager.clbTextFoldersClickCheck(Sender: TObject);
var
  Cont: integer;
begin
  if System = nil then
    Exit;
  Cont := 0;
  System.TextModes.Clear;
  while Cont < clbTextFolders.Count do
  begin
    if clbTextFolders.Checked[Cont] then
      System.TextModes.Add(BoolToStr(True))
    else
      System.TextModes.Add(BoolToStr(False));
    Inc(Cont);
  end;
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
  var
    Translator: cTranslator;
  begin
    Translator := cTranslator.Create(Config.LanguageFolder +
      Config.LanguageFile);
    try
      Translator.Section := Self.Name;
      {Self.Caption := }Translator.Translate(Self);

      // Título y paneles
      Self.Caption := Application.Title + ': ' + Self.Caption;

      // Cadenas de texto
      rsSelectSystem := Translator.Translate('rsSelectSystem',
        'Select a system.');
      rsSystemName := Translator.Translate('rsSystemName', 'System name');
      rsAutoConfigSystem :=
        Translator.Translate('rsAutoConfigSystem',
        'This action will create many subfolders in:' +
        sLineBreak + '%s' + sLineBreak + 'Are you sure?');
      rsAutoFolderImg := Translator.Translate('rsAutoFolderImages', 'Images');
      rsAutoFolderImgTitle :=
        Translator.Translate('rsAutoFolderImgTitle', 'Title');
      rsAutoFolderImgInGame :=
        Translator.Translate('rsAutoFolderImgInGame', 'In game');
      rsAutoFolderImgFront :=
        Translator.Translate('rsAutoFolderImgFront', 'Front');
      rsAutoFolderImgMarquee :=
        Translator.Translate('rsAutoFolderImgSpine', 'Spine');
      rsAutoFolderImgBack :=
        Translator.Translate('rsAutoFolderImgBack', 'Back');
      rsAutoFolderImgMedia :=
        Translator.Translate('rsAutoFolderImgMedia', 'Media');
      rsAutoFolderIcons := Translator.Translate('rsAutoFolderIcons', 'Icon');
      rsAutoFolderGames := Translator.Translate('rsAutoFolderGames', 'Games');
      rsAutoFolderMusic := Translator.Translate('rsAutoFolderMusic', 'Music');
      rsAutoFolderMusicDemo :=
        Translator.Translate('rsAutoFolderMusicDemo', 'Demo');
      rsAutoFolderMusicOST :=
        Translator.Translate('rsAutoFolderMusicOST', 'OST');
      rsAutoFolderMusicMix :=
        Translator.Translate('rsAutoFolderMusicMix', 'Mix');
      rsAutoFolderTxt := Translator.Translate('rsAutoFolderTexts', 'Texts');
      rsAutoFolderTemp := Translator.Translate('rsAutoFolderTemp', 'Temp');
      rsAutoFolderTxtInfo :=
        Translator.Translate('rsAutoFolderTxtInfo', 'Information');
      rsAutoFolderTxtManual :=
        Translator.Translate('rsAutoFolderTxtInstr', 'Instructions');
      rsAutoFolderTxtCheat :=
        Translator.Translate('rsAutoFolderTxtCheats', 'Cheats');
      rsAutoFolderTxtNotes :=
        Translator.Translate('rsAutoFolderTxtNotes', 'Notes');
      rsAutoFolderTxtCredit :=
        Translator.Translate('rsAutoFolderTxtCredits', 'Credits');
      rsAutoFolderVideo := Translator.Translate('rsAutoDirVideo', 'Videos');
      rsAutoFolderVideoDemo :=
        Translator.Translate('rsAutoFolderVideoDemo', 'Demo');
      rsAutoFolderVideoTAS :=
        Translator.Translate('rsAutoFolderVideoInputs', 'Inputs');
      rsAutoFolderVideoInGame :=
        Translator.Translate('rsAutoFolderVideoGame', 'In game');
      rsAutoFolderBIOS := Translator.Translate('rsAutoFolderBIOS', 'BIOS');
      rsAutoFolderSaves := Translator.Translate('rsAutoFolderSaves', 'Saves');
      rsAutoFolderTools := Translator.Translate('rsAutoFolderTools', 'Tools');
      rsSystemIniFilter :=
        Translator.Translate('rsSystemsIniFile', 'Systems Ini File (*.ini)');
    finally
      FreeAndNil(Translator);
    end;
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

procedure TfrmSystemManager.SetrsAutoConfigSystem(AValue: String);
begin
  FrsAutoConfigSystem := AValue;
end;

procedure TfrmSystemManager.SetrsAutoFolderBIOS(AValue: String);
begin
  FrsAutoFolderBIOS := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderGames(AValue: String);
begin
  FrsAutoFolderGames := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderIcons(AValue: String);
begin
  FrsAutoFolderIcons := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderImg(AValue: String);
begin
  FrsAutoFolderImg := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderImgBack(AValue: String);
begin
  FrsAutoFolderImgBack := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderImgFront(AValue: String);
begin
  FrsAutoFolderImgFront := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderImgInGame(AValue: String);
begin
  FrsAutoFolderImgInGame := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderImgMarquee(AValue: String);
begin
  FrsAutoFolderImgMarquee := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderImgMedia(AValue: String);
begin
  FrsAutoFolderImgMedia := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderImgTitle(AValue: String);
begin
  FrsAutoFolderImgTitle := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderMusic(AValue: String);
begin
  FrsAutoFolderMusic := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderMusicDemo(AValue: String);
begin
  FrsAutoFolderMusicDemo := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderMusicMix(AValue: String);
begin
  FrsAutoFolderMusicMix := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderMusicOST(AValue: String);
begin
  FrsAutoFolderMusicOST := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderSaves(AValue: String);
begin
  FrsAutoFolderSaves := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderTemp(AValue: String);
begin
  FrsAutoFolderTemp := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderTools(AValue: String);
begin
  FrsAutoFolderTools := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderTxt(AValue: String);
begin
  FrsAutoFolderTxt := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderTxtCheat(AValue: String);
begin
  FrsAutoFolderTxtCheat := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderTxtCredit(AValue: String);
begin
  FrsAutoFolderTxtCredit := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderTxtInfo(AValue: String);
begin
  FrsAutoFolderTxtInfo := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderTxtManual(AValue: String);
begin
  FrsAutoFolderTxtManual := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderTxtNotes(AValue: String);
begin
  FrsAutoFolderTxtNotes := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderVideo(AValue: String);
begin
  FrsAutoFolderVideo := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderVideoDemo(AValue: String);
begin
  FrsAutoFolderVideoDemo := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderVideoInGame(AValue: String);
begin
  FrsAutoFolderVideoInGame := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsAutoFolderVideoTAS(AValue: String);
begin
  FrsAutoFolderVideoTAS := SetAsFolder(AValue);
end;

procedure TfrmSystemManager.SetrsSelectSystem(AValue: String);
begin
  FrsSelectSystem := AValue;
end;

procedure TfrmSystemManager.SetrsSystemIniFilter(AValue: String);
begin
  FrsSystemIniFilter := AValue;
end;

procedure TfrmSystemManager.SetrsSystemName(AValue: String);
begin
  FrsSystemName := AValue;
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
  pnlSystem.Caption := rsSelectSystem;

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

  clbImageFolders.Clear;
  lbImageCaptions.Clear;

  clbTextFolders.Clear;
  lbTextCaptions.Clear;

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

  clbImageFolders.Clear;
  clbImageFolders.Items.AddStrings(System.ImageFolders);
  lbImageCaptions.Clear;
  lbImageCaptions.Items.AddStrings(System.ImageCaptions);
  i := 0;
  while i < System.ImageModes.Count do
  begin
    if StrToBoolDef(System.ImageModes[i], False) then
      clbImageFolders.Checked[i] := True;
    Inc(i);
  end;

  clbTextFolders.Clear;
  clbTextFolders.Items.AddStrings(System.TextFolders);
  lbTextCaptions.Clear;
  lbTextCaptions.Items.AddStrings(System.TextCaptions);
  i := 0;
  while i < System.TextModes.Count do
  begin
    if StrToBoolDef(System.TextModes[i], False) then
      clbTextFolders.Checked[i] := True;
    Inc(i);
  end;

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
    if StrToBool(System.MusicModes[i]) then
      aItem.SubItems.Add('*')
    else
      aItem.SubItems.Add('');
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
    if StrToBool(System.VideoModes[i]) then
      aItem.SubItems.Add('*')
    else
      aItem.SubItems.Add('');
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

