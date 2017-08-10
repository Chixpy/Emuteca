unit ufrLEmuTKMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls,
  Graphics, Dialogs, LazUTF8, LCLIntf,
  ActnList, Menus, StdActns, ComCtrls, ExtCtrls, LCLTranslator,
  IniPropStorage, StdCtrls,
  // Misc
  uVersionSupport, u7zWrapper,
  // CHX units
  uCHXStrUtils, uCHXFileUtils, uCHXImageUtils, ucCHXImageList,
  // CHX forms
  ufCHXForm, ufCHXProgressBar,
  // Emuteca clases
  ucEmuteca, uEmutecaCommon, ucEmutecaGroup, ucEmutecaSoftware,
  // Emuteca forms
  ufEmutecaScriptManager, ufrLEmuTKAbout,
  // Emuteca windows
  ufEmutecaActAddSoft, ufEmutecaActAddFolder,
  // LazEmuteca frames
  ufLEmuTKMain, ufLEmuTKSysManager, ufLEmuTKEmuManager, ufLEmuTKMediaManager,
  uGUIConfig, uLEmuTKCommon;

type

  { TfrmLEmuTKMain }

  TfrmLEmuTKMain = class(TForm)
    actEmulatorManager: TAction;
    actAddFolder: TAction;
    actAddSoft: TAction;
    actAutoSave: TAction;
    actExportData: TAction;
    actOpenTempFolder: TAction;
    actSaveLists: TAction;
    actMediaManager: TAction;
    actScriptManager: TAction;
    actSystemManager: TAction;
    ActionList: TActionList;
    FileExit1: TFileExit;
    HelpOnHelp1: THelpOnHelp;
    ActImages: TImageList;
    IniPropStorage: TIniPropStorage;
    MainMenu: TMainMenu;
    mmiExportData: TMenuItem;
    MenuItem2: TMenuItem;
    mmiAbout: TMenuItem;
    mmiAddFiles: TMenuItem;
    mmiScanFolder: TMenuItem;
    mmiAddSoft: TMenuItem;
    MenuItem13: TMenuItem;
    mmiSaveLists: TMenuItem;
    MenuItem15: TMenuItem;
    mmiSaveOnExit: TMenuItem;
    mmiOpenTempFolder: TMenuItem;
    mmiExit: TMenuItem;
    mmiManagers: TMenuItem;
    mmiEmulatorManager: TMenuItem;
    mmiSystemManager: TMenuItem;
    mmiScriptManager: TMenuItem;
    mmiMediaManager: TMenuItem;
    mmiTest: TMenuItem;
    mmiFiles: TMenuItem;
    mmiHelp: TMenuItem;
    mmiFile: TMenuItem;
    stbHelp: TStatusBar;
    procedure actAddSoftExecute(Sender: TObject);
    procedure actAddFolderExecute(Sender: TObject);
    procedure actAutoSaveExecute(Sender: TObject);
    procedure actEmulatorManagerExecute(Sender: TObject);
    procedure actExportDataExecute(Sender: TObject);
    procedure actMediaManagerExecute(Sender: TObject);
    procedure actOpenTempFolderExecute(Sender: TObject);
    procedure actSaveListsExecute(Sender: TObject);
    procedure actScriptManagerExecute(Sender: TObject);
    procedure actSystemManagerExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpOnHelp1Execute(Sender: TObject);

  private
    FfmEmutecaMainFrame: TfmLEmuTKMain;
    FGUIIconsFile: string;
    FSHA1Folder: string;
    FVerIcons: cCHXImageList;
    FEmuteca: cEmuteca;
    FGUIConfig: cGUIConfig;
    FIconList: cCHXImageList;
    FZoneIcons: cCHXImageMap;
    procedure SetGUIIconsFile(AValue: string);
    procedure SetSHA1Folder(AValue: string);

  protected
    property fmEmutecaMainFrame: TfmLEmuTKMain read FfmEmutecaMainFrame;
    //< Main Frame

    property Emuteca: cEmuteca read FEmuteca;
    //< Main Emuteca Core
    property GUIConfig: cGUIConfig read FGUIConfig;
    //< GUI config
    property GUIIconsFile: string read FGUIIconsFile write SetGUIIconsFile;

    property IconList: cCHXImageList read FIconList;
    // Icons for parents, soft, systems and emulators
    property VerIcons: cCHXImageList read FVerIcons;
    // Icons for dump info
    property ZoneIcons: cCHXImageMap read FZoneIcons;
    // Icons of zones

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    procedure LoadIcons;

    procedure LoadEmuteca;
    //< Load Emuteca, remove not saved data.
    procedure SaveEmuteca;
    //< Save Emuteca.

    function AddZoneIcon(aFolder: string; FileInfo: TSearchRec): boolean;
    //< Add Zone icon to list
    function OnProgressBar(const Title, Info1, Info2: string;
      const Value, MaxValue: int64): boolean;
    //< Progress bar call back

  public
    { public declarations }
  end;

var
  frmLEmuTKMain: TfrmLEmuTKMain;

implementation

{$R *.lfm}

{ TfrmLEmuTKMain }

procedure TfrmLEmuTKMain.HelpOnHelp1Execute(Sender: TObject);
begin
  Application.CreateForm(TfrmLEmuTKAbout, frmLEmuTKAbout);
  try
    frmLEmuTKAbout.Caption :=
      Format(rsFmtWindowCaption, [Application.Title, HelpOnHelp1.Caption]);
    frmLEmuTKAbout.Emuteca := Emuteca;
    frmLEmuTKAbout.CachedIcons := IconList;
    frmLEmuTKAbout.ZoneIcons := ZoneIcons;
    frmLEmuTKAbout.VersionIcons := VerIcons;
    frmLEmuTKAbout.UpdateInfo;
    frmLEmuTKAbout.ShowModal;
  finally
    FreeAndNil(frmLEmuTKAbout);
  end;
end;

procedure TfrmLEmuTKMain.SetGUIIconsFile(AValue: string);
begin
  if FGUIIconsFile = AValue then
    Exit;
  FGUIIconsFile := AValue;
end;

procedure TfrmLEmuTKMain.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
end;

procedure TfrmLEmuTKMain.LoadIcons;

  procedure AddIcon(aImageList: cCHXImageList; aIconFile: string);
  begin
    if FileExistsUTF8(aIconFile) then
      aImageList.AddImageFile(aIconFile)
    else
      aImageList.AddImageFile('');
  end;

var
  aFolder, aFile: string;
begin
  ActImages.Clear;

  // Icons for TActions
  ReadActionsIcons(GUIIconsFile, Name, ActImages, ActionList);

  // Icons for menus (without assigned TAction)
  ReadMenuIcons(GUIIconsFile, Name, ActImages, MainMenu);
 {  ReadMenuIcons(GUIIconsFile, Name, ActImages, pmSystemImage);
    ReadMenuIcons(GUIIconsFile, Name, ActImages, pmGameImage);
    ReadMenuIcons(GUIIconsFile, Name, ActImages, pmGameList);
    }

  // Zone icons
  ZoneIcons.Clear;
  aFolder := SetAsAbsoluteFile(GUIConfig.ZoneIcnFolder, ProgramDirectory);
  IterateFolderObj(aFolder, @AddZoneIcon, False);

  // Adding No Zone Icon
  aFolder := SetAsAbsoluteFile(GUIConfig.DefImgFolder, ProgramDirectory);
  ZoneIcons.AddImageFile('', aFolder + 'NoZone.png');

  // Cached Icons (Sys, soft, group, emu)
  IconList.Clear;
 { Icons for games parents and software, first default one
    0: Default for software
    1: Default for parent
    2: Default for system
    3: Default for emulator
  }
  AddIcon(IconList, aFolder + 'SoftIcon.png');
  AddIcon(IconList, aFolder + 'GroupIcon.png');
  AddIcon(IconList, aFolder + 'SysIcon.png');
  AddIcon(IconList, aFolder + 'EmuIcon.png');

  { Icons for "flags" column, see ufEmutecaIcnSoftList.LazEmuTKIconFiles
    0: Verified.png
    1: GoodDump.png
    2: Alternate.png
    3: OverDump.png
    4: BadDump.png
    5: UnderDump.png
    6  Fixed.png
    7: Trainer.png
    8: Translation.png
    9: Pirate.png
    10: Cracked.png
    11: Modified.png
    12: Hack.png
    }
  VerIcons.Clear;
  aFolder := SetAsAbsoluteFile(GUIConfig.DumpIcnFolder, ProgramDirectory);
  for aFile in LazEmuTKIconFiles do
    AddIcon(FVerIcons, aFolder + aFile + '.png');
end;

procedure TfrmLEmuTKMain.LoadEmuteca;
begin
  // Fix runtime errors, while trying to update
  fmEmutecaMainFrame.Emuteca := nil;
  LoadIcons; // Resets cached icons

  Emuteca.LoadData;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.SaveEmuteca;
begin

end;

function TfrmLEmuTKMain.OnProgressBar(const Title, Info1, Info2: string;
  const Value, MaxValue: int64): boolean;
begin
  // We asume that frmCHXProgressBar is always created...
  Result := frmCHXProgressBar.UpdTextAndBar(Title, Info1, Info2,
    Value, MaxValue);
end;

function TfrmLEmuTKMain.AddZoneIcon(aFolder: string;
  FileInfo: TSearchRec): boolean;
begin
  Result := True; // Don't Stop

  // Testing extension
  if GUIConfig.ImageExtensions.IndexOf(UTF8LowerCase(UTF8Copy(
    ExtractFileExt(FileInfo.Name), 2, MaxInt))) = -1 then
    Exit;

  ZoneIcons.AddImageFile(UTF8LowerCase(ExtractFileNameOnly(FileInfo.Name)),
    aFolder + FileInfo.Name);
end;

procedure TfrmLEmuTKMain.FormCreate(Sender: TObject);
begin
  Application.Title := Format(rsFmtApplicationTitle,
    [krsEmuteca, GetFileVersion]); // Usually is deleted in .lpr file...

  // Always work from program folder :P
  // TODO 3: Change for Linux... ¬_¬U
  ChDir(ProgramDirectory);

  // Used to store translations...
  if not DirectoryExistsUTF8('locale') then
    mkdir('locale');

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors
  StandardFormatSettings;

  // Creating ProgressBar
  if not Assigned(frmCHXProgressBar) then
    Application.CreateForm(TfrmCHXProgressBar, frmCHXProgressBar);

  // Windows Caption
  Caption := Format(rsFmtWindowCaption, [Application.Title, Caption]);

  FGUIConfig := cGUIConfig.Create(self);
  GUIConfig.LoadConfig(SetAsAbsoluteFile('GUI.ini', ProgramDirectory));
  IniPropStorage.IniFileName := GUIConfig.ConfigFile;
  IniPropStorage.Restore;

  GUIIconsFile := SetAsAbsoluteFile(GUIConfig.GUIIcnFile, ProgramDirectory);

  // Experimental
  SHA1Folder := SetAsAbsoluteFile(GUIConfig.GlobalCache, ProgramDirectory);
  w7zSetGlobalCache(SHA1Folder);

  // Image lists
  FIconList := cCHXImageList.Create(True);
  FVerIcons := cCHXImageList.Create(True);
  FZoneIcons := cCHXImageMap.Create(True);

  // Creating Emuteca Core :-D
  FEmuteca := cEmuteca.Create(self);
  Emuteca.BaseFolder := ProgramDirectory;
  Emuteca.ProgressCallBack := @OnProgressBar;
  Emuteca.LoadConfig(GUIConfig.EmutecaIni);

  LoadIcons;

  // Creating main frame
  FfmEmutecaMainFrame := TfmLEmuTKMain.Create(Self);
  fmEmutecaMainFrame.IconList := IconList;
  fmEmutecaMainFrame.DumpIcons := VerIcons;
  fmEmutecaMainFrame.ZoneIcons := ZoneIcons;
  fmEmutecaMainFrame.Emuteca := Emuteca;
  fmEmutecaMainFrame.GUIConfig := GUIConfig;
  fmEmutecaMainFrame.GUIIconsIni := GUIConfig.GUIIcnFile;
  fmEmutecaMainFrame.Align := alClient;
  fmEmutecaMainFrame.Parent := Self;

  // Misc
  actAutoSave.Checked := GUIConfig.SaveOnExit; // TODO: Use IniPropStorage?

  if Emuteca.SystemManager.EnabledList.Count = 0 then
    actSystemManager.Execute;
end;

procedure TfrmLEmuTKMain.actEmulatorManagerExecute(Sender: TObject);
var
  aForm: TfrmCHXForm;
  aFrame: TfmLEmuTKEmuManager;
begin
  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmLEmuTKEmuManager';
    aForm.GUIConfigIni := GUIConfig.ConfigFile;
    aForm.Caption := Format(rsFmtWindowCaption,
      [Application.Title, actEmulatorManager.Caption]);

    aFrame := TfmLEmuTKEmuManager.Create(aForm);
    aFrame.EmuManager := Emuteca.EmulatorManager;
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.GUIIconsIni := GUIConfig.GUIIcnFile;
    aFrame.Align := alClient;
    aFrame.Parent := aForm;

    aForm.ShowModal;
  finally
    FreeAndNil(aForm);
  end;
end;

procedure TfrmLEmuTKMain.actExportDataExecute(Sender: TObject);
begin
  //if not Assigned(frmLEmuTKExportData) then
  //  Application.CreateForm(TfrmLEmuTKExportData, frmLEmuTKExportData);

  //frmLEmuTKExportData.GUIConfigIni := GUIConfig.ConfigFile;
  //frmLEmuTKExportData.GUIIconsIni := GUIConfig.GUIIcnFile;
  //frmLEmuTKExportData.Emuteca := Emuteca;

  //frmLEmuTKExportData.ShowModal;
  //FreeAndNil(frmLEmuTKExportData);
end;

procedure TfrmLEmuTKMain.actMediaManagerExecute(Sender: TObject);
begin
  TfmLEmuTKMediaManager.SimpleForm(Emuteca, GUIIconsFile, GUIConfig);
end;

procedure TfrmLEmuTKMain.actOpenTempFolderExecute(Sender: TObject);
begin
  OpenDocument(Emuteca.TempFolder);
end;

procedure TfrmLEmuTKMain.actSaveListsExecute(Sender: TObject);
begin
  SaveEmuteca;
end;

procedure TfrmLEmuTKMain.actScriptManagerExecute(Sender: TObject);
begin
  Application.CreateForm(TfrmEmutecaScriptManager, frmEmutecaScriptManager);

  frmEmutecaScriptManager.IconsIni := GUIConfig.GUIIcnFile;
  frmEmutecaScriptManager.SetBaseFolder(GUIConfig.ScriptsFolder);
  frmEmutecaScriptManager.Emuteca := Emuteca;

  frmEmutecaScriptManager.ShowModal;
  FreeAndNil(frmEmutecaScriptManager);
end;

procedure TfrmLEmuTKMain.actAddFolderExecute(Sender: TObject);
begin
  if TfmEmutecaActAddFolder.SimpleForm(Emuteca, GUIIconsFile,
    GUIConfig.ConfigFile) = mrOk then
  begin
    Emuteca.SaveData;
    LoadEmuteca;
  end;
end;

procedure TfrmLEmuTKMain.actAutoSaveExecute(Sender: TObject);
begin
  GUIConfig.SaveOnExit := actAutoSave.Checked;
end;

procedure TfrmLEmuTKMain.actAddSoftExecute(Sender: TObject);
begin
  if TfmEmutecaActAddSoft.SimpleForm(Emuteca, GUIIconsFile,
    GUIConfig.ConfigFile) = mrOk then
  begin
    Emuteca.SaveData;
    LoadEmuteca;
  end;
end;

procedure TfrmLEmuTKMain.actSystemManagerExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update
  fmEmutecaMainFrame.Emuteca := nil;

  Emuteca.SaveData;

  TfmLEmuTKSysManager.SimpleForm(Emuteca, SHA1Folder, GUIIconsFile,
    GUIConfig.ConfigFile);

  // Load anyway until SysManager creates/destroy systems on the fly.
  LoadEmuteca;
end;

procedure TfrmLEmuTKMain.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  GUIConfig.SaveConfig('');
  if GUIConfig.SaveOnExit then
    Emuteca.SaveData;
  ;
end;

procedure TfrmLEmuTKMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FZoneIcons);
  FreeAndNil(FVerIcons);
  FreeAndNil(FIconList);
  FreeAndNil(FGUIConfig);
  FreeAndNil(FEmuteca);
end;

end.
