unit ufrETKGUIMain;

{< TfrmETKGUIMain form unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2011-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LazFileUtils, LCLIntf, IniFiles, LCLTranslator, ActnList, StdActns, Menus,
  ComCtrls, LazUTF8, Clipbrd,
  // CHX units
  uCHXRscStr, uCHX7zWrapper, uCHXStrUtils, uCHXFileUtils, uCHXImageUtils,
  uCHXMenuUtils, uCHXExecute, uCHXVerInfo,
  // CHX classes
  ucCHXImageList,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXProgressBar,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core clases
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftware, ucEmutecaSoftList, ucEmutecaEmulator,
  // Emuteca Core frames
  ufEmutecaActAddSoft, ufEmutecaActAddFolder, ufEmutecaActExportSoftData,
  ufEmutecaActImportSoftData, ufEmutecaGroupEditor,
  // Emuteca GUI units
  uETKGUIConst, uETKGUIRscStr,
  // Emuteca GUI classes
  ucETKGUIConfig,
  // Emuteca GUI frames
  ufETKGUIMain, ufETKGUISysManager, ufETKGUIEmuManager, ufETKGUIMediaManager,
  ufETKGUIScriptManager, ufETKGUIFullSysEditor, ufETKGUIFullEmuEditor,
  ufETKGUIactMergeGroup,
  // Emuteca GUI forms
  ufETKGUIAbout,
  // Emuteca GUI threads
  utETKGUICacheSysIcons, utETKGUICacheGrpIcons, utETKGUICacheSoftIcons,
  utETKGUICacheEmuIcons;

type

  { TfrmETKGUIMain }

  TfrmETKGUIMain = class(TfrmCHXForm)
    actAddFolder: TAction;
    actAddSoft: TAction;
    actCleanSystemData: TAction;
    actEditEmulator: TAction;
    actEditSystem: TAction;
    actEmulatorManager: TAction;
    actExportSoftData: TAction;
    ActImages: TImageList;
    actImportSoftData: TAction;
    actEditGroup: TAction;
    actExitWOSaving: TAction;
    actCopyFileName2ClipBoard: TAction;
    actRunDBEditorSoft: TAction;
    actRunDBEditorGroups: TAction;
    actRunETKIconBorderLogo: TAction;
    actRunETKIconBorderIcon: TAction;
    actRemoveSoft: TAction;
    actOpenEmulatorWeb: TAction;
    actRunEmulatorAlone: TAction;
    ActionList: TActionList;
    actMediaManager: TAction;
    actMergeGroupFiles: TAction;
    actOpen7zCacheFolder: TAction;
    actOpenEmulatorFolder: TAction;
    actOpenEmutecaFolder: TAction;
    actOpenSoftFolder: TAction;
    actOpenSystemBaseFolder: TAction;
    actOpenTempFolder: TAction;
    actRunSoftware: TAction;
    actSaveLists: TAction;
    actScriptManager: TAction;
    actSystemManager: TAction;
    actUpdateGroupList: TAction;
    actExit: TFileExit;
    HelpOnHelp1: THelpOnHelp;
    MainMenu: TMainMenu;
    mipmSCopyFileName2ClipBoard: TMenuItem;
    mimmRunDBEditorSoft: TMenuItem;
    mimmRunDBEditorGroups: TMenuItem;
    mimmExit: TMenuItem;
    mimmRunETKIconBorderIcon: TMenuItem;
    mimmRunETKIconBorderLogo: TMenuItem;
    mimmTools: TMenuItem;
    mipmGEditGroup: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mipmMergeGroupFiles2: TMenuItem;
    mipmSDeleteSoft: TMenuItem;
    mimmOpenEmulatorWeb: TMenuItem;
    mimmSearchInternetE: TMenuItem;
    mimmSearchInternetS: TMenuItem;
    mipmGSearchInternet: TMenuItem;
    mimmRunEmulatorAlone: TMenuItem;
    mimmAbout: TMenuItem;
    mimmAddFiles: TMenuItem;
    mimmAddSoft: TMenuItem;
    mimmAddSoftFolder: TMenuItem;
    mimmCleanSystem: TMenuItem;
    mimmDebug: TMenuItem;
    mimmEditEmulator: TMenuItem;
    mimmEditSystem: TMenuItem;
    mimmEmulator: TMenuItem;
    mimmEmulatorManager: TMenuItem;
    mimmExitWSaving: TMenuItem;
    mimmExport: TMenuItem;
    mimmFile: TMenuItem;
    mimmGroup: TMenuItem;
    mimmHelp: TMenuItem;
    mimmImport: TMenuItem;
    mimmImportExport: TMenuItem;
    mimmManagers: TMenuItem;
    mimmMediaManager: TMenuItem;
    mimmOpen7zCacheFolder: TMenuItem;
    mimmOpenEmulatorFolder: TMenuItem;
    mimmOpenEmutecaFolder: TMenuItem;
    mimmOpenSystemBaseFolder: TMenuItem;
    mimmOpenTempFolder: TMenuItem;
    mimmSaveLists: TMenuItem;
    mimmScriptManager: TMenuItem;
    mimmSoft: TMenuItem;
    mimmSystem: TMenuItem;
    mimmSystemManager: TMenuItem;
    mimmUpdateSystemGroups: TMenuItem;
    mipmSOpenSoftFolder: TMenuItem;
    mipmSRunSoft: TMenuItem;
    pmGroup: TPopupMenu;
    pmSoft: TPopupMenu;
    Separator1: TMenuItem;
    stbHelp: TStatusBar;
    procedure actAddFolderExecute(Sender: TObject);
    procedure actAddSoftExecute(Sender: TObject);
    procedure actCleanSystemDataExecute(Sender: TObject);
    procedure actCopyFileName2ClipBoardExecute(Sender: TObject);
    procedure actEditEmulatorExecute(Sender: TObject);
    procedure actEditGroupExecute(Sender: TObject);
    procedure actEditSystemExecute(Sender: TObject);
    procedure actEmulatorManagerExecute(Sender: TObject);
    procedure actExitWOSavingExecute(Sender: TObject);
    procedure actExportSoftDataExecute(Sender: TObject);
    procedure actImportSoftDataExecute(Sender: TObject);
    procedure actMediaManagerExecute(Sender: TObject);
    procedure actMergeGroupFilesExecute(Sender: TObject);
    procedure actOpen7zCacheFolderExecute(Sender: TObject);
    procedure actOpenEmulatorFolderExecute(Sender: TObject);
    procedure actOpenEmulatorWebExecute(Sender: TObject);
    procedure actOpenEmutecaFolderExecute(Sender: TObject);
    procedure actOpenSoftFolderExecute(Sender: TObject);
    procedure actOpenSystemBaseFolderExecute(Sender: TObject);
    procedure actOpenTempFolderExecute(Sender: TObject);
    procedure actRemoveSoftExecute(Sender: TObject);
    procedure actRunDBEditorExecute(Sender: TObject);
    procedure actRunDBEditorGroupsExecute(Sender: TObject);
    procedure actRunDBEditorSoftExecute(Sender: TObject);
    procedure actRunEmulatorAloneExecute(Sender: TObject);
    procedure actRunETKIconBorderIconExecute(Sender: TObject);
    procedure actRunETKIconBorderLogoExecute(Sender: TObject);
    procedure actRunSoftwareExecute(Sender: TObject);
    procedure actSaveListsExecute(Sender: TObject);
    procedure actScriptManagerExecute(Sender: TObject);
    procedure actSystemManagerExecute(Sender: TObject);
    procedure actUpdateGroupListExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpOnHelp1Execute(Sender: TObject);

  private
    FBaseFolder: string;
    FCacheEmuIconsThread: ctETKGUICacheEmuIcons;
    FCacheGrpIconsThread: ctEGUICacheGrpIcons;
    FCacheSoftIconsThread: ctEGUICacheSoftIcons;
    FCacheSysIconsThread: ctEGUICacheSysIcons;
    FCurrentEmu: cEmutecaEmulator;
    FCurrentGroup: cEmutecaGroup;
    FCurrentSoft: cEmutecaSoftware;
    FCurrentSystem: cEmutecaSystem;
    FDumpIcons: cCHXImageList;
    FEmuteca: cEmuteca;
    FfmEmutecaMainFrame: TfmETKGUIMain;
    FfmProgressBar: TfmCHXProgressBar;
    FGUIConfig: cETKGUIConfig;
    FGUIIconsFile: string;
    FIconList: cCHXImageList;
    FSaveOnExit: boolean;
    FSHA1Folder: string;
    Fw7zErrorFileName: string;
    FZoneIcons: cCHXImageMap;
    procedure SetBaseFolder(AValue: string);
    procedure SetCacheEmuIconsThread(const AValue: ctETKGUICacheEmuIcons);
    procedure SetCacheGrpIconsThread(AValue: ctEGUICacheGrpIcons);
    procedure SetCacheSoftIconsThread(AValue: ctEGUICacheSoftIcons);
    procedure SetCacheSysIconsThread(AValue: ctEGUICacheSysIcons);
    procedure SetCurrentEmu(aCurrentEmu: cEmutecaEmulator);
    procedure SetCurrentGroup(AValue: cEmutecaGroup);
    procedure SetCurrentSoft(AValue: cEmutecaSoftware);
    procedure SetCurrentSystem(AValue: cEmutecaSystem);
    procedure SetGUIIconsFile(AValue: string);
    procedure SetSaveOnExit(AValue: boolean);
    procedure SetSHA1Folder(AValue: string);
    procedure Setw7zErrorFileName(AValue: string);

  protected
    property fmEmutecaMainFrame: TfmETKGUIMain read FfmEmutecaMainFrame;
    //< Main Frame
    property fmProgressBar: TfmCHXProgressBar read FfmProgressBar;
    //< General Progress Bar

    property Emuteca: cEmuteca read FEmuteca;
    //< Main Emuteca Core
    property GUIConfig: cETKGUIConfig read FGUIConfig;
    //< GUI config

    property IconList: cCHXImageList read FIconList;
    //< Icons for parents, soft, systems and emulators
    property DumpIcons: cCHXImageList read FDumpIcons;
    //< Icons for dump info
    property ZoneIcons: cCHXImageMap read FZoneIcons;
    //< Icons for zones

    property CurrentSystem: cEmutecaSystem
      read FCurrentSystem write SetCurrentSystem;
    property CurrentGroup: cEmutecaGroup read FCurrentGroup
      write SetCurrentGroup;
    property CurrentSoft: cEmutecaSoftware
      read FCurrentSoft write SetCurrentSoft;
    property CurrentEmu: cEmutecaEmulator read FCurrentEmu write SetCurrentEmu;


    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;
    //< Global Cache folder
    property w7zErrorFileName: string read Fw7zErrorFileName
      write Setw7zErrorFileName;
    //< File for w7z errors and warnings

    property SaveOnExit: boolean read FSaveOnExit write SetSaveOnExit;

    property CacheSysIconsThread: ctEGUICacheSysIcons
      read FCacheSysIconsThread write SetCacheSysIconsThread;
    procedure CacheSysIconsThreadTerminated(Sender: TObject);
    //< For use with TThread.OnTerminate, auto nil.
    property CacheEmuIconsThread: ctETKGUICacheEmuIcons
      read FCacheEmuIconsThread write SetCacheEmuIconsThread;
    procedure CacheEmuIconsThreadTerminated(Sender: TObject);
    //< For use with TThread.OnTerminate, auto nil.
    property CacheGrpIconsThread: ctEGUICacheGrpIcons
      read FCacheGrpIconsThread write SetCacheGrpIconsThread;
    procedure CacheGrpIconsThreadTerminated(Sender: TObject);
    //< For use with TThread.OnTerminate, auto nil.
    property CacheSoftIconsThread: ctEGUICacheSoftIcons
      read FCacheSoftIconsThread write SetCacheSoftIconsThread;
    procedure CacheSoftIconsThreadTerminated(Sender: TObject);
    //< For use with TThread.OnTerminate, auto nil.

    procedure LoadIcons;
    procedure LoadSystemsIcons;
    procedure LoadEmuIcons;
    function AddZoneIcon(aFolder: string; FileInfo: TSearchRec): boolean;
    //< Add Zone icon to list
    procedure LoadGrpIcons(aGroupList: cEmutecaGroupList);
    procedure LoadSoftIcons(aSoftList: cEmutecaSoftList);

    procedure LoadSearchLinks;
    procedure SearchInInternet(Sender: TObject);

    procedure RunSoftware(aSoftware: cEmutecaSoftware);
    //< Run a software

    function DoProgressBar(const Title, Info: string;
      const Value, MaxValue: int64; const IsCancelable: boolean): boolean;
    //< Progress bar call back

    procedure DoChangeGrpList(aGroupList: cEmutecaGroupList);

    procedure DoLoadGUIIcons(aIniFile: TIniFile;
      const aBaseFolder: string); override;


  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    {< Base folder for Emuteca data.

       It was ProgramDirectory, but with the program in a subfolder it's
          ProgramDirectory's parent folder.
    }

    property GUIIconsFile: string read FGUIIconsFile write SetGUIIconsFile;
  end;

var
  frmETKGUIMain: TfrmETKGUIMain;

implementation

{$R *.lfm}

{ TfrmETKGUIMain }

procedure TfrmETKGUIMain.SetCurrentGroup(AValue: cEmutecaGroup);
begin
  if FCurrentGroup = AValue then
    Exit;
  FCurrentGroup := AValue;

  mimmGroup.Enabled := Assigned(CurrentGroup);

  if Assigned(CurrentGroup) then
  begin
    LoadSoftIcons(CurrentGroup.SoftList);
    CurrentSystem := cEmutecaSystem(CurrentGroup.CachedSystem);

    actMergeGroupFiles.Enabled := CurrentSystem.MergeableGroups;
  end
  else
  begin
    actMergeGroupFiles.Enabled := False;
  end;
end;

procedure TfrmETKGUIMain.FormCreate(Sender: TObject);

  procedure CloneMenus;
  var
    aItem: TMenuItem;
  begin
    // Copying PopUp menús to Main menú.
    AddSubMenu(pmGroup, mimmGroup);
    AddSubMenu(pmSoft, mimmSoft);

    // Adding Group, System and Emulator from Main menú to popups.
    aItem := TMenuItem.Create(pmGroup);
    aItem.Caption := '-';
    pmGroup.Items.Add(aItem);
    aItem := CloneMenuItem(mimmSystem);
    aItem.Enabled := True; // It can be always enabled in popup menu
    pmGroup.Items.Add(aItem);
    aItem := TMenuItem.Create(pmGroup);
    aItem.Caption := '-';
    pmGroup.Items.Add(aItem);
    aItem := CloneMenuItem(mimmEmulator);
    aItem.Enabled := True; // It can be always enabled in popup menu
    pmGroup.Items.Add(aItem);

    aItem := TMenuItem.Create(pmSoft);
    aItem.Caption := '-';
    pmSoft.Items.Add(aItem);
    aItem := CloneMenuItem(mimmGroup);
    aItem.Enabled := True; // It can be always enabled in popup menu
    pmSoft.Items.Add(aItem);
    aItem := TMenuItem.Create(pmSoft);
    aItem.Caption := '-';
    pmSoft.Items.Add(aItem);
    aItem := CloneMenuItem(mimmSystem);
    aItem.Enabled := True; // It can be always enabled in popup menu
    pmSoft.Items.Add(aItem);
    aItem := TMenuItem.Create(pmSoft);
    aItem.Caption := '-';
    pmSoft.Items.Add(aItem);
    aItem := CloneMenuItem(mimmEmulator);
    aItem.Enabled := True; // It can be always enabled in popup menu
    pmSoft.Items.Add(aItem);
  end;

begin
  // Title of application, usually it's autodeleted in .lpr file...
  Application.Title := Format(rsFmtApplicationTitle,
    [Application.Title, GetFileVersion]);

  // Changing base folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
  ChDir(BaseFolder);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  // Loading GUI config
  FGUIConfig := cETKGUIConfig.Create(self);
  GUIConfig.DefaultFileName := SetAsAbsoluteFile('GUI.ini', BaseFolder);
  GUIConfig.LoadFromFile('');

  // Loading translation
  if not DirectoryExistsUTF8(BaseFolder + GUIConfig.LangFolder) then
    mkdir(BaseFolder + GUIConfig.LangFolder);
  SetDefaultLang('', BaseFolder + GUIConfig.LangFolder);

  // Experimental:
  //   - 7z files cache folder
  //   - 7z error logs file
  SHA1Folder := SetAsAbsoluteFile(GUIConfig.GlobalCache, BaseFolder);
  w7zErrorFileName := SetAsAbsoluteFile(GUIConfig.w7zErrorFileName,
    BaseFolder);

  // Image lists
  FIconList := cCHXImageList.Create(True);
  FDumpIcons := cCHXImageList.Create(True);
  FZoneIcons := cCHXImageMap.Create(True);

  // Creating ProgressBar form
  FfmProgressBar := TfmCHXProgressBar.SimpleForm(GUIConfig.DefaultFileName);

  // Creating Emuteca Core :-D
  FEmuteca := cEmuteca.Create(self);
  Emuteca.BaseFolder := BaseFolder;
  Emuteca.ProgressCallBack := @DoProgressBar;
  Emuteca.LoadConfig(SetAsAbsoluteFile(GUIConfig.EmutecaIni, BaseFolder));

  // This must be after creating and loading Emuteca,
  //   it runs CacheSysIconsThread too
  LoadIcons;

  // Loading search links
  LoadSearchLinks;

  // Copying menu items.
  CloneMenus;

  // Creating main frame
  FfmEmutecaMainFrame := TfmETKGUIMain.Create(Self);
  fmEmutecaMainFrame.OnSystemChanged := @SetCurrentSystem;
  fmEmutecaMainFrame.OnGrpListChanged := @DoChangeGrpList;
  fmEmutecaMainFrame.OnGroupChanged := @SetCurrentGroup;
  fmEmutecaMainFrame.OnSoftChanged := @SetCurrentSoft;
  fmEmutecaMainFrame.OnSoftDblClk := @RunSoftware;
  fmEmutecaMainFrame.OnEmulatorChanged := @SetCurrentEmu;
  fmEmutecaMainFrame.pmGroup := pmGroup;
  fmEmutecaMainFrame.pmSoft := pmSoft;
  fmEmutecaMainFrame.DumpIcons := DumpIcons;
  fmEmutecaMainFrame.ZoneIcons := ZoneIcons;
  fmEmutecaMainFrame.Emuteca := Emuteca;
  fmEmutecaMainFrame.SHA1Folder := SHA1Folder;
  fmEmutecaMainFrame.GUIConfig := GUIConfig;
  fmEmutecaMainFrame.Align := alClient;

  GUIIconsFile := GUIConfig.GUIIcnFile;
  LoadGUIIcons(GUIIconsFile);
  LoadGUIConfig(GUIConfig.DefaultFileName);

  fmEmutecaMainFrame.Parent := Self;

  SaveOnExit := True;

  // if there is not enabled systems then open SysManager
  if Emuteca.SystemManager.EnabledList.Count = 0 then
    actSystemManager.Execute;
end;

procedure TfrmETKGUIMain.FormDestroy(Sender: TObject);
begin
  ZoneIcons.Free;
  DumpIcons.Free;
  IconList.Free;
  GUIConfig.Free;
  Emuteca.Free;
end;

procedure TfrmETKGUIMain.FormShow(Sender: TObject);
begin
  // Windows Caption
  // It is removed if set in OnCreate ¿?
  Self.Caption := Format(krsFmtWindowCaption, [Application.Title, Caption]);
end;

procedure TfrmETKGUIMain.HelpOnHelp1Execute(Sender: TObject);
begin
  TfmETKGUIAbout.SimpleModalForm(Emuteca, IconList, DumpIcons, ZoneIcons,
    GUIConfig.DefaultFileName, GUIIconsFile);
end;

procedure TfrmETKGUIMain.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  GUIConfig.SaveToFile('', False); // File has Forms config too, don't delete

  if SaveOnExit then Emuteca.SaveAllData;

  // If CacheSysIconsThread is not terminated, maybe last SHA1 is not saved;
  //   but at least not all work is losed on error...

  // Teminate threads if they are running.
  if Assigned(CacheSoftIconsThread) then
  begin
    CacheSoftIconsThread.OnTerminate := nil;
    CacheSoftIconsThread.Terminate;
    CacheSoftIconsThread.WaitFor;
  end;
  // CacheSoftIconsThread.Free; Auto freed with FreeOnTerminate

  if Assigned(CacheGrpIconsThread) then
  begin
    CacheGrpIconsThread.OnTerminate := nil;
    CacheGrpIconsThread.Terminate;
    CacheGrpIconsThread.WaitFor;
  end;
  // CacheGrpIconsThread.Free; Auto freed with FreeOnTerminate

  if Assigned(CacheSysIconsThread) then
  begin
    CacheSysIconsThread.OnTerminate := nil;
    CacheSysIconsThread.Terminate;
    CacheSysIconsThread.WaitFor;
  end;
  // CacheSysIconsThread.Free; Auto freed with FreeOnTerminate

  if Assigned(CacheEmuIconsThread) then
  begin
    CacheEmuIconsThread.OnTerminate := nil;
    CacheEmuIconsThread.Terminate;
    CacheEmuIconsThread.WaitFor;
  end;
  // CacheEmuIconsThread.Free; Auto freed with FreeOnTerminate

  CanClose := True;
end;

procedure TfrmETKGUIMain.actEmulatorManagerExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmETKGUIEmuManager.SimpleForm(Emuteca.EmulatorManager,
    SHA1Folder, GUIConfig.DefaultFileName, GUIIconsFile);

  Emuteca.UpdateSysEmulators;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actExitWOSavingExecute(Sender: TObject);
begin
  SaveOnExit := False;
  actExit.Execute;
end;

procedure TfrmETKGUIMain.actExportSoftDataExecute(Sender: TObject);
begin
  TfmEmutecaActExportSoftData.SimpleForm(Emuteca, CurrentSystem,
    GUIConfig.DefaultFileName, GUIIconsFile);
end;

procedure TfrmETKGUIMain.actImportSoftDataExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmEmutecaActImportSoftData.SimpleForm(Emuteca, CurrentSystem,
    GUIConfig.DefaultFileName, GUIIconsFile);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actAddFolderExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmEmutecaActAddFolder.SimpleForm(Emuteca, CurrentSystem,
    GUIConfig.DefaultFileName, GUIIconsFile);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actAddSoftExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmEmutecaActAddSoft.SimpleForm(Emuteca, CurrentSystem, GUIIconsFile,
    GUIConfig.DefaultFileName);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actCleanSystemDataExecute(Sender: TObject);
var
  aPCB: TEmutecaProgressCallBack;
begin
  // TODO: Make this a script?
  if not assigned(CurrentSystem) then
    Exit;

  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;
  aPCB := CurrentSystem.ProgressCallBack;
  CurrentSystem.ProgressCallBack := @DoProgressBar;
  CurrentSystem.CleanSoftGroupLists;
  CurrentSystem.ProgressCallBack := aPCB;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actCopyFileName2ClipBoardExecute(Sender: TObject);
begin
  if not Assigned(CurrentSoft) then Exit;
  Clipboard.AsText := CurrentSoft.FileName;
end;

procedure TfrmETKGUIMain.actEditEmulatorExecute(Sender: TObject);
begin
  if not assigned(CurrentEmu) then
    Exit;

  TfmETKGUIFullEmuEditor.SimpleModalForm(CurrentEmu, SHA1Folder,
    GUIConfig.DefaultFileName, GUIIconsFile);
end;

procedure TfrmETKGUIMain.actEditGroupExecute(Sender: TObject);
begin
  if not assigned(CurrentGroup) then
    Exit;

  TfmEmutecaGroupEditor.SimpleModalForm(CurrentGroup, '',
    GUIConfig.DefaultFileName, GUIIconsFile);
end;

procedure TfrmETKGUIMain.actEditSystemExecute(Sender: TObject);
begin
  if not assigned(CurrentSystem) then
    Exit;

  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmETKGUIFullSystemEditor.SimpleModalForm(Emuteca, CurrentSystem,
    SHA1Folder, GUIConfig.DefaultFileName, GUIIconsFile);
  LoadSystemsIcons;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actMediaManagerExecute(Sender: TObject);
begin
  fmEmutecaMainFrame.Emuteca := nil;

  TfmETKGUIMediaManager.SimpleForm(Emuteca, CurrentSystem,
    GUIConfig, GUIIconsFile);
  LoadSystemsIcons; // Reloads system icons, they can be changed

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actMergeGroupFilesExecute(Sender: TObject);
begin
  TfmETKGUIactMergeGroup.SimpleForm(CurrentGroup, GUIIconsFile,
    GUIConfig.DefaultFileName);
end;

procedure TfrmETKGUIMain.actOpen7zCacheFolderExecute(Sender: TObject);
begin
  if not OpenDocument(w7zGetCacheDir) then
    raise EFileNotFoundException.CreateFmt(rsFmtNotFound,
      [GetCurrentDirUTF8, ExtractFileDir(w7zGetCacheDir)]);
end;

procedure TfrmETKGUIMain.actOpenEmulatorFolderExecute(Sender: TObject);
begin
  if not Assigned(CurrentEmu) then
    Exit;

  if not OpenDocument(ExtractFileDir(CurrentEmu.ExeFile)) then
    raise EFileNotFoundException.CreateFmt(rsFmtNotFound,
      [GetCurrentDirUTF8, ExtractFileDir(CurrentEmu.ExeFile)]);
end;

procedure TfrmETKGUIMain.actOpenEmulatorWebExecute(Sender: TObject);
begin
  if not assigned(CurrentEmu) then
    Exit;

  if CurrentEmu.WebPage = '' then
    ShowMessage(rsEmuWebEmpty)
  else
    OpenURL(CurrentEmu.WebPage);
end;

procedure TfrmETKGUIMain.actOpenEmutecaFolderExecute(Sender: TObject);
begin
  if not OpenDocument(BaseFolder) then
    raise EFileNotFoundException.CreateFmt(rsFmtNotFound,
      [GetCurrentDirUTF8, BaseFolder]);
end;

procedure TfrmETKGUIMain.actOpenSoftFolderExecute(Sender: TObject);
begin
  if not assigned(CurrentSystem) then
    Exit;

  if not assigned(CurrentSoft) then
    Exit;

  if not OpenDocument(CurrentSoft.Folder) then
    raise EFileNotFoundException.CreateFmt(rsFmtNotFound,
      [GetCurrentDirUTF8, CurrentSoft.Folder]);
end;

procedure TfrmETKGUIMain.actOpenSystemBaseFolderExecute(Sender: TObject);
begin
  if not assigned(CurrentSystem) then
    Exit;

  if CurrentSystem.BaseFolder = '' then
  begin
    ShowMessage('System base folder is not defined.');
    Exit;
  end;

  if not OpenDocument(CurrentSystem.BaseFolder) then
    raise EFileNotFoundException.CreateFmt(rsFmtNotFound,
      [GetCurrentDirUTF8, CurrentSystem.BaseFolder]);
end;

procedure TfrmETKGUIMain.actOpenTempFolderExecute(Sender: TObject);
begin
  if not OpenDocument(Emuteca.TempFolder) then
    raise EFileNotFoundException.CreateFmt(rsFmtNotFound,
      [GetCurrentDirUTF8, Emuteca.TempFolder]);
end;

procedure TfrmETKGUIMain.actRemoveSoftExecute(Sender: TObject);
var
  aSystem: cEmutecaSystem;
begin
  if not Assigned(CurrentSoft) then
    Exit;

  if (not (CurrentSoft.CachedGroup is cEmutecaGroup)) or
    (not (CurrentSoft.CachedSystem is cEmutecaSystem)) then
    // This must not happen
    Exit;

  if not QuestionDlg('Are you sure?', Format(rsAskDeleteItem,
    [CurrentSoft.Title]), mtConfirmation, [mrYes, mrNo], 0) = mrYes then
    Exit;

  aSystem := cEmutecaSystem(CurrentSoft.CachedSystem);
  aSystem.RemoveSoft(CurrentSoft);

  CurrentSoft := nil; // Unselecting current soft

  // Updating group list.
  actUpdateGroupList.Execute;
end;

procedure TfrmETKGUIMain.actRunDBEditorExecute(Sender: TObject);
var
  sError, sOutput: string;
  ExitC: integer;
begin
  if not FileExistsUTF8(GUIConfig.DBEditor) then
  begin
    ShowMessageFmt(rsFileNotFound, [GUIConfig.DBEditor]);
    Exit;
  end;

  ExecuteCMDString(ExtractFileDir(GUIConfig.DBEditor),
    GUIConfig.DBEditor, '', sError, sOutput, ExitC);
end;

procedure TfrmETKGUIMain.actRunDBEditorGroupsExecute(Sender: TObject);
var
  SysPBCB: TEmutecaProgressCallBack;
  aFileWOExt: string;
  sError, sOutput: string;
  ExitC: integer;
begin
  if not FileExistsUTF8(GUIConfig.DBEditor) then
  begin
    ShowMessageFmt(rsFileNotFound, [GUIConfig.DBEditor]);
    Exit;
  end;

  if CurrentSystem = nil then
  begin
    ShowMessage(rsSelectSystem);
    Exit;
  end;

  fmEmutecaMainFrame.Emuteca := nil;

  SysPBCB := CurrentSystem.ProgressCallBack;
  CurrentSystem.ProgressCallBack := @(fmProgressBar.UpdTextAndBar);

  aFileWOExt := Emuteca.TempFolder + CurrentSystem.ListFileName;

  CurrentSystem.ExportSoftGroupLists(aFileWOExt, False);

  ExecuteCMDArray(ExtractFileDir(GUIConfig.DBEditor), GUIConfig.DBEditor,
    [aFileWOExt + krsFileExtGroup], sError, sOutput, ExitC);

  CurrentSystem.ImportSoftGroupLists(aFileWOExt);

  DeleteFileUTF8(aFileWOExt + krsFileExtSoft);
  DeleteFileUTF8(aFileWOExt + krsFileExtGroup);

  CurrentSystem.ProgressCallBack := SysPBCB;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actRunDBEditorSoftExecute(Sender: TObject);
var
  SysPBCB: TEmutecaProgressCallBack;
  aFileWOExt: string;
  sError, sOutput: string;
  iNotCached, ExitC: LongInt;
begin
  if not FileExistsUTF8(GUIConfig.DBEditor) then
  begin
    ShowMessageFmt(rsFileNotFound, [GUIConfig.DBEditor]);
    Exit;
  end;

  if not assigned(CurrentSystem) then
  begin
    ShowMessage(rsSelectSystem);
    Exit;
  end;

  iNotCached := CurrentSystem.IsSoftSHA1Cached;

  if iNotCached > 0 then
  begin
    // TODO: Actually this is not current file.
    ShowMessageFmt(rsExportingNoSHA1,
      [CurrentSystem.SoftManager.FullList[iNotCached].Folder,
      CurrentSystem.SoftManager.FullList[iNotCached].FileName,
      iNotCached,
      CurrentSystem.SoftManager.FullList.Count]);
    Exit;
  end;

  fmEmutecaMainFrame.Emuteca := nil;

  SysPBCB := CurrentSystem.ProgressCallBack;
  CurrentSystem.ProgressCallBack := @(fmProgressBar.UpdTextAndBar);

  aFileWOExt := Emuteca.TempFolder + CurrentSystem.ListFileName;

  CurrentSystem.ExportSoftGroupLists(aFileWOExt, False);

  ExecuteCMDArray(ExtractFileDir(GUIConfig.DBEditor), GUIConfig.DBEditor,
    [aFileWOExt + krsFileExtSoft], sError, sOutput, ExitC);

  CurrentSystem.ImportSoftGroupLists(aFileWOExt);

  DeleteFileUTF8(aFileWOExt + krsFileExtSoft);
  DeleteFileUTF8(aFileWOExt + krsFileExtGroup);

  CurrentSystem.ProgressCallBack := SysPBCB;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actRunEmulatorAloneExecute(Sender: TObject);
begin
  CurrentEmu.ExecuteAlone;
end;

procedure TfrmETKGUIMain.actRunETKIconBorderIconExecute(Sender: TObject);
var
  WorkDir, OutFolder: string;
  sError, sOutput: string;
  ExitC: integer;
begin
  if not FileExistsUTF8(GUIConfig.IconBorder) then
  begin
    ShowMessageFmt(rsFileNotFound, [GUIConfig.IconBorder]);
    Exit;
  end;

  WorkDir := ExtractFileDir(GUIConfig.IconBorder);
  OutFolder := '';
  if Assigned(CurrentSystem) then
    OutFolder := SysPath(CurrentSystem.IconFolder);

  ExecuteCMDArray(WorkDir, GUIConfig.IconBorder, [OutFolder],
    sError, sOutput, ExitC);

  // TODO 3: Show a message if IconBorder or WorkDir not found.
end;

procedure TfrmETKGUIMain.actRunETKIconBorderLogoExecute(Sender: TObject);
var
  WorkDir, OutFolder: string;
  sError, sOutput: string;
  ExitC: integer;
begin
  if not FileExistsUTF8(GUIConfig.IconBorder) then
  begin
    ShowMessageFmt(rsFileNotFound, [GUIConfig.IconBorder]);
    Exit;
  end;

  WorkDir := ExtractFileDir(GUIConfig.IconBorder);
  OutFolder := '';
  if Assigned(CurrentSystem) then
    OutFolder := SysPath(CurrentSystem.LogoFolder);

  ExecuteCMDArray(WorkDir, GUIConfig.IconBorder, [OutFolder],
    sError, sOutput, ExitC);

  // TODO 3: Show a message if IconBorder or WorkDir not found.
end;

procedure TfrmETKGUIMain.actRunSoftwareExecute(Sender: TObject);
begin
  RunSoftware(CurrentSoft);
end;

procedure TfrmETKGUIMain.actSaveListsExecute(Sender: TObject);
begin
  Emuteca.SaveAllData;
end;

procedure TfrmETKGUIMain.actScriptManagerExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmETKGUIScriptManager.SimpleForm(Emuteca,
    SetAsAbsoluteFile(GUIConfig.ScriptsFolder, BaseFolder),
    GUIConfig.DefaultFileName, GUIIconsFile);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actSystemManagerExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmETKGUISysManager.SimpleForm(Emuteca, SHA1Folder,
    GUIConfig.DefaultFileName, GUIIconsFile);
  LoadSystemsIcons; // Reloads system icons

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.actUpdateGroupListExecute(Sender: TObject);
begin
  if not Assigned(CurrentSystem) then
    Exit;

  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  CurrentSystem.CacheGroups;
  CurrentSystem.CleanGroupList;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmETKGUIMain.SetCacheGrpIconsThread(AValue: ctEGUICacheGrpIcons);
begin
  if FCacheGrpIconsThread = AValue then
    Exit;
  FCacheGrpIconsThread := AValue;
end;

procedure TfrmETKGUIMain.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

procedure TfrmETKGUIMain.SetCacheEmuIconsThread(
  const AValue: ctETKGUICacheEmuIcons);
begin
  if FCacheEmuIconsThread = AValue then
    Exit;
  FCacheEmuIconsThread := AValue;
end;

procedure TfrmETKGUIMain.SetCacheSoftIconsThread(
  AValue: ctEGUICacheSoftIcons);
begin
  if FCacheSoftIconsThread = AValue then
    Exit;
  FCacheSoftIconsThread := AValue;
end;

procedure TfrmETKGUIMain.SetCacheSysIconsThread(AValue: ctEGUICacheSysIcons);
begin
  if FCacheSysIconsThread = AValue then
    Exit;
  FCacheSysIconsThread := AValue;
end;

procedure TfrmETKGUIMain.SetCurrentEmu(aCurrentEmu: cEmutecaEmulator);
begin
  if FCurrentEmu = aCurrentEmu then
    Exit;
  FCurrentEmu := aCurrentEmu;

  mimmEmulator.Enabled := Assigned(CurrentEmu);
end;

procedure TfrmETKGUIMain.SetCurrentSoft(AValue: cEmutecaSoftware);
begin
  if FCurrentSoft = AValue then
    Exit;
  FCurrentSoft := AValue;

  mimmSoft.Enabled := Assigned(CurrentSoft);

  if Assigned(CurrentSoft) then
    CurrentGroup := cEmutecaGroup(CurrentSoft.CachedGroup)
  else
    CurrentGroup := nil;
end;

procedure TfrmETKGUIMain.SetCurrentSystem(AValue: cEmutecaSystem);
begin
  if FCurrentSystem = AValue then
    Exit;
  FCurrentSystem := AValue;

  CurrentSoft := nil;
  CurrentGroup := nil;

  mimmSystem.Enabled := Assigned(CurrentSystem);
end;

procedure TfrmETKGUIMain.SetGUIIconsFile(AValue: string);
begin
  FGUIIconsFile := SetAsAbsoluteFile(AValue, BaseFolder);
end;

procedure TfrmETKGUIMain.SetSaveOnExit(AValue: boolean);
begin
  if FSaveOnExit = AValue then Exit;
  FSaveOnExit := AValue;
end;

procedure TfrmETKGUIMain.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(SetAsAbsoluteFile(AValue, BaseFolder));
  w7zSetGlobalCache(SHA1Folder);
end;

procedure TfrmETKGUIMain.Setw7zErrorFileName(AValue: string);
begin
  Fw7zErrorFileName := SetAsFolder(SetAsAbsoluteFile(AValue, BaseFolder));
  w7zSetErrorListFile(w7zErrorFileName);
end;

procedure TfrmETKGUIMain.CacheSysIconsThreadTerminated(Sender: TObject);
begin
  CacheSysIconsThread := nil;
end;

procedure TfrmETKGUIMain.CacheEmuIconsThreadTerminated(Sender: TObject);
begin
  CacheEmuIconsThread := nil;
end;

procedure TfrmETKGUIMain.CacheGrpIconsThreadTerminated(Sender: TObject);
begin
  CacheGrpIconsThread := nil;
end;

procedure TfrmETKGUIMain.CacheSoftIconsThreadTerminated(Sender: TObject);
begin
  CacheSoftIconsThread := nil;
end;

procedure TfrmETKGUIMain.LoadIcons;
var
  aFolder, aFile: string;
begin
  // Zone icons
  ZoneIcons.Clear;
  aFolder := SetAsAbsoluteFile(GUIConfig.ZoneIcnFolder, BaseFolder);
  IterateFolderObj(aFolder, @AddZoneIcon, False);

  // Adding No Zone Icon
  aFolder := SetAsAbsoluteFile(GUIConfig.DefImgFolder, BaseFolder);
  ZoneIcons.AddImageFile('', aFolder + 'NoZone.png');

  // Cached Icons (Sys, soft, group, emu)
  IconList.Clear;
 { Icons for games parents and software; first, ugly default ones
    0: Default for software
    1: Default for system
    2: Default for emulator
  }
  IconList.AddImageFile(aFolder + 'SoftIcon.png');
  IconList.AddImageFile(aFolder + 'SysIcon.png');
  IconList.AddImageFile(aFolder + 'EmuIcon.png');

  { Icons for "flags" column. }
  DumpIcons.Clear;
  aFolder := SetAsAbsoluteFile(GUIConfig.DumpIcnFolder, BaseFolder);
  for aFile in EmutecaDumpStatusStrK do
    DumpIcons.AddImageFile(aFolder + aFile + '.png');
  for aFile in LazEmuTKDumpInfoIconFiles do
    DumpIcons.AddImageFile(aFolder + aFile + '.png');

  LoadSystemsIcons;
  LoadEmuIcons;
end;

procedure TfrmETKGUIMain.LoadSystemsIcons;
begin
  // Teminate if it's running
  if assigned(CacheSysIconsThread) then
  begin
    CacheSysIconsThread.OnTerminate := nil;
    CacheSysIconsThread.Terminate;
    // CacheSysIconsThread.WaitFor; Don't wait
  end;
  // Auto freed with FreeOnTerminate and set to nil

  if not (Assigned(Emuteca) and Assigned(IconList)) then
    Exit;

  // Creating background thread for loading system icons.
  FCacheSysIconsThread := ctEGUICacheSysIcons.Create;
  if Assigned(CacheSysIconsThread.FatalException) then
    raise CacheSysIconsThread.FatalException;
  CacheSysIconsThread.OnTerminate := @CacheSysIconsThreadTerminated; //Autonil

  CacheSysIconsThread.SystemManager := Emuteca.SystemManager;
  CacheSysIconsThread.IconList := IconList;

  if IconList.Count > 1 then
    // 1: Default for system
    CacheSysIconsThread.DefSysIcon := IconList[1]
  else if IconList.Count > 0 then
    CacheSysIconsThread.DefSysIcon := IconList[IconList.Count - 1];

  if IconList.Count > 0 then
    // 0: Default for soft
    CacheSysIconsThread.DefSoftIcon := IconList[0];

  CacheSysIconsThread.Start;
end;

procedure TfrmETKGUIMain.LoadEmuIcons;
begin
  // Teminate if it's running
  if assigned(CacheEmuIconsThread) then
  begin
    CacheEmuIconsThread.OnTerminate := nil;
    CacheEmuIconsThread.Terminate;
    // CacheEmuIconsThread.WaitFor; Don't wait
  end;
  // Auto freed with FreeOnTerminate and set to nil

  if not (Assigned(Emuteca) and Assigned(IconList)) then
    Exit;

  // Creating background thread for loading emu icons.
  FCacheEmuIconsThread := ctETKGUICacheEmuIcons.Create;
  if Assigned(CacheEmuIconsThread.FatalException) then
    raise CacheEmuIconsThread.FatalException;
  CacheEmuIconsThread.OnTerminate := @CacheEmuIconsThreadTerminated; //Autonil

  CacheEmuIconsThread.EmuManager := Emuteca.EmulatorManager;
  CacheEmuIconsThread.IconList := IconList;

  if IconList.Count > 2 then
    // 2: Default for Emulatots
    CacheEmuIconsThread.DefEmuIcon := IconList[2];

  CacheEmuIconsThread.Start;
end;

function TfrmETKGUIMain.AddZoneIcon(aFolder: string;
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

procedure TfrmETKGUIMain.LoadGrpIcons(aGroupList: cEmutecaGroupList);
begin
  // Teminate if it's running
  if assigned(CacheGrpIconsThread) then
  begin
    CacheGrpIconsThread.Terminate;
    CacheGrpIconsThread.WaitFor;
  end;
  // Auto freed with FreeOnTerminate and nil

  if not (Assigned(aGroupList) and Assigned(IconList) and
    Assigned(GUIConfig)) then
    Exit;

  // Creating background thread for loading group icons.
  FCacheGrpIconsThread := ctEGUICacheGrpIcons.Create;
  if Assigned(CacheGrpIconsThread.FatalException) then
    raise CacheGrpIconsThread.FatalException;
  CacheGrpIconsThread.OnTerminate := @CacheGrpIconsThreadTerminated; //Autonil

  CacheGrpIconsThread.GroupList := aGroupList;
  CacheGrpIconsThread.IconList := IconList;

  CacheGrpIconsThread.ImageExt := GUIConfig.ImageExtensions;
  if Assigned(Emuteca) then
    CacheGrpIconsThread.TempFolder := Emuteca.TempFolder;

  CacheGrpIconsThread.Start;
end;

procedure TfrmETKGUIMain.LoadSoftIcons(aSoftList: cEmutecaSoftList);
begin
  // Teminate if it's running
  if assigned(CacheSoftIconsThread) then
  begin
    CacheSoftIconsThread.OnTerminate := nil;
    CacheSoftIconsThread.Terminate;
    // CacheSoftIconsThread.WaitFor; Don't wait
  end;
  // Auto freed with FreeOnTerminate

  if not (Assigned(aSoftList) and Assigned(IconList) and
    Assigned(GUIConfig)) then
    Exit;

  FCacheSoftIconsThread := ctEGUICacheSoftIcons.Create;
  if Assigned(CacheSoftIconsThread.FatalException) then
    raise CacheSoftIconsThread.FatalException;
  CacheSoftIconsThread.OnTerminate := @CacheSoftIconsThreadTerminated;
  //< Autonil when terminated

  CacheSoftIconsThread.SoftList := aSoftList;
  CacheSoftIconsThread.IconList := IconList;
  CacheSoftIconsThread.ImageExt := GUIConfig.ImageExtensions;
  if Assigned(Emuteca) then
    CacheSoftIconsThread.TempFolder := Emuteca.TempFolder;

  CacheSoftIconsThread.Start;
end;

procedure TfrmETKGUIMain.LoadSearchLinks;
var
  aFile: TStringList;
  aSearcher: TStringList;
  i: integer;
  aPos: integer;
  aAction: TAction;
  aMenu: TMenuItem;
  aFilename: string;
begin
  aFilename := SetAsAbsoluteFile(GUIConfig.SearchFile, BaseFolder);

  // TODO: remove actions and menúes

  if not FileExistsUTF8(aFilename) then
    Exit;

  aFile := TStringList.Create;
  aSearcher := TStringList.Create;

  try
    aFile.LoadFromFile(aFilename);
    i := 0;
    while i < aFile.Count do
    begin
      aPos := UTF8Pos('##', aFile[i]); // Used for comments
      if aPos <> 0 then
        aFile[i] := UTF8Copy(aFile[i], 1, aPos - 1);
      aSearcher.Clear;
      aSearcher.CommaText := aFile[i];

      if aSearcher.Count >= 3 then
      begin
        aSearcher[0] := UTF8UpperCase(aSearcher[0]);

        while aSearcher.Count < 4 do
          aSearcher.add('');

        // TODO 4: Repeated code...

        // Web searcher for Game
        aPos := UTF8Pos('G', aSearcher[0]);
        if aPos <> 0 then
        begin
          aAction := TAction.Create(nil); //< nil?
          if aSearcher[3] <> '' then
            aAction.Name := 'actSearcherG' + Trim(aSearcher[3])
          else
            aAction.Name := 'actSearcherG' + IntToStr(i);
          aAction.Caption := aSearcher[1];
          // Hint is used to show URL... and store it
          aAction.Hint := aSearcher[2];
          aAction.Tag := 1; //< Tag = 1 -> Search for Game
          aAction.Category := 'Game search';
          aAction.OnExecute := @SearchInInternet;
          aAction.ActionList := ActionList;

          aMenu := TMenuItem.Create(nil);
          aMenu.Name := 'mipmGSearcherG' + IntToStr(i);
          aMenu.Action := aAction;
          mipmGSearchInternet.Add(aMenu);
        end;

        // Web searcher for System
        aPos := UTF8Pos('S', aSearcher[0]);
        if aPos <> 0 then
        begin
          aAction := TAction.Create(nil); //< nil?
          if aSearcher[3] <> '' then
            aAction.Name := 'actSearcherS' + Trim(aSearcher[3])
          else
            aAction.Name := 'actSearcherS' + IntToStr(i);
          aAction.Caption := aSearcher[1];
          // Hint is used to show URL... and store it
          aAction.Hint := aSearcher[2];
          aAction.Tag := 2; //< Tag = 2 -> Search for System
          aAction.Category := 'System search';
          aAction.OnExecute := @SearchInInternet;
          aAction.ActionList := ActionList;

          aMenu := TMenuItem.Create(nil);
          aMenu.Name := 'mimmSearcherS' + IntToStr(i);
          aMenu.Action := aAction;
          mimmSearchInternetS.Add(aMenu);
        end;

        // Web searcher for Emulator
        aPos := UTF8Pos('E', aSearcher[0]);
        if aPos <> 0 then
        begin
          aAction := TAction.Create(nil); //< nil?
          if aSearcher[3] <> '' then
            aAction.Name := 'actSearcherE' + Trim(aSearcher[3])
          else
            aAction.Name := 'actSearcherE' + IntToStr(i);
          aAction.Caption := aSearcher[1];
          // Hint is used to show URL... and store it
          aAction.Hint := aSearcher[2];
          aAction.Tag := 3; //< Tag = 3 -> Search for Emulator
          aAction.Category := 'Emulator search';
          aAction.OnExecute := @SearchInInternet;
          aAction.ActionList := ActionList;

          // TODO 2: Add icon to the list and assign it to the action

          aMenu := TMenuItem.Create(nil);
          aMenu.Name := 'mimmSearcherE' + IntToStr(i);
          aMenu.Action := aAction;
          mimmSearchInternetE.Add(aMenu);
        end;
      end;

      Inc(i);
    end;

  finally
    aFile.Free;
    aSearcher.Free;
  end;
end;

procedure TfrmETKGUIMain.SearchInInternet(Sender: TObject);
var
  aAction: TCustomAction;
  TempStr: string;
begin
  if not (Sender is TCustomAction) then
    Exit;
  aAction := TCustomAction(Sender);

  case aAction.Tag of
    1:
    begin
      if not Assigned(CurrentGroup) then
        Exit;
      // Better to search SortTitle than Title.
      if Assigned(CurrentSoft) then
        TempStr := CurrentSoft.SortTitle
      else
        TempStr := CurrentGroup.SortTitle;
    end;
    2:
    begin
      if not Assigned(CurrentSystem) then
        Exit;
      TempStr := CurrentSystem.Title;
    end;
    3:
    begin
      if not Assigned(CurrentEmu) then
        Exit;
      TempStr := CurrentEmu.Title;
    end;
    else
    begin
      Exit;
    end;
  end;
  if TempStr = '' then
    Exit;

  TempStr := Format(aAction.Hint, [TempStr]);
  OpenURL(TempStr);
end;

procedure TfrmETKGUIMain.RunSoftware(aSoftware: cEmutecaSoftware);
var
  aError: integer;
begin
  aError := Emuteca.RunSoftware(aSoftware);

  case aError of
    0: ; // All OK
    kErrorRunSoftUnknown:
    begin
      ShowMessageFmt(rsRunSoftwareUnkError, [aSoftware.Folder,
        aSoftware.FileName]);
    end;
    kErrorRunSoftNoSoft:
    begin
      ShowMessage(rsRunSoftwareNoSoft);
    end;
    kErrorRunSoftNoEmu:
    begin
      ShowMessageFmt(rsRunSoftwareNoEmu, [aSoftware.Folder,
        aSoftware.FileName]);
    end;
    kErrorRunSoftNoSoftFile:
    begin
      ShowMessageFmt(rsRunSoftwareNoSoftFile,
        [aSoftware.Folder, aSoftware.FileName]);
    end;
    kErrorRunSoftNoEmuFile:
    begin
      ShowMessage(rsRunSoftwareNoEmuExe);
    end;
    kError7zDecompress, kError7zDecompress - 1, kError7zDecompress - 2:
    begin
      ShowMessage(rsRunSoftware7zError);
    end;
    else
    begin
      ShowMessageFmt(rsRunSoftwareExitCode, [aError]);
    end;
  end;
end;

function TfrmETKGUIMain.DoProgressBar(const Title, Info: string;
  const Value, MaxValue: int64; const IsCancelable: boolean): boolean;
begin
  // We asume that fmCHXProgressBar is always created...
  Result := fmProgressBar.UpdTextAndBar(Title, Info, Value,
    MaxValue, IsCancelable);
end;

procedure TfrmETKGUIMain.DoChangeGrpList(aGroupList: cEmutecaGroupList);
begin
  CurrentSoft := nil;
  CurrentGroup := nil;

  LoadGrpIcons(aGroupList);
end;

procedure TfrmETKGUIMain.DoLoadGUIIcons(aIniFile: TIniFile;
  const aBaseFolder: string);
begin
  inherited DoLoadGUIIcons(aIniFile, aBaseFolder);
  // Icons for TActions
  ReadActionsIconsIni(aIniFile, aBaseFolder, Name, ActImages, ActionList);

  // Icons for menus (without assigned TAction)
  ReadMenuIconsIni(aIniFile, aBaseFolder, Name, ActImages, MainMenu);
  ReadMenuIconsIni(aIniFile, aBaseFolder, Name, ActImages, pmGroup);
  ReadMenuIconsIni(aIniFile, aBaseFolder, Name, ActImages, pmSoft);
end;

constructor TfrmETKGUIMain.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfrmETKGUIMain.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfrmETKGUIMain);

finalization
  UnRegisterClass(TfrmETKGUIMain);
end.
{
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
