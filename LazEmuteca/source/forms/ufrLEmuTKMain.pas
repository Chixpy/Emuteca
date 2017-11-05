{ Main form of LazEmuteca.

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
unit ufrLEmuTKMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls,
  Graphics, Dialogs, LazUTF8, LCLIntf, IniFiles,
  ActnList, Menus, StdActns, ComCtrls, ExtCtrls, LCLTranslator,
  IniPropStorage, StdCtrls,
  // Misc
  uVersionSupport, u7zWrapper,
  // CHX units
  uCHXStrUtils, uCHXFileUtils, uCHXImageUtils, ucCHXImageList,
  // CHX forms
  ufrCHXForm, ufCHXProgressBar,
  // Emuteca clases
  ucEmuteca, ucEmutecaGroupList, uEmutecaCommon,
  // Emuteca forms
  ufrLEmuTKAbout,
  // Emuteca windows
  ufEmutecaActAddSoft, ufEmutecaActAddFolder, ufEmutecaActExportSoftData,
  ufEmutecaActImportSoftData,
  // LazEmuteca units
  uLEmuTKCommon, uGUIConfig,
  // LazEmuteca frames
  ufLEmuTKMain, ufLEmuTKSysManager, ufLEmuTKEmuManager,
  ufLEmuTKMediaManager, ufLEmuTKScriptManager,
  // LazEmuteca threads
  utLEmuTKCacheSysIcons, utLEmuTKCacheGrpIcons;

type

  { TfrmLEmuTKMain }
  // TODO: Make it TCHXForm
  TfrmLEmuTKMain = class(TForm)
    actEmulatorManager: TAction;
    actAddFolder: TAction;
    actAddSoft: TAction;
    actAutoSave: TAction;
    actExportSoftData: TAction;
    actImportSoftData: TAction;
    actCleanAllSystems: TAction;
    actCleanSystemData: TAction;
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
    mimmCleanSystem: TMenuItem;
    MenuItem3: TMenuItem;
    mimmCleanAllSystems: TMenuItem;
    mimmSystem: TMenuItem;
    mimmImportSoftData: TMenuItem;
    mimmExportSoftData: TMenuItem;
    MenuItem2: TMenuItem;
    mmiAbout: TMenuItem;
    mimmAddFiles: TMenuItem;
    mimmScanFolder: TMenuItem;
    mimmAddSoft: TMenuItem;
    MenuItem13: TMenuItem;
    mimmSaveLists: TMenuItem;
    MenuItem15: TMenuItem;
    mimmSaveOnExit: TMenuItem;
    mimmOpenTempFolder: TMenuItem;
    mimmExit: TMenuItem;
    mimmManagers: TMenuItem;
    mimmEmulatorManager: TMenuItem;
    mimmSystemManager: TMenuItem;
    mimmScriptManager: TMenuItem;
    mimmMediaManager: TMenuItem;
    mimmTest: TMenuItem;
    mimmSoft: TMenuItem;
    mmiHelp: TMenuItem;
    mimmFile: TMenuItem;
    stbHelp: TStatusBar;
    procedure actAddSoftExecute(Sender: TObject);
    procedure actAddFolderExecute(Sender: TObject);
    procedure actAutoSaveExecute(Sender: TObject);
    procedure actCleanAllSystemsExecute(Sender: TObject);
    procedure actCleanSystemDataExecute(Sender: TObject);
    procedure actEmulatorManagerExecute(Sender: TObject);
    procedure actExportSoftDataExecute(Sender: TObject);
    procedure actImportSoftDataExecute(Sender: TObject);
    procedure actMediaManagerExecute(Sender: TObject);
    procedure actOpenTempFolderExecute(Sender: TObject);
    procedure actSaveListsExecute(Sender: TObject);
    procedure actScriptManagerExecute(Sender: TObject);
    procedure actSystemManagerExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpOnHelp1Execute(Sender: TObject);
    procedure mimmTestClick(Sender: TObject);

  private
    FCacheGrpIconsThread: ctLEmuTKCacheGrpIcons;
    FCacheSysIconsThread: ctLEmuTKCacheSysIcons;
    FfmEmutecaMainFrame: TfmLEmuTKMain;
    FGUIIconsFile: string;
    FSHA1Folder: string;
    FDumpIcons: cCHXImageList;
    FEmuteca: cEmuteca;
    FGUIConfig: cGUIConfig;
    FIconList: cCHXImageList;
    FZoneIcons: cCHXImageMap;
    procedure SetCacheGrpIconsThread(AValue: ctLEmuTKCacheGrpIcons);
    procedure SetCacheSysIconsThread(AValue: ctLEmuTKCacheSysIcons);
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
    property DumpIcons: cCHXImageList read FDumpIcons;
    // Icons for dump info
    property ZoneIcons: cCHXImageMap read FZoneIcons;
    // Icons of zones

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    property CacheSysIconsThread: ctLEmuTKCacheSysIcons
      read FCacheSysIconsThread write SetCacheSysIconsThread;
    procedure CacheSysIconsThreadTerminated(Sender: TObject); // For TThread.OnTerminate

    property CacheGrpIconsThread: ctLEmuTKCacheGrpIcons read FCacheGrpIconsThread write SetCacheGrpIconsThread;
    procedure CacheGrpIconsThreadTerminated(Sender: TObject); // For TThread.OnTerminate

    procedure LoadIcons;
    procedure LoadSystemsIcons;
    procedure LoadGrpIcons(aGroupList: cEmutecaGroupList);

    procedure LoadEmuteca;
    //< Load Emuteca, remove not saved data.
    procedure SaveEmuteca;
    //< Save Emuteca.

    function AddZoneIcon(aFolder: string; FileInfo: TSearchRec): boolean;
    //< Add Zone icon to list

    function DoProgressBar(const Title, Info1, Info2: string;
      const Value, MaxValue: int64): boolean;
    //< Progress bar call back

    procedure DoChangeGrpList(aGroupList: cEmutecaGroupList);

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
      Format(krsFmtWindowCaption, [Application.Title, HelpOnHelp1.Caption]);
    frmLEmuTKAbout.Emuteca := Emuteca;
    frmLEmuTKAbout.CachedIcons := IconList;
    frmLEmuTKAbout.ZoneIcons := ZoneIcons;
    frmLEmuTKAbout.VersionIcons := DumpIcons;
    frmLEmuTKAbout.UpdateInfo;
    frmLEmuTKAbout.ShowModal;
  finally
    FreeAndNil(frmLEmuTKAbout);
  end;
end;

procedure TfrmLEmuTKMain.mimmTestClick(Sender: TObject);
begin
  ShowMessage('Temp button for fast testing.');
end;

procedure TfrmLEmuTKMain.SetGUIIconsFile(AValue: string);
begin
  if FGUIIconsFile = AValue then
    Exit;
  FGUIIconsFile := AValue;
end;

procedure TfrmLEmuTKMain.SetCacheSysIconsThread(AValue: ctLEmuTKCacheSysIcons);
begin
  if FCacheSysIconsThread = AValue then
    Exit;
  FCacheSysIconsThread := AValue;
end;

procedure TfrmLEmuTKMain.SetCacheGrpIconsThread(AValue: ctLEmuTKCacheGrpIcons);
begin
  if FCacheGrpIconsThread = AValue then Exit;
  FCacheGrpIconsThread := AValue;
end;

procedure TfrmLEmuTKMain.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(SetAsAbsoluteFile(AValue, ProgramDirectory));
  w7zSetGlobalCache(SHA1Folder);
end;

procedure TfrmLEmuTKMain.CacheSysIconsThreadTerminated(Sender: TObject);
begin
  CacheSysIconsThread := nil;
end;

procedure TfrmLEmuTKMain.CacheGrpIconsThreadTerminated(Sender: TObject);
begin
  CacheGrpIconsThread := nil;
end;

procedure TfrmLEmuTKMain.LoadIcons;
var
  aFolder, aFile: string;
begin
  ActImages.Clear;

  // Icons for TActions
  ReadActionsIconsFile(GUIIconsFile, Name, ActImages, ActionList);

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
  IconList.AddImageFile(aFolder + 'SoftIcon.png');
  IconList.AddImageFile(aFolder + 'GroupIcon.png');
  IconList.AddImageFile(aFolder + 'SysIcon.png');
  IconList.AddImageFile(aFolder + 'EmuIcon.png');

  { Icons for "flags" column, see ufEmutecaIcnSoftList.LazEmuTKIconFiles
    case ord(LazEmuTKIconFiles)
    }
  DumpIcons.Clear;
  aFolder := SetAsAbsoluteFile(GUIConfig.DumpIcnFolder, ProgramDirectory);
  for aFile in LazEmuTKIconFiles do
    DumpIcons.AddImageFile(aFolder + aFile + '.png');

  LoadSystemsIcons;
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
  Emuteca.SaveData;
end;

function TfrmLEmuTKMain.DoProgressBar(const Title, Info1, Info2: string;
  const Value, MaxValue: int64): boolean;
begin
  // We asume that frmCHXProgressBar is always created...
  Result := frmCHXProgressBar.UpdTextAndBar(Title, Info1, Info2,
    Value, MaxValue);
end;

procedure TfrmLEmuTKMain.DoChangeGrpList(aGroupList: cEmutecaGroupList);
begin
   LoadGrpIcons(aGroupList);
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

procedure TfrmLEmuTKMain.LoadSystemsIcons;
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

  // Creating background thread for
  FCacheSysIconsThread := ctLEmuTKCacheSysIcons.Create;
  if Assigned(CacheSysIconsThread.FatalException) then
    raise CacheSysIconsThread.FatalException;
  CacheSysIconsThread.OnTerminate := @CacheSysIconsThreadTerminated; //Autonil

  CacheSysIconsThread.SystemManager := Emuteca.SystemManager;
  CacheSysIconsThread.IconList := IconList;

  if IconList.Count > 2 then
    // 2: Default for system
    CacheSysIconsThread.DefaultIcon := IconList[2]
  else if IconList.Count > 0 then
    CacheSysIconsThread.DefaultIcon := IconList[IconList.Count - 1];

  CacheSysIconsThread.Start;
end;

procedure TfrmLEmuTKMain.LoadGrpIcons(aGroupList: cEmutecaGroupList);
begin
  // Teminate if it's running
  if assigned(CacheGrpIconsThread) then
  begin
    CacheGrpIconsThread.OnTerminate:= nil;
    CacheGrpIconsThread.Terminate;
    // CacheGrpIconsThread.WaitFor; Don't wait
  end;
  // Auto freed with FreeOnTerminate and nil

  if not (Assigned(aGroupList) and Assigned(IconList) and Assigned(GUIConfig)) then
    Exit;

  FCacheGrpIconsThread := ctLEmuTKCacheGrpIcons.Create;
  if Assigned(CacheGrpIconsThread.FatalException) then
    raise CacheGrpIconsThread.FatalException;
  CacheGrpIconsThread.OnTerminate := @CacheGrpIconsThreadTerminated; //Autonil

  CacheGrpIconsThread.GroupList :=aGroupList;
  CacheGrpIconsThread.IconList := IconList;
  CacheGrpIconsThread.ImageExt := GUIConfig.ImageExtensions;

  if IconList.Count > 1 then
    // 1: Default for groups
    CacheGrpIconsThread.DefaultIcon := IconList[1]
  else if IconList.Count > 0 then
    CacheGrpIconsThread.DefaultIcon := IconList[IconList.Count - 1];

  CacheGrpIconsThread.Start;
end;

procedure TfrmLEmuTKMain.FormCreate(Sender: TObject);
var
  aIni: TMemIniFile;
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
  Caption := Format(krsFmtWindowCaption, [Application.Title, Caption]);

  FGUIConfig := cGUIConfig.Create(self);
  GUIConfig.LoadConfig(SetAsAbsoluteFile('GUI.ini', ProgramDirectory));
  IniPropStorage.IniFileName := GUIConfig.ConfigFile;
  IniPropStorage.Restore;

  GUIIconsFile := SetAsAbsoluteFile(GUIConfig.GUIIcnFile, ProgramDirectory);

  // Experimental
  SHA1Folder := SetAsAbsoluteFile(GUIConfig.GlobalCache, ProgramDirectory);

  // Image lists
  FIconList := cCHXImageList.Create(True);
  FDumpIcons := cCHXImageList.Create(True);
  FZoneIcons := cCHXImageMap.Create(True);

  // Creating Emuteca Core :-D
  FEmuteca := cEmuteca.Create(self);
  Emuteca.BaseFolder := ProgramDirectory;
  Emuteca.ProgressCallBack := @DoProgressBar;
  Emuteca.LoadConfig(GUIConfig.EmutecaIni);

  // This must after creating and loading Emuteca
  //   runs CacheSysIconsThread too
  LoadIcons;

  // Creating main frame
  FfmEmutecaMainFrame := TfmLEmuTKMain.Create(Self);
  fmEmutecaMainFrame.OnGrpListChanged := @DoChangeGrpList;
  fmEmutecaMainFrame.IconList := IconList;
  fmEmutecaMainFrame.DumpIcons := DumpIcons;
  fmEmutecaMainFrame.ZoneIcons := ZoneIcons;
  fmEmutecaMainFrame.Emuteca := Emuteca;
  fmEmutecaMainFrame.SHA1Folder := SHA1Folder;
  fmEmutecaMainFrame.GUIConfig := GUIConfig;

  aIni := TMemIniFile.Create(GUIConfig.GUIIcnFile);
  try
    fmEmutecaMainFrame.LoadGUIIcons(aIni, ExtractFilePath(aIni.FileName));
  finally
    aIni.Free;
  end;

  fmEmutecaMainFrame.Align := alClient;

  aIni := TMemIniFile.Create(GUIConfig.ConfigFile);
  try
    fmEmutecaMainFrame.LoadGUIConfig(aIni);
  finally
    aIni.Free;
  end;
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
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, actEmulatorManager.Caption]);

    aFrame := TfmLEmuTKEmuManager.Create(aForm);
    aFrame.EmuManager := Emuteca.EmulatorManager;
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;


    aForm.LoadGUIConfig(GUIConfig.ConfigFile);
    aForm.LoadGUIIcons(GUIConfig.GUIIcnFile);
    aFrame.Parent := aForm;

    aForm.ShowModal;
  finally
    FreeAndNil(aForm);
  end;
end;

procedure TfrmLEmuTKMain.actExportSoftDataExecute(Sender: TObject);
begin
  TfmActExportSoftData.SimpleForm(Emuteca, GUIIconsFile, GUIConfig.ConfigFile);
end;

procedure TfrmLEmuTKMain.actImportSoftDataExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmEmutecaActImportSoftData.SimpleForm(Emuteca, GUIIconsFile,
    GUIConfig.ConfigFile);

  fmEmutecaMainFrame.Emuteca := Emuteca;
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
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmLEmuTKScriptManager.SimpleForm(Emuteca,
    SetAsAbsoluteFile(GUIConfig.ScriptsFolder, ProgramDirectory),
    GUIIconsFile, GUIConfig.ConfigFile);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actAddFolderExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmEmutecaActAddFolder.SimpleForm(Emuteca, GUIIconsFile,
    GUIConfig.ConfigFile);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actAutoSaveExecute(Sender: TObject);
begin
  GUIConfig.SaveOnExit := actAutoSave.Checked;
end;

procedure TfrmLEmuTKMain.actCleanAllSystemsExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  Emuteca.CleanSystems;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actCleanSystemDataExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;


  // TODO: We need a callback setting current system/group/soft
  // Emuteca.CleanSystems;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actAddSoftExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmEmutecaActAddSoft.SimpleForm(Emuteca, GUIIconsFile,
    GUIConfig.ConfigFile);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actSystemManagerExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmLEmuTKSysManager.SimpleForm(Emuteca, SHA1Folder, GUIIconsFile,
    GUIConfig.ConfigFile);
  LoadSystemsIcons; // Reloads system icons

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  aIni: TMemIniFile;
begin
  // Teminate threads if they are running.
  if Assigned(CacheSysIconsThread) then
  begin
    CacheSysIconsThread.OnTerminate := nil;
    CacheSysIconsThread.Terminate;
    CacheSysIconsThread.WaitFor;
  end;
  // CacheSysIconsThread.Free; Auto freed with FreeOnTerminate

  if Assigned(CacheGrpIconsThread) then
  begin
    CacheGrpIconsThread.OnTerminate := nil;
    CacheGrpIconsThread.Terminate;
    CacheGrpIconsThread.WaitFor;
  end;
  // CacheSysIconsThread.Free; Auto freed with FreeOnTerminate

  aIni := TMemIniFile.Create(GUIConfig.ConfigFile);
  try
    fmEmutecaMainFrame.SaveGUIConfig(aIni);
  finally
    aIni.Free;
  end;

  GUIConfig.SaveConfig('');
  if GUIConfig.SaveOnExit then
    SaveEmuteca;

  CanClose := True;
end;

procedure TfrmLEmuTKMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FZoneIcons);
  FreeAndNil(FDumpIcons);
  FreeAndNil(FIconList);
  FreeAndNil(FGUIConfig);
  FreeAndNil(FEmuteca);
end;

end.
