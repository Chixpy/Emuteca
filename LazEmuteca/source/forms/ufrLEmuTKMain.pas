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
  ActnList, Menus, StdActns, ComCtrls, ExtCtrls, DefaultTranslator,
  IniPropStorage, StdCtrls,
  // Misc
  uVersionSupport,
  // CHX units
  uCHX7zWrapper, uCHXStrUtils, uCHXFileUtils, uCHXImageUtils, ucCHXImageList,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXProgressBar,
  // Emuteca units
  uEmutecaCommon,
  // Emuteca clases
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftware, ucEmutecaSoftList,
  // Emuteca forms
  ufrLEmuTKAbout,
  // Emuteca frames
  ufEmutecaActAddSoft, ufEmutecaActAddFolder, ufEmutecaActExportSoftData,
  ufEmutecaActImportSoftData,
  // LazEmuteca units
  uLEmuTKCommon, uGUIConfig,
  // LazEmuteca frames
  ufLEmuTKMain,
  ufLEmuTKSysManager, ufLEmuTKEmuManager, ufLEmuTKMediaManager,
  ufLEmuTKScriptManager,
  ufLEmuTKFullSystemEditor,
  ufLEmuTKactMergeGroup,
  // LazEmuteca threads
  utLEmuTKCacheSysIcons, utLEmuTKCacheGrpIcons, utLEmuTKCacheSoftIcons;

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
    actCleanSystemData: TAction;
    actEditSystem: TAction;
    actEditEmulator: TAction;
    actOpen7zCacheFolder: TAction;
    actOpenEmulatorFolder: TAction;
    actOpenEmutecaFolder: TAction;
    actOpenSystemBaseFolder: TAction;
    actOpenSoftFolder: TAction;
    actRunSoftware: TAction;
    actMergeGroupFiles: TAction;
    actUpdateGroupList: TAction;
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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    mimmOpen7zCacheFolder: TMenuItem;
    mimmOpenEmulatorFolder: TMenuItem;
    mimmOpenEmutecaFolder: TMenuItem;
    mimmExport: TMenuItem;
    mimmImport: TMenuItem;
    mimmImportExport: TMenuItem;
    mimmAddSoft: TMenuItem;
    mimmAddSoftFolder: TMenuItem;
    mimmOpenTempFolder: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    mimmDebug: TMenuItem;
    mimmAddFiles: TMenuItem;
    mipmSSOpenSysBaseFolder: TMenuItem;
    mipmSSystem: TMenuItem;
    mimmOpenSystemBaseFolder: TMenuItem;
    mimmEditEmulator: TMenuItem;
    mimmEmulator: TMenuItem;
    mimmEditSystem: TMenuItem;
    mipmSOpenSoftFolder: TMenuItem;
    mipmSRunSoft: TMenuItem;
    mummOpenSoftFolder: TMenuItem;
    mimmRunSoftware: TMenuItem;
    mimmMergeGroupFiles: TMenuItem;
    mimmGroup: TMenuItem;
    mimmUpdateSystemGroups: TMenuItem;
    mimmCleanSystem: TMenuItem;
    MenuItem3: TMenuItem;
    mimmSystem: TMenuItem;
    mimmAbout: TMenuItem;
    MenuItem13: TMenuItem;
    mimmSaveLists: TMenuItem;
    MenuItem15: TMenuItem;
    mimmSaveOnExit: TMenuItem;
    mimmExit: TMenuItem;
    mimmManagers: TMenuItem;
    mimmEmulatorManager: TMenuItem;
    mimmSystemManager: TMenuItem;
    mimmScriptManager: TMenuItem;
    mimmMediaManager: TMenuItem;
    mimmSoft: TMenuItem;
    mimmHelp: TMenuItem;
    mimmFile: TMenuItem;
    pmGroup: TPopupMenu;
    pmSoft: TPopupMenu;
    stbHelp: TStatusBar;
    procedure actAddSoftExecute(Sender: TObject);
    procedure actAddFolderExecute(Sender: TObject);
    procedure actAutoSaveExecute(Sender: TObject);
    procedure actCleanSystemDataExecute(Sender: TObject);
    procedure actEditEmulatorExecute(Sender: TObject);
    procedure actEditSystemExecute(Sender: TObject);
    procedure actEmulatorManagerExecute(Sender: TObject);
    procedure actExportSoftDataExecute(Sender: TObject);
    procedure actImportSoftDataExecute(Sender: TObject);
    procedure actMediaManagerExecute(Sender: TObject);
    procedure actMergeGroupFilesExecute(Sender: TObject);
    procedure actOpen7zCacheFolderExecute(Sender: TObject);
    procedure actOpenEmutecaFolderExecute(Sender: TObject);
    procedure actOpenSoftFolderExecute(Sender: TObject);
    procedure actOpenSystemBaseFolderExecute(Sender: TObject);
    procedure actOpenTempFolderExecute(Sender: TObject);
    procedure actRunSoftwareExecute(Sender: TObject);
    procedure actSaveListsExecute(Sender: TObject);
    procedure actScriptManagerExecute(Sender: TObject);
    procedure actSystemManagerExecute(Sender: TObject);
    procedure actUpdateGroupListExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpOnHelp1Execute(Sender: TObject);

  private
    FCacheGrpIconsThread: ctLEmuTKCacheGrpIcons;
    FCacheSoftIconsThread: ctLEmuTKCacheSoftIcons;
    FCacheSysIconsThread: ctLEmuTKCacheSysIcons;
    FCurrentGroup: cEmutecaGroup;
    FCurrentSoft: cEmutecaSoftware;
    FCurrentSystem: cEmutecaSystem;
    FfmEmutecaMainFrame: TfmLEmuTKMain;
    FfmProgressBar: TfmCHXProgressBar;
    FGUIIconsFile: string;
    FSHA1Folder: string;
    FDumpIcons: cCHXImageList;
    FEmuteca: cEmuteca;
    FGUIConfig: cGUIConfig;
    FIconList: cCHXImageList;
    Fw7zErrorFileName: string;
    FZoneIcons: cCHXImageMap;
    procedure SetCacheGrpIconsThread(AValue: ctLEmuTKCacheGrpIcons);
    procedure SetCacheSoftIconsThread(AValue: ctLEmuTKCacheSoftIcons);
    procedure SetCacheSysIconsThread(AValue: ctLEmuTKCacheSysIcons);
    procedure SetCurrentGroup(AValue: cEmutecaGroup);
    procedure SetCurrentSoft(AValue: cEmutecaSoftware);
    procedure SetCurrentSystem(AValue: cEmutecaSystem);
    procedure SetGUIIconsFile(AValue: string);
    procedure SetSHA1Folder(AValue: string);
    procedure Setw7zErrorFileName(AValue: string);

  protected
    property fmEmutecaMainFrame: TfmLEmuTKMain read FfmEmutecaMainFrame;
    //< Main Frame
    property fmProgressBar: TfmCHXProgressBar read FfmProgressBar;
    //< ProgressBar

    property Emuteca: cEmuteca read FEmuteca;
    //< Main Emuteca Core
    property GUIConfig: cGUIConfig read FGUIConfig;
    //< GUI config
    property GUIIconsFile: string read FGUIIconsFile write SetGUIIconsFile;

    property IconList: cCHXImageList read FIconList;
    //< Icons for parents, soft, systems and emulators
    property DumpIcons: cCHXImageList read FDumpIcons;
    //< Icons for dump info
    property ZoneIcons: cCHXImageMap read FZoneIcons;
    //< Icons of zones

    property CurrentSystem: cEmutecaSystem
      read FCurrentSystem write SetCurrentSystem;
    property CurrentGroup: cEmutecaGroup read FCurrentGroup
      write SetCurrentGroup;
    property CurrentSoft: cEmutecaSoftware
      read FCurrentSoft write SetCurrentSoft;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;
    //< Global Cache folder
    property w7zErrorFileName: string read Fw7zErrorFileName
      write Setw7zErrorFileName;
    //< File for w7z errors and warnings

    property CacheSysIconsThread: ctLEmuTKCacheSysIcons
      read FCacheSysIconsThread write SetCacheSysIconsThread;
    procedure CacheSysIconsThreadTerminated(Sender: TObject);
    // For TThread.OnTerminate

    property CacheGrpIconsThread: ctLEmuTKCacheGrpIcons
      read FCacheGrpIconsThread write SetCacheGrpIconsThread;
    procedure CacheGrpIconsThreadTerminated(Sender: TObject);
    // For TThread.OnTerminate

    property CacheSoftIconsThread: ctLEmuTKCacheSoftIcons read FCacheSoftIconsThread write SetCacheSoftIconsThread;
    procedure CacheSoftIconsThreadTerminated(Sender: TObject);
    // For TThread.OnTerminate

    procedure LoadIcons;
    procedure LoadSystemsIcons;
    procedure LoadGrpIcons(aGroupList: cEmutecaGroupList);
    procedure LoadSoftIcons(aSoftList: cEmutecaSoftList);

    procedure LoadEmuteca;
    //< Load Emuteca, remove not saved data.
    procedure SaveEmuteca;
    //< Save Emuteca.

    function AddZoneIcon(aFolder: string; FileInfo: TSearchRec): boolean;
    //< Add Zone icon to list

    function RunSoftware(aSoftware: cEmutecaSoftware): boolean;
    //< Run a software

    function DoProgressBar(const Title, Info: string;
      const Value, MaxValue: int64; const IsCancelable: Boolean): boolean;
    //< Progress bar call back

    function DoChangeSystem(aSystem: cEmutecaSystem): boolean;
    function DoChangeGrpList(aGroupList: cEmutecaGroupList): boolean;
    function DoChangeGroup(aGroup: cEmutecaGroup): boolean;
    function DoChangeSoft(aSoft: cEmutecaSoftware): boolean;

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

procedure TfrmLEmuTKMain.SetGUIIconsFile(AValue: string);
begin
  if FGUIIconsFile = AValue then
    Exit;
  FGUIIconsFile := SetAsFile(AValue);
end;

procedure TfrmLEmuTKMain.SetCacheSysIconsThread(AValue: ctLEmuTKCacheSysIcons);
begin
  if FCacheSysIconsThread = AValue then
    Exit;
  FCacheSysIconsThread := AValue;
end;

procedure TfrmLEmuTKMain.SetCurrentGroup(AValue: cEmutecaGroup);
begin
  if FCurrentGroup = AValue then
    Exit;
  FCurrentGroup := AValue;

  if Assigned(CurrentGroup) then
    LoadSoftIcons(CurrentGroup.SoftList);
end;

procedure TfrmLEmuTKMain.SetCurrentSoft(AValue: cEmutecaSoftware);
begin
  if FCurrentSoft = AValue then
    Exit;
  FCurrentSoft := AValue;
end;

procedure TfrmLEmuTKMain.SetCurrentSystem(AValue: cEmutecaSystem);
begin
  if FCurrentSystem = AValue then
    Exit;
  FCurrentSystem := AValue;
end;

procedure TfrmLEmuTKMain.SetCacheGrpIconsThread(AValue: ctLEmuTKCacheGrpIcons);
begin
  if FCacheGrpIconsThread = AValue then
    Exit;
  FCacheGrpIconsThread := AValue;
end;

procedure TfrmLEmuTKMain.SetCacheSoftIconsThread(AValue: ctLEmuTKCacheSoftIcons
  );
begin
  if FCacheSoftIconsThread=AValue then Exit;
  FCacheSoftIconsThread:=AValue;
end;

procedure TfrmLEmuTKMain.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(SetAsAbsoluteFile(AValue, ProgramDirectory));
  w7zSetGlobalCache(SHA1Folder);
end;

procedure TfrmLEmuTKMain.Setw7zErrorFileName(AValue: string);
begin
  Fw7zErrorFileName := SetAsFolder(SetAsAbsoluteFile(AValue,
    ProgramDirectory));
  w7zSetErrorListFile(w7zErrorFileName);
end;

procedure TfrmLEmuTKMain.CacheSysIconsThreadTerminated(Sender: TObject);
begin
  CacheSysIconsThread := nil;
end;

procedure TfrmLEmuTKMain.CacheGrpIconsThreadTerminated(Sender: TObject);
begin
  CacheGrpIconsThread := nil;
end;

procedure TfrmLEmuTKMain.CacheSoftIconsThreadTerminated(Sender: TObject);
begin
  CacheSoftIconsThread := nil;
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
  aFolder := SetAsAbsoluteFile(GUIConfig.DumpIcnFolder, ProgramDirectory);
  for aFile in EmutecaDumpStatusStrK do
    DumpIcons.AddImageFile(aFolder + aFile + '.png');
  for aFile in LazEmuTKDumpInfoIconFiles do
    DumpIcons.AddImageFile(aFolder + aFile + '.png');

  LoadSystemsIcons;
end;

procedure TfrmLEmuTKMain.LoadEmuteca;
begin
  // Fix runtime errors, while trying to update
  fmEmutecaMainFrame.Emuteca := nil;
  LoadIcons; // Resets cached icons

  Emuteca.LoadAllData;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.SaveEmuteca;
begin
  Emuteca.SaveAllData;
end;

function TfrmLEmuTKMain.DoProgressBar(const Title, Info: string; const Value,
  MaxValue: int64; const IsCancelable: Boolean): boolean;
begin
  // We asume that fmCHXProgressBar is always created...
  Result := fmProgressBar.UpdTextAndBar(Title, Info,
    Value, MaxValue, IsCancelable);
end;

function TfrmLEmuTKMain.DoChangeSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := True;
  CurrentSoft := nil;
  CurrentGroup := nil;
  CurrentSystem := aSystem;

  mimmSystem.Enabled := Assigned(aSystem);

  // TODO: This must be enabled if system's current emulator is set
  mimmEmulator.Enabled := mimmSystem.Enabled;
end;

function TfrmLEmuTKMain.DoChangeGrpList(aGroupList:
  cEmutecaGroupList): boolean;
begin
  Result := True;
  LoadGrpIcons(aGroupList);
end;

function TfrmLEmuTKMain.DoChangeGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := True;

  mimmGroup.Enabled := Assigned(aGroup);

  if Assigned(aGroup) then
    Result := DoChangeSystem(cEmutecaSystem(aGroup.CachedSystem));
  CurrentGroup := aGroup;
end;

function TfrmLEmuTKMain.DoChangeSoft(aSoft: cEmutecaSoftware): boolean;
begin
  Result := True;

  mimmSoft.Enabled := Assigned(aSoft);

  if Assigned(aSoft) then
    Result := DoChangeGroup(cEmutecaGroup(aSoft.CachedGroup));
  CurrentSoft := aSoft;
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

function TfrmLEmuTKMain.RunSoftware(aSoftware: cEmutecaSoftware): boolean;
var
  aError: integer;
begin
  Result := False;
  aError := Emuteca.RunSoftware(aSoftware);

  case aError of
    0: ; // All OK
    kErrorRunSoftUnknown:
    begin
      ShowMessageFmt('TfmLEmuTKMain.RunSoftware: Unknown Error.' +
        LineEnding + '%0:s' + LineEnding + '%1:s',
        [aSoftware.Folder, aSoftware.FileName]);
    end;
    kErrorRunSoftNoSoft:
    begin
      ShowMessage('TfmLEmuTKMain.RunSoftware: Software = nil.');
    end;
    kErrorRunSoftNoEmu:
    begin
      ShowMessageFmt('TfmLEmuTKMain.RunSoftware: Emulator = nil.' +
        LineEnding + '%0:s' + LineEnding + '%1:s',
        [aSoftware.Folder, aSoftware.FileName]);
    end;
    kErrorRunSoftNoSoftFile:
    begin
      ShowMessageFmt('TfmLEmuTKMain.RunSoftware: Soft file not found.' +
        LineEnding + '%0:s' + LineEnding + '%1:s',
        [aSoftware.Folder, aSoftware.FileName]);
    end;
    kErrorRunSoftNoEmuFile:
    begin
      ShowMessage('TfmLEmuTKMain.RunSoftware: Emulator executable not found');
    end;
    kError7zDecompress:
    begin
      ShowMessage('TfmLEmuTKMain.RunSoftware: Unknown decompress error.');
    end;
    else
    begin
      ShowMessageFmt('TfmLEmuTKMain.RunSoftware: Emulator returned: %0:d',
        [aError]);
    end;
  end;

  Result := aError = 0;
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

procedure TfrmLEmuTKMain.LoadGrpIcons(aGroupList: cEmutecaGroupList);
begin
  // Teminate if it's running
  if assigned(CacheGrpIconsThread) then
  begin
    CacheGrpIconsThread.OnTerminate := nil;
    CacheGrpIconsThread.Terminate;
    // CacheGrpIconsThread.WaitFor; Don't wait
  end;
  // Auto freed with FreeOnTerminate and nil

  if not (Assigned(aGroupList) and Assigned(IconList) and
    Assigned(GUIConfig)) then
    Exit;

  FCacheGrpIconsThread := ctLEmuTKCacheGrpIcons.Create;
  if Assigned(CacheGrpIconsThread.FatalException) then
    raise CacheGrpIconsThread.FatalException;
  CacheGrpIconsThread.OnTerminate := @CacheGrpIconsThreadTerminated; //Autonil

  CacheGrpIconsThread.GroupList := aGroupList;
  CacheGrpIconsThread.IconList := IconList;
  CacheGrpIconsThread.ImageExt := GUIConfig.ImageExtensions;

  CacheGrpIconsThread.Start;
end;

procedure TfrmLEmuTKMain.LoadSoftIcons(aSoftList: cEmutecaSoftList);
begin
   // Teminate if it's running
  if assigned(CacheSoftIconsThread) then
  begin
    CacheSoftIconsThread.OnTerminate := nil;
    CacheSoftIconsThread.Terminate;
    // CacheSoftIconsThread.WaitFor; Don't wait
  end;
  // Auto freed with FreeOnTerminate and nil

  if not (Assigned(aSoftList) and Assigned(IconList) and
    Assigned(GUIConfig)) then
    Exit;

  FCacheSoftIconsThread := ctLEmuTKCacheSoftIcons.Create;
  if Assigned(CacheSoftIconsThread.FatalException) then
    raise CacheSoftIconsThread.FatalException;
  CacheSoftIconsThread.OnTerminate := @CacheSoftIconsThreadTerminated; //Autonil

  CacheSoftIconsThread.SoftList := aSoftList;
  CacheSoftIconsThread.IconList := IconList;
  CacheSoftIconsThread.ImageExt := GUIConfig.ImageExtensions;

  CacheSoftIconsThread.Start;
end;

procedure TfrmLEmuTKMain.FormCreate(Sender: TObject);
var
  aIni: TMemIniFile;
begin
  Application.Title := Format(rsFmtApplicationTitle,
    [Application.Title, GetFileVersion]); // Usually it's autodeleted in .lpr file...

  // Always work from program folder :P
  // TODO 3: Change for Linux... ¬_¬U
  ChDir(ProgramDirectory);

  // Used to store translations...
  if not DirectoryExistsUTF8('locale') then
    mkdir('locale');

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  // Windows Caption
  Caption := Format(krsFmtWindowCaption, [Application.Title, Caption]);

  // Loading GUI config
  FGUIConfig := cGUIConfig.Create(self);
  GUIConfig.DefaultFileName := SetAsAbsoluteFile('GUI.ini', ProgramDirectory);
  GUIConfig.LoadFromFile('');
  IniPropStorage.IniFileName := GUIConfig.DefaultFileName;
  IniPropStorage.Restore;

  GUIIconsFile := SetAsFile(SetAsAbsoluteFile(GUIConfig.GUIIcnFile, ProgramDirectory));

  // Experimental:
  //   - 7z files cache
  //   - 7z error logs
  SHA1Folder := GUIConfig.GlobalCache;
  w7zErrorFileName := GUIConfig.w7zErrorFileName;

  // Image lists
  FIconList := cCHXImageList.Create(True);
  FDumpIcons := cCHXImageList.Create(True);
  FZoneIcons := cCHXImageMap.Create(True);

  // Creating ProgressBar form
  FfmProgressBar := TfmCHXProgressBar.SimpleForm(GUIConfig.DefaultFileName);

  // Creating Emuteca Core :-D
  FEmuteca := cEmuteca.Create(self);
  Emuteca.BaseFolder := ProgramDirectory;
  Emuteca.ProgressCallBack := @DoProgressBar;
  Emuteca.LoadConfig(GUIConfig.EmutecaIni);

  // This must be after creating and loading Emuteca,
  //   it runs CacheSysIconsThread too
  LoadIcons;

  // Creating main frame
  FfmEmutecaMainFrame := TfmLEmuTKMain.Create(Self);
  fmEmutecaMainFrame.OnSystemChanged := @DoChangeSystem;
  fmEmutecaMainFrame.OnGrpListChanged := @DoChangeGrpList;
  fmEmutecaMainFrame.OnGroupChanged := @DoChangeGroup;
  fmEmutecaMainFrame.OnSoftChanged := @DoChangeSoft;
  fmEmutecaMainFrame.OnSoftDblClk := @RunSoftware;
  fmEmutecaMainFrame.pmGroup:= pmGroup;
  fmEmutecaMainFrame.pmSoft:= pmSoft;
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

  aIni := TMemIniFile.Create(GUIConfig.DefaultFileName);
  try
    fmEmutecaMainFrame.LoadGUIConfig(aIni);
  finally
    aIni.Free;
  end;
  fmEmutecaMainFrame.Parent := Self;

  // Misc
  actAutoSave.Checked := GUIConfig.SaveOnExit; // TODO: Use IniPropStorage?

  // if there is not enabled systems then open SysManager
  if Emuteca.SystemManager.EnabledList.Count = 0 then
    actSystemManager.Execute;
end;

procedure TfrmLEmuTKMain.actEmulatorManagerExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmLEmuTKEmuManager.SimpleForm(Emuteca.EmulatorManager, SHA1Folder, GUIIconsFile, GUIConfig.DefaultFileName);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actExportSoftDataExecute(Sender: TObject);
begin
  TfmActExportSoftData.SimpleForm(Emuteca, GUIIconsFile, GUIConfig.DefaultFileName);
end;

procedure TfrmLEmuTKMain.actImportSoftDataExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmEmutecaActImportSoftData.SimpleForm(Emuteca, GUIIconsFile, GUIConfig.DefaultFileName);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actMediaManagerExecute(Sender: TObject);
begin
  TfmLEmuTKMediaManager.SimpleForm(Emuteca, GUIIconsFile, GUIConfig);
end;

procedure TfrmLEmuTKMain.actMergeGroupFilesExecute(Sender: TObject);
begin
  TfmLEmuTKactMergeGroup.SimpleForm(CurrentGroup, GUIIconsFile, GUIConfig.DefaultFileName);
end;

procedure TfrmLEmuTKMain.actOpen7zCacheFolderExecute(Sender: TObject);
begin
  OpenDocument(w7zGetCacheDir);
end;

procedure TfrmLEmuTKMain.actOpenEmutecaFolderExecute(Sender: TObject);
begin
   OpenDocument(ProgramDirectory);
end;

procedure TfrmLEmuTKMain.actOpenSoftFolderExecute(Sender: TObject);
begin
  if assigned(CurrentSoft) then
    OpenDocument(CurrentSoft.Folder);
end;

procedure TfrmLEmuTKMain.actOpenSystemBaseFolderExecute(Sender: TObject);
begin
  if not assigned(CurrentSystem) then Exit;

  if not OpenDocument(CurrentSystem.BaseFolder) then
    raise EFileNotFoundException.CreateFmt(rsFmtNotFound, [GetCurrentDirUTF8, CurrentSystem.BaseFolder]);
end;

procedure TfrmLEmuTKMain.actOpenTempFolderExecute(Sender: TObject);
begin
  if not OpenDocument(Emuteca.TempFolder) then
    raise EFileNotFoundException.CreateFmt(rsFmtNotFound, [GetCurrentDirUTF8, Emuteca.TempFolder]);
end;

procedure TfrmLEmuTKMain.actRunSoftwareExecute(Sender: TObject);
begin
  RunSoftware(CurrentSoft);
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
    SetAsAbsoluteFile(GUIConfig.ScriptsFolder, ProgramDirectory), GUIIconsFile, GUIConfig.DefaultFileName);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actAddFolderExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmEmutecaActAddFolder.SimpleForm(Emuteca, GUIIconsFile, GUIConfig.DefaultFileName);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actAutoSaveExecute(Sender: TObject);
begin
  GUIConfig.SaveOnExit := actAutoSave.Checked;
end;

procedure TfrmLEmuTKMain.actCleanSystemDataExecute(Sender: TObject);
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
  CurrentSystem.CleanSoftGroup;
  CurrentSystem.ProgressCallBack := aPCB;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actEditEmulatorExecute(Sender: TObject);
begin
   if not assigned(CurrentSystem) then
    Exit;


  // TODO
  {
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;


  //TfmLEmuTKFullSystemEditor.SimpleForm(Emuteca, CurrentSystem, SHA1Folder,
    GUIIconsFile, GUIConfig.ConfigFile);
  //LoadSystemsIcons;

  fmEmutecaMainFrame.Emuteca := Emuteca;
  }
end;

procedure TfrmLEmuTKMain.actEditSystemExecute(Sender: TObject);
begin
  if not assigned(CurrentSystem) then
    Exit;

  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmLEmuTKFullSystemEditor.SimpleForm(Emuteca, CurrentSystem, SHA1Folder, GUIIconsFile, GUIConfig.DefaultFileName);
  LoadSystemsIcons;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actAddSoftExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmEmutecaActAddSoft.SimpleForm(Emuteca, GUIIconsFile, GUIConfig.DefaultFileName);

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actSystemManagerExecute(Sender: TObject);
begin
  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  TfmLEmuTKSysManager.SimpleForm(Emuteca, SHA1Folder, GUIIconsFile, GUIConfig.DefaultFileName);
  LoadSystemsIcons; // Reloads system icons

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.actUpdateGroupListExecute(Sender: TObject);
begin
  if not Assigned(CurrentSystem) then Exit;

  // Fix runtime errors, while trying to update if something is changed
  fmEmutecaMainFrame.Emuteca := nil;

  CurrentSystem.CacheGroups;

  fmEmutecaMainFrame.Emuteca := Emuteca;
end;

procedure TfrmLEmuTKMain.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  aIni: TMemIniFile;
begin
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

  // Teminate threads if they are running.
  if Assigned(CacheSysIconsThread) then
  begin
    CacheSysIconsThread.OnTerminate := nil;
    CacheSysIconsThread.Terminate;
    CacheSysIconsThread.WaitFor;
  end;
  // CacheSysIconsThread.Free; Auto freed with FreeOnTerminate

  aIni := TMemIniFile.Create(GUIConfig.DefaultFileName);
  try
    fmEmutecaMainFrame.SaveGUIConfig(aIni);
  finally
    aIni.Free;
  end;

  GUIConfig.SaveToFile('', False); // File has Forms config too, don't delete
  if GUIConfig.SaveOnExit then SaveEmuteca;

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
