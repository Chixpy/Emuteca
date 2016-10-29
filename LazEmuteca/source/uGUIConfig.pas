unit uGUIConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8,
  uCHXStrUtils, uCHXvcConfig;

const
  // Sections and keys for ini file
  // [Images]
  krsSectionImages = 'Images';
  krsKeyDefImgFolder = 'DefImgFolder';
  krsKeyGUIIcnFile = 'GUIIcnFile';
  krsKeyDumpIcnFolder = 'DumpIcnFolder';
  krsKeyZoneIcnFolder = 'ZoneIcnFolder';

  // [Config]
  krsSectionConfig = 'Config';
  krsKeyEmutecaIni = 'EmutecaIni';
  krsKeySaveOnExit = 'SaveOnExit';
  krsKeySearchFile = 'SearchFile';
  krsKeyHelpFolder = 'HelpFolder';

  // [Tools]
  krsSectionTools = 'Tools';
  krsKeyMPlayerExe = 'mPlayerExe';

type

  { cGUIConfig: Class wich has all general options and configurations
      for GUI use only.

    All folder paths (absolute and relative) have the trailing path separator.
  }
  cGUIConfig = class(vcConfig)
  private
    FConfigFile: string;
    FDefImgFolder: string;
    FEmutecaIni: string;
    FZoneIcnFolder: string;
    FHelpFolder: string;
    FGUIIcnFile: string;
    FmPlayerExe: string;
    FSaveOnExit: boolean;
    FSearchFile: string;
    FDumpIcnFolder: string;
    procedure SetConfigFile(AValue: string);
    procedure SetDefImgFolder(AValue: string);
    procedure SetEmutecaIni(AValue: string);
    procedure SetZoneIcnFolder(AValue: string);
    procedure SetHelpFolder(AValue: string);
    procedure SetGUIIcnFile(AValue: string);
    procedure SetmPlayerExe(AValue: string);
    procedure SetSaveOnExit(AValue: boolean);
    procedure SetSearchFile(AValue: string);
    procedure SetDumpIcnFolder(AValue: string);

  protected
    procedure OnLoadConfig(IniFile: TIniFile); override;
    procedure OnSaveConfig(IniFile: TIniFile); override;
    procedure OnSetDefaults; override;

  published
    // Images
    // ------

    property DefImgFolder: string read FDefImgFolder write SetDefImgFolder;
    //< Folder with default images and icons for soft, parents and systems.
    property GUIIcnFile: string read FGUIIcnFile write SetGUIIcnFile;
    //< File for GUI Icons
    property ZoneIcnFolder: string read FZoneIcnFolder write SetZoneIcnFolder;
    //< Folder with flags of zones
    property DumpIcnFolder: string read FDumpIcnFolder write SetDumpIcnFolder;
    //< Folder with icons for Dump Status

    // Tools
    // -----
    property mPlayerExe: string read FmPlayerExe write SetmPlayerExe;
    //< Path to mPlayer[2].exe

    // Config/Data
    // -----------
    property EmutecaIni: string read FEmutecaIni write SetEmutecaIni;
    //< Emuteca config file
    property SaveOnExit: boolean read FSaveOnExit write SetSaveOnExit;
    //< Save software and parent lists on exit?
    property HelpFolder: string read FHelpFolder write SetHelpFolder;
    //< Folder with help
    property SearchFile: string read FSearchFile write SetSearchFile;
    //< File with search configuration

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cGUIConfig }

procedure cGUIConfig.SetConfigFile(AValue: string);
begin
  FConfigFile := SetAsFile(AValue);
end;

procedure cGUIConfig.SetDefImgFolder(AValue: string);
begin
  FDefImgFolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetEmutecaIni(AValue: string);
begin
  FEmutecaIni := SetAsFile(AValue);
end;

procedure cGUIConfig.SetZoneIcnFolder(AValue: string);
begin
  FZoneIcnFolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetHelpFolder(AValue: string);
begin
  FHelpFolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetGUIIcnFile(AValue: string);
begin
  FGUIIcnFile := SetAsFile(AValue);
end;

procedure cGUIConfig.SetmPlayerExe(AValue: string);
begin
  FmPlayerExe := SetAsFile(AValue);
end;

procedure cGUIConfig.SetSaveOnExit(AValue: boolean);
begin
  if FSaveOnExit = AValue then
    Exit;
  FSaveOnExit := AValue;
end;

procedure cGUIConfig.SetSearchFile(AValue: string);
begin
  FSearchFile := SetAsFile(AValue);
end;

procedure cGUIConfig.SetDumpIcnFolder(AValue: string);
begin
  FDumpIcnFolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.OnLoadConfig(IniFile: TIniFile);
begin
  // Images
  DefImgFolder := IniFile.ReadString(krsSectionImages,
    krsKeyDefImgFolder, DefImgFolder);
  ZoneIcnFolder := IniFile.ReadString(krsSectionImages,
    krsKeyZoneIcnFolder, ZoneIcnFolder);
  DumpIcnFolder := IniFile.ReadString(krsSectionImages,
    krsKeyDumpIcnFolder, DumpIcnFolder);
  GUIIcnFile := IniFile.ReadString(krsSectionImages,
    krsKeyGUIIcnFile, GUIIcnFile);


  // Config/Data
  EmutecaIni := IniFile.ReadString(krsSectionConfig,
    krsKeyEmutecaIni, EmutecaIni);
  SaveOnExit := IniFile.ReadBool(krsSectionConfig, krsKeySaveOnExit,
    SaveOnExit);
  SearchFile := IniFile.ReadString(krsSectionConfig,
    krsKeySearchFile, SearchFile);
  HelpFolder := IniFile.ReadString(krsSectionConfig,
    krsKeyHelpFolder, HelpFolder);

  // Tools
  mPlayerExe := IniFile.ReadString(krsSectionTools,
    krsKeyMPlayerExe, mPlayerExe);
end;

procedure cGUIConfig.OnSaveConfig(IniFile: TIniFile);
begin
  // Images
  IniFile.WriteString(krsSectionImages, krsKeyDefImgFolder, DefImgFolder);
  IniFile.WriteString(krsSectionImages, krsKeyZoneIcnFolder, ZoneIcnFolder);
  IniFile.WriteString(krsSectionImages, krsKeyDumpIcnFolder, DumpIcnFolder);
  IniFile.WriteString(krsSectionImages, krsKeyGUIIcnFile, GUIIcnFile);


  // Data
  IniFile.WriteString(krsSectionConfig, krsKeyEmutecaIni, EmutecaIni);
  IniFile.WriteBool(krsSectionConfig, krsKeySaveOnExit, SaveOnExit);
  IniFile.WriteString(krsSectionConfig, krsKeySearchFile, SearchFile);
  IniFile.WriteString(krsSectionConfig, krsKeyHelpFolder, HelpFolder);

  // Tools
  IniFile.WriteString(krsSectionTools, krsKeyMPlayerExe, mPlayerExe);
end;

procedure cGUIConfig.OnSetDefaults;
begin
  // Images
  DefImgFolder := 'Images/Default';
  GUIIcnFile := 'Images/GUI/Icons.ini';
  ZoneIcnFolder := 'Images/Zone';
  DumpIcnFolder := 'Images/DumpInfo';

  // Config/Data
  EmutecaIni := 'Emuteca.ini';
  SaveOnExit := True;
  SearchFile := 'Search.ini';
  HelpFolder := 'Help';

  // Tools
  mPlayerExe := 'Tools/mplayer/mplayer2.exe';
end;

constructor cGUIConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cGUIConfig.Destroy;
begin
  inherited Destroy;
end;

end.
