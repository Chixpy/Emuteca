unit uGUIConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8, Graphics,
  // CHX units
  uCHX7zWrapper, uCHXStrUtils,
  // CHX abstracts
  uaCHXConfig;

const
  // Sections and keys for ini file
  // [Images]
  krsSectionImages = 'Images';
  krsKeyDefImgFolder = 'DefImgFolder';
  krsKeyGUIIcnFile = 'GUIIcnFile';
  krsKeyDumpIcnFolder = 'DumpIcnFolder';
  krsKeyZoneIcnFolder = 'ZoneIcnFolder';
  krsKeyImgExt = 'ImageExt';

  // [Texts]
  krsSectionTexts = 'Texts';
  krsKeyTxtExt = 'TxtExt';

  // [Config]
  krsSectionConfig = 'Config';
  krsKeyEmutecaIni = 'EmutecaIni';
  krsKeyCurrSystem = 'CurrSystem';
  krsKeySaveOnExit = 'SaveOnExit';
  krsKeySearchFile = 'SearchFile';
  krsKeyHelpFolder = 'HelpFolder';

  // [Tools]
  krsSectionTools = 'Tools';
  krsKeyScriptsFolder = 'ScriptsFolder';
  krsKeyMPlayerExe = 'mPlayerExe';

  // [Experimental]
  krsSectionExperimental = 'Experimental';
  krsKeyGlobalCache = 'GlobalCache';
  krsKeyw7zErrorFileName = 'w7zErrorFileName';

type

  { cGUIConfig: Class wich has all general options and configurations
      for GUI use only.

    All folder paths (absolute and relative) have the trailing path separator.
  }
  cGUIConfig = class(caCHXConfig)
  private
    FCurrSystem: string;
    FDefImgFolder: string;
    FEmutecaIni: string;
    FGlobalCache: string;
    FImageExtensions: TStringList;
    FScriptsFolder: string;
    FTextExtensions: TStringList;
    Fw7zErrorFileName: string;
    FZoneIcnFolder: string;
    FHelpFolder: string;
    FGUIIcnFile: string;
    FmPlayerExe: string;
    FSaveOnExit: boolean;
    FSearchFile: string;
    FDumpIcnFolder: string;
    procedure SetCurrSystem(AValue: string);
    procedure SetDefImgFolder(AValue: string);
    procedure SetEmutecaIni(AValue: string);
    procedure SetGlobalCache(AValue: string);
    procedure SetScriptsFolder(AValue: string);
    procedure Setw7zErrorFileName(AValue: string);
    procedure SetZoneIcnFolder(AValue: string);
    procedure SetHelpFolder(AValue: string);
    procedure SetGUIIcnFile(AValue: string);
    procedure SetmPlayerExe(AValue: string);
    procedure SetSaveOnExit(AValue: boolean);
    procedure SetSearchFile(AValue: string);
    procedure SetDumpIcnFolder(AValue: string);

  public
    procedure ResetDefaultConfig; override;

    procedure LoadFromIni(IniFile: TMemIniFile); override;
    procedure SaveToIni(IniFile: TMemIniFile); override;


    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

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
    property ImageExtensions: TStringList read FImageExtensions;

    // Texts
    property TextExtensions: TStringList read FTextExtensions;

    // Tools
    // -----
    property ScriptsFolder: string read FScriptsFolder write SetScriptsFolder;
    property mPlayerExe: string read FmPlayerExe write SetmPlayerExe;
    //< Path to mPlayer[2].exe

    // Config/Data
    // -----------
    property EmutecaIni: string read FEmutecaIni write SetEmutecaIni;
    //< Emuteca config file
    property CurrSystem: string read FCurrSystem write SetCurrSystem;
    //< Last system used
    property SaveOnExit: boolean read FSaveOnExit write SetSaveOnExit;
    //< Save software and parent lists on exit?
    property HelpFolder: string read FHelpFolder write SetHelpFolder;
    //< Folder with help
    property SearchFile: string read FSearchFile write SetSearchFile;
    //< File with search configuration

    // Experimental
    property GlobalCache: string read FGlobalCache write SetGlobalCache;

    property w7zErrorFileName: string read Fw7zErrorFileName write Setw7zErrorFileName;
end;

implementation

{ cGUIConfig }

procedure cGUIConfig.SetDefImgFolder(AValue: string);
begin
  FDefImgFolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetCurrSystem(AValue: string);
begin
  if FCurrSystem = AValue then Exit;
  FCurrSystem := AValue;
end;

procedure cGUIConfig.SetEmutecaIni(AValue: string);
begin
  FEmutecaIni := SetAsFile(AValue);
end;

procedure cGUIConfig.SetGlobalCache(AValue: string);
begin
  FGlobalCache := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetScriptsFolder(AValue: string);
begin
  FScriptsFolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.Setw7zErrorFileName(AValue: string);
begin
  Fw7zErrorFileName:=SetAsFile(AValue)
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

procedure cGUIConfig.ResetDefaultConfig;
var
  TempStr: string;
begin
  // Images
  DefImgFolder := 'Images/Default';
  GUIIcnFile := 'Images/GUI/Icons.ini';
  ZoneIcnFolder := 'Images/Zone';
  DumpIcnFolder := 'Images/DumpInfo';

  TempStr := '"' + UTF8LowerCase(GraphicFileMask(TGraphic)) + '"';
  TempStr := UTF8TextReplace(TempStr, '*.', '');
  TempStr := UTF8TextReplace(TempStr, ';', '","');
  ImageExtensions.CommaText := TempStr;

  // Texts
  TextExtensions.CommaText := 'txt,nfo';

  // Config/Data
  EmutecaIni := 'Emuteca.ini';
  CurrSystem := '';
  SaveOnExit := True;
  SearchFile := 'Search.ini';
  HelpFolder := 'Help';

  // Tools
  ScriptsFolder := 'Scripts';
  mPlayerExe := 'Tools/mplayer/mplayer2.exe';

  // Experimental
  GlobalCache := 'SHA1Cache/';
  w7zErrorFileName := 'w7zErrors.log'
end;

procedure cGUIConfig.LoadFromIni(IniFile: TMemIniFile);
begin
  // Images
  DefImgFolder := IniFile.ReadString(krsSectionImages, krsKeyDefImgFolder, DefImgFolder);
  ZoneIcnFolder := IniFile.ReadString(krsSectionImages,
    krsKeyZoneIcnFolder, ZoneIcnFolder);
  DumpIcnFolder := IniFile.ReadString(krsSectionImages,
    krsKeyDumpIcnFolder, DumpIcnFolder);
  GUIIcnFile := IniFile.ReadString(krsSectionImages,
    krsKeyGUIIcnFile, GUIIcnFile);
  ImageExtensions.CommaText :=
    IniFile.ReadString(krsSectionImages, krsKeyImgExt,
    ImageExtensions.CommaText);

  // Texts
  TextExtensions.CommaText :=
    IniFile.ReadString(krsSectionTexts, krsKeyTxtExt,
    TextExtensions.CommaText);

  // Config/Data
  EmutecaIni := IniFile.ReadString(krsSectionConfig,
    krsKeyEmutecaIni, EmutecaIni);
    CurrSystem := IniFile.ReadString(krsSectionConfig,
    krsKeyCurrSystem, CurrSystem);
  SaveOnExit := IniFile.ReadBool(krsSectionConfig, krsKeySaveOnExit,
    SaveOnExit);
  SearchFile := IniFile.ReadString(krsSectionConfig,
    krsKeySearchFile, SearchFile);
  HelpFolder := IniFile.ReadString(krsSectionConfig,
    krsKeyHelpFolder, HelpFolder);

  // Tools
    ScriptsFolder := IniFile.ReadString(krsSectionTools,
    krsKeyScriptsFolder, ScriptsFolder);
  mPlayerExe := IniFile.ReadString(krsSectionTools,
    krsKeyMPlayerExe, mPlayerExe);

  // Experimental
  GlobalCache := IniFile.ReadString(krsSectionExperimental,
    krsKeyGlobalCache, GlobalCache);
  w7zErrorFileName := IniFile.ReadString(krsSectionExperimental,
    krsKeyw7zErrorFileName, w7zErrorFileName);
end;

procedure cGUIConfig.SaveToIni(IniFile: TMemIniFile);
begin
  // Images
  IniFile.WriteString(krsSectionImages, krsKeyDefImgFolder, DefImgFolder);
  IniFile.WriteString(krsSectionImages, krsKeyZoneIcnFolder, ZoneIcnFolder);
  IniFile.WriteString(krsSectionImages, krsKeyDumpIcnFolder, DumpIcnFolder);
  IniFile.WriteString(krsSectionImages, krsKeyGUIIcnFile, GUIIcnFile);
  IniFile.WriteString(krsSectionImages, krsKeyImgExt,
    ImageExtensions.CommaText);

  // Texts
  IniFile.WriteString(krsSectionTexts, krsKeyTxtExt,
    TextExtensions.CommaText);

  // Data
  IniFile.WriteString(krsSectionConfig, krsKeyEmutecaIni, EmutecaIni);
    IniFile.WriteString(krsSectionConfig, krsKeyCurrSystem, CurrSystem);
  IniFile.WriteBool(krsSectionConfig, krsKeySaveOnExit, SaveOnExit);
  IniFile.WriteString(krsSectionConfig, krsKeySearchFile, SearchFile);
  IniFile.WriteString(krsSectionConfig, krsKeyHelpFolder, HelpFolder);

  // Tools
  IniFile.WriteString(krsSectionTools, krsKeyScriptsFolder, ScriptsFolder);
  IniFile.WriteString(krsSectionTools, krsKeyMPlayerExe, mPlayerExe);


  IniFile.WriteString(krsSectionExperimental, krsKeyGlobalCache, GlobalCache);
  IniFile.WriteString(krsSectionExperimental, krsKeyw7zErrorFileName, w7zErrorFileName);
end;

constructor cGUIConfig.Create(aOwner: TComponent);
begin
  // We must create objects before calling inherited, because
  //   ResetDefaultConfig is called
  FImageExtensions := TStringList.Create;
  ImageExtensions.Sorted := True;
  ImageExtensions.CaseSensitive := False;

  FTextExtensions :=TStringList.Create;
  TextExtensions.Sorted := True;
  TextExtensions.CaseSensitive := False;

  inherited Create(aOwner);
end;

destructor cGUIConfig.Destroy;
begin
  FreeAndNil(FImageExtensions);
  FreeAndNil(FTextExtensions);
  inherited Destroy;
end;

end.
