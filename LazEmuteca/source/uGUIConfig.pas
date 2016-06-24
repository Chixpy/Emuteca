unit uGUIConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8,
  uCHXStrUtils, uCHXvcConfig;

const
  // Sections and keys for ini file
  // [Images]
  krsIniSectionImages = 'Images';
  krsIniKeyImagesFolder = 'ImagesFolder';
  krsIniKeyFlagsSubfolder = 'FlagsSubfolder';
  krsIniKeyVIIconsSubfolder = 'VIIconsSubfolder';
  krsIniKeyIconsSubfolder = 'IconsSubfolder';
  krsIniKeyIconsIniFile = 'IconsIniFile';

  // [Config]
  krsIniSectionConfig = 'Config';
  krsSearchFile = 'SearchFile';
  krsHelpFolder = 'HelpFolder';

  // [Tools]
  krsTools = 'Tools';
  krsMPlayerSubfolder = 'mPlayerSubfolder';
  krsmPlayerExecutable = 'mPlayerExecutable';

type

  { cGUIConfig: Class wich has all general options and configurations
      for GUI use only.

    All folder paths (absolute and relative) have the trailing path separator.
  }
  cGUIConfig = class(vcConfig)
  private
    FConfigFile: string;
    FDefaultImagesSubfolder: string;
    FFlagsSubfolder: string;
    FHelpFolder: string;
    FIconsIniFile: string;
    FIconsSubfolder: string;
    FImagesFolder: string;
    FmPlayerExecutable: string;
    FmPlayerSubfolder: string;
    FSearchFile: string;
    FToolsFolder: string;
    FVIIconsSubfolder: string;
    procedure SetConfigFile(AValue: string);
    procedure SetDefaultImagesSubfolder(AValue: string);
    procedure SetFlagsSubfolder(AValue: string);
    procedure SetHelpFolder(AValue: string);
    procedure SetIconsIniFile(AValue: string);
    procedure SetIconsSubfolder(AValue: string);
    procedure SetImagesFolder(AValue: string);
    procedure SetmPlayerExecutable(AValue: string);
    procedure SetmPlayerSubfolder(AValue: string);
    procedure SetSearchFile(AValue: string);
    procedure SetToolsFolder(AValue: string);
    procedure SetVIIconsSubfolder(AValue: string);

  protected
    procedure OnLoadConfig(IniFile: TIniFile); override;
    procedure OnSaveConfig(IniFile: TIniFile); override;
    procedure OnSetDefaults; override;

  published
    // Default images
    property ImagesFolder: string read FImagesFolder write SetImagesFolder;
    property DefaultImagesSubfolder: string
      read FDefaultImagesSubfolder write SetDefaultImagesSubfolder;
    property IconsSubfolder: string read FIconsSubfolder
      write SetIconsSubfolder;
    property IconsIniFile: string read FIconsIniFile write SetIconsIniFile;
    property FlagsSubfolder: string read FFlagsSubfolder
      write SetFlagsSubfolder;
    property VIIconsSubfolder: string read FVIIconsSubfolder
      write SetVIIconsSubfolder;

    // Tools
    property ToolsFolder: string read FToolsFolder write SetToolsFolder;
    property mPlayerSubfolder: string
      read FmPlayerSubfolder write SetmPlayerSubfolder;
    property mPlayerExecutable: string
      read FmPlayerExecutable write SetmPlayerExecutable;

    // Config/Data
    property HelpFolder: string read FHelpFolder write SetHelpFolder;
    property SearchFile: string read FSearchFile write SetSearchFile;

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

procedure cGUIConfig.SetDefaultImagesSubfolder(AValue: string);
begin
  FDefaultImagesSubfolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetFlagsSubfolder(AValue: string);
begin
  FFlagsSubfolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetHelpFolder(AValue: string);
begin
  FHelpFolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetIconsIniFile(AValue: string);
begin
  FIconsIniFile := SetAsFile(AValue);
end;

procedure cGUIConfig.SetIconsSubfolder(AValue: string);
begin
  FIconsSubfolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetImagesFolder(AValue: string);
begin
  FImagesFolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetmPlayerExecutable(AValue: string);
begin
  FmPlayerExecutable := SetAsFile(AValue);
end;

procedure cGUIConfig.SetmPlayerSubfolder(AValue: string);
begin
  FmPlayerSubfolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetSearchFile(AValue: string);
begin
  FSearchFile := SetAsFile(AValue);
end;

procedure cGUIConfig.SetToolsFolder(AValue: string);
begin
  FToolsFolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.SetVIIconsSubfolder(AValue: string);
begin
  FVIIconsSubfolder := SetAsFolder(AValue);
end;

procedure cGUIConfig.OnLoadConfig(IniFile: TIniFile);
begin
  // Images
  ImagesFolder := IniFile.ReadString(krsIniSectionImages,
    krsIniKeyImagesFolder, ImagesFolder);
  FlagsSubfolder := IniFile.ReadString(krsIniSectionImages,
    krsIniKeyFlagsSubfolder, FlagsSubfolder);
  VIIconsSubfolder := IniFile.ReadString(krsIniSectionImages,
    krsIniKeyVIIconsSubfolder, VIIconsSubfolder);
  IconsSubfolder := IniFile.ReadString(krsIniSectionImages,
    krsIniKeyIconsSubfolder, IconsSubfolder);
  IconsIniFile := IniFile.ReadString(krsIniSectionImages,
    krsIniKeyIconsIniFile, IconsIniFile);

  // Config/Data

  // Data
  SearchFile := IniFile.ReadString(krsIniSectionConfig,
    krsSearchFile, SearchFile);
  HelpFolder := IniFile.ReadString(krsIniSectionConfig,
    krsHelpFolder, HelpFolder);

  // Tools

  mPlayerSubfolder := IniFile.ReadString(krsTools,
    krsMPlayerSubfolder, mPlayerSubfolder);
  mPlayerExecutable := IniFile.ReadString(krsTools, krsmPlayerExecutable,
    mPlayerExecutable);
end;

procedure cGUIConfig.OnSaveConfig(IniFile: TIniFile);
begin
  // Images
  IniFile.WriteString(krsIniSectionImages, krsIniKeyImagesFolder,
    ImagesFolder);
  IniFile.WriteString(krsIniSectionImages, krsIniKeyFlagsSubfolder,
    FlagsSubfolder);
  IniFile.WriteString(krsIniSectionImages, krsIniKeyVIIconsSubfolder,
    VIIconsSubfolder);
  IniFile.WriteString(krsIniSectionImages, krsIniKeyIconsSubfolder,
    IconsSubfolder);
  IniFile.WriteString(krsIniSectionImages, krsIniKeyIconsIniFile,
    IconsIniFile);

  // Data
  IniFile.WriteString(krsIniSectionConfig, krsSearchFile, SearchFile);
  IniFile.WriteString(krsIniSectionConfig, krsHelpFolder, HelpFolder);

  // Tools

  IniFile.WriteString(krsTools, krsMPlayerSubfolder, mPlayerSubfolder);
  IniFile.WriteString(krsTools, krsmPlayerExecutable, mPlayerExecutable);
end;

procedure cGUIConfig.OnSetDefaults;
begin
  // Images
  ImagesFolder := 'Images';
  IconsSubfolder := 'Icons';
  IconsIniFile := 'Icons.ini';
  FlagsSubfolder := 'Flags';
  VIIconsSubfolder := 'VerInfo';

  // Config/Data
  SearchFile := 'Search.ini';
  HelpFolder := 'Help';

  // Tools
  ToolsFolder := 'Tools';
  mPlayerSubfolder := SetAsFolder('mplayer');
  mPlayerExecutable := 'mplayer2.exe';
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
