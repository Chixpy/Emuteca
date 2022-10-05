unit ucEIBConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  uCHXStrUtils,
  // CHX abstract classes
  uaCHXConfig;

const
  krsFoldersSection = 'Folders';
  krsLastInFolderKey = 'LastInFolder';
  krsLastOutFolderKey = 'LastOutFolder';

type

  { cEIBConfig }

  cEIBConfig = class(caCHXConfig)

  private
    FLastInFolder: string;
    FLastOutFolder: string;
    procedure SetLastInFolder(AValue: string);
    procedure SetLastOutFolder(AValue: string);

  public
    property LastOutFolder: string read FLastOutFolder write SetLastOutFolder;
    property LastInFolder: string read FLastInFolder write SetLastInFolder;

    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure ResetDefaultConfig; override;
    {< Sets config properties to default values. }
    procedure SaveToIni(aIniFile: TMemIniFile); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation


{ cEIBConfig }

procedure cEIBConfig.SetLastInFolder(AValue: string);
begin
  FLastInFolder := SetAsFolder(AValue);
end;

procedure cEIBConfig.SetLastOutFolder(AValue: string);
begin
  FLastOutFolder := SetAsFolder(AValue);
end;

procedure cEIBConfig.LoadFromIni(aIniFile: TMemIniFile);
begin
  LastInFolder := aIniFile.ReadString(krsFoldersSection,
    krsLastInFolderKey, LastInFolder);
  LastOutFolder := aIniFile.ReadString(krsFoldersSection,
    krsLastOutFolderKey, LastOutFolder);
end;

procedure cEIBConfig.ResetDefaultConfig;
begin
  LastOutFolder := '';
  LastInFolder := '';
end;

procedure cEIBConfig.SaveToIni(aIniFile: TMemIniFile);
begin
  aIniFile.WriteString(krsFoldersSection, krsLastInFolderKey, LastInFolder);
  aIniFile.WriteString(krsFoldersSection, krsLastOutFolderKey, LastOutFolder);
end;

constructor cEIBConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cEIBConfig.Destroy;
begin
  inherited Destroy;
end;

end.
