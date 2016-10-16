{ This file is part of Emuteca

  Copyright (C) 2006-2016 Chixpy

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

{ cSystem unit. }
unit ucEmutecaSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazFileUtils, LazUTF8, contnrs,
  uCHXStrUtils,
  uEmutecaCommon, uaEmutecaStorable;

const
  // Ini file Keys
  // -------------
  krsIniKeyEnabled = 'Enabled';  // TODO: uEmutecaCommon.pas?
  krsIniKeyTitle = 'Title';
  krsIniKeyExtraFile = 'ExtraFile';
  krsIniKeyExtensions = 'Extensions';
  krsIniKeyBaseFolder = 'BaseFolder';
  krsIniKeyTempFolder = 'TempFolder';
  krsIniKeyGamesKey = 'GamesKey';
  krsIniKeyExtractAll = 'ExtractAll';
  krsIniKeyMainEmulator = 'MainEmulator';
  krsIniKeyOtherEmulators = 'OtherEmulators';

resourcestring
  rsLoadingSystemList = 'Loading system list...';
  rsSavingSystemList = 'Saving system list...';
  rsAllSystems = 'All Systems';

type

  TEmutecaFileKey = (TEFKSHA1, TEFKCRC32, TEFKFileName, TEFKCustom);

  { @name.

    @definitionList(
    @itemLabel(NOTE:)
    @item(Because PascalScript don't suport overloaded methods,
      we don't use them right here.)
    )
  }

  { cEmutecaSystem }

  cEmutecaSystem = class(caEmutecaStorableIni)
  private
    FBaseFolder: string;
    FCompany: string;
    FEnabled: boolean;
    FExtensions: TStringList;
    FExtractAll: boolean;
    FExtraFile: string;
    FGameKey: TEmutecaFileKey;
    FID: string;
    FMainEmulator: string;
    FModel: string;
    FOtherEmulators: TStringList;
    FTempFolder: string;
    FTitle: string;
    procedure SetBaseFolder(AValue: string);
    procedure SetCompany(AValue: string);
    procedure SetEnabled(AValue: boolean);
    procedure SetExtractAll(AValue: boolean);
    procedure SetExtraFile(AValue: string);
    procedure SetGameKey(AValue: TEmutecaFileKey);
    procedure SetID(AValue: string);
    procedure SetMainEmulator(AValue: string);
    procedure SetModel(AValue: string);
    procedure SetTempFolder(AValue: string);
    procedure SetTitle(AValue: string);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFileIni(IniFile: TCustomIniFile); override;
    procedure SaveToFileIni(IniFile: TCustomIniFile;
      const ExportMode: boolean); override;

  published

    property ID: string read FID write SetID;
    //< ID of the system.

    property Title: string read FTitle write SetTitle;
    {< Visible name (Usually "%Company%: %Model% %(info)%"}

    property ExtraFile: string read FExtraFile write SetExtraFile;
    {< File with extra info of the system, without extension }

    property Enabled: boolean read FEnabled write SetEnabled;
    //< Is the system visible?

    property ExtractAll: boolean read FExtractAll write SetExtractAll;
    //< Must all files be extracted from compressed archives?

    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    {< System base folder

       Used by default to store some data if the file isn't defined
         (System image or text).
    }
    property TempFolder: string read FTempFolder write SetTempFolder;
    {< Temp folder for decompress Software, it's recommended leave it empty
         so Emuteca will use OS Temp folder.

       Some cases of use it:
       @unorderedList(
         @item(File must extracted to a specific folder. So emulator
           can recognize it.)
         @item(Emulator can't run from command line and you must browse to the
           file with the emulator... :-@ )
         @item(Lazy testing... >_<U )
       )
     }
    property MainEmulator: string read FMainEmulator write SetMainEmulator;
    //< Main emulator ID.
    property OtherEmulators: TStringList read FOtherEmulators;
    //< Ids of other emulators for the system.

    property GameKey: TEmutecaFileKey read FGameKey write SetGameKey;
    {< Must CRC/SHA be used as game identifiers (when importing/exporting
         data)}
    property Extensions: TStringList read FExtensions;
    {< Extensions used by the system.

    Only one extension in every string, without dot.
    }
  end;

  // After many test with generics implementing Observer...Keep it simple
  cEmutecaSystemList = TComponentList;

  TEmutecaReturnSystemCB = function(aSystem: cEmutecaSystem): boolean of
    object;

{< For CallBack functions }

function EmutecaFileKey2Str(aEFK: TEmutecaFileKey): string;
function Str2EmutecaFileKey(aString: string): TEmutecaFileKey;


implementation

function EmutecaFileKey2Str(aEFK: TEmutecaFileKey): string;
begin
  case aEFK of
    TEFKCRC32: Result := krsCRC32;
    TEFKCustom: Result := krsCustom;
    TEFKFileName: Result := krsFileName;
    else  // SHA1 by default
      Result := krsSHA1;
  end;
end;

function Str2EmutecaFileKey(aString: string): TEmutecaFileKey;
begin

  // In Emuteca <= 0.7, True => CRC32 / False => FileName
  aString := UTF8UpperCase(aString);

  // I don't like this "else if" format but it's clearer...
  if (aString = UTF8UpperCase(krsCRC32)) or
    (StrToBoolDef(aString, False)) then
    Result := TEFKCRC32
  else if (aString = UTF8UpperCase(krsFileName)) or
    (not StrToBoolDef(aString, True)) then
    Result := TEFKFileName
  else if (aString = UTF8UpperCase(krsSHA1)) then
    Result := TEFKSHA1
  else if (aString = UTF8UpperCase(krsCustom)) then
    Result := TEFKCustom;
end;

{ cEmutecaSystem }

procedure cEmutecaSystem.LoadFromFileIni(IniFile: TCustomIniFile);
begin
  if IniFile = nil then
    Exit;

  Enabled := IniFile.ReadBool(ID, krsIniKeyEnabled, Enabled);

  Title := IniFile.ReadString(ID, krsIniKeyTitle, Title);

  ExtraFile := IniFile.ReadString(ID, krsIniKeyExtraFile, ExtraFile);
  if ExtraFile = '' then ExtraFile := CleanFileName(Title);

  Extensions.CommaText := IniFile.ReadString(ID, krsIniKeyExtensions,
    Extensions.CommaText);

  BaseFolder := IniFile.ReadString(ID, krsIniKeyBaseFolder, BaseFolder);
  TempFolder := IniFile.ReadString(ID, krsIniKeyTempFolder, TempFolder);

  GameKey := Str2EmutecaFileKey(IniFile.ReadString(ID,
    krsIniKeyGamesKey, EmutecaFileKey2Str(GameKey)));
  ExtractAll := IniFile.ReadBool(ID, krsIniKeyExtractAll, ExtractAll);

  MainEmulator := IniFile.ReadString(ID, krsIniKeyMainEmulator, MainEmulator);
  OtherEmulators.CommaText :=
    IniFile.ReadString(ID, krsIniKeyOtherEmulators, OtherEmulators.CommaText);
end;

procedure cEmutecaSystem.SaveToFileIni(IniFile: TCustomIniFile;
  const ExportMode: boolean);
begin
  if IniFile = nil then
    Exit;

  IniFile.WriteString(ID, krsIniKeyTitle, Title);
  IniFile.WriteString(ID, krsIniKeyExtraFile, ExtraFile);

  IniFile.WriteString(ID, krsIniKeyExtensions, Extensions.CommaText);
  IniFile.WriteString(ID, krsIniKeyGamesKey, EmutecaFileKey2Str(GameKey));

  IniFile.WriteBool(ID, krsIniKeyExtractAll, ExtractAll);

  IniFile.WriteString(ID, krsIniKeyMainEmulator, MainEmulator);
  IniFile.WriteString(ID, krsIniKeyOtherEmulators, OtherEmulators.CommaText);

  if ExportMode then
  begin // Las borramos por si acaso existen
    IniFile.DeleteKey(ID, krsIniKeyEnabled);
    IniFile.DeleteKey(ID, krsIniKeyBaseFolder);
    IniFile.DeleteKey(ID, krsIniKeyTempFolder);
  end
  else
  begin
    IniFile.WriteBool(ID, krsIniKeyEnabled, Enabled);
    IniFile.WriteString(ID, krsIniKeyBaseFolder, BaseFolder);
    IniFile.WriteString(ID, krsIniKeyTempFolder, TempFolder);
  end;
end;

procedure cEmutecaSystem.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSystem.SetCompany(AValue: string);
begin
  if FCompany = AValue then
    Exit;
  FCompany := AValue;
end;

procedure cEmutecaSystem.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
end;

procedure cEmutecaSystem.SetExtractAll(AValue: boolean);
begin
  if FExtractAll = AValue then
    Exit;
  FExtractAll := AValue;
end;

procedure cEmutecaSystem.SetExtraFile(AValue: string);
begin
  FExtraFile := CleanFileName(AValue);
end;

procedure cEmutecaSystem.SetGameKey(AValue: TEmutecaFileKey);
begin
  if FGameKey = AValue then
    Exit;
  FGameKey := AValue;
end;

procedure cEmutecaSystem.SetID(AValue: string);
begin
  FID := SetAsID(AValue);
end;

procedure cEmutecaSystem.SetMainEmulator(AValue: string);
begin
  FMainEmulator := SetAsID(AValue);
end;

procedure cEmutecaSystem.SetModel(AValue: string);
begin
  if FModel = AValue then
    Exit;
  FModel := AValue;
end;

procedure cEmutecaSystem.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSystem.SetTitle(AValue: string);
begin
  if FTitle = AValue then Exit;
  FTitle := AValue;
end;

constructor cEmutecaSystem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Enabled := False;
  Self.GameKey := TEFKSHA1;

  Self.FExtensions := TStringList.Create;
  Self.FExtensions.CaseSensitive := False;
  self.Extensions.Sorted := True;

  Self.FOtherEmulators := TStringList.Create;
  Self.FOtherEmulators.CaseSensitive := False;
  Self.FOtherEmulators.Sorted := True;
end;

destructor cEmutecaSystem.Destroy;
begin
  FreeAndNil(Self.FExtensions);
  FreeAndNil(Self.FOtherEmulators);

  inherited Destroy;
end;

end.
