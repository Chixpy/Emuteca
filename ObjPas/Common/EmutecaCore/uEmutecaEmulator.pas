{ This file is part of Emuteca

  Copyright (C) 2006-2013 Chixpy

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

{ cGame unit. }
unit uEmutecaEmulator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IniFiles, strutils, LCLIntf,
  // Common
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca
  uEmutecaPlayingStats, uCHXStrUtils;

type

  { @name.

    @definitionList(
    @itemLabel(NOTE:)
    @item(Because PascalScript don't suport overloaded methods,
      we don't use them right here.)
    )
  }

  { cEmulator }

  cEmulator = class(cEmutecaPlayingStats)
  private
    FIcon: string;
    FImage: string;
    FExeFile: string;
    FInfoFile: string;
    FNormalExitCode: integer;
    FParameters: string;
    FWebPage: string;
    FID: string;
    FConfigFile: string;
    FName: string;
    FDeveloper: string;
    FEnabled: boolean;
    FWorkingFolder: string;
    procedure SetDeveloper(const Value: string);
    procedure SetExeFile(const Value: string);
    procedure SetConfigFile(const Value: string);
    procedure SetIcon(const AValue: string);
    procedure SetID(const Value: string);
    procedure SetImage(const AValue: string);
    procedure SetInfoFile(AValue: string);
    procedure SetName(const Value: string);
    procedure SetNormalExitCode(AValue: integer);
    procedure SetWebPage(const Value: string);
    procedure SetParameters(const Value: string);
    procedure SetEnabled(const Value: boolean);
    procedure SetWorkingFolder(const Value: string);

  public
    property Enabled: boolean read FEnabled write SetEnabled;

    property ID: string read FID write SetID;
    property Name: string read FName write SetName;
    property Developer: string read FDeveloper write SetDeveloper;
    property WebPage: string read FWebPage write SetWebPage;

    property Image: string read FImage write SetImage;
    property Icon: string read FIcon write SetIcon;

    property ExeFile: string read FExeFile write SetExeFile;
    property WorkingFolder: string
      read FWorkingFolder write SetWorkingFolder;
    property Parameters: string read FParameters write SetParameters;
    property InfoFile: string read FInfoFile write SetInfoFile;
    property ConfigFile: string read FConfigFile write SetConfigFile;

    property NormalExitCode: integer
      read FNormalExitCode write SetNormalExitCode;

    function Execute(GameFile: string): integer;
    function ExecuteAlone: integer;

    procedure LoadFromFile(const IniFile: string);
    procedure LoadFromFileIni(aIniFile: TMemIniFile);
    procedure SaveToFile(const aIniFile: string;
      const ExportMode: boolean = False);
    procedure SaveToFileIni(IniFile: TMemIniFile;
      const ExportMode: boolean = False);

    constructor Create(const EmulatorID: string);

  end;

implementation

{ cEmulator }

constructor cEmulator.Create(const EmulatorID: string);
begin
  inherited Create;
  self.ID := EmulatorID;

  // By default
  self.Name := self.ID;
  self.WorkingFolder := kEmutecaEmuDirKey;
  self.Parameters := '"' + kEmutecaROMPathKey + '"';
end;

function cEmulator.Execute(GameFile: string): integer;
var
  CurrFolder: string;
  TempDir: string;
  TempParam: string;
  TempTime: TTime;
begin
  CurrFolder := GetCurrentDirUTF8;

  {$IFDEF MSWindows}
  // Some emulators don't work with Unix style...
  GameFile := StringReplace(GameFile, '/', '\',
    [rfReplaceAll, rfIgnoreCase]);
  {$ENDIF}

  // GameFile is relative to emuteca directory.
  // Absolute path is better right now.
  if not FilenameIsAbsolute(GameFile) then
    GameFile := CreateAbsolutePath(GameFile, CurrFolder);

  // Changing current directory
  TempDir := Self.WorkingFolder;
  TempDir := AnsiReplaceText(TempDir, kEmutecaEmuDirKey,
    ExtractFileDir(ExeFile));
  TempDir := AnsiReplaceText(TempDir, kEmutecaRomDirKey,
    ExtractFileDir(GameFile));
  TempDir := AnsiReplaceText(TempDir, kEmutecaCurrentDirKey,
    ExtractFileDir(CurrFolder));
  TempDir := SetAsFolder(TempDir);
  if TempDir <> '' then
    if DirectoryExistsUTF8(TempDir) then
      ChDir(TempDir);

  // Changing parameters
  TempParam := Self.Parameters;
  TempParam := AnsiReplaceText(TempParam, kEmutecaROMPathKey, GameFile);
  TempParam := AnsiReplaceText(TempParam, kEmutecaRomDirKey,
    ExtractFileDir(GameFile));
  TempParam := AnsiReplaceText(TempParam, kEmutecaROMFileNameKey,
    ExtractFileName(GameFile));
  TempParam := AnsiReplaceText(TempParam, kEmutecaROMFileNameNoExtKey,
    ExtractFileNameWithoutExt(GameFile));
  TempParam := AnsiReplaceText(TempParam, kEmutecaROMFileExtKey,
    ExtractFileExt(GameFile));
  TempParam := AnsiReplaceText(TempParam, kEmutecaNullKey, '');
  TempParam := Trim(TempParam);

  try
    TempTime := Now;

    // Hack for run system executables ;P
    if ExeFile = '' then
      { TODO -oAuthor : If not an executable try OpenDocument }
      Result := SysUtils.ExecuteProcess(UTF8ToSys(TempParam), '')
    else
      Result := SysUtils.ExecuteProcess(UTF8ToSys(ExeFile),
        UTF8ToSys(TempParam));

    // Is there an error? No, then add statistics
    // TODO 2: Emulator statistics are not saved in the file...
    if (Result = Self.NormalExitCode) and
      (Now > (EncodeTime(0, 1, 0, 0) + TempTime)) then
    begin
      Self.AddPlayingTime(Now, TempTime);
      Self.LastTime := TempTime;
      Self.TimesPlayed := Self.TimesPlayed + 1;
    end;

    // Hack: If normal exit code <> 0, switch 0 and NormalExitCode
    //   So, this way 0 always is the correct exit of the program,
    //     and Managers don't care about wich is the actual code
    if (Self.NormalExitCode <> 0) then
      if Result = 0 then
        Result := Self.NormalExitCode
      else
        Result := 0;

  finally
    ChDir(CurrFolder);
  end;
end;

function cEmulator.ExecuteAlone: integer;
var
  CurrFolder: string;
  TempDir: string;
begin
  if ExeFile = '' then
    Exit;

  // Changing current file
  CurrFolder := GetCurrentDirUTF8;
  TempDir := Self.WorkingFolder;
  TempDir := AnsiReplaceText(TempDir, kEmutecaEmuDirKey,
    ExtractFileDir(ExeFile));
  TempDir := AnsiReplaceText(TempDir, kEmutecaRomDirKey,
    ExtractFileDir(ExeFile));
  TempDir := AnsiReplaceText(TempDir, kEmutecaCurrentDirKey,
    ExtractFileDir(CurrFolder));
  if TempDir <> '' then
    ChDir(TempDir);

  try
    Result := SysUtils.ExecuteProcess(UTF8ToSys(ExeFile), '');

    if (Self.NormalExitCode <> 0) then
      if Result = 0 then
        Result := Self.NormalExitCode
      else
        Result := 0;

  finally
    ChDir(CurrFolder);
  end;
end;

procedure cEmulator.SaveToFileIni(IniFile: TMemIniFile;
  const ExportMode: boolean = False);
begin
  if IniFile = nil then
    Exit;
  IniFile.WriteString(self.ID, 'Name', Name);
  IniFile.WriteString(self.ID, 'Developer', Developer);
  IniFile.WriteString(self.ID, 'Webpage', WebPage);

  IniFile.WriteString(self.ID, 'WorkingFolder', WorkingFolder);
  IniFile.WriteString(self.ID, 'Parameters', Parameters);
  IniFile.WriteInteger(self.ID, 'NormalExitCode', NormalExitCode);

  if ExportMode then
  begin
    IniFile.DeleteKey(self.ID, 'ExeFile');
    IniFile.DeleteKey(self.ID, 'Icon');
    IniFile.DeleteKey(self.ID, 'Image');

    IniFile.DeleteKey(self.ID, 'ConfigFile');
    IniFile.DeleteKey(self.ID, 'InfoFile');

    IniFile.DeleteKey(self.ID, 'Enabled');

    // TODO 2: Esto lo debería hacer uGameStats.
    IniFile.DeleteKey(self.ID, 'PlayingTime');
    IniFile.DeleteKey(self.ID, 'TimesPlayed');
    IniFile.DeleteKey(self.ID, 'LastTime');
  end
  else
  begin
    IniFile.WriteString(self.ID, 'ExeFile', ExeFile);
    IniFile.WriteString(self.ID, 'Icon', Icon);
    IniFile.WriteString(self.ID, 'Image', Image);

    IniFile.WriteString(self.ID, 'ConfigFile', ConfigFile);
    IniFile.WriteString(self.ID, 'InfoFile', InfoFile);

    IniFile.WriteBool(self.ID, 'Enabled', Enabled);

    // TODO 2: Esto lo debería hacer uGameStats.
    IniFile.WriteString(Self.ID, 'PlayingTime', IntToStr(Self.PlayingTime));
    IniFile.WriteString(Self.ID, 'TimesPlayed', IntToStr(Self.TimesPlayed));
    IniFile.WriteDateTime(Self.ID, 'LastTime', Self.LastTime);
  end;

end;

procedure cEmulator.SaveToFile(const aIniFile: string;
  const ExportMode: boolean = False);
var
  F: TMemInifile;
begin
  F := TMemIniFile.Create(UTF8ToSys(aIniFile));
  try
    Self.SaveToFileIni(F, ExportMode);
    F.UpdateFile;
  finally
    FreeAndNil(F);
  end;
end;

procedure cEmulator.LoadFromFileIni(aIniFile: TMemIniFile);
var
  TmpString: string;
begin
  if aIniFile = nil then
    Exit;

  Enabled := aIniFile.ReadBool(self.ID, 'Enabled', Enabled);

  Name := aIniFile.ReadString(self.ID, 'Name', Name);

  Developer := aIniFile.ReadString(self.ID, 'Developer', Developer);
  WebPage := aIniFile.ReadString(self.ID, 'Webpage', WebPage);

  Icon := aIniFile.ReadString(self.ID, 'Icon', Icon);
  Image := aIniFile.ReadString(self.ID, 'Image', Image);

  ExeFile := aIniFile.ReadString(self.ID, 'ExeFile', ExeFile);
  WorkingFolder := aIniFile.ReadString(self.ID, 'WorkingFolder',
    WorkingFolder);
  NormalExitCode := aIniFile.ReadInteger(self.ID, 'NormalExitCode',
    NormalExitCode);

  Parameters := aIniFile.ReadString(self.ID, 'Parameters', Parameters);
  ConfigFile := aIniFile.ReadString(self.ID, 'ConfigFile', ConfigFile);
  InfoFile := aIniFile.ReadString(self.ID, 'InfoFile', InfoFile);

  // TODO 2: Esto lo debería hacer uGameStats.
  TmpString := aIniFile.ReadString(Self.ID, 'LastTime', '');
  if TmpString <> '' then
    LastTime := StrToDateTimeDef(TmpString, LastTime);
  TmpString := aIniFile.ReadString(Self.ID, 'PlayingTime', '');
  PlayingTime := StrToCardinalDef(TmpString, PlayingTime);
  TmpString := aIniFile.ReadString(Self.ID, 'TimesPlayed', '');
  TimesPlayed := StrToCardinalDef(TmpString, TimesPlayed);
end;

procedure cEmulator.LoadFromFile(const IniFile: string);
var
  F: TMemInifile;
begin
  F := TMemIniFile.Create(UTF8ToSys(IniFile));
  try
    self.LoadFromFileIni(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure cEmulator.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure cEmulator.SetDeveloper(const Value: string);
begin
  FDeveloper := Value;
end;

procedure cEmulator.SetWorkingFolder(const Value: string);
begin
  FWorkingFolder := SetAsFolder(Value);
end;

procedure cEmulator.SetExeFile(const Value: string);
begin
  FExeFile := Value;
end;

procedure cEmulator.SetConfigFile(const Value: string);
begin
  FConfigFile := Value;
end;

procedure cEmulator.SetIcon(const AValue: string);
begin
  FIcon := AValue;
end;

procedure cEmulator.SetID(const Value: string);
begin
  FID := Trim(Value);
end;

procedure cEmulator.SetImage(const AValue: string);
begin
  FImage := AValue;
end;

procedure cEmulator.SetInfoFile(AValue: string);
begin
  FInfoFile := AValue;
end;

procedure cEmulator.SetName(const Value: string);
begin
  FName := Value;
end;

procedure cEmulator.SetNormalExitCode(AValue: integer);
begin
  FNormalExitCode := AValue;
end;

procedure cEmulator.SetWebPage(const Value: string);
begin
  FWebPage := Value;
end;

procedure cEmulator.SetParameters(const Value: string);
begin
  FParameters := Value;
end;

end.
