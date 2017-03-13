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

{ cEmutecaEmulator unit. }
unit ucEmutecaEmulator;

{$mode objfpc}{$H+}

interface

uses  Classes, SysUtils, FileUtil, StrUtils, LazUTF8, LazFileUtils, contnrs,
  IniFiles, fgl,
  // CHX units
  uCHXStrUtils,
  // Emuteca units
  uaCHXStorable,
  ucEmutecaPlayingStats;

const
  // Ini file Keys
  // -------------
  krsEmulatorEnabledKey = 'Enabled';  // TODO: uEmutecaCommon.pas
  krsEmulatorNameKey = 'Name';
  krsEmulatorWorkingFoldeKey = 'WorkingFolder';
  krsEmulatorParameters = 'Parameters';
  krsEmulatorExitCodeKey = 'ExitCode';
  krsEmulatorExeFileKey = 'ExeFile';
  krsEmulatorFileExtKey = 'Extensions';

  // Keys for command line parameters for emulators
  // ----------------------------------------------
  // Working folders
  kEmutecaEmuDirKey = '%EMUDIR%';
  {< Emulator's directory key. }
  kEmutecaRomDirKey = '%ROMDIR%';
  {< ROM's directory key. }
  kEmutecaCurrentDirKey = '%CURRENTDIR%';
  {< Current directory key. }

  // Parameters
  kEmutecaROMPathKey = '%ROM%';
  {< ROM full path. }
  kEmutecaROMFileNameKey = '%ROMNAME%';
  {< ROM filename. }
  kEmutecaROMFileNameNoExtKey = '%ROMNAMENOEXT%';
  {< ROM filename without extension. }
  kEmutecaROMFileExtKey = '%ROMEXT%';
{< ROM file extension. }

type
  { cEmutecaEmulator class.

    Stores all basic info of an emulator. }
  cEmutecaEmulator = class(caCHXStorable)
  private
    FEmulatorName: string;
    FEnabled: boolean;
    FExeFile: string;
    FExitCode: integer;
    FFileExt: TStringList;
    FID: string;
    FParameters: string;
    FStats: cEmutecaPlayingStats;
    FWorkingFolder: string;
    procedure SetEmulatorName(AValue: string);
    procedure SetEnabled(AValue: boolean);
    procedure SetExeFile(AValue: string);
    procedure SetExitCode(AValue: integer);
    procedure SetFileExt(AValue: TStringList);
    procedure SetID(AValue: string);
    procedure SetParameters(AValue: string);
    procedure SetWorkingFolder(AValue: string);

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(GameFile: string): integer;
    function ExecuteAlone: integer;

    procedure LoadFromIni(IniFile: TCustomIniFile); override;
    procedure SaveToIni(IniFile: TCustomIniFile;
      const ExportMode: boolean); override;

  published
    property ID: string read FID write SetID;
    {< ID of the Emulator. }

    property Enabled: boolean read FEnabled write SetEnabled;
    {< Is it enabled? }

    property EmulatorName: string read FEmulatorName write SetEmulatorName;
    {< Emulator's name. }
    property ExeFile: string read FExeFile write SetExeFile;
    {< Path to executable. }
    property WorkingFolder: string read FWorkingFolder write SetWorkingFolder;
    {< Working folder. Emuteca will change to that folder before launching the
         emulator. Following key items can be used:

       @definitionList(
         @itemLabel(%EMUDIR% (default))
         @item(Change to emulator's folder.)
         @itemLabel(%ROMDIR%)
         @item(Change to ROM's folder.)
         @itemLabel(%CURRENTDIR%)
         @item(Use current folder.)
       )
    }
    property Parameters: string read FParameters write SetParameters;
    {< Parameters used with the executable.

       Tip: Following key items can be used:

       @definitionList(
         @itemLabel(%ROM%)
         @item(Full ROM path.)
         @itemLabel(%ROMNAME%)
         @item(ROM filename with extension, but without folder.)
         @itemLabel(%ROMNAMENOEXT%)
         @item(Only ROM filename, without folder or extension.
           Usefull for MAME.)
         @itemLabel(%ROMEXT%)
         @item(Only ROM extension.)
       )
    }
    property FileExt: TStringList read FFileExt write SetFileExt;
    {< Extensions used by the emulator.

    Only one extension in every string, without dot.
    }

    property ExitCode: integer read FExitCode write SetExitCode;
    {< Code returned by emulator in usual conditions. Emuteca will not show
         an error message if this code is returned. }

             // Usage statitics
    // ---------------
    property Stats: cEmutecaPlayingStats read FStats;
  end;

  cEmutecaGenEmulatorList = specialize TFPGObjectList<cEmutecaEmulator>;
  cEmutecaEmulatorList = class (cEmutecaGenEmulatorList);

  TEmutecaReturnEmulatorCB = function(aEmulator: cEmutecaEmulator): boolean of
    object;

implementation

{ cEmutecaEmulator }

procedure cEmutecaEmulator.SetID(AValue: string);
begin
  if FID = AValue then
    Exit;
  FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure cEmutecaEmulator.SetParameters(AValue: string);
begin
  if FParameters = AValue then
    Exit;
  FParameters := AValue;
end;

procedure cEmutecaEmulator.SetWorkingFolder(AValue: string);
begin
  //if FWorkingFolder = AValue then
  //  Exit;
  FWorkingFolder := SetAsFolder(AValue);
end;

procedure cEmutecaEmulator.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
end;

procedure cEmutecaEmulator.SetEmulatorName(AValue: string);
begin
  if FEmulatorName = AValue then
    Exit;
  FEmulatorName := AValue;
end;

procedure cEmutecaEmulator.SetExeFile(AValue: string);
begin
  FExeFile := SetAsFile(AValue);
end;

procedure cEmutecaEmulator.SetExitCode(AValue: integer);
begin
  if FExitCode = AValue then
    Exit;
  FExitCode := AValue;
end;

procedure cEmutecaEmulator.SetFileExt(AValue: TStringList);
begin
  if FFileExt = AValue then
    Exit;
  FFileExt := AValue;
end;

constructor cEmutecaEmulator.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

   FStats := cEmutecaPlayingStats.Create(Self);

  self.WorkingFolder := kEmutecaEmuDirKey;
  self.Parameters := '"' + kEmutecaROMPathKey + '"';

  FFileExt := TStringList.Create;
end;


destructor cEmutecaEmulator.Destroy;
begin
  FreeAndNil(FFileExt);
  FreeAndNil(FStats);

  inherited Destroy;
end;

function cEmutecaEmulator.Execute(GameFile: string): integer;
var
  CurrFolder: string;
  TempDir: string;
  TempParam: string;
begin
  CurrFolder := GetCurrentDirUTF8;

  GameFile := SysPath(GameFile);

  // If GameFile is relative to emuteca directory,
  //   absolute path is better right now.
  if not FilenameIsAbsolute(GameFile) then
    GameFile := CreateAbsoluteSearchPath(GameFile, CurrFolder);

  // Changing current directory
  TempDir := SysPath(Self.WorkingFolder);
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
  TempParam := Trim(TempParam);

  try
    // Hack for run system executables ;P
    if ExeFile = '' then
      { TODO : If not an executable try OpenDocument }
      Result := ExecuteProcess(UTF8ToSys(TempParam), '')
    else
      Result := ExecuteProcess(UTF8ToSys(SysPath(ExeFile)),
        UTF8ToSys(TempParam));

    // Hack: If normal exit code <> 0, switch 0 and ExitCode
    //   So, this way 0 always is the correct exit of the program,
    //     and Managers don't care about wich is the actual code
    if (Self.ExitCode <> 0) then
      if Result = 0 then
        Result := Self.ExitCode
      else
        Result := 0;

  finally
    ChDir(CurrFolder);
  end;
end;

function cEmutecaEmulator.ExecuteAlone: integer;
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

    if (Self.ExitCode <> 0) then
      if Result = 0 then
        Result := Self.ExitCode
      else
        Result := 0;

  finally
    ChDir(CurrFolder);
  end;
end;

procedure cEmutecaEmulator.LoadFromIni(IniFile: TCustomIniFile);
begin
  if not assigned(IniFile) then
    Exit;

  Enabled := IniFile.ReadBool(self.ID, krsEmulatorEnabledKey, Enabled);

  EmulatorName := IniFile.ReadString(self.ID, krsEmulatorNameKey,
    EmulatorName);

  ExeFile := IniFile.ReadString(self.ID, krsEmulatorExeFileKey, ExeFile);
  WorkingFolder := IniFile.ReadString(self.ID, krsEmulatorWorkingFoldeKey,
    WorkingFolder);
  Parameters := IniFile.ReadString(self.ID, krsEmulatorParameters,
    Parameters);
  FileExt.CommaText := IniFile.ReadString(self.ID,
    krsEmulatorFileExtKey, FileExt.CommaText);
  ExitCode := IniFile.ReadInteger(self.ID, krsEmulatorExitCodeKey, ExitCode);
end;

procedure cEmutecaEmulator.SaveToIni(IniFile: TCustomIniFile;
  const ExportMode: boolean);
begin
  if not assigned(IniFile) then
    Exit;
  IniFile.WriteString(self.ID, krsEmulatorNameKey, EmulatorName);

  IniFile.WriteString(self.ID, krsEmulatorWorkingFoldeKey, WorkingFolder);
  IniFile.WriteString(self.ID, krsEmulatorParameters, Parameters);
  IniFile.WriteInteger(self.ID, krsEmulatorExitCodeKey, ExitCode);

  if ExportMode then
  begin
    IniFile.DeleteKey(self.ID, krsEmulatorExeFileKey);
    IniFile.DeleteKey(self.ID, krsEmulatorEnabledKey);
  end
  else
  begin
    IniFile.WriteString(self.ID, krsEmulatorExeFileKey, ExeFile);
    IniFile.WriteBool(self.ID, krsEmulatorEnabledKey, Enabled);
  end;
end;

end.
