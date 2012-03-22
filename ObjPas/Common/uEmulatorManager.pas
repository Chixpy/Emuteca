{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

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

{cEmulatorManager unit}
unit uEmulatorManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, IniFiles, FileUtil, LazUTF8,
  uEmulator;

type

  { cEmulatorManager }

  cEmulatorManager = class
  private
    FConfigFile: String;
    FEmulators: TFPObjectList;
    procedure SetConfigFile(const Value: String);

  protected
    property Emulators: TFPObjectList read FEmulators;

  public
    property ConfigFile: String read FConfigFile write SetConfigFile;

    function Emulator(const Index: longint): cEmulator; overload;
    function Emulator(const EmulatorID: String): cEmulator; overload;
    function Count: longint;

    procedure LoadEmulatorsFile; overload;
    procedure SaveEmulatorsFile(const ExportMode: boolean = False); overload;

    procedure ImportEmulatorsFile(const aFileName: String); overload;
    procedure ExportEmulatorsFile(const aFileName: String;
      const ExportMode: boolean = False); overload;

    function IndexOf(aEmulator: cEmulator): integer; overload;
    function IndexOf(EmulatorID: String): integer; overload;
    function AddEmulator(aEmulator: cEmulator): cEmulator; overload;
    function AddEmulator(const EmulatorID: String): cEmulator; overload;
    procedure RemoveEmulator(const Index: longint); overload;
    procedure RemoveEmulator(const EmulatorID: String); overload;

    procedure ListEmulators(aList: TStrings);
    procedure ListEnabledEmulators(aList: TStrings);

    function RunEmulator(const EmulatorID, GameFile: String): longword;

    constructor Create(const aEmulatorsFile: String = '');
    destructor Destroy; override;
  end;


implementation

{ cEmulatorManager }

function cEmulatorManager.AddEmulator(aEmulator: cEmulator): cEmulator;
begin
  Result := aEmulator;
  if aEmulator = nil then
    Exit;
  RemoveEmulator(aEmulator.ID);
  Emulators.Add(aEmulator);
end;

function cEmulatorManager.AddEmulator(const EmulatorID: String): cEmulator;
var
  aEmulator: cEmulator;
begin
  Result := nil;
  aEmulator := cEmulator.Create(EmulatorID);
  if aEmulator.ID = '' then
    FreeAndNil(aEmulator);
  if aEmulator <> nil then
    Result := AddEmulator(aEmulator);
end;

constructor cEmulatorManager.Create(const aEmulatorsFile: String);
begin
  inherited Create;
  FEmulators := TFPObjectList.Create(True);
  ConfigFile := aEmulatorsFile;
  LoadEmulatorsFile;
end;

destructor cEmulatorManager.Destroy;
begin
  FreeAndNil(FEmulators);
  inherited Destroy;
end;

function cEmulatorManager.RunEmulator(
  const EmulatorID, GameFile: String): longword;
var
  Emu: cEmulator;
begin
  Result := 256;
  Emu := Emulator(EmulatorID);
  if Emu = nil then
    Exit;
  Result := Emu.Execute(GameFile);

  if Result = 0 then
    Emu.SaveToFile(ConfigFile, False);
  //Guardamos los datos del Emulator
end;

function cEmulatorManager.Emulator(const EmulatorID: String): cEmulator;
begin
  Result := Emulator(Self.IndexOf(EmulatorID));
end;

function cEmulatorManager.Emulator(const Index: integer): cEmulator;
begin
  Result := nil;
  if (Index < Self.Count) and (Index >= 0) then
    Result := cEmulator(Emulators[Index]);
end;

procedure cEmulatorManager.SaveEmulatorsFile(const ExportMode: boolean);
var
  IniFile: TMemIniFile;
  i: longint;
begin
  if not ExportMode then
    if FileExistsUTF8(ConfigFile) then
      DeleteFileUTF8(ConfigFile);

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  IniFile.CaseSensitive := False;
  try
    i := 0;
    while i < Emulators.Count do
    begin
      Emulator(i).SaveToFileIni(IniFile, ExportMode);
      Inc(i);
    end;
  finally
    IniFile.UpdateFile;
    FreeAndNil(IniFile);
  end;
end;

procedure cEmulatorManager.ExportEmulatorsFile(const aFileName: String;
  const ExportMode: boolean);
var
  OldCF: String;
begin
  OldCF := ConfigFile;
  ConfigFile := aFileName;
  SaveEmulatorsFile(ExportMode);
  ConfigFile := OldCF;
end;

function cEmulatorManager.IndexOf(aEmulator: cEmulator): integer;
begin
  Result := -1;
  if aEmulator = nil then
    Exit;
  Result := IndexOf(aEmulator.ID);
end;

function cEmulatorManager.IndexOf(EmulatorID: String): integer;
var
  Cont: integer;
begin
  Result := -1;
  EmulatorID := UTF8LowerCase(Trim(EmulatorID));

  Cont := Self.Count - 1;
  while (Cont >= 0) and (Result = -1) do
  begin
    if UTF8LowerCase(Emulator(Cont).ID) = EmulatorID then
      Result := Cont;
    Dec(Cont);
  end;
end;

procedure cEmulatorManager.LoadEmulatorsFile;
var
  IniFile: TMemIniFile;
  TempString: TStringList;
  TempEmulator: cEmulator;
  i: longint;
begin
  Emulators.Clear;
  if not FileExistsUTF8(ConfigFile) then
    Exit;

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  IniFile.CaseSensitive := False;
  TempString := TStringList.Create;
  TempString.BeginUpdate;
  try
    IniFile.ReadSections(TempString);
    TempString.Sort;

    i := 0;
    while i < TempString.Count do
    begin
      TempEmulator := Emulator(TempString[i]);
      if TempEmulator = nil then
      begin
        TempEmulator := cEmulator.Create(TempString[i]);
        Emulators.Add(TempEmulator);
      end;
      TempEmulator.LoadFromFileIni(IniFile);
      Inc(i);
    end;
  finally
    FreeAndNil(IniFile);
    TempString.EndUpdate;
    FreeAndNil(TempString);
  end;
end;

procedure cEmulatorManager.ImportEmulatorsFile(const aFileName: String);
var
  OldCF: String;
begin
  OldCF := ConfigFile;
  ConfigFile := aFileName;
  LoadEmulatorsFile;
  ConfigFile := OldCF;
end;

procedure cEmulatorManager.ListEnabledEmulators(aList: TStrings);
var
  TempString: TStringList;
  i: longint;
begin
  if aList = nil then
    aList := TStringList.Create;

  aList.Clear;
  TempString := TStringList.Create;
  try
    i := 0;
    while i < Count do
    begin
      if Emulator(i).Enabled then
        TempString.Add(Emulator(i).ID);
      Inc(i);
    end;
    aList.AddStrings(TempString);
  finally
    FreeAndNil(TempString);
  end;
end;

procedure cEmulatorManager.ListEmulators(aList: TStrings);
var
  TempString: TStringList;
  i: longint;
begin
  if aList = nil then
    aList := TStringList.Create;

  aList.Clear;
  TempString := TStringList.Create;
  try
    i := 0;
    while i < Count do
    begin
      TempString.Add(Emulator(i).ID);
      Inc(i);
    end;
    aList.AddStrings(TempString);
  finally
    FreeAndNil(TempString);
  end;
end;

function cEmulatorManager.Count: longint;
begin
  Result := Emulators.Count;
end;

procedure cEmulatorManager.RemoveEmulator(const Index: longint);
begin
  if (Index < Self.Count) and (Index >= 0) then
    Emulators.Delete(Index);
end;

procedure cEmulatorManager.RemoveEmulator(const EmulatorID: String);
begin
  RemoveEmulator(Self.IndexOf(EmulatorID));
end;

procedure cEmulatorManager.SetConfigFile(const Value: String);
begin
  FConfigFile := Value;
end;

end.

