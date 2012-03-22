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

{cSystemManager unit}
unit uSystemManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uSystem, contnrs, IniFiles, FileUtil, LazUTF8;

type

  { cSystemManager }

  cSystemManager = class
  private
    FConfigFile: String;
    FSystems: TFPObjectList;
    procedure SetConfigFile(const AValue: String);

  protected
    property Systems: TFPObjectList read FSystems;

  public
    property ConfigFile: String read FConfigFile write SetConfigFile;

    function System(const Index: longint): cSystem; overload;
    function System(const SystemID: String): cSystem; overload;
    function Count: longint;

    procedure LoadSystemsFile;
    procedure SaveSystemsFile(const ExportMode: boolean = False);

    procedure ImportSystemsFile(const aFilename: String);
    procedure ExportSystemsFile(const aFilename: String;
      const ExportMode: boolean = True);

    function IndexOf(aSystem: cSystem): integer;
    function IndexOf(SystemID: String): integer;
    function AddSystem(aSystem: cSystem): cSystem; overload;
    function AddSystem(const SystemID: String): cSystem; overload;
    procedure RemoveSystem(const Index: longint); overload;
    procedure RemoveSystem(const SystemID: String); overload;

    procedure ListSystems(aList: TStrings);
    procedure ListEnabledSystems(aList: TStrings);

    constructor Create(const aSystemsFile: String = '');
    destructor Destroy; override;
  end;

implementation

{ cEmulatorManager }

procedure cSystemManager.SetConfigFile(const AValue: String);
begin
  if FConfigFile = AValue then
    exit;
  FConfigFile := AValue;
end;

function cSystemManager.System(const Index: longint): cSystem;
begin
  Result := nil;
  if (Index < Count) and (Index >= 0) then
    Result := cSystem(Systems[Index]);
end;

function cSystemManager.System(const SystemID: String): cSystem;
begin
  Result := System(Self.IndexOf(SystemID));
end;

function cSystemManager.Count: longint;
begin
  Result := Systems.Count;
end;

procedure cSystemManager.LoadSystemsFile;
var
  IniFile: TMemIniFile;
  TempList: TStringList;
  TempSys: cSystem;
  i: longint;
begin
  Systems.Clear;
  if not FileExistsUTF8(ConfigFile) then
    Exit;

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  IniFile.CaseSensitive := False;
  TempList := TStringList.Create;
  TempList.BeginUpdate;
  try
    IniFile.ReadSections(TempList);
    TempList.Sort;

    i := 0;
    while i < TempList.Count do
    begin
      TempSys := System(TempList[i]);
      if TempSys = nil then
      begin
        TempSys := cSystem.Create(TempList[i]);
        Systems.Add(TempSys);
      end;
      TempSys.LoadFromFileIni(IniFile);
      Inc(i);
    end;
  finally
    FreeAndNil(IniFile);
    TempList.EndUpdate;
    FreeAndNil(TempList);
  end;
end;

procedure cSystemManager.SaveSystemsFile(const ExportMode: boolean);
var
  IniFile: TMemIniFile;
  Cont: longint;
begin
  if not ExportMode then
    if FileExistsUTF8(ConfigFile) then
      DeleteFileUTF8(ConfigFile);

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  IniFile.CaseSensitive := False;
  try
    Cont := 0;
    while Cont < Systems.Count do
    begin
      System(Cont).SaveToFileIni(IniFile, ExportMode);
      Inc(Cont);
    end;
  finally
    IniFile.UpdateFile;
    FreeAndNil(IniFile);
  end;
end;

procedure cSystemManager.ImportSystemsFile(const aFilename: String);
var
  OldCF: String;
begin
  OldCF := ConfigFile;
  ConfigFile := aFilename;
  LoadSystemsFile;
  ConfigFile := OldCF;
end;

procedure cSystemManager.ExportSystemsFile(const aFilename: String;
  const ExportMode: boolean);
var
  OldCF: string;
begin
  OldCF := ConfigFile;
  ConfigFile := aFilename;
  SaveSystemsFile(ExportMode);
  ConfigFile := OldCF;
end;

function cSystemManager.IndexOf(aSystem: cSystem): integer;
begin
  Result := -1;
  if aSystem = nil then
    Exit;
  Result := IndexOf(aSystem.ID);
end;

function cSystemManager.IndexOf(SystemID: String): integer;
var
  Cont: integer;
begin
  Result := -1;
  SystemID := UTF8LowerCase(Trim(SystemID));

  Cont := Self.Count - 1;
  while (Cont >= 0) and (Result = -1) do
  begin
    if UTF8LowerCase(System(Cont).ID) = SystemID then
      Result := Cont;
    Dec(Cont);
  end;
end;

function cSystemManager.AddSystem(aSystem: cSystem): cSystem;
begin
  Result := aSystem;
  if aSystem = nil then
    Exit;
  RemoveSystem(aSystem.ID);
  Systems.Add(aSystem);
end;

function cSystemManager.AddSystem(const SystemID: String): cSystem;
var
  aSystem: cSystem;
begin
  Result := nil;
  aSystem := cSystem.Create(SystemID);
  if aSystem.ID = '' then
    FreeAndNil(aSystem);
  if aSystem <> nil then
    Result := AddSystem(aSystem);
end;

procedure cSystemManager.RemoveSystem(const Index: longint);
begin
  if (Index < Self.Count) and (Index >= 0) then
    Systems.Delete(Index);
end;

procedure cSystemManager.RemoveSystem(const SystemID: String);
begin
  RemoveSystem(Self.IndexOf(SystemID));
end;

procedure cSystemManager.ListSystems(aList: TStrings);
var
  TempList: TStringList;
  i: longint;
begin
  if aList = nil then
    aList := TStringList.Create;

  aList.Clear;
  TempList := TStringList.Create;
  try
    i := 0;
    while i < Count do
    begin
      TempList.Add(System(i).ID);
      Inc(i);
    end;
    aList.AddStrings(TempList);
  finally
    FreeAndNil(TempList);
  end;
end;

procedure cSystemManager.ListEnabledSystems(aList: TStrings);
var
  TempList: TStringList;
  i: longint;
begin
  if aList = nil then
    aList := TStringList.Create;

  aList.Clear;
  TempList := TStringList.Create;
  try
    i := 0;
    while i < Count do
    begin
      if System(i).Enabled then
        TempList.Add(System(i).ID);
      Inc(i);
    end;
    aList.AddStrings(TempList);
  finally
    FreeAndNil(TempList);
  end;
end;

constructor cSystemManager.Create(const aSystemsFile: String);
begin
  inherited Create;
  ConfigFile := aSystemsFile;
  FSystems := TFPObjectList.Create(True);
  LoadSystemsFile;
end;

destructor cSystemManager.Destroy;
begin
  FreeAndNil(FSystems);
  inherited Destroy;
end;

end.

