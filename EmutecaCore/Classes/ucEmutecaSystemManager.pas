{ This file is part of Emuteca

  Copyright (C) 2006-2017 Chixpy

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

{ cEmutecaSystemManager unit. }
unit ucEmutecaSystemManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, IniFiles,
  uCHXStrUtils,
  uaCHXStorable,
  uEmutecaCommon,
  ucEmutecaSystemList;

type
  { cEmutecaSystemManager }

  cEmutecaSystemManager = class(caCHXStorableIni)
  private
    FFullList: cEmutecaSystemList;
    FEnabledList: cEmutecaSystemList;
    FProgressCallBack: TEmutecaProgressCallBack;
    FSysDataFolder: string;
    procedure SetSysDataFolder(AValue: string);

  protected
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  public
    property SysDataFolder: string read FSysDataFolder write SetSysDataFolder;

    procedure ClearData;
    //< Clears all data WITHOUT saving.
    procedure LoadData;
    //< Reload last data file WITHOUT saving changes.
    procedure SaveData;
    //< Save data to last data file.

    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure SaveToIni(aIniFile: TMemIniFile;
      const ExportMode: boolean); override;

    procedure AssingAllTo(aList: TStrings); deprecated;
    procedure AssingEnabledTo(aList: TStrings); deprecated;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;


  published
    property FullList: cEmutecaSystemList read FFullList;
    property EnabledList: cEmutecaSystemList read FEnabledList;

  end;

implementation
uses uaEmutecaCustomSystem, ucEmutecaSystem;

{ cEmutecaSystemManager }

procedure cEmutecaSystemManager.SetSysDataFolder(AValue: string);
begin
  FSysDataFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSystemManager.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then Exit;
  FProgressCallBack := AValue;
end;

procedure cEmutecaSystemManager.ClearData;
begin
  EnabledList.Clear;
  FullList.Clear;
end;

procedure cEmutecaSystemManager.LoadData;
begin
  ClearData;
  LoadFromFileIni('');
end;

procedure cEmutecaSystemManager.SaveData;
begin
  SaveToFileIni('', False);
end;

procedure cEmutecaSystemManager.LoadFromIni(aIniFile: TMemIniFile);
var
  TempList: TStringList;
  TempSys: cEmutecaSystem;
  i: longint;
begin
  if not Assigned(aIniFile) then
    Exit;

  TempList := TStringList.Create;
  try
    aIniFile.ReadSections(TempList);
    TempList.Sort;

    i := 0;
    while i < TempList.Count do
    begin
      TempSys := cEmutecaSystem.Create(nil);
      TempSys.ID := TempList[i];
      TempSys.IniFileName := IniFileName;
      TempSys.LoadFromIni(aIniFile);
      FullList.Add(TempSys);
      if TempSys.Enabled then
        EnabledList.Add(TempSys);
      Inc(i);

      if ProgressCallBack <> nil then
        ProgressCallBack(rsLoadingSystemList, TempSys.ID,
          TempSys.Title, i, TempList.Count);

      if TempSys.Enabled then
        TempSys.LoadLists(SysDataFolder + TempSys.FileName);

    end;
  finally
    FreeAndNil(TempList);
  end;
end;

procedure cEmutecaSystemManager.SaveToIni(aIniFile: TMemIniFile;
  const ExportMode: boolean);
var
  i: longint;
  aSystem: cEmutecaSystem;
begin
  if not Assigned(aIniFile) then
    Exit;

  // If not export mode remove file data
  if not ExportMode then
    aIniFile.Clear;

  i := 0;
  while i < FullList.Count do
  begin
    aSystem := cEmutecaSystem(FullList[i]);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingSystemList, aSystem.ID,
        aSystem.Title, i, FullList.Count);

    aSystem.SaveToIni(aIniFile, ExportMode);
    Inc(i);

    if aSystem.Enabled then
      aSystem.SaveLists(SysDataFolder + aSystem.FileName, False);
  end;
  if ProgressCallBack <> nil then
    ProgressCallBack(rsSavingSystemList, '', '', 0, 0);
end;


procedure cEmutecaSystemManager.AssingAllTo(aList: TStrings);
var
  i: integer;
  aSystem: cEmutecaSystem;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    aSystem := cEmutecaSystem(FullList[i]);
    aList.AddObject(aSystem.Title, FullList[i]);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaSystemManager.AssingEnabledTo(aList: TStrings);
var
  i: longint;
  aSystem: cEmutecaSystem;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < EnabledList.Count do
  begin
    aSystem := cEmutecaSystem(EnabledList[i]);
    if aSystem.Enabled then
    begin
      aList.AddObject(aSystem.Title, aSystem);
    end;
    Inc(i);
  end;
  aList.EndUpdate;
end;

constructor cEmutecaSystemManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFullList := cEmutecaSystemList.Create(True);
  FEnabledList := cEmutecaSystemList.Create(False);
end;

destructor cEmutecaSystemManager.Destroy;
begin
  FreeAndNil(FEnabledList);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

end.
