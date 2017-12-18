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
  Classes, SysUtils, fgl, LazFileUtils, LazUTF8, IniFiles,
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
    FTempFolder: string;
    procedure SetSysDataFolder(AValue: string);
    procedure SetTempFolder(AValue: string);

  protected
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  public
    property TempFolder: string read FTempFolder write SetTempFolder;
    property SysDataFolder: string read FSysDataFolder write SetSysDataFolder;

    procedure ClearData;
    //< Clears all data WITHOUT saving.
    procedure LoadData;
    //< Reload last data file WITHOUT saving changes.
    procedure SaveData;
    //< Save data to last data file.

    procedure UpdateEnabledList;

    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    procedure LoadFromIni(aIniFile: TIniFile); override;
    procedure SaveToIni(aIniFile: TMemIniFile; const ExportMode: boolean);
      override;

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

procedure cEmutecaSystemManager.SetTempFolder(AValue: string);
var
  i: integer;
begin
  FTempFolder := SetAsFolder(AValue);

  i := 0;
  while i < FullList.Count do
  begin
    FullList[i].TempFolder := TempFolder;
    Inc(i);
  end;
end;

procedure cEmutecaSystemManager.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
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

procedure cEmutecaSystemManager.UpdateEnabledList;
var
  i: integer;
  aSys: cEmutecaSystem;
begin
  EnabledList.Clear;
  i := 0;
  while i < FullList.Count do
  begin
    aSys := FullList[i];
    if aSys.Enabled then
      EnabledList.Add(aSys);
    Inc(i);
  end;
end;

procedure cEmutecaSystemManager.LoadFromIni(aIniFile: TIniFile);
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
      TempSys.TempFolder := TempFolder;
      TempSys.IniFileName := IniFileName;
      TempSys.LoadFromIni(aIniFile);

      if assigned(ProgressCallBack) then
        ProgressCallBack(rsLoadingSystemList, TempSys.Title, TempSys.ID,
           i, TempList.Count);

      FullList.Add(TempSys);
      Inc(i);


      if TempSys.Enabled then
        TempSys.LoadSoftGroupLists(SysDataFolder + TempSys.FileName);
    end;
  finally
    FreeAndNil(TempList);
  end;

  UpdateEnabledList;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', '', 0, 0);
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

    if assigned(ProgressCallBack) then
      ProgressCallBack(rsSavingSystemList, aSystem.Title, aSystem.ID,
        i, FullList.Count);

    aSystem.SaveToIni(aIniFile, ExportMode);
    Inc(i);


    // If ExportMode, we want to save only system info without user data
    //   (exporting other Systems.ini), if we want export system data
    //   we must call aSystem.SaveSoftGroupLists(XXX, True);
    // If not ExportMode, we want to save both:
    //   Systems.ini AND Systems/aSystem.cvs
    if (aSystem.Enabled) and (not ExportMode) then
      aSystem.SaveSoftGroupLists(SysDataFolder + aSystem.FileName, False);
  end;
  if assigned(ProgressCallBack) then
    ProgressCallBack(rsSavingSystemList, '', '', 0, 0);
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
