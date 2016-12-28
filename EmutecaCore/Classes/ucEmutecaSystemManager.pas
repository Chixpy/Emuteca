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

{ cEmutecaSystemManager unit. }
unit ucEmutecaSystemManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, IniFiles, contnrs,
  uaEmutecaManager, ucEmutecaSystem;

type
  { cEmutecaSystemManager }

  cEmutecaSystemManager = class(caEmutecaManagerIni)
  private
    FFullList: cEmutecaSystemList;
    FVisibleList: cEmutecaSystemList;

  protected

  public
    procedure LoadFromFileIni(IniFile: TCustomIniFile); override;
    procedure SaveToFileIni(IniFile: TCustomIniFile;
      const ExportMode: boolean); override;

    function ItemById(aId: string; Autocreate: Boolean = False): cEmutecaSystem;
    {< Returns the system with aId key.

       @Result cEmutecaSystem found or nil.
    }

    procedure AssingAllTo(aList: TStrings); override; deprecated;
    procedure AssingEnabledTo(aList: TStrings); override; deprecated;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FullList: cEmutecaSystemList read FFullList;
    property VisibleList: cEmutecaSystemList read FVisibleList;

  end;

implementation

{ cEmutecaSystemManager }


procedure cEmutecaSystemManager.LoadFromFileIni(IniFile: TCustomIniFile);
var
  TempList: TStringList;
  TempSys: cEmutecaSystem;
  i: longint;
begin
  if not Assigned(IniFile) then
    Exit;

  TempList := TStringList.Create;
  try
    IniFile.ReadSections(TempList);
    TempList.Sort;

    i := 0;
    while i < TempList.Count do
    begin
      TempSys := cEmutecaSystem.Create(nil);
      TempSys.ID := TempList[i];
      TempSys.LoadFromFileIni(IniFile);
      FullList.Add(TempSys);
      if TempSys.Enabled then
        VisibleList.Add(TempSys);
      Inc(i);

      if ProgressCallBack <> nil then
        ProgressCallBack(rsLoadingSystemList, TempSys.ID,
          TempSys.Title, i, TempList.Count);
    end;
  finally
    FreeAndNil(TempList);
  end;
end;

procedure cEmutecaSystemManager.SaveToFileIni(IniFile: TCustomIniFile;
  const ExportMode: boolean);
var
  i: longint;
  aSystem: cEmutecaSystem;
begin
  if not Assigned(IniFile) then
    Exit;

  // if not ExportMode then
  //   IniFile.Clear;  <-- TMemIniFile

  i := 0;
  while i < FullList.Count do
  begin
    aSystem := cEmutecaSystem(FullList[i]);
    aSystem.SaveToFileIni(IniFile, ExportMode);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingSystemList, aSystem.ID,
        aSystem.Title, i, FullList.Count);
  end;
end;

function cEmutecaSystemManager.ItemById(aId: string; Autocreate: Boolean
  ): cEmutecaSystem;
var
  i: integer;
  aSystem: cEmutecaSystem;
begin
  Result := nil;

  i := 0;
  while (Result = nil) and (i < FullList.Count) do
  begin
    aSystem := cEmutecaSystem(FullList[i]);
    if UTF8CompareText(aSystem.ID, aId) = 0 then
      Result := aSystem;
    inc(i);
  end;

    // Opps, creating it
  if Autocreate and (not assigned(Result)) then
  begin
    Result := cEmutecaSystem.Create(nil);
    Result.ID := aId;
    Result.Title := aId;
    Result.Enabled := True;
    Self.FullList.Add(Result);
    Self.VisibleList.Add(Result);
  end;
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
  while i < VisibleList.Count do
  begin
    aSystem := cEmutecaSystem(VisibleList[i]);
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
  FFullList := TComponentList.Create(True);
  // TODO: OnCompare FullList.OnCompare := ;
  FVisibleList := TComponentList.Create(False);
  // TODO: OnCompare VisibleList.OnCompare := ;
end;

destructor cEmutecaSystemManager.Destroy;
begin
  FreeAndNil(FVisibleList);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

end.
