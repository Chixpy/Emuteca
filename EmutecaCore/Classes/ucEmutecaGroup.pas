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

{ cEmutecaGroup unit. }
unit ucEmutecaGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, IniFiles, fgl, LazUTF8,
  uaCHXStorable,
  ucEmutecaPlayingStats;

const
  krsCSVGroupHeader = '"ID","Title","System","Year","Developer"';
  krsCSVGroupStatsHeader = krsCSVGroupHeader + ',' + krsCSVStatsHeader;

type
  { cEmutecaGroup }

  cEmutecaGroup = class(caCHXStorableTxt)
  private
    FDeveloper: string;
    FID: string;
    FStats: cEmutecaPlayingStats;
    FSystem: TObject;
    FTitle: string;
    FYear: string;
    function GetDataString: string;
    function GetTitle: string;
    procedure SetDataString(AValue: string);
    procedure SetDeveloper(AValue: string);
    procedure SetID(AValue: string);
    procedure SetSystem(AValue: TObject);
    procedure SetTitle(AValue: string);
    procedure SetYear(AValue: string);


  public
    property System: TObject read FSystem write SetSystem;
    //< Dirty hack...

    property DataString: string read GetDataString write SetDataString;

    procedure LoadFromIni(aIniFile: TCustomIniFile); override;
    procedure SaveToIni(aIniFile: TCustomIniFile; const ExportMode: boolean);
      override;

    function GetActualTitle: string;
    //< Gets actual Title string, not automade

    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToStrLst(aTxtFile: TStrings; const ExportMode: boolean);
      override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ID: string read FID write SetID;
    {< ID and Sort Title of the Parent. }
    property Title: string read GetTitle write SetTitle;
    {< Name of the parent. }
    property Year: string read FYear write SetYear;
    {< Development year. }
    property Developer: string read FDeveloper write SetDeveloper;
    {< Developer. }

    property Stats: cEmutecaPlayingStats read FStats;
  end;

  { cEmutecaGroupList }

  cEmutecaGenGroupList = specialize TFPGObjectList<cEmutecaGroup>;
  cEmutecaGroupList = class(cEmutecaGenGroupList);

  TEmutecaReturnGroupCB = function(aGroup: cEmutecaGroup): boolean of
    object;

implementation

{ cEmutecaGroup }

procedure cEmutecaGroup.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;

  if UTF8CompareText(AValue, ID) = 0 then
    FTitle := ''
  else
    FTitle := AValue;
end;

procedure cEmutecaGroup.SetYear(AValue: string);
begin
  if FYear = AValue then
    Exit;
  FYear := AValue;
end;

procedure cEmutecaGroup.LoadFromIni(aIniFile: TCustomIniFile);
begin

end;

procedure cEmutecaGroup.SaveToIni(aIniFile: TCustomIniFile;
  const ExportMode: boolean);
begin

end;

function cEmutecaGroup.GetActualTitle: string;
begin
  Result := FTitle;
end;

procedure cEmutecaGroup.SetID(AValue: string);
begin
  if FID = AValue then
    Exit;
  FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure cEmutecaGroup.SetSystem(AValue: TObject);
begin
  if FSystem = AValue then Exit;
  FSystem := AValue;
end;

function cEmutecaGroup.GetDataString: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    SaveToStrLst(aStringList, False);
  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

function cEmutecaGroup.GetTitle: string;
begin
  Result := FTitle;
  if FTitle <> '' then
    Result := FTitle
  else
    Result := ID;
end;

procedure cEmutecaGroup.SetDataString(AValue: string);
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.CommaText := AValue;

    LoadFromStrLst(aStringList);
  finally
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaGroup.SetDeveloper(AValue: string);
begin
  if FDeveloper = AValue then
    Exit;
  FDeveloper := AValue;
end;

constructor cEmutecaGroup.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FStats := cEmutecaPlayingStats.Create(Self);
end;

destructor cEmutecaGroup.Destroy;
begin
  Stats.Destroy;
  inherited Destroy;
end;

procedure cEmutecaGroup.LoadFromStrLst(aTxtFile: TStrings);
begin
  if not assigned(aTxtFile) then
    Exit;

  while aTxtFile.Count < 4 do
    aTxtFile.Add('');

  ID := aTxtFile[0];
  Title := aTxtFile[1];
  Year := aTxtFile[2];
  Developer := aTxtFile[3];
  Stats.LoadFromStrLst(aTxtFile, 4);
  // Next := aTxtFile[7]
end;

procedure cEmutecaGroup.SaveToStrLst(aTxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(aTxtFile) then
    Exit;

  aTxtFile.Add(ID);
  aTxtFile.Add(GetActualTitle);
  aTxtFile.Add(Year);
  aTxtFile.Add(Developer);

  Stats.WriteToStrLst(aTxtFile, ExportMode);
end;


initialization
  RegisterClass(cEmutecaGroup);

finalization
  UnRegisterClass(cEmutecaGroup);

end.
