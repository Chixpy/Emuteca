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

{ cGameGroup unit. }
unit uEmutecaGameGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, uCHXStrUtils, FileUtil, LazUTF8,
  // Emuteca
  uEmutecaConst, uEmutecaRscStr, uEmutecaPlayingStats, uEmutecaGame;

type

  { cEmutecaGameGroup

    @definitionList(
    @itemLabel(NOTE:)
    @item(Because PascalScript don't suport overloaded methods,
      we don't use them right here.)
    )
  }
  cEmutecaGameGroup = class (cEmutecaPlayingStats)
  private
    FDeveloper: String;
    FGames: cGameList;
    FKey: String;
    FName: String;
    FMediaFileName: String;
    FSortKey: String;
    FTags: TStrings;
    FYear: String;
    function GetDataString: String;
    procedure SetDataString(const AValue: String);
    procedure SetDeveloper(const AValue: String);
    procedure SetKey(const AValue: String);
    procedure SetName(const AValue: String);
    procedure SetMediaFileName(const AValue: String);
    procedure SetSortKey(AValue: String);
    procedure SetTags(const AValue: TStrings);
    procedure SetYear(const AValue: String);

  protected

  public
    property Key: String read FKey write SetKey;
    {< Key used as group's ID. }
    property Name: String read FName write SetName;
    {< Name of the group. }
    property SortKey: String read FSortKey write SetSortKey;
    {< String used for sorting operations.

    For example: 'The Game' may be want to be sorted as 'Game, The' }
    property Year: String read FYear write SetYear;
    {< Date of development. "YYYY/MM/DD"}

    property Developer: String read FDeveloper write SetDeveloper;
    {< Developer of the game. }
    property Tags: TStrings read FTags write SetTags;
    {< Shared tags of games. }
    property MediaFileName: String
      read FMediaFileName write SetMediaFileName;
    {< Filaname for group media. }

    property Games: cGameList read FGames;

    property DataString: String read GetDataString write SetDataString;

    procedure ExportData(aFilename: String);
    procedure ExportDataIni(aIniFile: TCustomIniFile);
    procedure ImportData(aFilename: String);
    procedure ImportDataIni(aIniFile: TCustomIniFile);

    constructor Create(aName: String);
    destructor Destroy; override;
  end;

implementation

{ cEmutecaGameGroup }

procedure cEmutecaGameGroup.SetName(const AValue: String);
begin
  FName := UTF8Trim(AValue);
end;

procedure cEmutecaGameGroup.SetKey(const AValue: String);
begin
  FKey := UTF8Trim(UTF8LowerCase(AValue));
end;

procedure cEmutecaGameGroup.SetDeveloper(const AValue: String);
begin
  FDeveloper := UTF8Trim(AValue);
end;

function cEmutecaGameGroup.GetDataString: String;
var
  Tmp: TStringList;
begin
  Result := '';
  Tmp := TStringList.Create;
  try
    Tmp.BeginUpdate;

    // El orden es importante
    Tmp.Add(Key);
    Tmp.Add(Name);
    Tmp.Add(SortKey);
    tmp.Add(MediaFileName);

    Tmp.Add(Developer);
    Tmp.Add(Year);
    tmp.Add(Tags.CommaText);

    tmp.EndUpdate;
    Result := Tmp.CommaText;
  finally
    FreeAndNil(Tmp);
  end;
end;

procedure cEmutecaGameGroup.SetDataString(const AValue: String);
var
  Tmp: TStringList;
  i: Integer;
begin
  tmp := TStringList.Create;
  try
    tmp.CommaText := AValue;

    i := 0;
    while i < Tmp.Count do
    begin
      case i of
        0: Self.Key := Tmp[i];
        1: Self.Name := Tmp[i];
        2: Self.SortKey := Tmp[i];
        3: Self.MediaFileName := Tmp[i];
        4: Self.Developer := Tmp[i];
        5: Self.Year := Tmp[i];
        6: Self.Tags.CommaText := Tmp[i];
      end;
      Inc(i);
    end;
  finally
    FreeAndNil(Tmp);
  end;
end;

procedure cEmutecaGameGroup.SetMediaFileName(const AValue: String);
begin
  if RightStr(AValue, Length(kEmutecaVirtualGroupExt)) = kEmutecaVirtualGroupExt then
  begin
    FMediaFileName := UTF8Trim(ExtractFileNameOnly(AValue));
    FMediaFileName := CleanFileName(FMediaFileName + kEmutecaVirtualGroupExt);
  end
  else
    FMediaFileName := CleanFileName(UTF8Trim(AValue) + kEmutecaVirtualGroupExt);
end;

procedure cEmutecaGameGroup.SetSortKey(AValue: String);
begin
  FSortKey := UTF8Trim(AValue);
end;

procedure cEmutecaGameGroup.SetTags(const AValue: TStrings);
begin
  FTags := AValue;
end;

procedure cEmutecaGameGroup.SetYear(const AValue: String);
begin
  FYear := AValue;
end;

procedure cEmutecaGameGroup.ExportData(aFilename: String);
var
  F: TMemInifile;
begin
  F := TMemIniFile.Create(UTF8ToSys(aFilename));
  try
    ExportDataIni(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure cEmutecaGameGroup.ExportDataIni(aIniFile: TCustomIniFile);
begin
  if aIniFile = nil then
    Exit;
  aIniFile.WriteString(kGroupSectionKey + Key, 'Name', Name);
  aIniFile.WriteString(kGroupSectionKey + Key, 'SortName', SortKey);
  aIniFile.WriteString(kGroupSectionKey + Key, 'Year', Year);

  aIniFile.WriteString(kGroupSectionKey + Key, 'Developer', Developer);
  aIniFile.WriteString(kGroupSectionKey + Key, 'Tags', Tags.CommaText);
  aIniFile.WriteString(kGroupSectionKey + Key, 'MediaFileName', MediaFileName);
end;

procedure cEmutecaGameGroup.ImportData(aFilename: String);
var
  F: TMemInifile;
begin
  if not FileExistsUTF8(aFilename) then
    Exit;
  F := TMemIniFile.Create(UTF8ToSys(aFilename));
  try
    ImportDataIni(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure cEmutecaGameGroup.ImportDataIni(aIniFile: TCustomIniFile);
begin
  if aIniFile = nil then
    Exit;

  Name := aIniFile.ReadString(kGroupSectionKey + Key, 'Name',  Name);
  SortKey := aIniFile.ReadString(kGroupSectionKey + Key, 'SortName',  SortKey);
  Year := aIniFile.ReadString(kGroupSectionKey + Key, 'Year', Year);

  Developer := aIniFile.ReadString(kGroupSectionKey + Key, 'Developer', Developer);
  Tags.CommaText := aIniFile.ReadString(kGroupSectionKey + Key, 'Tags',Tags.CommaText);
  MediaFileName := aIniFile.ReadString(kGroupSectionKey + Key, 'MediaFileName', MediaFileName);
end;

constructor cEmutecaGameGroup.Create(aName: String);
begin
  inherited Create;

  Name := aName;
  Key := aName;
  SortKey := aName;
  MediaFileName := aName + kEmutecaVirtualGroupExt;
  FTags := TStringList.Create;
  FGames := cGameList.Create;
end;

destructor cEmutecaGameGroup.Destroy;
begin
  FreeAndNil(FGames);
  FreeAndNil(FTags);
  inherited Destroy;
end;

end.
