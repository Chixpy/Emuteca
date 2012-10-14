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

{cGameGroup unit}
unit uEmutecaGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, uCHXStrUtils, FileUtil, LazUTF8,
  // Common
  uEmutecaRscStr, uEmutecaConst,
  // Emuteca
  uGameStats;

type

  { @name.

    @definitionList(
    @itemLabel(NOTE:)
    @item(Because PascalScript don't suport overloaded methods,
      we don't use them right here.)
    )
  }

  cGameGroup = class (cGameStats)
  private
    FDeveloper: String;
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
    property Name: String read FName write SetName;
    property SortKey: String read FSortKey write SetSortKey;
    property Year: String read FYear write SetYear;

    property Developer: String read FDeveloper write SetDeveloper;
    property Tags: TStrings read FTags write SetTags;
    property MediaFileName: String
      read FMediaFileName write SetMediaFileName;

    property DataString: String read GetDataString write SetDataString;

    procedure ExportData(aFilename: String; ExportMode: Boolean);
    procedure ExportDataIni(aIniFile: TCustomIniFile; ExportMode: Boolean);
    procedure ImportData(aFilename: String);
    procedure ImportDataIni(aIniFile: TCustomIniFile);

    constructor Create(aName: String);
    destructor Destroy; override;
  end;

implementation

{ cGameGroup }

procedure cGameGroup.SetName(const AValue: String);
begin
  FName := Trim(AValue);
end;

procedure cGameGroup.SetKey(const AValue: String);
begin
  FKey := Trim(UTF8LowerCase(AValue));
end;

procedure cGameGroup.SetDeveloper(const AValue: String);
begin
  FDeveloper := Trim(AValue);
end;

function cGameGroup.GetDataString: String;
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

procedure cGameGroup.SetDataString(const AValue: String);
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

procedure cGameGroup.SetMediaFileName(const AValue: String);
begin
  if RightStr(AValue, Length(kCUVirtualGroupExt)) = kCUVirtualGroupExt then
  begin
    FMediaFileName := Trim(ExtractFileNameOnly(AValue));
    FMediaFileName := CleanFileName(FMediaFileName + kCUVirtualGroupExt);
  end
  else
    FMediaFileName := CleanFileName(Trim(AValue) + kCUVirtualGroupExt);
end;

procedure cGameGroup.SetSortKey(AValue: String);
begin
  FSortKey := Trim(AValue);
end;

procedure cGameGroup.SetTags(const AValue: TStrings);
begin
  FTags := AValue;
end;

procedure cGameGroup.SetYear(const AValue: String);
begin
  FYear := AValue;
end;

procedure cGameGroup.ExportData(aFilename: String; ExportMode: Boolean);
var
  F: TMemInifile;
begin
  F := TMemIniFile.Create(UTF8ToSys(aFilename));
  try
    ExportDataIni(F, ExportMode);
  finally
    FreeAndNil(F);
  end;
end;

procedure cGameGroup.ExportDataIni(aIniFile: TCustomIniFile;
  ExportMode: Boolean);
begin
  if aIniFile = nil then
    Exit;
  aIniFile.WriteString(CGAMEGROUPKEY + Key, 'Name', Name);
  aIniFile.WriteString(CGAMEGROUPKEY + Key, 'SortName', SortKey);
  aIniFile.WriteString(CGAMEGROUPKEY + Key, 'Year', Year);

  aIniFile.WriteString(CGAMEGROUPKEY + Key, 'Developer', Developer);
  aIniFile.WriteString(CGAMEGROUPKEY + Key, 'Tags', Tags.CommaText);
  aIniFile.WriteString(CGAMEGROUPKEY + Key, 'MediaFileName', MediaFileName);
end;

procedure cGameGroup.ImportData(aFilename: String);
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

procedure cGameGroup.ImportDataIni(aIniFile: TCustomIniFile);
begin
  if aIniFile = nil then
    Exit;

  Name := aIniFile.ReadString(CGAMEGROUPKEY + Key, 'Name',  Name);
  SortKey := aIniFile.ReadString(CGAMEGROUPKEY + Key, 'SortName',  SortKey);
  Year := aIniFile.ReadString(CGAMEGROUPKEY + Key, 'Year', Year);

  Developer := aIniFile.ReadString(CGAMEGROUPKEY + Key, 'Developer', Developer);
  Tags.CommaText := aIniFile.ReadString(CGAMEGROUPKEY + Key, 'Tags',Tags.CommaText);
  MediaFileName := aIniFile.ReadString(CGAMEGROUPKEY + Key, 'MediaFileName', MediaFileName);
end;

constructor cGameGroup.Create(aName: String);
begin
  inherited Create;

  Name := aName;
  Key := aName;
  SortKey := aName;
  MediaFileName := aName + kCUVirtualGroupExt;
  FTags := TStringList.Create;
end;

destructor cGameGroup.Destroy;
begin
  FreeAndNil(FTags);
  inherited Destroy;
end;

end.

