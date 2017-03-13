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

{ cEmutecaGroup unit. }
unit ucEmutecaGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, contnrs, IniFiles, fgl,
  uCHXStrUtils, uaCHXStorable;

type
  { cEmutecaGroup }

  cEmutecaGroup = class(caCHXStorable)
  private
    FDeveloper: string;
    FID: string;
    FTitle: string;
    FYear: string;
    function GetDataString: string;
    procedure SetDataString(AValue: string);
    procedure SetDeveloper(AValue: string);
    procedure SetID(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetYear(AValue: string);


  public
    procedure LoadFromIni(aIniFile: TCustomIniFile); override;
    procedure SaveToIni(aIniFile: TCustomIniFile; const ExportMode: boolean);
      override;
    property DataString: string read GetDataString write SetDataString;

    procedure LoadFromStrLst(TxtFile: TStrings); override;
    procedure SaveToStrLst(TxtFile: TStrings; const ExportMode: boolean);
      override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ID: string read FID write SetID;
    {< ID and Sort Title of the Parent. }
    property Title: string read FTitle write SetTitle;
    {< Name of the parent. }
    property Year: string read FYear write SetYear;
    {< Development year. }
    property Developer: string read FDeveloper write SetDeveloper;
    {< Developer. }

  end;

  { cEmutecaGroupList }

  cEmutecaGenGroupList = specialize TFPGObjectList<cEmutecaGroup>;
  cEmutecaGroupList = class (cEmutecaGenGroupList);

  TEmutecaReturnGroupCB = function(aGroup: cEmutecaGroup): boolean of
    object;

implementation

{ cEmutecaGroup }

procedure cEmutecaGroup.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
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

procedure cEmutecaGroup.SetID(AValue: string);
begin
  if FID = AValue then
    Exit;
  FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
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
end;

destructor cEmutecaGroup.Destroy;
begin
  inherited Destroy;
end;

procedure cEmutecaGroup.LoadFromStrLst(TxtFile: TStrings);
var
  i: integer;
begin
  if not assigned(TxtFile) then
    Exit;

  while TxtFile.Count < 4 do
    TxtFile.Add('');

      ID := TxtFile[0];
      Title := TxtFile[1];
      Year := TxtFile[2];
      Developer := TxtFile[3];
end;

procedure cEmutecaGroup.SaveToStrLst(TxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(TxtFile) then
    Exit;

  TxtFile.Add(ID);
  TxtFile.Add(Title);
  TxtFile.Add(Year);
  TxtFile.Add(Developer);
end;


initialization
  RegisterClass(cEmutecaGroup);

finalization
  UnRegisterClass(cEmutecaGroup);

end.
