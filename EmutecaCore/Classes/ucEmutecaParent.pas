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

{ cEmutecaParent unit. }
unit ucEmutecaParent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazFileUtils,
  uEmutecaCommon, uaEmutecaStorable;

type
  { cEmutecaParent }

  cEmutecaParent = class(caEmutecaStorableTxt)
  private
    FSortName: string;
    FSystem: string;
    FTitle: string;
    function GetDataString: string;
    procedure SetDataString(AValue: string);
    procedure SetSortName(AValue: string);
    procedure SetSystem(AValue: string);
    procedure SetTitle(AValue: string);


  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    property DataString: string read GetDataString write SetDataString;

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

  published
    property Title: string read FTitle write SetTitle;
    {< Name of the parent. }
    property System: string read FSystem write SetSystem;
    {< ID of the system. }
    property SortName: string read FSortName write SetSortName;
    {< ID of the Parent (and Sorting)}
  end;

  { cEmutecaParentMap }

  cEmutecaParentMap = specialize TFPGMapObject<string, cEmutecaParent>;

implementation

{ cEmutecaParent }

procedure cEmutecaParent.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

procedure cEmutecaParent.SetSystem(AValue: string);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;
end;

procedure cEmutecaParent.SetSortName(AValue: string);
begin
  if FSortName = AValue then
    Exit;
  FSortName := AValue;
end;

function cEmutecaParent.GetDataString: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    SaveToFileTxt(aStringList, True);
  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaParent.SetDataString(AValue: string);
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.CommaText := AValue;

    LoadFromFileTxt(aStringList);
  finally
    FreeAndNil(aStringList);
  end;
end;

constructor cEmutecaParent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cEmutecaParent.Destroy;
begin
  inherited Destroy;
end;

procedure cEmutecaParent.LoadFromFileTxt(TxtFile: TStrings);
begin
  if not assigned(TxtFile) then
    Exit;

  if TxtFile.Count > 0 then
    self.SortName := TxtFile[0];
  if TxtFile.Count > 1 then
    self.System := TxtFile[1];
  if TxtFile.Count > 2 then
    self.Title := TxtFile[2];
end;

procedure cEmutecaParent.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(TxtFile) then
    Exit;

  TxtFile.Add(SortName);
  TxtFile.Add(System);
  TxtFile.Add(Title);
end;

end.
