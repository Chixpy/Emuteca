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
  Classes, SysUtils, fgl, LazFileUtils, contnrs,
  uCHXStrUtils,
  uaCHXStorable,
  ucEmutecaPlayingStats, ucEmutecaSystem;

type
  { cEmutecaParent }

  cEmutecaParent = class(caCHXStorableTxt, IFPObserver)
  private
    FID: string;
    FStats: cEmutecaPlayingStats;
    FSystem: cEmutecaSystem;
    FSystemKey: string;
    FTitle: string;
    function GetDataString: string;
    procedure SetDataString(AValue: string);
    procedure SetID(AValue: string);
    procedure SetStats(AValue: cEmutecaPlayingStats);
    procedure SetSystem(AValue: cEmutecaSystem);
    procedure SetSystemKey(AValue: string);
    procedure SetTitle(AValue: string);


  public
    property DataString: string read GetDataString write SetDataString;

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

    // Cached Data
    // -----------
    property System: cEmutecaSystem read FSystem write SetSystem;
    {< Cache link for system. }

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    {< Subject has changed. }


    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Title: string read FTitle write SetTitle;
    {< Name of the parent. }
    property SystemKey: string read FSystemKey write SetSystemKey;
    {< ID of the system. }
    property ID: string read FID write SetID;
    {< ID of the Parent (and Sorting)}

    // Usage statitics
    // ---------------
    property Stats: cEmutecaPlayingStats read FStats write SetStats;
  end;

  { cEmutecaParentList }

  cEmutecaParentList = TComponentList;

  TEmutecaReturnParentCB = function(aSystem: cEmutecaParent): boolean of
    object;

implementation

{ cEmutecaParent }

procedure cEmutecaParent.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

procedure cEmutecaParent.SetSystemKey(AValue: string);
begin
  FSystemKey := SetAsID(AValue);
end;

procedure cEmutecaParent.SetID(AValue: string);
begin
  FID := SetAsID(AValue);
end;

procedure cEmutecaParent.SetStats(AValue: cEmutecaPlayingStats);
begin
  if FStats = AValue then Exit;
  FStats := AValue;
end;

procedure cEmutecaParent.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;

  if Assigned(FSystem) then
    FSystem.FPODetachObserver(Self);

  FSystem := AValue;

  if Assigned(System) then
    System.FPOAttachObserver(Self);
end;

function cEmutecaParent.GetDataString: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    SaveToFileTxt(aStringList, False);
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

  FStats := cEmutecaPlayingStats.Create(Self);
end;

destructor cEmutecaParent.Destroy;
begin
  if Assigned(System) then
    System.FPODetachObserver(Self);
  FreeAndNil(FStats);

  inherited Destroy;
end;

procedure cEmutecaParent.LoadFromFileTxt(TxtFile: TStrings);
begin
  if not assigned(TxtFile) then
    Exit;

  if TxtFile.Count > 0 then
    self.ID := TxtFile[0];
  if TxtFile.Count > 1 then
    self.SystemKey := TxtFile[1];
  if TxtFile.Count > 2 then
    self.Title := TxtFile[2];
end;

procedure cEmutecaParent.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(TxtFile) then
    Exit;

  TxtFile.Add(ID);
  TxtFile.Add(SystemKey);
  TxtFile.Add(Title);
end;

procedure cEmutecaParent.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooFree: System := nil;
    else
      SystemKey := cEmutecaSystem(ASender).ID;
  end;
end;

initialization
  RegisterClass(cEmutecaParent);

finalization
  UnRegisterClass(cEmutecaParent);

end.
