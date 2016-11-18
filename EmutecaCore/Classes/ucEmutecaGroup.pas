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
  Classes, SysUtils, fgl, LazFileUtils, contnrs,
  uCHXStrUtils,
  uaCHXStorable,
  ucEmutecaPlayingStats, ucEmutecaSystem;

type
  { cEmutecaGroup }

  cEmutecaGroup = class(caCHXStorableTxt, IFPObserver)
  private
    FDeveloper: string;
    FID: string;
    FStats: cEmutecaPlayingStats;
    FSystem: cEmutecaSystem;
    FSystemKey: string;
    FTitle: string;
    FYear: string;
    function GetDataString: string;
    procedure SetDataString(AValue: string);
    procedure SetDeveloper(AValue: string);
    procedure SetID(AValue: string);
    procedure SetStats(AValue: cEmutecaPlayingStats);
    procedure SetSystem(AValue: cEmutecaSystem);
    procedure SetSystemKey(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetYear(AValue: string);


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
    property ID: string read FID write SetID;
    {< ID of the Parent (and Sorting)}
    property Title: string read FTitle write SetTitle;
    {< Name of the parent. }
    property SystemKey: string read FSystemKey write SetSystemKey;
    {< ID of the system. }
    property Year: string read FYear write SetYear;
    property Developer: string read FDeveloper write SetDeveloper;

    // Usage statitics
    // ---------------
    property Stats: cEmutecaPlayingStats read FStats write SetStats;
  end;

  { cEmutecaGroupList }

  cEmutecaGroupList = TComponentList;

  TEmutecaReturnGroupCB = function(aSystem: cEmutecaGroup): boolean of
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
  if FYear = AValue then Exit;
  FYear := AValue;
end;

procedure cEmutecaGroup.SetSystemKey(AValue: string);
begin
  FSystemKey := SetAsID(AValue);
end;

procedure cEmutecaGroup.SetID(AValue: string);
begin
  FID := SetAsID(AValue);
end;

procedure cEmutecaGroup.SetStats(AValue: cEmutecaPlayingStats);
begin
  if FStats = AValue then Exit;
  FStats := AValue;
end;

procedure cEmutecaGroup.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;

  if Assigned(FSystem) then
    FSystem.FPODetachObserver(Self);

  FSystem := AValue;

  if Assigned(System) then
    System.FPOAttachObserver(Self);
end;

function cEmutecaGroup.GetDataString: string;
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

procedure cEmutecaGroup.SetDataString(AValue: string);
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

procedure cEmutecaGroup.SetDeveloper(AValue: string);
begin
  if FDeveloper = AValue then Exit;
  FDeveloper := AValue;
end;

constructor cEmutecaGroup.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FStats := cEmutecaPlayingStats.Create(Self);
end;

destructor cEmutecaGroup.Destroy;
begin
  if Assigned(System) then
    System.FPODetachObserver(Self);
  FreeAndNil(FStats);

  inherited Destroy;
end;

procedure cEmutecaGroup.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
begin
  if not assigned(TxtFile) then
    Exit;

  i := 0;
  while i < TxtFile.Count do
  begin
    case i of
      0: ID := TxtFile[i];
      1: SystemKey := TxtFile[i];
      2: Title := TxtFile[i];
      3: Year := TxtFile[i];
      4: Developer := TxtFile[i];
      // 5: ;
      6: Stats.LastTime := StrToFloatDef(TxtFile[i], 0);
      7: Stats.TimesPlayed := StrToIntDef(TxtFile[i], 0);
      8: Stats.PlayingTime := StrToCardinalDef(TxtFile[i], 0);
      else
        ;
    end;
    inc(i);
  end;
end;

procedure cEmutecaGroup.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(TxtFile) then
    Exit;

  TxtFile.Add(ID);
  TxtFile.Add(SystemKey);
  TxtFile.Add(Title);
  TxtFile.Add(Year);
  TxtFile.Add(Developer);
  TxtFile.Add('');

    // Usage statitics
  // ---------------
  TxtFile.Add(FloatToStr(Stats.LastTime));
  TxtFile.Add(IntToStr(Stats.TimesPlayed));
  TxtFile.Add(IntToStr(Stats.PlayingTime));
end;

procedure cEmutecaGroup.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooFree: System := nil;
    else
      SystemKey := cEmutecaSystem(ASender).ID;
  end;
end;

initialization
  RegisterClass(cEmutecaGroup);

finalization
  UnRegisterClass(cEmutecaGroup);

end.
