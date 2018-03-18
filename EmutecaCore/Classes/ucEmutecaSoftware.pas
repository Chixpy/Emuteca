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
unit ucEmutecaSoftware;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, uaEmutecaCustomSystem,
  uaEmutecaCustomGroup, uaEmutecaCustomSoft, uEmutecaCommon, uCHXStrUtils;

type
  { cEmutecaSoftware. }

  cEmutecaSoftware = class(caEmutecaCustomSoft, IFPObserver)
  private
    FCachedGroup: caEmutecaCustomGroup;
    FCachedSystem: caEmutecaCustomSystem;
    procedure SetCachedGroup(AValue: caEmutecaCustomGroup);
    procedure SetCachedSystem(AValue: caEmutecaCustomSystem);

  protected
    function GetTitle: string; override;
    procedure SetTitle(AValue: string); override;

  public

    // Cached data
    property CachedSystem: caEmutecaCustomSystem
      read FCachedSystem write SetCachedSystem;
    property CachedGroup: caEmutecaCustomGroup
      read FCachedGroup write SetCachedGroup;

    function MatchGroupFile: boolean; override;

    procedure SearchAllRelatedFiles(OutFileList: TStrings;
      aFolder: string; Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean); override;
    function SearchFirstRelatedFile(aFolder: string;
      Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean): string; override;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  end;

  TEmutecaReturnSoftCB = function(aSoft: cEmutecaSoftware): boolean of object;

implementation

{ cEmutecaSoftware }

procedure cEmutecaSoftware.SetCachedSystem(AValue: caEmutecaCustomSystem);
begin
  if FCachedSystem = AValue then
    Exit;

  if Assigned(FCachedSystem) then
    FCachedSystem.FPODetachObserver(Self);

  FCachedSystem := AValue;

  if Assigned(CachedSystem) then
  begin
    CachedSystem.FPOAttachObserver(Self);
  end;
end;

function cEmutecaSoftware.GetTitle: string;
begin
  Result := FTitle;
  if Result <> '' then
    Exit;

  if Assigned(CachedGroup) then
    Result := CachedGroup.Title
  else
    Result := GroupKey;
end;

procedure cEmutecaSoftware.SetCachedGroup(AValue: caEmutecaCustomGroup);
begin
  if FCachedGroup = AValue then
    Exit;

  if Assigned(FCachedGroup) then
  begin
    FCachedGroup.FPODetachObserver(Self);
  end;

  FCachedGroup := AValue;

  if Assigned(FCachedGroup) then
  begin
    FCachedGroup.FPOAttachObserver(Self);
    GroupKey := CachedGroup.ID;
  end;
  // else GroupKey := ''; We don't want to delete old GroupKey
end;

procedure cEmutecaSoftware.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;

  if Assigned(CachedGroup) and
    (UTF8CompareText(AValue, CachedGroup.Title) = 0) then
    FTitle := ''
  else
    inherited SetTitle(AValue);
end;

function cEmutecaSoftware.MatchGroupFile: boolean;
begin
  if Assigned(CachedGroup) then
  begin
    Result := CompareFilenames(CachedGroup.MediaFileName, GetMediaFileName) = 0;
  end
  else
    inherited MatchGroupFile;
end;

procedure cEmutecaSoftware.SearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; Extensions: TStrings; SearchInComp: boolean;AutoExtract: boolean);
begin
  if Assigned(CachedSystem) then
      EmuTKSearchAllRelatedFiles(OutFileList, aFolder, GetMediaFileName, Extensions, SearchInComp,
        AutoExtract, CachedSystem.TempFolder)
  else
    inherited;
end;

function cEmutecaSoftware.SearchFirstRelatedFile(aFolder: string;
  Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean): string;
begin
  if Assigned(CachedSystem) then
      Result := EmuTKSearchFirstRelatedFile(aFolder, GetMediaFileName,
        Extensions, SearchInComp, AutoExtract, CachedSystem.TempFolder)
  else
    Result := inherited;
end;

procedure cEmutecaSoftware.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if not assigned(ASender) then
    Exit;

  if ASender = CachedSystem then
  begin
    case Operation of
      ooFree: CachedSystem := nil;
      else
        // There isn't SystemKey
        // SystemKey := caEmutecaCustomSystem(ASender).ID;
        ;
    end;
  end
  else
  if ASender = CachedGroup then
  begin
    case Operation of
      ooFree: CachedGroup := nil;
      else
        GroupKey := caEmutecaCustomGroup(ASender).ID;
    end;
  end;
end;

constructor cEmutecaSoftware.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cEmutecaSoftware.Destroy;
begin
  if Assigned(CachedSystem) then
    CachedSystem.FPODetachObserver(Self);
  if Assigned(CachedGroup) then
    CachedGroup.FPODetachObserver(Self);

  inherited Destroy;
end;

initialization
  RegisterClass(cEmutecaSoftware);

finalization
  UnRegisterClass(cEmutecaSoftware);
end.
