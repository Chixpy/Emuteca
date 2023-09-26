unit ucEmutecaSoftware;

{< cEmutecaSoftware class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2020 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8,
  // CHX units
  uCHXStrUtils,
  // Emuteca Core abstracts
  uaEmutecaCustomSystem, uaEmutecaCustomGroup, uaEmutecaCustomSoft;

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

    function GetSortTitle: string; override;
    procedure SetSortTitle(AValue: string); override;

  public

    // Cached data
    property CachedSystem: caEmutecaCustomSystem
      read FCachedSystem write SetCachedSystem;
    property CachedGroup: caEmutecaCustomGroup
      read FCachedGroup write SetCachedGroup;

    function MatchGroupFile: boolean; override;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  end;

  {< This class defines a parented sofware with links to its System and Group.
       In CachedSystem and CachedGroup properties, and autoupdating the
       reference keys from both with Observer Pattern.

     Overrides the GetTitle method to get actual title from group if Title
       property is empty.

     It can check if the filenames for media match with groups one.
  }

  TEmutecaSoftCB = procedure(aSoft: cEmutecaSoftware) of object;

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
    Result := GroupKey; // Faster than inherited;?
end;

procedure cEmutecaSoftware.SetCachedGroup(AValue: caEmutecaCustomGroup);
var
  aTitle, aSort: string;
begin
  if FCachedGroup = AValue then
    Exit;

  // Keeping Title and Sort
  aTitle := FTitle;
  aSort := FSortTitle;

  if Assigned(FCachedGroup) then
  begin
    if aTitle = '' then
    begin
      aTitle := Title;
      aSort := SortTitle;
    end;

    FCachedGroup.FPONotifyObservers(Self, ooDeleteItem, nil);
    FCachedGroup.FPODetachObserver(Self);
  end;

  FCachedGroup := AValue;

  if Assigned(FCachedGroup) then
  begin
    FCachedGroup.FPOAttachObserver(Self);
    FCachedGroup.FPONotifyObservers(Self, ooAddItem, nil);
    GroupKey := CachedGroup.ID;
  end;
  // else GroupKey := ''; We don't want to delete old GroupKey if not assigned

  Title := aTitle;
  SortTitle := aSort;
end;

procedure cEmutecaSoftware.SetTitle(AValue: string);
begin
  AValue := UTF8Trim(AValue);

  // Uhm... we want to check Group Title first if CachedGroup is assigned.
  // if FTitle = AValue then
  //   Exit;

  if Assigned(CachedGroup) then
  begin
    if AValue = CachedGroup.Title then
    begin
      FTitle := '';
      FSortTitle := ''; // FSortTitle must empty if FTitle is empty too
    end
    else
    begin
      FTitle := AValue;
    end;
  end
  else
    inherited SetTitle(AValue);
end;

function cEmutecaSoftware.GetSortTitle: string;
begin
  Result := FSortTitle;
  if Result <> '' then
    Exit;

  Result := FormatSortTitle(FTitle);
  if Result <> '' then
    Exit;

  if Assigned(CachedGroup) then
    Result := CachedGroup.SortTitle
  else
    Result := inherited GetSortTitle;
end;

procedure cEmutecaSoftware.SetSortTitle(AValue: string);
begin
  if (FTitle = '') then
  begin
    // If FTitle is empty then FSortTitle must be empty (use group's SortTitle)
    FSortTitle := '';
    Exit;
  end;

  inherited SetSortTitle(AValue);
end;

function cEmutecaSoftware.MatchGroupFile: boolean;
begin
  if Assigned(CachedGroup) then
  begin
    if (GetActualTitle = '') {and (GetActualSortTitle = '')} then
      Result := True
    else
      Result := CompareFilenames(CachedGroup.MediaFileName,
        MediaFileName) = 0;
  end
  else
    Result := inherited MatchGroupFile;
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
        // Actually... There isn't SystemKey as software lists are stored by
        //   their system.
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
{
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
