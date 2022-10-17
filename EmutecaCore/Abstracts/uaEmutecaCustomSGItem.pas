unit uaEmutecaCustomSGItem;

{< caEmutecaCustomSGItem abstract class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2020-2020 Chixpy

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, LazFileUtils,
  // CHX units
  uCHXStrUtils,
  // CHX abstracts
  uaCHXStorable,
  // Emuteca Core units
  uEmutecaConst,
  // Emuteca Core classes
  ucEmutecaPlayingStats;

type

  { caEmutecaCustomSGItem }

  caEmutecaCustomSGItem = class(caCHXStorableTxt)
  private
    FStats: cEmutecaPlayingStats;

  protected
    FDate: string;
    FID: string;
    FSortTitle: string;
    FTitle: string;
    function GetDate: string; virtual;
    function GetID: string; virtual;
    function GetMediaFileName: string; virtual;
    function GetSortTitle: string; virtual;
    function GetTitle: string; virtual;
    procedure SetDate(AValue: string); virtual;
    procedure SetID(AValue: string); virtual;
    procedure SetSortTitle(AValue: string); virtual;
    procedure SetTitle(AValue: string); virtual;

    function FormatSortTitle(const aString: string): string;
    function CleanInfo(const aString: string): string;

  public
    function CompareID(aID: string): integer;
    {< Compares aID with current Group ID (case insensitive). }
    function MatchID(aID: string): boolean;
    {< True if ID is matched (case insensitive). }

    function GetActualID: string;
    //< Gets actual ID string, not automade.
    function GetActualTitle: string;
    //< Gets actual SortTitle string, not inherited from group or automade.
    function GetActualSortTitle: string;
    //< Gets actual SortTitle string, not inherited from group or automade.

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ID: string read GetID write SetID;
    {< ID of the item. }
    property Title: string read GetTitle write SetTitle;
    {< Name of the item.

      If empty, then returns Self.ID (overrided by Group.title in Software)
    }
    property SortTitle: string read GetSortTitle write SetSortTitle;
    {< String used for sorting purpouses.

      If empty, then returns Self.Title
    }
    property Date: string read GetDate write SetDate;
    {< Date of item. Developed year for groups, release date for soft.

      Format: YYYY/MM/DD
    }
    property MediaFileName: string read GetMediaFileName;
    {< Filename for media files.

      Returns cleaned Self.SortTitle
    }

    property Stats: cEmutecaPlayingStats read FStats;
    {< Statitisc info. }
  end;

{< caEmutecaCustomSGItem defines common properties, methods and rules for
     software and groups.

   }

implementation

{ caEmutecaCustomSGItem }

function caEmutecaCustomSGItem.GetDate: string;
begin
  Result := FDate;
end;

function caEmutecaCustomSGItem.GetID: string;
begin
  Result := FID;
end;

function caEmutecaCustomSGItem.GetMediaFileName: string;
begin
  Result := CleanFileName(SortTitle, True, False);

  // HACK: Removing last dot like in "Super Mario Bros.",
  // Windows have problems with removing folders ended with dot...
  if Utf8EndsText('.', Result) then
    Result[UTF8LengthFast(Result)] := '_';
end;

function caEmutecaCustomSGItem.GetSortTitle: string;
begin
  Result := FSortTitle;
  if Result <> '' then
    Exit;

  Result := FormatSortTitle(Title);
end;

function caEmutecaCustomSGItem.GetTitle: string;
begin
  Result := FTitle;
  if Result <> '' then
    Exit;

  Result := ID;
end;

procedure caEmutecaCustomSGItem.SetDate(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  AValue := UTF8StringReplace(AValue, '-', '/', [rfReplaceAll, rfIgnoreCase]);

  FDate := AValue;
end;

procedure caEmutecaCustomSGItem.SetID(AValue: string);
begin
  AValue := UTF8Trim(AValue);

  if AValue = ID then
    Exit;

  FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure caEmutecaCustomSGItem.SetSortTitle(AValue: string);
var
  aTitle: string;
begin
  AValue := FormatSortTitle(AValue);
  aTitle := FormatSortTitle(Title);

  if (AValue = aTitle) then
    FSortTitle := ''
  else
    FSortTitle := AValue;
end;

procedure caEmutecaCustomSGItem.SetTitle(AValue: string);
begin
  AValue := UTF8Trim(AValue);

  if AValue = ID then
    FTitle := ''
  else
    FTitle := AValue;
end;

function caEmutecaCustomSGItem.FormatSortTitle(const aString: string): string;
begin
  Result := UTF8TextReplace(UTF8Trim(aString), ': ', ' - ');
  Result := UTF8TextReplace(Result, ' & ', ' and ');
end;

function caEmutecaCustomSGItem.CleanInfo(const aString: string): string;
begin
  Result := aString;

  // TODO: Not completely sure about this...
  Result := UTF8StringReplace(Result, ') (', '; ', [rfReplaceAll, rfIgnoreCase]);
  Result := UTF8StringReplace(Result, ')(', '; ', [rfReplaceAll, rfIgnoreCase]);

  Result := UTF8StringReplace(Result, '] [', '; ', [rfReplaceAll, rfIgnoreCase]);
  Result := UTF8StringReplace(Result, '][', '; ', [rfReplaceAll, rfIgnoreCase]);

  Result := UTF8StringReplace(Result, ') [', '; ', [rfReplaceAll, rfIgnoreCase]);
  Result := UTF8StringReplace(Result, '] (', '; ', [rfReplaceAll, rfIgnoreCase]);
  Result := UTF8StringReplace(Result, ')[', '; ', [rfReplaceAll, rfIgnoreCase]);
  Result := UTF8StringReplace(Result, '](', '; ', [rfReplaceAll, rfIgnoreCase]);

  Result := UTF8StringReplace(Result, ']', '', [rfReplaceAll, rfIgnoreCase]);
  Result := UTF8StringReplace(Result, '[', '', [rfReplaceAll, rfIgnoreCase]);
  Result := UTF8StringReplace(Result, ')', '', [rfReplaceAll, rfIgnoreCase]);
  Result := UTF8StringReplace(Result, '(', '', [rfReplaceAll, rfIgnoreCase]);

  Result := UTF8Trim(Result);
end;

function caEmutecaCustomSGItem.GetActualID: string;
begin
  Result := FID;
end;

function caEmutecaCustomSGItem.GetActualTitle: string;
begin
  Result := FTitle;
end;

function caEmutecaCustomSGItem.GetActualSortTitle: string;
begin
  Result := FSortTitle;
end;


function caEmutecaCustomSGItem.CompareID(aID: string): integer;
begin
  Result := UTF8CompareText(Self.ID, aID);
end;

function caEmutecaCustomSGItem.MatchID(aID: string): boolean;
begin
  Result := CompareID(aID) = 0;
end;

constructor caEmutecaCustomSGItem.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FStats := cEmutecaPlayingStats.Create(Self);
end;

destructor caEmutecaCustomSGItem.Destroy;
begin
  Stats.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(caEmutecaCustomSGItem);

finalization
  UnRegisterClass(caEmutecaCustomSGItem);
end.
