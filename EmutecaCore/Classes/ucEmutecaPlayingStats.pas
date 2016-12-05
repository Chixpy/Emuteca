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

{ cGameStats unit. }
unit ucEmutecaPlayingStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils, IniFiles;

const
  krsIniKeyPlayingTime = 'PlayingTime';
  krsIniKeyTimesPlayed = 'TimesPlayed';
  krsIniKeyLastTime = 'LastTime';

type

  { cEmutecaPlayingStats class.

    This class is for storing stats (System, Emulator, Parent and Soft).
  }

  cEmutecaPlayingStats = class(TComponent)
  private
    FIconIndex: Integer;
    FLastTime: TDateTime;
    FPlayingTime: Int64;
    FTimesPlayed: Int64;
    procedure SetIconIndex(AValue: Integer);
    procedure SetLastTime(AValue: TDateTime);
    procedure SetPlayingTime(AValue: Int64);
    procedure SetTimesPlayed(AValue: Int64);

  public
    procedure AddPlayingTime(const Start: TDateTime; NumberOfSeconds: Int64);
    {< Adds the seconds between two TDateTime to PlayingTime.

       Aditionally adds 1 to TimesPlayed counter and updates LastTime
    }

    procedure WriteToIni(aIniFile: TCustomIniFile; const Section: string);
    procedure LoadFromIni(aIniFile: TCustomIniFile; const Section: string);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property LastTime: TDateTime read FLastTime write SetLastTime;
    {< Last time played.

        In cGroup maybe used to store the last time that a game
        has played from the group. }
    property TimesPlayed: Int64 read FTimesPlayed write SetTimesPlayed;
    {< Total times played. }
    property PlayingTime: Int64 read FPlayingTime write SetPlayingTime;
    {< Total seconds played. }

    { TODO : Cached data for GUI, store elsewhere? }
    property IconIndex: Integer read FIconIndex write SetIconIndex;
  end;

implementation

{ cEmutecaPlayingStats }

procedure cEmutecaPlayingStats.SetLastTime(AValue: TDateTime);
begin
  if FLastTime = AValue then
    Exit;
  FLastTime := AValue;
end;

procedure cEmutecaPlayingStats.SetIconIndex(AValue: Integer);
begin
  if FIconIndex = AValue then Exit;
  FIconIndex := AValue;
end;

procedure cEmutecaPlayingStats.SetPlayingTime(AValue: Int64);
begin
  if FPlayingTime = AValue then
    Exit;
  FPlayingTime := AValue;
end;

procedure cEmutecaPlayingStats.SetTimesPlayed(AValue: Int64);
begin
  if FTimesPlayed = AValue then
    Exit;
  FTimesPlayed := AValue;
end;

procedure cEmutecaPlayingStats.AddPlayingTime(const Start: TDateTime;
  NumberOfSeconds: Int64);
begin
  PlayingTime := PlayingTime + NumberOfSeconds;
  LastTime := Start;
  TimesPlayed := TimesPlayed + 1;
end;

procedure cEmutecaPlayingStats.WriteToIni(aIniFile: TCustomIniFile;
  const Section: string);
begin
  // TODO: Exception...
  if not assigned(aIniFile) then
    Exit;

  aIniFile.WriteInt64(Section, krsIniKeyPlayingTime, PlayingTime);
  aIniFile.WriteInt64(Section, krsIniKeyTimesPlayed, TimesPlayed);
  aIniFile.WriteDateTime(Section, krsIniKeyLastTime, LastTime);
end;

procedure cEmutecaPlayingStats.LoadFromIni(aIniFile: TCustomIniFile;
  const Section: string);
var
  TmpString: string;
begin
  TmpString := aIniFile.ReadString(Section, krsIniKeyLastTime, '');
  if TmpString <> '' then
    LastTime := StrToDateTimeDef(TmpString, LastTime);
  TmpString := aIniFile.ReadString(Section, krsIniKeyPlayingTime, '');
  PlayingTime := StrToInt64Def(TmpString, PlayingTime);
  TmpString := aIniFile.ReadString(Section, krsIniKeyTimesPlayed, '');
  TimesPlayed := StrToInt64Def(TmpString, TimesPlayed);
end;

constructor cEmutecaPlayingStats.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  IconIndex := -1;
end;

destructor cEmutecaPlayingStats.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(cEmutecaPlayingStats);

finalization
  UnRegisterClass(cEmutecaPlayingStats);
end.
