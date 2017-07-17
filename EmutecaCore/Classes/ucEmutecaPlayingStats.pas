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

{ cGameStats unit. }
unit ucEmutecaPlayingStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils, IniFiles, Graphics;

const
  krsIniKeyPlayingTime = 'PlayingTime';
  krsIniKeyTimesPlayed = 'TimesPlayed';
  krsIniKeyLastTime = 'LastTime';
  krsCSVStatsHeader = '"Last Time","Times Played","Playing Time"';

type

  { cEmutecaPlayingStats class.

    This class is for storing stats (System, Emulator, Parent and Soft).
  }

  cEmutecaPlayingStats = class(TComponent)
  private
    FIcon: TPicture;
    FIconIndex: integer;
    FLastTime: TDateTime;
    FPlayingTime: int64;
    FTimesPlayed: int64;
    procedure SetIcon(AValue: TPicture);
    procedure SetIconIndex(AValue: integer);
    procedure SetLastTime(AValue: TDateTime);
    procedure SetPlayingTime(AValue: int64);
    procedure SetTimesPlayed(AValue: int64);

  public
    procedure AddPlayingTime(const Start: TDateTime; NumberOfSeconds: int64);
    {< Adds the seconds between two TDateTime to PlayingTime.

       Aditionally adds 1 to TimesPlayed counter and updates LastTime
    }

    procedure WriteToIni(aIniFile: TCustomIniFile; const Section: string;
      ExportMode: boolean);
    procedure LoadFromIni(aIniFile: TCustomIniFile; const Section: string);
    procedure WriteToStrLst(aTxtFile: TStrings; ExportMode: boolean);
    procedure LoadFromStrLst(aTxtFile: TStrings; const NLine: integer);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property LastTime: TDateTime read FLastTime write SetLastTime;
    {< Last time played.

        In cGroup maybe used to store the last time that a game
        has played from the group. }
    property TimesPlayed: int64 read FTimesPlayed write SetTimesPlayed;
    {< Total times played. }
    property PlayingTime: int64 read FPlayingTime write SetPlayingTime;
    {< Total seconds played. }

    { TODO : Cached data for GUI, store elsewhere? }
    property IconIndex: integer read FIconIndex write SetIconIndex;
    property Icon: TPicture read FIcon write SetIcon;
  end;

implementation

{ cEmutecaPlayingStats }

procedure cEmutecaPlayingStats.SetLastTime(AValue: TDateTime);
begin
  if FLastTime = AValue then
    Exit;
  FLastTime := AValue;
end;

procedure cEmutecaPlayingStats.SetIconIndex(AValue: integer);
begin
  if FIconIndex = AValue then
    Exit;
  FIconIndex := AValue;
end;

procedure cEmutecaPlayingStats.SetIcon(AValue: TPicture);
begin
  if FIcon = AValue then Exit;
  FIcon := AValue;
end;

procedure cEmutecaPlayingStats.SetPlayingTime(AValue: int64);
begin
  if FPlayingTime = AValue then
    Exit;
  FPlayingTime := AValue;
end;

procedure cEmutecaPlayingStats.SetTimesPlayed(AValue: int64);
begin
  if FTimesPlayed = AValue then
    Exit;
  FTimesPlayed := AValue;
end;

procedure cEmutecaPlayingStats.AddPlayingTime(const Start: TDateTime;
  NumberOfSeconds: int64);
begin
  PlayingTime := PlayingTime + NumberOfSeconds;
  LastTime := Start;
  TimesPlayed := TimesPlayed + 1;
end;

procedure cEmutecaPlayingStats.WriteToIni(aIniFile: TCustomIniFile;
  const Section: string; ExportMode: boolean);
begin
  // TODO: Exception...
  if not assigned(aIniFile) then
    Exit;

  if ExportMode then
  begin
    aIniFile.DeleteKey(Section, krsIniKeyPlayingTime);
    aIniFile.DeleteKey(Section, krsIniKeyPlayingTime);
    aIniFile.DeleteKey(Section, krsIniKeyPlayingTime);
  end
  else
  begin
    aIniFile.WriteInt64(Section, krsIniKeyPlayingTime, PlayingTime);
    aIniFile.WriteInt64(Section, krsIniKeyTimesPlayed, TimesPlayed);
    aIniFile.WriteDateTime(Section, krsIniKeyLastTime, LastTime);
  end;
end;

procedure cEmutecaPlayingStats.LoadFromIni(aIniFile: TCustomIniFile;
  const Section: string);
var
  TmpString: string;
begin
  // TODO: Exception...
  if not assigned(aIniFile) then
    Exit;

  TmpString := aIniFile.ReadString(Section, krsIniKeyLastTime, '');
  if TmpString <> '' then
    LastTime := StrToDateTimeDef(TmpString, LastTime);
  TmpString := aIniFile.ReadString(Section, krsIniKeyPlayingTime, '');
  PlayingTime := StrToInt64Def(TmpString, PlayingTime);
  TmpString := aIniFile.ReadString(Section, krsIniKeyTimesPlayed, '');
  TimesPlayed := StrToInt64Def(TmpString, TimesPlayed);
end;

procedure cEmutecaPlayingStats.WriteToStrLst(aTxtFile: TStrings;
  ExportMode: boolean);
begin
  // TODO: Exception...
  if not assigned(aTxtFile) then
    Exit;

  if ExportMode then
  begin
    aTxtFile.Add('');
    aTxtFile.Add('');
    aTxtFile.Add('');
  end
  else
  begin
    aTxtFile.Add(DateTimeToStr(LastTime));
    aTxtFile.Add(IntToStr(TimesPlayed));
    aTxtFile.Add(IntToStr(PlayingTime));
  end;
end;

procedure cEmutecaPlayingStats.LoadFromStrLst(aTxtFile: TStrings;
  const NLine: integer);
begin
  // TODO: Exception...
  if not Assigned(aTxtFile) then
    Exit;

  if NLine + 3 > aTxtFile.Count then
    Exit;

  LastTime := StrToDateTimeDef(aTxtFile[Nline], LastTime);
  TimesPlayed := StrToInt64Def(aTxtFile[Nline + 1], TimesPlayed);
  PlayingTime := StrToInt64Def(aTxtFile[Nline + 2], PlayingTime);
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
