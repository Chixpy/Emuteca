unit ucEmutecaPlayingStats;

{ cEmutecaPlayingStats class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils, IniFiles, Graphics,
  // CHX units
  uCHXStrUtils,
  // Emuteca Core units.
  uEmutecaConst, uEmutecaRscStr;

type

  { cEmutecaPlayingStats class. }

  cEmutecaPlayingStats = class(TComponent)
  private
    FIcon: TPicture;
    FLastTime: TDateTime;
    FPlayingTime: int64;
    FSysSoftIcon: TPicture;
    FTimesPlayed: int64;
    procedure SetIcon(AValue: TPicture);
    procedure SetLastTime(AValue: TDateTime);
    procedure SetPlayingTime(AValue: int64);
    procedure SetSysSoftIcon(AValue: TPicture);
    procedure SetTimesPlayed(AValue: int64);

  public
    procedure AddPlayingTime(const Start: TDateTime; NumberOfSeconds: int64);
    {< Adds the seconds between two TDateTime to PlayingTime.

       Additionally adds 1 to TimesPlayed counter and updates LastTime
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

    function LastTimeStr: string;
    {< Returns LastTime as string. }
    function TimesPlayedStr: string;
    {< Returns TimesPlayed as string. }
    function PlayingTimeStr: string;
    {< Returns PlayingTime as string. }

    // TODO: This must be stored in another place...
    property Icon: TPicture read FIcon write SetIcon;
    property SysSoftIcon: TPicture read FSysSoftIcon write SetSysSoftIcon;
  end;

  {< This class is for storing stats (System, Emulator, Parent and Soft).
  }

implementation

{ cEmutecaPlayingStats }

procedure cEmutecaPlayingStats.SetLastTime(AValue: TDateTime);
begin
  if FLastTime = AValue then
    Exit;
  FLastTime := AValue;
end;

procedure cEmutecaPlayingStats.SetIcon(AValue: TPicture);
begin
  if FIcon = AValue then
    Exit;
  FIcon := AValue;
end;

procedure cEmutecaPlayingStats.SetPlayingTime(AValue: int64);
begin
  if FPlayingTime = AValue then
    Exit;
  FPlayingTime := AValue;
end;

procedure cEmutecaPlayingStats.SetSysSoftIcon(AValue: TPicture);
begin
  if FSysSoftIcon = AValue then
    Exit;
  FSysSoftIcon := AValue;
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
    aIniFile.DeleteKey(Section, krsIniKeyLastTime);
    aIniFile.DeleteKey(Section, krsIniKeyTimesPlayed);
    aIniFile.DeleteKey(Section, krsIniKeyPlayingTime);
  end
  else
  begin
    aIniFile.WriteDateTime(Section, krsIniKeyLastTime, LastTime);
    aIniFile.WriteInt64(Section, krsIniKeyTimesPlayed, TimesPlayed);
    aIniFile.WriteInt64(Section, krsIniKeyPlayingTime, PlayingTime);
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

  LastTime := aIniFile.ReadDateTime(Section, krsIniKeyLastTime, LastTime);
  PlayingTime := aIniFile.ReadInt64(Section, krsIniKeyPlayingTime,
    PlayingTime);
  TimesPlayed := aIniFile.ReadInt64(Section, krsIniKeyTimesPlayed,
    TimesPlayed);
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
    aTxtFile.Add(TimesPlayed.ToString);
    aTxtFile.Add(IntToStr(PlayingTime));
  end;
end;

procedure cEmutecaPlayingStats.LoadFromStrLst(aTxtFile: TStrings;
  const NLine: integer);
var
  aStr: string;
begin
  // TODO: Exception...
  if not Assigned(aTxtFile) then
    Exit;

  if NLine + 3 > aTxtFile.Count then
    Exit;

  aStr := aTxtFile[Nline];
  if (aStr <> '') and (aStr <> krsImportKeepValueKey) then
    LastTime := StrToDateTimeDef(aStr, LastTime);
  aStr := aTxtFile[Nline + 1];
  if (aStr <> '') and (aStr <> krsImportKeepValueKey) then
    TimesPlayed := StrToInt64Def(aStr, TimesPlayed);
  aStr := aTxtFile[Nline + 2];
  if (aStr <> '') and (aStr <> krsImportKeepValueKey) then
    PlayingTime := StrToInt64Def(aStr, PlayingTime);
end;

constructor cEmutecaPlayingStats.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cEmutecaPlayingStats.Destroy;
begin
  inherited Destroy;
end;

function cEmutecaPlayingStats.LastTimeStr: string;
begin
  if LastTime = 0 then
    Result := rsNever
  else
    Result := DateTimeToStr(LastTime);
end;

function cEmutecaPlayingStats.TimesPlayedStr: string;
begin
  Result := IntToStr(TimesPlayed);
end;

function cEmutecaPlayingStats.PlayingTimeStr: string;
begin
  Result := SecondsToFmtStr(PlayingTime);
end;

initialization
  RegisterClass(cEmutecaPlayingStats);

finalization
  UnRegisterClass(cEmutecaPlayingStats);
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
