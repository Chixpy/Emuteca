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
  Classes, SysUtils, dateutils, IniFiles, uCHXStrUtils;

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
    FPlayingTime: longword;
    FTimesPlayed: longword;
    procedure SetIconIndex(AValue: Integer);
    procedure SetLastTime(AValue: TDateTime);
    procedure SetPlayingTime(AValue: longword);
    procedure SetTimesPlayed(AValue: longword);

  public
    procedure AddPlayingTime(const Stop: TDateTime; Start: TDateTime = 0);
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
    property TimesPlayed: longword read FTimesPlayed write SetTimesPlayed;
    {< Total times played. }
    property PlayingTime: longword read FPlayingTime write SetPlayingTime;
    {< Total seconds played. }

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

procedure cEmutecaPlayingStats.SetPlayingTime(AValue: longword);
begin
  if FPlayingTime = AValue then
    Exit;
  FPlayingTime := AValue;
end;

procedure cEmutecaPlayingStats.SetTimesPlayed(AValue: longword);
begin
  if FTimesPlayed = AValue then
    Exit;
  FTimesPlayed := AValue;
end;

procedure cEmutecaPlayingStats.AddPlayingTime(const Stop: TDateTime;
  Start: TDateTime);
begin
  PlayingTime := PlayingTime + SecondsBetween(Stop, Start);
  LastTime := Stop;
  TimesPlayed := TimesPlayed + 1;
end;

procedure cEmutecaPlayingStats.WriteToIni(aIniFile: TCustomIniFile;
  const Section: string);
begin
  // TODO: Exception...
  if not assigned(aIniFile) then
    Exit;

  aIniFile.WriteString(Section, krsIniKeyPlayingTime, IntToStr(PlayingTime));
  aIniFile.WriteString(Section, krsIniKeyTimesPlayed, IntToStr(TimesPlayed));
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
  PlayingTime := StrToCardinalDef(TmpString, PlayingTime);
  TmpString := aIniFile.ReadString(Section, krsIniKeyTimesPlayed, '');
  TimesPlayed := StrToCardinalDef(TmpString, TimesPlayed);
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
