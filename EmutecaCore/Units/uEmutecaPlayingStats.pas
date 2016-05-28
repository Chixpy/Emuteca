{ This file is part of Emuteca

  Copyright (C) 2006-2013 Chixpy

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
unit uEmutecaPlayingStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils, IniFiles, uCHXStrUtils;

type

  { cEmutecaPlayingStats class.

    This class is for storing stats by cGameManager and
     other information that can be used by GUI as cache.

    @definitionList(
      @itemLabel(NOTE:)
      @item(Because PascalScript don't suport overloaded methods,
        we don't use them right here.)
      )
  }

  cEmutecaPlayingStats = class(TComponent)
  private
    FIconIndex: integer;
    FLastTime: TDateTime;
    FPlayingTime: longword;
    FTimesPlayed: longword;
    procedure SetIconIndex(AValue: integer);
    procedure SetLastTime(AValue: TDateTime);
    procedure SetPlayingTime(AValue: longword);
    procedure SetTimesPlayed(AValue: longword);

  public
    // Only used by GUI
    // ----------------
    { TODO -oChixpy : Move these elsewhere... }
    property IconIndex: integer read FIconIndex write SetIconIndex;
    {< Index of the icon in a image list. }

    procedure Init;
    {< }
    procedure AddPlayingTime(const Stop: TDateTime; Start: TDateTime = 0);
    {< Adds the seconds between two TDateTime to PlayingTime.

       Aditionally adds 1 to TimesPlayed counter and updates LastTime
    }

    procedure WriteToIni(aIniFile: TCustomIniFile; const Section: string);
    procedure LoadFromIni(aIniFile: TCustomIniFile; const Section: string);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    // Used by cGameManager (and by GUI as cache for cGroup ).
    // -------------------------------------------------------
    property LastTime: TDateTime read FLastTime write SetLastTime;
      {< Last time played the game.

        In cGroup maybe used to store the last time that a game
        has played from the group. }
    property TimesPlayed: longword read FTimesPlayed write SetTimesPlayed;
    {< Total times played the game. }
    property PlayingTime: longword read FPlayingTime write SetPlayingTime;
    {< Total seconds played. }
  end;

implementation

{ cEmutecaPlayingStats }

procedure cEmutecaPlayingStats.SetIconIndex(AValue: integer);
begin
  if FIconIndex = AValue then
    Exit;
  FIconIndex := AValue;
end;

procedure cEmutecaPlayingStats.SetLastTime(AValue: TDateTime);
begin
  if FLastTime = AValue then
    Exit;
  FLastTime := AValue;
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

procedure cEmutecaPlayingStats.Init;
begin
  IconIndex := -1;
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

  aIniFile.WriteString(Section, 'PlayingTime', IntToStr(PlayingTime));
  aIniFile.WriteString(Section, 'TimesPlayed', IntToStr(TimesPlayed));
  aIniFile.WriteDateTime(Section, 'LastTime', LastTime);
end;

procedure cEmutecaPlayingStats.LoadFromIni(aIniFile: TCustomIniFile;
  const Section: string);
var
  TmpString: string;
begin
  TmpString := aIniFile.ReadString(Section, 'LastTime', '');
  if TmpString <> '' then
    LastTime := StrToDateTimeDef(TmpString, LastTime);
  TmpString := aIniFile.ReadString(Section, 'PlayingTime', '');
  PlayingTime := StrToCardinalDef(TmpString, PlayingTime);
  TmpString := aIniFile.ReadString(Section, 'TimesPlayed', '');
  TimesPlayed := StrToCardinalDef(TmpString, TimesPlayed);
end;

constructor cEmutecaPlayingStats.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Init;
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
