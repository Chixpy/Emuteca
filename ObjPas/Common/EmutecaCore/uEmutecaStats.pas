{ This file is part of Emuteca Front End

  Copyright (C) 2006-2012 Chixpy

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

{ Unit of cGameStats object. }
unit uEmutecaStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils;

type

  { @name class.

    This class is the ancestor of cGame and cGroup, giving them
     some properties used for storing stats by cGameManager or
     other information that can be used by GUI as cache.

    @definitionList(
    @itemLabel(NOTE:)
    @item(Because PascalScript don't suport overloaded methods,
      we don't use them right here.)
    )
  }
  cGameStats = class(TObject)
  private
    FIconIndex: Integer;
    FLastTime: TDateTime;
    FPlayingTime: LongWord;
    FTimesPlayed: LongWord;
    procedure SetIconIndex(const AValue: Integer);
    procedure SetLastTime(const AValue: TDateTime);
    procedure SetPlayingTime(AValue: LongWord);
    procedure SetTimesPlayed(AValue: LongWord);

  public
    // Used by cGameManager (and by GUI as cache for cGroup ).
    // --------------------
    property LastTime: TDateTime read FLastTime write SetLastTime;
    {< Last time played the game.

      In cGroup maybe used to store the last time that a game
      has played from the group. }
    property TimesPlayed: LongWord read FTimesPlayed write SetTimesPlayed;
    {< Total times played the game. }
    property PlayingTime: LongWord read FPlayingTime write SetPlayingTime;
    {< Total seconds played. }

    // Only used by GUI
    // ----------------
    property IconIndex: Integer read FIconIndex write SetIconIndex;
    {< Index of the icon in a image list. }

    procedure IncTimesPlayed;
    {< Increment the times played by 1.

      Yes, it's a silly method.}
    procedure AddPlayingTime(const Stop: TDateTime; Start: TDateTime = 0);
    {< Add the seconds between two TDateTime to PlayingTime.}

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cGameStats }

procedure cGameStats.SetIconIndex(const AValue: Integer);
begin
  FIconIndex := AValue;
end;

procedure cGameStats.SetLastTime(const AValue: TDateTime);
begin
  FLastTime := AValue;
end;

procedure cGameStats.SetPlayingTime(AValue: LongWord);
begin
  FPlayingTime:=AValue;
end;

procedure cGameStats.SetTimesPlayed(AValue: LongWord);
begin
  FTimesPlayed := AValue;
end;

procedure cGameStats.IncTimesPlayed;
begin
  TimesPlayed := TimesPlayed + 1;
end;

procedure cGameStats.AddPlayingTime(const Stop: TDateTime; Start: TDateTime);
begin
  PlayingTime := PlayingTime + SecondsBetween(Stop, Start);
end;

constructor cGameStats.Create;
begin
  IconIndex := -1;
end;

destructor cGameStats.Destroy;
begin
  inherited Destroy;
end;

end.

