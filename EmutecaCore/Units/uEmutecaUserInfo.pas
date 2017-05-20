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

{ cEmutecaUserInfo unit.

  Component storing user info about games.
}
unit uEmutecaUserInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { cEmutecaUserInfo }

  cEmutecaUserInfo = class(TComponent)
  private
    FFinished: boolean;
    FOwned: boolean;
    FRating: integer;
    procedure SetFinished(AValue: boolean);
    procedure SetOwned(AValue: boolean);
    procedure SetRating(AValue: integer);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Rating: integer read FRating write SetRating;
    property Finished: boolean read FFinished write SetFinished;
    property Owned: boolean read FOwned write SetOwned;

  end;

implementation

{ cEmutecaUserInfo }

procedure cEmutecaUserInfo.SetFinished(AValue: boolean);
begin
  if FFinished = AValue then
    Exit;
  FFinished := AValue;
end;

procedure cEmutecaUserInfo.SetOwned(AValue: boolean);
begin
  if FOwned = AValue then
    Exit;
  FOwned := AValue;
end;

procedure cEmutecaUserInfo.SetRating(AValue: integer);
begin
  if FRating = AValue then
    Exit;
  FRating := AValue;
end;

constructor cEmutecaUserInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor cEmutecaUserInfo.Destroy;
begin
  inherited Destroy;
end;

end.
