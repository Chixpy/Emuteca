unit ucEmutecaGameTag;
{< cEmutecaTag class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2018 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { cEmutecaTag }

  cEmutecaTag = class(TComponent)
  private
    FDescription: string;
    FTitle: string;
    procedure SetDescription(AValue: string);
    procedure SetTitle(AValue: string);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Title: string read FTitle write SetTitle;
    {< Name of the tag or tag group. }
    property Description: string read FDescription write SetDescription;
    {< Description. }
  end;

implementation

{ cEmutecaTag }

procedure cEmutecaTag.SetDescription(AValue: string);
begin
  if FDescription = AValue then
    Exit;
  FDescription := AValue;
end;

procedure cEmutecaTag.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

constructor cEmutecaTag.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor cEmutecaTag.Destroy;
begin
  inherited Destroy;
end;

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
