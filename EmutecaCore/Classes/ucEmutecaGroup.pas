unit ucEmutecaGroup;
{< cEmutecaGroup class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2018 Chixpy

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
  Classes, SysUtils, LazFileUtils, LazUTF8,
  // Emuteca Core abstracts
  uaEmutecaCustomSystem, uaEmutecaCustomGroup,
  // Emuteca Core classes
  ucEmutecaSoftList;

type
  { cEmutecaGroup }

  cEmutecaGroup = class(caEmutecaCustomGroup)
  private
    FCachedSystem: caEmutecaCustomSystem;
    FSoftList: cEmutecaSoftList;
    procedure SetCachedSystem(AValue: caEmutecaCustomSystem);

  public
    property SoftList: cEmutecaSoftList read FSoftList;

    property CachedSystem: caEmutecaCustomSystem
      read FCachedSystem write SetCachedSystem;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
  end;

  TEmutecaReturnGroupCB = procedure(aGroup: cEmutecaGroup) of
    object;

implementation

{ cEmutecaGroup }

procedure cEmutecaGroup.SetCachedSystem(AValue: caEmutecaCustomSystem);
begin
  if FCachedSystem = AValue then
    Exit;
  FCachedSystem := AValue;
end;

constructor cEmutecaGroup.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSoftList := cEmutecaSoftList.Create(False);
end;

destructor cEmutecaGroup.Destroy;
begin
  SoftList.Free;
  inherited Destroy;
end;


initialization
  RegisterClass(cEmutecaGroup);

finalization
  UnRegisterClass(cEmutecaGroup);

end.
