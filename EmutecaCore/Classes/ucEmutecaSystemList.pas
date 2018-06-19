{ List of systems of Emuteca.

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
unit ucEmutecaSystemList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8,
  // Emuteca classes
  ucEmutecaSystem;

type
  cEmutecaGenSystemList = specialize TFPGObjectList<cEmutecaSystem>;

  { cEmutecaSystemList }

  cEmutecaSystemList = class(cEmutecaGenSystemList)
  public
    procedure AssignToStrLst(aStrList: TStrings);
    function ItemById(aId: string): cEmutecaSystem;
    {< Returns the system with aId key.

       @Result cEmutecaSystem found or nil.
    }
  end;

implementation

{ cEmutecaSystemList }

procedure cEmutecaSystemList.AssignToStrLst(aStrList: TStrings);
var
  i: Integer;
  aSystem: cEmutecaSystem;
begin
   if not assigned(aStrList) then
    aStrList := TStringList.Create;

  aStrList.BeginUpdate;
  aStrList.Capacity := aStrList.Count + Count; // Speed up?
  i := 0;
  while i < Count do
  begin
    aSystem := Items[i];
    aStrList.AddObject(aSystem.Title, aSystem);
    Inc(i);
  end;
  aStrList.EndUpdate;
end;

function cEmutecaSystemList.ItemById(aId: string): cEmutecaSystem;
var
  i: integer;
  aSystem: cEmutecaSystem;
begin
  Result := nil;

  // Inverse search can be faster
  i := Count;
  while (not assigned(Result)) and (i > 0) do
  begin
    Dec(i);
    aSystem := Items[i];
    if aSystem.MatchID(aId) then
      Result := aSystem;
  end;
end;

end.

