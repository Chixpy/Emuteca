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
unit ucEmutecaSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8,
  ucEmutecaSoftware;

type

  cEmutecaGenSoftList = specialize TFPGObjectList<cEmutecaSoftware>;

  { cEmutecaSoftList }

  cEmutecaSoftList = class(cEmutecaGenSoftList)
  public
    function ItemById(aId: string): cEmutecaSoftware;
    {< Returns the software with aId key.

       @Result cEmutecaSoftware found or nil.
    }
  end;

// Functions for sorting lists
function EmutecaCompareSoftByID(const aSoft1, aSoft2: cEmutecaSoftware): integer;
function EmutecaCompareSoftByGroupKey(const aSoft1, aSoft2: cEmutecaSoftware): integer;


implementation

function EmutecaCompareSoftByID(const aSoft1, aSoft2: cEmutecaSoftware
  ): integer;
begin
  Result := aSoft1.CompareID(aSoft2.ID);
end;

function EmutecaCompareSoftByGroupKey(const aSoft1, aSoft2: cEmutecaSoftware
  ): integer;
begin
  Result := aSoft1.CompareGroupKey(aSoft2.GroupKey);
end;

{ cEmutecaSoftList }

function cEmutecaSoftList.ItemById(aId: string): cEmutecaSoftware;
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  Result := nil;

  // Inverse search can be faster
  i := Count;
  while (not assigned(Result)) and (i > 0) do
  begin
    Dec(i);
    aSoft := Items[i];
    if aSoft.MatchID(aId) then
      Result := aSoft;
  end;
end;

end.
