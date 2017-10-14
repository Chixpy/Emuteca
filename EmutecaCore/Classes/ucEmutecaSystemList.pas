unit ucEmutecaSystemList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8,
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

