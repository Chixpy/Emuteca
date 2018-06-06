{ Script Engine.

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
unit utETKGUICacheGrpIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LazFileUtils,
  // CHX classes
  ucCHXImageList,
  // Emuteca classes
  ucEmutecaGroupList, ucEmutecaGroup, ucEmutecaSoftware;

type

  { ctEGUICacheGrpIcons

    This Thread loads group icons in background.
  }

  ctEGUICacheGrpIcons = class(TThread)
  private
    FGroupList: cEmutecaGroupList;
    FIconList: cCHXImageList;
    FImageExt: TStrings;
    procedure SetGroupList(AValue: cEmutecaGroupList);
    procedure SetIconList(AValue: cCHXImageList);
    procedure SetImageExt(AValue: TStrings);

  protected
    procedure Execute; override;

  public
    property ImageExt: TStrings read FImageExt write SetImageExt;
    property GroupList: cEmutecaGroupList read FGroupList write SetGroupList;
    property IconList: cCHXImageList read FIconList write SetIconList;

    constructor Create;
  end;

implementation

{ ctEGUICacheGrpIcons }

procedure ctEGUICacheGrpIcons.SetGroupList(AValue: cEmutecaGroupList);
begin
  if FGroupList = AValue then
    Exit;
  FGroupList := AValue;
end;

procedure ctEGUICacheGrpIcons.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;
end;

procedure ctEGUICacheGrpIcons.SetImageExt(AValue: TStrings);
begin
  if FImageExt = AValue then
    Exit;
  FImageExt := AValue;
end;

procedure ctEGUICacheGrpIcons.Execute;
var
  i: integer;
  aGroup: cEmutecaGroup;
  aSoft: cEmutecaSoftware;
  aIcon: TPicture;
  TempStr: string;
begin
  if not assigned(GroupList) then
    Exit;
  if not assigned(IconList) then
    Exit;

  i := 0;
  while (not Terminated) and (i < GroupList.Count) do
  begin
    aGroup := GroupList[i];

    aIcon := aGroup.Stats.Icon;
    begin
      TempStr := aGroup.SearchFirstRelatedFile(aGroup.CachedSystem.IconFolder,
        ImageExt, True, True);

      if FileExistsUTF8(TempStr) then
      begin
        aIcon := IconList[IconList.AddImageFile(TempStr)];
      end
      else
      begin
        if Terminated then
          Exit;
        aIcon := aGroup.CachedSystem.Stats.SysSoftIcon;
      end;

      if Terminated then
        Exit;
      aGroup.Stats.Icon := aIcon;

      // If group have only one software, search its icon.
      // ufETKGUIIcnSoftTree shows straight its child.
      if aGroup.SoftList.Count = 1 then
      begin
        aSoft := aGroup.SoftList[0];

        if not Assigned(aSoft.Stats.Icon) then
        begin
          if aSoft.MatchGroupFile then
          begin
            // Same icon as group
            if Terminated then
              Exit;
            aSoft.Stats.Icon := aGroup.Stats.Icon;
          end
          else
          begin

            if Terminated then
              Exit;
            TempStr := aSoft.SearchFirstRelatedFile(aSoft.CachedSystem.IconFolder,
              ImageExt, True, True);

            if FileExistsUTF8(TempStr) then
            begin
              aIcon := IconList[IconList.AddImageFile(TempStr)];

            end
            else
            begin
              aIcon := aSoft.CachedGroup.Stats.Icon;
            end;

            if Terminated then
              Exit;
            aSoft.Stats.Icon := aIcon;

          end;

        end;


      end;
    end;

    Inc(i);
  end;
end;

constructor ctEGUICacheGrpIcons.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

end.
