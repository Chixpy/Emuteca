unit utETKGUICacheSoftIcons;
{< ctEGUICacheSoftIcons thread class unit of Emuteca GUI.

  ----

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
  Classes, SysUtils, Graphics, LazFileUtils,
  // CHX classes
  ucCHXImageList,
  // Emuteca Core units
  uEmutecaCommon,
  // Emuteca Core classes
  ucEmutecaSoftList, ucEmutecaSoftware;

type

  { ctEGUICacheSoftIcons }

  ctEGUICacheSoftIcons = class(TThread)
  private
    FIconList: cCHXImageList;
    FImageExt: TStrings;
    FSoftList: cEmutecaSoftList;
    FTempFolder: string;
    procedure SetIconList(AValue: cCHXImageList);
    procedure SetImageExt(AValue: TStrings);
    procedure SetSoftList(AValue: cEmutecaSoftList);
    procedure SetTempFolder(const aTempFolder: string);

  protected
    procedure Execute; override;

  public
    property ImageExt: TStrings read FImageExt write SetImageExt;
    property TempFolder: string read FTempFolder write SetTempFolder;
    property SoftList: cEmutecaSoftList read FSoftList write SetSoftList;
    property IconList: cCHXImageList read FIconList write SetIconList;

    constructor Create;
  end;

{< This thread loads software icons in background. }

implementation

{ ctEGUICacheSoftIcons }

procedure ctEGUICacheSoftIcons.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;
end;

procedure ctEGUICacheSoftIcons.SetImageExt(AValue: TStrings);
begin
  if FImageExt = AValue then
    Exit;
  FImageExt := AValue;
end;

procedure ctEGUICacheSoftIcons.SetSoftList(AValue: cEmutecaSoftList);
begin
  if FSoftList = AValue then
    Exit;
  FSoftList := AValue;
end;

procedure ctEGUICacheSoftIcons.SetTempFolder(const aTempFolder: string);
begin
  if FTempFolder = aTempFolder then
    Exit;
  FTempFolder := aTempFolder;
end;

procedure ctEGUICacheSoftIcons.Execute;
var
  i: integer;
  aSoft: cEmutecaSoftware;
  aIcon: TPicture;
  TempStr: string;
begin
  if not assigned(SoftList) then
    Exit;
  if not assigned(IconList) then
    Exit;

  i := 0;
  while (not Terminated) and (i < SoftList.Count) do
  begin
    aSoft := SoftList[i];

    if not Assigned(aSoft.Stats.Icon) then
    begin
      if aSoft.MatchGroupFile then
      begin
        if Terminated then
          Exit;
        aSoft.Stats.Icon := aSoft.CachedGroup.Stats.Icon;
      end
      else
      begin
        if Terminated then
          Exit;
        TempStr := EmuTKSearchFirstRelatedFile(aSoft.CachedSystem.IconFolder,
          aSoft.MediaFileName, ImageExt, True, True, TempFolder);

        if FileExistsUTF8(TempStr) then
        begin
          aIcon := IconList[IconList.AddImageFile(TempStr)];
          if Terminated then
            Exit;
          aSoft.Stats.Icon := aIcon;
        end
        else
        begin
          if Terminated then
            Exit;
          aSoft.Stats.Icon := aSoft.CachedGroup.Stats.Icon;
        end;
      end;
    end;

    Inc(i);
  end;
end;

constructor ctEGUICacheSoftIcons.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

end.
