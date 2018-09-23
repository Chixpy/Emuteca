unit ufETKGUIIcnSoftTree;
{< TfmETKGUIIcnSoftTree frame unit.

  ----

  This file is part of Emuteca GUI.

  Copyright (C) 2011-2018 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  IniFiles, VirtualTrees, LCLIntf, LCLType, ComCtrls, Menus,
  ActnList, LazUTF8,
  // CHX units
  uCHXImageUtils,
  // CHX clases
  ucCHXImageList,
  // Emuteca Core units
  uEmutecaConst,uEmutecaRscStr,uEmutecaCommon,
  // Emuteca Core abstracts
  uaEmutecaCustomSoft,
  // Emuteca Core clases
  ucEmutecaGroup, ucEmutecaSoftware,
  // Emuteca Core frames
  ufEmutecaSoftTree,
  // Emuteca GUI units
  uETKGUIConst, uETKGUIRscStr;

type

  { TfmETKGUIIcnSoftTree }

  TfmETKGUIIcnSoftTree = class(TfmEmutecaSoftTree)
    actOpenSoftFolder: TAction;

    procedure VDTDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);
    procedure VDTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    FDumpIconList: cCHXImageList;
    FZoneIconMap: cCHXImageMap;
    procedure SetDumpIconList(AValue: cCHXImageList);
    procedure SetZoneIconMap(AValue: cCHXImageMap);

  protected
    procedure SetIconColumnsWidth;

    procedure DoLoadGUIConfig(aIniFile: TIniFile); override;

  public
    property DumpIconList: cCHXImageList read FDumpIconList
      write SetDumpIconList;
    //< Icons of dump info.
    property ZoneIconMap: cCHXImageMap read FZoneIconMap write SetZoneIconMap;
    //< Icons for zones

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUIIcnSoftTree }

procedure TfmETKGUIIcnSoftTree.VDTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);

  procedure DrawGroupText(aGroup: cEmutecaGroup; TargetCanvas: TCanvas;
    Column: TColumnIndex; const CellText: string; const CellRect: TRect;
  var DefaultDraw: boolean);
  var
    IconRect: TRect;
    aIcon: TPicture;
  begin
    case Column of
      0: // System
      begin
        DefaultDraw := False;

        // Icon space
        IconRect := CellRect;
        IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

        aIcon := aGroup.CachedSystem.Stats.Icon;

        if assigned(aIcon) then
          TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
            aIcon.Graphic);

        // Don't draw text

        // Text space
        //  IconRect := CellRect;
        //  IconRect.Left := IconRect.Left + IconRect.Bottom -
        //  IconRect.Top + VST.TextMargin;

        // DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
        //  DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
        //  DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
      end;

      1: // Title
      begin
        DefaultDraw := False;

        // Icon space
        IconRect := CellRect;
        IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

        aIcon := aGroup.Stats.Icon;
        if assigned(aIcon) then
          TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
            aIcon.Graphic);

        // Text space
        IconRect := CellRect;
        IconRect.Left := IconRect.Left + IconRect.Bottom -
          IconRect.Top + VDT.TextMargin;

        DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
          DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
          DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
      end;
      else
        ;
    end;
  end;

  procedure DrawSoftText(aSoft: cEmutecaSoftware; TargetCanvas: TCanvas;
    Column: TColumnIndex; const CellText: string; const CellRect: TRect;
  var DefaultDraw: boolean);
  var
    IconRect: TRect;
    aIcon: TPicture;
    i: integer;
    TmpStr: string;
  begin
    case Column of
      0: // System
      begin
        DefaultDraw := False;

        // Draw system if group havs only one sofware.
        if cEmutecaGroup(aSoft.CachedGroup).SoftList.Count <> 1 then Exit;

        // Icon space
        IconRect := CellRect;
        IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

        aIcon := aSoft.CachedSystem.Stats.Icon;
        if assigned(aIcon) then
          TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
            aIcon.Graphic);
      end;

      1: // Title
      begin
        DefaultDraw := False;

        // Icon space
        IconRect := CellRect;
        IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

        aIcon := aSoft.Stats.Icon;
        if assigned(aIcon) then
          TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
            aIcon.Graphic);

        // Text space
        IconRect := CellRect;
        IconRect.Left := IconRect.Left + IconRect.Bottom -
          IconRect.Top + VDT.TextMargin;

        DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
          DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
          DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
      end;

      2: // Version
      begin
        if not assigned(ZoneIconMap) then
          Exit;

        DefaultDraw := False;

        // Icon space
        IconRect := CellRect;
        IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

        if not ZoneIconMap.TryGetData(aSoft.Zone, aIcon) then
          ZoneIconMap.TryGetData('', aIcon);
        if assigned(aIcon) then
          TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
            aIcon.Graphic);

        // Text space
        IconRect := CellRect;
        IconRect.Left := IconRect.Left + IconRect.Bottom -
          IconRect.Top + VDT.TextMargin;

        DrawText(TargetCanvas.Handle, PChar(aSoft.Version), -1, IconRect,
          DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
          DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
      end;

      5: // Flags
      begin
        if not assigned(DumpIconList) then
          Exit;

        DefaultDraw := False;

        IconRect := CellRect;
        IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

        // DumpStatus
        TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect,
          DumpIconList[Ord(aSoft.DumpStatus)]),
          DumpIconList[Ord(aSoft.DumpStatus)].Graphic);

        // Others
        for i := 0 to High(LazEmuTKDumpInfoIconFiles) do
        begin
          IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top;
          IconRect.Right := IconRect.Right + IconRect.Bottom - IconRect.Top;

          case i of
            0: // Fixed
              TmpStr := aSoft.Fixed;
            1: // Trainer
              TmpStr := aSoft.Trainer;
            2: // Translation;
              TmpStr := aSoft.Translation;
            3: // Pirate
              TmpStr := aSoft.Pirate;
            4: // Cracked
              TmpStr := aSoft.Cracked;
            5: // Modified
              TmpStr := aSoft.Modified;
            6: // Hack
              TmpStr := aSoft.Hack;
            else
              TmpStr := '';
          end;

          // Draw icon
          if (TmpStr <> '') then
          begin
            TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect,
              DumpIconList[i + Length(EmutecaDumpStatusStrK)]), DumpIconList[i + Length(EmutecaDumpStatusStrK)].Graphic);

            // Some magic
            case i of
              2: // Translation;
              begin
                if (TmpStr[1] = '+') then
                  TmpStr := Trim(UTF8Copy(TmpStr, 2, 3))
                else  // GoodXXX or "-" TOSEC
                  TmpStr := Trim(UTF8Copy(TmpStr, 1, 3));

                //Drawing text over icon
                DrawText(TargetCanvas.Handle, PChar(TmpStr), -1, IconRect,
                  DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
                  DT_EDITCONTROL or DT_CENTER);
              end;
              else
                ;
            end;
          end;
        end;
      end;
      else
        DefaultDraw := True;
    end;

  end;

var
  pData: ^TObject;
begin
  pData := Sender.GetNodeData(Node);
  if not Assigned(pData) then Exit;
  if not Assigned(pData^) then Exit;

  if pData^ is cEmutecaGroup then
  begin
    DrawGroupText(cEmutecaGroup(pData^), TargetCanvas, Column,
      CellText, CellRect, DefaultDraw);
  end
  else if pData^ is cEmutecaSoftware then
  begin
    DrawSoftText(cEmutecaSoftware(pData^), TargetCanvas, Column,
      CellText, CellRect, DefaultDraw);
  end;
end;

procedure TfmETKGUIIcnSoftTree.VDTKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;

  SetIconColumnsWidth;
end;

procedure TfmETKGUIIcnSoftTree.SetDumpIconList(AValue: cCHXImageList);
begin
  if FDumpIconList = AValue then
    Exit;
  FDumpIconList := AValue;
end;

procedure TfmETKGUIIcnSoftTree.SetZoneIconMap(AValue: cCHXImageMap);
begin
  if FZoneIconMap = AValue then
    Exit;
  FZoneIconMap := AValue;
end;

procedure TfmETKGUIIcnSoftTree.SetIconColumnsWidth;
var
  TempOptions: TVTColumnOptions;
begin
  // Set Width of icon columns
  // System
  VDT.Header.Columns[0].Width :=
    VDT.DefaultNodeHeight + VDT.Header.Columns[0].Spacing * 2 +
    VDT.Header.Columns[0].Margin * 2;
  TempOptions := VDT.Header.Columns[0].Options;
  Exclude(TempOptions, coResizable);
  VDT.Header.Columns[0].Options := TempOptions;

  // DumpStatus
  VDT.Header.Columns[5].Width :=
    VDT.DefaultNodeHeight * 8 + VDT.Header.Columns[5].Spacing *
    2 + VDT.Header.Columns[5].Margin * 2;
  TempOptions := VDT.Header.Columns[5].Options;
  Exclude(TempOptions, coResizable);
  VDT.Header.Columns[5].Options := TempOptions;
end;

procedure TfmETKGUIIcnSoftTree.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  inherited DoLoadGUIConfig(aIniFile);

  // Restoring icons columns to its fixed size
  SetIconColumnsWidth;
end;

constructor TfmETKGUIIcnSoftTree.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SetIconColumnsWidth;
end;

destructor TfmETKGUIIcnSoftTree.Destroy;
begin
  inherited Destroy;
end;

end.
