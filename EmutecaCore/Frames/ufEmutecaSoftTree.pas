unit ufEmutecaSoftTree;
{< TfmEmutecaSoftTree frame unit.

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
  Classes, SysUtils, FileUtil, VirtualTrees, VTHeaderPopup, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ActnList, Menus, LazUTF8, LCLType,
  LazFileUtils, IniFiles,
  // CHX units
  uCHXImageUtils,
  // CHX frames
  ufCHXFrame,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core abstract
  uaEmutecaCustomSoft,
  // Emuteca Core classes
  ucEmutecaGroupList, ucEmutecaGroup, ucEmutecaSoftware;

const
  krsIniSoftTreeSection = 'SoftTree';
  {< Config file section name. }
  krsIniSoftTreeWidthFmt = 'Column%0:d_Width';
  {< Config file columns width key. }
  krsIniSoftTreeVisibleFmt = 'Column%0:d_Visible';
  {< Config file columns visibility key. }
  krsIniSoftTreePositionFmt = 'Column%0:d_Position';
  {< Config file columns position key. }
  krsIniSoftTreeGroupFont = 'GroupFont';
  krsIniSoftTreeSoftFont = 'SoftFont';

type

  { TfmEmutecaSoftTree }

  TfmEmutecaSoftTree = class(TfmCHXFrame)
    actRunSoftware: TAction;
    actMergeGroupFiles: TAction;
    miPMSRunSoftware: TMenuItem;
    miPMGMergeGroupFiles: TMenuItem;
    StatusBar: TStatusBar;
    VDT: TVirtualStringTree;
    VTHPopupMenu: TVTHeaderPopupMenu;
    procedure VDTAfterItemPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
    procedure VDTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VDTCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure VDTDblClick(Sender: TObject);
    procedure VDTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure VDTGetPopupMenu(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
      var AskParent: boolean; var aPopupMenu: TPopupMenu);
    procedure VDTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VDTInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: cardinal);
    procedure VDTInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VDTKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);

  private
    FGroupList: cEmutecaGroupList;
    FGUIConfigFile: string;
    FIconsIniFile: string;
    FOnDblClkGroup: TEmutecaReturnGroupCB;
    FOnDblClkSoft: TEmutecaSoftCB;
    FOnSelectGroup: TEmutecaReturnGroupCB;
    FOnSelectSoft: TEmutecaSoftCB;
    FpmGroup: TPopupMenu;
    FpmSoft: TPopupMenu;
    procedure SetGroupList(AValue: cEmutecaGroupList);
    procedure SetGUIConfigFile(AValue: string);
    procedure SetIconsIniFile(AValue: string);
    procedure SetOnDblClkGroup(AValue: TEmutecaReturnGroupCB);
    procedure SetOnDblClkSoft(AValue: TEmutecaSoftCB);
    procedure SetOnSelectGroup(AValue: TEmutecaReturnGroupCB);
    procedure SetOnSelectSoft(AValue: TEmutecaSoftCB);
    procedure SetpmGroup(AValue: TPopupMenu);
    procedure SetpmSoft(AValue: TPopupMenu);

  protected
    procedure UpdateSBNodeCount;
    {< Updates the Status Bar visible node count. }

    procedure SetNodesHeight(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: boolean);
    {< Callback for IterateSubtree, sets node height to default (changed) one.}

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoLoadGUIConfig(aIniFile: TIniFile); virtual;
    procedure DoSaveGUIConfig(aIniFile: TIniFile); virtual;

  public
    property GroupList: cEmutecaGroupList read FGroupList write SetGroupList;
    {< GroupList to show. }

    // Callbacks on actions
    // --------------------
    property OnSelectGroup: TEmutecaReturnGroupCB
      read FOnSelectGroup write SetOnSelectGroup;
    {< Callback when a group is selected. }
    property OnDblClkGroup: TEmutecaReturnGroupCB
      read FOnDblClkGroup write SetOnDblClkGroup;
    {< Callback when a group is double clicked.

      Automatically group childs are expanded. }
    property OnSelectSoft: TEmutecaSoftCB
      read FOnSelectSoft write SetOnSelectSoft;
    {< Callback when a software is selected. }
    property OnDblClkSoft: TEmutecaSoftCB
      read FOnDblClkSoft write SetOnDblClkSoft;
    {< Callback when a software is double clicked. }

    // Menu popups
    property pmSoft: TPopupMenu read FpmSoft write SetpmSoft;
    {< PopUp menu used when RClick on a software. }
    property pmGroup: TPopupMenu read FpmGroup write SetpmGroup;
    {< PopUp menu used when RClick on a group. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {< Example frame for Emuteca Core to show a group list.

    It uses a VirtualTreeView (TVirtualStringTree) a base component.

    It show a tree structure with group-soft relationship, unless a group
      only have one software with it's showed straight.

    With TitleFilter property, nodes not matching are hidden, empty string will
      show all.

    OnSelectX are callbacks functions for some common events.

    pmX are PopUp menus showed when right click on an item.

    CTRL + + and CTRL + - change row's height.

    It autoloads/saves columns order, witdh, visibility; font properties; etc.
  }

implementation

{$R *.lfm}

{ TfmEmutecaSoftTree }

procedure TfmEmutecaSoftTree.VDTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

  procedure GetGroupText(aGroup: cEmutecaGroup; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string);
  begin
    case TextType of
      ttNormal:
      begin
        case Column of
          0: // System
            CellText := aGroup.CachedSystem.Title;
          1: // Name
            CellText := aGroup.Title;
          2: // N. Versions
            CellText := Format(rsFmtNVersions, [aGroup.SoftList.Count]);
          3: // Developer
            CellText := aGroup.Developer;
          4: // Year
            CellText := aGroup.Year;
          5: // Flags
            CellText := '';
          6: // Times
            CellText := aGroup.Stats.TimesPlayedStr;
          7: // Total time
            CellText := aGroup.Stats.PlayingTimeStr;
          8: // Last time
            CellText := aGroup.Stats.LastTimeStr;
          9: // File
            CellText := aGroup.MediaFileName;
          10: // Folder
            CellText := '';
          else
            ;
        end;
      end;
      else // ttStatic
        ;
    end;
  end;


  procedure GetSoftText(aSoft: cEmutecaSoftware; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string);
  begin
    case TextType of
      ttNormal:
      begin
        case Column of
          0: // System
            CellText := aSoft.CachedSystem.Title;
          1: // Title
            CellText := aSoft.Title;
          2: // Version
            CellText := aSoft.Zone + ' ' + aSoft.Version;
          3: // Publisher
          begin
            if aSoft.Publisher = '' then
            begin
              if assigned(aSoft.CachedGroup) and
                (aSoft.CachedGroup.Developer <> '') then
                CellText := '(' + aSoft.CachedGroup.Developer + ')'
              else
                CellText := '';
            end
            else
              CellText := aSoft.Publisher;
          end;
          4: // Year
          begin
            if aSoft.Year = '' then
            begin
              if assigned(aSoft.CachedGroup) and
                (aSoft.CachedGroup.Year <> '') then
                CellText := '(' + aSoft.CachedGroup.Year + ')'
              else
                CellText := '';
            end
            else
              CellText := aSoft.Year;
          end;
          5: // Flags
          begin
            // A simple formated output, to be overriden
            CellText := DumpSt2Str(aSoft.DumpStatus);

            if aSoft.DumpInfo <> '' then
              CellText += ' [' + aSoft.DumpInfo + ']';

            if aSoft.Fixed <> '' then
              CellText += ' [f ' + aSoft.Fixed + ']';

            if aSoft.Trainer <> '' then
              CellText += ' [t ' + aSoft.Trainer + ']';

            if aSoft.Translation <> '' then
              CellText += ' [tr ' + aSoft.Translation + ']';

            if aSoft.Pirate <> '' then
              CellText += ' [p ' + aSoft.Pirate + ']';

            if aSoft.Cracked <> '' then
              CellText += ' [cr ' + aSoft.Cracked + ']';

            if aSoft.Modified <> '' then
              CellText += ' [m ' + aSoft.Modified + ']';

            if aSoft.Hack <> '' then
              CellText += ' [t ' + aSoft.Hack + ']';
          end;
          6: // Times Played
            CellText := aSoft.Stats.TimesPlayedStr;
          7: // Playing Time
            CellText := aSoft.Stats.PlayingTimeStr;
          8: // Last Time
            CellText := aSoft.Stats.LastTimeStr;
          9: // File
            CellText := aSoft.FileName;
          10: // Folder
            CellText := aSoft.Folder;
          else
            ;
        end;
      end;
      else // ttStatic
        ;
    end;
  end;

var
  pData: ^TObject;
begin
  pData := Sender.GetNodeData(Node);
  if not Assigned(pData) then
    Exit;
  if not Assigned(pData^) then
    Exit;

  if pData^ is cEmutecaGroup then
  begin
    GetGroupText(cEmutecaGroup(pData^), Column, TextType, CellText);
  end
  else if pData^ is cEmutecaSoftware then
  begin
    GetSoftText(cEmutecaSoftware(pData^), Column, TextType, CellText);
  end;
end;

procedure TfmEmutecaSoftTree.VDTInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: cardinal);
var
  pGroup: ^cEmutecaGroup;
begin
  if not Assigned(Node) then
    Exit;
  pGroup := Sender.GetNodeData(Node);
  ChildCount := pGroup^.SoftList.Count;
end;

procedure TfmEmutecaSoftTree.VDTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  pGroup: ^cEmutecaGroup;
  pSoft: ^cEmutecaSoftware;
begin
  if not Assigned(ParentNode) then
  begin
    if (GroupList[Node^.Index].SoftList.Count = 1) then
    begin
      // If group has one game, list it
      pSoft := Sender.GetNodeData(Node);
      pSoft^ := GroupList[Node^.Index].SoftList[0];
    end
    else
    begin
      // Nodo base -> grupo
      pGroup := Sender.GetNodeData(Node);
      pGroup^ := GroupList[Node^.Index];
      Include(InitialStates, ivsHasChildren);
    end;
  end
  else
  begin
    pGroup := Sender.GetNodeData(ParentNode);
    pSoft := Sender.GetNodeData(Node);
    pSoft^ := pGroup^.SoftList[Node^.Index];
  end;
end;

procedure TfmEmutecaSoftTree.VDTKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  Tree: TVirtualStringTree;
  // tmpGroupList: cEmutecaGroupList;
begin
  if not Assigned(Sender) then
    Exit;

  Tree := TVirtualStringTree(Sender);
  if (Shift = [ssCtrl]) then //CTRL key down
  begin
    if (Key = VK_ADD) then //"+" (plus) key down
    begin
      Key := 0; //so no FHeader.AutoFitColumns from TBaseVirtualTree.WMKeyDown
      Tree.BeginUpdate;
      Tree.Font.Height := abs(Tree.Font.Height) + 1;
      Tree.DefaultNodeHeight := Tree.Font.Height;
      // This way is faster, but it don't keep selected item:
      // tmpGroupList := GroupList;
      // GroupList := nil;
      // GroupList := tmpGroupList;
      Tree.IterateSubtree(nil, @SetNodesHeight, nil);
      Tree.EndUpdate;
    end;
    if (Key = VK_SUBTRACT) then //"-" (minus) key down
    begin
      Key := 0; //so no FHeader.RestoreColumns from TBaseVirtualTree.WMKeyDown
      if (abs(Tree.Font.Height) > 8) then // Minimal size...
      begin
        Tree.BeginUpdate;
        Tree.Font.Height := abs(Tree.Font.Height) - 1;
        Tree.DefaultNodeHeight := Tree.Font.Height;
        // This way is faster, but it don't keep selected item:
        // tmpGroupList := GroupList;
        // GroupList := nil;
        // GroupList := tmpGroupList;
        Tree.IterateSubtree(nil, @SetNodesHeight, nil);
        Tree.EndUpdate;
      end;
    end;
  end; //CTRL
end;

procedure TfmEmutecaSoftTree.VDTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pData: ^TObject;
  aSoft: cEmutecaSoftware;
  aGroup: cEmutecaGroup;
begin
  pData := Sender.GetNodeData(Node);
  if not assigned(pData) then
  begin
    StatusBar.Panels[1].Text := '';
    OnSelectSoft(nil);
    Exit;
  end;

  if pData^ is cEmutecaSoftware then
  begin
    aSoft := cEmutecaSoftware(pData^);
    StatusBar.Panels[1].Text := aSoft.Folder + aSoft.FileName;
    if Assigned(OnSelectSoft) then
      OnSelectSoft(aSoft);
  end
  else
  begin
    if pData^ is cEmutecaGroup then
      aGroup := cEmutecaGroup(pData^);
    StatusBar.Panels[1].Text := aGroup.ID;
    if Assigned(OnSelectGroup) then
      OnSelectGroup(aGroup);
  end;
end;

procedure TfmEmutecaSoftTree.VDTAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  //if Node^.Parent <> Sender.RootNode then Exit;
  //TargetCanvas.Line(ItemRect.Left, ItemRect.Top,ItemRect.Right, ItemRect.Top);
end;

procedure TfmEmutecaSoftTree.VDTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);

  function CompareGroups(aGroup1, aGroup2: cEmutecaGroup;
    Column: TColumnIndex): integer;
  begin
    Result := 0;

    case Column of
      0: // System
        Result := UTF8CompareText(aGroup1.CachedSystem.Title,
          aGroup2.CachedSystem.Title);
      1: // Name
        Result := UTF8CompareText(aGroup1.SortTitle, aGroup2.SortTitle);
      2: // N. Versions
        Result := aGroup1.SoftList.Count - aGroup2.SoftList.Count;
      3: // Developer
        Result := UTF8CompareText(aGroup1.Developer, aGroup2.Developer);
      4: // Year
        Result := UTF8CompareText(aGroup1.Year, aGroup2.Year);
      //      5: // Flags
      //        Result := 0;
      6: // Times
        Result := aGroup1.Stats.TimesPlayed - aGroup2.Stats.TimesPlayed;
      7: // Total time
        Result := aGroup1.Stats.PlayingTime - aGroup2.Stats.PlayingTime;
      8: // Last time
        Result := Trunc(aGroup1.Stats.LastTime - aGroup2.Stats.LastTime);
      9: // File
        Result := UTF8CompareText(aGroup1.MediaFileName,
          aGroup2.MediaFileName);
        //      10: // Folder
        //        Result := 0;
      else
        ;
    end;

  end;

  function CompareSoftware(aSoft1, aSoft2: cEmutecaSoftware;
    Column: TColumnIndex): integer;
  begin
    Result := 0;

    case Column of
      0: // System
        Result := UTF8CompareText(aSoft1.CachedSystem.Title,
          aSoft2.CachedSystem.Title);
      1: // Title
        Result := UTF8CompareText(aSoft1.SortTitle, aSoft2.SortTitle);
      2: // Version
      begin
        Result := UTF8CompareText(aSoft1.Zone, aSoft2.Zone);

        if Result = 0 then
          Result := UTF8CompareText(aSoft1.Version, aSoft2.Version);
      end;
      3: // Publisher
        Result := UTF8CompareText(aSoft1.Publisher, aSoft2.Publisher);
      4: // Year
        Result := UTF8CompareText(aSoft1.Year, aSoft2.Year);
      5: // Flags
      begin
        Result := Ord(aSoft1.DumpStatus) - Ord(aSoft2.DumpStatus);

        if Result = 0 then
          Result := UTF8CompareText(aSoft1.DumpInfo, aSoft2.DumpInfo);
      end;
      6: // Times Played
        Result := aSoft1.Stats.TimesPlayed - aSoft2.Stats.TimesPlayed;
      7: // Playing Time
        Result := aSoft1.Stats.PlayingTime - aSoft2.Stats.PlayingTime;
      8: // Last Time
        Result := Trunc(aSoft1.Stats.LastTime - aSoft2.Stats.LastTime);
      9: // File
        Result := UTF8CompareText(aSoft1.FileName, aSoft2.FileName);
      10: // Folder
        Result := UTF8CompareText(aSoft1.Folder, aSoft2.Folder);
      else
        ;
    end;
  end;

  function CompareSoftGroup(aSoft: cEmutecaSoftware;
    aGroup: cEmutecaGroup; Column: TColumnIndex): integer;
  begin
    Result := 0;

    case Column of
      0: // System
        Result := UTF8CompareText(aSoft.CachedSystem.Title,
          aGroup.CachedSystem.Title);
      1: // Title
        Result := UTF8CompareText(aSoft.CachedGroup.SortTitle, aGroup.SortTitle);
      2: // Version
      begin
        // aSoft = 1 version; aGroup > 1 version
        Result := -1;
      end;
      3: // Publisher/Developer
        // Comparing only developers
        Result := UTF8CompareText(aSoft.CachedGroup.Developer,
          aGroup.Developer);
      4: // Year
        // Comparing only developing years
        Result := UTF8CompareText(aSoft.CachedGroup.Year, aGroup.Year);
      // 5: // Flags
      //  Result := 0
      6: // Times Played
        Result := aSoft.Stats.TimesPlayed - aGroup.Stats.TimesPlayed;
      7: // Playing Time
        Result := aSoft.Stats.PlayingTime - aGroup.Stats.PlayingTime;
      8: // Last Time
        Result := Trunc(aSoft.Stats.LastTime - aGroup.Stats.LastTime);
      9: // File
        Result := UTF8CompareText(aSoft.FileName, aGroup.MediaFileName);
      10: // Folder
        Result := 1;
      else
        ;
    end;
  end;

var
  pData1, pData2: ^TObject;
begin
  Result := 0;
  pData1 := VDT.GetNodeData(Node1);
  pData2 := VDT.GetNodeData(Node2);

  if (not assigned(pData1^)) or (not assigned(pData2^)) then
    Exit;

  if pData1^ is cEmutecaGroup then
  begin
    if not (pData2^ is cEmutecaGroup) then
    begin
      if pData2^ is cEmutecaSoftware then
        Result := -CompareSoftGroup(cEmutecaSoftware(pData2^),
          cEmutecaGroup(pData1^), Column)
      else
        Exit;
    end
    else
    begin
      Result := CompareGroups(cEmutecaGroup(pData1^),
        cEmutecaGroup(pData2^), Column);
    end;
  end
  else if pData1^ is cEmutecaSoftware then
  begin
    if not (pData2^ is cEmutecaSoftware) then
    begin
      if pData2^ is cEmutecaGroup then
        Result := CompareSoftGroup(cEmutecaSoftware(pData1^),
          cEmutecaGroup(pData2^), Column)
      else
        Exit;
    end
    else
    begin
      Result := CompareSoftware(cEmutecaSoftware(pData1^),
        cEmutecaSoftware(pData2^), Column);
    end;
  end;

end;

procedure TfmEmutecaSoftTree.VDTDblClick(Sender: TObject);
var
  pData: ^TObject;
begin
  pData := VDT.GetNodeData(VDT.FocusedNode);
  if not assigned(pData) then
  begin
    OnDblClkGroup(nil);
    Exit;
  end;

  if pData^ is cEmutecaSoftware then
  begin
    if Assigned(OnDblClkSoft) then
      OnDblClkSoft(cEmutecaSoftware(pData^));
  end
  else
  begin
    if Assigned(OnDblClkGroup) then
      OnDblClkGroup(cEmutecaGroup(pData^));
  end;
end;

procedure TfmEmutecaSoftTree.VDTGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

  procedure GetGroupHint(aGroup: cEmutecaGroup; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
  begin
    LineBreakStyle := hlbForceMultiLine;
    case Column of
      0: // System
        HintText := aGroup.CachedSystem.Title;
      1: // Name
        HintText := aGroup.ID;
      else
        ;
    end;
  end;

  procedure GetSoftHint(aSoft: cEmutecaSoftware; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
  begin
    LineBreakStyle := hlbForceMultiLine;
    case Column of
      0: // System
        HintText := aSoft.CachedSystem.Title + sLineBreak +
          aSoft.CachedSystem.ID;
      1: // Title
        HintText := aSoft.Title + sLineBreak + aSoft.SortTitle;
      2: // Zone / Version
      begin
        if aSoft.Zone <> '' then
          HintText := 'Zone: ' + aSoft.Zone
        else
          HintText := 'Zone: ' + rsUnknown;
      end;
      5: // Flags
      begin
        HintText := DumpSt2Str(aSoft.DumpStatus);

        if aSoft.DumpInfo <> '' then
          HintText += ' (' + aSoft.DumpInfo + ')';

        if aSoft.Fixed <> '' then
          HintText += sLineBreak + 'Fixed: ' + aSoft.Fixed;

        if aSoft.Trainer <> '' then
          HintText += sLineBreak + 'Trainer: ' + aSoft.Trainer;

        if aSoft.Translation <> '' then
          HintText += sLineBreak + 'Translation: ' + aSoft.Translation;

        if aSoft.Pirate <> '' then
          HintText += sLineBreak + 'Pirate: ' + aSoft.Pirate;

        if aSoft.Cracked <> '' then
          HintText += sLineBreak + 'Cracked: ' + aSoft.Cracked;

        if aSoft.Modified <> '' then
          HintText += sLineBreak + 'Modified: ' + aSoft.Modified;

        if aSoft.Hack <> '' then
          HintText += sLineBreak + 'Hack: ' + aSoft.Hack;
      end;
      9: // File
        HintText := aSoft.FileName;
      10: // Folder
        HintText := aSoft.Folder;
      else
        ;
    end;
  end;

var
  pData: ^TObject;
begin
  pData := Sender.GetNodeData(Node);
  if not assigned(pData) then
    Exit;
  if not assigned(pData^) then
    Exit;

  if pData^ is cEmutecaGroup then
  begin
    GetGroupHint(cEmutecaGroup(pData^), Column, LineBreakStyle, HintText);
  end
  else if pData^ is cEmutecaSoftware then
  begin
    GetSoftHint(cEmutecaSoftware(pData^), Column, LineBreakStyle, HintText);
  end;
end;

procedure TfmEmutecaSoftTree.VDTGetPopupMenu(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
  var AskParent: boolean; var aPopupMenu: TPopupMenu);
var
  pData: ^TObject;
begin
  pData := Sender.GetNodeData(Node);
  if not Assigned(pData) then
    Exit;
  if not Assigned(pData^) then
    Exit;

  AskParent := False;

  if pData^ is cEmutecaGroup then
  begin
    if Assigned(pmGroup) then
      aPopupMenu := pmGroup;
  end
  else if pData^ is cEmutecaSoftware then
  begin
    if Assigned(pmSoft) then
      aPopupMenu := pmSoft;
  end
  else
    AskParent := True; // Ask Parent Popup
end;

procedure TfmEmutecaSoftTree.SetGroupList(AValue: cEmutecaGroupList);
begin
  if FGroupList = AValue then
    Exit;
  FGroupList := AValue;

  LoadFrameData;
end;

procedure TfmEmutecaSoftTree.SetGUIConfigFile(AValue: string);
begin
  if FGUIConfigFile = AValue then
    Exit;
  FGUIConfigFile := AValue;
end;

procedure TfmEmutecaSoftTree.SetIconsIniFile(AValue: string);
begin
  if FIconsIniFile = AValue then
    Exit;
  FIconsIniFile := AValue;
end;

procedure TfmEmutecaSoftTree.SetOnDblClkGroup(AValue: TEmutecaReturnGroupCB);
begin
  if FOnDblClkGroup = AValue then
    Exit;
  FOnDblClkGroup := AValue;
end;

procedure TfmEmutecaSoftTree.SetOnDblClkSoft(AValue: TEmutecaSoftCB);
begin
  if FOnDblClkSoft = AValue then
    Exit;
  FOnDblClkSoft := AValue;
end;

procedure TfmEmutecaSoftTree.SetOnSelectGroup(AValue: TEmutecaReturnGroupCB);
begin
  if FOnSelectGroup = AValue then
    Exit;
  FOnSelectGroup := AValue;
end;

procedure TfmEmutecaSoftTree.SetOnSelectSoft(AValue: TEmutecaSoftCB);
begin
  if FOnSelectSoft = AValue then
    Exit;
  FOnSelectSoft := AValue;
end;

procedure TfmEmutecaSoftTree.SetpmGroup(AValue: TPopupMenu);
begin
  if FpmGroup = AValue then
    Exit;
  FpmGroup := AValue;
end;

procedure TfmEmutecaSoftTree.SetpmSoft(AValue: TPopupMenu);
begin
  if FpmSoft = AValue then
    Exit;
  FpmSoft := AValue;
end;

procedure TfmEmutecaSoftTree.DoLoadGUIConfig(aIniFile: TIniFile);
var
  i: integer;
  aBool: boolean;
  vstOptions: TVTColumnOptions;
begin
  VDT.BeginUpdate;
  // VST Fonts
  LoadFontFromIni(aIniFile, krsIniSoftTreeSection,
    krsIniSoftTreeSoftFont, VDT.Font);
  VDT.DefaultNodeHeight := abs(VDT.Font.Height);
  VDT.IterateSubtree(nil, @SetNodesHeight, nil);

  // VST Columns
  i := 0;
  while i < VDT.Header.Columns.Count do
  begin
    // Columns width
    VDT.Header.Columns.Items[i].Width :=
      aIniFile.ReadInteger(krsIniSoftTreeSection,
      Format(krsIniSoftTreeWidthFmt, [i]), VDT.Header.Columns.Items[i].Width);

    vstOptions := VDT.Header.Columns.Items[i].Options;

    // Columns visibility
    aBool := aIniFile.ReadBool(krsIniSoftTreeSection,
      Format(krsIniSoftTreeVisibleFmt, [i]), True);
    if aBool then
      Include(vstOptions, coVisible)
    else
      Exclude(vstOptions, coVisible);

    VDT.Header.Columns.Items[i].Options := vstOptions;

    // Columns position
    VDT.Header.Columns.Items[i].Position :=
      aIniFile.ReadInteger(krsIniSoftTreeSection,
      Format(krsIniSoftTreePositionFmt, [i]),
      VDT.Header.Columns.Items[i].Position);
    Inc(i);
  end;
  VDT.EndUpdate;
end;

procedure TfmEmutecaSoftTree.DoSaveGUIConfig(aIniFile: TIniFile);
var
  i: integer;
begin
  // VST Fonts
  SaveFontToIni(aIniFile, krsIniSoftTreeSection,
    krsIniSoftTreeSoftFont, VDT.Font);

  // VST Columns
  i := 0;
  while i < VDT.Header.Columns.Count do
  begin
    // Columns width
    aIniFile.WriteInteger(krsIniSoftTreeSection,
      Format(krsIniSoftTreeWidthFmt, [i]), VDT.Header.Columns.Items[i].Width);

    // Columns visibility
    aIniFile.WriteBool(krsIniSoftTreeSection,
      Format(krsIniSoftTreeVisibleFmt, [i]),
      coVisible in VDT.Header.Columns.Items[i].Options);

    // Columns position
    aIniFile.WriteInteger(krsIniSoftTreeSection,
      Format(krsIniSoftTreePositionFmt, [i]),
      VDT.Header.Columns.Items[i].Position);

    Inc(i);
  end;
end;

procedure TfmEmutecaSoftTree.UpdateSBNodeCount;
begin
  StatusBar.Panels[0].Text :=
    Format(rsFmtNItems, [VDT.RootNodeCount]);
end;

procedure TfmEmutecaSoftTree.SetNodesHeight(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: boolean);
begin
  Abort := False;
  Sender.NodeHeight[Node] := VDT.DefaultNodeHeight;
end;

procedure TfmEmutecaSoftTree.DoClearFrameData;
begin
  VDT.Clear;
  VDT.RootNodeCount := 0;
  UpdateSBNodeCount;
end;

procedure TfmEmutecaSoftTree.DoLoadFrameData;
begin
  Enabled := assigned(GroupList);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  VDT.Clear;
  VDT.RootNodeCount := GroupList.Count;

  UpdateSBNodeCount;
end;

constructor TfmEmutecaSoftTree.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  VDT.NodeDataSize := SizeOf(TObject);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;

  OnLoadGUIConfig := @DoLoadGUIConfig;
  OnSaveGUIConfig := @DoSaveGUIConfig;
end;

destructor TfmEmutecaSoftTree.Destroy;
begin
  inherited Destroy;
end;

end.
