unit ufEmutecaSoftTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, VTHeaderPopup, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ActnList, Menus, LazUTF8, LazFileUtils, IniFiles,
  ufCHXFrame,
  uLEmuTKCommon,
  uaEmutecaCustomSoft,
  ucEmutecaGroupList, ucEmutecaGroup, ucEmutecaSoftware, uEmutecaCommon;

const
  krsIniSoftTreeSection = 'SoftTree';
  krsIniSoftTreeWidthFmt = 'Column%0:d_Width';
  krsIniSoftTreeVisibleFmt = 'Column%0:d_Visible';

type

  { TfmEmutecaSoftTree }

  TfmEmutecaSoftTree = class(TfmCHXFrame)
    actRunSoftware: TAction;
    actMergeGroupFiles: TAction;
    ActionList: TActionList;
    miPMSRunSoftware: TMenuItem;
    miPMGMergeGroupFiles: TMenuItem;
    pmSTGroup: TPopupMenu;
    pmSTSoft: TPopupMenu;
    StatusBar: TStatusBar;
    VDT: TVirtualStringTree;
    VTHPopupMenu: TVTHeaderPopupMenu;
    procedure actMergeGroupFilesExecute(Sender: TObject);
    procedure VDTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VDTCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure VDTDblClick(Sender: TObject);
    procedure VDTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure VDTGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const P: TPoint; var AskParent: Boolean;
      var aPopupMenu: TPopupMenu);
    procedure VDTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VDTInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: cardinal);
    procedure VDTInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    FGroupList: cEmutecaGroupList;
    FOnDblClkGroup: TEmutecaReturnGroupCB;
    FOnDblClkSoft: TEmutecaReturnSoftCB;
    FOnSelectGroup: TEmutecaReturnGroupCB;
    FOnSelectSoft: TEmutecaReturnSoftCB;
    procedure SetGroupList(AValue: cEmutecaGroupList);
    procedure SetOnDblClkGroup(AValue: TEmutecaReturnGroupCB);
    procedure SetOnDblClkSoft(AValue: TEmutecaReturnSoftCB);
    procedure SetOnSelectGroup(AValue: TEmutecaReturnGroupCB);
    procedure SetOnSelectSoft(AValue: TEmutecaReturnSoftCB);

  protected
    procedure UpdateSBNodeCount;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoLoadGUIConfig(aIniFile: TIniFile); virtual;
    procedure DoSaveGUIConfig(aIniFile: TIniFile);

  public
    property GroupList: cEmutecaGroupList read FGroupList write SetGroupList;

    property OnSelectGroup: TEmutecaReturnGroupCB
      read FOnSelectGroup write SetOnSelectGroup;
    property OnDblClkGroup: TEmutecaReturnGroupCB
      read FOnDblClkGroup write SetOnDblClkGroup;
    property OnSelectSoft: TEmutecaReturnSoftCB
      read FOnSelectSoft write SetOnSelectSoft;
    property OnDblClkSoft: TEmutecaReturnSoftCB
      read FOnDblClkSoft write SetOnDblClkSoft;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSoftTree }

procedure TfmEmutecaSoftTree.VDTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

  procedure GetGroupText(aGroup: cEmutecaGroup; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string);
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
      9: // Folder
        CellText := '';
      10: // File
        CellText := aGroup.MediaFileName;
      else
        ;
    end;
  end;


  procedure GetSoftText(aSoft: cEmutecaSoftware; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string);
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
      9: // Folder
        CellText := aSoft.Folder;
      10: // File
        CellText := aSoft.FileName;
      else
        ;
    end;
  end;

var
  pData: ^TObject;
begin
  pData := Sender.GetNodeData(Node);
  if pData^ = nil then
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
  if Node = nil then
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
  if ParentNode = nil then
  begin
    pGroup := Sender.GetNodeData(Node);
    pGroup^ := GroupList[Node^.Index];
    Include(InitialStates, ivsHasChildren);
  end
  else
  begin
    pGroup := Sender.GetNodeData(ParentNode);
    pSoft := Sender.GetNodeData(Node);
    pSoft^ := pGroup^.SoftList[Node^.Index];
  end;
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
    OnSelectGroup(nil);
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

procedure TfmEmutecaSoftTree.actMergeGroupFilesExecute(Sender: TObject);
begin
  raise ENotImplemented.Create('Not Implemented.');
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
      //        Result := '';
      6: // Times
        Result := aGroup1.Stats.TimesPlayed - aGroup2.Stats.TimesPlayed;
      7: // Total time
        Result := aGroup1.Stats.PlayingTime - aGroup2.Stats.PlayingTime;
      8: // Last time
        Result := Trunc(aGroup1.Stats.LastTime - aGroup2.Stats.LastTime);
      //      9: // Folder
      //        Result := '';
      10: // File
        Result := UTF8CompareText(aGroup1.MediaFileName,
          aGroup2.MediaFileName);
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
      9: // Folder
        Result := UTF8CompareText(aSoft1.Folder, aSoft2.Folder);
      10: // File
        Result := UTF8CompareText(aSoft1.FileName, aSoft2.FileName);
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
      Exit;
    Result := CompareGroups(cEmutecaGroup(pData1^), cEmutecaGroup(
      pData2^), Column);
  end
  else if pData1^ is cEmutecaSoftware then
  begin
    if not (pData2^ is cEmutecaSoftware) then
      Exit;
    Result := CompareSoftware(cEmutecaSoftware(pData1^),
      cEmutecaSoftware(pData2^), Column);
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
    case Column of
      0: // System
        HintText := aSoft.CachedSystem.Title + sLineBreak +
          aSoft.CachedSystem.ID;
      1: // Title
        HintText := aSoft.Title + sLineBreak + aSoft.TranslitTitle +
          sLineBreak + aSoft.SortTitle;
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
      9: // Folder
        HintText := aSoft.Folder;
      10: // File
        HintText := aSoft.FileName;
      else
        ;
    end;
  end;

var
  pData: ^TObject;
begin
  pData := Sender.GetNodeData(Node);
  if pData^ = nil then
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
  var AskParent: Boolean; var aPopupMenu: TPopupMenu);
var
  pData: ^TObject;
begin
  pData := Sender.GetNodeData(Node);
  if pData^ = nil then
    Exit;

  if pData^ is cEmutecaGroup then
  begin
    aPopupMenu := pmSTGroup;
  end
  else if pData^ is cEmutecaSoftware then
  begin
    aPopupMenu := pmSTSoft;
  end;
end;

procedure TfmEmutecaSoftTree.SetGroupList(AValue: cEmutecaGroupList);
begin
  if FGroupList = AValue then
    Exit;
  FGroupList := AValue;

  LoadFrameData;
end;

procedure TfmEmutecaSoftTree.SetOnDblClkGroup(AValue: TEmutecaReturnGroupCB);
begin
  if FOnDblClkGroup = AValue then
    Exit;
  FOnDblClkGroup := AValue;
end;

procedure TfmEmutecaSoftTree.SetOnDblClkSoft(AValue: TEmutecaReturnSoftCB);
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

procedure TfmEmutecaSoftTree.SetOnSelectSoft(AValue: TEmutecaReturnSoftCB);
begin
  if FOnSelectSoft = AValue then
    Exit;
  FOnSelectSoft := AValue;
end;

procedure TfmEmutecaSoftTree.DoLoadGUIConfig(aIniFile: TIniFile);
var
  i: integer;
  aBool: Boolean;
  vstOptions: TVTColumnOptions;
begin
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
    Inc(i);
  end;
end;

procedure TfmEmutecaSoftTree.DoSaveGUIConfig(aIniFile: TIniFile);
var
  i: integer;
begin
  i := 0;
  while i < VDT.Header.Columns.Count do
  begin
    aIniFile.WriteInteger(krsIniSoftTreeSection,
      Format(krsIniSoftTreeWidthFmt, [i]), VDT.Header.Columns.Items[i].Width);

    aIniFile.WriteBool(krsIniSoftTreeSection,
      Format(krsIniSoftTreeVisibleFmt, [i]),
      coVisible in VDT.Header.Columns.Items[i].Options);
    Inc(i);
  end;
end;

procedure TfmEmutecaSoftTree.UpdateSBNodeCount;
begin
  StatusBar.Panels[0].Text :=
    Format(rsFmtNItems, [VDT.RootNodeCount, VDT.VisibleCount]);
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
