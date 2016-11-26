unit ufEmutecaSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, ComCtrls,
  ActnList, Menus, LazUTF8,
  uCHXStrUtils,
  uEmutecaCommon, ucEmutecaSoftware;

type
  { TfmEmutecaSoftList }

  TfmEmutecaSoftList = class(TFrame)
    StatusBar1: TStatusBar;
    VST: TVirtualStringTree;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);

  private
    FFilterStr: string;
    FOnDblClick: TEmutecaReturnSoftCB;
    FOnItemSelect: TEmutecaReturnSoftCB;
    FSoftList: cEmutecaSoftList;
    procedure SetFilterStr(AValue: string);
    procedure SetOnDblClick(AValue: TEmutecaReturnSoftCB);
    procedure SetOnItemSelect(AValue: TEmutecaReturnSoftCB);
    procedure SetSoftList(AValue: cEmutecaSoftList);

  protected
    procedure HideNodes(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: boolean);

    procedure UpdateStatusBar;

  public
    property SoftList: cEmutecaSoftList read FSoftList write SetSoftList;
    //< Actual list to show

    property FilterStr: string read FFilterStr write SetFilterStr;
    {< String to show/hide nodes }

    property OnItemSelect: TEmutecaReturnSoftCB
      read FOnItemSelect write SetOnItemSelect;
    //< CallBack function when item selected.
    property OnDblClick: TEmutecaReturnSoftCB read FOnDblClick write SetOnDblClick;
    //< CallBack function when item Double Click.

    procedure UpdateList;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSoftList }

procedure TfmEmutecaSoftList.SetOnItemSelect(AValue: TEmutecaReturnSoftCB);
begin
  if FOnItemSelect = AValue then
    Exit;
  FOnItemSelect := AValue;
end;

procedure TfmEmutecaSoftList.SetSoftList(AValue: cEmutecaSoftList);
begin
  if FSoftList = AValue then
    Exit;
  FSoftList := AValue;

  UpdateList;
end;

procedure TfmEmutecaSoftList.HideNodes(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: boolean);
var
  pData: ^cEmutecaSoftware;
begin
  Abort := False;

  pData := Sender.GetNodeData(Node);
  if FilterStr <> '' then
    Sender.IsVisible[Node] :=
      (UTF8Pos(FilterStr, UTF8LowerString(pData^.Title)) >= 1) or
      (UTF8Pos(FilterStr, UTF8LowerString(pData^.TranslitTitle)) >= 1)
  else
    Sender.IsVisible[Node] := True;
end;

procedure TfmEmutecaSoftList.UpdateStatusBar;
begin
  StatusBar1.SimpleText :=
    Format(rsFmtNItems, [vst.RootNodeCount, vst.VisibleCount]);
end;

procedure TfmEmutecaSoftList.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pData: ^cEmutecaSoftware;
begin
  if Sender.SelectedCount <> 1 then
    Exit;

  if not Assigned(OnItemSelect) then
    exit;

  pData := Sender.GetNodeData(Node);
  if not Assigned(pData) then
    OnItemSelect(nil)
  else
    OnItemSelect(pData^);
end;

procedure TfmEmutecaSoftList.VSTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pData1, pData2: ^cEmutecaSoftware;
begin
  Result := 0;
  if Column = -1 then
    Exit;

  pData1 := Sender.GetNodeData(Node1);
  pData2 := Sender.GetNodeData(Node2);

  if (pData1^ = nil) or (pData2^ = nil) then
    Exit;

  case Column of
    0: // System
      Result := UTF8CompareText(pData1^.System.Title, pData2^.System.Title);
    1: // Title
      Result := UTF8CompareText(pData1^.SortTitle, pData2^.SortTitle);
    2: // Version
      Result := UTF8CompareText(pData1^.Version, pData2^.Version);
    3: // Publisher
      Result := UTF8CompareText(pData1^.Publisher, pData2^.Publisher);
    4: // Year
      Result := UTF8CompareText(pData1^.Year, pData2^.Year);
    5: // Flags
    begin
      if pData1^.DumpStatus <> pData2^.DumpStatus then
       Result := ord(pData1^.DumpStatus) - ord(pData2^.DumpStatus)
      else
        Result := UTF8CompareText(pData1^.Translation, pData2^.Translation);
    end;
    6: // Times Played
      Result := pData1^.Stats.TimesPlayed - pData2^.Stats.TimesPlayed;
    7: // Playing Time
      Result := pData1^.Stats.PlayingTime - pData2^.Stats.PlayingTime;
    8: // Last Time
      Result := Trunc(pData1^.Stats.LastTime - pData2^.Stats.LastTime);
    9: // Folder
      Result := UTF8CompareText(pData1^.Folder, pData2^.Folder);
    10: // File;
      Result := UTF8CompareText(pData1^.FileName, pData2^.FileName);
    else
      ;
  end;
end;

procedure TfmEmutecaSoftList.VSTDblClick(Sender: TObject);
var
  pData: ^cEmutecaSoftware;
begin
  if Assigned(OnDblClick) then
  begin
    pData := VST.GetNodeData(vst.FocusedNode);
    if not Assigned(pData) then
      OnDblClick(nil)
    else
      OnDblClick(pData^);
  end;
end;

procedure TfmEmutecaSoftList.VSTGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  pData: ^cEmutecaSoftware;
begin
  pData := VST.GetNodeData(Node);
  if pData^ = nil then
    Exit;

  case Column of
    1: // Title
      HintText := pData^.Title + sLineBreak + pData^.TranslitTitle + sLineBreak + pData^.SortTitle;
    5: // Flags
    begin
      HintText := EmutecaDumpStatusStrs[pData^.DumpStatus];

      if pData^.DumpInfo <> '' then
        HintText += ' (' + pData^.DumpInfo + ')';

      if pData^.Fixed <> '' then
        HintText += sLineBreak + 'Fixed: ' + pData^.Fixed;

      if pData^.Trainer <> '' then
        HintText += sLineBreak + 'Trainer: ' + pData^.Trainer;

      if pData^.Translation <> '' then
        HintText += sLineBreak + 'Translation: ' + pData^.Translation;

      if pData^.Pirate <> '' then
        HintText += sLineBreak + 'Pirate: ' + pData^.Pirate;

      if pData^.Cracked <> '' then
        HintText += sLineBreak + 'Cracked: ' + pData^.Cracked;

      if pData^.Modified <> '' then
        HintText += sLineBreak + 'Modified: ' + pData^.Modified;

      if pData^.Hack <> '' then
        HintText += sLineBreak + 'Hack: ' + pData^.Hack;
    end;
  end;
end;

procedure TfmEmutecaSoftList.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pData: ^cEmutecaSoftware;
begin
  pData := VST.GetNodeData(Node);
  if pData^ = nil then
    Exit;

  case Column of
    0: // System
      CellText := pData^.System.Title;
    1: // Title
      CellText := pData^.Title;
    2: // Version
      CellText := pData^.Zone + ' ' + pData^.Version;
    3: // Publisher
    begin
      if pData^.Publisher = '' then
      begin
          if pData^.Group.Developer <> '' then
            CellText := '(' + pData^.Group.Developer + ')'
          else
            CellText := '';
      end
      else
        CellText := pData^.Publisher;
    end;
    4: // Year
    begin
      if pData^.Year = '' then
      begin
          if pData^.Group.Year <> '' then
            CellText := '(' +pData^.Group.Year + ')'
            else
              CellText := '';
      end
      else
        CellText := pData^.Year;
    end;
    5: // Flags
    begin
      // A simple formated output, to be overriden
      CellText := EmutecaDumpStatusStrs[pData^.DumpStatus];

      if pData^.DumpInfo <> '' then
        CellText += ' [' + pData^.DumpInfo + ']';

      if pData^.Fixed <> '' then
        CellText += ' [f ' + pData^.Fixed + ']';

      if pData^.Trainer <> '' then
        CellText += ' [t ' + pData^.Trainer + ']';

      if pData^.Translation <> '' then
        CellText += ' [tr ' + pData^.Translation + ']';

      if pData^.Pirate <> '' then
        CellText += ' [p ' + pData^.Pirate + ']';

      if pData^.Cracked <> '' then
        CellText += ' [cr ' + pData^.Cracked + ']';

      if pData^.Modified <> '' then
        CellText += ' [m ' + pData^.Modified + ']';

      if pData^.Hack <> '' then
        CellText += ' [t ' + pData^.Hack + ']';
    end;
    6: // Times Played
      CellText := IntToStr(pData^.Stats.TimesPlayed);
    7: // Playing Time
      CellText := SecondsToFmtStr(pData^.Stats.PlayingTime);
    8: // Last Time
    if pData^.Stats.LastTime = 0 then
      CellText := rsNever
      else
      CellText := DateTimeToStr(pData^.Stats.LastTime);
    9: // Folder
      CellText := pData^.Folder;
    10: // File
      CellText := pData^.FileName;
    else // Title
      CellText := pData^.Title;
  end;
end;

procedure TfmEmutecaSoftList.VSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  pData: ^cEmutecaSoftware;
begin
  pData := VST.GetNodeData(Node);
  pData^ := cEmutecaSoftware(SoftList[Node^.Index]);
end;

procedure TfmEmutecaSoftList.SetOnDblClick(AValue: TEmutecaReturnSoftCB);
begin
  if FOnDblClick = AValue then
    Exit;
  FOnDblClick := AValue;
end;

procedure TfmEmutecaSoftList.SetFilterStr(AValue: string);
var
  aTemp: string;
begin
  aTemp := UTF8LowerString(AValue);
  if aTemp = FFilterStr then
    Exit;
  FFilterStr := aTemp;

  VST.BeginUpdate;
  try
    VST.IterateSubtree(nil, @HideNodes, nil) // ,[],True);
  finally
    VST.EndUpdate;
  end;
  UpdateStatusBar;
end;

procedure TfmEmutecaSoftList.UpdateList;
var
  aTemp: string;
begin
  VST.Clear;
  StatusBar1.SimpleText := '';
  if not assigned(SoftList) then
    Exit;

  vst.RootNodeCount := SoftList.Count;

  if FilterStr <> '' then
  begin
    // HACK: SetFilterStr(FilterStr)
    aTemp := FilterStr;
    FFilterStr := '';
    FilterStr := aTemp;
  end;
  UpdateStatusBar;

end;

constructor TfmEmutecaSoftList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  VST.NodeDataSize := SizeOf(cEmutecaSoftware);
end;

destructor TfmEmutecaSoftList.Destroy;
begin
  inherited Destroy;
end;

end.
