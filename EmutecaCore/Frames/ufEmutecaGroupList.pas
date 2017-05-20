unit ufEmutecaGroupList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms,
  Controls, ComCtrls,
  LazUTF8,
  uCHXStrUtils,
  uEmutecaCommon, uEmutecaRscStr, ucEmutecaSystem, ucEmutecaGroup;

type
  { TfmEmutecaGroupList }

  TfmEmutecaGroupList = class(TFrame)
    StatusBar1: TStatusBar;
    VST: TVirtualStringTree;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
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
    FGroupList: cEmutecaGroupList;
    FOnItemSelect: TEmutecaReturnGroupCB;
    procedure SetFilterStr(AValue: string);
    procedure SetGroupList(AValue: cEmutecaGroupList);
    procedure SetOnItemSelect(AValue: TEmutecaReturnGroupCB);

  protected
    procedure HideNodes(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: boolean);

    procedure UpdateStatusBar;

  public
    { public declarations }
    property GroupList: cEmutecaGroupList read FGroupList write SetGroupList;

    property FilterStr: string read FFilterStr write SetFilterStr;
    //< String to show/hide nodes.

    property OnItemSelect: TEmutecaReturnGroupCB
      read FOnItemSelect write SetOnItemSelect;
    //< CallBack function when item selected.

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    procedure UpdateList;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;


  end;

implementation

{$R *.lfm}

{ TfmEmutecaGroupList }

procedure TfmEmutecaGroupList.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pData: ^cEmutecaGroup;
begin
  if Assigned(OnItemSelect) then
  begin
    if Sender.SelectedCount > 1 then
      Exit;

    pData := Sender.GetNodeData(Node);
    if not Assigned(pData) then
      OnItemSelect(nil)
    else
      OnItemSelect(pData^);
  end;
end;

procedure TfmEmutecaGroupList.VSTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pData1, pData2: ^cEmutecaGroup;
begin
  Result := 0;
  pData1 := Sender.GetNodeData(Node1);
  pData2 := Sender.GetNodeData(Node2);

  if (pData1^ = nil) or (pData2^ = nil) then
    Exit;

  case Column of
    0: // System
      Result := UTF8CompareText(cEmutecaSystem(pData1^.System).Title,
        cEmutecaSystem(pData2^.System).Title);
    1: // Name
      Result := UTF8CompareText(pData1^.ID, pData2^.ID);
    2: // Developer
      Result := UTF8CompareText(pData1^.Developer, pData2^.Developer);
    3: // Year
      Result := UTF8CompareText(pData1^.Year, pData2^.Year);
    4: // Times
      Result := pData1^.Stats.TimesPlayed - pData2^.Stats.TimesPlayed;
    5: // Total time
      Result := pData1^.Stats.PlayingTime - pData2^.Stats.PlayingTime;
    6: // Last time
      Result := Trunc(pData1^.Stats.LastTime - pData2^.Stats.LastTime);
    else
      ;
  end;
end;

procedure TfmEmutecaGroupList.VSTGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  pData: ^cEmutecaGroup;
begin
  pData := Sender.GetNodeData(Node);
  if pData^ = nil then
    Exit;

  case Column of
        0: // System
      HintText := cEmutecaSystem(pData^.System).Title;
    1: // Name
      HintText := pData^.ID;
    2: ; // Develepor
    3: ; // Year
    4: ; // Times
    5: ; // Total time
    6: ; // Last time
    else
      ;
  end;
end;

procedure TfmEmutecaGroupList.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pData: ^cEmutecaGroup;
begin
  pData := Sender.GetNodeData(Node);
  if pData^ = nil then
    Exit;

  case Column of
    0: // System
      CellText := cEmutecaSystem(pData^.System).Title;
    1: // Name
      CellText := pData^.Title;
    2: // Develepor
      CellText := pData^.Developer;
    3: // Year
      CellText := pData^.Year;
    4: // Times
      CellText := IntToStr(pData^.Stats.TimesPlayed);
    5: // Total time
      CellText := SecondsToFmtStr(pData^.Stats.PlayingTime);
    6: // Last time
      if pData^.Stats.LastTime = 0 then
        CellText := rsNever
      else
        CellText := DateTimeToStr(pData^.Stats.LastTime);
    else
      ;
  end;
end;

procedure TfmEmutecaGroupList.VSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  pData: ^cEmutecaGroup;
begin

  pData := Sender.GetNodeData(Node);
  if Node^.Index < GroupList.Count then
    pData^ := cEmutecaGroup(GroupList[Node^.Index])
  else
    pData^ := nil;
end;

procedure TfmEmutecaGroupList.SetGroupList(AValue: cEmutecaGroupList);
begin
  FGroupList := AValue;
  UpdateList;
end;

procedure TfmEmutecaGroupList.SetFilterStr(AValue: string);
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

procedure TfmEmutecaGroupList.SetOnItemSelect(AValue: TEmutecaReturnGroupCB);
begin
  if FOnItemSelect = AValue then
    Exit;
  FOnItemSelect := AValue;
end;

procedure TfmEmutecaGroupList.HideNodes(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: boolean);
var
  pData: ^cEmutecaGroup;
begin
  Abort := False;

  pData := Sender.GetNodeData(Node);
  if FilterStr <> '' then
    Sender.IsVisible[Node] :=
      (UTF8Pos(FilterStr, UTF8LowerString(pData^.Title)) >= 1) or
      (UTF8Pos(FilterStr, UTF8LowerString(pData^.ID)) >= 1)
  else
    Sender.IsVisible[Node] := True;
end;

procedure TfmEmutecaGroupList.UpdateStatusBar;
begin
  StatusBar1.SimpleText :=
    Format(rsFmtNItems, [vst.RootNodeCount, vst.VisibleCount]);
end;

procedure TfmEmutecaGroupList.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
var
  pData: ^cEmutecaGroup;
begin

end;

procedure TfmEmutecaGroupList.UpdateList;
begin
  VST.Clear;

  StatusBar1.SimpleText := '';
  if not assigned(GroupList) then
    Exit;

  vst.RootNodeCount := GroupList.Count + 1;

  UpdateStatusBar;
end;

constructor TfmEmutecaGroupList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  VST.NodeDataSize := SizeOf(cEmutecaGroup);
end;

destructor TfmEmutecaGroupList.Destroy;
begin
  inherited Destroy;
end;

end.
