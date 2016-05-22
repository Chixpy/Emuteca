unit ufEmutecaParentList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms,
  Controls, ComCtrls,
  LazUTF8,
  uEmutecaCommon, ucEmutecaParent;

type
  TFEPLItemSelected = procedure(const aParent: cEmutecaParent) of object;

  { TfmEmutecaParentList }

  TfmEmutecaParentList = class(TFrame)
    StatusBar1: TStatusBar;
    VST: TVirtualStringTree;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

  private
    FParentList: cEmutecaParentMap;
    FOnItemSelect: TFEPLItemSelected;
    procedure SetParentList(AValue: cEmutecaParentMap);
    procedure SetOnItemSelect(AValue: TFEPLItemSelected);

  protected
    procedure UpdateList;

  public
    { public declarations }
    property ParentList: cEmutecaParentMap
      read FParentList write SetParentList;

    property OnItemSelect: TFEPLItemSelected
      read FOnItemSelect write SetOnItemSelect;
    //< CallBack function when item selected.

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;


  end;

implementation

{$R *.lfm}

{ TfmEmutecaParentList }

procedure TfmEmutecaParentList.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pData: ^cEmutecaParent;
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

procedure TfmEmutecaParentList.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  pData1, pData2: ^cEmutecaParent;
begin
  Result := 0;
  pData1 := Sender.GetNodeData(Node1);
  pData2 := Sender.GetNodeData(Node2);

  if (pData1^ = nil) or (pData2^ = nil) then
    Exit;

    case Column of
      -1, 0: // Name
        Result := UTF8CompareText(pData1^.SortName, pData2^.SortName);
      1: // System
        Result := UTF8CompareText(pData1^.System, pData2^.System);
  end;
end;

procedure TfmEmutecaParentList.VSTGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  pData: ^cEmutecaParent;
begin
  pData := Sender.GetNodeData(Node);
  if pData^ = nil then
    Exit;

  case Column of
    -1, 0: // Name
      HintText := pData^.SortName;
  end;
end;

procedure TfmEmutecaParentList.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pData: ^cEmutecaParent;
begin
  pData := Sender.GetNodeData(Node);
  if pData^ = nil then
    Exit;

  case Column of
    -1, 0: // Name
      CellText := pData^.Title;
    1: // System
      CellText := pData^.System;
  end;
end;

procedure TfmEmutecaParentList.VSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  pData: ^cEmutecaParent;
begin
  pData := Sender.GetNodeData(Node);
  pData^ := ParentList.Data[Node^.Index];
end;

procedure TfmEmutecaParentList.SetParentList(AValue: cEmutecaParentMap);
begin
  FParentList := AValue;
  UpdateList;
end;

procedure TfmEmutecaParentList.SetOnItemSelect(AValue: TFEPLItemSelected);
begin
  if FOnItemSelect = AValue then
    Exit;
  FOnItemSelect := AValue;
end;

procedure TfmEmutecaParentList.UpdateList;
begin
  VST.Clear;
  vst.RootNodeCount := ParentList.Count;
  StatusBar1.SimpleText:=Format(rsFmtNItems, [ParentList.Count]);
end;

constructor TfmEmutecaParentList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  VST.NodeDataSize := SizeOf(cEmutecaParent);
end;

destructor TfmEmutecaParentList.Destroy;
begin
  inherited Destroy;
end;

end.
