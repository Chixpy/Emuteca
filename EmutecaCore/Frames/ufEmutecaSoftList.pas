unit ufEmutecaSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, ComCtrls,
  ActnList, Menus, LazUTF8,
  uEmutecaCommon, ucEmutecaSoftware;

type
  TFEVLItemSelected = procedure(const aVersion: cEmutecaSoftware) of object;
  TFEVLDblClick = procedure(const aVersion: cEmutecaSoftware) of object;

  { TfmEmutecaSoftList }

  TfmEmutecaSoftList = class(TFrame)
    ActionList1: TActionList;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    VST: TVirtualStringTree;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);

  private
    FFilterStr: string;
    FOnDblClick: TFEVLDblClick;
    FOnItemSelect: TFEVLItemSelected;
    FSoftList: cEmutecaSoftList;
    procedure SetFilterStr(AValue: string);
    procedure SetOnDblClick(AValue: TFEVLDblClick);
    procedure SetOnItemSelect(AValue: TFEVLItemSelected);
    procedure SetSoftList(AValue: cEmutecaSoftList);

  protected
    procedure HideNodes(Sender: TBaseVirtualTree;  Node: PVirtualNode; Data: Pointer; var Abort: boolean);

    procedure UpdateStatusBar;
  public
    property SoftList: cEmutecaSoftList read FSoftList write SetSoftList;

    property FilterStr: string read FFilterStr write SetFilterStr;
    {< String to show/hide nodes }

    property OnItemSelect: TFEVLItemSelected
      read FOnItemSelect write SetOnItemSelect;
    //< CallBack function when item selected.
    property OnDblClick: TFEVLDblClick read FOnDblClick write SetOnDblClick;
    //< CallBack function when item Double Click.

    procedure UpdateList;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSoftList }

procedure TfmEmutecaSoftList.SetOnItemSelect(AValue: TFEVLItemSelected);
begin
  if FOnItemSelect = AValue then
    Exit;
  FOnItemSelect := AValue;
end;

procedure TfmEmutecaSoftList.SetSoftList(AValue: cEmutecaSoftList);
begin
  if FSoftList=AValue then Exit;
  FSoftList:=AValue;

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
  Sender.IsVisible[Node] := UTF8Pos(FilterStr, UTF8LowerString(pData^.Title)) >= 1
  else
    Sender.IsVisible[Node] := True;
end;

procedure TfmEmutecaSoftList.UpdateStatusBar;
begin
  StatusBar1.SimpleText := Format(rsFmtNItems, [vst.RootNodeCount, vst.VisibleCount]);
end;

procedure TfmEmutecaSoftList.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pData: ^cEmutecaSoftware;
begin
  if Assigned(OnItemSelect) then
  begin
    if Sender.SelectedCount <> 1 then
      Exit;

    pData := Sender.GetNodeData(Node);
    if not Assigned(pData) then
      OnItemSelect(nil)
    else
      OnItemSelect(pData^);
  end;
end;

procedure TfmEmutecaSoftList.VSTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pData1, pData2: ^cEmutecaSoftware;
begin
  Result := 0;
  pData1 := Sender.GetNodeData(Node1);
  pData2 := Sender.GetNodeData(Node2);

  if (pData1^ = nil) or (pData2^ = nil) then
    Exit;

  case Column of
    0: // Title
      Result := UTF8CompareText(pData1^.Title, pData2^.Title);
    1: // System
      Result := UTF8CompareText(pData1^.System, pData2^.System);
    2: // Description
      Result := UTF8CompareText(pData1^.Description, pData2^.Description);
    3: // Folder
      Result := UTF8CompareText(pData1^.Folder, pData2^.Folder);
    4: // File;
      Result := UTF8CompareText(pData1^.FileName, pData2^.FileName);
    else
      Result := UTF8CompareText(pData1^.Title, pData2^.Title);
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
    0: // Title
      CellText := pData^.Title;
    1: // System
      CellText := pData^.System;
    2: // Description
      CellText := pData^.Description;
    3: // Folder
      CellText := pData^.Folder;
    4: // File
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

procedure TfmEmutecaSoftList.SetOnDblClick(AValue: TFEVLDblClick);
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
  if aTemp = FFilterStr then Exit;
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
  aTemp: String;
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
