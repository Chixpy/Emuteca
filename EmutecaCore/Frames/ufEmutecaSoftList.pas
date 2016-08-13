unit ufEmutecaSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, ComCtrls,
  ActnList, Menus, LazUTF8,
  uEmutecaCommon, ucEmutecaSoftware;

type
  TFEVLItemSelected = procedure(const aVersion: cEmutecaVersion) of object;
  TFEVLDblClick = procedure(const aVersion: cEmutecaVersion) of object;

  { TfmEmutecaVersionList }

  TfmEmutecaVersionList = class(TFrame)
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
    FOnDblClick: TFEVLDblClick;
    FOnItemSelect: TFEVLItemSelected;
    FSoftList: cEmutecaVersionList;
    procedure SetOnDblClick(AValue: TFEVLDblClick);
    procedure SetOnItemSelect(AValue: TFEVLItemSelected);
    procedure SetSoftList(AValue: cEmutecaVersionList);

  protected


  public
    property SoftList: cEmutecaVersionList read FSoftList write SetSoftList;

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

{ TfmEmutecaVersionList }

procedure TfmEmutecaVersionList.SetOnItemSelect(AValue: TFEVLItemSelected);
begin
  if FOnItemSelect = AValue then
    Exit;
  FOnItemSelect := AValue;
end;

procedure TfmEmutecaVersionList.SetSoftList(AValue: cEmutecaVersionList);
begin
  if FSoftList=AValue then Exit;
  FSoftList:=AValue;

  UpdateList;
end;

procedure TfmEmutecaVersionList.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pData: ^cEmutecaVersion;
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

procedure TfmEmutecaVersionList.VSTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pData1, pData2: ^cEmutecaVersion;
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

procedure TfmEmutecaVersionList.VSTDblClick(Sender: TObject);
var
  pData: ^cEmutecaVersion;
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

procedure TfmEmutecaVersionList.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pData: ^cEmutecaVersion;
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

procedure TfmEmutecaVersionList.VSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  pData: ^cEmutecaVersion;
begin
  pData := VST.GetNodeData(Node);
  pData^ := SoftList[Node^.Index];
end;

procedure TfmEmutecaVersionList.SetOnDblClick(AValue: TFEVLDblClick);
begin
  if FOnDblClick = AValue then
    Exit;
  FOnDblClick := AValue;
end;

procedure TfmEmutecaVersionList.UpdateList;
begin
  VST.Clear;
  StatusBar1.SimpleText := '';
  if not assigned(SoftList) then
    Exit;

  vst.RootNodeCount := SoftList.Count;
  StatusBar1.SimpleText := Format(rsFmtNItems, [vst.RootNodeCount]);
end;

constructor TfmEmutecaVersionList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  VST.NodeDataSize := SizeOf(cEmutecaVersion);
end;

destructor TfmEmutecaVersionList.Destroy;
begin
  inherited Destroy;
end;

end.
