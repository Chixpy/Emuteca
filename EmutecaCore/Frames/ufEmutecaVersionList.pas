unit ufEmutecaVersionList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, ComCtrls,
  ActnList, Menus, LazUTF8,
  uEmutecaCommon, ucEmuteca, ucEmutecaVersion,
  ufEmutecaActAddVersion;

type
  TFEVLItemSelected = procedure(const aVersion: cEmutecaVersion) of object;
  TFEVLDblClick = procedure(const aVersion: cEmutecaVersion) of object;

  { TfmEmutecaVersionList }

  TfmEmutecaVersionList = class(TFrame)
    actAddVersion: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    VST: TVirtualStringTree;
    procedure actAddVersionExecute(Sender: TObject);
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
    FEmuteca: cEmuteca;
    FOnDblClick: TFEVLDblClick;
    FOnItemSelect: TFEVLItemSelected;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetOnDblClick(AValue: TFEVLDblClick);
    procedure SetOnItemSelect(AValue: TFEVLItemSelected);



  protected
    procedure UpdateList;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    {< Sincerelly, I don't like this. But is needed for many actions...}

    property OnItemSelect: TFEVLItemSelected
      read FOnItemSelect write SetOnItemSelect;
    //< CallBack function when item selected.
    property OnDblClick: TFEVLDblClick read FOnDblClick write SetOnDblClick;

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

procedure TfmEmutecaVersionList.actAddVersionExecute(Sender: TObject);
var
  aForm: TForm;
  aFrame: TfmActAddVersion;
begin
  Application.CreateForm(TForm, aForm);

  aForm.Position := poMainFormCenter;
  aForm.Caption := Format(rsFmtWindowCaption,
    [Application.Title, actAddVersion.Caption]);

  aFrame := TfmActAddVersion.Create(aForm);
  aFrame.Parent := aForm;
  aFrame.Align := alClient;
  aFrame.Emuteca := Emuteca;
  {
  aFrame.IconsIni := Emuteca.Config.ImagesFolder +
    Emuteca.Config.IconsSubfolder + Emuteca.Config.IconsIniFile;
  }
  aForm.ShowModal;
  FreeAndNil(aForm);
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
    1: // Description
      Result := UTF8CompareText(pData1^.Description, pData2^.Description);
    2: // Folder
      Result := UTF8CompareText(pData1^.Folder, pData2^.Folder);
    3: // File;
      Result := UTF8CompareText(pData1^.FileName, pData2^.FileName);
    else // Title
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
    1: // Description
      CellText := pData^.Description;
    2: // Folder
      CellText := pData^.Folder;
    3: // File
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
  pData^ := Emuteca.SoftManager.List.Items[Node^.Index];
end;

procedure TfmEmutecaVersionList.SetOnDblClick(AValue: TFEVLDblClick);
begin
  if FOnDblClick = AValue then
    Exit;
  FOnDblClick := AValue;
end;

procedure TfmEmutecaVersionList.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  //ReadActionsIcons(IconsIni, Self.Name, '', ilActions, ActionList1);

  UpdateList;
end;

procedure TfmEmutecaVersionList.UpdateList;
begin
  VST.Clear;
  StatusBar1.SimpleText := '';
  if not assigned(Emuteca) then
    Exit;

  vst.RootNodeCount := Emuteca.SoftManager.List.Count;
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
