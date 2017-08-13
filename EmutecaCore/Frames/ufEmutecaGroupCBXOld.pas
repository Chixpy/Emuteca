unit ufEmutecaGroupCBXOld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ucEmutecaGroupManager, ucEmutecaGroup;

type

  { TfmEmutecaGroupCBX }
  // TODO: TCHXFrame

  TfmEmutecaGroupCBX = class(TFrame)
    cbxGroup: TComboBox;
    procedure cbxGroupChange(Sender: TObject);

  private
    FSelectedGroup: cEmutecaGroup;
    FOnSelectGroup: TEmutecaReturnGroupCB;
    FGroupManager: cEmutecaGroupManager;
    procedure SetSelectedGroup(AValue: cEmutecaGroup);
    procedure SetOnSelectGroup(AValue: TEmutecaReturnGroupCB);
    procedure SetGroupManager(AValue: cEmutecaGroupManager);

  protected

  public
    property GroupManager: cEmutecaGroupManager read FGroupManager write SetGroupManager;
    {< List of parents observed. }

    property SelectedGroup: cEmutecaGroup
      read FSelectedGroup write SetSelectedGroup;
    {< Returns current selected parent or select it in cbx. }

    property OnSelectGroup: TEmutecaReturnGroupCB
      read FOnSelectGroup write SetOnSelectGroup;
    {< Callback when selecting a parent. }

    procedure ClearData;
    procedure LoadData;
    procedure SaveData;

    procedure SelectGroupByID(aGroupKey: string);
    //< Select a group by ID, or only set the text in the CBX

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaGroupCBX }

procedure TfmEmutecaGroupCBX.cbxGroupChange(Sender: TObject);
begin
  if cbxGroup.ItemIndex <> -1 then
    SelectedGroup := cEmutecaGroup(cbxGroup.Items.Objects[cbxGroup.ItemIndex])
  else
    SelectedGroup := nil;

  if Assigned(OnSelectGroup) then
    {Var := } OnSelectGroup(SelectedGroup);
end;

procedure TfmEmutecaGroupCBX.SetOnSelectGroup(AValue: TEmutecaReturnGroupCB);
begin
  if FOnSelectGroup = AValue then
    Exit;
  FOnSelectGroup := AValue;
end;

procedure TfmEmutecaGroupCBX.SetSelectedGroup(AValue: cEmutecaGroup);
var
  aPos: integer;
begin
  if FSelectedGroup = AValue then
    Exit;
  FSelectedGroup := AValue;

  if not assigned(SelectedGroup) then
  begin
    cbxGroup.ItemIndex := -1;
    Exit;
  end;

  aPos := cbxGroup.Items.IndexOfObject(SelectedGroup);
  if aPos = -1 then
  begin
    // Uhm....
    aPos := cbxGroup.Items.AddObject(SelectedGroup.Title, SelectedGroup);
  end;
  cbxGroup.ItemIndex := aPos;
end;

procedure TfmEmutecaGroupCBX.SetGroupManager(AValue: cEmutecaGroupManager);
begin
  if FGroupManager = AValue then
    Exit;
  FGroupManager := AValue;

  LoadData;
end;

procedure TfmEmutecaGroupCBX.ClearData;
begin
  cbxGroup.Clear;
end;

procedure TfmEmutecaGroupCBX.LoadData;
begin
  Enabled := Assigned(GroupManager);

  ClearData;

   if not Enabled then
    Exit;

   GroupManager.FullList.AssignToStrLst(cbxGroup.Items);
end;

procedure TfmEmutecaGroupCBX.SaveData;
begin

end;

procedure TfmEmutecaGroupCBX.SelectGroupByID(aGroupKey: string);
var
  i: integer;
begin
  cbxGroup.ItemIndex := -1;

  if aGroupKey = '' then
    Exit;

  i := 0;
  while (i < cbxGroup.Items.Count) and (cbxGroup.ItemIndex = -1) do
  begin
    if cEmutecaGroup(cbxGroup.Items.Objects[i]).MatchID(aGroupKey) then
      cbxGroup.ItemIndex := i;
    Inc(i);
  end;

  if cbxGroup.ItemIndex = -1 then
    cbxGroup.Text := aGroupKey;
end;

constructor TfmEmutecaGroupCBX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaGroupCBX.Destroy;
begin
  inherited Destroy;
end;

end.
