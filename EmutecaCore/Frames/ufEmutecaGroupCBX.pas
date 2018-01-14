unit ufEmutecaGroupCBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ucEmutecaGroupList, ucEmutecaGroup, ufCHXFrame;

type

  { TfmEmutecaGroupCBX }

  // TODO: Think about do it like TfmEmutecaEmulatorCBX

  TfmEmutecaGroupCBX = class(TfmCHXFrame)
    cbxGroup: TComboBox;
    procedure cbxGroupChange(Sender: TObject);
  private
    FGroupList: cEmutecaGroupList;
    FOnSelectGroup: TEmutecaReturnGroupCB;
    FSelectedGroup: cEmutecaGroup;
    procedure SetGroupList(AValue: cEmutecaGroupList);
    procedure SetOnSelectGroup(AValue: TEmutecaReturnGroupCB);
    procedure SetSelectedGroup(AValue: cEmutecaGroup);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;

  public

    property GroupList: cEmutecaGroupList read FGroupList write SetGroupList;
    {< List of groups. }

    property SelectedGroup: cEmutecaGroup
      read FSelectedGroup write SetSelectedGroup;
    {< Returns current selected group or select it in cbx. }

    property OnSelectGroup: TEmutecaReturnGroupCB
      read FOnSelectGroup write SetOnSelectGroup;
    {< Callback when selecting a group. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaGroupCBX }

procedure TfmEmutecaGroupCBX.cbxGroupChange(Sender: TObject);
begin
  // We don't need to call SetSelectedSystem
  if cbxGroup.ItemIndex <> -1 then
    FSelectedGroup := cEmutecaGroup(
      cbxGroup.Items.Objects[cbxGroup.ItemIndex])
  else
    FSelectedGroup := nil;

  if Assigned(OnSelectGroup) then
    {Var := } OnSelectGroup(SelectedGroup);
end;

procedure TfmEmutecaGroupCBX.SetOnSelectGroup(AValue: TEmutecaReturnGroupCB);
begin
  if FOnSelectGroup = AValue then
    Exit;
  FOnSelectGroup := AValue;
end;

procedure TfmEmutecaGroupCBX.SetGroupList(AValue: cEmutecaGroupList);
begin
  if FGroupList = AValue then
    Exit;
  FGroupList := AValue;
  LoadFrameData;
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

procedure TfmEmutecaGroupCBX.DoClearFrameData;
begin
  cbxGroup.Clear;
end;

procedure TfmEmutecaGroupCBX.DoLoadFrameData;
begin
  Enabled := Assigned(GroupList);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  cbxGroup.Clear;
  GroupList.AssignToStrLst(cbxGroup.Items);

  if cbxGroup.Items.Count = 0 then
  begin
    cbxGroup.ItemIndex := -1;
    Exit;
  end;

  cbxGroup.ItemIndex := cbxGroup.Items.IndexOfObject(SelectedGroup);
end;

constructor TfmEmutecaGroupCBX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
end;

destructor TfmEmutecaGroupCBX.Destroy;
begin
  inherited Destroy;
end;

end.
