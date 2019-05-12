unit ufEmutecaGroupEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList,
  ufCHXPropEditor,
  ucEmutecaGroup;

type

  { TfmEmutecaGroupEditor }

  TfmEmutecaGroupEditor = class(TfmCHXPropEditor, IFPObserver)
    eDeveloper: TComboBox;
    eGroupID: TEdit;
    eMediaFile: TEdit;
    eSortTitle: TEdit;
    eTitle: TEdit;
    eYear: TEdit;
  private
    FGroup: cEmutecaGroup;
    procedure SetGroup(AValue: cEmutecaGroup);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Group: cEmutecaGroup read FGroup write SetGroup;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaGroupEditor }

procedure TfmEmutecaGroupEditor.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;

  // Observed
  if Assigned(FGroup) then
    FGroup.FPODetachObserver(Self);

  FGroup := AValue;

  if Assigned(Group) then
    Group.FPOAttachObserver(Self);

  LoadFrameData;
end;

procedure TfmEmutecaGroupEditor.DoClearFrameData;
begin
  eGroupID.Clear;
  eTitle.Clear;
  eSortTitle.Clear;
  // eDeveloper.Clear; We don't want to clear item list.
  eDeveloper.Text := '';
  eYear.Clear;
  eMediaFile.Clear;
end;

procedure TfmEmutecaGroupEditor.DoLoadFrameData;
begin
  Enabled := Assigned(Group);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eGroupID.Text := Group.ID;
  eTitle.Text := Group.Title;
  eSortTitle.Text := Group.GetActualSortTitle;

  eDeveloper.Text := Group.Developer;
  // Adding to ComboBox List
  if (eDeveloper.ItemIndex = -1) and (Group.Developer <> '') then
    eDeveloper.ItemIndex := eDeveloper.Items.Add(Group.Developer);

  eYear.Text := Group.Year;
  eMediaFile.Text := Group.GetActualMediaFilename;
end;

procedure TfmEmutecaGroupEditor.DoSaveFrameData;
begin
  if not Assigned(Group) then
    Exit;

  Group.ID := eGroupID.Text;
  Group.Title := eTitle.Text;
  Group.SortTitle := eSortTitle.Text;

  Group.Developer := eDeveloper.Text;
  // Adding to ComboBox List
  if (Group.Developer <> '') and
    (eDeveloper.Items.IndexOf(Group.Developer) = -1) then
    eDeveloper.AddItem(Group.Developer, nil);

  Group.Year := eYear.Text;
  Group.MediaFileName := eMediaFile.Text;
end;

procedure TfmEmutecaGroupEditor.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = Group then
    case Operation of
      ooFree: Group := nil
      else
        ;
    end;
end;

constructor TfmEmutecaGroupEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaGroupEditor.Destroy;
begin
    if Assigned(FGroup) then
    FGroup.FPODetachObserver(Self);

  inherited Destroy;
end;

end.
