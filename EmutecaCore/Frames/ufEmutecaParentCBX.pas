unit ufEmutecaParentCBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ucEmutecaParent;

type

  { TfmEmutecaParentCBX }

  TfmEmutecaParentCBX = class(TFrame, IFPObserver)
    cbxParent: TComboBox;
    procedure cbxParentChange(Sender: TObject);

  private
    FCurrentParent: cEmutecaParent;
    FOnSelectParent: TEmutecaReturnParentCB;
    FParentList: cEmutecaParentList;
    procedure SetCurrentParent(AValue: cEmutecaParent);
    procedure SetOnSelectParent(AValue: TEmutecaReturnParentCB);
    procedure SetParentList(AValue: cEmutecaParentList);

  protected
    procedure UpdateParents;
    {< Update drop down list. }

  public
    property ParentList: cEmutecaParentList
      read FParentList write SetParentList;
    {< List of parents observed. }

    property CurrentParent: cEmutecaParent
      read FCurrentParent write SetCurrentParent;
    {< Returns current selected parent or select it in cbx. }

    property OnSelectParent: TEmutecaReturnParentCB
      read FOnSelectParent write SetOnSelectParent;
    {< Callback when selecting a parent. }

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    {< Subject has changed. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaParentCBX }

procedure TfmEmutecaParentCBX.cbxParentChange(Sender: TObject);
begin
  if cbxParent.ItemIndex <> -1 then
    CurrentParent := cEmutecaParent(
      cbxParent.Items.Objects[cbxParent.ItemIndex])
  else
    CurrentParent := nil;

  if Assigned(OnSelectParent) then
    {Var := } OnSelectParent(CurrentParent);

  // TODO: True, change Emuteca.CurrentSystem?
end;

procedure TfmEmutecaParentCBX.SetOnSelectParent(AValue:
  TEmutecaReturnParentCB);
begin
  if FOnSelectParent = AValue then
    Exit;
  FOnSelectParent := AValue;
end;

procedure TfmEmutecaParentCBX.SetCurrentParent(AValue: cEmutecaParent);
var
  aPos: integer;
begin
  if FCurrentParent = AValue then
    Exit;
  FCurrentParent := AValue;

  if not assigned(CurrentParent) then
  begin
    cbxParent.ItemIndex := -1;
    Exit;
  end;

  aPos := cbxParent.Items.IndexOfObject(CurrentParent);
  if aPos = -1 then
  begin
    // Uhm....
    cbxParent.ItemIndex :=
      cbxParent.Items.AddObject(CurrentParent.Title, CurrentParent);
  end
  else
    cbxParent.ItemIndex := aPos;
end;

procedure TfmEmutecaParentCBX.SetParentList(AValue: cEmutecaParentList);
begin
  if FParentList = AValue then
    Exit;

  if Assigned(FParentList) then
    FParentList.FPODetachObserver(Self);

  FParentList := AValue;

  if Assigned(ParentList) then
    ParentList.FPOAttachObserver(Self);

  UpdateParents;
end;

procedure TfmEmutecaParentCBX.UpdateParents;
var
  i: integer;
  aParent: cEmutecaParent;
begin
  cbxParent.Items.BeginUpdate;
  cbxParent.Clear;
  if assigned(ParentList) then
  begin
    i := 0;
    while i < ParentList.Count do
    begin
      aParent := cEmutecaParent(ParentList[i]);
      cbxParent.Items.AddObject(aParent.Title, aParent);
      Inc(i);
    end;
  end;
  cbxParent.Items.EndUpdate;
end;

procedure TfmEmutecaParentCBX.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooChange: UpdateParents;
    ooFree: ParentList := nil;
    ooAddItem: UpdateParents; // TODO: Quick add Item
    ooDeleteItem: UpdateParents; // TODO: Quick delete Item
    ooCustom: UpdateParents;
  end;
end;

constructor TfmEmutecaParentCBX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaParentCBX.Destroy;
begin
  if Assigned(ParentList) then
    ParentList.FPODetachObserver(Self);

  inherited Destroy;
end;

end.
