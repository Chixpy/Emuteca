unit ufEmutecaSystemCBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ucEmutecaSystem;

type

  { TfmEmutecaSystemCBX }

  TfmEmutecaSystemCBX = class(TFrame, IFPObserver)
    cbxSystem: TComboBox;
    procedure cbxSystemChange(Sender: TObject);

  private
    FCurrentSystem: cEmutecaSystem;
    FOnSelectSystem: TEmutecaReturnSystemCB;
    FSystemList: cEmutecaSystemList;
    procedure SetCurrentSystem(AValue: cEmutecaSystem);
    procedure SetOnSelectSystem(AValue: TEmutecaReturnSystemCB);
    procedure SetSystemList(AValue: cEmutecaSystemList);

  protected
    procedure UpdateSystems;
    {< Update drop down list. }
  public
    property SystemList: cEmutecaSystemList
      read FSystemList write SetSystemList;
    {< List of systems observed. }

    property CurrentSystem: cEmutecaSystem read FCurrentSystem write SetCurrentSystem;
    {< Returns current selected system or select it in cbx. }

    property OnSelectSystem: TEmutecaReturnSystemCB
      read FOnSelectSystem write SetOnSelectSystem;
    {< Callback when selecting a system. }

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    {< Subject has changed. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemCBX }

procedure TfmEmutecaSystemCBX.SetSystemList(AValue: cEmutecaSystemList);
begin
  if FSystemList = AValue then
    Exit;

  if Assigned(FSystemList) then
    FSystemList.FPODetachObserver(Self);

  FSystemList := AValue;

  if Assigned(SystemList) then
    SystemList.FPOAttachObserver(Self);

  UpdateSystems;
end;

procedure TfmEmutecaSystemCBX.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooChange: UpdateSystems;
    ooFree: SystemList := nil;
    ooAddItem: UpdateSystems; // TODO: Quick add Item
    ooDeleteItem: UpdateSystems; // TODO: Quick delete Item
    ooCustom: UpdateSystems;
  end;
end;

constructor TfmEmutecaSystemCBX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaSystemCBX.Destroy;
begin
  if Assigned(SystemList) then
    SystemList.FPODetachObserver(Self);

  inherited Destroy;
end;

procedure TfmEmutecaSystemCBX.cbxSystemChange(Sender: TObject);
begin
  if Assigned(OnSelectSystem) then
    {Var := } OnSelectSystem(cEmutecaSystem(
      cbxSystem.Items.Objects[cbxSystem.ItemIndex]));

  // TODO: True, change Emuteca.CurrentSystem?
end;

procedure TfmEmutecaSystemCBX.SetOnSelectSystem(AValue:
  TEmutecaReturnSystemCB);
begin
  if FOnSelectSystem = AValue then
    Exit;
  FOnSelectSystem := AValue;
end;

procedure TfmEmutecaSystemCBX.SetCurrentSystem(AValue: cEmutecaSystem);
var
  aPos: Integer;
begin
  if FCurrentSystem = AValue then Exit;
  FCurrentSystem := AValue;

    if not assigned(CurrentSystem) then
  begin
    cbxSystem.ItemIndex := -1;
    Exit;
  end;

  aPos := cbxSystem.Items.IndexOfObject(CurrentSystem);
  if aPos = -1 then
  begin
    // Uhm....
    cbxSystem.ItemIndex :=
      cbxSystem.Items.AddObject(CurrentSystem.Title, CurrentSystem);
  end
  else
    cbxSystem.ItemIndex := aPos;
end;

procedure TfmEmutecaSystemCBX.UpdateSystems;
var
  i: integer;
  aSystem: cEmutecaSystem;
begin
  cbxSystem.Items.BeginUpdate;
  cbxSystem.Clear;
  if assigned(SystemList) then
  begin
    i := 0;
    while i < SystemList.Count do
    begin
      aSystem := cEmutecaSystem(SystemList[i]);
      cbxSystem.Items.AddObject(aSystem.Title, aSystem);
      Inc(i);
    end;
  end;
  cbxSystem.Items.Insert(0, rsAllSystems);
  cbxSystem.ItemIndex := 0;
  cbxSystem.Items.EndUpdate;
end;

end.