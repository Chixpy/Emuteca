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
    FOnSelectSystem: TEmutecaReturnSystemCB;
    FSystemList: cEmutecaSystemList;
    procedure SetOnSelectSystem(AValue: TEmutecaReturnSystemCB);
    procedure SetSystemList(AValue: cEmutecaSystemList);

  public
    property SystemList: cEmutecaSystemList
      read FSystemList write SetSystemList;

    property OnSelectSystem: TEmutecaReturnSystemCB
      read FOnSelectSystem write SetOnSelectSystem;

    procedure UpdateSystems;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
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

  if Assigned(FSystemList) then
    FSystemList.FPOAttachObserver(Self);

  UpdateSystems;
end;

procedure TfmEmutecaSystemCBX.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooChange: ;
    ooFree: SystemList := nil;
    ooAddItem: UpdateSystems;
    ooDeleteItem: UpdateSystems;
    ooCustom: ;
  end;
end;

procedure TfmEmutecaSystemCBX.cbxSystemChange(Sender: TObject);
begin
  if Assigned(OnSelectSystem) then
    OnSelectSystem(cEmutecaSystem(
      cbxSystem.Items.Objects[cbxSystem.ItemIndex]));
end;

procedure TfmEmutecaSystemCBX.SetOnSelectSystem(AValue:
  TEmutecaReturnSystemCB);
begin
  if FOnSelectSystem = AValue then
    Exit;
  FOnSelectSystem := AValue;
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
      cbxSystem.Items.AddObject(aSystem.Company + ' - ' + aSystem.Model, aSystem);
      Inc(i);
    end;
  end;
  cbxSystem.Items.Insert(0, rsAllSystems);
  cbxSystem.ItemIndex := 0;
  cbxSystem.Items.EndUpdate;
end;

end.
