unit ufEmutecaSystemCBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ufCHXFrame,
  ucEmutecaSystemList, ucEmutecaSystem;

resourcestring
  rsAllSystems = 'All Systems';
  rsSelectSystem = 'Select a System';

type
  TETKSysCBXFirstItem = (ETKSysCBXFINone, ETKSysCBXFISelect, ETKSysCBXFIAll);

  { TfmEmutecaSystemCBX }

  TfmEmutecaSystemCBX = class(TfmCHXFrame)
    cbxSystem: TComboBox;
    procedure cbxSystemChange(Sender: TObject);

  private
    FFirstItem: TETKSysCBXFirstItem;
    FOnSelectSystem: TEmutecaReturnSystemCB;
    FSelectedSystem: cEmutecaSystem;
    FSystemList: cEmutecaSystemList;
    procedure SetFirstItem(AValue: TETKSysCBXFirstItem);
    procedure SetOnSelectSystem(AValue: TEmutecaReturnSystemCB);
    procedure SetSelectedSystem(AValue: cEmutecaSystem);
    procedure SetSystemList(AValue: cEmutecaSystemList);

  protected
    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property SystemList: cEmutecaSystemList
      read FSystemList write SetSystemList;
    property SelectedSystem: cEmutecaSystem
      read FSelectedSystem write SetSelectedSystem;
    property OnSelectSystem: TEmutecaReturnSystemCB
      read FOnSelectSystem write SetOnSelectSystem;

    property FirstItem: TETKSysCBXFirstItem read FFirstItem write SetFirstItem;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemCBX }

procedure TfmEmutecaSystemCBX.cbxSystemChange(Sender: TObject);
begin
  // We don't need to call SetSelectedSystem
  if cbxSystem.ItemIndex <> -1 then
    FSelectedSystem := cEmutecaSystem(
      cbxSystem.Items.Objects[cbxSystem.ItemIndex])
  else
    FSelectedSystem := nil;

  if Assigned(OnSelectSystem) then
    {Var := } OnSelectSystem(SelectedSystem);
end;

procedure TfmEmutecaSystemCBX.SetOnSelectSystem(AValue:
  TEmutecaReturnSystemCB);
begin
  if FOnSelectSystem = AValue then
    Exit;
  FOnSelectSystem := AValue;
end;

procedure TfmEmutecaSystemCBX.SetFirstItem(AValue: TETKSysCBXFirstItem);
begin
  if FFirstItem = AValue then
    Exit;
  FFirstItem := AValue;

  if cbxSystem.Items.Count = 0 then
    Exit;

  if not assigned(cbxSystem.Items.Objects[0]) then
  begin  // Already have rsSelectSystem or rsAllSystems
    case FirstItem of
      ETKSysCBXFISelect: cbxSystem.Items[0] := rsSelectSystem;
      ETKSysCBXFIAll: cbxSystem.Items[0] := rsAllSystems;
      else
        cbxSystem.Items.Delete(0);
    end;
  end
  else
  begin // Don't have rsSelectSystem or rsAllSystems
    case FirstItem of
      ETKSysCBXFISelect: cbxSystem.Items.Insert(0, rsSelectSystem);
      ETKSysCBXFIAll: cbxSystem.Items.Insert(0, rsAllSystems);
      else
        ;
    end;
  end;
end;

procedure TfmEmutecaSystemCBX.SetSelectedSystem(AValue: cEmutecaSystem);
var
  aPos: integer;
begin
  if FSelectedSystem = AValue then
    Exit;
  FSelectedSystem := AValue;

  if not assigned(SelectedSystem) then
  begin
    if cbxSystem.Items.Count = 0 then Exit;

    cbxSystem.ItemIndex := 0;
    if FirstItem = ETKSysCBXFINone then
    begin
      FSelectedSystem := cEmutecaSystem(cbxSystem.Items.Objects[0]);
    end;
    Exit;
  end;

  aPos := cbxSystem.Items.IndexOfObject(SelectedSystem);

  if aPos = -1 then
  begin
    // Uhm....
    aPos := cbxSystem.Items.AddObject(SelectedSystem.Title, SelectedSystem);
  end;

  cbxSystem.ItemIndex := aPos;
end;

procedure TfmEmutecaSystemCBX.SetSystemList(AValue: cEmutecaSystemList);
begin
  if FSystemList = AValue then
    Exit;
  FSystemList := AValue;

  LoadFrameData;
end;

procedure TfmEmutecaSystemCBX.ClearFrameData;
begin
  cbxSystem.Clear;
end;

procedure TfmEmutecaSystemCBX.LoadFrameData;
begin
  Enabled := Assigned(SystemList);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  cbxSystem.Clear;
  SystemList.AssignToStrLst(cbxSystem.Items);

  if cbxSystem.Items.Count = 0 then
  begin
    cbxSystem.ItemIndex := -1;
    Exit;
    end;

  case FirstItem of
    ETKSysCBXFISelect: cbxSystem.Items.Insert(0, rsSelectSystem);
    ETKSysCBXFIAll: cbxSystem.Items.Insert(0, rsAllSystems);
    else
      ;
  end;

  cbxSystem.ItemIndex := cbxSystem.Items.IndexOfObject(SelectedSystem);
end;

constructor TfmEmutecaSystemCBX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaSystemCBX.Destroy;
begin
  inherited Destroy;
end;

end.
