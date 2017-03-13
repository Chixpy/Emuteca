unit ufEmutecaSystemCBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ucEmutecaSystem;

type

  { TfmEmutecaSystemCBX }

  TfmEmutecaSystemCBX = class(TFrame)
    cbxSystem: TComboBox;
    procedure cbxSystemChange(Sender: TObject);

  private
    FSelectedSystem: cEmutecaSystem;
    FOnSelectSystem: TEmutecaReturnSystemCB;
    FSystemList: cEmutecaSystemList;
    procedure SetSelectedSystem(AValue: cEmutecaSystem);
    procedure SetOnSelectSystem(AValue: TEmutecaReturnSystemCB);
    procedure SetSystemList(AValue: cEmutecaSystemList);

  protected
    procedure UpdateSystems;
    {< Update drop down list. }

  public
    property SystemList: cEmutecaSystemList
      read FSystemList write SetSystemList;
    {< List of systems observed. }

    property SelectedSystem: cEmutecaSystem
      read FSelectedSystem write SetSelectedSystem;
    {< Returns current selected system or select it in cbx. }

    property OnSelectSystem: TEmutecaReturnSystemCB
      read FOnSelectSystem write SetOnSelectSystem;
    {< Callback when selecting a system. }

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

  FSystemList := AValue;

  UpdateSystems;
end;

constructor TfmEmutecaSystemCBX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaSystemCBX.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmutecaSystemCBX.cbxSystemChange(Sender: TObject);
begin
  if cbxSystem.ItemIndex <> -1 then
    SelectedSystem := cEmutecaSystem(
      cbxSystem.Items.Objects[cbxSystem.ItemIndex])
  else
    SelectedSystem := nil;

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

procedure TfmEmutecaSystemCBX.SetSelectedSystem(AValue: cEmutecaSystem);
var
  aPos: integer;
begin
  if FSelectedSystem = AValue then
    Exit;
  FSelectedSystem := AValue;

  if not assigned(SelectedSystem) then
  begin
    cbxSystem.ItemIndex := 0;
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
  // Inserting 'All systems option'
  { TODO: Make this optional... }
  cbxSystem.Items.Insert(0, rsAllSystems);
  cbxSystem.ItemIndex := 0;
  cbxSystem.Items.EndUpdate;
end;

end.
