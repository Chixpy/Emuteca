unit ufLEmuTKEmuManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, CheckLst, ActnList, Menus,
  ufCHXChkLstPropEditor,
  ucEmutecaEmulatorManager, ucEmutecaEmulator,
  ufEmutecaEmulatorEditor;

resourcestring
  rsEmulatorName = 'Emulator name';

type

  { TfmLEmuTKEmuManager }

  TfmLEmuTKEmuManager = class(TfmCHXChkLstPropEditor)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;

  private
    FEmuEditor: TfmEmutecaEmulatorEditor;
    FEmuManager: cEmutecaEmulatorManager;
    procedure SetEmuEditor(AValue: TfmEmutecaEmulatorEditor);
    procedure SetEmuManager(AValue: cEmutecaEmulatorManager);

  protected
    property EmuEditor: TfmEmutecaEmulatorEditor
      read FEmuEditor write SetEmuEditor;

    procedure AddItemToList; override;
    procedure DeleteItemFromList; override;
    procedure ExportList; override;
    procedure ImportList; override;
    procedure OnListClick(aObject: TObject); override;
    procedure OnListClickCheck(aObject: TObject; aBool: boolean); override;
    procedure SetCheckedAll(aBool: boolean); override;

  protected
    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property EmuManager: cEmutecaEmulatorManager
      read FEmuManager write SetEmuManager;

    procedure SaveFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKEmuManager }

procedure TfmLEmuTKEmuManager.SetEmuEditor(AValue: TfmEmutecaEmulatorEditor);
begin
  if FEmuEditor = AValue then
    Exit;
  FEmuEditor := AValue;
end;

procedure TfmLEmuTKEmuManager.SetEmuManager(AValue: cEmutecaEmulatorManager);
begin
  if FEmuManager = AValue then
    Exit;
  FEmuManager := AValue;

  LoadFrameData;
end;

procedure TfmLEmuTKEmuManager.ClearFrameData;
begin
  inherited ClearFrameData;
end;

procedure TfmLEmuTKEmuManager.SetCheckedAll(aBool: boolean);
var
  i: integer;
  aEmulator: cEmutecaEmulator;
begin
  if not assigned(EmuManager) then
    Exit;

  i := 0;
  while i < EmuManager.FullList.Count do
  begin
    aEmulator := cEmutecaEmulator(EmuManager.FullList[i]);
    aEmulator.Enabled := aBool;
    Inc(i);
  end;
end;

procedure TfmLEmuTKEmuManager.AddItemToList;
var
  EmulatorID: string;
  aEmulator: cEmutecaEmulator;
begin
  if not assigned(EmuManager) then
    Exit;
  EmulatorID := Trim(InputBox(actAddItem.Caption, rsEmulatorName, ''));
  if EmulatorID = '' then
    Exit;

  aEmulator := cEmutecaEmulator.Create(nil);
  aEmulator.ID := EmulatorID;
  aEmulator.EmulatorName := EmulatorID;
  aEmulator.Enabled := True;
  EmuManager.FullList.Add(aEmulator);

  LoadFrameData;

  EmuEditor.Emulator := aEmulator;
end;

procedure TfmLEmuTKEmuManager.DeleteItemFromList;
begin
  if not assigned(EmuManager) then
    Exit;
  if clbPropItems.ItemIndex = -1 then
    exit;

  EmuEditor.Emulator := nil;
  try
    EmuManager.FullList.Remove(cEmutecaEmulator(
      clbPropItems.Items.Objects[clbPropItems.ItemIndex]));
  finally
    LoadFrameData;
  end;
end;

procedure TfmLEmuTKEmuManager.ExportList;
begin
  if not assigned(EmuManager) then
    Exit;

  if not SaveDialog1.Execute then
    Exit;

  EmuManager.SaveToFileIni(SaveDialog1.FileName, True);
end;

procedure TfmLEmuTKEmuManager.ImportList;
begin
  if not assigned(EmuManager) then
    Exit;

  if not OpenDialog1.Execute then
    Exit;

  EmuManager.LoadFromFileIni(OpenDialog1.FileName);
end;

procedure TfmLEmuTKEmuManager.LoadFrameData;
var
  i: integer;
begin
  ClearFrameData;

  Enabled := Assigned(EmuManager);

  if not Enabled then
    Exit;

  EmuManager.FullList.AssignToStrLst(clbPropItems.Items);

  i := 0;
  while i < clbPropItems.Items.Count do
  begin
    clbPropItems.Checked[i] :=
      cEmutecaEmulator(clbPropItems.Items.Objects[i]).Enabled;
    Inc(i);
  end;
end;

procedure TfmLEmuTKEmuManager.SaveFrameData;
begin
  if not assigned(EmuManager) then
    Exit;

  // Automatilly save to file
  EmuManager.SaveToFileIni('', False);
end;

procedure TfmLEmuTKEmuManager.OnListClick(aObject: TObject);
begin
  EmuEditor.Emulator := cEmutecaEmulator(aObject);
end;

procedure TfmLEmuTKEmuManager.OnListClickCheck(aObject: TObject;
  aBool: boolean);
var
  CurrItem: cEmutecaEmulator;
begin
  if not assigned(aObject) then
    Exit;

  CurrItem := cEmutecaEmulator(aObject);
  CurrItem.Enabled := aBool;
end;

constructor TfmLEmuTKEmuManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FEmuEditor := TfmEmutecaEmulatorEditor.Create(Self);
  EmuEditor.Parent := Self;
  EmuEditor.Align := alClient;
end;

destructor TfmLEmuTKEmuManager.Destroy;
begin
  inherited Destroy;
end;

end.
