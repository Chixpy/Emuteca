unit ufLEmuTKEmuManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, CheckLst, ActnList, Menus,
  uCHXStrUtils,
  ufrCHXForm, ufCHXChkLstPropEditor,
  uEmutecaCommon,
  ucEmutecaEmulatorManager, ucEmutecaEmulator,
  uLEmuTKCommon,
  ufLEmuTKFullEmuEditor;

resourcestring
  rsEmulatorName = 'Emulator name.';

type
  { Frame for Emulator Manager. }

  { TfmLEmuTKEmuManager }

  TfmLEmuTKEmuManager = class(TfmCHXChkLstPropEditor)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;

  private
    FfmEmuEditor: TfmLEmuTKFullEmuEditor;
    FEmuManager: cEmutecaEmulatorManager;
    FSHA1Folder: string;
    procedure SetEmuManager(AValue: cEmutecaEmulatorManager);
    procedure SetSHA1Folder(AValue: string);

  protected
    property fmEmuEditor: TfmLEmuTKFullEmuEditor read FfmEmuEditor;

    procedure AddItemToList; override;
    procedure DeleteItemFromList; override;
    procedure ExportList; override;
    procedure ImportList; override;
    procedure OnListClick(aObject: TObject); override;
    procedure OnListClickCheck(aObject: TObject; aBool: boolean); override;
    procedure SetCheckedAll(aBool: boolean); override;

  protected
    procedure DoClearFrameData; override;
    procedure DoLoadFrameData;
    procedure DOSaveFrameData;

  public
    property EmuManager: cEmutecaEmulatorManager
      read FEmuManager write SetEmuManager;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    // Creates a form with Emulator Manager.
    class function SimpleForm(aEmuManager: cEmutecaEmulatorManager;
      aSHA1Folder: string; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKEmuManager }

procedure TfmLEmuTKEmuManager.SetEmuManager(AValue: cEmutecaEmulatorManager);
begin
  if FEmuManager = AValue then
    Exit;
  FEmuManager := AValue;

  LoadFrameData;
end;

procedure TfmLEmuTKEmuManager.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
  fmEmuEditor.SHA1Folder := SHA1Folder;
end;

procedure TfmLEmuTKEmuManager.DoClearFrameData;
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

  fmEmuEditor.Emulator := aEmulator;
end;

procedure TfmLEmuTKEmuManager.DeleteItemFromList;
var
  aEmulator: cEmutecaEmulator;
begin
  if not assigned(EmuManager) then
    Exit;

  if clbPropItems.ItemIndex = -1 then
    Exit;

  fmEmuEditor.Emulator := nil;

  aEmulator := cEmutecaEmulator(clbPropItems.Items.Objects[clbPropItems.ItemIndex]);
  try
    // If already in enabled list remove here too.
    EmuManager.EnabledList.Remove(aEmulator);

    // FullList frees the object too.
    EmuManager.FullList.Remove(aEmulator);
    //aEmulator.Free;
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

procedure TfmLEmuTKEmuManager.DoLoadFrameData;
var
  i: integer;
begin
  Enabled := Assigned(EmuManager);


    if not Enabled then
    begin
      ClearFrameData;
      Exit;
    end;

  clbPropItems.Clear;
  EmuManager.FullList.AssignToStrLst(clbPropItems.Items);

  i := 0;
  while i < clbPropItems.Items.Count do
  begin
    clbPropItems.Checked[i] :=
      cEmutecaEmulator(clbPropItems.Items.Objects[i]).Enabled;
    Inc(i);
  end;
end;

procedure TfmLEmuTKEmuManager.DoSaveFrameData;
begin
  if not assigned(EmuManager) then
    Exit;

   // Saving current system data
  if assigned(fmEmuEditor.Emulator) then fmEmuEditor.SaveFrameData;

  EmuManager.UpdateEnabledList;
end;

class function TfmLEmuTKEmuManager.SimpleForm(
  aEmuManager: cEmutecaEmulatorManager; aSHA1Folder: string;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmLEmuTKEmuManager;
begin
    Result := mrNone;

      Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmLEmuTKEmuManager';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Emulator Manager']);

    aFrame := TfmLEmuTKEmuManager.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.SHA1Folder := aSHA1Folder;
    aFrame.EmuManager := aEmuManager;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

procedure TfmLEmuTKEmuManager.OnListClick(aObject: TObject);
begin
  fmEmuEditor.Emulator := cEmutecaEmulator(aObject);
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

  FfmEmuEditor := TfmLEmuTKFullEmuEditor.Create(Self);
  fmEmuEditor.SaveButtons := True;
  fmEmuEditor.ButtonClose := False;
  fmEmuEditor.Align := alClient;
  fmEmuEditor.Parent := Self;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmLEmuTKEmuManager.Destroy;
begin
  inherited Destroy;
end;

end.
