unit ufLEmuTKSysManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, CheckLst, ActnList, Menus,
  ufCHXChkLstPropEditor,
  ucEmuteca, ucEmutecaSystem,
  ufLEmuTKFullSystemEditor;

resourcestring
  rsSystemName = 'System name [Company: Model (extra)]';

type
  { Frame for System Manager. }

  { TfmLEmuTKSysManager }

  TfmLEmuTKSysManager = class(TfmCHXChkLstPropEditor)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
  private
    FEmuteca: cEmuteca;
    FSysEditor: TfmLEmuTKFullSystemEditor;
    procedure SetEmuteca(AValue: cEmuteca);

  protected

    property SysEditor: TfmLEmuTKFullSystemEditor
      read FSysEditor;

    procedure ClearData; override;
    procedure SetGUIIconsIni(AValue: TFilename); override;
    procedure AddItemToList; override;
    procedure DeleteItemFromList; override;
    procedure ExportList; override;
    procedure ImportList; override;
    procedure OnListClick(aObject: TObject); override;
    procedure OnListClickCheck(aObject: TObject; aBool: Boolean); override;
    procedure SetCheckedAll(aBool: Boolean); override;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure LoadData; override;
    procedure SaveData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSysManager }

procedure TfmLEmuTKSysManager.ClearData;
begin
  inherited ClearData;

  SysEditor.System := nil;
  clbPropItems.Clear;
end;

procedure TfmLEmuTKSysManager.SetGUIIconsIni(AValue: TFilename);
begin
  inherited SetGUIIconsIni(AValue);
  SysEditor.GUIIconsIni := self.GUIIconsIni;
end;

procedure TfmLEmuTKSysManager.SetCheckedAll(aBool: Boolean);
var
  i: integer;
  aSystem: cEmutecaSystem;
begin
  if not assigned(Emuteca) then
    Exit;

  i := 0;
  while i < Emuteca.SystemManager.FullList.Count do
  begin
    aSystem := cEmutecaSystem(Emuteca.SystemManager.FullList[i]);
    aSystem.Enabled := aBool;
    Inc(i);
  end;
end;

procedure TfmLEmuTKSysManager.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  LoadData;

  self.Enabled := Assigned(Emuteca);
  SysEditor.Emuteca := Emuteca;
end;

procedure TfmLEmuTKSysManager.OnListClick(aObject: TObject);
begin
  SysEditor.System := cEmutecaSystem(aObject);
end;

procedure TfmLEmuTKSysManager.OnListClickCheck(aObject: TObject; aBool: Boolean);
var
  CurrItem: cEmutecaSystem;
begin
  if not assigned(aObject) then
    Exit;

  CurrItem := cEmutecaSystem(aObject);
  CurrItem.Enabled := aBool;
end;

procedure TfmLEmuTKSysManager.AddItemToList;
var
  SystemID: string;
  aSystem: cEmutecaSystem;
begin
  if not assigned(Emuteca) then
    Exit;

  SystemID := Trim(InputBox(actAddItem.Caption, rsSystemName, ''));
  if SystemID = '' then
    Exit;

  aSystem := cEmutecaSystem.Create(nil);
  aSystem.ID := SystemID;
  aSystem.Title := SystemID;
  aSystem.Enabled := True;
  Emuteca.SystemManager.FullList.Add(aSystem);

  LoadData;

  SysEditor.System := aSystem;
end;

procedure TfmLEmuTKSysManager.DeleteItemFromList;
begin
  if not assigned(Emuteca) then
    Exit;
  if clbPropItems.ItemIndex = -1 then
    exit;

  SysEditor.System := nil;
  try
  Emuteca.SystemManager.FullList.Remove(
    cEmutecaSystem(clbPropItems.Items.Objects[clbPropItems.ItemIndex]));

  finally
  LoadData;
  end;
end;

procedure TfmLEmuTKSysManager.ExportList;
begin
  if not assigned(Emuteca) then
    Exit;

  if not SaveDialog1.Execute then
    Exit;

  Emuteca.SystemManager.SaveToFileIni(SaveDialog1.FileName, True);
end;

procedure TfmLEmuTKSysManager.ImportList;
begin
   if not assigned(Emuteca) then
    Exit;

  if not OpenDialog1.Execute then
    Exit;

  Emuteca.SystemManager.LoadFromFileIni(OpenDialog1.FileName);
end;

procedure TfmLEmuTKSysManager.LoadData;
var
  i: integer;
begin
  ClearData;

  if not assigned(Emuteca) then
    Exit;

  Emuteca.SystemManager.AssingAllTo(clbPropItems.Items);
  i := 0;
  while i < clbPropItems.Items.Count do
  begin
    clbPropItems.Checked[i] :=
      cEmutecaSystem(clbPropItems.Items.Objects[i]).Enabled;
    Inc(i);
  end;
end;

procedure TfmLEmuTKSysManager.SaveData;
begin
  if not assigned(Emuteca) then
    Exit;

  // Automatilly save to file
  Emuteca.SystemManager.SaveToFileIni('', False);
end;

constructor TfmLEmuTKSysManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSysEditor := TfmLEmuTKFullSystemEditor.Create(Self);
  SysEditor.SaveButtons := True;
  SysEditor.ButtonClose := False;
  SysEditor.Align := alClient;
  SysEditor.Parent := Self;
end;

destructor TfmLEmuTKSysManager.Destroy;
begin
  inherited Destroy;
end;

end.
