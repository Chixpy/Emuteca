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
    procedure SetSysEditor(AValue: TfmLEmuTKFullSystemEditor);

  protected
    property SysEditor: TfmLEmuTKFullSystemEditor
      read FSysEditor write SetSysEditor;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure AddItemToList; override;
    procedure DeleteItemFromList; override;
    procedure ExportList; override;
    procedure ImportList; override;
    procedure LoadList; override;
    procedure SaveList; override;
    procedure OnListCheckAll; override;
    procedure OnListUncheckAll; override;
    procedure OnListClick; override;
    procedure OnListClickCheck; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSysManager }

procedure TfmLEmuTKSysManager.SetSysEditor(AValue: TfmLEmuTKFullSystemEditor);
begin
  if FSysEditor = AValue then
    Exit;
  FSysEditor := AValue;

  if Assigned(SysEditor) then
    SysEditor.Emuteca := Emuteca;
end;

procedure TfmLEmuTKSysManager.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  LoadList;

  if Assigned(SysEditor) then
    SysEditor.Emuteca := Emuteca;
end;

procedure TfmLEmuTKSysManager.OnListClick;
begin
  if not assigned(SysEditor) then
    Exit;

  if clbPropItems.ItemIndex = -1 then
    SysEditor.System := nil
  else
    SysEditor.System := cEmutecaSystem(
      clbPropItems.Items.Objects[clbPropItems.ItemIndex]);
end;

procedure TfmLEmuTKSysManager.OnListClickCheck;
var
  CurrItem: cEmutecaSystem;
begin
  CurrItem := nil;

  if clbPropItems.ItemIndex > -1 then
    CurrItem := cEmutecaSystem(
      clbPropItems.Items.Objects[clbPropItems.ItemIndex]);

  if not assigned(CurrItem) then
    Exit;

  CurrItem.Enabled := clbPropItems.Checked[clbPropItems.ItemIndex];
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

  LoadList;

  // TODO Autoselecting last item.
  {ñññ
  CheckListBox1.ItemIndex := CheckListBox1.Items.IndexOf(SystemID);
  CheckListBox1.Checked[CheckListBox1.ItemIndex] := aSystem.Enabled;
  SelectItem;
  }
end;

procedure TfmLEmuTKSysManager.DeleteItemFromList;
begin
  if not assigned(Emuteca) then
    Exit;
  if clbPropItems.ItemIndex = -1 then
    exit;

  SysEditor.System := nil;
  Emuteca.SystemManager.FullList.Remove(
    cEmutecaSystem(clbPropItems.Items.Objects[clbPropItems.ItemIndex]));
  LoadList;
end;

procedure TfmLEmuTKSysManager.ExportList;
begin
  if not assigned(Emuteca) then
    Exit;

  if not SaveDialog1.Execute then
    Exit;

  Emuteca.SystemManager.SaveToFile(SaveDialog1.FileName, True);
end;

procedure TfmLEmuTKSysManager.ImportList;
begin
   if not assigned(Emuteca) then
    Exit;

  if not OpenDialog1.Execute then
    Exit;

  Emuteca.SystemManager.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfmLEmuTKSysManager.LoadList;
var
  i: integer;
begin
  SysEditor.System := nil;

  clbPropItems.Clear;

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

procedure TfmLEmuTKSysManager.OnListCheckAll;
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
    aSystem.Enabled := True;
    Inc(i);
  end;
  LoadList;
end;

procedure TfmLEmuTKSysManager.OnListUncheckAll;
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
    aSystem.Enabled := False;
    Inc(i);
  end;
  LoadList;
end;

procedure TfmLEmuTKSysManager.SaveList;
begin
  if not assigned(Emuteca) then
    Exit;

  // Automatilly save to file
  Emuteca.SystemManager.SaveToFile('', False);
end;

constructor TfmLEmuTKSysManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSysEditor := TfmLEmuTKFullSystemEditor.Create(Self);
  SysEditor.Parent := Self;
  SysEditor.Align := alClient;
end;

destructor TfmLEmuTKSysManager.Destroy;
begin
  inherited Destroy;
end;

end.
