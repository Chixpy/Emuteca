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

    property SysEditor: TfmLEmuTKFullSystemEditor read FSysEditor;

    procedure SetGUIIconsIni(AValue: string); override;
    procedure AddItemToList; override;
    procedure DeleteItemFromList; override;
    procedure ExportList; override;
    procedure ImportList; override;
    procedure OnListClick(aObject: TObject); override;
    procedure OnListClickCheck(aObject: TObject; aBool: boolean); override;
    procedure SetCheckedAll(aBool: boolean); override;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure ClearData; override;
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

procedure TfmLEmuTKSysManager.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);
  SysEditor.GUIIconsIni := GUIIconsIni;
end;

procedure TfmLEmuTKSysManager.SetCheckedAll(aBool: boolean);
var
  i: integer;
  aSystem: cEmutecaSystem;
begin
  if not assigned(Emuteca) then
    Exit;

  i := 0;
  while i < Emuteca.SystemManager.FullList.Count do
  begin
    aSystem := Emuteca.SystemManager.FullList[i];
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

  Enabled := Assigned(Emuteca);
  SysEditor.Emuteca := Emuteca;
end;

procedure TfmLEmuTKSysManager.OnListClick(aObject: TObject);
begin
  SysEditor.System := cEmutecaSystem(aObject);
end;

procedure TfmLEmuTKSysManager.OnListClickCheck(aObject: TObject;
  aBool: boolean);
begin

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

  // TODO: Don't Add systems on the fly, only when saved
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
    // TODO: Don't delete systems on the fly, only when saved
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

  Emuteca.SystemManager.FullList.AssignToStrLst(clbPropItems.Items);
  i := 0;
  while i < clbPropItems.Items.Count do
  begin
    clbPropItems.Checked[i] :=
      cEmutecaSystem(clbPropItems.Items.Objects[i]).Enabled;
    Inc(i);
  end;
end;

procedure TfmLEmuTKSysManager.SaveData;
var
  i: integer;
  aSystem: cEmutecaSystem;
begin
  if not assigned(Emuteca) then
    Exit;

  // HACK: Preventing lost data from changed systems,
  //   saving current data or reloading from disk.
  i := 0;
  while i < clbPropItems.Items.Count do
  begin
    aSystem := cEmutecaSystem(clbPropItems.Items.Objects[i]);

    if aSystem.Enabled <> clbPropItems.Checked[i] then
    begin
      if aSystem.FileName = '' then
        aSystem.FileName := aSystem.Title;

      if aSystem.Enabled then
      begin
        // Saving soft of previously enabled systems ...
        aSystem.SaveLists(Emuteca.SystemManager.SysDataFolder + aSystem.FileName, False);
      end
      else
      begin
        // ... loading soft of previously disabled systems
        aSystem.LoadLists(Emuteca.SystemManager.SysDataFolder + aSystem.FileName);
      end;
      aSystem.Enabled := clbPropItems.Checked[i];
     end;

    Inc(i);
  end;

  Emuteca.SaveData;
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
