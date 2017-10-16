unit ufLEmuTKSysManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, CheckLst, ActnList, Menus,
  uCHXStrUtils,
  ufCHXChkLstPropEditor, ufrCHXForm,
  ucEmuteca, uEmutecaCommon, ucEmutecaSystem,
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
    FSHA1Folder: string;
    FfmSysEditor: TfmLEmuTKFullSystemEditor;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSHA1Folder(AValue: string);

  protected

    property fmSysEditor: TfmLEmuTKFullSystemEditor read FfmSysEditor;

    procedure AddItemToList; override;
    procedure DeleteItemFromList; override;
    procedure ExportList; override;
    procedure ImportList; override;
    procedure OnListClick(aObject: TObject); override;
    procedure OnListClickCheck(aObject: TObject; aBool: boolean); override;
    procedure SetCheckedAll(aBool: boolean); override;

    procedure DoClearFrameData; override;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    //< Needed by fmSysEditor
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;


    // Creates a form with System Manager.
    class function SimpleForm(aEmuteca: cEmuteca; aSHA1Folder: string; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSysManager }

procedure TfmLEmuTKSysManager.DoClearFrameData;
begin
  inherited ClearFrameData;
end;

procedure TfmLEmuTKSysManager.SetCheckedAll(aBool: boolean);
//var
//  i: integer;
//  aSystem: cEmutecaSystem;
begin

  // DO NOTHING, ENABLING SYSTEMS IS DONE ON SAVING LIST

  //if not assigned(Emuteca) then
  //  Exit;

  // i := 0;
  // while i < Emuteca.SystemManager.FullList.Count do
  // begin
  //   aSystem := Emuteca.SystemManager.FullList[i];
  //   aSystem.Enabled := aBool;
  //   Inc(i);
  // end;
end;

procedure TfmLEmuTKSysManager.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  fmSysEditor.Emuteca := Emuteca;

  LoadFrameData;
end;

procedure TfmLEmuTKSysManager.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
  fmSysEditor.SHA1Folder := SHA1Folder;
end;

procedure TfmLEmuTKSysManager.OnListClick(aObject: TObject);
begin
  fmSysEditor.System := cEmutecaSystem(aObject);
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

  Emuteca.SystemManager.FullList.Add(aSystem);

  LoadFrameData;

  fmSysEditor.System := aSystem;
end;

procedure TfmLEmuTKSysManager.DeleteItemFromList;
begin
  if not assigned(Emuteca) then
    Exit;
  if clbPropItems.ItemIndex = -1 then
    exit;

  fmSysEditor.System := nil;
  try
    Emuteca.SystemManager.FullList.Remove(
      cEmutecaSystem(clbPropItems.Items.Objects[clbPropItems.ItemIndex]));

  finally
    LoadFrameData;
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

procedure TfmLEmuTKSysManager.DoLoadFrameData;
var
  i: integer;
begin
  Enabled := assigned(Emuteca);

  if not assigned(Emuteca) then begin
    ClearFrameData;
    Exit;
  end;

  Emuteca.SystemManager.FullList.AssignToStrLst(clbPropItems.Items);
  i := 0;
  while i < clbPropItems.Items.Count do
  begin
    clbPropItems.Checked[i] :=
      cEmutecaSystem(clbPropItems.Items.Objects[i]).Enabled;
    Inc(i);
  end;
end;

procedure TfmLEmuTKSysManager.DoSaveFrameData;
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
        aSystem.SaveSoftGroupLists(Emuteca.SystemManager.SysDataFolder +
          aSystem.FileName, False);
      end
      else
      begin
        // ... loading soft of previously disabled systems
        aSystem.LoadSoftGroupLists(Emuteca.SystemManager.SysDataFolder +
          aSystem.FileName);
      end;
      aSystem.Enabled := clbPropItems.Checked[i];
    end;

    Inc(i);
  end;

  Emuteca.SystemManager.UpdateEnabledList;
end;

class function TfmLEmuTKSysManager.SimpleForm(aEmuteca: cEmuteca;
  aSHA1Folder: string; aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmLEmuTKSysManager;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmLEmuTKSysManager';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'System Manager']);

    aFrame := TfmLEmuTKSysManager.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.SHA1Folder := aSHA1Folder;
    aFrame.Emuteca := aEmuteca;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmLEmuTKSysManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FfmSysEditor := TfmLEmuTKFullSystemEditor.Create(Self);
  fmSysEditor.SaveButtons := True;
  fmSysEditor.ButtonClose := False;
  fmSysEditor.Align := alClient;
  fmSysEditor.Parent := Self;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmLEmuTKSysManager.Destroy;
begin
  inherited Destroy;
end;

end.
