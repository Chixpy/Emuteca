unit ufEmutecaEmulatorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn, Spin, Menus, LazFileUtils, LCLIntf,
  // CHX units
  uCHXStrUtils, uCHXDlgUtils,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca classes
  ucEmutecaEmulator;

type

  { TfmEmutecaEmulatorEditor }

  TfmEmutecaEmulatorEditor = class(TfmCHXPropEditor)
    actParamROMExtra: TAction;
    actParamROMFileExtension: TAction;
    actParamROMFileNoExt: TAction;
    actParamROMFilename: TAction;
    actParamROMDir: TAction;
    actParamROMPath: TAction;
    actWFEmuteca: TAction;
    actOpenWebPage: TAction;
    actWFEmulator: TAction;
    actWFROM: TAction;
    bGoWebPage: TSpeedButton;
    bParameters: TSpeedButton;
    eDeveloper: TEdit;
    eExePath: TFileNameEdit;
    eExitCode: TSpinEdit;
    eName: TEdit;
    eParameters: TEdit;
    eWebPage: TEdit;
    eWorkingFolder: TDirectoryEdit;
    gbxAdvanced: TGroupBox;
    gbxBasic: TGroupBox;
    lDeveloper: TLabel;
    lExePath: TLabel;
    lExitCode: TLabel;
    lExtensions: TLabel;
    lExtraParameters: TLabel;
    lID: TLabel;
    lName: TLabel;
    lParameters: TLabel;
    lWebPage: TLabel;
    lWorkingFolder: TLabel;
    mExtraParameters: TMemo;
    pmiParamROMExtra: TMenuItem;
    pmiParamROMFileExtension: TMenuItem;
    pmiParamROMFileNoExt: TMenuItem;
    pmiParamROMFilename: TMenuItem;
    pmiParamROMDir: TMenuItem;
    pmiParamROMPath: TMenuItem;
    pmiWFEmuteca: TMenuItem;
    pmiWFEmu: TMenuItem;
    pmiWFROM: TMenuItem;
    mExtensions: TMemo;
    pmParameters: TPopupMenu;
    pmWFolder: TPopupMenu;
    pParameters: TPanel;
    pWebPage: TPanel;
    pWFolder: TPanel;
    bWorkingFolder: TSpeedButton;
    Splitter1: TSplitter;
    procedure actOpenWebPageExecute(Sender: TObject);
    procedure actParamROMDirExecute(Sender: TObject);
    procedure actParamROMExtraExecute(Sender: TObject);
    procedure actParamROMFileExtensionExecute(Sender: TObject);
    procedure actParamROMFilenameExecute(Sender: TObject);
    procedure actParamROMFileNoExtExecute(Sender: TObject);
    procedure actParamROMPathExecute(Sender: TObject);
    procedure actWFEmulatorExecute(Sender: TObject);
    procedure actWFEmutecaExecute(Sender: TObject);
    procedure actWFROMExecute(Sender: TObject);
    procedure bParametersClick(Sender: TObject);
    procedure bWorkingFolderClick(Sender: TObject);
    procedure eFileButtonClick(Sender: TObject);

  private
    FEmulator: cEmutecaEmulator;
    procedure SetEmulator(AValue: cEmutecaEmulator);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmEmutecaEmulatorEditor }

constructor TfmEmutecaEmulatorEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaEmulatorEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmutecaEmulatorEditor.actWFEmulatorExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaEmuDirKey;
end;

procedure TfmEmutecaEmulatorEditor.actWFEmutecaExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaCurrentDirKey;
end;

procedure TfmEmutecaEmulatorEditor.actOpenWebPageExecute(Sender: TObject);
begin
  if eWebPage.Text = '' then Exit;
  OpenURL(eWebPage.Text);
end;

procedure TfmEmutecaEmulatorEditor.actParamROMDirExecute(Sender: TObject);
begin
   eParameters.SelText:=kEmutecaROMDirKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMExtraExecute(Sender: TObject);
begin
  eParameters.SelText:=kEmutecaROMExtraParamKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMFileExtensionExecute(
  Sender: TObject);
begin
  eParameters.SelText:=kEmutecaROMFileExtKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMFilenameExecute(Sender: TObject);
begin
  eParameters.SelText:=kEmutecaROMFileNameKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMFileNoExtExecute(Sender: TObject);
begin
   eParameters.SelText:=kEmutecaROMFileNameNoExtKey;
end;

procedure TfmEmutecaEmulatorEditor.actParamROMPathExecute(Sender: TObject);
begin
  eParameters.SelText:=kEmutecaROMPathKey;
end;

procedure TfmEmutecaEmulatorEditor.actWFROMExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaROMDirKey;
end;

procedure TfmEmutecaEmulatorEditor.bParametersClick(Sender: TObject);
begin
  pmParameters.PopUp;
end;

procedure TfmEmutecaEmulatorEditor.bWorkingFolderClick(Sender: TObject);
begin
  pmWFolder.PopUp;
end;

procedure TfmEmutecaEmulatorEditor.eFileButtonClick(Sender: TObject);
begin
  SetFileEditInitialDir(TFileNameEdit(Sender), ProgramDirectory);
end;

procedure TfmEmutecaEmulatorEditor.SetEmulator(AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
  LoadFrameData;
end;

procedure TfmEmutecaEmulatorEditor.DoClearFrameData;
begin
  lID.Caption := ' ';
  eName.Clear;
  eDeveloper.Clear;
  eWebPage.Clear;
  eExePath.Clear;
  eWorkingFolder.Clear;
  eParameters.Clear;
  mExtensions.Clear;
  eExitCode.Value := 0;
  mExtraParameters.Clear;
end;

procedure TfmEmutecaEmulatorEditor.DoLoadFrameData;
begin
  ClearFrameData;

  Enabled := assigned(Emulator);

  if not Enabled then
    Exit;

  lID.Caption := Emulator.ID;
  eName.Text := Emulator.Title;
  eDeveloper.Text := Emulator.Developer;
  eWebPage.Text := Emulator.WebPage;

  eExePath.Text := SysPath(Emulator.ExeFile);
  eWorkingFolder.Text := SysPath(Emulator.WorkingFolder);
  eParameters.Text := Emulator.Parameters;
  mExtensions.Lines.Assign(Emulator.FileExt);
  eExitCode.Value := Emulator.ExitCode;
  mExtraParameters.Lines.Assign(Emulator.ExtraParamFormat);
end;

procedure TfmEmutecaEmulatorEditor.DoSaveFrameData;
begin
  Emulator.Title := eName.Text;
  Emulator.Developer := eDeveloper.Text;
  Emulator.WebPage := eWebPage.Text;

  Emulator.ExeFile := eExePath.Text;
  Emulator.WorkingFolder := eWorkingFolder.Text;
  Emulator.Parameters := eParameters.Text;
  Emulator.FileExt.Assign(mExtensions.Lines);
  Emulator.ExitCode := eExitCode.Value;
  Emulator.ExtraParamFormat.Assign(mExtraParameters.Lines);
end;

end.
